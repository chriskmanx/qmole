/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gku-prompt.h"
#include "gku-prompt-marshal.h"
#include "gku-prompt-util.h"

#include "egg/egg-cleanup.h"
#include "egg/egg-dh.h"
#include "egg/egg-error.h"
#include "egg/egg-hex.h"
#include "egg/egg-hkdf.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-spawn.h"

#include "gcr/gcr-unlock-options.h"

#include "pkcs11/pkcs11i.h"

#include <gcrypt.h>

#include <sys/wait.h>

#ifdef _DEBUG
#define DEBUG_PROMPT 0
#define DEBUG_STDERR 0
#endif

enum {
	RESPONDED,
	COMPLETED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

typedef struct _TransportCrypto {
	gcry_mpi_t private;
	gcry_mpi_t prime;
	gpointer key;
	gsize n_key;
} TransportCrypto;

struct _GkuPromptPrivate {
	GKeyFile *input;
	GKeyFile *output;
	gchar *executable;
	gboolean completed;
	gboolean failure;

	/* Transport crypto */
	TransportCrypto *transport;

	/* Information about child */
	GPid pid;

	/* Input and output */
	gchar *in_data;
	gsize in_offset;
	gsize in_length;
	GString *out_data;
	GString *err_data;
	guint io_tag;
};

G_DEFINE_TYPE (GkuPrompt, gku_prompt, G_TYPE_OBJECT);

/* Forward declaration*/
static void display_async_prompt (GkuPrompt *);

/* User choices we transfer over during a soft prompt reset */
const struct { const gchar *section; const gchar *name; } SOFT_RESET[] = {
	{ "unlock-options", "choice" },
	{ "unlock-options", "ttl" },
	{ "details", "expanded" },
};

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
kill_process (GkuPrompt *self)
{
	if (self->pv->pid)
		kill (self->pv->pid, SIGHUP);
}

static void
mark_completed (GkuPrompt *self)
{
	g_assert (GKU_IS_PROMPT (self));
	g_assert (!self->pv->completed);
	self->pv->completed = TRUE;
	g_signal_emit (self, signals[COMPLETED], 0);
}

static void
mark_failed (GkuPrompt *self)
{
	g_assert (GKU_IS_PROMPT (self));
	g_assert (!self->pv->failure);
	self->pv->failure = TRUE;
	if (!self->pv->completed)
		mark_completed (self);
}

static void
mark_responded (GkuPrompt *self)
{
	gboolean continu = FALSE;

	g_assert (GKU_IS_PROMPT (self));
	g_signal_emit (self, signals[RESPONDED], 0, &continu);
	g_assert (GKU_IS_PROMPT (self));

	/* The prompt gets displayed again */
	if (continu) {
		display_async_prompt (self);

	/* The prompt has completed */
	} else {
		mark_completed (self);
	}
}

static gboolean
on_standard_input (int fd, gpointer user_data)
{
	GkuPrompt *self = GKU_PROMPT (user_data);
	gssize ret;

	g_return_val_if_fail (GKU_IS_PROMPT (self), FALSE);

	if (self->pv->in_offset >= self->pv->in_length)
		return FALSE;

	g_assert (self->pv->in_data);
	ret = egg_spawn_write_input (fd, self->pv->in_data + self->pv->in_offset,
	                             self->pv->in_length - self->pv->in_offset);

	if (ret <= 0) {
		g_warning ("couldn't write all input to prompt process");
		mark_failed (self);
		return FALSE;
	}

	self->pv->in_offset += ret;
	return TRUE;
}

static gboolean
on_standard_output (int fd, gpointer user_data)
{
	GkuPrompt *self = GKU_PROMPT (user_data);
	gchar buffer[1024];
	gssize ret;

	g_return_val_if_fail (GKU_IS_PROMPT (self), FALSE);

	ret = egg_spawn_read_output (fd, buffer, sizeof (buffer));
	if (ret < 0) {
		g_warning ("couldn't read output data from prompt process");
		mark_failed (self);
		return FALSE;
	}

	if (!self->pv->out_data)
		self->pv->out_data = g_string_new_len (buffer, ret);
	else
		g_string_append_len (self->pv->out_data, buffer, ret);

	return (ret > 0);
}

static gboolean
on_standard_error (int fd, gpointer user_data)
{
	GkuPrompt *self = GKU_PROMPT (user_data);
	gchar buffer[1024];
	gssize ret;
	gchar *ptr;

	g_return_val_if_fail (GKU_IS_PROMPT (self), FALSE);

	ret = egg_spawn_read_output (fd, buffer, sizeof (buffer));
	if (ret < 0) {
		g_warning ("couldn't read error data from prompt process");
		mark_failed (self);
		return FALSE;
	}

	if (!self->pv->err_data)
		self->pv->err_data = g_string_new_len (buffer, ret);
	else
		g_string_append_len (self->pv->err_data, buffer, ret);

	/* Print all stderr lines as messages */
	while ((ptr = strchr (self->pv->err_data->str, '\n')) != NULL) {
		*ptr = '\0';
		g_printerr ("%s\n", self->pv->err_data->str);
		g_string_erase (self->pv->err_data, 0,
		                ptr - self->pv->err_data->str + 1);
	}

	return ret > 0;
}

static void
on_io_completed (gpointer user_data)
{
	GkuPrompt *self = GKU_PROMPT (user_data);
	GError *error = NULL;

	g_return_if_fail (GKU_IS_PROMPT (self));

	g_assert (!self->pv->output);
	g_assert (self->pv->io_tag != 0);
	g_assert (!self->pv->completed);

	/* Should be the last call we receive */
	self->pv->io_tag = 0;

	/* Print out any remaining errors */
	if (self->pv->err_data && self->pv->err_data->len)
		g_message ("%s", self->pv->err_data->str);

	/* Parse the output data properly */
	if (!self->pv->failure) {
		if (!self->pv->out_data->len) {
			g_warning ("no data returned from the prompt");
			mark_failed (self);
		}
	}

	if (!self->pv->failure) {
#if DEBUG_PROMPT
		g_printerr ("PROMPT OUTPUT:\n%s\n", self->pv->out_data->str);
#endif
		self->pv->output = g_key_file_new ();
		if (!g_key_file_load_from_data (self->pv->output, self->pv->out_data->str,
						self->pv->out_data->len, G_KEY_FILE_NONE, &error)) {
			g_key_file_free (self->pv->output);
			g_warning ("couldn't parse output from prompt: %s", egg_error_message (error));
			g_clear_error (&error);
			mark_failed (self);
		} else {
			mark_responded (self);
		}
	}
}

static void
on_child_exited (GPid pid, gint status, gpointer user_data)
{
	GkuPrompt *self = GKU_PROMPT (user_data);
	gint code;

	if (pid == self->pv->pid) {
		self->pv->pid = 0;
		if (!self->pv->failure) {
			if (WIFEXITED (status)) {
				code = WEXITSTATUS (status);
				if (code != 0) {
					g_message ("prompt process exited with failure code: %d", code);
					mark_failed (self);
				}
			} else if (WIFSIGNALED (status)) {
				code = WTERMSIG (status);
				g_message ("prompt process was killed with signal: %d", code);
				mark_failed (self);
			}
		}
	}

	g_spawn_close_pid (pid);
}

static void
prepare_transport_crypto (GkuPrompt *self)
{
	TransportCrypto *transport;
	gcry_mpi_t pub, base;

	if (!g_key_file_has_group (self->pv->input, "transport")) {
		g_assert (!self->pv->transport);
		transport = g_slice_new0 (TransportCrypto);

		/* Figure out our prime, base, public and secret bits */
		if (!egg_dh_default_params ("ietf-ike-grp-modp-1536", &transport->prime, &base) ||
		    !egg_dh_gen_pair (transport->prime, base, 0, &pub, &transport->private))
			g_return_if_reached ();

		/* Send over the prime, base, and public bits */
		gku_prompt_util_encode_mpi (self->pv->input, "transport", "prime", transport->prime);
		gku_prompt_util_encode_mpi (self->pv->input, "transport", "base", base);
		gku_prompt_util_encode_mpi (self->pv->input, "transport", "public", pub);

		gcry_mpi_release (base);
		gcry_mpi_release (pub);

		self->pv->transport = transport;
	}

	if (self->pv->transport) {
		egg_secure_free (self->pv->transport->key);
		self->pv->transport->key = NULL;
		self->pv->transport->n_key = 0;
	}
}

static gconstpointer
calculate_transport_key (GkuPrompt *self, gsize *n_key)
{
	gcry_mpi_t peer;
	gpointer ikm, key;
	gsize n_ikm;

	g_assert (self->pv->output);
	g_assert (n_key);

	if (!self->pv->transport) {
		g_warning ("GkuPrompt did not negotiate crypto, but its caller is now asking"
		           " it to do the decryption. This is an error in gnome-keyring");
		return NULL;
	}

	if (!self->pv->transport->key) {
		if (!gku_prompt_util_decode_mpi (self->pv->output, "transport", "public", &peer))
			return NULL;

		ikm = egg_dh_gen_secret (peer, self->pv->transport->private,
		                         self->pv->transport->prime, &n_ikm);

		gcry_mpi_release (peer);

		if (!ikm)
			return NULL;

		key = egg_secure_alloc (16);
		if (!egg_hkdf_perform ("sha256", ikm, n_ikm, NULL, 0, NULL, 0, key, 16))
			g_return_val_if_reached (NULL);

		egg_secure_free (ikm);
		egg_secure_free (self->pv->transport->key);
		self->pv->transport->key = key;
		self->pv->transport->n_key = 16;
	}

	*n_key = self->pv->transport->n_key;
	return self->pv->transport->key;
}

static gboolean
prepare_input_data (GkuPrompt *self)
{
	GError *error = NULL;

	g_assert (self->pv->input);

	prepare_transport_crypto (self);

	self->pv->in_data = g_key_file_to_data (self->pv->input, &self->pv->in_length, &error);
	if (!self->pv->in_data) {
		g_warning ("couldn't encode data for prompt: %s", egg_error_message (error));
		g_clear_error (&error);
		mark_failed (self);
		return FALSE;
	}

#if DEBUG_PROMPT
	g_printerr ("PROMPT INPUT:\n%s\n", self->pv->in_data);
#endif

	/* No further modifications to input are possible */
	g_key_file_free (self->pv->input);
	self->pv->input = NULL;

	return TRUE;
}

static void
display_async_prompt (GkuPrompt *self)
{
	EggSpawnCallbacks callbacks;
	GError *error = NULL;
	gchar **names, **envp;
	int i, n;

	char *argv[] = {
		self->pv->executable,
		NULL,
	};

	g_assert (!self->pv->pid);

	/* Fires completed event when fails */
	if (!prepare_input_data (self))
		return;

	/* Any environment we have */
	names = g_listenv ();
	for (n = 0; names && names[n]; ++n);
	envp = g_new (char*, n + 2);
	for (i = 0; i < n; i++)
		envp[i] = g_strdup_printf ("%s=%s", names[i], g_getenv (names[i]));
	envp[i++] = NULL;
	g_strfreev (names);

	memset (&callbacks, 0, sizeof (callbacks));
	callbacks.standard_input = on_standard_input;
	callbacks.standard_output = on_standard_output;
	callbacks.standard_error = on_standard_error;
	callbacks.completed = on_io_completed;
	callbacks.finalize_func = g_object_unref;

#ifdef DEBUG_STDERR
	/* Let stderr show through if desired */
	callbacks.standard_error = NULL;
#endif

	self->pv->io_tag = egg_spawn_async_with_callbacks (NULL, argv, envp, G_SPAWN_DO_NOT_REAP_CHILD,
	                                                   &self->pv->pid, &callbacks, g_object_ref (self),
	                                                   NULL, &error);
	if (!self->pv->io_tag) {
		g_warning ("couldn't spawn prompt tool: %s", egg_error_message (error));
		g_clear_error (&error);
		self->pv->pid = 0;
		mark_failed (self);
		return;
	}

	g_child_watch_add_full (G_PRIORITY_DEFAULT, self->pv->pid, on_child_exited,
	                        g_object_ref (self), g_object_unref);
}

static void
display_dummy_prompt (GkuPrompt *self, const gchar *response)
{
	GError *error = NULL;
	gchar *title = NULL;

	g_assert (GKU_IS_PROMPT (self));

	if (!response && self->pv->input)
		title = g_key_file_get_string (self->pv->input, "prompt", "title", NULL);

	/* Fires completed event when fails */
	if (!prepare_input_data (self)) {
		g_free (title);
		return;
	}

	if (response) {
	#if DEBUG_PROMPT
		g_printerr ("PROMPT OUTPUT:\n%s\n", response);
	#endif
		self->pv->output = g_key_file_new ();
		if (!g_key_file_load_from_data (self->pv->output, response, strlen (response),
						G_KEY_FILE_NONE, &error)) {
			g_key_file_free (self->pv->output);
			g_warning ("couldn't parse output from prompt: %s", egg_error_message (error));
			g_clear_error (&error);
			mark_failed (self);
		} else {
			mark_responded (self);
		}

	/* No response, problem! */
	} else {
		g_warning ("unexpected prompt without response: %s", title ? title : "unknown");
		mark_failed (self);
	}

	g_free (title);
}

static void
clear_prompt_data (GkuPrompt *self)
{
	TransportCrypto *transport;

	if (self->pv->input)
		g_key_file_free (self->pv->input);
	self->pv->input = NULL;

	if (self->pv->output)
		g_key_file_free (self->pv->output);
	self->pv->output = NULL;

	self->pv->failure = FALSE;
	self->pv->completed = FALSE;

	g_free (self->pv->in_data);
	self->pv->in_data = NULL;
	self->pv->in_length = 0;
	self->pv->in_offset = 0;

	if (self->pv->out_data)
		g_string_free (self->pv->out_data, TRUE);
	self->pv->out_data = NULL;

	if (self->pv->err_data)
		g_string_free (self->pv->err_data, TRUE);
	self->pv->err_data = NULL;

	if (self->pv->io_tag)
		g_source_remove (self->pv->io_tag);
	self->pv->io_tag = 0;

	if (self->pv->transport) {
		transport = self->pv->transport;
		if (transport->prime)
			gcry_mpi_release (transport->prime);
		if (transport->private)
			gcry_mpi_release (transport->private);
		if (transport->key) {
			egg_secure_clear (transport->key, transport->n_key);
			egg_secure_free (transport->key);
		}
		g_slice_free (TransportCrypto, transport);
		self->pv->transport = NULL;
	}
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static gboolean
gku_prompt_real_responded (GkuPrompt *self)
{
	/* The prompt is done, if nobody overrode this signal and returned TRUE */
	return FALSE;
}

static void
gku_prompt_real_completed (GkuPrompt *self)
{
	/* Nothing to do */
}

static GObject*
gku_prompt_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkuPrompt *self = GKU_PROMPT (G_OBJECT_CLASS (gku_prompt_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	if (!self->pv->executable)
		self->pv->executable = g_strdup (PROMPTEXEC);

	return G_OBJECT (self);
}

static void
gku_prompt_init (GkuPrompt *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKU_TYPE_PROMPT, GkuPromptPrivate);
	gku_prompt_reset (self, TRUE);
}

static void
gku_prompt_dispose (GObject *obj)
{
	GkuPrompt *self = GKU_PROMPT (obj);

	kill_process (self);
	clear_prompt_data (self);

	G_OBJECT_CLASS (gku_prompt_parent_class)->dispose (obj);
}

static void
gku_prompt_finalize (GObject *obj)
{
	GkuPrompt *self = GKU_PROMPT (obj);

	g_assert (self->pv->pid == 0);
	g_assert (!self->pv->input);
	g_assert (!self->pv->output);
	g_assert (!self->pv->in_data);
	g_assert (!self->pv->out_data);
	g_assert (!self->pv->err_data);
	g_assert (!self->pv->io_tag);
	g_assert (!self->pv->transport);

	g_free (self->pv->executable);
	self->pv->executable = NULL;

	G_OBJECT_CLASS (gku_prompt_parent_class)->finalize (obj);
}

static void
gku_prompt_class_init (GkuPromptClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructor = gku_prompt_constructor;
	gobject_class->dispose = gku_prompt_dispose;
	gobject_class->finalize = gku_prompt_finalize;

	klass->responded = gku_prompt_real_responded;
	klass->completed = gku_prompt_real_completed;

	g_type_class_add_private (klass, sizeof (GkuPromptPrivate));

	signals[COMPLETED] = g_signal_new ("completed", GKU_TYPE_PROMPT,
	                                   G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkuPromptClass, completed),
	                                   NULL, NULL, g_cclosure_marshal_VOID__VOID,
	                                   G_TYPE_NONE, 0);

	signals[RESPONDED] = g_signal_new ("responded", GKU_TYPE_PROMPT,
	                                   G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GkuPromptClass, responded),
	                                   g_signal_accumulator_true_handled, NULL, gku_prompt_marshal_BOOLEAN__VOID,
	                                   G_TYPE_BOOLEAN, 0);

	egg_libgcrypt_initialize ();
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkuPrompt*
gku_prompt_new (void)
{
	return g_object_new (GKU_TYPE_PROMPT, NULL);
}

void
gku_prompt_set_title (GkuPrompt *self, const gchar *title)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_key_file_set_string (self->pv->input, "prompt", "title", title);
}

void
gku_prompt_set_primary_text (GkuPrompt *self, const gchar *primary)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_key_file_set_string (self->pv->input, "prompt", "primary", primary);
}

void
gku_prompt_set_secondary_text (GkuPrompt *self, const gchar *secondary)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_key_file_set_string (self->pv->input, "prompt", "secondary", secondary);
}

void
gku_prompt_show_widget (GkuPrompt *self, const gchar *widget)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_key_file_set_boolean (self->pv->input, "visibility", widget, TRUE);
}

void
gku_prompt_hide_widget (GkuPrompt *self, const gchar *widget)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_key_file_set_boolean (self->pv->input, "visibility", widget, FALSE);
}

void
gku_prompt_select_widget (GkuPrompt *self, const gchar *widget)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_key_file_set_boolean (self->pv->input, "selected", widget, TRUE);
}

gboolean
gku_prompt_has_response (GkuPrompt *self)
{
	g_return_val_if_fail (GKU_IS_PROMPT (self), FALSE);
	return self->pv->output ? TRUE : FALSE;
}

gint
gku_prompt_get_response (GkuPrompt *self)
{
	gchar *response;
	guint ret;

	g_return_val_if_fail (GKU_IS_PROMPT (self), GKU_RESPONSE_FAILURE);
	if (self->pv->failure)
		return GKU_RESPONSE_FAILURE;

	g_return_val_if_fail (self->pv->output, GKU_RESPONSE_FAILURE);

	response = g_key_file_get_string (self->pv->output, "prompt", "response", NULL);
	if (!response || g_str_equal (response, "")) {
		ret = GKU_RESPONSE_NONE;
	} else if (g_str_equal (response, "ok")) {
		ret = GKU_RESPONSE_OK;
	} else if (g_str_equal (response, "no")) {
		ret =  GKU_RESPONSE_NO;
	} else if (g_str_equal (response, "other")) {
		ret = GKU_RESPONSE_OTHER;
	} else {
		g_warning ("invalid response field received from prompt: %s", response);
		ret = GKU_RESPONSE_NONE;
	}

	g_free (response);
	return ret;
}

gchar*
gku_prompt_get_password (GkuPrompt *self, const gchar *password_type)
{
	gchar *result;
	gpointer data;
	gsize n_data;
	gconstpointer key;
	gsize n_key;
	gpointer parameter;
	gsize n_parameter;

	g_return_val_if_fail (GKU_IS_PROMPT (self), NULL);

	if (!gku_prompt_get_transport_password (self, password_type,
	                                        &parameter, &n_parameter,
	                                        &data, &n_data))
		return NULL;

	/* Parse the encryption params and figure out a key */
	if (n_parameter) {
		key = calculate_transport_key (self, &n_key);
		g_return_val_if_fail (key, NULL);
		result = gku_prompt_util_decrypt_text (key, n_key,
		                                       parameter, n_parameter,
		                                       data, n_data);

	/* A non-encrypted password */
	} else {
		result = egg_secure_alloc (n_data + 1);
		memcpy (result, data, n_data);
	}

	g_free (parameter);
	g_free (data);
	return result;
}

gboolean
gku_prompt_is_widget_selected (GkuPrompt *self, const gchar *widget)
{
	g_return_val_if_fail (GKU_IS_PROMPT (self), FALSE);
	g_return_val_if_fail (self->pv->output, FALSE);

	if (!self->pv->failure)
		return FALSE;

	g_assert (self->pv->output);
	return g_key_file_get_boolean (self->pv->output, "selected", widget, NULL);
}

void
gku_prompt_set_window_id (GkuPrompt *self, const gchar *window_id)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	if (!window_id)
		g_key_file_remove_key (self->pv->input, "prompt", "window-id", NULL);
	else
		g_key_file_set_string (self->pv->input, "prompt", "window-id", window_id);
}

void
gku_prompt_set_warning (GkuPrompt *self, const gchar *warning)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	if (!warning)
		g_key_file_remove_key (self->pv->input, "prompt", "warning", NULL);
	else
		g_key_file_set_string (self->pv->input, "prompt", "warning", warning);
}

void
gku_prompt_reset (GkuPrompt *self, gboolean hard)
{
	GKeyFile *input;
	gchar *value;
	gint i;

	g_return_if_fail (GKU_IS_PROMPT (self));

	kill_process (self);
	self->pv->pid = 0;

	input = g_key_file_new ();

	/* If not a hard reset, copy over some user data */
	if (!hard && self->pv->output) {
		for (i = 0; i < G_N_ELEMENTS (SOFT_RESET); ++i) {
			value = g_key_file_get_value (self->pv->output, SOFT_RESET[i].section,
			                              SOFT_RESET[i].name, NULL);
			if (value != NULL)
				g_key_file_set_value (input, SOFT_RESET[i].section,
				                      SOFT_RESET[i].name, value);
			g_free (value);
		}
	}

	clear_prompt_data (self);
	self->pv->input = input;
}


void
gku_prompt_set_transport_param (GkuPrompt *self, const gchar *name,
                                gconstpointer value, gsize n_value)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);
	g_return_if_fail (name);
	gku_prompt_util_encode_hex (self->pv->input, "transport", name, value, n_value);
}

gpointer
gku_prompt_get_transport_param (GkuPrompt *self, const gchar *name, gsize *n_value)
{
	g_return_val_if_fail (GKU_IS_PROMPT (self), NULL);
	g_return_val_if_fail (name, NULL);
	g_return_val_if_fail (n_value, NULL);

	if (self->pv->failure)
		return NULL;

	g_return_val_if_fail (self->pv->output, NULL);
	return gku_prompt_util_decode_hex (self->pv->output, "transport", name, n_value);

}

gboolean
gku_prompt_get_transport_password (GkuPrompt *self, const gchar *password_type,
                                   gpointer *parameter, gsize *n_parameter,
                                   gpointer *value, gsize *n_value)
{
	if (!password_type)
		password_type = "password";

	g_return_val_if_fail (parameter, FALSE);
	g_return_val_if_fail (n_parameter, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (n_value, FALSE);

	if (self->pv->failure)
		return FALSE;

	g_return_val_if_fail (self->pv->output, FALSE);

	/* Parse out an IV */
	*parameter = gku_prompt_util_decode_hex (self->pv->output, password_type,
	                                         "parameter", n_parameter);
	if (*parameter == NULL)
		*n_parameter = 0;

	/* Parse out the password */
	*value = gku_prompt_util_decode_hex (self->pv->output, password_type,
	                                     "value", n_value);
	if (*value == NULL)
		*n_value = 0;

	return TRUE;
}

const gchar*
gku_prompt_get_unlock_choice (GkuPrompt *self)
{
	const gchar *result;
	gchar *value;

	g_return_val_if_fail (GKU_IS_PROMPT (self), NULL);
	g_return_val_if_fail (self->pv->output, NULL);

	value = g_key_file_get_string (self->pv->output, "unlock-options", "choice", NULL);
	if (value == NULL)
		return "";

	result = g_intern_string (value);
	g_free (value);

	return result;
}

void
gku_prompt_set_unlock_choice (GkuPrompt *self, const gchar *option)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (option);
	g_return_if_fail (self->pv->input);

	g_key_file_set_string (self->pv->input, "unlock-options", "choice", option);

	/* Expand if anything but session unlock is chosen */
	if (!g_str_equal (option, GCR_UNLOCK_OPTION_SESSION) &&
	    !g_key_file_has_key (self->pv->input, "details", "expanded", NULL))
		g_key_file_set_boolean (self->pv->input, "details", "expanded", TRUE);
}

guint
gku_prompt_get_unlock_ttl (GkuPrompt *self)
{
	g_return_val_if_fail (GKU_IS_PROMPT (self), 0);
	g_return_val_if_fail (self->pv->output, 0);

	return g_key_file_get_integer (self->pv->output, "unlock-options", "ttl", NULL);
}

void
gku_prompt_set_unlock_ttl (GkuPrompt *self, guint ttl)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);

	g_key_file_set_integer (self->pv->input, "unlock-options", "ttl", ttl);
}

void
gku_prompt_set_unlock_sensitive (GkuPrompt *self, const gchar *option,
                                 gboolean sensitive, const gchar *reason)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (self->pv->input);

	g_key_file_set_boolean (self->pv->input, option, "sensitive", sensitive);
	if (reason)
		g_key_file_set_string (self->pv->input, option, "reason", reason);
}

void
gku_prompt_set_unlock_label (GkuPrompt *self, const gchar *option, const gchar *label)
{
	g_return_if_fail (GKU_IS_PROMPT (self));
	g_return_if_fail (option);
	g_return_if_fail (label);
	g_return_if_fail (self->pv->input);

	g_key_file_set_string (self->pv->input, option, "label", label);
}

/* ----------------------------------------------------------------------------------
 * ATTENTION QUEUES
 */

/* Forward declaration */
static void next_attention_req (const gchar *);
static void free_attention_req (gpointer data);

typedef struct _Attention {
	gchar *window_id;
	GkuPromptAttentionFunc callback;
	GDestroyNotify destroy;
	gpointer user_data;
	GkuPrompt *prompt;
	gboolean active;
	GCond *cond;
} AttentionReq;

static GStaticMutex attention_mutex = G_STATIC_MUTEX_INIT;
static GHashTable *attention_reqs = NULL;

/* Used for testing, to queue responses without showing prompt */
static gboolean dummy_responses = FALSE;
static GQueue queued_responses = G_QUEUE_INIT;

static void
done_attention_req (GkuPrompt *prompt, gpointer user_data)
{
	AttentionReq *att = user_data;

	g_assert (att);
	g_assert (GKU_IS_PROMPT (att->prompt));

	if (att->active) {
		att->active = FALSE;
		next_attention_req (att->window_id);
	}

	if (att->cond) {
		g_static_mutex_lock (&attention_mutex);
		g_cond_broadcast (att->cond);
		g_static_mutex_unlock (&attention_mutex);
		att->cond = NULL;
	}

	free_attention_req (att);
}

static void
clear_attention_reqs (gpointer unused)
{
	g_assert (attention_reqs);
	g_hash_table_destroy (attention_reqs);
}

static AttentionReq*
alloc_attention_req (const gchar *window_id)
{
	AttentionReq *att;

	g_assert (window_id);

	att = g_slice_new0 (AttentionReq);
	att->window_id = g_strdup (window_id);
	return att;
}

static void
free_attention_req (gpointer data)
{
	AttentionReq *att = data;

	if (att) {
		att->cond = NULL;
		if (att->destroy)
			(att->destroy) (att->user_data);
		if (att->prompt) {
			g_signal_handlers_disconnect_by_func (att->prompt, done_attention_req, att);
			g_object_unref (att->prompt);
		}
		g_free (att->window_id);
		g_slice_free (AttentionReq, att);
	}
}

static void
free_attention_queue (gpointer data)
{
	GQueue *queue = data;
	AttentionReq *att;

	if (queue) {
		while (!g_queue_is_empty (queue)) {
			att = g_queue_pop_head (queue);
			att->active = FALSE;
			free_attention_req (att);
		}
		g_queue_free (queue);
	}
}

static GQueue*
alloc_attention_queue (void)
{
	return g_queue_new ();
}

static void
next_attention_req (const gchar *window_id)
{
	gchar *response;
	AttentionReq *att;
	GQueue *queue;

	g_assert (window_id);
	g_assert (attention_reqs);

	queue = g_hash_table_lookup (attention_reqs, window_id);
	g_return_if_fail (queue);

	/* Nothing more to process for this window */
	if (g_queue_is_empty (queue)) {
		g_hash_table_remove (attention_reqs, window_id);
		return;
	}

	/* Get the next one out */
	att = g_queue_pop_head (queue);
	g_assert (att);
	g_assert (att->window_id);
	g_assert (g_str_equal (att->window_id, window_id));
	g_assert (!att->prompt);
	g_assert (att->callback);

	/* Callback populates the prompt */
	att->prompt = (att->callback) (att->user_data);

	/* Don't show the prompt */
	if (att->prompt == NULL) {
		free_attention_req (att);
		next_attention_req (window_id);
		return;
	}

	att->active = TRUE;
	g_signal_connect_data (att->prompt, "completed", G_CALLBACK (done_attention_req), att,
	                       NULL, G_CONNECT_AFTER);
	gku_prompt_set_window_id (att->prompt, window_id);

	/* Fake display the prompt */
	if (dummy_responses) {
		g_static_mutex_lock (&attention_mutex);
			response = g_queue_pop_head (&queued_responses);
		g_static_mutex_unlock (&attention_mutex);
		display_dummy_prompt (att->prompt, response);
		g_free (response);

	/* Actually display the prompt, "completed" signal will fire */
	} else {
		display_async_prompt (att->prompt);
	}

	/* att can be freed here so beware! */
}

static gboolean
service_attention_req (gpointer user_data)
{
	AttentionReq *att = user_data;
	gboolean now = FALSE;
	GQueue *queue;

	g_assert (att);

	if (!attention_reqs) {
		attention_reqs = g_hash_table_new_full (g_str_hash, g_str_equal,
		                                        g_free, free_attention_queue);
		egg_cleanup_register (clear_attention_reqs, NULL);
	}

	queue = g_hash_table_lookup (attention_reqs, att->window_id);
	if (queue == NULL) {
		queue = alloc_attention_queue ();
		g_hash_table_insert (attention_reqs, g_strdup (att->window_id), queue);
		now = TRUE;
	}

	g_queue_push_tail (queue, att);
	if (now == TRUE)
		next_attention_req (att->window_id);

	/* Remove this timeout handler after one call */
	return FALSE;
}

static AttentionReq*
prepare_attention_req (const gchar *window_id, GkuPromptAttentionFunc callback,
                       gpointer user_data, GDestroyNotify destroy_notify)
{
	AttentionReq *att;

	g_return_val_if_fail (callback, NULL);

	if (!window_id)
		window_id = "";
	att = alloc_attention_req (window_id);
	att->callback = callback;
	att->user_data = user_data;
	att->destroy = destroy_notify;

	return att;
}

void
gku_prompt_request_attention_async (const gchar *window_id, GkuPromptAttentionFunc callback,
                                    gpointer user_data, GDestroyNotify destroy_notify)
{
	AttentionReq *att = prepare_attention_req (window_id, callback, user_data, destroy_notify);
	g_return_if_fail (att);
	g_timeout_add (0, service_attention_req, att);
}

void
gku_prompt_request_attention_sync (const gchar *window_id, GkuPromptAttentionFunc callback,
                                   gpointer user_data, GDestroyNotify destroy_notify)
{
	AttentionReq *att = prepare_attention_req (window_id, callback, user_data, destroy_notify);
	GCond *cond = g_cond_new ();

	g_return_if_fail (att);
	att->cond = cond;

	g_static_mutex_lock (&attention_mutex);
		g_timeout_add (0, service_attention_req, att);

		/* WARNING: att may have been destroyed past this point */

		g_cond_wait (cond, g_static_mutex_get_mutex (&attention_mutex));
	g_static_mutex_unlock (&attention_mutex);

	g_cond_free (cond);
}

#ifdef WITH_TESTABLE

void
gku_prompt_dummy_prepare_response (void)
{
	g_static_mutex_lock (&attention_mutex);
		dummy_responses = TRUE;
		while (!g_queue_is_empty (&queued_responses))
			g_free (g_queue_pop_head (&queued_responses));
	g_static_mutex_unlock (&attention_mutex);
}

gboolean
gku_prompt_dummy_have_response (void)
{
	gboolean ret;
	g_static_mutex_lock (&attention_mutex);
		ret = !g_queue_is_empty (&queued_responses);
	g_static_mutex_unlock (&attention_mutex);
	return ret;
}

static void
queue_dummy_response (gchar *response)
{
	g_assert (response);
	g_static_mutex_lock (&attention_mutex);
		dummy_responses = TRUE;
		g_queue_push_tail (&queued_responses, response);
	g_static_mutex_unlock (&attention_mutex);
}

void
gku_prompt_dummy_queue_response (const gchar *response)
{
	g_return_if_fail (response);
	queue_dummy_response (g_strdup (response));
}

void
gku_prompt_dummy_queue_ok_password (const gchar *password)
{
	const static gchar *RESPONSE = "[password]\nparameter=\nvalue=%s\n[prompt]\nresponse=ok\n";
	gchar *value;

	g_return_if_fail (password);
	value = egg_hex_encode ((const guchar*)password, strlen (password));
	queue_dummy_response (g_strdup_printf (RESPONSE, value));
	g_free (value);
}

void
gku_prompt_dummy_queue_ok_passwords (const gchar *original, const gchar *password)
{
	const static gchar *RESPONSE = "[password]\nparameter=\nvalue=%s\n"
	                               "[original]\nparameter=\nvalue=%s\n"
	                               "[prompt]\nresponse=ok\n";
	gchar *value, *ovalue;

	g_return_if_fail (password);
	value = egg_hex_encode ((const guchar*)password, strlen (password));
	ovalue = egg_hex_encode ((const guchar*)original, strlen (original));
	queue_dummy_response (g_strdup_printf (RESPONSE, value, ovalue));
	g_free (value);
	g_free (ovalue);
}

void
gku_prompt_dummy_queue_auto_password (const gchar *password)
{
	const static gchar *RESPONSE = "[password]\nparameter=\nvalue=%s\n[prompt]\nresponse=ok\n"
	                               "[unlock-options]\nchoice=always\n";
	gchar *value;

	g_return_if_fail (password);
	value = egg_hex_encode ((const guchar*)password, strlen (password));
	queue_dummy_response (g_strdup_printf (RESPONSE, value));
	g_free (value);
}

void
gku_prompt_dummy_queue_no (void)
{
	const static gchar *RESPONSE = "[prompt]\nresponse=no\n";
	gku_prompt_dummy_queue_response (RESPONSE);
}

#endif /* WITH_TESTABLE */
