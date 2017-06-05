/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#include "gkd-gpg-agent.h"
#include "gkd-gpg-agent-private.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "gcr/gcr-unlock-options.h"

#include "pkcs11/pkcs11i.h"

#include "ui/gku-prompt.h"

#include <glib/gi18n.h>

#include <ctype.h>
#include <string.h>

#define GKD_GPG_AGENT_PASS_AS_DATA    0x00000001
#define GKD_GPG_AGENT_REPEAT          0x00000002

#define COLLECTION    "session"
#define N_COLLECTION  7

/* ----------------------------------------------------------------------------------
 * PASSWORD STUFF
 */

static void
keyid_to_field_attribute (const gchar *keyid, GckAttributes *attrs)
{
	GString *fields = g_string_sized_new (128);

	g_assert (keyid);
	g_assert (attrs);

	/* Remember that attribute names are sorted */

	g_string_append (fields, "keyid");
	g_string_append_c (fields, '\0');
	g_string_append (fields, keyid);
	g_string_append_c (fields, '\0');

	g_string_append (fields, "source");
	g_string_append_c (fields, '\0');
	g_string_append (fields, "gnome-keyring:gpg-agent");
	g_string_append_c (fields, '\0');

	gck_attributes_add_data (attrs, CKA_G_FIELDS, fields->str, fields->len);
	g_string_free (fields, TRUE);
}

static gchar*
calculate_label_for_key (const gchar *keyid, const gchar *description)
{
	gchar *label = NULL;
	gchar **lines, **l;
	const gchar *line;
	gsize len;

	/* Use the line that starts and ends with quotes */
	if (description) {
		lines = g_strsplit (description, "\n", -1);
		for (l = lines, line = *l; !label && line; l++, line = *l) {
			len = strlen (line);
			if (len > 2 && line[0] == '\"' && line[len - 1] == '\"')
				label = g_strndup (line + 1, len - 2);
		}
		g_strfreev (lines);
	}

	/* Use last eight characters of keyid */
	if (!label && keyid) {
		len = strlen (keyid);
		if (len > 8)
			label = g_strdup (keyid + (len - 8));
		else
			label = g_strdup (keyid);
	}

	if (!label)
		label = g_strdup (_("Unknown"));

	return label;
}

static GList*
find_saved_items (GckSession *session, GckAttributes *attrs)
{
	GckAttributes *template;
	GError *error = NULL;
	GckAttribute *attr;
	GckObject *search;
	GList *results;
	gpointer data;
	gsize n_data;

	template = gck_attributes_new ();
	gck_attributes_add_ulong (template, CKA_CLASS, CKO_G_SEARCH);
	gck_attributes_add_boolean (template, CKA_TOKEN, FALSE);

	attr = gck_attributes_find (attrs, CKA_G_COLLECTION);
	if (attr != NULL)
		gck_attributes_add (template, attr);

	attr = gck_attributes_find (attrs, CKA_G_FIELDS);
	g_return_val_if_fail (attr != NULL, NULL);
	gck_attributes_add (template, attr);

	search = gck_session_create_object (session, template, NULL, &error);
	gck_attributes_unref (template);

	if (search == NULL) {
		g_warning ("couldn't perform search for gpg agent stored passphrases: %s",
		           egg_error_message (error));
		g_clear_error (&error);
		return NULL;
	}

	data = gck_object_get_data (search, CKA_G_MATCHED, NULL, &n_data, &error);
	gck_object_destroy (search, NULL, NULL);
	g_object_unref (search);

	if (data == NULL) {
		g_warning ("couldn't retrieve list of gpg agent stored passphrases: %s",
		           egg_error_message (error));
		g_clear_error (&error);
		return NULL;
	}

	results = gck_objects_from_handle_array (session, data, n_data / sizeof (CK_ULONG));

	g_free (data);
	return results;
}

static void
do_save_password (GckSession *session, const gchar *keyid, const gchar *description,
                  const gchar *password, GckAttributes *options)
{
	GckAttributes *attrs;
	gpointer identifier;
	gsize n_identifier;
	GList *previous;
	GError *error = NULL;
	GckObject *item;
	gchar *text;
	gchar *label;
	gint i;

	g_assert (password);
	g_assert (keyid);

	/* Sending a password, needs to be secure */
	attrs = gck_attributes_new_full (egg_secure_realloc);

	/* Build up basic set of attributes */
	gck_attributes_add_boolean (attrs, CKA_TOKEN, TRUE);
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);
	keyid_to_field_attribute (keyid, attrs);

	/* Bring in all the unlock options */
	for (i = 0; options && i < gck_attributes_count (options); ++i)
		gck_attributes_add (attrs, gck_attributes_at (options, i));

	/* Find a previously stored object like this, and replace if so */
	previous = find_saved_items (session, attrs);
	if (previous) {
		identifier = gck_object_get_data (previous->data, CKA_ID, NULL, &n_identifier, NULL);
		if (identifier != NULL)
			gck_attributes_add_data (attrs, CKA_ID, identifier, n_identifier);
		g_free (identifier);
		gck_list_unref_free (previous);
	}

	text = calculate_label_for_key (keyid, description);
	label = g_strdup_printf (_("PGP Key: %s"), text);
	g_free (text);

	/* Put in the remainder of the attributes */
	gck_attributes_add_string (attrs, CKA_VALUE, password);
	gck_attributes_add_string (attrs, CKA_LABEL, label);
	g_free (label);

	item = gck_session_create_object (session, attrs, NULL, &error);
	if (item == NULL) {
		g_warning ("couldn't store gpg agent password: %s", egg_error_message (error));
		g_clear_error (&error);
	}

	if (item != NULL)
		g_object_unref (item);
	gck_attributes_unref (attrs);
}

static gboolean
do_clear_password (GckSession *session, const gchar *keyid)
{
	GckAttributes *attrs;
	GList *objects, *l;
	GError *error = NULL;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);
	keyid_to_field_attribute (keyid, attrs);

	objects = find_saved_items (session, attrs);
	gck_attributes_unref (attrs);

	if (!objects)
		return TRUE;

	/* Delete first item */
	for (l = objects; l; l = g_list_next (l)) {
		if (gck_object_destroy (l->data, NULL, &error)) {
			break; /* Only delete the first item */
		} else {
			g_warning ("couldn't clear gpg agent password: %s",
			           egg_error_message (error));
			g_clear_error (&error);
		}
	}

	gck_list_unref_free (objects);
	return TRUE;
}

static gchar*
do_lookup_password (GckSession *session, const gchar *keyid)
{
	GckAttributes *attrs;
	GList *objects, *l;
	GError *error = NULL;
	gpointer data = NULL;
	gsize n_data;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_SECRET_KEY);
	keyid_to_field_attribute (keyid, attrs);

	objects = find_saved_items (session, attrs);
	gck_attributes_unref (attrs);

	if (!objects)
		return NULL;

	/* Return first password */
	for (l = objects; l; l = g_list_next (l)) {
		data = gck_object_get_data_full (l->data, CKA_VALUE, egg_secure_realloc, NULL, &n_data, &error);
		if (error) {
			g_warning ("couldn't lookup gpg agent password: %s", egg_error_message (error));
			g_clear_error (&error);
			data = NULL;
		} else {
			break;
		}
	}

	gck_list_unref_free (objects);

	/* Data is null terminated */
	return data;
}

static void
load_unlock_options (GkuPrompt *prompt)
{
	GSettings *settings;
	gchar *method;
	gint ttl;

	settings = gkd_gpg_agent_settings ();

	method = g_settings_get_string (settings, "gpg-cache-method");
	if (!method) {
		method = g_strdup (GCR_UNLOCK_OPTION_SESSION);

	/* COMPAT: with old seahorse-agent settings that were migrated */
	} else if (g_str_equal (method, "gnome")) {
		g_free (method);
		method = g_strdup (GCR_UNLOCK_OPTION_ALWAYS);
	} else if (g_str_equal (method, "internal")) {
		g_free (method);
		method = g_strdup (GCR_UNLOCK_OPTION_SESSION);
	}

	gku_prompt_set_unlock_choice (prompt, method);
	g_free (method);

	ttl = g_settings_get_int (settings, "gpg-cache-ttl");
	gku_prompt_set_unlock_ttl (prompt, ttl <= 0 ? 1 : (guint)ttl);
}

static void
save_unlock_options (GkuPrompt *prompt)
{
	GSettings *settings;
	const gchar *method;
	gint ttl;

	settings = gkd_gpg_agent_settings ();

	method = gku_prompt_get_unlock_choice (prompt);
	if (method)
		g_settings_set_string (settings, "gpg-cache-method", method);

	ttl = gku_prompt_get_unlock_ttl (prompt);
	if (ttl >= 0)
		g_settings_set_int (settings, "gpg-cache-ttl", (gint)ttl);
}

static GkuPrompt*
prepare_password_prompt (GckSession *session, const gchar *errmsg, const gchar *prompt_text,
                         const gchar *description, gboolean confirm)
{
	GkuPrompt *prompt;
	GError *error = NULL;
	gboolean auto_unlock;
	GList *objects;
	GckAttributes *attrs;

	g_assert (GCK_IS_SESSION (session));

	prompt = gku_prompt_new ();

	gku_prompt_set_title (prompt, _("Enter Passphrase"));
	gku_prompt_set_primary_text (prompt, prompt_text ? prompt_text : _("Enter Passphrase"));
	gku_prompt_set_secondary_text (prompt, description);

	gku_prompt_hide_widget (prompt, "name_area");
	if (confirm)
		gku_prompt_show_widget (prompt, "confirm_area");
	else
		gku_prompt_hide_widget (prompt, "confirm_area");
	gku_prompt_show_widget (prompt, "password_area");
	gku_prompt_show_widget (prompt, "details_area");
	gku_prompt_show_widget (prompt, "options_area");

	auto_unlock = FALSE;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_G_COLLECTION);
	gck_attributes_add_string (attrs, CKA_ID, "login");
	gck_attributes_add_boolean (attrs, CKA_G_LOCKED, FALSE);

	/* Check if the login keyring is usable */
	objects = gck_session_find_objects (session, attrs, NULL, &error);
	gck_attributes_unref (attrs);

	if (errmsg)
		gku_prompt_set_warning (prompt, errmsg);

	if (error) {
		g_warning ("gpg agent couldn't lookup for login keyring: %s", egg_error_message (error));
		g_clear_error (&error);
	} else if (objects) {
		auto_unlock = TRUE;
	}

	gku_prompt_set_unlock_sensitive (prompt, GCR_UNLOCK_OPTION_ALWAYS, auto_unlock, NULL);
	gku_prompt_set_unlock_label (prompt, GCR_UNLOCK_OPTION_IDLE, _("Forget this password if idle for"));
	gku_prompt_set_unlock_label (prompt, GCR_UNLOCK_OPTION_TIMEOUT, _("Forget this password after"));
	gku_prompt_set_unlock_label (prompt, GCR_UNLOCK_OPTION_SESSION, _("Forget this password when I log out"));

	load_unlock_options (prompt);

	gck_list_unref_free (objects);

	return prompt;
}

static GkuPrompt*
on_prompt_attention (gpointer user_data)
{
	/* We passed the prompt as the argument */
	return g_object_ref (user_data);
}

static gchar*
do_get_password (GckSession *session, const gchar *keyid, const gchar *errmsg,
                 const gchar *prompt_text, const gchar *description, gboolean confirm)
{
	GckAttributes *attrs;
	gchar *password = NULL;
	GkuPrompt *prompt;
	const gchar *choice;
	guint ttl;

	g_assert (GCK_IS_SESSION (session));
	g_assert (keyid);

	password = do_lookup_password (session, keyid);
	if (password != NULL)
		return password;

	/* Do we have the keyid? */
	prompt = prepare_password_prompt (session, errmsg, prompt_text, description, confirm);

	gku_prompt_request_attention_sync (NULL, on_prompt_attention,
	                                   g_object_ref (prompt), g_object_unref);

	if (gku_prompt_get_response (prompt) == GKU_RESPONSE_OK) {
		password = gku_prompt_get_password (prompt, "password");
		g_return_val_if_fail (password, NULL);

		/* Load up the save options */
		attrs = gck_attributes_new ();

		choice = gku_prompt_get_unlock_choice (prompt);
		ttl = gku_prompt_get_unlock_ttl (prompt);

		if (g_str_equal (choice, GCR_UNLOCK_OPTION_ALWAYS))
			gck_attributes_add_string (attrs, CKA_G_COLLECTION, "login");
		else
			gck_attributes_add_string (attrs, CKA_G_COLLECTION, "session");

		if (g_str_equal (choice, GCR_UNLOCK_OPTION_IDLE))
			gck_attributes_add_ulong (attrs, CKA_G_DESTRUCT_IDLE, ttl);

		else if (g_str_equal (choice, GCR_UNLOCK_OPTION_TIMEOUT))
			gck_attributes_add_ulong (attrs, CKA_G_DESTRUCT_AFTER, ttl);

		/* Now actually save the password */
		do_save_password (session, keyid, description, password, attrs);
		gck_attributes_unref (attrs);

		save_unlock_options (prompt);
	}

	g_object_unref (prompt);
	return password;
}

/* ----------------------------------------------------------------------------------
 * PARSING and UTIL
 */

/* Is the argument a assuan null parameter? */
static gboolean
is_null_argument (gchar *arg)
{
	return (strcmp (arg, "X") == 0);
}

static const gchar HEX_CHARS[] = "0123456789ABCDEF";

/* Decode an assuan parameter */
static void
decode_assuan_arg (gchar *arg)
{
	gchar *t;
	gint len;

	for (len = strlen (arg); len > 0; arg++, len--) {
		switch (*arg) {
		/* + becomes a space */
		case '+':
			*arg = ' ';
			break;

		/* hex encoded as in URIs */
		case '%':
			*arg = '?';
			t = strchr (HEX_CHARS, arg[1]);
			if (t != NULL) {
				*arg = ((t - HEX_CHARS) & 0xf) << 4;
				t = strchr (HEX_CHARS, arg[2]);
				if (t != NULL)
					*arg |= (t - HEX_CHARS) & 0xf;
			}
			len -= 2;
			if (len < 1) /* last char, null terminate */
				arg[1] = 0;
			else /* collapse rest */
				memmove (arg + 1, arg + 3, len);
			break;
		};
	}
}

/* Parse an assuan argument that we recognize */
static guint32
parse_assuan_flag (gchar *flag)
{
	g_assert (flag);
	if (g_str_equal (flag, GPG_AGENT_FLAG_DATA))
		return GKD_GPG_AGENT_PASS_AS_DATA;
	else if (g_str_has_prefix (flag, GPG_AGENT_FLAG_REPEAT)) {
		gint count = 1;

		flag += strlen(GPG_AGENT_FLAG_REPEAT);
		if (*flag == '=') {
			count = atoi (++flag);
			if (!(count == 0 || count == 1))
				g_warning ("--repeat=%d treated as --repeat=1", count);
		}

		if (count)
			return GKD_GPG_AGENT_REPEAT;
	}
	return 0;
}

/* Split a line into each of it's arguments. This modifies line */
static void
split_arguments (gchar *line, guint32 *flags, ...)
{
	gchar **cur;
	gchar *flag;
	va_list ap;

	va_start (ap, flags);

	/* Initial white space */
	while (*line && isspace (*line))
		line++;

	/* The flags */
	if (flags) {
		*flags = 0;

		while (*line) {
			/* Options start with a double dash */
			if(!(line[0] == '-' && line[1] == '-'))
				break;
			line +=2;
			flag = line;

			/* All non-whitespace */
			while (*line && !isspace (*line))
				line++;

			/* Skip and null any whitespace */
			while (*line && isspace (*line)) {
				*line = 0;
				line++;
			}

			*flags |= parse_assuan_flag (flag);
		}
	}

	/* The arguments */
	while ((cur = va_arg (ap, gchar **)) != NULL) {
		if (*line) {
			*cur = line;

			/* All non-whitespace */
			while (*line && !isspace (*line))
				line++;

			/* Skip and null any whitespace */
			while (*line && isspace (*line)) {
				*line = 0;
				line++;
			}

			decode_assuan_arg (*cur);
		} else {
			*cur = NULL;
		}
	}

	va_end (ap);
}

static guint
x11_display_dot_offset (const gchar *d)
{
	const gchar *p;
	guint l = strlen (d);

	for (p = d + l; *p != '.'; --p) {
		if (p <= d)
			break;
		if (*p == ':')
			break;
	}
	if (*p == '.')
		l = p - d;

	return l;
}

/*
 * Displays are of the form: hostname:displaynumber.screennumber, where
 * hostname can be empty (to indicate a local connection).
 * Two displays are equivalent if their hostnames and displaynumbers match.
 */
static gboolean
x11_displays_eq (const gchar *d1, const gchar *d2)
{
	guint l1, l2;
	l1 = x11_display_dot_offset (d1);
	l2 = x11_display_dot_offset (d2);
	return (g_ascii_strncasecmp (d1, d2, l1 > l2 ? l1 : l2) == 0);
}

/* Does command have option? */
static gboolean
command_has_option (gchar *command, gchar *option)
{
	gboolean has_option = FALSE;

	if (!strcmp (command, GPG_AGENT_GETPASS)) {
		has_option = (!strcmp (option, GPG_AGENT_FLAG_DATA) ||
		              !strcmp (option, GPG_AGENT_FLAG_REPEAT));
	}
	/* else if (other commands) */

	return has_option;
}

/* Encode a password in hex */
static gchar*
encode_password (const gchar *pass)
{
	static const char HEXC[] = "0123456789abcdef";
	int j, c;
	gchar *enc, *k;

	/* Encode the password */
	c = sizeof (gchar *) * ((strlen (pass) * 2) + 1);
	k = enc = egg_secure_alloc (c);

	/* Simple hex encoding */
	while (*pass) {
		j = *(pass) >> 4 & 0xf;
		*(k++) = HEXC[j];

		j = *(pass++) & 0xf;
		*(k++) = HEXC[j];
	}

	return enc;
}

/* ----------------------------------------------------------------------------------
 * OPERATIONS
 */

gboolean
gkd_gpg_agent_ops_options (GkdGpgAgentCall *call, gchar *args)
{
	gchar *option;
	gsize len;

	split_arguments (args, NULL, &option, NULL);
	if (!option) {
		g_message ("received invalid option argument");
		return gkd_gpg_agent_send_reply (call, FALSE, "105 parameter error");
	}

	/*
	 * If the option is a display option we make sure it's
	 * the same as our display. Otherwise we don't answer.
	 */
	len = strlen (GPG_AGENT_OPT_DISPLAY);
	if (g_ascii_strncasecmp (option, GPG_AGENT_OPT_DISPLAY, len) == 0) {
		option += len;

		if (x11_displays_eq (option, g_getenv ("DISPLAY"))) {
			call->terminal_ok = TRUE;
		} else {
			g_message ("received request different display: %s", option);
			return gkd_gpg_agent_send_reply (call, FALSE, "105 parameter conflict");
		}
	}

	/* We don't do anything with the other options right now */
	return gkd_gpg_agent_send_reply (call, TRUE, NULL);
}

gboolean
gkd_gpg_agent_ops_getpass (GkdGpgAgentCall *call, gchar *args)
{
	gchar *id;
	gchar *errmsg;
	gchar *prompt;
	gchar *description;
	GckSession *session;
	gchar *password;
	gchar *encoded;
	guint32 flags;

	/* We don't answer this unless it's from the right terminal */
	if (!call->terminal_ok) {
		g_message ("received passphrase request from wrong terminal");
		return gkd_gpg_agent_send_reply (call, FALSE, "113 Server Resource Problem");
	}

	split_arguments (args, &flags, &id, &errmsg, &prompt, &description, NULL);

	if (!id || !errmsg || !prompt || !description) {
		g_message ("received invalid passphrase request");
		return gkd_gpg_agent_send_reply (call, FALSE, "105 parameter error");
	}

	if (is_null_argument (id))
		id = NULL;
	if (is_null_argument (errmsg))
		errmsg = NULL;
	if (is_null_argument (prompt))
		prompt = NULL;
	if (is_null_argument (description))
		description = NULL;

	session = gkd_gpg_agent_checkout_main_session ();
	g_return_val_if_fail (session, FALSE);

	password = do_get_password (session, id, errmsg, prompt, description,
	                            flags & GKD_GPG_AGENT_REPEAT);

	gkd_gpg_agent_checkin_main_session (session);

	if (password == NULL) {
		gkd_gpg_agent_send_reply (call, FALSE, "111 cancelled");
	} else if (flags & GKD_GPG_AGENT_PASS_AS_DATA) {
		gkd_gpg_agent_send_data (call, password);
		gkd_gpg_agent_send_reply (call, TRUE, NULL);
	} else {
		encoded = encode_password (password);
		gkd_gpg_agent_send_reply (call, TRUE, encoded);
		egg_secure_strfree (encoded);
	}

	egg_secure_strfree (password);
	return TRUE;
}

gboolean
gkd_gpg_agent_ops_clrpass (GkdGpgAgentCall *call, gchar *args)
{
	GckSession *session;
	gchar *id;

	/* We don't answer this unless it's from the right terminal */
	if (!call->terminal_ok) {
		g_message ("received passphrase request from wrong terminal");
		return gkd_gpg_agent_send_reply (call, FALSE, "113 Server Resource Problem");
	}

	split_arguments (args, NULL, &id, NULL);

	if (!id) {
		gkd_gpg_agent_send_reply (call, FALSE, "105 parameter error");
		g_warning ("received invalid clear pass request: %s", args);
	}

	session = gkd_gpg_agent_checkout_main_session ();
	g_return_val_if_fail (session, FALSE);

	/* Ignore the result, always return success */
	do_clear_password (session, id);

	gkd_gpg_agent_checkin_main_session (session);

	gkd_gpg_agent_send_reply (call, TRUE, NULL);
	return TRUE;
}

gboolean
gkd_gpg_agent_ops_getinfo (GkdGpgAgentCall *call, gchar *request)
{
	gchar *args;
	gboolean implemented = FALSE;

	args = strchr (request, ' ');
	if (args) {
		*args = 0;
		args++;
		while (isspace (*args))
			args++;
	}

	if (!strcmp (request, "cmd_has_option")) {
		gchar *command = args;
		gchar *option;

		if (!command || !*command)
			return gkd_gpg_agent_send_reply (call, FALSE, "105 parameter error");

		option = strchr(args, ' ');

		if (option) {
			*option = 0;
			option++;
			while (isspace (*option))
				option++;
		} else {
			return gkd_gpg_agent_send_reply (call, FALSE, "105 parameter error");
		}

		implemented = command_has_option (command, option);
	}

	/* else if (other info request) */

	if (implemented)
		return gkd_gpg_agent_send_reply (call, TRUE, NULL);
	else
		return gkd_gpg_agent_send_reply (call, FALSE, "100 not implemented");
}

gboolean
gkd_gpg_agent_ops_nop (GkdGpgAgentCall *call, gchar *args)
{
	return gkd_gpg_agent_send_reply (call, TRUE, NULL);
}

gboolean
gkd_gpg_agent_ops_bye (GkdGpgAgentCall *call, gchar *args)
{
	gkd_gpg_agent_send_reply (call, TRUE, "closing connection");
	return FALSE;
}

gboolean
gkd_gpg_agent_ops_reset (GkdGpgAgentCall *call, gchar *args)
{
	/* We keep no state :) */
	return gkd_gpg_agent_send_reply (call, TRUE, NULL);
}

gboolean
gkd_gpg_agent_ops_id (GkdGpgAgentCall *call, gchar *args)
{
	return gkd_gpg_agent_send_reply (call, TRUE, "gnome-keyring-daemon");
}
