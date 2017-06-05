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

#include "gkd-secret-change.h"
#include "gkd-secret-prompt.h"
#include "gkd-secret-secret.h"
#include "gkd-secret-service.h"
#include "gkd-secret-session.h"
#include "gkd-secret-types.h"
#include "gkd-secret-util.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

#include <gck/gck.h>

#include <string.h>

enum {
	PROP_0,
	PROP_COLLECTION_PATH
};

struct _GkdSecretChange {
	GkdSecretPrompt parent;
	gchar *collection_path;
};

G_DEFINE_TYPE (GkdSecretChange, gkd_secret_change, GKD_SECRET_TYPE_PROMPT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
prepare_change_prompt (GkdSecretChange *self, GckObject *collection, gboolean first)
{
	GError *error = NULL;
	GkuPrompt *prompt;
	gpointer data;
	gsize n_data;
	gchar *label;
	gchar *text;

	prompt = GKU_PROMPT (self);

	data = gck_object_get_data (collection, CKA_LABEL, NULL, &n_data, &error);
	if (!data) {
		g_warning ("couldn't get label for collection: %s", egg_error_message (error));
		g_clear_error (&error);
	}

	if (!data || !n_data)
		label = g_strdup (_("Unnamed"));
	else
		label = g_strndup (data, n_data);
	g_free (data);

	/* Hard reset on first prompt, soft thereafter */
	gku_prompt_reset (prompt, first);

	gku_prompt_set_title (prompt, _("Change Keyring Password"));

	text = g_markup_printf_escaped (_("Choose a new password for the '%s' keyring"), label);
	gku_prompt_set_primary_text (prompt, text);
	g_free (text);

	text = g_markup_printf_escaped (_("An application wants to change the password for the '%s' keyring. "
	                                  "Choose the new password you want to use for it."), label);
	gku_prompt_set_secondary_text (prompt, text);
	g_free (text);

	gku_prompt_hide_widget (prompt, "name_area");
	gku_prompt_hide_widget (prompt, "details_area");

	gku_prompt_show_widget (prompt, "password_area");
	gku_prompt_show_widget (prompt, "original_area");
	gku_prompt_show_widget (prompt, "confirm_area");

	g_free (label);
}

static void
set_warning_wrong (GkdSecretChange *self)
{
	g_assert (GKD_SECRET_IS_CHANGE (self));
	gku_prompt_set_warning (GKU_PROMPT (self), _("The original password was incorrect"));
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkd_secret_change_prompt_ready (GkdSecretPrompt *prompt)
{
	GkdSecretChange *self = GKD_SECRET_CHANGE (prompt);
	GkdSecretSecret *original, *master;
	DBusError derr = DBUS_ERROR_INIT;
	GckObject *collection;
	gboolean result;

	collection = gkd_secret_prompt_lookup_collection (prompt, self->collection_path);

	/* No more prompt, just go away */
	if (collection == NULL) {
		gkd_secret_prompt_dismiss (prompt);
		return;
	}

	if (!gku_prompt_has_response (GKU_PROMPT (prompt))) {
		prepare_change_prompt (self, collection, TRUE);
		return;
	}

	original = gkd_secret_prompt_get_secret (prompt, "original");
	master = gkd_secret_prompt_get_secret (prompt, "password");

	result = gkd_secret_change_with_secrets (collection, original, master, &derr);

	gkd_secret_secret_free (original);
	gkd_secret_secret_free (master);

	/* The change succeeded, yay */
	if (result) {
		gkd_secret_prompt_complete (prompt);

	/* The original password was incorrect */
	} else if (dbus_error_has_name (&derr, INTERNAL_ERROR_DENIED)) {
		prepare_change_prompt (self, collection, FALSE);
		set_warning_wrong (self);

	/* Other failures */
	} else {
		gkd_secret_prompt_dismiss (prompt);
	}

	g_object_unref (collection);
}

static void
gkd_secret_change_encode_result (GkdSecretPrompt *base, DBusMessageIter *iter)
{
	DBusMessageIter variant;
	const gchar *string = "";

	dbus_message_iter_open_container (iter, DBUS_TYPE_VARIANT, "s", &variant);
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_STRING, &string);
	dbus_message_iter_close_container (iter, &variant);
}

static void
gkd_secret_change_init (GkdSecretChange *self)
{

}

static void
gkd_secret_change_finalize (GObject *obj)
{
	GkdSecretChange *self = GKD_SECRET_CHANGE (obj);

	g_free (self->collection_path);
	self->collection_path = NULL;

	G_OBJECT_CLASS (gkd_secret_change_parent_class)->finalize (obj);
}

static void
gkd_secret_change_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	GkdSecretChange *self = GKD_SECRET_CHANGE (obj);

	switch (prop_id) {
	case PROP_COLLECTION_PATH:
		g_return_if_fail (!self->collection_path);
		self->collection_path = g_value_dup_string (value);
		g_return_if_fail (self->collection_path);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkd_secret_change_get_property (GObject *obj, guint prop_id, GValue *value,
                                GParamSpec *pspec)
{
	GkdSecretChange *self = GKD_SECRET_CHANGE (obj);

	switch (prop_id) {
	case PROP_COLLECTION_PATH:
		g_value_set_string (value, self->collection_path);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkd_secret_change_class_init (GkdSecretChangeClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkdSecretPromptClass *prompt_class = GKD_SECRET_PROMPT_CLASS (klass);

	gobject_class->finalize = gkd_secret_change_finalize;
	gobject_class->get_property = gkd_secret_change_get_property;
	gobject_class->set_property = gkd_secret_change_set_property;

	prompt_class->prompt_ready = gkd_secret_change_prompt_ready;
	prompt_class->encode_result = gkd_secret_change_encode_result;

	g_object_class_install_property (gobject_class, PROP_COLLECTION_PATH,
		g_param_spec_string ("collection-path", "Collection Path", "Collection Path",
		                     "/", G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkdSecretChange*
gkd_secret_change_new (GkdSecretService *service, const gchar *caller,
                       const gchar *path)
{
	g_return_val_if_fail (GKD_SECRET_IS_SERVICE (service), NULL);
	g_return_val_if_fail (caller, NULL);
	g_return_val_if_fail (path, NULL);

	return g_object_new (GKD_SECRET_TYPE_CHANGE,
	                     "service", service,
	                     "caller", caller,
	                     "collection-path", path,
	                     NULL);
}

gboolean
gkd_secret_change_with_secrets (GckObject *collection, GkdSecretSecret *original,
                                GkdSecretSecret *master, DBusError *derr)
{
	GError *error = NULL;
	GckAttributes *attrs = NULL;
	gboolean result = FALSE;
	GckObject *ocred = NULL;
	GckObject *mcred = NULL;

	/* Create the new credential */
	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_G_CREDENTIAL);
	gck_attributes_add_boolean (attrs, CKA_TOKEN, FALSE);
	mcred = gkd_secret_session_create_credential (master->session, NULL, attrs, master, derr);
	if (mcred == NULL)
		goto cleanup;

	/* Create the original credential, in order to make sure we can the collection */
	gck_attributes_add_ulong (attrs, CKA_G_OBJECT, gck_object_get_handle (collection));
	ocred = gkd_secret_session_create_credential (original->session, NULL, attrs, original, derr);
	if (ocred == NULL)
		goto cleanup;

	gck_attributes_unref (attrs);
	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_G_CREDENTIAL, gck_object_get_handle (mcred));

	/* Now set the collection credentials to the first one */
	result = gck_object_set (collection, attrs, NULL, &error);

cleanup:
	if (ocred) {
		/* Always destroy the original credential */
		gck_object_destroy (ocred, NULL, NULL);
		g_object_unref (ocred);
	}
	if (mcred) {
		/* Destroy the master credential if failed */
		if (!result)
			gck_object_destroy (mcred, NULL, NULL);
		g_object_unref (mcred);
	}

	gck_attributes_unref (attrs);

	if (!result && error) {
		if (g_error_matches (error, GCK_ERROR, CKR_USER_NOT_LOGGED_IN))
			dbus_set_error (derr, INTERNAL_ERROR_DENIED, "The original password was invalid");
		else
			g_warning ("failure occurred while changing password: %s", egg_error_message (error));
	}

	if (!result && !dbus_error_is_set (derr))
		dbus_set_error (derr, DBUS_ERROR_FAILED, "Couldn't change master password");

	g_clear_error (&error);
	return result;
}
