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

#include "gkd-secret-create.h"
#include "gkd-secret-error.h"
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
	PROP_PKCS11_ATTRIBUTES
};

struct _GkdSecretCreate {
	GkdSecretPrompt parent;
	GckAttributes *pkcs11_attrs;
	gchar *result_path;
};

G_DEFINE_TYPE (GkdSecretCreate, gkd_secret_create, GKD_SECRET_TYPE_PROMPT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
prepare_create_prompt (GkdSecretCreate *self)
{
	GkuPrompt *prompt;
	gchar *label;
	gchar *text;

	g_assert (GKD_SECRET_IS_CREATE (self));
	g_assert (self->pkcs11_attrs);

	prompt = GKU_PROMPT (self);

	if (!gck_attributes_find_string (self->pkcs11_attrs, CKA_LABEL, &label))
		label = g_strdup (_("Unnamed"));

	gku_prompt_reset (prompt, TRUE);

	gku_prompt_set_title (prompt, _("New Keyring Password"));
	gku_prompt_set_primary_text (prompt, _("Choose password for new keyring"));

	text = g_markup_printf_escaped (_("An application wants to create a new keyring called '%s'. "
	                                  "Choose the password you want to use for it."), label);
	gku_prompt_set_secondary_text (prompt, text);
	g_free (text);

	gku_prompt_show_widget (prompt, "password_area");
	gku_prompt_show_widget (prompt, "confirm_area");

	g_free (label);
}

static gboolean
create_collection_with_secret (GkdSecretCreate *self, GkdSecretSecret *master)
{
	DBusError derr = DBUS_ERROR_INIT;

	g_assert (GKD_SECRET_IS_CREATE (self));
	g_assert (master);
	g_assert (!self->result_path);

	self->result_path = gkd_secret_create_with_secret (self->pkcs11_attrs, master, &derr);

	if (!self->result_path) {
		g_warning ("couldn't create new collection: %s", derr.message);
		dbus_error_free (&derr);
		return FALSE;
	}

	return TRUE;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkd_secret_create_prompt_ready (GkdSecretPrompt *prompt)
{
	GkdSecretCreate *self = GKD_SECRET_CREATE (prompt);
	GkdSecretSecret *master;

	if (!gku_prompt_has_response (GKU_PROMPT (prompt))) {
		prepare_create_prompt (self);
		return;
	}

	/* Already prompted, create collection */
	g_return_if_fail (gku_prompt_get_response (GKU_PROMPT (prompt)) == GKU_RESPONSE_OK);
	master = gkd_secret_prompt_get_secret (prompt, "password");

	if (master && create_collection_with_secret (self, master))
		gkd_secret_prompt_complete (prompt);
	else
		gkd_secret_prompt_dismiss (prompt);

	gkd_secret_secret_free (master);
}

static void
gkd_secret_create_encode_result (GkdSecretPrompt *base, DBusMessageIter *iter)
{
	GkdSecretCreate *self = GKD_SECRET_CREATE (base);
	DBusMessageIter variant;
	const gchar *path;

	dbus_message_iter_open_container (iter, DBUS_TYPE_VARIANT, "o", &variant);
	path = self->result_path ? self->result_path : "/";
	dbus_message_iter_append_basic (&variant, DBUS_TYPE_OBJECT_PATH, &path);
	dbus_message_iter_close_container (iter, &variant);
}

static void
gkd_secret_create_init (GkdSecretCreate *self)
{

}

static void
gkd_secret_create_finalize (GObject *obj)
{
	GkdSecretCreate *self = GKD_SECRET_CREATE (obj);

	gck_attributes_unref (self->pkcs11_attrs);
	self->pkcs11_attrs = NULL;

	g_free (self->result_path);
	self->result_path = NULL;

	G_OBJECT_CLASS (gkd_secret_create_parent_class)->finalize (obj);
}

static void
gkd_secret_create_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	GkdSecretCreate *self = GKD_SECRET_CREATE (obj);

	switch (prop_id) {
	case PROP_PKCS11_ATTRIBUTES:
		g_return_if_fail (!self->pkcs11_attrs);
		self->pkcs11_attrs = g_value_dup_boxed (value);
		g_return_if_fail (self->pkcs11_attrs);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkd_secret_create_get_property (GObject *obj, guint prop_id, GValue *value,
                                GParamSpec *pspec)
{
	GkdSecretCreate *self = GKD_SECRET_CREATE (obj);

	switch (prop_id) {
	case PROP_PKCS11_ATTRIBUTES:
		g_value_set_boxed (value, self->pkcs11_attrs);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkd_secret_create_class_init (GkdSecretCreateClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkdSecretPromptClass *prompt_class = GKD_SECRET_PROMPT_CLASS (klass);

	gobject_class->finalize = gkd_secret_create_finalize;
	gobject_class->get_property = gkd_secret_create_get_property;
	gobject_class->set_property = gkd_secret_create_set_property;

	prompt_class->prompt_ready = gkd_secret_create_prompt_ready;
	prompt_class->encode_result = gkd_secret_create_encode_result;

	g_object_class_install_property (gobject_class, PROP_PKCS11_ATTRIBUTES,
		g_param_spec_boxed ("pkcs11-attributes", "PKCS11 Attributes", "PKCS11 Attributes",
		                     GCK_TYPE_ATTRIBUTES, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkdSecretCreate*
gkd_secret_create_new (GkdSecretService *service, const gchar *caller,
                       GckAttributes *attrs)
{
	return g_object_new (GKD_SECRET_TYPE_CREATE,
	                     "service", service,
	                     "caller", caller,
	                     "pkcs11-attributes", attrs,
	                     NULL);
}

GckObject*
gkd_secret_create_with_credential (GckSession *session, GckAttributes *attrs,
                                   GckObject *cred, GError **error)
{
	GckAttributes *atts;
	GckAttribute *attr;
	GckObject *collection;
	gboolean token;

	atts = gck_attributes_new ();
	gck_attributes_add_ulong (atts, CKA_G_CREDENTIAL, gck_object_get_handle (cred));
	gck_attributes_add_ulong (atts, CKA_CLASS, CKO_G_COLLECTION);

	attr = gck_attributes_find (attrs, CKA_LABEL);
	if (attr != NULL)
		gck_attributes_add (atts, attr);
	if (!gck_attributes_find_boolean (attrs, CKA_TOKEN, &token))
		token = FALSE;
	gck_attributes_add_boolean (atts, CKA_TOKEN, token);

	collection = gck_session_create_object (session, atts, NULL, error);
	gck_attributes_unref (atts);

	return collection;
}

gchar*
gkd_secret_create_with_secret (GckAttributes *attrs, GkdSecretSecret *master,
                               DBusError *derr)
{
	GckAttributes *atts;
	GckObject *cred;
	GckObject *collection;
	GckSession *session;
	GError *error = NULL;
	gpointer identifier;
	gsize n_identifier;
	gboolean token;
	gchar *path;

	if (!gck_attributes_find_boolean (attrs, CKA_TOKEN, &token))
		token = FALSE;

	atts = gck_attributes_new ();
	gck_attributes_add_ulong (atts, CKA_CLASS, CKO_G_CREDENTIAL);
	gck_attributes_add_boolean (atts, CKA_GNOME_TRANSIENT, TRUE);
	gck_attributes_add_boolean (atts, CKA_TOKEN, token);

	session = gkd_secret_session_get_pkcs11_session (master->session);
	g_return_val_if_fail (session, NULL);

	/* Create ourselves some credentials */
	cred = gkd_secret_session_create_credential (master->session, session, atts, master, derr);
	gck_attributes_unref (atts);

	if (cred == NULL)
		return FALSE;

	collection = gkd_secret_create_with_credential (session, attrs, cred, &error);

	gck_attributes_unref (atts);
	g_object_unref (cred);

	if (collection == NULL) {
		g_warning ("couldn't create collection: %s", egg_error_message (error));
		g_clear_error (&error);
		dbus_set_error (derr, DBUS_ERROR_FAILED, "Couldn't create new collection");
		return FALSE;
	}

	identifier = gck_object_get_data (collection, CKA_ID, NULL, &n_identifier, &error);
	g_object_unref (collection);

	if (!identifier) {
		g_warning ("couldn't lookup new collection identifier: %s", egg_error_message (error));
		g_clear_error (&error);
		dbus_set_error (derr, DBUS_ERROR_FAILED, "Couldn't find new collection just created");
		return FALSE;
	}

	path = gkd_secret_util_build_path (SECRET_COLLECTION_PREFIX, identifier, n_identifier);
	g_free (identifier);
	return path;
}
