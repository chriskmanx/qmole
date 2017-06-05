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

#include "gkd-dbus-util.h"
#include "gkd-secret-dispatch.h"
#include "gkd-secret-objects.h"
#include "gkd-secret-secret.h"
#include "gkd-secret-session.h"
#include "gkd-secret-service.h"
#include "gkd-secret-types.h"
#include "gkd-secret-unlock.h"
#include "gkd-secret-util.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "login/gkd-login.h"

#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

#include <gck/gck.h>

#include <string.h>

enum {
	PROP_0,
	PROP_CALLER,
	PROP_OBJECT_PATH,
	PROP_SERVICE
};

struct _GkdSecretUnlock {
	GObject parent;
	gchar *object_path;
	GkdSecretService *service;
	gchar *caller;
	gchar *window_id;
	GQueue *queued;
	gchar *current;
	GArray *results;
	gboolean prompted;
	gboolean completed;
	GCancellable *cancellable;
};

/* Forward declarations */
static void gkd_secret_dispatch_iface (GkdSecretDispatchIface *iface);
static void perform_next_unlock (GkdSecretUnlock *self);

G_DEFINE_TYPE_WITH_CODE (GkdSecretUnlock, gkd_secret_unlock, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GKD_SECRET_TYPE_DISPATCH, gkd_secret_dispatch_iface));

static guint unique_prompt_number = 0;

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GckObject*
lookup_collection (GkdSecretUnlock *self, const gchar *path)
{
	GkdSecretObjects *objects = gkd_secret_service_get_objects (self->service);
	return gkd_secret_objects_lookup_collection (objects, self->caller, path);
}

static gboolean
check_locked_collection (GckObject *collection, gboolean *locked)
{
	GError *error = NULL;
	gpointer value;
	gsize n_value;

	value = gck_object_get_data (collection, CKA_G_LOCKED, NULL, &n_value, &error);
	if (value == NULL) {
		if (!g_error_matches (error, GCK_ERROR, CKR_OBJECT_HANDLE_INVALID))
			g_warning ("couldn't check locked status of collection: %s",
			           egg_error_message (error));
		return FALSE;
	}

	*locked = (value && n_value == sizeof (CK_BBOOL) && *(CK_BBOOL*)value);
	g_free (value);
	return TRUE;
}

static void
common_unlock_attributes (GckAttributes *attrs, GckObject *collection)
{
	g_assert (attrs);
	g_assert (GCK_IS_OBJECT (collection));
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_G_CREDENTIAL);
	gck_attributes_add_ulong (attrs, CKA_G_OBJECT, gck_object_get_handle (collection));
}

static gboolean
mark_as_complete (GkdSecretUnlock *self, gboolean dismissed)
{
	DBusMessage *signal;
	DBusMessageIter iter;
	dbus_bool_t bval;
	DBusMessageIter variant;
	DBusMessageIter array;
	const char *value;
	gint i;

	if (self->completed)
		return FALSE;
	self->completed = TRUE;

	signal = dbus_message_new_signal (self->object_path, SECRET_PROMPT_INTERFACE,
	                                  "Completed");
	dbus_message_set_destination (signal, self->caller);
	dbus_message_iter_init_append (signal, &iter);

	bval = dismissed;
	dbus_message_iter_append_basic (&iter, DBUS_TYPE_BOOLEAN, &bval);

	dbus_message_iter_open_container (&iter, DBUS_TYPE_VARIANT, "ao", &variant);
	dbus_message_iter_open_container (&variant, DBUS_TYPE_ARRAY, "o", &array);

	for (i = 0; i < self->results->len; ++i) {
		value = g_array_index (self->results, gchar*, i);
		dbus_message_iter_append_basic (&array, DBUS_TYPE_OBJECT_PATH, &value);
	}

	dbus_message_iter_close_container (&variant, &array);
	dbus_message_iter_close_container (&iter, &variant);

	gkd_secret_service_send (self->service, signal);
	dbus_message_unref (signal);
	return TRUE;
}

static void
on_unlock_complete (GObject *object, GAsyncResult *res, gpointer user_data)
{
	GkdSecretUnlock *self = GKD_SECRET_UNLOCK (user_data);
	GckObject *cred;
	GError *error = NULL;

	cred = gck_session_create_object_finish (GCK_SESSION (object), res, &error);

	/* Successfully authentication */
	if (cred) {
		g_object_unref (cred);
		g_array_append_val (self->results, self->current);
		self->current = NULL;
		perform_next_unlock (self);

	/* The user cancelled the protected auth prompt */
	} else if (g_error_matches (error, GCK_ERROR, CKR_PIN_INCORRECT)) {
		g_free (self->current);
		self->current = NULL;
		mark_as_complete (self, TRUE);

	/* The operation was cancelled via Dismiss call */
	} else if (g_error_matches (error, GCK_ERROR, CKR_CANCEL)) {
		/* Should have been the result of a dismiss */
		g_return_if_fail (self->completed);

	/* Another error, something's broken */
	} else {
		g_warning ("couldn't create credential for collection: %s",
		           egg_error_message (error));
	}

	g_clear_error (&error);

	/* refed for async call */
	g_object_unref (self);
}

static void
perform_next_unlock (GkdSecretUnlock *self)
{
	GckObject *collection;
	GckAttributes *template;
	GckSession *session;
	gboolean locked;
	gchar *objpath;

	while (!self->current) {
		objpath = g_queue_pop_head (self->queued);

		/* Nothing more to prompt for? */
		if (!objpath) {
			mark_as_complete (self, FALSE);
			break;
		}

		/* Find the collection, make sure it's still around */
		collection = lookup_collection (self, objpath);
		if (collection == NULL) {
			g_free (objpath);
			continue;
		}

		if (!check_locked_collection (collection, &locked)) {
			g_object_unref (collection);
			g_free (objpath);
			continue;

		} else if (!locked) {
			g_array_append_val (self->results, objpath);
			g_object_unref (collection);
			continue;
		}

		/* The various unlock options */
		template = gck_attributes_new ();
		common_unlock_attributes (template, collection);
		gck_attributes_add_data (template, CKA_VALUE, NULL, 0);

		session = gkd_secret_service_get_pkcs11_session (self->service, self->caller);
		gck_session_create_object_async (session, template, self->cancellable, on_unlock_complete,
		                                 g_object_ref (self));
		gck_attributes_unref (template);

		g_object_unref (collection);
		self->current = objpath;
	}
}

/* -----------------------------------------------------------------------------
 * DBUS
 */

static DBusMessage*
prompt_method_prompt (GkdSecretUnlock *self, DBusMessage *message)
{
	DBusMessage *reply;
	const char *window_id;

	/* Act as if this object no longer exists */
	if (self->completed)
		return NULL;

	if (!dbus_message_get_args (message, NULL, DBUS_TYPE_STRING,
	                            &window_id, DBUS_TYPE_INVALID))
		return NULL;

	/* Prompt can only be called once */
	if (self->prompted)
		return dbus_message_new_error (message, SECRET_ERROR_ALREADY_EXISTS,
		                               "This prompt has already been shown.");

	g_free (self->window_id);
	self->window_id = g_strdup (window_id);

	self->prompted = TRUE;
	perform_next_unlock (self);

	reply = dbus_message_new_method_return (message);
	dbus_message_append_args (reply, DBUS_TYPE_INVALID);
	return reply;
}

static DBusMessage*
prompt_method_dismiss (GkdSecretUnlock *self, DBusMessage *message)
{
	DBusMessage *reply;

	/* Act as if this object no longer exists */
	if (self->completed)
		return NULL;

	if (!dbus_message_get_args (message, NULL, DBUS_TYPE_INVALID))
		return NULL;

	g_cancellable_cancel (self->cancellable);
	mark_as_complete (self, TRUE);

	reply = dbus_message_new_method_return (message);
	dbus_message_append_args (reply, DBUS_TYPE_INVALID);
	return reply;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static DBusMessage*
gkd_secret_unlock_real_dispatch_message (GkdSecretDispatch *base, DBusMessage *message)
{
	DBusMessage *reply = NULL;
	GkdSecretUnlock *self;
	const gchar *caller;

	g_return_val_if_fail (message, NULL);
	g_return_val_if_fail (GKD_SECRET_IS_UNLOCK (base), NULL);
	self = GKD_SECRET_UNLOCK (base);

	/* This should already have been caught elsewhere */
	caller = dbus_message_get_sender (message);
	if (!caller || !g_str_equal (caller, self->caller))
		g_return_val_if_reached (NULL);

	/* org.freedesktop.Secrets.Prompt.Prompt() */
	else if (dbus_message_is_method_call (message, SECRET_PROMPT_INTERFACE, "Prompt"))
		reply = prompt_method_prompt (self, message);

	/* org.freedesktop.Secrets.Prompt.Negotiate() */
	else if (dbus_message_is_method_call (message, SECRET_PROMPT_INTERFACE, "Dismiss"))
		reply = prompt_method_dismiss (self, message);

	else if (dbus_message_has_interface (message, DBUS_INTERFACE_INTROSPECTABLE))
		return gkd_dbus_introspect_handle (message, "prompt");

	return reply;
}

static void
gkd_secret_unlock_init (GkdSecretUnlock *self)
{
	self->queued = g_queue_new ();
	self->results = g_array_new (TRUE, TRUE, sizeof (gchar*));
	self->cancellable = g_cancellable_new ();
}

static GObject*
gkd_secret_unlock_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkdSecretUnlock *self = GKD_SECRET_UNLOCK (G_OBJECT_CLASS (gkd_secret_unlock_parent_class)->constructor(type, n_props, props));

	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (self->caller, NULL);
	g_return_val_if_fail (self->service, NULL);

	/* Setup the path for the object */
	self->object_path = g_strdup_printf (SECRET_PROMPT_PREFIX "/u%d", ++unique_prompt_number);

	return G_OBJECT (self);
}

static void
gkd_secret_unlock_dispose (GObject *obj)
{
	GkdSecretUnlock *self = GKD_SECRET_UNLOCK (obj);

	g_free (self->object_path);
	self->object_path = NULL;

	if (self->service) {
		g_object_remove_weak_pointer (G_OBJECT (self->service),
		                              (gpointer*)&(self->service));
		self->service = NULL;
	}

	G_OBJECT_CLASS (gkd_secret_unlock_parent_class)->dispose (obj);
}

static void
gkd_secret_unlock_finalize (GObject *obj)
{
	GkdSecretUnlock *self = GKD_SECRET_UNLOCK (obj);

	if (self->queued) {
		while (!g_queue_is_empty (self->queued))
			g_free (g_queue_pop_head (self->queued));
		g_queue_free (self->queued);
		self->queued = NULL;
	}

	if (self->results) {
		gkd_secret_unlock_reset_results (self);
		g_array_free (self->results, TRUE);
		self->results = NULL;
	}

	g_free (self->current);
	self->current = NULL;

	g_object_unref (self->cancellable);
	self->cancellable = NULL;

	g_assert (!self->object_path);
	g_assert (!self->service);

	g_free (self->caller);
	self->caller = NULL;

	g_free (self->window_id);
	self->window_id = NULL;

	G_OBJECT_CLASS (gkd_secret_unlock_parent_class)->finalize (obj);
}

static void
gkd_secret_unlock_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	GkdSecretUnlock *self = GKD_SECRET_UNLOCK (obj);

	switch (prop_id) {
	case PROP_CALLER:
		g_return_if_fail (!self->caller);
		self->caller = g_value_dup_string (value);
		break;
	case PROP_SERVICE:
		g_return_if_fail (!self->service);
		self->service = g_value_get_object (value);
		g_return_if_fail (self->service);
		g_object_add_weak_pointer (G_OBJECT (self->service),
		                           (gpointer*)&(self->service));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkd_secret_unlock_get_property (GObject *obj, guint prop_id, GValue *value,
                                GParamSpec *pspec)
{
	GkdSecretUnlock *self = GKD_SECRET_UNLOCK (obj);

	switch (prop_id) {
	case PROP_CALLER:
		g_value_set_string (value, self->caller);
		break;
	case PROP_OBJECT_PATH:
		g_value_set_pointer (value, self->object_path);
		break;
	case PROP_SERVICE:
		g_value_set_object (value, self->service);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}


static void
gkd_secret_unlock_class_init (GkdSecretUnlockClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructor = gkd_secret_unlock_constructor;
	gobject_class->get_property = gkd_secret_unlock_get_property;
	gobject_class->set_property = gkd_secret_unlock_set_property;
	gobject_class->dispose = gkd_secret_unlock_dispose;
	gobject_class->finalize = gkd_secret_unlock_finalize;

	g_object_class_install_property (gobject_class, PROP_CALLER,
		g_param_spec_string ("caller", "Caller", "DBus caller name",
		                     NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY ));

	g_object_class_install_property (gobject_class, PROP_OBJECT_PATH,
	        g_param_spec_pointer ("object-path", "Object Path", "DBus Object Path",
		                      G_PARAM_READABLE));

	g_object_class_install_property (gobject_class, PROP_SERVICE,
		g_param_spec_object ("service", "Service", "Service which owns this prompt",
		                     GKD_SECRET_TYPE_SERVICE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
gkd_secret_dispatch_iface (GkdSecretDispatchIface *iface)
{
	iface->dispatch_message = gkd_secret_unlock_real_dispatch_message;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkdSecretUnlock*
gkd_secret_unlock_new (GkdSecretService *service, const gchar *caller)
{
	return g_object_new (GKD_SECRET_TYPE_UNLOCK, "service", service, "caller", caller, NULL);
}

void
gkd_secret_unlock_queue (GkdSecretUnlock *self, const gchar *objpath)
{
	gboolean locked = TRUE;
	GckObject *coll;
	gchar *path;

	g_return_if_fail (GKD_SECRET_IS_UNLOCK (self));
	g_return_if_fail (objpath);

	coll = lookup_collection (self, objpath);
	if (coll == NULL)
		return;

	/* Try to unlock with an empty password, which produces no prompt */
	if (gkd_secret_unlock_with_password (coll, (const guchar*)"", 0, NULL)) {
		locked = FALSE;

	}

	path = g_strdup (objpath);
	if (locked)
		g_queue_push_tail (self->queued, path);
	else
		g_array_append_val (self->results, path);

	g_object_unref (coll);
}

gboolean
gkd_secret_unlock_have_queued (GkdSecretUnlock *self)
{
	g_return_val_if_fail (GKD_SECRET_IS_UNLOCK (self), FALSE);
	return !g_queue_is_empty (self->queued) || self->current;
}

gchar**
gkd_secret_unlock_get_results (GkdSecretUnlock *self, gint *n_results)
{
	g_return_val_if_fail (GKD_SECRET_IS_UNLOCK (self), NULL);
	g_return_val_if_fail (n_results, NULL);
	*n_results = self->results->len;
	return (gchar**)self->results->data;
}

void
gkd_secret_unlock_reset_results (GkdSecretUnlock *self)
{
	gint i;

	g_return_if_fail (GKD_SECRET_IS_UNLOCK (self));

	for (i = 0; i < self->results->len; ++i)
		g_free (g_array_index (self->results, gchar*, i));
	g_array_set_size (self->results, 0);
}

gboolean
gkd_secret_unlock_with_secret (GckObject *collection, GkdSecretSecret *master,
                               DBusError *derr)
{
	GckAttributes *attrs;
	GckObject *cred;
	gboolean locked;

	g_return_val_if_fail (GCK_IS_OBJECT (collection), FALSE);
	g_return_val_if_fail (master, FALSE);

	/* Shortcut if already unlocked */
	if (check_locked_collection (collection, &locked) && !locked)
		return TRUE;

	attrs = gck_attributes_new ();
	common_unlock_attributes (attrs, collection);
	gck_attributes_add_boolean (attrs, CKA_GNOME_TRANSIENT, TRUE);
	gck_attributes_add_boolean (attrs, CKA_TOKEN, TRUE);

	cred = gkd_secret_session_create_credential (master->session, NULL, attrs, master, derr);

	gck_attributes_unref (attrs);

	if (cred != NULL)
		g_object_unref (cred);
	return (cred != NULL);
}

gboolean
gkd_secret_unlock_with_password (GckObject *collection, const guchar *password,
                                 gsize n_password, DBusError *derr)
{
	GckAttributes *attrs;
	GError *error = NULL;
	GckSession *session;
	GckObject *cred;
	gboolean locked;

	g_return_val_if_fail (GCK_IS_OBJECT (collection), FALSE);

	/* Shortcut if already unlocked */
	if (check_locked_collection (collection, &locked) && !locked)
		return TRUE;

	session = gck_object_get_session (collection);
	g_return_val_if_fail (session, FALSE);

	attrs = gck_attributes_new_full (egg_secure_realloc);
	common_unlock_attributes (attrs, collection);
	gck_attributes_add_boolean (attrs, CKA_GNOME_TRANSIENT, TRUE);
	gck_attributes_add_boolean (attrs, CKA_TOKEN, TRUE);
	gck_attributes_add_data (attrs, CKA_VALUE, password, n_password);

	cred = gck_session_create_object (session, attrs, NULL, &error);
	if (cred == NULL) {
		if (g_error_matches (error, GCK_ERROR, CKR_PIN_INCORRECT)) {
			dbus_set_error_const (derr, INTERNAL_ERROR_DENIED, "The password was incorrect.");
		} else {
			g_message ("couldn't create credential: %s", egg_error_message (error));
			dbus_set_error_const (derr, DBUS_ERROR_FAILED, "Couldn't use credentials");
		}
		g_clear_error (&error);
		return FALSE;
	}

	g_object_unref (cred);
	return TRUE;
}
