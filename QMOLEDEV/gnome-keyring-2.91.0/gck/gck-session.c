/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-session.h - the GObject PKCS#11 wrapper library

   Copyright (C) 2008, Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <nielsen@memberwebs.com>
*/

#include "config.h"

#include "gck.h"
#include "gck-marshal.h"
#include "gck-private.h"

#include <string.h>

#include <glib/gi18n.h>

/**
 * SECTION:gck-session
 * @title: GckSession
 * @short_description: Represents an open PKCS11 session.
 *
 * Before performing any PKCS11 operations, a session must be opened. This is
 * analogous to an open database handle, or a file handle.
 */

/**
 * GckSession:
 *
 * Represents an open PKCS11 session.
 */

/**
 * GckMechanism:
 * @type: The mechanism type
 * @parameter: Mechanism specific data.
 * @n_parameter: Length of mechanism specific data.
 *
 * Represents a mechanism used with crypto operations.
 */

enum {
	DISCARD_HANDLE,
	LAST_SIGNAL
};

enum {
	PROP_0,
	PROP_MODULE,
	PROP_HANDLE,
	PROP_SLOT,
	PROP_OPTIONS,
};

struct _GckSessionPrivate {
	GckSlot *slot;
	GckModule *module;
	CK_SESSION_HANDLE handle;
	guint options;

	/* Modified atomically */
	gint discarded;
};

G_DEFINE_TYPE (GckSession, gck_session, G_TYPE_OBJECT);

static guint signals[LAST_SIGNAL] = { 0 };

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static gboolean
gck_session_real_discard_handle (GckSession *self, CK_OBJECT_HANDLE handle)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_RV rv;

	/* The default functionality, close the handle */

	g_return_val_if_fail (self->pv->module, FALSE);
	g_object_ref (self->pv->module);

	funcs = gck_module_get_functions (self->pv->module);
	g_return_val_if_fail (funcs, FALSE);

	rv = (funcs->C_CloseSession) (handle);
	if (rv != CKR_OK) {
		g_warning ("couldn't close session properly: %s",
		           gck_message_from_rv (rv));
	}

	g_object_unref (self->pv->module);
	return TRUE;
}

static void
gck_session_init (GckSession *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCK_TYPE_SESSION, GckSessionPrivate);
}

static void
gck_session_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GckSession *self = GCK_SESSION (obj);

	switch (prop_id) {
	case PROP_MODULE:
		g_value_take_object (value, gck_session_get_module (self));
		break;
	case PROP_HANDLE:
		g_value_set_ulong (value, gck_session_get_handle (self));
		break;
	case PROP_SLOT:
		g_value_take_object (value, gck_session_get_slot (self));
		break;
	case PROP_OPTIONS:
		g_value_set_uint (value, gck_session_get_options (self));
		break;
	}
}

static void
gck_session_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GckSession *self = GCK_SESSION (obj);

	/* Only valid calls are from constructor */

	switch (prop_id) {
	case PROP_MODULE:
		g_return_if_fail (!self->pv->module);
		self->pv->module = g_value_dup_object (value);
		g_return_if_fail (self->pv->module);
		break;
	case PROP_HANDLE:
		g_return_if_fail (!self->pv->handle);
		self->pv->handle = g_value_get_ulong (value);
		break;
	case PROP_SLOT:
		g_return_if_fail (!self->pv->slot);
		self->pv->slot = g_value_dup_object (value);
		g_return_if_fail (self->pv->slot);
		break;
	case PROP_OPTIONS:
		g_return_if_fail (!self->pv->options);
		self->pv->options = g_value_get_uint (value);
		break;
	}
}

static void
gck_session_dispose (GObject *obj)
{
	GckSession *self = GCK_SESSION (obj);
	gboolean handled;

	g_return_if_fail (GCK_IS_SESSION (self));

	if (g_atomic_int_compare_and_exchange (&self->pv->discarded, 0, 1)) {

		/*
		 * Let the world know that we're discarding the session
		 * handle. This allows any necessary session reuse to work.
		 */

		g_signal_emit_by_name (self, "discard-handle", self->pv->handle, &handled);
		g_return_if_fail (handled);
	}

	G_OBJECT_CLASS (gck_session_parent_class)->dispose (obj);
}

static void
gck_session_finalize (GObject *obj)
{
	GckSession *self = GCK_SESSION (obj);

	g_assert (g_atomic_int_get (&self->pv->discarded) != 0);

	if (self->pv->slot)
		g_object_unref (self->pv->slot);
	self->pv->slot = NULL;

	if (self->pv->module)
		g_object_unref (self->pv->module);
	self->pv->module = NULL;

	G_OBJECT_CLASS (gck_session_parent_class)->finalize (obj);
}

static void
gck_session_class_init (GckSessionClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gck_session_parent_class = g_type_class_peek_parent (klass);

	gobject_class->get_property = gck_session_get_property;
	gobject_class->set_property = gck_session_set_property;
	gobject_class->dispose = gck_session_dispose;
	gobject_class->finalize = gck_session_finalize;

	klass->discard_handle = gck_session_real_discard_handle;

	/**
	 * GckSession:module:
	 *
	 * The GckModule that this session is opened on.
	 */
	g_object_class_install_property (gobject_class, PROP_MODULE,
		g_param_spec_object ("module", "Module", "PKCS11 Module",
		                     GCK_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckSession:handle:
	 *
	 * The raw CK_SESSION_HANDLE handle of this session.
	 */
	g_object_class_install_property (gobject_class, PROP_HANDLE,
		g_param_spec_ulong ("handle", "Session Handle", "PKCS11 Session Handle",
		                    0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckSession:slot:
	 *
	 * The GckSlot this session is opened on.
	 */
	g_object_class_install_property (gobject_class, PROP_SLOT,
		g_param_spec_object ("slot", "Slot that this session uses", "PKCS11 Slot",
		                     GCK_TYPE_SLOT, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckSession:options:
	 *
	 * The options this session was opened with.
	 */
	g_object_class_install_property (gobject_class, PROP_OPTIONS,
		g_param_spec_uint ("options", "Session Options", "Session Options",
		                   0, G_MAXUINT, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckSession::discard-handle:
	 * @session: The session.
	 * @handle: The handle being discarded.
	 *
	 * When a GckSession is being disposed of it emits this signal to allow
	 * a session pool to pick up the handle and keep it around.
	 *
	 * If no signal handler claims the handle, then it is closed. This is used by
	 * gck_module_set_pool_sessions() to implement the module session pool.
	 *
	 * Returns: Whether or not this handle was claimed.
	 */
	signals[DISCARD_HANDLE] = g_signal_new ("discard-handle", GCK_TYPE_SESSION,
	                G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GckSessionClass, discard_handle),
			g_signal_accumulator_true_handled, NULL,
			_gck_marshal_BOOLEAN__ULONG, G_TYPE_BOOLEAN, 1, G_TYPE_ULONG);

	g_type_class_add_private (klass, sizeof (GckSessionPrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * GckSessionInfo:
 * @slot_id: The handle of the PKCS11 slot that this session is opened on.
 * @state: The user login state of the session.
 * @flags: Various PKCS11 flags.
 * @device_error: The last device error that occurred from an operation on this session.
 *
 * Information about the session. This is analogous to a CK_SESSION_INFO structure.
 *
 * When done with this structure, release it using gck_session_info_free().
 */

/**
 * gck_session_info_free:
 * @session_info: Session info to free.
 *
 * Free the GckSessionInfo structure and all associated memory.
 **/
void
gck_session_info_free (GckSessionInfo *session_info)
{
	if (!session_info)
		return;
	g_free (session_info);
}

/**
 * gck_session_from_handle:
 * @slot: The slot which the session belongs to.
 * @handle: The raw PKCS#11 handle of the session.
 * @options: Session options. Those which are used during opening a session have no effect.
 *
 * Initialize a GckSession object from a raw PKCS#11 session handle.
 * Usually one would use the gck_slot_open_session() function to
 * create a session.
 *
 * Return value: The new GckSession object.
 **/
GckSession*
gck_session_from_handle (GckSlot *slot, CK_SESSION_HANDLE handle, guint options)
{
	GckModule *module;
	GckSession *session;

	g_return_val_if_fail (GCK_IS_SLOT (slot), NULL);

	module = gck_slot_get_module (slot);
	session = g_object_new (GCK_TYPE_SESSION, "module", module,
	                        "handle", handle, "slot", slot,
	                        "options", options, NULL);
	g_object_unref (module);

	return session;
}

/**
 * gck_session_get_handle:
 * @self: The session object.
 *
 * Get the raw PKCS#11 session handle from a GckSession object.
 *
 * Return value: The raw session handle.
 **/
CK_SESSION_HANDLE
gck_session_get_handle (GckSession *self)
{
	g_return_val_if_fail (GCK_IS_SESSION (self), (CK_SESSION_HANDLE)-1);
	return self->pv->handle;
}

/**
 * gck_session_get_module:
 * @self: The session object.
 *
 * Get the PKCS#11 module to which this session belongs.
 *
 * Return value: The module, which should be unreffed after use.
 **/
GckModule*
gck_session_get_module (GckSession *self)
{
	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);
	g_return_val_if_fail (GCK_IS_MODULE (self->pv->module), NULL);
	return g_object_ref (self->pv->module);
}

/**
 * gck_session_get_slot:
 * @self: The session object.
 *
 * Get the PKCS#11 slot to which this session belongs.
 *
 * Return value: The slot, which should be unreffed after use.
 **/
GckSlot*
gck_session_get_slot (GckSession *self)
{
	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);
	g_return_val_if_fail (GCK_IS_SLOT (self->pv->slot), NULL);
	return g_object_ref (self->pv->slot);
}

/**
 * gck_session_get_info:
 * @self: The session object.
 *
 * Get information about the session.
 *
 * Return value: The session info. Use the gck_session_info_free() to release
 * when done.
 **/
GckSessionInfo*
gck_session_get_info (GckSession *self)
{
	GckSessionInfo *sessioninfo;
	CK_FUNCTION_LIST_PTR funcs;
	CK_SESSION_INFO info;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);
	g_return_val_if_fail (GCK_IS_MODULE (self->pv->module), NULL);

	g_object_ref (self->pv->module);

	funcs = gck_module_get_functions (self->pv->module);
	g_return_val_if_fail (funcs, NULL);

	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetSessionInfo) (self->pv->handle, &info);

	g_object_unref (self->pv->module);

	if (rv != CKR_OK) {
		g_warning ("couldn't get session info: %s", gck_message_from_rv (rv));
		return NULL;
	}

	sessioninfo = g_new0 (GckSessionInfo, 1);
	sessioninfo->flags = info.flags;
	sessioninfo->slot_id = info.slotID;
	sessioninfo->state = info.state;
	sessioninfo->device_error = info.ulDeviceError;

	return sessioninfo;
}

gulong
gck_session_get_state (GckSession *self)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_SESSION_INFO info;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SESSION (self), 0);
	g_return_val_if_fail (GCK_IS_MODULE (self->pv->module), 0);

	g_object_ref (self->pv->module);

	funcs = gck_module_get_functions (self->pv->module);
	g_return_val_if_fail (funcs, 0);

	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetSessionInfo) (self->pv->handle, &info);

	g_object_unref (self->pv->module);

	if (rv != CKR_OK) {
		g_warning ("couldn't get session info: %s", gck_message_from_rv (rv));
		return 0;
	}

	return info.state;
}

/**
 * gck_session_get_options:
 * @self: The session to get options from.
 *
 * Get the options this session was opened with.
 *
 * Return value: The session options.
 **/
guint
gck_session_get_options (GckSession *self)
{
	g_return_val_if_fail (GCK_IS_SESSION (self), 0);
	return self->pv->options;
}

/* ---------------------------------------------------------------------------------------------
 * INIT PIN
 */

typedef struct _InitPin {
	GckArguments base;
	guchar *pin;
	gsize n_pin;
} InitPin;


static void
free_init_pin (InitPin *args)
{
	g_free (args->pin);
	g_free (args);
}

static CK_RV
perform_init_pin (InitPin *args)
{
	return (args->base.pkcs11->C_InitPIN) (args->base.handle, (CK_BYTE_PTR)args->pin,
	                                       args->n_pin);
}

/**
 * gck_session_init_pin:
 * @self: Initialize PIN for this session's slot.
 * @pin: The user's PIN, or NULL for protected authentication path.
 * @n_pin: The length of the PIN.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error.
 *
 * Initialize the user's pin on this slot that this session is opened on.
 * According to the PKCS#11 standards, the session must be logged in with
 * the CKU_SO user type.
 *
 * This call may block for an indefinite period.
 *
 * Return value: Whether successful or not.
 **/
gboolean
gck_session_init_pin (GckSession *self, const guchar *pin, gsize n_pin,
                      GCancellable *cancellable, GError **err)
{
	InitPin args = { GCK_ARGUMENTS_INIT, (guchar*)pin, n_pin };
	return _gck_call_sync (self, perform_init_pin, NULL, &args, cancellable, err);

}

/**
 * gck_session_init_pin_async:
 * @self: Initialize PIN for this session's slot.
 * @pin: The user's PIN, or NULL for protected authentication path.
 * @n_pin: The length of the PIN.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Initialize the user's pin on this slot that this session is opened on.
 * According to the PKCS#11 standards, the session must be logged in with
 * the CKU_SO user type.
 *
 * This call will return immediately and completes asynchronously.
 **/
void
gck_session_init_pin_async (GckSession *self, const guchar *pin, gsize n_pin,
                             GCancellable *cancellable, GAsyncReadyCallback callback,
                             gpointer user_data)
{
	InitPin* args = _gck_call_async_prep (self, self, perform_init_pin, NULL, sizeof (*args), free_init_pin);

	args->pin = pin && n_pin ? g_memdup (pin, n_pin) : NULL;
	args->n_pin = n_pin;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_init_pin_finish:
 * @self: The session.
 * @result: The result passed to the callback.
 * @err: A location to return an error.
 *
 * Get the result of initializing a user's PIN.
 *
 * Return value: Whether the operation was successful or not.
 **/
gboolean
gck_session_init_pin_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	return _gck_call_basic_finish (result, err);
}


/* ---------------------------------------------------------------------------------------------
 * SET PIN
 */

typedef struct _SetPin {
	GckArguments base;
	guchar *old_pin;
	gsize n_old_pin;
	guchar *new_pin;
	gsize n_new_pin;
} SetPin;

static void
free_set_pin (SetPin *args)
{
	g_free (args->old_pin);
	g_free (args->new_pin);
	g_free (args);
}

static CK_RV
perform_set_pin (SetPin *args)
{
	return (args->base.pkcs11->C_SetPIN) (args->base.handle, (CK_BYTE_PTR)args->old_pin,
	                                      args->n_old_pin, args->new_pin, args->n_new_pin);
}

/**
 * gck_session_set_pin:
 * @self: Change the PIN for this session's slot.
 * @old_pin: The user's old PIN, or NULL for protected authentication path.
 * @n_old_pin: The length of the PIN.
 * @new_pin: The user's new PIN, or NULL for protected authentication path.
 * @n_new_pin: The length of the PIN.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error.
 *
 * Change the user's pin on this slot that this session is opened on.
 *
 * This call may block for an indefinite period.
 *
 * Return value: Whether successful or not.
 **/
gboolean
gck_session_set_pin (GckSession *self, const guchar *old_pin, gsize n_old_pin,
                     const guchar *new_pin, gsize n_new_pin, GCancellable *cancellable,
                     GError **err)
{
	SetPin args = { GCK_ARGUMENTS_INIT, (guchar*)old_pin, n_old_pin, (guchar*)new_pin, n_new_pin };
	return _gck_call_sync (self, perform_set_pin, NULL, &args, cancellable, err);
}

/**
 * gck_session_set_pin_async:
 * @self: Change the PIN for this session's slot.
 * @old_pin: The user's old PIN, or NULL for protected authentication path.
 * @n_old_pin: The length of the PIN.
 * @new_pin: The user's new PIN, or NULL for protected authentication path.
 * @n_new_pin: The length of the PIN.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Change the user's pin on this slot that this session is opened on.
 *
 * This call will return immediately and completes asynchronously.
 **/
void
gck_session_set_pin_async (GckSession *self, const guchar *old_pin, gsize n_old_pin,
                            const guchar *new_pin, gsize n_new_pin, GCancellable *cancellable,
                            GAsyncReadyCallback callback, gpointer user_data)
{
	SetPin* args = _gck_call_async_prep (self, self, perform_set_pin, NULL, sizeof (*args), free_set_pin);

	args->old_pin = old_pin && n_old_pin ? g_memdup (old_pin, n_old_pin) : NULL;
	args->n_old_pin = n_old_pin;
	args->new_pin = new_pin && n_new_pin ? g_memdup (new_pin, n_new_pin) : NULL;
	args->n_new_pin = n_new_pin;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_set_pin_finish:
 * @self: The session.
 * @result: The result passed to the callback.
 * @err: A location to return an error.
 *
 * Get the result of changing a user's PIN.
 *
 * Return value: Whether the operation was successful or not.
 **/
gboolean
gck_session_set_pin_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	return _gck_call_basic_finish (result, err);
}


/* ---------------------------------------------------------------------------------------------
 * LOGIN
 */

typedef struct _Login {
	GckArguments base;
	gulong user_type;
	guchar *pin;
	gsize n_pin;
} Login;

static void
free_login (Login *args)
{
	g_free (args->pin);
	g_free (args);
}

static CK_RV
perform_login (Login *args)
{
	return (args->base.pkcs11->C_Login) (args->base.handle, args->user_type,
	                                     (CK_BYTE_PTR)args->pin, args->n_pin);
}

/**
 * gck_session_login:
 * @self: Log in to this session.
 * @user_type: The type of login user.
 * @pin: The user's PIN, or NULL for protected authentication path.
 * @n_pin: The length of the PIN.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error.
 *
 * Login the user on the session. This call may block for
 * an indefinite period.
 *
 * Return value: Whether successful or not.
 **/
gboolean
gck_session_login (GckSession *self, gulong user_type, const guchar *pin,
                   gsize n_pin, GCancellable *cancellable, GError **err)
{
	Login args = { GCK_ARGUMENTS_INIT, user_type, (guchar*)pin, n_pin };
	return _gck_call_sync (self, perform_login, NULL, &args, cancellable, err);

}

/**
 * gck_session_login_async:
 * @self: Log in to this session.
 * @user_type: The type of login user.
 * @pin: The user's PIN, or NULL for protected authentication path.
 * @n_pin: The length of the PIN.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Login the user on the session. This call will return
 * immediately and completes asynchronously.
 **/
void
gck_session_login_async (GckSession *self, gulong user_type, const guchar *pin,
                          gsize n_pin, GCancellable *cancellable, GAsyncReadyCallback callback,
                          gpointer user_data)
{
	Login* args = _gck_call_async_prep (self, self, perform_login, NULL, sizeof (*args), free_login);

	args->user_type = user_type;
	args->pin = pin && n_pin ? g_memdup (pin, n_pin) : NULL;
	args->n_pin = n_pin;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);

}

/**
 * gck_session_login_finish:
 * @self: The session logged into.
 * @result: The result passed to the callback.
 * @err: A location to return an error.
 *
 * Get the result of a login operation.
 *
 * Return value: Whether the operation was successful or not.
 **/
gboolean
gck_session_login_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	return _gck_call_basic_finish (result, err);
}




/* LOGOUT */

static CK_RV
perform_logout (GckArguments *args)
{
	return (args->pkcs11->C_Logout) (args->handle);
}

/**
 * gck_session_logout:
 * @self: Logout of this session.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error.
 *
 * Log out of the session. This call may block for an indefinite period.
 *
 * Return value: Whether the logout was successful or not.
 **/
gboolean
gck_session_logout (GckSession *self, GCancellable *cancellable, GError **err)
{
	GckArguments args = GCK_ARGUMENTS_INIT;
	return _gck_call_sync (self, perform_logout, NULL, &args, cancellable, err);
}

/**
 * gck_session_logout_async:
 * @self: Logout of this session.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Log out of the session. This call returns immediately and completes
 * asynchronously.
 **/
void
gck_session_logout_async (GckSession *self, GCancellable *cancellable,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	GckArguments *args = _gck_call_async_prep (self, self, perform_logout, NULL, 0, NULL);
	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_logout_finish:
 * @self: Logout of this session.
 * @result: The result passed to the callback.
 * @err: A location to return an error.
 *
 * Get the result of logging out of a session.
 *
 * Return value: Whether the logout was successful or not.
 **/
gboolean
gck_session_logout_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	return _gck_call_basic_finish (result, err);
}




/* CREATE OBJECT */

typedef struct _CreateObject {
	GckArguments base;
	GckAttributes *attrs;
	CK_OBJECT_HANDLE object;
} CreateObject;

static void
free_create_object (CreateObject *args)
{
	gck_attributes_unref (args->attrs);
	g_free (args);
}

static CK_RV
perform_create_object (CreateObject *args)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;

	attrs = _gck_attributes_commit_out (args->attrs, &n_attrs);

	return (args->base.pkcs11->C_CreateObject) (args->base.handle,
	                                            attrs, n_attrs,
	                                            &args->object);
}

/**
 * gck_session_create_object:
 * @self: The session to create the object on.
 * @attrs: The attributes to create the object with.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Create a new PKCS#11 object. This call may block for an
 * indefinite period.
 *
 * Return value: The newly created object or NULL if an error occurred.
 **/
GckObject*
gck_session_create_object (GckSession *self, GckAttributes *attrs,
                           GCancellable *cancellable, GError **err)
{
	CreateObject args = { GCK_ARGUMENTS_INIT, attrs, 0 };
	gboolean ret;

	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);
	g_return_val_if_fail (attrs, NULL);

	_gck_attributes_lock (attrs);
	ret = _gck_call_sync (self, perform_create_object, NULL, &args, cancellable, err);
	_gck_attributes_unlock (attrs);

	if (!ret)
		return NULL;

	return gck_object_from_handle (self, args.object);
}

/**
 * gck_session_create_object_async:
 * @self: The session to create the object on.
 * @attrs: The attributes to create the object with.
 * @cancellable: Optional cancellation object or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Create a new PKCS#11 object. This call will return immediately
 * and complete asynchronously.
 **/
void
gck_session_create_object_async (GckSession *self, GckAttributes *attrs,
                                  GCancellable *cancellable, GAsyncReadyCallback callback,
                                  gpointer user_data)
{
	CreateObject *args = _gck_call_async_prep (self, self, perform_create_object,
	                                            NULL, sizeof (*args), free_create_object);

	g_return_if_fail (attrs);

	args->attrs = gck_attributes_ref (attrs);
	_gck_attributes_lock (attrs);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_create_object_finish:
 * @self: The session to create the object on.
 * @result: The result passed to the callback.
 * @err: A location to return an error, or NULL.
 *
 * Get the result of creating a new PKCS#11 object.
 *
 * Return value: The newly created object or NULL if an error occurred.
 **/
GckObject*
gck_session_create_object_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	CreateObject *args;

	args = _gck_call_arguments (result, CreateObject);
	_gck_attributes_unlock (args->attrs);

	if (!_gck_call_basic_finish (result, err))
		return NULL;
	return gck_object_from_handle (self, args->object);
}



/* FIND OBJECTS */

typedef struct _FindObjects {
	GckArguments base;
	GckAttributes *attrs;
	CK_OBJECT_HANDLE_PTR objects;
	CK_ULONG n_objects;
} FindObjects;

static void
free_find_objects (FindObjects *args)
{
	gck_attributes_unref (args->attrs);
	g_free (args->objects);
}

static CK_RV
perform_find_objects (FindObjects *args)
{
	CK_OBJECT_HANDLE_PTR batch;
	CK_ULONG n_batch, n_found;
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;
	GArray *array;
	CK_RV rv;

	attrs = _gck_attributes_commit_out (args->attrs, &n_attrs);

	rv = (args->base.pkcs11->C_FindObjectsInit) (args->base.handle,
	                                             attrs, n_attrs);
	if (rv != CKR_OK)
		return rv;

	batch = NULL;
	n_found = n_batch = 4;
	array = g_array_new (0, 1, sizeof (CK_OBJECT_HANDLE));

	do {
		/*
		 * Reallocate and double in size:
		 *  - First time.
		 *  - Each time we found as many as batch
		 */

		if (n_found == n_batch) {
			n_batch *= 2;
			batch = g_realloc (batch, sizeof (CK_OBJECT_HANDLE) * n_batch);
		}

		rv = (args->base.pkcs11->C_FindObjects) (args->base.handle,
		                                         batch, n_batch, &n_found);
		if (rv != CKR_OK)
			break;

		g_array_append_vals (array, batch, n_found);

	} while (n_found > 0);

	g_free (batch);

	if (rv == CKR_OK) {
		args->n_objects = array->len;
		args->objects = (CK_OBJECT_HANDLE_PTR)g_array_free (array, FALSE);
		rv = (args->base.pkcs11->C_FindObjectsFinal) (args->base.handle);
	} else {
		args->objects = NULL;
		args->n_objects = 0;
		g_array_free (array, TRUE);
	}

	return rv;
}

static GList*
objlist_from_handles (GckSession *self, CK_OBJECT_HANDLE_PTR objects,
                      CK_ULONG n_objects)
{
	GList *results = NULL;

	while (n_objects > 0) {
		results = g_list_prepend (results,
		                gck_object_from_handle (self, objects[--n_objects]));
	}

	return g_list_reverse (results);
}

/**
 * gck_session_find_objects:
 * @self: The session to find objects on.
 * @attrs: The attributes to match.
 * @cancellable: Optional cancellation object or NULL.
 * @err: A location to return an error or NULL.
 *
 * Find the objects matching the passed attributes. This call may
 * block for an indefinite period.
 *
 * Return value: A list of the matching objects, which may be empty.
 **/
GList*
gck_session_find_objects (GckSession *self, GckAttributes *attrs,
                          GCancellable *cancellable, GError **err)
{
	FindObjects args = { GCK_ARGUMENTS_INIT, attrs, NULL, 0 };
	GList *results = NULL;

	g_return_val_if_fail (attrs, NULL);
	_gck_attributes_lock (attrs);

	if (_gck_call_sync (self, perform_find_objects, NULL, &args, cancellable, err))
		results = objlist_from_handles (self, args.objects, args.n_objects);

	g_free (args.objects);
	_gck_attributes_unlock (attrs);
	return results;
}

/**
 * gck_session_find_objects_async:
 * @self: The session to find objects on.
 * @attrs: The attributes to match.
 * @cancellable: Optional cancellation object or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Find the objects matching the passed attributes. This call will
 * return immediately and complete asynchronously.
 **/
void
gck_session_find_objects_async (GckSession *self, GckAttributes *attrs,
                                 GCancellable *cancellable, GAsyncReadyCallback callback,
                                 gpointer user_data)
{
	FindObjects *args = _gck_call_async_prep (self, self, perform_find_objects,
	                                           NULL, sizeof (*args), free_find_objects);
	args->attrs = gck_attributes_ref (attrs);
	_gck_attributes_lock (attrs);
	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_find_objects_finish:
 * @self: The session to find objects on.
 * @result: The attributes to match.
 * @err: A location to return an error.
 *
 * Get the result of a find operation.
 *
 * Return value: A list of the matching objects, which may be empty.
 **/
GList*
gck_session_find_objects_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	FindObjects *args;

	args = _gck_call_arguments (result, FindObjects);
	_gck_attributes_unlock (args->attrs);

	if (!_gck_call_basic_finish (result, err))
		return NULL;
	return objlist_from_handles (self, args->objects, args->n_objects);
}

/* -----------------------------------------------------------------------------
 * KEY PAIR GENERATION
 */

typedef struct _GenerateKeyPair {
	GckArguments base;
	GckMechanism mechanism;
	GckAttributes *public_attrs;
	GckAttributes *private_attrs;
	CK_OBJECT_HANDLE public_key;
	CK_OBJECT_HANDLE private_key;
} GenerateKeyPair;

static void
free_generate_key_pair (GenerateKeyPair *args)
{
	gck_attributes_unref (args->public_attrs);
	gck_attributes_unref (args->private_attrs);
	g_free (args);
}

static CK_RV
perform_generate_key_pair (GenerateKeyPair *args)
{
	CK_ATTRIBUTE_PTR pub_attrs, priv_attrs;
	CK_ULONG n_pub_attrs, n_priv_attrs;

	g_assert (sizeof (CK_MECHANISM) == sizeof (GckMechanism));

	pub_attrs = _gck_attributes_commit_out (args->public_attrs, &n_pub_attrs);
	priv_attrs = _gck_attributes_commit_out (args->private_attrs, &n_priv_attrs);

	return (args->base.pkcs11->C_GenerateKeyPair) (args->base.handle,
	                                               (CK_MECHANISM_PTR)&(args->mechanism),
	                                               pub_attrs, n_pub_attrs,
	                                               priv_attrs, n_priv_attrs,
	                                               &args->public_key,
	                                               &args->private_key);
}

/**
 * gck_session_generate_key_pair:
 * @self: The session to use.
 * @mech_type: The mechanism type to use for key generation.
 * @public_attrs: Additional attributes for the generated public key.
 * @private_attrs: Additional attributes for the generated private key.
 * @public_key: A location to return the resulting public key.
 * @private_key: A location to return the resulting private key.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Generate a new key pair of public and private keys. This call may block for an
 * indefinite period.
 *
 * Return value: TRUE if the operation succeeded.
 **/
gboolean
gck_session_generate_key_pair (GckSession *self, gulong mech_type,
                               GckAttributes *public_attrs, GckAttributes *private_attrs,
                               GckObject **public_key, GckObject **private_key,
                               GCancellable *cancellable, GError **err)
{
	GckMechanism mech = { mech_type, NULL, 0 };
	return gck_session_generate_key_pair_full (self, &mech, public_attrs, private_attrs, public_key, private_key, cancellable, err);
}

/**
 * gck_session_generate_key_pair_full:
 * @self: The session to use.
 * @mechanism: The mechanism to use for key generation.
 * @public_attrs: Additional attributes for the generated public key.
 * @private_attrs: Additional attributes for the generated private key.
 * @public_key: A location to return the resulting public key.
 * @private_key: A location to return the resulting private key.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Generate a new key pair of public and private keys. This call may block for an
 * indefinite period.
 *
 * Return value: TRUE if the operation succeeded.
 **/
gboolean
gck_session_generate_key_pair_full (GckSession *self, GckMechanism *mechanism,
                                     GckAttributes *public_attrs, GckAttributes *private_attrs,
                                     GckObject **public_key, GckObject **private_key,
                                     GCancellable *cancellable, GError **err)
{
	GenerateKeyPair args = { GCK_ARGUMENTS_INIT, GCK_MECHANISM_EMPTY, public_attrs, private_attrs, 0, 0 };
	gboolean ret;

	g_return_val_if_fail (GCK_IS_SESSION (self), FALSE);
	g_return_val_if_fail (mechanism, FALSE);
	g_return_val_if_fail (public_attrs, FALSE);
	g_return_val_if_fail (private_attrs, FALSE);
	g_return_val_if_fail (public_key, FALSE);
	g_return_val_if_fail (private_key, FALSE);

	/* Shallow copy of the mechanism structure */
	memcpy (&args.mechanism, mechanism, sizeof (args.mechanism));

	_gck_attributes_lock (public_attrs);
	if (public_attrs != private_attrs)
		_gck_attributes_lock (private_attrs);
	ret = _gck_call_sync (self, perform_generate_key_pair, NULL, &args, cancellable, err);
	if (public_attrs != private_attrs)
		_gck_attributes_unlock (private_attrs);
	_gck_attributes_unlock (public_attrs);

	if (!ret)
		return FALSE;

	*public_key = gck_object_from_handle (self, args.public_key);
	*private_key = gck_object_from_handle (self, args.private_key);
	return TRUE;
}

/**
 * gck_session_generate_key_pair_async:
 * @self: The session to use.
 * @mechanism: The mechanism to use for key generation.
 * @public_attrs: Additional attributes for the generated public key.
 * @private_attrs: Additional attributes for the generated private key.
 * @cancellable: Optional cancellation object or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Generate a new key pair of public and private keys. This call will
 * return immediately and complete asynchronously.
 **/
void
gck_session_generate_key_pair_async (GckSession *self, GckMechanism *mechanism,
                                      GckAttributes *public_attrs, GckAttributes *private_attrs,
                                      GCancellable *cancellable, GAsyncReadyCallback callback,
                                      gpointer user_data)
{
	GenerateKeyPair *args = _gck_call_async_prep (self, self, perform_generate_key_pair,
	                                               NULL, sizeof (*args), free_generate_key_pair);

	g_return_if_fail (GCK_IS_SESSION (self));
	g_return_if_fail (mechanism);
	g_return_if_fail (public_attrs);
	g_return_if_fail (private_attrs);

	/* Shallow copy of the mechanism structure */
	memcpy (&args->mechanism, mechanism, sizeof (args->mechanism));

	args->public_attrs = gck_attributes_ref (public_attrs);
	_gck_attributes_lock (public_attrs);
	args->private_attrs = gck_attributes_ref (private_attrs);
	if (public_attrs != private_attrs)
		_gck_attributes_lock (private_attrs);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_generate_key_pair_finish:
 * @self: The session to use.
 * @result: The async result passed to the callback.
 * @public_key: A location to return the resulting public key.
 * @private_key: A location to return the resulting private key.
 * @err: A location to return an error.
 *
 * Get the result of a generate key pair operation.
 *
 * Return value: TRUE if the operation succeeded.
 **/
gboolean
gck_session_generate_key_pair_finish (GckSession *self, GAsyncResult *result,
                                       GckObject **public_key, GckObject **private_key,
                                       GError **err)
{
	GenerateKeyPair *args;

	g_return_val_if_fail (GCK_IS_SESSION (self), FALSE);
	g_return_val_if_fail (public_key, FALSE);
	g_return_val_if_fail (private_key, FALSE);

	args = _gck_call_arguments (result, GenerateKeyPair);
	_gck_attributes_unlock (args->public_attrs);
	if (args->public_attrs != args->private_attrs)
		_gck_attributes_unlock (args->private_attrs);

	if (!_gck_call_basic_finish (result, err))
		return FALSE;

	*public_key = gck_object_from_handle (self, args->public_key);
	*private_key = gck_object_from_handle (self, args->private_key);
	return TRUE;
}

/* -----------------------------------------------------------------------------
 * KEY WRAPPING
 */

typedef struct _WrapKey {
	GckArguments base;
	GckMechanism mechanism;
	CK_OBJECT_HANDLE wrapper;
	CK_OBJECT_HANDLE wrapped;
	gpointer result;
	gulong n_result;
} WrapKey;

static void
free_wrap_key (WrapKey *args)
{
	g_free (args->result);
	g_free (args);
}

static CK_RV
perform_wrap_key (WrapKey *args)
{
	CK_RV rv;

	g_assert (sizeof (CK_MECHANISM) == sizeof (GckMechanism));

	/* Get the length of the result */
	rv = (args->base.pkcs11->C_WrapKey) (args->base.handle,
	                                     (CK_MECHANISM_PTR)&(args->mechanism),
	                                     args->wrapper, args->wrapped,
	                                     NULL, &args->n_result);
	if (rv != CKR_OK)
		return rv;

	/* And try again with a real buffer */
	args->result = g_malloc0 (args->n_result);
	return (args->base.pkcs11->C_WrapKey) (args->base.handle,
	                                       (CK_MECHANISM_PTR)&(args->mechanism),
	                                       args->wrapper, args->wrapped,
	                                       args->result, &args->n_result);
}

/**
 * gck_session_wrap_key:
 * @self: The session to use.
 * @wrapper: The key to use for wrapping.
 * @mechanism: The mechanism type to use for wrapping.
 * @wrapped: The key to wrap.
 * @n_result: A location in which to return the length of the wrapped data.
 * @err: A location to return an error, or NULL.
 *
 * Wrap a key into a byte stream. This call may block for an
 * indefinite period.
 *
 * Return value: The wrapped data or NULL if the operation failed.
 **/
gpointer
gck_session_wrap_key (GckSession *self, GckObject *key, gulong mech_type,
                      GckObject *wrapped, gsize *n_result, GCancellable *cancellable, GError **err)
{
	GckMechanism mech = { mech_type, NULL, 0 };
	return gck_session_wrap_key_full (self, key, &mech, wrapped, n_result, cancellable, err);
}

/**
 * gck_session_wrap_key_full:
 * @self: The session to use.
 * @wrapper: The key to use for wrapping.
 * @mechanism: The mechanism to use for wrapping.
 * @wrapped: The key to wrap.
 * @n_result: A location in which to return the length of the wrapped data.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Wrap a key into a byte stream. This call may block for an
 * indefinite period.
 *
 * Return value: The wrapped data or NULL if the operation failed.
 **/
gpointer
gck_session_wrap_key_full (GckSession *self, GckObject *wrapper, GckMechanism *mechanism,
                            GckObject *wrapped, gsize *n_result, GCancellable *cancellable,
                            GError **err)
{
	WrapKey args = { GCK_ARGUMENTS_INIT, GCK_MECHANISM_EMPTY, 0, 0, NULL, 0 };
	gboolean ret;

	g_return_val_if_fail (GCK_IS_SESSION (self), FALSE);
	g_return_val_if_fail (mechanism, FALSE);
	g_return_val_if_fail (GCK_IS_OBJECT (wrapped), FALSE);
	g_return_val_if_fail (GCK_IS_OBJECT (wrapper), FALSE);
	g_return_val_if_fail (n_result, FALSE);

	/* Shallow copy of the mechanism structure */
	memcpy (&args.mechanism, mechanism, sizeof (args.mechanism));

	g_object_get (wrapper, "handle", &args.wrapper, NULL);
	g_return_val_if_fail (args.wrapper != 0, NULL);
	g_object_get (wrapped, "handle", &args.wrapped, NULL);
	g_return_val_if_fail (args.wrapped != 0, NULL);

	ret = _gck_call_sync (self, perform_wrap_key, NULL, &args, cancellable, err);

	if (!ret)
		return FALSE;

	*n_result = args.n_result;
	return args.result;
}

/**
 * gck_session_wrap_key_async:
 * @self: The session to use.
 * @wrapper: The key to use for wrapping.
 * @mechanism: The mechanism to use for wrapping.
 * @wrapped: The key to wrap.
 * @cancellable: Optional cancellation object or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Wrap a key into a byte stream. This call will
 * return immediately and complete asynchronously.
 **/
void
gck_session_wrap_key_async (GckSession *self, GckObject *key, GckMechanism *mechanism,
                             GckObject *wrapped, GCancellable *cancellable,
                             GAsyncReadyCallback callback, gpointer user_data)
{
	WrapKey *args = _gck_call_async_prep (self, self, perform_wrap_key,
	                                       NULL, sizeof (*args), free_wrap_key);

	g_return_if_fail (GCK_IS_SESSION (self));
	g_return_if_fail (mechanism);
	g_return_if_fail (GCK_IS_OBJECT (wrapped));
	g_return_if_fail (GCK_IS_OBJECT (key));

	/* Shallow copy of the mechanism structure */
	memcpy (&args->mechanism, mechanism, sizeof (args->mechanism));

	g_object_get (key, "handle", &args->wrapper, NULL);
	g_return_if_fail (args->wrapper != 0);
	g_object_get (wrapped, "handle", &args->wrapped, NULL);
	g_return_if_fail (args->wrapped != 0);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_wrap_key_finish:
 * @self: The session to use.
 * @result: The async result passed to the callback.
 * @n_result: A location in which to return the length of the wrapped data.
 * @err: A location to return an error.
 *
 * Get the result of a wrap key operation.
 *
 * Return value: The wrapped data or NULL if the operation failed.
 **/
gpointer
gck_session_wrap_key_finish (GckSession *self, GAsyncResult *result,
                              gsize *n_result, GError **err)
{
	WrapKey *args;
	gpointer ret;

	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);
	g_return_val_if_fail (n_result, NULL);

	args = _gck_call_arguments (result, WrapKey);

	if (!_gck_call_basic_finish (result, err))
		return NULL;

	*n_result = args->n_result;
	args->n_result = 0;
	ret = args->result;
	args->result = NULL;

	return ret;
}

/* -----------------------------------------------------------------------------
 * KEY UNWRAPPING
 */

typedef struct _UnwrapKey {
	GckArguments base;
	GckMechanism mechanism;
	GckAttributes *attrs;
	CK_OBJECT_HANDLE wrapper;
	gconstpointer input;
	gulong n_input;
	CK_OBJECT_HANDLE unwrapped;
} UnwrapKey;

static void
free_unwrap_key (UnwrapKey *args)
{
	gck_attributes_unref (args->attrs);
	g_free (args);
}

static CK_RV
perform_unwrap_key (UnwrapKey *args)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;

	g_assert (sizeof (CK_MECHANISM) == sizeof (GckMechanism));

	attrs = _gck_attributes_commit_out (args->attrs, &n_attrs);

	return (args->base.pkcs11->C_UnwrapKey) (args->base.handle,
	                                         (CK_MECHANISM_PTR)&(args->mechanism),
	                                         args->wrapper, (CK_BYTE_PTR)args->input,
	                                         args->n_input, attrs, n_attrs,
	                                         &args->unwrapped);
}

/**
 * gck_session_unwrap_key:
 * @self: The session to use.
 * @wrapper: The key to use for unwrapping.
 * @mech_type: The mechanism to use for unwrapping.
 * @input: The wrapped data as a byte stream.
 * @n_input: The length of the wrapped data.
 * @attrs: Additional attributes for the unwrapped key.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Unwrap a key from a byte stream. This call may block for an
 * indefinite period.
 *
 * Return value: The new unwrapped key or NULL if the operation failed.
 **/
GckObject*
gck_session_unwrap_key (GckSession *self, GckObject *wrapper, gulong mech_type,
                        gconstpointer input, gsize n_input, GckAttributes *attrs,
                        GCancellable *cancellable, GError **err)
{
	GckMechanism mech = { mech_type, NULL, 0 };
	return gck_session_unwrap_key_full (self, wrapper, &mech, input, n_input, attrs, cancellable, err);
}

/**
 * gck_session_unwrap_key_full:
 * @self: The session to use.
 * @wrapper: The key to use for unwrapping.
 * @mechanism: The mechanism to use for unwrapping.
 * @input: The wrapped data as a byte stream.
 * @n_input: The length of the wrapped data.
 * @attrs: Additional attributes for the unwrapped key.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Unwrap a key from a byte stream. This call may block for an
 * indefinite period.
 *
 * Return value: The new unwrapped key or NULL if the operation failed.
 **/
GckObject*
gck_session_unwrap_key_full (GckSession *self, GckObject *wrapper, GckMechanism *mechanism,
                             gconstpointer input, gsize n_input, GckAttributes *attrs,
                             GCancellable *cancellable, GError **err)
{
	UnwrapKey args = { GCK_ARGUMENTS_INIT, GCK_MECHANISM_EMPTY, attrs, 0, input, n_input, 0 };
	gboolean ret;

	g_return_val_if_fail (GCK_IS_SESSION (self), FALSE);
	g_return_val_if_fail (GCK_IS_OBJECT (wrapper), FALSE);
	g_return_val_if_fail (mechanism, FALSE);
	g_return_val_if_fail (attrs, FALSE);

	/* Shallow copy of the mechanism structure */
	memcpy (&args.mechanism, mechanism, sizeof (args.mechanism));

	g_object_get (wrapper, "handle", &args.wrapper, NULL);
	g_return_val_if_fail (args.wrapper != 0, NULL);

	_gck_attributes_lock (attrs);
	ret = _gck_call_sync (self, perform_unwrap_key, NULL, &args, cancellable, err);
	_gck_attributes_unlock (attrs);

	if (!ret)
		return NULL;

	return gck_object_from_handle (self, args.unwrapped);
}

/**
 * gck_session_unwrap_key_async:
 * @self: The session to use.
 * @wrapper: The key to use for unwrapping.
 * @mechanism: The mechanism to use for unwrapping.
 * @input: The wrapped data as a byte stream.
 * @n_input: The length of the wrapped data.
 * @attrs: Additional attributes for the unwrapped key.
 * @cancellable: Optional cancellation object or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Unwrap a key from a byte stream. This call will
 * return immediately and complete asynchronously.
 **/
void
gck_session_unwrap_key_async (GckSession *self, GckObject *wrapper, GckMechanism *mechanism,
                               gconstpointer input, gsize n_input, GckAttributes *attrs,
                               GCancellable *cancellable, GAsyncReadyCallback callback,
                               gpointer user_data)
{
	UnwrapKey *args = _gck_call_async_prep (self, self, perform_unwrap_key,
	                                         NULL, sizeof (*args), free_unwrap_key);

	g_return_if_fail (GCK_IS_SESSION (self));
	g_return_if_fail (GCK_IS_OBJECT (wrapper));
	g_return_if_fail (attrs);

	g_object_get (wrapper, "handle", &args->wrapper, NULL);
	g_return_if_fail (args->wrapper != 0);

	/* Shallow copy of the mechanism structure */
	memcpy (&args->mechanism, mechanism, sizeof (args->mechanism));

	args->attrs = gck_attributes_ref (attrs);
	args->input = input;
	args->n_input = n_input;
	_gck_attributes_lock (attrs);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_wrap_key_finish:
 * @self: The session to use.
 * @result: The async result passed to the callback.
 * @err: A location to return an error.
 *
 * Get the result of a unwrap key operation.
 *
 * Return value: The new unwrapped key or NULL if the operation failed.
 **/
GckObject*
gck_session_unwrap_key_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	UnwrapKey *args;

	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);

	args = _gck_call_arguments (result, UnwrapKey);
	_gck_attributes_unlock (args->attrs);

	if (!_gck_call_basic_finish (result, err))
		return NULL;
	return gck_object_from_handle (self, args->unwrapped);
}

/* -----------------------------------------------------------------------------
 * KEY DERIVATION
 */

typedef struct _DeriveKey {
	GckArguments base;
	GckMechanism mechanism;
	GckAttributes *attrs;
	CK_OBJECT_HANDLE key;
	CK_OBJECT_HANDLE derived;
} DeriveKey;

static void
free_derive_key (DeriveKey *args)
{
	gck_attributes_unref (args->attrs);
	g_free (args);
}

static CK_RV
perform_derive_key (DeriveKey *args)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;

	g_assert (sizeof (CK_MECHANISM) == sizeof (GckMechanism));

	attrs = _gck_attributes_commit_out (args->attrs, &n_attrs);

	return (args->base.pkcs11->C_DeriveKey) (args->base.handle,
	                                         (CK_MECHANISM_PTR)&(args->mechanism),
	                                         args->key, attrs, n_attrs,
	                                         &args->derived);
}

/**
 * gck_session_derive_key:
 * @self: The session to use.
 * @base: The key to derive from.
 * @mechanism: The mechanism to use for derivation.
 * @attrs: Additional attributes for the derived key.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Derive a key from another key. This call may block for an
 * indefinite period.
 *
 * Return value: The new derived key or NULL if the operation failed.
 **/
GckObject*
gck_session_derive_key (GckSession *self, GckObject *base, gulong mech_type,
                        GckAttributes *attrs, GCancellable *cancellable, GError **err)
{
	GckMechanism mech = { mech_type, NULL, 0 };
	return gck_session_derive_key_full (self, base, &mech, attrs, cancellable, err);
}

/**
 * gck_session_derive_key_full:
 * @self: The session to use.
 * @base: The key to derive from.
 * @mechanism: The mechanism to use for derivation.
 * @attrs: Additional attributes for the derived key.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 *
 * Derive a key from another key. This call may block for an
 * indefinite period.
 *
 * Return value: The new derived key or NULL if the operation failed.
 **/
GckObject*
gck_session_derive_key_full (GckSession *self, GckObject *base, GckMechanism *mechanism,
                             GckAttributes *attrs, GCancellable *cancellable, GError **err)
{
	DeriveKey args = { GCK_ARGUMENTS_INIT, GCK_MECHANISM_EMPTY, attrs, 0, 0 };
	gboolean ret;

	g_return_val_if_fail (GCK_IS_SESSION (self), FALSE);
	g_return_val_if_fail (GCK_IS_OBJECT (base), FALSE);
	g_return_val_if_fail (mechanism, FALSE);
	g_return_val_if_fail (attrs, FALSE);

	/* Shallow copy of the mechanism structure */
	memcpy (&args.mechanism, mechanism, sizeof (args.mechanism));

	g_object_get (base, "handle", &args.key, NULL);
	g_return_val_if_fail (args.key != 0, NULL);

	_gck_attributes_lock (attrs);
	ret = _gck_call_sync (self, perform_derive_key, NULL, &args, cancellable, err);
	_gck_attributes_unlock (attrs);

	if (!ret)
		return NULL;

	return gck_object_from_handle (self, args.derived);
}

/**
 * gck_session_derive_key_async:
 * @self: The session to use.
 * @base: The key to derive from.
 * @mechanism: The mechanism to use for derivation.
 * @attrs: Additional attributes for the derived key.
 * @cancellable: Optional cancellation object or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Derive a key from another key. This call will
 * return immediately and complete asynchronously.
 **/
void
gck_session_derive_key_async (GckSession *self, GckObject *base, GckMechanism *mechanism,
                               GckAttributes *attrs, GCancellable *cancellable,
                               GAsyncReadyCallback callback, gpointer user_data)
{
	DeriveKey *args = _gck_call_async_prep (self, self, perform_derive_key,
	                                         NULL, sizeof (*args), free_derive_key);

	g_return_if_fail (GCK_IS_SESSION (self));
	g_return_if_fail (GCK_IS_OBJECT (base));
	g_return_if_fail (attrs);

	g_object_get (base, "handle", &args->key, NULL);
	g_return_if_fail (args->key != 0);

	/* Shallow copy of the mechanism structure */
	memcpy (&args->mechanism, mechanism, sizeof (args->mechanism));

	args->attrs = gck_attributes_ref (attrs);
	_gck_attributes_lock (attrs);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_wrap_key_finish:
 * @self: The session to use.
 * @result: The async result passed to the callback.
 * @err: A location to return an error.
 *
 * Get the result of a derive key operation.
 *
 * Return value: The new derived key or NULL if the operation failed.
 **/
GckObject*
gck_session_derive_key_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	DeriveKey *args;

	g_return_val_if_fail (GCK_IS_SESSION (self), NULL);

	args = _gck_call_arguments (result, DeriveKey);
	_gck_attributes_unlock (args->attrs);

	if (!_gck_call_basic_finish (result, err))
		return NULL;

	return gck_object_from_handle (self, args->derived);
}

/* --------------------------------------------------------------------------------------------------
 * AUTHENTICATE
 */

typedef enum _AuthenticateState {
	AUTHENTICATE_NONE,
	AUTHENTICATE_CAN,
	AUTHENTICATE_WANT,
	AUTHENTICATE_PERFORM
} AuthenticateState;

typedef struct _Authenticate {
	AuthenticateState state;
	gboolean protected_auth;
	GckModule *module;
	GckObject *object;
	gchar *label;
	gchar *password;
} Authenticate;

static CK_RV
authenticate_perform (Authenticate *args, GckArguments *base)
{
	CK_ATTRIBUTE attributes[2];
	CK_OBJECT_HANDLE handle;
	CK_ULONG pin_len;
	CK_BBOOL bvalue;
	CK_RV rv;

	g_assert (args);
	g_assert (base);

	switch (args->state) {

	/*
	 * Cannot authenticate for whatever reason, perhaps not
	 * enabled, or failed incomprehensibly etc.
	 *
	 */
	case AUTHENTICATE_NONE:
		return CKR_OK;

	/*
	 * Can authenticate but haven't seen if we should, yet
	 * check out the object in question.
	 */
	case AUTHENTICATE_CAN:

		handle = gck_object_get_handle (args->object);

		attributes[0].type = CKA_LABEL;
		attributes[0].pValue = NULL;
		attributes[0].ulValueLen = 0;
		attributes[1].type = CKA_ALWAYS_AUTHENTICATE;
		attributes[1].pValue = &bvalue;
		attributes[1].ulValueLen = sizeof (bvalue);

		rv = (base->pkcs11->C_GetAttributeValue) (base->handle, handle, attributes, 2);
		if (rv == CKR_ATTRIBUTE_TYPE_INVALID)
			bvalue = CK_FALSE;
		else if (rv != CKR_OK)
			return rv;

		/* No authentication needed, on this object */
		if (bvalue != CK_TRUE) {
			args->state = AUTHENTICATE_NONE;
			return CKR_OK;
		}

		/* Protected authentication path, just go to perform */
		if (args->protected_auth) {
			args->state = AUTHENTICATE_PERFORM;
			return authenticate_perform (args, base);
		}

		/* Get the label for a prompt */
		g_assert (!args->label);
		if (attributes[0].ulValueLen) {
			attributes[0].pValue = g_malloc0 (attributes[0].ulValueLen + 1);
			rv = (base->pkcs11->C_GetAttributeValue) (base->handle, handle, attributes, 2);
			if (rv == CKR_OK) {
				g_assert (!args->label);
				args->label = attributes[0].pValue;
				args->label[attributes[0].ulValueLen] = 0;
			} else {
				g_free (attributes[0].pValue);
			}
		}

		/* Need a password */
		args->state = AUTHENTICATE_WANT;
		return CKR_USER_NOT_LOGGED_IN;

	/*
	 * This state should be handled in verify_authenticate.
	 */
	case AUTHENTICATE_WANT:
		g_assert (FALSE);
		return CKR_GENERAL_ERROR;

	/*
	 * Do the actual login authentication.
	 */
	case AUTHENTICATE_PERFORM:
		pin_len = args->password ? strlen (args->password) : 0;
		rv = (base->pkcs11->C_Login) (base->handle, CKU_CONTEXT_SPECIFIC,
		                              (CK_UTF8CHAR_PTR)args->password, pin_len);
		if (rv == CKR_PIN_INCORRECT && !args->protected_auth)
			args->state = AUTHENTICATE_WANT;
		else
			args->state = AUTHENTICATE_NONE;
		return rv;

	default:
		g_assert_not_reached ();
		return CKR_GENERAL_ERROR;
	}
}

static gboolean
authenticate_complete (Authenticate *auth, GckArguments *base, CK_RV result)
{
	g_assert (auth);
	g_assert (base);

	/* We're done here if not in this state */
	if (auth->state == AUTHENTICATE_WANT) {

		g_assert (GCK_IS_MODULE (auth->module));
		g_assert (GCK_IS_OBJECT (auth->object));

		g_free (auth->password);
		auth->password = NULL;

		if (_gck_module_fire_authenticate_object (auth->module, auth->object, auth->label, &auth->password)) {
			auth->state = AUTHENTICATE_PERFORM;
			return FALSE; /* Want to continue processing this call */
		}
	}

	/* Free up various memory */
	if (auth->module)
		g_object_unref (auth->module);
	if (auth->object)
		g_object_unref (auth->object);
	g_free (auth->label);
	g_free (auth->password);

	/* The call is complete */
	return TRUE;
}

static void
authenticate_init (Authenticate *auth, GckSlot *slot, GckObject *object, guint options)
{
	GckModule *module;

	g_assert (GCK_IS_SLOT (slot));
	g_assert (GCK_IS_OBJECT (object));

	module = gck_slot_get_module (slot);
	if ((options & GCK_SESSION_AUTHENTICATE) == GCK_SESSION_AUTHENTICATE) {
		auth->state = AUTHENTICATE_CAN;
		auth->protected_auth = gck_slot_has_flags (slot, CKF_PROTECTED_AUTHENTICATION_PATH);
		auth->module = module;
		auth->object = g_object_ref (object);
	} else {
		auth->state = AUTHENTICATE_NONE;
		g_object_unref (module);
	}
}

/* --------------------------------------------------------------------------------------------------
 * COMMON CRYPTO ROUTINES
 */

typedef struct _Crypt {
	GckArguments base;

	/* Authenticator */
	Authenticate auth;

	/* Functions to call */
	CK_C_EncryptInit init_func;
	CK_C_Encrypt complete_func;

	/* Input */
	CK_OBJECT_HANDLE key;
	GckMechanism mechanism;
	guchar *input;
	CK_ULONG n_input;

	/* Output */
	guchar *result;
	CK_ULONG n_result;

} Crypt;

static CK_RV
perform_crypt (Crypt *args)
{
	CK_RV rv;

	g_assert (args);
	g_assert (args->init_func);
	g_assert (args->complete_func);
	g_assert (!args->result);
	g_assert (!args->n_result);

	/* Initialize the crypt operation */
	rv = (args->init_func) (args->base.handle, (CK_MECHANISM_PTR)&(args->mechanism), args->key);
	if (rv != CKR_OK)
		return rv;

	rv = authenticate_perform (&args->auth, &args->base);
	if (rv != CKR_OK)
		return rv;

	/* Get the length of the result */
	rv = (args->complete_func) (args->base.handle, args->input, args->n_input, NULL, &args->n_result);
	if (rv != CKR_OK)
		return rv;

	/* And try again with a real buffer */
	args->result = g_malloc0 (args->n_result);
	return (args->complete_func) (args->base.handle, args->input, args->n_input, args->result, &args->n_result);
}

static gboolean
complete_crypt (Crypt *args, CK_RV result)
{
	if (!authenticate_complete (&args->auth, &args->base, result))
		return FALSE;

	/* Call is complete */
	return TRUE;
}

static void
free_crypt (Crypt *args)
{
	g_free (args->input);
	g_free (args->result);
	g_free (args);
}

static guchar*
crypt_sync (GckSession *self, GckObject *key, GckMechanism *mechanism, const guchar *input,
            gsize n_input, gsize *n_result, GCancellable *cancellable, GError **err,
            CK_C_EncryptInit init_func, CK_C_Encrypt complete_func)
{
	Crypt args;
	GckSlot *slot;

	g_return_val_if_fail (GCK_IS_OBJECT (key), NULL);
	g_return_val_if_fail (mechanism, NULL);
	g_return_val_if_fail (init_func, NULL);
	g_return_val_if_fail (complete_func, NULL);

	memset (&args, 0, sizeof (args));
	g_object_get (key, "handle", &args.key, NULL);
	g_return_val_if_fail (args.key != 0, NULL);

	/* Shallow copy of the mechanism structure */
	memcpy (&args.mechanism, mechanism, sizeof (args.mechanism));

	/* No need to copy in this case */
	args.input = (guchar*)input;
	args.n_input = n_input;

	args.init_func = init_func;
	args.complete_func = complete_func;

	slot = gck_session_get_slot (self);
	authenticate_init (&args.auth, slot, key, self->pv->options);
	g_object_unref (slot);

	if (!_gck_call_sync (self, perform_crypt, complete_crypt, &args, cancellable, err)) {
		g_free (args.result);
		return NULL;
	}

	*n_result = args.n_result;
	return args.result;
}

static void
crypt_async (GckSession *self, GckObject *key, GckMechanism *mechanism, const guchar *input,
             gsize n_input, GCancellable *cancellable, GAsyncReadyCallback callback, gpointer user_data,
             CK_C_EncryptInit init_func, CK_C_Encrypt complete_func)
{
	Crypt *args = _gck_call_async_prep (self, self, perform_crypt, complete_crypt, sizeof (*args), free_crypt);
	GckSlot *slot;

	g_return_if_fail (GCK_IS_OBJECT (key));
	g_return_if_fail (mechanism);
	g_return_if_fail (init_func);
	g_return_if_fail (complete_func);

	g_object_get (key, "handle", &args->key, NULL);
	g_return_if_fail (args->key != 0);

	/* Shallow copy of the mechanism structure */
	memcpy (&args->mechanism, mechanism, sizeof (args->mechanism));

	args->input = input && n_input ? g_memdup (input, n_input) : NULL;
	args->n_input = n_input;

	args->init_func = init_func;
	args->complete_func = complete_func;

	slot = gck_session_get_slot (self);
	authenticate_init (&args->auth, slot, key, self->pv->options);
	g_object_unref (slot);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

static guchar*
crypt_finish (GckSession *self, GAsyncResult *result, gsize *n_result, GError **err)
{
	Crypt *args;
	guchar *res;

	if (!_gck_call_basic_finish (result, err))
		return NULL;
	args = _gck_call_arguments (result, Crypt);

	/* Steal the values from the results */
	res = args->result;
	args->result = NULL;
	*n_result = args->n_result;
	args->n_result = 0;

	return res;
}

/* --------------------------------------------------------------------------------------------------
 * ENCRYPT
 */

/**
 * gck_session_encrypt:
 * @self: The session.
 * @key: The key to encrypt with.
 * @mech_type: The mechanism type to use for encryption.
 * @input: The data to encrypt.
 * @n_input: The length of the data to encrypt.
 * @n_result: A location to store the length of the result data.
 * @err: A location to place error information.
 *
 * Encrypt data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: The data that was encrypted, or NULL if an error occured.
 */
guchar*
gck_session_encrypt (GckSession *self, GckObject *key, gulong mech_type, const guchar *input,
                      gsize n_input, gsize *n_result, GCancellable *cancellable, GError **err)
{
	GckMechanism mechanism = { mech_type, NULL, 0 };
	return gck_session_encrypt_full (self, key, &mechanism, input, n_input, n_result, cancellable, err);
}

/**
 * gck_session_encrypt_full:
 * @self: The session.
 * @key: The key to encrypt with.
 * @mechanism: The mechanism type and parameters to use for encryption.
 * @input: The data to encrypt.
 * @n_input: The length of the data to encrypt.
 * @n_result: A location to store the length of the result data.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @err: A location to place error information.
 *
 * Encrypt data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: The data that was encrypted, or NULL if an error occured.
 */
guchar*
gck_session_encrypt_full (GckSession *self, GckObject *key, GckMechanism *mechanism,
                           const guchar *input, gsize n_input, gsize *n_result,
                           GCancellable *cancellable, GError **err)
{
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;
	guchar *ret;

	g_object_get (self, "module", &module, NULL);
	g_return_val_if_fail (module != NULL, NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (module != NULL, NULL);

	ret = crypt_sync (self, key, mechanism, input, n_input, n_result, cancellable, err,
	                  funcs->C_EncryptInit, funcs->C_Encrypt);

	g_object_unref (module);
	return ret;
}

/**
 * gck_session_encrypt_async:
 * @self: The session.
 * @key: The key to encrypt with.
 * @mechanism: The mechanism type and parameters to use for encryption.
 * @input: The data to encrypt.
 * @n_input: The length of the data to encrypt.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @callback: Called when the operation completes.
 * @user_data: A pointer to pass to the callback.
 *
 * Encrypt data in a mechanism specific manner. This call will
 * return immediately and complete asynchronously.
 **/
void
gck_session_encrypt_async (GckSession *self, GckObject *key, GckMechanism *mechanism,
                            const guchar *input, gsize n_input, GCancellable *cancellable,
                            GAsyncReadyCallback callback, gpointer user_data)
{
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;

	g_object_get (self, "module", &module, NULL);
	g_return_if_fail (module != NULL);

	funcs = gck_module_get_functions (module);
	g_return_if_fail (module != NULL);

	crypt_async (self, key, mechanism, input, n_input, cancellable, callback, user_data,
	             funcs->C_EncryptInit, funcs->C_Encrypt);

	g_object_unref (module);
}

/**
 * gck_session_encrypt_finish:
 * @self: The session.
 * @result: The result object passed to the callback.
 * @n_result: A location to store the length of the result data.
 * @err: A location to place error information.
 *
 * Get the result of an encryption operation.
 *
 * Returns: The data that was encrypted, or NULL if an error occurred.
 */
guchar*
gck_session_encrypt_finish (GckSession *self, GAsyncResult *result, gsize *n_result,
                             GError **err)
{
	return crypt_finish (self, result, n_result, err);
}

/* --------------------------------------------------------------------------------------------------
 * DECRYPT
 */

/**
 * gck_session_decrypt:
 * @self: The session.
 * @key: The key to decrypt with.
 * @mech_type: The mechanism type to use for decryption.
 * @input: The data to decrypt.
 * @n_input: The length of the data to decrypt.
 * @n_result: A location to store the length of the result data.
 * @err: A location to place an error.
 *
 * Decrypt data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: The data that was decrypted, or NULL if an error occured.
 */
guchar*
gck_session_decrypt (GckSession *self, GckObject *key, gulong mech_type, const guchar *input,
                      gsize n_input, gsize *n_result, GCancellable *cancellable, GError **err)
{
	GckMechanism mechanism = { mech_type, NULL, 0 };
	return gck_session_decrypt_full (self, key, &mechanism, input, n_input, n_result, cancellable, err);
}

/**
 * gck_session_decrypt_full:
 * @self: The session.
 * @key: The key to decrypt with.
 * @mechanism: The mechanism type and parameters to use for decryption.
 * @input: The data to decrypt.
 * @n_input: The length of the data to decrypt.
 * @n_result: A location to store the length of the result data.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @err: A location to place error information.
 *
 * Decrypt data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: The data that was decrypted, or NULL if an error occured.
 */
guchar*
gck_session_decrypt_full (GckSession *self, GckObject *key, GckMechanism *mechanism,
                           const guchar *input, gsize n_input, gsize *n_result,
                           GCancellable *cancellable, GError **err)
{
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;
	guchar *ret;

	g_object_get (self, "module", &module, NULL);
	g_return_val_if_fail (module != NULL, NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (module != NULL, NULL);

	ret = crypt_sync (self, key, mechanism, input, n_input, n_result, cancellable, err,
	                  funcs->C_DecryptInit, funcs->C_Decrypt);
	g_object_unref (module);
	return ret;
}

/**
 * gck_session_decrypt_async:
 * @self: The session.
 * @key: The key to decrypt with.
 * @mechanism: The mechanism type and parameters to use for decryption.
 * @input: The data to decrypt.
 * @n_input: The length of the data to decrypt.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @callback: Called when the operation completes.
 * @user_data: A pointer to pass to the callback.
 *
 * Decrypt data in a mechanism specific manner. This call will
 * return immediately and complete asynchronously.
 */
void
gck_session_decrypt_async (GckSession *self, GckObject *key, GckMechanism *mechanism,
                            const guchar *input, gsize n_input, GCancellable *cancellable,
                            GAsyncReadyCallback callback, gpointer user_data)
{
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;

	g_object_get (self, "module", &module, NULL);
	g_return_if_fail (module != NULL);

	funcs = gck_module_get_functions (module);
	g_return_if_fail (module != NULL);

	crypt_async (self, key, mechanism, input, n_input, cancellable, callback, user_data,
	             funcs->C_DecryptInit, funcs->C_Decrypt);
	g_object_unref (module);
}

/**
 * gck_session_decrypt_finish:
 * @self: The session.
 * @result: The result object passed to the callback.
 * @n_result: A location to store the length of the result data.
 * @err: A location to place error information.
 *
 * Get the result of an decryption operation.
 *
 * Returns: The data that was decrypted, or NULL if an error occurred.
 */
guchar*
gck_session_decrypt_finish (GckSession *self, GAsyncResult *result,
                             gsize *n_result, GError **err)
{
	return crypt_finish (self, result, n_result, err);
}

/* --------------------------------------------------------------------------------------------------
 * SIGN
 */

/**
 * gck_session_sign:
 * @self: The session.
 * @key: The key to sign with.
 * @mech_type: The mechanism type to use for signing.
 * @input: The data to sign.
 * @n_input: The length of the data to sign.
 * @n_result: A location to store the length of the result data.
 * @err: A location to place an error.
 *
 * Sign data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: The data that was signed, or NULL if an error occured.
 */
guchar*
gck_session_sign (GckSession *self, GckObject *key, gulong mech_type, const guchar *input,
                   gsize n_input, gsize *n_result, GCancellable *cancellable, GError **err)
{
	GckMechanism mechanism = { mech_type, NULL, 0 };
	return gck_session_sign_full (self, key, &mechanism, input, n_input, n_result, NULL, err);
}

/**
 * gck_session_sign_full:
 * @self: The session.
 * @key: The key to sign with.
 * @mechanism: The mechanism type and parameters to use for signing.
 * @input: The data to sign.
 * @n_input: The length of the data to sign.
 * @n_result: A location to store the length of the result data.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @err: A location to place error information.
 *
 * Sign data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: The data that was signed, or NULL if an error occured.
 */
guchar*
gck_session_sign_full (GckSession *self, GckObject *key, GckMechanism *mechanism,
                        const guchar *input, gsize n_input, gsize *n_result,
                        GCancellable *cancellable, GError **err)
{
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;
	guchar *ret;

	g_object_get (self, "module", &module, NULL);
	g_return_val_if_fail (module != NULL, NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (module != NULL, NULL);

	ret = crypt_sync (self, key, mechanism, input, n_input, n_result, cancellable, err,
	                  funcs->C_SignInit, funcs->C_Sign);
	g_object_unref (module);
	return ret;
}

/**
 * gck_session_sign_async:
 * @self: The session.
 * @key: The key to sign with.
 * @mechanism: The mechanism type and parameters to use for signing.
 * @input: The data to sign.
 * @n_input: The length of the data to sign.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @callback: Called when the operation completes.
 * @user_data: A pointer to pass to the callback.
 *
 * Sign data in a mechanism specific manner. This call will
 * return immediately and complete asynchronously.
 */
void
gck_session_sign_async (GckSession *self, GckObject *key, GckMechanism *mechanism,
                         const guchar *input, gsize n_input, GCancellable *cancellable,
                         GAsyncReadyCallback callback, gpointer user_data)
{
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;

	g_object_get (self, "module", &module, NULL);
	g_return_if_fail (module != NULL);

	funcs = gck_module_get_functions (module);
	g_return_if_fail (module != NULL);

	crypt_async (self, key, mechanism, input, n_input, cancellable, callback, user_data,
	             funcs->C_SignInit, funcs->C_Sign);
	g_object_unref (module);
}

/**
 * gck_session_sign_finish:
 * @self: The session.
 * @result: The result object passed to the callback.
 * @n_result: A location to store the length of the result data.
 * @err: A location to place error information.
 *
 * Get the result of an signing operation.
 *
 * Returns: The data that was signed, or NULL if an error occurred.
 */
guchar*
gck_session_sign_finish (GckSession *self, GAsyncResult *result,
                          gsize *n_result, GError **err)
{
	return crypt_finish (self, result, n_result, err);
}

/* --------------------------------------------------------------------------------------------------
 * VERIFY
 */

typedef struct _Verify {
	GckArguments base;

	/* Authenticator */
	Authenticate auth;

	/* Input */
	CK_OBJECT_HANDLE key;
	GckMechanism mechanism;
	guchar *input;
	CK_ULONG n_input;
	guchar *signature;
	CK_ULONG n_signature;

} Verify;

static CK_RV
perform_verify (Verify *args)
{
	CK_RV rv;

	/* Initialize the crypt operation */
	rv = (args->base.pkcs11->C_VerifyInit) (args->base.handle, (CK_MECHANISM_PTR)&(args->mechanism), args->key);
	if (rv != CKR_OK)
		return rv;

	rv = authenticate_perform (&args->auth, &args->base);
	if (rv != CKR_OK)
		return rv;

	/* Do the actual verify */
	return (args->base.pkcs11->C_Verify) (args->base.handle, args->input, args->n_input,
	                                      args->signature, args->n_signature);
}

static gboolean
complete_verify (Verify *args, CK_RV result)
{
	if (!authenticate_complete (&args->auth, &args->base, result))
		return FALSE;

	/* Call is complete */
	return TRUE;
}

static void
free_verify (Verify *args)
{
	g_free (args->input);
	g_free (args->signature);
	g_free (args);
}

/**
 * gck_session_verify:
 * @self: The session.
 * @key: The key to verify with.
 * @mech_type: The mechanism type to use for verifying.
 * @input: The data to verify.
 * @n_input: The length of the data to verify.
 * @signature: The signature.
 * @n_signature: The length of the signature.
 * @err: A location to place an error.
 *
 * Verify data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: TRUE if the data verified correctly, otherwise a failure or error occurred.
 */
gboolean
gck_session_verify (GckSession *self, GckObject *key, gulong mech_type, const guchar *input,
                     gsize n_input, const guchar *signature, gsize n_signature, GCancellable *cancellable, GError **err)
{
	GckMechanism mechanism = { mech_type, NULL, 0 };
	return gck_session_verify_full (self, key, &mechanism, input, n_input,
	                                 signature, n_signature, NULL, err);
}

/**
 * gck_session_verify_full:
 * @self: The session.
 * @key: The key to verify with.
 * @mechanism: The mechanism type and parameters to use for signing.
 * @input: The data to verify.
 * @n_input: The length of the data to verify.
 * @signature: The signature.
 * @n_signature: The length of the signature.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @err: A location to place an error.
 *
 * Verify data in a mechanism specific manner. This call may
 * block for an indefinite period.
 *
 * Returns: TRUE if the data verified correctly, otherwise a failure or error occurred.
 */
gboolean
gck_session_verify_full (GckSession *self, GckObject *key, GckMechanism *mechanism,
                          const guchar *input, gsize n_input, const guchar *signature,
                          gsize n_signature, GCancellable *cancellable, GError **err)
{
	Verify args;
	GckSlot *slot;

	g_return_val_if_fail (GCK_IS_OBJECT (key), FALSE);
	g_return_val_if_fail (mechanism, FALSE);

	memset (&args, 0, sizeof (args));
	g_object_get (key, "handle", &args.key, NULL);
	g_return_val_if_fail (args.key != 0, FALSE);

	/* Shallow copy of the mechanism structure */
	memcpy (&args.mechanism, mechanism, sizeof (args.mechanism));

	/* No need to copy in this case */
	args.input = (guchar*)input;
	args.n_input = n_input;
	args.signature = (guchar*)signature;
	args.n_signature = n_signature;

	slot = gck_session_get_slot (self);
	authenticate_init (&args.auth, slot, key, self->pv->options);
	g_object_unref (slot);

	return _gck_call_sync (self, perform_verify, complete_verify, &args, cancellable, err);
}

/**
 * gck_session_verify_async:
 * @self: The session.
 * @key: The key to verify with.
 * @mechanism: The mechanism type and parameters to use for signing.
 * @input: The data to verify.
 * @n_input: The length of the data to verify.
 * @signature: The signature.
 * @n_signature: The length of the signature.
 * @cancellable: A GCancellable which can be used to cancel the operation.
 * @callback: Called when the operation completes.
 * @user_data: A pointer to pass to the callback.
 *
 * Verify data in a mechanism specific manner. This call returns
 * immediately and completes asynchronously.
 */
void
gck_session_verify_async (GckSession *self, GckObject *key, GckMechanism *mechanism,
                           const guchar *input, gsize n_input, const guchar *signature,
                           gsize n_signature, GCancellable *cancellable,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	Verify *args = _gck_call_async_prep (self, self, perform_verify, complete_verify, sizeof (*args), free_verify);
	GckSlot *slot;

	g_return_if_fail (GCK_IS_OBJECT (key));
	g_return_if_fail (mechanism);

	g_object_get (key, "handle", &args->key, NULL);
	g_return_if_fail (args->key != 0);

	/* Shallow copy of the mechanism structure */
	memcpy (&args->mechanism, mechanism, sizeof (args->mechanism));

	args->input = input && n_input ? g_memdup (input, n_input) : NULL;
	args->n_input = n_input;
	args->signature = signature && n_signature ? g_memdup (signature, n_signature) : NULL;
	args->n_signature = n_signature;

	slot = gck_session_get_slot (self);
	authenticate_init (&args->auth, slot, key, self->pv->options);
	g_object_unref (slot);

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_session_verify_finish:
 * @self: The session.
 * @result: The result object passed to the callback.
 * @err: A location to place error information.
 *
 * Get the result of an verify operation.
 *
 * Returns: TRUE if the data verified correctly, otherwise a failure or error occurred.
 */
gboolean
gck_session_verify_finish (GckSession *self, GAsyncResult *result, GError **err)
{
	return _gck_call_basic_finish (result, err);
}
