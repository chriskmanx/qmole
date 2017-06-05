/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gp11-object.c - the GObject PKCS#11 wrapper library

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

#include "gp11.h"
#include "gp11-private.h"

#include <string.h>

/**
 * SECTION:gp11-object
 * @title: GP11Object
 * @short_description: Represents a PKCS11 object such as a key or certificate.
 * 
 * A GP11Object holds a handle to a PKCS11 object such as a key or certificate. Token objects
 * are stored on the token persistently. Others are transient and are called session objects. 
 */

/**
 * GP11Object:
 * 
 * Represents a PKCS11 object handle such as a key or certifiacte.
 */

/*
 * MT safe -- Nothing in GP11ObjectData changes between 
 * init and finalize. All GP11ObjectPrivate access between init
 * and finalize is locked.
 */

enum {
	PROP_0,
	PROP_MODULE,
	PROP_SLOT,
	PROP_HANDLE,
	PROP_SESSION
};

typedef struct _GP11ObjectData {
	GP11Module *module;
	GP11Slot *slot;
	CK_OBJECT_HANDLE handle;
} GP11ObjectData;

typedef struct _GP11ObjectPrivate {
	GP11ObjectData data;
	GStaticMutex mutex;
	GP11Session *session;
} GP11ObjectPrivate;

#define GP11_OBJECT_GET_DATA(o) \
      (G_TYPE_INSTANCE_GET_PRIVATE((o), GP11_TYPE_OBJECT, GP11ObjectData))

G_DEFINE_TYPE (GP11Object, gp11_object, G_TYPE_OBJECT);

/* ----------------------------------------------------------------------------
 * INTERNAL
 */

static void
run_call_with_session (GP11Call *call, GP11Session *session)
{
	g_assert (GP11_IS_CALL (call));
	g_assert (GP11_IS_SESSION (session));
	
	/* Hold onto this session for the length of the call */
	g_object_set_data_full (G_OBJECT (call), "call-opened-session", 
	                        g_object_ref (session), g_object_unref);

	_gp11_call_async_object (call, session);
	_gp11_call_async_go (call);	
}

static void
opened_session (GObject *obj, GAsyncResult *result, gpointer user_data)
{
	GP11Session *session;
	GError *err = NULL;
	GP11Call *call;
	
	g_assert (GP11_IS_CALL (user_data));
	call = GP11_CALL (user_data);
	
	session = gp11_slot_open_session_finish (GP11_SLOT (obj), result, &err);
	
	/* Transtfer the error to the outer call and finish */
	if (!session) {
		_gp11_call_async_short (user_data, err->code);
		g_error_free (err);
		return;
	}

	run_call_with_session (GP11_CALL (user_data), session);
	g_object_unref (session);
}

static void
require_session_async (GP11Object *self, GP11Call *call, 
                       gulong flags, GCancellable *cancellable)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	GP11Session *session;
	
	g_assert (GP11_IS_OBJECT (self));
	
	session = gp11_object_get_session (self);
	if (session) {
		run_call_with_session (call, session);
		g_object_unref (session);
	} else {
		gp11_slot_open_session_async (data->slot, flags, NULL, NULL,
		                              cancellable, opened_session, call);
	}
	
}

static GP11Session*
require_session_sync (GP11Object *self, gulong flags, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	GP11Session *session;
	
	g_assert (GP11_IS_OBJECT (self));

	session = gp11_object_get_session (self);
	if (session)
		return session;
	
	return gp11_slot_open_session (data->slot, flags, err);
}

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static void
gp11_object_init (GP11Object *self)
{
	GP11ObjectPrivate *pv = (G_TYPE_INSTANCE_GET_PRIVATE(self, GP11_TYPE_OBJECT, GP11ObjectPrivate));
	g_static_mutex_init (&pv->mutex);
}

static void
gp11_object_get_property (GObject *obj, guint prop_id, GValue *value, 
                          GParamSpec *pspec)
{
	GP11Object *self = GP11_OBJECT (obj);

	switch (prop_id) {
	case PROP_MODULE:
		g_value_take_object (value, gp11_object_get_module (self));
		break;
	case PROP_SLOT:
		g_value_take_object (value, gp11_object_get_slot (self));
		break;
	case PROP_SESSION:
		g_value_take_object (value, gp11_object_get_session (self));
		break;
	case PROP_HANDLE:
		g_value_set_ulong (value, gp11_object_get_handle (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gp11_object_set_property (GObject *obj, guint prop_id, const GValue *value, 
                          GParamSpec *pspec)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (obj);
	GP11Object *self = GP11_OBJECT (obj);
	
	/* The sets to data below are only allowed during construction */ 
	
	switch (prop_id) {
	case PROP_MODULE:
		g_return_if_fail (!data->module);
		data->module = g_value_get_object (value);
		g_return_if_fail (data->module);
		g_object_ref (data->module);
		break;
	case PROP_SLOT:
		g_return_if_fail (!data->slot);
		data->slot = g_value_get_object (value);
		g_return_if_fail (data->slot);
		g_object_ref (data->slot);
		break;
	case PROP_SESSION:
		gp11_object_set_session (self, g_value_get_object (value));
		break;
	case PROP_HANDLE:
		g_return_if_fail (!data->handle);
		data->handle = g_value_get_ulong (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gp11_object_finalize (GObject *obj)
{
	GP11ObjectPrivate *pv = (G_TYPE_INSTANCE_GET_PRIVATE(obj, GP11_TYPE_OBJECT, GP11ObjectPrivate));
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (obj);

	if (data->slot)
		g_object_unref (data->slot);
	data->slot = NULL;
	
	if (data->module)
		g_object_unref (data->module);
	data->module = NULL;
	
	if (pv->session)
		g_object_unref (pv->session);
	pv->session = NULL;
	
	data->handle = 0;
	
	g_static_mutex_free (&pv->mutex);
	
	G_OBJECT_CLASS (gp11_object_parent_class)->finalize (obj);
}


static void
gp11_object_class_init (GP11ObjectClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gp11_object_parent_class = g_type_class_peek_parent (klass);
	
	gobject_class->get_property = gp11_object_get_property;
	gobject_class->set_property = gp11_object_set_property;
	gobject_class->finalize = gp11_object_finalize;
	
	/**
	 * GP11Object:module:
	 * 
	 * The GP11Module that this object belongs to.
	 */
	g_object_class_install_property (gobject_class, PROP_MODULE,
		g_param_spec_object ("module", "Module", "PKCS11 Module",
		                     GP11_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GP11Object:slot:
	 * 
	 * The GP11Slot that this object belongs to. 
	 * 
	 * If this is a token object then it will be stored on the token in this slot.
	 * If this is a session object, then it belongs to a session opened on this slot. 
	 */
	g_object_class_install_property (gobject_class, PROP_SLOT,
		g_param_spec_object ("slot", "slot", "PKCS11 Slot",
		                     GP11_TYPE_SLOT, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GP11Object:handle:
	 * 
	 * The raw PKCS11 handle for this object.
	 */
	g_object_class_install_property (gobject_class, PROP_HANDLE,
		g_param_spec_ulong ("handle", "Object Handle", "PKCS11 Object Handle",
		                   0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GP11Object:session:
	 * 
	 * The PKCS11 session to make calls on when this object needs to 
	 * perform operations on itself. 
	 * 
	 * If this is NULL then a new session is opened for each operation, 
	 * such as gp11_object_get(), gp11_object_set() or gp11_object_destroy().
	 */
	g_object_class_install_property (gobject_class, PROP_SESSION,
		g_param_spec_object ("session", "session", "PKCS11 Session to make calls on",
		                     GP11_TYPE_SESSION, G_PARAM_READWRITE));
	
	g_type_class_add_private (klass, sizeof (GP11ObjectPrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC 
 */

/**
 * gp11_object_from_handle:
 * @slot: The slot on which this object is present.
 * @handle: The raw handle of the object. 
 * 
 * Initialize a GP11Object from a raw PKCS#11 handle. Normally you would use 
 * gp11_session_create_object() or gp11_session_find_objects() to access objects. 
 * 
 * Return value: The new GP11Object. You should use g_object_unref() when done with this object.
 **/
GP11Object*
gp11_object_from_handle (GP11Slot *slot, CK_OBJECT_HANDLE handle)
{
	GP11Module *module = NULL;
	GP11Object *object;
	
	g_return_val_if_fail (GP11_IS_SLOT (slot), NULL);
	
	module = gp11_slot_get_module (slot);
	object = g_object_new (GP11_TYPE_OBJECT, "module", module, "handle", handle, "slot", slot, NULL);
	g_object_unref (module);
	
	return object;
}

/**
 * gp11_objects_from_handle_array:
 * @slot: The slot on which these objects are present.
 * @handles: The raw object handles.
 * @n_handles: The number of raw object handles.
 * 
 * Initialize a list of GP11Object from raw PKCS#11 handles. The handles argument must contain 
 * contiguous CK_OBJECT_HANDLE handles in an array.
 * 
 * Return value: The list of GP11Object. You should use gp11_list_unref_free() when done with 
 * this list. 
 **/
GList*
gp11_objects_from_handle_array (GP11Slot *slot, CK_OBJECT_HANDLE_PTR handles, CK_ULONG n_handles)
{
	GList *results = NULL;
	CK_ULONG i;
	
	g_return_val_if_fail (GP11_IS_SLOT (slot), NULL);
	g_return_val_if_fail (handles || !n_handles, NULL);
	
	for (i = 0; i < n_handles; ++i)
		results = g_list_prepend (results, gp11_object_from_handle (slot, handles[i]));
	return g_list_reverse (results);
}

/**
 * gp11_object_equal:
 * @object1: A pointer to the first GP11Object
 * @object2: A pointer to the second GP11Object
 * 
 * Checks equality of two objects. Two GP11Object objects can point to the same 
 * underlying PKCS#11 object.
 * 
 * Return value: TRUE if object1 and object2 are equal. FALSE if either is not a GP11Object.
 **/
gboolean
gp11_object_equal (gconstpointer object1, gconstpointer object2)
{
	GP11ObjectData *data1, *data2;

	if (object1 == object2)
		return TRUE;
	if (!GP11_IS_OBJECT (object1) || !GP11_IS_OBJECT (object2))
		return FALSE;
	
	data1 = GP11_OBJECT_GET_DATA (object1);
	data2 = GP11_OBJECT_GET_DATA (object2);
	
	return data1->handle == data2->handle && 
	       gp11_slot_equal (data1->slot, data2->slot);
}

/**
 * gp11_object_hash:
 * @object: A pointer to a GP11Object
 * 
 * Create a hash value for the GP11Object. 
 * 
 * This function is intended for easily hashing a GP11Object to add to 
 * a GHashTable or similar data structure.
 * 
 * Return value: An integer that can be used as a hash value, or 0 if invalid.
 **/
guint
gp11_object_hash (gconstpointer object)
{
	GP11ObjectData *data;
	
	g_return_val_if_fail (GP11_IS_OBJECT (object), 0);

	data = GP11_OBJECT_GET_DATA (object);
	
	return _gp11_ulong_hash (&data->handle) ^
	       gp11_slot_hash (data->slot);
}


/**
 * gp11_object_get_handle:
 * @self: The object.
 * 
 * Get the raw PKCS#11 handle of a GP11Object.
 * 
 * Return value: The raw object handle.
 **/
CK_OBJECT_HANDLE
gp11_object_get_handle (GP11Object *self)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_OBJECT (self), (CK_OBJECT_HANDLE)-1);
	return data->handle;
}

/**
 * gp11_object_get_module:
 * @self: The object.
 * 
 * Get the PKCS#11 module to which this object belongs.
 * 
 * Return value: The module, which should be unreffed after use.
 **/
GP11Module*
gp11_object_get_module (GP11Object *self)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GP11_IS_MODULE (data->module), NULL);
	return g_object_ref (data->module);
}

/**
 * gp11_object_get_slot:
 * @self: The object.
 * 
 * Get the PKCS#11 slot to which this object belongs.
 * 
 * Return value: The slot, which should be unreffed after use.
 **/
GP11Slot*
gp11_object_get_slot (GP11Object *self)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GP11_IS_SLOT (data->slot), NULL);
	return g_object_ref (data->slot);
}

/**
 * gp11_object_get_session:
 * @self: The object
 * 
 * Get the PKCS#11 session assigned to make calls on when operating
 * on this object.  
 * 
 * This will only return a session if it was set explitly on this 
 * object. By default an object will open and close sessions 
 * appropriate for its calls.
 * 
 * Return value: The assigned session, which must be unreffed after use.
 **/
GP11Session*
gp11_object_get_session (GP11Object *self)
{
	GP11ObjectPrivate *pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GP11_TYPE_OBJECT, GP11ObjectPrivate));
	GP11Session *session;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	
	g_static_mutex_lock (&pv->mutex);
	
	{
		session = pv->session ? g_object_ref (pv->session) : NULL;
	}
	
	g_static_mutex_unlock (&pv->mutex);
	
	return session;
}

/**
 * gp11_object_set_session:
 * @self: The object
 * @session: The assigned session
 * 
 * Set the PKCS#11 session assigned to make calls on when operating
 * on this object.  
 * 
 * It isn't always necessary to assign a session to an object. 
 * By default an object will open and close sessions appropriate for 
 * its calls.
 * 
 * If you assign a read-only session, then calls on this object
 * that modify the state of the object will probably fail.
 **/
void
gp11_object_set_session (GP11Object *self, GP11Session *session)
{
	GP11ObjectPrivate *pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GP11_TYPE_OBJECT, GP11ObjectPrivate));

	g_return_if_fail (GP11_IS_OBJECT (self));
	
	g_static_mutex_lock (&pv->mutex);
	
	{
		if (session)
			g_object_ref (session);
		if (pv->session)
			g_object_unref (pv->session);
		pv->session = session;
	}
	
	g_static_mutex_unlock (&pv->mutex);
}

/* --------------------------------------------------------------------------------------
 * DESTROY
 */

typedef struct _Destroy {
	GP11Arguments base;
	CK_OBJECT_HANDLE object;
} Destroy;

static CK_RV
perform_destroy (Destroy *args)
{
	g_assert (args);
	return (args->base.pkcs11->C_DestroyObject) (args->base.handle, args->object);
}

/**
 * gp11_object_destroy:
 * @self: The object to destroy.
 * @err: A location to return an error.
 * 
 * Destroy a PKCS#11 object, deleting it from storage or the session.
 * This call may block for an indefinite period.
 * 
 * Return value: Whether the call was successful or not.
 **/
gboolean
gp11_object_destroy (GP11Object *self, GError **err)
{
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);
	return gp11_object_destroy_full (self, NULL, err);
}

/**
 * gp11_object_destroy_full:
 * @self: The object to destroy.
 * @cancellable: Optional cancellable object, or NULL to ignore. 
 * @err: A location to return an error.
 * 
 * Destroy a PKCS#11 object, deleting it from storage or the session.
 * This call may block for an indefinite period.
 * 
 * Return value: Whether the call was successful or not.
 **/
gboolean
gp11_object_destroy_full (GP11Object *self, GCancellable *cancellable, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	Destroy args = { GP11_ARGUMENTS_INIT, 0 };
	GP11Session *session;
	gboolean ret = FALSE;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GP11_IS_SLOT (data->slot), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);
	
	args.object = data->handle;

	session = require_session_sync (self, CKF_RW_SESSION, err);
	if (session)
		ret = _gp11_call_sync (session, perform_destroy, NULL, &args, cancellable, err);
	g_object_unref (session);
	return ret;
}

/**
 * gp11_object_destroy_async:
 * @self: The object to destroy.
 * @cancellable: Optional cancellable object, or NULL to ignore. 
 * @callback: Callback which is called when operation completes.
 * @user_data: Data to pass to the callback.
 * 
 * Destroy a PKCS#11 object, deleting it from storage or the session.
 * This call will return immediately and complete asynchronously.
 **/
void
gp11_object_destroy_async (GP11Object *self, GCancellable *cancellable,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	Destroy* args;
	GP11Call *call;

	g_return_if_fail (GP11_IS_OBJECT (self));
	g_return_if_fail (GP11_IS_SLOT (data->slot));

	args = _gp11_call_async_prep (data->slot, self, perform_destroy, NULL, sizeof (*args), NULL);
	args->object = data->handle;
	
	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	require_session_async (self, call, CKF_RW_SESSION, cancellable);
}

/**
 * gp11_object_destroy_finish:
 * @self: The object being destroyed.
 * @result: The result of the destory operation passed to the callback.
 * @err: A location to store an error.
 * 
 * Get the status of the operation to destroy a PKCS#11 object, begun with 
 * gp11_object_destroy_async(). 
 * 
 * Return value: Whether the object was destroyed successfully or not.
 */
gboolean
gp11_object_destroy_finish (GP11Object *self, GAsyncResult *result, GError **err)
{
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GP11_IS_CALL (result), FALSE);
	return _gp11_call_basic_finish (result, err);
}

/* --------------------------------------------------------------------------------------
 * SET ATTRIBUTES
 */

typedef struct _SetAttributes {
	GP11Arguments base;
	GP11Attributes *attrs;
	CK_OBJECT_HANDLE object;
} SetAttributes;

static CK_RV
perform_set_attributes (SetAttributes *args)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;
	
	g_assert (args);
	attrs = _gp11_attributes_commit_out (args->attrs, &n_attrs);
	
	return (args->base.pkcs11->C_SetAttributeValue) (args->base.handle, args->object, 
	                                                 attrs, n_attrs);
}

static void
free_set_attributes (SetAttributes *args)
{
	g_assert (args);
	gp11_attributes_unref (args->attrs);
	g_free (args);
}

/**
 * gp11_object_set:
 * @self: The object to set attributes on.
 * @err: A location to return an error.
 * @...: The attributes to set.
 *
 * Set PKCS#11 attributes on an object.
 * This call may block for an indefinite period.
 * 
 * The arguments must be triples of: attribute type, data type, value
 * 
 * <para>The variable argument list should contain:
 * 	<variablelist>
 *		<varlistentry>
 * 			<term>a)</term>
 * 			<listitem><para>The gulong attribute type (ie: CKA_LABEL). </para></listitem>
 * 		</varlistentry>
 * 		<varlistentry>
 * 			<term>b)</term>
 * 			<listitem><para>The attribute data type (one of GP11_BOOLEAN, GP11_ULONG, 
 * 				GP11_STRING, GP11_DATE) orthe raw attribute value length.</para></listitem>
 * 		</varlistentry>
 * 		<varlistentry>
 * 			<term>c)</term>
 * 			<listitem><para>The attribute value, either a gboolean, gulong, gchar*, GDate* or 
 * 				a pointer to a raw attribute value.</para></listitem>
 * 		</varlistentry>
 * 	</variablelist>
 * The variable argument list should be terminated with GP11_INVALID.</para> 
 * 
 * Return value: Whether the call was successful or not.
 **/
gboolean
gp11_object_set (GP11Object *self, GError **err, ...)
{
	GP11Attributes *attrs;
	va_list va;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);
	
	va_start (va, err);
	attrs = gp11_attributes_new_valist (g_realloc, va);
	va_end (va);
	
	rv = gp11_object_set_full (self, attrs, NULL, err);
	
	gp11_attributes_unref (attrs);
	return rv;
}

/**
 * gp11_object_set_full:
 * @self: The object to set attributes on.
 * @attrs: The attributes to set on the object.
 * @cancellable: Optional cancellable object, or NULL to ignore. 
 * @err: A location to return an error.
 * 
 * Set PKCS#11 attributes on an object. This call may block for an indefinite period.
 * 
 * Return value: Whether the call was successful or not.
 **/
gboolean
gp11_object_set_full (GP11Object *self, GP11Attributes *attrs,
                      GCancellable *cancellable, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	SetAttributes args;
	GP11Session *session;
	gboolean ret = FALSE;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (attrs, FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);
	
	_gp11_attributes_lock (attrs);
	
	memset (&args, 0, sizeof (args));
	args.attrs = attrs;
	args.object = data->handle;

	session = require_session_sync (self, CKF_RW_SESSION, err);
	if (session)
		ret = _gp11_call_sync (session, perform_set_attributes, NULL, &args, cancellable, err);
	
	_gp11_attributes_unlock (attrs);
	g_object_unref (session);
	return ret;
}

/**
 * gp11_object_set_async:
 * @self: The object to set attributes on.
 * @attrs: The attributes to set on the object.
 * @cancellable: Optional cancellable object, or NULL to ignore. 
 * @callback: Callback which is called when operation completes.
 * @user_data: Data to pass to the callback.
 * 
 * Set PKCS#11 attributes on an object. This call will return 
 * immediately and completes asynchronously.
 **/
void
gp11_object_set_async (GP11Object *self, GP11Attributes *attrs, GCancellable *cancellable,
                       GAsyncReadyCallback callback, gpointer user_data)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	SetAttributes *args;
	GP11Call *call;
	
	g_return_if_fail (GP11_IS_OBJECT (self));
	g_return_if_fail (attrs);

	args = _gp11_call_async_prep (data->slot, self, perform_set_attributes, 
	                              NULL, sizeof (*args), free_set_attributes);
	
	_gp11_attributes_lock (attrs);
	args->attrs = gp11_attributes_ref (attrs);
	args->object = data->handle;
	
	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	require_session_async (self, call, CKF_RW_SESSION, cancellable);
}

/**
 * gp11_object_set_finish:
 * @self: The object to set attributes on.
 * @result: The result of the destory operation passed to the callback.
 * @err: A location to store an error.
 * 
 * Get the status of the operation to set attributes on a PKCS#11 object, 
 * begun with gp11_object_set_async(). 
 * 
 * Return value: Whether the attributes were successfully set on the object or not.
 */
gboolean
gp11_object_set_finish (GP11Object *self, GAsyncResult *result, GError **err)
{
	SetAttributes *args;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GP11_IS_CALL (result), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);
	
	/* Unlock the attributes we were using */
	args = _gp11_call_arguments (result, SetAttributes);
	g_assert (args->attrs);
	_gp11_attributes_unlock (args->attrs);

	return _gp11_call_basic_finish (result, err);
}

/* ------------------------------------------------------------------------------------
 * GET ATTRIBUTES
 */

typedef struct _GetAttributes {
	GP11Arguments base;
	CK_OBJECT_HANDLE object;
	GP11Attributes *attrs;
} GetAttributes;

/* 
 * Certain failure return values only apply to individual attributes
 * being retrieved. These are ignored, since the attribute should 
 * already have -1 set as the length.
 */
static gboolean
is_ok_get_attributes_rv (CK_RV rv) 
{
	switch (rv) {
	case CKR_OK:
	case CKR_ATTRIBUTE_SENSITIVE:
	case CKR_ATTRIBUTE_TYPE_INVALID:
		return TRUE;
	default:
		return FALSE;
	}
}

static CK_RV
perform_get_attributes (GetAttributes *args)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;
	CK_RV rv;
	
	g_assert (args);
	g_assert (args->attrs);
	
	/* Prepare all the attributes */
	attrs = _gp11_attributes_prepare_in (args->attrs, &n_attrs);

	/* Get the size of each value */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object,
	                                               attrs, n_attrs);
	if (!is_ok_get_attributes_rv (rv))
		return rv;

	/* Allocate memory for each value */
	attrs = _gp11_attributes_commit_in (args->attrs, &n_attrs);
	
	/* Now get the actual values */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object,
	                                               attrs, n_attrs);
	
	if (is_ok_get_attributes_rv (rv))
		rv = CKR_OK;
	
	return rv;
}

static void
free_get_attributes (GetAttributes *args)
{
	g_assert (args);
	g_assert (args->attrs);
	gp11_attributes_unref (args->attrs);
	g_free (args);
}


/**
 * gp11_object_get:
 * @self: The object to get attributes from.
 * @err: A location to store an error.
 * @...: The attribute types to get.
 *
 * Get the specified attributes from the object. This call may
 * block for an indefinite period.
 * 
 * Return value: The resulting PKCS#11 attributes, or NULL if an error occurred. 
 * The result must be unreffed when you're finished with it.
 **/
GP11Attributes*
gp11_object_get (GP11Object *self, GError **err, ...)
{
	GP11Attributes *attrs;
	va_list va;
	gulong type;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	attrs = gp11_attributes_new ();
	va_start (va, err);
	for (;;) {
		type = va_arg (va, gulong);
		if (type == GP11_INVALID)
			break;
		gp11_attributes_add_invalid (attrs, type);
	}
	va_end (va);
	
	if (!gp11_object_get_full (self, attrs, NULL, err)) {
		gp11_attributes_unref (attrs);
		return NULL;
	}

	return attrs;
}

/**
 * gp11_object_get_full:
 * @self: The object to get attributes from.
 * @attrs: The attributes to get, with the types filled in.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to store an error.
 * 
 * Get the specified attributes from the object. This call may
 * block for an indefinite period.
 * 
 * No extra references are added to the returned attributes pointer. 
 * During this call you may not access the attributes in any way. 
 * 
 * Return value: A pointer to the filled in attributes if successful, 
 * or NULL if not. 
 **/
GP11Attributes*
gp11_object_get_full (GP11Object *self, GP11Attributes *attrs,
                      GCancellable *cancellable, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	GetAttributes args;
	GP11Session *session;
	gboolean ret;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!err || !*err, NULL);
	
	session = require_session_sync (self, 0, err);
	if (!session)
		return NULL;
	
	_gp11_attributes_lock (attrs);
	
	memset (&args, 0, sizeof (args));
	args.attrs = attrs;
	args.object = data->handle;

	ret = _gp11_call_sync (session, perform_get_attributes, NULL, &args, cancellable, err);
	_gp11_attributes_unlock (attrs);
	g_object_unref (session);
	
	return ret ? attrs : NULL;
}

/**
 * gp11_object_get_async:
 * @self: The object to get attributes from.
 * @attrs: The attributes to get, initialized with their types.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: A callback which is called when the operation completes.
 * @user_data: Data to be passed to the callback.
 * 
 * Get the specified attributes from the object. The attributes will be cleared
 * of their current values, and new attributes will be stored. The attributes
 * should not be accessed in any way except for referencing and unreferencing 
 * them until gp11_object_get_finish() is called.
 * 
 * This call returns immediately and completes asynchronously.
 **/
void
gp11_object_get_async (GP11Object *self, GP11Attributes *attrs, GCancellable *cancellable, 
                       GAsyncReadyCallback callback, gpointer user_data)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	GetAttributes *args;
	GP11Call *call;
	
	g_return_if_fail (GP11_IS_OBJECT (self));
	g_return_if_fail (attrs);

	args = _gp11_call_async_prep (data->slot, self, perform_get_attributes, 
	                              NULL, sizeof (*args), free_get_attributes);
	
	_gp11_attributes_lock (attrs);
	args->attrs = gp11_attributes_ref (attrs);
	args->object = data->handle;
	
	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	require_session_async (self, call, 0, cancellable);
}

/**
 * gp11_object_get_finish:
 * @self: The object to get attributes from.
 * @result: The result passed to the callback.
 * @err: A location to store an error.
 * 
 * Get the result of a get operation and return specified attributes from 
 * the object. 
 * 
 * No extra references are added to the returned attributes pointer. 
 * 
 * Return value: The filled in attributes structure if successful or 
 * NULL if not successful.
 **/
GP11Attributes*
gp11_object_get_finish (GP11Object *self, GAsyncResult *result, GError **err)
{
	GetAttributes *args;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GP11_IS_CALL (result), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	args = _gp11_call_arguments (result, GetAttributes);
	_gp11_attributes_unlock (args->attrs);

	if (!_gp11_call_basic_finish (result, err))
		return NULL;
	
	return args->attrs;
}

/* ---------------------------------------------------------------------------------
 * GET ATTRIBUTE DATA
 */

typedef struct _GetAttributeData {
	GP11Arguments base;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_TYPE type;
	GP11Allocator allocator;
	guchar *result;
	gsize n_result;
} GetAttributeData;

static CK_RV
perform_get_attribute_data (GetAttributeData *args)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;
	
	g_assert (args);
	g_assert (args->allocator);
	
	attr.type = args->type;
	attr.ulValueLen = 0;
	attr.pValue = 0;
	
	/* Get the size of the value */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object,
	                                               &attr, 1);
	if (rv != CKR_OK) 
		return rv;
	
	/* Allocate memory for the value */
	args->result = (args->allocator) (NULL, attr.ulValueLen + 1);
	g_assert (args->result);
	attr.pValue = args->result;

	/* Now get the actual value */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object,
	                                               &attr, 1);

	if (rv == CKR_OK) {
		args->n_result = attr.ulValueLen;
		args->result[args->n_result] = 0;
	}

	return rv;
}

static void
free_get_attribute_data (GetAttributeData *args)
{
	g_assert (args);
	g_free (args->result);
	g_free (args);
}

/**
 * gp11_object_get_data:
 * @self: The object to get attribute data from.
 * @attr_type: The attribute to get data for.
 * @n_data: The length of the resulting data.
 * @err: A location to store an error.
 * 
 * Get the data for the specified attribute from the object. For convenience
 * the returned data has a null terminator.
 *
 * This call may block for an indefinite period.
 * 
 * Return value: The resulting PKCS#11 attribute data, or NULL if an error occurred. 
 **/
gpointer
gp11_object_get_data (GP11Object *self, gulong attr_type, gsize *n_data, GError **err)
{
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (!err || !*err, NULL);
	
	return gp11_object_get_data_full (self, attr_type, g_realloc, NULL, n_data, err);
}

/**
 * gp11_object_get_data_full:
 * @self: The object to get attribute data from.
 * @attr_type: The attribute to get data for.
 * @allocator: An allocator with which to allocate memory for the data, or NULL for default.
 * @cancellable: Optional cancellation object, or NULL.
 * @n_data: The length of the resulting data.
 * @err: A location to store an error.
 * 
 * Get the data for the specified attribute from the object. For convenience
 * the returned data has an extra null terminator, not included in the returned length.
 *
 * This call may block for an indefinite period.
 * 
 * Return value: The resulting PKCS#11 attribute data, or NULL if an error occurred. 
 **/
gpointer
gp11_object_get_data_full (GP11Object *self, gulong attr_type, GP11Allocator allocator,
                           GCancellable *cancellable, gsize *n_data, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	GetAttributeData args;
	GP11Session *session;
	gboolean ret;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (!err || !*err, NULL);
	
	if (!allocator)
		allocator = g_realloc;
	
	session = require_session_sync (self, 0, err);
	if (!session)
		return NULL;
	
	memset (&args, 0, sizeof (args));
	args.allocator = allocator;
	args.object = data->handle;
	args.type = attr_type;

	ret = _gp11_call_sync (session, perform_get_attribute_data, NULL, &args, cancellable, err);
	g_object_unref (session);
	
	/* Free any value if failed */
	if (!ret) {
		if (args.result)
			(allocator) (args.result, 0);
		return NULL;
	}
	
	*n_data = args.n_result;
	return args.result;
}

/**
 * gp11_object_get_data_async:
 * @self: The object to get attribute data from.
 * @attr_type: The attribute to get data for.
 * @allocator: An allocator with which to allocate memory for the data, or NULL for default.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to be passed to the callback.
 * 
 * Get the data for the specified attribute from the object.
 *
 * This call will return immediately and complete asynchronously.
 **/
void
gp11_object_get_data_async (GP11Object *self, gulong attr_type, GP11Allocator allocator, 
                            GCancellable *cancellable, GAsyncReadyCallback callback, 
                            gpointer user_data)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	GetAttributeData *args;
	GP11Call *call;
	
	g_return_if_fail (GP11_IS_OBJECT (self));
	
	if (!allocator)
		allocator = g_realloc;

	args = _gp11_call_async_prep (data->slot, self, perform_get_attribute_data, 
	                              NULL, sizeof (*args), free_get_attribute_data);

	args->allocator = allocator;
	args->object = data->handle;
	args->type = attr_type;
	
	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	require_session_async (self, call, 0, cancellable);
}

/**
 * gp11_object_get_data_finish:
 * @self: The object to get an attribute from.
 * @result: The result passed to the callback.
 * @n_data: The length of the resulting data.
 * @err: A location to store an error.
 *
 * Get the result of an operation to get attribute data from
 * an object. For convenience the returned data has an extra null terminator,
 * not included in the returned length.
 *
 * 
 * Return value: The PKCS#11 attribute data or NULL if an error occurred.
 **/
gpointer
gp11_object_get_data_finish (GP11Object *self, GAsyncResult *result, 
                             gsize *n_data, GError **err)
{
	GetAttributeData *args;
	guchar *data;
	
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GP11_IS_CALL (result), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (!err || !*err, NULL);
	
	if (!_gp11_call_basic_finish (result, err))
		return NULL;

	args = _gp11_call_arguments (result, GetAttributeData);

	*n_data = args->n_result;
	data = args->result;
	args->result = NULL;
	
	return data;
}

/* ---------------------------------------------------------------------------------------
 * SET TEMPLATE
 */

typedef struct _set_template_args {
	GP11Arguments base;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_TYPE type;
	GP11Attributes *attrs;
} set_template_args;

static CK_RV
perform_set_template (set_template_args *args)
{
	CK_ATTRIBUTE attr;
	CK_ULONG n_attrs;

	g_assert (args);

	attr.type = args->type;
	attr.pValue = _gp11_attributes_commit_out (args->attrs, &n_attrs);
	attr.ulValueLen = n_attrs * sizeof (CK_ATTRIBUTE);

	return (args->base.pkcs11->C_SetAttributeValue) (args->base.handle, args->object, &attr, 1);
}

static void
free_set_template (set_template_args *args)
{
	g_assert (args);
	gp11_attributes_unref (args->attrs);
	g_free (args);
}

/**
 * gp11_object_set_template:
 * @self: The object to set an attribute template on.
 * @attr_type: The attribute template type.
 * @attrs: The attribute template.
 * @err: A location to store an error.
 *
 * Set an attribute template on the object. The attr_type must be for
 * an attribute which contains a template.
 *
 * This call may block for an indefinite period.
 *
 * Return value: TRUE if the operation succeeded.
 **/
gboolean
gp11_object_set_template (GP11Object *self, gulong attr_type, GP11Attributes *attrs,
                          GError **err)
{
	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);
	return gp11_object_set_template_full (self, attr_type, attrs, NULL, err);
}

/**
 * gp11_object_set_template_full:
 * @self: The object to set an attribute template on.
 * @attr_type: The attribute template type.
 * @attrs: The attribute template.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to store an error.
 *
 * Set an attribute template on the object. The attr_type must be for
 * an attribute which contains a template.
 *
 * This call may block for an indefinite period.
 *
 * Return value: TRUE if the operation succeeded.
 **/
gboolean
gp11_object_set_template_full (GP11Object *self, gulong attr_type, GP11Attributes *attrs,
                               GCancellable *cancellable, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	set_template_args args;
	GP11Session *session;
	gboolean ret = FALSE;

	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (attrs, FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	_gp11_attributes_lock (attrs);

	memset (&args, 0, sizeof (args));
	args.attrs = attrs;
	args.type = attr_type;
	args.object = data->handle;

	session = require_session_sync (self, CKF_RW_SESSION, err);
	if (session)
		ret = _gp11_call_sync (session, perform_set_template, NULL, &args, cancellable, err);

	_gp11_attributes_unlock (attrs);
	g_object_unref (session);
	return ret;
}

/**
 * gp11_object_set_template_async:
 * @self: The object to set an attribute template on.
 * @attr_type: The attribute template type.
 * @attrs: The attribute template.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to be passed to the callback.
 *
 * Set an attribute template on the object. The attr_type must be for
 * an attribute which contains a template.
 *
 * This call will return immediately and complete asynchronously.
 **/
void
gp11_object_set_template_async (GP11Object *self, gulong attr_type, GP11Attributes *attrs,
                                GCancellable *cancellable, GAsyncReadyCallback callback,
                                gpointer user_data)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	set_template_args *args;
	GP11Call *call;

	g_return_if_fail (GP11_IS_OBJECT (self));
	g_return_if_fail (attrs);

	args = _gp11_call_async_prep (data->slot, self, perform_set_template,
	                              NULL, sizeof (*args), free_set_template);

	_gp11_attributes_lock (attrs);
	args->attrs = gp11_attributes_ref (attrs);
	args->type = attr_type;
	args->object = data->handle;

	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	require_session_async (self, call, CKF_RW_SESSION, cancellable);
}

/**
 * gp11_object_set_template_finish:
 * @self: The object to set an attribute template on.
 * @result: The result passed to the callback.
 * @err: A location to store an error.
 *
 * Get the result of an operation to set attribute template on
 * an object.
 *
 * Return value: TRUE if the operation succeeded.
 **/
gboolean
gp11_object_set_template_finish (GP11Object *self, GAsyncResult *result, GError **err)
{
	set_template_args *args;

	g_return_val_if_fail (GP11_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GP11_IS_CALL (result), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	/* Unlock the attributes we were using */
	args = _gp11_call_arguments (result, set_template_args);
	g_assert (args->attrs);
	_gp11_attributes_unlock (args->attrs);

	return _gp11_call_basic_finish (result, err);
}

/* ---------------------------------------------------------------------------------------
 * GET TEMPLATE
 */

typedef struct _get_template_args {
	GP11Arguments base;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_TYPE type;
	GP11Attributes *attrs;
} get_template_args;

static CK_RV
perform_get_template (get_template_args *args)
{
	CK_ATTRIBUTE attr;
	CK_ULONG n_attrs, i;
	CK_RV rv;

	g_assert (args);
	g_assert (!args->attrs);

	args->attrs = gp11_attributes_new ();
	attr.type = args->type;
	attr.ulValueLen = 0;
	attr.pValue = 0;

	/* Get the length of the entire template */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object, &attr, 1);
	if (rv != CKR_OK)
		return rv;

	/* Number of attributes, rounded down */
	n_attrs = (attr.ulValueLen / sizeof (CK_ATTRIBUTE));
	for (i = 0; i < n_attrs; ++i)
		gp11_attributes_add_empty (args->attrs, 0);

	/* Prepare all the attributes */
	_gp11_attributes_lock (args->attrs);
	attr.pValue = _gp11_attributes_prepare_in (args->attrs, &n_attrs);

	/* Get the size of each value */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object, &attr, 1);
	if (rv != CKR_OK)
		return rv;

	/* Allocate memory for each value */
	attr.pValue = _gp11_attributes_commit_in (args->attrs, &n_attrs);

	/* Now get the actual values */
	return (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object, &attr, 1);
}

static void
free_get_template (get_template_args *args)
{
	g_assert (args);
	gp11_attributes_unref (args->attrs);
	g_free (args);
}

/**
 * gp11_object_get_template:
 * @self: The object to get an attribute template from.
 * @attr_type: The attribute template type.
 * @err: A location to store an error.
 *
 * Get an attribute template from the object. The attr_type must be for
 * an attribute which returns a template.
 *
 * This call may block for an indefinite period.
 *
 * Return value: The resulting PKCS#11 attribute template, or NULL if an error occurred.
 **/
GP11Attributes*
gp11_object_get_template (GP11Object *self, gulong attr_type, GError **err)
{
	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	return gp11_object_get_template_full (self, attr_type, NULL, err);
}

/**
 * gp11_object_get_template_full:
 * @self: The object to get an attribute template from.
 * @attr_type: The template attribute type.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to store an error.
 *
 * Get an attribute template from the object. The attr_type must be for
 * an attribute which returns a template.
 *
 * This call may block for an indefinite period.
 *
 * Return value: The resulting PKCS#11 attribute template, or NULL if an error occurred.
 **/
GP11Attributes*
gp11_object_get_template_full (GP11Object *self, gulong attr_type,
                               GCancellable *cancellable, GError **err)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	get_template_args args;
	GP11Session *session;
	gboolean ret;

	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	session = require_session_sync (self, 0, err);
	if (!session)
		return NULL;

	memset (&args, 0, sizeof (args));
	args.object = data->handle;
	args.type = attr_type;

	ret = _gp11_call_sync (session, perform_get_template, NULL, &args, cancellable, err);
	g_object_unref (session);

	_gp11_attributes_unlock (args.attrs);

	/* Free any value if failed */
	if (!ret) {
		gp11_attributes_unref (args.attrs);
		args.attrs = NULL;
	}

	return args.attrs;
}

/**
 * gp11_object_get_template_async:
 * @self: The object to get an attribute template from.
 * @attr_type: The template attribute type.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to be passed to the callback.
 *
 * Get an attribute template from the object. The attr_type must be for
 * an attribute which returns a template.
 *
 * This call will return immediately and complete asynchronously.
 **/
void
gp11_object_get_template_async (GP11Object *self, gulong attr_type,
                                GCancellable *cancellable, GAsyncReadyCallback callback,
                                gpointer user_data)
{
	GP11ObjectData *data = GP11_OBJECT_GET_DATA (self);
	get_template_args *args;
	GP11Call *call;

	g_return_if_fail (GP11_IS_OBJECT (self));

	args = _gp11_call_async_prep (data->slot, self, perform_get_template,
	                              NULL, sizeof (*args), free_get_template);

	args->object = data->handle;
	args->type = attr_type;

	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	require_session_async (self, call, 0, cancellable);
}

/**
 * gp11_object_get_template_finish:
 * @self: The object to get an attribute from.
 * @result: The result passed to the callback.
 * @err: A location to store an error.
 *
 * Get the result of an operation to get attribute template from
 * an object.
 *
 * Return value: The resulting PKCS#11 attribute template, or NULL if an error occurred.
 **/
GP11Attributes*
gp11_object_get_template_finish (GP11Object *self, GAsyncResult *result,
                                 GError **err)
{
	get_template_args *args;

	g_return_val_if_fail (GP11_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GP11_IS_CALL (result), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	if (!_gp11_call_basic_finish (result, err))
		return NULL;

	args = _gp11_call_arguments (result, get_template_args);
	_gp11_attributes_unlock (args->attrs);
	return gp11_attributes_ref (args->attrs);
}
