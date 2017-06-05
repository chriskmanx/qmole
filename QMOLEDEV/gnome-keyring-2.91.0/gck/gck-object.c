/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-object.c - the GObject PKCS#11 wrapper library

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
#include "gck-private.h"

#include <string.h>

/**
 * SECTION:gck-object
 * @title: GckObject
 * @short_description: Represents a PKCS11 object such as a key or certificate.
 *
 * A GckObject holds a handle to a PKCS11 object such as a key or certificate. Token objects
 * are stored on the token persistently. Others are transient and are called session objects.
 */

/**
 * GckObject:
 *
 * Represents a PKCS11 object handle such as a key or certifiacte.
 */

/*
 * MT safe -- Nothing in GckObjectData changes between
 * init and finalize. All GckObjectPrivate access between init
 * and finalize is locked.
 */

enum {
	PROP_0,
	PROP_MODULE,
	PROP_SESSION,
	PROP_HANDLE
};

struct _GckObjectPrivate {
	GckModule *module;
	GckSession *session;
	CK_OBJECT_HANDLE handle;
};

G_DEFINE_TYPE (GckObject, gck_object, G_TYPE_OBJECT);

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static void
gck_object_init (GckObject *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCK_TYPE_OBJECT, GckObjectPrivate);
}

static void
gck_object_get_property (GObject *obj, guint prop_id, GValue *value,
                          GParamSpec *pspec)
{
	GckObject *self = GCK_OBJECT (obj);

	switch (prop_id) {
	case PROP_MODULE:
		g_value_take_object (value, gck_object_get_module (self));
		break;
	case PROP_SESSION:
		g_value_take_object (value, gck_object_get_session (self));
		break;
	case PROP_HANDLE:
		g_value_set_ulong (value, gck_object_get_handle (self));
		break;
	}
}

static void
gck_object_set_property (GObject *obj, guint prop_id, const GValue *value,
                         GParamSpec *pspec)
{
	GckObject *self = GCK_OBJECT (obj);

	/* The sets to data below are only allowed during construction */

	switch (prop_id) {
	case PROP_MODULE:
		g_return_if_fail (!self->pv->module);
		self->pv->module = g_value_get_object (value);
		g_return_if_fail (self->pv->module);
		g_object_ref (self->pv->module);
		break;
	case PROP_SESSION:
		g_return_if_fail (!self->pv->session);
		self->pv->session = g_value_get_object (value);
		g_return_if_fail (self->pv->session);
		g_object_ref (self->pv->session);
		break;
	case PROP_HANDLE:
		g_return_if_fail (!self->pv->handle);
		self->pv->handle = g_value_get_ulong (value);
		break;
	}
}

static void
gck_object_finalize (GObject *obj)
{
	GckObject *self = GCK_OBJECT (obj);

	if (self->pv->session)
		g_object_unref (self->pv->session);
	self->pv->session = NULL;

	if (self->pv->module)
		g_object_unref (self->pv->module);
	self->pv->module = NULL;

	self->pv->handle = 0;

	G_OBJECT_CLASS (gck_object_parent_class)->finalize (obj);
}


static void
gck_object_class_init (GckObjectClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gck_object_parent_class = g_type_class_peek_parent (klass);

	gobject_class->get_property = gck_object_get_property;
	gobject_class->set_property = gck_object_set_property;
	gobject_class->finalize = gck_object_finalize;

	/**
	 * GckObject:module:
	 *
	 * The GckModule that this object belongs to.
	 */
	g_object_class_install_property (gobject_class, PROP_MODULE,
		g_param_spec_object ("module", "Module", "PKCS11 Module",
		                     GCK_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckObject:handle:
	 *
	 * The raw PKCS11 handle for this object.
	 */
	g_object_class_install_property (gobject_class, PROP_HANDLE,
		g_param_spec_ulong ("handle", "Object Handle", "PKCS11 Object Handle",
		                   0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckObject:session:
	 *
	 * The PKCS11 session to make calls on when this object needs to
	 * perform operations on itself.
	 *
	 * If this is NULL then a new session is opened for each operation,
	 * such as gck_object_get(), gck_object_set() or gck_object_destroy().
	 */
	g_object_class_install_property (gobject_class, PROP_SESSION,
		g_param_spec_object ("session", "session", "PKCS11 Session to make calls on",
		                     GCK_TYPE_SESSION, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (klass, sizeof (GckObjectPrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gck_object_from_handle:
 * @session: The session through which this object is accessed or created.
 * @handle: The raw handle of the object.
 *
 * Initialize a GckObject from a raw PKCS#11 handle. Normally you would use
 * gck_session_create_object() or gck_session_find_objects() to access objects.
 *
 * Return value: The new GckObject. You should use g_object_unref() when done with this object.
 **/
GckObject*
gck_object_from_handle (GckSession *session, CK_OBJECT_HANDLE handle)
{
	GckModule *module = NULL;
	GckObject *object;

	g_return_val_if_fail (GCK_IS_SESSION (session), NULL);

	module = gck_session_get_module (session);
	object = g_object_new (GCK_TYPE_OBJECT, "module", module, "handle", handle, "session", session, NULL);
	g_object_unref (module);

	return object;
}

/**
 * gck_objects_from_handle_array:
 * @slot: The slot on which these objects are present.
 * @handles: The raw object handles.
 * @n_handles: The number of raw object handles.
 *
 * Initialize a list of GckObject from raw PKCS#11 handles. The handles argument must contain
 * contiguous CK_OBJECT_HANDLE handles in an array.
 *
 * Return value: The list of GckObject. You should use gck_list_unref_free() when done with
 * this list.
 **/
GList*
gck_objects_from_handle_array (GckSession *session, CK_OBJECT_HANDLE_PTR handles, CK_ULONG n_handles)
{
	GList *results = NULL;
	CK_ULONG i;

	g_return_val_if_fail (GCK_IS_SESSION (session), NULL);
	g_return_val_if_fail (handles || !n_handles, NULL);

	for (i = 0; i < n_handles; ++i)
		results = g_list_prepend (results, gck_object_from_handle (session, handles[i]));
	return g_list_reverse (results);
}

/**
 * gck_object_equal:
 * @object1: A pointer to the first GckObject
 * @object2: A pointer to the second GckObject
 *
 * Checks equality of two objects. Two GckObject objects can point to the same
 * underlying PKCS#11 object.
 *
 * Return value: TRUE if object1 and object2 are equal. FALSE if either is not a GckObject.
 **/
gboolean
gck_object_equal (gconstpointer object1, gconstpointer object2)
{
	GckObject *obj1, *obj2;
	GckSlot *slot1, *slot2;
	gboolean ret;

	if (object1 == object2)
		return TRUE;
	if (!GCK_IS_OBJECT (object1) || !GCK_IS_OBJECT (object2))
		return FALSE;

	obj1 = GCK_OBJECT (object1);
	obj2 = GCK_OBJECT (object2);

	slot1 = gck_session_get_slot (obj1->pv->session);
	slot2 = gck_session_get_slot (obj2->pv->session);

	ret = obj1->pv->handle == obj2->pv->handle &&
	      gck_slot_equal (slot1, slot2);

	g_object_unref (slot1);
	g_object_unref (slot2);

	return ret;
}

/**
 * gck_object_hash:
 * @object: A pointer to a GckObject
 *
 * Create a hash value for the GckObject.
 *
 * This function is intended for easily hashing a GckObject to add to
 * a GHashTable or similar data structure.
 *
 * Return value: An integer that can be used as a hash value, or 0 if invalid.
 **/
guint
gck_object_hash (gconstpointer object)
{
	GckObject *self;
	GckSlot *slot;
	guint hash;

	g_return_val_if_fail (GCK_IS_OBJECT (object), 0);

	self = GCK_OBJECT (object);
	slot = gck_session_get_slot (self->pv->session);

	hash = _gck_ulong_hash (&self->pv->handle) ^
	       gck_slot_hash (slot);

	g_object_unref (slot);

	return hash;
}


/**
 * gck_object_get_handle:
 * @self: The object.
 *
 * Get the raw PKCS#11 handle of a GckObject.
 *
 * Return value: The raw object handle.
 **/
CK_OBJECT_HANDLE
gck_object_get_handle (GckObject *self)
{
	g_return_val_if_fail (GCK_IS_OBJECT (self), (CK_OBJECT_HANDLE)-1);
	return self->pv->handle;
}

/**
 * gck_object_get_module:
 * @self: The object.
 *
 * Get the PKCS#11 module to which this object belongs.
 *
 * Return value: The module, which should be unreffed after use.
 **/
GckModule*
gck_object_get_module (GckObject *self)
{
	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GCK_IS_MODULE (self->pv->module), NULL);
	return g_object_ref (self->pv->module);
}


/**
 * gck_object_get_session:
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
GckSession*
gck_object_get_session (GckObject *self)
{
	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GCK_IS_SESSION (self->pv->session), NULL);
	return g_object_ref (self->pv->session);
}

/* --------------------------------------------------------------------------------------
 * DESTROY
 */

typedef struct _Destroy {
	GckArguments base;
	CK_OBJECT_HANDLE object;
} Destroy;

static CK_RV
perform_destroy (Destroy *args)
{
	g_assert (args);
	return (args->base.pkcs11->C_DestroyObject) (args->base.handle, args->object);
}

/**
 * gck_object_destroy:
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
gck_object_destroy (GckObject *self, GCancellable *cancellable, GError **err)
{
	Destroy args = { GCK_ARGUMENTS_INIT, 0 };

	g_return_val_if_fail (GCK_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GCK_IS_SESSION (self->pv->session), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	args.object = self->pv->handle;
	return _gck_call_sync (self->pv->session, perform_destroy, NULL, &args, cancellable, err);
}

/**
 * gck_object_destroy_async:
 * @self: The object to destroy.
 * @cancellable: Optional cancellable object, or NULL to ignore.
 * @callback: Callback which is called when operation completes.
 * @user_data: Data to pass to the callback.
 *
 * Destroy a PKCS#11 object, deleting it from storage or the session.
 * This call will return immediately and complete asynchronously.
 **/
void
gck_object_destroy_async (GckObject *self, GCancellable *cancellable,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	Destroy* args;

	g_return_if_fail (GCK_IS_OBJECT (self));
	g_return_if_fail (GCK_IS_SESSION (self->pv->session));

	args = _gck_call_async_prep (self->pv->session, self, perform_destroy, NULL, sizeof (*args), NULL);
	args->object = self->pv->handle;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_object_destroy_finish:
 * @self: The object being destroyed.
 * @result: The result of the destory operation passed to the callback.
 * @err: A location to store an error.
 *
 * Get the status of the operation to destroy a PKCS#11 object, begun with
 * gck_object_destroy_async().
 *
 * Return value: Whether the object was destroyed successfully or not.
 */
gboolean
gck_object_destroy_finish (GckObject *self, GAsyncResult *result, GError **err)
{
	g_return_val_if_fail (GCK_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GCK_IS_CALL (result), FALSE);
	return _gck_call_basic_finish (result, err);
}

/* --------------------------------------------------------------------------------------
 * SET ATTRIBUTES
 */

typedef struct _SetAttributes {
	GckArguments base;
	GckAttributes *attrs;
	CK_OBJECT_HANDLE object;
} SetAttributes;

static CK_RV
perform_set_attributes (SetAttributes *args)
{
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs;

	g_assert (args);
	attrs = _gck_attributes_commit_out (args->attrs, &n_attrs);

	return (args->base.pkcs11->C_SetAttributeValue) (args->base.handle, args->object,
	                                                 attrs, n_attrs);
}

static void
free_set_attributes (SetAttributes *args)
{
	g_assert (args);
	gck_attributes_unref (args->attrs);
	g_free (args);
}

/**
 * gck_object_set:
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
gck_object_set (GckObject *self, GckAttributes *attrs,
                GCancellable *cancellable, GError **err)
{
	SetAttributes args;
	gboolean ret = FALSE;

	g_return_val_if_fail (GCK_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (attrs, FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	_gck_attributes_lock (attrs);

	memset (&args, 0, sizeof (args));
	args.attrs = attrs;
	args.object = self->pv->handle;

	ret = _gck_call_sync (self->pv->session, perform_set_attributes, NULL, &args, cancellable, err);

	_gck_attributes_unlock (attrs);
	return ret;
}

/**
 * gck_object_set_async:
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
gck_object_set_async (GckObject *self, GckAttributes *attrs, GCancellable *cancellable,
                       GAsyncReadyCallback callback, gpointer user_data)
{
	SetAttributes *args;

	g_return_if_fail (GCK_IS_OBJECT (self));
	g_return_if_fail (attrs);

	args = _gck_call_async_prep (self->pv->session, self, perform_set_attributes,
	                             NULL, sizeof (*args), free_set_attributes);

	_gck_attributes_lock (attrs);
	args->attrs = gck_attributes_ref (attrs);
	args->object = self->pv->handle;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_object_set_finish:
 * @self: The object to set attributes on.
 * @result: The result of the destory operation passed to the callback.
 * @err: A location to store an error.
 *
 * Get the status of the operation to set attributes on a PKCS#11 object,
 * begun with gck_object_set_async().
 *
 * Return value: Whether the attributes were successfully set on the object or not.
 */
gboolean
gck_object_set_finish (GckObject *self, GAsyncResult *result, GError **err)
{
	SetAttributes *args;

	g_return_val_if_fail (GCK_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GCK_IS_CALL (result), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	/* Unlock the attributes we were using */
	args = _gck_call_arguments (result, SetAttributes);
	g_assert (args->attrs);
	_gck_attributes_unlock (args->attrs);

	return _gck_call_basic_finish (result, err);
}

/* ------------------------------------------------------------------------------------
 * GET ATTRIBUTES
 */

typedef struct _GetAttributes {
	GckArguments base;
	CK_OBJECT_HANDLE object;
	GckAttributes *attrs;
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
	attrs = _gck_attributes_prepare_in (args->attrs, &n_attrs);

	/* Get the size of each value */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object,
	                                               attrs, n_attrs);
	if (!is_ok_get_attributes_rv (rv))
		return rv;

	/* Allocate memory for each value */
	attrs = _gck_attributes_commit_in (args->attrs, &n_attrs);

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
	gck_attributes_unref (args->attrs);
	g_free (args);
}


/**
 * gck_object_get:
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
GckAttributes*
gck_object_get (GckObject *self, GCancellable *cancellable, GError **err, ...)
{
	GckAttributes *attrs;
	GArray *array;
	va_list va;
	gulong type;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	array = g_array_new (FALSE, TRUE, sizeof (gulong));
	va_start (va, err);
	for (;;) {
		type = va_arg (va, gulong);
		if (type == GCK_INVALID)
			break;
		g_array_append_val (array, type);
	}
	va_end (va);

	attrs = gck_object_get_full (self, (gulong*)array->data, array->len, cancellable, err);
	g_array_free (array, TRUE);

	return attrs;
}

/**
 * gck_object_get_full:
 * @self: The object to get attributes from.
 * @attr_types: The types of the attributes to get.
 * @n_attr_types: The number of attr_types
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
GckAttributes*
gck_object_get_full (GckObject *self, gulong *attr_types, guint n_attr_types,
                      GCancellable *cancellable, GError **err)
{
	GetAttributes args;
	GckAttributes *attrs;
	gboolean ret;
	guint i;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (n_attr_types, NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	attrs = gck_attributes_new ();
	for (i = 0; i < n_attr_types; ++i)
		gck_attributes_add_empty (attrs, attr_types[i]);

	_gck_attributes_lock (attrs);

	memset (&args, 0, sizeof (args));
	args.attrs = attrs;
	args.object = self->pv->handle;

	ret = _gck_call_sync (self->pv->session, perform_get_attributes, NULL, &args, cancellable, err);
	_gck_attributes_unlock (attrs);

	if (!ret) {
		gck_attributes_unref (attrs);
		attrs = NULL;
	}

	return attrs;
}

/**
 * gck_object_get_async:
 * @self: The object to get attributes from.
 * @attr_types: The types of the attributes to get.
 * @n_attr_types: The number of attr_types
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: A callback which is called when the operation completes.
 * @user_data: Data to be passed to the callback.
 *
 * Get the specified attributes from the object. The attributes will be cleared
 * of their current values, and new attributes will be stored. The attributes
 * should not be accessed in any way except for referencing and unreferencing
 * them until gck_object_get_finish() is called.
 *
 * This call returns immediately and completes asynchronously.
 **/
void
gck_object_get_async (GckObject *self, gulong *attr_types, guint n_attr_types, GCancellable *cancellable,
                      GAsyncReadyCallback callback, gpointer user_data)
{
	GckAttributes *attrs;
	GetAttributes *args;
	guint i;

	g_return_if_fail (GCK_IS_OBJECT (self));
	g_return_if_fail (n_attr_types);

	attrs = gck_attributes_new ();
	for (i = 0; i < n_attr_types; ++i)
		gck_attributes_add_empty (attrs, attr_types[i]);

	args = _gck_call_async_prep (self->pv->session, self, perform_get_attributes,
	                             NULL, sizeof (*args), free_get_attributes);

	_gck_attributes_lock (attrs);
	args->attrs = attrs;
	args->object = self->pv->handle;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_object_get_finish:
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
GckAttributes*
gck_object_get_finish (GckObject *self, GAsyncResult *result, GError **err)
{
	GetAttributes *args;
	GckAttributes *attrs;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GCK_IS_CALL (result), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	args = _gck_call_arguments (result, GetAttributes);
	_gck_attributes_unlock (args->attrs);
	attrs = gck_attributes_ref (args->attrs);

	if (!_gck_call_basic_finish (result, err)) {
		gck_attributes_unref (attrs);
		attrs = NULL;
	}

	return attrs;
}

/* ---------------------------------------------------------------------------------
 * GET ATTRIBUTE DATA
 */

typedef struct _GetAttributeData {
	GckArguments base;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_TYPE type;
	GckAllocator allocator;
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
 * gck_object_get_data:
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
gck_object_get_data (GckObject *self, gulong attr_type, GCancellable *cancellable,
                     gsize *n_data, GError **err)
{
	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	return gck_object_get_data_full (self, attr_type, g_realloc, cancellable, n_data, err);
}

/**
 * gck_object_get_data_full:
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
gck_object_get_data_full (GckObject *self, gulong attr_type, GckAllocator allocator,
                           GCancellable *cancellable, gsize *n_data, GError **err)
{
	GetAttributeData args;
	gboolean ret;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	if (!allocator)
		allocator = g_realloc;

	memset (&args, 0, sizeof (args));
	args.allocator = allocator;
	args.object = self->pv->handle;
	args.type = attr_type;

	ret = _gck_call_sync (self->pv->session, perform_get_attribute_data, NULL, &args, cancellable, err);

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
 * gck_object_get_data_async:
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
gck_object_get_data_async (GckObject *self, gulong attr_type, GckAllocator allocator,
                            GCancellable *cancellable, GAsyncReadyCallback callback,
                            gpointer user_data)
{
	GetAttributeData *args;

	g_return_if_fail (GCK_IS_OBJECT (self));

	if (!allocator)
		allocator = g_realloc;

	args = _gck_call_async_prep (self->pv->session, self, perform_get_attribute_data,
	                             NULL, sizeof (*args), free_get_attribute_data);

	args->allocator = allocator;
	args->object = self->pv->handle;
	args->type = attr_type;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_object_get_data_finish:
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
gck_object_get_data_finish (GckObject *self, GAsyncResult *result,
                             gsize *n_data, GError **err)
{
	GetAttributeData *args;
	guchar *data;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GCK_IS_CALL (result), NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	if (!_gck_call_basic_finish (result, err))
		return NULL;

	args = _gck_call_arguments (result, GetAttributeData);

	*n_data = args->n_result;
	data = args->result;
	args->result = NULL;

	return data;
}

/* ---------------------------------------------------------------------------------------
 * SET TEMPLATE
 */

typedef struct _set_template_args {
	GckArguments base;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_TYPE type;
	GckAttributes *attrs;
} set_template_args;

static CK_RV
perform_set_template (set_template_args *args)
{
	CK_ATTRIBUTE attr;
	CK_ULONG n_attrs;

	g_assert (args);

	attr.type = args->type;
	attr.pValue = _gck_attributes_commit_out (args->attrs, &n_attrs);
	attr.ulValueLen = n_attrs * sizeof (CK_ATTRIBUTE);

	return (args->base.pkcs11->C_SetAttributeValue) (args->base.handle, args->object, &attr, 1);
}

static void
free_set_template (set_template_args *args)
{
	g_assert (args);
	gck_attributes_unref (args->attrs);
	g_free (args);
}

/**
 * gck_object_set_template:
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
gck_object_set_template (GckObject *self, gulong attr_type, GckAttributes *attrs,
                         GCancellable *cancellable, GError **err)
{
	set_template_args args;
	gboolean ret = FALSE;

	g_return_val_if_fail (GCK_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (attrs, FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	_gck_attributes_lock (attrs);

	memset (&args, 0, sizeof (args));
	args.attrs = attrs;
	args.type = attr_type;
	args.object = self->pv->handle;

	ret = _gck_call_sync (self->pv->session, perform_set_template, NULL, &args, cancellable, err);

	_gck_attributes_unlock (attrs);
	return ret;
}

/**
 * gck_object_set_template_async:
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
gck_object_set_template_async (GckObject *self, gulong attr_type, GckAttributes *attrs,
                                GCancellable *cancellable, GAsyncReadyCallback callback,
                                gpointer user_data)
{
	set_template_args *args;

	g_return_if_fail (GCK_IS_OBJECT (self));
	g_return_if_fail (attrs);

	args = _gck_call_async_prep (self->pv->session, self, perform_set_template,
	                             NULL, sizeof (*args), free_set_template);

	_gck_attributes_lock (attrs);
	args->attrs = gck_attributes_ref (attrs);
	args->type = attr_type;
	args->object = self->pv->handle;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_object_set_template_finish:
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
gck_object_set_template_finish (GckObject *self, GAsyncResult *result, GError **err)
{
	set_template_args *args;

	g_return_val_if_fail (GCK_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GCK_IS_CALL (result), FALSE);
	g_return_val_if_fail (!err || !*err, FALSE);

	/* Unlock the attributes we were using */
	args = _gck_call_arguments (result, set_template_args);
	g_assert (args->attrs);
	_gck_attributes_unlock (args->attrs);

	return _gck_call_basic_finish (result, err);
}

/* ---------------------------------------------------------------------------------------
 * GET TEMPLATE
 */

typedef struct _get_template_args {
	GckArguments base;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_TYPE type;
	GckAttributes *attrs;
} get_template_args;

static CK_RV
perform_get_template (get_template_args *args)
{
	CK_ATTRIBUTE attr;
	CK_ULONG n_attrs, i;
	CK_RV rv;

	g_assert (args);
	g_assert (!args->attrs);

	args->attrs = gck_attributes_new ();
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
		gck_attributes_add_empty (args->attrs, 0);

	/* Prepare all the attributes */
	_gck_attributes_lock (args->attrs);
	attr.pValue = _gck_attributes_prepare_in (args->attrs, &n_attrs);

	/* Get the size of each value */
	rv = (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object, &attr, 1);
	if (rv != CKR_OK)
		return rv;

	/* Allocate memory for each value */
	attr.pValue = _gck_attributes_commit_in (args->attrs, &n_attrs);

	/* Now get the actual values */
	return (args->base.pkcs11->C_GetAttributeValue) (args->base.handle, args->object, &attr, 1);
}

static void
free_get_template (get_template_args *args)
{
	g_assert (args);
	gck_attributes_unref (args->attrs);
	g_free (args);
}

/**
 * gck_object_get_template:
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
GckAttributes*
gck_object_get_template (GckObject *self, gulong attr_type,
                         GCancellable *cancellable, GError **err)
{
	get_template_args args;
	gboolean ret;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	memset (&args, 0, sizeof (args));
	args.object = self->pv->handle;
	args.type = attr_type;

	ret = _gck_call_sync (self->pv->session, perform_get_template, NULL, &args, cancellable, err);

	_gck_attributes_unlock (args.attrs);

	/* Free any value if failed */
	if (!ret) {
		gck_attributes_unref (args.attrs);
		args.attrs = NULL;
	}

	return args.attrs;
}

/**
 * gck_object_get_template_async:
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
gck_object_get_template_async (GckObject *self, gulong attr_type,
                                GCancellable *cancellable, GAsyncReadyCallback callback,
                                gpointer user_data)
{
	get_template_args *args;

	g_return_if_fail (GCK_IS_OBJECT (self));

	args = _gck_call_async_prep (self->pv->session, self, perform_get_template,
	                             NULL, sizeof (*args), free_get_template);

	args->object = self->pv->handle;
	args->type = attr_type;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
}

/**
 * gck_object_get_template_finish:
 * @self: The object to get an attribute from.
 * @result: The result passed to the callback.
 * @err: A location to store an error.
 *
 * Get the result of an operation to get attribute template from
 * an object.
 *
 * Return value: The resulting PKCS#11 attribute template, or NULL if an error occurred.
 **/
GckAttributes*
gck_object_get_template_finish (GckObject *self, GAsyncResult *result,
                                GError **err)
{
	get_template_args *args;

	g_return_val_if_fail (GCK_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GCK_IS_CALL (result), NULL);
	g_return_val_if_fail (!err || !*err, NULL);

	if (!_gck_call_basic_finish (result, err))
		return NULL;

	args = _gck_call_arguments (result, get_template_args);
	_gck_attributes_unlock (args->attrs);
	return gck_attributes_ref (args->attrs);
}
