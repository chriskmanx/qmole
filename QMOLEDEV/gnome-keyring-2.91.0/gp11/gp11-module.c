/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gp11-module.c - the GObject PKCS#11 wrapper library

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
#include "gp11-marshal.h"

#include <string.h>

/**
 * SECTION:gp11-module
 * @title: GP11Module
 * @short_description: A loaded and initialized PKCS#11 module.
 * 
 * A GP11Module object holds a loaded PKCS#11 module. A PKCS#11 module is a shared library. 
 * 
 * You can load and initialize a PKCS#11 module with the gp11_module_initialize() call. If you already
 * have a loaded and initialized module that you'd like to use with the various GP11 functions, then 
 * you can use gp11_module_new(). 
 */

/**
 * GP11Module:
 * 
 * Holds a loaded and initialized PKCS#11 module.
 */

/**
 * GP11ModuleInfo:
 * @pkcs11_version_major: The major version of the module.
 * @pkcs11_version_minor: The minor version of the module.
 * @manufacturer_id: The module manufacturer.
 * @flags: The module PKCS&num;11 flags.
 * @library_description: The module description.
 * @library_version_major: The major version of the library.
 * @library_version_minor: The minor version of the library.
 * 
 * Holds information about the PKCS&num;11 module. 
 * 
 * This structure corresponds to CK_MODULE_INFO in the PKCS#11 standard. The 
 * strings are NULL terminated for easier use. 
 * 
 * Use gp11_module_info_free() to release this structure when done with it.
 */

/*
 * MT safe 
 * 
 * The only thing that can change after object initialization in
 * a GP11Module is the finalized flag, which can be set
 * to 1 in dispose.
 */

enum {
	PROP_0,
	PROP_PATH,
	PROP_FUNCTIONS,
	PROP_POOL_SESSIONS,
	PROP_AUTO_AUTHENTICATE
};

enum {
	AUTHENTICATE_SLOT,
	AUTHENTICATE_OBJECT,
	LAST_SIGNAL
};

typedef struct _GP11ModuleData {
	GModule *module;
	gchar *path;
	gboolean initialized;
	CK_FUNCTION_LIST_PTR funcs;
	CK_C_INITIALIZE_ARGS init_args;
} GP11ModuleData;

typedef struct _GP11ModulePrivate {
	GP11ModuleData data;
	GStaticMutex mutex;
	gboolean finalized;
	GHashTable *open_sessions;
	gint auto_authenticate;
} GP11ModulePrivate;

#define GP11_MODULE_GET_DATA(o) \
      (G_TYPE_INSTANCE_GET_PRIVATE((o), GP11_TYPE_MODULE, GP11ModuleData))

G_DEFINE_TYPE (GP11Module, gp11_module, G_TYPE_OBJECT);

static guint signals[LAST_SIGNAL] = { 0 }; 

typedef struct _SessionPool {
	CK_SLOT_ID slot;
	CK_FUNCTION_LIST_PTR funcs;
	GArray *ro_sessions; /* array of CK_SESSION_HANDLE */
	GArray *rw_sessions; /* array of CK_SESSION_HANDLE */
} SessionPool;

/* ----------------------------------------------------------------------------
 * HELPERS
 */

static CK_RV
create_mutex (void **mutex)
{
	if (!mutex)
		return CKR_ARGUMENTS_BAD;
	
	if (!g_thread_supported ()) {
		g_warning ("cannot create pkcs11 mutex, threading has not been initialized");
		return CKR_GENERAL_ERROR;
	}
		
	*mutex = g_mutex_new ();
	g_return_val_if_fail (*mutex, CKR_GENERAL_ERROR);
	return CKR_OK;
}

static CK_RV
destroy_mutex (void *mutex)
{
	if (!mutex)
		return CKR_MUTEX_BAD;
	g_mutex_free ((GMutex*)mutex);
	return CKR_OK;
}

static CK_RV
lock_mutex (void *mutex)
{
	if (!mutex)
		return CKR_MUTEX_BAD;
	g_mutex_lock ((GMutex*)mutex);
	return CKR_OK;
}

static CK_RV
unlock_mutex (void *mutex)
{
	if (!mutex)
		return CKR_MUTEX_BAD;
	g_mutex_unlock ((GMutex*)mutex);
	return CKR_OK;
}

static void
close_session (CK_FUNCTION_LIST_PTR funcs, CK_SESSION_HANDLE handle)
{
	CK_RV rv; 
	
	g_return_if_fail (funcs);
	
	rv = (funcs->C_CloseSession) (handle);
	if (rv != CKR_OK) {
		g_warning ("couldn't close session properly: %s",
		           gp11_message_from_rv (rv));
	}
}

/* ----------------------------------------------------------------------------
 * INTERNAL
 */

static GP11ModulePrivate*
lock_private (gpointer obj)
{
	GP11ModulePrivate *pv;
	GP11Module *self;
	
	g_assert (GP11_IS_MODULE (obj));
	self = GP11_MODULE (obj);
	
	g_object_ref (self);
	
	pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GP11_TYPE_MODULE, GP11ModulePrivate);
	g_static_mutex_lock (&pv->mutex);
	
	return pv;
}

static void
unlock_private (gpointer obj, GP11ModulePrivate *pv)
{
	GP11Module *self;

	g_assert (pv);
	g_assert (GP11_IS_MODULE (obj));
	
	self = GP11_MODULE (obj);
	
	g_assert (G_TYPE_INSTANCE_GET_PRIVATE (self, GP11_TYPE_MODULE, GP11ModulePrivate) == pv);
	
	g_static_mutex_unlock (&pv->mutex);
	g_object_unref (self);
}

static void
free_session_pool (gpointer p)
{
	SessionPool *pool = p;
	guint i;
	
	if (pool->ro_sessions) {
		for(i = 0; i < pool->ro_sessions->len; ++i)
			close_session (pool->funcs, g_array_index (pool->ro_sessions, CK_SESSION_HANDLE, i));
		g_array_free (pool->ro_sessions, TRUE);
	}
	
	if (pool->rw_sessions) {
		for(i = 0; i < pool->rw_sessions->len; ++i)
			close_session (pool->funcs, g_array_index (pool->rw_sessions, CK_SESSION_HANDLE, i));
		g_array_free (pool->rw_sessions, TRUE);
	}
	
	g_free (pool);
}

static gboolean
push_session_table (GP11ModulePrivate *pv, CK_SLOT_ID slot, gulong flags, CK_SESSION_HANDLE handle)
{
	SessionPool *pool;
	GArray *array;

	g_assert (handle);

	if (pv->open_sessions == NULL)
		return FALSE;
		
	pool = g_hash_table_lookup (pv->open_sessions, &slot);
	if (!pool) {
		pool = g_new0 (SessionPool, 1);
		pool->funcs = pv->data.funcs;
		g_hash_table_insert (pv->open_sessions, g_memdup (&slot, sizeof (slot)), pool);
	}

	if (flags & CKF_RW_SESSION) {
		if (!pool->rw_sessions)
			pool->rw_sessions = g_array_new (FALSE, TRUE, sizeof (CK_SESSION_HANDLE));
		array = pool->rw_sessions;
	} else {
		if (!pool->ro_sessions)
			pool->ro_sessions = g_array_new (FALSE, TRUE, sizeof (CK_SESSION_HANDLE));
		array = pool->ro_sessions;
	}

	g_array_append_val (array, handle);
	return TRUE;
}

static CK_SESSION_HANDLE
pop_session_table (GP11ModulePrivate *pv, CK_SLOT_ID slot, gulong flags)
{
	CK_SESSION_HANDLE result = 0;
	SessionPool *pool;
	GArray **array;

	g_return_val_if_fail (pv, 0);

	if (!pv->open_sessions)
		return 0;
	
	pool = g_hash_table_lookup (pv->open_sessions, &slot);
	if (pool == NULL) 
		return 0;
	
	if (flags & CKF_RW_SESSION)
		array = &pool->rw_sessions;
	else
		array = &pool->ro_sessions;
	
	if (*array == NULL)
		return 0;
	
	g_assert ((*array)->len > 0);
	result = g_array_index (*array, CK_SESSION_HANDLE, (*array)->len - 1);
	g_assert (result != 0);
	g_array_remove_index_fast (*array, (*array)->len - 1);
	
	if (!(*array)->len) {
		g_array_free (*array, TRUE);
		*array = NULL;
		if (!pool->rw_sessions && !pool->ro_sessions)
			g_hash_table_remove (pv->open_sessions, &slot);
	}

	return result;
}

static void
destroy_session_table (GP11ModulePrivate *pv)
{
	if (pv->open_sessions)
		g_hash_table_unref (pv->open_sessions);
	pv->open_sessions = NULL;
}

static void
create_session_table (GP11ModulePrivate *pv)
{
	if (!pv->open_sessions)
		pv->open_sessions = g_hash_table_new_full (_gp11_ulong_hash, _gp11_ulong_equal, g_free, free_session_pool);
}

CK_SESSION_HANDLE
_gp11_module_pooled_session_handle (GP11Module *self, CK_SLOT_ID slot, gulong flags)
{
	GP11ModulePrivate *pv = lock_private (self);
	CK_SESSION_HANDLE handle;
	
	g_return_val_if_fail (GP11_IS_MODULE (self), 0);

	{
		handle = pop_session_table (pv, slot, flags);
	}
	
	unlock_private (self, pv);
	
	return handle;
}

gboolean
_gp11_module_pool_session_handle (GP11Session *session, CK_SESSION_HANDLE handle, GP11Module *self)
{
	GP11ModuleData *data = GP11_MODULE_GET_DATA (self);
	GP11ModulePrivate *pv;
	CK_SESSION_INFO info;
	gboolean handled = FALSE;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_SESSION (session), FALSE);
	g_return_val_if_fail (GP11_IS_MODULE (self), FALSE);
	
	/* Get the session info so we know where to categorize this */
	rv = (data->funcs->C_GetSessionInfo) (handle, &info);

	if (rv == CKR_OK) {
	
		pv = lock_private (self);
		
		{
			/* Keep this one around for later use */
			handled = push_session_table (pv, info.slotID, info.flags, handle);
		}
		
		unlock_private (self, pv);
	
	} else {
	
		/* An already closed session, we don't want to bother with */
		if (rv == CKR_SESSION_CLOSED || rv == CKR_SESSION_HANDLE_INVALID)
			handled = TRUE;
	}

	return handled;
}

gboolean
_gp11_module_fire_authenticate_slot (GP11Module *self, GP11Slot *slot, gchar *label, gchar **password)
{
	GP11TokenInfo *info;
	gchar *allocated = NULL;
	gboolean ret;
	
	g_assert (GP11_IS_MODULE (self));

	info = gp11_slot_get_token_info (slot);
	if (info != NULL) {
		
		/* 
		 * We'll have tried to login at least once at this point,
		 * with NULL password. This means that CKF_PROTECTED_AUTHENTICATION_PATH
		 * tokens have had their chance and we don't need to prompt for it.
		 */

		if (info->flags & CKF_PROTECTED_AUTHENTICATION_PATH)
			return FALSE;
		
		if (label == NULL)
			label = allocated = g_strdup (info->label);
		
		gp11_token_info_free (info);
	}
	
	g_signal_emit (self, signals[AUTHENTICATE_SLOT], 0, slot, label, password, &ret);
	g_free (allocated);
	return ret;
}

gboolean
_gp11_module_fire_authenticate_object (GP11Module *self, GP11Object *object,
                                       gchar *label, gchar **password)
{
	GP11TokenInfo *info;
	GP11Slot *slot;
	gboolean ret;

	g_assert (GP11_IS_MODULE (self));
	g_assert (GP11_IS_OBJECT (object));
	g_assert (password);

	slot = gp11_object_get_slot (object);
	info = gp11_slot_get_token_info (slot);
	g_object_unref (slot);
	
	if (info != NULL) {
		if (info->flags & CKF_PROTECTED_AUTHENTICATION_PATH) {
			gp11_token_info_free (info);
			*password = NULL;
			return TRUE;
		}
		
		gp11_token_info_free (info);
	}

	g_signal_emit (self, signals[AUTHENTICATE_OBJECT], 0, object, label, password, &ret);
	return ret;
}

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static gboolean 
gp11_module_real_authenticate_slot (GP11Module *module, GP11Slot *self, gchar *label, gchar **password)
{
	return FALSE;
}

static gboolean 
gp11_module_real_authenticate_object (GP11Module *module, GP11Object *object, gchar *label, gchar **password)
{
	return FALSE;
}

static void
gp11_module_init (GP11Module *self)
{
	GP11ModulePrivate *pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GP11_TYPE_MODULE, GP11ModulePrivate);
	g_static_mutex_init (&pv->mutex);
}

static void
gp11_module_get_property (GObject *obj, guint prop_id, GValue *value, 
                          GParamSpec *pspec)
{
	GP11Module *self = GP11_MODULE (obj);

	switch (prop_id) {
	case PROP_PATH:
		g_value_set_string (value, gp11_module_get_path (self));
		break;
	case PROP_FUNCTIONS:
		g_value_set_pointer (value, gp11_module_get_functions (self));
		break;
	case PROP_AUTO_AUTHENTICATE:
		g_value_set_int (value, gp11_module_get_auto_authenticate (self));
		break;
	case PROP_POOL_SESSIONS:
		g_value_set_boolean (value, gp11_module_get_pool_sessions (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gp11_module_set_property (GObject *obj, guint prop_id, const GValue *value, 
                          GParamSpec *pspec)
{
	GP11Module *self = GP11_MODULE (obj);
	GP11ModuleData *data = GP11_MODULE_GET_DATA (obj);

	/* Only allowed during initialization */
	switch (prop_id) {
	case PROP_PATH:
		g_return_if_fail (!data->path);
		data->path = g_value_dup_string (value);
		break;
	case PROP_FUNCTIONS:
		g_return_if_fail (!data->funcs);
		data->funcs = g_value_get_pointer (value);
		break;
	case PROP_AUTO_AUTHENTICATE:
		gp11_module_set_auto_authenticate (self, g_value_get_int (value));
		break;
	case PROP_POOL_SESSIONS:
		gp11_module_set_pool_sessions (self, g_value_get_boolean (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gp11_module_dispose (GObject *obj)
{
	GP11ModuleData *data = GP11_MODULE_GET_DATA (obj);
	GP11ModulePrivate *pv = lock_private (obj);
	gboolean finalize = FALSE;
	CK_RV rv;
	
	{
		destroy_session_table (pv);
		
		if (!pv->finalized && data->initialized && data->funcs) {
			finalize = TRUE;
			pv->finalized = TRUE;
		}
	}

	unlock_private (obj, pv);

	/* Must be careful when accessing funcs */
	if (finalize) {
		rv = (data->funcs->C_Finalize) (NULL);
		if (rv != CKR_OK) {
			g_warning ("C_Finalize on module '%s' failed: %s", 
			           data->path, gp11_message_from_rv (rv));
		}
	}
	
	G_OBJECT_CLASS (gp11_module_parent_class)->dispose (obj);
}

static void
gp11_module_finalize (GObject *obj)
{
	GP11ModulePrivate *pv = G_TYPE_INSTANCE_GET_PRIVATE (obj, GP11_TYPE_MODULE, GP11ModulePrivate);
	GP11ModuleData *data = GP11_MODULE_GET_DATA (obj);

	g_assert (!pv->open_sessions);
	
	data->funcs = NULL;
	
	if (data->module) {
		if (!g_module_close (data->module))
			g_warning ("failed to close the pkcs11 module: %s", 
			           g_module_error ());
		data->module = NULL;
	}

	g_free (data->path);
	data->path = NULL;
	
	g_static_mutex_free (&pv->mutex);
	
	G_OBJECT_CLASS (gp11_module_parent_class)->finalize (obj);
}


static void
gp11_module_class_init (GP11ModuleClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gp11_module_parent_class = g_type_class_peek_parent (klass);
	
	gobject_class->get_property = gp11_module_get_property;
	gobject_class->set_property = gp11_module_set_property;
	gobject_class->dispose = gp11_module_dispose;
	gobject_class->finalize = gp11_module_finalize;
	
	klass->authenticate_object = gp11_module_real_authenticate_object;
	klass->authenticate_slot = gp11_module_real_authenticate_slot;

	/**
	 * GP11Module:path:
	 * 
	 * The PKCS&num;11 module file path. 
	 * 
	 * This may be set to NULL if this object was created from an already
	 * initialized module via the gp11_module_new() function. 
	 */
	g_object_class_install_property (gobject_class, PROP_PATH,
		g_param_spec_string ("path", "Module Path", "Path to the PKCS11 Module",
		                     NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GP11Module:functions:
	 * 
	 * The raw PKCS&num;11 function list for the module.
	 * 
	 * This points to a CK_FUNCTION_LIST structure. 
	 */
	g_object_class_install_property (gobject_class, PROP_FUNCTIONS,
		g_param_spec_pointer ("functions", "Function List", "PKCS11 Function List",
		                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GP11Module:auto-authenticate:
	 * 
	 * Whether or not to automatically authenticate token objects that need
	 * a C_Login call before they can be used.
	 * 
	 * The #GP11Module::authenticate-object signal will be fired when an 
	 * object needs to be authenticated.
	 */
	g_object_class_install_property (gobject_class, PROP_AUTO_AUTHENTICATE,
		g_param_spec_int ("auto-authenticate", "Auto Authenticate", "Auto Login to Token when necessary",
		                  0, G_MAXINT, 0, G_PARAM_READWRITE));
	
	/**
	 * GP11Module:pool-sessions:
	 * 
	 * Whether or not to pool PKCS&num;11 sessions. When this is set, sessions
	 * will be pooled and reused if their flags match when gp11_slot_open_session() 
	 * is called. 
	 */
	g_object_class_install_property (gobject_class, PROP_POOL_SESSIONS,
		g_param_spec_boolean ("pool-sessions", "Pool Sessions", "Pool sessions?",
		                      FALSE, G_PARAM_READWRITE));

	/**
	 * GP11Module::authenticate-slot:
	 * @module: The module
	 * @slot: The slot to be authenticated.
	 * @string: A displayable label which describes the object.
	 * @password: A gchar** where a password should be returned. 
	 * 
	 * This signal is emitted when a password is needed to authenticate a PKCS&num;11 
	 * slot. If the module prompts for passwords itself, then this signal will 
	 * not be emitted.
	 * 
	 * Returns: FALSE if the user cancelled, TRUE if we should proceed.
	 */
	signals[AUTHENTICATE_SLOT] = g_signal_new ("authenticate-slot", GP11_TYPE_MODULE, 
			G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GP11ModuleClass, authenticate_slot),
			g_signal_accumulator_true_handled, NULL, _gp11_marshal_BOOLEAN__OBJECT_STRING_POINTER, 
			G_TYPE_BOOLEAN, 3, GP11_TYPE_SLOT, G_TYPE_STRING, G_TYPE_POINTER);

	/**
	 * GP11Module::authenticate-object:
	 * @module: The module.
	 * @object: The object to be authenticated.
	 * @label: A displayable label which describes the object.
	 * @password: A gchar** where a password should be returned.
	 * 
	 * This signal is emitted when a password is needed to authenticate a PKCS&num;11
	 * object like a key. If the module prompts for passwords itself, then this signal will 
	 * not be emitted.
	 * 
	 * Returns: FALSE if the user cancelled, TRUE if we should proceed.
	 */
	signals[AUTHENTICATE_OBJECT] = g_signal_new ("authenticate-object", GP11_TYPE_MODULE, 
			G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GP11ModuleClass, authenticate_object),
			g_signal_accumulator_true_handled, NULL, _gp11_marshal_BOOLEAN__OBJECT_STRING_POINTER, 
			G_TYPE_BOOLEAN, 3, GP11_TYPE_OBJECT, G_TYPE_STRING, G_TYPE_POINTER);

	g_type_class_add_private (gobject_class, sizeof (GP11ModulePrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC 
 */

/**
 * gp11_module_info_free:
 * @module_info: The module info to free, or NULL.
 * 
 * Free a GP11ModuleInfo structure.
 **/
void
gp11_module_info_free (GP11ModuleInfo *module_info)
{
	if (!module_info)
		return;
	g_free (module_info->library_description);
	g_free (module_info->manufacturer_id);
	g_free (module_info);
}

/**
 * gp11_module_initialize:
 * @path: The file system path to the PKCS#11 module to load.
 * @reserved: Extra arguments for the PKCS#11 module, should usually be NULL.
 * @err: A location to store an error resulting from a failed load.
 * 
 * Load and initialize a PKCS#11 module represented by a GP11Module object.
 * 
 * Return value: The loaded PKCS#11 module or NULL if failed.
 **/
GP11Module*
gp11_module_initialize (const gchar *path, gpointer reserved, GError **err)
{
	CK_C_GetFunctionList get_function_list;
	CK_FUNCTION_LIST_PTR funcs;
	GP11ModuleData *data;
	GModule *module;
	GP11Module *mod;
	CK_RV rv;
	
	g_return_val_if_fail (path != NULL, NULL);
	g_return_val_if_fail (!err || !*err, NULL);
	
	/* Load the actual module */
	module = g_module_open (path, 0);
	if (!module) {
		g_set_error (err, GP11_ERROR, (int)CKR_GP11_MODULE_PROBLEM,
		             "Error loading pkcs11 module: %s", g_module_error ());
		return NULL;
	}
	
	/* Get the entry point */
	if (!g_module_symbol (module, "C_GetFunctionList", (void**)&get_function_list)) {
		g_set_error (err, GP11_ERROR, (int)CKR_GP11_MODULE_PROBLEM,
		             "Invalid pkcs11 module: %s", g_module_error ());
		g_module_close (module);
		return NULL;
	}
	
	/* Get the function list */
	rv = (get_function_list) (&funcs);
	if (rv != CKR_OK) {
		g_set_error (err, GP11_ERROR, rv, "Couldn't get pkcs11 function list: %s",
		             gp11_message_from_rv (rv));
		g_module_close (module);
		return NULL;
	}
	
	mod = g_object_new (GP11_TYPE_MODULE, "functions", funcs, "path", path, NULL);
	data = GP11_MODULE_GET_DATA (mod);
	data->module = module;

	memset (&data->init_args, 0, sizeof (data->init_args));
	data->init_args.flags = CKF_OS_LOCKING_OK;
	data->init_args.CreateMutex = create_mutex;
	data->init_args.DestroyMutex = destroy_mutex;
	data->init_args.LockMutex = lock_mutex;
	data->init_args.UnlockMutex = unlock_mutex;
	data->init_args.pReserved = reserved;
	
	/* Now initialize the module */
	rv = (data->funcs->C_Initialize) (&data->init_args);
	if (rv != CKR_OK) {
		g_set_error (err, GP11_ERROR, rv, "Couldn't initialize module: %s",
		             gp11_message_from_rv (rv));
		g_object_unref (mod);
		return NULL;
	}
	
	data->initialized = TRUE;
	return mod;
}

/**
 * gp11_module_new:
 * @funcs: Initialized PKCS#11 function list pointer
 *
 * Create a GP11Module representing a PKCS#11 module. It is assumed that 
 * this the module is already initialized. In addition it will not be 
 * finalized when complete.
 * 
 * Return value: The new PKCS#11 module.
 **/
GP11Module*
gp11_module_new (CK_FUNCTION_LIST_PTR funcs)
{
	g_return_val_if_fail (funcs, NULL);
	return g_object_new (GP11_TYPE_MODULE, "functions", funcs, NULL);
}

/**
 * gp11_module_equal:
 * @module1: A pointer to the first GP11Module
 * @module2: A pointer to the second GP11Module
 * 
 * Checks equality of two modules. Two GP11Module objects can point to the same 
 * underlying PKCS#11 module.
 * 
 * Return value: TRUE if module1 and module2 are equal. FALSE if either is not a GP11Module.
 **/
gboolean
gp11_module_equal (gconstpointer module1, gconstpointer module2)
{
	GP11ModuleData *data1, *data2;

	if (module1 == module2)
		return TRUE;
	if (!GP11_IS_MODULE (module1) || !GP11_IS_MODULE (module2))
		return FALSE;
	
	data1 = GP11_MODULE_GET_DATA (module1);
	data2 = GP11_MODULE_GET_DATA (module2);
	
	return data1->funcs == data2->funcs;
}

/**
 * gp11_module_hash:
 * @module: A pointer to a GP11Module
 * 
 * Create a hash value for the GP11Module. 
 * 
 * This function is intended for easily hashing a GP11Module to add to 
 * a GHashTable or similar data structure.
 * 
 * Return value: An integer that can be used as a hash value, or 0 if invalid.
 **/
guint
gp11_module_hash (gconstpointer module)
{
	GP11ModuleData *data;
	
	g_return_val_if_fail (GP11_IS_MODULE (module), 0);

	data = GP11_MODULE_GET_DATA (module);
	
	return g_direct_hash (data->funcs);
}

/**
 * gp11_module_get_info:
 * @self: The module to get info for.
 * 
 * Get the info about a PKCS#11 module. 
 * 
 * Return value: The module info. Release this with gp11_module_info_free().
 **/
GP11ModuleInfo*
gp11_module_get_info (GP11Module *self)
{
	GP11ModuleData *data = GP11_MODULE_GET_DATA (self);
	GP11ModuleInfo *modinfo;
	CK_INFO info;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_MODULE (self), NULL);
	g_return_val_if_fail (data->funcs, NULL);
	
	memset (&info, 0, sizeof (info));
	rv = (data->funcs->C_GetInfo (&info));
	if (rv != CKR_OK) {
		g_warning ("couldn't get module info: %s", gp11_message_from_rv (rv));
		return NULL;
	}
	
	modinfo = g_new0 (GP11ModuleInfo, 1);
	modinfo->flags = info.flags;
	modinfo->library_description = gp11_string_from_chars (info.libraryDescription, 
	                                                       sizeof (info.libraryDescription));
	modinfo->manufacturer_id = gp11_string_from_chars (info.manufacturerID,
	                                                   sizeof (info.manufacturerID));
	modinfo->library_version_major = info.libraryVersion.major;
	modinfo->library_version_minor = info.libraryVersion.minor;
	modinfo->pkcs11_version_major = info.cryptokiVersion.major;
	modinfo->pkcs11_version_minor = info.cryptokiVersion.minor;
	
	return modinfo;
}

/**
 * gp11_module_get_slots:
 * @self: The module for which to get the slots.
 * @token_present: Whether to limit only to slots with a token present.
 * 
 * Get the GP11Slot objects for a given module. 
 * 
 * Return value: The possibly empty list of slots. Release this with gp11_list_unref_free().
 */
GList*
gp11_module_get_slots (GP11Module *self, gboolean token_present)
{
	GP11ModuleData *data = GP11_MODULE_GET_DATA (self);
	CK_SLOT_ID_PTR slot_list;
	CK_ULONG count, i;
	GList *result;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_MODULE (self), NULL);
	g_return_val_if_fail (data->funcs, NULL);

	rv = (data->funcs->C_GetSlotList) (token_present ? CK_TRUE : CK_FALSE, NULL, &count);
	if (rv != CKR_OK) {
		g_warning ("couldn't get slot count: %s", gp11_message_from_rv (rv));
		return NULL;
	}
	
	if (!count)
		return NULL;
	
	slot_list = g_new (CK_SLOT_ID, count);
	rv = (data->funcs->C_GetSlotList) (token_present ? CK_TRUE : CK_FALSE, slot_list, &count);
	if (rv != CKR_OK) {
		g_warning ("couldn't get slot list: %s", gp11_message_from_rv (rv));
		g_free (slot_list);
		return NULL;
	}
	
	result = NULL;
	for (i = 0; i < count; ++i) {
		result = g_list_prepend (result, g_object_new (GP11_TYPE_SLOT, 
		                                               "handle", slot_list[i],
		                                               "module", self, NULL));
	}
	
	g_free (slot_list);
	return g_list_reverse (result);
}

/**
 * gp11_module_get_path:
 * @self: The module for which to get the path.
 * 
 * Get the file path of this module. This may not be an absolute path, and 
 * usually reflects the path passed to gp11_module_initialize().
 * 
 * Return value: The path, do not modify or free this value. 
 **/
const gchar*
gp11_module_get_path (GP11Module *self)
{
	GP11ModuleData *data = GP11_MODULE_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_MODULE (self), NULL);
	return data->path;
}

/**
 * gp11_module_get_functions:
 * @self: The module for which to get the function list.
 * 
 * Get the PKCS#11 function list for the module.
 * 
 * Return value: The function list, do not modify this structure. 
 **/
CK_FUNCTION_LIST_PTR
gp11_module_get_functions (GP11Module *self)
{
	GP11ModuleData *data = GP11_MODULE_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_MODULE (self), NULL);
	return data->funcs;	
}


/**
 * gp11_module_get_pool_sessions:
 * @self: The module to get setting from.
 * 
 * Get the reuse sessions setting. When this is set, sessions
 * will be pooled and reused if their flags match when 
 * gp11_slot_open_session() is called. 
 * 
 * Return value: Whether reusing sessions or not.
 **/
gboolean
gp11_module_get_pool_sessions (GP11Module *self)
{
	GP11ModulePrivate *pv = lock_private (self);
	gboolean ret;
	
	g_return_val_if_fail (pv, FALSE);
	
	{
		ret = pv->open_sessions != NULL;
	}
	
	unlock_private (self, pv);

	return ret;
}

/**
 * gp11_module_set_pool_sessions:
 * @self: The module to set the setting on.
 * @pool: Whether to reuse sessions or not.
 * 
 * When this is set, sessions will be pooled and reused
 * if their flags match when gp11_slot_open_session() is called.
 **/
void
gp11_module_set_pool_sessions (GP11Module *self, gboolean pool)
{
	GP11ModulePrivate *pv = lock_private (self);

	g_return_if_fail (pv);
	
	{
		if (pool)
			create_session_table (pv);
		else
			destroy_session_table (pv);
	}
	
	unlock_private (self, pv);
	g_object_notify (G_OBJECT (self), "pool-sessions");
}

/**
 * gp11_module_get_auto_authenticate:
 * @self: The module to get setting from.
 * 
 * Get the auto login setting. When this is set, this slot 
 * will emit the 'authenticate-slot' signal when a session
 * requires authentication, and the 'authenticate-object'
 * signal when an object requires authintication.
 * 
 * Return value: Whether auto login or not.
 **/
gint
gp11_module_get_auto_authenticate (GP11Module *self)
{
	GP11ModulePrivate *pv = lock_private (self);
	gint ret;
	
	g_return_val_if_fail (pv, FALSE);
	
	{
		ret = pv->auto_authenticate;
	}
	
	unlock_private (self, pv);

	return ret;
}

/**
 * gp11_module_set_auto_authenticate:
 * @self: The module to set the setting on.
 * @auto_authenticate: Whether auto login or not.
 * 
 * When this is set, this slot 
 * will emit the 'authenticate-slot' signal when a session
 * requires authentication, and the 'authenticate-object'
 * signal when an object requires authintication.
 **/
void
gp11_module_set_auto_authenticate (GP11Module *self, gint auto_authenticate)
{
	GP11ModulePrivate *pv = lock_private (self);
	
	/* HACK: Get needed fix around API freeze. */
	if (auto_authenticate == 1)
		auto_authenticate = GP11_AUTHENTICATE_TOKENS | GP11_AUTHENTICATE_OBJECTS;

	g_return_if_fail (pv);
	
	{
		pv->auto_authenticate = auto_authenticate;
	}
	
	unlock_private (self, pv);
	g_object_notify (G_OBJECT (self), "auto-authenticate");
}

/**
 * gp11_module_enumerate_objects:
 * @self: The module to enumerate objects.
 * @func: Function to call for each object.
 * @user_data: Data to pass to the function.
 * @...: The arguments must be triples of: attribute type, data type, value.
 *
 * Call a function for every matching object on the module. This call may 
 * block for an indefinite period.
 * 
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
 * This function will open a session per slot. It's recommended that you 
 * set the 'reuse-sessions' property on each slot if you'll be calling 
 * it a lot.
 * 
 * You can access the session in which the object was found, by using the 
 * gp11_object_get_session() function on the resulting objects.
 * 
 * This function skips tokens that are not initialize, and makes a best effort to 
 * find objects on valid tokens. 
 * 
 * The function can return FALSE to stop the enumeration.
 * 
 * Return value: If FALSE then an error prevented all matching objects from being enumerated.
 **/
gboolean
gp11_module_enumerate_objects (GP11Module *self, GP11ObjectForeachFunc func,
                               gpointer user_data, ...)
{
	GP11Attributes *attrs;
	GError *error = NULL;
	va_list va;
	
	va_start (va, user_data);
	attrs = gp11_attributes_new_valist (g_realloc, va);
	va_end (va);

	gp11_module_enumerate_objects_full (self, attrs, NULL, func, user_data, &error);
	gp11_attributes_unref (attrs);
	
	if (error != NULL) {
		g_warning ("enumerating objects failed: %s", error->message);
		g_clear_error (&error);
		return FALSE;
	}
	
	return TRUE;
}

/**
 * gp11_module_enumerate_objects_full:
 * @self: The module to enumerate objects.
 * @attrs: Attributes that the objects must have, or empty for all objects.
 * @cancellable: Optional cancellation object, or NULL.
 * @func: Function to call for each object.
 * @user_data: Data to pass to the function.
 * @error: Location to return error information.
 * 
 * Call a function for every matching object on the module. This call may 
 * block for an indefinite period. 
 * 
 * This function will open a session per slot. It's recommended that you 
 * set the 'reuse-sessions' property on each slot if you'll be calling 
 * it a lot.
 * 
 * You can access the session in which the object was found, by using the 
 * gp11_object_get_session() function on the resulting objects.
 * 
 * The function can return FALSE to stop the enumeration.
 * 
 * Return value: If FALSE then an error prevented all matching objects from being enumerated.
 **/
gboolean
gp11_module_enumerate_objects_full (GP11Module *self, GP11Attributes *attrs, 
                                    GCancellable *cancellable, GP11ObjectForeachFunc func, 
                                    gpointer user_data, GError **err)
{
	gboolean stop = FALSE;
	gboolean ret = TRUE;
	GList *objects, *o;
	GList *slots, *l;
	GError *error = NULL;
	GP11Session *session;
	
	g_return_val_if_fail (GP11_IS_MODULE (self), FALSE);
	g_return_val_if_fail (attrs, FALSE);
	g_return_val_if_fail (func, FALSE);
	
	gp11_attributes_ref (attrs);
	slots = gp11_module_get_slots (self, TRUE);
	
	for (l = slots; ret && !stop && l; l = g_list_next (l)) {

		/* TODO: We really should allow the caller to specify the flags, at least read-write */
		session = gp11_slot_open_session (l->data, CKF_RW_SESSION | CKF_SERIAL_SESSION, &error);
		if (!session) {
			g_return_val_if_fail (error != NULL, FALSE);
			
			/* Ignore these errors when enumerating */
			if (g_error_matches (error, GP11_ERROR, CKR_USER_PIN_NOT_INITIALIZED)) {
				g_clear_error (&error);
				
			} else {
				ret = FALSE;
				g_propagate_error (err, error);
				error = NULL;
			}
			continue;
		}
		
		objects = gp11_session_find_objects_full (session, attrs, cancellable, &error);
		if (error) {
			ret = FALSE;
			g_object_unref (session);
			g_propagate_error (err, error);
			error = NULL;
			continue;
		}
		
		for (o = objects; !stop && o; o = g_list_next (o)) {
			gp11_object_set_session (o->data, session);
			if (!(func)(o->data, user_data)) {
				stop = TRUE;
				break;
			}
		}
		
		g_object_unref (session);
		gp11_list_unref_free (objects);
	}
	
	gp11_list_unref_free (slots);
	gp11_attributes_unref (attrs);
	
	return ret;
}

/**
 * GP11ObjectForeachFunc:
 * @object: The enumerated object.
 * @user_data: Data passed to enumerate function.
 * 
 * This function is passed to gp11_module_enumerate_objects() or a similar function.
 * It is called once for each object matched. 
 * 
 * The GP11Session through which the object is accessible can be retrieved by calling
 * gp11_object_get_session() on object.
 * 
 * Returns: TRUE to continue enumerating, FALSE to stop.
 */
