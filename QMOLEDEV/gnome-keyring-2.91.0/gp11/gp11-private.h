/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gp11-private.h - the GObject PKCS#11 wrapper library

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

#ifndef GP11_PRIVATE_H_
#define GP11_PRIVATE_H_

#include "gp11.h"

#include <glib.h>
#include <glib-object.h>
#include <gio/gio.h>

G_BEGIN_DECLS

/* ---------------------------------------------------------------------------
 * ATTRIBUTE INTERNALS
 */

void                _gp11_attributes_lock                   (GP11Attributes *attrs);

void                _gp11_attributes_unlock                 (GP11Attributes *attrs);

CK_ATTRIBUTE_PTR    _gp11_attributes_prepare_in             (GP11Attributes *attrs, 
                                                             CK_ULONG_PTR n_attrs);

CK_ATTRIBUTE_PTR    _gp11_attributes_commit_in              (GP11Attributes *attrs, 
                                                             CK_ULONG_PTR n_attrs);

CK_ATTRIBUTE_PTR    _gp11_attributes_commit_out             (GP11Attributes *attrs, 
                                                             CK_ULONG_PTR n_attrs);

/* ----------------------------------------------------------------------------
 * MISC
 */

guint               _gp11_ulong_hash                        (gconstpointer v);

gboolean            _gp11_ulong_equal                       (gconstpointer v1, 
                                                             gconstpointer v2);

/* ----------------------------------------------------------------------------
 * MODULE
 */

gboolean            _gp11_module_fire_authenticate_slot     (GP11Module *module,
                                                             GP11Slot *slot,
                                                             gchar *label,
                                                             gchar **password);

gboolean            _gp11_module_fire_authenticate_object   (GP11Module *module,
                                                             GP11Object *object,
                                                             gchar *label,
                                                             gchar **password);

gboolean            _gp11_module_pool_session_handle        (GP11Session *session, 
                                                             CK_SESSION_HANDLE handle, 
                                                             GP11Module *self);

CK_SESSION_HANDLE   _gp11_module_pooled_session_handle      (GP11Module *module,
                                                             CK_SLOT_ID slot,
                                                             gulong flags);

/* ----------------------------------------------------------------------------
 * SLOT
 */

GP11Object*         _gp11_slot_object_from_handle           (GP11Slot *slot,
                                                             CK_OBJECT_HANDLE handle);

/* ----------------------------------------------------------------------------
 * CALL
 */

typedef CK_RV (*GP11PerformFunc) (gpointer call_data);
typedef gboolean (*GP11CompleteFunc) (gpointer call_data, CK_RV result); 

typedef struct _GP11Call GP11Call;

typedef struct _GP11Arguments {
	GP11Call *call;
	
	/* For the call function to use */
	CK_FUNCTION_LIST_PTR pkcs11;
	CK_ULONG handle;
	
} GP11Arguments;

#define GP11_ARGUMENTS_INIT 	   { NULL, NULL, 0 }

#define GP11_TYPE_CALL             (_gp11_call_get_type())
#define GP11_CALL(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), GP11_TYPE_CALL, GP11Call))
#define GP11_CALL_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass), GP11_TYPE_CALL, GP11Call))
#define GP11_IS_CALL(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj), GP11_TYPE_CALL))
#define GP11_IS_CALL_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass), GP11_TYPE_CALL))
#define GP11_CALL_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj), GP11_TYPE_CALL, GP11CallClass))

typedef struct _GP11CallClass GP11CallClass;

GType              _gp11_call_get_type                    (void) G_GNUC_CONST;

#define            _gp11_call_arguments(call, type)       (type*)(_gp11_call_get_arguments (GP11_CALL (call)))

gpointer           _gp11_call_get_arguments               (GP11Call *call);

void               _gp11_call_uninitialize                (void);

gboolean           _gp11_call_sync                        (gpointer object, 
                                                           gpointer perform, 
                                                           gpointer complete,
                                                           gpointer args, 
                                                           GCancellable *cancellable, 
                                                           GError **err);

gpointer           _gp11_call_async_prep                  (gpointer object, 
                                                           gpointer cb_object,
                                                           gpointer perform,
                                                           gpointer complete,
                                                           gsize args_size,
                                                           gpointer destroy_func);

GP11Call*          _gp11_call_async_ready                 (gpointer args, 
                                                           GCancellable *cancellable, 
                                                           GAsyncReadyCallback callback, 
                                                           gpointer user_data);

void               _gp11_call_async_go                    (GP11Call *call);

void               _gp11_call_async_ready_go              (gpointer args, 
                                                           GCancellable *cancellable, 
                                                           GAsyncReadyCallback callback, 
                                                           gpointer user_data);

void               _gp11_call_async_short                 (GP11Call *call, 
                                                           CK_RV rv);

gboolean           _gp11_call_basic_finish                (GAsyncResult *result,
                                                           GError **err);

void               _gp11_call_async_object                (GP11Call *call,
                                                           gpointer object);

#endif /* GP11_PRIVATE_H_ */
