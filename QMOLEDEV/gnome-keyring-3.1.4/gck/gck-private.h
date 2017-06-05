/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-private.h - the GObject PKCS#11 wrapper library

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

#ifndef GCK_PRIVATE_H_
#define GCK_PRIVATE_H_

#include "gck.h"

#include <glib.h>
#include <glib-object.h>
#include <gio/gio.h>

G_BEGIN_DECLS

/* ---------------------------------------------------------------------------
 * ATTRIBUTE INTERNALS
 */

void                _gck_attributes_lock                   (GckAttributes *attrs);

void                _gck_attributes_unlock                 (GckAttributes *attrs);

CK_ATTRIBUTE_PTR    _gck_attributes_prepare_in             (GckAttributes *attrs,
                                                             CK_ULONG_PTR n_attrs);

CK_ATTRIBUTE_PTR    _gck_attributes_commit_in              (GckAttributes *attrs,
                                                             CK_ULONG_PTR n_attrs);

CK_ATTRIBUTE_PTR    _gck_attributes_commit_out             (GckAttributes *attrs,
                                                             CK_ULONG_PTR n_attrs);

/* ----------------------------------------------------------------------------
 * MISC
 */

guint               _gck_ulong_hash                        (gconstpointer v);

gboolean            _gck_ulong_equal                       (gconstpointer v1,
                                                             gconstpointer v2);

/* ----------------------------------------------------------------------------
 * MODULE
 */

GckModule*          _gck_module_new_initialized            (CK_FUNCTION_LIST_PTR funcs);

gboolean            _gck_module_fire_authenticate_slot     (GckModule *module,
                                                             GckSlot *slot,
                                                             gchar *label,
                                                             gchar **password);

gboolean            _gck_module_fire_authenticate_object   (GckModule *module,
                                                             GckObject *object,
                                                             gchar *label,
                                                             gchar **password);

GckModuleInfo*      _gck_module_info_from_pkcs11            (CK_INFO_PTR info);

void                _gck_module_info_to_pkcs11              (GckModuleInfo* module_info,
                                                             CK_INFO_PTR info);

gboolean            _gck_module_info_match                  (GckModuleInfo *match,
                                                             GckModuleInfo *module_info);

/* -----------------------------------------------------------------------------
 * ENUMERATOR
 */

GckEnumerator*      _gck_enumerator_new                     (GList *modules,
                                                             guint session_options,
                                                             GckUriData *uri_data);

/* ----------------------------------------------------------------------------
 * SLOT
 */

GckTokenInfo*      _gck_token_info_from_pkcs11              (CK_TOKEN_INFO_PTR info);

void               _gck_token_info_to_pkcs11                (GckTokenInfo *token_info,
                                                             CK_TOKEN_INFO_PTR info);

gboolean           _gck_token_info_match                    (GckTokenInfo *match,
                                                             GckTokenInfo *info);

/* ----------------------------------------------------------------------------
 * CALL
 */

typedef CK_RV (*GckPerformFunc) (gpointer call_data);
typedef gboolean (*GckCompleteFunc) (gpointer call_data, CK_RV result);

typedef struct _GckCall GckCall;

typedef struct _GckArguments {
	GckCall *call;

	/* For the call function to use */
	CK_FUNCTION_LIST_PTR pkcs11;
	CK_ULONG handle;

} GckArguments;

#define GCK_MECHANISM_EMPTY        { 0UL, NULL, 0 }

#define GCK_ARGUMENTS_INIT 	   { NULL, NULL, 0 }

#define GCK_TYPE_CALL             (_gck_call_get_type())
#define GCK_CALL(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), GCK_TYPE_CALL, GckCall))
#define GCK_CALL_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass), GCK_TYPE_CALL, GckCall))
#define GCK_IS_CALL(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj), GCK_TYPE_CALL))
#define GCK_IS_CALL_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass), GCK_TYPE_CALL))
#define GCK_CALL_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj), GCK_TYPE_CALL, GckCallClass))

typedef struct _GckCallClass GckCallClass;

GType              _gck_call_get_type                    (void) G_GNUC_CONST;

#define            _gck_call_arguments(call, type)       (type*)(_gck_call_get_arguments (GCK_CALL (call)))

gpointer           _gck_call_get_arguments               (GckCall *call);

void               _gck_call_uninitialize                (void);

gboolean           _gck_call_sync                        (gpointer object,
                                                           gpointer perform,
                                                           gpointer complete,
                                                           gpointer args,
                                                           GCancellable *cancellable,
                                                           GError **err);

gpointer           _gck_call_async_prep                  (gpointer object,
                                                           gpointer cb_object,
                                                           gpointer perform,
                                                           gpointer complete,
                                                           gsize args_size,
                                                           gpointer destroy_func);

GckCall*          _gck_call_async_ready                 (gpointer args,
                                                           GCancellable *cancellable,
                                                           GAsyncReadyCallback callback,
                                                           gpointer user_data);

void               _gck_call_async_go                    (GckCall *call);

void               _gck_call_async_ready_go              (gpointer args,
                                                           GCancellable *cancellable,
                                                           GAsyncReadyCallback callback,
                                                           gpointer user_data);

void               _gck_call_async_short                 (GckCall *call,
                                                           CK_RV rv);

gboolean           _gck_call_basic_finish                (GAsyncResult *result,
                                                           GError **err);

void               _gck_call_async_object                (GckCall *call,
                                                           gpointer object);

#endif /* GCK_PRIVATE_H_ */
