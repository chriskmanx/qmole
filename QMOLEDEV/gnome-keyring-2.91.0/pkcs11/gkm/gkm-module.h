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

#ifndef __GKM_MODULE_H__
#define __GKM_MODULE_H__

#include <glib-object.h>

#include "pkcs11/pkcs11.h"

#include "gkm-factory.h"
#include "gkm-types.h"

#define GKM_TYPE_MODULE               (gkm_module_get_type ())
#define GKM_MODULE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_MODULE, GkmModule))
#define GKM_MODULE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_MODULE, GkmModuleClass))
#define GKM_IS_MODULE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_MODULE))
#define GKM_IS_MODULE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_MODULE))
#define GKM_MODULE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_MODULE, GkmModuleClass))

typedef struct _GkmModuleClass GkmModuleClass;
typedef struct _GkmModulePrivate GkmModulePrivate;

struct _GkmModule {
	GObject parent;
	CK_FUNCTION_LIST pkcs11_funcs;
	GkmModulePrivate *pv;
};

struct _GkmModuleClass {
	GObjectClass parent_class;

	/* virtual methods */

	void (*parse_argument) (GkmModule *self, const gchar *name, const gchar *value);

	const CK_SLOT_INFO* (*get_slot_info) (GkmModule *self);

	const CK_TOKEN_INFO* (*get_token_info) (GkmModule *self);

	CK_RV (*refresh_token) (GkmModule *self);

	void (*add_token_object) (GkmModule *self, GkmTransaction *transaction, GkmObject *object);

	void (*store_token_object) (GkmModule *self, GkmTransaction *transaction, GkmObject *object);

	void (*remove_token_object) (GkmModule *self, GkmTransaction *transaction, GkmObject *object);

	CK_RV (*login_change) (GkmModule *self, CK_SLOT_ID slot_id,
	                       CK_UTF8CHAR_PTR old_pin, CK_ULONG n_old_pin,
	                       CK_UTF8CHAR_PTR new_pin, CK_ULONG n_new_pin);

	CK_RV (*login_user) (GkmModule *self, CK_SLOT_ID slot_id,
	                     CK_UTF8CHAR_PTR pin, CK_ULONG n_pin);

	CK_RV (*logout_user) (GkmModule *self, CK_SLOT_ID slot_id);

	CK_RV (*login_so) (GkmModule *self, CK_SLOT_ID slot_id,
	                   CK_UTF8CHAR_PTR pin, CK_ULONG n_pin);

	CK_RV (*logout_so) (GkmModule *self, CK_SLOT_ID slot_id);
};

/*
 * The PKCS#11 module is created by the following code in a header file:
 *
 *     #include "gkm-module.h"
 *     GKM_DECLARE_MODULE(my_module);
 *
 * And the following code in a source file:
 *
 *     #include "gkm-module-ep.h"
 *     GKM_DEFINE_MODULE(my_module, MY_TYPE_MODULE)
 *
 */

#define GKM_DECLARE_MODULE(prefix) \
	extern const CK_FUNCTION_LIST_PTR prefix ## _function_list

#define GKM_DEFINE_MODULE(prefix, type) \
	static GkmModule* gkm_module_instantiate (CK_C_INITIALIZE_ARGS_PTR args, GMutex* mutex) \
		{ return g_object_new ((type), "initialize-args", args, "mutex", mutex, NULL); } \
	const CK_FUNCTION_LIST_PTR prefix ## _function_list = &gkm_module_function_list;

GType                  gkm_module_get_type                        (void);

GkmManager*            gkm_module_get_manager                     (GkmModule *self);

gboolean               gkm_module_get_write_protected             (GkmModule *self);

CK_ULONG               gkm_module_next_handle                     (GkmModule *self);

GkmSession*            gkm_module_lookup_session                  (GkmModule *self,
                                                                   CK_SESSION_HANDLE handle);

CK_RV                  gkm_module_login_change                    (GkmModule *self,
                                                                   CK_SLOT_ID slot_id,
                                                                   CK_UTF8CHAR_PTR old_pin,
                                                                   CK_ULONG n_old_pin,
                                                                   CK_UTF8CHAR_PTR new_pin,
                                                                   CK_ULONG n_new_pin);

CK_RV                  gkm_module_login_user                      (GkmModule *self,
                                                                   CK_SLOT_ID slot_id,
                                                                   CK_UTF8CHAR_PTR pin,
                                                                   CK_ULONG n_pin);

CK_RV                  gkm_module_logout_user                     (GkmModule *self,
                                                                   CK_SLOT_ID slot_id);

CK_RV                  gkm_module_login_so                        (GkmModule *self,
                                                                   CK_SLOT_ID slot_id,
                                                                   CK_UTF8CHAR_PTR pin,
                                                                   CK_ULONG n_pin);

CK_RV                  gkm_module_logout_so                       (GkmModule *self,
                                                                   CK_SLOT_ID slot_id);

CK_RV                  gkm_module_refresh_token                   (GkmModule *self);

void                   gkm_module_add_token_object                (GkmModule *self,
                                                                   GkmTransaction *transaction,
                                                                   GkmObject *object);

void                   gkm_module_store_token_object              (GkmModule *self,
                                                                   GkmTransaction *transaction,
                                                                   GkmObject *object);

void                   gkm_module_remove_token_object             (GkmModule *self,
                                                                   GkmTransaction *transaction,
                                                                   GkmObject *object);

GkmFactory*            gkm_module_find_factory                    (GkmModule *self,
                                                                   CK_ATTRIBUTE_PTR attrs,
                                                                   CK_ULONG n_attrs);

void                   gkm_module_register_factory                (GkmModule *self,
                                                                   GkmFactory *factory);

CK_RV                  gkm_module_C_GetInfo                       (GkmModule *self,
                                                                   CK_INFO_PTR info);

CK_RV                  gkm_module_C_GetSlotList                   (GkmModule *self,
                                                                   CK_BBOOL token_present,
                                                                   CK_SLOT_ID_PTR slot_list,
                                                                   CK_ULONG_PTR count);

CK_RV                  gkm_module_C_GetSlotInfo                   (GkmModule *self,
                                                                   CK_SLOT_ID id,
                                                                   CK_SLOT_INFO_PTR info);

CK_RV                  gkm_module_C_GetTokenInfo                  (GkmModule *self,
                                                                   CK_SLOT_ID id,
                                                                   CK_TOKEN_INFO_PTR info);

CK_RV                  gkm_module_C_GetMechanismList              (GkmModule *self,
                                                                   CK_SLOT_ID id,
                                                                   CK_MECHANISM_TYPE_PTR mech_list,
                                                                   CK_ULONG_PTR count);

CK_RV                  gkm_module_C_GetMechanismInfo              (GkmModule *self,
                                                                   CK_SLOT_ID id,
                                                                   CK_MECHANISM_TYPE type,
                                                                   CK_MECHANISM_INFO_PTR info);

CK_RV                  gkm_module_C_InitToken                     (GkmModule *self,
                                                                   CK_SLOT_ID id,
                                                                   CK_UTF8CHAR_PTR pin,
                                                                   CK_ULONG pin_len,
                                                                   CK_UTF8CHAR_PTR label);

CK_RV                  gkm_module_C_OpenSession                   (GkmModule *self,
                                                                   CK_SLOT_ID id,
                                                                   CK_FLAGS flags,
                                                                   CK_VOID_PTR user_data,
                                                                   CK_NOTIFY callback,
                                                                   CK_SESSION_HANDLE_PTR session);

CK_RV                  gkm_module_C_CloseSession                  (GkmModule *self,
                                                                   CK_SESSION_HANDLE session);

CK_RV                  gkm_module_C_CloseAllSessions              (GkmModule *self,
                                                                   CK_SLOT_ID id);

CK_RV                  gkm_module_C_InitPIN                       (GkmModule* self,
                                                                   CK_SESSION_HANDLE session,
                                                                   CK_UTF8CHAR_PTR pin,
                                                                   CK_ULONG pin_len);

CK_RV                  gkm_module_C_SetPIN                        (GkmModule* self,
                                                                   CK_SESSION_HANDLE session,
                                                                   CK_UTF8CHAR_PTR old_pin,
                                                                   CK_ULONG old_pin_len,
                                                                   CK_UTF8CHAR_PTR new_pin,
                                                                   CK_ULONG new_pin_len);

CK_RV                  gkm_module_C_Login                         (GkmModule *self,
                                                                   CK_SESSION_HANDLE session,
                                                                   CK_USER_TYPE user_type,
                                                                   CK_UTF8CHAR_PTR pin,
                                                                   CK_ULONG pin_len);

CK_RV                  gkm_module_C_Logout                        (GkmModule *self,
                                                                   CK_SESSION_HANDLE session);

#endif /* __GKM_MODULE_H__ */
