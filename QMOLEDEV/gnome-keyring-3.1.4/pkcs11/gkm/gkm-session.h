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

#ifndef __GKM_SESSION_H__
#define __GKM_SESSION_H__

#include <glib-object.h>

#include "gkm-module.h"
#include "gkm-manager.h"

#define GKM_TYPE_SESSION               (gkm_session_get_type ())
#define GKM_SESSION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SESSION, GkmSession))
#define GKM_SESSION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SESSION, GkmSessionClass))
#define GKM_IS_SESSION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SESSION))
#define GKM_IS_SESSION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SESSION))
#define GKM_SESSION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SESSION, GkmSessionClass))

typedef struct _GkmSessionClass GkmSessionClass;
typedef struct _GkmSessionPrivate GkmSessionPrivate;

struct _GkmSession {
	GObject parent;
	GkmSessionPrivate *pv;
};

struct _GkmSessionClass {
	GObjectClass parent_class;

#if 0
	/* signals --------------------------------------------------------- */

	void (*signal) (GkmSession *session);
#endif
};

GType                    gkm_session_get_type                           (void);

GkmSession*              gkm_session_for_session_object                 (GkmObject *obj);

GkmSession*              gkm_session_for_session_object                 (GkmObject *obj);

CK_SESSION_HANDLE        gkm_session_get_handle                         (GkmSession *self);

CK_SLOT_ID               gkm_session_get_slot_id                        (GkmSession *self);

CK_ULONG                 gkm_session_get_apartment                      (GkmSession *self);

GkmModule*               gkm_session_get_module                         (GkmSession *self);

GkmManager*              gkm_session_get_manager                        (GkmSession *self);

gboolean                 gkm_session_get_read_only                      (GkmSession *self);

gulong                   gkm_session_get_logged_in                      (GkmSession *self);

void                     gkm_session_set_logged_in                      (GkmSession *self,
                                                                         gulong logged_in);

gpointer                 gkm_session_get_crypto_state                   (GkmSession *self);

void                     gkm_session_set_crypto_state                   (GkmSession *self,
                                                                         gpointer state,
                                                                         GDestroyNotify destroy);

GkmCredential*           gkm_session_get_credential                     (GkmSession *self);

CK_RV                    gkm_session_lookup_readable_object             (GkmSession *self,
                                                                         CK_OBJECT_HANDLE handle,
                                                                         GkmObject **result);

CK_RV                    gkm_session_lookup_writable_object             (GkmSession *self,
                                                                         CK_OBJECT_HANDLE handle,
                                                                         GkmObject **result);

CK_RV                    gkm_session_login_context_specific             (GkmSession *self,
                                                                         CK_UTF8CHAR_PTR pin,
                                                                         CK_ULONG n_pin);

void                     gkm_session_add_session_object                 (GkmSession *self,
                                                                         GkmTransaction *transaction,
                                                                         GkmObject *obj);

void                     gkm_session_destroy_session_object             (GkmSession *self,
                                                                         GkmTransaction *transaction,
                                                                         GkmObject *obj);

GkmObject*               gkm_session_create_object_for_factory          (GkmSession *self,
                                                                         GkmFactory *factory,
                                                                         GkmTransaction *transaction,
                                                                         CK_ATTRIBUTE_PTR attrs,
                                                                         CK_ULONG n_attrs);

GkmObject*               gkm_session_create_object_for_attributes       (GkmSession *self,
                                                                         GkmTransaction *transaction,
                                                                         CK_ATTRIBUTE_PTR attrs,
                                                                         CK_ULONG n_attrs);

void                     gkm_session_complete_object_creation           (GkmSession *self,
                                                                         GkmTransaction *transaction,
                                                                         GkmObject *object,
                                                                         gboolean add,
                                                                         CK_ATTRIBUTE_PTR attrs,
                                                                         CK_ULONG n_attrs);

CK_RV                    gkm_session_C_GetFunctionStatus                (GkmSession *self);

CK_RV                    gkm_session_C_CancelFunction                   (GkmSession *self);

CK_RV                    gkm_session_C_GetSessionInfo                   (GkmSession* self,
                                                                         CK_SESSION_INFO_PTR info);

CK_RV                    gkm_session_C_GetOperationState                (GkmSession* self,
                                                                         CK_BYTE_PTR operation_state,
                                                                         CK_ULONG_PTR operation_state_len);

CK_RV                    gkm_session_C_SetOperationState                (GkmSession* self,
                                                                         CK_BYTE_PTR operation_state,
                                                                         CK_ULONG operation_state_len,
                                                                         CK_OBJECT_HANDLE encryption_key,
                                                                         CK_OBJECT_HANDLE authentication_key);

CK_RV                    gkm_session_C_CreateObject                     (GkmSession* self,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count,
                                                                         CK_OBJECT_HANDLE_PTR new_object);

CK_RV                    gkm_session_C_CopyObject                       (GkmSession* self,
                                                                         CK_OBJECT_HANDLE object,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count,
                                                                         CK_OBJECT_HANDLE_PTR new_object);

CK_RV                    gkm_session_C_DestroyObject                    (GkmSession* self,
                                                                         CK_OBJECT_HANDLE object);

CK_RV                    gkm_session_C_GetObjectSize                    (GkmSession* self,
                                                                         CK_OBJECT_HANDLE object,
                                                                         CK_ULONG_PTR size);

CK_RV                    gkm_session_C_GetAttributeValue                (GkmSession* self,
                                                                         CK_OBJECT_HANDLE handle,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count);

CK_RV                    gkm_session_C_SetAttributeValue                (GkmSession* self,
                                                                         CK_OBJECT_HANDLE handle,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count);

CK_RV                    gkm_session_C_FindObjectsInit                  (GkmSession* self,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count);

CK_RV                    gkm_session_C_FindObjects                      (GkmSession* self,
                                                                         CK_OBJECT_HANDLE_PTR objects,
                                                                         CK_ULONG max_count,
                                                                         CK_ULONG_PTR count);

CK_RV                    gkm_session_C_FindObjectsFinal                 (GkmSession* self);

CK_RV                    gkm_session_C_EncryptInit                      (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_Encrypt                          (GkmSession *self,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG data_len,
                                                                         CK_BYTE_PTR encrypted_data,
                                                                         CK_ULONG_PTR encrypted_data_len);

CK_RV                    gkm_session_C_EncryptUpdate                    (GkmSession *self,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG part_len,
                                                                         CK_BYTE_PTR encrypted_part,
                                                                         CK_ULONG_PTR encrypted_part_len);

CK_RV                    gkm_session_C_EncryptFinal                     (GkmSession *self,
                                                                         CK_BYTE_PTR last_part,
                                                                         CK_ULONG_PTR last_part_len);

CK_RV                    gkm_session_C_DecryptInit                      (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_Decrypt                          (GkmSession *self,
                                                                         CK_BYTE_PTR enc_data,
                                                                         CK_ULONG enc_data_len,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG_PTR data_len);

CK_RV                    gkm_session_C_DecryptUpdate                    (GkmSession *self,
                                                                         CK_BYTE_PTR enc_part,
                                                                         CK_ULONG enc_part_len,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG_PTR part_len);

CK_RV                    gkm_session_C_DecryptFinal                     (GkmSession *self,
                                                                         CK_BYTE_PTR last_part,
                                                                         CK_ULONG_PTR last_part_len);

CK_RV                    gkm_session_C_DigestInit                       (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism);

CK_RV                    gkm_session_C_Digest                           (GkmSession *self,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG data_len,
                                                                         CK_BYTE_PTR digest,
                                                                         CK_ULONG_PTR digest_len);

CK_RV                    gkm_session_C_DigestUpdate                     (GkmSession *self,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG part_len);

CK_RV                    gkm_session_C_DigestKey                        (GkmSession *self,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_DigestFinal                      (GkmSession *self,
                                                                         CK_BYTE_PTR digest,
                                                                         CK_ULONG_PTR digest_len);

CK_RV                    gkm_session_C_SignInit                         (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_Sign                             (GkmSession *self,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG data_len,
                                                                         CK_BYTE_PTR signature,
                                                                         CK_ULONG_PTR signature_len);

CK_RV                    gkm_session_C_SignUpdate                       (GkmSession *self,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG part_len);

CK_RV                    gkm_session_C_SignFinal                        (GkmSession *self,
                                                                         CK_BYTE_PTR signature,
                                                                         CK_ULONG_PTR signature_len);

CK_RV                    gkm_session_C_SignRecoverInit                  (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_SignRecover                      (GkmSession *self,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG data_len,
                                                                         CK_BYTE_PTR signature,
                                                                         CK_ULONG_PTR signature_len);

CK_RV                    gkm_session_C_VerifyInit                       (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_Verify                           (GkmSession *self,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG data_len,
                                                                         CK_BYTE_PTR signature,
                                                                         CK_ULONG signature_len);

CK_RV                    gkm_session_C_VerifyUpdate                     (GkmSession *self,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG part_len);

CK_RV                    gkm_session_C_VerifyFinal                      (GkmSession *self,
                                                                         CK_BYTE_PTR signature,
                                                                         CK_ULONG signature_len);

CK_RV                    gkm_session_C_VerifyRecoverInit                (GkmSession *self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE key);

CK_RV                    gkm_session_C_VerifyRecover                    (GkmSession *self,
                                                                         CK_BYTE_PTR signature,
                                                                         CK_ULONG signature_len,
                                                                         CK_BYTE_PTR data,
                                                                         CK_ULONG_PTR data_len);

CK_RV                    gkm_session_C_DigestEncryptUpdate              (GkmSession *self,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG part_len,
                                                                         CK_BYTE_PTR enc_part,
                                                                         CK_ULONG_PTR enc_part_len);

CK_RV                    gkm_session_C_DecryptDigestUpdate              (GkmSession *self,
                                                                         CK_BYTE_PTR enc_part,
                                                                         CK_ULONG enc_part_len,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG_PTR part_len);

CK_RV                    gkm_session_C_SignEncryptUpdate                (GkmSession *self,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG part_len,
                                                                         CK_BYTE_PTR enc_part,
                                                                         CK_ULONG_PTR enc_part_len);

CK_RV                    gkm_session_C_DecryptVerifyUpdate              (GkmSession *self,
                                                                         CK_BYTE_PTR enc_part,
                                                                         CK_ULONG enc_part_len,
                                                                         CK_BYTE_PTR part,
                                                                         CK_ULONG_PTR part_len);

CK_RV                    gkm_session_C_GenerateKey                      (GkmSession* self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count,
                                                                         CK_OBJECT_HANDLE_PTR key);

CK_RV                    gkm_session_C_GenerateKeyPair                  (GkmSession* self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_ATTRIBUTE_PTR pub_template,
                                                                         CK_ULONG pub_count,
                                                                         CK_ATTRIBUTE_PTR priv_template,
                                                                         CK_ULONG priv_count,
                                                                         CK_OBJECT_HANDLE_PTR pub_key,
                                                                         CK_OBJECT_HANDLE_PTR priv_key);

CK_RV                    gkm_session_C_WrapKey                          (GkmSession* self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE wrapping_key,
                                                                         CK_OBJECT_HANDLE key,
                                                                         CK_BYTE_PTR wrapped_key,
                                                                         CK_ULONG_PTR wrapped_key_len);

CK_RV                    gkm_session_C_UnwrapKey                        (GkmSession* self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE unwrapping_key,
                                                                         CK_BYTE_PTR wrapped_key,
                                                                         CK_ULONG wrapped_key_len,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count,
                                                                         CK_OBJECT_HANDLE_PTR key);

CK_RV                    gkm_session_C_DeriveKey                        (GkmSession* self,
                                                                         CK_MECHANISM_PTR mechanism,
                                                                         CK_OBJECT_HANDLE base_key,
                                                                         CK_ATTRIBUTE_PTR template,
                                                                         CK_ULONG count,
                                                                         CK_OBJECT_HANDLE_PTR key);

CK_RV                    gkm_session_C_SeedRandom                       (GkmSession* self,
                                                                         CK_BYTE_PTR seed,
                                                                         CK_ULONG seed_len);

CK_RV                    gkm_session_C_GenerateRandom                   (GkmSession* self,
                                                                         CK_BYTE_PTR random_data,
                                                                         CK_ULONG random_len);

#endif /* __GKM_SESSION_H__ */
