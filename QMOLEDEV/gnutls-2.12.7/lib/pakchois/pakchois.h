/* 
   pakchois PKCS#11 interface
   Copyright (C) 2008, Joe Orton <joe@manyfish.co.uk>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA

*/

/*
  This interface is directly derived from the scute.org PKCS#11
  cryptoki interface, which is:

   Copyright 2006, 2007 g10 Code GmbH
   Copyright 2006 Andreas Jellinghaus

   This file is free software; as a special exception the author gives
   unlimited permission to copy and/or distribute it, with or without
   modifications, as long as this notice is preserved.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY, to the extent permitted by law; without even
   the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.
*/

#ifndef PAKCHOIS_H
#define PAKCHOIS_H

#define CRYPTOKI_GNU

#include "pakchois11.h"

/* API version: major is bumped for any backwards-incompatible
 * changes. minor is bumped for any new interfaces.  Note that the API
 * is versioned independent of the project release version.  */
#define PAKCHOIS_API_MAJOR (0)
#define PAKCHOIS_API_MINOR (2)

/* API version history (note that API versions do not map directly to
   the project version!):

   0.1: Initial release
   0.2: Addition of pakchois_error()
        Concurrent access guarantee added for pakchois_module_load()
        Thread-safety guarantee added for pakchois_wait_for_slot_event()
*/

typedef struct pakchois_module_s pakchois_module_t;
typedef struct pakchois_session_s pakchois_session_t;

/* Load a PKCS#11 module by name (for example "opensc" or
 * "gnome-keyring").  Returns CKR_OK on success.  Any module of given
 * name may be safely loaded multiple times within an application; the
 * underlying PKCS#11 provider will be loaded only once. */
ck_rv_t pakchois_module_load (pakchois_module_t ** module, const char *name);

/* Load a PKCS#11 module by absolute file name (for example "/lib/opensc-pkcs.so" 
 * Returns CKR_OK on success.  Any module of given name may be safely loaded 
 * multiple times within an application; the underlying PKCS#11 provider will 
 * be loaded only once. */
ck_rv_t pakchois_module_load_abs (pakchois_module_t ** module,
                                  const char *name);

/* Load an NSS "softokn" which violates the PKCS#11 standard in
 * initialization, with given name (e.g. "softokn3").  The directory
 * in which the NSS database resides must be specified; the other
 * arguments may be NULL to use defaults. Returns CKR_OK on
 * success. */
ck_rv_t pakchois_module_nssload (pakchois_module_t ** module,
                                 const char *name,
                                 const char *directory,
                                 const char *cert_prefix,
                                 const char *key_prefix,
                                 const char *secmod_db);

ck_rv_t pakchois_module_nssload_abs (pakchois_module_t ** module,
                                     const char *name,
                                     const char *directory,
                                     const char *cert_prefix,
                                     const char *key_prefix,
                                     const char *secmod_db);

/* Destroy a PKCS#11 module. */
void pakchois_module_destroy (pakchois_module_t * module);

void pakchois_destructor (void);

/* Return the error string corresponding to the given return value.
 * Never returns NULL.  */
const char *pakchois_error (ck_rv_t rv);

/* All following interfaces model the PKCS#11 equivalents, without the
   camel-cased naming convention.  The PKCS#11 specification has
   detailed interface descriptions:
   
      http://www.rsa.com/rsalabs/node.asp?id=2133

   The differences between this interface and PKCS#11 are:
   
   1. some interfaces take a module pointer as first argument

   2. session handlers are represented as opaque objects

   3. the notify callback type has changed accordingly

   4. the C_Initialize, C_Finalize, and C_GetFunctionList interfaces
   are not exposed (these are called internally by
   pakchois_module_load and pakchois_module_destroy)

   5. pakchois_wait_for_slot_event() is thread-safe against other
   callers of pakchois_wait_for_slot_event(); the call to the
   underlying provider's WaitForSlotEvent function is protected by a
   mutex.

   6. pakchois_close_all_sessions() only closes sessions associated
   with the given module instance; any sessions opened by other users
   of the underlying provider are unaffected.

   If a module object is used concurrently from separate threads,
   undefined behaviour results.  If a session object is used
   concurrently from separate threads, undefined behavioure results.

*/
ck_rv_t pakchois_get_info (pakchois_module_t * module, struct ck_info *info);

ck_rv_t pakchois_get_slot_list (pakchois_module_t * module,
                                unsigned char token_present,
                                ck_slot_id_t * slot_list,
                                unsigned long *count);

ck_rv_t pakchois_get_slot_info (pakchois_module_t * module,
                                ck_slot_id_t slot_id,
                                struct ck_slot_info *info);

ck_rv_t pakchois_get_token_info (pakchois_module_t * module,
                                 ck_slot_id_t slot_id,
                                 struct ck_token_info *info);

ck_rv_t pakchois_wait_for_slot_event (pakchois_module_t * module,
                                      ck_flags_t flags, ck_slot_id_t * slot,
                                      void *reserved);

ck_rv_t pakchois_get_mechanism_list (pakchois_module_t * module,
                                     ck_slot_id_t slot_id,
                                     ck_mechanism_type_t * mechanism_list,
                                     unsigned long *count);

ck_rv_t pakchois_get_mechanism_info (pakchois_module_t * module,
                                     ck_slot_id_t slot_id,
                                     ck_mechanism_type_t type,
                                     struct ck_mechanism_info *info);

ck_rv_t pakchois_init_token (pakchois_module_t * module,
                             ck_slot_id_t slot_id, unsigned char *pin,
                             unsigned long pin_len, unsigned char *label);

ck_rv_t pakchois_init_pin (pakchois_session_t * session, unsigned char *pin,
                           unsigned long pin_len);

ck_rv_t pakchois_set_pin (pakchois_session_t * session,
                          unsigned char *old_pin, unsigned long old_len,
                          unsigned char *new_pin, unsigned long new_len);

typedef ck_rv_t (*pakchois_notify_t) (pakchois_session_t * sess,
                                      ck_notification_t event,
                                      void *application);

ck_rv_t pakchois_open_session (pakchois_module_t * module,
                               ck_slot_id_t slot_id, ck_flags_t flags,
                               void *application, pakchois_notify_t notify,
                               pakchois_session_t ** session);

ck_rv_t pakchois_close_session (pakchois_session_t * session);

ck_rv_t pakchois_close_all_sessions (pakchois_module_t * module,
                                     ck_slot_id_t slot_id);

ck_rv_t pakchois_get_session_info (pakchois_session_t * session,
                                   struct ck_session_info *info);
ck_rv_t pakchois_get_operation_state (pakchois_session_t * session,
                                      unsigned char *operation_state,
                                      unsigned long *operation_state_len);
ck_rv_t pakchois_set_operation_state (pakchois_session_t * session,
                                      unsigned char *operation_state,
                                      unsigned long operation_state_len,
                                      ck_object_handle_t encryption_key,
                                      ck_object_handle_t authentiation_key);

ck_rv_t pakchois_login (pakchois_session_t * session,
                        ck_user_type_t user_type, unsigned char *pin,
                        unsigned long pin_len);
ck_rv_t pakchois_logout (pakchois_session_t * session);

ck_rv_t pakchois_create_object (pakchois_session_t * session,
                                struct ck_attribute *templ,
                                unsigned long count,
                                ck_object_handle_t * object);
ck_rv_t pakchois_copy_object (pakchois_session_t * session,
                              ck_object_handle_t object,
                              struct ck_attribute *templ, unsigned long count,
                              ck_object_handle_t * new_object);
ck_rv_t pakchois_destroy_object (pakchois_session_t * session,
                                 ck_object_handle_t object);
ck_rv_t pakchois_get_object_size (pakchois_session_t * session,
                                  ck_object_handle_t object,
                                  unsigned long *size);

ck_rv_t pakchois_get_attribute_value (pakchois_session_t * session,
                                      ck_object_handle_t object,
                                      struct ck_attribute *templ,
                                      unsigned long count);
ck_rv_t pakchois_set_attribute_value (pakchois_session_t * session,
                                      ck_object_handle_t object,
                                      struct ck_attribute *templ,
                                      unsigned long count);
ck_rv_t pakchois_find_objects_init (pakchois_session_t * session,
                                    struct ck_attribute *templ,
                                    unsigned long count);
ck_rv_t pakchois_find_objects (pakchois_session_t * session,
                               ck_object_handle_t * object,
                               unsigned long max_object_count,
                               unsigned long *object_count);
ck_rv_t pakchois_find_objects_final (pakchois_session_t * session);

ck_rv_t pakchois_encrypt_init (pakchois_session_t * session,
                               struct ck_mechanism *mechanism,
                               ck_object_handle_t key);
ck_rv_t pakchois_encrypt (pakchois_session_t * session,
                          unsigned char *data, unsigned long data_len,
                          unsigned char *encrypted_data,
                          unsigned long *encrypted_data_len);
ck_rv_t pakchois_encrypt_update (pakchois_session_t * session,
                                 unsigned char *part, unsigned long part_len,
                                 unsigned char *encrypted_part,
                                 unsigned long *encrypted_part_len);
ck_rv_t pakchois_encrypt_final (pakchois_session_t * session,
                                unsigned char *last_encrypted_part,
                                unsigned long *last_encrypted_part_len);

ck_rv_t pakchois_decrypt_init (pakchois_session_t * session,
                               struct ck_mechanism *mechanism,
                               ck_object_handle_t key);
ck_rv_t pakchois_decrypt (pakchois_session_t * session,
                          unsigned char *encrypted_data,
                          unsigned long encrypted_data_len,
                          unsigned char *data, unsigned long *data_len);
ck_rv_t pakchois_decrypt_update (pakchois_session_t * session,
                                 unsigned char *encrypted_part,
                                 unsigned long encrypted_part_len,
                                 unsigned char *part,
                                 unsigned long *part_len);
ck_rv_t pakchois_decrypt_final (pakchois_session_t * session,
                                unsigned char *last_part,
                                unsigned long *last_part_len);
ck_rv_t pakchois_digest_init (pakchois_session_t * session,
                              struct ck_mechanism *mechanism);
ck_rv_t pakchois_digest (pakchois_session_t * session, unsigned char *data,
                         unsigned long data_len, unsigned char *digest,
                         unsigned long *digest_len);
ck_rv_t pakchois_digest_update (pakchois_session_t * session,
                                unsigned char *part, unsigned long part_len);
ck_rv_t pakchois_digest_key (pakchois_session_t * session,
                             ck_object_handle_t key);
ck_rv_t pakchois_digest_final (pakchois_session_t * session,
                               unsigned char *digest,
                               unsigned long *digest_len);

ck_rv_t pakchois_sign_init (pakchois_session_t * session,
                            struct ck_mechanism *mechanism,
                            ck_object_handle_t key);
ck_rv_t pakchois_sign (pakchois_session_t * session, unsigned char *data,
                       unsigned long data_len, unsigned char *signature,
                       unsigned long *signature_len);
ck_rv_t pakchois_sign_update (pakchois_session_t * session,
                              unsigned char *part, unsigned long part_len);
ck_rv_t pakchois_sign_final (pakchois_session_t * session,
                             unsigned char *signature,
                             unsigned long *signature_len);
ck_rv_t pakchois_sign_recover_init (pakchois_session_t * session,
                                    struct ck_mechanism *mechanism,
                                    ck_object_handle_t key);
ck_rv_t pakchois_sign_recover (pakchois_session_t * session,
                               unsigned char *data, unsigned long data_len,
                               unsigned char *signature,
                               unsigned long *signature_len);

ck_rv_t pakchois_verify_init (pakchois_session_t * session,
                              struct ck_mechanism *mechanism,
                              ck_object_handle_t key);
ck_rv_t pakchois_verify (pakchois_session_t * session, unsigned char *data,
                         unsigned long data_len, unsigned char *signature,
                         unsigned long signature_len);
ck_rv_t pakchois_verify_update (pakchois_session_t * session,
                                unsigned char *part, unsigned long part_len);
ck_rv_t pakchois_verify_final (pakchois_session_t * session,
                               unsigned char *signature,
                               unsigned long signature_len);
ck_rv_t pakchois_verify_recover_init (pakchois_session_t * session,
                                      struct ck_mechanism *mechanism,
                                      ck_object_handle_t key);
ck_rv_t pakchois_verify_recover (pakchois_session_t * session,
                                 unsigned char *signature,
                                 unsigned long signature_len,
                                 unsigned char *data,
                                 unsigned long *data_len);

ck_rv_t pakchois_digest_encrypt_update (pakchois_session_t * session,
                                        unsigned char *part,
                                        unsigned long part_len,
                                        unsigned char *encrypted_part,
                                        unsigned long *encrypted_part_len);
ck_rv_t pakchois_decrypt_digest_update (pakchois_session_t * session,
                                        unsigned char *encrypted_part,
                                        unsigned long encrypted_part_len,
                                        unsigned char *part,
                                        unsigned long *part_len);
ck_rv_t pakchois_sign_encrypt_update (pakchois_session_t * session,
                                      unsigned char *part,
                                      unsigned long part_len,
                                      unsigned char *encrypted_part,
                                      unsigned long *encrypted_part_len);
ck_rv_t pakchois_decrypt_verify_update (pakchois_session_t * session,
                                        unsigned char *encrypted_part,
                                        unsigned long encrypted_part_len,
                                        unsigned char *part,
                                        unsigned long *part_len);

ck_rv_t pakchois_generate_key (pakchois_session_t * session,
                               struct ck_mechanism *mechanism,
                               struct ck_attribute *templ,
                               unsigned long count, ck_object_handle_t * key);
ck_rv_t pakchois_generate_key_pair (pakchois_session_t * session,
                                    struct ck_mechanism *mechanism,
                                    struct ck_attribute *public_key_template,
                                    unsigned long public_key_attribute_count,
                                    struct ck_attribute *private_key_template,
                                    unsigned long private_key_attribute_count,
                                    ck_object_handle_t * public_key,
                                    ck_object_handle_t * private_key);

ck_rv_t pakchois_wrap_key (pakchois_session_t * session,
                           struct ck_mechanism *mechanism,
                           ck_object_handle_t wrapping_key,
                           ck_object_handle_t key, unsigned char *wrapped_key,
                           unsigned long *wrapped_key_len);
ck_rv_t pakchois_unwrap_key (pakchois_session_t * session,
                             struct ck_mechanism *mechanism,
                             ck_object_handle_t unwrapping_key,
                             unsigned char *wrapped_key,
                             unsigned long wrapped_key_len,
                             struct ck_attribute *templ,
                             unsigned long attribute_count,
                             ck_object_handle_t * key);
ck_rv_t pakchois_derive_key (pakchois_session_t * session,
                             struct ck_mechanism *mechanism,
                             ck_object_handle_t base_key,
                             struct ck_attribute *templ,
                             unsigned long attribute_count,
                             ck_object_handle_t * key);

ck_rv_t pakchois_seed_random (pakchois_session_t * session,
                              unsigned char *seed, unsigned long seed_len);
ck_rv_t pakchois_generate_random (pakchois_session_t * session,
                                  unsigned char *random_data,
                                  unsigned long random_len);

#endif /* PAKCHOIS_H */
