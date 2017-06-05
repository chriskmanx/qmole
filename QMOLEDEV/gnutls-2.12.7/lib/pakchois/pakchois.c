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
  The interface is directly derived from the scute.org PKCS#11
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

#include "config.h"

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "../locks.h"
#include "../system.h"
#include "dlopen.h"

#ifdef HAVE_WORDEXP
#include <wordexp.h>
#endif
#include "pakchois.h"

struct provider
{
  dev_t dev;
  ino_t ino;
  char *name;
  void *handle;
  void *mutex;
  const struct ck_function_list *fns;
  unsigned int refcount;
  unsigned int finalize:1; /* whether to finalize this one */
  struct provider *next, **prevref;
  void *reserved;
};

struct pakchois_module_s
{
  struct slot *slots;
  struct provider *provider;
};

static void *provider_mutex = NULL;

/* List of loaded providers; any modification to the list or any
 * individual module must performed whilst holding this mutex. */
static struct provider *provider_list;

struct pakchois_session_s
{
  pakchois_module_t *module;
  ck_session_handle_t id;
  pakchois_notify_t notify;
  void *notify_data;
  /* Doubly-linked list.  Either prevref = &previous->next or else
   * prevref = &slot->sessions for the list head. */
  pakchois_session_t **prevref;
  pakchois_session_t *next;
};

struct slot
{
  ck_slot_id_t id;
  pakchois_session_t *sessions;
  struct slot *next;
};

#define DIR_DELIMITER '/'

static char *
pkcs11ize (const char *name)
{
  int len;
  char *oname;
  char *base;
  char *suffix;

  oname = strdup (name);
  if (oname == NULL)
    {
      return NULL;
    }

  /* basename has too many ifs to use */
  base = strrchr (oname, DIR_DELIMITER);
  if (base == NULL)
    {
      base = oname;
    }
  else
    {
      base++;
    }

  suffix = strchr (base, '.');
  if (suffix != NULL)
    {
      if (strncmp (suffix, ".so", 3) == 0)
        {
          suffix[0] = 0;        /* null terminate before . */
        }
    }

  /* check and remove for -p11 or -pkcs11 */
  suffix = base;
  while ((suffix = strchr (suffix, '-')) != NULL)
    {
      if (strncasecmp (suffix, "-p11", 4) == 0 ||
          strncasecmp (suffix, "-pkcs11", 7) == 0)
        {
          suffix[0] = 0;
          break;
        }
      suffix++;
    }

  len = strlen (base);

  memmove (oname, base, len);
  oname[len] = 0;

  return oname;
}

static const char *suffix_prefixes[][2] = {
  {"lib", "pk11.so"},
  {"", "-pkcs11.so"},
  {"", ".so"},
  {"lib", ".so"},
  {NULL, NULL}
};

#define CALL(name, args) (mod->provider->fns->C_ ## name) args
#define CALLS(name, args) (sess->module->provider->fns->C_ ## name) args
#define CALLS1(n, a) CALLS(n, (sess->id, a))
#define CALLS2(n, a, b) CALLS(n, (sess->id, a, b))
#define CALLS3(n, a, b, c) CALLS(n, (sess->id, a, b, c))
#define CALLS4(n, a, b, c, d) CALLS(n, (sess->id, a, b, c, d))
#define CALLS5(n, a, b, c, d, e) CALLS(n, (sess->id, a, b, c, d, e))
#define CALLS7(n, a, b, c, d, e, f, g) CALLS(n, (sess->id, a, b, c, d, e, f, g))

#ifndef PAKCHOIS_MODPATH
#define PAKCHOIS_MODPATH "/lib:/usr/lib"
#endif

/* Returns an allocated name of the real module as well
 * as it's inode and device numbers.
 */
static char *
find_pkcs11_module_name (const char *hint, dev_t * dev, ino_t * ino)
{
  char module_path[] = PAKCHOIS_MODPATH;
  char *next = module_path;
  struct stat st;

  while (next)
    {
      char *dir = next, *sep = strchr (next, ':');
      unsigned i;

      if (sep)
        {
          *sep++ = '\0';
          next = sep;
        }
      else
        {
          next = NULL;
        }

      for (i = 0; suffix_prefixes[i][0]; i++)
        {
          char path[PATH_MAX];

          snprintf (path, sizeof path, "%s/%s%s%s", dir,
                    suffix_prefixes[i][0], hint, suffix_prefixes[i][1]);

          if (stat (path, &st) < 0)
            continue;

          *dev = st.st_dev;
          *ino = st.st_ino;

          return strdup (path);
        }
    }

  return NULL;
}

/* Expands the given filename and returns an allocated
 * string, if the expanded file exists. In that case
 * dev and ino are filled in as well.
 */
static char *
find_real_module_name (const char *name, dev_t * dev, ino_t * ino)
{
  char *exname = NULL;
  struct stat st;
#ifdef HAVE_WORDEXP
  int len;
  wordexp_t we;

  len = wordexp (name, &we, 0);
  if (len == 0)
    {                           /* success */
      if (we.we_wordc > 0)
        {                       /* we care about the 1st */
          exname = strdup (we.we_wordv[0]);
        }
      wordfree (&we);
    }
#endif

  if (exname == NULL)
    exname = strdup (name);

  /* find file information */
  if (exname != NULL)
    {
      if (stat (exname, &st) >= 0)
        {
          *dev = st.st_dev;
          *ino = st.st_ino;
        }
      else
        {
          free (exname);
          return NULL;
        }
    }

  return exname;
}

static struct provider *
find_provider (dev_t dev, ino_t ino)
{
  struct provider *p;

  for (p = provider_list; p; p = p->next)
    {
      if (dev == p->dev && ino == p->ino)
        {
          return p;
        }
    }

  return NULL;
}

/* The provider list must be locked when calling it
 */
static ck_rv_t
load_pkcs11_module (struct provider **provider,
                    const char *name, dev_t dev, ino_t ino, void *reserved)
{
  struct provider *prov;
  CK_C_GetFunctionList gfl;
  struct ck_c_initialize_args args;
  struct ck_function_list *fns;
  void *h;
  ck_rv_t rv;

  /* try the plain name first */
  h = dlopen (name, RTLD_LOCAL | RTLD_NOW);
  if (h == NULL)
    {
      return CKR_GENERAL_ERROR;
    }

  gfl = dlsym (h, "C_GetFunctionList");
  if (!gfl)
    {
      rv = CKR_GENERAL_ERROR;
      goto fail_dso;
    }

  prov = malloc (sizeof *prov);
  if (prov == NULL)
    {
      rv = CKR_HOST_MEMORY;
      goto fail_dso;
    }

  if (gnutls_mutex_init (&prov->mutex))
    {
      rv = CKR_CANT_LOCK;
      goto fail_ctx;
    }

  rv = gfl (&fns);
  if (rv != CKR_OK)
    {
      goto fail_ctx;
    }

  prov->dev = dev;
  prov->ino = ino;
  prov->name = pkcs11ize (name);
  prov->handle = h;
  prov->fns = fns;
  prov->refcount = 1;
  prov->reserved = reserved;

  /* Require OS locking, the only sane option. */
  memset (&args, 0, sizeof args);
  args.flags = CKF_OS_LOCKING_OK;
  args.reserved = reserved;

  rv = fns->C_Initialize (&args);
  if (rv != CKR_OK && rv != CKR_CRYPTOKI_ALREADY_INITIALIZED)
    {
      goto fail_ctx;
    }

  /* no need to finalize if someone else has 
   * initialized the library before us.
   */
  if (rv == CKR_CRYPTOKI_ALREADY_INITIALIZED)
    prov->finalize = 0;
  else
    prov->finalize = 1;

  prov->next = provider_list;
  prov->prevref = &provider_list;
  if (prov->next)
    {
      prov->next->prevref = &prov->next;
    }
  provider_list = prov;

  *provider = prov;
  return CKR_OK;

fail_ctx:
  free (prov);
fail_dso:
  dlclose (h);

  return rv;
}

/* Will load a provider using the given name. If real_name is zero
 * name is used as a hint to find library otherwise it is used as
 * absolute name.
 */
static ck_rv_t
load_provider (struct provider **provider, const char *name,
               void *reserved, int real_name)
{
  ck_rv_t rv;
  char *cname = NULL;
  dev_t dev;
  ino_t ino;

  if (gnutls_mutex_lock (&provider_mutex) != 0)
    {
      return CKR_CANT_LOCK;
    }

  if (real_name)
    {
      cname = find_real_module_name (name, &dev, &ino);
    }
  else
    {
      cname = find_pkcs11_module_name (name, &dev, &ino);
    }

  if (cname == NULL)
    {
      rv = CKR_ARGUMENTS_BAD;
      goto fail_locked;
    }

  *provider = find_provider (dev, ino);
  if (*provider)
    {
      (*provider)->refcount++;
      free (cname);
      gnutls_mutex_unlock (&provider_mutex);
      return CKR_OK;
    }

  rv = load_pkcs11_module (provider, cname, dev, ino, reserved);
  if (rv != CKR_OK)
    {
      goto fail_ndup;
    }

  rv = CKR_OK;

fail_ndup:
  free (cname);
fail_locked:
  gnutls_mutex_unlock (&provider_mutex);
  return rv;
}

static void
providers_reinit (void)
{
  struct ck_c_initialize_args args;
  ck_rv_t rv;
  struct provider *p;

  assert (gnutls_mutex_lock (&provider_mutex) == 0);

  memset (&args, 0, sizeof args);
  args.flags = CKF_OS_LOCKING_OK;

  for (p = provider_list; p; p = p->next)
    {
      args.reserved = p->reserved;
      rv = p->fns->C_Initialize (&args);
      assert (rv == CKR_OK);    /* what can we do? */
    }

  gnutls_mutex_unlock (&provider_mutex);
}

static ck_rv_t
load_module (pakchois_module_t ** module, const char *name,
             void *reserved, unsigned int real_name)
{
  ck_rv_t rv;
  pakchois_module_t *pm = malloc (sizeof *pm);
  static int forkinit = 0;

  if (!pm)
    {
      return CKR_HOST_MEMORY;
    }

  if (provider_mutex == NULL)
    {
      gnutls_mutex_init (&provider_mutex);
    }

  assert (gnutls_mutex_lock (&provider_mutex) == 0);

  if (forkinit == 0)
    {
      _gnutls_atfork (NULL, NULL, providers_reinit);
      forkinit++;
    }

  gnutls_mutex_unlock (&provider_mutex);

  rv = load_provider (&pm->provider, name, reserved, real_name);
  if (rv)
    {
      return rv;
    }

  *module = pm;
  pm->slots = NULL;

  return CKR_OK;
}

ck_rv_t
pakchois_module_load (pakchois_module_t ** module, const char *name)
{
  return load_module (module, name, NULL, 0);
}

ck_rv_t
pakchois_module_load_abs (pakchois_module_t ** module, const char *name)
{
  return load_module (module, name, NULL, 1);
}

ck_rv_t
pakchois_module_nssload (pakchois_module_t ** module,
                         const char *name,
                         const char *directory,
                         const char *cert_prefix,
                         const char *key_prefix, const char *secmod_db)
{
  char buf[256];

  snprintf (buf, sizeof buf,
            "configdir='%s' certPrefix='%s' keyPrefix='%s' secmod='%s'",
            directory, cert_prefix ? cert_prefix : "",
            key_prefix ? key_prefix : "",
            secmod_db ? secmod_db : "secmod.db");

  return load_module (module, name, buf, 0);
}

ck_rv_t
pakchois_module_nssload_abs (pakchois_module_t ** module,
                             const char *name,
                             const char *directory,
                             const char *cert_prefix,
                             const char *key_prefix, const char *secmod_db)
{
  char buf[256];

  snprintf (buf, sizeof buf,
            "configdir='%s' certPrefix='%s' keyPrefix='%s' secmod='%s'",
            directory, cert_prefix ? cert_prefix : "",
            key_prefix ? key_prefix : "",
            secmod_db ? secmod_db : "secmod.db");

  return load_module (module, name, buf, 1);
}

/* Unreference a provider structure and destoy if, if necessary.  Must
 * be called WIHTOUT the provider mutex held.  */
static void
provider_unref (struct provider *prov)
{
  assert (gnutls_mutex_lock (&provider_mutex) == 0);

  if (--prov->refcount == 0)
    {
      if (prov->finalize)
        prov->fns->C_Finalize (NULL);
      dlclose (prov->handle);
      *prov->prevref = prov->next;
      if (prov->next)
        {
          prov->next->prevref = prov->prevref;
        }
      free (prov->name);
      free (prov);
    }
  gnutls_mutex_unlock (&provider_mutex);
}

void
pakchois_module_destroy (pakchois_module_t * mod)
{
  provider_unref (mod->provider);

  while (mod->slots)
    {
      struct slot *slot = mod->slots;
      pakchois_close_all_sessions (mod, slot->id);
      mod->slots = slot->next;
      free (slot);
    }

  free (mod);
}

void pakchois_destructor (void)
{
  if (provider_mutex != NULL)
    {
      gnutls_mutex_deinit (&provider_mutex);
      provider_mutex = NULL;
    }
}

ck_rv_t
pakchois_get_info (pakchois_module_t * mod, struct ck_info *info)
{
  return CALL (GetInfo, (info));
}

ck_rv_t
pakchois_get_slot_list (pakchois_module_t * mod,
                        unsigned char token_present,
                        ck_slot_id_t * slot_list, unsigned long *count)
{
  return CALL (GetSlotList, (token_present, slot_list, count));
}

ck_rv_t
pakchois_get_slot_info (pakchois_module_t * mod,
                        ck_slot_id_t slot_id, struct ck_slot_info * info)
{
  return CALL (GetSlotInfo, (slot_id, info));
}

ck_rv_t
pakchois_get_token_info (pakchois_module_t * mod,
                         ck_slot_id_t slot_id, struct ck_token_info * info)
{
  return CALL (GetTokenInfo, (slot_id, info));
}

ck_rv_t
pakchois_wait_for_slot_event (pakchois_module_t * mod,
                              ck_flags_t flags, ck_slot_id_t * slot,
                              void *reserved)
{
  ck_rv_t rv;

  if (gnutls_mutex_lock (&mod->provider->mutex))
    {
      return CKR_CANT_LOCK;
    }

  rv = CALL (WaitForSlotEvent, (flags, slot, reserved));
  gnutls_mutex_unlock (&mod->provider->mutex);
  return rv;
}

ck_rv_t
pakchois_get_mechanism_list (pakchois_module_t * mod,
                             ck_slot_id_t slot_id,
                             ck_mechanism_type_t * mechanism_list,
                             unsigned long *count)
{
  return CALL (GetMechanismList, (slot_id, mechanism_list, count));
}

ck_rv_t
pakchois_get_mechanism_info (pakchois_module_t * mod,
                             ck_slot_id_t slot_id,
                             ck_mechanism_type_t type,
                             struct ck_mechanism_info * info)
{
  return CALL (GetMechanismInfo, (slot_id, type, info));
}

ck_rv_t
pakchois_init_token (pakchois_module_t * mod,
                     ck_slot_id_t slot_id, unsigned char *pin,
                     unsigned long pin_len, unsigned char *label)
{
  return CALL (InitToken, (slot_id, pin, pin_len, label));
}

ck_rv_t
pakchois_init_pin (pakchois_session_t * sess, unsigned char *pin,
                   unsigned long pin_len)
{
  return CALLS2 (InitPIN, pin, pin_len);
}

ck_rv_t
pakchois_set_pin (pakchois_session_t * sess, unsigned char *old_pin,
                  unsigned long old_len, unsigned char *new_pin,
                  unsigned long new_len)
{
  return CALLS4 (SetPIN, old_pin, old_len, new_pin, new_len);
}

static ck_rv_t
notify_thunk (ck_session_handle_t session,
              ck_notification_t event, void *application)
{
  pakchois_session_t *sess = application;

  return sess->notify (sess, event, sess->notify_data);
}

static struct slot *
find_slot (pakchois_module_t * mod, ck_slot_id_t id)
{
  struct slot *slot;

  for (slot = mod->slots; slot; slot = slot->next)
    if (slot->id == id)
      return slot;

  return NULL;
}

static struct slot *
find_or_create_slot (pakchois_module_t * mod, ck_slot_id_t id)
{
  struct slot *slot = find_slot (mod, id);

  if (slot)
    {
      return slot;
    }

  slot = malloc (sizeof *slot);
  if (!slot)
    {
      return NULL;
    }

  slot->id = id;
  slot->sessions = NULL;
  slot->next = mod->slots;
  mod->slots = slot;

  return slot;
}

static ck_rv_t
insert_session (pakchois_module_t * mod,
                pakchois_session_t * session, ck_slot_id_t id)
{
  struct slot *slot = find_or_create_slot (mod, id);

  if (!slot)
    {
      return CKR_HOST_MEMORY;
    }

  session->prevref = &slot->sessions;
  session->next = slot->sessions;
  if (session->next)
    {
      session->next->prevref = session->prevref;
    }
  slot->sessions = session;

  return CKR_OK;
}

ck_rv_t
pakchois_open_session (pakchois_module_t * mod,
                       ck_slot_id_t slot_id, ck_flags_t flags,
                       void *application, pakchois_notify_t notify,
                       pakchois_session_t ** session)
{
  ck_session_handle_t sh;
  pakchois_session_t *sess;
  ck_rv_t rv;

  sess = calloc (1, sizeof *sess);
  if (sess == NULL)
    {
      return CKR_HOST_MEMORY;
    }

  rv = CALL (OpenSession, (slot_id, flags, sess, notify_thunk, &sh));
  if (rv != CKR_OK)
    {
      free (sess);
      return rv;
    }

  *session = sess;
  sess->module = mod;
  sess->id = sh;

  return insert_session (mod, sess, slot_id);
}

ck_rv_t
pakchois_close_session (pakchois_session_t * sess)
{
  /* PKCS#11 says that all bets are off on failure, so destroy the
   * session object and just return the error code. */
  ck_rv_t rv = CALLS (CloseSession, (sess->id));
  *sess->prevref = sess->next;
  if (sess->next)
    {
      sess->next->prevref = sess->prevref;
    }
  free (sess);
  return rv;
}

ck_rv_t
pakchois_close_all_sessions (pakchois_module_t * mod, ck_slot_id_t slot_id)
{
  struct slot *slot;
  ck_rv_t rv, frv = CKR_OK;

  slot = find_slot (mod, slot_id);

  if (!slot)
    {
      return CKR_SLOT_ID_INVALID;
    }

  while (slot->sessions)
    {
      rv = pakchois_close_session (slot->sessions);
      if (rv != CKR_OK)
        {
          frv = rv;
        }
      slot = slot->next;
    }

  return frv;
}

ck_rv_t
pakchois_get_session_info (pakchois_session_t * sess,
                           struct ck_session_info * info)
{
  return CALLS1 (GetSessionInfo, info);
}

ck_rv_t
pakchois_get_operation_state (pakchois_session_t * sess,
                              unsigned char *operation_state,
                              unsigned long *operation_state_len)
{
  return CALLS2 (GetOperationState, operation_state, operation_state_len);
}

ck_rv_t
pakchois_set_operation_state (pakchois_session_t * sess,
                              unsigned char *operation_state,
                              unsigned long operation_state_len,
                              ck_object_handle_t encryption_key,
                              ck_object_handle_t authentiation_key)
{
  return CALLS4 (SetOperationState, operation_state,
                 operation_state_len, encryption_key, authentiation_key);
}

ck_rv_t
pakchois_login (pakchois_session_t * sess, ck_user_type_t user_type,
                unsigned char *pin, unsigned long pin_len)
{
  return CALLS3 (Login, user_type, pin, pin_len);
}

ck_rv_t
pakchois_logout (pakchois_session_t * sess)
{
  return CALLS (Logout, (sess->id));
}

ck_rv_t
pakchois_create_object (pakchois_session_t * sess,
                        struct ck_attribute * templ,
                        unsigned long count, ck_object_handle_t * object)
{
  return CALLS3 (CreateObject, templ, count, object);
}

ck_rv_t
pakchois_copy_object (pakchois_session_t * sess,
                      ck_object_handle_t object,
                      struct ck_attribute * templ,
                      unsigned long count, ck_object_handle_t * new_object)
{
  return CALLS4 (CopyObject, object, templ, count, new_object);
}

ck_rv_t
pakchois_destroy_object (pakchois_session_t * sess, ck_object_handle_t object)
{
  return CALLS1 (DestroyObject, object);
}

ck_rv_t
pakchois_get_object_size (pakchois_session_t * sess,
                          ck_object_handle_t object, unsigned long *size)
{
  return CALLS2 (GetObjectSize, object, size);
}

ck_rv_t
pakchois_get_attribute_value (pakchois_session_t * sess,
                              ck_object_handle_t object,
                              struct ck_attribute * templ,
                              unsigned long count)
{
  return CALLS3 (GetAttributeValue, object, templ, count);
}

ck_rv_t
pakchois_set_attribute_value (pakchois_session_t * sess,
                              ck_object_handle_t object,
                              struct ck_attribute * templ,
                              unsigned long count)
{
  return CALLS3 (SetAttributeValue, object, templ, count);
}

ck_rv_t
pakchois_find_objects_init (pakchois_session_t * sess,
                            struct ck_attribute * templ, unsigned long count)
{
  return CALLS2 (FindObjectsInit, templ, count);
}

ck_rv_t
pakchois_find_objects (pakchois_session_t * sess,
                       ck_object_handle_t * object,
                       unsigned long max_object_count,
                       unsigned long *object_count)
{
  return CALLS3 (FindObjects, object, max_object_count, object_count);
}

ck_rv_t
pakchois_find_objects_final (pakchois_session_t * sess)
{
  return CALLS (FindObjectsFinal, (sess->id));
}

ck_rv_t
pakchois_encrypt_init (pakchois_session_t * sess,
                       struct ck_mechanism * mechanism,
                       ck_object_handle_t key)
{
  return CALLS2 (EncryptInit, mechanism, key);
}

ck_rv_t
pakchois_encrypt (pakchois_session_t * sess,
                  unsigned char *data, unsigned long data_len,
                  unsigned char *encrypted_data,
                  unsigned long *encrypted_data_len)
{
  return CALLS4 (Encrypt, data, data_len, encrypted_data, encrypted_data_len);
}

ck_rv_t
pakchois_encrypt_update (pakchois_session_t * sess,
                         unsigned char *part,
                         unsigned long part_len,
                         unsigned char *encrypted_part,
                         unsigned long *encrypted_part_len)
{
  return CALLS4 (EncryptUpdate, part, part_len,
                 encrypted_part, encrypted_part_len);
}

ck_rv_t
pakchois_encrypt_final (pakchois_session_t * sess,
                        unsigned char *last_encrypted_part,
                        unsigned long *last_encrypted_part_len)
{
  return CALLS2 (EncryptFinal, last_encrypted_part, last_encrypted_part_len);
}

ck_rv_t
pakchois_decrypt_init (pakchois_session_t * sess,
                       struct ck_mechanism * mechanism,
                       ck_object_handle_t key)
{
  return CALLS2 (DecryptInit, mechanism, key);
}

ck_rv_t
pakchois_decrypt (pakchois_session_t * sess,
                  unsigned char *encrypted_data,
                  unsigned long encrypted_data_len,
                  unsigned char *data, unsigned long *data_len)
{
  return CALLS4 (Decrypt, encrypted_data, encrypted_data_len, data, data_len);
}

ck_rv_t
pakchois_decrypt_update (pakchois_session_t * sess,
                         unsigned char *encrypted_part,
                         unsigned long encrypted_part_len,
                         unsigned char *part, unsigned long *part_len)
{
  return CALLS4 (DecryptUpdate, encrypted_part, encrypted_part_len,
                 part, part_len);
}

ck_rv_t
pakchois_decrypt_final (pakchois_session_t * sess,
                        unsigned char *last_part,
                        unsigned long *last_part_len)
{
  return CALLS2 (DecryptFinal, last_part, last_part_len);
}

ck_rv_t
pakchois_digest_init (pakchois_session_t * sess,
                      struct ck_mechanism * mechanism)
{
  return CALLS1 (DigestInit, mechanism);
}

ck_rv_t
pakchois_digest (pakchois_session_t * sess, unsigned char *data,
                 unsigned long data_len, unsigned char *digest,
                 unsigned long *digest_len)
{
  return CALLS4 (Digest, data, data_len, digest, digest_len);
}

ck_rv_t
pakchois_digest_update (pakchois_session_t * sess,
                        unsigned char *part, unsigned long part_len)
{
  return CALLS2 (DigestUpdate, part, part_len);
}

ck_rv_t
pakchois_digest_key (pakchois_session_t * sess, ck_object_handle_t key)
{
  return CALLS1 (DigestKey, key);
}

ck_rv_t
pakchois_digest_final (pakchois_session_t * sess,
                       unsigned char *digest, unsigned long *digest_len)
{
  return CALLS2 (DigestFinal, digest, digest_len);
}

ck_rv_t
pakchois_sign_init (pakchois_session_t * sess,
                    struct ck_mechanism * mechanism, ck_object_handle_t key)
{
  return CALLS2 (SignInit, mechanism, key);
}

ck_rv_t
pakchois_sign (pakchois_session_t * sess, unsigned char *data,
               unsigned long data_len, unsigned char *signature,
               unsigned long *signature_len)
{
  return CALLS4 (Sign, data, data_len, signature, signature_len);
}

ck_rv_t
pakchois_sign_update (pakchois_session_t * sess,
                      unsigned char *part, unsigned long part_len)
{
  return CALLS2 (SignUpdate, part, part_len);
}

ck_rv_t
pakchois_sign_final (pakchois_session_t * sess,
                     unsigned char *signature, unsigned long *signature_len)
{
  return CALLS2 (SignFinal, signature, signature_len);
}

ck_rv_t
pakchois_sign_recover_init (pakchois_session_t * sess,
                            struct ck_mechanism * mechanism,
                            ck_object_handle_t key)
{
  return CALLS2 (SignRecoverInit, mechanism, key);
}

ck_rv_t
pakchois_sign_recover (pakchois_session_t * sess,
                       unsigned char *data, unsigned long data_len,
                       unsigned char *signature, unsigned long *signature_len)
{
  return CALLS4 (SignRecover, data, data_len, signature, signature_len);
}

ck_rv_t
pakchois_verify_init (pakchois_session_t * sess,
                      struct ck_mechanism * mechanism, ck_object_handle_t key)
{
  return CALLS2 (VerifyInit, mechanism, key);
}

ck_rv_t
pakchois_verify (pakchois_session_t * sess, unsigned char *data,
                 unsigned long data_len, unsigned char *signature,
                 unsigned long signature_len)
{
  return CALLS4 (Verify, data, data_len, signature, signature_len);
}

ck_rv_t
pakchois_verify_update (pakchois_session_t * sess,
                        unsigned char *part, unsigned long part_len)
{
  return CALLS2 (VerifyUpdate, part, part_len);
}

ck_rv_t
pakchois_verify_final (pakchois_session_t * sess,
                       unsigned char *signature, unsigned long signature_len)
{
  return CALLS2 (VerifyFinal, signature, signature_len);
}

ck_rv_t
pakchois_verify_recover_init (pakchois_session_t * sess,
                              struct ck_mechanism * mechanism,
                              ck_object_handle_t key)
{
  return CALLS2 (VerifyRecoverInit, mechanism, key);
}

ck_rv_t
pakchois_verify_recover (pakchois_session_t * sess,
                         unsigned char *signature,
                         unsigned long signature_len,
                         unsigned char *data, unsigned long *data_len)
{
  return CALLS4 (VerifyRecover, signature, signature_len, data, data_len);
}

ck_rv_t
pakchois_digest_encrypt_update (pakchois_session_t * sess,
                                unsigned char *part,
                                unsigned long part_len,
                                unsigned char *encrypted_part,
                                unsigned long *encrypted_part_len)
{
  return CALLS4 (DigestEncryptUpdate, part, part_len,
                 encrypted_part, encrypted_part_len);
}

ck_rv_t
pakchois_decrypt_digest_update (pakchois_session_t * sess,
                                unsigned char *encrypted_part,
                                unsigned long encrypted_part_len,
                                unsigned char *part, unsigned long *part_len)
{
  return CALLS4 (DecryptDigestUpdate, encrypted_part,
                 encrypted_part_len, part, part_len);
}

ck_rv_t
pakchois_sign_encrypt_update (pakchois_session_t * sess,
                              unsigned char *part,
                              unsigned long part_len,
                              unsigned char *encrypted_part,
                              unsigned long *encrypted_part_len)
{
  return CALLS4 (SignEncryptUpdate, part, part_len,
                 encrypted_part, encrypted_part_len);
}

ck_rv_t
pakchois_decrypt_verify_update (pakchois_session_t * sess,
                                unsigned char *encrypted_part,
                                unsigned long encrypted_part_len,
                                unsigned char *part, unsigned long *part_len)
{
  return CALLS4 (DecryptVerifyUpdate, encrypted_part,
                 encrypted_part_len, part, part_len);
}

ck_rv_t
pakchois_generate_key (pakchois_session_t * sess,
                       struct ck_mechanism * mechanism,
                       struct ck_attribute * templ,
                       unsigned long count, ck_object_handle_t * key)
{
  return CALLS4 (GenerateKey, mechanism, templ, count, key);
}

ck_rv_t
pakchois_generate_key_pair (pakchois_session_t * sess,
                            struct ck_mechanism * mechanism,
                            struct ck_attribute *
                            public_key_template,
                            unsigned long
                            public_key_attribute_count,
                            struct ck_attribute *
                            private_key_template,
                            unsigned long
                            private_key_attribute_count,
                            ck_object_handle_t * public_key,
                            ck_object_handle_t * private_key)
{
  return CALLS7 (GenerateKeyPair, mechanism,
                 public_key_template, public_key_attribute_count,
                 private_key_template, private_key_attribute_count,
                 public_key, private_key);
}

ck_rv_t
pakchois_wrap_key (pakchois_session_t * sess,
                   struct ck_mechanism * mechanism,
                   ck_object_handle_t wrapping_key,
                   ck_object_handle_t key,
                   unsigned char *wrapped_key, unsigned long *wrapped_key_len)
{
  return CALLS5 (WrapKey, mechanism, wrapping_key,
                 key, wrapped_key, wrapped_key_len);
}

ck_rv_t
pakchois_unwrap_key (pakchois_session_t * sess,
                     struct ck_mechanism * mechanism,
                     ck_object_handle_t unwrapping_key,
                     unsigned char *wrapped_key,
                     unsigned long wrapped_key_len,
                     struct ck_attribute * templ,
                     unsigned long attribute_count, ck_object_handle_t * key)
{
  return CALLS7 (UnwrapKey, mechanism, unwrapping_key,
                 wrapped_key, wrapped_key_len, templ, attribute_count, key);
}

ck_rv_t
pakchois_derive_key (pakchois_session_t * sess,
                     struct ck_mechanism * mechanism,
                     ck_object_handle_t base_key,
                     struct ck_attribute * templ,
                     unsigned long attribute_count, ck_object_handle_t * key)
{
  return CALLS5 (DeriveKey, mechanism, base_key, templ, attribute_count, key);
}


ck_rv_t
pakchois_seed_random (pakchois_session_t * sess,
                      unsigned char *seed, unsigned long seed_len)
{
  return CALLS2 (SeedRandom, seed, seed_len);
}

ck_rv_t
pakchois_generate_random (pakchois_session_t * sess,
                          unsigned char *random_data,
                          unsigned long random_len)
{
  return CALLS2 (GenerateRandom, random_data, random_len);
}
