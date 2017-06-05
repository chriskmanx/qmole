/*
 * GnuTLS PKCS#11 support
 * Copyright (C) 2010 Free Software Foundation
 * Copyright (C) 2008, Joe Orton <joe@manyfish.co.uk>
 * 
 * Author: Nikos Mavrogiannopoulos
 *
 * Inspired and some parts (pkcs11_login) based on neon PKCS #11 support 
 * by Joe Orton. More ideas came from the pkcs11-helper library by 
 * Alon Bar-Lev.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
*/

#include <gnutls_int.h>
#include <gnutls/pkcs11.h>
#include <stdio.h>
#include <string.h>
#include <gnutls_errors.h>
#include <gnutls_datum.h>
#include <pkcs11_int.h>

#define MAX_PROVIDERS 16

static void terminate_string (unsigned char *str, size_t len);

/* XXX: try to eliminate this */
#define MAX_CERT_SIZE 8*1024

struct gnutls_pkcs11_provider_s
{
  pakchois_module_t *module;
  unsigned long nslots;
  ck_slot_id_t *slots;
  struct ck_info info;
};

struct flags_find_data_st
{
  struct pkcs11_url_info info;
  unsigned int slot_flags;
};

struct url_find_data_st
{
  gnutls_pkcs11_obj_t crt;
};

struct crt_find_data_st
{
  gnutls_pkcs11_obj_t *p_list;
  unsigned int *n_list;
  unsigned int current;
  gnutls_pkcs11_obj_attr_t flags;
  struct pkcs11_url_info info;
};


static struct gnutls_pkcs11_provider_s providers[MAX_PROVIDERS];
static int active_providers = 0;

static gnutls_pkcs11_pin_callback_t pin_func;
static void *pin_data;

gnutls_pkcs11_token_callback_t token_func;
void *token_data;

int
pkcs11_rv_to_err (ck_rv_t rv)
{
  switch (rv)
    {
    case CKR_OK:
      return 0;
    case CKR_HOST_MEMORY:
      return GNUTLS_E_MEMORY_ERROR;
    case CKR_SLOT_ID_INVALID:
      return GNUTLS_E_PKCS11_SLOT_ERROR;
    case CKR_ARGUMENTS_BAD:
    case CKR_MECHANISM_PARAM_INVALID:
      return GNUTLS_E_INVALID_REQUEST;
    case CKR_NEED_TO_CREATE_THREADS:
    case CKR_CANT_LOCK:
    case CKR_FUNCTION_NOT_PARALLEL:
    case CKR_MUTEX_BAD:
    case CKR_MUTEX_NOT_LOCKED:
      return GNUTLS_E_LOCKING_ERROR;
    case CKR_ATTRIBUTE_READ_ONLY:
    case CKR_ATTRIBUTE_SENSITIVE:
    case CKR_ATTRIBUTE_TYPE_INVALID:
    case CKR_ATTRIBUTE_VALUE_INVALID:
      return GNUTLS_E_PKCS11_ATTRIBUTE_ERROR;
    case CKR_DEVICE_ERROR:
    case CKR_DEVICE_MEMORY:
    case CKR_DEVICE_REMOVED:
      return GNUTLS_E_PKCS11_DEVICE_ERROR;
    case CKR_DATA_INVALID:
    case CKR_DATA_LEN_RANGE:
    case CKR_ENCRYPTED_DATA_INVALID:
    case CKR_ENCRYPTED_DATA_LEN_RANGE:
    case CKR_OBJECT_HANDLE_INVALID:
      return GNUTLS_E_PKCS11_DATA_ERROR;
    case CKR_FUNCTION_NOT_SUPPORTED:
    case CKR_MECHANISM_INVALID:
      return GNUTLS_E_PKCS11_UNSUPPORTED_FEATURE_ERROR;
    case CKR_KEY_HANDLE_INVALID:
    case CKR_KEY_SIZE_RANGE:
    case CKR_KEY_TYPE_INCONSISTENT:
    case CKR_KEY_NOT_NEEDED:
    case CKR_KEY_CHANGED:
    case CKR_KEY_NEEDED:
    case CKR_KEY_INDIGESTIBLE:
    case CKR_KEY_FUNCTION_NOT_PERMITTED:
    case CKR_KEY_NOT_WRAPPABLE:
    case CKR_KEY_UNEXTRACTABLE:
      return GNUTLS_E_PKCS11_KEY_ERROR;
    case CKR_PIN_INCORRECT:
    case CKR_PIN_INVALID:
    case CKR_PIN_LEN_RANGE:
      return GNUTLS_E_PKCS11_PIN_ERROR;
    case CKR_PIN_EXPIRED:
      return GNUTLS_E_PKCS11_PIN_EXPIRED;
    case CKR_PIN_LOCKED:
      return GNUTLS_E_PKCS11_PIN_LOCKED;
    case CKR_SESSION_CLOSED:
    case CKR_SESSION_COUNT:
    case CKR_SESSION_HANDLE_INVALID:
    case CKR_SESSION_PARALLEL_NOT_SUPPORTED:
    case CKR_SESSION_READ_ONLY:
    case CKR_SESSION_EXISTS:
    case CKR_SESSION_READ_ONLY_EXISTS:
    case CKR_SESSION_READ_WRITE_SO_EXISTS:
      return GNUTLS_E_PKCS11_SESSION_ERROR;
    case CKR_SIGNATURE_INVALID:
    case CKR_SIGNATURE_LEN_RANGE:
      return GNUTLS_E_PKCS11_SIGNATURE_ERROR;
    case CKR_TOKEN_NOT_PRESENT:
    case CKR_TOKEN_NOT_RECOGNIZED:
    case CKR_TOKEN_WRITE_PROTECTED:
      return GNUTLS_E_PKCS11_TOKEN_ERROR;
    case CKR_USER_ALREADY_LOGGED_IN:
    case CKR_USER_NOT_LOGGED_IN:
    case CKR_USER_PIN_NOT_INITIALIZED:
    case CKR_USER_TYPE_INVALID:
    case CKR_USER_ANOTHER_ALREADY_LOGGED_IN:
    case CKR_USER_TOO_MANY_TYPES:
      return GNUTLS_E_PKCS11_USER_ERROR;
    case CKR_BUFFER_TOO_SMALL:
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    default:
      return GNUTLS_E_PKCS11_ERROR;
    }
}

/* Fake scan */
void
pkcs11_rescan_slots (void)
{
  unsigned long slots;

  pakchois_get_slot_list (providers[active_providers - 1].module, 0,
                          NULL, &slots);
}

/**
 * gnutls_pkcs11_add_provider:
 * @name: The filename of the module
 * @params: should be NULL
 *
 * This function will load and add a PKCS 11 module to the module
 * list used in gnutls. After this function is called the module will
 * be used for PKCS 11 operations.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_add_provider (const char *name, const char *params)
{

  if (active_providers >= MAX_PROVIDERS)
    {
      gnutls_assert ();
      return GNUTLS_E_CONSTRAINT_ERROR;
    }

  active_providers++;
  if (pakchois_module_load_abs
      (&providers[active_providers - 1].module, name) != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("p11: Cannot load provider %s\n", name);
      active_providers--;
      return GNUTLS_E_PKCS11_LOAD_ERROR;
    }

  /* cache the number of slots in this module */
  if (pakchois_get_slot_list
      (providers[active_providers - 1].module, 0, NULL,
       &providers[active_providers - 1].nslots) != CKR_OK)
    {
      gnutls_assert ();
      goto fail;
    }

  providers[active_providers - 1].slots =
    gnutls_malloc (sizeof (*providers[active_providers - 1].slots) *
                   providers[active_providers - 1].nslots);
  if (providers[active_providers - 1].slots == NULL)
    {
      gnutls_assert ();
      goto fail;
    }

  if (pakchois_get_slot_list
      (providers[active_providers - 1].module, 0,
       providers[active_providers - 1].slots,
       &providers[active_providers - 1].nslots) != CKR_OK)
    {
      gnutls_assert ();
      gnutls_free (providers[active_providers - 1].slots);
      goto fail;
    }

  memset (&providers[active_providers - 1].info, 0,
          sizeof (providers[active_providers - 1].info));
  pakchois_get_info (providers[active_providers - 1].module,
                     &providers[active_providers - 1].info);

  terminate_string (providers[active_providers - 1].info.manufacturer_id,
                    sizeof (providers[active_providers - 1].
                            info.manufacturer_id));
  terminate_string (providers[active_providers - 1].info.library_description,
                    sizeof (providers[active_providers - 1].
                            info.library_description));

  _gnutls_debug_log ("p11: loaded provider '%s' with %d slots\n",
                     name, (int) providers[active_providers - 1].nslots);

  return 0;

fail:
  pakchois_module_destroy (providers[active_providers - 1].module);
  active_providers--;
  return GNUTLS_E_PKCS11_LOAD_ERROR;

}


/**
 * gnutls_pkcs11_obj_get_info:
 * @crt: should contain a #gnutls_pkcs11_obj_t structure
 * @itype: Denotes the type of information requested
 * @output: where output will be stored
 * @output_size: contains the maximum size of the output and will be overwritten with actual
 *
 * This function will return information about the PKCS 11 certificatesuch
 * as the label, id as well as token information where the key is stored. When
 * output is text it returns null terminated string although %output_size contains
 * the size of the actual data only.
 *
 * Returns: zero on success or a negative value on error.
 **/
int
gnutls_pkcs11_obj_get_info (gnutls_pkcs11_obj_t crt,
                            gnutls_pkcs11_obj_info_t itype,
                            void *output, size_t * output_size)
{
  return pkcs11_get_info (&crt->info, itype, output, output_size);
}

int
pkcs11_get_info (struct pkcs11_url_info *info,
                 gnutls_pkcs11_obj_info_t itype, void *output,
                 size_t * output_size)
{
  const char *str = NULL;
  size_t len;

  switch (itype)
    {
    case GNUTLS_PKCS11_OBJ_ID:
      if (*output_size < info->certid_raw_size)
        {
          *output_size = info->certid_raw_size;
          return GNUTLS_E_SHORT_MEMORY_BUFFER;
        }
      if (output)
        memcpy (output, info->certid_raw, info->certid_raw_size);
      *output_size = info->certid_raw_size;

      return 0;
    case GNUTLS_PKCS11_OBJ_ID_HEX:
      str = info->id;
      break;
    case GNUTLS_PKCS11_OBJ_LABEL:
      str = info->label;
      break;
    case GNUTLS_PKCS11_OBJ_TOKEN_LABEL:
      str = info->token;
      break;
    case GNUTLS_PKCS11_OBJ_TOKEN_SERIAL:
      str = info->serial;
      break;
    case GNUTLS_PKCS11_OBJ_TOKEN_MANUFACTURER:
      str = info->manufacturer;
      break;
    case GNUTLS_PKCS11_OBJ_TOKEN_MODEL:
      str = info->model;
      break;
    case GNUTLS_PKCS11_OBJ_LIBRARY_DESCRIPTION:
      str = info->lib_desc;
      break;
    case GNUTLS_PKCS11_OBJ_LIBRARY_VERSION:
      str = info->lib_version;
      break;
    case GNUTLS_PKCS11_OBJ_LIBRARY_MANUFACTURER:
      str = info->lib_manufacturer;
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  len = strlen (str);

  if (len + 1 > *output_size)
    {
      *output_size = len + 1;
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }

  strcpy (output, str);

  *output_size = len;

  return 0;
}

static int init = 0;


/**
 * gnutls_pkcs11_init:
 * @flags: %GNUTLS_PKCS11_FLAG_MANUAL or %GNUTLS_PKCS11_FLAG_AUTO
 * @configfile: either NULL or the location of a configuration file
 *
 * This function will initialize the PKCS 11 subsystem in gnutls. It will
 * read a configuration file if %GNUTLS_PKCS11_FLAG_AUTO is used or allow
 * you to independently load PKCS 11 modules using gnutls_pkcs11_add_provider()
 * if %GNUTLS_PKCS11_FLAG_MANUAL is specified.
 *
 * Normally you don't need to call this function since it is being called
 * by gnutls_global_init() using the %GNUTLS_PKCS11_FLAG_AUTO. If other option
 * is required then it must be called before it.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_init (unsigned int flags, const char *configfile)
{
  int ret;

  if (init != 0)
    {
      init++;
      return 0;
    }
  init++;

  if (flags == GNUTLS_PKCS11_FLAG_MANUAL)
    return 0;
  else
    {
      FILE *fp;
      char line[512];
      const char *library;

      if (configfile == NULL)
        configfile = "/etc/gnutls/pkcs11.conf";

      fp = fopen (configfile, "r");
      if (fp == NULL)
        {
          gnutls_assert ();
          _gnutls_debug_log ("Cannot load %s\n", configfile);
          return GNUTLS_E_FILE_ERROR;
        }

      while (fgets (line, sizeof (line), fp) != NULL)
        {
          if (strncmp (line, "load", sizeof ("load") - 1) == 0)
            {
              char *p;
              p = strchr (line, '=');
              if (p == NULL)
                continue;

              library = ++p;

              p = strchr (line, '\n');
              if (p != NULL)
                {
                  *p = 0;
                }

              ret = gnutls_pkcs11_add_provider (library, NULL);
              if (ret < 0)
                {
                  gnutls_assert ();
                  _gnutls_debug_log ("Cannot load provider: %s\n", library);
                  continue;
                }
            }
        }
      fclose(fp);
    }

  return 0;
}

/**
 * gnutls_pkcs11_deinit:
 *
 * This function will deinitialize the PKCS 11 subsystem in gnutls.
 *
 **/
void
gnutls_pkcs11_deinit (void)
{
  int i;

  init--;
  if (init > 0)
    return;
  if (init < 0)
    {
      init = 0;
      return;
    }

  for (i = 0; i < active_providers; i++)
    {
      pakchois_module_destroy (providers[i].module);
    }
  active_providers = 0;
  pakchois_destructor();
}

/**
 * gnutls_pkcs11_set_pin_function:
 * @fn: The PIN callback
 * @userdata: data to be supplied to callback
 *
 * This function will set a callback function to be used when a PIN
 * is required for PKCS 11 operations.
 *
 * Callback for PKCS#11 PIN entry.  The callback provides the PIN code
 * to unlock the token with label 'token_label', specified by the URL 
 * 'token_url'.
 *
 * The PIN code, as a NUL-terminated ASCII string, should be copied
 * into the 'pin' buffer (of maximum size pin_max), and
 * return 0 to indicate success. Alternatively, the callback may
 * return a negative gnutls error code to indicate failure and cancel
 * PIN entry (in which case, the contents of the 'pin' parameter are ignored).
 *
 * When a PIN is required, the callback will be invoked repeatedly
 * (and indefinitely) until either the returned PIN code is correct,
 * the callback returns failure, or the token refuses login (e.g. when
 * the token is locked due to too many incorrect PINs!).  For the
 * first such invocation, the 'attempt' counter will have value zero;
 * it will increase by one for each subsequent attempt.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
void
gnutls_pkcs11_set_pin_function (gnutls_pkcs11_pin_callback_t fn,
                                void *userdata)
{
  pin_func = fn;
  pin_data = userdata;
}

/**
 * gnutls_pkcs11_set_token_function:
 * @fn: The token callback
 * @userdata: data to be supplied to callback
 *
 * This function will set a callback function to be used when a token
 * needs to be inserted to continue PKCS 11 operations.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
void
gnutls_pkcs11_set_token_function (gnutls_pkcs11_token_callback_t fn,
                                  void *userdata)
{
  token_func = fn;
  token_data = userdata;
}

static int
unescape_string (char *output, const char *input, size_t * size,
                 char terminator)
{
  gnutls_buffer_st str;
  int ret = 0;
  char *p;
  int len;

  _gnutls_buffer_init (&str);

  /* find terminator */
  p = strchr (input, terminator);
  if (p != NULL)
    len = p - input;
  else
    len = strlen (input);

  ret = _gnutls_buffer_append_data (&str, input, len);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_buffer_unescape (&str);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_buffer_append_data (&str, "", 1);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  _gnutls_buffer_pop_data (&str, output, size);

  _gnutls_buffer_clear (&str);

  return ret;
}

int
pkcs11_url_to_info (const char *url, struct pkcs11_url_info *info)
{
  int ret;
  char *p1, *p2;
  size_t l;

  memset (info, 0, sizeof (*info));

  if (strstr (url, "pkcs11:") == NULL)
    {
      ret = GNUTLS_E_PARSING_ERROR;
      goto cleanup;
    }

  if ((p1 = strstr (url, "library-manufacturer=")) != NULL)
    {
      p1 += sizeof ("library-manufacturer=") - 1;
      l = sizeof (info->lib_manufacturer);

      ret = unescape_string (info->lib_manufacturer, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "library-description=")) != NULL)
    {
      p1 += sizeof ("library-description=") - 1;
      l = sizeof (info->lib_desc);

      ret = unescape_string (info->lib_desc, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "library-version=")) != NULL)
    {
      p1 += sizeof ("library-version=") - 1;
      l = sizeof (info->lib_version);

      ret = unescape_string (info->lib_version, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, ";manufacturer=")) != NULL ||
      (p1 = strstr (url, ":manufacturer=")) != NULL)
    {

      p1 += sizeof (";manufacturer=") - 1;
      l = sizeof (info->manufacturer);

      ret = unescape_string (info->manufacturer, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "token=")) != NULL)
    {
      p1 += sizeof ("token=") - 1;
      l = sizeof (info->token);

      ret = unescape_string (info->token, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "object=")) != NULL)
    {
      p1 += sizeof ("object=") - 1;
      l = sizeof (info->label);

      ret = unescape_string (info->label, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "serial=")) != NULL)
    {
      p1 += sizeof ("serial=") - 1;
      l = sizeof (info->serial);

      ret = unescape_string (info->serial, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "model=")) != NULL)
    {
      p1 += sizeof ("model=") - 1;
      l = sizeof (info->model);

      ret = unescape_string (info->model, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if ((p1 = strstr (url, "objecttype=")) != NULL)
    {
      p1 += sizeof ("objecttype=") - 1;
      l = sizeof (info->type);

      ret = unescape_string (info->type, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
    }

  if (((p1 = strstr (url, ";id=")) != NULL)
      || ((p1 = strstr (url, ":id=")) != NULL))
    {
      p1 += sizeof (";id=") - 1;
      l = sizeof (info->certid_raw);

      ret = unescape_string (info->certid_raw, p1, &l, ';');
      if (ret < 0)
        {
          goto cleanup;
        }
      /* not null terminated */
      info->certid_raw_size = l-1;

      p2 = _gnutls_bin2hex(info->certid_raw, info->certid_raw_size,
                           info->id, sizeof(info->id), ":");
      if (p2 == NULL)
        {
          ret = GNUTLS_E_PARSING_ERROR;
          goto cleanup;
        }
    }

  ret = 0;

cleanup:

  return ret;

}

#define INVALID_CHARS       "\\/\"'%&#@!?$* <>{}[]()`|:;,.+-"

/* Appends @tname to @dest under the name @p11name.
 * init indicates whether it is the initial addition to buffer.
 */
static int
append (gnutls_buffer_st * dest, const void *tname, int tname_size,
        const char *p11name, int all, int init)
{
  gnutls_buffer_st tmpstr;
  int ret;

  _gnutls_buffer_init (&tmpstr);
  if ((ret = _gnutls_buffer_append_data (&tmpstr, tname, tname_size)) < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = _gnutls_buffer_escape (&tmpstr, all, INVALID_CHARS);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  if ((ret = _gnutls_buffer_append_data (&tmpstr, "", 1)) < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  if ((ret =
       _gnutls_buffer_append_printf (dest, "%s%s=%s",
                                     (init != 0) ? ";" : "", p11name,
                                     tmpstr.data)) < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = 0;

cleanup:
  _gnutls_buffer_clear (&tmpstr);

  return ret;

}


int
pkcs11_info_to_url (const struct pkcs11_url_info *info,
                    gnutls_pkcs11_url_type_t detailed, char **url)
{
  gnutls_buffer_st str;
  int init = 0;
  int ret;

  _gnutls_buffer_init (&str);

  _gnutls_buffer_append_str (&str, "pkcs11:");

  if (info->token[0])
    {
      ret = append (&str, info->token, strlen(info->token), "token", 0, init);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      init = 1;
    }

  if (info->serial[0])
    {
      ret = append (&str, info->serial, strlen(info->serial), "serial", 0, init);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      init = 1;
    }

  if (info->model[0])
    {
      ret = append (&str, info->model, strlen(info->model), "model", 0, init);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      init = 1;
    }


  if (info->manufacturer[0])
    {
      ret = append (&str, info->manufacturer, strlen(info->manufacturer), "manufacturer", 0, init);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      init = 1;
    }

  if (info->label[0])
    {
      ret = append (&str, info->label, strlen(info->label), "object", 0, init);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      init = 1;
    }

  if (info->type[0])
    {
      ret = append (&str, info->type, strlen(info->type), "objecttype", 0, init);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
      init = 1;
    }

  if (detailed > GNUTLS_PKCS11_URL_GENERIC)
    {
      if (info->lib_manufacturer[0])
        {
          ret =
            append (&str, info->lib_manufacturer, strlen(info->lib_manufacturer), "library-manufacturer",
                    0, init);
          if (ret < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
          init = 1;
        }

      if (info->lib_desc[0])
        {
          ret = append (&str, info->lib_desc, strlen(info->lib_desc), "library-description", 0, init);
          if (ret < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
          init = 1;
        }
    }

  if (detailed > GNUTLS_PKCS11_URL_LIB)
    {
      if (info->lib_version[0])
        {
          ret = append (&str, info->lib_version, strlen(info->lib_version), "library-version", 0, init);
          if (ret < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }
          init = 1;
        }
    }

  if (info->certid_raw_size > 0)
    {
      ret = append (&str, info->certid_raw, info->certid_raw_size, "id", 1, init);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
    }

  _gnutls_buffer_append_data (&str, "", 1);

  *url = str.data;

  return 0;

cleanup:
  _gnutls_buffer_clear (&str);
  return ret;
}

/**
 * gnutls_pkcs11_obj_init:
 * @obj: The structure to be initialized
 *
 * This function will initialize a pkcs11 certificate structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_obj_init (gnutls_pkcs11_obj_t * obj)
{
  *obj = gnutls_calloc (1, sizeof (struct gnutls_pkcs11_obj_st));
  if (*obj == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  return 0;
}

/**
 * gnutls_pkcs11_obj_deinit:
 * @obj: The structure to be initialized
 *
 * This function will deinitialize a certificate structure.
 **/
void
gnutls_pkcs11_obj_deinit (gnutls_pkcs11_obj_t obj)
{
  _gnutls_free_datum (&obj->raw);
  free (obj);
}

/**
 * gnutls_pkcs11_obj_export:
 * @obj: Holds the object
 * @output_data: will contain a certificate PEM or DER encoded
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will export the pkcs11 object data. It is normal
 * for PKCS #11 data to be inaccesible and in that case %GNUTLS_E_INVALID_REQUEST
 * will be returned.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *output_data_size is updated and GNUTLS_E_SHORT_MEMORY_BUFFER will
 * be returned.
 *
 * If the structure is PEM encoded, it will have a header
 * of "BEGIN CERTIFICATE".
 *
 * Return value: In case of failure a negative value will be
 *   returned, and 0 on success.
 **/
int
gnutls_pkcs11_obj_export (gnutls_pkcs11_obj_t obj,
                          void *output_data, size_t * output_data_size)
{
  if (obj == NULL || obj->raw.data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (output_data == NULL || *output_data_size < obj->raw.size)
    {
      *output_data_size = obj->raw.size;
      gnutls_assert ();
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }
  *output_data_size = obj->raw.size;

  memcpy (output_data, obj->raw.data, obj->raw.size);
  return 0;
}

static void
terminate_string (unsigned char *str, size_t len)
{
  unsigned char *ptr = str + len - 1;

  while ((*ptr == ' ' || *ptr == '\t' || *ptr == '\0') && ptr >= str)
    ptr--;

  if (ptr == str - 1)
    str[0] = '\0';
  else if (ptr == str + len - 1)
    str[len - 1] = '\0';
  else
    ptr[1] = '\0';
}

int
pkcs11_find_object (pakchois_session_t ** _pks,
                    ck_object_handle_t * _obj,
                    struct pkcs11_url_info *info, unsigned int flags)
{
  int ret;
  pakchois_session_t *pks;
  ck_object_handle_t obj;
  ck_object_class_t class;
  struct ck_attribute a[4];
  int a_vals = 0;
  unsigned long count;
  ck_rv_t rv;

  class = pkcs11_strtype_to_class (info->type);
  if (class == -1)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  ret = pkcs11_open_session (&pks, info, flags & SESSION_LOGIN);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  a[a_vals].type = CKA_CLASS;
  a[a_vals].value = &class;
  a[a_vals].value_len = sizeof class;
  a_vals++;

  if (info->certid_raw_size > 0)
    {
      a[a_vals].type = CKA_ID;
      a[a_vals].value = info->certid_raw;
      a[a_vals].value_len = info->certid_raw_size;
      a_vals++;
    }

  rv = pakchois_find_objects_init (pks, a, a_vals);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pk11: FindObjectsInit failed.\n");
      ret = pkcs11_rv_to_err (rv);
      goto fail;
    }

  if (pakchois_find_objects (pks, &obj, 1, &count) == CKR_OK && count == 1)
    {
      *_obj = obj;
      *_pks = pks;
      pakchois_find_objects_final (pks);
      return 0;
    }

  ret = GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
  pakchois_find_objects_final (pks);
fail:
  pakchois_close_session (pks);

  return ret;
}

static void
fix_strings (struct token_info *info)
{
  terminate_string (info->tinfo.manufacturer_id,
                    sizeof info->tinfo.manufacturer_id);
  terminate_string (info->tinfo.label, sizeof info->tinfo.label);
  terminate_string (info->tinfo.model, sizeof info->tinfo.model);
  terminate_string (info->tinfo.serial_number,
                    sizeof info->tinfo.serial_number);
  terminate_string (info->sinfo.slot_description,
                    sizeof info->sinfo.slot_description);
}

int
pkcs11_find_slot (pakchois_module_t ** module, ck_slot_id_t * slot,
                  struct pkcs11_url_info *info, struct token_info *_tinfo)
{
  int x, z;

  for (x = 0; x < active_providers; x++)
    {
      for (z = 0; z < providers[x].nslots; z++)
        {
          struct token_info tinfo;

          if (pakchois_get_token_info
              (providers[x].module, providers[x].slots[z],
               &tinfo.tinfo) != CKR_OK)
            {
              continue;
            }
          tinfo.sid = providers[x].slots[z];
          tinfo.prov = &providers[x];

          if (pakchois_get_slot_info
              (providers[x].module, providers[x].slots[z],
               &tinfo.sinfo) != CKR_OK)
            {
              continue;
            }

          /* XXX make wrapper for token_info? */
          fix_strings (&tinfo);

          if (pkcs11_token_matches_info (info, &tinfo.tinfo,
                                         &providers[x].info) < 0)
            {
              continue;
            }

          /* ok found */
          *module = providers[x].module;
          *slot = providers[x].slots[z];

          if (_tinfo != NULL)
            memcpy (_tinfo, &tinfo, sizeof (tinfo));

          return 0;
        }
    }

  gnutls_assert ();
  return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
}

int
pkcs11_open_session (pakchois_session_t ** _pks,
                     struct pkcs11_url_info *info, unsigned int flags)
{
  ck_rv_t rv;
  int ret;
  pakchois_session_t *pks = NULL;
  pakchois_module_t *module;
  ck_slot_id_t slot;
  struct token_info tinfo;

  ret = pkcs11_find_slot (&module, &slot, info, &tinfo);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  rv = pakchois_open_session (module,
                              slot,
                              ((flags & SESSION_WRITE)
                               ? CKF_RW_SESSION : 0) |
                              CKF_SERIAL_SESSION, NULL, NULL, &pks);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      return pkcs11_rv_to_err (rv);
    }

  if (flags & SESSION_LOGIN)
    {
      ret = pkcs11_login (pks, &tinfo, (flags & SESSION_SO) ? 1 : 0);
      if (ret < 0)
        {
          gnutls_assert ();
          pakchois_close_session (pks);
          return ret;
        }
    }

  /* ok found */
  *_pks = pks;
  return 0;
}


int
_pkcs11_traverse_tokens (find_func_t find_func, void *input,
                         unsigned int flags)
{
  ck_rv_t rv;
  int found = 0, x, z, ret;
  pakchois_session_t *pks = NULL;

  for (x = 0; x < active_providers; x++)
    {
      for (z = 0; z < providers[x].nslots; z++)
        {
          struct token_info info;

          ret = GNUTLS_E_PKCS11_ERROR;

          if (pakchois_get_token_info
              (providers[x].module, providers[x].slots[z],
               &info.tinfo) != CKR_OK)
            {
              continue;
            }
          info.sid = providers[x].slots[z];
          info.prov = &providers[x];

          if (pakchois_get_slot_info
              (providers[x].module, providers[x].slots[z],
               &info.sinfo) != CKR_OK)
            {
              continue;
            }

          /* XXX make wrapper for token_info? */
          fix_strings (&info);

          rv = pakchois_open_session (providers[x].module,
                                      providers[x].slots[z],
                                      ((flags & SESSION_WRITE)
                                       ? CKF_RW_SESSION : 0) |
                                      CKF_SERIAL_SESSION, NULL, NULL, &pks);
          if (rv != CKR_OK)
            {
              continue;
            }

          if (flags & SESSION_LOGIN)
            {
              ret = pkcs11_login (pks, &info, (flags & SESSION_SO) ? 1 : 0);
              if (ret < 0)
                {
                  gnutls_assert ();
                  return ret;
                }
            }

          ret = find_func (pks, &info, &providers[x].info, input);

          if (ret == 0)
            {
              found = 1;
              goto finish;
            }
          else
            {
              pakchois_close_session (pks);
              pks = NULL;
            }
        }
    }

finish:
  /* final call */

  if (found == 0)
    {
      ret = find_func (pks, NULL, NULL, input);
    }
  else
    {
      ret = 0;
    }

  if (pks != NULL)
    {
      pakchois_close_session (pks);
    }

  return ret;
}

static const char *
pkcs11_obj_type_to_str (gnutls_pkcs11_obj_type_t type)
{
  switch (type)
    {
    case GNUTLS_PKCS11_OBJ_X509_CRT:
      return "cert";
    case GNUTLS_PKCS11_OBJ_PUBKEY:
      return "public";
    case GNUTLS_PKCS11_OBJ_PRIVKEY:
      return "private";
    case GNUTLS_PKCS11_OBJ_SECRET_KEY:
      return "secretkey";
    case GNUTLS_PKCS11_OBJ_DATA:
      return "data";
    case GNUTLS_PKCS11_OBJ_UNKNOWN:
    default:
      return "unknown";
    }
}

/* imports a raw certificate from a token to a pkcs11_obj_t structure.
 */
static int
pkcs11_obj_import (unsigned int class, gnutls_pkcs11_obj_t obj,
                   const gnutls_datum_t * data,
                   const gnutls_datum_t * id,
                   const gnutls_datum_t * label,
                   struct ck_token_info *tinfo, struct ck_info *lib_info)
{
  char *s;
  int ret;

  switch (class)
    {
    case CKO_CERTIFICATE:
      obj->type = GNUTLS_PKCS11_OBJ_X509_CRT;
      break;
    case CKO_PUBLIC_KEY:
      obj->type = GNUTLS_PKCS11_OBJ_PUBKEY;
      break;
    case CKO_PRIVATE_KEY:
      obj->type = GNUTLS_PKCS11_OBJ_PRIVKEY;
      break;
    case CKO_SECRET_KEY:
      obj->type = GNUTLS_PKCS11_OBJ_SECRET_KEY;
      break;
    case CKO_DATA:
      obj->type = GNUTLS_PKCS11_OBJ_DATA;
      break;
    default:
      obj->type = GNUTLS_PKCS11_OBJ_UNKNOWN;
    }

  if (obj->type != GNUTLS_PKCS11_OBJ_UNKNOWN)
    strcpy (obj->info.type, pkcs11_obj_type_to_str (obj->type));

  if (data && data->data)
    {
      ret = _gnutls_set_datum (&obj->raw, data->data, data->size);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }
    }

  terminate_string (tinfo->manufacturer_id, sizeof tinfo->manufacturer_id);
  terminate_string (tinfo->label, sizeof tinfo->label);
  terminate_string (tinfo->model, sizeof tinfo->model);
  terminate_string (tinfo->serial_number, sizeof tinfo->serial_number);

  /* write data */
  snprintf (obj->info.manufacturer, sizeof (obj->info.manufacturer),
            "%s", tinfo->manufacturer_id);
  snprintf (obj->info.token, sizeof (obj->info.token), "%s", tinfo->label);
  snprintf (obj->info.model, sizeof (obj->info.model), "%s", tinfo->model);
  snprintf (obj->info.serial, sizeof (obj->info.serial), "%s",
            tinfo->serial_number);

  snprintf (obj->info.lib_manufacturer, sizeof (obj->info.lib_manufacturer),
            "%s", lib_info->manufacturer_id);
  snprintf (obj->info.lib_desc, sizeof (obj->info.lib_desc), "%s",
            lib_info->library_description);
  snprintf (obj->info.lib_version, sizeof (obj->info.lib_version), "%u.%u",
            (unsigned int) lib_info->library_version.major,
            (unsigned int) lib_info->library_version.minor);



  if (label && label->data)
    {
      memcpy (obj->info.label, label->data, label->size);
      obj->info.label[label->size] = 0;
    }

  if (id && id->data)
    {
      s = _gnutls_bin2hex (id->data, id->size, obj->info.id,
                           sizeof (obj->info.id), ":");
      if (s == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_PKCS11_ERROR;
        }

      memmove (obj->info.certid_raw, id->data, id->size);
      obj->info.certid_raw_size = id->size;
    }

  return 0;
}

static int
pkcs11_obj_import_pubkey (pakchois_session_t * pks,
                          ck_object_handle_t obj,
                          gnutls_pkcs11_obj_t crt,
                          const gnutls_datum_t * id,
                          const gnutls_datum_t * label,
                          struct ck_token_info *tinfo,
                          struct ck_info *lib_info)
{

  struct ck_attribute a[4];
  ck_key_type_t key_type;
  opaque tmp1[2048];
  opaque tmp2[2048];
  int ret;
  ck_bool_t tval;

  a[0].type = CKA_KEY_TYPE;
  a[0].value = &key_type;
  a[0].value_len = sizeof (key_type);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      switch (key_type)
        {
        case CKK_RSA:
          a[0].type = CKA_MODULUS;
          a[0].value = tmp1;
          a[0].value_len = sizeof (tmp1);
          a[1].type = CKA_PUBLIC_EXPONENT;
          a[1].value = tmp2;
          a[1].value_len = sizeof (tmp2);

          if (pakchois_get_attribute_value (pks, obj, a, 2) == CKR_OK)
            {

              ret =
                _gnutls_set_datum (&crt->pubkey[0],
                                   a[0].value, a[0].value_len);

              if (ret >= 0)
                ret =
                  _gnutls_set_datum (&crt->pubkey
                                     [1], a[1].value, a[1].value_len);

              if (ret < 0)
                {
                  gnutls_assert ();
                  _gnutls_free_datum (&crt->pubkey[1]);
                  _gnutls_free_datum (&crt->pubkey[0]);
                  return GNUTLS_E_MEMORY_ERROR;
                }
            }
          else
            {
              gnutls_assert ();
              return GNUTLS_E_PKCS11_ERROR;
            }
          crt->pk_algorithm = GNUTLS_PK_RSA;
          break;
        case CKK_DSA:
          a[0].type = CKA_PRIME;
          a[0].value = tmp1;
          a[0].value_len = sizeof (tmp1);
          a[1].type = CKA_SUBPRIME;
          a[1].value = tmp2;
          a[1].value_len = sizeof (tmp2);

          if (pakchois_get_attribute_value (pks, obj, a, 2) == CKR_OK)
            {
              ret =
                _gnutls_set_datum (&crt->pubkey[0],
                                   a[0].value, a[0].value_len);

              if (ret >= 0)
                ret =
                  _gnutls_set_datum (&crt->pubkey
                                     [1], a[1].value, a[1].value_len);

              if (ret < 0)
                {
                  gnutls_assert ();
                  _gnutls_free_datum (&crt->pubkey[1]);
                  _gnutls_free_datum (&crt->pubkey[0]);
                  return GNUTLS_E_MEMORY_ERROR;
                }
            }
          else
            {
              gnutls_assert ();
              return GNUTLS_E_PKCS11_ERROR;
            }

          a[0].type = CKA_BASE;
          a[0].value = tmp1;
          a[0].value_len = sizeof (tmp1);
          a[1].type = CKA_VALUE;
          a[1].value = tmp2;
          a[1].value_len = sizeof (tmp2);

          if (pakchois_get_attribute_value (pks, obj, a, 2) == CKR_OK)
            {
              ret =
                _gnutls_set_datum (&crt->pubkey[2],
                                   a[0].value, a[0].value_len);

              if (ret >= 0)
                ret =
                  _gnutls_set_datum (&crt->pubkey
                                     [3], a[1].value, a[1].value_len);

              if (ret < 0)
                {
                  gnutls_assert ();
                  _gnutls_free_datum (&crt->pubkey[0]);
                  _gnutls_free_datum (&crt->pubkey[1]);
                  _gnutls_free_datum (&crt->pubkey[2]);
                  _gnutls_free_datum (&crt->pubkey[3]);
                  return GNUTLS_E_MEMORY_ERROR;
                }
            }
          else
            {
              gnutls_assert ();
              return GNUTLS_E_PKCS11_ERROR;
            }
          crt->pk_algorithm = GNUTLS_PK_RSA;
          break;
        default:
          gnutls_assert ();
          return GNUTLS_E_UNIMPLEMENTED_FEATURE;
        }
    }

  /* read key usage flags */
  a[0].type = CKA_ENCRYPT;
  a[0].value = &tval;
  a[0].value_len = sizeof (tval);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      if (tval != 0)
        {
          crt->key_usage |= GNUTLS_KEY_DATA_ENCIPHERMENT;
        }
    }

  a[0].type = CKA_VERIFY;
  a[0].value = &tval;
  a[0].value_len = sizeof (tval);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      if (tval != 0)
        {
          crt->key_usage |= GNUTLS_KEY_DIGITAL_SIGNATURE |
            GNUTLS_KEY_KEY_CERT_SIGN | GNUTLS_KEY_CRL_SIGN
            | GNUTLS_KEY_NON_REPUDIATION;
        }
    }

  a[0].type = CKA_VERIFY_RECOVER;
  a[0].value = &tval;
  a[0].value_len = sizeof (tval);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      if (tval != 0)
        {
          crt->key_usage |= GNUTLS_KEY_DIGITAL_SIGNATURE |
            GNUTLS_KEY_KEY_CERT_SIGN | GNUTLS_KEY_CRL_SIGN
            | GNUTLS_KEY_NON_REPUDIATION;
        }
    }

  a[0].type = CKA_DERIVE;
  a[0].value = &tval;
  a[0].value_len = sizeof (tval);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      if (tval != 0)
        {
          crt->key_usage |= GNUTLS_KEY_KEY_AGREEMENT;
        }
    }

  a[0].type = CKA_WRAP;
  a[0].value = &tval;
  a[0].value_len = sizeof (tval);

  if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
    {
      if (tval != 0)
        {
          crt->key_usage |= GNUTLS_KEY_KEY_ENCIPHERMENT;
        }
    }

  return pkcs11_obj_import (CKO_PUBLIC_KEY, crt, NULL, id, label,
                            tinfo, lib_info);
}

ck_object_class_t
pkcs11_strtype_to_class (const char *type)
{
  ck_object_class_t class;

  if (strcmp (type, "cert") == 0)
    {
      class = CKO_CERTIFICATE;
    }
  else if (strcmp (type, "public") == 0)
    {
      class = CKO_PUBLIC_KEY;
    }
  else if (strcmp (type, "private") == 0)
    {
      class = CKO_PRIVATE_KEY;
    }
  else if (strcmp (type, "secretkey") == 0)
    {
      class = CKO_SECRET_KEY;
    }
  else if (strcmp (type, "data") == 0)
    {
      class = CKO_DATA;
    }
  else
    {
      class = -1;
    }

  return class;
}


static int
find_obj_url (pakchois_session_t * pks, struct token_info *info,
              struct ck_info *lib_info, void *input)
{
  struct url_find_data_st *find_data = input;
  struct ck_attribute a[4];
  ck_object_class_t class = -1;
  ck_certificate_type_t type = -1;
  ck_rv_t rv;
  ck_object_handle_t obj;
  unsigned long count, a_vals;
  int found = 0, ret;
  opaque *cert_data = NULL;
  char label_tmp[PKCS11_LABEL_SIZE];

  if (info == NULL)
    {                           /* we don't support multiple calls */
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  /* do not bother reading the token if basic fields do not match
   */
  if (pkcs11_token_matches_info
      (&find_data->crt->info, &info->tinfo, lib_info) < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (find_data->crt->info.type[0] != 0)
    {
      class = pkcs11_strtype_to_class (find_data->crt->info.type);
      if (class == CKO_CERTIFICATE)
        type = CKC_X_509;

      if (class == -1)
        {
          gnutls_assert ();
          return GNUTLS_E_INVALID_REQUEST;
        }
    }

  /* search the token for the id */

  cert_data = gnutls_malloc (MAX_CERT_SIZE);
  if (cert_data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* Find objects with given class and type */

  a[0].type = CKA_ID;
  a[0].value = find_data->crt->info.certid_raw;
  a[0].value_len = find_data->crt->info.certid_raw_size;

  a_vals = 1;

  if (class != -1)
    {
      a[a_vals].type = CKA_CLASS;
      a[a_vals].value = &class;
      a[a_vals].value_len = sizeof class;
      a_vals++;
    }

  if (type != -1)
    {
      a[a_vals].type = CKA_CERTIFICATE_TYPE;
      a[a_vals].value = &type;
      a[a_vals].value_len = sizeof type;
      a_vals++;
    }

  rv = pakchois_find_objects_init (pks, a, a_vals);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pk11: FindObjectsInit failed.\n");
      ret = pkcs11_rv_to_err (rv);
      goto cleanup;
    }

  while (pakchois_find_objects (pks, &obj, 1, &count) == CKR_OK && count == 1)
    {

      a[0].type = CKA_VALUE;
      a[0].value = cert_data;
      a[0].value_len = MAX_CERT_SIZE;
      a[1].type = CKA_LABEL;
      a[1].value = label_tmp;
      a[1].value_len = sizeof (label_tmp);

      if (pakchois_get_attribute_value (pks, obj, a, 2) == CKR_OK)
        {
          gnutls_datum_t id = { find_data->crt->info.certid_raw,
            find_data->crt->info.certid_raw_size
          };
          gnutls_datum_t data = { a[0].value, a[0].value_len };
          gnutls_datum_t label = { a[1].value, a[1].value_len };

          if (class == CKO_PUBLIC_KEY)
            {
              ret =
                pkcs11_obj_import_pubkey (pks, obj,
                                          find_data->crt,
                                          &id, &label,
                                          &info->tinfo, lib_info);
            }
          else
            {
              ret =
                pkcs11_obj_import (class,
                                   find_data->crt,
                                   &data, &id, &label,
                                   &info->tinfo, lib_info);
            }
          if (ret < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }

          found = 1;
          break;
        }
      else
        {
          _gnutls_debug_log ("pk11: Skipped cert, missing attrs.\n");
        }
    }

  if (found == 0)
    {
      gnutls_assert ();
      ret = GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }
  else
    {
      ret = 0;
    }

cleanup:
  gnutls_free (cert_data);
  pakchois_find_objects_final (pks);

  return ret;
}

unsigned int
pkcs11_obj_flags_to_int (unsigned int flags)
{
  unsigned int ret_flags = 0;

  if (flags & GNUTLS_PKCS11_OBJ_FLAG_LOGIN)
    ret_flags |= SESSION_LOGIN;
  if (flags & GNUTLS_PKCS11_OBJ_FLAG_LOGIN_SO)
    ret_flags |= SESSION_LOGIN|SESSION_SO;

  return ret_flags;
}

/**
 * gnutls_pkcs11_privkey_import_url:
 * @cert: The structure to store the parsed certificate
 * @url: a PKCS 11 url identifying the key
 * @flags: One of GNUTLS_PKCS11_OBJ_* flags
 *
 * This function will "import" a PKCS 11 URL identifying a certificate
 * key to the #gnutls_pkcs11_obj_t structure. This does not involve any
 * parsing (such as X.509 or OpenPGP) since the #gnutls_pkcs11_obj_t is
 * format agnostic. Only data are transferred.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_obj_import_url (gnutls_pkcs11_obj_t cert, const char *url,
                              unsigned int flags)
{
  int ret;
  struct url_find_data_st find_data;

  /* fill in the find data structure */
  find_data.crt = cert;

  ret = pkcs11_url_to_info (url, &cert->info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    _pkcs11_traverse_tokens (find_obj_url, &find_data,
                             pkcs11_obj_flags_to_int (flags));

  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

struct token_num
{
  struct pkcs11_url_info info;
  unsigned int seq;             /* which one we are looking for */
  unsigned int current;         /* which one are we now */
};

static int
find_token_num (pakchois_session_t * pks,
                struct token_info *tinfo,
                struct ck_info *lib_info, void *input)
{
  struct token_num *find_data = input;

  if (tinfo == NULL)
    {                           /* we don't support multiple calls */
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (find_data->current == find_data->seq)
    {
      strcpy (find_data->info.manufacturer, tinfo->tinfo.manufacturer_id);
      strcpy (find_data->info.token, tinfo->tinfo.label);
      strcpy (find_data->info.model, tinfo->tinfo.model);
      strcpy (find_data->info.serial, tinfo->tinfo.serial_number);

      strcpy (find_data->info.lib_manufacturer, lib_info->manufacturer_id);
      strcpy (find_data->info.lib_desc, lib_info->library_description);
      snprintf (find_data->info.lib_version,
                sizeof (find_data->info.lib_version), "%u.%u",
                (unsigned int) lib_info->library_version.major,
                (unsigned int) lib_info->library_version.minor);

      return 0;
    }

  find_data->current++;
  /* search the token for the id */


  return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE; /* non zero is enough */
}

/**
 * gnutls_pkcs11_token_get_url:
 * @seq: sequence number starting from 0
 * @detailed: non zero if a detailed URL is required
 * @url: will contain an allocated url
 *
 * This function will return the URL for each token available
 * in system. The url has to be released using gnutls_free()
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, %GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE
 * if the sequence number exceeds the available tokens, otherwise a negative error value.
 **/

int
gnutls_pkcs11_token_get_url (unsigned int seq,
                             gnutls_pkcs11_url_type_t detailed, char **url)
{
  int ret;
  struct token_num tn;

  memset (&tn, 0, sizeof (tn));
  tn.seq = seq;

  ret = _pkcs11_traverse_tokens (find_token_num, &tn, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = pkcs11_info_to_url (&tn.info, detailed, url);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;

}

/**
 * gnutls_pkcs11_token_get_info:
 * @url: should contain a PKCS 11 URL
 * @ttype: Denotes the type of information requested
 * @output: where output will be stored
 * @output_size: contains the maximum size of the output and will be overwritten with actual
 *
 * This function will return information about the PKCS 11 token such
 * as the label, id as well as token information where the key is stored.
 *
 * Returns: zero on success or a negative value on error.
 **/
int
gnutls_pkcs11_token_get_info (const char *url,
                              gnutls_pkcs11_token_info_t ttype,
                              void *output, size_t * output_size)
{
  const char *str;
  size_t len;
  struct pkcs11_url_info info;
  int ret;

  ret = pkcs11_url_to_info (url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  switch (ttype)
    {
    case GNUTLS_PKCS11_TOKEN_LABEL:
      str = info.token;
      break;
    case GNUTLS_PKCS11_TOKEN_SERIAL:
      str = info.serial;
      break;
    case GNUTLS_PKCS11_TOKEN_MANUFACTURER:
      str = info.manufacturer;
      break;
    case GNUTLS_PKCS11_TOKEN_MODEL:
      str = info.model;
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  len = strlen (str);

  if (len + 1 > *output_size)
    {
      *output_size = len + 1;
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    }

  strcpy (output, str);

  *output_size = len;

  return 0;
}

/**
 * gnutls_pkcs11_obj_export_url:
 * @obj: Holds the PKCS 11 certificate
 * @detailed: non zero if a detailed URL is required
 * @url: will contain an allocated url
 *
 * This function will export a URL identifying the given certificate.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_obj_export_url (gnutls_pkcs11_obj_t obj,
                              gnutls_pkcs11_url_type_t detailed, char **url)
{
  int ret;

  ret = pkcs11_info_to_url (&obj->info, detailed, url);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/**
 * gnutls_pkcs11_obj_get_type:
 * @certificate: Holds the PKCS 11 certificate
 *
 * This function will return the type of the certificate being
 * stored in the structure.
 *
 * Returns: The type of the certificate.
 **/
gnutls_pkcs11_obj_type_t
gnutls_pkcs11_obj_get_type (gnutls_pkcs11_obj_t obj)
{
  return obj->type;
}

struct pkey_list
{
  gnutls_buffer_st *key_ids;
  size_t key_ids_size;
};

int
pkcs11_login (pakchois_session_t * pks, const struct token_info *info, int so)
{
  int attempt = 0, ret;
  ck_rv_t rv;
  char *token_url;
  int pin_len;
  struct pkcs11_url_info uinfo;


  if (so == 0 && (info->tinfo.flags & CKF_LOGIN_REQUIRED) == 0)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pk11: No login required.\n");
      return 0;
    }

  memset (&uinfo, 0, sizeof (uinfo));
  strcpy (uinfo.manufacturer, info->tinfo.manufacturer_id);
  strcpy (uinfo.token, info->tinfo.label);
  strcpy (uinfo.model, info->tinfo.model);
  strcpy (uinfo.serial, info->tinfo.serial_number);
  ret = pkcs11_info_to_url (&uinfo, 1, &token_url);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  /* For a token with a "protected" (out-of-band) authentication
   * path, calling login with a NULL username is all that is
   * required. */
  if (info->tinfo.flags & CKF_PROTECTED_AUTHENTICATION_PATH)
    {
      rv = pakchois_login (pks, (so == 0) ? CKU_USER : CKU_SO, NULL, 0);
      if (rv == CKR_OK || rv == CKR_USER_ALREADY_LOGGED_IN)
        {
          return 0;
        }
      else
        {
          gnutls_assert ();
          _gnutls_debug_log ("pk11: Protected login failed.\n");
          ret = GNUTLS_E_PKCS11_ERROR;
          goto cleanup;
        }
    }

  /* Otherwise, PIN entry is necessary for login, so fail if there's
   * no callback. */
  if (!pin_func)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pk11: No pin callback but login required.\n");
      ret = GNUTLS_E_PKCS11_ERROR;
      goto cleanup;
    }

  do
    {
      struct ck_token_info tinfo;
      char pin[GNUTLS_PKCS11_MAX_PIN_LEN];
      unsigned int flags;

      memcpy(&tinfo, &info->tinfo, sizeof(tinfo));

      /* If login has been attempted once already, check the token
       * status again, the flags might change. */
      if (attempt)
        {
          if (pakchois_get_token_info
              (info->prov->module, info->sid, &tinfo) != CKR_OK)
            {
              gnutls_assert ();
              _gnutls_debug_log ("pk11: GetTokenInfo failed\n");
              ret = GNUTLS_E_PKCS11_ERROR;
              goto cleanup;
            }
        }

      flags = 0;
      if (so == 0)
        {
          flags |= GNUTLS_PKCS11_PIN_USER;
          if (tinfo.flags & CKF_USER_PIN_COUNT_LOW)
            flags |= GNUTLS_PKCS11_PIN_COUNT_LOW;
          if (tinfo.flags & CKF_USER_PIN_FINAL_TRY)
            flags |= GNUTLS_PKCS11_PIN_FINAL_TRY;
        }
      else
        {
          flags |= GNUTLS_PKCS11_PIN_SO;
          if (tinfo.flags & CKF_SO_PIN_COUNT_LOW)
            flags |= GNUTLS_PKCS11_PIN_COUNT_LOW;
          if (tinfo.flags & CKF_SO_PIN_FINAL_TRY)
            flags |= GNUTLS_PKCS11_PIN_FINAL_TRY;
        }

      ret = pin_func (pin_data, attempt++,
                      (char *) token_url,
                      (char *) info->tinfo.label, flags, pin, sizeof (pin));
      if (ret < 0)
        {
          gnutls_assert ();
          ret = GNUTLS_E_PKCS11_PIN_ERROR;
          goto cleanup;
        }
      pin_len = strlen (pin);

      rv = pakchois_login (pks, (so == 0) ? CKU_USER : CKU_SO,
                           (unsigned char *) pin, pin_len);

      /* Try to scrub the pin off the stack.  Clever compilers will
       * probably optimize this away, oh well. */
      memset (pin, 0, sizeof pin);
    }
  while (rv == CKR_PIN_INCORRECT);

  _gnutls_debug_log ("pk11: Login result = %lu\n", rv);


  ret = (rv == CKR_OK
         || rv == CKR_USER_ALREADY_LOGGED_IN) ? 0 : pkcs11_rv_to_err (rv);

cleanup:
  gnutls_free (token_url);
  return ret;
}

static int
find_privkeys (pakchois_session_t * pks, struct token_info *info,
               struct pkey_list *list)
{
  struct ck_attribute a[3];
  ck_object_class_t class;
  ck_rv_t rv;
  ck_object_handle_t obj;
  unsigned long count, current;
  char certid_tmp[PKCS11_ID_SIZE];

  class = CKO_PRIVATE_KEY;

  /* Find an object with private key class and a certificate ID
   * which matches the certificate. */
  /* FIXME: also match the cert subject. */
  a[0].type = CKA_CLASS;
  a[0].value = &class;
  a[0].value_len = sizeof class;

  rv = pakchois_find_objects_init (pks, a, 1);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      return pkcs11_rv_to_err (rv);
    }

  list->key_ids_size = 0;
  while (pakchois_find_objects (pks, &obj, 1, &count) == CKR_OK && count == 1)
    {
      list->key_ids_size++;
    }

  pakchois_find_objects_final (pks);

  if (list->key_ids_size == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  list->key_ids =
    gnutls_malloc (sizeof (gnutls_buffer_st) * list->key_ids_size);
  if (list->key_ids == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* actual search */
  a[0].type = CKA_CLASS;
  a[0].value = &class;
  a[0].value_len = sizeof class;

  rv = pakchois_find_objects_init (pks, a, 1);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      return pkcs11_rv_to_err (rv);
    }

  current = 0;
  while (pakchois_find_objects (pks, &obj, 1, &count) == CKR_OK && count == 1)
    {

      a[0].type = CKA_ID;
      a[0].value = certid_tmp;
      a[0].value_len = sizeof (certid_tmp);

      _gnutls_buffer_init (&list->key_ids[current]);

      if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
        {
          _gnutls_buffer_append_data (&list->key_ids[current],
                                      a[0].value, a[0].value_len);
          current++;
        }

      if (current > list->key_ids_size)
        break;
    }

  pakchois_find_objects_final (pks);

  list->key_ids_size = current - 1;

  return 0;
}

/* Recover certificate list from tokens */


static int
find_objs (pakchois_session_t * pks, struct token_info *info,
           struct ck_info *lib_info, void *input)
{
  struct crt_find_data_st *find_data = input;
  struct ck_attribute a[4];
  ck_object_class_t class = -1;
  ck_certificate_type_t type = -1;
  unsigned int trusted;
  ck_rv_t rv;
  ck_object_handle_t obj;
  unsigned long count;
  opaque *cert_data;
  char certid_tmp[PKCS11_ID_SIZE];
  char label_tmp[PKCS11_LABEL_SIZE];
  int ret, i;
  struct pkey_list plist;       /* private key holder */
  int tot_values = 0;

  if (info == NULL)
    {                           /* final call */
      if (find_data->current <= *find_data->n_list)
        ret = 0;
      else
        ret = GNUTLS_E_SHORT_MEMORY_BUFFER;

      *find_data->n_list = find_data->current;

      return ret;
    }

  /* do not bother reading the token if basic fields do not match
   */
  if (pkcs11_token_matches_info (&find_data->info, &info->tinfo, lib_info) <
      0)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (find_data->info.type[0] != 0)
    {
      class = pkcs11_strtype_to_class (find_data->info.type);
      if (class == CKO_CERTIFICATE)
        type = CKC_X_509;
      else
        type = -1;

      if (class == -1)
        {
          gnutls_assert ();
          return GNUTLS_E_INVALID_REQUEST;
        }
    }


  memset (&plist, 0, sizeof (plist));

  if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_CRT_WITH_PRIVKEY)
    {
      ret = find_privkeys (pks, info, &plist);
      if (ret < 0)
        {
          gnutls_assert ();
          return ret;
        }

      if (plist.key_ids_size == 0)
        {
          gnutls_assert ();
          return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
        }
    }

  cert_data = gnutls_malloc (MAX_CERT_SIZE);
  if (cert_data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* Find objects with cert class and X.509 cert type. */

  tot_values = 0;

  if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_CRT_ALL
      || find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_CRT_WITH_PRIVKEY)
    {
      class = CKO_CERTIFICATE;
      type = CKC_X_509;
      trusted = 1;

      a[tot_values].type = CKA_CLASS;
      a[tot_values].value = &class;
      a[tot_values].value_len = sizeof class;
      tot_values++;

      a[tot_values].type = CKA_CERTIFICATE_TYPE;
      a[tot_values].value = &type;
      a[tot_values].value_len = sizeof type;
      tot_values++;

    }
  else if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED)
    {
      class = CKO_CERTIFICATE;
      type = CKC_X_509;
      trusted = 1;

      a[tot_values].type = CKA_CLASS;
      a[tot_values].value = &class;
      a[tot_values].value_len = sizeof class;
      tot_values++;

      a[tot_values].type = CKA_TRUSTED;
      a[tot_values].value = &trusted;
      a[tot_values].value_len = sizeof trusted;
      tot_values++;

    }
  else if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_PUBKEY)
    {
      class = CKO_PUBLIC_KEY;

      a[tot_values].type = CKA_CLASS;
      a[tot_values].value = &class;
      a[tot_values].value_len = sizeof class;
      tot_values++;
    }
  else if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_PRIVKEY)
    {
      class = CKO_PRIVATE_KEY;

      a[tot_values].type = CKA_CLASS;
      a[tot_values].value = &class;
      a[tot_values].value_len = sizeof class;
      tot_values++;
    }
  else if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_ALL)
    {
      if (class != -1)
        {
          a[tot_values].type = CKA_CLASS;
          a[tot_values].value = &class;
          a[tot_values].value_len = sizeof class;
          tot_values++;
        }
      if (type != -1)
        {
          a[tot_values].type = CKA_CERTIFICATE_TYPE;
          a[tot_values].value = &type;
          a[tot_values].value_len = sizeof type;
          tot_values++;
        }
    }
  else
    {
      gnutls_assert ();
      ret = GNUTLS_E_INVALID_REQUEST;
      goto fail;
    }

  if (find_data->info.certid_raw_size != 0)
    {
      a[tot_values].type = CKA_ID;
      a[tot_values].value = find_data->info.certid_raw;
      a[tot_values].value_len = find_data->info.certid_raw_size;
      tot_values++;
    }

  rv = pakchois_find_objects_init (pks, a, tot_values);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      _gnutls_debug_log ("pk11: FindObjectsInit failed.\n");
      return pkcs11_rv_to_err (rv);
    }

  while (pakchois_find_objects (pks, &obj, 1, &count) == CKR_OK && count == 1)
    {
      gnutls_datum_t label, id, value;

      a[0].type = CKA_LABEL;
      a[0].value = label_tmp;
      a[0].value_len = sizeof label_tmp;

      if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
        {
          label.data = a[0].value;
          label.size = a[0].value_len;
        }
      else
        {
          label.data = NULL;
          label.size = 0;
        }

      a[0].type = CKA_ID;
      a[0].value = certid_tmp;
      a[0].value_len = sizeof certid_tmp;

      if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
        {
          id.data = a[0].value;
          id.size = a[0].value_len;
        }
      else
        {
          id.data = NULL;
          id.size = 0;
        }

      a[0].type = CKA_VALUE;
      a[0].value = cert_data;
      a[0].value_len = MAX_CERT_SIZE;
      if (pakchois_get_attribute_value (pks, obj, a, 1) == CKR_OK)
        {
          value.data = a[0].value;
          value.size = a[0].value_len;
        }
      else
        {
          value.data = NULL;
          value.size = 0;
        }

      if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_ALL)
        {
          a[0].type = CKA_CLASS;
          a[0].value = &class;
          a[0].value_len = sizeof class;

          pakchois_get_attribute_value (pks, obj, a, 1);
        }

      if (find_data->flags == GNUTLS_PKCS11_OBJ_ATTR_CRT_WITH_PRIVKEY)
        {
          for (i = 0; i < plist.key_ids_size; i++)
            {
              if (plist.key_ids[i].length !=
                  a[1].value_len
                  || memcmp (plist.key_ids[i].data,
                             a[1].value, a[1].value_len) != 0)
                {
                  /* not found */
                  continue;
                }
            }
        }

      if (find_data->current < *find_data->n_list)
        {
          ret =
            gnutls_pkcs11_obj_init (&find_data->p_list[find_data->current]);
          if (ret < 0)
            {
              gnutls_assert ();
              goto fail;
            }

          if (class == CKO_PUBLIC_KEY)
            {
              ret =
                pkcs11_obj_import_pubkey (pks, obj,
                                          find_data->p_list
                                          [find_data->current],
                                          &id, &label,
                                          &info->tinfo, lib_info);
            }
          else
            {
              ret =
                pkcs11_obj_import (class,
                                   find_data->p_list
                                   [find_data->current],
                                   &value, &id, &label,
                                   &info->tinfo, lib_info);
            }
          if (ret < 0)
            {
              gnutls_assert ();
              goto fail;
            }
        }

      find_data->current++;

    }

  gnutls_free (cert_data);
  pakchois_find_objects_final (pks);

  return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE; /* continue until all tokens have been checked */

fail:
  gnutls_free (cert_data);
  pakchois_find_objects_final (pks);
  if (plist.key_ids != NULL)
    {
      for (i = 0; i < plist.key_ids_size; i++)
        {
          _gnutls_buffer_clear (&plist.key_ids[i]);
        }
      gnutls_free (plist.key_ids);
    }
  for (i = 0; i < find_data->current; i++)
    {
      gnutls_pkcs11_obj_deinit (find_data->p_list[i]);
    }
  find_data->current = 0;

  return ret;
}

/**
 * gnutls_pkcs11_obj_list_import_url:
 * @p_list: An uninitialized object list (may be NULL)
 * @n_list: initially should hold the maximum size of the list. Will contain the actual size.
 * @url: A PKCS 11 url identifying a set of objects
 * @attrs: Attributes of type #gnutls_pkcs11_obj_attr_t that can be used to limit output
 * @flags: One of GNUTLS_PKCS11_OBJ_* flags
 *
 * This function will initialize and set values to an object list
 * by using all objects identified by a PKCS 11 URL.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs11_obj_list_import_url (gnutls_pkcs11_obj_t * p_list,
                                   unsigned int *n_list,
                                   const char *url,
                                   gnutls_pkcs11_obj_attr_t attrs,
                                   unsigned int flags)
{
  int ret;
  struct crt_find_data_st find_data;

  /* fill in the find data structure */
  find_data.p_list = p_list;
  find_data.n_list = n_list;
  find_data.flags = attrs;
  find_data.current = 0;

  if (url == NULL || url[0] == 0)
    {
      url = "pkcs11:";
    }

  ret = pkcs11_url_to_info (url, &find_data.info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret =
    _pkcs11_traverse_tokens (find_objs, &find_data,
                             pkcs11_obj_flags_to_int (flags));
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  return 0;
}

/**
 * gnutls_x509_crt_import_pkcs11_url:
 * @crt: A certificate of type #gnutls_x509_crt_t
 * @url: A PKCS 11 url
 * @flags: One of GNUTLS_PKCS11_OBJ_* flags
 *
 * This function will import a PKCS 11 certificate directly from a token
 * without involving the #gnutls_pkcs11_obj_t structure. This function will
 * fail if the certificate stored is not of X.509 type.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crt_import_pkcs11_url (gnutls_x509_crt_t crt,
                                   const char *url, unsigned int flags)
{
  gnutls_pkcs11_obj_t pcrt;
  int ret;

  ret = gnutls_pkcs11_obj_init (&pcrt);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_pkcs11_obj_import_url (pcrt, url, flags);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = gnutls_x509_crt_import (crt, &pcrt->raw, GNUTLS_X509_FMT_DER);
  if (ret < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  ret = 0;
cleanup:

  gnutls_pkcs11_obj_deinit (pcrt);

  return ret;
}


/**
 * gnutls_x509_crt_import_pkcs11:
 * @crt: A certificate of type #gnutls_x509_crt_t
 * @pkcs11_crt: A PKCS 11 object that contains a certificate
 *
 * This function will import a PKCS 11 certificate to a #gnutls_x509_crt_t
 * structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crt_import_pkcs11 (gnutls_x509_crt_t crt,
                               gnutls_pkcs11_obj_t pkcs11_crt)
{
  return gnutls_x509_crt_import (crt, &pkcs11_crt->raw, GNUTLS_X509_FMT_DER);
}

/**
 * gnutls_x509_crt_list_import_pkcs11:
 * @certs: A list of certificates of type #gnutls_x509_crt_t
 * @cert_max: The maximum size of the list
 * @objs: A list of PKCS 11 objects
 * @flags: 0 for now
 *
 * This function will import a PKCS 11 certificate list to a list of 
 * #gnutls_x509_crt_t structure. These must not be initialized.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_crt_list_import_pkcs11 (gnutls_x509_crt_t * certs,
                                    unsigned int cert_max,
                                    gnutls_pkcs11_obj_t * const objs,
                                    unsigned int flags)
{
  int i, j;
  int ret;

  for (i = 0; i < cert_max; i++)
    {
      ret = gnutls_x509_crt_init (&certs[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      ret = gnutls_x509_crt_import_pkcs11 (certs[i], objs[i]);
      if (ret < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }

  return 0;

cleanup:
  for (j = 0; j < i; j++)
    {
      gnutls_x509_crt_deinit (certs[j]);
    }

  return ret;
}

static int
find_flags (pakchois_session_t * pks, struct token_info *info,
            struct ck_info *lib_info, void *input)
{
  struct flags_find_data_st *find_data = input;

  if (info == NULL)
    {                           /* we don't support multiple calls */
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  /* do not bother reading the token if basic fields do not match
   */
  if (pkcs11_token_matches_info (&find_data->info, &info->tinfo, lib_info) <
      0)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  /* found token! */

  find_data->slot_flags = info->sinfo.flags;

  return 0;
}

/**
 * gnutls_pkcs11_token_get_flags:
 * @url: should contain a PKCS 11 URL
 * @flags: The output flags (GNUTLS_PKCS11_TOKEN_*)
 *
 * This function will return information about the PKCS 11 token flags.
 *
 * Returns: zero on success or a negative value on error.
 **/
int
gnutls_pkcs11_token_get_flags (const char *url, unsigned int *flags)
{
  struct flags_find_data_st find_data;
  int ret;

  ret = pkcs11_url_to_info (url, &find_data.info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _pkcs11_traverse_tokens (find_flags, &find_data, 0);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  *flags = 0;
  if (find_data.slot_flags & CKF_HW_SLOT)
    *flags |= GNUTLS_PKCS11_TOKEN_HW;

  return 0;

}


/**
 * gnutls_pkcs11_token_get_mechanism:
 * @url: should contain a PKCS 11 URL
 * @idx: The index of the mechanism
 * @mechanism: The PKCS #11 mechanism ID
 *
 * This function will return the names of the supported mechanisms
 * by the token. It should be called with an increasing index until
 * it return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE.
 *
 * Returns: zero on success or a negative value on error.
 **/
int
gnutls_pkcs11_token_get_mechanism (const char *url, int idx,
                                   unsigned long *mechanism)
{
  int ret;
  ck_rv_t rv;
  pakchois_module_t *module;
  ck_slot_id_t slot;
  struct token_info tinfo;
  struct pkcs11_url_info info;
  unsigned long count;
  ck_mechanism_type_t mlist[400];

  ret = pkcs11_url_to_info (url, &info);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }


  ret = pkcs11_find_slot (&module, &slot, &info, &tinfo);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  count = sizeof (mlist) / sizeof (mlist[0]);
  rv = pakchois_get_mechanism_list (module, slot, mlist, &count);
  if (rv != CKR_OK)
    {
      gnutls_assert ();
      return pkcs11_rv_to_err (rv);
    }

  if (idx >= count)
    {
      gnutls_assert ();
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  *mechanism = mlist[idx];

  return 0;

}


const char *
gnutls_pkcs11_type_get_name (gnutls_pkcs11_obj_type_t type)
{
  switch (type)
    {
    case GNUTLS_PKCS11_OBJ_X509_CRT:
      return "X.509 Certificate";
    case GNUTLS_PKCS11_OBJ_PUBKEY:
      return "Public key";
    case GNUTLS_PKCS11_OBJ_PRIVKEY:
      return "Private key";
    case GNUTLS_PKCS11_OBJ_SECRET_KEY:
      return "Secret key";
    case GNUTLS_PKCS11_OBJ_DATA:
      return "Data";
    case GNUTLS_PKCS11_OBJ_UNKNOWN:
    default:
      return "Unknown";
    }
}

int
pkcs11_token_matches_info (struct pkcs11_url_info *info,
                           struct ck_token_info *tinfo,
                           struct ck_info *lib_info)
{
  if (info->manufacturer[0] != 0)
    {
      if (strcmp (info->manufacturer, tinfo->manufacturer_id) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (info->token[0] != 0)
    {
      if (strcmp (info->token, tinfo->label) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (info->model[0] != 0)
    {
      if (strcmp (info->model, tinfo->model) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (info->serial[0] != 0)
    {
      if (strcmp (info->serial, tinfo->serial_number) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (info->lib_manufacturer[0] != 0)
    {
      if (strcmp (info->lib_manufacturer, lib_info->manufacturer_id) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (info->lib_desc[0] != 0)
    {
      if (strcmp (info->lib_desc, lib_info->library_description) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  if (info->lib_version[0] != 0)
    {
      char version[16];

      snprintf (version, sizeof (version), "%u.%u",
                (unsigned int) lib_info->library_version.major,
                (unsigned int) lib_info->library_version.minor);
      if (strcmp (info->lib_version, version) != 0)
        return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }

  return 0;
}
