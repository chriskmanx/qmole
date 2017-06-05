/*
 * Copyright (C) 2003, 2004, 2005, 2008, 2010 Free Software Foundation,
 * Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

/* Functions that relate on PKCS12 packet parsing.
 */

#include <gnutls_int.h>
#include <libtasn1.h>

#ifdef ENABLE_PKI

#include <gnutls_datum.h>
#include <gnutls_global.h>
#include <gnutls_errors.h>
#include <gnutls_num.h>
#include <common.h>
#include <x509_b64.h>
#include "x509_int.h"
#include <random.h>


/* Decodes the PKCS #12 auth_safe, and returns the allocated raw data,
 * which holds them. Returns an ASN1_TYPE of authenticatedSafe.
 */
static int
_decode_pkcs12_auth_safe (ASN1_TYPE pkcs12, ASN1_TYPE * authen_safe,
                          gnutls_datum_t * raw)
{
  char oid[MAX_OID_SIZE];
  ASN1_TYPE c2 = ASN1_TYPE_EMPTY;
  gnutls_datum_t auth_safe = { NULL, 0 };
  int len, result;
  char error_str[ASN1_MAX_ERROR_DESCRIPTION_SIZE];

  len = sizeof (oid) - 1;
  result = asn1_read_value (pkcs12, "authSafe.contentType", oid, &len);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  if (strcmp (oid, DATA_OID) != 0)
    {
      gnutls_assert ();
      _gnutls_x509_log ("Unknown PKCS12 Content OID '%s'\n", oid);
      return GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE;
    }

  /* Step 1. Read the content data
   */

  result =
    _gnutls_x509_read_value (pkcs12, "authSafe.content", &auth_safe, 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* Step 2. Extract the authenticatedSafe.
   */

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.pkcs-12-AuthenticatedSafe",
        &c2)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result = asn1_der_decoding (&c2, auth_safe.data, auth_safe.size, error_str);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      _gnutls_x509_log ("DER error: %s\n", error_str);
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if (raw == NULL)
    {
      _gnutls_free_datum (&auth_safe);
    }
  else
    {
      raw->data = auth_safe.data;
      raw->size = auth_safe.size;
    }

  if (authen_safe)
    *authen_safe = c2;
  else
    asn1_delete_structure (&c2);

  return 0;

cleanup:
  if (c2)
    asn1_delete_structure (&c2);
  _gnutls_free_datum (&auth_safe);
  return result;
}

/**
 * gnutls_pkcs12_init:
 * @pkcs12: The structure to be initialized
 *
 * This function will initialize a PKCS12 structure. PKCS12 structures
 * usually contain lists of X.509 Certificates and X.509 Certificate
 * revocation lists.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs12_init (gnutls_pkcs12_t * pkcs12)
{
  *pkcs12 = gnutls_calloc (1, sizeof (gnutls_pkcs12_int));

  if (*pkcs12)
    {
      int result = asn1_create_element (_gnutls_get_pkix (),
                                        "PKIX1.pkcs-12-PFX",
                                        &(*pkcs12)->pkcs12);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          gnutls_free (*pkcs12);
          return _gnutls_asn2err (result);
        }
      return 0;                 /* success */
    }
  return GNUTLS_E_MEMORY_ERROR;
}

/**
 * gnutls_pkcs12_deinit:
 * @pkcs12: The structure to be initialized
 *
 * This function will deinitialize a PKCS12 structure.
 **/
void
gnutls_pkcs12_deinit (gnutls_pkcs12_t pkcs12)
{
  if (!pkcs12)
    return;

  if (pkcs12->pkcs12)
    asn1_delete_structure (&pkcs12->pkcs12);

  gnutls_free (pkcs12);
}

/**
 * gnutls_pkcs12_import:
 * @pkcs12: The structure to store the parsed PKCS12.
 * @data: The DER or PEM encoded PKCS12.
 * @format: One of DER or PEM
 * @flags: an ORed sequence of gnutls_privkey_pkcs8_flags
 *
 * This function will convert the given DER or PEM encoded PKCS12
 * to the native gnutls_pkcs12_t format. The output will be stored in 'pkcs12'.
 *
 * If the PKCS12 is PEM encoded it should have a header of "PKCS12".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs12_import (gnutls_pkcs12_t pkcs12,
                      const gnutls_datum_t * data,
                      gnutls_x509_crt_fmt_t format, unsigned int flags)
{
  int result = 0, need_free = 0;
  gnutls_datum_t _data;
  char error_str[ASN1_MAX_ERROR_DESCRIPTION_SIZE];

  _data.data = data->data;
  _data.size = data->size;

  if (pkcs12 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* If the PKCS12 is in PEM format then decode it
   */
  if (format == GNUTLS_X509_FMT_PEM)
    {
      opaque *out;

      result = _gnutls_fbase64_decode (PEM_PKCS12, data->data, data->size,
                                       &out);

      if (result <= 0)
        {
          if (result == 0)
            result = GNUTLS_E_INTERNAL_ERROR;
          gnutls_assert ();
          return result;
        }

      _data.data = out;
      _data.size = result;

      need_free = 1;
    }

  result =
    asn1_der_decoding (&pkcs12->pkcs12, _data.data, _data.size, error_str);
  if (result != ASN1_SUCCESS)
    {
      result = _gnutls_asn2err (result);
      _gnutls_x509_log ("DER error: %s\n", error_str);
      gnutls_assert ();
      goto cleanup;
    }

  if (need_free)
    _gnutls_free_datum (&_data);

  return 0;

cleanup:
  if (need_free)
    _gnutls_free_datum (&_data);
  return result;
}


/**
 * gnutls_pkcs12_export:
 * @pkcs12: Holds the pkcs12 structure
 * @format: the format of output params. One of PEM or DER.
 * @output_data: will contain a structure PEM or DER encoded
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will export the pkcs12 structure to DER or PEM format.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *output_data_size will be updated and GNUTLS_E_SHORT_MEMORY_BUFFER
 * will be returned.
 *
 * If the structure is PEM encoded, it will have a header
 * of "BEGIN PKCS12".
 *
 * Return value: In case of failure a negative value will be
 *   returned, and 0 on success.
 **/
int
gnutls_pkcs12_export (gnutls_pkcs12_t pkcs12,
                      gnutls_x509_crt_fmt_t format, void *output_data,
                      size_t * output_data_size)
{
  if (pkcs12 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return _gnutls_x509_export_int (pkcs12->pkcs12, format, PEM_PKCS12,
                                  output_data, output_data_size);
}

static int
oid2bag (const char *oid)
{
  if (strcmp (oid, BAG_PKCS8_KEY) == 0)
    return GNUTLS_BAG_PKCS8_KEY;
  if (strcmp (oid, BAG_PKCS8_ENCRYPTED_KEY) == 0)
    return GNUTLS_BAG_PKCS8_ENCRYPTED_KEY;
  if (strcmp (oid, BAG_CERTIFICATE) == 0)
    return GNUTLS_BAG_CERTIFICATE;
  if (strcmp (oid, BAG_CRL) == 0)
    return GNUTLS_BAG_CRL;
  if (strcmp (oid, BAG_SECRET) == 0)
    return GNUTLS_BAG_SECRET;

  return GNUTLS_BAG_UNKNOWN;
}

static const char *
bag_to_oid (int bag)
{
  switch (bag)
    {
    case GNUTLS_BAG_PKCS8_KEY:
      return BAG_PKCS8_KEY;
    case GNUTLS_BAG_PKCS8_ENCRYPTED_KEY:
      return BAG_PKCS8_ENCRYPTED_KEY;
    case GNUTLS_BAG_CERTIFICATE:
      return BAG_CERTIFICATE;
    case GNUTLS_BAG_CRL:
      return BAG_CRL;
    case GNUTLS_BAG_SECRET:
      return BAG_SECRET;
    }
  return NULL;
}

static inline char *
ucs2_to_ascii (char *data, int size)
{
  int i, j;

  for (i = 0; i < size / 2; i++)
    {
      j = 2 * i + 1;
      if (isascii (data[j]))
        data[i] = data[i * 2 + 1];
      else
        data[i] = '?';
    }
  data[i] = 0;

  return data;
}

/* Decodes the SafeContents, and puts the output in
 * the given bag. 
 */
int
_pkcs12_decode_safe_contents (const gnutls_datum_t * content,
                              gnutls_pkcs12_bag_t bag)
{
  char oid[MAX_OID_SIZE], root[ASN1_MAX_NAME_SIZE];
  ASN1_TYPE c2 = ASN1_TYPE_EMPTY;
  int len, result;
  int bag_type;
  gnutls_datum_t attr_val;
  int count = 0, i, attributes, j;
  size_t size;

  /* Step 1. Extract the SEQUENCE.
   */

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.pkcs-12-SafeContents",
        &c2)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result = asn1_der_decoding (&c2, content->data, content->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Count the number of bags
   */
  result = asn1_number_of_elements (c2, "", &count);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  bag->bag_elements = MIN (MAX_BAG_ELEMENTS, count);

  for (i = 0; i < bag->bag_elements; i++)
    {

      snprintf (root, sizeof (root), "?%u.bagId", i + 1);

      len = sizeof (oid);
      result = asn1_read_value (c2, root, oid, &len);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      /* Read the Bag type
       */
      bag_type = oid2bag (oid);

      if (bag_type < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      /* Read the Bag Value
       */

      snprintf (root, sizeof (root), "?%u.bagValue", i + 1);

      result = _gnutls_x509_read_value (c2, root, &bag->element[i].data, 0);
      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

      if (bag_type == GNUTLS_BAG_CERTIFICATE || bag_type == GNUTLS_BAG_CRL
          || bag_type == GNUTLS_BAG_SECRET)
        {
          gnutls_datum_t tmp = bag->element[i].data;

          result =
            _pkcs12_decode_crt_bag (bag_type, &tmp, &bag->element[i].data);
          if (result < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }

          _gnutls_free_datum (&tmp);
        }

      /* read the bag attributes
       */
      snprintf (root, sizeof (root), "?%u.bagAttributes", i + 1);

      result = asn1_number_of_elements (c2, root, &attributes);
      if (result != ASN1_SUCCESS && result != ASN1_ELEMENT_NOT_FOUND)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      if (attributes < 0)
        attributes = 1;

      if (result != ASN1_ELEMENT_NOT_FOUND)
        for (j = 0; j < attributes; j++)
          {

            snprintf (root, sizeof (root), "?%u.bagAttributes.?%u", i + 1,
                      j + 1);

            result =
              _gnutls_x509_decode_and_read_attribute (c2, root, oid,
                                                      sizeof (oid), &attr_val,
                                                      1, 0);

            if (result < 0)
              {
                gnutls_assert ();
                continue;       /* continue in case we find some known attributes */
              }

            if (strcmp (oid, KEY_ID_OID) == 0)
              {
                size = attr_val.size;

                result =
                  _gnutls_x509_decode_octet_string (NULL, attr_val.data, size,
                                                    attr_val.data, &size);
                attr_val.size = size;
                if (result < 0)
                  {
                    _gnutls_free_datum (&attr_val);
                    gnutls_assert ();
                    _gnutls_x509_log
                      ("Error decoding PKCS12 Bag Attribute OID '%s'\n", oid);
                    continue;
                  }
                bag->element[i].local_key_id = attr_val;
              }
            else if (strcmp (oid, FRIENDLY_NAME_OID) == 0)
              {
                size = attr_val.size;
                result =
                  _gnutls_x509_decode_octet_string ("BMPString",
                                                    attr_val.data, size,
                                                    attr_val.data, &size);
                attr_val.size = size;
                if (result < 0)
                  {
                    _gnutls_free_datum (&attr_val);
                    gnutls_assert ();
                    _gnutls_x509_log
                      ("Error decoding PKCS12 Bag Attribute OID '%s'\n", oid);
                    continue;
                  }
                bag->element[i].friendly_name =
                  ucs2_to_ascii (attr_val.data, attr_val.size);
              }
            else
              {
                _gnutls_free_datum (&attr_val);
                _gnutls_x509_log
                  ("Unknown PKCS12 Bag Attribute OID '%s'\n", oid);
              }
          }


      bag->element[i].type = bag_type;

    }

  asn1_delete_structure (&c2);


  return 0;

cleanup:
  if (c2)
    asn1_delete_structure (&c2);
  return result;

}


static int
_parse_safe_contents (ASN1_TYPE sc, const char *sc_name,
                      gnutls_pkcs12_bag_t bag)
{
  gnutls_datum_t content = { NULL, 0 };
  int result;

  /* Step 1. Extract the content.
   */

  result = _gnutls_x509_read_value (sc, sc_name, &content, 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _pkcs12_decode_safe_contents (&content, bag);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  _gnutls_free_datum (&content);

  return 0;

cleanup:
  _gnutls_free_datum (&content);
  return result;
}


/**
 * gnutls_pkcs12_get_bag:
 * @pkcs12: should contain a gnutls_pkcs12_t structure
 * @indx: contains the index of the bag to extract
 * @bag: An initialized bag, where the contents of the bag will be copied
 *
 * This function will return a Bag from the PKCS12 structure.
 *
 * After the last Bag has been read
 * %GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE will be returned.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs12_get_bag (gnutls_pkcs12_t pkcs12,
                       int indx, gnutls_pkcs12_bag_t bag)
{
  ASN1_TYPE c2 = ASN1_TYPE_EMPTY;
  int result, len;
  char root2[ASN1_MAX_NAME_SIZE];
  char oid[MAX_OID_SIZE];

  if (pkcs12 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* Step 1. decode the data.
   */
  result = _decode_pkcs12_auth_safe (pkcs12->pkcs12, &c2, NULL);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  /* Step 2. Parse the AuthenticatedSafe
   */

  snprintf (root2, sizeof (root2), "?%u.contentType", indx + 1);

  len = sizeof (oid) - 1;
  result = asn1_read_value (c2, root2, oid, &len);

  if (result == ASN1_ELEMENT_NOT_FOUND)
    {
      result = GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
      goto cleanup;
    }

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Not encrypted Bag
   */

  snprintf (root2, sizeof (root2), "?%u.content", indx + 1);

  if (strcmp (oid, DATA_OID) == 0)
    {
      result = _parse_safe_contents (c2, root2, bag);
      goto cleanup;
    }

  /* ENC_DATA_OID needs decryption */

  bag->element[0].type = GNUTLS_BAG_ENCRYPTED;
  bag->bag_elements = 1;

  result = _gnutls_x509_read_value (c2, root2, &bag->element[0].data, 0);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = 0;

cleanup:
  if (c2)
    asn1_delete_structure (&c2);
  return result;
}

/* Creates an empty PFX structure for the PKCS12 structure.
 */
static int
create_empty_pfx (ASN1_TYPE pkcs12)
{
  uint8_t three = 3;
  int result;
  ASN1_TYPE c2 = ASN1_TYPE_EMPTY;

  /* Use version 3
   */
  result = asn1_write_value (pkcs12, "version", &three, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Write the content type of the data
   */
  result = asn1_write_value (pkcs12, "authSafe.contentType", DATA_OID, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Check if the authenticatedSafe content is empty, and encode a
   * null one in that case.
   */

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.pkcs-12-AuthenticatedSafe",
        &c2)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result =
    _gnutls_x509_der_encode_and_copy (c2, "", pkcs12, "authSafe.content", 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }
  asn1_delete_structure (&c2);

  return 0;

cleanup:
  asn1_delete_structure (&c2);
  return result;

}

/**
 * gnutls_pkcs12_set_bag:
 * @pkcs12: should contain a gnutls_pkcs12_t structure
 * @bag: An initialized bag
 *
 * This function will insert a Bag into the PKCS12 structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs12_set_bag (gnutls_pkcs12_t pkcs12, gnutls_pkcs12_bag_t bag)
{
  ASN1_TYPE c2 = ASN1_TYPE_EMPTY;
  ASN1_TYPE safe_cont = ASN1_TYPE_EMPTY;
  int result;
  int enc = 0, dum = 1;
  char null;

  if (pkcs12 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* Step 1. Check if the pkcs12 structure is empty. In that
   * case generate an empty PFX.
   */
  result = asn1_read_value (pkcs12->pkcs12, "authSafe.content", &null, &dum);
  if (result == ASN1_VALUE_NOT_FOUND)
    {
      result = create_empty_pfx (pkcs12->pkcs12);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }
    }

  /* Step 2. decode the authenticatedSafe.
   */
  result = _decode_pkcs12_auth_safe (pkcs12->pkcs12, &c2, NULL);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  /* Step 3. Encode the bag elements into a SafeContents 
   * structure.
   */
  result = _pkcs12_encode_safe_contents (bag, &safe_cont, &enc);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  /* Step 4. Insert the encoded SafeContents into the AuthenticatedSafe
   * structure.
   */
  result = asn1_write_value (c2, "", "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if (enc)
    result = asn1_write_value (c2, "?LAST.contentType", ENC_DATA_OID, 1);
  else
    result = asn1_write_value (c2, "?LAST.contentType", DATA_OID, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if (enc)
    {
      /* Encrypted packets are written directly.
       */
      result =
        asn1_write_value (c2, "?LAST.content",
                          bag->element[0].data.data,
                          bag->element[0].data.size);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }
    }
  else
    {
      result =
        _gnutls_x509_der_encode_and_copy (safe_cont, "", c2,
                                          "?LAST.content", 1);
      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }

  asn1_delete_structure (&safe_cont);


  /* Step 5. Reencode and copy the AuthenticatedSafe into the pkcs12
   * structure.
   */
  result =
    _gnutls_x509_der_encode_and_copy (c2, "", pkcs12->pkcs12,
                                      "authSafe.content", 1);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  asn1_delete_structure (&c2);

  return 0;

cleanup:
  asn1_delete_structure (&c2);
  asn1_delete_structure (&safe_cont);
  return result;
}

/**
 * gnutls_pkcs12_generate_mac:
 * @pkcs12: should contain a gnutls_pkcs12_t structure
 * @pass: The password for the MAC
 *
 * This function will generate a MAC for the PKCS12 structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs12_generate_mac (gnutls_pkcs12_t pkcs12, const char *pass)
{
  opaque salt[8], key[20];
  int result;
  const int iter = 1;
  digest_hd_st td1;
  gnutls_datum_t tmp = { NULL, 0 };
  opaque sha_mac[20];

  if (pkcs12 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* Generate the salt.
   */
  result = _gnutls_rnd (GNUTLS_RND_NONCE, salt, sizeof (salt));
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  /* Write the salt into the structure.
   */
  result =
    asn1_write_value (pkcs12->pkcs12, "macData.macSalt", salt, sizeof (salt));
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* write the iterations
   */

  if (iter > 1)
    {
      result =
        _gnutls_x509_write_uint32 (pkcs12->pkcs12, "macData.iterations",
                                   iter);
      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }
    }

  /* Generate the key.
   */
  result = _gnutls_pkcs12_string_to_key (3 /*MAC*/, salt, sizeof (salt),
                                         iter, pass, sizeof (key), key);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* Get the data to be MACed
   */
  result = _decode_pkcs12_auth_safe (pkcs12->pkcs12, NULL, &tmp);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* MAC the data
   */
  result = _gnutls_hmac_init (&td1, GNUTLS_MAC_SHA1, key, sizeof (key));
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  _gnutls_hmac (&td1, tmp.data, tmp.size);
  _gnutls_free_datum (&tmp);

  _gnutls_hmac_deinit (&td1, sha_mac);


  result =
    asn1_write_value (pkcs12->pkcs12, "macData.mac.digest", sha_mac,
                      sizeof (sha_mac));
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result =
    asn1_write_value (pkcs12->pkcs12,
                      "macData.mac.digestAlgorithm.parameters", NULL, 0);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result =
    asn1_write_value (pkcs12->pkcs12,
                      "macData.mac.digestAlgorithm.algorithm", HASH_OID_SHA1,
                      1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  return 0;

cleanup:
  _gnutls_free_datum (&tmp);
  return result;
}

/**
 * gnutls_pkcs12_verify_mac:
 * @pkcs12: should contain a gnutls_pkcs12_t structure
 * @pass: The password for the MAC
 *
 * This function will verify the MAC for the PKCS12 structure.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_pkcs12_verify_mac (gnutls_pkcs12_t pkcs12, const char *pass)
{
  opaque key[20];
  int result;
  unsigned int iter;
  int len;
  digest_hd_st td1;
  gnutls_datum_t tmp = { NULL, 0 }, salt =
  {
  NULL, 0};
  opaque sha_mac[20];
  opaque sha_mac_orig[20];

  if (pkcs12 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* read the iterations
   */

  result =
    _gnutls_x509_read_uint (pkcs12->pkcs12, "macData.iterations", &iter);
  if (result < 0)
    {
      iter = 1;                 /* the default */
    }


  /* Read the salt from the structure.
   */
  result =
    _gnutls_x509_read_value (pkcs12->pkcs12, "macData.macSalt", &salt, 0);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* Generate the key.
   */
  result = _gnutls_pkcs12_string_to_key (3 /*MAC*/, salt.data, salt.size,
                                         iter, pass, sizeof (key), key);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  _gnutls_free_datum (&salt);

  /* Get the data to be MACed
   */
  result = _decode_pkcs12_auth_safe (pkcs12->pkcs12, NULL, &tmp);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  /* MAC the data
   */
  result = _gnutls_hmac_init (&td1, GNUTLS_MAC_SHA1, key, sizeof (key));
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  _gnutls_hmac (&td1, tmp.data, tmp.size);
  _gnutls_free_datum (&tmp);

  _gnutls_hmac_deinit (&td1, sha_mac);

  len = sizeof (sha_mac_orig);
  result =
    asn1_read_value (pkcs12->pkcs12, "macData.mac.digest", sha_mac_orig,
                     &len);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if (memcmp (sha_mac_orig, sha_mac, sizeof (sha_mac)) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_MAC_VERIFY_FAILED;
    }

  return 0;

cleanup:
  _gnutls_free_datum (&tmp);
  _gnutls_free_datum (&salt);
  return result;
}


static int
write_attributes (gnutls_pkcs12_bag_t bag, int elem,
                  ASN1_TYPE c2, const char *where)
{
  int result;
  char root[128];

  /* If the bag attributes are empty, then write
   * nothing to the attribute field.
   */
  if (bag->element[elem].friendly_name == NULL &&
      bag->element[elem].local_key_id.data == NULL)
    {
      /* no attributes
       */
      result = asn1_write_value (c2, where, NULL, 0);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          return _gnutls_asn2err (result);
        }

      return 0;
    }

  if (bag->element[elem].local_key_id.data != NULL)
    {

      /* Add a new Attribute
       */
      result = asn1_write_value (c2, where, "NEW", 1);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          return _gnutls_asn2err (result);
        }

      _gnutls_str_cpy (root, sizeof (root), where);
      _gnutls_str_cat (root, sizeof (root), ".?LAST");

      result =
        _gnutls_x509_encode_and_write_attribute (KEY_ID_OID, c2, root,
                                                 bag->
                                                 element[elem].local_key_id.
                                                 data,
                                                 bag->
                                                 element[elem].local_key_id.
                                                 size, 1);
      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }
    }

  if (bag->element[elem].friendly_name != NULL)
    {
      opaque *name;
      int size, i;
      const char *p;

      /* Add a new Attribute
       */
      result = asn1_write_value (c2, where, "NEW", 1);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          return _gnutls_asn2err (result);
        }

      /* convert name to BMPString
       */
      size = strlen (bag->element[elem].friendly_name) * 2;
      name = gnutls_malloc (size);

      if (name == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      p = bag->element[elem].friendly_name;
      for (i = 0; i < size; i += 2)
        {
          name[i] = 0;
          name[i + 1] = *p;
          p++;
        }

      _gnutls_str_cpy (root, sizeof (root), where);
      _gnutls_str_cat (root, sizeof (root), ".?LAST");

      result =
        _gnutls_x509_encode_and_write_attribute (FRIENDLY_NAME_OID, c2,
                                                 root, name, size, 1);

      gnutls_free (name);

      if (result < 0)
        {
          gnutls_assert ();
          return result;
        }
    }

  return 0;
}


/* Encodes the bag into a SafeContents structure, and puts the output in
 * the given datum. Enc is set to non zero if the data are encrypted;
 */
int
_pkcs12_encode_safe_contents (gnutls_pkcs12_bag_t bag, ASN1_TYPE * contents,
                              int *enc)
{
  ASN1_TYPE c2 = ASN1_TYPE_EMPTY;
  int result;
  int i;
  const char *oid;

  if (bag->element[0].type == GNUTLS_BAG_ENCRYPTED && enc)
    {
      *enc = 1;
      return 0;                 /* ENCRYPTED BAG, do nothing. */
    }
  else if (enc)
    *enc = 0;

  /* Step 1. Create the SEQUENCE.
   */

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.pkcs-12-SafeContents",
        &c2)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  for (i = 0; i < bag->bag_elements; i++)
    {

      oid = bag_to_oid (bag->element[i].type);
      if (oid == NULL)
        {
          gnutls_assert ();
          continue;
        }

      result = asn1_write_value (c2, "", "NEW", 1);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      /* Copy the bag type.
       */
      result = asn1_write_value (c2, "?LAST.bagId", oid, 1);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      /* Set empty attributes
       */
      result = write_attributes (bag, i, c2, "?LAST.bagAttributes");
      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }


      /* Copy the Bag Value
       */

      if (bag->element[i].type == GNUTLS_BAG_CERTIFICATE ||
          bag->element[i].type == GNUTLS_BAG_SECRET ||
          bag->element[i].type == GNUTLS_BAG_CRL)
        {
          gnutls_datum_t tmp;

          /* in that case encode it to a CertBag or
           * a CrlBag.
           */

          result =
            _pkcs12_encode_crt_bag (bag->element[i].type,
                                    &bag->element[i].data, &tmp);

          if (result < 0)
            {
              gnutls_assert ();
              goto cleanup;
            }

          result = _gnutls_x509_write_value (c2, "?LAST.bagValue", &tmp, 0);

          _gnutls_free_datum (&tmp);

        }
      else
        {

          result = _gnutls_x509_write_value (c2, "?LAST.bagValue",
                                             &bag->element[i].data, 0);
        }

      if (result < 0)
        {
          gnutls_assert ();
          goto cleanup;
        }

    }

  /* Encode the data and copy them into the datum
   */
  *contents = c2;

  return 0;

cleanup:
  if (c2)
    asn1_delete_structure (&c2);
  return result;

}


#endif /* ENABLE_PKI */
