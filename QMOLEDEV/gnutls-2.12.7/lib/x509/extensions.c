/*
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
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

/* Functions that relate to the X.509 extension parsing.
 */

#include <gnutls_int.h>
#include <gnutls_errors.h>
#include <gnutls_global.h>
#include <libtasn1.h>
#include <common.h>
#include <x509_int.h>
#include <gnutls_datum.h>

static int
get_extension (ASN1_TYPE asn, const char *root,
               const char *extension_id, int indx,
               gnutls_datum_t * ret, unsigned int *_critical)
{
  int k, result, len;
  char name[ASN1_MAX_NAME_SIZE], name2[ASN1_MAX_NAME_SIZE];
  char str[1024];
  char str_critical[10];
  int critical = 0;
  char extnID[128];
  gnutls_datum_t value;
  int indx_counter = 0;

  ret->data = NULL;
  ret->size = 0;

  k = 0;
  do
    {
      k++;

      snprintf (name, sizeof (name), "%s.?%u", root, k);

      len = sizeof (str) - 1;
      result = asn1_read_value (asn, name, str, &len);

      /* move to next
       */

      if (result == ASN1_ELEMENT_NOT_FOUND)
        {
          break;
        }

      do
        {

          _gnutls_str_cpy (name2, sizeof (name2), name);
          _gnutls_str_cat (name2, sizeof (name2), ".extnID");

          len = sizeof (extnID) - 1;
          result = asn1_read_value (asn, name2, extnID, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            {
              gnutls_assert ();
              break;
            }
          else if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              return _gnutls_asn2err (result);
            }

          /* Handle Extension 
           */
          if (strcmp (extnID, extension_id) == 0 && indx == indx_counter++)
            {
              /* extension was found 
               */

              /* read the critical status.
               */
              _gnutls_str_cpy (name2, sizeof (name2), name);
              _gnutls_str_cat (name2, sizeof (name2), ".critical");

              len = sizeof (str_critical);
              result = asn1_read_value (asn, name2, str_critical, &len);

              if (result == ASN1_ELEMENT_NOT_FOUND)
                {
                  gnutls_assert ();
                  break;
                }
              else if (result != ASN1_SUCCESS)
                {
                  gnutls_assert ();
                  return _gnutls_asn2err (result);
                }

              if (str_critical[0] == 'T')
                critical = 1;
              else
                critical = 0;

              /* read the value.
               */
              _gnutls_str_cpy (name2, sizeof (name2), name);
              _gnutls_str_cat (name2, sizeof (name2), ".extnValue");

              result = _gnutls_x509_read_value (asn, name2, &value, 0);
              if (result < 0)
                {
                  gnutls_assert ();
                  return result;
                }

              ret->data = value.data;
              ret->size = value.size;

              if (_critical)
                *_critical = critical;

              return 0;
            }


        }
      while (0);
    }
  while (1);

  if (result == ASN1_ELEMENT_NOT_FOUND)
    {
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }
  else
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }
}

/* This function will attempt to return the requested extension found in
 * the given X509v3 certificate. The return value is allocated and stored into
 * ret.
 *
 * Critical will be either 0 or 1.
 *
 * If the extension does not exist, GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE will
 * be returned.
 */
int
_gnutls_x509_crt_get_extension (gnutls_x509_crt_t cert,
                                const char *extension_id, int indx,
                                gnutls_datum_t * ret, unsigned int *_critical)
{
  return get_extension (cert->cert, "tbsCertificate.extensions", extension_id,
                        indx, ret, _critical);
}

int
_gnutls_x509_crl_get_extension (gnutls_x509_crl_t crl,
                                const char *extension_id, int indx,
                                gnutls_datum_t * ret, unsigned int *_critical)
{
  return get_extension (crl->crl, "tbsCertList.crlExtensions", extension_id,
                        indx, ret, _critical);
}


/* This function will attempt to return the requested extension OID found in
 * the given X509v3 certificate. 
 *
 * If you have passed the last extension, GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE will
 * be returned.
 */
static int
get_extension_oid (ASN1_TYPE asn, const char *root,
                   int indx, void *oid, size_t * sizeof_oid)
{
  int k, result, len;
  char name[ASN1_MAX_NAME_SIZE], name2[ASN1_MAX_NAME_SIZE];
  char str[1024];
  char extnID[128];
  int indx_counter = 0;

  k = 0;
  do
    {
      k++;

      snprintf (name, sizeof (name), "%s.?%u", root, k);

      len = sizeof (str) - 1;
      result = asn1_read_value (asn, name, str, &len);

      /* move to next
       */

      if (result == ASN1_ELEMENT_NOT_FOUND)
        {
          break;
        }

      do
        {

          _gnutls_str_cpy (name2, sizeof (name2), name);
          _gnutls_str_cat (name2, sizeof (name2), ".extnID");

          len = sizeof (extnID) - 1;
          result = asn1_read_value (asn, name2, extnID, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            {
              gnutls_assert ();
              break;
            }
          else if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              return _gnutls_asn2err (result);
            }

          /* Handle Extension 
           */
          if (indx == indx_counter++)
            {
              len = strlen (extnID) + 1;

              if (*sizeof_oid < (unsigned) len)
                {
                  *sizeof_oid = len;
                  gnutls_assert ();
                  return GNUTLS_E_SHORT_MEMORY_BUFFER;
                }

              memcpy (oid, extnID, len);
              *sizeof_oid = len - 1;

              return 0;
            }


        }
      while (0);
    }
  while (1);

  if (result == ASN1_ELEMENT_NOT_FOUND)
    {
      return GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;
    }
  else
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }
}

/* This function will attempt to return the requested extension OID found in
 * the given X509v3 certificate. 
 *
 * If you have passed the last extension, GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE will
 * be returned.
 */
int
_gnutls_x509_crt_get_extension_oid (gnutls_x509_crt_t cert,
                                    int indx, void *oid, size_t * sizeof_oid)
{
  return get_extension_oid (cert->cert, "tbsCertificate.extensions", indx,
                            oid, sizeof_oid);
}

int
_gnutls_x509_crl_get_extension_oid (gnutls_x509_crl_t crl,
                                    int indx, void *oid, size_t * sizeof_oid)
{
  return get_extension_oid (crl->crl, "tbsCertList.crlExtensions", indx, oid,
                            sizeof_oid);
}

/* This function will attempt to set the requested extension in
 * the given X509v3 certificate. 
 *
 * Critical will be either 0 or 1.
 */
static int
add_extension (ASN1_TYPE asn, const char *root, const char *extension_id,
               const gnutls_datum_t * ext_data, unsigned int critical)
{
  int result;
  const char *str;
  char name[ASN1_MAX_NAME_SIZE];

  snprintf (name, sizeof (name), "%s", root);

  /* Add a new extension in the list.
   */
  result = asn1_write_value (asn, name, "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  if (root[0] != 0)
    snprintf (name, sizeof (name), "%s.?LAST.extnID", root);
  else
    snprintf (name, sizeof (name), "?LAST.extnID");

  result = asn1_write_value (asn, name, extension_id, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  if (critical == 0)
    str = "FALSE";
  else
    str = "TRUE";

  if (root[0] != 0)
    snprintf (name, sizeof (name), "%s.?LAST.critical", root);
  else
    snprintf (name, sizeof (name), "?LAST.critical");

  result = asn1_write_value (asn, name, str, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  if (root[0] != 0)
    snprintf (name, sizeof (name), "%s.?LAST.extnValue", root);
  else
    snprintf (name, sizeof (name), "?LAST.extnValue");

  result = _gnutls_x509_write_value (asn, name, ext_data, 0);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/* Overwrite the given extension (using the index)
 * index here starts from one.
 */
static int
overwrite_extension (ASN1_TYPE asn, const char *root, unsigned int indx,
                     const gnutls_datum_t * ext_data, unsigned int critical)
{
  char name[ASN1_MAX_NAME_SIZE], name2[ASN1_MAX_NAME_SIZE];
  const char *str;
  int result;

  if (root[0] != 0)
    snprintf (name, sizeof (name), "%s.?%u", root, indx);
  else
    snprintf (name, sizeof (name), "?%u", indx);

  if (critical == 0)
    str = "FALSE";
  else
    str = "TRUE";

  _gnutls_str_cpy (name2, sizeof (name2), name);
  _gnutls_str_cat (name2, sizeof (name2), ".critical");

  result = asn1_write_value (asn, name2, str, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  _gnutls_str_cpy (name2, sizeof (name2), name);
  _gnutls_str_cat (name2, sizeof (name2), ".extnValue");

  result = _gnutls_x509_write_value (asn, name2, ext_data, 0);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

static int
set_extension (ASN1_TYPE asn, const char *root,
               const char *ext_id,
               const gnutls_datum_t * ext_data, unsigned int critical)
{
  int result;
  int k, len;
  char name[ASN1_MAX_NAME_SIZE], name2[ASN1_MAX_NAME_SIZE];
  char extnID[128];

  /* Find the index of the given extension.
   */
  k = 0;
  do
    {
      k++;

      if (root[0] != 0)
        snprintf (name, sizeof (name), "%s.?%u", root, k);
      else
        snprintf (name, sizeof (name), "?%u", k);

      len = sizeof (extnID) - 1;
      result = asn1_read_value (asn, name, extnID, &len);

      /* move to next
       */

      if (result == ASN1_ELEMENT_NOT_FOUND)
        {
          break;
        }

      do
        {

          _gnutls_str_cpy (name2, sizeof (name2), name);
          _gnutls_str_cat (name2, sizeof (name2), ".extnID");

          len = sizeof (extnID) - 1;
          result = asn1_read_value (asn, name2, extnID, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            {
              gnutls_assert ();
              break;
            }
          else if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              return _gnutls_asn2err (result);
            }

          /* Handle Extension 
           */
          if (strcmp (extnID, ext_id) == 0)
            {
              /* extension was found 
               */
              return overwrite_extension (asn, root, k, ext_data, critical);
            }


        }
      while (0);
    }
  while (1);

  if (result == ASN1_ELEMENT_NOT_FOUND)
    {
      return add_extension (asn, root, ext_id, ext_data, critical);
    }
  else
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }


  return 0;
}

/* This function will attempt to overwrite the requested extension with
 * the given one. 
 *
 * Critical will be either 0 or 1.
 */
int
_gnutls_x509_crt_set_extension (gnutls_x509_crt_t cert,
                                const char *ext_id,
                                const gnutls_datum_t * ext_data,
                                unsigned int critical)
{
  return set_extension (cert->cert, "tbsCertificate.extensions", ext_id,
                        ext_data, critical);
}

int
_gnutls_x509_crl_set_extension (gnutls_x509_crl_t crl,
                                const char *ext_id,
                                const gnutls_datum_t * ext_data,
                                unsigned int critical)
{
  return set_extension (crl->crl, "tbsCertList.crlExtensions", ext_id,
                        ext_data, critical);
}

#ifdef ENABLE_PKI
int
_gnutls_x509_crq_set_extension (gnutls_x509_crq_t crq,
                                const char *ext_id,
                                const gnutls_datum_t * ext_data,
                                unsigned int critical)
{
  unsigned char *extensions = NULL;
  size_t extensions_size = 0;
  gnutls_datum_t der;
  ASN1_TYPE c2;
  int result;

  result = gnutls_x509_crq_get_attribute_by_oid (crq, "1.2.840.113549.1.9.14",
                                                 0, NULL, &extensions_size);
  if (result == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      extensions = gnutls_malloc (extensions_size);
      if (extensions == NULL)
        {
          gnutls_assert ();
          return GNUTLS_E_MEMORY_ERROR;
        }

      result = gnutls_x509_crq_get_attribute_by_oid (crq,
                                                     "1.2.840.113549.1.9.14",
                                                     0, extensions,
                                                     &extensions_size);
    }
  if (result < 0)
    {
      if (result == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
        {
          extensions_size = 0;
        }
      else
        {
          gnutls_assert ();
          gnutls_free (extensions);
          return result;
        }
    }

  result = asn1_create_element (_gnutls_get_pkix (), "PKIX1.Extensions", &c2);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      gnutls_free (extensions);
      return _gnutls_asn2err (result);
    }

  if (extensions_size > 0)
    {
      result = asn1_der_decoding (&c2, extensions, extensions_size, NULL);
      gnutls_free (extensions);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          asn1_delete_structure (&c2);
          return _gnutls_asn2err (result);
        }
    }

  result = set_extension (c2, "", ext_id, ext_data, critical);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&c2);
      return result;
    }

  result = _gnutls_x509_der_encode (c2, "", &der, 0);

  asn1_delete_structure (&c2);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  result = gnutls_x509_crq_set_attribute_by_oid (crq, "1.2.840.113549.1.9.14",
                                                 der.data, der.size);
  gnutls_free (der.data);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }


  return 0;
}

#endif

/* Here we only extract the KeyUsage field, from the DER encoded
 * extension.
 */
int
_gnutls_x509_ext_extract_keyUsage (uint16_t * keyUsage,
                                   opaque * extnValue, int extnValueLen)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int len, result;
  uint8_t str[2];

  str[0] = str[1] = 0;
  *keyUsage = 0;

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.KeyUsage", &ext)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&ext, extnValue, extnValueLen, NULL);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  len = sizeof (str);
  result = asn1_read_value (ext, "", str, &len);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return 0;
    }

  *keyUsage = str[0] | (str[1] << 8);

  asn1_delete_structure (&ext);

  return 0;
}

/* extract the basicConstraints from the DER encoded extension
 */
int
_gnutls_x509_ext_extract_basicConstraints (int *CA,
                                           int *pathLenConstraint,
                                           opaque * extnValue,
                                           int extnValueLen)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  char str[128];
  int len, result;

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.BasicConstraints", &ext)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&ext, extnValue, extnValueLen, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  if (pathLenConstraint)
    {
      result = _gnutls_x509_read_uint (ext, "pathLenConstraint",
                                       pathLenConstraint);
      if (result == GNUTLS_E_ASN1_ELEMENT_NOT_FOUND)
        *pathLenConstraint = -1;
      else if (result != GNUTLS_E_SUCCESS)
        {
          gnutls_assert ();
          asn1_delete_structure (&ext);
          return _gnutls_asn2err (result);
        }
    }

  /* the default value of cA is false.
   */
  len = sizeof (str) - 1;
  result = asn1_read_value (ext, "cA", str, &len);
  if (result == ASN1_SUCCESS && strcmp (str, "TRUE") == 0)
    *CA = 1;
  else
    *CA = 0;

  asn1_delete_structure (&ext);

  return 0;
}

/* generate the basicConstraints in a DER encoded extension
 * Use 0 or 1 (TRUE) for CA.
 * Use negative values for pathLenConstraint to indicate that the field
 * should not be present, >= 0 to indicate set values.
 */
int
_gnutls_x509_ext_gen_basicConstraints (int CA,
                                       int pathLenConstraint,
                                       gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  const char *str;
  int result;

  if (CA == 0)
    str = "FALSE";
  else
    str = "TRUE";

  result =
    asn1_create_element (_gnutls_get_pkix (), "PKIX1.BasicConstraints", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (ext, "cA", str, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  if (pathLenConstraint < 0)
    {
      result = asn1_write_value (ext, "pathLenConstraint", NULL, 0);
      if (result < 0)
        result = _gnutls_asn2err (result);
    }
  else
    result = _gnutls_x509_write_uint32 (ext, "pathLenConstraint",
                                        pathLenConstraint);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return result;
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/* extract an INTEGER from the DER encoded extension
 */
int
_gnutls_x509_ext_extract_number (opaque * number,
                                 size_t * _nr_size,
                                 opaque * extnValue, int extnValueLen)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;
  int nr_size = *_nr_size;

  /* here it doesn't matter so much that we use CertificateSerialNumber. It is equal
   * to using INTEGER.
   */
  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.CertificateSerialNumber",
        &ext)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&ext, extnValue, extnValueLen, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  /* the default value of cA is false.
   */
  result = asn1_read_value (ext, "", number, &nr_size);
  if (result != ASN1_SUCCESS)
    result = _gnutls_asn2err (result);
  else
    result = 0;

  *_nr_size = nr_size;

  asn1_delete_structure (&ext);

  return result;
}

/* generate an INTEGER in a DER encoded extension
 */
int
_gnutls_x509_ext_gen_number (const opaque * number, size_t nr_size,
                             gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;

  result =
    asn1_create_element (_gnutls_get_pkix (), "PKIX1.CertificateSerialNumber",
                         &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (ext, "", number, nr_size);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/* generate the keyUsage in a DER encoded extension
 * Use an ORed SEQUENCE of GNUTLS_KEY_* for usage.
 */
int
_gnutls_x509_ext_gen_keyUsage (uint16_t usage, gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;
  uint8_t str[2];

  result = asn1_create_element (_gnutls_get_pkix (), "PKIX1.KeyUsage", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  str[0] = usage & 0xff;
  str[1] = usage >> 8;

  result = asn1_write_value (ext, "", str, 9);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

static int
write_new_general_name (ASN1_TYPE ext, const char *ext_name,
                        gnutls_x509_subject_alt_name_t type,
                        const void *data, unsigned int data_size)
{
  const char *str;
  int result;
  char name[128];

  result = asn1_write_value (ext, ext_name, "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  switch (type)
    {
    case GNUTLS_SAN_DNSNAME:
      str = "dNSName";
      break;
    case GNUTLS_SAN_RFC822NAME:
      str = "rfc822Name";
      break;
    case GNUTLS_SAN_URI:
      str = "uniformResourceIdentifier";
      break;
    case GNUTLS_SAN_IPADDRESS:
      str = "iPAddress";
      break;
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  if (ext_name[0] == 0)
    {                           /* no dot */
      _gnutls_str_cpy (name, sizeof (name), "?LAST");
    }
  else
    {
      _gnutls_str_cpy (name, sizeof (name), ext_name);
      _gnutls_str_cat (name, sizeof (name), ".?LAST");
    }

  result = asn1_write_value (ext, name, str, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  _gnutls_str_cat (name, sizeof (name), ".");
  _gnutls_str_cat (name, sizeof (name), str);

  result = asn1_write_value (ext, name, data, data_size);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  return 0;
}

/* Convert the given name to GeneralNames in a DER encoded extension.
 * This is the same as subject alternative name.
 */
int
_gnutls_x509_ext_gen_subject_alt_name (gnutls_x509_subject_alt_name_t
                                       type, const void *data,
                                       unsigned int data_size,
                                       gnutls_datum_t * prev_der_ext,
                                       gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;

  result =
    asn1_create_element (_gnutls_get_pkix (), "PKIX1.GeneralNames", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  if (prev_der_ext != NULL && prev_der_ext->data != NULL
      && prev_der_ext->size != 0)
    {
      result =
        asn1_der_decoding (&ext, prev_der_ext->data, prev_der_ext->size,
                           NULL);

      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          asn1_delete_structure (&ext);
          return _gnutls_asn2err (result);
        }
    }

  result = write_new_general_name (ext, "", type, data, data_size);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return result;
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/* generate the SubjectKeyID in a DER encoded extension
 */
int
_gnutls_x509_ext_gen_key_id (const void *id, size_t id_size,
                             gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;

  result =
    asn1_create_element (_gnutls_get_pkix (),
                         "PKIX1.SubjectKeyIdentifier", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (ext, "", id, id_size);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/* generate the AuthorityKeyID in a DER encoded extension
 */
int
_gnutls_x509_ext_gen_auth_key_id (const void *id, size_t id_size,
                                  gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;

  result =
    asn1_create_element (_gnutls_get_pkix (),
                         "PKIX1.AuthorityKeyIdentifier", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (ext, "keyIdentifier", id, id_size);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  asn1_write_value (ext, "authorityCertIssuer", NULL, 0);
  asn1_write_value (ext, "authorityCertSerialNumber", NULL, 0);

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}


/* Creates and encodes the CRL Distribution points. data_string should be a name
 * and type holds the type of the name. 
 * reason_flags should be an or'ed sequence of GNUTLS_CRL_REASON_*.
 *
 */
int
_gnutls_x509_ext_gen_crl_dist_points (gnutls_x509_subject_alt_name_t
                                      type, const void *data,
                                      unsigned int data_size,
                                      unsigned int reason_flags,
                                      gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  gnutls_datum_t gnames = { NULL, 0 };
  int result;
  uint8_t reasons[2];

  reasons[0] = reason_flags & 0xff;
  reasons[1] = reason_flags >> 8;

  result =
    asn1_create_element (_gnutls_get_pkix (),
                         "PKIX1.CRLDistributionPoints", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  result = asn1_write_value (ext, "", "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  if (reason_flags)
    {
      result = asn1_write_value (ext, "?LAST.reasons", reasons, 9);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }
    }
  else
    {
      result = asn1_write_value (ext, "?LAST.reasons", NULL, 0);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }
    }

  result = asn1_write_value (ext, "?LAST.cRLIssuer", NULL, 0);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

  /* When used as type CHOICE.
   */
  result = asn1_write_value (ext, "?LAST.distributionPoint", "fullName", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto cleanup;
    }

#if 0
  /* only needed in old code (where defined as SEQUENCE OF) */
  asn1_write_value (ext,
                    "?LAST.distributionPoint.nameRelativeToCRLIssuer",
                    NULL, 0);
#endif

  result =
    write_new_general_name (ext, "?LAST.distributionPoint.fullName",
                            type, data, data_size);
  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  if (result < 0)
    {
      gnutls_assert ();
      goto cleanup;
    }

  result = 0;

cleanup:
  _gnutls_free_datum (&gnames);
  asn1_delete_structure (&ext);

  return result;
}

/* extract the proxyCertInfo from the DER encoded extension
 */
int
_gnutls_x509_ext_extract_proxyCertInfo (int *pathLenConstraint,
                                        char **policyLanguage,
                                        char **policy,
                                        size_t * sizeof_policy,
                                        opaque * extnValue, int extnValueLen)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;
  gnutls_datum_t value;

  if ((result = asn1_create_element
       (_gnutls_get_pkix (), "PKIX1.ProxyCertInfo", &ext)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&ext, extnValue, extnValueLen, NULL);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  if (pathLenConstraint)
    {
      result = _gnutls_x509_read_uint (ext, "pCPathLenConstraint",
                                       pathLenConstraint);
      if (result == GNUTLS_E_ASN1_ELEMENT_NOT_FOUND)
        *pathLenConstraint = -1;
      else if (result != GNUTLS_E_SUCCESS)
        {
          asn1_delete_structure (&ext);
          return _gnutls_asn2err (result);
        }
    }

  result = _gnutls_x509_read_value (ext, "proxyPolicy.policyLanguage",
                                    &value, 0);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return result;
    }

  if (policyLanguage)
    *policyLanguage = gnutls_strdup (value.data);

  result = _gnutls_x509_read_value (ext, "proxyPolicy.policy", &value, 0);
  if (result == GNUTLS_E_ASN1_ELEMENT_NOT_FOUND)
    {
      if (policy)
        *policy = NULL;
      if (sizeof_policy)
        *sizeof_policy = 0;
    }
  else if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return result;
    }
  else
    {
      if (policy)
        *policy = value.data;
      if (sizeof_policy)
        *sizeof_policy = value.size;
    }

  asn1_delete_structure (&ext);

  return 0;
}

/* generate the proxyCertInfo in a DER encoded extension
 */
int
_gnutls_x509_ext_gen_proxyCertInfo (int pathLenConstraint,
                                    const char *policyLanguage,
                                    const char *policy,
                                    size_t sizeof_policy,
                                    gnutls_datum_t * der_ext)
{
  ASN1_TYPE ext = ASN1_TYPE_EMPTY;
  int result;

  result = asn1_create_element (_gnutls_get_pkix (),
                                "PKIX1.ProxyCertInfo", &ext);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  if (pathLenConstraint < 0)
    {
      result = asn1_write_value (ext, "pCPathLenConstraint", NULL, 0);
      if (result < 0)
        result = _gnutls_asn2err (result);
    }
  else
    result = _gnutls_x509_write_uint32 (ext, "pCPathLenConstraint",
                                        pathLenConstraint);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return result;
    }

  result = asn1_write_value (ext, "proxyPolicy.policyLanguage",
                             policyLanguage, 1);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  result = asn1_write_value (ext, "proxyPolicy.policy",
                             policy, sizeof_policy);
  if (result < 0)
    {
      gnutls_assert ();
      asn1_delete_structure (&ext);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_der_encode (ext, "", der_ext, 0);

  asn1_delete_structure (&ext);

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}
