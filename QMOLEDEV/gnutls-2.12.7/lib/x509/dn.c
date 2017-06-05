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

#include <gnutls_int.h>
#include <libtasn1.h>
#include <gnutls_datum.h>
#include <gnutls_global.h>
#include <gnutls_errors.h>
#include <gnutls_str.h>
#include <common.h>
#include <gnutls_num.h>

/* This file includes all the required to parse an X.509 Distriguished
 * Name (you need a parser just to read a name in the X.509 protoocols!!!)
 */

/* Converts the given OID to an ldap acceptable string or
 * a dotted OID.
 */
static const char *
oid2ldap_string (const char *oid)
{
  const char *ret;

  ret = _gnutls_x509_oid2ldap_string (oid);
  if (ret)
    return ret;

  /* else return the OID in dotted format */
  return oid;
}

/* Escapes a string following the rules from RFC2253.
 */
static char *
str_escape (char *str, char *buffer, unsigned int buffer_size)
{
  int str_length, j, i;

  if (str == NULL || buffer == NULL)
    return NULL;

  str_length = MIN (strlen (str), buffer_size - 1);

  for (i = j = 0; i < str_length; i++)
    {
      if (str[i] == ',' || str[i] == '+' || str[i] == '"'
          || str[i] == '\\' || str[i] == '<' || str[i] == '>'
          || str[i] == ';')
        buffer[j++] = '\\';

      buffer[j++] = str[i];
    }

  /* null terminate the string */
  buffer[j] = 0;

  return buffer;
}

/* Parses an X509 DN in the asn1_struct, and puts the output into
 * the string buf. The output is an LDAP encoded DN.
 *
 * asn1_rdn_name must be a string in the form "tbsCertificate.issuer.rdnSequence".
 * That is to point in the rndSequence.
 */
int
_gnutls_x509_parse_dn (ASN1_TYPE asn1_struct,
                       const char *asn1_rdn_name, char *buf,
                       size_t * sizeof_buf)
{
  gnutls_buffer_st out_str;
  int k2, k1, result;
  char tmpbuffer1[ASN1_MAX_NAME_SIZE];
  char tmpbuffer2[ASN1_MAX_NAME_SIZE];
  char tmpbuffer3[ASN1_MAX_NAME_SIZE];
  opaque value[MAX_STRING_LEN], *value2 = NULL;
  char *escaped = NULL;
  const char *ldap_desc;
  char oid[MAX_OID_SIZE];
  int len, printable;
  char *string = NULL;
  size_t sizeof_string, sizeof_escaped;

  if (sizeof_buf == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (*sizeof_buf > 0 && buf)
    buf[0] = 0;
  else
    *sizeof_buf = 0;

  _gnutls_buffer_init (&out_str);

  k1 = 0;
  do
    {

      k1++;
      /* create a string like "tbsCertList.issuer.rdnSequence.?1"
       */
      if (asn1_rdn_name[0] != 0)
        snprintf (tmpbuffer1, sizeof (tmpbuffer1), "%s.?%u", asn1_rdn_name,
                  k1);
      else
        snprintf (tmpbuffer1, sizeof (tmpbuffer1), "?%u", k1);

      len = sizeof (value) - 1;
      result = asn1_read_value (asn1_struct, tmpbuffer1, value, &len);

      if (result == ASN1_ELEMENT_NOT_FOUND)
        {
          break;
        }

      if (result != ASN1_VALUE_NOT_FOUND)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      k2 = 0;

      do
        {                       /* Move to the attibute type and values
                                 */
          k2++;

          if (tmpbuffer1[0] != 0)
            snprintf (tmpbuffer2, sizeof (tmpbuffer2), "%s.?%u", tmpbuffer1,
                      k2);
          else
            snprintf (tmpbuffer2, sizeof (tmpbuffer2), "?%u", k2);

          /* Try to read the RelativeDistinguishedName attributes.
           */

          len = sizeof (value) - 1;
          result = asn1_read_value (asn1_struct, tmpbuffer2, value, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            break;
          if (result != ASN1_VALUE_NOT_FOUND)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }

          /* Read the OID 
           */
          _gnutls_str_cpy (tmpbuffer3, sizeof (tmpbuffer3), tmpbuffer2);
          _gnutls_str_cat (tmpbuffer3, sizeof (tmpbuffer3), ".type");

          len = sizeof (oid) - 1;
          result = asn1_read_value (asn1_struct, tmpbuffer3, oid, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            break;
          else if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }

          /* Read the Value 
           */
          _gnutls_str_cpy (tmpbuffer3, sizeof (tmpbuffer3), tmpbuffer2);
          _gnutls_str_cat (tmpbuffer3, sizeof (tmpbuffer3), ".value");

          len = 0;
          result = asn1_read_value (asn1_struct, tmpbuffer3, NULL, &len);

          value2 = gnutls_malloc (len);
          if (value2 == NULL)
            {
              gnutls_assert ();
              result = GNUTLS_E_MEMORY_ERROR;
              goto cleanup;
            }

          result = asn1_read_value (asn1_struct, tmpbuffer3, value2, &len);

          if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }
#define STR_APPEND(y) if ((result=_gnutls_buffer_append_str( &out_str, y)) < 0) { \
	gnutls_assert(); \
	goto cleanup; \
}
          /*   The encodings of adjoining RelativeDistinguishedNames are separated
           *   by a comma character (',' ASCII 44).
           */

          /*   Where there is a multi-valued RDN, the outputs from adjoining
           *   AttributeTypeAndValues are separated by a plus ('+' ASCII 43)
           *   character.
           */
          if (k1 != 1)
            {                   /* the first time do not append a comma */
              if (k2 != 1)
                {               /* adjoining multi-value RDN */
                  STR_APPEND ("+");
                }
              else
                {
                  STR_APPEND (",");
                }
            }

          ldap_desc = oid2ldap_string (oid);
          printable = _gnutls_x509_oid_data_printable (oid);

          /* leading #, hex encoded value and terminating NULL */
          sizeof_escaped = 2 * len + 2;

          escaped = gnutls_malloc (sizeof_escaped);
          if (escaped == NULL)
            {
              gnutls_assert ();
              result = GNUTLS_E_MEMORY_ERROR;
              goto cleanup;
            }

          sizeof_string = 2 * len + 2;  /* in case it is not printable */

          string = gnutls_malloc (sizeof_string);
          if (string == NULL)
            {
              gnutls_assert ();
              result = GNUTLS_E_MEMORY_ERROR;
              goto cleanup;
            }

          STR_APPEND (ldap_desc);
          STR_APPEND ("=");
          result = 0;

          if (printable)
            result =
              _gnutls_x509_oid_data2string (oid,
                                            value2, len,
                                            string, &sizeof_string);

          if (!printable || result < 0)
            result =
              _gnutls_x509_data2hex (value2, len, string, &sizeof_string);

          if (result < 0)
            {
              gnutls_assert ();
              _gnutls_x509_log
                ("Found OID: '%s' with value '%s'\n",
                 oid, _gnutls_bin2hex (value2, len, escaped, sizeof_escaped,
                                       NULL));
              goto cleanup;
            }
          STR_APPEND (str_escape (string, escaped, sizeof_escaped));
          gnutls_free (string);
          string = NULL;

          gnutls_free (escaped);
          escaped = NULL;
          gnutls_free (value2);
          value2 = NULL;

        }
      while (1);

    }
  while (1);

  if (out_str.length >= (unsigned int) *sizeof_buf)
    {
      gnutls_assert ();
      *sizeof_buf = out_str.length + 1;
      result = GNUTLS_E_SHORT_MEMORY_BUFFER;
      goto cleanup;
    }

  if (buf)
    {
      _gnutls_buffer_pop_data (&out_str, buf, sizeof_buf);
      buf[*sizeof_buf] = 0;
    }
  else
    *sizeof_buf = out_str.length;

  result = 0;

cleanup:
  gnutls_free (value2);
  gnutls_free (string);
  gnutls_free (escaped);
  _gnutls_buffer_clear (&out_str);
  return result;
}

/* Parses an X509 DN in the asn1_struct, and searches for the
 * given OID in the DN.
 *
 * If raw_flag == 0, the output will be encoded in the LDAP way. (#hex for non printable)
 * Otherwise the raw DER data are returned.
 *
 * asn1_rdn_name must be a string in the form "tbsCertificate.issuer.rdnSequence".
 * That is to point in the rndSequence.
 *
 * indx specifies which OID to return. Ie 0 means return the first specified
 * OID found, 1 the second etc.
 */
int
_gnutls_x509_parse_dn_oid (ASN1_TYPE asn1_struct,
                           const char *asn1_rdn_name,
                           const char *given_oid, int indx,
                           unsigned int raw_flag,
                           void *buf, size_t * sizeof_buf)
{
  int k2, k1, result;
  char tmpbuffer1[ASN1_MAX_NAME_SIZE];
  char tmpbuffer2[ASN1_MAX_NAME_SIZE];
  char tmpbuffer3[ASN1_MAX_NAME_SIZE];
  opaque value[256];
  char oid[MAX_OID_SIZE];
  int len, printable;
  int i = 0;
  char *cbuf = buf;

  if (cbuf == NULL)
    *sizeof_buf = 0;
  else
    cbuf[0] = 0;

  k1 = 0;
  do
    {

      k1++;
      /* create a string like "tbsCertList.issuer.rdnSequence.?1"
       */
      if (asn1_rdn_name[0] != 0)
        snprintf (tmpbuffer1, sizeof (tmpbuffer1), "%s.?%u", asn1_rdn_name,
                  k1);
      else
        snprintf (tmpbuffer1, sizeof (tmpbuffer1), "?%u", k1);

      len = sizeof (value) - 1;
      result = asn1_read_value (asn1_struct, tmpbuffer1, value, &len);

      if (result == ASN1_ELEMENT_NOT_FOUND)
        {
          gnutls_assert ();
          break;
        }

      if (result != ASN1_VALUE_NOT_FOUND)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      k2 = 0;

      do
        {                       /* Move to the attibute type and values
                                 */
          k2++;

          if (tmpbuffer1[0] != 0)
            snprintf (tmpbuffer2, sizeof (tmpbuffer2), "%s.?%u", tmpbuffer1,
                      k2);
          else
            snprintf (tmpbuffer2, sizeof (tmpbuffer2), "?%u", k2);

          /* Try to read the RelativeDistinguishedName attributes.
           */

          len = sizeof (value) - 1;
          result = asn1_read_value (asn1_struct, tmpbuffer2, value, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            {
              break;
            }
          if (result != ASN1_VALUE_NOT_FOUND)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }

          /* Read the OID 
           */
          _gnutls_str_cpy (tmpbuffer3, sizeof (tmpbuffer3), tmpbuffer2);
          _gnutls_str_cat (tmpbuffer3, sizeof (tmpbuffer3), ".type");

          len = sizeof (oid) - 1;
          result = asn1_read_value (asn1_struct, tmpbuffer3, oid, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            break;
          else if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }

          if (strcmp (oid, given_oid) == 0 && indx == i++)
            {                   /* Found the OID */

              /* Read the Value 
               */
              _gnutls_str_cpy (tmpbuffer3, sizeof (tmpbuffer3), tmpbuffer2);
              _gnutls_str_cat (tmpbuffer3, sizeof (tmpbuffer3), ".value");

              len = *sizeof_buf;
              result = asn1_read_value (asn1_struct, tmpbuffer3, buf, &len);

              if (result != ASN1_SUCCESS)
                {
                  gnutls_assert ();
                  if (result == ASN1_MEM_ERROR)
                    *sizeof_buf = len;
                  result = _gnutls_asn2err (result);
                  goto cleanup;
                }

              if (raw_flag != 0)
                {
                  if ((unsigned) len > *sizeof_buf)
                    {
                      *sizeof_buf = len;
                      result = GNUTLS_E_SHORT_MEMORY_BUFFER;
                      goto cleanup;
                    }
                  *sizeof_buf = len;

                  return 0;

                }
              else
                {               /* parse data. raw_flag == 0 */
                  printable = _gnutls_x509_oid_data_printable (oid);

                  if (printable == 1)
                    result =
                      _gnutls_x509_oid_data2string (oid, buf, len,
                                                    cbuf, sizeof_buf);
                  else
                    result =
                      _gnutls_x509_data2hex (buf, len, cbuf, sizeof_buf);

                  if (result < 0)
                    {
                      gnutls_assert ();
                      goto cleanup;
                    }

                  return 0;

                }               /* raw_flag == 0 */
            }
        }
      while (1);

    }
  while (1);

  gnutls_assert ();

  result = GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;

cleanup:
  return result;
}


/* Parses an X509 DN in the asn1_struct, and returns the requested
 * DN OID.
 *
 * asn1_rdn_name must be a string in the form "tbsCertificate.issuer.rdnSequence".
 * That is to point in the rndSequence.
 *
 * indx specifies which OID to return. Ie 0 means return the first specified
 * OID found, 1 the second etc.
 */
int
_gnutls_x509_get_dn_oid (ASN1_TYPE asn1_struct,
                         const char *asn1_rdn_name,
                         int indx, void *_oid, size_t * sizeof_oid)
{
  int k2, k1, result;
  char tmpbuffer1[ASN1_MAX_NAME_SIZE];
  char tmpbuffer2[ASN1_MAX_NAME_SIZE];
  char tmpbuffer3[ASN1_MAX_NAME_SIZE];
  char value[256];
  char oid[MAX_OID_SIZE];
  int len;
  int i = 0;

  k1 = 0;
  do
    {

      k1++;
      /* create a string like "tbsCertList.issuer.rdnSequence.?1"
       */
      if (asn1_rdn_name[0] != 0)
        snprintf (tmpbuffer1, sizeof (tmpbuffer1), "%s.?%u", asn1_rdn_name,
                  k1);
      else
        snprintf (tmpbuffer1, sizeof (tmpbuffer1), "?%u", k1);

      len = sizeof (value) - 1;
      result = asn1_read_value (asn1_struct, tmpbuffer1, value, &len);

      if (result == ASN1_ELEMENT_NOT_FOUND)
        {
          gnutls_assert ();
          break;
        }

      if (result != ASN1_VALUE_NOT_FOUND)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto cleanup;
        }

      k2 = 0;

      do
        {                       /* Move to the attibute type and values
                                 */
          k2++;

          if (tmpbuffer1[0] != 0)
            snprintf (tmpbuffer2, sizeof (tmpbuffer2), "%s.?%u", tmpbuffer1,
                      k2);
          else
            snprintf (tmpbuffer2, sizeof (tmpbuffer2), "?%u", k2);

          /* Try to read the RelativeDistinguishedName attributes.
           */

          len = sizeof (value) - 1;
          result = asn1_read_value (asn1_struct, tmpbuffer2, value, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            {
              break;
            }
          if (result != ASN1_VALUE_NOT_FOUND)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }

          /* Read the OID 
           */
          _gnutls_str_cpy (tmpbuffer3, sizeof (tmpbuffer3), tmpbuffer2);
          _gnutls_str_cat (tmpbuffer3, sizeof (tmpbuffer3), ".type");

          len = sizeof (oid) - 1;
          result = asn1_read_value (asn1_struct, tmpbuffer3, oid, &len);

          if (result == ASN1_ELEMENT_NOT_FOUND)
            break;
          else if (result != ASN1_SUCCESS)
            {
              gnutls_assert ();
              result = _gnutls_asn2err (result);
              goto cleanup;
            }

          if (indx == i++)
            {                   /* Found the OID */

              len = strlen (oid) + 1;

              if (*sizeof_oid < (unsigned) len)
                {
                  *sizeof_oid = len;
                  gnutls_assert ();
                  return GNUTLS_E_SHORT_MEMORY_BUFFER;
                }

              memcpy (_oid, oid, len);
              *sizeof_oid = len - 1;

              return 0;
            }
        }
      while (1);

    }
  while (1);

  gnutls_assert ();

  result = GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE;

cleanup:
  return result;
}

/* This will encode and write the AttributeTypeAndValue field.
 * 'multi' must be zero if writing an AttributeTypeAndValue, and 1 if Attribute.
 * In all cases only one value is written.
 */
int
_gnutls_x509_encode_and_write_attribute (const char *given_oid,
                                         ASN1_TYPE asn1_struct,
                                         const char *where,
                                         const void *_data,
                                         int sizeof_data, int multi)
{
  const char *val_name;
  const opaque *data = _data;
  char tmp[128];
  ASN1_TYPE c2;
  int result;


  /* Find how to encode the data.
   */
  val_name = _gnutls_x509_oid2asn_string (given_oid);
  if (val_name == NULL)
    {
      gnutls_assert ();
      _gnutls_x509_log ("Cannot find OID: %s\n", given_oid);
      return GNUTLS_E_X509_UNSUPPORTED_OID;
    }

  result = asn1_create_element (_gnutls_get_pkix (), val_name, &c2);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  tmp[0] = 0;

  if ((result = _gnutls_x509_oid_data_choice (given_oid)) > 0)
    {
      const char *string_type;
      int i;

      string_type = "printableString";

      /* Check if the data is plain ascii, and use
       * the UTF8 string type if not.
       */
      for (i = 0; i < sizeof_data; i++)
        {
          if (!isascii (data[i]))
            {
              string_type = "utf8String";
              break;
            }
        }

      /* if the type is a CHOICE then write the
       * type we'll use.
       */
      result = asn1_write_value (c2, "", string_type, 1);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto error;
        }

      _gnutls_str_cpy (tmp, sizeof (tmp), string_type);
    }

  result = asn1_write_value (c2, tmp, data, sizeof_data);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto error;
    }


  /* write the data (value)
   */

  _gnutls_str_cpy (tmp, sizeof (tmp), where);
  _gnutls_str_cat (tmp, sizeof (tmp), ".value");

  if (multi != 0)
    {                           /* if not writing an AttributeTypeAndValue, but an Attribute */
      _gnutls_str_cat (tmp, sizeof (tmp), "s"); /* values */

      result = asn1_write_value (asn1_struct, tmp, "NEW", 1);
      if (result != ASN1_SUCCESS)
        {
          gnutls_assert ();
          result = _gnutls_asn2err (result);
          goto error;
        }

      _gnutls_str_cat (tmp, sizeof (tmp), ".?LAST");

    }

  result = _gnutls_x509_der_encode_and_copy (c2, "", asn1_struct, tmp, 0);
  if (result < 0)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto error;
    }

  /* write the type
   */
  _gnutls_str_cpy (tmp, sizeof (tmp), where);
  _gnutls_str_cat (tmp, sizeof (tmp), ".type");

  result = asn1_write_value (asn1_struct, tmp, given_oid, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      goto error;
    }

  result = 0;

error:
  asn1_delete_structure (&c2);
  return result;
}

/* This will write the AttributeTypeAndValue field. The data must be already DER encoded.
 * 'multi' must be zero if writing an AttributeTypeAndValue, and 1 if Attribute.
 * In all cases only one value is written.
 */
static int
_gnutls_x509_write_attribute (const char *given_oid,
                              ASN1_TYPE asn1_struct, const char *where,
                              const void *_data, int sizeof_data)
{
  char tmp[128];
  int result;

  /* write the data (value)
   */

  _gnutls_str_cpy (tmp, sizeof (tmp), where);
  _gnutls_str_cat (tmp, sizeof (tmp), ".value");

  result = asn1_write_value (asn1_struct, tmp, _data, sizeof_data);
  if (result < 0)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  /* write the type
   */
  _gnutls_str_cpy (tmp, sizeof (tmp), where);
  _gnutls_str_cat (tmp, sizeof (tmp), ".type");

  result = asn1_write_value (asn1_struct, tmp, given_oid, 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  return 0;
}


/* Decodes an X.509 Attribute (if multi==1) or an AttributeTypeAndValue
 * otherwise.
 *
 * octet_string should be non zero if we are to decode octet strings after
 * decoding.
 *
 * The output is allocated and stored in value.
 */
int
_gnutls_x509_decode_and_read_attribute (ASN1_TYPE asn1_struct,
                                        const char *where, char *oid,
                                        int oid_size, gnutls_datum_t * value,
                                        int multi, int octet_string)
{
  char tmpbuffer[128];
  int len, result;

  /* Read the OID 
   */
  _gnutls_str_cpy (tmpbuffer, sizeof (tmpbuffer), where);
  _gnutls_str_cat (tmpbuffer, sizeof (tmpbuffer), ".type");

  len = oid_size - 1;
  result = asn1_read_value (asn1_struct, tmpbuffer, oid, &len);

  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      result = _gnutls_asn2err (result);
      return result;
    }

  /* Read the Value 
   */

  _gnutls_str_cpy (tmpbuffer, sizeof (tmpbuffer), where);
  _gnutls_str_cat (tmpbuffer, sizeof (tmpbuffer), ".value");

  if (multi)
    _gnutls_str_cat (tmpbuffer, sizeof (tmpbuffer), "s.?1");    /* .values.?1 */

  result =
    _gnutls_x509_read_value (asn1_struct, tmpbuffer, value, octet_string);
  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;

}

/* Sets an X509 DN in the asn1_struct, and puts the given OID in the DN.
 * The input is assumed to be raw data.
 *
 * asn1_rdn_name must be a string in the form "tbsCertificate.issuer".
 * That is to point before the rndSequence.
 *
 */
int
_gnutls_x509_set_dn_oid (ASN1_TYPE asn1_struct,
                         const char *asn1_name, const char *given_oid,
                         int raw_flag, const char *name, int sizeof_name)
{
  int result;
  char tmp[ASN1_MAX_NAME_SIZE], asn1_rdn_name[ASN1_MAX_NAME_SIZE];

  if (sizeof_name == 0 || name == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  /* create the rdnSequence
   */
  result = asn1_write_value (asn1_struct, asn1_name, "rdnSequence", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  _gnutls_str_cpy (asn1_rdn_name, sizeof (asn1_rdn_name), asn1_name);
  _gnutls_str_cat (asn1_rdn_name, sizeof (asn1_rdn_name), ".rdnSequence");

  /* create a new element 
   */
  result = asn1_write_value (asn1_struct, asn1_rdn_name, "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  _gnutls_str_cpy (tmp, sizeof (tmp), asn1_rdn_name);
  _gnutls_str_cat (tmp, sizeof (tmp), ".?LAST");

  /* create the set with only one element
   */
  result = asn1_write_value (asn1_struct, tmp, "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }


  /* Encode and write the data
   */
  _gnutls_str_cpy (tmp, sizeof (tmp), asn1_rdn_name);
  _gnutls_str_cat (tmp, sizeof (tmp), ".?LAST.?LAST");

  if (!raw_flag)
    {
      result =
        _gnutls_x509_encode_and_write_attribute (given_oid,
                                                 asn1_struct,
                                                 tmp, name, sizeof_name, 0);
    }
  else
    {
      result =
        _gnutls_x509_write_attribute (given_oid, asn1_struct,
                                      tmp, name, sizeof_name);
    }

  if (result < 0)
    {
      gnutls_assert ();
      return result;
    }

  return 0;
}

/**
 * gnutls_x509_dn_init:
 * @dn: the object to be initialized
 *
 * This function initializes a #gnutls_x509_dn_t structure.
 *
 * The object returned must be deallocated using
 * gnutls_x509_dn_deinit().
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Since: 2.4.0
 **/
int
gnutls_x509_dn_init (gnutls_x509_dn_t * dn)
{
  int result;
  ASN1_TYPE tmpdn = ASN1_TYPE_EMPTY;

  if ((result =
       asn1_create_element (_gnutls_get_pkix (),
                            "PKIX1.Name", &tmpdn)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  *dn = tmpdn;

  return 0;
}

/**
 * gnutls_x509_dn_import:
 * @dn: the structure that will hold the imported DN
 * @data: should contain a DER encoded RDN sequence
 *
 * This function parses an RDN sequence and stores the result to a
 * #gnutls_x509_dn_t structure. The structure must have been initialized
 * with gnutls_x509_dn_init(). You may use gnutls_x509_dn_get_rdn_ava() to
 * decode the DN.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 *
 * Since: 2.4.0
 **/
int
gnutls_x509_dn_import (gnutls_x509_dn_t dn, const gnutls_datum_t * data)
{
  int result;
  char err[ASN1_MAX_ERROR_DESCRIPTION_SIZE];

  result = asn1_der_decoding ((ASN1_TYPE *) & dn,
                              data->data, data->size, err);
  if (result != ASN1_SUCCESS)
    {
      /* couldn't decode DER */
      _gnutls_x509_log ("ASN.1 Decoding error: %s\n", err);
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  return 0;
}

/**
 * gnutls_x509_dn_deinit:
 * @dn: a DN opaque object pointer.
 *
 * This function deallocates the DN object as returned by
 * gnutls_x509_dn_import().
 *
 * Since: 2.4.0
 **/
void
gnutls_x509_dn_deinit (gnutls_x509_dn_t dn)
{
  asn1_delete_structure ((ASN1_TYPE *) & dn);
}

/**
 * gnutls_x509_rdn_get:
 * @idn: should contain a DER encoded RDN sequence
 * @buf: a pointer to a structure to hold the peer's name
 * @sizeof_buf: holds the size of @buf
 *
 * This function will return the name of the given RDN sequence.  The
 * name will be in the form "C=xxxx,O=yyyy,CN=zzzz" as described in
 * RFC2253.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, or
 * %GNUTLS_E_SHORT_MEMORY_BUFFER is returned and *@sizeof_buf is
 * updated if the provided buffer is not long enough, otherwise a
 * negative error value.
 **/
int
gnutls_x509_rdn_get (const gnutls_datum_t * idn,
                     char *buf, size_t * sizeof_buf)
{
  int result;
  ASN1_TYPE dn = ASN1_TYPE_EMPTY;

  if (sizeof_buf == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  if (buf)
    buf[0] = 0;


  if ((result =
       asn1_create_element (_gnutls_get_pkix (),
                            "PKIX1.Name", &dn)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&dn, idn->data, idn->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      /* couldn't decode DER */
      gnutls_assert ();
      asn1_delete_structure (&dn);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_parse_dn (dn, "rdnSequence", buf, sizeof_buf);

  asn1_delete_structure (&dn);
  return result;

}

/**
 * gnutls_x509_rdn_get_by_oid:
 * @idn: should contain a DER encoded RDN sequence
 * @oid: an Object Identifier
 * @indx: In case multiple same OIDs exist in the RDN indicates which
 *   to send. Use 0 for the first one.
 * @raw_flag: If non zero then the raw DER data are returned.
 * @buf: a pointer to a structure to hold the peer's name
 * @sizeof_buf: holds the size of @buf
 *
 * This function will return the name of the given Object identifier,
 * of the RDN sequence.  The name will be encoded using the rules
 * from RFC2253.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, or
 * %GNUTLS_E_SHORT_MEMORY_BUFFER is returned and *@sizeof_buf is
 * updated if the provided buffer is not long enough, otherwise a
 * negative error value.
 **/
int
gnutls_x509_rdn_get_by_oid (const gnutls_datum_t * idn, const char *oid,
                            int indx, unsigned int raw_flag,
                            void *buf, size_t * sizeof_buf)
{
  int result;
  ASN1_TYPE dn = ASN1_TYPE_EMPTY;

  if (sizeof_buf == 0)
    {
      return GNUTLS_E_INVALID_REQUEST;
    }

  if ((result =
       asn1_create_element (_gnutls_get_pkix (),
                            "PKIX1.Name", &dn)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&dn, idn->data, idn->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      /* couldn't decode DER */
      gnutls_assert ();
      asn1_delete_structure (&dn);
      return _gnutls_asn2err (result);
    }

  result =
    _gnutls_x509_parse_dn_oid (dn, "rdnSequence", oid, indx,
                               raw_flag, buf, sizeof_buf);

  asn1_delete_structure (&dn);
  return result;

}

/**
 * gnutls_x509_rdn_get_oid:
 * @idn: should contain a DER encoded RDN sequence
 * @indx: Indicates which OID to return. Use 0 for the first one.
 * @buf: a pointer to a structure to hold the peer's name OID
 * @sizeof_buf: holds the size of @buf
 *
 * This function will return the specified Object identifier, of the
 * RDN sequence.
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, or
 * %GNUTLS_E_SHORT_MEMORY_BUFFER is returned and *@sizeof_buf is
 * updated if the provided buffer is not long enough, otherwise a
 * negative error value.
 *
 * Since: 2.4.0
 **/
int
gnutls_x509_rdn_get_oid (const gnutls_datum_t * idn,
                         int indx, void *buf, size_t * sizeof_buf)
{
  int result;
  ASN1_TYPE dn = ASN1_TYPE_EMPTY;

  if (sizeof_buf == 0)
    {
      return GNUTLS_E_INVALID_REQUEST;
    }

  if ((result =
       asn1_create_element (_gnutls_get_pkix (),
                            "PKIX1.Name", &dn)) != ASN1_SUCCESS)
    {
      gnutls_assert ();
      return _gnutls_asn2err (result);
    }

  result = asn1_der_decoding (&dn, idn->data, idn->size, NULL);
  if (result != ASN1_SUCCESS)
    {
      /* couldn't decode DER */
      gnutls_assert ();
      asn1_delete_structure (&dn);
      return _gnutls_asn2err (result);
    }

  result = _gnutls_x509_get_dn_oid (dn, "rdnSequence", indx, buf, sizeof_buf);

  asn1_delete_structure (&dn);
  return result;

}

/*
 * Compares the DER encoded part of a DN.
 *
 * FIXME: use a real DN comparison algorithm.
 *
 * Returns 1 if the DN's match and zero if they don't match. Otherwise
 * a negative value is returned to indicate error.
 */
int
_gnutls_x509_compare_raw_dn (const gnutls_datum_t * dn1,
                             const gnutls_datum_t * dn2)
{

  if (dn1->size != dn2->size)
    {
      gnutls_assert ();
      return 0;
    }
  if (memcmp (dn1->data, dn2->data, dn2->size) != 0)
    {
      gnutls_assert ();
      return 0;
    }
  return 1;                     /* they match */
}

/**
 * gnutls_x509_dn_export:
 * @dn: Holds the opaque DN object
 * @format: the format of output params. One of PEM or DER.
 * @output_data: will contain a DN PEM or DER encoded
 * @output_data_size: holds the size of output_data (and will be
 *   replaced by the actual size of parameters)
 *
 * This function will export the DN to DER or PEM format.
 *
 * If the buffer provided is not long enough to hold the output, then
 * *@output_data_size is updated and %GNUTLS_E_SHORT_MEMORY_BUFFER
 * will be returned.
 *
 * If the structure is PEM encoded, it will have a header
 * of "BEGIN NAME".
 *
 * Returns: On success, %GNUTLS_E_SUCCESS is returned, otherwise a
 *   negative error value.
 **/
int
gnutls_x509_dn_export (gnutls_x509_dn_t dn,
                       gnutls_x509_crt_fmt_t format, void *output_data,
                       size_t * output_data_size)
{
  ASN1_TYPE asn1 = dn;

  if (asn1 == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return _gnutls_x509_export_int_named (asn1, "rdnSequence",
                                        format, "NAME",
                                        output_data, output_data_size);
}
