/*
 * Copyright (C) 2002, 2004, 2006, 2008, 2009, 2010, 2011 Free Software
 * Foundation, Inc.
 *
 * This file is part of LIBTASN1.
 *
 * The LIBTASN1 library is free software; you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */


/*****************************************************/
/* File: decoding.c                                  */
/* Description: Functions to manage DER decoding     */
/*****************************************************/

#include <int.h>
#include "parser_aux.h"
#include <gstr.h>
#include "structure.h"
#include "element.h"

static asn1_retCode
_asn1_get_indefinite_length_string (const unsigned char *der, int *len);

static void
_asn1_error_description_tag_error (ASN1_TYPE node, char *ErrorDescription)
{

  Estrcpy (ErrorDescription, ":: tag error near element '");
  _asn1_hierarchical_name (node, ErrorDescription + strlen (ErrorDescription),
			   ASN1_MAX_ERROR_DESCRIPTION_SIZE - 40);
  Estrcat (ErrorDescription, "'");

}

/**
 * asn1_get_length_der:
 * @der: DER data to decode.
 * @der_len: Length of DER data to decode.
 * @len: Output variable containing the length of the DER length field.
 *
 * Extract a length field from DER data.
 *
 * Returns: Return the decoded length value, or -1 on indefinite
 *   length, or -2 when the value was too big.
 **/
signed long
asn1_get_length_der (const unsigned char *der, int der_len, int *len)
{
  unsigned long ans;
  int k, punt;

  *len = 0;
  if (der_len <= 0)
    return 0;

  if (!(der[0] & 128))
    {
      /* short form */
      *len = 1;
      return der[0];
    }
  else
    {
      /* Long form */
      k = der[0] & 0x7F;
      punt = 1;
      if (k)
	{			/* definite length method */
	  ans = 0;
	  while (punt <= k && punt < der_len)
	    {
	      unsigned long last = ans;

	      ans = ans * 256 + der[punt++];
	      if (ans < last)
		/* we wrapped around, no bignum support... */
		return -2;
	    }
	}
      else
	{			/* indefinite length method */
	  ans = -1;
	}

      *len = punt;
      return ans;
    }
}

/**
 * asn1_get_tag_der:
 * @der: DER data to decode.
 * @der_len: Length of DER data to decode.
 * @cls: Output variable containing decoded class.
 * @len: Output variable containing the length of the DER TAG data.
 * @tag: Output variable containing the decoded tag.
 *
 * Decode the class and TAG from DER code.
 *
 * Returns: Returns %ASN1_SUCCESS on success, or an error.
 **/
int
asn1_get_tag_der (const unsigned char *der, int der_len,
		  unsigned char *cls, int *len, unsigned long *tag)
{
  int punt, ris;

  if (der == NULL || der_len < 2 || len == NULL)
    return ASN1_DER_ERROR;

  *cls = der[0] & 0xE0;
  if ((der[0] & 0x1F) != 0x1F)
    {
      /* short form */
      *len = 1;
      ris = der[0] & 0x1F;
    }
  else
    {
      /* Long form */
      punt = 1;
      ris = 0;
      while (punt <= der_len && der[punt] & 128)
	{
	  int last = ris;
	  ris = ris * 128 + (der[punt++] & 0x7F);
	  if (ris < last)
	    /* wrapper around, and no bignums... */
	    return ASN1_DER_ERROR;
	}
      if (punt >= der_len)
	return ASN1_DER_ERROR;
      {
	int last = ris;
	ris = ris * 128 + (der[punt++] & 0x7F);
	if (ris < last)
	  /* wrapper around, and no bignums... */
	  return ASN1_DER_ERROR;
      }
      *len = punt;
    }
  if (tag)
    *tag = ris;
  return ASN1_SUCCESS;
}

/**
 * asn1_get_length_ber:
 * @ber: BER data to decode.
 * @ber_len: Length of BER data to decode.
 * @len: Output variable containing the length of the BER length field.
 *
 * Extract a length field from BER data.  The difference to
 * asn1_get_length_der() is that this function will return a length
 * even if the value has indefinite encoding.
 *
 * Returns: Return the decoded length value, or negative value when
 *   the value was too big.
 *
 * Since: 2.0
 **/
signed long
asn1_get_length_ber (const unsigned char *ber, int ber_len, int *len)
{
  int ret;
  long err;

  ret = asn1_get_length_der (ber, ber_len, len);
  if (ret == -1)
    {				/* indefinite length method */
      ret = ber_len;
      err = _asn1_get_indefinite_length_string (ber + 1, &ret);
      if (err != ASN1_SUCCESS)
	return -3;
    }

  return ret;
}

/**
 * asn1_get_octet_der:
 * @der: DER data to decode containing the OCTET SEQUENCE.
 * @der_len: Length of DER data to decode.
 * @ret_len: Output variable containing the length of the DER data.
 * @str: Pre-allocated output buffer to put decoded OCTET SEQUENCE in.
 * @str_size: Length of pre-allocated output buffer.
 * @str_len: Output variable containing the length of the OCTET SEQUENCE.
 *
 * Extract an OCTET SEQUENCE from DER data.
 *
 * Returns: Returns %ASN1_SUCCESS on success, or an error.
 **/
int
asn1_get_octet_der (const unsigned char *der, int der_len,
		    int *ret_len, unsigned char *str, int str_size,
		    int *str_len)
{
  int len_len;

  if (der_len <= 0)
    return ASN1_GENERIC_ERROR;

  /* if(str==NULL) return ASN1_SUCCESS; */
  *str_len = asn1_get_length_der (der, der_len, &len_len);

  if (*str_len < 0)
    return ASN1_DER_ERROR;

  *ret_len = *str_len + len_len;
  if (str_size >= *str_len)
    memcpy (str, der + len_len, *str_len);
  else
    {
      return ASN1_MEM_ERROR;
    }

  return ASN1_SUCCESS;
}

/* Returns ASN1_SUCCESS on success or an error code on error.
 */
static int
_asn1_get_time_der (const unsigned char *der, int der_len, int *ret_len,
		    char *str, int str_size)
{
  int len_len, str_len;

  if (der_len <= 0 || str == NULL)
    return ASN1_DER_ERROR;
  str_len = asn1_get_length_der (der, der_len, &len_len);
  if (str_len < 0 || str_size < str_len)
    return ASN1_DER_ERROR;
  memcpy (str, der + len_len, str_len);
  str[str_len] = 0;
  *ret_len = str_len + len_len;

  return ASN1_SUCCESS;
}

static int
_asn1_get_objectid_der (const unsigned char *der, int der_len, int *ret_len,
			char *str, int str_size)
{
  int len_len, len, k;
  int leading;
  char temp[20];
  unsigned long val, val1, prev_val;

  *ret_len = 0;
  if (str && str_size > 0)
    str[0] = 0;			/* no oid */

  if (str == NULL || der_len <= 0)
    return ASN1_GENERIC_ERROR;
  len = asn1_get_length_der (der, der_len, &len_len);

  if (len < 0 || len > der_len || len_len > der_len)
    return ASN1_DER_ERROR;

  val1 = der[len_len] / 40;
  val = der[len_len] - val1 * 40;

  _asn1_str_cpy (str, str_size, _asn1_ltostr (val1, temp));
  _asn1_str_cat (str, str_size, ".");
  _asn1_str_cat (str, str_size, _asn1_ltostr (val, temp));

  prev_val = 0;
  val = 0;
  leading = 1;
  for (k = 1; k < len; k++)
    {
      /* X.690 mandates that the leading byte must never be 0x80
       */
      if (leading != 0 && der[len_len + k] == 0x80)
	return ASN1_DER_ERROR;
      leading = 0;

      /* check for wrap around */
      val = val << 7;
      val |= der[len_len + k] & 0x7F;

      if (val < prev_val)
	return ASN1_DER_ERROR;

      prev_val = val;

      if (!(der[len_len + k] & 0x80))
	{
	  _asn1_str_cat (str, str_size, ".");
	  _asn1_str_cat (str, str_size, _asn1_ltostr (val, temp));
	  val = 0;
	  prev_val = 0;
	  leading = 1;
	}
    }
  *ret_len = len + len_len;

  return ASN1_SUCCESS;
}

/**
 * asn1_get_bit_der:
 * @der: DER data to decode containing the BIT SEQUENCE.
 * @der_len: Length of DER data to decode.
 * @ret_len: Output variable containing the length of the DER data.
 * @str: Pre-allocated output buffer to put decoded BIT SEQUENCE in.
 * @str_size: Length of pre-allocated output buffer.
 * @bit_len: Output variable containing the size of the BIT SEQUENCE.
 *
 * Extract a BIT SEQUENCE from DER data.
 *
 * Returns: Return %ASN1_SUCCESS on success, or an error.
 **/
int
asn1_get_bit_der (const unsigned char *der, int der_len,
		  int *ret_len, unsigned char *str, int str_size,
		  int *bit_len)
{
  int len_len, len_byte;

  if (der_len <= 0)
    return ASN1_GENERIC_ERROR;
  len_byte = asn1_get_length_der (der, der_len, &len_len) - 1;
  if (len_byte < 0)
    return ASN1_DER_ERROR;

  *ret_len = len_byte + len_len + 1;
  *bit_len = len_byte * 8 - der[len_len];

  if (str_size >= len_byte)
    memcpy (str, der + len_len + 1, len_byte);
  else
    {
      return ASN1_MEM_ERROR;
    }

  return ASN1_SUCCESS;
}

static int
_asn1_extract_tag_der (ASN1_TYPE node, const unsigned char *der, int der_len,
		       int *ret_len)
{
  ASN1_TYPE p;
  int counter, len2, len3, is_tag_implicit;
  unsigned long tag, tag_implicit = 0;
  unsigned char class, class2, class_implicit = 0;

  if (der_len <= 0)
    return ASN1_GENERIC_ERROR;

  counter = is_tag_implicit = 0;

  if (node->type & CONST_TAG)
    {
      p = node->down;
      while (p)
	{
	  if (type_field (p->type) == TYPE_TAG)
	    {
	      if (p->type & CONST_APPLICATION)
		class2 = ASN1_CLASS_APPLICATION;
	      else if (p->type & CONST_UNIVERSAL)
		class2 = ASN1_CLASS_UNIVERSAL;
	      else if (p->type & CONST_PRIVATE)
		class2 = ASN1_CLASS_PRIVATE;
	      else
		class2 = ASN1_CLASS_CONTEXT_SPECIFIC;

	      if (p->type & CONST_EXPLICIT)
		{
		  if (asn1_get_tag_der
		      (der + counter, der_len - counter, &class, &len2,
		       &tag) != ASN1_SUCCESS)
		    return ASN1_DER_ERROR;

		  if (counter + len2 > der_len)
		    return ASN1_DER_ERROR;
		  counter += len2;

		  len3 =
		    asn1_get_length_ber (der + counter, der_len - counter,
					 &len2);
		  if (len3 < 0)
		    return ASN1_DER_ERROR;

		  counter += len2;
		  if (counter > der_len)
		    return ASN1_DER_ERROR;

		  if (!is_tag_implicit)
		    {
		      if ((class != (class2 | ASN1_CLASS_STRUCTURED)) ||
			  (tag != strtoul ((char *) p->value, NULL, 10)))
			return ASN1_TAG_ERROR;
		    }
		  else
		    {		/* ASN1_TAG_IMPLICIT */
		      if ((class != class_implicit) || (tag != tag_implicit))
			return ASN1_TAG_ERROR;
		    }
		  is_tag_implicit = 0;
		}
	      else
		{		/* ASN1_TAG_IMPLICIT */
		  if (!is_tag_implicit)
		    {
		      if ((type_field (node->type) == TYPE_SEQUENCE) ||
			  (type_field (node->type) == TYPE_SEQUENCE_OF) ||
			  (type_field (node->type) == TYPE_SET) ||
			  (type_field (node->type) == TYPE_SET_OF))
			class2 |= ASN1_CLASS_STRUCTURED;
		      class_implicit = class2;
		      tag_implicit = strtoul ((char *) p->value, NULL, 10);
		      is_tag_implicit = 1;
		    }
		}
	    }
	  p = p->right;
	}
    }

  if (is_tag_implicit)
    {
      if (asn1_get_tag_der
	  (der + counter, der_len - counter, &class, &len2,
	   &tag) != ASN1_SUCCESS)
	return ASN1_DER_ERROR;
      if (counter + len2 > der_len)
	return ASN1_DER_ERROR;

      if ((class != class_implicit) || (tag != tag_implicit))
	{
	  if (type_field (node->type) == TYPE_OCTET_STRING)
	    {
	      class_implicit |= ASN1_CLASS_STRUCTURED;
	      if ((class != class_implicit) || (tag != tag_implicit))
		return ASN1_TAG_ERROR;
	    }
	  else
	    return ASN1_TAG_ERROR;
	}
    }
  else
    {
      if (type_field (node->type) == TYPE_TAG)
	{
	  counter = 0;
	  *ret_len = counter;
	  return ASN1_SUCCESS;
	}

      if (asn1_get_tag_der
	  (der + counter, der_len - counter, &class, &len2,
	   &tag) != ASN1_SUCCESS)
	return ASN1_DER_ERROR;

      if (counter + len2 > der_len)
	return ASN1_DER_ERROR;

      switch (type_field (node->type))
	{
	case TYPE_NULL:
	  if ((class != ASN1_CLASS_UNIVERSAL) || (tag != ASN1_TAG_NULL))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_BOOLEAN:
	  if ((class != ASN1_CLASS_UNIVERSAL) || (tag != ASN1_TAG_BOOLEAN))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_INTEGER:
	  if ((class != ASN1_CLASS_UNIVERSAL) || (tag != ASN1_TAG_INTEGER))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_ENUMERATED:
	  if ((class != ASN1_CLASS_UNIVERSAL) || (tag != ASN1_TAG_ENUMERATED))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_OBJECT_ID:
	  if ((class != ASN1_CLASS_UNIVERSAL) || (tag != ASN1_TAG_OBJECT_ID))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_TIME:
	  if (node->type & CONST_UTC)
	    {
	      if ((class != ASN1_CLASS_UNIVERSAL)
		  || (tag != ASN1_TAG_UTCTime))
		return ASN1_DER_ERROR;
	    }
	  else
	    {
	      if ((class != ASN1_CLASS_UNIVERSAL)
		  || (tag != ASN1_TAG_GENERALIZEDTime))
		return ASN1_DER_ERROR;
	    }
	  break;
	case TYPE_OCTET_STRING:
	  if (((class != ASN1_CLASS_UNIVERSAL)
	       && (class != (ASN1_CLASS_UNIVERSAL | ASN1_CLASS_STRUCTURED)))
	      || (tag != ASN1_TAG_OCTET_STRING))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_GENERALSTRING:
	  if ((class != ASN1_CLASS_UNIVERSAL)
	      || (tag != ASN1_TAG_GENERALSTRING))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_BIT_STRING:
	  if ((class != ASN1_CLASS_UNIVERSAL) || (tag != ASN1_TAG_BIT_STRING))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_SEQUENCE:
	case TYPE_SEQUENCE_OF:
	  if ((class != (ASN1_CLASS_UNIVERSAL | ASN1_CLASS_STRUCTURED))
	      || (tag != ASN1_TAG_SEQUENCE))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_SET:
	case TYPE_SET_OF:
	  if ((class != (ASN1_CLASS_UNIVERSAL | ASN1_CLASS_STRUCTURED))
	      || (tag != ASN1_TAG_SET))
	    return ASN1_DER_ERROR;
	  break;
	case TYPE_ANY:
	  counter -= len2;
	  break;
	default:
	  return ASN1_DER_ERROR;
	  break;
	}
    }

  counter += len2;
  *ret_len = counter;
  return ASN1_SUCCESS;
}

static int
_asn1_delete_not_used (ASN1_TYPE node)
{
  ASN1_TYPE p, p2;

  if (node == NULL)
    return ASN1_ELEMENT_NOT_FOUND;

  p = node;
  while (p)
    {
      if (p->type & CONST_NOT_USED)
	{
	  p2 = NULL;
	  if (p != node)
	    {
	      p2 = _asn1_find_left (p);
	      if (!p2)
		p2 = _asn1_find_up (p);
	    }
	  asn1_delete_structure (&p);
	  p = p2;
	}

      if (!p)
	break;			/* reach node */

      if (p->down)
	{
	  p = p->down;
	}
      else
	{
	  if (p == node)
	    p = NULL;
	  else if (p->right)
	    p = p->right;
	  else
	    {
	      while (1)
		{
		  p = _asn1_find_up (p);
		  if (p == node)
		    {
		      p = NULL;
		      break;
		    }
		  if (p->right)
		    {
		      p = p->right;
		      break;
		    }
		}
	    }
	}
    }
  return ASN1_SUCCESS;
}

static asn1_retCode
_asn1_extract_der_octet (ASN1_TYPE node, const unsigned char *der,
			 int der_len)
{
  int len2, len3;
  int counter2, counter_end;

  len2 = asn1_get_length_der (der, der_len, &len3);
  if (len2 < -1)
    return ASN1_DER_ERROR;

  counter2 = len3 + 1;

  if (len2 == -1)
    counter_end = der_len - 2;
  else
    counter_end = der_len;

  while (counter2 < counter_end)
    {
      len2 = asn1_get_length_der (der + counter2, der_len - counter2, &len3);

      if (len2 < -1)
	return ASN1_DER_ERROR;

      if (len2 > 0)
	{
	  _asn1_append_value (node, der + counter2 + len3, len2);
	}
      else
	{			/* indefinite */

	  len2 =
	    _asn1_extract_der_octet (node, der + counter2 + len3,
				     der_len - counter2 - len3);
	  if (len2 < 0)
	    return len2;
	}

      counter2 += len2 + len3 + 1;
    }

  return ASN1_SUCCESS;
}

static asn1_retCode
_asn1_get_octet_string (const unsigned char *der, ASN1_TYPE node, int *len)
{
  int len2, len3, counter, tot_len, indefinite;

  counter = 0;

  if (*(der - 1) & ASN1_CLASS_STRUCTURED)
    {
      tot_len = 0;
      indefinite = asn1_get_length_der (der, *len, &len3);
      if (indefinite < -1)
	return ASN1_DER_ERROR;

      counter += len3;
      if (indefinite >= 0)
	indefinite += len3;

      while (1)
	{
	  if (counter > (*len))
	    return ASN1_DER_ERROR;

	  if (indefinite == -1)
	    {
	      if ((der[counter] == 0) && (der[counter + 1] == 0))
		{
		  counter += 2;
		  break;
		}
	    }
	  else if (counter >= indefinite)
	    break;

	  if (der[counter] != ASN1_TAG_OCTET_STRING)
	    return ASN1_DER_ERROR;

	  counter++;

	  len2 = asn1_get_length_der (der + counter, *len - counter, &len3);
	  if (len2 <= 0)
	    return ASN1_DER_ERROR;

	  counter += len3 + len2;
	  tot_len += len2;
	}

      /* copy */
      if (node)
	{
	  unsigned char temp[DER_LEN];
	  int ret;

	  len2 = sizeof (temp);

	  asn1_length_der (tot_len, temp, &len2);
	  _asn1_set_value (node, temp, len2);

	  tot_len += len2;

	  ret = _asn1_extract_der_octet (node, der, *len);
	  if (ret != ASN1_SUCCESS)
	    return ret;

	}
    }
  else
    {				/* NOT STRUCTURED */
      len2 = asn1_get_length_der (der, *len, &len3);
      if (len2 < 0)
	return ASN1_DER_ERROR;
      if (len3 + len2 > *len)
	return ASN1_DER_ERROR;
      if (node)
	_asn1_set_value (node, der, len3 + len2);
      counter = len3 + len2;
    }

  *len = counter;
  return ASN1_SUCCESS;

}

static asn1_retCode
_asn1_get_indefinite_length_string (const unsigned char *der, int *len)
{
  int len2, len3, counter, indefinite;
  unsigned long tag;
  unsigned char class;

  counter = indefinite = 0;

  while (1)
    {
      if ((*len) < counter)
	return ASN1_DER_ERROR;

      if ((der[counter] == 0) && (der[counter + 1] == 0))
	{
	  counter += 2;
	  indefinite--;
	  if (indefinite <= 0)
	    break;
	  else
	    continue;
	}

      if (asn1_get_tag_der
	  (der + counter, *len - counter, &class, &len2,
	   &tag) != ASN1_SUCCESS)
	return ASN1_DER_ERROR;
      if (counter + len2 > *len)
	return ASN1_DER_ERROR;
      counter += len2;
      len2 = asn1_get_length_der (der + counter, *len - counter, &len3);
      if (len2 < -1)
	return ASN1_DER_ERROR;
      if (len2 == -1)
	{
	  indefinite++;
	  counter += 1;
	}
      else
	{
	  counter += len2 + len3;
	}
    }

  *len = counter;
  return ASN1_SUCCESS;

}

/**
 * asn1_der_decoding:
 * @element: pointer to an ASN1 structure.
 * @ider: vector that contains the DER encoding.
 * @len: number of bytes of *@ider: @ider[0]..@ider[len-1].
 * @errorDescription: null-terminated string contains details when an
 *   error occurred.
 *
 * Fill the structure *@ELEMENT with values of a DER encoding
 * string. The structure must just be created with function
 * asn1_create_element().  If an error occurs during the decoding
 * procedure, the *@ELEMENT is deleted and set equal to
 * %ASN1_TYPE_EMPTY.
 *
 * Returns: %ASN1_SUCCESS if DER encoding OK, %ASN1_ELEMENT_NOT_FOUND
 *   if @ELEMENT is %ASN1_TYPE_EMPTY, and %ASN1_TAG_ERROR or
 *   %ASN1_DER_ERROR if the der encoding doesn't match the structure
 *   name (*@ELEMENT deleted).
 **/
asn1_retCode
asn1_der_decoding (ASN1_TYPE * element, const void *ider, int len,
		   char *errorDescription)
{
  ASN1_TYPE node, p, p2, p3;
  char temp[128];
  int counter, len2, len3, len4, move, ris, tlen;
  unsigned char class;
  unsigned long tag;
  int indefinite, result;
  const unsigned char *der = ider;

  node = *element;

  if (node == ASN1_TYPE_EMPTY)
    return ASN1_ELEMENT_NOT_FOUND;

  if (node->type & CONST_OPTION)
    {
      asn1_delete_structure (element);
      return ASN1_GENERIC_ERROR;
    }

  counter = 0;
  move = DOWN;
  p = node;
  while (1)
    {
      ris = ASN1_SUCCESS;
      if (move != UP)
	{
	  if (p->type & CONST_SET)
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if (len2 == -1)
		{
		  if (!der[counter] && !der[counter + 1])
		    {
		      p = p2;
		      move = UP;
		      counter += 2;
		      continue;
		    }
		}
	      else if (counter == len2)
		{
		  p = p2;
		  move = UP;
		  continue;
		}
	      else if (counter > len2)
		{
		  asn1_delete_structure (element);
		  return ASN1_DER_ERROR;
		}
	      p2 = p2->down;
	      while (p2)
		{
		  if ((p2->type & CONST_SET) && (p2->type & CONST_NOT_USED))
		    {
		      if (type_field (p2->type) != TYPE_CHOICE)
			ris =
			  _asn1_extract_tag_der (p2, der + counter,
						 len - counter, &len2);
		      else
			{
			  p3 = p2->down;
			  while (p3)
			    {
			      ris =
				_asn1_extract_tag_der (p3, der + counter,
						       len - counter, &len2);
			      if (ris == ASN1_SUCCESS)
				break;
			      p3 = p3->right;
			    }
			}
		      if (ris == ASN1_SUCCESS)
			{
			  p2->type &= ~CONST_NOT_USED;
			  p = p2;
			  break;
			}
		    }
		  p2 = p2->right;
		}
	      if (p2 == NULL)
		{
		  asn1_delete_structure (element);
		  return ASN1_DER_ERROR;
		}
	    }

	  if ((p->type & CONST_OPTION) || (p->type & CONST_DEFAULT))
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if (counter == len2)
		{
		  if (p->right)
		    {
		      p2 = p->right;
		      move = RIGHT;
		    }
		  else
		    move = UP;

		  if (p->type & CONST_OPTION)
		    asn1_delete_structure (&p);

		  p = p2;
		  continue;
		}
	    }

	  if (type_field (p->type) == TYPE_CHOICE)
	    {
	      while (p->down)
		{
		  if (counter < len)
		    ris =
		      _asn1_extract_tag_der (p->down, der + counter,
					     len - counter, &len2);
		  else
		    ris = ASN1_DER_ERROR;
		  if (ris == ASN1_SUCCESS)
		    {
		      while (p->down->right)
			{
			  p2 = p->down->right;
			  asn1_delete_structure (&p2);
			}
		      break;
		    }
		  else if (ris == ASN1_ERROR_TYPE_ANY)
		    {
		      asn1_delete_structure (element);
		      return ASN1_ERROR_TYPE_ANY;
		    }
		  else
		    {
		      p2 = p->down;
		      asn1_delete_structure (&p2);
		    }
		}

	      if (p->down == NULL)
		{
		  if (!(p->type & CONST_OPTION))
		    {
		      asn1_delete_structure (element);
		      return ASN1_DER_ERROR;
		    }
		}
	      else
		p = p->down;
	    }

	  if ((p->type & CONST_OPTION) || (p->type & CONST_DEFAULT))
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if ((len2 != -1) && (counter > len2))
		ris = ASN1_TAG_ERROR;
	    }

	  if (ris == ASN1_SUCCESS)
	    ris =
	      _asn1_extract_tag_der (p, der + counter, len - counter, &len2);
	  if (ris != ASN1_SUCCESS)
	    {
	      if (p->type & CONST_OPTION)
		{
		  p->type |= CONST_NOT_USED;
		  move = RIGHT;
		}
	      else if (p->type & CONST_DEFAULT)
		{
		  _asn1_set_value (p, NULL, 0);
		  move = RIGHT;
		}
	      else
		{
		  if (errorDescription != NULL)
		    _asn1_error_description_tag_error (p, errorDescription);

		  asn1_delete_structure (element);
		  return ASN1_TAG_ERROR;
		}
	    }
	  else
	    counter += len2;
	}

      if (ris == ASN1_SUCCESS)
	{
	  switch (type_field (p->type))
	    {
	    case TYPE_NULL:
	      if (der[counter])
		{
		  asn1_delete_structure (element);
		  return ASN1_DER_ERROR;
		}
	      counter++;
	      move = RIGHT;
	      break;
	    case TYPE_BOOLEAN:
	      if (der[counter++] != 1)
		{
		  asn1_delete_structure (element);
		  return ASN1_DER_ERROR;
		}
	      if (der[counter++] == 0)
		_asn1_set_value (p, "F", 1);
	      else
		_asn1_set_value (p, "T", 1);
	      move = RIGHT;
	      break;
	    case TYPE_INTEGER:
	    case TYPE_ENUMERATED:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      if (len2 + len3 > len - counter)
		return ASN1_DER_ERROR;
	      _asn1_set_value (p, der + counter, len3 + len2);
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_OBJECT_ID:
	      result =
		_asn1_get_objectid_der (der + counter, len - counter, &len2,
					temp, sizeof (temp));
	      if (result != ASN1_SUCCESS)
		{
		  asn1_delete_structure (element);
		  return result;
		}

	      tlen = strlen (temp);
	      if (tlen > 0)
		_asn1_set_value (p, temp, tlen + 1);
	      counter += len2;
	      move = RIGHT;
	      break;
	    case TYPE_TIME:
	      result =
		_asn1_get_time_der (der + counter, len - counter, &len2, temp,
				    sizeof (temp) - 1);
	      if (result != ASN1_SUCCESS)
		{
		  asn1_delete_structure (element);
		  return result;
		}
	      tlen = strlen (temp);
	      if (tlen > 0)
		_asn1_set_value (p, temp, tlen + 1);
	      counter += len2;
	      move = RIGHT;
	      break;
	    case TYPE_OCTET_STRING:
	      len3 = len - counter;
	      ris = _asn1_get_octet_string (der + counter, p, &len3);
	      if (ris != ASN1_SUCCESS)
		return ris;
	      counter += len3;
	      move = RIGHT;
	      break;
	    case TYPE_GENERALSTRING:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      if (len3 + len2 > len - counter)
		return ASN1_DER_ERROR;
	      _asn1_set_value (p, der + counter, len3 + len2);
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_BIT_STRING:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      if (len3 + len2 > len - counter)
		return ASN1_DER_ERROR;
	      _asn1_set_value (p, der + counter, len3 + len2);
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_SEQUENCE:
	    case TYPE_SET:
	      if (move == UP)
		{
		  len2 = strtol (p->value, NULL, 10);
		  _asn1_set_value (p, NULL, 0);
		  if (len2 == -1)
		    {		/* indefinite length method */
		      if (len - counter + 1 > 0)
			{
			  if ((der[counter]) || der[counter + 1])
			    {
			      asn1_delete_structure (element);
			      return ASN1_DER_ERROR;
			    }
			}
		      else
			return ASN1_DER_ERROR;
		      counter += 2;
		    }
		  else
		    {		/* definite length method */
		      if (len2 != counter)
			{
			  asn1_delete_structure (element);
			  return ASN1_DER_ERROR;
			}
		    }
		  move = RIGHT;
		}
	      else
		{		/* move==DOWN || move==RIGHT */
		  len3 =
		    asn1_get_length_der (der + counter, len - counter, &len2);
		  if (len3 < -1)
		    return ASN1_DER_ERROR;
		  counter += len2;
		  if (len3 > 0)
		    {
		      _asn1_ltostr (counter + len3, temp);
		      tlen = strlen (temp);
		      if (tlen > 0)
			_asn1_set_value (p, temp, tlen + 1);
		      move = DOWN;
		    }
		  else if (len3 == 0)
		    {
		      p2 = p->down;
		      while (p2)
			{
			  if (type_field (p2->type) != TYPE_TAG)
			    {
			      p3 = p2->right;
			      asn1_delete_structure (&p2);
			      p2 = p3;
			    }
			  else
			    p2 = p2->right;
			}
		      move = RIGHT;
		    }
		  else
		    {		/* indefinite length method */
		      _asn1_set_value (p, "-1", 3);
		      move = DOWN;
		    }
		}
	      break;
	    case TYPE_SEQUENCE_OF:
	    case TYPE_SET_OF:
	      if (move == UP)
		{
		  len2 = strtol (p->value, NULL, 10);
		  if (len2 == -1)
		    {		/* indefinite length method */
		      if ((counter + 2) > len)
			return ASN1_DER_ERROR;
		      if ((der[counter]) || der[counter + 1])
			{
			  _asn1_append_sequence_set (p);
			  p = p->down;
			  while (p->right)
			    p = p->right;
			  move = RIGHT;
			  continue;
			}
		      _asn1_set_value (p, NULL, 0);
		      counter += 2;
		    }
		  else
		    {		/* definite length method */
		      if (len2 > counter)
			{
			  _asn1_append_sequence_set (p);
			  p = p->down;
			  while (p->right)
			    p = p->right;
			  move = RIGHT;
			  continue;
			}
		      _asn1_set_value (p, NULL, 0);
		      if (len2 != counter)
			{
			  asn1_delete_structure (element);
			  return ASN1_DER_ERROR;
			}
		    }
		}
	      else
		{		/* move==DOWN || move==RIGHT */
		  len3 =
		    asn1_get_length_der (der + counter, len - counter, &len2);
		  if (len3 < -1)
		    return ASN1_DER_ERROR;
		  counter += len2;
		  if (len3)
		    {
		      if (len3 > 0)
			{	/* definite length method */
			  _asn1_ltostr (counter + len3, temp);
			  tlen = strlen (temp);

			  if (tlen > 0)
			    _asn1_set_value (p, temp, tlen + 1);
			}
		      else
			{	/* indefinite length method */
			  _asn1_set_value (p, "-1", 3);
			}
		      p2 = p->down;
		      while ((type_field (p2->type) == TYPE_TAG)
			     || (type_field (p2->type) == TYPE_SIZE))
			p2 = p2->right;
		      if (p2->right == NULL)
			_asn1_append_sequence_set (p);
		      p = p2;
		    }
		}
	      move = RIGHT;
	      break;
	    case TYPE_ANY:
	      if (asn1_get_tag_der
		  (der + counter, len - counter, &class, &len2,
		   &tag) != ASN1_SUCCESS)
		return ASN1_DER_ERROR;
	      if (counter + len2 > len)
		return ASN1_DER_ERROR;
	      len4 =
		asn1_get_length_der (der + counter + len2,
				     len - counter - len2, &len3);
	      if (len4 < -1)
		return ASN1_DER_ERROR;
	      if (len4 > len - counter + len2 + len3)
		return ASN1_DER_ERROR;
	      if (len4 != -1)
		{
		  len2 += len4;
		  _asn1_set_value_octet (p, der + counter, len2 + len3);
		  counter += len2 + len3;
		}
	      else
		{		/* indefinite length */
		  /* Check indefinite lenth method in an EXPLICIT TAG */
		  if ((p->type & CONST_TAG) && (der[counter - 1] == 0x80))
		    indefinite = 1;
		  else
		    indefinite = 0;

		  len2 = len - counter;
		  ris =
		    _asn1_get_indefinite_length_string (der + counter, &len2);
		  if (ris != ASN1_SUCCESS)
		    {
		      asn1_delete_structure (element);
		      return ris;
		    }

		  _asn1_set_value_octet (p, der + counter, len2);
		  counter += len2;

		  /* Check if a couple of 0x00 are present due to an EXPLICIT TAG with
		     an indefinite length method. */
		  if (indefinite)
		    {
		      if (!der[counter] && !der[counter + 1])
			{
			  counter += 2;
			}
		      else
			{
			  asn1_delete_structure (element);
			  return ASN1_DER_ERROR;
			}
		    }
		}
	      move = RIGHT;
	      break;
	    default:
	      move = (move == UP) ? RIGHT : DOWN;
	      break;
	    }
	}

      if (p == node && move != DOWN)
	break;

      if (move == DOWN)
	{
	  if (p->down)
	    p = p->down;
	  else
	    move = RIGHT;
	}
      if ((move == RIGHT) && !(p->type & CONST_SET))
	{
	  if (p->right)
	    p = p->right;
	  else
	    move = UP;
	}
      if (move == UP)
	p = _asn1_find_up (p);
    }

  _asn1_delete_not_used (*element);

  if (counter != len)
    {
      asn1_delete_structure (element);
      return ASN1_DER_ERROR;
    }

  return ASN1_SUCCESS;
}

#define FOUND        1
#define SAME_BRANCH  2
#define OTHER_BRANCH 3
#define EXIT         4

/**
 * asn1_der_decoding_element:
 * @structure: pointer to an ASN1 structure
 * @elementName: name of the element to fill
 * @ider: vector that contains the DER encoding of the whole structure.
 * @len: number of bytes of *der: der[0]..der[len-1]
 * @errorDescription: null-terminated string contains details when an
 *   error occurred.
 *
 * Fill the element named @ELEMENTNAME with values of a DER encoding
 * string.  The structure must just be created with function
 * asn1_create_element().  The DER vector must contain the encoding
 * string of the whole @STRUCTURE.  If an error occurs during the
 * decoding procedure, the *@STRUCTURE is deleted and set equal to
 * %ASN1_TYPE_EMPTY.
 *
 * Returns: %ASN1_SUCCESS if DER encoding OK, %ASN1_ELEMENT_NOT_FOUND
 *   if ELEMENT is %ASN1_TYPE_EMPTY or @elementName == NULL, and
 *   %ASN1_TAG_ERROR or %ASN1_DER_ERROR if the der encoding doesn't
 *   match the structure @structure (*ELEMENT deleted).
 **/
asn1_retCode
asn1_der_decoding_element (ASN1_TYPE * structure, const char *elementName,
			   const void *ider, int len, char *errorDescription)
{
  ASN1_TYPE node, p, p2, p3, nodeFound = ASN1_TYPE_EMPTY;
  char temp[128], currentName[ASN1_MAX_NAME_SIZE * 10], *dot_p, *char_p;
  int nameLen = ASN1_MAX_NAME_SIZE * 10 - 1, state;
  int counter, len2, len3, len4, move, ris, tlen;
  unsigned char class;
  unsigned long tag;
  int indefinite, result;
  const unsigned char *der = ider;

  node = *structure;

  if (node == ASN1_TYPE_EMPTY)
    return ASN1_ELEMENT_NOT_FOUND;

  if (elementName == NULL)
    {
      asn1_delete_structure (structure);
      return ASN1_ELEMENT_NOT_FOUND;
    }

  if (node->type & CONST_OPTION)
    {
      asn1_delete_structure (structure);
      return ASN1_GENERIC_ERROR;
    }

  if ((*structure)->name)
    {				/* Has *structure got a name? */
      nameLen -= strlen ((*structure)->name);
      if (nameLen > 0)
	strcpy (currentName, (*structure)->name);
      else
	{
	  asn1_delete_structure (structure);
	  return ASN1_MEM_ERROR;
	}
      if (!(strcmp (currentName, elementName)))
	{
	  state = FOUND;
	  nodeFound = *structure;
	}
      else if (!memcmp (currentName, elementName, strlen (currentName)))
	state = SAME_BRANCH;
      else
	state = OTHER_BRANCH;
    }
  else
    {				/* *structure doesn't have a name? */
      currentName[0] = 0;
      if (elementName[0] == 0)
	{
	  state = FOUND;
	  nodeFound = *structure;
	}
      else
	{
	  state = SAME_BRANCH;
	}
    }

  counter = 0;
  move = DOWN;
  p = node;
  while (1)
    {

      ris = ASN1_SUCCESS;

      if (move != UP)
	{
	  if (p->type & CONST_SET)
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if (counter == len2)
		{
		  p = p2;
		  move = UP;
		  continue;
		}
	      else if (counter > len2)
		{
		  asn1_delete_structure (structure);
		  return ASN1_DER_ERROR;
		}
	      p2 = p2->down;
	      while (p2)
		{
		  if ((p2->type & CONST_SET) && (p2->type & CONST_NOT_USED))
		    {
		      if (type_field (p2->type) != TYPE_CHOICE)
			ris =
			  _asn1_extract_tag_der (p2, der + counter,
						 len - counter, &len2);
		      else
			{
			  p3 = p2->down;
			  while (p3)
			    {
			      ris =
				_asn1_extract_tag_der (p3, der + counter,
						       len - counter, &len2);
			      if (ris == ASN1_SUCCESS)
				break;
			      p3 = p3->right;
			    }
			}
		      if (ris == ASN1_SUCCESS)
			{
			  p2->type &= ~CONST_NOT_USED;
			  p = p2;
			  break;
			}
		    }
		  p2 = p2->right;
		}
	      if (p2 == NULL)
		{
		  asn1_delete_structure (structure);
		  return ASN1_DER_ERROR;
		}
	    }

	  if ((p->type & CONST_OPTION) || (p->type & CONST_DEFAULT))
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if (counter == len2)
		{
		  if (p->right)
		    {
		      p2 = p->right;
		      move = RIGHT;
		    }
		  else
		    move = UP;

		  if (p->type & CONST_OPTION)
		    asn1_delete_structure (&p);

		  p = p2;
		  continue;
		}
	    }

	  if (type_field (p->type) == TYPE_CHOICE)
	    {
	      while (p->down)
		{
		  if (counter < len)
		    ris =
		      _asn1_extract_tag_der (p->down, der + counter,
					     len - counter, &len2);
		  else
		    ris = ASN1_DER_ERROR;
		  if (ris == ASN1_SUCCESS)
		    {
		      while (p->down->right)
			{
			  p2 = p->down->right;
			  asn1_delete_structure (&p2);
			}
		      break;
		    }
		  else if (ris == ASN1_ERROR_TYPE_ANY)
		    {
		      asn1_delete_structure (structure);
		      return ASN1_ERROR_TYPE_ANY;
		    }
		  else
		    {
		      p2 = p->down;
		      asn1_delete_structure (&p2);
		    }
		}

	      if (p->down == NULL)
		{
		  if (!(p->type & CONST_OPTION))
		    {
		      asn1_delete_structure (structure);
		      return ASN1_DER_ERROR;
		    }
		}
	      else
		p = p->down;
	    }

	  if ((p->type & CONST_OPTION) || (p->type & CONST_DEFAULT))
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if (counter > len2)
		ris = ASN1_TAG_ERROR;
	    }

	  if (ris == ASN1_SUCCESS)
	    ris =
	      _asn1_extract_tag_der (p, der + counter, len - counter, &len2);
	  if (ris != ASN1_SUCCESS)
	    {
	      if (p->type & CONST_OPTION)
		{
		  p->type |= CONST_NOT_USED;
		  move = RIGHT;
		}
	      else if (p->type & CONST_DEFAULT)
		{
		  _asn1_set_value (p, NULL, 0);
		  move = RIGHT;
		}
	      else
		{
		  if (errorDescription != NULL)
		    _asn1_error_description_tag_error (p, errorDescription);

		  asn1_delete_structure (structure);
		  return ASN1_TAG_ERROR;
		}
	    }
	  else
	    counter += len2;
	}

      if (ris == ASN1_SUCCESS)
	{
	  switch (type_field (p->type))
	    {
	    case TYPE_NULL:
	      if (der[counter])
		{
		  asn1_delete_structure (structure);
		  return ASN1_DER_ERROR;
		}

	      if (p == nodeFound)
		state = EXIT;

	      counter++;
	      move = RIGHT;
	      break;
	    case TYPE_BOOLEAN:
	      if (der[counter++] != 1)
		{
		  asn1_delete_structure (structure);
		  return ASN1_DER_ERROR;
		}

	      if (state == FOUND)
		{
		  if (der[counter++] == 0)
		    _asn1_set_value (p, "F", 1);
		  else
		    _asn1_set_value (p, "T", 1);

		  if (p == nodeFound)
		    state = EXIT;

		}
	      else
		counter++;

	      move = RIGHT;
	      break;
	    case TYPE_INTEGER:
	    case TYPE_ENUMERATED:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      if (state == FOUND)
		{
		  if (len3 + len2 > len - counter)
		    return ASN1_DER_ERROR;
		  _asn1_set_value (p, der + counter, len3 + len2);

		  if (p == nodeFound)
		    state = EXIT;
		}
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_OBJECT_ID:
	      if (state == FOUND)
		{
		  result =
		    _asn1_get_objectid_der (der + counter, len - counter,
					    &len2, temp, sizeof (temp));
		  if (result != ASN1_SUCCESS)
		    {
		      return result;
		    }

		  tlen = strlen (temp);

		  if (tlen > 0)
		    _asn1_set_value (p, temp, tlen + 1);

		  if (p == nodeFound)
		    state = EXIT;
		}
	      else
		{
		  len2 =
		    asn1_get_length_der (der + counter, len - counter, &len3);
		  if (len2 < 0)
		    return ASN1_DER_ERROR;
		  len2 += len3;
		}

	      counter += len2;
	      move = RIGHT;
	      break;
	    case TYPE_TIME:
	      if (state == FOUND)
		{
		  result =
		    _asn1_get_time_der (der + counter, len - counter, &len2,
					temp, sizeof (temp) - 1);
		  if (result != ASN1_SUCCESS)
		    {
		      asn1_delete_structure (structure);
		      return result;
		    }

		  tlen = strlen (temp);
		  if (tlen > 0)
		    _asn1_set_value (p, temp, tlen + 1);

		  if (p == nodeFound)
		    state = EXIT;
		}
	      else
		{
		  len2 =
		    asn1_get_length_der (der + counter, len - counter, &len3);
		  if (len2 < 0)
		    return ASN1_DER_ERROR;
		  len2 += len3;
		}

	      counter += len2;
	      move = RIGHT;
	      break;
	    case TYPE_OCTET_STRING:
	      len3 = len - counter;
	      if (state == FOUND)
		{
		  ris = _asn1_get_octet_string (der + counter, p, &len3);
		  if (p == nodeFound)
		    state = EXIT;
		}
	      else
		ris = _asn1_get_octet_string (der + counter, NULL, &len3);

	      if (ris != ASN1_SUCCESS)
		return ris;
	      counter += len3;
	      move = RIGHT;
	      break;
	    case TYPE_GENERALSTRING:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      if (state == FOUND)
		{
		  if (len3 + len2 > len - counter)
		    return ASN1_DER_ERROR;
		  _asn1_set_value (p, der + counter, len3 + len2);

		  if (p == nodeFound)
		    state = EXIT;
		}
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_BIT_STRING:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      if (state == FOUND)
		{
		  if (len3 + len2 > len - counter)
		    return ASN1_DER_ERROR;
		  _asn1_set_value (p, der + counter, len3 + len2);

		  if (p == nodeFound)
		    state = EXIT;
		}
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_SEQUENCE:
	    case TYPE_SET:
	      if (move == UP)
		{
		  len2 = strtol (p->value, NULL, 10);
		  _asn1_set_value (p, NULL, 0);
		  if (len2 == -1)
		    {		/* indefinite length method */
		      if ((der[counter]) || der[counter + 1])
			{
			  asn1_delete_structure (structure);
			  return ASN1_DER_ERROR;
			}
		      counter += 2;
		    }
		  else
		    {		/* definite length method */
		      if (len2 != counter)
			{
			  asn1_delete_structure (structure);
			  return ASN1_DER_ERROR;
			}
		    }
		  if (p == nodeFound)
		    state = EXIT;
		  move = RIGHT;
		}
	      else
		{		/* move==DOWN || move==RIGHT */
		  if (state == OTHER_BRANCH)
		    {
		      len3 =
			asn1_get_length_der (der + counter, len - counter,
					     &len2);
		      if (len3 < 0)
			return ASN1_DER_ERROR;
		      counter += len2 + len3;
		      move = RIGHT;
		    }
		  else
		    {		/*  state==SAME_BRANCH or state==FOUND */
		      len3 =
			asn1_get_length_der (der + counter, len - counter,
					     &len2);
		      if (len3 < 0)
			return ASN1_DER_ERROR;
		      counter += len2;
		      if (len3 > 0)
			{
			  _asn1_ltostr (counter + len3, temp);
			  tlen = strlen (temp);

			  if (tlen > 0)
			    _asn1_set_value (p, temp, tlen + 1);
			  move = DOWN;
			}
		      else if (len3 == 0)
			{
			  p2 = p->down;
			  while (p2)
			    {
			      if (type_field (p2->type) != TYPE_TAG)
				{
				  p3 = p2->right;
				  asn1_delete_structure (&p2);
				  p2 = p3;
				}
			      else
				p2 = p2->right;
			    }
			  move = RIGHT;
			}
		      else
			{	/* indefinite length method */
			  _asn1_set_value (p, "-1", 3);
			  move = DOWN;
			}
		    }
		}
	      break;
	    case TYPE_SEQUENCE_OF:
	    case TYPE_SET_OF:
	      if (move == UP)
		{
		  len2 = strtol (p->value, NULL, 10);
		  if (len2 > counter)
		    {
		      _asn1_append_sequence_set (p);
		      p = p->down;
		      while (p->right)
			p = p->right;
		      move = RIGHT;
		      continue;
		    }
		  _asn1_set_value (p, NULL, 0);
		  if (len2 != counter)
		    {
		      asn1_delete_structure (structure);
		      return ASN1_DER_ERROR;
		    }

		  if (p == nodeFound)
		    state = EXIT;
		}
	      else
		{		/* move==DOWN || move==RIGHT */
		  if (state == OTHER_BRANCH)
		    {
		      len3 =
			asn1_get_length_der (der + counter, len - counter,
					     &len2);
		      if (len3 < 0)
			return ASN1_DER_ERROR;
		      counter += len2 + len3;
		      move = RIGHT;
		    }
		  else
		    {		/* state==FOUND or state==SAME_BRANCH */
		      len3 =
			asn1_get_length_der (der + counter, len - counter,
					     &len2);
		      if (len3 < 0)
			return ASN1_DER_ERROR;
		      counter += len2;
		      if (len3)
			{
			  _asn1_ltostr (counter + len3, temp);
			  tlen = strlen (temp);

			  if (tlen > 0)
			    _asn1_set_value (p, temp, tlen + 1);
			  p2 = p->down;
			  while ((type_field (p2->type) == TYPE_TAG)
				 || (type_field (p2->type) == TYPE_SIZE))
			    p2 = p2->right;
			  if (p2->right == NULL)
			    _asn1_append_sequence_set (p);
			  p = p2;
			  state = FOUND;
			}
		    }
		}

	      break;
	    case TYPE_ANY:
	      if (asn1_get_tag_der
		  (der + counter, len - counter, &class, &len2,
		   &tag) != ASN1_SUCCESS)
		return ASN1_DER_ERROR;
	      if (counter + len2 > len)
		return ASN1_DER_ERROR;

	      len4 =
		asn1_get_length_der (der + counter + len2,
				     len - counter - len2, &len3);
	      if (len4 < -1)
		return ASN1_DER_ERROR;

	      if (len4 != -1)
		{
		  len2 += len4;
		  if (state == FOUND)
		    {
		      _asn1_set_value_octet (p, der + counter, len2 + len3);

		      if (p == nodeFound)
			state = EXIT;
		    }
		  counter += len2 + len3;
		}
	      else
		{		/* indefinite length */
		  /* Check indefinite lenth method in an EXPLICIT TAG */
		  if ((p->type & CONST_TAG) && (der[counter - 1] == 0x80))
		    indefinite = 1;
		  else
		    indefinite = 0;

		  len2 = len - counter;
		  ris =
		    _asn1_get_indefinite_length_string (der + counter, &len2);
		  if (ris != ASN1_SUCCESS)
		    {
		      asn1_delete_structure (structure);
		      return ris;
		    }

		  if (state == FOUND)
		    {
		      _asn1_set_value_octet (p, der + counter, len2);

		      if (p == nodeFound)
			state = EXIT;
		    }

		  counter += len2;

		  /* Check if a couple of 0x00 are present due to an EXPLICIT TAG with
		     an indefinite length method. */
		  if (indefinite)
		    {
		      if (!der[counter] && !der[counter + 1])
			{
			  counter += 2;
			}
		      else
			{
			  asn1_delete_structure (structure);
			  return ASN1_DER_ERROR;
			}
		    }
		}
	      move = RIGHT;
	      break;

	    default:
	      move = (move == UP) ? RIGHT : DOWN;
	      break;
	    }
	}

      if ((p == node && move != DOWN) || (state == EXIT))
	break;

      if (move == DOWN)
	{
	  if (p->down)
	    {
	      p = p->down;

	      if (state != FOUND)
		{
		  nameLen -= strlen (p->name) + 1;
		  if (nameLen > 0)
		    {
		      if (currentName[0])
			strcat (currentName, ".");
		      strcat (currentName, p->name);
		    }
		  else
		    {
		      asn1_delete_structure (structure);
		      return ASN1_MEM_ERROR;
		    }
		  if (!(strcmp (currentName, elementName)))
		    {
		      state = FOUND;
		      nodeFound = p;
		    }
		  else
		    if (!memcmp
			(currentName, elementName, strlen (currentName)))
		    state = SAME_BRANCH;
		  else
		    state = OTHER_BRANCH;
		}
	    }
	  else
	    move = RIGHT;
	}

      if ((move == RIGHT) && !(p->type & CONST_SET))
	{
	  if (p->right)
	    {
	      p = p->right;

	      if (state != FOUND)
		{
		  dot_p = char_p = currentName;
		  while ((char_p = strchr (char_p, '.')))
		    {
		      dot_p = char_p++;
		      dot_p++;
		    }

		  nameLen += strlen (currentName) - (dot_p - currentName);
		  *dot_p = 0;

		  nameLen -= strlen (p->name);
		  if (nameLen > 0)
		    strcat (currentName, p->name);
		  else
		    {
		      asn1_delete_structure (structure);
		      return ASN1_MEM_ERROR;
		    }

		  if (!(strcmp (currentName, elementName)))
		    {
		      state = FOUND;
		      nodeFound = p;
		    }
		  else
		    if (!memcmp
			(currentName, elementName, strlen (currentName)))
		    state = SAME_BRANCH;
		  else
		    state = OTHER_BRANCH;
		}
	    }
	  else
	    move = UP;
	}

      if (move == UP)
	{
	  p = _asn1_find_up (p);

	  if (state != FOUND)
	    {
	      dot_p = char_p = currentName;
	      while ((char_p = strchr (char_p, '.')))
		{
		  dot_p = char_p++;
		  dot_p++;
		}

	      nameLen += strlen (currentName) - (dot_p - currentName);
	      *dot_p = 0;

	      if (!(strcmp (currentName, elementName)))
		{
		  state = FOUND;
		  nodeFound = p;
		}
	      else
		if (!memcmp (currentName, elementName, strlen (currentName)))
		state = SAME_BRANCH;
	      else
		state = OTHER_BRANCH;
	    }
	}
    }

  _asn1_delete_not_used (*structure);

  if (counter > len)
    {
      asn1_delete_structure (structure);
      return ASN1_DER_ERROR;
    }

  return ASN1_SUCCESS;
}

/**
 * asn1_der_decoding_startEnd:
 * @element: pointer to an ASN1 element
 * @ider: vector that contains the DER encoding.
 * @len: number of bytes of *@ider: @ider[0]..@ider[len-1]
 * @name_element: an element of NAME structure.
 * @start: the position of the first byte of NAME_ELEMENT decoding
 *   (@ider[*start])
 * @end: the position of the last byte of NAME_ELEMENT decoding
 *  (@ider[*end])
 *
 * Find the start and end point of an element in a DER encoding
 * string. I mean that if you have a der encoding and you have already
 * used the function asn1_der_decoding() to fill a structure, it may
 * happen that you want to find the piece of string concerning an
 * element of the structure.
 *
 * One example is the sequence "tbsCertificate" inside an X509
 * certificate.
 *
 * Returns: %ASN1_SUCCESS if DER encoding OK, %ASN1_ELEMENT_NOT_FOUND
 *   if ELEMENT is %ASN1_TYPE EMPTY or @name_element is not a valid
 *   element, %ASN1_TAG_ERROR or %ASN1_DER_ERROR if the der encoding
 *   doesn't match the structure ELEMENT.
 **/
asn1_retCode
asn1_der_decoding_startEnd (ASN1_TYPE element, const void *ider, int len,
			    const char *name_element, int *start, int *end)
{
  ASN1_TYPE node, node_to_find, p, p2, p3;
  int counter, len2, len3, len4, move, ris;
  unsigned char class;
  unsigned long tag;
  int indefinite;
  const unsigned char *der = ider;

  node = element;

  if (node == ASN1_TYPE_EMPTY)
    return ASN1_ELEMENT_NOT_FOUND;

  node_to_find = asn1_find_node (node, name_element);

  if (node_to_find == NULL)
    return ASN1_ELEMENT_NOT_FOUND;

  if (node_to_find == node)
    {
      *start = 0;
      *end = len - 1;
      return ASN1_SUCCESS;
    }

  if (node->type & CONST_OPTION)
    return ASN1_GENERIC_ERROR;

  counter = 0;
  move = DOWN;
  p = node;
  while (1)
    {
      ris = ASN1_SUCCESS;

      if (move != UP)
	{
	  if (p->type & CONST_SET)
	    {
	      p2 = _asn1_find_up (p);
	      len2 = strtol (p2->value, NULL, 10);
	      if (len2 == -1)
		{
		  if (!der[counter] && !der[counter + 1])
		    {
		      p = p2;
		      move = UP;
		      counter += 2;
		      continue;
		    }
		}
	      else if (counter == len2)
		{
		  p = p2;
		  move = UP;
		  continue;
		}
	      else if (counter > len2)
		return ASN1_DER_ERROR;
	      p2 = p2->down;
	      while (p2)
		{
		  if ((p2->type & CONST_SET) && (p2->type & CONST_NOT_USED))
		    {		/* CONTROLLARE */
		      if (type_field (p2->type) != TYPE_CHOICE)
			ris =
			  _asn1_extract_tag_der (p2, der + counter,
						 len - counter, &len2);
		      else
			{
			  p3 = p2->down;
			  ris =
			    _asn1_extract_tag_der (p3, der + counter,
						   len - counter, &len2);
			}
		      if (ris == ASN1_SUCCESS)
			{
			  p2->type &= ~CONST_NOT_USED;
			  p = p2;
			  break;
			}
		    }
		  p2 = p2->right;
		}
	      if (p2 == NULL)
		return ASN1_DER_ERROR;
	    }

	  if (p == node_to_find)
	    *start = counter;

	  if (type_field (p->type) == TYPE_CHOICE)
	    {
	      p = p->down;
	      ris =
		_asn1_extract_tag_der (p, der + counter, len - counter,
				       &len2);
	      if (p == node_to_find)
		*start = counter;
	    }

	  if (ris == ASN1_SUCCESS)
	    ris =
	      _asn1_extract_tag_der (p, der + counter, len - counter, &len2);
	  if (ris != ASN1_SUCCESS)
	    {
	      if (p->type & CONST_OPTION)
		{
		  p->type |= CONST_NOT_USED;
		  move = RIGHT;
		}
	      else if (p->type & CONST_DEFAULT)
		{
		  move = RIGHT;
		}
	      else
		{
		  return ASN1_TAG_ERROR;
		}
	    }
	  else
	    counter += len2;
	}

      if (ris == ASN1_SUCCESS)
	{
	  switch (type_field (p->type))
	    {
	    case TYPE_NULL:
	      if (der[counter])
		return ASN1_DER_ERROR;
	      counter++;
	      move = RIGHT;
	      break;
	    case TYPE_BOOLEAN:
	      if (der[counter++] != 1)
		return ASN1_DER_ERROR;
	      counter++;
	      move = RIGHT;
	      break;
	    case TYPE_INTEGER:
	    case TYPE_ENUMERATED:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_OBJECT_ID:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      counter += len2 + len3;
	      move = RIGHT;
	      break;
	    case TYPE_TIME:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      counter += len2 + len3;
	      move = RIGHT;
	      break;
	    case TYPE_OCTET_STRING:
	      len3 = len - counter;
	      ris = _asn1_get_octet_string (der + counter, NULL, &len3);
	      if (ris != ASN1_SUCCESS)
		return ris;
	      counter += len3;
	      move = RIGHT;
	      break;
	    case TYPE_GENERALSTRING:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_BIT_STRING:
	      len2 =
		asn1_get_length_der (der + counter, len - counter, &len3);
	      if (len2 < 0)
		return ASN1_DER_ERROR;
	      counter += len3 + len2;
	      move = RIGHT;
	      break;
	    case TYPE_SEQUENCE:
	    case TYPE_SET:
	      if (move != UP)
		{
		  len3 =
		    asn1_get_length_der (der + counter, len - counter, &len2);
		  if (len3 < -1)
		    return ASN1_DER_ERROR;
		  counter += len2;
		  if (len3 == 0)
		    move = RIGHT;
		  else
		    move = DOWN;
		}
	      else
		{
		  if (!der[counter] && !der[counter + 1])	/* indefinite length method */
		    counter += 2;
		  move = RIGHT;
		}
	      break;
	    case TYPE_SEQUENCE_OF:
	    case TYPE_SET_OF:
	      if (move != UP)
		{
		  len3 =
		    asn1_get_length_der (der + counter, len - counter, &len2);
		  if (len3 < -1)
		    return ASN1_DER_ERROR;
		  counter += len2;
		  if ((len3 == -1) && !der[counter] && !der[counter + 1])
		    counter += 2;
		  else if (len3)
		    {
		      p2 = p->down;
		      while ((type_field (p2->type) == TYPE_TAG) ||
			     (type_field (p2->type) == TYPE_SIZE))
			p2 = p2->right;
		      p = p2;
		    }
		}
	      else
		{
		  if (!der[counter] && !der[counter + 1])	/* indefinite length method */
		    counter += 2;
		}
	      move = RIGHT;
	      break;
	    case TYPE_ANY:
	      if (asn1_get_tag_der
		  (der + counter, len - counter, &class, &len2,
		   &tag) != ASN1_SUCCESS)
		return ASN1_DER_ERROR;
	      if (counter + len2 > len)
		return ASN1_DER_ERROR;

	      len4 =
		asn1_get_length_der (der + counter + len2,
				     len - counter - len2, &len3);
	      if (len4 < -1)
		return ASN1_DER_ERROR;

	      if (len4 != -1)
		{
		  counter += len2 + len4 + len3;
		}
	      else
		{		/* indefinite length */
		  /* Check indefinite lenth method in an EXPLICIT TAG */
		  if ((p->type & CONST_TAG) && (der[counter - 1] == 0x80))
		    indefinite = 1;
		  else
		    indefinite = 0;

		  len2 = len - counter;
		  ris =
		    _asn1_get_indefinite_length_string (der + counter, &len2);
		  if (ris != ASN1_SUCCESS)
		    return ris;
		  counter += len2;

		  /* Check if a couple of 0x00 are present due to an EXPLICIT TAG with
		     an indefinite length method. */
		  if (indefinite)
		    {
		      if (!der[counter] && !der[counter + 1])
			counter += 2;
		      else
			return ASN1_DER_ERROR;
		    }
		}
	      move = RIGHT;
	      break;
	    default:
	      move = (move == UP) ? RIGHT : DOWN;
	      break;
	    }
	}

      if ((p == node_to_find) && (move == RIGHT))
	{
	  *end = counter - 1;
	  return ASN1_SUCCESS;
	}

      if (p == node && move != DOWN)
	break;

      if (move == DOWN)
	{
	  if (p->down)
	    p = p->down;
	  else
	    move = RIGHT;
	}
      if ((move == RIGHT) && !(p->type & CONST_SET))
	{
	  if (p->right)
	    p = p->right;
	  else
	    move = UP;
	}
      if (move == UP)
	p = _asn1_find_up (p);
    }

  return ASN1_ELEMENT_NOT_FOUND;
}

/**
 * asn1_expand_any_defined_by:
 * @definitions: ASN1 definitions
 * @element: pointer to an ASN1 structure
 *
 * Expands every "ANY DEFINED BY" element of a structure created from
 * a DER decoding process (asn1_der_decoding function). The element
 * ANY must be defined by an OBJECT IDENTIFIER. The type used to
 * expand the element ANY is the first one following the definition of
 * the actual value of the OBJECT IDENTIFIER.
 *
 * Returns: %ASN1_SUCCESS if Substitution OK, %ASN1_ERROR_TYPE_ANY if
 *   some "ANY DEFINED BY" element couldn't be expanded due to a
 *   problem in OBJECT_ID -> TYPE association, or other error codes
 *   depending on DER decoding.
 **/
asn1_retCode
asn1_expand_any_defined_by (ASN1_TYPE definitions, ASN1_TYPE * element)
{
  char definitionsName[ASN1_MAX_NAME_SIZE], name[2 * ASN1_MAX_NAME_SIZE + 1],
    value[ASN1_MAX_NAME_SIZE];
  asn1_retCode retCode = ASN1_SUCCESS, result;
  int len, len2, len3;
  ASN1_TYPE p, p2, p3, aux = ASN1_TYPE_EMPTY;
  char errorDescription[ASN1_MAX_ERROR_DESCRIPTION_SIZE];

  if ((definitions == ASN1_TYPE_EMPTY) || (*element == ASN1_TYPE_EMPTY))
    return ASN1_ELEMENT_NOT_FOUND;

  strcpy (definitionsName, definitions->name);
  strcat (definitionsName, ".");

  p = *element;
  while (p)
    {

      switch (type_field (p->type))
	{
	case TYPE_ANY:
	  if ((p->type & CONST_DEFINED_BY) && (p->value))
	    {
	      /* search the "DEF_BY" element */
	      p2 = p->down;
	      while ((p2) && (type_field (p2->type) != TYPE_CONSTANT))
		p2 = p2->right;

	      if (!p2)
		{
		  retCode = ASN1_ERROR_TYPE_ANY;
		  break;
		}

	      p3 = _asn1_find_up (p);

	      if (!p3)
		{
		  retCode = ASN1_ERROR_TYPE_ANY;
		  break;
		}

	      p3 = p3->down;
	      while (p3)
		{
		  if ((p3->name) && !(strcmp (p3->name, p2->name)))
		    break;
		  p3 = p3->right;
		}

	      if ((!p3) || (type_field (p3->type) != TYPE_OBJECT_ID) ||
		  (p3->value == NULL))
		{

		  p3 = _asn1_find_up (p);
		  p3 = _asn1_find_up (p3);

		  if (!p3)
		    {
		      retCode = ASN1_ERROR_TYPE_ANY;
		      break;
		    }

		  p3 = p3->down;

		  while (p3)
		    {
		      if ((p3->name) && !(strcmp (p3->name, p2->name)))
			break;
		      p3 = p3->right;
		    }

		  if ((!p3) || (type_field (p3->type) != TYPE_OBJECT_ID) ||
		      (p3->value == NULL))
		    {
		      retCode = ASN1_ERROR_TYPE_ANY;
		      break;
		    }
		}

	      /* search the OBJECT_ID into definitions */
	      p2 = definitions->down;
	      while (p2)
		{
		  if ((type_field (p2->type) == TYPE_OBJECT_ID) &&
		      (p2->type & CONST_ASSIGN))
		    {
		      strcpy (name, definitionsName);
		      strcat (name, p2->name);

		      len = ASN1_MAX_NAME_SIZE;
		      result =
			asn1_read_value (definitions, name, value, &len);

		      if ((result == ASN1_SUCCESS)
			  && (!strcmp (p3->value, value)))
			{
			  p2 = p2->right;	/* pointer to the structure to
						   use for expansion */
			  while ((p2) && (p2->type & CONST_ASSIGN))
			    p2 = p2->right;

			  if (p2)
			    {
			      strcpy (name, definitionsName);
			      strcat (name, p2->name);

			      result =
				asn1_create_element (definitions, name, &aux);
			      if (result == ASN1_SUCCESS)
				{
				  _asn1_set_name (aux, p->name);
				  len2 =
				    asn1_get_length_der (p->value,
							 p->value_len, &len3);
				  if (len2 < 0)
				    return ASN1_DER_ERROR;

				  result =
				    asn1_der_decoding (&aux, p->value + len3,
						       len2,
						       errorDescription);
				  if (result == ASN1_SUCCESS)
				    {

				      _asn1_set_right (aux, p->right);
				      _asn1_set_right (p, aux);

				      result = asn1_delete_structure (&p);
				      if (result == ASN1_SUCCESS)
					{
					  p = aux;
					  aux = ASN1_TYPE_EMPTY;
					  break;
					}
				      else
					{	/* error with asn1_delete_structure */
					  asn1_delete_structure (&aux);
					  retCode = result;
					  break;
					}
				    }
				  else
				    {	/* error with asn1_der_decoding */
				      retCode = result;
				      break;
				    }
				}
			      else
				{	/* error with asn1_create_element */
				  retCode = result;
				  break;
				}
			    }
			  else
			    {	/* error with the pointer to the structure to exapand */
			      retCode = ASN1_ERROR_TYPE_ANY;
			      break;
			    }
			}
		    }
		  p2 = p2->right;
		}		/* end while */

	      if (!p2)
		{
		  retCode = ASN1_ERROR_TYPE_ANY;
		  break;
		}

	    }
	  break;
	default:
	  break;
	}


      if (p->down)
	{
	  p = p->down;
	}
      else if (p == *element)
	{
	  p = NULL;
	  break;
	}
      else if (p->right)
	p = p->right;
      else
	{
	  while (1)
	    {
	      p = _asn1_find_up (p);
	      if (p == *element)
		{
		  p = NULL;
		  break;
		}
	      if (p->right)
		{
		  p = p->right;
		  break;
		}
	    }
	}
    }

  return retCode;
}

/**
 * asn1_expand_octet_string:
 * @definitions: ASN1 definitions
 * @element: pointer to an ASN1 structure
 * @octetName: name of the OCTECT STRING field to expand.
 * @objectName: name of the OBJECT IDENTIFIER field to use to define
 *    the type for expansion.
 *
 * Expands an "OCTET STRING" element of a structure created from a DER
 * decoding process (the asn1_der_decoding() function).  The type used
 * for expansion is the first one following the definition of the
 * actual value of the OBJECT IDENTIFIER indicated by OBJECTNAME.
 *
 * Returns: %ASN1_SUCCESS if substitution OK, %ASN1_ELEMENT_NOT_FOUND
 *   if @objectName or @octetName are not correct,
 *   %ASN1_VALUE_NOT_VALID if it wasn't possible to find the type to
 *   use for expansion, or other errors depending on DER decoding.
 **/
asn1_retCode
asn1_expand_octet_string (ASN1_TYPE definitions, ASN1_TYPE * element,
			  const char *octetName, const char *objectName)
{
  char name[2 * ASN1_MAX_NAME_SIZE + 1], value[ASN1_MAX_NAME_SIZE];
  asn1_retCode retCode = ASN1_SUCCESS, result;
  int len, len2, len3;
  ASN1_TYPE p2, aux = ASN1_TYPE_EMPTY;
  ASN1_TYPE octetNode = ASN1_TYPE_EMPTY, objectNode = ASN1_TYPE_EMPTY;
  char errorDescription[ASN1_MAX_ERROR_DESCRIPTION_SIZE];

  if ((definitions == ASN1_TYPE_EMPTY) || (*element == ASN1_TYPE_EMPTY))
    return ASN1_ELEMENT_NOT_FOUND;

  octetNode = asn1_find_node (*element, octetName);
  if (octetNode == ASN1_TYPE_EMPTY)
    return ASN1_ELEMENT_NOT_FOUND;
  if (type_field (octetNode->type) != TYPE_OCTET_STRING)
    return ASN1_ELEMENT_NOT_FOUND;
  if (octetNode->value == NULL)
    return ASN1_VALUE_NOT_FOUND;

  objectNode = asn1_find_node (*element, objectName);
  if (objectNode == ASN1_TYPE_EMPTY)
    return ASN1_ELEMENT_NOT_FOUND;

  if (type_field (objectNode->type) != TYPE_OBJECT_ID)
    return ASN1_ELEMENT_NOT_FOUND;

  if (objectNode->value == NULL)
    return ASN1_VALUE_NOT_FOUND;


  /* search the OBJECT_ID into definitions */
  p2 = definitions->down;
  while (p2)
    {
      if ((type_field (p2->type) == TYPE_OBJECT_ID) &&
	  (p2->type & CONST_ASSIGN))
	{
	  strcpy (name, definitions->name);
	  strcat (name, ".");
	  strcat (name, p2->name);

	  len = sizeof (value);
	  result = asn1_read_value (definitions, name, value, &len);

	  if ((result == ASN1_SUCCESS)
	      && (!strcmp (objectNode->value, value)))
	    {

	      p2 = p2->right;	/* pointer to the structure to
				   use for expansion */
	      while ((p2) && (p2->type & CONST_ASSIGN))
		p2 = p2->right;

	      if (p2)
		{
		  strcpy (name, definitions->name);
		  strcat (name, ".");
		  strcat (name, p2->name);

		  result = asn1_create_element (definitions, name, &aux);
		  if (result == ASN1_SUCCESS)
		    {
		      _asn1_set_name (aux, octetNode->name);
		      len2 =
			asn1_get_length_der (octetNode->value,
					     octetNode->value_len, &len3);
		      if (len2 < 0)
			return ASN1_DER_ERROR;

		      result =
			asn1_der_decoding (&aux, octetNode->value + len3,
					   len2, errorDescription);
		      if (result == ASN1_SUCCESS)
			{

			  _asn1_set_right (aux, octetNode->right);
			  _asn1_set_right (octetNode, aux);

			  result = asn1_delete_structure (&octetNode);
			  if (result == ASN1_SUCCESS)
			    {
			      aux = ASN1_TYPE_EMPTY;
			      break;
			    }
			  else
			    {	/* error with asn1_delete_structure */
			      asn1_delete_structure (&aux);
			      retCode = result;
			      break;
			    }
			}
		      else
			{	/* error with asn1_der_decoding */
			  retCode = result;
			  break;
			}
		    }
		  else
		    {		/* error with asn1_create_element */
		      retCode = result;
		      break;
		    }
		}
	      else
		{		/* error with the pointer to the structure to exapand */
		  retCode = ASN1_VALUE_NOT_VALID;
		  break;
		}
	    }
	}

      p2 = p2->right;

    }

  if (!p2)
    retCode = ASN1_VALUE_NOT_VALID;

  return retCode;
}
