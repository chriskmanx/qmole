/*
 * Copyright (C) 2002, 2006, 2007, 2008, 2009, 2010, 2011 Free Software
 * Foundation, Inc.
 *
 * This file is part of LIBTASN1.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/******************************************************/
/* File: Test_encoding.c                              */
/* Description: Test writing values and DER encoding. */
/******************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "libtasn1.h"


unsigned char data[256];
int data_size = sizeof (data);


int
main (int argc, char *argv[])
{
  asn1_retCode result;
  ASN1_TYPE definitions = ASN1_TYPE_EMPTY;
  ASN1_TYPE asn1_element = ASN1_TYPE_EMPTY;
  char errorDescription[ASN1_MAX_ERROR_DESCRIPTION_SIZE];
  const char *treefile = getenv ("ASN1ENCODING");

  if (!treefile)
    treefile = "Test_encoding.asn";

  printf ("\n\n/****************************************/\n");
  printf ("/*     Test sequence : coding-decoding  */\n");
  printf ("/****************************************/\n\n");

  /* Check version */
  if (asn1_check_version ("0.3.3") == NULL)
    printf ("\nLibrary version check ERROR:\n actual version: %s\n\n",
	    asn1_check_version (NULL));

  result = asn1_parser2tree (treefile, &definitions, errorDescription);

  if (result != ASN1_SUCCESS)
    {
      asn1_perror (result);
      printf ("ErrorDescription = %s\n\n", errorDescription);
      exit (1);
    }

  result = asn1_create_element (definitions, "TEST_TREE.Koko", &asn1_element);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "asn1_create_element(): ");
      asn1_perror (result);
      exit (1);
    }

  result = asn1_write_value (asn1_element, "seqint", "NEW", 1);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "asn1_write_value(): seqint ");
      asn1_perror (result);
      exit (1);
    }

  result = asn1_write_value (asn1_element, "seqint.?LAST", "1234", 0);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "asn1_write_value(): seqint.?LAST ");
      asn1_perror (result);
      exit (1);
    }

  result = asn1_write_value (asn1_element, "int", "\x0f\xff\x01", 3);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "asn1_write_value(): int ");
      asn1_perror (result);
      exit (1);
    }

  result = asn1_write_value (asn1_element, "str", "string", 6);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "asn1_write_value(): str ");
      asn1_perror (result);
      exit (1);
    }

  /* Clear the definition structures */
  asn1_delete_structure (&definitions);

  result = asn1_der_coding (asn1_element, "", data, &data_size, NULL);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "Encoding error.\n");
      asn1_perror (result);
      exit (1);
    }

  result = asn1_der_decoding (&asn1_element, data, data_size, NULL);
  if (result != ASN1_SUCCESS)
    {
      fprintf (stderr, "Decoding error.\n");
      asn1_perror (result);
      exit (1);
    }

  asn1_delete_structure (&asn1_element);

  printf ("Success\n");
  exit (0);
}
