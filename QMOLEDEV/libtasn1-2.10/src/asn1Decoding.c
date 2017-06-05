/* asn1Decoding.c ---  program to generate an ASN1 type from a DER coding.
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

#include <config.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#include <libtasn1.h>

#include <progname.h>
#include <version-etc.h>
#include <read-file.h>

/* This feature is available in gcc versions 2.5 and later.  */
#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 5)
# define ATTR_NO_RETRUN
#else
# define ATTR_NO_RETRUN __attribute__ ((__noreturn__))
#endif

ATTR_NO_RETRUN static void
usage (int status)
{
  if (status != EXIT_SUCCESS)
    fprintf (stderr, "Try `%s --help' for more information.\n", program_name);
  else
    {
      printf ("\
Usage: %s [OPTION] DEFINITIONS ENCODED ASN1TYPE\n", program_name);
      printf ("\
Decodes DER data in ENCODED file, for the ASN1TYPE element\n\
described in ASN.1 DEFINITIONS file, and print decoded structures.\n\
\n");
      printf ("\
  -h, --help            display this help and exit\n\
  -v, --version         output version information and exit\n");
      emit_bug_reporting_address ();
    }
  exit (status);
}

int
main (int argc, char *argv[])
{
  static const struct option long_options[] = {
    {"help", no_argument, 0, 'h'},
    {"version", no_argument, 0, 'v'},
    {0, 0, 0, 0}
  };
  int option_index = 0;
  int option_result;
  char *inputFileAsnName = NULL;
  char *inputFileDerName = NULL;
  char *typeName = NULL;
  ASN1_TYPE definitions = ASN1_TYPE_EMPTY;
  ASN1_TYPE structure = ASN1_TYPE_EMPTY;
  char errorDescription[ASN1_MAX_ERROR_DESCRIPTION_SIZE];
  int asn1_result = ASN1_SUCCESS;
  unsigned char *der;
  int der_len = 0;
  /* FILE *outputFile; */

  set_program_name (argv[0]);

  opterr = 0;			/* disable error messages from getopt */

  while (1)
    {

      option_result =
	getopt_long (argc, argv, "hvc", long_options, &option_index);

      if (option_result == -1)
	break;

      switch (option_result)
	{
	case 'h':		/* HELP */
	  usage (EXIT_SUCCESS);
	  break;
	case 'v':		/* VERSION */
	  version_etc (stdout, program_name, PACKAGE, VERSION,
		       "Fabio Fiorina", NULL);
	  exit (0);
	  break;
	case '?':		/* UNKNOW OPTION */
	  fprintf (stderr,
		   "asn1Decoding: option '%s' not recognized or without argument.\n\n",
		   argv[optind - 1]);
	  usage (EXIT_FAILURE);
	  break;
	default:
	  fprintf (stderr,
		   "asn1Decoding: ?? getopt returned character code Ox%x ??\n",
		   option_result);
	}
    }

  if (optind == argc || optind == argc - 1 || optind == argc - 2)
    {
      fprintf (stderr, "asn1Decoding: input files or ASN.1 type "
	       "name missing\n");
      usage (EXIT_FAILURE);
    }

  inputFileAsnName = (char *) malloc (strlen (argv[optind]) + 1);
  strcpy (inputFileAsnName, argv[optind]);

  inputFileDerName = (char *) malloc (strlen (argv[optind + 1]) + 1);
  strcpy (inputFileDerName, argv[optind + 1]);

  typeName = (char *) malloc (strlen (argv[optind + 2]) + 1);
  strcpy (typeName, argv[optind + 2]);

  asn1_result =
    asn1_parser2tree (inputFileAsnName, &definitions, errorDescription);

  switch (asn1_result)
    {
    case ASN1_SUCCESS:
      printf ("Parse: done.\n");
      break;
    case ASN1_FILE_NOT_FOUND:
      printf ("asn1Decoding: FILE %s NOT FOUND\n", inputFileAsnName);
      break;
    case ASN1_SYNTAX_ERROR:
    case ASN1_IDENTIFIER_NOT_FOUND:
    case ASN1_NAME_TOO_LONG:
      printf ("asn1Decoding: %s\n", errorDescription);
      break;
    default:
      printf ("libtasn1 ERROR: %s\n", asn1_strerror (asn1_result));
    }

  if (asn1_result != ASN1_SUCCESS)
    {
      free (inputFileAsnName);
      free (inputFileDerName);
      free (typeName);
      exit (1);
    }


  {
    size_t tmplen;
    der = (unsigned char *) read_binary_file (inputFileDerName, &tmplen);
    der_len = tmplen;
  }

  if (der == NULL)
    {
      printf ("asn1Decoding: could not read '%s'\n", inputFileDerName);
      asn1_delete_structure (&definitions);

      free (inputFileAsnName);
      free (inputFileDerName);
      free (typeName);
      exit (1);
    }


 /*****************************************/
  /* ONLY FOR TEST                         */
 /*****************************************/
  /*
     der_len=0;
     outputFile=fopen("data.p12","w");
     while(fscanf(inputFile,"%c",der+der_len) != EOF){
     if((der_len>=0x11) && (der_len<=(0xe70)))
     fprintf(outputFile,"%c",der[der_len]);
     der_len++;
     }
     fclose(outputFile);
     fclose(inputFile);
   */

  asn1_result = asn1_create_element (definitions, typeName, &structure);

  /* asn1_print_structure(stdout,structure,"",ASN1_PRINT_ALL); */


  if (asn1_result != ASN1_SUCCESS)
    {
      printf ("Structure creation: %s\n", asn1_strerror (asn1_result));
      asn1_delete_structure (&definitions);

      free (inputFileAsnName);
      free (inputFileDerName);
      free (typeName);
      free (der);
      exit (1);
    }

  asn1_result =
    asn1_der_decoding (&structure, der, der_len, errorDescription);
  printf ("\nDecoding: %s\n", asn1_strerror (asn1_result));
  if (asn1_result != ASN1_SUCCESS)
    printf ("asn1Decoding: %s\n", errorDescription);

  printf ("\nDECODING RESULT:\n");
  asn1_print_structure (stdout, structure, "", ASN1_PRINT_NAME_TYPE_VALUE);

 /*****************************************/
  /* ONLY FOR TEST                         */
 /*****************************************/
  /*
     der_len=10000;
     option_index=0;
     asn1_result=asn1_read_value(structure,"?2.content",der,&der_len);
     outputFile=fopen("encryptedData.p12","w");
     while(der_len>0){
     fprintf(outputFile,"%c",der[option_index]);
     der_len--;
     option_index++;
     }
     fclose(outputFile);
   */

  asn1_delete_structure (&definitions);
  asn1_delete_structure (&structure);

  free (der);

  free (inputFileAsnName);
  free (inputFileDerName);
  free (typeName);

  if (asn1_result != ASN1_SUCCESS)
    exit (1);

  exit (0);
}
