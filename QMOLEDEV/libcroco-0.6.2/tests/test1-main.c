/* -*- Mode: C; indent-tabs-mode:nil; c-basic-offset:8 -*- */

/*
 * This file is part of The Croco Library
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *
 * See COPYRIGHTS file for copyright information
 */

#include <stdio.h>
#include "cr-input.h"
#include "string.h"

/**
 *@file
 *Some test facilities for the #CRInput class.
 */

/**
 *The options data structure.
 *The variable of this data structure are set
 *during the  parsing the command line by the
 *parse_command_line() function.
 */
struct Options {
        gboolean display_help;
        gboolean display_about;
        gchar **files_list;
};

static void
  display_help (char *prg_name);

static void
  display_about (char *prg_name);

static void
  parse_command_line (int a_argc, char **a_argv, struct Options *a_options);

static enum CRStatus
  test_cr_input_read_char (guchar * a_file_uri);

/**
 *Displays the usage of the test
 *facility.
 *@param a_argc the argc variable passed to the main function.
 *@param a_argv the argv variable passed to the main function.
 */
static void
display_help (char *prg_name)
{
        g_print ("\n\n");
        g_print ("usage: %s <file-to-parse>\n", prg_name);
        g_print ("\t <file-to-parse>: the file to parse\n");
        g_print ("\n\n");
        g_print ("This test just reads the file character per character\nand sends each character to stdout\n");
        g_print ("\n\n");
}

/**
 *Displays the about text.
 *@param a_argc the argc variable passed to the main function.
 *@param a_argv the argv variable passed to the main function.
 */
static void
display_about (char *prg_name)
{
        g_print ("\n\n");
        g_print ("%s is a libcroco CRInput class test program.\n", prg_name);
        g_print ("It should run on GNU compliants systems.\n");
        g_print ("\n\n");
        g_print ("Initial author: Dodji Seketeli <dodji@seketeli.org>.\n");
        g_print ("\n\n");
}

/**
 *Parses the command line and updates an abstract "options" data structure.
 *@param a_argc the argc variable passed to the main function by the OS.
 *@param a_argv the argv variable passed to the main function by the OS.
 *@param a_options out parameter. The abstraction of the parsed the options.
 */
static void
parse_command_line (int a_argc, char **a_argv, struct Options *a_options)
{
        int i = 0;

        g_return_if_fail (a_options);

        memset (a_options, 0, sizeof (struct Options));

        for (i = 1; i < a_argc; i++) {
                if (a_argv[i][0] != '-')
                        break;

                if (!strcmp (a_argv[i], "-h")
                    || !strcmp (a_argv[i], "--help")) {
                        a_options->display_help = TRUE;
                }
                if (!strcmp (a_argv[i], "--about")) {
                        a_options->display_about = TRUE;
                }
        }

        if (i >= a_argc) {
                /*No file parameter where given */
                a_options->files_list = NULL;
        } else {
                a_options->files_list = &a_argv[i];
        }
}

/**
 *The test of the cr_input_read_byte() method.
 *Reads the each byte of a_file_uri using the
 *cr_input_read_byte() method. Each byte is send to
 *stdout.
 *@param a_file_uri the file to read.
 *@return CR_OK upon successfull completion of the
 *function, an error code otherwise.
 */
enum CRStatus
test_cr_input_read_char (guchar * a_file_uri)
{
        enum CRStatus status = CR_OK;
        CRInput *input = NULL;
        guint32 c = 0;

        g_return_val_if_fail (a_file_uri, CR_BAD_PARAM_ERROR);

        input = cr_input_new_from_uri (a_file_uri, CR_UTF_8);

        if (!input) {
                cr_utils_trace_debug ("Input Stream creation failed.");
                return CR_ERROR;
        }

        for (status = CR_OK; status == CR_OK;) {
                status = cr_input_read_char (input, &c);

                if (status == CR_OK) {
                        printf ("%c", c);
                        fflush (stdout);
                }
        }

        if (status == CR_END_OF_INPUT_ERROR) {
                status = CR_OK;
        }

        cr_input_destroy (input);
        input = NULL;

        return status;
}

/**
 *The entry point of the testing routine.
 */
int
main (int argc, char **argv)
{
        struct Options options;

        parse_command_line (argc, argv, &options);

        if (options.display_help == TRUE) {
                display_help (argv[0]);
                return 0;
        }

        if (options.display_about == TRUE) {
                display_about (argv[0]);
                return 0;
        }

        if (options.files_list == NULL) {
                display_help (argv[0]);
                return 0;
        }

        test_cr_input_read_char (options.files_list[0]);

        return 0;
}
