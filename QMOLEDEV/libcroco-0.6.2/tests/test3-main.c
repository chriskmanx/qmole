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
 * Author: Dodji Seketeli
 * See COPYRIGHTS file for copyright information.
 */

#include <stdio.h>
#include <string.h>
#include "cr-test-utils.h"
#include "cr-parser.h"

/**
 *@file
 *Some test facilities for the #CRParser class.
 */

CRDocHandler *gv_test_handler = { 0 };

static void display_help (char *prg_name);

static void display_about (char *prg_name);

static enum CRStatus test_cr_parser_parse (guchar * a_file_uri);

/**
 *Displays the usage of the test
 *facility.
 *@param a_argc the argc variable passed to the main function.
 *@param a_argv the argv variable passed to the main function.
 */
static void
display_help (char *prg_name)
{
        fprintf (stdout, "\n\n");
        fprintf (stdout, "usage: %s <file-to-parse>\n", prg_name);
        fprintf (stdout, "\t <file-to-parse>: the file to parse\n");
        fprintf (stdout, "\n\n");
        fprintf (stdout, "Tests the cr_parser_parse () method.\n");
        fprintf (stdout, "Tests the parsing following the css core syntax\n");
        fprintf (stdout, "Returns OK if the status is CR_OK, KO otherwise\n");
        fprintf (stdout, "\n\n");
}

/**
 *Displays the about text.
 *@param a_argc the argc variable passed to the main function.
 *@param a_argv the argv variable passed to the main function.
 */
static void
display_about (char *prg_name)
{
        fprintf (stdout, "\n\n");
        fprintf (stdout, "%s is a libcroco CRParser class test program.\n",
                 prg_name);
        fprintf (stdout, "It should run on GNU compliants systems.\n");
        fprintf (stdout, "\n\n");
        fprintf (stdout,
                 "Initial author: Dodji Seketeli <dodji@seketeli.org>.\n");
        fprintf (stdout, "\n\n");
}

/***************************
 *Some SAC document handlers
 *for TEST PURPOSES.
 ***************************/

static void
test_start_document (CRDocHandler * a_handler)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "start_document\n");
        fprintf (stdout, "***************\n\n");
}

static void
test_end_document (CRDocHandler * a_handler)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "end_document\n");
        fprintf (stdout, "***************\n\n");
}

static void
test_import_style (CRDocHandler * a_handler,
                   GList * a_media_list, CRString * a_uri,
                   CRString * a_uri_default_ns,
                   CRParsingLocation *a_location)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "****************\n");
        fprintf (stdout, "import_style\n");

        if (a_media_list) {
                GList *cur = NULL;

                fprintf (stdout, "\nmedia list:\n");
                fprintf (stdout, "-------------\n");

                for (cur = a_media_list; cur; cur = cur->next) {
                        if (cur->data) {
                                guchar *str =
                                        cr_string_dup2 ((CRString *) cur->data);
                                if (str) {
                                        fprintf (stdout, str);
                                        fprintf (stdout, "\n");
                                        g_free (str);
                                        str = NULL;
                                }
                        }
                }
                fprintf (stdout, "\ndefault namespace:\n");
                fprintf (stdout, "--------------------\n");

                if (a_uri_default_ns) {
                        guchar *str = cr_string_dup2 (a_uri_default_ns) ;
                        if (str) {
                                fprintf (stdout, str);
                                fprintf (stdout, "\n");
                                g_free (str);
                                str = NULL;
                        }
                }
        }
        fprintf (stdout, "******************\n\n");
        a_uri = NULL;           /*keep compiler happy */
}

static void
test_namespace_declaration (CRDocHandler * a_handler,
                            CRString * a_prefix, 
                            CRString * a_uri,
                            CRParsingLocation *a_location)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "namespace_declaration:\n");

        if (a_prefix) {
                guchar *prefix = NULL;

                prefix = cr_string_dup2 (a_prefix);
                if (prefix) {
                        fprintf (stdout, "prefix: %s\n", prefix);
                        g_free (prefix);
                        prefix = NULL;
                }
        }

        if (a_uri) {
                guchar *uri = NULL;
                uri = cr_string_dup2 (a_uri) ;
                if (uri) {
                        fprintf (stdout, "uri: %s\n", uri);
                        g_free (uri);
                        uri = NULL;
                }
        }
        fprintf (stdout, "\n");

        fprintf (stdout, "***************\n\n");

}

static void
test_comment (CRDocHandler * a_handler, 
              CRString * a_comment)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "comment:\n");

        if (a_comment) {
                guchar *comment = NULL;

                comment = cr_string_dup2 (a_comment);

                if (comment) {
                        fprintf (stdout, "\n/*----------------------\n");
                        fprintf (stdout, "%s\n", comment);
                        fprintf (stdout, "-------------------------*/\n");
                        g_free (comment);
                        comment = NULL;
                }
        }
        fprintf (stdout, "***************\n\n");
}

static void
test_start_selector (CRDocHandler * a_handler, 
                     CRSelector * a_selector_list)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "start_selector\n");

        if (a_selector_list) {
                cr_selector_dump (a_selector_list, stdout);
                fprintf (stdout, "\n");
        }

        fprintf (stdout, "***************\n\n");
}

static void
test_end_selector (CRDocHandler * a_handler, 
                   CRSelector * a_selector_list)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "end_selector\n");

        if (a_selector_list) {
                cr_selector_dump (a_selector_list, stdout);
                fprintf (stdout, "\n");
        }

        fprintf (stdout, "***************\n\n");
}

static void
test_property (CRDocHandler * a_handler, CRString * a_name,
               CRTerm * a_expr, gboolean a_important)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "property\n");

        if (a_name) {
                guchar *name = cr_string_dup2  (a_name);
                if (name) {
                        fprintf (stdout, name);
                }
                if (a_expr) {
                        fprintf (stdout, ": ");
                        cr_term_dump (a_expr, stdout);
                }
                if (name) {
                        g_free (name);
                        name = NULL;
                }
                fprintf (stdout, "\n");
        }
        fprintf (stdout, "***************\n\n");
}

static void
test_start_font_face (CRDocHandler * a_handler,
                      CRParsingLocation *a_location)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "start_font_face\n");
        fprintf (stdout, "***************\n\n");
}

static void
test_end_font_face (CRDocHandler * a_handler)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "end_font_face\n");
        fprintf (stdout, "***************\n\n");

}

static void
test_start_media (CRDocHandler * a_handler, 
                  GList * a_media_list,
                  CRParsingLocation *a_location)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "start_media\n");

        if (a_media_list) {
                GList *cur = NULL;
                guchar *medium = NULL;

                for (cur = a_media_list; cur; cur = cur->next) {
                        if (cur->data == NULL)
                                continue;
                        medium = cr_string_dup2 ((CRString *) cur->data);
                        if (medium == NULL)
                                continue;
                        fprintf (stdout, "medium: %s\n", medium);
                        if (medium) {
                                g_free (medium);
                                medium = NULL;
                        }
                }
        }
        fprintf (stdout, "***************\n\n");
}

static void
test_end_media (CRDocHandler * a_handler, 
                GList * a_media_list)
{
        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "end_media\n");

        if (a_media_list) {
                GList *cur = NULL;
                guchar *medium = NULL;

                for (cur = a_media_list; cur; cur = cur->next) {
                        if (cur->data == NULL)
                                continue;
                        medium = cr_string_dup2 ((CRString *) cur->data);
                        if (medium == NULL)
                                continue;
                        fprintf (stdout, "medium: %s\n", medium);
                        if (medium) {
                                g_free (medium);
                                medium = NULL;
                        }
                }
        }
        fprintf (stdout, "***************\n\n");
}

static void
test_start_page (CRDocHandler * a_handler,
                 CRString * a_name, 
                 CRString * a_pseudo_page,
                 CRParsingLocation *a_location)
{
        guchar *name = NULL,
                *pseudo_page = NULL;

        g_return_if_fail (a_handler);

        fprintf (stdout, "***************\n");
        fprintf (stdout, "start_page\n");

        if (a_name) {
                name = cr_string_dup2 (a_name);
        }
        if (a_pseudo_page) {
                pseudo_page = cr_string_dup2 (a_pseudo_page);
        }
        if (name) {
                fprintf (stdout, "%s", name);
        }
        if (pseudo_page) {
                fprintf (stdout, ": %s\n", pseudo_page);
        }
        fprintf (stdout, "***************\n\n");
        if (name) {
                g_free (name);
                name = NULL;
        }
        if (pseudo_page) {
                g_free (pseudo_page);
                pseudo_page = NULL;
        }
}

static void
test_end_page (CRDocHandler * a_handler,
               CRString * a_name, 
               CRString * a_pseudo_page)
{
        guchar *name = NULL,
                *pseudo_page = NULL;

        g_return_if_fail (a_handler);
        fprintf (stdout, "***************\n");
        fprintf (stdout, "end_page\n");

        if (a_name) {
                name = cr_string_dup2 (a_name);
        }
        if (a_pseudo_page) {
                pseudo_page = cr_string_dup2 (a_pseudo_page) ;
        }
        if (name) {
                fprintf (stdout, "%s", name);
        }
        if (pseudo_page) {
                fprintf (stdout, ": %s\n", pseudo_page);

        }
        fprintf (stdout, "***************\n\n");
        if (name) {
                g_free (name);
                name = NULL;
        }
        if (pseudo_page) {
                g_free (pseudo_page);
                pseudo_page = NULL;
        }
}

static void
test_ignorable_at_rule (CRDocHandler * a_handler, 
                        CRString * a_name)
{
        guchar *name = NULL;

        g_return_if_fail (a_handler);

        fprintf (stdout, "*********************\n");
        fprintf (stdout, "ignorable_at_rule\n");

        if (a_name) {
                name = cr_string_dup2 (a_name);
        }
        if (name) {
                fprintf (stdout, "%s\n", name);
        }
        fprintf (stdout, "*********************\n\n");
}

static void
init_test_sac_handler (CRDocHandler * a_handler)
{
        a_handler->start_document = test_start_document;
        a_handler->end_document = test_end_document;
        a_handler->import_style = test_import_style;
        a_handler->namespace_declaration = test_namespace_declaration;
        a_handler->comment = test_comment;
        a_handler->start_selector = test_start_selector;
        a_handler->end_selector = test_end_selector;
        a_handler->property = test_property;
        a_handler->start_font_face = test_start_font_face;
        a_handler->end_font_face = test_end_font_face;
        a_handler->start_media = test_start_media;
        a_handler->end_media = test_end_media;
        a_handler->start_page = test_start_page;
        a_handler->end_page = test_end_page;
        a_handler->ignorable_at_rule = test_ignorable_at_rule;
}

/***************************
 *END of TEST SAC document
 *handlers.
 ***************************/

/**
 *The test of the cr_input_read_byte() method.
 *Reads the each byte of a_file_uri using the
 *cr_input_read_byte() method. Each byte is send to
 *stdout.
 *@param a_file_uri the file to read.
 *@return CR_OK upon successfull completion of the
 *function, an error code otherwise.
 */
static enum CRStatus
test_cr_parser_parse (guchar * a_file_uri)
{
        enum CRStatus status = CR_OK;
        CRParser *parser = NULL;

        g_return_val_if_fail (a_file_uri, CR_BAD_PARAM_ERROR);

        gv_test_handler = cr_doc_handler_new ();
        init_test_sac_handler (gv_test_handler);

        parser = cr_parser_new (NULL);

        status = cr_parser_set_sac_handler (parser, gv_test_handler);

        if (status != CR_OK) {
                cr_parser_destroy (parser);
                g_return_val_if_fail (status == CR_OK, CR_ERROR);
        }

        status = cr_parser_set_use_core_grammar (parser, TRUE);
        status = cr_parser_parse_file (parser, a_file_uri, CR_ASCII);

        cr_parser_destroy (parser);

        gv_test_handler = NULL;

        return status;
}

/**
 *The entry point of the testing routine.
 */
int
main (int argc, char **argv)
{
        struct Options options;
        enum CRStatus status = CR_OK;

        cr_test_utils_parse_cmd_line (argc, argv, &options);

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

        status = test_cr_parser_parse (options.files_list[0]);

        if (status != CR_OK) {
                fprintf (stdout, "KO\n");
        }

        return 0;
}
