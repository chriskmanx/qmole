/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 8 -*- */

/*
 * This file is part of The Croco Library
 *
 * Copyright (C) 2002-2003 Dodji Seketeli <dodji@seketeli.org>
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
 */

#include <string.h>
#include "cr-test-utils.h"
#include "libcroco.h"

/**
 *@file
 *Some test facilities for the #CRParser class.
 */

CRDocHandler *gv_test_handler = { 0 };

const guchar *xml_content =
        "<document>"
        "<E0>text0</E0> "
        "<E1><E1-1>text1</E1-1></E1>"
        "<E2 attr2=\"val2\">text2</E2>"
        "<E3 attr3=\"val3_1 val3_2 val3_3\">text3</E3>"
        "<E4 attr4=\"val4_1-val4_2-val4_3\">text4</E4>"
        "<E5 class=\"class5\">text5</E5>"
        "<E6 id=\"id6\">text6</E6>"
        "<E7 lang=\"fr\">text7</E7>" "</document>";

static void
  display_help (char *prg_name);

static void
  display_about (char *prg_name);
static enum CRStatus
  test_sel_eng (guchar * a_file_uri);

static void
  walk_xml_tree_and_lookup_rules (CRSelEng * a_sel_eng,
                                  CRStyleSheet * a_sheet, xmlNode * a_node);

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
        g_print ("Test the selection engine");
        g_print ("Returns OK if the status is CR_OK, KO otherwise\n");
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
        g_print ("%s is a libcroco CROMParser class test program.\n",
                 prg_name);
        g_print ("%s Parses a file and builds a CSS object model", prg_name);
        g_print ("It should run on GNU compliants systems.\n");
        g_print ("\n\n");
        g_print ("Initial author: Dodji Seketeli <dodji@seketeli.org>.\n");
        g_print ("\n\n");
}

static void
walk_xml_tree_and_lookup_rules (CRSelEng * a_sel_eng,
                                CRStyleSheet * a_sheet, xmlNode * a_node)
{
        CRStatement **stmts_tab = NULL;
        gulong tab_len = 0,
                i = 0;
        enum CRStatus status = CR_OK;

        xmlNode *cur_node = a_node;

        for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
                status = cr_sel_eng_get_matched_rulesets
                        (a_sel_eng, a_sheet, cur_node, &stmts_tab, &tab_len);

                if (status == CR_OK && tab_len) {
                        g_print ("'''''''''''''''''''''''''\n");
                        g_print ("xml start element: %s\n", cur_node->name);

                        for (i = 0; i < tab_len; i++) {
                                if (stmts_tab[i]) {
                                        g_print ("\n");
                                        cr_statement_dump (stmts_tab[i],
                                                           stdout, 2);
                                }
                        }
                        g_print ("\n\nxml end element: %s\n", cur_node->name);
                        g_print ("'''''''''''''''''''''''''\n");
                }

                if (stmts_tab) {
                        g_free (stmts_tab);
                        stmts_tab = NULL;
                }
                if (cur_node->children) {
                        walk_xml_tree_and_lookup_rules
                                (a_sel_eng, a_sheet, cur_node->children);
                }
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
static enum CRStatus
test_sel_eng (guchar * a_file_uri)
{
        enum CRStatus status = CR_OK;
        CROMParser *parser = NULL;
        CRStyleSheet *stylesheet = NULL;
        xmlDoc *xml_doc = NULL;
        xmlNode *cur_node = NULL;
        CRSelEng *selection_engine = NULL;

        g_return_val_if_fail (a_file_uri, CR_BAD_PARAM_ERROR);

        parser = cr_om_parser_new (NULL);
        status = cr_om_parser_parse_file (parser, a_file_uri, CR_ASCII,
                                          &stylesheet);
        if (status != CR_OK || !stylesheet) {
                cr_utils_trace_info ("Could not parse xml content");
                goto error;
        }

        xml_doc = xmlParseMemory (xml_content, strlen (xml_content));
        if (!xml_doc) {
                cr_utils_trace_info ("Could not parse xml content");
                goto error;

        }

        selection_engine = cr_sel_eng_new ();

        cur_node = xml_doc->children;

        walk_xml_tree_and_lookup_rules (selection_engine,
                                        stylesheet, cur_node);

        if (parser) {
                cr_om_parser_destroy (parser);
                parser = NULL;
        }

        if (xml_doc) {
                xmlFreeDoc (xml_doc);
                xml_doc = NULL;
        }

        if (stylesheet) {
                cr_stylesheet_destroy (stylesheet);
                stylesheet = NULL;
        }
        if (selection_engine) {
                cr_sel_eng_destroy (selection_engine) ;
                selection_engine = NULL ;
        }
        xmlCleanupParser ();
        return status;

      error:

        if (parser) {
                cr_om_parser_destroy (parser);
                parser = NULL;
        }

        if (xml_doc) {
                xmlFreeDoc (xml_doc);
                xml_doc = NULL;
        }

        if (stylesheet) {
                cr_stylesheet_destroy (stylesheet);
                stylesheet = NULL;
        }

        xmlCleanupParser ();
        return CR_ERROR;
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

        status = test_sel_eng (options.files_list[0]);

        if (status != CR_OK) {
                g_print ("\nKO\n");
        }

        return 0;
}
