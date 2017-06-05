/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  bonobo-activation-sysconf: a simple utility to manipulate
 *                             activation configuration files.
 *
 *  Copyright (C) 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Mathieu Lacage <mathieu@eazel.com>
 *
 */

#include "config.h"
#include <string.h>
#include <glib.h>
#include <libxml/tree.h>   
#include <libxml/parser.h>  
#include <libxml/xmlmemory.h>
#include <popt.h>        /* popt :) */

#include <bonobo-activation/bonobo-activation.h>
#include "bonobo-activation/bonobo-activation-i18n.h"
#include "server/object-directory-config-file.h"

static xmlDocPtr
open_file (void)
{
        char *config_file;
        xmlDocPtr doc;

        config_file = g_strconcat (
                SERVER_CONFDIR, SERVER_CONFIG_FILE, NULL);
        
        doc = xmlParseFile (config_file);

        return doc;
}


static void 
save_file (xmlDocPtr doc)
{
        char *config_file;
        

        config_file = g_strconcat (
                SERVER_CONFDIR, SERVER_CONFIG_FILE, NULL);
        if (xmlSaveFile (config_file, doc) == -1) {
                g_print (_("Could not save configuration file.\n"));
                g_print (_("Please, make sure you have permissions to write "
                           "to '%s'.\n"), config_file);
        } else {
                g_print (_("Successfully wrote configuration file.\n"));
        }
        g_free (config_file);

}

static void
display_config_path (void)
{
        char *config_file;

        config_file = g_strconcat (
                SERVER_CONFDIR, SERVER_CONFIG_FILE, NULL);

        g_print (_("configuration file is:\n    %s\n"), config_file);
        
        g_free (config_file);
}

static xmlNodePtr
get_root_first_child (xmlDocPtr doc)
{
	if (doc == NULL)
		return NULL;
	if (doc->xmlRootNode == NULL)
		return NULL;
	return doc->xmlRootNode->xmlChildrenNode;
}

static void
add_directory (const char *directory)
{
        xmlDocPtr doc;
        xmlNodePtr search_node;
        gboolean is_already_there;
        
        is_already_there = FALSE;
        doc = open_file ();

        /* make sure the directory we want to add is not already
           in the config file */
        search_node = get_root_first_child (doc);
        while (search_node != NULL) {
                if (strcmp (search_node->name, "searchpath") == 0) {
                        xmlNodePtr item_node;
                        item_node = search_node->xmlChildrenNode;
                        while (item_node != NULL) {
                                if (strcmp (item_node->name, "item") == 0) {
                                        char *dir_path;
                                        dir_path = xmlNodeGetContent (item_node);
                                        if (strcmp (dir_path, directory) == 0) {
                                                is_already_there = TRUE;
                                                g_print (_("%s already in configuration file\n"), 
                                                         directory);
                                                
                                        }
                                        xmlFree (dir_path);
                                }
                                item_node = item_node->next; 
                        }
                }
                search_node = search_node->next;
        }


        if (!is_already_there) {
                xmlNodePtr new_node;

                /* add the directory to the config file */
                search_node = get_root_first_child (doc);

                if (search_node == NULL)
			g_print (_("there is not a properly structured configuration file\n"));
		else {
			/* go to the first searchpath node */
			while (strcmp (search_node->name, "searchpath") != 0) {
				search_node = search_node->next;                        
			}
			new_node = xmlNewDocNode (doc, NULL, "item", directory);
			xmlAddChild (search_node, new_node);
			
			save_file (doc);
		}
        }
        
        xmlFreeDoc (doc);
}

static void
remove_directory (const char *directory)
{
        xmlDocPtr doc;
        xmlNodePtr search_node;

        doc = open_file ();

        search_node = get_root_first_child (doc);
        while (search_node != NULL) {
                if (strcmp (search_node->name, "searchpath") == 0) {
                        xmlNodePtr item_node;
                        item_node = search_node->xmlChildrenNode;
                        while (item_node != NULL) {
                                if (strcmp (item_node->name, "item") == 0) {
                                        char *dir_path;
                                        dir_path = xmlNodeGetContent (item_node);
                                        if (strcmp (dir_path, directory) == 0) {
                                                if (strcmp (dir_path, directory) == 0) {
                                                        xmlDocPtr doc;
                                                        doc = item_node->doc;
                                                        xmlUnlinkNode (item_node);
                                                        xmlFreeNode (item_node);
                                                        save_file (doc);
                                                        xmlFree (dir_path);
                                                        return;
                                                }
                                        }
                                        xmlFree (dir_path);
                                }
                                item_node = item_node->next; 
                        }
                }
                search_node = search_node->next;
        }

        xmlFreeDoc (doc);                                                
}

static void
display_directories (void)
{
        xmlDocPtr doc;
        xmlNodePtr search_node;

        doc = open_file ();

        g_print (_("Bonobo-activation configuration file contains:\n"));

	search_node = get_root_first_child (doc);
        while (search_node != NULL) {
                if (strcmp (search_node->name, "searchpath") == 0) {
                        xmlNodePtr item_node;
                        item_node = search_node->xmlChildrenNode;
                        while (item_node != NULL) {
                                if (strcmp (item_node->name, "item") == 0) {
                                        char *dir_path;
                                        dir_path = xmlNodeGetContent (item_node);
                                        g_print ("    %s\n", dir_path);
                                        xmlFree (dir_path);
                                }
                                item_node = item_node->next; 
                        }
                }
                search_node = search_node->next;
        }
        xmlFreeDoc (doc);                                                
}



#define REMOVE_DIRECTORY_OPERATION 1
#define ADD_DIRECTORY_OPERATION 2
#define DISPLAY_DIRECTORIES_OPERATION 3
#define DISPLAY_CONFIG_PATH_OPERATION 4

struct poptOption oaf_sysconf_popt_options[] = {
        {"remove-directory", '\0', POPT_ARG_STRING, NULL, 
         REMOVE_DIRECTORY_OPERATION,
         N_("Directory to remove from configuration file"), N_("directory path")},
        {"add-directory", '\0', POPT_ARG_STRING, NULL, 
         ADD_DIRECTORY_OPERATION,
         N_("Directory to add to configuration file"), N_("directory path")},
        {"display-directories", '\0', POPT_ARG_NONE, NULL, 
         DISPLAY_DIRECTORIES_OPERATION,
         N_("Display directories in configuration file"), NULL},
        {"config-file-path", '\0', POPT_ARG_NONE, NULL, 
         DISPLAY_CONFIG_PATH_OPERATION,
         N_("Display path to configuration file"), NULL},
        POPT_AUTOHELP
        {NULL}
};

int main (int argc, char **argv)
{
        poptContext context;
        int popt_option;

        /* init nls */
        bindtextdomain (PACKAGE, SERVER_LOCALEDIR);
        textdomain (PACKAGE);

        /* init popt */
        context = poptGetContext ("oaf-sysconf", argc, (const char **)argv, 
                                  oaf_sysconf_popt_options, 0);

        popt_option = poptGetNextOpt (context);
        if (popt_option <= -1) {
                poptPrintHelp (context, stderr, 0);
                poptFreeContext (context);
                return 0;
        }

        while (popt_option != -1) {
                const char *arg;
                arg = (const char *)poptGetOptArg (context);
                switch (popt_option) {

                case REMOVE_DIRECTORY_OPERATION:
                        remove_directory (arg);
                        break;
                        
                case ADD_DIRECTORY_OPERATION:
                        add_directory (arg);
                        break;
                        
                case DISPLAY_DIRECTORIES_OPERATION:
                        display_directories ();
                        break;

                case DISPLAY_CONFIG_PATH_OPERATION:
                        display_config_path ();
                        break;
	
                }
                popt_option = poptGetNextOpt (context);
        }

    
        poptFreeContext (context);

        return 0;
}





