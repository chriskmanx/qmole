/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include "gconf.h"
#include "gconf-internals.h"
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/globals.h>
#include <stdlib.h>
#include <errno.h>
#include <locale.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <glib.h>

typedef enum {
  LOAD_SCHEMA_FILE,
  LOAD_ENTRY_FILE
} LoadType;

static int set_mode = FALSE;
static int get_mode = FALSE;
static int unset_mode = FALSE;
static int toggle_mode = FALSE;
static int all_entries_mode = FALSE;
static int all_subdirs_mode = FALSE;
static char* dir_exists = NULL;
static int recursive_list = FALSE;
static int search_key = FALSE;
static int search_key_regex = FALSE;
static int dump_values = FALSE;
static int set_schema_mode = FALSE;
static char* value_type = NULL;
static int get_type_mode = FALSE;
static int get_list_size_mode = FALSE;
static int get_list_element_mode = FALSE;
static char* value_list_type = NULL;
static char* value_car_type = NULL;
static char* value_cdr_type = NULL;
static int shutdown_gconfd = FALSE;
static int ping_gconfd = FALSE;
static int spawn_gconfd = FALSE;
static char* short_desc = NULL;
static char* long_desc = NULL;
static char* owner = NULL;
static char* schema_file = NULL;
static char* entry_file = NULL;
static char* unload_entry_file = NULL;
static const char* config_source = NULL;
static int use_local_source = FALSE;
static int makefile_install_mode = FALSE;
static int makefile_uninstall_mode = FALSE;
static int break_key_mode = FALSE;
static int break_dir_mode = FALSE;
static int short_docs_mode = FALSE;
static int long_docs_mode = FALSE;
static int schema_name_mode = FALSE;
static int associate_schema_mode = FALSE;
static int dissociate_schema_mode = FALSE;
static int ignore_schema_defaults = FALSE;
static int default_source_mode = FALSE;
static int recursive_unset_mode = FALSE;
static int do_version = FALSE;
static const gchar **args = NULL;

static const GOptionEntry client_entries[] = {
  {
    "set",
    's',
    0, 
    G_OPTION_ARG_NONE,
    &set_mode,
    N_("Set a key to a value and sync. Use with --type."),
    NULL
  },
  { 
    "get",
    'g',
    0,
    G_OPTION_ARG_NONE,
    &get_mode,
    N_("Print the value of a key to standard output."),
    NULL
  },
  {

    "unset",
    'u',
    0,
    G_OPTION_ARG_NONE,
    &unset_mode,
    N_("Unset the keys on the command line"),
    NULL
  },
  {
    "recursive-unset",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &recursive_unset_mode,
    N_("Recursively unset all keys at or below the key/directory names on the command line"),
    NULL
  },
  {
    "toggle",
    '\0',
    0, 
    G_OPTION_ARG_NONE,
    &toggle_mode,
    N_("Toggles a boolean key."),
    NULL
  },
  { 
    "all-entries",
    'a',
    0,
    G_OPTION_ARG_NONE,
    &all_entries_mode,
    N_("Print all key/value pairs in a directory."),
    NULL
  },
  {
    "all-dirs",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &all_subdirs_mode,
    N_("Print all subdirectories in a directory."),
    NULL
  },
  {
    "recursive-list",
    'R',
    0,
    G_OPTION_ARG_NONE,
    &recursive_list,
    N_("Print all subdirectories and entries under a directory, recursively."),
    NULL
  },
  {
    "search-key",
    'S',
    0,
    G_OPTION_ARG_NONE,
    &search_key,
    N_("Search for a key, recursively."),
    NULL
  },
  {
    "search-key-regex",
    0,
    0,
    G_OPTION_ARG_NONE,
    &search_key_regex,
    N_("Search for a key, recursively."),
    NULL
  },
  {
    "short-docs",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &short_docs_mode,
    N_("Get the short doc string for a key"),
    NULL
  },
  {
    "long-docs",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &long_docs_mode,
    N_("Get the long doc string for a key"),
    NULL
  },
  { 
    "dir-exists",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &dir_exists,
    N_("Return 0 if the directory exists, 2 if it does not."),
    NULL
  },
  {
    "ignore-schema-defaults",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &ignore_schema_defaults,
    N_("Ignore schema defaults when reading values."),
    NULL
  },
  {
    NULL
  }
};

static const GOptionEntry load_entries[] = {
  {
    "dump",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &dump_values,
    N_("Dump to standard output an XML description of all entries under a directory, recursively."),
    NULL
  },
  {
    "load",
    '\0',
    0,
    G_OPTION_ARG_FILENAME,
    &entry_file,
    N_("Load from the specified file an XML description of values and set them relative to a directory."),
    NULL
  },
  {
    "unload",
    '\0',
    0,
    G_OPTION_ARG_FILENAME,
    &unload_entry_file,
    N_("Unload a set of values described in an XML file."),
    NULL
  },
  {
    NULL
  }
};

static const GOptionEntry server_entries[] = {
  {
    "get-default-source",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &default_source_mode,
    N_("Get the name of the default source"),
    NULL
  },
  { 
    "shutdown",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &shutdown_gconfd,
    N_("Shut down gconfd. DON'T USE THIS OPTION WITHOUT GOOD REASON."),
    NULL
  },
  { 
    "ping",
    'p',
    0,
    G_OPTION_ARG_NONE,
    &ping_gconfd,
    N_("Return 0 if gconfd is running, 2 if not."),
    NULL
  },
  { 
    "spawn",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &spawn_gconfd,
    N_("Launch the configuration server (gconfd). (Normally happens automatically when needed.)"),
    NULL
  },
  {
    NULL
  }
};

static const GOptionEntry type_entries[] = {
  { 
    "type",
    't',
    0,
    G_OPTION_ARG_STRING,
    &value_type,
    N_("Specify the type of the value being set, or the type of the value a schema describes. Unique abbreviations OK."),
    N_("int|bool|float|string|list|pair")
  },
  {
    "get-type",
    'T',
    0,
    G_OPTION_ARG_NONE,
    &get_type_mode,
    N_("Print the data type of a key to standard output."),
    NULL
  },
  {
    "get-list-size",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &get_list_size_mode,
    N_("Get the number of elements in a list key."),
    NULL
  },
  {
    "get-list-element",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &get_list_element_mode,
    N_("Get a specific element from a list key, numerically indexed."),
    NULL
  },
  { 
    "list-type",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &value_list_type,
    N_("Specify the type of the list value being set, or the type of the value a schema describes. Unique abbreviations OK."),
    N_("int|bool|float|string")
  },  
  { 
    "car-type",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &value_car_type,
    N_("Specify the type of the car pair value being set, or the type of the value a schema describes. Unique abbreviations OK."),
    N_("int|bool|float|string")
  },  
  { 
    "cdr-type",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &value_cdr_type,
    N_("Specify the type of the cdr pair value being set, or the type of the value a schema describes. Unique abbreviations OK."),
    N_("int|bool|float|string")
  },  
  {
    NULL
  }
};

static const GOptionEntry install_entries[] = {
  {
    "install-schema-file",
    '\0',
    0,
    G_OPTION_ARG_FILENAME,
    &schema_file,
    N_("Specify a schema file to be installed"),
    N_("FILENAME")
  },
  {
    "config-source",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &config_source,
    N_("Specify a configuration source to use rather than the default path"),
    N_("SOURCE")
  },
  {
    "direct",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &use_local_source,
    N_("Bypass server, and access the configuration database directly. Requires that gconfd is not running."),
    NULL
  },
  {
    "makefile-install-rule",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &makefile_install_mode,
    N_("Properly installs schema files on the command line into the database. Specify a custom configuration source in the GCONF_CONFIG_SOURCE environment variable, or set set the variable to an empty string to use the default configuration source."),
    NULL
  },
  {
    "makefile-uninstall-rule",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &makefile_uninstall_mode,
    N_("Properly uninstalls schema files on the command line from the database. GCONF_CONFIG_SOURCE environment variable should be set to a non-default configuration source or set to the empty string to use the default."),
    NULL
  },
  {
    NULL
  }
};

static const GOptionEntry test_entries[] = {
  {
    "break-key",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &break_key_mode,
    N_("Torture-test an application by setting and unsetting a bunch of values of different types for keys on the command line."),
    NULL
  },
  {
    "break-directory",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &break_dir_mode,
    N_("Torture-test an application by setting and unsetting a bunch of keys inside the directories on the command line."),
    NULL
  },
  {
    NULL
  }
};

static const GOptionEntry schema_entries[] = {
  {
    "set-schema",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &set_schema_mode,
    N_("Set a schema and sync. Use with --short-desc, --long-desc, --owner, and --type."),
    NULL
  },
  { 
    "short-desc",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &short_desc,
    N_("Specify a short half-line description to go in a schema."),
    N_("DESCRIPTION")
  },
  { 
    "long-desc",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &long_desc,
    N_("Specify a several-line description to go in a schema."),
    N_("DESCRIPTION")
  },
  {
    "owner",
    '\0',
    0,
    G_OPTION_ARG_STRING,
    &owner,
    N_("Specify the owner of a schema"),
    N_("OWNER")
  },
  {
    "get-schema-name",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &schema_name_mode,
    N_("Get the name of the schema applied to this key"),
    NULL
  },
  {
    "apply-schema",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &associate_schema_mode,
    N_("Specify the schema name followed by the key to apply the schema name to"),
    NULL
  },
  {
    "unapply-schema",
    '\0',
    0,
    G_OPTION_ARG_NONE,
    &dissociate_schema_mode,
    N_("Remove any schema name applied to the given keys"),
    NULL
  },
  {
    NULL
  }
};

static const GOptionEntry main_entries[] = {
  {
    "version",
    'v',
    0,
    G_OPTION_ARG_NONE,
    &do_version,
    N_("Print version"),
    NULL
  },
  {
    G_OPTION_REMAINING,
    '\0',
    0,
    G_OPTION_ARG_STRING_ARRAY,
    &args,
    N_("[FILE...]|[KEY...]|[DIR...]"),
    NULL
  },
  {
    NULL
  }
};

static int do_break_key(GConfEngine* conf, const gchar** args);
static int do_break_directory(GConfEngine* conf, const gchar** args);
static int do_makefile_install(GConfEngine* conf, const gchar** args, gboolean unload);
static int do_recursive_list(GConfEngine* conf, const gchar** args);
static int do_search_key(GConfEngine* conf, const gchar** args);
static int do_search_key_regex(GConfEngine* conf, const gchar** args);
static int do_dump_values(GConfEngine* conf, const gchar** args);
static int do_all_pairs(GConfEngine* conf, const gchar** args);
static void list_pairs_in_dir(GConfEngine* conf, const gchar* dir, guint depth);
static int get_schema_from_xml(xmlNodePtr node, gchar **schema_key, GHashTable** schemas_hash, GSList **applyto_list);
static int get_first_value_from_xml(xmlNodePtr node, GConfValue** ret_value);
static void print_value_in_xml(GConfValue* value, int indent);
static void dump_entries_in_dir(GConfEngine* conf, const gchar* dir, const gchar *base_dir);
static gboolean do_dir_exists(GConfEngine* conf, const gchar* dir);
static void do_spawn_daemon(GConfEngine* conf);
static int do_get(GConfEngine* conf, const gchar** args);
static int do_set(GConfEngine* conf, const gchar** args);
static int do_toggle(GConfEngine* conf, const gchar** args);
static int do_get_type(GConfEngine* conf, const gchar** args);
static int do_get_list_size(GConfEngine* conf, const gchar** args);
static int do_get_list_element(GConfEngine* conf, const gchar** args);
static int do_set_schema(GConfEngine* conf, const gchar** args);
static int do_all_entries(GConfEngine* conf, const gchar** args);
static int do_unset(GConfEngine* conf, const gchar** args);
static int do_recursive_unset (GConfEngine* conf, const gchar** args);
static int do_all_subdirs(GConfEngine* conf, const gchar** args);
static int do_load_file(GConfEngine* conf, LoadType load_type, gboolean unload, const gchar* file, const gchar** base_dirs);
static int do_sync(GConfEngine* conf);
static int do_short_docs (GConfEngine *conf, const gchar **args);
static int do_long_docs (GConfEngine *conf, const gchar **args);
static int do_get_schema_name (GConfEngine *conf, const gchar **args);
static int do_associate_schema (GConfEngine *conf, const gchar **args);
static int do_dissociate_schema (GConfEngine *conf, const gchar **args);
static int do_get_default_source (const gchar **args);

int 
main (int argc, char** argv)
{
  GConfEngine* conf;
  GOptionContext *context;
  GOptionGroup *group;
  GError* err = NULL;

  LIBXML_TEST_VERSION;
  xmlKeepBlanksDefault(1);

  setlocale (LC_ALL, "");
  bindtextdomain (GETTEXT_PACKAGE,GCONF_LOCALE_DIR);
  textdomain (GETTEXT_PACKAGE);

  g_thread_init (NULL);

  _gconf_init_i18n ();
  
  context = g_option_context_new (N_("- Tool to manipulate a GConf configuration"));
#if GLIB_CHECK_VERSION (2, 12, 0)
  g_option_context_set_translation_domain (context, GETTEXT_PACKAGE);
#endif

  group = g_option_group_new ("client", N_("Client options:"), N_("Show client options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, client_entries);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("key-type", N_("Key type options:"), N_("Show key type options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, type_entries);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("load", N_("Load/Save options:"), N_("Show load/save options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, load_entries);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("server", N_("Server options:"), N_("Show server options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, server_entries);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("install", N_("Installation options:"), N_("Show installation options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, install_entries);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("test", N_("Test options:"), N_("Show test options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, test_entries);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("schema", N_("Schema options:"), N_("Show schema options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, schema_entries);
  g_option_context_add_group (context, group);

  g_option_context_add_main_entries (context, main_entries, GETTEXT_PACKAGE);

  if (argc < 2) 
    {
      g_printerr (_("Run '%s --help' to see a full list of available command line options.\n"),
                  argv[0]);
      return 1;
    } 

  g_option_context_parse (context, &argc, &argv, &err);
  g_option_context_free (context);

  if (err)
    {
      g_printerr (_("Error while parsing options: %s.\nRun '%s --help' to see a full list of available command line options.\n"),
                  err->message,
                  argv[0]);
      g_error_free (err);
      return 1;
    }

  /* Um, this is a mess. Not using compatible options? */

  if (do_version)
    {
      g_print ("%s\n", VERSION);
      return 0;
    }
  
  if ((get_mode && set_mode) ||
      (get_mode && unset_mode))
    {
      g_printerr (_("Can't get and set/unset simultaneously\n"));
      return 1;
    }

  if ((set_mode && get_mode) ||
      (set_mode && unset_mode) ||
      (set_mode && get_type_mode) ||
      (set_mode && get_list_size_mode) ||
      (set_mode && get_list_element_mode)) 
    {
      g_printerr (_("Can't set and get/unset simultaneously\n"));
      return 1;
    }

  if ((get_type_mode && set_mode) ||
      (get_type_mode && unset_mode))
    {
      g_printerr (_("Can't get type and set/unset simultaneously\n"));
      return 1;
    }

  if ((toggle_mode && set_mode) ||
      (toggle_mode && get_mode) ||
      (toggle_mode && unset_mode)) 
    {
      g_printerr (_("Can't toggle and get/set/unset simultaneously\n"));
      return 1;
    }

  if ((all_entries_mode && get_mode) ||
      (all_entries_mode && set_mode) ||
      (all_entries_mode && get_type_mode) ||
      (all_entries_mode && unset_mode) ||
      (all_entries_mode && get_list_size_mode) ||
      (all_entries_mode && get_list_element_mode))
    {
      g_printerr (_("Can't use --all-entries with --get or --set\n"));
      return 1;
    }

  if ((all_subdirs_mode && get_mode) ||
      (all_subdirs_mode && set_mode) ||
      (all_subdirs_mode && get_type_mode) ||
      (all_subdirs_mode && unset_mode) ||
      (all_subdirs_mode && get_list_size_mode) ||
      (all_subdirs_mode && get_list_element_mode))
    {
      g_printerr (_("Can't use --all-dirs with --get or --set\n"));
      return 1;
    }

  if ((recursive_list && get_mode) ||
      (recursive_list && set_mode) ||
      (recursive_list && unset_mode) ||
      (recursive_list && get_type_mode) ||
      (recursive_list && get_list_size_mode) ||
      (recursive_list && get_list_element_mode) ||
      (recursive_list && all_entries_mode) ||
      (recursive_list && all_subdirs_mode) ||
      (recursive_list && (search_key || search_key_regex)))
    {
      g_printerr (_("--recursive-list should not be used with --get, --set, --unset, --all-entries, --all-dirs, or --search-key\n"));
      return 1;
    }

  if ((set_schema_mode && get_mode) ||
      (set_schema_mode && set_mode) ||
      (set_schema_mode && unset_mode) ||
      (set_schema_mode && get_type_mode) ||
      (set_schema_mode && get_list_size_mode) ||
      (set_schema_mode && get_list_element_mode) ||
      (set_schema_mode && all_entries_mode) ||
      (set_schema_mode && all_subdirs_mode) ||
      (set_schema_mode && (search_key || search_key_regex)))
    {
      g_printerr (_("--set_schema should not be used with --get, --set, --unset, --all-entries, --all-dirs, or --search-key\n"));
      return 1;
    }

  if ((value_type != NULL) && !(set_mode || set_schema_mode))
    {
      g_printerr (_("Value type is only relevant when setting a value\n"));
      return 1;
    }

  if (set_mode && (value_type == NULL))
    {
      g_printerr (_("Must specify a type when setting a value\n"));
      return 1;
    }

  if (ignore_schema_defaults && !(get_mode || all_entries_mode ||
				  dump_values || recursive_list ||
				  get_list_size_mode || get_list_element_mode))
    {
      g_printerr (_("--ignore-schema-defaults is only relevant with --get, --all-entries, --dump, --recursive-list, --get-list-size or --get-list-element\n"));
      return 1;
    }

  if (ping_gconfd && (shutdown_gconfd || set_mode || get_mode || unset_mode ||
                      all_subdirs_mode || all_entries_mode || recursive_list || 
                      get_type_mode || get_list_size_mode || get_list_element_mode ||
                      spawn_gconfd || dir_exists || schema_file ||
                      makefile_install_mode || makefile_uninstall_mode ||
                      break_key_mode || break_dir_mode || short_docs_mode ||
                         long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "-p/--ping");
      return 1;
    }

  if (dir_exists && (shutdown_gconfd || set_mode || get_mode || unset_mode ||
                     all_subdirs_mode || all_entries_mode || recursive_list || search_key || search_key_regex ||
                     get_type_mode || get_list_size_mode || get_list_element_mode ||
                     spawn_gconfd || schema_file ||
                     makefile_install_mode || makefile_uninstall_mode ||
                     break_key_mode || break_dir_mode || short_docs_mode ||
                         long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "--dir-exists");
      return 1;
    }

  if (schema_file && (shutdown_gconfd || set_mode || get_mode || unset_mode ||
                      all_subdirs_mode || all_entries_mode || recursive_list || search_key || search_key_regex ||
                      get_type_mode || get_list_size_mode || get_list_element_mode ||
                      spawn_gconfd || dir_exists ||
                      makefile_install_mode || makefile_uninstall_mode ||
                      break_key_mode || break_dir_mode || short_docs_mode ||
                         long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "--install-schema-file");
      return 1;
    }


  if (makefile_install_mode && (shutdown_gconfd || set_mode || get_mode || unset_mode ||
                                all_subdirs_mode || all_entries_mode || recursive_list || search_key || search_key_regex ||
                                get_type_mode || get_list_size_mode || get_list_element_mode ||
                                makefile_uninstall_mode ||
                                spawn_gconfd || dir_exists || schema_file ||
                                break_key_mode || break_dir_mode || short_docs_mode ||
                         long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "--makefile-install-rule");
      return 1;
    }
  
  if (makefile_uninstall_mode && (shutdown_gconfd || set_mode || get_mode ||
                                  unset_mode || all_subdirs_mode ||
                                  all_entries_mode || recursive_list || search_key || search_key_regex ||
                                  makefile_install_mode ||
                                  spawn_gconfd || dir_exists || schema_file ||
                                  break_key_mode || break_dir_mode || short_docs_mode ||
                                  long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "--makefile-uninstall-rule");
      return 1;
    }
  
  if (break_key_mode && (shutdown_gconfd || set_mode || get_mode || unset_mode ||
                                all_subdirs_mode || all_entries_mode || recursive_list || search_key || search_key_regex ||
                                get_type_mode || get_list_size_mode || get_list_element_mode ||
                                spawn_gconfd || dir_exists || schema_file ||
                                makefile_install_mode || makefile_uninstall_mode ||
                                break_dir_mode || short_docs_mode ||
                         long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "--break-key");
      return 1;
    }

  
  if (break_dir_mode && (shutdown_gconfd || set_mode || get_mode || unset_mode ||
                                all_subdirs_mode || all_entries_mode || recursive_list || search_key || search_key_regex ||
                                get_type_mode || get_list_size_mode || get_list_element_mode ||
                                spawn_gconfd || dir_exists || schema_file ||
                                break_key_mode || makefile_install_mode ||
                                makefile_uninstall_mode || short_docs_mode ||
                         long_docs_mode || schema_name_mode))
    {
      g_printerr (_("%s option must be used by itself.\n"),
		      "--break-directory");
      return 1;
    }

  /* FIXME not checking that --recursive-unset, --dump or --load are used alone */
  
  if (use_local_source && config_source == NULL)
    {
      g_printerr (_("You must specify a configuration source with --config-source when using --direct\n"));
      return 1;
    }

  if (!gconf_init(argc, argv, &err))
    {
      g_printerr (_("Failed to init GConf: %s\n"), err->message);
      g_error_free(err);
      err = NULL;
      return 1;
    }

  /* Do this first, since we want to do only this if the user selected
     it. */
  if (ping_gconfd)
    {
      if (gconf_ping_daemon())
        return 0;
      else 
        return 2;
    }

  /* Before creating engine */
  if (default_source_mode)
    {
      if (do_get_default_source (args)  == 1)
        return 1;
      else
        return 0;
    }

  if (makefile_install_mode &&
      g_getenv ("GCONF_DISABLE_MAKEFILE_SCHEMA_INSTALL"))
    {
      g_print (_("GCONF_DISABLE_MAKEFILE_SCHEMA_INSTALL is set, not installing schemas\n"));
      makefile_install_mode = FALSE;
    }

  if (makefile_uninstall_mode &&
      g_getenv ("GCONF_DISABLE_MAKEFILE_SCHEMA_UNINSTALL"))
    {
      g_print (_("GCONF_DISABLE_MAKEFILE_SCHEMA_UNINSTALL is set, not uninstalling schemas\n"));
      makefile_uninstall_mode = FALSE;
    }

  if (makefile_install_mode || makefile_uninstall_mode)
    {
      g_assert (config_source == NULL);

      /* Try the environment variable */
      config_source = g_getenv ("GCONF_CONFIG_SOURCE");

      if (config_source == NULL)
        {
          g_printerr (_("Must set the GCONF_CONFIG_SOURCE environment variable\n"));
          return 1;
        }

      if (*config_source == '\0')
        {
          /* Properly set, but set to nothing (use default source) */
          config_source = NULL;
        }

      use_local_source = TRUE;

      /* shut down daemon, this is a race condition, but will usually work. */
      gconf_shutdown_daemon (NULL);
    }

  if (config_source == NULL)
    {
        /* If we aren't running from within a session,
         * assume we'll be touching the database locally
         */
      conf = NULL;
      if (g_getenv ("DBUS_SESSION_BUS_ADDRESS") == NULL)
        {
          char *conffile;
          GSList *addresses;

          conffile = g_strconcat (GCONF_CONFDIR, "/path", NULL);
          addresses = gconf_load_source_path (conffile, NULL);
          g_free(conffile);

          conf = gconf_engine_get_local_for_addresses (addresses, &err);
          gconf_address_list_free (addresses);
        }

      if (conf == NULL)
        conf = gconf_engine_get_default();
    }
  else
    {
      GSList *addresses;

      addresses = gconf_persistent_name_get_address_list (config_source);

      if (use_local_source)
        conf = gconf_engine_get_local_for_addresses (addresses, &err);
      else
        conf = gconf_engine_get_for_addresses (addresses, &err);

      gconf_address_list_free (addresses);
    }
  
  if (conf == NULL)
    {
      g_assert(err != NULL);
      g_printerr (_("Failed to access configuration source(s): %s\n"), err->message);
      g_error_free(err);
      err = NULL;
      return 1;
    }
  else
    {
      g_assert(err == NULL);
    }
  
  g_assert(conf != NULL);
  
  if (dir_exists != NULL) 
    {
      gboolean success;

      success = do_dir_exists(conf, dir_exists);

      gconf_engine_unref(conf);
      
      if (success)
        return 0; /* TRUE */
      else
        return 2; /* FALSE */
    }

  if (schema_file != NULL)
    {
      gint retval;

      retval = do_load_file(conf, LOAD_SCHEMA_FILE, FALSE, schema_file, NULL);
      if (!retval)
	retval = do_sync(conf);

      gconf_engine_unref(conf);

      return retval;
    }

  if (entry_file != NULL)
    {
      gint retval;

      retval = do_load_file(conf, LOAD_ENTRY_FILE, FALSE, entry_file, args);
      if (!retval)
	retval = do_sync(conf);

      gconf_engine_unref(conf);

      return retval;
    }
  
  if (unload_entry_file != NULL)
    {
      gint retval;

      retval = do_load_file(conf, LOAD_ENTRY_FILE, TRUE, unload_entry_file, args);
      if (!retval)
	retval = do_sync(conf);

      gconf_engine_unref(conf);

      return retval;
    }

  if (spawn_gconfd)
    {
      do_spawn_daemon(conf);
      /* don't exit, it's OK to have this along with other options
         (however, it's probably pointless) */
    }

  if (makefile_install_mode)
    {
      gint retval;

      umask (022);
      retval = do_makefile_install (conf, args, FALSE);
      
      gconf_engine_unref (conf);

      return retval;
    }

  if (makefile_uninstall_mode)
    {
      gint retval;

      umask (022);
      retval = do_makefile_install (conf, args, TRUE);
      
      gconf_engine_unref (conf);

      return retval;
    }

  if (break_key_mode)
    {
      gint retval = do_break_key(conf, args);

      gconf_engine_unref(conf);

      return retval;
    }
  
  if (break_dir_mode)
    {
      gint retval = do_break_directory(conf, args);

      gconf_engine_unref(conf);

      return retval;
    }
  
  if (get_mode)
    {
      if (do_get(conf, args)  == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  
  if (set_mode)
    {
      if (do_set(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (toggle_mode)
    {
      if (do_toggle(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (get_type_mode)
    {
      if (do_get_type (conf, args) == 1)
        {
          gconf_engine_unref (conf);
          return 1;
        }
    }

  if (get_list_size_mode)
    {
      if (do_get_list_size (conf, args) == 1)
        {
          gconf_engine_unref (conf);
          return 1;
        }
    }

  if (get_list_element_mode)
    {
      if (do_get_list_element (conf, args) == 1)
        {
          gconf_engine_unref (conf);
          return 1;
        }
    }

  if (set_schema_mode)
    {
      if (do_set_schema(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (short_docs_mode)
    {
      if (do_short_docs(conf, args)  == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (long_docs_mode)
    {
      if (do_long_docs(conf, args)  == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (schema_name_mode)
    {
      if (do_get_schema_name(conf, args)  == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (associate_schema_mode)
    {
      if (do_associate_schema(conf, args)  == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (dissociate_schema_mode)
    {
      if (do_dissociate_schema (conf, args)  == 1)
        {
          gconf_engine_unref (conf);
          return 1;
        }
    }
  
  if (all_entries_mode)
    {
      if (do_all_entries(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (unset_mode)
    {
      if (do_unset(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (recursive_unset_mode)
    {
      if (do_recursive_unset(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }
  
  if (all_subdirs_mode)
    {
      if (do_all_subdirs(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (recursive_list)
    {
      if (do_recursive_list(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

    if (search_key)
    {
      if (do_search_key(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

    if (search_key_regex)
    {
      if (do_search_key_regex(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  if (dump_values)
    {
      if (do_dump_values(conf, args) == 1)
        {
          gconf_engine_unref(conf);
          return 1;
        }
    }

  gconf_engine_unref(conf);

  if (shutdown_gconfd)
    {
      err = NULL;
      gconf_shutdown_daemon(&err);
    }
      
  if (err != NULL)
    {
      g_printerr (_("Shutdown error: %s\n"),
		  err->message);
      g_error_free(err);
      err = NULL;
    }

  return 0;
}

static void 
recurse_subdir_list(GConfEngine* conf, GSList* subdirs, const gchar* parent, guint depth)
{
  GSList* tmp;
  gchar* whitespace;

  whitespace = g_strnfill(depth, ' ');

  tmp = subdirs;
  
  while (tmp != NULL)
    {
      gchar* s = tmp->data;
      
      g_print ("%s%s:\n", whitespace, s);
      
      list_pairs_in_dir(conf, s, depth);

      recurse_subdir_list(conf, gconf_engine_all_dirs(conf, s, NULL), s, depth+1);

      g_free(s);
      
      tmp = g_slist_next(tmp);
    }
  
  g_slist_free(subdirs);
  g_free(whitespace);
}

static int
do_recursive_list(GConfEngine* conf, const gchar** args)
{
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more directories to recursively list.\n"));
      return 1;
    }

  while (*args)
    {
      GSList* subdirs;

      subdirs = gconf_engine_all_dirs(conf, *args, NULL);

      list_pairs_in_dir(conf, *args, 0);
          
      recurse_subdir_list(conf, subdirs, *args, 1);
 
      ++args;
    }

  return 0;
}

static gboolean
match_pattern (gpointer pattern, const char *key)
{
  return g_pattern_match_string((GPatternSpec*) pattern, key);
}
    
static gboolean
match_regex (gpointer regex, const char *key)
{
  return g_regex_match((GRegex*) regex, key, 0, NULL);
}
    
typedef gboolean (* MatchFunc) (gpointer match_data, const char *key);

static void 
search_key_in_dir(GConfEngine* conf, const gchar* dir, MatchFunc match_func, gpointer match_data)
{
  GSList* pairs;
  GSList* tmp;
  GError* err = NULL;
  
  pairs = gconf_engine_all_entries(conf, dir, &err);
          
  if (err != NULL)
    {
      g_printerr (_("Failure listing entries in `%s': %s\n"),
              dir, err->message);
      g_error_free(err);
      err = NULL;
    }

  if (pairs != NULL)
    {
      tmp = pairs;

      while (tmp != NULL)
        {
          GConfEntry* pair = tmp->data;
          gchar* s;
	  const gchar *k;

          if (gconf_entry_get_value (pair) && 
	      (!ignore_schema_defaults || !gconf_entry_get_is_default (pair)))
            s = gconf_value_to_string (gconf_entry_get_value (pair));
          else
            s = g_strdup(_("(no value set)"));

	  k = gconf_key_key (gconf_entry_get_key (pair));

	  if (match_func(match_data, k))
	    g_print (" %s/%s = %s\n", dir, k, s);

          g_free(s);
                  
          gconf_entry_free(pair);

          tmp = g_slist_next(tmp);
        }

      g_slist_free(pairs);
    }
}

static void 
recurse_subdir_search(GConfEngine* conf, GSList* subdirs, const gchar* parent,
		      MatchFunc match_func, gpointer match_data)
{
  GSList* tmp;

  tmp = subdirs;
  
  while (tmp != NULL)
    {
      gchar* s = tmp->data;

      search_key_in_dir(conf, s, match_func, match_data);

      recurse_subdir_search(conf, gconf_engine_all_dirs(conf, s, NULL), s, match_func, match_data);

      g_free(s);
      
      tmp = g_slist_next(tmp);
    }
  
  g_slist_free(subdirs);
}

static int
do_search(GConfEngine* conf, MatchFunc match_func, gpointer match_data)
{
  GSList* subdirs;

  subdirs = gconf_engine_all_dirs(conf, "/", NULL);

  search_key_in_dir(conf, "/", match_func, match_data);
          
  recurse_subdir_search(conf, subdirs, "/", match_func, match_data);

  return 0;
}

static int
do_search_key(GConfEngine* conf, const gchar** args)
{
  GPatternSpec* pattern;
  
  if (args == NULL)
    {
      g_printerr (_("Must specify a key pattern to search for.\n"));
      return 1;
    }

  pattern = g_pattern_spec_new (*args);
  do_search(conf, match_pattern, pattern);
  g_pattern_spec_free (pattern);

  return 0;
}

static int
do_search_key_regex(GConfEngine* conf, const gchar** args)
{
  GRegex* regex;
  GError *error = NULL;
  
  if (args == NULL)
    {
      g_printerr(_("Must specify a PCRE regex to search for.\n"));
      return 1;
    }

  regex = g_regex_new(args[0], G_REGEX_OPTIMIZE, 0, &error);
  if (!regex)
    {
      g_printerr(_("Error compiling regex: %s\n"), error->message);
      g_error_free(error);
      return 1;
    }

  do_search(conf, match_regex, regex);
  g_regex_unref(regex);

  return 0;
}

static void 
recurse_subdir_dump(GConfEngine* conf, GSList* dirs, const gchar* base_dir)
{
  GSList* tmp;

  tmp = dirs;
  
  while (tmp != NULL)
    {
      gchar* s = tmp->data;
      GSList* subdirs;
      
      dump_entries_in_dir(conf, s, base_dir);

      subdirs = g_slist_sort(gconf_engine_all_dirs(conf, s, NULL),
			     (GCompareFunc)strcmp);

      recurse_subdir_dump(conf, subdirs, base_dir);

      g_free(s);
      
      tmp = tmp->next;
    }
  
  g_slist_free(dirs);
}

static int
do_dump_values(GConfEngine* conf, const gchar** args)
{
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more directories to dump.\n"));
      return 1;
    }

  g_print ("<gconfentryfile>\n");

  while (*args)
    {
      GSList* subdirs;

      g_print ("  <entrylist base=\"%s\">\n", *args);

      subdirs = g_slist_sort(gconf_engine_all_dirs(conf, *args, NULL),
			     (GCompareFunc)strcmp);

      dump_entries_in_dir(conf, *args, *args);

      recurse_subdir_dump(conf, subdirs, *args);

      g_print ("  </entrylist>\n");
 
      ++args;
    }

  g_print ("</gconfentryfile>\n");
  return 0;
}

static void 
list_pairs_in_dir(GConfEngine* conf, const gchar* dir, guint depth)
{
  GSList* pairs;
  GSList* tmp;
  gchar* whitespace;
  GError* err = NULL;
  
  whitespace = g_strnfill(depth, ' ');

  pairs = gconf_engine_all_entries(conf, dir, &err);
          
  if (err != NULL)
    {
      g_printerr (_("Failure listing entries in `%s': %s\n"),
              dir, err->message);
      g_error_free(err);
      err = NULL;
    }

  if (pairs != NULL)
    {
      tmp = pairs;

      while (tmp != NULL)
        {
          GConfEntry* pair = tmp->data;
          gchar* s;

          if (gconf_entry_get_value (pair) && 
	      (!ignore_schema_defaults || !gconf_entry_get_is_default (pair)))
            s = gconf_value_to_string (gconf_entry_get_value (pair));
          else
            s = g_strdup(_("(no value set)"));
          
          g_print (" %s%s = %s\n", whitespace,
		   gconf_key_key (gconf_entry_get_key (pair)),
		   s);

          g_free(s);
                  
          gconf_entry_free(pair);

          tmp = g_slist_next(tmp);
        }

      g_slist_free(pairs);
    }

  g_free(whitespace);
}

static int
do_all_pairs(GConfEngine* conf, const gchar** args)
{      
  while (*args)
    {
      list_pairs_in_dir(conf, *args, 0);
      ++args;
    }
  return 0;
}

/* FIXME: only prints the schema for the current locale.
 *        Need some way of getting the schemas for all locales.
 */
static void
print_schema_in_xml(GConfValue* value, int indent)
{
  GConfSchema* schema;
  GConfValueType type;
  GConfValueType list_type;
  GConfValueType car_type;
  GConfValueType cdr_type;
  GConfValue* default_value;
  const gchar* owner;
  const gchar* short_desc;
  const gchar* long_desc;
  gchar* whitespace;

  whitespace = g_strnfill(indent, ' ');

  schema = gconf_value_get_schema(value);

  type = gconf_schema_get_type(schema);
  list_type = gconf_schema_get_list_type(schema);
  car_type = gconf_schema_get_car_type(schema);
  cdr_type = gconf_schema_get_cdr_type(schema);

  owner = gconf_schema_get_owner(schema);
  default_value = gconf_schema_get_default_value(schema);
  short_desc = gconf_schema_get_short_desc(schema);
  long_desc = gconf_schema_get_long_desc(schema);

  g_print ("%s<schema>\n", whitespace);

  if (owner)
    g_print ("%s  <owner>%s</owner>\n", whitespace, owner);

  g_print ("%s  <type>%s</type>\n", whitespace, gconf_value_type_to_string(type));

  if (type == GCONF_VALUE_LIST)
    g_print ("%s  <list_type>%s</list_type>\n", whitespace, gconf_value_type_to_string(list_type));
  else if (type == GCONF_VALUE_PAIR)
    {
      g_print ("%s  <car_type>%s</car_type>\n", whitespace, gconf_value_type_to_string(car_type));
      g_print ("%s  <cdr_type>%s</cdr_type>\n", whitespace, gconf_value_type_to_string(cdr_type));
    }

  g_print ("%s  <locale name=\"%s\">\n", whitespace, gconf_schema_get_locale (schema));

  if (default_value)
    {
      g_print ("%s    <default_value>\n", whitespace);
      print_value_in_xml(default_value, indent + 6);
      g_print ("%s    </default_value>\n", whitespace);
    }

  if (short_desc)
    {
      gchar* tmp = g_markup_escape_text(short_desc, -1);
      g_print ("%s    <short>%s</short>\n", whitespace, tmp);
      g_free(tmp);
    }

  if (long_desc)
    {
      gchar* tmp = g_markup_escape_text(long_desc, -1);
      g_print ("%s    <long>%s</long>\n", whitespace, tmp);
      g_free(tmp);
    }

  g_print ("%s  </locale>\n", whitespace);

  g_print ("%s</schema>\n", whitespace);

  g_free(whitespace);
}

static void
print_pair_in_xml(GConfValue* value, int indent)
{
  gchar* whitespace;

  whitespace = g_strnfill(indent, ' ');

  g_print ("%s<pair>\n", whitespace);

  g_print ("%s  <car>\n", whitespace);
  print_value_in_xml(gconf_value_get_car(value), indent + 4);
  g_print ("%s  </car>\n", whitespace);

  g_print ("%s  <cdr>\n", whitespace);
  print_value_in_xml(gconf_value_get_cdr(value), indent + 4);
  g_print ("%s  </cdr>\n", whitespace);

  g_print ("%s</pair>\n", whitespace);

  g_free(whitespace);
}

static void
print_list_in_xml(GConfValue* value, int indent)
{
  GConfValueType list_type;
  GSList* tmp;
  gchar* whitespace;

  whitespace = g_strnfill(indent, ' ');

  list_type = gconf_value_get_list_type(value);

  g_print ("%s<list type=\"%s\">\n", whitespace, gconf_value_type_to_string(list_type));

  tmp = gconf_value_get_list(value);
  while (tmp)
    {
      print_value_in_xml(tmp->data, indent + 4);

      tmp = tmp->next;
    }

  g_print ("%s</list>\n", whitespace);

  g_free(whitespace);
}

static void
print_value_in_xml(GConfValue* value, int indent)
{
  gchar* whitespace;
  gchar* tmp;

  whitespace = g_strnfill(indent, ' ');

  g_print ("%s<value>\n", whitespace);

  switch (value->type)
    {
    case GCONF_VALUE_INT:
      tmp = gconf_value_to_string(value);
      g_print ("%s  <int>%s</int>\n", whitespace, tmp);
      g_free(tmp);
      break;
    case GCONF_VALUE_FLOAT:
      tmp = gconf_value_to_string(value);
      g_print ("%s  <float>%s</float>\n", whitespace, tmp);
      g_free(tmp);
      break;
    case GCONF_VALUE_STRING:
      tmp = g_markup_escape_text(gconf_value_get_string(value), -1);
      g_print ("%s  <string>%s</string>\n", whitespace, (tmp[0] == ' ' && tmp[1] == '\0') ? "" : tmp);
      g_free(tmp);
      break;
    case GCONF_VALUE_BOOL:
      tmp = gconf_value_to_string(value);
      g_print ("%s  <bool>%s</bool>\n", whitespace, tmp);
      g_free(tmp);
      break;
    case GCONF_VALUE_LIST:
      print_list_in_xml(value, indent + 2);
      break;
    case GCONF_VALUE_PAIR:
      print_pair_in_xml(value, indent + 2);
      break;
    case GCONF_VALUE_SCHEMA:
      print_schema_in_xml(value, indent + 2);
      break;
    default:
      g_assert_not_reached();
      break;
    }

  g_print ("%s</value>\n", whitespace);

  g_free(whitespace);
}

static inline const gchar*
get_key_relative(const gchar* key, const gchar* dir)
{
  int i;

  g_assert(key != NULL);
  g_assert(dir != NULL);

  i = 0;
  while (dir [i])
    {
      if (dir [i] != key [i])
        return key;
      ++i;
    }

  if (key [i] != '/' || key [i] == '\0')
    return key;

  ++i;

  return key + i;
}

static int
compare_entries (GConfEntry* a, GConfEntry *b)
{
  return strcmp(gconf_entry_get_key(a), gconf_entry_get_key(b));
}

static void 
dump_entries_in_dir(GConfEngine* conf, const gchar* dir, const gchar* base_dir)
{
  GSList* entries;
  GSList* tmp;
  GError* err = NULL;
  
  entries = g_slist_sort(gconf_engine_all_entries(conf, dir, &err),
			 (GCompareFunc)compare_entries);
          
  if (err != NULL)
    {
      g_printerr (_("Failure listing entries in `%s': %s\n"),
		  dir, err->message);
      g_error_free(err);
      err = NULL;
    }

  tmp = entries;
  while (tmp != NULL)
    {
      GConfEntry* entry = tmp->data;

      g_print ("    <entry>\n");

      g_print ("      <key>%s</key>\n",
	       get_key_relative(gconf_entry_get_key(entry), base_dir));

      /* <schema_key> will only be relative if its under the base dir */
      if (gconf_entry_get_schema_name(entry))
        g_print ("      <schema_key>%s</schema_key>\n",
		 get_key_relative(gconf_entry_get_schema_name(entry), base_dir));

      if (entry->value && 
	  (!ignore_schema_defaults || !gconf_entry_get_is_default(entry)))
        print_value_in_xml(entry->value, 6);

      g_print ("    </entry>\n");

      gconf_entry_free(entry);

      tmp = tmp->next;
    }
  g_slist_free(entries);
}

static gboolean
do_dir_exists(GConfEngine* conf, const gchar* dir)
{
  GError* err = NULL;
  gboolean exists = FALSE;
  
  exists = gconf_engine_dir_exists(conf, dir_exists, &err);
  
  if (err != NULL)
    {
      g_printerr ("%s\n", err->message);
      g_error_free(err);
      err = NULL;
    }
  
  return exists;
}

static void
do_spawn_daemon(GConfEngine* conf)
{
  GError* err = NULL;

  if (!gconf_spawn_daemon(&err))
    {
      g_printerr (_("Failed to spawn the configuration server (gconfd): %s\n"), 
		  err->message);
      g_error_free(err);
      err = NULL;
    }
}

static inline GConfValue *
get_maybe_without_default (GConfEngine  *conf,
			   const gchar  *key,
			   GError      **err)
{
  if (!ignore_schema_defaults)
    {
      return gconf_engine_get (conf, key, err);
    }
  else
    {
      return gconf_engine_get_without_default (conf, key, err);
    }
}

static int
do_get(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;

  if (args == NULL)
    {
      g_printerr (_("Must specify a key or keys to get\n"));
      return 1;
    }
      
  while (*args)
    {
      GConfValue* value;
      gchar* s;

      err = NULL;

      value = get_maybe_without_default (conf, *args, &err);
         
      if (value != NULL)
        {
          if (value->type != GCONF_VALUE_SCHEMA)
            {
              s = gconf_value_to_string(value);

              g_print ("%s\n", s);

              g_free(s);
            }
          else
            {
              GConfSchema* sc = gconf_value_get_schema(value);
              GConfValueType stype = gconf_schema_get_type(sc);
              GConfValueType slist_type = gconf_schema_get_list_type(sc);
              GConfValueType scar_type = gconf_schema_get_car_type(sc);
              GConfValueType scdr_type = gconf_schema_get_cdr_type(sc);
              const gchar* long_desc = gconf_schema_get_long_desc(sc);
              const gchar* short_desc = gconf_schema_get_short_desc(sc);
              const gchar* owner = gconf_schema_get_owner(sc);
              GConfValue* def_value = gconf_schema_get_default_value(sc);
              
              g_print (_("Type: %s\n"), gconf_value_type_to_string(stype));
              g_print (_("List Type: %s\n"), gconf_value_type_to_string(slist_type));
              g_print (_("Car Type: %s\n"), gconf_value_type_to_string(scar_type));
              g_print (_("Cdr Type: %s\n"), gconf_value_type_to_string(scdr_type));
              if (def_value)
                s = gconf_value_to_string (def_value);
              else
                s = NULL;
              g_print (_("Default Value: %s\n"), def_value ? s : _("Unset"));
              g_free (s);
              g_print (_("Owner: %s\n"), owner ? owner : _("Unset"));
              g_print (_("Short Desc: %s\n"), short_desc ? short_desc : _("Unset"));
              g_print (_("Long Desc: %s\n"), long_desc ? long_desc : _("Unset"));
            }

          gconf_value_free(value);
        }
      else
        {
          if (err == NULL)
            {
              g_printerr (_("No value set for `%s'\n"), *args);
            }
          else
            {
              g_printerr (_("Failed to get value for `%s': %s\n"),
                      *args, err->message);
              g_error_free(err);
              err = NULL;
            }
        }
 
      ++args;
    }
  return 0;
}

static GConfValueType
read_value_type(const char *string)
{
  GConfValueType type = GCONF_VALUE_INVALID;
  switch (*string)
    {
    case 'i':
    case 'I':
      type = GCONF_VALUE_INT;
      break;
    case 'f':
    case 'F':
      type = GCONF_VALUE_FLOAT;
      break;
    case 'b':
    case 'B':
      type = GCONF_VALUE_BOOL;
      break;
    case 's':
    case 'S':
      switch (string[1])
	{
	case 't':
	case 'T':
	  type = GCONF_VALUE_STRING;
	  break;
	case 'c':
	case 'C':
	  type = GCONF_VALUE_SCHEMA;
	  break;
	default:
	  g_printerr (_("Don't understand type `%s'\n"), string);
	}
      break;
    case 'l':
    case 'L':
      type = GCONF_VALUE_LIST;
      break;
    case 'p':
    case 'P':
      type = GCONF_VALUE_PAIR;
      break;
    default:
      g_printerr (_("Don't understand type `%s'\n"), string);
    }
  return type;
}

static int
do_set(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;
  
  if (args == NULL)
    {
      g_printerr (_("Must specify alternating keys/values as arguments\n"));
      return 1;
    }

  while (*args)
    {
      const gchar* key;
      const gchar* value;
      GConfValueType type = GCONF_VALUE_INVALID;
      GConfValueType list_type = GCONF_VALUE_INVALID;
      GConfValueType car_type = GCONF_VALUE_INVALID;
      GConfValueType cdr_type = GCONF_VALUE_INVALID;
      GConfValue* gval;

      key = *args;
      ++args;
      value = *args;

      if (value == NULL)
        {
          g_printerr (_("No value to set for key: `%s'\n"), key);
          return 1;
        }

      type = read_value_type(value_type);
      if (type == GCONF_VALUE_INVALID)
        return 1;
      if (value_list_type != NULL)
        {
          list_type = read_value_type(value_list_type);
          if (list_type == GCONF_VALUE_INVALID)
            return 1;
	}
      if (value_car_type != NULL)
        {
          car_type = read_value_type(value_car_type);
          if (car_type == GCONF_VALUE_INVALID)
            return 1;
	}
      if (value_cdr_type != NULL)
        {
          cdr_type = read_value_type(value_cdr_type);
          if (cdr_type == GCONF_VALUE_INVALID)
            return 1;
	}

      if (type == GCONF_VALUE_SCHEMA)
        {
          g_printerr (_("Cannot set schema as value\n"));
          return 1;
	}

      if (type == GCONF_VALUE_LIST &&
          list_type != GCONF_VALUE_STRING &&
          list_type != GCONF_VALUE_INT &&
          list_type != GCONF_VALUE_FLOAT &&
          list_type != GCONF_VALUE_BOOL)
        {
          g_printerr (_("When setting a list you must specify a primitive list-type\n"));
          return 1;
	}

      if (type == GCONF_VALUE_PAIR &&
          ((car_type != GCONF_VALUE_STRING &&
            car_type != GCONF_VALUE_INT &&
            car_type != GCONF_VALUE_FLOAT &&
            car_type != GCONF_VALUE_BOOL) ||
           (cdr_type != GCONF_VALUE_STRING &&
            cdr_type != GCONF_VALUE_INT &&
            cdr_type != GCONF_VALUE_FLOAT &&
            cdr_type != GCONF_VALUE_BOOL)))
        {
          g_printerr (_("When setting a pair you must specify a primitive car-type and cdr-type\n"));
          return 1;
	}

      err = NULL;

      if (type == GCONF_VALUE_LIST)
	gval = gconf_value_new_list_from_string(list_type, value, &err);
      else if (type == GCONF_VALUE_PAIR)
	gval = gconf_value_new_pair_from_string(car_type, cdr_type, value, &err);
      else
        gval = gconf_value_new_from_string(type, value, &err);

      if (gval == NULL)
        {
          g_printerr (_("Error: %s\n"),
		      err->message);
          g_error_free(err);
          err = NULL;
          return 1;
        }

      err = NULL;
          
      gconf_engine_set (conf, key, gval, &err);

      if (err != NULL)
        {
          g_printerr (_("Error setting value: %s\n"),
		      err->message);
          g_error_free(err);
          err = NULL;
          return 1;
        }

      gconf_value_free(gval);

      ++args;
    }

  err = NULL;

  gconf_engine_suggest_sync(conf, &err);

  if (err != NULL)
    {
      g_printerr (_("Error syncing: %s\n"),
              err->message);
      return 1;
    }

  return 0;
}

static int
do_toggle(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;
  
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more keys as arguments\n"));
      return 1;
    }

  while (*args)
    {
      const gchar* key;
      GConfValue* value;

      key = *args;
      err = NULL;

      value = get_maybe_without_default (conf, key, &err);

      if (value == NULL)
        {
          g_printerr (_("No value found for key %s\n"), key);
          return 1;
        }

      if (value->type != GCONF_VALUE_BOOL)
        {
          g_printerr (_("Not a boolean value: %s\n"), key);
          return 1;
        }

      gconf_value_set_bool (value, !gconf_value_get_bool (value));

      gconf_engine_set (conf, key, value, &err);

      if (err != NULL)
        {
          g_printerr (_("Error setting value: %s\n"), err->message);
          g_error_free(err);
          err = NULL;
          return 1;
        }

      gconf_value_free (value);

      ++args;     
    }

  return 0;
}

static int
do_get_type(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;

  if (args == NULL)
    {
      g_printerr (_("Must specify a key or keys to get type\n"));
      return 1;
    }
      
  while (*args)
    {
      GConfValue* value;

      err = NULL;

      value = get_maybe_without_default (conf, *args, &err);
         
      if (value != NULL)
	{
	  g_print ("%s\n", gconf_value_type_to_string (value->type));
	}
      else
        {
          if (err == NULL)
            {
              g_printerr (_("No value set for `%s'\n"), *args);
            }
          else
            {
              g_printerr (_("Failed to get value for `%s': %s\n"),
			  *args, err->message);
              g_error_free (err);
              err = NULL;
            }
        }
 
      ++args;
    }
  return 0;
}

static int
do_get_list_size(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;
  GConfValue *list = NULL;

  if (args == NULL || *args == NULL)
    {
      g_printerr (_("Must specify a key to lookup size of.\n"));
      return 1;
    }

  list = get_maybe_without_default (conf, *args, &err);

  if (list == NULL)
    {
      if (err == NULL)
	{
	  g_printerr (_("No value set for `%s'\n"), *args);
	}
      else
	{
	  g_printerr (_("Failed to get value for `%s': %s\n"),
		      *args, err->message);
	  g_error_free (err);
	  err = NULL;
	}

      return 1;
    }

  if (list->type != GCONF_VALUE_LIST)
    {
      g_printerr (_("Key %s is not a list.\n"), *args);
      return 1;
    }

  g_print ("%u\n", g_slist_length (gconf_value_get_list (list)));

  return 0;
}

static int
do_get_list_element(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;
  GConfValue *list = NULL, *element = NULL;
  GSList *iter = NULL;
  gchar* s = NULL;
  int idx = 0;

  if (args == NULL || *args == NULL)
    {
      g_printerr (_("Must specify a key from which to get list element.\n"));
      return 1;
    }

  list = get_maybe_without_default (conf, *args, &err);

  if (list == NULL)
    {
      if (err == NULL)
	{
	  g_printerr (_("No value set for `%s'\n"), *args);
	}
      else
	{
	  g_printerr (_("Failed to get value for `%s': %s\n"),
		      *args, err->message);
	  g_error_free (err);
	  err = NULL;
	}

      return 1;
    }

  if (list->type != GCONF_VALUE_LIST)
    {
      g_printerr (_("Key %s is not a list.\n"), *args);
      return 1;
    }

  if (args[1] == NULL)
    {
      g_printerr (_("Must specify list index.\n"));
      return 1;
    }

  idx = atoi (args[1]);
  if (idx < 0)
    {
      g_printerr (_("List index must be non-negative.\n"));
      return 1;
    }

  iter = gconf_value_get_list (list);
  element = g_slist_nth_data (iter, idx);

  if (element == NULL)
    {
      g_printerr (_("List index is out of bounds.\n"));
      return 1;
    }

  s = gconf_value_to_string (element);
  g_print ("%s\n", s);
  g_free (s);

  return 0;
} 

enum
{
  SCHEMA_INFO_SHORT_DOCS,
  SCHEMA_INFO_LONG_DOCS,
  SCHEMA_INFO_SCHEMA_NAME
};

static int
do_schema_info (GConfEngine *conf, const gchar **args,
                int info)
{
  GError* err = NULL;

  if (args == NULL)
    {
      g_printerr (_("Must specify a key or keys on the command line\n"));
      return 1;
    }
      
  while (*args)
    {
      GConfEntry* entry;

      err = NULL;

      entry = gconf_engine_get_entry (conf, *args, NULL, TRUE, &err);

      if (entry != NULL)
        {
          const char *s;
          
          s = gconf_entry_get_schema_name (entry);

          if (s == NULL)
            {
              g_printerr (_("No schema known for `%s'\n"), *args);
            }
          else if (info == SCHEMA_INFO_SCHEMA_NAME)
            {
              g_print ("%s\n", s);
            }
          else
            {
              GConfValue *val;

              err = NULL;
              
              val = gconf_engine_get (conf, s, &err);

              if (val != NULL && val->type == GCONF_VALUE_SCHEMA)
                {
                  GConfSchema *schema;
                  const char *docs;

                  docs = NULL;
                  schema = gconf_value_get_schema (val);

                  if (schema)
                    {
                      if (info == SCHEMA_INFO_SHORT_DOCS)
                        docs = gconf_schema_get_short_desc (schema);
                      else if (info == SCHEMA_INFO_LONG_DOCS)
                        docs = gconf_schema_get_long_desc (schema);
                    }
                  
                  if (docs)
                    g_print ("%s\n", docs);
                  else
                    g_printerr (_("No doc string stored in schema at '%s'\n"),
				s);
                }
              else if (err != NULL)
                {
                  g_printerr (_("Error getting schema at '%s': %s\n"),
			      s, err->message);
                  g_error_free (err);
                }
              else
                {
                  if (val == NULL)
                    g_printerr (_("No schema stored at '%s'\n"),
				s);
                  else
                    g_printerr (_("Value at '%s' is not a schema\n"),
				s);
                }

              if (val)
                gconf_value_free (val);
            }
          
          gconf_entry_free (entry);
        }
      else
        {
          if (err == NULL)
            {
              g_printerr (_("No value set for `%s'\n"), *args);
            }
          else
            {
              g_printerr (_("Failed to get value for `%s': %s\n"),
			  *args, err->message);
              g_error_free(err);
              err = NULL;
            }
        }
 
      ++args;
    }
  
  return 0;
}

static int
do_short_docs (GConfEngine *conf, const gchar **args)
{
  return do_schema_info (conf, args, SCHEMA_INFO_SHORT_DOCS);
}

static int
do_long_docs (GConfEngine *conf, const gchar **args)
{
  return do_schema_info (conf, args, SCHEMA_INFO_LONG_DOCS);
}

static int
do_get_schema_name (GConfEngine *conf, const gchar **args)
{
  return do_schema_info (conf, args, SCHEMA_INFO_SCHEMA_NAME);
}

static int
do_associate_schema (GConfEngine *conf, const gchar **args)
{
  GError *err;
  
  if (!args || args[0] == NULL || args[1] == NULL || args[2] != NULL)
    {
      g_printerr (_("Must specify a schema name followed by the key name to apply it to\n"));
      return 1;
    }

  err = NULL;
  if (!gconf_engine_associate_schema (conf, args[1], args[0], &err))
    {
      g_printerr (_("Error associating schema name '%s' with key name '%s': %s\n"),
		  args[0], args[1], err->message);
      g_error_free (err);

      return 1;
    }

  return 0;
}

static int
do_dissociate_schema (GConfEngine *conf, const gchar **args)
{
  GError *err;
  gboolean failed;
  
  if (!args || args[0] == NULL)
    {
      g_printerr (_("Must specify keys to unapply schema from\n"));
      return 1;
    }

  failed = FALSE;
  while (*args)
    {
      err = NULL;
      if (!gconf_engine_associate_schema (conf, *args, NULL, &err))
        {
          g_printerr (_("Error removing schema name from '%s': %s\n"),
		      *args, err->message);
          g_error_free (err);
          failed = TRUE;
        }
      
      ++args;
    }

  if (failed)
    return 1;
  else
    return 0;
}

static int
do_set_schema(GConfEngine* conf, const gchar** args)
{
  GConfSchema* sc;
  GConfValue* val;
  const gchar* key;
  GError* err = NULL;
  
  if ((args == NULL) || (args[1] != NULL))
    {
      g_printerr (_("Must specify key (schema name) as the only argument\n"));
      return 1;
    }
      
  key = *args;

  val = gconf_value_new(GCONF_VALUE_SCHEMA);

  sc = gconf_schema_new();

  gconf_value_set_schema_nocopy(val, sc);

  if (short_desc)
    gconf_schema_set_short_desc(sc, short_desc);

  if (long_desc)
    gconf_schema_set_long_desc(sc, long_desc);

  if (owner)
    gconf_schema_set_owner(sc, owner);

  if (value_type)
    {
      GConfValueType type = GCONF_VALUE_INVALID;

      type = read_value_type(value_type);

      if (type != GCONF_VALUE_INVALID)
        gconf_schema_set_type(sc, type);
    }

  if (value_list_type)
    {
      GConfValueType type = GCONF_VALUE_INVALID;

      type = read_value_type(value_list_type);

      if (type != GCONF_VALUE_STRING &&
          type != GCONF_VALUE_INT &&
          type != GCONF_VALUE_FLOAT &&
          type != GCONF_VALUE_BOOL)
	{
          g_printerr (_("List type must be a primitive type: string, int, float or bool\n"));
          return 1;

	}

      if (type != GCONF_VALUE_INVALID)
        gconf_schema_set_list_type(sc, type);
    }

  if (value_car_type)
    {
      GConfValueType type = GCONF_VALUE_INVALID;

      type = read_value_type(value_car_type);

      if (type != GCONF_VALUE_STRING &&
          type != GCONF_VALUE_INT &&
          type != GCONF_VALUE_FLOAT &&
          type != GCONF_VALUE_BOOL)
	{
          g_printerr (_("Pair car type must be a primitive type: string, int, float or bool\n"));
          return 1;

	}

      if (type != GCONF_VALUE_INVALID)
        gconf_schema_set_car_type(sc, type);
    }

  if (value_cdr_type)
    {
      GConfValueType type = GCONF_VALUE_INVALID;

      type = read_value_type(value_cdr_type);

      if (type != GCONF_VALUE_STRING &&
          type != GCONF_VALUE_INT &&
          type != GCONF_VALUE_FLOAT &&
          type != GCONF_VALUE_BOOL)
	{
          g_printerr (_("Pair cdr type must be a primitive type: string, int, float or bool\n"));
          return 1;

	}

      if (type != GCONF_VALUE_INVALID)
        gconf_schema_set_cdr_type(sc, type);
    }

  err = NULL;
      
  gconf_engine_set (conf, key, val, &err);
      
  if (err != NULL)
    {
      g_printerr (_("Error setting value: %s"),
		  err->message);
      g_error_free(err);
      err = NULL;
      return 1;
    }
      
  gconf_value_free(val);

  err = NULL;
  gconf_engine_suggest_sync(conf, &err);
      
  if (err != NULL)
    {
      g_printerr (_("Error syncing: %s"),
		  err->message);
      g_error_free(err);
      err = NULL;
      return 1;
    }

  return 0;
}

static int
do_all_entries(GConfEngine* conf, const gchar** args)
{
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more directories to get key/value pairs from.\n"));
      return 1;
    }
  
  return do_all_pairs(conf, args);
}

static int
do_unset(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;
  
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more keys to unset.\n"));
      return 1;
    }

  while (*args)
    {
      err = NULL;
      gconf_engine_unset(conf, *args, &err);

      if (err != NULL)
        {
          g_printerr (_("Error unsetting `%s': %s\n"),
		      *args, err->message);
          g_error_free(err);
          err = NULL;
        }

      ++args;
    }

  err = NULL;
  gconf_engine_suggest_sync(conf, NULL); /* ignore errors */

  return 0;
}

static int
do_recursive_unset (GConfEngine* conf, const gchar** args)
{
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more keys to recursively unset.\n"));
      return 1;
    }  
      
  while (*args)
    {
      GError *err;

      err = NULL;
      gconf_engine_recursive_unset (conf, *args,
                                    GCONF_UNSET_INCLUDING_SCHEMA_NAMES,
                                    &err);
      if (err != NULL)
        {
          g_printerr (_("Failure during recursive unset of \"%s\": %s\n"),
		      *args, err->message);
          g_error_free (err);
        }
      
      ++args;
    }

  gconf_engine_suggest_sync (conf, NULL); /* ignore errors */
  
  return 0;
}

static int
do_all_subdirs(GConfEngine* conf, const gchar** args)
{
  GError* err = NULL;
  
  if (args == NULL)
    {
      g_printerr (_("Must specify one or more directories to get subdirs from.\n"));
      return 1;
    }
      
  while (*args)
    {
      GSList* subdirs;
      GSList* tmp;

      err = NULL;

      subdirs = gconf_engine_all_dirs(conf, *args, &err);
          
      if (subdirs != NULL)
        {
          tmp = subdirs;

          while (tmp != NULL)
            {
              gchar* s = tmp->data;

              g_print (" %s\n", s);

              g_free(s);

              tmp = g_slist_next(tmp);
            }

          g_slist_free(subdirs);
        }
      else
        {
          if (err != NULL)
            {
              g_printerr (_("Error listing dirs: %s\n"),
			  err->message);
              g_error_free(err);
              err = NULL;
            }
        }
 
      ++args;
    }

  return 0;
}

static void
hash_listify_foreach(gpointer key, gpointer val, gpointer user_data)
{
  GSList** values;
  GConfSchema* schema;
  GConfValue* value;

  values = user_data;
  schema = val;

  value = gconf_value_new(GCONF_VALUE_SCHEMA);

  gconf_value_set_schema_nocopy(value, schema);

  *values = g_slist_prepend(*values, value);
}

static int
get_list_value_from_xml(xmlNodePtr node, GConfValue** ret_value)
{
  xmlNodePtr iter;
  GConfValue* value = NULL;
  GConfValueType list_type;
  GSList* list = NULL;
  char* list_type_str;

  list_type_str = (char *)xmlGetProp(node, (xmlChar *)"type");
  if (!list_type_str)
    {
      g_printerr ("WARNING: must specify a list type for using <list type=\"type\">\n");
      return 1;
    }

  list_type = gconf_value_type_from_string(list_type_str);

  xmlFree(list_type_str);

  if (list_type == GCONF_VALUE_INVALID)
    {
      g_printerr ("WARNING: invalid list type '%s'\n", list_type_str);
      return 1;
    }

  iter = node->xmlChildrenNode;

  while (iter != NULL)
    {
      if (strcmp((char *)iter->name, "value") == 0) 
        {
          GConfValue* element = NULL;

          if (get_first_value_from_xml(iter, &element) == 0)
            {
              if (element->type == list_type)
                list = g_slist_prepend (list, element);
              else
                g_printerr ("WARNING: list type and element type do not match: %s != %s\n",
			    gconf_value_type_to_string (list_type),
			    gconf_value_type_to_string (element->type));
            }
        }

      iter = iter->next;
    }

  list = g_slist_reverse(list);

  value = gconf_value_new(GCONF_VALUE_LIST);

  gconf_value_set_list_type(value, list_type);
  gconf_value_set_list_nocopy(value, list);

  *ret_value = value;

  return 0;
}

static inline void
get_car_cdr_value(xmlNodePtr node, GConfValue** ret_value)
{
  xmlNodePtr iter;

  iter = node->xmlChildrenNode;
  while (iter != NULL)
    {
      if (strcmp((char *)iter->name, "value") == 0)
        get_first_value_from_xml(iter, ret_value);

      iter = iter->next;
    }
}

static int
get_pair_value_from_xml(xmlNodePtr node, GConfValue** ret_value)
{
  xmlNodePtr iter;
  GConfValue* value = NULL;
  GConfValue* cdr = NULL;
  GConfValue* car = NULL;
  int retval = 0;

  iter = node->xmlChildrenNode;

  while (iter != NULL)
    {
      if (strcmp((char *)iter->name, "car") == 0)
        get_car_cdr_value(iter, &car);

      else if (strcmp((char *)iter->name, "cdr") == 0)
        get_car_cdr_value(iter, &cdr);

      iter = iter->next;
    }

  if (car && cdr)
    {
      value = gconf_value_new(GCONF_VALUE_PAIR);

      gconf_value_set_car(value, car);
      gconf_value_set_cdr(value, cdr);
    }
  else
    {
      g_printerr (_("WARNING: must specify both a <car> and a <cdr> in a <pair>\n"));
      retval = 1;
    }

  if (car)
    gconf_value_free(car);
  if (cdr)
    gconf_value_free(cdr);

  *ret_value = value;

  return retval;
}

static int
get_schema_values_from_xml(xmlNodePtr node, GSList** ret_values)
{
  GHashTable* schemas_hash;
  GSList* values = NULL;
  GSList* applyto_list;
  GSList* tmp;
  gchar* schema_key;
    
  if (get_schema_from_xml(node, &schema_key, &schemas_hash, &applyto_list) == 1)
    return 1;
    
  if (schema_key != NULL)
    g_printerr (_("WARNING: key specified (%s) for schema under a <value> - ignoring\n"),
            schema_key);
    
  g_hash_table_foreach(schemas_hash, hash_listify_foreach, &values);
    
  g_hash_table_destroy(schemas_hash);
    
  tmp = applyto_list;
  while (tmp != NULL)
    {
      g_free(tmp->data);
      tmp = tmp->next;
    }
  g_slist_free(applyto_list);
    
  g_free(schema_key);

  *ret_values = values;

  return 0;
}

static int
get_values_from_xml(xmlNodePtr node, GSList** ret_values)
{
  xmlNodePtr iter;
  GSList* values = NULL;
  GConfValue* value = NULL;

  iter = node->xmlChildrenNode;

  if (!iter)
    {
      g_printerr (_("WARNING: must have a child node under <value>\n"));
      return 1;
    }

  if (strcmp((char *)node->name, "value") != 0)
    {
      g_printerr (_("WARNING: node <%s> not understood\n"), node->name);
      return 1;
    }

  while (iter)
    {
      GError* error = NULL;
      char* content;

      if (strcmp((char *)iter->name, "int") == 0)
        {
          if ((content = (char *)xmlNodeGetContent(iter)) != NULL)
            {
              value = gconf_value_new_from_string(GCONF_VALUE_INT, content, &error);
              if (value == NULL)
                {
                  g_assert(error != NULL);
    
                  g_printerr (_("WARNING: Failed to parse int value `%s'\n"), content);
    
                  g_error_free(error);
                  error = NULL;
                }
              else
                {
                  g_assert(error == NULL);
                }
              xmlFree(content);
            }
        }
      else if (strcmp((char *)iter->name, "float") == 0)
        {
          if ((content = (char *)xmlNodeGetContent(iter)) != NULL)
            {
              value = gconf_value_new_from_string(GCONF_VALUE_FLOAT, content, &error);
              if (value == NULL)
                {
                  g_assert(error != NULL);
    
                  g_printerr (_("WARNING: Failed to parse float value `%s'\n"), content);
    
                  g_error_free(error);
                  error = NULL;
                }
              else
                {
                  g_assert(error == NULL);
                }
              xmlFree(content);
            }
    
        }
      else if (strcmp((char *)iter->name, "string") == 0)
        {
          if ((content = (char *)xmlNodeGetContent(iter)) != NULL)
            {
              value = gconf_value_new_from_string(GCONF_VALUE_STRING, content, &error);
              if (value == NULL)
                {
                  g_assert(error != NULL);
    
                  g_printerr (_("WARNING: Failed to parse string value `%s'\n"), content);
    
                  g_error_free(error);
                  error = NULL;
                }
              else
                {
                  g_assert(error == NULL);
                }
              xmlFree(content);
            }
        }
      else if (strcmp((char *)iter->name, "bool") == 0)
        {
          if ((content = (char *)xmlNodeGetContent(iter)) != NULL)
            {
              value = gconf_value_new_from_string(GCONF_VALUE_BOOL, content, &error);
              if (value == NULL)
                {
                  g_assert(error != NULL);
    
                  g_printerr (_("WARNING: Failed to parse boolean value `%s'\n"), content);
    
                  g_error_free(error);
                  error = NULL;
                }
              else
                {
                  g_assert(error == NULL);
                }
              xmlFree(content);
            }
        }
      else if (strcmp((char *)iter->name, "list") == 0)
        get_list_value_from_xml(iter, &value);

      else if (strcmp((char *)iter->name, "pair") == 0)
        get_pair_value_from_xml(iter, &value);

      else if (strcmp((char *)iter->name, "schema") == 0)
        get_schema_values_from_xml(iter, &values);

      iter = iter->next;
    }    

  /* common case */
  if (value)
    {
      g_assert (values == NULL);
      values = g_slist_prepend(values, value);
    }

  *ret_values = values;

  return 0;
}

/* Confusingly, parsing a <value> can give you multiple
 * values in the case of schemas. Use this if you're sure
 * there it can't be a schema.
 */
static int
get_first_value_from_xml(xmlNodePtr node, GConfValue** ret_value)
{
  GConfValue* value = NULL;
  GSList* values;
  GSList* tmp;

  if (get_values_from_xml(node, &values) == 1)
    return 1;

  g_assert (g_slist_length(values) <= 1);

  tmp = values;
  while (tmp)
    {
      if (!value)
        value = tmp->data;
      else
        gconf_value_free(tmp->data);
      tmp = tmp->next;
    }
  g_slist_free(values);

  *ret_value = value;

  return 0;
}

/*
 * Loading entries
 */

typedef struct {
  gchar *key;
  GConfValueType type;
  GConfValueType list_type;
  GConfValueType car_type;
  GConfValueType cdr_type;
  GConfValue* value;
} EntryInfo;

static void
set_values(GConfEngine* conf, gboolean unload, const gchar* base_dir, const gchar* key, const char* schema_key, GSList* values)
{
  GSList* tmp;
  gchar* full_key;
 
  if (base_dir)
    full_key = gconf_concat_dir_and_key(base_dir, key);
  else
    full_key = g_strdup(key);

  if (unload || schema_key)
    {
      gchar* full_schema_key = NULL;
      GError* error = NULL;

      if (!unload)
        {
          if (base_dir && *schema_key != '/')
            full_schema_key = gconf_concat_dir_and_key(base_dir, schema_key);
          else
            full_schema_key = g_strdup(schema_key);
        }

      if (!gconf_engine_associate_schema(conf, full_key, full_schema_key,  &error))
        {
          g_assert(error != NULL);
          
          g_printerr (_("WARNING: failed to associate schema `%s' with key `%s': %s\n"),
                      full_schema_key, full_key, error->message);
          g_error_free(error);
        }

      g_free(full_schema_key);
    }

  tmp = values;
  while (tmp)
    {
      GConfValue* value = tmp->data;
      GError* error;

      error = NULL;
      if (!unload)
        gconf_engine_set(conf, full_key, value, &error);
      else
        gconf_engine_unset(conf, full_key, &error);
      if (error != NULL)
        {
          g_printerr (_("Error setting value: %s\n"), error->message);
          g_error_free(error);
          error = NULL;
        }

      tmp = tmp->next;
    }

  g_free(full_key);
}

static int
process_entry(GConfEngine* conf, gboolean unload, xmlNodePtr node, const gchar** base_dirs, const char* orig_base)
{
  xmlNodePtr iter;
  GSList* values = NULL;
  GSList* tmp;
  char* key = NULL;
  char* schema_key = NULL;
  int retval = 0;

  iter = node->xmlChildrenNode;
  while (iter)
    {
      if (strcmp((char *)iter->name, "key") == 0)
        key = (char *)xmlNodeGetContent(iter);

      else if (strcmp((char *)iter->name, "value") == 0)
        get_values_from_xml(iter, &values);

      else if (strcmp((char *)iter->name, "schema_key") == 0)
        schema_key = (char *)xmlNodeGetContent(iter);

      iter = iter->next;
    }

  if (key && (values || schema_key))
    {
      if (!base_dirs)
        set_values(conf, unload, orig_base, key, schema_key, values);

      else while (*base_dirs)
        {
          set_values(conf, unload, *base_dirs, key, schema_key, values);
          ++base_dirs;
        }
    }
  else
    retval = 1;

  tmp = values;
  while (tmp)
    {
      gconf_value_free(tmp->data);
      tmp = tmp->next;
    }
  g_slist_free(values);

  if (key)
    xmlFree(key);

  if (schema_key)
    xmlFree(schema_key);

  return retval;
}

/*
 * Schema stuff
 */

typedef struct _SchemaInfo SchemaInfo;

struct _SchemaInfo {
  gchar* key;
  gchar* owner;
  GSList* apply_to;
  GConfValueType type;
  GConfValueType list_type;
  GConfValueType car_type;
  GConfValueType cdr_type;
  GConfValue* global_default;
  GHashTable* hash;
};

static int
fill_default_from_string(SchemaInfo* info, const gchar* default_value,
                         GConfValue** retloc)
{
  g_return_val_if_fail(default_value != NULL, 1);

  switch (info->type)
    {
    case GCONF_VALUE_INVALID:
      g_printerr (_("WARNING: invalid or missing type for schema (%s)\n"),
		  info->key);
      break;

    case GCONF_VALUE_LIST:
      {
        GError* error = NULL;
        if (info->list_type == GCONF_VALUE_INVALID)
	  {
            g_printerr (_("WARNING: invalid or missing list_type for schema (%s)\n"),
			info->key);
            break;
	  }
        *retloc = gconf_value_new_list_from_string(info->list_type,
						   default_value,
						   &error);
        if (*retloc == NULL)
          {
            g_assert(error != NULL);

            g_printerr (_("WARNING: Failed to parse default value `%s' for schema (%s)\n"), default_value, info->key);

            g_error_free(error);
            error = NULL;
          }
        else
          {
            g_assert(error == NULL);
          }
      }
      break;

    case GCONF_VALUE_PAIR:
      {
        GError* error = NULL;
        if (info->car_type == GCONF_VALUE_INVALID ||
	    info->cdr_type == GCONF_VALUE_INVALID)
	  {
            g_printerr (_("WARNING: invalid or missing car_type or cdr_type for schema (%s)\n"),
                    info->key);
            break;
	  }
        *retloc = gconf_value_new_pair_from_string(info->car_type,
						   info->cdr_type,
						   default_value,
						   &error);
        if (*retloc == NULL)
          {
            g_assert(error != NULL);

            g_printerr (_("WARNING: Failed to parse default value `%s' for schema (%s)\n"), default_value, info->key);

            g_error_free(error);
            error = NULL;
          }
        else
          {
            g_assert(error == NULL);
          }
      }
      break;

    case GCONF_VALUE_SCHEMA:
      g_printerr (_("WARNING: You cannot set a default value for a schema\n"));
      break;

    case GCONF_VALUE_STRING:
    case GCONF_VALUE_INT:
    case GCONF_VALUE_BOOL:
    case GCONF_VALUE_FLOAT:
      {
        GError* error = NULL;
        *retloc = gconf_value_new_from_string(info->type,
                                              default_value,
                                              &error);
        if (*retloc == NULL)
          {
            g_assert(error != NULL);

            g_printerr (_("WARNING: Failed to parse default value `%s' for schema (%s)\n"), default_value, info->key);

            g_error_free(error);
            error = NULL;
          }
        else
          {
            g_assert(error == NULL);
          }
      }
      break;
      
    default:
      g_printerr (_("WARNING: gconftool internal error, unknown GConfValueType\n"));
      break;
    }

  return 0;
}

static int
extract_global_info(xmlNodePtr node,
                    SchemaInfo* info)
{
  xmlNodePtr iter;
  xmlNodePtr default_value_node = NULL;
  char* default_value = NULL;
      
  iter = node->xmlChildrenNode;

  while (iter != NULL)
    {
      if (iter->type == XML_ELEMENT_NODE)
        {
          char* tmp;
      
          if (strcmp((char *)iter->name, "key") == 0)
            {
              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp)
                {
                  info->key = g_strdup(tmp);
                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "owner") == 0)
            {
              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp)
                {
                  info->owner = g_strdup(tmp);
                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "type") == 0)
            {
              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp)
                {
                  info->type = gconf_value_type_from_string(tmp);
                  if (info->type == GCONF_VALUE_INVALID)
                    g_printerr (_("WARNING: failed to parse type name `%s'\n"),
				tmp);
                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "list_type") == 0)
            {
              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp)
                {
                  info->list_type = gconf_value_type_from_string(tmp);
                  if (info->list_type != GCONF_VALUE_INT &&
		      info->list_type != GCONF_VALUE_FLOAT &&
		      info->list_type != GCONF_VALUE_STRING &&
		      info->list_type != GCONF_VALUE_BOOL)
		    {
		      info->list_type = GCONF_VALUE_INVALID;
                      g_printerr (_("WARNING: list_type can only be int, float, string or bool and not `%s'\n"),
				  tmp);
		    }
                  else if (info->list_type == GCONF_VALUE_INVALID)
                    g_printerr (_("WARNING: failed to parse type name `%s'\n"),
				tmp);
    		  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "car_type") == 0)
            {
              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp)
                {
                  info->car_type = gconf_value_type_from_string(tmp);
                  if (info->car_type != GCONF_VALUE_INT &&
		      info->car_type != GCONF_VALUE_FLOAT &&
		      info->car_type != GCONF_VALUE_STRING &&
		      info->car_type != GCONF_VALUE_BOOL)
		    {
		      info->car_type = GCONF_VALUE_INVALID;
                      g_printerr (_("WARNING: car_type can only be int, float, string or bool and not `%s'\n"),
				  tmp);
		    }
                  else if (info->car_type == GCONF_VALUE_INVALID)
                    g_printerr (_("WARNING: failed to parse type name `%s'\n"),
				tmp);
                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "cdr_type") == 0)
            {
              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp)
                {
                  info->cdr_type = gconf_value_type_from_string(tmp);
                  if (info->cdr_type != GCONF_VALUE_INT &&
		      info->cdr_type != GCONF_VALUE_FLOAT &&
		      info->cdr_type != GCONF_VALUE_STRING &&
		      info->cdr_type != GCONF_VALUE_BOOL)
		    {
		      info->cdr_type = GCONF_VALUE_INVALID;
                      g_printerr (_("WARNING: cdr_type can only be int, float, string or bool and not `%s'\n"),
				  tmp);
		    }
                  else if (info->cdr_type == GCONF_VALUE_INVALID)
                    g_printerr (_("WARNING: failed to parse type name `%s'\n"),
				tmp);
                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "default") == 0)
            {
              default_value = (char *)xmlNodeGetContent(iter);
            }
          else if (strcmp((char *)iter->name, "default_value") == 0)
            {
              xmlNodePtr tmp_node = iter->xmlChildrenNode;

              while (tmp_node)
                {
                  if (strcmp((char *)tmp_node->name, "value") == 0)
                    default_value_node = tmp_node;

                  tmp_node = tmp_node->next;
                }
            }
          else if (strcmp((char *)iter->name, "locale") == 0)
            {
              ; /* ignore, this is parsed later after we have the global info */
            }
          else if (strcmp((char *)iter->name, "applyto") == 0)
            {
              /* Add the contents to the list of nodes to apply to */
              tmp = (char *)xmlNodeGetContent(iter);

              if (tmp)
                {
                  info->apply_to = g_slist_prepend(info->apply_to, g_strdup(tmp));
                  xmlFree(tmp);
                }
              else
                g_printerr (_("WARNING: empty <applyto> node"));
            }
          else
            g_printerr (_("WARNING: node <%s> not understood below <schema>\n"),
			iter->name);

        }
      
      iter = iter->next;
    }

  if (info->type == GCONF_VALUE_LIST &&
      info->list_type == GCONF_VALUE_INVALID)
    {
      g_printerr (_("WARNING: no <list_type> specified for schema of type list\n"));
    }

  if (info->type == GCONF_VALUE_PAIR &&
      info->car_type == GCONF_VALUE_INVALID)
    {
      g_printerr (_("WARNING: no <car_type> specified for schema of type pair\n"));
    }

  if (info->type == GCONF_VALUE_PAIR &&
      info->cdr_type == GCONF_VALUE_INVALID)
    {
      g_printerr (_("WARNING: no <cdr_type> specified for schema of type pair\n"));
    }
  
  if (default_value_node != NULL)
    get_first_value_from_xml (default_value_node, &info->global_default);

  else if (default_value != NULL)
    /* Have to do this last, because the type may come after the default
     * value
     */
    fill_default_from_string(info, default_value, &info->global_default);

  if (default_value)
    xmlFree(default_value);

  return 0;
}

static int
process_locale_info(xmlNodePtr node, SchemaInfo* info)
{
  char* name;
  GConfSchema* schema;
  xmlNodePtr iter;
  
  name = (char *)xmlGetProp(node, (xmlChar *)"name");

  if (name == NULL)
    {
      g_printerr (_("WARNING: <locale> node has no `name=\"locale\"' attribute, ignoring\n"));
      return 1;
    }

  if (g_hash_table_lookup(info->hash, name) != NULL)
    {
      g_printerr (_("WARNING: multiple <locale> nodes for locale `%s', ignoring all past first\n"),
              name);
      xmlFree(name);
      return 1;
    }
  
  schema = gconf_schema_new();

  gconf_schema_set_locale(schema, name);

  /* Only set the global default on the C locale */
  if (strcmp(name, "C") == 0 && info->global_default != NULL)
    gconf_schema_set_default_value(schema, info->global_default);

  /* Fill in the global info */
  if (info->type != GCONF_VALUE_INVALID)
    gconf_schema_set_type(schema, info->type);

  if (info->list_type != GCONF_VALUE_INVALID)
    gconf_schema_set_list_type(schema, info->list_type);

  if (info->car_type != GCONF_VALUE_INVALID)
    gconf_schema_set_car_type(schema, info->car_type);

  if (info->cdr_type != GCONF_VALUE_INVALID)
    gconf_schema_set_cdr_type(schema, info->cdr_type);

  if (info->owner != NULL)
    gconf_schema_set_owner(schema, info->owner);

  xmlFree(name);

  /* Locale-specific info */
  iter = node->xmlChildrenNode;
  
  while (iter != NULL)
    {
      if (iter->type == XML_ELEMENT_NODE)
        {
          if (strcmp((char *)iter->name, "default_value") == 0)
            {
              GConfValue* val = NULL;
              xmlNodePtr tmp_node;

              tmp_node = iter->xmlChildrenNode;
              while (tmp_node)
                {
                  if (strcmp((char *)tmp_node->name, "value") == 0)
                    {
                      get_first_value_from_xml(tmp_node, &val);
                      if (val)
                        gconf_schema_set_default_value_nocopy(schema, val);
                      break;
                    }
                  tmp_node = tmp_node->next;
                }
            }
          else if (strcmp((char *)iter->name, "default") == 0)
            {
              GConfValue* val = NULL;
              char* tmp;

              tmp = (char *)xmlNodeGetContent(iter);
              if (tmp != NULL)
                {
                  fill_default_from_string(info, tmp, &val);
                  if (val)
                    gconf_schema_set_default_value_nocopy(schema, val);

                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "short") == 0)
            {
              char* tmp;

              tmp = (char *)xmlNodeGetContent(iter);

              if (tmp != NULL)
                {
                  gconf_schema_set_short_desc(schema, tmp);
                  xmlFree(tmp);
                }
            }
          else if (strcmp((char *)iter->name, "long") == 0)
            {
              char* tmp;

              tmp = (char *)xmlNodeGetContent(iter);

              if (tmp != NULL)
                {
                  gconf_schema_set_long_desc(schema, tmp);
                  xmlFree(tmp);
                }
            }
          else
            {
              g_printerr (_("WARNING: Invalid node <%s> in a <locale> node\n"),
			  iter->name);
            }
        }
      
      iter = iter->next;
    }

  g_hash_table_insert(info->hash,
                      (gchar*)gconf_schema_get_locale(schema), /* cheat to save copying this string */
                      schema);

  return 0;
}

static int
process_key_list(GConfEngine* conf, gboolean unload, const gchar* schema_name, GSList* keylist)
{
  GSList* tmp;

  tmp = keylist;
  while (tmp != NULL)
    {
      GError* error = NULL;

      if (!gconf_engine_associate_schema(conf, tmp->data, !unload ? schema_name : NULL, &error))
        {
          g_assert(error != NULL);
          
          g_printerr (_("WARNING: failed to associate schema `%s' with key `%s': %s\n"),
		      schema_name, (gchar*)tmp->data, error->message);
          g_error_free(error);
          error = NULL;
        }
      else
        {
          g_assert(error == NULL);
          g_print (_("Attached schema `%s' to key `%s'\n"),
		   schema_name, (gchar*)tmp->data);
        }
          
      tmp = g_slist_next(tmp);
    }
  
  return 0;
}

static int
get_schema_from_xml(xmlNodePtr node, gchar **schema_key, GHashTable** schemas_hash, GSList **applyto_list)
{
  xmlNodePtr iter;
  SchemaInfo info;

  info.key = NULL;
  info.type = GCONF_VALUE_INVALID;
  info.list_type = GCONF_VALUE_INVALID;
  info.car_type = GCONF_VALUE_INVALID;
  info.cdr_type = GCONF_VALUE_INVALID;
  info.apply_to = NULL;
  info.owner = NULL;
  info.global_default = NULL;
  info.hash = g_hash_table_new(g_str_hash, g_str_equal);
  
  extract_global_info(node, &info);

  if (info.type == GCONF_VALUE_INVALID)
    {
      g_free(info.owner);

      if (info.global_default)
        gconf_value_free(info.global_default);

      return 1;
    }
  
  iter = node->xmlChildrenNode;

  while (iter != NULL)
    {
      if (iter->type == XML_ELEMENT_NODE)
        {
          if (strcmp((char *)iter->name, "key") == 0)
            ; /* nothing */
          else if (strcmp((char *)iter->name, "owner") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "type") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "list_type") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "car_type") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "cdr_type") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "default") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "applyto") == 0)
            ;  /* nothing */
          else if (strcmp((char *)iter->name, "locale") == 0)
            {
              process_locale_info(iter, &info);
            }
          else
            g_printerr (_("WARNING: node <%s> not understood below <schema>\n"),
			iter->name);
        }
          
      iter = iter->next;
    }

  if (g_hash_table_size(info.hash) == 0)
    {
      g_printerr (_("You must have at least one <locale> entry in a <schema>\n"));
      return 1;
    }

  *schema_key = info.key;
  *schemas_hash = info.hash;
  *applyto_list = info.apply_to;
      
  g_free(info.owner);
  if (info.global_default)
    gconf_value_free(info.global_default);
  
  return 0;
}

static void
hash_install_foreach(gpointer key, gpointer value, gpointer user_data)
{
  struct {
    GConfEngine* conf;
    gboolean unload;
    char* key;
  } *info;
  GConfSchema* schema;
  GError* error = NULL;
  
  info = user_data;
  schema = value;

  if (!info->unload)
    {
      if (!gconf_engine_set_schema(info->conf, info->key, schema, &error))
	{
	  g_assert(error != NULL);

	  g_printerr (_("WARNING: failed to install schema `%s', locale `%s': %s\n"),
		      info->key, gconf_schema_get_locale(schema), error->message);
	  g_error_free(error);
	  error = NULL;
	}
      else
	{
	  g_assert(error == NULL);
	  g_print (_("Installed schema `%s' for locale `%s'\n"),
		   info->key, gconf_schema_get_locale(schema));
	}
    }
  else
    {
      if (!gconf_engine_unset (info->conf, info->key, &error))
	{
	  g_assert(error != NULL);

	  g_printerr (_("WARNING: failed to uninstall schema `%s', locale `%s': %s\n"),
		      info->key, gconf_schema_get_locale(schema), error->message);
	  g_error_free(error);
	  error = NULL;
	}
      else
	{
	  g_assert(error == NULL);
	  g_print (_("Uninstalled schema `%s' from locale `%s'\n"),
		   info->key, gconf_schema_get_locale(schema));
	}
    }

  gconf_schema_free(schema);
}

static int
process_schema(GConfEngine* conf, gboolean unload, xmlNodePtr node)
{
  struct {
    GConfEngine* conf;
    gboolean unload;
    char* key;
  } hash_foreach_info;
  GHashTable* schemas_hash = NULL;
  GSList* applyto_list = NULL;
  GSList* tmp;
  gchar* schema_key = NULL;
  int retval = 0;

  if (get_schema_from_xml(node, &schema_key, &schemas_hash, &applyto_list) == 1)
    return 1;

  g_assert(schemas_hash != NULL);

  if (schema_key != NULL)
    {
      process_key_list(conf, unload, schema_key, applyto_list);

      hash_foreach_info.conf = conf;
      hash_foreach_info.unload = unload;
      hash_foreach_info.key = schema_key;
      g_hash_table_foreach(schemas_hash, hash_install_foreach, &hash_foreach_info);
    }
  else
    {
      g_printerr (_("WARNING: no key specified for schema\n"));
      retval = 1;
    }

  g_hash_table_destroy(schemas_hash);

  tmp = applyto_list;
  while (tmp != NULL)
    {
      g_free(tmp->data);
      tmp = tmp->next;
    }
  g_slist_free(applyto_list);

  g_free(schema_key);

  return retval;
}

static int
process_list(GConfEngine* conf, LoadType load_type, gboolean unload, xmlNodePtr node, const gchar** base_dirs)
{
#define LOAD_TYPE_TO_LIST(t) ((t == LOAD_SCHEMA_FILE) ? "schemalist" : "entrylist")

  xmlNodePtr iter;
  char* orig_base = NULL;
  
  iter = node->xmlChildrenNode;

  if (load_type == LOAD_ENTRY_FILE)
    orig_base = (char *)xmlGetProp(node, (xmlChar *)"base");

  while (iter != NULL)
    {
      if (iter->type == XML_ELEMENT_NODE)
        {
          if (load_type == LOAD_SCHEMA_FILE && strcmp((char *)iter->name, "schema") == 0)
            process_schema(conf, unload, iter);
          else if (load_type == LOAD_ENTRY_FILE && strcmp((char *)iter->name, "entry") == 0)
            process_entry(conf, unload, iter, base_dirs, orig_base);
          else
            g_printerr (_("WARNING: node <%s> not understood below <%s>\n"),
			iter->name, LOAD_TYPE_TO_LIST(load_type));
        }
      
      iter = iter->next;
    }

  if (orig_base)
    xmlFree(orig_base);

  return 0;

#undef LOAD_TYPE_TO_LIST
}

static int
do_load_file(GConfEngine* conf, LoadType load_type, gboolean unload, const gchar* file, const gchar** base_dirs)
{
#define LOAD_TYPE_TO_ROOT(t) ((t == LOAD_SCHEMA_FILE) ? "gconfschemafile" : "gconfentryfile")
#define LOAD_TYPE_TO_LIST(t) ((t == LOAD_SCHEMA_FILE) ? "schemalist" : "entrylist")

  xmlDocPtr doc;
  xmlNodePtr iter;
  /* file comes from the command line, is thus in locale charset */
  gchar *utf8_file = g_locale_to_utf8 (file, -1, NULL, NULL, NULL);;

  errno = 0;
  doc = xmlParseFile(file);

  if (doc == NULL)
    {
      if (errno != 0)
        g_printerr (_("Failed to open `%s': %s\n"),
		    utf8_file, g_strerror(errno));
      return 1;
    }

  if (doc->xmlRootNode == NULL)
    {
      g_printerr (_("Document `%s' is empty?\n"),
		  utf8_file);
      return 1;
    }

  iter = doc->xmlRootNode;
  while (iter != NULL) 
    {
      if (iter->type == XML_ELEMENT_NODE)
        { 
          if (strcmp((char *)iter->name, LOAD_TYPE_TO_ROOT(load_type)) != 0)
            {
              g_printerr (_("Document `%s' has the wrong type of root node (<%s>, should be <%s>)\n"),
			  utf8_file, iter->name, LOAD_TYPE_TO_ROOT(load_type));
              return 1;
            }
          else
            break;
        }         

      iter = iter->next;
    }
  
  if (iter == NULL)
    {
      g_printerr (_("Document `%s' has no top level <%s> node\n"),
		  utf8_file, LOAD_TYPE_TO_ROOT(load_type));
      return 1;
    }

  iter = iter->xmlChildrenNode;

  while (iter != NULL)
    {
      if (iter->type == XML_ELEMENT_NODE)
        {
          if (strcmp((char *)iter->name, LOAD_TYPE_TO_LIST(load_type)) == 0)
            process_list(conf, load_type, unload, iter, base_dirs);
          else
            g_printerr (_("WARNING: node <%s> below <%s> not understood\n"),
			iter->name, LOAD_TYPE_TO_ROOT(load_type));
        }
          
      iter = iter->next;
    }
  
  return 0;
#undef LOAD_TYPE_TO_LIST
#undef LOAD_TYPE_TO_ROOT
}

static int
do_sync(GConfEngine* conf)
{
  GError *err = NULL;
  
  gconf_engine_suggest_sync(conf, &err);

  if (err != NULL)
    {
      g_printerr (_("Error syncing configuration data: %s"),
		  err->message);
      g_error_free(err);
      return 1;
    }
  
  return 0;
}

static int
do_makefile_install(GConfEngine* conf, const gchar** args, gboolean unload)
{
  int retval = 0;

  if (args == NULL)
    {
      g_printerr (_("Must specify some schema files to install\n"));
      return 1;
    }

  while (*args)
    {
      if (do_load_file(conf, LOAD_SCHEMA_FILE, unload, *args, NULL) != 0)
        retval |= 1;

      ++args;
    }

  retval |= do_sync (conf);
  return retval;
}

typedef enum {
  BreakageSetBadValues,
  BreakageCleanup
} BreakagePhase;

static gboolean
check_err(GError** err)
{
  g_print (".");
  
  if (*err != NULL)
    {
      g_printerr (_("\n%s\n"),
		  (*err)->message);
      g_error_free(*err);
      *err = NULL;
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
key_breakage(GConfEngine* conf, const gchar* key, BreakagePhase phase)
{
  GError* error = NULL;
  
  if (phase == BreakageCleanup)
    {
      gconf_engine_unset(conf, key, &error);
      if (error != NULL)
        {
          g_printerr (_("Failed to unset breakage key %s: %s\n"),
		      key, error->message);
          g_error_free(error);
          return FALSE;
        }
    }
  else if (phase == BreakageSetBadValues)
    {
      gint an_int = 43;
      gboolean a_bool = TRUE;
      gdouble a_float = 43695.435;
      const gchar* a_string = "Hello";
      GConfValue* val;
      GSList* list = NULL;
      
      g_print ("  +");
      
      gconf_engine_set_string(conf, key, "", &error);
      if (check_err(&error))
        return FALSE;
      
      gconf_engine_set_string(conf, key, "blah blah blah 93475028934670 @%^%$&%$&^%", &error);
      if (check_err(&error))
        return FALSE;
      
      gconf_engine_set_bool(conf, key, TRUE, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_bool(conf, key, FALSE, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_float(conf, key, 100.0, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_float(conf, key, -100.0, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_float(conf, key, 0.0, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_int(conf, key, 0, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_int(conf, key, 5384750, &error);
      if (check_err(&error))
        return FALSE;
      
      gconf_engine_set_int(conf, key, -11, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_list(conf, key, GCONF_VALUE_BOOL, list, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_pair(conf, key, GCONF_VALUE_INT, GCONF_VALUE_BOOL,
                            &an_int, &a_bool, &error);
      if (check_err(&error))
        return FALSE;

      gconf_engine_set_pair(conf, key, GCONF_VALUE_FLOAT, GCONF_VALUE_STRING,
                            &a_float, &a_string, &error);
      if (check_err(&error))
        return FALSE;

      /* empty pair */
      val = gconf_value_new(GCONF_VALUE_PAIR);
      gconf_engine_set (conf, key, val, &error);
      gconf_value_free(val);
      if (check_err(&error))
        return FALSE;

      list = NULL;
      gconf_engine_set_list(conf, key, GCONF_VALUE_STRING, list, &error);
      if (check_err(&error))
        return FALSE;
      gconf_engine_set_list(conf, key, GCONF_VALUE_INT, list, &error);
      if (check_err(&error))
        return FALSE;
      gconf_engine_set_list(conf, key, GCONF_VALUE_BOOL, list, &error);
      if (check_err(&error))
        return FALSE;

      list = g_slist_prepend(list, GINT_TO_POINTER(10));
      list = g_slist_prepend(list, GINT_TO_POINTER(14));
      list = g_slist_prepend(list, GINT_TO_POINTER(-93));
      list = g_slist_prepend(list, GINT_TO_POINTER(1000000));
      list = g_slist_prepend(list, GINT_TO_POINTER(32));
      gconf_engine_set_list(conf, key, GCONF_VALUE_INT, list, &error);
      if (check_err(&error))
        return FALSE;

      g_slist_free(list);
      list = NULL;

      list = g_slist_prepend(list, "");
      list = g_slist_prepend(list, "blah");
      list = g_slist_prepend(list, "");
      list = g_slist_prepend(list, "\n\t\r\n     \n");
      list = g_slist_prepend(list, "woo fooo s^%*^%&@^$@%&@%$");
      gconf_engine_set_list(conf, key, GCONF_VALUE_STRING, list, &error);
      if (check_err(&error))
        return FALSE;

      g_slist_free(list);
      list = NULL;
      
      g_print ("\n");
    }
  else
    g_assert_not_reached();
      
  return TRUE;
}

static int
do_break_key(GConfEngine* conf, const gchar** args)
{
  if (args == NULL)
    {
      g_printerr (_("Must specify some keys to break\n"));
      return 1;
    }
  
  while (*args)
    {
      g_print (_("Trying to break your application by setting bad values for key:\n  %s\n"), *args);
      
      if (!key_breakage(conf, *args, BreakageSetBadValues))
        return 1;
      if (!key_breakage(conf, *args, BreakageCleanup))
        return 1;

      ++args;
    }

  return 0;
}

static int
do_break_directory(GConfEngine* conf, const gchar** args)
{
  if (args == NULL)
    {
      g_printerr (_("Must specify some directories to break\n"));
      return 1;
    }
  
  while (*args)
    {
      gchar* keys[10] = { NULL };
      gchar* full_keys[10] = { NULL };
      int i;

      i = 0;
      while (i < 10)
        {
          keys[i] = gconf_unique_key();
          full_keys[i] = gconf_concat_dir_and_key(*args, keys[i]);

          ++i;
        }

      g_print (_("Trying to break your application by setting bad values for keys in directory:\n  %s\n"), *args);

      i = 0;
      while (i < 10)
        {
          if (!key_breakage(conf, full_keys[i], BreakageSetBadValues))
            return 1;

          ++i;
        }
      
      i = 0;
      while (i < 10)
        {
          if (!key_breakage(conf, full_keys[i], BreakageCleanup))
            return 1;
          
          ++i;
        }

      i = 0;
      while (i < 10)
        {
          g_free(keys[i]);
          g_free(full_keys[i]);

          ++i;
        }
      
      ++args;
    }

  return 0;
}

static int
do_get_default_source (const gchar** args)
{
  gchar *filename;
  gchar *source;
  gchar buf[512];
  FILE *f;

  /* Try with $sysgconfdir/schema-install-source */
  filename = g_build_filename (GCONF_ETCDIR, "schema-install-source", NULL);
  f = g_fopen(filename, "r");
  g_free (filename);

  if (f != NULL)
    {
      source = fgets(buf, 512, f);
      fclose(f);  
      if (source)
	{
	  g_strchomp(source);
	  if (*source != '\0')
	    {
	      g_print ("%s\n", source);
	      return 0;
	    }
	}
    }

  /* Use default database */
  source = g_strconcat("xml:merged:", GCONF_ETCDIR, "/gconf.xml.defaults", NULL);
  g_print ("%s\n", source);
  g_free(source);

  return 0;
}
