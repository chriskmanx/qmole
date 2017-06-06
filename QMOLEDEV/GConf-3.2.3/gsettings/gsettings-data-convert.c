/*
 * Copyright (C) 2010 Red Hat, Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Author: Matthias Clasen <mclasen@redhat.com>
 */

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include <glib.h>
#include <gio/gio.h>
#include <gconf/gconf-client.h>

static gboolean changed = FALSE;
static gboolean verbose = FALSE;
static gboolean dry_run = FALSE;

extern const gchar *gconf_value_type_to_string (int type);

static gboolean
type_uint32 (GSettings   *settings,
             const gchar *key)
{
  const GVariantType *type;
  GVariant *value;

  value = g_settings_get_value (settings, key);
  type = g_variant_get_type (value);
  g_variant_unref (value);

  return g_variant_type_equal (type, G_VARIANT_TYPE_UINT32);
}

static gboolean
handle_file (const gchar *filename)
{
  GKeyFile *keyfile;
  GConfClient *client;
  GConfValue *value;
  gint i, j;
  gchar *gconf_key;
  gchar **groups;
  gchar **keys;
  GVariantBuilder *builder;
  GVariant *v;
  const gchar *s;
  gchar *str;
  gint ii;
  GSList *list, *l;
  GSettings *settings;
  GError *error;

  keyfile = g_key_file_new ();

  error = NULL;
  if (!g_key_file_load_from_file (keyfile, filename, 0, &error))
    {
      if (verbose)
        g_printerr ("%s: %s\n", filename, error->message);
      g_error_free (error);

      g_key_file_free (keyfile);

      return FALSE;
    }

  client = gconf_client_get_default ();

  groups = g_key_file_get_groups (keyfile, NULL);
  for (i = 0; groups[i]; i++)
    {
      gchar **schema_path;

      schema_path = g_strsplit (groups[i], ":", 2);

      if (verbose)
        {
          g_print ("collecting settings for schema '%s'\n", schema_path[0]);
          if (schema_path[1])
            g_print ("for storage at '%s'\n", schema_path[1]);
        }

      if (schema_path[1] != NULL)
        settings = g_settings_new_with_path (schema_path[0], schema_path[1]);
      else
        settings = g_settings_new (schema_path[0]);

      g_settings_delay (settings);

      error = NULL;
      if ((keys = g_key_file_get_keys (keyfile, groups[i], NULL, &error)) == NULL)
        {
          g_printerr ("%s", error->message);
          g_error_free (error);

          continue;
        }

      for (j = 0; keys[j]; j++)
        {
          if (strchr (keys[j], '/') != 0)
            {
              g_printerr ("Key '%s' contains a '/'\n", keys[j]);

              continue;
            }

          error = NULL;
          if ((gconf_key = g_key_file_get_string (keyfile, groups[i], keys[j], &error)) ==  NULL)
            {
              g_printerr ("%s", error->message);
              g_error_free (error);

              continue;
            }

          error = NULL;
          if ((value = gconf_client_get_without_default (client, gconf_key, &error)) == NULL)
            {
              if (error)
                {
                  g_printerr ("Failed to get GConf key '%s': %s\n",
                              gconf_key, error->message);
                  g_error_free (error);
                }
              else
                {
                  if (verbose)
                    g_print ("Skipping GConf key '%s', no user value\n",
                             gconf_key);
                }

              g_free (gconf_key);

              continue;
            }

          switch (value->type)
            {
            case GCONF_VALUE_STRING:
              if (dry_run)
                g_print ("set key '%s' to string '%s'\n", keys[j],
                         gconf_value_get_string (value));
              else
                g_settings_set (settings, keys[j], "s",
                                gconf_value_get_string (value));
              break;

            case GCONF_VALUE_INT:
              if (dry_run)
                g_print ("set key '%s' to integer '%d'\n",
                         keys[j], gconf_value_get_int (value));
              else
                {
                  GVariant *range;
                  gchar *type;

                  range = g_settings_get_range (settings, keys[j]);
                  g_variant_get (range, "(&sv)", &type, NULL);

                  if (strcmp (type, "enum") == 0)
                    g_settings_set_enum (settings, keys[j], gconf_value_get_int (value));
                  else if (strcmp (type, "flags") == 0)
                    g_settings_set_flags (settings, keys[j], gconf_value_get_int (value));
                  else if (type_uint32 (settings, keys[j]))
                    g_settings_set (settings, keys[j], "u",
                                    gconf_value_get_int (value));
                  else
                    g_settings_set (settings, keys[j], "i",
                                    gconf_value_get_int (value));

                  g_variant_unref (range);
                }
              break;

            case GCONF_VALUE_BOOL:
              if (dry_run)
                g_print ("set key '%s' to boolean '%d'\n",
                         keys[j], gconf_value_get_bool (value));
              else
                g_settings_set (settings, keys[j], "b",
                                gconf_value_get_bool (value));
              break;

            case GCONF_VALUE_FLOAT:
              if (dry_run)
                g_print ("set key '%s' to double '%g'\n",
                         keys[j], gconf_value_get_float (value));
              else
                g_settings_set (settings, keys[j], "d",
                                gconf_value_get_float (value));
              break;

            case GCONF_VALUE_LIST:
              switch (gconf_value_get_list_type (value))
                {
                case GCONF_VALUE_STRING:
                  builder = g_variant_builder_new (G_VARIANT_TYPE_ARRAY);
                  list = gconf_value_get_list (value);
                  if (list != NULL)
                    {
                      for (l = list; l; l = l->next)
                        {
                          GConfValue *lv = l->data;
                          s = gconf_value_get_string (lv);
                          g_variant_builder_add (builder, "s", s);
                        }
                      v = g_variant_new ("as", builder);
                    }
                  else
                    v = g_variant_new_array (G_VARIANT_TYPE_STRING, NULL, 0);
                  g_variant_ref_sink (v);

                  if (dry_run)
                    {
                      str = g_variant_print (v, FALSE);
                      g_print ("set key '%s' to a list of strings: %s\n",
                               keys[j], str);
                      g_free (str);
                    }
                  else
                    g_settings_set_value (settings, keys[j], v);

                  g_variant_unref (v);
                  g_variant_builder_unref (builder);
                  break;

                case GCONF_VALUE_INT:
                  builder = g_variant_builder_new (G_VARIANT_TYPE_ARRAY);
                  list = gconf_value_get_list (value);
                  if (list != NULL)
                    {
                      for (l = list; l; l = l->next)
                        {
                          GConfValue *lv = l->data;
                          ii = gconf_value_get_int (lv);
                          g_variant_builder_add (builder, "i", ii);
                        }
                      v = g_variant_new ("ai", builder);
                    }
                  else
                    v = g_variant_new_array (G_VARIANT_TYPE_INT32, NULL, 0);
                  g_variant_ref_sink (v);

                  if (dry_run)
                    {
                      str = g_variant_print (v, FALSE);
                      g_print ("set key '%s' to a list of integers: %s\n",
                               keys[j], str);
                      g_free (str);
                    }
                  else
                    g_settings_set_value (settings, keys[j], v);

                  g_variant_unref (v);
                  g_variant_builder_unref (builder);
                  break;

                default:
                  g_printerr ("Keys of type 'list of %s' not handled yet\n",
                              gconf_value_type_to_string (gconf_value_get_list_type (value)));
                  break;
                }
              break;

            default:
              g_printerr ("Keys of type %s not handled yet\n",
                          gconf_value_type_to_string (value->type));
              break;
            }

          gconf_value_free (value);
          g_free (gconf_key);
        }

      g_strfreev (keys);

      if (!dry_run)
        g_settings_apply (settings);

      g_object_unref (settings);
      g_strfreev (schema_path);
    }

  g_strfreev (groups);

  g_object_unref (client);

  return TRUE;
}

static gboolean
handle_dir (const gchar *dirname,
            time_t       stored_mtime,
            GHashTable  *converted)
{
  time_t dir_mtime;
  struct stat statbuf;
  GDir *dir;
  const gchar *name;
  gchar *filename;
  GError *error;

  /* If the directory is not newer, exit */
  if (stat (dirname, &statbuf) == 0)
    dir_mtime = statbuf.st_mtime;
 else
    {
      if (verbose)
        g_print ("Directory '%s' does not exist, nothing to do\n", dirname);
      return TRUE;
    }

  if (dir_mtime <= stored_mtime)
    {
      if (verbose)
        g_print ("Directory '%s' all uptodate, nothing to do\n", dirname);
      return TRUE;
    }

  error = NULL;
  dir = g_dir_open (dirname, 0, &error);
  if (dir == NULL)
    {
      g_printerr ("Failed to open '%s': %s\n", dirname, error->message);
      return FALSE;
    }

  while ((name = g_dir_read_name (dir)) != NULL)
    {
      if (g_hash_table_lookup (converted, name))
        {
          if (verbose)
            g_print ("File '%s already converted, skipping\n", name);
          goto next;
        }

      filename = g_build_filename (dirname, name, NULL);

      if (handle_file (filename))
        {
          gchar *myname = g_strdup (name);

          /* Add the the file to the converted list */
          g_hash_table_insert (converted, myname, myname);
          changed = TRUE;
        }

      g_free (filename);

 next: ;
    }

  return TRUE;
}

/* get_string_set() and set_string_set() could be GKeyFile API */
static GHashTable *
get_string_set (GKeyFile     *keyfile,
                const gchar  *group,
                const gchar  *key,
                GError      **error)
{
  GHashTable *converted;
  gchar **list;
  gint i;

  list = g_key_file_get_string_list (keyfile, group, key, NULL, error);

  if (list == NULL)
    return NULL;

  converted = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
  for (i = 0; list[i]; i++)
    g_hash_table_insert (converted, list[i], list[i]);

  /* The hashtable now owns the strings, so only free the array */
  g_free (list);

  return converted;
}

static void
set_string_set (GKeyFile    *keyfile,
                const gchar *group,
                const gchar *key,
                GHashTable  *set)
{
  GHashTableIter iter;
  GString *list;
  gpointer item;

  list = g_string_new (NULL);
  g_hash_table_iter_init (&iter, set);
  while (g_hash_table_iter_next (&iter, &item, NULL))
    g_string_append_printf (list, "%s;", (const gchar *) item);

  g_key_file_set_value (keyfile, group, key, list->str);
  g_string_free (list, TRUE);
}

static GHashTable *
load_state (time_t *mtime)
{
  GHashTable *converted;
  GHashTable *tmp;
  gchar *filename;
  GKeyFile *keyfile;
  GError *error;
  gchar *str;

  converted = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
  *mtime = 0;

  filename = g_build_filename (g_get_user_data_dir (), "gsettings-data-convert", NULL);
  keyfile = g_key_file_new ();

  /* ensure file exists */
  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    return converted;

  error = NULL;
  if (!g_key_file_load_from_file (keyfile, filename, 0, &error))
    {
      g_printerr ("%s: %s\n", filename, error->message);
      g_error_free (error);
      return converted;
    }

  error = NULL;
  if ((str = g_key_file_get_string (keyfile, "State", "timestamp", &error)) == NULL)
    {
      g_printerr ("%s\n", error->message);
      g_error_free (error);
    }
  else
    {
      *mtime = (time_t)g_ascii_strtoll (str, NULL, 0);
      g_free (str);
    }

  error = NULL;
  if ((tmp = get_string_set (keyfile, "State", "converted", &error)) == NULL)
    {
      g_printerr ("%s\n", error->message);
      g_error_free (error);
    }
  else
    {
      g_hash_table_unref (converted);
      converted = tmp;
    }

  g_key_file_free (keyfile);
  g_free (filename);

  return converted;
}

static gboolean
save_state (GHashTable *converted)
{
  gchar *filename;
  GKeyFile *keyfile;
  gchar *str;
  GError *error;
  gboolean result;

  /* Make sure the state directory exists */
  if (g_mkdir_with_parents (g_get_user_data_dir (), 0755))
    {
      g_printerr ("Failed to create directory %s: %s\n",
                  g_get_user_data_dir (), g_strerror (errno));
      return FALSE;
    }

  filename = g_build_filename (g_get_user_data_dir (), "gsettings-data-convert", NULL);
  keyfile = g_key_file_new ();

  str = g_strdup_printf ("%ld", time (NULL));
  g_key_file_set_string (keyfile,
                         "State", "timestamp", str);
  g_free (str);

  set_string_set (keyfile, "State", "converted", converted);

  str = g_key_file_to_data (keyfile, NULL, NULL);
  g_key_file_free (keyfile);

  error = NULL;
  if (!g_file_set_contents (filename, str, -1, &error))
    {
      g_printerr ("%s\n", error->message);
      g_error_free (error);

      result = FALSE;
    }
  else
    result = TRUE;

  g_free (filename);
  g_free (str);

  return result;
}

int
main (int argc, char *argv[])
{
  time_t stored_mtime;
  const gchar * const *data_dirs;
  gint i;
  GError *error;
  GHashTable *converted;
  GOptionContext *context;
  const gchar *extra_file = NULL;
  GOptionEntry entries[] = {
    { "verbose", 0, 0, G_OPTION_ARG_NONE, &verbose, "show verbose messages", NULL },
    { "dry-run", 0, 0, G_OPTION_ARG_NONE, &dry_run, "do not perform any changes", NULL },
    { "file", 0, 0, G_OPTION_ARG_STRING, &extra_file, "perform conversions from an extra file", NULL },
    { NULL }
  };

  g_type_init();

  context = g_option_context_new ("");

  g_option_context_set_summary (context,
    "Migrate settings from the users GConf database to GSettings.");

  g_option_context_add_main_entries (context, entries, NULL);

  error = NULL;
  if (!g_option_context_parse (context, &argc, &argv, &error))
    {
      g_printerr ("%s\n", error->message);
      return 1;
    }

  converted = load_state (&stored_mtime);

  if (extra_file)
    {
      gchar *base;

      base = g_path_get_basename (extra_file);

      if (g_hash_table_lookup (converted, base))
        {
          if (verbose)
            g_print ("'%s' is already converted.  Skipping.\n", base);
        }
      else
        {
          if (handle_file (extra_file))
            {
              gchar *myname = g_strdup (base);

              g_hash_table_insert (converted, myname, myname);
              changed = TRUE;
            }
        }

      g_free (base);
    }

  data_dirs = g_get_system_data_dirs ();
  for (i = 0; data_dirs[i]; i++)
    {
      gchar *convert_dir;

      convert_dir = g_build_filename (data_dirs[i], "GConf", "gsettings", NULL);

      if (!handle_dir (convert_dir, stored_mtime, converted))
        return 1;

      g_free (convert_dir);
    }

  if (changed && !dry_run)
    {
      if (!save_state (converted))
        return 1;
    }

  return 0;
}

