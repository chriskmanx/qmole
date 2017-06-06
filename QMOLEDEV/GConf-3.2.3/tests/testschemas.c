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











/*
 *
 *
 * DO NOT USE THESE CRAPPY TESTS AS EXAMPLE CODE. USE THE DOCS AND examples
 *
 *
 *
 */








#include <gconf/gconf.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <locale.h>
#include <gconf/gconf-internals.h>

/* glibc printf() functions accept NULL for a %s format, but to be
   safe, check for null strings and return a printable value */
const char     *
NULL_SAFE(const char *x)
{
	return x ? x : "<NULL>";
}

static void
check(gboolean condition, const gchar* fmt, ...)
{
  va_list args;
  gchar* description;
  
  va_start (args, fmt);
  description = g_strdup_vprintf(fmt, args);
  va_end (args);
  
  if (condition)
    {
      printf(".");
      fflush(stdout);
    }
  else
    {
      fprintf(stderr, "\n*** FAILED: %s\n", description);
      exit(1);
    }
  
  g_free(description);
}

static const gchar*
keys[] = {
  "/testing/foo/tar",
  "/testing/foo/bar",
  "/testing/quad",
  "/testing/blah",
  "/testing/q/a/b/c/z/w/x/y/z",
  "/testing/foo/baz",
  "/testing/oops/bloo",
  "/testing/oops/snoo",
  "/testing/oops/kwoo",
  "/testing/foo/quaz",
  NULL
};

static const gchar*
locales[] = {
  "C",
  "es",
  "en",
  "no",
  NULL
};

static gint ints[] = { -1, -2, -3, 0, 1, 2, 3, 4000, 0xfffff, -0xfffff, G_MININT, G_MAXINT, 0, 0, 57, 83, 95 };
static const guint n_ints = sizeof(ints)/sizeof(ints[0]);

static void
check_unset(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;

  keyp = keys;

  while (*keyp)
    {
      g_assert (err == NULL);
      
      gconf_engine_unset (conf, *keyp, &err);

      if (err != NULL)
        {
          fprintf (stderr, "unset of `%s' failed: %s\n", *keyp, err->message);
          g_error_free (err);
          err = NULL;
          exit (1);
        }
      else
        {
          GConfValue* val;
          gchar* valstr;
          
          val = gconf_engine_get_without_default (conf, *keyp, &err);

          if (err != NULL)
            {
              fprintf (stderr, "gconf_engine_get_without_default on key %s failed: %s\n", *keyp, err->message);
              g_error_free (err);
              err = NULL;
              exit (1);
            }
          
          if (val)
            valstr = gconf_value_to_string (val);
          else
            valstr = g_strdup ("(none)");
          
          check (val == NULL, "unsetting a previously-set value `%s' the value `%s' existed", *keyp, valstr);

          g_free (valstr);
        }
      
      ++keyp;
    }
}

void
check_int_storage(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;
  guint i; 
  
  /* Loop over keys, storing all values at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      while (i < n_ints)
        {
          gint gotten;
          
          if (!gconf_engine_set_int(conf, *keyp, ints[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%d': %s\n",
                      *keyp, ints[i], err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_int(conf, *keyp, &err);

              if (err != NULL)
                {
                  check(gotten == 0.0, "0.0 not returned though there was an error");

                  fprintf(stderr, "Failed to get key `%s': %s\n",
                          *keyp, err->message);
                  g_error_free(err);
                  err = NULL;
                }
              else
                {
                  check (ints[i] == gotten,
                         "int set/get pair: `%d' set, `%d' got",
                         ints[i], gotten);

                }
            }
          
          ++i;
        }
      
      ++keyp;
    }

  /* Now invert the loop and see if that causes problems */

  i = 0;
  while (i < n_ints)
    {

      keyp = keys;

      while (*keyp)
        {
          gint gotten;
          
          if (!gconf_engine_set_int(conf, *keyp, ints[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%d': %s\n",
                      *keyp, ints[i], err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_int(conf, *keyp, &err);

              if (err != NULL)
                {
                  check(gotten == 0, "0 not returned though there was an error");

                  fprintf(stderr, "Failed to get key `%s': %s\n",
                          *keyp, err->message);
                  g_error_free(err);
                  err = NULL;
                }
              else
                {
                  check (ints[i] == gotten,
                         "int set/get pair: `%d' set, `%d' got",
                         ints[i], gotten);

                }
            }
          
      
          ++keyp;
        }

      ++i;
    }
          
  check_unset(conf);
}

static int
null_safe_strcmp(const char* lhs, const char* rhs)
{
  if (lhs == NULL && rhs == NULL)
    return 0;
  else if (lhs == NULL)
    return 1;
  else if (rhs == NULL)
    return -1;
  else
    return strcmp(lhs, rhs);
}

void
check_one_schema(GConfEngine* conf, const gchar** keyp, GConfSchema* schema)
{
  GError* err = NULL;
  
  if (!gconf_engine_set_schema (conf, *keyp, schema, &err))
    {
      fprintf (stderr, "Failed to set key `%s' to schema: %s\n",
              *keyp, err->message);
      g_error_free (err);
      err = NULL;
    }
  else
    {
      GConfSchema* gotten;
      GConfValue *val;
      
      val = gconf_engine_get_with_locale (conf, *keyp, gconf_schema_get_locale (schema), &err);

      if (err != NULL)
        {
          check (val == NULL, "NULL not returned though there was an error");

          fprintf (stderr, "Failed to get key `%s': %s\n",
                   *keyp, err->message);
          g_error_free(err);
          err = NULL;
        }
      else
        {
          check (val != NULL, "Did not get a schema back after setting it");

          check (val->type == GCONF_VALUE_SCHEMA, "Got type %s back instead of schema",
                 gconf_value_type_to_string (val->type));

          gotten = gconf_value_get_schema (val);
          
          check (gconf_schema_get_type (schema) == gconf_schema_get_type (gotten),
                 "schema set/get pair: type `%s' set, `%s' got",
                 gconf_value_type_to_string (gconf_schema_get_type (schema)),
                 gconf_value_type_to_string (gconf_schema_get_type (gotten)));

          /* If we set the schema for the current locale be sure we get it back */
          if (null_safe_strcmp (gconf_current_locale (), gconf_schema_get_locale (schema)) == 0)
            {
              check (null_safe_strcmp(gconf_current_locale(), gconf_schema_get_locale(gotten)) == 0,
                     "schema set/get pair: locale `%s' set, `%s' got",
                     gconf_current_locale(),
                     NULL_SAFE (gconf_schema_get_locale (gotten)));
            }
              
          check (null_safe_strcmp (gconf_schema_get_short_desc (schema), gconf_schema_get_short_desc (gotten)) == 0,
                 "schema set/get pair: short_desc `%s' set, `%s' got",
                 NULL_SAFE (gconf_schema_get_short_desc (schema)),
                 NULL_SAFE (gconf_schema_get_short_desc (gotten)));

          check (null_safe_strcmp (gconf_schema_get_long_desc (schema), gconf_schema_get_long_desc (gotten)) == 0,
                 "schema set/get pair: long_desc `%s' set, `%s' got",
                 NULL_SAFE (gconf_schema_get_long_desc (schema)),
                 NULL_SAFE (gconf_schema_get_long_desc (gotten)));

          check (null_safe_strcmp (gconf_schema_get_owner(schema), gconf_schema_get_owner(gotten)) == 0,
                 "schema set/get pair: owner `%s' set, `%s' got",
                 NULL_SAFE (gconf_schema_get_owner (schema)),
                 NULL_SAFE (gconf_schema_get_owner (gotten)));

          {
            GConfValue* set_default;
            GConfValue* got_default;

            set_default = gconf_schema_get_default_value (schema);
            got_default = gconf_schema_get_default_value (gotten);

            if (set_default && got_default)
              {
                check (set_default->type == GCONF_VALUE_INT,
                      "set default type is INT");
                
                check (set_default->type == got_default->type,
                       "schema set/get pair: default value type `%s' set, `%s' got",
                       gconf_value_type_to_string(set_default->type),
                       gconf_value_type_to_string(got_default->type));
                
                check (set_default->type == got_default->type,
                       "schema set/get pair: default value type `%s' set, `%s' got",
                       gconf_value_type_to_string(set_default->type),
                       gconf_value_type_to_string(got_default->type));
                
                check (gconf_value_get_int(set_default) == gconf_value_get_int(got_default),
                       "schema set/get pair: default value (int) `%d' set, `%d' got",
                       gconf_value_get_int(set_default), gconf_value_get_int(got_default));
              }
            else
              {
                /* mem leak */
                check (set_default == NULL && got_default == NULL,
                       "set and got default value aren't both NULL (`%s' and `%s')",
                       set_default ? gconf_value_to_string(set_default) : "NULL",
                       got_default ? gconf_value_to_string(got_default) : "NULL");
              }
          }
          
          gconf_value_free (val);
        }
    }
}
      
void
check_schema_storage (GConfEngine* conf)
{
  const gchar** keyp = NULL;
  guint i; 
  const gchar** localep = NULL;
  
  /* Loop over keys, storing all values at each then retrieving them */
  
  keyp = keys;
  localep = locales;

  while (*keyp)
    {
      i = 0;
      while (i < n_ints)
        {
          GConfSchema* schema;
          gchar* short_desc;
          gchar* long_desc;
          GConfValue* default_value;
          const int default_value_int = 97992;

          if (*localep == NULL)
            localep = locales;
          
          schema = gconf_schema_new ();

          gconf_schema_set_type (schema, GCONF_VALUE_INT);
          gconf_schema_set_locale (schema, *localep);
          short_desc = g_strdup_printf ("Schema for key `%s' storing value %d",
                                        *keyp, ints[i]);
          gconf_schema_set_short_desc (schema, short_desc);

          long_desc = g_strdup_printf ("Long description for schema with short description `%s' is really really long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long "
                                      "UTF-8: German (Deutsch Süd) Grüß Gott Greek (Ελληνικά) Γειά σας Hebrew(שלום) Hebrew punctuation(\xd6\xbfש\xd6\xbb\xd6\xbc\xd6\xbb\xd6\xbfל\xd6\xbcו\xd6\xbc\xd6\xbb\xd6\xbb\xd6\xbfם\xd6\xbc\xd6\xbb\xd6\xbf) Japanese (日本語) Thai (สวัสดีครับ) Thai wrong spelling (คำต่อไปนื่สะกดผิด พัั้ัั่งโกะ)\n", short_desc);
          
          gconf_schema_set_long_desc (schema, long_desc);

          gconf_schema_set_owner (schema, "testschemas");

          default_value = gconf_value_new (GCONF_VALUE_INT);
          gconf_value_set_int (default_value, default_value_int);
          
          gconf_schema_set_default_value_nocopy (schema, default_value);

          check (gconf_value_get_int (gconf_schema_get_default_value (schema)) == default_value_int,
                 "Properly stored default int value in the schema");

          check_one_schema (conf, keyp, schema);
          
          gconf_schema_free(schema);
          g_free(long_desc);
          g_free(short_desc);
          
          ++i;
        }
      
      ++keyp;
      ++localep;
    }

  /* Check setting/getting "empty" schemas */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      while (i < n_ints)
        {
          GConfSchema* schema;
          
          schema = gconf_schema_new();

          /* this isn't guaranteed to be the same on get/set */
          gconf_schema_set_locale(schema, "C");
          
          gconf_schema_set_type(schema, GCONF_VALUE_INT);

          check_one_schema(conf, keyp, schema);

          gconf_schema_free(schema);
          
          ++i;
        }
      
      ++keyp;
    }
  
  check_unset(conf);
}


enum
{
  SCHEMA_INFO_SHORT_DOCS,
  SCHEMA_INFO_LONG_DOCS,
  SCHEMA_INFO_SCHEMA_NAME
};

static char*
get_schema_info (GConfEngine *conf, const gchar *key, int info)
{
  GError* err = NULL;
  GConfEntry* entry;
  char *retval;

  retval = NULL;
  
  err = NULL;
  entry = gconf_engine_get_entry (conf, key, NULL, TRUE, &err);

  if (entry != NULL)
    {
      const char *s;
          
      s = gconf_entry_get_schema_name (entry);

      if (s == NULL)
        {
          retval = NULL;
        }
      else if (info == SCHEMA_INFO_SCHEMA_NAME)
        {
          retval = g_strdup (s);
        }
      else
        {
          GConfValue *val;

          err = NULL;
              
          val = gconf_engine_get (conf, s, &err);

          if (val != NULL && val->type == GCONF_VALUE_SCHEMA)
            {
              GConfSchema *schema;

              schema = gconf_value_get_schema (val);

              if (schema)
                {
                  if (info == SCHEMA_INFO_SHORT_DOCS)
                    retval = g_strdup (gconf_schema_get_short_desc (schema));
                  else if (info == SCHEMA_INFO_LONG_DOCS)
                    retval = g_strdup (gconf_schema_get_long_desc (schema));
                }
            }
          else if (err != NULL)
            {
              fprintf (stderr, "Error getting schema at '%s': %s\n",
                       s, err->message);
              g_error_free (err);
              exit (1);
            }
          else
            {
              if (val == NULL)
                fprintf (stderr, "No schema stored at '%s'\n",
                         s);
              else
                fprintf (stderr, "Value at '%s' is not a schema\n",
                         s);

              exit (1);
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
          fprintf(stderr, "No value set for `%s'\n", key);
        }
      else
        {
          fprintf(stderr, "Failed to get value for `%s': %s\n",
                  key, err->message);
          g_error_free(err);
          err = NULL;
        }
    }

  return retval;
}

void 
check_schema_use(GConfEngine * conf)
{
  GError     *err = NULL;
  const gchar   **keyp = NULL;
  GConfSchema    *schema = gconf_schema_new();
  GConfValue     *value = gconf_value_new(GCONF_VALUE_STRING);
  char           *schema_key = "/schemas/desktop/test-schema";
  char           *s;
  char           *schema_value = "string value from schema";
  char           *non_schema_value = "a string value *not* from schema";
  
  gconf_schema_set_type(schema, GCONF_VALUE_STRING);
  gconf_schema_set_locale(schema, "C");
  gconf_schema_set_short_desc(schema, "short desc");
  gconf_schema_set_long_desc(schema, "long desc");
  gconf_schema_set_owner(schema, "testschemas");
  gconf_value_set_string(value, schema_value);
  gconf_schema_set_default_value(schema, value);

  if (!gconf_engine_set_schema(conf, schema_key, schema, &err))
    {
      fprintf(stderr, "gconf_engine_set_schema -> %s\n", err->message);
      g_error_free (err);      
      err = NULL;
      exit (1);
    }
  keyp = keys;
  while (*keyp)
    {
      if (!gconf_engine_associate_schema(conf, *keyp, schema_key, &err))
        {
          fprintf(stderr, "gconf_engine_associate_schema -> %s\n", err->message);
          g_error_free (err);          
          err = NULL;
          exit (1);
        }

      /* Make sure we can get schema name */
      s = get_schema_info (conf, *keyp, SCHEMA_INFO_SCHEMA_NAME);
      if (s == NULL)
        {
          fprintf (stderr, "ERROR: Failed to get initial schema name\n");
          exit (1);
        }
      else if (strcmp (s, schema_key) != 0)
        {
          fprintf (stderr, "ERROR: got wrong schema name '%s'\n", schema_key);
          exit (1);
        }

      g_free (s);

      
      ++keyp;
    }

  /* FIXME we need to actually exit with error if these checks fail */
  
  keyp = keys;
  while (*keyp)
    {
      g_assert (err == NULL);
      gconf_engine_unset (conf, *keyp, &err);

      if (err)
        {
          fprintf(stderr, "gconf_engine_unset -> %s\n", err->message);
          g_error_free (err);
          err = NULL;
          exit (1);
        }
      
      s = gconf_engine_get_string(conf, *keyp, &err);

      if (err)
        {
          fprintf(stderr, "gconf_engine_get_string -> %s\n", err->message);
          g_error_free (err);
          err = NULL;
          exit (1);
        }
      
      if (s)
        {
          if (strcmp(s, schema_value) != 0)
            {
              fprintf(stderr, "ERROR: Failed to get schema value (got '%s', not '%s')\n", s, schema_value);
              exit (1);
            }
          else
            {
              printf(".");
            }

          g_free (s);
        }
      else
        {
          fprintf(stderr, "ERROR: Failed to get a value when expecting schema value\n");
          exit (1);
        }

      /* associate_schema should accept NULL to unset schemas */
      if (!gconf_engine_associate_schema (conf, *keyp, "/bogus-nonschema", &err))
        {
          fprintf(stderr, "gconf_engine_associate_schema -> %s\n", err->message);
          g_error_free (err);          
          err = NULL;
          exit (1);
        }

      /* Make sure we can get schema name */
      s = get_schema_info (conf, *keyp, SCHEMA_INFO_SCHEMA_NAME);
      if (s == NULL)
        {
          fprintf (stderr, "ERROR: Failed to get bogus schema name\n");
          exit (1);
        }
      else if (strcmp (s, "/bogus-nonschema") != 0)
        {
          fprintf (stderr, "ERROR: got wrong schema name '%s'\n", schema_key);
          exit (1);
        }

      g_free (s);
      
      s = gconf_engine_get_string(conf, *keyp, &err);

      if (err)
        {
          fprintf(stderr, "gconf_engine_get_string -> %s\n", err->message);
          g_error_free (err);
          err = NULL;
          exit (1);
        }
      
      if (s != NULL)
        {
          fprintf(stderr, "Failed to disassociate schema, found '%s'\n", s);
          g_free (s);
          exit (1);
        }
      
      gconf_engine_set_string(conf, *keyp, non_schema_value, &err);
      if (err != NULL)
        {
          fprintf(stderr, "gconf_engine_set_string -> %s\n", err->message);
          g_error_free (err);          
          err = NULL;
          exit (1);
        }
      
      s = gconf_engine_get_string(conf, *keyp, &err);
      
      if (s)
        {
          if (strcmp(s, non_schema_value) != 0)
            {
              fprintf(stderr, "ERROR: Failed to get non-schema value (got '%s', not '%s')\n", s, non_schema_value);
              exit (1);
            }
          else
            {
              printf(".");
            }

          g_free (s);
        }
      else
        {
          fprintf(stderr, "ERROR: Failed to get a value when expecting non-schema value\n");

          if (err != NULL)
            {
              fprintf(stderr, "gconf_engine_get_string -> %s\n", err->message);
              g_error_free (err);          
              err = NULL;
            }

          exit (1);
        }

      /* Make sure we can still get schema name when we have a value set */
      s = get_schema_info (conf, *keyp, SCHEMA_INFO_SCHEMA_NAME);
      if (s == NULL)
        {
          fprintf (stderr, "ERROR: Failed to get schema name when a value was set in the database\n");
          exit (1);
        }
      else if (strcmp (s, "/bogus-nonschema") != 0)
        {
          fprintf (stderr, "ERROR: got wrong schema name '%s'\n", schema_key);
          exit (1);
        }

      g_free (s);
      
      fflush(stdout);
      ++keyp;
    }
}

int 
main (int argc, char** argv)
{
  GConfEngine* conf;
  GError* err = NULL;

  setlocale (LC_ALL, "");
  
  if (!gconf_init(argc, argv, &err))
    {
      fprintf(stderr, "Failed to init GConf: %s\n", err->message);
      g_error_free(err);
      err = NULL;
      return 1;
    }
  
  conf = gconf_engine_get_default();

  check(conf != NULL, "create the default conf engine");
  
  printf("\nChecking integer storage:");
  
  check_int_storage(conf);

  printf("\nChecking schema storage:");

  check_schema_storage(conf);
  
  printf("\nChecking schema use:");

  check_schema_use(conf);
  
  gconf_engine_unref(conf);

  printf("\n\n");
  
  return 0;
}
