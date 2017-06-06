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
#include <gconf/gconf-internals.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>

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
keys_in_foo[] = {
  "tar",
  "bar",
  "klar",
  "dar",
  "far",
  "sar",
  NULL
};

/* Check whether all_dirs works if you implicitly create
   directories by creating keys inside them */
static void
check_dir_listing(GConfEngine* conf)
{
  const gchar** iter;
  GSList* entries;
  GSList* iter2;
  GError* error = NULL;
  gboolean found[sizeof(keys_in_foo)];
  int i;
  gboolean got_it;
  
  iter = keys_in_foo;

  while (*iter)
    {
      gchar* full_key;
      gchar* tmp;

      tmp = g_strconcat(*iter, "/woo", NULL);
      
      full_key = gconf_concat_dir_and_key("/testing/foo", tmp);
      g_free(tmp);
      
      gconf_engine_set_int(conf, full_key, 10, &error);

      check (error == NULL, "Error setting key %s: %s",
             full_key, error ? error->message : "");
      
      g_free(full_key);

      ++iter;
    }

  /* Check that "testing" comes back in a list of / */
  entries = gconf_engine_all_dirs(conf, "/", &error);

  check (error == NULL, "Error getting list of dirs in /: %s",
         error ? error->message : "");

  got_it = FALSE;
  
  iter2 = entries;
  while (iter2 != NULL)
    {
      if (strcmp(iter2->data, "/testing") == 0)
        got_it = TRUE;
      
      iter2 = g_slist_next(iter2);
    }
  g_slist_free(entries);
  
  check(got_it, "Did not get 'testing' in listing of / after setting keys such as /testing/foo/bar/woo");
  
  /* Check that "foo" comes back in a list of /testing */
  entries = gconf_engine_all_dirs(conf, "/testing", &error);

  check (error == NULL, "Error getting list of dirs in /testing: %s",
         error ? error->message : "");

  got_it = FALSE;
  
  iter2 = entries;
  while (iter2 != NULL)
    {
      if (strcmp(iter2->data, "/testing/foo") == 0)
        got_it = TRUE;
      
      iter2 = g_slist_next(iter2);
    }
  g_slist_free(entries);
  
  check(got_it, "Did not get 'foo' in listing of /testing after setting keys such as /testing/foo/bar/woo");
  
  /* Check that /testing/foo/ subdirs come back in a listing of /testing/foo */
  
  entries = gconf_engine_all_dirs(conf, "/testing/foo", &error);

  check (error == NULL, "Error getting list of dirs in /testing/foo: %s",
         error ? error->message : "");

  iter2 = entries;
  while (iter2 != NULL)
    {
      i = 0;
      iter = keys_in_foo;
      while (*iter)
        {
          gchar *full = gconf_concat_dir_and_key ("/testing/foo", *iter);
          if (strcmp(iter2->data, full) == 0)
            found[i] = TRUE;

          g_free (full);
          
          ++i;
          ++iter;
        }

      iter2 = g_slist_next(iter2);
    }
  g_slist_free(entries);

  i = 0;
  iter = keys_in_foo;
  while (*iter)
    {
      check(found[i], "Did not get key %s back in the /testing/foo listing",
            *iter);
      
      ++i;
      ++iter;
    }

  iter = keys_in_foo;

  while (*iter)
    {
      gchar* full_key;
      gchar* tmp;

      tmp = g_strconcat(*iter, "/woo", NULL);

      full_key = gconf_concat_dir_and_key("/testing/foo", tmp);
      g_free(tmp);
      
      gconf_engine_unset(conf, full_key, &error);

      check (error == NULL, "Error unsetting key %s: %s",
             full_key, error ? error->message : "");
      
      g_free(full_key);

      ++iter;
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
      g_assert(err != NULL);
      fprintf(stderr, "Failed to init GConf: %s\n", err->message);
      fflush(stderr);
      g_error_free(err);
      err = NULL;
      return 1;
    }
  
  conf = gconf_engine_get_default();

  check(conf != NULL, "create the default conf engine");
  
  check_dir_listing(conf);
  
  gconf_engine_unref(conf);

  printf("\n\n");
  
  return 0;
}
