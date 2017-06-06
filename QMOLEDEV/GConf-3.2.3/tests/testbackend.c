/* GConf
 * Copyright (C) 2002 Red Hat Inc.
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

#include <gconf/gconf-backend.h>
#include <gconf/gconf-internals.h>
#include <gconf/gconf-locale.h>
#include <gconf/gconf.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <locale.h>
#include <math.h>

static const char **locales = NULL;
static gboolean sync_enabled = FALSE;

typedef void (* ForeachEntryFunc) (GConfEntry *entry,
                                   int         depth,
                                   void       *data);

static void
exit_if_error (GError *error)
{
  if (error != NULL)
    {
      g_printerr ("Error: %s\n", error->message);
      g_error_free (error);
      exit (1);
    }
}

static void
foreach_recursive (GConfSource     *source,
                   const char      *dir_key,
                   int              depth,
                   ForeachEntryFunc func,
                   void            *data)
{
  GSList *entries;
  GSList *subdirs;
  GSList *tmp;
  GError *error;  

  error = NULL;
  entries = (* source->backend->vtable.all_entries) (source, dir_key,
						     locales, &error);
  exit_if_error (error);

  tmp = entries;
  while (tmp != NULL)
    {
      GConfEntry *entry;
      const char *key;
      
      entry = tmp->data;

      key = gconf_entry_get_key (entry);

      if (strchr (key, '/') != NULL)
        {
          g_printerr ("Backend returned a key containing / \"%s\" from all_entries\n",
                      key);
          exit (1);
        }

      (* func) (entry, depth, data);
      

      gconf_entry_free (tmp->data);
      tmp = tmp->next;
    }

  g_slist_free (entries);

  error = NULL;
  subdirs = (* source->backend->vtable.all_subdirs) (source, dir_key,
						     &error);
  exit_if_error (error);

  tmp = subdirs;
  while (tmp != NULL)
    {
      char *fullname;

      fullname = gconf_concat_dir_and_key (dir_key, tmp->data);

      foreach_recursive (source, fullname, depth + 1, func, data);
      
      g_free (fullname);

      g_free (tmp->data);
      tmp = tmp->next;
    }
  
  g_slist_free (subdirs);  
}

static const char*
null_safe (const char *s)
{
  return s ? s : "<null>";
}

static int
null_safe_strcmp (const char* lhs, const char* rhs)
{
  if (lhs == NULL && rhs == NULL)
    return 0;
  else if (lhs == NULL)
    return 1;
  else if (rhs == NULL)
    return -1;
  else
    return strcmp (lhs, rhs);
}

static void
sync_and_clear (GConfSource *source)
{
  GError *err;

  if (!sync_enabled)
    return;
  
  err = NULL;
  if (!(* source->backend->vtable.sync_all) (source, &err))
    {
      g_printerr ("Failed to sync: %s\n", err->message);
      g_error_free (err);
      
      exit (1);
    }

  (* source->backend->vtable.clear_cache) (source);
}

static void
check (gboolean condition, const char* fmt, ...)
{
  va_list args;
  char* description;
  
  va_start (args, fmt);
  description = g_strdup_vprintf (fmt, args);
  va_end (args);
  
  if (condition)
    {
      /* fputc ('.', stdout); */
      fflush (stdout);
    }
  else
    {
      g_printerr ("\n*** FAILED: %s\n", description);
      exit (1);
    }
  
  g_free (description);
}

static const char*
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

static const char*
some_strings[] = {
  "dkadfhg;ifb;klndfl;kghpaodigjhrekjt45u62%&@#&@#kl6$%76k@$%&L jk245L:Yj45&@$&KL #$^UY $5",
  "",
  "sdkjfkljg",
  "a",
  "&",
  "UTF-8: German (Deutsch SÃ¼d) GrÃ¼Ã Gott Greek (ÎÎ»Î»Î·Î½Î¹ÎºÎ¬) ÎÎµÎ¹Î¬ ÏÎ±Ï Hebrew(×©×××) Hebrew punctuation(\xd6\xbf×©\xd6\xbb\xd6\xbc\xd6\xbb\xd6\xbf×\xd6\xbc×\xd6\xbc\xd6\xbb\xd6\xbb\xd6\xbf×\xd6\xbc\xd6\xbb\xd6\xbf) Japanese (æ¥æ¬èª) Thai (à¸ªà¸§à¸±à¸ªà¸à¸µà¸à¸£à¸±à¸) Thai wrong spelling (à¸à¸³à¸à¹à¸­à¹à¸à¸à¸·à¹à¸ªà¸°à¸à¸à¸à¸´à¸ à¸à¸±à¸±à¹à¸±à¸±à¹à¸à¹à¸à¸°)\n",
  "#$&&^(%^^#$&&*(%^&#!$%$%&^&(%&>>>>>>>>>>>>>!>>>.....<<<<<<<<<<<<<<,,,,,,,&&&&&&",
  "sjdflkjg;kljklj",
  "hello this is a string with spaces and \t\t\t\ttabs",
  "hello this\nstring\nhas\nnewlines\n   \t\t\t\t\t\ttabs and spaces  \n",
  "<?xml version=\"1.0\"?>"
  "<gmr:Workbook xmlns:gmr=\"http://www.gnome.org/gnumeric/\">"
  "<gmr:Style HAlign=\"1\" VAlign=\"1\" Fit=\"0\" Orient=\"1\" Shade=\"0\" Format=\"#,##0_);[red](#,##0)\">"
  "<gmr:Font Unit=\"14\" NAME=\"FontDef1\">-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*</gmr:Font>"
  "</gmr:Style>"
  "<gmr:Geometry Width=\"610\" Height=\"418\"/>"
  "<gmr:Sheets>"
  "<gmr:Sheet>"
  "<gmr:Name>Sheet 0</gmr:Name>"
  "<gmr:MaxCol>6</gmr:MaxCol>"
  "<gmr:MaxRow>14</gmr:MaxRow>"
  "<gmr:Zoom>1.000000</gmr:Zoom>"
  "<gmr:Cols>"
  "<gmr:ColInfo No=\"0\" Unit=\"97\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:ColInfo No=\"1\" Unit=\"80\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:ColInfo No=\"2\" Unit=\"80\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:ColInfo No=\"3\" Unit=\"80\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:ColInfo No=\"6\" Unit=\"80\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "</gmr:Cols>"
  "<gmr:Rows>"
  "<gmr:RowInfo No=\"0\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"1\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"2\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"3\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"4\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"5\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"6\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"7\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"8\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"9\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"10\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"11\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"12\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"13\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "<gmr:RowInfo No=\"14\" Unit=\"20\" MarginA=\"1\" MarginB=\"1\" HardSize=\"0\"/>"
  "</gmr:Rows>"
  "<gmr:Objects>"
  "<gmr:Ellipse Pattern=\"0\" Width=\"1\" Color=\"black\">"
  "<gmr:Points>(258.000000 320.000000)(356.000000 356.000000)</gmr:Points>"
  "</gmr:Ellipse>"
  "<gmr:Arrow Width=\"1\" Color=\"black\">"
  "<gmr:Points>(500.000000 131.000000)(332.000000 320.000000)</gmr:Points>"
  "</gmr:Arrow>"
  "</gmr:Objects>"
  "<gmr:Cells>"
  "<gmr:Cell Col=\"3\" Row=\"1\">"
  "<gmr:Style HAlign=\"1\" VAlign=\"1\" Fit=\"0\" Orient=\"1\" Shade=\"0\" Format=\"#,##0_);[red](#,##0)\">"
  "<gmr:Font Unit=\"14\" NAME=\"FontDef2\">-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*</gmr:Font>"
  "</gmr:Style>"
  "<gmr:Content>500</gmr:Content>",

  NULL
};

static gint ints[] = { -1, -2, -3, 0, 1, 2, 3, 4000, 0xfffff, -0xfffff, G_MININT, G_MAXINT, 0, 0, 57, 83, 95 };
static const guint n_ints = sizeof (ints)/sizeof (ints[0]);

static gboolean bools[] = { TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                            FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE };

static const guint n_bools = sizeof (bools)/sizeof (bools[0]);

static gdouble floats[] = {
  0.0, 1.0, 2.0, 3.0, 4.0, -10.0,
  -10.34645764573475637657367346743734878734109870187200000000000009,
  -100.39458694856908, 3.14159, 4.4532464e7, 9.35e-10, 4.5, 6.7, 8.3,  
  -5.1, G_MINFLOAT, G_MAXFLOAT
};

static const guint n_floats = sizeof (floats)/sizeof (floats[0]);

static void
check_unset (GConfSource *source)
{
  GError* err = NULL;
  const char** keyp = NULL;

  keyp = keys;

  while (*keyp)
    {
      (* source->backend->vtable.unset_value) (source,
					       *keyp,
					       NULL,
					       &err);

      if (err != NULL)
        {
          g_printerr ("unset of `%s' failed: %s\n", *keyp, err->message);
          g_error_free (err);
          err = NULL;
        }
      else
        {
          GConfValue* val;
          char* valstr;

          sync_and_clear (source);
          
          val = (* source->backend->vtable.query_value) (source,
							 *keyp,
							 locales,
							 NULL,
							 &err);

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

static gboolean
set_value (GConfSource *source,
           const char  *key,
           GConfValue  *value,
           GError     **err)
{
  GError *tmp_err;

  tmp_err = NULL;
  (* source->backend->vtable.set_value) (source, key, value, &tmp_err);
  if (tmp_err)
    g_propagate_error (err, tmp_err);

  return tmp_err == NULL;
}

static gboolean
set_string (GConfSource *source,
            const char  *key,
            const char  *str,
            GError     **err)
{
  GConfValue *value;
  gboolean ret;
  
  value = gconf_value_new (GCONF_VALUE_STRING);
  gconf_value_set_string (value, str);
  ret = set_value (source, key, value, err);
  gconf_value_free (value);
  return ret;
}

static gboolean
set_int (GConfSource *source,
         const char  *key,
         int          v,
         GError     **err)
{
  GConfValue *value;
  gboolean ret;
  
  value = gconf_value_new (GCONF_VALUE_INT);
  gconf_value_set_int (value, v);
  ret = set_value (source, key, value, err);
  gconf_value_free (value);
  return ret;
}

static gboolean
set_bool (GConfSource *source,
          const char  *key,
          gboolean     v,
          GError     **err)
{
  GConfValue *value;
  gboolean ret;
  
  value = gconf_value_new (GCONF_VALUE_BOOL);
  gconf_value_set_bool (value, v);
  ret = set_value (source, key, value, err);
  gconf_value_free (value);
  return ret;
}

static gboolean
set_float (GConfSource *source,
           const char  *key,
           double       v,
           GError     **err)
{
  GConfValue *value;
  gboolean ret;
  
  value = gconf_value_new (GCONF_VALUE_FLOAT);
  gconf_value_set_float (value, v);
  ret = set_value (source, key, value, err);
  gconf_value_free (value);
  return ret;
}

static gboolean
set_list (GConfSource   *source,
          const char    *key,
          GConfValueType list_type,
          GSList        *list,
          GError       **err)
{
  GConfValue *value_list;
  GError *tmp_err = NULL;
  gboolean ret;
  
  g_return_val_if_fail (source != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (list_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail (list_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail (list_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

  value_list = gconf_value_list_from_primitive_list (list_type, list, &tmp_err);

  if (tmp_err)
    {
      g_propagate_error (err, tmp_err);
      return FALSE;
    }

  ret = set_value (source, key, value_list, err);
  gconf_value_free (value_list);
  return ret;
}

static GConfValue*
get_value (GConfSource *source,
           const char  *key,
           GError     **err)
{
  return (* source->backend->vtable.query_value) (source, key, locales, NULL, err);
}

static char*
get_string (GConfSource *source,
            const char  *key,
            GError     **err)
{
  GConfValue *val;
  char *s;
  
  val = get_value (source, key, err);

  if (val)
    {
      g_assert (val->type == GCONF_VALUE_STRING);
      s = gconf_value_steal_string (val);
      gconf_value_free (val);
    }
  else
    {
      s = NULL;
    }

  return s;
}

static gboolean
get_bool (GConfSource *source,
          const char  *key,
          GError     **err)
{
  GConfValue *val;
  gboolean b;
  
  val = get_value (source, key, err);

  if (val)
    {
      g_assert (val->type == GCONF_VALUE_BOOL);
      b = gconf_value_get_bool (val);
      gconf_value_free (val);
    }
  else
    {
      b = FALSE;
    }

  return b;
}

static int
get_int (GConfSource *source,
          const char  *key,
          GError     **err)
{
  GConfValue *val;
  int i;
  
  val = get_value (source, key, err);

  if (val)
    {
      g_assert (val->type == GCONF_VALUE_INT);
      i = gconf_value_get_int (val);
      gconf_value_free (val);
    }
  else
    {
      i = FALSE;
    }

  return i;
}

static double
get_float (GConfSource *source,
           const char  *key,
           GError     **err)
{
  GConfValue *val;
  double d;
  
  val = get_value (source, key, err);

  if (val)
    {
      g_assert (val->type == GCONF_VALUE_FLOAT);
      d = gconf_value_get_float (val);
      gconf_value_free (val);
    }
  else
    {
      d = FALSE;
    }

  return d;
}

static GSList*
get_list (GConfSource     *source,
          const char      *key,
          GConfValueType   list_type,
          GError         **err)
{
  GConfValue* val;

  g_return_val_if_fail (source != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);
  g_return_val_if_fail (list_type != GCONF_VALUE_INVALID, NULL);
  g_return_val_if_fail (list_type != GCONF_VALUE_LIST, NULL);
  g_return_val_if_fail (list_type != GCONF_VALUE_PAIR, NULL);
  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  val = get_value (source, key, err);

  if (val == NULL)
    return NULL;
  else
    {
      /* This type-checks the value */
      return gconf_value_list_to_primitive_list_destructive (val, list_type, err);
    }
}

static void
check_string_storage (GConfSource *source)
{
  GError* err = NULL;
  const char** keyp = NULL;
  const char** valp = NULL;

  /* Loop over keys, storing all strings at each key then immediately
   * retrieving them
   */
  
  keyp = keys;

  while (*keyp)
    {
      valp = some_strings;
      while (*valp)
        {
          char* gotten;
          
          if (!set_string (source, *keyp, *valp, &err))
            {
              g_printerr ("Failed to set key `%s' to `%s': %s\n",
                          *keyp, *valp, err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_string (source, *keyp, &err);
              
              if (err != NULL)
                {
                  check (gotten == NULL, "string was returned though there was an error");
                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (null_safe_strcmp (gotten, *valp) == 0, "string set/get pair: `%s' set, `%s' got",
                         *valp, gotten);
              
                  g_free (gotten);
                }
            }
          
          ++valp;
        }

      ++keyp;
    }

  /* Now invert the loop and see if that causes problems */
  
  valp = some_strings;
  
  while (*valp)
    {
      keyp = keys;
      while (*keyp)
        {
          char* gotten;
          
          if (!set_string (source, *keyp, *valp, &err))
            {
              g_printerr ("Failed to set key `%s' to `%s': %s\n",
                          *keyp, *valp, err->message);
              g_error_free (err);
              err = NULL;
            }

          sync_and_clear (source);
          
          gotten = get_string (source, *keyp, &err);

          if (err != NULL)
            {
              check (gotten == NULL, "string was returned though there was an error");
              g_printerr ("Failed to get key `%s': %s\n",
                          *keyp, err->message);
              g_error_free (err);
              err = NULL;
            }
          else if (gotten)
            {
              check (null_safe_strcmp (gotten, *valp) == 0, "string set/get pair: `%s' set, `%s' got",
                     *valp, null_safe (gotten));
              
              g_free (gotten);
            }
              
          ++keyp;
        }

      ++valp;
    }


  check_unset (source);
}

void
check_bool_storage (GConfSource *source)
{
  GError* err = NULL;
  const char** keyp = NULL;
  guint i; 
  
  /* Loop over keys, storing all bools at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      while (i < n_bools)
        {
          gboolean gotten;
          
          if (!set_bool (source, *keyp, bools[i], &err))
            {
              g_printerr ("Failed to set key `%s' to `%d': %s\n",
                          *keyp, bools[i], err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_bool (source, *keyp, &err);

              if (err != NULL)
                {
                  check (gotten == FALSE, "TRUE was returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (bools[i] == gotten, "bool set/get pair %s: `%d' set, `%d' got",
                         *keyp, bools[i], gotten);

                }
            }
          
          ++i;
        }
      
      ++keyp;
    }

  /* Now invert the loop and see if that causes problems */

  i = 0;
      
  while (i < n_bools)
    {
      keyp = keys;
      
      while (*keyp)
        {
          gboolean gotten;
          
          if (!set_bool (source, *keyp, bools[i], &err))
            {
              g_printerr ("Failed to set key `%s' to `%d': %s\n",
                          *keyp, bools[i], err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_bool (source, *keyp, &err);
              
              if (err != NULL)
                {
                  check (gotten == FALSE, "TRUE was returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (bools[i] == gotten, "bool set/get pair %s: `%d' set, `%d' got",
                         *keyp, bools[i], gotten);

                }
            }

          ++keyp;
        }

      ++i;
    }
  
  check_unset (source);
}

void
check_float_storage (GConfSource *source)
{
  GError* err = NULL;
  const char** keyp = NULL;
  guint i; 
  const gdouble tolerance = 1e-5;
  
  /* Loop over keys, storing all values at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      while (i < n_floats)
        {
          gdouble gotten;
          
          if (!set_float (source, *keyp, floats[i], &err))
            {
              g_printerr ("Failed to set key `%s' to `%g': %s\n",
                          *keyp, floats[i], err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_float (source, *keyp, &err);

              if (err != NULL)
                {
                  check (gotten == 0.0, "0.0 not returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (fabs (floats[i] - gotten) < tolerance,
                         "float set/get pair: `%g' set, `%g' got, `%g' epsilon",
                         floats[i], gotten, floats[i] - gotten);

                }
            }
          
          ++i;
        }
      
      ++keyp;
    }

  /* Now invert the loop and see if that causes problems */

  i = 0;
  while (i < n_floats)
    {

      keyp = keys;

      while (*keyp)
        {
          gdouble gotten;
          
          if (!set_float (source, *keyp, floats[i], &err))
            {
              g_printerr ("Failed to set key `%s' to `%g': %s\n",
                          *keyp, floats[i], err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_float (source, *keyp, &err);

              if (err != NULL)
                {
                  check (gotten == 0.0, "0.0 not returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (fabs (floats[i] - gotten) < tolerance,
                         "float set/get pair: `%g' set, `%g' got, `%g' epsilon",
                         floats[i], gotten, floats[i] - gotten);

                }
            }
          
      
          ++keyp;
        }

      ++i;
    }
          
  check_unset (source);
}

void
check_int_storage (GConfSource *source)
{
  GError* err = NULL;
  const char** keyp = NULL;
  guint i; 
  
  /* Loop over keys, storing all values at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      while (i < n_ints)
        {
          gint gotten;
          
          if (!set_int (source, *keyp, ints[i], &err))
            {
              g_printerr ("Failed to set key `%s' to `%d': %s\n",
                          *keyp, ints[i], err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_int (source, *keyp, &err);

              if (err != NULL)
                {
                  check (gotten == 0.0, "0.0 not returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (ints[i] == gotten,
                         "int set/get pair %s: `%d' set, `%d' got",
                         *keyp, ints[i], gotten);

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
          
          if (!set_int (source, *keyp, ints[i], &err))
            {
              g_printerr ("Failed to set key `%s' to `%d': %s\n",
                          *keyp, ints[i], err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_int (source, *keyp, &err);

              if (err != NULL)
                {
                  check (gotten == 0, "0 not returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  check (ints[i] == gotten,
                         "int set/get pair %s: `%d' set, `%d' got",
                         *keyp, ints[i], gotten);

                }
            }
          
      
          ++keyp;
        }

      ++i;
    }
          
  check_unset (source);
}

static void
compare_lists (GConfValueType type, GSList* first, GSList* second)
{
  GSList* l1;
  GSList* l2;

  l1 = first;
  l2 = second;
  
  while (l1 != NULL)
    {      
      check (l2 != NULL, "second list too short");
      
      switch (type)
        {
        case GCONF_VALUE_INT:
          check (GPOINTER_TO_INT (l1->data) == GPOINTER_TO_INT (l2->data),
                 "integer values %d and %d are not equal",
                 GPOINTER_TO_INT (l1->data), GPOINTER_TO_INT (l2->data));
          break;
          
        case GCONF_VALUE_BOOL:
          check (GPOINTER_TO_INT (l1->data) == GPOINTER_TO_INT (l2->data),
                 "boolean values %d and %d are not equal",
                 GPOINTER_TO_INT (l1->data), GPOINTER_TO_INT (l2->data));
          break;
          
        case GCONF_VALUE_FLOAT:
          {
            gdouble d1 = *((gdouble*)l1->data);
            gdouble d2 = *((gdouble*)l2->data);
            check (fabs (d2 - d1) < 1e-5,
                   "float values %g and %g are not equal (epsilon %g)",
                   d1, d2, d2 - d1);
          }
          break;
          
        case GCONF_VALUE_STRING:
          check (null_safe_strcmp (l1->data, l2->data) == 0, 
                 "string values `%s' and `%s' are not equal",
                 l1->data, l2->data);
          break;
          
        default:
          g_assert_not_reached ();
          break;
        }
      
      l1 = g_slist_next (l1);
      l2 = g_slist_next (l2);
    }

  check (l2 == NULL, "second list too long with list type %s",
         gconf_value_type_to_string (type));
}

static void
free_list (GConfValueType type, GSList* list)
{
  GSList* tmp = list;

  while (tmp != NULL)
    {
      switch (type)
        {
        case GCONF_VALUE_INT:
        case GCONF_VALUE_BOOL:
          break;

        case GCONF_VALUE_FLOAT:
        case GCONF_VALUE_STRING:
          g_free (tmp->data);
          break;
          
        default:
          g_assert_not_reached ();
          break;
        }

      tmp = g_slist_next (tmp);
    }

  g_slist_free (list);
}

static GSList*
list_of_ints (void)
{
  GSList* retval = NULL;
  guint i = 0;
  while (i < n_ints)
    {      
      retval = g_slist_prepend (retval, GINT_TO_POINTER (ints[i]));
      
      ++i;
    }
  return retval;
}

static GSList*
list_of_strings (void)
{
  GSList* retval = NULL;
  const char** stringp = some_strings;
  while (*stringp)
    {     
      retval = g_slist_prepend (retval, g_strdup (*stringp));
      
      ++stringp;
    }
  return retval;
}

static GSList*
list_of_bools (void)
{
  GSList* retval = NULL;
  guint i = 0;
  while (i < n_bools)
    {      
      retval = g_slist_prepend (retval, GINT_TO_POINTER (bools[i]));
      
      ++i;
    }
  return retval;
}

static GSList*
list_of_floats (void)
{
  GSList* retval = NULL;
  guint i = 0;
  while (i < n_floats)
    {      
      retval = g_slist_prepend (retval,
                                g_memdup (&floats[i], sizeof (floats[i])));
      
      ++i;
    }
  return retval;
}


static void
check_list_storage (GConfSource *source)
{
  GError* err = NULL;
  const char** keyp = NULL;
  guint i;
  GConfValueType list_types[] = { GCONF_VALUE_INT, GCONF_VALUE_INT,
                                  GCONF_VALUE_STRING, GCONF_VALUE_STRING,
                                  GCONF_VALUE_FLOAT, GCONF_VALUE_FLOAT,
                                  GCONF_VALUE_BOOL, GCONF_VALUE_BOOL };
  GSList* lists[] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
  const guint n_lists = sizeof (lists)/sizeof (lists[0]);

  /* List of integers */
  lists[0] = list_of_ints ();

  /* empty list of integers */
  lists[1] = NULL;

  /* lists of string */
  lists[2] = list_of_strings ();
  lists[3] = NULL;

  /* of float */
  lists[4] = list_of_floats ();
  lists[5] = NULL;

  /* of bool */
  lists[6] = list_of_bools ();
  lists[7] = NULL;
  
  /* Loop over keys, storing all values at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      
      while (i < n_lists)
        {
          GSList* gotten = NULL;
          
          if (!set_list (source, *keyp, list_types[i], lists[i], &err))
            {
              g_printerr ("Failed to set key `%s' to list: %s\n",
                          *keyp, err->message);
              g_error_free (err);
              err = NULL;
            }
          else
            {
              sync_and_clear (source);
              
              gotten = get_list (source, *keyp, list_types[i], &err);

              if (err != NULL)
                {
                  check (gotten == NULL, "NULL not returned though there was an error");

                  g_printerr ("Failed to get key `%s': %s\n",
                              *keyp, err->message);
                  g_error_free (err);
                  err = NULL;
                }
              else
                {
                  compare_lists (list_types[i], lists[i], gotten);
                }
            }

          ++i;
        }
      
      ++keyp;
    }

  i = 0;
  while (i < n_lists)
    {
      free_list (list_types[i], lists[i]);
      ++i;
    }

  check_unset (source);
}

typedef struct
{
  int entry_count;
} Stats;

static void
print_entry (GConfEntry *entry,
             int         depth,
             void       *data)
{
  GConfValue *val;
  char *str;
  Stats *stats;

  stats = data;
  
  stats->entry_count += 1;

  return; /* don't actually print all that junk */
  
  while (depth > 0)
    {
      fputc (' ', stdout);
      fputc (' ', stdout);
      --depth;
    }

  val = gconf_entry_get_value (entry);
  if (val)
    str = gconf_value_to_string (val);
  else
    str = g_strdup ("<unset>");
  
  g_print ("%s = %s\n", gconf_entry_get_key (entry), str);

  g_free (str);
}

static void
run_all_checks (const char *address)
{
  GConfSource *source;
  GError *error;
  Stats stats;
    
  error = NULL;
  source = gconf_resolve_address (address, &error);
  if (error != NULL)
    {
      g_printerr ("Could not resolve address: %s\n", error->message);
      g_error_free (error);
      exit (1);
    }

  g_assert (source != NULL);
  
  stats.entry_count = 0;
  foreach_recursive (source, "/", 0, print_entry, &stats);

  g_print ("%d entries seen\n", stats.entry_count);

  g_print ("\nChecking list storage:");
  
  check_list_storage (source);

  /* FIXME we aren't checking storage of pairs */
  
  g_print ("\nChecking integer storage:");
  
  check_int_storage (source);

  g_print ("\nChecking float storage:");
  
  check_float_storage (source);

  g_print ("\nChecking string storage:");
  
  check_string_storage (source);

  g_print ("\nChecking bool storage:");
  
  check_bool_storage (source);

  sync_and_clear (source);
  
  gconf_source_free (source);

  g_print ("\n\n");
}

int
main (int argc, char **argv)
{ 
  if (argc != 2)
    {
      g_printerr ("Must specify a config source address on the command line\n");
      return 1;
    }

  setlocale (LC_ALL, "");

  locales = (const char**) gconf_split_locale (gconf_current_locale ());

  run_all_checks (argv[1]);
  sync_enabled = TRUE;
  run_all_checks (argv[1]);
  
  return 0;
}
