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
some_strings[] = {
  "",
  "dkadfhg;ifb;klndfl;kghpaodigjhrekjt45u62%&@#&@#kl6$%76k@$%&L jk245L:Yj45&@$&KL #$^UY $5",
  "sdkjfkljg",
  "a",
  "&",
  "UTF-8: German (Deutsch Süd) Grüß Gott Greek (Ελληνικά) Γειά σας Hebrew(שלום) Hebrew punctuation(\xd6\xbfש\xd6\xbb\xd6\xbc\xd6\xbb\xd6\xbfל\xd6\xbcו\xd6\xbc\xd6\xbb\xd6\xbb\xd6\xbfם\xd6\xbc\xd6\xbb\xd6\xbf) Japanese (日本語) Thai (สวัสดีครับ) Thai wrong spelling (คำต่อไปนื่สะกดผิด พัั้ัั่งโกะ)\n",
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
static const guint n_ints = sizeof(ints)/sizeof(ints[0]);

static gboolean bools[] = { TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                       FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE };

static const guint n_bools = sizeof(bools)/sizeof(bools[0]);

static gdouble floats[] = { 0.0, 1.0, 2.0, 3.0, 4.0, -10.0, -10.34645764573475637657367346743734878734109870187200000000000009, -100.39458694856908, 3.14159, 4.4532464e7, 9.35e-10, 4.5, 6.7, 8.3, -5.1, G_MINFLOAT, -G_MAXFLOAT, G_MAXFLOAT }; /* don't use max/min double, we don't guarantee that we can store huge numbers */

static const guint n_floats = sizeof(floats)/sizeof(floats[0]);

static void
check_unset(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;

  keyp = keys;

  while (*keyp)
    {
      gconf_engine_associate_schema (conf, *keyp, "/test-bogus-schema", &err);

      if (err != NULL)
        {
          fprintf(stderr, "unset of `%s' failed: %s\n", *keyp, err->message);
          g_error_free(err);
          err = NULL;
          exit (1);
        }
      
      gconf_engine_unset(conf, *keyp, &err);

      if (err != NULL)
        {
          fprintf(stderr, "unset of `%s' failed: %s\n", *keyp, err->message);
          g_error_free(err);
          err = NULL;
          exit (1);
        }
      else
        {
          GConfValue* val;
          gchar* valstr;
          
          val = gconf_engine_get (conf, *keyp, &err);


          if (val)
            valstr = gconf_value_to_string(val);
          else
            valstr = g_strdup("(none)");
          
          check(val == NULL, "unsetting a previously-set value `%s' the value `%s' existed", *keyp, valstr);

          g_free(valstr);
        }
      
      ++keyp;
    }
}

static void
check_string_storage(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;
  const gchar** valp = NULL;

  /* Loop over keys, storing all strings at each key then immediately
     retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      valp = some_strings;
      while (*valp)
        {
          gchar* gotten;
          
          if (!gconf_engine_set_string(conf, *keyp, *valp, &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%s': %s\n",
                      *keyp, *valp, err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_string(conf, *keyp, &err);
              
              if (err != NULL)
                {
                  check(gotten == NULL, "string was returned though there was an error");
                  fprintf(stderr, "Failed to get key `%s': %s\n",
                          *keyp, err->message);
                  g_error_free(err);
                  err = NULL;
                }
              else
                {
                  check (strcmp(gotten, *valp) == 0, "string set/get pair: `%s' set, `%s' got",
                         *valp, gotten);
              
                  g_free(gotten);
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
          gchar* gotten;
          
          if (!gconf_engine_set_string(conf, *keyp, *valp, &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%s': %s\n",
                      *keyp, *valp, err->message);
              g_error_free(err);
              err = NULL;
            }

          gotten = gconf_engine_get_string(conf, *keyp, &err);

          if (err != NULL)
            {
              check(gotten == NULL, "string was returned though there was an error");
              fprintf(stderr, "Failed to get key `%s': %s\n",
                      *keyp, err->message);
              g_error_free(err);
              err = NULL;
            }
          else if (gotten)
            {
              check (strcmp(gotten, *valp) == 0, "string set/get pair: `%s' set, `%s' got",
                     *valp, gotten);
              
              g_free(gotten);
            }
              
          ++keyp;
        }

      ++valp;
    }


  check_unset(conf);
}

void
check_bool_storage(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;
  guint i; 
  
  /* Loop over keys, storing all bools at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      while (i < n_bools)
        {
          gboolean gotten;
          
          if (!gconf_engine_set_bool(conf, *keyp, bools[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%d': %s\n",
                      *keyp, bools[i], err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_bool(conf, *keyp, &err);

              if (err != NULL)
                {
                  check(gotten == FALSE, "TRUE was returned though there was an error");

                  fprintf(stderr, "Failed to get key `%s': %s\n",
                          *keyp, err->message);
                  g_error_free(err);
                  err = NULL;
                }
              else
                {
                  check (bools[i] == gotten, "bool set/get pair: `%d' set, `%d' got",
                         bools[i], gotten);

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
          
          if (!gconf_engine_set_bool(conf, *keyp, bools[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%d': %s\n",
                      *keyp, bools[i], err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_bool(conf, *keyp, &err);
              
              if (err != NULL)
                {
                  check(gotten == FALSE, "TRUE was returned though there was an error");

                  fprintf(stderr, "Failed to get key `%s': %s\n",
                          *keyp, err->message);
                  g_error_free(err);
                  err = NULL;
                }
              else
                {
                  check (bools[i] == gotten, "bool set/get pair: `%d' set, `%d' got",
                         bools[i], gotten);

                }
            }

          ++keyp;
        }

      ++i;
    }
  
  check_unset(conf);
}

void
check_float_storage(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;
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
          
          if (!gconf_engine_set_float(conf, *keyp, floats[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%g': %s\n",
                      *keyp, floats[i], err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_float(conf, *keyp, &err);

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
                  check (fabs(floats[i] - gotten) < tolerance,
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
          
          if (!gconf_engine_set_float(conf, *keyp, floats[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to `%g': %s\n",
                      *keyp, floats[i], err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_float(conf, *keyp, &err);

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
                  check (fabs(floats[i] - gotten) < tolerance,
                         "float set/get pair: `%g' set, `%g' got, `%g' epsilon",
                         floats[i], gotten, floats[i] - gotten);

                }
            }
          
      
          ++keyp;
        }

      ++i;
    }
          
  check_unset(conf);
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
  keyp = keys;
  while (*keyp)
    {
          gint gotten;
      i = n_ints-1;
          
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

static void
compare_lists(GConfValueType type, GSList* first, GSList* second)
{
  GSList* l1;
  GSList* l2;

  l1 = first;
  l2 = second;
  
  while (l1 != NULL)
    {      
      check(l2 != NULL, "second list too short");
      
      switch (type)
        {
        case GCONF_VALUE_INT:
          check(GPOINTER_TO_INT(l1->data) == GPOINTER_TO_INT(l2->data),
                "integer values %d and %d are not equal",
                GPOINTER_TO_INT(l1->data), GPOINTER_TO_INT(l2->data));
          break;
          
        case GCONF_VALUE_BOOL:
          check(GPOINTER_TO_INT(l1->data) == GPOINTER_TO_INT(l2->data),
                "boolean values %d and %d are not equal",
                GPOINTER_TO_INT(l1->data), GPOINTER_TO_INT(l2->data));
          break;
          
        case GCONF_VALUE_FLOAT:
          {
            gdouble d1 = *((gdouble*)l1->data);
            gdouble d2 = *((gdouble*)l2->data);
            check(fabs(d2 - d1) < 1e-5,
                  "float values %g and %g are not equal (epsilon %g)",
                  d1, d2, d2 - d1);
          }
          break;
          
        case GCONF_VALUE_STRING:
          check(strcmp(l1->data, l2->data) == 0, 
                "string values `%s' and `%s' are not equal",
                l1->data, l2->data);
          break;
          
        default:
          g_assert_not_reached();
          break;
        }
      
      l1 = g_slist_next(l1);
      l2 = g_slist_next(l2);
    }

  check(l2 == NULL, "second list too long with list type %s",
        gconf_value_type_to_string(type));
}

static void
free_list(GConfValueType type, GSList* list)
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
          g_free(tmp->data);
          break;
          
        default:
          g_assert_not_reached();
          break;
        }

      tmp = g_slist_next(tmp);
    }

  g_slist_free(list);
}

static GSList*
list_of_ints(void)
{
  GSList* retval = NULL;
  guint i = 0;
  while (i < n_ints)
    {      
      retval = g_slist_prepend(retval, GINT_TO_POINTER(ints[i]));
      
      ++i;
    }
  return retval;
}

static GSList*
list_of_strings(void)
{
  GSList* retval = NULL;
  const gchar** stringp = some_strings;
  while (*stringp)
    {     
      retval = g_slist_prepend(retval, g_strdup(*stringp));
      
      ++stringp;
    }
  return retval;
}

static GSList*
list_of_bools(void)
{
  GSList* retval = NULL;
  guint i = 0;
  while (i < n_bools)
    {      
      retval = g_slist_prepend(retval, GINT_TO_POINTER(bools[i]));
      
      ++i;
    }
  return retval;
}

static GSList*
list_of_floats(void)
{
  GSList* retval = NULL;
  guint i = 0;
  while (i < n_floats)
    {      
      retval = g_slist_prepend(retval,
                               g_memdup(&floats[i], sizeof(floats[i])));
      
      ++i;
    }
  return retval;
}


static void
check_list_storage(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;
  guint i;
  GConfValueType list_types[] = { GCONF_VALUE_INT, GCONF_VALUE_INT,
                                  GCONF_VALUE_STRING, GCONF_VALUE_STRING,
                                  GCONF_VALUE_FLOAT, GCONF_VALUE_FLOAT,
                                  GCONF_VALUE_BOOL, GCONF_VALUE_BOOL };
  GSList* lists[] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
  const guint n_lists = sizeof(lists)/sizeof(lists[0]);

  /* List of integers */
  lists[0] = list_of_ints();

  /* empty list of integers */
  lists[1] = NULL;

  /* lists of string */
  lists[2] = list_of_strings();
  lists[3] = NULL;

  /* of float */
  lists[4] = list_of_floats();
  lists[5] = NULL;

  /* of bool */
  lists[6] = list_of_bools();
  lists[7] = NULL;
  
  /* Loop over keys, storing all values at each then retrieving them */
  
  keyp = keys;

  while (*keyp)
    {
      i = 0;
      
      while (i < n_lists)
        {
          GSList* gotten = NULL;
          
          if (!gconf_engine_set_list(conf, *keyp, list_types[i], lists[i], &err))
            {
              fprintf(stderr, "Failed to set key `%s' to list: %s\n",
                      *keyp, err->message);
              g_error_free(err);
              err = NULL;
            }
          else
            {
              gotten = gconf_engine_get_list(conf, *keyp, list_types[i], &err);

              if (err != NULL)
                {
                  check(gotten == NULL, "NULL not returned though there was an error");

                  fprintf(stderr, "Failed to get key `%s': %s\n",
                          *keyp, err->message);
                  g_error_free(err);
                  err = NULL;
                }
              else
                {
                  compare_lists(list_types[i], lists[i], gotten);
                }
            }

          ++i;
        }
      
      ++keyp;
    }

  i = 0;
  while (i < n_lists)
    {
      free_list(list_types[i], lists[i]);
      ++i;
    }

  check_unset(conf);
}

static void
check_utils (void)
{
  int i;
  const char *escape_tests[] = {
    "Hello",
    "@@@@@@@@@@@@",
    "foo\135\224\12\f\n\r@@\128@hello/foo",
    "bar baz woo/",
    "//////////",
    "@128@@129@",
    "/./././",
    "",
    "/",
    "a",
    "@"
  };

  i = 0;
  while (i < G_N_ELEMENTS (escape_tests))
    {
      char *escaped;
      char *unescaped;
      char *whole_key;
      
      escaped = gconf_escape_key (escape_tests[i], -1);

      /* escaped is a key element, not a key */
      whole_key = g_strconcat ("/", escaped, NULL);
      
      check (gconf_valid_key (whole_key, NULL), "Escaped key '%s' is valid (original '%s')",
             escaped, escape_tests[i]);
      g_free (whole_key);
      
      unescaped = gconf_unescape_key (escaped, -1);
      check (strcmp (escape_tests[i], unescaped) == 0,
             "Unescaped key '%s' same as original '%s' (escaped was '%s'\n",
             unescaped, escape_tests[i], escaped);

      g_free (escaped);
      g_free (unescaped);
      
      ++i;
    }  
}

int 
main (int argc, char** argv)
{
  GConfEngine* conf;
  GError* err = NULL;
  const char *locale;
  
  locale = setlocale (LC_ALL, "");

  if (locale == NULL)
    g_printerr ("Failed to set locale, invalid env variables?\n");
  
  g_print ("Locale = %s\n", setlocale (LC_ALL, NULL));
  
  if (!gconf_init(argc, argv, &err))
    {
      g_assert(err != NULL);
      fprintf(stderr, "Failed to init GConf: %s\n", err->message);
      fflush(stderr);
      g_error_free(err);
      err = NULL;
      return 1;
    }

  check_utils ();
  
  conf = gconf_engine_get_default();

  check(conf != NULL, "create the default conf engine");

  printf("\nChecking list storage:");
  
  check_list_storage(conf);
  
  printf("\nChecking integer storage:");
  
  check_int_storage(conf);

  printf("\nChecking float storage:");
  
  check_float_storage(conf);

  printf("\nChecking string storage:");
  
  check_string_storage(conf);

  printf("\nChecking bool storage:");
  
  check_bool_storage(conf);

  gconf_engine_set_bool(conf, "/foo", TRUE, &err);

  gconf_engine_unref(conf);

  printf("\n\n");
  
  return gconf_debug_shutdown ();
}
