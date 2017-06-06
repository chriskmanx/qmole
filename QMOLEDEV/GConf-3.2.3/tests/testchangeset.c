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
#include <gconf/gconf-changeset.h>
#include <stdio.h>
#include <locale.h>
#include <unistd.h>
#include <math.h>
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
  GConfChangeSet* cs;

  cs = gconf_change_set_new();
  
  keyp = keys;

  while (*keyp)
    {
      gconf_change_set_unset(cs, *keyp);

      ++keyp;
    }

  
  gconf_engine_commit_change_set(conf, cs, TRUE, &err);
  
  if (err != NULL)
    {
      fprintf(stderr, "unset commit failed: %s\n", err->message);
      g_error_free(err);
      err = NULL;
      exit(1);
    }

  gconf_change_set_unref(cs);
  
  keyp = keys;
  while (*keyp)
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
      
      ++keyp;
    }
}

static void
check_string_storage(GConfEngine* conf)
{
  GError* err = NULL;
  const gchar** keyp = NULL;
  const gchar** valp = NULL;
  GConfChangeSet* cs;

  cs = gconf_change_set_new();
  
  keyp = keys;
  valp = some_strings;
  
  while (*keyp && *valp)
    {
      gconf_change_set_set_string(cs, *keyp, *valp);

      ++valp;
      ++keyp;
    }

  gconf_engine_commit_change_set(conf, cs, TRUE, &err);
  
  if (err != NULL)
    {
      fprintf(stderr, "set commit failed: %s\n", err->message);
      g_error_free(err);
      err = NULL;
      exit(1);
    }

  gconf_change_set_unref(cs);
  
  keyp = keys;
  valp = some_strings;
  
  while (*keyp && *valp)
    {
      gchar* gotten;
      
      gotten = gconf_engine_get_string(conf, *keyp, &err);
      
      if (err != NULL)
        {
          check(gotten == NULL, "string was returned though there was an error");
          fprintf(stderr, "Failed to get key `%s': %s\n",
                  *keyp, err->message);
          g_error_free(err);
          err = NULL;
          exit(1);
        }
      else
        {
          check (strcmp(gotten, *valp) == 0, "string set/get pair: `%s' set, `%s' got",
                 *valp, gotten);
          
          g_free(gotten);
        }
      
      ++valp;
      ++keyp;
    }

  check_unset(conf);
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

  printf("\nChecking string storage via GConfChangeSet:");
  
  check_string_storage(conf);
  
  gconf_engine_unref(conf);

  printf("\n\n");
  
  return 0;
}
