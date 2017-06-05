/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

#include <config.h>
#include <string.h>
#include <stdio.h>
#include <locale.h>

#include "gnome-i18nP.h"

static GHashTable *alias_table = NULL;

/*read an alias file for the locales*/
static void
read_aliases (char *file)
{
  FILE *fp;
  char buf[256];
  if (!alias_table)
    alias_table = g_hash_table_new (g_str_hash, g_str_equal);
  fp = fopen (file,"r");
  if (!fp)
    return;
  while (fgets (buf,256,fp))
    {
      char *p;
      g_strstrip(buf);
      if (buf[0]=='#' || buf[0]=='\0')
        continue;
      p = strtok (buf,"\t ");
      if (!p)
	continue;
      p = strtok (NULL,"\t ");
      if(!p)
	continue;
      if (!g_hash_table_lookup (alias_table, buf))
	g_hash_table_insert (alias_table, g_strdup(buf), g_strdup(p));
    }
  fclose (fp);
}

/*return the un-aliased language as a newly allocated string*/
static char *
unalias_lang (char *lang)
{
  char *p;
  int i;
  if (!alias_table)
    {
      read_aliases ("/usr/share/locale/locale.alias");
      read_aliases ("/usr/local/share/locale/locale.alias");
      read_aliases ("/usr/lib/X11/locale/locale.alias");
      read_aliases ("/usr/openwin/lib/locale/locale.alias");
    }
  i = 0;
  while ((p=g_hash_table_lookup(alias_table,lang)) && strcmp(p, lang))
    {
      lang = p;
      if (i++ == 30)
        {
          static gboolean said_before = FALSE;
	  if (!said_before)
            g_warning (_("Too many alias levels for a locale, "
			 "may indicate a loop"));
	  said_before = TRUE;
	  return lang;
	}
    }
  return lang;
}

/* Mask for components of locale spec. The ordering here is from
 * least significant to most significant
 */
enum
{
  COMPONENT_CODESET =   1 << 0,
  COMPONENT_TERRITORY = 1 << 1,
  COMPONENT_MODIFIER =  1 << 2
};

/* Break an X/Open style locale specification into components
 */
static guint
explode_locale (const gchar *locale,
		gchar **language, 
		gchar **territory, 
		gchar **codeset, 
		gchar **modifier)
{
  const gchar *uscore_pos;
  const gchar *at_pos;
  const gchar *dot_pos;

  guint mask = 0;

  uscore_pos = strchr (locale, '_');
  dot_pos = strchr (uscore_pos ? uscore_pos : locale, '.');
  at_pos = strchr (dot_pos ? dot_pos : (uscore_pos ? uscore_pos : locale), '@');

  if (at_pos)
    {
      mask |= COMPONENT_MODIFIER;
      *modifier = g_strdup (at_pos);
    }
  else
    at_pos = locale + strlen (locale);

  if (dot_pos)
    {
      mask |= COMPONENT_CODESET;
      *codeset = g_new (gchar, 1 + at_pos - dot_pos);
      strncpy (*codeset, dot_pos, at_pos - dot_pos);
      (*codeset)[at_pos - dot_pos] = '\0';
    }
  else
    dot_pos = at_pos;

  if (uscore_pos)
    {
      mask |= COMPONENT_TERRITORY;
      *territory = g_new (gchar, 1 + dot_pos - uscore_pos);
      strncpy (*territory, uscore_pos, dot_pos - uscore_pos);
      (*territory)[dot_pos - uscore_pos] = '\0';
    }
  else
    uscore_pos = dot_pos;

  *language = g_new (gchar, 1 + uscore_pos - locale);
  strncpy (*language, locale, uscore_pos - locale);
  (*language)[uscore_pos - locale] = '\0';

  return mask;
}

/*
 * Compute all interesting variants for a given locale name -
 * by stripping off different components of the value.
 *
 * For simplicity, we assume that the locale is in
 * X/Open format: language[_territory][.codeset][@modifier]
 *
 * TODO: Extend this to handle the CEN format (see the GNUlibc docs)
 *       as well. We could just copy the code from glibc wholesale
 *       but it is big, ugly, and complicated, so I'm reluctant
 *       to do so when this should handle 99% of the time...
 */
static GList *
compute_locale_variants (const gchar *locale)
{
  GList *retval = NULL;

  gchar *language;
  gchar *territory;
  gchar *codeset;
  gchar *modifier;

  guint mask;
  guint i;

  g_return_val_if_fail (locale != NULL, NULL);

  mask = explode_locale (locale, &language, &territory, &codeset, &modifier);

  /* Iterate through all possible combinations, from least attractive
   * to most attractive.
   */
  for (i=0; i<=mask; i++)
    if ((i & ~mask) == 0)
      {
	gchar *val = g_strconcat(language,
				 (i & COMPONENT_TERRITORY) ? territory : "",
				 (i & COMPONENT_CODESET) ? codeset : "",
				 (i & COMPONENT_MODIFIER) ? modifier : "",
				 NULL);
	retval = g_list_prepend (retval, val);
      }

  g_free (language);
  if (mask & COMPONENT_CODESET)
    g_free (codeset);
  if (mask & COMPONENT_TERRITORY)
    g_free (territory);
  if (mask & COMPONENT_MODIFIER)
    g_free (modifier);

  return retval;
}

/* The following is (partly) taken from the gettext package.
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.  */

static const gchar *
guess_category_value (const gchar *categoryname)
{
  const gchar *retval;

  /* The highest priority value is the `LANGUAGE' environment
     variable.  This is a GNU extension.  */
  retval = g_getenv ("LANGUAGE");
  if (retval != NULL && retval[0] != '\0')
    return retval;

  /* `LANGUAGE' is not set.  So we have to proceed with the POSIX
     methods of looking to `LC_ALL', `LC_xxx', and `LANG'.  On some
     systems this can be done by the `setlocale' function itself.  */

  /* Setting of LC_ALL overwrites all other.  */
  retval = g_getenv ("LC_ALL");  
  if (retval != NULL && retval[0] != '\0')
    return retval;

  /* Next comes the name of the desired category.  */
  retval = g_getenv (categoryname);
  if (retval != NULL && retval[0] != '\0')
    return retval;

  /* Last possibility is the LANG environment variable.  */
  retval = g_getenv ("LANG");
  if (retval != NULL && retval[0] != '\0')
    return retval;

  return NULL;
}


static GHashTable *category_table= NULL;


/**
 * gnome_i18n_get_language_list:
 * @category_name: Name of category to look up, e.g. %"LC_MESSAGES".
 * 
 * This computes a list of language strings that the user wants.  It searches in
 * the standard environment variables to find the list, which is sorted in order
 * from most desirable to least desirable.  The `C' locale is appended to the
 * list if it does not already appear (other routines depend on this
 * behaviour). If @category_name is %NULL, then %LC_ALL is assumed.
 * 
 * Return value: the list of languages, this list should not be freed as it is
 * owned by gnome-i18n.
 **/
const GList *
gnome_i18n_get_language_list (const gchar *category_name)
{
  GList *list;

  if (!category_name)
    category_name= "LC_ALL";

  if (category_table)
    {
      list= g_hash_table_lookup (category_table, (const gpointer) category_name);
    }
  else
    {
      category_table= g_hash_table_new (g_str_hash, g_str_equal);
      list= NULL;
    }

  if (!list)
    {
      gint c_locale_defined= FALSE;
  
      const gchar *category_value;
      gchar *category_memory, *orig_category_memory;

      category_value = guess_category_value (category_name);
      if (! category_value)
	category_value = "C";
      orig_category_memory = category_memory =
	g_malloc (strlen (category_value)+1);
      
      while (category_value[0] != '\0')
	{
	  while (category_value[0] != '\0' && category_value[0] == ':')
	    ++category_value;
	  
	  if (category_value[0] != '\0')
	    {
	      char *cp= category_memory;
	      
	      while (category_value[0] != '\0' && category_value[0] != ':')
		*category_memory++= *category_value++;
	      
	      category_memory[0]= '\0'; 
	      category_memory++;
	      
	      cp = unalias_lang(cp);
	      
	      if (strcmp (cp, "C") == 0)
		c_locale_defined= TRUE;
	      
	      list= g_list_concat (list, compute_locale_variants (cp));
	    }
	}

      g_free (orig_category_memory);
      
      if (!c_locale_defined)
	list= g_list_append (list, "C");

      g_hash_table_insert (category_table, (gpointer) category_name, list);
    }
  
  return list;
}

static int numeric_c_locale_depth = 0;
static char *numeric_locale = NULL;

/**
 * gnome_i18n_push_c_numeric_locale:
 *
 * Description:  Saves the current LC_NUMERIC locale and sets it to "C"
 * This way you can safely read write floating point numbers all in the
 * same format.  You should make sure that code between
 * gnome_i18n_push_c_numeric_locale() and gnome_i18n_pop_c_numeric_locale()
 * doesn't do any setlocale calls or locale may end up in a strange setting.
 * Also make sure to always pop the c numeric locale after you've pushed it.
 * The calls can be nested.
 **/
void
gnome_i18n_push_c_numeric_locale (void)
{
	if (numeric_c_locale_depth == 0) {
		g_free (numeric_locale);
		numeric_locale = g_strdup (setlocale (LC_NUMERIC, NULL));
		setlocale (LC_NUMERIC, "C");
	}
	numeric_c_locale_depth ++;
}

/**
 * gnome_i18n_pop_c_numeric_locale:
 *
 * Description:  Restores the LC_NUMERIC locale to what it was 
 * before the matching gnome_i18n_push_c_numeric_locale(). If these calls
 * were nested, then this is a no-op until we get to the most outermost
 * layer. Code in between these should not do any setlocale calls
 * to change the LC_NUMERIC locale or things may come out very strange.
 **/
void
gnome_i18n_pop_c_numeric_locale (void)
{
	if (numeric_c_locale_depth == 0) {
		return;
	}

	numeric_c_locale_depth --;

	if (numeric_c_locale_depth == 0) {
		setlocale (LC_NUMERIC, numeric_locale);
		g_free (numeric_locale);
		numeric_locale = NULL;
	}
}
