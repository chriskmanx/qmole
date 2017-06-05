/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/*  gtksourcelanguagemanager.c
 *
 *  Copyright (C) 2003-2007 - Paolo Maggi <paolo.maggi@polito.it>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include "gtksourceview-i18n.h"
#include "gtksourcelanguage-private.h"
#include "gtksourcelanguage.h"
#include "gtksourceview-utils.h"
#include <gio/gio.h>

/**
 * SECTION:languagemanager
 * @Short_description: Object which provides access to #GtkSourceLanguage<!-- -->s
 * @Title: GtkSourceLanguageManager
 * @See_also: #GtkSourceLanguage
 *
 * #GtkSourceLanguageManager is an object which processes language description
 * files and creates and stores #GtkSourceLanguage objects, and provides API to
 * access them.
 * Use gtk_source_language_manager_get_default() to retrieve the default
 * instance of #GtkSourceLanguageManager, and
 * gtk_source_language_manager_guess_language() to get a #GtkSourceLanguage for
 * given file name and content type.
 */

#define RNG_SCHEMA_FILE		"language2.rng"
#define LANGUAGE_DIR		"language-specs"
#define LANG_FILE_SUFFIX	".lang"

enum {
	PROP_0,
	PROP_SEARCH_PATH,
	PROP_LANGUAGE_IDS
};

struct _GtkSourceLanguageManagerPrivate
{
	GHashTable	*language_ids;

	gchar	       **lang_dirs;
	gchar		*rng_file;

	gchar          **ids; /* Cache the IDs of the available languages */
};

G_DEFINE_TYPE (GtkSourceLanguageManager, gtk_source_language_manager, G_TYPE_OBJECT)


static void
gtk_source_language_manager_set_property (GObject 	*object,
					  guint 	 prop_id,
					  const GValue *value,
					  GParamSpec	*pspec)
{
	GtkSourceLanguageManager *lm;

	lm = GTK_SOURCE_LANGUAGE_MANAGER (object);

	switch (prop_id)
	{
		case PROP_SEARCH_PATH:
			gtk_source_language_manager_set_search_path (lm, g_value_get_boxed (value));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_language_manager_get_property (GObject 	*object,
					  guint 	 prop_id,
					  GValue 	*value,
					  GParamSpec	*pspec)
{
	GtkSourceLanguageManager *lm;

	lm = GTK_SOURCE_LANGUAGE_MANAGER (object);

	switch (prop_id)
	{
		case PROP_SEARCH_PATH:
			g_value_set_boxed (value, gtk_source_language_manager_get_search_path (lm));
			break;

		case PROP_LANGUAGE_IDS:
			g_value_set_boxed (value, gtk_source_language_manager_get_language_ids (lm));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_language_manager_finalize (GObject *object)
{
	GtkSourceLanguageManager *lm;

	lm = GTK_SOURCE_LANGUAGE_MANAGER (object);

	if (lm->priv->language_ids)
		g_hash_table_destroy (lm->priv->language_ids);

	g_strfreev (lm->priv->ids);

	g_strfreev (lm->priv->lang_dirs);
	g_free (lm->priv->rng_file);

	G_OBJECT_CLASS (gtk_source_language_manager_parent_class)->finalize (object);
}

static void
gtk_source_language_manager_class_init (GtkSourceLanguageManagerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize	= gtk_source_language_manager_finalize;

	object_class->set_property = gtk_source_language_manager_set_property;
	object_class->get_property = gtk_source_language_manager_get_property;

	g_object_class_install_property (object_class,
					 PROP_SEARCH_PATH,
					 g_param_spec_boxed ("search-path",
						 	     _("Language specification directories"),
							     _("List of directories where the "
							       "language specification files (.lang) "
							       "are located"),
							     G_TYPE_STRV,
							     G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_LANGUAGE_IDS,
					 g_param_spec_boxed ("language-ids",
						 	     _("Language ids"),
							     _("List of the ids of the available "
							       "languages"),
							     G_TYPE_STRV,
							     G_PARAM_READABLE));

	g_type_class_add_private (object_class, sizeof(GtkSourceLanguageManagerPrivate));
}

static void
gtk_source_language_manager_init (GtkSourceLanguageManager *lm)
{
	lm->priv = G_TYPE_INSTANCE_GET_PRIVATE (lm, GTK_TYPE_SOURCE_LANGUAGE_MANAGER,
						GtkSourceLanguageManagerPrivate);
	lm->priv->language_ids = NULL;
	lm->priv->ids = NULL;
	lm->priv->lang_dirs = NULL;
	lm->priv->rng_file = NULL;
}

/**
 * gtk_source_language_manager_new:
 *
 * Creates a new language manager. If you do not need more than one language
 * manager or a private language manager instance then use
 * gtk_source_language_manager_get_default() instead.
 *
 * Returns: a #GtkSourceLanguageManager.
 */
GtkSourceLanguageManager *
gtk_source_language_manager_new (void)
{
	return g_object_new (GTK_TYPE_SOURCE_LANGUAGE_MANAGER, NULL);
}

/**
 * gtk_source_language_manager_get_default:
 *
 * Returns the default #GtkSourceLanguageManager instance.
 *
 * Returns: a #GtkSourceLanguageManager. Return value is owned
 * by GtkSourceView library and must not be unref'ed.
 */
GtkSourceLanguageManager *
gtk_source_language_manager_get_default (void)
{
	static GtkSourceLanguageManager *instance;

	if (instance == NULL)
	{
		instance = gtk_source_language_manager_new ();
		g_object_add_weak_pointer (G_OBJECT (instance),
					   (gpointer) &instance);
	}

	return instance;
}

static void
notify_search_path (GtkSourceLanguageManager *mgr)
{
	g_object_notify (G_OBJECT (mgr), "search-path");
	g_object_notify (G_OBJECT (mgr), "language-ids");
}

/**
 * gtk_source_language_manager_set_search_path:
 * @lm: a #GtkSourceLanguageManager.
 * @dirs: a %NULL-terminated array of strings or %NULL.
 *
 * Sets the list of directories where the @lm looks for
 * language files.
 * If @dirs is %NULL, the search path is reset to default.
 *
 * <note>
 *   <para>
 *     At the moment this function can be called only before the
 *     language files are loaded for the first time. In practice
 *     to set a custom search path for a #GtkSourceLanguageManager,
 *     you have to call this function right after creating it.
 *   </para>
 * </note>
 */
void
gtk_source_language_manager_set_search_path (GtkSourceLanguageManager *lm,
					     gchar                   **dirs)
{
	gchar **tmp;

	g_return_if_fail (GTK_IS_SOURCE_LANGUAGE_MANAGER (lm));

	/* Search path cannot be changed in the list of available languages
	 * as been already computed */
	g_return_if_fail (lm->priv->ids == NULL);

	tmp = lm->priv->lang_dirs;

	if (dirs == NULL)
		lm->priv->lang_dirs = _gtk_source_view_get_default_dirs (LANGUAGE_DIR, TRUE);
	else
		lm->priv->lang_dirs = g_strdupv (dirs);

	g_strfreev (tmp);

	notify_search_path (lm);
}

/**
 * gtk_source_language_manager_get_search_path:
 * @lm: a #GtkSourceLanguageManager.
 *
 * Gets the list directories where @lm looks for language files.
 *
 * Returns: %NULL-terminated array containg a list of language files directories.
 * The array is owned by @lm and must not be modified.
 */
G_CONST_RETURN gchar* G_CONST_RETURN *
gtk_source_language_manager_get_search_path (GtkSourceLanguageManager *lm)
{
	g_return_val_if_fail (GTK_IS_SOURCE_LANGUAGE_MANAGER (lm), NULL);

	if (lm->priv->lang_dirs == NULL)
		lm->priv->lang_dirs = _gtk_source_view_get_default_dirs (LANGUAGE_DIR, TRUE);

	return (const gchar * const *)lm->priv->lang_dirs;
}

/**
 * _gtk_source_language_manager_get_rng_file:
 * @lm: a #GtkSourceLanguageManager.
 *
 * Returns location of the RNG schema file for lang files version 2.
 *
 * Returns: path to RNG file. It belongs to %lm and must not be freed or modified.
 */
const char *
_gtk_source_language_manager_get_rng_file (GtkSourceLanguageManager *lm)
{
	g_return_val_if_fail (GTK_IS_SOURCE_LANGUAGE_MANAGER (lm), NULL);

	if (lm->priv->rng_file == NULL)
	{
		const gchar * const *dirs;

		for (dirs = gtk_source_language_manager_get_search_path (lm);
		     dirs != NULL && *dirs != NULL;
		     ++dirs)
		{
			gchar *file;

			file = g_build_filename (*dirs, RNG_SCHEMA_FILE, NULL);
			if (g_file_test (file, G_FILE_TEST_EXISTS))
			{
				lm->priv->rng_file = file;
				break;
			}

			g_free (file);
		}
	}

	return lm->priv->rng_file;
}

static void
ensure_languages (GtkSourceLanguageManager *lm)
{
	GSList *filenames, *l;
	GPtrArray *ids_array = NULL;

	if (lm->priv->language_ids != NULL)
		return;

	lm->priv->language_ids = g_hash_table_new_full (g_str_hash, g_str_equal,
							g_free, g_object_unref);

	filenames = _gtk_source_view_get_file_list ((gchar **)gtk_source_language_manager_get_search_path (lm),
						    LANG_FILE_SUFFIX,
						    TRUE);

	for (l = filenames; l != NULL; l = l->next)
	{
		GtkSourceLanguage *lang;
		gchar *filename;

		filename = l->data;

		lang = _gtk_source_language_new_from_file (filename, lm);

		if (lang == NULL)
		{
			g_warning ("Error reading language specification file '%s'", filename);
			continue;
		}

		if (g_hash_table_lookup (lm->priv->language_ids, lang->priv->id) == NULL)
		{
			g_hash_table_insert (lm->priv->language_ids,
					     g_strdup (lang->priv->id),
					     lang);

			if (ids_array == NULL)
				ids_array = g_ptr_array_new ();

			g_ptr_array_add (ids_array, g_strdup (lang->priv->id));
		}
		else
		{
			g_object_unref (lang);
		}
	}

	if (ids_array != NULL)
	{
		/* Ensure the array is NULL terminated */
		g_ptr_array_add (ids_array, NULL);
		lm->priv->ids = (gchar **)g_ptr_array_free (ids_array, FALSE);
	}

	g_slist_foreach (filenames, (GFunc) g_free, NULL);
	g_slist_free (filenames);
}


/**
 * gtk_source_language_manager_get_language_ids:
 * @lm: a #GtkSourceLanguageManager
 *
 * Returns the ids of the available languages.
 *
 * Returns: a %NULL-terminated array of string containing the ids of the
 * available languages or %NULL if no language is available. The array
 * is owned by @lm and must not be modified.
 */
G_CONST_RETURN gchar* G_CONST_RETURN *
gtk_source_language_manager_get_language_ids (GtkSourceLanguageManager *lm)
{
	g_return_val_if_fail (GTK_IS_SOURCE_LANGUAGE_MANAGER (lm), NULL);

	ensure_languages (lm);

	return (const gchar * const *)lm->priv->ids;
}

/**
 * gtk_source_language_manager_get_language:
 * @lm: a #GtkSourceLanguageManager.
 * @id: a language id.
 *
 * Gets the #GtkSourceLanguage identified by the given @id in the language
 * manager.
 *
 * Returns: a #GtkSourceLanguage, or %NULL if there is no language
 * identified by the given @id. Return value is owned by @lm and should not
 * be freed.
 */
GtkSourceLanguage *
gtk_source_language_manager_get_language (GtkSourceLanguageManager *lm,
					  const gchar              *id)
{
	g_return_val_if_fail (GTK_IS_SOURCE_LANGUAGE_MANAGER (lm), NULL);
	g_return_val_if_fail (id != NULL, NULL);

	ensure_languages (lm);

	return g_hash_table_lookup (lm->priv->language_ids, id);
}

static GSList *
pick_langs_for_filename (GtkSourceLanguageManager *lm,
			 const gchar              *filename)
{
	char *filename_utf8;
	const gchar* const * p;
	GSList *langs = NULL;

	/* Use g_filename_display_name() instead of g_filename_to_utf8() because
	 * g_filename_display_name() doesn't fail and replaces non-convertible
	 * characters to unicode substitution symbol. */
	filename_utf8 = g_filename_display_name (filename);

	for (p = gtk_source_language_manager_get_language_ids (lm);
	     p != NULL && *p != NULL;
	     p++)
	{
		GtkSourceLanguage *lang;
		gchar **globs, **gptr;

		lang = gtk_source_language_manager_get_language (lm, *p);
		globs = gtk_source_language_get_globs (lang);

		for (gptr = globs; gptr != NULL && *gptr != NULL; gptr++)
		{
			/* FIXME g_pattern_match is wrong: there are no '[...]'
			 * character ranges and '*' and '?' can not be escaped
			 * to include them literally in a pattern.  */
			if (g_pattern_match_simple (*gptr, filename_utf8))
			{
				langs = g_slist_prepend (langs, lang);
			}
		}

		g_strfreev (globs);
	}

	g_free (filename_utf8);
	return langs;
}

static GtkSourceLanguage *
pick_lang_for_mime_type_pass (GtkSourceLanguageManager *lm,
			      const char               *mime_type,
			      gboolean                  exact_match)
{
	const gchar* const * id_ptr;

	for (id_ptr = gtk_source_language_manager_get_language_ids (lm);
	     id_ptr != NULL && *id_ptr != NULL;
	     id_ptr++)
	{
		GtkSourceLanguage *lang;
		gchar **mime_types, **mptr;

		lang = gtk_source_language_manager_get_language (lm, *id_ptr);
		mime_types = gtk_source_language_get_mime_types (lang);

		for (mptr = mime_types; mptr != NULL && *mptr != NULL; mptr++)
		{
			gboolean matches;

			if (exact_match)
				matches = strcmp (mime_type, *mptr) == 0;
			else
				matches = g_content_type_is_a (mime_type, *mptr);

			if (matches)
			{
				g_strfreev (mime_types);
				return lang;
			}
		}

		g_strfreev (mime_types);
	}

	return NULL;
}

static GtkSourceLanguage *
pick_lang_for_mime_type_real (GtkSourceLanguageManager *lm,
			      const char               *mime_type)
{
	GtkSourceLanguage *lang;
	lang = pick_lang_for_mime_type_pass (lm, mime_type, TRUE);
	if (!lang)
		lang = pick_lang_for_mime_type_pass (lm, mime_type, FALSE);
	return lang;
}

#ifdef G_OS_WIN32
static void
grok_win32_content_type (const gchar  *content_type,
			 gchar       **alt_filename,
			 gchar       **mime_type)
{
	*alt_filename = NULL;
	*mime_type = NULL;

	/* If it contains slash, then it's probably a mime type.
	 * Otherwise treat is an extension. */
	if (strchr (content_type, '/') != NULL)
		*mime_type = g_strdup (content_type);
	else
		*alt_filename = g_strjoin ("filename", content_type, NULL);
}
#endif

static GtkSourceLanguage *
pick_lang_for_mime_type (GtkSourceLanguageManager *lm,
			 const gchar              *content_type)
{
	GtkSourceLanguage *lang = NULL;

#ifndef G_OS_WIN32
	/* On Unix "content type" is mime type */
	lang = pick_lang_for_mime_type_real (lm, content_type);
#else
	/* On Windows "content type" is an extension, but user may pass a mime type too */
	gchar *mime_type;
	gchar *alt_filename;

	grok_win32_content_type (content_type, &alt_filename, &mime_type);

	if (alt_filename != NULL)
	{
		GSList *langs;
		
		langs = pick_langs_for_filename (lm, alt_filename);
		
		if (langs != NULL)
			lang = GTK_SOURCE_LANGUAGE (langs->data);
	}
	
	if (lang == NULL && mime_type != NULL)
		lang = pick_lang_for_mime_type_real (lm, mime_type);

	g_free (mime_type);
	g_free (alt_filename);
#endif
	return lang;
}

/**
 * gtk_source_language_manager_guess_language:
 * @lm: a #GtkSourceLanguageManager.
 * @filename: a filename in Glib filename encoding, or %NULL.
 * @content_type: a content type (as in GIO API), or %NULL.
 *
 * Picks a #GtkSourceLanguage for given file name and content type,
 * according to the information in lang files. Either @filename or
 * @content_type may be %NULL. This function can be used as follows:
 *
 * <informalexample><programlisting>
 *   GtkSourceLanguage *lang;
 *   lang = gtk_source_language_manager_guess_language (filename, NULL);
 *   gtk_source_buffer_set_language (buffer, lang);
 * </programlisting></informalexample>
 *
 * or
 *
 * <informalexample><programlisting>
 *   GtkSourceLanguage *lang = NULL;
 *   gboolean result_uncertain;
 *   gchar *content_type;
 *
 *   content_type = g_content_type_guess (filename, NULL, 0, &result_uncertain);
 *   if (result_uncertain)
 *     {
 *       g_free (content_type);
 *       content_type = NULL;
 *     }
 *
 *   lang = gtk_source_language_manager_guess_language (manager, filename, content_type);
 *   gtk_source_buffer_set_language (buffer, lang);
 *
 *   g_free (content_type);
 * </programlisting></informalexample>
 *
 * etc. Use gtk_source_language_get_mime_types() and gtk_source_language_get_globs()
 * if you need full control over file -> language mapping.
 *
 * Returns: a #GtkSourceLanguage, or %NULL if there is no suitable language
 * for given @filename and/or @content_type. Return value is owned by @lm
 * and should not be freed.
 *
 * Since: 2.4
 */
GtkSourceLanguage *
gtk_source_language_manager_guess_language (GtkSourceLanguageManager *lm,
					    const gchar		     *filename,
					    const gchar		     *content_type)
{
	GtkSourceLanguage *lang = NULL;
	GSList *langs = NULL;

	g_return_val_if_fail (GTK_IS_SOURCE_LANGUAGE_MANAGER (lm), NULL);
	g_return_val_if_fail (filename != NULL || content_type != NULL, NULL);
	g_return_val_if_fail ((filename == NULL || *filename != 0) &&
			      (content_type == NULL || *content_type != 0), NULL);

	ensure_languages (lm);

	/* Glob take precedence over mime match. Mime match is used in the
	   following cases:
	  - to pick among the list of glob matches
	  - to refine a glob match (e.g. glob is xml and mime is an xml dialect)
	  - no glob matches
	*/

	if (filename != NULL)
		langs = pick_langs_for_filename (lm, filename);

	if (langs != NULL)
	{
		/* Use mime to pick among glob matches */
		if (content_type != NULL)
		{
			GSList *l;
			
			for (l = langs; l != NULL; l = g_slist_next (l))
			{
				gchar **mime_types, **gptr;
				
				lang = GTK_SOURCE_LANGUAGE (l->data);
				mime_types = gtk_source_language_get_mime_types (lang);
				
				for (gptr = mime_types; gptr != NULL && *gptr != NULL; gptr++)
				{
					gchar *content;
					
					content = g_content_type_from_mime_type (*gptr);
					
					if (content != NULL && g_content_type_is_a (content_type, content))
					{
						if (!g_content_type_equals (content_type, content))
						{
							GtkSourceLanguage *mimelang;
							
							mimelang = pick_lang_for_mime_type (lm, content_type);
							
							if (mimelang != NULL)
								lang = mimelang;
						}
						
						g_strfreev (mime_types);
						g_slist_free (langs);
						g_free (content);
						
						return lang;
					}
					g_free (content);
				}
				
				g_strfreev (mime_types);
			}
		}
		lang = GTK_SOURCE_LANGUAGE (langs->data);
		
		g_slist_free (langs);
	}
	/* No glob match */
	else if (langs == NULL && content_type != NULL)
	{
		lang = pick_lang_for_mime_type (lm, content_type);
	}

	return lang;
}
