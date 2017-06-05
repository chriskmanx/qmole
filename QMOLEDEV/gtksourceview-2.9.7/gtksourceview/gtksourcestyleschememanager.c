/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/*  gtksourcestyleschememanager.c
 *
 *  Copyright (C) 2003-2007 - Paolo Maggi <paolo@gnome.org>
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

#include "gtksourcestyleschememanager.h"
#include "gtksourceview-marshal.h"
#include "gtksourceview-i18n.h"
#include "gtksourceview-utils.h"
#include <string.h>

/**
 * SECTION:styleschememanager
 * @Short_description: Object which provides access to #GtkSourceStyleScheme<!-- -->s
 * @Title: GtkSourceStyleSchemeManager
 * @See_also: #GtkSourceStyleScheme
 */

#define SCHEME_FILE_SUFFIX	".xml"
#define STYLES_DIR		"styles"

struct _GtkSourceStyleSchemeManagerPrivate
{
	GHashTable	*schemes_hash;

	gchar          **search_path;
	gboolean	 need_reload;

	gchar          **ids; /* Cache the IDs of the available schemes */
};


enum {
	PROP_0,
	PROP_SEARCH_PATH,
	PROP_SCHEME_IDS
};


G_DEFINE_TYPE (GtkSourceStyleSchemeManager, gtk_source_style_scheme_manager, G_TYPE_OBJECT)

static void
gtk_source_style_scheme_manager_set_property (GObject 	   *object,
					      guint         prop_id,
					      const GValue *value,
					      GParamSpec   *pspec)
{
	GtkSourceStyleSchemeManager *sm;

	sm = GTK_SOURCE_STYLE_SCHEME_MANAGER (object);

	switch (prop_id)
	{
		case PROP_SEARCH_PATH:
			gtk_source_style_scheme_manager_set_search_path
					(sm, g_value_get_boxed (value));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object,
							   prop_id,
							   pspec);
			break;
	}
}

static void
gtk_source_style_scheme_manager_get_property (GObject    *object,
					      guint       prop_id,
					      GValue     *value,
					      GParamSpec *pspec)
{
	GtkSourceStyleSchemeManager *sm;

	sm = GTK_SOURCE_STYLE_SCHEME_MANAGER (object);

	switch (prop_id)
	{
		case PROP_SEARCH_PATH:
			g_value_set_boxed (value,
					   gtk_source_style_scheme_manager_get_search_path (sm));
			break;

		case PROP_SCHEME_IDS:
			g_value_set_boxed (value,
					   gtk_source_style_scheme_manager_get_scheme_ids (sm));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object,
							   prop_id,
							   pspec);
			break;
	}
}

static void
free_schemes (GtkSourceStyleSchemeManager *mgr)
{
	if (mgr->priv->schemes_hash != NULL)
	{
		g_hash_table_destroy (mgr->priv->schemes_hash);
		mgr->priv->schemes_hash = NULL;
	}

	g_strfreev (mgr->priv->ids);
	mgr->priv->ids = NULL;
}

static void
gtk_source_style_scheme_manager_finalize (GObject *object)
{
	GtkSourceStyleSchemeManager *mgr;

	mgr = GTK_SOURCE_STYLE_SCHEME_MANAGER (object);

	free_schemes (mgr);

	g_strfreev (mgr->priv->search_path);

	G_OBJECT_CLASS (gtk_source_style_scheme_manager_parent_class)->finalize (object);
}

static void
gtk_source_style_scheme_manager_class_init (GtkSourceStyleSchemeManagerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize	= gtk_source_style_scheme_manager_finalize;
	object_class->set_property = gtk_source_style_scheme_manager_set_property;
	object_class->get_property = gtk_source_style_scheme_manager_get_property;

	g_object_class_install_property (object_class,
					 PROP_SEARCH_PATH,
					 g_param_spec_boxed ("search-path",
						 	     _("Style scheme search path"),
							     _("List of directories and files where the "
							       "style schemes are located"),
							     G_TYPE_STRV,
							     G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_SCHEME_IDS,
					 g_param_spec_boxed ("scheme-ids",
						 	     _("Scheme ids"),
							     _("List of the ids of the available "
							       "style schemes"),
							     G_TYPE_STRV,
							     G_PARAM_READABLE));

	g_type_class_add_private (object_class, sizeof(GtkSourceStyleSchemeManagerPrivate));
}

static void
gtk_source_style_scheme_manager_init (GtkSourceStyleSchemeManager *mgr)
{
	mgr->priv = G_TYPE_INSTANCE_GET_PRIVATE (mgr,
						 GTK_TYPE_SOURCE_STYLE_SCHEME_MANAGER,
						 GtkSourceStyleSchemeManagerPrivate);
	mgr->priv->schemes_hash = NULL;
	mgr->priv->ids = NULL;
	mgr->priv->search_path = NULL;
	mgr->priv->need_reload = TRUE;
}

/**
 * gtk_source_style_scheme_manager_new:
 *
 * Creates a new style manager. If you do not need more than one style
 * manager then use gtk_source_style_scheme_manager_get_default() instead.
 *
 * Returns: a #GtkSourceStyleSchemeManager.
 */
GtkSourceStyleSchemeManager *
gtk_source_style_scheme_manager_new (void)
{
	return g_object_new (GTK_TYPE_SOURCE_STYLE_SCHEME_MANAGER, NULL);
}

/**
 * gtk_source_style_scheme_manager_get_default:
 *
 * Returns the default #GtkSourceStyleSchemeManager instance.
 *
 * Returns: a #GtkSourceStyleSchemeManager. Return value is owned
 * by GtkSourceView library and must not be unref'ed.
 */
GtkSourceStyleSchemeManager *
gtk_source_style_scheme_manager_get_default (void)
{
	static GtkSourceStyleSchemeManager *instance;

	if (instance == NULL)
	{
		instance = gtk_source_style_scheme_manager_new ();
		g_object_add_weak_pointer (G_OBJECT (instance),
					   (gpointer) &instance);
	}

	return instance;
}

static GSList *
ids_list_remove (GSList *ids, const gchar *id, gboolean free_data)
{
	GSList *o = g_slist_find_custom (ids, id, (GCompareFunc) strcmp);

	if (o != NULL)
	{
		if (free_data)
			g_free (o->data);
		ids = g_slist_delete_link (ids, o);
	}

	return ids;
}

static gboolean
build_reference_chain (GtkSourceStyleScheme *scheme,
		       GHashTable           *hash,
		       GSList              **ret)
{
	GSList *chain;
	gboolean retval = TRUE;

	chain = g_slist_prepend (NULL, scheme);

	while (TRUE)
	{
		GtkSourceStyleScheme *parent_scheme;
		const gchar *parent_id;

		parent_id = _gtk_source_style_scheme_get_parent_id (scheme);

		if (parent_id == NULL)
			break;

		parent_scheme = g_hash_table_lookup (hash, parent_id);

		if (parent_scheme == NULL)
		{
			g_warning ("Unknown parent scheme '%s' in scheme '%s'",
				   parent_id, gtk_source_style_scheme_get_id (scheme));
			retval = FALSE;
			break;
		}
		else if (g_slist_find (chain, parent_scheme) != NULL)
		{
			g_warning ("Reference cycle in scheme '%s'", parent_id);
			retval = FALSE;
			break;
		}
		else
		{
			_gtk_source_style_scheme_set_parent (scheme, parent_scheme);
		}

		chain = g_slist_prepend (chain, parent_scheme);
		scheme = parent_scheme;
	}

	*ret = chain;
	return retval;
}

static GSList *
check_parents (GSList     *ids,
	       GHashTable *hash)
{
	GSList *to_check;

	to_check = g_slist_copy (ids);

	while (to_check != NULL)
	{
		GSList *chain;
		gboolean valid;
		GtkSourceStyleScheme *scheme_to_check;

		scheme_to_check = g_hash_table_lookup (hash, to_check->data);
		g_return_val_if_fail (scheme_to_check != NULL, NULL);

		valid = build_reference_chain (scheme_to_check, hash, &chain);

		while (chain != NULL)
		{
			const gchar *id;
			GtkSourceStyleScheme *scheme = chain->data;

			id = gtk_source_style_scheme_get_id (scheme);

			to_check = ids_list_remove (to_check, id, FALSE);

			if (!valid)
			{
				ids = ids_list_remove (ids, id, TRUE);
				g_hash_table_remove (hash, id);
			}

			chain = g_slist_delete_link (chain, chain);
		}
	}

	return ids;
}

static gchar **
slist_to_strv (GSList *list)
{
	gchar **res;
	guint i = 0;

	res = g_new (gchar *, g_slist_length (list) + 1);

	for ( ; list != NULL; list = list->next)
	{
		res[i] = list->data;
		++i;
	}

	res[i] = NULL;

	return res;
}

static void
reload_if_needed (GtkSourceStyleSchemeManager *mgr)
{
	GSList *ids = NULL;
	GSList *files;
	GSList *l;
	GHashTable *schemes_hash;

	if (!mgr->priv->need_reload)
		return;

	schemes_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_object_unref);

	files = _gtk_source_view_get_file_list ((gchar **)gtk_source_style_scheme_manager_get_search_path (mgr),
						SCHEME_FILE_SUFFIX,
						FALSE);

	for (l = files; l != NULL; l = l->next)
	{
		GtkSourceStyleScheme *scheme;
		gchar *filename;

		filename = l->data;

		scheme = _gtk_source_style_scheme_new_from_file (filename);

		if (scheme != NULL)
		{
			const gchar *id = gtk_source_style_scheme_get_id (scheme);
			GtkSourceStyleScheme *old;

			old = g_hash_table_lookup (schemes_hash, id);

			if (old != NULL)
				ids = ids_list_remove (ids, id, TRUE);

			ids = g_slist_prepend (ids, g_strdup (id));
			g_hash_table_insert (schemes_hash, g_strdup (id), scheme);
		}
	}

	ids = check_parents (ids, schemes_hash);

	g_slist_foreach (files, (GFunc) g_free, NULL);
	g_slist_free (files);

	free_schemes (mgr);

	mgr->priv->need_reload = FALSE;
	mgr->priv->schemes_hash = schemes_hash;

	mgr->priv->ids = slist_to_strv (ids);
	g_slist_free (ids); /* slist_to_strv trasfer the onwnership of the strings
	                       to the string array */
}

static void
notify_search_path (GtkSourceStyleSchemeManager *mgr)
{
	mgr->priv->need_reload = TRUE;

	g_object_notify (G_OBJECT (mgr), "search-path");
	g_object_notify (G_OBJECT (mgr), "scheme-ids");
}

/**
 * gtk_source_style_scheme_manager_set_search_path:
 * @manager: a #GtkSourceStyleSchemeManager.
 * @path: a %NULL-terminated array of strings or %NULL.
 *
 * Sets the list of directories where the @manager looks for
 * style scheme files.
 * If @dirs is %NULL, the search path is reset to default.
 */
void
gtk_source_style_scheme_manager_set_search_path (GtkSourceStyleSchemeManager  *manager,
						 gchar	                     **path)
{
	gchar **tmp;

	g_return_if_fail (GTK_IS_SOURCE_STYLE_SCHEME_MANAGER (manager));

	tmp = manager->priv->search_path;

	if (path == NULL)
		manager->priv->search_path = _gtk_source_view_get_default_dirs (STYLES_DIR, FALSE);
	else
		manager->priv->search_path = g_strdupv (path);

	g_strfreev (tmp);

	notify_search_path (manager);
}

/**
 * gtk_source_style_scheme_manager_append_search_path:
 * @manager: a #GtkSourceStyleSchemeManager.
 * @path: a directory or a filename.
 *
 * Appends @path to the list of directories where the @manager looks for
 * style scheme files.
 * See gtk_source_style_scheme_manager_set_search_path() for details.
 */
void
gtk_source_style_scheme_manager_append_search_path (GtkSourceStyleSchemeManager *manager,
						    const gchar                 *path)
{
	guint len = 0;

	g_return_if_fail (GTK_IS_SOURCE_STYLE_SCHEME_MANAGER (manager));
	g_return_if_fail (path != NULL);

	if (manager->priv->search_path == NULL)
		manager->priv->search_path = _gtk_source_view_get_default_dirs (STYLES_DIR, FALSE);

	g_return_if_fail (manager->priv->search_path != NULL);

	len = g_strv_length (manager->priv->search_path);

	manager->priv->search_path = g_renew (gchar *,
					      manager->priv->search_path,
					      len + 2); /* old path + new entry + NULL */

	manager->priv->search_path[len] = g_strdup (path);
	manager->priv->search_path[len + 1] = NULL;

	notify_search_path (manager);
}

/**
 * gtk_source_style_scheme_manager_prepend_search_path:
 * @manager: a #GtkSourceStyleSchemeManager.
 * @path: a directory or a filename.
 *
 * Prepends @path to the list of directories where the @manager looks
 * for style scheme files.
 * See gtk_source_style_scheme_manager_set_search_path() for details.
 */
void
gtk_source_style_scheme_manager_prepend_search_path (GtkSourceStyleSchemeManager *manager,
						     const gchar                 *path)
{
	guint len = 0;
	gchar **new_search_path;

	g_return_if_fail (GTK_IS_SOURCE_STYLE_SCHEME_MANAGER (manager));
	g_return_if_fail (path != NULL);

	if (manager->priv->search_path == NULL)
		manager->priv->search_path = _gtk_source_view_get_default_dirs (STYLES_DIR, FALSE);

	g_return_if_fail (manager->priv->search_path != NULL);

	len = g_strv_length (manager->priv->search_path);

	new_search_path = g_new (gchar *, len + 2);
	new_search_path[0] = g_strdup (path);
	memcpy (new_search_path + 1, manager->priv->search_path, (len + 1) * sizeof (gchar*));

	g_free (manager->priv->search_path);
	manager->priv->search_path = new_search_path;

	notify_search_path (manager);
}

/**
 * gtk_source_style_scheme_manager_get_search_path:
 * @manager: a #GtkSourceStyleSchemeManager.
 *
 * Returns the current search path for the @manager.
 * See gtk_source_style_scheme_manager_set_search_path() for details.
 *
 * Returns: a NULL-terminated array of string containing the search path.
 * The array is owned by the @manager and must not be modified.
 */
G_CONST_RETURN gchar* G_CONST_RETURN *
gtk_source_style_scheme_manager_get_search_path (GtkSourceStyleSchemeManager *manager)
{
	g_return_val_if_fail (GTK_IS_SOURCE_STYLE_SCHEME_MANAGER (manager), NULL);

	if (manager->priv->search_path == NULL)
		manager->priv->search_path = _gtk_source_view_get_default_dirs (STYLES_DIR, FALSE);

	return (const gchar * const *)manager->priv->search_path;
}

/**
 * gtk_source_style_scheme_manager_force_rescan:
 * @manager: a #GtkSourceStyleSchemeManager
 *
 * Mark any currently cached information about the available style scehems
 * as invalid. All the available style schemes will be reloaded next time
 * the @manager is accessed.
 */
void
gtk_source_style_scheme_manager_force_rescan (GtkSourceStyleSchemeManager *manager)
{
	manager->priv->need_reload = TRUE;

	g_object_notify (G_OBJECT (manager), "scheme-ids");
}

/**
 * gtk_source_style_scheme_manager_get_scheme_ids:
 * @manager: a #GtkSourceStyleSchemeManager
 *
 * Returns the ids of the available style schemes.
 *
 * Returns: a %NULL-terminated array of string containing the ids of the
 * available style schemes or %NULL if no style scheme is available. The array
 * is owned by the @manager and must not be modified.
 */
G_CONST_RETURN gchar* G_CONST_RETURN *
gtk_source_style_scheme_manager_get_scheme_ids (GtkSourceStyleSchemeManager *manager)
{
	g_return_val_if_fail (GTK_IS_SOURCE_STYLE_SCHEME_MANAGER (manager), NULL);

	reload_if_needed (manager);

	return (const gchar * const *)manager->priv->ids;
}

/**
 * gtk_source_style_scheme_manager_get_scheme:
 * @manager: a #GtkSourceStyleSchemeManager
 * @scheme_id: style scheme id to find
 *
 * Looks up style scheme by id.
 *
 * Returns: a #GtkSourceStyleScheme object. Returned value is owned by
 * @manager and must not be unref'ed.
 */
GtkSourceStyleScheme *
gtk_source_style_scheme_manager_get_scheme (GtkSourceStyleSchemeManager *manager,
					    const gchar                 *scheme_id)
{
	g_return_val_if_fail (GTK_IS_SOURCE_STYLE_SCHEME_MANAGER (manager), NULL);
	g_return_val_if_fail (scheme_id != NULL, NULL);

	reload_if_needed (manager);

	return g_hash_table_lookup (manager->priv->schemes_hash, scheme_id);
}
