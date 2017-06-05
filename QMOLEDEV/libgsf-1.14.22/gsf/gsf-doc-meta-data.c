/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-doc-meta-data.c:
 *
 * Copyright (C) 2002-2006 Dom Lachowicz (cinamod@hotmail.com)
 * 			   Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-doc-meta-data.h>
#include <gsf/gsf-docprop-vector.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>
#include <stdlib.h>

struct _GsfDocMetaData {
	GObject	base;

	GHashTable *table;
};
typedef GObjectClass GsfDocMetaDataClass;

struct _GsfDocProp {
	char   *name;
	GValue *val;
	char   *linked_to; /* optionally NULL */
};

static GObjectClass *parent_class;

static void
gsf_doc_meta_data_finalize (GObject *obj)
{
	g_hash_table_destroy (GSF_DOC_META_DATA (obj)->table);
	parent_class->finalize (obj);
}

static void
gsf_doc_meta_data_init (GObject *obj)
{
	GsfDocMetaData *meta = GSF_DOC_META_DATA (obj);
	meta->table = g_hash_table_new_full (g_str_hash, g_str_equal,
		NULL, (GDestroyNotify) gsf_doc_prop_free);
}

static void
gsf_doc_meta_data_class_init (GObjectClass *gobject_class)
{
	gobject_class->finalize = gsf_doc_meta_data_finalize;
	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS(GsfDocMetaData, gsf_doc_meta_data,
	  gsf_doc_meta_data_class_init, gsf_doc_meta_data_init,
	  G_TYPE_OBJECT)

/**********************************************************************/

/**
 * gsf_doc_meta_data_new :
 *
 * Returns: a new metadata property collection
 **/
GsfDocMetaData *
gsf_doc_meta_data_new (void)
{
	return g_object_new (GSF_DOC_META_DATA_TYPE, NULL);
}

/**
 * gsf_doc_meta_data_lookup :
 * @meta : #GsfDocMetaData
 * @name :
 *
 * Returns: the property with name @id in @meta.  The caller can modify the
 * property value and link but not the name.
 **/
GsfDocProp *
gsf_doc_meta_data_lookup (GsfDocMetaData const *meta, char const *name)
{
	g_return_val_if_fail (IS_GSF_DOC_META_DATA (meta), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	return g_hash_table_lookup (meta->table, name);
}

/**
 * gsf_doc_meta_data_insert :
 * @meta : #GsfDocMetaData
 * @name : the id.
 * @value : #GValue
 *
 * Take ownership of @name and @value and insert a property into @meta.
 * If a property exists with @name, it is replaced (The link is lost)
 **/
void
gsf_doc_meta_data_insert (GsfDocMetaData *meta, char *name, GValue *value)
{
	GsfDocProp *prop;

	g_return_if_fail (IS_GSF_DOC_META_DATA (meta));
	g_return_if_fail (name != NULL);
	prop = g_new (GsfDocProp, 1);
	prop->name = name;
	prop->val  = value;
	prop->linked_to = NULL;
	g_hash_table_replace (meta->table, prop->name, prop);
}

/**
 * gsf_doc_meta_data_remove :
 * @meta : the collection
 * @name : the non-null string name of the property
 *
 * If @name does not exist in the collection, do nothing. If @name does exist,
 * remove it and its value from the collection
 **/
void
gsf_doc_meta_data_remove (GsfDocMetaData *meta, char const *name)
{
	g_return_if_fail (IS_GSF_DOC_META_DATA (meta));
	g_return_if_fail (name != NULL);
	g_hash_table_remove (meta->table, name);
}

/**
 * gsf_doc_meta_data_store :
 * @meta : #GsfDocMetaData
 * @name :
 *
 **/
GsfDocProp *
gsf_doc_meta_data_steal (GsfDocMetaData *meta, char const *name)
{
	GsfDocProp *prop;
	g_return_val_if_fail (IS_GSF_DOC_META_DATA (meta), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	prop = g_hash_table_lookup (meta->table, name);
	if (NULL != prop)
		g_hash_table_steal (meta->table, name);
	return prop;
}

/**
 * gsf_doc_meta_data_store :
 * @meta : #GsfDocMetaData
 * @prop : #GsfDocProp
 *
 **/
void
gsf_doc_meta_data_store (GsfDocMetaData *meta, GsfDocProp *prop)
{
	g_return_if_fail (IS_GSF_DOC_META_DATA (meta));
	g_return_if_fail (prop != NULL);
	g_return_if_fail (prop != g_hash_table_lookup (meta->table, prop->name));
	g_hash_table_replace (meta->table, prop->name, prop);
}

static void
cb_collect_pairs (char *prop_name, GsfDocProp *prop, GPtrArray *pairs)
{
	g_ptr_array_add (pairs, prop_name);
	g_ptr_array_add (pairs, prop);
}

static int
deref_strcmp (const char **a, const char **b)
{
	return strcmp (*a, *b);
}

/**
 * gsf_doc_meta_data_foreach :
 * @meta : the collection
 * @func : the function called once for each element in the collection
 * @user_data : any supplied user data or %NULL
 *
 * Iterate through each (key, value) pair in this collection
 **/
void
gsf_doc_meta_data_foreach (GsfDocMetaData const *meta, GHFunc func, gpointer user_data)
{
	GPtrArray *pairs;
	unsigned ui;

	g_return_if_fail (IS_GSF_DOC_META_DATA (meta));

	if (g_hash_table_size (meta->table) == 0)
		return;

	/* Sort the pairs by property name in order to generate consistent
	   files. */
	pairs = g_ptr_array_new ();
	g_hash_table_foreach (meta->table, (GHFunc)cb_collect_pairs, pairs);

	qsort (&g_ptr_array_index (pairs, 0),
	       pairs->len / 2,
	       2 * sizeof (gpointer),
	       (GCompareFunc)deref_strcmp);

	for (ui = 0; ui < pairs->len; ui += 2)
		func (g_ptr_array_index (pairs, ui),
		      g_ptr_array_index (pairs, ui + 1),
		      user_data);

	g_ptr_array_free (pairs, TRUE);
}

/**
 * gsf_doc_meta_data_size :
 * @meta : the collection
 *
 * Returns: the number of items in this collection
 **/
gsize
gsf_doc_meta_data_size (GsfDocMetaData const *meta)
{
	g_return_val_if_fail (meta != NULL, 0);
	return (gsize) g_hash_table_size (meta->table);
}

static void
cb_print_property (G_GNUC_UNUSED char const *name,
		   GsfDocProp const *prop)
{
	if (gsf_doc_prop_get_link (prop) != NULL)
		g_print ("prop '%s' LINKED TO  -> '%s'\n",
			 name, gsf_doc_prop_get_link (prop));
	else
		g_print ("prop '%s'\n", name);

	gsf_doc_prop_dump (prop);
}

/**
 * gsf_doc_meta_dump :
 * @meta : #GsfDocMetaData
 *
 * A debugging utility to dump the content of @meta via g_print
 **/
void
gsf_doc_meta_dump (GsfDocMetaData const *meta)
{
	gsf_doc_meta_data_foreach (meta,
		(GHFunc) cb_print_property, NULL);
}

/**********************************************************************/

/**
 * gsf_doc_prop_new :
 * @name :
 *
 * Returns: a new #GsfDocProp which the caller is responsible for freeing.
 * Takes ownership of @name.
 **/
GsfDocProp *
gsf_doc_prop_new  (char *name)
{
	GsfDocProp *prop;

	g_return_val_if_fail (name != NULL, NULL);

	prop = g_new (GsfDocProp, 1);
	prop->name = name;
	prop->val  = NULL;
	prop->linked_to = NULL;

	return prop;
}

/**
 * gsf_doc_prop_free :
 * @prop : #GsfDocProp
 *
 * If @prop is non %NULL free the memory associated with it
 **/
void
gsf_doc_prop_free (GsfDocProp *prop)
{
	if (NULL != prop) {
		g_free (prop->linked_to);

		if (prop->val) {
			g_value_unset (prop->val);
			g_free (prop->val);
		}
		g_free (prop->name);
		g_free (prop);
	}
}

/**
 * gsf_doc_prop_get_name :
 * @prop : #GsfDocProp
 *
 * Returns: the name of the property, the caller should not modify the result.
 **/
char const *
gsf_doc_prop_get_name (GsfDocProp const *prop)
{
	g_return_val_if_fail (prop != NULL, NULL);
	return prop->name;
}

/**
 * gsf_doc_prop_get_val :
 * @prop : the property
 *
 * Returns: the value of the property, the caller should not modify the result.
 **/
GValue const *
gsf_doc_prop_get_val (GsfDocProp const *prop)
{
	g_return_val_if_fail (prop != NULL, NULL);
	return prop->val;
}

/**
 * gsf_doc_prop_set_val :
 * @prop : #GsfDocProp
 * @val  : #GValue
 *
 * Assigns @val to @prop, and unsets and frees the current value.
 **/
void
gsf_doc_prop_set_val (GsfDocProp *prop, GValue *val)
{
	g_return_if_fail (prop != NULL);

	if (val != prop->val) {
		if (prop->val != NULL) {
			g_value_unset (prop->val);
			g_free (prop->val);
		}
		prop->val = val;
	}
}

/**
 * gsf_doc_prop_swap_val :
 * @prop : #GsfDocProp
 * @val  : #GValue
 *
 * Returns: the current value of @prop, and replaces it with @val
 * 	Caller is responsible for unsetting and freeing the result.
 **/
GValue *
gsf_doc_prop_swap_val (GsfDocProp *prop, GValue *val)
{
	GValue *old_val;
	g_return_val_if_fail (prop != NULL, NULL);

	old_val = prop->val;
	prop->val = val;
	return old_val;
}

/**
 * gsf_doc_prop_get_link :
 * @prop : #GsfDocProp
 *
 * Returns: the current link descriptor of @prop.  The result should not be
 * 	freed or modified.
 **/
char const *
gsf_doc_prop_get_link (GsfDocProp const *prop)
{
	g_return_val_if_fail (prop != NULL, NULL);
	return prop->linked_to;
}

/**
 * gsf_doc_prop_set_link :
 * @prop : #GsfDocProp
 * @link : optionally %NULL
 *
 * Sets @prop's link to @link
 **/
void
gsf_doc_prop_set_link (GsfDocProp *prop, char *link)
{
	g_return_if_fail (prop != NULL);

	if (link != prop->linked_to) {
		g_free (prop->linked_to);
		prop->linked_to = link;
	}
}

/**
 * gsf_doc_prop_dump :
 * @prop : #GsfDocProp
 *
 * A debugging utility to dump @prop as text via g_print
 * New in 1.14.2
 **/
void
gsf_doc_prop_dump (GsfDocProp const *prop)
{
	GValue const *val = gsf_doc_prop_get_val  (prop);
	char *tmp;
	if (VAL_IS_GSF_DOCPROP_VECTOR ((GValue *)val)) {
		GValueArray *va = gsf_value_get_docprop_varray (val);
		unsigned i;

		for (i = 0 ; i < va->n_values; i++) {
			tmp = g_strdup_value_contents (
				g_value_array_get_nth (va, i));
			g_print ("\t[%u] = %s\n", i, tmp);
			g_free (tmp);
		}
	} else {
		tmp = g_strdup_value_contents (val);
		g_print ("\t= %s\n", tmp);
		g_free (tmp);
	}
}
