/* $Id: e2_cache.c 2828 2013-10-23 07:54:48Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>
Copyright (C) 2004 Florian Zaehringer (flo.zaehringer@web.de)

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/config/e2_cache.c
@brief cache system

This file contains functions for e2's cache system: eg. to register
and unregister caches, and to write and read them to/from disk.
Custom-icon cacheing is handled here. We do not use GtkIconFactory etc as that's
mainly designed for "fixed" UI and is too much hassle to work with emelFM2
runtime-configuration
*/
/**
\page cache cacheing of emelFM2 runtime data

ToDo description of how cacheing works
*/

#include "e2_cache.h"
#include <string.h>
#include "e2_fs.h"
#include "e2_task.h"

static gchar *default_cache_file;
static GList *cache_list;
static GList *unknown_list;

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief check whether the name member of @a cache_element matches @a name
@param cache_element struct with member to compare
@param name name string to compare
@return 0 if @a cache_element matches
*/
static gint _e2_cache_str_compare (E2_Cache *cache_element, const gchar *name)
{
	return strcmp (name, cache_element->name);
}

  /******************/
 /***** public *****/
/******************/

/**
@brief check whether cache entry @a name exists
@param name cache identifier string
@return TRUE if cache item exists in cache_list or unknown_list
*/
gboolean e2_cache_check (gchar *name)
{
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
		return TRUE;
	else
	{
		found = g_list_find_custom (unknown_list, name,
			(GCompareFunc) _e2_cache_str_compare);
		if (found != NULL)
			return TRUE;
	}
	return FALSE;
}
/**
@brief set integer parameter from cache or default
@param name identifier string used in cache list
@param value pointer to store for cached value, if any, or @a def
@param def default value to use if none found in cache
@return
*/
void e2_cache_int_register (gchar *name, gint *value, gint def)
{
	E2_Cache *cache;
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		printd (DEBUG, "re-register int cache '%s'", name);
		return;
	}
	//first-time processing
	cache = ALLOCATE (E2_Cache);	//FIXME no deallocation of any caches
	CHECKALLOCATEDFATAL (cache)
	cache->type = E2_CACHE_TYPE_INT;
	found = g_list_find_custom (unknown_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found == NULL)
	{ //not in the queue, either
		printd (DEBUG, "int cache '%s' not found, creating default with %d", name, def);
		cache->name = g_strdup (name);
		*value = def;
	}
	else
	{  //process the queued item
		printd (DEBUG, "cache '%s' found", name);
		E2_Cache *unknown = found->data;
		cache->name = unknown->name;
		gchar *end = NULL;
		gchar *str = (gchar *) unknown->data;
		*value = g_ascii_strtoull (str, &end, 10);
		if (end == str)
		{
			printd (WARN, "int cache '%s' data conversion failed from '%s", name, str);
			*value = def;
		} else
			g_free (str);
		unknown_list = g_list_remove_link (unknown_list, found);
		g_list_free (found);
		DEALLOCATE (E2_Cache, unknown);
	}
	cache->data = GINT_TO_POINTER (value);
	cache_list = g_list_append (cache_list, cache);
	return;
}
/**
@brief set long integer parameter from cache or default
This function needed as we cannot determine at compile-time
how big a time_t is
If there are ever any uses for a long int cache item, this could
be converted, and the uses for this changed to run-time checks
@param name identifier string used in cache list
@param value pointer to store for cached value, if any, or @a def
@param def default value to use if none found in cache
@return
*/
void e2_cache_time_register (gchar *name, time_t *value, time_t def)
{
	E2_Cache *cache;
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		printd (DEBUG, "re-register long int cache '%s'", name);
		return;
	}
	//first-time processing
	cache = ALLOCATE (E2_Cache);
	CHECKALLOCATEDFATAL (cache)
	cache->type = E2_CACHE_TYPE_TIME;
	found = g_list_find_custom (unknown_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found == NULL)
	{ //not in the queue, either
		printd (DEBUG, "time cache '%s' not found, creating default with %d", name, def);
		cache->name = g_strdup (name);
		*value = def;
	}
	else
	{  //process the queued item
		printd (DEBUG, "cache '%s' found", name);
		E2_Cache *unknown = found->data;
		cache->name = unknown->name;
		gchar *end = NULL;
		gchar *str = (gchar *) unknown->data;
		*value = g_ascii_strtoull (str, &end, 10);
		if (end == str)
		{
			printd (WARN, "time cache '%s' data conversion failed from '%s", name, str);
			*value = def;
		} else
			g_free (str);
		unknown_list = g_list_remove_link (unknown_list, found);
		g_list_free (found);
		DEALLOCATE (E2_Cache, unknown);
	}
	cache->data = (gpointer) value;
	cache_list = g_list_append (cache_list, cache);
	return;
}
/**
@brief set double parameter from cache or default
@param name identifier string used in cache list
@param value pointer to store for cached value, if any, or @a def
@param def default value to use if none found in cache
@return
*/
void e2_cache_double_register (gchar *name, gdouble *value, gdouble def)
{
	E2_Cache *cache;
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		printd (DEBUG, "re-register double cache '%s'", name);
		return;
	}
	cache = ALLOCATE (E2_Cache);
	CHECKALLOCATEDFATAL (cache)
	cache->type = E2_CACHE_TYPE_DOUBLE;
	found = g_list_find_custom (unknown_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found == NULL)
	{  //not in queue either
		printd (DEBUG, "double cache '%s' not found, creating default with %f", name, def);
		cache->name = g_strdup (name);
		*value = def;
	}
	else
	{  //process queued item
		printd (DEBUG, "cache '%s' found", name);
		E2_Cache *unknown = found->data;
		cache->name = unknown->name;
		gchar *end = NULL;
		gchar *str = (gchar *) unknown->data;
		*value = g_ascii_strtod (str, &end);
		if (end == str)
		{
			printd (WARN, "double cache '%s' data conversion failed from '%s", name, str);
			*value = def;
		} else
			g_free (str);
		unknown_list = g_list_remove_link (unknown_list, found);
		g_list_free (found);
		DEALLOCATE (E2_Cache, unknown);
	}
	cache->data = (gpointer *) value;
	cache_list = g_list_append (cache_list, cache);
	return;
}
/**
@brief set string parameter from cache or default
@param name identifier string used in cache list
@param str pointer to store for cached value, if any, or @a def
@param def default value to use if none found in cache
@return
*/
void e2_cache_str_register (gchar *name, gchar **str, gchar *def)
{
	E2_Cache *cache;
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		printd (DEBUG, "re-register string cache '%s'", name);
		return;
	}
	cache = ALLOCATE (E2_Cache);
	CHECKALLOCATEDFATAL (cache)
	cache->type = E2_CACHE_TYPE_STR;
	found = g_list_find_custom (unknown_list, name,
		(GCompareFunc)_e2_cache_str_compare);
	if (found == NULL)
	{
		printd (DEBUG, "string cache '%s' not found, creating dedault with %s", name, def);
		cache->name = g_strdup (name);
		*str = strdup (def);
	}
	else
	{
		printd (DEBUG, "cache '%s' found", name);
		E2_Cache *unknown = found->data;
		cache->name = unknown->name;
		*str = (gchar *) unknown->data;
		unknown_list = g_list_remove_link (unknown_list, found);
		g_list_free (found);
		DEALLOCATE (E2_Cache, unknown);
	}
	cache->data = (gpointer *) str;
	cache_list = g_list_append (cache_list, cache);
	return;
}
/**
@brief setup liststore or treestore with data from cache, or empty
The element will be created if it does not exist
Unlike other cache-register functions, this does not expect an argument
which is the default to use when no matching cache is found.
@a preparefunc must be void preparefunc (gpointer *, GList *), set @a store,
and convert a list of rowstrings (possibly NULL), to data in that store.
@a syncfunc must be void syncfunc (gpointer*) and convert store data from
store *pointer to a GList of rowstrings, then set *pointer to address of that
list
As for lists, the value of the store pointer may be altered after cacheing, as
the address of that pointer is remembered and used
@param name identifier string used in cache file
@param store address to save pointer of liststore (or treestore?) holding any ached data
@param fillfunc function which creates store and adds any cached data
@param syncfunc function which converts store data to a list ready for cacheing, or NULL
@param syncdata data to be provided as second argument to @a syncfunc

@return
*/
void e2_cache_store_register (gchar *name, gpointer *store,
	void (*fillfunc) (gpointer*, GList*), GList*(*syncfunc) (gpointer, gpointer),
	gpointer syncdata)
{
	//any cached data will be a list. Try to get that
	GList *rowdata;
	E2_Cache *cache = e2_cache_list_register (name, &rowdata);

//	void (*func) (gpointer*, GList*) = fillfunc;
//	(*func) (store, rowdata);
	fillfunc (store, rowdata);

	e2_list_free_with_data (&rowdata);	//data stored, this now redundant
	//adjust parameters to suit subsequent cacheing
	cache->type = E2_CACHE_TYPE_STORE;
	cache->data = store;
	cache->sync_func = syncfunc;
	cache->sync_data = syncdata;
}
/**
@brief get data-list element corresponding to @a name from the cache list
The element will be created if it does not exist
Unlike other cache-register functions, this does not expect an argument
which is the default to use when no matching cache is found. Instead, it
sets the list pointer to NULL (the usual default is an empty list).
This approach minimises irrelevant creation of default lists.
Non-empty defaults need to be set after this function is called.
Cache data is the address of the store pointer, NOT the value of that pointer,
So @a list needs to be a known/constant address while the cache data is in
use, and any transient cache list needs to be un-registered before that
list's address is abandoned
@param name identifier string used in cache file
@param list pointer to list to store the cached values
@return pointer to the cache data struct, or NULL if error occurred
*/
E2_Cache *e2_cache_list_register (gchar *name, GList **list)
{
	E2_Cache *cache;
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		printd (DEBUG, "re-register list cache '%s'", name);
		cache = found->data;
		return cache;
	}
	cache = ALLOCATE (E2_Cache);
	CHECKALLOCATEDFATAL (cache)
	cache->type = E2_CACHE_TYPE_LIST;
	cache->sync_func = NULL;
	cache->sync_data = NULL;
	found = g_list_find_custom (unknown_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found == NULL)
	{
		printd (DEBUG, "list cache '%s' not found, creating NULL entry", name);
		cache->name = g_strdup (name);
		*list = NULL;
	}
	else
	{
		printd (DEBUG, "cache '%s' found", name);
		E2_Cache *unknown = found->data;
		cache->name = unknown->name;
		*list = (GList *) unknown->data; //the backup records list start, not its store
		unknown_list = g_list_remove_link (unknown_list, found);
		g_list_free (found);
		DEALLOCATE (E2_Cache, unknown);
	}
	cache->data = (gpointer *) list;
	cache_list = g_list_append (cache_list, cache);
	return cache;
}
/**
@brief register a cached array of integers
@param name identifier string used in cache list
@param size the number of elements in the array
@param values pointer to store for array of cached values, if any, or @a defs
@param defs array of default values to use if any are mission from cache
@return
*/
void e2_cache_array_register (gchar *name, guint size, gint *values, gint *defs)
{
	E2_Cache *cache;
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		printd (DEBUG, "re-register array cache '%s'", name);
		return;
	}
	//first-time processing
	cache = ALLOCATE (E2_Cache);
	CHECKALLOCATEDFATAL (cache)
	cache->type = E2_CACHE_TYPE_ARRAY;
	cache->sync_func = NULL;
//	cache->sync_data = NULL; set below, to size of array
	found = g_list_find_custom (unknown_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	gint *stores = values;
	guint i;
	if (found == NULL)
	{ //not in the queue, either
		printd (DEBUG, "array cache '%s' not found, creating default", name);
		cache->name = g_strdup (name);
		for (i = 0 ; i < size ; i++)
			*stores++ = *defs++;
	}
	else
	{  //process the queued item
		printd (DEBUG, "cache '%s' found", name);
		E2_Cache *unknown = found->data;
		cache->name = unknown->name;
		gchar *end = NULL;
		gchar *str = (gchar *) unknown->data;
		gchar **split = g_strsplit (str, ",", -1);
		if (g_strv_length (split) == size)
		{
			for (i = 0 ; split[i] != NULL ; i++)
			{
				*stores = g_ascii_strtoull (split[i], &end, 10);
				if (end == split[i])
				{
					printd (WARN, "array cache '%s' data conversion failed from '%s", name, split[i]);
					*stores = defs[i];
				}
				stores++;
			}
		}
		else
		{
			printd (WARN, "array cache '%s' wrong no. of value(s)", name);
			for (i = 0 ; i < size ; i++)
				*stores++ = *defs++;
		}
/*		if (i < size)
		{
			printd (WARN, "array cache '%s' missing value(s)", name);
			//set all values to default, in case earlier ones are missing
			stores = values;
			for (i = 0 ; i < size ; i++)
				*stores++ = defs[i];
			//append missing values
//			for (; i < size ; i++)
//				*stores++ = defs[i];
		} */
		g_strfreev (split);
		g_free (str);
		unknown_list = g_list_remove_link (unknown_list, found);
		g_list_free (found);
		DEALLOCATE (E2_Cache, unknown);
	}
	cache->data = (gpointer) values;
	cache->sync_data = GINT_TO_POINTER (size);
	cache_list = g_list_append (cache_list, cache);
	return;
}
#ifdef E2_VFS
/**
@brief helper func to recursively walk a list/tree store and convert all its
contents to strings
CHECKME is there another func that does this already e.g. for config backup
*/
static void _e2_cache_store_walk (GtkTreeModel *model, GtkTreeIter *iter, gint level, GList **stringlist)
{
	gchar *rowstring;
	GtkTreeIter child;
	do
	{
#ifdef E2_VFSTMP
//e2_tree_row_to_string() is currently parked in the vfs plugin
//		rowstring = e2_tree_row_to_string (model, &iter, -1, level);
#endif
		rowstring = g_strdup ("FIXME");
		*stringlist = g_list_append (*stringlist, rowstring);
		if (gtk_tree_model_iter_children (model, &child, iter))
			_e2_cache_store_walk (model, &child, level+1, stringlist);
	} while (gtk_tree_model_iter_next (model, iter));
}
#endif
/**
@brief remove an item from the cache list, and park in the unknown list
All data is copied. Any cleanup should be done by the caller, using a saved
pointer to the original cache->data (because that is changed here)
@param name identifier string used in cache list
@return
*/
void e2_cache_unregister (gchar *name)
{
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found == NULL)
	{
		printd (WARN, "trying to unregister cache '%s' which doesn't exist", name);
		return;
	}
	cache_list = g_list_remove_link (cache_list, found);
	unknown_list = g_list_concat (unknown_list, found);
	E2_Cache *cache = found->data;
	//revert cache data to cache-file string form
	switch (cache->type)
	{
		case E2_CACHE_TYPE_BOOL:
			cache->data = (gpointer) g_strdup ((*cache->data) ? "true" : "false");
			break;
		case E2_CACHE_TYPE_INT:
			cache->data = (gpointer) g_strdup_printf ("%d", *((gint *)cache->data));
			break;
		case E2_CACHE_TYPE_TIME:
			//time_t may be int or long int
			cache->data = (gpointer) g_strdup_printf ("%ld", *((glong *)cache->data));
			break;
		case E2_CACHE_TYPE_DOUBLE:
			cache->data = (gpointer) g_strdup_printf ("%f", *((gdouble *)cache->data));
			break;
		case E2_CACHE_TYPE_STR:
			//copy this, in case original cleared
			cache->data = (gpointer) g_strdup ((gchar *) cache->data);
			break;
#ifdef E2_VFS
//sofar, only cached store is for vfs data
		case E2_CACHE_TYPE_STORE:
			if (*(gpointer *)cache->data == NULL)	//make sure store still exists
				break;
			GList *stringlist;
			if (cache->sync_func != NULL)
			{	//custom conversion to list for backup
				GList* (*func) (gpointer, gpointer) = cache->sync_func;
				stringlist = func (*(gpointer *)cache->data, cache->sync_data);
			}
			else
			{
				stringlist = NULL;
				GtkTreeIter iter;
				GtkTreeModel *model = GTK_TREE_MODEL ((GtkListStore *)*cache->data);
				if (gtk_tree_model_get_iter_first (model, &iter))
					_e2_cache_store_walk (model, &iter, 0, &stringlist);
			}
			cache->data = (gpointer) stringlist; //backup records list start, not a pointer address
			break;
#endif
		case E2_CACHE_TYPE_LIST:
			if (cache->sync_func != NULL)
			{
				void (*func) (gpointer) = cache->sync_func;
				func (cache->sync_data);
			}
#ifndef E2_VFS
			GList *
#endif
			stringlist = NULL;
			if (cache->data != NULL && *cache->data != NULL)
			{
				//just steal the existing list
				stringlist = (GList *) *cache->data;
				*cache->data = NULL;	//no further use elsewhere
			}
			cache->data = (gpointer) stringlist;
			break;
		case E2_CACHE_TYPE_ARRAY:
			if (cache->sync_func != NULL)
			{
				void (*func) (gpointer) = cache->sync_func;
				func (cache->sync_data);
			}
			GString *merged = g_string_new ("");
			gint *values = (gint *) cache->data;
			gint i, j = GPOINTER_TO_INT (cache->sync_data);
			if (j > 0)
				g_string_append_printf (merged,"%d", *values++);
			for (i = 1 ; i < j ; i++)
				g_string_append_printf (merged,",%d", *values++);

			cache->data = (gpointer) merged->str;
			g_string_free (merged, FALSE);
			break;
//		case E2_CACHE_TYPE_LONG:
//		case E2_CACHE_TYPE_FLOAT:
//		case E2_CACHE_TYPE_TREE:
		default:
			break;
	}
}
/**
@brief write cache file to disk
File contents are saved in utf-8 encoding, not localised.
Expects BGL to be on/closed
@return
*/
void e2_cache_file_write (void)
{
	if (e2_cl_options.original)
		return;
#ifdef E2_VFS
	VPATH ddata;
	VPATH tdata;
#endif
	gchar *utfpath = g_build_filename
		(e2_cl_options.config_dir, default_cache_file, NULL);
	gchar *local = F_FILENAME_TO_LOCALE (utfpath);
	gchar *tempname = e2_utils_get_tempname (local);

	E2_FILE *f = e2_fs_open_writestream (tempname E2_ERR_NONE());

	if (f != NULL)
	{
		printd (DEBUG, "write cache file: %s", utfpath);
		GList *member;
		E2_Cache *cache;

		if (e2_fs_file_write (f,
			//first line is language-independent, for version checking
			"# "PROGNAME" (v "VERSION")\n\n") == 0)
				goto error_handler;
		if (e2_fs_file_write (f,
			_("%sThis file stores runtime configuration data for %s.\n"
			"%sThe file will be overwritten each time %s is shut down.\n\n"),
			 "# ", PROGNAME, "# ", PROGNAME) == 0)
				goto error_handler;
		if (cache_list != NULL)
		{
extern gint real_width;
extern gint real_height;
			//these data need special 'syncing' FIXME use array sync process for this
			e2_fileview_update_col_cachedata ();
			//do not cache current window size in fullscreen mode
			if (app.mainwindow_state & GDK_WINDOW_STATE_FULLSCREEN)
			{
				member = g_list_find_custom (cache_list, "window-width",
					(GCompareFunc) _e2_cache_str_compare);
				if (member != NULL)
				{
					cache = (E2_Cache *)member->data;
					*((gint *)cache->data) = real_width;
				}
				member = g_list_find_custom (cache_list, "window-height",
					(GCompareFunc) _e2_cache_str_compare);
				if (member != NULL)
				{
					cache = (E2_Cache *)member->data;
					*((gint *)cache->data) = real_height;
				}
			}

			for (member = cache_list; member != NULL; member = member->next)
			{
				cache = member->data;
				switch (cache->type)
				{
					case E2_CACHE_TYPE_BOOL:
						if (e2_fs_file_write (f, "%s=%s\n", cache->name,
							*cache->data ? "true" : "false") == 0)
								goto error_handler;
						break;
					case E2_CACHE_TYPE_INT:
						if (e2_fs_file_write (f, "%s=%d\n", cache->name, *((gint *)cache->data)) == 0)
							goto error_handler;
						break;
					case E2_CACHE_TYPE_TIME:
						//time_t may be int or long int
						if (e2_fs_file_write (f, "%s=%ld\n", cache->name, *((glong *)cache->data)) == 0)
							goto error_handler;
						break;
					case E2_CACHE_TYPE_DOUBLE:
						{
						gchar doubl[G_ASCII_DTOSTR_BUF_SIZE];
						g_ascii_dtostr (doubl, G_ASCII_DTOSTR_BUF_SIZE, *((gdouble *)cache->data));
						if (e2_fs_file_write (f, "%s=%s\n", cache->name, doubl) == 0)
							goto error_handler;
						}
						break;
					case E2_CACHE_TYPE_STR:
						if (e2_fs_file_write (f, "%s=%s\n", cache->name, (gchar *) *cache->data) == 0)
							goto error_handler;
						break;
#ifdef E2_VFS
//sofar, only cached store is for vfs data
					case E2_CACHE_TYPE_STORE:
						if (*(gpointer *)cache->data != NULL)	//make sure store still exists
						{
							GList *stringlist;
							if (cache->sync_func != NULL)
							{	//custom conversion to list for backup
								GList* (*fun) (gpointer, gpointer) = cache->sync_func;
								stringlist = fun (*(gpointer *)cache->data, cache->sync_data);
							}
							else
							{
								stringlist = NULL;
								GtkTreeIter iter;
								GtkTreeModel *model = GTK_TREE_MODEL ((GtkListStore *)*cache->data);
								if (gtk_tree_model_get_iter_first (model, &iter))
									_e2_cache_store_walk (model, &iter, 0, &stringlist);
							}
							if (stringlist != NULL)
							{
								if (e2_fs_file_write (f, "%s=<\n", cache->name) == 0)
									goto error_handler;
								GList *node;
								for (node = stringlist; node != NULL; node = node->next)
								{
									//escape anything that would seem like a tree-option start or end
									if (//*((gchar *)tmp->data) == '<'//ascii check is ok here
										//||
										*((gchar *)node->data) == '>')
											e2_fs_file_write (f, "\\");
									if (e2_fs_file_write (f, "%s\n", (gchar *) node->data) == 0)
										goto error_handler;
								}
								if (e2_fs_file_write (f, ">\n") == 0)
									goto error_handler;
									//CHECKME = leaks when this is done before session-end ??
							}
						}
						break;
#endif
					case E2_CACHE_TYPE_LIST:
						if (cache->sync_func != NULL)
						{
							void (*fun) (gpointer) = cache->sync_func;
							fun (cache->sync_data);
						}
						if ((cache->data != NULL) && (*cache->data != NULL))
						{
//							if (e2_fs_file_write (f, "<%s\n", cache->name) == 0)
							if (e2_fs_file_write (f, "%s=<\n", cache->name) == 0)
								goto error_handler;
							GList *node;
							for (node = (GList *) *cache->data; node != NULL; node = node->next)
							{
								//escape anything that would seem like a tree-option start or end
								if (node->data && //*((gchar *)tmp->data) == '<'//ascii check is ok here
									//||
									*((gchar *)node->data) == '>')
										e2_fs_file_write (f, "\\");
								if (e2_fs_file_write (f, "%s\n", (gchar *) node->data) == 0)
									goto error_handler;
							}
							if (e2_fs_file_write (f, ">\n") == 0)
								goto error_handler;
							//CHECKME = leaks when this is done before session-end ??
						}
						break;
					case E2_CACHE_TYPE_ARRAY:
						if (cache->sync_func != NULL)
						{
							void (*fun) (gpointer) = cache->sync_func;
							fun (cache->sync_data);	//sync_data = size of array
						}
						if (e2_fs_file_write (f, "%s=", cache->name) == 0)
							goto error_handler;
						gint *values = (gint *) cache->data;
						//j = array size - 1 - the last is handled separately
						gint i, j = GPOINTER_TO_INT (cache->sync_data) - 1;
						for (i = 0 ; i < j ; i++)
							if (e2_fs_file_write (f, "%d,", *values++) == 0)
								goto error_handler;
						if (e2_fs_file_write (f, "%d\n", *values) == 0)
							goto error_handler;
						break;
	//				case E2_CACHE_TYPE_LONG:
	//				case E2_CACHE_TYPE_FLOAT:
	//				case E2_CACHE_TYPE_TREE:
					default:
						printd (WARN, "don't know how to write '%s' to cache file",
							cache->name);
						break;
				}
			}
		}
		else
			printd (DEBUG, "no caches registered");
		for (member = unknown_list; member != NULL; member = member->next)
		{
			cache = member->data;
			switch (cache->type)
			{
				case E2_CACHE_TYPE_BOOL:
				case E2_CACHE_TYPE_INT:
				case E2_CACHE_TYPE_TIME:
				case E2_CACHE_TYPE_DOUBLE:
				case E2_CACHE_TYPE_STR:
				case E2_CACHE_TYPE_ARRAY:
					if (e2_fs_file_write (f, "%s=%s\n", cache->name,
						(gchar *) cache->data) == 0)
							goto error_handler;
					break;
				case E2_CACHE_TYPE_LIST:
				case E2_CACHE_TYPE_STORE:
					if (e2_fs_file_write (f, "%s=<\n", cache->name) == 0)
						goto error_handler;
					GList *node;
					for (node = (GList *) cache->data; node != NULL; node = node->next)
					{
						//escape anything that would seem like a tree-option start or end
						if (// *((gchar *)tmp->data) == '<'//ascii check is ok here
							//||
							*((gchar *)node->data) == '>')
								e2_fs_file_write (f, "\\");
						if (e2_fs_file_write (f, "%s\n", (gchar *) node->data) == 0)
							goto error_handler;
					}
					if (e2_fs_file_write (f, ">\n") == 0)
						goto error_handler;
					break;
//				case E2_CACHE_TYPE_LONG:
//				case E2_CACHE_TYPE_FLOAT:
//				case E2_CACHE_TYPE_TREE:
				default:
					printd (WARN, "don't know how to write '%s' to cache file",
						cache->name);
					break;
			}
		}

		//fs rename operation may be not reliably atomic (ext4)
		gint fdesc = fileno (f);
		if (fdesc == -1)
			goto error_handler;
		if (e2_fs_writeflush (fdesc) != 0)
			goto error_handler;

		e2_fs_close_stream (f);

#ifdef E2_VFS
		tdata.path = tempname;
		tdata.spacedata = NULL;
		ddata.path = local;
		ddata.spacedata = NULL;
#endif
		OPENBGL	//downstream error messages invoke local mutex management
#ifdef E2_VFS
		e2_task_backend_rename (&tdata, &ddata);
		e2_fs_chmod (&ddata, 0600 E2_ERR_NONE());
#else
		e2_task_backend_rename (tempname, local);
		e2_fs_chmod (local, 0600 E2_ERR_NONE());
#endif
		CLOSEBGL
		goto cleanup;
	}
error_handler:
	if (f != NULL)
	{
		e2_fs_close_stream (f);
		OPENBGL
#ifdef E2_VFS
		e2_task_backend_delete (&tdata);
#else
		e2_task_backend_delete (tempname);
#endif
		CLOSEBGL
	}
	gchar *msg = g_strdup_printf (_("Cannot write cache file %s - %s"),
		utfpath, g_strerror (errno));	//ok for native-only config file
	e2_output_print_error (msg, TRUE);
cleanup:
	g_free (utfpath);
	F_FREE (local, utfpath);
	g_free (tempname);
}
/**
@brief interpret a line of the cache file
The line content is assumed to be encoded in utf-8
For each new cache-item, an E2_Cache struct is created,
and added to list unknown_list, ready for later use
@param f pointer to the start of the string
@return
*/
static void _e2_cache_read_from_string (gchar *f[])
{
	gboolean list_mode = FALSE;
	GList *list = NULL;
	E2_Cache *cache = NULL;

	gint i = 0;
	gchar *line;

	while ((line = f[i++]) != NULL)
	{
		g_strchomp (line);
		//ignore empty lines and comments
		if (*line == '\0')
			continue;
		if (line[0] == '#')
			continue;
		//list values start with a line like name=<
		gchar *s = strrchr (line, '=');
		if (s != NULL && *(s+1) == '<' && *(s+2) == '\0')
		{
			list_mode = TRUE;
			list = NULL;
			cache = ALLOCATE (E2_Cache);
			CHECKALLOCATEDFATAL (cache)
			*s = '\0';
			cache->name = g_strdup (line);
			cache->type = E2_CACHE_TYPE_LIST;
			continue;
		}
		//deprecated format for list cache data - remove sometime ...
		if (line[0] == '<')
		{
			list_mode = TRUE;
			list = NULL;
			cache = ALLOCATE (E2_Cache);
			CHECKALLOCATEDFATAL (cache)
			cache->name = g_strdup (line + 1);
			cache->type = E2_CACHE_TYPE_LIST;
			continue;
		}

		if (line[0] == '>')
		{
			list_mode = FALSE;
			if (list == NULL)
				continue;
			cache->data = (gpointer *) list;
			unknown_list = g_list_append (unknown_list, cache);
			continue;
		}

		if (list_mode)
		{
			//cleanup any escaped action-names
			if (g_str_has_prefix (line, "\\>"))
				line++;
			list = g_list_append (list, g_strdup (line));
		}
		else
		{
			cache = ALLOCATE (E2_Cache);
			CHECKALLOCATEDFATAL (cache)
			gchar **split = g_strsplit (line, "=", 2);
			cache->name = g_strdup (split[0]);
			cache->data = (gpointer *) g_strdup (split[1]);
			cache->type = E2_CACHE_TYPE_STR; //initially, all items assumed to be strings
			g_strfreev (split);
			unknown_list = g_list_append (unknown_list, cache);
		}
	}
}
/**
@brief read cache file named @a fn from disk, and process its contents
@param fn name of cache file
@return
*/
static void _e2_cache_file_read (gchar *fn)
{
	//find absolute path to config file
	gchar *filepath = g_build_filename (e2_cl_options.config_dir, fn, NULL);
	gchar *localpath = F_FILENAME_TO_LOCALE (filepath);
#ifdef E2_VFS
	VPATH ddata = { localpath, NULL };	//only local cache file
#endif
	gpointer contents;
	//NOTE any error during this read must not print error message, as
	//config and output data are not yet known
	if (e2_fs_get_file_contents (
#ifdef E2_VFS
		&ddata,
#else
		localpath,
#endif
		&contents, NULL, TRUE E2_ERR_NONE()))
	{
		if (contents != NULL)
		{
			printd (DEBUG, "cache read from file '%s'", filepath);
			gchar **split = g_strsplit ((gchar *)contents, "\n", -1);
			_e2_cache_read_from_string (split);
			g_strfreev (split);
			g_free (contents);	//need free() if file buffer allocated by malloc()
		}
		else
		{
			printd (WARN, "empty cache file '%s'", filepath);
		}
	}
	else
	{
		printd (WARN, "could not open cache file '%s'", filepath);
	}

	g_free (filepath);
	F_FREE (localpath, filepath);
}
/**
@brief initialize the cache

This function will initilize the cache system of e2. It will automatically
be called at startup.
@param config_dir_ready TRUE when config dir found or created

@return
*/
void e2_cache_init (gboolean config_dir_ready)
{
	cache_list = NULL;
	unknown_list = NULL;
	default_cache_file = "cache"; //DO NOT TRANSLATE THIS
	if (config_dir_ready)
		_e2_cache_file_read (default_cache_file);
}
/**
@brief clean cache memory-allocations

@return
*/
void e2_cache_clean (void)
{
	GList *member;
	if (cache_list != NULL)
	{
		for (member = cache_list; member != NULL; member = member->next)
		{
			g_free (((E2_Cache*) member->data)->name);
			DEALLOCATE (E2_Cache, (E2_Cache*) member->data);
		}
		g_list_free (cache_list);
		cache_list = NULL;
	}

	if (unknown_list != NULL)
	{
		for (member = unknown_list; member != NULL; member = member->next)
		{
			g_free (((E2_Cache*) member->data)->name);
			DEALLOCATE (E2_Cache, (E2_Cache*) member->data);
		}
		g_list_free (unknown_list);
		unknown_list = NULL;
	}
}
/**
@brief clean one item from cache runtime data
This is used only to cleanup things during a version-upgrade
@param name the key of the item to clear

@return
*/
void e2_cache_clean1 (const gchar *name)
{
	GList *found = g_list_find_custom (cache_list, name,
		(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		g_free (((E2_Cache*) found->data)->name);
		if (((E2_Cache*) found->data)->type == E2_CACHE_TYPE_STR)
			g_free (((E2_Cache*) found->data)->data);	//other things will leak
		DEALLOCATE (E2_Cache, (E2_Cache*) found->data);
		cache_list = g_list_delete_link (cache_list, found);
	}
	found = g_list_find_custom (unknown_list, name,
			(GCompareFunc) _e2_cache_str_compare);
	if (found != NULL)
	{
		g_free (((E2_Cache*) found->data)->name);
		g_free (((E2_Cache*) found->data)->data);
		DEALLOCATE (E2_Cache, (E2_Cache*) found->data);
		unknown_list = g_list_delete_link (unknown_list, found);
	}
}
