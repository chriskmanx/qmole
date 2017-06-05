/* $Id: e2_option_color.c 2745 2013-09-19 22:55:24Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"
#include <string.h>
#include "e2_option.h"
#include "e2_option_color.h"

#ifdef E2_RAINBOW
/**
@brief setup heap space for a block of file-text color data

Allocates memory for ATOMSPERCHUNK GdkColor data structs and their associated
pointers.
Error messages assume BGL closed.

@return pointer to array of GdkColors, or NULL
*/
static GdkColor **_e2_option_color_create_chunk (void)
{
#ifdef USE_GLIB2_10
	gpointer slice = g_slice_alloc0 ((sizeof (GdkColor *) + sizeof (GdkColor)) * ATOMSPERCHUNK);
# if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (slice, return NULL;)
# else
	if (slice == NULL)
		return NULL;
# endif
	GdkColor **pointers = slice;
	//store pointers at front, colours at back
	GdkColor *thisptr = slice + sizeof (GdkColor *) * ATOMSPERCHUNK;
	gint i;
	for (i = 0; i < ATOMSPERCHUNK; i++)
		pointers[i] = thisptr++;
	app.colorchunks = g_list_append (app.colorchunks, slice);
	return pointers;
#else
	E2_ColorData *data = ALLOCATE (E2_ColorData);
# if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (data, return NULL;)
# else
	if (data == NULL)
		return NULL;
# endif
	//space for a bunch of pointers
	data->pointers = g_try_malloc (sizeof (gpointer) * ATOMSPERCHUNK);	//never freed
# if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (data->pointers, DEALLOCATE (E2_ColorData, data); return NULL;)
# else
	if (data->pointers == NULL)
	{
		DEALLOCATE (E2_ColorData, data);
		return NULL;
	}
# endif
	gint i;
	//a GMemChunk for the associated color structs
	//CHECKME do we need the chunk itself to keep track of freed space when
	//an extension is repeated, hence re-hashed ?
	data->chunk = g_mem_chunk_new
		(NULL, sizeof(GdkColor), sizeof(GdkColor)*ATOMSPERCHUNK, G_ALLOC_ONLY);
	//allocate all its atoms
	for (i = 0; i < ATOMSPERCHUNK; i++)
		data->pointers[i] = g_chunk_new (GdkColor, data->chunk);
	//remember the array and chunk addresses, for later use
	app.colorchunks = g_list_append (app.colorchunks, data);
	return (data->pointers);
#endif
}
/**
@brief cleanup all space/data for color data structs
@return
*/
static void _e2_option_color_clear_data (void)
{
	if (app.colors != NULL)
	{
		g_hash_table_destroy (app.colors);
		app.colors = NULL;
	}
	if (app.colorchunks != NULL)
	{
		GList *tmp;
		for (tmp = app.colorchunks; tmp != NULL; tmp = tmp->next)
		{
#ifdef USE_GLIB2_10
			g_slice_free1 (
				(sizeof (GdkColor *) + sizeof (GdkColor)) * ATOMSPERCHUNK,
				tmp->data);
#else
			E2_ColorData *data = (E2_ColorData *) tmp->data;
			g_mem_chunk_destroy (data->chunk);
			g_free (data->pointers);
			DEALLOCATE (E2_ColorData, data);
#endif
		}
		g_list_free (app.colorchunks);
		app.colorchunks = NULL;
	}
}
/**
@brief scan all filetypes in the config data, to parse any associated text-color

Filetype extensions are hashed in localised form, for faster comparison
when loading filelists
Any color string in the field adjacent to a catgory or extension is
converted to color data
Extension color will over-ride category color

@return
*/
void e2_option_color_filetypes_sync (void)
{
	E2_OptionSet *typeset = e2_option_get_simple ("filetypes");
	if (typeset == NULL)
		return;	//can't do anything

	_e2_option_color_clear_data ();
	app.colors = g_hash_table_new_full
		(g_str_hash, g_str_equal, g_free,
		NULL);	//data are chunked, and not worth individually clearing

	gint i = ATOMSPERCHUNK;	//force initial chunk creation
//#warning ignore compiler warning about unitialized usage of id
	GdkColor **id = NULL;	//assignment for complier-warning prevention only
	gchar *def_color;
	gboolean freedef;

	GtkTreeModel *mdl = typeset->ex.tree.model;
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (mdl, &iter))
	{
		gboolean anycase = e2_option_bool_get ("anycase-filetypes");
		do
		{
			GtkTreeIter iter2, iter3;	//for cfg levels
			gchar *node_label, *ext_type, *ext_color;
			//should always be level-2 child nodes, but test anyway
			if (gtk_tree_model_iter_children (mdl, &iter2, &iter))
			{
				do
				{	// extension or command loop = level 2
					gtk_tree_model_get (mdl, &iter2, 1, &node_label, -1);
					if (gtk_tree_model_iter_children (mdl, &iter3, &iter2))
					{
						if (!strcmp (node_label, _C(13)))  //extensions node found
						{
							//get category-default color, if any
							gtk_tree_model_get (mdl, &iter2, 2, &def_color, -1);
			 				//CHECKME better to dup the hashed string ?
							freedef = TRUE;	//assume the default will not be used

							do
							{
								//extension loop = level 3, process the extensions
								gtk_tree_model_get (mdl, &iter3, 1, &ext_type,
									2,&ext_color, -1);
								//ignore the pseudo-file-types & empty colors
								if (!g_str_has_prefix (ext_type, "<"))
								{
									gchar *usecolor, *local;
									if (*ext_color != '\0')
										usecolor = ext_color;
									else if (*def_color != '\0')
									{
										usecolor = def_color;
										freedef = FALSE;
									}
									else
										usecolor = NULL;
									if (usecolor != NULL)
									{
										if (i == ATOMSPERCHUNK)
										{	//full (or no) chunk, create another
											id = _e2_option_color_create_chunk ();
											i = 0;
										}
										gdk_color_parse (usecolor, id[i]);
										if (anycase)
											local = g_utf8_casefold (ext_type, -1);
										else
											local = F_FILENAME_TO_LOCALE (ext_type);
										if (local != ext_type)	//conversion really happened
											g_free (ext_type);
										//CHECKME do we want to cleanup any replaced color struct ?
										g_hash_table_replace (app.colors, local, id[i]);
										i++;
									}
								}
								else
								{	//clean this if not hashed
									g_free (ext_type);
								}
								g_free (ext_color);
							} while (gtk_tree_model_iter_next (mdl, &iter3));
							if (freedef)
							g_free (def_color);
						}
						else if (strcmp (node_label, _C(6)))  //commands node is ignored
						{ //OOPS
							printd (WARN, "un-recognised node in filetypes config");
						}
					}
					g_free (node_label); //just cleanup
				} while (gtk_tree_model_iter_next (mdl, &iter2));
			}
		} while (gtk_tree_model_iter_next (mdl, &iter));
	}
}
#endif

/**
@brief register new color option

register new color option.

@param name    name of the option, a constant string
@param group   group the option belongs to, used in config dialog, a r-t string  FREEME
@param desc    textual description of the option used in config dialog, a r-t _() string FREEME ?
@param tip     tooltip used when displaying the config dialog, a _() string
@param depends name of another option this one depends on, or NULL
@param value   value for the option
@param flags   bitflags determining how the option data is to be handled

@return  E2_OptionSet of the new option
*/
E2_OptionSet *e2_option_color_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gchar *value, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_register (E2_OPTION_TYPE_COLOR, name, group,
		desc, tip, depends, flags);
	set->ival = -1;
	if (gdk_color_parse (value, &set->ex.color.value))
		set->sval = g_strdup (value);
	else
	{
		set->sval = g_strdup ("#000000");
		gdk_color_parse (set->sval, &set->ex.color.value);
	}
	return set;
}

GdkColor *e2_option_color_get (const gchar *name)
{
	E2_OptionSet *set = e2_option_get (name);
	if (set == NULL)
		return NULL;
	if (set->type == E2_OPTION_TYPE_COLOR)
	{
		return &set->ex.color.value;
	}
	else
	{
		printd (WARN, "trying to get color option '%s', which isn't a color option", set->name);
		return NULL;
	}
}
#ifdef USE_GTK3_0
/**
@brief populate @a color with data of colour-option @a name

The alpha value of @a color is set to 1.0, the caller should change that if needed

@param name name of option
@param color pointer to data struct to be filled

@return TRUE if @a color is populated successfully
*/
gboolean e2_option_color_get_RGBA (const gchar *name, GdkRGBA *color)
{
	E2_OptionSet *set = e2_option_get (name);
	if (set == NULL)
		return FALSE;
	if (set->type == E2_OPTION_TYPE_COLOR)
	{
		color->red = (gdouble)set->ex.color.value.red / 65535;
		color->green = (gdouble)set->ex.color.value.green / 65535;
		color->blue = (gdouble)set->ex.color.value.blue / 65535;
		color->alpha = 1.0;
		return TRUE;
	}
	else
	{
		printd (WARN, "trying to get color option '%s', which isn't a color option", set->name);
		return FALSE;
	}
}
#endif

gboolean e2_option_color_set_str (gchar *name, gchar *value)
{
	E2_OptionSet *set = e2_option_get (name);
	if (set->type == E2_OPTION_TYPE_COLOR)
	{
		return e2_option_color_set_str_direct (set, value);
	}
	else
	{
		printd (WARN, "trying to set color option '%s', which isn't a color option", set->name);
		return FALSE;
	}
}

gboolean e2_option_color_set_str_direct (E2_OptionSet *set, gchar *value)
{
	if (gdk_color_parse (value, &set->ex.color.value))
	{
		g_free (set->sval);
		set->sval = g_strdup (value);
		return TRUE;
	}
	else
	{
		gdk_color_parse (set->sval, &set->ex.color.value);
		return FALSE;
	}
}
