/* $Id: e2_filetype.c 2725 2013-08-26 14:02:03Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_filetype.h"
#ifdef OLDFTDLG
#include "e2_config_dialog.h"
#else
#include "e2_dialog.h"
#endif

/* *
@brief get extensions array for filetype category @a category

Not much use - in the config data, the categories are user-editable !!
@param category i18n utf8 string containing category to be matched

@return newly-allocated, NULL-terminated string array of utf-8 extensions for @a category, or NULL if none found
*/
/*gchar **e2_filetype_get_extensions (gchar *category)
{
	E2_OptionSet *set;
	GtkTreeIter iter, iter2, iter3;  //3 tree levels to scan
	gchar *extension;
	GPtrArray *e_ray;

	if (
		((set = e2_option_get_simple ("filetypes")) != NULL) &&
		gtk_tree_model_get_iter_first (set->ex.tree.model, &iter)
		)
	{
		e_ray = g_ptr_array_sized_new (5);

		while (e2_tree_find_iter_from_str_simple (set->ex.tree.model, 0,
			category, &iter, FALSE))	//must do children check later, anyhow
		{	//category loop = level 1
			if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, &iter))
			{
				do
				{  // extension or command loop = level 2
					if (gtk_tree_model_iter_children (set->ex.tree.model, &iter3, &iter2))
					{
						gtk_tree_model_get (set->ex.tree.model, &iter2, 1, &extension, -1);
						if (!strcmp (extension, _C(13)))  //extensions node found
						{
							g_free (extension);
							do
							{  //extension loop = level 3, build the extensions array
								gtk_tree_model_get (set->ex.tree.model, &iter3,
									1, &extension, -1);
								g_ptr_array_add (e_ray, extension);
							} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter3));
						}
						else
							g_free (extension); //not an extensions branch, just cleanup
					}

				} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter2));
			}
		}

		if (e_ray->len > 0)
		{
			g_ptr_array_add (e_ray, NULL);	//add end marker (also to support g_strfreev)
			return ((gchar **)g_ptr_array_free (e_ray, FALSE));
		}
		g_ptr_array_free (e_ray, TRUE);
	}
	printd (WARN, "no config filetype information for %s", category);
	return NULL;
} */
/**
@brief helper function to get actions list for specified file extension without regard to string case
@param key an extension logged in the filetypes hash table
@param value UNUSED
@param ext the filetype extension being souht

@return TRUE when a match is found
*/
gboolean _e2_filetype_find_any (gpointer key, gpointer value, gchar *ext)
{
	return (e2_utf8_caseless_match ((gchar *)key, ext, -1, -1));
}
/**
@brief get actions 'list' for specified file extension

Returns the actions array (in the file types hash) for the extension.
The returned array must NOT be altered or freed.
Used only by the context-menu creator.

@param ext utf8 string containing file extension to be matched

@return null-terminated string array of actions for @a ext, or NULL if none found
*/
const gchar **e2_filetype_get_actions (gchar *ext)
{
	const gchar **actions;

	if ( !strcmp (ext, _("<directory>"))
	  || !strcmp (ext, _("<executable>"))
	  || !strcmp (ext, _("<none>"))
	  || (!e2_option_bool_get ("anycase-filetypes") && e2_fileview_is_cased (curr_view)))
		actions = (const gchar **) g_hash_table_lookup (app.filetypes, ext);
	else
		actions = (const gchar **) g_hash_table_find (app.filetypes,
						(GHRFunc)_e2_filetype_find_any, ext);

	if (!(actions == NULL || *actions == '\0'))
		return actions;
	else
		return NULL;
}
/**
@brief get default action for specified file extension

Allocates and returns the first action from the actions array (in the file types
hash) for the specified extension.
The returned string needs to be freed.
Used only by callback open_cb.

@param ext utf8 string containing extension to be matched

* @return newly-allocated string which is the default action for ext, or NULL if none found
*/
gchar *e2_filetype_get_default_action (gchar *ext)
{
	const gchar **actions = e2_filetype_get_actions (ext);
	if (!(actions == NULL || *actions == '\0'))
		return g_strdup (*actions);
	else
		return NULL;
}
/**
@brief load all filetype info into hash

Converts config tree data into filetype runtime data hash by means of a
depth-first search of 3-level tree (graph)
For each 'category' of filetypes, the order of level-2 'commands' and 'extensions'
nodes is irrelevant, and multiple 'commands' and/or 'extensions' nodes are allowed.
The hash entry for each file extension is an arrray of command strings.
If a (level-3) command has a separate menu name, the command string is stored as
menuname"@"commandstring (so menu names can't have a @ in them)
NB in the previous line, the "" around the @ are to fix doxygen, the string
doesn't really have them

@return
*/
void e2_filetype_add_all (void)
{
	E2_OptionSet *set;
	GtkTreeIter iter, iter2, iter3;  //3 tree levels to scan
	gchar *category, *extension, *cmd_string;
	GPtrArray *e_ray, *c_ray;

	app.filetypes = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	app.typelist = NULL;

	if (
		((set = e2_option_get_simple ("filetypes")) != NULL) &&
		gtk_tree_model_get_iter_first (set->ex.tree.model, &iter)
		)
	{
		e_ray = g_ptr_array_sized_new (5);
		do
		{  //category loop = level 1
			//re-use the extensions array
			if (e_ray->len > 0)
				g_ptr_array_remove_range (e_ray, 0, e_ray->len);
			//setup a fresh commands  array for each category
			c_ray = g_ptr_array_sized_new (5);
			//note the category, in case error msg is needed
			gtk_tree_model_get (set->ex.tree.model, &iter, 0, &category, -1);
			if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, &iter))
			{
				do
				{  // extension or command loop = level 2
					gtk_tree_model_get (set->ex.tree.model, &iter2, 1, &extension, -1);
					if (gtk_tree_model_iter_children (set->ex.tree.model, &iter3, &iter2))
					{
						if (!strcmp (extension, _C(13)))  //extensions node found
						{
							g_free (extension);
							do
							{  //extension loop = level 3, build the extensions array
								gtk_tree_model_get (set->ex.tree.model, &iter3, 1, &extension,
									 -1);
								g_ptr_array_add (e_ray, extension);
							} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter3));
						}
						else if (!strcmp (extension, _C(6)))  //commands node found
						{
							g_free (extension);
							do
							{  //commands loop also = level 3, build the commands array
								//this time, extension is really a command name
								gtk_tree_model_get (set->ex.tree.model, &iter3, 1, &extension,
									2, &cmd_string, -1);
								if (*cmd_string != '\0')
								{  //concatenate with separator
									gchar *freeme = extension;
									extension = g_strconcat (extension, "@", cmd_string, NULL);
									g_free (freeme);
								}
								g_free (cmd_string);
								g_ptr_array_add (c_ray, extension);
							} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter3));
						}
						else
						{ //OOPS
							printd (WARN, "un-recognised node in filetypes config for %s", category);
						}
					}
					else
						g_free (extension); //no children, just cleanup

				} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter2));

				g_ptr_array_add (c_ray, NULL);	//add end marker (for detecting end, also to support g_strfreev)
				//register the new filetype(s)
				guint i;
				gchar **iterator = (gchar **) e_ray->pdata;
				for (i = 0; i < e_ray->len; i++, iterator++)
					g_hash_table_insert (app.filetypes, *iterator, c_ray->pdata);
				//one copy of the commands array, so it can't be freed when hash is destroyed,
				//so note where it is, for separate cleanup purposes
				app.typelist = g_slist_append (app.typelist, c_ray->pdata);
				g_ptr_array_free (c_ray, FALSE);  //cleanup, ready for next category
			}
			g_free (category);
		} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter));

		g_ptr_array_free (e_ray, FALSE);  //no need to free any strings, that is done when they are added to hash

	} else
	{
		printd (WARN, "no config filetype information");
	}
}
/**
@brief run filetype action on selected items when %f missing

This is like a regular command, but if there is no %f in the action given, it
appends the selected item(s) to the end of the action string.
This is so the user can just enter like "xmms" as an action and not have to
remember to append the %f

@param action action/command to be run, utf8 string

@return
*/
void e2_filetype_exec_action (const gchar *action)
{
	gchar *s = g_strdup (action);
	e2_command_run (s, E2_COMMAND_RANGE_FILE_ACTION, curr_view->treeview
#ifdef E2_COMMANDQ
	, FALSE
#endif
	);
	g_free (s);
}
/**
@brief change filetype config data for a selected item

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if something was selected in the active pane
*/
static gboolean _e2_filetype_config (gpointer from, E2_ActionRuntime *art)
{
	//find current filetype
	FileInfo *info = e2_fileview_get_selected_first_local (curr_view, FALSE);
	if (info == NULL)
		return FALSE;

	gchar *infopath, *ext;
	infopath = F_FILENAME_TO_LOCALE (curr_view->dir);
#ifdef E2_VFS
	VPATH sdata = { infopath, curr_view->spacedata };
	if (e2_fs_is_dir (&sdata, info))
#else
	if (e2_fs_is_dir (infopath, info))
#endif
		ext =  _("<directory>");
#ifdef E2_VFS
	else if (e2_fs_is_executable (&sdata, info))
#else
	else if (e2_fs_is_executable (infopath, info))
#endif
		ext = _("<executable>");
	else
	{
		//maybe extra '.'(s) in the name eg version no so ext needs to be reparsed
		ext = strchr (info->filename, '.');	//localised text, assumes '.' is ascii
		if (ext == NULL || *(ext+1) == '\0')
			ext = _("<none>");
		else
			ext++;
	}
	F_FREE (infopath, curr_view->dir);
	
	//open the config dialog at the appropriate place
	gchar *utf = F_FILENAME_FROM_LOCALE (ext);	//DISPLAY ??
	e2_filetype_config_show (utf);
	F_FREE (utf, ext);
	return TRUE;
}
/**
@brief open filetypes config dialog

This may be used in a callback from the filetype dialog,
in which case @a extension == NULL
OR this may be called with a specific extension,
in which case @a extension != NULL

@param extension pointer to extension to change, or NULL

@return
*/
void e2_filetype_config_show (gchar *extension)
{
/*#ifdef OLDFTDLG
	//create dialog, but don't show it yet
	E2_SpecificConfDialogRuntime *rt =
	e2_config_dialog_single ("filetypes", e2_filetype_apply_allnew, FALSE); //internal name, no translation
	if (rt == NULL)
		HANDLE ERROR
	//get path to filetype's node
	GtkTreeModel *model = rt->set->ex.tree.model;
	GtkTreeIter iter;
	if (!gtk_tree_model_get_iter_first (model, &iter))
	{
		printd (WARN, "e2_filetype_config_show: can't find config data for filetypes");
		gtk_widget_destroy (rt->dialog);
		DEALLOCATE (E2_SpecificConfDialogRuntime, rt);
		return;
	}
	GtkTreeView *treeview = GTK_TREE_VIEW (rt->set->widget);

	gboolean found = FALSE;
	if (extension != NULL && *extension != '\0')
	{
		//if this is a specific-extension edit, find it in the tree
		gchar *tmp = extension;
		while (!found)
		{
			found = e2_tree_find_iter_from_str_simple (model, 1, tmp,
				&iter, FALSE);
			//make sure there's no 'smaller' valid extension
			if (!found)
			{
				tmp = strchr (tmp, '.');	//always ascii
				if (tmp == NULL)
					break;
				tmp++;	//pass the '.'
				gtk_tree_model_get_iter_first (model, &iter);
			}
		}
	}

	gtk_tree_view_collapse_all (treeview);	//this fn causes dialog window to be shown !
	if (found)
	{
		//path to extension
		GtkTreePath *path = gtk_tree_model_get_path (model, &iter);
		GtkTreePath *path2 = gtk_tree_path_copy (path);
		gtk_tree_path_up (path2);	//extensions node
		gtk_tree_path_up (path2);	//category node
		//show it
//		gtk_tree_view_expand_to_path (treeview, path);
		gtk_tree_view_expand_row (treeview, path2, TRUE);
		GtkTreeViewColumn *col = gtk_tree_view_get_column (treeview, 0);
		gtk_tree_view_set_cursor  (treeview, path, col, FALSE);
		gtk_tree_view_scroll_to_cell (treeview, path, col, TRUE, 0.5, 0.5);
		gtk_tree_path_free (path);
		gtk_tree_path_free (path2);
	}
//	else
//	{
		//non-recognised extension, just show all the categories
//		gtk_tree_view_collapse_all (treeview);
//		gtk_tree_view_expand_row (treeview, path, FALSE);
//	}
	//go there
//	GtkTreeViewColumn *col = gtk_tree_view_get_column (treeview, 0);
//	gtk_tree_view_set_cursor  (treeview, path, col, FALSE);
//	gtk_tree_view_scroll_to_cell (treeview, path, col, TRUE, 0.5, 0.5);
//	gtk_tree_path_free (path);

	//now we're ready to show this dialog
//	gtk_widget_show_all (rt->dialog);
	gtk_widget_show_all (rt->set->widget);
#else
	//use new form of dialog
*/
	E2_OptionSet *set = e2_option_get_simple ("filetypes");
	if (set == NULL)
	{
		printd (WARN, "e2_filetype_config_show: can't find config data for filetypes");
		return;
	}
	//get filetype's node, if we can
	GtkTreeModel *model = set->ex.tree.model;
	GtkTreeIter iter;
	if (!gtk_tree_model_get_iter_first (model, &iter))
	{
		printd (WARN, "e2_filetype_config_show: can't find config data for filetypes");
		return;
	}
	gboolean found = FALSE;
	gchar *category = NULL;
	if (extension != NULL && *extension != '\0')
	{
		//if this is a specific-extension edit, find it in the tree
		gchar *tmp = extension;
		while (!found)
		{
			found = e2_tree_find_iter_from_str_simple (model, 1, tmp,
				&iter, FALSE);
			if (found)
			{
				GtkTreeIter iter2;
				gtk_tree_model_iter_parent (model, &iter2, &iter);	//extensions node
				gtk_tree_model_iter_parent (model, &iter, &iter2);	//category node
				gtk_tree_model_get (model, &iter, 0, &category, -1);
				break;
			}
			else //make sure there's no 'smaller' valid extension
			{
				tmp = strchr (tmp, '.');	//always ascii
				if (tmp == NULL)
					break;
				tmp++;	//pass the '.'
				gtk_tree_model_get_iter_first (model, &iter);
			}
		}
	}

	e2_filetype_dialog_edit_create (category);
	if (category != NULL)
		g_free (category);
//#endif
}
/**
@brief update filetypes information

This is the 'apply' fn for the edit filetypes dialog, also used in
general config dialog
The entries in app.typelist are NULL-terminated string arrays,
but are created as pointer arrays, not with g_strsplit

@return
*/
void e2_filetype_apply_allnew (void)
{
	g_hash_table_destroy (app.filetypes);
	g_slist_foreach (app.typelist, (GFunc) g_strfreev, NULL);
	g_slist_free (app.typelist);
	e2_filetype_add_all ();
}
/**
@brief register filetype action data

@return
*/
void e2_filetype_actions_register (void)
{
	E2_Action action =
	{g_strconcat(_A(3),".",_A(48),NULL),_e2_filetype_config, FALSE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL};
	e2_action_register (&action);
}
/**
@brief install default tree options for filetypes
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_filetype_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup ("filetypes=<"),
	g_strconcat (_("directories"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),	// _("extensions")
			g_strconcat ("\t\t|",_("<directory>"),"|",NULL), //_I(
		g_strconcat ("\t|",_C(6),"|",NULL),  // _("commands")
			g_strconcat ("\t\t|",_("_Open"),"|cd %%f",NULL),	//must be no quotes in path!
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(68),NULL), //<
			g_strconcat ("\t\t|",_("_Mount"),"|mount %p",NULL),	//CHECKME quotes in path ok?
			g_strconcat ("\t\t|",_("_Unmount"),"|umount %p",NULL),
	g_strconcat (_("executables"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strconcat ("\t\t|",_("<executable>"),"|",NULL),	//_I(
			g_strconcat ("\t\t|",_("<none>"),"|",NULL),	//_I( //no extension
			g_strdup ("\t\t|sh|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Run"),"|%p",NULL),
//			g_strconcat ("\t\t|",_("Edit with _vi"),"|x vi",NULL),
//			g_strconcat ("\t\t|",_("Edit with _emacs"),"|x emacs",NULL),
			g_strconcat ("\t\t|",_("Edit _with.."),"|%{(editors)@",_("Editor command:"),"} %p",NULL),
			g_strconcat ("\t\t|",_("_Edit"),"|",_A(6),".",_A(46),NULL), //_("file.edit") //<
	g_strconcat (_("office documents"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|doc|"),
			g_strdup ("\t\t|odt|"),
			g_strdup ("\t\t|sxw|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Openoffice"),"|oowriter",NULL),
	g_strconcat (_("HTML documents"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|htm|"),
			g_strdup ("\t\t|html|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Firefox"),"|mozilla-firefox %$file://$%p",NULL),
			g_strconcat ("\t\t|",_("_Mozilla"),"|mozilla %$file://$%p",NULL),
			g_strconcat ("\t\t|",_("_Lynx"),"|x lynx",NULL),
			g_strconcat ("\t\t|",_("_Opera"),"|opera %p",NULL),
	g_strconcat (_("PDF documents"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|pdf|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strdup ("\t\t|evince|evince"),
			g_strdup ("\t\t|xpdf|xpdf"),
			g_strdup ("\t\t|acroread|acroread"),
	g_strconcat (_("postscript documents"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|ps|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strdup ("\t\t|gv|gv"),
	g_strconcat (_("text documents"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|txt|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
//not loaded	g_strconcat ("\t\t|",PLUGIN,":",_("View"),"|",_A(6),".",_("view"),NULL),  //conformed name
			g_strconcat ("\t\t|",_("View"),"|",_A(6),".",_A(109),NULL), //_("file.view") //<
			g_strconcat ("\t\t|",_("_Edit"),"|",_A(6),".",_A(46),NULL), //_("file.edit") //<
//this removed in favour of the default context menu item open-with
//			g_strconcat ("\t\t|",_("Edit _with.."),"|%{(editors)@",_("Editor command:"),"} %p",NULL),
//			g_strconcat ("\t\t|",_("Edit with vi"),"|x vi",NULL),
	g_strconcat (_("spreadsheets"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|xls|"),
			g_strdup ("\t\t|ods|"),
			g_strdup ("\t\t|sxc|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Openoffice"),"|oocalc",NULL),
	g_strconcat (_("audio files"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|mp3|"),
			g_strdup ("\t\t|m3u|"),
			g_strdup ("\t\t|wav|"),
			g_strdup ("\t\t|ogg|"),
			g_strdup ("\t\t|mp2|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strdup ("\t\t|_Muine|muine"),
			g_strdup ("\t\t|_Xmms|xmms"),
			g_strdup ("\t\t|xmms _queue|xmms -e"),
	g_strconcat (_("image files"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|jpeg|"),
			g_strdup ("\t\t|jpg|"),
			g_strdup ("\t\t|gif|"),
			g_strdup ("\t\t|svg|"),
			g_strdup ("\t\t|png|"),
			g_strdup ("\t\t|xpm|"),
//			g_strdup ("\t\t|sxd|"),
//			g_strdup ("\t\t|sxi|"),
//			g_strdup ("\t\t|sxp|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strdup ("\t\t|_gview|gview"),
			g_strdup ("\t\t|gee_qie|geeqie"),
			g_strdup ("\t\t|_viewnior|viewnior"),
//			g_strdup ("\t\t|G_Qview|gqview"),
//			g_strdup ("\t\t|GImage_View|gimageview"),
//			g_strconcat ("\t\t|",_("_Oo impress"),"|ooimpress",NULL),
//			g_strconcat ("\t\t|",_("Oo _draw"),"|oodraw",NULL),
			g_strdup ("\t\t|gi_mp|gimp"),
			g_strdup ("\t\t|i_nkscape|inkscape"),
			g_strdup ("\t\t|_xv|xv"),
	g_strconcat (_("video files"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|avi|"),
			g_strdup ("\t\t|divx|"),
			g_strdup ("\t\t|mp4|"),
			g_strdup ("\t\t|mpg|"),
			g_strdup ("\t\t|mpeg|"),
			g_strdup ("\t\t|mov|"),
			g_strdup ("\t\t|mkv|"),
			g_strdup ("\t\t|wmv|"),
			g_strdup ("\t\t|webm|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strdup ("\t\t|_Totem|totem"),
			g_strdup ("\t\t|_mplayer|mplayer"),
			g_strdup ("\t\t|_xine|xine"),
	g_strconcat (_("plain tarballs"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|tar|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("Unpack"),"|tar -xf %f",NULL),
			g_strconcat ("\t\t|",_("Unpack in other pane"),"|",_A(14),".",_A(78),";x tar -C %D -xf %f;",_A(14),".",_A(77),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),  //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|>tar -tf %f \\| less",NULL),
	g_strconcat (_("gzip tarballs"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|tar.gz|"),
			g_strdup ("\t\t|tgz|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Unpack"),"|tar -zxf  %f",NULL),
			g_strconcat ("\t\t|",_("Unpack in _other pane"),"|",_A(14),".",_A(78),";tar -C %D -zxf %f;",_A(14),".",_A(77),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),   //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|>tar -ztf %f \\| less",NULL),
	g_strconcat (_("bzip2 tarballs"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|tar.bz2|"),
			g_strdup ("\t\t|tbz2|"),
			g_strdup ("\t\t|tbz|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Unpack"),"|>bzip2 -d -c %f \\| tar -xf -",NULL),
			g_strconcat ("\t\t|",_("Unpack in _other pane"),"|",_A(14),".",_A(78),";>bzip2 -d -c %f \\| tar -C %D -xf -;",_A(14),".",_A(77),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),    //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|>bzip2 -d -c %f \\| tar -tf - \\| less",NULL),
	g_strconcat (_("lzma tarballs"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|tar.lzma|"),
			g_strdup ("\t\t|tlz|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Unpack"),"|>lzma -d -c %f \\| tar -xf -",NULL),
			g_strconcat ("\t\t|",_("Unpack in _other pane"),"|",_A(14),".",_A(78),";>lzma -d -c %f \\| tar -C %D -xf -;",_A(14),".",_A(77),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),    //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|>lzma -d -c %f \\| tar -tf - \\| less",NULL),
	g_strconcat (_("xz tarballs"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|tar.xz|"),
			g_strdup ("\t\t|txz|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Unpack"),"|>xz -d -c %f \\| tar -xf -",NULL),
			g_strconcat ("\t\t|",_("Unpack in _other pane"),"|",_A(14),".",_A(78),";>xz -d -c %f \\| tar -C %D -xf -;",_A(14),".",_A(77),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),    //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|>xz -d -c %f \\| tar -tf - \\| less",NULL),
	g_strconcat (_("7z archives"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|7z|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Unpack"),"|7za x %f",NULL),
			g_strconcat ("\t\t|",_("Unpack in _other pane"),"|",_A(14),".",_A(78),";7za x -o%D %f;",_A(14),".",_A(77),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),    //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|7za l %f",NULL),
	g_strconcat (_("zip archives"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|zip|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_Unzip"),"|unzip %f",NULL),
			g_strconcat ("\t\t|",_("Unzip in _other pane"),"|",_A(14),".",_A(98),";unzip %P;",_A(14),".",_A(98),NULL), //<*2
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),    //conformed name
#endif
			g_strconcat ("\t\t|",_("_List contents"),"|>unzip -l %f \\| less",NULL),
/* plugin functionality not tested yet, for these
	g_strconcat (_("arj archives"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|arj|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open")"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),
#endif
	g_strconcat (_("rar archives"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|rar|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open")"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#else
			g_strconcat ("\t\t|",PLUGIN,":",_("Unpack"),"|",_A(6),".",_A(107),NULL),
#endif
*/
	g_strconcat (_("RPM packages"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|rpm|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("In_formation"),"|rpm -qlip %f",NULL),
			g_strconcat ("\t\t|",_("_Install"),"|su rpm -Uvh %f",NULL),
#ifdef E2_VFS
			g_strconcat ("\t\t|",_("_Open"),"|",_A(6),".",_A(107),NULL),
			g_strconcat ("\t\t|",_("O_pen in other"),"|",_A(6),".",_A(126),NULL),
#endif
	g_strconcat (_("source code files"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|c|"),
			g_strdup ("\t\t|cpp|"),
			g_strdup ("\t\t|h|"),
			g_strdup ("\t\t|pl|"),
			g_strdup ("\t\t|java|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("Edit _with.."),"|%{(editors)@",_("Editor command:"),"} %p",NULL),
//			g_strconcat ("\t\t|",_("Edit with _vi"),"|x vi",NULL),
//			g_strconcat ("\t\t|",_("Edit with _emacs"),"|x emacs",NULL),
			g_strconcat ("\t\t|",_("_Edit"),"|",_A(6),".",_A(46),NULL), //_("file.edit") //<
	g_strconcat (_("object files"),"||",NULL),
		g_strconcat ("\t|",_C(13),"|",NULL),
			g_strdup ("\t\t|o|"),
			g_strdup ("\t\t|so|"),
			g_strdup ("\t\t|a|"),
		g_strconcat ("\t|",_C(6),"|",NULL),
			g_strconcat ("\t\t|",_("_View symbols"),"|>nm %f \\| less",NULL),
	g_strdup (">"),
	NULL);
}
/**
@brief register filetype config data

Ths sets up for the corresponding page in the config dialog
and prepares the default tree of filetype config data
The format of the data needs to conform with the 3-level
interrogation scheme used in e2_filetype_add_all().

@return
*/
void e2_filetype_options_register (void)
{
	//no screen rebuilds needed after any change to this option
	gchar *group_name = _C(15); //_("filetypes")
	E2_OptionSet *set = e2_option_tree_register ("filetypes", group_name,
		group_name, NULL, NULL, NULL,
		E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED
#ifdef E2_RAINBOW
		| E2_OPTION_FLAG_BUILDPANES //may be a color-data change
#endif
		| E2_OPTION_FLAG_BUILDFILES);
	e2_option_tree_add_column (set, _("Category"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	//this column used for headings, extensions and command labels
	e2_option_tree_add_column (set, "", E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Command"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, NULL, GINT_TO_POINTER (E2_ACTION_EXCLUDE_LAYOUT | E2_ACTION_INCLUDE_FILES));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_filetype_tree_defaults);

	group_name = g_strconcat(_C(15) ,".",_C(27),":",_C(26),NULL);  //_("filetypes.options:miscellaneous"   //FREEME group_name not here !
	e2_option_bool_register ("anycase-filetypes", group_name, _("case-insensitive filetypes"),
		_("This causes text-case to always be ignored when matching a file-extension"), NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREEGROUP ); //no rebuild
}
