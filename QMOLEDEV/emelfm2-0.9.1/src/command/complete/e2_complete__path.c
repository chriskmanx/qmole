/* $Id: e2_complete__path.c 2742 2013-09-11 00:37:42Z tpgww $

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

#include "e2_complete.h"
#include <string.h>

/**
@brief dir-foreach callback func
This is called with BGL open/off, but no UI-change here (or downstream?)
@param parent absolute path of dir being processed, localised string
@param itemname name of discovered item
@param entries pointer to store for list of data items for @a parent
@param user_data pointer to data specified when the foreach was called, with members ->word and ->prefix

@return TRUE to signal the read has not been aborted
*/
static gboolean _e2_complete_path_drcb_check_dir (VPATH *parent,
	const gchar *itemname, GList **found, E2_Duo *user_data)
{
	gchar *utf, *localpath, *temp;
	gboolean is_exec, is_dir;

	utf = F_FILENAME_FROM_LOCALE (itemname);	//not DISPLAY
	if (g_str_has_prefix (utf, user_data->a))	//word
	{
		localpath = g_build_filename (VPCSTR (parent), itemname, NULL);
#ifdef E2_VFS
		VPATH ddata = { localpath, parent->spacedata };
		is_exec = ! e2_fs_access (&ddata, X_OK E2_ERR_NONE());
		is_dir = e2_fs_is_dir3 (&ddata E2_ERR_NONE());
#else
		is_exec = ! e2_fs_access (localpath, X_OK E2_ERR_NONE());
		is_dir = e2_fs_is_dir3 (localpath E2_ERR_NONE());
#endif
		g_free (localpath);
		if (is_exec || is_dir)
		{
			printd (DEBUG, "match: %s - %s", utf, user_data->a);
			if (user_data->b == NULL)	//prefix
			{
				temp = (is_dir) ?
					g_strconcat (utf, G_DIR_SEPARATOR_S, NULL):
					g_strdup (utf);
			}
			else
			{
				temp = (is_dir) ?
					g_strconcat (user_data->b, utf, G_DIR_SEPARATOR_S, NULL):
					g_strconcat (user_data->b, utf, NULL);
			}
			*found = g_list_append (*found, temp);
		}
	}
	F_FREE (utf, itemname);
	return TRUE;
}
/**
@brief add the sub-dirs and executables in @a localpath that complete @a word, to list @a found
@a localpath may be the active-pane dir or a descendant of that (of any sort)
or a native dir in $PATH
Downstream code expects BGL to be closed on arrival here
@param localpath path of directory to scan, localised string, assumed no trailing /
@param word the text to be completed, utf8 string
@param prefix utf8 string to be prepended to each matching item, or NULL
@param found store of list to which matching items will be appended

@return the number of successful matches
*/
static guint _e2_complete_path_check_dir (VPATH *localpath, gchar *word, gchar *prefix,
	GList **found)
{
	printd (DEBUG, "check_dir (dir:%s,word:%s,found:_,prefix:%s)", localpath, word, prefix);
	E2_Duo pair = { word, prefix };

	OPENBGL
	GList *checks = (GList *) e2_fs_dir_foreach (localpath,
			E2_DIRWATCH_CHECK,	//this is irrelevant for non-local dirs
			_e2_complete_path_drcb_check_dir, &pair, NULL E2_ERR_NONE());
	CLOSEBGL

	if (E2DREAD_FAILED (checks))
		return 0;
	if (*found == NULL)
		*found = checks;
	else
		*found = g_list_concat (*found, checks);
	return (g_list_length (checks));
}
/**
@brief create list of executable items that are valid completions of @a line

The string to be completed extends from after the ' ' (if any) preceding
the cursor position @a pos, to that position. But if @a pos = 0, the string
will be empty
If the string starts with "./", the scanning occurs in the active directory
Otherwise the scan iterates over $PATH.

@param line utf-8 string containing the whole line to be completed
@param word utf-8 string, copy of part of @a line with the 'word' to be completed
@param pos 0-based index of cursor position (characters, not bytes) in @a line
@param found pointer to list to record matching items, must be initialised by caller
@param flags pointer to bit-flags indicating type of match (dirs or files)
@param data discretionary data pointer for this method
@param unused parameter needed for other complete funcs

@return the number of successful matches
*/
guint e2_complete_path (gchar *line, gchar *word, gint pos, GList **found,
	E2_CompleteFlags *flags, gpointer data, guint unused)
{
	printd (DEBUG, "complete_path (line:%s,word:%s,pos:%d,found:_,flags:%d,data:_)",
		line, word, pos, *flags);

	if ((word == NULL) || (*word == '\0'))
		return 0;
/*
WRONG ! //the word to complete needs to be the whole line
	//(i.e. no ' ' in the line prior to the cursor position)
	//or else we can't match a path
	if (strcmp (word, line))
	//error message in this case?
		return 0;
*/
	guint ret;
#ifdef E2_VFS
	VPATH ddata;
#endif

	if (strchr (word, G_DIR_SEPARATOR) != NULL) //string-to-complete has path separator
	{
		gchar *p1, *p2, *localpath;
		p1 = strrchr (word, G_DIR_SEPARATOR) + sizeof(gchar);	//the 'real' word to complete
		p2 = g_strndup (word, p1 - word);	//prefix for matches
		if (word[0] == G_DIR_SEPARATOR)
		{	//word is an absolute path string
			localpath = F_FILENAME_TO_LOCALE (p2);
#ifdef E2_VFS
			ddata.path = localpath;
			ddata.spacedata = curr_view->spacedata;
#endif
			ret = _e2_complete_path_check_dir (
#ifdef E2_VFS
				&ddata,
#else
				localpath,
#endif
				p1, p2, found);
			F_FREE (localpath, p2);
		}
		else	//a relative-path string
		{
			gchar *p3 = e2_utils_translate_relative_path (curr_pane->path, p2);
//E2_VFSTMPOK
			localpath = F_FILENAME_TO_LOCALE (p3);
#ifdef E2_VFS
			ddata.path = localpath;
			ddata.spacedata = curr_view->spacedata;
#endif
			ret = _e2_complete_path_check_dir (
#ifdef E2_VFS
				&ddata,
#else
				localpath,
#endif
				p1, p2, found);
			g_free (p3);
			F_FREE (localpath, p3);
		}
		g_free (p2);
		//after checking for completing items in path, never try other categories
//		*flags |= E2_COMPLETE_FLAG_STOP;
	}
	else //no separator
	{	//look everywhere in $PATH
		printd (DEBUG, "complete from path");
		ret = 0;
		const gchar *p = g_getenv ("PATH");  //_I( = this ok for all languages?
		if (p != NULL) 	//CHECKME needed ?
		{
			gchar **paths = g_strsplit (p, G_SEARCHPATH_SEPARATOR_S, -1);
			//iterate over the path members
			gint i = 0;
			while (paths[i] != NULL)
			{
#ifdef E2_VFS
				ddata.path = paths[i];
				ddata.spacedata = NULL;	//$PATH items must be local
				ret += _e2_complete_path_check_dir (&ddata, word, NULL, found);
#else
				ret += _e2_complete_path_check_dir (paths[i], word, NULL, found);
#endif
				i++;
			}
			g_strfreev (paths);
		}
//		if (ret > 0)
//			*flags |= E2_COMPLETE_FLAG_STOP;
	}

	return ret;
}
