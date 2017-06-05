/* $Id: e2_complete__mount.c 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2005-2010 tooar <tooar@emelfm2.net>
Portions copyright (C) 2004 Florian Zaehringer <flo.zaehringer@web.de>

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
@file src/command/complete/e2_complete__mount.c
@brief mountpoints command-completion functions
*/

#include "e2_complete.h"

#ifdef E2_FS_MOUNTABLE

#include <string.h>

/**
@brief construct list of the items in @a points that complete @a word

@param points list of mountpoint strings to be scanned
@param word utf-8 string with string to be completed ("mount" or start of mountpoint path)
@param found sore for pointer to list to hold matching items from @a points, must be initialized by caller

@return the number of matches found
*/
static guint _e2_complete_mount_find (GList *points, gchar *word, GList **found)
{
	gchar *path, *match;
	GList *member;
	guint ret = 0;
	gboolean all = FALSE; //the mountpoint was started in the command
	if ((match = strstr (word, E2_MOUNTCOMMAND)) != NULL)	//_I(
	{
		match = e2_utils_find_whitespace (match);
		if (match == NULL)
			all = TRUE;	//the command is at the end of "word"
		else
		{
			match = e2_utils_pass_whitespace (match);
			if (match == NULL)
				all = TRUE;	//command + whitespace is at the end of "word"
		}
	}

	if (all)
		path = NULL;
	else
	{
		if (g_str_has_prefix (word, E2_COMMAND_PREFIX))	//current dir "./"
//E2_VFSTMPOK
			path = g_strconcat (curr_pane->path, word + sizeof(E2_COMMAND_PREFIX), NULL);
		else
			path = g_strdup (word);
	}

	for (member = points; member != NULL; member = member->next)
	{
		gchar *thispoint;
		gchar **dir_last_part;

		thispoint = (gchar *) member->data;
		if (all || g_str_has_prefix (thispoint, path))
		{	//to conform to e2 'style', make sure all mountpoints have a trailing '/'
//			if (g_str_has_suffix (dir, G_DIR_SEPARATOR_S))
				match = g_strdup (thispoint);
//			else
//				match = g_strconcat (thispoint, G_DIR_SEPARATOR_S, NULL);

			if (!all)
			{
				dir_last_part = g_strsplit (match, path, 2);
				g_free (match);

				//prepend word, coz it got killed in g_strsplit
				match = g_strconcat (word, dir_last_part[1], NULL);
				g_strfreev (dir_last_part);
			}

			*found = g_list_append (*found, match);
			ret++;
			//if this matches - we take it. no doubled entries.
			continue;
		}

		if (g_str_has_prefix (thispoint, word))
		{
//			if (g_str_has_suffix (thispoint, G_DIR_SEPARATOR_S))
				match = g_strdup (thispoint);
//			else
//				match = g_strconcat (thispoint, G_DIR_SEPARATOR_S, NULL);

			*found = g_list_append (*found, match);
			ret++;
		}
	}
	if (path != NULL)
		g_free (path);
	return ret;
}
/**
@brief for a mount or umount command, create list of partitions that can be mounted or unmounted
This does no namespace checking. This sort of mounting should only apply to the
native filesystem
The string to be completed extends from after the ' ' (if any) preceding
the cursor position @a pos, to that position. But if @a pos = 0, the string
will be empty
@param line utf8 string containing the whole line to be completed
@param word utf8 string, copy of part of @a line with the 'word' to be completed
@param pos 0-based index of cursor position (characters, not bytes) in @a line
@param found store for pointer to list of matching items (utf-8 paths)
@param flags pointer to bit-flags indicating type of match
@param data discretionary data pointer for this method
@param unused parameter needed for other complete funcs

@return the number of successful matches
*/
guint e2_complete_mount (gchar *line, gchar *word, gint pos, GList **found,
	E2_CompleteFlags *flags, gpointer data, guint unused)
{
	GList *candidates;
	//make sure this is an (un)mount command
	gchar *tmp = g_utf8_offset_to_pointer (line, pos);
	gint byte_pos = (tmp - line);
	if (g_strstr_len (line, byte_pos, E2_MOUNTCOMMAND) == NULL)
		return 0;

	//after mountpoints completion, do not try any other other category
//	*flags |= E2_COMPLETE_FLAG_STOP;

//	uid = getuid (); needed if checking permissions

	if (g_strstr_len (line, byte_pos, "umount") != NULL)	//_I( ??
		candidates = e2_fs_mount_get_mounts_list ();
	else
		candidates = e2_fs_mount_get_mountable_list ();

	guint ret;
	if (candidates != NULL)
	{
		ret = _e2_complete_mount_find (candidates, word, found);
		e2_list_free_with_data (&candidates);
	}
	else
		ret = 0;

	return ret;
}
#endif	//def E2_FS_MOUNTABLE
