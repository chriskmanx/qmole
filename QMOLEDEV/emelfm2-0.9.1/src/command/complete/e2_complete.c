/* $Id: e2_complete.c 2740 2013-08-30 06:51:48Z tpgww $

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

/**
@file src/command/complete/e2_complete.c
@brief string auto completion

This file contains functions to auto-complete strings, e.g. in a command
line or dir line. It has a registry which makes it possible to add new
completion checks. The type of the completion is then choosen from the
registry when completing strings.
*/

#include "e2_complete.h"
#include <string.h>
#include <glob.h>


static GList *checks = NULL;

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief check whether a competion of the kind associated with @a rt is applicable, for the current competion request
@param flags bitflags in accord with the type(s) of competion to be done for the current request
@param rt pointer to E2_CompleteRuntime data struct for a registered completion check
@return TRUE if @a rt relates to a completion that is to be used in this completion request
*/
/*static gboolean _e2_complete_should_check (E2_CompleteFlags flags, E2_CompleteRuntime *rt)
{
	return (flags & rt->flags);
}*/
/**
@brief find the word in @a line that should be completed
this scans @a line backward from cursor position, looking for
the most recent non-escaped ' ' character
*word is set to a copy of @a line after that ' ', up to the cursor position.
(if cursor is at the beginning of @a line, *word is set to a copy of "")
@param line utf8 string containing the line to be checked
@param pos the current cursor position (characters) in @a line
@param word pointer set to the word
@return a copy of the part of @a line before *word, copy of "" if cursor is at start of @a line or no space before cursor
*/
static gchar *_e2_complete_find_word (const gchar *line, gint pos, gchar **word)
{
	//ignore leading whitespace
	gchar *str = e2_utils_pass_whitespace ((gchar *)line);
	if (str != NULL)
		pos -= (str-line);
	//quick exit if cursor is at the beginning
	if (pos == 0)
	{
		*word = g_strdup ("");
		return g_strdup ("");
	}
	//copy the part of the string to the cursor position
	str = e2_utf8_ndup (str, pos);
	//clear any trailing whitespace too (eg in a mount command)
	gchar *tail = (str + strlen (str) - 1);
	while (*tail == ' ' || *tail == '\t')	//don't bother with utf8 get, whitespace is always ascii
	{
		*tail-- = '\0';
		pos--;
	}

	//get the character offset index in str (line) where the word starts
	gint i;
	//search backwards from the current cursor position for the beginning of
	//the word
	for (i = pos; i > 0; i--)
	{
		gunichar cur = g_utf8_get_char (g_utf8_offset_to_pointer (str, i));
		gunichar before = g_utf8_get_char (g_utf8_offset_to_pointer (str, i - 1));
		//a non-escaped space character indicates the gap before the word
		if ((cur == ' ') && (before != '\\'))
		{
			i++;
			break;
		}
	}

	gchar *tmp = g_utf8_offset_to_pointer (str, i);
	*word = e2_utf8_unescape (tmp, ' ');
	gchar *retval = e2_utf8_ndup (str, i);
	g_free (str);
	return retval;
}

/**
@brief store data for a string-completion "method"
@param priority priority assigned to the method (not actually used)
@param flags bit-flags indicating type of match(es) etc
@param func the function which does the completion for the method
@param data pointer to data to be sent to @a func
@return
*/
static void _e2_complete_register_method (guint priority, E2_CompleteFlags flags,
	guint (*func)(gchar*,gchar*,gint,GList**,E2_CompleteFlags*,gpointer,guint),
	gpointer data)
{
	E2_CompleteRuntime *rt = ALLOCATE (E2_CompleteRuntime);	//FIXME never deallocated
	CHECKALLOCATEDWARN (rt, return);
	rt->priority = priority;
	rt->flags = flags;
	rt->func = func;
	rt->data = data;
	checks = g_list_append (checks, rt);
}

  /*******************/
 /***** methods *****/
/*******************/
//other methods are in separate files
/**
@brief create list of files (and non-files) that are valid completions of @a word
Only native completions are suported.
The string to be completed extends from after the ' ' (if any) preceding
the cursor position @a pos, to that position. But if @a pos = 0, the string
will be empty
If the string in question is from a dirline, the corresponding directory
is scanned for completions
For a dirline, @a line = @a word
This function temporarily changes the filesystem CWD

@param line utf8 string containing the whole line to be completed
@param word utf8 string, copy of part of @a line with the 'word' to be completed
@param pos 0-based index of cursor position (characters, not bytes) in @a line
@param found pointer to list to record matching items, must be initialized by caller
@param flags pointer to bit-flags indicating type of match (dirs or files)
@param data discretionary data pointer
@param pane enumerator of pane to be used for getting dir string 1,2,0=current

@return the number of successful matches
*/
guint _e2_complete_files (gchar *line, gchar *word, gint pos, GList **found,
	E2_CompleteFlags *flags, gpointer data, guint pane)
{
	printd (DEBUG, "_e2_complete_files (line:%s,word:%s,pos:%d,found:_,flags:%d,data:_)",
		line, word, pos, *flags);
	gchar *path, *pattern, *pattern_local;
#ifdef E2_VFS
	PlaceInfo *spacedata;
	switch (pane)
	{
		case E2PANE1:
			spacedata = app.pane1.view.spacedata;
			break;
		case E2PANE2:
			spacedata = app.pane2.view.spacedata;
			break;
		default:
			spacedata = curr_view->spacedata;
			break;
	}
	if (spacedata != NULL)
		return 0;	//no completion ATM for non-local places
#ifdef E2_VFSTMP
	make this work ?
#endif
#endif
	if (!g_path_is_absolute (word)
#ifdef E2_VFSTMP
		&& spacedata == NULL //cd only for native filesystem
#endif
		)
	{
		switch (pane)
		{
			case E2PANE1:
				path = app.pane1.view.dir;
				break;
			case E2PANE2:
				path = app.pane2.view.dir;
				break;
			default:
				path = curr_view->dir;
				break;
		}
		if (!e2_fs_chdir (path E2_ERR_NONE()))
			return 0;
	}

#ifdef VFS_TMP
	//FIXME use dir foreach func with callback to g_pattern_match*
#endif
	guint count = 0;
	pattern = g_strconcat (word, "*", NULL);
	pattern_local = F_FILENAME_TO_LOCALE (pattern);
	glob_t matches;
	if (glob (pattern_local, GLOB_MARK, NULL, &matches) == 0)	//this doesn not support vfs matching
	{
		guint i;
		gboolean dirs = (*flags & E2_COMPLETE_FLAG_DIRS) && (!(*flags & E2_COMPLETE_FLAG_FILES));
		gboolean files = (*flags & E2_COMPLETE_FLAG_FILES) && (!(*flags & E2_COMPLETE_FLAG_DIRS));
		for (i = 0; i < matches.gl_pathc; i++)
		{
			if (dirs)
			{
				if (!g_str_has_suffix (matches.gl_pathv[i], G_DIR_SEPARATOR_S))
					continue;
			}
			else if (files)
			{
				if (g_str_has_suffix (matches.gl_pathv[i], G_DIR_SEPARATOR_S))
					continue;
			}
			*found = g_list_append (*found, D_FILENAME_FROM_LOCALE (matches.gl_pathv[i]));
			count++;
		}
	}
	globfree (&matches);

	//only relevant for native filesystem
//#ifdef E2_VFS
//	if (spacedata == NULL)
//#endif
		e2_fs_chdir (curr_view->dir E2_ERR_NONE());
	g_free (pattern);
	F_FREE (pattern_local, pattern);

	return count;
}
/* *
@brief create list of text strings (maybe multi-line) that are valid completions of @a word
Only native completions are suported.
The string to be completed extends from after the ' ' (if any) preceding the
cursor position @a pos, to that position. But if @a pos = 0, the string will be
empty
This is not intended to be used for dirlines
ATM works only for bash
@param line utf8 string containing the whole line to be completed
@param word utf8 string, copy of part of @a line with the 'word' to be completed
@param pos 0-based index of cursor position (characters, not bytes) in @a line
@param found pointer to list to record matching items, must be initialised by caller
@param flags pointer to bit-flags indicating type of match (dirs or files)
@param data discretionary data pointer
@param pane enumerator of pane to be used for getting dir string 1,2,0=current

@return the number of successful matches
*/
/* FOR BASH NO WAY TO TRIGGER THE FULL COMPLETION, JUST GETS FILES OR DIRS
guint _e2_complete_shell (gchar *line, gchar *word, gint pos, GList **found,
	E2_CompleteFlags *flags, gpointer data, guint pane)
{
	printd (DEBUG, "complete_shell (line:%s,word:%s,pos:%d,found:_,flags:%d,data:_)",
		line, word, pos, *flags);
	if (pos == 0)	//nothing to complete
		return 0;
	//FIXME tailor command, use whole line as needed
	gchar *shellcmd = g_find_program_in_path ("bash");
	if (shellcmd == NULL)
		return 0;
	gboolean iscmd = !strcmp (line, word);	//are we doing command or arg ?
	gchar *local = (iscmd) ? F_FILENAME_TO_LOCALE (word):
							 e2_utf8_to_locale (word);
	gchar *command = g_strdup_printf (
		"%s -c 'compgen -o bashdefault %s %s'", shellcmd, (iscmd) ? "-c" : "-c -d", local);
	guint count = 0;
	gpointer results;
	if (e2_fs_get_command_output (command, &results))
	{
		gchar **match;
		gchar **split = g_strsplit ((gchar *)results, "\n", -1);
		for (match = split; *match != NULL; match++)
		{
			if (**match != '\0')
			{
				//CHECKME sometimes lines-join etc
				//assume file/dir at start of command
				gchar *utf = (iscmd) ?
					D_FILENAME_FROM_LOCALE (*match) : e2_utf8_from_locale (*match);
				if (utf != NULL)
				{
					*found = g_list_prepend (*found, utf);
					count++;
				}
			}
		}
		if (count > 1)
			*found = g_list_reverse (*found);
		g_free (results);
		g_strfreev (split);
	}
	g_free (shellcmd);
	g_free (command);
	if (local != word)
		g_free (local);
	return count;
}
*/

  /******************/
 /***** public *****/
/******************/
/**
@brief create sorted list of strings which complete @a complete
@param complete pointer to utf8 string containing the text to be completed
@param pos pointer to 0-based index of cursor position (characters) in @a line
@param found pointer to list of matching strings, need not be initialised by caller
@param flags bit-flags indicating type of match(es) etc
@param pane enumerator of pane to be used for getting dir string 1,2,0=current

@return the number of successful matches
*/
guint e2_complete_str (gchar **complete, gint *pos, GList **found,
	E2_CompleteFlags flags, guint pane)
{
	gchar *word;
	gchar *part1 = _e2_complete_find_word (*complete, *pos, &word);
	//part1 will be empty when no word-break is before the cursor or when cursor
	//is after whitespace at end of *complete

	//this is a hack to get around killing a (u)mount command string with no arg
	gint pos_hack;
	if (strstr (word, E2_MOUNTCOMMAND) != NULL && *part1 == '\0') //_I(
	{
		g_free (part1);
		part1 = g_strconcat (word, " ", NULL);
		pos_hack = strlen (part1);
	}
	else
		pos_hack = 0;

	*found = NULL;
	guint (*func) (gchar*,gchar*,gint,GList**,E2_CompleteFlags*,gpointer,guint);
	gboolean all = flags & E2_COMPLETE_FLAG_ALL;
	guint count = 0;
	GList *member;
	//CHECKME search on the basis of completion priority as distinct from registration order ??
	for (member = checks; member != NULL; member = member->next)
	{
		E2_CompleteRuntime *crt = (E2_CompleteRuntime *)member->data;
		if (all || (flags & crt->flags))
		{
			func = crt->func;
			gint cur = func (*complete, word, *pos, found, &flags, crt->data, pane);
			count += cur;
			if (flags & E2_COMPLETE_FLAG_STOP)
				break;
		}
	}

	if (*found != NULL)
	{
		gchar *repl = NULL;
		gchar *part2 = g_utf8_offset_to_pointer (*complete, *pos);
		gint common = 0;
		if ((*found)->next != NULL)
		{
			repl = g_strdup ((*found)->data);
			gint len1 = g_utf8_strlen ((*found)->data, -1);
			gint common = len1;
			for (member = (*found)->next; member != NULL; member = member->next)
			{
				gint len2 = g_utf8_strlen (member->data, -1);
				gint min = MIN (len1, len2);
				if (common > min)
					common = min;
				gint i;
				for (i = 0; i < min; i++)
				{
					if ((((gchar *) (*found)->data)[i]) != (((gchar *) member->data)[i]))
						break;
				}
				if (common > i)
					common = i;
			}
			if (common == 0)
			{
				g_free (repl);
				repl = NULL;
			}
			else
				repl[common] = '\0';
		} else
		{
			repl = g_strdup ((*found)->data);
		}
		if (repl != NULL)
		{
			gchar *tmp = e2_utf8_escape (repl, ' ');
			gchar *comp_new = g_strconcat (part1, tmp, part2, NULL);
			*pos += g_utf8_strlen (tmp, -1) - g_utf8_strlen (word, -1) + pos_hack; //for [u]mount command
			g_free (tmp);
			g_free (*complete);
			*complete = comp_new;
			if (common != 0)
				repl[common] = ' ';
			g_free (repl);
		}
	}

	if (count > 1)
		*found = g_list_sort (*found, (GCompareFunc) g_utf8_collate);

	g_free (part1);
	g_free (word);
	return count;
}
/**
@brief initialize the auto completion system

These registrations are not config options, and do not appear in the
config dialog, or anywhere else in the UI. They are not really actions, either

@return
*/
void e2_complete_init (void)
{
	RUN_ONCE_CHECK ();

	checks = NULL;

	//register default methods
	/*NOTE that the order of registration is relevant to option
	 E2_COMPLETE_FLAG_STOP which can be set at any time
	 These are registered in priority order, tho' priority is not explicitly
	 examined, now */
#ifdef E2_FS_MOUNTABLE
	_e2_complete_register_method (5, E2_COMPLETE_FLAG_MOUNT,
//	"filesystems", "default", NULL, _("complete filesystems in (u)mount commands"), UNUSED
		e2_complete_mount, NULL);
#endif
	_e2_complete_register_method (10, E2_COMPLETE_FLAG_PATH,
//	"executables from path", "default", NULL, _("search active directory or PATH for matching items"),  UNUSED
		e2_complete_path, NULL);
	_e2_complete_register_method (100, E2_COMPLETE_FLAG_FILES | E2_COMPLETE_FLAG_DIRS,
// "files & directories", "default", NULL, _("complete file and directory names"), UNUSED
		_e2_complete_files, NULL);
/*NOT WORKING AS WANTED
	_e2_complete_register_method (150, E2_COMPLETE_FLAG_SHELL,
// "shell", "default", NULL, _("use external shell tocomplete"), UNUSED
		_e2_complete_shell, NULL);
*/
}
/**
@brief cleanup auto completion data

@return
*/
void e2_complete_clear (void)
{
	GList *member;
	for (member = checks; member != NULL; member = member->next)
		DEALLOCATE (E2_CompleteRuntime, (E2_CompleteRuntime *)member->data);
}
