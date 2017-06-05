/* $Id: e2p_rename.c 3019 2014-01-21 21:51:45Z tpgww $

Copyright (C) 2005-2014 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/e2p_rename.c
@brief file-rename plugin

This file contains functions related to creation of a file-rename dialog,
and execution of a find & rename task in accord with options selected in
that dialog.
*/

/* TODO
FIXME's
hard = detect end of shell command, and update start/stop/help btn sensitivity accordingly
share code with the find plugin ?
*/

#include "emelfm2.h"
#include <string.h>
#include <dirent.h>
#include <fnmatch.h>
#include <regex.h>
#include "e2p_rename.h"
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_icons.h"
#include "e2_filelist.h"
#include "e2_fs.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "rename"

//enable counter-macros in replacement name pattern
#define E2_COUNTER_ENABLED

typedef struct _E2P_RenameData
{
//	E2P_RenFlags flags; see rt->modeflags
	union
	{
		const gchar *pattern;//string with entered 'old' name string
		regex_t *regex; 	//compiled regex for pattern
	} match;
	GPtrArray *candidates;	//array of absolute path strings, each member matches
							//pattern, localised encoding
	E2_RenDialogRuntime *rt;//dialog data, to make things available during treewalk
} E2P_RenameData;

static PluginIface iface;

static gboolean flags[MAX_RENFLAGS];	//session-cache for toggle-values
static GList *dir_history;
static GList *pattern_history;
static GList *newpattern_history;

#ifdef E2_COUNTER_ENABLED
#define MAX_COUNTERS 4
//enum { CINDEX,
enum { CLEN, CSTART, CWIDTH, CCOLSCOUNT };
static guint countercount;
static guint counterdata [MAX_COUNTERS][CCOLSCOUNT];
#endif

static gboolean _e2p_renameQ (E2_ActionTaskData *qed);

  /*********************/
 /***** utilities *****/
/*********************/

/**
@brief set specified flag to T/F

The relevant array value is set

@param f enumerated value of flag to be set
@param value new value for the flag
@param rt ptr to dialog data struct

@return
*/
static void _e2p_ren_set_flag (renflag_t f, gboolean value)
{
	if (f < MAX_RENFLAGS)
	{
		flags [(gint) f] = value;
		printd (DEBUG, "Set rename-flag %d, now %s", f, value ? "TRUE":"FALSE");
	}
}
/**
@brief return the value of a specified flag

@param f enumerated value of flag to be interrogated

@return flag value, T/F, or FALSE if the value is not recognised
*/
static gboolean _e2p_ren_get_flag (renflag_t f)
{
	if (f < MAX_RENFLAGS)
		return (flags [(gint) f]);
	else
		return FALSE;
}
/**
@brief cleanup flags related to radio-buttons group

Ensure one and only one flag in @a checks is set

@param checks array of flag enums
@param count no. of items in @a checks
@param def enumerated value of flag to become default, if all flags in @a checks are FALSE

@return flag value, T/F, or FALSE if the value is not recognised
*/
static void _e2p_ren_clean_flags (renflag_t checks[], guint count, renflag_t def)
{
	gboolean set = FALSE;
	guint i;
	for (i = 0; i < count; i++)
	{
		renflag_t f = checks[i];
		if (flags[f])
		{
			if (!set)
				set = TRUE;
			else
				flags[f] = FALSE;
		}
	}
	if (!set)
		flags[def] = TRUE;
}
/* *
@brief set most flags to FALSE

@return
*/
/*UNUSED
static void _e2p_ren_reset_flags (void)
{
	gint i;
	for (i = 0; i < MAX_RENFLAGS; i++)
		flags[i] = FALSE;
	flags[SEARCH_CURRENT_P] = TRUE;
	flags[OLD_WILD_P] = TRUE;
	flags[NEW_THIS_P] = TRUE;
	flags[CONFIRM_P] = TRUE;
} */
#ifdef E2_COUNTER_ENABLED
/**
@brief setup counter information for replacement name
Up to MAX_COUNTERS counter macros can be used in @a newtemplate.
For each counter, if "i" is provided it is the number which starts that series
For each counter, if "w" is provided it is the minumum width of each count
string in that series, which will be padded with leading 0's as required
Sets rt->modeflags E2PR_COUNTER if "%c" detected.

@param newtemplate replace pattern string, maybe with %c[i[,w]]
@param rt pointer to dialog data struct

@return TRUE if one or more counter macros were processed
*/
//FIXME make this work with replacement name, instead of replacement pattern
static gboolean _e2p_ren_parse_counters (const gchar *newtemplate,
	E2_RenDialogRuntime *rt)
{
	guint indx = 0;	//array index
	gulong start, width;
	const gchar *c, *s;
	gchar *endptr;
	rt->modeflags &= ~E2PR_COUNTER;
	s = newtemplate;
	while ((s = strstr (s, "%c")) != NULL)
	{
		rt->modeflags |= E2PR_COUNTER;	//signal we have counter(s)
		c = s;
		s += 2;	//skip ascii macro signature
		start = strtoul (s, &endptr, 10);
		if (endptr == s)
			start = 1;	//no number provided
		else
			s = endptr;
		if (*s == ',')	//no whitespace check
		{
			s++;
			width  = strtoul (s, &endptr, 10);
			if  (endptr == s)
				width = 1;	//no number provided
			else
				s = endptr;
		}
		else
			width = 1;
		//store counter data for later use
//		counterdata [indx][CINDEX] = c - newtemplate; we need to scan each replacement name
		counterdata [indx][CLEN] = s - c;
		counterdata [indx][CSTART] = (guint) start;
		counterdata [indx][CWIDTH] = (guint) width;
		if (++indx == MAX_COUNTERS)
			break;
	}
	countercount = indx;
	return (rt->modeflags & E2PR_COUNTER);
}
#endif
/**
@brief parse replacement pattern a regex-matched file

Disaggregate replacement filename @a newtempate into 'chunks' separated by
any of \1, \2 ... and/or \g{1}, \g{2} ... and/or \g{-1}, \g{-2} ...
An allocated copy of each chunk is stored in rt->nchunks.
If \0 is detected, a copy of the whole of @a newtemplate is the first and only
member of the array. Otherwise, the first member is NULL and ignored (for
consistency with compiled-regex array arrangement).
Adds rt->modeflags E2PR_NEWALL and E2PR_WHOLE, if \0 detected.
rt->nchunks assumed empty.
@a newtemplate will be localised, but that's not relevant here.

@param newtemplate replacement filename pattern string maybe
	including regex backrefs
@param rt pointer to dialog data struct

@return
*/
static void _e2p_ren_parse_regexpattern (const gchar *newtemplate, E2_RenDialogRuntime *rt)
{
	const gchar *s1, *s2;
	guint chunkcount = 1;

	//first, get likely approx. no. of chunks we will create, for sanity checking
	s1 = newtemplate;
	while ((s1 = strchr (s1, '\\')) != NULL)	//always ASCII
	{
		s1++;
		if (*s1 != '\\')	//ignore any escaped '\'
			chunkcount++;
	}

	g_ptr_array_add (rt->nchunks, NULL);	//normally, we ignore the first member

	s1 = newtemplate;
	while ((s2 = strchr (s1, '\\')) != NULL)	//always ASCII
	{
		glong thiscount;
		gchar *s3;

		s2++;
		if (*s2 == '\\')	//ignore any escaped '\'
		{
			s1 = s2 + 1;
		}
		else if (*s2 == 'g' && *(s2+1) == '{')	//check for bracketed backref
		{
			thiscount = strtol (s2+2, &s3, 10);
			if (s3 > s2+2)
			{
				gboolean store = FALSE;

				if (thiscount == 0)
				{
					//ensure "\0" is sole backref
					if (rt->nchunks->len > 0)
					{
						g_ptr_array_foreach (rt->nchunks, (GFunc)g_free, NULL);
						g_ptr_array_set_size (rt->nchunks, 0);
					}
					rt->modeflags |= (E2PR_WHOLE | E2PR_NEWALL);
					s1 = newtemplate;
					break;
				}
				//In principle, thiscount's value doesn't matter here, we're
				//just separating. However a value that's inconsistent with the
				//number of chunks is highly likely to become a problem when the
				//renaming happens, later
				else if (thiscount > 0 && thiscount < chunkcount)
				{
					store = TRUE;
				}
				else if (thiscount < 0 && thiscount > -chunkcount)
				{
					store = TRUE;
				}
				else
				{
					printd (WARN,"Bad backref count (%i) used", thiscount);
				}

				gboolean pass = FALSE;
				gchar c;
				while ((c = *s3) == '}' || c == ' ')
				{
					if (c == '}')
						pass = TRUE;
					s3++;
				}
				if (store && pass)
					g_ptr_array_add (rt->nchunks, g_strndup (s1, s2 - s1 - 1));

				s1 = s3;
			}
			else
				s1 = s2 + 2;
		}
		else
		{
			thiscount = strtoul (s2, &s3, 10);
			if (s3 > s2)
			{
				if (thiscount == 0)
				{
					//ensure "\0" is sole backref
					if (rt->nchunks->len > 0)
					{
						g_ptr_array_foreach (rt->nchunks, (GFunc)g_free, NULL);
						g_ptr_array_set_size (rt->nchunks, 0);
					}
					rt->modeflags |= (E2PR_WHOLE | E2PR_NEWALL);
					s1 = newtemplate;
					break;
				}
				else if (thiscount > 0)
				{
					do
					{
						if (thiscount < chunkcount)
						{
							g_ptr_array_add (rt->nchunks, g_strndup (s1, s2 - s1 - 1));
							break;
						}
						else if (thiscount > 10)
						{
							//hack to fix probable number after backref ...
							thiscount /= 10;
							s3--;
						}
						else
						{
							printd (WARN,"Bad backref count (%i) used", thiscount);
							break;
						}
					} while (1);
				}
				else
				{
					printd (WARN,"Bad backref count (%i) used", thiscount);
				}
				s1 = s3;
			}
			else
				s1 = s2;
		}
	}

	if (*s1 != '\0')
		g_ptr_array_add (rt->nchunks, g_strdup (s1)); //get the tail of the pattern too
}
/**
@brief parse replacement pattern for a wildcard matched file

Disaggregate replacement filename @a newtempate into 'chunks' separated by
'*' or '?'. Some may be "". An allocated copy of each chunk is stored in rt->nchunks.
If nothing wild is detected, a copy of the whole of @a newtemplate is the first
and only member of the array. Otherwise, the first member is NULL and ignored
(for consistency with compiled-regex array arrangement).
Adds rt->modeflag E2PR_NEWALL if nothing wild is detected.
Adds rt->modeflags E2PR_WHOLE if "\0" detected.
rt->nchunks assumed empty.
@a newtemplate will be localised, but that's not relevant here.

@param newtemplate replacement filename pattern string, maybe including
 one or more '*', '?' or a single "\0"
@param rt pointer to dialog data struct

@return
*/
static void _e2p_ren_parse_wildpattern (const gchar *newtemplate, E2_RenDialogRuntime *rt)
{
	if (strstr (newtemplate, "\\0") != NULL)
	{
		g_ptr_array_add (rt->nchunks, g_strdup (newtemplate));
		rt->modeflags |= (E2PR_NEWALL | E2PR_WHOLE);
	}
	else if ((strchr (newtemplate, '*') == NULL) && (strchr (newtemplate, '?') == NULL)) 	//always ASCII
	{	//there's nothing wild about it ...
		g_ptr_array_add (rt->nchunks, g_strdup (newtemplate));
		rt->modeflags |= E2PR_NEWALL;
	}
	else
	{
		g_ptr_array_add (rt->nchunks, NULL);	//we will ignore the first member

		gchar **split = g_strsplit_set (newtemplate, "*?", -1);
		gchar **tmp;
		for (tmp = split; *tmp != NULL; tmp++)
			g_ptr_array_add (rt->nchunks, *tmp);
		g_free (split);
	}
}
#ifdef E2_COUNTER_ENABLED
/**
@brief update @a newname with current counter values

@param newname string possibly including occurrence(s) of counter macro %c[n[,m]]
@param rt pointer to dialog data struct

@return replacement name, newly-allocated string in same encoding as original
*/
static gchar *_e2p_ren_count_replace (const gchar *newname, E2_RenDialogRuntime *rt)
{
	guint i;
 	gchar *c, *s, *p, *expanded = g_strdup (newname);
	gchar numfmt[20];
	numfmt[0] = '%';
	//use each stored start, width, macro string from when parsed
	for (i = 0; i < countercount; i++)
	{
		//get count string for current value and width
		if (counterdata [i][CWIDTH] > 1)
			g_snprintf (numfmt+1, sizeof(numfmt)-1, "0%uu", counterdata [i][CWIDTH]);
		else
			g_strlcpy (numfmt+1, "u", sizeof(numfmt)-1);
		c = g_strdup_printf (numfmt, counterdata [i][CSTART]);
		//substitute count string for counter macro
		p = strstr (expanded, "%c");
		if (p == NULL)
			break;	//oops !
//		p = (expanded + counterdata [i][CINDEX] + adj);
		*p = '\0';
		s = (p + counterdata [i][CLEN]);
		p = expanded;
		expanded = g_strconcat (p, c, s, NULL);
		//updates
//		adj += strlen (c) - counterdata [i][CLEN];
		counterdata [i][CSTART]++;
		//cleanups
		g_free (c);
		g_free (p);
	}

	return expanded;
}
#endif
/**
@brief determine replacement name for a file
For same-case renames, new tempate chunks (rt->nchunks) will have been populated
before coming here

@param oldtemplate search pattern string (localised) possibly with 'extended' regex
 as appropriate, or NULL
@param oldpath absolute path string (localised) of item to be changed
@param rt pointer to dialog data struct

@return replacement name, newly allocated localised string
*/
static gchar *_e2p_ren_name_replace (gchar *oldtemplate, gchar *oldpath,
	E2_RenDialogRuntime *rt)
{
	gchar *s1;
	gchar *newbase = g_path_get_basename (oldpath);
	GString *result;

	if (rt->modeflags & E2PR_NEWALL)	//no wild or regex to interpret in replacement template
	{
#ifdef E2_COUNTER_ENABLED
		if ((rt->modeflags & (E2PR_COUNTER | E2PR_WHOLE)) == (E2PR_COUNTER | E2PR_WHOLE))
		{
			//get replacement pattern with counters substituted
			s1 = _e2p_ren_count_replace (
				(const gchar *)g_ptr_array_index(rt->nchunks, 0), rt);
			gchar *s2 = e2_utils_str_replace (s1, "\\0", newbase); //handle "\0"
			result = g_string_new (s2);
			g_free (s1);
			g_free (s2);
		}
		else
#endif
 			if (rt->modeflags & E2PR_WHOLE)
		{
			s1 = e2_utils_str_replace (
				(const gchar *)g_ptr_array_index(rt->nchunks, 0), "\\0", newbase);
			result = g_string_new (s1);
			g_free (s1);
		}
#ifdef E2_COUNTER_ENABLED
		else	//no "\0" in pattern
			if (rt->modeflags & E2PR_COUNTER)
		{
			//counter[s] required, use the template, ignore the found filename
			s1 = _e2p_ren_count_replace (
					(const gchar *)g_ptr_array_index(rt->nchunks, 1), rt); //CHECKME which ?
			result = g_string_new (s1);
			g_free (s1);
		}
#endif
		else
			result = g_string_new (newbase);
	}
	else
	{	//not using the whole pattern
		regex_t oldcompiled;
//		gint cflags = (ext_regex) ? REG_EXTENDED : 0 ;  //not REG_ICASE
		if (!regcomp (&oldcompiled, oldtemplate, REG_EXTENDED))
		{
			//need this many data spots
			gint matchcount = (gint)oldcompiled.re_nsub + 1;
			regmatch_t matcholdptr [matchcount];
			gint error;
			gint eflags = 0;	//not REG_NOTBOL | REG_NOTEOL;
			if ((error = regexec (&oldcompiled, newbase, matchcount, matcholdptr, eflags)) != 0)
			{//handle nomatch
				//DEBUG
				size_t len = regerror (error, &oldcompiled, NULL, 0);
				gchar local[len+2];
				regerror (error,  &oldcompiled,  local, len+2);
				printd (DEBUG, local);
				e2_output_print_error (local, FALSE);
			}
			//transform newtemplate into new name, with corresponding matches from oldname
			gchar buf[NAME_MAX+2];
			regmatch_t rx;

			result = g_string_new ("");
			//ignore initial chunk and any chunk(s) past what we asked for
			gint newwilds = (gint)rt->nchunks->len - 1;
			if (newwilds < matchcount)
				matchcount = newwilds;
			gint j = 1; //skip the initial chunk assigned to whole-of-pattern when needed
			gint i;
			//ignore the initial 'whole pattern' match in the regex data
			for (i = 1; i < matchcount; i++)
			{
				//get the matched substring
				rx = matcholdptr[i];
				if (rx.rm_so > -1)
				{	//the substring was actually used in the match
					//get a null-terminated copy
					gint L;
					s1 = newbase + rx.rm_so;
					L = rx.rm_eo - rx.rm_so;
					if (L > NAME_MAX+1)
						L = NAME_MAX+1;	//prevent buffer overflow
					memcpy (buf, s1, L);
					buf[L] = '\0';
				}
				else
				{
					//FIXME
					continue;
				}
				//progressively join up fixed and variable
				for ( ; j <= i ; j++)
				{
					s1 = (gchar *)g_ptr_array_index (rt->nchunks, j);
					if (s1 != NULL && *s1 != '\0')
						result = g_string_append (result, s1);
				}
				result = g_string_append (result, buf);
			}
			//and add any left-over chunks
			if (newwilds <= (gint)oldcompiled.re_nsub + 1)
			{
				s1 = (gchar *)g_ptr_array_index (rt->nchunks, j);
				if (s1 != NULL && *s1 != '\0')
					result = g_string_append (result, s1);
			}
/*??		for ( ; j < rt->nchunks->len; j++)
			{
				s1 = (gchar *)g_ptr_array_index (rt->nchunks, j);
				if (s1 != NULL && *s1 != '\0')
					result = g_string_append (result, s1);
			}
*/
			//cleanup
			regfree (&oldcompiled);
		}
		else
		{
			//handle error - e.g. no regex in expression
			result = g_string_new (newbase);
		}
#ifdef E2_COUNTER_ENABLED
		if (rt->modeflags & E2PR_COUNTER)
		{
			s1 = _e2p_ren_count_replace (result->str, rt);
			result = g_string_assign (result, s1);
			g_free (s1);
		}
#endif
		if (rt->modeflags & E2PR_WHOLE)
		{	//handle all "\0"
			s1 = e2_utils_str_replace (result->str, "\\0", newbase);
			result = g_string_assign (result, s1);
			g_free (s1);
		}
	}

	g_free (newbase);

	if (rt->modeflags & E2PR_LOWER)
	{
		gchar *t = F_FILENAME_FROM_LOCALE (result->str);
		gchar *u = g_utf8_strdown (t, -1);
		s1 = D_FILENAME_TO_LOCALE (u);
		F_FREE (t, result->str);
		g_free (u);
		g_string_free (result, TRUE);
		return s1;
	}
	else if (rt->modeflags & E2PR_UPPER)
	{
		gchar *t = F_FILENAME_FROM_LOCALE (result->str);
		gchar *u = g_utf8_strup (t, -1);
		s1 = D_FILENAME_TO_LOCALE (u);
		F_FREE (t, result->str);
		g_free (u);
		g_string_free (result, TRUE);
		return s1;
	}

	return g_string_free (result, FALSE);
}
/**
@brief check whether @a name is a candidate for renaming

@param name localised name string of candidate
@param data pointer to rename data struct

@return TRUE if @a name matches the pattern sought
*/
static gboolean _e2p_ren_match_name (gchar *name, E2P_RenameData *data)
{
	if (data->rt->modeflags & (E2PR_NORMAL | E2PR_WILD))
	 //non-regex search
		return (!fnmatch (data->match.pattern, name, 0));
	else
	 //regex search
		return (!regexec (data->match.regex, name, 0, NULL, REG_NOTBOL));
}
/**
@brief when recursing, update matching items array
This is a callback for a treewalk function.
Name(s) (utf8) of matched item(s) are stored in an array
Expects BGL to be closed
@param localpath absolute path of item reported by the walker, localised string
@param statptr pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param twdata pointer to tw data struct

@return E2TW_CONTINUE always
*/
static E2_TwResult _e2p_ren_twcb (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2P_RenameData *twdata)
{
	WAIT_FOR_EVENTS
	if (twdata->rt->abort)
	{
		twdata->rt->abort = FALSE;
		return E2TW_STOP;
	}
	gchar *s;
	switch (status)
	{
		case E2TW_DM:	//directory, not opened due to different file system (reported upstream)
		case E2TW_DL:	//directory, not opened due to tree-depth limit (reported upstream)
		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
		case E2TW_DRR:	//directory now readable
		case E2TW_D:	//directory
		case E2TW_F:	//not directory or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
			//the first call here provides the root dir for the search
			//distinguished by a trailing /
			s = strrchr (VPSTR (localpath), G_DIR_SEPARATOR);
			//really, there must be a / in the string, but test anyway ...
			s = (s == NULL) ? (gchar *)localpath : s+1;
			//s now at the name start, or after a trailing / from the root dir
			if ((ITEM_ISHIDDEN(s) && (s[1] == '\0' || (s[1] == '.' && s[2] == '\0')))
				|| *s == '\0'	//this is the first call, with just the root dir
			)
				break;
			if (_e2p_ren_match_name (s, twdata))
				g_ptr_array_add (twdata->candidates, g_strdup(VPSTR(localpath)));
			break;
//		case E2TW_DP:	//directory, finished
//		case E2TW_NS:	//un-stattable item (for which, error is reported upstream)
		default:
			break;
	}
	return E2TW_CONTINUE;
}
/* *
@brief when not recursing, update matching items array

@param dirpath localised path of directory to scan for matching items
@param data pointer to rename data struct
@return
*/
/*static void _e2p_ren_dirscan (gchar *dirpath, E2P_RenameData *data)
{
	gchar *msg, *joined;
	struct dirent *entry;
	struct dirent entrybuf;
#ifdef E2_VFSTMP
	//this open/iterate/clode mechanism probably useless for vfs dir
#endif
	DIR *dp = e2_fs_dir_open (dirpath E2_ERR_NONE());
	if (dp == NULL)
	{
		gchar *utf = F_DISPLAYNAME_FROM_LOCALE (dirpath);
		msg = g_strdup_printf (_("Cannot open directory %s"), utf);
		F_FREE (utf, dirpath);
		e2_output_print_error (msg, TRUE);
		return;
	}

	while (!e2_fs_dir_read (dp, &entrybuf, &entry E2_ERR_NONE()) && entry != NULL)	//FIXME vfs
	{
		if (ITEM_ISHIDDEN(entry->d_name)
			&& (entry->d_name[1] == '\0' || !strcmp (entry->d_name, ".."))
			)
			continue;
		if (_e2p_ren_match_name (entry->d_name, data))
		{
			joined = e2_utils_strcat (dirpath, entry->d_name);
			g_ptr_array_add (data->candidates, joined);
		}
	}

	e2_fs_dir_close (dp E2_ERR_NONE());
}
*/
/**
@brief replace all '.', '*' and '?' in @a pattern with their extended-regex equivalents

@param pattern string to be processed

@return allocated string with replacements done
*/
static gchar *_e2p_ren_wild2regex (const gchar *pattern)
{
	gchar **split = g_strsplit (pattern, ".", -1);
	gchar *joined = g_strjoinv ("\\.", split);
	g_strfreev (split);
	split = g_strsplit (joined, "?", -1);
	g_free (joined);
	joined = g_strjoinv ("(.)", split);
	g_strfreev (split);
	split = g_strsplit (joined, "*", -1);
	g_free (joined);
	joined = g_strjoinv ("(.*?)", split); //lazy-star to prevent greedy matching ?
	g_strfreev (split);
	return joined;
}
/**
@brief perform rename task
This is called only from within callbacks, BGL closed
Then processes the results

@param rt ptr to dialog data struct

@return
*/
static void _e2p_ren_rename (E2_RenDialogRuntime *rt)
{
	const gchar *old, *new;
	if (!_e2p_ren_get_flag (OLD_SEL_P))	//, rt))
	{
		old = gtk_entry_get_text (
#ifdef USE_GTK2_14
			GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->pattern)))
#else
			GTK_ENTRY (GTK_BIN (rt->pattern)->child)
#endif
			);
		if (*old == '\0')
		{
			e2_output_print_error (_("No current name pattern is specified"), FALSE);
			return;
		}
		e2_list_update_history (&pattern_history, old, NULL, 20, FALSE);
	}
	else
		old = NULL;

	if (_e2p_ren_get_flag (NEW_THIS_P))	//, rt))
	{
		new = gtk_entry_get_text (
#ifdef USE_GTK2_14
			GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->newpattern)))
#else
			GTK_ENTRY (GTK_BIN (rt->newpattern)->child)
#endif
			);
		if (*new == '\0')
		{
			e2_output_print_error (_("No replacement name pattern is specified"), FALSE);
			return;
		}
		//eliminate inappropriate replacement pattern entered when processing selected items
		//(but no check for regex backrefs, \0 is ok)
		if (_e2p_ren_get_flag (OLD_SEL_P)	//, rt)
			&& ( (strchr (new, '?') != NULL) || (strchr (new, '*') != NULL) ) )	//always ascii
		{	//there's something wild about it ...
			e2_output_print_error (_("Replacement name pattern cannot have wildcard(s)"), FALSE);
			return;
		}
		e2_list_update_history (&newpattern_history, new, NULL, 20, FALSE);
	}
	else
		new = NULL;

	*rt->status = E2_TASK_RUNNING;
	//update dialog button sensitivities while we search
	gtk_widget_set_sensitive (rt->help_button, FALSE);
	gtk_widget_set_sensitive (rt->start_button, FALSE);
	gtk_widget_set_sensitive (rt->stop_button, TRUE);
	WAIT_FOR_EVENTS

	gchar *localroot;
	gchar *tmp;
	gchar *srctemplate = NULL;//assignment for complier-warning prevention only
	const gchar *startdir;
	regex_t compiled;
	E2P_RenameData data;
#ifdef E2_VFS
	VPATH sdata;
#endif

	//get paths/names of items to replace
	gboolean result;
	memset (&data, 0, sizeof (E2P_RenameData));
	data.rt = rt;
	data.candidates = g_ptr_array_new ();

	if (_e2p_ren_get_flag (OLD_SEL_P))	//, rt))
	{
#ifdef E2_VFS
		sdata.spacedata = curr_view->spacedata;
#endif
		//to distinguish items, get quoted names then remove quotes
		gchar *macro = (_e2p_ren_get_flag (SEARCH_OTHER_P)) ? "%F" : "%p"; //%F==%P

		gchar *itempaths = e2_utils_expand_macros (macro, NULL);
		if (itempaths != NULL)
		{
			gchar *local, *s, *p = itempaths;
			while (*p != '\0')
			{
				s = strchr (p, '"');
				p = s+1;
				s = strchr (p, '"');
				*s = '\0';
				local = D_FILENAME_TO_LOCALE (p);
				g_ptr_array_add (data.candidates, local);
				p = s+1;
			}
			g_free (itempaths);
		}
		result = (data.candidates->len > 0);
	}
	else	//not renaming selected items
	{
		result = TRUE;
		//get search-start dir, with a trailing /, among other reasons
		//to signal to the tw cb which is the search-root to be ignored
		if (_e2p_ren_get_flag (SEARCH_CURRENT_P))	//, rt))
		{
#ifdef E2_VFS
			sdata.spacedata = curr_view->spacedata;
#endif
			startdir = curr_view->dir;
		}
		else if (_e2p_ren_get_flag (SEARCH_OTHER_P))	//, rt))
		{
#ifdef E2_VFS
			sdata.spacedata = other_view->spacedata;
#endif
			startdir = other_view->dir;
		}
		else if (_e2p_ren_get_flag (SEARCH_ALL_P))	//, rt))
		{
#ifdef E2_VFS
			sdata.spacedata = curr_view->spacedata;
#endif
			startdir = G_DIR_SEPARATOR_S;	//root of filesystem tree FIXME vfs
			_e2p_ren_set_flag (SEARCH_SUBDIRS_P, TRUE);	//, rt);
		}
		else	//search in specified dir
		{
#ifdef E2_VFS
# ifdef E2_VFSTMP
	FIXME get relevant space
# endif
			sdata.spacedata = curr_view->spacedata;
#endif
			startdir = gtk_entry_get_text (
#ifdef USE_GTK2_14
				GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->directory)))
#else
				GTK_ENTRY (GTK_BIN (rt->directory)->child)
#endif
			);
			startdir = e2_utils_pass_whitespace ((gchar *)startdir);
			if (startdir == NULL)
			{
				result = FALSE;
			}
			else
			{
				if (*startdir != G_DIR_SEPARATOR)
				{
					tmp = e2_utils_strcat (curr_view->dir, startdir);
					gtk_entry_set_text (
#ifdef USE_GTK2_14
						GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->directory))),
#else
						GTK_ENTRY (GTK_BIN (rt->directory)->child),
#endif
						tmp);
					g_free (tmp);
					startdir = gtk_entry_get_text (
#ifdef USE_GTK2_14
						GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->directory)))
#else
						GTK_ENTRY (GTK_BIN (rt->directory)->child)
#endif
						);
				}
				e2_list_update_history (&dir_history, startdir, NULL, 20, FALSE);
			}
		}

		if (result)
		{
			if (_e2p_ren_get_flag (OLD_WILD_P))	//, rt))	//exact or wildcard match
			{
				rt->modeflags = E2PR_WILD;
				data.match.pattern = old; //CHECKME this is UTF8
			}
			else //regex match
			{
				if (regcomp (&compiled, old, REG_EXTENDED))
				{
					result = FALSE;
					tmp = g_strdup_printf
						(_("Error in regular expression %s"), old);
					e2_output_print_error (tmp, TRUE);
				}
				else
				{
					rt->modeflags = E2PR_REGEX;
					data.match.regex = &compiled;
				}
			}

			if (result)
			{
				//accumulate array of matching items, path-by-path
				localroot = D_FILENAME_TO_LOCALE (startdir);	//always copied
				//add trailing / if need be
				if ((tmp = strrchr (localroot, G_DIR_SEPARATOR)) == NULL
				  || tmp != (localroot + strlen (localroot) - 1))
				{
					tmp = localroot;
					localroot = e2_utils_strcat (localroot, G_DIR_SEPARATOR_S);
					g_free (tmp);
				}
#ifdef E2_VFS
				sdata.path = localroot;
#endif
				if (_e2p_ren_get_flag (SEARCH_SUBDIRS_P))	//, rt))
				{
					rt->modeflags |= E2PR_RECURSE;
					e2_dialog_set_cursor (rt->dialog, GDK_WATCH);
					OPENBGL
#ifdef E2_VFS
					result = e2_fs_tw (&sdata, _e2p_ren_twcb, &data, -1,
#else
					result = e2_fs_tw (localroot, _e2p_ren_twcb, &data, -1,
#endif
						E2TW_PHYS E2_ERR_NONE());
					CLOSEBGL
					e2_dialog_set_cursor (rt->dialog, GDK_LEFT_PTR);
				}
				else
				{
					OPENBGL
#ifdef E2_VFS
					result = e2_fs_tw (&sdata, _e2p_ren_twcb, &data, 1,
#else
					result = e2_fs_tw (localroot, _e2p_ren_twcb, &data, 1,
#endif
						E2TW_QT | E2TW_PHYS E2_ERR_NONE());
					CLOSEBGL
				}

				g_free (localroot);
				//restore real flag state
				if (_e2p_ren_get_flag (SEARCH_ALL_P))	//, rt))
				{
					gboolean oldstate =
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_button));
#else
						GTK_TOGGLE_BUTTON (rt->recurse_button)->active;
#endif
					_e2p_ren_set_flag (SEARCH_SUBDIRS_P, oldstate);	//, rt);
				}

				if (data.candidates->len == 0)
				{
					//couldn't find anything
					tmp = g_strdup_printf (_("Cannot find anything which matches %s"), old);
					e2_output_print_error (tmp, TRUE);
					result = FALSE;
				}
			}
		}
	}

	if (!result)
	{
		gtk_widget_set_sensitive (rt->help_button, TRUE);
		gtk_widget_set_sensitive (rt->start_button, TRUE);
		gtk_widget_set_sensitive (rt->stop_button, FALSE);
//#ifndef USE_GLIB2_22
		g_ptr_array_foreach (data.candidates, (GFunc)g_free, NULL);
//#endif
		g_ptr_array_free (data.candidates, TRUE);
		WAIT_FOR_EVENTS
		*rt->status = E2_TASK_PAUSED;
		return;
	}

	gboolean multi = (data.candidates->len > 1);

	if (_e2p_ren_get_flag (NEW_THIS_P))	//*new != '\0' checked before
	{
		if (_e2p_ren_get_flag (OLD_WILD_P))	//, rt)) (data.flags == E2PR_WILD)
		{	//wildcard or specific names used
			tmp = F_FILENAME_TO_LOCALE (old);
			srctemplate = _e2p_ren_wild2regex (tmp); //freeme later
			F_FREE (tmp, old);
//			printd (DEBUG, "wildcard rename pattern is %s", pattern);
			//get the chunks of the replacement pattern
			_e2p_ren_parse_wildpattern (new, rt);
		}
		else if (_e2p_ren_get_flag (OLD_REGEX_P))
		{	//regex names (actually, paths) used
			//convert the name using the same regex as 'find' used
			srctemplate = D_FILENAME_TO_LOCALE (old);  //freeme later
			//get the chunks of the replacement pattern
			_e2p_ren_parse_regexpattern (new, rt);
		}
		else //OLD_SEL_P
			//for this case, real srctemplate is set inside remame loop
			//for selected-item renaming \0 or %c can be in the pattern
			//cheap parsing, having already checked that new pattern does not have * or ?
			_e2p_ren_parse_wildpattern (new, rt);

#ifdef E2_COUNTER_ENABLED
		_e2p_ren_parse_counters (new, rt);
#endif
		rt->modeflags |= E2PR_PATTERN;
	}
	else	//we are _only_ changing case
	{
		rt->modeflags = E2PR_NEWALL;	//prevent rename func from trying to parse srctemplate
//		srctemplate = NULL;	//don't need an old pattern
	}

	if (_e2p_ren_get_flag (OLD_SEL_P))	//, rt))
		rt->modeflags |= E2PR_SEL;
	if (_e2p_ren_get_flag (NEW_LOWER_P))	//, rt))
		rt->modeflags |= E2PR_LOWER;
	else if (_e2p_ren_get_flag (NEW_UPPER_P))	//, rt))
		rt->modeflags |= E2PR_UPPER;

	//FIXME start an async task-thread
	GtkWidget *dialog;
	gboolean check;

	if (_e2p_ren_get_flag (CONFIRM_P))	//, rt))
	{ //setup for repeated non-modal dialogs
		dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION, "", _("confirm"),
			DUMMY_RESPONSE_CB, NULL);
		//set default button to 'no'
		if (multi)
		{
			e2_dialog_set_negative_response (dialog, E2_RESPONSE_NOTOALL);
			e2_dialog_add_defined_button (dialog, &E2_BUTTON_CANCEL);
		}
		else
			e2_dialog_set_negative_response (dialog, GTK_RESPONSE_NO);

		E2_Button local_btn;
		//set no label
		e2_button_derive (&local_btn, &E2_BUTTON_NO,
				(multi) ? BTN_NO_SKIP : BTN_NO_CANCEL);

		local_btn.showflags |= E2_BTN_DEFAULT;
		e2_dialog_add_defined_button (dialog, &local_btn);

		local_btn = E2_BUTTON_YES;
		local_btn.label = _("_Rename");
		local_btn.name = STOCK_NAME_CONVERT;
		local_btn.showflags &= ~E2_BTN_DEFAULT;
		e2_dialog_add_defined_button (dialog, &local_btn);

		e2_dialog_setup (dialog, app.main_window);
		check = e2_option_bool_get ("confirm-overwrite");
	}
	else
	{
		dialog = NULL;
		check = FALSE;
	}

	e2_filelist_disable_refresh ();
	e2_dialog_set_cursor (rt->dialog, GDK_WATCH);

	DialogButtons choice;
	gpointer *p;
	guint indx;
	//walk the array of matching filepaths
	for (p = data.candidates->pdata, indx = 0;
		indx < data.candidates->len;
		p++, indx++)
	{
		gchar *localpath = *((gchar **)p); //localised
		gchar *localbase = g_path_get_basename (localpath); //localised
		gchar *localdir = g_path_get_dirname (localpath); //localised

		gchar *utfbase = F_FILENAME_FROM_LOCALE (localbase);
		gchar *utfdir = F_FILENAME_FROM_LOCALE (localdir);

		//check for filelist refresh needed
		gint len = strlen (localdir);
		gboolean incurr = (strncmp (curr_view->dir, localdir, len) == 0
			&& curr_view->dir[len] == G_DIR_SEPARATOR
			&& curr_view->dir[len+1] == '\0');
		gboolean inothr = (strncmp (other_view->dir, localdir, len) == 0
			&& other_view->dir[len] == G_DIR_SEPARATOR
			&& other_view->dir[len+1] == '\0');
		//for other cases, srctemplate is set outside loop (maybe NULL)
		if ((rt->modeflags & (E2PR_SEL | E2PR_PATTERN)) == (E2PR_SEL | E2PR_PATTERN))
			srctemplate = localbase;
		//get the replacement basename
		gchar *newbase = _e2p_ren_name_replace (srctemplate, localpath, rt);
		gchar *utfnew = F_FILENAME_FROM_LOCALE (newbase);
		gchar *newpath = g_build_filename (localdir, newbase, NULL);

		//get displayable names, with any pango-annoying element escaped
		gchar *dir_public = g_markup_escape_text (utfdir, -1);
		gchar *base_public = g_markup_escape_text (utfbase, -1);
		gchar *newbase_public = g_markup_escape_text (utfnew, -1);

#ifdef E2_VFS
		VPATH sdata = { localpath, sdata.spacedata};
		VPATH ddata = { newpath, sdata.spacedata };
#endif
		//ask, if the confirm option is is force, and the parent's stop btn not pressed
		if (_e2p_ren_get_flag (CONFIRM_P) && ! rt->abort)
		{
			gint result;

			if (check)
			{
#ifdef E2_VFS
				result = e2_fs_path_exists (&ddata);
#else
				result = e2_fs_path_exists (newpath);
#endif
				if (result == 1)
				{
					//no extra info for item with same name but different case
					if (e2_utf8_caseless_match (base_public, newbase_public, -1, -1))
						result = 0;
				}
			}
			else
				result = 0; //no extra advice, in this case

			const gchar *fmt = (result > 0) ?
				_("Rename\n<b>%s</b>\nto\n<b>%s</b> (which already exists)\nin %s"):
				_("Rename\n<b>%s</b>\nto\n<b>%s</b>\nin %s");
			gchar *prompt = g_strdup_printf (fmt, base_public, newbase_public, dir_public);
			GtkWidget *label = g_object_get_data (G_OBJECT (dialog), "e2-dialog-label");
			gtk_label_set_markup (GTK_LABEL (label), prompt);
			g_free (prompt);

			gtk_widget_show_all (dialog);

			*rt->status = E2_TASK_PAUSED;
			choice = e2_dialog_wait (dialog, TRUE, TRUE, multi, TRUE);
			*rt->status = E2_TASK_RUNNING;
			gtk_widget_hide (dialog); //CHECKME if closed by user
			gtk_widget_grab_focus (rt->dialog);
		}
		else	//no individual name-confirmation
			if (!rt->abort)
		{
			//ALWAYS check for overwrite, when not showing specific rename
#ifdef E2_VFS
			gint result = e2_fs_path_exists (&ddata);
#else
			gint result = e2_fs_path_exists (newpath);
#endif
			if (result == 1)
			{
				//ignore item with same name but different case
				if (e2_utf8_caseless_match (base_public, newbase_public, -1, -1))
					result = 0;
			}
			if (result > 0)
			{
				OPENBGL
				choice = e2_dialog_ow_check (
#ifdef E2_VFS
					&sdata, &ddata,
#else
					localpath, newpath,
#endif
					NOALL);
				CLOSEBGL
			}
			else
				choice = OK;
		}
		else	//aborted
			choice = NO_TO_ALL;

		if (choice == OK)
		{
			gboolean success;

			OPENBGL	//downstream errors invoke local mutex locking
#ifdef E2_VFS
			success = e2_task_backend_rename (&sdata, &ddata);
#else
			success = e2_task_backend_rename (localpath, newpath);
#endif
			CLOSEBGL

			if (success)
			{
				//show renamed item in filelist(s), if needed
				if (incurr)
					e2_fileview_adjust_name (curr_view,
						localbase, newbase, utfbase, utfnew);
				if (inothr)
					e2_fileview_adjust_name (other_view,
						localbase, newbase, utfbase, utfnew);

				if (!_e2p_ren_get_flag (CONFIRM_P))	//, rt))
				{	//we didn't ask already, so show what's done
					gchar *msg = g_strdup_printf (_("Renamed %s to %s in %s"),
						base_public, newbase_public, dir_public);
					e2_output_print (&app.tab, msg, NULL, TRUE, NULL);
					g_free (msg);
				}
			}
		}

		F_FREE (utfbase, localbase);
		g_free (localbase);
		F_FREE (utfdir, localdir);
		g_free (localdir);
		F_FREE (utfnew, newbase);
		g_free (newbase);
		g_free (newpath);
		g_free (dir_public);
		g_free (base_public);
		g_free (newbase_public);

		if (choice == NO_TO_ALL)
		{	//confirm dialog or o/w dialog or main dialog stop btn pressed
			rt->abort = FALSE;  //make sure this is now clear
			break;
		}
	}	//end of candidate-paths loop

	if (GTK_IS_DIALOG(dialog))	//not explicitly closed by the user
		gtk_widget_destroy (dialog);
	*rt->status = E2_TASK_PAUSED;

	e2_filelist_enable_refresh ();
	e2_dialog_set_cursor (rt->dialog, GDK_LEFT_PTR);

	//cleanups
	g_ptr_array_foreach (data.candidates, (GFunc)g_free, NULL);
	g_ptr_array_free (data.candidates, TRUE);

	if (_e2p_ren_get_flag (OLD_WILD_P) || _e2p_ren_get_flag (OLD_REGEX_P))
		//new pattern with wildcards was constructed
		g_free (srctemplate);
	g_ptr_array_foreach (rt->nchunks, (GFunc)g_free, NULL);
	g_ptr_array_set_size (rt->nchunks, 0);
	rt->parsed = FALSE;
	//revert the buttons
	gtk_widget_set_sensitive (rt->help_button, TRUE);
	gtk_widget_set_sensitive (rt->start_button, TRUE);
	gtk_widget_set_sensitive (rt->stop_button, FALSE);

	gtk_window_present (GTK_WINDOW (rt->dialog));
/*	gtk_widget_grab_focus (
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->pattern))
#else
		GTK_BIN (rt->pattern)->child
#endif
	);
*/
}

  /*********************/
 /***** callbacks *****/
/*********************/

#ifdef E2_RENCHOOSER
/**
@brief change search directory

@param chooser the selection object
@param rt pointer to dialog data

@return
*/
static void _e2p_ren_choose_directory_cb (GtkFileChooser *chooser, E2_RenDialogRuntime *rt)
{
	NEEDCLOSEBGL
	gchar *uri = gtk_file_chooser_get_uri (chooser);
	if (uri != NULL)
	{
		gchar *dirpath = g_filename_from_uri (uri, NULL, NULL);
		if (dirpath != NULL)
		{
			if (*dirpath != '\0')
			{
				gchar *opendir = F_FILENAME_FROM_LOCALE (dirpath);
				gtk_entry_set_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->directory))),
#else
					GTK_ENTRY (GTK_BIN (rt->directory)->child),
#endif
					opendir);
				F_FREE (opendir, dirpath);
			}
			g_free (dirpath);
		}
		g_free (uri);
	}
	NEEDOPENBGL
}
/**
@brief idle callback to add file-chooser button to dialog
This allows adding button in main-thread, which is needed to avoid crash
@param rt pointer to dialog data struct
@return FALSE to cancel idle
*/
static gboolean _e2p_ren_add_chooser (E2_RenDialogRuntime *rt)
{
	const gchar *message = _("Choose directory");
	rt->chooser_button =
	gtk_file_chooser_button_new (message, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_file_chooser_set_show_hidden (GTK_FILE_CHOOSER (rt->chooser_button), TRUE);
	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (rt->chooser_button), rt->chooser_startdir);
	g_free (rt->chooser_startdir);
	rt->chooser_startdir = NULL;
	e2_widget_set_safetip (rt->chooser_button, message);
	g_signal_connect (G_OBJECT (rt->chooser_button), "current-folder-changed",
		G_CALLBACK (_e2p_ren_choose_directory_cb), rt);

	gtk_box_pack_end (GTK_BOX (rt->chooser_box), rt->chooser_button, FALSE, FALSE, 0);
	gboolean state = _e2p_ren_get_flag (SEARCH_THIS_P);
	gtk_widget_set_sensitive (rt->chooser_button, state);
	CLOSEBGL
	gtk_widget_show (rt->chooser_button);
	OPENBGL
	return FALSE;
}
#endif
/**
@brief toggle specified option flag

@param button toggled button widget
@param flagnum pointerized number of the flag to be toggled

@return
*/
static void _e2p_ren_toggle_cb (GtkToggleButton *button, gpointer flagnum)
{
	E2_RenDialogRuntime *rt = g_object_get_data (G_OBJECT (button), "e2-runtime");
	NEEDCLOSEBGL
	//if this if this is during setup, before a widget is created ...
#ifdef USE_GTK2_20
	if (!gtk_widget_get_mapped (rt->dialog))
#else
	if (!GTK_WIDGET_MAPPED (rt->dialog))
#endif
	{
		NEEDOPENBGL
		return;
	}

	renflag_t flg = (renflag_t) flagnum;
	gboolean newflag = ! _e2p_ren_get_flag (flg);	//, rt);
	_e2p_ren_set_flag (flg, newflag);	//, rt);
	switch (flg)
	{
		case OLD_SEL_P:
		  if (newflag
			&& (_e2p_ren_get_flag (SEARCH_ALL_P)	//, rt)
			 || _e2p_ren_get_flag (SEARCH_THIS_P)))	//, rt)))
			  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->active_button), TRUE);
		  if (newflag)
		  {
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->recurse_button), FALSE);
			gtk_widget_set_sensitive (rt->pattern, FALSE);
		  }
		  gtk_widget_set_sensitive (rt->recurse_button, !newflag);
		  break;
		case OLD_WILD_P:
		case OLD_REGEX_P:
		  if (newflag)
		  {
			gtk_widget_set_sensitive (rt->pattern, TRUE);
			gtk_widget_grab_focus (
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->pattern))
#else
				GTK_BIN (rt->pattern)->child
#endif
			);
		  }
		  break;
		case SEARCH_ALL_P:
		  if (newflag)
		  {
			if (_e2p_ren_get_flag (OLD_SEL_P))
			  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->wild_button), TRUE);
		  }
		  break;
		case SEARCH_THIS_P:
		  gtk_widget_set_sensitive (rt->directory, newflag);
#ifdef E2_RENCHOOSER
		  gtk_widget_set_sensitive (rt->chooser_button, newflag);
#endif
		  if (newflag)
		  {
			if (_e2p_ren_get_flag (OLD_SEL_P))
			  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->wild_button), TRUE);
			gtk_widget_grab_focus (
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->directory))
#else
				GTK_BIN (rt->directory)->child
#endif
			);
		  }
		  break;
		case NEW_THIS_P:
		  gtk_widget_set_sensitive (rt->newpattern, newflag);
		  if (newflag)
			gtk_widget_grab_focus (
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->newpattern))
#else
				GTK_BIN (rt->newpattern)->child
#endif
		  );
		  break;
		default:
		  break;
	}
	NEEDOPENBGL
}
/**
@brief toggle specified option flag

@param button toggled button widget
@param flagnum pointerized number of the flag to be toggled

@return
*/
static void _e2p_ren_grouptoggle_cb (GtkToggleButton *button, gpointer flagnum)
{
	renflag_t flg = (renflag_t) flagnum;
//	E2_RenDialogRuntime *rt = g_object_get_data (G_OBJECT (button), "e2-runtime");
	gboolean newflag = ! _e2p_ren_get_flag (flg);	//, rt);
	_e2p_ren_set_flag (flg, newflag);	//, rt);
	if (newflag)
	{	//clear all other members of the group
		GtkToggleButton *tmp = (GtkToggleButton *)g_object_get_data
			(G_OBJECT (button), "group_leader");
		GSList *members;
		NEEDCLOSEBGL
		for (members = g_object_get_data (G_OBJECT (tmp), "group_members");
			 members != NULL; members = members->next)
		{
			tmp = (GtkToggleButton *)members->data;
			if (tmp != button)
				gtk_toggle_button_set_active (tmp, FALSE);
		}
		NEEDOPENBGL
	}
}
/**
@brief adjust directory string in @a entry
After a keypress, this clears any selection and completes the path.
If the current content is not an absolute path, the active-pane directory
is referenced for completion.
@param entry the entry widget for directory data
@param event pointer to event data struct
@param data UNUSED data specified when callback was connnected

@return TRUE if the key was non-modified and a textkey and completion was done
*/
static gboolean _e2p_ren_key_press2_cb (GtkWidget *entry, GdkEventKey *event,
	gpointer data)
{
	if ((event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK
		| GDK_MOD3_MASK | GDK_MOD4_MASK | GDK_MOD5_MASK	//CHECKME
#ifdef USE_GTK2_10
		| GDK_SUPER_MASK | GDK_HYPER_MASK | GDK_META_MASK
#endif
	)) == 0
		&& (event->keyval < 0xF000 || event->keyval > 0xFFFF))
	{
		NEEDCLOSEBGL
		gboolean ret = e2_fs_complete_dir (entry, event->keyval, 0); //default is active pane
		NEEDOPENBGL
		return ret;
	}
	return FALSE;
}
/**
@brief entry-activation signal callback to commence renaming

@param entry UNUSED entry widget which was activated
@param rt ptr to dialog data struct

@return
*/
static void _e2p_ren_activation_cb (GtkEntry *entry, E2_RenDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2p_ren_rename (rt);
	NEEDOPENBGL
}
/**
@brief dialog response callback

@param dialog the dialog where the response was triggered
@param response the response for the clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2p_ren_response_cb (GtkDialog *dialog, gint response,
	E2_RenDialogRuntime *rt)
{
	switch (response)
	{
		case E2_RESPONSE_USER1:	//rename
			NEEDCLOSEBGL
			_e2p_ren_rename (rt);
			NEEDOPENBGL
			break;
		case E2_RESPONSE_USER2:	//help
			NEEDCLOSEBGL
			e2_utils_show_help ("rename plugin"); //no translation unless help doc is translated
			gtk_widget_grab_focus (rt->dialog);
			NEEDOPENBGL
			break;
		case E2_RESPONSE_NOTOALL: //cancel button click
	 	//this can be clicked during a scan for matching items, or before
		//renaming a matched item
			rt->abort = TRUE;
			break;
		default:	//no
			if (rt->groups != NULL)
			{	//rt->groups is a list of "leader" buttons in the dialog
				GSList *members, *tmp;
				for (tmp = rt->groups; tmp != NULL; tmp=tmp->next)
				{
					members = g_object_get_data (G_OBJECT (tmp->data), "group_members");
					g_slist_free (members);
				}
				g_slist_free (rt->groups);
			}
			break;
	}
}

  /***********************/
 /*** widget creation ***/
/***********************/

/**
@brief create and show a toggle in a specified container

@param box the widget into which the button is to be placed
@param label  translated string for the button label
@param state T/F initial state of the toggle
@param callback the button's "toggled" signal callback function
@param f enumerated value of flag to be associated with the button
@param rt ptr to dialog data struct

@return the button widget (UNUSED, now)
*/
static GtkWidget *__e2p_ren_create_toggle_button (GtkWidget *box,
	gchar *label, gboolean state, void (*callback)(GtkToggleButton*,gpointer),
	renflag_t f, E2_RenDialogRuntime *rt)
{
  GtkWidget *button = e2_button_add_toggle (box, TRUE, state, label,
	NULL, FALSE, E2_PADDING_XSMALL, callback, (gpointer) f);
  g_object_set_data (G_OBJECT (button), "e2-runtime", rt);
  //cached flags default to FALSE
//  if (state)
//	 _e2p_ren_set_flag (f, TRUE);	//, rt);
  return button;
}
/**
@brief create and show a grouped toggle in a specified container

@param box the widget into which the button is to be placed
@param leader widget for the 'leader' of the group, or NULL if this is the leader
@param label translated string for the button label
@param f enumerated value of flag to be associated with the button
@param rt ptr to dialog data struct

@return the button widget
*/
static GtkWidget *_e2p_ren_create_toggle_grouped_button (GtkWidget *box,
	GtkWidget *leader, gchar *label, renflag_t f, E2_RenDialogRuntime *rt)
{
	gboolean state = _e2p_ren_get_flag (f);
	GtkWidget *button = __e2p_ren_create_toggle_button
		(box, label, state, _e2p_ren_grouptoggle_cb, f, rt);
	GtkWidget *leadptr;
	GSList *members;
	if (leader == NULL)
	{  //this is the leader of a new group
		leadptr = button;  //leader points to self
		members = NULL;	//start a new list
		rt->groups = g_slist_append (rt->groups, button);  //remember it, for cleaning up
	}
	else
	{  //this is a group member
		leadptr = leader;  //point to group leader, which has list
		members = g_object_get_data (G_OBJECT (leader), "group_members");
	}
	members = g_slist_append (members, button);
	g_object_set_data (G_OBJECT (leadptr), "group_members", members);
	g_object_set_data (G_OBJECT (button), "group_leader", leadptr);
	return button;
}
/**
@brief create and show a toggle in a specified container

@param box the widget into which the button is to be placed
@param label  translated string for the button label
@param state T/F initial state of the toggle
@param f enumerated value of flag to be associated with the button
@param rt pointer to dialog data struct

@return the button widget
*/
static GtkWidget *_e2p_ren_create_toggle_button (GtkWidget *box, gchar *label,
	renflag_t f, E2_RenDialogRuntime *rt)
{
	gboolean state = _e2p_ren_get_flag (f);
	GtkWidget *button = __e2p_ren_create_toggle_button
		(box, label, state, _e2p_ren_toggle_cb, f, rt);
	return button;
}
/**
@brief create and show a 'leader' radio button in a specified container
The initial button-state is set to value of @a f, it may be toggled by other
group members
@param box the widget into which the button is to be placed
@param label  translated string for the button label
@param f enumerated value of flag to be associated with the button
@param rt pointer to dialog data struct

@return the button widget
*/
static GtkWidget *_e2p_ren_create_radio_button (GtkWidget *box, gchar *label,
	renflag_t f, E2_RenDialogRuntime *rt)
{
  GtkWidget *button = e2_button_add_radio (box, label, NULL, _e2p_ren_get_flag (f),
	TRUE, E2_PADDING_XSMALL, _e2p_ren_toggle_cb, (gpointer) f);
  g_object_set_data (G_OBJECT (button), "e2-runtime", rt);

  return button;
}
/**
@brief create and show a radio btn in a specified container
The intial button-state is set to the value of @a f
@param box the widget into which the button is to be placed
@param leader the radio button widget that 'leads' the group
@param label  translated string for the button label
@param f enumerated value of flag to be associated with the button
@param rt pointer to dialog data struct

@return the button widget
*/
static GtkWidget *_e2p_ren_create_radio_grouped_button (GtkWidget *box, GtkWidget *leader,
	gchar *label, renflag_t f, E2_RenDialogRuntime *rt)
{
  GSList *group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (leader));
  GtkWidget *button = e2_button_add_radio (box, label, group, _e2p_ren_get_flag (f),
	TRUE, E2_PADDING_XSMALL, _e2p_ren_toggle_cb, (gpointer) f);
  g_object_set_data (G_OBJECT (button), "e2-runtime", rt);

  return button;
}
/* *
@brief  create and show a file selection widget for identifying the search-root dir

@param title string used for file selection widget title
@param ok ptr to void ok-callback for the widget
@param cancel  ptr to void cancel-callback for the widget
@param rt ptr to find data struct

@return the file selection widget
*/
/* UNUSED
static GtkWidget *_e2p_ren_create_filesel (gchar *title, void (*ok)(GtkWidget *, E2_FindDialogRuntime *),
	       void(*cancel)(GtkWidget *, E2_FindDialogRuntime *), E2_FindDialogRuntime *rt )
{
  GtkWidget *wid = gtk_file_selection_new (title);
  g_signal_connect (G_OBJECT (GTK_FILE_SELECTION (wid)->ok_button),
		     "clicked", G_CALLBACK (ok), rt);
  g_signal_connect (G_OBJECT (GTK_FILE_SELECTION (wid)->cancel_button),
		     "clicked", G_CALLBACK (cancel), rt);
  return wid;
} */
/**
@brief establish and show rename dialog
Operation is queued, even though it does not necessarily work on either
displayed filelist.
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_rename_dialog_create (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_RENAME, art, from,
		_e2p_renameQ, NULL));
}
static gboolean _e2p_renameQ (E2_ActionTaskData *qed)
{
	E2_RenDialogRuntime rt;
	memset (&rt, 0, sizeof(E2_RenDialogRuntime));

	rt.status = qed->status;	//enable status changes on-the-fly
	rt.nchunks = g_ptr_array_new ();
	//don't need to be considered active while simply showing the dialog
	*qed->status = E2_TASK_PAUSED;
	//create dialog
	CLOSEBGL
	rt.dialog = e2_dialog_create (NULL, NULL, _("rename items"),
		(ResponseFunc)_e2p_ren_response_cb, &rt);
	OPENBGL
	//populate it with widgets
	GtkWidget *dialog_vbox, *hbox;
	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt.dialog));
#else
		GTK_DIALOG (rt.dialog)->vbox;
#endif
	e2_widget_add_mid_label (dialog_vbox, _("Search for items:"), 0.02, TRUE, 0);
	GtkWidget *radio = _e2p_ren_create_radio_button (dialog_vbox, _("any_where"),
		SEARCH_ALL_P, &rt);
	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, TRUE, E2_PADDING);
	rt.active_button =
	_e2p_ren_create_radio_grouped_button (hbox, radio, _("in _active directory"),
		SEARCH_CURRENT_P, &rt);
	_e2p_ren_create_radio_grouped_button (hbox, radio, _("in _other directory"),
		SEARCH_OTHER_P, &rt);
#ifdef E2_RENCHOOSER
	rt.chooser_box = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, FALSE, E2_PADDING);
#endif
	_e2p_ren_create_radio_grouped_button (
#ifdef E2_RENCHOOSER
		rt.chooser_box,
#else
		dialog_vbox,
#endif
		radio, _("in _directory"), SEARCH_THIS_P, &rt);
/* DO THIS IN IDLE CALLBACK TO PREVENT CRASH
#ifdef E2_RENCHOOSER
	const gchar *message = _("Choose directory");
	rt.chooser_button =
	gtk_file_chooser_button_new (message, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_file_chooser_set_show_hidden (GTK_FILE_CHOOSER (rt.chooser_button), TRUE);

	gchar *local = qed->currdir;
	gchar *s = local + strlen (local) - sizeof(gchar);
	if (s > local && *s == G_DIR_SEPARATOR)
		*s = '\0';
	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (rt.chooser_button), local);
	if (s > local && *s == '\0')
		*s = G_DIR_SEPARATOR;
	e2_widget_set_safetip (rt.chooser_button, message);
	g_signal_connect (G_OBJECT (rt.chooser_button), "current-folder-changed",
		G_CALLBACK (_e2p_rename_choose_directory_cb), &rt);

	gtk_box_pack_end (GTK_BOX (hbox), rt.chooser_button, FALSE, FALSE, 0);
	gboolean state = _e2p_ren_get_flag (SEARCH_THIS_P);
	gtk_widget_set_sensitive (rt.chooser_button, state);
#endif
*/
	//cuz E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, downstream calls
	//gtk_widget_set_name() which MAY? need BGL closed
	CLOSEBGL
	rt.directory = e2_combobox_add (dialog_vbox, FALSE, E2_PADDING_XSMALL,
		(ActivateFunc)_e2p_ren_activation_cb, &rt, &dir_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);
#ifndef USE_GTK3_0
	OPENBGL
#endif
//#ifndef E2_RENCHOOSER
	gboolean state = _e2p_ren_get_flag (SEARCH_THIS_P);
//#endif
	gtk_widget_set_sensitive (rt.directory, state);	//later toggled if corresponsing radio is selected
	//handle path completion
	g_signal_connect (G_OBJECT
#ifdef USE_GTK2_14
		(gtk_bin_get_child (GTK_BIN (rt.directory))),
#else
		(GTK_BIN (rt.directory)->child),
#endif
		"key-press-event", G_CALLBACK (_e2p_ren_key_press2_cb), NULL);

#ifdef E2_RENCHOOSER
	//to avoid crash, must setup chooser in main thread
	rt.chooser_startdir = g_strdup (qed->currdir);
	gchar *s = rt.chooser_startdir + strlen (rt.chooser_startdir) - sizeof(gchar);
	if (s > rt.chooser_startdir && *s == G_DIR_SEPARATOR)
		*s = '\0';
	g_idle_add_full (G_PRIORITY_HIGH_IDLE, (GSourceFunc)_e2p_ren_add_chooser, &rt, NULL);
#endif

	rt.recurse_button =
	_e2p_ren_create_toggle_button (dialog_vbox, _("R_ecurse subdirectories"),
		SEARCH_SUBDIRS_P, &rt);
	e2_widget_add_separator (dialog_vbox, TRUE, 0);

	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, E2_PADDING);
	radio = _e2p_ren_create_radio_button (hbox, _("_Selected items"),
		OLD_SEL_P, &rt);
	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, E2_PADDING);
	rt.wild_button =
	_e2p_ren_create_radio_grouped_button (hbox, radio, _("Match _exact/wildcard"),
		OLD_WILD_P, &rt);
	_e2p_ren_create_radio_grouped_button (hbox, radio, _("Match regular e_xpression"),
		OLD_REGEX_P, &rt);

	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, 0);
	GtkWidget *wid =
	e2_widget_add_mid_label (hbox, _("Current name is like this:"), 0.0, FALSE, E2_PADDING);
	GtkSizeGroup *same = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget (same, wid);

#ifndef USE_GTK3_0
	CLOSEBGL
#endif
	rt.pattern = e2_combobox_add (hbox, TRUE, E2_PADDING_XSMALL,
		(ActivateFunc)_e2p_ren_activation_cb, &rt, &pattern_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);
#ifndef USE_GTK3_0
	OPENBGL
#endif
	gtk_entry_set_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt.pattern))),
#else
		GTK_ENTRY (GTK_BIN (rt.pattern)->child),
#endif
		"(.*)");
	state = _e2p_ren_get_flag (OLD_SEL_P);
	gtk_widget_set_sensitive (rt.pattern, !state);	//later toggled if corresponsing radio is selected

	e2_widget_add_separator (dialog_vbox, TRUE, 0);

	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, E2_PADDING);
/*	radio = _e2p_ren_create_radio_button (hbox, _("New name is _upper case"),
		NEW_UPPER_P, rt);
	_e2p_ren_create_radio_grouped_button (hbox, radio, _("New name is _lower case"),
		NEW_LOWER_P, rt);
	hbox = e2_widget_add_box (vbox, TRUE, 0, FALSE, FALSE, E2_PADDING);
	_e2p_ren_create_radio_grouped_button (hbox, radio, _("_New name is like this:"),
		NEW_THIS_P, rt);
*/	radio = _e2p_ren_create_toggle_grouped_button (hbox, NULL,
		_("New name is _upper case"), NEW_UPPER_P, &rt);
	_e2p_ren_create_toggle_grouped_button (hbox, radio,
		_("New name is _lower case"), NEW_LOWER_P, &rt);
	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, E2_PADDING);
	wid = _e2p_ren_create_toggle_button (hbox, _("_New name is like this:"),
		NEW_THIS_P, &rt);
#ifndef USE_GTK3_0
	CLOSEBGL
#endif
	gtk_size_group_add_widget (same, wid);
	g_object_unref (G_OBJECT (same));
	rt.newpattern = e2_combobox_add (hbox, TRUE, E2_PADDING_XSMALL,
		(ActivateFunc)_e2p_ren_activation_cb, &rt, &newpattern_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);
	state = _e2p_ren_get_flag (NEW_THIS_P);
	gtk_widget_set_sensitive (rt.newpattern, state);	//later toggled if corresponsing radio is selected
	//  gtk_entry_set_text (GTK_ENTRY (rt->newpattern), "\\1");  //FIXME utf-8
	//  g_object_set_data (G_OBJECT (rt->newpattern), "reset_yourself", reset_entry);
#ifndef USE_GTK3_0
	OPENBGL
#endif
	e2_widget_add_separator (dialog_vbox, TRUE, 0);
	_e2p_ren_create_toggle_button (dialog_vbox, _("Con_firm before each rename"),
		CONFIRM_P, &rt);

	//add action buttons in the order that they will appear
	rt.help_button =
	e2_dialog_add_custom_button_full
		(rt.dialog, FALSE, E2_RESPONSE_USER2, _("_Help"), STOCK_NAME_HELP,
		_("Get advice on rename options"), NULL, NULL);
	E2_Button stop_btn = { _("_Stop"), STOCK_NAME_STOP, _("Stop the current search"),
		E2_BTN_TIPPED, 0, E2_RESPONSE_NOTOALL };
	rt.stop_button = e2_dialog_add_defined_button (rt.dialog, &stop_btn);
	//de-sensitize stop btn, at this stage
	gtk_widget_set_sensitive (rt.stop_button, FALSE);
	E2_BUTTON_CLOSE.showflags |= E2_BTN_DEFAULT;	//CHECKME local copy ?
	e2_dialog_add_defined_button (rt.dialog, &E2_BUTTON_CLOSE);
	rt.start_button =
	e2_dialog_add_custom_button_full (rt.dialog, FALSE,
		E2_RESPONSE_USER1, _("_Rename"),
		STOCK_NAME_CONVERT, _("Begin renaming"), NULL, NULL);
	e2_dialog_set_negative_response (rt.dialog, E2_RESPONSE_NOTOALL);

	if (!_e2p_ren_get_flag (OLD_SEL_P))
		gtk_widget_grab_focus (
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt.pattern))
#else
			GTK_BIN (rt.pattern)->child
#endif
		);

#ifndef USE_GTK3_0
	CLOSEBGL
#endif
	e2_dialog_setup (rt.dialog, app.main_window);
	//block actions Q until user closes
	e2_dialog_run (rt.dialog, NULL,	E2_DIALOG_BLOCKED | E2_DIALOG_FREE);

//	gtk_widget_grab_focus (curr_view->treeview);
	OPENBGL

	g_ptr_array_free (rt.nchunks, TRUE);

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONE_START(_A(1),_("renext"),_e2p_rename_dialog_create,
		_("_Rename.."),
		_("Rename items, using wildcards or regular-expressions"),
		"plugin_"ANAME E2ICONTB)

	if (!e2_cache_check ("rename-flags"))
	{
		//initialise TRUE flags
		flags[SEARCH_CURRENT_P] = TRUE;
		flags[OLD_WILD_P] = TRUE;
		flags[NEW_THIS_P] = TRUE;
		flags[CONFIRM_P] = TRUE;
	}
	e2_cache_array_register ("rename-flags", MAX_RENFLAGS, flags, flags);

	//ensure that radio-button flags are consistent
	renflag_t checks[4] = {	SEARCH_ALL_P, SEARCH_CURRENT_P, SEARCH_OTHER_P, SEARCH_THIS_P };
	_e2p_ren_clean_flags (checks, 4, 1);
	checks[0] = OLD_SEL_P;
	checks[1] = OLD_WILD_P;
	checks[2] = OLD_REGEX_P;
	_e2p_ren_clean_flags (checks, 3, 0);

	dir_history = (GList *)g_new0 (gpointer, 1);
	e2_cache_list_register ("rename-dir-history", &dir_history);
	pattern_history = (GList *)g_new0 (gpointer, 1);
	e2_cache_list_register ("rename-oldpattern-history", &pattern_history);
	newpattern_history = (GList *)g_new0 (gpointer, 1);
	e2_cache_list_register ("rename-newpattern-history", &newpattern_history);

	PLUGINIT_ONE_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	if (ret)
	{
		//backup the cache data
		e2_cache_unregister ("rename-flags");
		e2_cache_unregister ("rename-dir-history");
		e2_cache_unregister ("rename-oldpattern-history");
		e2_cache_unregister ("rename-newpattern-history");
		//cleanup
		e2_list_free_with_data (&dir_history);
		e2_list_free_with_data (&pattern_history);
		e2_list_free_with_data (&newpattern_history);
	}
	return ret;
}
