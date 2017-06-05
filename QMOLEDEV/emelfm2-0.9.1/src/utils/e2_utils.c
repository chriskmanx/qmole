/* $Id: e2_utils.c 2957 2013-11-18 23:05:50Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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
#include <signal.h>
#include <pwd.h>
#ifdef E2_MAGIC
# include <dlfcn.h>
#endif
#include "e2_utils.h"
#include "e2_dialog.h"
#include "e2_filelist.h"
#include "e2_task.h"

typedef struct _E2_WildData
{
	gchar **path_elements;	//array of strings, from g_strsplit of utf8 path
	gchar **path_matches;	//array of localised strings, with matching path segments, if any
	guint curr_depth;		//count of element currently being matched
	guint last_depth;		//count of the last path element
	guint first_wild_depth;	//first element that has wildcard char(s)
	guint last_wild_depth;	//last element to be matched (maybe 1 more than last depth)
	struct stat *statptr;	//access to statbuf for shared use
} E2_WildData;

typedef struct _E2_BtnEvent
{
	guint button;
	gdouble x_root;
	gdouble y_root;
	gint drag_threshold;
	gchar *device_name;	//event->device->name
} E2_BtnEvent;

//list of E2_BtnEvent's, for reconciling events from multiple pointers
static GList *press_events = NULL;
//static gchar *last_trashpath;
/*max msec between clicks when checking for a doubleclick
maybe replaced by gtk's value in e2_utils_update_gtk_settings()*/
guint click_interval = E2_CLICKINTERVAL;

/**
@brief setup struct with 3 NULL pointers
@return the struct, or NULL
*/
/*E2_Trio *e2_utils_trio_new (void)
{
	E2_Trio *t = ALLOCATE0 (E2_Trio);
	CHECKALLOCATEDWARN (t, );
	return t;
} */
/**
@brief setup struct with 6 NULL pointers
@return the struct, or NULL
*/
E2_Sextet *e2_utils_sextet_new (void)
{
	E2_Sextet *s = ALLOCATE0 (E2_Sextet);
	CHECKALLOCATEDWARN (s, );
	return s;
}
/**
@brief setup struct with 9 NULL pointers
@return the struct, or NULL
*/
E2_Nontet *e2_utils_nontet_new (void)
{
	E2_Nontet *n = ALLOCATE0 (E2_Nontet);
	CHECKALLOCATEDWARN (n, );
	return n;
}
/* *
@brief de-allocate struct with 3 pointers
@param t pointer to the struct to clear
@return
*/
/*void e2_utils_trio_destroy (E2_Trio *t)
{
	if (t != NULL) DEALLOCATE (E2_Trio, t);
} */
/**
@brief de-allocate struct with 6 pointers
@param s pointer to the struct to clear
@return
*/
void e2_utils_sextet_destroy (E2_Sextet *s)
{
	if (s != NULL) DEALLOCATE (E2_Sextet, s);
}
/**
@brief de-allocate struct with 9 pointers
@param n pointer to the struct to clear
@return
*/
void e2_utils_nontet_destroy (E2_Nontet *n)
{
	if (n != NULL) DEALLOCATE (E2_Nontet, n);
}
/**
@brief display error message about insufficient memory
Expects BGL to be closed
@return
*/
void e2_utils_show_memory_message (void)
{
	e2_output_print_error (_("Not enough memory! Things may not work as expected"), FALSE);
}
/**
@brief handle fatal lack of memory
Expects BGL to be open
@return never does
*/
void e2_utils_memory_error (void)
{
	g_critical ("Not enough memory");
	CLOSEBGL 	//called func expects BGL closed
	e2_main_closedown (TRUE, TRUE, TRUE); //no user-cancellation
}
/**
@brief show user help at the heading @a title
Expects BGL to be on/closed
@param title heading string (NO []) to search for in the main help doc, undefined encoding
@return
*/
void e2_utils_show_help (gchar *title)
{
	gchar *helpfile = e2_option_str_get ("usage-help-doc");
	if (helpfile != NULL)
	{
		gchar *local = F_FILENAME_TO_LOCALE (helpfile);
#ifdef E2_VFS
		VPATH ddata = { local, NULL };	//local helpfiles only
		if (!e2_fs_access (&ddata, R_OK E2_ERR_NONE()))
#else
		if (!e2_fs_access (local, R_OK E2_ERR_NONE()))
#endif
		{
			//setup help doc name and heading name, as parameters for command
			gchar *filepath = g_strdup_printf ("%s [%s]", local, title);
#ifdef E2_VFS
			ddata.path = filepath; //possibly mixed encoding
			e2_view_dialog_create_immediate (&ddata);
#else
			e2_view_dialog_create_immediate (filepath);
#endif
			g_free (filepath);
		}
		else
			helpfile = NULL;

		F_FREE (local, helpfile);
	}

	if (helpfile == NULL)
	{
		gchar *msg = g_strdup_printf (_("Cannot read USAGE help document"));
		e2_output_print_error (msg, TRUE);
	}
}
/**
@brief convert color data to string form
@param color Gdk color data struct
@return newly-allocated string with the color data
*/
gchar *e2_utils_color2str (GdkColor *color)
{
	return g_strdup_printf ("#%.2X%.2X%.2X",
		color->red/256, color->green/256, color->blue/256);
}
/**
@brief replace all occurrences of @a old in @a str with @a new
This uses strsplit and strjoinv. No utf8 consideration.
@param str the 'haystack' string in which to search for @a old
@param old the 'needle' string which is to be repaced
@param new the replacement string for @a old
@return newly allocated string with the repacements made
*/
gchar *e2_utils_str_replace (const gchar *str, const gchar *old, const gchar *new)
{
	gchar **split = g_strsplit (str, old, -1);
	gchar *join = g_strjoinv (new, split);
	g_strfreev (split);
	return join;
}
/**
@brief put spaces between all characters in string @a str
@param str utf-8 string which is to be expanded
@return newly-allocated expanded string
*/
gchar *e2_utils_str_stretch (gchar *str)
{
	if (!g_utf8_validate (str, -1, NULL))
		return (g_strdup (str));

	gchar *retval;
	glong len;
	gunichar *conv = g_utf8_to_ucs4_fast (str, -1, &len);
	//the last ' ' is replaced by \0 so don't need memory for that
#ifdef __USE_GNU
	gunichar stretch [len * 2];
#else
	gunichar *stretch = NEW (gunichar, len * 2);
	if (stretch != NULL)
#endif
	{
		glong i, j;
		for (i = 0, j = 0; i < len; i++, j++)
		{
			stretch[j] = conv[i];
			stretch[++j] = ' ';
		}
		stretch[--j] = '\0';

		retval = g_ucs4_to_utf8 (stretch, -1, NULL, NULL, NULL);
	}
#ifndef __USE_GNU
	else
		retval = g_strdup (str);

	g_free (stretch);
#endif
	g_free (conv);
	return retval;
}
/**
@brief ellipsize the specified part of @a string if it's longer than desired
"..." is substituted for the relevant part of @a string, if that's longer than @a length
Note - for middle-position dots, @a limit >= 8
If @a limit is < 3, the returned string may be 1 or 2 chars longer than @a length
@param string utf string which may need to be shortened
@param limit the threshold length (chars, not bytes) for ellipsizing
@param position enumerator for dots at start, middle or end

@return newly-allocated string, shortened or the same as @a string
*/
gchar *e2_utils_str_shorten (gchar *string, gint limit, E2_DotMode position)
{
	g_strchug (string);	//just in case string is a path with trailing space
	glong len = g_utf8_strlen (string, -1);
	if (len > limit)
	{
		gchar *p, *s, *copy;
		glong trim = (limit > 2) ? len - limit + 3 : len - 3 ;
		if (trim > 0)
		{
			switch (position)
			{
				case E2_DOTS_START:
					p = g_utf8_offset_to_pointer (string, trim);
					return g_strconcat ("...", p, NULL);
				case E2_DOTS_END:
					s = g_strdup (string);
					p = g_utf8_offset_to_pointer (s, len - trim);
					*p = '\0';
					p = g_strconcat (s, "...", NULL);
					g_free (s);
					return p;
				default:
					p = string;
					glong gapoffset, chroffset = 0;
					//10 allows for 3 dots, 2 spaces and 5 trailing chars
					if (limit > 10)
					{
						s = string;
						while (s != NULL)
						{
							s = e2_utils_find_whitespace (s);
							if (s != NULL)
							{
								gapoffset = g_utf8_pointer_to_offset (string, s);
								if (gapoffset >= limit-10)
									break;
								p = s;	//p = start of last usable whitespace
								chroffset = gapoffset;
								s = e2_utils_pass_whitespace (s);
							}
						}
					}
					if (p == string)
					{	//no suitable gap found, break toward the middle
						chroffset = limit/2 - 2;
						p = g_utf8_offset_to_pointer (string, chroffset);
					}
					copy = g_strndup (string, (p-string));
					s = g_utf8_offset_to_pointer (string, chroffset+len-limit+3);
					p = g_strconcat (copy, " ... ", s, NULL);
					g_free (copy);
					return p;
			}
		}
	}
	return g_strdup (string);
}

/**
@brief convert from a character-offset to a byte-offset within @a utf8_string
@param utf8_string the string in which we want the offset
@param charoffset 0-based character-position in @a utf8_string, may be < 0 to indicate position relative to end
@return the byte-offset, or -1 upon error
*/
gint e2_utils_get_byte_position (const gchar *utf8_string, gint charoffset)
{
	if (charoffset == 0)
		return 0;
	//glib doesn't do any bounds check, so we do that here
	gint len = g_utf8_strlen (utf8_string, -1);
	if (charoffset > 0)
	{
		if (charoffset >= len)
			return -1;
	}
	else
#ifdef USE_GLIB2_10
	{
		if (-charoffset >= len)
			return -1;
	}
#else
		return -1;
#endif

	const gchar *s = g_utf8_offset_to_pointer (utf8_string, charoffset);
	return (s - utf8_string);
}

/**
@brief if necessary, replace "non-UNIX" line-separators in @a text with LF
In-place conversion is performed. Assumes CR occurs before LF.
@param text 0-terminated string to be processed
@return value indicating the type of separator, CR or LF or CR+LF
*/
gint e2_utils_LF_line_ends (gchar *text)
{
	register gchar c;
	gint retval;
//#ifdef DEBUG_MESSAGES
//	gchar *saved = text;
//#endif
	//quick check for whether conversion is needed
	while ((c = *text) != '\0')
	{
		if (c == LF || c == CR)
			break;
		text++;
	}
	if (c == CR && *(text+sizeof(gchar)) == LF)
		retval = CR + LF;
	else if (c != '\0')
		retval = c;
	else
		retval = LF;	//default to UNIX-style if we find no break

	//replace or remove CR's
	if (c == CR)
	{
		gchar *cleaned = text;
		//loop constructed specifically to work around some compiler behaviour
		while (TRUE)
		{
			if (c == CR)
			{
				*cleaned = LF;
				if (*(text + 1) == LF)
					text++;
			}
			else
			{
				*cleaned = c;
				if (c == '\0')
				{
//					printd (DEBUG, "processed %u chars, new length is %u", text - saved, cleaned - saved);
					break;
				}
			}
			cleaned++;
			text++;
			c = *text;
		}
	}

	return retval;
}
/**
@brief Convert "non-UNIX" line-separtors in @a text to @a separator
@param text string to be processed, must be freeable
@param linecount no. of extra spaces needed when reverting to CR+LF
@param separator desired line-break = CR or LF or CR+LF
@return pointer to adjusted text, maybe different from the original
*/
gchar *e2_utils_revert_line_ends (gchar *text, guint linecount, gint separator)
{
	gchar c;
	gchar *s, *d;
	switch (separator)
	{
		case CR:
			//same size of text, simple replacement
			s = text;
			while ((c = *s) != '\0')
			{
				if (c == LF)
					*s = CR;
				s++;
			}
			break;
		case CR+LF:
		{
			gint textlen = strlen (text) + 1;	//+1 = \0
			gchar *newtext = (gchar *) g_try_realloc (text, (textlen + linecount));
			if (newtext != NULL)
			{
				text = newtext;
				s = text+linecount;
				d = text;
				memmove (s, d, textlen);
//				*d = *s;	//ensure no bad finish at 1st byte
				for (; *d != '\0'; s++, d++)
				{
					if (*s == LF)
					{
						*d = CR;
						d++;
					}
					*d = *s;
				}
			}
		}
			break;
		//case LF:
		default:
			//nothing to do
			break;
	}
	return text;
}
#ifdef E2_MAGIC
/**
@brief establish non-persistent interface to libmagic
@param iface pointer to iface struct to be populated

@return TRUE if connenction is all ok
*/
gboolean e2_utils_fill_magic_iface (MagicIface *iface)
{
	iface->libhandle = dlopen (MAGIC_LIB_NAME, RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
	| RTLD_DEEPBIND
#endif
	);
	if (iface->libhandle != NULL)
	{
		iface->open = dlsym (iface->libhandle, "magic_open");
		iface->close = dlsym (iface->libhandle, "magic_close");
		iface->setflags = dlsym (iface->libhandle, "magic_setflags");
		iface->load = dlsym (iface->libhandle, "magic_load");
		iface->file = dlsym (iface->libhandle, "magic_file");
		iface->error = dlsym (iface->libhandle, "magic_error");
		return TRUE;
	}
	return FALSE;
}
#endif
/**
@brief get mimetype of @a localpath
This is intended only for occasional, non-time-critical usage
Symlinks will not be traversed
@param localpath pointer to item data, including localised path string

@return newly-allocated string (presumably ASCII) or NULL
*/
gchar *e2_utils_get_mimetype (VPATH *localpath)
{
	gchar *mime = NULL;
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
		gchar *command;
//tag E2_BADQUOTES
		gchar *qp = e2_utils_quote_string (VPCSTR (localpath));
		command = e2_utils_strcat ("xdg-mime query filetype ", qp);
		if (e2_fs_get_command_output (command, (gpointer *) &mime))
		{
			g_strstrip (mime); //strip trailing \n and any whitespace
			if (*mime == '\0')
			{
				g_free (mime);
				mime = NULL;
			}
		}
		if (mime == NULL)
		{
			//xdg command failed, maybe due to no xdg utilities, or unrecognised type
#ifdef E2_MAGIC
			MagicIface ifc;

			if (e2_utils_fill_magic_iface (&ifc))
			{
				magic_t handle = ifc.open (
					MAGIC_PRESERVE_ATIME | MAGIC_RAW | MAGIC_ERROR | MAGIC_DEVICES | MAGIC_MIME_TYPE);
				if (handle != NULL)
				{
					ifc.load (handle, NULL); //load failure will result in NULL msg
					const gchar *msg = ifc.file (handle, VPCSTR (localpath));
					if (msg != NULL)
					{
						mime = g_strdup (msg);
						g_strstrip (mime);	//get rid of \n etc
						if (*mime == '\0')
						{
							g_free (mime);
							mime = NULL;
						}
					}
					else
					{
						//X - ifc.error (handle);
						printd (DEBUG, "no libmagic advice for %s", VPSTR (localpath));
					}
					ifc.close (handle);
				}
				else
				{
					printd (WARN, "no libmagic connection");
				}
				dlclose (ifc.libhandle);
			}
			else
			{
				printd (WARN, "no libmagic found");
			}
#else
			g_free (command);
			command = e2_utils_strcat ("file -bhprs --mime-type ", qp);
			if (e2_fs_get_command_output (command, (gpointer *) &mime))
			{
				g_strstrip (mime); //strip trailing \n and any whitespace
				if (*mime == '\0')
				{
					g_free (mime);
					mime = NULL;
				}
			}
#endif
		}
		g_free (qp);
		g_free (command);
#ifdef E2_VFS
	}
	else
	{
# ifdef E2_VFSTMP
		//FIXME get mime for GFile
# else
		mime = g_strdup ("unknown");
# endif
	}
#endif
	return mime;
}
/**
@brief get name of current locale's default character encoding, with fallback

@param encoding store for pointer to name string
@return
*/
void e2_utils_get_charset (const gchar **encoding)
{
	g_get_charset (encoding);
	if (*encoding == NULL)
	//might as well use fs fallback encoding as any other !
		*encoding = e2_cl_options.fallback_encoding;
}
/**
@brief convert utf8 string @a string to lower case

@param string utf-8 string to be processed
@return newly allocated lc string: must be freed
*/
gchar *e2_utils_str_to_lower (gchar *string)
{
	return g_utf8_strdown (string, -1);
}
/**
@brief append @a string to the full path of the dir shown in @a view

@param view pointer to view data struct
@param string the string which will be appended, localised or utf-8, in accord with @a localised
@param localise TRUE to localise dir path before joining

@return newly allocated joined localised or utf8 string, or NULL on error
*/
gchar *e2_utils_dircat (ViewInfo *view, const gchar *string, gboolean localise)
{
	gchar *dir, *joined;
	dir = (localise) ? D_FILENAME_TO_LOCALE (view->dir) : view->dir;
	joined = e2_utils_strcat (dir, string);
	if (localise)
		g_free (dir);
	return joined;
}
/**
@brief join strings @a string1 and @a string2

This is for localised strings, for which
g_strconcat() (maybe?) shouldn't be used
TODO allocation-error message needs BGL closed

@param string1 the string which will start the returned string
@param string2 the string which will be appended
@return newly allocated joined string, or NULL upon error
*/
gchar *e2_utils_strcat (const gchar *string1, const gchar *string2)
{
	gint len1 = strlen (string1);
	gint len2 = strlen (string2) + sizeof (gchar);	//include the trailing 0
	gchar *result = g_try_malloc (len1 + len2);
	CHECKALLOCATEDWARN (result, return NULL;); //TODO assumes BGL closed
	if (result != NULL)
	{
#ifdef __USE_GNU
		gchar *next = mempcpy (result, string1, len1);
		next = mempcpy (next, string2, len2);
#else
		memcpy (result, string1, len1);
		memcpy (result + len1, string2, len2);
#endif
	}
	return result;
}
/**
@brief Like strsplit() but retains the delimiter as part of the string

@param string string to be split
@param delimiter separator string
@param max_tokens maximum number of separated parts, or -1 for no limit
@return NULL-terminated array of strings, or NULL if error occurred
*/
gchar **e2_utils_str_breakup (const gchar *string, const gchar *delimiter,
	gint max_tokens)
{
	GSList *string_list = NULL, *slist;
	gchar *s, *casefold, *new_string;
	gchar **str_array;
	guint i, n = 1;

	g_return_val_if_fail (string != NULL, NULL);
	g_return_val_if_fail (delimiter != NULL, NULL);

	if (max_tokens < 1)
		max_tokens = G_MAXINT;

	s = strstr (string, delimiter);
	if (s != NULL)
	{
		guint delimiter_len = strlen (delimiter);

		do
		{
			guint len = s - string + delimiter_len;
			new_string = NEW (gchar, len + 1);
			if (new_string == NULL && string_list != NULL)
			{
				g_slist_foreach (string_list, (GFunc) g_free, NULL);
				g_slist_free (string_list);
			}
			g_return_val_if_fail (new_string != NULL, NULL);
			g_strlcpy (new_string, string, len);
			new_string[len] = 0;
			casefold = g_utf8_casefold (new_string, -1);
			g_free (new_string);
			new_string = g_utf8_normalize (casefold, -1, G_NORMALIZE_ALL);
			g_free (casefold);
			string_list = g_slist_prepend (string_list, new_string);
			n++;
			string = s + delimiter_len;
			s = strstr (string, delimiter);
		} while (--max_tokens && s);
	}

	if (*string)
	{
		n++;
		casefold = g_utf8_casefold (string, -1);
		new_string = g_utf8_normalize (casefold, -1, G_NORMALIZE_ALL);
		g_free (casefold);
		string_list = g_slist_prepend (string_list, new_string);
	}

	str_array = NEW (gchar*, n);
	if (str_array == NULL && string_list != NULL)
	{
		g_slist_foreach (string_list, (GFunc) g_free, NULL);
		g_slist_free (string_list);
	}
	g_return_val_if_fail (str_array != NULL, NULL);

	i = n - 1;

	str_array[i--] = NULL;
	for (slist = string_list; slist; slist = slist->next)
		str_array[i--] = slist->data;

	g_slist_free (string_list);

	return str_array;
}
/**
@brief find the first (if any) "normal" occurrence of 1-byte char @a c in @a string
In this context, a normal occurrence is (sort-of) outside quotes, and not escaped.
Intended for finding whitespace or punctuation, and so uses ascii scanning
@param string the string which is to be examined
@param c the character to scan for
@return pointer to the found char, or NULL if not found
*/
gchar *e2_utils_bare_strchr (gchar *string, gchar c)
{
	gchar *p = string;
	gint cnt1 = 0; //counter for ' chars
	gint cnt2 = 0; //counter for " chars
	gint prevq = 0; //quote-type most-recently found, for processing nests
	//FIXME make this bullet-proof for any arbitrary combination of ' and/or "
	//(is bad for detecting "real" nested quotes of same type e.g. "" c "")
	while (*p != '\0')
	{
		if (*p == '\'')
		{
			if (p == string || *(p-1) != '\\')
			{
				cnt1++;
				if (prevq == 2	//handle nested quotes
					&& cnt2 % 2 == 1
					&& cnt1 % 2 == 0
					)
						cnt2++; //CHECKME reset to 0 if not really using count, depth etc
				prevq = 1;
			}
		}
		else if (*p == '"')
		{
			if (p == string || *(p-1) != '\\')
			{
				cnt2++;
				if (prevq == 1
					&& cnt1 % 2 == 1
					&& cnt2 % 2 == 0
					)
						cnt1++;
				prevq = 2;
			}
		}
		else if (*p == c)
		{	//check if separator seems to be outside parentheses
			if (p == string || *(p-1) != '\\')
			{
				if (cnt1 % 2 == 0 && cnt2 % 2 == 0)
					return p; //found one
			}
		}
		p++;
	}
	return NULL;
}
/**
@brief find the first (if any) space or tab from the start of @a string
This uses ascii scanning
@param string the string which is to be processed
@return pointer to the whitespace char, or NULL if not found
*/
gchar *e2_utils_find_whitespace (gchar *string)
{
	register gchar c;
	gchar *s = string;
	while ((c = *s) != '\0')
	{
		if (c == ' ' || c == '\t')
			return s;
		s++;
	}
	return NULL;
}
/**
@brief find the first (if any) non-space-or-tab from the start of @a string
This uses ascii scanning
@param string the string which is to be processed
@return pointer to the non-whitespace char, or NULL if not found
*/
gchar *e2_utils_pass_whitespace (gchar *string)
{
	register gchar c;
	gchar *s = string;
	while ((c = *s) != '\0')
	{
		if (!(c == ' ' || c == '\t'))
			return s;
		s++;
	}
	return NULL;
}
//tag E2_BADQUOTES
/**
@brief get a copy of @a string with surrounding double quotes if not already quoted
If @a string is empty, no quotes are added. Existing quotes may be '' or "".
Any embedded and unescaped char matching the quotes will be escaped
@param string the string which is to be processed

@return newly-allocated string
*/
gchar *e2_utils_quote_string (const gchar *string)
{
	gboolean quoted;
	gchar c;
	gchar *p, *q;
	gchar escaped[3] = { '\\',0,0 };
	gchar *unescaped = escaped + sizeof(gchar);

	c = *string;
	if (c == '"') //single-quoted string works only if double-quotes added
	{
		q = (gchar *) string + strlen (string) - sizeof (gchar);
		quoted = (*q == c);
		if (quoted) //already quoted
		{
			*unescaped = c;	//setup fake strings
			//prevent double-escaping
			//don't ignore existing quotes for cleanup, that stuffs a filename that's actually quoted
			p = e2_utils_str_replace (string, "\\", "\\\\");
			q = e2_utils_str_replace (p, unescaped, escaped);
			g_free (p);
			p = g_strconcat (unescaped, q, unescaped, NULL);
			g_free (q);
		}
	}
	else if (G_LIKELY (c != '\0'))
		quoted = FALSE;
	else	//string is empty
	{
		quoted = TRUE;
		p = g_strdup (string);
	}

	if (!quoted)	//add double-quotes
	{
		*unescaped = '"';
		p = e2_utils_str_replace (string, "\\", "\\\\");
		q = e2_utils_str_replace (p, unescaped, escaped);
		g_free (p);
		p = g_strconcat (unescaped, q, unescaped, NULL);
		g_free (q);
	}
	return p;
}
/**
@brief get a copy of @a string without quotes
Leading and/or trailing whitespace is ignored. Quote chars (if any) may be ' or ".
If quotes are removed, any corresponding escaped quote-char inside @a string is
unescaped
@param string the string which is to be processed

@return newly-allocated string, or NULL if @a string is empty or only whitespace
*/
gchar *e2_utils_unquote_string (const gchar *string)
{
	gboolean quoted;
	gchar c1;
	gchar *p;

	p = e2_utils_pass_whitespace ((gchar *)string);
	if (p == NULL)
		return NULL;
	c1 = *p;
	quoted = (c1 == '"' || c1 == '\'');
	if (quoted)
	{
		gchar c2;
		gchar *q = p + strlen (p) - sizeof (gchar);
getlast:
		c2 = *q;
		if (c1 == c2)
		{
			gchar escaped[3] = { '\\',c1,0 };
			gchar *unescaped = escaped + sizeof(gchar);
			p++;
			p = g_strndup (p, q - p);
			q = e2_utils_str_replace (p, "\\\\", "\\");
			g_free (p);
			p = e2_utils_str_replace (q, escaped, unescaped);
			g_free (q);
		}
		else if (c2 == ' ' || c2 == '\t')
		{
			q--;	//doesn't matter if now at backend of UTF-8 char
			if (q > p)
				goto getlast;
			else
				p = g_strdup (p);
		}
		else
			p = g_strdup (p);
	}
	else
		p = g_strdup (p);

	return p;
}
/**
@brief find the first (if any) space-separated substring in @a string
Leading whitespace in @a string is ignored.
Embedded escaped chars (esp. " or ') may be present in the returned string,
whether or not that's quoted.
@param string the string which is to be processed
@param quoted TRUE to return the substring with surrounding "" or '', "" added if necessary
@return newly-allocated string, or NULL if no substring found
*/
gchar *e2_utils_get_first_part (gchar *string, gboolean quoted)
{
	gchar *s1, *s2;
	gboolean quotednow;
	gchar c, n;
	gchar q[2] = { 0,0 };	//fake string

	s1 = e2_utils_pass_whitespace (string);
	if (s1 == NULL)
		return NULL;
	c = *s1;
	quotednow = (c == '"' || c == '\'');
	if (quoted)
	{
		if (quotednow)
		{
			s2 = ++s1;
			while ((s2 = strchr (s2, c)) != NULL)
			{
				if (G_LIKELY (s2 > s1))
				{
					if (*(s2 - sizeof (gchar)) != '\\'	//not escaped
						|| s2 == s1 + sizeof (gchar)	//2 adjacent quotes at start
						|| *(s2 - 2 * sizeof (gchar)) == '\\' //double-escaped == not really
					   )
					{
						n = *(++s2);
						if (n == ' ' || n == '\t' || n == '\0')
							break;
					}
					else
						s2++;
				}
				else	//found at start
				{
					n = *(++s2);
					if (n == ' ' || n == '\t' || n == '\0')
						break;
				}
			}
			s1--;
			q[0] = c;
			s2 = (s2 != NULL) ?
				g_strndup (s1, s2 - s1):	//include the found " or '
				e2_utils_strcat (s1, q);
//tag E2_BADQUOTES
			s1 = e2_utils_quote_string (s2);
			g_free (s2);
		}
		else	//add quotes
		{
			s2 = e2_utils_find_whitespace (s1);
			if (s2 != NULL)
			{
				c = *s2;
				*s2 = '\0';
			}
			else
				c = 0;	//warning prevention
			//quote part- or whole-string CHECKME ok to escape embedded "
			s1 = e2_utils_quote_string (s1);
			if (s2 != NULL)
				*s2 = c;
		}
	}
	else	//want substring without quotes
	{
		if (quotednow)
		{	//strip quotes
			s1++;
			s2 = s1;
			while ((s2 = strchr (s2, c)) != NULL)
			{
				if (G_LIKELY (s2 > s1))
				{
					if (*(s2 - sizeof (gchar)) != '\\'	//not escaped
						|| s2 == s1 + sizeof (gchar)	//2 adjacent quotes at start
						|| *(s2 - 2 * sizeof (gchar)) == '\\' //double-escaped == not really
					   )
					{
						n = *(++s2);
						if (n == ' ' || n == '\t' || n == '\0')
						{
							s2--; //back to the quote
							break;
						}
					}
					else
						s2++;
				}
				else	//found at start
				{
					n = *(++s2);
					if (n == ' ' || n == '\t' || n == '\0')
						break;
				}
			}
			gchar escaped[3] = { '\\',c,0 };
			gchar *unescaped = escaped + sizeof(gchar);
			if (s2 != NULL)
			{	//unescape quoted substring
				s1 = g_strndup (s1, s2 - s1);
				s2 = e2_utils_str_replace (s1, "\\\\", "\\");
				g_free (s1);
			}
			else	//no closing quote
			{
				s2 = e2_utils_str_replace (s1, "\\\\", "\\");
			}
			s1 = e2_utils_str_replace (s2, escaped, unescaped);
			g_free (s2);
		}
		else
		{	//get part without quotes
			s2 = e2_utils_find_whitespace (s1);
			s1 = (s2 != NULL) ? g_strndup (s1, s2 - s1) : g_strdup (s1);
		}
	}
	return s1;
}
/*gchar *get_key_name (gint keyval)
{
	return gdk_keyval_name (gdk_keyval_to_lower (keyval));
} */
static gchar *prefix = NULL;
static gulong savecount = 0;	//so initial used value defaults to 1

/**
@brief Replace macro(s) in string @a text with appropriate value(s)

Supported macro codes are %c, [%]%d, [%]%D, [%]%e, [%]%E, [%]%f, [%]%F,
[%]%p, [%]%P(= F), [%]%o, [%]%O, %t, %{...} %$...$. Also *
These 'letters' are hard-coded CHECKME should the letters be translatable, non-ascii ?
(%c macro is also processed in rename plugin, so that counter(s) can be recorded
for incrementation purposes)
Expects BGL on/closed
@param text UTF-8 (or ascii) string, possibly with macros to be expanded
@param for_each UTF-8 string with name(s) or path(s) of item(s) to substitute for any "%e" "%f" or "%p", or NULL to use selected items

@return newly allocated string, or NULL if failure, or 0x1 if prompt macro cancelled
*/
gchar *e2_utils_expand_macros (const gchar *text, const gchar *for_each)
{
	GString *command_string = g_string_new ("");
	gchar *s, *free_this, *command_copy, *utf;

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, expand macros");
#endif
	command_copy = g_strdup (text);
	free_this = s = command_copy;
	while ((s = strchr (command_copy, '%')) != NULL)	//if always ascii %, don't need g_utf8_strchr()
	{
		*s = '\0';
		s++;
		g_string_append (command_string, command_copy);
		gboolean with_quotes = TRUE;
		command_copy = s + sizeof (gchar); //CHECKME just s?
		if (*s == '%')
		{
			s++;
			command_copy++;
			with_quotes = FALSE;
		}
		switch (*s)
		{
#ifdef E2_BADQUOTES
			case 'e':
#endif
			case 'f':
			case 'p':
			{
				gchar *qp;
				if (for_each != NULL)	//use specified item name
				{
					gchar *name, *p;
					//the supplied name may or may not have a path
					name = g_path_get_basename (for_each);
					if (!strcmp (name, for_each)) //no path in for_each
						p = (*s == 'p') ?
							e2_utils_dircat (curr_view, for_each, FALSE) : (gchar *)for_each;
					else //path in for_each
						p = (*s == 'p') ? (gchar *)for_each : name;
					if (with_quotes)	//use existing "" or '' or added ""
					{
//tag E2_BADQUOTES
						qp = e2_utils_quote_string (p);
						command_string = g_string_append (command_string, qp);
						g_free (qp);
					}
					else
					{
#ifdef E2_BADQUOTES
						if (*s == 'e')	//escape only
						{
							qp = e2_utils_quote_string (p);
							gchar *e = qp + strlen (qp) - sizeof (gchar);
							*e = '\0';
							command_string = g_string_append (command_string, qp + sizeof (gchar));
							g_free (qp);
						}
						else
#endif
							command_string = g_string_append (command_string, p);
					}

					g_free (name);
					if (p != for_each && p != name)
						g_free (p);
				}
				else	//use selected items
				{
					e2_filelist_disable_one_refresh (PANEACTIVE);  //prevent any change to the selected items ?
					GList *sel = e2_fileview_get_selected_local (curr_view);
#ifdef E2_REFRESH_DEBUG
					printd (DEBUG, "enable refresh, expand macros 1");
#endif
					if (sel == NULL)
					{
						e2_filelist_enable_one_refresh (PANEACTIVE);
						e2_output_print_error (_("No item selected"), FALSE);
						//CHECKME continue parsing instead of aborting
						g_free (free_this);
						g_string_free (command_string, TRUE);
						return NULL;
					}
					else
					{
						GList *member;
						for (member = sel; member != NULL; member = member->next)
						{
							//refresh disabled, no need for D_FILENAME_FROM_LOCALE ?
							utf = F_FILENAME_FROM_LOCALE (((FileInfo *)member->data)->filename);
							gchar *replace = (*s == 'p') ?
								e2_utils_strcat (curr_view->dir, utf):
								g_strdup (utf);
							if (prefix != NULL)
							{
								gchar *freeme = replace;
								replace = e2_utils_strcat (prefix, replace);
								g_free (freeme);
							}
							if (with_quotes)
							{
//tag E2_BADQUOTES
								qp = e2_utils_quote_string (replace);
								command_string = g_string_append (command_string, qp);
								g_free (qp);
							}
							else
							{
#ifdef E2_BADQUOTES
								if (*s == 'e')	//escape only
								{
									qp = e2_utils_quote_string (replace);
									gchar *e = qp + strlen (qp) - sizeof (gchar);
									*e = '\0';
									command_string = g_string_append (command_string, qp + sizeof (gchar));
									g_free (qp);
								}
								else
#endif
									command_string = g_string_append (command_string, replace);
							}

							g_free (replace);

							if (member->next != NULL)
								command_string = g_string_append_c (command_string, ' ');
							F_FREE (utf, ((FileInfo *)member->data)->filename);
						}
						g_list_free (sel);
						e2_filelist_enable_one_refresh (PANEACTIVE);
					}
				}
			}
			break;
#ifdef E2_BADQUOTES
			case 'E':
#endif
			case 'F':
			case 'P':
			{
				e2_filelist_disable_one_refresh (PANEINACTIVE);
				GList *sel = e2_fileview_get_selected_local (other_view);
#ifdef E2_REFRESH_DEBUG
				printd (DEBUG, "enable refresh, expand macros 2");
#endif
				if (sel == NULL)
				{
					e2_filelist_enable_one_refresh (PANEINACTIVE);
					e2_output_print_error (_("No item selected in other pane"), FALSE);
					//FIXME continue parsing instead of aborting
					g_free (free_this);
					g_string_free (command_string, TRUE);
					return NULL;
				}
				else
				{
					GList *member;
					for (member = sel; member != NULL; member = member->next)
					{
						gchar *qp;
						//refresh disabled, no need for D_FILENAME_FROM_LOCALE
						utf = F_FILENAME_FROM_LOCALE (((FileInfo *)member->data)->filename);
						gchar *replace = (*s == 'P') ?
							e2_utils_strcat (other_view->dir, utf):
							g_strdup (utf);
						if (prefix != NULL)
						{
							gchar *freeme = replace;
							replace = e2_utils_strcat (prefix, replace);
							g_free (freeme);
						}
						if (with_quotes)
						{
//tag E2_BADQUOTES
							gchar *qp = e2_utils_quote_string (replace);
							command_string = g_string_append (command_string, qp);
							g_free (qp);
						}
						else
						{
#ifdef E2_BADQUOTES
							if (*s == 'E')	//escape only
							{
								qp = e2_utils_quote_string (replace);
								gchar *e = qp + strlen (qp) - sizeof (gchar);
								*e = '\0';
								command_string = g_string_append (command_string, qp + sizeof (gchar));
								g_free (qp);
							}
							else
#endif
								command_string = g_string_append (command_string, replace);
						}

						if (member->next != NULL)
							command_string = g_string_append_c (command_string, ' ');
						g_free (replace);
						F_FREE (utf, ((FileInfo *)member->data)->filename);
					}
					g_list_free (sel);
					e2_filelist_enable_one_refresh (PANEINACTIVE);
				}
			}
			break;
			case 'd':
			case 'D':
			{
				gchar *s1, *s2;
				s1 = (*s == 'd') ? curr_view->dir : other_view->dir;
				s1 = g_strdup (s1);
				s2 = s1 + strlen (s1) - sizeof(gchar);
				if (s2 > s1 && *s2 == G_DIR_SEPARATOR)
					*s2 = '\0';
				//no auto shell-style quoting/escaping for dir path as the user
				//can manually quote a %d or %D
				s2 = (with_quotes) ? "\"%s\"" : "%s" ;
				g_string_append_printf (command_string, s2, s1);
				g_free (s1);
			}
				break;
			case 'o':
			case 'O':
			{
				gchar *s1;
				s1 = e2_output_get_active_text (*s == 'O');
				if (s1 != NULL && *s1 != '\0')
				{
					gchar *s2;
					s2 = (with_quotes) ?
						e2_utils_quote_string (s1) : e2_utils_unquote_string (s1);
					command_string = g_string_append (command_string, s2);
					g_free (s2);
				}
				g_free (s1);
			}
				break;
			case 'c':
			{
				gchar numfmt[20];
				gulong count, width;
				//parse count and width
				s++;
				count = strtoul (s, &command_copy, 10);
				if (command_copy == s)	//no number provided
					count = ++savecount;	//use stored value
				else
				{
					savecount = count;
					s = command_copy;
				}
				if (*s == ',')	//no whitespace check
				{
					s++;
					width  = strtoul (s, &command_copy, 10);
					if  (command_copy == s)
						width = 1;	//no number provided
					else
						s = command_copy;
				}
				else
					width = 1;
				numfmt[0] = '%';
				//create count string using value and width
				if (width > 1)
					g_snprintf (numfmt+1, sizeof(numfmt)-1, "0%uu", (guint) width);
				else
					g_strlcpy (numfmt+1, "u", sizeof(numfmt)-1);
				g_string_append_printf (command_string, numfmt, count);

				command_copy = s;
			}
				break;
			case 't':
			{
				gchar *tmp = e2_utils_get_temp_path (NULL);
				g_string_append (command_string, tmp);	//no quoting
				g_free (tmp);
			}
				break;
			case '!':
			case 'h':
			{
				gchar *s1, *s2, *beginswith = NULL;
				gboolean wordfind, ignore;
				glong back, firstword = 0, lastword = 0;
				s1 = s + sizeof (gchar);
				s2 = NULL;
				//how far back in the history maybe specified
				back = strtol (s1, &s2, 10);
				if (back == 0) //no conversion or entered 0
					back = 1;
				else if (back < 0) //-n is allowed
					back = -back;
				if (s2 > s1)
					s1 = s2;
				wordfind = ignore = FALSE;
reswitch:
				switch (*s1)
				{
					case ' ':
					case '\t':
					case '\n':
					case '\r':
					case '(':
						ignore = TRUE;
						break; //un-recognised sequence
					case '!':
						back = 1;
						s1++;
						goto reswitch;
					case ':':
						wordfind = TRUE;
						s1++;
						goto reswitch;
					case '^':
						firstword = 1;
						lastword = 1;
						s2 = s1 + sizeof (gchar);
						break;
					case '$':
						firstword = -1;
						lastword = -1;
						s2 = s1 + sizeof (gchar);
						break;
					case '*':
						firstword = 1;
						lastword = -1;
						s2 = s1 + sizeof (gchar);
						break;
					case '-':
						if (wordfind)
						{
							firstword = 0;
							s1++;
							if (*(s1) == '$')
							{
								lastword = -1;
								s2 = s1 + sizeof (gchar);
								break;
							}
							else
							{
								lastword = strtol (s1, &s2, 10);
								if (s2 > s1 && lastword >= 0) //conversion done
									break;

								ignore = TRUE;
							}
						}
						//cleanup and abort
//						e2_output_print_error (_("Invalid command-history-macro"), FALSE);
//						g_string_free (command_string, TRUE);
//						return NULL;
						break;
					default:
						if (wordfind)
						{
							//check for [n][][-[][m]]
							firstword = strtol (s1, &s2, 10);
							if (s2 > s1)
							{
								if (firstword < 0)
								{
									lastword = -firstword;
									firstword = 0;
								}
								else //a +ve number found, for first word
								{
									s1 = s2;
									lastword = strtol (s1, &s2, 10);
									if (s2 > s1)
									{
										if (lastword < 0)
											lastword = -lastword;
										else
										{
											//handle []-[]$
											s2 = e2_utils_pass_whitespace (s1);
											if (s2 != NULL && *s2 == '-')
											{
												s2++;
												if (*s2 == '$')
												{
													lastword = -1;
													s2++;
												}
												else
												{
													//FIXME
													ignore = TRUE;
												}
											}
										}
									}
									else //no conversion for a second number
									{
										//handle []-[]$
										s2 = e2_utils_pass_whitespace (s1);
										if (s2 != NULL && *s2 == '-')
										{
											s2++;
											if (*s2 == '$')
											{
												lastword = -1;
												s2++;
											}
											else
											{
												//FIXME
												ignore = TRUE;
											}
										}
										else //nothing found, so just a single number
										{
											lastword = firstword;
											s2 = s1;
										}
									}
								}
							}
						}
						else //assume searched command is wanted
							if ((s2 = e2_utils_bare_strchr (s1, ':')) != NULL)
						{
							beginswith = g_strndup (s1, s2-s1);
							s1 = s2 + sizeof (gchar);
							wordfind = TRUE;
							firstword = 0; //default to minimal (safe) part of prior command
							lastword = 0;
							goto reswitch;
						}
						else //use some or all of this command as search string
						{
							s2 = e2_utils_bare_strchr (s1, ' ');
							beginswith = (s2 != NULL) ?
								g_strndup (s1, s2-s1) : g_strdup (s1);
							firstword = 0;
							lastword = 0;
							if (s2 == NULL)
								s2 = s1 + strlen (s1); //to end of the used string
						}
						break;
				}

				if (ignore)
				{
					g_free (beginswith);
					break;
				}

				//get the command and its wanted part(s)
				const gchar *priorcmd = e2_task_find_prior_command (back, beginswith);
				g_free (beginswith);

				if (priorcmd != NULL)
				{
					GString *history = g_string_new ("");
					const gchar *p = priorcmd;
					glong indx = 0;

					if (firstword >= 0 && lastword >= 0 && lastword < firstword)
					{
						glong tmp = firstword;
						firstword = lastword;
						lastword = tmp;
					}

					while (*p != '\0')
					{
						if (firstword == -1)
							history = g_string_truncate (history, 0); //ignore previous words

						gchar *sep = e2_utils_bare_strchr ((gchar *)p, ' ');
						if (sep != NULL)
						{
							if (++indx > firstword)
							{
								//append this word
								gchar *thisword = g_strndup (p, sep - p);
								history = g_string_append_c (history, ' ');
								history = g_string_append (history, thisword);
								g_free (thisword);
							}
							if (indx > lastword && lastword != -1)
								break;
							p = e2_utils_pass_whitespace (sep);
							if (p == NULL)
								break;
						}
						else
						{
							//handle last word
							if (++indx > firstword)
							{
								history = g_string_append_c (history, ' ');
								history = g_string_append (history, p);
							}
							break;
						}
					}

					if (indx == lastword + 1 || lastword == -1)
					{
						//good
						command_string = g_string_append (command_string,
							history->str + sizeof (gchar)); //omit leading ' '
						command_copy = s2;
					}
					else
					{
						//FIXME
//						e2_output_print_error (_("No matching command-history item"), FALSE);
//						g_string_free (history, TRUE);
//						g_string_free (command_string, TRUE);
//						return NULL;
					}
					g_string_free (history, TRUE);
				}
				else //no suitable history item
				{
					//FIXME
//					e2_output_print_error (_("No matching command-history item"), FALSE);
//					g_string_free (command_string, TRUE);
//					return NULL;
				}
			}
				break;
			case '{':
				if ((s = strchr (command_copy, '}')) == NULL)	//always ascii }, don't need g_utf8_strchr()
				{
					e2_output_print_error (_("No matching '}' found in action text"), FALSE);
					g_free (free_this);
					g_string_free (command_string, TRUE);
					return NULL;
				}
				else
				{
					*s = '\0';	//end of bracketed text
					gchar *user_input, *cend, *sep, *cleaned;
					DialogButtons result;
//tag PASSWORDINPUT gboolean hidden = FALSE;
					gboolean has_history = FALSE;
					GList *thishistory = NULL;
					sep = command_copy;
					//FIXME a better syntax (but | separator makes command look like a pipe)
					while ((sep = strchr (sep, '@')) != NULL)
					{
						if (*(sep-1) == '\\' || *(sep+1) == '@' || *(sep-1) == '@')
							sep++;
						else
							break;
					}
					if (sep != NULL)
					{
						while (command_copy < sep)
						{
							if (*command_copy == '(')
							{
								command_copy++;
								*sep = '\0';
								if ((cend = strchr (command_copy, ')')) != NULL)
								{
									*cend = '\0';
									has_history = TRUE;
									break;
								}
							}
/*tag PASSWORDINPUT
							else if (*command_copy == '*')
							{
								hidden = TRUE;
								break;
							}
*/							command_copy++;	//keep looking
						}
					}
					if (//!hidden &&
						!has_history)
						cleaned = e2_utils_str_replace (command_copy, "\\@", "@");
					else
						cleaned = e2_utils_str_replace (sep+1, "\\@", "@");
/*tag PASSWORDINPUT
					if (hidden)
						result = e2_dialog_password_input (NULL, cleaned, &user_input);

					else
*/						if (has_history)
					{
						e2_cache_list_register (command_copy, &thishistory);
						result = e2_dialog_combo_input (NULL, cleaned, NULL, 0,
							&thishistory, &user_input);
						e2_cache_unregister (command_copy);	//backup the history list
						e2_list_free_with_data (&thishistory);
					}
					else	//default
						result = e2_dialog_line_input (NULL, cleaned, "", 0,
							FALSE, &user_input);
					g_free (cleaned);

					command_copy = s+1;
					if (result == OK)
					{	//a blank entry will not return OK
						//re-enter to expand %f etc in input
						gchar *expinput = e2_utils_expand_macros (user_input, for_each);
						if (expinput > (gchar *)1)	//no check for 1 (no nested inputs)
						{
							g_string_append (command_string, expinput);
							g_free (expinput);
						}
						g_free (user_input);
					}
					else
					{
						g_free (free_this);
						g_string_free (command_string, TRUE);
						return GINT_TO_POINTER (1);	//1 is cancel signal
					}
				}
				break;
			case '$':
				if ((s = strchr (command_copy, '$')) == NULL)	//if always ascii }, don't need g_utf8_strchr()
				{
					e2_output_print_error (_("No matching '$' found in action text"), FALSE);
					g_free (free_this);
					g_string_free (command_string, TRUE);
					return NULL;
				}
				else if (s > command_copy) //ignore $$
				{
					prefix = command_copy;	//store prefix for use in rest of expansion
					*s = '\0';
					gboolean freepfx;
					if (strchr (prefix, '%') != NULL)
					{
						prefix = e2_utils_expand_macros (prefix, NULL);
						freepfx = TRUE;
					}
					else
						freepfx = FALSE;
					command_copy = s+1;
					//re-enter to expand %f etc in rest of string
					gchar *expinput = e2_utils_expand_macros (command_copy, for_each);
					if (freepfx)
						g_free (prefix);
					prefix = NULL;
					if (expinput > (gchar *)1)
					{
						g_string_append (command_string, expinput);
						s = command_string->str;
						g_free (expinput);
						g_free (free_this);
						g_string_free (command_string, FALSE);
						return s;
					}
				}
				break;
			default:
				g_string_append_c (command_string, '%');
				g_string_append_c (command_string, *s);
				break;
		}
	}
	g_string_append (command_string, command_copy);
	g_free (free_this);
	s = command_string->str;
	g_string_free (command_string, FALSE);
	return s;
}
/**
@brief helper function to do macro replacement
@param text string to be processed
@param unquoted string to replace any %p %f %e in @a text
@param quoted string to replace any %%p %%f %%e in @a text

@return newly-allocated string with any replacement(s) done
*/
static gchar *_e2_utils_replace_names (const gchar *text, const gchar *unquoted,
	const gchar *quoted)
{
	gchar *freeme;
	gchar *retval = g_strdup (text);
	if (strstr (retval, "%%p") != NULL)
	{
		freeme = retval;
		retval = e2_utils_str_replace (retval, "%%p", unquoted);
		g_free (freeme);
	}
	if (strstr (retval, "%p") != NULL)
	{
		freeme = retval;
		retval = e2_utils_str_replace (retval, "%p", quoted);
		g_free (freeme);
	}
	if (strstr (retval, "%%f") != NULL)
	{
		freeme = retval;
		retval = e2_utils_str_replace (retval, "%%f", unquoted);
		g_free (freeme);
	}
	if (strstr (retval, "%f") != NULL)
	{
		freeme = retval;
		retval = e2_utils_str_replace (retval, "%f", quoted);
		g_free (freeme);
	}
#ifdef E2_BADQUOTES
	if (strstr (retval, "%%e") != NULL)
	{
		//remove the quotes (of either sort) without un-escaping
		gchar *qp = g_strdup (quoted);
		gchar *p = qp + strlen (qp) - sizeof (gchar);
		*p = '\0';

		freeme = retval;
		retval = e2_utils_str_replace (retval, "%%e", qp + sizeof (gchar));
		g_free (freeme);

		g_free (qp);
	}
	if (strstr (retval, "%e") != NULL)
	{
		freeme = retval;
		retval = e2_utils_str_replace (retval, "%e", quoted);
		g_free (freeme);
	}
#endif
	return retval;
}
/**
@brief replace instance(s) of macros [%]%e [%]%f and [%]%p in @a text, with @a itempath
This is intended for populating a generic command string with a specific item.
It differs from e2_utils_expand_macros() due to no distinction between [%]%p and
[%]%f (@a itempath always has full path).
@param text string to be processed
@param itempath replacement string (should be same encoding as @a text)

@return newly-allocated replacement string, or @a text if no replacement is actually needed
*/
gchar *e2_utils_replace_name_macros (const gchar *text, const gchar *itempath)
{
//tag E2_BADQUOTES
	gchar *quoted = e2_utils_quote_string (itempath);
	gchar *retval = _e2_utils_replace_names (text, itempath, quoted);
	g_free (quoted);
	if (!strcmp (text, retval))
	{
		g_free (retval);
		retval = (gchar *)text;	//un-allocated version
	}
	return retval;
}
/**
@brief replace instance(s) of macros [%]%e [%]%f and [%]%p in @a text, with item(s) in @a names
Any %p will force a path to always be prepended for any %f, %e, and also prevent
escaping. Otherwise, any %e will force ecaping for any %f. The first-detected
[p/e/f] macro will determine quoting.
@param text UTF-8 string to be processed
@param path absolute UTF-8 path (with or without trailer), or NULL to use active pane dir
@param names array of selected items (localised)
@param single TRUE to use only the first item of @a array

@return newly-allocated replacement string, or @a text if no replacement done
*/
gchar *e2_utils_replace_multiname (const gchar *text, gchar *path, GPtrArray *names,
	gboolean single)
{
	gboolean quoted, pathed;
#ifdef E2_BADQUOTES
	gboolean escaped;
#endif
	gchar *retval, *usepath, *utf, *s;
	guint count;
	GString *joined;

	if (names->len == 0)
		return (gchar *)text;
	if ((s = strstr (text, "%p")) != NULL)
	{
		if (G_LIKELY (s > text))
			quoted = *(s - sizeof (gchar)) != '%';
		else
			quoted = TRUE;
		pathed = TRUE;
#ifdef E2_BADQUOTES
		escaped = FALSE;
#endif
	}
#ifdef E2_BADQUOTES
	else if ((s = strstr (text, "%e")) != NULL)
	{
		if (G_LIKELY (s > text))
			quoted = *(s - sizeof (gchar)) != '%';
		else
			quoted = TRUE;
		pathed = FALSE;
		escaped = TRUE;
	}
#endif
	else if ((s = strstr (text, "%f")) != NULL)
	{
		if (G_LIKELY (s > text))
			quoted = *(s - sizeof (gchar)) != '%';
		else
			quoted = TRUE;
		pathed = FALSE;
#ifdef E2_BADQUOTES
		escaped = FALSE;
#endif
	}
	else
		return (gchar *)text;

	//get path from supplied parameter, or active pane
	if (pathed)
	{
		if (path != NULL)
		{
			usepath = g_strdup (path);
			s = usepath + strlen (usepath) - sizeof(gchar);
			if (s > usepath && *s != G_DIR_SEPARATOR)
			{
				s = usepath;
				usepath = e2_utils_strcat (usepath, G_DIR_SEPARATOR_S);
				g_free (s);
			}
		}
		else
			usepath = g_strdup (curr_view->dir);
	}
	else
		usepath = NULL;	//warning prevention

	joined = g_string_sized_new (256);
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	for (count = 0; count < names->len; count++, iterator++)
	{
		utf = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
#ifdef E2_BADQUOTES
		if (escaped)
		{
			s = (pathed) ? e2_utils_strcat (usepath, utf) : utf;
			gchar *qp = e2_utils_quote_string (s);
			if (pathed)
				g_free (s);
			if (quoted) //|| *qp != '"')
				s = qp;
			else
			{
				s = qp + strlen (qp) - sizeof (gchar);
				*s = '\0';
				s = qp + sizeof (gchar);
			}
			joined = g_string_append (joined, s);
			g_free (qp);
		}
		else
		{
#endif
			if (quoted)
				joined = g_string_append_c (joined, '"');
			if (pathed)
				joined = g_string_append (joined, usepath);	//path + separator
			joined = g_string_append (joined, utf);
			if (quoted)
				joined = g_string_append_c (joined, '"');
#ifdef E2_BADQUOTES
		}
#endif
		F_FREE (utf, (*iterator)->filename);
		joined = g_string_append_c (joined, ' ');
		if (single && (count == 0))
			break;
	}
	joined = g_string_truncate (joined, joined->len - 1);	//clear last ' '

	if (pathed)
		g_free (usepath);
#ifdef E2_BADQUOTES
	if (escaped)
		text = e2_utils_str_replace (text, "%e", "%f");	//don't want downstream escaping
#endif

	retval = _e2_utils_replace_names (text, joined->str, joined->str);

	g_string_free (joined, TRUE);
#ifdef E2_BADQUOTES
	if (escaped)
		g_free ((gchar *)text);
#endif
	return retval;
}
/**
@brief construct a temporary local itemname by adding a number-suffix to @a localpath
@param localpath path of item to be processed, localised string
@return newly-allocated, localised, path string comprising the temp name
*/
gchar *e2_utils_get_tempname (const gchar *localpath)
{
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = NULL;
#endif
	gchar *temppath;
	guint i = 0;
	while (TRUE)
	{
		temppath = g_strdup_printf ("%s.tmp~%d", localpath, i);	//no translation or utf-8 conversion needed
		if (i == 0)
		{	//first attempt has no "~N" suffix
			gchar *s = strrchr (temppath, '~');
			*s = '\0';
		}
		E2_ERR_DECLARE
#ifdef E2_VFS
		ddata.path = temppath;
		if (e2_fs_access2 (&ddata E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#else
		if (e2_fs_access2 (temppath E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#endif
		{
			E2_ERR_CLEAR
			break;
		}
		E2_ERR_CLEAR
		g_free (temppath);
		i++;
	}
	return temppath;
}
/**
@brief truncate @a path at the right-most separator, if any
Multiple adjacent separators are treated as one.
@param path path string to be processed
@param ignore_trailer TRUE to strip any trailing separator before checking

@return TRUE if parent path was created
*/
gboolean e2_utils_get_parent_path (gchar *path, gboolean ignore_trailer)
{
	gchar *s;
	gint len = strlen (path);
	if (ignore_trailer)
	{
		s = path + len - sizeof (gchar);
		if (s > path && *s == G_DIR_SEPARATOR)
		{
			*s = '\0';
			len -= sizeof (gchar);
		}
	}
#ifdef E2_VFSTMP
	//FIXME handle going past namespace root
#endif
	if (len == sizeof (gchar))
		return FALSE;	//path this short may be root or not, but can't have a parent
	s = strrchr (path, G_DIR_SEPARATOR);
	if (s == NULL)
		return FALSE;
	if (s > path)
	{
		while (*(--s) == G_DIR_SEPARATOR && s > path) {}
	}
	*(s + sizeof (gchar)) = '\0';
	return TRUE;
}
/**
@brief ensure path string @a path has appropriate separators and embedded relativities

Removes leading whitespace, superfluous separators, and "./", "../" from inside
@a path, but doesn't change any leading instance(s) of "../".
Appends separator if not one there already.
If possible, the returned string pointer will be the same as @a path, otherwise it
will be a reallocation of @a path.

@param path absolute or relative UTF-8 path string to be checked, must be freeable

@return UTF-8 path string, as clean as possible, may be same as @a path
*/
gchar *e2_utils_path_clean (gchar *path)
{
	g_strchug (path);	//trailing whitespace may be deliberate, but scrub any leading

	if (path[0] == G_DIR_SEPARATOR && e2_utils_pass_whitespace (path + sizeof (gchar)) == NULL)
		return path;
	//path is not just the file system root
	gchar *clean;
	gint l = strlen (path);
	//if path is already clean but without a trailing separator, we need extra
	//space anyway, might as well work in that space now, and save effort later
	if (*(path + l - sizeof(gchar)) != G_DIR_SEPARATOR)
	{
		clean = g_try_realloc (path, l + 2 * sizeof(gchar));	//typically == path
		CHECKALLOCATEDWARN (clean, return path;)
	}
	else
		clean = path;

	GPtrArray *elements = g_ptr_array_sized_new (10); //start large enough for most cases
	//parse the path into its elements
	gchar *s, *d;
	s = clean;
	g_ptr_array_add (elements, s);	//log start
	while (*s != '\0')
	{
		if (*s == G_DIR_SEPARATOR)
		{
			*s = '\0';
			s++;
			g_ptr_array_add (elements, s); //log next position in case it starts an element
		}
		else
			s++;
	}
	g_ptr_array_add (elements, s); //log closing '\0'
	//cleanups
	guint i;
	for (i = 0; i < elements->len; i++)
	{
		s = (gchar*) elements->pdata[i];
		if (*s == '.')
		{
			if (*(s + sizeof(gchar)) == '\0')
				*s = '\0';	//ignore "./"
			else if (*(s + sizeof(gchar)) == '.' && *(s + 2 * sizeof(gchar)) == '\0')
			{
				l = (gint)i - 1;
				while (l > -1)
				{
					s = (gchar*) elements->pdata[l];
					if (*s != '\0')
					{
						if (*s != '.' || *(s + sizeof(gchar)) != '.' || *(s + 2 * sizeof(gchar)) != '\0')
							break;
					}
					l--;
				}
				if (l > -1)
				{
					for (; l <= i; l++)
					{
						s = (gchar*) elements->pdata[l];
						*s = '\0';
					}
				}

			}
		}
	}
	//re"join"
	d = clean;
	if (*d == '\0')	//reinstate leading separator if any
	{
		*d = G_DIR_SEPARATOR;
		d += sizeof (gchar);
		i = 1;
	}
	else
		i = 0;

	for ( ; i < elements->len - 1; i++)
	{
		s = (gchar*) elements->pdata[i];
		if (*s != '\0')	//ignore elements that are cleaned
		{
			l = elements->pdata[i+1] - elements->pdata[i]; //l may or may not include a former separator
			if (s == d)	//commonly, the path was clean already, so nothing to change
			{
				d += l - sizeof(gchar);
			}
			else
			{
				//slide element down, append '/'
				guint k;
				for (k = 0; k < l; k++)
					*d++ = *s++;
				d -= sizeof(gchar);
			}
			if (*d != '\0')
				d += sizeof(gchar);
			*d = G_DIR_SEPARATOR;
			d += sizeof(gchar);
		}
	}
	*d = '\0';
	g_ptr_array_free (elements, TRUE);

	return clean;
}
/**
@brief ensure @a new_path is absolute and clean

Creates an absolute path string from a relative one, or if the supplied path is
already absolute, just makes sure it is 'clean'.
Works with UTF8 and non-UTF8 path strings.

@param base_dir path string which serves as 'prefix' for @a new_path if that is not absolute
@param new_path path string which is to be made absolute if not already so

@return newly-allocated, cleaned, absolute path with trailing separator
*/
gchar *e2_utils_translate_relative_path (const gchar *base_dir, const gchar *new_path)
{
	gchar *str = (g_path_is_absolute (new_path)) ?
		g_strdup (new_path): //copy so that e2_utils_path_clean() can work on it
		g_build_filename (base_dir, new_path, NULL);
	return (e2_utils_path_clean (str));
}
/**
@brief create relative path of @a src relative to @a dest
If @a src is not an absolute path, it is returned unchanged
This only works for ascii single-byte separator-characters in filepaths. It
expects path strings to be 'clean' i.e. no checks for multiple separators etc
@param src includes the path to be relativised, a localised string
@param dest includes the reference path, a localised string
@return newly-allocated localised path string
*/
gchar *e2_utils_create_relative_path (VPATH *src, VPATH *dest)
{
	const gchar *sp, *dp, *tsp;

	sp = VPCSTR(src);
#ifdef E2_VFS
	if (src->spacedata != dest->spacedata)
		return (g_strdup(sp));
#endif
	if (*sp != G_DIR_SEPARATOR)
		//not an absolute path, we don't want, or don't know how, to relativise
		return g_strdup (sp);

	dp = VPCSTR(dest);
	tsp = NULL;

	// skip common path
	while ((*sp != '\0') && (*sp == *dp))
	{
		sp += sizeof (gchar);
		if (*dp == G_DIR_SEPARATOR)
			tsp = sp; // remember latest 'tail' path
		dp += sizeof (gchar);
	}
	if (tsp != NULL && *tsp != '\0')
	{
#ifdef RELLINK_FIX
		//workaround bad behaviour of [g]libc with relative links
		//if topmost non-common dir in the target path is an existing top-level dir,
		//make the target absolute (cuz otherwise, symlink() strips the relativisers)
#ifdef E2_VFS
		VPATH ddata;
#endif
		gint res;
		gchar *t = strchr (tsp, G_DIR_SEPARATOR);
		if (t != NULL)
			*t = '\0';
#ifdef E2_VFS
		ddata.path = tsp-1;
		ddata.spacedata = localpath->spacedata;
		res = e2_fs_access (&ddata, F_OK E2_ERR_NONE());
#else
		res = e2_fs_access (tsp-1, F_OK E2_ERR_NONE());
#endif
		if (t != NULL)
			*t = G_DIR_SEPARATOR;
		if (res == 0)
			return g_strdup (VPCSTR(src));
#endif //def RELLINK_FIX
		GString *rel = g_string_sized_new (PATH_MAX);
		// insert non-common ancestor dirs (if any) as uplinks
		while (*dp != '\0')
		{
			if (*dp == G_DIR_SEPARATOR)
				g_string_append (rel, ".."G_DIR_SEPARATOR_S);  //.. applies in all languages ?
			dp += sizeof (gchar);
		}
		// and add tail path
		g_string_append (rel, tsp);
		return (g_string_free (rel, FALSE));
	}
	else	//whole path is the same (probably an overwrite happening)
		if (tsp == NULL)
			return g_strdup (VPCSTR(src));	//no separator in paths
	else
		return g_path_get_basename (VPSTR(src));
}
/**
@brief skip past irrelevant leading "./" and/or "../" in the path string of @a localpath
This is for funcs that work with relative paths, like readlink()
@param localpath virtual path with localised string to be processed

@return pointer set to somewhere in the path string of @a localpath
*/
const gchar *e2_utils_skip_relative_path (VPATH *localpath)
{
	gchar *s1, *t = VPSTR(localpath);

	while (*t == '.')
	{
		gchar *s2;
		s1 = t + 1;
		s2 = t + 2;

		if (*s1 == G_DIR_SEPARATOR)
			t = s2;
		else if (*s1 == '.')
		{
			if (*s2 == G_DIR_SEPARATOR)
				t = s2 + 1;
			else
				break;
		}
		else
			break;
	}

	if (t > VPSTR(localpath))
	{
#ifdef E2_VFS
		VPATH ddata;
#endif
		gint res;
		s1 = strchr (t, G_DIR_SEPARATOR);
		if (s1 != NULL)
			*s1 = '\0';
#ifdef E2_VFS
		ddata.path = --t;
		ddata.spacedata = localpath->spacedata;
		res = e2_fs_access (&ddata, F_OK E2_ERR_NONE());
#else
		res = e2_fs_access (--t, F_OK E2_ERR_NONE());
#endif
		if (s1 != NULL)
			*s1 = G_DIR_SEPARATOR;
		if (!res)
			return t;
	}
	return (VPSTR(localpath));
}
/**
@brief get unique temp dir name

@param id localised string to embed in returned basename, or NULL

@return newly-allocated path, utf8 string, no trailing "/"
*/
gchar *e2_utils_get_temp_path (const gchar *id)
{
	gchar *tmp = (id == NULL) ? "":(gchar *)id;
	const gchar *systmp = g_get_tmp_dir ();
	if (g_str_has_prefix (systmp, g_get_home_dir ()))
		tmp = g_strdup_printf ("%s"G_DIR_SEPARATOR_S"%s%s", systmp, BINNAME, tmp);
	else
		//in shared space, make user-identifiable temp dir
		tmp = g_strdup_printf ("%s"G_DIR_SEPARATOR_S"%d-%s%s", systmp, getuid (),
			BINNAME, tmp);
	//systmp, BINNAME and uid no. are all localised
	gchar *retval = e2_utils_get_tempname (tmp);
	g_free (tmp);
	tmp = retval;
	retval = D_FILENAME_FROM_LOCALE (tmp);
	g_free (tmp);
	return retval;
}
/*
@brief try to get a home dir for a path string like ~path (a specified user's home maybe with descendant(s))
This is fairly liberal about what may be in a user's name-string
@param path UTF8 string, possibly excluding the leading ~ that was found

@return newly allocated path with subtitution done, or NULL
**/
gchar *e2_utils_get_home_path (const gchar *utfpath)
{
	const gchar *s, *e;
	if (*utfpath == '~')
		s = ++utfpath;
	else
	{
		s = e2_utils_pass_whitespace ((gchar *)utfpath);
		if (s == NULL)
			return NULL;
		if (*s == '~')
			s++;
		else
			s = utfpath;	//no chug in this case
	}
	e = e2_utils_bare_strchr ((gchar *)s, ' ');
	if (e == NULL)
		e = s + strlen (s);
	gchar *temp = g_strndup (s, e - s);
	gchar *clean = e2_utils_unquote_string (temp);
	g_free (temp);
	if (clean == NULL)
		return NULL;
	temp = strchr (clean, G_DIR_SEPARATOR);
	if (temp != NULL)
	{
		if (temp == clean)
		{
			g_free (clean);
			return NULL;
		}
		*temp = '\0';
	}
	gchar *home;
	gchar *local = F_FILENAME_TO_LOCALE (clean);
	struct passwd *userinfo = getpwnam (local);
	if (userinfo == NULL || userinfo->pw_dir == NULL)
		home = NULL;
	else
	{
		gchar *utf = F_FILENAME_FROM_LOCALE (userinfo->pw_dir);
		if (temp != NULL)
			*temp = G_DIR_SEPARATOR;
		//FIXME handle any prior un-escaping
		home = g_strconcat (utf, (temp != NULL)?temp:"", e, NULL);
		F_FREE (utf, userinfo->pw_dir);
	}
	g_free (clean);
	F_FREE (local, clean);
	return home;
}
/**
@brief helper to check whether @a localpath meets conditions for a parent trash dir
@param localtrashpath absolute localised path of item to check, normally it has "Trash" suffix
@param real TRUE to test for non-link
@param sticky TRUE to test for sticky-bit set
@return TRUE if test(s) all passed
*/
static gboolean _e2_utils_check_valid_trash (const gchar *localtrashpath,
	gboolean real, gboolean sticky)
{
#ifdef E2_VFS
	VPATH ddata = { localtrashpath, NULL };
	if (e2_fs_access3 (&ddata, W_OK | X_OK E2_ERR_NONE()) == 0)
#else
	if (e2_fs_access3 (localtrashpath, W_OK | X_OK E2_ERR_NONE()) == 0)
#endif
	{
		struct stat sb;
		e2_fs_lstat
#ifdef E2_VFS
		(&ddata, &sb, NULL);	//won't fail since access3 succeeded
#else
		(localtrashpath, &sb);	//won't fail since access3 succeeded
#endif
		if (!S_ISDIR (sb.st_mode))
			return FALSE;
		if (real && S_ISLNK (sb.st_mode))
			return FALSE;
		if (sticky && !(sb.st_mode & S_ISVTX))
			return FALSE;
		//quick check for files inside Trash, don't bother with info
#ifdef E2_VFS
		ddata.path =
#else
		gchar *tf =
#endif
		e2_utils_strcat (localtrashpath, G_DIR_SEPARATOR_S"files");
		gboolean retval =
#ifdef E2_VFS
		(e2_fs_access3 (&ddata, W_OK | X_OK E2_ERR_NONE()) == 0);
#else
		(e2_fs_access3 (tf, W_OK | X_OK E2_ERR_NONE()) == 0);
#endif
		g_free
#ifdef E2_VFS
		((gchar *)ddata.path);
#else
		(tf);
#endif
		return retval;
	}
	return FALSE;
}
/**
@brief get path of trash directory relevant for @a localpath
This almost conforms to FDO trash spec, it looks for (but does not create) a
'native' and device-specific trash dir and defaults to the one in the user's
home dir. Not relevant for any namepace other than mounted filesystem.
Returned string will end with / or (if @a filesplace is TRUE) /files/
Device-specific returned string will have a UID before the sufffix.
@param localpath absolute localised path of item for which trash place is wanted, NULL for CWD
@param filesplace TRUE to append "files/" to the main trash path

@return pointer to newly-allocated UTF-8 pathstring, or NULL if there is none
*/
gchar *e2_utils_get_trash_path (gchar *localpath, gboolean filesplace)
{
	struct stat sb;
	gboolean freearg;
	if (localpath == NULL)
	{
		localpath = D_FILENAME_TO_LOCALE (curr_view->dir); //always dup, to avoid dirchange race
		freearg = TRUE;
	}
	else
		freearg = FALSE;

	if (e2_cl_options.trash_dir != NULL)
	{	//we can use the home trash dir if it's relevant
#ifdef E2_VFS
		VPATH ddata = { localpath, NULL }; //only work with native trash
		if (e2_fs_lstat (&ddata, &sb, NULL))
#else
		if (e2_fs_lstat (localpath, &sb E2_ERR_NONE()))
#endif
		{
			//FIXME handle error;
			printd (DEBUG, "lstat error: %s", g_strerror(errno));
			goto hometrash;
		}
		dev_t curr_dev = sb.st_dev;

#ifdef E2_VFS
		ddata.path =
#else
		const gchar *home =
#endif
		g_get_home_dir ();
#ifdef E2_VFS
		if (e2_fs_stat (&ddata, &sb, NULL))	//thru link ok
#else
		if (e2_fs_stat (home, &sb))	//thru link ok
#endif
		{
			//FIXME handle error;
			printd (DEBUG, "stat error: %s", g_strerror(errno));
			goto hometrash;
		}
		if (curr_dev == sb.st_dev)
			//want trash for device where home is located
			goto hometrash;
	}
	//find the top dir of the device where localpath is
#ifdef E2_VFSTMP
	FIXME
#endif
	gchar *dir = e2_fs_mount_get_enclosing_point (localpath);
	if (dir != NULL)
	{
		//try to access (but NOT create) a valid trash dir there
		gchar *tlocal = g_build_filename (dir, ".Trash", NULL);
		g_free (dir);
		dir = tlocal;
		gint myuid = getuid ();
		if (_e2_utils_check_valid_trash (tlocal, TRUE, TRUE))
			tlocal = g_strdup_printf ("%s"G_DIR_SEPARATOR_S"%d", tlocal, myuid);
		else
			tlocal = g_strdup_printf ("%s-%d", tlocal, myuid);
		g_free (dir);
		if (_e2_utils_check_valid_trash (tlocal, TRUE, FALSE))
		{
			gchar *utf = F_FILENAME_FROM_LOCALE (tlocal);
			dir = (filesplace) ?
					g_build_filename (utf, "files"G_DIR_SEPARATOR_S, NULL):
					g_strconcat (utf, G_DIR_SEPARATOR_S, NULL);
			if (freearg)
				g_free (localpath);
			g_free (tlocal);
			F_FREE (utf, tlocal);
			return dir;
		}
		g_free (tlocal);
	}
hometrash:
	if (freearg)
		g_free (localpath);
	if (e2_cl_options.trash_dir == NULL)
		return NULL;
	else
		return ((filesplace) ?
			g_build_filename (e2_cl_options.trash_dir, "files"G_DIR_SEPARATOR_S, NULL):
			g_strdup (e2_cl_options.trash_dir));
}
/**
@brief get a list of all valid trash paths for current user
This almost conforms to FDO trash spec, it looks for (but does not create) a
device-specific trash dir and the one in the user's home dir
Not relevant for any namepace other than mounted filesystem.
Each returned trashpath has no trailer, typically it has UID or "Trash" suffix
@return list of absolute localised trashpath strings, or NULL if no trashes present
*/
GList *e2_utils_get_trash_all (void)
{
	gint myuid = getuid ();
	gchar *tlocal;
	GList *member;
	GList *trashes = NULL;
	GList *mounts = e2_fs_mount_get_mounts_list ();	//list of UTF-8 paths other than /
	mounts = g_list_prepend (mounts, g_strdup (G_DIR_SEPARATOR_S));
	for (member = mounts; member != NULL; member = member->next)
	{
		gchar *point = (gchar *)member->data;
		gchar *dir = F_FILENAME_TO_LOCALE (point);
		//try to access (but NOT create) a valid trash dir there
		tlocal = g_build_filename (dir, ".Trash", NULL);
		F_FREE (dir, point);
		dir = tlocal;
		if (_e2_utils_check_valid_trash (tlocal, TRUE, TRUE))
			tlocal = g_strdup_printf ("%s"G_DIR_SEPARATOR_S"%d", tlocal, myuid);
		else
			tlocal = g_strdup_printf ("%s-%d", tlocal, myuid);
		g_free (dir);
		if (_e2_utils_check_valid_trash (tlocal, TRUE, FALSE))
			trashes = g_list_prepend (trashes, tlocal);
		else
			g_free (tlocal);
	}
	e2_list_free_with_data (&mounts);
	if (trashes != NULL && trashes->next != NULL);
		trashes = g_list_reverse (trashes);

	if (e2_cl_options.trash_dir != NULL)
	{
		tlocal = D_FILENAME_TO_LOCALE (e2_cl_options.trash_dir);
		if (_e2_utils_check_valid_trash (tlocal, FALSE, FALSE))
		{
			//ignore miniscule risk that this dir is already listed
			*(tlocal + strlen (tlocal) - sizeof (gchar)) = '\0';	//no trailer wanted
			trashes = g_list_prepend (trashes, tlocal);
		}
		else
			g_free (tlocal);
	}
	return trashes;
}
/**
@brief get output-pane font name

@return string with the name
*/
const gchar *e2_utils_get_output_font (void)
{
	gchar *fntname;
	if (e2_option_bool_get ("custom-output-font"))
	{
		fntname = e2_option_str_get ("output-font");
		if (*fntname == '\0')
			fntname = NULL;
	}
	else
		fntname = NULL;
	if (fntname == NULL)
	{
		GtkSettings* defs = gtk_settings_get_default ();
		g_object_get (G_OBJECT (defs), "gtk-font-name", &fntname, NULL);
		if (fntname == NULL)	//CHECKME needed ?
		{
			printd (WARN, "No default font detected");
			fntname = "Sans 10";
		}
	}
	return fntname;
}
/**
@brief update gtk properties like menu delays

Update gtk internal properties, e.g. menu popup and popdown delays.
This function is usually called after configuration changes to update
the gtk properties.

@return
*/
void e2_utils_update_gtk_settings (void)
{
	GtkSettings *defs = gtk_settings_get_default ();
	//CHECKME cache original values & revert @ session-end ?
#ifndef USE_GTK3_10
	gint delay = e2_option_int_get ("submenu-up-delay");
	//be on the safe side
	if (delay < 0) delay = 0;
	gtk_settings_set_long_property (defs, "gtk-menu-popup-delay", delay,
		"XProperty");
	delay = e2_option_int_get ("submenu-down-delay");
	if (delay < 0) delay = 0;
	gtk_settings_set_long_property (defs, "gtk-menu-popdown-delay", delay,
		"XProperty");
# ifdef USE_GTK2_12
	gtk_settings_set_long_property (defs, "gtk-auto-mnemonics", FALSE,
		"XProperty");
# endif
#endif
	//set double-click interval threshold to gtk's value
	g_object_get (G_OBJECT (defs), "gtk-double-click-time", &click_interval, NULL);
	if (click_interval < E2_CLICKINTERVAL)
	{
		click_interval = E2_CLICKINTERVAL;
		g_object_set (G_OBJECT (defs), "gtk-double-click-time", E2_CLICKINTERVAL, NULL);
	}
}

/**
@brief helper function to find matches for last or only path segment
Arrives here with BGL off
Note: to eliminate BGL-racing, no UI-change from here
@param parent absolute path of dir being processed, localised string with trailer
@param itemname name of discovered item, localised string
@param found pointer to store for list of data items for @a parent
@param pair pointer to struct with parameters for use here

@return TRUE to signal the read has not been aborted
*/
static gboolean _e2_utils_drcb_match_wild_last (VPATH *parent,
	const gchar *itemname, GList **found, E2_Duo *pair)
{
	if (strcmp (itemname, ".."))
	{
		GPatternSpec *pattern = (GPatternSpec *)pair->b;
		gchar *utfname = F_FILENAME_FROM_LOCALE (itemname);
		if (g_pattern_match_string (pattern, utfname))
		{
			gchar *escname;
			gboolean all = GPOINTER_TO_INT (pair->a);
			if (all)
			{
				escname = e2_utf8_escape (utfname, ' ');
				*found = g_list_append (*found, escname);
			}
			else
			{
				gchar *freeme = e2_utils_strcat (VPSTR (parent), itemname);
#ifdef E2_VFS
				VPATH ddata = { freeme, parent->spacedata };
				if (e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
				if (e2_fs_is_dir3 (freeme E2_ERR_NONE()))
#endif
				{
					escname = e2_utf8_escape (utfname, ' ');
					*found = g_list_append (*found, escname);
				}
				g_free (freeme);
			}
		}
		F_FREE (utfname, itemname);
	}
	return TRUE;
}
/**
@brief find matches for last or only path segment of @a arg, if that contains * and/or ?
Searching is in $PATH directories
@a arg may include an absolute or relative path.
Relative path is assumed relative to curr_view->dir
If @a arg does include any path, that is expected to be all non-wild.
"." and ".." items are excluded
Expects BGL to be on/closed on arrival here
@param arg path or itemname which may have wildcard(s) in its only or last segment, utf-8 string
@param all TRUE to match any type of item, FALSE to match dirs only

@return list of utf8 names which match, or 0x1 if if no match was found,
or NULL if there is no wildcard in @a arg or an error occurred
*/
static GList *_e2_utils_match_wild_last (gchar *arg, gboolean all)
{
	gboolean freepath, freename;
//	E2_FSType dirtype;
	gchar *name, *path, *localpath, *freeme;
#ifdef E2_VFSTMP
	//FIXME relevant path is ?
//	dirtype = ?;
#else
	path = g_path_get_dirname (arg);
//	dirtype = FS_LOCAL;	//FIXME
#endif
	if (!strcmp (path, "."))
	{	//no path in arg
		g_free (path);
		if (strchr (arg, '*') == NULL && strchr (arg, '?') == NULL)
			return NULL;
		if (g_str_has_prefix (arg, "$"))	//some shell or language variables can be ignored
			return GINT_TO_POINTER (1);
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
		path = curr_view->dir;
#endif
		name = arg;
		freepath = freename = FALSE;
	}
	else
	{	//path was found
		name = g_path_get_basename (arg);
		if (!strcmp (name, ".")
			|| (strchr (name, '*') == NULL && strchr (name, '?') == NULL))
		{
			g_free (path);
			g_free (name);
			return NULL;
		}
		//ensure trailing separator
		freeme = path;
		path = g_strconcat (path, G_DIR_SEPARATOR_S, NULL);
		g_free (freeme);
		if (!g_path_is_absolute (path))
		{
			freeme = path;
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
			path = e2_utils_dircat (curr_view, path, FALSE);
#endif
			g_free (freeme);
		}
		freepath = freename = TRUE;
	}

	OPENBGL	//needed for dirforeach
	localpath = F_FILENAME_TO_LOCALE (path);
	GPatternSpec *pattern = g_pattern_spec_new (name);
	E2_Duo pair = { GINT_TO_POINTER (all), pattern };

#ifdef E2_VFS
#ifdef E2_VFSTMP
	FIXME placedata
#endif
	VPATH ddata = { localpath, NULL };
	GList *matches = (GList *)e2_fs_dir_foreach (&ddata,
#else
	GList *matches = (GList *)e2_fs_dir_foreach (localpath,
#endif
		E2_DIRWATCH_NO,	//$PATH is local places only
		_e2_utils_drcb_match_wild_last, &pair, NULL E2_ERR_NONE());
	//conform results to API
	if (E2DREAD_FAILED (matches))
		matches = NULL;
	else if (matches == NULL)
		matches = (GList *) 1;

	g_pattern_spec_free (pattern);
	if (freepath)
		g_free (path);
	F_FREE (localpath, path);
	if (freename)
		g_free (name);
	CLOSEBGL
	return matches;
}

/**
@brief helper function to match path which has wildcard(s) in its element(s)
Arrives with BGL open/off
Note: to eliminate BGL-racing, no UI-change from here
Reentrant use of _e2_utils_match_wild_path() assmumes BGL is still off here
@param parent UNUSED absolute path of dir being processed, localised string with or without trailer
@param itemname name of discovered item, localised string
@param found pointer to store for list of data items for @a parent
@param pattern the matcher for desired items

@return TRUE to signal the read has not been aborted
*/
static gboolean _e2_utils_drcb_match_wild_path (VPATH *parent,
	const gchar *itemname, GList **found, GPatternSpec *pattern)
{
	if (strcmp (itemname, ".."))	//"." entries are filtered at source
	{
		gchar *utfname = F_FILENAME_FROM_LOCALE (itemname);
		if (g_pattern_match_string (pattern, utfname))
			*found = g_list_append (*found, g_strdup (itemname));

		F_FREE (utfname, itemname);
	}
	return TRUE;
}
/**
@brief recursively find a path that is a descendant of @a parent and otherwise matches wildcard data in @a wdata
This scans @a parent depth-first, until either a complete match is found, or
no match is possible, in which case it backs up a level and tries the next
match at that level, and so on.
Matching path segments are stored in @a wdata
Expects BGL to be closed on arrival here
@param parent absolute path, no wildcard char(s) or redundant separators or trailer, localised string
@param wdata pointer to struct with parameters for use here

@return TRUE if a matching path was found
*/
static gboolean _e2_utils_match_wild_path (gchar *parent, E2_WildData *wdata)
{
	gboolean retval;
	guint i, here;
	gchar *format;
	GString *checker;
	GPatternSpec *pattern;
	GList *matches, *member;
#ifdef E2_VFS
	VPATH ddata;
#ifdef E2_VFSTMP
	FIXME valid spacedata for parent
#endif
	ddata.spacedata = NULL;
#endif
	OPENBGL	//needed for dirforeach

	here = wdata->curr_depth;
	checker = g_string_sized_new (256);
	checker = g_string_append (checker, parent);
	pattern = g_pattern_spec_new (wdata->path_elements[here]);
#ifdef E2_VFS
	ddata.path = checker->str;
	matches = (GList *)e2_fs_dir_foreach (&ddata,
#else
	matches = (GList *)e2_fs_dir_foreach (checker->str,
#endif
		E2_DIRWATCH_NO,	//$PATH is local places only
		_e2_utils_drcb_match_wild_path, pattern, NULL E2_ERR_NONE());
	retval = !(matches == NULL || E2DREAD_FAILED (matches));

	if (retval)
	{
		if (here < wdata->last_wild_depth)
		{
			i = checker->len;	//for truncating
			format = (i == sizeof (gchar)) ? "%s" : G_DIR_SEPARATOR_S"%s";
			wdata->curr_depth++;
			for (member = matches; member != NULL; member = member->next)
			{
				g_string_append_printf (checker, format, (gchar *)member->data);
#ifdef E2_VFS
				ddata.path = checker->str;
				retval = !e2_fs_stat (&ddata, wdata->statptr E2_ERR_NONE())
#else
				retval = !e2_fs_stat (checker->str, wdata->statptr E2_ERR_NONE())
#endif
						&& S_ISDIR (wdata->statptr->st_mode)
						&& _e2_utils_match_wild_path (checker->str, wdata);	//recurse into dir at next level
				if (retval)
					break;	//everything afterward is matched
				//not a dir or not completely matched downwards
				//prepare to try again at this level
				checker = g_string_truncate (checker, i);
			}
			if (!retval)	//preserve success pointer if all was successful
				wdata->curr_depth--;
		}
		else
			member = matches;

		if (retval)
			wdata->path_matches [here] = g_strdup ((gchar *)member->data);

		e2_list_free_with_data (&matches);
	}

	g_pattern_spec_free (pattern);
	g_string_free (checker, TRUE);

	CLOSEBGL
	return retval;
}
/**
@brief replace any wildcard character(s) '*' and '?' in @a string
If it's quoted (" or ') nothing is done. If it includes path separator(s),
any wildcard in path element(s) before the last one is replaced by the
fist-found valid match, or if there's no match, the expansion fails.
Any wildcard in the last (or only) path segment is expanded to all matches,
with prepended path if appropriate
Assumes BGL is closed, here (for error msgs) and downstream
@param string a whitespace-separated "segment" of a commandline utf-8 string maybe with wildcard(s) to replace

@return newly-allocated string, copy of @a string or string with wildcards replaced
*/
static gchar *_e2_utils_match_wild_segment (gchar *string)
{
	if (strchr (string, '*') == NULL && strchr (string, '?') == NULL)
		return (g_strdup (string));
	gint len = strlen (string);
	if (*string == '"' && *(string + len - 1) == '"')
		return (g_strdup (string));
	if (*string == '\'' && *(string + len - 1) == '\'')
		return (g_strdup (string));

	guint last_wild_depth, last_depth;
	gchar *freeme, *path, *parent_path, *expanded;
	GList *matches;

	//trailing separator confuses things
	if (g_str_has_suffix (string, G_DIR_SEPARATOR_S))
		*(string + len - 1) = '\0';

#ifdef E2_VFSTMP
	//FIXME path when not mounted local
#endif
	path = g_path_get_dirname (string);	//no trailing separator
	if (strcmp (path, "."))
	{	//the string has a path
		gboolean abs = g_path_is_absolute (path);
		if (strchr (path, '*') != NULL || strchr (path, '?') != NULL)
		{
			guint i, count;
			gchar *s;
			E2_WildData wdata = {0};

			g_free (path);
			//make sure processed string is absolute
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
			path = (abs) ?
				g_strdup (string):	//use whole string in case basename is also wild
				e2_utils_dircat (curr_view, string, FALSE);
#endif
			//making GPatternSpec's requires utf-8 patterns
			wdata.path_elements = g_strsplit (path, G_DIR_SEPARATOR_S, -1);
			//for the matched elements ... setup for strfreev later
			count = g_strv_length (wdata.path_elements);
			wdata.path_matches = (gchar **)
#ifdef USE_GLIB2_8
				g_try_malloc0 ((count + 1) * sizeof (gchar *));
#else
				//don't use calloc() so that g_strfreev() is ok to clean up
				g_try_malloc ((count + 1) * sizeof (gchar *));
#endif
			CHECKALLOCATEDWARN (wdata.path_matches, {});
			if (wdata.path_matches == NULL)
			{
				g_free (path);
				g_strfreev (wdata.path_elements);
				return (g_strdup (string));
			}
#ifndef USE_GLIB2_8
			else
				memset (wdata.path_matches, 0, (count + 1) * sizeof (gchar *));
#endif
			//find the first reportable segment of the path
/*			if (!abs)
			{
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
				s = curr_view->dir;
#endif
				while (*s != '\0')
				{
					if (*s == G_DIR_SEPARATOR)
						wdata.first_depth++;
					s++;
				}
			}
*/
			last_wild_depth = 0;	//warning prevention
			//find the bounds of the scan, skipping empty fictitious segment
			//from before leading separator
			for (i = 1; i < count; i++)
			{
				s = wdata.path_elements[i];
				if (strchr (s, '*') != NULL || strchr (s, '*') != NULL)
				{
					if (wdata.first_wild_depth == 0)
						wdata.first_wild_depth = i;//highest level that has a wildcard char
					last_wild_depth = i;//lowest level that has a wildcard char
				}
			}
			last_depth = i - 1;	//level of the last path segment
			//extra match needed when path extends after the last wild segment
			if (last_wild_depth < last_depth)
				last_wild_depth++;
/*			else
			{	//special treatment if 2nd last segment of string is wild and last is explicit
				gchar *name = g_path_get_basename (string);
				if (strchr (name, '*') != NULL || strchr (name, '*') != NULL)
				{
					//FIXME allocate a replacement array with 1 more pointer
					local = F_FILENAME_TO_LOCALE (name);
					wdata.path_segments[wdata.last_depth] = local;
					wdata.path_segments[wdata.last_depth+1] = NULL;
					F_FREE (local, name);
					wdata.last_wild_depth++;
				}
				g_free (name);
			}
*/
			if (last_wild_depth == count - 1)	//the last item also is wild
				last_depth--;	//later, we don't want to use the last matched element

			wdata.curr_depth = wdata.first_wild_depth;
			wdata.last_wild_depth = last_wild_depth;
			wdata.last_depth = last_depth;

			//construct non-wild localised parent path, to start the scan (no trailing separator)
			//and fill corresponding elements of the matches array
			wdata.path_matches [0] = g_strdup ("");
			if (wdata.first_wild_depth == 1)
				s = g_strdup (G_DIR_SEPARATOR_S);
			else
			{
				s = g_strdup ("");
				for (i = 1; i < wdata.first_wild_depth; i++)
				{
					wdata.path_matches[i] = D_FILENAME_TO_LOCALE (wdata.path_elements[i]);
					freeme = s;
					s = g_strconcat
						(freeme, G_DIR_SEPARATOR_S, wdata.path_matches[i], NULL);
					g_free (freeme);
				}
			}

			//scan the filesystem to match the full path
			struct stat statbuf;
			wdata.statptr = &statbuf;
			if (_e2_utils_match_wild_path (s, &wdata))
			{
				g_free (s);
				//create "real" parent path string from the matched segments, with trailer
#ifdef E2_VFSTMP
	//CHECKME dir when not mounted local
#endif
				//FIXME strip any prepended curr_view->dir
				//FIXME ensure this array is fully populated
				s = g_strdup ("");

//				for (i = wdata.first_depth; i <= last_depth; i++)
				for (i = 0; i <= last_depth; i++)
				{
					freeme = s;
					s = g_strconcat
						(freeme, wdata.path_matches[i], G_DIR_SEPARATOR_S, NULL);
					g_free (freeme);
				}
				parent_path = D_FILENAME_FROM_LOCALE (s);
			}
			else	//no match found for the path
			{
				if (!abs)
				{	//we want the supplied path only
					g_free (path);
					path = g_path_get_dirname (string);
				}
				freeme = g_path_get_dirname (string);
				parent_path = g_strconcat (freeme, G_DIR_SEPARATOR_S, NULL);
				g_free (freeme);
			}

			g_free (s);

			g_strfreev (wdata.path_elements);
			g_strfreev (wdata.path_matches);
		}
		else	//the path is all explicit
		{
			if (!abs)
			{	//we want the supplied path only
				g_free (path);
				path = g_path_get_dirname (string);
			}
			parent_path = g_strconcat (path, G_DIR_SEPARATOR_S, NULL);
		}
		//allocated parent_path has no prepended cwd, and has trailing separator

		//now expand the last segment in the supplied path string
		gchar *name = g_path_get_basename (string);
		if (strchr (name, '*') != NULL || strchr (name, '*') != NULL)
		{	//last path segment is wild
			freeme = g_strconcat (parent_path, name, NULL);
			matches = _e2_utils_match_wild_last (freeme, TRUE);
			g_free (freeme);
			if (matches == NULL //error
				|| matches == GINT_TO_POINTER (0x1)) //no match found
				//send back is what we have now
				expanded = g_strconcat (parent_path, name, NULL);
			else
			{	//append each matched item to matched path
				expanded = g_strdup ("");
				GList *tmp;
				for (tmp = matches; tmp != NULL; tmp = tmp->next)
				{
					freeme = expanded;
					expanded = g_strconcat (freeme, " ", parent_path,
						(gchar*)tmp->data, NULL);
					g_free (freeme);
				}
				e2_list_free_with_data (&matches);
			}
		}
		else	//last path segment is explicit
			expanded = g_strconcat (parent_path, name, NULL);

		g_free (parent_path);
		g_free (name);
	}
	else
	{	//no path in the string
		matches = _e2_utils_match_wild_last (string, TRUE);
		if (matches == NULL) //no wildcard in the name (or error)
			expanded = g_strdup (string);
		else if (matches == GINT_TO_POINTER (0x1)) //no match
			expanded = g_strdup ("");
		else
		{
			expanded = g_strdup ("");
			GList *tmp;
			for (tmp = matches; tmp != NULL; tmp = tmp->next)
			{
				freeme = expanded;
				expanded = g_strconcat (freeme, " ", (gchar*)tmp->data, NULL);
				g_free (freeme);
			}
			e2_list_free_with_data (&matches);
		}
	}

	g_free (path);
	return expanded;
}
/**
@brief replace any wildcard character(s) '*' and '?' in commandline UTF-8 string @a raw
@a raw may include whitespace gap(s), in which case each gap-separated "element"
is separately handled.
If any element is quoted (by " or ') no wildcard is expanded in that element.
If any element includes path separator(s), any wildcarded path segment(s)
before the last one are replaced by the first-found match, or if there's no
match, the whole expansion fails. Any wildcard in the last (or only) path segment
is expanded to _all_ matches, with prepended path if appropriate.
@param raw string maybe with wildcard(s) to replace

@return @a raw, or a replacement string with wildcards replaced
*/
gchar *e2_utils_replace_wildcards (gchar *raw)
{
	//quick check
	if (strchr (raw, '*') == NULL	//if always ascii ;, don't need g_utf8_strchr()
		&& strchr (raw, '?') == NULL)	//if always ascii ;, don't need g_utf8_strchr()
		return raw;

	gchar *p, *s, *freeme, *expanded = g_strdup ("");
	gchar sep[2] = {'\0', '\0'};
	gint cnt1 = 0; //counter for ' chars
	gint cnt2 = 0; //counter for " chars

	s = p = raw;
	while (*p != '\0')
	{
		if (*p == '\'')
		{
			if (p == raw || *(p-1) != '\\')
				cnt1++;
		}
		else if (*p == '"')
		{
			if (p == raw || *(p-1) != '\\')
				cnt2++;
		}
		else if (*p == ' ' || *p == '\t')
		{	//check if separator seems to be outside parentheses
			if (cnt1 % 2 == 0 && cnt2 % 2 == 0)
			{ //found a gap in the command string
				sep[0] = *p;
				*p = '\0';
				s = _e2_utils_match_wild_segment (s);
				freeme = expanded;
				expanded = g_strconcat (freeme, s, sep, NULL);
				g_free (s);
				g_free (freeme);
				*p = sep[0];
				//resume scanning
				s = e2_utils_pass_whitespace (p+1);
				if (s == NULL) 	//irrelevant trailing whitespace
					break;
				p = s;
				continue;
			}
		}
		p++;
	}
	if (s != NULL && *s != '\0')
	{
		//process last (or only) command_element
		s = _e2_utils_match_wild_segment (s);
		//append string to buffer
		freeme = expanded;
		expanded = g_strconcat (freeme, s, NULL);
		g_free (s);
		g_free (freeme);
	}

	return expanded;
}
/**
@brief get a single variable value
@param variable pointer to store for input variable and replacement string, if any
@a variable must be freeable
@return TRUE if @a variable has been subsituted
*/
gboolean e2_utils_get_variable (gchar **variable)
{
	gboolean retval = FALSE;
	if (variable != NULL && *variable != NULL)
	{
		g_strstrip (*variable);
		while (**variable == '$')
		{
			gchar *value = e2_utils_replace_vars (*variable, FALSE);
			if (!strcmp (value, *variable))
				break;
			g_free (*variable);
			*variable = value;
			retval = TRUE;
		}
	}
	return retval;
}
//no. of single-quote characters found in the replacement string,
//up to the end of the current segment
static gint p_count;
//and correspondingly for double-quotes
static gint p2_count;
static gchar *dollar = "$";
/**
@brief add segment to the replacement string

Empty strings are ignored. The index for the next segment
is updated.
The count of single-parentheses is updated. This uses
single-byte ascii scanning.

@param string string to record
@param join pointer to the array of segments of the replacement string
@param join_count pointer to index of the next segment

@return
*/
static void _e2_utils_replace_vars_add (gchar *string, const gchar *join[], gint *join_count)
{
	if (*string != '\0') //no point in adding empty strings
	{
		join[*join_count] = string;
		(*join_count)++;
		//update count of ' characters sofar detected
		gchar *p = string;
		while (*p != '\0')
		{
			if (*p != '\\')
			{
				if (*p++ == '\'')
					p_count++;
				if (*p == '"')
					p2_count++;
			}
			else
				p++;
		}
	}
}
/**
@brief add original segment to the replacement string

To avoid leaks, this adds 2 segments - one with just the
separator "$" and the second with the ignored segment
of the original string.
The count of single-parentheses is updated. This uses
single-byte ascii scanning.

@param string string to record
@param join pointer to the array of segments of the replacement string
@param join_count pointer to index of the next segment

@return
*/
static void _e2_utils_replace_vars_ignore (gchar *string, const gchar *join[], gint *join_count)
{
	join[*join_count] = dollar;
	(*join_count)++;
	_e2_utils_replace_vars_add (string, join, join_count);
}
/**
@brief replace variables in a string

Replaces all relevant '~' characters, and all valid internal or environment
variables and option variables, in @a raw.
~ will be replaced if preceeded by nothing or whitespace, and followed by
nothing or '/' or (possibly, depending on @a rawpath) by whitespace.
Environment references and internal variables must have the form @c \$VAR or
@c \${VAR} where @c VAR is the name of the variable. Internal variables get
precedence over environment variable with the same name.
As a special case, $$ will be replaced by active directory (without trailing /),
in effect = `pwd`
Option references must have the form @c \$[VAR] where @c VAR is the 'internal'
name of the option.
Unrecognised variables are ignored. Environment variables inside single
parentheses are ignored. Strings prefaced by "\$" are ignored.

@code
gchar *str = e2_utils_replace_vars ("my home is \${HOME} and my e2 terminal is \$[command-xterm]");
@endcode

@param raw utf string maybe with variable(s) etc to replace
@param rawpath TRUE to constrain "~" expansion consistent with a single path string

@return newly allocated string, with variables (if any) replaced as appropriate
*/
gchar *e2_utils_replace_vars (gchar *raw, gboolean rawpath)
{
//	printd (DEBUG, "e2_utils_replace_vars (raw:%s)", raw);
	gchar *s = g_strdup (raw), *p = s, *freeme;
	gint cnt1 = 0; //counter for ' chars
	gint cnt2 = 0; //counter for " chars
	while (*p != '\0')
	{
		if (*p == '\'')
		{
			if (p == s || *(p-1) != '\\')
				cnt1++;
		}
		else if (*p == '"')
		{
			if (p == s || *(p-1) != '\\')
				cnt2++;
		}
		else if (*p == '~')	//always ASCII ~, don't need UTF-8 handler
		{	//check if tilde seems to be outside parentheses
			if (cnt1 % 2 == 0 && cnt2 % 2 == 0)
			{ //found a candidate for replacement
				if (p == s || *(p-1) == ' ' || *(p-1) == '\t' ) //preceeded by nothing or whitespace
				{
					register gchar c = *(p+1);
					if ( c == '\0'	//~ is last char in string
					 ||  c == G_DIR_SEPARATOR //or followed by '/'
					|| ((c == ' ' || c == '\t') && !rawpath))	//or a stand-alone not in a clean path
					{
						*p = '\0';
						freeme = s;
						s = g_strconcat (s, "${HOME}", p+1, NULL);
						p = s + (p - freeme);
						g_free (freeme);
						continue;
					}
					else if (c != ' ' && c != '\t')
					{
						freeme = e2_utils_get_home_path (p);
						if (freeme != NULL)
						{
							*p = '\0';
							p = s;
							s = g_strconcat (s, freeme, NULL);
							g_free (freeme);
							freeme = p;
							p = s + strlen (p);
							g_free (freeme);
							continue;
						}
					}
				}
			}
		}
		p++;
	}

	//replace $... occurrences, taking quoting into account
	if (strchr (s, '$') != NULL)	//if always ascii $, don't need g_utf8_strchr()
	{
		//break into pieces and count them
		gchar **split = g_strsplit (s, "$", -1);
		gint split_count = 0;
		while (split[split_count] != NULL)
			split_count++;
		//init the stack-store for the pieces of the replacement string
		//join[] members are shared with split[] or otherwise const, so don't clean
		const gchar *join[split_count * 2 + 1];
		gint join_count = 0;
		//init the counts of quote-chars
		p_count = p2_count = 0;
		gchar *rest;

		_e2_utils_replace_vars_add (split[0], join, &join_count);	//save any segment before the 1st '$'

		gint i;
		//scan from after 1st '$'
		for (i = 1; i < split_count; i++)
		{
			if (*split[i] == '\0')	//2 '$' chars in a row, or single trailing '$'
			{
				if (split[i+1] != NULL) //not a trailing '$'
				{
					gchar *cwd;
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
					//CHECKME special-case a string like cd [<path>]$$[<path2>] ?
					if (i > 0 && g_str_has_suffix (split[i-1], G_DIR_SEPARATOR_S))
						cwd = g_strdup (curr_view->dir + sizeof (gchar)); //don't want adjacent separators
					else
						cwd = g_strdup (curr_view->dir);
#endif
					//generally strip trailer
					gint len = strlen (cwd);
					if (len > 1)
						*(cwd + len - 1) = '\0';
					_e2_utils_replace_vars_add (cwd, join, &join_count);
				}
				else	//trailing '$'
					if (*split[i-1] != '\0' //not a trailing "$$"
						|| i == 1)	//just a single "$"
						_e2_utils_replace_vars_add ("$", join, &join_count);
			}
			else if (g_str_has_suffix (split[i-1], "\\")	//escaped '$'
					  || *split[i] == '(')	//shell command
				_e2_utils_replace_vars_ignore (split[i], join, &join_count);

			else if (*split[i] == '[')	//option to replace
			{
				//no protection check for internal vars
				gchar *rest = strchr (split[i], ']'); //if always ascii ], don't need g_utf8_strchr()
				if (rest != NULL)
				{
					*rest++ = '\0';
					E2_OptionSet *set = e2_option_get (split[i]+1);
					if (set != NULL)
					{
						join[join_count++] = e2_option_str_get_direct (set);
						_e2_utils_replace_vars_add (rest, join, &join_count);
					}
					else
					{
						*(--rest) = ']';
						_e2_utils_replace_vars_ignore (split[i], join, &join_count);
					}
				}
				else
					_e2_utils_replace_vars_ignore (split[i], join, &join_count);
			}

			else if (*split[i] == '{')	//variable to replace
			{
				//make sure separator was outside ' '
				if (p_count % 2 == 1)
					_e2_utils_replace_vars_ignore (split[i], join, &join_count);
				else
				{
					rest = strchr (split[i], '}');	//if always ascii }, don't need g_utf8_strchr()
					if (rest != NULL)
					{
						*rest++ = '\0';
						const gchar *env = e2_command_get_variable_value (split[i]+1, NULL);
						if (env == NULL)
							env = g_getenv (split[i]+1);
						if (env != NULL)
						{
							join[join_count++] = (gchar *) env;
							_e2_utils_replace_vars_add (rest, join, &join_count);
						}
						else
						{
							*(--rest) = '}';
							_e2_utils_replace_vars_ignore (split[i], join, &join_count);
						}
					}
					else
						_e2_utils_replace_vars_ignore (split[i], join, &join_count);
				}
			}
			else	//something else (maybe a variable)
			{
				//make sure separator was outside ' '
				if (p_count % 2 == 1)
					_e2_utils_replace_vars_ignore (split[i], join, &join_count);
				else
				{
					gchar c = 0;
					gchar *st;
					//the variable may not be surrounded by whitespace
					//e.g. in a string like cd [<path>]$VAR[<path2>]
					//but this does help focus a bit
					rest = e2_utils_find_whitespace (split[i]);
					if (p2_count % 2 == 1)
					{	//inside double-quote
						st = strchr (split[i], '"');
						if (st != NULL && (rest == NULL || st < rest))
							rest = st;
					}
					if (rest != NULL)
					{
						c = *rest;
						*rest = '\0';
					}
					//split[i] is now a string of length > 0, possibly with a variable as its prefix
					const gchar *tail = "";
					const gchar *value = e2_command_get_variable_value (split[i], &tail);
					if (value == NULL)
					{
						//not an internal variable, maybe it's external
						//this works only if $VAR is followed by 0 or whitespace
						//otherwise, must use ${VAR}
						st = F_FILENAME_TO_LOCALE (split[i]);
						value = g_getenv (st);
						F_FREE (st, split[i]);
					}

					if (rest != NULL)
						*rest = c;

					if (value != NULL)
					{
						gboolean clean = (*tail != '\0' || rest != NULL);
						if (clean)
						{
							st = split[i];
							split[i] = g_strconcat (value, tail, NULL);
							g_free (st);
						}
						join[join_count++] = (clean) ? split[i] : (gchar *)value;
					}
					else if (i > 1 && *split[i-1] == '\0') //double '$'
						_e2_utils_replace_vars_add (split[i], join, &join_count);
					else
						_e2_utils_replace_vars_ignore (split[i], join, &join_count);
				}
			}
		}
		join[join_count] = NULL;
		g_free (s);
		s = g_strjoinv (NULL, (gchar**)join);

		g_strfreev (split);
	}
	return s;
}
/**
@brief get coordinates of @a widget relative to its current screen
@param widget the activated widget whose position is to be calculated
@param x pointer to gint storage for the x (left) coordinate
@param y pointer to gint storage for the y (top) coordinate
@return
*/
void e2_utils_get_abs_pos (GtkWidget *widget, gint *x, gint *y)
{
#ifdef USE_GTK2_18
	if (gtk_widget_is_toplevel (widget))
#else
	if (GTK_WIDGET_TOPLEVEL (widget))
#endif
		gdk_window_get_position (
#ifdef USE_GTK2_14
		gtk_widget_get_window (widget),
#else
		widget->window,
#endif
		x, y);
	else
	{
		GtkWidget *current = widget;
		while (
#ifdef USE_GTK2_18
			!gtk_widget_get_has_window (current)
#else
			GTK_WIDGET_NO_WINDOW (current)
#endif
			&& (current = gtk_widget_get_parent(current)) != NULL) {}
		if (current == NULL)	//should never happen
			current = app.main_window;
		GdkWindow *window =
#ifdef USE_GTK2_14
			gtk_widget_get_window (current);
#else
			current->window;
#endif
		gint lx, ly;
		gdk_window_get_origin (window, &lx, &ly);

#ifdef USE_GTK2_18
		GtkAllocation alloc;
		gtk_widget_get_allocation (widget, &alloc);
		lx += alloc.x;
		ly += alloc.y;
#else
		lx += widget->allocation.x;
		ly += widget->allocation.y;
#endif
		*x = lx;
		*y = ly;
	}
}
#ifdef USE_GTK3_0
/**
@brief log modifiers-mask when key is pressed or released on @a widget

Called only when event->is_modifier is TRUE. Event->state is not set, for this
modifier at least, when a mod-key is pressed. So we check the long way.

@param widget the focused widget when a key was pressed or released

@return
*/
void e2_utils_save_state (GtkWidget *widget)
{
	GdkDeviceManager *manager = gdk_display_get_device_manager (gdk_display_get_default());
	GdkDevice *device = gdk_device_manager_get_client_pointer (manager);
	if (device != NULL)
	{
		GdkWindow *win = gdk_device_get_window_at_position (device, NULL, NULL);
		if (win != NULL)
		{
			GdkModifierType log = 0;
			gdk_device_get_state (device, win, NULL, &log);
#ifdef DEBUG_MESSAGES
			log &= E2_MODIFIER_MASK;
			printd (DEBUG, "Mod-keys mask now is %u", log);
			g_object_set_data (G_OBJECT(widget), "e2_keymods", GUINT_TO_POINTER(log));
#else
			g_object_set_data (G_OBJECT(widget), "e2_keymods", GUINT_TO_POINTER(log & E2_MODIFIER_MASK));
#endif
		}
	}
}
/**
@brief get mask for modifier key(s) currently pressed on @a widget

@param widget the focused widget when a key was previously pressed or released

@return the modifier flags
*/
GdkModifierType e2_utils_get_savedstate (GtkWidget *widget)
{
	return GPOINTER_TO_UINT (g_object_get_data (G_OBJECT(widget), "e2_keymods"));
}
#else //ndef USE_GTK3_0
//on gtk 3.0 at least, this doesn't work, returned mask is always 0
/**
@brief get current modifiers-mask, in raw form
CHECKME maybe needs BGL closed ?
@return bitmask of modifiers
*/
GdkModifierType e2_utils_get_modifiers (void)
{
	GdkModifierType mask = 0;
	if (!gtk_get_current_event_state (&mask))
	{
		GdkDisplay *display =
		gdk_display_manager_get_default_display (gdk_display_manager_get());
		gdk_display_get_pointer (display, NULL, NULL, NULL, &mask);	//CHECKME any pointer ?
	}
//	guint modifiers = gtk_accelerator_get_default_mod_mask ();
//	return mask & modifiers;
	return mask;
}
#endif
#ifdef USE_GTK3_0 //CHECKME devices
/**
@brief get a list of pointer-type GdkDevice's which are associated with @a widget

@param widget the widget to be checked, or NULL for any widget

@return allocated list of devices, or NULL. The list itself should be freed by
 the caller, but the data must not be altered
*/
static GList *_e2_utils_get_pointer_devices_for_widget (GtkWidget *widget)
{
	GdkWindow *wwin;
	if (widget != NULL)
	{
		wwin = gtk_widget_get_window (widget);
		if (wwin == NULL)
			return NULL;
	}
	else
		wwin = NULL;

	GdkDeviceManager *manager = gdk_display_get_device_manager (gdk_display_get_default());
	GList *devices = gdk_device_manager_list_devices (manager, GDK_DEVICE_TYPE_MASTER);
	if (devices != NULL)
	{
		GList *member;
		for (member = devices; member != NULL; member = member->next)
		{
			GdkDevice *dev = (GdkDevice *)member->data;
			if (gdk_device_get_mode (dev) != GDK_MODE_WINDOW)
			{
				member->data = NULL;
				continue;
			}
			GdkInputSource type = gdk_device_get_source (dev);
			if (type == GDK_SOURCE_KEYBOARD || type == GDK_SOURCE_ERASER)
			{
				member->data = NULL;
				continue;
			}

			GdkWindow *dwin = gdk_device_get_window_at_position (dev, NULL, NULL);
			if (dwin == NULL || (wwin != NULL && wwin != dwin))
			{
				member->data = NULL;
				continue;
			}
		}
		devices = g_list_remove_all (devices, NULL);
	}
	return devices;
}
/**
@brief get the position of the (or a) pointer-device associated with @a widget,
 during the processing of some event
Works fastest during button-press and button-release events.
This func accesses X11, so should be called only with BGL closed.
Returned coordinates are relative to the upper left corner of @a widget ->window.

@param widget the widget relative to which the position is wanted
@param x pointer to store for device X-coordinate, or NULL
@param y pointer to store for device Y-coordinate, or NULL

@return TRUE if the position(s) are set, else FALSE
*/
gboolean e2_utils_get_pointer_position (GtkWidget *widget, gint *x, gint *y)
{
	GdkDevice *device;
	GdkEvent *event = gtk_get_current_event ();
	if (event->type == GDK_BUTTON_PRESS || event->type == GDK_BUTTON_RELEASE)
		device = gdk_event_get_device (event);
	else
	{
		GList *devs = _e2_utils_get_pointer_devices_for_widget (widget);
		if (devs != NULL)
		{
			device = g_list_last (devs)->data;
			g_list_free (devs);
		}
		else
			device = NULL;
	}

	if (device != NULL)
	{
		GdkWindow *win = gtk_widget_get_window (widget);
		//this func returns NULL, but that's irrelevant, contrary to implication of API doc
		gdk_window_get_device_position (win, device, x, y, NULL); //accesses X11
		return TRUE;
	}
	//in case upstream checks these
	if (x != NULL)
		*x = 0;
	if (y != NULL)
		*y = 0;
	return FALSE;
}
#endif
/**
@brief emit beep sound
@return
*/
void e2_utils_beep (void)
{
	GdkDisplay *display =
	gdk_display_manager_get_default_display (gdk_display_manager_get());
	gdk_display_beep (display);
}
/**
@brief check whether more than 1 item is selected
@param srclist glist of selected items
@return TRUE if more than 1 is selected
*/
gboolean e2_utils_multi_src (GList *srclist)
{
	gint ctr=0;
	for (; srclist != NULL ; srclist = srclist->next)
	{
		ctr++;
		if (ctr > 1) break;
	}
	return (ctr > 1);
}
/**
@brief block all relevant signals to a thread
Posix doesn't specify which thread receives signals. So this func is generally
called inside newly-created threads to prevent signals (esp. SIGCHILD etc for
running commands) being delivered to the wrong thread.
@return
*/
void e2_utils_block_thread_signals (void)
{
	sigset_t set;
	sigfillset (&set);	//block all allowed signals
//	sigemptyset (&set);	//block SIGCHILD signals
//	sigaddset (&set, SIGCHLD);
	pthread_sigmask (SIG_BLOCK, &set, NULL);
}
/**
@brief get char (if any) used as mnemonic in translated @a label
@param label translated, UTF8-compatible string which may include an '_'
		indicating the following char is a mnemonic
@return lower-case char, if there's a mnemonic, otherwise (gunichar)0
*/
gunichar e2_utils_get_mnemonic_char (const gchar *label)
{
	gunichar c;
	const gchar *uscore = strchr (label, '_');
	if (uscore == NULL)
		c = (gunichar)'\0';
	else
	{
		uscore++;
		if (*uscore != '\0')
		{
			c = g_utf8_get_char_validated (uscore, -1);
			if (c == (gunichar)-1 || c == (gunichar)-2)
				c = (gunichar)'\0';
			else
				c = g_unichar_tolower (c);
		}
		else
			c = (gunichar)'\0';
	}
	return c;
}
/**
@brief get gdk key code which matches the char (if any) used as mnemonic in translated @a label
@param label translated, utf8-compatible string which may include an '_'
		indicating the following char is a mnemonic
@return gdk keycode, if there's a mnemonic, otherwise 0
*/
guint e2_utils_get_mnemonic_keycode (gchar *label)
{
	gunichar c = e2_utils_get_mnemonic_char (label);
	guint retval = (c == (gunichar)'\0') ? 0 : gdk_unicode_to_keyval (c);
	return retval;
}
#ifndef USE_GTK2_10
/**
@brief check whether @a event is for a modifier-key
@param event Gdk event data struct

@return TRUE for a modifier key
*/
gboolean e2_utils_key_is_modifier (GdkEventKey *event)
{
	//FIXME do this better
	switch (event->keyval)
	{
		case GDK_Shift_L:
		case GDK_Shift_R:
		case GDK_Control_L:
		case GDK_Control_R:
		case GDK_Alt_L:
		case GDK_Alt_R:
		case GDK_Meta_L:
		case GDK_Meta_R:
		case GDK_Super_L:
		case GDK_Super_R:
		case GDK_Hyper_L:
		case GDK_Hyper_R:
		case GDK_Kana_Shift:
		case GDK_Eisu_Shift:
			return TRUE;
		default:
			return FALSE;
	}
}
#endif
/**
@brief generic key-press/release callback

@param widget UNUSED the focused widget when the key was pressed or released
@param event pointer to event data struct
@param user_data UNUSED data specified when callback was connected

@return FALSE always
*/
gboolean e2_utils_key_translate_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer user_data)
{
	printd (DEBUG, "e2_utils_key_translate_cb, key: %u widget: %x", event->keyval, widget);
//	CLOSEBGL
	e2_utils_translate_key_event (event);
//	OPENBGL
	return FALSE;
}
/**
@brief compare-function for hash table
@param a pointerised keycode to compare
@param b pointerised keycode to compare

@return TRUE if @a equals @b
*/
static gboolean _e2_utils_match_translate_keys (gpointer a, gpointer b)
{
	return (a == b);
}
/**
@brief setup key-translation hash-tables
@a native is commonly TRUE, @a local is commonly FALSE
Totally wasteful, but not fatal, if both @a native and @a local are FALSE
@param native TRUE to [re]create the 'native-to-localised' table
@param local TRUE to [re]create the 'local-to-native' table
@return
*/
void e2_utils_translate_keys (gboolean native, gboolean local)
{
	guint i;
	gchar asciistr[2];
	gunichar unistr[2];
	const gchar *translated;
	gchar *freeme = NULL;

	if (native)
	{
		if (G_UNLIKELY(app.keysnative != NULL))
			g_hash_table_destroy (app.keysnative);
		app.keysnative = g_hash_table_new_full (g_direct_hash,
			(GEqualFunc)_e2_utils_match_translate_keys,	NULL, NULL);
	}
	if (local)
	{
		if (G_UNLIKELY(app.keyslocal != NULL))
			g_hash_table_destroy (app.keyslocal);
		app.keyslocal = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
	}

	/* Note for translators - key shortcut translations
	   At runtime, keybinding strings for alphabetic keys like "a" or
	   "<Control>z" are interrogated and the letter is replaced by the
	   _same-position_ letter from a translated UTF-8 string corresponding
	   to "KEYS_abcdefghijklmnopqrstuvwxyz". Any specified modifier(s) e.g.
	   "<Control><Alt>" are unchanged by this translation process.
	   All letters in the translated string are lower-case. The translated
	   string need not have a 'prefix' before its 1st letter, but if it does,
	   the prefix must have a single '_' to indicate the start of the actual
	   letters. It may be appropriate to pad the translated string, to align
	   the position of replacement letters with corresponding originals. In
	   that case, and if there's no suitable alternative key, insert the
	   english letter unchanged, or (more explicit) '\001' (without the quotes),
	   for each padded position. The translated string need not have all 26
	   replacements, but must have one (letter or padder) for each position
	   up to the last one to be handled i.e. no intermediate gaps.
	*/
	const gchar *newkeys = _("KEYS_abcdefghijklmnopqrstuvwxyz");
	if (newkeys == NULL || !strcmp (newkeys, "KEYS_abcdefghijklmnopqrstuvwxyz"))
		return;

	if (g_utf8_validate (newkeys, -1, &translated))
	{
		translated = strchr (newkeys, '_');
		if (translated != NULL)
			translated++;
		else
			translated = newkeys;
	}
	else	//use as much of the string as possible
	{
		//FIXME warn user
		printd (WARN, "Encoding error in keybindings translation template");
		const gchar *s = memchr (newkeys, '_', translated - newkeys);
		if (s != NULL && ++s < translated)
		{
			freeme = g_strndup (s, translated - s);
			translated = (const gchar *)freeme;
		}
		else
			return;	//can't decide whether any of the string is relevant
	}

	asciistr[1] = '\0';
	unistr[1] = '\0';

	for (i = 0; i < 26; i++)
	{
		if (*translated != '\0')
		{
			unistr[0] = g_unichar_tolower (g_utf8_get_char (translated));
			gchar *converted = g_ucs4_to_utf8 (unistr, 1, NULL, NULL, NULL);

			if (converted != NULL)
			{
				asciistr[0] = 'a' + i;
				if (strcmp (asciistr, converted))
				{
					guint keycode;
					gtk_accelerator_parse (converted, &keycode, NULL);
					if (keycode != 0)
					{
						if (native)
							g_hash_table_insert (app.keysnative,
								GUINT_TO_POINTER(keycode), GUINT_TO_POINTER(GDK_a + i));
						if (local)
							g_hash_table_insert (app.keyslocal,
								g_strdup(asciistr), converted);
					}
					else
						g_free (converted);
				}
				else
					g_free (converted);
			}

			translated = g_utf8_next_char(translated);
		}
		else
			break;
	}
	if (freeme != NULL)
		g_free (freeme);
}
/**
@brief set event data to 'vanilla' mod-key-data if appropriate
@param event pointer to key-event data

@return
*/
void e2_utils_translate_key_event (GdkEventKey *event)
{
	if (event->length > 0 //0 length is probably a mod key, but in any event, not translatable
		&& (event->state & E2_MODIFIER_MASK & ~GDK_SHIFT_MASK))
	{
		guint asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (event->keyval)));
		if (asciicode != 0)
		{
			gchar str[2] = { 0, 0 };
			event->keyval = asciicode;
			event->length = 1;
			str[0] = 'a' + (asciicode - GDK_a);
			g_free (event->string);
			event->string = g_strdup (str);
			//CHECKME what about hardware code ?
			printd (DEBUG, "e2_utils_translate_key_event, new key %s", asciicode);
		}
	}
}
/**
@brief generic button-press callback
This logs event data to use when processing related button-event(s) e.g. releases,
to allow matching of events from multiple pointers.
This does not need to be called for all press-events. It may be called directtly
e.g. from inside another cb.
@param widget UNUSED object where button-press occurred
@param event Gdk event data struct
@param user_data UNUSED pointer to callback data

@return FALSE
*/
gboolean e2_utils_generic_press_cb (GtkWidget *widget, GdkEventButton *event,
	gpointer user_data)
{
	//not interested in multi-press events
	if (event->type == GDK_BUTTON_PRESS)
	{
		E2_BtnEvent *data = ALLOCATE (E2_BtnEvent);
		CHECKALLOCATEDWARN (data, return FALSE);

		data->button = event->button;
		data->x_root = event->x_root;
		data->y_root = event->y_root;
		//get data for click-vs-drag checks - CHECKME could do this less-often ?
		GtkSettings* defs = gtk_settings_get_default ();
		g_object_get (G_OBJECT (defs), "gtk-dnd-drag-threshold",
			&data->drag_threshold, NULL);
//		NEEDCLOSEBGL
#ifdef USE_GTK2_22
		data->device_name = g_strdup (gdk_device_get_name (event->device));
#else
		data->device_name = g_strdup (event->device->name);
#endif
//		NEEDOPENBGL
		press_events = g_list_prepend (press_events, data);
	}

	return FALSE;
}
/**
@brief queue a fake event to trigger UI update e.g. popup of any pending blocked-dialog
*/
void e2_utils_fake_event (void)
{
	//CHECKME can GDK_VISIBILITY_NOTIFY event work ? window needs corresponding mask ?
	GdkEvent *event = gdk_event_new (GDK_BUTTON_PRESS); //GDK_VISIBILITY_NOTIFY);
	event->any.window =
#ifdef USE_GTK2_14
		gtk_widget_get_window (app.main_window);
#else
		app.main_window->window;
#endif
//	event->any.send_event = 1;
	event->button.button = 0;
	event->button.state = 0;
//	event->visibility.state = GDK_VISIBILITY_PARTIAL;
	gtk_widget_event (app.main_window, event);
	event->any.window = NULL; //prevent destruction of the 'real' event window during cleanup
	gdk_event_free (event);
}
/* *
@brief generic button-release callback
This checks the listed press-events to reconcile events from multiple pointers.
This may also be called directtly from inside another cb.
@param widget UNUSED object where button-release occurred
@param event Gdk event data struct
@param user_data UNUSED pointer to callback data

@return FALSE
*/
/*UNUSED
gboolean e2_utils_generic_release_cb (GtkWidget *widget, GdkEventButton *event,
	gpointer user_data)
{
	if (event->type == GDK_BUTTON_RELEASE)
	{
		GList *member;
		CLOSEBGL
		for (member = press_events; member != NULL; member = member->next)
		{
			E2_BtnEvent *data = (E2_BtnEvent *)member->data;
			gboolean match = (data->button == event->button &&
			((data->device_name == NULL && event->device->name == NULL)
			  || !strcmp (data->device_name, event->device->name)));
			if (match)
			{
				press_events = g_list_remove (press_events, data);
				g_free (data->device_name);
				DEALLOCATE (E2_BtnEvent, data);
				break;
			}
		}
		OPENBGL
	}
	return FALSE;
}
*/
/**
@brief generic mouse-drag check, following a button-release
This checks the listed press-events to reconcile events from multiple pointers.
@param event Gdk event data struct

@return 0 if no matching event, 1 if matched but no drag, 2 if the event occurred
	sufficiently far from the corresponding press event
*/
gint e2_utils_check_drag (GdkEventButton *event)
{
	gint drag = 0;
	if (event->type == GDK_BUTTON_RELEASE)
	{
		GList *member;
		for (member = press_events; member != NULL; member = member->next)
		{
			gboolean match;
			E2_BtnEvent *data = (E2_BtnEvent *)member->data;
#ifdef USE_GTK2_22
			if (data->button == event->button)
			{
				const gchar *devname = gdk_device_get_name (event->device);
				match = ((data->device_name == NULL && devname == NULL)
			  			|| !strcmp (data->device_name, devname));
			}
			else
				match = FALSE;
#else
			match = (data->button == event->button &&
			((data->device_name == NULL && event->device->name == NULL)
			  || !strcmp (data->device_name, event->device->name)));
#endif
			if (match)
			{
				drag =
					(ABS (event->x_root - data->x_root) > data->drag_threshold ||
					 ABS (event->y_root - data->y_root) > data->drag_threshold) ?
					2:1;
				press_events = g_list_remove (press_events, data);
				g_free (data->device_name);
				DEALLOCATE (E2_BtnEvent, data);
				break;
			}
		}
	}
	return drag;
}
/**
@brief generic check, following a button-release
This checks the listed press-events to reconcile events from multiple pointers.
@param event Gdk event data struct

@return TRUE if matching press-event was fount
*/
gboolean e2_utils_check_release (GdkEventButton *event)
{
	gboolean match = FALSE;
	if (event->type == GDK_BUTTON_RELEASE)
	{
		GList *member;
		for (member = press_events; member != NULL; member = member->next)
		{
			E2_BtnEvent *data = (E2_BtnEvent *)member->data;
#ifdef USE_GTK2_22
			if (data->button == event->button)
			{
				const gchar *devname = gdk_device_get_name (event->device);
				match = ((data->device_name == NULL && devname == NULL)
			  			|| !strcmp (data->device_name, devname));
			}
			else
				match = FALSE;
#else
			match = (data->button == event->button &&
			((data->device_name == NULL && event->device->name == NULL)
			  || !strcmp (data->device_name, event->device->name)));
#endif
			if (match)
			{
				press_events = g_list_remove (press_events, data);
				g_free (data->device_name);
				DEALLOCATE (E2_BtnEvent, data);
				break;
			}
		}
	}
	return match;
}
