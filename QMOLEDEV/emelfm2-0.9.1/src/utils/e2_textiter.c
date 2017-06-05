/* $Id: e2_textiter.c 2486 2012-04-06 10:14:55Z tpgww $

Portions copyright (C) 2003-2012 tooar <tooar@emelfm2.net>
Portions copyright (C) 2000, 2002 Paolo Maggi
Portions copyright (C) 2002, 2003 Jeroen Zwartepoorte

This sourcecode is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This sourcecode is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with emelFM2. If not, see http://www.gnu.org/licenses.
*/

/*
Parts of this file are from the gedit or glimmer projects.
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_textiter.h"

#define GTK_TEXT_UNKNOWN_CHAR 0xFFFC

/**
@brief move foward @a count characters, ignoring any that match supplied flags

@param iter text iter referring to current position
@param count the no. of chars to move
@param skip_invisible TRUE to skip invisible chars
@param skip_nontext TRUE to skip non-text chars
@param skip_decomp TRUE to skip canonical decompositions

@return
*/
static void _e2_textiter_forward_chars_with_skipping (
		GtkTextIter *iter,
		gint         count,
		gboolean     skip_invisible,
		gboolean     skip_nontext,
		gboolean     skip_decomp)
{
	gint i;

	g_return_if_fail (count >= 0);

	i = count;

	while (i > 0)
	{
		gboolean ignored = FALSE;

		if (skip_nontext && gtk_text_iter_get_char (iter) == GTK_TEXT_UNKNOWN_CHAR)
			ignored = TRUE;

		if (!ignored && skip_invisible
		    //&& _gtk_text_btree_char_is_invisible (iter)
			)
				ignored = TRUE;

		if (!ignored && skip_decomp)
		{
			/* being utf8-correct sucks; this accounts for extra
			   offsets coming from canonical decompositions of
			   utf8 characters (e.g. accented characters) which
			   g_utf8_normalize() performs */
			gsize decomp_len;
#ifdef USE_GLIB2_30
			decomp_len = g_unichar_fully_decompose (
				gtk_text_iter_get_char (iter), FALSE, NULL, 0);
#else
			gunichar *decomp;
			decomp = g_unicode_canonical_decomposition (
				gtk_text_iter_get_char (iter), &decomp_len);
			g_free (decomp);
#endif
			i -= (decomp_len - 1);
		}

		gtk_text_iter_forward_char (iter);

		if (!ignored)
			--i;
	}
}
/**
@brief scan forwards from @a start for text which matches @a lines
This performs line-by-line case-insensitive comparisons
It may be re-entrant, when @a lines has > 1 member
@param start pointer to textiter in buffer where the search is to commence
@param lines array of \n-trailing line-strings, together comprising the search string
@param visible_only TRUE to ignore hidden chars in the text
@param slice TRUE to include "unknown" (0xFFFC) chars in the text
@param match_start textiter to store the start of a matching string, or NULL when re-entering to process line(s) after 1st
@param match_end textiter store the start of a matching string, or NULL

@return TRUE if a match was found
*/
static gboolean _e2_textiter_forward_lines_match (
		const GtkTextIter *start,
		const gchar      **lines,
		gboolean           visible_only,
		gboolean           slice,
		GtkTextIter       *match_start,
		GtkTextIter       *match_end)
{
	GtkTextIter next;
	gchar *line_text;
	const gchar *found;
	gint offset;

	if (*lines == NULL || **lines == '\0')
	{	//nothing to match, so everything matches
		if (match_start)
			*match_start = *start;
		if (match_end)
			*match_end = *start;
		return TRUE;
	}

	//CHECKME why break search into lines ?
	next = *start;
	if (!gtk_text_iter_forward_line (&next))
	{
		if (gtk_text_iter_is_end (start))
			return FALSE;
		else
		{
			next = *start;
			gtk_text_iter_forward_to_end (&next);	//CHECKME
		}
	}
	else if (gtk_text_iter_equal (start, &next)) //no more text in buffer
		return FALSE;

	if (slice)
	{
		if (visible_only)
			line_text = gtk_text_iter_get_visible_slice (start, &next);
		else
			line_text = gtk_text_iter_get_slice (start, &next);
	}
	else
	{
		if (visible_only)
			line_text = gtk_text_iter_get_visible_text (start, &next);
		else
			line_text = gtk_text_iter_get_text (start, &next);
	}

	if (match_start != NULL) //if this is the first line we're matching
	{
		found = e2_utf8_strcasestr (line_text, *lines);
	}
	else
	{
		//not the first line, we have to match from the start of the line
		if (e2_utf8_caseless_match (line_text, *lines, -1, -1))
			found = line_text;
		else
			found = NULL;
	}

	if (found == NULL)
	{
		g_free (line_text);
		return FALSE;
	}

	//get offset to start of search string
	offset = g_utf8_strlen (line_text, found - line_text);

	next = *start;

	/* If match start needs to be returned, set it to the
	   start of the search string */
	_e2_textiter_forward_chars_with_skipping (&next, offset, visible_only,
		!slice, FALSE);
	if (match_start != NULL)
	{
		*match_start = next;
	}

	//go to end of search string
	_e2_textiter_forward_chars_with_skipping (&next, g_utf8_strlen (*lines, -1),
		visible_only, !slice, TRUE);

	g_free (line_text);

	++lines;

	if (match_end != NULL)
		*match_end = next;

	/* try to match the rest of the lines forward, passing NULL for
	   match_start so lines_match will try to match the entire line */
	return _e2_textiter_forward_lines_match (&next, lines, visible_only, slice,
		NULL, match_end);
}
/**
@brief scan backwards from @a start for text which matches @a lines
This performs line-by-line case-insensitive comparisons
@param start pointer to textiter in buffer where the search is to commence
@param lines array of \n-trailing line-strings, together comprising the search string
@param visible_only TRUE to ignore hidden chars in the text
@param slice TRUE to include "unknown" (0xFFFC) chars in the text
@param match_start textiter to store the start of a matching string, or NULL when re-entering to parse lines after 1st
@param match_end textiter store the start of a matching string, or NULL

@return TRUE if a match was found
*/
static gboolean _e2_textiter_backward_lines_match (
		const GtkTextIter *start,
		const gchar      **lines,
		gboolean           visible_only,
		gboolean           slice,
		GtkTextIter       *match_start,
		GtkTextIter       *match_end)
{
	GtkTextIter line, next;
	gchar *line_text;
	const gchar *found;
	gint offset;

	if (*lines == NULL || **lines == '\0')
	{	//nothing to match, so everything matches
		if (match_start)
			*match_start = *start;
		if (match_end)
			*match_end = *start;
		return TRUE;
	}

	line = next = *start;
	if (gtk_text_iter_get_line_offset (&next) == 0)	//CHECKME why break search into lines ?
	{
		if (!gtk_text_iter_backward_line (&next))
			return FALSE;
	}
	else
		gtk_text_iter_set_line_offset (&next, 0);

	if (slice)
	{
		if (visible_only)
			line_text = gtk_text_iter_get_visible_slice (&next, &line);
		else
			line_text = gtk_text_iter_get_slice (&next, &line);
	}
	else
	{
		if (visible_only)
			line_text = gtk_text_iter_get_visible_text (&next, &line);
		else
			line_text = gtk_text_iter_get_text (&next, &line);
	}

	if (match_start != NULL) //if this is the first line we're matching
	{
		found = e2_utf8_strrcasestr (line_text, *lines);
	}
	else
	{
		//not the first line, we have to match from the start of the line
		if (e2_utf8_caseless_match (line_text, *lines, -1, -1))
			found = line_text;
		else
			found = NULL;
	}

	if (found == NULL)
	{
		g_free (line_text);
		return FALSE;
	}

	//get offset to start of search string
	offset = g_utf8_strlen (line_text, found - line_text);

	_e2_textiter_forward_chars_with_skipping (&next, offset, visible_only,
		!slice, FALSE);

	//if match start needs to be returned, set it to the start of the search string
	if (match_start != NULL)
	{
		*match_start = next;
	}

	//go to end of search string
	_e2_textiter_forward_chars_with_skipping (&next, g_utf8_strlen (*lines, -1),
		visible_only, !slice, TRUE);

	g_free (line_text);

	++lines;

	if (match_end != NULL)
		*match_end = next;

	/* try to match the rest of the lines forward, passing NULL for
	   match_start so lines_match will try to match the entire line */
	return _e2_textiter_forward_lines_match (&next, lines, visible_only, slice,
		NULL, match_end);
}
/**
@brief Search forward from @a iter to try to to find next match of @a str in textbuffer

This is like gtk_text_iter_forward_search(), but supports case-insensitive
and whole-word searching.

Any match is returned by setting @a match_start to the first character of
the match and @a match_end to the first character after the match. The search
will not continue past @a limit.
Note that a search is a linear or O(n) operation, so you may wish to use
@a limit to avoid locking up your UI on large buffers.

If the GTK_SOURCE_SEARCH_VISIBLE_ONLY flag is present, the match may have
invisible text interspersed in @a str. i.e. @a str will be a
possibly-noncontiguous subset of the matched range. Similarly, if you
specify GTK_SOURCE_SEARCH_TEXT_ONLY, the match may have pixbufs or child
widgets inside the matched range. If these flags are not given, the match
must be exact; the special 0xFFFC character in @a str will match embedded
pixbufs or child widgets. If you specify the GTK_SOURCE_SEARCH_CASE_INSENSITIVE
flag, the text will be matched regardless of its case.

@param  iter GtkTextIter where the search begins
@param  str  search string, may include 1 or more \n
@param  flags  flags affecting how the search is done
@param  match_start  return location for start of match, or NULL
@param  match_end  return location for end of match, or NULL
@param  limit  upper bound for the match start, or NULL for the end of the buffer

@return  TRUE if a match was found
*/
gboolean e2_iter_forward_search (
		const GtkTextIter   *iter,
		const gchar         *str,
		E2TextSearchFlags   flags,
		GtkTextIter         *match_start,
		GtkTextIter         *match_end,
		const GtkTextIter   *limit)
{
	gboolean visible_only, slice, retval;
	GtkTextIter match, search;

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (str != NULL, FALSE);

	if (!(flags & E2_SEARCH_CASE_INSENSITIVE))
	{
		search = *iter;
rescan:
		retval = gtk_text_iter_forward_search (&search, str, flags,
					match_start, match_end, limit);
		if (retval && (flags & E2_SEARCH_WHOLE_WORD)
					&& (!gtk_text_iter_starts_word (match_start)
		//to allow incremental searching, whole_words option causes only a check
		//for word start here, the end-check is done when highlighting
						//|| !gtk_text_iter_ends_word (match_end)
		))
		{
			search = *match_start;
			if (gtk_text_iter_forward_char (&search))
				goto rescan;
			retval = FALSE;
		}
		return retval;
	}

	if (limit != NULL && gtk_text_iter_compare (iter, limit) > 0)
		return FALSE;

	if (*str == '\0')	//matching nothing
	{
		//if we can move one char, return that location for a match
		match = *iter;
		if (gtk_text_iter_forward_char (&match))
		{
			if (limit == NULL || gtk_text_iter_compare (&match, limit) <= 0)
			{
				if (match_start != NULL)
					*match_start = match;
				if (match_end != NULL)
					*match_end = match;
				return TRUE;
			}
		}
		return FALSE;
	}

	//split search string into lines
	gchar **lines = e2_utils_str_breakup (str, "\n", -1);
	if (lines == NULL)
		return FALSE;	//FIXME warn user about error

	visible_only = (flags & E2_SEARCH_VISIBLE_ONLY) != 0;
	slice = (flags & E2_SEARCH_TEXT_ONLY) == 0;
	retval = FALSE;
	search = *iter;

	do
	{
		/* This loop has an inefficient worst-case, where
		   gtk_text_iter_get_text() is called repeatedly on a single line */
		GtkTextIter end;
rescan2:
		if (limit != NULL && gtk_text_iter_compare (&search, limit) > 0)
			break;

		if (_e2_textiter_forward_lines_match (&search, (const gchar**)lines,
				 visible_only, slice, &match, &end))
		{
			if (limit == NULL || gtk_text_iter_compare (&end, limit) <= 0)
			{
				if ((flags & E2_SEARCH_WHOLE_WORD) &&
				(!gtk_text_iter_starts_word (&match)
//see comment above re end-checking when highlighting || !gtk_text_iter_ends_word (&end)
					))
				{
					search = match;
					if (gtk_text_iter_forward_char (&search))
						goto rescan2;
				}
				else
				{
					retval = TRUE;
					if (match_start != NULL)
						*match_start = match;
					if (match_end != NULL)
						*match_end = end;
				}
			}
			break;
		}
	} while (gtk_text_iter_forward_line (&search));

	g_strfreev (lines);

	return retval;
}

/**
@brief Search backward from @a iter to try to find next match of @a str in textbuffer

This is like gtk_text_iter_backward_search(), but supports case-insensitive
and whole-word searching.
See comments for e2_iter_forward_search().

@param  iter a GtkTextIter where the search begins
@param  str search string, may include 1 or more \n
@param  flags bitmask of flags affecting the search
@param  match_start return location for start of match, or NULL
@param  match_end return location for end of match, or NULL
@param  limit lower bound of match end, or NULL for start of buffer

@return TRUE if a match was found
*/
gboolean e2_iter_backward_search (
		const GtkTextIter   *iter,
		const gchar         *str,
		E2TextSearchFlags    flags,
		GtkTextIter         *match_start,
		GtkTextIter         *match_end,
		const GtkTextIter   *limit)
{
	gboolean visible_only, slice, retval;
	GtkTextIter search, match;

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (str != NULL, FALSE);

	if (!(flags & E2_SEARCH_CASE_INSENSITIVE))
	{
		search = *iter;
rescan:
		retval = gtk_text_iter_backward_search (&search, str, flags,
						      match_start, match_end, limit);
		if (retval && (flags & E2_SEARCH_WHOLE_WORD)
					&& (!gtk_text_iter_starts_word (match_start)
//see comment above re end-checking when highlighting || !gtk_text_iter_ends_word (match_end)
		))
		{
			search = *match_start;
			if (gtk_text_iter_backward_char (&search))
				goto rescan;
			retval = FALSE;
		}
		return retval;
	}

	if (limit != NULL && gtk_text_iter_compare (iter, limit) < 0)
		return FALSE;

	if (*str == '\0') //matching nothing
	{
		//if we can move one char, return that location for the match
		match = *iter;
		if (gtk_text_iter_backward_char (&match))
		{
			if (limit == NULL || gtk_text_iter_compare (&match, limit) >= 0)
			{
				if (match_start != NULL)
					*match_start = match;
				if (match_end != NULL)
					*match_end = match;
				return TRUE;
			}
		}
		return FALSE;
	}

	//split search string into lines
	gchar **lines = e2_utils_str_breakup (str, "\n", -1);
	if (lines == NULL)
		return FALSE;	//FIXME warn user about error

	visible_only = (flags & E2_SEARCH_VISIBLE_ONLY) != 0;
	slice = (flags & E2_SEARCH_TEXT_ONLY) == 0;
	retval = FALSE;
	search = *iter;

	while (TRUE)
	{
		/* This loop has an inefficient worst-case, where
		   gtk_text_iter_get_text() is called repeatedly on each single line */
		GtkTextIter end;
rescan2:
		if (limit != NULL && gtk_text_iter_compare (&search, limit) < 0)
			break;

		if (_e2_textiter_backward_lines_match (&search, (const gchar**)lines,
					  visible_only, slice, &match, &end))
		{
			if (limit == NULL || gtk_text_iter_compare (&end, limit) >= 0)
			{
				if ((flags & E2_SEARCH_WHOLE_WORD) &&
				(!gtk_text_iter_starts_word (&match)
//see comment above re end-checking when highlighting || !gtk_text_iter_ends_word (&end)
					))
				{
					search = match;
					if (gtk_text_iter_backward_char (&search))
						goto rescan2;
				}
				else
				{
					retval = TRUE;
					if (match_start)
						*match_start = match;
					if (match_end)
						*match_end = end;
				}
			}
			break;
		}

		if (gtk_text_iter_get_line_offset (&search) == 0)	//at start of line
		{
			if (!gtk_text_iter_backward_line (&search))	//can't move to start of previous line
				break;
		}
		else
			gtk_text_iter_set_line_offset (&search, 0);	//go to start of current line and check again
	}

	g_strfreev (lines);

	return retval;
}
