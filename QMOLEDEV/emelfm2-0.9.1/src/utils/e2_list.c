/* $Id: e2_list.c 2746 2013-09-19 22:59:03Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>
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

/**
@file src/utils/e2_list.c
@brief glist utilities

glist utility-functions

*/

#include "emelfm2.h"
#include <string.h>

/**
@brief compare function for finding @a b in a GList of strings

@param a string data for current member of the glist being scanned
@param b string to find in the glist

@return 0 if the strings are equal
*/
gint e2_list_strcmp (const gchar *a, const gchar *b)
{
	return strcmp (a, b);
}
/**
@brief update history-list for string @a latest
@a latest will be prepended to the list.
@param history store for history list pointer
@param latest string to be listed
@param cur store for new history length, or NULL
@param max the maximum length of the list, or 0 for no limit
@param doubl TRUE to allow multiple-instances of @a command in the list

@return
*/
void e2_list_update_history (GList **history, const gchar *latest,
	guint *cur, guint max, gboolean doubl)
{
	GList *tmp = doubl ? NULL : g_list_find_custom (*history, latest,
		(GCompareFunc) e2_list_strcmp);

	if (tmp != NULL)
	{	//found the string to be added in history already
		if (tmp != *history)
		{	//somewhere later in the list
			*history = g_list_remove_link (*history, tmp);
			*history = g_list_concat (tmp, *history);
		}
		//no net change of list length
	}
	else
	{	//multiples allowed, or new entry
		tmp = *history;
		if (tmp == NULL || strcmp ((gchar *)tmp->data, latest))
		{
			*history = g_list_prepend (*history, g_strdup (latest));
			if (max > 0 && g_list_length (*history) > max)
			{
				tmp = g_list_last (*history);
				g_free (tmp->data);
				*history = g_list_delete_link (*history, tmp);
			}
		}
	}
	if (cur != NULL)
		*cur = g_list_length (*history);
}
/**
@brief convert a list of strings to an array of strings

The strings in the returned array are not copied.
TODO error message assumes BGL closed

@param list GList with gchar pointers as data

@return allocated, NULL-terminated, array of strings from @a list, or NULL
*/
gchar **e2_list_to_strv (GList *list)
{
	guint len = g_list_length (list);
	gchar **strv = g_try_malloc ((len + 1) * sizeof (gpointer));
	CHECKALLOCATEDWARN (strv, return NULL;); //TODO assumes BGL closed
	if (strv)
	{
		guint i;
		GList *l;
		for (i = 0, l = list; i < len; i++, l = g_list_next (l))
			strv[i] = (gchar *)l->data;
		strv[len] = NULL;
	}
	return strv;
}
/**
@brief copy a GList of strings

This function copies a GList and also copies the data of each element
with g_strdup.

@param list the GList to copy

@return new GList that has to be freed
*/
GList *e2_list_copy_with_data (GList *list)
{
	GList *copy = NULL;
	GList *tmp;
	for (tmp = g_list_first (list); tmp != NULL; tmp = g_list_next (tmp))
	{
		copy = g_list_append (copy, g_strdup (tmp->data));
	}
	return copy;
}
/**
@brief clear list of strings
The list pointer is set to NULL after clearing
@param list store for list pointer

@return
*/
void e2_list_free_with_data (GList **list)
{
	if ((list != NULL) && (*list != NULL))
	{
		g_list_foreach (*list, (GFunc) g_free, NULL);
		g_list_free (*list);
		*list = NULL;
	}
}
/* *
@brief clear listed strings, but not the list itself

@param list store for list pointer

@return
*/
/* UNUSED
void e2_list_free_data_only (GList **list)
{
	if ((list != NULL) && (*list != NULL))
	{
		g_list_foreach (*list, (GFunc) g_free, NULL);
	}
} */
/* *
@brief find member of list @a list, whose data is @a search_text

@param list pointer to list to be scanned
@param search_text the text to find

@return pointer to list member that holds @a search_text, or NULL
*/
/* UNUSED
GList *e2_list_find_data_string (GList *list, gchar *search_text)
{
	GList *tmp;
	gchar *curr;

	for (tmp = list; tmp != NULL; tmp = tmp->next)
	{
		curr = tmp->data;
		if (!strcmp (curr, search_text)) return tmp;
	}
	return NULL;
} */

/* *
@brief break @a list into two parts, the second part starting at index @a breaker

@param list pointer to list to be processed, may be NULL
If @a part1 == @a part2, @a part1 will simply be overwritten
If @a part1 &/| @a part2 is NULL, corresponding list will be cleared, any data will leak
@param breaker index of a member of @a list which will start the second part
@param part1 store for pointer to first broken part, may be NULL
@param part2 store for pointer to second broken part, may be NULL

@return
*/
/*
void e2_list_nth_break (GList **list, guint breaker, GList **part1, GList **part2)
{
	if (*list == NULL)
		e2_list_break (NULL, NULL, FALSE, part1, part2);
	else if (breaker > g_list_length (*list))
		e2_list_break (list, NULL, TRUE, part1, part2);
	else
		e2_list_break (list, g_list_nth (*list, breaker), FALSE, part1, part2);
}
*/
/* *
@brief break @a list into two parts, the second part starting with @a breaker

@param list pointer to list to be processed, may be NULL
@a part1 or @a part2 may be address of @a list
If @a part1 == @a part2, @a part1 will simply be overwritten
If @a part1 &/| @a part2 is NULL, corresponding list will be cleared, any data will leak
@param breaker pointer to member of @a list which will start the second part, may be NULL
@param nullatend if TRUE, and @a breqak is NULL, it's at the end of @a list, i.e. from a nextward scan
@param part1 store for pointer to first broken part, may be NULL
@param part2 store for pointer to second broken part, may be NULL

@return
*/
/*void e2_list_break (GList **list, GList *breaker, gboolean nullatend,
	GList **part1, GList **part2)
{
	if (*list != NULL)
	{
		if (breaker != NULL)
		{
			GList *prev = breaker->prev;
			if (prev != NULL)
			{
				prev->next = NULL;
				breaker->prev = NULL;
				if (part1 != NULL)
					*part1 = *list;
				else
				{
					g_list_free (*list);	//any data will leak
					*list = NULL;
				}
			}
			if (part2 != NULL)
				*part2 = breaker;	//g_list_concat (NULL, breaker);
			else
			{
				g_list_free (breaker);	//any data will leak
//				breaker = NULL;
			}

		}
		else	//NULL breaker
		{
			if (nullatend)
			{
				if (part1 != NULL)
					*part1 = *list;
				else
				{
					g_list_free (*list);	//any data will leak
					*list = NULL;
				}
				if (part2 != NULL)
					*part2 = NULL;
			}
			else
			{
				if (part1 != NULL)
					*part1 = NULL;
				if (part2 != NULL)
					*part2 = *list;
				else
				{
					g_list_free (*list);	//any data will leak
					*list = NULL;
				}
			}
		}
	}
	else //list doesn't exist
	{
		if (part1 != NULL)
			*part1 = NULL;
		if (part2 != NULL)
			*part2 = NULL;
	}
}
*/
