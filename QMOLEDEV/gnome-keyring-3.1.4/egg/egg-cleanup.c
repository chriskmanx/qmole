/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-cleanup.c - for data cleanup at end of program

   Copyright (C) 2007 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "egg-cleanup.h"

typedef struct _EggCleanup {
	GDestroyNotify notify;
	gpointer user_data;
} EggCleanup;

static GSList *registered_cleanups = NULL;

void    
egg_cleanup_register (GDestroyNotify notify, gpointer user_data)
{
	EggCleanup *cleanup = g_new0 (EggCleanup, 1);
	
	g_assert (notify);
	cleanup->notify = notify;
	cleanup->user_data = user_data;
	
	/* Note we're reversing the order, so calls happen that way */
	registered_cleanups = g_slist_prepend (registered_cleanups, cleanup);
}

void
egg_cleanup_unregister (GDestroyNotify notify, gpointer user_data)
{
	EggCleanup *cleanup;
	GSList *l;
	
	for (l = registered_cleanups; l; l = g_slist_next (l)) {
		cleanup = (EggCleanup*)l->data;
		if (cleanup->notify == notify && cleanup->user_data == user_data) {
			registered_cleanups = g_slist_remove (registered_cleanups, cleanup);
			break;
		}
	}
}


void    
egg_cleanup_perform (void)
{
	GSList *cleanups, *l;
	EggCleanup *cleanup;
	
	while (registered_cleanups) {
		
		/* 
		 * While performing cleanups, more cleanups may be registered.
		 * So swap out the list, and keep going until empty.
		 */
	 
		cleanups = registered_cleanups;
		registered_cleanups = NULL;
		
		for (l = cleanups; l; l = g_slist_next (l)) {
			cleanup = (EggCleanup*)l->data;
			g_assert (cleanup->notify);
			
			(cleanup->notify) (cleanup->user_data);
			g_free (cleanup);
		}
		
		g_slist_free (cleanups);
	}
}
