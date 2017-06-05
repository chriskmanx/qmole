/* $Id: e2_complete.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2004-2009 tooar <tooar@emelfm2.net>

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
@file src/command/complete/e2_complete.h
@brief header for string auto completion

This is the header file for the string auto completion functions.
*/

#ifndef __E2_COMPLETE_H__
#define __E2_COMPLETE_H__

#include "emelfm2.h"

typedef enum
{
	E2_COMPLETE_FLAG_DONT_ESCAPE = 1,	//unused
	E2_COMPLETE_FLAG_STOP    = 1<<1,
	E2_COMPLETE_FLAG_ALL     = 1<<2,
	E2_COMPLETE_FLAG_FILES   = 1<<3,
	E2_COMPLETE_FLAG_DIRS    = 1<<4,
	E2_COMPLETE_FLAG_EXEC    = 1<<5,
	E2_COMPLETE_FLAG_PATH    = 1<<6,
	E2_COMPLETE_FLAG_HISTORY = 1<<7,
	E2_COMPLETE_FLAG_MOUNT   = 1<<8,
	E2_COMPLETE_FLAG_SHELL   = 1<<9,
} E2_CompleteFlags;

typedef struct _E2_CompleteRuntime
{
	guint priority;
	E2_CompleteFlags flags;
	//the funtion which implements the completion method
	guint (*func)(gchar*,gchar*,gint,GList**,E2_CompleteFlags*,gpointer,guint);
	gpointer data;	//available for method-specific data usage
} E2_CompleteRuntime;

guint e2_complete_str (gchar **complete, gint *pos, GList **found,
	E2_CompleteFlags flags, guint pane);
void e2_complete_init (void);
void e2_complete_clear (void);

  /*************************************************/
 /***** functions from e2_complete__*.c files *****/
/*************************************************/

guint e2_complete_path (gchar *line, gchar *word, gint pos, GList **found,
	E2_CompleteFlags *flags, gpointer data, guint unused);
#ifdef E2_FS_MOUNTABLE
guint e2_complete_mount (gchar *line, gchar *word, gint pos, GList **found,
	E2_CompleteFlags *flags, gpointer data, guint unused);
gboolean e2_complete_mount_menu_create (gpointer from, E2_ActionRuntime *art);
void e2_complete_actions_register (void);
#endif
#if defined(__linux__) || defined(__FreeBSD__)
GList *e2_complete_mount_get_fusemounts_list (void);
#endif

#endif //ndef __E2_COMPLETE_H__
