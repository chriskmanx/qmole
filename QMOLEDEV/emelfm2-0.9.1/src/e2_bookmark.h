/* $Id: e2_bookmark.h 2118 2010-06-25 04:30:29Z tpgww $

Copyright (C) 2004-2010 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_BOOKMARK_H__
#define __E2_BOOKMARK_H__

#include "emelfm2.h"

//mark_flags contents
enum
{
	E2_MARKFLAG_ACTIONS = 1,
	E2_MARKFLAG_ONETIME = 1 << 1,
};

/* data associated with each item in the bookmarks menu */
typedef struct _E2_MarkData
{
	gint32 pane_num;	//E2PANECUR, E2PANE1, E2PANE2, pane in which menu shown, hence view->dir
	guint32 mark_flags;	//flags for: no-actions-menu, destroy top_menu ...
	gchar *mark_path;	//bookmark dir for item in relevant marks (sub)menu
	GtkTreePath *tpath; //bookmarks-set treemodel path of item in relevant marks (sub)menu
	GtkWidget *top_menu; //main bookmarks menu, for hide and/or destroy
} E2_MarkData;

void e2_bookmark_add_cb (GtkWidget *widget, gchar *arg);
gboolean e2_bookmark_button_press_cb (GtkWidget *widget, GdkEventButton *event,
	E2_MarkData *mdata);

//void e2_bookmark_clean ();
void e2_bookmark_actions_register ();
void e2_bookmark_options_register ();

#endif //ndef __E2_BOOKMARK_H__
