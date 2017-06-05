/* $Id: e2_filelist.h 2995 2014-01-13 03:24:50Z tpgww $

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

#ifndef __E2_FILELIST_H__
#define __E2_FILELIST_H__

#include "emelfm2.h"

/*enum { FILENAME ... MAX_COLUMNS } specified in e2_fileview.h */
//additional model columns, not displayed
enum { NAMEKEY = MAX_COLUMNS,
	FINFO, FORECOLOR, BACKCOLOR,
#ifdef E2_SELTXT_RECOLOR
	SELCOLOR,
#endif
	MODEL_COLUMNS };  //VISIBLE, MODEL_COLUMNS };

typedef enum
{
	PANE1, PANE2, PANEACTIVE, PANEINACTIVE
} E2_ListChoice;

typedef struct _E2_Column
{
	gchar *title;
	gint size;	//default width
	gint (*sort_func) ();
} E2_Column;

//gboolean order;  //sort order flag, TRUE for ascending, FALSE for descending
E2_Column e2_all_columns[MAX_COLUMNS];

gboolean e2_filelist_repoll (GtkWidget *widget, GdkEvent *event, gpointer userdata);
void e2_filelist_stop_refresh_checks (void);
void e2_filelist_start_refresh_checks (void);
void e2_filelist_disable_one_refresh (E2_ListChoice pane);
void e2_filelist_enable_one_refresh (E2_ListChoice pane);
gboolean e2_filelist_disable_refresh_action (gpointer from, E2_ActionRuntime *art);
gboolean e2_filelist_enable_refresh_action (gpointer from, E2_ActionRuntime *art);
void e2_filelist_disable_refresh (void);
void e2_filelist_reset_refresh (void); //also does config !!
void e2_filelist_enable_refresh (void);
gboolean e2_filelist_request_refresh (gchar *dir, gboolean immediate);
//void e2_filelist_request_focus (GtkWidget *focus_wid);
gboolean e2_filelist_check_dirty (gpointer userdata);
gboolean e2_filelist_clear_old_stores (gpointer user_data);
GtkListStore *e2_filelist_fill_store (GList *entries, ViewInfo *view) G_GNUC_MALLOC;
GtkListStore *e2_filelist_make_store (void) G_GNUC_MALLOC;
#ifdef E2_VFS
GtkListStore *e2_filelist_copy_store (GtkListStore *original) G_GNUC_MALLOC;
#endif
void e2_filelist_cleaninfo (FileInfo *info, gpointer data);
gboolean e2_filelist_make_all_infos (gchar *parentpath, GList **list);

#endif //ndef __E2_FILELIST_H__
