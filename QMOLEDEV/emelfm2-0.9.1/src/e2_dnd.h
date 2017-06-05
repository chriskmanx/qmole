/* $Id: e2_dnd.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2006-2009 tooar <tooar@emelfm2.net>

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

#ifndef __E2_DND_H__
#define __E2_DND_H__

#include "emelfm2.h"

enum { TARGET_URI, TARGET_STRING, TARGET_XDS };

//GdkAtom atom_XdndDirectSave0;
//GdkAtom atom_text_plain;
//GdkAtom atom_text_uri_list;

void e2_dnd_drag_begin_cb (GtkWidget *treeview, GdkDragContext *context,
	ViewInfo *view);
gboolean e2_dnd_drag_motion_cb (GtkWidget *treeview, GdkDragContext *context,
	gint x, gint y, guint time, ViewInfo *view);
gboolean e2_dnd_drag_leave_cb (GtkWidget *treeview, GdkDragContext *context,
	guint time, ViewInfo *view);
/*void e2_dnd_drag_delete_cb (GtkWidget *treeview, GdkDragContext *context,
	ViewInfo *view); */
//gboolean e2_dnd_drag_drop_cb (GtkWidget *treeview, GdkDragContext *context,
//	gint x, gint y, guint time, ViewInfo *view);
void e2_dnd_drag_data_get_cb (GtkWidget *treeview, GdkDragContext *context,
	GtkSelectionData *data, guint info_arg, guint time, ViewInfo *view);
void e2_dnd_drag_data_received_cb (GtkWidget *treeview, GdkDragContext *context,
	gint x, gint y, GtkSelectionData *data, guint drag_info, guint time,
	ViewInfo *view);

#endif	//ndef __E2_DND_H__
