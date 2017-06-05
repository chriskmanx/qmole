/* $Id: e2_select_image_dialog.h 2377 2011-06-11 00:16:34Z tpgww $

Copyright (C) 2003-2009 tooar <tooar@emelfm2.net>

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

#ifndef __E2_SELECT_IMAGE_DIALOG_H__
#define __E2_SELECT_IMAGE_DIALOG_H__

#include "emelfm2.h"

typedef struct _E2_SID_Runtime
{
	gchar *name;
	gchar *icon; //gtk-stock-icon name, or path to icon file, taken from tree-option store
	GtkWidget *dialog;
	GtkWidget *parent;
	GtkNotebook *notebook;
	gint page;
	GtkIconView *stockview;
	GtkTreeModel *stockmodel;
	GtkIconView *customview;
	GtkTreeModel *custommodel;
	GtkWidget *dir_chooser; //button
	GtkWidget *rem_btn;
} E2_SID_Runtime;

GtkWidget *e2_sid_create (GtkWidget *parent, const gchar *name, gchar *icon, GdkEventButton *event);

#endif //ndef __E2_SELECT_IMAGE_DIALOG_H__
