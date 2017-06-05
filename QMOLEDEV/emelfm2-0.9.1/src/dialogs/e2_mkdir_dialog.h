/* $Id: e2_mkdir_dialog.h 2064 2010-03-12 13:15:36Z tpgww $

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

#ifndef __E2_MKDIR_DIALOG_H__
#define __E2_MKDIR_DIALOG_H__

#include "emelfm2.h"
#include "e2_task.h"

typedef struct _E2_MkdirDialogRuntime
{
	GtkWidget *dialog;
	GtkWidget *scrolled;
	GtkWidget *combo;
	GtkWidget *info_expander;
	GtkWidget *info_box;
	GtkWidget *info_label;
	GtkWidget *info_label2;
	GtkWidget *menu;
//	GtkWidget *create_btn;
	gchar *path; //UTF-8 encoded
	GList *history;
	gint history_cur;
	gboolean creation_possible;
	gboolean opt_show_last;
	gboolean opt_suggest_dir;
	guint idle_id;
	E2_TaskStatus *status;	//pointer to Q status indicator
} E2_MkdirDialogRuntime;

GtkWidget *e2_mkdir_dialog_create ();
void e2_mkdir_dialog_show ();
void e2_mkdir_dialog_actions_register ();
void e2_mkdir_dialog_options_register ();

#endif //ndef __E2_MKDIR_DIALOG_H__
