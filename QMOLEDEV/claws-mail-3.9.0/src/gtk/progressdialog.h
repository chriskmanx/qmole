/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __PROGRESS_H__
#define __PROGRESS_H__

#include <glib.h>
#include <gtk/gtk.h>

typedef struct _ProgressDialog	ProgressDialog;

struct _ProgressDialog
{
	GtkWidget *window;
	GtkWidget *label;
	GtkWidget *showlog_btn;
	GtkWidget *cancel_btn;
	GtkWidget *progressbar;
	GtkWidget *treeview;
	GtkListStore *store;
};

ProgressDialog *progress_dialog_create	(void);
void progress_dialog_set_label		(ProgressDialog	*progress,
					 gchar		*str);
void progress_dialog_get_fraction	(ProgressDialog	*progress);
void progress_dialog_set_fraction	(ProgressDialog	*progress,
					 gfloat		 percentage);
void progress_dialog_destroy		(ProgressDialog	*progress);


/*
 * Use these functions to access the dialog list
 */

gint progress_dialog_list_set_status	(ProgressDialog *progress,
					 gint		 row,
					 const gchar    *status);
gint progress_dialog_list_set		(ProgressDialog *progress,
					 gint		 row,
					 GdkPixbuf	*image,
					 const gchar	*account_name,
					 const gchar    *status);

void progress_dialog_scroll_to_row	(ProgressDialog	*progress,
					 gint		 row);

#endif /* __PROGRESS_H__ */
