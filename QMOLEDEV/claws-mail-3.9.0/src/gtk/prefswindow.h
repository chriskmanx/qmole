/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail Team
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

#ifndef PREFSWINDOW_H
#define PREFSWINDOW_H 1

#include <glib.h>
#include <gtk/gtk.h>

typedef struct _PrefsPage PrefsPage;

typedef void (*PrefsCreateWidgetFunc) (PrefsPage *, GtkWindow *window, gpointer);
typedef void (*PrefsDestroyWidgetFunc) (PrefsPage *);
typedef void (*PrefsSavePageFunc) (PrefsPage *);
typedef gboolean (*PrefsCanClosePageFunc) (PrefsPage *);
typedef void (*PrefsOpenCallbackFunc) (GtkWindow *);
typedef void (*PrefsCloseCallbackFunc) (GtkWindow *);

struct _PrefsPage
{
	gchar 			**path;
	gboolean		  page_open;
	GtkWidget		 *widget;
	gfloat			  weight;

	PrefsCreateWidgetFunc	  create_widget;
	PrefsDestroyWidgetFunc	  destroy_widget;
	PrefsSavePageFunc	  save_page;
	PrefsCanClosePageFunc	  can_close;
};

void prefswindow_open_full		(const gchar *title, 
					 GSList *prefs_pages,
					 gpointer data,
					 GDestroyNotify func,
					 gint *save_width, gint *save_height,
					 gboolean preload_pages,
					 PrefsOpenCallbackFunc open_cb,
					 PrefsCloseCallbackFunc close_cb);

void prefswindow_open			(const gchar *title, 
					 GSList *prefs_pages,
					 gpointer data,
					 gint *save_width, gint *save_height,
					 PrefsOpenCallbackFunc open_cb,
					 PrefsCloseCallbackFunc close_cb);

#endif
