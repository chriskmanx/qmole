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

#ifndef __MANAGE_WINDOW_H__
#define __MANAGE_WINDOW_H__

#include <glib.h>
#include <gtk/gtk.h>

#define MANAGE_WINDOW_SIGNALS_CONNECT(window) \
{ \
	g_signal_connect(G_OBJECT(window), "focus_in_event", \
			 G_CALLBACK(manage_window_focus_in), NULL); \
	g_signal_connect(G_OBJECT(window), "focus_out_event", \
			 G_CALLBACK(manage_window_focus_out), NULL); \
	g_signal_connect(G_OBJECT(window), "unmap_event", \
			 G_CALLBACK(manage_window_unmap), NULL); \
	g_signal_connect(G_OBJECT(window), "destroy", \
			 G_CALLBACK(manage_window_destroy), NULL); \
}

gint manage_window_focus_in		(GtkWidget	*widget,
					 GdkEventFocus	*event,
					 gpointer	 data);
gint manage_window_focus_out		(GtkWidget	*widget,
					 GdkEventFocus	*event,
					 gpointer	 data);
gint manage_window_unmap		(GtkWidget	*widget,
					 GdkEventAny	*event,
					 gpointer	 data);
void manage_window_destroy		(GtkWidget	*widget,
					 gpointer	 data);

void manage_window_set_transient	(GtkWindow	*window);

extern GtkWidget *focus_window;

#endif /* __MANAGE_WINDOW_H__ */
