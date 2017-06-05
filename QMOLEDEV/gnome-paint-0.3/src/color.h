/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- */
/*
 * color.h
 * Copyright (C) Rogério Ferro do Nascimento 2009 <rogerioferro@gmail.com>
 * 
 * color.h is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * color.h is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <gtk/gtk.h>

void foreground_set_color			( GdkColor *color );
void foreground_set_color_from_rgb  ( guint color );

/* GUI CallBack */
void on_color_palette_entry_realize     (GtkWidget *widget, gpointer user_data);
void on_background_color_picker_realize	(GtkWidget *widget, gpointer user_data);
void on_foreground_color_picker_realize	(GtkWidget *widget, gpointer user_data);


/* GUI Events*/
gboolean on_background_color_picker_button_release_event	(   GtkWidget	   *widget, 
																GdkEventButton *event,
																gpointer       user_data );
gboolean on_foreground_color_picker_button_release_event	(   GtkWidget	   *widget, 
																GdkEventButton *event,
																gpointer       user_data );
gboolean on_color_palette_entry_button_press_event			(   GtkWidget	   *widget, 
																GdkEventButton *event,
			                                                   gpointer       user_data );

