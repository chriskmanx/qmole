/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 Hiroyuki Yamamoto & The Claws Mail Team
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

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "gtkutils.h"
#include "colorsel.h"
#include "manage_window.h"

static void quote_colors_set_dialog_ok(GtkWidget *widget, gpointer data)
{
	*((gint *) data) = 0;
	gtk_main_quit();
}

static void quote_colors_set_dialog_cancel(GtkWidget *widget, gpointer data)
{
	*((gint *) data) = 1;
	gtk_main_quit();
}

static gboolean quote_colors_set_dialog_key_pressed(GtkWidget *widget,
						GdkEventKey *event,
						gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape) {
		*((gint *) data) = 1;
		gtk_main_quit();
		return TRUE;
	} else if (event && event->keyval == GDK_KEY_Return) {
		*((gint *) data) = 0;
		gtk_main_quit();
		return FALSE;
	}
	return FALSE;
}

gint colorsel_select_color_rgb(gchar *title, gint rgbvalue)
{
	GdkColor color;
	GtkColorSelectionDialog *color_dialog;
	GtkWidget *ok_button, *cancel_button;
	gint result;

	color_dialog = GTK_COLOR_SELECTION_DIALOG(gtk_color_selection_dialog_new(title));
	gtk_window_set_modal(GTK_WINDOW(color_dialog), TRUE);
	gtk_window_set_resizable(GTK_WINDOW(color_dialog), FALSE);
	manage_window_set_transient(GTK_WINDOW(color_dialog));

	g_object_get(color_dialog,
		"ok-button", &ok_button,
		"cancel-button", &cancel_button,
		NULL);

	g_signal_connect(G_OBJECT(ok_button),
			 "clicked", 
			 G_CALLBACK(quote_colors_set_dialog_ok), &result);
	g_signal_connect(G_OBJECT(cancel_button),
			 "clicked", 
			 G_CALLBACK(quote_colors_set_dialog_cancel), &result);
	g_signal_connect(G_OBJECT(color_dialog), "key_press_event",
			 G_CALLBACK(quote_colors_set_dialog_key_pressed), &result);

	/* preselect the previous color in the color selection dialog */
	gtkut_convert_int_to_gdk_color(rgbvalue, &color);
	gtk_color_selection_set_current_color(GTK_COLOR_SELECTION(
		gtk_color_selection_dialog_get_color_selection(color_dialog)), &color);

	gtk_widget_show(GTK_WIDGET(color_dialog));
	gtk_main();

	if (result == 0) {
		gtk_color_selection_get_current_color(GTK_COLOR_SELECTION(
			gtk_color_selection_dialog_get_color_selection(color_dialog)), &color);
		rgbvalue = gtkut_convert_gdk_color_to_int(&color);
	}

	gtk_widget_destroy(GTK_WIDGET(color_dialog));

	return rgbvalue;
}

