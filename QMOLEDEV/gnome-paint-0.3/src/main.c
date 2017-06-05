/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- */
/*
 * main.c
 * Copyright (C) Rogério Ferro do Nascimento 2009 <rogerioferro@gmail.com>
 * 
 * main.c is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * main.c is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "common.h"
#include "color.h"
#include "toolbar.h"
#include "cv_drawing.h"
#include "file.h"
#include "undo.h"
#include "color-picker.h"


#include <glib/gi18n.h>
#include <gtk/gtk.h>

#define UI_FILE		PACKAGE_DATA_DIR G_DIR_SEPARATOR_S "gnome-paint" G_DIR_SEPARATOR_S "ui" G_DIR_SEPARATOR_S "gnome_paint.ui"
#define ICON_DIR	PACKAGE_DATA_DIR G_DIR_SEPARATOR_S "gnome-paint" G_DIR_SEPARATOR_S "icons"


GtkWidget	*create_window			( void );
void		gnome_paint_init		( int argc, char *argv[] );
void		on_window_destroy		( GtkObject *object, gpointer user_data );
void		on_menu_about_activate  ( GtkMenuItem *menuitem, gpointer user_data );



void		
on_menu_new_activate( GtkMenuItem *menuitem, gpointer user_data)
{
	g_spawn_command_line_async (g_get_prgname(), NULL);
}

void main_color_changed	(ColorPicker *color_picker, gpointer user_data)
{
	GdkColor *color;
	color = color_picker_get_color (color_picker);
	foreground_set_color ( color );
}

void color_picker_released (ColorPicker *color_picker, gpointer user_data)
{
	toolbar_go_to_previous_tool ();
}


int
main (int argc, char *argv[])
{

 	GtkWidget   *window;
	ColorPicker *color_picker;

//	g_mem_set_vtable (glib_mem_profiler_table);

	bindtextdomain (GETTEXT_PACKAGE, PACKAGE_LOCALE_DIR);
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
	textdomain (GETTEXT_PACKAGE);
	
	gtk_set_locale ();
	gtk_init (&argc, &argv);

	/* Add application specific icons to search path */
	gtk_icon_theme_append_search_path (gtk_icon_theme_get_default (), ICON_DIR);
	gtk_window_set_default_icon_name ("gp");
	window = create_window ();
	gnome_paint_init (argc, argv);
	gtk_widget_show (window);

	color_picker = color_picker_new ();
	toolbar_set_color_picker ( color_picker );
	g_signal_connect (color_picker, "color-changed",
		            G_CALLBACK (main_color_changed), NULL);

	g_signal_connect (color_picker, "released",
		            G_CALLBACK (color_picker_released), NULL);
	
	gtk_main ();

//	g_mem_profile ();
	
	return 0;	
}


void
gnome_paint_init	( int argc, char *argv[] )
{
	if (argc > 1)
	{
		if( argc > 2 )
		{   /*open others images*/
			gchar   *new_argv[argc];
			gint	i,n;
			n = argc - 1;
			new_argv[0] = argv[0];
			new_argv[n] = NULL;
			for (i=1; i < n ; i++)
			{
				new_argv[i] = argv[i+1];
			}
			g_spawn_async_with_pipes ( NULL,
				                  	   new_argv,
			                           NULL,
			                           G_SPAWN_SEARCH_PATH,
			                           NULL,NULL,NULL,NULL,NULL,NULL,NULL);
		}
		/*open the first image*/
		file_open (argv[1]);
	}
}

gboolean
on_window_delete_event (GtkWidget       *widget,
                        GdkEvent        *event,
                        gpointer         user_data)
{
	return file_save_dialog ();
}

void 
on_window_destroy ( GtkObject   *object, 
				    gpointer	user_data )
{
	gtk_main_quit();
}

GtkWidget*
create_window (void)
{
	GtkWidget		*window;
	GtkWidget		*widget;
	GtkBuilder		*builder;

	builder = gtk_builder_new ();
    gtk_builder_add_from_file (builder, UI_FILE, NULL);
    window = GTK_WIDGET (gtk_builder_get_object (builder, "window"));
	g_assert ( window );
	file_set_parent_window ( GTK_WINDOW(window) );	
    gtk_builder_connect_signals (builder, NULL);          
    g_object_unref (G_OBJECT (builder));	
	
	/* To show all widget that is set invisible on Glade */
	/* and call realize event */
	gtk_widget_show_all(window);

	return window;
}

void 
on_menu_about_activate ( GtkMenuItem *menuitem, gpointer user_data )
{
	const gchar *authors[] = { "Rogério Ferro do Nascimento",
							   "Juan Balderas",
							   NULL };
	GtkAboutDialog *dlg;
	
	dlg = GTK_ABOUT_DIALOG ( gtk_about_dialog_new () ); 
	gtk_about_dialog_set_name ( dlg, "gnome-paint" );
	gtk_about_dialog_set_version ( dlg, "0.3"); 
	gtk_about_dialog_set_copyright ( dlg, 
									"(c) Rogério Ferro do Nascimento");
	gtk_about_dialog_set_comments ( dlg, 
									_("gnome-paint is a simple, easy to use paint program for GNOME.") );
	gtk_about_dialog_set_license ( dlg, 
								_( "This program is free software;"
								   " you may redistribute it and/or modify it"
								   " under the terms of the GNU General Public License"
								   " as published by the Free Software Foundation, "
								   "either version 3, or (at your opinion) any later version.\n") );
	gtk_about_dialog_set_wrap_license ( dlg, TRUE );
	gtk_about_dialog_set_authors ( dlg, authors );
	gtk_about_dialog_set_website ( dlg, 
									"http://code.google.com/p/gnome-paint/");
	//gtk_about_dialog_set_logo ( dlg, pixbuf);
	gtk_dialog_run ( dlg );
	gtk_widget_destroy (dlg);
}



