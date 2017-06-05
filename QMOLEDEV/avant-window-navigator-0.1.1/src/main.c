/*
 *  Copyright (C) 2007 Neil Jagdish Patel <njpatel@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA.
 *
 *  Author : Neil Jagdish Patel <njpatel@gmail.com>
*/

#include <gtk/gtk.h>

#include "config.h"

#include "awn-gconf.h"
#include "awn-bar.h"
#include "awn-window.h"
#include "awn-app.h"
#include "awn-win-manager.h"

static gboolean
expose (GtkWidget *widget, GdkEventExpose *event, GtkWindow *window)
{
        gint width, height;
        
        gtk_window_get_size (GTK_WINDOW (widget), &width, &height);
        
        //gtk_window_resize(window, width, height);
        awn_bar_resize(window, width);
        return FALSE;
}

int 
main (int argc, char** argv)
{
	
	AwnSettings* settings = awn_gconf_new();
	GtkWidget *win = NULL;
	GtkWidget *bar = NULL;
	GtkWidget *box = NULL;
	GtkWidget *winman = NULL;
	GtkWidget *lab = NULL;
	
	gtk_init (&argc, &argv);
	
	bar = awn_bar_new(settings);
	
	win = awn_window_new (settings);
	gtk_window_set_policy (GTK_WINDOW (win), FALSE, FALSE, TRUE);
	
	g_signal_connect(G_OBJECT(win), "expose-event",
			 G_CALLBACK(expose), bar);
	
	box = gtk_hbox_new(FALSE, 2);
	
	winman = awn_win_mgr_new(settings);
	
	gtk_box_pack_start(GTK_BOX(box), gtk_label_new(" "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), winman, FALSE, TRUE, 0);	
	gtk_box_pack_start(GTK_BOX(box), gtk_label_new(" "), FALSE, FALSE, 0);

	gtk_container_add(GTK_CONTAINER(win), box);
	
	gtk_widget_show_all(bar);
	gtk_widget_show_all(win);
	gtk_window_set_transient_for(GTK_WINDOW(win), GTK_WINDOW(bar));
	
        
	gtk_main ();
}


