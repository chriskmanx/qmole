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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "common/utils.h"
#include "prefs.h"
#include "prefs_gtk.h"
#include "prefswindow.h"

#include "prefs_common.h"
#include "prefs_gtk.h"

typedef struct _ImageViewerPage
{
	PrefsPage page;
	
	GtkWidget *window;		/* do not modify */

	GtkWidget *autoload_img;
	GtkWidget *resize_img;
	GtkWidget *inline_img;
	GtkWidget *print_imgs;
}ImageViewerPage;

static void imageviewer_create_widget_func(PrefsPage * _page,
					   GtkWindow * window,
					   gpointer data)
{
	ImageViewerPage *prefs_imageviewer = (ImageViewerPage *) _page;

	GtkWidget *table;
	GtkWidget *autoload_img;
	GtkWidget *resize_img;
	GtkWidget *inline_img;
	GtkWidget *print_imgs;

	table = gtk_table_new(4, 1, FALSE);
	gtk_widget_show(table);
	gtk_container_set_border_width(GTK_CONTAINER(table), VBOX_BORDER);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	autoload_img = gtk_check_button_new_with_label(_("Automatically display attached images"));
	gtk_widget_show(autoload_img);
	gtk_table_attach(GTK_TABLE(table), autoload_img, 0, 1, 0, 1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);

	resize_img = gtk_check_button_new_with_label(_("Resize attached images by default"));
	gtk_widget_show(resize_img);
	CLAWS_SET_TIP(resize_img,
			     _("Clicking image toggles scaling"));
	gtk_table_attach(GTK_TABLE(table), resize_img, 0, 1, 1, 2,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);

	inline_img = gtk_check_button_new_with_label(_("Display images inline"));
	gtk_widget_show(inline_img);
	gtk_table_attach(GTK_TABLE(table), inline_img, 0, 1, 2, 3,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	
	print_imgs = gtk_check_button_new_with_label(_("Print images"));
	gtk_widget_show(print_imgs);
	gtk_table_attach(GTK_TABLE(table), print_imgs, 0, 1, 3, 4,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(resize_img), prefs_common.resize_img);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(autoload_img), prefs_common.display_img);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(inline_img), prefs_common.inline_img);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(print_imgs), prefs_common.print_imgs);

	prefs_imageviewer->window	= GTK_WIDGET(window);
	prefs_imageviewer->autoload_img = autoload_img;
	prefs_imageviewer->resize_img 	= resize_img;
	prefs_imageviewer->inline_img 	= inline_img;
	prefs_imageviewer->print_imgs 	= print_imgs;

	prefs_imageviewer->page.widget = table;
}

static void imageviewer_destroy_widget_func(PrefsPage *_page)
{
}

static void imageviewer_save_func(PrefsPage * _page)
{
	ImageViewerPage *imageviewer = (ImageViewerPage *) _page;
	
	prefs_common.display_img =
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
					 (imageviewer->autoload_img));
	prefs_common.resize_img =
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
	    				 (imageviewer->resize_img));
	prefs_common.inline_img =
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
	    				 (imageviewer->inline_img));
	prefs_common.print_imgs =
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
	    				 (imageviewer->print_imgs));
}

ImageViewerPage *prefs_imageviewer;

void prefs_image_viewer_init(void)
{
	ImageViewerPage *page;
	static gchar *path[3];

	path[0] = _("Message View");
	path[1] = _("Image Viewer");
	path[2] = NULL;

	page = g_new0(ImageViewerPage, 1);
	page->page.path = path;
	page->page.create_widget = imageviewer_create_widget_func;
	page->page.destroy_widget = imageviewer_destroy_widget_func;
	page->page.save_page = imageviewer_save_func;
	page->page.weight = 160.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_imageviewer = page;
}

void prefs_image_viewer_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_imageviewer);
	g_free(prefs_imageviewer);
}
