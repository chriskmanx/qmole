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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "prefs_common.h"
#include "prefs_gtk.h"

#include "gtk/prefswindow.h"

typedef struct _FontsPage
{
	PrefsPage page;

	GtkWidget *window;		/* do not modify */

	GtkWidget *entry_folderview_smallfont;
	GtkWidget *entry_folderview_normalfont;
	GtkWidget *entry_folderview_boldfont;
	GtkWidget *entry_messageviewfont;
	GtkWidget *derive_from_normalfont_checkbutton;
	GtkWidget *print_checkbutton;
	GtkWidget *entry_messageprintfont;
} FontsPage;

static void prefs_fonts_create_widget(PrefsPage *_page, GtkWindow *window, 
			       gpointer data)
{
	FontsPage *prefs_fonts = (FontsPage *) _page;

	GtkWidget *table;
	GtkWidget *entry_folderview_smallfont;
	GtkWidget *entry_folderview_normalfont;
	GtkWidget *entry_folderview_boldfont;
	GtkWidget *entry_messageviewfont;
	GtkWidget *tmplabel;
	GtkWidget *entry_messageprintfont;
	GtkWidget *print_checkbutton;
	GtkWidget *derive_from_normalfont_checkbutton;
	GtkWidget *vbox;
	gint      row = 0;

	table = gtk_table_new(10, 2, FALSE);
	gtk_widget_show(table);
	gtk_container_set_border_width(GTK_CONTAINER(table), VBOX_BORDER);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	/* normal font label */
	tmplabel = gtk_label_new (_("Folder and Message Lists"));
	gtk_widget_show (tmplabel);
	gtk_table_attach (GTK_TABLE (table), tmplabel, 0, 1, row, row+1,
			 (GtkAttachOptions) GTK_FILL,
			 (GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(tmplabel), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(tmplabel), 1, 0.5);

	/* normal font button */
	entry_folderview_normalfont = gtk_font_button_new_with_font (prefs_common.normalfont);
	g_object_set(G_OBJECT(entry_folderview_normalfont), 
			      "use-font", TRUE, 
			      NULL);
	gtk_widget_show (entry_folderview_normalfont);
	gtk_table_attach (GTK_TABLE (table), entry_folderview_normalfont, 1, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	row++;

	/* message font label */
	tmplabel = gtk_label_new (_("Message"));
	gtk_widget_show (tmplabel);
	gtk_table_attach (GTK_TABLE (table), tmplabel, 0, 1, row, row+1,
			 (GtkAttachOptions) GTK_FILL,
			 (GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(tmplabel), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(tmplabel), 1, 0.5);

	/* message font button */
	entry_messageviewfont = gtk_font_button_new_with_font (prefs_common.textfont);
	g_object_set(G_OBJECT(entry_messageviewfont), 
			      "use-font", TRUE, 
			      NULL);
	gtk_widget_show (entry_messageviewfont);
	gtk_table_attach (GTK_TABLE (table), entry_messageviewfont, 1, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	row++;

	vbox = gtk_vbox_new(FALSE, VSPACING_NARROW);
	gtk_widget_show(vbox);
	gtk_table_attach (GTK_TABLE (table), vbox, 0, 4, row, row+1,
			 (GtkAttachOptions) GTK_FILL,
			 (GtkAttachOptions) (0), 0, 0);
	row++;
	
	/* derive from normal font check button */
	derive_from_normalfont_checkbutton = gtk_check_button_new_with_label(_("Derive small and bold fonts from Folder and Message Lists font"));
	gtk_widget_show(derive_from_normalfont_checkbutton);
	gtk_table_attach (GTK_TABLE (table), derive_from_normalfont_checkbutton, 0, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(derive_from_normalfont_checkbutton),
		 prefs_common.derive_from_normal_font);
	row++;

	/* small font label */
	tmplabel = gtk_label_new (_("Small"));
	gtk_widget_show (tmplabel);
	gtk_table_attach (GTK_TABLE (table), tmplabel, 0, 1, row, row+1,
			 (GtkAttachOptions) GTK_FILL,
			 (GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(tmplabel), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(tmplabel), 1, 0.5);
	SET_TOGGLE_SENSITIVITY_REVERSE (derive_from_normalfont_checkbutton, tmplabel);

	/* small font button */
	entry_folderview_smallfont = gtk_font_button_new_with_font (prefs_common.smallfont);
	g_object_set(G_OBJECT(entry_folderview_smallfont), 
			      "use-font", TRUE, 
			      NULL);
	gtk_widget_show (entry_folderview_smallfont);
	gtk_table_attach (GTK_TABLE (table), entry_folderview_smallfont, 1, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	SET_TOGGLE_SENSITIVITY_REVERSE (derive_from_normalfont_checkbutton, entry_folderview_smallfont);
	row++;

	/* bold font label */
	tmplabel = gtk_label_new (_("Bold"));
	gtk_widget_show (tmplabel);
	gtk_table_attach (GTK_TABLE (table), tmplabel, 0, 1, row, row+1,
			 (GtkAttachOptions) GTK_FILL,
			 (GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(tmplabel), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(tmplabel), 1, 0.5);
	SET_TOGGLE_SENSITIVITY_REVERSE (derive_from_normalfont_checkbutton, tmplabel);

	/* bold font button */
	entry_folderview_boldfont = gtk_font_button_new_with_font (prefs_common.boldfont);
	g_object_set(G_OBJECT(entry_folderview_boldfont), 
			      "use-font", TRUE, 
			      NULL);
	gtk_widget_show (entry_folderview_boldfont);
	gtk_table_attach (GTK_TABLE (table), entry_folderview_boldfont, 1, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	SET_TOGGLE_SENSITIVITY_REVERSE (derive_from_normalfont_checkbutton, entry_folderview_boldfont);
	row++;

	/* print check button */
	print_checkbutton = gtk_check_button_new_with_label(_("Use different font for printing"));
	gtk_widget_show(print_checkbutton);
	gtk_table_attach (GTK_TABLE (table), print_checkbutton, 0, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(print_checkbutton),
		 prefs_common.use_different_print_font);
	row++;

	/* print font label */
	tmplabel = gtk_label_new (_("Message Printing"));
	gtk_widget_show (tmplabel);
	gtk_table_attach (GTK_TABLE (table), tmplabel, 0, 1, row, row+1,
			 (GtkAttachOptions) GTK_FILL,
			 (GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(tmplabel), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(tmplabel), 1, 0.5);
	SET_TOGGLE_SENSITIVITY (print_checkbutton, tmplabel);

	/* print font button */
	entry_messageprintfont = gtk_font_button_new_with_font (prefs_common.printfont);
	g_object_set(G_OBJECT(entry_messageprintfont), 
			      "use-font", TRUE, 
			      NULL);
	gtk_widget_show (entry_messageprintfont);
	gtk_table_attach (GTK_TABLE (table), entry_messageprintfont, 1, 2, row, row+1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	SET_TOGGLE_SENSITIVITY (print_checkbutton, entry_messageprintfont);
	row++;

	prefs_fonts->window			= GTK_WIDGET(window);
	prefs_fonts->entry_folderview_smallfont = entry_folderview_smallfont;
	prefs_fonts->entry_folderview_normalfont = entry_folderview_normalfont;
	prefs_fonts->entry_folderview_boldfont   = entry_folderview_boldfont;
	prefs_fonts->entry_messageviewfont	= entry_messageviewfont;
	prefs_fonts->derive_from_normalfont_checkbutton = derive_from_normalfont_checkbutton;
	prefs_fonts->entry_messageprintfont	= entry_messageprintfont;
	prefs_fonts->print_checkbutton	   	= print_checkbutton;

	prefs_fonts->page.widget = table;
}

static void prefs_fonts_save(PrefsPage *_page)
{
	FontsPage *fonts = (FontsPage *) _page;

	g_free(prefs_common.boldfont);
	prefs_common.boldfont = g_strdup(gtk_font_button_get_font_name
		(GTK_FONT_BUTTON(fonts->entry_folderview_boldfont)));

	g_free(prefs_common.normalfont);
	prefs_common.normalfont = g_strdup(gtk_font_button_get_font_name
		(GTK_FONT_BUTTON(fonts->entry_folderview_normalfont)));
		
	g_free(prefs_common.smallfont);		
	prefs_common.smallfont  = g_strdup(gtk_font_button_get_font_name
		(GTK_FONT_BUTTON(fonts->entry_folderview_smallfont)));

	g_free(prefs_common.textfont);		
	prefs_common.textfont   = g_strdup(gtk_font_button_get_font_name
		(GTK_FONT_BUTTON(fonts->entry_messageviewfont)));

	prefs_common.derive_from_normal_font = gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON(fonts->derive_from_normalfont_checkbutton));

	g_free(prefs_common.printfont);		
	prefs_common.printfont   = g_strdup(gtk_font_button_get_font_name
		(GTK_FONT_BUTTON(fonts->entry_messageprintfont)));
	prefs_common.use_different_print_font = gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON(fonts->print_checkbutton));

	main_window_reflect_prefs_all();
}

static void prefs_fonts_destroy_widget(PrefsPage *_page)
{
	/* FontsPage *fonts = (FontsPage *) _page; */

}

FontsPage *prefs_fonts;

void prefs_fonts_init(void)
{
	FontsPage *page;
	static gchar *path[3];

	path[0] = _("Display");
	path[1] = _("Fonts");
	path[2] = NULL;

	page = g_new0(FontsPage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_fonts_create_widget;
	page->page.destroy_widget = prefs_fonts_destroy_widget;
	page->page.save_page = prefs_fonts_save;
	page->page.weight = 135.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_fonts = page;
}

void prefs_fonts_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_fonts);
	g_free(prefs_fonts);
}
