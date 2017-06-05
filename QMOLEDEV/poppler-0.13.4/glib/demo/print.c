/*
 * Copyright (C) 2009 Carlos Garcia Campos  <carlosgc@gnome.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "config.h"

#include <gtk/gtk.h>
#include "print.h"

typedef struct {
	PopplerDocument *doc;
} PgdPrintDemo;

static void
pgd_print_free (PgdPrintDemo *demo)
{
	if (!demo)
		return;

	if (demo->doc) {
		g_object_unref (demo->doc);
		demo->doc = NULL;
	}

	g_free (demo);
}

static void
pgd_print_begin_print (GtkPrintOperation *op,
		       GtkPrintContext   *context,
		       PgdPrintDemo      *demo)
{
	gtk_print_operation_set_n_pages (op, poppler_document_get_n_pages (demo->doc));
}

static void
pgd_print_draw_page (GtkPrintOperation *op,
		     GtkPrintContext   *context,
		     gint               page_nr,
		     PgdPrintDemo      *demo)
{
	PopplerPage *page;
	cairo_t     *cr;

	page = poppler_document_get_page (demo->doc, page_nr);
	if (!page)
		return;

	cr = gtk_print_context_get_cairo_context (context);
	poppler_page_render_for_printing (page, cr);
	g_object_unref (page);
}

static void
pgd_print_print (GtkWidget    *button,
		 PgdPrintDemo *demo)
{
	GtkPrintOperation *op;
	GError            *error = NULL;

	op = gtk_print_operation_new ();
	g_signal_connect (op, "begin-print",
			  G_CALLBACK (pgd_print_begin_print),
			  demo);
	g_signal_connect (op, "draw-page",
			  G_CALLBACK (pgd_print_draw_page),
			  demo);
	gtk_print_operation_run (op,
				 GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
				 GTK_WINDOW (gtk_widget_get_toplevel (button)),
				 &error);
	if (error) {
		GtkWidget *dialog;

		dialog = gtk_message_dialog_new (GTK_WINDOW (gtk_widget_get_toplevel (button)),
						 GTK_DIALOG_DESTROY_WITH_PARENT,
						 GTK_MESSAGE_ERROR,
						 GTK_BUTTONS_CLOSE,
						 "%s", error->message);
		g_error_free (error);

		g_signal_connect (dialog, "response",
				  G_CALLBACK (gtk_widget_destroy), NULL);

		gtk_widget_show (dialog);
	}
	g_object_unref (op);
}

GtkWidget *
pgd_print_create_widget (PopplerDocument *document)
{
	PgdPrintDemo *demo;
	GtkWidget    *vbox;
	GtkWidget    *hbox;
	GtkWidget    *button;

	demo = g_new0 (PgdPrintDemo, 1);

	demo->doc = g_object_ref (document);

	vbox = gtk_vbox_new (FALSE, 12);

	hbox = gtk_hbox_new (FALSE, 6);

	button = gtk_button_new_with_label ("Print...");
	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (pgd_print_print),
			  (gpointer)demo);
	gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, FALSE, 0);
	gtk_widget_show (button);

	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
	gtk_widget_show (hbox);

	g_object_weak_ref (G_OBJECT (vbox),
			   (GWeakNotify)pgd_print_free,
			   (gpointer)demo);

	return vbox;
}
