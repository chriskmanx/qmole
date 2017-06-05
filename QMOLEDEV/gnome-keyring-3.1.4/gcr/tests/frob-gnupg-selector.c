/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "gcr/gcr.h"
#include "gcr/gcr-gnupg-collection.h"
#include "gcr/gcr-gnupg-key.h"
#include "gcr/gcr-list-selector-private.h"
#include "gcr/gcr-live-search.h"

#include <gtk/gtk.h>

#include <unistd.h>
#include <string.h>
#include <errno.h>

static void
on_collection_loaded (GObject *source, GAsyncResult *result, gpointer unused)
{
	GError *error = NULL;

	_gcr_gnupg_collection_load_finish (GCR_GNUPG_COLLECTION (source), result, &error);
	if (error) {
		g_warning ("collection load failed: %s", error->message);
		g_clear_error (&error);
	}
}

int
main (int argc, char *argv[])
{
	GcrCollection *collection;
	GcrListSelector *selector;
	GtkWidget *scroll;
	GtkDialog *dialog;
	GtkWidget *search;
	GtkBox *box;

	gtk_init (&argc, &argv);

	dialog = GTK_DIALOG (gtk_dialog_new ());
	g_object_ref_sink (dialog);

	collection = _gcr_gnupg_collection_new (NULL);
	selector = gcr_list_selector_new (collection);
	search = _gcr_live_search_new (GTK_WIDGET (selector));
	_gcr_list_selector_set_live_search (selector, GCR_LIVE_SEARCH (search));

	scroll = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scroll), GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add (GTK_CONTAINER (scroll), GTK_WIDGET (selector));

	box = GTK_BOX (gtk_dialog_get_content_area (dialog));
	gtk_box_pack_start (box, GTK_WIDGET (scroll), TRUE, TRUE, 0);
	gtk_box_pack_end (box, search, FALSE, TRUE, 0);

	gtk_widget_show (GTK_WIDGET (selector));
	gtk_widget_show (scroll);

	_gcr_gnupg_collection_load_async (GCR_GNUPG_COLLECTION (collection), NULL,
	                                  on_collection_loaded, NULL);

	gtk_window_set_default_size (GTK_WINDOW (dialog), 550, 400);
	gtk_container_set_border_width (GTK_CONTAINER (dialog), 20);

	g_object_unref (collection);

	gtk_dialog_run (dialog);
	gtk_widget_destroy (GTK_WIDGET (dialog));
	g_object_unref (dialog);

	return 0;
}
