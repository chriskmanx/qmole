/*
 * This file Copyright (C) 2005-2012 Paul Mangan <paul@claws-mail.org>
 * and the Claws Mail team
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
#include <gdk/gdkkeysyms.h>

#include "gtkutils.h"
#include "stock_pixmap.h"
#include "prefs_gtk.h"

#define ICONS 21

StockPixmap legend_icons[ICONS] = {
	STOCK_PIXMAP_NEW,
	STOCK_PIXMAP_UNREAD,
	STOCK_PIXMAP_REPLIED, 
	STOCK_PIXMAP_FORWARDED, 
	STOCK_PIXMAP_REPLIED_AND_FORWARDED, 
	STOCK_PIXMAP_IGNORETHREAD,
	STOCK_PIXMAP_WATCHTHREAD,
	STOCK_PIXMAP_SPAM,
	STOCK_PIXMAP_CLIP,
	STOCK_PIXMAP_GPG_SIGNED,
	STOCK_PIXMAP_KEY,
	STOCK_PIXMAP_CLIP_GPG_SIGNED,
	STOCK_PIXMAP_CLIP_KEY,
	STOCK_PIXMAP_MARK,
	STOCK_PIXMAP_DELETED,
	STOCK_PIXMAP_MOVED,
	STOCK_PIXMAP_COPIED,
	STOCK_PIXMAP_LOCKED,
	STOCK_PIXMAP_DIR_OPEN, 
	STOCK_PIXMAP_DIR_OPEN_HRM,
	STOCK_PIXMAP_DIR_OPEN_MARK,
};

static gchar *legend_icon_desc[] = {
/* status column */
	N_("New message"),
	N_("Unread message"),
	N_("Message has been replied to"),
	N_("Message has been forwarded"),
	N_("Message has been forwarded and replied to"),
	N_("Message is in an ignored thread"),
	N_("Message is in a watched thread"),
	N_("Message is spam"),
/* attachment column */
	N_("Message has attachment(s)"),
	N_("Digitally signed message"),
	N_("Encrypted message"),
	N_("Message is signed and has attachment(s)"),
	N_("Message is encrypted and has attachment(s)"),
/* mark column */
	N_("Marked message"),
	N_("Message is marked for deletion"),
	N_("Message is marked for moving"),
	N_("Message is marked for copying"),
/* locked column */
	N_("Locked message"),
/* others */
	N_("Folder (normal, opened)"),
	N_("Folder with read messages hidden"),
	N_("Folder contains marked messages"),
};

static struct LegendDialog {
	GtkWidget *window;
} legend;

static void legend_create(void);
static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event);
static void legend_close(void);

void legend_show(void)
{
	if (!legend.window)
		legend_create();
	else
		gtk_window_present(GTK_WINDOW(legend.window));
}

static void legend_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *confirm_area;
	GtkWidget *close_button;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *icon_label;
	GtkWidget *desc_label;
	GtkWidget *scrolled_window;
	GtkWidget *table;
	gint i;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "icon_legend");
	gtk_window_set_title(GTK_WINDOW(window), _("Icon Legend"));
	gtk_container_set_border_width(GTK_CONTAINER(window), 8);
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
	gtk_widget_set_size_request(window, 360, 570);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(legend_close), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	gtk_widget_realize(window);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(window), vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 2);

	hbox = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	label = gtk_label_new(_("<span weight=\"bold\">The following icons "
				"are used to show the status of messages and "
				"folders:</span>"));
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_widget_show(label);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);

	table = gtk_table_new(ICONS, 2, FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(table), 8);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	for (i = 0; i < ICONS; ++i) {
		icon_label = stock_pixmap_widget(window, legend_icons[i]);
		gtk_misc_set_alignment (GTK_MISC (icon_label), 0.5, 0.5);
		gtk_table_attach(GTK_TABLE(table), icon_label, 0, 1, i, i+1,
				GTK_FILL, 0, 0, 0);

		desc_label = gtk_label_new(gettext(legend_icon_desc[i]));
		gtk_misc_set_alignment (GTK_MISC (desc_label), 0, 0.5);
		gtk_label_set_line_wrap(GTK_LABEL(desc_label), TRUE);
		gtk_table_attach(GTK_TABLE(table), desc_label, 1, 2, i, i+1,
				GTK_FILL, 0, 0, 0);
	}	

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      table);

	gtkut_stock_button_set_create(&confirm_area, &close_button, GTK_STOCK_CLOSE,
				      NULL, NULL, NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), confirm_area, FALSE, FALSE, 4);
	gtk_widget_grab_default(close_button);
	g_signal_connect_closure
		(G_OBJECT(close_button), "clicked",
		 g_cclosure_new_swap(G_CALLBACK(legend_close),
				     window, NULL), FALSE);

	gtk_widget_show_all(window);

	legend.window = window;
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event)
{
	if (event && event->keyval == GDK_KEY_Escape) {
		legend_close();
	}
	return FALSE;
}

static void legend_close(void)
{
	gtk_widget_destroy(legend.window);
	legend.window = NULL;
}
