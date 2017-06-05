/* 
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 Hiroyuki Yamamoto & The Claws Mail Team
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
#include <stdio.h>
#include <string.h>
#include <time.h>

#if HAVE_LIBCOMPFACE
#  include <compface.h>
#endif

#include "prefs_common.h"
#include "gtkutils.h"
#include "utils.h"
#include "stock_pixmap.h"

#include "noticeview.h"

static void noticeview_button_pressed	(GtkButton *button, NoticeView *noticeview);
static void noticeview_2ndbutton_pressed(GtkButton *button, NoticeView *noticeview);
static gboolean noticeview_icon_pressed	(GtkWidget *widget, GdkEventButton *evt,
					 NoticeView *noticeview);
static gboolean noticeview_visi_notify(GtkWidget *widget,
				       GdkEventVisibility *event,
				       NoticeView *noticeview);
static gboolean noticeview_leave_notify(GtkWidget *widget,
				      GdkEventCrossing *event,
				      NoticeView *textview);
static gboolean noticeview_enter_notify(GtkWidget *widget,
				      GdkEventCrossing *event,
				      NoticeView *textview);

static GdkCursor *hand_cursor = NULL;

NoticeView *noticeview_create(MainWindow *mainwin)
{
	NoticeView *noticeview;
	GtkWidget  *vbox;
	GtkWidget  *hsep;
	GtkWidget  *hbox;
	GtkWidget  *icon;
	GtkWidget  *text;
	GtkWidget  *widget;
	GtkWidget  *widget2;
	GtkWidget  *evtbox;

	debug_print("Creating notice view...\n");
	noticeview = g_new0(NoticeView, 1);

	if (!hand_cursor)
		hand_cursor = gdk_cursor_new(GDK_HAND2);

	noticeview->window = mainwin->window;
	
	vbox = gtk_vbox_new(FALSE, 4);
	gtk_widget_show(vbox);
	hsep = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(vbox), hsep, FALSE, TRUE, 1);
	
	hbox = gtk_hbox_new(FALSE, 4);
	gtk_widget_show(hbox);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, TRUE, 1);

	evtbox = gtk_event_box_new();
	gtk_event_box_set_visible_window(GTK_EVENT_BOX(evtbox), FALSE);
	gtk_widget_show(evtbox);

	icon = stock_pixmap_widget(noticeview->window, STOCK_PIXMAP_NOTICE_WARN); 

	gtk_widget_show(icon);
	g_signal_connect(G_OBJECT(evtbox), "button-press-event", 
			 G_CALLBACK(noticeview_icon_pressed),
			 (gpointer) noticeview);
	g_signal_connect(G_OBJECT(evtbox), "motion-notify-event",
			 G_CALLBACK(noticeview_visi_notify), noticeview);
	g_signal_connect(G_OBJECT(evtbox), "leave-notify-event",
			 G_CALLBACK(noticeview_leave_notify), noticeview);
	g_signal_connect(G_OBJECT(evtbox), "enter-notify-event",
			 G_CALLBACK(noticeview_enter_notify), noticeview);
	
	gtk_container_add(GTK_CONTAINER(evtbox), icon);
	gtk_box_pack_start(GTK_BOX(hbox), evtbox, FALSE, TRUE, 0);
	
	text = gtk_label_new("");
	gtk_widget_show(text);
	gtk_box_pack_start(GTK_BOX(hbox), text, FALSE, FALSE, 0);

	widget = gtk_button_new_with_label("");
	g_signal_connect(G_OBJECT(widget), "clicked", 
			 G_CALLBACK(noticeview_button_pressed),
			 (gpointer) noticeview);
	gtk_box_pack_start(GTK_BOX(hbox), widget, FALSE, FALSE, 4);
	
	widget2 = gtk_button_new_with_label("");
	g_signal_connect(G_OBJECT(widget2), "clicked", 
			 G_CALLBACK(noticeview_2ndbutton_pressed),
			 (gpointer) noticeview);
	gtk_box_pack_start(GTK_BOX(hbox), widget2, FALSE, FALSE, 0);
	
	noticeview->vbox   = vbox;
	noticeview->hsep   = hsep;
	noticeview->hbox   = hbox;
	noticeview->icon   = icon;
	noticeview->text   = text;
	noticeview->button = widget;
	noticeview->button2= widget2;
	noticeview->evtbox = evtbox;
	noticeview->visible= TRUE;
#if !(GTK_CHECK_VERSION(2,12,0))
	noticeview->tooltips = tips;
#endif
	return noticeview;
}

void noticeview_destroy(NoticeView *noticeview)
{
	g_free(noticeview);
}

gboolean noticeview_is_visible(NoticeView *noticeview)
{
	return noticeview->visible;
}

void noticeview_show(NoticeView *noticeview)
{
	if (!noticeview->visible) {
		gtk_widget_show(GTK_WIDGET_PTR(noticeview));
		noticeview->visible = TRUE;
	}	
}

void noticeview_hide(NoticeView *noticeview)
{
	if (noticeview && noticeview->visible) {
		gtk_widget_hide(GTK_WIDGET_PTR(noticeview));
		noticeview->visible = FALSE;
	}	
}

void noticeview_set_text(NoticeView *noticeview, const char *text)
{
	cm_return_if_fail(noticeview);
	gtk_label_set_text(GTK_LABEL(noticeview->text), text);
}

void noticeview_set_button_text(NoticeView *noticeview, const char *text)
{
	cm_return_if_fail(noticeview);

	if (text != NULL) {
		gtk_label_set_text
			(GTK_LABEL(gtk_bin_get_child(GTK_BIN((noticeview->button)))), text);
		gtk_widget_show(noticeview->button);
	} else
		gtk_widget_hide(noticeview->button);
	
	/* Callers defining only one button don't have to mind 
	 * resetting the second one. Callers defining two have
	 * to define the second button after the first one. 
	 */
	gtk_widget_hide(noticeview->button2);
}

void noticeview_set_button_press_callback(NoticeView	*noticeview,
					  void 		(*callback)(void),
					  gpointer	*user_data)
{
	noticeview->press     = (void (*) (NoticeView *, gpointer)) callback;
	noticeview->user_data = user_data;
}

static void noticeview_button_pressed(GtkButton *button, NoticeView *noticeview)
{
	if (noticeview->press) {
		noticeview->press(noticeview, noticeview->user_data);
	}
}

static gboolean noticeview_icon_pressed(GtkWidget *widget, GdkEventButton *evt,
				    NoticeView *noticeview)
{
	if (evt && evt->button == 1 && noticeview->icon_clickable) {
		noticeview_button_pressed(NULL, noticeview);
	}
	return FALSE;
}

static gboolean noticeview_visi_notify(GtkWidget *widget,
				       GdkEventVisibility *event,
				       NoticeView *noticeview)
{
	if (noticeview->icon_clickable)
		gdk_window_set_cursor(gtk_widget_get_window(noticeview->evtbox), hand_cursor);
	return FALSE;
}

static gboolean noticeview_leave_notify(GtkWidget *widget,
				      GdkEventCrossing *event,
				      NoticeView *noticeview)
{
	gdk_window_set_cursor(gtk_widget_get_window(noticeview->evtbox), NULL);
	return FALSE;
}

static gboolean noticeview_enter_notify(GtkWidget *widget,
				      GdkEventCrossing *event,
				      NoticeView *noticeview)
{
	if (noticeview->icon_clickable)
		gdk_window_set_cursor(gtk_widget_get_window(noticeview->evtbox), hand_cursor);
	return FALSE;
}

void noticeview_set_2ndbutton_text(NoticeView *noticeview, const char *text)
{
	cm_return_if_fail(noticeview);

	if (text != NULL) {
		gtk_label_set_text
			(GTK_LABEL(gtk_bin_get_child(GTK_BIN((noticeview->button2)))), text);
		gtk_widget_show(noticeview->button2);
	} else
		gtk_widget_hide(noticeview->button2);
}

void noticeview_set_2ndbutton_press_callback(NoticeView	*noticeview,
					  void 		(*callback)(void),
					  gpointer	*user_data)
{
	noticeview->press2     = (void (*) (NoticeView *, gpointer)) callback;
	noticeview->user_data2 = user_data;
}

static void noticeview_2ndbutton_pressed(GtkButton *button, NoticeView *noticeview)
{
	if (noticeview->press2) {
		noticeview->press2(noticeview, noticeview->user_data2);
	}
}

void noticeview_set_icon(NoticeView *noticeview, StockPixmap icon)
{
	GdkPixbuf *pixbuf;
	
	if (stock_pixbuf_gdk(noticeview->window, icon, &pixbuf) < 0)
		return;
	
	gtk_image_set_from_pixbuf(GTK_IMAGE(noticeview->icon), pixbuf);
}

void noticeview_set_icon_clickable(NoticeView *noticeview, gboolean setting)
{
	noticeview->icon_clickable = setting;
}		

void noticeview_set_tooltip (NoticeView *noticeview, const gchar *text)
{
#if !(GTK_CHECK_VERSION(2,12,0))
	GtkTooltips *tips = noticeview->tooltips;
#endif
	CLAWS_SET_TIP(noticeview->evtbox,
			text);

}
