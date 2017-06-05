/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#include <stddef.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "hooks.h"
#include "mainwindow.h"
#include "alertpanel.h"
#include "manage_window.h"
#include "utils.h"
#include "gtkutils.h"
#include "inc.h"
#include "logwindow.h"
#include "prefs_common.h"

#define ALERT_PANEL_WIDTH	380
#define TITLE_HEIGHT		72
#define MESSAGE_HEIGHT		62

static AlertValue value;
static gboolean alertpanel_is_open = FALSE;
static GtkWidget *dialog;

static void alertpanel_show		(void);
static void alertpanel_create		(const gchar	*title,
					 const gchar	*message,
					 const gchar	*button1_label,
					 const gchar	*button2_label,
					 const gchar	*button3_label,
					 gboolean	 can_disable,
					 GtkWidget	*custom_widget,
					 gint		 alert_type,
					 AlertValue	 default_value);

static void alertpanel_button_toggled	(GtkToggleButton	*button,
					 gpointer		 data);
static void alertpanel_button_clicked	(GtkWidget		*widget,
					 gpointer		 data);
static gint alertpanel_deleted		(GtkWidget		*widget,
					 GdkEventAny		*event,
					 gpointer		 data);
static gboolean alertpanel_close	(GtkWidget		*widget,
					 GdkEventAny		*event,
					 gpointer		 data);

AlertValue alertpanel_with_widget(const gchar *title,
				  const gchar *message,
				  const gchar *button1_label,
				  const gchar *button2_label,
				  const gchar *button3_label,
				  gboolean     can_disable,
				  AlertValue   default_value,
				  GtkWidget   *widget)
{
	return alertpanel_full(title, message, button1_label,
				    button2_label, button3_label,
				    can_disable, widget, ALERT_QUESTION,
				    default_value);
}

AlertValue alertpanel_full(const gchar *title, const gchar *message,
			   const gchar *button1_label,
			   const gchar *button2_label,
			   const gchar *button3_label,
			   gboolean     can_disable,
			   GtkWidget   *widget,
			   AlertType    alert_type,
			   AlertValue   default_value)
{
	if (alertpanel_is_open)
		return -1;
	else {
		alertpanel_is_open = TRUE;
		hooks_invoke(ALERTPANEL_OPENED_HOOKLIST, &alertpanel_is_open);
	}
	alertpanel_create(title, message, button1_label, button2_label,
			  button3_label, can_disable, widget, alert_type,
			  default_value);
	alertpanel_show();

	debug_print("return value = %d\n", value);
	return value;
}

AlertValue alertpanel(const gchar *title,
		      const gchar *message,
		      const gchar *button1_label,
		      const gchar *button2_label,
		      const gchar *button3_label)
{
	return alertpanel_full(title, message, button1_label, button2_label,
			       button3_label, FALSE, NULL, ALERT_QUESTION,
			       G_ALERTDEFAULT);
}

static void alertpanel_message(const gchar *title, const gchar *message, gint type)
{
	if (alertpanel_is_open)
		return;
	else {
		alertpanel_is_open = TRUE;
		hooks_invoke(ALERTPANEL_OPENED_HOOKLIST, &alertpanel_is_open);
	}

	alertpanel_create(title, message, GTK_STOCK_CLOSE, NULL, NULL,
			  FALSE, NULL, type, G_ALERTDEFAULT);
	alertpanel_show();
}

void alertpanel_notice(const gchar *format, ...)
{
	va_list args;
	gchar buf[256];

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);
	strretchomp(buf);

	alertpanel_message(_("Notice"), buf, ALERT_NOTICE);
}

void alertpanel_warning(const gchar *format, ...)
{
	va_list args;
	gchar buf[256];

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);
	strretchomp(buf);

	alertpanel_message(_("Warning"), buf, ALERT_WARNING);
}

void alertpanel_error(const gchar *format, ...)
{
	va_list args;
	gchar buf[512];

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);
	strretchomp(buf);

	alertpanel_message(_("Error"), buf, ALERT_ERROR);
}

/*!
 *\brief	display an error with a View Log button
 *
 */
void alertpanel_error_log(const gchar *format, ...)
{
	va_list args;
	int val;
	MainWindow *mainwin;
	gchar buf[256];

	va_start(args, format);
	g_vsnprintf(buf, sizeof(buf), format, args);
	va_end(args);
	strretchomp(buf);

	mainwin = mainwindow_get_mainwindow();
	
	if (mainwin && mainwin->logwin) {
		mainwindow_clear_error(mainwin);
		val = alertpanel_full(_("Error"), buf, GTK_STOCK_CLOSE,
				      _("_View log"), NULL, FALSE, NULL,
				      ALERT_ERROR, G_ALERTDEFAULT);
		if (val == G_ALERTALTERNATE)
			log_window_show(mainwin->logwin);
	} else
		alertpanel_error("%s", buf);
}

static void alertpanel_show(void)
{
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	manage_window_set_transient(GTK_WINDOW(dialog));
	gtk_widget_show_all(dialog);
	value = G_ALERTWAIT;

	if (gdk_pointer_is_grabbed())
		gdk_pointer_ungrab(GDK_CURRENT_TIME);
	inc_lock();
	while ((value & G_ALERT_VALUE_MASK) == G_ALERTWAIT)
		gtk_main_iteration();

	gtk_widget_destroy(dialog);
	GTK_EVENTS_FLUSH();

	alertpanel_is_open = FALSE;
	hooks_invoke(ALERTPANEL_OPENED_HOOKLIST, &alertpanel_is_open);

	inc_unlock();
}

static void alertpanel_create(const gchar *title,
			      const gchar *message,
			      const gchar *button1_label,
			      const gchar *button2_label,
			      const gchar *button3_label,
			      gboolean	   can_disable,
			      GtkWidget   *custom_widget,
			      gint	   alert_type,
			      AlertValue   default_value)
{
	static PangoFontDescription *font_desc;
	GtkWidget *image;
	GtkWidget *label;
	GtkWidget *hbox;
	GtkWidget *vbox;
	GtkWidget *disable_checkbtn;
	GtkWidget *confirm_area;
	GtkWidget *button1;
	GtkWidget *button2;
	GtkWidget *button3;
	const gchar *label2;
	const gchar *label3;
	gchar *tmp = title?g_markup_printf_escaped("%s", title)
			:g_strdup("");
	gchar *title_full = g_strdup_printf("<span weight=\"bold\" "
				"size=\"larger\">%s</span>",
				tmp);
	g_free(tmp);
	debug_print("Creating alert panel dialog...\n");

	dialog = gtk_dialog_new();
	gtk_window_set_title(GTK_WINDOW(dialog), title);
	gtk_window_set_resizable(GTK_WINDOW(dialog), TRUE);

	gtk_window_set_default_size(GTK_WINDOW(dialog), 375, 100);
	
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	g_signal_connect(G_OBJECT(dialog), "delete_event",
			 G_CALLBACK(alertpanel_deleted),
			 (gpointer)G_ALERTCANCEL);
	g_signal_connect(G_OBJECT(dialog), "key_press_event",
			 G_CALLBACK(alertpanel_close),
			 (gpointer)G_ALERTCANCEL);

	/* for title icon, label and message */
	hbox = gtk_hbox_new(FALSE, 12);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 12);
	gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))),
			   hbox, FALSE, FALSE, 0);

	/* title icon */
	switch (alert_type) {
	case ALERT_QUESTION:
		image = gtk_image_new_from_stock
			(GTK_STOCK_DIALOG_QUESTION, GTK_ICON_SIZE_DIALOG);
		break;
	case ALERT_WARNING:
		image = gtk_image_new_from_stock
			(GTK_STOCK_DIALOG_WARNING, GTK_ICON_SIZE_DIALOG);
		break;
	case ALERT_ERROR:
		image = gtk_image_new_from_stock
			(GTK_STOCK_DIALOG_ERROR, GTK_ICON_SIZE_DIALOG);
		break;
	case ALERT_NOTICE:
	default:
		image = gtk_image_new_from_stock
			(GTK_STOCK_DIALOG_INFO, GTK_ICON_SIZE_DIALOG);
		break;
	}
	gtk_misc_set_alignment(GTK_MISC(image), 0.5, 0.0);
	gtk_box_pack_start(GTK_BOX(hbox), image, FALSE, FALSE, 0);

	vbox = gtk_vbox_new (FALSE, 12);
	gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);
	gtk_widget_show (vbox);
	
	label = gtk_label_new(title_full);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_label_set_use_markup(GTK_LABEL (label), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	if (!font_desc) {
		gint size;

		size = pango_font_description_get_size
			(gtk_widget_get_style(label)->font_desc);
		font_desc = pango_font_description_new();
		pango_font_description_set_weight
			(font_desc, PANGO_WEIGHT_BOLD);
		pango_font_description_set_size
			(font_desc, size * PANGO_SCALE_LARGE);
	}
	if (font_desc)
		gtk_widget_modify_font(label, font_desc);
	g_free(title_full);
	
	label = gtk_label_new(message);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	gtkut_widget_set_can_focus(label, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	gtk_widget_show(label);
		
	/* Claws: custom widget */
	if (custom_widget) {
		gtk_box_pack_start(GTK_BOX(vbox), custom_widget, FALSE,
				   FALSE, 0);
	}

	if (can_disable) {
		hbox = gtk_hbox_new(FALSE, 0);
		gtk_box_pack_start(GTK_BOX(
			gtk_dialog_get_content_area(GTK_DIALOG(dialog))), hbox,
				   FALSE, FALSE, 0);

		disable_checkbtn = gtk_check_button_new_with_label
			(_("Show this message next time"));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(disable_checkbtn),
					     TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), disable_checkbtn,
				   FALSE, FALSE, 12);
		g_signal_connect(G_OBJECT(disable_checkbtn), "toggled",
				 G_CALLBACK(alertpanel_button_toggled),
				 GUINT_TO_POINTER(G_ALERTDISABLE));
	}

	/* for button(s) */
	if (!button1_label)
		button1_label = GTK_STOCK_OK;
	label2 = button2_label;
	label3 = button3_label;
	if (label2 && *label2 == '+') label2++;
	if (label3 && *label3 == '+') label3++;

	gtkut_stock_button_set_create(&confirm_area,
				      &button1, button1_label,
				      button2_label ? &button2 : NULL, label2,
				      button3_label ? &button3 : NULL, label3);

	gtk_box_pack_end(GTK_BOX(gtk_dialog_get_action_area(GTK_DIALOG(dialog))),
			 confirm_area, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(confirm_area), 5);
	gtk_widget_grab_default(button1);
	gtk_widget_grab_focus(button1);
	if (button2_label &&
	    (default_value == G_ALERTALTERNATE || *button2_label == '+')) {
		gtk_widget_grab_default(button2);
		gtk_widget_grab_focus(button2);
	}
	if (button3_label &&
	    (default_value == G_ALERTOTHER || *button3_label == '+')) {
		gtk_widget_grab_default(button3);
		gtk_widget_grab_focus(button3);
	}

	g_signal_connect(G_OBJECT(button1), "clicked",
			 G_CALLBACK(alertpanel_button_clicked),
			 GUINT_TO_POINTER(G_ALERTDEFAULT));
	if (button2_label)
		g_signal_connect(G_OBJECT(button2), "clicked",
				 G_CALLBACK(alertpanel_button_clicked),
				 GUINT_TO_POINTER(G_ALERTALTERNATE));
	if (button3_label)
		g_signal_connect(G_OBJECT(button3), "clicked",
				 G_CALLBACK(alertpanel_button_clicked),
				 GUINT_TO_POINTER(G_ALERTOTHER));

	gtk_widget_show_all(dialog);
}

static void alertpanel_button_toggled(GtkToggleButton *button,
				      gpointer data)
{
	if (gtk_toggle_button_get_active(button))
		value &= ~GPOINTER_TO_UINT(data);
	else
		value |= GPOINTER_TO_UINT(data);
}

static void alertpanel_button_clicked(GtkWidget *widget, gpointer data)
{
	value = (value & ~G_ALERT_VALUE_MASK) | (AlertValue)data;
}

static gint alertpanel_deleted(GtkWidget *widget, GdkEventAny *event,
			       gpointer data)
{
	value = (value & ~G_ALERT_VALUE_MASK) | (AlertValue)data;
	return TRUE;
}

static gboolean alertpanel_close(GtkWidget *widget, GdkEventAny *event,
				 gpointer data)
{
	if (event->type == GDK_KEY_PRESS)
		if (((GdkEventKey *)event)->keyval != GDK_KEY_Escape)
			return FALSE;

	value = (value & ~G_ALERT_VALUE_MASK) | (AlertValue)data;
	return FALSE;
}
