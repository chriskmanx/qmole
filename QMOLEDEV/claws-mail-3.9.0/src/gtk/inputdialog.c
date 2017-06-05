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

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "inputdialog.h"
#include "manage_window.h"
#include "gtkutils.h"
#include "utils.h"
#include "combobox.h"
#include "prefs_common.h"


#define INPUT_DIALOG_WIDTH	420

typedef enum
{
	INPUT_DIALOG_NORMAL,
	INPUT_DIALOG_INVISIBLE,
	INPUT_DIALOG_COMBO
} InputDialogType;

static gboolean ack;
static gboolean fin;

static InputDialogType type;

static GtkWidget *dialog;
static GtkWidget *msg_title;
static GtkWidget *msg_label;
static GtkWidget *entry;
static GtkWidget *combo;
static GtkWidget *remember_checkbtn;
static GtkWidget *ok_button;
static GtkWidget *icon_q, *icon_p;
static gboolean is_pass = FALSE;
static void input_dialog_create	(gboolean is_password);
static gchar *input_dialog_open	(const gchar	*title,
				 const gchar	*message,
				 const gchar  *checkbtn_label,
				 const gchar	*default_string,
				 gboolean default_checkbtn_state,
				 gboolean	*remember);
static void input_dialog_set	(const gchar	*title,
				 const gchar	*message,
				 const gchar	*default_string);

static void ok_clicked		(GtkWidget	*widget,
				 gpointer	 data);
static void cancel_clicked	(GtkWidget	*widget,
				 gpointer	 data);
static gint delete_event	(GtkWidget	*widget,
				 GdkEventAny	*event,
				 gpointer	 data);
static gboolean key_pressed	(GtkWidget	*widget,
				 GdkEventKey	*event,
				 gpointer	 data);
static void entry_activated	(GtkEditable	*editable);
static void combo_activated	(GtkEditable	*editable);


gchar *input_dialog(const gchar *title, const gchar *message,
		    const gchar *default_string)
{
	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		input_dialog_create(FALSE);

	type = INPUT_DIALOG_NORMAL;
	gtk_widget_hide(combo);
	gtk_widget_show(entry);

	gtk_widget_hide(remember_checkbtn);

	gtk_widget_show(icon_q);
	gtk_widget_hide(icon_p);
	is_pass = FALSE;
	gtk_entry_set_visibility(GTK_ENTRY(entry), TRUE);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(entry), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_AUTOCAP);
#endif

	return input_dialog_open(title, message, NULL, default_string, FALSE, NULL);
}

gchar *input_dialog_with_invisible(const gchar *title, const gchar *message,
				   const gchar *default_string)
{
	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		input_dialog_create(TRUE);

	type = INPUT_DIALOG_INVISIBLE;
	gtk_widget_hide(combo);
	gtk_widget_show(entry);
	gtk_widget_hide(remember_checkbtn);

	gtk_widget_hide(icon_q);
	gtk_widget_show(icon_p);
	is_pass = TRUE;
	gtk_entry_set_visibility(GTK_ENTRY(entry), FALSE);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(entry), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_INVISIBLE);
#endif

	return input_dialog_open(title, message, NULL, default_string, FALSE, NULL);
}

gchar *input_dialog_with_invisible_checkbtn(const gchar *title, const gchar *message,
				   const gchar *default_string, const gchar *checkbtn_label,
				   gboolean *checkbtn_state)
{
	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		input_dialog_create(TRUE);

	type = INPUT_DIALOG_INVISIBLE;
	gtk_widget_hide(combo);
	gtk_widget_show(entry);

	if (checkbtn_label && checkbtn_state) {
		gtk_widget_show(remember_checkbtn);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(remember_checkbtn), *checkbtn_state);
	}
	else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(remember_checkbtn), FALSE);
		gtk_widget_hide(remember_checkbtn);
	}

	gtk_widget_hide(icon_q);
	gtk_widget_show(icon_p);
	is_pass = TRUE;
	gtk_entry_set_visibility(GTK_ENTRY(entry), FALSE);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(entry), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_INVISIBLE);
#endif

	return input_dialog_open(title, message, checkbtn_label, default_string, (checkbtn_state? *checkbtn_state:FALSE), checkbtn_state);
}

gchar *input_dialog_combo(const gchar *title, const gchar *message,
			  const gchar *default_string, GList *list)
{
	return input_dialog_combo_remember(title, message, 
		default_string, list, FALSE);
}

gchar *input_dialog_combo_remember(const gchar *title, const gchar *message,
			  const gchar *default_string, GList *list,
			  gboolean *remember)
{
	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		input_dialog_create(FALSE);

	type = INPUT_DIALOG_COMBO;
	gtk_widget_hide(entry);
	gtk_widget_show(combo);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(remember_checkbtn), FALSE);
	if (remember)
		gtk_widget_show(remember_checkbtn);
	else
		gtk_widget_hide(remember_checkbtn);

	gtk_widget_show(icon_q);
	gtk_widget_hide(icon_p);
	is_pass = FALSE;

	combobox_unset_popdown_strings(GTK_COMBO_BOX(combo));
	combobox_set_popdown_strings(GTK_COMBO_BOX(combo), list);

	return input_dialog_open(title, message, NULL, default_string, FALSE, remember);
}

gchar *input_dialog_with_checkbtn(const gchar	*title,
				   const gchar	*message,
				   const gchar	*default_string,
				   const gchar  *checkbtn_label,
				   gboolean *checkbtn_state)
{
	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		input_dialog_create(FALSE);

	type = INPUT_DIALOG_NORMAL;
	gtk_widget_hide(combo);
	gtk_widget_show(entry);

	if (checkbtn_label && checkbtn_state)
		gtk_widget_show(remember_checkbtn);
	else
		gtk_widget_hide(remember_checkbtn);

	gtk_widget_show(icon_q);
	gtk_widget_hide(icon_p);
	is_pass = FALSE;
	gtk_entry_set_visibility(GTK_ENTRY(entry), TRUE);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(entry), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_AUTOCAP);
#endif

	return input_dialog_open(title, message, checkbtn_label, default_string, 
	       			 prefs_common.inherit_folder_props, checkbtn_state);
}

gchar *input_dialog_query_password(const gchar *server, const gchar *user)
{
	gchar *message;
	gchar *pass;

	if (server && user)
		message = g_strdup_printf(_("Input password for %s on %s:"),
				  user, server);
	else if (server)
		message = g_strdup_printf(_("Input password for %s:"),
				  server);
	else if (user)
		message = g_strdup_printf(_("Input password for %s:"),
				  user);
	else
		message = g_strdup_printf(_("Input password:"));
	pass = input_dialog_with_invisible(_("Input password"), message, NULL);
	g_free(message);

	return pass;
}

gchar *input_dialog_query_password_keep(const gchar *server, const gchar *user, gchar **keep)
{
	gchar *message;
	gchar *pass;

	if (server && user)
		message = g_strdup_printf(_("Input password for %s on %s:"),
				  user, server);
	else if (server)
		message = g_strdup_printf(_("Input password for %s:"),
				  server);
	else if (user)
		message = g_strdup_printf(_("Input password for %s:"),
				  user);
	else
		message = g_strdup_printf(_("Input password:"));
        if (keep) {
		if (*keep != NULL) {
			pass = g_strdup (*keep);
		}
		else {
			gboolean state = prefs_common.session_passwords;
			pass = input_dialog_with_invisible_checkbtn(_("Input password"), 
					message, NULL,
					_("Remember password for this session"), 
					&state);
			if (state) {
				*keep = g_strdup (pass);
				debug_print("keeping session password for account\n");
			}
			prefs_common.session_passwords = state;
		}
	}
	else {
		pass = input_dialog_with_invisible(_("Input password"), message, NULL);
	}		
	g_free(message);

	return pass;
}

static void input_dialog_create(gboolean is_password)
{
	static PangoFontDescription *font_desc;
	GtkWidget *hbox;
	GtkWidget *vbox;
	GtkWidget *cancel_button;
	GtkWidget *confirm_area;

	dialog = gtk_dialog_new();

	gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE);
	gtk_window_set_default_size(GTK_WINDOW(dialog), 375, 100);
	gtk_window_set_title(GTK_WINDOW(dialog), "");

	g_signal_connect(G_OBJECT(dialog), "delete_event",
			 G_CALLBACK(delete_event), NULL);
	g_signal_connect(G_OBJECT(dialog), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(dialog);

	vbox = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
	gtk_box_set_spacing (GTK_BOX (vbox), 14);
	hbox = gtk_hbox_new (FALSE, 12);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox), hbox,
			    FALSE, FALSE, 0);

	/* for title label */
	icon_q = gtk_image_new_from_stock(GTK_STOCK_DIALOG_QUESTION,
        			GTK_ICON_SIZE_DIALOG); 
	gtk_misc_set_alignment (GTK_MISC (icon_q), 0.5, 0.0);
	gtk_box_pack_start (GTK_BOX (hbox), icon_q, FALSE, FALSE, 0);
	icon_p = gtk_image_new_from_stock(GTK_STOCK_DIALOG_AUTHENTICATION,
        			GTK_ICON_SIZE_DIALOG); 
	gtk_misc_set_alignment (GTK_MISC (icon_p), 0.5, 0.0);
	gtk_box_pack_start (GTK_BOX (hbox), icon_p, FALSE, FALSE, 0);
	
	vbox = gtk_vbox_new (FALSE, 12);
	gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);
	gtk_widget_show (vbox);
	
	msg_title = gtk_label_new("");
	gtk_misc_set_alignment(GTK_MISC(msg_title), 0, 0.5);
	gtk_label_set_justify(GTK_LABEL(msg_title), GTK_JUSTIFY_LEFT);
	gtk_label_set_use_markup (GTK_LABEL (msg_title), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), msg_title, FALSE, FALSE, 0);
	gtk_label_set_line_wrap(GTK_LABEL(msg_title), TRUE);
	if (!font_desc) {
		gint size;

		size = pango_font_description_get_size
			(gtk_widget_get_style(msg_title)->font_desc);
		font_desc = pango_font_description_new();
		pango_font_description_set_weight
			(font_desc, PANGO_WEIGHT_BOLD);
		pango_font_description_set_size
			(font_desc, size * PANGO_SCALE_LARGE);
	}
	if (font_desc)
		gtk_widget_modify_font(msg_title, font_desc);
	
	msg_label = gtk_label_new("");
	gtk_misc_set_alignment(GTK_MISC(msg_label), 0, 0.5);
	gtk_label_set_justify(GTK_LABEL(msg_label), GTK_JUSTIFY_LEFT);
	gtk_box_pack_start(GTK_BOX(vbox), msg_label, FALSE, FALSE, 0);
	gtk_widget_show(msg_label);
		
	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(vbox), entry, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(entry), "activate",
			 G_CALLBACK(entry_activated), NULL);

	combo = gtk_combo_box_entry_new_text();
	gtk_box_pack_start(GTK_BOX(vbox), combo, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((combo)))), "activate",
			 G_CALLBACK(combo_activated), NULL);

	remember_checkbtn = gtk_check_button_new_with_label(_("Remember this"));
	gtk_box_pack_start(GTK_BOX(vbox), remember_checkbtn, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(TRUE, 0);

	gtkut_stock_button_set_create(&confirm_area,
				      &cancel_button, GTK_STOCK_CANCEL,
				      &ok_button, GTK_STOCK_OK,
				      NULL, NULL);

	gtk_box_pack_end(GTK_BOX(gtk_dialog_get_action_area(GTK_DIALOG(dialog))),
			 confirm_area, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(confirm_area), 5);

	gtk_widget_show_all(gtk_dialog_get_content_area(GTK_DIALOG(dialog)));
	
	gtk_widget_hide(remember_checkbtn);

	if (is_password)
		gtk_widget_hide(icon_q);
	else
		gtk_widget_hide(icon_p);

	is_pass = is_password;

	gtk_widget_grab_default(ok_button);

	g_signal_connect(G_OBJECT(ok_button), "clicked",
			 G_CALLBACK(ok_clicked), NULL);
	g_signal_connect(G_OBJECT(cancel_button), "clicked",
			 G_CALLBACK(cancel_clicked), NULL);
}

static gchar *input_dialog_open(const gchar *title, const gchar *message,
				const gchar *checkbtn_label,
				const gchar *default_string,
				gboolean default_checkbtn_state,
				gboolean *remember)
{
	gchar *str;

	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		input_dialog_create(FALSE);

	if (checkbtn_label)
		gtk_button_set_label(GTK_BUTTON(remember_checkbtn), checkbtn_label);
	else
		gtk_button_set_label(GTK_BUTTON(remember_checkbtn), _("Remember this"));

	input_dialog_set(title, message, default_string);
	gtk_widget_show(dialog);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(remember_checkbtn),
				     default_checkbtn_state);
	if (remember)
		gtk_widget_show(remember_checkbtn);
	else
		gtk_widget_hide(remember_checkbtn);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	manage_window_set_transient(GTK_WINDOW(dialog));

	ack = fin = FALSE;

	while (fin == FALSE)
		gtk_main_iteration();

	manage_window_focus_out(dialog, NULL, NULL);
	gtk_widget_hide(dialog);

	if (ack) {
		GtkEditable *editable;

		if (type == INPUT_DIALOG_COMBO)
			editable = GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((combo))));
		else
			editable = GTK_EDITABLE(entry);

		str = gtk_editable_get_chars(editable, 0, -1);
		if (str && *str == '\0' && !is_pass) {
			g_free(str);
			str = NULL;
		}
	} else
		str = NULL;

	GTK_EVENTS_FLUSH();

	if (remember) {
		*remember = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(remember_checkbtn));
	}
	if (is_pass)
		debug_print("return string = %s\n", str ? "********": ("none"));
	else
		debug_print("return string = %s\n", str ? str : "(none)");
	return str;
}

static void input_dialog_set(const gchar *title, const gchar *message,
			     const gchar *default_string)
{
	GtkWidget *entry_;

	if (type == INPUT_DIALOG_COMBO)
		entry_ = gtk_bin_get_child(GTK_BIN((combo)));
	else
		entry_ = entry;

	gtk_window_set_title(GTK_WINDOW(dialog), title);
	gtk_label_set_text(GTK_LABEL(msg_title), title);
	gtk_label_set_text(GTK_LABEL(msg_label), message);
	if (default_string && *default_string) {
		gtk_entry_set_text(GTK_ENTRY(entry_), default_string);
		gtk_editable_set_position(GTK_EDITABLE(entry_), 0);
		gtk_editable_select_region(GTK_EDITABLE(entry_), 0, -1);
	} else
		gtk_entry_set_text(GTK_ENTRY(entry_), "");

	gtk_widget_grab_focus(ok_button);
	gtk_widget_grab_focus(entry_);
}

static void ok_clicked(GtkWidget *widget, gpointer data)
{
	ack = TRUE;
	fin = TRUE;
}

static void cancel_clicked(GtkWidget *widget, gpointer data)
{
	ack = FALSE;
	fin = TRUE;
}

static gint delete_event(GtkWidget *widget, GdkEventAny *event, gpointer data)
{
	ack = FALSE;
	fin = TRUE;

	return TRUE;
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape) {
		ack = FALSE;
		fin = TRUE;
	} else if (event && event->keyval == GDK_KEY_Return) {
		ack = TRUE;
		fin = TRUE;
		return TRUE; /* do not let Return pass - it
			      * pops up the combo on validating */
	}

	return FALSE;
}

static void entry_activated(GtkEditable *editable)
{
	ack = TRUE;
	fin = TRUE;
}

static void combo_activated(GtkEditable *editable)
{
	ack = TRUE;
	fin = TRUE;
}
