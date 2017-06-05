/* $Id: e2_password_dialog.c 2864 2013-10-27 09:04:44Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/dialogs/e2_password_dialog.c
@brief functions to setup and cleanup password-related widgets for a dialog
*/

#include "e2_password_dialog.h"
#include "e2_button.h"
#include "e2_icons.h"

//#include <pthread.h>
#define E2_HINT_MSEC 600

//static assuming last-closed window sets size for next one in this session only
//static gint window_width = -1;
#ifdef USE_GTK2_10
static guint pwrefcount = 0;
static guint hinttime;
static gboolean hinted = FALSE;		//TRUE when the last char in either used entry is displayed as plaintext
#endif
static gboolean plaintext = FALSE;	//TRUE when all entered chars are echoed as is
static gboolean hidden = FALSE;		//TRUE when no entered char is to be echoed

/**
@brief toggle temporary display of last-entered character in the password
This is a "key-press-event" signal callback for both password-entry entry widgets
Toggles parameters in response to <Ctrl>p, <Ctrl>h, and for gtk >= 2.10, <Ctrl>t
@param entry pointer to the widget that received the keypress
@param event event data struct
@param rt pointer to data struct for the passwords process

@return TRUE if the key was one of the recognised ones
*/
static gboolean _e2_pwdlg_key_press_cb (GtkWidget *entry, GdkEventKey *event,
	E2_PWDataRuntime *rt)
{
	if (event->state & GDK_CONTROL_MASK)
	{
		NEEDCLOSEBGL
		if (event->keyval == GDK_p || event->keyval == GDK_P)
		{
			rt->plain = !rt->plain;
			gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry1), rt->plain);
			gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry2), rt->plain);
			return TRUE;
		}
		else if (event->keyval == GDK_h || event->keyval == GDK_H)
		{
			if (rt->plain)
			{
				rt->plain = FALSE;
				gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry1), FALSE);
				gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry2), FALSE);
			}
			rt->hide = !rt->hide;
			gunichar ch = (rt->hide) ? (gunichar) 0 : (gunichar) '*';
			gtk_entry_set_invisible_char (GTK_ENTRY (rt->pwentry1), ch);
			gtk_entry_set_invisible_char (GTK_ENTRY (rt->pwentry2), ch);
			return TRUE;
		}
#ifdef USE_GTK2_10
		else if (event->keyval == GDK_t || event->keyval == GDK_T)
		{
			if (rt->plain)
			{
				rt->plain = FALSE;
				gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry1), FALSE);
				gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry2), FALSE);
			}
			rt->hint = !rt->hint;
			guint msec = (rt->hint) ? E2_HINT_MSEC : 0;
			GtkSettings *s = gtk_settings_get_default ();
			g_object_set (G_OBJECT (s), "gtk-entry-password-hint-timeout", msec, NULL);
			return TRUE;
		}
#endif
		NEEDOPENBGL
	}
	return FALSE;
}
/**
@brief change entry focus or activate the parent dialog
This is an "activate" signal callback for both password-entry entry widgets
@param entry the widget that received the keypress
@param rt pointer to data struct for the passwords process

@return
*/
static void _e2_pwdlg_activate_cb (GtkEntry *entry, E2_PWDataRuntime *rt)
{
	//prevent activation of any newly-focused widget too
	g_signal_stop_emission_by_name ((gpointer)entry, "activate");

	NEEDCLOSEBGL
	if (entry == GTK_ENTRY (rt->pwentry1))
	{
		if (rt->confirm &&
#ifdef USE_GTK2_18
			gtk_widget_get_visible (rt->pwentry2))
#else
			GTK_WIDGET_VISIBLE (rt->pwentry2))
#endif
			gtk_widget_grab_focus (rt->pwentry2);
		else
		{
			if (rt->passwd != NULL)
			{
				if (*rt->passwd != NULL)
					g_free (*rt->passwd);
				*rt->passwd = gtk_editable_get_chars (GTK_EDITABLE (entry), 0, -1);
			}
			goto process;
		}
	}
	else //entry == GTK_ENTRY(rt->pwentry2)
		if (e2_password_dialog_confirm (rt))
	{
process:
		if (rt->dialog != NULL)
		{
			NEEDOPENBGL
			//assumes that E2_BUTTON_YES or some derivative is in the dialog
			g_signal_emit_by_name (G_OBJECT (rt->dialog), "response",
				GTK_RESPONSE_YES);
			NEEDCLOSEBGL
		}
		else if (rt->focus != NULL)
			gtk_widget_grab_focus (rt->focus);
	}
	NEEDOPENBGL
}
/**
@brief during entry destruction, zap plaintext password in gtk's buffer
@param entry the widget being destroyed/hidden
@param user_data UNUSED pointer to data specified when cb was connected
@return
*/
static void _e2_pwdlg_clear_entry (GtkWidget *entry, gpointer user_data)
{
	//ensure that gtk's internal cleanup will work as intended
	gtk_entry_set_visibility (GTK_ENTRY (entry), FALSE);
	//gtk will zap the buffer holding the text
	gtk_editable_delete_text (GTK_EDITABLE (entry), 0, -1);
}
/**
@brief setup password-related widgets to get user input with obfuscated text display

One or two sets of centred labels and entries are consecutively added to the end of @a box.
The respective entry(ies)' contents are zapped when the widgets are destroyed.
Probably best if BGL is closed upon arrival, to avoid entry-keypress latency.
The returned struct is used as callback data, and so must persist while the
widget(s) remain in use. Member(s) of the returned data struct (dialog, focus)
might need to be set by the calling code, especially to enable focusing and/or
response processing.

@a pw may hold a pointer to a suggested value.
@param box the widget into which things will be packed
@param confirm TRUE to get 2 matched copies of entered password
@param mainprompt custom prompt string, or NULL for default
@param pw store for password pointer, or NULL

@return pointer to allocated data struct, NULL upon error
*/
E2_PWDataRuntime *e2_password_dialog_setup (GtkWidget *box, gboolean confirm,
	const gchar *mainprompt, gchar **pw)
{
	E2_PWDataRuntime *rt = ALLOCATE0 (E2_PWDataRuntime);
	CHECKALLOCATEDWARN (rt, return NULL;)

	const gchar *realprompt = (mainprompt != NULL) ? mainprompt : _("Enter password:");
	gchar *realpw = (pw != NULL && *pw != NULL && **pw != '\0') ? *pw : NULL;

/*	if (tabular)
	{
		rt->table =
		e2_widget_add_table (box, (confirm)? 1:2, 2, FALSE, TRUE, E2_PADDING_SMALL);
#ifdef E2_ASSISTED
		GtkWidget *label =
#endif
		e2_widget_add_mid_label_to_table (rt->table, realprompt, 0.0, 0, 1, 0, 1);
		rt->pwentry1 = e2_widget_add_entry_to_table (rt->table, realpw, 1, 2, 0, 1);
#ifdef E2_ASSISTED
		e2_widget_set_label_relations (label, rt->pwentry1);
#endif
	}
	else
	{
		rt->table = NULL;
*/
#ifdef E2_ASSISTED
		GtkWidget *label;
#endif
		if (*realprompt != '\0')
#ifdef E2_ASSISTED
			label =
#endif
			e2_widget_add_label (box, realprompt, 0.5, 0.0, FALSE, E2_PADDING);
#ifdef E2_ASSISTED
		else
			label = NULL;
#endif
		rt->pwentry1 = e2_widget_add_entry (box, realpw, TRUE, (realpw != NULL));
#ifdef E2_ASSISTED
		if (label != NULL)
			e2_widget_set_label_relations (label, rt->pwentry1);
#endif
//	}
	rt->plain = plaintext;
#ifdef USE_GTK2_10
	rt->hint = hinted;
#endif
	rt->hide = hidden;
	if (!rt->plain)
	{
		gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry1), FALSE);
		if (rt->hide)
			gtk_entry_set_invisible_char (GTK_ENTRY (rt->pwentry1), 0);
#ifdef USE_GTK2_10
		//setup to handle password hinting
		GtkSettings *defs = gtk_settings_get_default ();
		//log original value
		guint msec;
		g_object_get (G_OBJECT (defs), "gtk-entry-password-hint-timeout", &msec, NULL);
		if (++pwrefcount == 1)
			hinttime = msec;
		msec =  (rt->hint) ? E2_HINT_MSEC : 0;
		g_object_set (G_OBJECT (defs), "gtk-entry-password-hint-timeout", msec, NULL);
#endif
	}
	//handle hint-key or hide-key presses
	g_signal_connect (G_OBJECT (rt->pwentry1), "key-press-event",
		G_CALLBACK (_e2_pwdlg_key_press_cb), rt);
	//handle Return-key presses when the entry is focused
	g_signal_connect (G_OBJECT (rt->pwentry1), "activate",
		G_CALLBACK (_e2_pwdlg_activate_cb), rt);
	//cleanup password buffer when widget is hidden/destroyed
	g_signal_connect (G_OBJECT (rt->pwentry1), "unrealize",
		G_CALLBACK (_e2_pwdlg_clear_entry), NULL);

	rt->confirm = confirm;
	if (confirm)
	{
/*		if (tabular)
		{
#ifdef E2_ASSISTED
			label =
#endif
			e2_widget_add_mid_label_to_table (rt->table, _("Confirm password:"), 0.0, 0, 1, 1, 2);
			rt->pwentry2 = e2_widget_add_entry_to_table (rt->table, NULL, 1, 2, 1, 2);
#ifdef E2_ASSISTED
			e2_widget_set_label_relations (label, rt->pwentry2);
#endif
		}
		else
		{
*/
#ifdef E2_ASSISTED
			label =
#endif
			e2_widget_add_label (box, _("Confirm password:"), 0.5, 0.0, FALSE, E2_PADDING);
			rt->pwentry2 = e2_widget_add_entry (box, NULL, TRUE, FALSE);
#ifdef E2_ASSISTED
			e2_widget_set_label_relations (label, rt->pwentry2);
#endif
//		}
		if (!rt->plain)
		{
			gtk_entry_set_visibility (GTK_ENTRY (rt->pwentry2), FALSE);
			if (rt->hide)
				gtk_entry_set_invisible_char (GTK_ENTRY (rt->pwentry2), 0);
		}
		//handle hint-key or hide-key presses
		g_signal_connect (G_OBJECT (rt->pwentry2), "key-press-event",
			G_CALLBACK (_e2_pwdlg_key_press_cb), rt);
		//handle Return-key presses when the entry is focused
		g_signal_connect (G_OBJECT (rt->pwentry2), "activate",
			G_CALLBACK (_e2_pwdlg_activate_cb), rt);
		//cleanup password buffer when widget is hidden/destroyed
		g_signal_connect (G_OBJECT (rt->pwentry2), "unrealize",
			G_CALLBACK (_e2_pwdlg_clear_entry), NULL);
	}
	rt->passwd = pw;

	return rt;
}
/**
@brief check whether entered password(s) are ok to use, and log it if so
There is no "editorial" about the merit of the password(s) - anything non-empty
will be accepted
Assumes BGL is closed
@param rt pointer to password(s) data

@return TRUE if the password(s) are ok
*/
gboolean e2_password_dialog_confirm (E2_PWDataRuntime *rt)
{
	const gchar *pw1;
	pw1 = gtk_entry_get_text (GTK_ENTRY (rt->pwentry1));
	if (*pw1 == '\0' && !rt->confirm)
	{
		gtk_widget_grab_focus (rt->pwentry1);
		return FALSE;
	}
	else	//initial pw not empty or is empty but confirm needed
		if (rt->confirm)
	{
		const gchar *pw2;
		pw2 = gtk_entry_get_text (GTK_ENTRY (rt->pwentry2));

		if (*pw1 != '\0' && *pw2 == '\0')
		{
			gtk_widget_grab_focus (rt->pwentry2);
			return FALSE;
		}
		else if (*pw2 != '\0' && *pw1 == '\0')
		{
			gtk_widget_grab_focus (rt->pwentry1);
			return FALSE;
		}
		else if (strcmp (pw1, pw2))
		{
			gtk_widget_grab_focus (rt->pwentry2);

			GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_ERROR,
			_("Entered passwords are different"), NULL, DUMMY_RESPONSE_CB, NULL);
			GtkWidget *parent = gtk_widget_get_toplevel (rt->pwentry1);
			if (parent == NULL)
				parent = app.main_window;
			//block until user responds
			e2_dialog_show (dialog, parent, E2_DIALOG_BLOCKED | E2_DIALOG_FREE,
				&E2_BUTTON_CLOSE, NULL);

			return FALSE;
		}
		else if (*pw1 == '\0') //pw1 and pw2 both empty
		{
			gtk_widget_grab_focus (rt->pwentry1);
			return FALSE;
		}
	}

	if (rt->passwd != NULL)
	{
		if (*rt->passwd != NULL)
			g_free (*rt->passwd);
		*rt->passwd = g_strdup (pw1);
	}

	return TRUE;
}
/**
@brief backup session-static settings
@param rt pointer to password data struct
@return
*/
void e2_password_dialog_backup (E2_PWDataRuntime *rt)
{
	plaintext = rt->plain;
	hidden = rt->hide;
#ifdef USE_GTK2_10
	if (--pwrefcount == 0)
	{
		GtkSettings *defs = gtk_settings_get_default ();
		g_object_set (G_OBJECT (defs), "gtk-entry-password-hint-timeout",
			hinttime, NULL);
	}
	hinted = rt->hint;
#endif
}
