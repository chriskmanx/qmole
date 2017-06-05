/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gku-prompt-tool.c - Handles gui authentication for the keyring daemon.

   Copyright (C) 2009 Stefan Walter

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gku-prompt-util.h"

#include "egg/egg-dh.h"
#include "egg/egg-entry-buffer.h"
#include "egg/egg-error.h"
#include "egg/egg-hex.h"
#include "egg/egg-libgcrypt.h"
#include "egg/egg-secure-memory.h"

#include "gcr/gcr-unlock-options-widget.h"

#include <gcrypt.h>

#include <glib/gi18n.h>

#include <gtk/gtk.h>

#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <unistd.h>

static GKeyFile *input_data = NULL;
static GKeyFile *output_data = NULL;
static gboolean keyboard_grabbed = FALSE;

/* An encryption key for returning passwords */
static gpointer the_key = NULL;
static gsize n_the_key = 0;

#define LOG_ERRORS 1
#define GRAB_KEYBOARD 1

/**
* SECTION: gku-prompt-tool.c
* @short_description: Displays a propmt for 3rd party programs (ssh, gnupg)
**/

/* ------------------------------------------------------------------------------ */

/**
* primary: The part that will be bold
* secondary: The normal part of the text or NULL
*
*
*
* Returns The text encased in markup
**/
static gchar*
create_markup (const gchar *primary, const gchar *secondary)
{
	return g_markup_printf_escaped ("<span weight=\"bold\" size=\"larger\">%s</span>\n\n%s",
					primary, secondary ? secondary : "");

}

/**
* win: The GTK which's window should get the focus
* event: The event that triggered grabbing
* data: ignored
*
* Will grab the keyboard to the widget's window
*
* Returns TRUE if grabbed, FALSE else
**/
static gboolean
grab_keyboard (GtkWidget *win, GdkEvent *event, gpointer data)
{
	GdkGrabStatus status;
	if (!keyboard_grabbed && GRAB_KEYBOARD) {
		status = gdk_keyboard_grab (gtk_widget_get_window (win), FALSE, gdk_event_get_time (event));
		if (status == GDK_GRAB_SUCCESS) {
			keyboard_grabbed = TRUE;
		} else {
			g_message ("could not grab keyboard: %d", (int)status);
		}
	}
	return FALSE;
}

/**
* win: ignored
* event: the event that triggered ungrabbing
* data: ignored
*
* Will ungrab the keyboard
*
* Returns FALSE
**/
static gboolean
ungrab_keyboard (GtkWidget *win, GdkEvent *event, gpointer data)
{
	if (keyboard_grabbed)
		gdk_keyboard_ungrab (gdk_event_get_time (event));
	keyboard_grabbed = FALSE;
	return FALSE;
}

/**
* win: The window that changed state
* event: The event that triggered it
* data: ignored
*
* Depending on the state it will grab the keyboard or ungrab it.
*
* Returns FALSE
**/
static gboolean
window_state_changed (GtkWidget *win, GdkEventWindowState *event, gpointer data)
{
	GdkWindowState state = gdk_window_get_state (gtk_widget_get_window (win));

	if (state & GDK_WINDOW_STATE_WITHDRAWN ||
	    state & GDK_WINDOW_STATE_ICONIFIED ||
	    state & GDK_WINDOW_STATE_FULLSCREEN ||
	    state & GDK_WINDOW_STATE_MAXIMIZED)
		ungrab_keyboard (win, (GdkEvent*)event, data);
	else
		grab_keyboard (win, (GdkEvent*)event, data);

	return FALSE;
}


/**
* editable: The GTK_ENTRY that changed
* user_data: the progress bar to update
*
* Will update the password quality displayed in the progress bar.
*
**/
static void
on_password_changed (GtkEditable *editable, gpointer user_data)
{
	const char *password;
	int length, i;
	int upper, lower, digit, misc;
	gdouble pwstrength;

	password = gtk_entry_get_text (GTK_ENTRY (editable));

	/*
	 * This code is based on the Master Password dialog in Firefox
	 * (pref-masterpass.js)
	 * Original code triple-licensed under the MPL, GPL, and LGPL
	 * so is license-compatible with this file
	 */

	length = strlen (password);
	upper = 0;
	lower = 0;
	digit = 0;
	misc = 0;

	for ( i = 0; i < length ; i++) {
		if (g_ascii_isdigit (password[i]))
			digit++;
		else if (g_ascii_islower (password[i]))
			lower++;
		else if (g_ascii_isupper (password[i]))
			upper++;
		else
			misc++;
	}

	if (length > 5)
		length = 5;
	if (digit > 3)
		digit = 3;
	if (upper > 3)
		upper = 3;
	if (misc > 3)
		misc = 3;

	pwstrength = ((length*0.1)-0.2) + (digit*0.1) + (misc*0.15) + (upper*0.1);

	if (pwstrength < 0.0)
		pwstrength = 0.0;
	if (pwstrength > 1.0)
		pwstrength = 1.0;

	gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (user_data), pwstrength);
}

/**
* builder: The builder object to look for visibility keys in
* dialog: ignored
*
* Depending on the input_data and the keys in the group "visibility"
* this toggles the visibility of displayed widgets
*
**/
static void
prepare_visibility (GtkBuilder *builder, GtkDialog *dialog)
{
	gchar **keys, **key;
	GObject *object;

	keys = g_key_file_get_keys (input_data, "visibility", NULL, NULL);
	g_return_if_fail (keys);

	for (key = keys; key && *key; ++key) {
		object = gtk_builder_get_object (builder, *key);
		if (!GTK_IS_WIDGET (object)) {
			g_warning ("can't set visibility on invalid builder object: %s", *key);
			continue;
		}
		if (g_key_file_get_boolean (input_data, "visibility", *key, NULL))
			gtk_widget_show (GTK_WIDGET (object));
		else
			gtk_widget_hide (GTK_WIDGET (object));
	}

	g_strfreev (keys);
}

/**
* builder: ignored
* dialog: The dialog to set the title for
*
* Sets a new title to the dialog. The title is extracted from the input_data
*
**/
static void
prepare_titlebar (GtkBuilder *builder, GtkDialog *dialog)
{
	gchar *title;

	title = g_key_file_get_string (input_data, "prompt", "title", NULL);
	if (title)
		gtk_window_set_title (GTK_WINDOW (dialog), title);
	gtk_window_set_icon_name(GTK_WINDOW(dialog), "stock_lock");
	gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_CENTER);

	gtk_window_set_keep_above (GTK_WINDOW (dialog), TRUE);
	gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);
	gtk_window_set_type_hint (GTK_WINDOW (dialog), GDK_WINDOW_TYPE_HINT_NORMAL);
}

/**
* builder: The GtkBuilder to extract the warning label from
* text: The text to display in the warning label
*
* Displays a text in the warning_label
*
**/
static void
prepare_warning (GtkBuilder *builder, const gchar *text)
{
	GtkLabel *label;
	gchar *markup;

	label = GTK_LABEL (gtk_builder_get_object (builder, "warning_label"));
	g_return_if_fail (label);

	markup = g_markup_printf_escaped ("<span style=\"italic\">%s</span>", text);
	gtk_label_set_markup (label, markup);
	g_free (markup);

	gtk_widget_show (GTK_WIDGET (label));
}

/**
* builder: The GTKBuilder to look for widgets in
* dialog: ignored
*
* Reads data from the input_data and prepares a prompt dialog
*
**/
static void
prepare_prompt (GtkBuilder *builder, GtkDialog *dialog)
{
	gchar *primary, *secondary, *markup, *warning;
	GtkLabel *label;

	primary = g_key_file_get_string (input_data, "prompt", "primary", NULL);
	g_return_if_fail (primary);
	secondary = g_key_file_get_string (input_data, "prompt", "secondary", NULL);

	markup = create_markup (primary, secondary);
	g_free (primary);
	g_free (secondary);

	label = GTK_LABEL (gtk_builder_get_object (builder, "prompt_label"));
	g_return_if_fail (label);

	gtk_label_set_markup (label, markup);
	g_free (markup);

	warning = g_key_file_get_string (input_data, "prompt", "warning", NULL);
	if (warning != NULL)
		prepare_warning (builder, warning);
	g_free (warning);
}

/**
* builder: The GTK builder to use
* dialog: the dialog to add buttons to
*
* Adds buttons to the dialog. Which buttons to add is defined in input_data
*
**/
static void
prepare_buttons (GtkBuilder *builder, GtkDialog *dialog)
{
	gchar *ok_text;
	gchar *cancel_text;
	gchar *other_text;

	ok_text = g_key_file_get_string (input_data, "buttons", "ok", NULL);
	cancel_text = g_key_file_get_string (input_data, "buttons", "cancel", NULL);
	other_text = g_key_file_get_string (input_data, "buttons", "other", NULL);

	if (other_text)
		gtk_dialog_add_button (dialog, other_text, GTK_RESPONSE_APPLY);
	gtk_dialog_add_button (dialog, cancel_text ? cancel_text : GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
	gtk_dialog_add_button (dialog, ok_text ? ok_text : GTK_STOCK_OK, GTK_RESPONSE_OK);

	gtk_dialog_set_default_response (dialog, GTK_RESPONSE_OK);

	g_free (ok_text);
	g_free (cancel_text);
	g_free (other_text);
}

/**
* entry: the entry to set the buffer for
*
* Adds the secure egg_entry_buffer to the entry
*
**/
static void
prepare_password_entry (GtkEntry *entry)
{
	GtkEntryBuffer *buffer = egg_entry_buffer_new ();
	g_return_if_fail (entry);
	gtk_entry_set_buffer (entry, buffer);
	g_object_unref (buffer);
}

/**
* builder: The GTKBuilder
* dialog: The Dialog to prepare
*
* Password entries use a secure buffer. Set this up here.
*
**/
static void
prepare_passwords (GtkBuilder *builder, GtkDialog *dialog)
{
	GtkEntry *entry;
	GtkWidget *strength;

	entry = GTK_ENTRY(gtk_builder_get_object (builder, "password_entry"));
	prepare_password_entry (entry);

	strength = GTK_WIDGET (gtk_builder_get_object (builder, "strength_bar"));
	g_signal_connect (entry, "changed", G_CALLBACK (on_password_changed), strength);

	entry = GTK_ENTRY(gtk_builder_get_object (builder, "original_entry"));
	prepare_password_entry (entry);

	entry = GTK_ENTRY(gtk_builder_get_object (builder, "confirm_entry"));
	prepare_password_entry (entry);
}

/**
* builder: The GTKBuilder
* dialog: the dialog to connect signals for
*
* Registers the signal handlers for keyboard grab
*
**/
static void
prepare_security (GtkBuilder *builder, GtkDialog *dialog)
{
	/*
	 * When passwords are involved we grab the keyboard so that people
	 * don't accidentally type their passwords in other windows.
	 */
	g_signal_connect (dialog, "map-event", G_CALLBACK (grab_keyboard), NULL);
	g_signal_connect (dialog, "unmap-event", G_CALLBACK (ungrab_keyboard), NULL);
	g_signal_connect (dialog, "window-state-event", G_CALLBACK (window_state_changed), NULL);
}

static void
prepare_unlock_option (GcrUnlockOptionsWidget *unlock, const gchar *option)
{
	GError *error = NULL;
	gboolean sensitive;
	gchar *text;

	text = g_key_file_get_string (input_data, option, "label", NULL);
	if (text)
		gcr_unlock_options_widget_set_label (unlock, option, text);
	g_free (text);

	sensitive = g_key_file_get_boolean (input_data, option, "sensitive", &error);
	if (error == NULL) {
		text = g_key_file_get_string (input_data, option, "reason", NULL);
		gcr_unlock_options_widget_set_sensitive (unlock, option, sensitive, text);
		g_free (text);
	}

	g_clear_error (&error);
}

/**
* builder: GtkBuilderobject to read widgets from
* dialog: the prompt dialog
*
* Set default value depending on input_data
*
**/
static void
prepare_lock (GtkBuilder *builder, GtkDialog *dialog)
{
	GcrUnlockOptionsWidget *unlock;
	GtkWidget *area;
	gchar *option;
	guint ttl;

	unlock = GCR_UNLOCK_OPTIONS_WIDGET (gcr_unlock_options_widget_new ());
	area = GTK_WIDGET (gtk_builder_get_object (builder, "options_area"));
	g_object_set_data (G_OBJECT (dialog), "unlock-options-widget", unlock);
	gtk_container_add (GTK_CONTAINER (area), GTK_WIDGET (unlock));
	gtk_widget_show (GTK_WIDGET (unlock));

	ttl = g_key_file_get_integer (input_data, "unlock-options", "ttl", NULL);
	gcr_unlock_options_widget_set_ttl (unlock, ttl);

	option = g_key_file_get_string (input_data, "unlock-options", "choice", NULL);
	gcr_unlock_options_widget_set_choice (unlock, option ? option : GCR_UNLOCK_OPTION_SESSION);
	g_free (option);

	prepare_unlock_option (unlock, "always");
	prepare_unlock_option (unlock, "idle");
	prepare_unlock_option (unlock, "timeout");
	prepare_unlock_option (unlock, "session");
}

/**
* builder: The GTKBuilder
* dialog: ignored
*
* Reads the input_data expands the details area depending on "details"-"expanded"
*
**/
static void
prepare_details (GtkBuilder *builder, GtkDialog *dialog)
{
	GtkExpander *expander;
	gboolean expanded;

	expander = GTK_EXPANDER (gtk_builder_get_object (builder, "details_area"));
	expanded = g_key_file_get_boolean (input_data, "details", "expanded", NULL);
	gtk_expander_set_expanded (expander, expanded);
}

/**
* builder: The gtk builder to add the gku-prompt.ui to
*
* Create and set up the dialog
*
* Returns the new dialog
**/
static GtkDialog*
prepare_dialog (GtkBuilder *builder)
{
	GError *error = NULL;
	GtkDialog *dialog;

	if (!gtk_builder_add_from_file (builder, UIDIR "gku-prompt.ui", &error)) {
		g_warning ("couldn't load prompt ui file: %s", egg_error_message (error));
		g_clear_error (&error);
		return NULL;
	}

	dialog = GTK_DIALOG (gtk_builder_get_object (builder, "prompt_dialog"));
	g_return_val_if_fail (GTK_IS_DIALOG (dialog), NULL);

	prepare_visibility (builder, dialog);
	prepare_titlebar (builder, dialog);
	prepare_prompt (builder, dialog);
	prepare_buttons (builder, dialog);
	prepare_passwords (builder, dialog);
	prepare_security (builder, dialog);
	prepare_lock (builder, dialog);
	prepare_details (builder, dialog);

	return dialog;
}

/**
* parent: The parent dialog
*
* Displays a verification dialog if the user wants to use empty passwords
*
* Returns TRUE if the user wants to store data unencrypted
**/
static gboolean
validate_blank_password (GtkWindow *parent)
{
	GtkWidget *dialog;
	gchar *markup;
	gint ret;

	dialog = gtk_message_dialog_new (parent, GTK_DIALOG_MODAL, GTK_MESSAGE_WARNING,
	                                 GTK_BUTTONS_NONE, NULL);

	markup = create_markup (_("Store passwords unencrypted?"),
	                        _("By choosing to use a blank password, your stored passwords will not be safely encrypted. "
	                          "They will be accessible by anyone with access to your files."));
	gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), markup);
	g_free (markup);

	gtk_dialog_add_buttons (GTK_DIALOG (dialog),
	                        GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
	                        _("Use Unsafe Storage"), GTK_RESPONSE_ACCEPT,
	                        NULL);

	ret = gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);

	return ret == GTK_RESPONSE_ACCEPT;
}

/**
* builder: The GTK builder being used
* dialog: The displayed password dialog
*
* Checks if the passwords are identical (password and confirm)
*
* Returns TRUE if the passwords match, FALSE else
**/
static gboolean
validate_passwords (GtkBuilder *builder, GtkDialog *dialog)
{
	GtkWidget *pentry, *centry;
	const gchar *password, *confirm;
	const gchar *env;

	pentry = GTK_WIDGET (gtk_builder_get_object (builder, "password_entry"));
	centry = GTK_WIDGET (gtk_builder_get_object (builder, "confirm_entry"));
	g_return_val_if_fail (pentry && centry, FALSE);

	/* No confirm, no password check */
	if (!gtk_widget_get_realized (GTK_WIDGET (centry)))
		return TRUE;

	password = gtk_entry_get_text (GTK_ENTRY (pentry));
	confirm = gtk_entry_get_text (GTK_ENTRY (centry));
	g_return_val_if_fail (password && confirm, FALSE);

	/* Do the passwords match? */
	if (!g_str_equal (password, confirm)) {
		prepare_warning (builder, _("Passwords do not match."));
		return FALSE;
	}

	/* Double check about blank passwords */
	if (!password[0]) {

		/* Don't allow blank passwords if in paranoid mode */
		env = g_getenv ("GNOME_KEYRING_PARANOID");
		if (env && *env) {
			prepare_warning (builder, _("Password cannot be blank"));
			return FALSE;

		/* Double check with the user */
		} else if (!validate_blank_password (GTK_WINDOW (dialog))) {
			return FALSE;
		}
	}

	return TRUE;
}

/**
* builder: GTKBuilder data
* dialog: The password dialog
* response: ignored
*
* Validates if passwords are identical or valid otherwise
*
* Returns TRUE if passwords are ok
**/
static gboolean
validate_dialog (GtkBuilder *builder, GtkDialog *dialog, gint response)
{
	if (!validate_passwords (builder, dialog))
		return FALSE;

	return TRUE;
}

/**
* Negotiates crypto between the calling programm and the prompt
*
* Reads data from the transport section of input_data and sends the public key back
* in the transport section of the output_data.
*
* Returns TRUE on success
**/
static gboolean
negotiate_transport_crypto (void)
{
	gcry_mpi_t base, prime, peer;
	gcry_mpi_t key, pub, priv;
	gboolean ret = FALSE;

	g_assert (!the_key);
	base = prime = peer = NULL;
	key = pub = priv = NULL;

	/* The DH stuff coming in from our caller */
	if (gku_prompt_util_decode_mpi (input_data, "transport", "prime", &prime) &&
	    gku_prompt_util_decode_mpi (input_data, "transport", "base", &base) &&
	    gku_prompt_util_decode_mpi (input_data, "transport", "public", &peer)) {

		/* Generate our own public/priv, and then a key, send it back */
		if (egg_dh_gen_pair (prime, base, 0, &pub, &priv)) {

			gku_prompt_util_encode_mpi (output_data, "transport", "public", pub);

			/* Build up a key we can use */
			n_the_key = 16;
			the_key = egg_dh_gen_secret (peer, priv, prime, n_the_key);
			ret = (the_key != NULL);
		}
	}

	gcry_mpi_release (base);
	gcry_mpi_release (prime);
	gcry_mpi_release (peer);
	gcry_mpi_release (key);
	gcry_mpi_release (pub);
	gcry_mpi_release (priv);

	return ret;
}

/**
* builder: The GTKBuilder
* password_type: password type description
*
* Reads the encrypted data from the prompt and transfers it using output_data.
* If crypto is available, it uses crypto.
*
**/
static void
gather_password (GtkBuilder *builder, const gchar *password_type)
{
	GtkEntry *entry;
	gchar iv[16];
	gpointer data;
	gsize n_data;
	gchar *name;
	const gchar *text;
	gchar *value;

	name = g_strdup_printf ("%s_entry", password_type);
	entry = GTK_ENTRY (gtk_builder_get_object (builder, name));
	g_return_if_fail (GTK_IS_ENTRY (entry));
	g_free (name);

	if (!gtk_widget_get_realized (GTK_WIDGET (entry)))
		return;

	/* A non-encrypted password: just send the value back */
	if (!g_key_file_has_group (input_data, "transport")) {
		text = gtk_entry_get_text (entry);
		value = egg_hex_encode ((const guchar*)text, strlen (text));
		g_key_file_set_string (output_data, password_type, "parameter", "");
		g_key_file_set_string (output_data, password_type, "value", value);
		g_free (value);
		return;
	}

	if (!the_key && !negotiate_transport_crypto ()) {
		g_warning ("couldn't negotiate transport crypto for password");
		return;
	}

	gcry_create_nonce (iv, sizeof (iv));
	data = gku_prompt_util_encrypt_text (the_key, n_the_key, iv, sizeof (iv),
	                                     gtk_entry_get_text (entry), &n_data);
	g_return_if_fail (data);

	gku_prompt_util_encode_hex (output_data, password_type, "parameter", iv, sizeof (iv));
	gku_prompt_util_encode_hex (output_data, password_type, "value", data, n_data);

	g_free (data);
}

/**
* response: The response value from the dialog
*
* Sets "prompt""response" of output_data to a string corresponding to
* the response value
*
**/
static void
gather_response (gint response)
{
	const gchar *value = NULL;

	switch (response) {
	case GTK_RESPONSE_OK:
		value = "ok";
		break;
	case GTK_RESPONSE_CANCEL:
		value = "no";
		break;
	case GTK_RESPONSE_DELETE_EVENT:
		value = "";
		break;
	case GTK_RESPONSE_APPLY:
		value = "other";
		break;
	default:
		g_return_if_reached ();
		break;
	}

	g_key_file_set_string (output_data, "prompt", "response", value);
}

/**
* builder: the gtk builder object
* dialog: The dialog to extract the data from
*
* Gets the unlocking settings and stores them in output_data
*
**/
static void
gather_unlock_options (GtkBuilder *builder, GtkDialog *dialog)
{
	GcrUnlockOptionsWidget *unlock;
	const gchar *choice;

	unlock = g_object_get_data (G_OBJECT (dialog), "unlock-options-widget");

	choice = gcr_unlock_options_widget_get_choice (unlock);
	if (choice) {
		g_key_file_set_integer (output_data, "unlock-options", "ttl",
		                        gcr_unlock_options_widget_get_ttl (unlock));

		g_key_file_set_string (output_data, "unlock-options", "choice", choice);
	}
}

/**
* builder: The builder
* dialog: ignored
*
* Extracts the status of the details expander and stores it in "details""expanded"
* of the output data
*
**/
static void
gather_details (GtkBuilder *builder, GtkDialog *dialog)
{
	GtkExpander *expander;

	expander = GTK_EXPANDER (gtk_builder_get_object (builder, "details_area"));
	g_key_file_set_boolean (output_data, "details", "expanded",
	                        gtk_expander_get_expanded (expander));
}


/**
* builder: The GTKBuilder object
* dialog: the prompt dialog
*
* Called on "ok" or "apply" user choice.
*
**/
static void
gather_dialog (GtkBuilder *builder, GtkDialog *dialog)
{
	gather_password (builder, "password");
	gather_password (builder, "confirm");
	gather_password (builder, "original");
	gather_unlock_options (builder, dialog);
	gather_details (builder, dialog);
}

/**
* Sets up the dialog, shows it and waits for response
*
*
**/
static void
run_dialog (void)
{
	GtkBuilder *builder;
	GtkDialog *dialog;
	gint res;

	builder = gtk_builder_new ();
	dialog = prepare_dialog (builder);
	if (!dialog) {
		g_object_unref (builder);
		return;
	}

	for (;;) {
		gtk_widget_show (GTK_WIDGET (dialog));
		res = gtk_dialog_run (dialog);
		switch (res) {
		case GTK_RESPONSE_OK:
		case GTK_RESPONSE_APPLY:
			if (!validate_dialog (builder, dialog, res))
				continue;
			gather_dialog (builder, dialog);
			break;
		case GTK_RESPONSE_CANCEL:
		case GTK_RESPONSE_DELETE_EVENT:
			break;
		default:
			g_return_if_reached ();
			break;
		}

		/* Break out of the loop by default */
		break;
	}

	gather_response (res);
	g_object_unref (builder);
}

/* -----------------------------------------------------------------------------
 * MEMORY
 */

static gboolean do_warning = TRUE;
#define WARNING  "couldn't allocate secure memory to keep passwords " \
		 "and or keys from being written to the disk"

#define ABORTMSG "The GNOME_KEYRING_PARANOID environment variable was set. " \
                 "Exiting..."

/*
 * These are called from gkr-secure-memory.c to provide appropriate
 * locking for memory between threads
 */

/**
 * egg_memory_lock:
 *
 * Memory locking for threads
 *
 */
void
egg_memory_lock (void)
{
	/* No threads used in prompt tool, doesn't need locking */
}

/**
 * egg_memory_unlock:
 *
 * Memory locking for threads
 *
 */
void
egg_memory_unlock (void)

{
	/* No threads used in prompt tool, doesn't need locking */
}

/**
 * egg_memory_fallback:
 * @p: Memory pointer. Can be NULL to create memory
 * @sz: Size of the pointer. 0 for freeing, everything else is resize or allocate
 *
 * An allround fallback function for
 * freeing, allocating and resizing memory
 * Behavior also depends on GNOME_KEYRING_PARANOID environment var.
 *
 * Returns:
 */
void*
egg_memory_fallback (void *p, size_t sz)
{
	const gchar *env;

	/* We were asked to free memory */
	if (!sz) {
		g_free (p);
		return NULL;
	}

	/* We were asked to allocate */
	if (!p) {
		if (do_warning) {
			g_message (WARNING);
			do_warning = FALSE;
		}

		env = g_getenv ("GNOME_KEYRING_PARANOID");
		if (env && *env)
			g_error (ABORTMSG);
		return g_malloc0 (sz);
	}

	/*
	 * Reallocation is a bit of a gray area, as we can be asked
	 * by external libraries (like libgcrypt) to reallocate a
	 * non-secure block into secure memory. We cannot satisfy
	 * this request (as we don't know the size of the original
	 * block) so we just try our best here.
	 */

	return g_realloc (p, sz);
}

/* -------------------------------------------------------------------------
 * HELPERS
 */

/**
* msg1: optional first message
* msg2: optional second message
*
* Because Solaris doesn't have err() :(
* prints an error, exits the program. Depending on LOG_ERRORS it will also write logs
*
**/
static void
fatal (const char *msg1, const char *msg2)
{
	g_printerr ("%s: %s%s%s\n",
	            g_get_prgname (),
	            msg1 ? msg1 : "",
	            msg1 && msg2 ? ": " : "",
	            msg2 ? msg2 : "");
#if LOG_ERRORS
	syslog (LOG_AUTH | LOG_ERR, "%s%s%s\n",
	         msg1 ? msg1 : "",
	         msg1 && msg2 ? ": " : "",
	         msg2 ? msg2 : "");
#endif
	exit (1);
}

/**
* log_domain: Optional domain for the log
* log_level: Flags for the log level
* message: Message for the log
* user_data: used for the default log handler
*
* Does logging
*
**/
static void
log_handler (const gchar *log_domain, GLogLevelFlags log_level,
             const gchar *message, gpointer user_data)
{
	int level;

	/* Note that crit and err are the other way around in syslog */

	switch (G_LOG_LEVEL_MASK & log_level) {
	case G_LOG_LEVEL_ERROR:
		level = LOG_CRIT;
		break;
	case G_LOG_LEVEL_CRITICAL:
		level = LOG_ERR;
		break;
	case G_LOG_LEVEL_WARNING:
		level = LOG_WARNING;
		break;
	case G_LOG_LEVEL_MESSAGE:
		level = LOG_NOTICE;
		break;
	case G_LOG_LEVEL_INFO:
		level = LOG_INFO;
		break;
	case G_LOG_LEVEL_DEBUG:
		level = LOG_DEBUG;
		break;
	default:
		level = LOG_ERR;
		break;
	}

#if LOG_ERRORS
	/* Log to syslog first */
	if (log_domain)
		syslog (level, "%s: %s", log_domain, message);
	else
		syslog (level, "%s", message);
#endif /* LOG_ERRORS */

	/* And then to default handler for aborting and stuff like that */
	g_log_default_handler (log_domain, log_level, message, user_data);
}

/**
*
* Sets up logging. Identity is "gnome-keyring-prompt"
*
**/
static void
prepare_logging ()
{
	GLogLevelFlags flags = G_LOG_FLAG_FATAL | G_LOG_LEVEL_ERROR |
	                       G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING |
	                       G_LOG_LEVEL_MESSAGE | G_LOG_LEVEL_INFO;

	openlog ("gnome-keyring-prompt", 0, LOG_AUTH);

	g_log_set_handler (NULL, flags, log_handler, NULL);
	g_log_set_handler ("Glib", flags, log_handler, NULL);
	g_log_set_handler ("Gtk", flags, log_handler, NULL);
	g_log_set_handler ("Gnome", flags, log_handler, NULL);
	g_log_set_default_handler (log_handler, NULL);
}

/**
* data: data to write to stdout
* len: size of this data
*
* Writes data to stdout
*
**/
static void
write_all_output (const gchar *data, gsize len)
{
	int res;

	while (len > 0) {
		res = write (1, data, len);
		if (res < 0) {
			if (errno == EAGAIN || errno == EINTR)
				continue;
			if (errno != EPIPE)
				g_warning ("couldn't write dialog response to output: %s",
				           g_strerror (errno));
			exit (1);
		} else if (res == 0) {
			g_warning ("couldn't write all dialog response to output");
		} else  {
			len -= res;
			data += res;
		}
	}
}

/**
* Reads input from stdin. This is a key-value "file" containing control
* data for this prompt.
*
* Returns the input as gchar*
**/
static gchar*
read_all_input (void)
{
	GString *data = g_string_new ("");
	gchar buf[256];
	int r;

	for (;;) {
		r = read (0, buf, sizeof (buf));
		if (r < 0) {
			if (errno == EAGAIN || errno == EINTR)
				continue;
			g_warning ("couldn't read auth dialog instructions from input: %s",
			           g_strerror (errno));
			exit (1);
		}
		if (r == 0)
			break;
		g_string_append_len (data, buf, r);
	}

	return g_string_free (data, FALSE);
}

/**
* sig: not used
*
* Exits
*
**/
static void
hup_handler (int sig)
{
	/*
	 * Exit due to being cancelled. No real need to do any
	 * cleanup or anything. All memory will be freed on process end.
	 **/
	_exit (0);
}

/**
 * main:
 * @argc:
 * @argv[]: Sent to gtk_init
 *
 * Prompt for GnuPG and SSH. Communicates using stdin/stdout. Communication data
 * is in ini-file structures
 *
 * Returns: 0
 */
int
main (int argc, char *argv[])
{
	GError *err = NULL;
	gchar *data;
	gboolean ret;
	gsize length;

	/* Exit on HUP signal */
	signal(SIGINT,  hup_handler);

	prepare_logging ();

	egg_libgcrypt_initialize ();

	input_data = g_key_file_new ();
	output_data = g_key_file_new ();

	gtk_init (&argc, &argv);

#ifdef HAVE_LOCALE_H
	/* internationalisation */
	setlocale (LC_ALL, "");
#endif

#ifdef HAVE_GETTEXT
	bindtextdomain (GETTEXT_PACKAGE, GNOMELOCALEDIR);
	textdomain (GETTEXT_PACKAGE);
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif

	data = read_all_input ();
	g_assert (data);

	if (!data[0])
		fatal ("no auth dialog instructions", NULL);

	ret = g_key_file_load_from_data (input_data, data, strlen (data), G_KEY_FILE_NONE, &err);
	g_free (data);

	if (!ret)
		fatal ("couldn't parse auth dialog instructions", egg_error_message (err));

	run_dialog ();

	/* Cleanup after any key */
	if (the_key) {
		egg_secure_clear (the_key, n_the_key);
		egg_secure_free (the_key);
		the_key = NULL;
		n_the_key = 0;
	}

	g_key_file_free (input_data);
	data = g_key_file_to_data (output_data, &length, &err);
	g_key_file_free (output_data);

	if (!data)
		fatal ("couldn't format auth dialog response: %s", egg_error_message (err));

	write_all_output (data, length);
	g_free (data);

	return 0;
}
