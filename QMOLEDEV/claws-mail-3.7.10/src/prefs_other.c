/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2011 Colin Leroy <colin@colino.net> & The Claws Mail Team
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

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"
#include "combobox.h"

#include "manage_window.h"
#ifdef HAVE_LIBETPAN
#include "imap-thread.h"
#endif

typedef struct _OtherPage
{
	PrefsPage page;

	GtkWidget *window;

	GtkWidget *checkbtn_addaddrbyclick;
	GtkWidget *checkbtn_confonexit;
	GtkWidget *checkbtn_cleanonexit;
	GtkWidget *checkbtn_askonclean;
	GtkWidget *checkbtn_warnqueued;
	GtkWidget *spinbtn_iotimeout;
	GtkWidget *checkbtn_gtk_can_change_accels;
	GtkWidget *checkbtn_askonfilter;
	GtkWidget *checkbtn_use_shred;
	GtkWidget *checkbtn_real_time_sync;
	GtkWidget *flush_metadata_faster_radiobtn;
	GtkWidget *flush_metadata_safer_radiobtn;
} OtherPage;

static struct KeybindDialog {
	GtkWidget *window;
	GtkWidget *combo;
} keybind;

static void prefs_keybind_select		(void);
static gint prefs_keybind_deleted		(GtkWidget	*widget,
						 GdkEventAny	*event,
						 gpointer	 data);
static gboolean prefs_keybind_key_pressed	(GtkWidget	*widget,
						 GdkEventKey	*event,
						 gpointer	 data);
static void prefs_keybind_cancel		(void);
static void prefs_keybind_apply_clicked		(GtkWidget	*widget);


static void prefs_keybind_select(void)
{
	GtkWidget *window;
	GtkWidget *vbox1;
	GtkWidget *hbox1;
	GtkWidget *label;
	GtkWidget *combo;
	GtkWidget *confirm_area;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_other");
	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	gtk_window_set_title (GTK_WINDOW (window), 
				_("Choose preset keyboard shortcuts"));
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal (GTK_WINDOW (window), TRUE);
	gtk_window_set_resizable(GTK_WINDOW (window), FALSE);
	manage_window_set_transient (GTK_WINDOW (window));

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_container_add (GTK_CONTAINER (window), vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), 2);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, FALSE, 0);

	label = gtk_label_new
		(_("Select preset:"));
	gtk_box_pack_start (GTK_BOX (hbox1), label, FALSE, FALSE, 0);
	gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);

	combo = combobox_text_new(FALSE,
			       _("Default"),
			       "Mew / Wanderlust",
			       "Mutt",
			       NULL);
	gtk_box_pack_start (GTK_BOX (hbox1), combo, TRUE, TRUE, 0);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, FALSE, 0);

	label = gtk_label_new
		(_("You can also modify each menu shortcut by pressing\n"
		   "any key(s) when focusing the mouse pointer on the item."));
	gtk_box_pack_start (GTK_BOX (hbox1), label, FALSE, FALSE, 0);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
	gtkut_widget_set_small_font_size (label);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, FALSE, 0);

	gtkut_stock_button_set_create (&confirm_area, &cancel_btn, GTK_STOCK_CANCEL,
				       &ok_btn, GTK_STOCK_OK,
				       NULL, NULL);
	gtk_box_pack_end (GTK_BOX (hbox1), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_focus (ok_btn);

	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect (G_OBJECT (window), "delete_event",
			  G_CALLBACK (prefs_keybind_deleted), NULL);
	g_signal_connect (G_OBJECT (window), "key_press_event",
			  G_CALLBACK (prefs_keybind_key_pressed), NULL);
	g_signal_connect (G_OBJECT (ok_btn), "clicked",
			  G_CALLBACK (prefs_keybind_apply_clicked),
			  NULL);
	g_signal_connect (G_OBJECT (cancel_btn), "clicked",
			  G_CALLBACK (prefs_keybind_cancel),
			  NULL);

	gtk_widget_show_all(window);

	keybind.window = window;
	keybind.combo = combo;
}

static gboolean prefs_keybind_key_pressed(GtkWidget *widget, GdkEventKey *event,
					  gpointer data)
{
	if (event && event->keyval == GDK_Escape)
		prefs_keybind_cancel();
	return FALSE;
}

static gint prefs_keybind_deleted(GtkWidget *widget, GdkEventAny *event,
				  gpointer data)
{
	prefs_keybind_cancel();
	return TRUE;
}

static void prefs_keybind_cancel(void)
{
	gtk_widget_destroy(keybind.window);
	keybind.window = NULL;
	keybind.combo = NULL;
}
  
struct KeyBind {
	const gchar *accel_path;
	const gchar *accel_key;
};

static void prefs_keybind_apply(struct KeyBind keybind[], gint num)
{
	gint i;
	guint key;
	GdkModifierType mods;

	for (i = 0; i < num; i++) {
		const gchar *accel_key
			= keybind[i].accel_key ? keybind[i].accel_key : "";
		gtk_accelerator_parse(accel_key, &key, &mods);
		gtk_accel_map_change_entry(keybind[i].accel_path,
					   key, mods, TRUE);
	}
}

static void prefs_keybind_apply_clicked(GtkWidget *widget)
{
	gchar *text;
	struct KeyBind *menurc;
	gint n_menurc;

	static struct KeyBind default_menurc[] = {
		/* main */
		{"<Actions>/Menu/File/EmptyTrashes",			"<shift>D"},
		{"<Actions>/Menu/File/SaveAs",				"<control>S"},
		{"<Actions>/Menu/File/Print",				"<control>P"},
		{"<Actions>/Menu/File/OfflineMode",			"<control>W"},
		{"<Actions>/Menu/File/SynchroniseFolders",		"<control><shift>S"},
		{"<Actions>/Menu/File/Exit",				"<control>Q"},

		{"<Actions>/Menu/Edit/Copy",				"<control>C"},
		{"<Actions>/Menu/Edit/SelectAll",			"<control>A"},
		{"<Actions>/Menu/Edit/Find",				"<control>F"},
		{"<Actions>/Menu/Edit/SearchFolder",			"<shift><control>F"},
		{"<Actions>/Menu/Edit/QuickSearch",			"slash"},

		{"<Actions>/Menu/View/ShowHide/MessageView",		"V"},
		{"<Actions>/Menu/View/ThreadView",			"<control>T"},
		{"<Actions>/Menu/View/GoTo/Prev",			"P"},
		{"<Actions>/Menu/View/GoTo/Next",			"N"},
		{"<Actions>/Menu/View/GoTo/PrevUnread",			"<shift>P"},
		{"<Actions>/Menu/View/GoTo/NextUnread",			"<shift>N"},
		{"<Actions>/Menu/View/GoTo/OtherFolder",		"G"},
		{"<Actions>/Menu/View/OpenNewWindow",			"<control><alt>N"},
		{"<Actions>/Menu/View/MessageSource",			"<control>U"},
		{"<Actions>/Menu/View/AllHeaders",			"<control>H"},
		{"<Actions>/Menu/View/UpdateSummary",			"<control><alt>U"},

		{"<Actions>/Menu/Message/Receive/CurrentAccount",
									"<control>I"},
		{"<Actions>/Menu/Message/Receive/AllAccounts",		"<shift><control>I"},
		{"<Actions>/Menu/Message/ComposeEmail",			"<control>M"},
		{"<Actions>/Menu/Message/Reply",			"<control>R"},
		{"<Actions>/Menu/Message/ReplyTo/All",			"<shift><control>R"},
		{"<Actions>/Menu/Message/ReplyTo/Sender",		""},
		{"<Actions>/Menu/Message/ReplyTo/List",			"<control>L"},
		{"<Actions>/Menu/Message/Forward",			"<control><alt>F"},
		{"<Actions>/Menu/Message/Move",				"<control>O"},
		{"<Actions>/Menu/Message/Copy",				"<shift><control>O"},
		{"<Actions>/Menu/Message/Trash",			"<control>D"},
		{"<Actions>/Menu/Message/Mark/Mark",			"<shift>asterisk"},
		{"<Actions>/Menu/Message/Mark/Unmark",			"U"},
		{"<Actions>/Menu/Message/Mark/MarkUnread",		"<shift>exclam"},
		{"<Actions>/Menu/Message/Mark/MarkRead",		""},

		{"<Actions>/Menu/Tools/AddressBook",			"<shift><control>A"},
		{"<Actions>/Menu/Tools/Execute",			"X"},
		{"<Actions>/Menu/Tools/NetworkLog",			"<shift><control>L"},
		/* compose */
		{"<Actions>/Menu/Message/Send",				"<control>Return"},
		{"<Actions>/Menu/Message/SendLater",			"<shift><control>S"},
		{"<Actions>/Menu/Message/AttachFile",			"<control>M"},
		{"<Actions>/Menu/Message/InsertFile",			"<control>I"},
		{"<Actions>/Menu/Message/InsertSig",			"<control>G"},
		{"<Actions>/Menu/Message/Save",				"<control>S"},
		{"<Actions>/Menu/Message/Close",			"<control>W"},
		{"<Actions>/Menu/Edit/Undo",				"<control>Z"},
		{"<Actions>/Menu/Edit/Redo",				"<control>Y"},
		{"<Actions>/Menu/Edit/Cut",				"<control>X"},
		{"<Actions>/Menu/Edit/Copy",				"<control>C"},
		{"<Actions>/Menu/Edit/Paste",				"<control>V"},
		{"<Actions>/Menu/Edit/SelectAll",			"<control>A"},
		{"<Actions>/Menu/Edit/Advanced/BackChar",		"<control>B"},
		{"<Actions>/Menu/Edit/Advanced/ForwChar",		"<control>F"},
		{"<Actions>/Menu/Edit/Advanced/BackWord",		""},
		{"<Actions>/Menu/Edit/Advanced/ForwWord",		""},
		{"<Actions>/Menu/Edit/Advanced/BegLine",		""},
		{"<Actions>/Menu/Edit/Advanced/EndLine",		"<control>E"},
		{"<Actions>/Menu/Edit/Advanced/PrevLine",		"<control>P"},
		{"<Actions>/Menu/Edit/Advanced/NextLine",		"<control>N"},
		{"<Actions>/Menu/Edit/Advanced/DelBackChar",		"<control>H"},
		{"<Actions>/Menu/Edit/Advanced/DelForwChar",		"<control>D"},
		{"<Actions>/Menu/Edit/Advanced/DelBackWord",		""},
		{"<Actions>/Menu/Edit/Advanced/DelForwWord",		""},
		{"<Actions>/Menu/Edit/Advanced/DelLine",		"<control>U"},
		{"<Actions>/Menu/Edit/Advanced/DelEndLine",		"<control>K"},
		{"<Actions>/Menu/Edit/WrapPara",			"<control>L"},
		{"<Actions>/Menu/Edit/WrapAllLines",			"<control><alt>L"},
		{"<Actions>/Menu/Edit/AutoWrap",			"<shift><control>L"},
		{"<Actions>/Menu/Edit/ExtEditor",			"<shift><control>X"},
		{"<Actions>/Menu/Tools/AddressBook",			"<shift><control>A"},
	};

	static struct KeyBind mew_wl_menurc[] = {
		/* main */
		{"<Actions>/Menu/File/EmptyTrashes",			"<shift>D"},
		{"<Actions>/Menu/File/SaveAs",				"Y"},
		{"<Actions>/Menu/File/Print",				"<control>numbersign"},
		{"<Actions>/Menu/File/Exit",				"<shift>Q"},

		{"<Actions>/Menu/Edit/Copy",				"<control>C"},
		{"<Actions>/Menu/Edit/SelectAll",			"<control>A"},
		{"<Actions>/Menu/Edit/Find",				"<control>F"},
		{"<Actions>/Menu/Edit/SearchFolder",			"<control>S"},
		{"<Actions>/Menu/Edit/QuickSearch",			"slash"},

		{"<Actions>/Menu/View/ShowHide/MessageView",		""},
		{"<Actions>/Menu/View/ThreadView",			"<shift>T"},
		{"<Actions>/Menu/View/GoTo/Prev",			"P"},
		{"<Actions>/Menu/View/GoTo/Next",			"N"},
		{"<Actions>/Menu/View/GoTo/PrevUnread",			"<shift>P"},
		{"<Actions>/Menu/View/GoTo/NextUnread",			"<shift>N"},
		{"<Actions>/Menu/View/GoTo/OtherFolder",		"G"},
		{"<Actions>/Menu/View/OpenNewWindow",			"<control><alt>N"},
		{"<Actions>/Menu/View/MessageSource",			"<control>U"},
		{"<Actions>/Menu/View/AllHeaders",			"<shift>H"},
		{"<Actions>/Menu/View/UpdateSummary",			"<shift>S"},

		{"<Actions>/Menu/Message/Receive/CurrentAccount",
									"<control>I"},
		{"<Actions>/Menu/Message/Receive/AllAccounts",		"<shift><control>I"},
		{"<Actions>/Menu/Message/ComposeEmail",			"W"},
		{"<Actions>/Menu/Message/Reply",			"<control>R"},
		{"<Actions>/Menu/Message/ReplyTo/All",			"<shift>A"},
		{"<Actions>/Menu/Message/ReplyTo/Sender",		""},
		{"<Actions>/Menu/Message/ReplyTo/List",			"<control>L"},
		{"<Actions>/Menu/Message/Forward",			"F"},
		{"<Actions>/Menu/Message/Move",				"O"},
		{"<Actions>/Menu/Message/Copy",				"<shift>O"},
		{"<Actions>/Menu/Message/Trash",			"D"},
		{"<Actions>/Menu/Message/Mark/Mark",			"<shift>asterisk"},
		{"<Actions>/Menu/Message/Mark/Unmark",			"U"},
		{"<Actions>/Menu/Message/Mark/MarkUnread",		"<shift>exclam"},
		{"<Actions>/Menu/Message/Mark/MarkRead",		"<shift>R"},

		{"<Actions>/Menu/Tools/AddressBook",			"<shift><control>A"},
		{"<Actions>/Menu/Tools/Execute",			"X"},
		{"<Actions>/Menu/Tools/NetworkLog",			"<shift><control>L"},
		/* compose */
		{"<Actions>/Menu/Message/Close",			"<alt>W"},
		{"<Actions>/Menu/Edit/SelectAll",			""},
		{"<Actions>/Menu/Edit/Advanced/BackChar",		"<alt>B"},
		{"<Actions>/Menu/Edit/Advanced/ForwChar",		"<alt>F"},
		{"<Actions>/Menu/Edit/Advanced/BackWord",		""},
		{"<Actions>/Menu/Edit/Advanced/ForwWord",		""},
		{"<Actions>/Menu/Edit/Advanced/BegLine",		"<control>A"},
		{"<Actions>/Menu/Edit/Advanced/DelBackWord",		"<control>W"},
		{"<Actions>/Menu/Edit/Advanced/DelForwWord",		"<alt>D"},
	};

	static struct KeyBind mutt_menurc[] = {
		/* main */
		{"<Actions>/Menu/File/EmptyTrashes",			""},
		{"<Actions>/Menu/File/SaveAs",				"S"},
		{"<Actions>/Menu/File/Print",				"P"},
		{"<Actions>/Menu/File/Exit",				"Q"},

		{"<Actions>/Menu/Edit/Copy",				"<control>C"},
		{"<Actions>/Menu/Edit/SelectAll",			"<control>A"},
		{"<Actions>/Menu/Edit/Find",				"<control>F"},
		{"<Actions>/Menu/Edit/SearchFolder",			"/"},

		{"<Actions>/Menu/View/ShowHide/MessageView",		"V"},
		{"<Actions>/Menu/View/ThreadView",			"<control>T"},
		{"<Actions>/Menu/View/GoTo/Prev",			""},
		{"<Actions>/Menu/View/GoTo/Next",			""},
		{"<Actions>/Menu/View/GoTo/PrevUnread",			""},
		{"<Actions>/Menu/View/GoTo/NextUnread",			""},
		{"<Actions>/Menu/View/GoTo/OtherFolder",		"C"},
		{"<Actions>/Menu/View/OpenNewWindow",			"<control><alt>N"},
		{"<Actions>/Menu/View/MessageSource",			"<control>U"},
		{"<Actions>/Menu/View/AllHeaders",			"<control>H"},
		{"<Actions>/Menu/View/UpdateSummary",			"<control><alt>U"},

		{"<Actions>/Menu/Message/Receive/CurrentAccount",
									"<control>I"},
		{"<Actions>/Menu/Message/Receive/AllAccounts",		"<shift><control>I"},
		{"<Actions>/Menu/Message/ComposeEmail",			"M"},
		{"<Actions>/Menu/Message/Reply",			"R"},
		{"<Actions>/Menu/Message/ReplyTo/All",			"G"},
		{"<Actions>/Menu/Message/ReplyTo/Sender",		""},
		{"<Actions>/Menu/Message/ReplyTo/List",			"<control>L"},
		{"<Actions>/Menu/Message/Forward",			"F"},
		{"<Actions>/Menu/Message/Move",				"<control>O"},
		{"<Actions>/Menu/Message/Copy",				"<shift>C"},
		{"<Actions>/Menu/Message/Trash",			"D"},
		{"<Actions>/Menu/Message/Mark/Mark",			"<shift>F"},
		{"<Actions>/Menu/Message/Mark/Unmark",			"U"},
		{"<Actions>/Menu/Message/Mark/MarkUnread",		"<shift>N"},
		{"<Actions>/Menu/Message/Mark/MarkRead",		""},

		{"<Actions>/Menu/Tools/AddressBook",			"<shift><control>A"},
		{"<Actions>/Menu/Tools/Execute",			"X"},
		{"<Actions>/Menu/Tools/NetworkLog",			"<shift><control>L"},
		/* compose */
		{"<Actions>/Menu/Message/Close",			"<alt>W"},
		{"<Actions>/Menu/Edit/SelectAll",			""},
		{"<Actions>/Menu/Edit/Advanced/BackWord",		"<alt>B"},
		{"<Actions>/Menu/Edit/Advanced/ForwWord",		"<alt>F"},
		{"<Actions>/Menu/Edit/Advanced/BegLine",		"<control>A"},
		{"<Actions>/Menu/Edit/Advanced/DelBackWord",		"<control>W"},
		{"<Actions>/Menu/Edit/Advanced/DelForwWord",		"<alt>D"},
	};

	text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(keybind.combo));

	if (!strcmp(text, _("Default"))) {
		menurc = default_menurc;
		n_menurc = G_N_ELEMENTS(default_menurc);
	} else if (!strcmp(text, "Mew / Wanderlust")) {
		menurc = mew_wl_menurc;
		n_menurc = G_N_ELEMENTS(mew_wl_menurc);
	} else if (!strcmp(text, "Mutt")) {
		menurc = mutt_menurc;
		n_menurc = G_N_ELEMENTS(mutt_menurc);
	} else {
		g_free(text);
		return;
	}
	g_free(text);

	prefs_keybind_apply(menurc, n_menurc);

	gtk_widget_destroy(keybind.window);
	keybind.window = NULL;
	keybind.combo = NULL;
}

static void prefs_other_create_widget(PrefsPage *_page, GtkWindow *window, 
			       	  gpointer data)
{
	OtherPage *prefs_other = (OtherPage *) _page;
	
	GtkWidget *vbox1;
	GtkWidget *hbox1;

	GtkWidget *frame_addr;
	GtkWidget *vbox_addr;
	GtkWidget *checkbtn_addaddrbyclick;
	
	GtkWidget *frame_exit;
	GtkWidget *vbox_exit;
	GtkWidget *checkbtn_confonexit;
	GtkWidget *checkbtn_cleanonexit;
	GtkWidget *checkbtn_warnqueued;

	GtkWidget *frame_keys;
	GtkWidget *vbox_keys;
	GtkWidget *checkbtn_gtk_can_change_accels;
	GtkWidget *button_keybind;

	GtkWidget *label_iotimeout;
	GtkWidget *spinbtn_iotimeout;
	GtkObject *spinbtn_iotimeout_adj;

	GtkWidget *vbox2;
	GtkWidget *checkbtn_askonclean;
	GtkWidget *checkbtn_askonfilter;
	GtkWidget *checkbtn_use_shred;
	GtkWidget *checkbtn_real_time_sync;

	GtkWidget *frame_metadata;
	GtkWidget *vbox_metadata;
	GtkWidget *metadata_label;
	GtkWidget *flush_metadata_faster_radiobtn;
	GtkWidget *flush_metadata_safer_radiobtn;

	gchar *shred_binary = NULL;
	CLAWS_TIP_DECL();

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	vbox_addr = gtkut_get_options_frame(vbox1, &frame_addr, _("Address book"));

	PACK_CHECK_BUTTON
		(vbox_addr, checkbtn_addaddrbyclick,
		 _("Add address to destination when double-clicked"));

	/* On Exit */
	vbox_exit = gtkut_get_options_frame(vbox1, &frame_exit, _("On exit"));

	PACK_CHECK_BUTTON (vbox_exit, checkbtn_confonexit,
			   _("Confirm on exit"));

	hbox1 = gtk_hbox_new (FALSE, 32);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox_exit), hbox1, FALSE, FALSE, 0);

	PACK_CHECK_BUTTON (hbox1, checkbtn_cleanonexit,
			   _("Empty trash on exit"));

	PACK_CHECK_BUTTON (vbox_exit, checkbtn_warnqueued,
			   _("Warn if there are queued messages"));

	vbox_keys = gtkut_get_options_frame(vbox1, &frame_keys, _("Keyboard shortcuts"));

	PACK_CHECK_BUTTON(vbox_keys, checkbtn_gtk_can_change_accels,
			_("Enable customisable keyboard shortcuts"));

	CLAWS_SET_TIP(checkbtn_gtk_can_change_accels,
			_("If checked, you can change the keyboard shortcuts of "
				"most of the menu items by focusing on the menu "
				"item and pressing a key combination.\n"
				"Uncheck this option if you want to lock all "
				"existing keyboard shortcuts."));

	button_keybind = gtk_button_new_with_label(
				_(" Choose preset keyboard shortcuts... "));
	gtk_widget_show (button_keybind);
	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox_keys), hbox1, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (hbox1), button_keybind, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (button_keybind), "clicked",
			  G_CALLBACK (prefs_keybind_select), NULL);


	vbox_metadata = gtkut_get_options_frame(vbox1, &frame_metadata, _("Metadata handling"));
	metadata_label = gtk_label_new(_("Safer mode asks the OS to write metadata to disk directly;\n"
					 "it avoids data loss after crashes but can take some time."));
	gtk_misc_set_alignment(GTK_MISC(metadata_label), 0, 0);
	gtk_box_pack_start (GTK_BOX (vbox_metadata), metadata_label, FALSE, FALSE, 0);
	flush_metadata_safer_radiobtn = gtk_radio_button_new_with_label(NULL, _("Safer"));
	flush_metadata_faster_radiobtn = gtk_radio_button_new_with_label_from_widget(
					   GTK_RADIO_BUTTON(flush_metadata_safer_radiobtn), _("Faster"));
	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox_metadata), hbox1, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (hbox1), flush_metadata_safer_radiobtn, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (hbox1), flush_metadata_faster_radiobtn, FALSE, FALSE, 0);
	
	if (prefs_common.flush_metadata)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(flush_metadata_safer_radiobtn), TRUE);
	else
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(flush_metadata_faster_radiobtn), TRUE);

	gtk_widget_show_all(frame_metadata);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, FALSE, 0);

	label_iotimeout = gtk_label_new (_("Socket I/O timeout"));
	gtk_widget_show (label_iotimeout);
	gtk_box_pack_start (GTK_BOX (hbox1), label_iotimeout, FALSE, FALSE, 0);

	spinbtn_iotimeout_adj = gtk_adjustment_new (60, 0, 1000, 1, 10, 0);
	spinbtn_iotimeout = gtk_spin_button_new
		(GTK_ADJUSTMENT (spinbtn_iotimeout_adj), 1, 0);
	gtk_widget_show (spinbtn_iotimeout);
	gtk_box_pack_start (GTK_BOX (hbox1), spinbtn_iotimeout,
			    FALSE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_iotimeout, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_iotimeout), TRUE);

	label_iotimeout = gtk_label_new (_("seconds"));
	gtk_widget_show (label_iotimeout);
	gtk_box_pack_start (GTK_BOX (hbox1), label_iotimeout, FALSE, FALSE, 0);

	vbox2 = gtk_vbox_new (FALSE, 8);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (vbox1), vbox2, FALSE, FALSE, 0);

	PACK_CHECK_BUTTON (vbox2, checkbtn_askonclean, 
			   _("Ask before emptying trash"));
	PACK_CHECK_BUTTON (vbox2, checkbtn_askonfilter,
			   _("Ask about account specific filtering rules when "
			     "filtering manually"));
	shred_binary = g_find_program_in_path("shred");
	if (shred_binary) {
		PACK_CHECK_BUTTON (vbox2, checkbtn_use_shred,
				   _("Use secure file deletion if possible"));
		g_free(shred_binary);
	} else {
		PACK_CHECK_BUTTON (vbox2, checkbtn_use_shred,
				   _("Use secure file deletion if possible\n"
				     "(the 'shred' program is not available)"));
		gtk_widget_set_sensitive(checkbtn_use_shred, FALSE);
	}
	CLAWS_SET_TIP(checkbtn_use_shred,
			_("Use the 'shred' program to overwrite files with random data before "
			  "deleting them. This slows down deletion. Be sure to "
			  "read shred's man page for caveats."));
	PACK_CHECK_BUTTON (vbox2, checkbtn_real_time_sync,
			   _("Synchronise offline folders as soon as possible"));

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_addaddrbyclick), 
		prefs_common.add_address_by_click);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_confonexit), 
		prefs_common.confirm_on_exit);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_cleanonexit), 
		prefs_common.clean_on_exit);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_askonclean), 
		prefs_common.ask_on_clean);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_warnqueued), 
		prefs_common.warn_queued_on_exit);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_gtk_can_change_accels),
		prefs_common.gtk_can_change_accels);

	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spinbtn_iotimeout),
		prefs_common.io_timeout_secs);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_askonfilter), 
		prefs_common.ask_apply_per_account_filtering_rules);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_use_shred), 
		prefs_common.use_shred);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_real_time_sync), 
		prefs_common.real_time_sync);

	prefs_other->checkbtn_addaddrbyclick = checkbtn_addaddrbyclick;
	prefs_other->checkbtn_confonexit = checkbtn_confonexit;
	prefs_other->checkbtn_cleanonexit = checkbtn_cleanonexit;
	prefs_other->checkbtn_askonclean = checkbtn_askonclean;
	prefs_other->checkbtn_warnqueued = checkbtn_warnqueued;
	prefs_other->spinbtn_iotimeout = spinbtn_iotimeout;
	prefs_other->checkbtn_gtk_can_change_accels = checkbtn_gtk_can_change_accels;
	prefs_other->checkbtn_askonfilter = checkbtn_askonfilter;
	prefs_other->checkbtn_use_shred = checkbtn_use_shred;
	prefs_other->checkbtn_real_time_sync = checkbtn_real_time_sync;
	prefs_other->flush_metadata_safer_radiobtn = flush_metadata_safer_radiobtn;
	prefs_other->flush_metadata_faster_radiobtn = flush_metadata_faster_radiobtn;
	prefs_other->page.widget = vbox1;
}

static void prefs_other_save(PrefsPage *_page)
{
	OtherPage *page = (OtherPage *) _page;
	gboolean gtk_can_change_accels;

	prefs_common.add_address_by_click = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_addaddrbyclick));
	prefs_common.confirm_on_exit = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_confonexit));
	prefs_common.clean_on_exit = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_cleanonexit)); 
	prefs_common.ask_on_clean = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_askonclean));
	prefs_common.warn_queued_on_exit = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_warnqueued)); 
	prefs_common.io_timeout_secs = gtk_spin_button_get_value_as_int(
		GTK_SPIN_BUTTON(page->spinbtn_iotimeout));
	prefs_common.flush_metadata = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->flush_metadata_safer_radiobtn));
	sock_set_io_timeout(prefs_common.io_timeout_secs);
#ifdef HAVE_LIBETPAN
	imap_main_set_timeout(prefs_common.io_timeout_secs);
#endif
	prefs_common.ask_apply_per_account_filtering_rules = 
		gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(page->checkbtn_askonfilter)); 
	prefs_common.use_shred = 
		gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(page->checkbtn_use_shred)); 
	prefs_common.real_time_sync = 
		gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(page->checkbtn_real_time_sync)); 

	gtk_can_change_accels = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_gtk_can_change_accels));

	if (prefs_common.gtk_can_change_accels != gtk_can_change_accels) {

		prefs_common.gtk_can_change_accels = gtk_can_change_accels;

		gtk_settings_set_long_property(gtk_settings_get_default(),
				"gtk-can-change-accels",
				(glong)prefs_common.gtk_can_change_accels,
				"XProperty");

		/* gtk_can_change_accels value changed : we have (only if changed)
		 * to apply the gtk property to all widgets : */
		gtk_rc_reparse_all_for_settings(gtk_settings_get_default(), TRUE);
	}
}

static void prefs_other_destroy_widget(PrefsPage *_page)
{
}

OtherPage *prefs_other;

void prefs_other_init(void)
{
	OtherPage *page;
	static gchar *path[3];

	path[0] = _("Other");
	path[1] = _("Miscellaneous");
	path[2] = NULL;

	page = g_new0(OtherPage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_other_create_widget;
	page->page.destroy_widget = prefs_other_destroy_widget;
	page->page.save_page = prefs_other_save;
	page->page.weight = 5.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_other = page;

	gtk_settings_set_long_property(gtk_settings_get_default(),
			"gtk-can-change-accels",
			(glong)prefs_common.gtk_can_change_accels,
			"XProperty");
}

void prefs_other_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_other);
	g_free(prefs_other);
}
