/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Tristan Chabredier <wwp@claws-mail.org> and 
 * the Claws Mail team
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

#include "prefs_common.h"
#include "prefs_gtk.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"
#include "gtk/menu.h"

#include "manage_window.h"

#include "log.h"
#include "combobox.h"

typedef struct _LoggingPage
{
	PrefsPage page;

	GtkWidget *window;

	GtkWidget *checkbtn_clip_network_log;
	GtkWidget *spinbtn_network_log_length;
	GtkWidget *checkbtn_log_standard;
	GtkWidget *checkbtn_log_warning;
	GtkWidget *checkbtn_log_error;
	GtkWidget *checkbtn_log_status;
#ifndef G_OS_WIN32
	GtkWidget *checkbtn_clip_filtering_log;
	GtkWidget *spinbtn_filtering_log_length;
	GtkWidget *checkbtn_filtering_log;
	GtkWidget *checkbtn_filtering_log_inc;
	GtkWidget *checkbtn_filtering_log_manual;
	GtkWidget *checkbtn_filtering_log_folder_proc;
	GtkWidget *checkbtn_filtering_log_pre_proc;
	GtkWidget *checkbtn_filtering_log_post_proc;
	GtkWidget *optmenu_filtering_log_level;
#endif
} LoggingPage;

static GtkWidget *prefs_logging_create_check_buttons(GtkWidget **checkbtn1,
			gchar *label1, GtkWidget **checkbtn2, gchar *label2)
{
	GtkWidget *hbox_checkbtn;

	hbox_checkbtn = gtk_hbox_new(FALSE, VBOX_BORDER);
	gtk_widget_show(hbox_checkbtn);
	
	PACK_CHECK_BUTTON (hbox_checkbtn, *checkbtn1, label1); 
	gtk_label_set_line_wrap(GTK_LABEL(gtk_bin_get_child(GTK_BIN((*checkbtn1)))), TRUE);

	PACK_CHECK_BUTTON (hbox_checkbtn, *checkbtn2, label2);
	gtk_label_set_line_wrap(GTK_LABEL(gtk_bin_get_child(GTK_BIN((*checkbtn2)))), TRUE);
	
	return hbox_checkbtn;
}

static void prefs_logging_create_widget(PrefsPage *_page, GtkWindow *window, 
			       	  gpointer data)
{
	LoggingPage *prefs_logging = (LoggingPage *) _page;
	
	GtkWidget *vbox1;

	GtkWidget *frame_logging;
	GtkWidget *vbox_network_log;
	GtkWidget *hbox_clip_network_log;
	GtkWidget *checkbtn_clip_network_log;
	GtkWidget *spinbtn_network_log_length;
	GtkAdjustment *spinbtn_network_log_length_adj;
	GtkWidget *hbox_checkbtn;
#ifndef G_OS_WIN32
	GtkWidget *vbox1_filtering_log;
	GtkWidget *hbox_clip_filtering_log;
	GtkWidget *checkbtn_clip_filtering_log;
	GtkWidget *spinbtn_filtering_log_length;
	GtkAdjustment *spinbtn_filtering_log_length_adj;
	GtkWidget *hbox_filtering_log;
	GtkWidget *checkbtn_filtering_log;
	GtkWidget *frame_filtering_log;
	GtkWidget *vbox2_filtering_log;
	GtkWidget *checkbtn_filtering_log_inc;
	GtkWidget *checkbtn_filtering_log_manual;
	GtkWidget *checkbtn_filtering_log_folder_proc;
	GtkWidget *checkbtn_filtering_log_pre_proc;
	GtkWidget *checkbtn_filtering_log_post_proc;
	GtkWidget *hbox_filtering_log_level;
	GtkWidget *label_filtering_log_level;
	GtkWidget *optmenu_filtering_log_level;
	GtkSizeGroup *filter_size_group;
	GtkListStore *menu;
	GtkTreeIter iter;
#endif
	GtkWidget *frame_disk_log;
	GtkWidget *vbox_disk_log;
	GtkWidget *label;
	GtkWidget *hbox;
	GtkWidget *checkbtn_log_standard;
	GtkWidget *checkbtn_log_warning;
	GtkWidget *checkbtn_log_error;
	GtkWidget *checkbtn_log_status;
	GtkSizeGroup *log_size_group;
	
	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	/* Protocol log */
	vbox_network_log = gtkut_get_options_frame(vbox1, &frame_logging, _("Network log"));

	hbox_clip_network_log = gtk_hbox_new (FALSE, 8);
	gtk_container_add (GTK_CONTAINER (vbox_network_log), hbox_clip_network_log);
	gtk_widget_show (hbox_clip_network_log);

	PACK_CHECK_BUTTON (hbox_clip_network_log, checkbtn_clip_network_log,
			   _("Restrict the log window to"));
	
	spinbtn_network_log_length_adj = GTK_ADJUSTMENT(gtk_adjustment_new (500, 0, G_MAXINT, 1, 10, 0));
	spinbtn_network_log_length = gtk_spin_button_new
		(GTK_ADJUSTMENT (spinbtn_network_log_length_adj), 1, 0);
	gtk_widget_show (spinbtn_network_log_length);
	gtk_box_pack_start (GTK_BOX (hbox_clip_network_log), spinbtn_network_log_length,
			    FALSE, FALSE, 0);
	gtk_widget_set_size_request (GTK_WIDGET (spinbtn_network_log_length), 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_network_log_length), TRUE);

	CLAWS_SET_TIP(spinbtn_network_log_length,
			     _("0 to stop logging in the log window"));

	label = gtk_label_new(_("lines"));
	gtk_widget_show (label);
  	gtk_box_pack_start(GTK_BOX(hbox_clip_network_log), label, FALSE, FALSE, 0);

	SET_TOGGLE_SENSITIVITY(checkbtn_clip_network_log, spinbtn_network_log_length);
	SET_TOGGLE_SENSITIVITY(checkbtn_clip_network_log, label);

#ifndef G_OS_WIN32
	/* Filtering/processing debug log */
	vbox1_filtering_log = gtkut_get_options_frame(vbox1,
				&frame_logging, _("Filtering/processing log"));

	PACK_CHECK_BUTTON (vbox1_filtering_log, checkbtn_filtering_log,
			   _("Enable logging of filtering/processing rules"));
	hbox_filtering_log = gtk_hbox_new (FALSE, 8);
	gtk_container_add (GTK_CONTAINER (vbox1_filtering_log), hbox_filtering_log);
	gtk_widget_show (hbox_filtering_log);

	CLAWS_SET_TIP(checkbtn_filtering_log,
			     _("If checked, turns on logging of filtering and processing rules.\n"
				"The log is accessible from 'Tools/Filtering log'.\n"
				"Caution: enabling this option will slow down the filtering/processing, "
				"this might be critical when applying many rules upon thousands of "
				"messages."));

	vbox2_filtering_log = gtkut_get_options_frame(vbox1_filtering_log, &frame_filtering_log,
							_("Log filtering/processing when..."));

	hbox_checkbtn = prefs_logging_create_check_buttons(
						&checkbtn_filtering_log_inc,
						_("filtering at incorporation"),
						&checkbtn_filtering_log_pre_proc,
						_("pre-processing folders"));
	gtk_box_pack_start(GTK_BOX(vbox2_filtering_log), hbox_checkbtn, FALSE, FALSE, 0);

	hbox_checkbtn = prefs_logging_create_check_buttons(
						&checkbtn_filtering_log_manual,
						_("manually filtering"),
						&checkbtn_filtering_log_post_proc,
						_("post-processing folders"));
	gtk_box_pack_start(GTK_BOX(vbox2_filtering_log), hbox_checkbtn, FALSE, FALSE, 0);
	
	hbox_checkbtn = gtk_hbox_new(TRUE, VBOX_BORDER);
	gtk_widget_show(hbox_checkbtn);
	gtk_box_pack_start(GTK_BOX(vbox2_filtering_log), hbox_checkbtn, FALSE, FALSE, 0);
	PACK_CHECK_BUTTON (hbox_checkbtn, checkbtn_filtering_log_folder_proc,
			   _("processing folders"));
	gtk_box_pack_start(GTK_BOX(hbox_checkbtn), gtk_label_new(""),
			   FALSE, TRUE, 0);

	filter_size_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget(filter_size_group, checkbtn_filtering_log_inc);
	gtk_size_group_add_widget(filter_size_group, checkbtn_filtering_log_manual);
	gtk_size_group_add_widget(filter_size_group, checkbtn_filtering_log_folder_proc);

	SET_TOGGLE_SENSITIVITY(checkbtn_filtering_log, frame_filtering_log);

	hbox_filtering_log_level = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox_filtering_log_level);
	gtk_box_pack_start(GTK_BOX (vbox1_filtering_log), hbox_filtering_log_level, FALSE, FALSE, 0);

	label_filtering_log_level = gtk_label_new (_("Log level"));
	gtk_widget_show (label_filtering_log_level);
	gtk_box_pack_start(GTK_BOX(hbox_filtering_log_level), label_filtering_log_level, FALSE, FALSE, 0);

 	optmenu_filtering_log_level = gtkut_sc_combobox_create(NULL, FALSE);
 	gtk_widget_show (optmenu_filtering_log_level);
 	
	menu = GTK_LIST_STORE(gtk_combo_box_get_model(
				GTK_COMBO_BOX(optmenu_filtering_log_level)));
	COMBOBOX_ADD (menu, _("Low"), 0);
	COMBOBOX_ADD (menu, _("Medium"), 1);
	COMBOBOX_ADD (menu, _("High"), 2);

	gtk_box_pack_start(GTK_BOX(hbox_filtering_log_level), optmenu_filtering_log_level, FALSE, FALSE, 0);

	CLAWS_SET_TIP(optmenu_filtering_log_level,
			     _("Select the level of detail of the logging.\n"
				"Choose Low to see when rules are applied, which "
				"conditions match or don't match and what actions are "
				"performed.\n"
				"Choose Medium to see more details about the message "
				"that is being processed, and why rules are skipped.\n"
				"Choose High to explicitly show the reason why all "
				"rules are processed or skipped, and why all conditions "
				"are matched or not matched.\n"
				"Caution: the higher the level, the greater the "
				"impact on performance."));

	hbox_clip_filtering_log = gtk_hbox_new (FALSE, 8);
	gtk_container_add (GTK_CONTAINER (vbox1_filtering_log), hbox_clip_filtering_log);
	gtk_widget_show (hbox_clip_filtering_log);
	PACK_CHECK_BUTTON (hbox_clip_filtering_log, checkbtn_clip_filtering_log,
			   _("Restrict the log window to"));
	
	spinbtn_filtering_log_length_adj = GTK_ADJUSTMENT(gtk_adjustment_new (500, 0, G_MAXINT, 1, 10, 0));
	spinbtn_filtering_log_length = gtk_spin_button_new
		(GTK_ADJUSTMENT (spinbtn_filtering_log_length_adj), 1, 0);
	gtk_widget_show (spinbtn_filtering_log_length);
	gtk_box_pack_start (GTK_BOX (hbox_clip_filtering_log), spinbtn_filtering_log_length,
			    FALSE, FALSE, 0);
	gtk_widget_set_size_request (GTK_WIDGET (spinbtn_filtering_log_length), 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_filtering_log_length), TRUE);

	CLAWS_SET_TIP(spinbtn_filtering_log_length,
			     _("0 to stop logging in the log window"));

	label = gtk_label_new(_("lines"));
	gtk_widget_show (label);
  	gtk_box_pack_start(GTK_BOX(hbox_clip_filtering_log), label, FALSE, FALSE, 0);

	SET_TOGGLE_SENSITIVITY(checkbtn_clip_filtering_log, spinbtn_filtering_log_length);
	SET_TOGGLE_SENSITIVITY(checkbtn_clip_filtering_log, label);
	SET_TOGGLE_SENSITIVITY(checkbtn_filtering_log, hbox_clip_filtering_log);
	SET_TOGGLE_SENSITIVITY(checkbtn_filtering_log, optmenu_filtering_log_level);
	SET_TOGGLE_SENSITIVITY(checkbtn_filtering_log, checkbtn_clip_filtering_log);
	SET_TOGGLE_SENSITIVITY(checkbtn_filtering_log, label_filtering_log_level);
#endif
	/* disk log */
	vbox_disk_log = gtkut_get_options_frame(vbox1, &frame_disk_log, _("Disk log"));

	label = gtk_label_new(_("Write the following information to disk..."));
	gtk_widget_show(label);
	hbox = gtk_hbox_new (FALSE, 8);
	gtk_container_add (GTK_CONTAINER (vbox_disk_log), hbox);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	gtk_widget_show (hbox);

	hbox_checkbtn = prefs_logging_create_check_buttons(&checkbtn_log_warning,
				_("Warning messages"), &checkbtn_log_standard,
				_("Network protocol messages"));
	gtk_box_pack_start(GTK_BOX(vbox_disk_log), hbox_checkbtn, FALSE, FALSE, 0);

	hbox_checkbtn = prefs_logging_create_check_buttons(&checkbtn_log_error,	
				_("Error messages"), &checkbtn_log_status,
				_("Status messages for filtering/processing log"));
	gtk_box_pack_start(GTK_BOX(vbox_disk_log), hbox_checkbtn, FALSE, FALSE, 0);
	
	log_size_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget(log_size_group, checkbtn_log_warning);
	gtk_size_group_add_widget(log_size_group, checkbtn_log_error);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_clip_network_log), 
		prefs_common.cliplog);
#ifndef G_OS_WIN32
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_clip_filtering_log), 
		prefs_common.filtering_debug_cliplog);
#endif
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_log_standard), 
		prefs_common.enable_log_standard);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_log_warning), 
		prefs_common.enable_log_warning);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_log_error), 
		prefs_common.enable_log_error);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_log_status), 
		prefs_common.enable_log_status);
#ifndef G_OS_WIN32
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_filtering_log), 
		prefs_common.enable_filtering_debug);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_filtering_log_inc), 
		prefs_common.enable_filtering_debug_inc);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_filtering_log_manual), 
		prefs_common.enable_filtering_debug_manual);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_filtering_log_folder_proc), 
		prefs_common.enable_filtering_debug_folder_proc);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_filtering_log_pre_proc), 
		prefs_common.enable_filtering_debug_pre_proc);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_filtering_log_post_proc), 
		prefs_common.enable_filtering_debug_post_proc);
#endif
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spinbtn_network_log_length),
		prefs_common.loglength);
#ifndef G_OS_WIN32
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spinbtn_filtering_log_length),
		prefs_common.filtering_debug_loglength);
	combobox_select_by_data(GTK_COMBO_BOX(optmenu_filtering_log_level),
			prefs_common.filtering_debug_level);
#endif

	prefs_logging->checkbtn_clip_network_log = checkbtn_clip_network_log;
	prefs_logging->spinbtn_network_log_length = spinbtn_network_log_length;
	prefs_logging->checkbtn_log_standard = checkbtn_log_standard;
	prefs_logging->checkbtn_log_warning = checkbtn_log_warning;
	prefs_logging->checkbtn_log_error = checkbtn_log_error;
	prefs_logging->checkbtn_log_status = checkbtn_log_status;
#ifndef G_OS_WIN32
	prefs_logging->checkbtn_clip_filtering_log = checkbtn_clip_filtering_log;
	prefs_logging->spinbtn_filtering_log_length = spinbtn_filtering_log_length;
	prefs_logging->checkbtn_filtering_log = checkbtn_filtering_log;
	prefs_logging->checkbtn_filtering_log_inc = checkbtn_filtering_log_inc;
	prefs_logging->checkbtn_filtering_log_manual = checkbtn_filtering_log_manual;
	prefs_logging->checkbtn_filtering_log_folder_proc = checkbtn_filtering_log_folder_proc;
	prefs_logging->checkbtn_filtering_log_pre_proc = checkbtn_filtering_log_pre_proc;
	prefs_logging->checkbtn_filtering_log_post_proc = checkbtn_filtering_log_post_proc;
	prefs_logging->optmenu_filtering_log_level = optmenu_filtering_log_level;
#endif
	prefs_logging->page.widget = vbox1;
}

static void prefs_logging_save(PrefsPage *_page)
{
	LoggingPage *page = (LoggingPage *) _page;
	MainWindow *mainwindow;
#ifndef G_OS_WIN32
	gboolean filtering_debug_enabled;
	prefs_common.filtering_debug_level =
		combobox_get_active_data(GTK_COMBO_BOX(page->optmenu_filtering_log_level));
#endif

	prefs_common.cliplog = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_clip_network_log));
	prefs_common.loglength = gtk_spin_button_get_value_as_int(
		GTK_SPIN_BUTTON(page->spinbtn_network_log_length));
	prefs_common.enable_log_standard = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_log_standard));
	prefs_common.enable_log_warning = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_log_warning));
	prefs_common.enable_log_error = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_log_error));
	prefs_common.enable_log_status = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_log_status));
#ifndef G_OS_WIN32
	prefs_common.filtering_debug_cliplog = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_clip_filtering_log));
	prefs_common.filtering_debug_loglength = gtk_spin_button_get_value_as_int(
		GTK_SPIN_BUTTON(page->spinbtn_filtering_log_length));
	filtering_debug_enabled = prefs_common.enable_filtering_debug;
	prefs_common.enable_filtering_debug = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_filtering_log));
	if (filtering_debug_enabled != prefs_common.enable_filtering_debug) {
		if (prefs_common.enable_filtering_debug)
			log_message(LOG_DEBUG_FILTERING, _("filtering log enabled\n"));
		else
			log_message(LOG_DEBUG_FILTERING, _("filtering log disabled\n"));
	}
	prefs_common.enable_filtering_debug_inc = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_filtering_log_inc));
	prefs_common.enable_filtering_debug_manual = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_filtering_log_manual));
	prefs_common.enable_filtering_debug_folder_proc = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_filtering_log_folder_proc));
	prefs_common.enable_filtering_debug_pre_proc = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_filtering_log_pre_proc));
	prefs_common.enable_filtering_debug_post_proc = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_filtering_log_post_proc));
#endif
	mainwindow = mainwindow_get_mainwindow();
	log_window_set_clipping(mainwindow->logwin, prefs_common.cliplog,
				prefs_common.loglength);
#ifndef G_OS_WIN32
	log_window_set_clipping(mainwindow->filtering_debugwin, prefs_common.filtering_debug_cliplog,
				prefs_common.filtering_debug_loglength);
#endif
}

static void prefs_logging_destroy_widget(PrefsPage *_page)
{
}

LoggingPage *prefs_logging;

void prefs_logging_init(void)
{
	LoggingPage *page;
	static gchar *path[3];

	path[0] = _("Other");
	path[1] = _("Logging");
	path[2] = NULL;

	page = g_new0(LoggingPage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_logging_create_widget;
	page->page.destroy_widget = prefs_logging_destroy_widget;
	page->page.save_page = prefs_logging_save;
	page->page.weight = 5.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_logging = page;
}

void prefs_logging_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_logging);
	g_free(prefs_logging);
}
