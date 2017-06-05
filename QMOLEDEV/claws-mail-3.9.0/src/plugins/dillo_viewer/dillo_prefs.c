/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 the Claws Mail Team
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

/*
 * The structure of this file has been borrowed from the structure of
 * the image_viewer plugin file. I also used it as an example of how to
 * build the preferences for the dillo plugin.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "gtkutils.h"
#include "utils.h"
#include "prefs.h"
#include "prefs_gtk.h"
#include "prefswindow.h"
#include "combobox.h"
#include "addressbook.h"

#include "dillo_prefs.h"

#define PREFS_BLOCK_NAME "Dillo"

DilloBrowserPrefs dillo_prefs;

typedef struct _DilloBrowserPage DilloBrowserPage;

struct _DilloBrowserPage {
        PrefsPage page;
        GtkWidget *local;
	GtkWidget *whitelist_ab;
	GtkWidget *whitelist_ab_folder_combo;
	GtkWidget *whitelist_ab_select_btn;
        GtkWidget *full;
};

static PrefParam param[] = {
        {"local_browse", "TRUE", &dillo_prefs.local, P_BOOL, NULL, NULL, NULL},
        {"full_window", "TRUE", &dillo_prefs.full, P_BOOL, NULL, NULL, NULL},
	{"whitelist_ab", "FALSE", &dillo_prefs.whitelist_ab, P_BOOL,
	 NULL, NULL, NULL},
	{"whitelist_ab_folder", N_("Any"), &dillo_prefs.whitelist_ab_folder, P_STRING,
	 NULL, NULL, NULL},

        {0,0,0,0,0,0,0}
};

static DilloBrowserPage prefs_page;

static void create_dillo_prefs_page	(PrefsPage *page,
					 GtkWindow *window,
					 gpointer   data);
static void destroy_dillo_prefs_page	(PrefsPage *page);
static void save_dillo_prefs		(PrefsPage *page);

#ifndef USE_NEW_ADDRBOOK
static void dillo_whitelist_ab_select_cb(GtkWidget *widget, gpointer data)
{
	DilloBrowserPage *page = (DilloBrowserPage *) data;
	const gchar *folderpath = NULL;
	gchar *new_path = NULL;

	folderpath = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((page->whitelist_ab_folder_combo)))));
	new_path = addressbook_folder_selection(folderpath);
	if (new_path) {
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((page->whitelist_ab_folder_combo)))), new_path);
		g_free(new_path);
	} 
}
#endif

static void local_checkbox_toggled(GtkToggleButton *button,
					  gpointer user_data)
{
	gboolean active = gtk_toggle_button_get_active(button);
        DilloBrowserPage *prefs_page = (DilloBrowserPage *) user_data;

	gtk_widget_set_sensitive(prefs_page->whitelist_ab, active);
	gtk_widget_set_sensitive(prefs_page->whitelist_ab_folder_combo, active && 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prefs_page->whitelist_ab)));
#ifndef USE_NEW_ADDRBOOK
	gtk_widget_set_sensitive(prefs_page->whitelist_ab_select_btn, active && 
			gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prefs_page->whitelist_ab)));
#endif
}

static void whitelist_checkbox_toggled(GtkToggleButton *button,
					  gpointer user_data)
{
	gboolean active = gtk_toggle_button_get_active(button);
        DilloBrowserPage *prefs_page = (DilloBrowserPage *) user_data;

	active &= gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prefs_page->local));

	gtk_widget_set_sensitive(prefs_page->whitelist_ab_folder_combo, active);
	gtk_widget_set_sensitive(prefs_page->whitelist_ab_select_btn, active);
}

void dillo_prefs_init(void)
{
	static gchar *path[3];
	gchar *rcpath;

	path[0] = _("Plugins");
	path[1] = _("Dillo Browser");
	path[2] = NULL;

        prefs_set_default(param);
	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, COMMON_RC, NULL);
        prefs_read_config(param, PREFS_BLOCK_NAME, rcpath, NULL);
	g_free(rcpath);
        
        prefs_page.page.path = path;
        prefs_page.page.create_widget = create_dillo_prefs_page;
        prefs_page.page.destroy_widget = destroy_dillo_prefs_page;
        prefs_page.page.save_page = save_dillo_prefs;
		prefs_page.page.weight = 35.0;
        prefs_gtk_register_page((PrefsPage *) &prefs_page);
}

void dillo_prefs_done(void)
{
        prefs_gtk_unregister_page((PrefsPage *) &prefs_page);
}

static void create_dillo_prefs_page(PrefsPage *page,
				    GtkWindow *window,
                                    gpointer data)
{
        DilloBrowserPage *prefs_page = (DilloBrowserPage *) page;

        GtkWidget *vbox;
        GtkWidget *local_checkbox;
        GtkWidget *full_checkbox;
        GtkWidget *label;
	GtkWidget *whitelist_ab_checkbtn;
	GtkWidget *whitelist_ab_folder_combo;
	GtkWidget *whitelist_ab_select_btn;
	GtkWidget *hbox_whitelist, *spacer;

        vbox = gtk_vbox_new(FALSE, 3);
        gtk_container_set_border_width(GTK_CONTAINER(vbox), VBOX_BORDER);
        gtk_widget_show(vbox);
        
        local_checkbox = gtk_check_button_new_with_label
				(_("Load remote links in mails"));
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(local_checkbox),
                                     !dillo_prefs.local);
        gtk_box_pack_start(GTK_BOX(vbox), local_checkbox, FALSE, FALSE, 0);
        gtk_widget_show(local_checkbox);
	CLAWS_SET_TIP(local_checkbox,
			     _("Equivalent to Dillo's '--local' option"));
        
	label = gtk_label_new(_("You can still load remote links "
			      "by reloading the page"));
        gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtkut_widget_set_small_font_size (label);
        gtk_widget_show(label);

	
	hbox_whitelist = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox_whitelist);
	gtk_box_pack_start (GTK_BOX (vbox), hbox_whitelist, FALSE, FALSE, 0);
	
	spacer = gtk_hbox_new(FALSE, 0);
	gtk_widget_set_size_request(spacer, 12, -1);
	gtk_widget_show(spacer);
	gtk_box_pack_start(GTK_BOX(hbox_whitelist), spacer, FALSE, FALSE, 0);

	whitelist_ab_checkbtn = gtk_check_button_new_with_label(_("Only for senders found in address book/folder"));
	gtk_widget_show(whitelist_ab_checkbtn);
	gtk_box_pack_start(GTK_BOX(hbox_whitelist), whitelist_ab_checkbtn, FALSE, FALSE, 0);

	whitelist_ab_folder_combo = combobox_text_new(TRUE, _("Any"), NULL);
	gtk_widget_set_size_request(whitelist_ab_folder_combo, 100, -1);
	gtk_box_pack_start (GTK_BOX (hbox_whitelist), whitelist_ab_folder_combo, TRUE, TRUE, 0);

	whitelist_ab_select_btn = gtk_button_new_with_label(_("Select ..."));
	gtk_widget_show (whitelist_ab_select_btn);
	gtk_box_pack_start (GTK_BOX (hbox_whitelist), whitelist_ab_select_btn, FALSE, FALSE, 0);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(whitelist_ab_checkbtn), dillo_prefs.whitelist_ab);
	if (dillo_prefs.whitelist_ab_folder != NULL) {
		/* translate "Any" (stored UNtranslated) */
		if (strcasecmp(dillo_prefs.whitelist_ab_folder, "Any") == 0)
			gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((whitelist_ab_folder_combo)))),
					_("Any"));
		else
		/* backward compatibility (when translated "Any" was stored) */
		if (g_utf8_collate(dillo_prefs.whitelist_ab_folder, _("Any")) == 0)
			gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((whitelist_ab_folder_combo)))),
					dillo_prefs.whitelist_ab_folder);
		else
			gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((whitelist_ab_folder_combo)))),
					dillo_prefs.whitelist_ab_folder);
	}

        full_checkbox = gtk_check_button_new_with_label
				(_("Full window mode (hide controls)"));
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(full_checkbox),
                                      dillo_prefs.full);
        gtk_box_pack_start(GTK_BOX(vbox), full_checkbox, FALSE, FALSE, 0);
        gtk_widget_show(full_checkbox);
	CLAWS_SET_TIP(full_checkbox,
			     _("Equivalent to Dillo's '--fullwindow' option"));

	g_signal_connect(G_OBJECT(local_checkbox), "toggled",
			 G_CALLBACK(local_checkbox_toggled),
			 prefs_page);

	g_signal_connect(G_OBJECT(whitelist_ab_checkbtn), "toggled",
			 G_CALLBACK(whitelist_checkbox_toggled),
			 prefs_page);

#ifndef USE_NEW_ADDRBOOK
	g_signal_connect(G_OBJECT (whitelist_ab_select_btn), "clicked",
			 G_CALLBACK(dillo_whitelist_ab_select_cb), prefs_page);
#else
	gtk_widget_set_sensitive(GTK_WIDGET(whitelist_ab_select_btn), FALSE);
#endif
	gtk_widget_set_sensitive(whitelist_ab_checkbtn, !dillo_prefs.local);
	gtk_widget_set_sensitive(whitelist_ab_folder_combo, !dillo_prefs.local && dillo_prefs.whitelist_ab);
	gtk_widget_set_sensitive(whitelist_ab_select_btn, !dillo_prefs.local && dillo_prefs.whitelist_ab);

        
        prefs_page->local = local_checkbox;
        prefs_page->full = full_checkbox;
	prefs_page->whitelist_ab = whitelist_ab_checkbtn;
	prefs_page->whitelist_ab_folder_combo = whitelist_ab_folder_combo;
	prefs_page->whitelist_ab_select_btn = whitelist_ab_select_btn;
        prefs_page->page.widget = vbox;
}

static void destroy_dillo_prefs_page(PrefsPage *page)
{
	/* Do nothing! */
}

static void save_dillo_prefs(PrefsPage *page)
{
        DilloBrowserPage *prefs_page = (DilloBrowserPage *) page;
        PrefFile *pref_file;
        gchar *rc_file_path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
                                          COMMON_RC, NULL);
        
        dillo_prefs.local = !gtk_toggle_button_get_active
				(GTK_TOGGLE_BUTTON(prefs_page->local));
        dillo_prefs.full = gtk_toggle_button_get_active
				(GTK_TOGGLE_BUTTON(prefs_page->full));

	dillo_prefs.whitelist_ab = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prefs_page->whitelist_ab));
	g_free(dillo_prefs.whitelist_ab_folder);
	dillo_prefs.whitelist_ab_folder = gtk_editable_get_chars(
				GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((prefs_page->whitelist_ab_folder_combo)))), 0, -1);
	/* store UNtranslated "Any" */
	if (g_utf8_collate(dillo_prefs.whitelist_ab_folder, _("Any")) == 0) {
		g_free(dillo_prefs.whitelist_ab_folder);
		dillo_prefs.whitelist_ab_folder = g_strdup("Any");
	}
    
        pref_file = prefs_write_open(rc_file_path);
        g_free(rc_file_path);
        
        if (!(pref_file) ||
	    (prefs_set_block_label(pref_file, PREFS_BLOCK_NAME) < 0))
          return;
        
        if (prefs_write_param(param, pref_file->fp) < 0) {
          g_warning("failed to write Dillo Plugin configuration\n");
          prefs_file_close_revert(pref_file);
          return;
        }

        if (fprintf(pref_file->fp, "\n") < 0) {
		FILE_OP_ERROR(rc_file_path, "fprintf");
		prefs_file_close_revert(pref_file);
	} else
	        prefs_file_close(pref_file);
}
