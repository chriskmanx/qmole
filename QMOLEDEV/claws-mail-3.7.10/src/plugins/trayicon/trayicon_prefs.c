/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2011 the Claws Mail Team
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
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
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

#include "trayicon_prefs.h"

TrayIconPrefs trayicon_prefs;

typedef struct _TrayIconPage TrayIconPage;

struct _TrayIconPage {
        PrefsPage page;
        GtkWidget *hide_at_startup;
		GtkWidget *close_to_tray;
		GtkWidget *hide_when_iconified;
};

static PrefParam param[] = {
        {"hide_at_startup", "FALSE", &trayicon_prefs.hide_at_startup, P_BOOL, NULL, NULL, NULL},
        {"close_to_tray", "FALSE", &trayicon_prefs.close_to_tray, P_BOOL, NULL, NULL, NULL},
        {"hide_when_iconified", "FALSE", &trayicon_prefs.hide_when_iconified, P_BOOL, NULL, NULL, NULL},
        {0,0,0,0,0,0,0}
};

static TrayIconPage prefs_page;

static void create_trayicon_prefs_page	(PrefsPage *page,
					 GtkWindow *window,
					 gpointer   data);
static void destroy_trayicon_prefs_page	(PrefsPage *page);
static void save_trayicon_prefs		(PrefsPage *page);

void trayicon_prefs_init(void)
{
	static gchar *path[3];
	gchar *rcpath;

	path[0] = _("Plugins");
	path[1] = _("Trayicon");
	path[2] = NULL;

        prefs_set_default(param);
	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, COMMON_RC, NULL);
        prefs_read_config(param, "TrayIcon", rcpath, NULL);
	g_free(rcpath);
        
        prefs_page.page.path = path;
        prefs_page.page.create_widget = create_trayicon_prefs_page;
        prefs_page.page.destroy_widget = destroy_trayicon_prefs_page;
        prefs_page.page.save_page = save_trayicon_prefs;

        prefs_gtk_register_page((PrefsPage *) &prefs_page);
}

void trayicon_prefs_done(void)
{
        prefs_gtk_unregister_page((PrefsPage *) &prefs_page);
}

static void create_trayicon_prefs_page(PrefsPage *page,
				    GtkWindow *window,
                                    gpointer data)
{
        TrayIconPage *prefs_page = (TrayIconPage *) page;

        GtkWidget *vbox;
        GtkWidget *hide_at_startup_checkbox;
        GtkWidget *close_to_tray_checkbox;
        GtkWidget *hide_when_iconified_checkbox;
	CLAWS_TIP_DECL();

        vbox = gtk_vbox_new(FALSE, 3);
        gtk_container_set_border_width(GTK_CONTAINER(vbox), VBOX_BORDER);
        gtk_widget_show(vbox);
        
        hide_at_startup_checkbox = gtk_check_button_new_with_label
				(_("Hide at start-up"));
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(hide_at_startup_checkbox),
                                     trayicon_prefs.hide_at_startup);
        gtk_box_pack_start(GTK_BOX(vbox), hide_at_startup_checkbox, FALSE, FALSE, 0);
        gtk_widget_show(hide_at_startup_checkbox);
	CLAWS_SET_TIP(hide_at_startup_checkbox,
			     _("Hide Claws Mail at start-up"));
        
        close_to_tray_checkbox = gtk_check_button_new_with_label
				(_("Close to tray"));
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(close_to_tray_checkbox),
                                     trayicon_prefs.close_to_tray);
        gtk_box_pack_start(GTK_BOX(vbox), close_to_tray_checkbox, FALSE, FALSE, 0);
        gtk_widget_show(close_to_tray_checkbox);
	CLAWS_SET_TIP(close_to_tray_checkbox,
			     _("Hide Claws Mail using the tray icon instead of closing it\nwhen the window close button is clicked"));

        hide_when_iconified_checkbox = gtk_check_button_new_with_label
				(_("Minimize to tray"));
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(hide_when_iconified_checkbox),
                                     trayicon_prefs.hide_when_iconified);
        gtk_box_pack_start(GTK_BOX(vbox), hide_when_iconified_checkbox, FALSE, FALSE, 0);
        gtk_widget_show(hide_when_iconified_checkbox);
	CLAWS_SET_TIP(hide_when_iconified_checkbox,
			     _("Hide Claws Mail using the tray icon instead of minimizing it"));

        prefs_page->hide_at_startup = hide_at_startup_checkbox;
        prefs_page->close_to_tray = close_to_tray_checkbox;
        prefs_page->hide_when_iconified = hide_when_iconified_checkbox;
        prefs_page->page.widget = vbox;
}

static void destroy_trayicon_prefs_page(PrefsPage *page)
{
	/* Do nothing! */
}

static void save_trayicon_prefs(PrefsPage *page)
{
        TrayIconPage *prefs_page = (TrayIconPage *) page;
        PrefFile *pref_file;
        gchar *rc_file_path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
                                          COMMON_RC, NULL);
        
        trayicon_prefs.hide_at_startup = gtk_toggle_button_get_active
				(GTK_TOGGLE_BUTTON(prefs_page->hide_at_startup));
        trayicon_prefs.close_to_tray = gtk_toggle_button_get_active
				(GTK_TOGGLE_BUTTON(prefs_page->close_to_tray));
        trayicon_prefs.hide_when_iconified = gtk_toggle_button_get_active
				(GTK_TOGGLE_BUTTON(prefs_page->hide_when_iconified));
        
        pref_file = prefs_write_open(rc_file_path);
        g_free(rc_file_path);
        
        if (!(pref_file) ||
	    (prefs_set_block_label(pref_file, "TrayIcon") < 0))
          return;
        
        if (prefs_write_param(param, pref_file->fp) < 0) {
          g_warning("failed to write TrayIcon Plugin configuration\n");
          prefs_file_close_revert(pref_file);
          return;
        }
        if (fprintf(pref_file->fp, "\n") < 0) {
		FILE_OP_ERROR(rc_file_path, "fprintf");
		prefs_file_close_revert(pref_file);
	} else
	        prefs_file_close(pref_file);
}
