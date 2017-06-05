/*
 * Copyright 2014 Vincent Sanders <vince@netsurf-browser.org>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef GTK_GUI_H
#define GTK_GUI_H

#include <inttypes.h>
#include <stdbool.h>
#include <gtk/gtk.h>

#include "utils/nsurl.h"

/** glade file paths. */
struct glade_file_location_s {
	char *netsurf;
	char *tabcontents;
	char *password;
	char *warning;
	char *login;
	char *ssl;
	char *toolbar;
	char *options;
	char *downloads;
	char *history;
	char *hotlist;
	char *cookies;
	char *viewdata;
};

/** location of all glade files. */
extern struct glade_file_location_s *glade_file_location;

/** language list file path. */
extern char *languages_file_location;

/** toolbar arrangement file path. */
extern char *toolbar_indices_file_location;

/** Resource directory path. */
extern char *res_dir_location;

/** Theme location. */
extern char *themelist_file_location;

/** Directory where all configuration files are held. */
extern char *nsgtk_config_home;

/** favicon default pixbuf */
extern GdkPixbuf *favicon_pixbuf;

/** resource search path vector */
extern char **respaths;

/** input conversion. */
uint32_t gtk_gui_gdkkey_to_nskey(GdkEventKey *eventkey);

/** login window request. */
extern void gui_401login_open(nsurl *url, const char *realm, nserror (*cb)(bool proceed, void *pw), void *cbpw);

/** set when no windows remain open. */
extern bool nsgtk_complete;

#endif /* GTK_GUI_H */
