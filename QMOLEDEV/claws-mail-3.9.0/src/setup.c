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

#include "inputdialog.h"
#include "alertpanel.h"
#include "mainwindow.h"
#include "gtkutils.h"
#include "mh.h"
#include "wizard.h"
#define SETUP_DIALOG_WIDTH	540

static void scan_tree_func(Folder *folder, FolderItem *item, gpointer data);

gboolean setup_write_mailbox_path(MainWindow *mainwin, const gchar *path)
{
	Folder *folder;
	gchar *base;

	if (!path) return FALSE;
	if (folder_find_from_path(path)) {
		g_warning("The mailbox already exists.\n");
		return FALSE;
	}

	base = g_path_get_basename(path);
	folder = folder_new(mh_get_class(), !strcmp(path, "Mail") ? _("Mailbox") : base, path);

	if (folder->klass->create_tree(folder) < 0) {
		alertpanel_error(_("Creation of the mailbox failed.\n"
				   "Maybe some files already exist, or you don't have the permission to write there."));
		folder_destroy(folder);
		g_free(base);
		return FALSE;
	}

	folder_add(folder);
	folder_set_ui_func(folder, scan_tree_func, mainwin);
	folder_scan_tree(folder, TRUE);
	folder_set_ui_func(folder, NULL, NULL);
	g_free(base);
	return TRUE;
}

void setup(MainWindow *mainwin)
{
	gchar *path;
	
	path = input_dialog
		(_("Mailbox setting"),
		 _("First, you have to set the location of mailbox.\n"
		   "You can use existing mailbox in MH format\n"
		   "if you have the one.\n"
		   "If you're not sure, just select OK."),
		 "Mail");
	setup_write_mailbox_path(mainwin, path);
	g_free(path);
}

static void scan_tree_func(Folder *folder, FolderItem *item, gpointer data)
{
	MainWindow *mainwin = (MainWindow *)data;
	gchar *str;

	if (item->path)
		str = g_strdup_printf(_("Scanning folder %s%c%s ..."),
				      LOCAL_FOLDER(folder)->rootpath,
				      G_DIR_SEPARATOR,
				      item->path);
	else
		str = g_strdup_printf(_("Scanning folder %s ..."),
				      LOCAL_FOLDER(folder)->rootpath);

	if (mainwin->statusbar)
		gtk_statusbar_push(GTK_STATUSBAR(mainwin->statusbar),
			   mainwin->mainwin_cid, str);
	if (mainwin->hbox_stat)
		gtkut_widget_draw_now(mainwin->hbox_stat);
	
	if (mainwin->statusbar)
		gtk_statusbar_pop(GTK_STATUSBAR(mainwin->statusbar),
			  mainwin->mainwin_cid);
	g_free(str);
}
