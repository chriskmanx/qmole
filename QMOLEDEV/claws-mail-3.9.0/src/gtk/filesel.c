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
#include "config.h"
#include "claws-features.h"
#endif 

#include <glib.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-file-chooser-dialog.h>
#else
#include <hildon-widgets/hildon-file-chooser-dialog.h>
#endif
#endif

#include "claws.h"
#include "filesel.h"
#include "manage_window.h"
#include "gtkutils.h"
#include "utils.h"
#include "codeconv.h"
#include "procmime.h"
#include "prefs_common.h"

static void
update_preview_cb (GtkFileChooser *file_chooser, gpointer data)
{
	GtkWidget *preview;
	char *filename = NULL, *type = NULL;
	GdkPixbuf *pixbuf = NULL;
	gboolean have_preview = FALSE;

	preview = GTK_WIDGET (data);
	filename = gtk_file_chooser_get_preview_filename (file_chooser);

	if (filename == NULL) {
		gtk_file_chooser_set_preview_widget_active (file_chooser, FALSE);
		return;
	}
	type = procmime_get_mime_type(filename);
	
	if (type && !strncmp(type, "image/", 6)) 
		pixbuf = gdk_pixbuf_new_from_file_at_size (filename, 128, 128, NULL);
	
	g_free(type);
	g_free (filename);

	if (pixbuf) {
		have_preview = TRUE;
		gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
		g_object_unref(G_OBJECT(pixbuf));
	}

	gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
}
	
static GList *filesel_create(const gchar *title, const gchar *path,
			     gboolean multiple_files,
			     gboolean open, gboolean folder_mode,
			     const gchar *filter)
{
	GSList *slist = NULL, *slist_orig = NULL;
	GList *list = NULL;

	gint action = (open == TRUE) ? 
			(folder_mode == TRUE ? GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER:
					       GTK_FILE_CHOOSER_ACTION_OPEN):
			(folder_mode == TRUE ? GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER:
					       GTK_FILE_CHOOSER_ACTION_SAVE);
			
	gchar * action_btn = (open == TRUE) ? GTK_STOCK_OPEN:GTK_STOCK_SAVE;
#ifdef MAEMO
	GtkWidget *chooser;
	if( path && strcmp(path, get_plugin_dir()) == 0 ) {
#if !GTK_CHECK_VERSION(2,14,0)
		chooser = gtk_file_chooser_dialog_new_with_backend
					(title, NULL, action, "gtk+",
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					action_btn, GTK_RESPONSE_ACCEPT, 
					NULL);
#else
		chooser = gtk_file_chooser_dialog_new
					(title, NULL, action,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					action_btn, GTK_RESPONSE_ACCEPT, 
					NULL);
#endif
	}
	else {
		chooser = hildon_file_chooser_dialog_new (NULL, action);
	}
#else
#if !GTK_CHECK_VERSION(2,14,0)
	GtkWidget *chooser = gtk_file_chooser_dialog_new_with_backend
				(title, NULL, action, "gtk+",
				GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				action_btn, GTK_RESPONSE_ACCEPT, 
				NULL);
#else
	GtkWidget *chooser = gtk_file_chooser_dialog_new
				(title, NULL, action, 
				GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				action_btn, GTK_RESPONSE_ACCEPT, 
				NULL);
#endif
#endif

#if GLIB_CHECK_VERSION(2,16,0)
	gtk_file_chooser_set_local_only(GTK_FILE_CHOOSER(chooser), FALSE);
#endif

	if (filter != NULL) {
		GtkFileFilter *file_filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(file_filter, filter);
		gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(chooser),
					    file_filter);
	}

	if (action == GTK_FILE_CHOOSER_ACTION_OPEN) {
		GtkImage *preview;
		preview = GTK_IMAGE(gtk_image_new ());
		gtk_file_chooser_set_preview_widget (GTK_FILE_CHOOSER(chooser), GTK_WIDGET(preview));
		g_signal_connect (chooser, "update-preview",
			    G_CALLBACK (update_preview_cb), preview);

	}

	if (action == GTK_FILE_CHOOSER_ACTION_SAVE) {
		gtk_dialog_set_default_response(GTK_DIALOG(chooser), GTK_RESPONSE_ACCEPT);
	}

	manage_window_set_transient (GTK_WINDOW(chooser));
	gtk_window_set_modal(GTK_WINDOW(chooser), TRUE);

	gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER(chooser), multiple_files);

	if (path && strlen(path) > 0) {
		char *filename = NULL;
		char *realpath = g_strdup(path);
		char *tmp = NULL;
		if (path[strlen(path)-1] == G_DIR_SEPARATOR) {
			filename = "";
		} else if ((filename = strrchr(path, G_DIR_SEPARATOR)) != NULL) {
			filename++;
			*(strrchr(realpath, G_DIR_SEPARATOR)+1) = '\0';
		} else {
			filename = (char *) path;
			g_free(realpath); 
			realpath = g_strdup(get_home_dir());
		}
		if (g_utf8_validate(realpath, -1, NULL))
			tmp = g_filename_from_utf8(realpath, -1, NULL, NULL, NULL);
		if (tmp == NULL)
			tmp = g_strdup(realpath);
		gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(chooser), tmp);
		g_free(tmp);
		if (action == GTK_FILE_CHOOSER_ACTION_SAVE) {
			if (g_utf8_validate(filename, -1, NULL))
				gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(chooser),
								  filename);
		}
		g_free(realpath);
	} else {
		gchar *tmp = NULL;
		if (!prefs_common.attach_load_dir || !*prefs_common.attach_load_dir)
			prefs_common.attach_load_dir = g_strdup_printf("%s%c", get_home_dir(), G_DIR_SEPARATOR);
		if (g_utf8_validate(prefs_common.attach_load_dir, -1, NULL))
			tmp = g_filename_from_utf8(prefs_common.attach_load_dir, -1, NULL, NULL, NULL);
		if (tmp == NULL)
			tmp = g_strdup(prefs_common.attach_load_dir);
		gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(chooser), tmp);
		g_free(tmp);
	}

#ifdef MAEMO
        if (gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_OK 
		|| gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_ACCEPT)
#else
	if (gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_ACCEPT) 
#endif
		slist = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (chooser));
	
	manage_window_focus_out(chooser, NULL, NULL);
	gtk_widget_destroy (chooser);

	slist_orig = slist;
	
	if (slist) {
		gchar *tmp = g_strdup(slist->data);

		if (!path && prefs_common.attach_load_dir)
			g_free(prefs_common.attach_load_dir);
		
		if (strrchr(tmp, G_DIR_SEPARATOR))
			*(strrchr(tmp, G_DIR_SEPARATOR)+1) = '\0';

		if (!path)
			prefs_common.attach_load_dir = g_filename_to_utf8(tmp, -1, NULL, NULL, NULL);

		g_free(tmp);
	}

	while (slist) {
		list = g_list_append(list, slist->data);
		slist = slist->next;
	}
	
	if (slist_orig)
		g_slist_free(slist_orig);
	
	return list;
}

/**
 * This function lets the user select multiple files.
 * This opens an Open type dialog.
 * @param title the title of the dialog
 */
GList *filesel_select_multiple_files_open(const gchar *title)
{
	return filesel_create(title, NULL, TRUE, TRUE, FALSE, NULL);
}

GList *filesel_select_multiple_files_open_with_filter(	const gchar *title,
							const gchar *path,
							const gchar *filter)
{
	return filesel_create (title, path, TRUE, TRUE, FALSE, filter);
}

/**
 * This function lets the user select one file.
 * This opens an Open type dialog if "file" is NULL, 
 * Save dialog if "file" contains a path.
 * @param title the title of the dialog
 * @param path the optional path to save to
 */
static gchar *filesel_select_file(const gchar *title, const gchar *path,
				  gboolean open, gboolean folder_mode,
				  const gchar *filter)
{
	GList * list = filesel_create(title, path, FALSE, open, folder_mode, filter);
	gchar * result = NULL;
	if (list) {
		result = g_strdup(list->data);
	}
	g_list_free(list);
	return result;
}
gchar *filesel_select_file_open(const gchar *title, const gchar *path)
{
	return filesel_select_file (title, path, TRUE, FALSE, NULL);
}

gchar *filesel_select_file_open_with_filter(const gchar *title, const gchar *path,
					    const gchar *filter)
{
	return filesel_select_file (title, path, TRUE, FALSE, filter);
}

gchar *filesel_select_file_save(const gchar *title, const gchar *path)
{
	return filesel_select_file (title, path, FALSE, FALSE, NULL);
}

gchar *filesel_select_file_open_folder(const gchar *title, const gchar *path)
{
	return filesel_select_file (title, path, TRUE, TRUE, NULL);
}

gchar *filesel_select_file_save_folder(const gchar *title, const gchar *path)
{
	return filesel_select_file (title, path, FALSE, TRUE, NULL);
}

