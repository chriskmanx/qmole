/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2007, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* logging.c - the action logger */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <gtk/gtk.h>

#include "global.h"

#include "log.h"
#include "main.h"
#include "gui_support.h"

static GtkTreeStore *log;

/* The columns in the log list store */
#define TIMESTAMP 0
#define DIRECTORY 1
#define MESSAGE 2

/* Static prototypes */
static void log_dialog_response(GtkDialog *dialog, gint resp_id,
				gpointer udata);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void log_init()
{
	log = gtk_tree_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

	log_info_paths(_("ROX-Filer started"), NULL, NULL);

#if 0
	GList *paths = NULL;

	paths = g_list_prepend(paths, "/first");
	paths = g_list_prepend(paths, "/second");

	log_info_paths("Test path", NULL, "/the/path");
	log_info_paths("Test paths", paths, NULL);
	log_info_paths("Test both", paths, "/other/path");
	log_info_paths("Test one", paths->next, "/other/path");
	log_info_paths("Test promote", paths->next, NULL);
#endif
}

/* Record a message in the log.
 * paths is the list of items being processed, if any
 * path is a single path (if only one is being processed)
 * paths and path may both be given (e.g. for copying or moving)
 */
void log_info_paths(const gchar *message, GList *paths, const gchar *path)
{
	GtkTreeIter iter;
	char timestamp[32];
	time_t t;
	struct tm *now;
	char *actual_message = NULL;
	int n_paths;

	if (!message)
		message = "(no log message!)";

	t = time(NULL);
	now = localtime(&t);

	if (now == NULL || !strftime(timestamp, sizeof(timestamp), "%Y-%m-%d %H:%M", now))
	{
		g_warning("Failed to generate timestamp!");
		strcpy(timestamp, "ERROR");
	}

	gtk_tree_store_append(log, &iter, NULL);

	n_paths = g_list_length(paths);

	if (path == NULL && n_paths == 1)
	{
		/* Promote the single item to the main path */
		path = paths->data;
		paths = NULL;
		n_paths = 0;
	}

	if (n_paths == 1)
		actual_message = g_strdup_printf(_("%s '%s'"), message, g_basename((char *) paths->data));
	else if (n_paths > 1)
		actual_message = g_strdup_printf(_("%s on %d items"), message, n_paths);

	gtk_tree_store_set(log, &iter,
			TIMESTAMP, timestamp,
			DIRECTORY, path,
			MESSAGE, actual_message ? actual_message : message,
			-1);

	while (paths)
	{
		GtkTreeIter child_iter;
		gtk_tree_store_append(log, &child_iter, &iter);
		gtk_tree_store_set(log, &child_iter,
				MESSAGE, _("Item"),
				DIRECTORY, paths->data,
				-1);
		paths = paths->next;
	}

	g_free(actual_message);
}

/* Open the log window. */
void log_show_window()
{
	GtkWidget *dialog;
	GtkBuilder *builder;
	GtkTreeView *tv;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;
	gchar *ids[] = {"Log viewer", NULL};

	builder = get_gtk_builder(&ids);

	tv = GTK_TREE_VIEW(gtk_builder_get_object(builder, "log_list"));
	gtk_tree_view_set_model(tv, GTK_TREE_MODEL(log));

	renderer = gtk_cell_renderer_text_new();

	column = gtk_tree_view_column_new_with_attributes(_("Time"), renderer,
							   "text", TIMESTAMP,
							   NULL);
	gtk_tree_view_append_column(tv, column);

	column = gtk_tree_view_column_new_with_attributes(_("Action"), renderer,
							   "text", MESSAGE,
							   NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_append_column(tv, column);

	column = gtk_tree_view_column_new_with_attributes(_("Path"), renderer,
							   "text", DIRECTORY,
							   NULL);
	gtk_tree_view_append_column(tv, column);

	dialog = gtk_builder_get_object(builder, "Log viewer");
	g_signal_connect(G_OBJECT(dialog),
			 "response", (GCallback) log_dialog_response, NULL);

	gtk_widget_show(dialog);
	g_object_unref(builder);
}
	
/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/
static void log_dialog_response(GtkDialog *dialog, gint resp_id,
				gpointer udata)
{
	/* Only response we should get is CLOSE */
	gtk_widget_hide(GTK_WIDGET(dialog));
	gtk_widget_destroy(GTK_WIDGET(dialog));
}
