/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
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

/* bulk_rename.c - rename multiple files at once */

#include "config.h"

#include <stdlib.h>
#include <gtk/gtk.h>
#include <sys/types.h>
#include <regex.h>
#include <string.h>
#include <errno.h>

#include "global.h"

#include "main.h"
#include "bulk_rename.h"
#include "support.h"
#include "gui_support.h"

enum {RESPONSE_RENAME, RESPONSE_RESET};

/* Static prototypes */
static gboolean apply_replace(GtkWidget *box);
static void response(GtkWidget *box, int resp, GtkListStore *model);
static void reset_model(GtkListStore *model);
static gboolean rename_items(const char *dir, GtkListStore *list);
static void cell_edited(GtkCellRendererText *cell, const gchar *path_string,
			const gchar *new_text, GtkTreeModel *model);


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Bulk rename these items */
void bulk_rename(const char *dir, GList *items)
{
	GtkWidget *box, *button, *tree, *swin, *hbox;
	GtkWidget *replace_entry, *with_entry;
	GtkTreeViewColumn *column;
	GtkCellRenderer *cell_renderer;
	GtkListStore *model;
	GtkRequisition req;

	box = gtk_dialog_new();
	g_object_set_data_full(G_OBJECT(box), "rename_dir",
				g_strdup(dir), g_free);
	gtk_window_set_title(GTK_WINDOW(box), _("Bulk rename files"));
	gtk_dialog_set_has_separator(GTK_DIALOG(box), FALSE);

	button = button_new_mixed(GTK_STOCK_REFRESH, _("Reset"));
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_dialog_add_action_widget(GTK_DIALOG(box), button, RESPONSE_RESET);
	gtk_dialog_set_default_response(GTK_DIALOG(box), RESPONSE_RESET);
	gtk_tooltips_set_tip(tooltips, button,
			_("Make the New column a copy of Old"), NULL);

	gtk_dialog_add_button(GTK_DIALOG(box),
				GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);

	button = button_new_mixed(GTK_STOCK_EXECUTE, _("_Rename"));
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_dialog_add_action_widget(GTK_DIALOG(box), button, RESPONSE_RENAME);
	gtk_dialog_set_default_response(GTK_DIALOG(box), RESPONSE_RENAME);

	/* Replace */

	hbox = gtk_hbox_new(FALSE, 4);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(box)->vbox),
				hbox, FALSE, TRUE, 0);

	gtk_box_pack_start(GTK_BOX(hbox),
				gtk_label_new(_("Replace:")), FALSE, TRUE, 0);

	replace_entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(box), "replace_entry", replace_entry);
	gtk_box_pack_start(GTK_BOX(hbox), replace_entry, TRUE, TRUE, 0);
	gtk_entry_set_text(GTK_ENTRY(replace_entry), "\\.htm$");
	gtk_tooltips_set_tip(tooltips, replace_entry,
			_("This is a regular expression to search for.\n"
			"^ matches the start of a filename\n"
			"$ matches the end\n"
			"\\. matches a dot\n"
			"\\.htm$ matches the '.htm' in 'index.htm', etc"),
			NULL);

	gtk_box_pack_start(GTK_BOX(hbox),
				gtk_label_new(_("With:")), FALSE, TRUE, 0);

	with_entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(box), "with_entry", with_entry);
	gtk_box_pack_start(GTK_BOX(hbox), with_entry, TRUE, TRUE, 0);
	gtk_entry_set_text(GTK_ENTRY(with_entry), ".html");
	gtk_tooltips_set_tip(tooltips, with_entry,
			_("The first match in each filename will be replaced "
			"by this string. "
			"The only special characters are back-references "
			"from \\0 to \\9. To use them literally, "
			"they have to be escaped with a backslash."), NULL);

	button = gtk_button_new_with_label(_("Apply"));
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);
	gtk_tooltips_set_tip(tooltips, button,
			_("Do a search-and-replace in the New column. "
			"The files are not actually renamed until you click "
			"on the Rename button below."), NULL);

	g_signal_connect_swapped(replace_entry, "activate",
			G_CALLBACK(gtk_widget_grab_focus), with_entry);
	g_signal_connect_swapped(with_entry, "activate",
			G_CALLBACK(apply_replace), box);
	g_signal_connect_swapped(button, "clicked",
			G_CALLBACK(apply_replace), box);

	/* The TreeView */

	model = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
	g_object_set_data(G_OBJECT(box), "tree_model", model);
	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));

	cell_renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes(
				_("Old name"), cell_renderer, "text", 0, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	cell_renderer = gtk_cell_renderer_text_new();
	g_object_set(G_OBJECT(cell_renderer), "editable", TRUE, NULL);
	g_signal_connect(G_OBJECT(cell_renderer), "edited",
			G_CALLBACK(cell_edited), model);
	column = gtk_tree_view_column_new_with_attributes(
				_("New name"), cell_renderer, "text", 1, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	swin = gtk_scrolled_window_new(NULL, NULL);
	gtk_container_set_border_width(GTK_CONTAINER(swin), 4);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(swin),
						GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(swin),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(box)->vbox), swin, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(swin), tree);

	while (items) {
		GtkTreeIter iter;
		const char *name = items->data;

		gtk_list_store_append(model, &iter);
		gtk_list_store_set(model, &iter, 0, name, 1, name, -1);

		items = items->next;
	}
	
	gtk_widget_show_all(tree);
	gtk_widget_size_request(tree, &req);
	req.width = MIN(req.width + 50, screen_width - 50);
	req.height = MIN(req.height + 150, screen_height - 50);

	gtk_window_set_default_size(GTK_WINDOW(box), req.width, req.height);

	number_of_windows++;
	g_signal_connect(box, "destroy", G_CALLBACK(one_less_window), NULL);
	g_signal_connect(box, "response", G_CALLBACK(response), model);
	gtk_widget_show_all(box);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void response(GtkWidget *box, int resp, GtkListStore *model)
{
	if (resp == RESPONSE_RESET)
		reset_model(model);
	else if (resp == RESPONSE_RENAME)
	{
		if (rename_items(g_object_get_data(G_OBJECT(box), "rename_dir"),
				 model))
			gtk_widget_destroy(box);
	}
	else
		gtk_widget_destroy(box);
}

/** Substitute: s/old/with/
 * Returns the result as a new string, or NULL if there is no match.
 * "replace" is a compiled version of "with".
 * Caller must free the result.
 */
static GString *subst(const char *old, regex_t *replace, const char *with)
{
	int max_subs = 10;
	GString *new;
	regmatch_t match[max_subs];

	if (regexec(replace, old, max_subs, match, 0) != 0)
		return NULL;		/* No match */

	g_return_val_if_fail(match[0].rm_so != -1, NULL);

	new = g_string_new(NULL);
	g_string_append_len(new, old, match[0].rm_so);

	int i;
	for (i = 0; with[i]; i++)
	{
		if (with[i] == '\\' && with[i+1])
		{
			i++;
			if (with[i] >= '0' && with[i]-'0' < max_subs)
			{
				int subpat;

				subpat = with[i] - '0';

				if (match[subpat].rm_so != -1)
					g_string_append_len(new, old + match[subpat].rm_so,
							match[subpat].rm_eo - match[subpat].rm_so);

			}
			else
			{
				// Escape next character
				g_string_append_c(new, with[i]);
			}
		}
		else
			g_string_append_c(new, with[i]);
	}

	g_string_append(new, old + match[0].rm_eo);

	return new;
}

/* Do a search-and-replace on the second column. */
static void update_model(GtkListStore *list, regex_t *replace, const char *with)
{
	GtkTreeIter iter;
	GtkTreeModel *model = (GtkTreeModel *) list;
	int n_matched = 0;
	int n_changed = 0;

	if (!gtk_tree_model_get_iter_first(model, &iter))
	{
		g_warning("Model empty!");
		return;
	}

	do
	{
		GString *new;
		char *old = NULL;

		gtk_tree_model_get(model, &iter, 1, &old, -1);

		new = subst(old, replace, with);
		if (new)
		{
			n_matched++;
			if (strcmp(old, new->str) != 0)
			{
				n_changed++;
				gtk_list_store_set(list, &iter, 1, new->str, -1);
			}

			g_string_free(new, TRUE);
		}
		g_free(old);

	} while (gtk_tree_model_iter_next(model, &iter));

	if (n_matched == 0)
		report_error(_("No strings (in the New column) matched "
				"the given expression"));
	else if (n_changed == 0)
	{
		if (n_matched == 1)
			report_error(_("One name matched, but the result was "
					"the same"));
		else
			report_error(_("%d names matched, but the results were "
					"all the same"), n_matched);
	}
}

static gboolean apply_replace(GtkWidget *box)
{
	GtkListStore *model;
	GtkEntry *replace_entry, *with_entry;
	const char *replace, *with;
	regex_t compiled;
	int error;

	replace_entry = g_object_get_data(G_OBJECT(box), "replace_entry");
	with_entry = g_object_get_data(G_OBJECT(box), "with_entry");
	model = g_object_get_data(G_OBJECT(box), "tree_model");

	g_return_val_if_fail(replace_entry != NULL, TRUE);
	g_return_val_if_fail(with_entry != NULL, TRUE);
	g_return_val_if_fail(model != NULL, TRUE);

	replace = gtk_entry_get_text(replace_entry);
	with = gtk_entry_get_text(with_entry);

	if (replace[0] == '\0' && with[0] == '\0')
	{
		report_error(_("Specify a regular expression to match, "
				"and a string to replace matches with."));
		return TRUE;
	}

	error = regcomp(&compiled, replace, REG_EXTENDED);
	if (error)
	{
		char *message;
		size_t size;

		size = regerror(error, &compiled, NULL, 0);
		g_return_val_if_fail(size > 0, TRUE);
		
		message = g_malloc(size);
		regerror(error, &compiled, message, size);

		report_error(_("%s (for '%s')"), message, replace);

		return TRUE;
	}

	update_model(model, &compiled, with);

	regfree(&compiled);

	return TRUE;
}

static void reset_model(GtkListStore *list)
{
	GtkTreeIter iter;
	GtkTreeModel *model = (GtkTreeModel *) list;

	if (!gtk_tree_model_get_iter_first(model, &iter))
		return;

	do {
		char *before;
		gtk_tree_model_get(model, &iter, 0, &before, -1);
		gtk_list_store_set(list, &iter, 1, before, -1);
		g_free(before);
	} while (gtk_tree_model_iter_next(model, &iter));
}

static gboolean do_rename(const char *before, const char *after)
{
	/* Check again, just in case */
	if (access(after, F_OK) == 0)
	{
		report_error(_("A file called '%s' already exists. "
				"Aborting bulk rename."), after);
	}
	else if (rename(before, after))
	{
		report_error(_("Failed to rename '%s' as '%s':\n%s\n"
				"Aborting bulk rename."), before, after,
				g_strerror(errno));
	}
	else
		return TRUE;
	return FALSE;
}

static gboolean rename_items(const char *dir, GtkListStore *list)
{
	GtkTreeModel *model = (GtkTreeModel *) list;
	GtkTreeIter iter;
	char *slash_example = NULL;
	GHashTable *names = NULL;
	gboolean success = FALSE;
	int n_renames = 0;

	g_return_val_if_fail(dir != NULL, FALSE);
	g_return_val_if_fail(list != NULL, FALSE);

	if (!gtk_tree_model_get_iter_first(model, &iter))
		return FALSE;	/* (error) */

	names = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
	do {
		char *before, *after;
		const char *dest;

		gtk_tree_model_get(model, &iter, 0, &before, 1, &after, -1);

		if (!slash_example && strchr(after, '/'))
		{
			slash_example = g_strdup(after);
		}

		if (g_hash_table_lookup(names, before))
		{
			report_error("Filename '%s' used twice!", before);
			goto fail;
		}
		g_hash_table_insert(names, before, "");

		if (after[0] == '\0' || strcmp(after, before) == 0)
		{
			g_free(after);
			continue;
		}

		if (g_hash_table_lookup(names, after))
		{
			report_error("Filename '%s' used twice!", after);
			goto fail;
		}
		g_hash_table_insert(names, after, "");

		if (after[0] == '/')
			dest = after;
		else
			dest = make_path(dir, after);
		if (access(dest, F_OK) == 0)
		{
			report_error(_("A file called '%s' already exists"),
					dest);
			goto fail;
		}

		n_renames++;
	} while (gtk_tree_model_iter_next(model, &iter));

	if (slash_example)
	{
		char *message;
		message = g_strdup_printf(_("Some of the New names contain "
				"/ characters (eg '%s'). "
				"This will cause the files to end up in "
				"different directories. "
				"Continue?"), slash_example);
		if (!confirm(message, GTK_STOCK_EXECUTE, "Rename anyway"))
		{
			g_free(message);
			goto fail;
		}
		g_free(message);
	}

	if (n_renames == 0)
	{
		report_error(_("None of the names have changed. "
				"Nothing to do!"));
		goto fail;
	}

	success = TRUE;
	gtk_tree_model_get_iter_first(model, &iter);
	while (success)
	{
		char *before, *after, *before_path;
		const char *dest;

		gtk_tree_model_get(model, &iter, 0, &before, 1, &after, -1);

		if (after[0] == '\0' || strcmp(after, before) == 0)
			dest = NULL;
		else if (after[0] == '/')
			dest = after;
		else
			dest = make_path(dir, after);

		before_path = g_build_filename(dir, before, NULL);

		if (dest == NULL || do_rename(before_path, dest))
		{
			/* Advances iter */
			if (!gtk_list_store_remove(list, &iter))
				break;	/* Last item; finished */
		}
		else
			success = FALSE;

		g_free(before_path);
		g_free(before);
		g_free(after);
	}

fail:
	g_free(slash_example);
	if (names)
		g_hash_table_destroy(names);
	return success;
}

static void cell_edited(GtkCellRendererText *cell,
			const gchar *path_string, const gchar *new_text,
		        GtkTreeModel *model)
{
	GtkTreePath *path;
	GtkTreeIter iter;

	path = gtk_tree_path_new_from_string(path_string);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_path_free(path);

	gtk_list_store_set(GTK_LIST_STORE(model), &iter, 1, new_text, -1);
}

#ifdef UNIT_TESTS
static void test_subst(const char *string, const char *pattern, const char *with, const char *expected)
{
	regex_t compiled;
	GString *new;

	g_print("Testing s/%s/%s\n", pattern, with);

	if (regcomp(&compiled, pattern, REG_EXTENDED))
		g_error("Failed to compiled '%s'", pattern);

	new = subst(string, &compiled, with);

	if (new == NULL)
	{
		g_return_if_fail(expected == NULL);
	}
	else
	{
		//g_print("Got: %s\n", new->str);
		g_return_if_fail(expected != NULL);
		g_return_if_fail(strcmp(new->str, expected) == 0);
		g_string_free(new, TRUE);
	}

	regfree(&compiled);
}

void bulk_rename_tests()
{
	test_subst("hello", "l", "L", "heLlo");
	test_subst("hello", "h(.*)l", "\\1-\\1", "el-elo");
	test_subst("hello", "(h)", "\\1", "hello");
	test_subst("hello", "(h)", "\\\\1", "\\1ello");
	test_subst("hello", "(h)", "\\\\\\1", "\\hello");
	test_subst("hello", "(h)", "\\", "\\ello");
	test_subst("hello", "(h)$", "\\", NULL);
	test_subst("hello", "(.)(.)(.).*", "\\0-\\1-\\2-\\3-\\4-\\9-\\:", "hello-h-e-l---:");
}
#endif
