/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include <glib/gprintf.h>

#include "main.h"
#include "bar_keywords.h"

#include "filedata.h"
#include "history_list.h"
#include "metadata.h"
#include "misc.h"
#include "ui_fileops.h"
#include "ui_misc.h"
#include "ui_utildlg.h"
#include "utilops.h"
#include "bar.h"
#include "ui_menu.h"
#include "rcfile.h"
#include "layout.h"
#include "dnd.h"


//static void bar_pane_keywords_keyword_update_all(void);
static void bar_pane_keywords_changed(GtkTextBuffer *buffer, gpointer data);

/*
 *-------------------------------------------------------------------
 * keyword / comment utils
 *-------------------------------------------------------------------
 */


GList *keyword_list_pull(GtkWidget *text_widget)
{
	GList *list;
	gchar *text;

	text = text_widget_text_pull(text_widget);
	list = string_to_keywords_list(text);

	g_free(text);

	return list;
}

/* the "changed" signal should be blocked before calling this */
static void keyword_list_push(GtkWidget *textview, GList *list)
{
	GtkTextBuffer *buffer;
	GtkTextIter start, end;

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
	gtk_text_buffer_get_bounds(buffer, &start, &end);
	gtk_text_buffer_delete(buffer, &start, &end);

	while (list)
		{
		const gchar *word = list->data;
		GtkTextIter iter;

		gtk_text_buffer_get_end_iter(buffer, &iter);
		if (word) gtk_text_buffer_insert(buffer, &iter, word, -1);
		gtk_text_buffer_get_end_iter(buffer, &iter);
		gtk_text_buffer_insert(buffer, &iter, "\n", -1);

		list = list->next;
		}
}


/*
 *-------------------------------------------------------------------
 * info bar
 *-------------------------------------------------------------------
 */


enum {
	FILTER_KEYWORD_COLUMN_TOGGLE = 0,
	FILTER_KEYWORD_COLUMN_MARK,
	FILTER_KEYWORD_COLUMN_NAME,
	FILTER_KEYWORD_COLUMN_IS_KEYWORD,
	FILTER_KEYWORD_COLUMN_COUNT
};

static GType filter_keyword_column_types[] = {G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN};

typedef struct _PaneKeywordsData PaneKeywordsData;
struct _PaneKeywordsData
{
	PaneData pane;
	GtkWidget *widget;

	GtkWidget *keyword_view;
	GtkWidget *keyword_treeview;

	GtkTreePath *click_tpath;

	gboolean expand_checked;
	gboolean collapse_unchecked;
	gboolean hide_unchecked;

	guint idle_id; /* event source id */	
	FileData *fd;
	gchar *key;
};

typedef struct _ConfDialogData ConfDialogData;
struct _ConfDialogData
{
	PaneKeywordsData *pkd;
	GtkTreePath *click_tpath;
	
	/* dialog parts */
	GenericDialog *gd;
	GtkWidget *edit_widget;
	gboolean is_keyword;
	
	gboolean edit_existing;
};

//static GList *bar_list = NULL;


static void bar_pane_keywords_write(PaneKeywordsData *pkd)
{
	GList *list;

	if (!pkd->fd) return;

	list = keyword_list_pull(pkd->keyword_view);

	metadata_write_list(pkd->fd, KEYWORD_KEY, list);

	string_list_free(list);
}

gboolean bar_keyword_tree_expand_if_set_cb(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer data)
{
	PaneKeywordsData *pkd = data;
	gboolean set;

	gtk_tree_model_get(model, iter, FILTER_KEYWORD_COLUMN_TOGGLE, &set, -1);
	
	if (set && !gtk_tree_view_row_expanded(GTK_TREE_VIEW(pkd->keyword_treeview), path))
		{
		gtk_tree_view_expand_to_path(GTK_TREE_VIEW(pkd->keyword_treeview), path);
		}
	return FALSE;
}

gboolean bar_keyword_tree_collapse_if_unset_cb(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer data)
{
	PaneKeywordsData *pkd = data;
	gboolean set;

	gtk_tree_model_get(model, iter, FILTER_KEYWORD_COLUMN_TOGGLE, &set, -1);
	
	if (!set && gtk_tree_view_row_expanded(GTK_TREE_VIEW(pkd->keyword_treeview), path))
		{
		gtk_tree_view_collapse_row(GTK_TREE_VIEW(pkd->keyword_treeview), path);
		}
	return FALSE;
}

static void bar_keyword_tree_sync(PaneKeywordsData *pkd)
{
	GtkTreeModel *model;

	GtkTreeModel *keyword_tree;
	GList *keywords;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));

	keywords = keyword_list_pull(pkd->keyword_view);
	keyword_show_set_in(GTK_TREE_STORE(keyword_tree), model, keywords);
	if (pkd->hide_unchecked) keyword_hide_unset_in(GTK_TREE_STORE(keyword_tree), model, keywords);
	string_list_free(keywords);

	gtk_tree_model_filter_refilter(GTK_TREE_MODEL_FILTER(model));

	if (pkd->expand_checked) gtk_tree_model_foreach(model, bar_keyword_tree_expand_if_set_cb, pkd);
	if (pkd->collapse_unchecked) gtk_tree_model_foreach(model, bar_keyword_tree_collapse_if_unset_cb, pkd);
}

#if 0
static void bar_pane_keywords_keyword_update_all(void)
{
	GList *work;

	work = bar_list;
	while (work)
		{
		PaneKeywordsData *pkd;
//		GList *keywords;

		pkd = work->data;
		work = work->next;

		bar_keyword_tree_sync(pkd);
		}
}
#endif

static void bar_pane_keywords_update(PaneKeywordsData *pkd)
{
	GList *keywords = NULL;
	GtkTextBuffer *keyword_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(pkd->keyword_view));

	g_signal_handlers_block_by_func(keyword_buffer, bar_pane_keywords_changed, pkd);

	keywords = metadata_read_list(pkd->fd, KEYWORD_KEY, METADATA_PLAIN);
	keyword_list_push(pkd->keyword_view, keywords);
	bar_keyword_tree_sync(pkd);
	string_list_free(keywords);
	
	g_signal_handlers_unblock_by_func(keyword_buffer, bar_pane_keywords_changed, pkd);

}

void bar_pane_keywords_set_fd(GtkWidget *pane, FileData *fd)
{
	PaneKeywordsData *pkd;

	pkd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pkd) return;

	file_data_unref(pkd->fd);
	pkd->fd = file_data_ref(fd);

	bar_pane_keywords_update(pkd);
}

static void bar_pane_keywords_write_config(GtkWidget *pane, GString *outstr, gint indent)
{
	PaneKeywordsData *pkd;

	pkd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pkd) return;

	WRITE_NL(); WRITE_STRING("<pane_keywords ");
	write_char_option(outstr, indent, "id", pkd->pane.id);
	write_char_option(outstr, indent, "title", gtk_label_get_text(GTK_LABEL(pkd->pane.title)));
	WRITE_BOOL(pkd->pane, expanded);
	WRITE_CHAR(*pkd, key);
	WRITE_STRING("/>");
}

gint bar_pane_keywords_event(GtkWidget *bar, GdkEvent *event)
{
	PaneKeywordsData *pkd;

	pkd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pkd) return FALSE;

#if GTK_CHECK_VERSION(2,20,0)
	if (gtk_widget_has_focus(pkd->keyword_view)) return gtk_widget_event(pkd->keyword_view, event);
#else
	if (GTK_WIDGET_HAS_FOCUS(pkd->keyword_view)) return gtk_widget_event(pkd->keyword_view, event);
#endif

	return FALSE;
}

static void bar_pane_keywords_keyword_toggle(GtkCellRendererToggle *toggle, const gchar *path, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;
	GtkTreeIter iter;
	GtkTreePath *tpath;
	gboolean active;
	GList *list;
	GtkTreeIter child_iter;
	GtkTreeModel *keyword_tree;
	
	GtkTextBuffer *keyword_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(pkd->keyword_view));

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));

	tpath = gtk_tree_path_new_from_string(path);
	gtk_tree_model_get_iter(model, &iter, tpath);
	gtk_tree_path_free(tpath);

	gtk_tree_model_get(model, &iter, FILTER_KEYWORD_COLUMN_TOGGLE, &active, -1);
	active = (!active);


	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, &iter);

	list = keyword_list_pull(pkd->keyword_view);
	if (active) 
		keyword_tree_set(keyword_tree, &child_iter, &list);
	else
		keyword_tree_reset(keyword_tree, &child_iter, &list);
	
	g_signal_handlers_block_by_func(keyword_buffer, bar_pane_keywords_changed, pkd);
	keyword_list_push(pkd->keyword_view, list);
	string_list_free(list);
	g_signal_handlers_unblock_by_func(keyword_buffer, bar_pane_keywords_changed, pkd);

	/* call this just once in the end */
	bar_pane_keywords_changed(keyword_buffer, pkd);
	/*
	  bar_pane_keywords_change calls bar_keyword_tree_sync, no need to do it again
	bar_keyword_tree_sync(pkd);
	*/
}

void bar_pane_keywords_filter_modify(GtkTreeModel *model, GtkTreeIter *iter, GValue *value, gint column, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	GtkTreeIter child_iter;

	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, iter);
	
	memset(value, 0, sizeof (GValue));

	switch (column)
		{
		case FILTER_KEYWORD_COLUMN_TOGGLE:
			{
			GList *keywords = keyword_list_pull(pkd->keyword_view);
			gboolean set = keyword_tree_is_set(keyword_tree, &child_iter, keywords);
			string_list_free(keywords);
			
			g_value_init(value, G_TYPE_BOOLEAN);
			g_value_set_boolean(value, set);
			break;
			}
		case FILTER_KEYWORD_COLUMN_MARK:
			gtk_tree_model_get_value(keyword_tree, &child_iter, KEYWORD_COLUMN_MARK, value);
			break;
		case FILTER_KEYWORD_COLUMN_NAME:
			gtk_tree_model_get_value(keyword_tree, &child_iter, KEYWORD_COLUMN_NAME, value);
			break;
		case FILTER_KEYWORD_COLUMN_IS_KEYWORD:
			gtk_tree_model_get_value(keyword_tree, &child_iter, KEYWORD_COLUMN_IS_KEYWORD, value);
			break;
		}
	return;

}

gboolean bar_pane_keywords_filter_visible(GtkTreeModel *keyword_tree, GtkTreeIter *iter, gpointer data)
{
	GtkTreeModel *filter = data;
	
	return !keyword_is_hidden_in(keyword_tree, iter, filter);
}

static void bar_pane_keywords_set_selection(PaneKeywordsData *pkd, gboolean append)
{
	GList *keywords = NULL;
	GList *list = NULL;
	GList *work;

	keywords = keyword_list_pull(pkd->keyword_view);

	list = layout_selection_list(pkd->pane.lw);
	list = file_data_process_groups_in_selection(list, FALSE, NULL);
	
	work = list;
	while (work)
		{
		FileData *fd = work->data;
		work = work->next;

		if (append)
			{
			metadata_append_list(fd, KEYWORD_KEY, keywords);
			}
		else
			{
			metadata_write_list(fd, KEYWORD_KEY, keywords);
			}
		}

	filelist_free(list);
	string_list_free(keywords);
}

static void bar_pane_keywords_sel_add_cb(GtkWidget *button, gpointer data)
{
	PaneKeywordsData *pkd = data;

	bar_pane_keywords_set_selection(pkd, TRUE);
}

static void bar_pane_keywords_sel_replace_cb(GtkWidget *button, gpointer data)
{
	PaneKeywordsData *pkd = data;

	bar_pane_keywords_set_selection(pkd, FALSE);
}

static void bar_pane_keywords_populate_popup_cb(GtkTextView *textview, GtkMenu *menu, gpointer data)
{
	PaneKeywordsData *pkd = data;

	menu_item_add_divider(GTK_WIDGET(menu));
	menu_item_add_stock(GTK_WIDGET(menu), _("Add keywords to selected files"), GTK_STOCK_ADD, G_CALLBACK(bar_pane_keywords_sel_add_cb), pkd);
	menu_item_add_stock(GTK_WIDGET(menu), _("Replace existing keywords in selected files"), GTK_STOCK_CONVERT, G_CALLBACK(bar_pane_keywords_sel_replace_cb), pkd);
}


static void bar_pane_keywords_notify_cb(FileData *fd, NotifyType type, gpointer data)
{
	PaneKeywordsData *pkd = data;
	if ((type & (NOTIFY_REREAD | NOTIFY_CHANGE | NOTIFY_METADATA)) && fd == pkd->fd) 
		{
		DEBUG_1("Notify pane_keywords: %s %04x", fd->path, type);
		bar_pane_keywords_update(pkd);
		}
}

static gboolean bar_pane_keywords_changed_idle_cb(gpointer data)
{
	PaneKeywordsData *pkd = data;

	file_data_unregister_notify_func(bar_pane_keywords_notify_cb, pkd);
	bar_pane_keywords_write(pkd);
	bar_keyword_tree_sync(pkd);
	file_data_register_notify_func(bar_pane_keywords_notify_cb, pkd, NOTIFY_PRIORITY_LOW);
	pkd->idle_id = 0;
	return FALSE;
}

static void bar_pane_keywords_changed(GtkTextBuffer *buffer, gpointer data)
{
	PaneKeywordsData *pkd = data;

	if (pkd->idle_id) return;
	/* higher prio than redraw */
	pkd->idle_id = g_idle_add_full(G_PRIORITY_HIGH_IDLE, bar_pane_keywords_changed_idle_cb, pkd, NULL);
}


/*
 *-------------------------------------------------------------------
 * dnd
 *-------------------------------------------------------------------
 */


static GtkTargetEntry bar_pane_keywords_drag_types[] = {
	{ TARGET_APP_KEYWORD_PATH_STRING, GTK_TARGET_SAME_WIDGET, TARGET_APP_KEYWORD_PATH },
	{ "text/plain", 0, TARGET_TEXT_PLAIN }
};
static gint n_keywords_drag_types = 2;


static GtkTargetEntry bar_pane_keywords_drop_types[] = {
	{ TARGET_APP_KEYWORD_PATH_STRING, GTK_TARGET_SAME_WIDGET, TARGET_APP_KEYWORD_PATH },
	{ "text/plain", 0, TARGET_TEXT_PLAIN }
};
static gint n_keywords_drop_types = 2;


static void bar_pane_keywords_dnd_get(GtkWidget *tree_view, GdkDragContext *context,
				     GtkSelectionData *selection_data, guint info,
				     guint time, gpointer data)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	GtkTreeIter child_iter;
	GtkTreeModel *keyword_tree;

	GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view)); 

        if (!gtk_tree_selection_get_selected(sel, &model, &iter)) return;

	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, &iter);

	switch (info)
		{
		case TARGET_APP_KEYWORD_PATH:
			{
			GList *path = keyword_tree_get_path(keyword_tree, &child_iter);
			gtk_selection_data_set(selection_data, selection_data->target,
					       8, (gpointer) &path, sizeof(path));
			break;
			}

		case TARGET_TEXT_PLAIN:
		default:
			{
			gchar *name = keyword_get_name(keyword_tree, &child_iter);
			gtk_selection_data_set_text(selection_data, name, -1);
			g_free(name);
			}
			break;
		}
}

static void bar_pane_keywords_dnd_begin(GtkWidget *tree_view, GdkDragContext *context, gpointer data)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	GtkTreeIter child_iter;
	GtkTreeModel *keyword_tree;
	gchar *name;

	GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view)); 

        if (!gtk_tree_selection_get_selected(sel, &model, &iter)) return;

	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, &iter);

	name = keyword_get_name(keyword_tree, &child_iter);

	dnd_set_drag_label(tree_view, context, name);
	g_free(name);

}

static void bar_pane_keywords_dnd_end(GtkWidget *widget, GdkDragContext *context, gpointer data)
{
}


static gboolean bar_pane_keywords_dnd_can_move(GtkTreeModel *keyword_tree, GtkTreeIter *src_kw_iter, GtkTreeIter *dest_kw_iter)
{
	gchar *src_name;
	GtkTreeIter parent;
	
	if (dest_kw_iter && keyword_same_parent(keyword_tree, src_kw_iter, dest_kw_iter)) 
		{
		return TRUE; /* reordering of siblings is ok */
		}
	if (!dest_kw_iter && !gtk_tree_model_iter_parent(keyword_tree, &parent, src_kw_iter))
		{
		return TRUE; /* reordering of top-level siblings is ok */
		}

	src_name = keyword_get_name(keyword_tree, src_kw_iter);
	if (keyword_exists(keyword_tree, NULL, dest_kw_iter, src_name, FALSE, NULL))
		{
		g_free(src_name);
		return FALSE;
	}
	g_free(src_name);
	return TRUE;
}

static gboolean bar_pane_keywords_dnd_skip_existing(GtkTreeModel *keyword_tree, GtkTreeIter *dest_kw_iter, GList **keywords)
{
	/* we have to find at least one keyword that does not already exist as a sibling of dest_kw_iter */
	GList *work = *keywords;
	while (work)
		{
		gchar *keyword = work->data;
		if (keyword_exists(keyword_tree, NULL, dest_kw_iter, keyword, FALSE, NULL))
			{
			GList *next = work->next;
			g_free(keyword);
			*keywords = g_list_delete_link(*keywords, work);
			work = next;
			}
		else
			{
			work = work->next;
			}
		}
	return !!*keywords;
}

static void bar_pane_keywords_dnd_receive(GtkWidget *tree_view, GdkDragContext *context,
					  gint x, gint y,
					  GtkSelectionData *selection_data, guint info,
					  guint time, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreePath *tpath = NULL;
        GtkTreeViewDropPosition pos;
	GtkTreeModel *model;

	GtkTreeModel *keyword_tree;
	gboolean src_valid = FALSE;
	GList *new_keywords = NULL;
	GList *work;

	/* iterators for keyword_tree */
	GtkTreeIter src_kw_iter;
	GtkTreeIter dest_kw_iter;
	GtkTreeIter new_kw_iter;

	g_signal_stop_emission_by_name(tree_view, "drag_data_received");

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));

	gtk_tree_view_get_dest_row_at_pos(GTK_TREE_VIEW(tree_view), x, y, &tpath, &pos);
	gtk_tree_view_set_drag_dest_row(GTK_TREE_VIEW(tree_view), NULL, pos);

	switch (info)
		{
		case TARGET_APP_KEYWORD_PATH:
			{
			GList *path = *(gpointer *)selection_data->data;
			src_valid = keyword_tree_get_iter(keyword_tree, &src_kw_iter, path);
			string_list_free(path);
			break;
			}
		default:
			new_keywords = string_to_keywords_list((gchar *)selection_data->data);
			break;
		}

	if (tpath)
		{
		GtkTreeIter dest_iter;
                gtk_tree_model_get_iter(model, &dest_iter, tpath);
		gtk_tree_path_free(tpath);
		gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &dest_kw_iter, &dest_iter);

		if (src_valid && gtk_tree_store_is_ancestor(GTK_TREE_STORE(keyword_tree), &src_kw_iter, &dest_kw_iter))
			{
			/* can't move to it's own child */
			return;
			}

		if (src_valid && keyword_compare(keyword_tree, &src_kw_iter, &dest_kw_iter) == 0)
			{
			/* can't move to itself */
			return;
			}

		if ((pos == GTK_TREE_VIEW_DROP_INTO_OR_BEFORE || pos == GTK_TREE_VIEW_DROP_INTO_OR_AFTER) &&
		    !gtk_tree_model_iter_has_child(keyword_tree, &dest_kw_iter))
			{
			/* the node has no children, all keywords can be added */
			gtk_tree_store_append(GTK_TREE_STORE(keyword_tree), &new_kw_iter, &dest_kw_iter);
			}
		else
			{
			if (src_valid && !bar_pane_keywords_dnd_can_move(keyword_tree, &src_kw_iter, &dest_kw_iter))
				{
				/* the keyword can't be moved if the same name already exist */
				return;
				}
			if (new_keywords && !bar_pane_keywords_dnd_skip_existing(keyword_tree, &dest_kw_iter, &new_keywords))
				{
				/* the keywords can't be added if the same name already exist */
				return;
				}
				
			switch (pos)
				{
				case GTK_TREE_VIEW_DROP_INTO_OR_BEFORE:
				case GTK_TREE_VIEW_DROP_BEFORE:
					gtk_tree_store_insert_before(GTK_TREE_STORE(keyword_tree), &new_kw_iter, NULL, &dest_kw_iter);
					break;
				case GTK_TREE_VIEW_DROP_INTO_OR_AFTER:
				case GTK_TREE_VIEW_DROP_AFTER:
					gtk_tree_store_insert_after(GTK_TREE_STORE(keyword_tree), &new_kw_iter, NULL, &dest_kw_iter);
					break;
				}
			}
			
		}
	else
		{
		if (src_valid && !bar_pane_keywords_dnd_can_move(keyword_tree, &src_kw_iter, NULL))
			{
			/* the keyword can't be moved if the same name already exist */
			return;
			}
		if (new_keywords && !bar_pane_keywords_dnd_skip_existing(keyword_tree, NULL, &new_keywords))
			{
			/* the keywords can't be added if the same name already exist */
			return;
			}
		gtk_tree_store_append(GTK_TREE_STORE(keyword_tree), &new_kw_iter, NULL);
		}
		
		
	if (src_valid)
		{
		keyword_move_recursive(GTK_TREE_STORE(keyword_tree), &new_kw_iter, &src_kw_iter);
		}
	
	work = new_keywords;
	while (work)
		{
		gchar *keyword = work->data;
		keyword_set(GTK_TREE_STORE(keyword_tree), &new_kw_iter, keyword, TRUE);
		work = work->next;

		if (work)
			{
			GtkTreeIter add;
			gtk_tree_store_insert_after(GTK_TREE_STORE(keyword_tree), &add, NULL, &new_kw_iter);
			new_kw_iter = add;
			}
		}
	string_list_free(new_keywords);
	bar_keyword_tree_sync(pkd);
}

static gint bar_pane_keywords_dnd_motion(GtkWidget *tree_view, GdkDragContext *context,
					gint x, gint y, guint time, gpointer data)
{
	GtkTreePath *tpath = NULL;
        GtkTreeViewDropPosition pos;
	gtk_tree_view_get_dest_row_at_pos(GTK_TREE_VIEW(tree_view), x, y, &tpath, &pos);
	if (tpath)
		{
		GtkTreeModel *model;
		GtkTreeIter dest_iter;
		model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
                gtk_tree_model_get_iter(model, &dest_iter, tpath);
		if (pos == GTK_TREE_VIEW_DROP_INTO_OR_BEFORE && gtk_tree_model_iter_has_child(model, &dest_iter))
			pos = GTK_TREE_VIEW_DROP_BEFORE;
		
		if (pos == GTK_TREE_VIEW_DROP_INTO_OR_AFTER && gtk_tree_model_iter_has_child(model, &dest_iter))
			pos = GTK_TREE_VIEW_DROP_AFTER;
		}

	gtk_tree_view_set_drag_dest_row(GTK_TREE_VIEW(tree_view), tpath, pos);
	gtk_tree_path_free(tpath);
	
	if (tree_view == gtk_drag_get_source_widget(context))
		gdk_drag_status(context, GDK_ACTION_MOVE, time);
	else
		gdk_drag_status(context, GDK_ACTION_COPY, time);
	
	return TRUE;
}

/*
 *-------------------------------------------------------------------
 * edit dialog
 *-------------------------------------------------------------------
 */

static void bar_pane_keywords_edit_destroy_cb(GtkWidget *widget, gpointer data)
{
	ConfDialogData *cdd = data;
	gtk_tree_path_free(cdd->click_tpath);
	g_free(cdd);
}


static void bar_pane_keywords_edit_cancel_cb(GenericDialog *gd, gpointer data)
{
}


static void bar_pane_keywords_edit_ok_cb(GenericDialog *gd, gpointer data)
{
	ConfDialogData *cdd = data;
	PaneKeywordsData *pkd = cdd->pkd;
	GtkTreeModel *model;

	GtkTreeModel *keyword_tree;
	GtkTreeIter kw_iter;
	
	gboolean have_dest = FALSE;
	
	GList *keywords;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	
        if (cdd->click_tpath)
		{
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter(model, &iter, cdd->click_tpath))
			{
			gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &kw_iter, &iter);
			have_dest = TRUE;
			}
		}
	
	if (cdd->edit_existing && !have_dest) return;
	
	keywords = keyword_list_pull(cdd->edit_widget);
	
	if (cdd->edit_existing)
		{
		if (keywords && keywords->data && /* there should be one keyword */
		    !keyword_exists(keyword_tree, NULL, &kw_iter, keywords->data, TRUE, NULL))
			{
			keyword_set(GTK_TREE_STORE(keyword_tree), &kw_iter, keywords->data, cdd->is_keyword);
			}
		}
	else
		{
		GList *work = keywords;
		gboolean append_to = FALSE;

		while (work)
			{
			GtkTreeIter add;
			if (keyword_exists(keyword_tree, NULL, (have_dest || append_to) ? &kw_iter : NULL, work->data, FALSE, NULL))
				{
				work = work->next;
				continue;
				}
			if (have_dest)
				{
				gtk_tree_store_append(GTK_TREE_STORE(keyword_tree), &add, &kw_iter);
				}
			else if (append_to)
				{
				gtk_tree_store_insert_after(GTK_TREE_STORE(keyword_tree), &add, NULL, &kw_iter);
				}
			else
				{
				gtk_tree_store_append(GTK_TREE_STORE(keyword_tree), &add, NULL);
				append_to = TRUE;
				kw_iter = add;
				}
			keyword_set(GTK_TREE_STORE(keyword_tree), &add, work->data, cdd->is_keyword);
			work = work->next;
			}
		}
	string_list_free(keywords);
}

static void bar_pane_keywords_conf_set_helper(GtkWidget *widget, gpointer data)
{
	ConfDialogData *cdd = data;
	cdd->is_keyword = FALSE;
}

static void bar_pane_keywords_conf_set_kw(GtkWidget *widget, gpointer data)
{
	ConfDialogData *cdd = data;
	cdd->is_keyword = TRUE;
}



static void bar_pane_keywords_edit_dialog(PaneKeywordsData *pkd, gboolean edit_existing)
{
	ConfDialogData *cdd;
	GenericDialog *gd;
	GtkWidget *table;
	GtkWidget *group;
	GtkWidget *button;
	
	gchar *name = NULL;
	gboolean is_keyword = TRUE;
	

        if (edit_existing && pkd->click_tpath)
		{
		GtkTreeModel *model;
		GtkTreeIter iter;
		model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));

	        if (gtk_tree_model_get_iter(model, &iter, pkd->click_tpath))
			{
			gtk_tree_model_get(model, &iter, FILTER_KEYWORD_COLUMN_NAME, &name,
							 FILTER_KEYWORD_COLUMN_IS_KEYWORD, &is_keyword, -1);
			}
		else
			{
			return;
			}
		}
		
	if (edit_existing && !name) return;
	

	cdd = g_new0(ConfDialogData, 1);
	cdd->pkd =pkd;
	cdd->click_tpath = pkd->click_tpath;
	pkd->click_tpath = NULL;
	cdd->edit_existing = edit_existing;

	cdd->gd = gd = generic_dialog_new(name ? _("Edit keyword") : _("Add keywords"), "keyword_edit",
				pkd->widget, TRUE,
				bar_pane_keywords_edit_cancel_cb, cdd);
	g_signal_connect(G_OBJECT(gd->dialog), "destroy",
			 G_CALLBACK(bar_pane_keywords_edit_destroy_cb), cdd);


	generic_dialog_add_message(gd, NULL, name ? _("Configure keyword") : _("Add keyword"), NULL);

	generic_dialog_add_button(gd, GTK_STOCK_OK, NULL,
				  bar_pane_keywords_edit_ok_cb, TRUE);

	table = pref_table_new(gd->vbox, 3, 1, FALSE, TRUE);
	pref_table_label(table, 0, 0, _("Keyword:"), 1.0);
	cdd->edit_widget = gtk_entry_new();
	gtk_widget_set_size_request(cdd->edit_widget, 300, -1);
	if (name) gtk_entry_set_text(GTK_ENTRY(cdd->edit_widget), name);
	gtk_table_attach_defaults(GTK_TABLE(table), cdd->edit_widget, 1, 2, 0, 1);
	/* here could eventually be a text view instead of entry */
	generic_dialog_attach_default(gd, cdd->edit_widget);
	gtk_widget_show(cdd->edit_widget);

	group = pref_group_new(gd->vbox, FALSE, _("Keyword type:"), GTK_ORIENTATION_VERTICAL);

	button = pref_radiobutton_new(group, NULL, _("Active keyword"),
				      (is_keyword),
				      G_CALLBACK(bar_pane_keywords_conf_set_kw), cdd);
	button = pref_radiobutton_new(group, button, _("Helper"),
				      (!is_keyword),
				      G_CALLBACK(bar_pane_keywords_conf_set_helper), cdd);

	cdd->is_keyword = is_keyword;

	g_free(name);

	gtk_widget_grab_focus(cdd->edit_widget);

	gtk_widget_show(gd->dialog);
}




/*
 *-------------------------------------------------------------------
 * popup menu
 *-------------------------------------------------------------------
 */

static void bar_pane_keywords_edit_dialog_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	bar_pane_keywords_edit_dialog(pkd, TRUE);
}

static void bar_pane_keywords_add_dialog_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	bar_pane_keywords_edit_dialog(pkd, FALSE);
}

static void bar_pane_keywords_connect_mark_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;

	GtkTreeModel *model;
	GtkTreeIter iter;

	GtkTreeModel *keyword_tree;
	GtkTreeIter kw_iter;

	gint mark = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu_widget), "mark")) - 1;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	
        if (!pkd->click_tpath) return;
        if (!gtk_tree_model_get_iter(model, &iter, pkd->click_tpath)) return;

	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &kw_iter, &iter);

	file_data_unregister_notify_func(bar_pane_keywords_notify_cb, pkd);

	meta_data_connect_mark_with_keyword(keyword_tree, &kw_iter, mark);

	file_data_register_notify_func(bar_pane_keywords_notify_cb, pkd, NOTIFY_PRIORITY_LOW);
//	bar_pane_keywords_update(pkd);
}


static void bar_pane_keywords_delete_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;
	GtkTreeIter iter;

	GtkTreeModel *keyword_tree;
	GtkTreeIter kw_iter;

        if (!pkd->click_tpath) return;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));

        if (!gtk_tree_model_get_iter(model, &iter, pkd->click_tpath)) return;
	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &kw_iter, &iter);
	
	keyword_delete(GTK_TREE_STORE(keyword_tree), &kw_iter);
}

static void bar_pane_keywords_hide_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;
	GtkTreeIter iter;

	GtkTreeModel *keyword_tree;
	GtkTreeIter kw_iter;

        if (!pkd->click_tpath) return;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));

        if (!gtk_tree_model_get_iter(model, &iter, pkd->click_tpath)) return;
	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &kw_iter, &iter);
	
	keyword_hide_in(GTK_TREE_STORE(keyword_tree), &kw_iter, model);
}

static void bar_pane_keywords_show_all_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;

	GtkTreeModel *keyword_tree;

	pkd->hide_unchecked = FALSE;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));

	keyword_show_all_in(GTK_TREE_STORE(keyword_tree), model);
	
	if (!pkd->collapse_unchecked) gtk_tree_view_expand_all(GTK_TREE_VIEW(pkd->keyword_treeview));
	bar_keyword_tree_sync(pkd);
}

static void bar_pane_keywords_expand_checked_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	gtk_tree_model_foreach(model, bar_keyword_tree_expand_if_set_cb, pkd);
}

static void bar_pane_keywords_collapse_unchecked_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	gtk_tree_model_foreach(model, bar_keyword_tree_collapse_if_unset_cb, pkd);
}

static void bar_pane_keywords_hide_unchecked_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeModel *model;

	GtkTreeModel *keyword_tree;
	GList *keywords;
	
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));

	keywords = keyword_list_pull(pkd->keyword_view);
	keyword_hide_unset_in(GTK_TREE_STORE(keyword_tree), model, keywords);
	string_list_free(keywords);
	bar_keyword_tree_sync(pkd);
}

static void bar_pane_keywords_expand_checked_toggle_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	pkd->expand_checked = !pkd->expand_checked;
	bar_keyword_tree_sync(pkd);
}

static void bar_pane_keywords_collapse_unchecked_toggle_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	pkd->collapse_unchecked = !pkd->collapse_unchecked;
	bar_keyword_tree_sync(pkd);
}

static void bar_pane_keywords_hide_unchecked_toggle_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	pkd->hide_unchecked = !pkd->hide_unchecked;
	bar_keyword_tree_sync(pkd);
}

/**
 * \brief Callback for adding selected keyword to all selected images.
 */
static void bar_pane_keywords_add_to_selected_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneKeywordsData *pkd = data;
	GtkTreeIter iter; /* This is the iter which initial holds the current keyword */
	GtkTreeIter child_iter;
	GtkTreeModel *model;
	GtkTreeModel *keyword_tree;
	GList *list, *work;
	GList *keywords = NULL;

	GtkTextBuffer *keyword_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(pkd->keyword_view));

	/* Aquire selected keyword */
	if (pkd->click_tpath)
		{
		gboolean is_keyword = TRUE;

		model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
	        if (!gtk_tree_model_get_iter(model, &iter, pkd->click_tpath)) return;
		gtk_tree_model_get(model, &iter, FILTER_KEYWORD_COLUMN_IS_KEYWORD, &is_keyword, -1);
		if (!is_keyword) return;
		}
	else
		return;

	keyword_tree = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, &iter);

	list = keyword_list_pull(pkd->keyword_view); /* Get the left keyword view */

	/* Now set the current image */
	keyword_tree_set(keyword_tree, &child_iter, &list);

	keyword_list_push(pkd->keyword_view, list); /* Set the left keyword view */
	string_list_free(list);

	bar_pane_keywords_changed(keyword_buffer, pkd); /* Get list of all keywords in the hierarchy */

	gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, &iter);
	keywords = keyword_tree_get(keyword_tree, &child_iter);

	list = layout_selection_list(pkd->pane.lw);
	work = list;
	while (work)
		{
		FileData *fd = work->data;
		work = work->next;
		metadata_append_list(fd, KEYWORD_KEY, keywords);
		}
	filelist_free(list);
	string_list_free(keywords);
}

static void bar_pane_keywords_menu_popup(GtkWidget *widget, PaneKeywordsData *pkd, gint x, gint y)
{
	GtkWidget *menu;
	GtkWidget *item;
	GtkWidget *submenu;
        GtkTreeViewDropPosition pos;
        
        if (pkd->click_tpath) gtk_tree_path_free(pkd->click_tpath);
        pkd->click_tpath = NULL;
	gtk_tree_view_get_dest_row_at_pos(GTK_TREE_VIEW(pkd->keyword_treeview), x, y, &pkd->click_tpath, &pos);

	menu = popup_menu_short_lived();

	menu_item_add_stock(menu, _("Add keyword"), GTK_STOCK_EDIT, G_CALLBACK(bar_pane_keywords_add_dialog_cb), pkd);
	
	menu_item_add_divider(menu);

	menu_item_add(menu, _("Add keyword to all selected images"), G_CALLBACK(bar_pane_keywords_add_to_selected_cb), pkd);

	menu_item_add_divider(menu);

	if (pkd->click_tpath)
		{
		/* for the entry */
		gchar *text;
		gchar *mark;
		gint i;
		
		GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(pkd->keyword_treeview));
		
		GtkTreeIter iter;
                gtk_tree_model_get_iter(model, &iter, pkd->click_tpath);
		gchar *name;
		
		gtk_tree_model_get(model, &iter, FILTER_KEYWORD_COLUMN_NAME, &name,
						 FILTER_KEYWORD_COLUMN_MARK, &mark, -1);
		
		text = g_strdup_printf(_("Hide \"%s\""), name);
		menu_item_add_stock(menu, text, GTK_STOCK_EDIT, G_CALLBACK(bar_pane_keywords_hide_cb), pkd);
		g_free(text);
		
		submenu = gtk_menu_new();
		for (i = 0; i < FILEDATA_MARKS_SIZE; i++)
			{
			text = g_strdup_printf(_("Mark %d"), i + 1);
			item = menu_item_add(submenu, text, G_CALLBACK(bar_pane_keywords_connect_mark_cb), pkd);
			g_object_set_data(G_OBJECT(item), "mark", GINT_TO_POINTER(i + 1));
			g_free(text);
			}
		text = g_strdup_printf(_("Connect \"%s\" to mark"), name);
		item = menu_item_add(menu, text, NULL, NULL);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);
		g_free(text);

		menu_item_add_divider(menu);

		text = g_strdup_printf(_("Edit \"%s\""), name);
		menu_item_add_stock(menu, text, GTK_STOCK_EDIT, G_CALLBACK(bar_pane_keywords_edit_dialog_cb), pkd);
		g_free(text);
		text = g_strdup_printf(_("Remove \"%s\""), name);
		menu_item_add_stock(menu, text, GTK_STOCK_DELETE, G_CALLBACK(bar_pane_keywords_delete_cb), pkd);
		g_free(text);

		
		if (mark && mark[0])
			{
			text = g_strdup_printf(_("Disconnect \"%s\" from mark %s"), name, mark);
			menu_item_add_stock(menu, text, GTK_STOCK_DELETE, G_CALLBACK(bar_pane_keywords_connect_mark_cb), pkd);
			g_free(text);
			}

		menu_item_add_divider(menu);
		g_free(mark);
		g_free(name);
		}
	/* for the pane */


	menu_item_add(menu, _("Expand checked"), G_CALLBACK(bar_pane_keywords_expand_checked_cb), pkd);
	menu_item_add(menu, _("Collapse unchecked"), G_CALLBACK(bar_pane_keywords_collapse_unchecked_cb), pkd);
	menu_item_add(menu, _("Hide unchecked"), G_CALLBACK(bar_pane_keywords_hide_unchecked_cb), pkd);
	menu_item_add(menu, _("Show all"), G_CALLBACK(bar_pane_keywords_show_all_cb), pkd);

	submenu = gtk_menu_new();
	item = menu_item_add(menu, _("On any change"), NULL, NULL);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), submenu);

	menu_item_add_check(submenu, _("Expand checked"), pkd->expand_checked, G_CALLBACK(bar_pane_keywords_expand_checked_toggle_cb), pkd);
	menu_item_add_check(submenu, _("Collapse unchecked"), pkd->collapse_unchecked, G_CALLBACK(bar_pane_keywords_collapse_unchecked_toggle_cb), pkd);
	menu_item_add_check(submenu, _("Hide unchecked"), pkd->hide_unchecked, G_CALLBACK(bar_pane_keywords_hide_unchecked_toggle_cb), pkd);

	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
}


static gboolean bar_pane_keywords_menu_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data) 
{ 
	PaneKeywordsData *pkd = data;
	if (bevent->button == MOUSE_BUTTON_RIGHT)
		{
		bar_pane_keywords_menu_popup(widget, pkd, bevent->x, bevent->y);
		return TRUE;
		}
	return FALSE;
} 

/*
 *-------------------------------------------------------------------
 * init
 *-------------------------------------------------------------------
 */

void bar_pane_keywords_close(GtkWidget *bar)
{
	PaneKeywordsData *pkd;

	pkd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pkd) return;
	
	g_free(pkd->pane.id);
	gtk_widget_destroy(pkd->widget);
}

static void bar_pane_keywords_destroy(GtkWidget *widget, gpointer data)
{
	PaneKeywordsData *pkd = data;

        if (pkd->click_tpath) gtk_tree_path_free(pkd->click_tpath);
	if (pkd->idle_id) g_source_remove(pkd->idle_id);
	file_data_unregister_notify_func(bar_pane_keywords_notify_cb, pkd);

	file_data_unref(pkd->fd);
	g_free(pkd->key);

	g_free(pkd);
}


static GtkWidget *bar_pane_keywords_new(const gchar *id, const gchar *title, const gchar *key, gboolean expanded)
{
	PaneKeywordsData *pkd;
	GtkWidget *hbox;
	GtkWidget *scrolled;
	GtkTextBuffer *buffer;
	GtkTreeModel *store;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;
	GtkTreeIter iter;

	pkd = g_new0(PaneKeywordsData, 1);

	pkd->pane.pane_set_fd = bar_pane_keywords_set_fd;
	pkd->pane.pane_event = bar_pane_keywords_event;
	pkd->pane.pane_write_config = bar_pane_keywords_write_config;
	pkd->pane.title = bar_pane_expander_title(title);
	pkd->pane.id = g_strdup(id);
	pkd->pane.type = PANE_KEYWORDS;

	pkd->pane.expanded = expanded;

	pkd->key = g_strdup(key);
	
	pkd->expand_checked = TRUE;
	
	hbox = gtk_hbox_new(FALSE, PREF_PAD_GAP);

	pkd->widget = hbox;
	g_object_set_data(G_OBJECT(pkd->widget), "pane_data", pkd);
	g_signal_connect(G_OBJECT(pkd->widget), "destroy",
			 G_CALLBACK(bar_pane_keywords_destroy), pkd);
	gtk_widget_show(hbox);

	scrolled = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled), GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
				       GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(hbox), scrolled, TRUE, TRUE, 0);
	gtk_widget_show(scrolled);

	pkd->keyword_view = gtk_text_view_new();
	gtk_container_add(GTK_CONTAINER(scrolled), pkd->keyword_view);
	g_signal_connect(G_OBJECT(pkd->keyword_view), "populate-popup",
			 G_CALLBACK(bar_pane_keywords_populate_popup_cb), pkd);
	gtk_widget_show(pkd->keyword_view);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(pkd->keyword_view));
	g_signal_connect(G_OBJECT(buffer), "changed",
			 G_CALLBACK(bar_pane_keywords_changed), pkd);

	scrolled = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled), GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
				       GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(hbox), scrolled, TRUE, TRUE, 0);
	gtk_widget_show(scrolled);


	if (!keyword_tree || !gtk_tree_model_get_iter_first(GTK_TREE_MODEL(keyword_tree), &iter))
		{
		/* keyword tree does not exist or is empty - fill with defaults */
		keyword_tree_new_default();
		}

	store = gtk_tree_model_filter_new(GTK_TREE_MODEL(keyword_tree), NULL);

	gtk_tree_model_filter_set_modify_func(GTK_TREE_MODEL_FILTER(store),
					      FILTER_KEYWORD_COLUMN_COUNT,
					      filter_keyword_column_types,
					      bar_pane_keywords_filter_modify,
					      pkd,
					      NULL);
	gtk_tree_model_filter_set_visible_func(GTK_TREE_MODEL_FILTER(store),
					       bar_pane_keywords_filter_visible,
					       store,
					       NULL);

	pkd->keyword_treeview = gtk_tree_view_new_with_model(store);
	g_object_unref(store);
	
	gtk_widget_set_size_request(pkd->keyword_treeview, -1, 400);

	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(pkd->keyword_treeview), FALSE);

//	gtk_tree_view_set_search_column(GTK_TREE_VIEW(pkd->keyword_treeview), FILTER_KEYWORD_COLUMN_);

	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);

	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);

	gtk_tree_view_column_add_attribute(column, renderer, "text", FILTER_KEYWORD_COLUMN_MARK);

	gtk_tree_view_append_column(GTK_TREE_VIEW(pkd->keyword_treeview), column);

	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	renderer = gtk_cell_renderer_toggle_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_column_add_attribute(column, renderer, "active", FILTER_KEYWORD_COLUMN_TOGGLE);
	gtk_tree_view_column_add_attribute(column, renderer, "visible", FILTER_KEYWORD_COLUMN_IS_KEYWORD);
	g_signal_connect(G_OBJECT(renderer), "toggled",
			 G_CALLBACK(bar_pane_keywords_keyword_toggle), pkd);

	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_add_attribute(column, renderer, "text", FILTER_KEYWORD_COLUMN_NAME);

	gtk_tree_view_append_column(GTK_TREE_VIEW(pkd->keyword_treeview), column);
	gtk_tree_view_set_expander_column(GTK_TREE_VIEW(pkd->keyword_treeview), column);

	gtk_drag_source_set(pkd->keyword_treeview,
			    GDK_BUTTON1_MASK | GDK_BUTTON2_MASK,
			    bar_pane_keywords_drag_types, n_keywords_drag_types,
			    GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK);

	g_signal_connect(G_OBJECT(pkd->keyword_treeview), "drag_data_get",
			 G_CALLBACK(bar_pane_keywords_dnd_get), pkd);

	g_signal_connect(G_OBJECT(pkd->keyword_treeview), "drag_begin",
			 G_CALLBACK(bar_pane_keywords_dnd_begin), pkd);
	g_signal_connect(G_OBJECT(pkd->keyword_treeview), "drag_end",
			 G_CALLBACK(bar_pane_keywords_dnd_end), pkd);

	gtk_drag_dest_set(pkd->keyword_treeview,
			  GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_HIGHLIGHT | GTK_DEST_DEFAULT_DROP,
			  bar_pane_keywords_drop_types, n_keywords_drop_types,
			  GDK_ACTION_COPY | GDK_ACTION_MOVE);
			  
	g_signal_connect(G_OBJECT(pkd->keyword_treeview), "drag_data_received",
			 G_CALLBACK(bar_pane_keywords_dnd_receive), pkd);

	g_signal_connect(G_OBJECT(pkd->keyword_treeview), "drag_motion",
			 G_CALLBACK(bar_pane_keywords_dnd_motion), pkd);

	g_signal_connect(G_OBJECT(pkd->keyword_treeview), "button_release_event", 
			 G_CALLBACK(bar_pane_keywords_menu_cb), pkd);
	
	gtk_container_add(GTK_CONTAINER(scrolled), pkd->keyword_treeview);
	gtk_widget_show(pkd->keyword_treeview);

	file_data_register_notify_func(bar_pane_keywords_notify_cb, pkd, NOTIFY_PRIORITY_LOW);

	return pkd->widget;
}

GtkWidget *bar_pane_keywords_new_from_config(const gchar **attribute_names, const gchar **attribute_values)
{
	gchar *id = g_strdup("keywords");
	gchar *title = NULL;
	gchar *key = g_strdup(COMMENT_KEY);
	gboolean expanded = TRUE;
	GtkWidget *ret;

	while (*attribute_names)
		{
		const gchar *option = *attribute_names++;
		const gchar *value = *attribute_values++;

		if (READ_CHAR_FULL("id", id)) continue;
		if (READ_CHAR_FULL("title", title)) continue;
		if (READ_CHAR_FULL("key", key)) continue;
		if (READ_BOOL_FULL("expanded", expanded)) continue;
		

		log_printf("unknown attribute %s = %s\n", option, value);
		}
	
	bar_pane_translate_title(PANE_KEYWORDS, id, &title);
	ret = bar_pane_keywords_new(id, title, key, expanded);
	g_free(id);
	g_free(title);
	g_free(key);
	return ret;
}

void bar_pane_keywords_update_from_config(GtkWidget *pane, const gchar **attribute_names, const gchar **attribute_values)
{
	PaneKeywordsData *pkd;

	pkd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pkd) return;

	gchar *title = NULL;

	while (*attribute_names)
		{
		const gchar *option = *attribute_names++;
		const gchar *value = *attribute_values++;

		if (READ_CHAR_FULL("title", title)) continue;
		if (READ_CHAR_FULL("key", pkd->key)) continue;
		if (READ_BOOL_FULL("expanded", pkd->pane.expanded)) continue;
		if (READ_CHAR_FULL("id", pkd->pane.id)) continue;
		

		log_printf("unknown attribute %s = %s\n", option, value);
		}

	if (title)
		{
		bar_pane_translate_title(PANE_KEYWORDS, pkd->pane.id, &title);
		gtk_label_set_text(GTK_LABEL(pkd->pane.title), title);
		g_free(title);
		}

	bar_update_expander(pane);
	bar_pane_keywords_update(pkd);
}


/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
