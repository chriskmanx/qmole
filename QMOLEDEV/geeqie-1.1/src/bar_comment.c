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


#include "main.h"
#include "bar_comment.h"

#include "bar.h"
#include "metadata.h"
#include "filedata.h"
#include "ui_menu.h"
#include "ui_misc.h"
#include "rcfile.h"
#include "layout.h"

static void bar_pane_comment_changed(GtkTextBuffer *buffer, gpointer data);

/*
 *-------------------------------------------------------------------
 * keyword / comment utils
 *-------------------------------------------------------------------
 */



typedef struct _PaneCommentData PaneCommentData;
struct _PaneCommentData
{
	PaneData pane;
	GtkWidget *widget;
	GtkWidget *comment_view;
	FileData *fd;
	gchar *key;
	gint height;
};


static void bar_pane_comment_write(PaneCommentData *pcd)
{
	gchar *comment;

	if (!pcd->fd) return;

	comment = text_widget_text_pull(pcd->comment_view);

	metadata_write_string(pcd->fd, pcd->key, comment);
	g_free(comment);
}


static void bar_pane_comment_update(PaneCommentData *pcd)
{
	gchar *comment = NULL;
	GtkTextBuffer *comment_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(pcd->comment_view));

	g_signal_handlers_block_by_func(comment_buffer, bar_pane_comment_changed, pcd);

	comment = metadata_read_string(pcd->fd, pcd->key, METADATA_PLAIN);
	gtk_text_buffer_set_text(comment_buffer,
				 (comment) ? comment : "", -1);
	g_free(comment);
	
	g_signal_handlers_unblock_by_func(comment_buffer, bar_pane_comment_changed, pcd);

	gtk_widget_set_sensitive(pcd->comment_view, (pcd->fd != NULL));
}

static void bar_pane_comment_set_selection(PaneCommentData *pcd, gboolean append)
{
	GList *list = NULL;
	GList *work;
	gchar *comment = NULL;

	comment = text_widget_text_pull(pcd->comment_view);

	list = layout_selection_list(pcd->pane.lw);
	list = file_data_process_groups_in_selection(list, FALSE, NULL);
	
	work = list;
	while (work)
		{
		FileData *fd = work->data;
		work = work->next;
		if (fd == pcd->fd) continue;

		if (append)
			{
			metadata_append_string(fd, pcd->key, comment);
			}
		else
			{
			metadata_write_string(fd, pcd->key, comment);
			}
		}

	filelist_free(list);
	g_free(comment);
}

static void bar_pane_comment_sel_add_cb(GtkWidget *button, gpointer data)
{
	PaneCommentData *pcd = data;

	bar_pane_comment_set_selection(pcd, TRUE);
}

static void bar_pane_comment_sel_replace_cb(GtkWidget *button, gpointer data)
{
	PaneCommentData *pcd = data;

	bar_pane_comment_set_selection(pcd, FALSE);
}


static void bar_pane_comment_set_fd(GtkWidget *bar, FileData *fd)
{
	PaneCommentData *pcd;

	pcd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pcd) return;

	file_data_unref(pcd->fd);
	pcd->fd = file_data_ref(fd);

	bar_pane_comment_update(pcd);
}

static gint bar_pane_comment_event(GtkWidget *bar, GdkEvent *event)
{
	PaneCommentData *pcd;

	pcd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pcd) return FALSE;

#if GTK_CHECK_VERSION(2,20,0)
	if (gtk_widget_has_focus(pcd->comment_view)) return gtk_widget_event(pcd->comment_view, event);
#else
	if (GTK_WIDGET_HAS_FOCUS(pcd->comment_view)) return gtk_widget_event(pcd->comment_view, event);
#endif

	return FALSE;
}

static void bar_pane_comment_write_config(GtkWidget *pane, GString *outstr, gint indent)
{
	PaneCommentData *pcd;

	pcd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pcd) return;

	WRITE_NL(); WRITE_STRING("<pane_comment ");
	write_char_option(outstr, indent, "id", pcd->pane.id);
	write_char_option(outstr, indent, "title", gtk_label_get_text(GTK_LABEL(pcd->pane.title)));
	WRITE_BOOL(pcd->pane, expanded);
	WRITE_CHAR(*pcd, key);
	WRITE_INT(*pcd, height); 
	WRITE_STRING("/>");
}

static void bar_pane_comment_notify_cb(FileData *fd, NotifyType type, gpointer data)
{
	PaneCommentData *pcd = data;
	if ((type & (NOTIFY_REREAD | NOTIFY_CHANGE | NOTIFY_METADATA)) && fd == pcd->fd) 
		{
		DEBUG_1("Notify pane_comment: %s %04x", fd->path, type);

		bar_pane_comment_update(pcd);
		}
}

static void bar_pane_comment_changed(GtkTextBuffer *buffer, gpointer data)
{
	PaneCommentData *pcd = data;

	file_data_unregister_notify_func(bar_pane_comment_notify_cb, pcd);
	bar_pane_comment_write(pcd);
	file_data_register_notify_func(bar_pane_comment_notify_cb, pcd, NOTIFY_PRIORITY_LOW);
}


static void bar_pane_comment_populate_popup(GtkTextView *textview, GtkMenu *menu, gpointer data)
{
	PaneCommentData *pcd = data;

	menu_item_add_divider(GTK_WIDGET(menu));
	menu_item_add_stock(GTK_WIDGET(menu), _("Add text to selected files"), GTK_STOCK_ADD, G_CALLBACK(bar_pane_comment_sel_add_cb), pcd);
	menu_item_add_stock(GTK_WIDGET(menu), _("Replace existing text in selected files"), GTK_STOCK_CONVERT, G_CALLBACK(bar_pane_comment_sel_replace_cb), data);
}

#if 0
static void bar_pane_comment_close(GtkWidget *bar)
{
	PaneCommentData *pcd;

	pcd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pcd) return;

	gtk_widget_destroy(pcd->comment_view);
}
#endif

static void bar_pane_comment_destroy(GtkWidget *widget, gpointer data)
{
	PaneCommentData *pcd = data;

	file_data_unregister_notify_func(bar_pane_comment_notify_cb, pcd);

	file_data_unref(pcd->fd);
	g_free(pcd->key);

	g_free(pcd->pane.id);

	g_free(pcd);
}


static GtkWidget *bar_pane_comment_new(const gchar *id, const gchar *title, const gchar *key, gboolean expanded, gint height)
{
	PaneCommentData *pcd;
	GtkWidget *scrolled;
	GtkTextBuffer *buffer;

	pcd = g_new0(PaneCommentData, 1);
	
	pcd->pane.pane_set_fd = bar_pane_comment_set_fd;
	pcd->pane.pane_event = bar_pane_comment_event;
	pcd->pane.pane_write_config = bar_pane_comment_write_config;
	pcd->pane.title = bar_pane_expander_title(title);
	pcd->pane.id = g_strdup(id);
	pcd->pane.type = PANE_COMMENT;

	pcd->pane.expanded = expanded;
	
	pcd->key = g_strdup(key);
	pcd->height = height;

	scrolled = gtk_scrolled_window_new(NULL, NULL);
	
	pcd->widget = scrolled;
	g_object_set_data(G_OBJECT(pcd->widget), "pane_data", pcd);
	g_signal_connect(G_OBJECT(pcd->widget), "destroy",
			 G_CALLBACK(bar_pane_comment_destroy), pcd);
	
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled), GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
				       GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

	gtk_widget_set_size_request(pcd->widget, -1, height);
	gtk_widget_show(scrolled);

	pcd->comment_view = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(pcd->comment_view), GTK_WRAP_WORD);
	gtk_container_add(GTK_CONTAINER(scrolled), pcd->comment_view);
	g_signal_connect(G_OBJECT(pcd->comment_view), "populate-popup",
			 G_CALLBACK(bar_pane_comment_populate_popup), pcd);
	gtk_widget_show(pcd->comment_view);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(pcd->comment_view));
	g_signal_connect(G_OBJECT(buffer), "changed",
			 G_CALLBACK(bar_pane_comment_changed), pcd);


	file_data_register_notify_func(bar_pane_comment_notify_cb, pcd, NOTIFY_PRIORITY_LOW);

	return pcd->widget;
}

GtkWidget *bar_pane_comment_new_from_config(const gchar **attribute_names, const gchar **attribute_values)
{
	gchar *title = NULL;
	gchar *key = g_strdup(COMMENT_KEY);
	gboolean expanded = TRUE;
	gint height = 50;
	gchar *id = g_strdup("comment");
	GtkWidget *ret;

	while (*attribute_names)
		{
		const gchar *option = *attribute_names++;
		const gchar *value = *attribute_values++;

		if (READ_CHAR_FULL("title", title)) continue;
		if (READ_CHAR_FULL("key", key)) continue;
		if (READ_BOOL_FULL("expanded", expanded)) continue;
		if (READ_INT_FULL("height", height)) continue;
		if (READ_CHAR_FULL("id", id)) continue;
		

		log_printf("unknown attribute %s = %s\n", option, value);
		}
	
	bar_pane_translate_title(PANE_COMMENT, id, &title);
	ret = bar_pane_comment_new(id, title, key, expanded, height);
	g_free(title);
	g_free(key);
	g_free(id);
	return ret;
}

void bar_pane_comment_update_from_config(GtkWidget *pane, const gchar **attribute_names, const gchar **attribute_values)
{
	PaneCommentData *pcd;

	pcd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pcd) return;

	gchar *title = NULL;

	while (*attribute_names)
		{
		const gchar *option = *attribute_names++;
		const gchar *value = *attribute_values++;

		if (READ_CHAR_FULL("title", title)) continue;
		if (READ_CHAR_FULL("key", pcd->key)) continue;
		if (READ_BOOL_FULL("expanded", pcd->pane.expanded)) continue;
		if (READ_INT_FULL("height", pcd->height)) continue;
		if (READ_CHAR_FULL("id", pcd->pane.id)) continue;
		

		log_printf("unknown attribute %s = %s\n", option, value);
		}

	if (title)
		{
		bar_pane_translate_title(PANE_COMMENT, pcd->pane.id, &title);
		gtk_label_set_text(GTK_LABEL(pcd->pane.title), title);
		g_free(title);
		}
	gtk_widget_set_size_request(pcd->widget, -1, pcd->height);
	bar_update_expander(pane);
	bar_pane_comment_update(pcd);
}

/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
