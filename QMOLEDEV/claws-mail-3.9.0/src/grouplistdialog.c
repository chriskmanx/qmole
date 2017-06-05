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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <string.h>

#include "grouplistdialog.h"
#include "mainwindow.h"
#include "manage_window.h"
#include "gtkutils.h"
#include "utils.h"
#include "news.h"
#include "folder.h"
#include "alertpanel.h"
#include "recv.h"
#include "socket.h"
#include "prefs_common.h"

#define GROUPLIST_DIALOG_WIDTH		450
#define GROUPLIST_DIALOG_HEIGHT		400
#define GROUPLIST_COL_NAME_WIDTH	250

static gboolean ack;
static gboolean locked;

static GtkWidget *dialog;
static GtkWidget *entry;
static GtkWidget *ctree;
static GtkWidget *status_label;
static GtkWidget *ok_button;
static GSList *group_list;
static Folder *news_folder;

static GSList *subscribed;

static void grouplist_dialog_create	(void);
static void grouplist_dialog_set_list	(const gchar	*pattern,
					 gboolean	 refresh);
static void grouplist_search		(void);
static void grouplist_clear		(void);
static gboolean grouplist_recv_func	(SockInfo	*sock,
					 gint		 count,
					 gint		 read_bytes,
					 gpointer	 data);
static void grouplist_size_allocate	(GtkWidget *widget,
					 GtkAllocation *allocation);

static gint window_deleted	(GtkWidget	*widget,
				 GdkEventAny	*event,
				 gpointer	 data);
static void ok_clicked		(GtkWidget	*widget,
				 gpointer	 data);
static void cancel_clicked	(GtkWidget	*widget,
				 gpointer	 data);
static void refresh_clicked	(GtkWidget	*widget,
				 gpointer	 data);
static gboolean key_pressed	(GtkWidget	*widget,
				 GdkEventKey	*event,
				 gpointer	 data);
static gboolean button_press_cb (GtkCMCTree	*ctree,
				 GdkEventButton	*button,
				 gpointer	 data);
static void entry_activated	(GtkEditable	*editable);
static void search_clicked	(GtkWidget	*widget,
				 gpointer	 data);

GSList *grouplist_dialog(Folder *folder)
{
	GNode *node;
	FolderItem *item;

	if (dialog && gtk_widget_get_visible(dialog)) return NULL;

	if (!dialog)
		grouplist_dialog_create();

	news_folder = folder;

	gtk_widget_show(dialog);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	manage_window_set_transient(GTK_WINDOW(dialog));
	gtk_widget_grab_focus(ok_button);
	gtk_widget_grab_focus(ctree);
	GTK_EVENTS_FLUSH();

	subscribed = NULL;
	for (node = folder->node->children; node != NULL; node = node->next) {
		item = FOLDER_ITEM(node->data);
		subscribed = g_slist_append(subscribed, g_strdup(item->path));
	}

	grouplist_dialog_set_list(NULL, TRUE);

	if (ack) gtk_main();

	manage_window_focus_out(dialog, NULL, NULL);
	gtk_widget_hide(dialog);

	if (!ack) {
		slist_free_strings_full(subscribed);
		subscribed = NULL;

		for (node = folder->node->children; node != NULL;
		     node = node->next) {
			item = FOLDER_ITEM(node->data);
			subscribed = g_slist_append(subscribed,
						    g_strdup(item->path));
		}
	}

	grouplist_clear();

	return subscribed;
}

static void grouplist_dialog_create(void)
{
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *msg_label;
	GtkWidget *search_button;
	GtkWidget *confirm_area;
	GtkWidget *cancel_button;	
	GtkWidget *refresh_button;	
	GtkWidget *scrolledwin;
	static GdkGeometry geometry;
	gchar *titles[3];
	gint i;

	dialog = gtk_dialog_new();
	gtk_window_set_resizable(GTK_WINDOW(dialog), TRUE);
	gtk_container_set_border_width
		(GTK_CONTAINER(gtk_dialog_get_action_area(GTK_DIALOG(dialog))), 5);
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Newsgroup subscription"));
	g_signal_connect(G_OBJECT(dialog), "delete_event",
			 G_CALLBACK(window_deleted), NULL);
	g_signal_connect(G_OBJECT(dialog), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	g_signal_connect(G_OBJECT(dialog), "size_allocate",
			 G_CALLBACK(grouplist_size_allocate), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(dialog);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(
				gtk_dialog_get_content_area(GTK_DIALOG(dialog))), vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 8);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	msg_label = gtk_label_new(_("Select newsgroups for subscription:"));
	gtk_box_pack_start(GTK_BOX(hbox), msg_label, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(FALSE, 8);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	msg_label = gtk_label_new(_("Find groups:"));
	gtk_box_pack_start(GTK_BOX(hbox), msg_label, FALSE, FALSE, 0);

	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_signal_connect(G_OBJECT(entry), "activate",
			 G_CALLBACK(entry_activated), NULL);

	search_button = gtk_button_new_with_label(_(" Search "));
	gtk_box_pack_start(GTK_BOX(hbox), search_button, FALSE, FALSE, 0);

	g_signal_connect(G_OBJECT(search_button), "clicked",
			 G_CALLBACK(search_clicked), NULL);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_box_pack_start(GTK_BOX (vbox), scrolledwin, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	titles[0] = _("Newsgroup name");
	titles[1] = _("Messages");
	titles[2] = _("Type");
	ctree = gtk_sctree_new_with_titles(3, 0, titles);
	gtk_container_add(GTK_CONTAINER(scrolledwin), ctree);
	gtk_cmclist_set_column_width
		(GTK_CMCLIST(ctree), 0, GROUPLIST_COL_NAME_WIDTH);
	gtk_cmclist_set_column_auto_resize(GTK_CMCLIST(ctree), 0, TRUE);
	gtk_cmclist_set_selection_mode(GTK_CMCLIST(ctree), GTK_SELECTION_MULTIPLE);
	
	gtk_cmctree_set_line_style(GTK_CMCTREE(ctree), GTK_CMCTREE_LINES_NONE);
	gtk_cmctree_set_expander_style(GTK_CMCTREE(ctree),
				GTK_CMCTREE_EXPANDER_TRIANGLE);

	for (i = 0; i < 3; i++)
		gtkut_widget_set_can_focus(GTK_CMCLIST(ctree)->column[i].button, FALSE);
	g_signal_connect(G_OBJECT(ctree), "button-press-event",
			 G_CALLBACK(button_press_cb), NULL);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	status_label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(hbox), status_label, FALSE, FALSE, 0);

	gtkut_stock_button_set_create(&confirm_area,
				      &refresh_button, GTK_STOCK_REFRESH,
				      &cancel_button, GTK_STOCK_CANCEL,
				      &ok_button, GTK_STOCK_OK);
	gtk_container_add(GTK_CONTAINER(gtk_dialog_get_action_area(GTK_DIALOG(dialog))),
			  confirm_area);
	gtk_widget_grab_default(ok_button);

	g_signal_connect(G_OBJECT(ok_button), "clicked",
			 G_CALLBACK(ok_clicked), NULL);
	g_signal_connect(G_OBJECT(cancel_button), "clicked",
			 G_CALLBACK(cancel_clicked), NULL);
	g_signal_connect(G_OBJECT(refresh_button), "clicked",
			 G_CALLBACK(refresh_clicked), NULL);

	if (!geometry.min_width) {
		geometry.min_width = GROUPLIST_DIALOG_WIDTH;
		geometry.min_height = GROUPLIST_DIALOG_HEIGHT;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(dialog), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_window_set_default_size(GTK_WINDOW(dialog),
					prefs_common.news_subscribe_width,
					prefs_common.news_subscribe_height);

	gtk_widget_show_all(gtk_dialog_get_content_area(GTK_DIALOG(dialog)));
}

static GHashTable *branch_node_table;

static void grouplist_hash_init(void)
{
	branch_node_table = g_hash_table_new(g_str_hash, g_str_equal);
}

static void grouplist_hash_done(void)
{
	hash_free_strings(branch_node_table);
	g_hash_table_destroy(branch_node_table);
}

static GtkCMCTreeNode *grouplist_hash_get_branch_node(const gchar *name)
{
	return g_hash_table_lookup(branch_node_table, name);
}

static void grouplist_hash_set_branch_node(const gchar *name,
					   GtkCMCTreeNode *node)
{
	g_hash_table_insert(branch_node_table, g_strdup(name), node);
}

static gchar *grouplist_get_parent_name(const gchar *name)
{
	gchar *p;

	p = strrchr(name, '.');
	if (!p)
		return g_strdup("");
	return g_strndup(name, p - name);
}

static GtkCMCTreeNode *grouplist_create_parent(const gchar *name,
					     const gchar *pattern)
{
	GtkCMCTreeNode *parent;
	GtkCMCTreeNode *node;
	gchar *cols[3];
	gchar *parent_name;

	if (*name == '\0') return NULL;
	node = grouplist_hash_get_branch_node(name);
	if (node != NULL) return node;

	cols[0] = (gchar *)name;
	cols[1] = cols[2] = "";

	parent_name = grouplist_get_parent_name(name);
	parent = grouplist_create_parent(parent_name, pattern);

	node = parent ? GTK_CMCTREE_ROW(parent)->children
		: GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	node = gtk_sctree_insert_node(GTK_CMCTREE(ctree), parent, node,
				     cols, 0, NULL, NULL,
				     FALSE, FALSE);
	if (parent && g_pattern_match_simple(pattern, parent_name) == FALSE)
		gtk_cmctree_expand(GTK_CMCTREE(ctree), parent);
	gtk_cmctree_node_set_selectable(GTK_CMCTREE(ctree), node, FALSE);

	grouplist_hash_set_branch_node(name, node);

	g_free(parent_name);

	return node;
}

static GtkCMCTreeNode *grouplist_create_branch(NewsGroupInfo *ginfo,
					     const gchar *pattern)
{
	GtkCMCTreeNode *node;
	GtkCMCTreeNode *parent;
	gchar *name = (gchar *)ginfo->name;
	gchar *parent_name;
	gchar *count_str;
	gchar *cols[3];
	gint count;

	count = ginfo->last - ginfo->first;
	if (count < 0)
		count = 0;
	count_str = itos(count);

	cols[0] = ginfo->name;
	cols[1] = count_str;
	if (ginfo->type == 'y')
		cols[2] = "";
	else if (ginfo->type == 'm')
		cols[2] = _("moderated");
	else if (ginfo->type == 'n')
		cols[2] = _("readonly");
	else
		cols[2] = _("unknown");

	parent_name = grouplist_get_parent_name(name);
	parent = grouplist_create_parent(parent_name, pattern);
	node = grouplist_hash_get_branch_node(name);
	if (node) {
		gtk_cmctree_set_node_info(GTK_CMCTREE(ctree), node, cols[0], 0,
					NULL, NULL, FALSE, FALSE);
		gtk_cmctree_node_set_text(GTK_CMCTREE(ctree), node, 1, cols[1]);
		gtk_cmctree_node_set_text(GTK_CMCTREE(ctree), node, 2, cols[2]);
	} else {
		node = parent ? GTK_CMCTREE_ROW(parent)->children
			: GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
		node = gtk_sctree_insert_node(GTK_CMCTREE(ctree), parent, node,
					     cols, 0, NULL, NULL,
					     TRUE, FALSE);
		if (parent &&
		    g_pattern_match_simple(pattern, parent_name) == FALSE)
			gtk_cmctree_expand(GTK_CMCTREE(ctree), parent);
	}
	gtk_cmctree_node_set_selectable(GTK_CMCTREE(ctree), node, TRUE);
	if (node)
		gtk_cmctree_node_set_row_data(GTK_CMCTREE(ctree), node, ginfo);

	g_free(parent_name);

	return node;
}

static void grouplist_expand_upwards(GtkCMCTree *ctree, const gchar *name) {
	const gchar *ptr;
	gchar *newname=g_malloc0(strlen(name));

	for (ptr=name; *ptr; ptr++) {
		if (*ptr == '.')
			gtk_cmctree_expand(ctree, 
				grouplist_hash_get_branch_node(newname));
		newname[ptr-name] = *ptr;
	}
	g_free(newname);
}

static void grouplist_dialog_set_list(const gchar *pattern, gboolean refresh)
{
	static GdkCursor *watch_cursor = NULL;
	GSList *cur;
	GtkCMCTreeNode *node;
	GPatternSpec *pspec;
	GdkWindow *window;

	if (locked) return;
	locked = TRUE;

	if (!pattern || *pattern == '\0')
		pattern = "*";

	if (!watch_cursor)
		watch_cursor = gdk_cursor_new(GDK_WATCH);
	window = gtk_widget_get_window(dialog);
	gdk_window_set_cursor(window, watch_cursor);
	main_window_cursor_wait(mainwindow_get_mainwindow());
	GTK_EVENTS_FLUSH();
	
	if (refresh) {
		ack = TRUE;
		grouplist_clear();
		recv_set_ui_func(grouplist_recv_func, NULL);
		group_list = news_get_group_list(news_folder);
		group_list = g_slist_reverse(group_list);
		recv_set_ui_func(NULL, NULL);
		if (group_list == NULL && ack == TRUE) {
			alertpanel_error(_("Can't retrieve newsgroup list."));
			locked = FALSE;
			gdk_window_set_cursor(window, NULL);
			main_window_cursor_normal(mainwindow_get_mainwindow());
			return;
		}
	} else
		gtk_cmclist_clear(GTK_CMCLIST(ctree));

	gtk_entry_set_text(GTK_ENTRY(entry), pattern);

	grouplist_hash_init();

	gtk_cmclist_freeze(GTK_CMCLIST(ctree));

	pspec = g_pattern_spec_new(pattern);

	for (cur = group_list; cur != NULL ; cur = cur->next) {
		NewsGroupInfo *ginfo = (NewsGroupInfo *)cur->data;

		if (g_pattern_match_string(pspec, ginfo->name)) {
			node = grouplist_create_branch(ginfo, pattern);
			if (g_slist_find_custom(subscribed, ginfo->name,
						(GCompareFunc)g_ascii_strcasecmp)
			    != NULL)
				gtk_cmctree_select(GTK_CMCTREE(ctree), node);
		}
	}
	for (cur = subscribed; cur; cur = g_slist_next(cur))
		grouplist_expand_upwards(GTK_CMCTREE(ctree), (gchar *)cur->data);

	g_pattern_spec_free(pspec);

	gtk_cmclist_thaw(GTK_CMCLIST(ctree));

	grouplist_hash_done();

	gtk_label_set_text(GTK_LABEL(status_label), _("Done."));

	gdk_window_set_cursor(window, NULL);
	main_window_cursor_normal(mainwindow_get_mainwindow());

	locked = FALSE;
}

static void grouplist_search(void)
{
	gchar *str;

	if (locked) return;

	str = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	grouplist_dialog_set_list(str, FALSE);
	g_free(str);
}

static void grouplist_clear(void)
{
	gtk_cmclist_clear(GTK_CMCLIST(ctree));
	gtk_entry_set_text(GTK_ENTRY(entry), "");
	news_group_list_free(group_list);
	group_list = NULL;
}

static gboolean grouplist_recv_func(SockInfo *sock, gint count, gint read_bytes,
				    gpointer data)
{
	gchar buf[BUFFSIZE];

	g_snprintf(buf, sizeof(buf),
		   _("%d newsgroups received (%s read)"),
		   count, to_human_readable((goffset)read_bytes));
	gtk_label_set_text(GTK_LABEL(status_label), buf);
	GTK_EVENTS_FLUSH();
	if (ack == FALSE)
		return FALSE;
	else
		return TRUE;
}

static void grouplist_size_allocate(GtkWidget *widget, GtkAllocation *allocation)
{
	cm_return_if_fail( allocation != NULL );
	
	prefs_common.news_subscribe_width	= allocation->width;
	prefs_common.news_subscribe_height	= allocation->height;
}

static gint window_deleted(GtkWidget *widget, GdkEventAny *event, gpointer data)
{
	ack = FALSE;
	if (gtk_main_level() > 1)
		gtk_main_quit();

	return TRUE;
}

static void ok_clicked(GtkWidget *widget, gpointer data)
{
	ack = TRUE;
	if (gtk_main_level() > 1)
		gtk_main_quit();
}

static void cancel_clicked(GtkWidget *widget, gpointer data)
{
	ack = FALSE;
	if (gtk_main_level() > 1)
		gtk_main_quit();
}

static void refresh_clicked(GtkWidget *widget, gpointer data)
{ 
	gchar *str;

	if (locked) return;

	news_remove_group_list_cache(news_folder);

	str = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	grouplist_dialog_set_list(str, TRUE);
	g_free(str);
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape)
		cancel_clicked(NULL, NULL);
	return FALSE;
}

/* clist/ctree clear old selection on click (gtk2 only)
 * - intercept all button clicks (always return TRUE)
 * - only allow left button single click
 * - handle click on expander
 * - update "subscribed" list and un-/select row
 */
static gboolean button_press_cb(GtkCMCTree *ctree, GdkEventButton *button,
				gpointer data)
{
	gint row, col;
	GtkCMCTreeNode *node;
	NewsGroupInfo *ginfo;
	GSList *list;

	if (button->type != GDK_BUTTON_PRESS) return TRUE;
	if (button->button != 1) return TRUE;

	gtk_cmclist_get_selection_info(GTK_CMCLIST(ctree), 
				     button->x, button->y, &row, &col);
	node = gtk_cmctree_node_nth(ctree, row);
	if (!node) return TRUE;

	if (gtk_cmctree_is_hot_spot(ctree, button->x, button->y)) {
		gtk_cmctree_toggle_expansion(ctree, node);
		return TRUE;
	}

	ginfo = gtk_cmctree_node_get_row_data(ctree, node);
	if (!ginfo) return TRUE;

	list = g_slist_find_custom(subscribed, ginfo->name,
				   (GCompareFunc)g_ascii_strcasecmp);
	if (list) {
		g_free(list->data);
		subscribed = g_slist_remove(subscribed, list->data);
		gtk_cmclist_unselect_row(GTK_CMCLIST(ctree), row, 0);
	} else {
		subscribed = g_slist_append(subscribed, g_strdup(ginfo->name));
		gtk_cmclist_select_row(GTK_CMCLIST(ctree), row, 0);
	}

	return TRUE;
}

static void entry_activated(GtkEditable *editable)
{
	grouplist_search();
}

static void search_clicked(GtkWidget *widget, gpointer data)
{
	grouplist_search();
}
