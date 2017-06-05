/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "prefs_gtk.h"
#include "prefs_display_header.h"
#include "prefs_common.h"
#include "manage_window.h"
#include "alertpanel.h"
#include "displayheader.h"
#include "utils.h"
#include "gtkutils.h"

enum {
	PREFS_HDR_HEADER,
	PREFS_HDR_DATA,
	N_PREFS_HDR_COLUMNS
};

static struct DisplayHeader {
	GtkWidget *window;

	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;

	GtkWidget *hdr_combo;
	GtkWidget *key_check;
	GtkWidget *headers_list_view;
	GtkWidget *hidden_headers_list_view;

	GtkWidget *other_headers;
} dispheader;

/* widget creating functions */
static void prefs_display_header_create	(void);

static void prefs_display_header_set_dialog	(void);
static void prefs_display_header_set_list	(void);
static void prefs_display_header_list_view_set_row	(gboolean hidden);

/* callback functions */
static void prefs_display_header_register_cb	(GtkButton	*btn,
						 gpointer	 hidden_data);
static void prefs_display_header_delete_cb	(GtkButton	*btn,
						 gpointer	 list_view_data);
static void prefs_display_header_up		(void);
static void prefs_display_header_down		(void);

static gboolean prefs_display_header_key_pressed	(GtkWidget	*widget,
							 GdkEventKey	*event,
							 gpointer	 data);
static void prefs_display_header_ok		(void);
static void prefs_display_header_cancel		(void);
static gint prefs_display_header_deleted	(GtkWidget	*widget,
						 GdkEventAny	*event,
						 gpointer	 data);


static GtkListStore *prefs_display_header_create_store	(void);
static void prefs_display_header_insert_header		(GtkListStore *store,
							 gchar *name,
							 DisplayHeaderProp *dp);
static GtkWidget *prefs_display_header_list_view_create	(const gchar *name);
static void prefs_filtering_create_list_view_columns	(GtkWidget *list_view, 
							 const gchar *name);
static void headers_list_model_rows_reordered		(GtkTreeModel *model,
							 GtkTreePath  *path, 
							 GtkTreeIter  *iter,
							 gpointer      arg,
							 GtkTreeView  *list_view);
							 
static void drag_end	(GtkTreeView *list_view,
			 GdkDragContext *context,
			 gpointer data);

#ifndef GENERIC_UMPC
static gchar *defaults[] =
{
	"From",
	"To",
	"Cc",
	"Subject",
	"Date",	
	"Reply-To",
	"Sender",
	"User-Agent",
	"X-Mailer",	
	"Newsgroups",
	"Followup-To",
	"Organization",
	"X-Newsreader",
	"-Received",
	"-Message-ID",
	"-In-Reply-To",
	"-References",
	"-Mime-Version",
	"-Content-Type",
	"-Content-Transfer-Encoding",
	"-X-UIDL",
	"-Precedence",
	"-Status",
	"-Priority",
	"-X-Face"
};
#else
static gchar *defaults[] =
{
	"From",
	"To",
	"Cc",
	"Subject",
	"Date",	
	"Newsgroups",
	"Followup-To",
	"-Reply-To",
	"-Sender",
	"-User-Agent",
	"-X-Mailer",	
	"-Organization",
	"-X-Newsreader",
	"-Received",
	"-Message-ID",
	"-In-Reply-To",
	"-References",
	"-Mime-Version",
	"-Content-Type",
	"-Content-Transfer-Encoding",
	"-X-UIDL",
	"-Precedence",
	"-Status",
	"-Priority",
	"-X-Face"
};
#endif
static void prefs_display_header_set_default(void)
{
	gint i;
	DisplayHeaderProp *dp;

	for(i = 0; i < sizeof(defaults) / sizeof(defaults[0]); i++) {
		dp = display_header_prop_read_str(defaults[i]);
		prefs_common.disphdr_list =
			g_slist_append(prefs_common.disphdr_list, dp);
	}
}

void prefs_display_header_open(void)
{
	if (prefs_rc_is_readonly(DISPLAY_HEADER_RC))
		return;

	if (!dispheader.window) {
		prefs_display_header_create();
	}

	manage_window_set_transient(GTK_WINDOW(dispheader.window));
	gtk_widget_grab_focus(dispheader.ok_btn);

	prefs_display_header_set_dialog();

	gtk_widget_show(dispheader.window);
	gtk_window_set_modal(GTK_WINDOW(dispheader.window), TRUE);
}

static void prefs_display_header_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *btn_hbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *confirm_area;

	GtkWidget *vbox1;

	GtkWidget *hbox1;
	GtkWidget *hdr_label;
	GtkWidget *hdr_combo;

	GtkWidget *btn_vbox;
	GtkWidget *reg_btn;
	GtkWidget *del_btn;
	GtkWidget *up_btn;
	GtkWidget *down_btn;

	GtkWidget *list_view_hbox;
	GtkWidget *list_view_hbox1;
	GtkWidget *list_view_hbox2;
	GtkWidget *list_view_scrolledwin;
	GtkWidget *headers_list_view;
	GtkWidget *hidden_headers_list_view;

	GtkWidget *checkbtn_other_headers;
	gint i;
	
	debug_print("Creating display header setting window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_display_header");
	gtk_container_set_border_width (GTK_CONTAINER (window), 8);
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW (window), TRUE);

	vbox = gtk_vbox_new (FALSE, 6);
	gtk_widget_show (vbox);
	gtk_container_add (GTK_CONTAINER (window), vbox);

	btn_hbox = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (btn_hbox);
	gtk_box_pack_end (GTK_BOX (vbox), btn_hbox, FALSE, FALSE, 0);

	gtkut_stock_button_set_create(&confirm_area, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_widget_show (confirm_area);
	gtk_box_pack_end (GTK_BOX(btn_hbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default (ok_btn);

	gtk_window_set_title (GTK_WINDOW(window),
			      _("Displayed header configuration"));
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect (G_OBJECT(window), "delete_event",
			  G_CALLBACK(prefs_display_header_deleted),
			  NULL);
	g_signal_connect (G_OBJECT(window), "key_press_event",
			  G_CALLBACK(prefs_display_header_key_pressed),
			  NULL);
	g_signal_connect (G_OBJECT(ok_btn), "clicked",
			  G_CALLBACK(prefs_display_header_ok),
			  NULL);
	g_signal_connect (G_OBJECT(cancel_btn), "clicked",
			  G_CALLBACK(prefs_display_header_cancel),
			  NULL);

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_box_pack_start (GTK_BOX (vbox), vbox1, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), 2);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, TRUE, 0);

	hdr_label = gtk_label_new (_("Header name"));
	gtk_widget_show (hdr_label);
	gtk_box_pack_start (GTK_BOX (hbox1), hdr_label, FALSE, FALSE, 0);

	hdr_combo = gtk_combo_box_entry_new_text();
	for(i=0; i < 9 ; i++)
		gtk_combo_box_append_text(GTK_COMBO_BOX (hdr_combo),
			(*defaults[i] == '-') ? defaults[i]+1 : defaults[i]);
	gtk_combo_box_set_active(GTK_COMBO_BOX(hdr_combo), 0);
	gtk_widget_show (hdr_combo);
	gtk_box_pack_start (GTK_BOX (hbox1), hdr_combo, TRUE, TRUE, 0);
	gtk_widget_set_size_request (hdr_combo, 150, -1);

	list_view_hbox = gtk_hbox_new (FALSE, 10);
	gtk_widget_show (list_view_hbox);
	gtk_box_pack_start (GTK_BOX (vbox1), list_view_hbox, TRUE, TRUE, 0);

	/* display headers list */

	list_view_hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (list_view_hbox1);
	gtk_box_pack_start (GTK_BOX (list_view_hbox), list_view_hbox1, TRUE, TRUE, 0);

	list_view_scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_set_size_request (list_view_scrolledwin, 200, 210);
	gtk_widget_show (list_view_scrolledwin);
	gtk_box_pack_start (GTK_BOX (list_view_hbox1), list_view_scrolledwin,
			    TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (list_view_scrolledwin),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(list_view_scrolledwin),
					    GTK_SHADOW_IN);

	headers_list_view = prefs_display_header_list_view_create
				(_("Displayed Headers"));
	gtk_widget_show (headers_list_view);
	gtk_container_add(GTK_CONTAINER(list_view_scrolledwin), headers_list_view);
	gtk_tree_view_set_reorderable(GTK_TREE_VIEW(headers_list_view), TRUE);

	g_signal_connect(G_OBJECT(headers_list_view), "drag_end", 			 
			 G_CALLBACK(drag_end),
			 headers_list_view);
	
	/* connect rows change for this list view's model */
	g_signal_connect(G_OBJECT(gtk_tree_view_get_model(GTK_TREE_VIEW(headers_list_view))),
			 "rows-reordered", 
			 G_CALLBACK(headers_list_model_rows_reordered),
			 headers_list_view);

	btn_vbox = gtk_vbox_new (FALSE, 8);
	gtk_widget_show (btn_vbox);
	gtk_box_pack_start (GTK_BOX (list_view_hbox1), btn_vbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock (GTK_STOCK_ADD);
	gtk_widget_show (reg_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect (G_OBJECT (reg_btn), "clicked",
			  G_CALLBACK (prefs_display_header_register_cb),
			    GINT_TO_POINTER(FALSE));
	del_btn = gtk_button_new_from_stock (GTK_STOCK_REMOVE);
	gtk_widget_show (del_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), del_btn, FALSE, TRUE, 0);
	g_signal_connect (G_OBJECT (del_btn), "clicked",
			  G_CALLBACK (prefs_display_header_delete_cb),
			  headers_list_view);

	up_btn = gtk_button_new_from_stock (GTK_STOCK_GO_UP);
	gtk_widget_show (up_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), up_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (up_btn), "clicked",
			  G_CALLBACK (prefs_display_header_up), NULL);

	down_btn = gtk_button_new_from_stock (GTK_STOCK_GO_DOWN);
	gtk_widget_show (down_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), down_btn, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (down_btn), "clicked",
			  G_CALLBACK (prefs_display_header_down), NULL);

	/* hidden headers list */

	list_view_hbox2 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (list_view_hbox2);
	gtk_box_pack_start (GTK_BOX (list_view_hbox), list_view_hbox2, TRUE, TRUE, 0);

	list_view_scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_set_size_request (list_view_scrolledwin, 200, 210);
	gtk_widget_show (list_view_scrolledwin);
	gtk_box_pack_start (GTK_BOX (list_view_hbox2), list_view_scrolledwin,
			    TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (list_view_scrolledwin),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(list_view_scrolledwin),
					    GTK_SHADOW_IN);

	hidden_headers_list_view = prefs_display_header_list_view_create
					(_("Hidden headers"));
	gtk_widget_show (hidden_headers_list_view);
	gtk_container_add (GTK_CONTAINER (list_view_scrolledwin),
			   hidden_headers_list_view);

	btn_vbox = gtk_vbox_new (FALSE, 8);
	gtk_widget_show (btn_vbox);
	gtk_box_pack_start (GTK_BOX (list_view_hbox2), btn_vbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock (GTK_STOCK_ADD);
	gtk_widget_show (reg_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect (G_OBJECT (reg_btn), "clicked",
			    G_CALLBACK
			    (prefs_display_header_register_cb),
			    GINT_TO_POINTER(TRUE));
	del_btn = gtk_button_new_from_stock (GTK_STOCK_DELETE);
	gtk_widget_show (del_btn);
	gtk_box_pack_start (GTK_BOX (btn_vbox), del_btn, FALSE, TRUE, 0);
	g_signal_connect (G_OBJECT	 (del_btn), "clicked",
			    G_CALLBACK (prefs_display_header_delete_cb),
			    hidden_headers_list_view);

	

	PACK_CHECK_BUTTON (vbox, checkbtn_other_headers,
			   _("Show all unspecified headers"));
	SET_TOGGLE_SENSITIVITY (checkbtn_other_headers, list_view_hbox2);

	gtk_widget_show_all(window);

	dispheader.window        = window;
	dispheader.ok_btn        = ok_btn;
	dispheader.cancel_btn    = cancel_btn;

	dispheader.hdr_combo     = hdr_combo;

	dispheader.headers_list_view        = headers_list_view;
	dispheader.hidden_headers_list_view = hidden_headers_list_view;

	dispheader.other_headers = checkbtn_other_headers;
}

void prefs_display_header_read_config(void)
{
	gchar *rcpath;
	FILE *fp;
	gchar buf[PREFSBUFSIZE];
	DisplayHeaderProp *dp;

	debug_print("Reading configuration for displaying headers...\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			     DISPLAY_HEADER_RC, NULL);
	if ((fp = g_fopen(rcpath, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(rcpath, "fopen");
		g_free(rcpath);
		prefs_common.disphdr_list = NULL;
		prefs_display_header_set_default();
		return;
	}
	g_free(rcpath);

	/* remove all previous headers list */
	while (prefs_common.disphdr_list != NULL) {
		dp = (DisplayHeaderProp *)prefs_common.disphdr_list->data;
		display_header_prop_free(dp);
		prefs_common.disphdr_list =
			g_slist_remove(prefs_common.disphdr_list, dp);
	}

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		g_strdelimit(buf, "\r\n", '\0');
		dp = display_header_prop_read_str(buf);
		if (dp)
			prefs_common.disphdr_list =
				g_slist_append(prefs_common.disphdr_list, dp);
	}

	fclose(fp);
}

static void prefs_display_header_write_config(void)
{
	gchar *rcpath;
	PrefFile *pfile;
	GSList *cur;

	debug_print("Writing configuration for displaying headers...\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			     DISPLAY_HEADER_RC, NULL);

	if ((pfile = prefs_write_open(rcpath)) == NULL) {
		g_warning("failed to write configuration to file\n");
		g_free(rcpath);
		return;
	}

	for (cur = prefs_common.disphdr_list; cur != NULL;
	     cur = cur->next) {
		DisplayHeaderProp *dp = (DisplayHeaderProp *)cur->data;
		gchar *dpstr;

		dpstr = display_header_prop_get_str(dp);
		if (fputs(dpstr, pfile->fp) == EOF ||
		    fputc('\n', pfile->fp) == EOF) {
			FILE_OP_ERROR(rcpath, "fputs || fputc");
			prefs_file_close_revert(pfile);
			g_free(rcpath);
			g_free(dpstr);
			return;
		}
		g_free(dpstr);
	}

	g_free(rcpath);

	if (prefs_file_close(pfile) < 0) {
		g_warning("failed to write configuration to file\n");
		return;
	}
}

static void prefs_display_header_set_dialog(void)
{
	GtkTreeView *list_view = GTK_TREE_VIEW(dispheader.headers_list_view);
	GtkTreeView *hidden_list_view = GTK_TREE_VIEW(dispheader.hidden_headers_list_view);
	GSList *cur;
	GtkTreeModel *model_list, *model_hidden;

	model_list = gtk_tree_view_get_model(list_view);
	model_hidden = gtk_tree_view_get_model(hidden_list_view);

	gtk_list_store_clear(GTK_LIST_STORE(model_list));
	gtk_list_store_clear(GTK_LIST_STORE(model_hidden));

	for (cur = prefs_common.disphdr_list; cur != NULL;
	     cur = cur->next) {
		DisplayHeaderProp *dp = (DisplayHeaderProp *)cur->data;

		if (dp->hidden)
			prefs_display_header_insert_header(GTK_LIST_STORE
						(model_hidden), dp->name, dp);	
		else
			prefs_display_header_insert_header(GTK_LIST_STORE
						(model_list), dp->name, dp);
	}

	gtk_toggle_button_set_active
		(GTK_TOGGLE_BUTTON(dispheader.other_headers),
		 prefs_common.show_other_header);
}

static void prefs_display_header_set_list(void)
{
	gint row = 0;
	DisplayHeaderProp *dp;
	GtkTreeModel *model;
	GtkTreeIter iter;

	g_slist_free(prefs_common.disphdr_list);
	prefs_common.disphdr_list = NULL;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(dispheader.headers_list_view));
	while (gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {
		gtk_tree_model_get(model, &iter, PREFS_HDR_DATA, &dp, -1);
		if (dp)
			prefs_common.disphdr_list =
				g_slist_append(prefs_common.disphdr_list, dp);
		row++;				
	}

	model = gtk_tree_view_get_model
			(GTK_TREE_VIEW(dispheader.hidden_headers_list_view));
	row = 0;
	while (gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {
		gtk_tree_model_get(model, &iter, PREFS_HDR_DATA, &dp, -1);
		if (dp) 
			prefs_common.disphdr_list =
				g_slist_append(prefs_common.disphdr_list, dp);
		row++;
	}
}

static gint prefs_display_header_find_header(GtkTreeView *list_view,
					     const gchar *header)
{
	gint row = 0;
	DisplayHeaderProp *dp;
	GtkTreeModel *model;
	GtkTreeIter iter;

	model = gtk_tree_view_get_model(list_view);
	while (gtk_tree_model_iter_nth_child(model, &iter, NULL, row)) {
		gtk_tree_model_get(model, &iter, PREFS_HDR_DATA, &dp, -1);
		if (dp && g_ascii_strcasecmp(dp->name, header) == 0)
			return row;
		row++;
	}

	return -1;
}

static void prefs_display_header_list_view_set_row(gboolean hidden)
{
	GtkTreeView *list_view;
	DisplayHeaderProp *dp;
	gchar *entry_text;
	GtkTreeModel *model;

	entry_text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(dispheader.hdr_combo));
	if (!entry_text)
		entry_text = gtk_editable_get_chars(
				GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(dispheader.hdr_combo))),0,-1);
	if (!entry_text || entry_text[0] == '\0') {
		alertpanel_error(_("Header name is not set."));
		return;
	}

	if (hidden)
		list_view = GTK_TREE_VIEW(dispheader.hidden_headers_list_view);
	else
		list_view = GTK_TREE_VIEW(dispheader.headers_list_view);

	if (prefs_display_header_find_header(list_view, entry_text) != -1) {
		alertpanel_error(_("This header is already in the list."));
		return;
	}

	dp = g_new0(DisplayHeaderProp, 1);

	dp->name = g_strdup(entry_text);
	dp->hidden = hidden;

	model = gtk_tree_view_get_model(list_view);
	prefs_display_header_insert_header(GTK_LIST_STORE(model),
					   dp->name, dp);

	prefs_display_header_set_list();
	
	g_free(entry_text);
}

static void prefs_display_header_register_cb(GtkButton *btn,
					     gpointer hidden_data)
{
	prefs_display_header_list_view_set_row(GPOINTER_TO_INT(hidden_data));
}

static void prefs_display_header_delete_cb(GtkButton *btn, gpointer list_view_data)
{
	GtkTreeView *list_view = GTK_TREE_VIEW(list_view_data);
	DisplayHeaderProp *dp;
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(list_view));
	GtkTreeSelection *selection = gtk_tree_view_get_selection(list_view);
	GtkTreeIter iter;

	if (!gtk_tree_selection_get_selected(selection, NULL, &iter))
		return;

	gtk_tree_model_get(GTK_TREE_MODEL(store), &iter, PREFS_HDR_DATA, &dp, -1);
	if (!dp) 
		return;

	prefs_common.disphdr_list =
		g_slist_remove(prefs_common.disphdr_list, dp);
	display_header_prop_free(dp);
	gtk_list_store_remove(store, &iter);
}

static void prefs_display_header_up(void)
{
	GtkTreePath *prev, *sel, *try;
	GtkTreeIter isel;
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(dispheader.headers_list_view)),
		 &model,	
		 &isel))
		return;
	store = (GtkListStore *)model;
	sel = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &isel);
	if (!sel)
		return;
	
	/* no move if we're at row 0... */
	try = gtk_tree_path_copy(sel);
	if (!gtk_tree_path_prev(try)) {
		gtk_tree_path_free(try);
		gtk_tree_path_free(sel);
		return;
	}

	prev = try;
	gtk_tree_model_get_iter(GTK_TREE_MODEL(store),
				&iprev, prev);
	gtk_list_store_swap(store, &iprev, &isel);

	gtk_tree_path_free(sel);
	gtk_tree_path_free(prev);
}

static void prefs_display_header_down(void)
{
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter next, sel;
	GtkTreePath *try;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(dispheader.headers_list_view)),
		 &model,
		 &sel))
		return;
	store = (GtkListStore *)model;
	try = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &sel);
	if (!try) 
		return;
	
	next = sel;
	if (gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &next))
		gtk_list_store_swap(store, &next, &sel);
		
	gtk_tree_path_free(try);
}

static gboolean prefs_display_header_key_pressed(GtkWidget *widget,
					     GdkEventKey *event,
					     gpointer data)
{
	if (event && event->keyval == GDK_Escape)
		prefs_display_header_cancel();
	return FALSE;
}

static void prefs_display_header_ok(void)
{
	prefs_common.show_other_header =
		gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON(dispheader.other_headers));
	prefs_display_header_write_config();
	gtk_widget_hide(dispheader.window);
	gtk_window_set_modal(GTK_WINDOW(dispheader.window), FALSE);
}

static void prefs_display_header_cancel(void)
{
	prefs_display_header_read_config();
	gtk_widget_hide(dispheader.window);
	gtk_window_set_modal(GTK_WINDOW(dispheader.window), FALSE);
}

static gint prefs_display_header_deleted(GtkWidget *widget, GdkEventAny *event,
					 gpointer data)
{
	prefs_display_header_cancel();
	return TRUE;
}

static GtkListStore *prefs_display_header_create_store(void)
{
	return gtk_list_store_new(N_PREFS_HDR_COLUMNS,
				  G_TYPE_STRING,
				  G_TYPE_POINTER,
				  -1);
}

static void prefs_display_header_insert_header(GtkListStore *store,
					       gchar *name,
					       DisplayHeaderProp *dp)
{
	GtkTreeIter iter;

	/* add new */
	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter,
			   PREFS_HDR_HEADER,
			   name,
			   PREFS_HDR_DATA, dp,
			   -1);
}

static GtkWidget *prefs_display_header_list_view_create(const gchar *name)
{
	GtkWidget *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(prefs_display_header_create_store());
	list_view = gtk_tree_view_new_with_model(model);
	g_object_unref(G_OBJECT(model));
	
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(list_view),
				     prefs_common.use_stripes_everywhere);
	
	selector = gtk_tree_view_get_selection(GTK_TREE_VIEW(list_view));
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	prefs_filtering_create_list_view_columns(GTK_WIDGET(list_view), name);

	return list_view;
}

static void prefs_filtering_create_list_view_columns(GtkWidget *list_view, 
						     const gchar *name)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(name, renderer, "text", PREFS_HDR_HEADER, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
}

/*!
 *\brief	Called as a result of a gtk_list_store_swap()
 */
static void headers_list_model_rows_reordered(GtkTreeModel *model,
					      GtkTreePath  *path, 
					      GtkTreeIter  *iter,
					      gpointer	    arg,
					      GtkTreeView  *list_view)
{
	prefs_display_header_set_list();
}

/*!
 *\brief	Called as a result of a drag & drop
 */
static void drag_end(GtkTreeView *list_view,
		    GdkDragContext *context,
		    gpointer data)
{
	prefs_display_header_set_list();
}

