/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto & The Claws Mail Team
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
#include "inc.h"
#include "utils.h"
#include "gtkutils.h"
#include "manage_window.h"
#include "mainwindow.h"
#include "prefs_common.h"
#include "alertpanel.h"
#include "prefs_actions.h"
#include "action.h"
#include "description_window.h"
#include "gtkutils.h"
#include "manual.h"
#include "menu.h"
#include "filtering.h"
#include "prefs_filtering_action.h"
#include "matcher_parser.h"

enum {
	PREFS_ACTIONS_STRING,	/*!< string pointer managed by list store, 
				 *   and never touched or retrieved by 
				 *   us */ 
	PREFS_ACTIONS_DATA,	/*!< pointer to string that is not managed by 
				 *   the list store, and which is retrieved
				 *   and touched by us */
	PREFS_ACTIONS_VALID,	/*!< contains a valid action, otherwise "(New)" */
	N_PREFS_ACTIONS_COLUMNS
};

static struct Actions
{
	GtkWidget *window;

	GtkWidget *ok_btn;
	GtkWidget *filter_btn;
	GtkWidget *name_entry;
	GtkWidget *cmd_entry;
	GtkWidget *info_btn;
	GtkWidget *shell_radiobtn;
	GtkWidget *filter_radiobtn;
	
	GtkWidget *actions_list_view;
} actions;

static int modified = FALSE;
static int modified_list = FALSE;

/* widget creating functions */
static void prefs_actions_create	(MainWindow *mainwin);
static void prefs_actions_set_dialog	(void);
static gint prefs_actions_clist_set_row	(gint row);

/* callback functions */
static void prefs_actions_info_cb	(GtkWidget	*w,
					 GtkWidget	*window);
static void prefs_actions_register_cb	(GtkWidget	*w,
					 gpointer	 data);
static void prefs_actions_substitute_cb	(GtkWidget	*w,
					 gpointer	 data);
static void prefs_actions_delete_cb	(gpointer gtk_action, gpointer data);
static void prefs_actions_delete_all_cb	(gpointer gtk_action, gpointer data);
static void prefs_actions_clear_cb	(gpointer gtk_action, gpointer data);
static void prefs_actions_duplicate_cb	(gpointer gtk_action, gpointer data);
static void prefs_actions_up		(GtkWidget	*w,
					 gpointer	 data);
static void prefs_actions_down		(GtkWidget	*w,
					 gpointer	 data);
static gint prefs_actions_deleted	(GtkWidget	*widget,
					 GdkEventAny	*event,
					 gpointer	*data);
static gboolean prefs_actions_key_pressed(GtkWidget	*widget,
					  GdkEventKey	*event,
					  gpointer	 data);
static gboolean prefs_actions_search_func_cb (GtkTreeModel *model, gint column, 
						const gchar *key, GtkTreeIter *iter, 
						gpointer search_data);
static void prefs_actions_cancel	(GtkWidget	*w,
					 gpointer	 data);
static void prefs_actions_ok		(GtkWidget	*w,
					 gpointer	 data);

static GtkListStore* prefs_actions_create_data_store	(void);

static void prefs_actions_list_view_insert_action	(GtkWidget *list_view,
							 gint row,
							 gchar *action,
							 gboolean is_valid);
static GtkWidget *prefs_actions_list_view_create	(void);
static void prefs_actions_create_list_view_columns	(GtkWidget *list_view);
static void prefs_actions_select_row(GtkTreeView *list_view, GtkTreePath *path);

static void prefs_action_filter_radiobtn_cb(GtkWidget *widget, gpointer data);
static void prefs_action_shell_radiobtn_cb(GtkWidget *widget, gpointer data);
static void prefs_action_filterbtn_cb(GtkWidget *widget, gpointer data);
static void prefs_action_define_filter_done(GSList * action_list);


void prefs_actions_open(MainWindow *mainwin)
{
	inc_lock();

	if (!actions.window)
		prefs_actions_create(mainwin);

	manage_window_set_transient(GTK_WINDOW(actions.window));
	gtk_widget_grab_focus(actions.ok_btn);

	prefs_actions_set_dialog();

	gtk_widget_show(actions.window);
	gtk_window_set_modal(GTK_WINDOW(actions.window), TRUE);
}

/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void prefs_actions_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.actionswin_width = allocation->width;
	prefs_common.actionswin_height = allocation->height;
}

static void prefs_actions_create(MainWindow *mainwin)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *filter_hbox;
	GtkWidget *help_btn;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *confirm_area;

	GtkWidget *vbox1;
	GtkWidget *table;

	GtkWidget *shell_radiobtn;
	GtkWidget *filter_radiobtn;

	GtkWidget *name_label;
	GtkWidget *name_entry;
	GtkWidget *cmd_label;
	GtkWidget *cmd_entry;
	GtkWidget *filter_btn;

	GtkWidget *reg_hbox;
	GtkWidget *btn_hbox;
	GtkWidget *arrow;
	GtkWidget *reg_btn;
	GtkWidget *subst_btn;
	GtkWidget *del_btn;
	GtkWidget *clear_btn;

	GtkWidget *cond_hbox;
	GtkWidget *cond_scrolledwin;
	GtkWidget *cond_list_view;

	GtkWidget *info_btn;

	GtkWidget *btn_vbox;
	GtkWidget *up_btn;
	GtkWidget *down_btn;
	static GdkGeometry geometry;
	CLAWS_TIP_DECL();

	debug_print("Creating actions configuration window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_actions");

	gtk_container_set_border_width(GTK_CONTAINER (window), 8);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);

	vbox = gtk_vbox_new(FALSE, 6);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	gtkut_stock_button_set_create_with_help(&confirm_area, &help_btn,
			&cancel_btn, GTK_STOCK_CANCEL,
			&ok_btn, GTK_STOCK_OK,
			NULL, NULL);
	gtk_widget_show(confirm_area);
	gtk_box_pack_end(GTK_BOX(vbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	gtk_window_set_title(GTK_WINDOW(window), _("Actions configuration"));
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(prefs_actions_deleted), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(prefs_actions_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(prefs_actions_key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(prefs_actions_ok), mainwin);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(prefs_actions_cancel), NULL);
	g_signal_connect(G_OBJECT(help_btn), "clicked",
			 G_CALLBACK(manual_open_with_anchor_cb),
			 MANUAL_ANCHOR_ACTIONS);

	vbox1 = gtk_vbox_new(FALSE, VSPACING);
	gtk_widget_show(vbox1);
	gtk_box_pack_start(GTK_BOX(vbox), vbox1, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(vbox1), 2);	

	table = gtk_table_new(3, 2, FALSE);
	gtk_table_set_row_spacings (GTK_TABLE (table), VSPACING_NARROW_2);
	gtk_table_set_col_spacings (GTK_TABLE (table), 4);
	gtk_widget_show(table);
	gtk_box_pack_start (GTK_BOX (vbox1), table, FALSE, FALSE, 0);

	name_label = gtk_label_new (_("Menu name"));
	gtk_widget_show (name_label);
	gtk_misc_set_alignment (GTK_MISC (name_label), 1, 0.5);
  	gtk_table_attach (GTK_TABLE (table), name_label, 0, 1, 0, 1,
                    	  (GtkAttachOptions) (GTK_FILL),
                    	  (GtkAttachOptions) (0), 0, 0);

	name_entry = gtk_entry_new ();
	gtk_widget_show (name_entry);
  	gtk_table_attach (GTK_TABLE (table), name_entry, 1, 2, 0, 1,
                    	  (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
			  (GtkAttachOptions) (0), 0, 0);

	cmd_label = gtk_label_new (_("Command"));
	gtk_widget_show (cmd_label);
	gtk_misc_set_alignment (GTK_MISC (cmd_label), 1, 0.5);
  	gtk_table_attach (GTK_TABLE (table), cmd_label, 0, 1, 2, 3,
                    	  (GtkAttachOptions) (GTK_FILL),
                    	  (GtkAttachOptions) (0), 0, 0);

	cmd_entry = gtk_entry_new ();
	gtk_widget_show (cmd_entry);
  	gtk_table_attach (GTK_TABLE (table), cmd_entry, 1, 2, 2, 3,
                    	  (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
                    	  (GtkAttachOptions) (0), 0, 0);

	/* radio buttons for filter actions or shell */
	filter_hbox = gtk_hbox_new(FALSE,4);
	gtk_table_attach(GTK_TABLE(table), filter_hbox, 1, 2, 3, 4,
                    	  (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
			  (GtkAttachOptions) (0), 0, 0);
	gtk_widget_show(filter_hbox);

	shell_radiobtn = gtk_radio_button_new_with_label(NULL, _("Shell command"));
	gtk_box_pack_start(GTK_BOX(filter_hbox), shell_radiobtn, FALSE, FALSE, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(shell_radiobtn), TRUE);
	gtk_widget_show(shell_radiobtn);
	
	g_signal_connect(G_OBJECT(shell_radiobtn), "clicked",
			 G_CALLBACK(prefs_action_shell_radiobtn_cb), NULL);

	filter_radiobtn =
		gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(shell_radiobtn), 
							    _("Filter action"));
	gtk_box_pack_start(GTK_BOX(filter_hbox), filter_radiobtn, FALSE, FALSE, 0);
	gtk_widget_show(filter_radiobtn);
	g_signal_connect(G_OBJECT(filter_radiobtn), "clicked",
			 G_CALLBACK(prefs_action_filter_radiobtn_cb), NULL);

	filter_btn = gtk_button_new_with_label(_("Edit filter action"));
	gtk_box_pack_start(GTK_BOX(filter_hbox), filter_btn, FALSE, FALSE, 0);
	gtk_widget_set_sensitive(filter_btn, FALSE);
	g_signal_connect(G_OBJECT(filter_btn), "clicked",
			 G_CALLBACK(prefs_action_filterbtn_cb), NULL);
	gtk_widget_show(filter_btn);

	/* register / substitute / delete */

	reg_hbox = gtk_hbox_new(FALSE, 4);
	gtk_widget_show(reg_hbox);
	gtk_box_pack_start(GTK_BOX(vbox1), reg_hbox, FALSE, FALSE, 0);

	arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_widget_show(arrow);
	gtk_box_pack_start(GTK_BOX(reg_hbox), arrow, FALSE, FALSE, 0);
	gtk_widget_set_size_request(arrow, -1, 16);

	btn_hbox = gtk_hbox_new(TRUE, 4);
	gtk_widget_show(btn_hbox);
	gtk_box_pack_start(GTK_BOX(reg_hbox), btn_hbox, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_widget_show(reg_btn);
	gtk_box_pack_start(GTK_BOX(btn_hbox), reg_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(reg_btn), "clicked",
			 G_CALLBACK(prefs_actions_register_cb), NULL);
	CLAWS_SET_TIP(reg_btn,
			_("Append the new action above to the list"));

	subst_btn = gtkut_get_replace_btn(_("Replace"));
	gtk_widget_show(subst_btn);
	gtk_box_pack_start(GTK_BOX(btn_hbox), subst_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(subst_btn), "clicked",
			 G_CALLBACK(prefs_actions_substitute_cb), NULL);
	CLAWS_SET_TIP(subst_btn,
			_("Replace the selected action in list with the action above"));

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_widget_show(del_btn);
	gtk_box_pack_start(GTK_BOX(btn_hbox), del_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(prefs_actions_delete_cb), NULL);
	CLAWS_SET_TIP(del_btn,
			_("Delete the selected action from the list"));

	clear_btn = gtk_button_new_from_stock (GTK_STOCK_CLEAR);
	gtk_widget_show (clear_btn);
	gtk_box_pack_start (GTK_BOX (btn_hbox), clear_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT (clear_btn), "clicked",
			G_CALLBACK(prefs_actions_clear_cb), NULL);
	CLAWS_SET_TIP(clear_btn,
			_("Clear all the input fields in the dialog"));

	info_btn = gtk_button_new_from_stock(GTK_STOCK_INFO);
	gtk_widget_show(info_btn);
	gtk_box_pack_end(GTK_BOX(reg_hbox), info_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(info_btn), "clicked",
			 G_CALLBACK(prefs_actions_info_cb), GTK_WINDOW(window));
	CLAWS_SET_TIP(info_btn,
			_("Show information on configuring actions"));

	cond_hbox = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(cond_hbox);
	gtk_box_pack_start(GTK_BOX(vbox1), cond_hbox, TRUE, TRUE, 0);

	cond_scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(cond_scrolledwin);
	gtk_widget_set_size_request(cond_scrolledwin, -1, 150);
	gtk_box_pack_start(GTK_BOX(cond_hbox), cond_scrolledwin,
			   TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (cond_scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(cond_scrolledwin),
					    GTK_SHADOW_ETCHED_IN);

	cond_list_view = prefs_actions_list_view_create();				       
	gtk_widget_show(cond_list_view);
	gtk_container_add(GTK_CONTAINER (cond_scrolledwin), cond_list_view);

	btn_vbox = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(btn_vbox);
	gtk_box_pack_start(GTK_BOX(cond_hbox), btn_vbox, FALSE, FALSE, 0);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_widget_show(up_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), up_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(up_btn), "clicked",
			 G_CALLBACK(prefs_actions_up), NULL);
	CLAWS_SET_TIP(up_btn,
			_("Move the selected action up"));

	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_widget_show(down_btn);
	gtk_box_pack_start(GTK_BOX(btn_vbox), down_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(down_btn), "clicked",
			 G_CALLBACK(prefs_actions_down), NULL);
	CLAWS_SET_TIP(down_btn,
			_("Move selected action down"));

	if (!geometry.min_height) {
		geometry.min_width = 486;
		geometry.min_height = 322;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.actionswin_width,
				    prefs_common.actionswin_height);

	gtk_widget_show(window);

	actions.window = window;
	actions.ok_btn = ok_btn;
	actions.info_btn = info_btn;

	actions.name_entry = name_entry;
	actions.cmd_entry  = cmd_entry;
	actions.filter_btn = filter_btn;
	actions.shell_radiobtn = shell_radiobtn;
	actions.filter_radiobtn = filter_radiobtn;
	
	actions.actions_list_view = cond_list_view;
}

static void prefs_actions_reset_dialog(void)
{
	gtk_entry_set_text(GTK_ENTRY(actions.name_entry), "");
	gtk_entry_set_text(GTK_ENTRY(actions.cmd_entry), "");
}

void prefs_actions_read_config(void)
{
	gchar *rcpath;
	FILE *fp;
	gchar buf[PREFSBUFSIZE];
	gchar *act;

	debug_print("Reading actions configurations...\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, ACTIONS_RC, NULL);
	if ((fp = g_fopen(rcpath, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(rcpath, "fopen");
		g_free(rcpath);
		return;
	}
	g_free(rcpath);

	while (prefs_common.actions_list != NULL) {
		act = (gchar *)prefs_common.actions_list->data;
		prefs_common.actions_list =
			g_slist_remove(prefs_common.actions_list, act);
		g_free(act);
	}

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		const gchar *src_codeset = conv_get_locale_charset_str();
		const gchar *dest_codeset = CS_UTF_8;
		gchar *tmp;

		tmp = conv_codeset_strdup(buf, src_codeset, dest_codeset);
		if (!tmp) {
			g_warning("Failed to convert character set of action configuration\n");
			tmp = g_strdup(buf);
		}

		g_strchomp(tmp);
		act = strstr(tmp, ": ");
		if (act && act[2] && 
		    action_get_type(&act[2]) != ACTION_ERROR)
			prefs_common.actions_list =
				g_slist_append(prefs_common.actions_list,
					       tmp);
		else
			g_free(tmp);
	}
	fclose(fp);
}

void prefs_actions_write_config(void)
{
	gchar *rcpath;
	PrefFile *pfile;
	GSList *cur;

	debug_print("Writing actions configuration...\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, ACTIONS_RC, NULL);
	if ((pfile= prefs_write_open(rcpath)) == NULL) {
		g_warning("Failed to write configuration to file\n");
		g_free(rcpath);
		return;
	}

	for (cur = prefs_common.actions_list; cur != NULL; cur = cur->next) {
		gchar *tmp = (gchar *)cur->data;
		const gchar *src_codeset = CS_UTF_8;
		const gchar *dest_codeset = conv_get_locale_charset_str();
		gchar *act;

		act = conv_codeset_strdup(tmp, src_codeset, dest_codeset);
		if (!act) {
			g_warning("Failed to convert character set of action configuration\n");
			act = g_strdup(act);
		}

		if (fputs(act, pfile->fp) == EOF ||
		    fputc('\n', pfile->fp) == EOF) {
			FILE_OP_ERROR(rcpath, "fputs || fputc");
			prefs_file_close_revert(pfile);
			g_free(act);
			g_free(rcpath);
			return;
		}
		g_free(act);
	}
	
	g_free(rcpath);

	if (prefs_file_close(pfile) < 0) {
		g_warning("failed to write configuration to file\n");
		return;
	}
}

static void prefs_actions_clear_list(GtkListStore *list_store)
{
	gtk_list_store_clear(list_store);

	prefs_actions_list_view_insert_action(actions.actions_list_view,
					      -1, _("(New)"), FALSE);
}

static void prefs_actions_set_dialog(void)
{
	GtkListStore *store;
	GSList *cur;

	store = GTK_LIST_STORE(gtk_tree_view_get_model
				(GTK_TREE_VIEW(actions.actions_list_view)));

	prefs_actions_clear_list(store);	

	for (cur = prefs_common.actions_list; cur != NULL; cur = cur->next) {
		gchar *action = (gchar *) cur->data;
		
		prefs_actions_list_view_insert_action(actions.actions_list_view,
						      -1, action, TRUE);
	}
}

static void prefs_actions_set_list(void)
{
	GtkTreeIter iter;
	GtkListStore *store;
	
	g_slist_free(prefs_common.actions_list);
	prefs_common.actions_list = NULL;

	store = GTK_LIST_STORE(gtk_tree_view_get_model
				(GTK_TREE_VIEW(actions.actions_list_view)));

	if (gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store), &iter)) {
		do {
			gchar *action;
			gboolean is_valid;

			gtk_tree_model_get(GTK_TREE_MODEL(store), &iter,
					   PREFS_ACTIONS_DATA, &action,
					   PREFS_ACTIONS_VALID, &is_valid,
					   -1);
			
			if (is_valid) 
				prefs_common.actions_list = 
					g_slist_append(prefs_common.actions_list,
						       action);

		} while (gtk_tree_model_iter_next(GTK_TREE_MODEL(store),
						  &iter));
	}
}

#define GET_ENTRY(entry) \
	entry_text = gtk_entry_get_text(GTK_ENTRY(entry))

static gint prefs_actions_clist_set_row(gint row)
{
	const gchar *entry_text;
	gint len;
	gchar action[PREFSBUFSIZE];
	gchar *new_action;
	GtkListStore *store;

	store = GTK_LIST_STORE(gtk_tree_view_get_model
				(GTK_TREE_VIEW(actions.actions_list_view)));

	GET_ENTRY(actions.name_entry);
	if (entry_text[0] == '\0') {
		alertpanel_error(_("Menu name is not set."));
		return -1;
	}

	if (entry_text[0] == '/') {
		alertpanel_error(_("A leading '/' is not allowed in the menu name."));
		return -1;
	}

	if (strchr(entry_text, ':')) {
		alertpanel_error(_("Colon ':' is not allowed in the menu name."));
		return -1;
	}

	strncpy(action, entry_text, PREFSBUFSIZE - 1);

	while (strstr(action, "//")) {
		char *to_move = strstr(action, "//")+1;
		char *where = strstr(action, "//");
		int old_len = strlen(action);
		memmove(where, to_move, strlen(to_move));
		action[old_len-1] = '\0';
	}
	
	g_strstrip(action);

	/* Keep space for the ': ' delimiter */
	len = strlen(action) + 2;
	if (len >= PREFSBUFSIZE - 1) {
		alertpanel_error(_("Menu name is too long."));
		return -1;
	}

	strcat(action, ": ");

	GET_ENTRY(actions.cmd_entry);

	if (entry_text[0] == '\0') {
		alertpanel_error(_("Command-line not set."));
		return -1;
	}

	if (len + strlen(entry_text) >= PREFSBUFSIZE - 1) {
		alertpanel_error(_("Menu name and command are too long."));
		return -1;
	}

	if (action_get_type(entry_text) == ACTION_ERROR) {
		gchar *message;
		message = g_markup_printf_escaped(_("The command\n%s\nhas a syntax error."),
						entry_text);
		alertpanel_error("%s", message);
		g_free(message);
		return -1;
	}

	strcat(action, entry_text);

	new_action = g_strdup(action);	
	prefs_actions_list_view_insert_action(actions.actions_list_view,
	                                      row, new_action, TRUE);
						
	prefs_actions_set_list();

	return 0;
}

/* callback functions */

static void prefs_actions_register_cb(GtkWidget *w, gpointer data)
{
	prefs_actions_clist_set_row(-1);

	modified = FALSE;
	modified_list = TRUE;
}

static void prefs_actions_substitute_cb(GtkWidget *w, gpointer data)
{
	gint row;

	row = gtkut_list_view_get_selected_row(actions.actions_list_view);
	if (row <= 0)
		return;

	prefs_actions_clist_set_row(row);

	modified = FALSE;
	modified_list = TRUE;
}

static void prefs_actions_delete_cb(gpointer gtk_action, gpointer data)
{
	GtkTreeIter sel;
	GtkTreeModel *model;
	gchar *action;
	gint row;

	row = gtkut_list_view_get_selected_row(actions.actions_list_view);
	if (row <= 0) 
		return;	

	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection
				(GTK_TREE_VIEW(actions.actions_list_view)),
				&model, &sel))
		return;				

	if (alertpanel(_("Delete action"),
		       _("Do you really want to delete this action?"),
		       GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL) != G_ALERTALTERNATE)
		return;

	/* XXX: Here's the reason why we need to store the original 
	 * pointer: we search the slist for it. */
	gtk_tree_model_get(model, &sel,
			   PREFS_ACTIONS_DATA, &action,
			   -1);
	gtk_list_store_remove(GTK_LIST_STORE(model), &sel);

	prefs_common.actions_list = g_slist_remove(prefs_common.actions_list,
						   action);
	modified_list = TRUE;
}

static void prefs_actions_delete_all_cb(gpointer gtk_action, gpointer data)
{
	GtkListStore *list_store;

	if (alertpanel(_("Delete all actions"),
			  _("Do you really want to delete all the actions?"),
			  GTK_STOCK_CANCEL, GTK_STOCK_DELETE, NULL) != G_ALERTDEFAULT)
	   return;

	list_store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(actions.actions_list_view)));
	prefs_actions_clear_list(list_store);
	modified = FALSE;

	prefs_actions_reset_dialog();
	modified_list = TRUE;
}

static void prefs_actions_clear_cb(gpointer gtk_action, gpointer data)
{
	gint row;

	prefs_actions_reset_dialog();
	row = gtkut_list_view_get_selected_row(actions.actions_list_view);
	if (row < 1)
		modified = FALSE;
	else
		modified = TRUE;
}

static void prefs_actions_duplicate_cb(gpointer gtk_action, gpointer data)
{
	gint row;
	
	row = gtkut_list_view_get_selected_row(actions.actions_list_view);
	if (row <= 0)
		return;

	modified_list = !prefs_actions_clist_set_row(-row-2);
}

static void prefs_actions_up(GtkWidget *w, gpointer data)
{
	GtkTreePath *prev, *sel, *try;
	GtkTreeIter isel;
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter iprev;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(actions.actions_list_view)),
		 &model,	
		 &isel))
		return;
	store = (GtkListStore *)model;
	sel = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &isel);
	if (!sel)
		return;
	
	/* no move if we're at row 0 or 1, looks phony, but other
	 * solutions are more convoluted... */
	try = gtk_tree_path_copy(sel);
	if (!gtk_tree_path_prev(try) || !gtk_tree_path_prev(try)) {
		gtk_tree_path_free(try);
		gtk_tree_path_free(sel);
		return;
	}
	gtk_tree_path_free(try);

	prev = gtk_tree_path_copy(sel);		
	if (!gtk_tree_path_prev(prev)) {
		gtk_tree_path_free(prev);
		gtk_tree_path_free(sel);
		return;
	}

	gtk_tree_model_get_iter(GTK_TREE_MODEL(store),
				&iprev, prev);
	gtk_tree_path_free(sel);
	gtk_tree_path_free(prev);

	gtk_list_store_swap(store, &iprev, &isel);
	prefs_actions_set_list();
	modified_list = TRUE;
}

static void prefs_actions_down(GtkWidget *w, gpointer data)
{
	GtkListStore *store = NULL;
	GtkTreeModel *model = NULL;
	GtkTreeIter next, sel;
	GtkTreePath *try;
	
	if (!gtk_tree_selection_get_selected
		(gtk_tree_view_get_selection
			(GTK_TREE_VIEW(actions.actions_list_view)),
		 &model,
		 &sel))
		return;
	store = (GtkListStore *)model;
	try = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &sel);
	if (!try) 
		return;

	/* no move when we're at row 0 */
	if (!gtk_tree_path_prev(try)) {
		gtk_tree_path_free(try);
		return;
	}
	gtk_tree_path_free(try);

	next = sel;
	if (!gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &next)) 
		return;

	gtk_list_store_swap(store, &next, &sel);
	prefs_actions_set_list();
	modified_list = TRUE;
}

static gint prefs_actions_deleted(GtkWidget *widget, GdkEventAny *event,
				  gpointer *data)
{
	prefs_actions_cancel(widget, data);
	return TRUE;
}

static gboolean prefs_actions_key_pressed(GtkWidget *widget, GdkEventKey *event,
					  gpointer data)
{
	if (event && event->keyval == GDK_Escape)
		prefs_actions_cancel(widget, data);
	else {
		GtkWidget *focused = gtkut_get_focused_child(
				GTK_CONTAINER(widget));
		if (focused && GTK_IS_EDITABLE(focused))
			modified = TRUE;
	}
	return FALSE;
}

static gboolean prefs_actions_search_func_cb (GtkTreeModel *model, gint column, const gchar *key, 
						GtkTreeIter *iter, gpointer search_data) 
{
	gchar *store_string;
	gboolean retval;
	GtkTreePath *path;

	gtk_tree_model_get (model, iter, column, &store_string, -1);

	if (!store_string || !key)
		return FALSE;


	retval = (strncmp (key, store_string, strlen(key)) != 0);

	g_free(store_string);
	debug_print("selecting row\n");
	path = gtk_tree_model_get_path(model, iter);
	prefs_actions_select_row(GTK_TREE_VIEW(actions.actions_list_view), path);
	gtk_tree_path_free(path);

	return retval;
}
static void prefs_actions_cancel(GtkWidget *w, gpointer data)
{
	GtkListStore *store;

	if (modified && alertpanel(_("Entry not saved"),
				 _("The entry was not saved. Close anyway?"),
				 GTK_STOCK_CLOSE, _("+_Continue editing"),
				 NULL) != G_ALERTDEFAULT) {
		return;
	} else if (modified_list && alertpanel(_("Actions list not saved"),
				 _("The actions list has been modified. Close anyway?"),
				 GTK_STOCK_CLOSE, _("+_Continue editing"), 
				 NULL) != G_ALERTDEFAULT) {
		return;
	}
	modified = FALSE;
	modified_list = FALSE;
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW
				(actions.actions_list_view)));
	gtk_list_store_clear(store);
	prefs_actions_read_config();
	gtk_widget_hide(actions.window);
	gtk_window_set_modal(GTK_WINDOW(actions.window), FALSE);
	inc_unlock();
}

static void prefs_actions_ok(GtkWidget *widget, gpointer data)
{
	MainWindow *mainwin = (MainWindow *) data;
	GList *list;
	GList *iter;
	MessageView *msgview;
	Compose *compose;
	GtkListStore *store;

	if (modified && alertpanel(_("Entry not saved"),
				 _("The entry was not saved. Close anyway?"),
				 GTK_STOCK_CLOSE, _("+_Continue editing"),
				 NULL) != G_ALERTDEFAULT) {
		return;
	} 
	modified = FALSE;
	modified_list = FALSE;
	prefs_actions_set_list();
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW
				(actions.actions_list_view)));
	gtk_list_store_clear(store);
	prefs_actions_write_config();

	/* Update mainwindow actions menu */
	main_window_update_actions_menu(mainwin);

	/* Update separated message view actions menu */
	list = messageview_get_msgview_list();
	for (iter = list; iter; iter = iter->next) {
		msgview = (MessageView *) iter->data;
		messageview_update_actions_menu(msgview);
	}

	/* Update compose windows actions menu */
	list = compose_get_compose_list();
	for (iter = list; iter; iter = iter->next) {
		compose = (Compose *) iter->data;
		compose_update_actions_menu(compose);
	}

	gtk_widget_hide(actions.window);
	gtk_window_set_modal(GTK_WINDOW(actions.window), FALSE);
	inc_unlock();
}

/*
 * Strings describing action format strings
 * 
 * When adding new lines, remember to put one string for each line
 */
static gchar *actions_desc_strings[] = {
	N_("<span weight=\"bold\" underline=\"single\">Menu name:</span>"), NULL,
	N_("Use / in menu name to make submenus."), NULL,
	"", NULL,
	N_("<span weight=\"bold\" underline=\"single\">Command-line:</span>"), NULL,
	N_("<span weight=\"bold\">Begin with:</span>"), NULL,
	"     |",   N_("to send message body or selection to command's standard input"),
	"     &gt;",   N_("to send user provided text to command's standard input"),
	"     *",   N_("to send user provided hidden text to command's standard input"),
	N_("<span weight=\"bold\">End with:</span>"), NULL,
	"     |",   N_("to replace message body or selection with command's standard output"),
	"     &gt;",   N_("to insert command's standard output without replacing old text"),
	"     &amp;",   N_("to run command asynchronously"),
	N_("<span weight=\"bold\">Use:</span>"), NULL, 
	"     %f",  N_("for the file of the selected message in RFC822/2822 format "),
	"     %F",  N_("for the list of the files of the selected messages in RFC822/2822 format"),
	"     %p",  N_("for the file of the selected decoded message MIME part"),
	"     %u",  N_("for a user provided argument"),
	"     %h",  N_("for a user provided hidden argument (e.g. password)"),
	"     %s",  N_("for the text selection"),
	"  %as{}",  N_("apply filtering actions between {} to selected messages"),
	"     %%",  N_("for a literal %"),
	NULL, NULL
};


static DescriptionWindow actions_desc_win = { 
	NULL,
	NULL,
	2,
	N_("Actions"),
  	N_("The Actions feature is a way for the user to launch "
	   "external commands to process a complete message file or just "
	   "one of its parts."),
        actions_desc_strings
};


static void prefs_actions_info_cb(GtkWidget *w, GtkWidget *window)
{
	actions_desc_win.parent = window;
	description_window_create(&actions_desc_win);
}

static GtkListStore* prefs_actions_create_data_store(void)
{
	return gtk_list_store_new(N_PREFS_ACTIONS_COLUMNS,
				  G_TYPE_STRING,	
				  G_TYPE_POINTER,
				  G_TYPE_BOOLEAN,
				  -1);
}

static void prefs_actions_list_view_insert_action(GtkWidget *list_view,
						  gint row,
						  gchar *action,
						  gboolean is_valid) 
{
	GtkTreeIter iter;
	GtkTreeIter sibling;
	GtkListStore *list_store = GTK_LIST_STORE(gtk_tree_view_get_model
					(GTK_TREE_VIEW(list_view)));

/*	row -1 to add a new rule to store,
	row >=0 to change an existing row
	row <-1 insert a new row after (-row-2)
*/
	if (row >= 0 ) {
		/* modify the existing */
 		if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(list_store),
 						   &iter, NULL, row))
			row = -1;
	} else if (row < -1 ) {
		if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(list_store),
						   &sibling, NULL, -row-2))
 			row = -1;		
	}

	if (row == -1 ) {
		/* append new */
		gtk_list_store_append(list_store, &iter);
		gtk_list_store_set(list_store, &iter,
				   PREFS_ACTIONS_STRING, action,
				   PREFS_ACTIONS_DATA, action,
				   PREFS_ACTIONS_VALID,  is_valid,
				   -1);
	} else if (row < -1) {
		/* duplicate */
		gtk_list_store_insert_after(list_store, &iter, &sibling);
		gtk_list_store_set(list_store, &iter,
				   PREFS_ACTIONS_STRING, action,
				   PREFS_ACTIONS_DATA, action,
				   PREFS_ACTIONS_VALID,  is_valid,
				   -1);
	} else {
		/* change existing */
		gchar *old_action;

		gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
				   PREFS_ACTIONS_DATA, &old_action,
				   -1);
		g_free(old_action);				

		gtk_list_store_set(list_store, &iter,
				   PREFS_ACTIONS_STRING, action,
				   PREFS_ACTIONS_DATA, action,
				   -1);
	}
}

static GtkActionGroup *prefs_actions_popup_action = NULL;
static GtkWidget *prefs_actions_popup_menu = NULL;

static GtkActionEntry prefs_actions_popup_entries[] =
{
 	{"PrefsActionsPopup",			NULL, "PrefsActionsPopup" },
	{"PrefsActionsPopup/Delete",		NULL, N_("_Delete"), NULL, NULL, G_CALLBACK(prefs_actions_delete_cb) },
	{"PrefsActionsPopup/DeleteAll",	NULL, N_("Delete _all"), NULL, NULL, G_CALLBACK(prefs_actions_delete_all_cb) },
	{"PrefsActionsPopup/Duplicate",	NULL, N_("D_uplicate"), NULL, NULL, G_CALLBACK(prefs_actions_duplicate_cb) },
};

static gint prefs_actions_list_btn_pressed(GtkWidget *widget, GdkEventButton *event,
				   GtkTreeView *list_view)
{
	if (event) {
		/* left- or right-button click */
		if (event->button == 1 || event->button == 3) {
			GtkTreePath *path = NULL;
			if (gtk_tree_view_get_path_at_pos( list_view, event->x, event->y,
					&path, NULL, NULL, NULL)) {
				prefs_actions_select_row(list_view, path);
		}
		if (path)
			gtk_tree_path_free(path);
		}

		/* right-button click */
		if (event->button == 3) {
			GtkTreeModel *model = gtk_tree_view_get_model(list_view);
			GtkTreeIter iter;
			gboolean non_empty;
			gint row;

			if (!prefs_actions_popup_menu) {
				prefs_actions_popup_action = cm_menu_create_action_group("PrefsActionsPopup",
						prefs_actions_popup_entries, G_N_ELEMENTS(prefs_actions_popup_entries),
						(gpointer)list_view);
				MENUITEM_ADDUI("/Menus", "PrefsActionsPopup", "PrefsActionsPopup", GTK_UI_MANAGER_MENU)
				MENUITEM_ADDUI("/Menus/PrefsActionsPopup", "Delete", "PrefsActionsPopup/Delete", GTK_UI_MANAGER_MENUITEM)
				MENUITEM_ADDUI("/Menus/PrefsActionsPopup", "DeleteAll", "PrefsActionsPopup/DeleteAll", GTK_UI_MANAGER_MENUITEM)
				MENUITEM_ADDUI("/Menus/PrefsActionsPopup", "Duplicate", "PrefsActionsPopup/Duplicate", GTK_UI_MANAGER_MENUITEM)
				prefs_actions_popup_menu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
						gtk_ui_manager_get_widget(gtkut_ui_manager(), "/Menus/PrefsActionsPopup")) );
			}

			/* grey out some popup menu items if there is no selected row */
			row = gtkut_list_view_get_selected_row(GTK_WIDGET(list_view));
			cm_menu_set_sensitive("PrefsActionsPopup/Delete", (row > 0));
			cm_menu_set_sensitive("PrefsActionsPopup/Duplicate", (row > 0));

			/* grey out seom popup menu items if there is no row
			(not counting the (New) one at row 0) */
			non_empty = gtk_tree_model_get_iter_first(model, &iter);
			if (non_empty)
				non_empty = gtk_tree_model_iter_next(model, &iter);
			cm_menu_set_sensitive("PrefsActionsPopup/DeleteAll", non_empty);

			gtk_menu_popup(GTK_MENU(prefs_actions_popup_menu), 
					NULL, NULL, NULL, NULL, 
					event->button, event->time);
		}
   }
   return FALSE;
}

static gboolean prefs_actions_list_popup_menu(GtkWidget *widget, gpointer data)
{
   GtkTreeView *list_view = (GtkTreeView *)data;
   GdkEventButton event;
   
   event.button = 3;
   event.time = gtk_get_current_event_time();
   
   prefs_actions_list_btn_pressed(NULL, &event, list_view);

   return TRUE;
}

static GtkWidget *prefs_actions_list_view_create(void)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(prefs_actions_create_data_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	
	
#ifndef MAEMO
	g_signal_connect(G_OBJECT(list_view), "popup-menu",
			 G_CALLBACK(prefs_actions_list_popup_menu), list_view);
#else
	gtk_widget_tap_and_hold_setup(GTK_WIDGET(list_view), NULL, NULL,
			GTK_TAP_AND_HOLD_NONE | GTK_TAP_AND_HOLD_NO_INTERNALS);
	g_signal_connect(G_OBJECT(list_view), "tap-and-hold",
			 G_CALLBACK(prefs_actions_list_popup_menu), list_view);
#endif
	g_signal_connect(G_OBJECT(list_view), "button-press-event",
			G_CALLBACK(prefs_actions_list_btn_pressed), list_view);

	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_reorderable(list_view, TRUE);

	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);

	/* create the columns */
	prefs_actions_create_list_view_columns(GTK_WIDGET(list_view));

	return GTK_WIDGET(list_view);
}

static void prefs_actions_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		(_("Current actions"),
		 renderer,
		 "text", PREFS_ACTIONS_STRING,
		 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
	gtk_tree_view_set_search_equal_func(GTK_TREE_VIEW(list_view), prefs_actions_search_func_cb , NULL, NULL);
}

#define ENTRY_SET_TEXT(entry, str) \
	gtk_entry_set_text(GTK_ENTRY(entry), str ? str : "")

static void prefs_actions_select_row(GtkTreeView *list_view, GtkTreePath *path)
{
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	GtkTreeSelection *selection;
	gchar *action;
	gchar *cmd;
	gchar buf[PREFSBUFSIZE];
	GtkTreeIter iter;
	gboolean is_valid;

	if (!model || !path || !gtk_tree_model_get_iter(model, &iter, path))
		return;

	/* select row */
	selection = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_select_path(selection, path);

	gtk_tree_model_get(model, &iter, 
			   PREFS_ACTIONS_VALID,  &is_valid,
			   PREFS_ACTIONS_DATA, &action,
			   -1);
	if (!is_valid) {
		prefs_actions_reset_dialog();
		return;
	}
	
	strncpy(buf, action, PREFSBUFSIZE - 1);
	buf[PREFSBUFSIZE - 1] = '\0';
	cmd = strstr(buf, ": ");

	if (cmd && cmd[2])
		ENTRY_SET_TEXT(actions.cmd_entry, &cmd[2]);
	else
		return;

	*cmd = '\0';
	gtk_entry_set_text(GTK_ENTRY(actions.name_entry), buf);

	if (g_str_has_prefix(&cmd[2], "%as{") == TRUE)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
						actions.filter_radiobtn), TRUE);
	else
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
						actions.shell_radiobtn), TRUE);

	return;
}

static void prefs_action_filter_radiobtn_cb(GtkWidget *widget, gpointer data)
{
	if (actions.filter_btn)
		gtk_widget_set_sensitive(actions.filter_btn, TRUE);
	if (actions.cmd_entry)
		gtk_widget_set_sensitive(actions.cmd_entry, FALSE);
	if (actions.info_btn)
		gtk_widget_set_sensitive(actions.info_btn, FALSE);
}

static void prefs_action_shell_radiobtn_cb(GtkWidget *widget, gpointer data)
{
	if (actions.filter_btn)
		gtk_widget_set_sensitive(actions.filter_btn, FALSE);
	if (actions.cmd_entry)
		gtk_widget_set_sensitive(actions.cmd_entry, TRUE);
	if (actions.info_btn)
		gtk_widget_set_sensitive(actions.info_btn, TRUE);
}

static void prefs_action_filterbtn_cb(GtkWidget *widget, gpointer data)
{
	gchar *action_str, **tokens;
	GSList *action_list = NULL, *cur;

/* I think this warning is useless - it's logical to clear the field when
   changing its type.

	if(modified && alertpanel(_("Entry was modified"),
			_("Opening the filter action dialog will clear current modifications "
			"of the command line."),
			GTK_STOCK_CANCEL, _("+_Continue editing"), NULL) != G_ALERTDEFAULT)
		return;
*/
	action_str = gtk_editable_get_chars(GTK_EDITABLE(actions.cmd_entry), 0, -1);
	tokens = g_strsplit_set(action_str, "{}", 5);

	if (tokens[0] && tokens[1] && *tokens[1] != '\0') {
		action_list = matcher_parser_get_action_list(tokens[1]);
		if (action_list == NULL)
			alertpanel_error(_("Action string is not valid."));
	}
		
	prefs_filtering_action_open(action_list, prefs_action_define_filter_done);

	if (action_list != NULL) {
		for(cur = action_list ; cur != NULL ; cur = cur->next)
                        filteringaction_free(cur->data);
        }
        
	g_free(action_str);
	g_strfreev(tokens);
}

static void prefs_action_define_filter_done(GSList * action_list)
{
	gchar *str;

	if (action_list == NULL)
		return;

	str = filteringaction_list_to_string(action_list);

	if (str != NULL) {
		gchar *cmd;
		cmd = g_strdup_printf("%%as{%s}",str);
		g_free(str);
		gtk_entry_set_text(GTK_ENTRY(actions.cmd_entry), cmd);
		g_free(cmd);
		modified = TRUE;
	}
}
