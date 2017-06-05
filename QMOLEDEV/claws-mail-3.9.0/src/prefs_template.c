/*
 * Claws Mail templates subsystem 
 * Copyright (C) 2001 Alexander Barinov
 * Copyright (C) 2001-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>

#include "template.h"
#include "main.h"
#include "prefs_gtk.h"
#include "inc.h"
#include "utils.h"
#include "gtkutils.h"
#include "alertpanel.h"
#include "manage_window.h"
#include "compose.h"
#include "addr_compl.h"
#include "quote_fmt.h"
#include "prefs_common.h"
#include "manual.h"
#include "gtkutils.h"
#include "menu.h"
#include "account.h"

enum {
	TEMPL_TEXT,
	TEMPL_DATA,
	TEMPL_AUTO_DATA,	/*!< auto pointer */
	N_TEMPL_COLUMNS
};

static struct Templates {
	GtkWidget *window;
	GtkWidget *ok_btn;
	GtkWidget *list_view;
	GtkWidget *entry_name;
	GtkWidget *entry_subject;
	GtkWidget *entry_from;
	GtkWidget *entry_to;
	GtkWidget *entry_cc;	
	GtkWidget *entry_bcc;
	GtkWidget *text_value;
} templates;

static int modified = FALSE;
static int modified_list = FALSE;

static struct
{
	gchar *label;
	GtkWidget **entry;
	gboolean compl;
	gchar *tooltips;
} widgets_table[] = {
	{N_("Name"),	&templates.entry_name,		FALSE,
		N_("This name is used as the Menu item")},
	{"From",	&templates.entry_from,		TRUE,
		N_("Override composing account's From header. This doesn't change the composing account.")},
	{"To",		&templates.entry_to,		TRUE, 	NULL},
	{"Cc",		&templates.entry_cc,		TRUE, 	NULL},
	{"Bcc",		&templates.entry_bcc,		TRUE, 	NULL},
	{"Subject",	&templates.entry_subject,	FALSE,	NULL},
	{NULL,		NULL,				FALSE,	NULL}
};


/* widget creating functions */
static void prefs_template_window_create	(void);
static void prefs_template_window_setup		(void);

static GSList *prefs_template_get_list		(void);

/* callbacks */
static gint prefs_template_deleted_cb		(GtkWidget	*widget,
						 GdkEventAny	*event,
						 gpointer	 data);
static gboolean prefs_template_key_pressed_cb	(GtkWidget	*widget,
						 GdkEventKey	*event,
						 gpointer	 data);
static gboolean prefs_template_search_func_cb (GtkTreeModel *model, gint column, 
						const gchar *key, GtkTreeIter *iter, 
						gpointer search_data);

static void prefs_template_cancel_cb		(gpointer action, gpointer data);
static void prefs_template_ok_cb		(gpointer action, gpointer data);
static void prefs_template_register_cb		(gpointer action, gpointer data);
static void prefs_template_substitute_cb	(gpointer action, gpointer data);
static void prefs_template_delete_cb		(gpointer action, gpointer data);
static void prefs_template_delete_all_cb	(gpointer action, gpointer data);
static void prefs_template_clear_cb		(gpointer action, gpointer data);
static void prefs_template_duplicate_cb		(gpointer action, gpointer data);
static void prefs_template_top_cb		(gpointer action, gpointer data);
static void prefs_template_up_cb		(gpointer action, gpointer data);
static void prefs_template_down_cb		(gpointer action, gpointer data);
static void prefs_template_bottom_cb		(gpointer action, gpointer data);

static GtkListStore* prefs_template_create_data_store	(void);
static void prefs_template_list_view_insert_template	(GtkWidget *list_view,
							 gint row,
							 const gchar *template,
							 Template *data);
static GtkWidget *prefs_template_list_view_create	(void);
static void prefs_template_create_list_view_columns	(GtkWidget *list_view);
static void prefs_template_select_row(GtkTreeView *list_view, GtkTreePath *path);

/* Called from mainwindow.c */
void prefs_template_open(void)
{
	inc_lock();

	prefs_template_window_create();

	prefs_template_window_setup();
	gtk_widget_show(templates.window);
	gtk_window_set_modal(GTK_WINDOW(templates.window), TRUE);
}

/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void prefs_template_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.templateswin_width = allocation->width;
	prefs_common.templateswin_height = allocation->height;
}

static void prefs_template_window_create(void)
{
	/* window structure ;) */
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *scrolled_window;
	GtkWidget   *vpaned;
	GtkWidget     *vbox1;
	GtkWidget       *table; /* including : entry_[name|from|to|cc|bcc|subject] */
	GtkWidget       *scroll2;
	GtkWidget         *text_value;
	GtkWidget     *vbox2;
	GtkWidget       *hbox2;
	GtkWidget         *arrow1;
	GtkWidget         *hbox3;
	GtkWidget           *reg_btn;
	GtkWidget           *subst_btn;
	GtkWidget           *del_btn;
	GtkWidget           *clear_btn;
	GtkWidget         *desc_btn;
	GtkWidget       *hbox4;
	GtkWidget         *scroll1;
	GtkWidget           *list_view;
	GtkWidget         *vbox3;
	GtkWidget           *spc_vbox;
	GtkWidget           *top_btn;
	GtkWidget           *up_btn;
	GtkWidget           *down_btn;
	GtkWidget           *bottom_btn;
	GtkWidget       *confirm_area;
	GtkWidget         *help_btn;
	GtkWidget         *cancel_btn;
	GtkWidget         *ok_btn;
	static GdkGeometry geometry;
	gint i;

	debug_print("Creating templates configuration window...\n");

	/* main window */
	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "prefs_template");
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(window), vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 4);

	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show(scrolled_window);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(vbox), scrolled_window);

	/* vpaned to separate template settings from templates list */
	vpaned = gtk_vpaned_new();
	gtk_widget_show(vpaned);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      vpaned);
	gtk_viewport_set_shadow_type (GTK_VIEWPORT(
			gtk_bin_get_child(GTK_BIN(scrolled_window))), GTK_SHADOW_NONE);

	/* vbox to handle template name and content */
	vbox1 = gtk_vbox_new(FALSE, 6);
	gtk_widget_show(vbox1);
	gtk_container_set_border_width(GTK_CONTAINER(vbox1), 8);
	gtk_paned_pack1(GTK_PANED(vpaned), vbox1, FALSE, FALSE);

	table = gtk_table_new(5, 2, FALSE);
	gtk_table_set_row_spacings (GTK_TABLE (table), VSPACING_NARROW_2);
	gtk_table_set_col_spacings (GTK_TABLE (table), 4);
	gtk_widget_show(table);
	gtk_box_pack_start (GTK_BOX (vbox1), table, FALSE, FALSE, 0);


	for (i=0; widgets_table[i].label; i++) {

		GtkWidget *label;

		label = gtk_label_new( (i != 0) ?
			prefs_common_translated_header_name(widgets_table[i].label) :
			widgets_table[i].label);
		gtk_widget_show(label);
		gtk_table_attach(GTK_TABLE(table), label, 0, 1, i, (i + 1),
				(GtkAttachOptions) (GTK_FILL),
				(GtkAttachOptions) 0, 0, 0);
		gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

		*(widgets_table[i].entry) = gtk_entry_new();
		gtk_widget_show(*(widgets_table[i].entry));
		gtk_table_attach(GTK_TABLE(table), *(widgets_table[i].entry), 1, 2, i, (i + 1),
				(GtkAttachOptions) (GTK_EXPAND|GTK_SHRINK|GTK_FILL),
				(GtkAttachOptions) 0, 0, 0);
		CLAWS_SET_TIP(*(widgets_table[i].entry),
				widgets_table[i].tooltips);
	}

	/* template content */
	scroll2 = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scroll2);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll2),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll2),
					    GTK_SHADOW_IN);
	gtk_box_pack_start(GTK_BOX(vbox1), scroll2, TRUE, TRUE, 0);

	text_value = gtk_text_view_new();
	if (prefs_common.textfont) {
		PangoFontDescription *font_desc;

		font_desc = pango_font_description_from_string
						(prefs_common.textfont);
		if (font_desc) {
			gtk_widget_modify_font(text_value, font_desc);
			pango_font_description_free(font_desc);
		}
	}
	gtk_widget_show(text_value);
#ifndef GENERIC_UMPC
	gtk_widget_set_size_request(text_value, -1, 120);
#else
	gtk_widget_set_size_request(text_value, -1, 60);
#endif
	gtk_container_add(GTK_CONTAINER(scroll2), text_value);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text_value), TRUE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text_value), GTK_WRAP_WORD);

	/* vbox for buttons and templates list */
	vbox2 = gtk_vbox_new(FALSE, 6);
	gtk_widget_show(vbox2);
	gtk_container_set_border_width(GTK_CONTAINER(vbox2), 8);
	gtk_paned_pack2(GTK_PANED(vpaned), vbox2, TRUE, FALSE);

	/* register | substitute | delete */
	hbox2 = gtk_hbox_new(FALSE, 4);
	gtk_widget_show(hbox2);
	gtk_box_pack_start(GTK_BOX(vbox2), hbox2, FALSE, FALSE, 0);

	arrow1 = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_widget_show(arrow1);
	gtk_box_pack_start(GTK_BOX(hbox2), arrow1, FALSE, FALSE, 0);
	gtk_widget_set_size_request(arrow1, -1, 16);

	hbox3 = gtk_hbox_new(TRUE, 4);
	gtk_widget_show(hbox3);
	gtk_box_pack_start(GTK_BOX(hbox2), hbox3, FALSE, FALSE, 0);

	reg_btn = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_widget_show(reg_btn);
	gtk_box_pack_start(GTK_BOX(hbox3), reg_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT (reg_btn), "clicked",
			 G_CALLBACK (prefs_template_register_cb), NULL);
	CLAWS_SET_TIP(reg_btn,
			_("Append the new template above to the list"));

	subst_btn = gtkut_get_replace_btn(_("Replace"));
	gtk_widget_show(subst_btn);
	gtk_box_pack_start(GTK_BOX(hbox3), subst_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(subst_btn), "clicked",
			 G_CALLBACK(prefs_template_substitute_cb),
			 NULL);
	CLAWS_SET_TIP(subst_btn,
			_("Replace the selected template in list with the template above"));

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_widget_show(del_btn);
	gtk_box_pack_start(GTK_BOX(hbox3), del_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(prefs_template_delete_cb), NULL);
	CLAWS_SET_TIP(del_btn,
			_("Delete the selected template from the list"));

	clear_btn = gtk_button_new_from_stock (GTK_STOCK_CLEAR);
	gtk_widget_show (clear_btn);
	gtk_box_pack_start (GTK_BOX (hbox3), clear_btn, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT (clear_btn), "clicked",
			G_CALLBACK(prefs_template_clear_cb), NULL);
	CLAWS_SET_TIP(clear_btn,
			_("Clear all the input fields in the dialog"));

	desc_btn = gtk_button_new_from_stock(GTK_STOCK_INFO);
	gtk_widget_show(desc_btn);
	gtk_box_pack_end(GTK_BOX(hbox2), desc_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(desc_btn), "clicked",
			 G_CALLBACK(quote_fmt_quote_description), window);
	CLAWS_SET_TIP(desc_btn,
			_("Show information on configuring templates"));

	/* templates list */
	hbox4 = gtk_hbox_new(FALSE, 8);
	gtk_widget_show(hbox4);
	gtk_box_pack_start(GTK_BOX(vbox2), hbox4, TRUE, TRUE, 0);

	scroll1 = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scroll1);
	gtk_box_pack_start(GTK_BOX(hbox4), scroll1, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll1),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
				       
	vbox3 = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(vbox3);
	gtk_box_pack_start(GTK_BOX(hbox4), vbox3, FALSE, FALSE, 0);

	top_btn = gtk_button_new_from_stock(GTK_STOCK_GOTO_TOP);
	gtk_widget_show(top_btn);
	gtk_box_pack_start(GTK_BOX(vbox3), top_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(top_btn), "clicked",
			 G_CALLBACK(prefs_template_top_cb), NULL);
	CLAWS_SET_TIP(top_btn,
			_("Move the selected template to the top"));

	PACK_VSPACER(vbox3, spc_vbox, VSPACING_NARROW_2);

	up_btn = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_widget_show(up_btn);
	gtk_box_pack_start (GTK_BOX(vbox3), up_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(up_btn), "clicked",
			 G_CALLBACK(prefs_template_up_cb), NULL);
	CLAWS_SET_TIP(up_btn,
			_("Move the selected template up"));

	down_btn = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_widget_show (down_btn);
	gtk_box_pack_start(GTK_BOX (vbox3), down_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT (down_btn), "clicked",
			 G_CALLBACK(prefs_template_down_cb), NULL);
	CLAWS_SET_TIP(down_btn,
			_("Move the selected template down"));

	PACK_VSPACER(vbox3, spc_vbox, VSPACING_NARROW_2);

	bottom_btn = gtk_button_new_from_stock(GTK_STOCK_GOTO_BOTTOM);
	gtk_widget_show(bottom_btn);
	gtk_box_pack_start(GTK_BOX(vbox3), bottom_btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(bottom_btn), "clicked",
			 G_CALLBACK(prefs_template_bottom_cb), NULL);
	CLAWS_SET_TIP(bottom_btn,
			_("Move the selected template to the bottom"));

	list_view = prefs_template_list_view_create();
	gtk_widget_show(list_view);
	gtk_widget_set_size_request(scroll1, -1, 140);
	gtk_container_add(GTK_CONTAINER(scroll1), list_view);

	/* help | cancel | ok */
	gtkut_stock_button_set_create_with_help(&confirm_area, &help_btn,
			&cancel_btn, GTK_STOCK_CANCEL,
			&ok_btn, GTK_STOCK_OK,
			NULL, NULL);
	gtk_widget_show(confirm_area);
	gtk_box_pack_end(GTK_BOX(vbox), confirm_area, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	gtk_window_set_title(GTK_WINDOW(window), _("Template configuration"));

	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(prefs_template_deleted_cb), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(prefs_template_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(prefs_template_key_pressed_cb), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(prefs_template_ok_cb), NULL);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(prefs_template_cancel_cb), NULL);
	g_signal_connect(G_OBJECT(help_btn), "clicked",
			 G_CALLBACK(manual_open_with_anchor_cb),
			 MANUAL_ANCHOR_TEMPLATES);

	if (!geometry.min_height) {
		geometry.min_width = 500;
		geometry.min_height = 540;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.templateswin_width,
				    prefs_common.templateswin_height);

	templates.window = window;
	templates.ok_btn = ok_btn;
	templates.list_view = list_view;
	templates.text_value = text_value;
}

static void prefs_template_reset_dialog(void)
{
	GtkTextBuffer *buffer;

	gtk_entry_set_text(GTK_ENTRY(templates.entry_name), "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_from), "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_to), "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_cc), "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_bcc), "");			
	gtk_entry_set_text(GTK_ENTRY(templates.entry_subject), "");
	
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(templates.text_value));
	gtk_text_buffer_set_text(buffer, "", -1);
}

static void prefs_template_clear_list(void)
{
	GtkListStore *store;
	
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW
				(templates.list_view)));
	gtk_list_store_clear(store);

	prefs_template_list_view_insert_template(templates.list_view,
						 -1, _("(New)"),
						 NULL);
}

static void prefs_template_window_setup(void)
{
	GSList *tmpl_list;
	GSList *cur;
	Template *tmpl;
	int i;

	manage_window_set_transient(GTK_WINDOW(templates.window));
	gtk_widget_grab_focus(templates.ok_btn);

	prefs_template_clear_list();
	
	tmpl_list = template_read_config();

	address_completion_start(templates.window);

	for (i=0; widgets_table[i].label; i++) {
		if (widgets_table[i].compl)
			address_completion_register_entry(
				GTK_ENTRY(*(widgets_table[i].entry)), TRUE);
	}

	for (cur = tmpl_list; cur != NULL; cur = cur->next) {
		tmpl = (Template *)cur->data;
		prefs_template_list_view_insert_template(templates.list_view,
							 -1, tmpl->name, 
							 tmpl);
	}

	prefs_template_reset_dialog();

	g_slist_free(tmpl_list);
}

static gint prefs_template_deleted_cb(GtkWidget *widget, GdkEventAny *event,
				      gpointer data)
{
	prefs_template_cancel_cb(NULL, NULL);
	return TRUE;
}

static gboolean prefs_template_key_pressed_cb(GtkWidget *widget,
					      GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_KEY_Escape)
		prefs_template_cancel_cb(NULL, NULL);
	else {
		GtkWidget *focused = gtkut_get_focused_child(
					GTK_CONTAINER(widget));
		if (focused && GTK_IS_EDITABLE(focused)) {
			modified = TRUE;
		}
	}
	return FALSE;
}

static gboolean prefs_template_search_func_cb (GtkTreeModel *model, gint column, const gchar *key, 
						GtkTreeIter *iter, gpointer search_data) 
{
	gchar *store_string;
	gint key_len;
	gboolean retval;
	GtkTreePath *path;

	gtk_tree_model_get (model, iter, column, &store_string, -1);

	if (!store_string || !key) return FALSE;

	key_len = strlen (key);
	retval = (strncmp (key, store_string, key_len) != 0);

	g_free(store_string);
	debug_print("selecting row\n");
	path = gtk_tree_model_get_path(model, iter);
	prefs_template_select_row(GTK_TREE_VIEW(templates.list_view), path);
	gtk_tree_path_free(path);

	return retval;
}
static void prefs_template_address_completion_end(void)
{
	gint i;

	for (i=0; widgets_table[i].label; i++) {
		if (widgets_table[i].compl)
			address_completion_unregister_entry(
				GTK_ENTRY(*(widgets_table[i].entry)));
	}
	address_completion_end(templates.window);
}

static void prefs_template_ok_cb(gpointer action, gpointer data)
{
	GSList *tmpl_list;
	GtkListStore *store;

	if (modified && alertpanel(_("Entry not saved"),
				 _("The entry was not saved. Close anyway?"),
				 GTK_STOCK_CLOSE, _("+_Continue editing"), 
				 NULL) != G_ALERTDEFAULT) {
		return;
	} 

	prefs_template_address_completion_end();

	modified = FALSE;
	modified_list = FALSE;
	tmpl_list = prefs_template_get_list();
	template_set_config(tmpl_list);
	compose_reflect_prefs_all();
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW
				(templates.list_view)));
	gtk_list_store_clear(store);
	gtk_widget_destroy(templates.window);
	inc_unlock();
}

static void prefs_template_cancel_cb(gpointer action, gpointer data)
{
	GtkListStore *store;

	if (modified && alertpanel(_("Entry not saved"),
				 _("The entry was not saved. Close anyway?"),
				 GTK_STOCK_CLOSE, _("+_Continue editing"),
				 NULL) != G_ALERTDEFAULT) {
		return;
	} else if (modified_list && alertpanel(_("Templates list not saved"),
				 _("The templates list has been modified. Close anyway?"),
				 GTK_STOCK_CLOSE, _("+_Continue editing"), 
				 NULL) != G_ALERTDEFAULT) {
		return;
	}

	prefs_template_address_completion_end();

	modified = FALSE;
	modified_list = FALSE;
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW
				(templates.list_view)));
	gtk_list_store_clear(store);
	gtk_widget_destroy(templates.window);
	inc_unlock();
}

/*!
 *\brief	Request list for storage. New list is owned
 *		by template.c...
 */
static GSList *prefs_template_get_list(void)
{
	GSList *tmpl_list = NULL;
	Template *tmpl;
	GtkTreeModel *model;
	GtkTreeIter iter;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));
	if (!gtk_tree_model_get_iter_first(model, &iter))
		return NULL;

	do {
		gtk_tree_model_get(model, &iter,
				   TEMPL_DATA, &tmpl,
				   -1);
		
		if (tmpl) {
			Template *ntmpl;
			
			ntmpl = g_new(Template, 1);
			ntmpl->load_filename = NULL;
			ntmpl->name    = tmpl->name && *(tmpl->name) 
					 ? g_strdup(tmpl->name) 
					 : NULL;
			ntmpl->subject = tmpl->subject && *(tmpl->subject) 
					 ? g_strdup(tmpl->subject) 
					 : NULL;
			ntmpl->from    = tmpl->from && *(tmpl->from)
					 ? g_strdup(tmpl->from)
					 : NULL;
			ntmpl->to      = tmpl->to && *(tmpl->to)
					 ? g_strdup(tmpl->to)
					 : NULL;
			ntmpl->cc      = tmpl->cc && *(tmpl->cc)
					 ? g_strdup(tmpl->cc)
					 : NULL;
			ntmpl->bcc     = tmpl->bcc && *(tmpl->bcc)
					 ? g_strdup(tmpl->bcc)
					 : NULL;	
			ntmpl->value   = tmpl->value && *(tmpl->value)
			                 ? g_strdup(tmpl->value)
					 : NULL;
			tmpl_list = g_slist_append(tmpl_list, ntmpl);
		}			
	
	} while (gtk_tree_model_iter_next(model, &iter)); 

	return tmpl_list;
}

gboolean prefs_template_string_is_valid(gchar *string, gint *line, gboolean escaped_string, gboolean email)
{
	gboolean result = TRUE;
	if (string && *string != '\0') {
		gchar *tmp = NULL;
		gchar *parsed_buf;
		MsgInfo dummyinfo;
		PrefsAccount *account = account_get_default();

		if (escaped_string) {
			tmp = malloc(strlen(string)+1);
			pref_get_unescaped_pref(tmp, string);
		} else {
			tmp = g_strdup(string);
		}
		memset(&dummyinfo, 0, sizeof(MsgInfo));
		/* init dummy fields, so we can test the result of the parse */
		dummyinfo.date="Sat, 30 May 2009 01:23:45 +0200";
		dummyinfo.fromname="John Doe";
		dummyinfo.from="John Doe <john@example.com>";
		dummyinfo.to="John Doe <john@example.com>";
		dummyinfo.cc="John Doe <john@example.com>";
		dummyinfo.msgid="<1234john@example.com>";
		dummyinfo.inreplyto="<1234john@example.com>";
		dummyinfo.newsgroups="alt.test";
		dummyinfo.subject="subject";
		
		
#ifdef USE_ENCHANT
		quote_fmt_init(&dummyinfo, NULL, NULL, TRUE, account, FALSE, NULL);
#else
		quote_fmt_init(&dummyinfo, NULL, NULL, TRUE, account, FALSE);
#endif
		quote_fmt_scan_string(tmp);
		quote_fmt_parse();
		g_free(tmp);
		parsed_buf = quote_fmt_get_buffer();
		if (!parsed_buf) {
			if (line)
				*line = quote_fmt_get_line();
			return FALSE;
		}
		if (email) {
			const gchar *start = strrchr(parsed_buf, '<');
			const gchar *end = strrchr(parsed_buf, '>');
			const gchar *at = strrchr(parsed_buf, '@');
			const gchar *space = strrchr(parsed_buf, ' ');
			if (!at)
				result = FALSE;
			if (at && space && (!start || !end || end < start || start < space))
				result = FALSE;
		}
		quote_fmt_reset_vartable();
	}
	return result;
}

static gboolean prefs_template_list_view_set_row(gint row)
/* return TRUE if the row could be modified */
{
	Template *tmpl;
	gchar *name;
	gchar *subject;
	gchar *from;
	gchar *to;
	gchar *cc;
	gchar *bcc;	
	gchar *value;
	GtkTextBuffer *buffer;
	GtkTextIter start, end;
	gint line;

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(templates.text_value));
	gtk_text_buffer_get_start_iter(buffer, &start);
	gtk_text_buffer_get_iter_at_offset(buffer, &end, -1);
	value = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

	if (value && *value == '\0') {
			g_free(value);
		value = NULL;
		}
	if (!prefs_template_string_is_valid(value, &line, TRUE, FALSE)) {
		alertpanel_error(_("The body of the template has an error at line %d."), line);
		g_free(value);
		return FALSE;
	}

	name = gtk_editable_get_chars(GTK_EDITABLE(templates.entry_name),
				      0, -1);
	if (*name == '\0') {
		alertpanel_error(_("The template's name is not set."));
		g_free(value);
		return FALSE;
	}
	from = gtk_editable_get_chars(GTK_EDITABLE(templates.entry_from),
				    0, -1);
	to = gtk_editable_get_chars(GTK_EDITABLE(templates.entry_to),
				    0, -1);
	cc = gtk_editable_get_chars(GTK_EDITABLE(templates.entry_cc),
				    0, -1);
	bcc = gtk_editable_get_chars(GTK_EDITABLE(templates.entry_bcc),
				    0, -1);
	subject = gtk_editable_get_chars(GTK_EDITABLE(templates.entry_subject),
					 0, -1);

	if (from && *from == '\0') {
		g_free(from);
		from = NULL;
	}
	if (to && *to == '\0') {
		g_free(to);
		to = NULL;
	}
	if (cc && *cc == '\0') {
		g_free(cc);
		cc = NULL;
	}
	if (bcc && *bcc == '\0') {
		g_free(bcc);
		bcc = NULL;
	}
	if (subject && *subject == '\0') {
		g_free(subject);
		subject = NULL;
	}

	if (!prefs_template_string_is_valid(from, NULL, TRUE, TRUE)) {
		alertpanel_error(_("The \"From\" field of the template contains an invalid email address."));
		g_free(from);
		g_free(value);
		return FALSE;
	}
	if (!prefs_template_string_is_valid(to, NULL, TRUE, TRUE)) {
		alertpanel_error(_("The \"To\" field of the template contains an invalid email address."));
		g_free(to);
		g_free(value);
		return FALSE;
	}
	if (!prefs_template_string_is_valid(cc, NULL, TRUE, TRUE)) {
		alertpanel_error(_("The \"Cc\" field of the template contains an invalid email address."));	
		g_free(cc);
		g_free(value);
		return FALSE;
	}
	if (!prefs_template_string_is_valid(bcc, NULL, TRUE, TRUE)) {
		alertpanel_error(_("The \"Bcc\" field of the template contains an invalid email address."));	
		g_free(bcc);
		g_free(value);
		return FALSE;
	}
	if (!prefs_template_string_is_valid(subject, NULL, TRUE, FALSE)) {
		alertpanel_error(_("The \"Subject\" field of the template is invalid."));	
		g_free(subject);
		g_free(value);
		return FALSE;
	}
	
	tmpl = g_new(Template, 1);
	tmpl->load_filename = NULL;
	tmpl->name = name;
	tmpl->subject = subject;
	tmpl->from = from;
	tmpl->to = to;
	tmpl->cc = cc;
	tmpl->bcc = bcc;	
	tmpl->value = value;

	prefs_template_list_view_insert_template(templates.list_view,
						 row, tmpl->name, tmpl);

	return TRUE;
}

static void prefs_template_register_cb(gpointer action, gpointer data)
{
	modified = !prefs_template_list_view_set_row(-1);
	modified_list = TRUE;
}

static void prefs_template_substitute_cb(gpointer action, gpointer data)
{
	Template *tmpl;
	gint row;
	GtkTreeIter iter;
	GtkTreeModel *model;

	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row <= 0)
		return;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));
	if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row))
		return;

	gtk_tree_model_get(model, &iter, TEMPL_DATA, &tmpl, -1);
	if (!tmpl)
		return;

	modified = !prefs_template_list_view_set_row(row);
	modified_list = TRUE;
}

static void prefs_template_delete_cb(gpointer action, gpointer data)
{
	Template *tmpl;
	gint row;
	GtkTreeIter iter;
	GtkTreeModel *model;

	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row <= 0)
		return;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));
	if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row))
		return;

	gtk_tree_model_get(model, &iter, TEMPL_DATA, &tmpl, -1);
	if (!tmpl)
		return;

	if (alertpanel(_("Delete template"),
		       _("Do you really want to delete this template?"),
		       GTK_STOCK_CANCEL, GTK_STOCK_DELETE,
		       NULL) != G_ALERTALTERNATE)
		return;

	gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
	prefs_template_reset_dialog();
	modified_list = TRUE;	
}

static void prefs_template_delete_all_cb(gpointer action, gpointer data)
{
	if (alertpanel(_("Delete all templates"),
			  _("Do you really want to delete all the templates?"),
			  GTK_STOCK_CANCEL, "+"GTK_STOCK_DELETE, NULL) == G_ALERTDEFAULT)
	   return;

	prefs_template_clear_list();
	modified = FALSE;

	prefs_template_reset_dialog();
	modified_list = TRUE;
}

static void prefs_template_duplicate_cb(gpointer action, gpointer data)
{
	Template *tmpl;
	gint row;
	GtkTreeIter iter;
	GtkTreeModel *model;
	
	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row <= 0)
		return;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));
	if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row))
		return;

	gtk_tree_model_get(model, &iter, TEMPL_DATA, &tmpl, -1);
	if (!tmpl)
		return;

	modified_list = !prefs_template_list_view_set_row(-row-2);
}

static void prefs_template_clear_cb(gpointer action, gpointer data)
{
   prefs_template_reset_dialog();
}

static void prefs_template_top_cb(gpointer action, gpointer data)
{
	gint row;
	GtkTreeIter top, sel;
	GtkTreeModel *model;

	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row <= 1) 
		return;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));		
	
	if (!gtk_tree_model_iter_nth_child(model, &top, NULL, 0)
	||  !gtk_tree_model_iter_nth_child(model, &sel, NULL, row))
		return;

	gtk_list_store_move_after(GTK_LIST_STORE(model), &sel, &top);
	gtkut_list_view_select_row(templates.list_view, 1);
	modified_list = TRUE;
}

static void prefs_template_up_cb(gpointer action, gpointer data)
{
	gint row;
	GtkTreeIter top, sel;
	GtkTreeModel *model;

	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row <= 1) 
		return;
		
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));	

	if (!gtk_tree_model_iter_nth_child(model, &top, NULL, row - 1)
	||  !gtk_tree_model_iter_nth_child(model, &sel, NULL, row))
		return;

	gtk_list_store_swap(GTK_LIST_STORE(model), &top, &sel);
	gtkut_list_view_select_row(templates.list_view, row - 1);
	modified_list = TRUE;
}

static void prefs_template_down_cb(gpointer action, gpointer data)
{
	gint row, n_rows;
	GtkTreeIter top, sel;
	GtkTreeModel *model;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));	
	n_rows = gtk_tree_model_iter_n_children(model, NULL);
	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row < 1 || row >= n_rows - 1)
		return;

	if (!gtk_tree_model_iter_nth_child(model, &top, NULL, row)
	||  !gtk_tree_model_iter_nth_child(model, &sel, NULL, row + 1))
		return;
			
	gtk_list_store_swap(GTK_LIST_STORE(model), &top, &sel);
	gtkut_list_view_select_row(templates.list_view, row + 1);
	modified_list = TRUE;
}

static void prefs_template_bottom_cb(gpointer action, gpointer data)
{
	gint row, n_rows;
	GtkTreeIter top, sel;
	GtkTreeModel *model;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(templates.list_view));	
	n_rows = gtk_tree_model_iter_n_children(model, NULL);
	row = gtkut_list_view_get_selected_row(templates.list_view);
	if (row < 1 || row >= n_rows - 1)
		return;

	if (!gtk_tree_model_iter_nth_child(model, &top, NULL, row)
	||  !gtk_tree_model_iter_nth_child(model, &sel, NULL, n_rows - 1))
		return;

	gtk_list_store_move_after(GTK_LIST_STORE(model), &top, &sel);		
	gtkut_list_view_select_row(templates.list_view, n_rows - 1);
	modified_list = TRUE;
}

static GtkListStore* prefs_template_create_data_store(void)
{
	return gtk_list_store_new(N_TEMPL_COLUMNS,
				  G_TYPE_STRING,	
				  G_TYPE_POINTER,
				  G_TYPE_AUTO_POINTER,
				  -1);
}

static void prefs_template_list_view_insert_template(GtkWidget *list_view,
						     gint row,
						     const gchar *template,
						     Template *data)
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
				   TEMPL_TEXT, template,
				   TEMPL_DATA, data,
				   -1);
	} else if (row < -1) {
		/* duplicate */
		gtk_list_store_insert_after(list_store, &iter, &sibling);
		gtk_list_store_set(list_store, &iter,
				   TEMPL_TEXT, template,
				   TEMPL_DATA, data,
				   -1);
	} else {
		/* change existing */
		GAuto *auto_data =
					g_auto_pointer_new_with_free(data, (GFreeFunc) template_free);  

		/* if replacing data in an existing row, the auto pointer takes care
		 * of destroying the Template data */
		gtk_list_store_set(list_store, &iter,
				   TEMPL_TEXT, template,
				   TEMPL_DATA, data,
				   TEMPL_AUTO_DATA, auto_data,
				   -1);

		g_auto_pointer_free(auto_data);
	}
}

static GtkActionGroup *prefs_template_popup_action = NULL;
static GtkWidget *prefs_template_popup_menu = NULL;

static GtkActionEntry prefs_template_popup_entries[] =
{
	{"PrefsTemplatePopup",			NULL, "PrefsTemplatePopup" },
	{"PrefsTemplatePopup/Delete",		NULL, N_("_Delete"), NULL, NULL, G_CALLBACK(prefs_template_delete_cb) },
	{"PrefsTemplatePopup/DeleteAll",	NULL, N_("Delete _all"), NULL, NULL, G_CALLBACK(prefs_template_delete_all_cb) },
	{"PrefsTemplatePopup/Duplicate",	NULL, N_("D_uplicate"), NULL, NULL, G_CALLBACK(prefs_template_duplicate_cb) },
};

static void prefs_template_row_selected(GtkTreeSelection *selection,
					GtkTreeView *list_view)
{
	GtkTreePath *path;
	GtkTreeIter iter;
	GtkTreeModel *model;
	
	if (!gtk_tree_selection_get_selected(selection, &model, &iter))
		return;
	
	path = gtk_tree_model_get_path(model, &iter);
	prefs_template_select_row(list_view, path);
	gtk_tree_path_free(path);
}

static gint prefs_template_list_btn_pressed(GtkWidget *widget, GdkEventButton *event,
				   GtkTreeView *list_view)
{
   if (event) {
	   /* left- or right-button click */
	   if (event->button == 1 || event->button == 3) {
		   GtkTreePath *path = NULL;
		   if (gtk_tree_view_get_path_at_pos( list_view, event->x, event->y,
							   &path, NULL, NULL, NULL)) {
			   prefs_template_select_row(list_view, path);
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

		   if (!prefs_template_popup_menu) {
				prefs_template_popup_action = cm_menu_create_action_group("PrefsTemplatePopup", prefs_template_popup_entries,
					G_N_ELEMENTS(prefs_template_popup_entries), (gpointer)list_view);
				MENUITEM_ADDUI("/Menus", "PrefsTemplatePopup", "PrefsTemplatePopup", GTK_UI_MANAGER_MENU)
				MENUITEM_ADDUI("/Menus/PrefsTemplatePopup", "Delete", "PrefsTemplatePopup/Delete", GTK_UI_MANAGER_MENUITEM)
				MENUITEM_ADDUI("/Menus/PrefsTemplatePopup", "DeleteAll", "PrefsTemplatePopup/DeleteAll", GTK_UI_MANAGER_MENUITEM)
				MENUITEM_ADDUI("/Menus/PrefsTemplatePopup", "Duplicate", "PrefsTemplatePopup/Duplicate", GTK_UI_MANAGER_MENUITEM)
				prefs_template_popup_menu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
					gtk_ui_manager_get_widget(gtkut_ui_manager(), "/Menus/PrefsTemplatePopup")) );
		   }

		   /* grey out some popup menu items if there is no selected row */
		   row = gtkut_list_view_get_selected_row(GTK_WIDGET(list_view));
			cm_menu_set_sensitive("PrefsTemplatePopup/Delete", (row > 0));
			cm_menu_set_sensitive("PrefsTemplatePopup/Duplicate", (row > 0));

		   /* grey out seom popup menu items if there is no row
			  (not counting the (New) one at row 0) */
		   non_empty = gtk_tree_model_get_iter_first(model, &iter);
		   if (non_empty)
			   non_empty = gtk_tree_model_iter_next(model, &iter);
			cm_menu_set_sensitive("PrefsTemplatePopup/DeleteAll", non_empty);

		   gtk_menu_popup(GTK_MENU(prefs_template_popup_menu), 
					  NULL, NULL, NULL, NULL, 
					  event->button, event->time);
	   }
   }
   return FALSE;
}

static gboolean prefs_template_list_popup_menu(GtkWidget *widget, gpointer data)
{
   GtkTreeView *list_view = (GtkTreeView *)data;
   GdkEventButton event;
   
   event.button = 3;
   event.time = gtk_get_current_event_time();
   
   prefs_template_list_btn_pressed(NULL, &event, list_view);

   return TRUE;
}

static GtkWidget *prefs_template_list_view_create(void)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(prefs_template_create_data_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	

#ifndef MAEMO
	g_signal_connect(G_OBJECT(list_view), "popup-menu",
			 G_CALLBACK(prefs_template_list_popup_menu), list_view);
#else
	gtk_widget_tap_and_hold_setup(GTK_WIDGET(list_view), NULL, NULL,
			GTK_TAP_AND_HOLD_NONE | GTK_TAP_AND_HOLD_NO_INTERNALS);
	g_signal_connect(G_OBJECT(list_view), "tap-and-hold",
			 G_CALLBACK(prefs_template_list_popup_menu), list_view);
#endif
	g_signal_connect(G_OBJECT(list_view), "button-press-event",
			G_CALLBACK(prefs_template_list_btn_pressed), list_view);
	
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_reorderable(list_view, TRUE);

	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);
	g_signal_connect(G_OBJECT(selector), "changed",
			 G_CALLBACK(prefs_template_row_selected), list_view);

	/* create the columns */
	prefs_template_create_list_view_columns(GTK_WIDGET(list_view));

	return GTK_WIDGET(list_view);
}

static void prefs_template_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
			(_("Current templates"),
			 renderer,
			 "text", TEMPL_TEXT,
			 NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
	gtk_tree_view_set_search_equal_func(GTK_TREE_VIEW(list_view), prefs_template_search_func_cb , NULL, NULL);
}

 /*!
 *\brief	Triggered when a row has to be selected
 */
static void prefs_template_select_row(GtkTreeView *list_view, GtkTreePath *path)
{
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	GtkTreeSelection *selection;
	Template *tmpl;
	Template tmpl_def;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	GtkTreeIter titer;

	if (!model || !path || !gtk_tree_model_get_iter(model, &titer, path))
		return;

	/* select row */
	selection = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_select_path(selection, path);

	tmpl_def.name = _("Template");
	tmpl_def.subject = "";
	tmpl_def.from = "";
	tmpl_def.to = "";
	tmpl_def.cc = "";
	tmpl_def.bcc = "";	
	tmpl_def.value = "";

	gtk_tree_model_get(model, &titer, TEMPL_DATA, &tmpl, -1);
	if (!tmpl)
		tmpl = &tmpl_def;

	gtk_entry_set_text(GTK_ENTRY(templates.entry_name), tmpl->name);
	gtk_entry_set_text(GTK_ENTRY(templates.entry_from),
			   tmpl->from ? tmpl->from : "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_to),
			   tmpl->to ? tmpl->to : "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_cc),
			   tmpl->cc ? tmpl->cc : "");
	gtk_entry_set_text(GTK_ENTRY(templates.entry_bcc),
			   tmpl->bcc ? tmpl->bcc : "");			
	gtk_entry_set_text(GTK_ENTRY(templates.entry_subject),
			   tmpl->subject ? tmpl->subject : "");
	
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(templates.text_value));
	gtk_text_buffer_set_text(buffer, "", -1);
	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_insert(buffer, &iter, tmpl->value, -1);
}
