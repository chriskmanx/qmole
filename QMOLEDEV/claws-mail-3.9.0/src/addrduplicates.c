/* Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2012 Holger Berndt <hb@claws-mail.org> 
 * and the Claws Mail team
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
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>
#include <string.h>

#include "defs.h"

#ifdef USE_LDAP
#include "ldapserver.h"
#include "ldapupdate.h"
#endif
#include "addrduplicates.h"
#include "addrbook.h"
#include "addressbook.h"
#include "editaddress.h"
#include "alertpanel.h"
#include "gtkutils.h"
#include "inc.h"
#include "utils.h"
#include "prefs_common.h"

typedef struct
{
	ItemPerson        *person;
	AddressDataSource *ds;
	gchar             *book_path;
}
AddrDupListEntry;

enum {
    COL_BOOKPATH = 0,
    COL_NAME,
    COL_ITEM,
    COL_DS,
    NUM_COLS
};

static gboolean create_dialog();
static void refresh_addr_hash(void);
static void refresh_stores(gchar*,GSList*);
static void present_finder_results(GtkWindow*);
static void cb_finder_results_dialog_destroy(GtkWindow*, gpointer);
static gboolean cb_finder_results_dialog_key_pressed(GtkWidget*, GdkEventKey*,
        gpointer);
static void destroy_addr_hash_val(gpointer);
static GSList* deep_copy_hash_val(GSList*);
static void fill_hash_table();
static gint collect_emails(ItemPerson*, AddressDataSource*);
static gboolean is_not_duplicate(gpointer, gpointer, gpointer);
static gint books_compare(gconstpointer, gconstpointer);
static GtkWidget* create_email_view(GtkListStore*);
static GtkWidget* create_detail_view(GtkListStore*);
static void append_to_email_store(gpointer,gpointer,gpointer);
static void email_selection_changed(GtkTreeSelection*,gpointer);
static void detail_selection_changed(GtkTreeSelection*,gpointer);
static void detail_row_activated(GtkTreeView*,GtkTreePath*,
                                 GtkTreeViewColumn*,
                                 gpointer);
static gboolean detail_focus_in(GtkWidget*,GdkEventFocus*,gpointer);
static gboolean detail_focus_out(GtkWidget*,GdkEventFocus*,gpointer);

static void cb_del_btn_clicked(GtkButton *, gpointer);
static void cb_edit_btn_clicked(GtkButton *, gpointer);
static gchar* get_bookpath(ItemPerson*,AddressDataSource*);
static gboolean is_editing_entry_only_selection(void);
static void edit_post_update_cb(ItemPerson*);

static GHashTable *addr_hash;
static gboolean include_same_book = TRUE;
static gboolean include_other_books = TRUE;

static GtkListStore *email_store;
static GtkListStore *detail_store;
static GtkWidget    *email_view;
static GtkWidget    *detail_view;
static GtkWidget    *inline_edit_vbox;

static GtkWidget *del_btn;
static GtkWidget *edit_btn;

static GtkWidget *dialog;
static gchar *editing_uid;
static gboolean detail_view_has_focus;

void addrduplicates_find(GtkWindow *parent)
{
	if(create_dialog()) {
		refresh_addr_hash();
		present_finder_results(parent);
	}
}

static gboolean create_dialog()
{
	gboolean want_search;
	GtkWidget *vbox;
	GtkWidget *check_same_book;
	GtkWidget *check_other_book;
	AlertValue val;

	want_search = FALSE;

	vbox = gtk_vbox_new(FALSE, 0);
	check_same_book = gtk_check_button_new_with_label(_("Show duplicates in "
	                  "the same book"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_same_book),
	                             include_same_book);
	gtk_box_pack_start(GTK_BOX(vbox), check_same_book, FALSE, FALSE, 0);
	gtk_widget_show(check_same_book);
	check_other_book = gtk_check_button_new_with_label(_("Show duplicates in "
	                   "different books"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_other_book),
	                             include_other_books);
	gtk_box_pack_start(GTK_BOX(vbox), check_other_book, FALSE, FALSE, 0);
	gtk_widget_show(check_other_book);

	/* prevent checkboxes from being destroyed on dialog close */
	g_object_ref(check_same_book);
	g_object_ref(check_other_book);

	val = alertpanel_full(_("Find address book email duplicates"),
	                      _("Claws Mail will now search for duplicate email "
	                        "addresses in the address book."),
	                      GTK_STOCK_CANCEL,GTK_STOCK_FIND,NULL, FALSE, vbox, ALERT_NOTICE,
	                      G_ALERTALTERNATE);
	if(val == G_ALERTALTERNATE) {
		want_search = TRUE;

		/* save options */
		include_same_book =
		    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check_same_book));
		include_other_books =
		    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check_other_book));

	}

	g_object_unref(check_same_book);
	g_object_unref(check_other_book);
	return want_search;
}

static void refresh_addr_hash(void)
{
	if(addr_hash)
		g_hash_table_destroy(addr_hash);
	addr_hash = g_hash_table_new_full(g_str_hash, g_str_equal,
	                                  g_free, destroy_addr_hash_val);
	fill_hash_table();
}

static void destroy_addr_hash_val(gpointer value)
{
	GSList *list = (GSList*) value;
	GSList *walk;

	for(walk = list; walk; walk = walk->next) {
		AddrDupListEntry *entry = (AddrDupListEntry*) walk->data;
		if(entry && entry->book_path)
			g_free(entry->book_path);
		if(entry)
			g_free(entry);
	}
	if(list)
		g_slist_free(list);
}

static GSList* deep_copy_hash_val(GSList *in)
{
	GSList *walk;
	GSList *out = NULL;

	out = g_slist_copy(in);
	for(walk = out; walk; walk = walk->next) {
		AddrDupListEntry *out_entry;
		AddrDupListEntry *in_entry = walk->data;

		out_entry = g_new0(AddrDupListEntry,1);
		out_entry->person = in_entry->person;
		out_entry->ds     = in_entry->ds;
		out_entry->book_path = g_strdup(in_entry->book_path);
		walk->data = out_entry;
	}

	return out;
}

static void fill_hash_table()
{
	addrindex_load_person_ds(collect_emails);
	g_hash_table_foreach_remove(addr_hash,is_not_duplicate, NULL);
}

static gboolean is_not_duplicate(gpointer key, gpointer value,
                                 gpointer user_data)
{
	gboolean is_in_same_book;
	gboolean is_in_other_books;
	GSList *books;
	GSList *walk;
	gboolean retval;
	GSList *list = value;

	/* remove everything that is just in one book */
	if(g_slist_length(list) <= 1)
		return TRUE;

	/* work on a shallow copy */
	books = g_slist_copy(list);

	/* sorting the list makes it easier to check for books */
	books = g_slist_sort(books, books_compare);

	/* check if a book appears twice */
	is_in_same_book = FALSE;
	for(walk = books; walk && walk->next; walk = walk->next) {
		if(books_compare(walk->data, walk->next->data) == 0) {
			is_in_same_book = TRUE;
			break;
		}
	}

	/* check is at least two different books appear in the list */
	is_in_other_books = FALSE;
	if(books && books->next) {
		for(walk = books->next; walk; walk = walk->next) {
			if(books_compare(walk->data, books->data) != 0) {
				is_in_other_books = TRUE;
				break;
			}
		}
	}

	/* delete the shallow copy */
	g_slist_free(books);

	retval = FALSE;
	if(is_in_same_book && include_same_book)
		retval = TRUE;
	if(is_in_other_books && include_other_books)
		retval = TRUE;
	retval = !retval;

	return retval;
}

static gint collect_emails(ItemPerson *itemperson, AddressDataSource *ds)
{
	gchar *addr;
	GList *nodeM;
	GSList *old_val;
	GSList *new_val;
	AddrDupListEntry *entry;

	/* Process each E-Mail address */
	nodeM = itemperson->listEMail;
	while(nodeM) {
		ItemEMail *email = nodeM->data;

		addr = g_utf8_strdown(email->address, -1);
		old_val = g_hash_table_lookup(addr_hash, addr);
		if(old_val)
			new_val = deep_copy_hash_val(old_val);
		else
			new_val = NULL;

		entry = g_new0(AddrDupListEntry,1);
		entry->person = itemperson;
		entry->ds     = ds;
		entry->book_path = get_bookpath(itemperson, ds);

		new_val = g_slist_prepend(new_val, entry);
		g_hash_table_insert(addr_hash, addr, new_val);

		nodeM = g_list_next(nodeM);
	}
	return 0;
}

static gint books_compare(gconstpointer a, gconstpointer b)
{
	const AddrDupListEntry *entry1;
	const AddrDupListEntry *entry2;
	entry1 = a;
	entry2 = b;
	return strcmp(entry1->book_path, entry2->book_path);
}

static void present_finder_results(GtkWindow *parent)
{
	GtkWidget *scrolled_win;
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *hpaned;
	GtkWidget *vpaned;
	GtkWidget *close;
	gint pos;
	GtkTreeSelection *email_select;
	GtkTreeSelection *detail_select;
	static GdkGeometry geometry;

	if(g_hash_table_size(addr_hash) == 0) {
		alertpanel_notice(_("No duplicate email addresses found in the address book"));
		return;
	}

	email_store = gtk_list_store_new(1, G_TYPE_STRING);
	refresh_stores(NULL,NULL);
	email_view = create_email_view(email_store);
	email_select = gtk_tree_view_get_selection(GTK_TREE_VIEW(email_view));
	gtk_tree_selection_set_mode(email_select,GTK_SELECTION_SINGLE);

	g_signal_connect(email_select, "changed",
	                 (GCallback)email_selection_changed, NULL);

	detail_store = gtk_list_store_new(NUM_COLS, G_TYPE_STRING, G_TYPE_STRING,
	                                  G_TYPE_POINTER, G_TYPE_POINTER);
	detail_view = create_detail_view(detail_store);
	detail_select = gtk_tree_view_get_selection(GTK_TREE_VIEW(detail_view));
	gtk_tree_selection_set_mode(detail_select,GTK_SELECTION_MULTIPLE);

	g_signal_connect(detail_select, "changed",
	                 (GCallback)detail_selection_changed, NULL);

	dialog = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "address_dupes_finder");
	gtk_window_set_transient_for(GTK_WINDOW(dialog),parent);
	gtk_window_set_modal(GTK_WINDOW(dialog),TRUE);
	if(!geometry.min_height) {
		geometry.min_width = 600;
		geometry.min_height = 400;
	}
	gtk_window_set_geometry_hints(GTK_WINDOW(dialog), NULL, &geometry,
	                              GDK_HINT_MIN_SIZE);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Duplicate email addresses"));

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(dialog), vbox);

	hpaned = gtk_hpaned_new();
	gtk_box_pack_start(GTK_BOX(vbox), hpaned, TRUE, TRUE, 0);

	scrolled_win = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
	                               GTK_POLICY_AUTOMATIC,
	                               GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_win), email_view);

	gtk_paned_add1(GTK_PANED(hpaned), scrolled_win);

	scrolled_win = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
	                               GTK_POLICY_AUTOMATIC,
	                               GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_win), detail_view);

	if (prefs_common.addressbook_use_editaddress_dialog) {
		gtk_paned_add2(GTK_PANED(hpaned), scrolled_win);
		inline_edit_vbox = NULL;
	} else {
		inline_edit_vbox = gtk_vbox_new(FALSE, 4);
		vpaned = gtk_vpaned_new();
		gtk_paned_pack1(GTK_PANED(vpaned), scrolled_win, FALSE, FALSE);
		gtk_paned_pack2(GTK_PANED(vpaned), inline_edit_vbox, TRUE, FALSE);
		gtk_paned_pack2(GTK_PANED(hpaned), vpaned, TRUE, FALSE);
	}

	g_object_get(G_OBJECT(hpaned),
	             "position", &pos, NULL);
	if(pos < 200)
		gtk_paned_set_position(GTK_PANED(hpaned), 200);

	hbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(hbox), GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(hbox), 2);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
	gtk_box_pack_end(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	edit_btn = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	gtk_box_pack_start(GTK_BOX(hbox), edit_btn, TRUE, TRUE, 0);
	gtk_widget_set_sensitive(edit_btn, FALSE);

	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_box_pack_start(GTK_BOX(hbox), del_btn, TRUE, TRUE, 0);
	gtk_widget_set_sensitive(del_btn, FALSE);

	close = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
	gtk_box_pack_start(GTK_BOX(hbox), close, TRUE, TRUE, 0);

	g_signal_connect(dialog, "destroy",
	                 G_CALLBACK(cb_finder_results_dialog_destroy), NULL);
	g_signal_connect(G_OBJECT(dialog), "key-press-event",
	                 G_CALLBACK(cb_finder_results_dialog_key_pressed), NULL);
	g_signal_connect_swapped(close, "clicked",
	                         G_CALLBACK(gtk_widget_destroy), dialog);
	g_signal_connect(del_btn, "clicked",
	                 G_CALLBACK(cb_del_btn_clicked), detail_view);
	g_signal_connect(edit_btn, "clicked",
	                 G_CALLBACK(cb_edit_btn_clicked), detail_view);

	inc_lock();
	gtk_widget_show_all(dialog);
}

static void cb_finder_results_dialog_destroy(GtkWindow *win, gpointer data)
{
	email_store = NULL;
	detail_store = NULL;
	email_view = NULL;
	inline_edit_vbox = NULL;

	if(addr_hash) {
		g_hash_table_destroy(addr_hash);
		addr_hash = NULL;
	}
	dialog = NULL;
	addressbook_refresh();
	inc_unlock();
}

static GtkWidget* create_email_view(GtkListStore *store)
{
	GtkWidget *view;
	GtkCellRenderer *renderer;

	view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(view), prefs_common.use_stripes_everywhere);
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(view),
	        -1,
	        _("Address"),
	        renderer,
	        "text", 0,
	        NULL);
	g_object_unref(store);
	return view;
}

static GtkWidget* create_detail_view(GtkListStore *store)
{
	GtkWidget *view;
	GtkCellRenderer *renderer;
	GList *cols;
	GList *walk;

	view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(view), prefs_common.use_stripes_everywhere);
	renderer = gtk_cell_renderer_text_new();

	/* col 1 */
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(view),
	        -1,
	        _("Address book path"),
	        renderer,
	        "text", COL_BOOKPATH,
	        NULL);
	/* col 2 */
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(view),
	        -1,
	        _("Name"),
	        renderer,
	        "text", COL_NAME,
	        NULL);

	cols = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
	for(walk = cols; walk; walk = walk->next)
		gtk_tree_view_column_set_resizable(GTK_TREE_VIEW_COLUMN(walk->data),
		                                   TRUE);
	g_list_free(cols);

	g_signal_connect(view, "row-activated",
	                 G_CALLBACK(detail_row_activated), NULL);

	g_signal_connect(view, "focus-in-event",
	                 G_CALLBACK(detail_focus_in), NULL);
	g_signal_connect(view, "focus-out-event",
	                 G_CALLBACK(detail_focus_out), NULL);


	return view;
}

static void append_to_email_store(gpointer key,gpointer value,gpointer data)
{
	GtkTreeIter iter;
	GtkListStore *store = (GtkListStore*) data;

	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter, 0, (gchar*) key, -1);
}

static gboolean is_editing_entry_only_selection(void)
{
	GtkTreeSelection *sel_detail;
	GtkTreeIter iter;
	GList *selected;
	GtkTreeModel *model;
	ItemPerson *item;

	sel_detail = gtk_tree_view_get_selection(GTK_TREE_VIEW(detail_view));

	if(gtk_tree_selection_count_selected_rows(sel_detail) > 1)
		return FALSE;

	selected = gtk_tree_selection_get_selected_rows(sel_detail,&model);
	if(!selected)
		return FALSE;

	gtk_tree_model_get_iter(model, &iter, (GtkTreePath*)selected->data);
	g_list_foreach(selected, (GFunc)gtk_tree_path_free, NULL);
	g_list_free(selected);

	gtk_tree_model_get(model, &iter, COL_ITEM, &item,-1);
	if(ADDRITEM_ID(item) && editing_uid &&
	        strcmp(ADDRITEM_ID(item),editing_uid) == 0)
		return TRUE;
	else
		return FALSE;
}

static void detail_selection_changed(GtkTreeSelection *selection, gpointer data)
{
	gint num_selected;
	num_selected = gtk_tree_selection_count_selected_rows(selection);

	if(num_selected > 0)
		gtk_widget_set_sensitive(del_btn,TRUE);
	else
		gtk_widget_set_sensitive(del_btn,FALSE);

	if(num_selected == 1)
		gtk_widget_set_sensitive(edit_btn,TRUE);
	else
		gtk_widget_set_sensitive(edit_btn,FALSE);

	if(!is_editing_entry_only_selection())
		addressbook_edit_person_widgetset_hide();
}

static void email_selection_changed(GtkTreeSelection *selection, gpointer data)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	gchar *email;

	if(gtk_tree_selection_get_selected(selection, &model, &iter)) {
		GSList *hashval;
		GSList *walk;

		gtk_tree_model_get(model, &iter, 0, &email, -1);

		hashval = g_hash_table_lookup(addr_hash, email);
		gtk_list_store_clear(detail_store);
		for(walk = hashval; walk; walk = walk->next) {
			AddrDupListEntry *entry = walk->data;
			if(!entry)
				continue;
			gtk_list_store_append(detail_store, &iter);
			gtk_list_store_set(detail_store, &iter,
			                   COL_BOOKPATH, entry->book_path,
			                   COL_NAME, addressbook_set_col_name_guard(ADDRITEM_NAME(entry->person)),
			                   COL_ITEM, entry->person,
			                   COL_DS, entry->ds,
			                   -1);
		}
		g_free(email);
	}
}

static gchar* get_bookpath(ItemPerson *itemPerson, AddressDataSource *ds)
{
	gchar *path;
	gchar *tmp;
	AddrItemObject *item;

	item = (AddrItemObject*)itemPerson;
	path = g_strdup("");
	while((item = ADDRITEM_PARENT(item)) != NULL) {

		if(ADDRITEM_TYPE(item) == ITEMTYPE_FOLDER) {
			ItemFolder *folder = (ItemFolder*) item;
			tmp = path;
			path = g_strdup_printf("%s%s%s",
			                       folder->isRoot ? addrindex_ds_get_name(ds) :
			                       ADDRITEM_NAME(folder),
			                       (*tmp == '\0') ? "" : "/", tmp);
			g_free(tmp);
		}

	}

	/* prepend bookpath */
	if(ds && ds->interface && ds->interface->name) {
		tmp = path;
		path = g_strdup_printf("%s%s%s", ds->interface->name,
		                       (*tmp == '\0') ? "" : "/", tmp);
		g_free(tmp);
	}

	return path;
}

static void refresh_stores(gchar *email_to_select, GSList *detail_to_select)
{
	refresh_addr_hash();
	if(email_store)
		gtk_list_store_clear(email_store);
	if(detail_store)
		gtk_list_store_clear(detail_store);
	g_hash_table_foreach(addr_hash,append_to_email_store,email_store);

	/* sort the email store */
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(email_store),
	                                     0, GTK_SORT_ASCENDING);

	/* try to select email address */
	if(email_to_select) {
		/* Search email in email store */
		GtkTreeIter iter;
		GtkTreeSelection *selection;

		if(!gtk_tree_model_get_iter_first(GTK_TREE_MODEL(email_store), &iter))
			return;
		selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(email_view));

		do {
			gint retVal;
			gchar *email;

			gtk_tree_model_get(GTK_TREE_MODEL(email_store), &iter, 0, &email, -1);
			retVal = g_ascii_strncasecmp(email,email_to_select,strlen(email));
			g_free(email);
			if(retVal == 0) {
				gtk_tree_selection_select_iter(selection,&iter);
				break;
			}
		} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(email_store), &iter));

	}

	/* try to select detail rows */
	if(detail_to_select) {
		GtkTreeIter iter;
		GtkTreeSelection *sel;
		if(!gtk_tree_model_get_iter_first(GTK_TREE_MODEL(detail_store), &iter))
			return;
		sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(detail_view));

		do {
			GSList *walk;
			ItemPerson *person;
			gtk_tree_model_get(GTK_TREE_MODEL(detail_store), &iter,
			                   COL_ITEM, &person, -1);
			for(walk = detail_to_select; walk; walk = walk->next) {
				gchar *uid = walk->data;
				if(uid && ADDRITEM_ID(person) &&
				        (strcmp(uid,ADDRITEM_ID(person)) == 0))
					gtk_tree_selection_select_iter(sel,&iter);
			}
		} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(detail_store), &iter));
	}
}

static void detail_row_activated(GtkTreeView       *tree_view,
                                 GtkTreePath       *path,
                                 GtkTreeViewColumn *column,
                                 gpointer           user_data)
{
	GtkTreeIter iter;
	ItemPerson *person;
	AddressDataSource *ds;
	GtkTreeModel *model;
	AddressBookFile *abf;

	model = gtk_tree_view_get_model(tree_view);

	if(!gtk_tree_model_get_iter(model,&iter,path))
		return;

	gtk_tree_model_get(model, &iter, COL_ITEM, &person, COL_DS, &ds, -1);


	if(!((ds->type == ADDR_IF_BOOK) || ds->type == ADDR_IF_LDAP)) {
		debug_print("Unsupported address datasource type for editing\n");
		return;
	}

	abf = ds->rawDataSource;
	if(inline_edit_vbox)
		gtk_widget_show_all(inline_edit_vbox);
	if(editing_uid)
		g_free(editing_uid);
	editing_uid = g_strdup(ADDRITEM_ID(person));
	addressbook_edit_person(abf,NULL,person,FALSE,inline_edit_vbox,
	                        edit_post_update_cb,FALSE);
}

static void edit_post_update_cb(ItemPerson *item)
{
	GtkTreeSelection *sel;
	gchar *email;
	GList *detail_sel;
	GList *walk;
	GSList *detail;
	GtkTreeIter iter;
	GtkTreeModel *model;
	ItemPerson *person;

	/* save selection for after the update */

	/* email -> string of email address */
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(email_view));
	if(gtk_tree_selection_get_selected(sel,NULL,&iter))
		gtk_tree_model_get(GTK_TREE_MODEL(email_store), &iter, 0, &email, -1);
	else
		email = NULL;

	/* detail -> GSList of ItemPerson UIDs */
	detail = NULL;
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(detail_view));
	detail_sel = gtk_tree_selection_get_selected_rows(sel, &model);
	for(walk = detail_sel; walk; walk = walk->next) {
		GtkTreePath *path = walk->data;
		if(!gtk_tree_model_get_iter(model,&iter,path))
			continue;
		gtk_tree_model_get(model, &iter, COL_ITEM, &person,-1);
		detail = g_slist_prepend(detail, g_strdup(ADDRITEM_ID(person)));
	}
	g_list_foreach(detail_sel, (GFunc)gtk_tree_path_free, NULL);
	g_list_free(detail_sel);

	/* now refresh the stores, trying to keep the selections active */
	refresh_stores(email,detail);

	/* cleanup */
	if(email)
		g_free(email);
	g_slist_foreach(detail, (GFunc)g_free, NULL);
	g_slist_free(detail);
}

static void cb_edit_btn_clicked(GtkButton *button, gpointer data)
{
	GtkTreeSelection *selection;
	GList *selected;
	GtkTreeModel *model;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(detail_view));
	selected = gtk_tree_selection_get_selected_rows(selection,&model);
	cm_return_if_fail(selected);

	/* we are guaranteed to have exactly one row selected */
	gtk_tree_view_row_activated(GTK_TREE_VIEW(detail_view),(GtkTreePath*)selected->data,
	                            gtk_tree_view_get_column(GTK_TREE_VIEW(detail_view),0));

	g_list_foreach(selected, (GFunc)gtk_tree_path_free, NULL);
	g_list_free(selected);
}

static void cb_del_btn_clicked(GtkButton *button, gpointer data)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	GtkTreeSelection *selection;
	ItemPerson *item;
	AddressDataSource *ds;
	GList *list;
	GList *ref_list;
	GList *walk;
	GtkTreeRowReference *ref;
	AlertValue aval;
	GtkTreeSelection *sel;
	gchar *email;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(detail_view));

	list = gtk_tree_selection_get_selected_rows(selection, &model);

	if(!list)
		return;

	aval = alertpanel(_("Delete address(es)"),
	                  _("Really delete the address(es)?"),
	                  GTK_STOCK_CANCEL, "+"GTK_STOCK_DELETE, NULL);
	if(aval != G_ALERTALTERNATE)
		return;

	ref_list = NULL;
	for(walk = list; walk; walk = walk->next) {
		ref = gtk_tree_row_reference_new(model,(GtkTreePath*)(walk->data));
		ref_list = g_list_prepend(ref_list, ref);
	}
	g_list_foreach(list, (GFunc)gtk_tree_path_free, NULL);
	g_list_free(list);

	for(walk = ref_list; walk; walk = walk->next) {
		GtkTreePath *path;
		ref = walk->data;
		if(!gtk_tree_row_reference_valid(ref))
			continue;
		path = gtk_tree_row_reference_get_path(ref);
		if(gtk_tree_model_get_iter(model, &iter, path)) {
			gtk_tree_model_get(model, &iter, COL_ITEM, &item, COL_DS, &ds, -1);
			addrduplicates_delete_item_person(item,ds);
		}
		gtk_tree_path_free(path);
	}

	g_list_foreach(ref_list, (GFunc)gtk_tree_row_reference_free, NULL);
	g_list_free(ref_list);

	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(email_view));
	if(gtk_tree_selection_get_selected(sel,NULL,&iter))
		gtk_tree_model_get(GTK_TREE_MODEL(email_store), &iter, 0, &email, -1);
	else
		email = NULL;
	refresh_stores(email,NULL);
	if(email)
		g_free(email);
}

gboolean addrduplicates_delete_item_person(ItemPerson *item, AddressDataSource *ds)
{
	AddressBookFile *abf;
	AddressInterface *iface;
	if (!ds)
		return FALSE;
	/* Test for read only */
	iface = ds->interface;
	if( iface && iface->readOnly ) {
		alertpanel( _("Delete address"),
		            _("This address data is readonly and cannot be deleted."),
		            GTK_STOCK_CLOSE, NULL, NULL );
		return FALSE;
	}

	if(!(abf = ds->rawDataSource))
		return FALSE;

	item->status = DELETE_ENTRY;
	item = addrbook_remove_person(abf, item);

#ifdef USE_LDAP

	if (ds && ds->type == ADDR_IF_LDAP) {
		LdapServer *server = ds->rawDataSource;
		ldapsvr_set_modified(server, TRUE);
		ldapsvr_update_book(server, item);
	}

#endif

	if(item) {
		gchar *filename = addritem_person_get_picture(item);
		if (filename && is_file_exist(filename))
			claws_unlink(filename);
		g_free(filename);
		addritem_free_item_person(item);
	}
	return TRUE;
}

static gboolean cb_finder_results_dialog_key_pressed(GtkWidget *widget,
        GdkEventKey *event,
        gpointer data)
{
	if(event) {
		if(event->keyval == GDK_KEY_Delete && detail_view_has_focus)
			cb_del_btn_clicked(NULL,NULL);
		else if(event->keyval == GDK_KEY_Escape)
			gtk_widget_destroy(dialog);
	}

	return FALSE;
}

static gboolean detail_focus_in(GtkWidget *widget,
                                GdkEventFocus *event,gpointer data)
{
	detail_view_has_focus = TRUE;
	return FALSE;
}

static gboolean detail_focus_out(GtkWidget *widget,
                                 GdkEventFocus *event,gpointer data)
{
	detail_view_has_focus = FALSE;
	return FALSE;
}
