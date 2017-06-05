/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 *
 * Copyright (C) 2000-2011 by Alfons Hoogervorst & The Claws Mail Team.
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

/* We know this file uses some deprecated stuff. */
#undef G_DISABLE_DEPRECATED
#undef GTK_DISABLE_DEPRECATED
#undef GDK_DISABLE_DEPRECATED

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <string.h>
#include <ctype.h>
#if (HAVE_WCTYPE_H && HAVE_WCHAR_H)
#  include <wchar.h>
#  include <wctype.h>
#endif

#include "addrindex.h"
#include "addr_compl.h"
#include "utils.h"
#include "prefs_common.h"
#include "claws.h"
#include "hooks.h"
#include "gtkutils.h"
#include <pthread.h>

/*!
 *\brief	For the GtkListStore
 */
enum {
	ADDR_COMPL_ICON,
	ADDR_COMPL_ADDRESS,
	ADDR_COMPL_ISGROUP,
	ADDR_COMPL_GROUPLIST,
	N_ADDR_COMPL_COLUMNS
};

/*
 * How it works:
 *
 * The address book is read into memory. We set up an address list
 * containing all address book entries. Next we make the completion
 * list, which contains all the completable strings, and store a
 * reference to the address entry it belongs to.
 * After calling the g_completion_complete(), we get a reference
 * to a valid email address.  
 *
 * Completion is very simplified. We never complete on another prefix,
 * i.e. we neglect the next smallest possible prefix for the current
 * completion cache. This is simply done so we might break up the
 * addresses a little more (e.g. break up alfons@proteus.demon.nl into
 * something like alfons, proteus, demon, nl; and then completing on
 * any of those words).
 */

/**
 * completion_entry - structure used to complete addresses, with a reference
 * the the real address information.
 */
typedef struct
{
	gchar		*string; /* string to complete */
	address_entry	*ref;	 /* address the string belongs to  */
} completion_entry;

/*******************************************************************************/

static gint	    g_ref_count;	/* list ref count */
static GList 	   *g_completion_list = NULL;	/* list of strings to be checked */
static GList 	   *g_address_list = NULL;	/* address storage */
static GCompletion *g_completion;	/* completion object */

static GHashTable *_groupAddresses_ = NULL;
static gboolean _allowCommas_ = TRUE;

/* To allow for continuing completion we have to keep track of the state
 * using the following variables. No need to create a context object. */

static gint	    g_completion_count;		/* nr of addresses incl. the prefix */
static gint	    g_completion_next;		/* next prev address */
static GSList	   *g_completion_addresses;	/* unique addresses found in the
						   completion cache. */
static gchar	   *g_completion_prefix;	/* last prefix. (this is cached here
						 * because the prefix passed to g_completion
						 * is g_utf8_strdown()'ed */

static gchar *completion_folder_path = NULL;

/*******************************************************************************/

/*
 * Define the structure of the completion window.
 */
typedef struct _CompletionWindow CompletionWindow;
struct _CompletionWindow {
	gint      listCount;
	gchar     *searchTerm;
	GtkWidget *window;
	GtkWidget *entry;
	GtkWidget *list_view;

	gboolean   in_mouse;	/*!< mouse press pending... */
	gboolean   destroying;  /*!< destruction in progress */
};

static GtkListStore *addr_compl_create_store	(void);

static GtkWidget *addr_compl_list_view_create	(CompletionWindow *window);

static void addr_compl_create_list_view_columns	(GtkWidget *list_view);

static gboolean list_view_button_press		(GtkWidget *widget, 
						 GdkEventButton *event,
						 CompletionWindow *window);

static gboolean list_view_button_release	(GtkWidget *widget, 
						 GdkEventButton *event,
						 CompletionWindow *window);

static gboolean addr_compl_selected		(GtkTreeSelection *selector,
						 GtkTreeModel *model, 
						 GtkTreePath *path,
						 gboolean currently_selected,
						 gpointer data);
						 
static gboolean addr_compl_defer_select_destruct(CompletionWindow *window);

/**
 * Function used by GTK to find the string data to be used for completion.
 * \param data Pointer to data being processed.
 */
static gchar *completion_func(gpointer data)
{
	cm_return_val_if_fail(data != NULL, NULL);

	return ((completion_entry *)data)->string;
} 

/**
 * Initialize all completion index data.
 */
static void init_all(void)
{
	g_completion = g_completion_new(completion_func);
	cm_return_if_fail(g_completion != NULL);
}

static void free_all_addresses(void)
{
	GList *walk;
	if (!g_address_list)
		return;
	walk = g_address_list;
	for (; walk != NULL; walk = g_list_next(walk)) {
		address_entry *ae = (address_entry *) walk->data;
		g_free(ae->name);
		g_free(ae->address);
		g_list_free(ae->grp_emails);
		g_free(walk->data);
	}
	g_list_free(g_address_list);
	g_address_list = NULL;
	if (_groupAddresses_)
		g_hash_table_destroy(_groupAddresses_);
	_groupAddresses_ = NULL;
}

static void clear_completion_cache(void);
static void free_completion_list(void)
{
	GList *walk;
	if (!g_completion_list)
		return;
	
	clear_completion_cache();
	if (g_completion)
		g_completion_clear_items(g_completion);

	walk = g_list_first(g_completion_list);
	for (; walk != NULL; walk = g_list_next(walk)) {
		completion_entry *ce = (completion_entry *) walk->data;
		g_free(ce->string);
		g_free(walk->data);
	}
	g_list_free(g_completion_list);
	g_completion_list = NULL;
}
/**
 * Free up all completion index data.
 */
static void free_all(void)
{
	free_completion_list();	
	free_all_addresses();	
	g_completion_free(g_completion);
	g_completion = NULL;
}

/**
 * Append specified address entry to the index.
 * \param str Index string value.
 * \param ae  Entry containing address data.
 */
void addr_compl_add_address1(const char *str, address_entry *ae)
{
	completion_entry *ce1;
	ce1 = g_new0(completion_entry, 1),
	/* GCompletion list is case sensitive */
	ce1->string = g_utf8_strdown(str, -1);
	ce1->ref = ae;

	g_completion_list = g_list_prepend(g_completion_list, ce1);
}

/**
 * Adds address to the completion list. This function looks complicated, but
 * it's only allocation checks. Each value will be included in the index.
 * \param name    Recipient name.
 * \param address EMail address.
 * \param alias   Alias to append.
 * \param grp_emails the emails in case of a group. List should be freed later, 
 * but not its strings
 * \return <code>0</code> if entry appended successfully, or <code>-1</code>
 *         if failure.
 */
static gint add_address(const gchar *name, const gchar *address, 
			const gchar *nick, const gchar *alias, GList *grp_emails)
{
	address_entry    *ae;
	gboolean is_group = FALSE;

	if (!name || !address) {
		if (!address && !nick && !alias && grp_emails) {
			is_group = TRUE;
		} else
			return -1;
	}

	ae = g_new0(address_entry, 1);

	cm_return_val_if_fail(ae != NULL, -1);

	ae->name    = g_strdup(name);
	ae->address = g_strdup(address);
	ae->grp_emails = grp_emails;
	g_address_list = g_list_prepend(g_address_list, ae);

	addr_compl_add_address1(name, ae);
	if (address != NULL && *address != '\0')
		addr_compl_add_address1(address, ae);

	if (nick != NULL && *nick != '\0')
		addr_compl_add_address1(nick, ae);

	if ( alias != NULL && *alias != '\0') {
		addr_compl_add_address1(alias, ae);
	}

	return 0;
}

/**
 * Read address book, creating all entries in the completion index.
 */ 
static void read_address_book(gchar *folderpath) {
	free_all_addresses();
	free_completion_list();

	addrindex_load_completion( add_address, folderpath );

	/* plugins may hook in here to modify/extend the completion list */
	hooks_invoke(ADDDRESS_COMPLETION_BUILD_ADDRESS_LIST_HOOKLIST, &g_address_list);

	g_address_list = g_list_reverse(g_address_list);
	g_completion_list = g_list_reverse(g_completion_list);
	/* merge the completion entry list into g_completion */
	if (g_completion_list) {
		g_completion_add_items(g_completion, g_completion_list);
		if (debug_get_mode())
			debug_print("read %d items in %s\n",
				g_list_length(g_completion_list),
				folderpath?folderpath:"(null)");
	}
}

/**
 * Test whether there is a completion pending.
 * \return <code>TRUE</code> if pending.
 */
static gboolean is_completion_pending(void)
{
	/* check if completion pending, i.e. we might satisfy a request for the next
	 * or previous address */
	 return g_completion_count;
}

/**
 * Clear the completion cache.
 */
static void clear_completion_cache(void)
{
	if (is_completion_pending()) {
		g_free(g_completion_prefix);

		if (g_completion_addresses) {
			g_slist_free(g_completion_addresses);
			g_completion_addresses = NULL;
		}

		g_completion_count = g_completion_next = 0;
	}
}

/**
 * Prepare completion index. This function should be called prior to attempting
 * address completion.
 * \return The number of addresses in the completion list.
 */
gint start_address_completion(gchar *folderpath)
{
	gboolean different_book = FALSE;
	clear_completion_cache();

	if (strcmp2(completion_folder_path,folderpath))
		different_book = TRUE;

	g_free(completion_folder_path);
	if (folderpath != NULL)
		completion_folder_path = g_strdup(folderpath);
	else
		completion_folder_path = NULL;

	if (!g_ref_count) {
		init_all();
		/* open the address book */
		read_address_book(folderpath);
	} else if (different_book)
		read_address_book(folderpath);

	g_ref_count++;
	debug_print("start_address_completion(%s) ref count %d\n",
				folderpath?folderpath:"(null)", g_ref_count);

	return g_list_length(g_completion_list);
}

/**
 * Retrieve a possible address (or a part) from an entry box. To make life
 * easier, we only look at the last valid address component; address
 * completion only works at the last string component in the entry box.
 *
 * \param entry Address entry field.
 * \param start_pos Address of start position of address.
 * \return Possible address.
 */
static gchar *get_address_from_edit(GtkEntry *entry, gint *start_pos)
{
	const gchar *edit_text, *p;
	gint cur_pos;
	gboolean in_quote = FALSE;
	gboolean in_bracket = FALSE;
	gchar *str;

	edit_text = gtk_entry_get_text(entry);
	if (edit_text == NULL) return NULL;

	cur_pos = gtk_editable_get_position(GTK_EDITABLE(entry));

	/* scan for a separator. doesn't matter if walk points at null byte. */
	for (p = g_utf8_offset_to_pointer(edit_text, cur_pos);
	     p > edit_text;
	     p = g_utf8_prev_char(p)) {
		if (*p == '"') {
			in_quote = TRUE;
		} else if (!in_quote) {
			if (!in_bracket && *p == ',') {
				break;
			} else if (*p == '<')
				in_bracket = TRUE;
			else if (*p == '>')
				in_bracket = FALSE;
		}
	}

	/* have something valid */
	if (g_utf8_strlen(p, -1) == 0)
		return NULL;

#define IS_VALID_CHAR(x) \
	(g_ascii_isalnum(x) || (x) == '"' || (x) == '<' || (((unsigned char)(x)) > 0x7f))

	/* now scan back until we hit a valid character */
	for (; *p && !IS_VALID_CHAR(*p); p = g_utf8_next_char(p))
		;

#undef IS_VALID_CHAR

	if (g_utf8_strlen(p, -1) == 0)
		return NULL;

	if (start_pos) *start_pos = g_utf8_pointer_to_offset(edit_text, p);

	str = g_strdup(p);

	return str;
} 

static gchar *get_complete_address_from_name_email(const gchar *name, const gchar *email)
{
	gchar *address = NULL;
	if (!name || name[0] == '\0')
		address = g_strdup_printf("<%s>", email);
	else if (strchr_with_skip_quote(name, '"', ','))
		address = g_strdup_printf
			("\"%s\" <%s>", name, email);
	else
		address = g_strdup_printf
			("%s <%s>", name, email);
	return address;
}

/**
 * Replace an incompleted address with a completed one.
 * \param entry     Address entry field.
 * \param newtext   New text.
 * \param start_pos Insertion point in entry field.
 */
static void replace_address_in_edit(GtkEntry *entry, const gchar *newtext,
			     gint start_pos, gboolean is_group, GList *grp_emails)
{
	if (!newtext) return;
	gtk_editable_delete_text(GTK_EDITABLE(entry), start_pos, -1);
	if (!is_group) {
		gtk_editable_insert_text(GTK_EDITABLE(entry), newtext, strlen(newtext),
				 &start_pos);
	} else {
		gchar *addresses = NULL;
		GList *cur = grp_emails;
		for (; cur; cur = cur->next) {
			gchar *tmp;
			ItemEMail *email = (ItemEMail *)cur->data;
			ItemPerson *person = ( ItemPerson * ) ADDRITEM_PARENT(email);
			
			gchar *addr = get_complete_address_from_name_email(
				ADDRITEM_NAME(person), email->address);
			if (addresses)
				tmp = g_strdup_printf("%s, %s", addresses, addr);
			else
				tmp = g_strdup_printf("%s", addr);
			g_free(addr);
			g_free(addresses);
			addresses = tmp;
		}
		gtk_editable_insert_text(GTK_EDITABLE(entry), addresses, strlen(addresses),
				 &start_pos);
		g_free(addresses);
	}
	gtk_editable_set_position(GTK_EDITABLE(entry), -1);
}

/**
 * Attempt to complete an address, and returns the number of addresses found.
 * Use <code>get_complete_address()</code> to get an entry from the index.
 *
 * \param  str Search string to find.
 * \return Zero if no match was found, otherwise the number of addresses; the
 *         original prefix (search string) will appear at index 0. 
 */
guint complete_address(const gchar *str)
{
	GList *result = NULL;
	gchar *d = NULL;
	guint  count = 0;
	guint  cpl = 0;
	completion_entry *ce = NULL;

	cm_return_val_if_fail(str != NULL, 0);

	/* g_completion is case sensitive */
	d = g_utf8_strdown(str, -1);

	clear_completion_cache();
	g_completion_prefix = g_strdup(str);

	result = g_completion_complete(g_completion, d, NULL);

	count = g_list_length(result);
	if (count) {
		/* create list with unique addresses  */
		for (cpl = 0, result = g_list_first(result);
		     result != NULL;
		     result = g_list_next(result)) {
			ce = (completion_entry *)(result->data);
			if (NULL == g_slist_find(g_completion_addresses,
						 ce->ref)) {
				cpl++;
				g_completion_addresses =
					g_slist_append(g_completion_addresses,
						       ce->ref);
			}
		}
		count = cpl + 1;	/* index 0 is the original prefix */
		g_completion_next = 1;	/* we start at the first completed one */
	} else {
		g_free(g_completion_prefix);
		g_completion_prefix = NULL;
	}

	g_completion_count = count;

	g_free(d);

	return count;
}

/**
 * complete_matches_found() returns the number of matched addresses according
 * to the completion mechanism. Unlike complete_address(), the returned value
 * doesn't count str itself. If there's no match, it returns 0.
 * To get a list of completion matches, see complete_address() instead.
 */
guint complete_matches_found(const gchar *str)
{
	GList *result = NULL;
	gchar *d = NULL;

	cm_return_val_if_fail(str != NULL, 0);

	/* g_completion is case sensitive */
	d = g_utf8_strdown(str, -1);

	clear_completion_cache();
	g_completion_prefix = g_strdup(str);

	result = g_completion_complete(g_completion, d, NULL);

	g_free(g_completion_prefix);
	g_free(d);

	return g_list_length(result);
}

/**
 * Return a complete address from the index.
 * \param index Index of entry that was found (by the previous call to
 *              <code>complete_address()</code>
 * \return Completed address string; this should be freed when done.
 */
gchar *get_complete_address(gint index)
{
	const address_entry *p;
	gchar *address = NULL;

	if (index < g_completion_count) {
		if (index == 0)
			address = g_strdup(g_completion_prefix);
		else {
			/* get something from the unique addresses */
			p = (address_entry *)g_slist_nth_data
				(g_completion_addresses, index - 1);
			if (p != NULL && p->address != NULL) {
				address = get_complete_address_from_name_email(p->name, p->address);
			} else if (p != NULL && p->address == NULL && p->name != NULL) {
				/* that's a group */
				address = g_strdup_printf("%s (%s) <!--___group___-->", p->name, _("Group"));
				if (!_groupAddresses_) {
					_groupAddresses_ = g_hash_table_new(NULL, g_direct_equal);
				}
				if (!g_hash_table_lookup(_groupAddresses_, GINT_TO_POINTER(g_str_hash(address)))) {
					g_hash_table_insert(_groupAddresses_, GINT_TO_POINTER(g_str_hash(address)), p->grp_emails);

				}
			}
		}
	}

	return address;
}

/**
 * Return the next complete address match from the completion index.
 * \return Completed address string; this should be freed when done.
 */
static gchar *get_next_complete_address(void)
{
	if (is_completion_pending()) {
		gchar *res;

		res = get_complete_address(g_completion_next);
		g_completion_next += 1;
		if (g_completion_next >= g_completion_count)
			g_completion_next = 0;

		return res;
	} else
		return NULL;
}

/**
 * Return a count of the completed matches in the completion index.
 * \return Number of matched entries.
 */
static guint get_completion_count(void)
{
	if (is_completion_pending())
		return g_completion_count;
	else
		return 0;
}

/**
 * Invalidate address completion index. This function should be called whenever
 * the address book changes. This forces data to be read into the completion
 * data.
 * \return Number of entries in index.
 */
gint invalidate_address_completion(void)
{
	if (g_ref_count) {
		/* simply the same as start_address_completion() */
		debug_print("Invalidation request for address completion\n");
		read_address_book(completion_folder_path);
		clear_completion_cache();
	}

	return g_list_length(g_completion_list);
}

/**
 * Finished with completion index. This function should be called after
 * matching addresses.
 * \return Reference count.
 */
gint end_address_completion(void)
{
	gboolean different_folder = FALSE;
	clear_completion_cache();

	/* reset the folderpath to NULL */
	if (completion_folder_path) {
		g_free(completion_folder_path);
		completion_folder_path = NULL;
		different_folder = TRUE;
	}
	if (0 == --g_ref_count)
		free_all();

	debug_print("end_address_completion ref count %d\n", g_ref_count);
	if (g_ref_count && different_folder) {
		debug_print("still ref'd, different folder\n");
		invalidate_address_completion();
	}

	return g_ref_count; 
}

/**
 * Completion window.
 */
static CompletionWindow *_compWindow_ = NULL;

/**
 * Mutex to protect callback from multiple threads.
 */
static pthread_mutex_t _completionMutex_ = PTHREAD_MUTEX_INITIALIZER;

/**
 * Completion queue list.
 */
static GList *_displayQueue_ = NULL;
/**
 * Current query ID.
 */
static gint _queryID_ = 0;

/**
 * Completion idle ID.
 */
static guint _completionIdleID_ = 0;

/*
 * address completion entry ui. the ui (completion list was inspired by galeon's
 * auto completion list). remaining things powered by claws's completion engine.
 */

#define ENTRY_DATA_TAB_HOOK	"tab_hook"	/* used to lookup entry */
#define ENTRY_DATA_ALLOW_COMMAS	"allowcommas"	/* used to know whether to present groups */

static void address_completion_mainwindow_set_focus	(GtkWindow   *window,
							 GtkWidget   *widget,
							 gpointer     data);
static gboolean address_completion_entry_key_pressed	(GtkEntry    *entry,
							 GdkEventKey *ev,
							 gpointer     data);
static gboolean address_completion_complete_address_in_entry
							(GtkEntry    *entry,
							 gboolean     next);
static void address_completion_create_completion_window	(GtkEntry    *entry);

static gboolean completion_window_button_press
					(GtkWidget	 *widget,
					 GdkEventButton  *event,
					 CompletionWindow *compWin );

static gboolean completion_window_key_press
					(GtkWidget	 *widget,
					 GdkEventKey	 *event,
					 CompletionWindow *compWin );
static void address_completion_create_completion_window( GtkEntry *entry_ );

/**
 * Create a completion window object.
 * \return Initialized completion window.
 */
static CompletionWindow *addrcompl_create_window( void ) {
	CompletionWindow *cw;

	cw = g_new0( CompletionWindow, 1 );
	cw->listCount = 0;
	cw->searchTerm = NULL;
	cw->window = NULL;
	cw->entry = NULL;
	cw->list_view = NULL;
	cw->in_mouse = FALSE;
	cw->destroying = FALSE;

	return cw;	
}

/**
 * Destroy completion window.
 * \param cw Window to destroy.
 */
static void addrcompl_destroy_window( CompletionWindow *cw ) {
	/* Stop all searches currently in progress */
	addrindex_stop_search( _queryID_ );

	/* Remove idler function... or application may not terminate */
	if( _completionIdleID_ != 0 ) {
		g_source_remove( _completionIdleID_ );
		_completionIdleID_ = 0;
	}

	/* Now destroy window */	
	if( cw ) {
		/* Clear references to widgets */
		cw->entry = NULL;
		cw->list_view = NULL;

		/* Free objects */
		if( cw->window ) {
			gtk_widget_hide( cw->window );
			gtk_widget_destroy( cw->window );
		}
		cw->window = NULL;
		cw->destroying = FALSE;
		cw->in_mouse = FALSE;
	}
	
}

/**
 * Free up completion window.
 * \param cw Window to free.
 */
static void addrcompl_free_window( CompletionWindow *cw ) {
	if( cw ) {
		addrcompl_destroy_window( cw );

		g_free( cw->searchTerm );
		cw->searchTerm = NULL;

		/* Clear references */		
		cw->listCount = 0;

		/* Free object */		
		g_free( cw );
	}
}

/**
 * Advance selection to previous/next item in list.
 * \param list_view List to process.
 * \param forward Set to <i>TRUE</i> to select next or <i>FALSE</i> for
 *                previous entry.
 */
static void completion_window_advance_selection(GtkTreeView *list_view, gboolean forward)
{
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	GtkTreeModel *model;

	cm_return_if_fail(list_view != NULL);

	selection = gtk_tree_view_get_selection(list_view);
	if (!gtk_tree_selection_get_selected(selection, &model, &iter))
		return;

	if (forward) { 
		forward = gtk_tree_model_iter_next(model, &iter);
		if (forward) 
			gtk_tree_selection_select_iter(selection, &iter);
	} else {
		GtkTreePath *prev;

		prev = gtk_tree_model_get_path(model, &iter);
		if (!prev) 
			return;

		if (gtk_tree_path_prev(prev))
			gtk_tree_selection_select_path(selection, prev);
		
		gtk_tree_path_free(prev);
	}
}

/**
 * Resize window to accommodate maximum number of address entries.
 * \param cw Completion window.
 */
static void addrcompl_resize_window( CompletionWindow *cw ) {
	GtkRequisition r;
	gint x, y, width, height, depth;

	/* Get current geometry of window */
	gdk_window_get_geometry( cw->window->window, &x, &y, &width, &height, &depth );

	gtk_widget_hide_all( cw->window );
	gtk_widget_show_all( cw->window );
	gtk_widget_size_request( cw->list_view, &r );

	/* Adjust window height to available screen space */
	if( ( y + r.height ) > gdk_screen_height() ) {
		gtk_window_set_resizable(GTK_WINDOW(cw->window), FALSE);
		gtk_widget_set_size_request( cw->window, width, gdk_screen_height() - y );
	} else
		gtk_widget_set_size_request(cw->window, width, r.height);
}

static GdkPixbuf *group_pixbuf = NULL;
static GdkPixbuf *email_pixbuf = NULL;

/**
 * Add an address the completion window address list.
 * \param cw      Completion window.
 * \param address Address to add.
 */
static void addrcompl_add_entry( CompletionWindow *cw, gchar *address ) {
	GtkListStore *store;
	GtkTreeIter iter;
	GtkTreeSelection *selection;
	gboolean is_group = FALSE;
	GList *grp_emails = NULL;
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(cw->list_view)));
	GdkPixbuf *pixbuf;
	
	if (!group_pixbuf) {
		stock_pixbuf_gdk(cw->list_view, STOCK_PIXMAP_ADDR_TWO, &group_pixbuf);
		g_object_ref(G_OBJECT(group_pixbuf));
	}
	if (!email_pixbuf) {
		stock_pixbuf_gdk(cw->list_view, STOCK_PIXMAP_ADDR_ONE, &email_pixbuf);
		g_object_ref(G_OBJECT(email_pixbuf));
	}
	/* g_print( "\t\tAdding :%s\n", address ); */
	if (strstr(address, " <!--___group___-->")) {
		is_group = TRUE;
		if (_groupAddresses_)
			grp_emails = g_hash_table_lookup(_groupAddresses_, GINT_TO_POINTER(g_str_hash(address)));
		*(strstr(address, " <!--___group___-->")) = '\0';
		pixbuf = group_pixbuf;
	} else if (strchr(address, '@') && strchr(address, '<') &&
		   strchr(address, '>')) {
		pixbuf = email_pixbuf;
	} else
		pixbuf = NULL;
	
	if (is_group && !_allowCommas_)
		return;
	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter, 
				ADDR_COMPL_ICON, pixbuf,
				ADDR_COMPL_ADDRESS, address, 
				ADDR_COMPL_ISGROUP, is_group, 
				ADDR_COMPL_GROUPLIST, grp_emails,
				-1);
	cw->listCount++;

	/* Resize window */
	addrcompl_resize_window( cw );
	gtk_grab_add( cw->window );

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(cw->list_view));
	gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store), &iter);

	if( cw->listCount == 1 ) {
		/* Select first row for now */
		gtk_tree_selection_select_iter(selection, &iter);
	}
#ifndef GENERIC_UMPC
	else if( cw->listCount == 2 ) {
		gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &iter);
		/* Move off first row */
		gtk_tree_selection_select_iter(selection, &iter);
	}
#endif
}

/**
 * Completion idle function. This function is called by the main (UI) thread
 * during UI idle time while an address search is in progress. Items from the
 * display queue are processed and appended to the address list.
 *
 * \param data Target completion window to receive email addresses.
 * \return <i>TRUE</i> to ensure that idle event do not get ignored.
 */
static gboolean addrcompl_idle( gpointer data ) {
	GList *node;
	gchar *address;

	/* Process all entries in display queue */
	pthread_mutex_lock( & _completionMutex_ );
	if( _displayQueue_ ) {
		node = _displayQueue_;
		while( node ) {
			address = node->data;
			/* g_print( "address ::: %s :::\n", address ); */
			addrcompl_add_entry( _compWindow_, address );
			g_free( address );
			node = g_list_next( node );
		}
		g_list_free( _displayQueue_ );
		_displayQueue_ = NULL;
	}
	pthread_mutex_unlock( & _completionMutex_ );
	claws_do_idle();

	return TRUE;
}

/**
 * Callback entry point. The background thread (if any) appends the address
 * list to the display queue.
 * \param sender     Sender of query.
 * \param queryID    Query ID of search request.
 * \param listEMail  List of zero of more email objects that met search
 *                   criteria.
 * \param data       Query data.
 */
static gint addrcompl_callback_entry(
	gpointer sender, gint queryID, GList *listEMail, gpointer data )
{
	GList *node;
	gchar *address;

	/* g_print( "addrcompl_callback_entry::queryID=%d\n", queryID ); */
	pthread_mutex_lock( & _completionMutex_ );
	if( queryID == _queryID_ ) {
		/* Append contents to end of display queue */
		node = listEMail;
		while( node ) {
			ItemEMail *email = node->data;

			address = addritem_format_email( email );
			/* g_print( "\temail/address ::%s::\n", address ); */
			_displayQueue_ = g_list_append( _displayQueue_, address );
			node = g_list_next( node );
		}
	}
	g_list_free( listEMail );
	pthread_mutex_unlock( & _completionMutex_ );

	return 0;
}

/**
 * Clear the display queue.
 */
static void addrcompl_clear_queue( void ) {
	/* Clear out display queue */
	pthread_mutex_lock( & _completionMutex_ );

	g_list_free( _displayQueue_ );
	_displayQueue_ = NULL;

	pthread_mutex_unlock( & _completionMutex_ );
}

/**
 * Add a single address entry into the display queue.
 * \param address Address to append.
 */
static void addrcompl_add_queue( gchar *address ) {
	pthread_mutex_lock( & _completionMutex_ );
	_displayQueue_ = g_list_append( _displayQueue_, address );
	pthread_mutex_unlock( & _completionMutex_ );
}

/**
 * Load list with entries from local completion index.
 */
static void addrcompl_load_local( void ) {
	guint count = 0;

	for (count = 0; count < get_completion_count(); count++) {
		gchar *address;

		address = get_complete_address( count );
		/* g_print( "\taddress ::%s::\n", address ); */

		/* Append contents to end of display queue */
		addrcompl_add_queue( address );
	}
}

/**
 * Start the search.
 */
static void addrcompl_start_search( void ) {
	gchar *searchTerm;

	searchTerm = g_strdup( _compWindow_->searchTerm );

	/* Setup the search */
	_queryID_ = addrindex_setup_search(
		searchTerm, NULL, addrcompl_callback_entry );
	g_free( searchTerm );
	/* g_print( "addrcompl_start_search::queryID=%d\n", _queryID_ ); */

	/* Load local stuff */
	addrcompl_load_local();

	/* Sit back and wait until something happens */
	_completionIdleID_ =
		g_idle_add( (GSourceFunc) addrcompl_idle, NULL );
	/* g_print( "addrindex_start_search::queryID=%d\n", _queryID_ ); */

	addrindex_start_search( _queryID_ );
}

/**
 * Apply the current selection in the list to the entry field. Focus is also
 * moved to the next widget so that Tab key works correctly.
 * \param list_view List to process.
 * \param entry Address entry field.
 * \param move_focus Move focus to the next widget ?
 */
static void completion_window_apply_selection(GtkTreeView *list_view,
						GtkEntry *entry,
						gboolean move_focus)
{
	gchar *address = NULL, *text = NULL;
	gint   cursor_pos;
	GtkWidget *parent;
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gboolean is_group = FALSE;
	cm_return_if_fail(list_view != NULL);
	cm_return_if_fail(entry != NULL);
	GList *grp_emails = NULL;

	selection = gtk_tree_view_get_selection(list_view);
	if (! gtk_tree_selection_get_selected(selection, &model, &iter))
		return;

	/* First remove the idler */
	if( _completionIdleID_ != 0 ) {
		g_source_remove( _completionIdleID_ );
		_completionIdleID_ = 0;
	}

	/* Process selected item */
	gtk_tree_model_get(model, &iter, ADDR_COMPL_ADDRESS, &text, 
				ADDR_COMPL_ISGROUP, &is_group, 
				ADDR_COMPL_GROUPLIST, &grp_emails,
				-1);

	address = get_address_from_edit(entry, &cursor_pos);
	g_free(address);
	replace_address_in_edit(entry, text, cursor_pos, is_group, grp_emails);
	g_free(text);

	/* Move focus to next widget */
	parent = GTK_WIDGET(entry)->parent;
	if( parent && move_focus) {
		gtk_widget_child_focus( parent, GTK_DIR_TAB_FORWARD );
	}
}

/**
 * Start address completion. Should be called when creating the main window
 * containing address completion entries.
 * \param mainwindow Main window.
 */
void address_completion_start(GtkWidget *mainwindow)
{
	start_address_completion(NULL);

	/* register focus change hook */
	g_signal_connect(G_OBJECT(mainwindow), "set_focus",
			 G_CALLBACK(address_completion_mainwindow_set_focus),
			 mainwindow);
}

/**
 * Need unique data to make unregistering signal handler possible for the auto
 * completed entry.
 */
#define COMPLETION_UNIQUE_DATA (GINT_TO_POINTER(0xfeefaa))

/**
 * Register specified entry widget for address completion.
 * \param entry Address entry field.
 */
void address_completion_register_entry(GtkEntry *entry, gboolean allow_commas)
{
	cm_return_if_fail(entry != NULL);
	cm_return_if_fail(GTK_IS_ENTRY(entry));

	/* add hooked property */
	g_object_set_data(G_OBJECT(entry), ENTRY_DATA_TAB_HOOK, entry);
	g_object_set_data(G_OBJECT(entry), ENTRY_DATA_ALLOW_COMMAS, GINT_TO_POINTER(allow_commas));

	/* add keypress event */
	g_signal_connect_closure
		(G_OBJECT(entry), "key_press_event",
		 g_cclosure_new(G_CALLBACK(address_completion_entry_key_pressed),
				COMPLETION_UNIQUE_DATA,
				NULL),
		 FALSE); /* magic */
}

/**
 * Unregister specified entry widget from address completion operations.
 * \param entry Address entry field.
 */
void address_completion_unregister_entry(GtkEntry *entry)
{
	GtkObject *entry_obj;

	cm_return_if_fail(entry != NULL);
	cm_return_if_fail(GTK_IS_ENTRY(entry));

	entry_obj = g_object_get_data(G_OBJECT(entry), ENTRY_DATA_TAB_HOOK);
	cm_return_if_fail(entry_obj);
	cm_return_if_fail(G_OBJECT(entry_obj) == G_OBJECT(entry));

	/* has the hooked property? */
	g_object_set_data(G_OBJECT(entry), ENTRY_DATA_TAB_HOOK, NULL);

	/* remove the hook */
	g_signal_handlers_disconnect_by_func(G_OBJECT(entry), 
			G_CALLBACK(address_completion_entry_key_pressed),
			COMPLETION_UNIQUE_DATA);
}

/**
 * End address completion. Should be called when main window with address
 * completion entries terminates. NOTE: this function assumes that it is
 * called upon destruction of the window.
 * \param mainwindow Main window.
 */
void address_completion_end(GtkWidget *mainwindow)
{
	/* if address_completion_end() is really called on closing the window,
	 * we don't need to unregister the set_focus_cb */
	end_address_completion();
}

/* if focus changes to another entry, then clear completion cache */
static void address_completion_mainwindow_set_focus(GtkWindow *window,
						    GtkWidget *widget,
						    gpointer   data)
{
	
	if (widget && GTK_IS_ENTRY(widget) &&
	    g_object_get_data(G_OBJECT(widget), ENTRY_DATA_TAB_HOOK)) {
		_allowCommas_ = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), ENTRY_DATA_ALLOW_COMMAS));
		clear_completion_cache();
	}
}

/**
 * Listener that watches for tab or other keystroke in address entry field.
 * \param entry Address entry field.
 * \param ev    Event object.
 * \param data  User data.
 * \return <i>TRUE</i>.
 */
static gboolean address_completion_entry_key_pressed(GtkEntry    *entry,
						     GdkEventKey *ev,
						     gpointer     data)
{
	if (ev->keyval == GDK_Tab) {
		addrcompl_clear_queue();
		_allowCommas_ = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(entry), ENTRY_DATA_ALLOW_COMMAS));
		if( address_completion_complete_address_in_entry( entry, TRUE ) ) {
			/* route a void character to the default handler */
			/* this is a dirty hack; we're actually changing a key
			 * reported by the system. */
			ev->keyval = GDK_AudibleBell_Enable;
			ev->state &= ~GDK_SHIFT_MASK;

			/* Create window */			
			address_completion_create_completion_window(entry);

			/* Start remote queries */
			addrcompl_start_search();

			return TRUE;
		}
		else {
			/* old behaviour */
		}
	} else if (ev->keyval == GDK_Shift_L
		|| ev->keyval == GDK_Shift_R
		|| ev->keyval == GDK_Control_L
		|| ev->keyval == GDK_Control_R
		|| ev->keyval == GDK_Caps_Lock
		|| ev->keyval == GDK_Shift_Lock
		|| ev->keyval == GDK_Meta_L
		|| ev->keyval == GDK_Meta_R
		|| ev->keyval == GDK_Alt_L
		|| ev->keyval == GDK_Alt_R) {
		/* these buttons should not clear the cache... */
	} else
		clear_completion_cache();

	return FALSE;
}
/**
 * Initialize search term for address completion.
 * \param entry Address entry field.
 */
static gboolean address_completion_complete_address_in_entry(GtkEntry *entry,
							     gboolean  next)
{
	gint ncount, cursor_pos;
	gchar *searchTerm, *new = NULL;

	cm_return_val_if_fail(entry != NULL, FALSE);

	if (!gtkut_widget_has_focus(GTK_WIDGET(entry))) return FALSE;

	/* get an address component from the cursor */
	searchTerm = get_address_from_edit( entry, &cursor_pos );
	if( ! searchTerm ) return FALSE;
	/* g_print( "search for :::%s:::\n", searchTerm ); */

	/* Clear any existing search */
	g_free( _compWindow_->searchTerm );
	_compWindow_->searchTerm = g_strdup( searchTerm );

	/* Perform search on local completion index */
	ncount = complete_address( searchTerm );
	if( 0 < ncount ) {
		new = get_next_complete_address();
		g_free( new );
	}
#if (!defined(USE_LDAP) && !defined(GENERIC_UMPC))
	/* Select the address if there is only one match */
	if (ncount == 2) {
		/* Display selected address in entry field */		
		gchar *addr = get_complete_address(1);
		if (addr && !strstr(addr, " <!--___group___-->")) {
			replace_address_in_edit(entry, addr, cursor_pos, FALSE, NULL);
			/* Discard the window */
			clear_completion_cache();
		} 
		g_free(addr);
	}
	/* Make sure that drop-down appears uniform! */
	else 
#endif
	if( ncount == 0 ) {
		addrcompl_add_queue( g_strdup( searchTerm ) );
	}
	g_free( searchTerm );

	return TRUE;
}

/**
 * Create new address completion window for specified entry.
 * \param entry_ Entry widget to associate with window.
 */
static void address_completion_create_completion_window( GtkEntry *entry_ )
{
	gint x, y, height, width, depth;
	GtkWidget *scroll, *list_view;
	GtkRequisition r;
	GtkWidget *window;
	GtkWidget *entry = GTK_WIDGET(entry_);

	/* Create new window and list */
	window = gtk_window_new(GTK_WINDOW_POPUP);
	list_view  = addr_compl_list_view_create(_compWindow_);

	/* Destroy any existing window */
	addrcompl_destroy_window( _compWindow_ );

	/* Create new object */
	_compWindow_->window    = window;
	_compWindow_->entry     = entry;
	_compWindow_->list_view = list_view;
	_compWindow_->listCount = 0;
	_compWindow_->in_mouse  = FALSE;

	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(window), scroll);
	gtk_container_add(GTK_CONTAINER(scroll), list_view);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll),
		GTK_SHADOW_OUT);
	/* Use entry widget to create initial window */
	gdk_window_get_geometry(entry->window, &x, &y, &width, &height, &depth);
	gdk_window_get_origin (entry->window, &x, &y);
	y += height;
	gtk_window_move(GTK_WINDOW(window), x, y);

	/* Resize window to fit initial (empty) address list */
	gtk_widget_size_request( list_view, &r );
	gtk_widget_set_size_request( window, width, r.height );
	gtk_widget_show_all( window );
	gtk_widget_size_request( list_view, &r );

	/* Setup handlers */
	g_signal_connect(G_OBJECT(list_view), "button_press_event",
			 G_CALLBACK(list_view_button_press),
			 _compWindow_);
			 
	g_signal_connect(G_OBJECT(list_view), "button_release_event",
			 G_CALLBACK(list_view_button_release),
			 _compWindow_);
	
	g_signal_connect(G_OBJECT(window),
			 "button-press-event",
			 G_CALLBACK(completion_window_button_press),
			 _compWindow_ );
	g_signal_connect(G_OBJECT(window),
			 "key-press-event",
			 G_CALLBACK(completion_window_key_press),
			 _compWindow_ );
	gdk_pointer_grab(window->window, TRUE,
			 GDK_POINTER_MOTION_MASK | GDK_BUTTON_PRESS_MASK |
			 GDK_BUTTON_RELEASE_MASK,
			 NULL, NULL, GDK_CURRENT_TIME);
	gtk_grab_add( window );

	/* XXX: GTK2 too??? 
	 *
	 * GTK1: this gets rid of the irritating focus rectangle that doesn't
	 * follow the selection */
	gtkut_widget_set_can_focus(list_view, FALSE);
}

/**
 * Respond to button press in completion window. Check if mouse click is
 * anywhere outside the completion window. In that case the completion
 * window is destroyed, and the original searchTerm is restored.
 *
 * \param widget   Window object.
 * \param event    Event.
 * \param compWin  Reference to completion window.
 */
static gboolean completion_window_button_press(GtkWidget *widget,
					       GdkEventButton *event,
					       CompletionWindow *compWin )
{
	GtkWidget *event_widget, *entry;
	gchar *searchTerm;
	gint cursor_pos;
	gboolean restore = TRUE;

	cm_return_val_if_fail(compWin != NULL, FALSE);

	entry = compWin->entry;
	cm_return_val_if_fail(entry != NULL, FALSE);

	/* Test where mouse was clicked */
	event_widget = gtk_get_event_widget((GdkEvent *)event);
	if (event_widget != widget) {
		while (event_widget) {
			if (event_widget == widget)
				return FALSE;
			else if (event_widget == entry) {
				restore = FALSE;
				break;
			}
			event_widget = event_widget->parent;
		}
	}

	if (restore) {
		/* Clicked outside of completion window - restore */
		searchTerm = _compWindow_->searchTerm;
		g_free(get_address_from_edit(GTK_ENTRY(entry), &cursor_pos));
		replace_address_in_edit(GTK_ENTRY(entry), searchTerm, cursor_pos, FALSE, NULL);
	}

	clear_completion_cache();
	addrcompl_destroy_window( _compWindow_ );

	return TRUE;
}

/**
 * Respond to key press in completion window.
 * \param widget   Window object.
 * \param event    Event.
 * \param compWind Reference to completion window.
 */
static gboolean completion_window_key_press(GtkWidget *widget,
					    GdkEventKey *event,
					    CompletionWindow *compWin )
{
	GdkEventKey tmp_event;
	GtkWidget *entry;
	gchar *searchTerm;
	gint cursor_pos;
	GtkWidget *list_view;
	GtkWidget *parent;
	cm_return_val_if_fail(compWin != NULL, FALSE);

	entry = compWin->entry;
	list_view = compWin->list_view;
	cm_return_val_if_fail(entry != NULL, FALSE);

	/* allow keyboard navigation in the alternatives tree view */
	if (event->keyval == GDK_Up || event->keyval == GDK_Down ||
	    event->keyval == GDK_Page_Up || event->keyval == GDK_Page_Down) {
		completion_window_advance_selection
			(GTK_TREE_VIEW(list_view),
			 event->keyval == GDK_Down ||
			 event->keyval == GDK_Page_Down ? TRUE : FALSE);
		return FALSE;
	}		

	/* make tab move to next field */
	if( event->keyval == GDK_Tab ) {
		/* Reference to parent */
		parent = GTK_WIDGET(entry)->parent;

		/* Discard the window */
		clear_completion_cache();
		addrcompl_destroy_window( _compWindow_ );

		/* Move focus to next widget */
		if( parent ) {
			gtk_widget_child_focus( parent, GTK_DIR_TAB_FORWARD );
		}
		return FALSE;
	}

	/* make backtab move to previous field */
	if( event->keyval == GDK_ISO_Left_Tab ) {
		/* Reference to parent */
		parent = GTK_WIDGET(entry)->parent;

		/* Discard the window */
		clear_completion_cache();
		addrcompl_destroy_window( _compWindow_ );

		/* Move focus to previous widget */
		if( parent ) {
			gtk_widget_child_focus( parent, GTK_DIR_TAB_BACKWARD );
		}
		return FALSE;
	}
	_allowCommas_ = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(entry), ENTRY_DATA_ALLOW_COMMAS));

	/* look for presses that accept the selection */
	if (event->keyval == GDK_Return || event->keyval == GDK_space ||
			event->keyval == GDK_KP_Enter ||
			(_allowCommas_ && event->keyval == GDK_comma)) {
		/* User selected address with a key press */

		/* Display selected address in entry field */		
		completion_window_apply_selection(
			GTK_TREE_VIEW(list_view), GTK_ENTRY(entry),
			event->keyval != GDK_comma);

		if (event->keyval == GDK_comma) {
			gint pos = gtk_editable_get_position(GTK_EDITABLE(entry));
			gtk_editable_insert_text(GTK_EDITABLE(entry), ", ", 2, &pos);
			gtk_editable_set_position(GTK_EDITABLE(entry), pos + 1);
		}

		/* Discard the window */
		clear_completion_cache();
		addrcompl_destroy_window( _compWindow_ );
		return FALSE;
	}

	/* key state keys should never be handled */
	if (event->keyval == GDK_Shift_L
		 || event->keyval == GDK_Shift_R
		 || event->keyval == GDK_Control_L
		 || event->keyval == GDK_Control_R
		 || event->keyval == GDK_Caps_Lock
		 || event->keyval == GDK_Shift_Lock
		 || event->keyval == GDK_Meta_L
		 || event->keyval == GDK_Meta_R
		 || event->keyval == GDK_Alt_L
		 || event->keyval == GDK_Alt_R) {
		return FALSE;
	}

	/* some other key, let's restore the searchTerm (orignal text) */
	searchTerm = _compWindow_->searchTerm;
	g_free(get_address_from_edit(GTK_ENTRY(entry), &cursor_pos));
	replace_address_in_edit(GTK_ENTRY(entry), searchTerm, cursor_pos, FALSE, NULL);

	/* make sure anything we typed comes in the edit box */
	tmp_event.type       = event->type;
	tmp_event.window     = entry->window;
	tmp_event.send_event = TRUE;
	tmp_event.time       = event->time;
	tmp_event.state      = event->state;
	tmp_event.keyval     = event->keyval;
	tmp_event.length     = event->length;
	tmp_event.string     = event->string;
	gtk_widget_event(entry, (GdkEvent *)&tmp_event);

	/* and close the completion window */
	clear_completion_cache();
	addrcompl_destroy_window( _compWindow_ );

	return TRUE;
}

/*
 * ============================================================================
 * Publically accessible functions.
 * ============================================================================
 */

/**
 * Setup completion object.
 */
void addrcompl_initialize( void ) {
	/* g_print( "addrcompl_initialize...\n" ); */
	if( ! _compWindow_ ) {
		_compWindow_ = addrcompl_create_window();
	}
	_queryID_ = 0;
	_completionIdleID_ = 0;
	/* g_print( "addrcompl_initialize...done\n" ); */
}

/**
 * Teardown completion object.
 */
void addrcompl_teardown( void ) {
	/* g_print( "addrcompl_teardown...\n" ); */
	addrcompl_free_window( _compWindow_ );
	_compWindow_ = NULL;
	if( _displayQueue_ ) {
		g_list_free( _displayQueue_ );
	}
	_displayQueue_ = NULL;
	_completionIdleID_ = 0;
	/* g_print( "addrcompl_teardown...done\n" ); */
}

/*
 * tree view functions
 */

static GtkListStore *addr_compl_create_store(void)
{
	return gtk_list_store_new(N_ADDR_COMPL_COLUMNS,
				  GDK_TYPE_PIXBUF,
				  G_TYPE_STRING,
				  G_TYPE_BOOLEAN,
				  G_TYPE_POINTER,
				  -1);
}
					     
static GtkWidget *addr_compl_list_view_create(CompletionWindow *window)
{
	GtkTreeView *list_view;
	GtkTreeSelection *selector;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(addr_compl_create_store());
	list_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(model));
	g_object_unref(model);	
	
	gtk_tree_view_set_rules_hint(list_view, prefs_common.use_stripes_everywhere);
	gtk_tree_view_set_headers_visible(list_view, FALSE);
	
	selector = gtk_tree_view_get_selection(list_view);
	gtk_tree_selection_set_mode(selector, GTK_SELECTION_BROWSE);
	gtk_tree_selection_set_select_function(selector, addr_compl_selected,
					       window, NULL);

	/* create the columns */
	addr_compl_create_list_view_columns(GTK_WIDGET(list_view));

	return GTK_WIDGET(list_view);
}

static void addr_compl_create_list_view_columns(GtkWidget *list_view)
{
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	renderer = gtk_cell_renderer_pixbuf_new();
	column = gtk_tree_view_column_new_with_attributes
		("", renderer,
	         "pixbuf", ADDR_COMPL_ICON, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
		("", renderer, "text", ADDR_COMPL_ADDRESS, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(list_view), column);		
}

static gboolean list_view_button_press(GtkWidget *widget, GdkEventButton *event,
				       CompletionWindow *window)
{
	if (window && event && event->type == GDK_BUTTON_PRESS) {
		window->in_mouse = TRUE;
	}
	return FALSE;
}

static gboolean list_view_button_release(GtkWidget *widget, GdkEventButton *event,
				         CompletionWindow *window)
{
	if (window && event && event->type == GDK_BUTTON_RELEASE) {
		window->in_mouse = FALSE;
	}
	return FALSE;
}

static gboolean addr_compl_selected(GtkTreeSelection *selector,
			            GtkTreeModel *model, 
				    GtkTreePath *path,
				    gboolean currently_selected,
				    gpointer data)
{
	CompletionWindow *window = data;

	if (currently_selected)
		return TRUE;
	
	if (!window->in_mouse)
		return TRUE;

	/* XXX: select the entry and kill window later... select is called before
	 * any other mouse events handlers including the tree view internal one;
	 * not using a time out would result in a crash. if this doesn't work
	 * safely, maybe we should set variables when receiving button presses
	 * in the tree view. */
	if (!window->destroying) {
		window->destroying = TRUE;
		g_idle_add((GSourceFunc) addr_compl_defer_select_destruct, data);
	}

	return TRUE;
}

static gboolean addr_compl_defer_select_destruct(CompletionWindow *window)
{
	GtkEntry *entry = GTK_ENTRY(window->entry);

	completion_window_apply_selection(GTK_TREE_VIEW(window->list_view), 
					  entry, TRUE);

	clear_completion_cache();

	addrcompl_destroy_window(window);
	return FALSE;
}


/*
 * End of Source.
 */

