/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 Match Grun and the Claws Mail team
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

/*
 * Browse LDAP entry.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_LDAP

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <pthread.h>
#include "gtkutils.h"
#include "stock_pixmap.h"
#include "prefs_common.h"
#include "browseldap.h"
#include "addritem.h"
#include "addrindex.h"
#include "manage_window.h"

#include "ldapquery.h"
#include "ldapserver.h"
#include "ldaplocate.h"

typedef enum {
	COL_NAME  = 0,
	COL_VALUE = 1
} LDAPEntryColumnPos;

#define BROWSELDAP_WIDTH    450
#define BROWSELDAP_HEIGHT   420

#define N_COLS              2
#define COL_WIDTH_NAME      140
#define COL_WIDTH_VALUE     140

static struct _LDAPEntry_dlg {
	GtkWidget *window;
	GtkWidget *label_server;
	GtkWidget *label_address;
	GtkWidget *list_entry;
	GtkWidget *close_btn;
} browseldap_dlg;

/**
 * Message queue.
 */
static GList *_displayQueue_ = NULL;

/**
 * Mutex to protect callback from multiple threads.
 */
static pthread_mutex_t _browseMutex_ = PTHREAD_MUTEX_INITIALIZER;

/**
 * Current query ID.
 */
static gint _queryID_ = 0;

/**
 * Completion idle ID.
 */
static guint _browseIdleID_ = 0;

/**
 * Search complete indicator.
 */
static gboolean _searchComplete_ = FALSE;

/**
 * Callback entry point for each LDAP entry processed. The background thread
 * (if any) appends the address list to the display queue.
 * 
 * \param qry        LDAP query object.
 * \param queryID    Query ID of search request.
 * \param listEMail  List of zero of more email objects that met search
 *                   criteria.
 * \param data       User data.
 */
static gint browse_callback_entry(
		LdapQuery *qry, gint queryID, GList *listValues, gpointer data )
{
	GList *node;
	NameValuePair *nvp;

	debug_print("browse_callback_entry...\n");
	pthread_mutex_lock( & _browseMutex_ );
	/* Append contents to end of display queue */
	node = listValues;
	while( node ) {
		nvp = ( NameValuePair * ) node->data;
		debug_print("adding to list: %s->%s\n",
				nvp->name?nvp->name:"null",
				nvp->value?nvp->value:"null");
		_displayQueue_ = g_list_append( _displayQueue_, nvp );
		node->data = NULL;
		node = g_list_next( node );
	}
	pthread_mutex_unlock( & _browseMutex_ );
	/* g_print( "browse_callback_entry...done\n" ); */

	return 0;
}

/**
 * Callback entry point for end of LDAP locate search.
 * 
 * \param qry     LDAP query object.
 * \param queryID Query ID of search request.
 * \param status  Status/error code.
 * \param data    User data.
 */
static gint browse_callback_end(
		LdapQuery *qry, gint queryID, gint status, gpointer data )
{
	debug_print("search completed\n");
	_searchComplete_ = TRUE;
	return 0;
}

/**
 * Clear the display queue.
 */
static void browse_clear_queue( void ) {
	/* Clear out display queue */
	pthread_mutex_lock( & _browseMutex_ );

	ldapqry_free_list_name_value( _displayQueue_ );
	g_list_free( _displayQueue_ );
	_displayQueue_ = NULL;

	pthread_mutex_unlock( & _browseMutex_ );
}

/**
 * Close window callback.
 * \param widget    Widget.
 * \param event     Event.
 * \param cancelled Cancelled flag.
 */
static gint browse_delete_event(
		GtkWidget *widget, GdkEventAny *event, gboolean *cancelled )
{
	gtk_main_quit();
	return TRUE;
}

/**
 * Respond to key press in window.
 * \param widget    Widget.
 * \param event     Event.
 * \param cancelled Cancelled flag.
 */
static void browse_key_pressed(
		GtkWidget *widget, GdkEventKey *event, gboolean *cancelled )
{
	if (event && event->keyval == GDK_KEY_Escape) {
		gtk_main_quit();
	}
}

/**
 * Callback to close window.
 * \param widget    Widget.
 * \param cancelled Cancelled flag.
 */
static void browse_close( GtkWidget *widget, gboolean *cancelled ) {
	gtk_main_quit();
}

/**
 * Create the window to display data.
 */
static void browse_create( void ) {
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *label_server;
	GtkWidget *label_addr;
	GtkWidget *list_entry;
	GtkWidget *vlbox;
	GtkWidget *tree_win;
	GtkWidget *hbbox;
	GtkWidget *close_btn;
	gint top;

	debug_print("creating browse widget\n");
	window = gtk_dialog_new();
	gtk_widget_set_size_request( window, BROWSELDAP_WIDTH, BROWSELDAP_HEIGHT );
	gtk_container_set_border_width( GTK_CONTAINER(window), 0 );
	gtk_window_set_title( GTK_WINDOW(window), _("Browse Directory Entry") );
	gtk_window_set_position( GTK_WINDOW(window), GTK_WIN_POS_MOUSE );
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(browse_delete_event), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(browse_key_pressed), NULL);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(window)->vbox), vbox, TRUE, TRUE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(vbox), 8 );

	table = gtk_table_new(2, 2, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 8 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	/* First row */
	top = 0;
	label = gtk_label_new(_("Server Name :"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	label_server = gtk_label_new("");
	gtk_table_attach(GTK_TABLE(table), label_server, 1, 2, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label_server), 0, 0.5);

	/* Second row */
	top++;
	label = gtk_label_new(_("Distinguished Name (dn) :"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	label_addr = gtk_label_new("");
	gtk_table_attach(GTK_TABLE(table), label_addr, 1, 2, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label_addr), 0, 0.5);

	/* Address book/folder tree */
	vlbox = gtk_vbox_new(FALSE, 8);
	gtk_box_pack_start(GTK_BOX(vbox), vlbox, TRUE, TRUE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(vlbox), 8 );

	tree_win = gtk_scrolled_window_new( NULL, NULL );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW(tree_win),
				        GTK_POLICY_AUTOMATIC,
				        GTK_POLICY_AUTOMATIC );
	gtk_box_pack_start( GTK_BOX(vlbox), tree_win, TRUE, TRUE, 0 );

	list_entry = gtk_cmclist_new( N_COLS );
	gtk_container_add( GTK_CONTAINER(tree_win), list_entry );
	gtk_cmclist_column_titles_show( GTK_CMCLIST(list_entry) );
	gtk_cmclist_set_column_title(
		GTK_CMCLIST(list_entry), COL_NAME, _( "LDAP Name" ) );
	gtk_cmclist_set_column_title(
		GTK_CMCLIST(list_entry), COL_VALUE, _( "Attribute Value" ) );
	gtk_cmclist_set_selection_mode(
		GTK_CMCLIST(list_entry), GTK_SELECTION_BROWSE );
	gtk_cmclist_set_column_width( GTK_CMCLIST(list_entry),
		COL_NAME, COL_WIDTH_NAME );
	gtk_cmclist_set_auto_sort( GTK_CMCLIST(list_entry), TRUE );

	/* Button panel */
	gtkut_stock_button_set_create(&hbbox, &close_btn, GTK_STOCK_CLOSE,
				      NULL, NULL, NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), hbbox, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(hbbox), 0 );

	g_signal_connect(G_OBJECT(close_btn), "clicked",
			 G_CALLBACK(browse_close), NULL);
	gtk_widget_grab_default(close_btn);

	gtk_widget_show_all(vbox);

	browseldap_dlg.window        = window;
	browseldap_dlg.label_server  = label_server;
	browseldap_dlg.label_address = label_addr;
	browseldap_dlg.list_entry    = list_entry;
	browseldap_dlg.close_btn     = close_btn;

	gtk_widget_show_all( window );

}

/**
 * Idler function. This function is called by the main (UI) thread during UI
 * idle time while an address search is in progress. Items from the display
 * queue are processed and appended to the address list.
 *
 * \param data Target data object.
 * \return <i>TRUE</i> to ensure that idle event do not get ignored.
 */
static gboolean browse_idle( gpointer data ) {
	GList *node;
	NameValuePair *nvp;
	gchar *text[N_COLS];

	/* Process all entries in display queue */
	pthread_mutex_lock( & _browseMutex_ );
	if( _displayQueue_ ) {
		node = _displayQueue_;
		while( node ) {
			/* Add entry into list */
			nvp = ( NameValuePair * ) node->data;
			text[COL_NAME]  = nvp->name;
			text[COL_VALUE] = nvp->value;
			debug_print("Adding row to list: %s->%s\n",
						nvp->name?nvp->name:"null",
						nvp->value?nvp->value:"null");
			gtk_cmclist_append(
				GTK_CMCLIST(browseldap_dlg.list_entry), text );

			/* Free up entry */
			ldapqry_free_name_value( nvp );
			node->data = NULL;
			node = g_list_next( node );
		}
		g_list_free( _displayQueue_ );
		_displayQueue_ = NULL;
	}
	pthread_mutex_unlock( & _browseMutex_ );

	if( _searchComplete_ ) {
		/* Remove idler */
		if( _browseIdleID_ != 0 ) {
			g_source_remove( _browseIdleID_ );
			_browseIdleID_ = 0;
			gtk_cmclist_select_row(
				GTK_CMCLIST( browseldap_dlg.list_entry ), 0, 0 );
		}
	}

	return TRUE;
}

/**
 * Main entry point to browse LDAP entries.
 * \param  ds Data source to process.
 * \param  dn Distinguished name to retrieve.
 * \return <code>TRUE</code>
 */
gboolean browseldap_entry( AddressDataSource *ds, const gchar *dn ) {
	LdapServer *server;

	_queryID_ = 0;
	_browseIdleID_ = 0;

	server = ds->rawDataSource;

	if( ! browseldap_dlg.window ) browse_create();
	gtk_widget_grab_focus(browseldap_dlg.close_btn);
	gtk_widget_show(browseldap_dlg.window);
	manage_window_set_transient(GTK_WINDOW(browseldap_dlg.window));
	gtk_window_set_modal(GTK_WINDOW(browseldap_dlg.window), TRUE);
	gtk_cmclist_select_row( GTK_CMCLIST( browseldap_dlg.list_entry ), 0, 0 );
	gtk_widget_show(browseldap_dlg.window);

	gtk_label_set_text( GTK_LABEL(browseldap_dlg.label_address ), "" );
	if( dn ) {
		gtk_label_set_text(
			GTK_LABEL(browseldap_dlg.label_address ), dn );
	}
	gtk_label_set_text(
		GTK_LABEL(browseldap_dlg.label_server ),
		ldapsvr_get_name( server ) );

	debug_print("browsing server: %s\n", ldapsvr_get_name(server));
	/* Setup search */
	_searchComplete_ = FALSE;
	_queryID_ = ldaplocate_search_setup(
			server, dn, browse_callback_entry, browse_callback_end );
	debug_print("query id: %d\n", _queryID_);
	_browseIdleID_ = g_idle_add( (GSourceFunc) browse_idle, NULL );

	/* Start search */
	debug_print("starting search\n");
	ldaplocate_search_start( _queryID_ );

	/* Display dialog */
	gtk_main();
	gtk_widget_hide( browseldap_dlg.window );
	gtk_window_set_modal(GTK_WINDOW(browseldap_dlg.window), FALSE);
	/* Stop query */
	debug_print("stopping search\n");
	ldaplocate_search_stop( _queryID_ );

	if( _browseIdleID_ != 0 ) {
		g_source_remove( _browseIdleID_ );
		_browseIdleID_ = 0;
	}
	browse_clear_queue();
	gtk_cmclist_clear( GTK_CMCLIST( browseldap_dlg.list_entry ) );

	return TRUE;
}

#endif /* USE_LDAP */

/*
* End of Source.
*/

