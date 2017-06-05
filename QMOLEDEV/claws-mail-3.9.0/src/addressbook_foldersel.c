/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2001-2012 Match Grun and the Claws Mail team
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
 * Add address to address book dialog.
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

#include "gtkutils.h"
#include "stock_pixmap.h"
#include "prefs_common.h"
#include "addressadd.h"
#include "addritem.h"
#include "addrbook.h"
#include "addrindex.h"
#include "manage_window.h"

typedef struct {
	AddressBookFile	*book;
	ItemFolder	*folder;
} FolderInfo;

typedef struct {
	gchar **folder_path;
	gboolean matched;
	gint index;
	GtkCMCTreeNode *node;
} FolderPathMatch;

static struct _AddressBookFolderSel_dlg {
	GtkWidget *window;
	GtkWidget *tree_folder;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	gint status_cid;
	FolderInfo *fiSelected;
} addressbook_foldersel_dlg;

static GdkPixbuf *folderXpm;
static GdkPixbuf *bookXpm;

static gboolean addressbook_foldersel_cancelled;

static FolderInfo *addressbook_foldersel_create_folderinfo( AddressBookFile *abf, ItemFolder *folder )
{
	FolderInfo *fi = g_new0( FolderInfo, 1 );
	fi->book   = abf;
	fi->folder = folder;
	return fi;
}

static void addressbook_foldersel_free_folderinfo( FolderInfo *fi ) {
	fi->book   = NULL;
	fi->folder = NULL;
	g_free( fi );
}

static gint addressbook_foldersel_delete_event( GtkWidget *widget, GdkEventAny *event, gboolean *cancelled )
{
	addressbook_foldersel_cancelled = TRUE;
	gtk_main_quit();
	return TRUE;
}

static gboolean addressbook_foldersel_key_pressed( GtkWidget *widget, GdkEventKey *event, gboolean *cancelled )
{
	if ( event && event->keyval == GDK_KEY_Escape ) {
		addressbook_foldersel_cancelled = TRUE;
		gtk_main_quit();
	}
	return FALSE;
}

static void addressbook_foldersel_ok( GtkWidget *widget, gboolean *cancelled )
{
	addressbook_foldersel_cancelled = FALSE;
	gtk_main_quit();
}

static void addressbook_foldersel_cancel( GtkWidget *widget, gboolean *cancelled )
{
	addressbook_foldersel_cancelled = TRUE;
	gtk_main_quit();
}

static void addressbook_foldersel_folder_select( GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gint column, gpointer data )
{
	addressbook_foldersel_dlg.fiSelected = gtk_cmctree_node_get_row_data( ctree, node );
}

static gboolean addressbook_foldersel_tree_button( GtkCMCTree *ctree, GdkEventButton *event, gpointer data )
{
	if ( ! event )
		return FALSE;
	if ( event->button == 1 ) {
		/* Handle double click */
		if ( event->type == GDK_2BUTTON_PRESS ) {
			addressbook_foldersel_cancelled = FALSE;
			gtk_main_quit();
		}
	}

	return FALSE;
}

static void addressbook_foldersel_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.addressbook_folderselwin_width = allocation->width;
	prefs_common.addressbook_folderselwin_height = allocation->height;
}

static void addressbook_foldersel_create( void )
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *tree_folder;
	GtkWidget *vlbox;
	GtkWidget *tree_win;
	GtkWidget *hbbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	static GdkGeometry geometry;
	gchar *titles[1];

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "addressbook_foldersel" );
	gtk_container_set_border_width( GTK_CONTAINER(window), 0 );
	gtk_window_set_title( GTK_WINDOW(window), _("Select Address Book Folder") );
	gtk_window_set_position( GTK_WINDOW(window), GTK_WIN_POS_MOUSE );
	g_signal_connect( G_OBJECT(window), "delete_event",
			  G_CALLBACK(addressbook_foldersel_delete_event), NULL );
	g_signal_connect( G_OBJECT(window), "key_press_event",
			  G_CALLBACK(addressbook_foldersel_key_pressed), NULL );
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(addressbook_foldersel_size_allocate_cb), NULL);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(window), vbox);
	gtk_container_set_border_width( GTK_CONTAINER(vbox), 8 );

	/* Address book/folder tree */
	vlbox = gtk_vbox_new(FALSE, 8);
	gtk_box_pack_start(GTK_BOX(vbox), vlbox, TRUE, TRUE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(vlbox), 8 );

	tree_win = gtk_scrolled_window_new( NULL, NULL );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW(tree_win),
				        GTK_POLICY_AUTOMATIC,
				        GTK_POLICY_AUTOMATIC );
	gtk_box_pack_start( GTK_BOX(vlbox), tree_win, TRUE, TRUE, 0 );

	titles[0] = _( "Address Book") ;

	tree_folder = gtk_sctree_new_with_titles( 1, 0, titles );
	gtk_container_add( GTK_CONTAINER(tree_win), tree_folder );
	gtk_cmclist_column_titles_show( GTK_CMCLIST(tree_folder) );
	gtk_cmctree_set_line_style(GTK_CMCTREE(tree_folder), GTK_CMCTREE_LINES_NONE);
	gtk_cmctree_set_expander_style(GTK_CMCTREE(tree_folder),
			     GTK_CMCTREE_EXPANDER_TRIANGLE);
	gtk_sctree_set_stripes(GTK_SCTREE(tree_folder), prefs_common.use_stripes_everywhere);
	gtk_cmclist_set_selection_mode( GTK_CMCLIST(tree_folder), GTK_SELECTION_BROWSE );
	gtk_cmctree_set_indent( GTK_CMCTREE(tree_folder), CTREE_INDENT );
	gtk_cmclist_set_auto_sort( GTK_CMCLIST(tree_folder), TRUE );

	/* Button panel */
	gtkut_stock_button_set_create( &hbbox, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL );
	gtk_box_pack_end( GTK_BOX(vbox), hbbox, FALSE, FALSE, 0 );
	gtk_container_set_border_width( GTK_CONTAINER(hbbox), 0 );
	gtk_widget_grab_default( ok_btn );

	g_signal_connect( G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(addressbook_foldersel_ok), NULL );
	g_signal_connect( G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(addressbook_foldersel_cancel), NULL );
	g_signal_connect( G_OBJECT(tree_folder), "tree_select_row",
			 G_CALLBACK(addressbook_foldersel_folder_select), NULL );
	g_signal_connect( G_OBJECT(tree_folder), "button_press_event",
			 G_CALLBACK(addressbook_foldersel_tree_button), NULL );

	if ( !geometry.min_height ) {
		geometry.min_width = 300;
		geometry.min_height = 350;
	}

	gtk_window_set_geometry_hints( GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE );
	gtk_widget_set_size_request( window, prefs_common.addressbook_folderselwin_width,
				    prefs_common.addressbook_folderselwin_height );

	gtk_widget_show_all( vbox );

	addressbook_foldersel_dlg.window      = window;
	addressbook_foldersel_dlg.tree_folder = tree_folder;
	addressbook_foldersel_dlg.ok_btn      = ok_btn;
	addressbook_foldersel_dlg.cancel_btn  = cancel_btn;

	gtk_widget_show_all( window );

	stock_pixbuf_gdk( window, STOCK_PIXMAP_BOOK, &bookXpm);
	stock_pixbuf_gdk( window, STOCK_PIXMAP_DIR_OPEN,
			  &folderXpm);
}

static void addressbook_foldersel_load_folder( GtkCMCTreeNode *parentNode, ItemFolder *parentFolder,
					FolderInfo *fiParent, FolderPathMatch *match )
{
	GtkCMCTree *tree = GTK_CMCTREE( addressbook_foldersel_dlg.tree_folder );
	GList *list;
	ItemFolder *folder;
	gchar *fName;
	gchar **name;
	GtkCMCTreeNode *node;
	FolderInfo *fi;
	FolderPathMatch *nextmatch = NULL;

	list = parentFolder->listFolder;
	while ( list ) {
		folder = list->data;
		fName = g_strdup( ADDRITEM_NAME(folder) );

		name = &fName;
		node = gtk_cmctree_insert_node( tree, parentNode, NULL, name, FOLDER_SPACING,
				folderXpm, folderXpm,
				FALSE, TRUE );

		/* match folder name, match pointer will be set to NULL if next recursive call
		   doesn't need to match subfolder name */
		if ( match != NULL &&
			 match->matched == FALSE ) {
			if ( strcmp(match->folder_path[match->index], folder->obj.uid) == 0 ) {
				/* folder name matches, prepare next subfolder match */

				debug_print("matched folder name '%s'\n", fName);

				match->index++;

				if ( match->folder_path[match->index] == NULL ) {
					/* we've matched all elements */
					match->matched = TRUE;
					match->node = node;
					debug_print("book/folder path matched!\n");
				} else {
					/* keep on matching */
					nextmatch = match;
				}
			}
		}

		g_free( fName );

		fi = addressbook_foldersel_create_folderinfo( fiParent->book, folder );
		gtk_cmctree_node_set_row_data_full( tree, node, fi,
				( GDestroyNotify ) addressbook_foldersel_free_folderinfo );
		addressbook_foldersel_load_folder( node, folder, fi, nextmatch );
		list = g_list_next( list );
	}
}

static void addressbook_foldersel_load_data( AddressIndex *addrIndex, 
					     FolderPathMatch* match )
{
	AddressDataSource *ds;
	GList *list, *nodeDS;
	gchar **name;
	gchar *dsName;
	ItemFolder *rootFolder;
	AddressBookFile *abf;
	FolderInfo *fi;
	GtkCMCTree *tree = GTK_CMCTREE( addressbook_foldersel_dlg.tree_folder );
	GtkCMCTreeNode *node;
	FolderPathMatch *nextmatch;

	gtk_cmclist_clear( GTK_CMCLIST( tree ) );
	list = addrindex_get_interface_list( addrIndex );
	while ( list ) {
		AddressInterface *interface = list->data;
		if ( interface->type == ADDR_IF_BOOK ) {
			nodeDS = interface->listSource;
			while ( nodeDS ) {
				ds = nodeDS->data;
				dsName = g_strdup( addrindex_ds_get_name( ds ) );

				/* Read address book */
				if( ! addrindex_ds_get_read_flag( ds ) ) {
					addrindex_ds_read_data( ds );
				}

				/* Add node for address book */
				abf = ds->rawDataSource;
				name = &dsName;
				node = gtk_cmctree_insert_node( tree, NULL, NULL,
						name, FOLDER_SPACING, bookXpm,
						bookXpm,
						FALSE, TRUE );
				g_free( dsName );

				/* try to match subfolders if this book is the right book
					(and if there's smth to match, and not yet matched) */
				nextmatch = NULL;
				if ( match->folder_path != NULL &&
					 match->matched == FALSE &&
					 match->folder_path[0] != NULL &&
					 strcmp(match->folder_path[0], abf->fileName) == 0 ) {

					debug_print("matched book name '%s'\n", abf->fileName);

					match->index = 1;

					if ( match->folder_path[match->index] == NULL ) {
						/* we've matched all elements */
						match->matched = TRUE;
						match->node = node;
						debug_print("book path matched!\n");
					} else {
						/* keep on matching */
						nextmatch = match;
					}
				}

				fi = addressbook_foldersel_create_folderinfo( abf, NULL );
				gtk_cmctree_node_set_row_data_full( tree, node, fi,
						( GDestroyNotify ) addressbook_foldersel_free_folderinfo );

				rootFolder = addrindex_ds_get_root_folder( ds );
				addressbook_foldersel_load_folder( node, rootFolder, fi, nextmatch );

				nodeDS = g_list_next( nodeDS );
			}
		}
		list = g_list_next( list );
	}
}

gboolean addressbook_foldersel_selection( AddressIndex *addrIndex,
					AddressBookFile **book, ItemFolder **folder, 
					const gchar* path)
{
	FolderPathMatch folder_path_match = { NULL, FALSE, 0, NULL };
	gboolean retVal = FALSE;
	addressbook_foldersel_cancelled = FALSE;

	if ( ! addressbook_foldersel_dlg.window )
		addressbook_foldersel_create();
	gtk_widget_grab_focus(addressbook_foldersel_dlg.ok_btn);
	gtk_widget_show(addressbook_foldersel_dlg.window);
	manage_window_set_transient(GTK_WINDOW(addressbook_foldersel_dlg.window));
	gtk_window_set_modal(GTK_WINDOW(addressbook_foldersel_dlg.window), TRUE);
	
	addressbook_foldersel_dlg.fiSelected = NULL;

	/* split the folder path we've received, we'll try to match this path, subpath by
	   subpath against the book/folder structure in order to select the folder that
       corresponds to what we received */

	if ( path != NULL ) {
		if ( g_utf8_collate(path, _("Any")) == 0 || strcasecmp(path, "Any") ==0 || *path == '\0' )
			/* consider "Any" (both translated or untranslated forms) and ""
			   as valid addressbook roots */
			folder_path_match.matched = TRUE;
		else
			folder_path_match.folder_path = g_strsplit( path, "/", 256 );
	}

	addressbook_foldersel_load_data( addrIndex, &folder_path_match );

	if ( folder_path_match.folder_path != NULL && folder_path_match.matched == FALSE)
		g_warning("addressbook_foldersel_load_data: couldn't match book/folder path '%s'\n", path);

	g_strfreev( folder_path_match.folder_path );

	if ( folder_path_match.node != NULL)
		gtk_cmctree_select( GTK_CMCTREE( addressbook_foldersel_dlg.tree_folder ),
							GTK_CMCTREE_NODE( folder_path_match.node ) );
	else
		gtk_cmclist_select_row( GTK_CMCLIST( addressbook_foldersel_dlg.tree_folder ), 0, 0 );
	gtk_widget_show(addressbook_foldersel_dlg.window);

	gtk_main();
	gtk_widget_hide( addressbook_foldersel_dlg.window );
	gtk_window_set_modal(GTK_WINDOW(addressbook_foldersel_dlg.window), FALSE);
	if ( ! addressbook_foldersel_cancelled ) {

		*book = NULL;
		*folder = NULL;

		if ( addressbook_foldersel_dlg.fiSelected ) {
			*book = addressbook_foldersel_dlg.fiSelected->book;
			*folder = addressbook_foldersel_dlg.fiSelected->folder;
			retVal = TRUE;
		}
	}

	gtk_cmclist_clear( GTK_CMCLIST( addressbook_foldersel_dlg.tree_folder ) );

	return retVal;
}

/*
* End of Source.
*/
