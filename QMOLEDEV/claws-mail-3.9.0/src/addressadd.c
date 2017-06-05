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
#include "prefs_gtk.h"
#include "addressadd.h"
#ifndef USE_NEW_ADDRBOOK
	#include "addritem.h"
	#include "addrbook.h"
	#include "addrindex.h"
	#include "ldapserver.h"
	#include "ldapupdate.h"
#else
	#include "addressbook-dbus.h"
#endif
#include "manage_window.h"
#include "alertpanel.h"

#ifndef USE_NEW_ADDRBOOK
typedef struct {
	AddressBookFile	*book;
	ItemFolder	*folder;
} FolderInfo;
#else
typedef struct {
    gchar* book;
} FolderInfo;
#endif

static struct _AddressAdd_dlg {
	GtkWidget *window;
	GtkWidget *picture;
	GtkWidget *entry_name;
	GtkWidget *label_address;
	GtkWidget *entry_remarks;
	GtkWidget *tree_folder;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	FolderInfo *fiSelected;
} addressadd_dlg;

static GdkPixbuf *folderXpm;
static GdkPixbuf *bookXpm;

static gboolean addressadd_cancelled;

#ifndef USE_NEW_ADDRBOOK
static FolderInfo *addressadd_create_folderinfo( AddressBookFile *abf, ItemFolder *folder )
{
	FolderInfo *fi = g_new0( FolderInfo, 1 );
	fi->book   = abf;
	fi->folder = folder;
	return fi;
}
#else
static FolderInfo *addressadd_create_folderinfo(gchar* book) {
	FolderInfo *fi = g_new0( FolderInfo, 1 );
	fi->book = book;
	return fi;
}
#endif

#ifndef USE_NEW_ADDRBOOK
static void addressadd_free_folderinfo( FolderInfo *fi ) {
	fi->book   = NULL;
	fi->folder = NULL;
	g_free( fi );
}
#else
static void addressadd_free_folderinfo( FolderInfo *fi ) {
	fi->book   = NULL;
	g_free( fi );
}
#endif

static gint addressadd_delete_event( GtkWidget *widget, GdkEventAny *event, gboolean *cancelled ) {
	addressadd_cancelled = TRUE;
	gtk_main_quit();
	return TRUE;
}

static gboolean addressadd_key_pressed( GtkWidget *widget, GdkEventKey *event, gboolean *cancelled ) {
	if (event && event->keyval == GDK_KEY_Escape) {
		addressadd_cancelled = TRUE;
		gtk_main_quit();
	}
	return FALSE;
}

static void addressadd_ok( GtkWidget *widget, gboolean *cancelled ) {
	addressadd_cancelled = FALSE;
	gtk_main_quit();
}

static void addressadd_cancel( GtkWidget *widget, gboolean *cancelled ) {
	addressadd_cancelled = TRUE;
	gtk_main_quit();
}

static void addressadd_folder_select( GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gint column, gpointer data )
{
	addressadd_dlg.fiSelected = gtk_cmctree_node_get_row_data( ctree, node );
}

static gboolean addressadd_tree_button( GtkCMCTree *ctree, GdkEventButton *event, gpointer data ) {
	if( ! event )
		return FALSE;
	if( event->button == 1 ) {
		/* Handle double click */
		if( event->type == GDK_2BUTTON_PRESS ) {
			addressadd_cancelled = FALSE;
			gtk_main_quit();
		}
	}

	return FALSE;
}

static void addressadd_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.addressaddwin_width = allocation->width;
	prefs_common.addressaddwin_height = allocation->height;
}

static void addressadd_create( void ) {
	GtkWidget *window;
	GtkWidget *vbox, *hbox;
	GtkWidget *frame;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *picture;
	GtkWidget *entry_name;
	GtkWidget *label_addr;
	GtkWidget *entry_rems;
	GtkWidget *tree_folder;
	GtkWidget *vlbox;
	GtkWidget *tree_win;
	GtkWidget *hbbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	gint top;
	static GdkGeometry geometry;
	gchar *titles[1];

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "addressadd");
	gtk_container_set_border_width( GTK_CONTAINER(window), VBOX_BORDER );
	gtk_window_set_title( GTK_WINDOW(window), _("Add to address book") );
	gtk_window_set_position( GTK_WINDOW(window), GTK_WIN_POS_MOUSE );
	g_signal_connect( G_OBJECT(window), "delete_event",
			  G_CALLBACK(addressadd_delete_event), NULL );
	g_signal_connect( G_OBJECT(window), "key_press_event",
			  G_CALLBACK(addressadd_key_pressed), NULL );
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(addressadd_size_allocate_cb), NULL);
	
	hbox = gtk_hbox_new(FALSE, 10);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 4);
	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	picture = gtk_image_new();
	gtk_box_pack_start(GTK_BOX(hbox), picture, FALSE, FALSE, 0);

	table = gtk_table_new(3, 2, FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), table, TRUE, TRUE, 0);
	gtk_table_set_row_spacings(GTK_TABLE(table), VSPACING_NARROW);
	gtk_table_set_col_spacings(GTK_TABLE(table), HSPACING_NARROW);

	frame = gtk_frame_new(_("Contact"));
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.01, 0.5);
	gtk_container_add(GTK_CONTAINER(frame), hbox);
	gtk_container_set_border_width( GTK_CONTAINER(frame), 4);
	gtk_box_pack_start(GTK_BOX(vbox), frame, FALSE, FALSE, 0);

	/* First row */
	top = 0;
	label = gtk_label_new(_("Name"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_name = gtk_entry_new();
	gtk_widget_set_size_request(entry_name, 150, -1);
	gtk_entry_set_text (GTK_ENTRY(entry_name),"");
	
	gtk_table_attach(GTK_TABLE(table), entry_name, 1, 2, top, (top + 1), GTK_FILL | GTK_EXPAND , 0, 0, 0);

	/* Second row */
	top = 1;
	label = gtk_label_new(_("Address"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0);

	label_addr = gtk_label_new("");
	gtk_widget_set_size_request(label_addr, 150, -1);
	gtk_table_attach(GTK_TABLE(table), label_addr, 1, 2, top, (top + 1), GTK_FILL | GTK_EXPAND, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label_addr), 0, 0.5);

	/* Third row */
	top = 2;
	label = gtk_label_new(_("Remarks"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_rems = gtk_entry_new();
	gtk_widget_set_size_request(entry_rems, 150, -1);
	gtk_table_attach(GTK_TABLE(table), entry_rems, 1, 2, top, (top + 1), GTK_FILL | GTK_EXPAND, 0, 0, 0);
	
	/* Address book/folder tree */
	vlbox = gtk_vbox_new(FALSE, VBOX_BORDER);
	gtk_box_pack_start(GTK_BOX(vbox), vlbox, TRUE, TRUE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(vlbox), 4);

	tree_win = gtk_scrolled_window_new( NULL, NULL );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW(tree_win),
				        GTK_POLICY_AUTOMATIC,
				        GTK_POLICY_AUTOMATIC );
	gtk_box_pack_start( GTK_BOX(vlbox), tree_win, TRUE, TRUE, 0 );

	titles[0] = _( "Select Address Book Folder") ;

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
	gtkut_stock_button_set_create(&hbbox, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), hbbox, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(hbbox), HSPACING_NARROW );
	gtk_widget_grab_default(ok_btn);

	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(addressadd_ok), NULL);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(addressadd_cancel), NULL);
	g_signal_connect(G_OBJECT(tree_folder), "tree_select_row",
			 G_CALLBACK(addressadd_folder_select), NULL);
	g_signal_connect(G_OBJECT(tree_folder), "button_press_event",
			 G_CALLBACK(addressadd_tree_button), NULL);

	if (!geometry.min_height) {
		geometry.min_width = 300;
		geometry.min_height = 350;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.addressaddwin_width,
				    prefs_common.addressaddwin_height);

	addressadd_dlg.window        = window;
	addressadd_dlg.picture       = picture;
	addressadd_dlg.entry_name    = entry_name;
	addressadd_dlg.label_address = label_addr;
	addressadd_dlg.entry_remarks = entry_rems;
	addressadd_dlg.tree_folder   = tree_folder;
	addressadd_dlg.ok_btn        = ok_btn;
	addressadd_dlg.cancel_btn    = cancel_btn;

	gtk_widget_show_all( window );

	stock_pixbuf_gdk( window, STOCK_PIXMAP_BOOK, &bookXpm );
	stock_pixbuf_gdk( window, STOCK_PIXMAP_DIR_OPEN,
			  &folderXpm );
}

#ifndef USE_NEW_ADDRBOOK
static void addressadd_load_folder( GtkCMCTreeNode *parentNode, ItemFolder *parentFolder,
					FolderInfo *fiParent )
{
	GtkCMCTree *tree = GTK_CMCTREE( addressadd_dlg.tree_folder );
	GList *list;
	ItemFolder *folder;
	gchar *fName;
	gchar **name;
	GtkCMCTreeNode *node;
	FolderInfo *fi;

	list = parentFolder->listFolder;
	while( list ) {
		folder = list->data;
		fName = g_strdup( ADDRITEM_NAME(folder) );
		name = &fName;
		node = gtk_sctree_insert_node( tree, parentNode, NULL, name, FOLDER_SPACING,
				folderXpm, folderXpm,
				FALSE, TRUE );
		g_free( fName );
		fi = addressadd_create_folderinfo( fiParent->book, folder );
		gtk_cmctree_node_set_row_data_full( tree, node, fi,
				( GDestroyNotify ) addressadd_free_folderinfo );
		addressadd_load_folder( node, folder, fi );
		list = g_list_next( list );
	}
}

static void addressadd_load_data( AddressIndex *addrIndex ) {
	AddressDataSource *ds;
	GList *list, *nodeDS;
	gchar **name;
	gchar *dsName;
	ItemFolder *rootFolder;
	AddressBookFile *abf;
	FolderInfo *fi;
	GtkCMCTree *tree = GTK_CMCTREE( addressadd_dlg.tree_folder );
	GtkCMCTreeNode *node;

	gtk_cmclist_clear( GTK_CMCLIST( tree ) );
	list = addrindex_get_interface_list( addrIndex );
	while( list ) {
		AddressInterface *ainterface = list->data;
		if( ainterface->type == ADDR_IF_BOOK || 
				ainterface->type == ADDR_IF_LDAP ) {
			nodeDS = ainterface->listSource;
			while( nodeDS ) {
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

				fi = addressadd_create_folderinfo( abf, NULL );
				gtk_cmctree_node_set_row_data_full( tree, node, fi,
						( GDestroyNotify ) addressadd_free_folderinfo );

				rootFolder = addrindex_ds_get_root_folder( ds );
				addressadd_load_folder( node, rootFolder, fi );

				nodeDS = g_list_next( nodeDS );
			}
		}
		list = g_list_next( list );
	}
}
#else
static void addressadd_load_data() {
	GSList *list;
	gchar *name;
	GtkCMCTree *tree = GTK_CMCTREE(addressadd_dlg.tree_folder);
	GtkCMCTreeNode *node;
	FolderInfo *fi = NULL;
	GError* error = NULL;

	gtk_cmclist_clear(GTK_CMCLIST(tree));
	list = addressbook_dbus_get_books(&error);
	for (; list; list = g_slist_next(list)) {
            name = (gchar *) list->data;
            node = gtk_cmctree_insert_node(tree, NULL, NULL,
                                           &name, FOLDER_SPACING, bookXpm,
                                           bookXpm, FALSE, TRUE);
	    fi = addressadd_create_folderinfo(name);
	    gtk_cmctree_node_set_row_data_full(tree, node, fi,
                    ( GDestroyNotify ) addressadd_free_folderinfo );
	}
}
#endif

#ifndef USE_NEW_ADDRBOOK 
gboolean addressadd_selection( AddressIndex *addrIndex, const gchar *name, 
		const gchar *address, const gchar *remarks, GdkPixbuf *picture ) {
#else
gboolean addressadd_selection(const gchar *name, const gchar *address,
							  const gchar *remarks, GdkPixbuf *picture ) {
#endif
	gboolean retVal = FALSE;
#ifndef USE_NEW_ADDRBOOK 
	ItemPerson *person = NULL;
#endif
	FolderInfo *fi = NULL;
	addressadd_cancelled = FALSE;

	if( ! addressadd_dlg.window ) addressadd_create();

	addressadd_dlg.fiSelected = NULL;
#ifndef USE_NEW_ADDRBOOK
	addressadd_load_data( addrIndex );
#else
	addressadd_load_data();
#endif
	gtk_cmclist_select_row( GTK_CMCLIST( addressadd_dlg.tree_folder ), 0, 0 );
	gtk_widget_show(addressadd_dlg.window);
	gtk_window_set_modal(GTK_WINDOW(addressadd_dlg.window), TRUE);
	gtk_widget_grab_focus(addressadd_dlg.ok_btn);

	manage_window_set_transient(GTK_WINDOW(addressadd_dlg.window));

	gtk_entry_set_text( GTK_ENTRY(addressadd_dlg.entry_name ), "" );
	gtk_label_set_text( GTK_LABEL(addressadd_dlg.label_address ), "" );
	gtk_entry_set_text( GTK_ENTRY(addressadd_dlg.entry_remarks ), "" );
	if( name )
		gtk_entry_set_text( GTK_ENTRY(addressadd_dlg.entry_name ), name );
	if( address )
		gtk_label_set_text( GTK_LABEL(addressadd_dlg.label_address ), address );
	if( remarks )
		gtk_entry_set_text( GTK_ENTRY(addressadd_dlg.entry_remarks ), remarks );
	if( picture ) {
		gtk_image_set_from_pixbuf(GTK_IMAGE(addressadd_dlg.picture), picture);
		gtk_widget_show(GTK_WIDGET(addressadd_dlg.picture));
	} else {
		gtk_widget_hide(GTK_WIDGET(addressadd_dlg.picture));
	}
	gtk_main();
	gtk_widget_hide( addressadd_dlg.window );
	gtk_window_set_modal(GTK_WINDOW(addressadd_dlg.window), FALSE);
	if( ! addressadd_cancelled ) {
		if( addressadd_dlg.fiSelected ) {
			gchar *returned_name;
			gchar *returned_remarks;
			returned_name = gtk_editable_get_chars( GTK_EDITABLE(addressadd_dlg.entry_name), 0, -1 );
			returned_remarks = gtk_editable_get_chars( GTK_EDITABLE(addressadd_dlg.entry_remarks), 0, -1 );

			fi = addressadd_dlg.fiSelected;
			
#ifndef USE_NEW_ADDRBOOK
			person = addrbook_add_contact( fi->book, fi->folder, 
							returned_name, 
							address, 
							returned_remarks);
			person->status = ADD_ENTRY;

			if (picture) {
				GError *error = NULL;
				gchar *name = g_strconcat( get_rc_dir(), G_DIR_SEPARATOR_S, ADDRBOOK_DIR, G_DIR_SEPARATOR_S, 
							ADDRITEM_ID(person), ".png", NULL );
				gdk_pixbuf_save(picture, name, "png", &error, NULL);
				if (error) {
					g_warning(_("Failed to save image: \n%s"),
							error->message);
					g_error_free(error);
				}
				addritem_person_set_picture( person, ADDRITEM_ID(person) ) ;
				g_free( name );
			}
#else
			ContactData* contact = g_new0(ContactData, 1);
			GError* error = NULL;
			
			if (returned_name)
				contact->cn = g_strdup(returned_name);
			else
				contact->cn = g_strdup(address);
			
			contact->name = g_strdup(returned_name);
			contact->email = g_strdup(address);
			contact->remarks = g_strdup(returned_remarks);
			contact->book = g_strdup(fi->book);
			contact->picture = picture;
			
            if (addressbook_dbus_add_contact(contact, &error) == 0) {
				debug_print("Added to addressbook:\n%s\n%s\n%s\n%s\n",
							 returned_name, address, returned_remarks, fi->book);
				retVal = TRUE;
			}
			else {
				retVal = FALSE;
				if (error) {
					GtkWidget* dialog = gtk_message_dialog_new (
								GTK_WINDOW(addressadd_dlg.window),
                                GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_ERROR,
                                GTK_BUTTONS_CLOSE,
                                "%s", error->message);
					gtk_dialog_run (GTK_DIALOG (dialog));
					gtk_widget_destroy (dialog);
					g_error_free(error);
				}
			}
			contact_data_free(&contact);
#endif
#ifdef USE_LDAP
			if (fi->book->type == ADBOOKTYPE_LDAP) {
				LdapServer *server = (LdapServer *) fi->book;
				ldapsvr_set_modified(server, TRUE);
				ldapsvr_update_book(server, person);
				if (server->retVal != LDAPRC_SUCCESS) {
					alertpanel( _("Add address(es)"),
						_("Can't add the specified address"),
						GTK_STOCK_CLOSE, NULL, NULL );
					return server->retVal;
				}
			}
#endif
			g_free(returned_name);
			g_free(returned_remarks);
#ifndef USE_NEW_ADDRBOOK
			if( person ) retVal = TRUE;
#endif
		}
	}

	gtk_cmclist_clear( GTK_CMCLIST( addressadd_dlg.tree_folder ) );

	return retVal;
}

/*
* End of Source.
*/

