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

#include "addressbook.h"
#include "addressitem.h"
#include "addrbook.h"
#include "addritem.h"

#include "mgutils.h"

#include "prefs_common.h"

#include "alertpanel.h"
#include "inputdialog.h"
#include "manage_window.h"
#include "gtkutils.h"

#define ADDRESSBOOK_GUESS_FOLDER_NAME	"NewFolder"
#define ADDRESSBOOK_GUESS_GROUP_NAME	"NewGroup"

typedef enum {
	GROUP_COL_NAME    = 0,
	GROUP_COL_EMAIL   = 1,
	GROUP_COL_REMARKS = 2
} GroupEditEMailColumnPos;

#define GROUP_N_COLS          3
#define GROUP_COL_WIDTH_NAME  140
#define GROUP_COL_WIDTH_EMAIL 120

static struct _GroupEdit_dlg {
	GtkWidget *window;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *statusbar;
	gint status_cid;

	/* Basic data tab */
	GtkWidget *entry_name;
	GtkCMCList *clist_group;
	GtkCMCList *clist_avail;

	GHashTable *hashEMail;

} groupeditdlg;


static gchar *_edit_group_dfl_message_ = NULL;

static void edit_group_status_show( gchar *msg ) {
	if( groupeditdlg.statusbar != NULL ) {
		gtk_statusbar_pop( GTK_STATUSBAR(groupeditdlg.statusbar), groupeditdlg.status_cid );
		if( msg ) {
			gtk_statusbar_push( GTK_STATUSBAR(groupeditdlg.statusbar), groupeditdlg.status_cid, msg );
		}
	}
}

static void edit_group_ok(GtkWidget *widget, gboolean *cancelled) {
	gchar *sName;
	gboolean errFlag = TRUE;

	sName = gtk_editable_get_chars( GTK_EDITABLE(groupeditdlg.entry_name), 0, -1 );
	if( sName ) {
		g_strstrip( sName );
		if( *sName != '\0' ) {
			gtk_entry_set_text(GTK_ENTRY(groupeditdlg.entry_name), sName );
			*cancelled = FALSE;
			gtk_main_quit();
			errFlag = FALSE;
		}
	}
	if( errFlag ) {
		edit_group_status_show( _( "A Group Name must be supplied." ) );
	}
	g_free( sName );
}

static void edit_group_cancel(GtkWidget *widget, gboolean *cancelled) {
	*cancelled = TRUE;
	gtk_main_quit();
}

static gint edit_group_delete_event(GtkWidget *widget, GdkEventAny *event, gboolean *cancelled) {
	*cancelled = TRUE;
	gtk_main_quit();
	return TRUE;
}

static gboolean edit_group_key_pressed(GtkWidget *widget, GdkEventKey *event, gboolean *cancelled) {
	if (event && event->keyval == GDK_KEY_Escape) {
		*cancelled = TRUE;
		gtk_main_quit();
	}
	return FALSE;
}

static gchar *edit_group_format_item_clist( ItemPerson *person, ItemEMail *email ) {
	gchar *str = NULL;
	gchar *aName = ADDRITEM_NAME(email);
	if( aName == NULL || *aName == '\0' ) return str;
	if( person ) {
		str = g_strdup_printf( "%s - %s", ADDRITEM_NAME(person), aName );
	}
	else {
		str = g_strdup( aName );
	}
	return str;
}

static gint edit_group_clist_add_email( GtkCMCList *clist, ItemEMail *email ) {
	ItemPerson *person = ( ItemPerson * ) ADDRITEM_PARENT(email);
	gchar *str = edit_group_format_item_clist( person, email );
	gchar *text[ GROUP_N_COLS ];
	gint row;
	if( str ) {
		text[ GROUP_COL_NAME ] = addressbook_set_col_name_guard(str);
	}
	else {
		text[ GROUP_COL_NAME ] = addressbook_set_col_name_guard(ADDRITEM_NAME(person));
	}
	text[ GROUP_COL_EMAIL   ] = email->address;
	text[ GROUP_COL_REMARKS ] = email->remarks;

	row = gtk_cmclist_append( clist, text );
	gtk_cmclist_set_row_data( clist, row, email );
	return row;
}

static void edit_group_load_clist( GtkCMCList *clist, GList *listEMail ) {
	GList *node = listEMail;
	gtk_cmclist_freeze(clist);
	while( node ) {
		ItemEMail *email = node->data;
		edit_group_clist_add_email( clist, email );
		node = g_list_next( node );
	}
	gtk_cmclist_thaw(clist);
}


static void edit_group_move_email( GtkCMCList *clist_from, GtkCMCList *clist_to, GtkCMCTreeNode *node ) {
	ItemEMail *email = gtk_cmctree_node_get_row_data( GTK_CMCTREE(clist_from), node );
	GtkCMCTreeNode *next = gtkut_ctree_node_next(GTK_CMCTREE(clist_from), node);
	GtkCMCTreeNode *prev = gtkut_ctree_node_prev(GTK_CMCTREE(clist_from), node);
	int rrow = 0;
	if( email ) {
		gtk_cmctree_remove_node(GTK_CMCTREE(clist_from), node);
		rrow = edit_group_clist_add_email( clist_to, email );
		gtk_cmclist_select_row( clist_to, rrow, 0 );
		if (next)
			gtk_cmctree_select(GTK_CMCTREE(clist_from), next);
		else if (prev)
			gtk_cmctree_select(GTK_CMCTREE(clist_from), prev);
	}
}

static void edit_group_to_group( GtkWidget *widget, gpointer data ) {
	GList *selected = g_list_copy(GTK_CMCLIST(groupeditdlg.clist_avail)->selection);
	/* Clear the selected rows on destination clist */
	gtk_cmclist_freeze(groupeditdlg.clist_avail);
	gtk_cmclist_freeze(groupeditdlg.clist_group);
	gtk_cmclist_unselect_all(groupeditdlg.clist_group);
	while (selected) {
		edit_group_move_email( groupeditdlg.clist_avail,
					groupeditdlg.clist_group, GTK_CMCTREE_NODE(selected->data) );
		selected = selected->next;
	}
	g_list_free(selected);
	gtk_cmclist_thaw(groupeditdlg.clist_avail);
	gtk_cmclist_thaw(groupeditdlg.clist_group);
}

static void edit_group_to_avail( GtkWidget *widget, gpointer data ) {
	GList *selected = g_list_copy(GTK_CMCLIST(groupeditdlg.clist_group)->selection);
	gtk_cmclist_freeze(groupeditdlg.clist_avail);
	gtk_cmclist_freeze(groupeditdlg.clist_group);
	gtk_cmclist_unselect_all(groupeditdlg.clist_avail);
	while (selected) {
		edit_group_move_email( groupeditdlg.clist_group,
					groupeditdlg.clist_avail, GTK_CMCTREE_NODE(selected->data) );
		selected = selected->next;
	}
	g_list_free(selected);
	gtk_cmclist_thaw(groupeditdlg.clist_avail);
	gtk_cmclist_thaw(groupeditdlg.clist_group);
}

static gboolean edit_group_list_group_button( GtkCMCList *clist, GdkEventButton *event, gpointer data ) {
	if( ! event ) return FALSE;

	if( event->button == 1 ) {
		if( event->type == GDK_2BUTTON_PRESS ) {
			edit_group_to_avail( NULL, NULL );
		}
	}
	return FALSE;
}

static gboolean edit_group_list_avail_button( GtkCMCList *clist, GdkEventButton *event, gpointer data ) {
	if( ! event ) return FALSE;

	if( event->button == 1 ) {
		if( event->type == GDK_2BUTTON_PRESS ) {
			edit_group_to_group( NULL, NULL );
		}
	}
	return FALSE;
}

static gint edit_group_list_compare_func( GtkCMCList *clist, gconstpointer ptr1, gconstpointer ptr2 ) {
	GtkCMCell *cell1 = ((GtkCMCListRow *)ptr1)->cell;
	GtkCMCell *cell2 = ((GtkCMCListRow *)ptr2)->cell;
	gchar *name1 = NULL, *name2 = NULL;
	if( cell1 ) name1 = cell1->u.text;
	if( cell2 ) name2 = cell2->u.text;
	if( ! name1 ) return ( name2 != NULL );
	if( ! name2 ) return -1;
	return g_utf8_collate( name1, name2 );
}

static void addressbook_edit_group_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.addressbookeditgroupwin_width = allocation->width;
	prefs_common.addressbookeditgroupwin_height = allocation->height;
}

static void addressbook_edit_group_create( gboolean *cancelled ) {
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *hbbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *hsbox;
	GtkWidget *statusbar;

	GtkWidget *hboxg;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *entry_name;
	GtkWidget *hboxl;
	GtkWidget *vboxl;
	GtkWidget *hboxh;

	GtkWidget *clist_swin;
	GtkWidget *clist_group;
	GtkWidget *clist_avail;

	GtkWidget *buttonGroup;
	GtkWidget *buttonAvail;
	gint top;

	gchar *titles[ GROUP_N_COLS ];
	gint i;

	static GdkGeometry geometry;

	titles[ GROUP_COL_NAME    ] = _( "Name" );
	titles[ GROUP_COL_EMAIL   ] = _("Email Address");
	titles[ GROUP_COL_REMARKS ] = _("Remarks");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "editgroup");
	gtk_container_set_border_width(GTK_CONTAINER(window), 0);
	gtk_window_set_title(GTK_WINDOW(window), _("Edit Group Data"));
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(edit_group_delete_event),
			 cancelled);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(edit_group_key_pressed),
			 cancelled);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(addressbook_edit_group_size_allocate_cb), NULL);

	vbox = gtk_vbox_new( FALSE, 6 );
	gtk_container_set_border_width(GTK_CONTAINER(vbox), BORDER_WIDTH);
	gtk_widget_show( vbox );
	gtk_container_add( GTK_CONTAINER( window ), vbox );

	/* Group area */
	hboxg = gtk_hbox_new( FALSE, 0 );
	gtk_box_pack_start(GTK_BOX(vbox), hboxg, FALSE, FALSE, 0);

	/* Data entry area */
	table = gtk_table_new( 1, 3, FALSE);
	gtk_box_pack_start(GTK_BOX(hboxg), table, TRUE, TRUE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 4 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 0);
	gtk_table_set_col_spacings(GTK_TABLE(table), 4);

	/* First row */
	top = 0;
	label = gtk_label_new(_("Group Name"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_name = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_name, 1, 2, top, (top + 1), GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	/* List area */
	hboxl = gtk_hbox_new( FALSE, 6 );
	gtk_container_set_border_width( GTK_CONTAINER(hboxl), 8 );
	gtk_box_pack_start(GTK_BOX(vbox), hboxl, TRUE, TRUE, 0);

	/* Group list */
	vboxl = gtk_vbox_new( FALSE, 0 );
	gtk_box_pack_start(GTK_BOX(hboxl), vboxl, TRUE, TRUE, 0);

	hboxh = gtk_hbox_new( FALSE, 0 );
	gtk_container_set_border_width( GTK_CONTAINER(hboxh), 4 );
	gtk_box_pack_start(GTK_BOX(vboxl), hboxh, FALSE, FALSE, 0);
	label = gtk_label_new(_("Addresses in Group"));
	gtk_box_pack_start(GTK_BOX(hboxh), label, TRUE, TRUE, 0);
	buttonAvail = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	gtk_box_pack_end(GTK_BOX(hboxh), buttonAvail, FALSE, FALSE, 0);

	clist_swin = gtk_scrolled_window_new( NULL, NULL );
	gtk_box_pack_start(GTK_BOX(vboxl), clist_swin, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(clist_swin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	clist_group = gtk_sctree_new_with_titles( GROUP_N_COLS, 0, titles );
	gtk_container_add( GTK_CONTAINER(clist_swin), clist_group );
	gtk_cmctree_set_line_style(GTK_CMCTREE(clist_group), GTK_CMCTREE_LINES_NONE);
	gtk_cmctree_set_expander_style(GTK_CMCTREE(clist_group),
			     GTK_CMCTREE_EXPANDER_TRIANGLE);
	gtk_sctree_set_stripes(GTK_SCTREE(clist_group), prefs_common.use_stripes_in_summaries);
	gtk_cmclist_set_selection_mode( GTK_CMCLIST(clist_group), GTK_SELECTION_EXTENDED );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_group), GROUP_COL_NAME, GROUP_COL_WIDTH_NAME );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_group), GROUP_COL_EMAIL, GROUP_COL_WIDTH_EMAIL );
	gtk_cmclist_set_compare_func( GTK_CMCLIST(clist_group), edit_group_list_compare_func );
	gtk_cmclist_set_auto_sort( GTK_CMCLIST(clist_group), TRUE );

	for( i = 0; i < GROUP_N_COLS; i++ )
		gtkut_widget_set_can_focus(GTK_CMCLIST(clist_group)->column[i].button, FALSE);

	/* Available list */
	vboxl = gtk_vbox_new( FALSE, 0 );
	gtk_box_pack_start(GTK_BOX(hboxl), vboxl, TRUE, TRUE, 0);

	hboxh = gtk_hbox_new( FALSE, 0 );
	gtk_container_set_border_width( GTK_CONTAINER(hboxh), 4 );
	gtk_box_pack_start(GTK_BOX(vboxl), hboxh, FALSE, FALSE, 0);
	buttonGroup = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(hboxh), buttonGroup, FALSE, FALSE, 0);
	label = gtk_label_new(_("Available Addresses"));
	gtk_box_pack_end(GTK_BOX(hboxh), label, TRUE, TRUE, 0);

	clist_swin = gtk_scrolled_window_new( NULL, NULL );
	gtk_box_pack_start(GTK_BOX(vboxl), clist_swin, TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(clist_swin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	clist_avail = gtk_sctree_new_with_titles( GROUP_N_COLS, 0, titles );
	gtk_container_add( GTK_CONTAINER(clist_swin), clist_avail );
	gtk_cmctree_set_line_style(GTK_CMCTREE(clist_avail), GTK_CMCTREE_LINES_NONE);
	gtk_cmctree_set_expander_style(GTK_CMCTREE(clist_avail),
			     GTK_CMCTREE_EXPANDER_TRIANGLE);
	gtk_cmclist_set_selection_mode( GTK_CMCLIST(clist_avail), GTK_SELECTION_EXTENDED );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_avail), GROUP_COL_NAME, GROUP_COL_WIDTH_NAME );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_avail), GROUP_COL_EMAIL, GROUP_COL_WIDTH_EMAIL );
	gtk_cmclist_set_compare_func( GTK_CMCLIST(clist_avail), edit_group_list_compare_func );
	gtk_cmclist_set_auto_sort( GTK_CMCLIST(clist_avail), TRUE );

	for( i = 0; i < GROUP_N_COLS; i++ )
		gtkut_widget_set_can_focus(GTK_CMCLIST(clist_avail)->column[i].button, FALSE);

	/* Status line */
	hsbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), hsbox, FALSE, FALSE, BORDER_WIDTH);
	statusbar = gtk_statusbar_new();
	gtk_box_pack_start(GTK_BOX(hsbox), statusbar, TRUE, TRUE, BORDER_WIDTH);

	/* Button panel */
	gtkut_stock_button_set_create(&hbbox, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), hbbox, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(edit_group_ok), cancelled);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(edit_group_cancel), cancelled);

	gtk_widget_show_all(vbox);

	/* Event handlers */
	g_signal_connect( G_OBJECT(buttonGroup), "clicked",
			  G_CALLBACK( edit_group_to_group ), NULL );
	g_signal_connect( G_OBJECT(buttonAvail), "clicked",
			  G_CALLBACK( edit_group_to_avail ), NULL );
	g_signal_connect(G_OBJECT(clist_avail), "button_press_event",
			 G_CALLBACK(edit_group_list_avail_button), NULL);
	g_signal_connect(G_OBJECT(clist_group), "button_press_event",
			 G_CALLBACK(edit_group_list_group_button), NULL);

	if (!geometry.min_height) {
		geometry.min_width = 580;
		geometry.min_height = 340;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window,
					prefs_common.addressbookeditgroupwin_width,
				    prefs_common.addressbookeditgroupwin_height);

	groupeditdlg.window     = window;
	groupeditdlg.ok_btn     = ok_btn;
	groupeditdlg.cancel_btn = cancel_btn;
	groupeditdlg.statusbar  = statusbar;
	groupeditdlg.status_cid = gtk_statusbar_get_context_id( GTK_STATUSBAR(statusbar), "Edit Group Dialog" );

	groupeditdlg.entry_name  = entry_name;
	groupeditdlg.clist_group = GTK_CMCLIST( clist_group );
	groupeditdlg.clist_avail = GTK_CMCLIST( clist_avail );

	if( ! _edit_group_dfl_message_ ) {
		_edit_group_dfl_message_ = _( "Move Email Addresses to or from Group with arrow buttons" );
	}
}

/*
* Return list of email items.
*/
static GList *edit_group_build_email_list() {
	GtkCMCList *clist = GTK_CMCLIST(groupeditdlg.clist_group);
	GList *listEMail = NULL;
	ItemEMail *email;
	gint row = 0;
	while( (email = gtk_cmclist_get_row_data( clist, row )) ) {
		listEMail = g_list_append( listEMail, email );
		row++;
	}
	return listEMail;
}

/*
* Edit group.
* Enter: abf    Address book.
*        folder Parent folder for group (or NULL if adding to root folder). Argument is
*               only required for new objects).
*        group  Group to edit, or NULL for a new group object.
* Return: Edited object, or NULL if cancelled.
*/
ItemGroup *addressbook_edit_group( AddressBookFile *abf, ItemFolder *parent, ItemGroup *group ) {
	static gboolean cancelled;
	GList *listEMail = NULL;
	gchar *name;

	if (!groupeditdlg.window)
		addressbook_edit_group_create(&cancelled);
	gtk_widget_grab_focus(groupeditdlg.ok_btn);
	gtk_widget_grab_focus(groupeditdlg.entry_name);
	gtk_widget_show(groupeditdlg.window);
	manage_window_set_transient(GTK_WINDOW(groupeditdlg.window));
	gtk_window_set_modal(GTK_WINDOW(groupeditdlg.window), TRUE);
	/* Clear all fields */
	edit_group_status_show( "" );
	gtk_cmclist_clear( GTK_CMCLIST(groupeditdlg.clist_group) );
	gtk_cmclist_clear( GTK_CMCLIST(groupeditdlg.clist_avail) );

	if( group ) {
		if( ADDRITEM_NAME(group) )
			gtk_entry_set_text(GTK_ENTRY(groupeditdlg.entry_name), ADDRITEM_NAME(group) );
		edit_group_load_clist( groupeditdlg.clist_group, group->listEMail );
		gtk_window_set_title( GTK_WINDOW(groupeditdlg.window), _("Edit Group Details"));
	}
	else {
		gtk_window_set_title( GTK_WINDOW(groupeditdlg.window), _("Add New Group"));
		gtk_entry_set_text(GTK_ENTRY(groupeditdlg.entry_name), ADDRESSBOOK_GUESS_GROUP_NAME );
	}

	listEMail = addrbook_get_available_email_list( abf, group );
	edit_group_load_clist( groupeditdlg.clist_avail, listEMail );
	mgu_clear_list( listEMail );
	listEMail = NULL;
	gtk_cmclist_select_row( groupeditdlg.clist_group, 0, 0 );
	gtk_cmclist_select_row( groupeditdlg.clist_avail, 0, 0 );

	edit_group_status_show( _edit_group_dfl_message_ );

	gtk_main();
	gtk_widget_hide( groupeditdlg.window );
	gtk_window_set_modal(GTK_WINDOW(groupeditdlg.window), FALSE);
	if( cancelled ) {
		return NULL;
	}

	listEMail = edit_group_build_email_list();
	if( group ) {
		/* Update email list */
		addrbook_update_group_list( abf, group, listEMail );
	}
	else {
		/* Create new person and email list */
		group = addrbook_add_group_list( abf, parent, listEMail );
	}
	name = gtk_editable_get_chars( GTK_EDITABLE(groupeditdlg.entry_name), 0, -1 );
	addritem_group_set_name( group, name );
	g_free( name );

	listEMail = NULL;
	return group;
}

/*
* Edit folder.
* Enter: abf    Address book.
*        parent Parent folder for folder (or NULL if adding to root folder). Argument is
*               only required for new objects).
*        folder	Folder to edit, or NULL for a new folder object.
* Return: Edited object, or NULL if cancelled.
*/
ItemFolder *addressbook_edit_folder( AddressBookFile *abf, ItemFolder *parent, ItemFolder *folder ) {
	gchar *name = NULL;

	if( folder ) {
		name = g_strdup( ADDRITEM_NAME(folder) );
		name = input_dialog( _("Edit folder"), _("Input the new name of folder:"), name );
	}
	else {
		name = input_dialog( _("New folder"),
				_("Input the name of new folder:"),
				_(ADDRESSBOOK_GUESS_FOLDER_NAME) );
	}
	if( ! name ) return NULL;
	g_strstrip( name );
	if( *name == '\0' ) {
		g_free( name );
		return NULL;
	}
	if( folder ) {
		if( strcmp( name, ADDRITEM_NAME(folder) ) == 0 ) {
			g_free( name );
			return NULL;
		}
	}

	if( ! folder ) {
		folder = addrbook_add_new_folder( abf, parent );
	}
	addritem_folder_set_name( folder, name );
	g_free( name );
	return folder;
}

/*
* End of Source.
*/

