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
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <string.h>
#include <setjmp.h>
#include <sys/types.h>
#include <dirent.h>

#include "main.h"
#include "addressbook.h"
#include "manage_window.h"
#include "prefs_common.h"
#include "alertpanel.h"
#include "inputdialog.h"
#include "menu.h"
#include "stock_pixmap.h"
#include "xml.h"
#include "prefs_gtk.h"
#include "procmime.h"
#include "utils.h"
#include "gtkutils.h"
#include "codeconv.h"
#include "about.h"
#include "addr_compl.h"

#include "mgutils.h"
#include "addressitem.h"
#include "addritem.h"
#include "addrcache.h"
#include "addrbook.h"
#include "addrindex.h"
#include "addressadd.h"
#include "addrduplicates.h"
#include "addressbook_foldersel.h"
#include "vcard.h"
#include "editvcard.h"
#include "editgroup.h"
#include "editaddress.h"
#include "editbook.h"
#include "importldif.h"
#include "importmutt.h"
#include "importpine.h"
#include "manual.h"

#ifdef USE_JPILOT
#include "jpilot.h"
#include "editjpilot.h"
#endif

#ifdef USE_LDAP
#include <pthread.h>
#include "ldapserver.h"
#include "editldap.h"
#include "ldapupdate.h"

#define ADDRESSBOOK_LDAP_BUSYMSG "Busy"
#endif

#include "addrquery.h"
#include "addrselect.h"
#include "addrclip.h"
#include "addrgather.h"
#include "adbookbase.h"
#include "exphtmldlg.h"
#include "expldifdlg.h"
#include "browseldap.h"
#include "addrcustomattr.h"
#ifdef G_OS_WIN32
#undef interface
#endif
typedef enum
{
	COL_SOURCES	= 0,
	N_INDEX_COLS	= 1
} AddressIndexColumns;

typedef enum
{
	COL_NAME	= 0,
	COL_ADDRESS	= 1,
	COL_REMARKS	= 2,
	N_LIST_COLS	= 3
} AddressListColumns;

typedef struct {
	AddressBookFile	*book;
	ItemFolder	*folder;
} FolderInfo;

typedef struct {
	gchar **folder_path;
	gboolean matched;
	gint index;
	AddressDataSource *book;
	ItemFolder *folder;
} FolderPathMatch;

static gchar *list_titles[] = { N_("Name"),
                                N_("Email Address"),
                                N_("Remarks") };

#define COL_NAME_WIDTH		164
#define COL_ADDRESS_WIDTH	156

#define COL_FOLDER_WIDTH	170
#define ADDRESSBOOK_WIDTH	640
#define ADDRESSBOOK_HEIGHT	360

#define ADDRESSBOOK_MSGBUF_SIZE 2048

static GdkPixbuf *folderxpm = NULL;
static GdkPixbuf *folderopenxpm = NULL;
static GdkPixbuf *groupxpm = NULL;
static GdkPixbuf *interfacexpm = NULL;
static GdkPixbuf *bookxpm = NULL;
static GdkPixbuf *addressxpm = NULL;
static GdkPixbuf *vcardxpm = NULL;
static GdkPixbuf *jpilotxpm = NULL;
static GdkPixbuf *categoryxpm = NULL;
static GdkPixbuf *ldapxpm = NULL;
static GdkPixbuf *addrsearchxpm = NULL;

/* Message buffer */
static gchar addressbook_msgbuf[ ADDRESSBOOK_MSGBUF_SIZE ];

/* Address list selection */
static AddrSelectList *_addressSelect_ = NULL;
static AddressClipboard *_clipBoard_ = NULL;

/* Address index file and interfaces */
static AddressIndex *_addressIndex_ = NULL;
static GList *_addressInterfaceList_ = NULL;
static GList *_addressIFaceSelection_ = NULL;
#define ADDRESSBOOK_IFACE_SELECTION "1/y,3/y,4/y,2/n"

static AddressBook_win addrbook;

static GHashTable *_addressBookTypeHash_ = NULL;
static GList *_addressBookTypeList_ = NULL;

static void addressbook_new_address_from_book_post_cb( ItemPerson *person );
static void addressbook_new_address_from_folder_post_cb( ItemPerson *person );
static void addressbook_edit_address_post_cb( ItemPerson *person );

static void addressbook_create			(void);
static gint addressbook_close			(void);

static gboolean address_index_has_focus = FALSE;
static gboolean address_list_has_focus = FALSE;

/* callback functions */
static void addressbook_del_clicked		(GtkButton	*button,
						 gpointer	 data);
static void addressbook_reg_clicked		(GtkButton	*button,
						 gpointer	 data);
static void addressbook_to_clicked		(GtkButton	*button,
						 gpointer	 data);
static void addressbook_lup_clicked		(GtkButton	*button,
						 gpointer	data);
static void addressbook_close_clicked		(GtkButton	*button,
						 gpointer	data);

static void addressbook_tree_selected		(GtkCMCTree	*ctree,
						 GtkCMCTreeNode	*node,
						 gint		 column,
						 gpointer	 data);
static void addressbook_select_row_tree		(GtkCMCTree	*ctree,
						 GtkCMCTreeNode	*node,
						 gint		 column,
						 gpointer	 data);
static void addressbook_list_row_selected	(GtkCMCTree	*clist,
						 GtkCMCTreeNode	*node,
						 gint		 column,
						 gpointer	 data);
static void addressbook_list_row_unselected	(GtkCMCTree	*clist,
						 GtkCMCTreeNode	*node,
						 gint		 column,
						 gpointer	 data);
static void addressbook_person_expand_node	(GtkCMCTree	*ctree,
						 GList		*node,
						 gpointer	*data );
static void addressbook_person_collapse_node	(GtkCMCTree	*ctree,
						 GList		*node,
						 gpointer	*data );

static gboolean addressbook_list_button_pressed	(GtkWidget	*widget,
						 GdkEventButton	*event,
						 gpointer	 data);
static gboolean addressbook_list_button_released(GtkWidget	*widget,
						 GdkEventButton	*event,
						 gpointer	 data);
static gboolean addressbook_tree_button_pressed	(GtkWidget	*ctree,
						 GdkEventButton	*event,
						 gpointer	 data);
static gboolean addressbook_tree_button_released(GtkWidget	*ctree,
						 GdkEventButton	*event,
						 gpointer	 data);

static void addressbook_new_folder_cb		(GtkAction	*action,
						 gpointer	 data);
static void addressbook_new_group_cb		(GtkAction	*action,
						 gpointer	 data);
static void addressbook_treenode_edit_cb	(GtkAction	*action,
						 gpointer	 data);
static void addressbook_treenode_delete_cb	(GtkAction	*action,
						 gpointer	 data);

static void addressbook_change_node_name	(GtkCMCTreeNode	*node,
						 const gchar	*name);

static void addressbook_new_address_cb		(GtkAction	*action,
						 gpointer	 data);
static void addressbook_edit_address_cb		(GtkAction	*action,
						 gpointer	 data);
static void addressbook_delete_address_cb	(GtkAction	*action,
						 gpointer	 data);

static void close_cb				(GtkAction	*action,
						 gpointer	 data);
static void addressbook_file_save_cb		(GtkAction	*action,
						 gpointer	 data);

/* Data source edit stuff */
static void addressbook_new_book_cb		(GtkAction	*action,
						 gpointer	 data);
static void addressbook_new_vcard_cb		(GtkAction	*action,
						 gpointer	 data);

#ifdef USE_JPILOT
static void addressbook_new_jpilot_cb		(GtkAction	*action,
						 gpointer	 data);
#endif

#ifdef USE_LDAP
static void addressbook_new_ldap_cb		(GtkAction	*action,
						 gpointer	 data);
#endif

static void addressbook_set_clist		(AddressObject	*obj,
						 gboolean 	 refresh);

static void addressbook_load_tree		(void);
void addressbook_read_file			(void);

static GtkCMCTreeNode *addressbook_add_object	(GtkCMCTreeNode	*node,
						 AddressObject	*obj);
static void addressbook_treenode_remove_item	( void );

static AddressDataSource *addressbook_find_datasource
						(GtkCMCTreeNode	*node );

static AddressBookFile *addressbook_get_book_file(void);

static GtkCMCTreeNode *addressbook_node_add_folder
						(GtkCMCTreeNode	*node,
						AddressDataSource *ds,
						ItemFolder	*itemFolder,
						AddressObjectType otype);
static GtkCMCTreeNode *addressbook_node_add_group (GtkCMCTreeNode	*node,
						AddressDataSource *ds,
						ItemGroup	*itemGroup);
static void addressbook_tree_remove_children	(GtkCMCTree	*ctree,
						GtkCMCTreeNode	*parent);
static void addressbook_move_nodes_up		(GtkCMCTree	*ctree,
						GtkCMCTreeNode	*node);
static GtkCMCTreeNode *addressbook_find_group_node (GtkCMCTreeNode	*parent,
						   ItemGroup	*group);
static gboolean addressbook_entry_key_pressed	(GtkWidget	*widget,
						 GdkEventKey	*event,
						 gpointer	 data);
static gint addressbook_treenode_compare_func	(GtkCMCList	*clist,
						 gconstpointer	 ptr1,
						 gconstpointer	 ptr2);
static void addressbook_folder_load_one_person	(GtkCMCTree *clist, 
						 ItemPerson *person,  
						 AddressTypeControlItem *atci, 
						 AddressTypeControlItem *atciMail);
static void addressbook_folder_refresh_one_person(GtkCMCTree *clist, 
						  ItemPerson *person);
static void addressbook_folder_remove_one_person(GtkCMCTree *clist, 
						 ItemPerson *person);
static void addressbook_folder_remove_node	(GtkCMCTree *clist, 
						 GtkCMCTreeNode *node);

static void addressbook_edit_address( gpointer data, guint action, GtkWidget *widget,
									  gboolean force_focus );

/* LUT's and IF stuff */
static void addressbook_free_treenode		( gpointer data );
static AddressTypeControlItem *addrbookctl_lookup	(gint		 ot);
static AddressTypeControlItem *addrbookctl_lookup_iface(AddressIfType	 ifType);

static void addrbookctl_build_map			(GtkWidget	*window);
static void addrbookctl_build_iflist			(void);
static AdapterInterface *addrbookctl_find_interface	(AddressIfType	 ifType);
static void addrbookctl_build_ifselect			(void);

static void addrbookctl_free_interface		(AdapterInterface *adapter);
static void addrbookctl_free_datasource		(AdapterDSource	  *adapter);
static void addrbookctl_free_folder		(AdapterFolder	  *adapter);
static void addrbookctl_free_group		(AdapterGroup	  *adapter);

static void addressbook_list_select_clear	( void );
static void addressbook_list_select_add		( AddrItemObject    *aio,
						  AddressDataSource *ds );
static void addressbook_list_select_remove	( AddrItemObject    *aio );

static void addressbook_import_ldif_cb		( GtkAction *action, gpointer data );
static void addressbook_find_duplicates_cb	( GtkAction *action, gpointer data );
static void addressbook_edit_custom_attr_cb	( GtkAction *action, gpointer data );
static void addressbook_import_mutt_cb		( GtkAction *action, gpointer data );
static void addressbook_import_pine_cb		( GtkAction *action, gpointer data );
static void addressbook_export_html_cb		( GtkAction *action, gpointer data );
static void addressbook_export_ldif_cb		( GtkAction *action, gpointer data );
static void addressbook_select_all_cb		( GtkAction *action, gpointer data );
static void addressbook_clip_cut_cb		( GtkAction *action, gpointer data );
static void addressbook_clip_copy_cb		( GtkAction *action, gpointer data );
static void addressbook_clip_paste_cb		( GtkAction *action, gpointer data );
static void addressbook_treenode_cut_cb		( GtkAction *action, gpointer data );
static void addressbook_treenode_copy_cb	( GtkAction *action, gpointer data );
static void addressbook_treenode_paste_cb	( GtkAction *action, gpointer data );

static void addressbook_mail_to_cb		( GtkAction *action, gpointer data );

#ifdef USE_LDAP
static void addressbook_browse_entry_cb		( GtkAction *action, gpointer data );
#endif
static void addressbook_edit_clicked(GtkButton *button, gpointer data);

static void addressbook_start_drag(GtkWidget *widget, gint button, 
				   GdkEvent *event,
			           void *data);
static void addressbook_drag_data_get(GtkWidget        *widget,
				     GdkDragContext   *drag_context,
				     GtkSelectionData *selection_data,
				     guint             info,
				     guint             time,
				     void	      *data);
static gboolean addressbook_drag_motion_cb(GtkWidget      *widget,
					  GdkDragContext *context,
					  gint            x,
					  gint            y,
					  guint           time,
					  void           *data);
static void addressbook_drag_leave_cb(GtkWidget      *widget,
				     GdkDragContext *context,
				     guint           time,
				     void           *data);
static void addressbook_drag_received_cb(GtkWidget        *widget,
					GdkDragContext   *drag_context,
					gint              x,
					gint              y,
					GtkSelectionData *data,
					guint             info,
					guint             time,
					void             *pdata);
static void addressbook_list_menu_setup( void );

static GtkTargetEntry addressbook_drag_types[] =
{
	{"claws-mail/internal", GTK_TARGET_SAME_APP, TARGET_DUMMY}
};

static GtkTargetList *addressbook_target_list = NULL;

static void about_show_cb(GtkAction *action, gpointer data)
{
	about_show();
}

static GtkActionEntry addressbook_entries[] =
{
	{"Menu",				NULL, "Menu" },
/* menus */
	{"Book",			NULL, N_("_Book") },
	{"Address",			NULL, N_("_Edit") },
	{"Tools",			NULL, N_("_Tools") },
	{"Help",			NULL, N_("_Help") },
	
/* Book menu */
	{"Book/NewBook",		NULL, N_("New _Book"), "<control>B", NULL, G_CALLBACK(addressbook_new_book_cb) },
	{"Book/NewFolder",		NULL, N_("New _Folder"), "<control>R", NULL, G_CALLBACK(addressbook_new_folder_cb) },
	{"Book/NewVCard",		NULL, N_("New _vCard"), "<control><shift>D", NULL, G_CALLBACK(addressbook_new_vcard_cb) },


#ifdef USE_JPILOT
	{"Book/NewJPilot",		NULL, N_("New _JPilot"), "<control>J", NULL, G_CALLBACK(addressbook_new_jpilot_cb) },
#endif
#ifdef USE_LDAP
	{"Book/NewLDAPServer",		NULL, N_("New LDAP _Server"), "<control><shift>S", NULL, G_CALLBACK(addressbook_new_ldap_cb) },
#endif
	{"Book/---",			NULL, "---", NULL, NULL, NULL },

	{"Book/EditBook",		NULL, N_("_Edit book"), NULL, NULL, G_CALLBACK(addressbook_treenode_edit_cb) },
	{"Book/DeleteBook",		NULL, N_("_Delete book"), NULL, NULL, G_CALLBACK(addressbook_treenode_delete_cb) },
	/* {"Book/---",			NULL, "---", NULL, NULL, NULL }, */
	{"Book/Save",			NULL, N_("_Save"), "<control>S", NULL, G_CALLBACK(addressbook_file_save_cb) },
	{"Book/Close",			NULL, N_("_Close"), "<control>W", NULL, G_CALLBACK(close_cb) },

/* Adress menu */
	{"Address/SelectAll",		NULL, N_("_Select all"), "<control>A", NULL, G_CALLBACK(addressbook_select_all_cb) },
	{"Address/---",			NULL, "---", NULL, NULL, NULL },
	{"Address/Cut",			NULL, N_("C_ut"), "<control>X", NULL, G_CALLBACK(addressbook_clip_cut_cb) },
	{"Address/Copy",		NULL, N_("_Copy"), "<control>C", NULL, G_CALLBACK(addressbook_clip_copy_cb) },
	{"Address/Paste",		NULL, N_("_Paste"), "<control>V", NULL, G_CALLBACK(addressbook_clip_paste_cb) },
	/* {"Address/---",			NULL, "---", NULL, NULL, NULL }, */
	{"Address/Edit",		NULL, N_("_Edit"), "<control>Return", NULL, G_CALLBACK(addressbook_edit_address_cb) },
	{"Address/Delete",		NULL, N_("_Delete"), "<control>D", NULL, G_CALLBACK(addressbook_delete_address_cb) },
	/* {"Address/---",			NULL, "---", NULL, NULL, NULL }, */
	{"Address/NewAddress",		NULL, N_("New _Address"), "<control>N", NULL, G_CALLBACK(addressbook_new_address_cb) },
	{"Address/NewGroup",		NULL, N_("New _Group"), "<control>G", NULL, G_CALLBACK(addressbook_new_group_cb) },
	/* {"Address/---",			NULL, "---", NULL, NULL, NULL }, */
	{"Address/Mailto",		NULL, N_("_Mail To"), "<control>M", NULL, G_CALLBACK(addressbook_mail_to_cb) },


/* Tools menu */
	{"Tools/ImportLDIF",		NULL, N_("Import _LDIF file..."), NULL, NULL, G_CALLBACK(addressbook_import_ldif_cb) },
	{"Tools/ImportMutt",		NULL, N_("Import M_utt file..."), NULL, NULL, G_CALLBACK(addressbook_import_mutt_cb) },
	{"Tools/ImportPine",		NULL, N_("Import _Pine file..."), NULL, NULL, G_CALLBACK(addressbook_import_pine_cb) },
	{"Tools/---",			NULL, "---", NULL, NULL, NULL },
	{"Tools/ExportHTML",		NULL, N_("Export _HTML..."), NULL, NULL, G_CALLBACK(addressbook_export_html_cb) },
	{"Tools/ExportLDIF",		NULL, N_("Export LDI_F..."), NULL, NULL, G_CALLBACK(addressbook_export_ldif_cb) },
	/* {"Tools/---",			NULL, "---", NULL, NULL, NULL },*/
	{"Tools/FindDuplicates",	NULL, N_("Find duplicates..."), NULL, NULL, G_CALLBACK(addressbook_find_duplicates_cb) },
	{"Tools/EditAttrs",		NULL, N_("Edit custom attributes..."), NULL, NULL, G_CALLBACK(addressbook_edit_custom_attr_cb) },

/* Help menu */
	{"Help/About",			NULL, N_("_About"), NULL, NULL, G_CALLBACK(about_show_cb) }, 

};

static GtkActionEntry addressbook_tree_popup_entries[] =
{
	{"ABTreePopup",			NULL, "ABTreePopup" },
	{"ABTreePopup/EditBook",	NULL, N_("_Edit"), NULL, NULL, G_CALLBACK(addressbook_treenode_edit_cb) },
	{"ABTreePopup/DeleteBook",	NULL, N_("_Delete"), NULL, NULL, G_CALLBACK(addressbook_treenode_delete_cb) },
	{"ABTreePopup/---",		NULL, "---", NULL, NULL, NULL },
	{"ABTreePopup/NewBook",		NULL, N_("New _Book"), NULL, NULL, G_CALLBACK(addressbook_new_book_cb) },
	{"ABTreePopup/NewFolder",	NULL, N_("New _Folder"), NULL, NULL, G_CALLBACK(addressbook_new_folder_cb) },
	{"ABTreePopup/NewGroup",	NULL, N_("New _Group"), NULL, NULL, G_CALLBACK(addressbook_new_group_cb) },
	/* {"ABTreePopup/---",		NULL, "---", NULL, NULL, NULL }, */
	{"ABTreePopup/Cut",		NULL, N_("C_ut"), NULL, NULL, G_CALLBACK(addressbook_treenode_cut_cb) },
	{"ABTreePopup/Copy",		NULL, N_("_Copy"), NULL, NULL, G_CALLBACK(addressbook_treenode_copy_cb) },
	{"ABTreePopup/Paste",		NULL, N_("_Paste"), NULL, NULL, G_CALLBACK(addressbook_treenode_paste_cb) },
};

static GtkActionEntry addressbook_list_popup_entries[] =
{
	{"ABListPopup",			NULL, "ABListPopup" },
	{"ABListPopup/SelectAll",	NULL, N_("_Select all"), NULL, NULL, G_CALLBACK(addressbook_select_all_cb) },
	{"ABListPopup/---",		NULL, "---", NULL, NULL, NULL },
	{"ABListPopup/Edit",		NULL, N_("_Edit"), NULL, NULL, G_CALLBACK(addressbook_edit_address_cb) },
	{"ABListPopup/Delete",		NULL, N_("_Delete"), NULL, NULL, G_CALLBACK(addressbook_delete_address_cb) },
	/* {"ABListPopup/---",		NULL, "---", NULL, NULL, NULL }, */
	{"ABListPopup/NewAddress",	NULL, N_("New _Address"), NULL, NULL, G_CALLBACK(addressbook_new_address_cb) },
	{"ABListPopup/NewGroup",	NULL, N_("New _Group"), NULL, NULL, G_CALLBACK(addressbook_new_group_cb) },
	/* {"ABListPopup/---",		NULL, "---", NULL, NULL, NULL }, */
	{"ABListPopup/Cut",		NULL, N_("C_ut"), NULL, NULL, G_CALLBACK(addressbook_clip_cut_cb) },
	{"ABListPopup/Copy",		NULL, N_("_Copy"), NULL, NULL, G_CALLBACK(addressbook_clip_copy_cb) },
	{"ABListPopup/Paste",		NULL, N_("_Paste"), NULL, NULL, G_CALLBACK(addressbook_clip_paste_cb) },
	/* {"ABListPopup/---",		NULL, "---", NULL, NULL, NULL }, */
	{"ABListPopup/Mailto",		NULL, N_("_Mail To"), NULL, NULL, G_CALLBACK(addressbook_mail_to_cb) },
#ifdef USE_LDAP
	{"ABListPopup/BrowseEntry",	NULL, N_("_Browse Entry"), NULL, NULL, G_CALLBACK(addressbook_browse_entry_cb) },
#endif
};

/**
 * Structure of error message table.
 */
typedef struct _ErrMsgTableEntry ErrMsgTableEntry;
struct _ErrMsgTableEntry {
	gint	code;
	gchar	*description;
};

static gchar *_errMsgUnknown_ = N_( "Unknown" );

/**
 * Lookup table of error messages for general errors. Note that a NULL
 * description signifies the end of the table.
 */
static ErrMsgTableEntry _lutErrorsGeneral_[] = {
	{ MGU_SUCCESS,		N_("Success") },
	{ MGU_BAD_ARGS,		N_("Bad arguments") },
	{ MGU_NO_FILE,		N_("File not specified") },
	{ MGU_OPEN_FILE,	N_("Error opening file") },
	{ MGU_ERROR_READ,	N_("Error reading file") },
	{ MGU_EOF,		N_("End of file encountered") },
	{ MGU_OO_MEMORY,	N_("Error allocating memory") },
	{ MGU_BAD_FORMAT,	N_("Bad file format") },
	{ MGU_ERROR_WRITE,	N_("Error writing to file") },
	{ MGU_OPEN_DIRECTORY,	N_("Error opening directory") },
	{ MGU_NO_PATH,      	N_("No path specified") },
	{ 0,			NULL }
};

#ifdef USE_LDAP
/**
 * Lookup table of error messages for LDAP errors.
 */
static ErrMsgTableEntry _lutErrorsLDAP_[] = {
	{ LDAPRC_SUCCESS,			N_("Success") },
	{ LDAPRC_CONNECT,			N_("Error connecting to LDAP server") },
	{ LDAPRC_INIT,				N_("Error initializing LDAP") },
	{ LDAPRC_BIND,				N_("Error binding to LDAP server") },
	{ LDAPRC_SEARCH,			N_("Error searching LDAP database") },
	{ LDAPRC_TIMEOUT,			N_("Timeout performing LDAP operation") },
	{ LDAPRC_CRITERIA,			N_("Error in LDAP search criteria") },
	{ LDAPRC_NOENTRIES,			N_("No LDAP entries found for search criteria") },
	{ LDAPRC_STOP_FLAG,			N_("LDAP search terminated on request") },
	{ LDAPRC_TLS,				N_("Error starting TLS connection") },
	{ LDAPRC_NODN,				N_("Distinguished Name (dn) is missing") },
	{ LDAPRC_NAMING_VIOLATION,		N_("Missing required information") },
	{ LDAPRC_ALREADY_EXIST,			N_("Another contact exists with that key") },
	{ LDAPRC_STRONG_AUTH,			N_("Strong(er) authentication required") },
	{ 0,					NULL }
};
#endif

/**
 * Lookup message for specified error code.
 * \param lut  Lookup table.
 * \param code Code to lookup.
 * \return Description associated to code.
 */
static gchar *addressbook_err2string( ErrMsgTableEntry lut[], gint code ) {
        gchar *desc = NULL;
        ErrMsgTableEntry entry;
        gint i;

        for( i = 0; ; i++ ) {
                entry = lut[ i ];
                if( entry.description == NULL ) break;
                if( entry.code == code ) {
                        desc = entry.description;
                        break;
                }
        }
        if( ! desc ) {
		desc = _errMsgUnknown_;
        }
        return desc;
}

static gboolean lastCanLookup = FALSE;

static void addressbook_show_buttons(gboolean add_and_delete, gboolean lookup, gboolean mail_ops)
{
	if (add_and_delete) {
		gtk_widget_show(addrbook.edit_btn);
		gtk_widget_show(addrbook.del_btn);
		gtk_widget_show(addrbook.reg_btn);
	} else {
		gtk_widget_hide(addrbook.edit_btn);
		gtk_widget_hide(addrbook.del_btn);
		gtk_widget_hide(addrbook.reg_btn);
	}
	
	if (lookup) {
		gtk_widget_show(addrbook.lup_btn);
		gtk_widget_show(addrbook.entry);
		gtk_widget_show(addrbook.label);
	} else {
		gtk_widget_hide(addrbook.lup_btn);
		gtk_widget_hide(addrbook.entry);
		gtk_widget_hide(addrbook.label);
	}

	lastCanLookup = lookup;

	if (mail_ops) {
		gtk_widget_show(addrbook.to_btn);
		gtk_widget_show(addrbook.cc_btn);
		gtk_widget_show(addrbook.bcc_btn);
	} else {
		gtk_widget_hide(addrbook.to_btn);
		gtk_widget_hide(addrbook.cc_btn);
		gtk_widget_hide(addrbook.bcc_btn);
	}
}

void addressbook_open(Compose *target)
{
	/* Initialize all static members */
	if( _clipBoard_ == NULL ) {
		_clipBoard_ = addrclip_create();
	}
	if( _addressIndex_ != NULL ) {
		addrclip_set_index( _clipBoard_, _addressIndex_ );
	}
	if( _addressSelect_ == NULL ) {
		_addressSelect_ = addrselect_list_create();
	}
	if (!addrbook.window) {
		addressbook_read_file();
		addressbook_create();
		addressbook_load_tree();
		gtk_sctree_select( GTK_SCTREE(addrbook.ctree),
				 GTK_CMCTREE_NODE(GTK_CMCLIST(addrbook.ctree)->row_list));
	}
	else {
		gtk_widget_hide(addrbook.window);
	}

	gtk_widget_show_all(addrbook.window);
#ifdef MAEMO
		maemo_window_full_screen_if_needed(GTK_WINDOW(addrbook.window));
		maemo_connect_key_press_to_mainwindow(GTK_WINDOW(addrbook.window));
#endif
	if (!prefs_common.addressbook_use_editaddress_dialog)
		addressbook_edit_person_widgetset_hide();

	address_completion_start(addrbook.window);

	addressbook_show_buttons(target == NULL, lastCanLookup, target != NULL);
	addressbook_set_target_compose(target);
}

/**
 * Destroy addressbook.
 */
void addressbook_destroy( void ) {
	/* Free up address stuff */
	if( _addressSelect_ != NULL ) {
		addrselect_list_free( _addressSelect_ );
	}
	if( _clipBoard_ != NULL ) {
		addrclip_free( _clipBoard_ );
	}
	if( _addressIndex_ != NULL ) {
		addrindex_free_index( _addressIndex_ );
		addrindex_teardown();
	}
	_addressSelect_ = NULL;
	_clipBoard_ = NULL;
	_addressIndex_ = NULL;
}

void addressbook_set_target_compose(Compose *target)
{
	addrbook.target_compose = target;
}

Compose *addressbook_get_target_compose(void)
{
	return addrbook.target_compose;
}

/**
 * Refresh addressbook and save to file(s).
 */
void addressbook_refresh( void )
{
	if (addrbook.window) {
		if (addrbook.treeSelected) {
			gtk_sctree_select( GTK_SCTREE(addrbook.ctree),
					 addrbook.treeSelected);
			addressbook_set_clist(
				gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
					addrbook.treeSelected),
				TRUE);

		}
	}
	addressbook_export_to_file();
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && event->keyval == GDK_Escape)
		addressbook_close();
	else if (event && event->keyval == GDK_Delete) {
		/* TODO: enable deletion when focus is in ctree (needs implementation in _del_clicked() */
		if ( /* address_index_has_focus || */ address_list_has_focus )
			addressbook_del_clicked(NULL, NULL);
	}
	return FALSE;
}

/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void addressbook_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.addressbookwin_width = allocation->width;
	prefs_common.addressbookwin_height = allocation->height;
}

static gint sort_column_number = 0;
static GtkSortType sort_column_type = GTK_SORT_ASCENDING;

static gint list_case_sort(
	GtkCMCList *clist, gconstpointer ptr1, gconstpointer ptr2 )
{
	GtkCMCListRow *row1 = (GtkCMCListRow *) ptr1;
	GtkCMCListRow *row2 = (GtkCMCListRow *) ptr2;
	gchar *name1 = NULL, *name2 = NULL;
	AddrItemObject *aio1 = ((GtkCMCListRow *)ptr1)->data;
	AddrItemObject *aio2 = ((GtkCMCListRow *)ptr2)->data;

	if( aio1->type == aio2->type ) {
		if( row1 ) 
			name1 = GTK_CMCELL_TEXT (row1->cell[sort_column_number])->text;
		if( row2 ) 
			name2 = GTK_CMCELL_TEXT (row2->cell[sort_column_number])->text;
		if( ! name1 ) return ( name2 != NULL );
		if( ! name2 ) return -1;
		return g_utf8_collate( name1, name2 );
	} else {
		/* Order groups before person */
		if( aio1->type == ITEMTYPE_GROUP ) {
			return (sort_column_type==GTK_SORT_ASCENDING) ? -1:+1;
		} else if( aio2->type == ITEMTYPE_GROUP ) {
			return (sort_column_type==GTK_SORT_ASCENDING) ? +1:-1;
		}
		return 0;
	}
}

static void addressbook_sort_list(GtkCMCList *clist, const gint col,
		const GtkSortType sort_type)
{
	gint pos;
	GtkWidget *hbox, *label, *arrow;

	sort_column_number = col;
	sort_column_type = sort_type;
	gtk_cmclist_set_compare_func(clist, list_case_sort);
	gtk_cmclist_set_sort_type(clist, sort_type);
	gtk_cmclist_set_sort_column(clist, col);	

	gtk_cmclist_freeze(clist);
	gtk_cmclist_sort(clist);
	
	for(pos = 0 ; pos < N_LIST_COLS ; pos++) {
		hbox = gtk_hbox_new(FALSE, 4);
		label = gtk_label_new(gettext(list_titles[pos]));
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
		
		if(pos == col) {
			arrow = gtk_arrow_new(sort_type == GTK_SORT_ASCENDING ?
				GTK_ARROW_DOWN : GTK_ARROW_UP, GTK_SHADOW_IN);
			gtk_box_pack_end(GTK_BOX(hbox), arrow, FALSE, FALSE, 0);
		}
		
		gtk_widget_show_all(hbox);
		gtk_cmclist_set_column_widget(clist, pos, hbox);
	}
	
	gtk_cmclist_thaw(clist);	
}

static void addressbook_name_clicked(GtkWidget *button, GtkCMCList *clist)
{
	static GtkSortType sort_type = GTK_SORT_ASCENDING;
	
	sort_type = (sort_type == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING :
			GTK_SORT_ASCENDING;
	addressbook_sort_list(clist, COL_NAME, sort_type);
}

static void addressbook_address_clicked(GtkWidget *button, GtkCMCList *clist)
{
	static GtkSortType sort_type = GTK_SORT_ASCENDING;

	sort_type = (sort_type == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING :
			GTK_SORT_ASCENDING;
	addressbook_sort_list(clist, COL_ADDRESS, sort_type);
}

static void addressbook_remarks_clicked(GtkWidget *button, GtkCMCList *clist)
{
	static GtkSortType sort_type = GTK_SORT_ASCENDING;

	sort_type = (sort_type == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING :
			GTK_SORT_ASCENDING;
	addressbook_sort_list(clist, COL_REMARKS, sort_type);
}

static gboolean addressbook_address_index_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
											 gpointer data)
{
	address_index_has_focus = TRUE;
	return FALSE;
}

static gboolean addressbook_address_index_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
											 gpointer data)
{
	address_index_has_focus = FALSE;
	if (!prefs_common.addressbook_use_editaddress_dialog
			&& !address_list_has_focus)
		addressbook_address_list_disable_some_actions();
	return FALSE;
}

static gboolean addressbook_address_list_focus_evt_in(GtkWidget *widget, GdkEventFocus *event,
											 gpointer data)
{
	address_list_has_focus = TRUE;
	return FALSE;
}

static gboolean addressbook_address_list_focus_evt_out(GtkWidget *widget, GdkEventFocus *event,
											 gpointer data)
{
	address_list_has_focus = FALSE;
	if (!prefs_common.addressbook_use_editaddress_dialog
			&& !address_index_has_focus)
		addressbook_address_list_disable_some_actions();
	return FALSE;
}

/* save hpane and vpane's handle position when it moves */
static void addressbook_pane_save_position(void)
{
	if (addrbook.hpaned)
		prefs_common.addressbook_hpaned_pos = 
			gtk_paned_get_position(GTK_PANED(addrbook.hpaned));
	if (addrbook.vpaned)
		prefs_common.addressbook_vpaned_pos = 
			gtk_paned_get_position(GTK_PANED(addrbook.vpaned));
}

/*
* Create the address book widgets. The address book contains two CTree widgets: the
* address index tree on the left and the address list on the right.
*
* The address index tree displays a hierarchy of interfaces and groups. Each node in
* this tree is linked to an address Adapter. Adapters have been created for interfaces,
* data sources and folder objects.
*
* The address list displays group, person and email objects. These items are linked
* directly to ItemGroup, ItemPerson and ItemEMail objects inside the address book data
* sources.
*
* In the tradition of MVC architecture, the data stores have been separated from the
* GUI components. The addrindex.c file provides the interface to all data stores.
*/
static void addressbook_create(void)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *menubar;
	GtkWidget *vbox2;
	GtkWidget *ctree_swin;
	GtkWidget *ctree;
	GtkWidget *editaddress_vbox;
	GtkWidget *clist_vbox;
	GtkWidget *clist_swin;
	GtkWidget *clist;
	GtkWidget *hpaned;
	GtkWidget *vpaned;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *entry;
	GtkWidget *statusbar;
	GtkWidget *hbbox;
	GtkWidget *hsbox;
	GtkWidget *help_btn;
	GtkWidget *del_btn;
	GtkWidget *edit_btn;
	GtkWidget *reg_btn;
	GtkWidget *lup_btn;
	GtkWidget *to_btn;
	GtkWidget *cc_btn;
	GtkWidget *bcc_btn;
	GtkWidget *close_btn;
	GtkWidget *tree_popup;
	GtkWidget *list_popup;
	GList *nodeIf;
	GtkUIManager *ui_manager;
	GtkActionGroup *action_group;
	gchar *index_titles[N_INDEX_COLS];
	gchar *text;
	gint i;

	static GdkGeometry geometry;

	debug_print("Creating addressbook window...\n");

	index_titles[COL_SOURCES] = _("Sources");

	/* Address book window */
	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "addressbook");
	gtk_window_set_title(GTK_WINDOW(window), _("Address book"));
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
	gtk_widget_realize(window);

	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(addressbook_close), NULL);
	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(addressbook_size_allocate_cb), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), NULL);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	/* Menu bar */
	ui_manager = gtk_ui_manager_new();
	action_group = cm_menu_create_action_group_full(ui_manager,"Menu", addressbook_entries,
			G_N_ELEMENTS(addressbook_entries), NULL);
	gtk_action_group_add_actions(action_group, addressbook_tree_popup_entries,
			G_N_ELEMENTS(addressbook_tree_popup_entries), NULL);
	gtk_action_group_add_actions(action_group, addressbook_list_popup_entries,
			G_N_ELEMENTS(addressbook_list_popup_entries), NULL);

#ifndef MAEMO
	MENUITEM_ADDUI_MANAGER(ui_manager, "/", "Menu", NULL, GTK_UI_MANAGER_MENUBAR)
#else
	MENUITEM_ADDUI_MANAGER(ui_manager, "/", "Menu", NULL, GTK_UI_MANAGER_POPUP)
#endif

	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu", "Book", "Book", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu", "Address", "Address", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu", "Tools", "Tools", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu", "Help", "Help", GTK_UI_MANAGER_MENU)

/* Book menu */
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "NewBook", "Book/NewBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "NewFolder", "Book/NewFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "NewVCard", "Book/NewVCard", GTK_UI_MANAGER_MENUITEM)
#ifdef USE_JPILOT
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "NewJPilot", "Book/NewJPilot", GTK_UI_MANAGER_MENUITEM)
#endif
#ifdef USE_LDAP
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "NewLDAPServer", "Book/NewLDAPServer", GTK_UI_MANAGER_MENUITEM)
#endif
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "Separator1", "Book/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "EditBook", "Book/EditBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "DeleteBook", "Book/DeleteBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "Separator2", "Book/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "Save", "Book/Save", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Book", "Close", "Book/Close", GTK_UI_MANAGER_MENUITEM)

/* Address menu */
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "SelectAll", "Address/SelectAll", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Separator1", "Address/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Cut", "Address/Cut", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Copy", "Address/Copy", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Paste", "Address/Paste", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Separator2", "Address/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Edit", "Address/Edit", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Delete", "Address/Delete", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Separator3", "Address/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "NewAddress", "Address/NewAddress", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "NewGroup", "Address/NewGroup", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Separator4", "Address/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Address", "Mailto", "Address/Mailto", GTK_UI_MANAGER_MENUITEM)

/* Tools menu */
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "ImportLDIF", "Tools/ImportLDIF", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "ImportMutt", "Tools/ImportMutt", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "ImportPine", "Tools/ImportPine", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "Separator1", "Tools/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "ExportHTML", "Tools/ExportHTML", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "ExportLDIF", "Tools/ExportLDIF", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "Separator2", "Tools/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "FindDuplicates", "Tools/FindDuplicates", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Tools", "EditAttrs", "Tools/EditAttrs", GTK_UI_MANAGER_MENUITEM)

/* Help menu */
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Menu/Help", "About", "Help/About", GTK_UI_MANAGER_MENUITEM)

	menubar = gtk_ui_manager_get_widget(ui_manager, "/Menu");

#ifndef MAEMO
	gtk_box_pack_start(GTK_BOX(vbox), menubar, FALSE, TRUE, 0);
#else
	hildon_window_set_menu(HILDON_WINDOW(window), GTK_MENU(menubar));
#endif

	vbox2 = gtk_vbox_new(FALSE, BORDER_WIDTH);
	gtk_container_set_border_width(GTK_CONTAINER(vbox2), BORDER_WIDTH);
	gtk_box_pack_start(GTK_BOX(vbox), vbox2, TRUE, TRUE, 0);

	ctree_swin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(ctree_swin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(ctree_swin, COL_FOLDER_WIDTH + 20, -1);

	/* Address index */
	ctree = gtk_sctree_new_with_titles(N_INDEX_COLS, 0, index_titles);
	gtkut_widget_set_can_focus(GTK_CMCLIST(ctree)->column[0].button, FALSE);

	gtk_container_add(GTK_CONTAINER(ctree_swin), ctree);
	gtk_cmclist_set_selection_mode(GTK_CMCLIST(ctree), GTK_SELECTION_BROWSE);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), 0, COL_FOLDER_WIDTH);
	if (prefs_common.enable_dotted_lines) {
		gtk_cmctree_set_line_style(GTK_CMCTREE(ctree), GTK_CMCTREE_LINES_DOTTED);
		gtk_cmctree_set_expander_style(GTK_CMCTREE(ctree),
				     GTK_CMCTREE_EXPANDER_SQUARE);
	} else {
		gtk_cmctree_set_line_style(GTK_CMCTREE(ctree), GTK_CMCTREE_LINES_NONE);
		gtk_cmctree_set_expander_style(GTK_CMCTREE(ctree),
				     GTK_CMCTREE_EXPANDER_TRIANGLE);
	}
	gtk_sctree_set_stripes(GTK_SCTREE(ctree), prefs_common.use_stripes_in_summaries);
	gtk_cmctree_set_indent(GTK_CMCTREE(ctree), CTREE_INDENT);
	gtk_cmclist_set_compare_func(GTK_CMCLIST(ctree),
				   addressbook_treenode_compare_func);

	g_signal_connect(G_OBJECT(ctree), "tree_select_row",
			 G_CALLBACK(addressbook_tree_selected), NULL);
	g_signal_connect(G_OBJECT(ctree), "button_press_event",
			 G_CALLBACK(addressbook_tree_button_pressed),
			 NULL);
	g_signal_connect(G_OBJECT(ctree), "button_release_event",
			 G_CALLBACK(addressbook_tree_button_released),
			 NULL);
	/* TEMPORARY */
	g_signal_connect(G_OBJECT(ctree), "select_row",
			 G_CALLBACK(addressbook_select_row_tree), NULL);

	gtk_drag_dest_set(ctree, GTK_DEST_DEFAULT_ALL & ~GTK_DEST_DEFAULT_HIGHLIGHT,
			  addressbook_drag_types, 1,
			  GDK_ACTION_MOVE | GDK_ACTION_COPY | GDK_ACTION_DEFAULT);
	g_signal_connect(G_OBJECT(ctree), "drag_motion",
			 G_CALLBACK(addressbook_drag_motion_cb),
			 ctree);
	g_signal_connect(G_OBJECT(ctree), "drag_leave",
			 G_CALLBACK(addressbook_drag_leave_cb),
			 ctree);
	g_signal_connect(G_OBJECT(ctree), "drag_data_received",
			 G_CALLBACK(addressbook_drag_received_cb),
			 ctree);
	g_signal_connect(G_OBJECT(ctree), "focus_in_event",
		G_CALLBACK(addressbook_address_index_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(ctree), "focus_out_event",
		G_CALLBACK(addressbook_address_index_focus_evt_out), NULL);

	clist_vbox = gtk_vbox_new(FALSE, 4);

	clist_swin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(clist_swin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(clist_vbox), clist_swin, TRUE, TRUE, 0);

	/* Address list */
	clist = gtk_sctree_new_with_titles(N_LIST_COLS, 0, list_titles);
	gtk_container_add(GTK_CONTAINER(clist_swin), clist);
	gtk_cmclist_set_selection_mode(GTK_CMCLIST(clist), GTK_SELECTION_EXTENDED);
	if (prefs_common.enable_dotted_lines) {
		gtk_cmctree_set_line_style(GTK_CMCTREE(clist), GTK_CMCTREE_LINES_DOTTED);
		gtk_cmctree_set_expander_style(GTK_CMCTREE(clist),
				     GTK_CMCTREE_EXPANDER_SQUARE);
	} else {
		gtk_cmctree_set_line_style(GTK_CMCTREE(clist), GTK_CMCTREE_LINES_NONE);
		gtk_cmctree_set_expander_style(GTK_CMCTREE(clist),
				     GTK_CMCTREE_EXPANDER_TRIANGLE);
	}
	gtk_sctree_set_stripes(GTK_SCTREE(ctree), prefs_common.use_stripes_in_summaries);
	gtk_cmctree_set_indent(GTK_CMCTREE(clist), CTREE_INDENT);
	gtk_cmclist_set_column_width(GTK_CMCLIST(clist), COL_NAME,
				   COL_NAME_WIDTH);
	gtk_cmclist_set_column_width(GTK_CMCLIST(clist), COL_ADDRESS,
				   COL_ADDRESS_WIDTH);
	gtk_widget_set_size_request(clist, -1, 80);

	addressbook_sort_list(GTK_CMCLIST(clist), COL_NAME, GTK_SORT_ASCENDING);
	g_signal_connect(G_OBJECT(GTK_CMCLIST(clist)->column[COL_NAME].button),
		"clicked", G_CALLBACK(addressbook_name_clicked), clist);
	g_signal_connect(G_OBJECT(GTK_CMCLIST(clist)->column[COL_ADDRESS].button),
		"clicked", G_CALLBACK(addressbook_address_clicked), clist);
	g_signal_connect(G_OBJECT(GTK_CMCLIST(clist)->column[COL_REMARKS].button),
		"clicked", G_CALLBACK(addressbook_remarks_clicked), clist);
	g_signal_connect(G_OBJECT(clist), "focus_in_event",
		G_CALLBACK(addressbook_address_list_focus_evt_in), NULL);
	g_signal_connect(G_OBJECT(clist), "focus_out_event",
		G_CALLBACK(addressbook_address_list_focus_evt_out), NULL);

	for (i = 0; i < N_LIST_COLS; i++)
		gtkut_widget_set_can_focus(GTK_CMCLIST(clist)->column[i].button,
					 FALSE);

	g_signal_connect(G_OBJECT(clist), "tree_select_row",
			 G_CALLBACK(addressbook_list_row_selected), NULL);
	g_signal_connect(G_OBJECT(clist), "tree_unselect_row",
			 G_CALLBACK(addressbook_list_row_unselected), NULL);
	g_signal_connect(G_OBJECT(clist), "button_press_event",
			 G_CALLBACK(addressbook_list_button_pressed),
			 NULL);
	g_signal_connect(G_OBJECT(clist), "button_release_event",
			 G_CALLBACK(addressbook_list_button_released),
			 NULL);
	g_signal_connect(G_OBJECT(clist), "tree_expand",
			 G_CALLBACK(addressbook_person_expand_node), NULL );
	g_signal_connect(G_OBJECT(clist), "tree_collapse",
			 G_CALLBACK(addressbook_person_collapse_node), NULL );
	g_signal_connect(G_OBJECT(clist), "start_drag",
			 G_CALLBACK(addressbook_start_drag), NULL);
	g_signal_connect(G_OBJECT(clist), "drag_data_get",
			 G_CALLBACK(addressbook_drag_data_get), NULL);	
	hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(clist_vbox), hbox, FALSE, FALSE, 0);

	label = gtk_label_new(_("Lookup name:"));
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);

	address_completion_register_entry(GTK_ENTRY(entry), FALSE);

	g_signal_connect(G_OBJECT(entry), "key_press_event",
			 G_CALLBACK(addressbook_entry_key_pressed),
			 NULL);

	if (!prefs_common.addressbook_use_editaddress_dialog) {
		editaddress_vbox = gtk_vbox_new(FALSE, 4);
		vpaned = gtk_vpaned_new();
		gtk_paned_pack1(GTK_PANED(vpaned), clist_vbox, FALSE, FALSE);
		gtk_paned_pack2(GTK_PANED(vpaned), editaddress_vbox, TRUE, FALSE);
	} else {
		vpaned = NULL;
		editaddress_vbox = NULL;
	}
	hpaned = gtk_hpaned_new();
	gtk_box_pack_start(GTK_BOX(vbox2), hpaned, TRUE, TRUE, 0);
	gtk_paned_pack1(GTK_PANED(hpaned), ctree_swin, FALSE, FALSE);
	if (prefs_common.addressbook_use_editaddress_dialog)
		gtk_paned_pack2(GTK_PANED(hpaned), clist_vbox, TRUE, FALSE);
	else
		gtk_paned_pack2(GTK_PANED(hpaned), vpaned, TRUE, FALSE);

	/* Status bar */
	hsbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), hsbox, FALSE, FALSE, BORDER_WIDTH);
	statusbar = gtk_statusbar_new();
	gtk_box_pack_start(GTK_BOX(hsbox), statusbar, TRUE, TRUE, BORDER_WIDTH);

	/* Button panel */
	hbbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(hbbox), GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(hbbox), 2);
	gtk_container_set_border_width(GTK_CONTAINER(hbbox), 4);
	gtk_box_pack_end(GTK_BOX(vbox), hbbox, FALSE, FALSE, 0);

	gtkut_stock_button_add_help(hbbox, &help_btn);

	edit_btn = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	gtkut_widget_set_can_default(edit_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), edit_btn, TRUE, TRUE, 0);
	del_btn = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtkut_widget_set_can_default(del_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), del_btn, TRUE, TRUE, 0);
	reg_btn = gtk_button_new_from_stock(GTK_STOCK_NEW);
	gtkut_widget_set_can_default(reg_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), reg_btn, TRUE, TRUE, 0);


	lup_btn = gtk_button_new_from_stock(GTK_STOCK_FIND);
	gtkut_widget_set_can_default(lup_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), lup_btn, TRUE, TRUE, 0);

	g_signal_connect(G_OBJECT(help_btn), "clicked",
			 G_CALLBACK(manual_open_with_anchor_cb),
			 MANUAL_ANCHOR_ADDRBOOK);

	g_signal_connect(G_OBJECT(edit_btn), "clicked",
			 G_CALLBACK(addressbook_edit_clicked), NULL);
	g_signal_connect(G_OBJECT(del_btn), "clicked",
			 G_CALLBACK(addressbook_del_clicked), NULL);
	g_signal_connect(G_OBJECT(reg_btn), "clicked",
			 G_CALLBACK(addressbook_reg_clicked), NULL);
	g_signal_connect(G_OBJECT(lup_btn), "clicked",
			 G_CALLBACK(addressbook_lup_clicked), NULL);

	to_btn = gtk_button_new_with_label
		(prefs_common_translated_header_name("To:"));
	gtkut_widget_set_can_default(to_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), to_btn, TRUE, TRUE, 0);
	cc_btn = gtk_button_new_with_label
		(prefs_common_translated_header_name("Cc:"));
	gtkut_widget_set_can_default(cc_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), cc_btn, TRUE, TRUE, 0);
	bcc_btn = gtk_button_new_with_label
		(prefs_common_translated_header_name("Bcc:"));
	gtkut_widget_set_can_default(bcc_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), bcc_btn, TRUE, TRUE, 0);

	close_btn = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
	gtkut_widget_set_can_default(close_btn, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), close_btn, TRUE, TRUE, 0);

	g_signal_connect(G_OBJECT(to_btn), "clicked",
			 G_CALLBACK(addressbook_to_clicked),
			 GINT_TO_POINTER(COMPOSE_TO));
	g_signal_connect(G_OBJECT(cc_btn), "clicked",
			 G_CALLBACK(addressbook_to_clicked),
			 GINT_TO_POINTER(COMPOSE_CC));
	g_signal_connect(G_OBJECT(bcc_btn), "clicked",
			 G_CALLBACK(addressbook_to_clicked),
			 GINT_TO_POINTER(COMPOSE_BCC));
	g_signal_connect(G_OBJECT(close_btn), "clicked",
			 G_CALLBACK(addressbook_close_clicked), NULL);

	/* Build icons for interface */

	/* Build control tables */
	addrbookctl_build_map(window);
	addrbookctl_build_iflist();
	addrbookctl_build_ifselect();

	addrbook.clist   = NULL;

	/* Add each interface into the tree as a root level folder */
	nodeIf = _addressInterfaceList_;
	while( nodeIf ) {
		AdapterInterface *adapter = nodeIf->data;
		AddressInterface *iface = adapter->interface;
		nodeIf = g_list_next(nodeIf);

		if(iface->useInterface) {
			AddressTypeControlItem *atci = adapter->atci;
			text = atci->displayName;
			adapter->treeNode =
				gtk_sctree_insert_node( GTK_CMCTREE(ctree),
					NULL, NULL, &text, FOLDER_SPACING,
					interfacexpm,
					interfacexpm,
					FALSE, FALSE );
			cm_menu_set_sensitive_full(ui_manager, atci->menuCommand, adapter->haveLibrary );
			gtk_cmctree_node_set_row_data_full(
				GTK_CMCTREE(ctree), adapter->treeNode, adapter,
				addressbook_free_treenode );
		}
	}

	/* Popup menu */

	MENUITEM_ADDUI_MANAGER(ui_manager, "/", "Popups", NULL, GTK_UI_MANAGER_MENUBAR);
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups", "ABTreePopup", "ABTreePopup", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "EditBook", "ABTreePopup/EditBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "DeleteBook", "ABTreePopup/DeleteBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "Separator1", "ABTreePopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "NewBook", "ABTreePopup/NewBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "NewFolder", "ABTreePopup/NewFolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "NewGroup", "ABTreePopup/NewGroup", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "Separator2", "ABTreePopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "Cut", "ABTreePopup/Cut", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "Copy", "ABTreePopup/Copy", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABTreePopup", "Paste", "ABTreePopup/Paste", GTK_UI_MANAGER_MENUITEM)
	
	tree_popup = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
				gtk_ui_manager_get_widget(ui_manager, "/Popups/ABTreePopup")));

	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups", "ABListPopup", "ABListPopup", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "SelectAll", "ABListPopup/SelectAll", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Separator1", "ABListPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Edit", "ABListPopup/Edit", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Delete", "ABListPopup/Delete", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Separator2", "ABListPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "NewAddress", "ABListPopup/NewAddress", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "NewGroup", "ABListPopup/NewGroup", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Separator3", "ABListPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Cut", "ABListPopup/Cut", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Copy", "ABListPopup/Copy", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Paste", "ABListPopup/Paste", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Separator4", "ABListPopup/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "Mailto", "ABListPopup/Mailto", GTK_UI_MANAGER_MENUITEM)
#ifdef USE_LDAP
	MENUITEM_ADDUI_MANAGER(ui_manager, "/Popups/ABListPopup", "BrowseEntry", "ABListPopup/BrowseEntry", GTK_UI_MANAGER_MENUITEM)
#endif
	list_popup = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
				gtk_ui_manager_get_widget(ui_manager, "/Popups/ABListPopup")));

	addrbook.window  = window;
	addrbook.hpaned  = hpaned;
	addrbook.vpaned  = vpaned;
	addrbook.menubar = menubar;
	addrbook.ctree   = ctree;
	addrbook.ctree_swin
			 = ctree_swin;
	addrbook.editaddress_vbox = editaddress_vbox;
	addrbook.clist   = clist;
	addrbook.label	 = label;
	addrbook.entry   = entry;
	addrbook.statusbar = statusbar;
	addrbook.status_cid = gtk_statusbar_get_context_id(
			GTK_STATUSBAR(statusbar), "Addressbook Window" );

	addrbook.help_btn = help_btn;
	addrbook.edit_btn = edit_btn;
	addrbook.del_btn = del_btn;
	addrbook.reg_btn = reg_btn;
	addrbook.lup_btn = lup_btn;
	addrbook.to_btn  = to_btn;
	addrbook.cc_btn  = cc_btn;
	addrbook.bcc_btn = bcc_btn;

	addrbook.tree_popup   = tree_popup;
	addrbook.list_popup   = list_popup;
	addrbook.ui_manager   = ui_manager;

	addrbook.listSelected = NULL;

	if (!geometry.min_height) {
		geometry.min_width = ADDRESSBOOK_WIDTH;
		geometry.min_height = ADDRESSBOOK_HEIGHT;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, prefs_common.addressbookwin_width,
				    prefs_common.addressbookwin_height);
#ifdef G_OS_WIN32
	gtk_window_move(GTK_WINDOW(window), 48, 48);
#endif

	if (!prefs_common.addressbook_use_editaddress_dialog) {
		if (prefs_common.addressbook_vpaned_pos > 0)
			gtk_paned_set_position(GTK_PANED(vpaned), 
				prefs_common.addressbook_vpaned_pos);
	}	
	if (prefs_common.addressbook_hpaned_pos > 0)
		gtk_paned_set_position(GTK_PANED(hpaned), 
			prefs_common.addressbook_hpaned_pos);


	gtk_widget_show_all(window);
}

/**
 * Close address book window and save to file(s).
 */
static gint addressbook_close( void ) {
	address_completion_end(addrbook.window);
	if (!prefs_common.addressbook_use_editaddress_dialog)
		addressbook_edit_person_invalidate(NULL, NULL, NULL);
	
	addressbook_pane_save_position();
	
	gtk_widget_hide(addrbook.window);
	addressbook_export_to_file();
	return TRUE;
}

/**
 * Display message in status line.
 * \param msg Message to display.
 */
static void addressbook_status_show( gchar *msg ) {
	if( addrbook.statusbar != NULL ) {
		gtk_statusbar_pop(
			GTK_STATUSBAR(addrbook.statusbar),
			addrbook.status_cid );
		if( msg ) {
			gtk_statusbar_push(
				GTK_STATUSBAR(addrbook.statusbar),
				addrbook.status_cid, msg );
		}
	}
}

static void addressbook_ds_show_message( AddressDataSource *ds ) {
	gint retVal;
	gchar *name;
	gchar *desc;
	*addressbook_msgbuf = '\0';
	if( ds ) {
		name = addrindex_ds_get_name( ds );
		retVal = addrindex_ds_get_status_code( ds );
		if( retVal == MGU_SUCCESS ) {
			g_snprintf( addressbook_msgbuf,
				    sizeof(addressbook_msgbuf), "%s", name );
		}
		else {
			desc = addressbook_err2string( _lutErrorsGeneral_, retVal );
			g_snprintf( addressbook_msgbuf, 
			    sizeof(addressbook_msgbuf), "%s: %s", name, desc );
		}
	}
	addressbook_status_show( addressbook_msgbuf );
}

static void addressbook_edit_clicked(GtkButton *button, gpointer data)
{
	addressbook_edit_address_cb(NULL, NULL);
}

static gboolean find_person(AddrSelectItem *item_a, ItemPerson *person)
{
	return ((ItemPerson *)item_a->addressItem == person)?0:-1;
}

/*
* Delete one or more objects from address list.
*/
static void addressbook_del_clicked(GtkButton *button, gpointer data)
{
	GtkCMCTree *clist = GTK_CMCTREE(addrbook.clist);
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	AddressObject *pobj;
	AdapterDSource *ads = NULL;
	GtkCMCTreeNode *nodeList;
	gboolean procFlag;
	AlertValue aval;
	AddressBookFile *abf = NULL;
	AddressDataSource *ds = NULL;
	AddressInterface *iface;
	AddrItemObject *aio;
	AddrSelectItem *item;
	GList *list, *node;
	gboolean refreshList = FALSE;
	
	pobj = gtk_cmctree_node_get_row_data(ctree, addrbook.opened );
	cm_return_if_fail(pobj != NULL);

	/* Test whether anything selected for deletion */
	nodeList = addrbook.listSelected;

	aio = gtk_cmctree_node_get_row_data( clist, nodeList );
	if( aio == NULL) return;
	ds = addressbook_find_datasource( addrbook.treeSelected );
	if( ds == NULL ) return;

	/* Test for read only */
	iface = ds->interface;
	if( iface->readOnly ) {
		alertpanel( _("Delete address(es)"),
			_("This address data is readonly and cannot be deleted."),
			GTK_STOCK_CLOSE, NULL, NULL );
		return;
	}

	/* Test whether Ok to proceed */
	procFlag = FALSE;
	if( pobj->type == ADDR_DATASOURCE ) {
		ads = ADAPTER_DSOURCE(pobj);
		if( ads->subType == ADDR_BOOK ) procFlag = TRUE;
	}
	else if( pobj->type == ADDR_ITEM_FOLDER ) {
		procFlag = TRUE;
	}
	else if( pobj->type == ADDR_ITEM_GROUP ) {
		procFlag = TRUE;
	}
	if( ! procFlag ) return;
	abf = ds->rawDataSource;
	if( abf == NULL ) return;

	gtk_cmclist_freeze(GTK_CMCLIST(addrbook.clist));
	g_signal_handlers_block_by_func
		(G_OBJECT(addrbook.clist),
		 G_CALLBACK(addressbook_list_row_unselected), NULL);

	/* Process deletions */
	if( pobj->type == ADDR_DATASOURCE || pobj->type == ADDR_ITEM_FOLDER ) {
		GList *groups = NULL, *persons = NULL, *emails = NULL;
		gboolean group_delete = TRUE;
		/* Items inside folders */
		list = addrselect_get_list( _addressSelect_ );
		/* Confirm deletion */
		node = list;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if( aio->type == ADDR_ITEM_PERSON || aio->type == ADDR_ITEM_EMAIL ) {
				group_delete = FALSE;
				break;
			}
		}
		if (group_delete) {
			aval = alertpanel( _("Delete group"),
					_("Really delete the group(s)?\n"
					  "The addresses it contains will not be lost."),
					GTK_STOCK_CANCEL, "+"GTK_STOCK_DELETE, NULL );
			if( aval != G_ALERTALTERNATE ) {
				goto thaw_ret;
			}
		} else {
			aval = alertpanel( _("Delete address(es)"),
					_("Really delete the address(es)?"),
					GTK_STOCK_CANCEL, "+"GTK_STOCK_DELETE, NULL );
			if( aval != G_ALERTALTERNATE ) {
				goto thaw_ret;
			}
		}
	
	/* first, set lists of groups and persons to remove */
		node = list;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if (!aio)
				continue;
			if( aio->type == ADDR_ITEM_GROUP ) {
				groups = g_list_prepend(groups, item);
			}
			else if( aio->type == ADDR_ITEM_PERSON ) {
				persons = g_list_prepend(persons, item);
			}
		}
	/* then set list of emails to remove *if* they're not children of
	 * persons to remove */
		node = list;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if (!aio)
				continue;
			if( aio->type == ADDR_ITEM_EMAIL ) {
				ItemEMail *sitem = ( ItemEMail * ) aio;
				ItemPerson *person = ( ItemPerson * ) ADDRITEM_PARENT(sitem);
				if (!g_list_find_custom(persons, person, (GCompareFunc)(find_person))) {
					emails = g_list_prepend(emails, item);
				}
				/* else, the email will be removed via the parent person */
			}
		}
	/* then delete groups */
		node = groups;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if (!aio)
				continue;
			if( aio->type == ADDR_ITEM_GROUP ) {
				ItemGroup *item = ( ItemGroup * ) aio;
				GtkCMCTreeNode *nd = NULL;
				nd = addressbook_find_group_node( addrbook.opened, item );
				item = addrbook_remove_group( abf, item );
				if( item ) {
					addritem_free_item_group( item );
				}
				/* Remove group from parent node */
				gtk_cmctree_remove_node( ctree, nd );
				refreshList = TRUE;
			}
		}
	/* then delete persons */
		node = persons;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if (!aio)
				continue;
			if( aio->type == ADDR_ITEM_PERSON ) {
				ItemPerson *item = ( ItemPerson * ) aio;
				item->status = DELETE_ENTRY; 
				addressbook_folder_remove_one_person( clist, item );
				if (pobj->type == ADDR_ITEM_FOLDER)
					addritem_folder_remove_person(ADAPTER_FOLDER(pobj)->itemFolder, item);
				item = addrbook_remove_person( abf, item );
#ifdef USE_LDAP
				if (ds && ds->type == ADDR_IF_LDAP) {
					LdapServer *server = ds->rawDataSource;
					ldapsvr_set_modified(server, TRUE);
					ldapsvr_update_book(server, item);
				}
#endif
				if( item ) {
					gchar *filename = addritem_person_get_picture(item);
					if (filename && is_file_exist(filename))
						claws_unlink(filename);
					g_free(filename);
					addritem_free_item_person( item );
				}
			}
		}
	/* then delete emails */
		node = emails;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if (!aio)
				continue;

			if( aio->type == ADDR_ITEM_EMAIL ) {
				ItemEMail *sitem = ( ItemEMail * ) aio;
				ItemPerson *person = ( ItemPerson * ) ADDRITEM_PARENT(sitem);
				sitem = addrbook_person_remove_email( abf, person, sitem );
				if( sitem ) {
					addrcache_remove_email(abf->addressCache, sitem);
					addritem_free_item_email( sitem );
				}
				addressbook_folder_refresh_one_person( clist, person );
			}
		}
		g_list_free( groups );
		g_list_free( persons );
		g_list_free( emails );
		g_list_free( list );
		addressbook_list_select_clear();
		if( refreshList ) {
			gtk_sctree_select( GTK_SCTREE(ctree), addrbook.opened);
			addressbook_set_clist(
				gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
					addrbook.opened),
				TRUE);
		}
		addrbook_set_dirty(abf, TRUE);
		addressbook_export_to_file();
		addressbook_list_menu_setup();
		goto thaw_ret;
	}
	else if( pobj->type == ADDR_ITEM_GROUP ) {
		/* Items inside groups */
		list = addrselect_get_list( _addressSelect_ );
		node = list;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = ( AddrItemObject * ) item->addressItem;
			if( aio->type == ADDR_ITEM_EMAIL ) {
				ItemEMail *item = ( ItemEMail * ) aio;
				ItemPerson *person = ( ItemPerson * ) ADDRITEM_PARENT(item);
				item = addrbook_person_remove_email( abf, person, item );
				if( item ) {
					addritem_free_item_email( item );
				}
			}
		}
		g_list_free( list );
		addressbook_list_select_clear();
		gtk_sctree_select( GTK_SCTREE(ctree), addrbook.opened);
		addressbook_set_clist(
			gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
				addrbook.opened),
			TRUE);
		
		addrbook_set_dirty(abf, TRUE);
		addressbook_export_to_file();
		addressbook_list_menu_setup();
		goto thaw_ret;
	}

	gtk_cmctree_node_set_row_data( clist, nodeList, NULL );
	gtk_cmctree_remove_node( clist, nodeList );
thaw_ret:
	gtk_cmclist_thaw(GTK_CMCLIST(addrbook.clist));
	g_signal_handlers_unblock_by_func
		(G_OBJECT(addrbook.clist),
		 G_CALLBACK(addressbook_list_row_unselected), NULL);
}

static void addressbook_reg_clicked(GtkButton *button, gpointer data)
{
	addressbook_new_address_cb( NULL, NULL );
}

static gchar *addressbook_format_address( AddrItemObject * aio ) {
	gchar *buf = NULL;
	gchar *name = NULL;
	gchar *address = NULL;

	if( aio->type == ADDR_ITEM_EMAIL ) {
		ItemPerson *person = NULL;
		ItemEMail *email = ( ItemEMail * ) aio;

		person = ( ItemPerson * ) ADDRITEM_PARENT(email);
		if( email->address ) {
			if( ADDRITEM_NAME(email) ) {
				name = ADDRITEM_NAME(email);
				if( *name == '\0' ) {
					name = ADDRITEM_NAME(person);
				}
			}
			else if( ADDRITEM_NAME(person) ) {
				name = ADDRITEM_NAME(person);
			}
			else {
				buf = g_strdup( email->address );
			}
			address = email->address;
		}
	}
	else if( aio->type == ADDR_ITEM_PERSON ) {
		ItemPerson *person = ( ItemPerson * ) aio;
		GList *node = person->listEMail;

		name = ADDRITEM_NAME(person);
		if( node ) {
			ItemEMail *email = ( ItemEMail * ) node->data;
			address = email->address;
		}
	}
	if( address ) {
		if( name && name[0] != '\0' ) {
			if( strchr_with_skip_quote( name, '"', ',' ) )
				buf = g_strdup_printf( "\"%s\" <%s>", name, address );
			else
				buf = g_strdup_printf( "%s <%s>", name, address );
		}
		else {
			buf = g_strdup( address );
		}
	}

	return buf;
}

static void addressbook_to_clicked(GtkButton *button, gpointer data)
{
	GList *list, *node;
	Compose *compose;
	AddrSelectItem *item;
	AddrItemObject *aio;
	gchar *addr;

	compose = addrbook.target_compose;
	if( ! compose ) return;

	/* Nothing selected, but maybe there is something in text entry */
	addr = (char *)gtk_entry_get_text( GTK_ENTRY( addrbook.entry) );
	if ( addr ) {
		compose_entry_append(
			compose, addr, (ComposeEntryType)data , PREF_NONE);
	}

	/* Select from address list */
	list = addrselect_get_list( _addressSelect_ );
	node = list;
	if (node) {
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			aio = item->addressItem;
			if( aio->type == ADDR_ITEM_PERSON ||
			    aio->type == ADDR_ITEM_EMAIL ) {
				addr = addressbook_format_address( aio );
				compose_entry_append(
					compose, addr, (ComposeEntryType) data, PREF_NONE );
				g_free( addr );
			}
			else if( aio->type == ADDR_ITEM_GROUP ) {
				ItemGroup *group = ( ItemGroup * ) aio;
				GList *nodeMail = group->listEMail;
				while( nodeMail ) {
					ItemEMail *email = nodeMail->data;

					addr = addressbook_format_address(
							( AddrItemObject * ) email );
					compose_entry_append(
						compose, addr, (ComposeEntryType) data, PREF_NONE );
					g_free( addr );
					nodeMail = g_list_next( nodeMail );
				}
			}
		}
	} else {
		AddressObject *obj = NULL;

		obj = gtk_cmctree_node_get_row_data( GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected );
	
		if( obj && obj->type == ADDR_ITEM_GROUP ) {
			ItemGroup *itemGroup = ADAPTER_GROUP(obj)->itemGroup;
			GList *nodeMail = itemGroup->listEMail;
			while( nodeMail ) {
				ItemEMail *email = nodeMail->data;

				addr = addressbook_format_address(
						( AddrItemObject * ) email );
				compose_entry_append(
					compose, addr, (ComposeEntryType) data, PREF_NONE );
				g_free( addr );
				nodeMail = g_list_next( nodeMail );
			}
		}
	}
	g_list_free( list );
}

static void addressbook_menubar_set_sensitive( gboolean sensitive ) {
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/EditBook",   sensitive );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/DeleteBook", sensitive );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/NewFolder",  sensitive );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/SelectAll",    TRUE );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Cut",    sensitive );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Copy",   sensitive );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Paste",  sensitive );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/NewAddress", sensitive );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/NewGroup",   sensitive );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Mailto",     sensitive );
	gtk_widget_set_sensitive( addrbook.edit_btn, sensitive );
	gtk_widget_set_sensitive( addrbook.del_btn, sensitive );
}

static void addressbook_menuitem_set_sensitive( AddressObject *obj, GtkCMCTreeNode *node ) {
	gboolean canEdit = FALSE;
	gboolean canDelete = TRUE;
	gboolean canAdd = FALSE;
	gboolean canEditTr = TRUE;
	gboolean editAddress = FALSE;
	gboolean canExport = TRUE;
	AddressTypeControlItem *atci = NULL;
	AddressDataSource *ds = NULL;
	AddressInterface *iface = NULL;

	if( obj == NULL ) return;
	if( obj->type == ADDR_INTERFACE ) {
		AdapterInterface *adapter = ADAPTER_INTERFACE(obj);
		iface = adapter->interface;
		if( iface ) {
			if( iface->haveLibrary ) {
				/* Enable appropriate File / New command */
				atci = adapter->atci;
				cm_menu_set_sensitive_full(addrbook.ui_manager, atci->menuCommand, TRUE );
			}
		}
		canEditTr = canExport = FALSE;
	}
	else if( obj->type == ADDR_DATASOURCE ) {
		AdapterDSource *ads = ADAPTER_DSOURCE(obj);
		ds = ads->dataSource;
		iface = ds->interface;
		if( ! iface->readOnly ) {
			canAdd = canEdit = editAddress = canDelete = TRUE;
		}
		if( ! iface->haveLibrary ) {
			canAdd = canEdit = editAddress = canExport = canDelete = FALSE;
		}
	}
	else if( obj->type == ADDR_ITEM_FOLDER ) {
		ds = addressbook_find_datasource( addrbook.treeSelected );
		if( ds ) {
			iface = ds->interface;
			if( iface->readOnly ) {
				canEditTr = FALSE;
				canDelete = FALSE;
			}
			else {
				canAdd = editAddress = TRUE;
			}
		}
	}
	else if( obj->type == ADDR_ITEM_GROUP ) {
		ds = addressbook_find_datasource( addrbook.treeSelected );
		if( ds ) {
			iface = ds->interface;
			if( ! iface->readOnly ) {
				editAddress = TRUE;
			}
		}
	}

	if( addrbook.listSelected == NULL )
		canEdit = FALSE;

	/* Enable add */
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/NewAddress", editAddress );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/NewGroup",   canAdd );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/NewFolder",  canAdd );
	gtk_widget_set_sensitive( addrbook.reg_btn, editAddress );

	/* Enable edit */
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Edit",   canEdit );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Delete", canDelete );
	gtk_widget_set_sensitive( addrbook.edit_btn, canEdit );
	gtk_widget_set_sensitive( addrbook.del_btn, canDelete );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/EditBook",      canEditTr );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/DeleteBook",    canEditTr );

	/* Export data */
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Tools/ExportHTML", canExport );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Tools/ExportLDIF", canExport );
}

/**
 * Address book tree callback function that responds to selection of tree
 * items.
 *
 * \param ctree  Tree widget.
 * \param node   Node that was selected.
 * \param column Column number where selected occurred.
 * \param data   Pointer to user data.
 */
static void addressbook_tree_selected(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gint column, gpointer data)
{
	AddressObject *obj = NULL;
	AdapterDSource *ads = NULL;
	AddressDataSource *ds = NULL;
	ItemFolder *rootFolder = NULL;
	AddressObjectType aot;

	addrbook.treeSelected = node;
	addrbook.listSelected = NULL;
	addressbook_status_show( "" );
	if( addrbook.entry != NULL ) gtk_entry_set_text(GTK_ENTRY(addrbook.entry), "");

	if( node ) obj = gtk_cmctree_node_get_row_data( ctree, node );
	if( obj == NULL ) {
		addressbook_set_clist(NULL, TRUE);
		return;
	}
	addrbook.opened = node;

	if( obj->type == ADDR_DATASOURCE ) {
		/* Read from file */
		static gboolean tVal = TRUE;

		ads = ADAPTER_DSOURCE(obj);
		if( ads == NULL ) return;
		ds = ads->dataSource;
		if( ds == NULL ) return;		

		if( addrindex_ds_get_modify_flag( ds ) ) {
			addrindex_ds_read_data( ds );
		}

		if( ! addrindex_ds_get_read_flag( ds ) ) {
			addrindex_ds_read_data( ds );
		}
		addressbook_ds_show_message( ds );

		if( ! addrindex_ds_get_access_flag( ds ) ) {
			/* Remove existing folders and groups */
			gtk_cmclist_freeze( GTK_CMCLIST(ctree) );
			addressbook_tree_remove_children( ctree, node );
			gtk_cmclist_thaw( GTK_CMCLIST(ctree) );

			/* Load folders into the tree */
			rootFolder = addrindex_ds_get_root_folder( ds );
			if( ds && ds->type == ADDR_IF_JPILOT ) {
				aot = ADDR_CATEGORY;
			}
			else if( ds && ds->type == ADDR_IF_LDAP ) {
				aot = ADDR_LDAP_QUERY;
			}
			else {
				aot = ADDR_ITEM_FOLDER;
			}
			addressbook_node_add_folder( node, ds, rootFolder, aot );
			addrindex_ds_set_access_flag( ds, &tVal );
			gtk_cmctree_expand( ctree, node );
		}
	} else {
		addressbook_set_clist(NULL, TRUE);
	}

	/* Update address list */
	g_signal_handlers_block_by_func
		(G_OBJECT(ctree),
		 G_CALLBACK(addressbook_tree_selected), NULL);
	addressbook_set_clist( obj, FALSE );
	g_signal_handlers_unblock_by_func
		(G_OBJECT(ctree),
		 G_CALLBACK(addressbook_tree_selected), NULL);
	if (!prefs_common.addressbook_use_editaddress_dialog)
		addressbook_edit_person_invalidate(NULL, NULL, NULL);

	/* Setup main menu selections */
	addressbook_menubar_set_sensitive( FALSE );
	addressbook_menuitem_set_sensitive( obj, node );
	addressbook_list_select_clear();
	addressbook_list_menu_setup();
	return;
}

/**
 * Setup address list popup menu items. Items are enabled or disabled as
 * required.
 */
static void addressbook_list_menu_setup( void ) {
	GtkCMCTree *clist = NULL;
	AddressObject *pobj = NULL;
	AddressObject *obj = NULL;
	AdapterDSource *ads = NULL;
	AddressInterface *iface = NULL;
	AddressDataSource *ds = NULL;
	gboolean canEdit = FALSE;
	gboolean canDelete = FALSE;
	gboolean canCut = FALSE;
	gboolean canCopy = FALSE;
	gboolean canPaste = FALSE;
	gboolean canBrowse = FALSE;

	pobj = gtk_cmctree_node_get_row_data( GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected );
	if( pobj == NULL ) return;

	clist = GTK_CMCTREE(addrbook.clist);
	obj = gtk_cmctree_node_get_row_data( clist, addrbook.listSelected );
	if( obj == NULL ) canEdit = FALSE;

	menu_set_insensitive_all( GTK_MENU_SHELL(addrbook.list_popup) );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/SelectAll", TRUE );

	if( pobj->type == ADDR_DATASOURCE ) {
		/* Parent object is a data source */
		ads = ADAPTER_DSOURCE(pobj);
		ds = ads->dataSource;
		if (!ds)
			return;
		iface = ds->interface;
		if (!iface)
			return;
		if( ! iface->readOnly ) {
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/NewAddress", TRUE );
			if (iface->type != ADDR_IF_LDAP)
				cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/NewGroup", TRUE );
			gtk_widget_set_sensitive( addrbook.reg_btn, TRUE );
			if( obj )
				canEdit = TRUE;
			canDelete = canEdit;
		}
	}
	else if( pobj->type != ADDR_INTERFACE ) {
		/* Parent object is not an interface */
		ds = addressbook_find_datasource( addrbook.treeSelected );
		if (!ds)
			return;
		iface = ds->interface;
		if (!iface)
			return;
		if( ! iface->readOnly ) {
			/* Folder or group */
			if( pobj->type == ADDR_ITEM_FOLDER || pobj->type == ADDR_ITEM_GROUP ) {
				cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/NewAddress", TRUE );
				gtk_widget_set_sensitive( addrbook.reg_btn, TRUE );
				if( obj ) canEdit = TRUE;
			}
			/* Folder */
			if( pobj->type == ADDR_ITEM_FOLDER ) {
				if (iface->type != ADDR_IF_LDAP)
					cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/NewGroup", TRUE );
				if( obj ) canEdit = TRUE;
			}
			canDelete = canEdit;
		}
		if( iface->type == ADDR_IF_LDAP ) {
			if( obj ) canBrowse = TRUE;
			canEdit = TRUE;
			canDelete = TRUE;
		}
	}

	if( iface ) {
		/* Enable cut and paste */
		if( ! addrclip_is_empty( _clipBoard_ ) )
			canPaste = TRUE;
		if( ! addrselect_test_empty( _addressSelect_ ) )
			canCut = TRUE;
		/* Enable copy if something is selected */
		if( ! addrselect_test_empty( _addressSelect_ ) )
			canCopy = TRUE;
	}

	/* Disable edit or browse if more than one row selected */
	if( GTK_CMCLIST(clist)->selection && GTK_CMCLIST(clist)->selection->next ) {
		canEdit = FALSE;
		canBrowse = FALSE;
	}

	/* Forbid write changes when read-only */
	if( iface && iface->readOnly ) {
		canCut = FALSE;
		canDelete = FALSE;
		canPaste = FALSE;
	}

	/* Now go finalize menu items */
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/Edit",   canEdit );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/Delete", canDelete );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/Cut",           canCut );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/Copy",          canCopy );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/Paste",         canPaste );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/Mailto",       canCopy );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Cut",           canCut );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Copy",          canCopy );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Paste",         canPaste );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Edit",    canEdit );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Delete",  canDelete );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Mailto", canCopy );

	gtk_widget_set_sensitive( addrbook.edit_btn, canEdit );
	gtk_widget_set_sensitive( addrbook.del_btn, canDelete );

	if (addrbook.target_compose) {
		gtk_widget_set_sensitive(addrbook.to_btn, obj ? TRUE : FALSE);	
		gtk_widget_set_sensitive(addrbook.cc_btn, obj ? TRUE : FALSE);
		gtk_widget_set_sensitive(addrbook.bcc_btn, obj ? TRUE : FALSE);
	}
#ifdef USE_LDAP
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/BrowseEntry",    canBrowse );
#endif
}

static void addressbook_select_row_tree	(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node,
					 gint		 column,
					 gpointer	 data)
{
}

/**
 * Add list of items into tree node below specified tree node.
 * \param treeNode  Tree node.
 * \param ds        Data source.
 * \param listItems List of items.
 */
static void addressbook_treenode_add_list(
	GtkCMCTreeNode *treeNode, AddressDataSource *ds, GList *listItems )
{
	GList *node;

	node = listItems;
	while( node ) {
		AddrItemObject *aio;
		GtkCMCTreeNode *nn;

		aio = node->data;
		if( ADDRESS_OBJECT_TYPE(aio) == ITEMTYPE_GROUP ) {
			ItemGroup *group;

			group = ( ItemGroup * ) aio;
			nn = addressbook_node_add_group( treeNode, ds, group );
		}
		else if( ADDRESS_OBJECT_TYPE(aio) == ITEMTYPE_FOLDER ) {
			ItemFolder *folder;

			folder = ( ItemFolder * ) aio;
			nn = addressbook_node_add_folder(
				treeNode, ds, folder, ADDR_ITEM_FOLDER );
		}
		node = g_list_next( node );
	}
}

static void addressbook_select_all_cb( GtkAction *action, gpointer data ) {
	gtk_cmclist_select_all(GTK_CMCLIST(addrbook.clist));
}

/**
 * Cut from address list widget.
 */
static void addressbook_clip_cut_cb( GtkAction *action, gpointer data ) {
	_clipBoard_->cutFlag = TRUE;
	addrclip_clear( _clipBoard_ );
	addrclip_add( _clipBoard_, _addressSelect_ );
	/* addrclip_list_show( _clipBoard_, stdout ); */
}

/**
 * Copy from address list widget.
 */
static void addressbook_clip_copy_cb(GtkAction *action, gpointer data) {
	_clipBoard_->cutFlag = FALSE;
	addrclip_clear( _clipBoard_ );
	addrclip_add( _clipBoard_, _addressSelect_ );
	/* addrclip_list_show( _clipBoard_, stdout ); */
}

/**
 * Paste clipboard into address list widget.
 */
static void addressbook_clip_paste_cb( GtkAction *action, gpointer data ) {
	GtkCMCTree *ctree = GTK_CMCTREE( addrbook.ctree );
	AddressObject *pobj = NULL;
	AddressDataSource *ds = NULL;
	AddressBookFile *abf = NULL;
	ItemFolder *folder = NULL;
	GList *folderGroup = NULL;

	ds = addressbook_find_datasource( GTK_CMCTREE_NODE(addrbook.treeSelected) );
	if( ds == NULL ) return;
	if( addrindex_ds_get_readonly( ds ) ) {
		alertpanel_error( _("Cannot paste. Target address book is readonly.") );
		return;
	}

	pobj = gtk_cmctree_node_get_row_data( ctree, addrbook.treeSelected );
	if( pobj ) {
		if( pobj->type == ADDR_ITEM_FOLDER ) {
			folder = ADAPTER_FOLDER(pobj)->itemFolder;
		}
		else if( pobj->type == ADDR_ITEM_GROUP ) {
			alertpanel_error( _("Cannot paste into an address group.") );
			return;
		}
	}

	/* Get an address book */
	abf = addressbook_get_book_file();
	if( abf == NULL ) return;

	if( _clipBoard_->cutFlag ) {
		/* Paste/Cut */
		folderGroup = addrclip_paste_cut( _clipBoard_, abf, folder );

		/* Remove all groups and folders in clipboard from tree node */
		addressbook_treenode_remove_item();

		/* Remove all "cut" items */
		addrclip_delete_item( _clipBoard_ );

		/* Clear clipboard - cut items??? */
		addrclip_clear( _clipBoard_ );
	}
	else {
		/* Paste/Copy */
		folderGroup = addrclip_paste_copy( _clipBoard_, abf, folder );
	}

	/* addrclip_list_show( _clipBoard_, stdout ); */
	if( folderGroup ) {
		/* Update tree by inserting node for each folder or group */
		addressbook_treenode_add_list(
			addrbook.treeSelected, ds, folderGroup );
		gtk_cmctree_expand( ctree, addrbook.treeSelected );
		g_list_free( folderGroup );
		folderGroup = NULL;
	}

	/* Display items pasted */
	gtk_sctree_select( GTK_SCTREE(ctree), addrbook.opened );
	addressbook_set_clist(
		gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
			addrbook.opened),
		TRUE);
	

}

/**
 * Add current treenode object to clipboard. Note that widget only allows
 * one entry from the tree list to be selected.
 */
static void addressbook_treenode_to_clipboard( void ) {
	AddressObject *obj = NULL;
	AddressDataSource *ds = NULL;
	AddrSelectItem *item;
	GtkCMCTree *ctree = GTK_CMCTREE( addrbook.ctree );
	GtkCMCTreeNode *node;

	node = addrbook.treeSelected;
	if( node == NULL ) return;
	obj = gtk_cmctree_node_get_row_data( ctree, node );
	if( obj == NULL ) return;

	ds = addressbook_find_datasource( node );
	if( ds == NULL ) return;

	item = NULL;
	if( obj->type == ADDR_ITEM_FOLDER ) {
		AdapterFolder *adapter = ADAPTER_FOLDER(obj);
		ItemFolder *folder = adapter->itemFolder;

		item = addrselect_create_node( obj );
		item->uid = g_strdup( ADDRITEM_ID(folder) );
	}
	else if( obj->type == ADDR_ITEM_GROUP ) {
		AdapterGroup *adapter = ADAPTER_GROUP(obj);
		ItemGroup *group = adapter->itemGroup;

		item = addrselect_create_node( obj );
		item->uid = g_strdup( ADDRITEM_ID(group) );
	}
	else if( obj->type == ADDR_DATASOURCE ) {
		/* Data source */
		item = addrselect_create_node( obj );
		item->uid = NULL;
	}

	if( item ) {
		/* Clear existing list and add item into list */
		gchar *cacheID;

		addressbook_list_select_clear();
		cacheID = addrindex_get_cache_id( _addressIndex_, ds );
		addrselect_list_add( _addressSelect_, item, cacheID );
		g_free( cacheID );
	}
}

/**
 * Cut from tree widget.
 */
static void addressbook_treenode_cut_cb( GtkAction *action, gpointer data ) {
	_clipBoard_->cutFlag = TRUE;
	addressbook_treenode_to_clipboard();
	addrclip_clear( _clipBoard_ );
	addrclip_add( _clipBoard_, _addressSelect_ );
	/* addrclip_list_show( _clipBoard_, stdout ); */
}

/**
 * Copy from tree widget.
 */
static void addressbook_treenode_copy_cb( GtkAction *action, gpointer data ) {
	_clipBoard_->cutFlag = FALSE;
	addressbook_treenode_to_clipboard();
	addrclip_clear( _clipBoard_ );
	addrclip_add( _clipBoard_, _addressSelect_ );
	/* addrclip_list_show( _clipBoard_, stdout ); */
}

/**
 * Paste clipboard into address tree widget.
 */
static void addressbook_treenode_paste_cb( GtkAction *action, gpointer data ) {
	addressbook_clip_paste_cb(NULL,NULL);
}

/**
 * Clear selected entries in clipboard.
 */
static void addressbook_list_select_clear( void ) {
	addrselect_list_clear( _addressSelect_ );
}

/**
 * Add specified address item to selected address list.
 * \param aio Address item object.
 * \param ds  Datasource.
 */
static void addressbook_list_select_add( AddrItemObject *aio, AddressDataSource *ds ) {
	gchar *cacheID;

	if( ds == NULL ) return;
	cacheID = addrindex_get_cache_id( _addressIndex_, ds );
	addrselect_list_add_obj( _addressSelect_, aio, cacheID );
	g_free( cacheID );
}

/**
 * Remove specified address item from selected address list.
 * \param aio Address item object.
 */
static void addressbook_list_select_remove( AddrItemObject *aio ) {
	addrselect_list_remove( _addressSelect_, aio );
}

/**
 * Invoke EMail compose window with addresses in selected address list.
 */
static void addressbook_mail_to_cb( GtkAction *action, gpointer data ) {
	GList *listAddress;

	if( ! addrselect_test_empty( _addressSelect_ ) ) {
		listAddress = addrselect_build_list( _addressSelect_ );
		compose_new_with_list( NULL, listAddress );
		mgu_free_dlist( listAddress );
		listAddress = NULL;
	}
}

static void addressbook_list_row_selected( GtkCMCTree *clist,
					   GtkCMCTreeNode *node,
					   gint column,
					   gpointer data )
{
	GtkEntry *entry = GTK_ENTRY(addrbook.entry);
	AddrItemObject *aio = NULL;
	AddressObject *pobj = NULL;
	AdapterDSource *ads = NULL;
	AddressDataSource *ds = NULL;

	gtk_entry_set_text( entry, "" );
	addrbook.listSelected = node;

	pobj = gtk_cmctree_node_get_row_data( GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected );
	if( pobj == NULL ) return;

	if( pobj->type == ADDR_DATASOURCE ) {
		ads = ADAPTER_DSOURCE(pobj);
		ds = ads->dataSource;
	}
	else if( pobj->type != ADDR_INTERFACE ) {
		ds = addressbook_find_datasource( addrbook.treeSelected );
	}

	aio = gtk_cmctree_node_get_row_data( clist, node );
	if( aio ) {
		/* g_print( "list select: %d : '%s'\n", aio->type, aio->name ); */
		addressbook_list_select_add( aio, ds );
	}

	addressbook_list_menu_setup();

	if (!addrbook.target_compose && !prefs_common.addressbook_use_editaddress_dialog) {
		AddressObject *obj = gtk_cmctree_node_get_row_data( clist, addrbook.listSelected );

		if (obj && obj->type != ADDR_ITEM_GROUP)
			addressbook_edit_address(NULL, 0, NULL, FALSE);
	}
}

static void addressbook_list_row_unselected( GtkCMCTree *ctree,
					     GtkCMCTreeNode *node,
					     gint column,
					     gpointer data )
{
	AddrItemObject *aio;

	aio = gtk_cmctree_node_get_row_data( ctree, node );
	if( aio != NULL ) {
		/* g_print( "list unselect: %d : '%s'\n", aio->type, aio->name ); */
		addressbook_list_select_remove( aio );
	}

	if (!prefs_common.addressbook_use_editaddress_dialog)
		addressbook_edit_person_invalidate(NULL, NULL, NULL);
}

static gboolean addressbook_list_button_pressed(GtkWidget *widget,
						GdkEventButton *event,
						gpointer data)
{
	if( ! event ) return FALSE;

	addressbook_list_menu_setup();

	if( event->button == 3 ) {
		gtk_menu_popup( GTK_MENU(addrbook.list_popup), NULL, NULL, NULL, NULL,
		       event->button, event->time );
	} else if (event->button == 1) {
		if (event->type == GDK_2BUTTON_PRESS) {
			if (prefs_common.add_address_by_click &&
			    addrbook.target_compose)
				addressbook_to_clicked(NULL, GINT_TO_POINTER(COMPOSE_TO));
			else
				if (prefs_common.addressbook_use_editaddress_dialog)
					addressbook_edit_address_cb(NULL, NULL);
				else {
					GtkCMCTree *clist = GTK_CMCTREE(addrbook.clist);
					AddressObject *obj = gtk_cmctree_node_get_row_data( clist, addrbook.listSelected );
					if( obj && obj->type == ADDR_ITEM_GROUP )
						addressbook_edit_address_cb(NULL, NULL);
				}
		}
	}

	return FALSE;
}

static gboolean addressbook_list_button_released(GtkWidget *widget,
						 GdkEventButton *event,
						 gpointer data)
{
	return FALSE;
}

static gboolean addressbook_tree_button_pressed(GtkWidget *ctree,
						GdkEventButton *event,
						gpointer data)
{
	GtkCMCList *clist = GTK_CMCLIST(ctree);
	gint row, column;
	AddressObject *obj = NULL;
	AdapterDSource *ads = NULL;
	AddressInterface *iface = NULL;
	AddressDataSource *ds = NULL;
	gboolean canEdit = FALSE;
	gboolean canDelete = FALSE;
	gboolean canCut = FALSE;
	gboolean canCopy = FALSE;
	gboolean canPaste = FALSE;
	gboolean canTreeCut = FALSE;
	gboolean canTreeCopy = FALSE;
	gboolean canTreePaste = FALSE;
	gboolean canLookup = FALSE;
	GtkCMCTreeNode *node = NULL;
	
	if( ! event ) return FALSE;
/*	if( ! event || event->type != GDK_BUTTON_PRESS) return FALSE;*/

	if (event->button == 1) {
		if (event->type == GDK_2BUTTON_PRESS) {
			if( gtk_cmclist_get_selection_info( clist, event->x, event->y, &row, &column ) ) {
				gtkut_clist_set_focus_row(clist, row);
				obj = gtk_cmclist_get_row_data( clist, row );
			}
			if( obj == NULL )
				return FALSE;

			if (obj->type == ADDR_ITEM_GROUP) {
				/* edit group */
				addressbook_treenode_edit_cb(NULL, NULL);
			} else {
				/* expand pr collapse */
				node = gtk_cmctree_node_nth(GTK_CMCTREE(ctree), row);
				gtk_cmctree_toggle_expansion(GTK_CMCTREE(ctree), node);
			}
			return FALSE;
		}
	}

	addressbook_menubar_set_sensitive( FALSE );

	if( gtk_cmclist_get_selection_info( clist, event->x, event->y, &row, &column ) ) {
		gtkut_clist_set_focus_row(clist, row);
		obj = gtk_cmclist_get_row_data( clist, row );
	}

	menu_set_insensitive_all(GTK_MENU_SHELL(addrbook.tree_popup));

	if( obj == NULL )
		return FALSE;
	node = gtk_cmctree_node_nth(GTK_CMCTREE(clist), row);

	if( ! addrclip_is_empty( _clipBoard_ ) )
		canTreePaste = TRUE;

	if (obj->type == ADDR_INTERFACE) {
		AdapterInterface *adapter = ADAPTER_INTERFACE(obj);
		iface = adapter->interface;
		if( !iface )
			goto just_set_sens;
		if( !iface->readOnly ) {
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/NewBook", TRUE );
			gtk_widget_set_sensitive( addrbook.reg_btn, TRUE );
		}
		if( iface->externalQuery )
			canLookup = TRUE;
	}
	if (obj->type == ADDR_DATASOURCE) {
		ads = ADAPTER_DSOURCE(obj);
		ds = ads->dataSource;
		if( !ds )
			goto just_set_sens;
		iface = ds->interface;
		if( !iface )
			goto just_set_sens;
		if( !iface->readOnly ) {
			canDelete = TRUE;
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/NewFolder", TRUE );
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/NewGroup", TRUE );
			gtk_widget_set_sensitive( addrbook.reg_btn, TRUE );
		}
		canEdit = TRUE;
		canTreeCopy = TRUE;
		if( iface->externalQuery )
			canLookup = TRUE;
	}
	else if (obj->type == ADDR_ITEM_FOLDER) {
		ds = addressbook_find_datasource( node );
		if( !ds )
			goto just_set_sens;
		iface = ds->interface;
		if( !iface )
			goto just_set_sens;
		if( !iface->readOnly ) {
			canEdit = TRUE;
			canDelete = TRUE;
			canTreeCut = TRUE;
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/NewFolder", TRUE );
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/NewGroup", TRUE );
			gtk_widget_set_sensitive( addrbook.reg_btn, TRUE );
		}
		canTreeCopy = TRUE;

		if( iface->externalQuery ) {
			/* Enable deletion of LDAP folder */
			canLookup = TRUE;
			canDelete = TRUE;
		}
	}
	else if (obj->type == ADDR_ITEM_GROUP) {
		ds = addressbook_find_datasource( node );
		if( !ds )
			goto just_set_sens;
		iface = ds->interface;
		if( !iface )
			goto just_set_sens;
		if( ! iface->readOnly ) {
			canEdit = TRUE;
			canDelete = TRUE;
			cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABListPopup/NewAddress", TRUE );
			gtk_widget_set_sensitive( addrbook.reg_btn, TRUE );
		}
	}

	if( canEdit && !addrselect_test_empty( _addressSelect_ ) )
		canCut = TRUE;
	if( ! addrselect_test_empty( _addressSelect_ ) )
		canCopy = TRUE;
	if( ! addrclip_is_empty( _clipBoard_ ) )
		canPaste = TRUE;

	/* Forbid write changes when read-only */
	if( iface && iface->readOnly ) {
		canTreeCut = FALSE;
		canTreePaste = FALSE;
		canCut = FALSE;
		canDelete = FALSE;
		canPaste = FALSE;
	}

just_set_sens:
	/* Enable edit */
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/EditBook",   canEdit );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/DeleteBook", canDelete );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/Cut",    canTreeCut );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/Copy",   canTreeCopy );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Popups/ABTreePopup/Paste",  canTreePaste );

	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/EditBook",          canEdit );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Book/DeleteBook",        canEdit );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Cut",           canCut );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Copy",          canCopy );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Paste",         canPaste );

	addressbook_show_buttons(addrbook.target_compose == NULL, canLookup,
			addrbook.target_compose != NULL);

	if( event->button == 3 )
		gtk_menu_popup(GTK_MENU(addrbook.tree_popup), NULL, NULL, NULL, NULL,
			       event->button, event->time);

	return FALSE;
}

static gboolean addressbook_tree_button_released(GtkWidget *ctree,
						 GdkEventButton *event,
						 gpointer data)
{
	gtkut_ctree_set_focus_row(GTK_CMCTREE(addrbook.ctree), addrbook.opened);
	return FALSE;
}

static void addressbook_new_folder_cb(GtkAction *action, gpointer data)
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	AddressObject *obj = NULL;
	AddressDataSource *ds = NULL;
	AddressBookFile *abf = NULL;
	ItemFolder *parentFolder = NULL;
	ItemFolder *folder = NULL;

	if( ! addrbook.treeSelected ) return;
	obj = gtk_cmctree_node_get_row_data( ctree, addrbook.treeSelected );
	if( obj == NULL ) return;
	ds = addressbook_find_datasource( addrbook.treeSelected );
	if( ds == NULL ) return;

	if( obj->type == ADDR_DATASOURCE ) {
		if( ADAPTER_DSOURCE(obj)->subType != ADDR_BOOK ) return;
	}
	else if( obj->type == ADDR_ITEM_FOLDER ) {
		parentFolder = ADAPTER_FOLDER(obj)->itemFolder;
	}
	else {
		return;
	}

	abf = ds->rawDataSource;
	if( abf == NULL ) return;
	folder = addressbook_edit_folder( abf, parentFolder, NULL );
	if( folder ) {
		GtkCMCTreeNode *nn;
		nn = addressbook_node_add_folder(
			addrbook.treeSelected, ds, folder, ADDR_ITEM_FOLDER );
		gtk_cmctree_expand( ctree, addrbook.treeSelected );
		if( addrbook.treeSelected == addrbook.opened )
			addressbook_set_clist(obj, TRUE);
	}
}

static void addressbook_new_group_cb(GtkAction *action, gpointer data)
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	AddressObject *obj = NULL;
	AddressDataSource *ds = NULL;
	AddressBookFile *abf = NULL;
	ItemFolder *parentFolder = NULL;
	ItemGroup *group = NULL;

	if( ! addrbook.treeSelected ) return;
	obj = gtk_cmctree_node_get_row_data(ctree, addrbook.treeSelected);
	if( obj == NULL ) return;
	ds = addressbook_find_datasource( addrbook.treeSelected );
	if( ds == NULL ) return;

	if( obj->type == ADDR_DATASOURCE ) {
		if( ADAPTER_DSOURCE(obj)->subType != ADDR_BOOK ) return;
	}
	else if( obj->type == ADDR_ITEM_FOLDER ) {
		parentFolder = ADAPTER_FOLDER(obj)->itemFolder;
	}
	else {
		return;
	}

	abf = ds->rawDataSource;
	if( abf == NULL ) return;
	group = addressbook_edit_group( abf, parentFolder, NULL );
	if( group ) {
		GtkCMCTreeNode *nn;
		nn = addressbook_node_add_group( addrbook.treeSelected, ds, group );
		gtk_cmctree_expand( ctree, addrbook.treeSelected );
		if( addrbook.treeSelected == addrbook.opened )
			addressbook_set_clist(obj, TRUE);
	}
}

static void addressbook_change_node_name(GtkCMCTreeNode *node, const gchar *name)
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	gchar *text[1];
	guint8 spacing;
	GdkPixbuf *pix_cl, *pix_op;
	gboolean is_leaf, expanded;

	gtk_cmctree_get_node_info(ctree, node, text, &spacing,
				&pix_cl, &pix_op,
				&is_leaf, &expanded);
	gtk_sctree_set_node_info(ctree, node, name, spacing,
				pix_cl, pix_op,
				is_leaf, expanded);
}

/**
 * Edit data source.
 * \param obj  Address object to edit.
 * \param node Node in tree.
 * \return New name of data source.
 */
static gchar *addressbook_edit_datasource( AddressObject *obj, GtkCMCTreeNode *node ) {
	gchar *newName = NULL;
	AddressDataSource *ds = NULL;
	AddressInterface *iface = NULL;
	AdapterDSource *ads = NULL;

	ds = addressbook_find_datasource( node );
	if( ds == NULL ) return NULL;
	iface = ds->interface;
	if( ! iface->haveLibrary ) return NULL;

	/* Read data from data source */
	if( addrindex_ds_get_modify_flag( ds ) ) {
		addrindex_ds_read_data( ds );
	}

	if( ! addrindex_ds_get_read_flag( ds ) ) {
		addrindex_ds_read_data( ds );
	}

	/* Handle edit */
	ads = ADAPTER_DSOURCE(obj);
	if( ads->subType == ADDR_BOOK ) {
                if( addressbook_edit_book( _addressIndex_, ads ) == NULL ) return NULL;
	}
	else if( ads->subType == ADDR_VCARD ) {
       	        if( addressbook_edit_vcard( _addressIndex_, ads ) == NULL ) return NULL;
	}
#ifdef USE_JPILOT
	else if( ads->subType == ADDR_JPILOT ) {
                if( addressbook_edit_jpilot( _addressIndex_, ads ) == NULL ) return NULL;
	}
#endif
#ifdef USE_LDAP
	else if( ads->subType == ADDR_LDAP ) {
		if( addressbook_edit_ldap( _addressIndex_, ads ) == NULL ) return NULL;
	}
#endif
	else {
		return NULL;
	}
	newName = obj->name;
	return newName;
}

/*
* Edit an object that is in the address tree area.
*/
static void addressbook_treenode_edit_cb(GtkAction *action, gpointer data)
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	AddressObject *obj;
	AddressDataSource *ds = NULL;
	AddressBookFile *abf = NULL;
	GtkCMCTreeNode *node = NULL, *parentNode = NULL;
	gchar *name = NULL;

	if( ! addrbook.treeSelected ) return;
	node = addrbook.treeSelected;
	if( GTK_CMCTREE_ROW(node)->level == 1 ) return;
	obj = gtk_cmctree_node_get_row_data( ctree, node );
	if( obj == NULL ) return;
	parentNode = GTK_CMCTREE_ROW(node)->parent;

	ds = addressbook_find_datasource( node );
	if( ds == NULL ) return;

	if( obj->type == ADDR_DATASOURCE ) {
		name = addressbook_edit_datasource( obj, node );
		if( name == NULL ) return;
	}
	else {
		abf = ds->rawDataSource;
		if( abf == NULL ) return;
		if( obj->type == ADDR_ITEM_FOLDER ) {
			AdapterFolder *adapter = ADAPTER_FOLDER(obj);
			ItemFolder *item = adapter->itemFolder;
			ItemFolder *parentFolder = NULL;
			parentFolder = ( ItemFolder * ) ADDRITEM_PARENT(item);
			if( addressbook_edit_folder( abf, parentFolder, item ) == NULL ) return;
			name = ADDRITEM_NAME(item);
		}
		else if( obj->type == ADDR_ITEM_GROUP ) {
			AdapterGroup *adapter = ADAPTER_GROUP(obj);
			ItemGroup *item = adapter->itemGroup;
			ItemFolder *parentFolder = NULL;
			parentFolder = ( ItemFolder * ) ADDRITEM_PARENT(item);
			if( addressbook_edit_group( abf, parentFolder, item ) == NULL ) return;
			name = ADDRITEM_NAME(item);
		}
	}
	if( name && parentNode ) {
		/* Update node in tree view */
		addressbook_change_node_name( node, name );
		gtk_sctree_sort_node(ctree, parentNode);
		gtk_cmctree_expand( ctree, node );
		gtk_sctree_select( GTK_SCTREE( ctree), node );
	}
}

typedef enum {
	ADDRTREE_DEL_NONE,
	ADDRTREE_DEL_DATA,
	ADDRTREE_DEL_FOLDER_ONLY,
	ADDRTREE_DEL_FOLDER_ADDR
} TreeItemDelType ;

/**
 * Delete an item from the tree widget.
 * \param data   Data passed in.
 * \param action Action.
 * \param widget Widget issuing callback.
 */
static void addressbook_treenode_delete_cb(GtkAction *action, gpointer data)
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	GtkCMCTreeNode *node = NULL;
	AddressObject *obj;
	gchar *message;
	AlertValue aval;
	AddrBookBase *adbase;
	AddressCache *cache;
	AdapterDSource *ads = NULL;
	AddressInterface *iface = NULL;
	AddressDataSource *ds = NULL;
	gboolean remFlag = FALSE;
	TreeItemDelType delType;

	if( ! addrbook.treeSelected ) return;
	node = addrbook.treeSelected;
	if( GTK_CMCTREE_ROW(node)->level == 1 ) return;

	obj = gtk_cmctree_node_get_row_data( ctree, node );
	cm_return_if_fail(obj != NULL);

	if( obj->type == ADDR_DATASOURCE ) {
		ads = ADAPTER_DSOURCE(obj);
		if( ads == NULL ) return;
		ds = ads->dataSource;
		if( ds == NULL ) return;
	}
	else {
		/* Must be folder or something else */
		ds = addressbook_find_datasource( node );
		if( ds == NULL ) return;

		/* Only allow deletion from non-readOnly */
		iface = ds->interface;
		if( iface->readOnly ) {
			/* Allow deletion of query results */
			if( ! iface->externalQuery ) return;
		}
	}

	/* Confirm deletion */
	delType = ADDRTREE_DEL_NONE;
	if( obj->type == ADDR_ITEM_FOLDER ) {
		if( iface->externalQuery ) {
			message = g_strdup_printf( _(
				"Do you want to delete the query " \
				"results and addresses in '%s' ?" ),
				obj->name );
			aval = alertpanel( _("Delete"), message,
				GTK_STOCK_CANCEL, "+"GTK_STOCK_DELETE, NULL );
			g_free(message);
			if( aval == G_ALERTALTERNATE ) {
				delType = ADDRTREE_DEL_FOLDER_ADDR;
			}
		}
		else {
			message = g_strdup_printf
				( _( "Do you want to delete '%s' ? "
			    	     "If you delete the folder only, the addresses it contains will be moved into the parent folder." ),
			 	 obj->name );
			aval = alertpanel( _("Delete folder"), message,
				GTK_STOCK_CANCEL, _("+Delete _folder only"), _("Delete folder and _addresses"));
			g_free(message);
			if( aval == G_ALERTALTERNATE ) {
				delType = ADDRTREE_DEL_FOLDER_ONLY;
			}
			else if( aval == G_ALERTOTHER ) {
				delType = ADDRTREE_DEL_FOLDER_ADDR;
			}
		}
	}
	else if( obj->type == ADDR_ITEM_GROUP ) {
		message = g_strdup_printf(_("Do you want to delete '%s'?\n"
					    "The addresses it contains will not be lost."), obj->name);
		aval = alertpanel(_("Delete"), message, GTK_STOCK_CANCEL, 
				"+" GTK_STOCK_DELETE, NULL);
		g_free(message);
		if( aval == G_ALERTALTERNATE ) delType = ADDRTREE_DEL_FOLDER_ONLY;
	} else {
		message = g_strdup_printf(_("Do you want to delete '%s'?\n"
					    "The addresses it contains will be lost."), obj->name);
		aval = alertpanel(_("Delete"), message, GTK_STOCK_CANCEL, 
				"+" GTK_STOCK_DELETE, NULL);
		g_free(message);
		if( aval == G_ALERTALTERNATE ) delType = ADDRTREE_DEL_DATA;
	}
	if( delType == ADDRTREE_DEL_NONE ) return;

	/* Proceed with deletion */
	if( obj->type == ADDR_DATASOURCE ) {
		/* Remove node from tree */
		gtk_cmctree_remove_node( ctree, node );
	
		/* Remove data source. */
		if( addrindex_index_remove_datasource( _addressIndex_, ds ) ) {
			addrindex_free_datasource( ds );
		}
		return;
	}

	/* Get reference to cache */
	adbase = ( AddrBookBase * ) ds->rawDataSource;
	if( adbase == NULL ) return;
	cache = adbase->addressCache;

	/* Remove query results folder */
	if( iface->externalQuery ) {
		AdapterFolder *adapter = ADAPTER_FOLDER(obj);
		ItemFolder *folder = adapter->itemFolder;

		adapter->itemFolder = NULL;
		/*
		g_print( "remove folder for ::%s::\n", obj->name );
		g_print( "      folder name ::%s::\n", ADDRITEM_NAME(folder) );
		g_print( "-------------- remove results\n" );
		*/
		addrindex_remove_results( ds, folder );
		/* g_print( "-------------- remove node\n" ); */
		gtk_cmctree_remove_node( ctree, node );
		return;
	}

	/* Code below is valid for regular address book deletion */
	if( obj->type == ADDR_ITEM_FOLDER ) {
		AdapterFolder *adapter = ADAPTER_FOLDER(obj);
		ItemFolder *item = adapter->itemFolder;

		if( delType == ADDRTREE_DEL_FOLDER_ONLY ) {
			/* Remove folder only */
			item = addrcache_remove_folder( cache, item );
			if( item ) {
				addritem_free_item_folder( item );
				addressbook_move_nodes_up( ctree, node );
				remFlag = TRUE;
			}
		}
		else if( delType == ADDRTREE_DEL_FOLDER_ADDR ) {
			/* Remove folder and addresses */
			item = addrcache_remove_folder_delete( cache, item );
			if( item ) {
				addritem_free_item_folder( item );
				remFlag = TRUE;
			}
		}
	}
	else if( obj->type == ADDR_ITEM_GROUP ) {
		AdapterGroup *adapter = ADAPTER_GROUP(obj);
		ItemGroup *item = adapter->itemGroup;

		item = addrcache_remove_group( cache, item );
		if( item ) {
			addritem_free_item_group( item );
			remFlag = TRUE;
		}
	}

	if( remFlag ) {
		/* Remove node. */
		gtk_cmctree_remove_node(ctree, node );
	}
}

static void addressbook_new_address_from_book_post_cb( ItemPerson *person )
{
	if( person && addrbook.treeSelected == addrbook.opened ) {
		person->status = ADD_ENTRY;
		gtk_cmclist_unselect_all( GTK_CMCLIST(addrbook.clist) );
		addressbook_folder_refresh_one_person(
			GTK_CMCTREE(addrbook.clist), person );
	}
	addressbook_address_list_set_focus();
}

static void addressbook_new_address_from_folder_post_cb( ItemPerson *person )
{
	if( person && addrbook.treeSelected == addrbook.opened) {
		person->status = ADD_ENTRY;
		gtk_sctree_select( GTK_SCTREE(addrbook.ctree), addrbook.opened );
		addressbook_set_clist(
			gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
				addrbook.opened),
			TRUE);
	}
	addressbook_address_list_set_focus();
}

/**
 * Label (a format string) that is used to name each folder.
 */
static gchar *_queryFolderLabel_ = N_( "Search '%s'" );

/**
 * Search ctree widget callback function.
 * \param  pA Pointer to node.
 * \param  pB Pointer to data item being sought.
 * \return Zero (0) if folder found.
 */
static int addressbook_treenode_find_folder_cb( gconstpointer pA, gconstpointer pB ) {
	AddressObject *aoA;

	aoA = ( AddressObject * ) pA;
	if( aoA->type == ADDR_ITEM_FOLDER ) {
		ItemFolder *folder, *fld;

		fld = ADAPTER_FOLDER(aoA)->itemFolder;
		folder = ( ItemFolder * ) pB;
		if( fld == folder ) return 0;	/* Found folder */
	}
	return 1;
}

static ItemFolder * addressbook_setup_subf(
		AddressDataSource *ds, gchar *title,
		GtkCMCTreeNode *pNode )
{
	AddrBookBase *adbase;
	AddressCache *cache;
	ItemFolder *folder;
	GtkCMCTree *ctree;
	GtkCMCTreeNode *nNode;
	gchar *name;
	AddressObjectType aoType = ADDR_NONE;
	GList *children;
	/* Setup a query */
	if( *title == '\0' || strlen( title ) < 1 ) return NULL;

	if( ds && ds->type == ADDR_IF_LDAP ) {
#if USE_LDAP
		aoType = ADDR_LDAP_QUERY;
#endif
	}
	else {
		return NULL;
	}

	ctree = GTK_CMCTREE(addrbook.ctree);
	/* Get reference to address cache */	
	adbase = ( AddrBookBase * ) ds->rawDataSource;
	cache = adbase->addressCache;
	
	if ((children = addrcache_get_list_folder(cache)) != NULL) {
		GList *cur = children;
		for (; cur; cur = cur->next) {
			ItemFolder *child = (ItemFolder *) cur->data;
			if (!strcmp2(ADDRITEM_NAME(child), title)) {
				nNode = gtk_cmctree_find_by_row_data_custom(
					ctree, NULL, child,
					addressbook_treenode_find_folder_cb );
				if( nNode ) {
					addrindex_remove_results( ds, child );
					while( child->listPerson ) {
						ItemPerson *item = ( ItemPerson * ) child->listPerson->data;
						item = addrcache_remove_person( cache, item );
						if( item ) {
							addritem_free_item_person( item );
							item = NULL;
						}
					}
					gtk_sctree_select( GTK_SCTREE(ctree), nNode );
					addrbook.treeSelected = nNode;
				}	
				return child;
			}
		}
	}
	
	/* Create a folder */
	folder = addrcache_add_new_folder( cache, NULL );
	name = g_strdup_printf( "%s", title );
	addritem_folder_set_name( folder, name );
	addritem_folder_set_remarks( folder, "" );
	g_free( name );

	/* Now let's see the folder */
	nNode = addressbook_node_add_folder( pNode, ds, folder, aoType );
	gtk_cmctree_expand( ctree, pNode );
	if( nNode ) {
		gtk_sctree_select( GTK_SCTREE(ctree), nNode );
		addrbook.treeSelected = nNode;
		return folder;
	}
	return NULL;
}

static void addressbook_new_address_cb( GtkAction *action, gpointer data ) {
	AddressObject *pobj = NULL;
	AddressDataSource *ds = NULL;
	AddressBookFile *abf = NULL;
	debug_print("adding address\n");
	pobj = gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected);
	if( pobj == NULL ) {
		debug_print("no row data\n");
		return;
	}
	ds = addressbook_find_datasource( GTK_CMCTREE_NODE(addrbook.treeSelected) );
	if( ds == NULL ) {
		debug_print("no datasource\n");
		return;
	}

	abf = ds->rawDataSource;
	if( abf == NULL ) {
		g_print("no addressbook file\n");
		return;
	}

	if( pobj->type == ADDR_DATASOURCE ) {
		if (ADAPTER_DSOURCE(pobj)->subType == ADDR_BOOK ||
		    ADAPTER_DSOURCE(pobj)->subType == ADDR_LDAP) {
			ItemPerson *person;
			ItemFolder *folder = NULL;
#ifdef USE_LDAP
			if (abf && abf->type == ADDR_IF_LDAP) {
				GtkCMCTreeNode *parentNode;
				ds = addressbook_find_datasource( GTK_CMCTREE_NODE( addrbook.treeSelected ) );
				if( ds == NULL ) return;

				/* We must have a datasource that is an external interface */
				if( ! ds->interface->haveLibrary ) return;
				if( ! ds->interface->externalQuery ) return;

				if( pobj->type == ADDR_ITEM_FOLDER ) {
					parentNode = GTK_CMCTREE_ROW(GTK_CMCTREE_NODE( addrbook.treeSelected ) )->parent;
				}
				else {
					parentNode = GTK_CMCTREE_NODE( addrbook.treeSelected );
				}
				folder = addressbook_setup_subf( ds, _("New Contacts"), parentNode );

				pobj = gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected);
				ds = addressbook_find_datasource( GTK_CMCTREE_NODE(addrbook.treeSelected) );
				if (ds)
					abf = ds->rawDataSource;
			}
#endif
			person = addressbook_edit_person( abf, folder, NULL, FALSE,
								  addrbook.editaddress_vbox,
								  addressbook_new_address_from_book_post_cb,
								  TRUE );
#ifdef USE_LDAP
			if (ds && abf && abf->type == ADDR_IF_LDAP) {
				LdapServer *server = ds->rawDataSource;
				ldapsvr_set_modified(server, TRUE);
				ldapsvr_update_book(server, NULL);
				if (server->retVal != LDAPRC_SUCCESS) {
					alertpanel( _("Add address(es)"),
						addressbook_err2string(_lutErrorsLDAP_, server->retVal),
						GTK_STOCK_CLOSE, NULL, NULL );
					server->retVal = LDAPRC_SUCCESS;
					return;
				}
			}
#endif
			if (prefs_common.addressbook_use_editaddress_dialog)
				addressbook_new_address_from_book_post_cb( person );
		}
	}
	else if( pobj->type == ADDR_ITEM_FOLDER ) {
		/* New address */
		ItemFolder *folder = ADAPTER_FOLDER(pobj)->itemFolder;
		ItemPerson *person;
#ifdef USE_LDAP
		if (abf && abf->type == ADDR_IF_LDAP) {
			GtkCMCTreeNode *parentNode;
			ds = addressbook_find_datasource( GTK_CMCTREE_NODE( addrbook.treeSelected ) );
			if( ds == NULL ) return;

			/* We must have a datasource that is an external interface */
			if( ! ds->interface->haveLibrary ) return;
			if( ! ds->interface->externalQuery ) return;

			if( pobj->type == ADDR_ITEM_FOLDER ) {
				parentNode = GTK_CMCTREE_ROW(GTK_CMCTREE_NODE( addrbook.treeSelected ) )->parent;
			}
			else {
				parentNode = GTK_CMCTREE_NODE( addrbook.treeSelected );
			}
			folder = addressbook_setup_subf( ds, _("New Contacts"), parentNode );
			if (!folder)
				return;
			pobj = gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected);
			ds = addressbook_find_datasource( GTK_CMCTREE_NODE(addrbook.treeSelected) );
			if (ds)
				abf = ds->rawDataSource;
		}
#endif
		person = addressbook_edit_person( abf, folder, NULL, FALSE,
							  addrbook.editaddress_vbox,
							  addressbook_new_address_from_folder_post_cb,
							  TRUE );
#ifdef USE_LDAP
		if (ds && abf && abf->type == ADDR_IF_LDAP) {
			LdapServer *server = ds->rawDataSource;
			ldapsvr_set_modified(server, TRUE);
			ldapsvr_update_book(server, NULL);
			if (server->retVal != LDAPRC_SUCCESS) {
				alertpanel( _("Add address(es)"),
						addressbook_err2string(_lutErrorsLDAP_, server->retVal),
					GTK_STOCK_CLOSE, NULL, NULL );
				return;
			}
		}
#endif
		if (prefs_common.addressbook_use_editaddress_dialog)
			addressbook_new_address_from_folder_post_cb( person );
	}
	else if( pobj->type == ADDR_ITEM_GROUP ) {
		/* New address in group */
		ItemGroup *group = ADAPTER_GROUP(pobj)->itemGroup;
		if( addressbook_edit_group( abf, NULL, group ) == NULL ) return;
		if (addrbook.treeSelected == addrbook.opened) {
			/* Change node name in tree. */
			addressbook_change_node_name( addrbook.treeSelected, ADDRITEM_NAME(group) );
			gtk_sctree_select( GTK_SCTREE(addrbook.ctree), addrbook.opened );
			addressbook_set_clist(
				gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
					addrbook.opened),
				TRUE);
		}
	}
}

/**
 * Search for specified child group node in address index tree.
 * \param parent Parent node.
 * \param group  Group to find.
 */
static GtkCMCTreeNode *addressbook_find_group_node( GtkCMCTreeNode *parent, ItemGroup *group ) {
	GtkCMCTreeNode *node = NULL;
	GtkCMCTreeRow *currRow;

	currRow = GTK_CMCTREE_ROW( parent );
	if( currRow ) {
		node = currRow->children;
		while( node ) {
			AddressObject *obj;

			obj = gtk_cmctree_node_get_row_data( GTK_CMCTREE(addrbook.ctree), node );
			if( obj->type == ADDR_ITEM_GROUP ) {
				ItemGroup *g = ADAPTER_GROUP(obj)->itemGroup;
				if( g == group ) return node;
			}
			currRow = GTK_CMCTREE_ROW(node);
			node = currRow->sibling;
		}
	}
	return NULL;
}

static AddressBookFile *addressbook_get_book_file() {
	AddressBookFile *abf = NULL;
	AddressDataSource *ds = NULL;

	ds = addressbook_find_datasource( addrbook.treeSelected );
	if( ds == NULL ) return NULL;
	if( ds->type == ADDR_IF_BOOK || ds->type == ADDR_IF_LDAP ) abf = ds->rawDataSource;
	return abf;
}

static void addressbook_tree_remove_children( GtkCMCTree *ctree, GtkCMCTreeNode *parent ) {
	GtkCMCTreeNode *node;
	GtkCMCTreeRow *row;

	/* Remove existing folders and groups */
	row = GTK_CMCTREE_ROW( parent );
	if( row ) {
		while( (node = row->children) ) {
			gtk_cmctree_remove_node( ctree, node );
		}
	}
}

static void addressbook_move_nodes_up( GtkCMCTree *ctree, GtkCMCTreeNode *node ) {
	GtkCMCTreeNode *parent, *child;
	GtkCMCTreeRow *currRow;
	currRow = GTK_CMCTREE_ROW( node );
	if( currRow ) {
		parent = currRow->parent;
		while( (child = currRow->children) ) {
			gtk_cmctree_move( ctree, child, parent, node );
		}
		gtk_sctree_sort_node( ctree, parent );
	}
}

static void addressbook_edit_address_post_cb( ItemPerson *person )
{
	if( person ) {
#ifdef USE_LDAP
		AddressBookFile *abf = addressbook_get_book_file();

		if (abf && abf->type == ADDR_IF_LDAP) {
			if (strcmp2(person->nickName, ADDRITEM_NAME(person)))
				addritem_person_set_nick_name( person, ADDRITEM_NAME(person));
		}
#endif
		addressbook_folder_refresh_one_person( GTK_CMCTREE(addrbook.clist), person );
		invalidate_address_completion();
	}
	addressbook_address_list_set_focus();
}

void addressbook_address_list_set_focus( void )
{
	if (!prefs_common.addressbook_use_editaddress_dialog) {
		gtk_window_set_focus(GTK_WINDOW(addrbook.window), addrbook.clist);
		addressbook_list_menu_setup();
	}
}

void addressbook_address_list_disable_some_actions(void)
{
	/* disable address copy/pasting when editing contact's detail (embedded form) */
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Cut",   FALSE );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Copy",  FALSE );
	cm_menu_set_sensitive_full( addrbook.ui_manager, "Menu/Address/Paste", FALSE );
}

static void addressbook_edit_address_cb( GtkAction *action, gpointer data ) {
	addressbook_edit_address(data, 0, NULL, TRUE);
}
	
static void addressbook_edit_address( gpointer data, guint action, GtkWidget *widget,
									  gboolean force_focus ) {
	GtkCMCTree *clist = GTK_CMCTREE(addrbook.clist);
	GtkCMCTree *ctree;
	AddressObject *obj = NULL, *pobj = NULL;
	AddressDataSource *ds = NULL;
	GtkCMCTreeNode *node = NULL, *parentNode = NULL;
	gchar *name = NULL;
	AddressBookFile *abf = NULL;

	if( addrbook.listSelected == NULL ) return;
	obj = gtk_cmctree_node_get_row_data( clist, addrbook.listSelected );
	cm_return_if_fail(obj != NULL);

       	ctree = GTK_CMCTREE( addrbook.ctree );
	pobj = gtk_cmctree_node_get_row_data( ctree, addrbook.treeSelected );
	node = gtk_cmctree_find_by_row_data( ctree, addrbook.treeSelected, obj );

	ds = addressbook_find_datasource( GTK_CMCTREE_NODE(addrbook.treeSelected) );
	if( ds == NULL ) return;

	abf = addressbook_get_book_file();
	
	if( obj->type == ADDR_ITEM_EMAIL ) {
		ItemEMail *email = ( ItemEMail * ) obj;
		if( email == NULL ) return;
		if( pobj && pobj->type == ADDR_ITEM_GROUP ) {
			/* Edit parent group */
			AdapterGroup *adapter = ADAPTER_GROUP(pobj);
			ItemGroup *itemGrp = adapter->itemGroup;
			if( abf == NULL ) return;
			if( addressbook_edit_group( abf, NULL, itemGrp ) == NULL ) return;
			name = ADDRITEM_NAME(itemGrp);
			node = addrbook.treeSelected;
			parentNode = GTK_CMCTREE_ROW(node)->parent;
		}
		else {
			/* Edit person - email page */
			ItemPerson *person;
			person = ( ItemPerson * ) ADDRITEM_PARENT(email);
			if  ( addressbook_edit_person( abf, NULL, person, TRUE, addrbook.editaddress_vbox,
										   addressbook_edit_address_post_cb,
										   (prefs_common.addressbook_use_editaddress_dialog||force_focus) )
				  != NULL ) { 
#ifdef USE_LDAP
				if (abf && abf->type == ADDR_IF_LDAP) {
					ldapsvr_set_modified( (LdapServer *) abf, TRUE );
					person->status = UPDATE_ENTRY;
				}
#endif
				if (prefs_common.addressbook_use_editaddress_dialog)
					addressbook_edit_address_post_cb( person );
			}
			return;
		}
	}
	else if( obj->type == ADDR_ITEM_PERSON ) {
		/* Edit person - basic page */
		ItemPerson *person = ( ItemPerson * ) obj;
		if( addressbook_edit_person( abf, NULL, person, FALSE, addrbook.editaddress_vbox,
									  addressbook_edit_address_post_cb,
									  (prefs_common.addressbook_use_editaddress_dialog||force_focus) )
			!= NULL ) {
#ifdef USE_LDAP
				if (abf && abf->type == ADDR_IF_LDAP) {
					ldapsvr_set_modified( (LdapServer *) abf, TRUE );
					person->status = UPDATE_ENTRY;
				}
#endif
				if (prefs_common.addressbook_use_editaddress_dialog)
					addressbook_edit_address_post_cb( person );
		}
		return;
	}
	else if( obj->type == ADDR_ITEM_GROUP ) {
		ItemGroup *itemGrp = ( ItemGroup * ) obj;
		if( addressbook_edit_group( abf, NULL, itemGrp ) == NULL ) return;
		parentNode = addrbook.treeSelected;
		node = addressbook_find_group_node( parentNode, itemGrp );
		name = ADDRITEM_NAME(itemGrp);
		invalidate_address_completion();
	}
	else {
		return;
	}

	/* Update tree node with node name */
	if( node == NULL ) return;
	addressbook_change_node_name( node, name );
	gtk_sctree_sort_node( ctree, parentNode );
	gtk_sctree_select( GTK_SCTREE(ctree), addrbook.opened ); 
	addressbook_set_clist(
		gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree),
			addrbook.opened),
		TRUE);
}

static void addressbook_delete_address_cb(GtkAction *action, gpointer data)
{
	addressbook_del_clicked(NULL, NULL);
}

static void close_cb(GtkAction *action, gpointer data)
{
	addressbook_close();
}

static void addressbook_file_save_cb( GtkAction *action, gpointer data ) {
	addressbook_export_to_file();
}

static void addressbook_person_expand_node( GtkCMCTree *ctree, GList *node, gpointer *data ) {
	if( node ) {
		ItemPerson *person = gtk_cmctree_node_get_row_data( ctree, GTK_CMCTREE_NODE(node) );
		if( person ) addritem_person_set_opened( person, TRUE );
	}
}

static void addressbook_person_collapse_node( GtkCMCTree *ctree, GList *node, gpointer *data ) {
	if( node ) {
		ItemPerson *person = gtk_cmctree_node_get_row_data( ctree, GTK_CMCTREE_NODE(node) );
		if( person ) addritem_person_set_opened( person, FALSE );
	}
}

static gchar *addressbook_format_item_clist( ItemPerson *person, ItemEMail *email ) {
	gchar *str = NULL;
	gchar *eMailAlias = ADDRITEM_NAME(email);
	if( eMailAlias && *eMailAlias != '\0' ) {
		if( person ) {
			str = g_strdup_printf( "%s - %s", ADDRITEM_NAME(person), eMailAlias );
		}
		else {
			str = g_strdup( eMailAlias );
		}
	}
	return str;
}

static void addressbook_load_group( GtkCMCTree *clist, ItemGroup *itemGroup ) {
	GList *items = itemGroup->listEMail;
	AddressTypeControlItem *atci = addrbookctl_lookup( ADDR_ITEM_EMAIL );
	for( ; items != NULL; items = g_list_next( items ) ) {
		GtkCMCTreeNode *nodeEMail = NULL;
		gchar *text[N_LIST_COLS];
		ItemEMail *email = items->data;
		ItemPerson *person;
		gchar *str = NULL;

		if( ! email ) continue;

		person = ( ItemPerson * ) ADDRITEM_PARENT(email);
		str = addressbook_format_item_clist( person, email );
		if( str ) {
			text[COL_NAME] = addressbook_set_col_name_guard(str);
		}
		else {
			text[COL_NAME] = addressbook_set_col_name_guard(ADDRITEM_NAME(person));
		}
		text[COL_ADDRESS] = email->address;
		text[COL_REMARKS] = email->remarks;
		nodeEMail = gtk_sctree_insert_node(
				clist, NULL, NULL,
				text, FOLDER_SPACING,
				atci->iconXpm,
				atci->iconXpmOpen,
				FALSE, FALSE );
		gtk_cmctree_node_set_row_data( clist, nodeEMail, email );
		g_free( str );
		str = NULL;
	}
}

gchar *addressbook_set_col_name_guard(gchar *value)
{
	gchar *ret = "<not set>";
	gchar *tmp = g_strdup(value);
	g_strstrip(tmp);
	if (tmp !=NULL && *tmp != '\0')
		ret = value;
	g_free(tmp);
	return ret;
}

static void addressbook_folder_load_one_person(
		GtkCMCTree *clist, ItemPerson *person,
		AddressTypeControlItem *atci,
		AddressTypeControlItem *atciMail )
{
	GtkCMCTreeNode *nodePerson = NULL;
	GtkCMCTreeNode *nodeEMail = NULL;
	gchar *text[N_LIST_COLS];
	gboolean flgFirst = TRUE, haveAddr = FALSE;
	GList *node;
#ifdef USE_LDAP
	AddressBookFile *abf = addressbook_get_book_file();
#endif

	if( person == NULL ) return;

	text[COL_NAME] = "";
	node = person->listEMail;
	while( node ) {
		ItemEMail *email = node->data;
		gchar *eMailAddr = NULL;
		node = g_list_next( node );

		text[COL_ADDRESS] = email->address;
		text[COL_REMARKS] = email->remarks;
		eMailAddr = ADDRITEM_NAME(email);
		if( eMailAddr && *eMailAddr == '\0' ) eMailAddr = NULL;
		if( flgFirst ) {
			/* First email belongs with person */
			gchar *str = addressbook_format_item_clist( person, email );
			if( str ) {
				text[COL_NAME] = addressbook_set_col_name_guard(str);
			}
#ifdef USE_LDAP
			else if( abf && abf->type == ADDR_IF_LDAP && 
				 person && person->nickName ) {
				if (person->nickName) {
					if (strcmp(person->nickName, "") != 0) {
						text[COL_NAME] = addressbook_set_col_name_guard(person->nickName);
					}
					else {
						text[COL_NAME] = addressbook_set_col_name_guard(ADDRITEM_NAME(person));
					}
				}
			}
#endif
			else {
				text[COL_NAME] = addressbook_set_col_name_guard(ADDRITEM_NAME(person));
			}
			nodePerson = gtk_sctree_insert_node(
					clist, NULL, NULL,
					text, FOLDER_SPACING,
					atci->iconXpm,
					atci->iconXpmOpen,
					FALSE, person->isOpened );
			g_free( str );
			str = NULL;
			gtk_cmctree_node_set_row_data(clist, nodePerson, person );
		}
		else {
			/* Subsequent email is a child node of person */
			text[COL_NAME] = ADDRITEM_NAME(email);
			nodeEMail = gtk_sctree_insert_node(
					clist, nodePerson, NULL,
					text, FOLDER_SPACING,
					atciMail->iconXpm,
					atciMail->iconXpmOpen,
					FALSE, TRUE );
			gtk_cmctree_node_set_row_data(clist, nodeEMail, email );
		}
		flgFirst = FALSE;
		haveAddr = TRUE;
	}
	if( ! haveAddr ) {
		/* Have name without EMail */
		text[COL_NAME] = addressbook_set_col_name_guard(ADDRITEM_NAME(person));
		text[COL_ADDRESS] = "";
		text[COL_REMARKS] = "";
		nodePerson = gtk_sctree_insert_node(
				clist, NULL, NULL,
				text, FOLDER_SPACING,
				atci->iconXpm,
				atci->iconXpmOpen,
				FALSE, person->isOpened );
		gtk_cmctree_node_set_row_data(clist, nodePerson, person );
	}
	return;
}

static void addressbook_folder_load_person( GtkCMCTree *clist, ItemFolder *itemFolder ) {
	GList *items;
	AddressTypeControlItem *atci = addrbookctl_lookup( ADDR_ITEM_PERSON );
	AddressTypeControlItem *atciMail = addrbookctl_lookup( ADDR_ITEM_EMAIL );

	if( atci == NULL ) return;
	if( atciMail == NULL ) return;

	/* Load email addresses */
	items = addritem_folder_get_person_list( itemFolder );
	for( ; items != NULL; items = g_list_next( items ) ) {
		addressbook_folder_load_one_person( clist, items->data, atci, atciMail );
	}
	/* Free up the list */
	mgu_clear_list( items );
	g_list_free( items );
}

static void addressbook_folder_remove_node( GtkCMCTree *clist, GtkCMCTreeNode *node ) { 
	addrbook.listSelected = NULL;
	gtk_cmctree_remove_node( clist, node );
	addressbook_menubar_set_sensitive( FALSE );
	addressbook_menuitem_set_sensitive(
		gtk_cmctree_node_get_row_data(
			GTK_CMCTREE(clist), addrbook.treeSelected ),
		addrbook.treeSelected );
}

static void addressbook_folder_refresh_one_person( GtkCMCTree *clist, ItemPerson *person ) {
	AddressTypeControlItem *atci = addrbookctl_lookup( ADDR_ITEM_PERSON );
	AddressTypeControlItem *atciMail = addrbookctl_lookup( ADDR_ITEM_EMAIL );
	GtkCMCTreeNode *node;
	if( atci == NULL ) return;
	if( atciMail == NULL ) return;
	if( person == NULL ) return;
	/* unload the person */
	
	node = gtk_cmctree_find_by_row_data( clist, NULL, person );
	if( node )
		addressbook_folder_remove_node( clist, node );
	addressbook_folder_load_one_person( clist, person, atci, atciMail );
	gtk_sctree_sort_node( clist, NULL );
	node = gtk_cmctree_find_by_row_data( clist, NULL, person );
	if( node ) {
		gtk_sctree_select( GTK_SCTREE(clist), node );
		if (!gtk_cmctree_node_is_visible( clist, node ) ) 
			gtk_cmctree_node_moveto( clist, node, 0, 0, 0 );
	}
}

static void addressbook_folder_remove_one_person( GtkCMCTree *clist, ItemPerson *person ) {
	GtkCMCTreeNode *node;
	gint row;
	
	if( person == NULL ) return;
	node = gtk_cmctree_find_by_row_data( clist, NULL, person );
	row  = gtk_cmclist_find_row_from_data( GTK_CMCLIST(clist), person );
	if( node ) {
		addressbook_folder_remove_node( clist, node );
	}
}

static void addressbook_folder_load_group( GtkCMCTree *clist, ItemFolder *itemFolder ) {
	GList *items;
	AddressTypeControlItem *atci =  addrbookctl_lookup( ADDR_ITEM_GROUP );

	/* Load any groups */
	if( ! atci ) return;
	items = addritem_folder_get_group_list( itemFolder );
	for( ; items != NULL; items = g_list_next( items ) ) {
		GtkCMCTreeNode *nodeGroup = NULL;
		gchar *text[N_LIST_COLS];
		ItemGroup *group = items->data;
		if( group == NULL ) continue;
		text[COL_NAME] = ADDRITEM_NAME(group);
		text[COL_ADDRESS] = "";
		text[COL_REMARKS] = "";
		nodeGroup = gtk_sctree_insert_node(clist, NULL, NULL,
				      text, FOLDER_SPACING,
				      atci->iconXpm,
				      atci->iconXpmOpen,
				      FALSE, FALSE);
		gtk_cmctree_node_set_row_data(clist, nodeGroup, group );
		gtk_sctree_sort_node(clist, NULL);
	}
	/* Free up the list */
	mgu_clear_list( items );
	g_list_free( items );
}

/**
 * Search ctree widget callback function.
 * \param  pA Pointer to node.
 * \param  pB Pointer to data item being sought.
 * \return Zero (0) if group found.
 */
static int addressbook_treenode_find_group_cb( gconstpointer pA, gconstpointer pB ) {
	AddressObject *aoA;

	aoA = ( AddressObject * ) pA;
	if( aoA->type == ADDR_ITEM_GROUP ) {
		ItemGroup *group, *grp;

		grp = ADAPTER_GROUP(aoA)->itemGroup;
		group = ( ItemGroup * ) pB;
		if( grp == group ) return 0;	/* Found group */
	}
	return 1;
}

/*
* Remove folder and group nodes from tree widget for items contained ("cut")
* in clipboard.
*/
static void addressbook_treenode_remove_item( void ) {
	GList *node;
	AddrSelectItem *cutItem;
	AddressCache *cache;
	AddrItemObject *aio;
	GtkCMCTree *ctree = GTK_CMCTREE( addrbook.ctree );
	GtkCMCTreeNode *tn;

	node = _clipBoard_->objectList;
	while( node ) {
		cutItem = node->data;
		node = g_list_next( node );
		cache = addrindex_get_cache(
			_clipBoard_->addressIndex, cutItem->cacheID );
		if( cache == NULL ) continue;
		aio = addrcache_get_object( cache, cutItem->uid );
		if( aio ) {
			tn = NULL;
			if( ADDRITEM_TYPE(aio) == ITEMTYPE_FOLDER ) {
				ItemFolder *folder;

				folder = ( ItemFolder * ) aio;
				tn = gtk_cmctree_find_by_row_data_custom(
					ctree, NULL, folder,
					addressbook_treenode_find_folder_cb );
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_GROUP ) {
				ItemGroup *group;

				group = ( ItemGroup * ) aio;
				tn = gtk_cmctree_find_by_row_data_custom(
					ctree, NULL, group,
					addressbook_treenode_find_group_cb );
			}

			if( tn ) {
				/* Free up adapter and remove node. */
				gtk_cmctree_remove_node( ctree, tn );
			}
		}
	}
}

/**
 * Find parent datasource for specified tree node.
 * \param  node Node to test.
 * \return Data source, or NULL if not found.
 */
static AddressDataSource *addressbook_find_datasource( GtkCMCTreeNode *node ) {
	AddressDataSource *ds = NULL;
	AddressObject *ao;

	cm_return_val_if_fail(addrbook.ctree != NULL, NULL);

	while( node ) {
		if( GTK_CMCTREE_ROW(node)->level < 2 ) return NULL;
		ao = gtk_cmctree_node_get_row_data( GTK_CMCTREE(addrbook.ctree), node );
		if( ao ) {
			/* g_print( "ao->type = %d\n", ao->type ); */
			if( ao->type == ADDR_DATASOURCE ) {
				AdapterDSource *ads = ADAPTER_DSOURCE(ao);
				/* g_print( "found it\n" ); */
				ds = ads->dataSource;
				break;
			}
		}
		node = GTK_CMCTREE_ROW(node)->parent;
	}
	return ds;
}

/**
 * Load address list widget with children of specified object.
 * \param obj Parent object to be loaded.
 */
static void addressbook_set_clist( AddressObject *obj, gboolean refresh ) {
	GtkCMCTree *ctreelist = GTK_CMCTREE(addrbook.clist);
	GtkCMCList *clist = GTK_CMCLIST(addrbook.clist);
	AddressDataSource *ds = NULL;
	AdapterDSource *ads = NULL;
	static AddressObject *last_obj = NULL;

	if (addrbook.clist == NULL) {
		return;
	}
	if (obj == last_obj && !refresh)
		return;

	last_obj = obj;
	if( obj == NULL ) {
		gtk_cmclist_clear(clist);
		return;
	}

	if( obj->type == ADDR_INTERFACE ) {
		/* g_print( "set_clist: loading datasource...\n" ); */
		/* addressbook_node_load_datasource( GTK_CMCTREE(clist), obj ); */
		return;
	}

	gtk_cmclist_freeze(clist);
	gtk_cmclist_clear(clist);

	if( obj->type == ADDR_DATASOURCE ) {
		ads = ADAPTER_DSOURCE(obj);
		ds = ADAPTER_DSOURCE(obj)->dataSource;
		if( ds ) {
			/* Load root folder */
			ItemFolder *rootFolder = NULL;
			rootFolder = addrindex_ds_get_root_folder( ds );
			addressbook_folder_load_person(
				ctreelist, addrindex_ds_get_root_folder( ds ) );
			addressbook_folder_load_group(
				ctreelist, addrindex_ds_get_root_folder( ds ) );
		}
	}
	else {
		if( obj->type == ADDR_ITEM_GROUP ) {
			/* Load groups */
			ItemGroup *itemGroup = ADAPTER_GROUP(obj)->itemGroup;
			addressbook_load_group( ctreelist, itemGroup );
		}
		else if( obj->type == ADDR_ITEM_FOLDER ) {
			/* Load folders */
			ItemFolder *itemFolder = ADAPTER_FOLDER(obj)->itemFolder;
			addressbook_folder_load_person( ctreelist, itemFolder );
			addressbook_folder_load_group( ctreelist, itemFolder );
		}
	}
	gtk_sctree_sort_recursive(GTK_CMCTREE(clist), NULL);
	clist->focus_row = -1;
	gtk_cmclist_thaw(clist);
}

/**
 * Call back function to free adaptor. Call back is setup by function
 * gtk_cmctree_node_set_row_data_full() when node is populated. This function is
 * called when the address book tree widget node is removed by calling
 * function gtk_cmctree_remove_node().
 * 
 * \param data Tree node's row data.
 */
static void addressbook_free_treenode( gpointer data ) {
	AddressObject *ao;

	ao = ( AddressObject * ) data;
	if( ao == NULL ) return;
	if( ao->type == ADDR_INTERFACE ) {
		AdapterInterface *ai = ADAPTER_INTERFACE(ao);
		addrbookctl_free_interface( ai );
	}
	else if( ao->type == ADDR_DATASOURCE ) {
		AdapterDSource *ads = ADAPTER_DSOURCE(ao);
		addrbookctl_free_datasource( ads );
	}
	else if( ao->type == ADDR_ITEM_FOLDER ) {
		AdapterFolder *af = ADAPTER_FOLDER(ao);
		addrbookctl_free_folder( af );
	}
	else if( ao->type == ADDR_ITEM_GROUP ) {
		AdapterGroup *ag = ADAPTER_GROUP(ao);
		addrbookctl_free_group( ag );
	}
}

/*
* Create new adaptor for specified data source.
*/
AdapterDSource *addressbook_create_ds_adapter( AddressDataSource *ds,
				AddressObjectType otype, gchar *name )
{
	AdapterDSource *adapter = g_new0( AdapterDSource, 1 );
	ADDRESS_OBJECT(adapter)->type = ADDR_DATASOURCE;
	ADDRESS_OBJECT_NAME(adapter) = g_strdup( name );
	adapter->dataSource = ds;
	adapter->subType = otype;
	return adapter;
}

void addressbook_ads_set_name( AdapterDSource *adapter, gchar *value ) {
	ADDRESS_OBJECT_NAME(adapter) =
		mgu_replace_string( ADDRESS_OBJECT_NAME(adapter), value );
}

/*
 * Load tree from address index with the initial data.
 */
static void addressbook_load_tree( void ) {
	GtkCMCTree *ctree = GTK_CMCTREE( addrbook.ctree );
	GList *nodeIf, *nodeDS;
	AdapterInterface *adapter;
	AddressInterface *iface;
	AddressTypeControlItem *atci;
	AddressDataSource *ds;
	AdapterDSource *ads;
	GtkCMCTreeNode *node, *newNode;
	gchar *name;

	nodeIf = _addressInterfaceList_;
	while( nodeIf ) {
		adapter = nodeIf->data;
		node = adapter->treeNode;
		iface = adapter->interface;
		atci = adapter->atci;
		if( iface ) {
			if( iface->useInterface ) {
				/* Load data sources below interface node */
				nodeDS = iface->listSource;
				while( nodeDS ) {
					ds = nodeDS->data;
					newNode = NULL;
					name = addrindex_ds_get_name( ds );
					ads = addressbook_create_ds_adapter(
							ds, atci->objectType, name );
					newNode = addressbook_add_object(
							node, ADDRESS_OBJECT(ads) );
					nodeDS = g_list_next( nodeDS );
				}
				gtk_cmctree_expand( ctree, node );
			}
		}
		nodeIf = g_list_next( nodeIf );
	}
}

/*
 * Convert the old address book to new format.
 */
static gboolean addressbook_convert( AddressIndex *addrIndex ) {
	gboolean retVal = FALSE;
	gboolean errFlag = TRUE;
	gchar *msg = NULL;

	/* Read old address book, performing conversion */
	debug_print( "Reading and converting old address book...\n" );
	addrindex_set_file_name( addrIndex, ADDRESSBOOK_OLD_FILE );
	addrindex_read_data( addrIndex );
	if( addrIndex->retVal == MGU_NO_FILE ) {
		/* We do not have a file - new user */
		debug_print( "New user... create new books...\n" );
		addrindex_create_new_books( addrIndex );
		if( addrIndex->retVal == MGU_SUCCESS ) {
			/* Save index file */
			addrindex_set_file_name( addrIndex, ADDRESSBOOK_INDEX_FILE );
			addrindex_save_data( addrIndex );
			if( addrIndex->retVal == MGU_SUCCESS ) {
				retVal = TRUE;
				errFlag = FALSE;
			}
			else {
				msg = _( "New user, could not save index file." );
			}
		}
		else {
			msg = _( "New user, could not save address book files." );
		}
	}
	else {
		/* We have an old file */
		if( addrIndex->wasConverted ) {
			/* Converted successfully - save address index */
			addrindex_set_file_name( addrIndex, ADDRESSBOOK_INDEX_FILE );
			addrindex_save_data( addrIndex );
			if( addrIndex->retVal == MGU_SUCCESS ) {
				msg = _( "Old address book converted successfully." );
				retVal = TRUE;
				errFlag = FALSE;
			}
			else {
				msg = _("Old address book converted,\n"
					"could not save new address index file." );
			}
		}
		else {
			/* File conversion failed - just create new books */
			debug_print( "File conversion failed... just create new books...\n" );
			addrindex_create_new_books( addrIndex );
			if( addrIndex->retVal == MGU_SUCCESS ) {
				/* Save index */
				addrindex_set_file_name( addrIndex, ADDRESSBOOK_INDEX_FILE );
				addrindex_save_data( addrIndex );
				if( addrIndex->retVal == MGU_SUCCESS ) {
					msg = _("Could not convert address book,\n"
						"but created empty new address book files." );
					retVal = TRUE;
					errFlag = FALSE;
				}
				else {
					msg = _("Could not convert address book,\n"
						"could not save new address index file." );
				}
			}
			else {
				msg = _("Could not convert address book\n"
					"and could not create new address book files." );
			}
		}
	}
	if( errFlag ) {
		debug_print( "Error\n%s\n", msg );
		alertpanel_full(_("Addressbook conversion error"), msg,
				GTK_STOCK_CLOSE, NULL, NULL, FALSE,
				NULL, ALERT_ERROR, G_ALERTDEFAULT);
	}
	else if( msg ) {
		debug_print( "Warning\n%s\n", msg );
		alertpanel_full(_("Addressbook conversion error"), msg,
				GTK_STOCK_CLOSE, NULL, NULL, FALSE,
				NULL, ALERT_WARNING, G_ALERTDEFAULT);
	}

	return retVal;
}

static gboolean migrate_addrbook(const gchar *origdir, const gchar *destdir)
{
	DIR *dp;
	struct dirent *d;
	gboolean failed = FALSE;

	if( ( dp = opendir( origdir ) ) == NULL ) {
		return FALSE;
	}
	
	while( ( d = readdir( dp ) ) != NULL ) {
		if (strncmp(d->d_name, "addrbook-", strlen("addrbook-")))
			continue;
		else {
			gchar *orig_file = g_strconcat(origdir, G_DIR_SEPARATOR_S, 
					d->d_name, NULL);
			gchar *dest_file = g_strconcat(destdir, G_DIR_SEPARATOR_S, 
					d->d_name, NULL);
			if (copy_file(orig_file, dest_file, FALSE) < 0) {
				failed = TRUE;
			}
			g_free(orig_file);
			g_free(dest_file);
			if (failed) {
				break;
			}
		}
	}

	closedir( dp );
	if (!failed) {
		/* all copies succeeded, we can remove source files */
		if( ( dp = opendir( origdir ) ) == NULL ) {
			return FALSE;
		}
		while( ( d = readdir( dp ) ) != NULL ) {
			if (strncmp(d->d_name, "addrbook-", strlen("addrbook-")))
				continue;
			else {
				gchar *orig_file = g_strconcat(origdir, G_DIR_SEPARATOR_S, 
						d->d_name, NULL);
				claws_unlink(orig_file);
				g_free(orig_file);
			}
		}
		closedir( dp );
	}
	
	return !failed;
}

void addressbook_read_file( void ) {
	AddressIndex *addrIndex = NULL;
	gchar *indexdir = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, ADDRBOOK_DIR, NULL);
	
	debug_print( "Reading address index...\n" );
	if( _addressIndex_ ) {
		debug_print( "address book already read!!!\n" );
		return;
	}

	addrIndex = addrindex_create_index();
	addrindex_initialize();

	/* Use new address book index. */
	
	if ( !is_dir_exist(indexdir) ) {
		if ( make_dir(indexdir) < 0 ) {
			addrindex_set_file_path( addrIndex, get_rc_dir() );
			g_warning( "couldn't create dir %s\n", indexdir);
		} else {
			if (!migrate_addrbook(get_rc_dir(), indexdir)) {
				remove_dir_recursive(indexdir);
				addrindex_set_file_path( addrIndex, get_rc_dir() );
				g_error("couldn't migrate dir %s", indexdir);
			} else {
				addrindex_set_file_path( addrIndex, indexdir);
			}
		}
	} else {
		addrindex_set_file_path( addrIndex, indexdir);
	}
	g_free(indexdir);
	addrindex_set_file_name( addrIndex, ADDRESSBOOK_INDEX_FILE );
	addrindex_read_data( addrIndex );
	if( addrIndex->retVal == MGU_NO_FILE ) {
		/* Conversion required */
		debug_print( "Converting...\n" );
		if( addressbook_convert( addrIndex ) ) {
			_addressIndex_ = addrIndex;
		}
	}
	else if( addrIndex->retVal == MGU_SUCCESS ) {
		_addressIndex_ = addrIndex;
	}
	else {
		/* Error reading address book */
		debug_print( "Could not read address index.\n" );
		addrindex_print_index( addrIndex, stdout );
		alertpanel_full(_("Addressbook Error"),
				_("Could not read address index"),
				GTK_STOCK_CLOSE, NULL, NULL, FALSE,
				NULL, ALERT_ERROR, G_ALERTDEFAULT);
	}
	debug_print( "done.\n" );
}

/*
* Add object into the address index tree widget.
* Enter: node	Parent node.
*        obj	Object to add.
* Return: Node that was added, or NULL if object not added.
*/
static GtkCMCTreeNode *addressbook_add_object(GtkCMCTreeNode *node,
					    AddressObject *obj)
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	GtkCMCTreeNode *added;
	AddressObject *pobj;
	AddressObjectType otype;
	AddressTypeControlItem *atci = NULL;

	cm_return_val_if_fail(node != NULL, NULL);
	cm_return_val_if_fail(obj  != NULL, NULL);

	pobj = gtk_cmctree_node_get_row_data(ctree, node);
	cm_return_val_if_fail(pobj != NULL, NULL);

	/* Determine object type to be displayed */
	if( obj->type == ADDR_DATASOURCE ) {
		otype = ADAPTER_DSOURCE(obj)->subType;
	}
	else {
		otype = obj->type;
	}

	/* Handle any special conditions. */
	added = node;
	atci = addrbookctl_lookup( otype );
	if( atci ) {
		if( atci->showInTree ) {
			/* Add object to tree */
			gchar **name;
			name = &obj->name;
			added = gtk_sctree_insert_node( ctree, node, NULL, name, FOLDER_SPACING,
				atci->iconXpm, atci->iconXpmOpen,
				atci->treeLeaf, atci->treeExpand );
			gtk_cmctree_node_set_row_data_full( ctree, added, obj,
				addressbook_free_treenode );
		}
	}

	gtk_sctree_sort_node(ctree, node);

	return added;
}

/**
 * Add group into the address index tree.
 * \param  node      Parent node.
 * \param  ds        Data source.
 * \param  itemGroup Group to add.
 * \return Inserted node.
 */
static GtkCMCTreeNode *addressbook_node_add_group(
		GtkCMCTreeNode *node, AddressDataSource *ds,
		ItemGroup *itemGroup )
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	GtkCMCTreeNode *newNode;
	AdapterGroup *adapter;
	AddressTypeControlItem *atci = NULL;
	gchar **name;

	if( ds == NULL ) return NULL;
	if( node == NULL || itemGroup == NULL ) return NULL;

	name = &itemGroup->obj.name;

	atci = addrbookctl_lookup( ADDR_ITEM_GROUP );

	adapter = g_new0( AdapterGroup, 1 );
	ADDRESS_OBJECT_TYPE(adapter) = ADDR_ITEM_GROUP;
	ADDRESS_OBJECT_NAME(adapter) = g_strdup( ADDRITEM_NAME(itemGroup) );
	adapter->itemGroup = itemGroup;

	newNode = gtk_sctree_insert_node( ctree, node, NULL, name, FOLDER_SPACING,
			atci->iconXpm, atci->iconXpm,
			atci->treeLeaf, atci->treeExpand );
	gtk_cmctree_node_set_row_data_full( ctree, newNode, adapter,
		addressbook_free_treenode );
	gtk_sctree_sort_node( ctree, node );
	return newNode;
}

/**
 * Add folder into the address index tree. Only visible folders are loaded into
 * the address index tree. Note that the root folder is not inserted into the
 * tree.
 *
 * \param  node	      Parent node.
 * \param  ds         Data source.
 * \param  itemFolder Folder to add.
 * \param  otype      Object type to display.
 * \return Inserted node for the folder.
*/
static GtkCMCTreeNode *addressbook_node_add_folder(
		GtkCMCTreeNode *node, AddressDataSource *ds,
		ItemFolder *itemFolder, AddressObjectType otype )
{
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	GtkCMCTreeNode *newNode = NULL;
	AdapterFolder *adapter;
	AddressTypeControlItem *atci = NULL;
	GList *listItems = NULL;
	gchar *name;
	ItemFolder *rootFolder;

	/* Only visible folders */
	if( itemFolder == NULL || itemFolder->isHidden ) 
		return NULL;

	if( ds == NULL ) 
		return NULL;
	if( node == NULL || itemFolder == NULL ) 
		return NULL;

	/* Determine object type */
	atci = addrbookctl_lookup( otype );
	if( atci == NULL ) 
		return NULL;

	rootFolder = addrindex_ds_get_root_folder( ds );
	if( itemFolder == rootFolder ) {
		newNode = node;
	}
	else {
		adapter = g_new0( AdapterFolder, 1 );
		ADDRESS_OBJECT_TYPE(adapter) = ADDR_ITEM_FOLDER;
		ADDRESS_OBJECT_NAME(adapter) = g_strdup( ADDRITEM_NAME(itemFolder) );
		adapter->itemFolder = itemFolder;

		name = ADDRITEM_NAME(itemFolder);
		newNode = gtk_sctree_insert_node( ctree, node, NULL, &name, FOLDER_SPACING,
				atci->iconXpm, atci->iconXpm,
				atci->treeLeaf, atci->treeExpand );
		if( newNode ) {
			gtk_cmctree_node_set_row_data_full( ctree, newNode, adapter,
				addressbook_free_treenode );
		}
	}

	listItems = itemFolder->listFolder;
	while( listItems ) {
		ItemFolder *item = listItems->data;
		addressbook_node_add_folder( newNode, ds, item, otype );
		listItems = g_list_next( listItems );
	}
	listItems = itemFolder->listGroup;
	while( listItems ) {
		ItemGroup *item = listItems->data;
		addressbook_node_add_group( newNode, ds, item );
		listItems = g_list_next( listItems );
	}
	gtk_sctree_sort_node( ctree, node );
	return newNode;
}

void addressbook_export_to_file( void ) {
	if( _addressIndex_ ) {
		/* Save all new address book data */
		debug_print( "Saving address books...\n" );
		addrindex_save_all_books( _addressIndex_ );

		debug_print( "Exporting addressbook to file...\n" );
		addrindex_save_data( _addressIndex_ );
		if( _addressIndex_->retVal != MGU_SUCCESS ) {
			addrindex_print_index( _addressIndex_, stdout );
		}

		/* Notify address completion of new data */
		invalidate_address_completion();
	}
}

static gboolean addressbook_entry_key_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && (event->keyval == GDK_Return || event->keyval == GDK_KP_Enter))
		addressbook_lup_clicked(NULL, NULL);
	return FALSE;
}

/*
* Comparison using cell contents (text in first column). Used for sort
* address index widget.
*/
static gint addressbook_treenode_compare_func(
	GtkCMCList *clist, gconstpointer ptr1, gconstpointer ptr2 )
{
	GtkCMCell *cell1 = ((GtkCMCListRow *)ptr1)->cell;
	GtkCMCell *cell2 = ((GtkCMCListRow *)ptr2)->cell;
	gchar *name1 = NULL, *name2 = NULL;
	if( cell1 ) name1 = cell1->u.text;
	if( cell2 ) name2 = cell2->u.text;
	if( ! name1 ) return ( name2 != NULL );
	if( ! name2 ) return -1;
	return g_utf8_collate( name1, name2 );
}

static void addressbook_new_book_cb( GtkAction *action, gpointer data ) {
	AdapterDSource *ads;
	AdapterInterface *adapter;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_BOOK );
	if( adapter == NULL ) return;
	ads = addressbook_edit_book( _addressIndex_, NULL );
	if( ads ) {
		newNode = addressbook_add_object( adapter->treeNode, ADDRESS_OBJECT(ads) );
		if( newNode ) {
			gtk_sctree_select( GTK_SCTREE(addrbook.ctree), newNode );
			addrbook.treeSelected = newNode;
		}
	}
}

static void addressbook_new_vcard_cb( GtkAction *action, gpointer data ) {
	AdapterDSource *ads;
	AdapterInterface *adapter;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_VCARD );
	if( adapter == NULL ) return;
	ads = addressbook_edit_vcard( _addressIndex_, NULL );
	if( ads ) {
		newNode = addressbook_add_object( adapter->treeNode, ADDRESS_OBJECT(ads) );
		if( newNode ) {
			gtk_sctree_select( GTK_SCTREE(addrbook.ctree), newNode );
			addrbook.treeSelected = newNode;
		}
	}
}

#ifdef USE_JPILOT
static void addressbook_new_jpilot_cb( GtkAction *action, gpointer data ) {
	AdapterDSource *ads;
	AdapterInterface *adapter;
	AddressInterface *iface;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_JPILOT );
	if( adapter == NULL ) return;
	iface = adapter->interface;
	if( ! iface->haveLibrary ) return;
	ads = addressbook_edit_jpilot( _addressIndex_, NULL );
	if( ads ) {
		newNode = addressbook_add_object( adapter->treeNode, ADDRESS_OBJECT(ads) );
		if( newNode ) {
			gtk_sctree_select( GTK_SCTREE(addrbook.ctree), newNode );
			addrbook.treeSelected = newNode;
		}
	}
}
#endif

#ifdef USE_LDAP
static void addressbook_new_ldap_cb( GtkAction *action, gpointer data ) {
	AdapterDSource *ads;
	AdapterInterface *adapter;
	AddressInterface *iface;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_LDAP );
	if( adapter == NULL ) return;
	iface = adapter->interface;
	if( ! iface->haveLibrary ) return;
	ads = addressbook_edit_ldap( _addressIndex_, NULL );
	if( ads ) {
		newNode = addressbook_add_object( adapter->treeNode, ADDRESS_OBJECT(ads) );
		if( newNode ) {
			gtk_sctree_select( GTK_SCTREE(addrbook.ctree), newNode );
			addrbook.treeSelected = newNode;
		}
	}
}
#endif

/**
 * Display address search status message.
 * \param queryType Query type.
 * \param status    Status/Error code.
 */
static void addressbook_search_message( gint queryType, gint sts ) {
	gchar *desc = NULL;
	*addressbook_msgbuf = '\0';

	if( sts != MGU_SUCCESS ) {
		if( queryType == ADDRQUERY_LDAP ) {
#ifdef USE_LDAP			
			desc = addressbook_err2string( _lutErrorsLDAP_, sts );
#endif
		}
	}
	if( desc ) {
		g_snprintf( addressbook_msgbuf,
			sizeof(addressbook_msgbuf), "%s", desc );
		addressbook_status_show( addressbook_msgbuf );
	}
	else {
		addressbook_status_show( "" );
	}
}

/**
 * Refresh addressbook by forcing refresh of current selected object in
 * tree.
 */
static void addressbook_refresh_current( void ) {
	AddressObject *obj;
	GtkCMCTree *ctree;

	ctree = GTK_CMCTREE(addrbook.ctree);
	obj = gtk_cmctree_node_get_row_data( ctree, addrbook.treeSelected );
	if( obj == NULL ) return;
	addressbook_set_clist( obj, TRUE );
}

/**
 * Message that is displayed whilst a query is executing in a background
 * thread.
 */
static gchar *_tempMessage_ = N_( "Busy searching..." );

/**
 * Address search idle function. This function is called during UI idle time
 * while a search is in progress.
 *
 * \param data Idler data.
 */
static void addressbook_search_idle( gpointer data ) {
	/*
	gint queryID;

	queryID = GPOINTER_TO_INT( data );
	g_print( "addressbook_ldap_idle... queryID=%d\n", queryID );
	*/
}

/**
 * Search completion callback function. This removes the query from the idle
 * list.
 *
 * \param sender  Sender of query.
 * \param queryID Query ID of search request.
 * \param status  Search status.
 * \param data    Query data.
 */
static void addressbook_search_callback_end(
		gpointer sender, gint queryID, gint status, gpointer data )
{
	gpointer ptrQID;
	QueryRequest *req;
	AddrQueryObject *aqo;

	/* Remove idler function */
	ptrQID = GINT_TO_POINTER( queryID );
	if( ptrQID ) {
		g_idle_remove_by_data( ptrQID );
	}

	/* Refresh addressbook contents */
	addressbook_refresh_current();
	req = qrymgr_find_request( queryID );
	if( req != NULL ) {
		aqo = ( AddrQueryObject * ) req->queryList->data;
		addressbook_search_message( aqo->queryType, status );
	}

	/* Stop the search */
	addrindex_stop_search( queryID );
}

/**
 * Perform search.
 *
 * \param ds         Data source to search.
 * \param searchTerm String to lookup.
 * \param pNode      Parent data source node.
 */
static void addressbook_perform_search(
		AddressDataSource *ds, gchar *searchTerm,
		GtkCMCTreeNode *pNode )
{
	AddrBookBase *adbase;
	AddressCache *cache;
	ItemFolder *folder;
	gchar *name;
	gint queryID;
	guint idleID;
#ifdef USE_LDAP
	AddressObjectType aoType = ADDR_NONE;
#endif

	/* Setup a query */
	if( *searchTerm == '\0' || strlen( searchTerm ) < 1 ) return;

	if( ds && ds->type == ADDR_IF_LDAP ) {
#if USE_LDAP
		aoType = ADDR_LDAP_QUERY;
#endif
	}
	else {
		return;
	}
	/* Get reference to address cache */	
	adbase = ( AddrBookBase * ) ds->rawDataSource;
	cache = adbase->addressCache;

	/* Create a folder for the search results */
	name = g_strdup_printf( _queryFolderLabel_, searchTerm );
	folder = addressbook_setup_subf(ds, name, pNode);
	g_free( name );

	/* Setup the search */
	queryID = addrindex_setup_explicit_search(
		ds, searchTerm, folder, addressbook_search_callback_end, NULL );
	if( queryID == 0 ) return;

	/* Set up idler function */
	idleID = g_idle_add(
			(GSourceFunc) addressbook_search_idle,
			GINT_TO_POINTER( queryID ) );

	/* Start search, sit back and wait for something to happen */
	addrindex_start_search( queryID );

	addressbook_status_show( _tempMessage_ );
}

/**
 * Lookup button handler. Address search is only performed against
 * address interfaces for external queries.
 *
 * \param button Lookup button widget.
 * \param data   Data object.
 */
static void addressbook_lup_clicked( GtkButton *button, gpointer data ) {
	GtkCMCTree *ctree;
	AddressObject *obj;
	AddressDataSource *ds;
	AddressInterface *iface;
	gchar *searchTerm;
	GtkCMCTreeNode *node, *parentNode;

	node = addrbook.treeSelected;
	if( ! node ) return;
	if( GTK_CMCTREE_ROW(node)->level == 1 ) return;

	ctree = GTK_CMCTREE(addrbook.ctree);
	obj = gtk_cmctree_node_get_row_data( ctree, node );
	if( obj == NULL ) return;

	ds = addressbook_find_datasource( node );
	if( ds == NULL ) return;

	/* We must have a datasource that is an external interface */
	iface = ds->interface;
	if( ! iface->haveLibrary ) return;
	if( ! iface->externalQuery ) return;

	searchTerm =
		gtk_editable_get_chars( GTK_EDITABLE(addrbook.entry), 0, -1 );
	g_strchomp( searchTerm );

	if( obj->type == ADDR_ITEM_FOLDER ) {
		parentNode = GTK_CMCTREE_ROW(node)->parent;
	}
	else {
		parentNode = node;
	}
	addressbook_perform_search( ds, searchTerm, parentNode );
	
	gtk_widget_grab_focus( addrbook.entry );

	g_free( searchTerm );
}

static void addressbook_close_clicked( GtkButton *button, gpointer data ) {
	addressbook_close();
}

#ifdef USE_LDAP
/**
 * Browse address entry for highlighted entry.
 */
static void addressbook_browse_entry_cb( GtkAction *action, gpointer data)
{
	GtkCMCTree *clist = GTK_CMCTREE(addrbook.clist);
	AddressObject *obj;
	AddressDataSource *ds;
	AddressInterface *iface;
	ItemPerson *person;
	ItemEMail *email;

	if(addrbook.listSelected == NULL)
		return;

	obj = gtk_cmctree_node_get_row_data(clist, addrbook.listSelected);
	if (obj == NULL)
		return;

	ds = addressbook_find_datasource(GTK_CMCTREE_NODE(addrbook.treeSelected));
	if(ds == NULL)
		return;

	iface = ds->interface;
	if(!iface || !iface->haveLibrary )
		return;

	person = NULL;
	if (obj->type == ADDR_ITEM_EMAIL) {
		email = ( ItemEMail * ) obj;
		if (email == NULL)
			return;
		
		person = (ItemPerson *) ADDRITEM_PARENT(email);
	}
	else if (obj->type == ADDR_ITEM_PERSON) {
		person = (ItemPerson *) obj;
	}
	else {
		/* None of these */
		return;
	}

	if( iface && iface->type == ADDR_IF_LDAP ) {
		browseldap_entry(ds, person->externalID);
	}
}
#endif

/* **********************************************************************
* Build lookup tables.
* ***********************************************************************
*/

/*
 * Remap object types.
 * Enter:  abType AddressObjectType (used in tree node).
 * Return: ItemObjectType (used in address cache data).
 */
ItemObjectType addressbook_type2item( AddressObjectType abType ) {
	ItemObjectType ioType;

	switch( abType ) {
		case ADDR_ITEM_PERSON: ioType = ITEMTYPE_PERSON;     break;
		case ADDR_ITEM_EMAIL:  ioType = ITEMTYPE_EMAIL;      break;
		case ADDR_ITEM_FOLDER: ioType = ITEMTYPE_FOLDER;     break;
		case ADDR_ITEM_GROUP:  ioType = ITEMTYPE_GROUP;      break;
		case ADDR_DATASOURCE:  ioType = ITEMTYPE_DATASOURCE; break;
		default:               ioType = ITEMTYPE_NONE;       break;
	}
	return ioType;
}

#define UPDATE_ICON_ATCI(id,icon,iconopen) {			\
	atci = addrbookctl_lookup(id);				\
	if (atci) {						\
		atci->iconXpm = icon;				\
		atci->iconXpmOpen = iconopen;			\
	} else {						\
		g_warning("can't get atci %d\n", id);		\
	}							\
}

/*
* Build table that controls the rendering of object types.
*/
static void addrbookctl_build_icons( GtkWidget *window ) {
	AddressTypeControlItem *atci;

	/* Build icons */
	if (interfacexpm)
		g_object_unref(interfacexpm);
	if (folderxpm)
		g_object_unref(folderxpm);
	if (folderopenxpm)
		g_object_unref(folderopenxpm);
	if (groupxpm)
		g_object_unref(groupxpm);
	if (vcardxpm)
		g_object_unref(vcardxpm);
	if (bookxpm)
		g_object_unref(bookxpm);
	if (addressxpm)
		g_object_unref(addressxpm);
	if (jpilotxpm)
		g_object_unref(jpilotxpm);
	if (categoryxpm)
		g_object_unref(categoryxpm);
	if (ldapxpm)
		g_object_unref(ldapxpm);
	if (addrsearchxpm)
		g_object_unref(addrsearchxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_INTERFACE, &interfacexpm );
	stock_pixbuf_gdk(window, STOCK_PIXMAP_DIR_CLOSE, &folderxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_DIR_OPEN, &folderopenxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_GROUP, &groupxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_VCARD, &vcardxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_BOOK, &bookxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_ADDRESS, &addressxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_JPILOT, &jpilotxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_CATEGORY, &categoryxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_LDAP, &ldapxpm);
	stock_pixbuf_gdk(window, STOCK_PIXMAP_ADDRESS_SEARCH, &addrsearchxpm);
	
	UPDATE_ICON_ATCI(ADDR_INTERFACE,folderxpm,folderopenxpm);
	UPDATE_ICON_ATCI(ADDR_BOOK,bookxpm,bookxpm);
	UPDATE_ICON_ATCI(ADDR_ITEM_PERSON,NULL,NULL);
	UPDATE_ICON_ATCI(ADDR_ITEM_EMAIL,addressxpm,addressxpm);
	UPDATE_ICON_ATCI(ADDR_ITEM_GROUP,groupxpm,groupxpm);
	UPDATE_ICON_ATCI(ADDR_ITEM_FOLDER,folderxpm,folderopenxpm);
	UPDATE_ICON_ATCI(ADDR_VCARD,vcardxpm,vcardxpm);
	UPDATE_ICON_ATCI(ADDR_JPILOT,jpilotxpm,jpilotxpm);
	UPDATE_ICON_ATCI(ADDR_CATEGORY,categoryxpm,categoryxpm);
	UPDATE_ICON_ATCI(ADDR_LDAP,ldapxpm,ldapxpm);
	UPDATE_ICON_ATCI(ADDR_LDAP_QUERY,addrsearchxpm,addrsearchxpm);

}

/*
* Build table that controls the rendering of object types.
*/
static void addrbookctl_build_map( GtkWidget *window ) {
	AddressTypeControlItem *atci;

	_addressBookTypeHash_ = g_hash_table_new( g_int_hash, g_int_equal );
	_addressBookTypeList_ = NULL;

	/* Interface */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_INTERFACE;
	atci->interfaceType = ADDR_IF_NONE;
	atci->showInTree = TRUE;
	atci->treeExpand = TRUE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "Interface" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* Address book */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_BOOK;
	atci->interfaceType = ADDR_IF_BOOK;
	atci->showInTree = TRUE;
	atci->treeExpand = TRUE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "Address Book" );
	atci->menuCommand = "Menu/Book/NewBook";
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* Item person */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_ITEM_PERSON;
	atci->interfaceType = ADDR_IF_NONE;
	atci->showInTree = FALSE;
	atci->treeExpand = FALSE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "Person" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* Item email */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_ITEM_EMAIL;
	atci->interfaceType = ADDR_IF_NONE;
	atci->showInTree = FALSE;
	atci->treeExpand = FALSE;
	atci->treeLeaf = TRUE;
	atci->displayName = _( "Email Address" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* Item group */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_ITEM_GROUP;
	atci->interfaceType = ADDR_IF_BOOK;
	atci->showInTree = TRUE;
	atci->treeExpand = FALSE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "Group" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* Item folder */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_ITEM_FOLDER;
	atci->interfaceType = ADDR_IF_BOOK;
	atci->showInTree = TRUE;
	atci->treeExpand = FALSE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "Folder" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* vCard */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_VCARD;
	atci->interfaceType = ADDR_IF_VCARD;
	atci->showInTree = TRUE;
	atci->treeExpand = TRUE;
	atci->treeLeaf = TRUE;
	atci->displayName = _( "vCard" );
	atci->menuCommand = "Menu/Book/NewVCard";
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* J-Pilot */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_JPILOT;
	atci->interfaceType = ADDR_IF_JPILOT;
	atci->showInTree = TRUE;
	atci->treeExpand = TRUE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "JPilot" );
	atci->menuCommand = "Menu/Book/NewJPilot";
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* Category */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_CATEGORY;
	atci->interfaceType = ADDR_IF_JPILOT;
	atci->showInTree = TRUE;
	atci->treeExpand = TRUE;
	atci->treeLeaf = TRUE;
	atci->displayName = _( "JPilot" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* LDAP Server */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_LDAP;
	atci->interfaceType = ADDR_IF_LDAP;
	atci->showInTree = TRUE;
	atci->treeExpand = TRUE;
	atci->treeLeaf = FALSE;
	atci->displayName = _( "LDAP servers" );
	atci->menuCommand = "Menu/Book/NewLDAPServer";
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	/* LDAP Query  */
	atci = g_new0( AddressTypeControlItem, 1 );
	atci->objectType = ADDR_LDAP_QUERY;
	atci->interfaceType = ADDR_IF_LDAP;
	atci->showInTree = TRUE;
	atci->treeExpand = FALSE;
	atci->treeLeaf = TRUE;
	atci->displayName = _( "LDAP Query" );
	atci->menuCommand = NULL;
	g_hash_table_insert( _addressBookTypeHash_, &atci->objectType, atci );
	_addressBookTypeList_ = g_list_append( _addressBookTypeList_, atci );

	addrbookctl_build_icons(window);
}

void addressbook_reflect_prefs_pixmap_theme(void)
{
	if (addrbook.window)
		addrbookctl_build_icons(addrbook.window);
}

/*
* Search for specified object type.
*/
static AddressTypeControlItem *addrbookctl_lookup( gint ot ) {
	gint objType = ot;
	return ( AddressTypeControlItem * ) g_hash_table_lookup( _addressBookTypeHash_, &objType );
}

/*
* Search for specified interface type.
*/
static AddressTypeControlItem *addrbookctl_lookup_iface( AddressIfType ifType ) {
	GList *node = _addressBookTypeList_;
	while( node ) {
		AddressTypeControlItem *atci = node->data;
		if( atci->interfaceType == ifType ) return atci;
		node = g_list_next( node );
	}
	return NULL;
}

static void addrbookctl_free_address( AddressObject *obj ) {
	g_free( obj->name );
	obj->type = ADDR_NONE;
	obj->name = NULL;
}

static void addrbookctl_free_interface( AdapterInterface *adapter ) {
	addrbookctl_free_address( ADDRESS_OBJECT(adapter) );
	adapter->interface = NULL;
	adapter->interfaceType = ADDR_IF_NONE;
	adapter->atci = NULL;
	adapter->enabled = FALSE;
	adapter->haveLibrary = FALSE;
	adapter->treeNode = NULL;
	g_free( adapter );
}

static void addrbookctl_free_datasource( AdapterDSource *adapter ) {
	addrbookctl_free_address( ADDRESS_OBJECT(adapter) );
	adapter->dataSource = NULL;
	adapter->subType = ADDR_NONE;
	g_free( adapter );
}

static void addrbookctl_free_folder( AdapterFolder *adapter ) {
	addrbookctl_free_address( ADDRESS_OBJECT(adapter) );
	adapter->itemFolder = NULL;
	g_free( adapter );
}

static void addrbookctl_free_group( AdapterGroup *adapter ) {
	addrbookctl_free_address( ADDRESS_OBJECT(adapter) );
	adapter->itemGroup = NULL;
	g_free( adapter );
}

/**
 * Build GUI interface list.
 */
static void addrbookctl_build_iflist( void ) {
	AddressTypeControlItem *atci;
	AdapterInterface *adapter;
	GList *list = NULL;

	if( _addressIndex_ == NULL ) {
		_addressIndex_ = addrindex_create_index();
		if( _clipBoard_ == NULL ) {
			_clipBoard_ = addrclip_create();
		}
		addrclip_set_index( _clipBoard_, _addressIndex_ );
	}
	_addressInterfaceList_ = NULL;
	list = addrindex_get_interface_list( _addressIndex_ );
	while( list ) {
		AddressInterface *interface = list->data;
		atci = addrbookctl_lookup_iface( interface->type );
		if( atci ) {
			adapter = g_new0( AdapterInterface, 1 );
			adapter->interfaceType = interface->type;
			adapter->atci = atci;
			adapter->interface = interface;
			adapter->treeNode = NULL;
			adapter->enabled = TRUE;
			adapter->haveLibrary = interface->haveLibrary;
			ADDRESS_OBJECT(adapter)->type = ADDR_INTERFACE;
			ADDRESS_OBJECT_NAME(adapter) = g_strdup( atci->displayName );
			_addressInterfaceList_ =
				g_list_append( _addressInterfaceList_, adapter );
		}
		list = g_list_next( list );
	}
}

/**
 * Find GUI interface type specified interface type.
 * \param  ifType Interface type.
 * \return Interface item, or NULL if not found.
 */
static AdapterInterface *addrbookctl_find_interface( AddressIfType ifType ) {
	GList *node = _addressInterfaceList_;
	while( node ) {
		AdapterInterface *adapter = node->data;
		if( adapter->interfaceType == ifType ) return adapter;
		node = g_list_next( node );
	}
	return NULL;
}

/**
 * Build interface list selection.
 */
static void addrbookctl_build_ifselect( void ) {
	GList *newList = NULL;
	gchar *selectStr;
	gchar **splitStr;
	gint ifType;
	gint i;
	gchar *endptr = NULL;
	gboolean enabled;
	AdapterInterface *adapter;

	selectStr = g_strdup( ADDRESSBOOK_IFACE_SELECTION );

	/* Parse string */
	splitStr = g_strsplit( selectStr, ",", -1 );
	for( i = 0; i < ADDRESSBOOK_MAX_IFACE; i++ ) {
		if( splitStr[i] ) {
			/* g_print( "%d : %s\n", i, splitStr[i] ); */
			ifType = strtol( splitStr[i], &endptr, 10 );
			enabled = TRUE;
			if( *endptr ) {
				if( strcmp( endptr, "/n" ) == 0 ) {
					enabled = FALSE;
				}
			}
			/* g_print( "\t%d : %s\n", ifType, enabled ? "yes" : "no" ); */
			adapter = addrbookctl_find_interface( ifType );
			if( adapter ) {
				newList = g_list_append( newList, adapter );
			}
		}
		else {
			break;
		}
	}
	/* g_print( "i=%d\n", i ); */
	g_strfreev( splitStr );
	g_free( selectStr );

	/* Replace existing list */
	mgu_clear_list( _addressIFaceSelection_ );
	g_list_free( _addressIFaceSelection_ );
	_addressIFaceSelection_ = newList;
	newList = NULL;
}

/* ***********************************************************************
 * Add sender to address book.
 * ***********************************************************************
 */

/*
 * This function is used by the Add sender to address book function.
 */
gboolean addressbook_add_contact(
		const gchar *name, const gchar *address, const gchar *remarks,
		GdkPixbuf *picture )
{
	debug_print( "addressbook_add_contact: name/address: %s - %s\n", name, address );
	if( addressadd_selection( _addressIndex_, name, address, remarks, picture ) ) {
		debug_print( "addressbook_add_contact - added\n" );
		addressbook_refresh();
	}
	return TRUE;
}

/* ***********************************************************************
 * Book/folder selection.
 * ***********************************************************************
 */

/*
 * This function is used by the matcher dialog to select a book/folder.
 */
gchar *addressbook_folder_selection( const gchar *folderpath)
{
	AddressBookFile *book = NULL;
	ItemFolder *folder = NULL;
	gchar *path = NULL;

	cm_return_val_if_fail( folderpath != NULL, NULL);

	if ( addressbook_foldersel_selection( _addressIndex_, &book, &folder, folderpath )
		&& book != NULL ) {
		if ( folder != NULL) {
			gchar *tmp = NULL;
			gchar *oldtmp = NULL;
			AddrItemObject *obj = NULL;

			/* walk thru folder->parent to build the full folder path */
			/* TODO: wwp: optimize this */
			obj = &folder->obj;
			tmp = g_strdup(obj->uid);
			while ( obj->parent ) {
				obj = obj->parent;
				if ( obj->name != NULL ) {
					oldtmp = g_strdup(tmp);
					g_free(tmp);
					tmp = g_strdup_printf("%s/%s", obj->uid, oldtmp);
					g_free(oldtmp);
				}
			}
			path = g_strdup_printf("%s/%s", book->fileName, tmp);
			g_free(tmp);
		} else {
			path = g_strdup_printf("%s", book->fileName);
		}
		debug_print( "addressbook_foldersel: %s\n", path?path:"(null)");
		return path;
	}
	return NULL;
}

/* ***********************************************************************
 * Book/folder checking.
 * ***********************************************************************
 */

static FolderInfo *addressbook_peek_subfolder_exists_create_folderinfo( AddressBookFile *abf, ItemFolder *folder )
{
	FolderInfo *fi = g_new0( FolderInfo, 1 );
	fi->book   = abf;
	fi->folder = folder;
	return fi;
}

static void addressbook_peek_subfolder_exists_load_folder( ItemFolder *parentFolder,
					FolderInfo *fiParent, FolderPathMatch *match )
{
	GList *list;
	ItemFolder *folder;
	gchar *fName;
	FolderInfo *fi;
	FolderPathMatch *nextmatch = NULL;

	if (!parentFolder)
		return;

	list = parentFolder->listFolder;
	while ( list ) {
		folder = list->data;
		fName = g_strdup( ADDRITEM_NAME(folder) );

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
					match->folder = folder;
					debug_print("book/folder path matched!\n");
				} else {
					/* keep on matching */
					nextmatch = match;
				}
			}
		}

		g_free( fName );

		fi = addressbook_peek_subfolder_exists_create_folderinfo( fiParent->book, folder );
		addressbook_peek_subfolder_exists_load_folder( folder, fi, nextmatch );
		g_free(fi);
		list = g_list_next( list );
	}
}

/*
 * This function is used by to check if a matcher book/folder path corresponds to an
   existing addressbook book/folder ("" or "Any" are considered as valid, NULL invalid).
   Caution: returned book and folder pointers can be NULL even when returning TRUE:
   if book AND folder are NULL this means that folderpath was empty or Any.
   If folderpath is a simple book name (without folder), book will not be NULL and folder
   will be NULL. It's not expected to return book as NULL and folder as non NULL.
 */

gboolean addressbook_peek_folder_exists( gchar *folderpath,
										 AddressDataSource **book,
										 ItemFolder **folder )
{
	AddressDataSource *ds;
	GList *list, *nodeDS;
	ItemFolder *rootFolder;
	AddressBookFile *abf;
	FolderInfo *fi;
	FolderPathMatch folder_path_match = { NULL, FALSE, 0, NULL, NULL };

	if ( book )
		*book = NULL;
	if ( folder )
		*folder = NULL;

	if ( folderpath == NULL )
		return FALSE;

	if ( strcasecmp(folderpath, "Any") == 0 || *folderpath == '\0' )
		return TRUE;

	/* split the folder path we've received, we'll try to match this path, subpath by
	   subpath against the book/folder structure in order */
	folder_path_match.folder_path = g_strsplit( folderpath, "/", 256 );
	if (!folder_path_match.folder_path)
		return FALSE;

	list = addrindex_get_interface_list( _addressIndex_ );
	while ( list && !folder_path_match.matched ) {
		AddressInterface *interface = list->data;
		if ( interface && interface->type == ADDR_IF_BOOK ) {
			nodeDS = interface->listSource;
			while ( nodeDS && !folder_path_match.matched ) {
				ds = nodeDS->data;

				/* Read address book */
				if( ! addrindex_ds_get_read_flag( ds ) ) {
					addrindex_ds_read_data( ds );
				}

				/* Add node for address book */
				abf = ds->rawDataSource;

				/* match book name */
				if ( abf && abf->fileName &&
				    strcmp(folder_path_match.folder_path[0], abf->fileName) == 0 ) {

					debug_print("matched book name '%s'\n", abf->fileName);
					folder_path_match.book = ds;

					if ( folder_path_match.folder_path[1] == NULL ) {
						/* no folder part to match */

						folder_path_match.matched = TRUE;
						folder_path_match.folder = NULL;
						debug_print("book path matched!\n");

					} else {
						/* match folder part */

						fi = addressbook_peek_subfolder_exists_create_folderinfo( abf, NULL );
						rootFolder = addrindex_ds_get_root_folder( ds );

						/* prepare for recursive call */
						folder_path_match.index = 1;
						/* this call will set folder_path_match.matched and folder_path_match.folder */
						addressbook_peek_subfolder_exists_load_folder( rootFolder, fi, &folder_path_match );
						g_free(fi);
					}
				}

				nodeDS = g_list_next( nodeDS );
			}
		}
		list = g_list_next( list );
	}

	g_strfreev( folder_path_match.folder_path );

	if ( book )
		*book = folder_path_match.book;
	if ( folder )
		*folder = folder_path_match.folder;
	return folder_path_match.matched;
}


/* **********************************************************************
 * Address Import.
 * ***********************************************************************
 */

/**
 * Import LDIF file.
 */
static void addressbook_import_ldif_cb( GtkAction *action, gpointer data ) {
	AddressDataSource *ds = NULL;
	AdapterDSource *ads = NULL;
	AddressBookFile *abf = NULL;
	AdapterInterface *adapter;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_BOOK );
	if( adapter ) {
		if( adapter->treeNode ) {
			abf = addressbook_imp_ldif( _addressIndex_ );
			if( abf ) {
				ds = addrindex_index_add_datasource(
					_addressIndex_, ADDR_IF_BOOK, abf );
				ads = addressbook_create_ds_adapter(
					ds, ADDR_BOOK, NULL );
				addressbook_ads_set_name(
					ads, addrbook_get_name( abf ) );
				newNode = addressbook_add_object(
					adapter->treeNode,
					ADDRESS_OBJECT(ads) );
				if( newNode ) {
					gtk_sctree_select( GTK_SCTREE(addrbook.ctree),
						newNode );
					addrbook.treeSelected = newNode;
				}

				/* Notify address completion */
				invalidate_address_completion();
			}
		}
	}
}

/**
 * Import MUTT file.
 */
static void addressbook_import_mutt_cb( GtkAction *action, gpointer data ) {
	AddressDataSource *ds = NULL;
	AdapterDSource *ads = NULL;
	AddressBookFile *abf = NULL;
	AdapterInterface *adapter;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_BOOK );
	if( adapter ) {
		if( adapter->treeNode ) {
			abf = addressbook_imp_mutt( _addressIndex_ );
			if( abf ) {
				ds = addrindex_index_add_datasource(
					_addressIndex_, ADDR_IF_BOOK, abf );
				ads = addressbook_create_ds_adapter(
					ds, ADDR_BOOK, NULL );
				addressbook_ads_set_name(
					ads, addrbook_get_name( abf ) );
				newNode = addressbook_add_object(
					adapter->treeNode,
					ADDRESS_OBJECT(ads) );
				if( newNode ) {
					gtk_sctree_select( GTK_SCTREE(addrbook.ctree),
						newNode );
					addrbook.treeSelected = newNode;
				}

				/* Notify address completion */
				invalidate_address_completion();
			}
		}
	}
}

/**
 * Import Pine file.
 */
static void addressbook_import_pine_cb( GtkAction *action, gpointer data ) {
	AddressDataSource *ds = NULL;
	AdapterDSource *ads = NULL;
	AddressBookFile *abf = NULL;
	AdapterInterface *adapter;
	GtkCMCTreeNode *newNode;

	adapter = addrbookctl_find_interface( ADDR_IF_BOOK );
	if( adapter ) {
		if( adapter->treeNode ) {
			abf = addressbook_imp_pine( _addressIndex_ );
			if( abf ) {
				ds = addrindex_index_add_datasource(
					_addressIndex_, ADDR_IF_BOOK, abf );
				ads = addressbook_create_ds_adapter(
					ds, ADDR_BOOK, NULL );
				addressbook_ads_set_name(
					ads, addrbook_get_name( abf ) );
				newNode = addressbook_add_object(
					adapter->treeNode,
					ADDRESS_OBJECT(ads) );
				if( newNode ) {
					gtk_sctree_select( GTK_SCTREE(addrbook.ctree),
						newNode );
					addrbook.treeSelected = newNode;
				}

				/* Notify address completion */
				invalidate_address_completion();
			}
		}
	}
}

/**
 * Harvest addresses.
 * \param folderItem Folder to import.
 * \param sourceInd  Source indicator: FALSE - Folder, TRUE - Messages.
 * \param msgList    List of message numbers, or NULL to process folder.
 */
void addressbook_harvest(
	FolderItem *folderItem, gboolean sourceInd, GList *msgList )
{
	AddressDataSource *ds = NULL;
	AdapterDSource *ads = NULL;
	AddressBookFile *abf = NULL;
	AdapterInterface *adapter;
	GtkCMCTreeNode *newNode;

	abf = addrgather_dlg_execute(
		folderItem, _addressIndex_, sourceInd, msgList );
	if( abf ) {
		ds = addrindex_index_add_datasource(
			_addressIndex_, ADDR_IF_BOOK, abf );

		adapter = addrbookctl_find_interface( ADDR_IF_BOOK );
		if( adapter ) {
			if( adapter->treeNode ) {
				ads = addressbook_create_ds_adapter(
					ds, ADDR_BOOK, addrbook_get_name( abf ) );
				newNode = addressbook_add_object(
						adapter->treeNode,
						ADDRESS_OBJECT(ads) );
			}
		}

		/* Notify address completion */
		invalidate_address_completion();
	}
}

/**
 * Export HTML file.
 */
static void addressbook_export_html_cb( GtkAction *action, gpointer data ) {
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	AddressObject *obj;
	AddressDataSource *ds = NULL;
	AddrBookBase *adbase;
	AddressCache *cache;
	GtkCMCTreeNode *node = NULL;

	if( ! addrbook.treeSelected ) return;
	node = addrbook.treeSelected;
	if( GTK_CMCTREE_ROW(node)->level == 1 ) return;
	obj = gtk_cmctree_node_get_row_data( ctree, node );
	if( obj == NULL ) return;

	ds = addressbook_find_datasource( node );
	if( ds == NULL ) return;
	adbase = ( AddrBookBase * ) ds->rawDataSource;
	cache = adbase->addressCache;
	addressbook_exp_html( cache );
}

/**
 * Export LDIF file.
 */
static void addressbook_export_ldif_cb( GtkAction *action, gpointer data ) {
	GtkCMCTree *ctree = GTK_CMCTREE(addrbook.ctree);
	AddressObject *obj;
	AddressDataSource *ds = NULL;
	AddrBookBase *adbase;
	AddressCache *cache;
	GtkCMCTreeNode *node = NULL;

	if( ! addrbook.treeSelected ) return;
	node = addrbook.treeSelected;
	if( GTK_CMCTREE_ROW(node)->level == 1 ) return;
	obj = gtk_cmctree_node_get_row_data( ctree, node );
	if( obj == NULL ) return;

	ds = addressbook_find_datasource( node );
	if( ds == NULL ) return;
	adbase = ( AddrBookBase * ) ds->rawDataSource;
	cache = adbase->addressCache;
	addressbook_exp_ldif( cache );
}

static void addressbook_find_duplicates_cb(GtkAction *action, gpointer data)
{
	addrduplicates_find(GTK_WINDOW(addrbook.window));	
}

static void addressbook_edit_custom_attr_cb(GtkAction *action, gpointer data)
{
	addressbook_custom_attr_edit();
}
		
static void addressbook_start_drag(GtkWidget *widget, gint button, 
				   GdkEvent *event,
			           void *data)
{
	GdkDragContext *context;
	if (addressbook_target_list == NULL)
		addressbook_target_list = gtk_target_list_new(
				addressbook_drag_types, 1);
	context = gtk_drag_begin(widget, addressbook_target_list,
				 GDK_ACTION_MOVE|GDK_ACTION_COPY|GDK_ACTION_DEFAULT, button, event);
	gtk_drag_set_icon_default(context);
}

static void addressbook_drag_data_get(GtkWidget        *widget,
				     GdkDragContext   *drag_context,
				     GtkSelectionData *selection_data,
				     guint             info,
				     guint             time,
				     void	      *data)
{
	AddrItemObject *aio = NULL;
	AddressObject *pobj = NULL;
	AdapterDSource *ads = NULL;
	AddressDataSource *ds = NULL;
	GList *cur;

	pobj = gtk_cmctree_node_get_row_data( GTK_CMCTREE(addrbook.ctree), addrbook.treeSelected );

	if( pobj == NULL ) return;

	if( pobj->type == ADDR_DATASOURCE ) {
		ads = ADAPTER_DSOURCE(pobj);
		ds = ads->dataSource;
	} else if (pobj->type == ADDR_ITEM_GROUP) {

		return;
	}
	
	else if( pobj->type != ADDR_INTERFACE ) {
		ds = addressbook_find_datasource( addrbook.treeSelected );

		if (!ds)
			return;
	}
	
	for(cur = GTK_CMCLIST(addrbook.clist)->selection; cur; cur = cur->next) {
		aio = (AddrItemObject *)gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.clist),
			GTK_CMCTREE_NODE(cur->data));
		while (aio && aio->type != ADDR_ITEM_PERSON) {
			aio = aio->parent;
		}
       }

	if (aio && aio->type == ADDR_ITEM_PERSON) {
		if( ds && ds->interface && ds->interface->readOnly)
			gtk_selection_data_set(selection_data,
				       selection_data->target, 8,
				       (const guchar *)"Dummy_addr_copy", 15);
		else
			gtk_selection_data_set(selection_data,
				       selection_data->target, 8,
				       (const guchar *)"Dummy_addr_move", 15);
	} 
}

static gboolean addressbook_drag_motion_cb(GtkWidget      *widget,
					  GdkDragContext *context,
					  gint            x,
					  gint            y,
					  guint           time,
					  void            *data)
{
	gint row, column;
	GtkCMCTreeNode *node = NULL;
	gboolean acceptable = FALSE;
	gint height = addrbook.ctree->allocation.height;
	gint total_height = addrbook.ctree->requisition.height;
	GtkAdjustment *pos = gtk_scrolled_window_get_vadjustment(
				GTK_SCROLLED_WINDOW(addrbook.ctree_swin));
	gfloat vpos = pos->value;
	
	if (gtk_cmclist_get_selection_info
		(GTK_CMCLIST(widget), x - 24, y - 24, &row, &column)) {

		if (y > height - 24 && height + vpos < total_height) {
			gtk_adjustment_set_value(pos, (vpos+5 > height ? height : vpos+5));
			gtk_adjustment_changed(pos);
		}
		if (y < 24 && y > 0) {
			gtk_adjustment_set_value(pos, (vpos-5 < 0 ? 0 : vpos-5));
			gtk_adjustment_changed(pos);
		}
		node = gtk_cmctree_node_nth(GTK_CMCTREE(widget), row);

		if (node != NULL) {
			AddressObject *obj = gtk_cmctree_node_get_row_data(GTK_CMCTREE(widget), node );
			if( obj->type == ADDR_ITEM_FOLDER 
			|| obj->type == ADDR_ITEM_GROUP)
				acceptable = TRUE;
			else {
				AdapterDSource *ads = NULL;
				AddressDataSource *ds = NULL;
				ads = ADAPTER_DSOURCE(obj);
				if (ads == NULL ){ return FALSE;}
				ds = ads->dataSource;
				if (ds == NULL ) { return FALSE;}

				acceptable = TRUE;
			}
		}
	}

	if (acceptable) {
		g_signal_handlers_block_by_func
			(G_OBJECT(widget),
			 G_CALLBACK(addressbook_tree_selected), NULL);
		gtk_sctree_select( GTK_SCTREE(widget), node);
		g_signal_handlers_unblock_by_func
			(G_OBJECT(widget),
			 G_CALLBACK(addressbook_tree_selected), NULL);
		gdk_drag_status(context, 
					(context->actions == GDK_ACTION_COPY ?
					GDK_ACTION_COPY : GDK_ACTION_MOVE) , time);
	} else {
		gdk_drag_status(context, 0, time);
	}
	return acceptable;
}

static void addressbook_drag_leave_cb(GtkWidget      *widget,
				     GdkDragContext *context,
				     guint           time,
				     void           *data)
{
	if (addrbook.treeSelected) {
		g_signal_handlers_block_by_func
			(G_OBJECT(widget),
			 G_CALLBACK(addressbook_tree_selected), NULL);
		gtk_sctree_select( GTK_SCTREE(widget), addrbook.opened);
		g_signal_handlers_unblock_by_func
			(G_OBJECT(widget),
			 G_CALLBACK(addressbook_tree_selected), NULL);
	}
	
}

static void addressbook_drag_received_cb(GtkWidget        *widget,
					GdkDragContext   *drag_context,
					gint              x,
					gint              y,
					GtkSelectionData *data,
					guint             info,
					guint             time,
					void             *pdata)
{
	gint row, column;
	GtkCMCTreeNode *node;
	GtkCMCTreeNode *lastopened = addrbook.opened;

	if (!strncmp(data->data, "Dummy_addr", 10)) {
		if (gtk_cmclist_get_selection_info
			(GTK_CMCLIST(widget), x - 24, y - 24, &row, &column) == 0) {
			return;
		}
		
		node = gtk_cmctree_node_nth(GTK_CMCTREE(widget), row);
		if( !node || !gtk_cmctree_node_get_row_data(GTK_CMCTREE(addrbook.ctree), node)) 
			return;
		
		gtk_cmclist_freeze(GTK_CMCLIST(addrbook.clist));
		if (drag_context->action == GDK_ACTION_COPY || 
		    !strcmp(data->data, "Dummy_addr_copy"))
			addressbook_clip_copy_cb(NULL, NULL);
		else
			addressbook_clip_cut_cb(NULL, NULL);
		gtk_sctree_select( GTK_SCTREE(addrbook.ctree), node);
		addressbook_clip_paste_cb(NULL,NULL);
		gtk_sctree_select( GTK_SCTREE(addrbook.ctree), lastopened);
		gtk_cmclist_thaw(GTK_CMCLIST(addrbook.clist));
		gtk_drag_finish(drag_context, TRUE, TRUE, time);
	}
}

/*
* End of Source.
*/
