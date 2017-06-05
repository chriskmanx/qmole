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
 * Import LDIF address book data.
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

#include "addrbook.h"
#include "addressbook.h"
#include "addressitem.h"
#include "gtkutils.h"
#include "stock_pixmap.h"
#include "prefs_common.h"
#include "manage_window.h"
#include "mgutils.h"
#include "ldif.h"
#include "utils.h"
#include "filesel.h"

#define IMPORTLDIF_GUESS_NAME      "LDIF Import"

#define PAGE_FILE_INFO             0
#define PAGE_ATTRIBUTES            1
#define PAGE_FINISH                2

#define IMPORTLDIF_WIDTH           390
#define IMPORTLDIF_HEIGHT          300

#define FIELDS_N_COLS              4
#define FIELDS_COL_WIDTH_RESERVED  10
#define FIELDS_COL_WIDTH_SELECT    10
#define FIELDS_COL_WIDTH_FIELD     140
#define FIELDS_COL_WIDTH_ATTRIB    140

typedef enum {
	FIELD_COL_RESERVED = 0,
	FIELD_COL_SELECT   = 1,
	FIELD_COL_FIELD    = 2,
	FIELD_COL_ATTRIB   = 3
} ImpLdif_FieldColPos;

/**
 * LDIF dialog definition.
 */
static struct _ImpLdif_Dlg {
	GtkWidget *window;
	GtkWidget *notebook;
	GtkWidget *entryFile;
	GtkWidget *entryName;
	GtkWidget *clist_field;
	GtkWidget *entryField;
	GtkWidget *entryAttrib;
	GtkWidget *checkSelect;
	GtkWidget *btnModify;
	GtkWidget *labelBook;
	GtkWidget *labelFile;
	GtkWidget *labelRecords;
	GtkWidget *btnPrev;
	GtkWidget *btnNext;
	GtkWidget *btnProceed;
	GtkWidget *btnCancel;
	GtkWidget *statusbar;
	gint      status_cid;
	gint      rowIndSelect;
	gint      rowCount;
	gchar     *nameBook;
	gchar     *fileName;
	gboolean  cancelled;
} impldif_dlg;

static struct _AddressFileSelection _imp_ldif_file_selector_;
static AddressBookFile *_importedBook_;
static AddressIndex *_imp_addressIndex_;
static LdifFile *_ldifFile_ = NULL;

static GdkPixbuf *markxpm;

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
static ErrMsgTableEntry _lutErrorsLDIF_[] = {
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

/**
 * Lookup message for specified error code.
 * \param lut  Lookup table.
 * \param code Code to lookup.
 * \return Description associated to code.
 */
static gchar *imp_ldif_err2string( ErrMsgTableEntry lut[], gint code ) {
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

/**
 * Display message in status field.
 * \param msg Message to display.
 */
static void imp_ldif_status_show( gchar *msg ) {
	if( impldif_dlg.statusbar != NULL ) {
		gtk_statusbar_pop( GTK_STATUSBAR(impldif_dlg.statusbar),
			impldif_dlg.status_cid );
		if( msg ) {
			gtk_statusbar_push(
				GTK_STATUSBAR(impldif_dlg.statusbar),
				impldif_dlg.status_cid, msg );
		}
	}
}

/**
 * Select and display status message appropriate for the page being displayed.
 */
static void imp_ldif_message( void ) {
	gchar *sMsg = NULL;
	gint pageNum;

	pageNum = gtk_notebook_get_current_page( GTK_NOTEBOOK(impldif_dlg.notebook) );
	if( pageNum == PAGE_FILE_INFO ) {
		sMsg = _( "Please specify address book name and file to import." );
	}
	else if( pageNum == PAGE_ATTRIBUTES ) {
		sMsg = _( "Select and rename LDIF field names to import." );
	}
	else if( pageNum == PAGE_FINISH ) {
		sMsg = _( "File imported." );
	}
	imp_ldif_status_show( sMsg );
}

/**
 * Update list with data for current row.
 * \param clist List to update.
 */
static void imp_ldif_update_row( GtkCMCList *clist ) {
	Ldif_FieldRec *rec;
	gchar *text[ FIELDS_N_COLS ];
	gint row;

	if( impldif_dlg.rowIndSelect < 0 ) return;
	row = impldif_dlg.rowIndSelect;

	rec = gtk_cmclist_get_row_data( clist, row );
	if (!rec)
		return;

	text[ FIELD_COL_RESERVED ] = "";
	text[ FIELD_COL_SELECT   ] = "";
	text[ FIELD_COL_FIELD    ] = rec->tagName;
	text[ FIELD_COL_ATTRIB   ] = rec->userName;

	gtk_cmclist_freeze( clist );
	gtk_cmclist_remove( clist, row );
	if( row == impldif_dlg.rowCount - 1 ) {
		gtk_cmclist_append( clist, text );
	}
	else {
		gtk_cmclist_insert( clist, row, text );
	}
	if( rec->selected ) {
		gtk_cmclist_set_pixbuf(
			clist, row, FIELD_COL_SELECT, markxpm );
	}
	if( rec->reserved ) {
		gtk_cmclist_set_pixbuf(
			clist, row, FIELD_COL_RESERVED, markxpm );
	}

	gtk_cmclist_set_row_data( clist, row, rec );
	gtk_cmclist_thaw( clist );
}

/**
 * Load list with LDIF fields read from file.
 * \param ldf LDIF control data.
 */
static void imp_ldif_load_fields( LdifFile *ldf ) {
	GtkCMCList *clist = GTK_CMCLIST(impldif_dlg.clist_field);
	GList *node, *list;
	gchar *text[ FIELDS_N_COLS ];

	impldif_dlg.rowIndSelect = -1;
	impldif_dlg.rowCount = 0;
	if( ! ldf->accessFlag ) return;
	gtk_cmclist_clear( clist );
	list = ldif_get_fieldlist( ldf );
	node = list;
	while( node ) {
		Ldif_FieldRec *rec = node->data;
		gint row;

		text[ FIELD_COL_RESERVED ] = "";
		text[ FIELD_COL_SELECT   ] = "";
		text[ FIELD_COL_FIELD    ] = rec->tagName;
		text[ FIELD_COL_ATTRIB   ] = rec->userName;
		row = gtk_cmclist_append( clist, text );
		gtk_cmclist_set_row_data( clist, row, rec );
		if( rec->selected ) {
			gtk_cmclist_set_pixbuf( clist, row,
				FIELD_COL_SELECT, markxpm );
		}
		if( rec->reserved ) {
			gtk_cmclist_set_pixbuf( clist, row,
				FIELD_COL_RESERVED, markxpm );
		}
		impldif_dlg.rowCount++;
		node = g_list_next( node );
	}
	g_list_free( list );
	list = NULL;
	ldif_set_accessed( ldf, FALSE );
}

/**
 * Callback function when list item is selected.
 * \param clist List widget.
 * \param row   Row.
 * \param col   Column.
 * \param event Event object.
 * \param data  User data.
 */
static void imp_ldif_field_list_selected(
		GtkCMCList *clist, gint row, gint column, GdkEvent *event,
		gpointer data )
{
	Ldif_FieldRec *rec = gtk_cmclist_get_row_data( clist, row );

	impldif_dlg.rowIndSelect = row;
	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryAttrib), "" );
	if( rec ) {
		/* Update widget contents */
		gtk_label_set_text(
			GTK_LABEL(impldif_dlg.entryField), rec->tagName );
		if( rec->userName )
			gtk_entry_set_text(
				GTK_ENTRY(impldif_dlg.entryAttrib), rec->userName );
		gtk_toggle_button_set_active(
			GTK_TOGGLE_BUTTON( impldif_dlg.checkSelect),
			rec->selected );

		/* Disable widgets for reserved fields */
		gtk_widget_set_sensitive(
			impldif_dlg.entryAttrib, ! rec->reserved );
		gtk_widget_set_sensitive(
			impldif_dlg.checkSelect, ! rec->reserved );
		gtk_widget_set_sensitive(
			impldif_dlg.btnModify,   ! rec->reserved );
	}
	gtk_widget_grab_focus(impldif_dlg.entryAttrib);
}

/**
 * Callback function to toggle selected LDIF field.
 * \param clist List to update.
 * \param event Event object.
 * \param data  Data.
 */
static gboolean imp_ldif_field_list_toggle(
		GtkCMCList *clist, GdkEventButton *event, gpointer data )
{
	Ldif_FieldRec *rec;
	gboolean toggle = FALSE;

	if( ! event ) return FALSE;
	if( impldif_dlg.rowIndSelect < 0 ) return FALSE;
	if( event->button == 1 ) {
		/* If single click in select column */
		if( event->type == GDK_BUTTON_PRESS ) {
			gint x = event->x;
			gint y = event->y;
			gint row, col;

			gtk_cmclist_get_selection_info( clist, x, y, &row, &col );
			if( col != FIELD_COL_SELECT ) return FALSE;
			if( row > impldif_dlg.rowCount ) return FALSE;

			/* Set row */
			impldif_dlg.rowIndSelect = row;
			toggle = TRUE;
		}

		/* If double click anywhere in row */
		else if( event->type == GDK_2BUTTON_PRESS ) {
			toggle = TRUE;
		}
	}
	/* Toggle field selection */
	if( toggle ) {
		rec = gtk_cmclist_get_row_data(
			clist, impldif_dlg.rowIndSelect );
		if( rec ) {
			ldif_field_toggle( rec );
			imp_ldif_update_row( clist );
		}
	}
	return FALSE;
}

/**
 * Callback function to update LDIF field data from input fields.
 * \param widget Widget (button).
 * \param data   User data.
 */
static void imp_ldif_modify_pressed( GtkWidget *widget, gpointer data ) {
	GtkCMCList *clist = GTK_CMCLIST(impldif_dlg.clist_field);
	Ldif_FieldRec *rec;
	gint row;

	if( impldif_dlg.rowIndSelect < 0 ) return;
	row = impldif_dlg.rowIndSelect;
	rec = gtk_cmclist_get_row_data( clist, impldif_dlg.rowIndSelect );

	ldif_field_set_name( rec, gtk_editable_get_chars(
		GTK_EDITABLE(impldif_dlg.entryAttrib), 0, -1 ) );
	ldif_field_set_selected( rec, gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON( impldif_dlg.checkSelect) ) );
	imp_ldif_update_row( clist );
	gtk_cmclist_select_row( clist, row, 0 );
	gtk_label_set_text( GTK_LABEL(impldif_dlg.entryField), "" );
	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryAttrib), "" );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( impldif_dlg.checkSelect), FALSE );
}

/**
 * Test whether we can move off fields page.
 * \return <i>TRUE</i> if OK to move off page.
 */
static gboolean imp_ldif_field_move() {
	gboolean retVal = FALSE;
	gchar *newFile;
	AddressBookFile *abf = NULL;

	if( _importedBook_ ) {
		addrbook_free_book( _importedBook_ );
	}

	abf = addrbook_create_book();
	addrbook_set_path( abf, _imp_addressIndex_->filePath );
	addrbook_set_name( abf, impldif_dlg.nameBook );
	newFile = addrbook_guess_next_file( abf );
	addrbook_set_file( abf, newFile );
	g_free( newFile );

	/* Import data into file */
	if( ldif_import_data( _ldifFile_, abf->addressCache ) == MGU_SUCCESS ) {
		addrbook_save_data( abf );
		_importedBook_ = abf;
		retVal = TRUE;
	}
	else {
		addrbook_free_book( abf );
	}

	return retVal;
}

/**
 * Test whether we can move off file page.
 * \return <i>TRUE</i> if OK to move off page.
 */
static gboolean imp_ldif_file_move() {
	gboolean retVal = FALSE;
	gchar *sName;
	gchar *sFile;
	gchar *sMsg = NULL;
	gboolean errFlag = FALSE;

	sFile = gtk_editable_get_chars( GTK_EDITABLE(impldif_dlg.entryFile), 0, -1 );
	g_strchug( sFile ); g_strchomp( sFile );

	sName = gtk_editable_get_chars( GTK_EDITABLE(impldif_dlg.entryName), 0, -1 );
	g_strchug( sName ); g_strchomp( sName );

	g_free( impldif_dlg.nameBook );
	g_free( impldif_dlg.fileName );
	impldif_dlg.nameBook = sName;
	impldif_dlg.fileName = sFile;

	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryFile), sFile );
	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryName), sName );

	if( *sFile == '\0' ) {
		sMsg = _( "Please select a file." );
		gtk_widget_grab_focus(impldif_dlg.entryFile);
		errFlag = TRUE;
	}

	if( ! errFlag && *sName == '\0' ) {
		sMsg = _( "Address book name must be supplied." );
		gtk_widget_grab_focus(impldif_dlg.entryName);
		errFlag = TRUE;
	}

	if( ! errFlag ) {
		/* Read attribute list */
		ldif_set_file( _ldifFile_, sFile );
		if( ldif_read_tags( _ldifFile_ ) == MGU_SUCCESS ) {
			/* Load fields */
			/* ldif_print_file( _ldifFile_, stdout ); */
			imp_ldif_load_fields( _ldifFile_ );
			retVal = TRUE;
		}
		else {
			sMsg = imp_ldif_err2string( _lutErrorsLDIF_, _ldifFile_->retVal );
		}
	}
	imp_ldif_status_show( sMsg );

	return retVal;
}

/**
 * Display finish page.
 */
static void imp_ldif_finish_show() {
	gchar *sMsg;
	gchar *name;

	name = gtk_editable_get_chars( GTK_EDITABLE(impldif_dlg.entryName), 0, -1 );
	gtk_label_set_text( GTK_LABEL(impldif_dlg.labelBook), name );
	g_free( name );
	gtk_label_set_text( GTK_LABEL(impldif_dlg.labelFile), _ldifFile_->path );
	gtk_label_set_text( GTK_LABEL(impldif_dlg.labelRecords), itos( _ldifFile_->importCount ) );
	gtk_widget_set_sensitive( impldif_dlg.btnPrev, FALSE );
	gtk_widget_hide( impldif_dlg.btnNext );
	gtk_widget_show( impldif_dlg.btnProceed );
	gtk_widget_set_sensitive( impldif_dlg.btnProceed, FALSE );
	if( _ldifFile_->retVal == MGU_SUCCESS ) {
		sMsg = _( "LDIF file imported successfully." );
	}
	else {
		sMsg = imp_ldif_err2string( _lutErrorsLDIF_, _ldifFile_->retVal );
	}
	imp_ldif_status_show( sMsg );
	gtk_widget_grab_focus(impldif_dlg.btnCancel);
}

/**
 * Callback function to select previous page.
 * \param widget Widget (button).
 */
static void imp_ldif_prev( GtkWidget *widget ) {
	gint pageNum;

	pageNum = gtk_notebook_get_current_page( GTK_NOTEBOOK(impldif_dlg.notebook) );
	if( pageNum == PAGE_ATTRIBUTES ) {
		/* Goto file page stuff */
		gtk_notebook_set_current_page(
			GTK_NOTEBOOK(impldif_dlg.notebook), PAGE_FILE_INFO );
		gtk_widget_set_sensitive( impldif_dlg.btnPrev, FALSE );
		gtk_widget_hide( impldif_dlg.btnProceed );
		gtk_widget_show( impldif_dlg.btnNext );
	}
	imp_ldif_message();
}

/**
 * Callback function to select next page.
 * \param widget Widget (button).
 */
static void imp_ldif_next( GtkWidget *widget ) {
	gint pageNum;

	pageNum = gtk_notebook_get_current_page( GTK_NOTEBOOK(impldif_dlg.notebook) );
	if( pageNum == PAGE_FILE_INFO ) {
		/* Goto attributes stuff */
		if( imp_ldif_file_move() ) {
			gtk_notebook_set_current_page(
				GTK_NOTEBOOK(impldif_dlg.notebook), PAGE_ATTRIBUTES );
			imp_ldif_message();
			gtk_widget_set_sensitive( impldif_dlg.btnPrev, TRUE );
			gtk_widget_hide( impldif_dlg.btnNext );
			gtk_widget_show( impldif_dlg.btnProceed );
			gtk_widget_set_sensitive( impldif_dlg.btnProceed, TRUE );
		}
		else {
			gtk_widget_set_sensitive( impldif_dlg.btnPrev, FALSE );
			_ldifFile_->dirtyFlag = TRUE;
		}
	}
	else if( pageNum == PAGE_ATTRIBUTES ) {
		/* Goto finish stuff */
		if( imp_ldif_field_move() ) {
			gtk_notebook_set_current_page(
				GTK_NOTEBOOK(impldif_dlg.notebook), PAGE_FINISH );
			gtk_button_set_label(GTK_BUTTON(impldif_dlg.btnCancel),
					     GTK_STOCK_CLOSE);
			imp_ldif_finish_show();
		}
	}
}

/**
 * Callback function to cancel and close dialog.
 * \param widget Widget (button).
 * \param data   User data.
 */
static void imp_ldif_cancel( GtkWidget *widget, gpointer data ) {
	gint pageNum;

	pageNum = gtk_notebook_get_current_page( GTK_NOTEBOOK(impldif_dlg.notebook) );
	if( pageNum != PAGE_FINISH ) {
		impldif_dlg.cancelled = TRUE;
	}
	gtk_main_quit();
}


/**
 * Create LDIF file selection dialog.
 * \param afs Address file selection data.
 */
static void imp_ldif_file_select_create( AddressFileSelection *afs ) {
	gchar *file = filesel_select_file_open(_("Select LDIF File"), NULL);

	if (file == NULL)
		afs->cancelled = TRUE;
	else {
		afs->cancelled = FALSE;
		gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryFile), file );
		g_free(file);
	}
}

/**
 * Callback function to display LDIF file selection dialog.
 */
static void imp_ldif_file_select( void ) {
	imp_ldif_file_select_create( & _imp_ldif_file_selector_ );
}

/**
 * Callback function to handle dialog close event.
 * \param widget Widget (dialog).
 * \param event  Event object.
 * \param data   User data.
 */
static gint imp_ldif_delete_event( GtkWidget *widget, GdkEventAny *event, gpointer data ) {
	imp_ldif_cancel( widget, data );
	return TRUE;
}

/**
 * Callback function to respond to dialog key press events.
 * \param widget Widget.
 * \param event  Event object.
 * \param data   User data.
 */
static gboolean imp_ldif_key_pressed( GtkWidget *widget, GdkEventKey *event, gpointer data ) {
	if (event && event->keyval == GDK_KEY_Escape) {
		imp_ldif_cancel( widget, data );
	}
	return FALSE;
}

/**
 * Format notebook "file" page.
 * \param pageNum Page (tab) number.
 * \param pageLbl Page (tab) label.
 */
static void imp_ldif_page_file( gint pageNum, gchar *pageLbl ) {
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *entryFile;
	GtkWidget *entryName;
	GtkWidget *btnFile;
	gint top;

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add( GTK_CONTAINER( impldif_dlg.notebook ), vbox );
	gtk_container_set_border_width( GTK_CONTAINER (vbox), BORDER_WIDTH );

	label = gtk_label_new( pageLbl );
	gtk_widget_show( label );
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK( impldif_dlg.notebook ),
		gtk_notebook_get_nth_page(
			GTK_NOTEBOOK( impldif_dlg.notebook ), pageNum ),
		label );

	table = gtk_table_new(2, 3, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 8 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8 );

	/* First row */
	top = 0;
	label = gtk_label_new(_("Address Book"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1),
		GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entryName = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entryName, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entryName, _( 
		"Specify the name for the address book that will " \
		"be created from the LDIF file data." ));

	/* Second row */
	top = 1;
	label = gtk_label_new(_("File Name"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1),
		GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entryFile = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entryFile, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entryFile,
		_( "The full file specification of the LDIF file to import." ));

	btnFile = gtkut_get_browse_file_btn(_("B_rowse"));
	gtk_table_attach(GTK_TABLE(table), btnFile, 2, 3, top, (top + 1),
		GTK_FILL, 0, 3, 0);

	CLAWS_SET_TIP(btnFile,
		_( "Select the LDIF file to import." ));

	gtk_widget_show_all(vbox);

	/* Button handler */
	g_signal_connect(G_OBJECT(btnFile), "clicked",
			 G_CALLBACK(imp_ldif_file_select), NULL);

	impldif_dlg.entryFile = entryFile;
	impldif_dlg.entryName = entryName;

}

/**
 * Format notebook fields page.
 * \param pageNum Page (tab) number.
 * \param pageLbl Page (tab) label.
 */
static void imp_ldif_page_fields( gint pageNum, gchar *pageLbl ) {
	GtkWidget *vbox;
	GtkWidget *vboxt;
	GtkWidget *vboxb;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *clist_swin;
	GtkWidget *clist_field;
	GtkWidget *entryField;
	GtkWidget *entryAttrib;
	GtkWidget *checkSelect;
	GtkWidget *btnModify;
	GtkWidget *eventBox;
	gint top;

	gchar *titles[ FIELDS_N_COLS ];
	gint i;

	titles[ FIELD_COL_RESERVED ] = _("R");
	titles[ FIELD_COL_SELECT   ] = _("S");
	titles[ FIELD_COL_FIELD    ] = _("LDIF Field Name");
	titles[ FIELD_COL_ATTRIB   ] = _("Attribute Name");

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add( GTK_CONTAINER( impldif_dlg.notebook ), vbox );
	gtk_container_set_border_width( GTK_CONTAINER (vbox), 4 );

	label = gtk_label_new( pageLbl );
	gtk_widget_show( label );
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK( impldif_dlg.notebook ),
		gtk_notebook_get_nth_page(GTK_NOTEBOOK( impldif_dlg.notebook ), pageNum ),
		label );

	/* Upper area - Field list */
	vboxt = gtk_vbox_new( FALSE, 4 );
	gtk_container_add( GTK_CONTAINER( vbox ), vboxt );

	clist_swin = gtk_scrolled_window_new( NULL, NULL );
	gtk_container_add( GTK_CONTAINER(vboxt), clist_swin );
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(clist_swin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);

	clist_field = gtk_cmclist_new_with_titles( FIELDS_N_COLS, titles );
	gtk_container_add( GTK_CONTAINER(clist_swin), clist_field );
	gtk_cmclist_set_selection_mode(
		GTK_CMCLIST(clist_field), GTK_SELECTION_BROWSE );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_field),
		FIELD_COL_RESERVED, FIELDS_COL_WIDTH_RESERVED );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_field),
		FIELD_COL_SELECT, FIELDS_COL_WIDTH_SELECT );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_field),
		FIELD_COL_FIELD, FIELDS_COL_WIDTH_FIELD );
	gtk_cmclist_set_column_width( GTK_CMCLIST(clist_field),
		FIELD_COL_ATTRIB, FIELDS_COL_WIDTH_ATTRIB );

	/* Remove focus capability for column headers */
	for( i = 0; i < FIELDS_N_COLS; i++ ) {
		gtkut_widget_set_can_focus(
			GTK_CMCLIST(clist_field)->column[i].button,
			FALSE);
	}

	/* Lower area - Edit area */
	vboxb = gtk_vbox_new( FALSE, 4 );
	gtk_box_pack_end(GTK_BOX(vbox), vboxb, FALSE, FALSE, 2);

	/* Data entry area */
	table = gtk_table_new( 3, 3, FALSE);
	gtk_box_pack_start(GTK_BOX(vboxb), table, FALSE, FALSE, 0);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 4);

	/* First row */
	top = 0;
	label = gtk_label_new(_("LDIF Field"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1),
		GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entryField = gtk_label_new( "" );
	gtk_misc_set_alignment(GTK_MISC(entryField), 0.01, 0.5);
	gtk_table_attach(GTK_TABLE(table), entryField, 1, 3, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	/* Second row */
	++top;
	label = gtk_label_new(_("Attribute"));
	/*
	 * Use an event box to attach some help in the form of a tooltip.
	 * Tried this for the clist but it looked bad.
	 */
	eventBox = gtk_event_box_new();
	gtk_container_add( GTK_CONTAINER(eventBox), label );
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach(GTK_TABLE(table), eventBox, 0, 1, top, (top + 1),
		GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(eventBox, _(
		"Choose the LDIF field that will be renamed or selected " \
		"for import in the list above. Reserved fields (marked " \
		"with a tick in the \"R\" column), are automatically " \
		"imported and cannot be renamed. A single click in the " \
		"Select (\"S\") column will select the field for import " \
		"with a tick. A single click anywhere in the row will " \
		"select that field for rename in the input area below " \
		"the list. A double click anywhere in the row will also " \
		"select the field for import."));

	entryAttrib = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entryAttrib, 1, 3, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entryAttrib,
		_( "The LDIF field can be renamed to the User Attribute name." ));

	/* Next row */
	++top;

	checkSelect = gtk_check_button_new_with_label( _( "Select for Import" ) );
	gtk_table_attach(GTK_TABLE(table), checkSelect, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(checkSelect,
		_( "Select the LDIF field for import into the address book." ));

	btnModify = gtk_button_new_with_label( _(" Modify "));
	gtk_table_attach(GTK_TABLE(table), btnModify, 2, 3, top, (top + 1),
		GTK_FILL, 0, 3, 0);

	CLAWS_SET_TIP(btnModify,
		_( "This button will update the list above with the data supplied." ));

	gtk_widget_show_all(vbox);

	/* Event handlers */
	g_signal_connect( G_OBJECT(clist_field), "select_row",
			  G_CALLBACK(imp_ldif_field_list_selected), NULL );
	g_signal_connect( G_OBJECT(clist_field), "button_press_event",
			  G_CALLBACK(imp_ldif_field_list_toggle), NULL );
	g_signal_connect( G_OBJECT(btnModify), "clicked",
			  G_CALLBACK(imp_ldif_modify_pressed), NULL );

	impldif_dlg.clist_field = clist_field;
	impldif_dlg.entryField  = entryField;
	impldif_dlg.entryAttrib = entryAttrib;
	impldif_dlg.checkSelect = checkSelect;
	impldif_dlg.btnModify   = btnModify;
}

/**
 * Format notebook finish page.
 * \param pageNum Page (tab) number.
 * \param pageLbl Page (tab) label.
 */
static void imp_ldif_page_finish( gint pageNum, gchar *pageLbl ) {
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *labelBook;
	GtkWidget *labelFile;
	GtkWidget *labelRecs;
	gint top;

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add( GTK_CONTAINER( impldif_dlg.notebook ), vbox );
	gtk_container_set_border_width( GTK_CONTAINER (vbox), BORDER_WIDTH );

	label = gtk_label_new( pageLbl );
	gtk_widget_show( label );
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK( impldif_dlg.notebook ),
		gtk_notebook_get_nth_page( GTK_NOTEBOOK( impldif_dlg.notebook ), pageNum ),
		label );

	table = gtk_table_new(3, 2, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 8 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	/* First row */
	top = 0;
	label = gtk_label_new( _( "Address Book :" ) );
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	labelBook = gtk_label_new("");
	gtk_table_attach(GTK_TABLE(table), labelBook, 1, 2, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(labelBook), 0, 0.5);

	/* Second row */
	top++;
	label = gtk_label_new( _( "File Name :" ) );
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	labelFile = gtk_label_new("");
	gtk_table_attach(GTK_TABLE(table), labelFile, 1, 2, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(labelFile), 0, 0.5);

	/* Third row */
	top++;
	label = gtk_label_new( _("Records Imported :") );
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	labelRecs = gtk_label_new("");
	gtk_table_attach(GTK_TABLE(table), labelRecs, 1, 2, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(labelRecs), 0, 0.5);

	impldif_dlg.labelBook    = labelBook;
	impldif_dlg.labelFile    = labelFile;
	impldif_dlg.labelRecords = labelRecs;
}

/**
 * Create main dialog decorations (excluding notebook pages).
 */
static void imp_ldif_dialog_create() {
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *vnbox;
	GtkWidget *notebook;
	GtkWidget *hbbox;
	GtkWidget *btnPrev;
	GtkWidget *btnNext;
	GtkWidget *btnProceed;
	GtkWidget *btnCancel;
	GtkWidget *hsbox;
	GtkWidget *statusbar;

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "importldif");
	gtk_widget_set_size_request(window, IMPORTLDIF_WIDTH, IMPORTLDIF_HEIGHT );
	gtk_container_set_border_width( GTK_CONTAINER(window), 0 );
	gtk_window_set_title( GTK_WINDOW(window), _("Import LDIF file into Address Book") );
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(imp_ldif_delete_event),
			 NULL );
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(imp_ldif_key_pressed),
			 NULL );

	vbox = gtk_vbox_new(FALSE, 4);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	vnbox = gtk_vbox_new(FALSE, 4);
	gtk_container_set_border_width(GTK_CONTAINER(vnbox), 4);
	gtk_widget_show(vnbox);
	gtk_box_pack_start(GTK_BOX(vbox), vnbox, TRUE, TRUE, 0);

	/* Notebook */
	notebook = gtk_notebook_new();
	gtk_notebook_set_show_tabs( GTK_NOTEBOOK(notebook), FALSE );
	gtk_widget_show(notebook);
	gtk_box_pack_start(GTK_BOX(vnbox), notebook, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(notebook), 6);

	/* Status line */
	hsbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), hsbox, FALSE, FALSE, BORDER_WIDTH);
	statusbar = gtk_statusbar_new();
	gtk_box_pack_start(GTK_BOX(hsbox), statusbar, TRUE, TRUE, BORDER_WIDTH);

	/* Button panel */
	gtkut_stock_button_set_create(&hbbox,
				      &btnCancel, GTK_STOCK_CANCEL, 
				      &btnPrev, GTK_STOCK_GO_BACK,
				      &btnNext, GTK_STOCK_GO_FORWARD);

	btnProceed = gtk_button_new_with_mnemonic(_("Proceed"));
	gtk_button_set_image(GTK_BUTTON(btnProceed),
			gtk_image_new_from_stock(GTK_STOCK_OK, GTK_ICON_SIZE_BUTTON));
	gtkut_widget_set_can_default(btnProceed, TRUE);
	gtk_box_pack_start(GTK_BOX(hbbox), btnProceed, TRUE, TRUE, 0);
	gtk_widget_hide(btnProceed);

	gtk_box_pack_end(GTK_BOX(vbox), hbbox, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(hbbox), 2);
	gtk_widget_grab_default(btnNext);

	/* Button handlers */
	g_signal_connect(G_OBJECT(btnPrev), "clicked",
			 G_CALLBACK(imp_ldif_prev), NULL);
	g_signal_connect(G_OBJECT(btnNext), "clicked",
			 G_CALLBACK(imp_ldif_next), NULL);
	g_signal_connect(G_OBJECT(btnProceed), "clicked",
			 G_CALLBACK(imp_ldif_next), NULL);
	g_signal_connect(G_OBJECT(btnCancel), "clicked",
			 G_CALLBACK(imp_ldif_cancel), NULL);

	gtk_widget_show_all(vbox);

	impldif_dlg.window     = window;
	impldif_dlg.notebook   = notebook;
	impldif_dlg.btnPrev    = btnPrev;
	impldif_dlg.btnNext    = btnNext;
	impldif_dlg.btnProceed = btnProceed;
	impldif_dlg.btnCancel  = btnCancel;
	impldif_dlg.statusbar  = statusbar;
	impldif_dlg.status_cid = gtk_statusbar_get_context_id(
			GTK_STATUSBAR(statusbar), "Import LDIF Dialog" );

}

/**
 * Create import LDIF dialog.
 */
static void imp_ldif_create() {
	imp_ldif_dialog_create();
	imp_ldif_page_file( PAGE_FILE_INFO, _( "File Info" ) );
	imp_ldif_page_fields( PAGE_ATTRIBUTES, _( "Attributes" ) );
	imp_ldif_page_finish( PAGE_FINISH, _( "Finish" ) );
	gtk_widget_show_all( impldif_dlg.window );
}

/**
 * Import LDIF file.
 * \param  addrIndex Address index.
 * \return Address book file of imported data, or <i>NULL</i> if import
 *         was cancelled.
 */
AddressBookFile *addressbook_imp_ldif( AddressIndex *addrIndex ) {
	_importedBook_ = NULL;
	_imp_addressIndex_ = addrIndex;

	if( ! impldif_dlg.window )
		imp_ldif_create();

	gtk_button_set_label(GTK_BUTTON(impldif_dlg.btnCancel),
			     GTK_STOCK_CANCEL);
	gtk_widget_hide(impldif_dlg.btnProceed);
	gtk_widget_show(impldif_dlg.btnNext);

	impldif_dlg.cancelled = FALSE;
	gtk_widget_show(impldif_dlg.window);
	manage_window_set_transient(GTK_WINDOW(impldif_dlg.window));
	gtk_widget_grab_default(impldif_dlg.btnNext);
	gtk_window_set_modal(GTK_WINDOW(impldif_dlg.window), TRUE);

	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryName), IMPORTLDIF_GUESS_NAME );
	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryFile), "" );
	gtk_label_set_text( GTK_LABEL(impldif_dlg.entryField), "" );
	gtk_entry_set_text( GTK_ENTRY(impldif_dlg.entryAttrib), "" );
	gtk_cmclist_clear( GTK_CMCLIST(impldif_dlg.clist_field) );
	gtk_notebook_set_current_page( GTK_NOTEBOOK(impldif_dlg.notebook), PAGE_FILE_INFO );
	gtk_widget_set_sensitive( impldif_dlg.btnPrev, FALSE );
	gtk_widget_set_sensitive( impldif_dlg.btnNext, TRUE );
	stock_pixbuf_gdk( impldif_dlg.window, STOCK_PIXMAP_MARK,
			  &markxpm );
	imp_ldif_message();
	gtk_widget_grab_focus(impldif_dlg.entryFile);

	impldif_dlg.rowIndSelect = -1;
	impldif_dlg.rowCount = 0;
	g_free( impldif_dlg.nameBook );
	g_free( impldif_dlg.fileName );
	impldif_dlg.nameBook = NULL;
	impldif_dlg.fileName = NULL;

	_ldifFile_ = ldif_create();
	gtk_main();
	gtk_widget_hide(impldif_dlg.window);
	gtk_window_set_modal(GTK_WINDOW(impldif_dlg.window), FALSE);
	ldif_free( _ldifFile_ );
	_ldifFile_ = NULL;
	_imp_addressIndex_ = NULL;

	g_free( impldif_dlg.nameBook );
	g_free( impldif_dlg.fileName );
	impldif_dlg.nameBook = NULL;
	impldif_dlg.fileName = NULL;

	if( impldif_dlg.cancelled == TRUE ) return NULL;
	return _importedBook_;
}

/*
 * ============================================================================
 * End of Source.
 * ============================================================================
 */

