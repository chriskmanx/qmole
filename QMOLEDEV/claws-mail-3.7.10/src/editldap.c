/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2001-2011 Match Grun and the Claws Mail team
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
 * Edit LDAP address book data.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef USE_LDAP

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "addressbook.h"
#include "prefs_common.h"
#include "addressitem.h"
#include "mgutils.h"
#include "ldapserver.h"
#include "ldapctrl.h"
#include "ldaputil.h"
#include "editldap_basedn.h"
#include "manage_window.h"
#include "gtkutils.h"
#include "prefs_gtk.h"

#define PAGE_BASIC      0
#define PAGE_SEARCH     1
#define PAGE_EXTENDED   2

#define ADDRESSBOOK_GUESS_LDAP_NAME	"MyServer"
#define ADDRESSBOOK_GUESS_LDAP_SERVER	"localhost"

#define LDAPEDIT_TABLE_ROWS	6
#define LDAPEDIT_TABLE_COLS	3

static struct _LDAPEdit {
	GtkWidget *window;
	GtkWidget *notebook;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *statusbar;
	gint status_cid;
	GtkWidget *entry_name;
	GtkWidget *entry_server;
	GtkWidget *spinbtn_port;
	GtkWidget *entry_baseDN;
	GtkWidget *spinbtn_timeout;
	GtkWidget *entry_bindDN;
	GtkWidget *entry_bindPW;
	GtkWidget *spinbtn_maxentry;
	GtkWidget *entry_criteria;
	GtkWidget *spinbtn_queryage;
	GtkWidget *check_dynsearch;
	GtkWidget *check_matchoption;
#ifdef USE_LDAP_TLS
	GtkWidget *enable_ssl;
	GtkWidget *enable_tls;
#endif
} ldapedit;

/**
 * Parse out individual attribute names from criteria string.
 * \param criteria Criteria string.
 * \ctl   Control object.
 */
static gboolean editldap_validate_criteria( gchar *criteria ) {
	gchar *ptr;
	gchar **splitStr;
	gint i;
	gboolean errorFlag;

	cm_return_val_if_fail(criteria != NULL, TRUE);

	errorFlag = TRUE;

	/* Replace delimiters with spaces */
	ptr = criteria;
	while( *ptr ) {
		if( *ptr == ',' || *ptr == ';' || *ptr == '|' )
			*ptr = ' ';
		ptr++;
	}
	debug_print("cleaned criteria list: %s\n", criteria);

	/* Parse string */
	splitStr = g_strsplit( criteria, " ", 0 );
	i = 0;
	while( TRUE ) {
		if( splitStr[i] ) {
			if( *splitStr[i] ) {
				errorFlag = FALSE;
				break;
			}
		}
		else {
			break;
		}
		i++;
	}
	g_strfreev( splitStr );
	return errorFlag;
}

/*
* Edit functions.
*/
static void edit_ldap_status_show( gchar *msg ) {
	if( ldapedit.statusbar != NULL ) {
		gtk_statusbar_pop( GTK_STATUSBAR(ldapedit.statusbar), ldapedit.status_cid );
		if( msg ) {
			gtk_statusbar_push( GTK_STATUSBAR(ldapedit.statusbar),
				ldapedit.status_cid, msg );
		}
	}
}

static gboolean edit_ldap_validate( void ) {
	gchar *str;
	gboolean errorFlag;
	gint page = 0;

	errorFlag = FALSE;
	str = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_name), 0, -1 );
	if( *str == '\0' ) {
		page = PAGE_BASIC;
		gtk_widget_grab_focus( ldapedit.entry_name );
		edit_ldap_status_show( _( "A Name must be supplied." ) );
		errorFlag = TRUE;
	}
	g_free( str );

	if( ! errorFlag ) {
		str = gtk_editable_get_chars(
				GTK_EDITABLE(ldapedit.entry_server), 0, -1 );
		if( *str == '\0' ) {
			page = PAGE_BASIC;
			gtk_widget_grab_focus( ldapedit.entry_server );
			edit_ldap_status_show(
				_( "A Hostname must be supplied for the server." ) );
			errorFlag = TRUE;
		}
		g_free( str );
	}

	if( ! errorFlag ) {
		str = gtk_editable_get_chars(
				GTK_EDITABLE(ldapedit.entry_criteria), 0, -1 );
		if( editldap_validate_criteria( str ) ) {
			page = PAGE_SEARCH;
			gtk_widget_grab_focus( ldapedit.entry_criteria );
			edit_ldap_status_show(
				_( "At least one LDAP search attribute should be supplied." ) );
			errorFlag = TRUE;
		}
		g_free( str );
	}

	/* Switch to page with error */
	if( errorFlag ) {
		gtk_notebook_set_current_page( GTK_NOTEBOOK(ldapedit.notebook), page );
	}

	return errorFlag;
}

static void edit_ldap_ok( GtkWidget *widget, gboolean *cancelled ) {
	if( ! edit_ldap_validate() ) {
		*cancelled = FALSE;
		gtk_main_quit();
	}
}

static void edit_ldap_cancel( GtkWidget *widget, gboolean *cancelled ) {
	*cancelled = TRUE;
	gtk_main_quit();
}

static gint edit_ldap_delete_event( GtkWidget *widget, GdkEventAny *event, gboolean *cancelled ) {
	*cancelled = TRUE;
	gtk_main_quit();
	return TRUE;
}

static gboolean edit_ldap_key_pressed( GtkWidget *widget, GdkEventKey *event, gboolean *cancelled ) {
	if (event && event->keyval == GDK_Escape) {
		*cancelled = TRUE;
		gtk_main_quit();
	}
	return FALSE;
}

static void edit_ldap_server_check( void ) {
	gchar *sHost, *sBind, *sPass;
	gint iPort, iTime;
	gchar *sMsg;
	gchar *sBaseDN = NULL;
	gint iBaseDN = 0;
	gboolean flg;
	gboolean tls = FALSE, ssl = FALSE;
	GList *baseDN = NULL;

	edit_ldap_status_show( "" );
	flg = FALSE;
	sHost = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_server), 0, -1 );
	sBind = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_bindDN), 0, -1 );
	sPass = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_bindPW), 0, -1 );
	iPort = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON( ldapedit.spinbtn_port ) );
	iTime = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON( ldapedit.spinbtn_timeout ) );
#ifdef USE_LDAP_TLS
	tls = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ldapedit.enable_tls));
	ssl = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ldapedit.enable_ssl));
#endif

	g_strchomp( sHost ); g_strchug( sHost );
	g_strchomp( sBind ); g_strchug( sBind );
	g_strchomp( sPass ); g_strchug( sPass );
	if( *sHost != '\0' ) {
		/* Test connection to server */
		debug_print("ldap server: %s\nport: %d\nssl: %d\ntls: %d\nbindDN: %s\n", sHost, iPort, ssl, tls, sBind);
		if( ldaputil_test_connect( sHost, iPort, ssl, tls, iTime ) ) {
			/* Attempt to read base DN */
			baseDN = ldaputil_read_basedn(sHost, iPort, sBind, sPass, iTime, ssl, tls);
			if( baseDN ) {
				GList *node = baseDN;
				while( node ) {
					++iBaseDN;
					if( ! sBaseDN ) {
						sBaseDN = g_strdup( node->data );
					}
					node = g_list_next( node );
				}
				mgu_free_dlist( baseDN );
				baseDN = node = NULL;
				flg = TRUE;
			} else {
				flg = FALSE;
			}
		}
	}
	g_free( sHost );
	g_free( sBind );
	g_free( sPass );

	if( sBaseDN ) {
		/* Load search DN */
		debug_print("baseDN: %s\n", sBaseDN);
		gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_baseDN), sBaseDN);
		g_free( sBaseDN );
	}

	/* Display appropriate message */
	if( flg ) {
		sMsg = _( "Connected successfully to server" );
	}
	else {
		sMsg = _( "Could not connect to server" );
	}
	edit_ldap_status_show( sMsg );
}

static void edit_ldap_basedn_select( void ) {
	gchar *sHost, *sBind, *sPass, *sBase;
	gint iPort, iTime, tls = 0, ssl = 0;
	gchar *selectDN;

	sHost = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_server), 0, -1 );
	sBase = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_baseDN), 0, -1 );
	sBind = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_bindDN), 0, -1 );
	sPass = gtk_editable_get_chars( GTK_EDITABLE(ldapedit.entry_bindPW), 0, -1 );
	iPort = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON( ldapedit.spinbtn_port ) );
	iTime = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON( ldapedit.spinbtn_timeout ) );
#ifdef USE_LDAP_TLS
	tls = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ldapedit.enable_tls));
	ssl = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ldapedit.enable_ssl));
#endif

	g_strchomp( sHost ); g_strchug( sHost );
	g_strchomp( sBind ); g_strchug( sBind );
	g_strchomp( sPass ); g_strchug( sPass );
	debug_print("ldap server: %s\nport: %d\nssl: %d\ntls: %d\nbindDN: %s\n", sHost, iPort, ssl, tls, sBind);
	selectDN = edit_ldap_basedn_selection( sHost, iPort, sBase, iTime, sBind, sPass, ssl, tls );
	if( selectDN ) {
		gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_baseDN), selectDN);
		g_free( selectDN );
		selectDN = NULL;
	}
	g_free( sHost );
	g_free( sBase );
	g_free( sBind );
	g_free( sPass );
}

static void edit_ldap_search_reset(void) {
	gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_criteria), LDAPCTL_DFL_ATTR_LIST);
}

static void addressbook_edit_ldap_dialog_create( gboolean *cancelled ) {
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *notebook;
	GtkWidget *hbbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *hsbox;
	GtkWidget *statusbar;

	debug_print("creating edit_ldap_dialog\n");
	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "editldap");
	gtk_widget_set_size_request(window, 450, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window), 0);
	gtk_window_set_title(GTK_WINDOW(window), _("Edit LDAP Server"));
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(edit_ldap_delete_event),
			 cancelled);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(edit_ldap_key_pressed),
			 cancelled);

	vbox = gtk_vbox_new( FALSE, 6 );
	gtk_widget_show( vbox );
	gtk_container_add( GTK_CONTAINER( window ), vbox );

	/* Notebook */
	notebook = gtk_notebook_new();
	gtk_widget_show( notebook );
	gtk_box_pack_start( GTK_BOX( vbox ), notebook, TRUE, TRUE, 0 );
	gtk_container_set_border_width( GTK_CONTAINER( notebook ), 6 );

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
			 G_CALLBACK(edit_ldap_ok), cancelled);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(edit_ldap_cancel), cancelled);

	gtk_widget_show_all(vbox);

	ldapedit.window     = window;
	ldapedit.notebook   = notebook;
	ldapedit.ok_btn     = ok_btn;
	ldapedit.cancel_btn = cancel_btn;
	ldapedit.statusbar  = statusbar;
	ldapedit.status_cid =
		gtk_statusbar_get_context_id(
			GTK_STATUSBAR(statusbar), "Edit LDAP Server Dialog" );
}

static void editldap_update_port (GtkToggleButton *ssl_btn, gpointer data) {
	gboolean val = gtk_toggle_button_get_active(ssl_btn);
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON( ldapedit.spinbtn_port ), 
			val ? LDAPCTL_DFL_SSL_PORT:LDAPCTL_DFL_PORT );
	debug_print("Setting port: %d\n", val ? LDAPCTL_DFL_SSL_PORT:LDAPCTL_DFL_PORT);
}

static void addressbook_edit_ldap_page_basic( gint pageNum, gchar *pageLbl ) {
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *entry_name;
	GtkWidget *entry_server;
	GtkWidget *hbox_spin;
	GtkObject *spinbtn_port_adj;
	GtkWidget *spinbtn_port;
#ifdef USE_LDAP_TLS
	GtkWidget *enable_ssl_checkbtn, *enable_tls_checkbtn;
#endif
	GtkWidget *entry_baseDN;
	GtkWidget *check_btn;
	GtkWidget *lookdn_btn;
	CLAWS_TIP_DECL();
	gint top;

	vbox = gtk_vbox_new( FALSE, 8 );
	gtk_widget_show( vbox );
	gtk_container_add( GTK_CONTAINER( ldapedit.notebook ), vbox );

	label = gtk_label_new( pageLbl );
	gtk_widget_show( label );
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK( ldapedit.notebook ),
		gtk_notebook_get_nth_page( GTK_NOTEBOOK( ldapedit.notebook ), pageNum ), label );

	table = gtk_table_new( LDAPEDIT_TABLE_ROWS, LDAPEDIT_TABLE_COLS, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 8 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	/* First row */
	top = 0;
	label = gtk_label_new(_("Name"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_name = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_name, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entry_name, _( 
		"A name that you wish to call the server." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Hostname"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_server = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_server, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entry_server, _( 
		"This is the hostname of the server. For example, " \
		"\"ldap.mydomain.com\" may be appropriate for the " \
		"\"mydomain.com\" organization. An IP address may also be " \
		"used. You may specify \"localhost\" if running an LDAP " \
		"server on the same computer as Claws Mail." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Port"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	hbox_spin = gtk_hbox_new (FALSE, 8);
	spinbtn_port_adj = gtk_adjustment_new (389, 1, 65535, 1, 1000, 0);
	spinbtn_port = gtk_spin_button_new(GTK_ADJUSTMENT (spinbtn_port_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_spin), spinbtn_port, TRUE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_port, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_port), TRUE);
	
#ifdef USE_LDAP_TLS
	enable_tls_checkbtn = gtk_check_button_new_with_label(_("TLS"));
	enable_ssl_checkbtn = gtk_check_button_new_with_label(_("SSL"));
	SET_TOGGLE_SENSITIVITY_REVERSE(enable_tls_checkbtn, enable_ssl_checkbtn);
	SET_TOGGLE_SENSITIVITY_REVERSE(enable_ssl_checkbtn, enable_tls_checkbtn);
	CLAWS_SET_TIP(enable_tls_checkbtn, _( 
		"Enable secure connection to the LDAP server via TLS."
		"If connection fails, be sure to check the correct "
		"configuration in ldap.conf (TLS_CACERTDIR and TLS_REQCERT fields)." ));
	CLAWS_SET_TIP(enable_ssl_checkbtn, _( 
		"Enable secure connection to the LDAP server via SSL."
		"If connection fails, be sure to check the correct "
		"configuration in ldap.conf (TLS_CACERTDIR and TLS_REQCERT fields)." ));

	gtk_box_pack_start (GTK_BOX (hbox_spin), enable_tls_checkbtn, TRUE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (hbox_spin), enable_ssl_checkbtn, TRUE, FALSE, 0);
#endif

	gtk_table_attach(GTK_TABLE(table), hbox_spin, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(spinbtn_port, _( 
		"The port number that the server listens on. Port 389 is " \
		"the default." ));

	check_btn = gtk_button_new_with_label( _(" Check Server "));
	gtk_table_attach(GTK_TABLE(table), check_btn, 2, 3, top, (top + 1), GTK_FILL, 0, 3, 0);

	CLAWS_SET_TIP(check_btn, _( 
		"Press this button to test the connection to the server." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Search Base"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_baseDN = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_baseDN, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entry_baseDN, _( 
		"This specifies the name of the directory to be searched " \
		"on the server. Examples include:\n" \
		"  dc=claws-mail,dc=org\n" \
		"  ou=people,dc=domainname,dc=com\n" \
		"  o=Organization Name,c=Country\n"
		));

	lookdn_btn = gtkut_get_browse_file_btn(_("_Browse"));
	gtk_table_attach(GTK_TABLE(table), lookdn_btn, 2, 3, top, (top + 1), GTK_FILL, 0, 3, 0);

	CLAWS_SET_TIP(lookdn_btn, _( 
		"Press this button to lookup the name of available " \
		"directory names on the server." ));

	/* Signal handlers */
	g_signal_connect(G_OBJECT(check_btn), "clicked",
			 G_CALLBACK(edit_ldap_server_check), NULL);
	g_signal_connect(G_OBJECT(lookdn_btn), "clicked",
			 G_CALLBACK(edit_ldap_basedn_select), NULL);

	/* Done */
	gtk_widget_show_all(vbox);

	ldapedit.entry_name   = entry_name;
	ldapedit.entry_server = entry_server;
	ldapedit.spinbtn_port = spinbtn_port;
	ldapedit.entry_baseDN = entry_baseDN;
#ifdef USE_LDAP_TLS
	ldapedit.enable_ssl = enable_ssl_checkbtn;
	ldapedit.enable_tls = enable_tls_checkbtn;

	g_signal_connect(G_OBJECT(enable_ssl_checkbtn), "toggled", \
			 G_CALLBACK(editldap_update_port), NULL); 
#endif			 
}

static void addressbook_edit_ldap_page_search( gint pageNum, gchar *pageLbl ) {
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *entry_criteria;
	GtkWidget *hbox_spin;
	GtkObject *spinbtn_queryage_adj;
	GtkWidget *spinbtn_queryage;
	GtkWidget *check_dynsearch;
	GtkWidget *check_matchoption;
	GtkWidget *reset_btn;
	CLAWS_TIP_DECL();
	gint top;

	vbox = gtk_vbox_new( FALSE, 8 );
	gtk_widget_show( vbox );
	gtk_container_add( GTK_CONTAINER( ldapedit.notebook ), vbox );

	label = gtk_label_new( pageLbl );
	gtk_widget_show( label );
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK( ldapedit.notebook ),
		gtk_notebook_get_nth_page( GTK_NOTEBOOK( ldapedit.notebook ), pageNum ), label );

	table = gtk_table_new( LDAPEDIT_TABLE_ROWS, LDAPEDIT_TABLE_COLS, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 8 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	/* First row */
	top = 0;
	label = gtk_label_new(_("Search Attributes"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_criteria = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_criteria, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entry_criteria, _( 
		"A list of LDAP attribute names that should be searched " \
		"when attempting to find a name or address." ));

	reset_btn = gtk_button_new_with_label( _(" Defaults "));
	gtk_table_attach(GTK_TABLE(table), reset_btn, 2, 3, top, (top + 1), GTK_FILL, 0, 3, 0);

	CLAWS_SET_TIP(reset_btn, _( 
		"This resets the attribute names to a default value " \
		"that should find most names and addresses during a " \
		"name or address search process." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Max Query Age (secs)"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	hbox_spin = gtk_hbox_new (FALSE, 8);
	spinbtn_queryage_adj = gtk_adjustment_new(
		LDAPCTL_DFL_QUERY_AGE, 1, LDAPCTL_MAX_QUERY_AGE, 10, 1000, 0 );
	spinbtn_queryage = gtk_spin_button_new(GTK_ADJUSTMENT (spinbtn_queryage_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_spin), spinbtn_queryage, FALSE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_queryage, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_queryage), TRUE);
	gtk_table_attach(GTK_TABLE(table), hbox_spin, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(spinbtn_queryage, _( 
		"This defines the maximum period of time (in seconds) that " \
		"an address search result is valid for address completion " \
		"purposes. Search results are stored in a cache until this " \
		"period of time has passed and then retired. This will " \
		"improve the response time when attempting to search for " \
		"the same name or address on subsequent address completion " \
		"requests. The cache will be searched in preference to " \
		"performing a new server search request. The default value " \
		"of 600 seconds (10 minutes), should be sufficient for most " \
		"servers. A larger value will reduce the search time for " \
		"subsequent searches. This is useful for servers that have " \
		"slow response times at the expense of more memory to cache " \
		"results." ));

	/* Next row */
	++top;
	check_dynsearch = gtk_check_button_new_with_label(
				_("Include server in dynamic search") );
	gtk_table_attach(GTK_TABLE(table), check_dynsearch, 1, 3, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(check_dynsearch, _( 
		"Check this option to include this server for dynamic " \
		"searches when using address completion." ));

	/* Next row */
	++top;
	check_matchoption = gtk_check_button_new_with_label(
				_("Match names 'containing' search term") );
	gtk_table_attach(GTK_TABLE(table), check_matchoption, 1, 3, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(check_matchoption, _( 
		"Searches for names and addresses can be performed either " \
		"using \"begins-with\" or \"contains\" search term. Check " \
		"this option to perform a \"contains\" search; this type of " \
		"search usually takes longer to complete. Note that for " \
		"performance reasons, address completion uses " \
		"\"begins-with\" for all searches against other address " \
		"interfaces." \
		));

	/* Signal handlers */
	g_signal_connect(G_OBJECT(reset_btn), "clicked",
			 G_CALLBACK(edit_ldap_search_reset), NULL);

	/* Done */
	gtk_widget_show_all(vbox);

	ldapedit.entry_criteria    = entry_criteria;
	ldapedit.spinbtn_queryage  = spinbtn_queryage;
	ldapedit.check_dynsearch   = check_dynsearch;
	ldapedit.check_matchoption = check_matchoption;
}

static void addressbook_edit_ldap_page_extended( gint pageNum, gchar *pageLbl ) {
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *entry_bindDN;
	GtkWidget *entry_bindPW;
	GtkWidget *hbox_spin;
	GtkObject *spinbtn_timeout_adj;
	GtkWidget *spinbtn_timeout;
	GtkObject *spinbtn_maxentry_adj;
	GtkWidget *spinbtn_maxentry;
	CLAWS_TIP_DECL();
	gint top;

	vbox = gtk_vbox_new( FALSE, 8 );
	gtk_widget_show( vbox );
	gtk_container_add( GTK_CONTAINER( ldapedit.notebook ), vbox );

	label = gtk_label_new( pageLbl );
	gtk_widget_show( label );
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK( ldapedit.notebook ),
		gtk_notebook_get_nth_page( GTK_NOTEBOOK( ldapedit.notebook ), pageNum ), label );

	table = gtk_table_new( LDAPEDIT_TABLE_ROWS, LDAPEDIT_TABLE_COLS, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_container_set_border_width( GTK_CONTAINER(table), 8 );
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	/* Next row */
	top = 0;
	label = gtk_label_new(_("Bind DN"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_bindDN = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_bindDN, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(entry_bindDN, _( 
		"The LDAP user account name to be used to connect to the server. " \
		"This is usually only used for protected servers. This name " \
		"is typically formatted as: \"cn=user,dc=claws-mail,dc=org\". " \
		"This is usually left empty when performing a search." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Bind Password"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_bindPW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_bindPW, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);
	gtk_entry_set_visibility(GTK_ENTRY(entry_bindPW), FALSE);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(entry_bindPW), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_INVISIBLE);
#endif

	CLAWS_SET_TIP(entry_bindPW, _( 
		"The password to be used when connecting as the \"Bind DN\" " \
		"user." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Timeout (secs)"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	hbox_spin = gtk_hbox_new (FALSE, 8);
	spinbtn_timeout_adj = gtk_adjustment_new (0, 0, 300, 1, 10, 0);
	spinbtn_timeout = gtk_spin_button_new(GTK_ADJUSTMENT (spinbtn_timeout_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_spin), spinbtn_timeout, FALSE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_timeout, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_timeout), TRUE);
	gtk_table_attach(GTK_TABLE(table), hbox_spin, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(spinbtn_timeout, _( 
		"The timeout period in seconds." ));

	/* Next row */
	++top;
	label = gtk_label_new(_("Maximum Entries"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	hbox_spin = gtk_hbox_new (FALSE, 8);
	spinbtn_maxentry_adj = gtk_adjustment_new (0, 0, 500, 1, 10, 0);
	spinbtn_maxentry = gtk_spin_button_new(GTK_ADJUSTMENT (spinbtn_maxentry_adj), 1, 0);
	gtk_box_pack_start (GTK_BOX (hbox_spin), spinbtn_maxentry, FALSE, FALSE, 0);
	gtk_widget_set_size_request (spinbtn_maxentry, 64, -1);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbtn_maxentry), TRUE);
	gtk_table_attach(GTK_TABLE(table), hbox_spin, 1, 2, top, (top + 1),
		GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	CLAWS_SET_TIP(spinbtn_maxentry, _( 
		"The maximum number of entries that should be returned " \
		"in the search result." ));

	/* Done */
	gtk_widget_show_all(vbox);

	ldapedit.entry_bindDN     = entry_bindDN;
	ldapedit.entry_bindPW     = entry_bindPW;
	ldapedit.spinbtn_timeout  = spinbtn_timeout;
	ldapedit.spinbtn_maxentry = spinbtn_maxentry;
}

static void addressbook_edit_ldap_create( gboolean *cancelled ) {
	gint page = 0;
	addressbook_edit_ldap_dialog_create( cancelled );
	addressbook_edit_ldap_page_basic( page++, _( "Basic" ) );
	addressbook_edit_ldap_page_search( page++, _( "Search" ) );
	addressbook_edit_ldap_page_extended( page++, _( "Extended" ) );
	gtk_widget_show_all( ldapedit.window );
}

/**
 * Format criteria list for display.
 * \param ctl Control object.
 * \return Formatted string, or <i>NULL</i> if no attributes found.
 */
static gchar *editldap_build_criteria_list( const LdapControl *ctl ) {
	gchar *str = NULL;
	gchar *tmp = NULL;
	GList *node;

	node = ldapctl_get_criteria_list( ctl );
	while( node ) {
		gchar *attr = node->data;
		if( str ) {
			tmp = g_strdup_printf( "%s, %s", str, attr );
			g_free( str );
			str = tmp;
			tmp = NULL;
		}
		else {
			str = g_strdup( attr );
		}
		node = g_list_next( node );
	}

	return str;
}

/**
 * Parse out individual attribute names from criteria string.
 * \param criteria Criteria string.
 * \ctl   Control object.
 */
static void editldap_parse_criteria( gchar *criteria, LdapControl *ctl ) {
	gchar *ptr;
	gchar **splitStr;
	gint i;

	/* Replace delimiters with spaces */
	ptr = criteria;
	while( *ptr ) {
		if( *ptr == ',' || *ptr == ';' || *ptr == '|' )
			*ptr = ' ';
		ptr++;
	}

	/* Parse string */
	ldapctl_criteria_list_clear( ctl );
	splitStr = g_strsplit( criteria, " ", 0 );
	i = 0;
	while( TRUE ) {
		if( splitStr[i] ) {
			if( *splitStr[i] ) {
				ldapctl_criteria_list_add( ctl, splitStr[i] );
			}
		}
		else {
			break;
		}
		i++;
	}
	g_strfreev( splitStr );
}

/**
 * Clear entry fields to reasonable defaults (for a new server entry).
 */
static void edit_ldap_clear_fields(void) {
	gtk_entry_set_text(
		GTK_ENTRY(ldapedit.entry_name), ADDRESSBOOK_GUESS_LDAP_NAME );
	gtk_entry_set_text(
		GTK_ENTRY(ldapedit.entry_server), ADDRESSBOOK_GUESS_LDAP_SERVER );
	gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_baseDN), "");
	gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_bindDN), "");
	gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_bindPW), "");
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON( ldapedit.spinbtn_port ), LDAPCTL_DFL_PORT );
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON( ldapedit.spinbtn_timeout ), LDAPCTL_DFL_TIMEOUT );
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON( ldapedit.spinbtn_maxentry ), LDAPCTL_DFL_TIMEOUT );
	gtk_entry_set_text(
		GTK_ENTRY(ldapedit.entry_criteria), LDAPCTL_DFL_ATTR_LIST);
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON(ldapedit.spinbtn_queryage), LDAPCTL_DFL_QUERY_AGE );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( ldapedit.check_dynsearch), TRUE );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( ldapedit.check_matchoption), FALSE );
#ifdef USE_LDAP_TLS
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( ldapedit.enable_ssl), FALSE );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( ldapedit.enable_tls), FALSE );
#endif
}

/**
 * Load entry fields from server control data.
 * \param server Server object.
 */
static void edit_ldap_set_fields( LdapServer *server ) {
	LdapControl *ctl;
	gchar *crit;
	gchar *pwd;

	if( ldapsvr_get_name( server ) )
		gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_name),
		ldapsvr_get_name( server ) );

	ctl = server->control;
	if( ctl->hostName )
		gtk_entry_set_text(
			GTK_ENTRY(ldapedit.entry_server), ctl->hostName);
	if( ctl->baseDN )
		gtk_entry_set_text(
			GTK_ENTRY(ldapedit.entry_baseDN), ctl->baseDN );
	if( ctl->bindDN )
		gtk_entry_set_text(
			GTK_ENTRY(ldapedit.entry_bindDN), ctl->bindDN );
	if( ctl->bindPass ) {
		pwd = ldapctl_get_bind_password( ctl );
		gtk_entry_set_text(	GTK_ENTRY(ldapedit.entry_bindPW),  pwd );
		g_free(pwd);
	}
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON(ldapedit.spinbtn_timeout), ctl->timeOut );
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON(ldapedit.spinbtn_maxentry), ctl->maxEntries );
#ifdef USE_LDAP_TLS
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(ldapedit.enable_tls), ctl->enableTLS );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(ldapedit.enable_ssl), ctl->enableSSL );
#endif
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON(ldapedit.spinbtn_port), ctl->port );
	/* Format criteria */
	crit = editldap_build_criteria_list( ctl );
	if( crit ) {
		gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_criteria), crit );
		g_free( crit );
	}
	else {
		gtk_entry_set_text(GTK_ENTRY(ldapedit.entry_criteria), "" );
	}
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON(ldapedit.spinbtn_queryage), ctl->maxQueryAge );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( ldapedit.check_dynsearch), server->searchFlag );
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON( ldapedit.check_matchoption),
		( ctl->matchingOption == LDAPCTL_MATCH_CONTAINS ) );
}

/**
 * Edit LDAP server datasource that appears addressbook.
 * \param addrIndex Address index object.
 * \param ads       Data source adapter.
 * \return Update data source adapter, or <code>NULL</code> if user cancelled
 *         edit with dialog.
 */
AdapterDSource *addressbook_edit_ldap(
	AddressIndex *addrIndex, AdapterDSource *ads )
{
	static gboolean cancelled;
	gchar *sName, *sHost, *sBase, *sBind, *sPass, *sCrit;
	gint iPort, iMaxE, iTime, iAge;
	gboolean bSrch, bMatch;
	AddressDataSource *ds = NULL;
	LdapServer *server = NULL;
	LdapControl *ctl = NULL;
	gboolean fin, ssl = FALSE, tls = FALSE;

	if (!ldapedit.window)
		addressbook_edit_ldap_create(&cancelled);
	gtk_notebook_set_current_page( GTK_NOTEBOOK(ldapedit.notebook), PAGE_BASIC );
	gtk_widget_grab_focus(ldapedit.ok_btn);
	gtk_widget_grab_focus(ldapedit.entry_name);
	gtk_widget_show(ldapedit.window);
	manage_window_set_transient(GTK_WINDOW(ldapedit.window));
	gtk_window_set_modal(GTK_WINDOW(ldapedit.window), TRUE);

	edit_ldap_status_show( "" );
	if( ads ) {
		ds = ads->dataSource;
		server = ds->rawDataSource;
		edit_ldap_set_fields( server );
		gtk_window_set_title(
			GTK_WINDOW(ldapedit.window), _("Edit LDAP Server"));
	}
	else {
		edit_ldap_clear_fields();
		gtk_window_set_title(
			GTK_WINDOW(ldapedit.window), _("Add New LDAP Server"));
	}

	gtk_main();
	gtk_widget_hide(ldapedit.window);
	gtk_window_set_modal(GTK_WINDOW(ldapedit.window), FALSE);
	if (cancelled == TRUE) return NULL;

	sName = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_name), 0, -1 );
	sHost = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_server), 0, -1 );
	sBase = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_baseDN), 0, -1 );
	sCrit = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_criteria), 0, -1 );
	sBind = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_bindDN), 0, -1 );
	sPass = gtk_editable_get_chars(
			GTK_EDITABLE(ldapedit.entry_bindPW), 0, -1 );
	iPort = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON( ldapedit.spinbtn_port ) );
	iTime = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON( ldapedit.spinbtn_timeout ) );
	iMaxE = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON( ldapedit.spinbtn_maxentry ) );
	iAge  = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON( ldapedit.spinbtn_queryage ) );
	bSrch = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON( ldapedit.check_dynsearch ) );
	bMatch = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON( ldapedit.check_matchoption ) );
#ifdef USE_LDAP_TLS
	ssl = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON( ldapedit.enable_ssl ) );
	tls = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON( ldapedit.enable_tls ) );
#endif
	debug_print("saving server config:\nname: %s\nhost: %s\nbase: %s\ncriteria: %s\nbind: %s\nport: %d\ntime: %d\nmax_entries: %d\ntimeout: %d\ndynamic: %d\ncheck_match: %d\n",
			sName, sHost, sBase, sCrit, sBind, iPort, iTime, iMaxE, iAge, bSrch, bMatch);
	fin = FALSE;
	if( *sName == '\0' ) fin = TRUE;
	if( *sHost == '\0' ) fin = TRUE;

	if( ! fin ) {
		/* Save changes */
		if( ! ads ) {
			/* New server */
			server = ldapsvr_create();
			ds = addrindex_index_add_datasource(
				addrIndex, ADDR_IF_LDAP, server );
			ads = addressbook_create_ds_adapter(
				ds, ADDR_LDAP, NULL );
		}
		ctl = server->control;
		addressbook_ads_set_name( ads, sName );
		ldapsvr_set_name( server, sName );
		ldapsvr_set_search_flag( server, bSrch );
		ldapctl_set_host( ctl, sHost );
		ldapctl_set_base_dn( ctl, sBase );
		ldapctl_set_bind_dn( ctl, sBind );
		ldapctl_set_bind_password( ctl, sPass, TRUE, TRUE );
		ldapctl_set_port( ctl, iPort );
		ldapctl_set_max_entries( ctl, iMaxE );
		ldapctl_set_timeout( ctl, iTime );
		ldapctl_set_max_query_age( ctl, iAge );
		ldapctl_set_tls( ctl, tls );
		ldapctl_set_ssl( ctl, ssl );
		ldapctl_set_matching_option(
			ctl, bMatch ?
			LDAPCTL_MATCH_CONTAINS : LDAPCTL_MATCH_BEGINWITH );

		addrindex_save_data(addrIndex);

		/* Save attributes */
		editldap_parse_criteria( sCrit, ctl );

	}
	g_free( sName );
	g_free( sHost );
	g_free( sBase );
	g_free( sBind );
	g_free( sPass );
	g_free( sCrit );

	return ads;
}

#endif /* USE_LDAP */

/*
* End of Source.
*/
