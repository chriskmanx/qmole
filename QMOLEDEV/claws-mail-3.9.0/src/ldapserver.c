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
 * Functions necessary to access LDAP servers.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_LDAP

#include <glib.h>
#include <sys/time.h>
#include <string.h>

#include "mgutils.h"
#include "addritem.h"
#include "addrcache.h"
#include "ldapctrl.h"
#include "ldapquery.h"
#include "ldapserver.h"
#include "ldaputil.h"
#include "utils.h"
#include "adbookbase.h"

/**
 * Create new LDAP server interface object with no control object.
 * \return Initialized LDAP server object.
 */
LdapServer *ldapsvr_create_noctl( void ) {
	LdapServer *server;

	server = g_new0( LdapServer, 1 );
	server->type = ADBOOKTYPE_LDAP;
	server->addressCache = addrcache_create();
	server->retVal = MGU_SUCCESS;
	server->control = NULL;
	server->listQuery = NULL;
	server->searchFlag = FALSE;
	return server;
}

/**
 * Create new LDAP server interface object.
 * \return Initialized LDAP server object.
 */
LdapServer *ldapsvr_create( void ) {
	LdapServer *server;
	server = ldapsvr_create_noctl();
	server->control = ldapctl_create();
	return server;
}

/**
 * Return name of server.
 * \param  server Server object.
 * \return Name for server.
 */
gchar *ldapsvr_get_name( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, NULL );
	return addrcache_get_name( server->addressCache );
}

/**
 * Specify name to be used.
 * \param server Server object.
 * \param value      Name for server.
 */
void ldapsvr_set_name( LdapServer* server, const gchar *value ) {
	cm_return_if_fail( server != NULL );
	addrcache_set_name( server->addressCache, value );
	debug_print("setting name: %s\n", value?value:"null");
}

/**
 * Refresh internal variables to force a file read.
 * \param server Server object.
 */
void ldapsvr_force_refresh( LdapServer *server ) {
	cm_return_if_fail( server != NULL );
	addrcache_refresh( server->addressCache );
}

/**
 * Return status/error code.
 * \param  server Server object.
 * \return Status/error code.
 */
gint ldapsvr_get_status( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, -1 );
	return server->retVal;
}

/**
 * Return reference to root level folder.
 * \param  server Server object.
 * \return Root level folder.
 */
ItemFolder *ldapsvr_get_root_folder( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, NULL );
	/*
	g_print( "ldapsvr_get_root_folder/start\n" );
	ldapsvr_print_data( server, stdout );
	g_print( "ldapsvr_get_root_folder/done\n" );
	*/
	return addrcache_get_root_folder( server->addressCache );
}

/**
 * Test whether server data has been accessed.
 * \param  server Server object.
 * \return <i>TRUE</i> if data was accessed.
 */
gboolean ldapsvr_get_accessed( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, FALSE );
	return server->addressCache->accessFlag;
}

/**
 * Specify that server's data whas beed accessed.
 * \param server Server object.
 * \param value      Value for flag.
 */
void ldapsvr_set_accessed( LdapServer *server, const gboolean value ) {
	cm_return_if_fail( server != NULL );
	server->addressCache->accessFlag = value;
	debug_print("setting accessFlag: %d\n", value);
}

/**
 * Test whether server data has been modified.
 * \param  server Server object.
 * \return <i>TRUE</i> if data was modified.
 */
gboolean ldapsvr_get_modified( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, FALSE );
	return server->addressCache->modified;
}

/**
 * Specify modify flag.
 * \param server Server object.
 * \param value      Value for flag.
 */
void ldapsvr_set_modified( LdapServer *server, const gboolean value ) {
	cm_return_if_fail( server != NULL );
	server->addressCache->modified = value;
	debug_print("setting modified: %d\n", value);
}

/**
 * Test whether data was read from server.
 * \param server Server object.
 * \return <i>TRUE</i> if data was read.
 */
gboolean ldapsvr_get_read_flag( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, FALSE );
	return server->addressCache->dataRead;
}

/**
 * Test whether server is to be used for dynamic searches.
 * \param server Server object.
 * \return <i>TRUE</i> if server is used for dynamic searches.
 */
gboolean ldapsvr_get_search_flag( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, FALSE );
	return server->searchFlag;
}

/**
 * Specify that server is to be used for dynamic searches.
 * \param server Server object.
 * \param value      Name for server.
 */
void ldapsvr_set_search_flag( LdapServer *server, const gboolean value ) {
	cm_return_if_fail( server != NULL );
	server->searchFlag = value;
	debug_print("setting searchFlag: %d\n", value);
}

/**
 * Specify the reference to control data that will be used for the query. The calling
 * module should be responsible for creating and destroying this control object.
 * \param server Server object.
 * \param ctl    Control data.
 */
void ldapsvr_set_control( LdapServer *server, LdapControl *ctl ) {
	cm_return_if_fail( server != NULL );
	addrcache_refresh( server->addressCache );
	server->control = ctl;
}

/**
 * Free all queries.
 * \param server Server object.
 */
void ldapsvr_free_all_query( LdapServer *server ) {
	GList *node;	
	cm_return_if_fail( server != NULL );

	node = server->listQuery;
	while( node ) {
		LdapQuery *qry = node->data;
		ldapqry_free( qry );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( server->listQuery );
	server->listQuery = NULL;
}

/**
 * Add query to server.
 * \param server Server object.
 * \param qry    Query object.
 */
void ldapsvr_add_query( LdapServer *server, LdapQuery *qry ) {
	cm_return_if_fail( server != NULL );
	cm_return_if_fail( qry != NULL );

	server->listQuery = g_list_append( server->listQuery, qry );
	qry->server = server;
}

/**
 * Free up LDAP server interface object by releasing internal memory.
 * \param server Server object.
 */
void ldapsvr_free( LdapServer *server ) {
	cm_return_if_fail( server != NULL );

	/* Stop and cancel any queries that may be active */
	ldapsvr_stop_all_query( server );
	ldapsvr_cancel_all_query( server );

	/* Clear cache */
	addrcache_clear( server->addressCache );
	addrcache_free( server->addressCache );

	/* Free LDAP control block */
	ldapctl_free( server->control );
	server->control = NULL;

	/* Free all queries */
	ldapsvr_free_all_query( server );

	/* Clear pointers */
	server->type = ADBOOKTYPE_NONE;
	server->addressCache = NULL;
	server->retVal = MGU_SUCCESS;
	server->listQuery = NULL;
	server->searchFlag = FALSE;

	/* Now release LDAP object */
	g_free( server );
}

/**
 * Display object to specified stream.
 * \param server Server object.
 * \param stream     Output stream.
 */
void ldapsvr_print_data( LdapServer *server, FILE *stream ) {
	GList *node;
	gint  i;

	cm_return_if_fail( server != NULL );

	fprintf( stream, "LdapServer:\n" );
	fprintf( stream, "  ret val: %d\n", server->retVal );
	fprintf( stream, "srch flag: %s\n",
			server->searchFlag ? "yes" : "no" );
	if( server->control ) {
		ldapctl_print( server->control, stream );
	}
	else {
		fprintf( stream, "  control: NULL\n" );
	}
	addrcache_print( server->addressCache, stream );
	addritem_print_item_folder( server->addressCache->rootFolder, stream );

	/* Dump queries */
	i = 1;
	node = server->listQuery;
	while( node ) {
		LdapQuery *qry = node->data;
		fprintf( stream, "    query: %2d : %s\n", i, ADDRQUERY_NAME(qry) );
		i++;
		node = g_list_next( node );
	}
}

/**
 * Return link list of persons.
 * \param server Server object.
 * \return List of persons.
 */
GList *ldapsvr_get_list_person( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, NULL );
	return addrcache_get_list_person( server->addressCache );
}

/**
 * Return link list of folders. There are no "real" folders that are returned
 * from the server.
 * \param  server Server object.
 * \return List of folders.
 */
GList *ldapsvr_get_list_folder( LdapServer *server ) {
	cm_return_val_if_fail( server != NULL, NULL );
	/* return addrcache_get_list_folder( server->addressCache ); */
	return NULL;
}

/**
 * Execute specified query.
 * \param server LDAP server.
 * \param qry    LDAP query.
 */
void ldapsvr_execute_query( LdapServer *server, LdapQuery *qry ) {
	LdapControl *ctlCopy;

	cm_return_if_fail( server != NULL );
	cm_return_if_fail( qry != NULL );

	/* Copy server's control data to the query */
	ctlCopy = ldapctl_create();
	ldapctl_copy( server->control, ctlCopy );
	ldapqry_set_control( qry, ctlCopy );
	ldapqry_initialize();

	/* Perform query */	
	debug_print("ldapsvr_execute_query::checking query...\n");
	if( ldapqry_check_search( qry ) ) {
		debug_print("ldapsvr_execute_query::reading with thread...\n");
		ldapqry_read_data_th( qry );
		if(qry->server->retVal == LDAPRC_SUCCESS) {
			debug_print("ldapsvr_execute_query::SUCCESS with thread...\n");
		}
	}
	debug_print("ldapsvr_execute_query... terminated\n");
}

/**
 * Stop all queries for specified ID.
 * \param server Server object.
 * \param queryID    Query ID to stop.
 */
void ldapsvr_stop_query_id( LdapServer *server, const gint queryID ) {
	GList *node;	
	cm_return_if_fail( server != NULL );

	node = server->listQuery;
	while( node ) {
		LdapQuery *qry = node->data;
		if( ADDRQUERY_ID(qry) == queryID ) {
			/* Notify thread to stop */
			ldapqry_set_stop_flag( qry, TRUE );
		}
		node = g_list_next( node );
	}
}

/**
 * Stop all queries by notifying each thread to stop.
 * \param server Server object.
 */
void ldapsvr_stop_all_query( LdapServer *server ) {
	GList *node;	
	cm_return_if_fail( server != NULL );

	node = server->listQuery;
	while( node ) {
		LdapQuery *qry = node->data;
		ldapqry_set_stop_flag( qry, TRUE );
		node = g_list_next( node );
	}
}

/**
 * Cancel all query threads for server.
 * \param server Server object.
 */
void ldapsvr_cancel_all_query( LdapServer *server ) {
	GList *node;	
	cm_return_if_fail( server != NULL );

	node = server->listQuery;
	while( node ) {
		LdapQuery *qry = node->data;
		/* Notify thread to stop */
		ldapqry_set_stop_flag( qry, TRUE );
		/* Now cancel thread */
		ldapqry_cancel( qry );
		node = g_list_next( node );
	}
}

/**
 * Search most recent query for specified search term. The most recent
 * completed query is returned. If no completed query is found, the most recent
 * incomplete is returned.
 * \param server LdapServer.
 * \param searchTerm Search term to locate.
 * \return Query object, or <i>NULL</i> if none found.
 */
static LdapQuery *ldapsvr_locate_query(
	const LdapServer *server, const gchar *searchTerm )
{
	LdapQuery *incomplete = NULL;
	GList *node;	
	cm_return_val_if_fail( server != NULL, NULL );

	node = server->listQuery;
	node = g_list_last( node );
	/* Search backwards for query */
	while( node ) {
		LdapQuery *qry = node->data;
		if( g_utf8_collate( ADDRQUERY_SEARCHVALUE(qry), searchTerm ) == 0 ) {
			if( qry->agedFlag ) continue;
			if( qry->completed ) {
				/* Found */
				return qry;
			}
			if( ! incomplete ) {
				incomplete = qry;
			}
		}
		node = g_list_previous( node );
	}
	return incomplete;
}

/**
 * Retire aged queries. Only the following queries are retired:
 *
 * a) Dynamic queries.
 * b) Explicit searches that have a hidden folders.
 * c) Locate searches that have a hidden folder.
 *
 * \param server LdapServer.
 */
void ldapsvr_retire_query( LdapServer *server ) {
	GList *node;
	GList *listDelete;
	GList *listQuery;
	gint maxAge;
	LdapControl *ctl;
	ItemFolder *folder;

	debug_print("ldapsvr_retire_query\n");
	cm_return_if_fail( server != NULL );
	ctl = server->control;
	maxAge = ctl->maxQueryAge;

	/* Identify queries to age and move to deletion list */
	listDelete = NULL;
	node = server->listQuery;
	while( node ) {
		LdapQuery *qry = node->data;

		node = g_list_next( node );
		folder = ADDRQUERY_FOLDER(qry);
		if( folder == NULL ) continue;
		if( ! folder->isHidden ) {
			if( ADDRQUERY_SEARCHTYPE(qry) == ADDRSEARCH_EXPLICIT ) continue;
			if( ADDRQUERY_SEARCHTYPE(qry) == ADDRSEARCH_LOCATE ) continue;
		}

		ldapqry_age( qry, maxAge );
		if( qry->agedFlag ) {
			/* Delete folder associated with query */
			debug_print("deleting folder... ::%s::\n",
					ADDRQUERY_NAME(qry)?ADDRQUERY_NAME(qry):"null");
			ldapqry_delete_folder( qry );
			listDelete = g_list_append( listDelete, qry );
		}
	}

	/* Delete queries */
	listQuery = server->listQuery;
	node = listDelete;
	while( node ) {
		LdapQuery *qry = node->data;

		listQuery = g_list_remove( listQuery, qry );
		ldapqry_free( qry );
		node->data = NULL;
		node = g_list_next( node );
	}
	server->listQuery = listQuery;

	/* Free up deletion list */
	g_list_free( listDelete );
}

/**
 * Return results of a previous query by executing callback for each address
 * contained in specified folder.
 * 
 * \param folder  Address book folder to process.
 * \param req Address query request object.
 */
static void ldapsvr_previous_query(
	const ItemFolder *folder, const QueryRequest *req, AddrQueryObject *aqo )
{
	AddrSearchCallbackEntry *callBack;
	GList *listEMail;
	GList *node;
	GList *nodeEM;
	gpointer sender;

	sender = aqo;
	callBack = ( AddrSearchCallbackEntry * ) req->callBackEntry;
	if( callBack ) {
		listEMail = NULL;
		node = folder->listPerson;
		while( node ) {
			AddrItemObject *aio = node->data;
			if( aio &&  aio->type == ITEMTYPE_PERSON ) {
				ItemPerson *person = node->data;
				nodeEM = person->listEMail;
				while( nodeEM ) {
					ItemEMail *email = nodeEM->data;

					nodeEM = g_list_next( nodeEM );
					listEMail = g_list_append( listEMail, email );
				}
			}
			node = g_list_next( node );
		}
		( callBack ) ( sender, req->queryID, listEMail, NULL );
		/* // g_list_free( listEMail ); */
	}
}

/**
 * Reuse search results from a previous LDAP query. If there is a query that
 * has the same search term as specified in the query request, then the query
 * will be reused.
 *
 * \param server  LDAP server object.
 * \param req Address query object.
 * \return <i>TRUE</i> if previous query was used.
 */
gboolean ldapsvr_reuse_previous( const LdapServer *server, const QueryRequest *req ) {
	LdapQuery *qry;
	gchar *searchTerm;
	ItemFolder *folder;

	cm_return_val_if_fail( server != NULL, FALSE );
	cm_return_val_if_fail( req != NULL, FALSE );

	searchTerm = req->searchTerm;

	/* Test whether any queries for the same term exist */
	qry = ldapsvr_locate_query( server, searchTerm );
	if( qry ) {
		/* Touch query to ensure it hangs around for a bit longer */
		ldapqry_touch( qry );
		folder = ADDRQUERY_FOLDER(qry);
		if( folder ) {
			ldapsvr_previous_query( folder, req, ADDRQUERY_OBJECT(qry) );
			return TRUE;
		}
	}
	return FALSE;
}

/**
 * Construct a new LdapQuery object that will be used to perform an dynamic
 * search request.
 *
 * \param server LdapServer.
 * \param req    Query request.
 * \return LdapQuery object, or <i>NULL</i> if none created.
 */
LdapQuery *ldapsvr_new_dynamic_search( LdapServer *server, QueryRequest *req )
{
	LdapQuery *qry;
	gchar *name;
	gchar *searchTerm;
	ItemFolder *folder;

	cm_return_val_if_fail( server != NULL, NULL );
	cm_return_val_if_fail( req != NULL, NULL );

	/* Retire any aged queries */
	/* // ldapsvr_retire_query( server ); */

	/* Name of folder and query */
	searchTerm = req->searchTerm;
	name = g_strdup_printf( "Search '%s'", searchTerm );

	/* Create a folder for the search results */
	folder = addrcache_add_new_folder( server->addressCache, NULL );
	addritem_folder_set_name( folder, name );
	addritem_folder_set_remarks( folder, "" );

	/* Construct a query */
	qry = ldapqry_create();
	ldapqry_set_query_id( qry, req->queryID );
	ldapqry_set_search_value( qry, searchTerm );
	ldapqry_set_search_type( qry, ADDRSEARCH_DYNAMIC );
	ldapqry_set_callback_entry( qry, req->callBackEntry );
	ldapqry_set_callback_end( qry, req->callBackEnd );

	/* Specify folder type and back reference */
	ADDRQUERY_FOLDER(qry) = folder;
	folder->folderType = ADDRFOLDER_QUERY_RESULTS;
	folder->folderData = ( gpointer ) qry;
	folder->isHidden = TRUE;

	/* Name the query */
	ldapqry_set_name( qry, name );
	g_free( name );

	/* Add query to request */
	qryreq_add_query( req, ADDRQUERY_OBJECT(qry) );

	/* Now start the search */
	ldapsvr_add_query( server, qry );

	return qry;	
}

/**
 * Construct a new LdapQuery object that will be used to perform an explicit
 * search request.
 *
 * \param server LdapServer.
 * \param req    Query request.
 * \param folder Folder that will be used to contain search results.
 * \return LdapQuery object, or <i>NULL</i> if none created.
 */
LdapQuery *ldapsvr_new_explicit_search(
		LdapServer *server, QueryRequest *req, ItemFolder *folder )
{
	LdapQuery *qry;
	gchar *searchTerm;
	gchar *name;

	cm_return_val_if_fail( server != NULL, NULL );
	cm_return_val_if_fail( req != NULL, NULL );
	cm_return_val_if_fail( folder != NULL, NULL );

	/* Retire any aged queries */
	/* // ldapsvr_retire_query( server ); */

	/* Name the query */
	searchTerm = req->searchTerm;
	name = g_strdup_printf( "Explicit search for '%s'", searchTerm );

	/* Construct a query */
	qry = ldapqry_create();
	ldapqry_set_query_id( qry, req->queryID );
	ldapqry_set_name( qry, name );
	ldapqry_set_search_value( qry, searchTerm );
	ldapqry_set_search_type( qry, ADDRSEARCH_EXPLICIT );
	ldapqry_set_callback_end( qry, req->callBackEnd );
	ldapqry_set_callback_entry( qry, req->callBackEntry );

	/* Specify folder type and back reference */
	ADDRQUERY_FOLDER(qry) = folder;
	folder->folderType = ADDRFOLDER_QUERY_RESULTS;
	folder->folderData = ( gpointer ) qry;

	/* Setup server */
	ldapsvr_add_query( server, qry );

	/* Set up query request */
	qryreq_add_query( req, ADDRQUERY_OBJECT(qry) );

	g_free( name );

	return qry;
}

gint ldapsvr_read_data( LdapServer *server )
{
	gchar *name;

	cm_return_val_if_fail( server != NULL, -1 );

	name = addrcache_get_name(server->addressCache);
	debug_print("...addrbook_read_data :%s:\n", name?name:"null");
	
	addrcache_clear(server->addressCache);
	ldapsvr_free_all_query( server );
	server->listQuery = NULL;
	server->addressCache->modified = FALSE;
	server->addressCache->accessFlag = FALSE;
	server->addressCache->dataRead = TRUE;
	addrcache_set_dirty(server->addressCache, FALSE);
	return 0;
}

void ldapsrv_set_options (gint secs, LDAP *ld)
{
	static struct timeval timeout;
	int rc;
	int i;
	timeout.tv_sec = secs;
	timeout.tv_usec = 0;
#ifdef G_OS_UNIX
	i = LDAP_OPT_X_TLS_ALLOW;
	rc = ldap_set_option(NULL, LDAP_OPT_X_TLS_REQUIRE_CERT, &i);
	if (ld)
		debug_print("cert %s\n", ldaputil_get_error(ld));
	else
		debug_print("cert %s\n", ldap_err2string(rc));
	/* can crash old libldaps... */
	rc = ldap_set_option(NULL, LDAP_OPT_NETWORK_TIMEOUT, &timeout);
	if (ld)
		debug_print("tm %s\n", ldaputil_get_error(ld));
	else
		debug_print("tm %s\n", ldap_err2string(rc));
#endif
}

/**
 * Connect to LDAP server.
 * \param  ctl Control object to process.
 * \return LDAP Resource to LDAP.
 */
LDAP *ldapsvr_connect(LdapControl *ctl) {
	LDAP *ld = NULL;
	gint rc;
	gint version;
	gchar *uri = NULL;
	gchar *pwd;

	cm_return_val_if_fail(ctl != NULL, NULL);

	ldapsrv_set_options (ctl->timeOut, NULL);
	uri = g_strdup_printf("ldap%s://%s:%d",
				ctl->enableSSL?"s":"",
				ctl->hostName, ctl->port);
#ifdef G_OS_UNIX
	ldap_initialize(&ld, uri);
#else
	ld = ldap_sslinit(ctl->hostName, ctl->port, ctl->enableSSL);
	if (ctl->enableSSL) {
		ldap_get_option(ld,LDAP_OPT_SSL,(void*)&rc);
		if ((void *)rc != LDAP_OPT_ON) {
			debug_print("Enabling SSL\n");
			if (ldap_set_option(ld,LDAP_OPT_SSL,LDAP_OPT_ON) != 0)
				debug_print("Failed: %s\n", ldaputil_get_error(ld));
		}
	}
#endif
	g_free(uri);

	if (ld == NULL)
		return NULL;


	debug_print("Got handle to LDAP host %s on port %d\n", ctl->hostName, ctl->port);

	version = LDAP_VERSION3;
	rc = ldap_set_option(ld, LDAP_OPT_PROTOCOL_VERSION, &version);
	if (rc == LDAP_OPT_SUCCESS) {
		ctl->version = LDAP_VERSION3;
	} else
		g_printerr("LDAP: Error %d (%s)\n",
			rc, ldaputil_get_error(ld));
#ifdef USE_LDAP_TLS
	/* Handle TLS */
	if (ctl->version == LDAP_VERSION3) {
		if (ctl->enableTLS && !ctl->enableSSL) {
			rc = ldap_start_tls_s(ld, NULL, NULL);
			
			if (rc != LDAP_SUCCESS) {
				g_printerr("LDAP Error(tls): ldap_simple_bind_s: %s\n",
					ldaputil_get_error(ld));
				return NULL;
			}
		}
	}
#endif

	/* Bind to the server, if required */
	if (ctl->bindDN) {
		if (* ctl->bindDN != '\0') {
			pwd = ldapctl_get_bind_password(ctl);
			rc = claws_ldap_simple_bind_s(ld, ctl->bindDN, pwd);
			if (rc != LDAP_SUCCESS) {
				g_printerr("bindDN: %s, bindPass xxx\n", ctl->bindDN);
				g_printerr("LDAP Error(bind): ldap_simple_bind_s: %s\n",
					ldaputil_get_error(ld));
				g_free(pwd);
				return NULL;
			}
			g_free(pwd);
		}
	}
	return ld;
}

/**
 * Disconnect to LDAP server.
 * \param ld Resource to LDAP.
 */
void ldapsvr_disconnect(LDAP *ld) {
	/* Disconnect */
	cm_return_if_fail(ld != NULL);
	ldap_unbind_ext(ld, NULL, NULL);
}

#endif	/* USE_LDAP */

/*
 * End of Source.
 */

