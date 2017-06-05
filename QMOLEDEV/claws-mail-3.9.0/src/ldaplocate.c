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
 * Functions to perform searches for LDAP entries.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_LDAP

#include <glib.h>
#include "addrquery.h"
#include "ldapserver.h"
#include "ldapquery.h"

void ldapsvr_add_query( LdapServer *server, LdapQuery *qry );
void ldapsvr_execute_query( LdapServer *server, LdapQuery *qry );
/**
 * Setup the search that will be performed and registered with the query
 * manager.
 *
 * \param  server        LDAP server object.
 * \param  searchTerm    Search term to locate.
 * \param  callBackEntry Function to call when each attribute is returned.
 * \param  callBackEnd   Function to call when search is complete.
 * \return Query ID allocated to query that will be executed.
 */
gint ldaplocate_search_setup(
	LdapServer *server, const gchar *searchTerm, void *callBackEntry,
	void *callBackEnd )
{
	QueryRequest *req;
	LdapQuery *qry;
	gint queryID;
	gchar *name;

	/* Name the query */
	name = g_strdup_printf( "Locate '%s'", searchTerm );

	/* Set up a generic address query */
	req = qrymgr_add_request( searchTerm, callBackEnd, callBackEntry );
	qryreq_set_search_type( req, ADDRSEARCH_LOCATE );
	queryID = req->queryID;

	/* Construct a query */
	qry = ldapqry_create();
	ldapqry_set_query_id( qry, queryID );
	ldapqry_set_name( qry, name );
	ldapqry_set_search_value( qry, searchTerm );
	ldapqry_set_search_type( qry, ADDRSEARCH_LOCATE );
	ldapqry_set_callback_end( qry, callBackEnd );
	ldapqry_set_callback_entry( qry, callBackEntry );

	/* Setup server */
	ldapsvr_add_query( server, qry );

	/* Set up query request */
	qryreq_add_query( req, ADDRQUERY_OBJECT(qry) );

	g_free( name );

	return queryID;
}

/**
 * Perform the previously registered search.
 * \param  queryID    ID of search query to be executed.
 * \return <i>TRUE</i> if search started successfully, or <i>FALSE</i> if
 *         failed.
 */
gboolean ldaplocate_search_start( const gint queryID ) {
	gboolean retVal;
	LdapServer *server;
	LdapQuery *qry;
	QueryRequest *req;
	AddrQueryObject *aqo;

	retVal = FALSE;
	req = qrymgr_find_request( queryID );
	if( req == NULL ) {
		return retVal;
	}

	/* Note: there should only be one query in the list */
	aqo = req->queryList->data;
	if( aqo->queryType == ADDRQUERY_LDAP ) {
		qry = ( LdapQuery * ) aqo;
		server = qry->server;

		/* Retire any aged queries */
		ldapsvr_retire_query( server );

		/* Start the search */
		retVal = TRUE;
		ldapsvr_execute_query( server, qry );
	}
	return retVal;
}

/**
 * Notify search to stop execution.
 * \param queryID Query to terminate.
 */
void ldaplocate_search_stop( const gint queryID ) {
	QueryRequest *req;
	AddrQueryObject *aqo;
	LdapQuery *qry;

	req = qrymgr_find_request( queryID );
	if( req == NULL ) {
		return;
	}

	aqo = req->queryList->data;
	if( aqo->queryType == ADDRQUERY_LDAP ) {
		/* Notify query to stop */
		qry = ( LdapQuery * ) aqo;
		ldapqry_set_stop_flag( qry, TRUE );
	}
	req->queryList->data = NULL;

	/* Delete query */
	qrymgr_delete_request( queryID );
}

#endif	/* USE_LDAP */

/*
 * End of Source.
 */


