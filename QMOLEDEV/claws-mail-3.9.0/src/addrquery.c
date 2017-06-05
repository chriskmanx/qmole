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
 * Functions to define an address query (a request).
 */

#include <stdio.h>
#include <string.h>
#include <glib.h>
#include <pthread.h>

#include "mgutils.h"
#include "addrquery.h"
#include "utils.h"

/**
 * Query list for tracking current queries.
 */
static GList *_requestList_ = NULL;

/**
 * Mutex to protect list from multiple threads.
 */
static pthread_mutex_t _requestListMutex_ = PTHREAD_MUTEX_INITIALIZER;

/**
 * Current query ID. This is incremented for each query request created.
 */
static gint _currentQueryID_ = 0;

/**
 * Clear the query.
 * \param req Request query object.
 */
static void qryreq_clear( QueryRequest *req ) {
	GList *node;

	cm_return_if_fail( req != NULL );
	g_free( req->searchTerm );
	req->queryID = 0;
	req->searchType = ADDRSEARCH_NONE;
	req->searchTerm = NULL;
	req->callBackEnd = NULL;
	req->callBackEntry = NULL;

	/* Empty the list */
	node = req->queryList;
	while( node ) {
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( req->queryList );
	req->queryList = NULL;
}

/**
 * Free query.
 * \param req Request query object.
 */
static void qryreq_free( QueryRequest *req ) {
	cm_return_if_fail( req != NULL );
	qryreq_clear( req );
	g_free( req );
}

/**
 * Specify search type.
 * \param req   Request query object.
 * \param value Type.
 */
void qryreq_set_search_type( QueryRequest *req, const AddrSearchType value ) {
	cm_return_if_fail( req != NULL );
	req->searchType = value;
}

/**
 * Add address query object to request.
 * \param req  Request query object.
 * \param aqo  Address query object that performs the search.
 */
void qryreq_add_query( QueryRequest *req, AddrQueryObject *aqo ) {
	cm_return_if_fail( req != NULL );
	cm_return_if_fail( aqo != NULL );
	req->queryList = g_list_append( req->queryList, aqo );
}

/**
 * Add query to list.
 *
 * \param searchTerm    Search term. A private copy will be made.
 * \param callBackEnd   Callback function that will be called when query
 * 			terminates.
 * \param callBackEntry Callback function that will be called after each
 * 			address entry has been read.
 * \return Initialize query request object. 			
 */
QueryRequest *qrymgr_add_request(
	const gchar *searchTerm, void *callBackEnd, void *callBackEntry )
{
	QueryRequest *req;

	req = g_new0( QueryRequest, 1 );
	req->searchTerm = g_strdup( searchTerm );
	req->callBackEnd = callBackEnd;
	req->callBackEntry = callBackEntry;
	req->timeStart = time( NULL );
	req->queryList = NULL;

	/* Insert in head of list */
	pthread_mutex_lock( & _requestListMutex_ );
	req->queryID = ++_currentQueryID_;
	_requestList_ = g_list_prepend( _requestList_, req );
	pthread_mutex_unlock( & _requestListMutex_ );

	return req;
}

/**
 * Find query in list.
 * \param  queryID ID of query to find.
 * \return Query object, or <i>NULL</i> if not found.
 */
QueryRequest *qrymgr_find_request( const gint queryID ) {
	QueryRequest *req;
	QueryRequest *q;
	GList *node;

	pthread_mutex_lock( & _requestListMutex_ );
	req = NULL;
	node = _requestList_;
	while( node ) {
		q = node->data;
		if( q->queryID == queryID ) {
			req = q;
			break;
		}
		node = g_list_next( node );
	}
	pthread_mutex_unlock( & _requestListMutex_ );

	return req;
}

/**
 * Delete specified query.
 * \param  queryID ID of query to retire.
 */
void qrymgr_delete_request( const gint queryID ) {
	QueryRequest *req;
	GList *node, *nf;

	pthread_mutex_lock( & _requestListMutex_ );

	/* Find node */
	nf = NULL;
	node = _requestList_;
	while( node ) {
		req = node->data;
		if( req->queryID == queryID ) {
			nf = node;
			qryreq_free( req );
			break;
		}
		node = g_list_next( node );
	}

	/* Free link element and associated query */
	if( nf ) {
		_requestList_ = g_list_remove_link( _requestList_, nf );
		g_list_free_1( nf );
	}

	pthread_mutex_unlock( & _requestListMutex_ );
}

/**
 * Initialize query manager.
 */
void qrymgr_initialize( void ) {
	_requestList_ = NULL;
}

/**
 * Free all queries.
 */
static void qrymgr_free_all_request( void ) {
	QueryRequest *req;
	GList *node;

	pthread_mutex_lock( & _requestListMutex_ );
	node = _requestList_;
	while( node ) {
		req = node->data;
		qryreq_free( req );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( _requestList_ );
	_requestList_ = NULL;
	pthread_mutex_unlock( & _requestListMutex_ );
}

/**
 * Teardown query manager.
 */
void qrymgr_teardown( void ) {
	qrymgr_free_all_request();
}

/*
* End of Source.
*/


