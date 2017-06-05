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

#ifndef __ADDRQUERY_H__
#define __ADDRQUERY_H__

#include <glib.h>
#include <stdio.h>
#include <sys/time.h>
#include "addritem.h"

/* Query types */
#define ADDRQUERY_NONE  0
#define ADDRQUERY_LDAP  1

/* Search type */
typedef enum {
	ADDRSEARCH_NONE,
	ADDRSEARCH_DYNAMIC,
	ADDRSEARCH_EXPLICIT,
	ADDRSEARCH_LOCATE
} AddrSearchType;

/* Data structures */
typedef struct {
	gint           queryID;
	AddrSearchType searchType;
	gchar          *searchTerm;
	time_t         timeStart;
	void           ( *callBackEnd ) ( void * );
	void           ( *callBackEntry ) ( void * );
	GList          *queryList;
}
QueryRequest;

/* Some macros */
#define ADDRQUERY_OBJECT(obj)		((AddrQueryObject *)obj)
#define ADDRQUERY_TYPE(obj)		(ADDRQUERY_OBJECT(obj)->queryType)
#define ADDRQUERY_ID(obj)		(ADDRQUERY_OBJECT(obj)->queryID)
#define ADDRQUERY_SEARCHTYPE(obj)	(ADDRQUERY_OBJECT(obj)->searchType)
#define ADDRQUERY_NAME(obj)		(ADDRQUERY_OBJECT(obj)->queryName)
#define ADDRQUERY_RETVAL(obj)		(ADDRQUERY_OBJECT(obj)->retVal)
#define ADDRQUERY_FOLDER(obj)		(ADDRQUERY_OBJECT(obj)->folder)
#define ADDRQUERY_SEARCHVALUE(obj)	(ADDRQUERY_OBJECT(obj)->searchValue)

/* Generic address query (base class) */
typedef struct _AddrQueryObject AddrQueryObject;
struct _AddrQueryObject {
	gint           queryType;
	gint           queryID;
	AddrSearchType searchType;
	gchar          *queryName;
	gint           retVal;
	ItemFolder     *folder;		/* Reference to folder in cache */
	gchar          *searchValue;
};

/* Address search call back functions */
typedef gint ( AddrSearchCallbackEntry ) ( gpointer sender,
				  	   gint queryID,
					   GList *listEMail,
					   gpointer data );

typedef void ( AddrSearchCallbackEnd ) ( gpointer sender,
					 gint queryID,
					 gint status,
	       				 gpointer data );

/* Function prototypes */
QueryRequest *qryreq_create	( void );
void qryreq_set_search_type	( QueryRequest *req, const AddrSearchType value );
void qryreq_add_query		( QueryRequest *req, AddrQueryObject *aqo );

void qrymgr_initialize		( void );
void qrymgr_teardown		( void );
QueryRequest *qrymgr_add_request( const gchar *searchTerm,
				  void *callBackEnd,
				  void *callBackEntry );

QueryRequest *qrymgr_find_request( const gint queryID );
void qrymgr_delete_request	( const gint queryID );

#endif /* __ADDRQUERY_H__ */

/*
* End of Source.
*/
