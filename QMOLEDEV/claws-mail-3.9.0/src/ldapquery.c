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
 * Functions necessary to define and perform LDAP queries.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_LDAP

#include <glib.h>
#include <sys/time.h>
#include <string.h>

#include "defs.h"
#include "ldaputil.h"
#include "ldapquery.h"
#include "ldapctrl.h"
#include "ldapserver.h"
#include "mgutils.h"

#include "addritem.h"
#include "addrcache.h"
#include "common/utils.h"

/*
 * Key for thread specific data.
 */
static pthread_key_t _queryThreadKey_;
static gboolean _queryThreadInit_ = FALSE;

static gboolean callbackend (gpointer data)
{
	LdapQuery *qry = (LdapQuery *)data;
	qry->callBackEnd( qry, ADDRQUERY_ID(qry), ADDRQUERY_RETVAL(qry), qry->data );
	return FALSE;
}


/**
 * Create new LDAP query object.
 * \return Initialized query object.
 */
LdapQuery *ldapqry_create( void ) {
	LdapQuery *qry;

	qry = g_new0( LdapQuery, 1 );
	ADDRQUERY_TYPE(qry) = ADDRQUERY_LDAP;
	ADDRQUERY_ID(qry) = 0;
	ADDRQUERY_SEARCHTYPE(qry) = ADDRSEARCH_NONE;
	ADDRQUERY_NAME(qry) = NULL;
	ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;
	ADDRQUERY_FOLDER(qry) = NULL;
	ADDRQUERY_SEARCHVALUE(qry) = NULL;
	qry->control = NULL;
	qry->server = NULL;
	qry->entriesRead = 0;
	qry->elapsedTime = 0;
	qry->stopFlag = FALSE;
	qry->busyFlag = FALSE;
	qry->agedFlag = FALSE;
	qry->completed = FALSE;
	qry->thread = NULL;
	qry->callBackEntry = NULL;
	qry->callBackEnd = NULL;
	qry->ldap = NULL;
	qry->data = NULL;

	/* Mutex to protect stop and busy flags */
	qry->mutexStop = g_malloc0( sizeof( pthread_mutex_t ) );
	pthread_mutex_init( qry->mutexStop, NULL );
	qry->mutexBusy = g_malloc0( sizeof( pthread_mutex_t ) );
	pthread_mutex_init( qry->mutexBusy, NULL );

	/* Mutex to protect critical section */
	qry->mutexEntry = g_malloc0( sizeof( pthread_mutex_t ) );
	pthread_mutex_init( qry->mutexEntry, NULL );

	return qry;
}

/**
 * Specify the reference to control data that will be used for the query. The calling
 * module should be responsible for creating and destroying this control object.
 * \param qry Query object.
 * \param ctl Control object.
 */
void ldapqry_set_control( LdapQuery *qry, LdapControl *ctl ) {
	cm_return_if_fail( qry != NULL );
	qry->control = ctl;
}

/**
 * Specify query name to be used.
 * \param qry   Query object.
 * \param value Name.
 */
void ldapqry_set_name( LdapQuery* qry, const gchar *value ) {
	cm_return_if_fail( qry != NULL );
	ADDRQUERY_NAME(qry) = mgu_replace_string( ADDRQUERY_NAME(qry), value );
	if (ADDRQUERY_NAME(qry) == NULL)
		return;
	g_strstrip( ADDRQUERY_NAME(qry) );
	debug_print("set name: %s\n", ADDRQUERY_NAME(qry));
}

/**
 * Specify search value to be used.
 * \param qry Query object.
 * \param value 
 */
void ldapqry_set_search_value( LdapQuery *qry, const gchar *value ) {
	cm_return_if_fail( qry != NULL );
	ADDRQUERY_SEARCHVALUE(qry) = mgu_replace_string( ADDRQUERY_SEARCHVALUE(qry), value );
	if (ADDRQUERY_SEARCHVALUE(qry) == NULL)
		return;
	g_strstrip( ADDRQUERY_SEARCHVALUE(qry) );
	debug_print("search value: %s\n", ADDRQUERY_SEARCHVALUE(qry));
}

/**
 * Specify query type.
 * \param qry Query object.
 * \param value Query type, either:
 * <ul>
 * <li><code>LDAPQUERY_NONE</code></li>
 * <li><code>LDAPQUERY_STATIC</code></li>
 * <li><code>LDAPQUERY_DYNAMIC</code></li>
 * </ul>
 */
/*
void ldapqry_set_query_type( LdapQuery* qry, const gint value ) {
	ADDRQUERY_TYPE(qry) = value;
}
*/

/**
 * Specify search type.
 * \param qry   Query object.
 * \param value Type.
 */
void ldapqry_set_search_type( LdapQuery *qry, const AddrSearchType value ) {
	cm_return_if_fail( qry != NULL );
	ADDRQUERY_SEARCHTYPE(qry) = value;
}

/**
 * Specify query ID.
 * \param qry Query object.
 * \param value ID for the query.
 */
void ldapqry_set_query_id( LdapQuery* qry, const gint value ) {
	cm_return_if_fail( qry != NULL );
	ADDRQUERY_ID(qry) = value;
}

/**
 * Register a callback function that will be executed when each entry
 * has been read and processed. When called, the function will be passed
 * this query object and a GList of ItemEMail objects as arguments. An
 * example of typical usage is shown below.
 *
 * <pre>
 * ------------------------------------------------------------
 * void myCallbackEntry( LdapQuery *qry, GList *listEMail ) {
 *   GList *node;
 *
 *   node = listEMail;
 *   while( node ) {
 *     ItemEMail *email = node->data;
 *     ... process email object ...
 *     node = g_list_next( node );
 *   }
 *   g_list_free( listEMail );
 * }
 * ...
 * ...
 * ldapqry_set_callback_entry( qry, myCallbackEntry );
 * ------------------------------------------------------------
 * </pre>
 *
 * \param qry Query object.
 * \param func Function.
 */
void ldapqry_set_callback_entry( LdapQuery *qry, void *func ) {
	pthread_mutex_lock( qry->mutexEntry );
	qry->callBackEntry = func;
	pthread_mutex_unlock( qry->mutexEntry );
}

/**
 * Register a callback function that will be executed when the search
 * is complete. When called, the function will be passed this query
 * object as an argument.
 * \param qry Query object.
 * \param func Function.
 */
void ldapqry_set_callback_end( LdapQuery *qry, void *func ) {
	qry->callBackEnd = func;
}

/**
 * Notify query to start/stop executing. This method should be called with a
 * value if <i>TRUE</i> to terminate an existing running query.
 *
 * \param qry Query object.
 * \param value Value of stop flag.
 */
void ldapqry_set_stop_flag( LdapQuery *qry, const gboolean value ) {
	cm_return_if_fail( qry != NULL );

	pthread_mutex_lock( qry->mutexStop );
	qry->stopFlag = value;
	pthread_mutex_unlock( qry->mutexStop );
}

/**
 * Test value of stop flag. This method should be used to determine whether a
 * query has stopped running.
 * \param qry Query object.
 * \return Value of stop flag.
 */
static gboolean ldapqry_get_stop_flag( LdapQuery *qry ) {
	gboolean value;
	cm_return_val_if_fail( qry != NULL, TRUE );

	pthread_mutex_lock( qry->mutexStop );
	value = qry->stopFlag;
	pthread_mutex_unlock( qry->mutexStop );
	return value;
}

/**
 * Set busy flag.
 * \param qry Query object.
 * \param value Value of busy flag.
 */
static void ldapqry_set_busy_flag( LdapQuery *qry, const gboolean value ) {
	cm_return_if_fail( qry != NULL );
	if (qry->mutexBusy == NULL)
		return; /* exiting, mutex already freed */

	pthread_mutex_lock( qry->mutexBusy );
	qry->busyFlag = value;
	pthread_mutex_unlock( qry->mutexBusy );
}

/**
 * Test value of busy flag. This method will return a value of <i>FALSE</i>
 * when a query has completed running.
 * \param qry Query object.
 * \return Value of busy flag.
 */
static gboolean ldapqry_get_busy_flag( LdapQuery *qry ) {
	gboolean value;
	cm_return_val_if_fail( qry != NULL, FALSE );

	pthread_mutex_lock( qry->mutexBusy );
	value = qry->busyFlag;
	pthread_mutex_unlock( qry->mutexBusy );
	return value;
}

/**
 * Set query aged flag.
 * \param qry Query object.
 * \param value Value of aged flag.
 */
static void ldapqry_set_aged_flag( LdapQuery *qry, const gboolean value ) {
	cm_return_if_fail( qry != NULL );
	qry->agedFlag = value;
}

/**
 * Clear LDAP query member variables.
 * \param qry Query object.
 */
static void ldapqry_clear( LdapQuery *qry ) {
	cm_return_if_fail( qry != NULL );

	/* Free internal stuff */
	g_free( ADDRQUERY_NAME(qry) );
	g_free( ADDRQUERY_SEARCHVALUE(qry) );

	/* Clear pointers and value */
	ADDRQUERY_NAME(qry) = NULL;
	ADDRQUERY_SEARCHVALUE(qry) = NULL;
	ADDRQUERY_ID(qry) = 0;
	ADDRQUERY_SEARCHTYPE(qry) = ADDRSEARCH_NONE;
	ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;
	qry->entriesRead = 0;
	qry->elapsedTime = 0;
	qry->stopFlag = FALSE;
	qry->busyFlag = FALSE;
	qry->agedFlag = FALSE;
	qry->completed = FALSE;
	qry->callBackEntry = NULL;
	qry->callBackEnd = NULL;
	qry->ldap = NULL;
	qry->data = NULL;
}

/**
 * Free up LDAP query object by releasing internal memory. Note that
 * the thread object will be freed by the OS.
 * \param qry Query object to process.
 */
void ldapqry_free( LdapQuery *qry ) {
	cm_return_if_fail( qry != NULL );

	/* Clear out internal members */
	ADDRQUERY_TYPE(qry) = ADDRQUERY_NONE;
	ldapqry_clear( qry );

	/* Free the mutex */
	pthread_mutex_destroy( qry->mutexStop );
	pthread_mutex_destroy( qry->mutexBusy );
	pthread_mutex_destroy( qry->mutexEntry );
	g_free( qry->mutexStop );
	g_free( qry->mutexBusy );
	g_free( qry->mutexEntry );
	qry->mutexEntry = NULL;
	qry->mutexBusy = NULL;
	qry->mutexStop = NULL;

	/* Do not free folder - parent server object should free */	
	ADDRQUERY_FOLDER(qry) = NULL;

	/* Do not free thread - thread should be terminated before freeing */
	qry->thread = NULL;

	/* Do not free LDAP control - should be destroyed before freeing */
	qry->control = NULL;

	/* Now release object */
	g_free( qry );
}

/**
 * Free linked lists of character strings.
 * \param listName  List of common names.
 * \param listAddr  List of addresses.
 * \param listFirst List of first names.
 * \param listLast  List of last names.
 */
static void ldapqry_free_lists(
		GSList *listName, GSList *listAddr, GSList *listFirst,
		GSList *listLast, GSList *listDisplay, GSList *other_attrs )
{
	GSList *cur = other_attrs;
	mgu_free_list( listName );
	mgu_free_list( listAddr );
	mgu_free_list( listFirst );
	mgu_free_list( listLast );
	mgu_free_list( listDisplay );
	for(;cur; cur = cur->next)
		addritem_free_attribute((UserAttribute *)cur->data);
	g_slist_free(other_attrs);
}

/**
 * Add all LDAP attribute values to a list.
 * \param ld LDAP handle.
 * \param entry LDAP entry to process.
 * \param attr  LDAP attribute.
 * \return List of values.
 */
static GSList *ldapqry_add_list_values(
		LDAP *ld, LDAPMessage *entry, char *attr )
{
	GSList *list = NULL;
	gint i;
	struct berval **vals;

	if( ( vals = ldap_get_values_len( ld, entry, attr ) ) != NULL ) {
		for( i = 0; vals[i] != NULL; i++ ) {
			/*debug_print("lv\t%s: %s\n", attr?attr:"null",
					vals[i]->bv_val?vals[i]->bv_val:"null");*/
			list = g_slist_append( list, g_strndup( vals[i]->bv_val, vals[i]->bv_len) );
		}
	}
	ldap_value_free_len( vals );
	return list;
}

/**
 * Add a single attribute value to a list.
 * \param  ld    LDAP handle.
 * \param  entry LDAP entry to process.
 * \param  attr  LDAP attribute name to process.
 * \return List of values; only one value will be present.
 */
static GSList *ldapqry_add_single_value( LDAP *ld, LDAPMessage *entry, char *attr ) {
	GSList *list = NULL;
	struct berval **vals;

	if( ( vals = ldap_get_values_len( ld, entry, attr ) ) != NULL ) {
		if( vals[0] != NULL ) {
			if (strcmp(attr, "jpegPhoto")) {
				debug_print("sv\t%s: %s\n", attr?attr:"null",
						vals[0]->bv_val?vals[0]->bv_val:"null");
				list = g_slist_append( list, g_strndup( vals[0]->bv_val, vals[0]->bv_len ));
			} else {
				char *file = get_tmp_file();
				FILE *fp = g_fopen(file, "wb");
				if (fp) {
					fwrite(vals[0]->bv_val, 1, vals[0]->bv_len, fp);
					fclose(fp);
				}
				list = g_slist_append( list, file);
			}
		}
	}
	ldap_value_free_len( vals );
	return list;
}

/**
 * Build an address list entry and append to list of address items. Name is formatted
 * as "<first-name> <last-name>".
 *
 * \param  cache     Address cache to load.
 * \param  qry Query object to process.
 * \param  dn        DN for entry found on server.
 * \param  listName  List of common names for entry; see notes below.
 * \param  listAddr  List of EMail addresses for entry.
 * \param  listFirst List of first names for entry.
 * \param  listLast  List of last names for entry.
 *
 * \return List of ItemEMail objects.
 *
 * Notes:
 * 1) Each LDAP server entry may have multiple LDAP attributes with the same
 *    name. For example, a single entry for a person may have more than one
 *    common name, email address, etc.
*
 * 2) The DN for the entry is unique for the server.
 */
static GList *ldapqry_build_items_fl(
		AddressCache *cache, LdapQuery *qry, gchar *dn,
		GSList *listName, GSList *listAddr, GSList *listFirst,
		GSList *listLast, GSList *listDisplay, GSList *attributes )
{
	GSList *nodeAddress, *cur;
	gchar *firstName = NULL, *lastName = NULL, *fullName = NULL;
	gboolean allocated = FALSE;
	ItemPerson *person;
	ItemEMail *email;
	ItemFolder *folder;
	gchar *picfile = NULL;
	GList *listReturn = NULL;

	folder = ADDRQUERY_FOLDER(qry);
	if( folder == NULL ) return listReturn;
	if( listAddr == NULL ) return listReturn;

	if ( listDisplay ) {
		allocated = FALSE;
		fullName = listDisplay->data;
	}

	/* Find longest first name in list */
	firstName = mgu_slist_longest_entry( listFirst );

	/* Format last name */
	if( listLast ) {
		lastName = listLast->data;
	}

	if ( fullName == NULL ) {
		/* Find longest common name */
		allocated = FALSE;
		fullName = mgu_slist_longest_entry( listName );
		if( fullName == NULL ) {
			/* Format a full name from first and last names */
			if( firstName ) {
				if( lastName ) {
					fullName = g_strdup_printf( "%s %s", firstName, lastName );
				}
				else {
					fullName = g_strdup_printf( "%s", firstName );
				}
			}
			else {
				if( lastName ) {
					fullName = g_strdup_printf( "%s", lastName );
				}
			}
			if( fullName ) {
				g_strchug( fullName ); g_strchomp( fullName );
				allocated = TRUE;
			}
		}
	}

	/* Add person into folder */		
	person = addritem_create_item_person();
	addritem_person_set_common_name( person, fullName );
	addritem_person_set_first_name( person, firstName );
	addritem_person_set_last_name( person, lastName );
	addritem_person_set_nick_name( person, fullName );
	addrcache_id_person( cache, person );
	addritem_person_set_external_id( person, dn );
	
	for (cur = attributes; cur; cur = cur->next) {
		UserAttribute *attrib = addritem_copy_attribute((UserAttribute *)cur->data);
		if (attrib->name && strcmp(attrib->name, "jpegPhoto")) {
			addritem_person_add_attribute( person, attrib );
		} else {
			if (qry && qry->server && qry->server->control) {
				gchar *dir = g_strconcat( get_rc_dir(), G_DIR_SEPARATOR_S, 
							ADDRBOOK_DIR, G_DIR_SEPARATOR_S, NULL );
				gchar *filename = g_strdup_printf("%s-%s-%s",
					qry->server->control->hostName?qry->server->control->hostName:"nohost", 
					qry->server->control->baseDN?qry->server->control->baseDN:"nobase", 
					dn);
				picfile = g_strdup_printf("%s%s.png", dir, filename);
				addritem_person_set_picture( person, filename );
				rename_force(attrib->value, picfile);
				g_free(filename);
				g_free(picfile);
				g_free(dir);
			}
		}
	}
	
	addrcache_folder_add_person( cache, ADDRQUERY_FOLDER(qry), person );

	qry->entriesRead++;

	/* Add each address item */
	nodeAddress = listAddr;
	while( nodeAddress ) {
		email = addritem_create_item_email();
		addritem_email_set_address( email, nodeAddress->data );
		addrcache_id_email( cache, email );
		addrcache_person_add_email( cache, person, email );
		addritem_person_add_email( person, email );
		/*if (debug_get_mode()) {
			addritem_print_item_email(email, stdout);
		}*/
		listReturn = g_list_append( listReturn, email );
		nodeAddress = g_slist_next( nodeAddress );
	}

	/* Free any allocated memory */
	if( allocated ) {
		g_free( fullName );
	}
	fullName = firstName = lastName = NULL;

	return listReturn;
}

/**
 * Process a single search entry.
 * \param  cache Address cache to load.
 * \param  qry   Query object to process.
 * \param  ld    LDAP handle.
 * \param  e     LDAP message.
 * \return List of EMail objects found.
 */
static GList *ldapqry_process_single_entry(
		AddressCache *cache, LdapQuery *qry, LDAP *ld, LDAPMessage *e )
{
	char *dnEntry;
	char *attribute;
	LdapControl *ctl;
	BerElement *ber;
	GSList *listName = NULL, *listAddress = NULL;
	GSList *listFirst = NULL, *listLast = NULL;
	GSList *listDisplay = NULL;
	GSList *other_attrs = NULL;
	GList *listReturn;

	listReturn = NULL;
	ctl = qry->control;
	dnEntry = ldap_get_dn( ld, e );
	debug_print( "DN: %s\n", dnEntry?dnEntry:"null" );

	/* Process all attributes */
	for( attribute = ldap_first_attribute( ld, e, &ber ); attribute != NULL;
		attribute = ldap_next_attribute( ld, e, ber ) ) {
		if( strcasecmp( attribute, ctl->attribEMail ) == 0 ) {
			listAddress = ldapqry_add_list_values( ld, e, attribute );
		}
		else if( strcasecmp( attribute, ctl->attribCName ) == 0 ) {
			listName = ldapqry_add_list_values( ld, e, attribute );
		}
		else if( strcasecmp( attribute, ctl->attribFName ) == 0 ) {
			listFirst = ldapqry_add_list_values( ld, e, attribute );
		}
		else if( strcasecmp( attribute, ctl->attribLName ) == 0 ) {
			listLast = ldapqry_add_single_value( ld, e, attribute );
		} else if( strcasecmp( attribute, ctl->attribDName ) == 0 ) {
			listDisplay = ldapqry_add_single_value( ld, e, attribute );
		} else {
			GSList *attlist = ldapqry_add_single_value( ld, e, attribute );
			UserAttribute *attrib = addritem_create_attribute();
			const gchar *attvalue = attlist?((gchar *)attlist->data):NULL;
			if (attvalue) {
				addritem_attrib_set_name( attrib, attribute );
				addritem_attrib_set_value( attrib, attvalue );
				other_attrs = g_slist_prepend(other_attrs, attrib);
			}
			mgu_free_list(attlist);
		}
		/* Free memory used to store attribute */
		ldap_memfree( attribute );
	}

	/* Format and add items to cache */
	listReturn = ldapqry_build_items_fl(
		cache, qry, dnEntry, listName, listAddress, listFirst, listLast, listDisplay, other_attrs );

	/* Free up */
	ldapqry_free_lists( listName, listAddress, listFirst, listLast, listDisplay, other_attrs );
	listName = listAddress = listFirst = listLast = listDisplay = other_attrs = NULL;

	if( ber != NULL ) {
		ber_free( ber, 0 );
	}
	g_free( dnEntry );

	return listReturn;
}

/**
 * Check parameters that are required for a search. This should
 * be called before performing a search.
 * \param  qry Query object to process.
 * \return <i>TRUE</i> if search criteria appear OK.
 */
gboolean ldapqry_check_search( LdapQuery *qry ) {
	LdapControl *ctl;
	ADDRQUERY_RETVAL(qry) = LDAPRC_CRITERIA;

	/* Test for control data */
	ctl = qry->control;
	if( ctl == NULL ) {
		return FALSE;
	}

	/* Test for search value */
	if( ADDRQUERY_SEARCHVALUE(qry) == NULL ) {
		return FALSE;
	}
	if( strlen( ADDRQUERY_SEARCHVALUE(qry) ) < 1 ) {
		return FALSE;
	}
	ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;
	return TRUE;
}

/**
 * Touch the query. This nudges the touch time with the current time.
 * \param qry Query object to process.
 */
void ldapqry_touch( LdapQuery *qry ) {
	qry->touchTime = time( NULL );
	qry->agedFlag = FALSE;
}

/**
 * Connect to LDAP server.
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_connect( LdapQuery *qry ) {
	LdapControl *ctl;
	LDAP *ld = NULL;

	/* Initialize connection */
	if (debug_get_mode()) {
		debug_print("===ldapqry_connect===\n");
		/*ldapqry_print(qry, stdout);*/
	}
	ctl = qry->control;
	/*if (debug_get_mode()) {
		ldapctl_print(ctl, stdout);
		debug_print("======\n");
	}*/
	ldapqry_touch( qry );
	qry->startTime = qry->touchTime;
	qry->elapsedTime = -1;
	ADDRQUERY_RETVAL(qry) = LDAPRC_INIT;

	ld = ldapsvr_connect(ctl);

	if (ld == NULL)
		return ADDRQUERY_RETVAL(qry);

	qry->ldap = ld;
	ADDRQUERY_RETVAL(qry) = LDAPRC_STOP_FLAG;
	if( ldapqry_get_stop_flag( qry ) ) {
		return ADDRQUERY_RETVAL(qry);
	}
	ldapqry_touch( qry );

	debug_print("connected to LDAP host %s on port %d\n",
			ctl->hostName?ctl->hostName:"null", ctl->port);

	ADDRQUERY_RETVAL(qry) = LDAPRC_STOP_FLAG;
	if( ldapqry_get_stop_flag( qry ) ) {
		return ADDRQUERY_RETVAL(qry);
	}
	ldapqry_touch( qry );

	ADDRQUERY_RETVAL(qry) = LDAP_SUCCESS;

	return ADDRQUERY_RETVAL(qry);
}

/**
 * Connect to LDAP server.
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_disconnect( LdapQuery *qry ) {
	/* Disconnect */
	if( qry->ldap ) ldap_unbind_ext( qry->ldap, NULL, NULL );
	qry->ldap = NULL;

	ldapqry_touch( qry );
	qry->elapsedTime = qry->touchTime - qry->startTime;

	return ADDRQUERY_RETVAL(qry);
}

/**
 * Perform the LDAP search, reading LDAP entries into cache.
 * Note that one LDAP entry can have multiple values for many of its
 * attributes. If these attributes are E-Mail addresses; these are
 * broken out into separate address items. For any other attribute,
 * only the first occurrence is read.
 * 
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_search_retrieve( LdapQuery *qry ) {
	LdapControl *ctl;
	LDAP *ld;
	LDAPMessage *result = NULL, *e = NULL;
	char **attribs;
	gchar *criteria;
	gboolean searchFlag;
	gboolean entriesFound;
	gboolean first;
	struct timeval timeout;
	gint rc;
	AddressCache *cache;
	GList *listEMail;

	/* Initialize some variables */
	ld = qry->ldap;
	ctl = qry->control;
	cache = qry->server->addressCache;
	timeout.tv_sec = ctl->timeOut;
	timeout.tv_usec = 0L;
	entriesFound = FALSE;
	ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;

	/* Define all attributes we are interested in. */
	attribs = ldapctl_full_attribute_array( ctl );

	/* Create LDAP search string */
	criteria = ldapctl_format_criteria( ctl, ADDRQUERY_SEARCHVALUE(qry) );
	debug_print("Search criteria ::%s::\n", criteria?criteria:"null");

	/*
	 * Execute the search - this step may take some time to complete
	 * depending on network traffic and server response time.
	 */
	ADDRQUERY_RETVAL(qry) = LDAPRC_TIMEOUT;
	rc = ldap_search_ext_s( ld, ctl->baseDN, LDAP_SCOPE_SUBTREE, criteria,
		attribs, 0, NULL, NULL, &timeout, 0, &result );
	debug_print("LDAP Error: ldap_search_st: %d\n", rc);
	debug_print("LDAP Error: ldap_search_st: %s\n", ldaputil_get_error(ld));
	ldapctl_free_attribute_array( attribs );
	g_free( criteria );
	criteria = NULL;
	if( rc == LDAP_TIMEOUT ) {
		return ADDRQUERY_RETVAL(qry);
	}
	ADDRQUERY_RETVAL(qry) = LDAPRC_SEARCH;

	/* Test valid returns */
	searchFlag = FALSE;
	if( rc == LDAP_ADMINLIMIT_EXCEEDED ) {
		searchFlag = TRUE;
	}
	else if( rc == LDAP_SUCCESS ) {
		searchFlag = TRUE;
	}
	else if( rc == LDAP_PARTIAL_RESULTS || (result && ldap_count_entries(ld, result) > 0) ) {
		searchFlag = TRUE;
	}
	else {
		debug_print("LDAP Error: ldap_search_st: %d\n", rc);
		debug_print("LDAP Error: ldap_search_st: %s\n", ldaputil_get_error(ld));
		return ADDRQUERY_RETVAL(qry);
	}
	ADDRQUERY_RETVAL(qry) = LDAPRC_STOP_FLAG;

	debug_print("Total results are: %d\n", ldap_count_entries(ld, result));

	/* Process results */
	first = TRUE;
	while( searchFlag ) {
		ldapqry_touch( qry );
		if( qry->entriesRead >= ctl->maxEntries ) break;		

		/* Test for stop */		
		if( ldapqry_get_stop_flag( qry ) ) {
			break;
		}

		/* Retrieve entry */		
		if( first ) {
			first = FALSE;
			e = ldap_first_entry( ld, result );
		}
		else {
			e = ldap_next_entry( ld, e );
		}
		if( e == NULL ) break;
		entriesFound = TRUE;

		/* Setup a critical section here */
		pthread_mutex_lock( qry->mutexEntry );

		/* Process entry */
		listEMail = ldapqry_process_single_entry( cache, qry, ld, e );

		/* Process callback */
		if( qry->callBackEntry )
			qry->callBackEntry( qry, ADDRQUERY_ID(qry), listEMail, qry->data );
		else
			g_list_free( listEMail );
		pthread_mutex_unlock( qry->mutexEntry );
	}

	/* Free up and disconnect */
	ldap_msgfree( result );

	if( searchFlag ) {
		if( entriesFound ) {
			ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;
		}
		else {
			ADDRQUERY_RETVAL(qry) = LDAPRC_NOENTRIES;
		}
	}

	return ADDRQUERY_RETVAL(qry);
}

/**
 * Connection, perform search and disconnect.
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_perform_search( LdapQuery *qry ) {
	/* Check search criteria */	
	if( ! ldapqry_check_search( qry ) ) {
		return ADDRQUERY_RETVAL(qry);
	}

	/* Connect */
	qry->ldap = NULL;
	ldapqry_connect( qry );
	if( ADDRQUERY_RETVAL(qry) == LDAPRC_SUCCESS ) {
		/* Perform search */
		ldapqry_search_retrieve( qry );
	}
	/* Disconnect */
	ldapqry_disconnect( qry );
	qry->ldap = NULL;

	return ADDRQUERY_RETVAL(qry);
}

static gint ldapqry_perform_locate( LdapQuery *qry );

/**
 * Wrapper around search.
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_search( LdapQuery *qry ) {
	gint retVal;

	cm_return_val_if_fail( qry != NULL, -1 );
	cm_return_val_if_fail( qry->control != NULL, -1 );

	ldapqry_touch( qry );
	qry->completed = FALSE;

	/* Setup pointer to thread specific area */
	pthread_setspecific( _queryThreadKey_, qry );

	pthread_detach( pthread_self() );
	
	/* Now perform the search */
	qry->entriesRead = 0;
	ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;
	ldapqry_set_busy_flag( qry, TRUE );
	ldapqry_set_stop_flag( qry, FALSE );
	if( ADDRQUERY_SEARCHTYPE(qry) == ADDRSEARCH_LOCATE ) {
		retVal = ldapqry_perform_locate( qry );
	}
	else {
		retVal = ldapqry_perform_search( qry );
	}
	if( retVal == LDAPRC_SUCCESS ) {
		qry->server->addressCache->dataRead = TRUE;
		qry->server->addressCache->accessFlag = FALSE;
		if( ldapqry_get_stop_flag( qry ) ) {
			debug_print("Search was terminated prematurely\n");
		}
		else {
			ldapqry_touch( qry );
			qry->completed = TRUE;
			debug_print("Search ran to completion\n");
		}
	}
	ldapqry_set_stop_flag( qry, TRUE );
	ldapqry_set_busy_flag( qry, FALSE );

	/* Process callback */	
	if( qry->callBackEnd ) {
		g_timeout_add(0, callbackend, qry);
	}

	return ADDRQUERY_RETVAL(qry);
}

/**
 * Read data into list using a background thread. Callback function will be
 * notified when search is complete.
 * \param  qry Query object to process.
 * \return Error/status code.
 */
gint ldapqry_read_data_th( LdapQuery *qry ) {
	cm_return_val_if_fail( qry != NULL, -1 );
	cm_return_val_if_fail( qry->control != NULL, -1 );

	ldapqry_set_stop_flag( qry, FALSE );
	ldapqry_touch( qry );
	if( ldapqry_check_search( qry ) ) {
		if( ADDRQUERY_RETVAL(qry) == LDAPRC_SUCCESS ) {
			debug_print("Starting LDAP search thread\n");
			ldapqry_set_busy_flag( qry, TRUE );
			qry->thread = g_malloc0( sizeof( pthread_t ) );

			/* Setup thread */			
			pthread_create( qry->thread, NULL,
				(void *) ldapqry_search, (void *) qry );
		}
	}
	return ADDRQUERY_RETVAL(qry);
}

/**
 * Cleanup LDAP thread data. This function will be called when each thread
 * exits. Note that the thread object will be freed by the kernel.
 * \param ptr Pointer to object being destroyed (a query object in this case).
 */
static void ldapqry_destroyer( void * ptr ) {
	LdapQuery *qry;

	qry = ( LdapQuery * ) ptr;
	cm_return_if_fail( qry != NULL );

	/* Perform any destruction here */
	if( qry->control != NULL ) {
		ldapctl_free( qry->control );
	}
	qry->control = NULL;
	qry->thread = NULL;
	ldapqry_set_busy_flag( qry, FALSE );
}

/**
 * Cancel thread associated with query.
 * \param qry Query object to process.
 */
void ldapqry_cancel( LdapQuery *qry ) {
	cm_return_if_fail( qry != NULL );

	if( ldapqry_get_busy_flag( qry ) ) {
		if( qry->thread ) {
			debug_print("calling pthread_cancel\n");
			pthread_cancel( * qry->thread );
		}
	}
}

/**
 * Initialize LDAP query. This function should be called once before executing
 * any LDAP queries to initialize thread specific data.
 */
void ldapqry_initialize( void ) {
	debug_print("ldapqry_initialize...\n");
	if( ! _queryThreadInit_ ) {
		debug_print("ldapqry_initialize::creating thread specific area\n");
		pthread_key_create( &_queryThreadKey_, ldapqry_destroyer );
		_queryThreadInit_ = TRUE;
	}
	debug_print("ldapqry_initialize... done!\n");
}

/**
 * Age the query based on LDAP control parameters.
 * \param qry    Query object to process.
 * \param maxAge Maximum age of query (in seconds).
 */
void ldapqry_age( LdapQuery *qry, gint maxAge ) {
	gint age;

	cm_return_if_fail( qry != NULL );

	/* Limit the time that queries can hang around */	
	if( maxAge < 1 ) maxAge = LDAPCTL_MAX_QUERY_AGE;

	/* Check age of query */
	age = time( NULL ) - qry->touchTime;
	if( age > maxAge ) {
		qry->agedFlag = TRUE;
	}
}

/**
 * Delete folder associated with query results.
 * \param qry Query object to process.
 */
void ldapqry_delete_folder( LdapQuery *qry ) {
	AddressCache *cache;
	ItemFolder *folder;

	cm_return_if_fail( qry != NULL );

	folder = ADDRQUERY_FOLDER(qry);
	if( folder ) {
		cache = qry->server->addressCache;
		folder = addrcache_remove_folder_delete( cache, folder );
		if( folder ) {
			addritem_free_item_folder( folder );
		}
		ADDRQUERY_FOLDER(qry) = NULL;
	}
}

/**
 * Create a name/value pair object.
 * \param n Name.
 * \param v Value.
 * \return Initialized object.
 */
static NameValuePair *ldapqry_create_name_value( const gchar *n, const gchar *v ) {
	NameValuePair *nvp = g_new0( NameValuePair, 1 );

	nvp->name = g_strdup( n );
	nvp->value = g_strdup( v );
	return nvp;
}

/**
 * Free up name/value pair object.
 * \param nvp Name/value object.
 */
void ldapqry_free_name_value( NameValuePair *nvp ) {
	if( nvp ) {
		g_free( nvp->name );
		g_free( nvp->value );
		nvp->name = nvp->value = NULL;
		g_free( nvp );
	}
}

/**
 * Free up a list name/value pair objects.
 * \param list List of name/value objects.
 */
void ldapqry_free_list_name_value( GList *list ) {
	GList *node;

	node = list;
	while( node ) {
		NameValuePair *nvp = ( NameValuePair * ) node->data;
		ldapqry_free_name_value( nvp );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( list );
}

/**
 * Load a list of name/value pairs from LDAP attributes.
 * \param  ld          LDAP handle.
 * \param  e          LDAP message.
 * \param  attr       Attribute name.
 * \param  listValues List to populate.
 * \return List of attribute name/value pairs.
 */
static GList *ldapqry_load_attrib_values(
		LDAP *ld, LDAPMessage *entry, char *attr,
		GList *listValues )
{
	GList *list = NULL;
	gint i;
	struct berval **vals;
	NameValuePair *nvp;

	list = listValues;
	if( ( vals = ldap_get_values_len( ld, entry, attr ) ) != NULL ) {
		for( i = 0; vals[i] != NULL; i++ ) {
			gchar *tmp = g_strndup( vals[i]->bv_val, vals[i]->bv_len);
			nvp = ldapqry_create_name_value( attr, tmp );
			g_free(tmp);
			list = g_list_append( list, nvp );
		}
	}
	ldap_value_free_len( vals );
	return list;
}

/**
 * Fetch a list of all attributes.
 * \param  ld    LDAP handle.
 * \param  e     LDAP message.
 * \return List of attribute name/value pairs.
 */
static GList *ldapqry_fetch_attribs( LDAP *ld, LDAPMessage *e )
{
	char *attribute;
	BerElement *ber;
	GList *listValues = NULL;

	/* Process all attributes */
	for( attribute = ldap_first_attribute( ld, e, &ber ); attribute != NULL;
		attribute = ldap_next_attribute( ld, e, ber ) ) {
		listValues = ldapqry_load_attrib_values( ld, e, attribute, listValues );
		ldap_memfree( attribute );
	}

	/* Free up */
	if( ber != NULL ) {
		ber_free( ber, 0 );
	}
	return listValues;
}

#define CRITERIA_SINGLE "(objectclass=*)"

/**
 * Perform the data retrieval for a specific LDAP record.
 * 
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_locate_retrieve( LdapQuery *qry ) {
	LdapControl *ctl;
	LDAP *ld;
	LDAPMessage *result, *e = NULL;
	gboolean entriesFound;
	gboolean first;
	struct timeval timeout;
	gint rc;
	gchar *dn;
	GList *listValues;

	/* Initialize some variables */
	ld = qry->ldap;
	ctl = qry->control;
	dn = ADDRQUERY_SEARCHVALUE(qry);
	timeout.tv_sec = ctl->timeOut;
	timeout.tv_usec = 0L;
	entriesFound = FALSE;

	/*
	 * Execute the search - this step may take some time to complete
	 * depending on network traffic and server response time.
	 */
	ADDRQUERY_RETVAL(qry) = LDAPRC_TIMEOUT;
	rc = ldap_search_ext_s( ld, dn, LDAP_SCOPE_BASE, CRITERIA_SINGLE,
		NULL, 0, NULL, NULL, &timeout, 0, &result );
	if( rc == LDAP_TIMEOUT ) {
		return ADDRQUERY_RETVAL(qry);
	}
	ADDRQUERY_RETVAL(qry) = LDAPRC_SEARCH;
	if( rc != LDAP_SUCCESS ) {
		debug_print("LDAP Error: ldap_search_st: %s\n", ldaputil_get_error(ld));
		return ADDRQUERY_RETVAL(qry);
	}

	debug_print("Total results are: %d\n", ldap_count_entries(ld, result));

	/* Process results */
	ADDRQUERY_RETVAL(qry) = LDAPRC_STOP_FLAG;
	first = TRUE;
	while( TRUE ) {
		ldapqry_touch( qry );
		if( qry->entriesRead >= ctl->maxEntries ) break;		

		/* Test for stop */
		if( ldapqry_get_stop_flag( qry ) ) {
			break;
		}

		/* Retrieve entry */		
		if( first ) {
			first = FALSE;
			e = ldap_first_entry( ld, result );
		}
		else {
			e = ldap_next_entry( ld, e );
		}
		if( e == NULL ) break;

		entriesFound = TRUE;

		/* Setup a critical section here */
		pthread_mutex_lock( qry->mutexEntry );

		/* Process entry */
		listValues = ldapqry_fetch_attribs( ld, e );

		/* Process callback */
		if( qry->callBackEntry ) {
			qry->callBackEntry( qry, ADDRQUERY_ID(qry), listValues, qry->data );
		}
		ldapqry_free_list_name_value( listValues );
		listValues = NULL;

		pthread_mutex_unlock( qry->mutexEntry );
	}

	/* Free up and disconnect */
	ldap_msgfree( result );

	if( entriesFound ) {
		ADDRQUERY_RETVAL(qry) = LDAPRC_SUCCESS;
	}
	else {
		ADDRQUERY_RETVAL(qry) = LDAPRC_NOENTRIES;
	}

	return ADDRQUERY_RETVAL(qry);
}

/**
 * Perform the search to locate a specific LDAP record identified by
 * distinguished name (dn).
 * 
 * \param  qry Query object to process.
 * \return Error/status code.
 */
static gint ldapqry_perform_locate( LdapQuery *qry ) {
	/* Connect */
	qry->ldap = NULL;
	ldapqry_connect( qry );
	if( ADDRQUERY_RETVAL(qry) == LDAPRC_SUCCESS ) {
		/* Perform search */
		ldapqry_locate_retrieve( qry );
	}
	/* Disconnect */
	ldapqry_disconnect( qry );
	qry->ldap = NULL;

	/* Process callback */	
	if( qry->callBackEnd ) {
		g_timeout_add(0, callbackend, qry);
	}

	return ADDRQUERY_RETVAL(qry);
}

/**
 * Remove results (folder and data) for specified LDAP query.
 * \param  qry Query object to process.
 * \return TRUE if folder deleted successfully.
 */
gboolean ldapquery_remove_results( LdapQuery *qry ) {
	gboolean retVal = FALSE;

	ldapqry_set_aged_flag( qry, TRUE );

	if( ldapqry_get_busy_flag( qry ) ) {
		ldapqry_set_stop_flag( qry, TRUE );
	}
	else {
		LdapServer *server = qry->server;
		server->listQuery = g_list_remove(server->listQuery, qry);

		retVal = TRUE;
	}
	return retVal;
}

void ldapqry_print(LdapQuery *qry, FILE *stream) {
	cm_return_if_fail( qry != NULL );

	ldapsvr_print_data(qry->server, stream);
	ldapctl_print(qry->control, stream);
	fprintf(stream, "entriesRead: %d\n", qry->entriesRead);
	fprintf(stream, "elapsedTime: %d\n", qry->elapsedTime);
	fprintf(stream, "stopFlag: %d\n", qry->stopFlag);
	fprintf(stream, "busyFlag: %d\n", qry->busyFlag);
	fprintf(stream, "agedFlag: %d\n", qry->agedFlag);
	fprintf(stream, "completed: %d\n", qry->completed);
	fprintf(stream, "startTime: %d\n", (int) qry->startTime);
	fprintf(stream, "touchTime: %d\n", (int) qry->touchTime);
	fprintf(stream, "data: %s\n", qry->data?(gchar *)qry->data:"null");
}

#endif	/* USE_LDAP */

/*
 * End of Source.
 */


