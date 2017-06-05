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
 * Functions necessary to define an LDAP query.
 */

#ifndef __LDAPQUERY_H__
#define __LDAPQUERY_H__

#ifdef USE_LDAP

#include <glib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>

#ifdef G_OS_UNIX
#include <ldap.h>
#include <lber.h>
#else
#include <windows.h>
#include <winldap.h>
#endif
#include "addrquery.h"
#include "ldapctrl.h"
#include "addritem.h"
#include "addrcache.h"
#include "ldapserver.h"

typedef struct _LdapQuery LdapQuery;
struct _LdapQuery {
	AddrQueryObject obj;
	LdapControl     *control;
	LdapServer      *server;	/* Reference to (parent) LDAP server */
	gint            entriesRead;
	gint            elapsedTime;
	gboolean        stopFlag;
	gboolean        busyFlag;
	gboolean        agedFlag;
	gboolean        completed;
	time_t          startTime;
	time_t          touchTime;
	pthread_t       *thread;
	pthread_mutex_t *mutexStop;
	pthread_mutex_t *mutexBusy;
	pthread_mutex_t *mutexEntry;
	void            (*callBackEntry)( void *, gint, void *, void * );
	void            (*callBackEnd)( void *, gint, gint, void * );
	LDAP            *ldap;
	gpointer        data;
};

typedef struct _NameValuePair NameValuePair;
struct _NameValuePair {
	gchar *name;
	gchar *value;
};

/* Function prototypes */
void ldapqry_print(LdapQuery *qry, FILE *stream);
void ldapqry_initialize		( void );
LdapQuery *ldapqry_create	( void );
void ldapqry_set_control	( LdapQuery *qry, LdapControl *ctl );
void ldapqry_set_name		( LdapQuery* qry, const gchar *value );
void ldapqry_set_search_value	( LdapQuery *qry, const gchar *value );
void ldapqry_set_search_type	( LdapQuery *qry, const AddrSearchType value );
void ldapqry_set_query_id	( LdapQuery* qry, const gint value );
void ldapqry_set_callback_start	( LdapQuery *qry, void *func );
void ldapqry_set_callback_entry	( LdapQuery *qry, void *func );
void ldapqry_set_callback_end	( LdapQuery *qry, void *func );
void ldapqry_free		( LdapQuery *qry );
void ldapqry_set_stop_flag( LdapQuery *qry, const gboolean value );
gboolean ldapqry_check_search	( LdapQuery *qry );
void ldapqry_touch		( LdapQuery *qry );
gint ldapqry_read_data_th	( LdapQuery *qry );
void ldapqry_cancel		( LdapQuery *qry );
void ldapqry_age		( LdapQuery *qry, gint maxAge );
void ldapqry_delete_folder	( LdapQuery *qry );
gboolean ldapquery_remove_results( LdapQuery *qry );
void ldapqry_free_list_name_value( GList *list );
void ldapqry_free_name_value( NameValuePair *nvp );
#endif	/* USE_LDAP */

#endif /* __LDAPQUERY_H__ */
