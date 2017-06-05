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
 * Definitions necessary to access LDAP servers.
 */

#ifndef __LDAPSERVER_H__
#define __LDAPSERVER_H__

#ifdef USE_LDAP

#include <glib.h>
#ifdef G_OS_UNIX
#include <ldap.h>
#else
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <windows.h>
#include <winldap.h>
#include <winber.h>
#endif

#include "ldapctrl.h"
#include "addritem.h"
#include "addrcache.h"
#include "adbookbase.h"
#include "addrquery.h"

/* Error codes */
#define LDAPRC_SUCCESS    0
#define LDAPRC_CONNECT    -1
#define LDAPRC_INIT       -2
#define LDAPRC_BIND       -3
#define LDAPRC_SEARCH     -4
#define LDAPRC_TIMEOUT    -5
#define LDAPRC_CRITERIA   -6
#define LDAPRC_NOENTRIES  -7
#define LDAPRC_STOP_FLAG  -8
#define LDAPRC_TLS        -9
#define LDAPRC_NODN		  -10
#define LDAPRC_NAMING_VIOLATION  -11
#define LDAPRC_ALREADY_EXIST -12
#define LDAPRC_STRONG_AUTH -13

typedef struct _LdapServer LdapServer;
struct _LdapServer {
	AddressBookType type;
	AddressCache    *addressCache;
	gint            retVal;
	LdapControl     *control;
	gboolean        searchFlag;
	GList           *listQuery;
};

/* Function prototypes */
LdapServer *ldapsvr_create_noctl( void );
LdapServer *ldapsvr_create	( void );
void ldapsvr_set_name		( LdapServer *server, const gchar *value );
void ldapsvr_set_accessed	( LdapServer *server, const gboolean value );
void ldapsvr_force_refresh	( LdapServer *server );
void ldapsvr_free_all_query	( LdapServer *server );
void ldapsvr_free		( LdapServer *server );
gint ldapsvr_get_status		( LdapServer *server );
gboolean ldapsvr_get_accessed	( LdapServer *server );
gchar *ldapsvr_get_name		( LdapServer *server );
gboolean ldapsvr_get_modified	( LdapServer *server );
void ldapsvr_set_modified	( LdapServer *server, const gboolean value );
gboolean ldapsvr_get_read_flag	( LdapServer *server );
gboolean ldapsvr_get_search_flag( LdapServer *server );
void ldapsvr_set_search_flag	( LdapServer *server, const gboolean value );
gint ldapsvr_read_data ( LdapServer *server );

void ldapsvr_print_data		( LdapServer *server, FILE *stream );
void ldapsvr_cancel_read	( LdapServer *server );

ItemFolder *ldapsvr_get_root_folder	( LdapServer *server );
GList *ldapsvr_get_list_person	( LdapServer *server );
GList *ldapsvr_get_list_folder	( LdapServer *server );

void ldapsvr_stop_query_id	( LdapServer *server, const gint queryID );
void ldapsvr_stop_all_query	( LdapServer *server );
void ldapsvr_cancel_all_query	( LdapServer *server );
void ldapsvr_retire_query	( LdapServer *server );

gboolean ldapsvr_reuse_previous	( const LdapServer *server,
				  const QueryRequest *req );
void ldapsrv_set_options (gint secs, LDAP *ld);
LDAP *ldapsvr_connect(LdapControl *ctl);
void ldapsvr_disconnect(LDAP *ld);
#endif	/* USE_LDAP */

#endif /* __LDAPSERVER_H__ */

