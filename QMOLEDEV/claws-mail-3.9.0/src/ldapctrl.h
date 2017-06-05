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
 * Functions for LDAP control data.
 */

#ifndef __LDAPCTRL_H__
#define __LDAPCTRL_H__

#ifdef USE_LDAP

#include <glib.h>
#include <stdio.h>
#include <pthread.h>

/*
 * Constants.
 */
#define LDAPCTL_DFL_PORT        389
#define LDAPCTL_DFL_SSL_PORT        636
#define LDAPCTL_MAX_ENTRIES     30
#define LDAPCTL_DFL_TIMEOUT     30
#define LDAPCTL_DFL_QUERY_AGE   600
#define LDAPCTL_MAX_QUERY_AGE   86400

#define LDAPCTL_ATTR_EMAIL      "mail"
#define LDAPCTL_ATTR_COMMONNAME "cn"
#define LDAPCTL_ATTR_GIVENNAME  "givenName"
#define LDAPCTL_ATTR_SURNAME    "sn"
#define LDAPCTL_ATTR_PHONE     "telephoneNumber"
#define LDAPCTL_ATTR_DISPLAYNAME "displayName"

#define LDAPCTL_DFL_ATTR_LIST   "mail, cn, givenName, sn, displayName"

/*
 * Search matching options.
 */
#define LDAPCTL_MATCH_BEGINWITH 0
#define LDAPCTL_MATCH_CONTAINS  1

/*
 * Data structures.
 */
typedef struct _LdapControl LdapControl;
struct _LdapControl {
	gchar     *hostName;
	gint      port;
	gchar     *baseDN;
	gchar     *bindDN;
	gchar     *bindPass;
	gint      maxEntries;
	gint      timeOut;
	gint      maxQueryAge;
	gint      matchingOption;
	gint      version;
	gboolean  enableTLS;
	gboolean  enableSSL;
	gchar     *attribEMail;
	gchar     *attribCName;
	gchar     *attribFName;
	gchar     *attribLName;
	gchar	  *attribDName;
	GList     *listCriteria;
	pthread_mutex_t *mutexCtl;
};

/* Function prototypes */
LdapControl *ldapctl_create	( void );
void ldapctl_set_host		( LdapControl* ctl, const gchar *value );
void ldapctl_set_port		( LdapControl* ctl, const gint value );
void ldapctl_set_base_dn	( LdapControl* ctl, const gchar *value );
void ldapctl_set_bind_dn	( LdapControl* ctl, const gchar *value );
void ldapctl_set_bind_password	
	( LdapControl* ctl, const gchar *value, gboolean encrypt, gboolean change );
gchar* ldapctl_get_bind_password ( LdapControl* ctl );
void ldapctl_set_max_entries	( LdapControl* ctl, const gint value );
void ldapctl_set_timeout	( LdapControl* ctl, const gint value );
void ldapctl_set_max_query_age	( LdapControl* ctl, const gint value );
void ldapctl_set_matching_option( LdapControl* ctl, const gint value );
void ldapctl_set_tls		( LdapControl* ctl, const gboolean value );
void ldapctl_set_ssl		( LdapControl* ctl, const gboolean value );
GList *ldapctl_get_criteria_list( const LdapControl* ctl );
void ldapctl_criteria_list_clear( LdapControl *ctl );
void ldapctl_criteria_list_add	( LdapControl *ctl, gchar *attr );
void ldapctl_free		( LdapControl *ctl );
void ldapctl_print		( const LdapControl *ctl, FILE *stream );
void ldapctl_copy		( const LdapControl *ctlFrom,
				  LdapControl *ctlTo );
gchar *ldapctl_format_criteria	( LdapControl *ctl, const gchar *searchVal );
char **ldapctl_attribute_array	( LdapControl *ctl );
char **ldapctl_full_attribute_array( LdapControl *ctl );
void ldapctl_free_attribute_array( char **ptrArray );
void ldapctl_parse_ldap_search	( LdapControl *ctl, gchar *criteria );
gchar *ldapctl_get_default_criteria(void);
GList *ldapctl_get_default_criteria_list();
gboolean ldapctl_compare_list(GList *l1, GList *l2);

#endif	/* USE_LDAP */

#endif /* __LDAPCTRL_H__ */

