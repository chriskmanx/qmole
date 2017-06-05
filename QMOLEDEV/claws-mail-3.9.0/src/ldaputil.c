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
 * Some utility functions to access LDAP servers.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_LDAP

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>
#include <sys/time.h>
#include <errno.h>
#include "common/utils.h"
#include "ldaputil.h"
#include "ldapserver.h"
#include "ldapctrl.h"

#define SYLDAP_TEST_FILTER   "(objectclass=*)"
#define SYLDAP_SEARCHBASE_V2 "cn=config"
#define SYLDAP_SEARCHBASE_V3 ""
#define SYLDAP_V2_TEST_ATTR  "database"
#define SYLDAP_V3_TEST_ATTR  "namingcontexts"

/**
 * Attempt to discover the base DN for a server using LDAP version 3.
 * \param  ld  LDAP handle for a connected server.
 * \param  tov Timeout value (seconds), or 0 for none, default 30 secs.
 * \return List of Base DN's, or NULL if could not read. List should be
 *         g_free() when done.
 */
static GList *ldaputil_test_v3( LDAP *ld, gint tov, gint *errcode ) {
	GList *baseDN = NULL;
	gint rc, i;
	LDAPMessage *result = NULL, *e;
	gchar *attribs[2];
	BerElement *ber;
	gchar *attribute;
	struct berval **vals;
	struct timeval timeout;

	/* Set timeout */
	timeout.tv_usec = 0L;
	if( tov > 0 ) {
		timeout.tv_sec = tov;
	}
	else {
		timeout.tv_sec = 30L;
	}

	/* Test for LDAP version 3 */
	attribs[0] = SYLDAP_V3_TEST_ATTR;
	attribs[1] = NULL;
	rc = ldap_search_ext_s(
		ld, SYLDAP_SEARCHBASE_V3, LDAP_SCOPE_BASE, SYLDAP_TEST_FILTER,
		attribs, 0, NULL, NULL, &timeout, 0, &result );

	if( rc == LDAP_SUCCESS ) {
		/* Process entries */
		for( e = ldap_first_entry( ld, result );
		     e != NULL;
		     e = ldap_next_entry( ld, e ) ) 
		{
			/* Process attributes */
			for( attribute = ldap_first_attribute( ld, e, &ber );
			     attribute != NULL;
			     attribute = ldap_next_attribute( ld, e, ber ) )
			{
				if( strcasecmp(
					attribute, SYLDAP_V3_TEST_ATTR ) == 0 )
				{
					vals = ldap_get_values_len( ld, e, attribute );
					if( vals != NULL ) {
						for( i = 0; vals[i] != NULL; i++ ) {
							baseDN = g_list_append(
								baseDN, g_strndup( vals[i]->bv_val, vals[i]->bv_len ) );
						}
					}
					ldap_value_free_len( vals );
				}
				ldap_memfree( attribute );
			}
			if( ber != NULL ) {
				ber_free( ber, 0 );
			}
			ber = NULL;
		}
	} else
		debug_print("LDAP: Error %d (%s)\n", rc, ldaputil_get_error(ld));
	
	if (errcode)
		*errcode = rc;
	if (result)
		ldap_msgfree( result );
	return baseDN;
}

/**
 * Attempt to discover the base DN for a server using LDAP version 2.
 * \param  ld  LDAP handle for a connected server.
 * \param  tov Timeout value (seconds), or 0 for none, default 30 secs.
 * \return List of Base DN's, or NULL if could not read. List should be
 *         g_free() when done.
 */
static GList *ldaputil_test_v2( LDAP *ld, gint tov ) {
	GList *baseDN = NULL;
	gint rc, i;
	LDAPMessage *result = NULL, *e;
	gchar *attribs[1];
	BerElement *ber;
	gchar *attribute;
	struct berval **vals;
	struct timeval timeout;

	/* Set timeout */
	timeout.tv_usec = 0L;
	if( tov > 0 ) {
		timeout.tv_sec = tov;
	}
	else {
		timeout.tv_sec = 30L;
	}

	attribs[0] = NULL;
	rc = ldap_search_ext_s(
		ld, SYLDAP_SEARCHBASE_V2, LDAP_SCOPE_BASE, SYLDAP_TEST_FILTER,
		attribs, 0, NULL, NULL, &timeout, 0, &result );

	if( rc == LDAP_SUCCESS ) {
		/* Process entries */
		for( e = ldap_first_entry( ld, result );
		     e != NULL;
		     e = ldap_next_entry( ld, e ) )
		{
			/* Process attributes */
			for( attribute = ldap_first_attribute( ld, e, &ber );
			     attribute != NULL;
			     attribute = ldap_next_attribute( ld, e, ber ) )
			{
				if( strcasecmp(
					attribute,
					SYLDAP_V2_TEST_ATTR ) == 0 ) {
					vals = ldap_get_values_len( ld, e, attribute );
					if( vals != NULL ) {
						for( i = 0; vals[i] != NULL; i++ ) {
							char *ch, *tmp;
							/*
							 * Strip the 'ldb:' from the
							 * front of the value.
							 */
							tmp = g_strndup( vals[i]->bv_val, vals[i]->bv_len);
							ch = ( char * ) strchr( tmp, ':' );
							if( ch ) {
								gchar *bn = g_strdup( ++ch );
								g_strchomp( bn );
								g_strchug( bn );
								baseDN = g_list_append(
									baseDN, g_strdup( bn ) );
								g_free( bn );
							}
							g_free(tmp);
						}
					}
					ldap_value_free_len( vals );
				}
				ldap_memfree( attribute );
			}
			if( ber != NULL ) {
				ber_free( ber, 0 );
			}
			ber = NULL;
		}
	}
	if (result)
		ldap_msgfree( result );
	return baseDN;
}

int claws_ldap_simple_bind_s( LDAP *ld, LDAP_CONST char *dn, LDAP_CONST char *passwd )
{
	struct berval cred;

	if ( passwd != NULL ) {
		cred.bv_val = (char *) passwd;
		cred.bv_len = strlen( passwd );
	} else {
		cred.bv_val = "";
		cred.bv_len = 0;
	}

	debug_print("binding: DN->%s\n", dn?dn:"null");
#ifdef G_OS_UNIX
	return ldap_sasl_bind_s( ld, dn, LDAP_SASL_SIMPLE, &cred,
		NULL, NULL, NULL );
#else
	return ldap_simple_bind_s(ld, dn, passwd);
#endif
}

/**
 * Attempt to discover the base DN for the server.
 * \param  host   Host name.
 * \param  port   Port number.
 * \param  bindDN Bind DN (optional).
 * \param  bindPW Bind PW (optional).
 * \param  tov    Timeout value (seconds), or 0 for none, default 30 secs.
 * \return List of Base DN's, or NULL if could not read. This list should be
 *         g_free() when done.
 */
GList *ldaputil_read_basedn(
		const gchar *host, const gint port, const gchar *bindDN,
		const gchar *bindPW, const gint tov, int ssl, int tls )
{
	GList *baseDN = NULL;
	LDAP *ld = NULL;
	LdapControl *ctl = ldapctl_create();
	gint rc;

	if( host == NULL ) 
		return NULL;
	if( port < 1 ) 
		return NULL;

	ldapctl_set_tls(ctl, tls);
	ldapctl_set_ssl(ctl, ssl);
	ldapctl_set_port(ctl, port);
	ldapctl_set_host(ctl, host);
	ldapctl_set_timeout(ctl, tov);
	ldapctl_set_bind_dn(ctl, bindDN);
	ldapctl_set_bind_password(ctl, bindPW, FALSE, FALSE);

	ld = ldapsvr_connect(ctl);
	if (ld == NULL) {
		ldapctl_free(ctl);
		return NULL;
	}
	baseDN = ldaputil_test_v3( ld, tov, &rc );
	if (baseDN)
		debug_print("Using LDAP v3\n");

#ifdef G_OS_UNIX
	if( baseDN == NULL && !LDAP_API_ERROR(rc) ) {
#else
	if( baseDN == NULL) {
#endif
		baseDN = ldaputil_test_v2( ld, tov );
		if (baseDN)
			debug_print("Using LDAP v2\n");
	}
	if (ld)
		ldapsvr_disconnect(ld);

	ldapctl_free(ctl);
	
	return baseDN;
}

/**
 * Attempt to connect to the server.
 * Enter:
 * \param  host Host name.
 * \param  port Port number.
 * \return <i>TRUE</i> if connected successfully.
 */
gboolean ldaputil_test_connect( const gchar *host, const gint port, int ssl, int tls, int secs ) {
	gboolean retVal = FALSE;
	LdapControl *ctl = ldapctl_create();
	LDAP *ld;

	ldapctl_set_tls(ctl, tls);
	ldapctl_set_ssl(ctl, ssl);
	ldapctl_set_port(ctl, port);
	ldapctl_set_host(ctl, host);
	ldapctl_set_timeout(ctl, secs);

	ld = ldapsvr_connect(ctl);
	if( ld != NULL ) {
		ldapsvr_disconnect(ld);
		debug_print("ld != NULL\n");
		retVal = TRUE;
	}
	ldapctl_free(ctl);

	return retVal;
}

/**
 * Test whether LDAP libraries installed.
 * Return: TRUE if library available.
 */
gboolean ldaputil_test_ldap_lib( void ) {
	return TRUE;
}

const gchar *ldaputil_get_error(LDAP *ld)
{
	gchar *ld_error = NULL;
	static gchar error[512];

	ldap_get_option( ld, LDAP_OPT_ERROR_STRING, &ld_error);
	if (ld_error != NULL)
		strncpy2(error, ld_error, sizeof(error));
	else
		strncpy2(error, _("Unknown error"), sizeof(error));
	ldap_memfree(ld_error);

	return error;
}
#endif	/* USE_LDAP */

/*
 * End of Source.
*/
