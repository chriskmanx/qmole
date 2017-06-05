/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2011 Match Grun and the Claws Mail team
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef USE_LDAP

#include <glib.h>
#include <sys/time.h>
#include <string.h>

#include "ldapctrl.h"
#include "mgutils.h"
#include "passcrypt.h"
#include "editaddress_other_attributes_ldap.h"
#include "common/utils.h"
#include "common/quoted-printable.h"

/**
 * Create new LDAP control block object.
 * \return Initialized control object.
 */
LdapControl *ldapctl_create( void ) {
	LdapControl *ctl;

	ctl = g_new0( LdapControl, 1 );
	ctl->hostName = NULL;
	ctl->port = LDAPCTL_DFL_PORT;
	ctl->baseDN = NULL;
	ctl->bindDN = NULL;
	ctl->bindPass = NULL;
	ctl->listCriteria = NULL;
	ctl->attribEMail = g_strdup( LDAPCTL_ATTR_EMAIL );
	ctl->attribCName = g_strdup( LDAPCTL_ATTR_COMMONNAME );
	ctl->attribFName = g_strdup( LDAPCTL_ATTR_GIVENNAME );
	ctl->attribLName = g_strdup( LDAPCTL_ATTR_SURNAME );
	ctl->attribDName = g_strdup( LDAPCTL_ATTR_DISPLAYNAME );
	ctl->maxEntries = LDAPCTL_MAX_ENTRIES;
	ctl->timeOut = LDAPCTL_DFL_TIMEOUT;
	ctl->maxQueryAge = LDAPCTL_DFL_QUERY_AGE;
	ctl->matchingOption = LDAPCTL_MATCH_BEGINWITH;
	ctl->version = 0;
	ctl->enableTLS = FALSE;
	ctl->enableSSL = FALSE;

	/* Mutex to protect control block */
	ctl->mutexCtl = g_malloc0( sizeof( pthread_mutex_t ) );
	pthread_mutex_init( ctl->mutexCtl, NULL );

	return ctl;
}

/**
 * Specify hostname to be used.
 * \param ctl   Control object to process.
 * \param value Host name.
 */
void ldapctl_set_host( LdapControl* ctl, const gchar *value ) {
	ctl->hostName = mgu_replace_string( ctl->hostName, value );

	if ( ctl->hostName == NULL )
		return;

	g_strstrip( ctl->hostName );
	debug_print("setting hostname: %s\n", ctl->hostName);
}

/**
 * Specify port to be used.
 * \param ctl  Control object to process.
 * \param value Port.
 */
void ldapctl_set_port( LdapControl* ctl, const gint value ) {
	if( value > 0 ) {
		ctl->port = value;
	}
	else {
		ctl->port = LDAPCTL_DFL_PORT;
	}
	debug_print("setting port: %d\n", ctl->port);
}

/**
 * Specify base DN to be used.
 * \param ctl  Control object to process.
 * \param value Base DN.
 */
void ldapctl_set_base_dn( LdapControl* ctl, const gchar *value ) {
	ctl->baseDN = mgu_replace_string( ctl->baseDN, value );

	if ( ctl->baseDN == NULL )
		return;

	g_strstrip( ctl->baseDN );
	debug_print("setting baseDN: %s\n", ctl->baseDN);
}

/**
 * Specify bind DN to be used.
 * \param ctl  Control object to process.
 * \param value Bind DN.
 */
void ldapctl_set_bind_dn( LdapControl* ctl, const gchar *value ) {
	ctl->bindDN = mgu_replace_string( ctl->bindDN, value );

	if ( ctl->bindDN == NULL )
		return;

	g_strstrip( ctl->bindDN );
	debug_print("setting bindDN: %s\n", ctl->bindDN);
}

/**
 * Specify bind password to be used.
 * \param ctl  Control object to process.
 * \param value Password.
 * \param encrypt Encrypt password
 * \param change Save encrypted
 */
void ldapctl_set_bind_password( 
	LdapControl* ctl, const gchar *value, gboolean encrypt, gboolean change ) {
	gchar *buf, *tmp;

	ctl->bindPass = mgu_replace_string( ctl->bindPass, value );

	if ( ctl->bindPass == NULL )
		return;

	g_strstrip( ctl->bindPass );
	
	buf = tmp = NULL;
	if ( encrypt ) {
		/* If first char is not ! the password is not encrypted */
		if (ctl->bindPass[0] == '!' || change) {
			if (ctl->bindPass[0] != '!' && change)
				buf = mgu_replace_string( buf, ctl->bindPass );
			else {
				if (ctl->bindPass[1] != '|')
					buf = mgu_replace_string( buf, ctl->bindPass + 1 );
				else {
					/* quoted printable decode */
					buf = mgu_replace_string( buf, ctl->bindPass + 2 );
					qp_decode_line(buf);
				}
			}
			
			passcrypt_encrypt( buf, strlen(buf) );
			if (ctl->bindPass[0] != '!' && change) {
				/* quoted printable encode */
				tmp = g_malloc0(qp_get_q_encoding_len(buf) + 1);
				qp_q_encode(tmp, buf);
				g_free(buf);
				buf = g_strconcat( "!|", tmp, NULL );
				g_free(tmp);
			}

			ctl->bindPass = mgu_replace_string( ctl->bindPass, buf );
			g_free(buf);
			
		}
	}
	debug_print("setting bindPassword\n");
}

/**
 * Fetch bind password to be used.
 * \param ctl  Control object to process.
 * \return Decrypted password.
 */
gchar* ldapctl_get_bind_password( LdapControl* ctl ) {
	gchar *pwd = NULL, *buf;

	if ( ctl->bindPass != NULL ) {
		pwd = mgu_replace_string( pwd, ctl->bindPass );
		/* If first char is not ! the password is not encrypted */
		if (pwd && pwd[0] == '!') {
			if (pwd[1] && pwd[1] == '|') {
				buf = g_strdup(pwd + 2);
				/* quoted printable decode */
				qp_decode_line(buf);
			}
			else {
				buf = g_strdup(pwd + 1);
			}
			g_free(pwd);
			
			passcrypt_decrypt( buf, strlen(buf) );

			pwd = g_strdup(buf);
			g_free(buf);
		}
	}

	debug_print("getting bindPassword\n");

	return pwd;
}

/**
 * Specify maximum number of entries to retrieve.
 * \param ctl  Control object to process.
 * \param value Maximum entries.
 */
void ldapctl_set_max_entries( LdapControl* ctl, const gint value ) {
	if( value > 0 ) {
		ctl->maxEntries = value;
	}
	else {
		ctl->maxEntries = LDAPCTL_MAX_ENTRIES;
	}
	debug_print("setting maxEntries: %d\n", ctl->maxEntries);
}

/**
 * Specify timeout value for LDAP operation (in seconds).
 * \param ctl  Control object to process.
 * \param value Timeout.
 */
void ldapctl_set_timeout( LdapControl* ctl, const gint value ) {
	if( value > 0 ) {
		ctl->timeOut = value;
	}
	else {
		ctl->timeOut = LDAPCTL_DFL_TIMEOUT;
	}
	debug_print("setting timeOut: %d\n", ctl->timeOut);
}

/**
 * Specify maximum age of query (in seconds) before query is retired.
 * \param ctl  Control object to process.
 * \param value Maximum age.
 */
void ldapctl_set_max_query_age( LdapControl* ctl, const gint value ) {
	if( value > LDAPCTL_MAX_QUERY_AGE ) {
		ctl->maxQueryAge = LDAPCTL_MAX_QUERY_AGE;
	}
	else if( value < 1 ) {
		ctl->maxQueryAge = LDAPCTL_DFL_QUERY_AGE;
	}
	else {
		ctl->maxQueryAge = value;
	}
	debug_print("setting maxAge: %d\n", ctl->maxQueryAge);
}

/**
 * Specify matching option to be used for searches.
 * \param ctl   Control object to process.
 * \param value Matching option, as follows:
 * <ul>
 * <li><code>LDAPCTL_MATCH_BEGINWITH</code> for "begins with" search</li>
 * <li><code>LDAPCTL_MATCH_CONTAINS</code> for "contains" search</li>
 * </ul>
 */
void ldapctl_set_matching_option( LdapControl* ctl, const gint value ) {
	if( value < LDAPCTL_MATCH_BEGINWITH ) {
		ctl->matchingOption = LDAPCTL_MATCH_BEGINWITH;
	}
	else if( value > LDAPCTL_MATCH_CONTAINS ) {
		ctl->matchingOption = LDAPCTL_MATCH_BEGINWITH;
	}
	else {
		ctl->matchingOption = value;
	}
	debug_print("setting matchingOption: %d\n", ctl->matchingOption);
}

/**
 * Specify TLS option.
 * \param ctl   Control object to process.
 * \param value <i>TRUE</i> to enable TLS.
 */
void ldapctl_set_tls( LdapControl* ctl, const gboolean value ) {
#ifdef USE_LDAP_TLS
	ctl->enableTLS = value;
	debug_print("setting TLS: %d\n", ctl->enableTLS);
#endif
}

void ldapctl_set_ssl( LdapControl* ctl, const gboolean value ) {
#ifdef USE_LDAP_TLS
	ctl->enableSSL = value;
	debug_print("setting SSL: %d\n", ctl->enableSSL);
#endif
}

/**
 * Return search criteria list.
 * \param  ctl  Control data object.
 * \return Linked list of character strings containing LDAP attribute names to
 *         use for a search. This should not be modified directly. Use the
 *         <code>ldapctl_set_criteria_list()</code>,
 *         <code>ldapctl_criteria_list_clear()</code> and
 *         <code>ldapctl_criteria_list_add()</code> functions for this purpose.
 */
GList *ldapctl_get_criteria_list( const LdapControl* ctl ) {
	cm_return_val_if_fail( ctl != NULL, NULL );
	return ctl->listCriteria;
}

/**
 * Clear list of LDAP search attributes.
 * \param  ctl  Control data object.
 */
void ldapctl_criteria_list_clear( LdapControl *ctl ) {
	cm_return_if_fail( ctl != NULL );
	mgu_free_dlist( ctl->listCriteria );
	ctl->listCriteria = NULL;
}

/**
 * Add LDAP attribute to criteria list.
 * \param ctl  Control object to process.
 * \param attr Attribute name to append. If not NULL and unique, a copy will
 *             be appended to the list.
 */
void ldapctl_criteria_list_add( LdapControl *ctl, gchar *attr ) {
	cm_return_if_fail( ctl != NULL );
	if( attr != NULL ) {
		if( mgu_list_test_unq_nc( ctl->listCriteria, attr ) ) {
			debug_print("adding to criteria list: %s\n", attr);
			ctl->listCriteria = g_list_append(
				ctl->listCriteria, g_strdup( attr ) );
		}
	}
}

/**
 * Clear LDAP server member variables.
 * \param ctl Control object to clear.
 */
static void ldapctl_clear( LdapControl *ctl ) {
	cm_return_if_fail( ctl != NULL );

	debug_print("clearing ldap controller members\n");
	/* Free internal stuff */
	g_free( ctl->hostName );
	g_free( ctl->baseDN );
	g_free( ctl->bindDN );
	g_free( ctl->bindPass );
	g_free( ctl->attribEMail );
	g_free( ctl->attribCName );
	g_free( ctl->attribFName );
	g_free( ctl->attribLName );
	g_free( ctl->attribDName );

	ldapctl_criteria_list_clear( ctl );

	/* Clear pointers */
	ctl->hostName = NULL;
	ctl->port = 0;
	ctl->baseDN = NULL;
	ctl->bindDN = NULL;
	ctl->bindPass = NULL;
	ctl->attribEMail = NULL;
	ctl->attribCName = NULL;
	ctl->attribFName = NULL;
	ctl->attribLName = NULL;
	ctl->attribDName = NULL;
	ctl->maxEntries = 0;
	ctl->timeOut = 0;
	ctl->maxQueryAge = 0;
	ctl->matchingOption = LDAPCTL_MATCH_BEGINWITH;
	ctl->version = 0;
	ctl->enableTLS = FALSE;
	ctl->enableSSL = FALSE;
}

/**
 * Free up LDAP server interface object by releasing internal memory.
 * \param ctl Control object to free.
 */
void ldapctl_free( LdapControl *ctl ) {
	cm_return_if_fail( ctl != NULL );

	debug_print("releasing requested memory for ldap controller\n");
	/* Free internal stuff */
	ldapctl_clear( ctl );

	/* Free the mutex */
	pthread_mutex_destroy( ctl->mutexCtl );
	g_free( ctl->mutexCtl );
	ctl->mutexCtl = NULL;

	/* Now release LDAP control object */
	g_free( ctl );
}

/**
 * Display object to specified stream.
 * \param ctl    Control object to process.
 * \param stream Output stream.
 */
void ldapctl_print( const LdapControl *ctl, FILE *stream ) {
	cm_return_if_fail( ctl != NULL );
	gchar *pwd;

	pthread_mutex_lock( ctl->mutexCtl );
	fprintf( stream, "LdapControl:\n" );
	fprintf( stream, "host name: '%s'\n", ctl->hostName?ctl->hostName:"null" );
	fprintf( stream, "     port: %d\n",   ctl->port );
	fprintf( stream, "  base dn: '%s'\n", ctl->baseDN?ctl->baseDN:"null" );
	fprintf( stream, "  bind dn: '%s'\n", ctl->bindDN?ctl->bindDN:"null" );
	pwd = ldapctl_get_bind_password((LdapControl *) ctl);
	fprintf( stream, "bind pass: '%s'\n", pwd?pwd:"null" );
	g_free(pwd);
	fprintf( stream, "attr mail: '%s'\n", ctl->attribEMail?ctl->attribEMail:"null" );
	fprintf( stream, "attr comn: '%s'\n", ctl->attribCName?ctl->attribCName:"null" );
	fprintf( stream, "attr frst: '%s'\n", ctl->attribFName?ctl->attribFName:"null" );
	fprintf( stream, "attr last: '%s'\n", ctl->attribLName?ctl->attribLName:"null" );
	fprintf( stream, "attr disn: '%s'\n", ctl->attribDName?ctl->attribDName:"null" );
	fprintf( stream, "max entry: %d\n",   ctl->maxEntries );
	fprintf( stream, "  timeout: %d\n",   ctl->timeOut );
	fprintf( stream, "  max age: %d\n",   ctl->maxQueryAge );
	fprintf( stream, "match opt: %d\n",   ctl->matchingOption );
	fprintf( stream, "  version: %d\n",   ctl->version );
	fprintf( stream, "      TLS: %s\n",   ctl->enableTLS ? "yes" : "no" );
	fprintf( stream, "      SSL: %s\n",   ctl->enableSSL ? "yes" : "no" );
	fprintf( stream, "crit list:\n" );
	if( ctl->listCriteria ) {
		mgu_print_dlist( ctl->listCriteria, stream );
	}
	else {
		fprintf( stream, "\t!!!none!!!\n" );
	}
	pthread_mutex_unlock( ctl->mutexCtl );
}

/**
 * Copy member variables to specified object. Mutex lock object is
 * not copied.
 * \param ctlFrom Object to copy from.
 * \param ctlTo   Destination object.
 */
void ldapctl_copy( const LdapControl *ctlFrom, LdapControl *ctlTo ) {
	GList *node;

	cm_return_if_fail( ctlFrom != NULL );
	cm_return_if_fail( ctlTo != NULL );

	debug_print("ldap controller copy\n");
	/* Lock both objects */
	pthread_mutex_lock( ctlFrom->mutexCtl );
	pthread_mutex_lock( ctlTo->mutexCtl );

	/* Clear our destination */
	ldapctl_clear( ctlTo );

	/* Copy strings */
	ctlTo->hostName = g_strdup( ctlFrom->hostName );
	ctlTo->baseDN = g_strdup( ctlFrom->baseDN );
	ctlTo->bindDN = g_strdup( ctlFrom->bindDN );
	ctlTo->bindPass = g_strdup( ctlFrom->bindPass );
	ctlTo->attribEMail = g_strdup( ctlFrom->attribEMail );
	ctlTo->attribCName = g_strdup( ctlFrom->attribCName );
	ctlTo->attribFName = g_strdup( ctlFrom->attribFName );
	ctlTo->attribLName = g_strdup( ctlFrom->attribLName );
	ctlTo->attribDName = g_strdup( ctlFrom->attribDName );

	/* Copy search criteria */
	node = ctlFrom->listCriteria;
	while( node ) {
		ctlTo->listCriteria = g_list_append(
			ctlTo->listCriteria, g_strdup( node->data ) );
		node = g_list_next( node );
	}

	/* Copy other members */
	ctlTo->port = ctlFrom->port;
	ctlTo->maxEntries = ctlFrom->maxEntries;
	ctlTo->timeOut = ctlFrom->timeOut;
	ctlTo->maxQueryAge = ctlFrom->maxQueryAge;
	ctlTo->matchingOption = ctlFrom->matchingOption;
	ctlTo->version = ctlFrom->version;
	ctlTo->enableTLS = ctlFrom->enableTLS;
	ctlTo->enableSSL = ctlFrom->enableSSL;

	/* Unlock */
	pthread_mutex_unlock( ctlTo->mutexCtl );
	pthread_mutex_unlock( ctlFrom->mutexCtl );
}

/**
 * Search criteria fragment - two terms - begin with (default).
 */
static gchar *_criteria2BeginWith = "(&(givenName=%s*)(sn=%s*))";

/**
 * Search criteria fragment - two terms - contains.
 */
static gchar *_criteria2Contains  = "(&(givenName=*%s*)(sn=*%s*))";

/**
 * Create an LDAP search criteria by parsing specified search term. The search
 * term may contain two names separated by the first embedded space found in
 * the search term. It is assumed that the two tokens are first name and last
 * name, or vice versa. An appropriate search criteria will be constructed.
 *
 * \param  searchTerm   Reference to search term to process.
 * \param  matchOption  Set to the following:
 * <ul>
 * <li><code>LDAPCTL_MATCH_BEGINWITH</code> for "begins with" search</li>
 * <li><code>LDAPCTL_MATCH_CONTAINS</code> for "contains" search</li>
 * </ul>
 *
 * \return Formatted search criteria, or <code>NULL</code> if there is no
 *         embedded spaces. The search term should be g_free() when no
 *         longer required.
 */
static gchar *ldapctl_build_ldap_criteria(
		const gchar *searchTerm, const gint matchOption )
{
	gchar *p;
	gchar *t1;
	gchar *t2 = NULL;
	gchar *term;
	gchar *crit = NULL;
	gchar *criteriaFmt;

	if( matchOption == LDAPCTL_MATCH_CONTAINS ) {
		criteriaFmt = _criteria2Contains;
	}
	else {
		criteriaFmt = _criteria2BeginWith;
	}

	term = g_strdup( searchTerm );
	g_strstrip( term );

	/* Find first space character */	
	t1 = p = term;
	while( *p ) {
		if( *p == ' ' ) {
			*p = '\0';
			t2 = g_strdup( 1 + p );
			break;
		}
		p++;
	}

	if( t2 ) {
		/* Format search criteria */
		gchar *p1, *p2;

		g_strstrip( t2 );
		p1 = g_strdup_printf( criteriaFmt, t1, t2 );
		p2 = g_strdup_printf( criteriaFmt, t2, t1 );
		crit = g_strdup_printf( "(&(|%s%s)(mail=*))", p1, p2 );

		g_free( t2 );
		g_free( p1 );
		g_free( p2 );
	}
	g_free( term );
	debug_print("search criteria: %s\n", crit?crit:"null");
	return crit;
}


/**
 * Search criteria fragment - single term - begin with (default).
 */
static gchar *_criteriaBeginWith = "(%s=%s*)";

/**
 * Search criteria fragment - single term - contains.
 */
static gchar *_criteriaContains  = "(%s=*%s*)";

/**
 * Build a formatted LDAP search criteria string from criteria list.
 * \param ctl  Control object to process.
 * \param searchVal Value to search for.
 * \return Formatted string. Should be g_free() when done.
 */
gchar *ldapctl_format_criteria( LdapControl *ctl, const gchar *searchVal ) {
	GList *node;
	gchar *p1, *p2, *retVal;
	gchar *criteriaFmt;

	cm_return_val_if_fail( ctl != NULL, NULL );
	cm_return_val_if_fail( searchVal != NULL, NULL );

	/* Test whether there are more that one search terms */
	retVal = ldapctl_build_ldap_criteria( searchVal, ctl->matchingOption );
	if( retVal ) return retVal;

	if( ctl->matchingOption ==  LDAPCTL_MATCH_CONTAINS ) {
		criteriaFmt = _criteriaContains;
	}
	else {
		criteriaFmt = _criteriaBeginWith;
	}

	/* No - just a simple search */
	/* p1 contains previous formatted criteria */
	/* p2 contains next formatted criteria */
	retVal = p1 = p2 = NULL;
	node = ctl->listCriteria;
	while( node ) {
		gchar *attr, *tmp;
		attr = node->data;
		node = g_list_next( node );

		/* Switch pointers */
		tmp = p1; p1 = p2; p2 = tmp;

		if( p1 ) {
			/* Subsequent time through */
			gchar *crit;

			/* Format query criteria */
			crit = g_strdup_printf( criteriaFmt, attr, searchVal );

			/* Append to existing criteria */			
			g_free( p2 );
			p2 = g_strdup_printf( "(|%s%s)", p1, crit );

			g_free( crit );
		}
		else {
			/* First time through - Format query criteria */
			p2 = g_strdup_printf( criteriaFmt, attr, searchVal );
		}
	}

	if( p2 == NULL ) {
		/* Nothing processed - format a default attribute */
		retVal = g_strdup_printf( "(%s=*)", LDAPCTL_ATTR_EMAIL );
	}
	else {
		/* We have something - free up previous result */
		retVal = p2;
		g_free( p1 );
	}
	debug_print("current search string: %s\n", retVal);
	return retVal;
}

/**
 * Return array of pointers to attributes for LDAP query.
 * \param  ctl  Control object to process.
 * \return NULL terminated list.
 */
char **ldapctl_attribute_array( LdapControl *ctl ) {
	char **ptrArray;
	GList *node;
	gint cnt, i;
	cm_return_val_if_fail( ctl != NULL, NULL );

	node = ctl->listCriteria;
	cnt = g_list_length( ctl->listCriteria );
	ptrArray = g_new0( char *, 1 + cnt );
	i = 0;
	while( node ) {
		ptrArray[ i++ ] = node->data;
		/*debug_print("adding search attribute: %s\n", (gchar *) node->data);*/
		node = g_list_next( node );
	}
	ptrArray[ i ] = NULL;
	return ptrArray;
}

/**
 * Return array of pointers to attributes for LDAP query.
 * \param  ctl  Control object to process.
 * \return NULL terminated list.
 */
char **ldapctl_full_attribute_array( LdapControl *ctl ) {
	char **ptrArray;
	GList *node, *def;
	GList *tmp = NULL;
	gint cnt, i;
	cm_return_val_if_fail( ctl != NULL, NULL );

	def = ctl->listCriteria;
	while (def) {
		tmp = g_list_append(tmp, g_strdup(def->data));
		def = def->next;
	}

	def = ldapctl_get_default_criteria_list();
	
	while (def) {
		if( g_list_find_custom(tmp, (gpointer)def->data, 
				(GCompareFunc)strcmp2) == NULL) {
			tmp = g_list_append(tmp, g_strdup(def->data));
		}
		def = def->next;
	}

	node = tmp;
	cnt = g_list_length( tmp );
	ptrArray = g_new0( char *, 1 + cnt);
	i = 0;
	while( node ) {
		ptrArray[ i++ ] = node->data;
		/*debug_print("adding search attribute: %s\n", (gchar *) node->data);*/
		node = g_list_next( node );
	}
	ptrArray[ i ] = NULL;
	return ptrArray;
}

/**
 * Free array of pointers allocated by ldapctl_criteria_array().
 * param ptrArray Array to clear.
 */
void ldapctl_free_attribute_array( char **ptrArray ) {
	gint i;

	/* Clear array to NULL's */
	for( i = 0; ptrArray[i] != NULL; i++ ) {
		ptrArray[i] = NULL;
	}
	g_free( ptrArray );
}	

/**
 * Parse LDAP search string, building list of LDAP criteria attributes. This
 * may be used to convert an old style Sylpheed LDAP search criteria to the
 * new format. The old style uses a standard LDAP search string, for example:
 * <pre>
 *    (&(mail=*)(cn=%s*))
 * </pre>
 * This function extracts the two LDAP attributes <code>mail</code> and
 * <code>cn</code>, adding each to a list.
 *
 * \param ctl Control object to process.
 * \param criteria LDAP search criteria string.
 */
void ldapctl_parse_ldap_search( LdapControl *ctl, gchar *criteria ) {
	gchar *ptr;
	gchar *pFrom;
	gchar *attrib;
	gint iLen;

	cm_return_if_fail( ctl != NULL );

	ldapctl_criteria_list_clear( ctl );
  	if( criteria == NULL ) return;

	pFrom = NULL;
	ptr = criteria;
	while( *ptr ) {
		if( *ptr == '(' ) {
			pFrom = 1 + ptr;
		}
		if( *ptr == '=' ) {
			if( pFrom ) {
				iLen = ptr - pFrom;
				attrib = g_strndup( pFrom, iLen );
				g_strstrip( attrib );
				ldapctl_criteria_list_add( ctl, attrib );
				g_free( attrib );
			}
			pFrom = NULL;
		}
		ptr++;
	}
}

/**
 * Return the default LDAP search criteria string.
 * \return Formatted string or <i>""</i>. Should be g_free() when done.
 */
gchar *ldapctl_get_default_criteria() {
	gchar *retVal = g_strdup(LDAPCTL_DFL_ATTR_LIST);
	const gchar **attrs = ATTRIBUTE; 

	while (*attrs) {
		gchar *tmp = g_strdup_printf("%s, %s", retVal, *attrs++);
		g_free(retVal);
		retVal = tmp;
	}
	debug_print("default search criteria: %s\n", retVal);
	return retVal;
}

/**
 * Return the default LDAP search criteria list.
 * \return GList or <i>NULL</i>.
 */
GList *ldapctl_get_default_criteria_list() {
	gchar *criteria, *item;
	gchar **c_list, **w_list;
	GList *attr_list = NULL;

	criteria = ldapctl_get_default_criteria();
	c_list = g_strsplit(criteria, " ", 0);
	g_free(criteria);
	criteria = NULL;
	w_list = c_list;
	while ((criteria = *w_list++) != 0) {
		/* copy string elimination <,> */
		if (*w_list)
			item = g_strndup(criteria, strlen(criteria) - 1);
		else
			item = g_strdup(criteria);
		debug_print("adding attribute to list: %s\n", item);
		attr_list = g_list_append(attr_list, g_strdup(item));
		g_free(item);
	}
	g_strfreev(c_list);
	return attr_list;
}

/**
 * Compare to GList for equality.
 * \param l1 First GList
 * \param l2 Second GList
 * \Return TRUE or FALSE
 */
gboolean ldapctl_compare_list(GList *l1, GList *l2) {
	gchar *first, *second;
	if (! l1 && ! l2)
		return TRUE;
	if ((! l1 && l2) || (l1 && ! l2))
		return FALSE;
	while (l1 && l2) {
		first = (gchar *) l1->data;
		second = (gchar *) l2->data;
		/*debug_print("comparing: %s = %s\n", first, second);*/
		if ( ! (first && second) || strcmp(first, second) != 0) {
			return FALSE;
		}
		l1 = g_list_next(l1);
		l2 = g_list_next(l2);
	}
	return TRUE;
}

#endif	/* USE_LDAP */

/*
 * End of Source.
 */

