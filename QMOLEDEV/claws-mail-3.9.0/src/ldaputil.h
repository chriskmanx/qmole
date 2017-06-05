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

#ifndef __LDAPUTIL_H__
#define __LDAPUTIL_H__

#ifdef USE_LDAP

#include <glib.h>
#ifdef G_OS_UNIX
#include <ldap.h>
#include <lber.h>
#else
#include <windows.h>
#include <winldap.h>
#define LDAP_CONST const
#define ldap_unbind_ext(ld,x,y) ldap_unbind_s(ld)
#define LDAP_ADMINLIMIT_EXCEEDED LDAP_ADMIN_LIMIT_EXCEEDED
#define timeval l_timeval
#endif
/* Function Prototypes */
GList *ldaputil_read_basedn	( const gchar *host, const gint port,
				  const gchar *bindDN, const gchar *bindPW,
				  const gint tov, int ssl, int tls );
gboolean ldaputil_test_connect	( const gchar *host, const gint port, int ssl, int tls, int secs);
gboolean ldaputil_test_ldap_lib	( void );
int claws_ldap_simple_bind_s( LDAP *ld, LDAP_CONST char *dn, LDAP_CONST char *passwd );
const gchar *ldaputil_get_error(LDAP *ld);

#endif	/* USE_LDAP */

#endif /* __LDAPUTIL_H__ */

/*
 * End of Source.
 */
