/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 Michael Rasmussen and the Claws Mail team
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
 * Definitions necessary to update LDAP servers.
 */

#ifndef __LDAPUPDATE_H__
#define __LDAPUPDATE_H__

#ifdef USE_LDAP

#include <glib.h>

#include "ldapserver.h"

/* Function proto types */
void ldapsvr_update_book(LdapServer *server, ItemPerson *item);

#endif	/* USE_LDAP */

#endif /* __LDAPUPDATE_H__ */

