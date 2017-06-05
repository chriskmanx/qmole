/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 Match Grun and the Claws Mail team
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
 * Address book base data.
 */

#ifndef __ADBOOKBASE_H__
#define __ADBOOKBASE_H__

#include <glib.h>

#include "addrcache.h"

typedef enum {
	ADBOOKTYPE_NONE,
	ADBOOKTYPE_BOOK,
	ADBOOKTYPE_VCARD,
	ADBOOKTYPE_JPILOT,
	ADBOOKTYPE_LDAP
} AddressBookType;

/*
 * All address book interfaces should implement the following data
 * structure at the start of their data structure.
 */
typedef struct _AddrBook_Base AddrBookBase;
struct _AddrBook_Base {
	AddressBookType type;
	AddressCache    *addressCache;
};

#endif /* __ADBOOKBASE_H__ */


