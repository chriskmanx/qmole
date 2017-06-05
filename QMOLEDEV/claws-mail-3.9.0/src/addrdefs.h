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
 * General address definitions.
 */

#ifndef __ADDRDEFS_H__
#define __ADDRDEFS_H__

/* Query types */
typedef enum {
	ADDRQUERY_NONE,
	ADDRQUERY_LDAP
} AddrQueryType;

/* Search type */
typedef enum {
	ADDRSEARCH_NONE,
	ADDRSEARCH_SEARCH,
	ADDRSEARCH_LOCATE
} AddrSearchType;

#endif /* __ADDRDEFS_H__ */

/*
* End of Source.
*/
