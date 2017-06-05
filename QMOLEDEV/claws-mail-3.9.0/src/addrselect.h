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
 * Address list item selection.
 */

#ifndef __ADDR_SELECT_H__
#define __ADDR_SELECT_H__

#include "addritem.h"
#include "addressitem.h"
#include "addrcache.h"

/*
* Selection item data.
*/
typedef struct _AddrSelectItem_ AddrSelectItem;
struct _AddrSelectItem_ {
	ItemObjectType    objectType;
	AddrItemObject    *addressItem;
	gchar             *uid;
	gchar             *cacheID;
};

/*
 * Selection list object.
 */
typedef struct _AddrSelectList_ AddrSelectList;
struct _AddrSelectList_ {
	GList *listSelect;
};

AddrSelectItem *addrselect_create_node	( AddressObject *obj );
AddrSelectItem *addrselect_item_copy	( AddrSelectItem *item );
void addrselect_item_free		( AddrSelectItem *item );
void addrselect_item_print		( AddrSelectItem *item,
					  FILE *stream );
AddrSelectList *addrselect_list_create	( void );
void addrselect_list_free		( AddrSelectList *asl );
GList *addrselect_get_list		( AddrSelectList *asl );
void addrselect_list_clear		( AddrSelectList *asl );
gboolean addrselect_test_empty		( AddrSelectList *asl );
void addrselect_list_add_obj		( AddrSelectList *asl,
					  AddrItemObject *aio,
					  gchar *cacheID );
void addrselect_list_add		( AddrSelectList *asl,
					  AddrSelectItem *item,
					  gchar *cacheID );
void addrselect_list_remove		( AddrSelectList *asl,
					  AddrItemObject *aio );
GList *addrselect_build_list		( AddrSelectList *asl );

#endif /* __ADDR_SELECT_H__ */

