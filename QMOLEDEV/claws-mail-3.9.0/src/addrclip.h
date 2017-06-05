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
 * Address clip board selection.
 */

#ifndef __ADDRESS_CLIP_H__
#define __ADDRESS_CLIP_H__

#include <stdio.h>
#include <glib.h>

#include "addrindex.h"
#include "addrselect.h"

/*
* Selection data.
*/
typedef struct _AddressClipboard_ AddressClipboard;
struct _AddressClipboard_ {
	gboolean     cutFlag;		/* Indicates cut/copy operation    */
	gboolean     moveFlag;		/* Internal move indicator for cut */
	AddressIndex *addressIndex;	/* Reference to address index      */
	GList        *objectList;	/* List of objects in clipboard    */
};

/*
* Function prototypes.
*/
AddressClipboard *addrclip_create	( void );
void addrclip_clear		( AddressClipboard *clipBoard );
void addrclip_free		( AddressClipboard *clipBoard );
void addrclip_set_index		( AddressClipboard *clipBoard,
				  AddressIndex *addrIndex );
void addrclip_add		( AddressClipboard *clipBoard,
				  AddrSelectList *asl );
gboolean addrclip_is_empty	( AddressClipboard *clipBoard );
void addrclip_list_show		( AddressClipboard *clipBoard,
				  FILE *stream );
void addrclip_delete_item	( AddressClipboard *clipBoard );
GList *addrclip_paste_copy	( AddressClipboard *clipBoard,
				  AddressBookFile *book,
				  ItemFolder *folder );
GList *addrclip_paste_cut	( AddressClipboard *clipBoard,
				  AddressBookFile *book,
				  ItemFolder *folder );

#endif /* __ADDRRESS_CLIP_H__ */

