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
 * Gather addresses.
 */

#ifndef __ADDR_GATHER_H__
#define __ADDR_GATHER_H__

/* Function prototypes */
#include "folder.h"

#ifndef USE_NEW_ADDRBOOK
#include "addrbook.h"

AddressBookFile *addrgather_dlg_execute( FolderItem *folderItem,
					 AddressIndex *addrIndex,
					 gboolean sourceInd,
					 GList *msgList );
#else
void addrgather_dlg_execute( FolderItem *folderItem,
					 		 gboolean sourceInd,
					 		 GList *msgList );
#endif
#endif /* __ADDR_GATHER_H__ */

/*
* End of Source.
*/

