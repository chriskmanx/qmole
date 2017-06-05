/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2001-2012 Match Grun and the Claws Mail team
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
 * Add address to address book dialog.
 */

#ifndef __ADDRESSBOOK_FOLDERSEL_H__
#define __ADDRESSBOOK_FOLDERSEL_H__

#include "addrindex.h"

gboolean addressbook_foldersel_selection( AddressIndex *addrIndex,
				AddressBookFile **book, ItemFolder **folder, 
				const gchar *path );

#endif /* __ADDRESSBOOK_FOLDERSEL_H__ */
