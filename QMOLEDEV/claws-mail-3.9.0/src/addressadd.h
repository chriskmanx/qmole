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

#ifndef __ADDRESS_ADD_H__
#define __ADDRESS_ADD_H__

#ifndef USE_NEW_ADDRBOOK
	#include "addrindex.h"
#endif
#include "gtk/gtk.h"

#ifndef USE_NEW_ADDRBOOK
gboolean addressadd_selection( AddressIndex *addrIndex, const gchar *name, const gchar *address, const gchar *remarks,
			       GdkPixbuf *picture );
#else
gboolean addressadd_selection(const gchar *name, const gchar *address, const gchar *remarks,
			       GdkPixbuf *picture );
#endif

#endif /* __ADDRESS_ADD_H__ */
