/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & the Claws Mail Team
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

#ifndef IMAP_GTK_H
#define IMAP_GTK_H

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif


#include <glib.h>
#include "folder.h"

void imap_gtk_init(void);
void imap_gtk_synchronise(FolderItem *item, gint days);
gboolean imap_gtk_should_override(void);

#endif /* IMAP_GTK_H */
