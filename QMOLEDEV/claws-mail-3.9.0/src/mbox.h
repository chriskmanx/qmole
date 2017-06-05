/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __MBOX_H__
#define __MBOX_H__

#include <glib.h>

#include "folder.h"

typedef enum {
	LOCK_FILE,
	LOCK_FLOCK
} LockType;


gint proc_mbox		(FolderItem	*dest,
			 const gchar	*mbox,
			 gboolean	 apply_filter,
			 PrefsAccount	*account);
gint lock_mbox		(const gchar	*base,
			 LockType	 type);
gint unlock_mbox	(const gchar	*base,
			 gint		 fd,
			 LockType	 type);
gint copy_mbox		(gint		 srcfd,
			 const gchar	*dest);
void empty_mbox		(const gchar	*mbox);

gint export_to_mbox	(FolderItem	*src,
			 const gchar	*mbox);
gint export_list_to_mbox(GSList 	*mlist, 
			 const gchar 	*mbox);

#endif /* __MBOX_H__ */
