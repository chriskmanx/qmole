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

#ifndef __FOLDERSEL_H__
#define __FOLDERSEL_H__

#include <glib.h>
#include <gtk/gtk.h>

#include "folder.h"

typedef enum
{
	FOLDER_SEL_ALL,
	FOLDER_SEL_MOVE,
	FOLDER_SEL_COPY
} FolderSelectionType;

FolderItem *foldersel_folder_sel(Folder			*cur_folder,
				 FolderSelectionType	 type,
				 const gchar		*default_folder,
				 gboolean 			 can_sel_mailbox);
void foldersel_reflect_prefs_pixmap_theme(void);

#endif /* __FOLDERSEL_H__ */
