/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 by the Claws Mail Team and Hiroyuki Yamamoto
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

#ifndef LOCALFOLDER_H
#define LOCALFOLDER_H 1

typedef struct _LocalFolder	LocalFolder;

#define LOCAL_FOLDER(obj)	((LocalFolder *)obj)

#include <glib.h>

#include "folder.h"

struct _LocalFolder
{
	Folder folder;

	gchar *rootpath;
};

void	folder_local_folder_init	(Folder 	*folder,
					 const gchar 	*name,
			    		 const gchar 	*path);
void 	folder_local_folder_destroy	(LocalFolder	*lfolder);
void 	folder_local_set_xml		(Folder 	*folder,
					 XMLTag 	*tag);
XMLTag *folder_local_get_xml		(Folder 	*folder);

#endif /* LOCALFOLDER_H */
