/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __NEWS_H__
#define __NEWS_H__

#include <glib.h>
#include <stdio.h>

typedef struct _NewsGroupInfo	NewsGroupInfo;

#include "folder.h"

struct _NewsGroupInfo
{
	gchar *name;
	guint first;
	guint last;
	gchar type;
};

FolderClass *news_get_class		(void);

GSList *news_get_group_list		(Folder		*folder);
void news_group_list_free		(GSList		*group_list);
void news_remove_group_list_cache	(Folder		*folder);

gint news_post				(Folder		*folder,
					 const gchar	*file);
gint news_cancel_article		(Folder 	*folder,
					 MsgInfo 	*msginfo);
int news_folder_locked			(Folder 	*folder);

guint nntp_folder_get_refcnt(Folder *folder);
void nntp_folder_ref(Folder *folder);
void nntp_folder_unref(Folder *folder);

#endif /* __NEWS_H__ */
