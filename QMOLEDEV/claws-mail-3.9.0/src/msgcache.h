/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & The Claws Mail Team
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

#ifndef __MSGCACHE_H__
#define __MSGCACHE_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>

typedef struct _MsgCache MsgCache;

#include "procmsg.h"
#include "folder.h"

MsgCache   	*msgcache_new				(void);
void	   	 msgcache_destroy			(MsgCache *cache);
MsgCache   	*msgcache_read_cache			(FolderItem *item,
							 const gchar *cache_file);
void	   	 msgcache_read_mark			(MsgCache *cache,
							 const gchar *mark_file);
void	   	 msgcache_read_tags			(MsgCache *cache,
							 const gchar *tags_file);
gint	   	 msgcache_write				(const gchar *cache_file,
							 const gchar *mark_file,
							 const gchar *tags_file,
							 MsgCache *cache);
void 	   	 msgcache_add_msg			(MsgCache *cache,
							 MsgInfo *msginfo);
void 	   	 msgcache_remove_msg			(MsgCache *cache,
							 guint num);
void 	    	 msgcache_update_msg			(MsgCache *cache,
							 MsgInfo *msginfo);
MsgInfo	   	*msgcache_get_msg			(MsgCache *cache,
							 guint num);
MsgInfo	   	*msgcache_get_msg_by_id			(MsgCache *cache,
							 const gchar *msgid);
MsgInfoList	*msgcache_get_msg_list			(MsgCache *cache);
time_t	   	 msgcache_get_last_access_time		(MsgCache *cache);
gint	   	 msgcache_get_memory_usage		(MsgCache *cache);

#endif
