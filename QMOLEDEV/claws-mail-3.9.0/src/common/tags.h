/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2012 The Claws Mail Team
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

#ifndef __TAGS_H__
#define __TAGS_H__

#include <glib.h>

/* Reserved tags */
#define RTAG_NON_JUNK "NonJunk"
#define RTAG_NOT_JUNK "NotJunk"
#define RTAG_NO_JUNK "NoJunk"
#define RTAG_JUNK "Junk"
#define RTAG_FORWARDED "$Forwarded"

#define IS_NOT_RESERVED_TAG(tag) \
	(strcmp((tag), "NonJunk") && \
         strcmp((tag), "NotJunk") && \
         strcmp((tag), "NoJunk") && \
         strcmp((tag), "Junk") && \
         strcmp((tag), "$Forwarded")) 

void tags_read_tags(void);
void tags_write_tags(void);
gint tags_add_tag(const gchar *tag);
void tags_remove_tag(gint id);
void tags_update_tag(gint id, const gchar *tag);
const gchar *tags_get_tag(gint id);
gint tags_get_id_for_str(const gchar *str);
GSList *tags_get_list(void);
guint tags_get_size(void);

#endif 
