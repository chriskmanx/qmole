/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> and 
 * the Claws Mail team
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

#ifndef BOGOFILTER_H
#define BOGOFILTER_H 1

#include <glib.h>
#include "folder.h"

typedef struct _BogofilterConfig BogofilterConfig;

typedef void (*MessageCallback) (gchar *, gint total, gint done, gboolean thread_safe);

struct _BogofilterConfig
{
	gboolean		 process_emails;
	gboolean 		 receive_spam;
	gchar 			*save_folder;
	guint 			 max_size;
	gchar			*bogopath;
	gboolean		 insert_header;
	gboolean		 whitelist_ab;
	gchar			*whitelist_ab_folder;
	gboolean		 learn_from_whitelist;
	gboolean		 save_unsure;
	gchar 			*save_unsure_folder;
	gboolean		 mark_as_read;
};

BogofilterConfig *bogofilter_get_config	      (void);
void		    bogofilter_save_config	      (void);
void 	            bogofilter_set_message_callback (MessageCallback callback);
gint bogofilter_gtk_init(void);
void bogofilter_gtk_done(void);
int bogofilter_learn(MsgInfo *msginfo, GSList *msglist, gboolean spam);
void bogofilter_register_hook(void);
void bogofilter_unregister_hook(void);
FolderItem *bogofilter_get_spam_folder(MsgInfo *msginfo);
#endif
