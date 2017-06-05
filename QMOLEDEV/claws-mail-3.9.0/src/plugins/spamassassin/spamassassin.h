/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 the Claws Mail Team
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

#ifndef SPAMASSASSIN_H
#define SPAMASSASSIN_H 1

#include <glib.h>
#include "folder.h"

typedef struct _SpamAssassinConfig SpamAssassinConfig;

typedef void (*MessageCallback) (gchar *);

typedef enum {
	SPAMASSASSIN_DISABLED            = 0, /* unused, just for compatibility */
	SPAMASSASSIN_TRANSPORT_LOCALHOST = 1,
	SPAMASSASSIN_TRANSPORT_TCP       = 2,
	SPAMASSASSIN_TRANSPORT_UNIX      = 3,
} SpamAssassinTransport;

struct _SpamAssassinConfig
{
	gboolean		 enable;
	SpamAssassinTransport	 transport;
	gchar			*hostname;
	guint 			 port;
	gchar			*socket;
	gboolean		 process_emails;
	gboolean 		 receive_spam;
	gchar 			*save_folder;
	guint 			 max_size;
	guint 			 timeout;
	gchar			*username;
	gboolean		 mark_as_read;
	gboolean		 whitelist_ab;
	gchar			*whitelist_ab_folder;
};

SpamAssassinConfig *spamassassin_get_config	      (void);
void		    spamassassin_save_config	      (void);
gint			spamassassin_check_username	      (void);  
void 	            spamassassin_set_message_callback (MessageCallback callback);
gint spamassassin_gtk_init(void);
void spamassassin_gtk_done(void);
int spamassassin_learn(MsgInfo *msginfo, GSList *msglist, gboolean spam);
void spamassassin_register_hook(void);
void spamassassin_unregister_hook(void);
FolderItem *spamassassin_get_spam_folder(MsgInfo *msginfo);

#endif
