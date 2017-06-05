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

#ifndef __ACCOUNT_H__
#define __ACCOUNT_H__

#include <glib.h>

#include "prefs_gtk.h"
#include "prefs_account.h"
#include "folder.h"

typedef gint	(*AccountFunc)	(PrefsAccount	*ac_prefs,
				 gpointer	 user_data);

extern PrefsAccount *cur_account;

PrefsAccount *account_get_cur_account   (void);
void	      account_read_config_all	(void);
void	      account_write_config_all	(void);

GList        *account_find_all_from_address	(GList		*ac_list,
						 const gchar	*address);
PrefsAccount *account_find_from_smtp_server	(const gchar	*address,
						 const gchar	*smtp_server);
PrefsAccount *account_find_from_address		(const gchar	*address, gboolean newsgroups_ok);
PrefsAccount *account_find_from_id		(gint		 id);
PrefsAccount *account_find_from_item		(FolderItem	*item);

void	      account_set_menu_only_toolbar	(void);

GList	     *account_get_list		(void);

void	      account_edit_open		(gpointer a, gpointer b);
void	      account_add		(void);
void	      account_open		(PrefsAccount	*ac_prefs);
void	      account_set_as_recv_at_get_all	(PrefsAccount	*ac_prefs);
PrefsAccount *account_get_default	(void);

void	      account_set_missing_folder(void);
FolderItem   *account_get_special_folder(PrefsAccount		*ac_prefs,
					 SpecialFolderItemType	 type);

PrefsAccount *account_get_reply_account	(MsgInfo 	*msginfo, 
					 gboolean	 reply_autosel);
void 	      account_rename_path	(const gchar 	*old_id, 
					 const gchar 	*new_id);
gchar *account_get_signature_str(PrefsAccount *account);

#endif /* __ACCOUNT_H__ */
