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

#ifndef __ADDRESSBOOK_H__
#define __ADDRESSBOOK_H__

#include <glib.h>
#include <gtk/gtk.h>

#include "compose.h"
#include "addritem.h"
#include "addrindex.h"

void addressbook_open			( Compose *target );
void addressbook_set_target_compose	( Compose *target );
Compose *addressbook_get_target_compose	( void );
void addressbook_read_file		( void );
void addressbook_export_to_file		( void );
gint addressbook_obj_name_compare	( gconstpointer a,
					  gconstpointer b );
void addressbook_destroy		( void );

gboolean addressbook_add_contact	( const gchar *name,
					  const gchar *address,
					  const gchar *remarks,
					  GdkPixbuf *picture );
					  
gchar *addressbook_folder_selection( const gchar *folderpath);
gboolean addressbook_peek_folder_exists( gchar *folderpath,
										 AddressDataSource **book,
										 ItemFolder **folder );

gboolean addressbook_load_completion	(gint (*callBackFunc) 
					       (const gchar *, 
					  	const gchar *, 
					  	const gchar *));

void addressbook_gather			( FolderItem *folderItem,
					  gboolean sourceInd,
					  GList *msgList );
void addressbook_harvest		( FolderItem *folderItem,
					  gboolean sourceInd,
					  GList *msgList);

void addressbook_read_all		( void );
void addressbook_address_list_set_focus( void );
void addressbook_address_list_disable_some_actions( void );
void addressbook_refresh( void );
gchar *addressbook_set_col_name_guard(gchar *value);
void addressbook_reflect_prefs_pixmap_theme(void);

#endif /* __ADDRESSBOOK_H__ */

