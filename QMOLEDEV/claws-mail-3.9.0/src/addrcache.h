/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2001-2012 Match Grun and the Claws Mail team
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

/*
 * Definitions for address cache.
 */

#ifndef __ADDRCACHE_H__
#define __ADDRCACHE_H__

#include <time.h>
#include <stdio.h>
#include <glib.h>
#include "addritem.h"

/* Address cache */
typedef struct _AddressCache AddressCache;

struct _AddressCache {
	gint       nextID;
	gboolean   dataRead;
	gboolean   modified;
	time_t     modifyTime;
	GHashTable *itemHash;
	GList      *tempList;
	ItemFolder *rootFolder;
	gchar      *cacheID;
	gboolean   dirtyFlag;
	gboolean   accessFlag;
	gchar      *name;
};

/* Function prototypes */
AddressCache *addrcache_create		( void );
ItemFolder *addrcache_get_root_folder	( AddressCache *cache );
GList *addrcache_get_list_folder	( AddressCache *cache );
GList *addrcache_get_list_person	( AddressCache *cache );
gboolean addrcache_get_dirty		( AddressCache *cache );
void addrcache_set_dirty		( AddressCache *cache,
					  const gboolean value );
gchar *addrcache_get_name		( AddressCache *cache );
void addrcache_set_name			( AddressCache *cache,
					  const gchar *value );

void addrcache_refresh			( AddressCache *cache );
void addrcache_clear			( AddressCache *cache );
void addrcache_free			( AddressCache *cache );
gboolean addrcache_check_file		( AddressCache *cache, gchar *path );
gboolean addrcache_mark_file		( AddressCache *cache, gchar *path );

void addrcache_print			( AddressCache *cache, FILE *stream );

void addrcache_id_person		( AddressCache *cache, ItemPerson *person );
void addrcache_id_group			( AddressCache *cache, ItemGroup *group );
void addrcache_id_folder		( AddressCache *cache, ItemFolder *folder );
void addrcache_id_email			( AddressCache *cache, ItemEMail *email );
void addrcache_id_attribute		( AddressCache *cache, UserAttribute *attrib );

gboolean addrcache_hash_add_person	( AddressCache *cache, ItemPerson *person );
gboolean addrcache_hash_add_group	( AddressCache *cache, ItemGroup *group );
gboolean addrcache_hash_add_folder	( AddressCache *cache, ItemFolder *folder );

gboolean addrcache_folder_add_person	( AddressCache *cache,
					  ItemFolder *folder,
					  ItemPerson *item );
gboolean addrcache_folder_add_folder	( AddressCache *cache,
					  ItemFolder *folder,
					  ItemFolder *item );
gboolean addrcache_folder_add_group	( AddressCache *cache,
					  ItemFolder *folder,
					  ItemGroup *item );

gboolean addrcache_add_person		( AddressCache *cache,
					  ItemPerson *person );
gboolean addrcache_add_group		( AddressCache *cache,
					  ItemGroup *group );
gboolean addrcache_person_add_email	( AddressCache *cache,
					  ItemPerson *person,
					  ItemEMail *email );
gboolean addrcache_group_add_email	( AddressCache *cache,
					  ItemGroup *group,
					  ItemEMail *email );
gboolean addrcache_add_folder		( AddressCache *cache,
					  ItemFolder *folder );

void addrcache_folder_move_person	( AddressCache *cache,
					  ItemPerson *person,
					  ItemFolder *target );
void addrcache_folder_move_group	( AddressCache *cache,
					  ItemGroup *group,
					  ItemFolder *target );
void addrcache_folder_move_folder	( AddressCache *cache,
					  ItemFolder *folder,
					  ItemFolder *target );

AddrItemObject *addrcache_get_object	( AddressCache *cache, const gchar *uid );
ItemEMail *addrcache_get_email		( AddressCache *cache, const gchar *eid );


ItemGroup *addrcache_remove_group	( AddressCache *cache, ItemGroup *group );
ItemPerson *addrcache_remove_person	( AddressCache *cache, ItemPerson *person );
ItemEMail *addrcache_remove_email	( AddressCache *cache, ItemEMail *email );

ItemEMail *addrcache_person_remove_email( AddressCache *cache,
					  ItemPerson *person, 
					  ItemEMail *email );

GList *addrcache_get_group_for_person	( AddressCache *cache, ItemPerson *person );

GList *addrcache_get_all_persons	( AddressCache *cache );
GList *addrcache_get_all_groups		( AddressCache *cache );

ItemFolder *addrcache_remove_folder		( AddressCache *cache,
						  ItemFolder *folder );
ItemFolder *addrcache_remove_folder_delete	( AddressCache *cache,
						  ItemFolder *folder );

ItemPerson *addrcache_add_contact	( AddressCache *cache,
					  ItemFolder *folder,
					  const gchar *name,
					  const gchar *address,
					  const gchar *remarks ); 

ItemFolder *addrcache_add_new_folder	( AddressCache *cache,
					  ItemFolder *parent );

#endif /* __ADDRCACHE_H__ */
