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
 * General functions for accessing address index file.
 */

#ifndef __ADDRINDEX_H__
#define __ADDRINDEX_H__

#include <stdio.h>
#include <glib.h>
#include "addritem.h"
#include "addrcache.h"
#include "addrquery.h"

#define ADDRESSBOOK_MAX_IFACE  4
#define ADDRESSBOOK_INDEX_FILE "addrbook--index.xml"
#define ADDRESSBOOK_OLD_FILE   "addressbook.xml"

typedef enum {
	ADDR_IF_NONE,
	ADDR_IF_BOOK,
	ADDR_IF_VCARD,
	ADDR_IF_JPILOT,
	ADDR_IF_LDAP,
	ADDR_IF_COMMON,
	ADDR_IF_PERSONAL
} AddressIfType;

typedef struct _AddressIndex AddressIndex;
struct _AddressIndex {
	AddrItemObject obj;
	gchar *filePath;
	gchar *fileName;
	gint  retVal;
	gboolean needsConversion;
	gboolean wasConverted;
	gboolean conversionError;
	AddressIfType lastType;
	gboolean dirtyFlag;
	GList *interfaceList;
	GHashTable *hashCache;
	gboolean loadedFlag;
	GList *searchOrder;
};

typedef struct _AddressInterface AddressInterface;
struct _AddressInterface {
	AddrItemObject obj;
	AddressIfType type;
	gchar *name;
	gchar *listTag;
	gchar *itemTag;
	gboolean legacyFlag;
	gboolean useInterface;
	gboolean haveLibrary;
	gboolean readOnly;
	GList *listSource;
	gboolean (*getModifyFlag)( void * );
	gboolean (*getAccessFlag)( void * );
	gboolean (*getReadFlag)( void * );
	gint (*getStatusCode)( void * );
	gint (*getReadData)( void * );
	ItemFolder *(*getRootFolder)( void * );
	GList *(*getListFolder)( void * );
	GList *(*getListPerson)( void * );
	GList *(*getAllPersons)( void * );
	GList *(*getAllGroups)( void * );
	gchar *(*getName)( void * );
	void (*setAccessFlag)( void *, void * );
	gboolean externalQuery;
	gint searchOrder;
	void (*startSearch)( void * );
	void (*stopSearch)( void * );
};

#ifdef G_OS_WIN32
/* W32 headers define INTERFACE to "struct".  */
#undef interface
#endif

typedef struct _AddressDataSource AddressDataSource;
struct _AddressDataSource {
	AddrItemObject obj;
	AddressIfType type;
	AddressInterface *interface;
	gpointer rawDataSource;
};

void addrindex_initialize		( void );
void addrindex_teardown			( void );

AddressIndex *addrindex_create_index	( void );
void addrindex_set_file_path		( AddressIndex *addrIndex,
					  const gchar *value );
void addrindex_set_file_name		( AddressIndex *addrIndex,
					  const gchar *value );

GList *addrindex_get_interface_list	( AddressIndex *addrIndex );
void addrindex_free_index		( AddressIndex *addrIndex );
void addrindex_print_index		( AddressIndex *addrIndex, FILE *stream );


AddressDataSource *addrindex_index_add_datasource	( AddressIndex *addrIndex,
							  AddressIfType ifType,
							  gpointer dataSource );
AddressDataSource *addrindex_index_remove_datasource	( AddressIndex *addrIndex,
							  AddressDataSource *dataSource );

void addrindex_free_datasource		( AddressDataSource *ds );
gchar *addrindex_get_cache_id		( AddressIndex *addrIndex,
					  AddressDataSource *ds );
AddressCache *addrindex_get_cache	( AddressIndex *addrIndex,
					  const gchar *cacheID );

gint addrindex_read_data		( AddressIndex *addrIndex );
gint addrindex_save_data		( AddressIndex *addrIndex );
gint addrindex_create_new_books		( AddressIndex *addrIndex );
gint addrindex_save_all_books		( AddressIndex *addrIndex );

gboolean addrindex_ds_get_modify_flag	( AddressDataSource *ds );
gboolean addrindex_ds_get_access_flag	( AddressDataSource *ds );
gboolean addrindex_ds_get_read_flag	( AddressDataSource *ds );
gint addrindex_ds_get_status_code	( AddressDataSource *ds );
gint addrindex_ds_read_data		( AddressDataSource *ds );
ItemFolder *addrindex_ds_get_root_folder( AddressDataSource *ds );
gchar *addrindex_ds_get_name		( AddressDataSource *ds );
void addrindex_ds_set_access_flag	( AddressDataSource *ds,
					  gboolean *value );
gboolean addrindex_ds_get_readonly	( AddressDataSource *ds );

/* Search support */
gint addrindex_setup_search		( const gchar *searchTerm,
					  void *callBackEnd,
					  void *callBackEntry );

gint addrindex_setup_static_search	( AddressDataSource *ds,
					  const gchar *searchTerm,
					  ItemFolder *folder,
					  void *callBackEnd,
					  void *callBackEntry );

gboolean addrindex_start_search		( const gint queryID );
void addrindex_stop_search		( const gint queryID );
gint addrindex_setup_explicit_search	( AddressDataSource *ds, 
					  const gchar *searchTerm, 
					  ItemFolder *folder,
					  void *callBackEnd,
					  void *callBackEntry );
void addrindex_remove_results		( AddressDataSource *ds,
					  ItemFolder *folder );

gboolean addrindex_load_completion(
		gint (*callBackFunc)
			( const gchar *, const gchar *, 
			  const gchar *, const gchar *, GList * ),
			gchar *folderpath );

gboolean addrindex_load_person_attribute( const gchar *attr,
		gint (*callBackFunc)
			( ItemPerson *, const gchar * ) );

gboolean addrindex_load_person_ds( gint (*callBackFunc)
			( ItemPerson *, AddressDataSource * ) );
gchar *addrindex_get_picture_file(const gchar *emailaddr);		
#endif /* __ADDRINDEX_H__ */

/*
* End of Source.
*/
