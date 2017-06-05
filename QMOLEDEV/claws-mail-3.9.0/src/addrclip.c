/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 Match Grun and the Claws Mail team
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
 * Contains address clipboard objects and related functions. The address 
 * clipboard is implemented as a linked list of AddrSelectItem objects. 
 * The address clipboard offers two groups of functions:
 *
 * a) Cut, copy and paste of address item objects (ItemFolder, ItemGroup,
 *    ItemPerson) into a folder. With this method, we can paste ItemPerson
 *    objects but not unattached ItemEMail objects into a folder. ItemEMail 
 *    objects are owned by an ItemPerson object. Any ItemEMail objects that 
 *    appear in the clipboard are ignored. If an ItemPerson object is found, 
 *    the ItemPerson *and* ItemEMail objects that it owns are pasted.
 *
 * b) Copy and paste of ItemEMail address objects only into (below) 
 *    ItemPerson objects. All ItemEMail objects which are owned by
 *    ItemPerson and referenced by ItemGroup objects are pasted. Any
 *    ItemFolder objects in the clipboard, and any objects owned by
 *    ItemFolder objects are ignored.
 *
 * Objects are inserted to the clipboard by copying (cloning) 
 * AddrSelectItem objects from the address books selection list to the
 * clipboard's internal selection list. The clipboard makes use of the
 * object id's and address cache id's to access objects contained in
 * the address cache. If the referenced object is not found, it is
 * ignored. This eliminates the need to delete pointers in multiple
 * linked lists when an address object is deleted.
 * 
 */

#include <stdio.h>
#include <glib.h>
#include <glib/gi18n.h>

#include "addrcache.h"
#include "addrbook.h"
#include "addrselect.h"
#include "addrindex.h"
#include "addrclip.h"
#include "alertpanel.h"
#include "defs.h"

/*
* Create a clipboard.
*/
AddressClipboard *addrclip_create( void ) {
	AddressClipboard *clipBoard;

	clipBoard = g_new0( AddressClipboard, 1 );
	clipBoard->cutFlag = FALSE;
	clipBoard->objectList = NULL;
	return clipBoard;
}

/*
* Clear clipboard.
*/
void addrclip_clear( AddressClipboard *clipBoard ) {
	GList *node;
	AddrSelectItem *item;

	cm_return_if_fail( clipBoard != NULL );
	node = clipBoard->objectList;
	while( node ) {
		item = node->data;
		addrselect_item_free( item );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( clipBoard->objectList );
	clipBoard->objectList = NULL;
}

/*
* Free up a clipboard.
*/
void addrclip_free( AddressClipboard *clipBoard ) {
	cm_return_if_fail( clipBoard != NULL );

	addrclip_clear( clipBoard );
	clipBoard->cutFlag = FALSE;
}

/*
* Setup reference to address index.
*/
void addrclip_set_index(
	AddressClipboard *clipBoard, AddressIndex *addrIndex )
{
	cm_return_if_fail( clipBoard != NULL );
	cm_return_if_fail( addrIndex != NULL );
	clipBoard->addressIndex = addrIndex;
}

/*
* Test whether clipboard is empty.
* Enter: clipBoard Clipboard.
* Return: TRUE if clipboard is empty.
*/
gboolean addrclip_is_empty( AddressClipboard *clipBoard ) {
	gboolean retVal = TRUE;

	if( clipBoard ) {
		if( clipBoard->objectList ) retVal = FALSE;
	}
	return retVal;
}

/*
* Add a list of address selection objects to clipbard.
* Enter: clipBoard Clipboard.
*        addrList  List of address selection objects.
*/
void addrclip_add( AddressClipboard *clipBoard, AddrSelectList *asl ) {
	GList *node;

	cm_return_if_fail( clipBoard != NULL );
	cm_return_if_fail( asl != NULL );
	node = asl->listSelect;
	while( node ) {
		AddrSelectItem *item, *itemCopy;

		item = node->data;
		itemCopy = addrselect_item_copy( item );
		clipBoard->objectList =
			g_list_append( clipBoard->objectList, itemCopy );
		node = g_list_next( node );
	}
}

/*
* Show clipboard contents.
* Enter: clipBoard Clipboard.
*        stream    Output stream.
*/
void addrclip_list_show( AddressClipboard *clipBoard, FILE *stream ) {
	GList *node;
	AddrItemObject *aio;
	AddressCache *cache;

	cm_return_if_fail( clipBoard != NULL );
	node = clipBoard->objectList;
	while( node != NULL ) {
		AddrSelectItem *item;

		item = node->data;
		addrselect_item_print( item, stream );

		cache = addrindex_get_cache( clipBoard->addressIndex, item->cacheID );
		aio = addrcache_get_object( cache, item->uid );
		if( aio ) {
			if( ADDRITEM_TYPE(aio) == ITEMTYPE_PERSON ) {
				addritem_print_item_person( ( ItemPerson * ) aio, stream );
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_EMAIL ) {
				addritem_print_item_email( ( ItemEMail * ) aio, stream );
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_GROUP ) {
				addritem_print_item_group( ( ItemGroup * ) aio, stream );
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_FOLDER ) {
				addritem_print_item_folder( ( ItemFolder * ) aio, stream );
			}
		}
		node = g_list_next( node );
	}
}

/* Pasted address pointers */
typedef struct _AddrClip_EMail_ AddrClip_EMail;
struct _AddrClip_EMail_ {
	ItemEMail *original;
	ItemEMail *copy;
};

/*
 * Free up specified list of addresses.
 */
static void addrclip_free_copy_list( GList *copyList ) {
	GList *node;

	node = copyList;
	while( node ) {
		AddrClip_EMail *em = node->data;
		em->original = NULL;
		em->copy = NULL;
		g_free( em );
		em = NULL;
		node = g_list_next( node );
	}
}

/*
 * Paste person into cache.
 * Enter: cache    Address cache to paste into.
 *        folder   Folder to store
 *        person   Person to paste.
 *        copyLIst List of email addresses pasted.
 * Return: Update list of email addresses pasted.
 */
static GList *addrclip_cache_add_person(
	AddressCache *cache, ItemFolder *folder, ItemPerson *person,
	GList *copyList )
{
	ItemPerson *newPerson;
	ItemEMail *email;
	ItemEMail *newEMail;
	UserAttribute *attrib;
	UserAttribute *newAttrib;
	GList *node;
	AddrClip_EMail *em;

	/* Copy person */
	newPerson = addritem_copy_item_person( person );
	addrcache_id_person( cache, newPerson );
	addrcache_folder_add_person( cache, folder, newPerson );

	/* Copy email addresses */
	node = person->listEMail;
	while( node ) {
		email = node->data;
		newEMail = addritem_copy_item_email( email );
		addrcache_id_email( cache, newEMail );
		addrcache_person_add_email( cache, newPerson, newEMail );
		node = g_list_next( node );

		/* Take a copy of the original */
		em = g_new0( AddrClip_EMail, 1 );
		em->original = email;
		em->copy = newEMail;
		copyList = g_list_append( copyList, em );
	}

	/* Copy user attributes */
	node = person->listAttrib;
	while( node ) {
		attrib = node->data;
		newAttrib = addritem_copy_attribute( attrib );
		addrcache_id_attribute( cache, newAttrib );
		addritem_person_add_attribute( newPerson, newAttrib );
		node = g_list_next( node );
	}

	/* Set picture name and create picture file (from copy) if missing */
	addritem_person_set_picture(newPerson, ADDRITEM_ID(newPerson));
	if( strcmp(ADDRITEM_ID(newPerson), ADDRITEM_ID(person)) ) {
		gchar *pictureFile;
		gchar *newPictureFile;

		pictureFile = g_strconcat( get_rc_dir(), G_DIR_SEPARATOR_S, ADDRBOOK_DIR, G_DIR_SEPARATOR_S, 
							person->picture, ".png", NULL );
		newPictureFile = g_strconcat( get_rc_dir(), G_DIR_SEPARATOR_S, ADDRBOOK_DIR, G_DIR_SEPARATOR_S, 
							newPerson->picture, ".png", NULL );
		if (file_exist(pictureFile, FALSE) && !file_exist(newPictureFile, FALSE)) {
			debug_print("copying contact picture file: %s -> %s\n", person->picture, newPerson->picture);
			copy_file(pictureFile, newPictureFile, FALSE);
		}
		g_free( pictureFile );
		g_free( newPictureFile );
	}

	return copyList;
}

/*
 * Search for new email record in copied email list.
 * Enter: copyList  List of copied email address mappings.
 *        emailOrig Original email item.
 * Return: New email item corresponding to original item if pasted. Or NULL if
 *         not found.
 */
static ItemEMail *addrclip_find_copied_email(
	GList *copyList, ItemEMail *emailOrig )
{
	ItemEMail *emailCopy;
	GList *node;
	AddrClip_EMail *em;

	emailCopy = NULL;
	node = copyList;
	while( node ) {
		em = node->data;
		if( em->original == emailOrig ) {
			emailCopy = em->copy;
			break;
		}
		node = g_list_next( node );
	}
	return emailCopy;
}

/*
 * Paste group into cache.
 * Enter: cache    Address cache to paste into.
 *        folder   Folder to store
 *        group    Group to paste.
 *        copyList List of email addresses pasted.
 * Return: Group added.
 */
static ItemGroup *addrclip_cache_add_group(
	AddressCache *cache, ItemFolder *folder, ItemGroup *group,
	GList *copyList )
{
	ItemGroup *newGroup;
	ItemEMail *emailOrig, *emailCopy;
	GList *node;

	/* Copy group */
	newGroup = addritem_copy_item_group( group );
	addrcache_id_group( cache, newGroup );
	addrcache_folder_add_group( cache, folder, newGroup );

	/* Add references of copied addresses to group */
	node = group->listEMail;
	while( node ) {
		emailOrig = ( ItemEMail * ) node->data;
		emailCopy = addrclip_find_copied_email( copyList, emailOrig );
		if( emailCopy ) {
			addrcache_group_add_email( cache, newGroup, emailCopy );
		}
		node = g_list_next( node );
	}
	return newGroup;
}

/*
 * Copy specified folder into cache. Note this functions uses pointers to
 * folders to copy from. There should not be any deleted items referenced
 * by these pointers!!!
 * Enter: cache        Address cache to copy into.
 *        targetFolder Target folder.
 *        folder       Folder to copy.
 * Return: Folder added.
 */
static ItemFolder *addrclip_cache_copy_folder(
	AddressCache *cache, ItemFolder *targetFolder, ItemFolder *folder )
{
	ItemFolder *newFolder;
	ItemGroup *newGroup;
	GList *node;
	GList *copyList;

	/* Copy folder */
	newFolder = addritem_copy_item_folder( folder );
	addrcache_id_folder( cache, newFolder );
	addrcache_folder_add_folder( cache, targetFolder, newFolder );

	/* Copy people to new folder */
	copyList = NULL;
	node = folder->listPerson;
	while( node ) {
		ItemPerson *item = node->data;
		node = g_list_next( node );
		copyList = addrclip_cache_add_person(
				cache, newFolder, item, copyList );
	}

	/* Copy groups to new folder */
	node = folder->listGroup;
	while( node ) {
		ItemGroup *item = node->data;
		node = g_list_next( node );
		newGroup = addrclip_cache_add_group(
				cache, newFolder, item, copyList );
		if (newGroup == NULL) {
			g_message("error allocating memory for new group\n");
		}
	}
	g_list_free( copyList );

	/* Copy folders to new folder (recursive) */
	node = folder->listFolder;
	while( node ) {
		ItemFolder *item = node->data;
		node = g_list_next( node );
		addrclip_cache_copy_folder( cache, newFolder, item );
	}

	return newFolder;
}

static gboolean addrclip_is_subfolder_of(ItemFolder *is_parent, ItemFolder *is_child)
{
	ItemFolder *folder;
	AddrItemObject *obj;

	cm_return_val_if_fail(is_parent != NULL, FALSE);
	cm_return_val_if_fail(is_child != NULL, FALSE);

	if (is_parent == is_child)
		return TRUE;

	folder = is_child;
	obj = folder->obj.parent;
	while (obj) {
		if ((void*)obj == (void*)is_parent)
			return TRUE;
		obj = obj->parent;
	}
	return FALSE;
}

/*
* Paste item list into address book.
* Enter: cache     Target address cache.
*        folder    Target folder where data is pasted.
*        itemList  List of items to paste.
*        clipBoard Clipboard.
* Return: List of group or folder items added.
*/
static GList *addrclip_cache_add_folder(
	AddressCache *cache, ItemFolder *folder, GList *itemList,
	AddressClipboard *clipBoard )
{
	GList *folderGroup;
	GList *node;
	AddrSelectItem *item;
	AddrItemObject *aio;
	AddressCache *cacheFrom;
	gboolean haveGroups;
	GList *copyList;

	folderGroup = NULL;
	copyList = NULL;
	haveGroups = FALSE;
	node = itemList;
	while( node ) {
		item = node->data;
		node = g_list_next( node );

		cacheFrom = addrindex_get_cache(
				clipBoard->addressIndex, item->cacheID );
		if( cacheFrom == NULL ) continue;
		if( item->uid ) {
			aio = addrcache_get_object( cacheFrom, item->uid );
			if( aio ) {
				if( ADDRITEM_TYPE(aio) == ITEMTYPE_PERSON ) {
					ItemPerson *person;

					person = ( ItemPerson * ) aio;
					copyList = addrclip_cache_add_person(
						cache, folder, person, copyList );
				}
				/*
				else if( ADDRITEM_TYPE(aio) == ITEMTYPE_EMAIL ) {
				} 
				*/
				else if( ADDRITEM_TYPE(aio) == ITEMTYPE_GROUP ) {
					haveGroups = TRUE;	/* Process later */
				}
				else if( ADDRITEM_TYPE(aio) == ITEMTYPE_FOLDER ) {
					ItemFolder *itemFolder, *newFolder;

					itemFolder = ( ItemFolder * ) aio;
					if (!addrclip_is_subfolder_of(itemFolder, folder)) {
						newFolder = addrclip_cache_copy_folder(
								cache, folder, itemFolder );
						folderGroup =
							g_list_append( folderGroup, newFolder );
					} else {
						alertpanel_error(
							_("Cannot copy a folder to itself or to its sub-structure.") );
					}
				}
			}
		}
		else {
			if( item->objectType == ITEMTYPE_DATASOURCE ) {
				/*
				* Must be an address book - allow copy only if
				* copying from a different cache.
				*/
				if( cache != cacheFrom ) {
					ItemFolder *itemFolder, *newFolder;

					itemFolder = cacheFrom->rootFolder;
					newFolder = addrclip_cache_copy_folder(
						cache, folder, itemFolder );
					addritem_folder_set_name( newFolder,
						addrcache_get_name( cacheFrom ) );
					folderGroup =
						g_list_append( folderGroup, newFolder );
				} else {
					alertpanel_error(
						_("Cannot copy an address book to itself.") );
				}
			}
		}
	}

	/* Finally add any groups */
	if( haveGroups ) {
		node = itemList;
		while( node ) {
			item = node->data;
			node = g_list_next( node );
			cacheFrom = addrindex_get_cache(
					clipBoard->addressIndex, item->cacheID );
			if( cacheFrom == NULL ) continue;
			aio = addrcache_get_object( cacheFrom, item->uid );
			if( aio ) {
				if( ADDRITEM_TYPE(aio) == ITEMTYPE_GROUP ) {
					ItemGroup *group, *newGroup;

					group = ( ItemGroup * ) aio;
					newGroup = addrclip_cache_add_group(
						cache, folder, group, copyList );
					folderGroup =
						g_list_append( folderGroup, newGroup );
				}
			}
		}
	}

	/* Free up stuff */
	addrclip_free_copy_list( copyList );
	g_list_free( copyList );
	copyList = NULL;

	return folderGroup;
}

/*
* Move items in list into new folder
* Enter: cache        Target address cache.
*        targetFolder Target folder where data is pasted.
*        itemList     List of items to paste.
*        clipBoard    Clipboard.
* Return: List of group or folder items added.
*/
static GList *addrclip_cache_move_items(
	AddressCache *cache, ItemFolder *targetFolder, GList *itemList,
	AddressClipboard *clipBoard )
{
	GList *folderGroup;
	GList *node;
	AddrSelectItem *item;
	AddrItemObject *aio;
	AddressCache *cacheFrom;

	folderGroup = NULL;
	node = itemList;
	while( node ) {
		item = node->data;
		node = g_list_next( node );
		cacheFrom = addrindex_get_cache(
				clipBoard->addressIndex, item->cacheID );
		if( cacheFrom == NULL ) continue;
		aio = addrcache_get_object( cacheFrom, item->uid );
		if( aio ) {
			if( ADDRITEM_TYPE(aio) == ITEMTYPE_PERSON ) {
				ItemPerson *person;

				person = ( ItemPerson * ) aio;
				addrcache_folder_move_person(
					cache, person, targetFolder );
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_GROUP ) {
				ItemGroup *group;

				group = ( ItemGroup * ) aio;
				addrcache_folder_move_group(
					cache, group, targetFolder );
				folderGroup = g_list_append( folderGroup, group );
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_FOLDER ) {
				ItemFolder *folder = ( ItemFolder * ) aio;

				if (!addrclip_is_subfolder_of(folder, targetFolder)) {
					addrcache_folder_move_folder(
						cache, folder, targetFolder );
					folderGroup =
						g_list_append( folderGroup, folder );
				} else {
					alertpanel_error(
						_("Cannot move a folder to itself or to its sub-structure.") );
				}
			}
		}
	}
	return folderGroup;
}

/*
* Get address cache of first item in list. This assumes that all items in
* the clipboard are located in the same cache.
* Enter: clipBoard Clipboard.
* Return: List of group or folder items added.
*/
static AddressCache *addrclip_list_get_cache( AddressClipboard *clipBoard ) {
	AddressCache *cache;
	GList *itemList;
	AddrSelectItem *item;

	cache = NULL;
	itemList = clipBoard->objectList;
	if( itemList ) {
		item = itemList->data;
		cache = addrindex_get_cache(
				clipBoard->addressIndex, item->cacheID );
	}
	return cache;
}

/*
* Paste (copy) clipboard into address book.
* Enter: clipBoard Clipboard.
*        book      Target address book.
*        folder    Target folder where data is pasted, or null for root folder.
* Return: List of group or folder items added.
*/
GList *addrclip_paste_copy(
	AddressClipboard *clipBoard, AddressBookFile *book,
	ItemFolder *folder )
{
	AddressCache *cache;
	GList *itemList;
	GList *folderGroup;

	cm_return_val_if_fail( clipBoard != NULL, NULL );

	cache = book->addressCache;
	if( folder == NULL ) folder = cache->rootFolder;

	folderGroup = NULL;
	itemList = clipBoard->objectList;
	folderGroup = addrclip_cache_add_folder(
			cache, folder, itemList, clipBoard );

	return folderGroup;
}

/*
* Remove items that were cut from clipboard.
* Enter: clipBoard Clipboard.
*/
void addrclip_delete_item( AddressClipboard *clipBoard ) {
	AddrSelectItem *item;
	AddrItemObject *aio;
	AddressCache *cacheFrom;
	GList *node;

	/* If cutting within current cache, no deletion is necessary */
	if( clipBoard->moveFlag ) return;

	/* Remove groups */
	node = clipBoard->objectList;
	while( node ) {
		item = node->data;
		node = g_list_next( node );
		cacheFrom = addrindex_get_cache(
				clipBoard->addressIndex, item->cacheID );
		if( cacheFrom == NULL ) continue;
		aio = addrcache_get_object( cacheFrom, item->uid );
		if( aio ) {
			if( ADDRITEM_TYPE(aio) == ITEMTYPE_GROUP ) {
				ItemGroup *group;

				group = ( ItemGroup * ) aio;
				group = addrcache_remove_group( cacheFrom, group );
				if( group ) {
					addritem_free_item_group( group );
				}
			}
		}
	}

	/* Remove persons and folders */
	node = clipBoard->objectList;
	while( node ) {
		item = node->data;
		node = g_list_next( node );

		cacheFrom = addrindex_get_cache(
				clipBoard->addressIndex, item->cacheID );
		if( cacheFrom == NULL ) continue;

		aio = addrcache_get_object( cacheFrom, item->uid );
		if( aio ) {
			if( ADDRITEM_TYPE(aio) == ITEMTYPE_PERSON ) {
				ItemPerson *person;

				person = ( ItemPerson * ) aio;
				person = addrcache_remove_person( cacheFrom, person );
				if( person ) {
					addritem_free_item_person( person );
				}
			}
			else if( ADDRITEM_TYPE(aio) == ITEMTYPE_FOLDER ) {
				ItemFolder *itemFolder;

				itemFolder = ( ItemFolder * ) aio;
				itemFolder = addrcache_remove_folder_delete(
						cacheFrom, itemFolder );
				addritem_free_item_folder( itemFolder );
			}
		}
	}
}

/*
* Paste (move) clipboard into address book.
* Enter: clipBoard Clipboard.
*        book      Target address book.
*        folder    Target folder where data is pasted, or null for root folder.
* Return: List of group or folder items added.
*/
GList *addrclip_paste_cut(
	AddressClipboard *clipBoard, AddressBookFile *book,
	ItemFolder *folder )
{
	AddressCache *cache, *cacheFrom;
	GList *itemList;
	GList *folderGroup;

	cm_return_val_if_fail( clipBoard != NULL, NULL );

	cache = book->addressCache;
	if( folder == NULL ) folder = cache->rootFolder;

	folderGroup = NULL;
	clipBoard->moveFlag = FALSE;
	cacheFrom = addrclip_list_get_cache( clipBoard );
	if( cacheFrom && cacheFrom == cache ) {
		/* Move items between folders in same book */
		itemList = clipBoard->objectList;
		folderGroup = addrclip_cache_move_items(
				cache, folder, itemList, clipBoard );
		clipBoard->moveFlag = TRUE;
	}
	else {
		/* Move items across address books */
		itemList = clipBoard->objectList;
		folderGroup = addrclip_cache_add_folder(
				cache, folder, itemList, clipBoard );
	}

	return folderGroup;
}
/*
* End of Source.
*/

