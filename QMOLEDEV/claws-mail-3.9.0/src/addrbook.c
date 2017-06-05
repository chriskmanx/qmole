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

/* General functions for accessing address book files */

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <math.h>
#include <setjmp.h>

#include "utils.h"
#include "xml.h"
#include "mgutils.h"
#include "addritem.h"
#include "addrcache.h"
#include "addrbook.h"
#include "adbookbase.h"

#ifndef DEV_STANDALONE
#include "prefs_gtk.h"
#include "codeconv.h"
#endif

#define ADDRBOOK_MAX_SEARCH_COUNT 1000
#define ADDRBOOK_PREFIX           "addrbook-"
#define ADDRBOOK_SUFFIX           ".xml"
#define FILE_NUMDIGITS            6

#define ID_TIME_OFFSET            998000000

static void addrbook_print_book		( AddressBookFile *book, FILE *stream );

/**
 * Create new address book
 * \return Address book.
 */
AddressBookFile *addrbook_create_book()
{
	AddressBookFile *book;

	book = g_new0(AddressBookFile, 1);
	book->type = ADBOOKTYPE_BOOK;
	book->addressCache = addrcache_create();
	book->retVal = MGU_SUCCESS;
	book->path = NULL;
	book->fileName = NULL;
	book->maxValue = 0;
	book->tempList = NULL;
	book->tempHash = NULL;
	book->addressCache->modified = TRUE;

	return book;
}

/**
 * Specify name to be used
 * \param book  Address book.
 * \param value Name.
 */
void addrbook_set_name(AddressBookFile *book, const gchar *value)
{
	cm_return_if_fail(book != NULL);
	addrcache_set_name(book->addressCache, value);
}

gchar *addrbook_get_name(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_get_name(book->addressCache);
}

/**
 * Specify path to address book file.
 * \param book  Address book.
 * \param value Path.
 */
void addrbook_set_path(AddressBookFile *book, const gchar *value)
{
	cm_return_if_fail(book != NULL);
	book->path = mgu_replace_string(book->path, value);
	addrcache_set_dirty(book->addressCache, TRUE);
}

/**
 * Specify filename to be used
 * \param book  Address book.
 * \param value Filename.
 */
void addrbook_set_file(AddressBookFile *book, const gchar *value)
{
	cm_return_if_fail(book != NULL);
	book->fileName = mgu_replace_string(book->fileName, value);
	addrcache_set_dirty(book->addressCache, TRUE);
}

gboolean addrbook_get_modified(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, FALSE);
	return book->addressCache->modified;
}

gboolean addrbook_get_accessed(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, FALSE);
	return book->addressCache->accessFlag;
}

/**
 * Specify address book as accessed.
 * \param book  Address book.
 * \param value Value.
 */
void addrbook_set_accessed(AddressBookFile *book, const gboolean value)
{
	cm_return_if_fail(book != NULL);
	book->addressCache->accessFlag = value;
}

gboolean addrbook_get_read_flag(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, FALSE);
	return book->addressCache->dataRead;
}

gint addrbook_get_status(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, -1);
	return book->retVal;
}

ItemFolder *addrbook_get_root_folder(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_get_root_folder(book->addressCache);
}

GList *addrbook_get_list_folder(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_get_list_folder(book->addressCache);
}

GList *addrbook_get_list_person(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_get_list_person(book->addressCache);
}

gboolean addrbook_get_dirty(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, FALSE);
	return addrcache_get_dirty(book->addressCache);
}

/**
 * Set address book as dirty (needs to be written to file).
 * \param book  Address book.
 * \param value Dirty flag.
 */
void addrbook_set_dirty(AddressBookFile *book, const gboolean value)
{
	cm_return_if_fail(book != NULL);
	addrcache_set_dirty(book->addressCache, value);
}

/**
 * Free address book.
 * \param book Address book.
 */
void addrbook_free_book(AddressBookFile *book)
{
	cm_return_if_fail(book != NULL);

	/* Clear cache */
	addrcache_free(book->addressCache);

	/* Free up internal objects */
	g_free(book->path);
	g_free(book->fileName);
	g_list_free(book->tempList);

	book->path = NULL;
	book->fileName = NULL;
	book->maxValue = 0;
	book->tempList = NULL;
	book->tempHash = NULL;

	book->type = ADBOOKTYPE_NONE;
	book->addressCache = NULL;
	book->retVal = MGU_SUCCESS;

	g_free(book);
}

/**
 * Print address book header.
 * \param book   Address book.
 * \param stream Output stream.
 */
static void addrbook_print_book(AddressBookFile *book, FILE *stream)
{
	cm_return_if_fail(book != NULL);

	fprintf(stream, "AddressBook:\n");
	fprintf(stream, "\tpath : '%s'\n", book->path);
	fprintf(stream, "\tfile : '%s'\n", book->fileName);
	fprintf(stream, "\tstatus : %d\n", book->retVal );
	addrcache_print(book->addressCache, stream);
}

/**
 * Dump entire address book traversing folders.
 * \param book   Address book.
 * \param stream Output stream.
 */
void addrbook_dump_book(AddressBookFile *book, FILE *stream)
{
	ItemFolder *folder;

	cm_return_if_fail(book != NULL);

	addrbook_print_book(book, stream);
	folder = book->addressCache->rootFolder;
	addritem_print_item_folder(folder, stream);
}

/**
 * Remove specified group from address book. Note that object should still
 * be freed.
 * Specify name to be used
 * \param book  Address book.
 * \param group Group to remove.
 * \param value Name.
 * \return Group, or NULL if not found. 
 */
ItemGroup *addrbook_remove_group(AddressBookFile *book, ItemGroup *group)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_remove_group(book->addressCache, group);
}

/**
 * Remove specified person from address book. Note that object should still
 * be freed.
 * \param  book   Address book.
 * \param  person Person to remove.
 * \return Person, or NULL if not found.
 */
ItemPerson *addrbook_remove_person(AddressBookFile *book, ItemPerson *person)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_remove_person(book->addressCache, person);
}

/**
 * Remove specified email address in address book for specified person.
 * Note that object should still be freed.
 * \param  book   Address book.
 * \param  person Person.
 * \param  email  EMail to remove.
 * \return EMail object, or NULL if not found.
 */
ItemEMail *addrbook_person_remove_email(AddressBookFile *book,
					ItemPerson *person, ItemEMail *email)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_person_remove_email(book->addressCache, person, email);
}

/*
* ***********************************************************************
* Read/Write XML data file...
* ===========================
* Notes:
* 1)	The address book is structured as follows:
*
*		address-book
*			person
*				address-list
*					address
*				attribute-list
*					attribute
*			group
*				member-list
*					member
*			folder
*				item-list
*					item
*
* 2)	This sequence of elements was chosen so that the most important
* 	elements (person and their email addresses) appear first.
*
* 3)	Groups then appear. When groups are loaded, person's email
*	addresses have already been loaded and can be found.
*
* 4)	Finally folders are loaded. Any forward and backward references
*	to folders, groups and persons in the folders are resolved after
*	loading.
*
* ***********************************************************************
*/

/* Element tag names */
#define AB_ELTAG_ADDRESS         "address"
#define AB_ELTAG_ATTRIBUTE       "attribute"
#define AB_ELTAG_ATTRIBUTE_LIST  "attribute-list"
#define AB_ELTAG_ADDRESS_LIST    "address-list"
#define AB_ELTAG_MEMBER          "member"
#define AB_ELTAG_MEMBER_LIST     "member-list"
#define AB_ELTAG_ITEM            "item"
#define AB_ELTAG_ITEM_LIST       "item-list"
#define AB_ELTAG_ADDRESS_BOOK    "address-book"
#define AB_ELTAG_PERSON          "person"
#define AB_ELTAG_GROUP           "group"
#define AB_ELTAG_FOLDER          "folder"

/* Attribute tag names */
#define AB_ATTAG_TYPE            "type"
#define AB_ATTAG_UID             "uid"
#define AB_ATTAG_NAME            "name"
#define AB_ATTAG_REMARKS         "remarks"
#define AB_ATTAG_FIRST_NAME      "first-name"
#define AB_ATTAG_LAST_NAME       "last-name"
#define AB_ATTAG_NICK_NAME       "nick-name"
#define AB_ATTAG_COMMON_NAME     "cn"
#define AB_ATTAG_ALIAS           "alias"
#define AB_ATTAG_EMAIL           "email"
#define AB_ATTAG_EID             "eid"
#define AB_ATTAG_PID             "pid"

/* Attribute values */
#define AB_ATTAG_VAL_PERSON      "person"
#define AB_ATTAG_VAL_GROUP       "group"
#define AB_ATTAG_VAL_FOLDER      "folder"

/**
 * Parse address item for person from XML file.
 * \param book   Address book.
 * \param file   XML file handle.
 * \param person Person.
 */
static void addrbook_parse_address(AddressBookFile *book, XMLFile *file, 
				   ItemPerson *person)
{
	GList *attr;
	gchar *name, *value;
	ItemEMail *email = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (!email)
			email = addritem_create_item_email();
		if (strcmp(name, AB_ATTAG_UID) == 0)
			ADDRITEM_ID(email) = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_ALIAS) == 0)
			ADDRITEM_NAME(email) = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_EMAIL) == 0)
			email->address = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_REMARKS) == 0)
			email->remarks = g_strdup(value);
		attr = g_list_next(attr);
	}
	if (email) {
		if (person) {
			addrcache_person_add_email(book->addressCache, person,
						   email);
		}
		else {
			addritem_free_item_email(email);
			email = NULL;
		}
	}
}

/**
 * Parse list of email address for person from XML file.
 * \param book   Address book.
 * \param file   XML file handle.
 * \param person Person.
 */
static void addrbook_parse_addr_list(AddressBookFile *book, XMLFile *file, 
				     ItemPerson *person)
{
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file)) {
			longjmp(book->jumper, 1);
		}
		if (file->level < prev_level) return;
		if (xml_compare_tag(file, AB_ELTAG_ADDRESS)) {
			addrbook_parse_address(book, file, person);
			addrbook_parse_addr_list(book, file, person);
		}
	}
}

/**
 * Parse attribute for person from XML file.
 * \param book   Address book.
 * \param file   XML file handle.
 * \param person Person.
 */
static void addrbook_parse_attribute(XMLFile *file, ItemPerson *person)
{
	GList *attr;
	gchar *name, *value;
	gchar *element;
	UserAttribute *uAttr = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (!uAttr) uAttr = addritem_create_attribute();
		if (strcmp(name, AB_ATTAG_UID) == 0)
			addritem_attrib_set_id(uAttr, value);
		else if (strcmp(name, AB_ATTAG_NAME) == 0) 
			addritem_attrib_set_name(uAttr, value);
		attr = g_list_next(attr);
	}

	element = xml_get_element(file);
	addritem_attrib_set_value(uAttr, element);
	g_free(element);

	if (uAttr) {
		if (person) {
			addritem_person_add_attribute(person, uAttr);
		}
		else {
			addritem_free_attribute(uAttr);
			uAttr = NULL;
		}
	}
}

/**
 * Parse list of attributes for person from XML file.
 * \param book   Address book.
 * \param file   XML file handle.
 * \param person Person.
 */
static void addrbook_parse_attr_list(AddressBookFile *book, XMLFile *file, 
				     ItemPerson *person)
{
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file)) {
			longjmp( book->jumper, 1 );
		}
		if (file->level < prev_level) return;
		if (xml_compare_tag(file, AB_ELTAG_ATTRIBUTE)) {
			addrbook_parse_attribute(file, person);
			addrbook_parse_attr_list(book, file, person);
		}
	}
}

/**
 * Parse person from XML file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_parse_person(AddressBookFile *book, XMLFile *file)
{
	GList *attr;
	gchar *name, *value;
	ItemPerson *person = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (!person) 
			person = addritem_create_item_person();
		if (strcmp(name, AB_ATTAG_UID) == 0) {
			ADDRITEM_ID(person) = g_strdup(value);
			person->picture = g_strdup(value);
		}
		else if (strcmp(name, AB_ATTAG_FIRST_NAME) == 0)
			person->firstName = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_LAST_NAME) == 0)
			person->lastName = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_NICK_NAME) == 0)
			person->nickName = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_COMMON_NAME) == 0)
			ADDRITEM_NAME(person) = g_strdup(value);
		attr = g_list_next(attr);
	}
	if (xml_parse_next_tag(file)) {	/* Consume closing tag */
		longjmp(book->jumper, 1);
	}
	if (xml_compare_tag(file, AB_ELTAG_ADDRESS_LIST)) {
		addrbook_parse_addr_list(book, file, person);
		if (person) {
			addrcache_hash_add_person(book->addressCache, person);
		}
	}
	if (xml_parse_next_tag(file)) {	/* Consume closing tag */
		longjmp(book->jumper, 1);
	}
	if (xml_compare_tag(file, AB_ELTAG_ATTRIBUTE_LIST)) {
		addrbook_parse_attr_list(book, file, person);
	}
}

/**
 * Parse group member from XML file.
 * \param book  Address book.
 * \param file  XML file handle.
 * \param group Group.
 */
static void addrbook_parse_member(AddressBookFile *book, XMLFile *file, 
				  ItemGroup *group)
{
	GList *attr;
	gchar *name, *value;
	gchar *eid = NULL;
	/* gchar *pid = NULL; */
	ItemEMail *email = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if( strcmp( name, AB_ATTAG_EID ) == 0 )
			eid = g_strdup( value );
		attr = g_list_next(attr);
	}
	/* email = addrcache_get_email( book->addressCache, pid, eid ); */
	email = addrcache_get_email(book->addressCache, eid);
	g_free(eid);
	if (email) {
		if (group) {
			addrcache_group_add_email(book->addressCache, group, 
						  email);
		}
		else {
			addritem_free_item_email(email);
			email = NULL;
		}
	}
}

/**
 * Parse list of group members from XML file.
 * \param book  Address book.
 * \param file  XML file handle.
 * \param group Group.
 */
static void addrbook_parse_member_list(AddressBookFile *book, XMLFile *file, 
				       ItemGroup *group)
{
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file)) {
			longjmp(book->jumper, 1);
		}
		if (file->level < prev_level)
			return;
		if (xml_compare_tag(file, AB_ELTAG_MEMBER)) {
			addrbook_parse_member(book, file, group);
			addrbook_parse_member_list(book, file, group);
		}
	}
}

/**
 * Parse group object from XML file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_parse_group(AddressBookFile *book, XMLFile *file)
{
	GList *attr;
	gchar *name, *value;
	ItemGroup *group = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (!group) 
			group = addritem_create_item_group();
		if (strcmp(name, AB_ATTAG_UID) == 0) 
			ADDRITEM_ID(group) = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_NAME) == 0)
			ADDRITEM_NAME(group) = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_REMARKS) == 0)
			group->remarks = g_strdup(value);
		attr = g_list_next(attr);
	}
	if (xml_parse_next_tag(file)) {	/* Consume closing tag */
		longjmp(book->jumper, 1);
	}
	if (xml_compare_tag(file, AB_ELTAG_MEMBER_LIST)) {
		if (group) {
			addrcache_hash_add_group(book->addressCache, group);
		}
		addrbook_parse_member_list(book, file, group);
	}
}

/**
 * Parse folder item from XML file.
 * \param book   Address book.
 * \param file   XML file handle.
 * \param folder Folder.
 */
static void addrbook_parse_folder_item(AddressBookFile *book, XMLFile *file, 
				       ItemFolder *folder)
{
	GList *attr;
	gchar *name, *value;
	gchar *uid = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (strcmp(name, AB_ATTAG_UID) == 0) {
			uid = g_strdup(value);
		}
		attr = g_list_next(attr);
	}
	if (folder) {
		if (uid) {
			folder->listItems = g_list_append(folder->listItems, uid);
		}
	}
}

/**
 * Parse list of folder items from XML file.
 * \param book   Address book.
 * \param file   XML file handle.
 * \param folder Folder.
 */
static void addrbook_parse_folder_list(AddressBookFile *book, XMLFile *file,
				       ItemFolder *folder)
{
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file)) {
			longjmp(book->jumper, 1);
		}
		if (file->level < prev_level)
			return;
		if (xml_compare_tag(file, AB_ELTAG_ITEM)) {
			addrbook_parse_folder_item(book, file, folder);
			addrbook_parse_folder_list(book, file, folder);
		}
	}
}

/**
 * Parse folder from XML file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_parse_folder(AddressBookFile *book, XMLFile *file) 
{
	GList *attr;
	gchar *name, *value;
	ItemFolder *folder = NULL;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (!folder)
			folder = addritem_create_item_folder();
		if (strcmp(name, AB_ATTAG_UID) == 0)
			ADDRITEM_ID(folder) = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_NAME) == 0)
			ADDRITEM_NAME(folder) = g_strdup(value);
		else if (strcmp(name, AB_ATTAG_REMARKS) == 0)
			folder->remarks = g_strdup(value);
		attr = g_list_next(attr);
	}
	if (xml_parse_next_tag(file)) {	/* Consume closing tag */
		longjmp(book->jumper, 1);
	}
	if (xml_compare_tag(file, AB_ELTAG_ITEM_LIST)) {
		if (folder) {
			if (addrcache_hash_add_folder(book->addressCache, 
						      folder)) {
				book->tempList = g_list_append(book->tempList, 
							       folder);
				/* We will resolve folder later */
				ADDRITEM_PARENT(folder) = NULL;
			}
		}
		addrbook_parse_folder_list(book, file, folder);
	}
}

/**
 * Read address book (DOM) tree from file.
 * \param  book Address book.
 * \param  file XML file handle.
 * \return <i>TRUE</i> if data read successfully, <i>FALSE</i> if error
 *         reading data.
 */
static gboolean addrbook_read_tree(AddressBookFile *book, XMLFile *file)
{
	gboolean retVal;
	GList *attr;
	gchar *name, *value;

	book->retVal = MGU_BAD_FORMAT;
	if (xml_get_dtd(file))
		return FALSE;
	if (xml_parse_next_tag(file))
		longjmp(book->jumper, 1);
	if (!xml_compare_tag(file, AB_ELTAG_ADDRESS_BOOK))
		return FALSE;

	attr = xml_get_current_tag_attr(file);
	while (attr) {
		name = ((XMLAttr *)attr->data)->name;
		value = ((XMLAttr *)attr->data)->value;
		if (strcmp( name, AB_ATTAG_NAME) == 0)
			addrbook_set_name( book, value );
		attr = g_list_next( attr );
	}

	retVal = TRUE;
	for (;;) {
		if (!file->level)
			break;
		/* Get next item tag (person, group or folder) */
		if (xml_parse_next_tag(file))
			longjmp( book->jumper, 1 );
			
		if (xml_compare_tag(file, AB_ELTAG_PERSON))
			addrbook_parse_person(book, file);
		else if (xml_compare_tag(file, AB_ELTAG_GROUP))
			addrbook_parse_group(book, file);
		else if (xml_compare_tag(file, AB_ELTAG_FOLDER))
			addrbook_parse_folder(book, file);
	}
	if (retVal) book->retVal = MGU_SUCCESS;
		return retVal;
}

/**
 * Resolve folder items callback function.
 * \param key   Table key.
 * \param value Reference to object contained in folder.
 * \param data  Reference to address book.
 */
static void addrbook_res_items_vis(gpointer key, gpointer value, gpointer data)
{
	AddressBookFile *book = data;
	AddrItemObject *obj = (AddrItemObject *) value;
	ItemFolder *rootFolder = book->addressCache->rootFolder;
	if (obj->parent == NULL) {
		if (ADDRITEM_TYPE(obj) == ITEMTYPE_PERSON) {
			rootFolder->listPerson = g_list_append(rootFolder->listPerson,
							       obj);
			ADDRITEM_PARENT(obj) = ADDRITEM_OBJECT(rootFolder);
		}
		else if (ADDRITEM_TYPE(obj) == ITEMTYPE_GROUP) {
			rootFolder->listGroup = g_list_append(rootFolder->listGroup,
							      obj);
			ADDRITEM_PARENT(obj) = ADDRITEM_OBJECT(rootFolder);
		}
	}
}

/**
 * Resolve folder items. Lists of UID's are replaced with pointers to
 * data items.
 * \param  book Address book.
 */
static void addrbook_resolve_folder_items(AddressBookFile *book)
{
	GList *nodeFolder = NULL;
	GList *listRemove = NULL;
	GList *node = NULL;
	ItemFolder *rootFolder = book->addressCache->rootFolder;
	nodeFolder = book->tempList;
	
	while (nodeFolder) {
		ItemFolder *folder = nodeFolder->data;
		listRemove = NULL;
		node = folder->listItems;
		while (node) {
			gchar *uid = node->data;
			AddrItemObject *aio = addrcache_get_object(book->addressCache, 
								   uid);
			if (aio) {
				if (aio->type == ITEMTYPE_FOLDER) {
					ItemFolder *item = (ItemFolder *) aio;
					folder->listFolder = g_list_append(folder->listFolder, item);
					ADDRITEM_PARENT(item) = ADDRITEM_OBJECT(folder);
					addrcache_hash_add_folder(book->addressCache, folder);
				}
				else if (aio->type == ITEMTYPE_PERSON) {
					ItemPerson *item = (ItemPerson *) aio;
					folder->listPerson = g_list_append(folder->listPerson, item);
					ADDRITEM_PARENT(item) = ADDRITEM_OBJECT(folder);
				}
				else if (aio->type == ITEMTYPE_GROUP) {
					ItemGroup *item = (ItemGroup *) aio;
					folder->listGroup = g_list_append(folder->listGroup, item);
					ADDRITEM_PARENT(item) = ADDRITEM_OBJECT(folder);
				}
				/* Replace data with pointer to item */
				g_free(uid);
				node->data = aio;
			}
			else { /* Not found, append to remove list. */
				listRemove = g_list_append(listRemove, uid);
			}
			node = g_list_next(node);
		}
		rootFolder->listFolder = g_list_append(rootFolder->listFolder, 
						       folder);
		/* Process remove list */
		node = listRemove;
		while (node) {
			gchar *uid = node->data;
			folder->listItems = g_list_remove(folder->listItems,
							  uid);
			g_free(uid);
			node = g_list_next(node);
		}
		g_list_free(listRemove);
		nodeFolder = g_list_next(nodeFolder);
	}
	/* Remove folders with parents. */
	listRemove = NULL;
	node = rootFolder->listFolder;
	while (node) {
		ItemFolder *folder = (ItemFolder *) node->data;
		if (ADDRITEM_PARENT(folder))
			/* Remove folders with parents */
			listRemove = g_list_append(listRemove, folder);
		else /* Add to root folder */
			ADDRITEM_PARENT(folder) = ADDRITEM_OBJECT(book->addressCache->rootFolder);

		node = g_list_next( node );
	}
	/* Process remove list */
	node = listRemove;
	while (node) {
		rootFolder->listFolder = g_list_remove(rootFolder->listFolder, 
						       node->data);
		node = g_list_next(node);
	}
	g_list_free(listRemove);

	/* Move all unparented persons and groups into root folder */
	g_hash_table_foreach(book->addressCache->itemHash, 
			     addrbook_res_items_vis, book);

	/* Free up some more */
	nodeFolder = book->tempList;
	while (nodeFolder) {
		ItemFolder *folder = nodeFolder->data;
		g_list_free(folder->listItems);
		folder->listItems = NULL;
		nodeFolder = g_list_next(nodeFolder);
	}
	g_list_free(book->tempList);
	book->tempList = NULL;
}

/**
 * Read address book.
 * \param  book Address book.
 * \return Status code.
 */
gint addrbook_read_data(AddressBookFile *book)
{
	XMLFile *file = NULL;
	gchar *fileSpec = NULL;

	cm_return_val_if_fail(book != NULL, -1);

	/*
	g_print( "...addrbook_read_data :%s:\t:%s:\n", book->fileName,
		addrcache_get_name( book->addressCache ) );
	*/

	fileSpec = g_strconcat(book->path, G_DIR_SEPARATOR_S, 
			       book->fileName, NULL);
	book->retVal = MGU_OPEN_FILE;
	addrcache_clear(book->addressCache);
	book->addressCache->modified = FALSE;
	book->addressCache->accessFlag = FALSE;
	file = xml_open_file(fileSpec);
	g_free(fileSpec);
	if (file) {
		book->tempList = NULL;
		/* Trap for parsing errors. */
		if (setjmp( book->jumper)) {
			xml_close_file(file);
			return book->retVal;
		}
		addrbook_read_tree(book, file);
		xml_close_file(file);
		/* Resolve folder items */
		addrbook_resolve_folder_items(book);
		book->tempList = NULL;
		book->addressCache->modified = FALSE;
		book->addressCache->dataRead = TRUE;
		addrcache_set_dirty(book->addressCache, FALSE);
	}
	return book->retVal;
}

/**
 * Write start element to file.
 * \param fp   File handle.
 * \param lvl  Indent level.
 * \param name Element name.
 */
static int addrbook_write_elem_s(FILE *fp, gint lvl, gchar *name)
{
	gint i;
	for (i = 0; i < lvl; i++) 
		if (fputs("  ", fp) == EOF)
			return -1;
	if (fputs("<", fp) == EOF)
		return -1;
	if (fputs(name, fp) == EOF)
		return -1;
		
	return 0;
}

/**
 * Write end element to file.
 * \param fp   File handle.
 * \param lvl  Indent level.
 * \param name Element name.
 */
static int addrbook_write_elem_e(FILE *fp, gint lvl, gchar *name)
{
	gint i;
	for(i = 0; i < lvl; i++)
		if (fputs("  ", fp) == EOF)
			return -1;
	if (fputs("</", fp) == EOF)
		return -1;
	if (fputs(name, fp) == EOF)
		return -1;
	if (fputs(">\n", fp) == EOF)
		return -1;
		
	return 0;
}

/**
 * Write attribute name/value pair to file.
 * \param fp    File handle.
 * \param name  Attribute name.
 * \param value Attribute value.
 */
static int addrbook_write_attr(FILE *fp, gchar *name, gchar *value)
{
	if (fputs(" ", fp) == EOF)
		return -1;
	if (fputs(name, fp) == EOF)
		return -1;
	if (fputs("=\"", fp) == EOF)
		return -1;
	if (xml_file_put_escape_str(fp, value) < 0)
		return -1;
	if (fputs("\"", fp) == EOF)
		return -1;
	
	return 0;
}

typedef struct _HashLoopData {
	FILE *fp;
	gboolean error;
} HashLoopData;

/**
 * Write person and associated addresses and attributes to file.
 * file hash table visitor function.
 * \param key   Table key.
 * \param value Reference to person.
 * \param data  File pointer.
 */
static void addrbook_write_item_person_vis(gpointer key, gpointer value, 
					   gpointer d)
{
	AddrItemObject *obj = (AddrItemObject *) value;
	HashLoopData *data = (HashLoopData *)d;
	FILE *fp = data->fp;
	GList *node;

	if (!obj)
		return;
	if (ADDRITEM_TYPE(obj) == ITEMTYPE_PERSON) {
		ItemPerson *person = (ItemPerson *) value;
		if (person) {
			if (addrbook_write_elem_s(fp, 1, AB_ELTAG_PERSON) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(person)) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_FIRST_NAME, person->firstName) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_LAST_NAME, person->lastName) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_NICK_NAME, person->nickName) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_COMMON_NAME, ADDRITEM_NAME(person)) < 0)
				data->error = TRUE;
			if (fputs(" >\n", fp) == EOF)
				data->error = TRUE;

			/* Output email addresses */
			if (addrbook_write_elem_s(fp, 2, AB_ELTAG_ADDRESS_LIST) < 0)
				data->error = TRUE;
			if (fputs(">\n", fp) == EOF)
				data->error = TRUE;
			node = person->listEMail;
			while (node) {
				ItemEMail *email = node->data;
				if (addrbook_write_elem_s(fp, 3, AB_ELTAG_ADDRESS) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(email)) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_ALIAS, ADDRITEM_NAME(email)) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_EMAIL, email->address) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_REMARKS, email->remarks) < 0)
					data->error = TRUE;
				if (fputs(" />\n", fp) == EOF)
					data->error = TRUE;
				node = g_list_next(node);
			}
			if (addrbook_write_elem_e(fp, 2, AB_ELTAG_ADDRESS_LIST) < 0)
				data->error = TRUE;

			/* Output user attributes */
			if (addrbook_write_elem_s(fp, 2, AB_ELTAG_ATTRIBUTE_LIST) < 0)
				data->error = TRUE;
			if (fputs(">\n", fp) == EOF)
				data->error = TRUE;
			node = person->listAttrib;
			while (node) {
				UserAttribute *attrib = node->data;
				if (addrbook_write_elem_s(fp, 3, AB_ELTAG_ATTRIBUTE) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_UID, attrib->uid) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_NAME, attrib->name) < 0)
					data->error = TRUE;
				if (fputs(" >", fp) == EOF)
					data->error = TRUE;
				if (xml_file_put_escape_str(fp, attrib->value) < 0)
					data->error = TRUE;
				if (addrbook_write_elem_e(fp, 0, AB_ELTAG_ATTRIBUTE) < 0)
					data->error = TRUE;
				node = g_list_next(node);
			}
			if (addrbook_write_elem_e(fp, 2, AB_ELTAG_ATTRIBUTE_LIST) < 0)
				data->error = TRUE;
			if (addrbook_write_elem_e(fp, 1, AB_ELTAG_PERSON) < 0)
				data->error = TRUE;
		}
	}
}

/**
 * Write group and associated references to addresses to file.
 * file hash table visitor function.
 * \param key   Table key.
 * \param value Reference to group.
 * \param data  File pointer.
 */
static void addrbook_write_item_group_vis(gpointer key, gpointer value, 
					  gpointer d)
{
	AddrItemObject *obj = (AddrItemObject *) value;
	HashLoopData *data = (HashLoopData *)d;
	FILE *fp = data->fp;

	GList *node;

	if (!obj)
		return;
	if (ADDRITEM_TYPE(obj) == ITEMTYPE_GROUP) {
		ItemGroup *group = (ItemGroup *) value;
		if (group) {
			if (addrbook_write_elem_s(fp, 1, AB_ELTAG_GROUP) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(group)) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_NAME, ADDRITEM_NAME(group)) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_REMARKS, group->remarks) < 0)
				data->error = TRUE;
			if (fputs(" >\n", fp) == EOF)
				data->error = TRUE;

			/* Output email address links */
			if (addrbook_write_elem_s(fp, 2, AB_ELTAG_MEMBER_LIST) < 0)
				data->error = TRUE;
			if (fputs(">\n", fp) == EOF)
				data->error = TRUE;
			node = group->listEMail;
			while (node) {
				ItemEMail *email = node->data;
				ItemPerson *person = (ItemPerson *) ADDRITEM_PARENT(email);
				if (addrbook_write_elem_s(fp, 3, AB_ELTAG_MEMBER) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_PID, ADDRITEM_ID(person)) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_EID, ADDRITEM_ID(email)) < 0)
					data->error = TRUE;
				if (fputs(" />\n", fp) == EOF)
					data->error = TRUE;
				node = g_list_next(node);
			}
			if (addrbook_write_elem_e(fp, 2, AB_ELTAG_MEMBER_LIST) < 0)
				data->error = TRUE;
			if (addrbook_write_elem_e(fp, 1, AB_ELTAG_GROUP) < 0)
				data->error = TRUE;
		}
	}
}

/**
 * Write folder and associated references to addresses to file.
 * file hash table visitor function.
 * \param key   Table key.
 * \param value Reference to folder.
 * \param data  File pointer.
 */
static void addrbook_write_item_folder_vis(gpointer key, gpointer value, 
					   gpointer d)
{
	AddrItemObject *obj = (AddrItemObject *) value;
	HashLoopData *data = (HashLoopData *)d;
	FILE *fp = data->fp;
	GList *node;

	if (!obj)
		return;
	if (ADDRITEM_TYPE(obj) == ITEMTYPE_FOLDER) {
		ItemFolder *folder = (ItemFolder *) value;
		if (folder) {
			if (addrbook_write_elem_s(fp, 1, AB_ELTAG_FOLDER) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(folder)) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_NAME, ADDRITEM_NAME(folder)) < 0)
				data->error = TRUE;
			if (addrbook_write_attr(fp, AB_ATTAG_REMARKS, folder->remarks) < 0)
				data->error = TRUE;
			if (fputs(" >\n", fp) == EOF)
				data->error = TRUE;
			if (addrbook_write_elem_s(fp, 2, AB_ELTAG_ITEM_LIST) < 0)
				data->error = TRUE;
			if (fputs(">\n", fp) == EOF)
				data->error = TRUE;

			/* Output persons */
			node = folder->listPerson;
			while (node) {
				ItemPerson *item = node->data;
				if (addrbook_write_elem_s(fp, 3, AB_ELTAG_ITEM) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_TYPE,  AB_ATTAG_VAL_PERSON) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(item)) < 0)
					data->error = TRUE;
				if (fputs(" />\n", fp) == EOF)
					data->error = TRUE;
				node = g_list_next(node);
			}

			/* Output groups */
			node = folder->listGroup;
			while (node) {
				ItemGroup *item = node->data;
				if (addrbook_write_elem_s(fp, 3, AB_ELTAG_ITEM) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_TYPE, AB_ATTAG_VAL_GROUP) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(item)) < 0)
					data->error = TRUE;
				if (fputs(" />\n", fp) == EOF)
					data->error = TRUE;
				node = g_list_next(node);
			}

			/* Output folders */
			node = folder->listFolder;
			while (node) {
				ItemFolder *item = node->data;
				if (addrbook_write_elem_s(fp, 3, AB_ELTAG_ITEM) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_TYPE, AB_ATTAG_VAL_FOLDER) < 0)
					data->error = TRUE;
				if (addrbook_write_attr(fp, AB_ATTAG_UID, ADDRITEM_ID(item)) < 0)
					data->error = TRUE;
				if (fputs(" />\n", fp) == EOF)
					data->error = TRUE;
				node = g_list_next(node);
			}
			if (addrbook_write_elem_e(fp, 2, AB_ELTAG_ITEM_LIST) < 0)
				data->error = TRUE;
			if (addrbook_write_elem_e(fp, 1, AB_ELTAG_FOLDER) < 0)
				data->error = TRUE;
		}
	}
}

/**
 * Output address book data to specified file.
 * \param  book Address book.
 * \param  newFile Filename of new file (in book's filepath).
 * \return Status code.
 */
static gint addrbook_write_to(AddressBookFile *book, gchar *newFile)
{
	FILE *fp;
	gchar *fileSpec;
	HashLoopData data;
#ifndef DEV_STANDALONE
	PrefFile *pfile;
#endif

	cm_return_val_if_fail(book != NULL, -1);
	cm_return_val_if_fail(newFile != NULL, -1);

	fileSpec = g_strconcat(book->path, G_DIR_SEPARATOR_S, newFile, NULL);

	book->retVal = MGU_OPEN_FILE;
#ifdef DEV_STANDALONE
	fp = g_fopen(fileSpec, "wb");
	g_free(fileSpec);
	if (fp) {
		if (fputs("<?xml version=\"1.0\" ?>\n", fp) == EOF) {
			book->retVal = MGU_ERROR_WRITE;
			return book->retVal;
		}
#else
	pfile = prefs_write_open(fileSpec);
	g_free(fileSpec);
	if (pfile) {
		fp = pfile->fp;
		if (fprintf( fp, "<?xml version=\"1.0\" encoding=\"%s\" ?>\n", CS_INTERNAL ) < 0)
			goto fail;
#endif
		if (addrbook_write_elem_s(fp, 0, AB_ELTAG_ADDRESS_BOOK) < 0)
			goto fail;
		if (addrbook_write_attr(fp, AB_ATTAG_NAME,
				    addrcache_get_name(book->addressCache)) < 0)
			goto fail;
		if (fputs(" >\n", fp) == EOF)
			goto fail;

		/* Output all persons */
		data.fp = fp;
		data.error = FALSE;

		g_hash_table_foreach(book->addressCache->itemHash, 
				     addrbook_write_item_person_vis, &data);
		if (data.error)
			goto fail;

		/* Output all groups */
		g_hash_table_foreach(book->addressCache->itemHash, 
				     addrbook_write_item_group_vis, &data);

		if (data.error)
			goto fail;

		/* Output all folders */
		g_hash_table_foreach(book->addressCache->itemHash, 
				     addrbook_write_item_folder_vis, &data);

		if (data.error)
			goto fail;

		if (addrbook_write_elem_e(fp, 0, AB_ELTAG_ADDRESS_BOOK) < 0)
			goto fail;

		book->retVal = MGU_SUCCESS;
#ifdef DEV_STANDALONE
		fclose(fp);
#else
		if (prefs_file_close( pfile ) < 0)
			book->retVal = MGU_ERROR_WRITE;
#endif
	}

	fileSpec = NULL;
	return book->retVal;
fail:
	g_warning("error writing AB\n");
	book->retVal = MGU_ERROR_WRITE;
	if (pfile)
		prefs_file_close_revert( pfile );
	return book->retVal;
}

/**
 * Output address book data to original file.
 * \param  book Address book.
 * \return Status code.
 */
gint addrbook_save_data(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, -1);

	book->retVal = MGU_NO_FILE;
	if (book->fileName == NULL || *book->fileName == '\0') 
		return book->retVal;
	if (book->path == NULL || *book->path == '\0')
		return book->retVal;

	addrbook_write_to(book, book->fileName);
	if (book->retVal == MGU_SUCCESS)
		addrcache_set_dirty(book->addressCache, FALSE);
	return book->retVal;
}

/*
 * **********************************************************************
 * Address book edit interface functions.
 * **********************************************************************
 */

/**
 * Hash table callback function for simple deletion of hashtable entries.
 * \param  key   Table key (will be freed).
 * \param  value Value stored in table.
 * \param  data  User data.
 * \return <i>TRUE</i> to indicate that entry freed.
 */
static gboolean addrbook_free_simple_hash_vis(gpointer *key, gpointer *value, 
					      gpointer *data)
{
	g_free(key);
	key = NULL;
	value = NULL;
	return TRUE;
}

/**
 * Update address book email list for specified person. Note: The existing
 * email addresses are replaced with the new addresses. Any references to
 * old addresses in the groups are re-linked to the new addresses. All old
 * addresses linked to the person are removed.
 * \param book      Address book.
 * \param person    Person to update.
 * \param listEMail List of new email addresses.
 */
void addrbook_update_address_list(AddressBookFile *book, ItemPerson *person, 
				  GList *listEMail)
{
	GList *node;
	GList *listDelete;
	GList *listGroup;

	cm_return_if_fail(book != NULL);
	cm_return_if_fail(person != NULL);

	/* Get groups where person's existing email addresses are listed */
	listGroup = addrcache_get_group_for_person(book->addressCache, person);
	if (listGroup) {
		GHashTable *hashEMail;
		GHashTable *hashEMailAlias;
		GList *nodeGrp;

		/* Load hash table with new address entries */
		hashEMail = g_hash_table_new(g_str_hash, g_str_equal);
		hashEMailAlias = g_hash_table_new(g_str_hash, g_str_equal);
	   	node = listEMail;
		while (node) {
			ItemEMail *email = node->data;
			gchar *alias = email->obj.name ;
			gchar *addr = g_utf8_strdown(email->address, -1);
			if (!g_hash_table_lookup(hashEMail, addr)) {
				g_hash_table_insert(hashEMail, addr, email);
			}
			if (*alias != '\0' && ! g_hash_table_lookup(hashEMailAlias,
			    alias)) 
				g_hash_table_insert(hashEMailAlias, alias, email);

			node = g_list_next(node);
		}

		/* Re-parent new addresses to existing groups, where email address match. */
		nodeGrp = listGroup;
		while (nodeGrp) {
			ItemGroup *group = (ItemGroup *) nodeGrp->data;
			GList *groupEMail = group->listEMail;
			GList *nodeGrpEM;
			GList *listRemove = NULL;

			/* Process each email item linked to group */
			nodeGrpEM = groupEMail;
			while (nodeGrpEM) {
				ItemEMail *emailGrp = (ItemEMail *) nodeGrpEM->data;

				if (ADDRITEM_PARENT(emailGrp) == ADDRITEM_OBJECT(person)) {
					/* Found an email address for this person */
					ItemEMail *emailNew = NULL;
					gchar *alias = emailGrp->obj.name;
					gchar *addr = g_utf8_strdown(emailGrp->address, -1);
					emailNew = (ItemEMail *)
						g_hash_table_lookup(hashEMail, addr);
					g_free( addr );
					/* If no match by e-mail, try to match by e-mail alias */
					if (!emailNew && *alias != '\0') {
						emailNew = (ItemEMail *)
							g_hash_table_lookup(hashEMailAlias, alias);
					}
					
					if (emailNew)
						/* Point to this entry */
						nodeGrpEM->data = emailNew;
					else if (g_hash_table_size(hashEMail)==1)
						/* If the person has just one e-mail address, then 
						   change e-mail address in group list */
						nodeGrpEM->data = listEMail->data;
					else 
						/* Mark for removal */
						listRemove = g_list_append(listRemove, emailGrp);
				}
				/* Move on to next email link */
				nodeGrpEM = g_list_next(nodeGrpEM);
			}

			/* Process all removed links in current group */
			nodeGrpEM = listRemove;
			while (nodeGrpEM) {
				ItemEMail *emailGrp = nodeGrpEM->data;
				groupEMail = g_list_remove(groupEMail, emailGrp);
				nodeGrpEM = g_list_next(nodeGrpEM);
			}

			g_list_free(listRemove);

			/* Move on to next group */
			nodeGrp = g_list_next(nodeGrp);

		}
		/* Clear hash table */
		g_hash_table_foreach_remove(hashEMail, (GHRFunc) 
					    addrbook_free_simple_hash_vis, NULL);
		g_hash_table_destroy(hashEMail);
		hashEMail = NULL;
		g_hash_table_destroy(hashEMailAlias);
		hashEMailAlias = NULL;
		g_list_free(listGroup);
		listGroup = NULL;
	}
	/* Remove old addresses from person and cache */
	listDelete = NULL;
	node = person->listEMail;
	while (node) {
		ItemEMail *email = node->data;

		if (addrcache_person_remove_email(book->addressCache, person, email))
			addrcache_remove_email(book->addressCache, email);

		listDelete = g_list_append(listDelete, email);
		node = person->listEMail;
	}
	/* Add new address entries */
   	node = listEMail;
	while (node) {
		ItemEMail *email = node->data;

		if (ADDRITEM_ID(email) == NULL)
			/* Allocate an ID for new address */
			addrcache_id_email(book->addressCache, email);
			
		addrcache_person_add_email( book->addressCache, person, email );
		node = g_list_next( node );
	}

	addrcache_set_dirty(book->addressCache, TRUE);

	/* Free up memory */
	g_list_free(listEMail);
	listEMail = NULL;

	node = listDelete;
	while (node) {
		ItemEMail *email = node->data;

		addritem_free_item_email(email);
		node = g_list_next(node);
	}
	g_list_free(listDelete);
	listDelete = NULL;

}

/**
 * Create person object and add person with specified address data to address
 * book. Note: A new person is created with specified list of email addresses.
 * All objects inserted into address book.
 *
 * \param  book      Address book.
 * \param  folder    Parent folder where to add person, or <i>NULL</i> for
 *                   root folder.
 * \param  listEMail List of new email addresses to associate with person.
 * \return Person object created.
 */
ItemPerson *addrbook_add_address_list(AddressBookFile *book, ItemFolder *folder,
				      GList *listEMail)
{
	ItemPerson *person;
	ItemFolder *f = folder;
	GList *node;

	cm_return_val_if_fail(book != NULL, NULL);

	if (!f) 
		f = book->addressCache->rootFolder;
	person = addritem_create_item_person();
	addrcache_id_person(book->addressCache, person);
	addrcache_folder_add_person(book->addressCache, f, person);

   	node = listEMail;
	while (node) {
		ItemEMail *email = node->data;
		if (ADDRITEM_ID(email) == NULL)
			addrcache_id_email(book->addressCache, email);

		addrcache_person_add_email(book->addressCache, person, email);
		node = g_list_next(node);
	}
	return person;
}

/**
 * Build available email list visitor function.
 * \param  key   Table key.
 * \param  value Value stored in table.
 * \param  data  Reference to address book.
 */
static void addrbook_build_avail_email_vis(gpointer key, gpointer value, 
					   gpointer data)
{
	AddrItemObject *obj = (AddrItemObject *) value;

	if (ADDRITEM_TYPE(obj) == ITEMTYPE_PERSON) {
		AddressBookFile *book = data;
		ItemPerson *person = (ItemPerson *) obj;
		GList *node = person->listEMail;
		while (node) {
			ItemEMail *email = node->data;
			/* gchar *newKey = g_strdup( ADDRITEM_ID(email) ); */

			if (!g_hash_table_lookup(book->tempHash,
					 	 ADDRITEM_ID(email)))
				book->tempList = g_list_append(book->tempList, email);

			node = g_list_next(node);
		}
	}
}

/**
 * Return link list of available email items that have not already been linked
 * to groups. Note that the list contains references to items and should be
 * <code>g_free()</code> when done. Do <b>*NOT*</b> attempt to used the
 * <code>addrbook_free_xxx()<code> functions... this will destroy the
 * addressbook data!
 *
 * \param  book  Address book.
 * \param  group Group to process.
 * \return List of items, or <i>NULL</i> if none.
 */
GList *addrbook_get_available_email_list(AddressBookFile *book, ItemGroup *group)
{
	GList *list = NULL;
	GHashTable *table;

	cm_return_val_if_fail(book != NULL, NULL);

	/* Load hash table with group email entries */
	table = g_hash_table_new(g_str_hash, g_str_equal);
	if (group) {
		list = group->listEMail;
		while (list) {
			ItemEMail *email = list->data;
			g_hash_table_insert(table, ADDRITEM_ID(email), email);
			list = g_list_next(list);
		}
	}

	/* Build list of available email addresses which exclude those already in groups */
	book->tempList = NULL;
	book->tempHash = table;
	g_hash_table_foreach(book->addressCache->itemHash, 
			     addrbook_build_avail_email_vis, book);
	list = book->tempList;
	book->tempList = NULL;
	book->tempHash = NULL;

	/* Clear hash table */
	g_hash_table_destroy(table);
	table = NULL;

	return list;
}

/**
 * Update address book email list for specified group. Note: The existing email
 * addresses are replaced with the new addresses. Any references to old addresses
 * in the groups are re-linked to the new addresses. All old addresses linked to
 * the person are removed.
 *
 * \param book      Address book.
 * \param group     Group to process.
 * \param listEMail List of email items. This should <b>*NOT*</b> be
 *                  <code>g_free()</code> when done.
 */
void addrbook_update_group_list(AddressBookFile *book, ItemGroup *group, 
				GList *listEMail)
{
	GList *oldData;

	cm_return_if_fail(book != NULL);
	cm_return_if_fail(group != NULL);

	addrcache_set_dirty(book->addressCache, TRUE);

	/* Remember old list */
	oldData = group->listEMail;
	group->listEMail = listEMail;
	mgu_clear_list(oldData);
	oldData = NULL;
}

/**
 * Create group object and add with specifed list of email addresses to
 * address book. Note: The existing email addresses are replaced with the new
 * addresses. Any references to old addresses in the groups are re-linked to
 * the new addresses. All old addresses linked to the person are removed.
 *
 * \param  book      Address book.
 * \param  folder    Parent folder where to add group, or <i>NULL</i> for
 *                   root folder.
 * \param  listEMail List of email items. This should <b>*NOT*</b> be
 *                  <code>g_free()</code> when done.
 * \return Group object created.
 */
ItemGroup *addrbook_add_group_list(AddressBookFile *book, ItemFolder *folder,
				   GList *listEMail)
{
	ItemGroup *group = NULL;
	ItemFolder *f = folder;

	cm_return_val_if_fail(book != NULL, NULL);

	if (!f)
		f = book->addressCache->rootFolder;
	group = addritem_create_item_group();
	addrcache_id_group(book->addressCache, group);
	addrcache_folder_add_group(book->addressCache, f, group);
	group->listEMail = listEMail;
	return group;
}

/**
 * Create a new folder and add to address book.
 * \param  book   Address book.
 * \param  folder Parent folder where to add folder, or <i>NULL</i> for
 *                root folder.
 * \return Folder that was created. This should <b>*NOT*</b> be
 *         <code>g_free()</code> when done.
 */
ItemFolder *addrbook_add_new_folder(AddressBookFile *book, ItemFolder *parent)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_add_new_folder( book->addressCache, parent );
}

/**
 * Update address book attribute list for specified person. Note: The existing
 * attributes are replaced with the new addresses. All old attributes linked
 * to the person are removed.
 *
 * \param book       Address book.
 * \param person     Person to receive attributes.
 * \param listAttrib New list of attributes.
 */
void addrbook_update_attrib_list(AddressBookFile *book, ItemPerson *person,
				 GList *listAttrib)
{
	GList *node;
	GList *oldData;

	cm_return_if_fail(book != NULL);
	cm_return_if_fail(person != NULL);

	/* Remember old list */
	oldData = person->listAttrib;

	/* Attach new address list to person. */
   	node = listAttrib;
	while (node) {
		UserAttribute *attrib = node->data;
		if (attrib->uid == NULL) {
			/* Allocate an ID */
			addrcache_id_attribute(book->addressCache, attrib);
		}
		node = g_list_next(node);
	}
	person->listAttrib = listAttrib;
	addrcache_set_dirty(book->addressCache, TRUE);

	/* Free up old data */
	addritem_free_list_attribute(oldData);
	oldData = NULL;
}

/**
 * Add attribute data for specified person to address book. Note: Only
 * attributes are inserted into address book.
 * \param book       Address book.
 * \param person     Person to receive attributes.
 * \param listAttrib List of attributes.
 */
void addrbook_add_attrib_list( AddressBookFile *book, ItemPerson *person, GList *listAttrib ) {
	GList *node;

	cm_return_if_fail( book != NULL );
	cm_return_if_fail( person != NULL );

   	node = listAttrib;
	while( node ) {
		UserAttribute *attrib = node->data;
		if( attrib->uid == NULL ) {
			addrcache_id_attribute( book->addressCache, attrib );
		}
		addritem_person_add_attribute( person, attrib );
		node = g_list_next( node );
	}
	addrcache_set_dirty( book->addressCache, TRUE );
}

#define WORK_BUFLEN     1024
#define ADDRBOOK_DIGITS "0123456789"

/**
 * Return list of existing address book files.
 * \param  book Address book.
 * \return List of files (as strings).
 */
GList *addrbook_get_bookfile_list(AddressBookFile *book) {
	gchar *adbookdir;
	GDir *dir;
	const gchar *dir_name;
	struct stat statbuf;
	gchar buf[WORK_BUFLEN + 1];
	gchar numbuf[WORK_BUFLEN];
	gint len, lenpre, lensuf, lennum;
	long int val, maxval;
	GList *fileList = NULL;

	cm_return_val_if_fail(book != NULL, NULL);

	if (book->path == NULL || *book->path == '\0') {
		book->retVal = MGU_NO_PATH;
		return NULL;
	}

	strncpy(buf, book->path, WORK_BUFLEN);
	len = strlen(buf);
	if (len > 0) {
		if (buf[len-1] != G_DIR_SEPARATOR) {
			buf[len] = G_DIR_SEPARATOR;
			buf[++len] = '\0';
		}
	}

	adbookdir = g_strdup(buf);
	strncat(buf, ADDRBOOK_PREFIX, WORK_BUFLEN - strlen(buf));

	if( ( dir = g_dir_open( adbookdir, 0, NULL ) ) == NULL ) {
		book->retVal = MGU_OPEN_DIRECTORY;
		g_free(adbookdir);
		return NULL;
	}

	lenpre = strlen(ADDRBOOK_PREFIX);
	lensuf = strlen(ADDRBOOK_SUFFIX);
	lennum = FILE_NUMDIGITS + lenpre;
	maxval = -1;

	while( ( dir_name = g_dir_read_name( dir ) ) != NULL ) {
		gchar *endptr = NULL;
		gint i;
		gboolean flg;

		strncpy(buf, adbookdir, WORK_BUFLEN);
		strncat(buf, dir_name, WORK_BUFLEN - strlen(buf));
		g_stat(buf, &statbuf);
		if (S_ISREG(statbuf.st_mode)) {
			if (strncmp(
				dir_name,
				ADDRBOOK_PREFIX, lenpre) == 0)
			{
				if (strncmp(
					(dir_name) + lennum,
					ADDRBOOK_SUFFIX, lensuf) == 0)
				{
					strncpy(numbuf,
						(dir_name) + lenpre,
						FILE_NUMDIGITS);
					numbuf[FILE_NUMDIGITS] = '\0';
					flg = TRUE;
					for(i = 0; i < FILE_NUMDIGITS; i++) {
						if(!strchr(ADDRBOOK_DIGITS, numbuf[i])) {
							flg = FALSE;
							break;
						}
					}
					if (flg) {
						/* Get value */
						val = strtol(numbuf, &endptr, 10);
						if (endptr  && val > -1) {
							if (val > maxval) maxval = val;
							fileList = g_list_append(
								fileList,
								g_strdup(dir_name));
						}
					}
				}
			}
		}
	}
	g_dir_close( dir );
	g_free(adbookdir);

	book->maxValue = maxval; 
	book->retVal = MGU_SUCCESS;
	return fileList;
}

/**
 * Return file name for specified file number.
 * \param  fileNum File number.
 * \return File name, or <i>NULL</i> if file number too large. Should be
 *         <code>g_free()</code> when done.
 */
gchar *addrbook_gen_new_file_name(gint fileNum) {
	gchar fmt[30];
	gchar buf[WORK_BUFLEN];
	gint n = fileNum;
	long int nmax;

	if (n < 1) 
		n = 1;
	nmax = -1 + (long int) pow(10, FILE_NUMDIGITS);
	if (fileNum > nmax)
		return NULL;
	g_snprintf(fmt, sizeof(fmt), "%%s%%0%dd%%s", FILE_NUMDIGITS);
	g_snprintf(buf, sizeof(buf), fmt, ADDRBOOK_PREFIX, n, ADDRBOOK_SUFFIX);
	return g_strdup(buf);
}

/*
 * **********************************************************************
 * Address book test functions...
 * **********************************************************************
 */

/**
 * Attempt to parse list of email address from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_addr_list( AddressBookFile *book, XMLFile *file )
{
	guint prev_level;
	/* GList *attr; */

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file))
			longjmp(book->jumper, 1);
		if (file->level < prev_level)
			return;
		/* attr = xml_get_current_tag_attr(file); */
		/* addrbook_show_attribs( attr ); */
		if (xml_compare_tag(file, AB_ELTAG_ADDRESS))
			addrbook_chkparse_addr_list(book, file);
	}
}

/**
 * Attempt to parse attributes for person address from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_attribute(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */
	/* gchar *element; */

	/* attr = xml_get_current_tag_attr(file); */
	/* addrbook_show_attribs( attr ); */
	/* element = xml_get_element(file); */
	/* g_print( "\t\tattrib value : %s\n", element ); */
}

/**
 * Attempt to parse list of attributes for person address from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_attr_list(AddressBookFile *book, XMLFile *file)
{
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file))
			longjmp(book->jumper, 1);
		if (file->level < prev_level) 
			return;
		if (xml_compare_tag(file, AB_ELTAG_ATTRIBUTE)) {
			addrbook_chkparse_attribute(book, file);
			addrbook_chkparse_attr_list(book, file);
		}
	}
}

/**
 * Attempt to parse person from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_person(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */

	/* attr = xml_get_current_tag_attr(file); */
	/* addrbook_show_attribs( attr ); */
	if (xml_parse_next_tag(file)) /* Consume closing tag */
		longjmp(book->jumper, 1);

	if (xml_compare_tag(file, AB_ELTAG_ADDRESS_LIST))
		addrbook_chkparse_addr_list(book, file);

	if (xml_parse_next_tag(file))	/* Consume closing tag */
		longjmp(book->jumper, 1);

	if (xml_compare_tag(file, AB_ELTAG_ATTRIBUTE_LIST))
		addrbook_chkparse_attr_list(book, file);
}

/**
 * Attempt to parse list of members from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_member_list(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file)) 
			longjmp(book->jumper, 1);

		if (file->level < prev_level)
			return;
			
		if (xml_compare_tag(file, AB_ELTAG_MEMBER)) {
			/* attr = xml_get_current_tag_attr(file); */
			/* addrbook_show_attribs( attr ); */
			addrbook_chkparse_member_list(book, file);
		}
		else {
			/* attr = xml_get_current_tag_attr(file); */
			/* addrbook_show_attribs( attr ); */
		}
	}
}

/**
 * Attempt to parse group from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_group(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */

	/* attr = xml_get_current_tag_attr(file); */
	/* addrbook_show_attribs( attr ); */
	if (xml_parse_next_tag(file))	/* Consume closing tag */
		longjmp(book->jumper, 1);

	if (xml_compare_tag(file, AB_ELTAG_MEMBER_LIST))
		addrbook_chkparse_member_list(book, file);
}

/**
 * Attempt to parse list of folders from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_folder_list(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */
	guint prev_level;

	for (;;) {
		prev_level = file->level;
		if (xml_parse_next_tag(file))
			longjmp(book->jumper, 1);

		if (file->level < prev_level)
			return;
			
		if (xml_compare_tag(file, AB_ELTAG_ITEM)) {
			/* attr = xml_get_current_tag_attr(file); */
			/* addrbook_show_attribs( attr ); */
			addrbook_chkparse_folder_list(book, file);
		}
		else {
			/* attr = xml_get_current_tag_attr(file); */
			/* addrbook_show_attribs( attr ); */
		}
	}
}

/**
 * Attempt to parse a folder from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static void addrbook_chkparse_folder(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */

	/* attr = xml_get_current_tag_attr(file); */
	/* addrbook_show_attribs( attr ); */
	if (xml_parse_next_tag(file))	/* Consume closing tag */
		longjmp(book->jumper, 1);

	if (xml_compare_tag(file, AB_ELTAG_ITEM_LIST))
		addrbook_chkparse_folder_list(book, file);
}

/**
 * Attempt to parse (DOM) tree from file.
 * \param book Address book.
 * \param file XML file handle.
 */
static gboolean addrbook_chkread_tree(AddressBookFile *book, XMLFile *file)
{
	/* GList *attr; */
	gboolean retVal;

	if (xml_get_dtd(file))
		return FALSE;

	if (xml_parse_next_tag(file))
		return FALSE;

	if (!xml_compare_tag(file, AB_ELTAG_ADDRESS_BOOK))
		return FALSE;

	/* attr = xml_get_current_tag_attr(file); */
	/* addrbook_show_attribs( attr ); */

	retVal = TRUE;
	for (;;) {
		if (!file->level) 
			break;
		/* Get item tag */
		if (xml_parse_next_tag(file))
			longjmp(book->jumper, 1);
			
		/* Get next tag (person, group or folder) */
		if (xml_compare_tag(file, AB_ELTAG_PERSON))
			addrbook_chkparse_person( book, file );
		else if (xml_compare_tag(file, AB_ELTAG_GROUP))
			addrbook_chkparse_group(book, file);
		else if (xml_compare_tag(file, AB_ELTAG_FOLDER))
			addrbook_chkparse_folder(book, file);
	}
	return retVal;
}

/**
 * Test address book file by parsing contents.
 * \param  book     Address book.
 * \param  fileName Filename of XML file.
 * \return Status code <i>MGU_SUCCESS</i> if file appears to be valid format.
 */
gint addrbook_test_read_file(AddressBookFile *book, gchar *fileName)
{
	XMLFile *file = NULL;
	gchar *fileSpec = NULL;

	cm_return_val_if_fail(book != NULL, -1);

	fileSpec = g_strconcat(book->path, G_DIR_SEPARATOR_S, fileName, NULL);
	book->retVal = MGU_OPEN_FILE;
	file = xml_open_file(fileSpec);
	g_free(fileSpec);
	if (file) {
		book->retVal = MGU_BAD_FORMAT;
		if (setjmp(book->jumper)) {
			/* g_print( "Caught Ya!!!\n" ); */
			xml_close_file(file);
			return book->retVal;
		}
		if (addrbook_chkread_tree(book, file))
			book->retVal = MGU_SUCCESS;

		xml_close_file( file );
	}
	return book->retVal;
}

/**
 * Return link list of all persons in address book.  Note that the list
 * contains references to items. Do <b>*NOT*</b> attempt to use the
 * <code>addrbook_free_xxx()</code> functions... this will destroy the
 * addressbook data!
 * \param  book     Address book.
 * \return List of persons, or NULL if none.
 */
GList *addrbook_get_all_persons(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_get_all_persons(book->addressCache);
}

GList *addrbook_get_all_groups(AddressBookFile *book)
{
	cm_return_val_if_fail(book != NULL, NULL);
	return addrcache_get_all_groups(book->addressCache);
}

/**
 * Add person and address data to address book.
 * \param  book    Address book.
 * \param  folder  Folder where to add person, or NULL for root folder.
 * \param  name    Common name.
 * \param  address EMail address.
 * \param  remarks Remarks.
 * \return Person added. Do not <b>*NOT*</b> to use the
 *         <code>addrbook_free_xxx()</code> functions... this will destroy
 *         the address book data.
 */
ItemPerson *addrbook_add_contact(AddressBookFile *book, ItemFolder *folder, 
				 const gchar *name,const gchar *address, 
				 const gchar *remarks)
{
	ItemPerson *person;

	cm_return_val_if_fail(book != NULL, NULL);
	person = addrcache_add_contact(
			book->addressCache, folder, name, address, remarks );
	return person;
}

/**
 * Return file name for next address book file.
 * \param  book Address book.
 * \return File name, or <i>NULL</i> if could not create. This should be
 *         <code>g_free()</code> when done.
 */
gchar *addrbook_guess_next_file(AddressBookFile *book)
{
	gchar *newFile = NULL;
	GList *fileList = NULL;
	gint fileNum = 1;
	fileList = addrbook_get_bookfile_list(book);
	if (fileList)
		fileNum = 1 + book->maxValue;
	
	newFile = addrbook_gen_new_file_name(fileNum);
	g_list_free(fileList);
	fileList = NULL;
	return newFile;
}

void addrbook_delete_book_file(AddressBookFile *book)
{
	gchar *book_path;
	
	if (!book->path || !book->fileName)
		return;
	
	book_path = g_strconcat(book->path, G_DIR_SEPARATOR_S,
				book->fileName, NULL);
	claws_unlink(book_path);
	g_free(book_path);
}

/*
* End of Source.
*/


