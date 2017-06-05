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
 * Address item data.
 */

#ifndef __ADDRITEM_H__
#define __ADDRITEM_H__

#include <stdio.h>
#include <glib.h>

#define ADDRITEM_OBJECT(obj)	((AddrItemObject *)obj)
#define ADDRITEM_TYPE(obj)	(ADDRITEM_OBJECT(obj)->type)
#define ADDRITEM_NAME(obj)	(ADDRITEM_OBJECT(obj)->name)
#define ADDRITEM_ID(obj)	(ADDRITEM_OBJECT(obj)->uid)
#define ADDRITEM_PARENT(obj)	(ADDRITEM_OBJECT(obj)->parent)
#define ADDRITEM_SUBTYPE(obj)	(ADDRITEM_OBJECT(obj)->subType)

typedef enum {
	ITEMTYPE_NONE,
	ITEMTYPE_PERSON,
	ITEMTYPE_EMAIL,
	ITEMTYPE_FOLDER,
	ITEMTYPE_GROUP,
	ITEMTYPE_INDEX,
	ITEMTYPE_INTERFACE,
	ITEMTYPE_DATASOURCE
} ItemObjectType;

typedef enum {
	ADDRFOLDER_NONE,
	ADDRFOLDER_ROOT,
	ADDRFOLDER_REGULAR,
	ADDRFOLDER_CATEGORY,
	ADDRFOLDER_QUERY_RESULTS
} AddressFolderType;

typedef enum {
	NONE,
	ADD_ENTRY,
	UPDATE_ENTRY,
	DELETE_ENTRY
} ContactStatus;

typedef struct _AddrItemObject AddrItemObject;
struct _AddrItemObject {
	ItemObjectType type;
	gchar          *name;
	gchar          *uid;
	AddrItemObject *parent;
	gint           subType;
};

typedef struct _ItemPerson ItemPerson;
struct _ItemPerson {
	AddrItemObject obj;
	gchar		*picture;
	gchar		*firstName;
	gchar		*lastName;
	gchar		*nickName;
	gchar		*externalID;
	GList		*listEMail;
	GList		*listAttrib;
	gboolean	isOpened;
	ContactStatus status;
};

typedef struct _ItemEMail ItemEMail;
struct _ItemEMail {
	AddrItemObject obj;
	gchar *address;
	gchar *remarks;
};

typedef struct _UserAttribute UserAttribute;
struct _UserAttribute {
	gchar *uid;
	gchar *name;
	gchar *value;
};

typedef struct _ItemFolder ItemFolder;
struct _ItemFolder {
	AddrItemObject obj;
	gchar    *remarks;
	gboolean isRoot;	/* TRUE if root folder */
	GList    *listItems;	/* Used for temporary items only */
	GList    *listFolder;	/* List of contained (child) folders */
	GList    *listPerson;	/* List of contained persons */
	GList    *listGroup;	/* List of contained (child) groups */
	AddressFolderType folderType;	/* Folder type */
	gpointer *folderData;		/* Pointer to folder's data */
	gboolean isHidden;	/* TRUE if folder is hidden */
};

typedef struct _ItemGroup ItemGroup;
struct _ItemGroup {
	AddrItemObject obj;
	gchar *remarks;
	GList *listEMail;
};

/* Function prototypes */
ItemEMail *addritem_create_item_email	( void );
ItemEMail *addritem_copy_item_email	( ItemEMail *item );
ItemEMail *addritem_copyfull_item_email	( ItemEMail *item );
void addritem_email_set_alias		( ItemEMail *email, const gchar *value );
void addritem_email_set_address		( ItemEMail *email, const gchar *value );
void addritem_email_set_remarks		( ItemEMail *email, const gchar *value );
void addritem_free_item_email		( ItemEMail *item );

UserAttribute *addritem_create_attribute( void );
UserAttribute *addritem_copy_attribute	( UserAttribute *item );
void addritem_attrib_set_id		( UserAttribute *item, const gchar *value );
void addritem_attrib_set_name		( UserAttribute *item, const gchar *value );
void addritem_attrib_set_value		( UserAttribute *item, const gchar *value );
void addritem_free_attribute		( UserAttribute *item );

ItemPerson *addritem_create_item_person	( void );
ItemPerson *addritem_copy_item_person	( ItemPerson *item );
void addritem_person_set_picture	( ItemPerson *person, const gchar *value );
gchar *addritem_person_get_picture	( ItemPerson *person);
void addritem_person_set_first_name	( ItemPerson *person, const gchar *value );
void addritem_person_set_last_name	( ItemPerson *person, const gchar *value );
void addritem_person_set_nick_name	( ItemPerson *person, const gchar *value );
void addritem_person_set_common_name	( ItemPerson *person, const gchar *value );
void addritem_person_set_external_id	( ItemPerson *person, const gchar *value );
void addritem_person_set_opened		( ItemPerson *person, const gboolean value );
void addritem_free_item_person		( ItemPerson *person );
void addritem_free_list_email		( GList *list );
void addritem_free_list_attribute	( GList *list );

ItemGroup *addritem_create_item_group	( void );
ItemGroup *addritem_copy_item_group	( ItemGroup *item );
void addritem_free_item_group		( ItemGroup *group );
void addritem_group_set_name		( ItemGroup *group, const gchar *value );

void addritem_print_item_email		( ItemEMail *item, FILE *stream );
void addritem_print_item_person		( ItemPerson *person, FILE *stream );
void addritem_print_item_group		( ItemGroup *group, FILE *stream );
void addritem_print_item_folder		( ItemFolder *folder, FILE *stream );

gboolean addritem_person_add_email		( ItemPerson *person, ItemEMail *email );
ItemEMail *addritem_person_remove_email		( ItemPerson *person, ItemEMail *email );

void addritem_person_add_attribute		( ItemPerson *person, UserAttribute *attrib );
void addritem_person_remove_attribute	( ItemPerson *person, const gchar *attrib );

ItemFolder *addritem_create_item_folder	( void );
ItemFolder *addritem_copy_item_folder	( ItemFolder *item );
void addritem_folder_set_name		( ItemFolder *folder, const gchar *value );
void addritem_folder_set_remarks	( ItemFolder *folder, const gchar *value );
void addritem_folder_set_hidden		( ItemFolder *folder, const gboolean value );
void addritem_free_item_folder		( ItemFolder *folder );

gboolean addritem_group_add_email	( ItemGroup *group, ItemEMail *email );

gboolean addritem_folder_add_person	( ItemFolder *folder, ItemPerson *item );
ItemPerson *addritem_folder_remove_person( ItemFolder *group, ItemPerson *person );

gboolean addritem_folder_add_folder	( ItemFolder *folder, ItemFolder *item );
gboolean addritem_folder_add_group	( ItemFolder *folder, ItemGroup *item );
GList *addritem_folder_get_person_list	( ItemFolder *folder );
GList *addritem_folder_get_group_list	( ItemFolder *folder );

void addritem_parse_first_last		( ItemPerson *person );
GList *addritem_folder_path		( const ItemFolder *folder,
					  const gboolean seq );
gchar *addritem_format_email		( ItemEMail *email );

#endif /* __ADDRITEM_H__ */
