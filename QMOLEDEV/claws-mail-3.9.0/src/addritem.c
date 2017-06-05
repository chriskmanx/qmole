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
 * General primitive address item objects.
 */

#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "utils.h"
#include "addritem.h"
#include "mgutils.h"
#include "codeconv.h"

/**
 * Create new email address item.
 * \return Initialized email item.
 */
ItemEMail *addritem_create_item_email( void ) {
	ItemEMail *item;
	item = g_new0( ItemEMail, 1 );
	ADDRITEM_TYPE(item) = ITEMTYPE_EMAIL;
	ADDRITEM_ID(item) = NULL;
	ADDRITEM_NAME(item) = NULL;
	ADDRITEM_PARENT(item) = NULL;
	ADDRITEM_SUBTYPE(item) = 0;
	item->address = NULL;
	item->remarks = NULL;
	return item;
}

/**
 * Create a shallow copy of specified email address item.
 * \param  item E-Mail to copy.
 * \return Copy of email, or <i>NULL</i> if null argument supplied.
 */
ItemEMail *addritem_copy_item_email( ItemEMail *item ) {
	ItemEMail *itemNew = NULL;
	if( item ) {
		itemNew = addritem_create_item_email();
		ADDRITEM_NAME(itemNew) = g_strdup( ADDRITEM_NAME(item) );
		itemNew->address = g_strdup( item->address );
		itemNew->remarks = g_strdup( item->remarks );
	}
	return itemNew;
}

/**
 * Create a full copy (deep copy) of specified email address item.
 * \param  item E-Mail to copy.
 * \return Copy of email.
 */
ItemEMail *addritem_copyfull_item_email( ItemEMail *item ) {
	ItemEMail *itemNew = NULL;
	if( item ) {
		itemNew = addritem_create_item_email();
		ADDRITEM_ID(itemNew) = g_strdup( ADDRITEM_ID(item) );
		ADDRITEM_NAME(itemNew) = g_strdup( ADDRITEM_NAME(item) );
		ADDRITEM_PARENT(itemNew) = ADDRITEM_PARENT(item);
		itemNew->address = g_strdup( item->address );
		itemNew->remarks = g_strdup( item->remarks );
	}
	return itemNew;
}

/**
 * Specify alias for email.
 * \param email E-Mail item.
 * \param value Alias.
 */
void addritem_email_set_alias( ItemEMail *email, const gchar *value ) {
	ADDRITEM_NAME(email) = mgu_replace_string( ADDRITEM_NAME(email), value );
}

/**
 * Specify address for email.
 * \param email E-Mail item.
 * \param value Address.
 */
void addritem_email_set_address( ItemEMail *email, const gchar *value ) {
	email->address = mgu_replace_string( email->address, value );
}

/**
 * Specify remarks for email.
 * \param email E-Mail item.
 * \param value Remarks.
 */
void addritem_email_set_remarks( ItemEMail *email, const gchar *value ) {
	email->remarks = mgu_replace_string( email->remarks, value );
}

/**
 * Free address item email object.
 * \param item E-Mail item to free.
 */
void addritem_free_item_email( ItemEMail *item ) {
	cm_return_if_fail( item != NULL );

	/* Free internal stuff */
	g_free( ADDRITEM_ID(item) );
	g_free( ADDRITEM_NAME(item) );
	g_free( item->address );
	g_free( item->remarks );

	ADDRITEM_OBJECT(item)->type = ITEMTYPE_NONE;
	ADDRITEM_ID(item) = NULL;
	ADDRITEM_NAME(item) = NULL;
	ADDRITEM_PARENT(item) = NULL;
	ADDRITEM_SUBTYPE(item) = 0;
	item->address = NULL;
	item->remarks = NULL;
	g_free( item );
}

/**
 * Create new attribute object.
 * \return Initialized attribute object.
 */
UserAttribute *addritem_create_attribute( void ) {
	UserAttribute *item;
	item = g_new0( UserAttribute, 1 );
	item->uid = NULL;
	item->name = NULL;
	item->value = NULL;
	return item;
}

/**
 * Create copy (deep copy) of specified attribute.
 * \param  item Attribute to copy.
 * \return Copy of attribute, or <i>NULL</i> if null argument supplied.
 */
UserAttribute *addritem_copy_attribute( UserAttribute *item ) {
	UserAttribute *itemNew = NULL;
	if( item ) {
		itemNew = addritem_create_attribute();
		itemNew->uid = g_strdup( item->uid );
		itemNew->name = g_strdup( item->name );
		itemNew->value = g_strdup( item->value );
	}
	return itemNew;
}

/**
 * Specify ID for attribute.
 * \param item Attribute object.
 * \param value ID.
 */
void addritem_attrib_set_id( UserAttribute *item, const gchar *value ) {
	cm_return_if_fail( item != NULL );
	item->uid = mgu_replace_string( item->uid, value );
}

/**
 * Specify name for attribute.
 * \param item Attribute object.
 * \param value Name.
 */
void addritem_attrib_set_name( UserAttribute *item, const gchar *value ) {
	cm_return_if_fail( item != NULL );
	item->name = mgu_replace_string( item->name, value );
}

/**
 * Specify value for attribute.
 * \param item Attribute object.
 * \param value Value.
 */
void addritem_attrib_set_value( UserAttribute *item, const gchar *value ) {
	cm_return_if_fail( item != NULL );
	item->value = mgu_replace_string( item->value, value );
}

/**
 * Free user attribute.
 * \param item Attribute object to free.
 */
void addritem_free_attribute( UserAttribute *item ) {
	cm_return_if_fail( item != NULL );
	g_free( item->uid );
	g_free( item->name );
	g_free( item->value );
	item->uid = NULL;
	item->name = NULL;
	item->value = NULL;
	g_free( item );
}

/**
 * Create new address book person.
 * \return Initialized person object.
 */
ItemPerson *addritem_create_item_person( void ) {
	ItemPerson *person;
	person = g_new0( ItemPerson, 1 );
	ADDRITEM_TYPE(person) = ITEMTYPE_PERSON;
	ADDRITEM_ID(person) = NULL;
	ADDRITEM_NAME(person) = NULL;
	ADDRITEM_PARENT(person) = NULL;
	ADDRITEM_SUBTYPE(person) = 0;
	person->picture = NULL;
	person->firstName = NULL;
	person->lastName = NULL;
	person->nickName = NULL;
	person->listEMail = NULL;
	person->listAttrib = NULL;
	person->externalID = NULL;
	person->isOpened = FALSE;
	person->status = NONE;
	return person;
}

/**
 * Create a shallow copy of address book person object.
 * \param  item Person to copy.
 * \return Copy of person, or <i>NULL</i> if null argument supplied.
 */
ItemPerson *addritem_copy_item_person( ItemPerson *item ) {
	ItemPerson *itemNew;

	itemNew = NULL;
	if( item ) {
		itemNew = addritem_create_item_person();
		ADDRITEM_NAME(itemNew) = g_strdup( ADDRITEM_NAME(item) );
		itemNew->picture = g_strdup( ADDRITEM_ID(itemNew) );
		itemNew->firstName = g_strdup( item->firstName );
		itemNew->lastName = g_strdup( item->lastName );
		itemNew->nickName = g_strdup( item->nickName );
		itemNew->externalID = g_strdup( item->externalID );
		itemNew->status = item->status;
	}
	return itemNew;
}

/**
 * Specify picture for person object.
 * \param person Person object.
 * \param value Picture.
 */
void addritem_person_set_picture( ItemPerson *person, const gchar *value ) {
	if (!value || g_utf8_validate(value, -1, NULL))
		person->picture = mgu_replace_string( person->picture, value );
	else {
		gchar *out = conv_codeset_strdup(value, 
				conv_get_locale_charset_str_no_utf8(),
				CS_INTERNAL);
		if (out)
			person->picture = mgu_replace_string( person->picture, out );
		g_free(out);
	}
}

/**
 * Get picture for person object.
 * \param person Person object.
 * \param value Picture.
 */
gchar *addritem_person_get_picture( ItemPerson *person) {
	if (person->picture)
		return g_strdup(person->picture);
	return NULL;
}
/**
 * Specify first name for person object.
 * \param person Person object.
 * \param value Name.
 */
void addritem_person_set_first_name( ItemPerson *person, const gchar *value ) {
	if (!value || g_utf8_validate(value, -1, NULL))
		person->firstName = mgu_replace_string( person->firstName, value );
	else {
		gchar *out = conv_codeset_strdup(value, 
				conv_get_locale_charset_str_no_utf8(),
				CS_INTERNAL);
		if (out)
			person->firstName = mgu_replace_string( person->firstName, out );
		g_free(out);
	}
}

/**
 * Specify last name for person object.
 * \param person Person object.
 * \param value name.
 */
void addritem_person_set_last_name( ItemPerson *person, const gchar *value ) {
	if (!value || g_utf8_validate(value, -1, NULL))
		person->lastName = mgu_replace_string( person->lastName, value );
	else {
		gchar *out = conv_codeset_strdup(value, 
				conv_get_locale_charset_str_no_utf8(),
				CS_INTERNAL);
		if (out)
			person->lastName = mgu_replace_string( person->lastName, out );
		g_free(out);
	}
}

/**
 * Specify nick name for person object.
 * \param person Person object.
 * \param value name.
 */
void addritem_person_set_nick_name( ItemPerson *person, const gchar *value ) {
	if (!value || g_utf8_validate(value, -1, NULL))
		person->nickName = mgu_replace_string( person->nickName, value );
	else {
		gchar *out = conv_codeset_strdup(value, 
				conv_get_locale_charset_str_no_utf8(),
				CS_INTERNAL);
		if (out)
			person->nickName = mgu_replace_string( person->nickName, out );
		g_free(out);
	}
}

/**
 * Specify common name for person object.
 * \param person Person object.
 * \param value name.
 */
void addritem_person_set_common_name( ItemPerson *person, const gchar *value ) {
	if (!value || g_utf8_validate(value, -1, NULL))
		ADDRITEM_NAME(person) = mgu_replace_string( ADDRITEM_NAME(person), value );
	else {
		gchar *out = conv_codeset_strdup(value, 
				conv_get_locale_charset_str_no_utf8(),
				CS_INTERNAL);
		if (out)
			ADDRITEM_NAME(person) = mgu_replace_string( ADDRITEM_NAME(person), out );
		g_free(out);
	}
}

/**
 * Specify external ID for person object.
 * \param person Person object.
 * \param value ID.
 */
void addritem_person_set_external_id( ItemPerson *person, const gchar *value ) {
	person->externalID = mgu_replace_string( person->externalID, value );
}

/**
 * Specify value of open indicator for person object. This is typically used to
 * simplify open/close folders in the address book GUI.
 * \param person Person object.
 * \param value  Value for indicator. Set to <i>TRUE</i> if opened.
 */
void addritem_person_set_opened( ItemPerson *person, const gboolean value ) {
	person->isOpened = value;
}

/**
 * Free linked list of item addresses; both addresses and the list are freed.
 * It is assumed that addresses are *NOT* contained within some other
 * container.
 * \param list List of addresses to be freed.
 */
void addritem_free_list_email( GList *list ) {
	GList *node = list;
	while( node ) {
		ItemEMail *email = node->data;

		addritem_free_item_email( email );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( list );
	list = NULL;
}

/**
 * Free linked list of attributes; both attributes and the list are freed.
 * It is assumed that attributes are *NOT* contained within some other
 * container.
 * \param list List of attributes to be freed.
 */
void addritem_free_list_attribute( GList *list ) {
	GList *node = list;
	while( node ) {
		addritem_free_attribute( node->data );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( list );
}

/**
 * Free address person object.
 * \param person Person object to free.
 */
void addritem_free_item_person( ItemPerson *person ) {
	cm_return_if_fail( person != NULL );

	/* Free internal stuff */
	g_free( ADDRITEM_ID(person) );
	g_free( ADDRITEM_NAME(person) );
	g_free( person->picture );
	g_free( person->firstName );
	g_free( person->lastName );
	g_free( person->nickName );
	g_free( person->externalID );
	g_list_free( person->listEMail );
	addritem_free_list_attribute( person->listAttrib );

	ADDRITEM_OBJECT(person)->type = ITEMTYPE_NONE;
	ADDRITEM_ID(person) = NULL;
	ADDRITEM_NAME(person) = NULL;
	ADDRITEM_PARENT(person) = NULL;
	ADDRITEM_SUBTYPE(person) = 0;
	person->picture = NULL;
	person->firstName = NULL;
	person->lastName = NULL;
	person->nickName = NULL;
	person->externalID = NULL;
	person->listEMail = NULL;
	person->listAttrib = NULL;

	g_free( person );
}

/**
 * Print E-Mail address object for debug.
 * \param item   Item to print.
 * \param stream Output stream.
 */
void addritem_print_item_email( ItemEMail *item, FILE *stream ) {
	cm_return_if_fail( item != NULL );
	fprintf( stream, "\t\tt/id: %d : '%s'\n", ADDRITEM_TYPE(item), ADDRITEM_ID(item) );
	fprintf( stream, "\t\tsubty: %d\n", ADDRITEM_SUBTYPE(item) );
	fprintf( stream, "\t\talis: '%s'\n", ADDRITEM_NAME(item) );
	fprintf( stream, "\t\taddr: '%s'\n", item->address );
	fprintf( stream, "\t\trems: '%s'\n", item->remarks );
	fprintf( stream, "\t\t---\n" );
}

/**
 * Print user attribute object for debug.
 * \param item   Attribute to print.
 * \param stream Output stream.
 */
static void addritem_print_attribute( UserAttribute *item, FILE *stream ) {
	cm_return_if_fail( item != NULL );
	fprintf( stream, "\t\tuid  : '%s'\n", item->uid );
	fprintf( stream, "\t\tname : '%s'\n", item->name );
	fprintf( stream, "\t\tvalue: '%s'\n", item->value );
	fprintf( stream, "\t\t---\n" );
}

/**
 * Print person item for debug.
 * \param person Person to print.
 * \param stream Output stream.
 */
void addritem_print_item_person( ItemPerson *person, FILE *stream ) {
	GList *node;
	cm_return_if_fail( person != NULL );
	fprintf( stream, "Person:\n" );
	fprintf( stream, "\tt/uid: %d : '%s'\n", ADDRITEM_TYPE(person), ADDRITEM_ID(person) );
	fprintf( stream, "\tsubty: %d\n", ADDRITEM_SUBTYPE(person) );
	fprintf( stream, "\tcommn: '%s'\n", ADDRITEM_NAME(person) );
	fprintf( stream, "\tphoto: '%s'\n", person->picture );
	fprintf( stream, "\tfirst: '%s'\n", person->firstName );
	fprintf( stream, "\tlast : '%s'\n", person->lastName );
	fprintf( stream, "\tnick : '%s'\n", person->nickName );
	fprintf( stream, "\textID: '%s'\n", person->externalID );
	fprintf( stream, "\teMail:\n" );
	fprintf( stream, "\t---\n" );
	node = person->listEMail;
	while( node ) {
		addritem_print_item_email( node->data, stream );
		node = g_list_next( node );
	}
	fprintf( stream, "\tuAttr:\n" );
	fprintf( stream, "\t---\n" );
	node = person->listAttrib;
	while( node ) {
		addritem_print_attribute( node->data, stream );
		node = g_list_next( node );
	}
	gchar *current_status;
	switch (person->status) {
		case NONE: current_status = g_strdup("Unchanged"); break;
		case ADD_ENTRY: current_status = g_strdup("New"); break;
		case UPDATE_ENTRY: current_status = g_strdup("Updated"); break;
		case DELETE_ENTRY: current_status = g_strdup("Deleted"); break;
		default: current_status = g_strdup("Unknown");
	}
	fprintf( stream, "\t\tStatus: %s\n", current_status );
	if ( current_status )
		g_free(current_status);
	fprintf( stream, "\t===\n" );
}

/**
 * Add E-Mail address object to person.
 * \param  person Person.
 * \param  email  E-Mail object to add.
 * \return <i>TRUE</i> if E-Mail added.
 */
gboolean addritem_person_add_email( ItemPerson *person, ItemEMail *email ) {
	GList *node;

	cm_return_val_if_fail( person != NULL, FALSE );
	cm_return_val_if_fail( email != NULL, FALSE );

	node = person->listEMail;
	while( node ) {
		if( node->data == email ) return FALSE;
		node = g_list_next( node );
	}
	person->listEMail = g_list_append( person->listEMail, email );
	ADDRITEM_PARENT(email) = ADDRITEM_OBJECT(person);
	return TRUE;
}

/**
 * Remove email address for specified person.
 * \param  person Person.
 * \param  email  EMail to remove.
 * \return EMail object, or <i>NULL</i> if not found. Note that object should
 *         still be freed after calling this method.
 */
ItemEMail *addritem_person_remove_email( ItemPerson *person, ItemEMail *email ) {
	gboolean found = FALSE;
	GList *node;

	cm_return_val_if_fail( person != NULL, NULL );
	if( email == NULL ) return NULL;

	/* Look for email */
	node = person->listEMail;
	while( node ) {
		if( node-> data == email ) {
			found = TRUE;
			break;
		}
		node = g_list_next( node );
	}

	if( found ) {
		/* Remove email from person's address list */
		if( person->listEMail ) {
			person->listEMail = g_list_remove( person->listEMail, email );
		}
		/* Unlink reference to person. */
		ADDRITEM_PARENT(email) = NULL;
		return email;
	}
	return NULL;
}

/**
 * Add user attribute to specified person.
 * \param  person Person.
 * \param  attrib Attribute to add.
 * \return <i>TRUE</i> if item added.
 */
void addritem_person_add_attribute(
			ItemPerson *person, UserAttribute *attrib )
{
	cm_return_if_fail( person != NULL );
	person->listAttrib = g_list_append( person->listAttrib, attrib );
}

/**
 * Remove user attribute from specified person.
 * \param  person Person.
 * \param  attrib Attribute to remove.
 */
void addritem_person_remove_attribute(	ItemPerson *person, const gchar *attrib ) {
	cm_return_if_fail( person != NULL && attrib != NULL );
	GList *attrib_list;

	attrib_list = person->listAttrib;
	while (attrib_list) {
		UserAttribute *user_attrib = attrib_list->data;
		if (strcmp(user_attrib->name, attrib) == 0) {
			person->listAttrib = g_list_remove(person->listAttrib, (gconstpointer) user_attrib);
			break;
		}
		attrib_list = g_list_next(attrib_list);
	}
}


/**
 * Create new address book group object.
 * \return Initialized group object.
 */
ItemGroup *addritem_create_item_group( void ) {
	ItemGroup *group;

	group = g_new0( ItemGroup, 1 );
	ADDRITEM_TYPE(group) = ITEMTYPE_GROUP;
	ADDRITEM_ID(group) = NULL;
	ADDRITEM_NAME(group) = NULL;
	ADDRITEM_PARENT(group) = NULL;
	ADDRITEM_SUBTYPE(group) = 0;
	group->remarks = NULL;
	group->listEMail = NULL;
	return group;
}

/**
 * Copy (deep copy) address book group.
 * \param  item Group to copy.
 * \return Copy of the group object, or <i>NULL</i> if null argument supplied.
 */
ItemGroup *addritem_copy_item_group( ItemGroup *item ) {
	ItemGroup *itemNew;

	itemNew = NULL;
	if( item ) {
		itemNew = addritem_create_item_group();
		ADDRITEM_NAME(itemNew) = g_strdup( ADDRITEM_NAME(item) );
		itemNew->remarks = g_strdup( item->remarks );
	}
	return itemNew;
}

/**
 * Specify name to be used for group.
 * \param group Group object.
 * \param value Name of group.
 */
void addritem_group_set_name( ItemGroup *group, const gchar *value ) {
	ADDRITEM_NAME(group) = mgu_replace_string( ADDRITEM_NAME(group), value );
}

/**
 * Free address group object.
 * \param group Group to free.
 */
void addritem_free_item_group( ItemGroup *group ) {
	cm_return_if_fail( group != NULL );

	/* Free internal stuff */
	g_free( ADDRITEM_ID(group) );
	g_free( ADDRITEM_NAME(group) );
	g_free( group->remarks );
	mgu_clear_list( group->listEMail );
	g_list_free( group->listEMail );

	ADDRITEM_TYPE(group) = ITEMTYPE_NONE;
	ADDRITEM_ID(group) = NULL;
	ADDRITEM_NAME(group) = NULL;
	ADDRITEM_PARENT(group) = NULL;
	ADDRITEM_SUBTYPE(group) = 0;
	group->remarks = NULL;
	group->listEMail = NULL;

	g_free( group );
}

/**
 * Add EMail address to group. Note that a reference to an E-Mail item is
 * added to a group. A person object is the only container that for an
 * address.
 * \param  group Group.
 * \param  email E-Mail object.
 * \return <i>TRUE</i> if email item added.
 */
gboolean addritem_group_add_email( ItemGroup *group, ItemEMail *email ) {
	GList *node;

	cm_return_val_if_fail( group != NULL, FALSE );
	cm_return_val_if_fail( email != NULL, FALSE );

	node = group->listEMail;
	while( node ) {
		if( node->data == email ) return FALSE;
		node = g_list_next( node );
	}
	group->listEMail = g_list_append( group->listEMail, email );
	return TRUE;
}

/**
 * Remove person object for specified group.
 * \param  group Group from which to remove address.
 * \param  email EMail to remove
 * \return EMail object, or <i>NULL if email not found in group. Note that
 *         this object is referenced (linked) to a group and should *NOT*
 *         be freed. An E-Mail object object should only be freed after
 *         removing from a person.
 */
ItemPerson *addritem_folder_remove_person( ItemFolder *group, ItemPerson *person ) {
	if( group && person ) {
		GList *node = group->listPerson;
		while( node ) {
			if( node->data == person ) {
				group->listPerson = g_list_remove( group->listPerson, person );
				return person;
			}
			node = g_list_next( node );
		}
	}
	return NULL;
}

/**
 * Print address group item for debug.
 * \param group  Group to print.
 * \param stream Output stream.
 */
void addritem_print_item_group( ItemGroup *group, FILE *stream ) {
	GList *node;
	ItemPerson *person;
	ItemEMail *item;
	cm_return_if_fail( group != NULL );
	fprintf( stream, "Group:\n" );
	fprintf( stream, "\tt/u: %d : '%s'\n", ADDRITEM_TYPE(group), ADDRITEM_ID(group) );
	fprintf( stream, "\tsub: %d\n", ADDRITEM_SUBTYPE(group) );
	fprintf( stream, "\tgrp: '%s'\n", ADDRITEM_NAME(group) );
	fprintf( stream, "\trem: '%s'\n", group->remarks );
	fprintf( stream, "\t---\n" );
	node = group->listEMail;
	while( node ) {
		item = node->data;
		person = ( ItemPerson * ) ADDRITEM_PARENT(item);
		if( person ) {
			fprintf( stream, "\t\tpid : '%s'\n", ADDRITEM_ID(person) );
			fprintf( stream, "\t\tcomn: '%s'\n", ADDRITEM_NAME(person) );
		}
		else {
			fprintf( stream, "\t\tpid : ???\n" );
			fprintf( stream, "\t\tcomn: ???\n" );
		}
		addritem_print_item_email( item, stream );
		node = g_list_next( node );
	}
	fprintf( stream, "\t***\n" );
}

/**
 * Create new address folder.
 * \return Initialized address folder object.
 */
ItemFolder *addritem_create_item_folder( void ) {
	ItemFolder *folder;
	folder = g_new0( ItemFolder, 1 );
	ADDRITEM_TYPE(folder) = ITEMTYPE_FOLDER;
	ADDRITEM_ID(folder) = NULL;
	ADDRITEM_NAME(folder) = NULL;
	ADDRITEM_PARENT(folder) = NULL;
	ADDRITEM_SUBTYPE(folder) = 0;
	folder->remarks = NULL;
	folder->isRoot = FALSE;
	folder->listItems = NULL;
	folder->listFolder = NULL;
	folder->listPerson = NULL;
	folder->listGroup = NULL;
	folder->folderType = ADDRFOLDER_NONE;
	folder->folderData = NULL;
	folder->isHidden = FALSE;
	return folder;
}

/**
 * Copy address book folder. Note that only the folder and not its contents are
 * copied.
 * \param  item Folder to copy.
 * \return A copy of the folder, or <i>NULL</i> if null argument supplied.
 */
ItemFolder *addritem_copy_item_folder( ItemFolder *item ) {
	ItemFolder *itemNew;

	itemNew = g_new0( ItemFolder, 1 );
	if( item ) {
		itemNew = addritem_create_item_folder();
		ADDRITEM_NAME(itemNew) = g_strdup( ADDRITEM_NAME(item) );
		itemNew->folderType = item->folderType;
	}
	return itemNew;
}

/**
 * Specify name to be used for folder.
 * \param folder Folder.
 * \param value  Name.
 */
void addritem_folder_set_name( ItemFolder *folder, const gchar *value ) {
	ADDRITEM_NAME(folder) = mgu_replace_string( ADDRITEM_NAME(folder), value );
}

/**
 * Specify remarks to be used for folder.
 * \param folder Folder.
 * \param value  Remarks.
 */
void addritem_folder_set_remarks( ItemFolder *folder, const gchar *value ) {
	folder->remarks = mgu_replace_string( folder->remarks, value );
}

/**
 * Specify visibility of folder.
 * \param folder Folder.
 * \param value  Set to <code>TRUE</code> to hide folder.
 */
void addritem_folder_set_hidden( ItemFolder *folder, const gboolean value ) {
	folder->isHidden = value;
}

/**
 * Free address folder. Note: this does not free up the lists of children
 * (folders, groups and person). This should be done prior to calling this
 * function.
 * \param folder Folder to free.
 */
void addritem_free_item_folder( ItemFolder *folder ) {
	cm_return_if_fail( folder != NULL );

	/* Free internal stuff */
	g_free( ADDRITEM_ID(folder) );
	g_free( ADDRITEM_NAME(folder) );
	g_free( folder->remarks );
	mgu_clear_list( folder->listItems );
	g_list_free( folder->listItems );

	ADDRITEM_TYPE(folder) = ITEMTYPE_NONE;
	ADDRITEM_ID(folder) = NULL;
	ADDRITEM_NAME(folder) = NULL;
	ADDRITEM_PARENT(folder) = NULL;
	ADDRITEM_SUBTYPE(folder) = 0;
	folder->isRoot = FALSE;
	folder->remarks = NULL;
	folder->listItems = NULL;
	folder->listFolder = NULL;
	folder->listGroup = NULL;
	folder->listPerson = NULL;
	folder->folderType = ADDRFOLDER_NONE;
	folder->folderData = NULL;
	folder->isHidden = FALSE;

	g_free( folder );
}

/**
 * Add person into folder.
 * \param  folder Folder.
 * \param  item   Person to add.
 * \return <i>TRUE</i> if person added.
 */
gboolean addritem_folder_add_person( ItemFolder *folder, ItemPerson *item ) {
	cm_return_val_if_fail( folder != NULL, FALSE );
	cm_return_val_if_fail( item != NULL, FALSE );

	folder->listPerson = g_list_append( folder->listPerson, item );
	ADDRITEM_PARENT(item) = ADDRITEM_OBJECT(folder);
	return TRUE;
}

/**
 * Add folder into folder.
 * \param  folder Folder.
 * \param  item   Folder to add.
 * \return <i>TRUE</i> if folder added.
 */
gboolean addritem_folder_add_folder( ItemFolder *folder, ItemFolder *item ) {
	cm_return_val_if_fail( folder != NULL, FALSE );
	cm_return_val_if_fail( item != NULL, FALSE );

	folder->listFolder = g_list_append( folder->listFolder, item );
	ADDRITEM_PARENT(item) = ADDRITEM_OBJECT(folder);
	return TRUE;
}

/**
 * Add group into folder.
 * \param  folder Folder.
 * \param  item   Group to add.
 * \return <i>TRUE</i> if group added.
 */
gboolean addritem_folder_add_group( ItemFolder *folder, ItemGroup *item ) {
	cm_return_val_if_fail( folder != NULL, FALSE );
	cm_return_val_if_fail( item != NULL, FALSE );

	folder->listGroup = g_list_append( folder->listGroup, item );
	ADDRITEM_PARENT(item) = ADDRITEM_OBJECT(folder);
	return TRUE;
}

/**
 * Print address folder item contents for debug.
 * \param folder Folder to process.
 * \param stream Output stream.
 */
void addritem_print_item_folder( ItemFolder *folder, FILE *stream ) {
	GList *node;
	/* ItemPerson *person; */
	ItemFolder *parent;

	cm_return_if_fail( folder != NULL );

	fprintf( stream, "Folder:\n" );
	fprintf( stream, "\tt/u: %d : '%s'\n", ADDRITEM_TYPE(folder), ADDRITEM_ID(folder) );
	fprintf( stream, "\tsub: %d\n", ADDRITEM_SUBTYPE(folder) );
	fprintf( stream, "\tnam: '%s'\n", ADDRITEM_NAME(folder) );
	fprintf( stream, "\trem: '%s'\n", folder->remarks );
	fprintf( stream, "\ttyp: %d\n", folder->folderType );
	fprintf( stream, "\thid: %s\n", folder->isHidden ? "hidden" : "visible" );
	fprintf( stream, "\t---\n" );
	parent = ( ItemFolder * ) ADDRITEM_PARENT(folder);
	if( parent ) {
		fprintf( stream, "\tpar: %s : %s\n", ADDRITEM_ID(parent), ADDRITEM_NAME(parent) );
	}
	else {
		fprintf( stream, "\tpar: NULL\n" );
	}
	node = folder->listFolder;
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio ) {
			if( aio->type == ITEMTYPE_FOLDER ) {
				ItemFolder *item = ( ItemFolder * ) aio;
				addritem_print_item_folder( item, stream );
			}
		}
		else {
			fprintf( stream, "\t\tpid : ???\n" );
		}

		node = g_list_next( node );
	}

	node = folder->listPerson;
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio ) {
			if( aio->type == ITEMTYPE_PERSON ) {
				ItemPerson *item = ( ItemPerson * ) aio;
				addritem_print_item_person( item, stream );
			}
		}
		else {
			fprintf( stream, "\t\tpid : ???\n" );
		}

		node = g_list_next( node );
	}

	node = folder->listGroup;
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio ) {
			if( aio->type == ITEMTYPE_GROUP ) {
				ItemGroup *item = ( ItemGroup * ) aio;
				addritem_print_item_group( item, stream );
			}
		}
		else {
			fprintf( stream, "\t\tpid : ???\n" );
		}
		node = g_list_next( node );
	}
	fprintf( stream, "\t###\n" );
}

/**
 * Return link list of persons for specified folder. Note that the list contains
 * references to items and should be g_free() when done. Do *NOT* attempt to use the
 * addritem_free_xxx() functions... this will destroy the addressbook data!
 *
 * \param  folder Folder to process.
 * \return List of items, or <i>NULL</i> if none.
 */
GList *addritem_folder_get_person_list( ItemFolder *folder ) {
	GList *list = NULL;
	GList *node = NULL;

	cm_return_val_if_fail( folder != NULL, NULL );

	node = folder->listPerson;
	while( node ) {
		ItemPerson *person = node->data;
		list = g_list_prepend( list, person );
		node = g_list_next( node );
	}
	return g_list_reverse(list);
}

/**
 * Return link list of groups for specified folder. Note that the list contains
 * references to items and should be g_free() when done. Do *NOT* attempt to use the
 * addritem_free_xxx() functions... this will destroy the addressbook data!
 *
 * \param  folder Folder to process.
 * \return List of items, or <i>NULL</i> if none.
 */
GList *addritem_folder_get_group_list( ItemFolder *folder ) {
	GList *list = NULL;
	GList *node = NULL;

	cm_return_val_if_fail( folder != NULL, NULL );

	node = folder->listGroup;
	while( node ) {
		ItemGroup *group = node->data;
		list = g_list_prepend( list, group );
		node = g_list_next( node );
	}
	return g_list_reverse(list);
}

/**
 * Parse first and last names for person from common name.
 * \param person Person to process.
 */
void addritem_parse_first_last( ItemPerson *person ) {
	gchar *name;
	gchar *fName, *lName;
	gchar *p;
	gint len, i;

	cm_return_if_fail( person != NULL );

	name = ADDRITEM_NAME(person);
	if( name == NULL ) return;

	fName = NULL;
	lName = NULL;
	p = strchr( name, ',' );
	if( p ) {
		len = ( size_t ) ( p - name );
		lName = g_strndup( name, len );
		fName = g_strdup( p + 1 );
	}
	else {
		/* Other way around */
		i = strlen( name );
		while( i >= 0 ) {
			if( name[i] == ' ' ) {
				fName = g_strndup( name, i );
				lName = g_strdup( &name[i] );
				break;
			}
			i--;
		}
		if( fName == NULL ) {
			fName = g_strdup( name );
		}
	}

	g_free( person->firstName );
	person->firstName = fName;
	if( person->firstName )
		g_strstrip( person->firstName );

	g_free( person->lastName );
	person->lastName = lName;
	if( person->lastName )
		g_strstrip( person->lastName );
}

/**
 * Build a path of all ancestor folders for specified folder.
 * \param  folder Folder.
 * \param  seq    Path sequence, FALSE top down, TRUE bottom up.
 * \return List of folders from the top down.
 */
GList *addritem_folder_path( const ItemFolder *folder, const gboolean seq ) {
	GList *list;
	AddrItemObject *item;

	list = NULL;
	item = ( AddrItemObject * ) folder;
	if( seq ) {
		while( item ) {
			list = g_list_prepend( list, item );
			item = ADDRITEM_PARENT( item );
		}
	}
	else {
		while( item ) {
			list = g_list_append( list, item );
			item = ADDRITEM_PARENT( item );
		}
	}
	return list;
}

/**
 * Format E-Mail address.
 * \param email EMail item to format.
 * \return Formatted string. Should be freed after use.
 */
gchar *addritem_format_email( ItemEMail *email ) {
	gchar *address;
	gchar *name;
	ItemPerson *person;

	address = NULL;
	name = NULL;
	if( ADDRITEM_NAME( email ) ) {
		if( strlen( ADDRITEM_NAME( email ) ) ) {
			name = ADDRITEM_NAME( email );
		}
	}
	if( ! name ) {
		person = ( ItemPerson * ) ADDRITEM_PARENT( email );
		name = ADDRITEM_NAME( person );
	}

	if( name ) {
		if( strchr_with_skip_quote( name, '"', ',' ) ) {
			address = g_strdup_printf( "\"%s\" <%s>", name, email->address );
		}
		else {
			address = g_strdup_printf( "%s <%s>", name, email->address );
		}
	}
	else {
		address = g_strdup_printf( "%s", email->address );
	}
	return address;
}

/*
* End of Source.
*/
