/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2011 Match Grun and the Claws Mail team
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
 * Address list item selection objects.
 */

#include <stdio.h>
#include <glib.h>

#include "addrselect.h"
#include "addressitem.h"
#include "mgutils.h"

static AddrSelectItem *addrselect_create_item	( AddrItemObject *aio );

/**
 * Create a selection record from an address cache item.
 * \param  aio Item object.
 * \return Address select item.
 */
static AddrSelectItem *addrselect_create_item( AddrItemObject *aio ) {
	AddrSelectItem *item = NULL;

	if( aio ) {
		item = g_new0( AddrSelectItem, 1 );
		item->objectType = aio->type;
		item->addressItem = aio;
		item->uid = g_strdup( aio->uid );
		item->cacheID = NULL;
	}
	return item;
}

/**
 * Create a selection record from an address object (in tree node).
 * \param obj Address object.
 * \return Address select item.
 */
AddrSelectItem *addrselect_create_node( AddressObject *obj ) {
	AddrSelectItem *item = NULL;

	if( obj ) {
		item = g_new0( AddrSelectItem, 1 );
		item->objectType = addressbook_type2item( obj->type );
		item->addressItem = NULL;
		item->uid = NULL;
		item->cacheID = NULL;
	}
	return item;
}

/**
 * Create a copy of a selection record.
 * Enter: item Address entry to copy.
 * \return Address select item.
 */
AddrSelectItem *addrselect_item_copy( AddrSelectItem *item ) {
	AddrSelectItem *copy = NULL;

	if( item ) {
		copy = g_new0( AddrSelectItem, 1 );
		copy->objectType = item->objectType;
		copy->addressItem = item->addressItem;
		copy->uid = g_strdup( item->uid );
		copy->cacheID = g_strdup( item->cacheID );
	}
	return copy;
}

/**
 * Free selection record.
 * \return Address select item.
 */
void addrselect_item_free( AddrSelectItem *item ) {
	if( item ) {
		g_free( item->uid );
		g_free( item->cacheID );
		item->objectType = ITEMTYPE_NONE;
		item->addressItem = NULL;
		item->uid = NULL;
		item->cacheID = NULL;
	}
	g_free( item );
}

/**
 * Print address selection item.
 * \param item   Address select item.
 * \param stream Output stream.
 */
void addrselect_item_print( AddrSelectItem *item, FILE *stream ) {
	fprintf( stream, "Select Record\n" );
	fprintf( stream, "obj type: %d\n", item->objectType );
	fprintf( stream, "     uid: %s\n", item->uid );
	fprintf( stream, "cache id: %s\n", item->cacheID );
	fprintf( stream, "---\n" );
}

/**
 * Create a new address selection object.
 * \return Initialized object.
 */
AddrSelectList *addrselect_list_create() {
	AddrSelectList *asl;

	asl = g_new0( AddrSelectList, 1 );
	asl->listSelect = NULL;
	return asl;
}

/**
 * Clear list of selection records.
 * \param asl  List to process.
 */
void addrselect_list_clear( AddrSelectList *asl ) {
	GList *node;

	cm_return_if_fail( asl != NULL );
	node = asl->listSelect;
	while( node ) {
		AddrSelectItem *item;

		item = node->data;
		addrselect_item_free( item );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( asl->listSelect );
	asl->listSelect = NULL;
}

/**
 * Free selection list.
 * \param asl  List to free.
 */
void addrselect_list_free( AddrSelectList *asl ) {
	cm_return_if_fail( asl != NULL );

	addrselect_list_clear( asl );
	g_list_free( asl->listSelect );
	asl->listSelect = NULL;
	g_free( asl );
}

/**
 * Test whether selection is empty.
 * \param  asl List to test.
 * \return <i>TRUE</i> if list is empty.
 */
gboolean addrselect_test_empty( AddrSelectList *asl ) {
	cm_return_val_if_fail( asl != NULL, TRUE );
	return ( asl->listSelect == NULL );
}

/**
 * Return list of AddrSelectItem objects.
 * \param  asl  List to process.
 * \return List of selection items. The list should should be freed with
 *         <code>g_list_free()</code> when done. Items contained in the
 *         list should <b>not</b> be freed!!!
 */
GList *addrselect_get_list( AddrSelectList *asl ) {
	GList *node, *list;

	cm_return_val_if_fail(asl != NULL, NULL);
	list = NULL;
	node = asl->listSelect;
	while( node ) {
		list = g_list_append( list, node->data );
		node = g_list_next( node );
	}
	return list;
}

/**
 * Format address item.
 * \param  aio Item.
 * \return Formatted address.
 */
static gchar *addrselect_format_address( AddrItemObject * aio ) {
	gchar *buf = NULL;
	gchar *name = NULL;
	gchar *address = NULL;

	if( aio->type == ADDR_ITEM_EMAIL ) {
		ItemPerson *person = NULL;
		ItemEMail *email = ( ItemEMail * ) aio;

		person = ( ItemPerson * ) ADDRITEM_PARENT(email);
		if( email->address ) {
			if( ADDRITEM_NAME(email) ) {
				name = ADDRITEM_NAME(email);
				if( *name == '\0' ) {
					name = ADDRITEM_NAME(person);
				}
			}
			else if( ADDRITEM_NAME(person) ) {
				name = ADDRITEM_NAME(person);
			}
			else {
				buf = g_strdup( email->address );
			}
			address = email->address;
		}
	}
	else if( aio->type == ADDR_ITEM_PERSON ) {
		ItemPerson *person = ( ItemPerson * ) aio;
		GList *node = person->listEMail;

		name = ADDRITEM_NAME(person);
		if( node ) {
			ItemEMail *email = ( ItemEMail * ) node->data;
			address = email->address;
		}
	}
	if( address ) {
		if( name ) {
			buf = g_strdup_printf( "%s <%s>", name, address );
		}
		else {
			buf = g_strdup( address );
		}
	}
	return buf;
}

/**
 * Test whether specified object is in list.
 * \param list List to check.
 * \param aio  Object to test.
 * \param item found, or <i>NULL</i> if not in list.
 */
static AddrSelectItem *addrselect_list_find( GList *list, AddrItemObject *aio ) {
	GList *node;

	node = list;
	while( node ) {
		AddrSelectItem *item;

		item = node->data;
		if( item->addressItem == aio ) return item;
		node = g_list_next( node );
	}
	return NULL;
}

/**
 * Add a single object into the list.
 * \param asl     Address selection object.
 * \param aio     Address object.
 * \param cacheID Cache ID. Should be freed after calling function.
 */
void addrselect_list_add_obj( AddrSelectList *asl, AddrItemObject *aio, gchar *cacheID ) {
	AddrSelectItem *item;

	cm_return_if_fail( asl != NULL );
	if( aio == NULL ) return;

	/* Check whether object is in list */
	if( addrselect_list_find( asl->listSelect, aio ) ) return;

	if( aio->type == ADDR_ITEM_PERSON ||
	    aio->type == ADDR_ITEM_EMAIL ||
	    aio->type == ADDR_ITEM_GROUP ) {
		item = addrselect_create_item( aio );
		item->cacheID = g_strdup( cacheID );
		asl->listSelect = g_list_append( asl->listSelect, item );
	}
	/* addrselect_list_show( asl, stdout ); */
}

/**
 * Add a single item into the list.
 * \param  asl     Address selection object.
 * \param  item    Address select item.
 * \param  cacheID Cache ID. Should be g_free() after calling function.
 */
void addrselect_list_add( AddrSelectList *asl, AddrSelectItem *item, gchar *cacheID ) {
	cm_return_if_fail( asl != NULL );
	if( item == NULL ) return;

	/* Check whether object is in list */
	if( g_list_find( asl->listSelect, item ) ) return;

	item->cacheID = g_strdup( cacheID );
	asl->listSelect = g_list_append( asl->listSelect, item );
}

/**
 * Remove specified object from list.
 * \param asl  Address selection object.
 * \param aio  Object to remove.
 */
void addrselect_list_remove( AddrSelectList *asl, AddrItemObject *aio ) {
	GList *node;
	AddrSelectItem *item;

	cm_return_if_fail( asl != NULL );
	if( aio == NULL ) return;
	node = asl->listSelect;
	while( node ) {
		item = node->data;
		if( item->addressItem == aio ) {
			addrselect_item_free( item );
			node->data = NULL;
			asl->listSelect = g_list_remove_link( asl->listSelect, node );
			break;
		}
		node = g_list_next( node );
	}
	/* addrselect_list_show( list, stdout ); */
}

/**
 * Build list of formatted addresses.
 * \param  asl List to process.
 * \return List of addresses, formatted as character strings. List should be
 *         freed when no longer required.
 */
GList *addrselect_build_list( AddrSelectList *asl ) {
	GList *list;
	GList *node;

	cm_return_val_if_fail(asl != NULL, NULL);
	list = NULL;
	node = asl->listSelect;
	while( node != NULL ) {
		AddrSelectItem *item;
		AddrItemObject *aio;
		gchar *addr;

		item = node->data;
		aio = ( AddrItemObject * ) item->addressItem;
		if( aio ) {
			if( aio->type == ADDR_ITEM_GROUP ) {
				ItemGroup *group = ( ItemGroup * ) aio;
				GList *node = group->listEMail;
				while( node ) {
					ItemEMail *email = node->data;
					addr = addrselect_format_address(
						( AddrItemObject * ) email );
					if( addr ) {
						list = g_list_append( list, addr );
					}
					node = g_list_next( node );
				}
			}
			else {
				addr = addrselect_format_address( aio );
				if( addr ) {
					list = g_list_append( list, addr );
				}
			}
		}
		node = g_list_next( node );
	}
	return list;
}

/*
* End of Source.
*/


