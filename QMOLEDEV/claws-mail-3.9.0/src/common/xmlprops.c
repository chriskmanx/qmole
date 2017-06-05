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
 * General functions for saving properties to an XML file.
 *
 * The file is structured as follows:
 *
 *   <property-list>
 *     <property name="first-name" value="Axle" >/
 *     <property name="last-name"  value="Rose" >/
 *   </property-list>
 *		
 * ***********************************************************************
 */


#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "prefs.h"
#include "xml.h"
#include "mgutils.h"
#include "xmlprops.h"
#include "utils.h"

/* Element tag names */
#define XMLS_ELTAG_PROP_LIST     "property-list"
#define XMLS_ELTAG_PROPERTY      "property"

/* Attribute tag names */
#define XMLS_ATTAG_NAME          "name"
#define XMLS_ATTAG_VALUE         "value"

static void xmlprops_clear		( XmlProperty *props );

typedef struct _HashLoopData {
	FILE *fp;
	int error;
} HashLoopData;

/*
 * Create new props.
 */
XmlProperty *xmlprops_create( void ) {
	XmlProperty *props;

	props = g_new0( XmlProperty, 1 );
	props->path = NULL;
	props->encoding = NULL;
	props->propertyTable = g_hash_table_new( g_str_hash, g_str_equal );
	props->retVal = MGU_SUCCESS;
	return props;
}

/*
 * Properties - file path.
 */
void xmlprops_set_path( XmlProperty *props, const gchar *value ) {
	cm_return_if_fail( props != NULL );
	props->path = mgu_replace_string( props->path, value );
}

/*
 * Free hash table visitor function.
 */
static gint xmlprops_free_entry_vis( gpointer key, gpointer value, gpointer data ) {
	g_free( key );
	g_free( value );
	key = NULL;
	value = NULL;
	return TRUE;
}

/*
 * Clear all properties.
 * Enter: props Property object.
 */
static void xmlprops_clear( XmlProperty *props ) {
	cm_return_if_fail( props != NULL );
	g_hash_table_foreach_remove(
		props->propertyTable, xmlprops_free_entry_vis, NULL );
}

/*
 * Free props.
 * Enter: props Property object.
 */
void xmlprops_free( XmlProperty *props ) {
	cm_return_if_fail( props != NULL );

	/* Clear property table */
	xmlprops_clear( props );
	g_hash_table_destroy( props->propertyTable );

	/* Free up internal objects */
	g_free( props->path );
	g_free( props->encoding );

	props->path = NULL;
	props->encoding = NULL;
	props->propertyTable = NULL;
	props->retVal = 0;

	g_free( props );
}

static int xmlprops_write_elem_s( FILE *fp, gint lvl, gchar *name ) {
	gint i;
	for( i = 0; i < lvl; i++ ) {
		if(fputs( "  ", fp ) == EOF)
			return -1;
	}
	if(fputs( "<", fp ) == EOF)
		return -1;
	if(fputs( name, fp ) == EOF)
		return -1;
	
	return 0;
}

static int xmlprops_write_elem_e( FILE *fp, gint lvl, gchar *name ) {
	gint i;
	for( i = 0; i < lvl; i++ ) {
		if(fputs( "  ", fp ) == EOF)
			return -1;
	}
	if(fputs( "</", fp ) == EOF)
		return -1;
	if(fputs( name, fp ) == EOF)
		return -1;
	if(fputs( ">\n", fp ) == EOF)
		return -1;
	
	return 0;
}

static int xmlprops_write_attr( FILE *fp, gchar *name, gchar *value ) {
	if(fputs( " ", fp ) == EOF)
		return -1;
	if(fputs( name, fp ) == EOF)
		return -1;
	if(fputs( "=\"", fp ) == EOF)
		return -1;
	if(xml_file_put_escape_str( fp, value ) < 0)
		return -1;
	if(fputs( "\"", fp ) == EOF)
		return -1;
	
	return 0;
}

static void xmlprops_write_vis( gpointer key, gpointer value, gpointer d ) {
	HashLoopData *data = (HashLoopData *)d;

	if(xmlprops_write_elem_s( data->fp, 1, XMLS_ELTAG_PROPERTY ) < 0)
		data->error = 1;
	if(xmlprops_write_attr( data->fp, XMLS_ATTAG_NAME, key ) < 0)
		data->error = 1;
	if(xmlprops_write_attr( data->fp, XMLS_ATTAG_VALUE, value ) < 0)
		data->error = 1;
	if(fputs( " />\n", data->fp ) == EOF)
		data->error = 1;
}

static gint xmlprops_write_to( XmlProperty *props, const gchar *fileSpec ) {
	PrefFile *pfile;
	FILE *fp;
	HashLoopData data;

	props->retVal = MGU_OPEN_FILE;
	pfile = prefs_write_open( fileSpec );
	if( pfile ) {
		fp = pfile->fp;
		if(fprintf( fp, "<?xml version=\"1.0\"" ) < 0)
			goto revert;
		if( props->encoding && *props->encoding ) {
			if(fprintf( fp, " encoding=\"%s\"", props->encoding ) < 0)
				goto revert;
		}
		if(fprintf( fp, " ?>\n" ) < 0)
			goto revert;
		if(xmlprops_write_elem_s( fp, 0, XMLS_ELTAG_PROP_LIST ) < 0)
			goto revert;
		if(fputs( ">\n", fp ) == EOF)
			goto revert;

		/* Output all properties */
		data.fp = fp;
		data.error = 0;
		g_hash_table_foreach( props->propertyTable, xmlprops_write_vis, &data );

		if (data.error)
			goto revert;

		if(xmlprops_write_elem_e( fp, 0, XMLS_ELTAG_PROP_LIST ) < 0)
			goto revert;
		
		props->retVal = MGU_SUCCESS;
		if( prefs_file_close( pfile ) < 0 ) {
			props->retVal = MGU_ERROR_WRITE;
			goto out;
		}
		goto out;
revert:
		props->retVal = MGU_ERROR_WRITE;
		if( prefs_file_close_revert( pfile ) < 0 ) {
			props->retVal = MGU_ERROR_WRITE;
		}
	
	}
out:
	return props->retVal;
}

/*
 * Save properties to file.
 * return: Status code.
 */
gint xmlprops_save_file( XmlProperty *props ) {
	cm_return_val_if_fail( props != NULL, -1 );

	props->retVal = MGU_NO_FILE;
	if( props->path == NULL || *props->path == '\0' ) return props->retVal;
	xmlprops_write_to( props, props->path );

	return props->retVal;
}

static void xmlprops_save_property(
		XmlProperty *props, const gchar *name, const gchar *value )
{
	gchar *key;
	gchar *val;

	if( strlen( name ) == 0 ) return;
	if( strlen( value ) == 0 ) return;
	if( g_hash_table_lookup( props->propertyTable, name ) ) return;
	key = g_strdup( name );
	val = g_strdup( value );
	g_hash_table_insert( props->propertyTable, key, val );
}

#define ATTR_BUFSIZE 256

static void xmlprops_read_props( XmlProperty *props, XMLFile *file ) {
	GList *attr;
	gchar *name, *value;
	gchar pName[ ATTR_BUFSIZE ];
	gchar pValue[ ATTR_BUFSIZE ];

	while( TRUE ) {
		*pName = '\0';
		*pValue = '\0';
		if (! file->level ) break;
		xml_parse_next_tag( file );
		xml_get_current_tag( file );
		if( xml_compare_tag( file, XMLS_ELTAG_PROPERTY ) ) {
			attr = xml_get_current_tag_attr( file );
			while( attr ) {
				name = ( ( XMLAttr * ) attr->data )->name;
				value = ( ( XMLAttr * ) attr->data )->value;
				if( strcmp( name, XMLS_ATTAG_NAME ) == 0 ) {
					strcpy( pName, value );
				}
				else if( strcmp( name, XMLS_ATTAG_VALUE ) == 0 ) {
					strcpy( pValue, value );
				}
				attr = g_list_next( attr );
			}
			xmlprops_save_property( props, pName, pValue );
		}
	}
}

#undef ATTR_BUFSIZE

/*
 * Load properties from file.
 * return: Status code.
 */
gint xmlprops_load_file( XmlProperty *props ) {
	XMLFile *file = NULL;

	cm_return_val_if_fail( props != NULL, -1 );
	props->retVal = MGU_NO_FILE;
	file = xml_open_file( props->path );
	if( file == NULL ) {
		return props->retVal;
	}

	props->retVal = MGU_BAD_FORMAT;
	if( xml_get_dtd( file ) == 0 ) {
		if( xml_parse_next_tag( file ) == 0 ) {
			if( xml_compare_tag( file, XMLS_ELTAG_PROP_LIST ) ) {
				xmlprops_read_props( props, file );
				props->retVal = MGU_SUCCESS;
			}
		}
	}
	xml_close_file( file );

	return props->retVal;
}

/*
 * Set property.
 * Enter: props Property object.
 *        name  Property name.
 *        value New value to save.
 */
void xmlprops_set_property(
		XmlProperty *props, const gchar *name, const gchar *value )
{
	gchar *key = NULL;
	gchar *val;

	cm_return_if_fail( props != NULL );
	if( name == NULL || strlen( name ) == 0 ) return;
	if( value == NULL || strlen( value ) == 0 ) return;
	val = g_hash_table_lookup( props->propertyTable, name );
	if( val == NULL ) {
		key = g_strdup( name );
	}
	else {
		g_free( val );
	}
	val = g_strdup( value );
	g_hash_table_insert( props->propertyTable, key, val );
}

/*
 * Set property to integer value.
 * Enter: props Property object.
 *        name  Property name.
 *        value New value to save.
 */
void xmlprops_set_property_i(
		XmlProperty *props, const gchar *name, const gint value )
{
	gchar buf[32];

	cm_return_if_fail( props != NULL );
	sprintf( buf, "%d", value );
	xmlprops_set_property( props, name, buf );
}

/*
 * Set property to boolean value.
 * Enter: props Property object.
 *        name  Property name.
 *        value New value to save.
 */
void xmlprops_set_property_b(
		XmlProperty *props, const gchar *name, const gboolean value )
{
	cm_return_if_fail( props != NULL );
	if( value ) {
		xmlprops_set_property( props, name, "y" );
	}
	else {
		xmlprops_set_property( props, name, "n" );
	}
}

/*
 * Get property into a buffer.
 * Enter:  props Property object.
 *         name  Property name.
 * Return: value found, or NULL if none. Should be g_free() when done.
 */
void xmlprops_get_property_s(
		XmlProperty *props, const gchar *name, gchar *buffer ) {
	gchar *val;

	cm_return_if_fail( props != NULL );
	if( buffer == NULL ) return;
	val = g_hash_table_lookup( props->propertyTable, name );
	if( val ) {
		strcpy( buffer, val );
	}
}

/*
 * Get property as integer value.
 * Enter:  props Property object.
 *         name  Property name.
 * Return: value found, or zero if not found.
 */
gint xmlprops_get_property_i( XmlProperty *props, const gchar *name ) {
	gchar *val;
	gchar *endptr;
	gint value;

	value = 0;
	cm_return_val_if_fail( props != NULL, value );
	val = g_hash_table_lookup( props->propertyTable, name );
	if( val ) {
		endptr = NULL;
		value = strtol( val, &endptr, 10 );
	}
	return value;
}

/*
 * Get property as boolean value.
 * Enter:  props Property object.
 *         name  Property name.
 * Return: value found, or FALSE if not found.
 */
gboolean xmlprops_get_property_b( XmlProperty *props, const gchar *name ) {
	gchar *val;
	gboolean value;

	value = FALSE;
	cm_return_val_if_fail( props != NULL, value );
	val = g_hash_table_lookup( props->propertyTable, name );
	if( val ) {
		value = ( g_ascii_strcasecmp( val, "y" ) == 0 );
	}
	return value;
}

/*
* End of Source.
*/


