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
 * Functions necessary to access vCard files. vCard files are used
 * by GnomeCard for addressbook, and Netscape for sending business
 * card information. Refer to RFC2426 for more information.
 */

#include <glib.h>
#include <sys/stat.h>
#include <string.h>

#include "mgutils.h"
#include "vcard.h"
#include "addritem.h"
#include "addrcache.h"
#include "adbookbase.h"
#include "utils.h"
#include "codeconv.h"
#include "quoted-printable.h"

#define GNOMECARD_DIR     ".gnome"
#define GNOMECARD_FILE    "GnomeCard"
#define GNOMECARD_SECTION "[file]"
#define GNOMECARD_PARAM   "open"

#define VCARD_TEST_LINES  200

/*
* Create new cardfile object.
*/
VCardFile *vcard_create() {
	VCardFile *cardFile;
	cardFile = g_new0( VCardFile, 1 );
	cardFile->type = ADBOOKTYPE_VCARD;
	cardFile->addressCache = addrcache_create();
	cardFile->retVal = MGU_SUCCESS;

	cardFile->file = NULL;
	cardFile->path = NULL;
	cardFile->bufptr = cardFile->buffer;
	return cardFile;
}

/*
* Properties...
*/
void vcard_set_name( VCardFile* cardFile, const gchar *value ) {
	cm_return_if_fail( cardFile != NULL );
	addrcache_set_name( cardFile->addressCache, value );
}
void vcard_set_file( VCardFile* cardFile, const gchar *value ) {
	cm_return_if_fail( cardFile != NULL );
	addrcache_refresh( cardFile->addressCache );
	cardFile->path = mgu_replace_string( cardFile->path, value );
	g_strstrip( cardFile->path );
}
void vcard_set_accessed( VCardFile *cardFile, const gboolean value ) {
	cm_return_if_fail( cardFile != NULL );
	cardFile->addressCache->accessFlag = value;
}

/*
* Test whether file was modified since last access.
* Return: TRUE if file was modified.
*/
gboolean vcard_get_modified( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, FALSE );
	cardFile->addressCache->modified =
		addrcache_check_file( cardFile->addressCache, cardFile->path );
	return cardFile->addressCache->modified;
}
gboolean vcard_get_accessed( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, FALSE );
	return cardFile->addressCache->accessFlag;
}

/*
* Test whether file was read.
* Return: TRUE if file was read.
*/
gboolean vcard_get_read_flag( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, FALSE );
	return cardFile->addressCache->dataRead;
}

/*
* Return status code from last file operation.
* Return: Status code.
*/
gint vcard_get_status( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, -1 );
	return cardFile->retVal;
}

ItemFolder *vcard_get_root_folder( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, NULL );
	return addrcache_get_root_folder( cardFile->addressCache );
}
gchar *vcard_get_name( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, NULL );
	return addrcache_get_name( cardFile->addressCache );
}

/*
* Create new cardfile object for specified file.
*/
static VCardFile *vcard_create_path( const gchar *path ) {
	VCardFile *cardFile;
	cardFile = vcard_create();
	vcard_set_file(cardFile, path);
	return cardFile;
}

/*
* Free up cardfile object by releasing internal memory.
*/
void vcard_free( VCardFile *cardFile ) {
	cm_return_if_fail( cardFile != NULL );

	/* Close file */
	if( cardFile->file ) fclose( cardFile->file );

	/* Clear cache */
	addrcache_clear( cardFile->addressCache );
	addrcache_free( cardFile->addressCache );

	/* Free internal stuff */
	g_free( cardFile->path );

	/* Clear pointers */
	cardFile->file = NULL;
	cardFile->path = NULL;
	cardFile->bufptr = NULL;

	cardFile->type = ADBOOKTYPE_NONE;
	cardFile->addressCache = NULL;
	cardFile->retVal = MGU_SUCCESS;

	/* Now release file object */
	g_free( cardFile );
}

/*
* Open file for read.
* return: TRUE if file opened successfully.
*/
static gint vcard_open_file( VCardFile* cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, -1 );

	/* g_print( "Opening file\n" ); */
	cardFile->addressCache->dataRead = FALSE;
	if( cardFile->path ) {
		cardFile->file = g_fopen( cardFile->path, "rb" );
		if( ! cardFile->file ) {
			/* g_printerr( "can't open %s\n", cardFile->path ); */
			cardFile->retVal = MGU_OPEN_FILE;
			return cardFile->retVal;
		}
	}
	else {
		/* g_printerr( "file not specified\n" ); */
		cardFile->retVal = MGU_NO_FILE;
		return cardFile->retVal;
	}

	/* Setup a buffer area */
	cardFile->buffer[0] = '\0';
	cardFile->bufptr = cardFile->buffer;
	cardFile->retVal = MGU_SUCCESS;
	return cardFile->retVal;
}

/*
* Close file.
*/
static void vcard_close_file( VCardFile *cardFile ) {
	cm_return_if_fail( cardFile != NULL );
	if( cardFile->file ) fclose( cardFile->file );
	cardFile->file = NULL;
}

/*
* Read line of text from file.
* Return: ptr to buffer where line starts.
*/
static gchar *vcard_read_line( VCardFile *cardFile ) {
	while( *cardFile->bufptr == '\n' || *cardFile->bufptr == '\0' ) {
		if( fgets( cardFile->buffer, VCARDBUFSIZE, cardFile->file ) == NULL )
			return NULL;
		g_strstrip( cardFile->buffer );
		cardFile->bufptr = cardFile->buffer;
	}
	return cardFile->bufptr;
}

/*
* Read line of text from file.
* Return: ptr to buffer where line starts.
*/
static gchar *vcard_get_line( VCardFile *cardFile ) {
	gchar buf[ VCARDBUFSIZE ];
	gchar *start, *end;
	gint len;

	if (vcard_read_line( cardFile ) == NULL ) {
		buf[0] = '\0';
		return NULL;
	}

	/* Copy into private buffer */
	start = cardFile->bufptr;
	len = strlen( start );
	end = start + len;
	strncpy( buf, start, len );
	buf[ len ] = '\0';
	g_strstrip(buf);
	cardFile->bufptr = end + 1;

	/* Return a copy of buffer */	
	return g_strdup( buf );
}

/*
* Free linked lists of character strings.
*/
static void vcard_free_lists( GSList *listName, GSList *listAddr, GSList *listRem, GSList* listID ) {
	mgu_free_list( listName );
	mgu_free_list( listAddr );
	mgu_free_list( listRem );
	mgu_free_list( listID );
}

/*
* Read quoted-printable text, which may span several lines into one long string.
* Param: cardFile - object.
* Param: tagvalue - will be placed into the linked list.
*/
static gchar *vcard_read_qp( VCardFile *cardFile, char *tagvalue ) {
	GSList *listQP = NULL;
	gint len = 0;
	gchar *line = tagvalue;
	while( line ) {
		listQP = g_slist_append( listQP, line );
		len = strlen( line ) - 1;
		if( line[ len ] != '=' ) break;
		line[ len ] = '\0';
		line = vcard_get_line( cardFile );
	}

	/* Coalesce linked list into one long buffer. */
	line = mgu_list_coalesce( listQP );

	/* Clean up */
	mgu_free_list( listQP );
	listQP = NULL;
	return line;
}

/*
* Parse tag name from line buffer.
* Return: Buffer containing the tag name, or NULL if no delimiter char found.
*/
static gchar *vcard_get_tagname( char* line, gchar dlm ) {
	gint len = 0;
	gchar *tag = NULL;
	gchar *lptr = line;
	gchar *down;
	while( *lptr++ ) {
		if( *lptr == dlm ) {
			len = lptr - line;
			tag = g_strndup( line, len+1 );
			tag[ len ] = '\0';
			down = g_utf8_strdown( tag, -1 );
			g_free(tag);
			return down;
		}
	}
	return tag;
}

/*
* Parse tag value from line buffer.
* Return: Buffer containing the tag value. Empty string is returned if
* no delimiter char found.
*/
static gchar *vcard_get_tagvalue( gchar* line, gchar dlm ) {
	gchar *value = NULL;
	gchar *start = NULL;
	gchar *lptr;
	gint len = 0;

	for( lptr = line; *lptr; lptr++ ) {
		if( *lptr == dlm ) {
			if( ! start )
				start = lptr + 1;
		}
	}
	if( start ) {
		len = lptr - start;
		value = g_strndup( start, len+1 );
	}
	else {
		/* Ensure that we get an empty string */
		value = g_strndup( "", 1 );
	}
	value[ len ] = '\0';
	return value;
}

/*
* Build an address list entry and append to list of address items.
*/
static void vcard_build_items(
	VCardFile *cardFile, GSList *listName, GSList *listAddr,
	GSList *listRem, GSList *listID )
{
	GSList *nodeName = listName;
	GSList *nodeID = listID;
	gchar *str;
	while( nodeName ) {
		GSList *nodeAddress = listAddr;
		GSList *nodeRemarks = listRem;
		ItemPerson *person = addritem_create_item_person();
		addritem_person_set_common_name( person, nodeName->data );
		while( nodeAddress ) {
			str = nodeAddress->data;
			if( *str != '\0' ) {
				ItemEMail *email = addritem_create_item_email();
				addritem_email_set_address( email, str );
				if( nodeRemarks ) {
					str = nodeRemarks->data;
					if( str ) {
						if( g_utf8_collate( str, "internet" ) != 0 ) {
							if( *str != '\0' )
								addritem_email_set_remarks( email, str );
						}
					}
				}
				addrcache_id_email( cardFile->addressCache, email );
				addrcache_person_add_email( cardFile->addressCache, person, email );
			}
			nodeAddress = g_slist_next( nodeAddress );
			nodeRemarks = g_slist_next( nodeRemarks );
		}
		if( person->listEMail ) {
			addrcache_id_person( cardFile->addressCache, person );
			addrcache_add_person( cardFile->addressCache, person );
			if( nodeID ) {
				str = nodeID->data;
				addritem_person_set_external_id( person, str );
			}
		}
		else {
			addritem_free_item_person( person );
		}
		nodeName = g_slist_next( nodeName );
		nodeID = g_slist_next( nodeID );
	}
}

/* Unescape characters in quoted-printable string. */
static gchar *vcard_unescape_qp( gchar *value ) {
	gchar *res = NULL;
	gint len;
	if (value == NULL)
		return NULL;
		
	len = strlen(value);
	res = g_malloc(len);
	qp_decode_const(res, len-1, value);
	if (!g_utf8_validate(res, -1, NULL)) {
		gchar *mybuf = g_malloc(strlen(res)*2 +1);
		conv_localetodisp(mybuf, strlen(res)*2 +1, res);
		g_free(res);
		res = mybuf;
	}
	return res;
}

 /*
* Read file data into root folder.
* Note that one vCard can have multiple E-Mail addresses (MAIL tags);
* these are broken out into separate address items. An address item
* is generated for the person identified by FN tag and each EMAIL tag.
* If a sub-type is included in the EMAIL entry, this will be used as
* the Remarks member. Also note that it is possible for one vCard
* entry to have multiple FN tags; this might not make sense. However,
* it will generate duplicate address entries for each person listed.
*/
static void vcard_read_file( VCardFile *cardFile ) {
	gchar *tagtemp = NULL, *tagname = NULL, *tagvalue = NULL, *tagtype = NULL;
	GSList *listName = NULL, *listAddress = NULL, *listRemarks = NULL, *listID = NULL;
	/* GSList *listQP = NULL; */

	for( ;; ) {
		gchar *line =  vcard_get_line( cardFile );
		if( line == NULL ) break;

		/* g_print( "%s\n", line ); */

		/* Parse line */
		tagtemp = vcard_get_tagname( line, VCARD_SEP_TAG );
		if( tagtemp == NULL ) {
			g_free( line );
			continue;
		}

		/* g_print( "\ttemp:  %s\n", tagtemp ); */
		tagvalue = vcard_get_tagvalue( line, VCARD_SEP_TAG );
		if( tagvalue == NULL ) {
			g_free( tagtemp );
			g_free( line );
			continue;
		}

		tagname = vcard_get_tagname( tagtemp, VCARD_SEP_TYPE );
		tagtype = vcard_get_tagvalue( tagtemp, VCARD_SEP_TYPE );
		if( tagname == NULL ) {
			tagname = tagtemp;
			tagtemp = NULL;
		}

		/* g_print( "\tname:  %s\n", tagname ); */
		/* g_print( "\ttype:  %s\n", tagtype ); */
		/* g_print( "\tvalue: %s\n", tagvalue ); */

		if( g_utf8_collate( tagtype, VCARD_TYPE_QP ) == 0 ) {
			gchar *tmp;
			/* Quoted-Printable: could span multiple lines */
			tagvalue = vcard_read_qp( cardFile, tagvalue );
			tmp = vcard_unescape_qp( tagvalue );
			g_free(tagvalue);
			tagvalue=tmp;
			/* g_print( "QUOTED-PRINTABLE !!! final\n>%s<\n", tagvalue ); */
		}

		if( g_utf8_collate( tagname, VCARD_TAG_START ) == 0 &&
			g_ascii_strcasecmp( tagvalue, VCARD_NAME ) == 0 ) {
			/* g_print( "start card\n" ); */
			vcard_free_lists( listName, listAddress, listRemarks, listID );
			listName = listAddress = listRemarks = listID = NULL;
		}
		if( g_utf8_collate( tagname, VCARD_TAG_FULLNAME ) == 0 ) {
			/* g_print( "- full name: %s\n", tagvalue ); */
			listName = g_slist_append( listName, g_strdup( tagvalue ) );
		}
		if( g_utf8_collate( tagname, VCARD_TAG_EMAIL ) == 0 ) {
			/* g_print( "- address: %s\n", tagvalue ); */
			listAddress = g_slist_append( listAddress, g_strdup( tagvalue ) );
			listRemarks = g_slist_append( listRemarks, g_strdup( tagtype ) );
		}
		if( g_utf8_collate( tagname, VCARD_TAG_UID ) == 0 ) {
			/* g_print( "- id: %s\n", tagvalue ); */
			listID = g_slist_append( listID, g_strdup( tagvalue ) );
		}
		if( g_utf8_collate( tagname, VCARD_TAG_END ) == 0 &&
			g_ascii_strcasecmp( tagvalue, VCARD_NAME ) == 0 ) {
			/* vCard is complete */
			/* g_print( "end card\n--\n" ); */
			/* vcard_dump_lists( listName, listAddress, listRemarks, listID, stdout ); */
			vcard_build_items( cardFile, listName, listAddress, listRemarks, listID );
			vcard_free_lists( listName, listAddress, listRemarks, listID );
			listName = listAddress = listRemarks = listID = NULL;
		}

		g_free( tagname );
		g_free( tagtype );
		g_free( tagvalue );
		g_free( tagtemp );
		g_free( line );
		line = NULL;
	}

	/* Free lists */
	vcard_free_lists( listName, listAddress, listRemarks, listID );
	listName = listAddress = listRemarks = listID = NULL;
}

/* ============================================================================================ */
/*
* Read file into list. Main entry point
* Return: TRUE if file read successfully.
*/
/* ============================================================================================ */
gint vcard_read_data( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, -1 );

	cardFile->retVal = MGU_SUCCESS;
	cardFile->addressCache->accessFlag = FALSE;
	if( addrcache_check_file( cardFile->addressCache, cardFile->path ) ) {
		addrcache_clear( cardFile->addressCache );
		vcard_open_file( cardFile );
		if( cardFile->retVal == MGU_SUCCESS ) {
			/* Read data into the list */
			vcard_read_file( cardFile );
			vcard_close_file( cardFile );

			/* Mark cache */
			addrcache_mark_file( cardFile->addressCache, cardFile->path );
			cardFile->addressCache->modified = FALSE;
			cardFile->addressCache->dataRead = TRUE;
		}
	}
	return cardFile->retVal;
}

/*
* Return link list of persons.
*/
GList *vcard_get_list_person( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, NULL );
	return addrcache_get_list_person( cardFile->addressCache );
}

/*
* Return link list of folders. This is always NULL since there are
* no folders in GnomeCard.
* Return: NULL.
*/
GList *vcard_get_list_folder( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, NULL );
	return NULL;
}

/*
* Return link list of all persons. Note that the list contains references
* to items. Do *NOT* attempt to use the addrbook_free_xxx() functions...
* this will destroy the addressbook data!
* Return: List of items, or NULL if none.
*/
GList *vcard_get_all_persons( VCardFile *cardFile ) {
	cm_return_val_if_fail( cardFile != NULL, NULL );
	return addrcache_get_all_persons( cardFile->addressCache );
}

#define WORK_BUFLEN 1024

/*
* Attempt to find a valid GnomeCard file.
* Return: Filename, or home directory if not found. Filename should
*	be g_free() when done.
*/
gchar *vcard_find_gnomecard( void ) {
	const gchar *homedir;
	gchar buf[ WORK_BUFLEN ];
	gchar str[ WORK_BUFLEN + 1 ];
	gchar *fileSpec;
	gint len, lenlbl, i;
	FILE *fp;

	homedir = get_home_dir();
	if( ! homedir ) return NULL;

	strncpy( str, homedir, WORK_BUFLEN );
	len = strlen( str );
	if( len > 0 ) {
		if( str[ len-1 ] != G_DIR_SEPARATOR ) {
			str[ len ] = G_DIR_SEPARATOR;
			str[ ++len ] = '\0';
		}
	}
	strncat( str, GNOMECARD_DIR, WORK_BUFLEN - strlen(str) );
	strncat( str, G_DIR_SEPARATOR_S, WORK_BUFLEN - strlen(str) );
	strncat( str, GNOMECARD_FILE, WORK_BUFLEN - strlen(str) );

	fileSpec = NULL;
	if( ( fp = g_fopen( str, "rb" ) ) != NULL ) {
		/* Read configuration file */
		lenlbl = strlen( GNOMECARD_SECTION );
		while( fgets( buf, sizeof( buf ), fp ) != NULL ) {
			if( 0 == g_ascii_strncasecmp( buf, GNOMECARD_SECTION, lenlbl ) ) {
				break;
			}
		}

		while( fgets( buf, sizeof( buf ), fp ) != NULL ) {
			g_strchomp( buf );
			if( buf[0] == '[' ) break;
			for( i = 0; i < lenlbl; i++ ) {
				if( buf[i] == '=' ) {
					if( 0 == g_ascii_strncasecmp( buf, GNOMECARD_PARAM, i ) ) {
						fileSpec = g_strdup( buf + i + 1 );
						g_strstrip( fileSpec );
					}
				}
			}
		}
		fclose( fp );
	}

	if( fileSpec == NULL ) {
		/* Use the home directory */
		str[ len ] = '\0';
		fileSpec = g_strdup( str );
	}

	return fileSpec;
}

/*
* Attempt to read file, testing for valid vCard format.
* Return: TRUE if file appears to be valid format.
*/
gint vcard_test_read_file( const gchar *fileSpec ) {
	gboolean haveStart;
	gchar *tagtemp = NULL, *tagname = NULL, *tagvalue = NULL, *tagtype = NULL, *line;
	VCardFile *cardFile;
	gint retVal, lines;

	if( ! fileSpec ) return MGU_NO_FILE;

	cardFile = vcard_create_path( fileSpec );
	cardFile->retVal = MGU_SUCCESS;
	vcard_open_file( cardFile );
	if( cardFile->retVal == MGU_SUCCESS ) {
		cardFile->retVal = MGU_BAD_FORMAT;
		haveStart = FALSE;
		lines = VCARD_TEST_LINES;
		while( lines > 0 ) {
			lines--;
			if( ( line =  vcard_get_line( cardFile ) ) == NULL ) break;

			/* Parse line */
			tagtemp = vcard_get_tagname( line, VCARD_SEP_TAG );
			if( tagtemp == NULL ) {
				g_free( line );
				continue;
			}

			tagvalue = vcard_get_tagvalue( line, VCARD_SEP_TAG );
			if( tagvalue == NULL ) {
				g_free( tagtemp );
				g_free( line );
				continue;
			}

			tagname = vcard_get_tagname( tagtemp, VCARD_SEP_TYPE );
			tagtype = vcard_get_tagvalue( tagtemp, VCARD_SEP_TYPE );
			if( tagname == NULL ) {
				tagname = tagtemp;
				tagtemp = NULL;
			}

			if( g_utf8_collate( tagtype, VCARD_TYPE_QP ) == 0 ) {
				gchar *tmp;
				/* Quoted-Printable: could span multiple lines */
				tagvalue = vcard_read_qp( cardFile, tagvalue );
				tmp = vcard_unescape_qp( tagvalue );
				g_free(tagvalue);
				tagvalue=tmp;
			}
			if( g_utf8_collate( tagname, VCARD_TAG_START ) == 0 &&
				g_ascii_strcasecmp( tagvalue, VCARD_NAME ) == 0 ) {
				haveStart = TRUE;
			}
			if( g_utf8_collate( tagname, VCARD_TAG_END ) == 0 &&
				g_ascii_strcasecmp( tagvalue, VCARD_NAME ) == 0 ) {
				/* vCard is complete */
				if( haveStart ) cardFile->retVal = MGU_SUCCESS;
			}

			g_free( tagname );
			g_free( tagtype );
			g_free( tagvalue );
			g_free( tagtemp );
			g_free( line );
		}
		vcard_close_file( cardFile );
	}
	retVal = cardFile->retVal;
	vcard_free( cardFile );
	cardFile = NULL;
	return retVal;
}

/*
* End of Source.
*/

