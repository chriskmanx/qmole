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
 * Functions necessary to access Pine address book file.
 */

#include <sys/stat.h>
#include <glib.h>
#include <string.h>

#include "utils.h"
#include "mgutils.h"
#include "pine.h"
#include "addritem.h"
#include "addrcache.h"

#define PINE_HOME_FILE  ".addressbook"
#define PINEBUFSIZE     2048
#define CHAR_QUOTE      '\"'
#define CHAR_APOS       '\''
#define CHAR_COMMA      ','
#define CHAR_AT         '@'

/*
* Create new object.
*/
PineFile *pine_create() {
	PineFile *pineFile;
	pineFile = g_new0( PineFile, 1 );
	pineFile->path = NULL;
	pineFile->file = NULL;
	pineFile->retVal = MGU_SUCCESS;
	pineFile->uniqTable = g_hash_table_new( g_str_hash, g_str_equal );
	pineFile->cbProgress = NULL;
	return pineFile;
}

/*
* Properties...
*/
void pine_set_file( PineFile* pineFile, const gchar *value ) {
	cm_return_if_fail( pineFile != NULL );
	pineFile->path = mgu_replace_string( pineFile->path, value );
	g_strstrip( pineFile->path );
}

/*
 * Free key in table.
 */
static gint pine_free_table_vis( gpointer key, gpointer value, gpointer data ) {
	g_free( key );
	key = NULL;
	value = NULL;
	return TRUE;
}

/*
* Free up object by releasing internal memory.
*/
void pine_free( PineFile *pineFile ) {
	cm_return_if_fail( pineFile != NULL );

	/* Close file */
	if( pineFile->file ) fclose( pineFile->file );

	/* Free internal stuff */
	g_free( pineFile->path );

	/* Free unique address table */
	g_hash_table_foreach_remove( pineFile->uniqTable, pine_free_table_vis, NULL );
	g_hash_table_destroy( pineFile->uniqTable );

	/* Clear pointers */
	pineFile->file = NULL;
	pineFile->path = NULL;
	pineFile->retVal = MGU_SUCCESS;
	pineFile->uniqTable = NULL;
	pineFile->cbProgress = NULL;

	/* Now release file object */
	g_free( pineFile );
}

/*
 * Open file for read.
 * Enter: pineFile File object.
 * return: TRUE if file opened successfully.
 */
static gint pine_open_file( PineFile* pineFile ) {
	if( pineFile->path ) {
		pineFile->file = g_fopen( pineFile->path, "rb" );
		if( ! pineFile->file ) {
			pineFile->retVal = MGU_OPEN_FILE;
			return pineFile->retVal;
		}
	}
	else {
		/* g_print( "file not specified\n" ); */
		pineFile->retVal = MGU_NO_FILE;
		return pineFile->retVal;
	}

	/* Setup a buffer area */
	pineFile->retVal = MGU_SUCCESS;
	return pineFile->retVal;
}

/*
 * Close file.
 * Enter: pineFile File object.
 */
static void pine_close_file( PineFile *pineFile ) {
	cm_return_if_fail( pineFile != NULL );
	if( pineFile->file ) fclose( pineFile->file );
	pineFile->file = NULL;
}

/*
 * Read line of text from file.
 * Enter: pineFile File object.
 * Return: Copy of buffer. Should be g_free'd when done.
 */
static gchar *pine_read_line( PineFile *pineFile ) {
	gchar buf[ PINEBUFSIZE ];
	int c, i = 0;
	gchar ch;

	if( feof( pineFile->file ) ) 
		return NULL;

	while( i < PINEBUFSIZE-1 ) {
		c = fgetc( pineFile->file );
		if( c == EOF ) {
			if( i == 0 ) 
				return NULL;
			break;
		}
		ch = (gchar) c;
		if( ch == '\0' ) {
			if( i == 0 ) 
				return NULL;
			break;
		}
		if( ch == '\n' ) {
			break;
		}
		buf[i] = ch;
		i++;
	}
	buf[i] = '\0';

	/* Copy into private buffer */
	return g_strdup( buf );
}

/*
 * Parsed address data.
 */
typedef struct _Pine_ParsedRec_ Pine_ParsedRec;
struct _Pine_ParsedRec_ {
	gchar *nickName;
	gchar *name;
	gchar *address;
	gchar *fcc;
	gchar *comments;
	gboolean isGroup;
	GSList *listName;
	GSList *listAddr;
};

/*
 * Free data record.
 * Enter: rec Data record.
 */
static void pine_free_rec( Pine_ParsedRec *rec ) {
	if( rec ) {
		g_free( rec->nickName );
		g_free( rec->name );
		g_free( rec->address );
		g_free( rec->fcc );
		g_free( rec->comments );
		mgu_clear_slist( rec->listName );
		mgu_clear_slist( rec->listAddr );
		g_slist_free( rec->listName );
		g_slist_free( rec->listAddr );
		rec->nickName = NULL;
		rec->name = NULL;
		rec->address = NULL;
		rec->fcc = NULL;
		rec->comments = NULL;
		rec->isGroup = FALSE;
		g_free( rec );
	}
}

/*
 * Clean name.
 */
static void pine_clean_name( Pine_ParsedRec *rec ) {
	gchar *p;

	p = rec->name;
	if( p == NULL ) return;
	if( *p == '\0' ) return;

	g_strstrip( rec->name );
	if( *p == CHAR_APOS || *p == CHAR_QUOTE ) {
		return;
	}

	/* If embedded comma present, surround match with quotes */
	while( *p ) {
		if( *p == CHAR_COMMA ) {
			p = g_strdup_printf( "\"%s\"", rec->name );
			g_free( rec->name );
			rec->name = p;
			return;
		}
		p++;
	}
}

/*
 * Parse pine address record.
 * Enter:  buf Address record buffer.
 * Return: Data record.
 */
static Pine_ParsedRec *pine_parse_record( gchar *buf ) {
	Pine_ParsedRec *rec;
	gchar *p, *f;
	gint pos, len, i;
	gchar *tmp[5];

	for( i = 0; i < 5; i++ )
		tmp[i] = NULL;

	/* Extract tab separated values */
	rec = NULL;
	pos = 0;
	p = f = buf;
	while( *p ) {
		if( *p == '\t' ) {
			len = p - f;
			if( len > 0 ) {
				tmp[ pos ] = g_strndup( f, len );
				f = p;
				f++;
			}
			pos++;
		}
		p++;
	}

	/* Extract last value */
	len = p - f;
	if( len > 0 ) {
		tmp[ pos++ ] = g_strndup( f, len );
	}

	/* Populate record */
	if( pos > 0 ) {
		rec = g_new0( Pine_ParsedRec, 1 );
		rec->isGroup = FALSE;
		for( i = 0; i < pos; i++ ) {
			f = tmp[i];
			if( f ) {
				g_strstrip( f );
			}
			if( i == 0 ) rec->nickName = f;
			else if( i == 1 ) rec->name = f;
			else if( i == 2 ) rec->address = f;
			else if( i == 3 ) rec->fcc = f;
			else if( i == 4 ) rec->comments = f;
			tmp[i] = NULL;
		}

		if( rec->address != NULL ) {
			/* Strip leading/trailing parens */
			p = rec->address;
			if( *p == '(' ) {
				len = strlen( p ) - 1;
				*p = ' ';
				*(p + len) = ' ';
				rec->isGroup = TRUE;
			}
		}
	}

	return rec;
}

/*
 * Parse name from email address string.
 * Enter: buf Start address of buffer to process (not modified).
 *        atp Pointer to email at (@) character.
 *        ap  Pointer to start of email address returned.
 *        ep  Pointer to end of email address returned.
 * Return: Parsed name or NULL if not present. This should be g_free'd
 * when done.
 */
static gchar *pine_parse_name(
		const gchar *buf, const gchar *atp, const gchar **ap,
		const gchar **ep )
{
	gchar *name;
	const gchar *pos;
	const gchar *tmp;
	const gchar *bp;
	gint ilen;

	name = NULL;
	*ap = NULL;
	*ep = NULL;

	/* Find first non-separator char */
	bp = buf;
	while( TRUE ) {
		if( strchr( ",; \n\r", *bp ) == NULL ) break;
		bp++;
	}

	/* Search back for start of name */
	tmp = atp;
	pos = atp;
	while( pos >= bp ) {
		tmp = pos;
		if( *pos == '<' ) {
			/* Found start of address/end of name part */
			ilen = -1 + ( size_t ) ( pos - bp );
			name = g_strndup( bp, ilen + 1 );
			*(name + ilen + 1) = '\0';

			/* Remove leading trailing quotes and spaces */
			mgu_str_ltc2space( name, '\"', '\"' );
			mgu_str_ltc2space( name, '\'', '\'' );
			mgu_str_ltc2space( name, '\"', '\"' );
			mgu_str_unescape( name );
			g_strstrip( name );
			break;
		}
		pos--;
	}
	*ap = tmp;

	/* Search forward for end of address */
	pos = atp + 1;
	while( TRUE ) {
		if( *pos == '>' ) {
			pos++;
			break;
		}
		if( strchr( ",; \'\n\r", *pos ) ) break;
		pos++;
	}
	*ep = pos;

	return name;
}

/*
 * Parse address list.
 * Enter: pineFile Pine control data.
 *        cache    Address cache.
 *        rec      Data record.
 */
static void pine_parse_address( PineFile *pineFile, AddressCache *cache, Pine_ParsedRec *rec ) {
	const gchar *buf;
	gchar addr[ PINEBUFSIZE ];
	const gchar *bp;
	const gchar *ep;
	gchar *atCh;
	gchar *name;
	gint len;

	cm_return_if_fail( rec->address != NULL );

	buf = rec->address;
	while((atCh = strchr( buf, CHAR_AT )) != NULL) {
		name = pine_parse_name( buf, atCh, &bp, &ep );
		len = ( size_t ) ( ep - bp );
		strncpy( addr, bp, len );
		addr[ len ] = '\0';
		extract_address( addr );

		if( name == NULL ) name = g_strdup( "" );
		rec->listName = g_slist_append( rec->listName, name );
		rec->listAddr = g_slist_append( rec->listAddr, g_strdup( addr ) );

		buf = ep;
		if( atCh == ep ) {
			buf++;
		}
	}
}

/*
 * Insert person and address into address cache.
 * Enter: pineFile Pine control data.
 *        cache    Address cache.
 *        address  E-Mail address.
 *        name     Name.
 *        remarks  Remarks.
 * Return: E-Mail object, either inserted or found in hash table.
 */
static ItemEMail *pine_insert_table(
		PineFile *pineFile, AddressCache *cache, gchar *address,
		gchar *name, gchar *remarks )
{
	ItemPerson *person;
	ItemEMail *email;
	gchar *key;

	cm_return_val_if_fail( address != NULL, NULL );

	/* create an entry with empty name if needed */
	if ( name == NULL )
		name = "";

	/* Test whether address already in hash table */
	key = g_utf8_strdown( address, -1 );
	email = g_hash_table_lookup( pineFile->uniqTable, key );

	if( email == NULL ) {
		/* No - create person */
		person = addritem_create_item_person();
		addritem_person_set_common_name( person, name );
		addrcache_id_person( cache, person );
		addrcache_add_person( cache, person );

		/* Add email for person */
		email = addritem_create_item_email();
		addritem_email_set_address( email, address );
		addritem_email_set_remarks( email, remarks );
		addrcache_id_email( cache, email );
		addrcache_person_add_email( cache, person, email );

		/* Insert entry */
		g_hash_table_insert( pineFile->uniqTable, key, email );
	}
	else {
		/* Yes - update person with longest name */
		person = ( ItemPerson * ) ADDRITEM_PARENT(email);
		if( strlen( name ) > strlen( ADDRITEM_NAME(person) ) ) {
			addritem_person_set_common_name( person, name );
		}

		/* Free up */
		g_free( key );
	}

	return email;
}

/*
 * Parse address line adn build address items.
 * Enter: pineFile Pine control data.
 *        cache    Address cache to load.
 *        line     Address record.
 */
static void pine_build_items( PineFile *pineFile, AddressCache *cache, gchar *line ) {
	Pine_ParsedRec *rec;
	GSList *nodeAddr, *nodeName;
	ItemGroup *group;
	ItemEMail *email;

	rec = pine_parse_record( line );
	if( rec ) {
		pine_clean_name( rec );
		pine_parse_address( pineFile, cache, rec );
		/* pine_print_rec( rec, stdout ); */
		/* g_print( "=========\n" ); */

		if( rec->isGroup ) {
			/* Create group */
			group = addritem_create_item_group();
			addritem_group_set_name( group, rec->nickName );
			addrcache_id_group( cache, group );
			addrcache_add_group( cache, group );

			/* Add email to group */
			nodeName = rec->listName;
			nodeAddr = rec->listAddr;
			while( nodeAddr ) {
				email = pine_insert_table(
						pineFile, cache, nodeAddr->data,
						nodeName->data, "" );

				/* Add email to group */
				addritem_group_add_email( group, email );

				nodeAddr = g_slist_next( nodeAddr );
				nodeName = g_slist_next( nodeName );
			}
		}
		else {
			email = pine_insert_table(
					pineFile, cache, rec->address,
					rec->name, rec->comments );
		}

		pine_free_rec( rec );
	}
}

/*
 * Read file data into address cache.
 * Enter: pineFile Pine control data.
 *        cache    Address cache to load.
 */
static void pine_read_file( PineFile *pineFile, AddressCache *cache ) {
	GSList *listValue = NULL;
	gboolean flagEOF = FALSE, flagProc = FALSE, flagDone = FALSE;
	gchar *line =  NULL, *lineValue = NULL;
	long posEnd = 0L;
	long posCur = 0L;

	/* Find EOF for progress indicator */
	fseek( pineFile->file, 0L, SEEK_END );
	posEnd = ftell( pineFile->file );
	fseek( pineFile->file, 0L, SEEK_SET );

	flagProc = FALSE;
	while( ! flagDone ) {
		if( flagEOF ) {
			flagDone = TRUE;
			flagProc = TRUE;
		}
		else {
			line =  pine_read_line( pineFile );
		}

		posCur = ftell( pineFile->file );
		if( pineFile->cbProgress ) {
			/* Call progress indicator */
			( pineFile->cbProgress ) ( pineFile, & posEnd, & posCur );
		}

		/* Add line to list */
		if( line == NULL ) {
			flagEOF = TRUE;
		}
		else {
			/* Check for continuation line (1 space only) */
			if( *line == ' ' ) {
				g_strchug( line );
				listValue = g_slist_append(
						listValue, g_strdup( line ) );
				flagProc = FALSE;
			}
			else {
				flagProc = TRUE;
			}
		}

		if( flagProc ) {
			if( listValue != NULL ) {
				/* Process list */
				lineValue = mgu_list_coalesce( listValue );
				if( lineValue ) {
					pine_build_items(
						pineFile, cache, lineValue );
				}
				g_free( lineValue );
				lineValue = NULL;
				mgu_free_list( listValue );
				listValue = NULL;
			}
			if( line != NULL ) {
				/* Append to list */
				listValue = g_slist_append(
						listValue, g_strdup( line ) );
			}
		}

		g_free( line );
		line = NULL;
	}

	/* Release data */
	mgu_free_list( listValue );
	listValue = NULL;
}

/*
 * ============================================================================================
 * Read file into list. Main entry point
 * Enter:  pineFile Pine control data.
 *         cache    Address cache to load.
 * Return: Status code.
 * ============================================================================================
 */
gint pine_import_data( PineFile *pineFile, AddressCache *cache ) {
	cm_return_val_if_fail( pineFile != NULL, MGU_BAD_ARGS );
	cm_return_val_if_fail( cache != NULL, MGU_BAD_ARGS );

	pineFile->retVal = MGU_SUCCESS;
	addrcache_clear( cache );
	cache->dataRead = FALSE;
	pine_open_file( pineFile );
	if( pineFile->retVal == MGU_SUCCESS ) {
		/* Read data into the cache */
		pine_read_file( pineFile, cache );
		pine_close_file( pineFile );

		/* Mark cache */
		cache->modified = FALSE;
		cache->dataRead = TRUE;
	}
	return pineFile->retVal;
}

#define WORK_BUFLEN 1024

/*
 * Attempt to find a Pine addressbook file.
 * Return: Filename, or home directory if not found, or empty string if
 * no home. Filename should be g_free() when done.
 */
gchar *pine_find_file( void ) {
	const gchar *homedir;
	gchar str[ WORK_BUFLEN ];
	gint len;
	FILE *fp;

	homedir = get_home_dir();
	if( ! homedir ) return g_strdup( "" );

	strcpy( str, homedir );
	len = strlen( str );
	if( len > 0 ) {
		if( str[ len-1 ] != G_DIR_SEPARATOR ) {
			str[ len ] = G_DIR_SEPARATOR;
			str[ ++len ] = '\0';
		}
	}
	strcat( str, PINE_HOME_FILE );

	/* Attempt to open */
	if( ( fp = g_fopen( str, "rb" ) ) != NULL ) {
		fclose( fp );
	}
	else {
		/* Truncate filename */
		str[ len ] = '\0';
	}
	return g_strdup( str );
}

/*
* End of Source.
*/

