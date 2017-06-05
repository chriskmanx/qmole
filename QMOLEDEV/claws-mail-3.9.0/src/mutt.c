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
 * Functions necessary to access MUTT address book file.
 */

#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <glib.h>

#include "utils.h"
#include "mgutils.h"
#include "mutt.h"
#include "addritem.h"
#include "addrcache.h"

#define MUTT_HOME_FILE  ".muttrc"
#define MUTTBUFSIZE     2048
#define	MUTT_TAG_ALIAS  "alias"

/*
* Create new object.
*/
MuttFile *mutt_create() {
	MuttFile *muttFile;
	muttFile = g_new0( MuttFile, 1 );
	muttFile->path = NULL;
	muttFile->file = NULL;
	muttFile->retVal = MGU_SUCCESS;
	muttFile->uniqTable = g_hash_table_new( g_str_hash, g_str_equal );
	muttFile->cbProgress = NULL;
	return muttFile;
}

/*
* Properties...
*/
void mutt_set_file( MuttFile* muttFile, const gchar *value ) {
	cm_return_if_fail( muttFile != NULL );
	muttFile->path = mgu_replace_string( muttFile->path, value );
	g_strstrip( muttFile->path );
}

/*
 * Free key in table.
 */
static gint mutt_free_table_vis( gpointer key, gpointer value, gpointer data ) {
	g_free( key );
	key = NULL;
	value = NULL;
	return TRUE;
}

/*
* Free up object by releasing internal memory.
*/
void mutt_free( MuttFile *muttFile ) {
	cm_return_if_fail( muttFile != NULL );

	/* Close file */
	if( muttFile->file ) fclose( muttFile->file );

	/* Free internal stuff */
	g_free( muttFile->path );

	/* Free unique address table */
	g_hash_table_foreach_remove( muttFile->uniqTable, mutt_free_table_vis, NULL );
	g_hash_table_destroy( muttFile->uniqTable );

	/* Clear pointers */
	muttFile->file = NULL;
	muttFile->path = NULL;
	muttFile->retVal = MGU_SUCCESS;
	muttFile->uniqTable = NULL;
	muttFile->cbProgress = NULL;

	/* Now release file object */
	g_free( muttFile );
}

/*
* Open file for read.
* return: TRUE if file opened successfully.
*/
static gint mutt_open_file( MuttFile* muttFile ) {
	if( muttFile->path ) {
		muttFile->file = g_fopen( muttFile->path, "rb" );
		if( ! muttFile->file ) {
			muttFile->retVal = MGU_OPEN_FILE;
			return muttFile->retVal;
		}
	}
	else {
		/* g_print( "file not specified\n" ); */
		muttFile->retVal = MGU_NO_FILE;
		return muttFile->retVal;
	}

	/* Setup a buffer area */
	muttFile->retVal = MGU_SUCCESS;
	return muttFile->retVal;
}

/*
* Close file.
*/
static void mutt_close_file( MuttFile *muttFile ) {
	cm_return_if_fail( muttFile != NULL );
	if( muttFile->file ) fclose( muttFile->file );
	muttFile->file = NULL;
}

/*
* Read line of text from file.
* Enter: muttFile File object.
*        flagCont Continuation flag, set if back-slash character at EOL.
* Return: ptr to buffer where line starts.
*/
static gchar *mutt_get_line( MuttFile *muttFile, gboolean *flagCont ) {
	gchar buf[ MUTTBUFSIZE ];
	int ch, lch;
	int i = 0, li = 0;

	*flagCont = FALSE;
	if( feof( muttFile->file ) ) 
		return NULL;

	memset(buf, 0, MUTTBUFSIZE);

	lch = '\0';
	while( i < MUTTBUFSIZE-1 ) {
		ch = fgetc( muttFile->file );
		if( ch == '\0' || ch == EOF ) {
			if( i == 0 ) 
				return NULL;
			break;
		}
		if( ch == '\n' ) {
			if( lch == '\\' ) {
				/* Replace backslash with NULL */
				if( li != 0 ) 
					buf[li] = '\0';
				*flagCont = TRUE;
			}
			break;
		}
		buf[i] = ch;
		li = i;
		lch = ch;
		i++;
	}
	buf[i]='\0';

	/* Copy into private buffer */
	return g_strdup( buf );
}

/*
 * Parsed address data.
 */
typedef struct _Mutt_ParsedRec_ Mutt_ParsedRec;
struct _Mutt_ParsedRec_ {
	gchar *address;
	gchar *name;
};

/*
 * Free data record.
 * Enter: rec Data record.
 */
static void mutt_free_rec( Mutt_ParsedRec *rec ) {
	if( rec ) {
		g_free( rec->address );
		g_free( rec->name );
		rec->address = NULL;
		rec->name = NULL;
		g_free( rec );
	}
}

/*
* Parse recipient list for each address.
* Enter: rcpList   Recipients extracted from file.
*        addrCount Updated with recipient count.
* Return: Linked list of recipients.
*/
static GSList *mutt_parse_rcplist( gchar *rcpList, gint *addrCount ) {
	gchar *ptr, *pStart, *pEnd, *pAddr, *pName, *address, *name;
	gchar ch;
	GSList *list;
	Mutt_ParsedRec *rec;
	gint  cnt;

	list = NULL;
	cnt = 0;
	pStart = rcpList;
	while( pStart && *pStart ) {
		ptr = pStart;
		address = NULL;
		pName = pAddr = NULL;
		/* Chew up spaces */
		while( *ptr ) {
			if( ! isspace( *ptr ) ) break;
			ptr++;
		}

		/* Find address */
		while( *ptr ) {
			ch = *ptr;
			if( ch == '(' ) {
				pAddr = pName = ptr;
				break;
			}
			if( ch == ',' ) {
				pAddr = ptr;
				ptr++;
				break;
			}
			if( isspace( ch ) ) {
				pAddr = ptr;
				break;
			}
			ptr++;
		}

		/* Extract address */
		if( pAddr ) {
			address = g_strndup( pStart, pAddr - pStart );
		}
		else {
			address = g_strdup( pStart );
		}
		g_strstrip( address );

		/* Chew up spaces */
		while( *ptr ) {
			ch = *ptr;
			if( ch == '(' ) {
				pName = ptr;
				break;
			}
			if( ch == ',' ) {
				ptr++;
				break;
			}
			ptr++;
			if( isspace( ch ) ) continue;
		}
		pStart = ptr;
	
		/* Extract name (if any) */
		if( pName ) {
			/* Look for closing parens */
			pName++;
			pEnd = NULL;
			ptr = pName;
			while( *ptr ) {
				if( *ptr == ')' ) {
					pEnd = ptr;
					break;
				}
				ptr++;
			}
			if( pEnd ) {
				name = g_strndup( pName, pEnd - pName );
				pEnd++;
				if( *pEnd ) pEnd++;
			}
			else {
				name = g_strdup( pName );
			}
			g_strstrip( name );
			pStart = pEnd;
		}
		else {
			name = g_strdup( "" );
		}

		/* New record */
		rec = g_new0( Mutt_ParsedRec, 1 );
		rec->address = address;
		rec->name = name;
		list = g_slist_append( list, rec );
		cnt++;

		/* mutt_print_rec( rec, stdout ); */
	}
	*addrCount = cnt;
	return list;
}

/*
 * Insert person and address into address cache.
 * Enter: muttFile MUTT control data.
 *        cache    Address cache.
 *        address  E-Mail address.
 *        name     Name.
 * Return: E-Mail object, either inserted or found in hash table.
 */
static ItemEMail *mutt_insert_table(
		MuttFile *muttFile, AddressCache *cache, gchar *address,
		gchar *name )
{
	ItemPerson *person;
	ItemEMail *email;
	gchar *key;

	/* Test whether address already in hash table */
	key = g_utf8_strdown( address, -1 );
	email = g_hash_table_lookup( muttFile->uniqTable, key );

	if( email == NULL ) {
		/* No - create person */
		person = addritem_create_item_person();
		addritem_person_set_common_name( person, name );
		addrcache_id_person( cache, person );
		addrcache_add_person( cache, person );

		/* Add email for person */
		email = addritem_create_item_email();
		addritem_email_set_address( email, address );
		addrcache_id_email( cache, email );
		addrcache_person_add_email( cache, person, email );

		/* Insert entry */
		g_hash_table_insert( muttFile->uniqTable, key, email );
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
 * Build address book entries.
 * Enter: muttFile  MUTT control data.
 *        cache     Address cache.
 *        aliasName Alias,
 *        listAddr  List of address items.
 *        addrCount Address list count.
 */
static void mutt_build_address(
		MuttFile *muttFile, AddressCache *cache,
		gchar *aliasName, GSList *listAddr, gint addrCount )
{
	GSList *node = NULL;
	ItemEMail *email;
	ItemGroup *group;
	Mutt_ParsedRec *rec;

	group = NULL;
	if( listAddr != NULL && addrCount > 1 ) {
		group = addritem_create_item_group();
		addritem_group_set_name( group, aliasName );
		addrcache_id_group( cache, group );
		addrcache_add_group( cache, group );
	}

	email = NULL;
	node = listAddr;
	while( node ) {
		rec = node->data;

		/* Insert person/email */
		email = mutt_insert_table(
				muttFile, cache, rec->address, rec->name );

		/* Add email to group */
		if( group ) {
			addritem_group_add_email( group, email );
		}

		mutt_free_rec( rec );
		node = g_slist_next( node );
	}
}

/*
 * Parse address line adn build address items.
 * Enter: muttFile MUTT control data.
 *        cache    Address cache.
 *        line     Data record.
 */
static void mutt_build_items( MuttFile *muttFile, AddressCache *cache, gchar *line ) {
	GList *list, *node;
	gint tCount, aCount;
	gchar *aliasTag, *aliasName, *recipient;
	GSList *addrList;

	/* g_print( "\nBUILD >%s<\n", line ); */
	list = mgu_parse_string( line,  3, &tCount );
	if( tCount < 3 ) {
		if( list ) {
			mgu_free_dlist( list );
			list = NULL;
		}
		return;
	}

	aliasTag = list->data;
	node = g_list_next( list );
	aliasName = node->data;
	node = g_list_next( node );
	recipient = node->data;

	addrList = NULL;
	if( strcmp( aliasTag, MUTT_TAG_ALIAS ) == 0 ) {
		aCount = 0;
		/* g_print( "aliasName :%s:\n", aliasName ); */
		/* g_print( "recipient :%s:\n", recipient ); */
		addrList = mutt_parse_rcplist( recipient, &aCount );
		/* g_print( "---\n" ); */
		mutt_build_address( muttFile, cache, aliasName, addrList, aCount );
	}

	mgu_free_dlist( list );
	list = NULL;

}

/*
 * Read file data into address cache.
 * Enter: muttFile MUTT control data.
 *        cache Address cache.
 */
static void mutt_read_file( MuttFile *muttFile, AddressCache *cache ) {
	GSList *listValue = NULL;
	gboolean flagEOF = FALSE, flagCont = FALSE, lastCont = FALSE;
	gchar *line =  NULL, *lineValue = NULL;
	long posEnd = 0L;
	long posCur = 0L;

	/* Find EOF for progress indicator */
	fseek( muttFile->file, 0L, SEEK_END );
	posEnd = ftell( muttFile->file );
	fseek( muttFile->file, 0L, SEEK_SET );

	while( ! flagEOF ) {
		flagCont = FALSE;
		line =  mutt_get_line( muttFile, &flagCont );

		posCur = ftell( muttFile->file );
		if( muttFile->cbProgress ) {
			/* Call progress indicator */
			( muttFile->cbProgress ) ( muttFile, & posEnd, & posCur );
		}

		if( line == NULL ) flagEOF = TRUE;
		if( ! lastCont ) {
			/* Save data */
			lineValue = mgu_list_coalesce( listValue );
			if( lineValue ) {
				mutt_build_items( muttFile, cache, lineValue );
			}
			g_free( lineValue );
			lineValue = NULL;
			mgu_free_list( listValue );
			listValue = NULL;
		}
		lastCont = flagCont;

		/* Add line to list */
		listValue = g_slist_append( listValue, g_strdup( line ) );

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
* Enter:  muttFile MUTT control data.
*         cache    Address cache to load.
* Return: Status code.
* ============================================================================================
*/
gint mutt_import_data( MuttFile *muttFile, AddressCache *cache ) {
	cm_return_val_if_fail( muttFile != NULL, MGU_BAD_ARGS );
	cm_return_val_if_fail( cache != NULL, MGU_BAD_ARGS );
	muttFile->retVal = MGU_SUCCESS;
	addrcache_clear( cache );
	cache->dataRead = FALSE;
	mutt_open_file( muttFile );
	if( muttFile->retVal == MGU_SUCCESS ) {
		/* Read data into the cache */
		mutt_read_file( muttFile, cache );
		mutt_close_file( muttFile );

		/* Mark cache */
		cache->modified = FALSE;
		cache->dataRead = TRUE;
	}
	return muttFile->retVal;
}

#define WORK_BUFLEN 1024

/*
* Attempt to find a Mutt file.
* Return: Filename, or home directory if not found, or empty string if
* no home. Filename should be g_free() when done.
*/
gchar *mutt_find_file( void ) {
	const gchar *homedir;
	gchar str[ WORK_BUFLEN + 1 ];
	gint len;
	FILE *fp;

	homedir = get_home_dir();
	if( ! homedir ) return g_strdup( "" );

	strncpy( str, homedir, WORK_BUFLEN );
	len = strlen( str );
	if( len > 0 ) {
		if( str[ len-1 ] != G_DIR_SEPARATOR ) {
			str[ len ] = G_DIR_SEPARATOR;
			str[ ++len ] = '\0';
		}
	}
	strncat( str, MUTT_HOME_FILE, WORK_BUFLEN - strlen(str) );

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

