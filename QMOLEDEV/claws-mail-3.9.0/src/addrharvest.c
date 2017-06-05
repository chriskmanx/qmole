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
 * Functions for an E-Mail address harvester.
 */

#include <sys/stat.h>
#include <dirent.h>
#include <glib.h>
#include <string.h>

#include "utils.h"
#include "mgutils.h"
#include "addrharvest.h"
#include "codeconv.h"
#include "addritem.h"
#ifdef USE_NEW_ADDRBOOK
	#include "addressbook-dbus.h"
#endif

/* Mail header names of interest */
static gchar *_headerFrom_     = HEADER_FROM;
static gchar *_headerReplyTo_  = HEADER_REPLY_TO;
static gchar *_headerSender_   = HEADER_SENDER;
static gchar *_headerErrorsTo_ = HEADER_ERRORS_TO;
static gchar *_headerCC_       = HEADER_CC;
static gchar *_headerTo_       = HEADER_TO;

#define ADDR_BUFFSIZE    1024
#define MSG_BUFFSIZE     2048
#define MSGNUM_BUFFSIZE  32
#define DFL_FOLDER_SIZE  20

/* Noise strings included by some other E-Mail clients */
#define REM_NAME_STRING  "(Email)"
#define REM_NAME_STRING2 "(Email 2)"

/* Directories to ignore */
#define DIR_IGNORE ".\t..\t.sylpheed_mark\t.sylpheed_claws_cache"

/*
 * Header entry.
 */
typedef struct _HeaderEntry HeaderEntry;
struct _HeaderEntry {
	gchar      *header;
	gboolean   selected;
	ItemFolder *folder;
	gint       count;
};

#ifdef USE_NEW_ADDRBOOK
typedef enum {
    FIRST = 0,
    LAST,
} Namepart;

#endif

/*
 * Build header table entry.
 * Enter: harvester Harvester object.
 *        name      Header name.
 */
static void addrharvest_build_entry(
		AddressHarvester* harvester, gchar *name )
{
	HeaderEntry *entry;

	entry = g_new0( HeaderEntry, 1 );
	entry->header = name;
	entry->selected = FALSE;
	entry->folder = NULL;
	entry->count = 0;
	harvester->headerTable = g_list_append( harvester->headerTable, entry );
}

/*
 * Free key in table.
 */
static gint addrharvest_free_table_vis( gpointer key, gpointer value, gpointer data ) {
	g_free( key );
	key = NULL;
	value = NULL;
	return TRUE;
}

/*
 * Free lookup table.
 */
static void addrharvest_free_table( AddressHarvester* harvester ) {
	GList *node;
	HeaderEntry *entry;

	/* Free header list */
	node = harvester->headerTable;
	while( node ) {
		entry = ( HeaderEntry * ) node->data;
		entry->header = NULL;
		entry->selected = FALSE;
		entry->folder = NULL;
		entry->count = 0;
		g_free( entry );
		node = g_list_next( node );
	}
	g_list_free( harvester->headerTable );
	harvester->headerTable = NULL;

	/* Free duplicate table */
	g_hash_table_foreach_remove( harvester->dupTable, addrharvest_free_table_vis, NULL );
	g_hash_table_destroy( harvester->dupTable );
	harvester->dupTable = NULL;
}

/*
* Create new object.
* Return: Harvester.
*/
AddressHarvester *addrharvest_create( void ) {
	AddressHarvester *harvester;

	harvester = g_new0( AddressHarvester, 1 );
	harvester->path = NULL;
	harvester->dupTable = g_hash_table_new( g_str_hash, g_str_equal );
	harvester->folderSize = DFL_FOLDER_SIZE;
	harvester->retVal = MGU_SUCCESS;

	/* Build header table */
	harvester->headerTable = NULL;
	addrharvest_build_entry( harvester, _headerFrom_ );
	addrharvest_build_entry( harvester, _headerReplyTo_ );
	addrharvest_build_entry( harvester, _headerSender_ );
	addrharvest_build_entry( harvester, _headerErrorsTo_ );
	addrharvest_build_entry( harvester, _headerCC_ );
	addrharvest_build_entry( harvester, _headerTo_ );

	return harvester;
}

/*
* Properties...
*/
/*
 * Specify path to folder that will be harvested.
 * Entry: harvester Harvester object.
 *        value     Full directory path.
 */
void addrharvest_set_path( AddressHarvester* harvester, const gchar *value ) {
	cm_return_if_fail( harvester != NULL );
	harvester->path = mgu_replace_string( harvester->path, value );
	g_strstrip( harvester->path );
}

/*
 * Specify maximum folder size.
 * Entry: harvester Harvester object.
 *        value     Folder size.
 */
void addrharvest_set_folder_size(
	AddressHarvester* harvester, const gint value )
{
	cm_return_if_fail( harvester != NULL );
	if( value > 0 ) {
		harvester->folderSize = value;
	}
}

/*
 * Specify folder recursion.
 * Entry: harvester Harvester object.
 *        value     TRUE to process sub-folders, FALSE to process folder only.
 */
void addrharvest_set_recurse(
	AddressHarvester* harvester, const gboolean value )
{
	cm_return_if_fail( harvester != NULL );
	harvester->folderRecurse = value;
}

/*
 * Search (case insensitive) for header entry with specified name.
 * Enter: harvester Harvester.
 *        name      Header name.
 * Return: Header, or NULL if not found.
 */
static HeaderEntry *addrharvest_find( 
	AddressHarvester* harvester, const gchar *name ) {
	HeaderEntry *retVal;
	GList *node;

	retVal = NULL;
	node = harvester->headerTable;
	while( node ) {
		HeaderEntry *entry;

		entry = node->data;
		if (g_ascii_strncasecmp(entry->header, name,
					strlen(entry->header)) == 0 ) {
			retVal = entry;
			break;
		}
		node = g_list_next( node );
	}
	return retVal;
}

/*
 * Set selection for specified heaader.
 * Enter: harvester Harvester.
 *        name      Header name.
 *        value     Value to set.
 */
void addrharvest_set_header(
	AddressHarvester* harvester, const gchar *name, const gboolean value )
{
	HeaderEntry *entry;

	cm_return_if_fail( harvester != NULL );
	entry = addrharvest_find( harvester, name );
	if( entry != NULL ) {
		entry->selected = value;
	}
}

/*
 * Get address count
 * Enter: harvester Harvester.
 *        name      Header name.
 * Return: Address count, or -1 if header not found.
 */
gint addrharvest_get_count( AddressHarvester* harvester, const gchar *name ) {
	HeaderEntry *entry;
	gint count;

	count = -1;
	cm_return_val_if_fail( harvester != NULL, count );
	entry = addrharvest_find( harvester, name );
	if( entry != NULL ) {
		count = entry->count;
	}
	return count;
}

/*
* Free up object by releasing internal memory.
* Enter: harvester Harvester.
*/
void addrharvest_free( AddressHarvester *harvester ) {
	cm_return_if_fail( harvester != NULL );

	/* Free internal stuff */
	addrharvest_free_table( harvester );
	g_free( harvester->path );

	/* Clear pointers */
	harvester->path = NULL;
	harvester->retVal = MGU_SUCCESS;
	harvester->headerTable = NULL;

	harvester->folderSize = 0;

	/* Now release object */
	g_free( harvester );
}

#ifdef USE_NEW_ADDRBOOK
static gchar* get_namepart(const gchar* name, Namepart namepart) {
    gchar *pos, *part = NULL;
    gchar *token = g_strdup(name);

    pos = g_strrstr(token, " ");
    if (namepart == FIRST) {
        if (pos) {
            *pos = '\0';
            part = g_strdup(token);
            *pos = ' ';
        }
    }
    else {
        if (! pos)
            part = g_strdup(token);
        else {
            pos +=1;
            part = g_strdup(pos);
        }
    }        
    g_free(token);
    return part;
}
#endif

/*
 * Insert address into cache.
 * Enter: harvester Harvester object.
 *        entry     Header object.
 *        cache     Address cache to load.
 *        name      Name.
 *        address   eMail address.
 */
static void addrharvest_insert_cache(
		AddressHarvester *harvester, HeaderEntry *entry,
		AddressCache *cache, const gchar *name,
		const gchar *address )
{
#ifndef USE_NEW_ADDRBOOK
	ItemPerson *person;
	ItemFolder *folder;
	gchar *folderName;
	gboolean newFolder;
	gint cnt;
	gchar *key, *value;

	newFolder = FALSE;
	folder = entry->folder;
	if( folder == NULL ) {
		newFolder = TRUE;	/* No folder yet */
	}
	if( entry->count % harvester->folderSize == 0 ) {
		newFolder = TRUE;	/* Folder is full */
	}
#else
    ContactEntry* person;
    gchar* key;
#endif

	/* Insert address */
	key = g_utf8_strdown( address, -1 );
	person = g_hash_table_lookup( harvester->dupTable, key );
#ifndef USE_NEW_ADDRBOOK
	if( person ) {
		/* Update existing person to use longest name */
		value = ADDRITEM_NAME(person);
		if( strlen( name ) > strlen( value ) ) {
			addritem_person_set_common_name( person, name );
		}
		g_free( key );
	}
	else {
		/* Folder if required */
		if( newFolder ) {
			cnt = 1 + ( entry->count / harvester->folderSize );
			folderName =g_strdup_printf( "%s (%d)",
					entry->header, cnt );
			folder = addritem_create_item_folder();
			addritem_folder_set_name( folder, folderName );
			addritem_folder_set_remarks( folder, "" );
			addrcache_id_folder( cache, folder );
			addrcache_add_folder( cache, folder );
			entry->folder = folder;
			g_free( folderName );
		}

		/* Insert entry */
		person = addrcache_add_contact(
				cache, folder, name, address, "" );
		g_hash_table_insert( harvester->dupTable, key, person );
		entry->count++;
	}
	addritem_parse_first_last( person );
#else
	if (! person) {
		person = g_new0(ContactEntry, 1);
		person->first_name = get_namepart(name, FIRST);
		person->last_name = get_namepart(name, LAST);
		person->email = g_strdup(address);
		g_hash_table_insert(harvester->dupTable, key, person);
		entry->count++;
	}
#endif
}

/*
 * Remove specified string from name.
 * Enter: name Name.
 *        str  String to remove.
 */
static void addrharvest_del_email( gchar *name, gchar *str ) {
	gchar *p;
	gint lenn, lenr;

	lenr = strlen( str );
	while((p = strcasestr( name, str )) != NULL) {
		lenn = strlen( p );
		memmove( p, p + lenr, lenn );
	}
}

/*
 * Find position of at (@) character in buffer.
 * Enter:  buffer Start of buffer.
 * Return: Position of at character, or NULL if not found.
 * Note: This function searches for the last occurrence of an 'at' character
 * prior to a valid delimiter character for the end of address. This enables
 * an address to be found where it is also used as the name of the
 * recipient. For example:
 *     "axle.rose@netscape.com" <axle.rose@netscape.com>
 * The last occurrence of the at character is detected.
 */
static gchar *addrharvest_find_at( const gchar *buffer ) {
	gchar *atCh;
	gchar *p;

	atCh = strchr( buffer, '@' );
	if( atCh ) {
		/* Search forward for another one */
		p = atCh + 1;
		while( *p ) {
			if( *p == '>' ) {
				break;
			}
			if( *p == ',' ) {
				break;
			}
			if( *p == '\n' ) {
				break;
			}
			if( *p == '@' ) {
				atCh = p;
				break;
			}
			p++;
		}
	}
	return atCh;
}

/*
 * Find start and end of address string.
 * Enter: buf Start address of buffer to process (not modified).
 *        atp Pointer to email at (@) character.
 *        bp  Pointer to start of email address (returned).
 *        ep  Pointer to end of email address (returned).
 */
static void addrharvest_find_address(
		const gchar *buf, const gchar *atp, const gchar **bp,
		const gchar **ep )
{
	const gchar *p;

	/* Find first non-separator char */
	*bp = NULL;
	p = buf;
	while( TRUE ) {
		if( strchr( ",; \n\r", *p ) == NULL ) break;
		p++;
	}
	*bp = p;

	/* Search forward for end of address */
	*ep = NULL;
	p = atp + 1;
	while( TRUE ) {
		if( strchr( ",;", *p ) ) break;
		p++;
	}
	*ep = p;
}

/*
 * Extract E-Mail address from buffer. If found, address is removed from
 * buffer.
 * Enter:  buffer Address buffer.
 * Return: E-Mail address, or NULL if none found. Must g_free() when done.
 */
static gchar *addrharvest_extract_address( gchar *buffer ) {
	gchar *addr;
	gchar *atCh, *p, *bp, *ep;
	gint len;

	addr = NULL;
	atCh = addrharvest_find_at( buffer );
	if( atCh ) {
		/* Search back for start of address */
		bp = NULL;
		p = atCh;
		while( p >= buffer ) {
			bp = p;
			if( *p == '<' ) {
				*p = ' ';
				bp++;
				break;
			}
			p--;
		}

		/* Search fwd for end */
		ep = NULL;
		ep = p = atCh;
		while( *p ) {
			if( *p == '>' ) {
				*p = ' ';
				break;
			}
			else if( *p == ' ' ) {
				break;
			}
			ep = p;
			p++;
		}

		/* Extract email */
		if( bp != NULL ) {
			len = ( ep - bp );
			if( len > 0 ) {
				addr = g_strndup( bp, len + 1 );
				memmove( bp, ep, len );
				*bp = ' ';
			}
		}	
	}
	return addr;
}

/*
 * Parse address from header buffer creating address in cache.
 * Enter: harvester Harvester object.
 *        entry     Header object.
 *        cache     Address cache to load.
 *        hdrBuf    Pointer to header buffer.
 */
static void addrharvest_parse_address(
		AddressHarvester *harvester, HeaderEntry *entry,
		AddressCache *cache, const gchar *hdrBuf )
{
	gchar buffer[ ADDR_BUFFSIZE + 2 ];
	const gchar *bp;
	const gchar *ep;
	gchar *atCh, *email, *name;
	gint bufLen;

	/* Search for an address */
	while((atCh = addrharvest_find_at( hdrBuf )) != NULL) {
		/* Find addres string */
		addrharvest_find_address( hdrBuf, atCh, &bp, &ep );

		/* Copy into buffer */
		bufLen = ( size_t ) ( ep - bp );
		if( bufLen > ADDR_BUFFSIZE -1 ) {
			bufLen = ADDR_BUFFSIZE - 1;
		}
		strncpy( buffer, bp, bufLen );
		buffer[ bufLen ] = '\0';
		buffer[ bufLen + 1 ] = '\0';
		buffer[ bufLen + 2 ] = '\0';

		/* Extract address from buffer */
		email = addrharvest_extract_address( buffer );
		if( email ) {
			/* Unescape characters */
			mgu_str_unescape( buffer );

			/* Remove noise characaters */
			addrharvest_del_email( buffer, REM_NAME_STRING );
			addrharvest_del_email( buffer, REM_NAME_STRING2 );

			/* Remove leading trailing quotes and spaces */
			mgu_str_ltc2space( buffer, '\"', '\"' );
			mgu_str_ltc2space( buffer, '\'', '\'' );
			mgu_str_ltc2space( buffer, '\"', '\"' );
			mgu_str_ltc2space( buffer, '(', ')' );
			g_strstrip( buffer );

			if( g_ascii_strcasecmp( buffer, email ) == 0 )
				name = g_strdup("");
			else
				name = conv_unmime_header(buffer, NULL, TRUE);

			/* Insert into address book */
#ifndef USE_NEW_ADDRBOOK
			addrharvest_insert_cache(
				harvester, entry, cache, name, email );
#else
			addrharvest_insert_cache(
				harvester, entry, NULL, name, email);
#endif
			g_free( email );
			g_free( name );
		}
		hdrBuf = ep;
	}
}

/*
 * Test whether buffer contains a header that appears in header list.
 * Enter: listHdr Header list.
 *        buf     Header buffer.
 * Return: TRUE if header in list.
 */
static gboolean addrharvest_check_hdr( GList *listHdr, gchar *buf ) {
	gboolean retVal;
	GList *node;
	gchar *p, *hdr, *nhdr;
	gint len;

	retVal = FALSE;
	p = strchr( buf, ':' );
	if( p ) {
		len = ( size_t ) ( p - buf );
		hdr = g_strndup( buf, len );
		node = listHdr;
		while( node ) {
			nhdr = node->data;
			if (g_ascii_strncasecmp(nhdr, hdr, strlen(nhdr)) == 0 ) {
				retVal = TRUE;
				break;
			}
			node = g_list_next( node );
		}
		g_free( hdr );
	}
	return retVal;
}

/*
 * Read header into a linked list of lines.
 * Enter:  fp      File to read.
 *         listHdr List of header lines of interest.
 *         done    End of headers or end of file reached.
 * Return: Linked list of lines.
 */
static GSList *addrharvest_get_header( FILE *fp, GList *listHdr, gboolean *done ) {
	GSList *list;
	gchar buf[ MSG_BUFFSIZE + 2 ];
	gint ch;
	gboolean foundHdr;

	list = NULL;

	/* Read line */
	if( fgets( buf, MSG_BUFFSIZE, fp ) == NULL ) {
		*done = TRUE;
		return list;
	}

	/* Test for end of headers */
	if( buf[0] == '\r' || buf[0] == '\n' ) {
		*done = TRUE;
		return list;
	}

	/* Test whether required header */
	foundHdr = addrharvest_check_hdr( listHdr, buf );

	/* Read all header lines. Only add reqd ones to list */
	while( TRUE ) {
		gchar *p;

		if( foundHdr ) {
			p = g_strdup( buf );
			list = g_slist_append( list, p );
		}

		/* Read first character */
		ch = fgetc( fp );
		if( ch == ' ' || ch == '\t' ) {
			/* Continuation character - read into buffer */
			if( fgets( buf, MSG_BUFFSIZE, fp ) == NULL ) {
				break;
			}
		}
		else {
			if( ch == EOF ) {
				*done = TRUE;
			}
			else {
				/* Push back character for next header */
				ungetc( ch, fp );
			}
			break;
		}
	}

	return list;
}

/*
 * Read specified file into address book.
 * Enter:  harvester Harvester object.
 *         fileName  File to read.
 *         cache     Address cache to load.
 * Return: Status.
 */
static gint addrharvest_readfile(
		AddressHarvester *harvester, const gchar *fileName,
		AddressCache *cache, GList *listHdr )
{
	gint retVal;
	FILE *msgFile;
	gchar *buf, *addr, *p;
	HeaderEntry *entry;
	GSList *list;
	gboolean done;

	msgFile = g_fopen( fileName, "rb" );
	if( ! msgFile ) {
		/* Cannot open file */
		retVal = MGU_OPEN_FILE;
		return retVal;
	}

	done = FALSE;
	while( TRUE ) {
		list = addrharvest_get_header( msgFile, listHdr, &done );
		if( done ) break;

		if( list == NULL ) {
			continue;
		}

		buf = mgu_list_coalesce( list );
		mgu_free_list( list );

		if(( p = strchr( buf, ':' ) ) != NULL ) {
			addr = p + 1;
			*p = '\0';

			entry = addrharvest_find( harvester, buf );
			if( entry && entry->selected ) {
				/* Sanitize control characters */
				p = addr;
				while( *p ) {
					if( *p == '\r' || *p == '\n' || *p == '\t' )
						*p = ' ';
					p++;
				}
				addrharvest_parse_address(
					harvester, entry, cache, addr );
			}
		}
		g_free( buf );
	}

	fclose( msgFile );
	return MGU_SUCCESS;
}

/*
 * Read all files in specified directory into address book. Directories are
 * traversed recursively if necessary.
 * Enter:  harvester Harvester object.
 *         cache     Address cache to load.
 *         msgList   List of message numbers, or NULL to process folder.
 *         dir       Directory to process.
 */
static void addrharvest_harvest_dir(
	AddressHarvester *harvester, AddressCache *cache, GList *listHdr,
	gchar *dir )
{
	DIR *dp;
	struct dirent *d;
	struct stat s;
	gint num;
	int r;

	if( ( dp = opendir( dir ) ) == NULL ) {
		return;
	}

	/* Process directory */
	r = chdir( dir );
	while( r == 0 && ( d = readdir( dp ) ) != NULL ) {
		g_stat( d->d_name, &s );
		if( S_ISDIR( s.st_mode ) ) {
			if( harvester->folderRecurse ) {
				if( strstr( DIR_IGNORE, d->d_name ) != NULL )
					continue;
				addrharvest_harvest_dir(
					harvester, cache, listHdr, d->d_name );
			}
		}
		if( S_ISREG( s.st_mode ) ) {
			if( ( num = to_number( d->d_name ) ) >= 0 ) {
				addrharvest_readfile(
					harvester, d->d_name, cache, listHdr );
			}
		}
	}
	r = chdir( ".." );
	closedir( dp );
}

/*
 * Read list of files in specified directory into address book.
 * Enter:  harvester Harvester object.
 *         cache     Address cache to load.
 *         msgList   List of message numbers, or NULL to process folder.
 */
static void addrharvest_harvest_list(
	AddressHarvester *harvester, AddressCache *cache, GList *listHdr,
	GList *msgList )
{
	DIR *dp;
	gint num;
	GList *node;
	gchar msgNum[ MSGNUM_BUFFSIZE ];
	int r;

	if( ( dp = opendir( harvester->path ) ) == NULL ) {
		g_message("cannot opendir %s\n", harvester->path);
		return;
	}

	/* Process message list */
	r = chdir( harvester->path );
	if (r != 0) {
		g_message("cannot chdir %s\n", harvester->path);
		return;
	}
	node = msgList;
	while( node ) {
		num = GPOINTER_TO_UINT( node->data );
		sprintf( msgNum, "%d", num );
		addrharvest_readfile( harvester, msgNum, cache, listHdr );
		node = g_list_next( node );
	}
	closedir( dp );
}

/*
 * ============================================================================
 * Read all files in specified directory into address book.
 * Enter:  harvester Harvester object.
 *         cache     Address cache to load.
 *         msgList   List of message numbers, or NULL to process folder.
 * Return: Status.
 * ============================================================================
 */
gint addrharvest_harvest(
	AddressHarvester *harvester, AddressCache *cache, GList *msgList )
{
	gint retVal;
	GList *node;
	GList *listHdr;

	retVal = MGU_BAD_ARGS;
	cm_return_val_if_fail( harvester != NULL, retVal );
#ifndef USE_NEW_ADDRBOOK
	cm_return_val_if_fail( cache != NULL, retVal );
#endif
	cm_return_val_if_fail( harvester->path != NULL, retVal );

#ifndef USE_NEW_ADDRBOOK
	/* Clear cache */
	addrcache_clear( cache );
	cache->dataRead = FALSE;
#endif
	/* Build list of headers of interest */
	listHdr = NULL;
	node = harvester->headerTable;
	while( node ) {
		HeaderEntry *entry;

		entry = node->data;
		if( entry->selected ) {
			gchar *p;

			p = g_utf8_strdown( entry->header, -1 );
			listHdr = g_list_append( listHdr, p );
		}
		node = g_list_next( node );
	}

	/* Process directory/files */
	if( msgList == NULL ) {
		addrharvest_harvest_dir( harvester, cache, listHdr, harvester->path );
	}
	else {
		addrharvest_harvest_list( harvester, cache, listHdr, msgList );
	}
	mgu_free_dlist( listHdr );

#ifndef USE_NEW_ADDRBOOK
	/* Mark cache */
	cache->modified = FALSE;
	cache->dataRead = TRUE;
#endif
	return retVal;
}

/*
 * ============================================================================
 * Test whether any headers have been selected for processing.
 * Enter:  harvester Harvester object.
 * Return: TRUE if a header was selected, FALSE if none were selected.
 * ============================================================================
 */
gboolean addrharvest_check_header( AddressHarvester *harvester ) {
	gboolean retVal;
	GList *node;

	retVal = FALSE;
	cm_return_val_if_fail( harvester != NULL, retVal );

	node = harvester->headerTable;
	while( node ) {
		HeaderEntry *entry;

		entry = ( HeaderEntry * ) node->data;
		if( entry->selected ) return TRUE;
		node = g_list_next( node );
	}
	return retVal;
}

/*
 * ============================================================================
 * End of Source.
 * ============================================================================
 */


