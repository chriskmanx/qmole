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
 * Functions necessary to access LDIF files (LDAP Data Interchange Format
 * files).
 */

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>
#include <sys/stat.h>

#include "mgutils.h"
#include "ldif.h"
#include "addritem.h"
#include "addrcache.h"

#include "base64.h"
#include "utils.h"

#define	LDIF_SEP_TAG    ':'
#define	LDIF_LANG_TAG   ';'

/**
 * Create new object.
 * \return Initialized LDIF file object.
 */
LdifFile *ldif_create() {
	LdifFile *ldifFile;
	ldifFile = g_new0( LdifFile, 1 );
	ldifFile->path = NULL;
	ldifFile->file = NULL;
	ldifFile->hashFields = g_hash_table_new( g_str_hash, g_str_equal );
	ldifFile->tempList = NULL;
	ldifFile->dirtyFlag = TRUE;
	ldifFile->accessFlag = FALSE;
	ldifFile->retVal = MGU_SUCCESS;
	ldifFile->cbProgress = NULL;
	ldifFile->importCount = 0;
	return ldifFile;
}

/**
 * Specify full file specification of LDIF file.
 * \param ldifFile LDIF import control object.
 * \param value    Value of access flag.
 */
void ldif_set_file( LdifFile *ldifFile, const gchar *value ) {
	cm_return_if_fail( ldifFile != NULL );

	if( ldifFile->path ) {
		if( strcmp( ldifFile->path, value ) != 0 )
			ldifFile->dirtyFlag = TRUE;
	}
	else {
		ldifFile->dirtyFlag = TRUE;
	}
	ldifFile->path = mgu_replace_string( ldifFile->path, value );
	g_strstrip( ldifFile->path );
	ldifFile->importCount = 0;
}

/**
 * Set the file access indicator.
 * \param ldifFile LDIF import control object.
 * \param value    File specification.
 */
void ldif_set_accessed( LdifFile *ldifFile, const gboolean value ) {
	cm_return_if_fail( ldifFile != NULL );
	ldifFile->accessFlag = value;
}

/**
 * Create field record object.
 * \return Initialized LDIF field object.
 */
static Ldif_FieldRec *ldif_create_fieldrec( const gchar *field ) {
	Ldif_FieldRec *rec = g_new0( Ldif_FieldRec, 1 );
	rec->tagName = g_strdup( field );
	rec->userName = NULL;
	rec->reserved = FALSE;
	rec->selected = FALSE;
	return rec;
}

/**
 * Free field record object.
 * \param rec LDIF field object.
 */
static void ldif_free_fieldrec( Ldif_FieldRec *rec ) {
	if( rec ) {
		g_free( rec->tagName );
		g_free( rec->userName );
		rec->tagName = NULL;
		rec->userName = NULL;
		rec->reserved = FALSE;
		rec->selected = FALSE;
		g_free( rec );
	}
}

/**
 * Set user name for field record.
 * \param rec   LDIF field object.
 * \param value User name to set. Note that reserved fields cannot be
 *              named.
 */
void ldif_field_set_name( Ldif_FieldRec *rec, const gchar *value ) {
	cm_return_if_fail( rec != NULL );

	if( ! rec->reserved ) {
		rec->userName = mgu_replace_string( rec->userName, value );
		g_strstrip( rec->userName );
	}
}

/**
 * Specify selection for field record.
 * \param rec   LDIF field object.
 * \param value Set to <i>TRUE</i> to select field. Note that reserved
 *              fields cannot be unselected.
 */
void ldif_field_set_selected( Ldif_FieldRec *rec, const gboolean value ) {
	cm_return_if_fail( rec != NULL );

	if( ! rec->reserved ) {
		rec->selected = value;
	}
}

/**
 * Toggle selection for field record. Note that reserved fields cannot be
 * toggled.
 * \param rec   LDIF field object.
 */
void ldif_field_toggle( Ldif_FieldRec *rec ) {
	cm_return_if_fail( rec != NULL );

	if( ! rec->reserved ) {
		rec->selected = !rec->selected;
	}
}

/**
 * Free hash table entry visitor function.
 * \param  key   Key.
 * \param  value Value (the LDIF field record).
 * \param  data  User data.
 * \return <code>-1</code>.
*/
static gint ldif_hash_free_vis( gpointer key, gpointer value, gpointer data ) {
	ldif_free_fieldrec( ( Ldif_FieldRec * ) value );
	value = NULL;
	key = NULL;
	return -1;
}

/**
 * Free up object by releasing internal memory.
 * \param ldifFile LDIF import control object.
 */
void ldif_free( LdifFile *ldifFile ) {
	cm_return_if_fail( ldifFile != NULL );

	/* Close file */
	if( ldifFile->file ) fclose( ldifFile->file );

	/* Free internal stuff */
	g_free( ldifFile->path );

	/* Free field list */
	g_hash_table_foreach_remove( ldifFile->hashFields, ldif_hash_free_vis, NULL );
	g_hash_table_destroy( ldifFile->hashFields );
	ldifFile->hashFields = NULL;

	/* Clear pointers */
	ldifFile->file = NULL;
	ldifFile->path = NULL;
	ldifFile->retVal = MGU_SUCCESS;
	ldifFile->tempList = NULL;
	ldifFile->dirtyFlag = FALSE;
	ldifFile->accessFlag = FALSE;
	ldifFile->cbProgress = NULL;

	/* Now release file object */
	g_free( ldifFile );
}

/**
 * Open file for read.
 * \param  ldifFile LDIF import control object.
 * \return <i>TRUE</i> if file opened successfully.
 */
static gint ldif_open_file( LdifFile* ldifFile ) {
	/* g_print( "Opening file\n" ); */
	if( ldifFile->path ) {
		ldifFile->file = g_fopen( ldifFile->path, "rb" );
		if( ! ldifFile->file ) {
			/* g_print( "can't open %s\n", ldifFile->path ); */
			ldifFile->retVal = MGU_OPEN_FILE;
			return ldifFile->retVal;
		}
	}
	else {
		/* g_print( "file not specified\n" ); */
		ldifFile->retVal = MGU_NO_FILE;
		return ldifFile->retVal;
	}

	/* Setup a buffer area */
	ldifFile->retVal = MGU_SUCCESS;
	return ldifFile->retVal;
}

/**
 * Close file.
 * \param  ldifFile LDIF import control object.
 */
static void ldif_close_file( LdifFile *ldifFile ) {
	cm_return_if_fail( ldifFile != NULL );
	if( ldifFile->file ) fclose( ldifFile->file );
	ldifFile->file = NULL;
}

/**
 * Read line of text from file.
 * \param  ldifFile LDIF import control object.
 * \return ptr to buffer where line starts.
 */
static gchar *ldif_get_line( LdifFile *ldifFile ) {
	gchar *buf = g_malloc(LDIFBUFSIZE);
	gint ch;
	int i = 0;
	int cur_alloc = LDIFBUFSIZE;

	if( feof( ldifFile->file ) ) {
		g_free(buf);
		return NULL;
	}

	while( i < cur_alloc-1 ) {
		ch = fgetc( ldifFile->file );
		if (ferror( ldifFile->file ))
			ldifFile->retVal = MGU_ERROR_READ;
		if( ch == '\0' || ch == EOF ) {
			if( i == 0 ) return NULL;
			break;
		}
#if HAVE_DOSISH_SYSTEM
#else
		if( ch == '\r' ) 
			continue;
#endif
		if( ch == '\n' ) 
			break;
		buf[i] = ch;
		i++;
		if (i == cur_alloc-1 && cur_alloc < LDIFBUFSIZE * 32) {
			cur_alloc += LDIFBUFSIZE;
			buf = g_realloc(buf, cur_alloc);
		}
	}
	buf[i] = '\0';

	/* Return a copy of buffer */
	return g_strdup( buf );
}

/**
 * Parse tag name from line buffer.
 * \param  line Buffer.
 * \param  flag64 Base-64 encoder flag.
 * \return Buffer containing the tag name, or NULL if no delimiter char found.
 *         If a double delimiter (::) is found, flag64 is set.
 */
static gchar *ldif_get_tagname( char* line, gboolean *flag64 ) {
	gint len = 0;
	gchar *tag = NULL;
	gchar *lptr = line;
	gchar *sptr = NULL;
	
	while( *lptr++ ) {
		/* Check for language tag */
		if( *lptr == LDIF_LANG_TAG ) {
			if( sptr == NULL ) sptr = lptr;
		}

		/* Check for delimiter */
		if( *lptr == LDIF_SEP_TAG ) {
			if( sptr ) {
				len = sptr - line;
			}
			else {
				len = lptr - line;
			}

			/* Base-64 encoding? */
			if( * ++lptr == LDIF_SEP_TAG ) *flag64 = TRUE;

			tag = g_strndup( line, len+1 );
			tag[ len ] = '\0';
                        return tag;
		}
	}
	return tag;
}

/**
 * Parse tag value from line buffer.
 * \param  line Buffer.
 * \return Buffer containing the tag value. Empty string is returned if
 *         no delimiter char found.
 */
static gchar *ldif_get_tagvalue( gchar* line ) {
	gchar *value = NULL;
	gchar *start = NULL;
	gchar *lptr;
	gint len = 0;

	for( lptr = line; *lptr; lptr++ ) {
		if( *lptr == LDIF_SEP_TAG ) {
			if( ! start )
				start = lptr + 1;
		}
	}
	if( start ) {
		if( *start == LDIF_SEP_TAG ) start++;
		len = lptr - start;
		value = g_strndup( start, len+1 );
		g_strstrip( value );
	}
	else {
		/* Ensure that we get an empty string */
		value = g_strndup( "", 1 );
	}
	value[ len ] = '\0';
	return value;
}

/**
 * Parsed address data record.
 */
typedef struct _Ldif_ParsedRec_ Ldif_ParsedRec;
struct _Ldif_ParsedRec_ {
	GSList *listCName;
	GSList *listFName;
	GSList *listLName;
	GSList *listNName;
	GSList *listAddress;
	GSList *listID;
	GSList *userAttr;
};

/**
 * User attribute data record.
 */
typedef struct _Ldif_UserAttr_ Ldif_UserAttr;
struct _Ldif_UserAttr_ {
	gchar *name;
	gchar *value;
};

/**
 * Build an address list entry and append to list of address items in the
 * address cache. Name is formatted as "<first-name> <last-name>".
 * \param ldifFile LDIF import control object.
 * \param rec      LDIF field object.
 * \param cache    Address cache to be populated with data.
 */
static void ldif_build_items(
		LdifFile *ldifFile, Ldif_ParsedRec *rec, AddressCache *cache )
{
	GSList *nodeFirst;
	GSList *nodeAddress;
	GSList *nodeAttr;
	gchar *firstName = NULL, *lastName = NULL, *fullName = NULL;
	gchar *nickName = NULL;
	gint iLen = 0, iLenT = 0;
	ItemPerson *person;
	ItemEMail *email;

	nodeAddress = rec->listAddress;
//	if( nodeAddress == NULL ) return;

	/* Find longest first name in list */
	nodeFirst = rec->listFName;
	while( nodeFirst ) {
		if( firstName == NULL ) {
			firstName = nodeFirst->data;
			iLen = strlen( firstName );
		}
		else {
			if( ( iLenT = strlen( nodeFirst->data ) ) > iLen ) {
				firstName = nodeFirst->data;
				iLen = iLenT;
			}
		}
		nodeFirst = g_slist_next( nodeFirst );
	}

	/* Format name */
	if( rec->listLName ) {
		lastName = rec->listLName->data;
	}

	if( firstName ) {
		if( lastName ) {
			fullName = g_strdup_printf(
				"%s %s", firstName, lastName );
		}
		else {
			fullName = g_strdup_printf( "%s", firstName );
		}
	}
	else {
		if( lastName ) {
			fullName = g_strdup_printf( "%s", lastName );
		}
	}
	
	if (!fullName || strlen(fullName) == 0) {
		g_free(fullName);
		fullName = NULL;
		if (rec->listCName)
			fullName = g_strdup(rec->listCName->data);
	}
	
	if( fullName ) {
		g_strchug( fullName ); g_strchomp( fullName );
	}

	if( rec->listNName ) {
		nickName = rec->listNName->data;
	}

	person = addritem_create_item_person();
	addritem_person_set_common_name( person, fullName );
	addritem_person_set_first_name( person, firstName );
	addritem_person_set_last_name( person, lastName );
	addritem_person_set_nick_name( person, nickName );
	addrcache_id_person( cache, person );
	addrcache_add_person( cache, person );
	++ldifFile->importCount;

	/* Add address item */
	while( nodeAddress ) {
		email = addritem_create_item_email();
		addritem_email_set_address( email, nodeAddress->data );
		addrcache_id_email( cache, email );
		addrcache_person_add_email( cache, person, email );
		nodeAddress = g_slist_next( nodeAddress );
	}
	g_free( fullName );
	fullName = firstName = lastName = NULL;

	/* Add user attributes */
	nodeAttr = rec->userAttr;
	while( nodeAttr ) {
		Ldif_UserAttr *attr = nodeAttr->data;
		UserAttribute *attrib = addritem_create_attribute();
		addritem_attrib_set_name( attrib, attr->name );
		addritem_attrib_set_value( attrib, attr->value );
		addritem_person_add_attribute( person, attrib );
		nodeAttr = g_slist_next( nodeAttr );
	}
	nodeAttr = NULL;
}

/**
 * Add selected field as user attribute.
 * \param rec       LDIF field object.
 * \param tagName   LDIF tag name.
 * \param tagValue  Data value.
 * \param hashField Hash table to populate.
 */
static void ldif_add_user_attr(
		Ldif_ParsedRec *rec, gchar *tagName, gchar *tagValue,
		GHashTable *hashField )
{
	Ldif_FieldRec *fld = NULL;
	Ldif_UserAttr *attr = NULL;
	gchar *name;

	fld = g_hash_table_lookup( hashField, tagName );
	if( fld ) {
		if( ! fld->selected ) return;

		name = fld->tagName;
		if( fld->userName ) {
			name = fld->userName;
		}
		attr = g_new0( Ldif_UserAttr, 1 );
		attr->name = g_strdup( name );
		attr->value = g_strdup( tagValue );
		rec->userAttr = g_slist_append( rec->userAttr, attr );
	}
}

/**
 * Add value to parsed data.
 * \param rec       LDIF field object.
 * \param tagName   LDIF tag name.
 * \param tagValue  Data value.
 * \param hashField Hash table to populate.
 */
static void ldif_add_value(
	       Ldif_ParsedRec *rec, gchar *tagName, gchar *tagValue,
	       GHashTable *hashField )
{
	gchar *nm, *val;

	nm = g_utf8_strdown( tagName, -1 );
	if( tagValue ) {
		val = g_strdup( tagValue );
	}
	else {
		val = g_strdup( "" );
	}
	g_strstrip( val );

	if( g_utf8_collate( nm, g_utf8_strdown( LDIF_TAG_COMMONNAME, -1 ) ) == 0 ) {
		rec->listCName = g_slist_append( rec->listCName, val );
	}
	else if( g_utf8_collate( nm, g_utf8_strdown( LDIF_TAG_FIRSTNAME, -1 ) ) == 0 ) {
		rec->listFName = g_slist_append( rec->listFName, val );
	}
	else if( g_utf8_collate( nm, g_utf8_strdown( LDIF_TAG_LASTNAME, -1 ) ) == 0 ) {
		rec->listLName = g_slist_append( rec->listLName, val );
	}
	else if( g_utf8_collate( nm, g_utf8_strdown( LDIF_TAG_NICKNAME, -1 ) ) == 0 ) {
		rec->listNName = g_slist_append( rec->listNName, val );
	}
	else if( g_utf8_collate( nm, g_utf8_strdown( LDIF_TAG_EMAIL, -1 ) ) == 0 ) {
		rec->listAddress = g_slist_append( rec->listAddress, val );
	}
	else {
		/* Add field as user attribute */
		ldif_add_user_attr( rec, tagName, tagValue, hashField );
	}
	g_free( nm );
}

/**
 * Clear parsed data record.
 * \param rec LDIF field object.
 */
static void ldif_clear_rec( Ldif_ParsedRec *rec ) {
	GSList *list;

	/* Free up user attributes */
	list = rec->userAttr;
	while( list ) {
		Ldif_UserAttr *attr = list->data;
		g_free( attr->name );
		g_free( attr->value );
		g_free( attr );
		list = g_slist_next( list );
	}
	g_slist_free( rec->userAttr );

	g_slist_free( rec->listCName );
	g_slist_free( rec->listFName );
	g_slist_free( rec->listLName );
	g_slist_free( rec->listNName );
	g_slist_free( rec->listAddress );
	g_slist_free( rec->listID );

	rec->userAttr = NULL;
	rec->listCName = NULL;
	rec->listFName = NULL;
	rec->listLName = NULL;
	rec->listNName = NULL;
	rec->listAddress = NULL;
	rec->listID = NULL;
}

/**
 * Read file data into address cache.
 * Note that one LDIF record identifies one entity uniquely with the
 * distinguished name (dn) tag. Each person can have multiple E-Mail
 * addresses. Also, each person can have many common name (cn) tags.
 *
 * \param  ldifFile LDIF import control object.
 * \param  cache    Address cache to be populated with data.
 */
static void ldif_read_file( LdifFile *ldifFile, AddressCache *cache ) {
	gchar *tagName = NULL, *tagValue = NULL;
	gchar *lastTag = NULL, *fullValue = NULL;
	GSList *listValue = NULL;
	gboolean flagEOF = FALSE, flagEOR = FALSE;
	gboolean flag64 = FALSE, last64 = FALSE;
	Ldif_ParsedRec *rec;
	long posEnd = 0L;
	long posCur = 0L;
	GHashTable *hashField;

	hashField = ldifFile->hashFields;
	rec = g_new0( Ldif_ParsedRec, 1 );
	ldif_clear_rec( rec );

	/* Find EOF for progress indicator */
	fseek( ldifFile->file, 0L, SEEK_END );
	posEnd = ftell( ldifFile->file );
	fseek( ldifFile->file, 0L, SEEK_SET );

	while( ! flagEOF ) {
		gchar *line =  ldif_get_line( ldifFile );

		posCur = ftell( ldifFile->file );
		if( ldifFile->cbProgress ) {
			/* Call progress indicator */
			( ldifFile->cbProgress ) ( ldifFile, & posEnd, & posCur );
		}

		flag64 = FALSE;
		if( line == NULL ) {
			flagEOF = flagEOR = TRUE;
		}
		else if( *line == '\0' ) {
			flagEOR = TRUE;
		}

		if( flagEOR ) {
			/* EOR, Output address data */
			if( lastTag ) {
				/* Save record */
				fullValue = mgu_list_coalesce( listValue );
				if (fullValue && last64) {
					gchar *out = g_malloc(strlen(fullValue));
					int len = 0;
					if ((len = base64_decode(out, fullValue,
							strlen(fullValue))) >= 0) {
						g_free(fullValue);
						fullValue = out;
						fullValue[len] = '\0';
					} else
						g_free(out);
				}
				/* Base-64 encoded data */
				/*
				if( last64 ) {
					ldif_dump_b64( fullValue );
				}
				*/

				ldif_add_value( rec, lastTag, fullValue, hashField );
				/* ldif_print_record( rec, stdout ); */
				ldif_build_items( ldifFile, rec, cache );
				ldif_clear_rec( rec );
				g_free( lastTag );
				mgu_free_list( listValue );
				lastTag = NULL;
				listValue = NULL;
				last64 = FALSE;
			}
		}
		if( line ) {
			flagEOR = FALSE;
			if( *line == ' ' ) {
				/* Continuation line */
				listValue = g_slist_append(
					listValue, g_strdup( line+1 ) );
			}
			else if( *line == '=' ) {
				/* Base-64 encoded continuation field */
				listValue = g_slist_append(
					listValue, g_strdup( line ) );
			}
			else {
				/* Parse line */
				tagName = ldif_get_tagname( line, &flag64 );
				if( tagName ) {
					tagValue = ldif_get_tagvalue( line );
					if( tagValue ) {
						if( lastTag ) {
							/* Save data */
							fullValue =
								mgu_list_coalesce( listValue );
							if (fullValue && last64) {
								gchar *out = g_malloc(strlen(fullValue));
								int len = 0;
								if ((len = base64_decode(out, fullValue,
										strlen(fullValue))) >= 0) {
									g_free(fullValue);
									fullValue = out;
									fullValue[len] = '\0';
								} else
									g_free(out);
							}
							/* Base-64 encoded data */
							/*
							if( last64 ) {
								ldif_dump_b64( fullValue );
							}
							*/

							ldif_add_value(
								rec, lastTag, fullValue,
								hashField );
							g_free( lastTag );
							mgu_free_list( listValue );
							lastTag = NULL;
							listValue = NULL;
						}

						lastTag = g_strdup( tagName );
						listValue = g_slist_append(
							listValue,
							g_strdup( tagValue ) );
						g_free( tagValue );
						last64 = flag64;
					}
					g_free( tagName );
				}
			}
		}
		g_free( line );
	}

	/* Release data */
	ldif_clear_rec( rec );
	g_free( rec );
	g_free( lastTag );
	mgu_free_list( listValue );
}

/**
 * Add list of field names to hash table.
 * \param table Hashtable.
 * \param list  List of fields.
 */
static void ldif_hash_add_list( GHashTable *table, GSList *list ) {
	GSList *node = list;

	/* mgu_print_list( list, stdout ); */
	while( node ) {
		gchar *tag = node->data;
		if( ! g_hash_table_lookup( table, tag ) ) {
			Ldif_FieldRec *rec = NULL;
			gchar *key = g_utf8_strdown( tag, -1 );

			rec = ldif_create_fieldrec( tag );
			if( g_utf8_collate( key, LDIF_TAG_DN ) == 0 ) {
				rec->reserved = rec->selected = TRUE;
				rec->userName = g_strdup( "dn" );
			}
			else if( g_utf8_collate( key, g_utf8_strdown( LDIF_TAG_COMMONNAME, -1 ) ) == 0 ) {
				rec->reserved = rec->selected = TRUE;
				rec->userName = g_strdup( _( "Display Name" ) );
			}
			else if( g_utf8_collate( key, g_utf8_strdown( LDIF_TAG_FIRSTNAME, -1 ) ) == 0 ) {
				rec->reserved = rec->selected = TRUE;
				rec->userName = g_strdup( _( "First Name" ) );
			}
			else if( g_utf8_collate( key, g_utf8_strdown( LDIF_TAG_LASTNAME, -1 ) ) == 0 ) {
				rec->reserved = rec->selected = TRUE;
				rec->userName = g_strdup( _( "Last Name" ) );
			}
			else if( g_utf8_collate( key, g_utf8_strdown( LDIF_TAG_NICKNAME, -1 ) ) == 0 ) {
				rec->reserved = rec->selected = TRUE;
				rec->userName = g_strdup( _( "Nick Name" ) );
			}
			else if( g_utf8_collate( key, g_utf8_strdown( LDIF_TAG_EMAIL, -1 ) ) == 0 ) {
				rec->reserved = rec->selected = TRUE;
				rec->userName = g_strdup( _( "Email Address" ) );
			}
			g_hash_table_insert( table, key, rec );
		}
		node = g_slist_next( node );
	}
}

/**
 * Sorted list comparison function.
 * \param  ptr1 First field.
 * \param  ptr2 Second field.
 * \return <code>-1, 0, +1</code> if first record less than, equal,
 *         greater than second.
 */
static gint ldif_field_compare( gconstpointer ptr1, gconstpointer ptr2 ) {
	const Ldif_FieldRec *rec1 = ptr1;
	const Ldif_FieldRec *rec2 = ptr2;

	if( rec1->reserved ) {
		if( ! rec2->reserved ) {
			return +1;
		}
	}
	else {
		if( rec2->reserved ) {
			return -1;
		}
	}
	return g_utf8_collate( rec1->tagName, rec2->tagName );
}

/*
 * Append hash table entry to list - visitor function.
 * \param key   Key.
 * \param value Data value.
 * \param data  User data (the LDIF import control object).
 */
static void ldif_hash2list_vis( gpointer key, gpointer value, gpointer data ) {
	LdifFile *ldf = data;
	ldf->tempList =
		g_list_insert_sorted( ldf->tempList, value, ldif_field_compare );
}

/**
 * Read tag names for file data.
 * \param  ldifFile LDIF import control object.
 */
static void ldif_read_tag_list( LdifFile *ldifFile ) {
	gchar *tagName = NULL;
	GSList *listTags = NULL;
	gboolean flagEOF = FALSE, flagEOR = FALSE, flagMail = FALSE;
	gboolean flag64 = FALSE;
	long posEnd = 0L;
	long posCur = 0L;

	/* Clear hash table */
	g_hash_table_foreach_remove(
		ldifFile->hashFields, ldif_hash_free_vis, NULL );

	/* Find EOF for progress indicator */
	fseek( ldifFile->file, 0L, SEEK_END );
	posEnd = ftell( ldifFile->file );
	fseek( ldifFile->file, 0L, SEEK_SET );

	if (posEnd == 0) {
		ldifFile->retVal = MGU_EOF;
		return;
	}
		
	/* Process file */
	while( ! flagEOF ) {
		gchar *line = ldif_get_line( ldifFile );
		posCur = ftell( ldifFile->file );
		if( ldifFile->cbProgress ) {
			/* Call progress indicator */
			( ldifFile->cbProgress ) ( ldifFile, & posEnd, & posCur );
		}

		flag64 = FALSE;
		if( line == NULL ) {
			flagEOF = flagEOR = TRUE;
		}
		else if( *line == '\0' ) {
			flagEOR = TRUE;
		}

		if( flagEOR ) {
			/* EOR, Output address data */
			/* Save field list to hash table */
			if( flagMail ) {
				ldif_hash_add_list(
					ldifFile->hashFields, listTags );
			}
			mgu_free_list( listTags );
			listTags = NULL;
			flagMail = FALSE;
		}
		if( line ) {
			flagEOR = FALSE;
			if( *line == ' ' ) {
				/* Continuation line */
			}
			else if( *line == '=' ) {
				/* Base-64 encoded continuation field */
			}
			else {
				/* Parse line */
				tagName = ldif_get_tagname( line, &flag64 );
				if( tagName ) {
					/* Add tag to list */
					listTags = g_slist_append( listTags, tagName );

					if( g_utf8_collate(
						tagName, LDIF_TAG_EMAIL ) == 0 )
					{
						flagMail = TRUE;
					}
				} else {
					g_strstrip(line);
					if (*line != '\0') {
						debug_print("ldif: bad format: '%s'\n", line);
						ldifFile->retVal = MGU_BAD_FORMAT;
					}
				}
			}
		}
		g_free( line );
	}

	/* Release data */
	mgu_free_list( listTags );
	listTags = NULL;
}

/**
 * Read file into list. Main entry point
 * \param  ldifFile LDIF import control object.
 * \param  cache    Address cache to load.
 * \return Status code.
 */
gint ldif_import_data( LdifFile *ldifFile, AddressCache *cache ) {
	cm_return_val_if_fail( ldifFile != NULL, MGU_BAD_ARGS );
	ldifFile->retVal = MGU_SUCCESS;
	addrcache_clear( cache );
	cache->dataRead = FALSE;
	ldif_open_file( ldifFile );
	if( ldifFile->retVal == MGU_SUCCESS ) {
		/* Read data into the cache */
		ldif_read_file( ldifFile, cache );
		ldif_close_file( ldifFile );

		/* Mark cache */
		cache->modified = FALSE;
		cache->dataRead = TRUE;
	}
	return ldifFile->retVal;
}

/**
 * Process entire file reading list of unique fields. List of fields may be
 * accessed with the <code>ldif_get_fieldlist()</code> function.
 * \param  ldifFile LDIF import control object.
 * \return Status code.
 */
gint ldif_read_tags( LdifFile *ldifFile ) {
	cm_return_val_if_fail( ldifFile != NULL, MGU_BAD_ARGS );
	ldifFile->retVal = MGU_SUCCESS;
	if( ldifFile->dirtyFlag ) {
		ldif_open_file( ldifFile );
		if( ldifFile->retVal == MGU_SUCCESS ) {
			/* Read data into the cache */
			ldif_read_tag_list( ldifFile );
			ldif_close_file( ldifFile );
			ldifFile->dirtyFlag = FALSE;
			ldifFile->accessFlag = TRUE;
		}
	}
	return ldifFile->retVal;
}

/**
 * Return list of fields for LDIF file.
 * \param  ldifFile LDIF import control object.
 * \return Linked list of <code>Ldif_FieldRec</code> objects. This list may be
 *         <code>g_free()</code>. Note that the objects in the list should not
 *         be freed since they refer to objects inside the internal cache.
 *         These objects will be freed when LDIF file object is freed.
 */
GList *ldif_get_fieldlist( LdifFile *ldifFile ) {
	GList *list = NULL;

	cm_return_val_if_fail( ldifFile != NULL, NULL );
	if( ldifFile->hashFields ) {
		ldifFile->tempList = NULL;
		g_hash_table_foreach( ldifFile->hashFields, ldif_hash2list_vis, ldifFile );
		list = ldifFile->tempList;
		ldifFile->tempList = NULL;
	}
	return list;
}

/**
 * Output LDIF name-value pair to stream. Only non-empty names and values will
 * be output to file.
 * \param stream File output stream.
 * \param name   Name.
 * \param value  Data value.
 * \return <i>TRUE</i> if data output.
 */
gboolean ldif_write_value( FILE *stream, const gchar *name, const gchar *value ) {
	if( name == NULL ) return FALSE;
	if( value == NULL ) return FALSE;
	if( strlen( name ) < 1 ) return FALSE;
	if( strlen( value ) < 1 ) return FALSE;
	fprintf( stream, "%s: ", name );
	fprintf( stream, "%s\n", value );
	return TRUE;
}

/**
 * Output LDIF End of Record to stream.
 * \param stream File output stream.
 * \return <i>TRUE</i> if data output.
 */
void ldif_write_eor( FILE *stream ) {
	/* Simple but caller should not need to know how to end record. */
	fprintf( stream, "\n" );
}

/*
 * ============================================================================
 * End of Source.
 * ============================================================================
 */

