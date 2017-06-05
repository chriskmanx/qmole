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
 * Definitions for generic functions.
 */

#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "mgutils.h"

/*
* Dump linked list of character strings (for debug).
*/
void mgu_print_list( GSList *list, FILE *stream ) {
	GSList *node = list;
	while( node ) {
		int r = fprintf( stream, "\t- >%s<\n", (gchar *)node->data );
		if (r < 0) {
			perror("fprintf");
			break;
		}
		node = g_slist_next( node );
	}
}

/*
* Dump linked list of character strings (for debug).
*/
void mgu_print_dlist( GList *list, FILE *stream ) {
	GList *node = list;
	while( node ) {
		int r = fprintf( stream, "\t- >%s<\n", (gchar *)node->data );
		if (r < 0) {
			perror("fprintf");
			break;
		}
		node = g_list_next( node );
	}
}

/*
* Free linked list of character strings.
*/
void mgu_free_list( GSList *list ) {
	GSList *node = list;
	while( node ) {
		g_free( node->data );
		node->data = NULL;
		node = g_slist_next( node );
	}
	g_slist_free( list );
}

/*
* Free linked list of character strings.
*/
void mgu_free_dlist( GList *list ) {
	GList *node = list;
	while( node ) {
		g_free( node->data );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( list );
}

/*
* Coalesce linked list of characaters into one long string.
*/
gchar *mgu_list_coalesce( GSList *list ) {
	gchar *str = NULL;
	gchar *buf = NULL;
	gchar *start = NULL;
	GSList *node = NULL;
	gint len;

	if( ! list ) return NULL;

	/* Calculate maximum length of text */
	len = 0;
	node = list;
	while( node ) {
		str = node->data;
		len += 1 + strlen( str );
		node = g_slist_next( node );
	}

	/* Create new buffer. */
	buf = g_new0( gchar, len+1 );
	start = buf;
	node = list;
	while( node ) {
		str = node->data;
		len = strlen( str );
		strcpy( start, str );
		start += len;
		node = g_slist_next( node );
	}
	return buf;
}

/*
* Replace existing string with new string.
*/
gchar *mgu_replace_string( gchar *str, const gchar *value ) {
	g_free( str );
	if( value ) {
		str = g_strdup( value );
		g_strstrip( str );
	}
	else {
		str = NULL;
	}
	return str;
}

/*
* Clear a linked list by setting node data pointers to NULL. Note that
* items are not freed.
*/
void mgu_clear_slist( GSList *list ) {
	GSList *node = list;
	while( node ) {
		node->data = NULL;
		node = g_slist_next( node );
	}
}

/*
* Clear a linked list by setting node data pointers to NULL. Note that
* items are not freed.
*/
void mgu_clear_list( GList *list ) {
	GList *node = list;
	while( node ) {
		node->data = NULL;
		node = g_list_next( node );
	}
}

/*
* Test and reformat an email address.
* Enter:  address.
* Return: Address, or NULL if address is empty.
* Note: Leading and trailing white space is removed.
*/
gchar *mgu_email_check_empty( gchar *address ) {
	gchar *retVal = NULL;
	if( address ) {
		retVal = g_strdup( address );
		retVal = g_strchug( retVal );
		retVal = g_strchomp( retVal );
		if( *retVal == '\0' ) {
			g_free( retVal );
			retVal = NULL;
		}
	}
	return retVal;
}

/*
* Parse string into linked list. Whitespace is used as a delimiter in parsing.
* Strings are parsed until maxTokens - 1 is reached. The remainder of the
* input string is copied into last element of list.
* Enter: line      String to parse.
*        maxTokens Maximum number of tokens to parse.
*        tokenCnt  If arg supplied, update with count of number of token parsed.
* Return: Linked list. The list contents should be g_free'd and list should
* freed when done.
*/
GList *mgu_parse_string( gchar *line, const gint maxTokens, gint *tokenCnt ) {
	gchar *ptr, *pStart, *pFound, *str;
	gint  args = 0;
	GList *list = NULL;
	gboolean done = FALSE;

	if( tokenCnt ) *tokenCnt = 0;
	if( line == NULL ) return NULL;
	if( maxTokens < 1 ) return NULL;

	ptr = line;
	while( ! done ) {
		args++;
		/* Skip over leading spaces */
		while( *ptr ) {
			if( ! isspace( *ptr ) ) break;
			ptr++;	
		}

		/* Find terminating space */
		pFound = NULL;
		pStart = ptr;
		while( *ptr ) {
			if( isspace( *ptr ) ) {
				pFound = pStart;
				break;
			}
			ptr++;
		}

		if( pFound ) {
			if( args == maxTokens ) {
				/* Rest of string */
				str = g_strdup( pStart );
				done = TRUE;
			}
			else {
				/* Extract part of string */
				str = g_strndup( pStart, ptr - pFound );
			}
		}
		else {
			/* Nothing there - treat as rest of string */
			str = g_strdup( pStart );
			done = TRUE;
		}
		list = g_list_append( list, str );
	}
	if( tokenCnt ) *tokenCnt = args;
	return list;
}

/*
 * Unescape characters by removing backslash character from input string.
 * Enter: str String to process.
 */
void mgu_str_unescape( gchar *str ) {
	gchar *p;
	gint ilen;

	p = str;
	while( *p ) {
		if( *p == '\\' ) {
			ilen = strlen( p + 1 );
			memmove( p, p + 1, ilen );
		}
		p++;
	}
}

/*
 * Replace leading and trailing characters (eg, quotes) in input string
 * with spaces. Only matching non-blank characters that appear at both
 * start and end of string are replaces. Control characters are also
 * replaced with spaces.
 * Enter: str    String to process.
 *        chlea  Lead character to remove.
 *        chtail Matching trailing character.
 */
void mgu_str_ltc2space( gchar *str, gchar chlead, gchar chtail ) {
	gchar *as;
	gchar *ae;

	/* Search forwards for first non-space match */
	as = str;
	ae = -1 + str + strlen( str );
	while( as < ae ) {
		if( *as != ' ' ) {
			if( *as == chlead ) {
				/* Search backwards from end for match */
				while( ae > as ) {
					if( *ae != ' ' ) {
						if( *ae == chtail ) {
							*as = ' ';
							*ae = ' ';
							return;
						}
						if( *ae < 32 ) {
							*ae = ' ';
						}
						else if( *ae == 127 ) {
							*ae = ' ';
						}
						else {
							return;
						}
					}
					ae--;
				}
			}
			if( *as < 32 ) {
				*as = ' ';
			}
			else if( *as == 127 ) {
				*as = ' ';
			}
			else {
				return;
			}
		}
		as++;
	}
	return;
}

/*
 * Return reference to longest entry in the specified linked list.
 * It is assumed that the list contains only gchar objects.
 * Enter:  list List of gchar strings to examine.
 * Return: Reference to longest entry, or NULL if nothing found.
 */
gchar *mgu_slist_longest_entry( GSList *list ) {
	GSList *node;
	gchar *name = NULL;
	gint iLen = 0, iLenT = 0;

	node = list;
	while( node ) {
		if( name == NULL ) {
			name = node->data;
			iLen = strlen( name );
		}
		else {
			iLenT = strlen( node->data );
			if( iLenT > iLen ) {
				name = node->data;
				iLen = iLenT;
			}
		}
		node = g_slist_next( node );
	}
	return name;
}	

/*
 * Test whether string appears in list of strings, ignoring case. NULL or empty
 * strings will be ignored.
 * Enter: list List to process.
 *        str  String to test.
 * Return: TRUE if string is unique.
 */
gboolean mgu_slist_test_unq_nc( GSList *list, gchar *str ) {
	GSList *node;

	if( str ) {
		if( strlen( str ) > 0 ) {
			node = list;
			while( node ) {
				if( g_utf8_collate( str, node->data ) == 0 )
					return FALSE;
				node = g_slist_next( node );
			}
			return TRUE;
		}
	}
	return FALSE;
}

/*
 * Test whether string appears in list of strings, ignoring case. NULL or empty
 * strings will be ignored.
 * Enter: list List to process.
 *        str  String to test.
 * Return: TRUE if string is unique.
 */
gboolean mgu_list_test_unq_nc( GList *list, gchar *str ) {
	GList *node;

	if( str ) {
		if( strlen( str ) > 0 ) {
			node = list;
			while( node ) {
				if( g_utf8_collate( str, node->data ) == 0 )
					return FALSE;
				node = g_list_next( node );
			}
			return TRUE;
		}
	}
	return FALSE;
}

/*
* End of Source.
*/
