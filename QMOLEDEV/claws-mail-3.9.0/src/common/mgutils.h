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

#ifndef __MGUTILS_H__
#define __MGUTILS_H__

#include <stdio.h>
#include <glib.h>

/* Error codes */
#define MGU_SUCCESS        0
#define MGU_BAD_ARGS       -1
#define MGU_NO_FILE        -2
#define MGU_OPEN_FILE      -3
#define MGU_ERROR_READ     -4
#define MGU_EOF            -5
#define MGU_OO_MEMORY      -6
#define MGU_BAD_FORMAT     -7
#define MGU_ERROR_WRITE    -15
#define MGU_OPEN_DIRECTORY -16
#define MGU_NO_PATH        -17

/* Function prototypes */
void mgu_print_list		( GSList *list, FILE *stream );
void mgu_print_dlist		( GList *list, FILE *stream );
void mgu_free_list		( GSList *list );
void mgu_free_dlist		( GList *list );
gchar *mgu_list_coalesce	( GSList *list );
gchar *mgu_replace_string	( gchar *str, const gchar *value );
void mgu_clear_slist		( GSList *list );
void mgu_clear_list		( GList *list );
gchar *mgu_email_check_empty	( gchar *address );
GList *mgu_parse_string		( gchar *line, const gint maxTokens,
				  gint *tokenCnt );
void mgu_str_unescape		( gchar *str );
void mgu_str_ltc2space		( gchar *str, gchar chlead, gchar chtail );
gchar *mgu_slist_longest_entry	( GSList *list );
gboolean mgu_slist_test_unq_nc	( GSList *list, gchar *str );
gboolean mgu_list_test_unq_nc	( GList *list, gchar *str );

#endif /* __MGUTILS_H__ */

/*
* End of Source.
*/

