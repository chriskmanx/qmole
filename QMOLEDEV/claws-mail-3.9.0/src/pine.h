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
 * Definitions necessary to access Pine addressbook files. These are
 * used by the Pine E-Mail client.
 */

#ifndef __PINE_H__
#define __PINE_H__

#include <stdio.h>
#include <glib.h>

#include "addrcache.h"

/*
* Typical Pine simple entry:
*
*   nick-name \t friendly-name \t email-address \t fcc \t comments
*
* Or for a distribution list:
*
*   group-name \t friendly-name \t ( email, email, ... ) \t fcc \t comments
*
* The record is may span several lines if longer than about 80 characters.
* Pine formats subsequent line with exactly 3 spaces to indicate that the
* line is a continuation.
*
* An example:
* 
* axe \t Axel Rose \t axelrose@aol.com \t \t Guitar player
*  \t Axel Rose \t axelrose@aol.com \t \t Guitarist
* evilaxis \t \t (bgates@hotmail.com,shoebomber@empire.com) \t \t Axis of Evil
*
*/

/* Pine file object */
typedef struct _PineFile PineFile;
struct _PineFile {
	FILE  *file;
	gchar *path;
	gint  retVal;
	GHashTable *uniqTable;
	void  (*cbProgress)( void *, void *, void * );
};

/* Function prototypes */
PineFile *pine_create	( void );
void pine_set_file	( PineFile* pineFile, const gchar *value );
void pine_free		( PineFile *pineFile );
gint pine_import_data	( PineFile *pineFile, AddressCache *cache );
gchar *pine_find_file	( void );

#endif /* __PINE_H__ */

