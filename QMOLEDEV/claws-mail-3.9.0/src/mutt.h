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
 * Definitions necessary to access MUTT addressbook files. These are
 * used by the MUTT E-Mail client.
 */

#ifndef __MUTT_H__
#define __MUTT_H__

#include <stdio.h>
#include <glib.h>

#include "addrcache.h"

/*
* Typical MUTT entry:
*
* alias alias-name email-address (friendly-name) [, email-address (friendly-name) ...]
*
* The alias-name maybe the name of an individual or the name of a group
* comprising many individuals. The alias name should not contain the space,
* comma (,) or at (@) character. The friendly-name is optional. A back-slash
* character at end-of-line may be used as a continuation character.
*
* An example:
* 
* alias myhome  axel@axelrose.com (Axel Rose)
* alias axe     axelrose@aol.com  (The Axe)
* alias buddies esr@wheres-my-mail.com (Eric Raymond), bgates@hotmail.com (Oops Lost It)
*
*/

/* MUTT file object */
typedef struct _MuttFile MuttFile;
struct _MuttFile {
	FILE  *file;
	gchar *path;
	gint  retVal;
	GHashTable *uniqTable;
	void  (*cbProgress)( void *, void *, void * );
};

/* Function prototypes */
MuttFile *mutt_create	( void );
void mutt_set_file	( MuttFile* muttFile, const gchar *value );
void mutt_free		( MuttFile *muttFile );
gint mutt_import_data	( MuttFile *muttFile, AddressCache *cache );
gchar *mutt_find_file	( void );

#endif /* __MUTT_H__ */

