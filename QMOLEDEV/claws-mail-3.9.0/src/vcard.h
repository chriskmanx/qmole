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
 * Definitions necessary to access vCard files. vCard files are used
 * by GnomeCard for addressbook, and Netscape for sending business
 * card information. Refer to RFC2426 for more information.
 */

#ifndef __VCARD_H__
#define __VCARD_H__

#include <stdio.h>
#include <glib.h>

#include "addritem.h"
#include "addrcache.h"
#include "adbookbase.h"

#define VCARDBUFSIZE       1024

#define	VCARD_TAG_START    "begin"
#define	VCARD_TAG_END      "end"
#define	VCARD_NAME         "vcard"

#define	VCARD_TAG_FULLNAME "fn"
#define VCARD_TAG_NAME     "n"
#define	VCARD_TAG_EMAIL    "email"
#define VCARD_TAG_UID      "uid"

#define VCARD_TYPE_QP      "quoted-printable"

#define	VCARD_SEP_TAG      ':'
#define	VCARD_SEP_TYPE     ';'

/*
* Typical vCard entry:
*
* BEGIN:VCARD
* FN:Axle Rose
* N:Rose;Axle;D;Ms;Jnr
* REV:2001-04-22T03:52:05
* ADR;HOME:;;777 Lexington Avenue;Denver;CO;80299;USA
* ADR;POSTAL:P O Box 777;;;Denver;CO;80298;Usa
* TEL;HOME:303-555-1234
* EMAIL;AOL:axlerose@aol.com
* EMAIL;INTERNET:axlerose@netscape.net
* TITLE:Janitor
* ORG:The Company
* URL:http://www.axlerose.com
* END:VCARD
*/

/* vCard object */
typedef struct _VCardFile VCardFile;
struct _VCardFile {
	AddressBookType type;
	AddressCache *addressCache;
	gint     retVal;
	FILE     *file;
	gchar    *path;
	gchar    buffer[ VCARDBUFSIZE ];
	gchar    *bufptr;
};

/* Function prototypes */
VCardFile *vcard_create			( void );
void vcard_set_name			( VCardFile* cardFile, const gchar *value );
void vcard_set_file			( VCardFile* cardFile, const gchar *value );
void vcard_set_modified			( VCardFile *cardFile, const gboolean value );
void vcard_set_accessed			( VCardFile *cardFile, const gboolean value );
gboolean vcard_get_modified		( VCardFile *cardFile );
gboolean vcard_get_accessed		( VCardFile *cardFile );
gboolean vcard_get_read_flag		( VCardFile *cardFile );
gint vcard_get_status			( VCardFile *cardFile );
ItemFolder *vcard_get_root_folder	( VCardFile *cardFile );
gchar *vcard_get_name			( VCardFile *cardFile );
void vcard_free				( VCardFile *cardFile );
gint vcard_read_data			( VCardFile *cardFile );
GList *vcard_get_list_person		( VCardFile *cardFile );
GList *vcard_get_list_folder		( VCardFile *cardFile );
GList *vcard_get_all_persons		( VCardFile *cardFile );
gchar *vcard_find_gnomecard		( void );
gint vcard_test_read_file		( const gchar *fileSpec );

#endif /* __VCARD_H__ */

