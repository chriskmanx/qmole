/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 Match Grun and the Claws Mail team
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
 * Definitions for export to LDIF file.
 */

#ifndef __EXPORT_LDIF_H__
#define __EXPORT_LDIF_H__

#include <glib.h>

#include "addrcache.h"

/* RDN ID's */
#define EXPORT_LDIF_ID_UID      0
#define EXPORT_LDIF_ID_DNAME    1
#define EXPORT_LDIF_ID_EMAIL    2

#define EXPORT_LDIF_FIRST_LAST  0
#define EXPORT_LDIF_LAST_FIRST  1

/* Export LDIF control data */
typedef struct _ExportLdifCtl ExportLdifCtl;
struct _ExportLdifCtl {
	gchar    *path;
	gchar    *dirOutput;
	gchar    *fileLdif;
	gchar    *settingsFile;
	gchar    *suffix;
	gint     rdnIndex;
	gboolean useDN;
	gboolean excludeEMail;
	gint     retVal;
	gint     rcCreate;
};

/* Function prototypes */
ExportLdifCtl *exportldif_create	( void );
void exportldif_free			( ExportLdifCtl *ctl );
void exportldif_set_file_html		( ExportLdifCtl *ctl,
					  const gchar *value );
void exportldif_set_suffix		( ExportLdifCtl *ctl,
					  const gchar *value );
void exportldif_set_rdn			( ExportLdifCtl *ctl,
					  const gint value );
void exportldif_set_use_dn		( ExportLdifCtl *ctl,
					  const gboolean value );
void exportldif_set_exclude_email	( ExportLdifCtl *ctl,
					  const gboolean value );
void exportldif_process			( ExportLdifCtl *ctl,
					  AddressCache *cache );
gboolean exportldif_test_dir		( ExportLdifCtl *ctl );
gboolean exportldif_create_dir		( ExportLdifCtl *ctl );
gchar *exportldif_get_create_msg	( ExportLdifCtl *ctl );

void exportldif_parse_filespec		( ExportLdifCtl *ctl,
					  gchar *fileSpec );
void exportldif_load_settings		( ExportLdifCtl *ctl );
void exportldif_save_settings		( ExportLdifCtl *ctl );

#endif /* __EXPORT_LDIF_H__ */

