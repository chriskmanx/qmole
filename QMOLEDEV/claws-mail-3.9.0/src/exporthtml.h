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
 * Definitions for an E-Mail address harvester.
 */

#ifndef __EXPORT_HTML_H__
#define __EXPORT_HTML_H__

#include <glib.h>

#include "addrcache.h"

/* Stylesheet ID's */
#define EXPORT_HTML_ID_NONE     0
#define EXPORT_HTML_ID_DEFAULT  1
#define EXPORT_HTML_ID_FULL     2
#define EXPORT_HTML_ID_CUSTOM   3
#define EXPORT_HTML_ID_CUSTOM2  4
#define EXPORT_HTML_ID_CUSTOM3  5
#define EXPORT_HTML_ID_CUSTOM4  6

#define EXPORT_HTML_FIRST_LAST  0
#define EXPORT_HTML_LAST_FIRST  1

/* Export HTML control data */
typedef struct _ExportHtmlCtl ExportHtmlCtl;
struct _ExportHtmlCtl {
	gchar    *path;
	gchar    *dirOutput;
	gchar    *fileHtml;
	gchar    *encoding;
	gchar    *settingsFile;
	gint     stylesheet;
	gint     nameFormat;
	gboolean banding;
	gboolean linkEMail;
	gboolean showAttribs;
	gint     retVal;
	gint     rcCreate;
	GList    *listStyle;
};

/* Function prototypes */
ExportHtmlCtl *exporthtml_create( void );
void exporthtml_free		( ExportHtmlCtl *ctl );
void exporthtml_set_stylesheet	( ExportHtmlCtl *ctl,
				  const gint value );
void exporthtml_set_name_format	( ExportHtmlCtl *ctl,
				  const gint value );
void exporthtml_set_banding	( ExportHtmlCtl *ctl,
				  const gboolean value );
void exporthtml_set_link_email	( ExportHtmlCtl *ctl,
				  const gboolean value );
void exporthtml_set_attributes	( ExportHtmlCtl *ctl,
				  const gboolean value );
void exporthtml_process		( ExportHtmlCtl *ctl,
				  AddressCache *cache );
gboolean exporthtml_test_dir	( ExportHtmlCtl *ctl );
gboolean exporthtml_create_dir	( ExportHtmlCtl *ctl );
gchar *exporthtml_get_create_msg( ExportHtmlCtl *ctl );

void exporthtml_parse_filespec	( ExportHtmlCtl *ctl,
				  gchar *fileSpec );
void exporthtml_load_settings	( ExportHtmlCtl *ctl );
void exporthtml_save_settings	( ExportHtmlCtl *ctl );

#endif /* __EXPORT_HTML_H__ */

