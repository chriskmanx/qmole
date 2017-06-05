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
 * Export address book to HTML file.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_PTHREAD
#include <pthread.h>
#endif

#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <glib.h>
#include <glib/gi18n.h>

#ifdef G_OS_WIN32
#  include <w32lib.h>
#endif

#include "mgutils.h"
#include "utils.h"
#include "exporthtml.h"
#include "xmlprops.h"

#ifdef MKDIR_TAKES_ONE_ARG
#undef mkdir
#define mkdir(a,b) mkdir(a)
#endif

#define DFL_DIR_CLAWS_OUT  "claws-mail-out"
#define DFL_FILE_CLAWS_OUT "addressbook.html"

#define FMT_BUFSIZE         2048
#define SC_HTML_SPACE          "&nbsp;"
#define BORDER_SIZE         2
#define CELL_PADDING        2
#define CELL_SPACING        2
#define CHAR_ENCODING       "UTF-8"

/* Stylesheet names */
#define FILENAME_NONE       ""
#define FILENAME_DEFAULT    "claws-mail.css"
#define FILENAME_FULL       "full.css"
#define FILENAME_CUSTOM     "custom.css"
#define FILENAME_CUSTOM2    "custom2.css"
#define FILENAME_CUSTOM3    "custom3.css"
#define FILENAME_CUSTOM4    "custom4.css"

/* Settings - properties */
#define EXML_PROPFILE_NAME  "exporthtml.xml"
#define EXMLPROP_DIRECTORY  "directory"
#define EXMLPROP_FILE       "file"
#define EXMLPROP_STYLESHEET "stylesheet"
#define EXMLPROP_FMT_NAME   "format-full-name"
#define EXMLPROP_FMT_EMAIL  "format-email-links"
#define EXMLPROP_FMT_ATTRIB "format-attributes"
#define EXMLPROP_BANDING    "color-banding"
#define EXMLPROP_VALUE_YES  "y"
#define EXMLPROP_VALUE_NO   "n"

static gchar *_idTagRowEven_ = "tab-row0";
static gchar *_idTagRowOdd_  = "tab-row1";

/*
 * Header entry.
 */
typedef struct _StylesheetEntry StylesheetEntry;
struct _StylesheetEntry {
	gchar    *fileName;
	gint     id;
	gboolean dflValue;
};

/*
 * Build stylesheet entry.
 * Enter: ctl   Export control data.
 *        file  Filename.
 *        id    File id.
 *        dfl   Default flag.
 */
static void exporthtml_build_entry(
		ExportHtmlCtl *ctl, const gchar *file, const gint id,
		const gboolean dfl )
{
	StylesheetEntry *entry;

	entry = g_new0( StylesheetEntry, 1 );
	entry->fileName = g_strdup( file );
	entry->id = id;
	entry->dflValue = dfl;
	ctl->listStyle = g_list_append( ctl->listStyle, entry );
}

/*
 * Free up object by releasing internal memory.
 * Enter: ctl Export control data.
 */
ExportHtmlCtl *exporthtml_create( void ) {
	ExportHtmlCtl *ctl = g_new0( ExportHtmlCtl, 1 );

	ctl->path = NULL;
	ctl->dirOutput = NULL;
	ctl->fileHtml = NULL;
	ctl->encoding = g_strconcat(CHAR_ENCODING, NULL);
	ctl->stylesheet = EXPORT_HTML_ID_NONE;
	ctl->nameFormat = EXPORT_HTML_FIRST_LAST;
	ctl->banding = FALSE;
	ctl->linkEMail = FALSE;
	ctl->showAttribs = FALSE;
	ctl->retVal = MGU_SUCCESS;
	ctl->listStyle = NULL;
	ctl->rcCreate = 0;
	ctl->settingsFile = g_strconcat(
		get_rc_dir(), G_DIR_SEPARATOR_S, EXML_PROPFILE_NAME, NULL );

	/* Build stylesheet list */
	exporthtml_build_entry(
		ctl, FILENAME_NONE,    EXPORT_HTML_ID_NONE, FALSE );
	exporthtml_build_entry(
		ctl, FILENAME_DEFAULT, EXPORT_HTML_ID_DEFAULT, TRUE );
	exporthtml_build_entry(
		ctl, FILENAME_FULL,    EXPORT_HTML_ID_FULL, FALSE );
	exporthtml_build_entry(
		ctl, FILENAME_CUSTOM,  EXPORT_HTML_ID_CUSTOM, FALSE );
	exporthtml_build_entry(
		ctl, FILENAME_CUSTOM2, EXPORT_HTML_ID_CUSTOM2, FALSE );
	exporthtml_build_entry(
		ctl, FILENAME_CUSTOM3, EXPORT_HTML_ID_CUSTOM3, FALSE );
	exporthtml_build_entry(
		ctl, FILENAME_CUSTOM4, EXPORT_HTML_ID_CUSTOM4, FALSE );

	return ctl;
}

/*
 * Free up object by releasing internal memory.
 * Enter: ctl Export control data.
 */
void exporthtml_free( ExportHtmlCtl *ctl ) {
	GList *node;
	StylesheetEntry *entry;

	cm_return_if_fail( ctl != NULL );

	/* Free stylesheet list */
	node = ctl->listStyle;
	while( node ) {
		entry = ( StylesheetEntry * ) node->data;
		g_free( entry->fileName );
		entry->fileName = NULL;
		entry->id = 0;
		entry->dflValue = FALSE;
		g_free( entry );
		node->data = NULL;
		node = g_list_next( node );
	}
	g_list_free( ctl->listStyle );
	ctl->listStyle = NULL;

	g_free( ctl->path );
	g_free( ctl->fileHtml );
	g_free( ctl->encoding );
	g_free( ctl->dirOutput );
	g_free( ctl->settingsFile );

	/* Clear pointers */
	ctl->path = NULL;
	ctl->dirOutput = NULL;
	ctl->fileHtml = NULL;
	ctl->encoding = NULL;
	ctl->stylesheet = EXPORT_HTML_ID_NONE;
	ctl->nameFormat = EXPORT_HTML_FIRST_LAST;
	ctl->banding = FALSE;
	ctl->linkEMail = FALSE;
	ctl->showAttribs = FALSE;
	ctl->retVal = MGU_SUCCESS;
	ctl->rcCreate = 0;

	/* Now release object */
	g_free( ctl );
}

/*
 * Find style entry.
 * Enter: ctl Export control data.
 * Return: Stylesheet object, or NULL if nothing found. If a default entry is
 * found in list, it will be returned.
 */
static StylesheetEntry *exporthtml_find_stylesheet( ExportHtmlCtl *ctl ) {
	StylesheetEntry *retVal = NULL;
	StylesheetEntry *entry;
	GList *node;

	node = ctl->listStyle;
	while( node ) {
		entry = ( StylesheetEntry * ) node->data;
		if( entry->id == ctl->stylesheet ) return entry;
		if( entry->dflValue ) retVal = entry;
		node = g_list_next( node );
	}
	return retVal;
}

void exporthtml_set_stylesheet( ExportHtmlCtl *ctl, const gint value ) {
	cm_return_if_fail( ctl != NULL );
	ctl->stylesheet = value;
}
void exporthtml_set_name_format( ExportHtmlCtl *ctl, const gint value ) {
	cm_return_if_fail( ctl != NULL );
	ctl->nameFormat = value;
}
void exporthtml_set_banding( ExportHtmlCtl *ctl, const gboolean value ) {
	cm_return_if_fail( ctl != NULL );
	ctl->banding = value;
}
void exporthtml_set_link_email( ExportHtmlCtl *ctl, const gboolean value ) {
	cm_return_if_fail( ctl != NULL );
	ctl->linkEMail = value;
}
void exporthtml_set_attributes( ExportHtmlCtl *ctl, const gboolean value ) {
	cm_return_if_fail( ctl != NULL );
	ctl->showAttribs = value;
}

/*
 * Create default CSS file.
 * Enter:  fileSpec File to create.
 * Return: Status code.
 */
static gint exporthtml_create_css_dfl( const gchar *fileSpec ) {
	FILE *cssFile;

	cssFile = g_fopen( fileSpec, "rb" );
	if( cssFile ) {
		fclose( cssFile );
		return MGU_SUCCESS;
	}
	cssFile = g_fopen( fileSpec, "wb" );
	if( ! cssFile ) {
		return MGU_OPEN_FILE;
	}

	fprintf( cssFile, "body {\n\tbackground: #ffffe0;\n" );
	fprintf( cssFile, "\tfont-family: lucida, helvetica, sans-serif;\n" );
	fprintf( cssFile, "\tfont-size: 10pt;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, "h1 {\n" );
	fprintf( cssFile, "\tcolor: #000000;\n" );
	fprintf( cssFile, "\ttext-align: center;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, "th {\n" );
	fprintf( cssFile, "\tfont-size: 10pt;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, "td {\n" );
	fprintf( cssFile, "\tfont-size: 10pt;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".fmt-folder {\n" );
	fprintf( cssFile, "\tcolor: #0000ff;\n" );
	fprintf( cssFile, "\tfont-size: 18pt;\n" );
	fprintf( cssFile, "\tfont-weight: bold;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-head {\n" );
	fprintf( cssFile, "\tbackground: #80c0f0;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-dn {\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-addr {\n" );
	fprintf( cssFile, "\tfont-style: italic;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-email {\n" );
	fprintf( cssFile, "\tfont-weight: bold;\n" );
	fprintf( cssFile, "\tfont-style: italic;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-fn {\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-attr {\n" );
	fprintf( cssFile, "}\n" );

	fclose( cssFile );
	return MGU_SUCCESS;
}

/*
 * Create full CSS file.
 * Enter:  fileSpec File to create.
 * Return: Status code.
 */
static gint exporthtml_create_css_full( const gchar *fileSpec ) {
	FILE *cssFile;

	cssFile = g_fopen( fileSpec, "rb" );
	if( cssFile ) {
		fclose( cssFile );
		return MGU_SUCCESS;
	}
	cssFile = g_fopen( fileSpec, "wb" );
	if( ! cssFile ) {
		return MGU_OPEN_FILE;
	}

	fprintf( cssFile, "body {\n\tbackground: #ffffe0;\n" );
	fprintf( cssFile, "\tfont-family: lucida, helvetica, sans-serif;\n" );
	fprintf( cssFile, "\tfont-size: 10pt;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, "h1 {\n" );
	fprintf( cssFile, "\tcolor: #000000;\n" );
	fprintf( cssFile, "\ttext-align: center;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, "th {\n" );
	fprintf( cssFile, "\tfont-size: 10pt;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, "td {\n" );
	fprintf( cssFile, "\tfont-size: 10pt;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".fmt-folder {\n" );
	fprintf( cssFile, "\tcolor: #0000ff;\n" );
	fprintf( cssFile, "\tfont-size: 18pt;\n" );
	fprintf( cssFile, "\tfont-weight: bold;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-head {\n" );
	fprintf( cssFile, "\tbackground: #80c0f0;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-row0 {\n" );
	fprintf( cssFile, "\tbackground: #f0f0f0;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-row1 {\n" );
	fprintf( cssFile, "\tbackground: #d0d0d0;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-dn {\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-addr {\n" );
	fprintf( cssFile, "\tfont-style: italic;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-email {\n" );
	fprintf( cssFile, "\tfont-weight: bold;\n" );
	fprintf( cssFile, "\tfont-style: italic;\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-fn {\n" );
	fprintf( cssFile, "}\n" );
	fprintf( cssFile, ".tab-attr {\n" );
	fprintf( cssFile, "}\n" );

	fclose( cssFile );
	return MGU_SUCCESS;
}

/*
 * Create stylesheet files.
 * Enter:  ctl  Export control data.
 */
static void exporthtml_create_css_files( ExportHtmlCtl *ctl ) {
	gchar *fileSpec;
	GList *node;

	node = ctl->listStyle;
	while( node ) {
		StylesheetEntry *entry = node->data;
		node = g_list_next( node );
		if( strlen( entry->fileName ) ) {
			fileSpec = g_strconcat(
					ctl->dirOutput, G_DIR_SEPARATOR_S, 
					entry->fileName, NULL );
			if( entry->id == EXPORT_HTML_ID_DEFAULT ) {
				exporthtml_create_css_dfl( fileSpec );
			}
			else if( entry->id != EXPORT_HTML_ID_NONE ) {
				exporthtml_create_css_full( fileSpec );
			}
			g_free( fileSpec );
		}
	}
}

/*
 * Comparison using linked list elements.
 */
static gint exporthtml_compare_name(
	gconstpointer ptr1, gconstpointer ptr2 )
{
	const AddrItemObject *item1 = ptr1;
	const AddrItemObject *item2 = ptr2;
	const gchar *name1 = NULL, *name2 = NULL;
	if( item1 ) name1 = ADDRITEM_NAME( item1 );
	if( item2 ) name2 = ADDRITEM_NAME( item2 );
	if( ! name1 ) return ( name2 != NULL );
	if( ! name2 ) return -1;
	return g_utf8_collate( name1, name2 );
}

/*
 * Comparison using linked list elements.
 */
static gint exporthtml_compare_email(
	gconstpointer ptr1, gconstpointer ptr2 )
{
	const ItemEMail *email1 = ptr1;
	const ItemEMail *email2 = ptr2;
	const gchar *name1 = NULL, *name2 = NULL;
	if( email1 ) name1 = email1->address;
	if( email2 ) name2 = email2->address;
	if( ! name1 ) return ( name2 != NULL );
	if( ! name2 ) return -1;
	return g_utf8_collate( name1, name2 );
}

/*
 * Comparison using linked list elements.
 */
static gint exporthtml_compare_attrib(
	gconstpointer ptr1, gconstpointer ptr2 )
{
	const UserAttribute *attr1 = ptr1;
	const UserAttribute *attr2 = ptr2;
	const gchar *name1 = NULL, *name2 = NULL;
	if( attr1 ) name1 = attr1->name;
	if( attr2 ) name2 = attr2->name;
	if( ! name1 ) return ( name2 != NULL );
	if( ! name2 ) return -1;
	return g_utf8_collate( name1, name2 );
}

/*
 * Build sorted list of named items.
 * Enter:  list  List of items to sorted.
 * Return: Sorted list.
 * Note: List should freed after use. Items referenced by list should not be
 * freed since they are managed by the address cache.
 */
static GList *exporthtml_sort_name( const GList *list ) {
	const GList *node;
	GList *sorted = NULL;

	node = list;
	while( node ) {
		sorted = g_list_insert_sorted(
				sorted, node->data, exporthtml_compare_name );
		node = g_list_next( node );
	}
	return sorted;
}

/*
 * Build sorted list of email items.
 * Enter:  list  List of E-Mail items to sorted.
 * Return: Sorted list.
 * Note: List should freed after use. Items referenced by list should not be
 * freed since they are managed by the address cache.
 */
static GList *exporthtml_sort_email( const GList *list ) {
	const GList *node;
	GList *sorted = NULL;

	node = list;
	while( node ) {
		sorted = g_list_insert_sorted(
				sorted, node->data, exporthtml_compare_email );
		node = g_list_next( node );
	}
	return sorted;
}

/*
 * Build sorted list of attributes.
 * Enter:  list  List of items to sorted.
 * Return: Sorted list.
 * Note: List should freed after use. Items referenced by list should not be
 * freed since they are managed by the address cache.
 */
static GList *exporthtml_sort_attrib( const GList *list ) {
	const GList *node;
	GList *sorted = NULL;

	sorted = NULL;
	node = list;
	while( node ) {
		sorted = g_list_insert_sorted(
				sorted, node->data, exporthtml_compare_attrib );
		node = g_list_next( node );
	}
	return sorted;
}

/*
 * Format a list of E-Mail addresses.
 * Enter: ctl       Export control data.
 *        stream    Output stream.
 *        listEMail List of addresses.
 *        sortFlag  Set to TRUE if address list should be sorted.
 */
static void exporthtml_fmt_email(
		ExportHtmlCtl *ctl, FILE *stream, const GList *listEMail,
		gboolean sortFlag )
{
	const GList *node;
	GList *list;
	gchar *name;

	if( listEMail == NULL ) {
		fprintf( stream, SC_HTML_SPACE );
		return;
	}

	list = NULL;
	if( sortFlag ) {
		node = list = exporthtml_sort_email( listEMail );
	}
	else {
		node = listEMail;
	}

	while( node ) {
		ItemEMail *email = ( ItemEMail * ) node->data;
		node = g_list_next( node );

		name = ADDRITEM_NAME( email );
		if( name ) {
			fprintf( stream, "%s ", name );
		}
		if( ctl->linkEMail ) {
			fprintf( stream, "<a href=\"mailto:%s\">",
				email->address );
		}
		fprintf( stream, "<span class=\"tab-email\">" );
		fprintf( stream, "%s", email->address );
		fprintf( stream, "</span>" );
		if( ctl->linkEMail ) {
			fprintf( stream, "</a>" );
		}
		if( email->remarks ) {
			if( strlen( email->remarks ) ) {
				fprintf( stream, " (%s)", email->remarks );
			}
		}
		fprintf( stream, "<br>\n" );
	}
	g_list_free( list );
}

/*
 * Format groups in an address book folder.
 * Enter:  ctl      Export control data.
 *         stream   Output stream.
 *         folder   Folder.
 *         prevFlag If FALSE, list of persons were output.
 * Return: TRUE if no groups were formatted.
 */
static gboolean exporthtml_fmt_group(
		ExportHtmlCtl *ctl, FILE *stream, const ItemFolder *folder,
		gboolean prevFlag )
{
	gboolean retVal, band;
	GList *node, *list;
	const gchar *tagName;

	retVal = TRUE;
	if( folder->listGroup == NULL ) return retVal;

	/* Write separator */
	if( ! prevFlag ) {
		fprintf( stream, "<br>\n" );
	}

	/* Write table headers */
	fprintf( stream, "<table" );
	fprintf( stream, " border=\"%d\"", BORDER_SIZE );
	fprintf( stream, " cellpadding=\"%d\"", CELL_PADDING );
	fprintf( stream, " cellspacing=\"%d\"", CELL_SPACING );
	fprintf( stream, ">\n" );

	fprintf( stream, "<tr class=\"tab-head\">\n" );
	fprintf( stream, "  <th width=\"200\">" );
	fprintf( stream, "%s", _( "Group Name" ) );
	fprintf( stream, "</th>\n" );
	fprintf( stream, "  <th width=\"300\">" );
	fprintf( stream, "%s", _( "Email Address" ) );
	fprintf( stream, "</th>\n" );
	fprintf( stream, "</tr>\n" );
	list = exporthtml_sort_name( folder->listGroup );

	band = FALSE;
	node = list;
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio && aio->type == ITEMTYPE_GROUP ) {
			ItemGroup *group = ( ItemGroup * ) aio;

			fprintf( stream, "<tr valign=\"top\"" );
			if( ctl->banding ) {
				if( band ) {
					tagName = _idTagRowOdd_;
				}
				else {
					tagName = _idTagRowEven_;
				}
				fprintf( stream, " class=\"%s\"", tagName );
				band = ! band;
			}
			fprintf( stream, "\">\n" );

			fprintf( stream, "  <td class=\"tab-dn\">" );
			fprintf( stream, "%s", ADDRITEM_NAME( group ) );
			fprintf( stream, "</td>\n" );
			fprintf( stream, "  <td class=\"tab-addr\">" );
			exporthtml_fmt_email( ctl, stream, group->listEMail, TRUE );
			fprintf( stream, "</td>\n" );
			fprintf( stream, "</tr>\n" );
			retVal = FALSE;
		}
		node = g_list_next( node );
	}

	g_list_free( list );
	fprintf( stream, "</table>\n" );
	return retVal;
}

/*
 * Format a list of E-Mail addresses.
 * Enter: ctl       Export control data.
 *        stream    Output stream.
 *        listAttr  List of attributes.
 */
static void exporthtml_fmt_attribs(
		ExportHtmlCtl *ctl, FILE *stream, const GList *listAttr )
{
	const GList *node;
	GList *list;

	if( listAttr == NULL ) {
		fprintf( stream, SC_HTML_SPACE );
		return;
	}

	fprintf( stream, "<table border=\"0\">\n" );
	node = list = exporthtml_sort_attrib( listAttr );
	while( node ) {
		UserAttribute *attr = ( UserAttribute * ) node->data;
		node = g_list_next( node );
		fprintf( stream, "<tr valign=\"top\">" );
		fprintf( stream, "<td align=\"right\">%s:</td>", attr->name );
		fprintf( stream, "<td>%s</td>", attr->value );
		fprintf( stream, "</tr>\n" );
	}

	g_list_free( list );
	fprintf( stream, "</table>" );
}

/*
 * Format full name.
 * Enter:  ctl     Export control data.
 *         buf     Output buffer.
 *         person  Person to format.
 */
static void exporthtml_fmt_fullname(
		ExportHtmlCtl *ctl, gchar *buf, const ItemPerson *person )
{
	gboolean flag;

	if( ctl->nameFormat == EXPORT_HTML_LAST_FIRST ) {
		flag = FALSE;
		if( person->lastName ) {
			if( *person->lastName ) {
				strcat( buf, " " );
				strcat( buf, person->lastName );
				flag = TRUE;
			}
		}
		if( person->firstName ) {
			if( *person->firstName ) {
				if( flag ) {
					strcat( buf, ", " );
				}
				strcat( buf, person->firstName );
			}
		}
	}
	else {
		if( person->firstName ) {
			if( *person->firstName ) {
				strcat( buf, person->firstName );
			}
		}
		if( person->lastName ) {
			if( *person->lastName ) {
				strcat( buf, " " );
				strcat( buf, person->lastName );
			}
		}
	}
	g_strstrip( buf );

	flag = FALSE;
	if( *buf ) flag = TRUE;
	if( person->nickName ) {
		if( strlen( person->nickName ) ) {
			if( flag ) {
				strcat( buf, " (" );
			}
			strcat( buf, person->nickName );
			if( flag ) {
				strcat( buf, ")" );
			}
		}
	}
	g_strstrip( buf );
}

/*
 * Format persons in an address book folder.
 * Enter:  ctl     Export control data.
 *         stream  Output stream.
 *         folder  Folder.
 * Return: TRUE if no persons were formatted.
 */
static gboolean exporthtml_fmt_person(
		ExportHtmlCtl *ctl, FILE *stream, const ItemFolder *folder )
{
	gboolean retVal, band;
	GList *node, *list;
	gchar buf[ FMT_BUFSIZE ];
	const gchar *tagName;

	retVal = TRUE;
	if( folder->listPerson == NULL ) return retVal;

	/* Write table headers */
	fprintf( stream, "<table" );
	fprintf( stream, " border=\"%d\"", BORDER_SIZE );
	fprintf( stream, " cellpadding=\"%d\"", CELL_PADDING );
	fprintf( stream, " cellspacing=\"%d\"", CELL_SPACING );
	fprintf( stream, ">\n" );

	fprintf( stream, "<tr class=\"tab-head\">\n" );
	fprintf( stream, "  <th width=\"200\">" );
	fprintf( stream, "%s", _( "Display Name" ) );
	fprintf( stream, "</th>\n" );
	fprintf( stream, "  <th width=\"300\">" );
	fprintf( stream, "%s", _( "Email Address" ) );
	fprintf( stream, "</th>\n" );
	fprintf( stream, "  <th width=\"200\">" );
	fprintf( stream, "%s", _( "Full Name" ) );
	fprintf( stream, "</th>\n" );
	if( ctl->showAttribs ) {
		fprintf( stream, "  <th width=\"250\">" );
		fprintf( stream, "%s", _( "Attributes" ) );
		fprintf( stream, "</th>\n" );
	}
	fprintf( stream, "</tr>\n" );

	band = FALSE;
	node = list = exporthtml_sort_name( folder->listPerson );
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio && aio->type == ITEMTYPE_PERSON ) {
			ItemPerson *person = ( ItemPerson * ) aio;

			/* Format first/last/nick name */
			*buf = '\0';
			exporthtml_fmt_fullname( ctl, buf,person );

			fprintf( stream, "<tr valign=\"top\"" );
			if( ctl->banding ) {
				if( band ) {
					tagName = _idTagRowOdd_;
				}
				else {
					tagName = _idTagRowEven_;
				}
				fprintf( stream, " class=\"%s\"", tagName );
				band = ! band;
			}
			fprintf( stream, ">\n" );

			fprintf( stream, "  <td class=\"tab-dn\">" );
			fprintf( stream, "%s", ADDRITEM_NAME( person ) );
			fprintf( stream, "</td>\n" );

			fprintf( stream, "  <td class=\"tab-addr\">" );
			exporthtml_fmt_email( ctl, stream, person->listEMail, FALSE );
			fprintf( stream, "</td>\n" );

			fprintf( stream, "  <td class=\"tab-fn\">" );
			if( *buf ) {
				fprintf( stream, "%s", buf );
			}
			else {
				fprintf( stream, "%s", SC_HTML_SPACE );
			}
			fprintf( stream, "</td>\n" );

			if( ctl->showAttribs ) {
				fprintf( stream, "  <td class=\"tab-attr\">" );
				exporthtml_fmt_attribs(
					ctl, stream, person->listAttrib );
				fprintf( stream, "</td>\n" );
			}
			fprintf( stream, "</tr>\n" );

			retVal = FALSE;
		}
		node = g_list_next( node );
	}

	g_list_free( list );
	fprintf( stream, "</table>\n" );
	return retVal;
}

/*
 * Format folder heirarchy.
 * Enter: stream Output stream.
 *        list   Heirarchy list.
 */
static void exporthtml_fmt_folderhead( FILE *stream, const GList *list ) {
	const GList *node;
	gboolean flag;
	gchar *name;

	flag = FALSE;
	node = list;
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio && aio->type == ITEMTYPE_FOLDER ) {
			ItemFolder *folder = ( ItemFolder * ) aio;

			name = ADDRITEM_NAME( folder );
			if( name ) {
				if( flag ) {
					fprintf( stream, "&nbsp;&gt;&nbsp;" );
				}
				fprintf( stream, "%s", name );
				flag = TRUE;
			}
		}
		node = g_list_next( node );
	}
}

/*
 * Format an address book folder.
 * Enter: ctl    Export control data.
 *        stream Output stream.
 *        folder Folder.
 */
static void exporthtml_fmt_folder(
		ExportHtmlCtl *ctl, FILE *stream, const ItemFolder *folder )
{
	const GList *node;
	GList *listHeir, *list;
	const gchar *name;
	gboolean ret1;

	name = ADDRITEM_NAME( folder );
	if( name ) {
		listHeir = addritem_folder_path( folder, TRUE );
		if( listHeir ) {
			fprintf( stream, "<p class=\"fmt-folder\">" );
			fprintf( stream, "%s: ", _( "Folder" ) );
			exporthtml_fmt_folderhead( stream, listHeir );
			fprintf( stream, "</p>\n" );
			g_list_free( listHeir );
		}
	}

	ret1 = exporthtml_fmt_person( ctl, stream, folder );
	exporthtml_fmt_group( ctl, stream, folder, ret1 );

	node = list = exporthtml_sort_name( folder->listFolder );
	while( node ) {
		AddrItemObject *aio = node->data;
		if( aio && aio->type == ITEMTYPE_FOLDER ) {
			ItemFolder *subFolder = ( ItemFolder * ) aio;
			exporthtml_fmt_folder( ctl, stream, subFolder );
		}
		node = g_list_next( node );
	}
	if( list ) {
		g_list_free( list );
	}
}

/*
 * Format header block.
 * Enter:  ctl    Export control data.
 *         stream Output stream.
 *         title  Page title.
 */
static void exporthtml_fmt_header(
		ExportHtmlCtl *ctl, FILE *stream, gchar *title )
{
	StylesheetEntry *entry;

	entry = exporthtml_find_stylesheet( ctl );

	fprintf( stream,
		"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n" );
	fprintf( stream,
                "\"http://www.w3.org/TR/html4/loose.dtd\">\n" );
	fprintf( stream, "<html>\n" );
	fprintf( stream, "<head>\n" );

	if( ctl->encoding && strlen( ctl->encoding ) > 0 ) {
		fprintf( stream, "<meta " );
		fprintf( stream, "http-equiv=\"Content-Type\" " );
		fprintf( stream, "content=\"text/html; charset=%s\">\n",
			ctl->encoding );
	}

	fprintf( stream, "<title>%s</title>\n", title );

	if( entry != NULL ) {
		if( entry->fileName && strlen( entry->fileName ) > 0 ) {
			fprintf( stream, "<link " );
			fprintf( stream, "rel=\"stylesheet\" " );
			fprintf( stream, "type=\"text/css\" " );
			fprintf( stream, "href=\"%s\" >\n", entry->fileName );
		}
	}
	fprintf( stream, "</head>\n" );
}

/*
 * ============================================================================
 * Export address book to HTML file.
 * Enter:  ctl   Export control data.
 *         cache Address book/data source cache.
 * Return: Status.
 * ============================================================================
 */
void exporthtml_process(
	ExportHtmlCtl *ctl, AddressCache *cache )
{
	ItemFolder *rootFolder;
	FILE *htmlFile;
	time_t tt;
	gchar *dsName;
	static gchar *title;
	gchar buf[512];

	htmlFile = g_fopen( ctl->path, "wb" );
	if( ! htmlFile ) {
		/* Cannot open file */
		g_print( "Cannot open file for write\n" );
		ctl->retVal = MGU_OPEN_FILE;
		return;
	}

	title = _( "Claws Mail Address Book" );
	rootFolder = cache->rootFolder;
	dsName = cache->name;

	exporthtml_fmt_header( ctl, htmlFile, title );

	fprintf( htmlFile, "<body>\n" );
	fprintf( htmlFile, "<h1>%s</h1>\n", title );

	fprintf( htmlFile, "<p class=\"fmt-folder\">" );
	fprintf( htmlFile, "%s: ", _( "Address Book" ) );
	fprintf( htmlFile, "%s", dsName );
	fprintf( htmlFile, "</p>\n" );

	exporthtml_fmt_folder( ctl, htmlFile, rootFolder );

	tt = time( NULL );
	fprintf( htmlFile, "<p>%s</p>\n", ctime_r( &tt, buf ) );
	fprintf( htmlFile, "<hr width=\"100%%\">\n" );

	fprintf( htmlFile, "</body>\n" );
	fprintf( htmlFile, "</html>\n" );

	fclose( htmlFile );
	ctl->retVal = MGU_SUCCESS;

	/* Create stylesheet files */
	exporthtml_create_css_files( ctl );

}

/*
 * Build full export file specification.
 * Enter:  ctl  Export control data.
 */
static void exporthtml_build_filespec( ExportHtmlCtl *ctl ) {
	gchar *fileSpec;

	fileSpec = g_strconcat(
		ctl->dirOutput, G_DIR_SEPARATOR_S, ctl->fileHtml, NULL );
	ctl->path = mgu_replace_string( ctl->path, fileSpec );
	g_free( fileSpec );
}

/*
 * ============================================================================
 * Parse directory and filename from full export file specification.
 * Enter:  ctl      Export control data.
 *         fileSpec File spec.
 * ============================================================================
 */
void exporthtml_parse_filespec( ExportHtmlCtl *ctl, gchar *fileSpec ) {
	gchar *t;
	gchar *base = g_path_get_basename(fileSpec);

	ctl->fileHtml =
		mgu_replace_string( ctl->fileHtml, base );
	g_free(base);
	t = g_path_get_dirname( fileSpec );
	ctl->dirOutput = mgu_replace_string( ctl->dirOutput, t );
	g_free( t );
	ctl->path = mgu_replace_string( ctl->path, fileSpec );
}

/*
 * ============================================================================
 * Test whether directory exists.
 * Enter:  ctl  Export control data.
 * Return: TRUE if exists.
 * ============================================================================
 */
gboolean exporthtml_test_dir( ExportHtmlCtl *ctl ) {
	gboolean retVal;
	DIR *dp;

	retVal = FALSE;
	if((dp = opendir( ctl->dirOutput )) != NULL) {
		retVal = TRUE;
		closedir( dp );
	}
	return retVal;
}

/*
 * ============================================================================
 * Create output directory.
 * Enter:  ctl  Export control data.
 * Return: TRUE if directory created.
 * ============================================================================
 */
gboolean exporthtml_create_dir( ExportHtmlCtl *ctl ) {
	gboolean retVal = FALSE;

	ctl->rcCreate = 0;
	if( mkdir( ctl->dirOutput, S_IRWXU ) == 0 ) {
		retVal = TRUE;
	}
	else {
		ctl->rcCreate = errno;
	}
	return retVal;
}

/*
 * ============================================================================
 * Retrieve create directory error message.
 * Enter:  ctl  Export control data.
 * Return: Message.
 * ============================================================================
 */
gchar *exporthtml_get_create_msg( ExportHtmlCtl *ctl ) {
	gchar *msg;

	if( ctl->rcCreate == EEXIST ) {
		msg = _( "Name already exists but is not a directory." );
	}
	else if( ctl->rcCreate == EACCES ) {
		msg = _( "No permissions to create directory." );
	}
	else if( ctl->rcCreate == ENAMETOOLONG ) {
		msg = _( "Name is too long." );
	}
	else {
		msg = _( "Not specified." );
	}
	return msg;
}

/*
 * Set default values.
 * Enter: ctl Export control data.
 */
static void exporthtml_default_values( ExportHtmlCtl *ctl ) {
	gchar *str;

	str = g_strconcat(
		get_home_dir(), G_DIR_SEPARATOR_S,
		DFL_DIR_CLAWS_OUT, NULL );

	ctl->dirOutput = mgu_replace_string( ctl->dirOutput, str );
	g_free( str );

	ctl->fileHtml =
		mgu_replace_string( ctl->fileHtml, DFL_FILE_CLAWS_OUT );
	ctl->encoding = NULL;
	ctl->stylesheet = EXPORT_HTML_ID_DEFAULT;
	ctl->nameFormat = EXPORT_HTML_FIRST_LAST;
	ctl->banding = TRUE;
	ctl->linkEMail = TRUE;
	ctl->showAttribs = TRUE;
	ctl->retVal = MGU_SUCCESS;
}

/*
 * ============================================================================
 * Load settings from XML properties file.
 * Enter: ctl  Export control data.
 * ============================================================================
 */
void exporthtml_load_settings( ExportHtmlCtl *ctl ) {
	XmlProperty *props;
	gint rc;
	gchar buf[256];

	*buf = '\0';
	props = xmlprops_create();
	xmlprops_set_path( props, ctl->settingsFile );
	rc = xmlprops_load_file( props );
	if( rc == 0 ) {
		/* Read settings */
		xmlprops_get_property_s( props, EXMLPROP_DIRECTORY, buf );
		ctl->dirOutput = mgu_replace_string( ctl->dirOutput, buf );

		xmlprops_get_property_s( props, EXMLPROP_FILE, buf );
		ctl->fileHtml = mgu_replace_string( ctl->fileHtml, buf );

		ctl->stylesheet =
			xmlprops_get_property_i( props, EXMLPROP_STYLESHEET );
		ctl->nameFormat =
			xmlprops_get_property_i( props, EXMLPROP_FMT_NAME );
		ctl->banding =
			xmlprops_get_property_b( props, EXMLPROP_BANDING );
		ctl->linkEMail =
			xmlprops_get_property_b( props, EXMLPROP_FMT_EMAIL );
		ctl->showAttribs =
			xmlprops_get_property_b( props, EXMLPROP_FMT_ATTRIB );
	}
	else {
		/* Set default values */
		exporthtml_default_values( ctl );
	}
	exporthtml_build_filespec( ctl );
	/* exporthtml_print( ctl, stdout ); */

	xmlprops_free( props );
}

/*
 * ============================================================================
 * Save settings to XML properties file.
 * Enter: ctl  Export control data.
 * ============================================================================
 */
void exporthtml_save_settings( ExportHtmlCtl *ctl ) {
	XmlProperty *props;

	props = xmlprops_create();
	xmlprops_set_path( props, ctl->settingsFile );

	xmlprops_set_property( props, EXMLPROP_DIRECTORY, ctl->dirOutput );
	xmlprops_set_property( props, EXMLPROP_FILE, ctl->fileHtml );
	xmlprops_set_property_i( props, EXMLPROP_STYLESHEET, ctl->stylesheet );
	xmlprops_set_property_i( props, EXMLPROP_FMT_NAME, ctl->nameFormat );
	xmlprops_set_property_b( props, EXMLPROP_BANDING, ctl->banding );
	xmlprops_set_property_b( props, EXMLPROP_FMT_EMAIL, ctl->linkEMail );
	xmlprops_set_property_b( props, EXMLPROP_FMT_ATTRIB, ctl->showAttribs );
	if (xmlprops_save_file( props ) != MGU_SUCCESS)
		g_warning("can't save settings");
	xmlprops_free( props );
}

/*
 * ============================================================================
 * End of Source.
 * ============================================================================
 */


