/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __PROCMIME_H__
#define __PROCMIME_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include "utils.h"
#include "proctypes.h"
typedef enum
{
	ENC_7BIT,
	ENC_8BIT,
	ENC_BINARY,
	ENC_QUOTED_PRINTABLE,
	ENC_BASE64,
	ENC_X_UUENCODE,
	ENC_UNKNOWN
} EncodingType;

typedef enum
{
	MIMETYPE_TEXT,
	MIMETYPE_IMAGE,
	MIMETYPE_AUDIO,
	MIMETYPE_VIDEO,
	MIMETYPE_APPLICATION,
	MIMETYPE_MESSAGE,
	MIMETYPE_MULTIPART,
	MIMETYPE_UNKNOWN
} MimeMediaType;

typedef enum
{
	DISPOSITIONTYPE_INLINE,
	DISPOSITIONTYPE_ATTACHMENT,
	DISPOSITIONTYPE_UNKNOWN
} DispositionType;

typedef enum
{
	MIMECONTENT_EMPTY,
	MIMECONTENT_FILE,		/* the file contains all content including sub parts */
	MIMECONTENT_MEM
} MimeContent;

#include <glib.h>
#include <stdio.h>

struct _PrivacyData;

struct _MimeType
{
	gchar *type;
	gchar *sub_type;

	gchar *extension;
};

struct _MimeParser
{
	MimeMediaType type;
	const gchar *sub_type;

	gboolean (*parse)(MimeParser *parser, MimeInfo *mimeinfo);
};

/*
 * An example of MimeInfo structure:
 *
 * 1: +- message/rfc822			(root)
 *       |
 * 2:    +- multipart/mixed		(children of 1)
 *          |
 * 3:       +- multipart/alternative	(children of 2)
 *          |  |
 * 4:       |  +- text/plain		(children of 3)
 *          |  |
 * 5:       |  +- text/html		(next of 4)
 *          |
 * 6:       +- message/rfc822		(next of 3)
 *          |   |
 * 7:       |   ...			(children of 6)
 *          |
 * 8:       +- image/jpeg		(next of 6)
 */

struct _MimeInfo
{
	/* Internal data */
	MimeContent content;
	union
	{
		gchar *filename;
		gchar *mem;
	} data;
	gboolean tmp;

	GNode *node;

	/* --- NEW MIME STUFF --- */
	/* Content-Type */
	MimeMediaType 	 type;
	gchar		*subtype;

	GHashTable	*typeparameters;

	/* Content-Transfer-Encoding */
	EncodingType	 encoding_type;

	/* Content-Description */
	gchar		*description;

	/* Content-ID */
	gchar		*id;

	/* Content-Location */
	gchar 		*location;

	guint		 offset;
	guint		 length;

	/* Content-Disposition */
	DispositionType	 disposition;
	GHashTable	*dispositionparameters;

	/* Privacy */
	struct _PrivacyData	*privacy;

	gboolean	 broken;
};

#define IS_BOUNDARY(s, bnd, len) \
	(bnd && s[0] == '-' && s[1] == '-' && !strncmp(s + 2, bnd, len))

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* MimeInfo handling */

MimeInfo *procmime_mimeinfo_new		(void);
void procmime_mimeinfo_free_all		(MimeInfo	*mimeinfo);

MimeInfo *procmime_mimeinfo_insert	(MimeInfo	*parent,
					 MimeInfo	*mimeinfo);
void procmime_mimeinfo_replace		(MimeInfo	*old_mimeinfo,
					 MimeInfo	*new_mimeinfo);

MimeInfo *procmime_mimeinfo_parent	(MimeInfo	*mimeinfo);
MimeInfo *procmime_mimeinfo_next	(MimeInfo	*mimeinfo);

MimeInfo *procmime_scan_message		(MsgInfo	*msginfo);
MimeInfo *procmime_scan_message_short	(MsgInfo	*msginfo);
void procmime_scan_multipart_message	(MimeInfo	*mimeinfo,
					 FILE		*fp);
const gchar *procmime_mimeinfo_get_parameter
					(MimeInfo	*mimeinfo,
					 const gchar	*name);

/* scan headers */

void procmime_scan_encoding		(MimeInfo	*mimeinfo,
					 const gchar	*encoding);
void procmime_scan_content_type		(MimeInfo	*mimeinfo,
					 const gchar	*content_type);
void procmime_scan_content_disposition	(MimeInfo	*mimeinfo,
					 const gchar	*content_disposition);
void procmime_scan_content_description	(MimeInfo	*mimeinfo,
					 const gchar	*content_description);
void procmime_scan_subject              (MimeInfo       *mimeinfo,
			                 const gchar    *subject);
MimeInfo *procmime_scan_mime_header	(FILE		*fp);

gboolean procmime_decode_content	(MimeInfo	*mimeinfo);
gboolean procmime_encode_content	(MimeInfo	*mimeinfo, EncodingType encoding);
gint procmime_get_part			(const gchar	*outfile,
					 MimeInfo	*mimeinfo);
FILE *procmime_get_first_text_content	(MsgInfo	*msginfo);
FILE *procmime_get_first_encrypted_text_content
					(MsgInfo 	*msginfo);

gchar *procmime_get_tmp_file_name	(MimeInfo	*mimeinfo);
gchar *procmime_get_part_file_name	(MimeInfo 	*mimeinfo);

gchar *procmime_get_mime_type		(const gchar	*filename);

GList *procmime_get_mime_type_list	(void);

EncodingType procmime_get_encoding_for_charset	(const gchar	*charset);
EncodingType procmime_get_encoding_for_text_file(const gchar	*file,
						 gboolean	*has_binary);
const gchar *procmime_get_encoding_str		(EncodingType	 encoding);
MimeInfo *procmime_scan_file			(const gchar	*filename);
MimeInfo *procmime_scan_queue_file		(const gchar 	*filename);
const gchar *procmime_get_media_type_str	(MimeMediaType 	 type);
MimeMediaType procmime_get_media_type		(const gchar 	*str);
gchar *procmime_get_content_type_str		(MimeMediaType   type,
						 const gchar	*subtype);
void procmime_force_charset			(const gchar 	*str);
void procmime_force_encoding			(EncodingType	 encoding);
gboolean procmime_msginfo_is_encrypted		(MsgInfo 	*msginfo);
int procmime_write_mime_header			(MimeInfo 	*mimeinfo, 
						 FILE 		*fp);
void renderer_read_config(void);

gint procmime_write_mimeinfo(MimeInfo *mimeinfo, FILE *fp);

void procmime_mimeparser_register(MimeParser *mimeparser);
void procmime_mimeparser_unregister(MimeParser *mimeparser);
FILE *procmime_get_text_content(MimeInfo *mimeinfo);
FILE *procmime_get_binary_content(MimeInfo *mimeinfo);

/* scans mimeinfo contents, calling scan_callback() once per line.
 * return TRUE and scan is aborted if scan_callback returns TRUE.
 * return TRUE on error.
 * return FALSE if scan completed and scan_callback never returned TRUE.
 */
gboolean procmime_scan_text_content(MimeInfo *mimeinfo,
		gboolean (*scan_callback)(const gchar *str, gpointer cb_data),
		gpointer cb_data);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __PROCMIME_H__ */
