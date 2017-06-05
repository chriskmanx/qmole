/*
 * Geeqie
 * (C) 2005 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 *  Authors:
 *    Original version 2005 Lars Ellenberg, base on dcraw by David coffin.
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef __FORMAT_RAW_H
#define __FORMAT_RAW_H

#include "exif-int.h"


typedef enum {
	FORMAT_RAW_MATCH_MAGIC,
	FORMAT_RAW_MATCH_TIFF_MAKE
} FormatRawMatchType;

typedef enum {
	FORMAT_RAW_EXIF_NONE,
	FORMAT_RAW_EXIF_TIFF,
	FORMAT_RAW_EXIF_JPEG,
	FORMAT_RAW_EXIF_IFD_II,
	FORMAT_RAW_EXIF_IFD_MM,
	FORMAT_RAW_EXIF_PROPRIETARY
} FormatRawExifType;

typedef gboolean (* FormatRawParseFunc)(guchar *data, const guint len,
				        guint *image_offset, guint *exif_offset);

typedef gboolean (* FormatRawExifParseFunc)(guchar *data, const guint len,
					    ExifData *exif);

gboolean format_raw_img_exif_offsets(guchar *data, const guint len,
				     guint *image_offset, guint *exif_offset);
gboolean format_raw_img_exif_offsets_fd(gint fd, const gchar *path,
				        guchar *header_data, const guint header_len,
				        guint *image_offset, guint *exif_offset);

FormatRawExifType format_raw_exif_offset(guchar *data, const guint len, guint *exif_offset,
					 FormatRawExifParseFunc *exif_parse_func);


typedef enum {
	FORMAT_EXIF_MATCH_MAKE,
	FORMAT_EXIF_MATCH_MAKERNOTE
} FormatExifMatchType;

typedef gint (* FormatExifParseFunc)(ExifData *exif, guchar *tiff, guint offset,
				     guint size, ExifByteOrder bo);

gboolean format_exif_makernote_parse(ExifData *exif, guchar *tiff, guint offset,
				     guint size, ExifByteOrder bo);


#define DEBUG_RAW_TIFF 0

#if DEBUG_RAW_TIFF

#define FORMAT_RAW_DEBUG_TIFF { "", \
				FORMAT_RAW_MATCH_MAGIC,     0, "II", 2, \
				FORMAT_RAW_EXIF_NONE, NULL, \
				"Tiff debugger II", format_debug_tiff_raw }, \
			      { "", \
				FORMAT_RAW_MATCH_MAGIC,     0, "MM", 2, \
				FORMAT_RAW_EXIF_NONE, NULL, \
				"Tiff debugger MM", format_debug_tiff_raw }

/* used for debugging only */
gint format_debug_tiff_raw(guchar *data, const guint len,
			   guint *image_offset, guint *exif_offset);
#endif

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
