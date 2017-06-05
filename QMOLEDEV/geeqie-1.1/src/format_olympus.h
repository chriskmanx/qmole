/*
 * Geeqie
 * (C) 2005 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef __FORMAT_OLYMPUS_H
#define __FORMAT_OLYMPUS_H


#include "exif-int.h"


gboolean format_olympus_raw(guchar *data, const guint len,
			    guint *image_offset, guint *exif_offset);


#define FORMAT_RAW_OLYMPUS { "orf", \
			     FORMAT_RAW_MATCH_MAGIC, 0, "IIR", 3, \
			     FORMAT_RAW_EXIF_IFD_II, NULL, \
			     "Olympus raw", format_olympus_raw }


gboolean format_olympus_makernote(ExifData *exif, guchar *tiff, guint offset,
			          guint size, ExifByteOrder bo);

#define FORMAT_EXIF_OLYMPUS { FORMAT_EXIF_MATCH_MAKERNOTE, "OLYMP\x00\x01", 7, \
			      "Olympus", format_olympus_makernote }, \
			    { FORMAT_EXIF_MATCH_MAKERNOTE, "OLYMP\x00\x02", 7, \
			      "Olympus", format_olympus_makernote }


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
