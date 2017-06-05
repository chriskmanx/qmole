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

#ifndef __FORMAT_FUJI_H
#define __FORMAT_FUJI_H


#include "exif-int.h"


gboolean format_fuji_raw(guchar *data, const guint len,
		         guint *image_offset, guint *exif_offset);


#define FORMAT_RAW_FUJI { "raf", \
			  FORMAT_RAW_MATCH_MAGIC, 0, "FUJIFILM", 8, \
			  FORMAT_RAW_EXIF_JPEG, NULL, \
			  "Fuji raw", format_fuji_raw }


gboolean format_fuji_makernote(ExifData *exif, guchar *tiff, guint offset,
			       guint size, ExifByteOrder bo);

#define FORMAT_EXIF_FUJI { FORMAT_EXIF_MATCH_MAKERNOTE, "FUJIFILM", 8, "Fujifilm", format_fuji_makernote }



#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
