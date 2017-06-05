/*
 * Geeqie
 * (C) 2006 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef IMAGE_OVERLAY_H
#define IMAGE_OVERLAY_H

typedef enum {
	IMAGE_OSD_NONE = 0,
	IMAGE_OSD_ROTATE_USER,
	IMAGE_OSD_ROTATE_AUTO,
	IMAGE_OSD_COLOR,
	IMAGE_OSD_FIRST,
	IMAGE_OSD_LAST,
	IMAGE_OSD_ICON,
	IMAGE_OSD_COUNT
} ImageOSDFlag;

typedef enum {
	OSD_SHOW_NOTHING	= 0,
	OSD_SHOW_INFO		= 1 << 0,
	OSD_SHOW_STATUS		= 1 << 1,
	OSD_SHOW_HISTOGRAM	= 1 << 2
} OsdShowFlags;

void set_image_overlay_template_string(gchar **template_string, const gchar *value);
void set_default_image_overlay_template_string(gchar **template_string);

void image_osd_set(ImageWindow *imd, OsdShowFlags show);
OsdShowFlags image_osd_get(ImageWindow *imd);

Histogram *image_osd_get_histogram(ImageWindow *imd);

void image_osd_copy_status(ImageWindow *src, ImageWindow *dest);

void image_osd_update(ImageWindow *imd);

void image_osd_icon(ImageWindow *imd, ImageOSDFlag flag, gint duration);

void image_osd_histogram_toggle_channel(ImageWindow *imd);
void image_osd_histogram_toggle_mode(ImageWindow *imd);
void image_osd_histogram_set_channel(ImageWindow *imd, gint chan);
void image_osd_histogram_set_mode(ImageWindow *imd, gint mode);
gint image_osd_histogram_get_channel(ImageWindow *imd);
gint image_osd_histogram_get_mode(ImageWindow *imd);

void image_osd_toggle(ImageWindow *imd);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
