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


#ifndef COLOR_MAN_H
#define COLOR_MAN_H

typedef enum {
	COLOR_PROFILE_NONE = -1,
	COLOR_PROFILE_MEM = -2,
	COLOR_PROFILE_SRGB = 0,
	COLOR_PROFILE_ADOBERGB,
	COLOR_PROFILE_FILE,
} ColorManProfileType;

typedef enum {
	COLOR_RETURN_SUCCESS = 0,
	COLOR_RETURN_ERROR,
	COLOR_RETURN_IMAGE_CHANGED
} ColorManReturnType;

typedef struct _ColorMan ColorMan;
typedef void (* ColorManDoneFunc)(ColorMan *cm, ColorManReturnType success, gpointer data);


struct _ColorMan {
	ImageWindow *imd;
	GdkPixbuf *pixbuf;
	gint incremental_sync;
	gint row;

	gpointer profile;

	guint idle_id; /* event source id */

	ColorManDoneFunc func_done;
	gpointer func_done_data;
};


ColorMan *color_man_new(ImageWindow *imd, GdkPixbuf *pixbuf,
			ColorManProfileType input_type, const gchar *input_file,
			ColorManProfileType screen_type, const gchar *screen_file,
			guchar *screen_data, guint screen_data_len);
ColorMan *color_man_new_embedded(ImageWindow *imd, GdkPixbuf *pixbuf,
				 guchar *input_data, guint input_data_len,
				 ColorManProfileType screen_type, const gchar *screen_file,
				 guchar *screen_data, guint screen_data_len);
void color_man_free(ColorMan *cm);

void color_man_update(void);

void color_man_correct_region(ColorMan *cm, GdkPixbuf *pixbuf, gint x, gint y, gint w, gint h);

void color_man_start_bg(ColorMan *cm, ColorManDoneFunc don_func, gpointer done_data);

gboolean color_man_get_status(ColorMan *cm, gchar **image_profile, gchar **screen_profile);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
