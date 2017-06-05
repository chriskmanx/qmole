/*
 * Copyright (C) 2008 nsf
 */

#ifndef BMPANEL_THEME_H
#define BMPANEL_THEME_H

#include <Imlib2.h>
#include "common.h"

#define BSTATE_IDLE 	0
#define BSTATE_PRESSED 	1

#define ALIGN_LEFT 	0
#define ALIGN_CENTER 	1
#define ALIGN_RIGHT 	2

#define PLACE_TOP	0
#define PLACE_BOTTOM 	1

#define WIDTH_TYPE_PIXELS	0
#define WIDTH_TYPE_PERCENT	1

struct color {
	uchar r, g, b;
};

struct clock_theme {
	Imlib_Image right_img;
	Imlib_Image tile_img;
	Imlib_Image left_img;

	Imlib_Font font;

	struct color text_color;
	int text_offset_x;
	int text_offset_y;
	int text_padding;
	uint text_align;
	
	int space_gap;

	char *format;
};

struct taskbar_theme {
	Imlib_Image right_img[2];
	Imlib_Image tile_img[2];
	Imlib_Image left_img[2];
	Imlib_Image separator_img;

	Imlib_Font font;
	
	struct color text_color[2];
	int text_offset_x;
	int text_offset_y;
	uint text_align;

	Imlib_Image default_icon_img;
	int icon_offset_x;
	int icon_offset_y;
	int icon_w;
	int icon_h;

	int space_gap;
};

struct switcher_theme {
	Imlib_Image left_corner_img[2];
	Imlib_Image right_corner_img[2];

	Imlib_Image right_img[2];
	Imlib_Image tile_img[2];
	Imlib_Image left_img[2];
	
	Imlib_Image separator_img;

	Imlib_Font font;
	
	struct color text_color[2];
	int text_offset_x;
	int text_offset_y;
	int text_padding;
	uint text_align;
	
	int space_gap;
};

struct theme {
	char *name;
	char *author;
	int version_major;
	int version_minor;

	/* general */
	int placement;
	char *elements;
	Imlib_Image tile_img;
	Imlib_Image separator_img;

	int tray_icon_w;
	int tray_icon_h;
	int tray_space_gap;
	int tray_icons_spacing;
	int use_composite;
	int height_override;
	int width;
	int alignment;
	int width_type;

	/* elements */
	struct clock_theme clock;
	struct taskbar_theme taskbar;
	struct switcher_theme switcher;

	/* these values are calculated on fly */
	int height;
	char *themedir;
};

#define THEME_USE_TASKBAR_ICON(t) \
	(is_element_in_theme((t), 'b') && \
	 (t)->taskbar.icon_w != 0 && \
	 (t)->taskbar.icon_h != 0)

struct theme *load_theme(const char *dir);
void free_theme(struct theme *t);
int theme_is_valid(struct theme *t);
int is_element_in_theme(struct theme *t, char e);
void theme_remove_element(struct theme* t, char e);

#endif
