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


#ifndef PAN_TYPES_H
#define PAN_TYPES_H

#include "cache.h"
#include "cache-loader.h"
#include "filedata.h"
#include "image.h"
#include "image-load.h"
#include "pixbuf_util.h"
#include "pixbuf-renderer.h"
#include "ui_misc.h"


/* thumbnail sizes and spacing */

#define PAN_THUMB_SIZE_DOTS 4
#define PAN_THUMB_SIZE_NONE 24
#define PAN_THUMB_SIZE_SMALL 64
#define PAN_THUMB_SIZE_NORMAL 128
#define PAN_THUMB_SIZE_LARGE 256
#define PAN_THUMB_SIZE pw->thumb_size

#define PAN_THUMB_GAP_DOTS 2
#define PAN_THUMB_GAP_SMALL 14
#define PAN_THUMB_GAP_NORMAL 30
#define PAN_THUMB_GAP_LARGE 40
#define PAN_THUMB_GAP_HUGE 50
#define PAN_THUMB_GAP pw->thumb_gap

/* basic sizes, colors, spacings */

#define PAN_SHADOW_OFFSET 6
#define PAN_SHADOW_FADE 5
#define PAN_SHADOW_COLOR 0, 0, 0
#define PAN_SHADOW_ALPHA 64

#define PAN_OUTLINE_THICKNESS 1
#define PAN_OUTLINE_COLOR_1 255, 255, 255
#define PAN_OUTLINE_COLOR_2 64, 64, 64
#define PAN_OUTLINE_ALPHA 180

#define PAN_BACKGROUND_COLOR 150, 150, 150

#define PAN_GRID_SIZE 60
#define PAN_GRID_COLOR 0, 0, 0
#define PAN_GRID_ALPHA 20

#define PAN_BOX_COLOR 255, 255, 255
#define PAN_BOX_ALPHA 100
#define PAN_BOX_BORDER 20

#define PAN_BOX_OUTLINE_THICKNESS 4
#define PAN_BOX_OUTLINE_COLOR 0, 0, 0
#define PAN_BOX_OUTLINE_ALPHA 128

#define PAN_TEXT_BORDER_SIZE 4
#define PAN_TEXT_COLOR 0, 0, 0

/* popup info box */

#define PAN_POPUP_COLOR 255, 255, 225
#define PAN_POPUP_ALPHA 255
#define PAN_POPUP_BORDER 1
#define PAN_POPUP_BORDER_COLOR 0, 0, 0
#define PAN_POPUP_TEXT_COLOR 0, 0, 0


#define PAN_GROUP_MAX 16



typedef enum {
	PAN_LAYOUT_TIMELINE = 0,
	PAN_LAYOUT_CALENDAR,
	PAN_LAYOUT_FOLDERS_LINEAR,
	PAN_LAYOUT_FOLDERS_FLOWER,
	PAN_LAYOUT_GRID,
	PAN_LAYOUT_COUNT
} PanLayoutType;

typedef enum {
	PAN_IMAGE_SIZE_THUMB_DOTS = 0,
	PAN_IMAGE_SIZE_THUMB_NONE,
	PAN_IMAGE_SIZE_THUMB_SMALL,
	PAN_IMAGE_SIZE_THUMB_NORMAL,
	PAN_IMAGE_SIZE_THUMB_LARGE,
	PAN_IMAGE_SIZE_10,
	PAN_IMAGE_SIZE_25,
	PAN_IMAGE_SIZE_33,
	PAN_IMAGE_SIZE_50,
	PAN_IMAGE_SIZE_100,
	PAN_IMAGE_SIZE_COUNT
} PanImageSize;

typedef enum {
	PAN_ITEM_NONE,
	PAN_ITEM_THUMB,
	PAN_ITEM_BOX,
	PAN_ITEM_TRIANGLE,
	PAN_ITEM_TEXT,
	PAN_ITEM_IMAGE
} PanItemType;

typedef enum {
	PAN_TEXT_ATTR_NONE = 0,
	PAN_TEXT_ATTR_BOLD = 1 << 0,
	PAN_TEXT_ATTR_HEADING = 1 << 1,
	PAN_TEXT_ATTR_MARKUP = 1 << 2
} PanTextAttrType;

typedef enum {
	PAN_BORDER_NONE = 0,
	PAN_BORDER_1 = 1 << 0,
	PAN_BORDER_2 = 1 << 1,
	PAN_BORDER_3 = 1 << 2,
	PAN_BORDER_4 = 1 << 3
} PanBorderType;

#define PAN_BORDER_TOP		PAN_BORDER_1
#define PAN_BORDER_RIGHT		PAN_BORDER_2
#define PAN_BORDER_BOTTOM	PAN_BORDER_3
#define PAN_BORDER_LEFT		PAN_BORDER_4


typedef struct _PanItem PanItem;
struct _PanItem {
	PanItemType type;
	gint x;
	gint y;
	gint width;
	gint height;
	gchar *key;

	FileData *fd;

	GdkPixbuf *pixbuf;
	gint refcount;

	gchar *text;
	PanTextAttrType text_attr;

	guint8 color_r;
	guint8 color_g;
	guint8 color_b;
	guint8 color_a;

	guint8 color2_r;
	guint8 color2_g;
	guint8 color2_b;
	guint8 color2_a;
	gint border;

	gpointer data;

	gboolean queued;
};

typedef struct _PanWindow PanWindow;
struct _PanWindow
{
	GtkWidget *window;
	ImageWindow *imd;
	ImageWindow *imd_normal;
	FullScreenData *fs;

	GtkWidget *path_entry;

	GtkWidget *label_message;
	GtkWidget *label_zoom;

	GtkWidget *search_box;
	GtkWidget *search_entry;
	GtkWidget *search_label;
	GtkWidget *search_button;
	GtkWidget *search_button_arrow;

	GtkWidget *date_button;

	GtkWidget *scrollbar_h;
	GtkWidget *scrollbar_v;

	FileData *dir_fd;
	PanLayoutType layout;
	PanImageSize size;
	gint thumb_size;
	gint thumb_gap;
	gint image_size;
	gboolean exif_date_enable;

	gint info_image_size;
	gboolean info_includes_exif;

	gboolean ignore_symlinks;

	GList *list;
	GList *list_static;
	GList *list_grid;

	GList *cache_list;
	GList *cache_todo;
	gint cache_count;
	gint cache_total;
	gint cache_tick;
	CacheLoader *cache_cl;

	ImageLoader *il;
	ThumbLoader *tl;
	PanItem *queue_pi;
	GList *queue;

	PanItem *click_pi;
	PanItem *search_pi;

	gint idle_id;
};

typedef struct _PanGrid PanGrid;
struct _PanGrid {
	gint x;
	gint y;
	gint w;
	gint h;
	GList *list;
};

typedef struct _PanCacheData PanCacheData;
struct _PanCacheData {
	FileData *fd;
	CacheData *cd;
};


/* pan-view.c */

GList *pan_layout_intersect(PanWindow *pw, gint x, gint y, gint width, gint height);
void pan_layout_resize(PanWindow *pw);

void pan_cache_sync_date(PanWindow *pw, GList *list);

GList *pan_cache_sort(GList *list, SortType method, gboolean ascend);
/* pan-item.c */

void pan_item_free(PanItem *pi);

void pan_item_set_key(PanItem *pi, const gchar *key);
void pan_item_added(PanWindow *pw, PanItem *pi);
void pan_item_remove(PanWindow *pw, PanItem *pi);

void pan_item_size_by_item(PanItem *pi, PanItem *child, gint border);
void pan_item_size_coordinates(PanItem *pi, gint border, gint *w, gint *h);


PanItem *pan_item_find_by_key(PanWindow *pw, PanItemType type, const gchar *key);
GList *pan_item_find_by_path(PanWindow *pw, PanItemType type, const gchar *path,
			     gboolean ignore_case, gboolean partial);
GList *pan_item_find_by_fd(PanWindow *pw, PanItemType type, FileData *fd,
			     gboolean ignore_case, gboolean partial);
PanItem *pan_item_find_by_coord(PanWindow *pw, PanItemType type,
				gint x, gint y, const gchar *key);


PanItem *pan_item_box_new(PanWindow *pw, FileData *fd, gint x, gint y, gint width, gint height,
			  gint border_size,
			  guint8 base_r, guint8 base_g, guint8 base_b, guint8 base_a,
			  guint8 bord_r, guint8 bord_g, guint8 bord_b, guint8 bord_a);
void pan_item_box_shadow(PanItem *pi, gint offset, gint fade);
gint pan_item_box_draw(PanWindow *pw, PanItem *pi, GdkPixbuf *pixbuf, PixbufRenderer *pr,
		       gint x, gint y, gint width, gint height);

PanItem *pan_item_tri_new(PanWindow *pw, FileData *fd, gint x, gint y, gint width, gint height,
			  gint x1, gint y1, gint x2, gint y2, gint x3, gint y3,
			  guint8 r, guint8 g, guint8 b, guint8 a);
void pan_item_tri_border(PanItem *pi, gint borders,
			 guint8 r, guint8 g, guint8 b, guint8 a);
gint pan_item_tri_draw(PanWindow *pw, PanItem *pi, GdkPixbuf *pixbuf, PixbufRenderer *pr,
		       gint x, gint y, gint width, gint height);

PanItem *pan_item_text_new(PanWindow *pw, gint x, gint y, const gchar *text,
			   PanTextAttrType attr, PanBorderType border,
			   guint8 r, guint8 g, guint8 b, guint8 a);
gint pan_item_text_draw(PanWindow *pw, PanItem *pi, GdkPixbuf *pixbuf, PixbufRenderer *pr,
			gint x, gint y, gint width, gint height);

PanItem *pan_item_thumb_new(PanWindow *pw, FileData *fd, gint x, gint y);
gint pan_item_thumb_draw(PanWindow *pw, PanItem *pi, GdkPixbuf *pixbuf, PixbufRenderer *pr,
			 gint x, gint y, gint width, gint height);

PanItem *pan_item_image_new(PanWindow *pw, FileData *fd, gint x, gint y, gint w, gint h);
gint pan_item_image_draw(PanWindow *pw, PanItem *pi, GdkPixbuf *pixbuf, PixbufRenderer *pr,
			 gint x, gint y, gint width, gint height);


typedef struct _PanTextAlignment PanTextAlignment;
struct _PanTextAlignment {
	PanWindow *pw;

	GList *column1;
	GList *column2;

	gint x;
	gint y;
	gchar *key;
};

PanTextAlignment *pan_text_alignment_new(PanWindow *pw, gint x, gint y, const gchar *key);
void pan_text_alignment_free(PanTextAlignment *ta);

PanItem *pan_text_alignment_add(PanTextAlignment *ta, const gchar *label, const gchar *text);
void pan_text_alignment_calc(PanTextAlignment *ta, PanItem *box);


/* utils in pan-util.c */

typedef enum {
	PAN_DATE_LENGTH_EXACT,
	PAN_DATE_LENGTH_HOUR,
	PAN_DATE_LENGTH_DAY,
	PAN_DATE_LENGTH_WEEK,
	PAN_DATE_LENGTH_MONTH,
	PAN_DATE_LENGTH_YEAR
} PanDateLengthType;

gboolean pan_date_compare(time_t a, time_t b, PanDateLengthType length);
gint pan_date_value(time_t d, PanDateLengthType length);
gchar *pan_date_value_string(time_t d,  PanDateLengthType length);
time_t pan_date_to_time(gint year, gint month, gint day);

gboolean pan_is_link_loop(const gchar *s);
gboolean pan_is_ignored(const gchar *s, gboolean ignore_symlinks);
GList *pan_list_tree(FileData *dir_fd, SortType sort, gboolean ascend,
		     gboolean ignore_symlinks);


/* the different view types */

void pan_calendar_update(PanWindow *pw, PanItem *pi_day);
void pan_calendar_compute(PanWindow *pw, FileData *dir_fd, gint *width, gint *height);
void pan_flower_compute(PanWindow *pw, FileData *dir_fd,
			gint *width, gint *height,
			gint *scroll_x, gint *scroll_y);
void pan_folder_tree_compute(PanWindow *pw, FileData *dir_fd, gint *width, gint *height);
void pan_grid_compute(PanWindow *pw, FileData *dir_fd, gint *width, gint *height);
void pan_timeline_compute(PanWindow *pw, FileData *dir_fd, gint *width, gint *height);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
