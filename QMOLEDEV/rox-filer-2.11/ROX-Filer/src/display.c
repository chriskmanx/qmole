/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* display.c - code for arranging and displaying file items */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>

#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>

#include "global.h"

#include "main.h"
#include "filer.h"
#include "display.h"
#include "support.h"
#include "gui_support.h"
#include "pixmaps.h"
#include "menu.h"
#include "dnd.h"
#include "run.h"
#include "mount.h"
#include "type.h"
#include "options.h"
#include "action.h"
#include "minibuffer.h"
#include "dir.h"
#include "diritem.h"
#include "fscache.h"
#include "view_iface.h"
#include "xtypes.h"

#define HUGE_WRAP (1.5 * o_large_width.int_value)

/* Options bits */
static Option o_display_caps_first;
static Option o_display_dirs_first;
Option o_display_size;
Option o_display_details;
Option o_display_sort_by;
static Option o_large_width;
Option o_small_width;
Option o_display_show_hidden;
Option o_display_show_thumbs;
Option o_display_show_headers;
Option o_display_show_full_type;
Option o_display_inherit_options;
static Option o_filer_change_size_num;
Option o_vertical_order_small, o_vertical_order_large;
Option o_xattr_show;

/* Static prototypes */
static void display_details_set(FilerWindow *filer_window, DetailsType details);
static void display_style_set(FilerWindow *filer_window, DisplayStyle style);
static void options_changed(void);
static char *details(FilerWindow *filer_window, DirItem *item);
static void display_set_actual_size_real(FilerWindow *filer_window);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void display_init()
{
	option_add_int(&o_display_caps_first, "display_caps_first", FALSE);
	option_add_int(&o_display_dirs_first, "display_dirs_first", FALSE);
	option_add_int(&o_display_size, "display_icon_size", AUTO_SIZE_ICONS);
	option_add_int(&o_display_details, "display_details", DETAILS_NONE);
	option_add_int(&o_display_sort_by, "display_sort_by", SORT_NAME);
	option_add_int(&o_large_width, "display_large_width", 155);
	option_add_int(&o_small_width, "display_small_width", 250);
	option_add_int(&o_display_show_hidden, "display_show_hidden", FALSE);
	option_add_int(&o_display_show_thumbs, "display_show_thumbs", FALSE);
	option_add_int(&o_display_show_headers, "display_show_headers", TRUE);
	option_add_int(&o_display_show_full_type, "display_show_full_type", FALSE);
	option_add_int(&o_display_inherit_options,
		       "display_inherit_options", FALSE); 
	option_add_int(&o_filer_change_size_num, "filer_change_size_num", 30); 
	option_add_int(&o_vertical_order_small, "vertical_order_small", FALSE);
	option_add_int(&o_vertical_order_large, "vertical_order_large", FALSE);
	option_add_int(&o_xattr_show, "xattr_show", TRUE);

	option_add_notify(options_changed);
}

void draw_emblem_on_icon(GdkWindow *window, GtkStyle   *style,
				const char *stock_id,
				int *x, int y)
{
	GtkIconSet *icon_set;
	GdkPixbuf  *pixbuf;

	icon_set = gtk_style_lookup_icon_set(style,
					     stock_id);
	if (icon_set)
	{
		pixbuf = gtk_icon_set_render_icon(icon_set,
						  style,
						  GTK_TEXT_DIR_LTR,
						  GTK_STATE_NORMAL,
						  mount_icon_size,
						  NULL,
						  NULL);
	}
	else
	{
		pixbuf=im_unknown->pixbuf;
		g_object_ref(pixbuf);
	}
	
	gdk_pixbuf_render_to_drawable_alpha(pixbuf,
				window,
				0, 0, 				/* src */
				*x, y,		                /* dest */
				-1, -1,
				GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
				GDK_RGB_DITHER_NORMAL, 0, 0);
	
	*x+=gdk_pixbuf_get_width(pixbuf)+1;
	g_object_unref(pixbuf);
}

/* Draw this icon (including any symlink or mount symbol) inside the
 * given rectangle.
 */
void draw_huge_icon(GdkWindow *window, GtkStyle *style, GdkRectangle *area,
		   DirItem  *item, MaskedPixmap *image, gboolean selected,
		   GdkColor *color)
{
	int		width, height;
	int		image_x;
	int		image_y;
	GdkPixbuf	*pixbuf;

	if (!image)
		return;

	width = image->huge_width;
	height = image->huge_height;
	image_x = area->x + ((area->width - width) >> 1);
	image_y = MAX(0, area->height - height - 6);

	pixbuf = selected
		? create_spotlight_pixbuf(image->huge_pixbuf, color)
		: image->huge_pixbuf;

	gdk_pixbuf_render_to_drawable_alpha(
			pixbuf,
			window,
			0, 0, 				/* src */
			image_x, area->y + image_y,	/* dest */
			width, height,
			GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
			GDK_RGB_DITHER_NORMAL, 0, 0);

	if (selected)
		g_object_unref(pixbuf);

	if (item->flags & ITEM_FLAG_MOUNT_POINT)
	{
		const char *mp = item->flags & ITEM_FLAG_MOUNTED
					? ROX_STOCK_MOUNTED
					: ROX_STOCK_MOUNT;
		draw_emblem_on_icon(window, style, mp, &image_x, area->y + 2);
	}
	if (item->flags & ITEM_FLAG_SYMLINK)
	{
		draw_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
				    &image_x, area->y + 2);
	}
	if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
	{
		draw_emblem_on_icon(window, style, ROX_STOCK_XATTR,
				    &image_x, area->y + 2);
	}
}

/* Draw this icon (including any symlink or mount symbol) inside the
 * given rectangle.
 */
void draw_large_icon(GdkWindow *window,
		     GtkStyle *style,
		     GdkRectangle *area,
		     DirItem  *item,
		     MaskedPixmap *image,
		     gboolean selected,
		     GdkColor *color)
{
	int	width;
	int	height;
	int	image_x;
	int	image_y;
	GdkPixbuf *pixbuf;

	if (!image)
		return;

	width = MIN(image->width, ICON_WIDTH);
	height = MIN(image->height, ICON_HEIGHT);
	image_x = area->x + ((area->width - width) >> 1);
	image_y = MAX(0, area->height - height - 6);

	pixbuf = selected
		? create_spotlight_pixbuf(image->pixbuf, color)
		: image->pixbuf;

	gdk_pixbuf_render_to_drawable_alpha(
			pixbuf,
			window,
			0, 0, 				/* src */
			image_x, area->y + image_y,	/* dest */
			width, height,
			GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
			GDK_RGB_DITHER_NORMAL, 0, 0);

	if (selected)
		g_object_unref(pixbuf);

	if (item->flags & ITEM_FLAG_MOUNT_POINT)
	{
		const char *mp = item->flags & ITEM_FLAG_MOUNTED
					? ROX_STOCK_MOUNTED
					: ROX_STOCK_MOUNT;
		draw_emblem_on_icon(window, style, mp, &image_x, area->y + 2);
	}
	if (item->flags & ITEM_FLAG_SYMLINK)
	{
		draw_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
				    &image_x, area->y + 2);
	}
	if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
	{
		draw_emblem_on_icon(window, style, ROX_STOCK_XATTR,
				    &image_x, area->y + 2);
	}
}

void draw_small_icon(GdkWindow *window, GtkStyle *style, GdkRectangle *area,
		     DirItem  *item, MaskedPixmap *image, gboolean selected,
		     GdkColor *color)
{
	int		width, height, image_x, image_y;
	GdkPixbuf	*pixbuf;
	
	if (!image)
		return;

	if (!image->sm_pixbuf)
		pixmap_make_small(image);

	width = MIN(image->sm_width, SMALL_WIDTH);
	height = MIN(image->sm_height, SMALL_HEIGHT);
	image_x = area->x + ((area->width - width) >> 1);
	image_y = MAX(0, SMALL_HEIGHT - image->sm_height);
		
	pixbuf = selected
		? create_spotlight_pixbuf(image->sm_pixbuf, color)
		: image->sm_pixbuf;

	gdk_pixbuf_render_to_drawable_alpha(
			pixbuf,
			window,
			0, 0, 				/* src */
			image_x, area->y + image_y,	/* dest */
			width, height,
			GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
			GDK_RGB_DITHER_NORMAL, 0, 0);

	if (selected)
		g_object_unref(pixbuf);

	if (item->flags & ITEM_FLAG_MOUNT_POINT)
	{
		const char *mp = item->flags & ITEM_FLAG_MOUNTED
					? ROX_STOCK_MOUNTED
					: ROX_STOCK_MOUNT;
		draw_emblem_on_icon(window, style, mp, &image_x, area->y + 2);
	}
	if (item->flags & ITEM_FLAG_SYMLINK)
	{
		draw_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
				    &image_x, area->y + 8);
	}
	if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
	{
		draw_emblem_on_icon(window, style, ROX_STOCK_XATTR,
				    &image_x, area->y + 8);
	}
}

/* The sort functions aren't called from outside, but they are
 * passed as arguments to display_set_sort_fn().
 */

#define IS_A_DIR(item) (item->base_type == TYPE_DIRECTORY && \
			!(item->flags & ITEM_FLAG_APPDIR))

#define SORT_DIRS	\
	if (o_display_dirs_first.int_value) {	\
		gboolean id1 = IS_A_DIR(i1);	\
		gboolean id2 = IS_A_DIR(i2);	\
		if (id1 && !id2) return -1;				\
		if (id2 && !id1) return 1;				\
	}

int sort_by_name(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	CollateKey *n1 = i1->leafname_collate;
	CollateKey *n2 = i2->leafname_collate;
	int retval;

	SORT_DIRS;

	retval = collate_key_cmp(n1, n2, o_display_caps_first.int_value);

	return retval ? retval : strcmp(i1->leafname, i2->leafname);
}

int sort_by_type(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	MIME_type *m1, *m2;

	int	 diff = i1->base_type - i2->base_type;

	if (!diff)
		diff = (i1->flags & ITEM_FLAG_APPDIR)
		     - (i2->flags & ITEM_FLAG_APPDIR);
	if (diff)
		return diff > 0 ? 1 : -1;

	m1 = i1->mime_type;
	m2 = i2->mime_type;
	
	if (m1 && m2)
	{
		diff = strcmp(m1->media_type, m2->media_type);
		if (!diff)
			diff = strcmp(m1->subtype, m2->subtype);
	}
	else if (m1 || m2)
		diff = m1 ? 1 : -1;
	else
		diff = 0;

	if (diff)
		return diff > 0 ? 1 : -1;
	
	return sort_by_name(item1, item2);
}

int sort_by_owner(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	const gchar *name1;
	const gchar *name2;

	if(i1->uid==i2->uid)
		return sort_by_name(item1, item2);

	name1=user_name(i1->uid);
	name2=user_name(i2->uid);

	return strcmp(name1, name2);
}

int sort_by_group(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	const gchar *name1;
	const gchar *name2;

	if(i1->gid==i2->gid)
		return sort_by_name(item1, item2);

	name1=group_name(i1->gid);
	name2=group_name(i2->gid);

	return strcmp(name1, name2);
}

int sort_by_date(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;

	/* SORT_DIRS; -- too confusing! */

	return i1->mtime < i2->mtime ? -1 :
		i1->mtime > i2->mtime ? 1 :
		sort_by_name(item1, item2);
}

int sort_by_size(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;

	SORT_DIRS;

	return i1->size < i2->size ? -1 :
		i1->size > i2->size ? 1 :
		sort_by_name(item1, item2);
}

void display_set_sort_type(FilerWindow *filer_window, SortType sort_type,
			   GtkSortType order)
{
	if (filer_window->sort_type == sort_type &&
	    filer_window->sort_order == order)
		return;

	filer_window->sort_type = sort_type;
	filer_window->sort_order = order;

	view_sort(filer_window->view);
}

/* Change the icon size and style.
 * force_resize should only be TRUE for new windows.
 */
void display_set_layout(FilerWindow  *filer_window,
			DisplayStyle style,
			DetailsType  details,
			gboolean     force_resize)
{
	gboolean style_changed = FALSE;

	g_return_if_fail(filer_window != NULL);

	if (filer_window->display_style_wanted != style 
	    || filer_window->details_type != details)
	{
		style_changed = TRUE;
	}
	  
	display_style_set(filer_window, style);
	display_details_set(filer_window, details);

	/* Recreate layouts because wrapping may have changed */
	view_style_changed(filer_window->view, VIEW_UPDATE_NAME);

	if (force_resize || o_filer_auto_resize.int_value == RESIZE_ALWAYS
	    || (o_filer_auto_resize.int_value == RESIZE_STYLE && style_changed))
	{
		view_autosize(filer_window->view);
	}
}

/* Set the 'Show Thumbnails' flag for this window */
void display_set_thumbs(FilerWindow *filer_window, gboolean thumbs)
{
	if (filer_window->show_thumbs == thumbs)
		return;

	filer_window->show_thumbs = thumbs;

	view_style_changed(filer_window->view, VIEW_UPDATE_VIEWDATA);

	if (!thumbs)
		filer_cancel_thumbnails(filer_window);

	filer_set_title(filer_window);

	filer_create_thumbs(filer_window);
}

void display_update_hidden(FilerWindow *filer_window)
{
	filer_detach_rescan(filer_window);	/* (updates titlebar) */

	display_set_actual_size(filer_window, FALSE);
}

/* Set the 'Show Hidden' flag for this window */
void display_set_hidden(FilerWindow *filer_window, gboolean hidden)
{
	if (filer_window->show_hidden == hidden)
		return;

	/*
	filer_window->show_hidden = hidden;
	*/
	filer_set_hidden(filer_window, hidden);

	display_update_hidden(filer_window);
}

/* Set the 'Filter Directories' flag for this window */
void display_set_filter_directories(FilerWindow *filer_window, gboolean filter_directories)
{
	if (filer_window->filter_directories == filter_directories)
		return;

	/*
	filer_window->show_hidden = hidden;
	*/
	filer_set_filter_directories(filer_window, filter_directories);

	display_update_hidden(filer_window);
}

void display_set_filter(FilerWindow *filer_window, FilterType type,
			const gchar *filter_string)
{
	if (filer_set_filter(filer_window, type, filter_string))
		display_update_hidden(filer_window);
}


/* Highlight (wink or cursor) this item in the filer window. If the item
 * isn't already there but we're scanning then highlight it if it
 * appears later.
 */
void display_set_autoselect(FilerWindow *filer_window, const gchar *leaf)
{
	gchar *new;
	
	g_return_if_fail(filer_window != NULL);
	g_return_if_fail(leaf != NULL);

	new = g_strdup(leaf);	/* leaf == old value sometimes */

	null_g_free(&filer_window->auto_select);

	if (view_autoselect(filer_window->view, new))
		g_free(new);
	else
		filer_window->auto_select = new;
}

/* Change the icon size (wraps) */
void display_change_size(FilerWindow *filer_window, gboolean bigger)
{
	DisplayStyle	new;

	g_return_if_fail(filer_window != NULL);

	switch (filer_window->display_style)
	{
		case LARGE_ICONS:
			new = bigger ? HUGE_ICONS : SMALL_ICONS;
			break;
		case HUGE_ICONS:
			if (bigger)
				return;
			new = LARGE_ICONS;
			break;
		default:
			if (!bigger)
				return;
			new = LARGE_ICONS;
			break;
	}

	display_set_layout(filer_window, new, filer_window->details_type,
			   FALSE);
}

ViewData *display_create_viewdata(FilerWindow *filer_window, DirItem *item)
{
	ViewData *view;

	view = g_new(ViewData, 1);

	view->layout = NULL;
	view->details = NULL;
	view->image = NULL;

	display_update_view(filer_window, item, view, TRUE);

	return view;
}

/* Set the display style to the desired style. If the desired style
 * is AUTO_SIZE_ICONS, choose an appropriate size. Also resizes filer
 * window, if requested.
 */
void display_set_actual_size(FilerWindow *filer_window, gboolean force_resize)
{
	display_set_layout(filer_window, filer_window->display_style_wanted,
			   filer_window->details_type, force_resize);
}


/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void options_changed(void)
{
	GList		*next;

	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;
		int flags = 0;

		if (o_display_dirs_first.has_changed ||
		    o_display_caps_first.has_changed)
			view_sort(VIEW(filer_window->view));

		if (o_display_show_headers.has_changed)
			flags |= VIEW_UPDATE_HEADERS;

		if (o_large_width.has_changed || o_small_width.has_changed)
			flags |= VIEW_UPDATE_NAME; /* Recreate PangoLayout */

		view_style_changed(filer_window->view, flags);
	}
}

/* Return a new string giving details of this item, or NULL if details
 * are not being displayed. If details are not yet available, return
 * a string of the right length.
 */
static char *details(FilerWindow *filer_window, DirItem *item)
{
	mode_t	m = item->mode;
	guchar 	*buf = NULL;
	gboolean scanned = item->base_type != TYPE_UNKNOWN;

	if (filer_window->details_type == DETAILS_NONE)
		return NULL;

	if (scanned && item->lstat_errno)
		buf = g_strdup_printf(_("lstat(2) failed: %s"),
				g_strerror(item->lstat_errno));
	else if (filer_window->details_type == DETAILS_TYPE)
	{
		MIME_type	*type = item->mime_type;

		if (!scanned)
			return g_strdup("application/octet-stream");

		buf = g_strdup_printf("%s/%s",
				      type->media_type, type->subtype);
	}
	else if (filer_window->details_type == DETAILS_TIMES)
	{
		guchar	*ctime, *mtime, *atime;
		
		ctime = pretty_time(&item->ctime);
		mtime = pretty_time(&item->mtime);
		atime = pretty_time(&item->atime);
		
		buf = g_strdup_printf("a[%s] c[%s] m[%s]", atime, ctime, mtime);
		g_free(ctime);
		g_free(mtime);
		g_free(atime);
	}
	else if (filer_window->details_type == DETAILS_PERMISSIONS)
	{
		if (!scanned)
			return g_strdup("---,---,---/--"
#ifdef S_ISVTX
					"-"
#endif
					" 12345678 12345678");

		buf = g_strdup_printf("%s %-8.8s %-8.8s",
				pretty_permissions(m),
				user_name(item->uid),
				group_name(item->gid));
	}
	else
	{
		if (!scanned)
		{
			if (filer_window->display_style == SMALL_ICONS)
				return g_strdup("1234M");
			else
				return g_strdup("1234 bytes");
		}

		if (item->base_type != TYPE_DIRECTORY)
		{
			if (filer_window->display_style == SMALL_ICONS)
				buf = g_strdup(format_size_aligned(item->size));
			else
				buf = g_strdup(format_size(item->size));
		}
		else
			buf = g_strdup("-");
	}
		
	return buf;
}

/* Note: Call style_changed after this */
static void display_details_set(FilerWindow *filer_window, DetailsType details)
{
	filer_window->details_type = details;
}

/* Note: Call style_changed after this */
static void display_style_set(FilerWindow *filer_window, DisplayStyle style)
{
	filer_window->display_style_wanted = style;
	display_set_actual_size_real(filer_window);
}

/* Each displayed item has a ViewData structure with some cached information
 * to help quickly draw the item (eg, the PangoLayout). This function updates
 * this information.
 */
void display_update_view(FilerWindow *filer_window,
			 DirItem *item,
			 ViewData *view,
			 gboolean update_name_layout)
{
	DisplayStyle	style = filer_window->display_style;
	int	w, h;
	int	wrap_width = -1;
	char	*str;
	static PangoFontDescription *monospace = NULL;
	PangoAttrList *list = NULL;

	if (!monospace)
		monospace = pango_font_description_from_string("monospace");
	
	if (view->details)
	{
		g_object_unref(G_OBJECT(view->details));
		view->details = NULL;
	}

	str = details(filer_window, item);
	if (str)
	{
		PangoAttrList	*details_list;
		int	perm_offset = -1;
		
		view->details = gtk_widget_create_pango_layout(
					filer_window->window, str);
		g_free(str);

		pango_layout_set_font_description(view->details, monospace);
		pango_layout_get_size(view->details, &w, &h);
		view->details_width = w / PANGO_SCALE;
		view->details_height = h / PANGO_SCALE;

		if (filer_window->details_type == DETAILS_PERMISSIONS)
			perm_offset = 0;
		if (perm_offset > -1)
		{
			PangoAttribute	*attr;

			attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);

			perm_offset += 4 * applicable(item->uid, item->gid);
			attr->start_index = perm_offset;
			attr->end_index = perm_offset + 3;

			details_list = pango_attr_list_new();
			pango_attr_list_insert(details_list, attr);
			pango_layout_set_attributes(view->details,
							details_list);
		}
	}

	if (view->image)
	{
		g_object_unref(view->image);
		view->image = NULL;
	}

	if (filer_window->show_thumbs && item->base_type == TYPE_FILE /*&&
									strcmp(item->mime_type->media_type, "image") == 0*/)
	{
		const guchar    *path;

		path = make_path(filer_window->real_path, item->leafname);

		view->image = g_fscache_lookup_full(pixmap_cache, path,
				FSCACHE_LOOKUP_ONLY_NEW, NULL);
	}

	if (!view->image)
	{
		view->image = di_image(item);
		if (view->image)
			g_object_ref(view->image);
	}

	if (view->layout && update_name_layout)
	{
		g_object_unref(G_OBJECT(view->layout));
		view->layout = NULL;
	}

	if (view->layout)
	{
		/* Do nothing */
	}
	else if (g_utf8_validate(item->leafname, -1, NULL))
	{
		view->layout = gtk_widget_create_pango_layout(
				filer_window->window, item->leafname);
		pango_layout_set_auto_dir(view->layout, FALSE);
	}
	else
	{
		PangoAttribute	*attr;
		gchar *utf8;

		utf8 = to_utf8(item->leafname);
		view->layout = gtk_widget_create_pango_layout(
				filer_window->window, utf8);
		g_free(utf8);

		attr = pango_attr_foreground_new(0xffff, 0, 0);
		attr->start_index = 0;
		attr->end_index = -1;
		if (!list)
			list = pango_attr_list_new();
		pango_attr_list_insert(list, attr);
	}

	if (item->flags & ITEM_FLAG_RECENT)
	{
		PangoAttribute	*attr;

		attr = pango_attr_weight_new(PANGO_WEIGHT_BOLD);
		attr->start_index = 0;
		attr->end_index = -1;
		if (!list)
			list = pango_attr_list_new();
		pango_attr_list_insert(list, attr);
	}

	if (list)
		pango_layout_set_attributes(view->layout, list);

	if (filer_window->details_type == DETAILS_NONE)
	{
		if (style == HUGE_ICONS)
			wrap_width = HUGE_WRAP * PANGO_SCALE;
		else if (style == LARGE_ICONS)
			wrap_width = o_large_width.int_value * PANGO_SCALE;
	}

#ifdef USE_PANGO_WRAP_WORD_CHAR
	pango_layout_set_wrap(view->layout, PANGO_WRAP_WORD_CHAR);
#endif
	if (wrap_width != -1)
		pango_layout_set_width(view->layout, wrap_width);

	pango_layout_get_size(view->layout, &w, &h);
	view->name_width = w / PANGO_SCALE;
	view->name_height = h / PANGO_SCALE;
}

/* Sets display_style from display_style_wanted.
 * See also display_set_actual_size().
 */
static void display_set_actual_size_real(FilerWindow *filer_window)
{
	DisplayStyle size = filer_window->display_style_wanted;
	int n;
	
	g_return_if_fail(filer_window != NULL);

	if (size == AUTO_SIZE_ICONS)
	{
		n = view_count_items(filer_window->view);

		if (n >= o_filer_change_size_num.int_value)
			size = SMALL_ICONS;
		else
			size = LARGE_ICONS;
	}
	
	filer_window->display_style = size;
}
