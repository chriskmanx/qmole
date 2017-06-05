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

/* cell_icon.c - a GtkCellRenderer used for the icons in details mode
 *
 * Based on gtkcellrendererpixbuf.c.
 */

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

#include "global.h"

#include "view_details.h"
#include "cell_icon.h"
#include "filer.h"
#include "display.h"
#include "diritem.h"
#include "pixmaps.h"
#include "type.h"
#include "support.h"
#include "fscache.h"

typedef struct _CellIcon CellIcon;
typedef struct _CellIconClass CellIconClass;

struct _CellIcon {
	GtkCellRenderer parent;

	ViewDetails *view_details;
	ViewItem *item;
	GdkColor background;
};

struct _CellIconClass {
	GtkCellRendererClass parent_class;
};


/* Static prototypes */
static void cell_icon_set_property(GObject *object, guint param_id,
				       const GValue *value, GParamSpec *pspec);
static void cell_icon_init       (CellIcon *cell);
static void cell_icon_class_init (CellIconClass *class);
static void cell_icon_get_size   (GtkCellRenderer	*cell,
				      GtkWidget		*widget,
				      GdkRectangle	*rectangle,
				      gint		*x_offset,
				      gint		*y_offset,
				      gint		*width,
				      gint		*height);
static void cell_icon_render     (GtkCellRenderer		*cell,
				      GdkWindow		*window,
				      GtkWidget		*widget,
				      GdkRectangle	*background_area,
				      GdkRectangle	*cell_area,
				      GdkRectangle	*expose_area,
				      guint		flags);
static GtkType cell_icon_get_type(void);

enum {
	PROP_ZERO,
	PROP_ITEM,
	PROP_BACKGROUND_GDK,
};

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

GtkCellRenderer *cell_icon_new(ViewDetails *view_details)
{
	GtkCellRenderer *cell;

	cell = GTK_CELL_RENDERER(g_object_new(cell_icon_get_type(), NULL));
	((CellIcon *) cell)->view_details = view_details;

	return cell;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/


static GtkType cell_icon_get_type(void)
{
	static GtkType cell_icon_type = 0;

	if (!cell_icon_type)
	{
		static const GTypeInfo cell_icon_info =
		{
			sizeof (CellIconClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) cell_icon_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (CellIcon),
			0,              /* n_preallocs */
			(GInstanceInitFunc) cell_icon_init,
		};

		cell_icon_type = g_type_register_static(GTK_TYPE_CELL_RENDERER,
							"CellIcon",
							&cell_icon_info, 0);
	}

	return cell_icon_type;
}

static void cell_icon_init(CellIcon *icon)
{
	icon->view_details = NULL;
}

static void cell_icon_class_init(CellIconClass *class)
{
	GObjectClass *object_class = G_OBJECT_CLASS(class);
	GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS(class);

	object_class->set_property = cell_icon_set_property;

	cell_class->get_size = cell_icon_get_size;
	cell_class->render = cell_icon_render;

	g_object_class_install_property(object_class,
			PROP_ITEM,
			g_param_spec_pointer("item",
				"DirItem",
				"The item to render.",
				G_PARAM_WRITABLE));

	g_object_class_install_property(object_class,
			PROP_BACKGROUND_GDK,
			g_param_spec_boxed("background_gdk",
				"Background color",
				"Background color as a GdkColor",
				GDK_TYPE_COLOR,
				G_PARAM_WRITABLE));  
}

static void cell_icon_set_property(GObject *object, guint param_id,
				       const GValue *value, GParamSpec *pspec)
{
	CellIcon *icon = (CellIcon *) object;

	switch (param_id)
	{
		case PROP_ITEM:
			icon->item = (ViewItem *) g_value_get_pointer(value);
			break;
		case PROP_BACKGROUND_GDK:
		{
			GdkColor *bg = g_value_get_boxed(value);
			if (bg)
			{
				icon->background.red = bg->red;
				icon->background.green = bg->green;
				icon->background.blue = bg->blue;
			}
			break;
		}
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object,
							  param_id, pspec);
			break;
	}
}

/* Return the size to display this icon at. If huge, ensures that the image
 * exists in that size, if present at all.
 */
static DisplayStyle get_style(GtkCellRenderer *cell)
{
	CellIcon *icon = (CellIcon *) cell;
	ViewItem *view_item = icon->item;
	DisplayStyle size;
	DirItem *item = view_item->item;

	if (!view_item->image)
	{
		FilerWindow *filer_window = icon->view_details->filer_window;

		if (filer_window->show_thumbs && item->base_type == TYPE_FILE)
		{
			const guchar    *path;

			path = make_path(filer_window->real_path,
					 item->leafname);

			view_item->image = g_fscache_lookup_full(pixmap_cache,
					path, FSCACHE_LOOKUP_ONLY_NEW, NULL);
		}
		if (!view_item->image)
		{
			view_item->image = di_image(item);
			if (view_item->image)
				g_object_ref(view_item->image);
		}
	}
	
	size = icon->view_details->filer_window->display_style_wanted;

	if (size == AUTO_SIZE_ICONS)
	{
		if (!view_item->image || view_item->image == di_image(item))
			size = SMALL_ICONS;
		else
			size = HUGE_ICONS;
	}
	if (size == HUGE_ICONS && view_item->image &&
	    !view_item->image->huge_pixbuf)
		pixmap_make_huge(view_item->image);

	return size;
}

static void cell_icon_get_size(GtkCellRenderer *cell,
				   GtkWidget       *widget,
				   GdkRectangle    *cell_area,
				   gint            *x_offset,
				   gint            *y_offset,
				   gint            *width,
				   gint            *height)
{
	MaskedPixmap *image;
	DisplayStyle size;
	int w, h;

	size = get_style(cell);
	image = ((CellIcon *) cell)->item->image;

	if (x_offset)
		*x_offset = 0;
	if (y_offset)
		*y_offset = 0;

	switch (size)
	{
		case SMALL_ICONS:
			w = SMALL_WIDTH;
			h = SMALL_HEIGHT;
			break;
		case LARGE_ICONS:
			if (image)
			{
				w = image->width;
				h = image->height;
			}
			else
			{
				w = ICON_WIDTH;
				h = ICON_HEIGHT;
			}
			break;
		case HUGE_ICONS:
			if (image)
			{
				w = image->huge_width;
				h = image->huge_height;
			}
			else
			{
				w = HUGE_WIDTH;
				h = HUGE_HEIGHT;
			}
			break;
		default:
			w = 2;
			h = 2;
			break;
	}

	if (width)
		*width = w;
	if (height)
		*height = h;
}

static void cell_icon_render(GtkCellRenderer    *cell,
			     GdkWindow		*window,
			     GtkWidget		*widget,
			     GdkRectangle	*background_area,
			     GdkRectangle	*cell_area,
			     GdkRectangle	*expose_area,
			     guint		flags)
{
	CellIcon *icon = (CellIcon *) cell;
	ViewItem *view_item = icon->item;
	DirItem *item;
	DisplayStyle size;
	gboolean selected = (flags & GTK_CELL_RENDERER_SELECTED) != 0;
	GdkColor *color;
	
	g_return_if_fail(view_item != NULL);

	item = view_item->item;
	size = get_style(cell);
	color = &widget->style->base[icon->view_details->filer_window->selection_state];

	/* Draw the icon */

	if (!view_item->image)
		return;

	switch (size)
	{
		case SMALL_ICONS:
		{
			GdkRectangle area = *cell_area;
			area.width = MIN(area.width, SMALL_WIDTH);
			area.x = cell_area->x + cell_area->width - area.width;
			draw_small_icon(window, widget->style, &area, item,
					view_item->image, selected, color);

			break;
		}
		case LARGE_ICONS:
			draw_large_icon(window, widget->style, cell_area, item,
					view_item->image, selected, color);
			break;
		case HUGE_ICONS:
			if (!di_image(item)->huge_pixbuf)
				pixmap_make_huge(di_image(item));
			draw_huge_icon(window, widget->style, cell_area, item,
					view_item->image, selected, color);
			break;
		default:
			g_warning("Unknown size %d\n", size);
			break;
	}
}
