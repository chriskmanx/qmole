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

/* wrapped.c - a replacement for GtkLabel that wraps sensibly */

#include "config.h"

#include <gtk/gtk.h>

#include "global.h"

#include "wrapped.h"

static gpointer parent_class = NULL;

/* Static prototypes */
static void wrapped_label_finialize(GObject *object);
static void wrapped_label_class_init(gpointer gclass, gpointer data);
static void wrapped_label_init(GTypeInstance *object, gpointer gclass);
static void wrapped_label_size_request(GtkWidget *widget,
				       GtkRequisition *requisition);
static gint wrapped_label_expose(GtkWidget *widget, GdkEventExpose *event);
static void wrapped_label_style_set(GtkWidget *widget,
				    GtkStyle *previous_style);


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

GtkWidget *wrapped_label_new(const char *text, gint width)
{
	WrappedLabel *wl;

	wl = g_object_new(wrapped_label_get_type(), NULL);
	wl->width = width;
	
	wrapped_label_set_text(wl, text);

	return GTK_WIDGET(wl);
}

void wrapped_label_set_text(WrappedLabel *wl, const char *text)
{
	GtkWidget *widget = GTK_WIDGET(wl);

	if (!wl->layout)
	{
		wl->layout = gtk_widget_create_pango_layout(widget, text);
		pango_layout_set_width(wl->layout, wl->width * PANGO_SCALE);
		pango_layout_set_wrap(wl->layout, PANGO_WRAP_WORD);
		pango_layout_set_alignment(wl->layout, PANGO_ALIGN_CENTER);
	}
	else
		pango_layout_set_text(wl->layout, text, -1);

	gtk_widget_queue_resize(widget);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void wrapped_label_finialize(GObject *object)
{
	WrappedLabel *wl = (WrappedLabel *) object;

	if (wl->layout)
	{
		g_object_unref(G_OBJECT(wl->layout));
		wl->layout = NULL;
	}

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void wrapped_label_class_init(gpointer gclass, gpointer data)
{
	GObjectClass *object = (GObjectClass *) gclass;
	GtkWidgetClass *widget_class = (GtkWidgetClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	object->finalize = wrapped_label_finialize;

	widget_class->size_request = wrapped_label_size_request;
	widget_class->expose_event = wrapped_label_expose;
	widget_class->style_set = wrapped_label_style_set;
}

static void wrapped_label_init(GTypeInstance *object, gpointer gclass)
{
	WrappedLabel *wl = (WrappedLabel *) object;
	GtkWidget *widget = (GtkWidget *) object;

	GTK_WIDGET_SET_FLAGS(widget, GTK_NO_WINDOW);

	wl->layout = NULL;
	wl->width = -1;
	wl->text_width = -1;
}

GType wrapped_label_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (WrappedLabelClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			wrapped_label_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(WrappedLabel),
			0,			/* n_preallocs */
			wrapped_label_init
		};

		type = g_type_register_static(GTK_TYPE_WIDGET, "WrappedLabel",
					      &info, 0);
	}

	return type;
}

static void wrapped_label_size_request(GtkWidget *widget,
				       GtkRequisition *requisition)
{
	PangoRectangle logical_rect;
	WrappedLabel *wl = (WrappedLabel *) widget;

	g_return_if_fail(wl->layout != NULL);
	
	pango_layout_get_extents(wl->layout, NULL, &logical_rect);

	wl->x_off = PANGO_PIXELS(logical_rect.x);
	wl->y_off = PANGO_PIXELS(logical_rect.y);
	wl->text_width = PANGO_PIXELS(logical_rect.width);

	requisition->width = PANGO_PIXELS(logical_rect.width);
	requisition->height = PANGO_PIXELS(logical_rect.height);
}

static gint wrapped_label_expose(GtkWidget *widget, GdkEventExpose *event)
{
	WrappedLabel *wl = (WrappedLabel *) widget;
	int x;

	g_return_val_if_fail(event != NULL, TRUE);
	g_return_val_if_fail(wl->layout != NULL, TRUE);

	x = widget->allocation.x -
		((wl->text_width - widget->allocation.width) >> 1);

	gtk_paint_layout(widget->style,
			 widget->window,
			 GTK_WIDGET_STATE(widget),
			 FALSE,
			 NULL,		/* DON'T clip! */
			 widget,
			 "label",
			 x - wl->x_off,
			 widget->allocation.y - wl->y_off,
			 wl->layout);

	return FALSE;
}

static void wrapped_label_style_set(GtkWidget *widget,
				    GtkStyle *previous_style)
{
	WrappedLabel *wl = (WrappedLabel *) widget;

	if (wl->layout)
	{
		const gchar *txt;
		PangoLayout *old = wl->layout;

		txt = pango_layout_get_text(wl->layout);

		wl->layout = NULL;
		wrapped_label_set_text(wl, txt);

		g_object_unref(old);
	}
}
