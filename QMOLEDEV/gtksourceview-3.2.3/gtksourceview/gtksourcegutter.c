/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- /
 * gtksourcegutter.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2009 - Jesse van den Kieboom
 *
 * GtkSourceView is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GtkSourceView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "gtksourcegutter-private.h"
#include "gtksourceview.h"
#include "gtksourceview-i18n.h"
#include "gtksourceview-marshal.h"
#include "gtksourcegutterrenderer.h"
#include "gtksourcegutterrenderer-private.h"

#include <gobject/gvaluecollector.h>

/**
 * SECTION:gutter
 * @Short_description: Gutter object for #GtkSourceView
 * @Title: GtkSourceGutter
 * @See_also:#GtkSourceView
 *
 * The #GtkSourceGutter object represents the left and right gutters of the text
 * view. It is used by #GtkSourceView to draw the line numbers and category
 * marks that might be present on a line. By packing additional #GtkSourceGutterRenderer
 * objects in the gutter, you can extend the gutter with your own custom 
 * drawings.
 *
 * The gutter works very much the same way as cells rendered in a #GtkTreeView.
 * The concept is similar, with the exception that the gutter does not have an
 * underlying #GtkTreeModel. The builtin line number renderer is at position
 * #GTK_SOURCE_VIEW_GUTTER_POSITION_LINES (-30) and the marks renderer is at
 * #GTK_SOURCE_VIEW_GUTTER_POSITION_MARKS (-20). You can use these values to
 * position custom renderers accordingly.
 *
 */

#define GTK_SOURCE_GUTTER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_SOURCE_TYPE_GUTTER, GtkSourceGutterPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_VIEW,
	PROP_WINDOW_TYPE,
	PROP_XPAD,
	PROP_YPAD
};

typedef struct
{
	GtkSourceGutterRenderer *renderer;

	gint prelit;
	gint position;

	guint queue_draw_handler;
	guint size_changed_handler;
	guint notify_xpad_handler;
	guint notify_ypad_handler;
	guint notify_visible_handler;
} Renderer;

enum
{
	DRAW,
	MOTION_NOTIFY_EVENT,
	BUTTON_PRESS_EVENT,
	ENTER_NOTIFY_EVENT,
	LEAVE_NOTIFY_EVENT,
	QUERY_TOOLTIP_EVENT,
	REALIZE,
	STYLE_UPDATED,
	LAST_EXTERNAL_SIGNAL
};

struct _GtkSourceGutterPrivate
{
	GtkSourceView *view;
	GtkTextWindowType window_type;
	GtkOrientation orientation;

	GList *renderers;

	gint xpad;
	gint ypad;

	guint signals[LAST_EXTERNAL_SIGNAL];

	guint is_drawing : 1;
};

G_DEFINE_TYPE (GtkSourceGutter, gtk_source_gutter, G_TYPE_OBJECT)

static gboolean on_view_draw (GtkSourceView   *view,
                              cairo_t         *cr,
                              GtkSourceGutter *gutter);

static gboolean on_view_motion_notify_event (GtkSourceView   *view,
                                             GdkEventMotion  *event,
                                             GtkSourceGutter *gutter);


static gboolean on_view_enter_notify_event (GtkSourceView    *view,
                                            GdkEventCrossing *event,
                                            GtkSourceGutter  *gutter);

static gboolean on_view_leave_notify_event (GtkSourceView    *view,
                                            GdkEventCrossing *event,
                                            GtkSourceGutter  *gutter);

static gboolean on_view_button_press_event (GtkSourceView    *view,
                                            GdkEventButton   *event,
                                            GtkSourceGutter  *gutter);

static gboolean on_view_query_tooltip (GtkSourceView   *view,
                                       gint             x,
                                       gint             y,
                                       gboolean         keyboard_mode,
                                       GtkTooltip      *tooltip,
                                       GtkSourceGutter *gutter);

static void on_view_style_updated (GtkSourceView    *view,
                                   GtkSourceGutter  *gutter);

static void do_redraw (GtkSourceGutter *gutter);
static void update_gutter_size (GtkSourceGutter *gutter);

static void
on_renderer_size_changed (GtkSourceGutterRenderer *renderer,
                          GParamSpec              *spec,
                          GtkSourceGutter         *gutter)
{
	update_gutter_size (gutter);
}

static void
on_renderer_queue_draw (GtkSourceGutterRenderer *renderer,
                        GtkSourceGutter         *gutter)
{
	do_redraw (gutter);
}

static void
on_renderer_notify_padding (GtkSourceGutterRenderer *renderer,
                            GParamSpec              *spec,
                            GtkSourceGutter         *gutter)
{
	update_gutter_size (gutter);
}

static void
on_renderer_notify_visible (GtkSourceGutterRenderer *renderer,
                            GParamSpec              *spec,
                            GtkSourceGutter         *gutter)
{
	update_gutter_size (gutter);
}

static Renderer *
renderer_new (GtkSourceGutter         *gutter,
              GtkSourceGutterRenderer *renderer,
              gint                     position)
{
	Renderer *ret = g_slice_new0 (Renderer);

	ret->renderer = g_object_ref_sink (renderer);
	ret->position = position;
	ret->prelit = -1;

	_gtk_source_gutter_renderer_set_view (renderer,
	                                      GTK_TEXT_VIEW (gutter->priv->view),
	                                      gutter->priv->window_type);

	ret->size_changed_handler =
		g_signal_connect (ret->renderer,
		                  "notify::size",
		                  G_CALLBACK (on_renderer_size_changed),
		                  gutter);

	ret->queue_draw_handler =
		g_signal_connect (ret->renderer,
		                  "queue-draw",
		                  G_CALLBACK (on_renderer_queue_draw),
		                  gutter);

	ret->notify_xpad_handler =
		g_signal_connect (ret->renderer,
		                  "notify::xpad",
		                  G_CALLBACK (on_renderer_notify_padding),
		                  gutter);

	ret->notify_ypad_handler =
		g_signal_connect (ret->renderer,
		                  "notify::ypad",
		                  G_CALLBACK (on_renderer_notify_padding),
		                  gutter);

	ret->notify_visible_handler =
		g_signal_connect (ret->renderer,
		                  "notify::visible",
		                  G_CALLBACK (on_renderer_notify_visible),
		                  gutter);

	return ret;
}

static void
renderer_free (Renderer *renderer)
{
	g_signal_handler_disconnect (renderer->renderer,
	                             renderer->queue_draw_handler);

	g_signal_handler_disconnect (renderer->renderer,
	                             renderer->size_changed_handler);

	g_signal_handler_disconnect (renderer->renderer,
	                             renderer->notify_xpad_handler);

	g_signal_handler_disconnect (renderer->renderer,
	                             renderer->notify_ypad_handler);

	g_signal_handler_disconnect (renderer->renderer,
	                             renderer->notify_visible_handler);

	_gtk_source_gutter_renderer_set_view (renderer->renderer,
	                                      NULL,
	                                      GTK_TEXT_WINDOW_PRIVATE);

	g_object_unref (renderer->renderer);
	g_slice_free (Renderer, renderer);
}

static void
gtk_source_gutter_finalize (GObject *object)
{
	G_OBJECT_CLASS (gtk_source_gutter_parent_class)->finalize (object);
}

static void
gtk_source_gutter_dispose (GObject *object)
{
	GtkSourceGutter *gutter = GTK_SOURCE_GUTTER (object);
	gint i;

	g_list_free_full (gutter->priv->renderers, (GDestroyNotify)renderer_free);

	if (gutter->priv->view)
	{
		for (i = 0; i < LAST_EXTERNAL_SIGNAL; ++i)
		{
			g_signal_handler_disconnect (gutter->priv->view,
				                     gutter->priv->signals[i]);
		}

		gutter->priv->view = NULL;
	}

	gutter->priv->renderers = NULL;
}

static void
gtk_source_gutter_get_property (GObject    *object,
                                guint       prop_id,
                                GValue     *value,
                                GParamSpec *pspec)
{
	GtkSourceGutter *self = GTK_SOURCE_GUTTER (object);

	switch (prop_id)
	{
		case PROP_VIEW:
			g_value_set_object (value, self->priv->view);
			break;
		case PROP_WINDOW_TYPE:
			g_value_set_enum (value, self->priv->window_type);
			break;
		case PROP_XPAD:
			g_value_set_int (value, self->priv->xpad);
			break;
		case PROP_YPAD:
			g_value_set_int (value, self->priv->ypad);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
on_view_realize (GtkSourceView   *view,
                 GtkSourceGutter *gutter)
{
	update_gutter_size (gutter);
}

static void
set_view (GtkSourceGutter *gutter,
          GtkSourceView   *view)
{
	gutter->priv->view = view;

	gutter->priv->signals[DRAW] =
		g_signal_connect (view,
		                  "draw",
		                  G_CALLBACK (on_view_draw),
		                  gutter);

	gutter->priv->signals[MOTION_NOTIFY_EVENT] =
		g_signal_connect (view,
		                  "motion-notify-event",
		                  G_CALLBACK (on_view_motion_notify_event),
		                  gutter);

	gutter->priv->signals[ENTER_NOTIFY_EVENT] =
		g_signal_connect (view,
		                  "enter-notify-event",
		                  G_CALLBACK (on_view_enter_notify_event),
		                  gutter);

	gutter->priv->signals[LEAVE_NOTIFY_EVENT] =
		g_signal_connect (view,
		                  "leave-notify-event",
		                  G_CALLBACK (on_view_leave_notify_event),
		                  gutter);

	gutter->priv->signals[BUTTON_PRESS_EVENT] =
		g_signal_connect (view,
		                  "button-press-event",
		                  G_CALLBACK (on_view_button_press_event),
		                  gutter);

	gutter->priv->signals[QUERY_TOOLTIP_EVENT] =
		g_signal_connect (view,
		                  "query-tooltip",
		                  G_CALLBACK (on_view_query_tooltip),
		                  gutter);

	gutter->priv->signals[REALIZE] =
		g_signal_connect (view,
		                  "realize",
		                  G_CALLBACK (on_view_realize),
		                  gutter);

	gutter->priv->signals[STYLE_UPDATED] =
		g_signal_connect (view,
		                  "style-updated",
		                  G_CALLBACK (on_view_style_updated),
		                  gutter);
}

static void
do_redraw (GtkSourceGutter *gutter)
{
	GdkWindow *window;

	window = gtk_text_view_get_window (GTK_TEXT_VIEW (gutter->priv->view),
	                                   gutter->priv->window_type);

	if (window && !gutter->priv->is_drawing)
	{
		gdk_window_invalidate_rect (window, NULL, FALSE);
	}
}

static gint
calculate_gutter_size (GtkSourceGutter  *gutter,
                       GArray           *sizes)
{
	GList *item;
	gint total_width = 0;

	/* Calculate size */
	for (item = gutter->priv->renderers; item; item = g_list_next (item))
	{
		Renderer *renderer = item->data;
		gint width;

		if (!gtk_source_gutter_renderer_get_visible (renderer->renderer))
		{
			width = 0;
		}
		else
		{
			gint xpad;
			gint size;

			size = gtk_source_gutter_renderer_get_size (renderer->renderer);

			gtk_source_gutter_renderer_get_padding (renderer->renderer,
			                                        &xpad,
			                                        NULL);

			width = size + 2 * xpad;
		}

		if (sizes)
		{
			g_array_append_val (sizes, width);
		}

		total_width += width + gutter->priv->xpad;
	}

	if (gutter->priv->renderers)
	{
		total_width += gutter->priv->xpad;
	}

	return total_width;
}

static void
update_gutter_size (GtkSourceGutter *gutter)
{
	gint width;

	width = calculate_gutter_size (gutter, NULL);

	gtk_text_view_set_border_window_size (GTK_TEXT_VIEW (gutter->priv->view),
	                                      gutter->priv->window_type,
	                                      width);
}

static gboolean
set_padding (GtkSourceGutter *gutter,
             gint            *field,
             gint             padding,
             const gchar     *name,
             gboolean         resize)
{
	if (*field == padding || padding < 0)
	{
		return FALSE;
	}

	*field = padding;

	g_object_notify (G_OBJECT (gutter), name);

	if (resize)
	{
		update_gutter_size (gutter);
	}

	return TRUE;
}

static gboolean
set_xpad (GtkSourceGutter *gutter,
          gint             xpad,
          gboolean         resize)
{
	return set_padding (gutter, &gutter->priv->xpad, xpad, "xpad", resize);
}

static gboolean
set_ypad (GtkSourceGutter *gutter,
          gint             ypad,
          gboolean         resize)
{
	return set_padding (gutter, &gutter->priv->ypad, ypad, "ypad", resize);
}

static void
gtk_source_gutter_set_property (GObject       *object,
                                guint          prop_id,
                                const GValue  *value,
                                GParamSpec    *pspec)
{
	GtkSourceGutter *self = GTK_SOURCE_GUTTER (object);

	switch (prop_id)
	{
		case PROP_VIEW:
			set_view (self, GTK_SOURCE_VIEW (g_value_get_object (value)));
			break;
		case PROP_WINDOW_TYPE:
			self->priv->window_type = g_value_get_enum (value);
			break;
		case PROP_XPAD:
			set_xpad (self, g_value_get_int (value), TRUE);
			break;
		case PROP_YPAD:
			set_ypad (self, g_value_get_int (value), TRUE);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_gutter_constructed (GObject *object)
{
	GtkSourceGutter *gutter;

	gutter = GTK_SOURCE_GUTTER (object);

	if (gutter->priv->window_type == GTK_TEXT_WINDOW_LEFT ||
	    gutter->priv->window_type == GTK_TEXT_WINDOW_RIGHT)
	{
		gutter->priv->orientation = GTK_ORIENTATION_HORIZONTAL;
	}
	else
	{
		gutter->priv->orientation = GTK_ORIENTATION_VERTICAL;
	}

	G_OBJECT_CLASS (gtk_source_gutter_parent_class)->constructed (object);
}

static void
gtk_source_gutter_class_init (GtkSourceGutterClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->set_property = gtk_source_gutter_set_property;
	object_class->get_property = gtk_source_gutter_get_property;

	object_class->finalize = gtk_source_gutter_finalize;
	object_class->dispose = gtk_source_gutter_dispose;
	object_class->constructed = gtk_source_gutter_constructed;

	/**
	 * GtkSourceGutter:view:
	 *
	 * The #GtkSourceView of the gutter
	 */
	g_object_class_install_property (object_class,
	                                 PROP_VIEW,
	                                 g_param_spec_object ("view",
	                                                      _("View"),
	                                                      _("The gutters' GtkSourceView"),
	                                                      GTK_SOURCE_TYPE_VIEW,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GtkSourceGutter:window-type:
	 *
	 * The text window type on which the window is placed
	 */
	g_object_class_install_property (object_class,
	                                 PROP_WINDOW_TYPE,
	                                 g_param_spec_enum ("window_type",
	                                                    _("Window Type"),
	                                                    _("The gutters text window type"),
	                                                    GTK_TYPE_TEXT_WINDOW_TYPE,
	                                                    0,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (object_class, sizeof(GtkSourceGutterPrivate));


	g_object_class_install_property (object_class,
	                                 PROP_XPAD,
	                                 g_param_spec_int ("xpad",
	                                                   _("X Padding"),
	                                                   _("The x-padding"),
	                                                   -1,
	                                                   G_MAXINT,
	                                                   0,
	                                                   G_PARAM_READWRITE | G_PARAM_CONSTRUCT));


	g_object_class_install_property (object_class,
	                                 PROP_YPAD,
	                                 g_param_spec_int ("ypad",
	                                                   _("Y Padding"),
	                                                   _("The y-padding"),
	                                                   -1,
	                                                   G_MAXINT,
	                                                   0,
	                                                   G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
gtk_source_gutter_init (GtkSourceGutter *self)
{
	self->priv = GTK_SOURCE_GUTTER_GET_PRIVATE (self);
}

static gint
sort_by_position (Renderer *r1,
                  Renderer *r2,
                  gpointer  data)
{
	if (r1->position < r2->position)
	{
		return -1;
	}
	else if (r1->position > r2->position)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

static void
append_renderer (GtkSourceGutter *gutter,
                 Renderer        *renderer)
{
	gutter->priv->renderers =
		g_list_insert_sorted_with_data (gutter->priv->renderers,
		                                renderer,
		                                (GCompareDataFunc)sort_by_position,
		                                NULL);

	update_gutter_size (gutter);
}

GtkSourceGutter *
gtk_source_gutter_new (GtkSourceView     *view,
                       GtkTextWindowType  type)
{
	return g_object_new (GTK_SOURCE_TYPE_GUTTER,
	                     "view", view,
	                     "window_type", type,
	                     NULL);
}

/* Public API */

/**
 * gtk_source_gutter_get_window:
 * @gutter: a #GtkSourceGutter.
 *
 * Get the #GdkWindow of the gutter. The window will only be available when the
 * gutter has at least one, non-zero width, cell renderer packed.
 *
 * Returns: (transfer none): the #GdkWindow of the gutter, or %NULL
 * if the gutter has no window.
 *
 * Since: 2.8
 */
GdkWindow *
gtk_source_gutter_get_window (GtkSourceGutter *gutter)
{
	g_return_val_if_fail (GTK_SOURCE_IS_GUTTER (gutter), NULL);
	g_return_val_if_fail (gutter->priv->view != NULL, NULL);

	return gtk_text_view_get_window (GTK_TEXT_VIEW (gutter->priv->view),
	                                 gutter->priv->window_type);
}

/**
 * gtk_source_gutter_insert:
 * @gutter: a #GtkSourceGutter.
 * @renderer: a gutter renderer (must inherit from #GtkSourceGutterRenderer).
 * @position: the renderer position.
 *
 * Insert @renderer into the gutter. If @renderer is yet unowned then gutter
 * claims its ownership. Otherwise just increases renderer's reference count.
 * @renderer cannot be already inserted to another gutter.
 *
 * Returns: %TRUE if operation succeeded. Otherwise %FALSE.
 *
 * Since: 3.0
 *
 **/

gboolean
gtk_source_gutter_insert (GtkSourceGutter         *gutter,
                          GtkSourceGutterRenderer *renderer,
                          gint                     position)
{
	Renderer* internal_renderer;

	g_return_val_if_fail (GTK_SOURCE_IS_GUTTER (gutter), FALSE);
	g_return_val_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER (renderer), FALSE);
	g_return_val_if_fail (gtk_source_gutter_renderer_get_view (renderer) == NULL, FALSE);
	g_return_val_if_fail (gtk_source_gutter_renderer_get_window_type (renderer) == GTK_TEXT_WINDOW_PRIVATE, FALSE);

	internal_renderer = renderer_new (gutter, renderer, position);
	append_renderer (gutter, internal_renderer);

	return TRUE;
}

static gboolean
renderer_find (GtkSourceGutter          *gutter,
               GtkSourceGutterRenderer  *renderer,
               Renderer                **ret,
               GList                   **retlist)
{
	GList *list;

	for (list = gutter->priv->renderers; list; list = g_list_next (list))
	{
		*ret = list->data;

		if ((*ret)->renderer == renderer)
		{
			if (retlist)
			{
				*retlist = list;
			}

			return TRUE;
		}
	}

	return FALSE;
}

/**
 * gtk_source_gutter_reorder:
 * @gutter: a #GtkSourceGutterRenderer.
 * @renderer: a #GtkCellRenderer.
 * @position: the new renderer position.
 *
 * Reorders @renderer in @gutter to new @position.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_reorder (GtkSourceGutter         *gutter,
                           GtkSourceGutterRenderer *renderer,
                           gint                     position)
{
	Renderer *ret;
	GList *retlist;

	g_return_if_fail (GTK_SOURCE_IS_GUTTER (gutter));
	g_return_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER (renderer));

	if (renderer_find (gutter, renderer, &ret, &retlist))
	{
		gutter->priv->renderers =
			g_list_delete_link (gutter->priv->renderers,
			                    retlist);

		ret->position = position;
		append_renderer (gutter, ret);
	}
}

/**
 * gtk_source_gutter_remove:
 * @gutter: a #GtkSourceGutter.
 * @renderer: a #GtkSourceGutterRenderer.
 *
 * Removes @renderer from @gutter.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_remove (GtkSourceGutter         *gutter,
                          GtkSourceGutterRenderer *renderer)
{
	Renderer *ret;
	GList *retlist;

	g_return_if_fail (GTK_SOURCE_IS_GUTTER (gutter));
	g_return_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER (renderer));

	if (renderer_find (gutter, renderer, &ret, &retlist))
	{
		gutter->priv->renderers =
			g_list_delete_link (gutter->priv->renderers,
			                    retlist);

		update_gutter_size (gutter);
		renderer_free (ret);
	}
}

/**
 * gtk_source_gutter_queue_draw:
 * @gutter: a #GtkSourceGutter.
 *
 * Invalidates the drawable area of the gutter. You can use this to force a
 * redraw of the gutter if something has changed and needs to be redrawn.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_queue_draw (GtkSourceGutter *gutter)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER (gutter));

	do_redraw (gutter);
}

/* This function is taken from gtk+/tests/testtext.c */
static gint
get_lines (GtkTextView  *text_view,
           gint          first_y,
           gint          last_y,
           GArray       *buffer_coords,
           GArray       *line_heights,
           GArray       *numbers,
           gint         *countp,
           GtkTextIter  *start,
           GtkTextIter  *end)
{
	GtkTextIter iter;
	gint count;
	gint last_line_num = -1;
	gint total_height = 0;

	g_array_set_size (buffer_coords, 0);
	g_array_set_size (numbers, 0);

	if (line_heights != NULL)
	{
		g_array_set_size (line_heights, 0);
	}

	/* Get iter at first y */
	gtk_text_view_get_line_at_y (text_view, &iter, first_y, NULL);

	/* For each iter, get its location and add it to the arrays.
	 * Stop when we pass last_y */
	count = 0;

	*start = iter;

	while (!gtk_text_iter_is_end (&iter))
	{
		gint y, height;

		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		g_array_append_val (buffer_coords, y);

		if (line_heights)
		{
			g_array_append_val (line_heights, height);
		}

		total_height += height;

		last_line_num = gtk_text_iter_get_line (&iter);
		g_array_append_val (numbers, last_line_num);

		++count;

		if ((y + height) >= last_y)
		{
			break;
		}

		gtk_text_iter_forward_line (&iter);
	}

	if (gtk_text_iter_is_end (&iter))
	{
		gint y, height;
		gint line_num;

		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		line_num = gtk_text_iter_get_line (&iter);

		if (line_num != last_line_num)
		{
			g_array_append_val (buffer_coords, y);

			if (line_heights)
			{
				g_array_append_val (line_heights, height);
			}

			total_height += height;

			g_array_append_val (numbers, line_num);
			++count;
		}
	}

	*countp = count;

	if (count == 0)
	{
		gint y = 0;
		gint n = 0;
		gint height;

		*countp = 1;

		g_array_append_val (buffer_coords, y);
		g_array_append_val (numbers, n);

		if (line_heights)
		{
			gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);
			g_array_append_val (line_heights, height);

			total_height += height;
		}
	}

	*end = iter;

	return total_height;
}

static gboolean
on_view_draw (GtkSourceView   *view,
              cairo_t         *cr,
              GtkSourceGutter *gutter)
{
	GdkWindow *window;
	GtkTextView *text_view;
	GArray *sizes;
	GdkRectangle clip;
	gint x, y;
	gint y1, y2;
	GArray *numbers;
	GArray *pixels;
	GArray *heights;
	GtkTextIter cur;
	gint cur_line;
	gint count;
	gint i;
	GList *item;
	GtkTextIter start;
	GtkTextIter end;
	GtkTextBuffer *buffer;
	GdkRectangle background_area;
	GdkRectangle cell_area;
	GtkTextIter selection_start;
	GtkTextIter selection_end;
	gboolean has_selection;
	gint idx;
	GtkStyleContext *style_context;
	GdkRGBA fg_color;

	window = gtk_source_gutter_get_window (gutter);

	if (window == NULL || !gtk_cairo_should_draw_window (cr, window))
	{
		return FALSE;
	}

	gtk_cairo_transform_to_window (cr, GTK_WIDGET (view), window);

	text_view = GTK_TEXT_VIEW (view);

	if (!gdk_cairo_get_clip_rectangle (cr, &clip))
	{
		return FALSE;
	}

	gutter->priv->is_drawing = TRUE;

	buffer = gtk_text_view_get_buffer (text_view);

	gdk_window_get_pointer (window, &x, &y, NULL);

	y1 = clip.y;
	y2 = y1 + clip.height;

	/* get the extents of the line printing */
	gtk_text_view_window_to_buffer_coords (text_view,
	                                       gutter->priv->window_type,
	                                       0,
	                                       y1,
	                                       NULL,
	                                       &y1);

	gtk_text_view_window_to_buffer_coords (text_view,
	                                       gutter->priv->window_type,
	                                       0,
	                                       y2,
	                                       NULL,
	                                       &y2);

	numbers = g_array_new (FALSE, FALSE, sizeof (gint));
	pixels = g_array_new (FALSE, FALSE, sizeof (gint));
	heights = g_array_new (FALSE, FALSE, sizeof (gint));
	sizes = g_array_new (FALSE, FALSE, sizeof (gint));

	calculate_gutter_size (gutter, sizes);

	i = 0;
	x = 0;

	background_area.x = 0;
	background_area.height = get_lines (text_view,
	                                    y1,
	                                    y2,
	                                    pixels,
	                                    heights,
	                                    numbers,
	                                    &count,
	                                    &start,
	                                    &end);

	cell_area.x = gutter->priv->xpad;
	cell_area.height = background_area.height;

	gtk_text_view_buffer_to_window_coords (text_view,
	                                       gutter->priv->window_type,
	                                       0,
	                                       g_array_index (pixels, gint, 0),
	                                       NULL,
	                                       &background_area.y);

	cell_area.y = background_area.y;

	item = gutter->priv->renderers;
	idx = 0;

	style_context = gtk_widget_get_style_context (GTK_WIDGET (view));

	gtk_style_context_get_color (style_context,
	                             gtk_widget_get_state (GTK_WIDGET (view)),
	                             &fg_color);

	gdk_cairo_set_source_rgba (cr, &fg_color);

	while (item)
	{
		Renderer *renderer = item->data;
		gint xpad;
		gint width;

		width = g_array_index (sizes, gint, idx++);

		if (gtk_source_gutter_renderer_get_visible (renderer->renderer))
		{
			gtk_source_gutter_renderer_get_padding (renderer->renderer,
			                                        &xpad,
			                                        NULL);

			background_area.width = width;

			cell_area.width = width - 2 * xpad;
			cell_area.x = background_area.x + xpad;

			cairo_save (cr);

			gdk_cairo_rectangle (cr, &background_area);
			cairo_clip (cr);

			gtk_source_gutter_renderer_begin (renderer->renderer,
			                                  cr,
			                                  &background_area,
			                                  &cell_area,
			                                  &start,
			                                  &end);

			cairo_restore (cr);

			background_area.x += background_area.width;
		}

		item = g_list_next (item);
	}

	gtk_text_buffer_get_iter_at_mark (buffer,
	                                  &cur,
	                                  gtk_text_buffer_get_insert (buffer));

	cur_line = gtk_text_iter_get_line (&cur);

	gtk_text_buffer_get_selection_bounds (buffer,
	                                      &selection_start,
	                                      &selection_end);

	has_selection = !gtk_text_iter_equal (&selection_start, &selection_end);

	if (has_selection)
	{
		if (!gtk_text_iter_starts_line (&selection_start))
		{
			gtk_text_iter_set_line_offset (&selection_start, 0);
		}

		if (!gtk_text_iter_ends_line (&selection_end))
		{
			gtk_text_iter_forward_to_line_end (&selection_end);
		}
	}

	for (i = 0; i < count; ++i)
	{
		gint pos;
		gint line_to_paint;

		end = start;

		if (!gtk_text_iter_ends_line (&end))
		{
			gtk_text_iter_forward_to_line_end (&end);
		}

		gtk_text_view_buffer_to_window_coords (text_view,
		                                       gutter->priv->window_type,
		                                       0,
		                                       g_array_index (pixels, gint, i),
		                                       NULL,
		                                       &pos);

		line_to_paint = g_array_index (numbers, gint, i);

		background_area.y = pos;
		background_area.height = g_array_index (heights, gint, i);
		background_area.x = 0;

		idx = 0;

		for (item = gutter->priv->renderers; item; item = g_list_next (item))
		{
			Renderer *renderer;
			gint width;
			GtkSourceGutterRendererState state;
			gint xpad;
			gint ypad;

			renderer = item->data;
			width = g_array_index (sizes, gint, idx++);

			if (!gtk_source_gutter_renderer_get_visible (renderer->renderer))
			{
				continue;
			}

			gtk_source_gutter_renderer_get_padding (renderer->renderer,
			                                        &xpad,
			                                        &ypad);

			background_area.width = width;

			cell_area.y = background_area.y + ypad;
			cell_area.height = background_area.height - 2 * ypad;

			cell_area.x = background_area.x + xpad;
			cell_area.width = background_area.width - 2 * xpad;

			state = GTK_SOURCE_GUTTER_RENDERER_STATE_NORMAL;

			if (line_to_paint == cur_line)
			{
				state |= GTK_SOURCE_GUTTER_RENDERER_STATE_CURSOR;
			}

			if (has_selection &&
			    gtk_text_iter_in_range (&start,
			                            &selection_start,
			                            &selection_end))
			{
				state |= GTK_SOURCE_GUTTER_RENDERER_STATE_SELECTED;
			}

			if (renderer->prelit >= 0 && cell_area.y <= renderer->prelit && cell_area.y + cell_area.height >= renderer->prelit)
			{
				state |= GTK_SOURCE_GUTTER_RENDERER_STATE_PRELIT;
			}

			gtk_source_gutter_renderer_query_data (renderer->renderer,
			                                       &start,
			                                       &end,
			                                       state);

			cairo_save (cr);

			gdk_cairo_rectangle (cr, &background_area);

			cairo_clip (cr);

			/* Call render with correct area */
			gtk_source_gutter_renderer_draw (renderer->renderer,
			                                 cr,
			                                 &background_area,
			                                 &cell_area,
			                                 &start,
			                                 &end,
			                                 state);

			cairo_restore (cr);

			background_area.x += background_area.width;
		}

		gtk_text_iter_forward_line (&start);
	}

	for (item = gutter->priv->renderers; item; item = g_list_next (item))
	{
		Renderer *renderer = item->data;

		if (gtk_source_gutter_renderer_get_visible (renderer->renderer))
		{
			gtk_source_gutter_renderer_end (renderer->renderer);
		}
	}

	g_array_free (numbers, TRUE);
	g_array_free (pixels, TRUE);
	g_array_free (heights, TRUE);

	g_array_free (sizes, TRUE);

	gutter->priv->is_drawing = FALSE;

	return FALSE;
}

static Renderer *
renderer_at_x (GtkSourceGutter *gutter,
               gint             x,
               gint            *start,
               gint            *width)
{
	GList *item;
	gint s;
	gint w;

	update_gutter_size (gutter);

	s = 0;

	for (item = gutter->priv->renderers; item; item = g_list_next (item))
	{
		Renderer *renderer = item->data;
		gint xpad;

		if (!gtk_source_gutter_renderer_get_visible (renderer->renderer))
		{
			continue;
		}

		w = gtk_source_gutter_renderer_get_size (renderer->renderer);

		gtk_source_gutter_renderer_get_padding (renderer->renderer,
		                                        &xpad,
		                                        NULL);

		s += xpad;

		if (w > 0 && x >= s && x < s + w)
		{
			if (width)
			{
				*width = w;
			}

			if (start)
			{
				*start = s;
			}

			return renderer;
		}

		s += w + xpad;
	}

	return NULL;
}

static void
get_renderer_rect (GtkSourceGutter *gutter,
                   Renderer        *renderer,
                   GtkTextIter     *iter,
                   gint             line,
                   GdkRectangle    *rectangle,
                   gint             start)
{
	gint y;
	gint ypad;

	rectangle->x = start;

	gtk_text_view_get_line_yrange (GTK_TEXT_VIEW (gutter->priv->view),
	                               iter,
	                               &y,
	                               &rectangle->height);

	rectangle->width = gtk_source_gutter_renderer_get_size (renderer->renderer);

	gtk_text_view_buffer_to_window_coords (GTK_TEXT_VIEW (gutter->priv->view),
	                                       gutter->priv->window_type,
	                                       0,
	                                       y,
	                                       NULL,
	                                       &rectangle->y);

	gtk_source_gutter_renderer_get_padding (renderer->renderer,
	                                        NULL,
	                                        &ypad);

	rectangle->y += ypad;
	rectangle->height -= 2 * ypad;
}

static gboolean
renderer_query_activatable (GtkSourceGutter *gutter,
                            Renderer        *renderer,
                            GdkEvent        *event,
                            gint             x,
                            gint             y,
                            GtkTextIter     *line_iter,
                            GdkRectangle    *rect,
                            gint             start)
{
	gint y_buf;
	gint yline;
	GtkTextIter iter;
	GdkRectangle r;

	if (!renderer)
	{
		return FALSE;
	}

	gtk_text_view_window_to_buffer_coords (GTK_TEXT_VIEW (gutter->priv->view),
	                                       gutter->priv->window_type,
	                                       x,
	                                       y,
	                                       NULL,
	                                       &y_buf);

	gtk_text_view_get_line_at_y (GTK_TEXT_VIEW (gutter->priv->view),
	                             &iter,
	                             y_buf,
	                             &yline);

	if (yline > y_buf)
	{
		return FALSE;
	}

	get_renderer_rect (gutter, renderer, &iter, yline, &r, start);

	if (line_iter)
	{
		*line_iter = iter;
	}

	if (rect)
	{
		*rect = r;
	}

	if (y < r.y || y > r.y + r.height)
	{
		return FALSE;
	}

	return gtk_source_gutter_renderer_query_activatable (renderer->renderer,
	                                                     &iter,
	                                                     &r,
	                                                     event);
}

static gboolean
redraw_for_window (GtkSourceGutter *gutter,
                   GdkEventAny     *event,
                   gboolean         act_on_window,
                   gint             x,
                   gint             y)
{
	Renderer *at_x = NULL;
	GList *item;
	gboolean redraw;
	gint start;

	if (event->window != gtk_source_gutter_get_window (gutter) && act_on_window)
	{
		return FALSE;
	}

	if (act_on_window)
	{
		at_x = renderer_at_x (gutter, x, &start, NULL);
	}

	redraw = FALSE;

	for (item = gutter->priv->renderers; item; item = g_list_next (item))
	{
		Renderer *renderer = item->data;
		gint prelit = renderer->prelit;

		if (!gtk_source_gutter_renderer_get_visible (renderer->renderer))
		{
			renderer->prelit = -1;
		}
		else
		{
			if (renderer != at_x || !act_on_window)
			{
				renderer->prelit = -1;
			}
			else if (renderer_query_activatable (gutter,
			                                     renderer,
			                                     (GdkEvent *)event,
			                                     x,
			                                     y,
			                                     NULL,
			                                     NULL,
			                                     start))
			{
				renderer->prelit = y;
			}
			else
			{
				renderer->prelit = -1;
			}
		}

		redraw |= (renderer->prelit != prelit);
	}

	if (redraw)
	{
		do_redraw (gutter);
	}

	return FALSE;
}

static gboolean
on_view_motion_notify_event (GtkSourceView    *view,
                             GdkEventMotion   *event,
                             GtkSourceGutter  *gutter)
{
	return redraw_for_window (gutter,
	                          (GdkEventAny *)event,
	                          TRUE,
	                          (gint)event->x,
	                          (gint)event->y);
}

static gboolean
on_view_enter_notify_event (GtkSourceView     *view,
                            GdkEventCrossing  *event,
                            GtkSourceGutter   *gutter)
{
	return redraw_for_window (gutter,
	                          (GdkEventAny *)event,
	                          TRUE,
	                          (gint)event->x,
	                          (gint)event->y);
}

static gboolean
on_view_leave_notify_event (GtkSourceView     *view,
                            GdkEventCrossing  *event,
                            GtkSourceGutter   *gutter)
{
	return redraw_for_window (gutter,
	                          (GdkEventAny *)event,
	                          FALSE,
	                          (gint)event->x,
	                          (gint)event->y);
}

static gboolean
on_view_button_press_event (GtkSourceView    *view,
                            GdkEventButton   *event,
                            GtkSourceGutter  *gutter)
{
	Renderer *renderer;
	GtkTextIter line_iter;
	gint start = -1;
	GdkRectangle rect;

	if (event->window != gtk_source_gutter_get_window (gutter))
	{
		return FALSE;
	}

	if (event->type != GDK_BUTTON_PRESS)
	{
		return FALSE;
	}

	/* Check cell renderer */
	renderer = renderer_at_x (gutter, event->x, &start, NULL);

	if (renderer_query_activatable (gutter,
	                                renderer,
	                                (GdkEvent *)event,
	                                (gint)event->x,
	                                (gint)event->y,
	                                &line_iter,
	                                &rect,
	                                start))
	{
		gtk_source_gutter_renderer_activate (renderer->renderer,
		                                     &line_iter,
		                                     &rect,
		                                     (GdkEvent *)event);

		do_redraw (gutter);

		return TRUE;
	}

	return FALSE;
}

static gboolean
on_view_query_tooltip (GtkSourceView   *view,
                       gint             x,
                       gint             y,
                       gboolean         keyboard_mode,
                       GtkTooltip      *tooltip,
                       GtkSourceGutter *gutter)
{
	GtkTextView *text_view = GTK_TEXT_VIEW (view);
	Renderer *renderer;
	gint start = 0;
	gint width = 0;
	gint y_buf;
	gint yline;
	GtkTextIter line_iter;
	GdkRectangle rect;

	if (keyboard_mode)
	{
		return FALSE;
	}

	/* Check cell renderer */
	renderer = renderer_at_x (gutter, x, &start, &width);

	if (!renderer)
	{
		return FALSE;
	}

	gtk_text_view_window_to_buffer_coords (text_view,
	                                       gutter->priv->window_type,
	                                       x, y,
	                                       NULL, &y_buf);

	gtk_text_view_get_line_at_y (GTK_TEXT_VIEW (view),
	                             &line_iter,
	                             y_buf,
	                             &yline);

	if (yline > y_buf)
	{
		return FALSE;
	}

	get_renderer_rect (gutter,
	                   renderer,
	                   &line_iter,
	                   yline,
	                   &rect,
	                   start);

	return gtk_source_gutter_renderer_query_tooltip (renderer->renderer,
	                                                 &line_iter,
	                                                 &rect,
	                                                 x,
	                                                 y,
	                                                 tooltip);
}

static void
on_view_style_updated (GtkSourceView   *view,
                       GtkSourceGutter *gutter)
{
	gtk_source_gutter_queue_draw (gutter);
}

void
gtk_source_gutter_set_padding (GtkSourceGutter *gutter,
                               gint             xpad,
                               gint             ypad)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER (gutter));

	if (set_xpad (gutter, xpad, FALSE) || set_ypad (gutter, ypad, FALSE))
	{
		update_gutter_size (gutter);
	}
}

void
gtk_source_gutter_get_padding (GtkSourceGutter *gutter,
                               gint            *xpad,
                               gint            *ypad)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER (gutter));

	if (xpad)
	{
		*xpad = gutter->priv->xpad;
	}

	if (ypad)
	{
		*ypad = gutter->priv->ypad;
	}
}

/**
 * gtk_source_gutter_get_renderer_at_pos:
 * @gutter: A #GtkSourceGutter.
 * @x: The x position to get identified.
 * @y: The y position to get identified.
 *
 * Finds the #GtkSourceGutterRenderer at (x, y).
 *
 * Returns: (transfer none): the renderer at (x, y) or %NULL.
 */
GtkSourceGutterRenderer *
gtk_source_gutter_get_renderer_at_pos (GtkSourceGutter *gutter,
                                       gint             x,
                                       gint             y)
{
	Renderer *renderer;

	g_return_val_if_fail (GTK_SOURCE_IS_GUTTER (gutter), NULL);

	renderer = renderer_at_x (gutter, x, NULL, NULL);

	if (renderer == NULL)
	{
		return NULL;
	}

	return renderer->renderer;
}
