/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/*
 * gtksourcegutter.c
 * This file is part of gtksourceview
 *
 * Copyright (C) 2009 - Jesse van den Kieboom
 *
 * gtksourceview is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * gtksourceview is distributed in the hope that it will be useful,
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

/**
 * SECTION:gutter
 * @Short_description: Gutter object for #GtkSourceView
 * @Title: GtkSourceGutter
 * @See_also:#GtkSourceView
 *
 * The #GtkSourceGutter object represents the left and right gutters of the text
 * view. It is used by #GtkSourceView to draw the line numbers and category
 * marks that might be present on a line. By packing additional #GtkCellRenderer
 * objects in the gutter, you can extend the gutter with your own custom 
 * drawings.
 *
 * The gutter works very much the same way as cells rendered in a #GtkTreeView.
 * The concept is similar, with the exception that the gutter does not have an
 * underlying #GtkTreeModel. Instead, you should use
 * #gtk_source_gutter_set_cell_data_func to set a callback to fill in any of the
 * cell renderers properties, given the line for which the cell is to be
 * rendered. Renderers are inserted into the gutter at a certain position. The
 * builtin line number renderer is at position 

 * #GTK_SOURCE_VIEW_GUTTER_POSITION_LINES (-30) and the marks renderer is at
 * #GTK_SOURCE_VIEW_GUTTER_POSITION_MARKS (-20). You can use these values to
 * position custom renderers accordingly. The width of a cell renderer can be
 * specified as either fixed (using

 * #gtk_cell_renderer_set_fixed_size) or dynamic, in which case you
 * <emphasis>must</emphasis> set #gtk_source_gutter_set_cell_size_func. This
 * callback is used to set the properties of the renderer such that
 * #gtk_cell_renderer_get_size yields the maximum width of the cell.
 */

#define GTK_SOURCE_GUTTER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_TYPE_SOURCE_GUTTER, GtkSourceGutterPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_VIEW,
	PROP_WINDOW_TYPE
};

/* Signals */
enum
{
	CELL_ACTIVATED,
	QUERY_TOOLTIP,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = {0,};

typedef struct
{
	GtkCellRenderer *renderer;
	gint position;

	GtkSourceGutterDataFunc data_func;
	gpointer data_func_data;
	GDestroyNotify data_func_destroy;

	GtkSourceGutterSizeFunc size_func;
	gpointer size_func_data;
	GDestroyNotify size_func_destroy;
} Renderer;

enum
{
	EXPOSE_EVENT,
	MOTION_NOTIFY_EVENT,
	BUTTON_PRESS_EVENT,
	ENTER_NOTIFY_EVENT,
	LEAVE_NOTIFY_EVENT,
	QUERY_TOOLTIP_EVENT,
	LAST_EXTERNAL_SIGNAL
};

struct _GtkSourceGutterPrivate
{
	GtkSourceView *view;
	GtkTextWindowType window_type;
	gint size;
	GList *renderers;
	guint signals[LAST_EXTERNAL_SIGNAL];
};

G_DEFINE_TYPE (GtkSourceGutter, gtk_source_gutter, G_TYPE_OBJECT)

static gboolean on_view_expose_event (GtkSourceView   *view,
                                      GdkEventExpose  *event,
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

static void
view_notify (GtkSourceGutter *gutter,
             gpointer         where_the_object_was)
{
	gutter->priv->view = NULL;
}

static Renderer *
renderer_new (GtkCellRenderer  *renderer,
              gint              position)
{
	Renderer *ret = g_slice_new0 (Renderer);

	ret->renderer = g_object_ref_sink (renderer);
	ret->position = position;

	return ret;
}

static void
renderer_free (Renderer *renderer)
{
	if (renderer->data_func_destroy && renderer->data_func_data)
	{
		renderer->data_func_destroy (renderer->data_func_data);
	}

	if (renderer->size_func_destroy && renderer->size_func_data)
	{
		renderer->size_func_destroy (renderer->size_func_data);
	}

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

	g_list_foreach (gutter->priv->renderers, (GFunc)renderer_free, NULL);
	g_list_free (gutter->priv->renderers);

	if (gutter->priv->view)
	{
		for (i = 0; i < LAST_EXTERNAL_SIGNAL; ++i)
		{
			g_signal_handler_disconnect (gutter->priv->view,
				                     gutter->priv->signals[i]);
		}

		g_object_weak_unref (G_OBJECT (gutter->priv->view),
		                     (GWeakNotify)view_notify,
		                     gutter);

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
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
set_view (GtkSourceGutter *gutter,
          GtkSourceView   *view)
{
	gutter->priv->view = view;

	gutter->priv->size = -1;

	g_object_weak_ref (G_OBJECT (view),
	                   (GWeakNotify)view_notify,
	                   gutter);

	gutter->priv->signals[EXPOSE_EVENT] =
		g_signal_connect (view,
	                  "expose-event",
	                  G_CALLBACK (on_view_expose_event),
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
}

static void
do_redraw (GtkSourceGutter *gutter)
{
	GdkWindow *window;

	window = gtk_text_view_get_window (GTK_TEXT_VIEW (gutter->priv->view),
	                                   gutter->priv->window_type);

	if (window)
	{
		gdk_window_invalidate_rect (window, NULL, FALSE);
	}
}

static void
revalidate_size (GtkSourceGutter *gutter)
{
	GdkWindow *window;

	window = gtk_source_gutter_get_window (gutter);

	if (!window && gutter->priv->renderers)
	{
		/* Make window visible by setting its size to minimum size,
		   actual size will be calculated in expose */
		gtk_text_view_set_border_window_size (GTK_TEXT_VIEW (gutter->priv->view),
		                                      gutter->priv->window_type,
		                                      1);
		gutter->priv->size = -1;
	}
	else if (window && !gutter->priv->renderers)
	{
		/* Make window invisible by setting size to 0 */
		gtk_text_view_set_border_window_size (GTK_TEXT_VIEW (gutter->priv->view),
		                                      gutter->priv->window_type,
		                                      0);
	}
	else if (window)
	{
		/* Redraw the window. Actual size will be calculated in expose */
		do_redraw (gutter);
	}
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
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
gtk_source_gutter_class_init (GtkSourceGutterClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->set_property = gtk_source_gutter_set_property;
	object_class->get_property = gtk_source_gutter_get_property;

	object_class->finalize = gtk_source_gutter_finalize;
	object_class->dispose = gtk_source_gutter_dispose;

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
							      GTK_TYPE_SOURCE_VIEW,
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


	/**
	 * GtkSourceGutter::cell-activated:
	 * @gutter: the #GtkSourceGutter
	 * @renderer: the #GtkCellRenderer which was activated
	 * @iter: the #GtkTextIter at which the cell was activated
	 * @event: the #GdkEvent with which the cell was activated
	 *
	 * Emitted when a cell has been activated (for instance when there was
	 * a button press on the cell). The signal is only emitted for cells
	 * that have the #activatable property set to %TRUE.
	 */
	signals [CELL_ACTIVATED] =
		g_signal_new ("cell-activated",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GtkSourceGutterClass, cell_activated),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__OBJECT_BOXED_POINTER,
			      G_TYPE_NONE,
			      3,
			      GTK_TYPE_CELL_RENDERER,
			      GTK_TYPE_TEXT_ITER,
			      GDK_TYPE_EVENT | G_SIGNAL_TYPE_STATIC_SCOPE);

	/**
	 * GtkSourceGutter::query-tooltip:
	 * @gutter: the #GtkSourceGutter
	 * @renderer: the #GtkCellRenderer which was activated
	 * @iter: the #GtkTextIter at which the cell was activated
	 * @tooltip: the #GtkTooltip
	 *
	 * Emitted when a tooltip is requested for a specific cell. Signal
	 * handlers can return %TRUE to notify the tooltip has been handled.
	 */
	signals [QUERY_TOOLTIP] =
		g_signal_new ("query-tooltip",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GtkSourceGutterClass, query_tooltip),
			      g_signal_accumulator_true_handled,
			      NULL,
			      _gtksourceview_marshal_BOOL__OBJECT_BOXED_OBJECT,
			      G_TYPE_BOOLEAN,
			      3,
			      GTK_TYPE_CELL_RENDERER,
			      GTK_TYPE_TEXT_ITER,
			      GTK_TYPE_TOOLTIP);

	g_type_class_add_private (object_class, sizeof(GtkSourceGutterPrivate));
}

static void
gtk_source_gutter_init (GtkSourceGutter *self)
{
	self->priv = GTK_SOURCE_GUTTER_GET_PRIVATE (self);

	self->priv->size = -1;
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
	revalidate_size (gutter);
}

GtkSourceGutter *
gtk_source_gutter_new (GtkSourceView     *view,
                       GtkTextWindowType  type)
{
	return g_object_new (GTK_TYPE_SOURCE_GUTTER,
	                     "view", view,
	                     "window_type", type,
	                     NULL);
}

/* Public API */

/**
 * gtk_source_gutter_get_window:
 * @gutter: a #GtkSourceGutter
 *
 * Get the #GdkWindow of the gutter. The window will only be available when the
 * gutter has at least one, non-zero width, cell renderer packed.
 *
 * Returns: the #GdkWindow of the gutter, or %NULL if the gutter has no window.
 *
 * Since: 2.8
 */
GdkWindow *
gtk_source_gutter_get_window (GtkSourceGutter *gutter)
{
	g_return_val_if_fail (GTK_IS_SOURCE_GUTTER (gutter), NULL);
	g_return_val_if_fail (gutter->priv->view != NULL, NULL);

	return gtk_text_view_get_window (GTK_TEXT_VIEW (gutter->priv->view),
	                                 gutter->priv->window_type);
}

/**
 * gtk_source_gutter_insert:
 * @gutter: a #GtkSourceGutter
 * @renderer: a #GtkCellRenderer
 * @position: the renderers position
 *
 * Inserts @renderer into @gutter at @position.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_insert (GtkSourceGutter *gutter,
                          GtkCellRenderer *renderer,
                          gint             position)
{
	g_return_if_fail (GTK_IS_SOURCE_GUTTER (gutter));
	g_return_if_fail (GTK_IS_CELL_RENDERER (renderer));

	append_renderer (gutter, renderer_new (renderer, position));
}

static gboolean
renderer_find (GtkSourceGutter  *gutter,
               GtkCellRenderer  *renderer,
               Renderer        **ret,
               GList           **retlist)
{
	GList *list;

	for (list = gutter->priv->renderers; list; list = g_list_next (list))
	{
		*ret = (Renderer *)list->data;

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
 * @gutter: a #GtkSourceGutter
 * @renderer: a #GtkCellRenderer
 * @position: the new renderer position
 *
 * Reorders @renderer in @gutter to new @position.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_reorder (GtkSourceGutter *gutter,
                           GtkCellRenderer *renderer,
                           gint             position)
{
	Renderer *ret;
	GList *retlist;

	g_return_if_fail (GTK_IS_SOURCE_GUTTER (gutter));
	g_return_if_fail (GTK_IS_CELL_RENDERER (renderer));

	if (renderer_find (gutter, renderer, &ret, &retlist))
	{
		gutter->priv->renderers = g_list_remove_link (gutter->priv->renderers, retlist);
		ret->position = position;
		append_renderer (gutter, ret);
	}
}

/**
 * gtk_source_gutter_remove:
 * @gutter: a #GtkSourceGutter
 * @renderer: a #GtkCellRenderer
 *
 * Removes @renderer from @gutter.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_remove (GtkSourceGutter *gutter,
                          GtkCellRenderer *renderer)
{
	Renderer *ret;
	GList *retlist;

	g_return_if_fail (GTK_IS_SOURCE_GUTTER (gutter));
	g_return_if_fail (GTK_IS_CELL_RENDERER (renderer));

	if (renderer_find (gutter, renderer, &ret, &retlist))
	{
		gutter->priv->renderers = g_list_remove_link (gutter->priv->renderers, retlist);

		revalidate_size (gutter);
		renderer_free (ret);
	}
}

/**
 * gtk_source_gutter_set_cell_data_func:
 * @gutter: a #GtkSourceGutter
 * @renderer: a #GtkCellRenderer
 * @func: the #GtkSourceGutterDataFunc to use
 * @func_data: the user data for @func
 * @destroy: the destroy notification for @func_data
 *
 * Sets the #GtkSourceGutterDataFunc to use for @renderer. This function is
 * used to setup the cell renderer properties for rendering the current cell.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_set_cell_data_func (GtkSourceGutter         *gutter,
                                      GtkCellRenderer         *renderer,
                                      GtkSourceGutterDataFunc  func,
                                      gpointer                 func_data,
                                      GDestroyNotify           destroy)
{
	Renderer *ret;

	g_return_if_fail (GTK_IS_SOURCE_GUTTER (gutter));
	g_return_if_fail (GTK_IS_CELL_RENDERER (renderer));

	if (!renderer_find (gutter, renderer, &ret, NULL))
	{
		return;
	}

	if (ret->data_func_data && ret->data_func_destroy)
	{
		ret->data_func_destroy (ret->data_func_data);
	}

	ret->data_func = func;
	ret->data_func_data = func_data;
	ret->data_func_destroy = destroy;

	revalidate_size (gutter);
}

/**
 * gtk_source_gutter_set_cell_size_func:
 * @gutter: a #GtkSourceGutter
 * @renderer: a #GtkCellRenderer
 * @func: the #GtkSourceGutterSizeFunc to use
 * @func_data: the user data for @func
 * @destroy: the destroy notification for @func_data
 *
 * Sets the #GtkSourceGutterSizeFunc to use for @renderer. This function is
 * used to setup the cell renderer properties for measuring the maximum size
 * of the cell.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_set_cell_size_func (GtkSourceGutter         *gutter,
                                      GtkCellRenderer         *renderer,
                                      GtkSourceGutterSizeFunc  func,
                                      gpointer                 func_data,
                                      GDestroyNotify           destroy)
{
	Renderer *ret;

	g_return_if_fail (GTK_IS_SOURCE_GUTTER (gutter));
	g_return_if_fail (GTK_IS_CELL_RENDERER (renderer));

	if (!renderer_find (gutter, renderer, &ret, NULL))
	{
		return;
	}

	if (ret->size_func_data && ret->size_func_destroy)
	{
		ret->size_func_destroy (ret->size_func_data);
	}

	ret->size_func = func;
	ret->size_func_data = func_data;
	ret->size_func_destroy = destroy;

	revalidate_size (gutter);
}

/**
 * gtk_source_gutter_queue_draw:
 * @gutter: a #GtkSourceGutter
 *
 * Invalidates the drawable area of the gutter. You can use this to force a
 * redraw of the gutter if something has changed and needs to be redrawn.
 *
 * Since: 2.8
 */
void
gtk_source_gutter_queue_draw (GtkSourceGutter *gutter)
{
	g_return_if_fail (GTK_IS_SOURCE_GUTTER (gutter));

	revalidate_size (gutter);
}

static gint
calculate_size (GtkSourceGutter  *gutter,
                Renderer         *renderer)
{
	gint width = -1;

	gtk_cell_renderer_get_fixed_size (renderer->renderer, &width, NULL);

	if (width == -1 && renderer->size_func)
	{
		gint height;
		renderer->size_func (gutter, renderer->renderer, renderer->size_func_data);

		gtk_cell_renderer_get_size (renderer->renderer,
		                            GTK_WIDGET (gutter->priv->view),
		                            NULL, NULL, NULL,
		                            &width, &height);
	}

	return width == -1 ? 1 : width;
}

static gint
calculate_sizes (GtkSourceGutter  *gutter,
                 GArray           *sizes)
{
	GList *item;
	gint total_width = 0;

	/* Calculate size */
	for (item = gutter->priv->renderers; item; item = g_list_next (item))
	{
		Renderer *renderer = (Renderer *)item->data;
		gint width;

		width = calculate_size (gutter, renderer);
		g_array_append_val (sizes, width);

		total_width += width;
	}

	return total_width;
}

/* This function is taken from gtk+/tests/testtext.c */
static void
get_lines (GtkTextView  *text_view,
           gint          first_y,
           gint          last_y,
           GArray       *buffer_coords,
           GArray       *line_heights,
           GArray       *numbers,
           gint         *countp)
{
	GtkTextIter iter;
	gint count;
	gint size;
      	gint last_line_num = -1;

	g_array_set_size (buffer_coords, 0);
	g_array_set_size (numbers, 0);

	if (line_heights != NULL)
		g_array_set_size (line_heights, 0);

	/* Get iter at first y */
	gtk_text_view_get_line_at_y (text_view, &iter, first_y, NULL);

	/* For each iter, get its location and add it to the arrays.
	 * Stop when we pass last_y */
	count = 0;
  	size = 0;

  	while (!gtk_text_iter_is_end (&iter))
    	{
		gint y, height;

		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		g_array_append_val (buffer_coords, y);

		if (line_heights)
		{
			g_array_append_val (line_heights, height);
		}

		last_line_num = gtk_text_iter_get_line (&iter);
		g_array_append_val (numbers, last_line_num);

		++count;

		if ((y + height) >= last_y)
			break;

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
		}
	}
}

static gboolean
on_view_expose_event (GtkSourceView   *view,
                      GdkEventExpose  *event,
                      GtkSourceGutter *gutter)
{
	GdkWindow *window;
	GtkTextView *text_view;
	GArray *sizes;
	gint size;

	window = gtk_source_gutter_get_window (gutter);

	if (window == NULL || event->window != window)
	{
		return FALSE;
	}

	text_view = GTK_TEXT_VIEW (view);
	sizes = g_array_new (FALSE, FALSE, sizeof (gint));

	/* This is fairly ugly, but we could not find a better way to
	 * do it: renderers could have changed size and they do not have
	 * a way to signal it. So on expose we revalidate the size and
	 * requeue another expose.
	 * To see if the size has changed we test the size of only the
	 * gutter itself since the full border window size is not under
	 * our control (see e.g bug #589382).
	 * This also means that if the user manually forces the
	 * border_window_size to a value smaller than the gutter, things
	 * will not be drawn properly. */
	size = calculate_sizes (gutter, sizes);
	if (gutter->priv->size != size)
	{

		gint border_size;

		border_size = gtk_text_view_get_border_window_size (text_view, gutter->priv->window_type);

		if (gutter->priv->size < 0)
			border_size += size;
		else
			border_size = MAX (0, border_size - gutter->priv->size) + size;

		gutter->priv->size = size;

		/* Will trigger a new expose */
		gtk_text_view_set_border_window_size (text_view, gutter->priv->window_type, border_size);
	}
	else
	{
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

		gdk_window_get_pointer (window, &x, &y, NULL);

		y1 = event->area.y;
		y2 = y1 + event->area.height;

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

		/* get the line numbers and y coordinates. */
		get_lines (text_view, y1, y2, pixels, heights, numbers, &count);

		gtk_text_buffer_get_iter_at_mark (text_view->buffer,
						  &cur,
						  gtk_text_buffer_get_insert (text_view->buffer));

		cur_line = gtk_text_iter_get_line (&cur);

		for (i = 0; i < count; ++i)
		{
			gint pos;
			gint line_to_paint;
			GdkRectangle cell_area;
			gint idx = 0;

			gtk_text_view_buffer_to_window_coords (text_view,
							       gutter->priv->window_type,
							       0,
							       g_array_index (pixels, gint, i),
							       NULL,
							       &pos);

			line_to_paint = g_array_index (numbers, gint, i);

			cell_area.x = 0;
			cell_area.y = pos;
			cell_area.height = g_array_index (heights, gint, i);

			for (item = gutter->priv->renderers; item; item = g_list_next (item))
			{
				Renderer *renderer = (Renderer *)item->data;
				gint width = g_array_index (sizes, gint, idx++);
				GtkCellRendererState state = 0;

				cell_area.width = width;

				/* Call data func if needed */
				if (renderer->data_func)
				{
					renderer->data_func (gutter,
						             renderer->renderer,
						             line_to_paint,
						             line_to_paint == cur_line,
						             renderer->data_func_data);
				}

				if (x >= cell_area.x && x < cell_area.x + cell_area.width &&
				    y >= cell_area.y && y < cell_area.y + cell_area.height)
				{
					GtkCellRendererMode mode;

					g_object_get (G_OBJECT (renderer->renderer),
						      "mode", &mode,
						      NULL);

					if (mode & GTK_CELL_RENDERER_MODE_ACTIVATABLE)
						state = GTK_CELL_RENDERER_PRELIT;
				}

				/* Call render with correct area */
				gtk_cell_renderer_render (renderer->renderer,
					                  window,
					                  GTK_WIDGET (view),
					                  &cell_area,
					                  &cell_area,
					                  &cell_area,
					                  state);

				cell_area.x += cell_area.width;
			}
		}

		g_array_free (numbers, TRUE);
		g_array_free (pixels, TRUE);
		g_array_free (heights, TRUE);
	}

	g_array_free (sizes, TRUE);

	return FALSE;
}

static gboolean
redraw_for_window (GtkSourceGutter *gutter,
                   GdkEventAny     *event,
                   gboolean         act_on_window)
{
	if (event->window == gtk_source_gutter_get_window (gutter) || !act_on_window)
	{
		gtk_source_gutter_queue_draw (gutter);
	}

	return FALSE;
}

static gboolean
on_view_motion_notify_event (GtkSourceView    *view,
                             GdkEventMotion   *event,
                             GtkSourceGutter  *gutter)
{
	return redraw_for_window (gutter, (GdkEventAny *)event, TRUE);
}


static gboolean
on_view_enter_notify_event (GtkSourceView     *view,
                            GdkEventCrossing  *event,
                            GtkSourceGutter   *gutter)
{
	return redraw_for_window (gutter, (GdkEventAny *)event, TRUE);
}


static gboolean
on_view_leave_notify_event (GtkSourceView     *view,
                            GdkEventCrossing  *event,
                            GtkSourceGutter   *gutter)
{
	return redraw_for_window (gutter, (GdkEventAny *)event, FALSE);
}

static Renderer *
renderer_at_x (GtkSourceGutter *gutter,
               gint             x,
               gint            *start,
               gint            *width)
{
	GList *item;

	for (item = gutter->priv->renderers; item; item = g_list_next (item))
	{
		Renderer *renderer = (Renderer *)item->data;

		*width = calculate_size (gutter, renderer);

		if (x >= *start && x < *start + *width)
		{
			return renderer;
		}

		*start += *width;
	}

	return NULL;
}

static gboolean
on_view_button_press_event (GtkSourceView    *view,
                            GdkEventButton   *event,
                            GtkSourceGutter  *gutter)
{
	Renderer *renderer;
	gint yline;
	GtkTextIter line_iter;
	GtkTextIter cur;
	gint cur_line;
	gint line;
	gint y_buf;
	gint start = 0;
	gint width = 0;
	GtkTextBuffer *buffer;
	GtkCellRendererMode mode;

	if (event->window != gtk_source_gutter_get_window (gutter))
	{
		return FALSE;
	}

	/* Check cell renderer */
	renderer = renderer_at_x (gutter, event->x, &start, &width);

	if (!renderer)
	{
		return FALSE;
	}

	gtk_text_view_window_to_buffer_coords (GTK_TEXT_VIEW (view),
	                                       gutter->priv->window_type,
	                                       event->x, event->y,
	                                       NULL, &y_buf);

	gtk_text_view_get_line_at_y (GTK_TEXT_VIEW (view),
	                             &line_iter,
	                             y_buf,
	                             &yline);

	if (yline > y_buf)
	{
		return FALSE;
	}

	line = gtk_text_iter_get_line (&line_iter);
	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	gtk_text_buffer_get_iter_at_mark (buffer,
					  &cur,
					  gtk_text_buffer_get_insert (buffer));

	cur_line = gtk_text_iter_get_line (&cur);

	if (renderer->data_func)
	{
		renderer->data_func (gutter,
		                     renderer->renderer,
		                     line,
		                     line == cur_line,
		                     renderer->data_func_data);
	}

	g_object_get (G_OBJECT (renderer->renderer),
	              "mode", &mode,
	              NULL);

	if (mode & GTK_CELL_RENDERER_MODE_ACTIVATABLE)
	{
		GdkRectangle area;
		gchar *path;
		gboolean ret;

		gtk_text_view_get_line_yrange (GTK_TEXT_VIEW (view),
		                               &line_iter,
		                               &area.y,
		                               &area.height);
		area.x = start;
		area.width = width;

		path = g_strdup_printf ("%d", line);

		ret = gtk_cell_renderer_activate (renderer->renderer,
		                                  (GdkEvent *)event,
		                                  GTK_WIDGET (gutter->priv->view),
		                                  path,
		                                  &area,
		                                  &area,
		                                  0);

		g_signal_emit (gutter,
		               signals[CELL_ACTIVATED],
		               0,
		               renderer->renderer,
		               &line_iter,
		               (GdkEvent *)event);

		g_free (path);
		do_redraw (gutter);

		return ret;
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
	gboolean ret;

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

	g_signal_emit (gutter,
	               signals[QUERY_TOOLTIP],
	               0,
	               renderer->renderer,
	               &line_iter,
	               tooltip,
	               &ret);

	return ret;
}

/* vi:ts=8 */
