/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/* gtksourcecompletioninfo.c
 * This file is part of gtksourcecompletion
 *
 * Copyright (C) 2007 -2009 Jesús Barbero Rodríguez <chuchiperriman@gmail.com>
 * Copyright (C) 2009 - Jesse van den Kieboom <jessevdk@gnome.org>
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

/**
 * SECTION:completioninfo
 * @title: GtkSourceCompletionInfo
 * @short_description: Calltips object
 *
 * This object can be used to show a calltip or help for the
.* current completion proposal.
 */
  
#include <gtksourceview/gtksourcecompletioninfo.h>
#include "gtksourcecompletionutils.h"
#include "gtksourceview-i18n.h"

#ifndef MIN
#define MIN (a, b) ((a) < (b) ? (a) : (b))
#endif

#ifndef MAX
#define MAX (a, b) ((a) > (b) ? (a) : (b))
#endif

struct _GtkSourceCompletionInfoPrivate
{
	GtkWidget *scroll;
	GtkWidget *widget;
	
	gint max_height;
	gint max_width;
	
	gboolean shrink_height;
	gboolean shrink_width;
	
	guint idle_resize;
	guint request_id;
};

/* Signals */
enum
{
	BEFORE_SHOW,
	LAST_SIGNAL
};

/* Properties */
enum
{
	PROP_0,
	PROP_MAX_WIDTH,
	PROP_MAX_HEIGHT,
	PROP_SHRINK_WIDTH,
	PROP_SHRINK_HEIGHT
};

static guint signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE(GtkSourceCompletionInfo, gtk_source_completion_info, GTK_TYPE_WINDOW);

#define GTK_SOURCE_COMPLETION_INFO_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), GTK_TYPE_SOURCE_COMPLETION_INFO, GtkSourceCompletionInfoPrivate))

static void
get_scrolled_window_sizing (GtkSourceCompletionInfo *info,
                            gint                    *border,
                            gint                    *hscroll,
                            gint                    *vscroll)
{
	GtkWidget *scrollbar;

	*border = 0;
	*hscroll = 0;
	*vscroll = 0;

	if (info->priv->scroll != NULL)
	{
		*border = gtk_container_get_border_width (GTK_CONTAINER (info));
		
		scrollbar = gtk_scrolled_window_get_hscrollbar (GTK_SCROLLED_WINDOW (info->priv->scroll));

		if (GTK_WIDGET_VISIBLE (scrollbar))
		{
			*hscroll = scrollbar->allocation.height;
		}

		scrollbar = gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW (info->priv->scroll));

		if (GTK_WIDGET_VISIBLE (scrollbar))
		{
			*vscroll = scrollbar->allocation.height;
		}
	}
}

static void
window_resize (GtkSourceCompletionInfo *info)
{
	GtkRequisition req;
	gint width;
	gint height;
	gint off;
	gint border;
	gint hscroll;
	gint vscroll;
	GtkStyle *style = GTK_WIDGET (info)->style;

	gtk_window_get_default_size (GTK_WINDOW (info), &width, &height);
	
	if (info->priv->widget != NULL)
	{
		/* Try to resize to fit widget, if necessary */
		gtk_widget_size_request (info->priv->widget, &req);
		
		get_scrolled_window_sizing (info, &border, &hscroll, &vscroll);
		off = (gtk_container_get_border_width (GTK_CONTAINER (info)) + border) * 2;

		if (info->priv->shrink_height)
		{
			if (info->priv->max_height == -1)
			{
				height = req.height + style->ythickness * 2;
			}
			else
			{
				height = MIN (req.height + style->ythickness * 2, info->priv->max_height);
			}
			
			height += off + hscroll;
		}
	
		if (info->priv->shrink_width)
		{
			if (info->priv->max_width == -1)
			{
				width = req.width + style->xthickness * 2;
			}
			else
			{
				width = MIN (req.width + style->xthickness * 2, info->priv->max_width);
			}
			
			width += off + vscroll;
		}
	}
	
	gtk_window_resize (GTK_WINDOW (info), width, height);
}

static void
gtk_source_completion_info_init (GtkSourceCompletionInfo *info)
{
	info->priv = GTK_SOURCE_COMPLETION_INFO_GET_PRIVATE (info);
	
	/* Tooltip style */
	gtk_window_set_title (GTK_WINDOW (info), _("Completion Info"));
	gtk_widget_set_name (GTK_WIDGET (info), "gtk-tooltip");
	gtk_widget_ensure_style (GTK_WIDGET (info));
	
	gtk_window_set_type_hint (GTK_WINDOW (info),
				  GDK_WINDOW_TYPE_HINT_NORMAL);

	gtk_window_set_default_size (GTK_WINDOW (info), 300, 200);
	gtk_container_set_border_width (GTK_CONTAINER (info), 1);
}

static gboolean
idle_resize (GtkSourceCompletionInfo *info)
{
	info->priv->idle_resize = 0;
	
	window_resize (info);
	return FALSE;
}

static void
queue_resize (GtkSourceCompletionInfo *info)
{
	if (info->priv->idle_resize == 0)
	{
		info->priv->idle_resize = g_idle_add ((GSourceFunc)idle_resize, info);
	}
}

static void
gtk_source_completion_info_get_property (GObject    *object, 
                                         guint       prop_id, 
                                         GValue     *value, 
                                         GParamSpec *pspec)
{
	GtkSourceCompletionInfo *info = GTK_SOURCE_COMPLETION_INFO (object);
	
	switch (prop_id)
	{
		case PROP_MAX_WIDTH:
			g_value_set_int (value, info->priv->max_width);
			break;
		case PROP_MAX_HEIGHT:
			g_value_set_int (value, info->priv->max_height);
			break;
		case PROP_SHRINK_WIDTH:
			g_value_set_boolean (value, info->priv->shrink_width);
			break;
		case PROP_SHRINK_HEIGHT:
			g_value_set_boolean (value, info->priv->shrink_height);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_completion_info_set_property (GObject      *object, 
                                         guint         prop_id, 
                                         const GValue *value, 
                                         GParamSpec   *pspec)
{
	GtkSourceCompletionInfo *info = GTK_SOURCE_COMPLETION_INFO (object);
	
	switch (prop_id)
	{
		case PROP_MAX_WIDTH:
			info->priv->max_width = g_value_get_int (value);
			queue_resize (info);
			break;
		case PROP_MAX_HEIGHT:
			info->priv->max_height = g_value_get_int (value);
			queue_resize (info);
			break;
		case PROP_SHRINK_WIDTH:
			info->priv->shrink_width = g_value_get_boolean (value);
			queue_resize (info);
			break;
		case PROP_SHRINK_HEIGHT:
			info->priv->shrink_height = g_value_get_boolean (value);
			queue_resize (info);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
gtk_source_completion_info_finalize (GObject *object)
{
	GtkSourceCompletionInfo *info = GTK_SOURCE_COMPLETION_INFO (object);
	
	if (info->priv->idle_resize != 0)
	{
		g_source_remove (info->priv->idle_resize);
	}
	
	G_OBJECT_CLASS (gtk_source_completion_info_parent_class)->finalize (object);
}

static void
gtk_source_completion_info_show (GtkWidget *widget)
{
	/* First emit BEFORE_SHOW and then chain up */
	g_signal_emit (widget, signals[BEFORE_SHOW], 0);
	
	GTK_WIDGET_CLASS (gtk_source_completion_info_parent_class)->show (widget);	
}

static gboolean
gtk_source_completion_info_expose (GtkWidget      *widget,
                                   GdkEventExpose *expose)
{
	GTK_WIDGET_CLASS (gtk_source_completion_info_parent_class)->expose_event (widget, expose);

	gtk_paint_shadow (widget->style,
			  widget->window,
			  GTK_STATE_NORMAL,
			  GTK_SHADOW_OUT,
			  NULL,
			  widget,
			  NULL,
			  widget->allocation.x,
			  widget->allocation.y,
			  widget->allocation.width,
			  widget->allocation.height);

	return FALSE;
}

static void
gtk_source_completion_info_class_init (GtkSourceCompletionInfoClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
	
	object_class->get_property = gtk_source_completion_info_get_property;
	object_class->set_property = gtk_source_completion_info_set_property;
	object_class->finalize = gtk_source_completion_info_finalize;
	
	widget_class->show = gtk_source_completion_info_show;
	widget_class->expose_event = gtk_source_completion_info_expose;
	
	/**
	 * GtkSourceCompletionInfo::show-info:
	 * @info: The #GscInf who emits the signal
	 *
	 * This signal is emited before any "show" management. You can connect
	 * to this signal if you want to change some properties or position
	 * before to so the real "show".
	 **/
	signals[BEFORE_SHOW] =
		g_signal_new ("before-show",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      0,
			      NULL, 
			      NULL,
			      g_cclosure_marshal_VOID__VOID, 
			      G_TYPE_NONE,
			      0);

	/* Properties */
	g_object_class_install_property (object_class,
					 PROP_MAX_WIDTH,
					 g_param_spec_int ("max-width",
							    _("Maximum width"),
							    _("The maximum allowed width"),
							    -1,
							    G_MAXINT,
							    -1,
							    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
					 PROP_MAX_HEIGHT,
					 g_param_spec_int ("max-height",
							    _("Maximum height"),
							    _("The maximum allowed height"),
							    -1,
							    G_MAXINT,
							    -1,
							    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
							    

	g_object_class_install_property (object_class,
					 PROP_SHRINK_WIDTH,
					 g_param_spec_boolean ("shrink-width",
							       _("Shrink width"),
							       _("Whether the window should shrink width to fit the contents"),
							       TRUE,
							       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
					 PROP_SHRINK_HEIGHT,
					 g_param_spec_boolean ("shrink-height",
							       _("Shrink height"),
							       _("Whether the window should shrink height to fit the contents"),
							       TRUE,
							       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
						       
	g_type_class_add_private (object_class, sizeof (GtkSourceCompletionInfoPrivate));
}

/**
 * gtk_source_completion_info_new:
 *
 * Returns: The new GtkSourceCompletionInfo.
 *
 */
GtkSourceCompletionInfo *
gtk_source_completion_info_new (void)
{
	return g_object_new (GTK_TYPE_SOURCE_COMPLETION_INFO, 
	                     "type", GTK_WINDOW_POPUP, 
	                     NULL);
}

/**
 * gtk_source_completion_info_move_to_iter:
 * @info: A #GtkSourceCompletionInfo
 * @view: A #GtkTextView on which the info window should be positioned
 * @iter: A #GtkTextIter
 *
 * Moves the #GtkSourceCompletionInfo to @iter. If @iter is %NULL @info is 
 * moved to the cursor position. Moving will respect the #GdkGravity setting
 * of the info window and will ensure the line at @iter is not occluded by
 * the window.
 *
 */
void
gtk_source_completion_info_move_to_iter (GtkSourceCompletionInfo *info,
					 GtkTextView             *view,
					 GtkTextIter             *iter)
{
	GtkTextBuffer *buffer;
	GtkTextMark *insert_mark;
	GtkTextIter start;
	
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_INFO (info));
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	
	if (iter == NULL)
	{
		buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
		insert_mark = gtk_text_buffer_get_insert (buffer);
		gtk_text_buffer_get_iter_at_mark (buffer, &start, insert_mark);
	}
	else
	{
		start = *iter;
	}
	
	gtk_source_completion_utils_move_to_iter (GTK_WINDOW (info),
						  GTK_SOURCE_VIEW (view),
						  &start);
}

/**
 * gtk_source_completion_info_set_sizing:
 * @info: A #GtkSourceCompletionInfo
 * @width: The maximum/requested width of the window (-1 to default)
 * @height: The maximum/requested height of the window (-1 to default)
 * @shrink_width: Whether to shrink the width of the window to fit its contents
 * @shrink_height: Whether to shrink the height of the window to fit its
 *                 contents
 *
 * Set sizing information for the info window. If @shrink_width or
 * @shrink_height is %TRUE, the info window will try to resize to fit the
 * window contents, with a maximum size given by @width and @height. Setting
 * @width or @height to -1 removes the maximum size of respectively the width
 * and height of the window.
 *
 */
void
gtk_source_completion_info_set_sizing (GtkSourceCompletionInfo *info,
				       gint                     width,
				       gint                     height,
				       gboolean                 shrink_width,
				       gboolean                 shrink_height)
{
	g_return_if_fail  (GTK_IS_SOURCE_COMPLETION_INFO (info));

	if (info->priv->max_width == width &&
	    info->priv->max_height == height &&
	    info->priv->shrink_width == shrink_width &&
	    info->priv->shrink_height == shrink_height)
	{
		return;
	}

	info->priv->max_width = width;
	info->priv->max_height = height;
	info->priv->shrink_width = shrink_width;
	info->priv->shrink_height = shrink_height;
	
	queue_resize (info);
}

static gboolean
needs_viewport (GtkWidget *widget)
{
	guint id;
	
	id = g_signal_lookup ("set-scroll-adjustments", G_TYPE_FROM_INSTANCE (widget));
	
	return id == 0;
}

static void
widget_size_request_cb (GtkWidget               *widget,
                        GtkRequisition          *requisition,
                        GtkSourceCompletionInfo *info)
{
	queue_resize (info);
}

static gboolean
use_scrolled_window (GtkSourceCompletionInfo *info,
                     GtkWidget               *widget)
{
	GtkRequisition req;
	gint mw;
	gint mh;
	
	mw = info->priv->max_width;
	mh = info->priv->max_height;
	gtk_widget_size_request (widget, &req);
	
	return (mw != -1 && mw < req.width) || (mh != -1 && mh < req.height);
}

static void
create_scrolled_window (GtkSourceCompletionInfo *info)
{
	/* Create scrolled window main widget */
	info->priv->scroll = gtk_scrolled_window_new (NULL, NULL);


	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (info->priv->scroll),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);

	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (info->priv->scroll),
	                                     GTK_SHADOW_NONE);
	gtk_widget_show (info->priv->scroll);
	gtk_container_add (GTK_CONTAINER (info), info->priv->scroll);
}

/**
 * gtk_source_completion_info_set_widget:
 * @info: A #GtkSourceCompletionInfo
 * @widget: A #GtkWidget
 *
 * Sets the content widget of the info window. If @widget does not fit within
 * the size requirements of the window, a #GtkScrolledWindow will automatically
 * be created and added to the window.
 *
 */
void
gtk_source_completion_info_set_widget (GtkSourceCompletionInfo *info,
				       GtkWidget               *widget)
{
	GtkWidget *child;

	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_INFO (info));
	g_return_if_fail (widget == NULL || GTK_IS_WIDGET (widget));

	if (info->priv->widget == widget)
	{
		return;
	}
	
	if (info->priv->widget != NULL)
	{
		g_signal_handler_disconnect (info->priv->widget, info->priv->request_id);
		
		gtk_container_remove (GTK_CONTAINER (gtk_widget_get_parent (info->priv->widget)),
		                      info->priv->widget);

		if (info->priv->scroll != NULL)
		{
			gtk_widget_destroy (info->priv->scroll);
			info->priv->scroll = NULL;
		}
	}
	
	info->priv->widget = widget;
	
	if (widget != NULL)
	{
		/* Keep it alive */
		if (g_object_is_floating (widget))
		{
			g_object_ref (widget);
		}
		
		info->priv->request_id = g_signal_connect_after (widget, 
                                                                 "size-request", 
                                                                 G_CALLBACK (widget_size_request_cb), 
                                                                 info);
		
		/* See if it needs a viewport */
		if (use_scrolled_window (info, widget))
		{
			create_scrolled_window (info);
			child = widget;
			
			if (needs_viewport (widget))
			{
				child = gtk_viewport_new (NULL, NULL);
				gtk_viewport_set_shadow_type (GTK_VIEWPORT (child), GTK_SHADOW_NONE);
				gtk_widget_show (child);

				gtk_container_add (GTK_CONTAINER (child), widget);
			}
			
			gtk_container_add (GTK_CONTAINER (info->priv->scroll), child);
		}
		else
		{
			gtk_container_add (GTK_CONTAINER (info), widget);
		}
		
		gtk_widget_show (widget);
	}

	queue_resize (info);
}

/**
 * gtk_source_completion_info_get_widget:
 * @info: A #GtkSourceCompletionInfo
 *
 * Get the current content widget.
 *
 * Returns: The current content widget.
 *
 */
GtkWidget *
gtk_source_completion_info_get_widget (GtkSourceCompletionInfo* info)
{
	g_return_val_if_fail (GTK_IS_SOURCE_COMPLETION_INFO (info), NULL);

	return info->priv->widget;
}

void
gtk_source_completion_info_process_resize (GtkSourceCompletionInfo *info)
{
	g_return_if_fail (GTK_IS_SOURCE_COMPLETION_INFO (info));
	
	if (info->priv->idle_resize != 0)
		window_resize (info);
}


