/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/* gtksourcecompletioninfo.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2007 -2009 Jesús Barbero Rodríguez <chuchiperriman@gmail.com>
 * Copyright (C) 2009 - Jesse van den Kieboom <jessevdk@gnome.org>
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

struct _GtkSourceCompletionInfoPrivate
{
	GtkWidget *scroll;
	GtkWidget *widget;
};

/* Signals */
enum
{
	BEFORE_SHOW,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE(GtkSourceCompletionInfo, gtk_source_completion_info, GTK_TYPE_WINDOW);

#define GTK_SOURCE_COMPLETION_INFO_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), GTK_SOURCE_TYPE_COMPLETION_INFO, GtkSourceCompletionInfoPrivate))

static void
gtk_source_completion_info_init (GtkSourceCompletionInfo *info)
{
	info->priv = GTK_SOURCE_COMPLETION_INFO_GET_PRIVATE (info);

	/* Tooltip style */
	gtk_window_set_title (GTK_WINDOW (info), _("Completion Info"));
	gtk_widget_set_name (GTK_WIDGET (info), "gtk-tooltip");

	gtk_window_set_type_hint (GTK_WINDOW (info),
	                          GDK_WINDOW_TYPE_HINT_NORMAL);

	gtk_window_set_default_size (GTK_WINDOW (info), 300, 200);
	gtk_container_set_border_width (GTK_CONTAINER (info), 1);

	/* Create scrolled window  */
	info->priv->scroll = gtk_scrolled_window_new (NULL, NULL);

	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (info->priv->scroll),
	                                GTK_POLICY_AUTOMATIC,
	                                GTK_POLICY_AUTOMATIC);

	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (info->priv->scroll),
	                                     GTK_SHADOW_NONE);
	gtk_container_add (GTK_CONTAINER (info), info->priv->scroll);
}

static void
gtk_source_completion_info_show (GtkWidget *widget)
{
	/* First emit BEFORE_SHOW and then chain up */
	g_signal_emit (widget, signals[BEFORE_SHOW], 0);

	GTK_WIDGET_CLASS (gtk_source_completion_info_parent_class)->show (widget);
}

static gboolean
gtk_source_completion_info_draw (GtkWidget *widget,
                                 cairo_t   *cr)
{
	GTK_WIDGET_CLASS (gtk_source_completion_info_parent_class)->draw (widget, cr);

	gtk_render_frame (gtk_widget_get_style_context (widget),
	                  cr,
	                  0, 0,
	                  gtk_widget_get_allocated_width (widget),
	                  gtk_widget_get_allocated_height (widget));

	return FALSE;
}

static void
gtk_source_completion_info_class_init (GtkSourceCompletionInfoClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

	widget_class->show = gtk_source_completion_info_show;
	widget_class->draw = gtk_source_completion_info_draw;

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

	g_type_class_add_private (object_class, sizeof (GtkSourceCompletionInfoPrivate));
}

/**
 * gtk_source_completion_info_new:
 *
 * Returns: a new GtkSourceCompletionInfo.
 */
GtkSourceCompletionInfo *
gtk_source_completion_info_new (void)
{
	return g_object_new (GTK_SOURCE_TYPE_COMPLETION_INFO,
	                     "type", GTK_WINDOW_POPUP,
	                     NULL);
}

/**
 * gtk_source_completion_info_move_to_iter:
 * @info: a #GtkSourceCompletionInfo.
 * @view: a #GtkTextView on which the info window should be positioned.
 * @iter: (allow-none): a #GtkTextIter.
 *
 * Moves the #GtkSourceCompletionInfo to @iter. If @iter is %NULL @info is
 * moved to the cursor position. Moving will respect the #GdkGravity setting
 * of the info window and will ensure the line at @iter is not occluded by
 * the window.
 */
void
gtk_source_completion_info_move_to_iter (GtkSourceCompletionInfo *info,
                                         GtkTextView             *view,
                                         GtkTextIter             *iter)
{
	GtkTextBuffer *buffer;
	GtkTextMark *insert_mark;
	GtkTextIter start;

	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_INFO (info));
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

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
 * gtk_source_completion_info_set_widget:
 * @info: a #GtkSourceCompletionInfo.
 * @widget: (allow-none): a #GtkWidget.
 *
 * Sets the content widget of the info window. If @widget does not fit within
 * the size requirements of the window, a #GtkScrolledWindow will automatically
 * be created and added to the window. See that the previous widget will lose
 * a reference and it can be destroyed, so if you do not want this to happen
 * you must g_object_ref() before calling this method.
 */
void
gtk_source_completion_info_set_widget (GtkSourceCompletionInfo *info,
                                       GtkWidget               *widget)
{
	g_return_if_fail (GTK_SOURCE_IS_COMPLETION_INFO (info));
	g_return_if_fail (widget == NULL || GTK_IS_WIDGET (widget));

	if (info->priv->widget == widget)
	{
		return;
	}

	if (info->priv->widget != NULL)
	{
		gtk_container_remove (GTK_CONTAINER (gtk_widget_get_parent (info->priv->widget)),
		                      info->priv->widget);
	}

	info->priv->widget = widget;

	if (widget != NULL)
	{
		/* See if it needs a viewport */
		if (GTK_IS_SCROLLABLE (widget))
		{
			gtk_container_add (GTK_CONTAINER (info->priv->scroll), widget);
		}
		else
		{
			gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (info->priv->scroll), widget);
		}

		gtk_widget_show_all (info->priv->scroll);
	}
	else
	{
		gtk_widget_hide (info->priv->scroll);
	}
}

/**
 * gtk_source_completion_info_get_widget:
 * @info: a #GtkSourceCompletionInfo.
 *
 * Get the current content widget.
 *
 * Returns: (transfer none): The current content widget.
 */
GtkWidget *
gtk_source_completion_info_get_widget (GtkSourceCompletionInfo* info)
{
	g_return_val_if_fail (GTK_SOURCE_IS_COMPLETION_INFO (info), NULL);

	return info->priv->widget;
}

