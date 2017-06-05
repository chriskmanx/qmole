/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*-
 * gtksourcegutterrenderertext.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
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

#include "gtksourcegutterrenderertext.h"
#include "gtksourceview-i18n.h"

#define GTK_SOURCE_GUTTER_RENDERER_TEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), GTK_SOURCE_TYPE_GUTTER_RENDERER_TEXT, GtkSourceGutterRendererTextPrivate))

struct _GtkSourceGutterRendererTextPrivate
{
	gchar *text;

	PangoLayout *cached_layout;
	PangoAttribute *fg_attr;
	PangoAttrList *cached_attr_list;

	guint is_markup : 1;
};

G_DEFINE_TYPE (GtkSourceGutterRendererText, gtk_source_gutter_renderer_text, GTK_SOURCE_TYPE_GUTTER_RENDERER)

enum
{
	PROP_0,
	PROP_MARKUP,
	PROP_TEXT
};

static void
create_layout (GtkSourceGutterRendererText *renderer,
               GtkWidget                   *widget)
{
	PangoLayout *layout;
	PangoAttribute *attr;
	GtkStyleContext *context;
	GdkRGBA color;
	PangoAttrList *attr_list;

	layout = gtk_widget_create_pango_layout (widget, NULL);

	context = gtk_widget_get_style_context (widget);
	gtk_style_context_get_color (context, GTK_STATE_FLAG_NORMAL, &color);

	attr = pango_attr_foreground_new (color.red * 65535,
	                                  color.green * 65535,
	                                  color.blue * 65535);

	attr->start_index = 0;
	attr->end_index = G_MAXINT;

	attr_list = pango_attr_list_new ();
	pango_attr_list_insert (attr_list, attr);

	renderer->priv->fg_attr = attr;
	renderer->priv->cached_layout = layout;
	renderer->priv->cached_attr_list = attr_list;
}

static void
gutter_renderer_text_begin (GtkSourceGutterRenderer      *renderer,
                            cairo_t                      *cr,
                            GdkRectangle                 *background_area,
                            GdkRectangle                 *cell_area,
                            GtkTextIter                  *start,
                            GtkTextIter                  *end)
{
	GtkSourceGutterRendererText *text;

	text = GTK_SOURCE_GUTTER_RENDERER_TEXT (renderer);

	create_layout (text, GTK_WIDGET (gtk_source_gutter_renderer_get_view (renderer)));
}

static void
center_on (GtkSourceGutterRenderer *renderer,
           GdkRectangle            *cell_area,
           GtkTextIter             *iter,
           gint                     width,
           gint                     height,
           gfloat                   xalign,
           gfloat                   yalign,
           gint                    *x,
           gint                    *y)
{
	GdkRectangle location;
	GtkTextView *view;

	view = gtk_source_gutter_renderer_get_view (renderer);

	gtk_text_view_get_iter_location (view, iter, &location);

	*x = cell_area->x + (cell_area->width - width) * xalign;
	*y = cell_area->y + (location.height - height) * yalign;
}

static void
gutter_renderer_text_draw (GtkSourceGutterRenderer      *renderer,
                           cairo_t                      *cr,
                           GdkRectangle                 *background_area,
                           GdkRectangle                 *cell_area,
                           GtkTextIter                  *start,
                           GtkTextIter                  *end,
                           GtkSourceGutterRendererState  state)
{
	GtkSourceGutterRendererText *text;
	gint width;
	gint height;
	PangoAttrList *attr_list;
	gfloat xalign;
	gfloat yalign;
	GtkSourceGutterRendererAlignmentMode mode;
	GtkTextView *view;
	gint x = 0;
	gint y = 0;
	GtkStyleContext *context;

	/* Chain up to draw background */
	GTK_SOURCE_GUTTER_RENDERER_CLASS (
		gtk_source_gutter_renderer_text_parent_class)->draw (renderer,
		                                                     cr,
		                                                     background_area,
		                                                     cell_area,
		                                                     start,
		                                                     end,
		                                                     state);

	text = GTK_SOURCE_GUTTER_RENDERER_TEXT (renderer);
	view = gtk_source_gutter_renderer_get_view (renderer);

	if (text->priv->is_markup)
	{
		pango_layout_set_markup (text->priv->cached_layout,
		                         text->priv->text,
		                         -1);
	}
	else
	{
		pango_layout_set_text (text->priv->cached_layout,
		                       text->priv->text,
		                       -1);
	}

	attr_list = pango_layout_get_attributes (text->priv->cached_layout);

	if (!attr_list)
	{
		pango_layout_set_attributes (text->priv->cached_layout,
		                             pango_attr_list_copy (text->priv->cached_attr_list));
	}
	else
	{
		pango_attr_list_insert (attr_list,
		                        pango_attribute_copy (text->priv->fg_attr));
	}

	pango_layout_get_size (text->priv->cached_layout, &width, &height);

	width /= PANGO_SCALE;
	height /= PANGO_SCALE;

	gtk_source_gutter_renderer_get_alignment (renderer,
	                                          &xalign,
	                                          &yalign);

	mode = gtk_source_gutter_renderer_get_alignment_mode (renderer);

	switch (mode)
	{
		case GTK_SOURCE_GUTTER_RENDERER_ALIGNMENT_MODE_CELL:
			x = cell_area->x + (cell_area->width - width) * xalign;
			y = cell_area->y + (cell_area->height - height) * yalign;
		break;
		case GTK_SOURCE_GUTTER_RENDERER_ALIGNMENT_MODE_FIRST:
			center_on (renderer,
			           cell_area,
			           start,
			           width,
			           height,
			           xalign,
			           yalign,
			           &x,
			           &y);
		break;
		case GTK_SOURCE_GUTTER_RENDERER_ALIGNMENT_MODE_LAST:
			center_on (renderer,
			           cell_area,
			           end,
			           width,
			           height,
			           xalign,
			           yalign,
			           &x,
			           &y);
		break;
	}

	context = gtk_widget_get_style_context (GTK_WIDGET (view));
	gtk_render_layout (context, cr, x, y, text->priv->cached_layout);
}

static void
gutter_renderer_text_end (GtkSourceGutterRenderer *renderer)
{
	GtkSourceGutterRendererText *text;

	text = GTK_SOURCE_GUTTER_RENDERER_TEXT (renderer);

	g_object_unref (text->priv->cached_layout);
	text->priv->cached_layout = NULL;

	pango_attr_list_unref (text->priv->cached_attr_list);
	text->priv->cached_attr_list = NULL;

	text->priv->fg_attr = NULL;
}

static void
measure_text (GtkSourceGutterRendererText *renderer,
              const gchar                 *markup,
              const gchar                 *text,
              gint                        *width,
              gint                        *height)
{
	PangoLayout *layout;
	gint w;
	gint h;
	GtkSourceGutterRenderer *r;
	GtkTextView *view;

	r = GTK_SOURCE_GUTTER_RENDERER (renderer);
	view = gtk_source_gutter_renderer_get_view (r);

	layout = gtk_widget_create_pango_layout (GTK_WIDGET (view), NULL);

	if (markup)
	{
		pango_layout_set_markup (layout,
		                         markup,
		                         -1);
	}
	else
	{
		pango_layout_set_text (layout,
		                       text,
		                       -1);
	}

	pango_layout_get_size (layout, &w, &h);

	if (width)
	{
		*width = w / PANGO_SCALE;
	}

	if (height)
	{
		*height = h / PANGO_SCALE;
	}

	g_object_unref (layout);
}

void
gtk_source_gutter_renderer_text_measure (GtkSourceGutterRendererText *renderer,
                                         const gchar                 *text,
                                         gint                        *width,
                                         gint                        *height)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER_TEXT (renderer));
	g_return_if_fail (text != NULL);

	measure_text (renderer, NULL, text, width, height);
}

void
gtk_source_gutter_renderer_text_measure_markup (GtkSourceGutterRendererText *renderer,
                                                const gchar                 *markup,
                                                gint                        *width,
                                                gint                        *height)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER_TEXT (renderer));
	g_return_if_fail (markup != NULL);

	measure_text (renderer, markup, NULL, width, height);
}

static void
gtk_source_gutter_renderer_text_finalize (GObject *object)
{
	GtkSourceGutterRendererText *renderer = GTK_SOURCE_GUTTER_RENDERER_TEXT (object);

	g_free (renderer->priv->text);

	G_OBJECT_CLASS (gtk_source_gutter_renderer_text_parent_class)->finalize (object);
}

static void
set_text (GtkSourceGutterRendererText *renderer,
          const gchar                 *text,
          gint                         length,
          gboolean                     is_markup)
{
	g_free (renderer->priv->text);

	renderer->priv->text = length >= 0 ? g_strndup (text, length) : g_strdup (text);
	renderer->priv->is_markup = is_markup;
}

static void
gtk_source_gutter_renderer_text_set_property (GObject      *object,
                                              guint         prop_id,
                                              const GValue *value,
                                              GParamSpec   *pspec)
{
	GtkSourceGutterRendererText *renderer;

	renderer = GTK_SOURCE_GUTTER_RENDERER_TEXT (object);

	switch (prop_id)
	{
		case PROP_MARKUP:
			set_text (renderer, g_value_get_string (value), -1, TRUE);
			break;
		case PROP_TEXT:
			set_text (renderer, g_value_get_string (value), -1, FALSE);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_gutter_renderer_text_get_property (GObject    *object,
                                              guint       prop_id,
                                              GValue     *value,
                                              GParamSpec *pspec)
{
	GtkSourceGutterRendererText *renderer;

	renderer = GTK_SOURCE_GUTTER_RENDERER_TEXT (object);

	switch (prop_id)
	{
		case PROP_MARKUP:
			g_value_set_string (value, renderer->priv->is_markup ? renderer->priv->text : NULL);
			break;
		case PROP_TEXT:
			g_value_set_string (value, !renderer->priv->is_markup ? renderer->priv->text : NULL);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_gutter_renderer_text_class_init (GtkSourceGutterRendererTextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GtkSourceGutterRendererClass *renderer_class = GTK_SOURCE_GUTTER_RENDERER_CLASS (klass);

	object_class->finalize = gtk_source_gutter_renderer_text_finalize;

	object_class->get_property = gtk_source_gutter_renderer_text_get_property;
	object_class->set_property = gtk_source_gutter_renderer_text_set_property;

	renderer_class->begin = gutter_renderer_text_begin;
	renderer_class->draw = gutter_renderer_text_draw;
	renderer_class->end = gutter_renderer_text_end;

	g_type_class_add_private (object_class, sizeof (GtkSourceGutterRendererTextPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_MARKUP,
	                                 g_param_spec_string ("markup",
	                                                      _("Markup"),
	                                                      _("The markup"),
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_TEXT,
	                                 g_param_spec_string ("text",
	                                                      _("Text"),
	                                                      _("The text"),
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
gtk_source_gutter_renderer_text_init (GtkSourceGutterRendererText *self)
{
	self->priv = GTK_SOURCE_GUTTER_RENDERER_TEXT_GET_PRIVATE (self);

	self->priv->is_markup = TRUE;
}

/**
 * gtk_source_gutter_renderer_text_new:
 *
 * Create a new #GtkSourceGutterRendererText.
 *
 * Returns: (transfer full): A #GtkSourceGutterRenderer
 *
 **/
GtkSourceGutterRenderer *
gtk_source_gutter_renderer_text_new ()
{
	return g_object_new (GTK_SOURCE_TYPE_GUTTER_RENDERER_TEXT, NULL);
}

void
gtk_source_gutter_renderer_text_set_markup (GtkSourceGutterRendererText *renderer,
                                            const gchar                 *markup,
                                            gint                         length)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER_TEXT (renderer));

	set_text (renderer, markup, length, TRUE);
}

void
gtk_source_gutter_renderer_text_set_text (GtkSourceGutterRendererText *renderer,
                                          const gchar                 *text,
                                          gint                         length)
{
	g_return_if_fail (GTK_SOURCE_IS_GUTTER_RENDERER_TEXT (renderer));

	set_text (renderer, text, length, FALSE);
}
