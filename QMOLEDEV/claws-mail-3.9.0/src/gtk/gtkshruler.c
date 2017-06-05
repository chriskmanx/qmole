/* LIBGTK - The GTK Library
 * Copyright (C) 1995-1997 Peter Mattis and Spencer Kimball
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include "claws-features.h"

#include <math.h>
#include <string.h>

#include <gtk/gtk.h>

#include "gtkutils.h"
#include "gtkshruler.h"
#include "gtkunit.h"

#define ROUND(x) ((int) ((x) + 0.5))

/**
 * SECTION: gimpparam
 * @title: gimpparam
 * @short_description: Definitions of useful #GParamFlags.
 *
 * Definitions of useful #GParamFlags.
 **/


/**
 * GTK_PARAM_STATIC_STRINGS:
 *
 * Since: GTK 2.4
 **/
#define GTK_PARAM_STATIC_STRINGS (G_PARAM_STATIC_NAME | \
                                   G_PARAM_STATIC_NICK | \
                                   G_PARAM_STATIC_BLURB)

/**
 * GTK_PARAM_READABLE:
 *
 * Since: GTK 2.4
 **/
#define GTK_PARAM_READABLE       (G_PARAM_READABLE    | \
                                   GTK_PARAM_STATIC_STRINGS)

/**
 * GTK_PARAM_WRITABLE:
 *
 * Since: GTK 2.4
 **/
#define GTK_PARAM_WRITABLE       (G_PARAM_WRITABLE    | \
                                   GTK_PARAM_STATIC_STRINGS)

/**
 * GTK_PARAM_READWRITE:
 *
 * Since: GTK 2.4
 **/
#define GTK_PARAM_READWRITE      (G_PARAM_READWRITE   | \
                                   GTK_PARAM_STATIC_STRINGS)


/**
 * SECTION: gimpruler
 * @title: GtkSHRuler
 * @short_description: A ruler widget with configurable unit and orientation.
 *
 * A ruler widget with configurable unit and orientation.
 **/


#define DEFAULT_RULER_FONT_SCALE  PANGO_SCALE_SMALL
#define MINIMUM_INCR              5


enum
{
  PROP_0,
  PROP_ORIENTATION,
  PROP_UNIT,
  PROP_LOWER,
  PROP_UPPER,
  PROP_POSITION,
  PROP_MAX_SIZE
};


/* All distances below are in 1/72nd's of an inch. (According to
 * Adobe that's a point, but points are really 1/72.27 in.)
 */
typedef struct
{
  GtkOrientation   orientation;
  GtkCMUnit        unit;
  gdouble          lower;
  gdouble          upper;
  gdouble          position;
  gdouble          max_size;

  GdkWindow       *input_window;
  cairo_surface_t *backing_store;
  PangoLayout     *layout;
  gdouble          font_scale;

  gint             xsrc;
  gint             ysrc;
} GtkSHRulerPrivate;

#define GTK_SHRULER_GET_PRIVATE(ruler) \
  G_TYPE_INSTANCE_GET_PRIVATE (ruler, GTK_TYPE_SHRULER, GtkSHRulerPrivate)


static const struct
{
  const gdouble  ruler_scale[16];
  const gint     subdivide[3];
} ruler_metric =
{
  { 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000 },
  { 1, 5, 10 }
};


static void          gtk_shruler_dispose       (GObject        *object);
static void          gtk_shruler_set_property  (GObject        *object,
                                               guint            prop_id,
                                               const GValue   *value,
                                               GParamSpec     *pspec);
static void          gtk_shruler_get_property  (GObject        *object,
                                               guint           prop_id,
                                               GValue         *value,
                                               GParamSpec     *pspec);

static void          gtk_shruler_realize       (GtkWidget      *widget);
static void          gtk_shruler_unrealize     (GtkWidget      *widget);
static void          gtk_shruler_map           (GtkWidget      *widget);
static void          gtk_shruler_unmap         (GtkWidget      *widget);
static void          gtk_shruler_size_allocate (GtkWidget      *widget,
                                              GtkAllocation  *allocation);
static void          gtk_shruler_size_request  (GtkWidget      *widget,
                                              GtkRequisition *requisition);
static void          gtk_shruler_style_set     (GtkWidget      *widget,
                                              GtkStyle       *prev_style);
static gboolean      gtk_shruler_motion_notify (GtkWidget      *widget,
                                              GdkEventMotion *event);
static gboolean      gtk_shruler_expose        (GtkWidget      *widget,
                                              GdkEventExpose *event);

static void          gtk_shruler_draw_ticks    (GtkSHRuler      *ruler);
static void          gtk_shruler_make_pixmap   (GtkSHRuler      *ruler);

static PangoLayout * gtk_shruler_get_layout    (GtkWidget      *widget,
                                              const gchar    *text);


G_DEFINE_TYPE (GtkSHRuler, gtk_shruler, GTK_TYPE_WIDGET)

#define parent_class gtk_shruler_parent_class


static void
gtk_shruler_class_init (GtkSHRulerClass *klass)
{
  GObjectClass   *object_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  object_class->dispose             = gtk_shruler_dispose;
  object_class->set_property        = gtk_shruler_set_property;
  object_class->get_property        = gtk_shruler_get_property;

  widget_class->realize             = gtk_shruler_realize;
  widget_class->unrealize           = gtk_shruler_unrealize;
  widget_class->map                 = gtk_shruler_map;
  widget_class->unmap               = gtk_shruler_unmap;
  widget_class->size_allocate       = gtk_shruler_size_allocate;
  widget_class->size_request        = gtk_shruler_size_request;
  widget_class->style_set           = gtk_shruler_style_set;
  widget_class->motion_notify_event = gtk_shruler_motion_notify;
  widget_class->expose_event        = gtk_shruler_expose;

  g_type_class_add_private (object_class, sizeof (GtkSHRulerPrivate));

  g_object_class_install_property (object_class,
                                   PROP_ORIENTATION,
                                   g_param_spec_enum ("orientation",
                                                      "Orientation",
                                                      "The orientation of the ruler",
                                                      GTK_TYPE_ORIENTATION,
                                                      GTK_ORIENTATION_HORIZONTAL,
                                                      GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_LOWER,
                                   gtk_param_spec_unit ("unit",
                                                         "Unit",
                                                         "Unit of ruler",
                                                         TRUE, TRUE,
                                                         CM_UNIT_PIXEL,
                                                         GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_LOWER,
                                   g_param_spec_double ("lower",
                                                        "Lower",
                                                        "Lower limit of ruler",
                                                        -G_MAXDOUBLE,
                                                        G_MAXDOUBLE,
                                                        0.0,
                                                        GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_UPPER,
                                   g_param_spec_double ("upper",
                                                        "Upper",
                                                        "Upper limit of ruler",
                                                        -G_MAXDOUBLE,
                                                        G_MAXDOUBLE,
                                                        0.0,
                                                        GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_POSITION,
                                   g_param_spec_double ("position",
                                                        "Position",
                                                        "Position of mark on the ruler",
                                                        -G_MAXDOUBLE,
                                                        G_MAXDOUBLE,
                                                        0.0,
                                                        GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_MAX_SIZE,
                                   g_param_spec_double ("max-size",
                                                        "Max Size",
                                                        "Maximum size of the ruler",
                                                        -G_MAXDOUBLE,
                                                        G_MAXDOUBLE,
                                                        0.0,
                                                        GTK_PARAM_READWRITE));

  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_double ("font-scale",
                                                                NULL, NULL,
                                                                0.0,
                                                                G_MAXDOUBLE,
                                                                DEFAULT_RULER_FONT_SCALE,
                                                                GTK_PARAM_READABLE));
}

static void
gtk_shruler_init (GtkSHRuler *ruler)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (ruler);

  gtkut_widget_set_has_window (GTK_WIDGET (ruler), FALSE);

  priv->orientation   = GTK_ORIENTATION_HORIZONTAL;
  priv->unit          = GTK_PIXELS;
  priv->lower         = 0;
  priv->upper         = 0;
  priv->position      = 0;
  priv->max_size      = 0;
  priv->backing_store = NULL;
  priv->font_scale    = DEFAULT_RULER_FONT_SCALE;
}

static void
gtk_shruler_dispose (GObject *object)
{
  G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
gtk_shruler_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
  GtkSHRuler        *ruler = GTK_SHRULER (object);
  GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (ruler);

  switch (prop_id)
    {
    case PROP_ORIENTATION:
      priv->orientation = g_value_get_enum (value);
      gtk_widget_queue_resize (GTK_WIDGET (ruler));
      break;

    case PROP_UNIT:
      gtk_shruler_set_unit (ruler, g_value_get_int (value));
      break;

    case PROP_LOWER:
      gtk_shruler_set_range (ruler,
                            g_value_get_double (value),
                            priv->upper,
                            priv->max_size);
      break;
    case PROP_UPPER:
      gtk_shruler_set_range (ruler,
                            priv->lower,
                            g_value_get_double (value),
                            priv->max_size);
      break;

    case PROP_POSITION:
      gtk_shruler_set_position (ruler, g_value_get_double (value));
      break;

    case PROP_MAX_SIZE:
      gtk_shruler_set_range (ruler,
                            priv->lower,
                            priv->upper,
                            g_value_get_double (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
gtk_shruler_get_property (GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
  GtkSHRuler        *ruler = GTK_SHRULER (object);
  GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (ruler);

  switch (prop_id)
    {
    case PROP_ORIENTATION:
      g_value_set_enum (value, priv->orientation);
      break;

    case PROP_UNIT:
      g_value_set_int (value, priv->unit);
      break;

    case PROP_LOWER:
      g_value_set_double (value, priv->lower);
      break;

    case PROP_UPPER:
      g_value_set_double (value, priv->upper);
      break;

    case PROP_POSITION:
      g_value_set_double (value, priv->position);
      break;

    case PROP_MAX_SIZE:
      g_value_set_double (value, priv->max_size);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/**
 * gtk_shruler_new:
 * @orientation: the ruler's orientation.
 *
 * Creates a new ruler.
 *
 * Return value: a new #GtkSHRuler widget.
 *
 * Since: GTK 2.8
 **/
GtkWidget *
gtk_shruler_new (GtkOrientation orientation)
{
  return g_object_new (GTK_TYPE_SHRULER,
                       "orientation", orientation,
                       NULL);
}

static void
gtk_shruler_update_position (GtkSHRuler *ruler,
                            gdouble    x,
                            gdouble    y)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (ruler);
  GtkAllocation     allocation;
  gdouble           lower;
  gdouble           upper;

  gtk_widget_get_allocation (GTK_WIDGET (ruler), &allocation);
  gtk_shruler_get_range (ruler, &lower, &upper, NULL);

  if (priv->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      gtk_shruler_set_position (ruler,
                               lower +
                               (upper - lower) * x / allocation.width);
    }
  else
    {
      gtk_shruler_set_position (ruler,
                               lower +
                               (upper - lower) * y / allocation.height);
    }
}

/**
 * gtk_shruler_set_unit:
 * @ruler: a #GtkSHRuler
 * @unit:  the #GtkCMUnit to set the ruler to
 *
 * This sets the unit of the ruler.
 *
 * Since: GTK 2.8
 */
void
gtk_shruler_set_unit (GtkSHRuler *ruler,
                     GtkCMUnit  unit)
{
  GtkSHRulerPrivate *priv;

  g_return_if_fail (GTK_IS_SHRULER (ruler));

  priv = GTK_SHRULER_GET_PRIVATE (ruler);

  if (priv->unit != unit)
    {
      priv->unit = unit;
      g_object_notify (G_OBJECT (ruler), "unit");

      gtk_widget_queue_draw (GTK_WIDGET (ruler));
    }
}

/**
 * gtk_shruler_get_unit:
 * @ruler: a #GtkSHRuler
 *
 * Return value: the unit currently used in the @ruler widget.
 *
 * Since: GTK 2.8
 **/
GtkCMUnit
gtk_shruler_get_unit (GtkSHRuler *ruler)
{
  g_return_val_if_fail (GTK_IS_SHRULER (ruler), 0);

  return GTK_SHRULER_GET_PRIVATE (ruler)->unit;
}

/**
 * gtk_shruler_set_position:
 * @ruler: a #GtkSHRuler
 * @position: the position to set the ruler to
 *
 * This sets the position of the ruler.
 *
 * Since: GTK 2.8
 */
void
gtk_shruler_set_position (GtkSHRuler *ruler,
                         gdouble    position)
{
  GtkSHRulerPrivate *priv;

  g_return_if_fail (GTK_IS_SHRULER (ruler));

  priv = GTK_SHRULER_GET_PRIVATE (ruler);

  if (priv->position != position)
    {
      priv->position = position;
      g_object_notify (G_OBJECT (ruler), "position");
    }
}

/**
 * gtk_shruler_get_position:
 * @ruler: a #GtkSHRuler
 *
 * Return value: the current position of the @ruler widget.
 *
 * Since: GTK 2.8
 **/
gdouble
gtk_shruler_get_position (GtkSHRuler *ruler)
{
  g_return_val_if_fail (GTK_IS_SHRULER (ruler), 0.0);

  return GTK_SHRULER_GET_PRIVATE (ruler)->position;
}

/**
 * gtk_shruler_set_range:
 * @ruler: a #GtkSHRuler
 * @lower: the lower limit of the ruler
 * @upper: the upper limit of the ruler
 * @max_size: the maximum size of the ruler used when calculating the space to
 * leave for the text
 *
 * This sets the range of the ruler.
 *
 * Since: GTK 2.8
 */
void
gtk_shruler_set_range (GtkSHRuler *ruler,
                      gdouble    lower,
                      gdouble    upper,
                      gdouble    max_size)
{
  GtkSHRulerPrivate *priv;

  g_return_if_fail (GTK_IS_SHRULER (ruler));

  priv = GTK_SHRULER_GET_PRIVATE (ruler);

  g_object_freeze_notify (G_OBJECT (ruler));
  if (priv->lower != lower)
    {
      priv->lower = lower;
      g_object_notify (G_OBJECT (ruler), "lower");
    }
  if (priv->upper != upper)
    {
      priv->upper = upper;
      g_object_notify (G_OBJECT (ruler), "upper");
    }
  if (priv->max_size != max_size)
    {
      priv->max_size = max_size;
      g_object_notify (G_OBJECT (ruler), "max-size");
    }
  g_object_thaw_notify (G_OBJECT (ruler));

  gtk_widget_queue_draw (GTK_WIDGET (ruler));
}

/**
 * gtk_shruler_get_range:
 * @ruler: a #GtkSHRuler
 * @lower: location to store lower limit of the ruler, or %NULL
 * @upper: location to store upper limit of the ruler, or %NULL
 * @max_size: location to store the maximum size of the ruler used when
 *            calculating the space to leave for the text, or %NULL.
 *
 * Retrieves values indicating the range and current position of a #GtkSHRuler.
 * See gtk_shruler_set_range().
 *
 * Since: GTK 2.8
 **/
void
gtk_shruler_get_range (GtkSHRuler *ruler,
                      gdouble   *lower,
                      gdouble   *upper,
                      gdouble   *max_size)
{
  GtkSHRulerPrivate *priv;

  g_return_if_fail (GTK_IS_SHRULER (ruler));

  priv = GTK_SHRULER_GET_PRIVATE (ruler);

  if (lower)
    *lower = priv->lower;
  if (upper)
    *upper = priv->upper;
  if (max_size)
    *max_size = priv->max_size;
}

static void
gtk_shruler_realize (GtkWidget *widget)
{
  GtkSHRuler        *ruler = GTK_SHRULER (widget);
  GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (ruler);
  GtkAllocation     allocation;
  GdkWindowAttr     attributes;
  gint              attributes_mask;

  GTK_WIDGET_CLASS (gtk_shruler_parent_class)->realize (widget);

  gtk_widget_get_allocation (widget, &allocation);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x           = allocation.x;
  attributes.y           = allocation.y;
  attributes.width       = allocation.width;
  attributes.height      = allocation.height;
  attributes.wclass      = GDK_INPUT_ONLY;
  attributes.event_mask  = (gtk_widget_get_events (widget) |
                            GDK_EXPOSURE_MASK              |
                            GDK_POINTER_MOTION_MASK        |
                            GDK_POINTER_MOTION_HINT_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y;

  priv->input_window = gdk_window_new (gtk_widget_get_window (widget),
                                       &attributes, attributes_mask);
  gdk_window_set_user_data (priv->input_window, ruler);

  gtk_shruler_make_pixmap (ruler);
}

static void
gtk_shruler_unrealize (GtkWidget *widget)
{
  GtkSHRuler        *ruler = GTK_SHRULER (widget);
  GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (ruler);

  if (priv->backing_store)
    {
      cairo_surface_destroy (priv->backing_store);
      priv->backing_store = NULL;
    }

  if (priv->layout)
    {
      g_object_unref (priv->layout);
      priv->layout = NULL;
    }

  if (priv->input_window)
    {
      gdk_window_destroy (priv->input_window);
      priv->input_window = NULL;
    }

  GTK_WIDGET_CLASS (gtk_shruler_parent_class)->unrealize (widget);
}

static void
gtk_shruler_map (GtkWidget *widget)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (widget);

  GTK_WIDGET_CLASS (parent_class)->map (widget);

  if (priv->input_window)
    gdk_window_show (priv->input_window);
}

static void
gtk_shruler_unmap (GtkWidget *widget)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (widget);

  if (priv->input_window)
    gdk_window_hide (priv->input_window);

  GTK_WIDGET_CLASS (parent_class)->unmap (widget);
}

static void
gtk_shruler_size_allocate (GtkWidget     *widget,
                          GtkAllocation *allocation)
{
  GtkSHRuler        *ruler = GTK_SHRULER (widget);
  GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (ruler);

  gtk_widget_set_allocation (widget, allocation);

  if (gtk_widget_get_realized (widget))
    {
      gdk_window_move_resize (priv->input_window,
                              allocation->x, allocation->y,
                              allocation->width, allocation->height);

      gtk_shruler_make_pixmap (ruler);
    }
}

static void
gtk_shruler_size_request (GtkWidget      *widget,
                         GtkRequisition *requisition)
{
  GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (widget);
  GtkStyle         *style = gtk_widget_get_style (widget);
  PangoLayout      *layout;
  PangoRectangle    ink_rect;
  gint              size;

  layout = gtk_shruler_get_layout (widget, "0123456789");
  pango_layout_get_pixel_extents (layout, &ink_rect, NULL);

  size = 2 + ink_rect.height * 1.7;

  if (priv->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      requisition->width  = style->xthickness * 2 + 1;
      requisition->height = style->ythickness * 2 + size;
    }
  else
    {
      requisition->width  = style->xthickness * 2 + size;
      requisition->height = style->ythickness * 2 + 1;
    }
}

static void
gtk_shruler_style_set (GtkWidget *widget,
                      GtkStyle  *prev_style)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (widget);

  GTK_WIDGET_CLASS (gtk_shruler_parent_class)->style_set (widget, prev_style);

  gtk_widget_style_get (widget,
                        "font-scale", &priv->font_scale,
                        NULL);

  if (priv->layout)
    {
      g_object_unref (priv->layout);
      priv->layout = NULL;
    }
}

static gboolean
gtk_shruler_motion_notify (GtkWidget      *widget,
                          GdkEventMotion *event)
{
  GtkSHRuler *ruler = GTK_SHRULER (widget);

  gdk_event_request_motions (event);

  gtk_shruler_update_position (ruler, event->x, event->y);

  return FALSE;
}

static gboolean
gtk_shruler_expose (GtkWidget      *widget,
                   GdkEventExpose *event)
{
  if (gtk_widget_is_drawable (widget))
    {
      GtkSHRuler        *ruler = GTK_SHRULER (widget);
      GtkSHRulerPrivate *priv  = GTK_SHRULER_GET_PRIVATE (ruler);
      GtkAllocation     allocation;
      cairo_t          *cr;

      gtk_shruler_draw_ticks (ruler);

      cr = gdk_cairo_create (gtk_widget_get_window (widget));
      gdk_cairo_region (cr, event->region);
      cairo_clip (cr);

      gtk_widget_get_allocation (widget, &allocation);
      cairo_translate (cr, allocation.x, allocation.y);

      cairo_set_source_surface (cr, priv->backing_store, 0, 0);
      cairo_paint (cr);

      cairo_destroy (cr);
    }

  return FALSE;
}

static void
gtk_shruler_draw_ticks (GtkSHRuler *ruler)
{
  GtkWidget        *widget = GTK_WIDGET (ruler);
  GtkStyle         *style  = gtk_widget_get_style (widget);
  GtkSHRulerPrivate *priv   = GTK_SHRULER_GET_PRIVATE (ruler);
  GtkStateType      state  = gtk_widget_get_state (widget);
  GtkAllocation     allocation;
  cairo_t          *cr;
  gint              i;
  gint              width, height;
  gint              xthickness;
  gint              ythickness;
  gint              length;
  gdouble           lower, upper;  /* Upper and lower limits, in ruler units */
  gdouble           increment;     /* Number of pixels per unit */
  gint              scale;         /* Number of units per major unit */
  gdouble           start, end, cur;
  gchar             unit_str[32];
  gint              digit_height;
  gint              digit_offset;
  gint              text_size;
  gint              pos;
  gdouble           max_size;
  GtkCMUnit           unit;
  PangoLayout      *layout;
  PangoRectangle    logical_rect, ink_rect;

  if (! gtk_widget_is_drawable (widget))
    return;

  gtk_widget_get_allocation (widget, &allocation);

  xthickness = style->xthickness;
  ythickness = style->ythickness;

  layout = gtk_shruler_get_layout (widget, "0123456789");
  pango_layout_get_extents (layout, &ink_rect, &logical_rect);

  digit_height = PANGO_PIXELS (ink_rect.height) + 2;
  digit_offset = ink_rect.y;

  if (priv->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      width  = allocation.width;
      height = allocation.height - ythickness * 2;
    }
  else
    {
      width  = allocation.height;
      height = allocation.width - ythickness * 2;
    }

  cr = cairo_create (priv->backing_store);
  gdk_cairo_set_source_color (cr, &style->bg[state]);

  cairo_paint (cr);

  gdk_cairo_set_source_color (cr, &style->fg[state]);

  gtk_shruler_get_range (ruler, &lower, &upper, &max_size);

  if ((upper - lower) == 0)
    goto out;

  increment = (gdouble) width / (upper - lower);

  /* determine the scale
   *   use the maximum extents of the ruler to determine the largest
   *   possible number to be displayed.  Calculate the height in pixels
   *   of this displayed text. Use this height to find a scale which
   *   leaves sufficient room for drawing the ruler.
   *
   *   We calculate the text size as for the vruler instead of
   *   actually measuring the text width, so that the result for the
   *   scale looks consistent with an accompanying vruler.
   */
  scale = ceil (max_size);
  g_snprintf (unit_str, sizeof (unit_str), "%d", scale);
  text_size = strlen (unit_str) * digit_height + 1;

  for (scale = 0; scale < G_N_ELEMENTS (ruler_metric.ruler_scale); scale++)
    if (ruler_metric.ruler_scale[scale] * fabs (increment) > 2 * text_size)
      break;

  if (scale == G_N_ELEMENTS (ruler_metric.ruler_scale))
    scale = G_N_ELEMENTS (ruler_metric.ruler_scale) - 1;

  unit = gtk_shruler_get_unit (ruler);

  /* drawing starts here */
  length = 0;
  for (i = G_N_ELEMENTS (ruler_metric.subdivide) - 1; i >= 0; i--)
    {
      gdouble subd_incr;

      /* hack to get proper subdivisions at full pixels */
      if (unit == CM_UNIT_PIXEL && scale == 1 && i == 1)
        subd_incr = 1.0;
      else
        subd_incr = ((gdouble) ruler_metric.ruler_scale[scale] /
                     (gdouble) ruler_metric.subdivide[i]);

      if (subd_incr * fabs (increment) <= MINIMUM_INCR)
        continue;

      /* don't subdivide pixels */
      if (unit == CM_UNIT_PIXEL && subd_incr < 1.0)
        continue;

      if (lower < upper)
        {
          start = floor (lower / subd_incr) * subd_incr;
          end   = ceil  (upper / subd_incr) * subd_incr;
        }
      else
        {
          start = floor (upper / subd_incr) * subd_incr;
          end   = ceil  (lower / subd_incr) * subd_incr;
        }

      for (cur = start; cur <= end; cur += subd_incr)
        {
          if (((int)cur) % 10 == 0)
          	length = height * 2 / 3;
          else if (((int)cur) % 5 == 0)
            length = height / 3;
          else
            length = height / 4;

          pos = ROUND ((cur - lower) * increment);

          if (priv->orientation == GTK_ORIENTATION_HORIZONTAL)
            {
              cairo_rectangle (cr,
                               pos, height + ythickness - length,
                               1,   length);
            }
          else
            {
              cairo_rectangle (cr,
                               height + xthickness - length, pos,
                               length,                       1);
            }

          /* draw label */
          if (i == 0 && ((int)cur) % 10 == 0)
            {
              g_snprintf (unit_str, sizeof (unit_str), "%d", (int) cur);

              if (priv->orientation == GTK_ORIENTATION_HORIZONTAL)
                {
                  pango_layout_set_text (layout, unit_str, -1);
                  pango_layout_get_extents (layout, &logical_rect, NULL);

                  cairo_move_to (cr,
                                 pos + 2,
                                 ythickness + PANGO_PIXELS (logical_rect.y - digit_offset));
                  pango_cairo_show_layout (cr, layout);
                }
              else
                {
                  gint j;

                  for (j = 0; j < (int) strlen (unit_str); j++)
                    {
                      pango_layout_set_text (layout, unit_str + j, 1);
                      pango_layout_get_extents (layout, NULL, &logical_rect);

                      cairo_move_to (cr,
                                     xthickness + 1,
                                     pos + digit_height * j + 2 + PANGO_PIXELS (logical_rect.y - digit_offset));
                      pango_cairo_show_layout (cr, layout);
                    }
                }
            }
        }
    }

  cairo_fill (cr);
out:
  cairo_destroy (cr);
}

static cairo_surface_t *
cm_gdk_window_create_similar_surface (GdkWindow *     window,
                                   cairo_content_t content,
                                   int             width,
                                   int             height)
{
#if !GTK_CHECK_VERSION(2, 22, 0)
  cairo_surface_t *window_surface, *surface;

  g_return_val_if_fail (GDK_IS_WINDOW (window), NULL);

  window_surface = GDK_DRAWABLE_GET_CLASS(window)->ref_cairo_surface(window);

  surface = cairo_surface_create_similar (window_surface,
                                          content,
                                          width, height);

  cairo_surface_destroy (window_surface);

  return surface;
#else
  return gdk_window_create_similar_surface(window, content, width, height);
#endif
}

static void
gtk_shruler_make_pixmap (GtkSHRuler *ruler)
{
  GtkWidget        *widget = GTK_WIDGET (ruler);
  GtkSHRulerPrivate *priv   = GTK_SHRULER_GET_PRIVATE (ruler);
  GtkAllocation     allocation;

  gtk_widget_get_allocation (widget, &allocation);

  if (priv->backing_store)
    cairo_surface_destroy (priv->backing_store);

  priv->backing_store =
    cm_gdk_window_create_similar_surface (gtk_widget_get_window (widget),
                                       CAIRO_CONTENT_COLOR,
                                       allocation.width,
                                       allocation.height);
}


static PangoLayout *
gtk_shruler_create_layout (GtkWidget   *widget,
                          const gchar *text)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (widget);
  PangoLayout      *layout;
  PangoAttrList    *attrs;
  PangoAttribute   *attr;

  layout = gtk_widget_create_pango_layout (widget, text);

  attrs = pango_attr_list_new ();

  attr = pango_attr_scale_new (priv->font_scale);
  attr->start_index = 0;
  attr->end_index   = -1;
  pango_attr_list_insert (attrs, attr);

  pango_layout_set_attributes (layout, attrs);
  pango_attr_list_unref (attrs);

  return layout;
}

static PangoLayout *
gtk_shruler_get_layout (GtkWidget   *widget,
                       const gchar *text)
{
  GtkSHRulerPrivate *priv = GTK_SHRULER_GET_PRIVATE (widget);

  if (priv->layout)
    {
      pango_layout_set_text (priv->layout, text, -1);
      return priv->layout;
    }

  priv->layout = gtk_shruler_create_layout (widget, text);

  return priv->layout;
}
