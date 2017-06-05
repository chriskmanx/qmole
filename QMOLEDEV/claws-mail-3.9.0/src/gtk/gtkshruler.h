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

#ifndef __GTK_SHRULER_H__
#define __GTK_SHRULER_H__

#include "gtkunit.h"

G_BEGIN_DECLS

#define GTK_TYPE_SHRULER            (gtk_shruler_get_type ())
#define GTK_SHRULER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_SHRULER, GtkSHRuler))
#define GTK_SHRULER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_SHRULER, GtkSHRulerClass))
#define GTK_IS_SHRULER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SHRULER))
#define GTK_IS_SHRULER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SHRULER))
#define GTK_SHRULER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_SHRULER, GtkSHRulerClass))

typedef struct _GtkSHRuler        GtkSHRuler;
typedef struct _GtkSHRulerClass   GtkSHRulerClass;

struct _GtkSHRuler
{
  GtkWidget  parent_instance;
};

struct _GtkSHRulerClass
{
  GtkWidgetClass  parent_class;

  /* Padding for future expansion */
  void (*_gtk_reserved1) (void);
  void (*_gtk_reserved2) (void);
  void (*_gtk_reserved3) (void);
  void (*_gtk_reserved4) (void);
};


GType       gtk_shruler_get_type            (void) G_GNUC_CONST;

GtkWidget * gtk_shruler_new                 (GtkOrientation  orientation);

void        gtk_shruler_add_track_widget    (GtkSHRuler      *ruler,
                                            GtkWidget      *widget);
void        gtk_shruler_remove_track_widget (GtkSHRuler      *ruler,
                                            GtkWidget      *widget);

void        gtk_shruler_set_unit            (GtkSHRuler      *ruler,
                                            GtkCMUnit        unit);
GtkCMUnit     gtk_shruler_get_unit            (GtkSHRuler      *ruler);
void        gtk_shruler_set_position        (GtkSHRuler      *ruler,
                                            gdouble         position);
gdouble     gtk_shruler_get_position        (GtkSHRuler      *ruler);
void        gtk_shruler_set_range           (GtkSHRuler      *ruler,
                                            gdouble         lower,
                                            gdouble         upper,
                                            gdouble         max_size);
void        gtk_shruler_get_range           (GtkSHRuler      *ruler,
                                            gdouble        *lower,
                                            gdouble        *upper,
                                            gdouble        *max_size);

G_END_DECLS

#endif /* __GTK_SHRULER_H__ */
