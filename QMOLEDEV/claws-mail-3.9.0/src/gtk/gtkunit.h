/* LIBGTK - The GTK Library
 * Copyright (C) 1995-1997 Peter Mattis and Spencer Kimball
 *
 * gimpunit.h
 * Copyright (C) 1999-2003 Michael Natterer <mitch@gimp.org>
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

#ifndef __CM_UNIT_H__
#define __CM_UNIT_H__

#include <glib-object.h>

typedef enum /*< skip >*/
{
  CM_UNIT_PIXEL   = 0,

  CM_UNIT_INCH    = 1,
  CM_UNIT_MM      = 2,
  CM_UNIT_POINT   = 3,
  CM_UNIT_PICA    = 4,

  CM_UNIT_END     = 5,

  CM_UNIT_PERCENT = 65536 /*< pdb-skip >*/
} GtkCMUnit;

typedef struct _GtkCMUnitVtable GtkCMUnitVtable;

struct _GtkCMUnitVtable
{
  gint          (* unit_get_number_of_units)          (void);
  gint          (* unit_get_number_of_built_in_units) (void);

  GtkCMUnit       (* unit_new)                          (gchar    *identifier,
                                                       gdouble   factor,
                                                       gint      digits,
                                                       gchar    *symbol,
                                                       gchar    *abbreviation,
                                                       gchar    *singular,
                                                       gchar    *plural);
  gboolean      (* unit_get_deletion_flag)            (GtkCMUnit  unit);
  void          (* unit_set_deletion_flag)            (GtkCMUnit  unit,
                                                       gboolean  deletion_flag);

  gdouble       (* unit_get_factor)                   (GtkCMUnit  unit);
  gint          (* unit_get_digits)                   (GtkCMUnit  unit);
  const gchar * (* unit_get_identifier)               (GtkCMUnit  unit);
  const gchar * (* unit_get_symbol)                   (GtkCMUnit  unit);
  const gchar * (* unit_get_abbreviation)             (GtkCMUnit  unit);
  const gchar * (* unit_get_singular)                 (GtkCMUnit  unit);
  const gchar * (* unit_get_plural)                   (GtkCMUnit  unit);

  void          (* _reserved_1)                       (void);
  void          (* _reserved_2)                       (void);
  void          (* _reserved_3)                       (void);
  void          (* _reserved_4)                       (void);
};


extern GtkCMUnitVtable _gimp_unit_vtable;


G_BEGIN_DECLS

void  gimp_base_init (GtkCMUnitVtable *vtable);


/* For information look into the C source or the html documentation */

/**
 * GTK_TYPE_UNIT:
 *
 * #GTK_TYPE_UNIT is a #GType derived from #G_TYPE_INT.
 **/

#define GTK_TYPE_UNIT               (gtk_unit_get_type ())
#define GTK_VALUE_HOLDS_UNIT(value) (G_TYPE_CHECK_VALUE_TYPE ((value), GTK_TYPE_UNIT))


GType        gtk_unit_get_type      (void) G_GNUC_CONST;


/*
 * GTK_TYPE_PARAM_UNIT
 */

#define GTK_TYPE_PARAM_UNIT              (gtk_param_unit_get_type ())
#define GTK_IS_PARAM_SPEC_UNIT(pspec)    (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), GTK_TYPE_PARAM_UNIT))

GType        gtk_param_unit_get_type     (void) G_GNUC_CONST;

GParamSpec * gtk_param_spec_unit         (const gchar  *name,
                                          const gchar  *nick,
                                          const gchar  *blurb,
                                          gboolean      allow_pixels,
                                          gboolean      allow_percent,
                                          GtkCMUnit      default_value,
                                          GParamFlags   flags);



gint          gtk_unit_get_number_of_units          (void);
gint          gtk_unit_get_number_of_built_in_units (void) G_GNUC_CONST;

GtkCMUnit       gtk_unit_new                 (gchar       *identifier,
                                            gdouble      factor,
                                            gint         digits,
                                            gchar       *symbol,
                                            gchar       *abbreviation,
                                            gchar       *singular,
                                            gchar       *plural);

gboolean      gtk_unit_get_deletion_flag   (GtkCMUnit     unit);
void          gtk_unit_set_deletion_flag   (GtkCMUnit     unit,
                                            gboolean     deletion_flag);

gdouble       gtk_unit_get_factor          (GtkCMUnit     unit);

gint          gtk_unit_get_digits          (GtkCMUnit     unit);

const gchar * gtk_unit_get_identifier      (GtkCMUnit     unit);

const gchar * gtk_unit_get_symbol          (GtkCMUnit     unit);
const gchar * gtk_unit_get_abbreviation    (GtkCMUnit     unit);
const gchar * gtk_unit_get_singular        (GtkCMUnit     unit);
const gchar * gtk_unit_get_plural          (GtkCMUnit     unit);

gchar       * gtk_unit_format_string       (const gchar *format,
                                            GtkCMUnit     unit);

gdouble       gtk_pixels_to_units          (gdouble      pixels,
                                            GtkCMUnit     unit,
                                            gdouble      resolution);
gdouble       gtk_units_to_pixels          (gdouble      value,
                                            GtkCMUnit     unit,
                                            gdouble      resolution);
gdouble       gtk_units_to_points          (gdouble      value,
                                            GtkCMUnit     unit,
                                            gdouble      resolution);


G_END_DECLS

#endif /* __CM_UNIT_H__ */
