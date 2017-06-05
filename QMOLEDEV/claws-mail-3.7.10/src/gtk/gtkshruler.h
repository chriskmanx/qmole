/* GTKSHRuler
 * Copyright (C) 2000-2011 Alfons Hoogervorst & The Claws Mail Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __GTK_SHRULER_H__
#define __GTK_SHRULER_H__


#include <gdk/gdk.h>
#include <gtk/gtk.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_SHRULER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), gtk_shruler_get_type (), GtkSHRuler))
#define GTK_SHRULER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), gtk_shruler_get_type (), GtkSHRulerClass))
#define GTK_IS_SHRULER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), gtk_shruler_get_type ()))


typedef struct _GtkSHRuler        GtkSHRuler;
typedef struct _GtkSHRulerClass   GtkSHRulerClass;

struct _GtkSHRuler
{
	GtkHRuler ruler;
};

struct _GtkSHRulerClass
{
 	GtkHRulerClass parent_class;
};


GType      gtk_shruler_get_type (void);
GtkWidget* gtk_shruler_new      (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_SHRULER_H__ */
