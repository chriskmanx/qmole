/*
 * peas-demo-window.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 Steve Frécinaux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __PEAS_DEMO_WINDOW_H__
#define __PEAS_DEMO_WINDOW_H__

#include <gtk/gtk.h>
#include <libpeas/peas.h>

G_BEGIN_DECLS

/*
 * Type checking and casting macros
 */
#define DEMO_TYPE_WINDOW              (demo_window_get_type())
#define DEMO_WINDOW(obj)              (G_TYPE_CHECK_INSTANCE_CAST((obj), DEMO_TYPE_WINDOW, DemoWindow))
#define DEMO_WINDOW_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST((klass), DEMO_TYPE_WINDOW, DemoWindowClass))
#define DEMO_IS_WINDOW(obj)           (G_TYPE_CHECK_INSTANCE_TYPE((obj), DEMO_TYPE_WINDOW))
#define DEMO_IS_WINDOW_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), DEMO_TYPE_WINDOW))
#define DEMO_WINDOW_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS((obj), DEMO_TYPE_WINDOW, DemoWindowClass))

typedef struct _DemoWindow         DemoWindow;
typedef struct _DemoWindowClass    DemoWindowClass;

struct _DemoWindow
{
  GtkWindow parent;

  GtkWidget *box;

  PeasEngine *engine;
  PeasExtensionSet *exten_set;
};

struct _DemoWindowClass
{
  GtkWindowClass parent_class;

  guint n_windows;
};

GType       demo_window_get_type      (void)  G_GNUC_CONST;
GtkWidget  *demo_window_new           (void);

G_END_DECLS

#endif /* __DEMO_WINDOW_H__  */
