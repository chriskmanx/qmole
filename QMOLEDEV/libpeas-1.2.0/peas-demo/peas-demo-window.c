/*
 * peas-demo-window.c
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "peas-demo-window.h"

G_DEFINE_TYPE (DemoWindow, demo_window, GTK_TYPE_WINDOW);

static void
on_extension_added (PeasExtensionSet *set,
                    PeasPluginInfo   *info,
                    PeasExtension    *exten,
                    DemoWindow       *dw)
{
  peas_activatable_activate (PEAS_ACTIVATABLE (exten));
}

static void
on_extension_removed (PeasExtensionSet *set,
                      PeasPluginInfo   *info,
                      PeasExtension    *exten,
                      DemoWindow       *dw)
{
  peas_activatable_deactivate (PEAS_ACTIVATABLE (exten));
}

static void
demo_window_init (DemoWindow *dw)
{
  DemoWindowClass *klass = DEMO_WINDOW_GET_CLASS (dw);
  gchar *label;

  dw->box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
  gtk_box_set_homogeneous (GTK_BOX (dw->box), TRUE);
  gtk_container_add (GTK_CONTAINER (dw), dw->box);

  label = g_strdup_printf ("Peas Window %d", ++(klass->n_windows));
  gtk_window_set_title (GTK_WINDOW (dw), label);
  g_free (label);

  dw->exten_set = peas_extension_set_new (peas_engine_get_default (),
                                          PEAS_TYPE_ACTIVATABLE,
                                          "object", dw,
                                          NULL);

  peas_extension_set_foreach (dw->exten_set,
                              (PeasExtensionSetForeachFunc) on_extension_added,
                              dw);

  g_signal_connect (dw->exten_set, "extension-added", G_CALLBACK (on_extension_added), dw);
  g_signal_connect (dw->exten_set, "extension-removed", G_CALLBACK (on_extension_removed), dw);
}

static void
demo_window_dispose (GObject *object)
{
  DemoWindow *dw = DEMO_WINDOW (object);

  if (dw->exten_set != NULL)
    {
      g_object_unref (dw->exten_set);
      dw->exten_set = NULL;
    }

  G_OBJECT_CLASS (demo_window_parent_class)->dispose (object);
}

static void
demo_window_class_init (DemoWindowClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = demo_window_dispose;

  klass->n_windows = 0;
}

GtkWidget *
demo_window_new (void)
{
  return GTK_WIDGET (g_object_new (DEMO_TYPE_WINDOW, NULL));
}
