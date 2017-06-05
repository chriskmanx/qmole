/*
 * testing.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Garrett Regier
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

#ifndef __TESTING_H__
#define __TESTING_H__

#include <libpeas/peas.h>
#include <libpeas-gtk/peas-gtk.h>
#include <testing-util.h>

G_BEGIN_DECLS

void            testing_init                     (void);

PeasEngine     *testing_engine_new               (void);

PeasPluginInfo *testing_get_plugin_info_for_iter (PeasGtkPluginManagerView *view,
                                                  GtkTreeIter              *iter);
gboolean        testing_get_iter_for_plugin_info (PeasGtkPluginManagerView *view,
                                                  PeasPluginInfo           *info,
                                                  GtkTreeIter              *iter);

void            testing_show_widget              (gpointer                  widget);

/* libtesting-util functions which do not need to be overridden */
#define testing_engine_free testing_util_engine_free
#define testing_run_tests   testing_util_run_tests

G_END_DECLS

#endif /* __TESTING_H__ */
