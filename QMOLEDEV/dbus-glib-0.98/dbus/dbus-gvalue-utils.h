/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gvalue-utils.h: Non-DBus-specific functions related to GType/GValue
 *
 * Copyright (C) 2005 Red Hat, Inc.
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#ifndef DBUS_GOBJECT_VALUE_UTILS_H
#define DBUS_GOBJECT_VALUE_UTILS_H

#include <glib.h>
#include <glib-object.h>

G_BEGIN_DECLS

void           _dbus_g_type_specialized_builtins_init (void);

gboolean       _dbus_g_type_is_fixed                  (GType gtype); 
guint          _dbus_g_type_fixed_get_size            (GType gtype); 

gboolean       _dbus_gvalue_set_from_pointer          (GValue *value,
						      gconstpointer storage);

typedef void (*DBusGHashValueForeachFunc) (GValue * key, GValue *val, gpointer data);

void           _dbus_g_hash_table_value_foreach       (GHashTable                *table,
						      GType                      hash_type,
						      DBusGHashValueForeachFunc  func,
						      gpointer                   data);

void           _dbus_g_hash_table_insert_values       (GHashTable                *table,
						      GValue                    *key_val,
						      GValue                    *value_val);
void           _dbus_g_hash_table_insert_steal_values (GHashTable *table,
						      GValue     *key_val,
						      GValue     *value_val);

gboolean       _dbus_gtype_is_valid_hash_key          (GType type);
gboolean       _dbus_gtype_is_valid_hash_value        (GType type);

GHashFunc      _dbus_g_hash_func_from_gtype           (GType gtype);
GEqualFunc     _dbus_g_hash_equal_from_gtype          (GType gtype);
GDestroyNotify _dbus_g_hash_free_from_gtype           (GType gtype);

gboolean       _dbus_gvalue_store                     (GValue          *value,
						      gpointer         storage);

gboolean       _dbus_gvalue_take                      (GValue          *value,
						      GTypeCValue     *cvalue);

gboolean       _dbus_gtype_can_signal_error          (GType                    gtype);
gboolean       _dbus_gvalue_signals_error            (const GValue            *value);


G_END_DECLS

#endif
