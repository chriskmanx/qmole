/* GConf
 * Copyright (C) 2003 Imendio HB
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef GCONF_GCONF_DATABASE_DBUS_H
#define GCONF_GCONF_DATABASE_DBUS_H

#include <dbus/dbus.h>
#include "gconf-database.h"

void         gconf_database_dbus_setup            (GConfDatabase    *db);
void         gconf_database_dbus_teardown         (GConfDatabase *db);
const gchar *gconf_database_dbus_get_path         (GConfDatabase    *db);
void         gconf_database_dbus_notify_listeners (GConfDatabase    *db,
						   GConfSources     *modified_sources,
						   const gchar      *key,
						   const GConfValue *value,
						   gboolean          is_default,
						   gboolean          is_writable,
						   gboolean          notify_others);

#endif
