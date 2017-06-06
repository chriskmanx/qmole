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

#ifndef GCONF_DBUS_UTILS_H
#define GCONF_DBUS_UTILS_H

#include <glib.h>
#include <dbus/dbus.h>
#include <gconf/gconf.h>
#include <gconf/gconf-value.h>

#define GCONF_DBUS_SERVICE                  "org.gnome.GConf"

#define GCONF_DBUS_SERVER_INTERFACE         "org.gnome.GConf.Server"
#define GCONF_DBUS_DATABASE_INTERFACE       "org.gnome.GConf.Database"

#define GCONF_DBUS_SERVER_OBJECT            "/org/gnome/GConf/Server"

#define GCONF_DBUS_SERVER_GET_DEFAULT_DB    "GetDefaultDatabase"
#define GCONF_DBUS_SERVER_GET_DB            "GetDatabase"
#define GCONF_DBUS_SERVER_SHUTDOWN          "Shutdown"
#define GCONF_DBUS_SERVER_BYE_SIGNAL        "Bye"

#define GCONF_DBUS_DATABASE_LOOKUP          "Lookup"
#define GCONF_DBUS_DATABASE_LOOKUP_EXTENDED "LookupExtended" 
#define GCONF_DBUS_DATABASE_LOOKUP_DEFAULT  "LookupDefault" 
#define GCONF_DBUS_DATABASE_SET             "Set"
#define GCONF_DBUS_DATABASE_UNSET           "UnSet"
#define GCONF_DBUS_DATABASE_RECURSIVE_UNSET "RecursiveUnset"
#define GCONF_DBUS_DATABASE_DIR_EXISTS      "DirExists"
#define GCONF_DBUS_DATABASE_GET_ALL_ENTRIES "AllEntries"
#define GCONF_DBUS_DATABASE_GET_ALL_DIRS    "AllDirs"
#define GCONF_DBUS_DATABASE_SET_SCHEMA      "SetSchema"
#define GCONF_DBUS_DATABASE_SUGGEST_SYNC    "SuggestSync"

#define GCONF_DBUS_DATABASE_ADD_NOTIFY      "AddNotify"
#define GCONF_DBUS_DATABASE_REMOVE_NOTIFY   "RemoveNotify"
 
#define GCONF_DBUS_LISTENER_NOTIFY          "Notify"

#define GCONF_DBUS_CLIENT_SERVICE           "org.gnome.GConf.ClientService"
#define GCONF_DBUS_CLIENT_OBJECT            "/org/gnome/GConf/Client"
#define GCONF_DBUS_CLIENT_INTERFACE         "org.gnome.GConf.Client"

#define GCONF_DBUS_UNSET_INCLUDING_SCHEMA_NAMES 0x1
 
#define GCONF_DBUS_ERROR_FAILED               "org.gnome.GConf.Error.Failed"
#define GCONF_DBUS_ERROR_NO_PERMISSION        "org.gnome.GConf.Error.NoPermission"
#define GCONF_DBUS_ERROR_BAD_ADDRESS          "org.gnome.GConf.Error.BadAddress"
#define GCONF_DBUS_ERROR_BAD_KEY              "org.gnome.GConf.Error.BadKey"
#define GCONF_DBUS_ERROR_PARSE_ERROR          "org.gnome.GConf.Error.ParseError"
#define GCONF_DBUS_ERROR_CORRUPT              "org.gnome.GConf.Error.Corrupt"
#define GCONF_DBUS_ERROR_TYPE_MISMATCH        "org.gnome.GConf.Error.TypeMismatch"
#define GCONF_DBUS_ERROR_IS_DIR               "org.gnome.GConf.Error.IsDir"
#define GCONF_DBUS_ERROR_IS_KEY               "org.gnome.GConf.Error.IsKey"
#define GCONF_DBUS_ERROR_NO_WRITABLE_DATABASE "org.gnome.GConf.Error.NoWritableDatabase"
#define GCONF_DBUS_ERROR_IN_SHUTDOWN          "org.gnome.GConf.Error.InShutdown"
#define GCONF_DBUS_ERROR_OVERRIDDEN           "org.gnome.GConf.Error.Overriden"
#define GCONF_DBUS_ERROR_LOCK_FAILED          "org.gnome.GConf.Error.LockFailed"

void        gconf_dbus_utils_append_value     (DBusMessageIter   *iter,
					       const GConfValue  *value);
GConfValue *gconf_dbus_utils_get_value        (DBusMessageIter   *iter);

void        gconf_dbus_utils_append_entry_values (DBusMessageIter   *iter,
						 const gchar       *key,
						 const GConfValue  *value,
						 gboolean           is_default,
						 gboolean           is_writable,
						 const gchar       *schema_name);
gboolean    gconf_dbus_utils_get_entry_values   (DBusMessageIter   *iter,
						 gchar            **key,
						 GConfValue       **value,
						 gboolean          *is_default,
						 gboolean          *is_writable,
						 gchar            **schema_name);

void gconf_dbus_utils_append_entries (DBusMessageIter *iter,
				      GSList          *entries);

GSList *gconf_dbus_utils_get_entries (DBusMessageIter *iter, const gchar *dir);


#endif/* GCONF_DBUS_UTILS_H */
