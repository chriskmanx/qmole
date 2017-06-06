/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
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
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifndef GCONF_GCONF_DATABASE_H
#define GCONF_GCONF_DATABASE_H

#include <glib.h>

G_BEGIN_DECLS

#include "gconf-error.h"
#ifdef HAVE_CORBA
#include "GConfX.h"
#endif
#include "gconf-listeners.h"
#include "gconf-sources.h"
#include "gconf-internals.h"

#ifdef HAVE_DBUS
#include <dbus/dbus.h>
#endif

#include "gconf-locale.h"

typedef struct _GConfDatabase GConfDatabase;

struct _GConfDatabase
{
#ifdef HAVE_CORBA
  /* "inherit" from the servant,
     must be first in struct */
  POA_ConfigDatabase3 servant;

  ConfigDatabase objref;
#endif

#ifdef HAVE_DBUS
  char           *object_path;
  
  /* Information about clients that want notification. */
  GHashTable     *notifications;
  GHashTable     *listening_clients;
#endif

  GConfListeners* listeners;
  GConfSources* sources;

  GTime last_access;
  guint sync_idle;
  guint sync_timeout;

  gchar *persistent_name;
};

GConfDatabase* gconf_database_new     (GConfSources  *sources);
void           gconf_database_free (GConfDatabase *db);

void           gconf_database_set_sources (GConfDatabase *db,
					   GConfSources  *sources);

void                gconf_database_drop_dead_listeners (GConfDatabase *db);

#ifdef HAVE_CORBA
CORBA_unsigned_long gconf_database_add_listener     (GConfDatabase       *db,
                                                     ConfigListener       who,
                                                     const char          *name,
                                                     const gchar         *where);
void                gconf_database_remove_listener  (GConfDatabase       *db,
                                                     CORBA_unsigned_long  cnxn);

CORBA_unsigned_long gconf_database_readd_listener   (GConfDatabase       *db,
                                                     ConfigListener       who,
                                                     const char          *name,
                                                     const gchar         *where);

void                gconf_database_notify_listeners (GConfDatabase       *db,
                                                     GConfSources        *modified_sources,
                                                     const gchar         *key,
                                                     const ConfigValue   *value,
                                                     gboolean             is_default,
                                                     gboolean             is_writable,
                                                     gboolean             notify_others);
#endif

GConfValue* gconf_database_query_value         (GConfDatabase  *db,
                                                const gchar    *key,
                                                const gchar   **locales,
                                                gboolean        use_schema_default,
                                                gchar         **schema_name,
                                                gboolean       *value_is_default,
                                                gboolean       *value_is_writable,
                                                GError    **err);
GConfValue* gconf_database_query_default_value (GConfDatabase  *db,
                                                const gchar    *key,
                                                const gchar   **locales,
                                                gboolean       *is_writable,
                                                GError    **err);



void gconf_database_set   (GConfDatabase      *db,
                           const gchar        *key,
                           GConfValue         *value,
#ifdef HAVE_CORBA
                           const ConfigValue  *cvalue,
#endif
                           GError        **err);
void gconf_database_unset (GConfDatabase      *db,
                           const gchar        *key,
                           const gchar        *locale,
                           GError        **err);

void gconf_database_recursive_unset (GConfDatabase      *db,
                                     const gchar        *key,
                                     const gchar        *locale,
                                     GConfUnsetFlags     flags,
                                     GError            **err);


gboolean gconf_database_dir_exists  (GConfDatabase  *db,
                                     const gchar    *dir,
                                     GError    **err);
void     gconf_database_remove_dir  (GConfDatabase  *db,
                                     const gchar    *dir,
                                     GError    **err);
GSList*  gconf_database_all_entries (GConfDatabase  *db,
                                     const gchar    *dir,
                                     const gchar   **locales,
                                     GError    **err);
GSList*  gconf_database_all_dirs    (GConfDatabase  *db,
                                     const gchar    *dir,
                                     GError    **err);
void     gconf_database_set_schema  (GConfDatabase  *db,
                                     const gchar    *key,
                                     const gchar    *schema_key,
                                     GError    **err);


void     gconf_database_sync             (GConfDatabase  *db,
                                          GError    **err);
gboolean gconf_database_synchronous_sync (GConfDatabase  *db,
                                          GError    **err);
void     gconf_database_clear_cache      (GConfDatabase  *db,
                                          GError    **err);
void     gconf_database_clear_cache_for_sources (GConfDatabase  *db,
						 GConfSources   *sources,
						 GError        **err);

GConfLocaleList* gconfd_locale_cache_lookup (const gchar *locale);

void gconfd_locale_cache_expire (void);
void gconfd_locale_cache_drop  (void);

const gchar* gconf_database_get_persistent_name (GConfDatabase *db);

#ifdef HAVE_CORBA
void gconf_database_log_listeners_to_string (GConfDatabase *db,
                                             gboolean is_default,
                                             GString *str);
#endif

G_END_DECLS

#endif



