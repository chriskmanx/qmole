
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

#ifndef GCONF_GCONFBACKEND_H
#define GCONF_GCONFBACKEND_H

#include "gconf/gconf-internals.h"
#include <gmodule.h>
#include "gconf/gconf-sources.h"

/*
 * This vtable is more complicated than strictly necessary, hoping
 * that backends can be smart and optimize some calls
 */

typedef struct _GConfBackendVTable GConfBackendVTable;

struct _GConfBackendVTable {
  /* Set to sizeof (GConfBackendVTable) - used for future proofing */
  gsize                  vtable_size;

  void                (* shutdown)        (GError** err);

  GConfSource*        (* resolve_address) (const gchar* address,
                                           GError** err);

  /* Thread locks. If the backend is thread-safe, then these
   * can be NULL. If per-source locks are needed, then these
   * calls should lock a mutex stored in the GConfSource.
   * If a per-backend lock is needed, then the calls can ignore
   * their source argument and lock the whole backend.
   */
  void                (* lock)            (GConfSource* source,
                                           GError** err);

  void                (* unlock)          (GConfSource* source,
                                           GError** err);

  /* Report whether a given key (and its subkeys) can be read/written.
   * Sources may not permit reading/writing from/to /foo but forbid
   * writing to /foo/bar; if a key can be read or written then its
   * subkeys may also be read/written.
   *
   * This field allows backends to be configured so that they only
   * store certain kinds of data in certain sections of the GConf
   * namespace.
   *
   * If these functions return an error, they MUST return FALSE as
   * well.
   */

  gboolean           (* readable)         (GConfSource* source,
                                           const gchar* key,
                                           GError** err);

  gboolean           (* writable)        (GConfSource* source,
                                           const gchar* key,
                                           GError** err);
  
  /* schema_name filled if NULL or GCONF_VALUE_IGNORE_SUBSEQUENT returned.
     if schema_name is NULL, it isn't filled */
  GConfValue*         (* query_value)     (GConfSource* source, 
                                           const gchar* key,
                                           const gchar** locales,
                                           gchar** schema_name,
                                           GError** err);
  
  GConfMetaInfo*      (* query_metainfo)  (GConfSource* source,
                                           const gchar* key,
                                           GError** err);
  
  void                (* set_value)       (GConfSource* source, 
                                           const gchar* key, 
                                           const GConfValue* value,
                                           GError** err);

  /* Returns list of GConfEntry with key set to a relative
   * pathname. In the public client-side API the key
   * is always absolute though.
   */
  GSList*             (* all_entries)     (GConfSource* source,
                                           const gchar* dir,
                                           const gchar** locales,
                                           GError** err);

  /* Returns list of allocated strings, relative names */
  GSList*             (* all_subdirs)     (GConfSource* source,
                                           const gchar* dir,
                                           GError** err);

  void                (* unset_value)     (GConfSource* source,
                                           const gchar* key,
                                           const gchar* locale,
                                           GError** err);

  gboolean            (* dir_exists)      (GConfSource* source,
                                           const gchar* dir,
                                           GError** err);
        
  void                (* remove_dir)      (GConfSource* source,
                                           const gchar* dir,
                                           GError** err);
  
  void                (* set_schema)      (GConfSource* source,
                                           const gchar* key,
                                           const gchar* schema_key,
                                           GError** err);

  gboolean            (* sync_all)        (GConfSource* source,
                                           GError** err);

  void                (* destroy_source)  (GConfSource* source);

  /* This is basically used by the test suite */
  void                (* clear_cache)     (GConfSource* source);

  /* used by gconf-sanity-check */
  void                (* blow_away_locks) (const char *address);

  void                (* set_notify_func) (GConfSource           *source,
					   GConfSourceNotifyFunc  notify_func,
					   gpointer               user_data);

  void                (* add_listener)    (GConfSource           *source,
					   guint                  id,
					   const gchar           *namespace_section);

  void                (* remove_listener) (GConfSource           *source,
					   guint                  id);
};

struct _GConfBackend {
  const gchar* name;
  guint refcount;
  GConfBackendVTable vtable;
  GModule* module;
};

/* Address utility stuff */

/* Get the backend name */
gchar*        gconf_address_backend(const gchar* address);
/* Get the resource name understood only by the backend */
gchar*        gconf_address_resource(const gchar* address);
/* Get the backend flags */
gchar**       gconf_address_flags(const gchar* address);

gchar*        gconf_backend_file(const gchar* address);

/* Obtain the GConfBackend for this address, based on the first part of the 
 * address. The refcount is always incremented, and you must unref() later.
 * The same backend may be returned by multiple calls to this routine,
 * but you can assume they are different if you do the refcounting
 * right. Returns NULL if it fails.
 */
GConfBackend* gconf_get_backend(const gchar* address, GError** err);

void          gconf_backend_ref(GConfBackend* backend);
void          gconf_backend_unref(GConfBackend* backend);

GConfSource*  gconf_backend_resolve_address (GConfBackend* backend, 
                                             const gchar* address,
                                             GError** err);

void          gconf_blow_away_locks       (const gchar* address);

#endif
