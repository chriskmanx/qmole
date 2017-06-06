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

#include <config.h>
#include "gconf-database.h"
#ifdef HAVE_DBUS
#include "gconf-database-dbus.h"
#endif
#include "gconf-listeners.h"
#include "gconf-sources.h"
#include "gconf-locale.h"
#include "gconfd.h"
#ifdef HAVE_DBUS
#include "gconfd-dbus.h"
#endif
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>

/*
 * Forward decls
 */

#ifdef HAVE_CORBA
typedef struct _Listener Listener;

struct _Listener {
  ConfigListener obj;
  char *name;
};

static Listener* listener_new (ConfigListener obj,
                               const char    *name);
static void      listener_destroy (Listener* l);

/*
 * CORBA implementation of ConfigDatabase
 */

static CORBA_unsigned_long
impl_ConfigDatabase_add_listener(PortableServer_Servant servant,
				 const CORBA_char * where,
				 const ConfigListener who,
                                 CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;

  if (gconfd_check_in_shutdown (ev))
    return 0;
  
  return gconf_database_add_listener (db, who, NULL, where);
}

static void
impl_ConfigDatabase_remove_listener(PortableServer_Servant servant,
				    CORBA_unsigned_long cnxn,
				    CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_database_remove_listener (db, cnxn);
}

static ConfigValue*
impl_ConfigDatabase_lookup_with_locale(PortableServer_Servant servant,
                                       const CORBA_char * key,
                                       const CORBA_char * locale,
                                       CORBA_boolean use_schema_default,
                                       CORBA_boolean * value_is_default,
                                       CORBA_boolean * value_is_writable,
                                       CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GConfValue* val;
  GError* error = NULL;
  GConfLocaleList* locale_list;
  gboolean is_default = FALSE;
  gboolean is_writable = TRUE;

  if (gconfd_check_in_shutdown (ev))
    return gconf_invalid_corba_value ();
  
  locale_list = gconfd_locale_cache_lookup(locale);
  
  val = gconf_database_query_value(db, key, locale_list->list,
                                   use_schema_default,
                                   NULL,
                                   &is_default,
                                   &is_writable,
                                   &error);

  *value_is_default = is_default;
  *value_is_writable = is_writable;
  
  gconf_locale_list_unref(locale_list);
  
  if (val != NULL)
    {
      ConfigValue* cval = gconf_corba_value_from_gconf_value(val);

      gconf_value_free(val);

      g_return_val_if_fail(error == NULL, cval);
      
      return cval;
    }
  else
    {
      gconf_set_exception(&error, ev);

      return gconf_invalid_corba_value ();
    }
}

static ConfigValue *
impl_ConfigDatabase_lookup(PortableServer_Servant servant,
                           const CORBA_char * key,
                           CORBA_Environment * ev)
{
  if (gconfd_check_in_shutdown (ev))
    return gconf_invalid_corba_value ();
  
  return impl_ConfigDatabase_lookup_with_locale (servant, key,
                                                 NULL, TRUE, NULL,
                                                 NULL, ev);
}

static ConfigValue*
impl_ConfigDatabase_lookup_default_value(PortableServer_Servant servant,
                                         const CORBA_char * key,
                                         const CORBA_char * locale,
                                         CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GConfValue* val;
  GError* error = NULL;
  GConfLocaleList* locale_list;  

  if (gconfd_check_in_shutdown (ev))
    return gconf_invalid_corba_value ();
  
  locale_list = gconfd_locale_cache_lookup(locale);
  
  val = gconf_database_query_default_value(db, key,
                                           locale_list->list,
                                           NULL,
                                           &error);

  gconf_locale_list_unref(locale_list);
  
  if (val != NULL)
    {
      ConfigValue* cval = gconf_corba_value_from_gconf_value(val);

      gconf_value_free(val);

      g_return_val_if_fail(error == NULL, cval);
      
      return cval;
    }
  else
    {
      gconf_set_exception(&error, ev);

      return gconf_invalid_corba_value ();
    }
}

static void
impl_ConfigDatabase_batch_lookup(PortableServer_Servant servant,
				 const ConfigDatabase_KeyList * keys,
				 const CORBA_char * locale,
				 ConfigDatabase_ValueList ** values,
				 ConfigDatabase_IsDefaultList ** is_defaults,
				 ConfigDatabase_IsWritableList ** is_writables,
                                 CORBA_Environment * ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
}

static void
impl_ConfigDatabase_set(PortableServer_Servant servant,
			const CORBA_char * key,
			const ConfigValue * value,
                        CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GConfValue* val;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  if (value->_d == InvalidVal)
    {
      gconf_log(GCL_ERR, _("Received invalid value in set request"));
      return;
    }

  val = gconf_value_from_corba_value(value);

  if (val == NULL)
    {
      gconf_log(GCL_ERR, _("Couldn't make sense of CORBA value received in set request for key `%s'"), key);
      return;
    }
      
#if 0
  {
    gchar* str = gconf_value_to_string(val);

    /* reduce traffice to the logfile */
    gconf_log(GCL_DEBUG, "Received request to set key `%s' to `%s'", key, str);

    g_free(str);
  }
#endif
  
  gconf_database_set(db, key, val, value, &error);

  gconf_set_exception(&error, ev);

  gconf_value_free(val);
}

static void
impl_ConfigDatabase_unset_with_locale (PortableServer_Servant servant,
                                       const CORBA_char * key,
                                       const CORBA_char * locale,
                                       CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_database_unset(db, key, locale, &error);

  gconf_set_exception(&error, ev);
}

static void
impl_ConfigDatabase_unset(PortableServer_Servant servant,
			  const CORBA_char * key,
                          CORBA_Environment * ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  /* This is a cheat, since const CORBA_char* isn't normally NULL */
  impl_ConfigDatabase_unset_with_locale (servant, key, NULL, ev);
}

static void
impl_ConfigDatabase_batch_change (PortableServer_Servant servant,
                                  const CORBA_char * locale,
                                  const ConfigDatabase_KeyList * keys,
                                  const ConfigDatabase_ValueList * values,
                                  CORBA_Environment * ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
}

static CORBA_boolean
impl_ConfigDatabase_dir_exists(PortableServer_Servant servant,
			       const CORBA_char * dir,
                               CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  CORBA_boolean retval;
  GError* error = NULL;  

  if (gconfd_check_in_shutdown (ev))
    return CORBA_FALSE;
  
  retval =
    gconf_database_dir_exists (db, dir, &error) ? CORBA_TRUE : CORBA_FALSE;

  gconf_set_exception(&error, ev);

  return retval;
}

static void
impl_ConfigDatabase_remove_dir(PortableServer_Servant servant,
			       const CORBA_char * dir,
                               CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError* error = NULL;  

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_database_remove_dir(db, dir, &error);

  gconf_set_exception(&error, ev);
}

static void
impl_ConfigDatabase_all_entries(PortableServer_Servant servant,
				const CORBA_char * dir,
				const CORBA_char * locale,
				ConfigDatabase_KeyList ** keys,
				ConfigDatabase_ValueList ** values,
				ConfigDatabase_IsDefaultList ** is_defaults,
                                ConfigDatabase_IsWritableList ** is_writables,
				CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GSList* pairs;
  guint n;
  GSList* tmp;
  guint i;
  GError* error = NULL;
  GConfLocaleList* locale_list;  

  if (gconfd_check_in_shutdown (ev))
    return;
  
  locale_list = gconfd_locale_cache_lookup(locale);
  
  pairs = gconf_database_all_entries(db, dir, locale_list->list, &error);
  
  gconf_locale_list_unref(locale_list);

  if (error != NULL)
    {
      gconf_set_exception(&error, ev);
      return;
    }
  
  n = g_slist_length(pairs);

  *keys= ConfigDatabase_KeyList__alloc();
  (*keys)->_buffer = CORBA_sequence_CORBA_string_allocbuf(n);
  (*keys)->_length = n;
  (*keys)->_maximum = n;
  (*keys)->_release = CORBA_TRUE; /* free buffer */
  
  *values= ConfigDatabase_ValueList__alloc();
  (*values)->_buffer = CORBA_sequence_ConfigValue_allocbuf(n);
  (*values)->_length = n;
  (*values)->_maximum = n;
  (*values)->_release = CORBA_TRUE; /* free buffer */

  *is_defaults = ConfigDatabase_IsDefaultList__alloc();
  (*is_defaults)->_buffer = CORBA_sequence_CORBA_boolean_allocbuf(n);
  (*is_defaults)->_length = n;
  (*is_defaults)->_maximum = n;
  (*is_defaults)->_release = CORBA_TRUE; /* free buffer */

  *is_writables = ConfigDatabase_IsWritableList__alloc();
  (*is_writables)->_buffer = CORBA_sequence_CORBA_boolean_allocbuf(n);
  (*is_writables)->_length = n;
  (*is_writables)->_maximum = n;
  (*is_writables)->_release = CORBA_TRUE; /* free buffer */
  
  tmp = pairs;
  i = 0;

  while (tmp != NULL)
    {
      GConfEntry* p = tmp->data;

      g_assert(p != NULL);
      g_assert(p->key != NULL);

      (*keys)->_buffer[i] = CORBA_string_dup(p->key);
      gconf_fill_corba_value_from_gconf_value (gconf_entry_get_value (p),
                                               &((*values)->_buffer[i]));
      (*is_defaults)->_buffer[i] = gconf_entry_get_is_default(p);
      (*is_writables)->_buffer[i] = gconf_entry_get_is_writable(p);
      
      gconf_entry_free(p);

      ++i;
      tmp = g_slist_next(tmp);
    }
  
  g_assert(i == n);

  g_slist_free(pairs);
}

static void
impl_ConfigDatabase_all_dirs(PortableServer_Servant servant,
			     const CORBA_char * dir,
			     ConfigDatabase_KeyList ** keys,
			     CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GSList* subdirs;
  guint n;
  GSList* tmp;
  guint i;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  subdirs = gconf_database_all_dirs (db, dir, &error);

  if (error != NULL)
    {
      /* I think this is right anyway... */
      gconf_set_exception (&error, ev);
      *keys = NULL;
      return;
    }
  
  n = g_slist_length (subdirs);

  *keys = ConfigDatabase_KeyList__alloc();
  (*keys)->_buffer = CORBA_sequence_CORBA_string_allocbuf(n);
  (*keys)->_length = n;
  (*keys)->_maximum = n;
  (*keys)->_release = CORBA_TRUE; /* free buffer */
  
  tmp = subdirs;
  i = 0;

  while (tmp != NULL)
    {
      gchar* subdir = tmp->data;

      (*keys)->_buffer[i] = CORBA_string_dup (subdir);

      g_free (subdir);

      ++i;
      tmp = g_slist_next (tmp);
    }
  
  g_assert (i == n);
  
  g_slist_free (subdirs);
}

static void
impl_ConfigDatabase_set_schema (PortableServer_Servant servant,
                                const CORBA_char * key,
                                const CORBA_char * schema_key,
                                CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_database_set_schema (db, key,
                             *schema_key != '\0' ?
                             schema_key : NULL,
                             &error);
  
  gconf_set_exception (&error, ev);
}

static void
impl_ConfigDatabase_sync (PortableServer_Servant servant,
                          CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_database_sync (db, &error);

  gconf_set_exception (&error, ev);
}

static void
impl_ConfigDatabase_clear_cache(PortableServer_Servant servant,
				CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_log (GCL_DEBUG, _("Received request to drop all cached data"));  
  
  gconf_database_clear_cache (db, &error);

  gconf_set_exception (&error, ev);
}

static void
impl_ConfigDatabase_synchronous_sync (PortableServer_Servant servant,
                                      CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError* error = NULL;

  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_log(GCL_DEBUG, _("Received request to sync synchronously"));
  
  
  gconf_database_synchronous_sync (db, &error);

  gconf_set_exception (&error, ev);
}


static ConfigValue*
impl_ConfigDatabase2_lookup_with_schema_name(PortableServer_Servant servant,
                                             const CORBA_char * key,
                                             const CORBA_char * locale,
                                             CORBA_boolean use_schema_default,
                                             CORBA_char    **schema_name,
                                             CORBA_boolean * value_is_default,
                                             CORBA_boolean * value_is_writable,
                                             CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GConfValue* val;
  GError* error = NULL;
  GConfLocaleList* locale_list;
  gboolean is_default = FALSE;
  gboolean is_writable = TRUE;
  char *s;
  ConfigValue* cval;
  
  if (gconfd_check_in_shutdown (ev))
    return gconf_invalid_corba_value ();
  
  locale_list = gconfd_locale_cache_lookup(locale);

  s = NULL;
  val = gconf_database_query_value(db, key, locale_list->list,
                                   use_schema_default,
                                   &s,
                                   &is_default,
                                   &is_writable,
                                   &error);

  *value_is_default = is_default;
  *value_is_writable = is_writable;

  if (s)
    *schema_name = CORBA_string_dup (s);
  else
    *schema_name = CORBA_string_dup ("");

  g_free (s);
  
  gconf_locale_list_unref(locale_list);
  
  if (val != NULL)
    {
      cval = gconf_corba_value_from_gconf_value(val);
      gconf_value_free(val);
      g_return_val_if_fail(error == NULL, cval);
    }
  else
    {
      cval = gconf_invalid_corba_value ();
    }

  gconf_log (GCL_DEBUG, "In lookup_with_schema_name returning schema name '%s' error '%s'",
             *schema_name, error ? error->message : "none");
  
  if (error != NULL)
    {
      gconf_set_exception (&error, ev);
    }

  return cval;
}

static void
impl_ConfigDatabase2_all_entries_with_schema_name(PortableServer_Servant servant,
                                                  const CORBA_char * dir,
                                                  const CORBA_char * locale,
                                                  ConfigDatabase_KeyList ** keys,
                                                  ConfigDatabase_ValueList ** values,
                                                  ConfigDatabase2_SchemaNameList **schema_names,
                                                  ConfigDatabase_IsDefaultList   **is_defaults,
                                                  ConfigDatabase_IsWritableList  **is_writables,
                                                  CORBA_Environment * ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GSList* pairs;
  guint n;
  GSList* tmp;
  guint i;
  GError* error = NULL;
  GConfLocaleList* locale_list;  

  if (gconfd_check_in_shutdown (ev))
    return;
  
  locale_list = gconfd_locale_cache_lookup(locale);
  
  pairs = gconf_database_all_entries(db, dir, locale_list->list, &error);
  
  gconf_locale_list_unref(locale_list);

  if (error != NULL)
    {
      gconf_set_exception(&error, ev);
      return;
    }
  
  n = g_slist_length(pairs);

  *keys= ConfigDatabase_KeyList__alloc();
  (*keys)->_buffer = CORBA_sequence_CORBA_string_allocbuf(n);
  (*keys)->_length = n;
  (*keys)->_maximum = n;
  (*keys)->_release = CORBA_TRUE; /* free buffer */
  
  *values= ConfigDatabase_ValueList__alloc();
  (*values)->_buffer = CORBA_sequence_ConfigValue_allocbuf(n);
  (*values)->_length = n;
  (*values)->_maximum = n;
  (*values)->_release = CORBA_TRUE; /* free buffer */

  *schema_names = ConfigDatabase2_SchemaNameList__alloc();
  (*schema_names)->_buffer = CORBA_sequence_CORBA_string_allocbuf(n);
  (*schema_names)->_length = n;
  (*schema_names)->_maximum = n;
  (*schema_names)->_release = CORBA_TRUE; /* free buffer */
  
  *is_defaults = ConfigDatabase_IsDefaultList__alloc();
  (*is_defaults)->_buffer = CORBA_sequence_CORBA_boolean_allocbuf(n);
  (*is_defaults)->_length = n;
  (*is_defaults)->_maximum = n;
  (*is_defaults)->_release = CORBA_TRUE; /* free buffer */

  *is_writables = ConfigDatabase_IsWritableList__alloc();
  (*is_writables)->_buffer = CORBA_sequence_CORBA_boolean_allocbuf(n);
  (*is_writables)->_length = n;
  (*is_writables)->_maximum = n;
  (*is_writables)->_release = CORBA_TRUE; /* free buffer */
  
  tmp = pairs;
  i = 0;

  while (tmp != NULL)
    {
      GConfEntry* p = tmp->data;

      g_assert(p != NULL);
      g_assert(p->key != NULL);

      (*keys)->_buffer[i] = CORBA_string_dup (p->key);
      gconf_fill_corba_value_from_gconf_value (gconf_entry_get_value (p),
                                               &((*values)->_buffer[i]));
      (*schema_names)->_buffer[i] = CORBA_string_dup (gconf_entry_get_schema_name (p));
      if ((*schema_names)->_buffer[i] == NULL)
        (*schema_names)->_buffer[i] = CORBA_string_dup ("");
      (*is_defaults)->_buffer[i] = gconf_entry_get_is_default(p);
      (*is_writables)->_buffer[i] = gconf_entry_get_is_writable(p);
      
      gconf_entry_free(p);

      ++i;
      tmp = g_slist_next(tmp);
    }
  
  g_assert(i == n);

  g_slist_free(pairs);
}

static CORBA_unsigned_long
impl_ConfigDatabase3_add_listener_with_properties (PortableServer_Servant servant,
                                                   const CORBA_char *where,
                                                   const ConfigListener who,
                                                   const ConfigDatabase3_PropList *properties,
                                                   CORBA_Environment *ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  const char *name = NULL;
  int i;
  
  if (gconfd_check_in_shutdown (ev))
    return 0;

  i = 0;
  while (i < (int) properties->_length)
    {
      if (strcmp (properties->_buffer[i].key, "name") == 0)
        name = properties->_buffer[i].value;

      ++i;
    }
  
  return gconf_database_add_listener (db, who, name, where);
}

static void
impl_ConfigDatabase3_recursive_unset (PortableServer_Servant servant,
                                      const CORBA_char *key,
                                      ConfigDatabase3_UnsetFlags flags,
                                      CORBA_Environment *ev)
{
  GConfDatabase *db = (GConfDatabase*) servant;
  GError *error;
  GConfUnsetFlags gconf_flags;
  
  if (gconfd_check_in_shutdown (ev))
    return;

  gconf_flags = 0;
  if (flags & ConfigDatabase3_UNSET_INCLUDING_SCHEMA_NAMES)
    gconf_flags |= GCONF_UNSET_INCLUDING_SCHEMA_NAMES;
  
  error = NULL;
  gconf_database_recursive_unset (db, key, NULL, gconf_flags, &error);

  gconf_set_exception (&error, ev);
}

static PortableServer_ServantBase__epv base_epv = {
  NULL,
  NULL,
  NULL
};

static POA_ConfigDatabase__epv server_epv = { 
  NULL,
  impl_ConfigDatabase_add_listener,
  impl_ConfigDatabase_remove_listener,
  impl_ConfigDatabase_lookup,
  impl_ConfigDatabase_lookup_with_locale,
  impl_ConfigDatabase_lookup_default_value,
  impl_ConfigDatabase_batch_lookup,
  impl_ConfigDatabase_set,
  impl_ConfigDatabase_unset,
  impl_ConfigDatabase_unset_with_locale,
  impl_ConfigDatabase_batch_change,
  impl_ConfigDatabase_dir_exists,
  impl_ConfigDatabase_remove_dir,
  impl_ConfigDatabase_all_entries,
  impl_ConfigDatabase_all_dirs,
  impl_ConfigDatabase_set_schema,
  impl_ConfigDatabase_sync,
  impl_ConfigDatabase_clear_cache,
  impl_ConfigDatabase_synchronous_sync
};

static POA_ConfigDatabase2__epv server2_epv = { 
  NULL,
  impl_ConfigDatabase2_lookup_with_schema_name,
  impl_ConfigDatabase2_all_entries_with_schema_name
};

static POA_ConfigDatabase3__epv server3_epv = { 
  NULL,
  impl_ConfigDatabase3_add_listener_with_properties,
  impl_ConfigDatabase3_recursive_unset
};

static POA_ConfigDatabase3__vepv poa_server_vepv = { &base_epv, &server_epv, &server2_epv, &server3_epv };

#endif /* HAVE_CORBA */

static void gconf_database_really_sync (GConfDatabase *db);
static void source_notify_cb           (GConfSource   *source,
					const gchar   *location,
					GConfDatabase *db);

void
gconf_database_set_sources (GConfDatabase *db,
                            GConfSources  *sources)
{
  if (db->sources != NULL)
    {
#ifdef HAVE_CORBA
      /* in the corba backend, this function should only be used when creating
       * the db; so assert when it was already created earlier (ie, when the
       * sources are already set) */
      g_assert_not_reached ();
#endif

      gconf_sources_clear_cache(db->sources);
      gconf_sources_free(db->sources);
    }

  db->sources = sources;

  gconf_sources_set_notify_func (db->sources,
				 (GConfSourceNotifyFunc) source_notify_cb,
				 db);
}

GConfDatabase*
gconf_database_new (GConfSources  *sources)
{
  GConfDatabase* db;
#ifdef HAVE_CORBA
  CORBA_Environment ev;
#endif
  
  db = g_new0 (GConfDatabase, 1);

#ifdef HAVE_CORBA
  db->servant._private = NULL;
  db->servant.vepv = &poa_server_vepv;

  CORBA_exception_init (&ev);
  
  POA_ConfigDatabase3__init (&db->servant, &ev);

  db->objref = PortableServer_POA_servant_to_reference (gconf_get_poa (),
                                                        &db->servant,
                                                        &ev);
  if (CORBA_Object_is_nil(db->objref, &ev))
    {
      gconf_log(GCL_ERR,
                _("Fatal error: failed to get object reference for ConfigDatabase"));

      exit (1);
    }
#endif

#ifdef HAVE_DBUS
  gconf_database_dbus_setup (db);
#endif

  db->listeners = gconf_listeners_new();

  gconf_database_set_sources(db, sources);

  db->last_access = time(NULL);

  db->sync_idle = 0;
  db->sync_timeout = 0;

  db->persistent_name = NULL;
  
  return db;
}

void
gconf_database_free (GConfDatabase *db)
{
#ifdef HAVE_CORBA
  PortableServer_ObjectId *oid;
  CORBA_Environment ev;

  CORBA_exception_init (&ev);
  
  CORBA_Object_release (db->objref, &ev);

  CORBA_exception_free (&ev);
  
  oid = PortableServer_POA_servant_to_id (gconf_get_poa(), &db->servant, &ev);

  CORBA_exception_free (&ev);
  
  PortableServer_POA_deactivate_object (gconf_get_poa (), oid, &ev);

  CORBA_exception_free (&ev);
  
  POA_ConfigDatabase3__fini (&db->servant, &ev);

  CORBA_free (oid);

  CORBA_exception_free (&ev);
#endif

#ifdef HAVE_DBUS
  gconf_database_dbus_teardown (db);
#endif

  if (db->listeners != NULL)
    {
      gboolean need_sync = FALSE;
      
      g_assert(db->sources != NULL);

      if (db->sync_idle != 0)
        {
          g_source_remove(db->sync_idle);
          db->sync_idle = 0;
          need_sync = TRUE;
        }

      if (db->sync_timeout != 0)
        {
          g_source_remove(db->sync_timeout);
          db->sync_timeout = 0;
          need_sync = TRUE;
        }

      if (need_sync)
        gconf_database_really_sync(db);
      
      gconf_listeners_free(db->listeners);
      gconf_sources_free(db->sources);
    }

  g_free (db->persistent_name);
  
  g_free (db);
}
  
#ifdef HAVE_CORBA
static gboolean
client_alive_predicate (const gchar* location,
                        guint        cnxn_id,
                        gpointer     listener_data,
                        gpointer     user_data)
{
  Listener *l = listener_data;
  CORBA_Environment ev;
  CORBA_boolean result;
  
  CORBA_exception_init (&ev);
  
  result = CORBA_Object_non_existent (l->obj, &ev);

  if (ev._major != CORBA_NO_EXCEPTION)
    {
      gconf_log (GCL_WARNING, "Exception from CORBA_Object_non_existent(), assuming stale listener %u",
                 cnxn_id);
      
      CORBA_exception_free (&ev);

      result = TRUE;
    }

  if (result)
    gconf_log (GCL_DEBUG, "Dropping dead listener %s (%u), appears to be nonexistent", l->name, cnxn_id);
  
  return result;
}
#endif

void
gconf_database_drop_dead_listeners (GConfDatabase *db)
{
#ifdef HAVE_CORBA
  if (db->listeners)
    {
      gconf_listeners_remove_if (db->listeners,
                                 client_alive_predicate,
                                 NULL);
    }
#endif
}

static gint
gconf_database_sync_idle (GConfDatabase* db)
{
  db->sync_idle = 0;

  /* could have been added before reaching the
   * idle
   */
  if (db->sync_timeout != 0)
    {
      g_source_remove (db->sync_timeout);
      db->sync_timeout = 0;
    }
  
  gconf_database_really_sync (db);
  
  /* Remove the idle function by returning FALSE */
  return FALSE; 
}

static gint
gconf_database_sync_timeout(GConfDatabase* db)
{
  db->sync_timeout = 0;
  
  /* Install the sync idle */
  if (db->sync_idle == 0)
    db->sync_idle = g_idle_add((GSourceFunc)gconf_database_sync_idle, db);

  gconf_log(GCL_DEBUG, "Sync queued one minute after changes occurred");
  
  /* Remove the timeout function by returning FALSE */
  return FALSE;
}

static void
gconf_database_really_sync(GConfDatabase* db)
{
  GError* error = NULL;
  
  if (!gconf_database_synchronous_sync(db, &error))
    {
      g_return_if_fail(error != NULL);

      gconf_log(GCL_ERR, _("Failed to sync one or more sources: %s"), 
                error->message);
      g_error_free(error);
    }
  else
    {
      gconf_log(GCL_DEBUG, "Sync completed without errors");
    }
}

static void
gconf_database_sync_nowish(GConfDatabase* db)
{
  /* Go ahead and sync as soon as the event loop quiets down */

  /* remove the scheduled sync */
  if (db->sync_timeout != 0)
    {
      g_source_remove(db->sync_timeout);
      db->sync_timeout = 0;
    }

  /* Schedule immediate post-quietdown sync */
  if (db->sync_idle == 0)
    db->sync_idle = g_idle_add((GSourceFunc)gconf_database_sync_idle, db);
}

static void
gconf_database_schedule_sync(GConfDatabase* db)
{
  /* Plan to sync within a minute or so */
  if (db->sync_idle != 0)
    return;
  else if (db->sync_timeout != 0)
    return;
  else
    {
      /* 1 minute timeout */
      db->sync_timeout = g_timeout_add_seconds(60, (GSourceFunc)gconf_database_sync_timeout, db);
    }
}

static void
source_notify_cb (GConfSource   *source,
		  const gchar   *location,
		  GConfDatabase *db)
{
  g_return_if_fail (source != NULL);
  g_return_if_fail (location != NULL);
  g_return_if_fail (db != NULL);

  if (gconf_sources_is_affected (db->sources, source, location))
    {
      GConfValue  *value;
#ifdef HAVE_CORBA
      ConfigValue *cvalue;
#endif
      GError      *error;
      gboolean     is_default;
      gboolean     is_writable;

      error = NULL;
      is_default = is_writable = FALSE;

      value = gconf_database_query_value (db,
					  location,
					  NULL,
					  TRUE,
					  NULL,
					  &is_default,
					  &is_writable,
					  &error);
      if (error != NULL)
	{
	  gconf_log (GCL_WARNING,
		     _("Error obtaining new value for `%s' after change notification from backend `%s': %s"),
		     location,
		     source->address,
		     error->message);
	  g_error_free (error);
	  return;
	}

#ifdef HAVE_CORBA
      cvalue = gconf_corba_value_from_gconf_value (value);
      gconf_database_notify_listeners (db,
				       NULL,
				       location,
				       cvalue,
				       is_default,
				       is_writable,
				       FALSE);
      CORBA_free (cvalue);
#endif

#ifdef HAVE_DBUS
      gconf_database_dbus_notify_listeners (db,
					    NULL,
					    location,
					    value,
					    is_default,
					    is_writable,
					    FALSE);
#endif
      gconf_value_free (value);
    }
}

#ifdef HAVE_CORBA
CORBA_unsigned_long
gconf_database_readd_listener   (GConfDatabase       *db,
                                 ConfigListener       who,
                                 const char          *name,
                                 const gchar         *where)
{
  Listener* l;
  guint cnxn;
  
  gconfd_need_log_cleanup ();
  
  g_assert(db->listeners != NULL);

  db->last_access = time(NULL);
  
  l = listener_new (who, name);

  cnxn = gconf_listeners_add (db->listeners, where, l,
                              (GFreeFunc)listener_destroy);

  gconf_sources_add_listener (db->sources, cnxn, where);

  if (l->name == NULL)
    l->name = g_strdup_printf ("%u", cnxn);
  
  gconf_log (GCL_DEBUG, "Added listener %s (%u)", l->name, cnxn);
  
  return cnxn;
}

CORBA_unsigned_long
gconf_database_add_listener    (GConfDatabase       *db,
                                ConfigListener       who,
                                const char          *name,
                                const gchar         *where)
{
  GError *err;
  CORBA_unsigned_long cnxn;  
  
  gconfd_need_log_cleanup ();
  
  cnxn = gconf_database_readd_listener (db, who, name, where);
  
  err = NULL;
  if (!gconfd_logfile_change_listener (db, TRUE, cnxn,
                                       who, where, &err))
    {
      /* This error is not fatal; we basically ignore it.
       * Because it's likely the right thing for the client
       * app to simply continue.
       */
      gconf_log (GCL_WARNING,
		 _("Failed to log addition of listener %s (%s); "
		   "will not be able to restore this listener on "
		   "gconfd restart, resulting in unreliable "
		   "notification of configuration changes."),
		 name, err->message);
      g_error_free (err);
    }
  
  return cnxn;
}

void
gconf_database_remove_listener (GConfDatabase       *db,
                                CORBA_unsigned_long  cnxn)
{
  union {
      Listener *l;
      gpointer l_ptr;
  } l = { NULL };
  GError *err;
  const gchar *location = NULL;

  gconfd_need_log_cleanup ();
  
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);

  gconf_log(GCL_DEBUG, "Removing listener %u", (guint)cnxn);

  if (!gconf_listeners_get_data (db->listeners, cnxn,
                                 &(l.l_ptr),
                                 &location))
    {
      gconf_log (GCL_WARNING, _("Listener ID %lu doesn't exist"),
                 (gulong) cnxn);
      return;
    }
  else
    {
      gconf_log (GCL_DEBUG, "Name of listener %u is %s",
                 (guint) cnxn, l.l->name);
    }
  
  err = NULL;
  if (!gconfd_logfile_change_listener (db, FALSE, cnxn,
                                       l.l->obj, location, &err))
    {
      gconf_log (GCL_WARNING, _("Failed to log removal of listener to logfile (most likely harmless, may result in a notification weirdly reappearing): %s"),
                 err->message);
      g_error_free (err);
    }
  
  gconf_sources_remove_listener (db->sources, cnxn);

  /* calls destroy notify */
  gconf_listeners_remove (db->listeners, cnxn);
}

typedef struct _ListenerNotifyClosure ListenerNotifyClosure;

struct _ListenerNotifyClosure {
  GConfDatabase* db;
  const ConfigValue* value;
  gboolean is_default;
  gboolean is_writable;
  GSList* dead;
  CORBA_Environment ev;
};

static void
notify_listeners_cb(GConfListeners* listeners,
                    const gchar* all_above_key,
                    guint cnxn_id,
                    gpointer listener_data,
                    gpointer user_data)
{
  Listener* l = listener_data;
  ListenerNotifyClosure* closure = user_data;
  
  ConfigListener_notify(l->obj,
                        closure->db->objref,
                        cnxn_id, 
                        (gchar*)all_above_key,
                        closure->value,
                        closure->is_default,
                        closure->is_writable,
                        &closure->ev);
  
  if(closure->ev._major != CORBA_NO_EXCEPTION) 
    {
      gconf_log (GCL_DEBUG, "Failed to notify listener %s (%u), removing: %s", 
                 l->name, cnxn_id, CORBA_exception_id (&closure->ev));
      CORBA_exception_free (&closure->ev);
      
      /* Dead listeners need to be forgotten */
      closure->dead = g_slist_prepend(closure->dead, GUINT_TO_POINTER(cnxn_id));
    }
  else
    {
      gconf_log (GCL_DEBUG, "Notified listener %s (%u) of change to key `%s'",
                 l->name, cnxn_id, all_above_key);
    }
}

void
gconf_database_notify_listeners (GConfDatabase       *db,
				 GConfSources        *modified_sources,
                                 const gchar         *key,
                                 const ConfigValue   *value,
                                 gboolean             is_default,
                                 gboolean             is_writable,
				 gboolean             notify_others)
{
  ListenerNotifyClosure closure;
  GSList* tmp;
  
  g_return_if_fail(db != NULL);

  closure.db = db;
  closure.value = value;
  closure.is_default = is_default;
  closure.is_writable = is_writable;
  closure.dead = NULL;
  
  CORBA_exception_init(&closure.ev);
  
  gconf_listeners_notify(db->listeners, key, notify_listeners_cb, &closure);

  tmp = closure.dead;

  if (tmp)
    gconfd_need_log_cleanup ();
  
  while (tmp != NULL)
    {
      guint dead = GPOINTER_TO_UINT(tmp->data);
      
      gconf_listeners_remove(db->listeners, dead);

      tmp = g_slist_next(tmp);
    }

  g_slist_free (closure.dead);

  if (modified_sources)
    {
      if (notify_others)
          gconfd_notify_other_listeners (db, modified_sources, key);

      g_list_free (modified_sources->sources);
      g_free (modified_sources);
    }
}
#endif

GConfValue*
gconf_database_query_value (GConfDatabase  *db,
                            const gchar    *key,
                            const gchar   **locales,
                            gboolean        use_schema_default,
                            char          **schema_name,
                            gboolean       *value_is_default,
                            gboolean       *value_is_writable,
                            GError    **err)
{
  GConfValue* val;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
  
  val = gconf_sources_query_value(db->sources, key, locales,
                                  use_schema_default,
                                  value_is_default,
                                  value_is_writable,
                                  schema_name,
                                  err);
  
  if (err && *err != NULL)
    {
      gconf_log(GCL_ERR, _("Error getting value for `%s': %s"),
                key, (*err)->message);
    }
  
  return val;
}

GConfValue*
gconf_database_query_default_value (GConfDatabase  *db,
                                    const gchar    *key,
                                    const gchar   **locales,
                                    gboolean       *is_writable,
                                    GError    **err)
{  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);

  return gconf_sources_query_default_value(db->sources, key, locales,
                                           is_writable,
                                           err);
}

void
gconf_database_set   (GConfDatabase      *db,
                      const gchar        *key,
                      GConfValue         *value,
#ifdef HAVE_CORBA
                      const ConfigValue  *cvalue,
#endif
                      GError        **err)
{
  GError *error = NULL;
  GConfSources *modified_sources;
  
  g_assert(db->listeners != NULL);
  g_return_if_fail(err == NULL || *err == NULL);
  
  db->last_access = time(NULL);

#if 0
  /* this really churns the logfile, so we avoid it */
  gconf_log(GCL_DEBUG, "Received request to set key `%s'", key);
#endif
  
  gconf_sources_set_value(db->sources, key, value, &modified_sources, &error);

  if (error)
    {
      g_assert (modified_sources == NULL);

      gconf_log(GCL_ERR, _("Error setting value for `%s': %s"),
                key, error->message);
      
      g_propagate_error (err, error);

      return;
    }
  else
    {
      gconf_database_schedule_sync(db);
      
      /* Can't possibly be the default, since we just set it,
       * and must be writable since setting it succeeded.
       */
#ifdef HAVE_CORBA
      gconf_database_notify_listeners (db,
				       modified_sources,
				       key,
				       cvalue,
                                       FALSE,
				       TRUE,
				       TRUE);
#endif
#ifdef HAVE_DBUS
      gconf_database_dbus_notify_listeners (db,
					    modified_sources,
					    key,
					    value,
					    FALSE,
					    TRUE,
					    TRUE);
#endif
    }
}

void
gconf_database_unset (GConfDatabase      *db,
                      const gchar        *key,
                      const gchar        *locale,
                      GError        **err)
{
#ifdef HAVE_CORBA
  ConfigValue* val;
#endif
  GError* error = NULL;
  GConfSources *modified_sources = NULL;
  
  g_return_if_fail(err == NULL || *err == NULL);
  
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
  
  gconf_log(GCL_DEBUG, "Received request to unset key `%s'", key);

  gconf_sources_unset_value(db->sources, key, locale, &modified_sources, &error);

  if (error != NULL)
    {
      g_assert (modified_sources == NULL);

      gconf_log(GCL_ERR, _("Error unsetting `%s': %s"),
                key, error->message);

      if (err)
        *err = error;
      else
        g_error_free(error);

      error = NULL;
    }
  else
    {
      GConfValue* def_value;
      const gchar* locale_list[] = { NULL, NULL };
      gboolean is_writable = TRUE;

      /* This is a somewhat dubious optimization
       * that assumes that if the unset was successful
       * the default value is the new value. Which is
       * safe for now, I _think_
       */
      locale_list[0] = locale;
      def_value = gconf_database_query_default_value(db,
                                                     key,
                                                     locale_list,
                                                     &is_writable,
                                                     err);

      if (err && *err)
        gconf_log(GCL_ERR, _("Error getting default value for `%s': %s"),
                  key, (*err)->message);

#ifdef HAVE_CORBA
      if (def_value != NULL)
        {
          val = gconf_corba_value_from_gconf_value(def_value);
          gconf_value_free(def_value);
        }
      else
        {
          val = gconf_invalid_corba_value ();
        }
          
      gconf_database_schedule_sync(db);

      gconf_database_notify_listeners(db,
				      modified_sources,
				      key,
				      val,
				      TRUE,
				      is_writable,
				      TRUE);
      CORBA_free(val);
#endif

#ifdef HAVE_DBUS
      gconf_database_schedule_sync(db);

      gconf_database_dbus_notify_listeners(db,
					   modified_sources,
					   key,
					   def_value,
					   TRUE,
					   is_writable,
					   TRUE);
      if (def_value)
	      gconf_value_free(def_value);
#endif
    }
}

void
gconf_database_recursive_unset (GConfDatabase      *db,
                                const gchar        *key,
                                const gchar        *locale,
                                GConfUnsetFlags     flags,
                                GError            **err)
{
#ifdef HAVE_CORBA
  ConfigValue* val;
#endif
  GError* error = NULL;
  GSList *notifies;
  GSList *tmp;
  
  g_return_if_fail (err == NULL || *err == NULL);
  
  g_assert (db->listeners != NULL);
  
  db->last_access = time (NULL);
  
  gconf_log (GCL_DEBUG, "Received request to recursively unset key \"%s\"", key);

  notifies = NULL;
  gconf_sources_recursive_unset (db->sources, key, locale,
                                 flags, &notifies, &error);

  /* We return the error but go ahead and finish the unset.
   * We're just returning the first error seen during the
   * unset process.
   */
  if (error != NULL)
    {
      g_assert (notifies == NULL);

      gconf_log (GCL_ERR, _("Error unsetting \"%s\": %s"),
                 key, error->message);

      if (err)
        *err = error;
      else
        g_error_free (error);

      error = NULL;
    }
  
  tmp = notifies;
  while (tmp != NULL)
    {
      GConfValue* new_value;
      const gchar* locale_list[] = { NULL, NULL };
      gboolean is_writable = TRUE;
      gboolean is_default = TRUE;
      GConfUnsetNotify *notify = tmp->data;

      locale_list[0] = locale;
      new_value = gconf_database_query_value (db,
                                              notify->key,
                                              locale_list,
                                              TRUE,
                                              NULL,
                                              &is_default,
                                              &is_writable,
                                              &error);
      if (error)
	{
	  gconf_log (GCL_ERR, _("Error getting new value for \"%s\": %s"),
		     notify->key, error->message);
	  g_propagate_error (err, error);
	  error = NULL;
	}

#ifdef HAVE_CORBA
      if (new_value != NULL)
        {
          val = gconf_corba_value_from_gconf_value (new_value);
          gconf_value_free (new_value);
        }
      else
        {
          val = gconf_invalid_corba_value ();
        }
          
      gconf_database_schedule_sync (db);

      gconf_database_notify_listeners (db,
				       notify->modified_sources,
				       notify->key,
				       val,
                                       is_default,
				       is_writable,
				       TRUE);
      
      CORBA_free (val);
#endif
#ifdef HAVE_DBUS
      gconf_database_schedule_sync (db);
      
      gconf_database_dbus_notify_listeners (db,
					    notify->modified_sources,
					    notify->key,
					    new_value,
					    is_default,
					    is_writable,
					    TRUE);
      if (new_value)
	      gconf_value_free (new_value);
#endif

      g_free (notify->key);
      g_free (notify);
      
      tmp = tmp->next;
    }

  g_slist_free (notifies);
}

gboolean
gconf_database_dir_exists  (GConfDatabase  *db,
                            const gchar    *dir,
                            GError    **err)
{
  gboolean ret;
  
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
  
  gconf_log(GCL_DEBUG, "Received dir_exists request for `%s'", dir);
  
  ret = gconf_sources_dir_exists(db->sources, dir, err);
  
  if (err && *err != NULL)
    {
      gconf_log(GCL_ERR, _("Error checking existence of `%s': %s"),
                 dir, (*err)->message);
      ret = FALSE;
    }

  return ret;
}

void
gconf_database_remove_dir  (GConfDatabase  *db,
                            const gchar    *dir,
                            GError    **err)
{  
  g_return_if_fail(err == NULL || *err == NULL);
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
  
  gconf_log (GCL_DEBUG, "Received request to remove directory \"%s\"", dir);
  
  gconf_sources_remove_dir(db->sources, dir, err);

  if (err && *err != NULL)
    {
      gconf_log (GCL_ERR, _("Error removing directory \"%s\": %s"),
                 dir, (*err)->message);
    }
  else
    {
      gconf_database_schedule_sync(db);
    }
}

GSList*
gconf_database_all_entries (GConfDatabase  *db,
                            const gchar    *dir,
                            const gchar   **locales,
                            GError    **err)
{
  GSList* entries;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
  
  entries = gconf_sources_all_entries(db->sources, dir, locales, err);

  if (err && *err != NULL)
    {
      gconf_log(GCL_ERR, _("Failed to get all entries in `%s': %s"),
                 dir, (*err)->message);
    }

  return entries;
}

GSList*
gconf_database_all_dirs (GConfDatabase  *db,
                         const gchar    *dir,
                         GError    **err)
{
  GSList* subdirs;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
    
  gconf_log (GCL_DEBUG, "Received request to list all subdirs in `%s'", dir);

  subdirs = gconf_sources_all_dirs (db->sources, dir, err);

  if (err && *err != NULL)
    {
      gconf_log (GCL_ERR, _("Error listing dirs in `%s': %s"),
                 dir, (*err)->message);
    }
  return subdirs;
}

void
gconf_database_set_schema (GConfDatabase  *db,
                           const gchar    *key,
                           const gchar    *schema_key,
                           GError        **err)
{
  g_return_if_fail (err == NULL || *err == NULL);
  g_assert (db->listeners != NULL);
  
  db->last_access = time (NULL);
  
  gconf_sources_set_schema (db->sources, key, schema_key, err);

  if (err && *err != NULL)
    {
      gconf_log (GCL_ERR, _("Error setting schema for `%s': %s"),
                key, (*err)->message);
    }
  else
    {
      gconf_database_schedule_sync (db);
    }
}

void
gconf_database_sync (GConfDatabase  *db,
                     GError    **err)
{
  g_assert(db->listeners != NULL);
  
  db->last_access = time(NULL);
  
  gconf_log(GCL_DEBUG, "Received suggestion to sync all configuration data");

  gconf_database_sync_nowish(db);
}

gboolean
gconf_database_synchronous_sync (GConfDatabase  *db,
                                 GError    **err)
{  
  /* remove the scheduled syncs */
  if (db->sync_timeout != 0)
    {
      g_source_remove(db->sync_timeout);
      db->sync_timeout = 0;
    }

  if (db->sync_idle != 0)
    {
      g_source_remove(db->sync_idle);
      db->sync_idle = 0;
    }

  db->last_access = time(NULL);
  
  return gconf_sources_sync_all(db->sources, err);
}

void
gconf_database_clear_cache (GConfDatabase  *db,
                            GError    **err)
{
  g_assert(db->listeners != NULL);

  db->last_access = time(NULL);

  gconf_sources_clear_cache(db->sources);
}

void
gconf_database_clear_cache_for_sources (GConfDatabase  *db,
					GConfSources   *sources,
					GError        **err)
{
  g_assert(db->listeners != NULL);

  db->last_access = time(NULL);

  gconf_sources_clear_cache_for_sources(db->sources, sources);
}

const gchar *
gconf_database_get_persistent_name (GConfDatabase *db)
{
  GList   *tmp;
  GString *str = NULL;

  if (db->persistent_name != NULL)
    return db->persistent_name;

  if (db->sources == NULL || db->sources->sources == NULL)
    {
      db->persistent_name = g_strdup ("empty");
      return db->persistent_name;
    }

  tmp = db->sources->sources;
  while (tmp != NULL)
    {
      GConfSource *source = tmp->data;

      if (str == NULL)
	{
	  str = g_string_new (source->address);
	}
      else
        {
          g_string_append_c (str, GCONF_DATABASE_LIST_DELIM);
          g_string_append (str, source->address);
        }

      tmp = tmp->next;
    }

  g_assert (str != NULL);

  db->persistent_name = str->str;
  g_string_free (str, FALSE);

  return db->persistent_name;
}

#ifdef HAVE_CORBA
struct ForeachData
{
  GString *str;
  gchar *db_name;
};

static void
listener_save_foreach (const gchar* location,
                       guint cnxn_id,
                       gpointer listener_data,
                       gpointer user_data)
{
  struct ForeachData *fd = user_data;
  Listener* l = listener_data;
  CORBA_ORB orb;
  CORBA_Environment ev;
  gchar *ior;
  gchar *s;

  gconf_log (GCL_DEBUG, "Saving listener %s (%u) to log file", l->name,
             (guint) cnxn_id);
  
  s = g_strdup_printf ("ADD %u %s ", cnxn_id, fd->db_name);

  g_string_append (fd->str, s);

  g_free (s);

  s = gconf_quote_string (location);
  g_string_append (fd->str, s);
  g_free (s);
  g_string_append_c (fd->str, ' ');
  
  orb = gconf_orb_get ();

  CORBA_exception_init (&ev);
  
  ior = CORBA_ORB_object_to_string(orb, l->obj, &ev);

  s = gconf_quote_string (ior);

  g_string_append (fd->str, s);

  g_free (s);
  
  CORBA_free(ior);

  g_string_append_c (fd->str, '\n');
}

void
gconf_database_log_listeners_to_string (GConfDatabase *db,
                                        gboolean is_default,
                                        GString *str)
{
  struct ForeachData fd;

  fd.str = str;
  
  if (is_default)
    fd.db_name = gconf_quote_string ("def");
  else
    {
      fd.db_name =
        gconf_quote_string (gconf_database_get_persistent_name (db));
    }
        
  gconf_listeners_foreach (db->listeners,
                           listener_save_foreach,
                           &fd);

  g_free (fd.db_name);
}
#endif

/*
 * Locale hash
 */

static GConfLocaleCache* locale_cache = NULL;

GConfLocaleList*
gconfd_locale_cache_lookup (const gchar *locale)
{
  GConfLocaleList* locale_list;
  
  if (locale_cache == NULL)
    locale_cache = gconf_locale_cache_new();

  locale_list = gconf_locale_cache_get_list(locale_cache, locale);

  g_assert(locale_list != NULL);
  g_assert(locale_list->list != NULL);
  
  return locale_list;
}

void
gconfd_locale_cache_expire(void)
{
  if (locale_cache != NULL)
    gconf_locale_cache_expire(locale_cache, 60 * 30); /* 60 sec * 30 min */
}

void
gconfd_locale_cache_drop(void)
{
  if (locale_cache != NULL)
    {
      gconf_locale_cache_free(locale_cache);
      locale_cache = NULL;
    }
}

#ifdef HAVE_CORBA
/*
 * The listener object
 */

static Listener* 
listener_new (ConfigListener obj,
              const char    *name)
{
  Listener* l;
  CORBA_Environment ev;

  CORBA_exception_init (&ev);

  l = g_new0 (Listener, 1);

  l->obj = CORBA_Object_duplicate (obj, &ev);
  l->name = g_strdup (name);
  
  return l;
}

static void      
listener_destroy (Listener* l)
{  
  CORBA_Environment ev;

  CORBA_exception_init (&ev);
  CORBA_Object_release (l->obj, &ev);
  g_free (l->name);
  g_free (l);
}
#endif



