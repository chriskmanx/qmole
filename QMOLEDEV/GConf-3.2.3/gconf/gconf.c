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

#if HAVE_CORBA
#include "GConfX.h"
#endif

#include "gconf.h"
#include "gconf-internals.h"
#include "gconf-sources.h"
#include "gconf-locale.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <sys/time.h>
#include <unistd.h>

#ifdef HAVE_CORBA
/* Returns TRUE if there was an error, frees exception, sets err */
static gboolean gconf_handle_corba_exception(CORBA_Environment* ev, GError** err);
/* just returns TRUE if there's an exception indicating the server is
   probably hosed; no side effects */
static gboolean gconf_server_broken(CORBA_Environment* ev);
static void gconf_detach_config_server(void);

/* Maximum number of times to try re-spawning the server if it's down. */
#define MAX_RETRIES 1
#endif /* HAVE_CORBA */

/* copied from gutf8.c where it exists as a (unfortunately) non-exported function */
static gchar *
utf8_make_valid (const gchar *name)
{
  GString *string;
  const gchar *remainder, *invalid;
  gint remaining_bytes, valid_bytes;
  
  string = NULL;
  remainder = name;
  remaining_bytes = strlen (name);
  
  while (remaining_bytes != 0) 
    {
      if (g_utf8_validate (remainder, remaining_bytes, &invalid)) 
        break;
      valid_bytes = invalid - remainder;
    
      if (string == NULL) 
        string = g_string_sized_new (remaining_bytes);

      g_string_append_len (string, remainder, valid_bytes);
      /* append U+FFFD REPLACEMENT CHARACTER */
      g_string_append (string, "\357\277\275");
      
      remaining_bytes -= valid_bytes + 1;
      remainder = invalid + 1;
    }
  
  if (string == NULL)
    return g_strdup (name);
  
  g_string_append (string, remainder);

  g_assert (g_utf8_validate (string->str, -1, NULL));
  
  return g_string_free (string, FALSE);
}

gboolean
gconf_key_check(const gchar* key, GError** err)
{
  gchar* why = NULL;

  if (key == NULL)
    {
      if (err)
        *err = gconf_error_new (GCONF_ERROR_BAD_KEY,
				_("Key is NULL"));
      return FALSE;
    }
  else if (!gconf_valid_key (key, &why))
    {
      if (err) {
        gchar *utf8_key = utf8_make_valid (key);
        *err = gconf_error_new (GCONF_ERROR_BAD_KEY, _("\"%s\": %s"),
                                utf8_key, why);
        g_free (utf8_key);
      }
      g_free(why);
      return FALSE;
    }
  return TRUE;
}

#ifdef HAVE_CORBA
typedef struct _CnxnTable CnxnTable;

struct _GConfEngine {
  guint refcount;

  ConfigDatabase database;

  CnxnTable* ctable;

  /* If non-NULL, this is a local engine;
     local engines don't do notification! */
  GConfSources* local_sources;
  
  /* A list of addresses that make up this db;
   * NULL if it uses the default db
   */
  GSList *addresses;

  /* A concatentation of the addresses above.
   */
  char *persistent_address;

  gpointer user_data;
  GDestroyNotify dnotify;

  gpointer owner;
  int owner_use_count;

  /* If TRUE, this is a local engine (and therefore
   * has no ctable and no notifications)
   */
  guint is_local : 1;
};

typedef struct _GConfCnxn GConfCnxn;

struct _GConfCnxn {
  gchar* namespace_section;
  guint client_id;
  CORBA_unsigned_long server_id; /* id returned from server */
  GConfEngine* conf;             /* engine we're associated with */
  GConfNotifyFunc func;
  gpointer user_data;
};

static GConfEngine *default_engine = NULL;

static GConfCnxn* gconf_cnxn_new     (GConfEngine         *conf,
                                      const gchar         *namespace_section,
                                      CORBA_unsigned_long  server_id,
                                      GConfNotifyFunc      func,
                                      gpointer             user_data);
static void       gconf_cnxn_destroy (GConfCnxn           *cnxn);
static void       gconf_cnxn_notify  (GConfCnxn           *cnxn,
                                      GConfEntry          *entry);


static ConfigServer   gconf_get_config_server    (gboolean     start_if_not_found,
                                                  GError **err);

/* Forget our current server object reference, so the next call to
   gconf_get_config_server will have to try to respawn the server */
static ConfigListener gconf_get_config_listener  (void);

static void           gconf_engine_detach       (GConfEngine     *conf);
static gboolean       gconf_engine_connect      (GConfEngine     *conf,
                                                 gboolean         start_if_not_found,
                                                 GError         **err);
static void           gconf_engine_set_database (GConfEngine     *conf,
                                                 ConfigDatabase   db);
static ConfigDatabase gconf_engine_get_database (GConfEngine     *conf,
                                                 gboolean         start_if_not_found,
                                                 GError         **err);


#define CHECK_OWNER_USE(engine)   \
  do { if ((engine)->owner && (engine)->owner_use_count == 0) \
     g_warning ("%s: You can't use a GConfEngine that has an active GConfClient wrapper object. Use GConfClient API instead.", G_STRFUNC);  \
  } while (0)

static void         register_engine           (GConfEngine    *conf);
static void         unregister_engine         (GConfEngine    *conf);
static GConfEngine *lookup_engine             (GSList         *addresses);
static GConfEngine *lookup_engine_by_database (ConfigDatabase  db);


/* We'll use client-specific connection numbers to return to library
   users, so if gconfd dies we can transparently re-register all our
   listener functions.  */

struct _CnxnTable {
  /* Hash from server-returned connection ID to GConfCnxn */
  GHashTable* server_ids;
  /* Hash from our connection ID to GConfCnxn */
  GHashTable* client_ids;
};

static CnxnTable* ctable_new                 (void);
static void       ctable_destroy             (CnxnTable           *ct);
static void       ctable_insert              (CnxnTable           *ct,
                                              GConfCnxn           *cnxn);
static void       ctable_remove              (CnxnTable           *ct,
                                              GConfCnxn           *cnxn);
static GSList*    ctable_remove_by_conf      (CnxnTable           *ct,
                                              GConfEngine         *conf);
static GConfCnxn* ctable_lookup_by_client_id (CnxnTable           *ct,
                                              guint                client_id);
static GConfCnxn* ctable_lookup_by_server_id (CnxnTable           *ct,
                                              CORBA_unsigned_long  server_id);
static void       ctable_reinstall           (CnxnTable           *ct,
                                              GConfCnxn           *cnxn,
                                              guint                old_server_id,
                                              guint                new_server_id);


static GConfEngine*
gconf_engine_blank (gboolean remote)
{
  GConfEngine* conf;

  _gconf_init_i18n ();
  
  conf = g_new0(GConfEngine, 1);

  conf->refcount = 1;
  
  conf->owner = NULL;
  conf->owner_use_count = 0;
  
  if (remote)
    {
      conf->database = CORBA_OBJECT_NIL;
      conf->ctable = ctable_new();
      conf->local_sources = NULL;
      conf->is_local = FALSE;
    }
  else
    {
      conf->database = CORBA_OBJECT_NIL;
      conf->ctable = NULL;
      conf->local_sources = NULL;
      conf->is_local = TRUE;
    }
  
  return conf;
}

void
gconf_engine_set_owner (GConfEngine *engine,
                        gpointer     client)
{
  g_return_if_fail (engine->owner_use_count == 0);
  
  engine->owner = client;
}

void
gconf_engine_push_owner_usage (GConfEngine *engine,
                               gpointer     client)
{
  g_return_if_fail (engine->owner == client);

  engine->owner_use_count += 1;
}

void
gconf_engine_pop_owner_usage  (GConfEngine *engine,
                               gpointer     client)
{
  g_return_if_fail (engine->owner == client);
  g_return_if_fail (engine->owner_use_count > 0);

  engine->owner_use_count -= 1;
}

static GHashTable *engines_by_db = NULL;

static GConfEngine *
lookup_engine_by_database (ConfigDatabase db)
{
  if (engines_by_db)
    return g_hash_table_lookup (engines_by_db, db);
  else
    return NULL;
}

static void
database_rec_release (gpointer rec)
{
  GConfEngine *conf = rec;
  CORBA_Environment ev;

  CORBA_exception_init (&ev);

  CORBA_Object_release (conf->database, &ev);
  conf->database = CORBA_OBJECT_NIL;
  
  CORBA_exception_free (&ev);
}

/* This takes ownership of the ConfigDatabase */
static void
gconf_engine_set_database (GConfEngine *conf,
                           ConfigDatabase db)
{
  gconf_engine_detach (conf);

  conf->database = db;

  if (engines_by_db == NULL)
    engines_by_db = g_hash_table_new_full (
	    (GHashFunc) gconf_CORBA_Object_hash,
	    (GCompareFunc) gconf_CORBA_Object_equal,
	    NULL,
	    database_rec_release);
  
  g_hash_table_insert (engines_by_db, conf->database, conf);  
}

static void
gconf_engine_detach (GConfEngine *conf)
{
  if (conf->database != CORBA_OBJECT_NIL)
    {
      g_hash_table_remove (engines_by_db, conf->database);
    }
}

static gboolean
gconf_engine_connect (GConfEngine *conf,
                      gboolean start_if_not_found,
                      GError **err)
{
  ConfigServer cs;
  ConfigDatabase db;
  int tries = 0;
  CORBA_Environment ev;
  
  g_return_val_if_fail (!conf->is_local, TRUE);
  
  CORBA_exception_init(&ev);

  if (!CORBA_Object_is_nil (conf->database, &ev))
    return TRUE;
  
 RETRY:
      
  cs = gconf_get_config_server(start_if_not_found, err);
      
  if (cs == CORBA_OBJECT_NIL)
    return FALSE; /* Error should already be set */

  if (conf->addresses == NULL)
    {
      db = ConfigServer_get_default_database (cs, &ev);      
    }
  else if (conf->addresses->next == NULL) /* single element list */
    {
      db = ConfigServer_get_database (cs, conf->addresses->data, &ev);
    }
  else
    {
      ConfigServer2_AddressList *address_list;
      GSList                    *tmp;
      int                        i;

      address_list = ConfigServer2_AddressList__alloc ();
      address_list->_length  = address_list->_maximum = g_slist_length (conf->addresses);
      address_list->_buffer  = ConfigServer2_AddressList_allocbuf (address_list->_length);
      address_list->_release = CORBA_TRUE;

      i = 0;
      tmp = conf->addresses;
      while (tmp != NULL)
        {
          g_assert (i < address_list->_length);

          address_list->_buffer [i] = CORBA_string_dup (tmp->data);

          tmp = tmp->next;
          i++;
        }

      db = ConfigServer2_get_database_for_addresses ((ConfigServer2) cs, address_list, &ev);

      CORBA_free (address_list);
    }

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_detach_config_server();
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    return FALSE;

  if (CORBA_Object_is_nil (db, &ev))
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_BAD_ADDRESS,
                               _("Server couldn't resolve the address `%s'"),
                               conf->persistent_address);
          
      return FALSE;
    }

  gconf_engine_set_database (conf, db);
  
  return TRUE;
}

static ConfigDatabase
gconf_engine_get_database (GConfEngine *conf,
                           gboolean start_if_not_found,
                           GError **err)
{
  if (!gconf_engine_connect (conf, start_if_not_found, err))
    return CORBA_OBJECT_NIL;
  else
    return conf->database;
}

static gboolean
gconf_engine_is_local(GConfEngine* conf)
{
  return conf->is_local;
}

static GHashTable *engines_by_address = NULL;

static void
register_engine (GConfEngine *conf)
{
  g_return_if_fail (conf->addresses != NULL);

  g_assert (conf->persistent_address == NULL);

  conf->persistent_address = 
          gconf_address_list_get_persistent_name (conf->addresses);

  if (engines_by_address == NULL)
    engines_by_address = g_hash_table_new (g_str_hash, g_str_equal);

  g_hash_table_insert (engines_by_address, conf->persistent_address, conf);
}

static void
unregister_engine (GConfEngine *conf)
{
  g_return_if_fail (engines_by_address != NULL);

  g_assert (conf->persistent_address != NULL);
  
  g_hash_table_remove (engines_by_address, conf->persistent_address);
  g_free (conf->persistent_address);
  conf->persistent_address = NULL;

  if (g_hash_table_size (engines_by_address) == 0)
    {
      g_hash_table_destroy (engines_by_address);
      
      engines_by_address = NULL;
    }
}

static GConfEngine *
lookup_engine (GSList *addresses)
{
  if (engines_by_address != NULL)
    {
      GConfEngine *retval;
      char        *key;

      key = gconf_address_list_get_persistent_name (addresses);

      retval = g_hash_table_lookup (engines_by_address, key);

      g_free (key);

      return retval;
    }

  return NULL;
}


/*
 *  Public Interface
 */

GConfEngine*
gconf_engine_get_local      (const gchar* address,
                             GError** err)
{
  GConfEngine* conf;
  GConfSource* source;
  GConfSources* sources;

  g_return_val_if_fail(address != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  source = gconf_resolve_address(address, err);

  if (source == NULL)
    return NULL;

  sources = gconf_sources_new_from_source(source);
  if (sources == NULL)
    return NULL;

  conf = gconf_engine_blank(FALSE);

  conf->local_sources = sources;

  g_assert (gconf_engine_is_local (conf));
  
  return conf;
}

GConfEngine *
gconf_engine_get_local_for_addresses (GSList  *addresses,
				      GError **err)
{
  GConfEngine *conf;
  GConfSources* sources;

  g_return_val_if_fail (addresses != NULL, NULL);
  g_return_val_if_fail (err == NULL || *err == NULL, NULL);
  
  sources = gconf_sources_new_from_addresses (addresses, err);

  if (sources == NULL)
    return NULL;

  conf = gconf_engine_blank (FALSE);

  conf->local_sources = sources;

  g_assert (gconf_engine_is_local (conf));
  
  return conf;
}

/**
 * gconf_engine_get_default: (skip)
 *
 * Returns the default #GConfEngine. All clients should use this, unless 
 * they are special configuration-related tools. The caller of this
 * function assumes one reference count, and must call
 * gconf_engine_unref() at some point. It's fairly important to unref the
 * #GConfEngine, to cleanly close the connection to
 * <application>gconfd</application>. So if possible close the connection
 * before exiting your application.
 *
 * Return value: (transfer full): the default #GConfEngine.
 */
GConfEngine*
gconf_engine_get_default (void)
{
  GConfEngine* conf = NULL;
  const gchar* source_path;
  GError* err = NULL;
  
  if (default_engine)
    conf = default_engine;

  if (conf == NULL)
    {
      conf = gconf_engine_blank(TRUE);

      default_engine = conf;

      source_path = g_getenv ("GCONF_DEFAULT_SOURCE_PATH");
      if (source_path != NULL)
	{
	  conf->addresses = gconf_load_source_path (source_path, &err);
	  if (err)
	    {
	      g_warning ("Could not parse GCONF_DEFAULT_SOURCE_PATH: %s",
			 err->message);
	      g_error_free (err);
	    }
	}
      else
	conf->addresses = NULL;

      /* Ignore errors, we never return a NULL default database, and
       * since we aren't starting if it isn't found, we'll probably
       * get errors half the time anyway.
       */
      gconf_engine_connect (conf, FALSE, NULL);
    }
  else
    conf->refcount += 1;
  
  return conf;
}

GConfEngine*
gconf_engine_get_for_address (const char  *address,
			      GError     **err)
{
  GConfEngine *conf;
  GSList      *addresses;

  addresses = g_slist_append (NULL, g_strdup (address));

  conf = lookup_engine (addresses);

  if (conf == NULL)
    {
      conf = gconf_engine_blank (TRUE);

      conf->addresses = addresses;

      if (!gconf_engine_connect (conf, TRUE, err))
        {
          gconf_engine_unref (conf);
          return NULL;
        }

      register_engine (conf);
    }
  else
    {
      g_free (addresses->data);
      g_slist_free (addresses);
      conf->refcount += 1;
    }
  
  return conf;
}

GConfEngine*
gconf_engine_get_for_addresses (GSList *addresses, GError** err)
{
  GConfEngine* conf;

  conf = lookup_engine (addresses);

  if (conf == NULL)
    {
      GSList *tmp;

      conf = gconf_engine_blank (TRUE);

      conf->addresses = NULL;

      tmp = addresses;
      while (tmp != NULL)
        {
          conf->addresses = g_slist_append (conf->addresses,
                                            g_strdup (tmp->data));
          tmp = tmp->next;
        }

      if (!gconf_engine_connect (conf, TRUE, err))
        {
          gconf_engine_unref (conf);
          return NULL;
        }

      register_engine (conf);
    }
  else
    conf->refcount += 1;
  
  return conf;
}

void
gconf_engine_ref(GConfEngine* conf)
{
  g_return_if_fail(conf != NULL);
  g_return_if_fail(conf->refcount > 0);

  conf->refcount += 1;
}

void         
gconf_engine_unref(GConfEngine* conf)
{
  g_return_if_fail(conf != NULL);
  g_return_if_fail(conf->refcount > 0);

  conf->refcount -= 1;
  
  if (conf->refcount == 0)
    {
      if (gconf_engine_is_local(conf))
        {
          if (conf->local_sources != NULL)
            gconf_sources_free(conf->local_sources);
        }
      else
        {
          /* Remove all connections associated with this GConf */
          GSList* removed;
          GSList* tmp;
          CORBA_Environment ev;
      
          CORBA_exception_init(&ev);

          /* FIXME CnxnTable only has entries for this GConfEngine now,
           * it used to be global and shared among GConfEngine objects.
           */
          removed = ctable_remove_by_conf (conf->ctable, conf);
  
          tmp = removed;
          while (tmp != NULL)
            {
              GConfCnxn* gcnxn = tmp->data;

              if (!CORBA_Object_is_nil (conf->database, &ev))
                {
                  GError* err = NULL;
              
                  ConfigDatabase_remove_listener(conf->database,
                                                 gcnxn->server_id,
                                                 &ev);

                  if (gconf_handle_corba_exception(&ev, &err))
                    {
                      /* Don't set error because realistically this
                         doesn't matter to clients */
#ifdef GCONF_ENABLE_DEBUG
                      g_warning("Failure removing listener %u from the configuration server: %s",
                                (guint)gcnxn->server_id,
                                err->message);
#endif
                    }
                }

              gconf_cnxn_destroy(gcnxn);

              tmp = g_slist_next(tmp);
            }

          g_slist_free(removed);

          if (conf->dnotify)
            {
              (* conf->dnotify) (conf->user_data);
            }
          
          if (conf->addresses)
	    {
	      gconf_address_list_free (conf->addresses);
	      conf->addresses = NULL;
	    }

	  if (conf->persistent_address)
	    {
	      unregister_engine (conf);
	    }

          /* Release the ConfigDatabase */
          gconf_engine_detach (conf);
          
          ctable_destroy (conf->ctable);
        }

      if (conf == default_engine)
        default_engine = NULL;

      g_free(conf);
    }
}

void
gconf_engine_set_user_data  (GConfEngine   *engine,
                             gpointer       data,
                             GDestroyNotify dnotify)
{
  if (engine->dnotify)
    {
      (* engine->dnotify) (engine->user_data);
    }

  engine->dnotify = dnotify;
  engine->user_data = data;
}

gpointer
gconf_engine_get_user_data  (GConfEngine   *engine)
{
  return engine->user_data;
}

/**
 * gconf_engine_notify_add: (skip)
 * @conf: a #GConfEngine to monitor for changes.
 * @namespace_section: the directory or key to watch; you will be notified of changes at or below this point.
 * @func: the callback to invoke when a notification is received from the server.
 * @user_data: the data to pass to the callback.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 * Return value: an ID for the notification request, or 0 on error.
 *
 * Registers a notification request with the <application>gconfd</application>
 * server.  The server will notify the client when any key at or below
 * @namespace_section is set or unset. Try to watch the smallest possible part of
 * the namespace; otherwise you will slow down the server and your application with
 * unnecessary notifications. Note that you should prefer gconf_client_notify_add()
 * if you're using the #GObject wrapper library, because
 * gconf_client_notify_add() does not require a client-server conversation for
 * every callback. gconf_engine_notify_add() requests a different server notification for
 * every callback. The function returns an ID you can use to remove the
 * notification request; 0 is an invalid ID, and is returned if an error occurs.
 *
 * Returns value: an ID for the notification request, or 0 on error.
 */
guint
gconf_engine_notify_add(GConfEngine* conf,
                        const gchar* namespace_section,
                        GConfNotifyFunc func,
                        gpointer user_data,
                        GError** err)
{
  ConfigDatabase db;
  ConfigListener cl;
  gulong id;
  CORBA_Environment ev;
  GConfCnxn* cnxn;
  gint tries = 0;
  ConfigDatabase3_PropList properties;
#define NUM_PROPERTIES 1
  ConfigStringProperty properties_buffer[1];
  
  g_return_val_if_fail(!gconf_engine_is_local(conf), 0);

  CHECK_OWNER_USE (conf);
  
  if (gconf_engine_is_local(conf))
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_LOCAL_ENGINE,
                               _("Can't add notifications to a local configuration source"));

      return 0;
    }

  properties._buffer = properties_buffer;
  properties._length = NUM_PROPERTIES;
  properties._maximum = NUM_PROPERTIES;
  properties._release = CORBA_FALSE; /* don't free static buffer */

  properties._buffer[0].key = "name";
  properties._buffer[0].value = g_get_prgname ();
  if (properties._buffer[0].value == NULL)
    properties._buffer[0].value = "unknown";
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    return 0;

  cl = gconf_get_config_listener ();
  
  /* Should have aborted the program in this case probably */
  g_return_val_if_fail(cl != CORBA_OBJECT_NIL, 0);
  
  id = ConfigDatabase3_add_listener_with_properties (db,
                                                     (gchar*)namespace_section, 
                                                     cl,
                                                     &properties,
                                                     &ev);
  
  if (ev._major == CORBA_SYSTEM_EXCEPTION &&
      CORBA_exception_id (&ev) &&
      strcmp (CORBA_exception_id (&ev), "IDL:CORBA/BAD_OPERATION:1.0") == 0)
    {
      CORBA_exception_free (&ev);
      CORBA_exception_init (&ev);      
  
      id = ConfigDatabase_add_listener(db,
                                       (gchar*)namespace_section, 
                                       cl, &ev);
    }
  
  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    return 0;

  cnxn = gconf_cnxn_new(conf, namespace_section, id, func, user_data);

  ctable_insert(conf->ctable, cnxn);

  return cnxn->client_id;
}

void         
gconf_engine_notify_remove(GConfEngine* conf,
                           guint client_id)
{
  GConfCnxn* gcnxn;
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  CHECK_OWNER_USE (conf);
  
  if (gconf_engine_is_local(conf))
    return;
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, NULL);

  if (db == CORBA_OBJECT_NIL)
    return;

  gcnxn = ctable_lookup_by_client_id(conf->ctable, client_id);

  g_return_if_fail(gcnxn != NULL);

  ConfigDatabase_remove_listener(db,
                                 gcnxn->server_id,
                                 &ev);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, NULL))
    {
      ; /* do nothing */
    }
  

  /* We want to do this even if the CORBA fails, so if we restart gconfd and 
     reinstall listeners we don't reinstall this one. */
  ctable_remove(conf->ctable, gcnxn);

  gconf_cnxn_destroy(gcnxn);
}

GConfValue *
gconf_engine_get_fuller (GConfEngine *conf,
                         const gchar *key,
                         const gchar *locale,
                         gboolean use_schema_default,
                         gboolean *is_default_p,
                         gboolean *is_writable_p,
                         gchar   **schema_name_p,
                         GError **err)
{
  GConfValue* val;
  ConfigValue* cv;
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;
  CORBA_boolean is_default = FALSE;
  CORBA_boolean is_writable = TRUE;
  CORBA_char *corba_schema_name = NULL;
  
  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(key, err))
    return NULL;

  if (gconf_engine_is_local(conf))
    {
      gchar** locale_list;
      gboolean tmp_is_default = FALSE;
      gboolean tmp_is_writable = TRUE;
      gchar *tmp_schema_name = NULL;
      
      locale_list = gconf_split_locale(locale);
      
      val = gconf_sources_query_value(conf->local_sources,
                                      key,
                                      (const gchar**)locale_list,
                                      use_schema_default,
                                      &tmp_is_default,
                                      &tmp_is_writable,
                                      schema_name_p ? &tmp_schema_name : NULL,
                                      err);

      if (locale_list != NULL)
        g_strfreev(locale_list);
      
      if (is_default_p)
        *is_default_p = tmp_is_default;

      if (is_writable_p)
        *is_writable_p = tmp_is_writable;

      if (schema_name_p)
        *schema_name_p = tmp_schema_name;
      else
        g_free (tmp_schema_name);
      
      return val;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, NULL);

      return NULL;
    }

  if (schema_name_p)
    *schema_name_p = NULL;


  corba_schema_name = NULL;
  cv = ConfigDatabase2_lookup_with_schema_name (db,
                                                (gchar*)key, (gchar*)
                                                (locale ? locale : gconf_current_locale()),
                                                use_schema_default,
                                                &corba_schema_name,
                                                &is_default,
                                                &is_writable,
                                                &ev);

  if (ev._major == CORBA_SYSTEM_EXCEPTION &&
      CORBA_exception_id (&ev) &&
      strcmp (CORBA_exception_id (&ev), "IDL:CORBA/BAD_OPERATION:1.0") == 0)
    {
      CORBA_exception_free (&ev);
      CORBA_exception_init (&ev);
      
      cv = ConfigDatabase_lookup_with_locale(db,
                                             (gchar*)key, (gchar*)
                                             (locale ? locale : gconf_current_locale()),
                                             use_schema_default,
                                             &is_default,
                                             &is_writable,
                                             &ev);
    }
  
  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    {
      /* NOTE: don't free cv since we got an exception! */
      return NULL;
    }
  else
    {
      val = gconf_value_from_corba_value(cv);
      CORBA_free(cv);

      if (is_default_p)
        *is_default_p = !!is_default;
      if (is_writable_p)
        *is_writable_p = !!is_writable;

      /* we can't get a null pointer through corba
       * so the server sent us an empty string
       */
      if (corba_schema_name && corba_schema_name[0] != '/')
        {
          CORBA_free (corba_schema_name);
          corba_schema_name = NULL;
        }

      if (schema_name_p)
        *schema_name_p = g_strdup (corba_schema_name);

      if (corba_schema_name)
        CORBA_free (corba_schema_name);
      
      return val;
    }
}


GConfValue *
gconf_engine_get_full (GConfEngine *conf,
                       const gchar *key,
                       const gchar *locale,
                       gboolean use_schema_default,
                       gboolean *is_default_p,
                       gboolean *is_writable_p,
                       GError **err)
{
  return gconf_engine_get_fuller (conf, key, locale, use_schema_default,
                                  is_default_p, is_writable_p,
                                  NULL, err);
}

GConfEntry*
gconf_engine_get_entry(GConfEngine* conf,
                       const gchar* key,
                       const gchar* locale,
                       gboolean use_schema_default,
                       GError** err)
{
  gboolean is_writable = TRUE;
  gboolean is_default = FALSE;
  GConfValue *val;
  GError *error;
  GConfEntry *entry;
  gchar *schema_name;

  CHECK_OWNER_USE (conf);
  
  schema_name = NULL;
  error = NULL;
  val = gconf_engine_get_fuller (conf, key, locale, use_schema_default,
                                 &is_default, &is_writable,
                                 &schema_name, &error);
  if (error != NULL)
    {
      g_propagate_error (err, error);
      return NULL;
    }

  entry = gconf_entry_new_nocopy (g_strdup (key),
                                  val);

  gconf_entry_set_is_default (entry, is_default);
  gconf_entry_set_is_writable (entry, is_writable);
  gconf_entry_set_schema_name (entry, schema_name);
  g_free (schema_name);

  return entry;
}
     
GConfValue*  
gconf_engine_get (GConfEngine* conf, const gchar* key, GError** err)
{
  return gconf_engine_get_with_locale(conf, key, NULL, err);
}

GConfValue*
gconf_engine_get_with_locale(GConfEngine* conf, const gchar* key,
                             const gchar* locale,
                             GError** err)
{
  return gconf_engine_get_full(conf, key, locale, TRUE,
                               NULL, NULL, err);
}

GConfValue*
gconf_engine_get_without_default(GConfEngine* conf, const gchar* key,
                                 GError** err)
{
  return gconf_engine_get_full(conf, key, NULL, FALSE, NULL, NULL, err);
}

GConfValue*
gconf_engine_get_default_from_schema (GConfEngine* conf,
                                      const gchar* key,
                                      GError** err)
{
  GConfValue* val;
  ConfigValue* cv;
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(key, err))
    return NULL;

  if (gconf_engine_is_local(conf))
    {
      gchar** locale_list;

      locale_list = gconf_split_locale(gconf_current_locale());
      
      val = gconf_sources_query_default_value(conf->local_sources,
                                              key,
                                              (const gchar**)locale_list,
                                              NULL,
                                              err);

      if (locale_list != NULL)
        g_strfreev(locale_list);
      
      return val;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, NULL);

      return NULL;
    }

  cv = ConfigDatabase_lookup_default_value(db,
                                           (gchar*)key,
                                           (gchar*)gconf_current_locale(),
                                           &ev);
  
  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    {
      /* NOTE: don't free cv since we got an exception! */
      return NULL;
    }
  else
    {
      val = gconf_value_from_corba_value(cv);
      CORBA_free(cv);

      return val;
    }
}

gboolean
gconf_engine_set (GConfEngine* conf, const gchar* key,
                  const GConfValue* value, GError** err)
{
  ConfigValue* cv;
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(value != NULL, FALSE);
  g_return_val_if_fail(value->type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail( (value->type != GCONF_VALUE_STRING) ||
                        (gconf_value_get_string(value) != NULL) , FALSE );
  g_return_val_if_fail( (value->type != GCONF_VALUE_LIST) ||
                        (gconf_value_get_list_type(value) != GCONF_VALUE_INVALID), FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(key, err))
    return FALSE;

  if (!gconf_value_validate (value, err))
    return FALSE;
  
  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      
      gconf_sources_set_value(conf->local_sources, key, value, NULL, &error);

      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }
          return FALSE;
        }
      
      return TRUE;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, FALSE);

      return FALSE;
    }

  cv = gconf_corba_value_from_gconf_value (value);

  ConfigDatabase_set(db,
                     (gchar*)key, cv,
                     &ev);

  CORBA_free(cv);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    return FALSE;

  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  return TRUE;
}

gboolean
gconf_engine_unset (GConfEngine* conf, const gchar* key, GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_val_if_fail (conf != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(key, err))
    return FALSE;

  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      
      gconf_sources_unset_value(conf->local_sources, key, NULL, NULL, &error);

      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }
          return FALSE;
        }
      
      return TRUE;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, FALSE);

      return FALSE;
    }

  ConfigDatabase_unset (db,
                        (gchar*)key,
                        &ev);

  if (gconf_server_broken (&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach(conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception (&ev, err))
    return FALSE;

  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  return TRUE;
}

/**
 * gconf_engine_recursive_unset:
 * @engine: a #GConfEngine
 * @key: a key or directory name
 * @flags: change how the unset is done
 * @err: return location for a #GError, or %NULL to ignore errors
 * 
 * Unsets all keys below @key, including @key itself.  If any unset
 * fails, continues on to unset as much as it can. The first
 * failure is returned in @err.
 *
 * Returns: %FALSE if error is set
 **/
gboolean
gconf_engine_recursive_unset (GConfEngine    *conf,
                              const char     *key,
                              GConfUnsetFlags flags,
                              GError        **err)
{
  CORBA_Environment ev;
  ConfigDatabase3 db;
  gint tries = 0;
  ConfigDatabase3_UnsetFlags corba_flags;
  
  g_return_val_if_fail (conf != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check (key, err))
    return FALSE;

  if (gconf_engine_is_local (conf))
    {
      GError* error = NULL;
      
      gconf_sources_recursive_unset (conf->local_sources, key, NULL,
                                     flags, NULL, &error);

      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free (error);
            }
          return FALSE;
        }
      
      return TRUE;
    }

  g_assert (!gconf_engine_is_local (conf));
  
  CORBA_exception_init(&ev);

  corba_flags = 0;
  if (flags & GCONF_UNSET_INCLUDING_SCHEMA_NAMES)
    corba_flags |= ConfigDatabase3_UNSET_INCLUDING_SCHEMA_NAMES;
  
 RETRY:
  
  db = (ConfigDatabase3) gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail (err == NULL || *err != NULL, FALSE);

      return FALSE;
    }

  ConfigDatabase3_recursive_unset (db, key, corba_flags, &ev);

  if (gconf_server_broken (&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach(conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception (&ev, err))
    return FALSE;

  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  return TRUE;
}

gboolean
gconf_engine_associate_schema  (GConfEngine* conf, const gchar* key,
                                const gchar* schema_key, GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_val_if_fail (conf != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  if (!gconf_key_check (key, err))
    return FALSE;

  if (schema_key && !gconf_key_check (schema_key, err))
    return FALSE;

  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      
      gconf_sources_set_schema (conf->local_sources, key, schema_key, &error);

      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }
          return FALSE;
        }
      
      return TRUE;
    }

  g_assert (!gconf_engine_is_local (conf));
  
  CORBA_exception_init (&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail (err == NULL || *err != NULL, FALSE);

      return FALSE;
    }

  ConfigDatabase_set_schema (db,
                             key,
                             /* empty string means unset */
                             schema_key ? schema_key : "",
                             &ev);

  if (gconf_server_broken (&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free (&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    return FALSE;

  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  return TRUE;
}


static void
qualify_entries (GSList *entries, const char *dir)
{
  GSList *tmp = entries;
  while (tmp != NULL)
    {
      GConfEntry *entry = tmp->data;
      gchar *full;

      full = gconf_concat_dir_and_key (dir, entry->key);

      g_free (entry->key);
      entry->key = full;

      tmp = g_slist_next (tmp);
    }
}

/**
 * gconf_engine_all_entries:
 * @conf: a #GConfEngine.
 * @dir: Directory to list.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 *
 * Lists the key-value pairs in @dir. Does not list subdirectories; for
 * that use gconf_engine_all_dirs(). The returned list contains #GConfEntry
 * objects. A #GConfEntry contains an <emphasis>absolute</emphasis> key
 * and a value. The list is not recursive, it contains only the immediate
 * children of @dir.  To free the returned list, gconf_entry_free()
 * each list element, then g_slist_free() the list itself.
 *
 * Returns value: (element-type GConfEntry) (transfer full): List of #GConfEntry.
 */
GSList*      
gconf_engine_all_entries(GConfEngine* conf, const gchar* dir, GError** err)
{
  GSList* pairs = NULL;
  ConfigDatabase_ValueList* values;
  ConfigDatabase_KeyList* keys;
  ConfigDatabase_IsDefaultList* is_defaults;
  ConfigDatabase_IsWritableList* is_writables;
  ConfigDatabase2_SchemaNameList *schema_names;
  CORBA_Environment ev;
  ConfigDatabase db;
  guint i;
  gint tries = 0;

  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(dir != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(dir, err))
    return NULL;


  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      gchar** locale_list;
      GSList* retval;
      
      locale_list = gconf_split_locale(gconf_current_locale());
      
      retval = gconf_sources_all_entries(conf->local_sources,
                                         dir,
                                         (const gchar**)locale_list,
                                         &error);

      if (locale_list)
        g_strfreev(locale_list);
      
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }

          g_assert(retval == NULL);
          
          return NULL;
        }

      qualify_entries (retval, dir);
      
      return retval;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);
  
 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, NULL);

      return NULL;
    }

  schema_names = NULL;
  
  ConfigDatabase2_all_entries_with_schema_name (db,
                                                (gchar*)dir,
                                                (gchar*)gconf_current_locale(),
                                                &keys, &values, &schema_names,
                                                &is_defaults, &is_writables,
                                                &ev);
  
  if (ev._major == CORBA_SYSTEM_EXCEPTION &&
      CORBA_exception_id (&ev) &&
      strcmp (CORBA_exception_id (&ev), "IDL:CORBA/BAD_OPERATION:1.0") == 0)
    {
      CORBA_exception_free (&ev);
      CORBA_exception_init (&ev);
      
      ConfigDatabase_all_entries(db,
                                 (gchar*)dir,
                                 (gchar*)gconf_current_locale(),
                                 &keys, &values, &is_defaults, &is_writables,
                                 &ev);
    }

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))
    return NULL;
  
  if (keys->_length != values->_length)
    {
      g_warning("Received unmatched key/value sequences in %s",
                G_STRFUNC);
      return NULL;
    }

  i = 0;
  while (i < keys->_length)
    {
      GConfEntry* pair;

      pair = 
        gconf_entry_new_nocopy(gconf_concat_dir_and_key (dir, keys->_buffer[i]),
                               gconf_value_from_corba_value(&(values->_buffer[i])));

      gconf_entry_set_is_default (pair, is_defaults->_buffer[i]);
      gconf_entry_set_is_writable (pair, is_writables->_buffer[i]);
      if (schema_names)
        {
          /* empty string means no schema name */
          if (*(schema_names->_buffer[i]) != '\0')
            gconf_entry_set_schema_name (pair, schema_names->_buffer[i]);
        }
      
      pairs = g_slist_prepend(pairs, pair);
      
      ++i;
    }
  
  CORBA_free(keys);
  CORBA_free(values);
  CORBA_free(is_defaults);
  CORBA_free(is_writables);
  if (schema_names)
    CORBA_free (schema_names);
  
  return pairs;
}


static void
qualify_keys (GSList *keys, const char *dir)
{
  GSList *tmp = keys;
  while (tmp != NULL)
    {
      char *key = tmp->data;
      gchar *full;

      full = gconf_concat_dir_and_key (dir, key);

      g_free (tmp->data);
      tmp->data = full;

      tmp = g_slist_next (tmp);
    }
}

/**
 * gconf_engine_all_dirs:
 * @conf: a #GConfEngine.
 * @dir: Directory to get subdirectories from.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 *
 * Lists the subdirectories in @dir. The returned list contains
 * allocated strings. Each string is the absolute path of a
 * subdirectory. You should g_free() each string in the list, then
 * g_slist_free() the list itself.
 *
 * Returns value: (element-type utf8) (transfer full): List of allocated subdirectory names.
 */
GSList*      
gconf_engine_all_dirs(GConfEngine* conf, const gchar* dir, GError** err)
{
  GSList* subdirs = NULL;
  ConfigDatabase_KeyList* keys;
  CORBA_Environment ev;
  ConfigDatabase db;
  guint i;
  gint tries = 0;

  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(dir != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(dir, err))
    return NULL;

  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      GSList* retval;
      
      retval = gconf_sources_all_dirs(conf->local_sources,
                                      dir,
                                      &error);
      
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }

          g_assert(retval == NULL);
          
          return NULL;
        }

      qualify_keys (retval, dir);
      
      return retval;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);
  
 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(((err == NULL) || (*err && ((*err)->code == GCONF_ERROR_NO_SERVER))), NULL);

      return NULL;
    }
  
  ConfigDatabase_all_dirs(db,
                          (gchar*)dir, 
                          &keys,
                          &ev);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }

  if (gconf_handle_corba_exception(&ev, err))
    return NULL;
  
  i = 0;
  while (i < keys->_length)
    {
      gchar* s;

      s = gconf_concat_dir_and_key (dir, keys->_buffer[i]);
      
      subdirs = g_slist_prepend(subdirs, s);
      
      ++i;
    }
  
  CORBA_free(keys);

  return subdirs;
}

/* annoyingly, this is REQUIRED for local sources */
void 
gconf_engine_suggest_sync(GConfEngine* conf, GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_if_fail(conf != NULL);
  g_return_if_fail(err == NULL || *err == NULL);

  CHECK_OWNER_USE (conf);
  
  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      
      gconf_sources_sync_all(conf->local_sources,
                             &error);
      
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }
          return;
        }
      
      return;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_if_fail(err == NULL || *err != NULL);

      return;
    }

  ConfigDatabase_sync(db, &ev);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))  
    ; /* nothing additional */
}

void 
gconf_clear_cache(GConfEngine* conf, GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_if_fail(conf != NULL);
  g_return_if_fail(err == NULL || *err == NULL);

  /* don't disallow non-owner use here since you can't do this
   * via GConfClient API and calling this function won't break
   * GConfClient anyway
   */
  
  if (gconf_engine_is_local(conf))
    {
      gconf_sources_clear_cache(conf->local_sources);
      
      return;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_if_fail(err == NULL || *err != NULL);

      return;
    }

  ConfigDatabase_clear_cache(db, &ev);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))  
    ; /* nothing additional */
}

void 
gconf_synchronous_sync(GConfEngine* conf, GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_if_fail(conf != NULL);
  g_return_if_fail(err == NULL || *err == NULL);

  if (gconf_engine_is_local(conf))
    {
      GError* error = NULL;
      
      gconf_sources_sync_all(conf->local_sources, &error);
      
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              g_error_free(error);
            }
          return;
        }
      
      return;
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);

 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_if_fail(err == NULL || *err != NULL);

      return;
    }

  ConfigDatabase_synchronous_sync(db, &ev);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))  
    ; /* nothing additional */
}

gboolean
gconf_engine_dir_exists(GConfEngine *conf, const gchar *dir, GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  CORBA_boolean server_ret;
  gint tries = 0;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(dir != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);

  CHECK_OWNER_USE (conf);
  
  if (!gconf_key_check(dir, err))
    return FALSE;
  
  if (gconf_engine_is_local(conf))
    {
      return gconf_sources_dir_exists(conf->local_sources,
                                      dir,
                                      err);
    }

  g_assert(!gconf_engine_is_local(conf));
  
  CORBA_exception_init(&ev);
  
 RETRY:
  
  db = gconf_engine_get_database(conf, TRUE, err);
  
  if (db == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, FALSE);

      return FALSE;
    }
  
  server_ret = ConfigDatabase_dir_exists(db,
                                         (gchar*)dir,
                                         &ev);
  
  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  
  if (gconf_handle_corba_exception(&ev, err))  
    ; /* nothing */

  return (server_ret == CORBA_TRUE);
}

void
gconf_engine_remove_dir (GConfEngine* conf,
                         const gchar* dir,
                         GError** err)
{
  CORBA_Environment ev;
  ConfigDatabase db;
  gint tries = 0;

  g_return_if_fail(conf != NULL);
  g_return_if_fail(dir != NULL);
  g_return_if_fail(err == NULL || *err == NULL);

  /* FIXME we have no GConfClient method for doing this */
  /*   CHECK_OWNER_USE (conf); */
  
  if (!gconf_key_check(dir, err))
    return;

  if (gconf_engine_is_local(conf))
    {
      gconf_sources_remove_dir(conf->local_sources, dir, err);
      return;
    }

  CORBA_exception_init(&ev);
  
 RETRY:
  
  db = gconf_engine_get_database (conf, TRUE, err);

  if (db == CORBA_OBJECT_NIL)
    {
      g_return_if_fail(err == NULL || *err != NULL);
      return;
    }
  
  ConfigDatabase_remove_dir(db, (gchar*)dir, &ev);

  if (gconf_server_broken(&ev))
    {
      if (tries < MAX_RETRIES)
        {
          ++tries;
          CORBA_exception_free(&ev);
          gconf_engine_detach (conf);
          goto RETRY;
        }
    }
  gconf_handle_corba_exception(&ev, err);
  
  return;
}

gboolean
gconf_engine_key_is_writable  (GConfEngine *conf,
                               const gchar *key,
                               GError     **err)
{
  gboolean is_writable = TRUE;
  GConfValue *val;

  CHECK_OWNER_USE (conf);
  
  /* FIXME implement IDL to allow getting only writability
   * (not that urgent since GConfClient caches this crap
   * anyway)
   */
  
  val = gconf_engine_get_full(conf, key, NULL, TRUE,
                              NULL, &is_writable, err);

  gconf_value_free (val);
  
  return is_writable;
}

/*
 * Connection maintenance
 */

static GConfCnxn* 
gconf_cnxn_new(GConfEngine* conf,
               const gchar* namespace_section,
               CORBA_unsigned_long server_id,
               GConfNotifyFunc func,
               gpointer user_data)
{
  GConfCnxn* cnxn;
  static guint next_id = 1;
  
  cnxn = g_new0(GConfCnxn, 1);

  cnxn->namespace_section = g_strdup(namespace_section);
  cnxn->conf = conf;
  cnxn->server_id = server_id;
  cnxn->client_id = next_id;
  cnxn->func = func;
  cnxn->user_data = user_data;

  ++next_id;

  return cnxn;
}

static void      
gconf_cnxn_destroy(GConfCnxn* cnxn)
{
  g_free(cnxn->namespace_section);
  g_free(cnxn);
}

static void       
gconf_cnxn_notify(GConfCnxn* cnxn,
                  GConfEntry *entry)
{
  (*cnxn->func)(cnxn->conf, cnxn->client_id,
                entry,
                cnxn->user_data);
}

/*
 *  CORBA glue
 */

static ConfigServer   server = CORBA_OBJECT_NIL;

/* errors in here should be GCONF_ERROR_NO_SERVER */
static ConfigServer
try_to_contact_server (gboolean start_if_not_found,
                       GError **err)
{
  CORBA_Environment ev;
  
  /* Try to launch server */      
  server = gconf_activate_server (start_if_not_found,
                                  err);
    
  /* Try to ping server, by adding ourselves as a client */
  CORBA_exception_init (&ev);   

  if (!CORBA_Object_is_nil (server, &ev))
    {
      ConfigServer_add_client (server,
                               gconf_get_config_listener (),
                               &ev);
      
      if (ev._major != CORBA_NO_EXCEPTION)
	{
          g_set_error (err,
                       GCONF_ERROR,
                       GCONF_ERROR_NO_SERVER,
                       _("Adding client to server's list failed, CORBA error: %s"),
                       CORBA_exception_id (&ev));

	  CORBA_Object_release (server, &ev);
	  server = CORBA_OBJECT_NIL;
          CORBA_exception_free(&ev);
	}
    }

#ifdef GCONF_ENABLE_DEBUG      
  if (server == CORBA_OBJECT_NIL && start_if_not_found)
    g_return_val_if_fail (err == NULL || *err != NULL, server);
#endif
  
  return server;
}

/* All errors set in here should be GCONF_ERROR_NO_SERVER; should
   only set errors if start_if_not_found is TRUE */
static ConfigServer
gconf_get_config_server(gboolean start_if_not_found, GError** err)
{
  g_return_val_if_fail(err == NULL || *err == NULL, server);
  
  if (server != CORBA_OBJECT_NIL)
    return server;

  server = try_to_contact_server(start_if_not_found, err);
  
  return server; /* return what we have, NIL or not */
}

static ConfigListener listener = CORBA_OBJECT_NIL;

static void
gconf_detach_config_server(void)
{  
  CORBA_Environment ev;

  CORBA_exception_init(&ev);

  if (listener != CORBA_OBJECT_NIL)
    {
      CORBA_Object_release(listener, &ev);
      listener = CORBA_OBJECT_NIL;
    }

  if (server != CORBA_OBJECT_NIL)
    {
      CORBA_Object_release(server, &ev);

      if (ev._major != CORBA_NO_EXCEPTION)
        {
          g_warning("Exception releasing gconfd server object: %s",
                    CORBA_exception_id(&ev));
        }

      server = CORBA_OBJECT_NIL;
    }

  CORBA_exception_free(&ev);

  if (engines_by_db != NULL)
    {
      g_hash_table_destroy (engines_by_db);
      engines_by_db = NULL;
    }
}

/**
 * gconf_debug_shutdown:
 * @void: 
 * 
 * Detach from the configuration server and release
 * all related resources.
 *
 * Returns: 1 if an exception occurs, 0 otherwise.
 **/
int
gconf_debug_shutdown (void)
{
  gconf_detach_config_server ();

  return gconf_orb_release ();
}

static void notify                  (PortableServer_Servant     servant,
                                     ConfigDatabase             db,
                                     CORBA_unsigned_long        cnxn,
                                     const CORBA_char          *key,
                                     const ConfigValue         *value,
                                     CORBA_boolean              is_default,
                                     CORBA_boolean              is_writable,
                                     CORBA_Environment         *ev);
static void ping                    (PortableServer_Servant     _servant,
                                     CORBA_Environment         *ev);
static void update_listener         (PortableServer_Servant     _servant,
                                     ConfigDatabase             db,
                                     const CORBA_char          *address,
                                     const CORBA_unsigned_long  old_cnxn,
                                     const CORBA_char          *key,
                                     const CORBA_unsigned_long  new_cnxn,
                                     CORBA_Environment         *ev);
static void invalidate_cached_values(PortableServer_Servant     _servant,
                                     ConfigDatabase             database,
                                     const ConfigListener_KeyList *keys,
                                     CORBA_Environment         *ev);
static void drop_all_caches         (PortableServer_Servant     _servant,
                                     CORBA_Environment         *ev);



static PortableServer_ServantBase__epv base_epv = {
  NULL,
  NULL,
  NULL
};

static POA_ConfigListener__epv listener_epv = {
  NULL,
  notify,
  ping,
  update_listener,
  invalidate_cached_values,
  drop_all_caches
};

static POA_ConfigListener__vepv poa_listener_vepv = { &base_epv, &listener_epv };
static POA_ConfigListener poa_listener_servant = { NULL, &poa_listener_vepv };

static void 
notify(PortableServer_Servant servant,
       ConfigDatabase db,
       CORBA_unsigned_long server_id,
       const CORBA_char* key,
       const ConfigValue* value,
       CORBA_boolean is_default,
       CORBA_boolean is_writable,
       CORBA_Environment *ev)
{
  GConfCnxn* cnxn;
  GConfValue* gvalue;
  GConfEngine* conf;
  GConfEntry* entry;
  
  conf = lookup_engine_by_database (db);

  if (conf == NULL)
    {
#ifdef GCONF_ENABLE_DEBUG
      g_warning ("Client received notify for unknown database object");
#endif
      return;
    }
  
  cnxn = ctable_lookup_by_server_id(conf->ctable, server_id);
  
  if (cnxn == NULL)
    {
#ifdef GCONF_ENABLE_DEBUG
      g_warning("Client received notify for unknown connection ID %u",
                (guint)server_id);
#endif
      return;
    }

  gvalue = gconf_value_from_corba_value(value);

  entry = gconf_entry_new_nocopy (g_strdup (key),
                                  gvalue);
  gconf_entry_set_is_default (entry, is_default);
  gconf_entry_set_is_writable (entry, is_writable);
  
  gconf_cnxn_notify(cnxn, entry);

  gconf_entry_free (entry);
}

static void
ping (PortableServer_Servant _servant, CORBA_Environment * ev)
{
  /* This one is easy :-) */
  
  return;
}

static void
update_listener (PortableServer_Servant _servant,
                 ConfigDatabase             db,
                 const CORBA_char          *address,
                 const CORBA_unsigned_long  old_cnxn_id,
                 const CORBA_char          *key,
                 const CORBA_unsigned_long  new_cnxn_id,
                 CORBA_Environment         *ev_ignored)
{
  GConfCnxn* cnxn;
  GConfEngine* conf;
  CORBA_Environment ev;
  
  conf = lookup_engine_by_database (db);

  /* See if we have an old engine with a now-invalid object
     reference, and update its reference. */
  if (conf == NULL)
    {
      CORBA_exception_init (&ev);
      
      if (strcmp (address, "def") == 0)
        conf = default_engine;
      else
        {
          GSList  *addresses;

          addresses = gconf_persistent_name_get_address_list (address);
    
          conf = lookup_engine (addresses);
    
          gconf_address_list_free (addresses);
        }

      if (conf)
        gconf_engine_set_database (conf,
                                   CORBA_Object_duplicate (db, &ev));
    }
  
  if (conf == NULL)
    {
#ifdef GCONF_ENABLE_DEBUG
      g_warning("Client received listener update for unknown database "
                "(this is not a big deal, this warning only appears if GConf is compiled with debugging)");
#endif
      return;
    }
  
  cnxn = ctable_lookup_by_server_id (conf->ctable, old_cnxn_id);
  
  if (cnxn == NULL)
    {
#ifdef GCONF_ENABLE_DEBUG
      g_warning("Client received listener update for unknown listener ID %u "
                "(this is not a big deal, this warning only appears if GConf is compiled with debugging)",
                (guint)old_cnxn_id);
#endif
      return;
    }
  
  ctable_reinstall (conf->ctable, cnxn, old_cnxn_id, new_cnxn_id);
}

static void
invalidate_cached_values (PortableServer_Servant     _servant,
                          ConfigDatabase             database,
                          const ConfigListener_KeyList *keys,
                          CORBA_Environment         *ev)
{
#if 0
  g_warning ("FIXME process %d received request to invalidate some cached GConf values from the server, but right now we don't know how to do that (not implemented).", (int) getpid());
#endif
}

static void
drop_all_caches (PortableServer_Servant     _servant,
                 CORBA_Environment         *ev)
{
#if 0
  g_warning ("FIXME process %d received request to invalidate all cached GConf values from the server, but right now we don't know how to do that (not implemented).", (int) getpid());
#endif
}

static ConfigListener 
gconf_get_config_listener(void)
{  
  if (listener == CORBA_OBJECT_NIL)
    {
      CORBA_Environment ev;
      PortableServer_POA poa;
      PortableServer_POAManager poa_mgr;

      CORBA_exception_init (&ev);
      POA_ConfigListener__init (&poa_listener_servant, &ev);
      
      g_assert (ev._major == CORBA_NO_EXCEPTION);

      poa =
        (PortableServer_POA) CORBA_ORB_resolve_initial_references (gconf_orb_get (),
                                                                   "RootPOA", &ev);

      g_assert (ev._major == CORBA_NO_EXCEPTION);

      poa_mgr = PortableServer_POA__get_the_POAManager (poa, &ev);
      PortableServer_POAManager_activate (poa_mgr, &ev);

      g_assert (ev._major == CORBA_NO_EXCEPTION);

      listener = PortableServer_POA_servant_to_reference(poa,
                                                         &poa_listener_servant,
                                                         &ev);

      CORBA_Object_release ((CORBA_Object) poa_mgr, &ev);
      CORBA_Object_release ((CORBA_Object) poa, &ev);

      g_assert (listener != CORBA_OBJECT_NIL);
      g_assert (ev._major == CORBA_NO_EXCEPTION);
    }
  
  return listener;
}
#endif /* HAVE_CORBA */

void
gconf_preinit (gpointer app, gpointer mod_info)
{
  /* Deprecated */
}

void
gconf_postinit (gpointer app, gpointer mod_info)
{
  /* Deprecated */
}

/* All deprecated */
const char gconf_version[] = VERSION;

struct 
{
    const char * longName;
    char shortName;
    int argInfo;
    void * arg;
    int val;
    const char * descrip;
    const char * argDescrip;
} gconf_options[] = { { NULL } };

/* Also deprecated */
gboolean     
gconf_init (int argc, char **argv, GError** err)
{
  
  return TRUE;
}

gboolean
gconf_is_initialized (void)
{
  return TRUE;
}

/* 
 * Ampersand and <> are not allowed due to the XML backend; shell
 * special characters aren't allowed; others are just in case we need
 * some magic characters someday.  hyphen, underscore, period, colon
 * are allowed as separators. % disallowed to avoid printf confusion.
 */

/* Key/dir validity is exactly the same, except that '/' must be a dir, 
   but we are sort of ignoring that for now. */

/* Also, keys can contain only ASCII */

static const gchar invalid_chars[] = " \t\r\n\"$&<>,+=#!()'|{}[]?~`;%\\";

gboolean     
gconf_valid_key      (const gchar* key, gchar** why_invalid)
{
  const gchar* s = key;
  gboolean just_saw_slash = FALSE;

  /* Key must start with the root */
  if (*key != '/')
    {
      if (why_invalid != NULL)
        *why_invalid = g_strdup(_("Must begin with a slash '/'"));
      return FALSE;
    }
  
  /* Root key is a valid dir */
  if (*key == '/' && key[1] == '\0')
    return TRUE;

  while (*s)
    {
      if (just_saw_slash)
        {
          /* Can't have two slashes in a row, since it would mean
           * an empty spot.
           * Can't have a period right after a slash,
           * because it would be a pain for filesystem-based backends.
           */
          if (*s == '/' || *s == '.')
            {
              if (why_invalid != NULL)
                {
                  if (*s == '/')
                    *why_invalid = g_strdup(_("Can't have two slashes '/' in a row"));
                  else
                    *why_invalid = g_strdup(_("Can't have a period '.' right after a slash '/'"));
                }
              return FALSE;
            }
        }

      if (*s == '/')
        {
          just_saw_slash = TRUE;
        }
      else
        {
          const gchar* inv = invalid_chars;
          guchar c = (unsigned char) *s;

          just_saw_slash = FALSE;
          
          if (c > 127)
            {
              if (why_invalid != NULL)
                *why_invalid = g_strdup_printf (_("'\\%o' is not an ASCII character and thus isn't allowed in key names"),
                                                (guint) c);
              return FALSE;
            }
          
          while (*inv)
            {
              if (*inv == *s)
                {
                  if (why_invalid != NULL)
                    *why_invalid = g_strdup_printf(_("`%c' is an invalid character in key/directory names"), *s);
                  return FALSE;
                }
              ++inv;
            }
        }

      ++s;
    }

  /* Can't end with slash */
  if (just_saw_slash)
    {
      if (why_invalid != NULL)
        *why_invalid = g_strdup(_("Key/directory may not end with a slash '/'"));
      return FALSE;
    }
  else
    return TRUE;
}

/**
 * gconf_escape_key:
 * @arbitrary_text: some text in any encoding or format
 * @len: length of @arbitrary_text in bytes, or -1 if @arbitrary_text is nul-terminated
 * 
 * Escape @arbitrary_text such that it's a valid key element (i.e. one
 * part of the key path). The escaped key won't pass gconf_valid_key()
 * because it isn't a whole key (i.e. it doesn't have a preceding
 * slash), but prepending a slash to the escaped text should always
 * result in a valid key.
 * 
 * Return value: a nul-terminated valid GConf key
 **/
char*
gconf_escape_key (const char *arbitrary_text,
                  int         len)
{
  const char *p;
  const char *end;
  GString *retval;

  g_return_val_if_fail (arbitrary_text != NULL, NULL);
  
  /* Nearly all characters we would normally use for escaping aren't allowed in key
   * names, so we use @ for that.
   *
   * Invalid chars and @ itself are escaped as @xxx@ where xxx is the
   * Latin-1 value in decimal
   */

  if (len < 0)
    len = strlen (arbitrary_text);

  retval = g_string_sized_new (len);

  p = arbitrary_text;
  end = arbitrary_text + len;
  while (p != end)
    {
      if (*p == '/' || *p == '.' || *p == '@' || ((guchar) *p) > 127 ||
          strchr (invalid_chars, *p))
        {
          g_string_append_printf (retval, "@%u@", (guchar) *p);
        }
      else
        g_string_append_c (retval, *p);
      
      ++p;
    }

  return g_string_free (retval, FALSE);
}

/**
 * gconf_unescape_key:
 * @escaped_key: a key created with gconf_escape_key()
 * @len: length of @escaped_key in bytes, or -1 if @escaped_key is nul-terminated
 * 
 * Converts a string escaped with gconf_escape_key() back into its original
 * form.
 * 
 * Return value: the original string that was escaped to create @escaped_key
 **/
char*
gconf_unescape_key (const char *escaped_key,
                    int         len)
{
  const char *p;
  const char *end;
  const char *start_seq;
  GString *retval;

  g_return_val_if_fail (escaped_key != NULL, NULL);
  
  if (len < 0)
    len = strlen (escaped_key);

  retval = g_string_new (NULL);

  p = escaped_key;
  end = escaped_key + len;
  start_seq = NULL;
  while (p != end)
    {
      if (start_seq)
        {
          if (*p == '@')
            {
              /* *p is the @ that ends a seq */
              char *end_seq;
              guchar val;
              
              val = strtoul (start_seq, &end_seq, 10);
              if (start_seq != end_seq)
                g_string_append_c (retval, val);
              
              start_seq = NULL;
            }
        }
      else
        {
          if (*p == '@')
            start_seq = p + 1;
          else
            g_string_append_c (retval, *p);
        }

      ++p;
    }

  return g_string_free (retval, FALSE);
}


gboolean
gconf_key_is_below   (const gchar* above, const gchar* below)
{
  int len;

  if (above[0] == '/' && above[1] == '\0')
    return TRUE;
  
  len = strlen (above);
  if (strncmp (below, above, len) == 0)
    {
      /* only if this is a complete key component,
       * so that /foo is not above /foofoo/bar */
      if (below[len] == '\0' || below[len] == '/')
        return TRUE;
      else
	return FALSE;
    }
  else
    return FALSE;
}

gchar*
gconf_unique_key (void)
{
  /* This function is hardly cryptographically random but should be
     "good enough" */
  
  static guint serial = 0;
  gchar* key;
  guint t, ut, p, u, r;
  GTimeVal tv;
  
  g_get_current_time(&tv);
  
  t = tv.tv_sec;
  ut = tv.tv_usec;

  p = getpid();
  
#ifdef HAVE_GETUID
  u = getuid();
#else
  u = 0;
#endif

  /* don't bother to seed; if it's based on the time or any other
     changing info we can get, we may as well just use that changing
     info. since we don't seed we'll at least get a different number
     on every call to this function in the same executable. */
  r = rand();
  
  /* The letters may increase uniqueness by preventing "melds"
     i.e. 01t01k01 and 0101t0k1 are not the same */
  key = g_strdup_printf("%ut%uut%uu%up%ur%uk%u",
                        /* Duplicate keys must be generated
                           by two different program instances */
                        serial,
                        /* Duplicate keys must be generated
                           in the same microsecond */
                        t,
                        ut,
                        /* Duplicate keys must be generated by
                           the same user */
                        u,
                        /* Duplicate keys must be generated by
                           two programs that got the same PID */
                        p,
                        /* Duplicate keys must be generated with the
                           same random seed and the same index into
                           the series of pseudorandom values */
                        r,
                        /* Duplicate keys must result from running
                           this function at the same stack location */
                        GPOINTER_TO_UINT(&key));

  ++serial;
  
  return key;
}

#ifdef HAVE_CORBA
/*
 * Table of connections 
 */ 

static gint
corba_unsigned_long_equal (gconstpointer v1,
                           gconstpointer v2)
{
  return *((const CORBA_unsigned_long*) v1) == *((const CORBA_unsigned_long*) v2);
}

static guint
corba_unsigned_long_hash (gconstpointer v)
{
  /* for our purposes we can just assume 32 bits are significant */
  return (guint)(*(const CORBA_unsigned_long*) v);
}

static CnxnTable* 
ctable_new(void)
{
  CnxnTable* ct;

  ct = g_new(CnxnTable, 1);

  ct->server_ids = g_hash_table_new (corba_unsigned_long_hash,
                                     corba_unsigned_long_equal);  
  ct->client_ids = g_hash_table_new (g_int_hash, g_int_equal);
  
  return ct;
}

static void
ctable_destroy(CnxnTable* ct)
{
  g_hash_table_destroy (ct->server_ids);
  g_hash_table_destroy (ct->client_ids);
  g_free(ct);
}

static void       
ctable_insert(CnxnTable* ct, GConfCnxn* cnxn)
{
  g_hash_table_insert (ct->server_ids, &cnxn->server_id, cnxn);
  g_hash_table_insert (ct->client_ids, &cnxn->client_id, cnxn);
}

static void       
ctable_remove(CnxnTable* ct, GConfCnxn* cnxn)
{
  g_hash_table_remove (ct->server_ids, &cnxn->server_id);
  g_hash_table_remove (ct->client_ids, &cnxn->client_id);
}

struct RemoveData {
  GSList* removed;
  GConfEngine* conf;
  gboolean save_removed;
};

static gboolean
remove_by_conf(gpointer key, gpointer value, gpointer user_data)
{
  struct RemoveData* rd = user_data;
  GConfCnxn* cnxn = value;
  
  if (cnxn->conf == rd->conf)
    {
      if (rd->save_removed)
        rd->removed = g_slist_prepend(rd->removed, cnxn);

      return TRUE;  /* remove this one */
    }
  else 
    return FALSE; /* or not */
}

/* FIXME this no longer makes any sense, because a CnxnTable
   belongs to a GConfEngine and all entries have the same
   GConfEngine.
*/

/* We return a list of the removed GConfCnxn */
static GSList*      
ctable_remove_by_conf(CnxnTable* ct, GConfEngine* conf)
{
  guint client_ids_removed;
  guint server_ids_removed;
  struct RemoveData rd;

  rd.removed = NULL;
  rd.conf = conf;
  rd.save_removed = TRUE;
  
  client_ids_removed = g_hash_table_foreach_remove (ct->server_ids,
                                                    remove_by_conf,
                                                    &rd);

  rd.save_removed = FALSE;

  server_ids_removed = g_hash_table_foreach_remove(ct->client_ids,
                                                   remove_by_conf,
                                                   &rd);

  g_assert(client_ids_removed == server_ids_removed);
  g_assert(client_ids_removed == g_slist_length(rd.removed));

  return rd.removed;
}

static GConfCnxn* 
ctable_lookup_by_client_id(CnxnTable* ct, guint client_id)
{
  return g_hash_table_lookup(ct->client_ids, &client_id);
}

static GConfCnxn* 
ctable_lookup_by_server_id(CnxnTable* ct, CORBA_unsigned_long server_id)
{
  return g_hash_table_lookup (ct->server_ids, &server_id);
}

static void
ctable_reinstall (CnxnTable* ct,
                  GConfCnxn *cnxn,
                  guint old_server_id,
                  guint new_server_id)
{
  g_return_if_fail (cnxn->server_id == old_server_id);

  g_hash_table_remove (ct->server_ids, &old_server_id);
  
  cnxn->server_id = new_server_id;

  g_hash_table_insert (ct->server_ids, &cnxn->server_id, cnxn);
}

/*
 * Daemon control
 */

void          
gconf_shutdown_daemon (GError** err)
{
  CORBA_Environment ev;
  ConfigServer cs;

  cs = gconf_get_config_server (FALSE, err); /* Don't want to spawn it if it's already down */

  if (err && *err && (*err)->code == GCONF_ERROR_NO_SERVER)
    {
      /* No server is hardly an error here */
      g_error_free (*err);
      *err = NULL;
    }
  
  if (cs == CORBA_OBJECT_NIL)
    {      
      
      return;
    }

  CORBA_exception_init (&ev);

  ConfigServer_shutdown (cs, &ev);

  if (ev._major != CORBA_NO_EXCEPTION)
    {
      if (err)
        *err = gconf_error_new (GCONF_ERROR_FAILED, _("Failure shutting down configuration server: %s"),
                                CORBA_exception_id (&ev));

      CORBA_exception_free(&ev);
    }
}

gboolean
gconf_ping_daemon(void)
{
  ConfigServer cs;
  
  cs = gconf_get_config_server(FALSE, NULL); /* ignore error, since whole point is to see if server is reachable */

  if (cs == CORBA_OBJECT_NIL)
    return FALSE;
  else
    return TRUE;
}

gboolean
gconf_spawn_daemon(GError** err)
{
  ConfigServer cs;

  cs = gconf_get_config_server(TRUE, err);

  if (cs == CORBA_OBJECT_NIL)
    {
      g_return_val_if_fail(err == NULL || *err != NULL, FALSE);
      return FALSE; /* Failed to spawn, error should be set */
    }
  else
    return TRUE;
}
#endif /* HAVE_CORBA */

/*
 * Sugar functions 
 */

gdouble      
gconf_engine_get_float (GConfEngine* conf, const gchar* key,
                 GError** err)
{
  GConfValue* val;
  static const gdouble deflt = 0.0;
  
  g_return_val_if_fail(conf != NULL, 0.0);
  g_return_val_if_fail(key != NULL, 0.0);
  
  val = gconf_engine_get (conf, key, err);

  if (val == NULL)
    return deflt;
  else
    {
      gdouble retval;
      
      if (val->type != GCONF_VALUE_FLOAT)
        {
          if (err)
            *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH, _("Expected float, got %s"),
                                    gconf_value_type_to_string(val->type));
          gconf_value_free(val);
          return deflt;
        }

      retval = gconf_value_get_float(val);

      gconf_value_free(val);

      return retval;
    }
}

gint         
gconf_engine_get_int   (GConfEngine* conf, const gchar* key,
                 GError** err)
{
  GConfValue* val;
  static const gint deflt = 0;
  
  g_return_val_if_fail(conf != NULL, 0);
  g_return_val_if_fail(key != NULL, 0);
  
  val = gconf_engine_get (conf, key, err);

  if (val == NULL)
    return deflt;
  else
    {
      gint retval;

      if (val->type != GCONF_VALUE_INT)
        {
          if (err)
            *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH, _("Expected int, got %s"),
                                    gconf_value_type_to_string(val->type));
          gconf_value_free(val);
          return deflt;
        }

      retval = gconf_value_get_int(val);

      gconf_value_free(val);

      return retval;
    }
}

gchar*       
gconf_engine_get_string(GConfEngine* conf, const gchar* key,
                 GError** err)
{
  GConfValue* val;
  static const gchar* deflt = NULL;
  
  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  
  val = gconf_engine_get (conf, key, err);

  if (val == NULL)
    return g_strdup(deflt);
  else
    {
      gchar* retval;

      if (val->type != GCONF_VALUE_STRING)
        {
          if (err)
            *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH, _("Expected string, got %s"),
                                    gconf_value_type_to_string(val->type));
          gconf_value_free(val);
          return g_strdup(deflt);
        }

      retval = gconf_value_steal_string (val);
      gconf_value_free (val);

      return retval;
    }
}

gboolean     
gconf_engine_get_bool  (GConfEngine* conf, const gchar* key,
                        GError** err)
{
  GConfValue* val;
  static const gboolean deflt = FALSE;
  
  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  
  val = gconf_engine_get (conf, key, err);

  if (val == NULL)
    return deflt;
  else
    {
      gboolean retval;

      if (val->type != GCONF_VALUE_BOOL)
        {
          if (err)
            *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH, _("Expected bool, got %s"),
                                    gconf_value_type_to_string(val->type));
          gconf_value_free(val);
          return deflt;
        }

      retval = gconf_value_get_bool(val);

      gconf_value_free(val);

      return retval;
    }
}

/**
 * gconf_engine_get_schema: (skip)
 * @conf: a #GConfEngine.
 * @key: key you want the value of.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 * @Returns: the value of @key as an allocated #GConfSchema, or <symbol>NULL</symbol> if no value was obtained.
 *
 * Requests the schema (%GCONF_VALUE_SCHEMA) stored at @key.
 * Automatically performs type-checking, so if a non-schema is stored at
 * @key, an error is returned. If no value is set or an error occurs,
 * <symbol>NULL</symbol> is returned.
 *
 * Return value: (transfer full): the value of @key as an allocated #GConfSchema, or <symbol>NULL</symbol> if no value was obtained.
 */
GConfSchema* 
gconf_engine_get_schema  (GConfEngine* conf, const gchar* key, GError** err)
{
  GConfValue* val;

  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  
  val = gconf_engine_get_with_locale(conf, key, gconf_current_locale(), err);

  if (val == NULL)
    return NULL;
  else
    {
      GConfSchema* retval;

      if (val->type != GCONF_VALUE_SCHEMA)
        {
          if (err)
            *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH, _("Expected schema, got %s"),
                                    gconf_value_type_to_string(val->type));
          gconf_value_free(val);
          return NULL;
        }

      retval = gconf_value_steal_schema (val);
      gconf_value_free (val);

      return retval;
    }
}

/**
 * gconf_engine_get_list: (skip)
 * @conf: a #GConfEngine.
 * @key: key you want the value of.
 * @list_type: type of each list element.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 *
 * Requests the list (%GCONF_VALUE_LIST) stored at @key.  Automatically
 * performs type-checking, so if a non-list is stored at @key, or the
 * list does not contain elements of type @list_type, an error is
 * returned. If no value is set or an error occurs, <symbol>NULL</symbol>
 * is returned. Note that <symbol>NULL</symbol> is also the empty list,
 * so if you need to distinguish the empty list from an unset value, you
 * must use gconf_engine_get () to obtain a raw #GConfValue.
 *
 * <emphasis>Remember that GConf lists can only store primitive types:
 * %GCONF_VALUE_FLOAT, %GCONF_VALUE_INT, %GCONF_VALUE_BOOL,
 * %GCONF_VALUE_STRING, %GCONF_VALUE_SCHEMA.</emphasis> Also remember
 * that lists must be uniform, you may not mix types in the same list.
 *
 * The type of the list elements depends on @list_type. A #GConfValue
 * with type %GCONF_VALUE_LIST normally stores a list of more #GConfValue
 * objects. gconf_engine_get_list() automatically converts to primitive C
 * types. Thus, the list-&gt;data fields in the returned list
 * contain:
 *  
 * <informaltable pgwide="1" frame="none">
 * <tgroup cols="2"><colspec colwidth="2*"/><colspec colwidth="8*"/>
 * <tbody>
 *  
 * <row>
 * <entry>%GCONF_VALUE_INT</entry>
 * <entry>The integer itself, converted with GINT_TO_POINTER()</entry>
 * </row>
 *  
 * <row>
 * <entry>%GCONF_VALUE_BOOL</entry>
 * <entry>The bool itself, converted with GINT_TO_POINTER()</entry>
 * </row>
 *  
 * <row>
 * <entry>%GCONF_VALUE_FLOAT</entry>
 * <entry>A pointer to #gdouble, which should be freed with g_free()</entry>
 * </row>
 *  
 * <row>
 * <entry>%GCONF_VALUE_STRING</entry>
 * <entry>A pointer to #gchar, which should be freed with g_free()</entry>
 * </row>
 *  
 * <row>
 * <entry>%GCONF_VALUE_SCHEMA</entry>
 * <entry>A pointer to #GConfSchema, which should be freed with gconf_schema_free()</entry>
 * </row>
 *  
 * </tbody></tgroup></informaltable>
 *  
 * In the %GCONF_VALUE_FLOAT and %GCONF_VALUE_STRING cases, you must
 * g_free() each list element. In the %GCONF_VALUE_SCHEMA case you must
 * gconf_schema_free() each element. In all cases you must free the
 * list itself with g_slist_free().
 *
 * Return value: an allocated list, with elements as described above.
 */
GSList*
gconf_engine_get_list    (GConfEngine* conf, const gchar* key,
                          GConfValueType list_type, GError** err)
{
  GConfValue* val;

  g_return_val_if_fail(conf != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_INVALID, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_LIST, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_PAIR, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  val = gconf_engine_get_with_locale(conf, key, gconf_current_locale(), err);

  if (val == NULL)
    return NULL;
  else
    {
      /* This type-checks the value */
      return gconf_value_list_to_primitive_list_destructive(val, list_type, err);
    }
}

gboolean
gconf_engine_get_pair    (GConfEngine* conf, const gchar* key,
                   GConfValueType car_type, GConfValueType cdr_type,
                   gpointer car_retloc, gpointer cdr_retloc,
                   GError** err)
{
  GConfValue* val;
  GError* error = NULL;
  
  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(car_retloc != NULL, FALSE);
  g_return_val_if_fail(cdr_retloc != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);  
  
  val = gconf_engine_get_with_locale(conf, key, gconf_current_locale(), &error);

  if (error != NULL)
    {
      g_assert(val == NULL);
      
      if (err)
        *err = error;
      else
        g_error_free(error);

      return FALSE;
    }
  
  if (val == NULL)
    {
      return TRUE;
    }
  else
    {
      /* Destroys val */
      return gconf_value_pair_to_primitive_pair_destructive(val,
                                                            car_type, cdr_type,
                                                            car_retloc, cdr_retloc,
                                                            err);
    }
}

/*
 * Setters
 */

static gboolean
error_checked_set(GConfEngine* conf, const gchar* key,
                  GConfValue* gval, GError** err)
{
  GError* my_err = NULL;
  
  gconf_engine_set (conf, key, gval, &my_err);

  gconf_value_free(gval);
  
  if (my_err != NULL)
    {
      if (err)
        *err = my_err;
      else
        g_error_free(my_err);
      return FALSE;
    }
  else
    return TRUE;
}

gboolean
gconf_engine_set_float   (GConfEngine* conf, const gchar* key,
                          gdouble val, GError** err)
{
  GConfValue* gval;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  gval = gconf_value_new(GCONF_VALUE_FLOAT);

  gconf_value_set_float(gval, val);

  return error_checked_set(conf, key, gval, err);
}

gboolean
gconf_engine_set_int     (GConfEngine* conf, const gchar* key,
                          gint val, GError** err)
{
  GConfValue* gval;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  gval = gconf_value_new(GCONF_VALUE_INT);

  gconf_value_set_int(gval, val);

  return error_checked_set(conf, key, gval, err);
}

gboolean
gconf_engine_set_string  (GConfEngine* conf, const gchar* key,
                          const gchar* val, GError** err)
{
  GConfValue* gval;

  g_return_val_if_fail (val != NULL, FALSE);
  g_return_val_if_fail (conf != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  g_return_val_if_fail (g_utf8_validate (val, -1, NULL), FALSE);
  
  gval = gconf_value_new(GCONF_VALUE_STRING);

  gconf_value_set_string(gval, val);

  return error_checked_set(conf, key, gval, err);
}

gboolean
gconf_engine_set_bool    (GConfEngine* conf, const gchar* key,
                          gboolean val, GError** err)
{
  GConfValue* gval;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  gval = gconf_value_new(GCONF_VALUE_BOOL);

  gconf_value_set_bool(gval, !!val); /* canonicalize the bool */

  return error_checked_set(conf, key, gval, err);
}

gboolean
gconf_engine_set_schema  (GConfEngine* conf, const gchar* key,
                          const GConfSchema* val, GError** err)
{
  GConfValue* gval;

  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(val != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  gval = gconf_value_new(GCONF_VALUE_SCHEMA);

  gconf_value_set_schema(gval, val);

  return error_checked_set(conf, key, gval, err);
}

gboolean
gconf_engine_set_list    (GConfEngine* conf, const gchar* key,
                          GConfValueType list_type,
                          GSList* list,
                          GError** err)
{
  GConfValue* value_list;
  GError *tmp_err = NULL;
  
  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(list_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(list_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(list_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);

  value_list = gconf_value_list_from_primitive_list(list_type, list, &tmp_err);

  if (tmp_err)
    {
      g_propagate_error (err, tmp_err);
      return FALSE;
    }
  
  /* destroys the value_list */
  
  return error_checked_set(conf, key, value_list, err);
}

gboolean
gconf_engine_set_pair    (GConfEngine* conf, const gchar* key,
                          GConfValueType car_type, GConfValueType cdr_type,
                          gconstpointer address_of_car,
                          gconstpointer address_of_cdr,
                          GError** err)
{
  GConfValue* pair;
  GError *tmp_err = NULL;
  
  g_return_val_if_fail(conf != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(address_of_car != NULL, FALSE);
  g_return_val_if_fail(address_of_cdr != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  

  pair = gconf_value_pair_from_primitive_pair(car_type, cdr_type,
                                              address_of_car, address_of_cdr,
                                              &tmp_err);

  if (tmp_err)
    {
      g_propagate_error (err, tmp_err);
      return FALSE;
    }  
  
  return error_checked_set(conf, key, pair, err);
}

#ifdef HAVE_CORBA
/* CORBA Util */

/* Set GConfError from an exception, free exception, etc. */

static GConfError
corba_errno_to_gconf_errno(ConfigErrorType corba_err)
{
  switch (corba_err)
    {
    case ConfigFailed:
      return GCONF_ERROR_FAILED;
    case ConfigNoPermission:
      return GCONF_ERROR_NO_PERMISSION;
    case ConfigBadAddress:
      return GCONF_ERROR_BAD_ADDRESS;
    case ConfigBadKey:
      return GCONF_ERROR_BAD_KEY;
    case ConfigParseError:
      return GCONF_ERROR_PARSE_ERROR;
    case ConfigCorrupt:
      return GCONF_ERROR_CORRUPT;
    case ConfigTypeMismatch:
      return GCONF_ERROR_TYPE_MISMATCH;
    case ConfigIsDir:
      return GCONF_ERROR_IS_DIR;
    case ConfigIsKey:
      return GCONF_ERROR_IS_KEY;
    case ConfigOverridden:
      return GCONF_ERROR_OVERRIDDEN;
    case ConfigLockFailed:
      return GCONF_ERROR_LOCK_FAILED;
    case ConfigNoWritableDatabase:
      return GCONF_ERROR_NO_WRITABLE_DATABASE;
    case ConfigInShutdown:
      return GCONF_ERROR_IN_SHUTDOWN;
    default:
      g_assert_not_reached();
      return GCONF_ERROR_SUCCESS; /* warnings */
    }
}

static gboolean
gconf_server_broken(CORBA_Environment* ev)
{
  switch (ev->_major)
    {
    case CORBA_SYSTEM_EXCEPTION:
      return TRUE;

    case CORBA_USER_EXCEPTION:
      {
        ConfigException* ce;

        ce = CORBA_exception_value(ev);

        return ce->err_no == ConfigInShutdown;
      }
      
    default:
      return FALSE;
    }
}

static gboolean
gconf_handle_corba_exception(CORBA_Environment* ev, GError** err)
{
  switch (ev->_major)
    {
    case CORBA_NO_EXCEPTION:
      CORBA_exception_free (ev);
      return FALSE;
    case CORBA_SYSTEM_EXCEPTION:
      if (err)
        *err = gconf_error_new (GCONF_ERROR_NO_SERVER, _("CORBA error: %s"),
                                CORBA_exception_id (ev));
      CORBA_exception_free (ev);
      return TRUE;
    case CORBA_USER_EXCEPTION:
      {        
        ConfigException* ce;

        ce = CORBA_exception_value (ev);

        if (err)
          *err = gconf_error_new (corba_errno_to_gconf_errno (ce->err_no),
                                  "%s", ce->message);
        CORBA_exception_free (ev);
        return TRUE;
      }
    default:
      g_assert_not_reached();
      return TRUE;
    }
}
#endif

/*
 * Enumeration conversions
 */

gboolean
gconf_string_to_enum (GConfEnumStringPair lookup_table[],
                      const gchar* str,
                      gint* enum_value_retloc)
{
  int i = 0;
  
  while (lookup_table[i].str != NULL)
    {
      if (g_ascii_strcasecmp (lookup_table[i].str, str) == 0)
        {
          *enum_value_retloc = lookup_table[i].enum_value;
          return TRUE;
        }

      ++i;
    }

  return FALSE;
}

const gchar*
gconf_enum_to_string (GConfEnumStringPair lookup_table[],
                      gint enum_value)
{
  int i = 0;
  
  while (lookup_table[i].str != NULL)
    {
      if (lookup_table[i].enum_value == enum_value)
        return lookup_table[i].str;

      ++i;
    }

  return NULL;
}
