/* -*- tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* GConf
 * Copyright (C) 1999, 2000, 2000 Red Hat Inc.
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
#include <stdio.h>
#include <string.h>

#include "gconf-client.h"
#include "gconf/gconf-internals.h"

#include "gconfmarshal.h"
#include "gconfmarshal.c"

static gboolean do_trace = FALSE;

static void
trace (const char *format, ...)
{
  va_list args;
  gchar *str;

  if (!do_trace)
    return;
  
  g_return_if_fail (format != NULL);
  
  va_start (args, format);
  str = g_strdup_vprintf (format, args);
  va_end (args);

  g_message ("%s", str);
  
  g_free (str);
}

/*
 * Error handler override
 */

static GConfClientErrorHandlerFunc global_error_handler = NULL;

/**
 * gconf_client_set_global_default_error_handler: (skip)
 * @func: pointer to the function to be called for error handling.
 *
 * Set @func as the default error handler for the #GConfClient. This handler would be called
 * for all #GConfClient internal errors.
 */
void
gconf_client_set_global_default_error_handler(GConfClientErrorHandlerFunc func)
{
  global_error_handler = func;
}

/*
 * Dir object (for list of directories we're watching)
 */

typedef struct _Dir Dir;

struct _Dir {
  gchar* name;
  guint notify_id;
  /* number of times this dir has been added */
  guint add_count;
};

static Dir* dir_new(const gchar* name, guint notify_id);
static void dir_destroy(Dir* d);

/*
 * Listener object
 */

typedef struct _Listener Listener;

struct _Listener {
  GConfClientNotifyFunc func;
  gpointer data;
  GFreeFunc destroy_notify;
};

static Listener* listener_new(GConfClientNotifyFunc func,
                              GFreeFunc destroy_notify,
                              gpointer data);

static void listener_destroy(Listener* l);

/*
 * GConfClient proper
 */

#define PUSH_USE_ENGINE(client) do { if ((client)->engine) gconf_engine_push_owner_usage ((client)->engine, client); } while (0)
#define POP_USE_ENGINE(client) do { if ((client)->engine) gconf_engine_pop_owner_usage ((client)->engine, client); } while (0)

enum {
  VALUE_CHANGED,
  UNRETURNED_ERROR,
  ERROR,
  LAST_SIGNAL
};

static void         register_client   (GConfClient *client);
static GConfClient *lookup_client     (GConfEngine *engine);
static void         unregister_client (GConfClient *client);

static void gconf_client_class_init (GConfClientClass *klass);
static void gconf_client_init       (GConfClient      *client);
static void gconf_client_real_unreturned_error (GConfClient* client, GError* error);
static void gconf_client_real_error            (GConfClient* client, GError* error);
static void gconf_client_finalize              (GObject* object); 

static gboolean gconf_client_cache          (GConfClient *client,
                                             gboolean     take_ownership,
                                             GConfEntry  *entry,
                                             gboolean    preserve_schema_name);

static gboolean gconf_client_lookup         (GConfClient *client,
                                             const char  *key,
                                             GConfEntry **entryp);

static void gconf_client_real_remove_dir    (GConfClient* client,
                                             Dir* d,
                                             GError** err);

static void gconf_client_queue_notify       (GConfClient *client,
                                             const char  *key);
static void gconf_client_flush_notifies     (GConfClient *client);
static void gconf_client_unqueue_notifies   (GConfClient *client);
static void notify_one_entry (GConfClient *client, GConfEntry  *entry);

static GConfEntry* get (GConfClient  *client,
                        const gchar  *key,
                        gboolean      use_default,
                        GError      **error);


static gpointer parent_class = NULL;
static guint client_signals[LAST_SIGNAL] = { 0 };

GType
gconf_client_get_type (void)
{
  static GType client_type = 0;

  if (!client_type)
    {
      static const GTypeInfo client_info =
      {
        sizeof (GConfClientClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) gconf_client_class_init,
        NULL,           /* class_finalize */
        NULL,           /* class_data */
        sizeof (GConfClient),
        0,              /* n_preallocs */
        (GInstanceInitFunc) gconf_client_init
      };

      client_type = g_type_register_static (G_TYPE_OBJECT,
                                            "GConfClient",
                                            &client_info,
                                            0);
    }

  return client_type;
}

static void
gconf_client_class_init (GConfClientClass *class)
{
  GObjectClass *object_class;

  object_class = (GObjectClass*) class;

  parent_class = g_type_class_peek_parent (class);

  client_signals[VALUE_CHANGED] =
    g_signal_new ("value_changed",
                  G_TYPE_FROM_CLASS (object_class),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (GConfClientClass, value_changed),
                  NULL, NULL,
                  gconf_marshal_VOID__STRING_POINTER,
                  G_TYPE_NONE, 2, G_TYPE_STRING, G_TYPE_POINTER);

  client_signals[UNRETURNED_ERROR] =
    g_signal_new ("unreturned_error",
                  G_TYPE_FROM_CLASS (object_class),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (GConfClientClass, unreturned_error),
                  NULL, NULL,
                  gconf_marshal_VOID__POINTER,
                  G_TYPE_NONE, 1, G_TYPE_POINTER);

  client_signals[ERROR] =
    g_signal_new ("error",
                  G_TYPE_FROM_CLASS (object_class),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (GConfClientClass, error),
                  NULL, NULL,
                  gconf_marshal_VOID__POINTER,
                  G_TYPE_NONE, 1, G_TYPE_POINTER);
  
  class->value_changed    = NULL;
  class->unreturned_error = gconf_client_real_unreturned_error;
  class->error            = gconf_client_real_error;

  object_class->finalize  = gconf_client_finalize;

  if (g_getenv ("GCONF_DEBUG_TRACE_CLIENT") != NULL)
    do_trace = TRUE;
}

static void
gconf_client_init (GConfClient *client)
{
  client->engine = NULL;
  client->error_mode = GCONF_CLIENT_HANDLE_UNRETURNED;
  client->dir_hash = g_hash_table_new (g_str_hash, g_str_equal);
  client->cache_hash = g_hash_table_new (g_str_hash, g_str_equal);
  client->cache_dirs = g_hash_table_new_full (g_str_hash, g_str_equal,
					      g_free, NULL);
  client->cache_recursive_dirs = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                        g_free, NULL);
  /* We create the listeners only if they're actually used */
  client->listeners = NULL;
  client->notify_list = NULL;
  client->notify_handler = 0;
}

static gboolean
destroy_dir_foreach_remove(gpointer key, gpointer value, gpointer user_data)
{
  GConfClient *client = user_data;
  Dir* d = value;
  
  /* remove notify for this dir */
  
  if (d->notify_id != 0)
    {
      trace ("REMOTED: Removing notify ID %u from engine", d->notify_id);
      PUSH_USE_ENGINE (client);
	  gconf_engine_notify_remove (client->engine, d->notify_id);
      POP_USE_ENGINE (client);
    }
  
  d->notify_id = 0;

  dir_destroy(value);

  return TRUE;
}

static void
set_engine (GConfClient *client,
            GConfEngine *engine)
{
  if (engine == client->engine)
    return;
  
  if (engine)
    {
      gconf_engine_ref (engine);

      gconf_engine_set_owner (engine, client);
    }
  
  if (client->engine)
    {
      gconf_engine_set_owner (client->engine, NULL);
      
      gconf_engine_unref (client->engine);
    }
  
  client->engine = engine;  
}

static void
gconf_client_finalize (GObject* object)
{
  GConfClient* client = GCONF_CLIENT(object);

  gconf_client_unqueue_notifies (client);
  
  g_hash_table_foreach_remove (client->dir_hash,
                               destroy_dir_foreach_remove, client);
  
  gconf_client_clear_cache (client);

  if (client->listeners != NULL)
    {
      gconf_listeners_free (client->listeners);
      client->listeners = NULL;
    }

  g_hash_table_destroy (client->dir_hash);
  client->dir_hash = NULL;
  
  g_hash_table_destroy (client->cache_hash);
  client->cache_hash = NULL;

  g_hash_table_destroy (client->cache_recursive_dirs);
  client->cache_recursive_dirs = NULL;

  g_hash_table_destroy (client->cache_dirs);
  client->cache_dirs = NULL;

  unregister_client (client);

  set_engine (client, NULL);

  if (G_OBJECT_CLASS (parent_class)->finalize)
    (* G_OBJECT_CLASS (parent_class)->finalize) (object);
}

/*
 * Default error handlers
 */

static void
gconf_client_real_unreturned_error (GConfClient* client, GError* error)
{
  trace ("Unreturned error '%s'", error->message);
  if (client->error_mode == GCONF_CLIENT_HANDLE_UNRETURNED)
    {
      if (global_error_handler != NULL)
        {
          (*global_error_handler) (client, error);
        }
      else
        {
          /* We silently ignore this, since it probably isn't
           * really an error per se
           */
          if (error->code == GCONF_ERROR_OVERRIDDEN ||
              error->code == GCONF_ERROR_NO_WRITABLE_DATABASE)
            return;
          
          g_printerr (_("GConf Error: %s\n"),
                      error->message);
        }
    }
}

static void
gconf_client_real_error            (GConfClient* client, GError* error)
{
  trace ("Error '%s'", error->message);
  if (client->error_mode == GCONF_CLIENT_HANDLE_ALL)
    {
      if (global_error_handler != NULL)
        {
          (*global_error_handler) (client, error);
        }
      else
        {
          g_printerr (_("GConf Error: %s\n"),
                      error->message);
        }
    }
}

/* Emit the proper signals for the error, and fill in err */
static gboolean
handle_error(GConfClient* client, GError* error, GError** err)
{
  if (error != NULL)
    {
      gconf_client_error(client, error);
      
      if (err == NULL)
        {
          gconf_client_unreturned_error(client, error);

          g_error_free(error);
        }
      else
        *err = error;

      return TRUE;
    }
  else
    return FALSE;
}

static void
notify_from_server_callback (GConfEngine* conf, guint cnxn_id,
                             GConfEntry *entry,
                             gpointer user_data)
{
  GConfClient* client = user_data;
  gboolean changed;
  
  g_return_if_fail (client != NULL);
  g_return_if_fail (GCONF_IS_CLIENT(client));
  g_return_if_fail (client->engine == conf);

  trace ("Received notify of change to '%s' from server",
         entry->key);
  
  /* First do the caching, so that state is sane for the
   * listeners or functions connected to value_changed.
   * We know this key is under a directory in our dir list.
   */
  changed = gconf_client_cache (client, FALSE, entry, TRUE);

  if (!changed)
    return; /* don't do the notify */

  gconf_client_queue_notify (client, entry->key);
}

/*
 * Public API
 */


/**
 * gconf_client_get_default:
 *
 * Creates a new #GConfClient using the default #GConfEngine. Normally this is the
 * engine you want. If someone else is already using the default
 * #GConfClient, this function returns the same one they're using, but
 * with the reference count incremented. So you have to unref either way.
 *
 * It's important to call g_type_init() before using this GObject, to initialize the type system.
 *
 * Return value: (transfer full): a new #GConfClient. g_object_unref() when you're done.
 */
GConfClient*
gconf_client_get_default (void)
{
  GConfClient *client;
  GConfEngine *engine;
  
  g_return_val_if_fail(gconf_is_initialized(), NULL);

  engine = gconf_engine_get_default ();
  
  client = lookup_client (engine);
  if (client)
    {
      g_assert (client->engine == engine);
      g_object_ref (G_OBJECT (client));
      gconf_engine_unref (engine);
      return client;
    }
  else
    {
      client = g_object_new (gconf_client_get_type (), NULL);
      g_object_ref (G_OBJECT (client));      
      set_engine (client, engine);      
      register_client (client);
    }
  
  return client;
}

/**
 * gconf_client_get_for_engine:
 * @engine: the #GConfEngine to use.
 *
 * Creates a new #GConfClient with a specific #GConfEngine. Only specialized
 * configuration-related programs should need to call this function. The
 * returned #GConfClient should be unref'd when you're done with g_object_unref().
 * Remember to avoid using the #GConfEngine directly once you have a #GConfClient
 * wrapper.
 *
 * Return value: (transfer full): a new #GConfClient.
 */
GConfClient*
gconf_client_get_for_engine (GConfEngine* engine)
{
  GConfClient *client;

  g_return_val_if_fail(gconf_is_initialized(), NULL);

  client = lookup_client (engine);
  if (client)
    {
      g_assert (client->engine == engine);
      g_object_ref (G_OBJECT (client));
      return client;
    }
  else
    {
      client = g_object_new (gconf_client_get_type (), NULL);

      set_engine (client, engine);

      register_client (client);
    }
  
  return client;
}

typedef struct {
  GConfClient *client;
  Dir *lower_dir;
  const char *dirname;
} OverlapData;

static void
foreach_setup_overlap(gpointer key, gpointer value, gpointer user_data)
{
  GConfClient *client;
  Dir *dir = value;
  OverlapData * od = user_data;

  client = od->client;

  /* if we have found the first (well there is only one anyway) directory
   * that includes us that has a notify handler
   */
#ifdef GCONF_ENABLE_DEBUG
  if (dir->notify_id != 0 &&
      gconf_key_is_below(dir->name, od->dirname))
    {
      g_assert(od->lower_dir == NULL);
      od->lower_dir = dir;
    }
#else
  if (od->lower_dir == NULL &&
      dir->notify_id != 0 &&
      gconf_key_is_below(dir->name, od->dirname))
      od->lower_dir = dir;
#endif
  /* if we have found a directory that we include and it has
   * a notify_id, remove the notify handler now
   * FIXME: this is a race, from now on we can miss notifies, it is
   * not an incredible amount of time so this is not a showstopper */
  else if (dir->notify_id != 0 &&
           gconf_key_is_below (od->dirname, dir->name))
    {
      PUSH_USE_ENGINE (client);
      gconf_engine_notify_remove (client->engine, dir->notify_id);
      POP_USE_ENGINE (client);
      dir->notify_id = 0;
    }
}

static Dir *
setup_overlaps(GConfClient* client, const gchar* dirname)
{
  OverlapData od;

  od.client = client;
  od.lower_dir = NULL;
  od.dirname = dirname;

  g_hash_table_foreach(client->dir_hash, foreach_setup_overlap, &od);

  return od.lower_dir;
}

void
gconf_client_add_dir     (GConfClient* client,
                          const gchar* dirname,
                          GConfClientPreloadType preload,
                          GError** err)
{
  Dir* d;
  guint notify_id = 0;
  GError* error = NULL;

  g_return_if_fail (gconf_valid_key (dirname, NULL));

  trace ("Adding directory '%s'", dirname);
  
  d = g_hash_table_lookup (client->dir_hash, dirname);

  if (d == NULL)
    {
      Dir *overlap_dir;

      overlap_dir = setup_overlaps (client, dirname);

      /* only if there is no directory that includes us
       * already add a notify
       */
      if (overlap_dir == NULL)
        {
          trace ("REMOTE: Adding notify to engine at '%s'",
                 dirname);
          PUSH_USE_ENGINE (client);
          notify_id = gconf_engine_notify_add (client->engine,
                                               dirname,
                                               notify_from_server_callback,
                                               client,
                                               &error);
          POP_USE_ENGINE (client);
          
          /* We got a notify ID or we got an error, not both */
          g_return_if_fail ( (notify_id != 0 && error == NULL) ||
                             (notify_id == 0 && error != NULL) );
      
      
          if (handle_error (client, error, err))
            return;

          g_assert (error == NULL);
        }
      else
        {
          notify_id = 0;
        }
      
      d = dir_new (dirname, notify_id);

      g_hash_table_insert (client->dir_hash, d->name, d);

      gconf_client_preload (client, dirname, preload, &error);

      handle_error (client, error, err);
    }

  g_assert (d != NULL);

  d->add_count += 1;
}

typedef struct {
  GConfClient *client;
  GError *error;
} AddNotifiesData;

static void
foreach_add_notifies(gpointer key, gpointer value, gpointer user_data)
{
  AddNotifiesData *ad = user_data;
  GConfClient *client;
  Dir *dir = value;

  client = ad->client;

  if (ad->error != NULL)
    return;

  if (dir->notify_id == 0)
    {
      Dir *overlap_dir;
      overlap_dir = setup_overlaps(client, dir->name);

      /* only if there is no directory that includes us
       * already add a notify */
      if (overlap_dir == NULL)
        {
          trace ("REMOTE: Adding notify to engine at '%s'",
                 dir->name);
          PUSH_USE_ENGINE (client);
          dir->notify_id = gconf_engine_notify_add(client->engine,
                                                   dir->name,
                                                   notify_from_server_callback,
                                                   client,
                                                   &ad->error);
          POP_USE_ENGINE (client);
          
          /* We got a notify ID or we got an error, not both */
          g_return_if_fail( (dir->notify_id != 0 && ad->error == NULL) ||
                            (dir->notify_id == 0 && ad->error != NULL) );

          /* if error is returned, then we'll just ignore
           * things until the end */
        }
    }
}

static gboolean
clear_dir_cache_foreach (char* key, GConfEntry* entry, char *dir)
{
  if (gconf_key_is_below (dir, key))
    {
      gconf_entry_free (entry);
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
clear_cache_dirs_foreach (char *key, gpointer value, char *dir)
{
  if (strcmp (dir, key) == 0 ||
      gconf_key_is_below (dir, key))
    {
      trace ("'%s' no longer fully cached", dir);
      return TRUE;
    }

  return FALSE;
}

static void
gconf_client_real_remove_dir    (GConfClient* client,
                                 Dir* d,
                                 GError** err)
{
  AddNotifiesData ad;

  g_return_if_fail(d != NULL);
  g_return_if_fail(d->add_count == 0);
  
  g_hash_table_remove(client->dir_hash, d->name);
  
  /* remove notify for this dir */
  
  if (d->notify_id != 0)
    {
      trace ("REMOTE: Removing notify from engine at '%s'", d->name);
      PUSH_USE_ENGINE (client);
      gconf_engine_notify_remove (client->engine, d->notify_id);
      POP_USE_ENGINE (client);
      d->notify_id = 0;
    }
  
  g_hash_table_foreach_remove (client->cache_hash,
                               (GHRFunc)clear_dir_cache_foreach,
                               d->name);
  g_hash_table_foreach_remove (client->cache_dirs,
                               (GHRFunc)clear_cache_dirs_foreach,
                               d->name);
  dir_destroy(d);

  ad.client = client;
  ad.error = NULL;

  g_hash_table_foreach(client->dir_hash, foreach_add_notifies, &ad);

  handle_error(client, ad.error, err);
}

void
gconf_client_remove_dir  (GConfClient* client,
                          const gchar* dirname,
                          GError** err)
{
  Dir* found = NULL;

  trace ("Removing directory '%s'", dirname);
  
  found = g_hash_table_lookup (client->dir_hash,
                               dirname);
  
  if (found != NULL)
    {
      g_return_if_fail(found->add_count > 0);

      found->add_count -= 1;

      if (found->add_count == 0) 
        gconf_client_real_remove_dir (client, found, err);
    }
#ifndef G_DISABLE_CHECKS
  else
    g_warning("Directory `%s' was not being monitored by GConfClient %p",
              dirname, client);
#endif
}

/**
 * gconf_client_notify_add:
 * @client:
 * @namespace_section
 * @func: (scope notified) (closure user_data) (destroy destroy_notify):
 * @user_data:
 * @destroy_notify:
 * @err:
 */
guint
gconf_client_notify_add (GConfClient* client,
                         const gchar* namespace_section,
                         GConfClientNotifyFunc func,
                         gpointer user_data,
                         GFreeFunc destroy_notify,
                         GError** err)
{
  guint cnxn_id = 0;
  
  g_return_val_if_fail(client != NULL, 0);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), 0);

  if (client->listeners == NULL)
    client->listeners = gconf_listeners_new();
  
  cnxn_id = gconf_listeners_add (client->listeners,
                                 namespace_section,
                                 listener_new (func, destroy_notify, user_data),
                                 (GFreeFunc)listener_destroy);

  return cnxn_id;
}

void
gconf_client_notify_remove  (GConfClient* client,
                             guint cnxn)
{
  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));
  g_return_if_fail(client->listeners != NULL);
  
  gconf_listeners_remove(client->listeners, cnxn);
  
  if (gconf_listeners_count(client->listeners) == 0)
    {
      gconf_listeners_free(client->listeners);
      client->listeners = NULL;
    }
}

void
gconf_client_notify (GConfClient* client, const char* key)
{
  GConfEntry *entry;

  g_return_if_fail (client != NULL);
  g_return_if_fail (GCONF_IS_CLIENT(client));
  g_return_if_fail (key != NULL);

  entry = gconf_client_get_entry (client, key, NULL, TRUE, NULL);
  if (entry != NULL)
    {
      notify_one_entry (client, entry);
      gconf_entry_unref (entry);
    }
}

void
gconf_client_set_error_handling(GConfClient* client,
                                GConfClientErrorHandlingMode mode)
{
  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));

  client->error_mode = mode;
}

static gboolean
clear_cache_foreach (char* key, GConfEntry* entry, GConfClient* client)
{
  gconf_entry_free (entry);

  return TRUE;
}

void
gconf_client_clear_cache(GConfClient* client)
{
  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));

  trace ("Clearing cache");
  
  g_hash_table_foreach_remove (client->cache_hash, (GHRFunc)clear_cache_foreach,
                               client);

  g_hash_table_remove_all (client->cache_dirs);
}

static void
cache_pairs_in_dir(GConfClient* client, const gchar* path, gboolean recursive);

static void 
recurse_subdir_list(GConfClient* client, GSList* subdirs)
{
  GSList* tmp;

  tmp = subdirs;
  
  while (tmp != NULL)
    {
      gchar* s = tmp->data;
      
      cache_pairs_in_dir(client, s, TRUE);

      trace ("REMOTE: All dirs at '%s'", s);
      PUSH_USE_ENGINE (client);
      recurse_subdir_list(client,
                          gconf_engine_all_dirs (client->engine, s, NULL));
      POP_USE_ENGINE (client);

      g_free(s);
      
      tmp = g_slist_next(tmp);
    }
  
  g_slist_free(subdirs);
}

static gboolean
key_being_monitored (GConfClient *client,
                     const char  *key)
{
  gboolean retval = FALSE;
  char* parent = g_strdup (key);
  char* end;

  end = parent + strlen (parent);
  
  while (end)
    {
      if (end == parent)
        *(end + 1) = '\0'; /* special-case "/" root dir */
      else
        *end = '\0'; /* chop '/' off of dir */
      
      if (g_hash_table_lookup (client->dir_hash, parent) != NULL)
        {
          retval = TRUE;
          break;
        }

      if (end != parent)
        end = strrchr (parent, '/');
      else
        end = NULL;
    }

  g_free (parent);

  return retval;
}

static void
cache_entry_list_destructively (GConfClient *client,
                                GSList      *entries)
{
  GSList *tmp;
  
  tmp = entries;
  
  while (tmp != NULL)
    {
      GConfEntry* entry = tmp->data;
      
      gconf_client_cache (client, TRUE, entry, FALSE);
      
      tmp = g_slist_next (tmp);
    }
  
  g_slist_free (entries);
}

static void 
cache_pairs_in_dir(GConfClient* client, const gchar* dir, gboolean recursive)
{
  GSList* pairs;
  GError* error = NULL;

  trace ("REMOTE: Caching values in '%s'", dir);
  
  PUSH_USE_ENGINE (client);
  pairs = gconf_engine_all_entries(client->engine, dir, &error);
  POP_USE_ENGINE (client);
  
  if (error != NULL)
    {
      g_printerr (_("GConf warning: failure listing pairs in `%s': %s"),
                  dir, error->message);
      g_error_free(error);
      error = NULL;
    }

  cache_entry_list_destructively (client, pairs);
  trace ("Mark '%s' as fully cached", dir);
  g_hash_table_insert (client->cache_dirs, g_strdup (dir), GINT_TO_POINTER (1));
  
  if (recursive)
    g_hash_table_insert (client->cache_recursive_dirs, g_strdup (dir), GINT_TO_POINTER (1));
}

void
gconf_client_preload    (GConfClient* client,
                         const gchar* dirname,
                         GConfClientPreloadType type,
                         GError** err)
{

  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));
  g_return_if_fail(dirname != NULL);

#ifdef GCONF_ENABLE_DEBUG
  if (!key_being_monitored (client, dirname))
    {
      g_warning("Can only preload directories you've added with gconf_client_add_dir() (tried to preload '%s')",
                dirname);
      return;
    }
#endif
  
  switch (type)
    {
    case GCONF_CLIENT_PRELOAD_NONE:
      /* nothing */
      break;

    case GCONF_CLIENT_PRELOAD_ONELEVEL:
      {
        trace ("Onelevel preload of '%s'", dirname);
        
        cache_pairs_in_dir (client, dirname, FALSE);
      }
      break;

    case GCONF_CLIENT_PRELOAD_RECURSIVE:
      {
        GSList* subdirs;

        trace ("Recursive preload of '%s'", dirname);
        
	trace ("REMOTE: All dirs at '%s'", dirname);
        PUSH_USE_ENGINE (client);
        subdirs = gconf_engine_all_dirs(client->engine, dirname, NULL);
        POP_USE_ENGINE (client);
        
        cache_pairs_in_dir(client, dirname, TRUE);
          
        recurse_subdir_list(client, subdirs);
      }
      break;

    default:
      g_assert_not_reached();
      break;
    }
}

#ifdef HAVE_DBUS

/*
 * Add functions for manipulating the internal cache of values - under the
 * D-BUS environment we update the internal cache when changes happen to
 * ensure a consistent state
 */
static gboolean
remove_key_from_cache_foreach (const gchar *cached_key,
                               GConfEntry  *entry,
                               const gchar *key)
{
  if (strcmp (cached_key, key) == 0)
    {
      gconf_entry_free (entry);
      return TRUE;
    }

  return FALSE;
}

static gboolean
remove_key_from_cache_recursively_foreach (const gchar *cached_key,
                                           GConfEntry  *entry,
                                           const gchar *key)
{
  if (gconf_key_is_below (cached_key, key) == 0 || strcmp (cached_key, key) == 0)
    {
      /* Not sure why a null entry would be in the cache, but it happens
       * sometimes, workaround for now to avoid crashing.
       */
      if (entry)
        gconf_entry_free (entry);
      return TRUE;
    }

  return FALSE;
}

static gboolean
remove_dir_from_cache_foreach (char     *cached_dir,
                               gpointer  value,
                               char     *dir)
{
  if (strcmp (dir, cached_dir) == 0)
    {
      trace ("'%s' no longer fully cached", dir);
      return TRUE;
    }

  return FALSE;
}

/* The dbus version cleans the cache after modifying a value. So we will
 * remove current dir (where the key is in) from the cache_dirs
 * when removing the key from the cache_hash.
 * This is a workaround. It will degrade the performance of querying negative keys in the same dir. 
 */
static void remove_dir_from_cache (GConfClient *client,
                                   const gchar *key)
{
  char *dir, *last_slash;
  dir = g_strdup (key);
  last_slash = strrchr (dir, '/');
  g_assert (last_slash != NULL);
  *last_slash = 0;
  trace ("Remove dir '%s' from cache since one of keys is changed", dir);
  g_hash_table_foreach_remove (client->cache_dirs,
                               (GHRFunc) remove_dir_from_cache_foreach,
                               dir);
  g_free (dir);
}

static void
remove_key_from_cache (GConfClient *client,
                       const gchar *key)
{
  g_hash_table_foreach_remove (client->cache_hash,
                               (GHRFunc) remove_key_from_cache_foreach,
                               (char *) key);
  remove_dir_from_cache (client, key);
}

static void
remove_key_from_cache_recursively (GConfClient *client,
                                   const gchar *key)
{
  g_hash_table_foreach_remove (client->cache_hash,
                               (GHRFunc) remove_key_from_cache_recursively_foreach,
                               (char *) key);
  remove_dir_from_cache (client, key);
}

static gboolean
cache_key_value (GConfClient      *client,
                 const gchar      *key,
                 const GConfValue *val)
{
  GConfEntry *entry;

  entry = gconf_entry_new (key, val);
  return gconf_client_cache (client, TRUE, entry, TRUE);
}

static void
cache_key_value_and_notify (GConfClient *client,
                            const gchar *key,
                            GConfValue  *val,
                            gboolean     free_value)
{
  if (cache_key_value (client, key, val))
    {
      if (key_being_monitored (client, key))
        gconf_client_queue_notify (client, key);
    }

  if (free_value)
    gconf_value_free (val);
}

#endif

/*
 * Basic key-manipulation facilities
 */

void
gconf_client_set             (GConfClient* client,
                              const gchar* key,
                              const GConfValue* val,
                              GError** err)
{
  GError* error = NULL;

  trace ("REMOTE: Setting value of '%s'", key);
  PUSH_USE_ENGINE (client);
  gconf_engine_set (client->engine, key, val, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (!error)
    cache_key_value_and_notify (client, key, (GConfValue *) val, FALSE);
#endif
  
  handle_error(client, error, err);
}

gboolean
gconf_client_unset          (GConfClient* client,
                             const gchar* key, GError** err)
{
  GError* error = NULL;

  trace ("REMOTE: Unsetting '%s'", key);
  PUSH_USE_ENGINE (client);
  gconf_engine_unset(client->engine, key, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (!error)
    remove_key_from_cache (client, key);
#endif

  handle_error(client, error, err);

  if (error != NULL)
    return FALSE;
  else
    return TRUE;
}

gboolean
gconf_client_recursive_unset (GConfClient *client,
                              const char     *key,
                              GConfUnsetFlags flags,
                              GError        **err)
{
  GError* error = NULL;

  trace ("REMOTE: Recursive unsetting '%s'", key);
  
  PUSH_USE_ENGINE (client);
  gconf_engine_recursive_unset(client->engine, key, flags, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (!error)
    remove_key_from_cache_recursively (client, key);
#endif

  handle_error(client, error, err);

  if (error != NULL)
    return FALSE;
  else
    return TRUE;
}

static GSList*
copy_entry_list (GSList *list)
{
  GSList *copy;
  GSList *tmp;
  
  copy = NULL;
  tmp = list;
  while (tmp != NULL)
    {
      copy = g_slist_prepend (copy,
                              gconf_entry_copy (tmp->data));
      tmp = tmp->next;
    }

  copy = g_slist_reverse (copy);

  return copy;
}

/**
 * gconf_client_all_entries:
 * @client: a #GConfClient.
 * @dir: directory to list.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 *
 * Lists the key-value pairs in @dir. Does not list subdirectories; for
 * that use gconf_client_all_dirs(). The returned list contains #GConfEntry
 * objects. A #GConfEntry contains an <emphasis>absolute</emphasis> key
 * and a value. The list is not recursive, it contains only the immediate
 * children of @dir.  To free the returned list, gconf_entry_free()
 * each list element, then g_slist_free() the list itself.
 * Just like gconf_engine_all_entries (), but uses #GConfClient caching and error-handling features.
 *
 * Return value: (element-type GConfEntry) (transfer full): List of #GConfEntry.
 */
GSList*
gconf_client_all_entries    (GConfClient* client,
                             const gchar* dir,
                             GError** err)
{
  GError *error = NULL;
  GSList *retval;
  int dirlen;

  if (g_hash_table_lookup (client->cache_dirs, dir))
    {
      GHashTableIter iter;
      gpointer key, value;

      trace ("CACHED: Getting all values in '%s'", dir);

      dirlen = strlen (dir);
      retval = NULL;
      g_hash_table_iter_init (&iter, client->cache_hash);
      while (g_hash_table_iter_next (&iter, &key, &value))
        {
          const gchar *id = key;
          GConfEntry *entry = value;
          if (g_str_has_prefix (id, dir) &&
              id + dirlen == strrchr (id, '/'))
            retval = g_slist_prepend (retval, gconf_entry_copy (entry));
        }

      return retval;
    }

  trace ("REMOTE: Getting all values in '%s'", dir);

  PUSH_USE_ENGINE (client);
  retval = gconf_engine_all_entries (client->engine, dir, &error);
  POP_USE_ENGINE (client);
  
  handle_error (client, error, err);

  if (error != NULL)
    return NULL;

  if (key_being_monitored (client, dir))
    {
      cache_entry_list_destructively (client, copy_entry_list (retval));
      trace ("Mark '%s' as fully cached", dir);
      g_hash_table_insert (client->cache_dirs, g_strdup (dir), GINT_TO_POINTER (1));
    }

  return retval;
}

/**
 * gconf_client_all_dirs:
 * @client: a #GConfClient.
 * @dir: directory to get subdirectories from.
 * @err: the return location for an allocated #GError, or <symbol>NULL</symbol> to ignore errors.
 *
 * Lists the subdirectories in @dir. The returned list contains
 * allocated strings. Each string is the absolute path of a
 * subdirectory. You should g_free() each string in the list, then
 * g_slist_free() the list itself.  Just like gconf_engine_all_dirs(),
 * but uses #GConfClient caching and error-handling features.
 *
 * Return value: (element-type utf8) (transfer full): List of allocated subdirectory names.
 */
GSList*
gconf_client_all_dirs       (GConfClient* client,
                             const gchar* dir, GError** err)
{
  GError* error = NULL;
  GSList* retval;

  trace ("REMOTE: Getting all dirs in '%s'", dir);
  
  PUSH_USE_ENGINE (client);
  retval = gconf_engine_all_dirs(client->engine, dir, &error);
  POP_USE_ENGINE (client);
  
  handle_error(client, error, err);

  return retval;
}

void
gconf_client_suggest_sync   (GConfClient* client,
                             GError** err)
{
  GError* error = NULL;

  trace ("REMOTE: Suggesting sync");
  
  PUSH_USE_ENGINE (client);
  gconf_engine_suggest_sync(client->engine, &error);
  POP_USE_ENGINE (client);
  
  handle_error(client, error, err);
}

gboolean
gconf_client_dir_exists(GConfClient* client,
                        const gchar* dir, GError** err)
{
  GError* error = NULL;
  gboolean retval;

  trace ("REMOTE: Checking whether directory '%s' exists...", dir);
  
  PUSH_USE_ENGINE (client);
  retval = gconf_engine_dir_exists (client->engine, dir, &error);
  POP_USE_ENGINE (client);
  
  handle_error (client, error, err);

  if (retval)
    trace ("'%s' exists", dir);
  else
    trace ("'%s' doesn't exist", dir);
  
  return retval;
}

gboolean
gconf_client_key_is_writable (GConfClient* client,
                              const gchar* key,
                              GError**     err)
{
  GError* error = NULL;
  GConfEntry *entry = NULL;
  gboolean is_writable;

  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

  if (gconf_client_lookup (client, key, &entry))
    {
      if (!entry)
        return FALSE;

      trace ("CACHED: Checking whether key '%s' is writable", key);
      return gconf_entry_get_is_writable (entry);
    }
  
  trace ("REMOTE: Checking whether key '%s' is writable", key);

  entry = get (client, key, TRUE, &error);

  if (entry == NULL && error != NULL)
    handle_error (client, error, err);
  else
    g_assert (error == NULL);

  if (entry == NULL)
    is_writable = FALSE;
  else
    is_writable = gconf_entry_get_is_writable (entry);  

  if (entry)
    gconf_entry_free (entry);
  
  return is_writable;
}

static gboolean
check_type(const gchar* key, GConfValue* val, GConfValueType t, GError** err)
{
  if (val->type != t)
    {
      gconf_set_error(err, GCONF_ERROR_TYPE_MISMATCH,
                      _("Expected `%s' got `%s' for key %s"),
                      gconf_value_type_to_string(t),
                      gconf_value_type_to_string(val->type),
                      key);
      return FALSE;
    }
  else
    return TRUE;
}

static GConfEntry*
get (GConfClient *client,
     const gchar *key,
     gboolean     use_default,
     GError     **error)
{
  GConfEntry *entry = NULL;
  
  g_return_val_if_fail (client != NULL, NULL);
  g_return_val_if_fail (GCONF_IS_CLIENT(client), NULL);
  g_return_val_if_fail (error != NULL, NULL);
  g_return_val_if_fail (*error == NULL, NULL);
  
  /* Check our client-side cache */
  if (gconf_client_lookup (client, key, &entry))

    {
      trace ("CACHED: Query for '%s'", key);
      
      if (entry == NULL)
        return NULL;
      
      if (gconf_entry_get_is_default (entry) && !use_default)
        return NULL;
      else
        return gconf_entry_copy (entry);
    }
      
  g_assert (entry == NULL); /* if it was in the cache we should have returned */

  /* Check the GConfEngine */
  trace ("REMOTE: Query for '%s'", key);
  PUSH_USE_ENGINE (client);
  entry = gconf_engine_get_entry (client->engine, key,
                                  gconf_current_locale(),
                                  TRUE /* always use default here */,
                                  error);
  POP_USE_ENGINE (client);
  
  if (*error != NULL)
    {
      g_return_val_if_fail (entry == NULL, NULL);
      return NULL;
    }
  else
    {
      g_assert (entry != NULL); /* gconf_engine_get_entry shouldn't return NULL ever */
      
      /* Cache this value, if it's in our directory list. */
      if (key_being_monitored (client, key))
        {
          /* cache a copy of val */
          gconf_client_cache (client, FALSE, entry, FALSE);
        }

      /* We don't own the entry, we're returning this copy belonging
       * to the caller
       */
      if (gconf_entry_get_is_default (entry) && !use_default)
        {
          gconf_entry_free (entry);
          return NULL;
        }
      else
        return entry;
    }
}
     
static GConfValue*
gconf_client_get_full        (GConfClient* client,
                              const gchar* key, const gchar* locale,
                              gboolean use_schema_default,
                              GError** err)
{
  GError* error = NULL;
  GConfEntry *entry;
  GConfValue *retval;
  
  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  if (locale != NULL)
    g_warning ("haven't implemented getting a specific locale in GConfClient");
  
  entry = get (client, key, use_schema_default,
               &error);

  if (entry == NULL && error != NULL)
    handle_error(client, error, err);
  else
    g_assert (error == NULL);

  retval = NULL;
  
  if (entry && gconf_entry_get_value (entry))
    retval = gconf_value_copy (gconf_entry_get_value (entry));

  if (entry != NULL)
    gconf_entry_free (entry);

  return retval;
}

GConfEntry*
gconf_client_get_entry (GConfClient* client,
                        const gchar* key,
                        const gchar* locale,
                        gboolean use_schema_default,
                        GError** err)
{
  GError* error = NULL;
  GConfEntry *entry;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  if (locale != NULL)
    g_warning("haven't implemented getting a specific locale in GConfClient");
  
  entry = get (client, key, use_schema_default,
               &error);

  if (entry == NULL && error != NULL)
    handle_error (client, error, err);
  else
    g_assert (error == NULL);
  
  return entry;
}

GConfValue*
gconf_client_get             (GConfClient* client,
                              const gchar* key,
                              GError** err)
{
  g_return_val_if_fail (GCONF_IS_CLIENT (client), NULL);
  g_return_val_if_fail (key != NULL, NULL);
  return gconf_client_get_full (client, key, NULL, TRUE, err);
}

GConfValue*
gconf_client_get_without_default  (GConfClient* client,
                                   const gchar* key,
                                   GError** err)
{
  g_return_val_if_fail (GCONF_IS_CLIENT (client), NULL);
  g_return_val_if_fail (key != NULL, NULL);
  return gconf_client_get_full (client, key, NULL, FALSE, err);
}

GConfValue*
gconf_client_get_default_from_schema (GConfClient* client,
                                      const gchar* key,
                                      GError** err)
{
  GError* error = NULL;
  GConfEntry *entry = NULL;
  GConfValue *val = NULL;
  
  g_return_val_if_fail (err == NULL || *err == NULL, NULL);  
  g_return_val_if_fail (client != NULL, NULL);
  g_return_val_if_fail (GCONF_IS_CLIENT(client), NULL);
  g_return_val_if_fail (key != NULL, NULL);
  
  /* Check our client-side cache to see if the default is the same as
   * the regular value (FIXME put a default_value field in the
   * cache and store both, lose the is_default flag)
   */
  if (gconf_client_lookup (client, key, &entry))
    {
      if (!entry)
        return NULL;

      if (gconf_entry_get_is_default (entry))
        {
	  trace ("CACHED: Getting schema default for '%s'", key);

          return gconf_entry_get_value (entry) ?
            gconf_value_copy (gconf_entry_get_value (entry)) :
            NULL;
        }
    }

  /* Check the GConfEngine */
  trace ("REMOTE: Getting schema default for '%s'", key);
  PUSH_USE_ENGINE (client);
  val = gconf_engine_get_default_from_schema (client->engine, key,
                                              &error);
  POP_USE_ENGINE (client);
  
  if (error != NULL)
    {
      g_assert (val == NULL);
      handle_error (client, error, err);
      return NULL;
    }
  else
    {
      /* FIXME eventually we'll cache the value
       * by adding a field to the cache
       */
      return val;
    }
}

gdouble
gconf_client_get_float (GConfClient* client, const gchar* key,
                        GError** err)
{
  static const gdouble def = 0.0;
  GError* error = NULL;
  GConfValue *val;

  g_return_val_if_fail (err == NULL || *err == NULL, 0.0);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      gdouble retval = def;

      g_assert(error == NULL);
      
      if (check_type (key, val, GCONF_VALUE_FLOAT, &error))
        retval = gconf_value_get_float (val);
      else
        handle_error (client, error, err);

      gconf_value_free (val);

      return retval;
    }
  else
    {
      if (error != NULL)
        handle_error (client, error, err);
      return def;
    }
}

gint
gconf_client_get_int   (GConfClient* client, const gchar* key,
                        GError** err)
{
  static const gint def = 0;
  GError* error = NULL;
  GConfValue* val;

  g_return_val_if_fail (err == NULL || *err == NULL, 0);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      gint retval = def;

      g_assert(error == NULL);
      
      if (check_type (key, val, GCONF_VALUE_INT, &error))
        retval = gconf_value_get_int(val);
      else
        handle_error (client, error, err);

      gconf_value_free (val);

      return retval;
    }
  else
    {
      if (error != NULL)
        handle_error (client, error, err);
      return def;
    }
}

gchar*
gconf_client_get_string(GConfClient* client, const gchar* key,
                        GError** err)
{
  GError* error = NULL;
  GConfValue* val;

  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      gchar* retval = NULL;

      g_assert(error == NULL);
      
      if (check_type (key, val, GCONF_VALUE_STRING, &error))
        retval = gconf_value_steal_string (val);
      else
        handle_error (client, error, err);
      
      gconf_value_free (val);

      return retval;
    }
  else
    {
      if (error != NULL)
        handle_error (client, error, err);
      return NULL;
    }
}


gboolean
gconf_client_get_bool  (GConfClient* client, const gchar* key,
                        GError** err)
{
  static const gboolean def = FALSE;
  GError* error = NULL;
  GConfValue* val;

  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      gboolean retval = def;

      g_assert (error == NULL);
      
      if (check_type (key, val, GCONF_VALUE_BOOL, &error))
        retval = gconf_value_get_bool (val);
      else
        handle_error (client, error, err);

      gconf_value_free (val);

      return retval;
    }
  else
    {
      if (error != NULL)
        handle_error (client, error, err);
      return def;
    }
}

GConfSchema*
gconf_client_get_schema  (GConfClient* client,
                          const gchar* key, GError** err)
{
  GError* error = NULL;
  GConfValue* val;

  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      GConfSchema* retval = NULL;

      g_assert(error == NULL);
      
      if (check_type (key, val, GCONF_VALUE_SCHEMA, &error))
        retval = gconf_value_steal_schema (val);
      else
        handle_error (client, error, err);
      
      gconf_value_free (val);

      return retval;
    }
  else
    {
      if (error != NULL)
        handle_error (client, error, err);
      return NULL;
    }
}

/**
 * gconf_client_get_list: (skip)
 * @client: a #GConfClient.
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
 * must use gconf_client_get () to obtain a raw #GConfValue.
 *
 * <emphasis>Remember that GConf lists can only store primitive types:
 * %GCONF_VALUE_FLOAT, %GCONF_VALUE_INT, %GCONF_VALUE_BOOL,
 * %GCONF_VALUE_STRING, %GCONF_VALUE_SCHEMA.</emphasis> Also remember
 * that lists must be uniform, you may not mix types in the same list.
 *
 * The type of the list elements depends on @list_type. A #GConfValue
 * with type %GCONF_VALUE_LIST normally stores a list of more #GConfValue
 * objects. gconf_client_get_list() automatically converts to primitive C
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
 * Just like gconf_engine_get_list (), but uses #GConfClient caching and error-handling features.
 *
* Return value: an allocated list, with elements as described above.
*/
GSList*
gconf_client_get_list    (GConfClient* client, const gchar* key,
                          GConfValueType list_type, GError** err)
{
  GError* error = NULL;
  GConfValue* val;

  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      GSList* retval;

      g_assert (error == NULL);

      /* This function checks the type and destroys "val" */
      retval = gconf_value_list_to_primitive_list_destructive (val, list_type, &error);

      if (error != NULL)
        {
          g_assert (retval == NULL);
          handle_error (client, error, err);
          return NULL;
        }
      else
        return retval;
    }
  else
    {
      if (error != NULL)
        handle_error (client, error, err);
      return NULL;
    }
}

gboolean
gconf_client_get_pair    (GConfClient* client, const gchar* key,
                          GConfValueType car_type, GConfValueType cdr_type,
                          gpointer car_retloc, gpointer cdr_retloc,
                          GError** err)
{
  GError* error = NULL;
  GConfValue* val;

  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

  val = gconf_client_get (client, key, &error);

  if (val != NULL)
    {
      g_assert(error == NULL);

      /* This function checks the type and destroys "val" */
      if (gconf_value_pair_to_primitive_pair_destructive (val, car_type, cdr_type,
                                                          car_retloc, cdr_retloc,
                                                          &error))
        {
          g_assert (error == NULL);
          return TRUE;
        }
      else
        {
          g_assert (error != NULL);
          handle_error (client, error, err);
          return FALSE;
        }
    }
  else
    {
      if (error != NULL)
        {
          handle_error (client, error, err);
          return FALSE;
        }
      else
        return TRUE;
    }
}

#ifdef HAVE_DBUS
#define MAKE_VALUE(v, TYPE, type, val) GConfValue *v = gconf_value_new (GCONF_VALUE_##TYPE); gconf_value_set_##type (v, val)
#endif

/*
 * For the set functions, we just set normally, and wait for the
 * notification to come back from the server before we update
 * our cache. This may be the wrong thing; maybe we should
 * update immediately?
 * Problem with delayed update: user calls set() then get(),
 *  results in weirdness
 * Problem with with regular update: get() before the notify
 *  is out of sync with the listening parts of the application
 * 
 */

gboolean
gconf_client_set_float   (GConfClient* client, const gchar* key,
                          gdouble val, GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);

  trace ("REMOTE: Setting float '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_float (client->engine, key, val, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      MAKE_VALUE (v, FLOAT, float, val);
      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}

gboolean
gconf_client_set_int     (GConfClient* client, const gchar* key,
                          gint val, GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);

  trace ("REMOTE: Setting int '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_int (client->engine, key, val, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      MAKE_VALUE (v, INT, int, val);
      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}

gboolean
gconf_client_set_string  (GConfClient* client, const gchar* key,
                          const gchar* val, GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(val != NULL, FALSE);

  trace ("REMOTE: Setting string '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_string(client->engine, key, val, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      MAKE_VALUE (v, STRING, string, val);
      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}

gboolean
gconf_client_set_bool    (GConfClient* client, const gchar* key,
                          gboolean val, GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);

  trace ("REMOTE: Setting bool '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_bool (client->engine, key, val, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      MAKE_VALUE (v, BOOL, bool, val);
      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}

gboolean
gconf_client_set_schema  (GConfClient* client, const gchar* key,
                          const GConfSchema* val, GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(val != NULL, FALSE);

  trace ("REMOTE: Setting schema '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_schema(client->engine, key, val, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      MAKE_VALUE (v, SCHEMA, schema, val);
      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}

gboolean
gconf_client_set_list    (GConfClient* client, const gchar* key,
                          GConfValueType list_type,
                          GSList* list,
                          GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);

  trace ("REMOTE: Setting list '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_list(client->engine, key, list_type, list, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      GConfValue *v = gconf_value_list_from_primitive_list (list_type, list, err);
      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}

gboolean
gconf_client_set_pair    (GConfClient* client, const gchar* key,
                          GConfValueType car_type, GConfValueType cdr_type,
                          gconstpointer address_of_car,
                          gconstpointer address_of_cdr,
                          GError** err)
{
  GError* error = NULL;
  gboolean result;
  
  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);  
  g_return_val_if_fail(key != NULL, FALSE);

  trace ("REMOTE: Setting pair '%s'", key);
  PUSH_USE_ENGINE (client);
  result = gconf_engine_set_pair (client->engine, key, car_type, cdr_type,
                                  address_of_car, address_of_cdr, &error);
  POP_USE_ENGINE (client);

#ifdef HAVE_DBUS
  if (result)
    {
      GConfValue *v = gconf_value_pair_from_primitive_pair (car_type, cdr_type, address_of_car, address_of_cdr, &error);
      if (!v)
        {
          handle_error (client, error, err);
          return FALSE;
        }

      cache_key_value_and_notify (client, key, v, TRUE);
    }
#endif

  if (result)
    return TRUE;
  else
    {
      handle_error(client, error, err);
      return FALSE;
    }
}


/*
 * Functions to emit signals
 */

void
gconf_client_error                  (GConfClient* client, GError* error)
{
  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));
  
  g_signal_emit (G_OBJECT(client), client_signals[ERROR], 0,
                 error);
}

void
gconf_client_unreturned_error       (GConfClient* client, GError* error)
{
  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));

  g_signal_emit (G_OBJECT(client), client_signals[UNRETURNED_ERROR], 0,
                 error);
}

void
gconf_client_value_changed          (GConfClient* client,
                                     const gchar* key,
                                     GConfValue* value)
{
  g_return_if_fail(client != NULL);
  g_return_if_fail(GCONF_IS_CLIENT(client));
  g_return_if_fail(key != NULL);
  
  g_signal_emit(G_OBJECT(client), client_signals[VALUE_CHANGED], 0,
                key, value);
}

/*
 * Internal utility
 */

static gboolean
gconf_client_cache (GConfClient *client,
                    gboolean     take_ownership,
                    GConfEntry  *new_entry,
                    gboolean     preserve_schema_name)
{
  gpointer oldkey = NULL, oldval = NULL;

  if (g_hash_table_lookup_extended (client->cache_hash, new_entry->key, &oldkey, &oldval))
    {
      /* Already have a value, update it */
      GConfEntry *entry = oldval;
      gboolean changed;
      
      g_assert (entry != NULL);

      changed = ! gconf_entry_equal (entry, new_entry);

      if (changed)
        {
          trace ("Updating value of '%s' in the cache",
                 new_entry->key);

          if (!take_ownership)
            new_entry = gconf_entry_copy (new_entry);
          
          if (preserve_schema_name)
            gconf_entry_set_schema_name (new_entry, 
                                         gconf_entry_get_schema_name (entry));

          g_hash_table_replace (client->cache_hash,
                                new_entry->key,
                                new_entry);

          /* oldkey is inside entry */
          gconf_entry_free (entry);
        }
      else
        {
          trace ("Value of '%s' hasn't actually changed, would have updated in cache if it had",
                 new_entry->key);

          if (take_ownership)
            gconf_entry_free (new_entry);
        }

      return changed;
    }
  else
    {
      /* Create a new entry */
      if (!take_ownership)
        new_entry = gconf_entry_copy (new_entry);
      
      g_hash_table_insert (client->cache_hash, new_entry->key, new_entry);
      trace ("Added value of '%s' to the cache",
             new_entry->key);

      return TRUE; /* changed */
    }
}

static gboolean
gconf_client_lookup (GConfClient *client,
                     const char  *key,
                     GConfEntry **entryp)
{
  GConfEntry *entry;

  g_return_val_if_fail (entryp != NULL, FALSE);
  g_return_val_if_fail (*entryp == NULL, FALSE);
  
  entry = g_hash_table_lookup (client->cache_hash, key);

  *entryp = entry;

  if (!entry)
  {
    char *dir, *last_slash;

    dir = g_strdup (key);
    last_slash = strrchr (dir, '/');
    g_assert (last_slash != NULL);
    *last_slash = 0;

    if (g_hash_table_lookup (client->cache_dirs, dir))
      {
        g_free (dir);
        trace ("Negative cache hit on %s", key);
        return TRUE;
      }
    else 
      {
        gboolean not_cached = FALSE;
        while(not_cached || (!g_hash_table_lookup (client->cache_recursive_dirs, dir)))
          {
            last_slash = strrchr (dir, '/');
            if (last_slash == NULL)
              break;
            else
              *last_slash = 0;
            if (g_hash_table_lookup (client->cache_recursive_dirs, dir))
              {
                g_free (dir);
                trace ("Non-existing dir for %s", key);
                return TRUE;
              }
            not_cached = TRUE;
          }
      }
    g_free (dir);
  }

  return entry != NULL;
}

/*
 * Dir
 */

static Dir*
dir_new(const gchar* name, guint notify_id)
{
  Dir* d;

  d = g_new(Dir, 1);

  d->name = g_strdup(name);
  d->notify_id = notify_id;
  d->add_count = 0;
  
  return d;
}

static void
dir_destroy(Dir* d)
{
  g_return_if_fail(d != NULL);
  g_return_if_fail(d->notify_id == 0);
  
  g_free(d->name);
  g_free(d);
}

/*
 * Listener
 */

static Listener* 
listener_new(GConfClientNotifyFunc func,
             GFreeFunc destroy_notify,
             gpointer data)
{
  Listener* l;

  l = g_new(Listener, 1);

  l->func = func;
  l->data = data;
  l->destroy_notify = destroy_notify;
  
  return l;
}

static void
listener_destroy(Listener* l)
{
  g_return_if_fail(l != NULL);

  if (l->destroy_notify)
    (* l->destroy_notify) (l->data);
  
  g_free(l);
}

/*
 * Change sets
 */


struct CommitData {
  GConfClient* client;
  GError* error;
  GSList* remove_list;
  gboolean remove_committed;
};

static void
commit_foreach (GConfChangeSet* cs,
                const gchar* key,
                GConfValue* value,
                gpointer user_data)
{
  struct CommitData* cd = user_data;

  g_assert(cd != NULL);

  if (cd->error != NULL)
    return;
  
  if (value)
    gconf_client_set   (cd->client, key, value, &cd->error);
  else
    gconf_client_unset (cd->client, key, &cd->error);

  if (cd->error == NULL && cd->remove_committed)
    {
      /* Bad bad bad; we keep the key reference, knowing that it's
         valid until we modify the change set, to avoid string copies.  */
      cd->remove_list = g_slist_prepend(cd->remove_list, (gchar*)key);
    }
}

gboolean
gconf_client_commit_change_set   (GConfClient* client,
                                  GConfChangeSet* cs,
                                  gboolean remove_committed,
                                  GError** err)
{
  struct CommitData cd;
  GSList* tmp;

  g_return_val_if_fail(client != NULL, FALSE);
  g_return_val_if_fail(GCONF_IS_CLIENT(client), FALSE);
  g_return_val_if_fail(cs != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  cd.client = client;
  cd.error = NULL;
  cd.remove_list = NULL;
  cd.remove_committed = remove_committed;

  /* Because the commit could have lots of side
     effects, this makes it safer */
  gconf_change_set_ref(cs);
  g_object_ref(G_OBJECT(client));
  
  gconf_change_set_foreach(cs, commit_foreach, &cd);

  tmp = cd.remove_list;
  while (tmp != NULL)
    {
      const gchar* key = tmp->data;
      
      gconf_change_set_remove(cs, key);

      /* key is now invalid due to our little evil trick */

      tmp = g_slist_next(tmp);
    }

  g_slist_free(cd.remove_list);
  
  gconf_change_set_unref(cs);
  g_object_unref(G_OBJECT(client));

  if (cd.error != NULL)
    {
      if (err != NULL)
        *err = cd.error;
      else
        g_error_free(cd.error);

      return FALSE;
    }
  else
    {
      g_assert((!remove_committed) ||
               (gconf_change_set_size(cs) == 0));
      
      return TRUE;
    }
}

struct RevertData {
  GConfClient* client;
  GError* error;
  GConfChangeSet* revert_set;
};

static void
revert_foreach (GConfChangeSet* cs,
                const gchar* key,
                GConfValue* value,
                gpointer user_data)
{
  struct RevertData* rd = user_data;
  GConfValue* old_value;
  GError* error = NULL;
  
  g_assert(rd != NULL);

  if (rd->error != NULL)
    return;

  old_value = gconf_client_get_without_default(rd->client, key, &error);

  if (error != NULL)
    {
      /* FIXME */
      g_warning("error creating revert set: %s", error->message);
      g_error_free(error);
      error = NULL;
    }
  
  if (old_value == NULL &&
      value == NULL)
    return; /* this commit will have no effect. */

  if (old_value == NULL)
    gconf_change_set_unset(rd->revert_set, key);
  else
    gconf_change_set_set_nocopy(rd->revert_set, key, old_value);
}


GConfChangeSet*
gconf_client_reverse_change_set  (GConfClient* client,
                                         GConfChangeSet* cs,
                                         GError** err)
{
  struct RevertData rd;

  rd.error = NULL;
  rd.client = client;
  rd.revert_set = gconf_change_set_new();

  /* we're emitting signals and such, avoid
     nasty side effects with these.
  */
  g_object_ref(G_OBJECT(rd.client));
  gconf_change_set_ref(cs);
  
  gconf_change_set_foreach(cs, revert_foreach, &rd);

  if (rd.error != NULL)
    {
      if (err != NULL)
        *err = rd.error;
      else
        g_error_free(rd.error);
    }

  g_object_unref(G_OBJECT(rd.client));
  gconf_change_set_unref(cs);
  
  return rd.revert_set;
}


GConfChangeSet*
gconf_client_change_set_from_currentv (GConfClient* client,
                                              const gchar** keys,
                                              GError** err)
{
  GConfValue* old_value;
  GConfChangeSet* new_set;
  const gchar** keyp;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  new_set = gconf_change_set_new();
  
  keyp = keys;

  while (*keyp != NULL)
    {
      GError* error = NULL;
      const gchar* key = *keyp;
      
      old_value = gconf_client_get_without_default(client, key, &error);

      if (error != NULL)
        {
          /* FIXME */
          g_warning("error creating change set from current keys: %s", error->message);
          g_error_free(error);
          error = NULL;
        }
      
      if (old_value == NULL)
        gconf_change_set_unset(new_set, key);
      else
        gconf_change_set_set_nocopy(new_set, key, old_value);

      ++keyp;
    }

  return new_set;
}

GConfChangeSet*
gconf_client_change_set_from_current (GConfClient* client,
                                             GError** err,
                                             const gchar* first_key,
                                             ...)
{
  GSList* keys = NULL;
  va_list args;
  const gchar* arg;
  const gchar** vec;
  GConfChangeSet* retval;
  GSList* tmp;
  guint i;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  va_start (args, first_key);

  arg = first_key;

  while (arg != NULL)
    {
      keys = g_slist_prepend(keys, (/*non-const*/gchar*)arg);

      arg = va_arg (args, const gchar*);
    }
  
  va_end (args);

  vec = g_new0(const gchar*, g_slist_length(keys) + 1);

  i = 0;
  tmp = keys;

  while (tmp != NULL)
    {
      vec[i] = tmp->data;
      
      ++i;
      tmp = g_slist_next(tmp);
    }

  g_slist_free(keys);
  
  retval = gconf_client_change_set_from_currentv(client, vec, err);
  
  g_free(vec);

  return retval;
}

static GHashTable * clients = NULL;

static void
register_client (GConfClient *client)
{
  if (clients == NULL)
    clients = g_hash_table_new (NULL, NULL);

  g_hash_table_insert (clients, client->engine, client);
}

static GConfClient *
lookup_client (GConfEngine *engine)
{
  if (clients == NULL)
    return NULL;
  else
    return g_hash_table_lookup (clients, engine);
}

static void
unregister_client (GConfClient *client)
{
  g_return_if_fail (clients != NULL);

  g_hash_table_remove (clients, client->engine);
}


/*
 * Notification
 */

static gboolean
notify_idle_callback (gpointer data)
{
  GConfClient *client = data;

  client->notify_handler = 0; /* avoid g_source_remove */
  
  gconf_client_flush_notifies (client);

  /* remove handler */
  return FALSE;
}

static void
gconf_client_queue_notify (GConfClient *client,
                           const char  *key)
{
  trace ("Queing notify on '%s', %d pending already", key,
         client->pending_notify_count);
  
  if (client->notify_handler == 0)
    client->notify_handler = g_idle_add (notify_idle_callback, client);
  
  client->notify_list = g_slist_prepend (client->notify_list, g_strdup (key));
  client->pending_notify_count += 1;
}

struct ClientAndEntry {
  GConfClient* client;
  GConfEntry* entry;
};

static void
notify_listeners_callback(GConfListeners* listeners,
                          const gchar* key,
                          guint cnxn_id,
                          gpointer listener_data,
                          gpointer user_data)
{
  Listener* l = listener_data;
  struct ClientAndEntry* cae = user_data;
  
  g_return_if_fail (cae != NULL);
  g_return_if_fail (cae->client != NULL);
  g_return_if_fail (GCONF_IS_CLIENT (cae->client));
  g_return_if_fail (l != NULL);
  g_return_if_fail (l->func != NULL);

  (*l->func) (cae->client, cnxn_id, cae->entry, l->data);
}
  
static void
notify_one_entry (GConfClient *client,
                  GConfEntry  *entry)
{
  g_object_ref (G_OBJECT (client));
  gconf_entry_ref (entry);
  
  /* Emit the value_changed signal before notifying specific listeners;
   * I'm not sure there's a reason this matters though
   */
  gconf_client_value_changed (client,
                              entry->key,
                              gconf_entry_get_value (entry));

  /* Now notify our listeners, if any */
  if (client->listeners != NULL)
    {
      struct ClientAndEntry cae;

      cae.client = client;
      cae.entry = entry;
      
      gconf_listeners_notify (client->listeners,
                              entry->key,
                              notify_listeners_callback,
                              &cae);
    }

  gconf_entry_unref (entry);
  g_object_unref (G_OBJECT (client));
}

static void
gconf_client_flush_notifies (GConfClient *client)
{
  GSList *tmp;
  GSList *to_notify;
  GConfEntry *last_entry;

  trace ("Flushing notify queue");
  
  /* Adopt notify list and clear it, to avoid reentrancy concerns.
   * Sort it to compress duplicates, and keep people from relying on
   * the notify order.
   */
  to_notify = g_slist_sort (client->notify_list, (GCompareFunc) strcmp);
  client->notify_list = NULL;
  client->pending_notify_count = 0;

  gconf_client_unqueue_notifies (client);

  last_entry = NULL;
  tmp = to_notify;
  while (tmp != NULL)
    {
      GConfEntry *entry = NULL;

      if (gconf_client_lookup (client, tmp->data, &entry) && entry != NULL)
        {
          if (entry != last_entry)
            {
              trace ("Doing notification for '%s'", entry->key);
              notify_one_entry (client, entry);
              last_entry = entry;
            }
          else
            {
              trace ("Ignoring duplicate notify for '%s'", entry->key);
            }
        }
      else
        {
#ifdef HAVE_DBUS
          /* The dbus version cleans the cache after modifying a value so a get
           * directly after a set doesn't return a stale value. That means we
           * have to check if the key is supposed to be monitored here, we can't
           * just rely on it being in the cache.
           */
          if (key_being_monitored (client, tmp->data))
            {
              trace ("Key %s was in notify queue but not in cache, but is being monitored",
                     tmp->data);

              entry = gconf_client_get_entry (client, tmp->data, NULL, TRUE, NULL);
              if (entry != NULL)
                {
                  notify_one_entry (client, entry);
                  gconf_entry_unref (entry);
                  last_entry = NULL;
                }
            }
          else
            {
              trace ("Key '%s' was in notify queue but not in cache; we must have stopped monitoring it; not notifying",
                     tmp->data);
            }
#else
              trace ("Key '%s' was in notify queue but not in cache; we must have stopped monitoring it; not notifying",
                     tmp->data);
#endif
        }
      
      tmp = tmp->next;
    }
  
  g_slist_foreach (to_notify, (GFunc) g_free, NULL);
  g_slist_free (to_notify);
}

static void
gconf_client_unqueue_notifies (GConfClient *client)
{
  if (client->notify_handler != 0)
    {
      g_source_remove (client->notify_handler);
      client->notify_handler = 0;
    }
  
  if (client->notify_list != NULL)
    {
      g_slist_foreach (client->notify_list, (GFunc) g_free, NULL);
      g_slist_free (client->notify_list);
      client->notify_list = NULL;
      client->pending_notify_count = 0;
    }
}
