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
#include "xml-cache.h"
#include "gconf/gconf-internals.h"

#include <string.h>
#include <time.h>

/* This makes hash table safer when debugging */
#ifndef GCONF_ENABLE_DEBUG
#define safe_g_hash_table_insert g_hash_table_insert
#else
static void
safe_g_hash_table_insert(GHashTable* ht, gpointer key, gpointer value)
{
  gpointer oldkey = NULL, oldval = NULL;

  if (g_hash_table_lookup_extended(ht, key, &oldkey, &oldval))
    {
      gconf_log(GCL_WARNING, "Hash key `%s' is already in the table!",
                (gchar*)key);
      return;
    }
  else
    {
      g_hash_table_insert(ht, key, value);
    }
}
#endif

static gboolean cache_is_nonexistent    (Cache       *cache,
                                         const gchar *key);
static void     cache_set_nonexistent   (Cache       *cache,
                                         const gchar *key,
                                         gboolean     setting);
static void     cache_unset_nonexistent (Cache       *cache,
                                         const gchar *key);
static void     cache_insert          (Cache       *cache,
                                       Dir         *d);

static void     cache_remove_from_parent (Cache *cache,
                                          Dir   *d);
static void     cache_add_to_parent      (Cache *cache,
                                          Dir   *d);

static GHashTable *caches_by_root_dir = NULL;

struct _Cache {
  gchar* root_dir;
  GHashTable* cache;
  GHashTable* nonexistent_cache;
  guint dir_mode;
  guint file_mode;
  guint refcount;
};

Cache*
cache_get (const gchar  *root_dir,
           guint dir_mode,
           guint file_mode)
{
  Cache* cache = NULL;

  if (caches_by_root_dir == NULL)
    caches_by_root_dir = g_hash_table_new (g_str_hash, g_str_equal);
  else
    cache = g_hash_table_lookup (caches_by_root_dir, root_dir);

  if (cache != NULL)
    {
      cache->refcount += 1;
      return cache;
    }

  cache = g_new(Cache, 1);

  cache->root_dir = g_strdup(root_dir);

  cache->cache = g_hash_table_new(g_str_hash, g_str_equal);
  cache->nonexistent_cache = g_hash_table_new_full(g_str_hash, g_str_equal,
                                                   g_free, NULL);

  cache->dir_mode = dir_mode;
  cache->file_mode = file_mode;
  cache->refcount = 1;

  safe_g_hash_table_insert (caches_by_root_dir, cache->root_dir, cache);
  
  return cache;
}

static void cache_destroy_foreach(const gchar* key,
                                  Dir* dir, gpointer data);

void
cache_unref (Cache *cache)
{
  g_return_if_fail (cache != NULL);
  g_return_if_fail (cache->refcount > 0);

  if (cache->refcount > 1)
    {
      cache->refcount -= 1;
      return;
    }

  g_hash_table_remove (caches_by_root_dir, cache->root_dir);
  if (g_hash_table_size (caches_by_root_dir) == 0)
    {
      g_hash_table_destroy (caches_by_root_dir);
      caches_by_root_dir = NULL;
    }
  
  g_free(cache->root_dir);
  g_hash_table_foreach(cache->cache, (GHFunc)cache_destroy_foreach,
                       NULL);
  g_hash_table_destroy(cache->cache);
  g_hash_table_destroy(cache->nonexistent_cache);
  
  g_free(cache);
}


typedef struct _SyncData SyncData;
struct _SyncData {
  gboolean failed;
  Cache* dc;
  gboolean deleted_some;
};

static void
listify_foreach (gpointer key, gpointer value, gpointer data)
{
  GSList **list = data;

  *list = g_slist_prepend (*list, value);
}

static void
cache_sync_foreach (Dir      *dir,
                    SyncData *sd)
{
  GError* error = NULL;
  gboolean deleted;
  
  deleted = FALSE;
  
  /* log errors but don't report the specific ones */
  if (!dir_sync (dir, &deleted, &error))
    {
      sd->failed = TRUE;
      g_return_if_fail (error != NULL);
      gconf_log (GCL_ERR, "%s", error->message);
      g_error_free (error);
      g_return_if_fail (dir_sync_pending (dir));
    }
  else
    {
      g_return_if_fail (error == NULL);
      g_return_if_fail (!dir_sync_pending (dir));

      if (deleted)
        {
          /* Get rid of this directory */
          cache_remove_from_parent (sd->dc, dir);
          g_hash_table_remove (sd->dc->cache,
                               dir_get_name (dir));
          cache_set_nonexistent (sd->dc, dir_get_name (dir),
                                 TRUE);
          dir_destroy (dir);

          sd->deleted_some = TRUE;
        }
    }
}

static int
dircmp (gconstpointer a,
        gconstpointer b)
{
  Dir *dir_a = (Dir*) a;
  Dir *dir_b = (Dir*) b;
  const char *key_a = dir_get_name (dir_a);
  const char *key_b = dir_get_name (dir_b);
  
  /* This function is supposed to sort the list such that
   * subdirectories are synced prior to their parents,
   * thus ensuring that we are always able to get rid
   * of directories that we don't need anymore.
   *
   * Keys with an ancestor/descendent relationship are always
   * sorted with descendent before ancestor. Other keys are sorted
   * in order to alphabetize directories, i.e. we find the common
   * path segments and alphabetize the level below the common level.
   * /foo/bar/a before /foo/bar/b, etc.
   *
   * This ensures that our sort function has proper semantics.
   */

  if (gconf_key_is_below (key_a, key_b))
    return 1; /* a above b, so b is earlier in the list */
  else if (gconf_key_is_below (key_b, key_a))
    return -1;
  else
    {
      const char *ap = key_a;
      const char *bp = key_b;

      while (*ap && *bp && *ap == *bp)
        {
          ++ap;
          ++bp;
        }
      
      if (*ap == '\0' && *bp == '\0')
        return 0;
      
      /* we don't care about localization here,
       * just some fixed order. Either
       * *ap or *bp may be '\0' if you have keys like
       * "foo" and "foo_bar"
       */
      if (*ap < *bp)
        return -1;
      else
        return 1;
    }
}

gboolean
cache_sync (Cache    *cache,
            GError  **err)
{
  SyncData sd = { FALSE, NULL, FALSE };
  GSList *list;
  
  sd.dc = cache;

  gconf_log (GCL_DEBUG, "Syncing the dir cache");

 redo:
  sd.failed = FALSE;
  sd.deleted_some = FALSE;
  
  /* get a list of everything; we can't filter by
   * whether a sync is pending since we may make parents
   * of removed directories dirty when we sync their child
   * dir.
   */
  list = NULL;
  g_hash_table_foreach (cache->cache, (GHFunc)listify_foreach, &list);

  /* sort subdirs before parents */
  list = g_slist_sort (list, dircmp);

  /* sync it all */
  g_slist_foreach (list, (GFunc) cache_sync_foreach, &sd);
  g_slist_free (list);

  /* If we deleted some subdirs, we may now be able to delete
   * more parent dirs. So go ahead and do the sync again.
   * Yeah this could be more efficient.
   */
  if (!sd.failed && sd.deleted_some)
    goto redo;
  
  if (sd.failed && err && *err == NULL)
    {
      gconf_set_error (err, GCONF_ERROR_FAILED,
		       _("Failed to sync XML cache contents to disk"));
    }
  
  return !sd.failed;  
}

typedef struct _CleanData CleanData;
struct _CleanData {
  GTime now;
  Cache* cache;
  GTime length;
};

static gboolean
cache_clean_foreach(const gchar* key,
                    Dir* dir, CleanData* cd)
{
  GTime last_access;

  last_access = dir_get_last_access(dir);

  if ((cd->now - last_access) >= cd->length)
    {
      if (!dir_sync_pending(dir))
        {
          dir_destroy(dir);
          return TRUE;
        }
      else
        {
          gconf_log(GCL_WARNING, _("Unable to remove directory `%s' from the XML backend cache, because it has not been successfully synced to disk"),
                    dir_get_name(dir));
          return FALSE;
        }
    }
  else
    return FALSE;
}

void
cache_clean      (Cache        *cache,
                  GTime         older_than)
{
  CleanData cd = { 0, NULL, 0 };
  cd.cache = cache;
  cd.length = older_than;
  
  cd.now = time(NULL); /* ha ha, it's an online store! */
  
  g_hash_table_foreach_remove(cache->cache, (GHRFunc)cache_clean_foreach,
                              &cd);

#if 0
  size = g_hash_table_size(cache->cache);

  if (size != 0)
    gconf_log (GCL_DEBUG,
               "%u items remain in the cache after cleaning already-synced items older than %u seconds",
               size,
               older_than);
#endif
}

Dir*
cache_lookup     (Cache        *cache,
                  const gchar  *key,
                  gboolean create_if_missing,
                  GError  **err)
{
  Dir* dir;
  
  g_assert(key != NULL);
  g_return_val_if_fail(cache != NULL, NULL);
  
  /* Check cache */
  dir = g_hash_table_lookup(cache->cache, key);
  
  if (dir != NULL)
    {
      gconf_log(GCL_DEBUG, "Using dir %s from cache", key);
      return dir;
    }
  else
    {
      /* Not in cache, check whether we already failed
         to load it */
      if (cache_is_nonexistent(cache, key))
        {
          if (!create_if_missing)
            return NULL;
        }
      else
        {
          /* Didn't already fail to load, try to load */
          dir = dir_load (key, cache->root_dir, err);
          
          if (dir != NULL)
            {
              g_assert(err == NULL || *err == NULL);
              
              /* Cache it and add to parent */
              cache_insert (cache, dir);
              cache_add_to_parent (cache, dir);
              
              return dir;
            }
          else
            {
              /* Remember that we failed to load it */
              if (!create_if_missing)
                {
                  cache_set_nonexistent(cache, key, TRUE);
              
                  return NULL;
                }
              else
                {
                  if (err && *err)
                    {
                      g_error_free(*err);
                      *err = NULL;
                    }
                }
            }
        }
    }
  
  g_assert(dir == NULL);
  g_assert(create_if_missing);
  g_assert(err == NULL || *err == NULL);
  
  if (dir == NULL)
    {
      gconf_log(GCL_DEBUG, "Creating new dir %s", key);
      
      dir = dir_new(key, cache->root_dir, cache->dir_mode, cache->file_mode);

      if (!dir_ensure_exists(dir, err))
        {
          dir_destroy(dir);
          
          g_return_val_if_fail((err == NULL) ||
                               (*err != NULL) ,
                               NULL);
          return NULL;
        }
      else
        {
          cache_insert (cache, dir);
          cache_add_to_parent (cache, dir);
          cache_unset_nonexistent (cache, dir_get_name (dir));
        }
    }

  return dir;
}

static gboolean
cache_is_nonexistent(Cache* cache,
                     const gchar* key)
{
  return GPOINTER_TO_INT(g_hash_table_lookup(cache->nonexistent_cache,
                                             key));
}

static void
cache_set_nonexistent   (Cache* cache,
                         const gchar* key,
                         gboolean setting)
{
  if (setting)
    {
      /* don't use safe_ here, doesn't matter */
      g_hash_table_insert(cache->nonexistent_cache,
                          g_strdup(key),
                          GINT_TO_POINTER(TRUE));
    }
  else
    g_hash_table_remove(cache->nonexistent_cache, key);
}

static void
cache_unset_nonexistent (Cache       *cache,
                         const gchar *key)
{
  char *parent_key;

  g_return_if_fail (key != NULL);

  cache_set_nonexistent (cache, key, FALSE);

  if (strcmp (key, "/") == 0)
    return;

  parent_key = gconf_key_directory (key);

  cache_unset_nonexistent (cache, parent_key);

  g_free (parent_key);
}

static void
cache_insert (Cache* cache,
              Dir* d)
{
  g_return_if_fail(d != NULL);

  gconf_log(GCL_DEBUG, "Caching dir %s", dir_get_name(d));
  
  safe_g_hash_table_insert(cache->cache, (gchar*)dir_get_name(d), d);
}

static void
cache_destroy_foreach(const gchar* key,
                      Dir* dir, gpointer data)
{
#ifdef GCONF_ENABLE_DEBUG
  if (dir_sync_pending (dir))
    gconf_log(GCL_DEBUG, "Destroying a directory (%s) with sync still pending",
              dir_get_name (dir));
#endif
  dir_destroy (dir);
}

static void
cache_remove_from_parent (Cache *cache,
                          Dir   *d)
{
  Dir *parent;
  const char *name;

  /* We have to actually force a load here, to decide
   * whether to delete the parent.
   */
  parent = cache_lookup (cache, dir_get_parent_name (d),
                         TRUE, NULL);

  /* parent == d means d is the root dir */
  if (parent == NULL || parent == d)
    return;
  
  name = gconf_key_key (dir_get_name (d));

  dir_child_removed (parent, name);
}

static void
cache_add_to_parent (Cache *cache,
                     Dir   *d)
{
  Dir *parent;
  const char *name;

  parent = cache_lookup (cache, dir_get_parent_name (d),
                         FALSE, NULL);

  /* parent == d means d is the root dir */
  if (parent == NULL || parent == d)
    return;

  name = gconf_key_key (dir_get_name (d));

  dir_child_added (parent, name);
}


void
xml_test_cache (void)
{
#ifndef GCONF_DISABLE_TESTS
  


#endif
}
