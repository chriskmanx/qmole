
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
#include "gconf/gconf-backend.h"
#include "gconf/gconf-internals.h"
#include "gconf/gconf.h"

#include "xml-cache.h"


#include <libxml/tree.h>
#include <libxml/parser.h>

#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>

/*
 * Overview
 * 
 * Basically we have a directory tree underneath an arbitrary root
 * directory.  The directory tree reflects the configuration
 * namespace. Each directory contains an XML file which contains
 * metadata for the directory and the key-value pairs in that
 * directory.  The magic file in each directory is called %gconf.xml,
 * and can't clash with the database namespace because names containing
 * % aren't allowed.  So:
 *
 * /
 *  %gconf.xml
 *   guppi/
 *     %gconf.xml
 *   gnumeric/
 *     %gconf.xml
 *
 *
 * Locking
 * 
 * Locking doesn't _really_ matter because there's only one instance
 * of the daemon at a time. However, eventually we want a non-daemon
 * command line tool and library, e.g. for the debconf stuff, 
 * so we will eventually have locking. I'll figure out then how
 * it will work.
 *
 * Caching
 *
 * I haven't decided the best way to do caching yet. As a first cut;
 * we'll cache the parse tree for any files we've looked at. The cache
 * will contain time stamps; we'll nuke cache entries that haven't been
 * used in a while, either in a main loop timeout or by checking whenever 
 * we add a new cache entry. Parse trees correspond to "directories" in the
 * configuration namespace.
 *
 * A more precise cache will store specific key-value pairs; this cache
 * will probably contain a pointer to the parse tree node the key-value
 * pair is inside.
 *
 * We'll of course need a "dirty" list of stuff not yet written to disk.
 *
 * We'll save the mod time of parse trees when we load them, so we can 
 * paranoia check that no one has change the file before we save.
 *
 * Ideally we could monitor our own process size and also free up
 * cache whenever we started to use massive RAM. However, not sure
 * this can be done at all portably. Could possibly have some measure
 * of parse tree size.
 *
 * The libxml parse trees are pretty huge, so in theory we could
 * "compress" them by extracting all the information we want into a
 * specialized data structure, then nuking the parse tree. However,
 * that would add more CPU overhead at load and save time. Anyway, as
 * a first cut I'm not going to do this, we might do it later.
 *
 * Atomic Saving
 *
 * We'll want to save atomically by creating a temporary file for the
 * new file version, renaming the original file, moving the temporary
 * file into place, then deleting the original file, checking for
 * errors and mod times along the way.
 *      
 * Failed lookup caching
 *
 * If a key/directory doesn't exist, we create a cache entry anyway
 * so we can rapidly re-determine that it doesn't exist.
 * We also need to save "dirty" nonexistent entries, so we can delete
 * the stuff off disk.  
 * 
 */

typedef struct _XMLSource XMLSource;

/* XMLSource **/

struct _XMLSource {
  GConfSource source; /* inherit from GConfSource */
  Cache* cache;
  gchar* root_dir;
  guint timeout_id;
  GConfLock* lock;
  guint dir_mode;
  guint file_mode;
};

static XMLSource* xs_new       (const gchar* root_dir,
                                guint dir_mode,
                                guint file_mode,
                                GConfLock* lock);
static void       xs_destroy   (XMLSource* source);

/*
 * VTable functions
 */

 /* shutdown() is a BSD libc function */
static void          x_shutdown        (GError** err);

static GConfSource*  resolve_address (const gchar* address,
                                      GError** err);

static void          lock            (GConfSource* source,
                                      GError** err);

static void          unlock          (GConfSource* source,
                                      GError** err);

static gboolean     readable         (GConfSource* source,
                                      const gchar* key,
                                      GError** err);

static gboolean     writable        (GConfSource* source,
                                      const gchar* key,
                                      GError** err);

static GConfValue*   query_value     (GConfSource* source,
                                      const gchar* key,
                                      const gchar** locales,
                                      gchar** schema_name,
                                      GError** err);

static GConfMetaInfo*query_metainfo  (GConfSource* source,
                                      const gchar* key,
                                      GError** err);

static void          set_value       (GConfSource* source,
                                      const gchar* key,
                                      const GConfValue* value,
                                      GError** err);

static GSList*       all_entries    (GConfSource* source,
                                     const gchar* dir,
                                     const gchar** locales,
                                     GError** err);

static GSList*       all_subdirs     (GConfSource* source,
                                      const gchar* dir,
                                      GError** err);

static void          unset_value     (GConfSource* source,
                                      const gchar* key,
                                      const gchar* locale,
                                      GError** err);

static gboolean      dir_exists      (GConfSource *source,
                                      const gchar *dir,
                                      GError** err);

static void          remove_dir      (GConfSource* source,
                                      const gchar* dir,
                                      GError** err);

static void          set_schema      (GConfSource* source,
                                      const gchar* key,
                                      const gchar* schema_key,
                                      GError** err);

static gboolean      sync_all        (GConfSource* source,
                                      GError** err);

static void          destroy_source  (GConfSource* source);

static void          clear_cache     (GConfSource* source);

static void          blow_away_locks (const char *address);

static GConfBackendVTable xml_vtable = {
  sizeof (GConfBackendVTable),
  x_shutdown,
  resolve_address,
  lock,
  unlock,
  readable,
  writable,
  query_value,
  query_metainfo,
  set_value,
  all_entries,
  all_subdirs,
  unset_value,
  dir_exists,
  remove_dir,
  set_schema,
  sync_all,
  destroy_source,
  clear_cache,
  blow_away_locks,
  NULL, /* set_notify_func */
  NULL, /* add_listener    */
  NULL  /* remove_listener */
};

static void          
x_shutdown (GError** err)
{
  gconf_log(GCL_DEBUG, _("Unloading XML backend module."));
}

static void
lock (GConfSource* source,
      GError** err)
{
  

}

static void
unlock (GConfSource* source,
        GError** err)
{


}

static gboolean
readable (GConfSource* source,
          const gchar* key,
          GError** err)
{

  return TRUE;
}

static gboolean
writable (GConfSource* source,
           const gchar* key,
           GError** err)
{

  return TRUE;
}

static char*
get_dir_from_address (const char *address,
                      GError    **err)
{
  char *root_dir;
  int len;
  
  root_dir = gconf_address_resource (address);

  if (root_dir == NULL)
    {
      gconf_set_error (err, GCONF_ERROR_BAD_ADDRESS,
                       _("Couldn't find the XML root directory in the address `%s'"),
                       address);
      return NULL;
    }

  /* Chop trailing '/' to canonicalize */
  len = strlen (root_dir);

  if (root_dir[len-1] == '/')
    root_dir[len-1] = '\0';

  return root_dir;
}

static char*
get_lock_dir_from_root_dir (const char *root_dir)
{
  gchar* lockdir;
  
  lockdir = gconf_concat_dir_and_key (root_dir, "%gconf-xml-backend.lock");

  return lockdir;
}

static GConfSource*  
resolve_address (const gchar* address, GError** err)
{
  struct stat statbuf;
  gchar* root_dir;
  XMLSource* xsource;
  GConfSource* source;
  gint flags = 0;
  GConfLock* lock = NULL;
  guint dir_mode = 0700;
  guint file_mode = 0600;
  gchar** address_flags;
  gchar** iter;
  gboolean force_readonly;
  
  root_dir = get_dir_from_address (address, err);
  if (root_dir == NULL)
    return NULL;

  if (g_stat (root_dir, &statbuf) == 0)
    {
      /* Already exists, base our dir_mode on it */
      dir_mode = _gconf_mode_t_to_mode (statbuf.st_mode);

      /* dir_mode without search bits */
      file_mode = dir_mode & (~0111); 
    }
  else if (g_mkdir (root_dir, dir_mode) < 0)
    {
      /* Error out even on EEXIST - shouldn't happen anyway */
      gconf_set_error (err, GCONF_ERROR_FAILED,
		       _("Could not make directory `%s': %s"),
		      (gchar *)root_dir, g_strerror (errno));
      g_free (root_dir);
      return NULL;
    }

  force_readonly = FALSE;
  
  address_flags = gconf_address_flags (address);  
  if (address_flags)
    {
      iter = address_flags;
      while (*iter)
        {
          if (strcmp (*iter, "readonly") == 0)
            {
              force_readonly = TRUE;
              break;
            }

          ++iter;
        }
    }

  g_strfreev (address_flags);

  {
    /* See if we're writable */
    gboolean writable;
    int fd;
    gchar* testfile;

    writable = FALSE;
    
    if (!force_readonly)
      {
        testfile = g_strconcat(root_dir, "/.testing.writeability", NULL);    
        
        fd = g_open(testfile, O_CREAT|O_WRONLY, S_IRWXU);
        
        if (fd >= 0)
          {
            writable = TRUE;
            close(fd);
          }
        
        g_unlink(testfile);
        
        g_free(testfile);
      }
    
    if (writable)
      flags |= GCONF_SOURCE_ALL_WRITEABLE;

#ifdef HAVE_CORBA
    /* We only do locking if it's writable,
     * and if not using local locks,
     * which is sort of broken but close enough
     */
    if (writable && !gconf_use_local_locks ())
      {
        gchar* lockdir;

        lockdir = get_lock_dir_from_root_dir (root_dir);
        
        lock = gconf_get_lock(lockdir, err);

        if (lock != NULL)
          gconf_log(GCL_DEBUG, "Acquired lock directory `%s'", lockdir);
        
        g_free(lockdir);
        
        if (lock == NULL)
          {
            g_free(root_dir);
            return NULL;
          }
      }
#endif
  }

  {
    /* see if we're readable */
    gboolean readable = FALSE;
    GDir* d;

    d = g_dir_open(root_dir, 0, NULL);

    if (d != NULL)
      {
        readable = TRUE;
        g_dir_close(d);
      }
    
    if (readable)
      flags |= GCONF_SOURCE_ALL_READABLE;
  }

  if (!(flags & GCONF_SOURCE_ALL_READABLE) &&
      !(flags & GCONF_SOURCE_ALL_WRITEABLE))
    {
      gconf_set_error(err, GCONF_ERROR_BAD_ADDRESS, _("Can't read from or write to the XML root directory in the address \"%s\""), address);
      g_free(root_dir);
      return NULL;
    }  
  
  /* Create the new source */

  xsource = xs_new(root_dir, dir_mode, file_mode, lock);

  gconf_log(GCL_DEBUG,
            _("Directory/file permissions for XML source at root %s are: %o/%o"),
            root_dir, dir_mode, file_mode);
  
  source = (GConfSource*)xsource;

  source->flags = flags;
  
  g_free(root_dir);
  
  return source;
}

static GConfValue* 
query_value (GConfSource* source,
             const gchar* key,
             const gchar** locales,
             gchar** schema_name,
             GError** err)
{
  XMLSource* xs = (XMLSource*)source;
  gchar* parent;
  Dir* dir;
  GError* error = NULL;

  parent = gconf_key_directory(key);
  
  g_assert(parent != NULL);
  
  dir = cache_lookup(xs->cache, parent, FALSE, &error);

  /* We DO NOT want to return an error unless it represents a general
     problem with the backend; since we don't want to report stuff
     like "this key doesn't exist yet" - however this is a maintenance
     problem, since some errors may be added that need reporting. */
  if (error != NULL)
    {
      gconf_log(GCL_WARNING, "%s", error->message);
      g_error_free(error);
      error = NULL;
    }
  
  g_free(parent);
  parent = NULL;
  
  if (dir != NULL)
    {
      const gchar* relative_key;
      GConfValue* retval;
      
      relative_key = gconf_key_key(key);

      retval = dir_get_value(dir, relative_key, locales, schema_name, &error);

      /* perhaps we should be reporting this error... */
      if (error != NULL)
        {
          gconf_log(GCL_WARNING, "%s", error->message);
          g_error_free(error);
          error = NULL;
        }
      
      return retval;
    }
  else
    return NULL;
}

static GConfMetaInfo*
query_metainfo  (GConfSource* source, const gchar* key,
                 GError** err)
{
  XMLSource* xs = (XMLSource*)source;
  gchar* parent;
  Dir* dir;

  parent = gconf_key_directory(key);

  if (parent != NULL)
    {
      dir = cache_lookup(xs->cache, parent, FALSE, err);
      g_free(parent);
      parent = NULL;
      
      if (dir != NULL)
        {
          const gchar* relative_key;
      
          relative_key = gconf_key_key (key);

          return dir_get_metainfo (dir, relative_key, err);
        }
    }

  /* No metainfo found */
  return NULL;
}

static void          
set_value (GConfSource* source, const gchar* key, const GConfValue* value,
           GError** err)
{
  XMLSource* xs = (XMLSource*)source;
  Dir* dir;
  gchar* parent;
  
  g_return_if_fail(value != NULL);
  g_return_if_fail(source != NULL);
  
  parent = gconf_key_directory(key);
  
  g_assert(parent != NULL);
  
  dir = cache_lookup(xs->cache, parent, TRUE, err);
  
  g_free(parent);
  parent = NULL;

  if (dir == NULL)
    {
      g_return_if_fail((err == NULL || *err != NULL));
      return;
    }
  else
    {
      const gchar* relative_key;
      
      relative_key = gconf_key_key(key);
      
      dir_set_value(dir, relative_key, value, err);
    }
}


static GSList*             
all_entries    (GConfSource* source,
                const gchar* key,
                const gchar** locales,
                GError** err)
{
  XMLSource* xs = (XMLSource*)source;
  Dir* dir;

  dir = cache_lookup(xs->cache, key, FALSE, err);
  
  if (dir == NULL)
    return NULL;
  else
    return dir_all_entries(dir, locales, err);
}

static GSList*
all_subdirs     (GConfSource* source,
                 const gchar* key,
                 GError** err)
{
  Dir* dir;
  XMLSource* xs = (XMLSource*)source;
  GError *sync_err;
  
  /* We have to sync before we can do this, to see which
   * subdirs have gone away.
   */
  sync_err = NULL;
  cache_sync (xs->cache, &sync_err);
  if (sync_err)
    {
      gconf_log (GCL_WARNING, _("Error syncing the XML backend directory cache: %s"),
                 sync_err->message);
      g_error_free (sync_err);
      sync_err = NULL;
      /* continue, may as well try our best. */
    }
  
  dir = cache_lookup (xs->cache, key, FALSE, err);
  
  if (dir == NULL)
    return NULL;
  else
    return dir_all_subdirs (dir, err);
}

static void          
unset_value     (GConfSource* source,
                 const gchar* key,
                 const gchar* locale,
                 GError** err)
{
  XMLSource* xs = (XMLSource*)source;
  Dir* dir;
  gchar* parent;

  gconf_log(GCL_DEBUG, "XML backend: unset value `%s'", key);
  
  parent = gconf_key_directory(key);
  
  dir = cache_lookup(xs->cache, parent, FALSE, err);

  g_free(parent);
  
  if (dir == NULL)
    return;
  else
    {
      const gchar* relative_key;
  
      relative_key = gconf_key_key(key);

      dir_unset_value(dir, relative_key, locale, err);
    }
}

static gboolean
dir_exists      (GConfSource*source,
                 const gchar* key,
                 GError** err)
{
  XMLSource *xs = (XMLSource*)source;
  Dir* dir;
  
  dir = cache_lookup(xs->cache, key, FALSE, err);
  
  return (dir != NULL);
}  

static void          
remove_dir      (GConfSource* source,
                 const gchar* key,
                 GError** err)
{
  g_set_error (err, GCONF_ERROR,
               GCONF_ERROR_FAILED,
               _("Remove directory operation is no longer supported, just remove all the values in the directory"));
}

static void          
set_schema (GConfSource *source,
            const gchar *key,
            const gchar *schema_key,
            GError     **err)
{
  XMLSource* xs = (XMLSource*)source;

  Dir* dir;
  gchar* parent;

  g_return_if_fail (source != NULL);
  g_return_if_fail (key != NULL);

  parent = gconf_key_directory (key);
  
  g_assert (parent != NULL);
  
  dir = cache_lookup (xs->cache, parent, TRUE, err);
  
  g_free (parent);
  parent = NULL;

  if (dir == NULL)
    return; /* error should be set */
  else
    {
      const gchar* relative_key;
      
      relative_key = gconf_key_key (key);
      
      dir_set_schema (dir, relative_key, schema_key, err);
    }
}

static gboolean      
sync_all        (GConfSource* source,
                 GError** err)
{
  XMLSource* xs = (XMLSource*)source;

  return cache_sync (xs->cache, err);
}

static void          
destroy_source  (GConfSource* source)
{
  xs_destroy((XMLSource*)source);
}

static void
clear_cache     (GConfSource* source)
{
  XMLSource* xs = (XMLSource*)source;

  /* clean all entries older than 0 seconds */
  cache_clean(xs->cache, 0);
}

static void
blow_away_locks (const char *address)
{
  char *root_dir;
  char *lock_dir;
  GDir *dp;
  const char *dent;

  /* /tmp locks should never be stuck, and possible security issue to
   * blow them away
   */
  if (gconf_use_local_locks ())
    return;
  
  root_dir = get_dir_from_address (address, NULL);
  if (root_dir == NULL)
    return;

  lock_dir = get_lock_dir_from_root_dir (root_dir);

  dp = g_dir_open (lock_dir, 0, NULL);
  
  if (dp == NULL)
    {
      g_printerr (_("Could not open lock directory for %s to remove locks: %s\n"),
                  address, g_strerror (errno));
      goto out;
    }
  
  while ((dent = g_dir_read_name (dp)) != NULL)
    {
      char *path;
      
      path = g_build_filename (lock_dir, dent, NULL);

      if (g_unlink (path) < 0)
        {
          g_printerr (_("Could not remove file %s: %s\n"),
                      path, g_strerror (errno));
        }

      g_free (path);
    }

 out:

  if (dp)
    g_dir_close (dp);
  
  g_free (root_dir);
  g_free (lock_dir);
}

/* Initializer */

#ifndef G_PLATFORM_WIN32
/* If we use G_MODULE_EXPORT, *only* thusly marked functions will be
 * exported, and xml-test uses other ones, too.
 */
G_MODULE_EXPORT
#endif
const gchar*
g_module_check_init (GModule *module)
{
  gconf_log(GCL_DEBUG, _("Initializing XML backend module"));

  LIBXML_TEST_VERSION;
  xmlKeepBlanksDefault(1);

  return NULL;
}

#ifndef G_PLATFORM_WIN32
G_MODULE_EXPORT
#endif
GConfBackendVTable* 
gconf_backend_get_vtable(void)
{
  return &xml_vtable;
}

/* ****************************************************/

/*
 *  XMLSource
 */ 

/* This timeout periodically cleans up
   the old cruft in the cache */
static gboolean
cleanup_timeout(gpointer data)
{
  XMLSource* xs = (XMLSource*)data;

  cache_clean(xs->cache, 60*5 /* 5 minutes */);

  return TRUE;
}

static XMLSource*
xs_new       (const gchar* root_dir, guint dir_mode, guint file_mode, GConfLock* lock)
{
  XMLSource* xs;

  g_return_val_if_fail(root_dir != NULL, NULL);

  xs = g_new0(XMLSource, 1);

  xs->root_dir = g_strdup(root_dir);

  xs->cache = cache_get(xs->root_dir, dir_mode, file_mode);

  xs->timeout_id = g_timeout_add_seconds(60*5, /* 1 sec * 60 s/min * 5 min */
                                 cleanup_timeout,
                                 xs);

  xs->lock = lock;

  xs->dir_mode = dir_mode;
  xs->file_mode = file_mode;
  
  return xs;
}

static void
xs_destroy   (XMLSource* xs)
{
#ifdef HAVE_CORBA
  GError* error = NULL;
#endif

  g_return_if_fail(xs != NULL);

#ifdef HAVE_CORBA
  /* do this first in case we're in a "fast cleanup just before exit"
     situation */
  if (xs->lock != NULL && !gconf_release_lock(xs->lock, &error))
    {
      gconf_log (GCL_ERR, _("Failed to give up lock on XML directory \"%s\": %s"),
                 xs->root_dir, error->message);
      g_error_free(error);
      error = NULL;
    }
#endif

  if (!g_source_remove(xs->timeout_id))
    {
      /* should not happen, don't translate */
      gconf_log(GCL_ERR, "timeout not found to remove?");
    }
  
  cache_unref(xs->cache);
  g_free(xs->root_dir);
  g_free(xs);
}

