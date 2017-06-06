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
#include "xml-dir.h"
#include "xml-entry.h"

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

#include "gconf/gconf-internals.h"

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

static xmlDocPtr my_xml_parse_file (const char *filename,
                                    GError    **err);

static gboolean dir_rescan_subdirs (Dir* d, GError** err);

struct _Dir {
  gchar* key;
  gchar* parent_key;
  gchar* fs_dirname;
  gchar* xml_filename;
  guint root_dir_len;
  GTime last_access; /* so we know when to un-cache */
  xmlDocPtr doc;
  GHashTable* entry_cache; /* store key-value entries */
  guint dir_mode;
  guint file_mode;
  GSList *subdir_names;
  guint dirty : 1;
  guint need_rescan_subdirs : 1;
};

static void
dir_load_doc(Dir* d, GError** err);

static Entry* dir_make_new_entry(Dir* d, const gchar* relative_key);

static gboolean dir_forget_entry_if_useless(Dir* d, Entry* e);

static Dir*
dir_blank(const gchar* key)
{
  Dir* d;
  
  d = g_new0(Dir, 1);

#ifdef GCONF_ENABLE_DEBUG
  {
    gchar* why;
    if (!gconf_valid_key(key, &why)) {
      gconf_log(GCL_DEBUG, "key `%s' invalid: %s",
                key, why);
    }
    g_assert(gconf_valid_key(key, NULL));
  }
#endif
  
  d->key = g_strdup (key);
  d->parent_key = gconf_key_directory (key);
  
  d->last_access = time(NULL);
  d->doc = NULL;

  d->entry_cache = g_hash_table_new (g_str_hash, g_str_equal);
  
  d->dirty = FALSE;
  d->need_rescan_subdirs = TRUE;

  d->subdir_names = NULL;
  
  d->dir_mode = 0700;
  d->file_mode = 0600;
  
  return d;
}

Dir*
dir_new (const gchar  *keyname,
         const gchar  *xml_root_dir,
         guint dir_mode,
         guint file_mode)
{
  Dir* d;
  
  d = dir_blank(keyname);

  /* sync with dir_load() */
  d->fs_dirname = gconf_concat_dir_and_key(xml_root_dir, keyname);
  d->xml_filename =  g_strconcat(d->fs_dirname, "/%gconf.xml", NULL);
  d->root_dir_len = strlen(xml_root_dir);

  d->dir_mode = dir_mode;
  d->file_mode = file_mode;
  
  return d;
}

Dir*
dir_load (const gchar* key, const gchar* xml_root_dir, GError** err)
{
  Dir* d;
  gchar* fs_dirname;
  gchar* xml_filename;
  guint dir_mode = 0700;
  guint file_mode = 0600;
  
  g_return_val_if_fail(gconf_valid_key(key, NULL), NULL);
  
  fs_dirname = gconf_concat_dir_and_key(xml_root_dir, key);
  xml_filename = g_strconcat(fs_dirname, "/%gconf.xml", NULL);

  {
    struct stat s;
    gboolean notfound = FALSE;
    
    if (g_stat(xml_filename, &s) != 0)
      {
        if (errno != ENOENT)
          {
            gconf_set_error (err, GCONF_ERROR_FAILED,
                             _("Could not stat `%s': %s"),
                             xml_filename, g_strerror(errno));

          }
        
        notfound = TRUE;
      }
    else if (S_ISDIR(s.st_mode))
      {
        gconf_set_error (err, GCONF_ERROR_FAILED,
                         _("XML filename `%s' is a directory"),
                         xml_filename);
        notfound = TRUE;
      }

    if (notfound)
      {
        gconf_log(GCL_DEBUG, "dir file %s not found", xml_filename);
        g_free(fs_dirname);
        g_free(xml_filename);
        return NULL;
      }
    else
      {
        /* Take directory mode from the xml_root_dir, if possible */
        if (g_stat (xml_root_dir, &s) == 0)
          {
            dir_mode = _gconf_mode_t_to_mode (s.st_mode);
          }
        
        file_mode = dir_mode & ~0111; /* turn off search bits */
      }
  }

  d = dir_blank(key);

  /* sync with dir_new() */
  d->fs_dirname = fs_dirname;
  d->xml_filename = xml_filename;
  d->root_dir_len = strlen(xml_root_dir);

  d->dir_mode = dir_mode;
  d->file_mode = file_mode;
  
  gconf_log (GCL_DEBUG, "loaded dir %s", fs_dirname);
  
  return d;
}


static void
entry_destroy_foreach(const gchar* name, Entry* e, gpointer data)
{
  entry_destroy (e);
}

void
dir_destroy(Dir* d)
{
  g_free (d->key);
  g_free (d->parent_key);
  g_free (d->fs_dirname);
  g_free (d->xml_filename);

  g_slist_foreach (d->subdir_names, (GFunc) g_free, NULL);
  g_slist_free (d->subdir_names);
  
  g_hash_table_foreach (d->entry_cache, (GHFunc)entry_destroy_foreach,
                        NULL);
  
  g_hash_table_destroy (d->entry_cache);

  if (d->doc != NULL)
    xmlFreeDoc (d->doc);
  
  g_free (d);
}

static gboolean
create_fs_dir(const gchar* dir, const gchar* xml_filename,
              guint root_dir_len,
              guint dir_mode, guint file_mode,
              GError** err);

gboolean
dir_ensure_exists (Dir* d,
                   GError** err)
{
  if (!create_fs_dir(d->fs_dirname, d->xml_filename, d->root_dir_len,
                     d->dir_mode, d->file_mode,
                     err))
    {

      /* check that error is set */
      g_return_val_if_fail( (err == NULL) || (*err != NULL), FALSE );
      
      return FALSE;
    }
  else
    {
      return TRUE;
    }
}

static void
entry_sync_foreach(const gchar* name, Entry* e, gpointer data)
{
  entry_sync_to_node(e);
}

gboolean
dir_sync_pending (Dir *d)
{
  return d->dirty;
}

void
dir_child_removed (Dir        *d,
                   const char *child_name)
{
  GSList *tmp;
  
  /* dirty because we need to consider removing
   * this directory, it may have become empty.
   */
  d->dirty = TRUE;
  
  if (d->need_rescan_subdirs)
    return; /* subdir_names is totally invalid anyhow */

  tmp = d->subdir_names;
  while (tmp != NULL)
    {
      if (strcmp (tmp->data, child_name) == 0)
        {
          char *tofree = tmp->data;
          
          d->subdir_names = g_slist_remove (d->subdir_names,
                                            tofree);
          g_free (tofree);

          break;
        }
      
      tmp = tmp->next;
    }
}

void
dir_child_added (Dir        *d,
                 const char *child_name)
{
  if (d->need_rescan_subdirs)
    return;

  if (g_slist_find_custom (d->subdir_names,
                           child_name,
                           (GCompareFunc) strcmp) == NULL)
    d->subdir_names = g_slist_prepend (d->subdir_names,
                                       g_strdup (child_name));
}

/* directories auto-disappear when they're empty */
static gboolean
dir_useless (Dir *d)
{
  if (d->doc == NULL)
    dir_load_doc (d, NULL);

  if (d->need_rescan_subdirs)
    dir_rescan_subdirs (d, NULL);
  
  return
    d->subdir_names == NULL &&
    g_hash_table_size (d->entry_cache) == 0;
}

/* for info on why this is used rather than xmlDocDump or xmlSaveFile
 * and friends, see http://bugzilla.gnome.org/show_bug.cgi?id=108329 */
static int
gconf_xml_doc_dump (FILE *fp, xmlDocPtr doc)
{
  char *xmlbuf;
  int fd, n;
  
  xmlDocDumpFormatMemory (doc, (xmlChar **) &xmlbuf, &n, TRUE);
  if (n <= 0)
    {
      errno = ENOMEM;
      return -1;
    }
  
  if (fwrite (xmlbuf, sizeof (xmlChar), n, fp) < n)
    {
      xmlFree (xmlbuf);
      return -1;
    }
  
  xmlFree (xmlbuf);
  
  /* From the fflush(3) man page:
   *
   * Note that fflush() only flushes the user space buffers provided by the
   * C library. To ensure that the data is physically stored on disk the
   * kernel buffers must be flushed too, e.g. with sync(2) or fsync(2).
   */
  
  /* flush user-space buffers */
  if (fflush (fp) != 0)
    return -1;
  
  if ((fd = fileno (fp)) == -1)
    return -1;
  
#ifdef HAVE_FSYNC
  /* sync kernel-space buffers to disk */
  if (fsync (fd) == -1)
    return -1;
#endif

  return 0;
}

gboolean
dir_sync (Dir      *d,
          gboolean *deleted,
          GError  **err)
{
  gboolean retval = TRUE;

  if (deleted)
    *deleted = FALSE;  

  if (!d->dirty)
    return TRUE; 

  gconf_log (GCL_DEBUG, "Syncing dir \"%s\"", d->key);
  
  d->last_access = time (NULL);
  
  if (dir_useless (d))
    {
      gconf_log (GCL_DEBUG, "Deleting useless dir \"%s\"",
                 d->key);
      
      if (g_unlink (d->xml_filename) != 0)
        {
          gconf_set_error (err, GCONF_ERROR_FAILED, _("Failed to delete \"%s\": %s"),
                           d->xml_filename, g_strerror (errno));
          return FALSE;
        }

      if (strcmp (d->key, "/") != 0) /* don't delete root dir */
        {
          if (g_rmdir (d->fs_dirname) != 0)
            {
              gconf_set_error (err, GCONF_ERROR_FAILED, _("Failed to delete \"%s\": %s"),
                               d->fs_dirname, g_strerror (errno));
              return FALSE;
            }
        }

      if (deleted)
        *deleted = TRUE;
    }
  else
    {
      gboolean old_existed = FALSE;
      gchar* tmp_filename;
      gchar* old_filename;
      FILE* outfile;

      /* We should have a doc if deleted is FALSE */
      g_assert(d->doc != NULL);
      
      /* First make sure entry values are synced to their
         XML nodes */
      g_hash_table_foreach(d->entry_cache, (GHFunc)entry_sync_foreach, NULL);
      
      tmp_filename = g_strconcat(d->fs_dirname, "/%gconf.xml.tmp", NULL);
      old_filename = g_strconcat(d->fs_dirname, "/%gconf.xml.old", NULL);

      outfile = g_fopen (tmp_filename, "w");

      if (outfile == NULL)
        {
          /* Try to solve the problem by creating the FS dir */
          if (!g_file_test (d->fs_dirname, G_FILE_TEST_EXISTS))
            {
              if (create_fs_dir(d->fs_dirname, d->xml_filename,
                                d->root_dir_len,
                                d->dir_mode, d->file_mode,
                                err))
                outfile = g_fopen (tmp_filename, "w");
            }

          if (outfile == NULL)
            {
              /* Don't set error if it's already set by some
               * earlier failure.
               */
              if (err && *err == NULL)
                gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to write file `%s': %s"), 
                                tmp_filename, g_strerror(errno));
              
              retval = FALSE;
              
              goto failed_end_of_sync;
            }
        }

#ifdef HAVE_FCHMOD
      /* Set permissions on the new file */
      if (fchmod (fileno (outfile), d->file_mode) != 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, 
                          _("Failed to set mode on `%s': %s"),
                          tmp_filename, g_strerror(errno));
          
          retval = FALSE;
          goto failed_end_of_sync;
        }
#endif

      if (gconf_xml_doc_dump (outfile, d->doc) < 0)
        {
          gconf_set_error (err, GCONF_ERROR_FAILED, 
                           _("Failed to write XML data to `%s': %s"),
                           tmp_filename, g_strerror (errno));
          
          retval = FALSE;
          goto failed_end_of_sync;
        }

      if (fclose (outfile) < 0)
        {
          gconf_set_error (err, GCONF_ERROR_FAILED, 
                           _("Failed to close file `%s': %s"),
                           tmp_filename, g_strerror (errno));
          
          retval = FALSE;
          outfile = NULL;
          goto failed_end_of_sync;
        }

      outfile = NULL;
      
#ifndef HAVE_FCHMOD
      /* Set permissions on the new file */
      if (chmod (tmp_filename, d->file_mode) != 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, 
                          _("Failed to set mode on `%s': %s"),
                          tmp_filename, g_strerror(errno));
          
          retval = FALSE;
          goto failed_end_of_sync;
        }
#endif

      old_existed = g_file_test (d->xml_filename, G_FILE_TEST_EXISTS);

      if (old_existed)
        {
          if (g_rename(d->xml_filename, old_filename) < 0)
            {
              gconf_set_error(err, GCONF_ERROR_FAILED, 
                              _("Failed to rename `%s' to `%s': %s"),
                              d->xml_filename, old_filename, g_strerror(errno));

              retval = FALSE;
              goto failed_end_of_sync;
            }
        }

      if (g_rename(tmp_filename, d->xml_filename) < 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to rename `%s' to `%s': %s"),
                          tmp_filename, d->xml_filename, g_strerror(errno));

          /* Put the original file back, so this isn't a total disaster. */
          if (g_rename(old_filename, d->xml_filename) < 0)
            {
              gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to restore `%s' from `%s': %s"),
                              d->xml_filename, old_filename, g_strerror(errno));
            }

          retval = FALSE;
          goto failed_end_of_sync;
        }

      if (old_existed)
        {
          if (g_unlink(old_filename) < 0)
            {
              gconf_log(GCL_WARNING, _("Failed to delete old file `%s': %s"),
                         old_filename, g_strerror(errno));
              /* Not a failure, just leaves cruft around. */
            }
        }

    failed_end_of_sync:
      
      g_free(old_filename);
      g_free(tmp_filename);
      if (outfile)
        fclose (outfile);
    }

  if (retval)
    d->dirty = FALSE;

  return retval;
}

void
dir_set_value (Dir* d, const gchar* relative_key,
               const GConfValue* value, GError** err)
{
  Entry* e;
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_if_fail( (err == NULL) || (*err != NULL) );
      return;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);
  
  if (e == NULL)
    e = dir_make_new_entry(d, relative_key);

  entry_set_value(e, value);

  d->last_access = time(NULL);
  entry_set_mod_time(e, d->last_access);

  entry_set_mod_user(e, g_get_user_name());
  
  d->dirty = TRUE;
}

GTime
dir_get_last_access (Dir          *d)
{
  return d->last_access;
}

GConfValue*
dir_get_value   (Dir* d,
                 const gchar* relative_key,
                 const gchar** locales,
                 gchar** schema_name,
                 GError** err)
{
  Entry* e;
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);

  d->last_access = time(NULL);

  if (e == NULL)
    {
      /* No entry; return */
      return NULL;
    }
  else
    {
      GConfValue* val;

      g_assert(e != NULL);

      val = entry_get_value (e, locales, err);

      /* Get schema name if requested */
      if (schema_name && entry_get_schema_name (e))
        *schema_name = g_strdup (entry_get_schema_name (e));
      
      /* return copy of the value */
      if (val != NULL)
        return gconf_value_copy(val);
      else
        return NULL;
    }
}

const gchar*
dir_get_name (Dir *d)
{
  g_return_val_if_fail (d != NULL, NULL);
  return d->key;
}

const char*
dir_get_parent_name (Dir *d)
{
  g_return_val_if_fail (d != NULL, NULL);
  return d->parent_key;
}

GConfMetaInfo*
dir_get_metainfo(Dir* d, const gchar* relative_key, GError** err)
{
  Entry* e;
  
  d->last_access = time(NULL);
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);

  if (e == NULL)
    return NULL;
  else
    return entry_get_metainfo(e);
}

void
dir_unset_value (Dir* d, const gchar* relative_key,
                 const gchar* locale, GError** err)
{
  Entry* e;
  
  d->last_access = time(NULL);
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_if_fail( (err == NULL) || (*err != NULL) );
      return;
    }
  
  e = g_hash_table_lookup(d->entry_cache, relative_key);
  
  if (e == NULL)     /* nothing to change */
    return;

  if (entry_unset_value(e, locale))
    {
      /* If entry_unset() returns TRUE then
         the entry was changed (not already unset) */
      
      d->dirty = TRUE;
      
      if (dir_forget_entry_if_useless(d, e))
        {
          /* entry is destroyed */
          return;
        }
      else
        {
          entry_set_mod_time(e, d->last_access);
          entry_set_mod_user(e, g_get_user_name());
        }
    }
  else
    {
      /* Check uselessness anyway; this ensures that if it was useless
         when the daemon started or we otherwise missed its lack of
         utility, we clean it up if the user does an explicit unset */
      dir_forget_entry_if_useless(d, e);
    }
}

typedef struct _ListifyData ListifyData;

struct _ListifyData {
  GSList* list;
  const gchar* name;
  const gchar** locales;
};

static void
listify_foreach(const gchar* key, Entry* e, ListifyData* ld)
{
  GConfValue* val;
  GConfEntry* entry;
  GError* error = NULL;
  
  val = entry_get_value (e, ld->locales, &error);

  if (error != NULL)
    {
      g_assert (val == NULL);
      g_error_free (error);
      return;
    }
  
  entry = gconf_entry_new_nocopy (g_strdup(key),
                                  val ? gconf_value_copy(val) : NULL);
  
  if (entry_get_schema_name (e))
    {
      gconf_entry_set_schema_name (entry, entry_get_schema_name (e));
    }
  
  ld->list = g_slist_prepend(ld->list, entry);
}

GSList*
dir_all_entries (Dir* d, const gchar** locales, GError** err)
{
  ListifyData ld;
  
  if (d->doc == NULL)
    dir_load_doc(d, err);

  if (d->doc == NULL)
    {
      g_return_val_if_fail( (err == NULL) || (*err != NULL), NULL );
      return NULL;
    }
  
  ld.list = NULL;
  ld.name = d->key;
  ld.locales = locales;

  g_hash_table_foreach(d->entry_cache, (GHFunc)listify_foreach,
                       &ld);
  
  return ld.list;
}

static GSList*
copy_string_list (GSList *src)
{
  GSList *copy;
  GSList *tmp;
  
  copy = NULL;
  tmp = src;
  while (tmp != NULL)
    {
      copy = g_slist_prepend (copy, g_strdup (tmp->data));
      tmp = tmp->next;
    }

  copy = g_slist_reverse (copy);

  return copy;
}

static gboolean
dir_rescan_subdirs (Dir* d, GError** err)
{
  GDir* dp;
  const char* dent;
  struct stat statbuf;
  GSList* retval = NULL;
  gchar* fullpath;
  gchar* fullpath_end;
  guint len;
  guint subdir_len;
  
  if (d->doc == NULL)
    dir_load_doc (d, err);
  
  if (d->doc == NULL)
    {
      g_return_val_if_fail ((err == NULL) || (*err != NULL), FALSE);
      return FALSE;
    }

  if (!d->need_rescan_subdirs)
    return TRUE;

  g_slist_foreach (d->subdir_names, (GFunc) g_free, NULL);
  g_slist_free (d->subdir_names);
  d->subdir_names = NULL;
  
  dp = g_dir_open (d->fs_dirname, 0, NULL);

  if (dp == NULL)
    {
      d->need_rescan_subdirs = FALSE;
      return TRUE;
    }

  len = strlen(d->fs_dirname);
  subdir_len = PATH_MAX - len;
  
  fullpath = g_malloc0(subdir_len + len + 20); /* ensure null termination */
  strcpy(fullpath, d->fs_dirname);
  
  fullpath_end = fullpath + len;
  *fullpath_end = '/';
  ++fullpath_end;
  *fullpath_end = '\0';

  while ((dent = g_dir_read_name(dp)) != NULL)
    {
      /* ignore all dot-files */
      if (dent[0] == '.')
        continue;

      len = strlen(dent);

      if (len < subdir_len)
        {
          strcpy(fullpath_end, dent);
          strncpy(fullpath_end+len, "/%gconf.xml", subdir_len - len);
        }
      else
        continue; /* Shouldn't ever happen since PATH_MAX is available */
      
      if (g_stat(fullpath, &statbuf) < 0)
        {
          /* This is some kind of cruft, not an XML directory */
          continue;
        }
      
      retval = g_slist_prepend (retval, g_strdup(dent));
    }

  /* if this fails, we really can't do a thing about it
   * and it's not a meaningful error
   */
  g_dir_close (dp);

  g_free (fullpath);

  d->subdir_names = retval;
  d->need_rescan_subdirs = FALSE;

  return TRUE;
}

GSList*
dir_all_subdirs (Dir* d, GError** err)
{
  if (!dir_rescan_subdirs (d, err))
    return NULL;

  return copy_string_list (d->subdir_names);
}

void
dir_set_schema  (Dir         *d,
                 const gchar *relative_key,
                 const gchar *schema_key,
                 GError     **err)
{
  Entry* e;

  if (d->doc == NULL)
    dir_load_doc (d, err);

  if (d->doc == NULL)
    {
      g_return_if_fail ((err == NULL) || (*err != NULL));
      return;
    }
  
  d->dirty = TRUE;
  d->last_access = time (NULL);
  
  e = g_hash_table_lookup (d->entry_cache, relative_key);

  if (e == NULL)
    e = dir_make_new_entry (d, relative_key);

  entry_set_mod_time (e, d->last_access);

  entry_set_schema_name (e, schema_key);

  if (schema_key == NULL)
    dir_forget_entry_if_useless (d, e);
}

/* private Dir functions */

static void
dir_fill_cache_from_doc(Dir* d);

static void
dir_load_doc(Dir* d, GError** err)
{
  gboolean xml_already_exists = TRUE;
  gboolean need_backup = FALSE;
  struct stat statbuf;
  
  g_return_if_fail(d->doc == NULL);

  if (stat(d->xml_filename, &statbuf) < 0)
    {
      switch (errno)
        {
        case ENOENT:
          xml_already_exists = FALSE;
          break;
        case ENOTDIR:
#ifdef ELOOP
        case ELOOP:
#endif
        case EFAULT:
        case EACCES:
        case ENOMEM:
        case ENAMETOOLONG:
        default:
          /* These are all fatal errors */
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to stat `%s': %s"),
                          d->xml_filename, g_strerror(errno));
          return;
          break;
        }
    }

  if (statbuf.st_size == 0)
    {
      xml_already_exists = FALSE;
    }

  if (xml_already_exists)
    {
      GError *tmp_err;
      gboolean error_was_fatal;

      error_was_fatal = FALSE;
      tmp_err = NULL;
      d->doc = my_xml_parse_file (d->xml_filename, &tmp_err);

      if (tmp_err != NULL)
        {
          gconf_log (GCL_WARNING,
                     "%s", tmp_err->message);

          /* file errors are assumed to be some kind of
           * blowup, like out of file descriptors, so
           * we play it safe and don't touch anything
           */
          if (tmp_err->domain == G_FILE_ERROR)
            error_was_fatal = TRUE;
          
          g_error_free (tmp_err);
        }

      if (error_was_fatal)
        return;
    }
  
  /* We recover from parse errors instead of passing them up */

  /* This has the potential to just blow away an entire corrupted
   * config file; but I think that is better than the alternatives
   * (disabling config for a directory because the document is mangled).
   *
   * Parse errors really should not happen from an XML file we created
   * ourselves anyway...
   */  

  /* Also we create empty %gconf.xml files when we create a new dir,
   * and those return a parse error, though they should be trapped
   * by the statbuf.st_size == 0 check above.
   */
  
  if (d->doc == NULL)
    {
      if (xml_already_exists)
        need_backup = TRUE; /* rather uselessly save whatever broken stuff was in the file */
          
      /* Create a new doc */
      
      d->doc = xmlNewDoc((xmlChar *)"1.0");
    }
  
  if (d->doc->xmlRootNode == NULL)
    {
      /* fill it in */
      d->doc->xmlRootNode = xmlNewDocNode(d->doc, NULL, "gconf", NULL);
    }
  else if (strcmp((char*)d->doc->xmlRootNode->name, "gconf") != 0)
    {
      xmlFreeDoc(d->doc);
      d->doc = xmlNewDoc((xmlChar*)"1.0");
      d->doc->xmlRootNode = xmlNewDocNode(d->doc, NULL, (xmlChar *)"gconf", NULL);
      need_backup = TRUE; /* save broken stuff */
    }
  else
    {
      /* We had an initial doc with a valid root */
      /* Fill child_cache from entries */
      dir_fill_cache_from_doc(d);
    }

  if (need_backup)
    {
      /* Back up the file we failed to parse, if it exists,
         we aren't going to be able to do anything if this call
         fails
      */
      
      gchar* backup = g_strconcat(d->xml_filename, ".bak", NULL);
      int fd;
      
      g_rename(d->xml_filename, backup);
      
      /* Recreate %gconf.xml to maintain our integrity and be sure
         all_subdirs works */
      /* If we failed to rename, we just give up and truncate the file */
      fd = g_open(d->xml_filename, O_CREAT | O_WRONLY | O_TRUNC, d->file_mode);
      if (fd >= 0)
        close(fd);
      
      g_free(backup);
    }
  
  g_assert(d->doc != NULL);
  g_assert(d->doc->xmlRootNode != NULL);
}

static Entry*
dir_make_new_entry(Dir* d, const gchar* relative_key)
{
  Entry* e;

  g_return_val_if_fail(d->doc != NULL, NULL);
  g_return_val_if_fail(d->doc->xmlRootNode != NULL, NULL);
  
  e = entry_new(relative_key);

  entry_set_node(e, xmlNewChild(d->doc->xmlRootNode, NULL, (xmlChar *)"entry", NULL));
  
  safe_g_hash_table_insert(d->entry_cache, (gchar*)entry_get_name(e), e);
  
  return e;
}

static gboolean
dir_forget_entry_if_useless(Dir* d, Entry* e)
{
  GConfValue* val;
  
  if (entry_get_schema_name(e) != NULL)
    return FALSE;
  
  val = entry_get_value(e, NULL, NULL);
  
  if (val != NULL)
    return FALSE; /* not useless */
      
  g_hash_table_remove(d->entry_cache, entry_get_name(e));

  entry_destroy(e);

  return TRUE;
}

static void
dir_fill_cache_from_doc(Dir* d)
{
  xmlNodePtr node;
  
  if (d->doc == NULL ||
      d->doc->xmlRootNode == NULL ||
      d->doc->xmlRootNode->xmlChildrenNode == NULL)
    {
      /* Empty document - just return. */
      return;
    }

  node = d->doc->xmlRootNode->xmlChildrenNode;

  while (node != NULL)
    {
      if (node->type == XML_ELEMENT_NODE && 
          (strcmp((xmlChar *)node->name, "entry") == 0))
        {
          gchar* attr = my_xmlGetProp(node, "name");

          if (attr != NULL)
            {
              if (g_hash_table_lookup(d->entry_cache, attr) != NULL)
                {
                  gconf_log(GCL_WARNING,
                             _("Duplicate entry `%s' in `%s', ignoring"),
                             attr, d->xml_filename);
                }
              else
                {
                  Entry* e;
                  
                  e = entry_new(attr);

                  entry_set_node(e, node);
                  
                  entry_fill_from_node(e);
                  
                  safe_g_hash_table_insert(d->entry_cache,
                                           (gchar*)entry_get_name(e), e);
                }

              free(attr);
            }
          else
            {
              gconf_log(GCL_WARNING,
                         _("Entry with no name in XML file `%s', ignoring"),
                         d->xml_filename);
            }
        }
      else
        {
          if (node->type == XML_ELEMENT_NODE)
            gconf_log(GCL_WARNING,
                      _("A toplevel node in XML file `%s' is <%s> rather than <entry>, ignoring"),
                      d->xml_filename,
                      node->name ? (char*) node->name : "unknown");
        }
      
      node = node->next;
    }
}

/*
 * Misc
 */

static gboolean
create_fs_dir(const gchar* dir, const gchar* xml_filename,
              guint root_dir_len, guint dir_mode, guint file_mode,
              GError** err)
{
  g_return_val_if_fail(xml_filename != NULL, FALSE);
  
  gconf_log(GCL_DEBUG, "Enter create_fs_dir: %s", dir);

  if (g_file_test(xml_filename, G_FILE_TEST_IS_REGULAR))
    {
      gconf_log(GCL_DEBUG, "XML backend file %s already exists", xml_filename);
      return TRUE;
    }
      
  /* Don't create anything above the root directory */
  if (strlen(dir) > root_dir_len)
    {
      gchar* parent;
      
      parent = _gconf_parent_dir (dir);

      gconf_log (GCL_DEBUG, "Parent dir is %s", parent);
      
      if (parent != NULL)
        {
          gchar* parent_xml = NULL;
          gboolean success = FALSE;
          
          if (xml_filename)
            parent_xml = g_strconcat(parent, "/%gconf.xml", NULL);
          
          success = create_fs_dir(parent, parent_xml, root_dir_len,
                                  dir_mode, file_mode, err);

          if (success)
            gconf_log(GCL_DEBUG, "created parent: %s", parent);
          else
            gconf_log(GCL_DEBUG, "failed parent: %s", parent);
          
          g_free(parent);
          g_free(parent_xml);
          
          if (!success)
            return FALSE;
        }
      else
        {
          gconf_log(GCL_DEBUG, "%s has no parent", dir);
        }
    }

  gconf_log(GCL_DEBUG, "Making directory %s", dir);
  
  if (g_mkdir(dir, dir_mode) < 0)
    {
      if (errno != EEXIST)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED,
                          _("Could not make directory \"%s\": %s"),
                          (gchar*)dir, g_strerror(errno));
          return FALSE;
        }
    }

  if (xml_filename != NULL)
    {
      int fd;
      /* don't truncate the file, it may well already exist */
      fd = g_open(xml_filename, O_CREAT | O_WRONLY, file_mode);

      gconf_log(GCL_DEBUG, "Creating XML file %s", xml_filename);
      
      if (fd < 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to create file `%s': %s"),
                          xml_filename, g_strerror(errno));
          
          return FALSE;
        }
      
      if (close(fd) < 0)
        {
          gconf_set_error(err, GCONF_ERROR_FAILED, _("Failed to close file `%s': %s"),
                          xml_filename, g_strerror(errno));
          
          return FALSE;
        }
    }
  else
    {
      gconf_log(GCL_DEBUG, "No XML filename passed to create_fs_dir() for %s", dir);
    }
  
  return TRUE;
}

gchar* 
_gconf_parent_dir (const gchar* dir)
{
  /* We assume the dir doesn't have a trailing slash, since that's our
     standard canonicalization in GConf */
  gchar* parent;
  gchar* last_slash;

  g_return_val_if_fail(*dir != '\0', NULL);

  if (dir[1] == '\0')
    {
      g_assert(dir[0] == '/');
      return NULL;
    }

  parent = g_strdup(dir);

  last_slash = strrchr(parent, '/');

  /* dir must have had at least the root slash in it */
  g_assert(last_slash != NULL);
  
  if (last_slash != parent)
    *last_slash = '\0';
  else 
    {
      ++last_slash;
      *last_slash = '\0';
    }

  return parent;
}

/* util */
guint
_gconf_mode_t_to_mode(mode_t orig)
{
  /* I don't think this is portable. */
  guint mode = 0;
  guint fullmask = S_IRWXG | S_IRWXU | S_IRWXO;
  

  mode = orig & fullmask;
  
  g_return_val_if_fail(mode <= 0777, 0700);

  return mode;
}

static xmlDocPtr
my_xml_parse_file (const char *filename,
                   GError    **err)
{
  char *text;
  gsize length;
  xmlDocPtr doc;
  
  text = NULL;
  length = 0;
  
  if (!g_file_get_contents (filename,
                            &text,
                            &length,
                            err))
    return NULL;


  doc = xmlParseMemory (text, length);

  g_free (text);

  if (doc == NULL)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_PARSE_ERROR,
                   _("Failed to parse XML file \"%s\""),
                   filename);
      return NULL;
    }
  
  return doc;
}


void
xml_test_dir (void)
{
#ifndef GCONF_DISABLE_TESTS
  


#endif
}
