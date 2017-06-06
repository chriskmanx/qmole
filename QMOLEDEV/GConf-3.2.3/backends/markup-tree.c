/* GConf
 * Copyright (C) 2002 Red Hat Inc.
 * Copyright (C) 2005 Mark McLoughlin
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
#include <glib.h>
#include "gconf/gconf-internals.h"
#include "gconf/gconf-schema.h"
#include "markup-tree.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#include <stdio.h>
#include <time.h>

#ifdef G_OS_WIN32
#include <io.h>
#include <conio.h>
#define _WIN32_WINNT 0x0500 
#include <windows.h>

static int
fsync (int fd)
{
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  DWORD err;

  if (h == INVALID_HANDLE_VALUE)
    {
      errno = EBADF;
      return -1;
    }

  if (!FlushFileBuffers (h))
    {
      err = GetLastError ();
      switch (err)
        {
           case ERROR_INVALID_HANDLE:
             errno = EINVAL;
             break;

           default:
             errno = EIO;
        }
      return -1;
    }

  return 0;
}
#endif

typedef struct
{
  char       *locale;
  char       *short_desc;
  char       *long_desc;
  GConfValue *default_value;
} LocalSchemaInfo;

struct _MarkupEntry
{
  MarkupDir  *dir;
  char       *name;
  GConfValue *value;
  /* list of LocalSchemaInfo */
  GSList     *local_schemas;
  char       *schema_name;
  char       *mod_user;
  GTime       mod_time;
};

static LocalSchemaInfo* local_schema_info_new  (void);
static void             local_schema_info_free (LocalSchemaInfo *info);

static MarkupDir* markup_dir_new                   (MarkupTree *tree,
						    MarkupDir  *parent,
						    const char *name);
static void       markup_dir_free                  (MarkupDir  *dir);
static gboolean   markup_dir_needs_sync            (MarkupDir  *dir);
static gboolean   markup_dir_sync                  (MarkupDir  *dir);
static char*      markup_dir_build_path            (MarkupDir  *dir,
                                                    gboolean    filesystem_path,
                                                    gboolean    with_data_file,
                                                    gboolean    subtree_data_file,
                                                    const char *locale);
static void       markup_dir_set_entries_need_save (MarkupDir  *dir);
static void       markup_dir_setup_as_subtree_root (MarkupDir  *dir);

static MarkupEntry* markup_entry_new  (MarkupDir   *dir,
				       const char  *name);
static void         markup_entry_free (MarkupEntry *entry);

static void parse_tree (MarkupDir   *root,
			gboolean     parse_subtree,
                        const char  *locale,
			GError     **err);
static void save_tree  (MarkupDir   *root,
			gboolean     save_as_subtree,
			guint        file_mode,
			GError     **err);


struct _MarkupTree
{
  char *dirname;
  guint dir_mode;
  guint file_mode;

  MarkupDir *root;

  guint refcount;

  guint merged : 1;
};

static GHashTable *trees_by_root_dir = NULL;

MarkupTree*
markup_tree_get (const char *root_dir,
                 guint       dir_mode,
                 guint       file_mode,
                 gboolean    merged)
{
  MarkupTree *tree = NULL;

  if (trees_by_root_dir == NULL)
    trees_by_root_dir = g_hash_table_new (g_str_hash, g_str_equal);
  else
    tree = g_hash_table_lookup (trees_by_root_dir, root_dir);

  if (tree != NULL)
    {
      tree->refcount += 1;
      if (merged && !tree->merged)
        tree->merged = TRUE;
      return tree;
    }

  tree = g_new0 (MarkupTree, 1);

  tree->dirname = g_strdup (root_dir);
  tree->dir_mode = dir_mode;
  tree->file_mode = file_mode;
  tree->merged = merged != FALSE;

  tree->root = markup_dir_new (tree, NULL, "/");  

  tree->refcount = 1;

  g_hash_table_insert (trees_by_root_dir, tree->dirname, tree);
  
  return tree;
}

void
markup_tree_unref (MarkupTree *tree)
{
  g_return_if_fail (tree != NULL);
  g_return_if_fail (tree->refcount > 0);

  if (tree->refcount > 1)
    {
      tree->refcount -= 1;
      return;
    }

  g_hash_table_remove (trees_by_root_dir, tree->dirname);
  if (g_hash_table_size (trees_by_root_dir) == 0)
    {
      g_hash_table_destroy (trees_by_root_dir);
      trees_by_root_dir = NULL;
    }

  markup_dir_free (tree->root);
  tree->root = NULL;

  g_free (tree->dirname);

  g_free (tree);
}

void
markup_tree_rebuild (MarkupTree *tree)
{
  g_return_if_fail (!markup_dir_needs_sync (tree->root));

  markup_dir_free (tree->root);
  tree->root = markup_dir_new (tree, NULL, "/");  
}

struct _MarkupDir
{
  MarkupTree *tree;
  MarkupDir *parent;
  MarkupDir *subtree_root;
  char *name;

  GSList *entries;
  GSList *subdirs;

  /* Available %gconf-tree-$(locale).xml files */
  GHashTable *available_local_descs;

  /* Have read the existing XML file */
  guint entries_loaded : 1;
  /* Need to rewrite the XML file since we changed
   * it
   */
  guint entries_need_save : 1;

  /* Have read the existing directories */
  guint subdirs_loaded : 1;

  /* Child needs sync */
  guint some_subdir_needs_sync : 1;

  /* We are pretty sure the filesystem dir exists */
  guint filesystem_dir_probably_exists : 1;

  /* Not represented by an actual filesystem dir */
  guint not_in_filesystem : 1;

  /* Save to %gconf-tree.xml when syncing */
  guint save_as_subtree : 1;

  /* We've loaded all locales in @available_local_descs */
  guint all_local_descs_loaded : 1;

  /* This is a temporary directory used only during parsing */
  guint is_parser_dummy : 1;

  /* Temporary flag used only when writing */
  guint is_dir_empty : 1;
};

static MarkupDir*
markup_dir_new (MarkupTree *tree,
                MarkupDir  *parent,
                const char *name)
{
  MarkupDir *dir;

  dir = g_new0 (MarkupDir, 1);

  dir->name = g_strdup (name);
  dir->tree = tree;
  dir->parent = parent;

  if (parent)
    {
      dir->subtree_root = parent->subtree_root;
      parent->subdirs = g_slist_prepend (parent->subdirs, dir);
    }
  else
    {
      markup_dir_setup_as_subtree_root (dir);
    }

  return dir;
}

static void
markup_dir_free (MarkupDir *dir)
{
  GSList *tmp;

  if (dir->available_local_descs != NULL)
    {
      g_hash_table_destroy (dir->available_local_descs);
      dir->available_local_descs = NULL;
    }

  tmp = dir->entries;
  while (tmp)
    {
      MarkupEntry *entry = tmp->data;

      markup_entry_free (entry);

      tmp = tmp->next;
    }
  g_slist_free (dir->entries);

  tmp = dir->subdirs;
  while (tmp)
    {
      MarkupDir *subdir = tmp->data;

      markup_dir_free (subdir);

      tmp = tmp->next;
    }
  g_slist_free (dir->subdirs);

  g_free (dir->name);

  g_free (dir);
}

static void
markup_dir_queue_sync (MarkupDir *dir)
{
  MarkupDir *iter;

  iter = dir->parent;
  while (iter != NULL) /* exclude root dir */
    {
      iter->some_subdir_needs_sync = TRUE;
      iter = iter->parent;
    }
}

static inline char *
markup_dir_build_file_path (MarkupDir  *dir,
                            gboolean    subtree_data_file,
                            const char *locale)
{
  return markup_dir_build_path (dir, TRUE, TRUE, subtree_data_file, locale);
}

static inline char *
markup_dir_build_dir_path (MarkupDir *dir,
                           gboolean   filesystem_path)
{
  return markup_dir_build_path (dir, filesystem_path, FALSE, FALSE, NULL);
}

static MarkupDir*
markup_tree_get_dir_internal (MarkupTree *tree,
                              const char *full_key,
                              gboolean    create_if_not_found,
                              GError    **err)
{
  char **components;
  int i;
  MarkupDir *dir;
  
  g_return_val_if_fail (*full_key == '/', NULL);

  /* Split without leading '/' */
  components = g_strsplit (full_key + 1, "/", -1);

  dir = tree->root;

  if (components) /* if components == NULL the root dir was requested */
    {
      i = 0;
      while (components[i])
        {
          MarkupDir *subdir;
          GError *tmp_err;

          tmp_err = NULL;

          if (create_if_not_found)
            subdir = markup_dir_ensure_subdir (dir, components[i], &tmp_err);
          else
            subdir = markup_dir_lookup_subdir (dir, components[i], &tmp_err);

          if (tmp_err != NULL)
            {
              dir = NULL;
              g_propagate_error (err, tmp_err);
              goto out;
            }

          if (subdir)
            {
              /* Descend one level */
              dir = subdir;
            }
          else
            {
              dir = NULL;
              goto out;
            }
          
          ++i;
        }
    }

 out:
  g_strfreev (components);

  return dir;
}

MarkupDir*
markup_tree_lookup_dir (MarkupTree *tree,
                        const char *full_key,
                        GError    **err)
     
{
  return markup_tree_get_dir_internal (tree, full_key, FALSE, err);
}

MarkupDir*
markup_tree_ensure_dir (MarkupTree *tree,
                        const char *full_key,
                        GError    **err)
{
  return markup_tree_get_dir_internal (tree, full_key, TRUE, err);  
}

gboolean
markup_tree_sync (MarkupTree *tree,
                  GError    **err)
{
  if (markup_dir_needs_sync (tree->root))
    {
      if (!markup_dir_sync (tree->root))
        {
          g_set_error (err, GCONF_ERROR,
                       GCONF_ERROR_FAILED,
                       _("Failed to write some configuration data to disk\n"));
          return FALSE;          
        }
    }

  return TRUE;
}

static void
markup_dir_setup_as_subtree_root (MarkupDir *dir)
{
  if (dir->subtree_root != dir)
    {
      dir->subtree_root = dir;

      dir->available_local_descs = g_hash_table_new_full (g_str_hash,
                                                          g_str_equal,
                                                          g_free,
                                                          NULL);
      dir->all_local_descs_loaded = TRUE;
    }
}

static void
markup_dir_list_available_local_descs (MarkupDir *dir)
{
#define LOCALE_FILE_PREFIX "%gconf-tree-"
#define LOCALE_FILE_SUFFIX ".xml"
#define LOCALE_FILE_PREFIX_LEN (sizeof (LOCALE_FILE_PREFIX) - 1)
#define LOCALE_FILE_SUFFIX_LEN (sizeof (LOCALE_FILE_SUFFIX) - 1)

  GDir       *dp;
  char       *dir_path;
  const char *dent;

  dir_path = markup_dir_build_dir_path (dir, TRUE);

  if ((dp = g_dir_open (dir_path, 0, NULL)) == NULL)
    {
      /* This is debug-only since it usually happens when creating a
       * new directory
       */
      gconf_log (GCL_DEBUG,
                 "Could not open directory \"%s\": %s\n",
                 dir_path, g_strerror (errno));
      g_free (dir_path);
      return;
    }

  g_assert (dir->available_local_descs != NULL);
  g_assert (g_hash_table_size (dir->available_local_descs) == 0);

  while ((dent = g_dir_read_name (dp)) != NULL)
    {
      gsize  dent_len;
      char  *locale;

      dent_len = strlen (dent);

      if (dent_len <= LOCALE_FILE_PREFIX_LEN + LOCALE_FILE_SUFFIX_LEN)
        continue;

      if (strncmp (dent, LOCALE_FILE_PREFIX, LOCALE_FILE_PREFIX_LEN) != 0)
        continue;

      if (strcmp (dent + dent_len - LOCALE_FILE_SUFFIX_LEN, LOCALE_FILE_SUFFIX) != 0)
        continue;

      locale = g_strndup (dent + LOCALE_FILE_PREFIX_LEN,
                          dent_len - LOCALE_FILE_PREFIX_LEN - LOCALE_FILE_SUFFIX_LEN);

      g_hash_table_replace (dir->available_local_descs,
                            locale,
                            GINT_TO_POINTER (FALSE));
    }

  if (g_hash_table_size (dir->available_local_descs) != 0)
    dir->all_local_descs_loaded = FALSE;

  /* if this fails, we really can't do a thing about it
   * and it's not a meaningful error
   */
  g_dir_close (dp);

  g_free (dir_path);

#undef LOCALE_FILE_SUFFIX_LEN
#undef LOCALE_FILE_PREFIX_LEN
#undef LOCALE_FILE_SUFFIX
#undef LOCALE_FILE_PREFIX
}

static gboolean
load_subtree (MarkupDir *dir)
{
  /* Load subtree from %gconf-tree.xml if exists */

  GError *tmp_err = NULL;
  char *markup_file;

  markup_file = markup_dir_build_file_path (dir, TRUE, NULL);
  if (!g_file_test (markup_file, G_FILE_TEST_EXISTS))
    {
      g_free (markup_file);
      return FALSE;
    }

  dir->subdirs_loaded  = TRUE;
  dir->entries_loaded  = TRUE;
  dir->save_as_subtree = TRUE;

  markup_dir_setup_as_subtree_root (dir);
  markup_dir_list_available_local_descs (dir);

  parse_tree (dir, TRUE, NULL, &tmp_err);
  if (tmp_err)
    {
      /* note that tmp_err may be a G_MARKUP_ERROR while only
       * GCONF_ERROR could be returned, so if we decide to return this
       * error someday we need to fix it up first
       */

      /* this message is debug-only because it usually happens
       * when creating a new directory
       */
      gconf_log (GCL_DEBUG,
		 "Failed to load file \"%s\": %s",
		 markup_file, tmp_err->message);
      g_error_free (tmp_err);
    }

  g_free (markup_file);

  return TRUE;
}

static gboolean
load_entries (MarkupDir *dir)
{
  /* Load the entries in this directory */
  
  if (dir->entries_loaded)
    return TRUE;

  /* We mark it loaded even if the next stuff
   * fails, because we don't want to keep trying and
   * failing, plus we have invariants
   * that assume entries_loaded is TRUE once we've
   * called load_entries()
   */
  dir->entries_loaded = TRUE;

  if (!load_subtree (dir))
    {
      GError *tmp_err = NULL;

      parse_tree (dir, FALSE, NULL, &tmp_err);
      if (tmp_err)
	{
	  char *markup_file;

	  /* note that tmp_err may be a G_MARKUP_ERROR while only
	   * GCONF_ERROR could be returned, so if we decide to return this
	   * error someday we need to fix it up first
	   */

	  /* this message is debug-only because it usually happens
	   * when creating a new directory
	   */
	  markup_file = markup_dir_build_file_path (dir, FALSE, NULL);
	  gconf_log (GCL_DEBUG,
		     "Failed to load file \"%s\": %s",
		     markup_file, tmp_err->message);
	  g_error_free (tmp_err);
	  g_free (markup_file);
	}
    }

  return TRUE;
}

static gboolean
load_subdirs (MarkupDir *dir)
{  
  GDir* dp;
  const char* dent;
  struct stat statbuf;
  gchar* fullpath;
  gchar* fullpath_end;
  guint len;
  guint subdir_len;
  char *markup_dir;
  
  if (dir->subdirs_loaded)
    return TRUE;
  
  /* We mark it loaded even if the next stuff
   * fails, because we don't want to keep trying and
   * failing, plus we have invariants
   * that assume subdirs_loaded is TRUE once we've
   * called load_subdirs()
   */
  dir->subdirs_loaded = TRUE;

  g_assert (dir->subdirs == NULL);

  if (load_subtree (dir))
    return TRUE;

  markup_dir = markup_dir_build_dir_path (dir, TRUE);
  
  dp = g_dir_open (markup_dir, 0, NULL);
  
  if (dp == NULL)
    {
      /* This is debug-only since it usually happens when creating a
       * new directory
       */
      gconf_log (GCL_DEBUG,
                 "Could not open directory \"%s\": %s\n",
                 markup_dir, g_strerror (errno));
      g_free (markup_dir);
      return FALSE;
    }

  len = strlen (markup_dir);
  
  subdir_len = PATH_MAX - len;
  
  fullpath = g_new0 (char, subdir_len + len + 2); /* ensure null termination */
  strcpy (fullpath, markup_dir);
  
  fullpath_end = fullpath + len;
  if (*(fullpath_end - 1) != '/')
    {
      *fullpath_end = '/';
      ++fullpath_end;
    }

  while ((dent = g_dir_read_name (dp)) != NULL)
    {
      /* ignore all dot-files */
      if (dent[0] == '.')
        continue;

      /* ignore stuff starting with % as it's an invalid gconf
       * dir name, and probably %gconf.xml
       */
      if (dent[0] == '%')
        continue;
      
      len = strlen (dent);
      
      if (len < subdir_len)
        {
          strcpy (fullpath_end, dent);
          strncpy (fullpath_end+len, "/%gconf.xml", subdir_len - len);
        }
      else
        continue; /* Shouldn't ever happen since PATH_MAX is available */

      if (g_stat (fullpath, &statbuf) < 0)
        {
          strncpy (fullpath_end+len, "/%gconf-tree.xml", subdir_len - len);
          if (g_stat (fullpath, &statbuf) < 0)
            {
              /* This is some kind of cruft, not an XML directory */
              continue;
            }
        }      

      markup_dir_new (dir->tree, dir, dent);
    }

  /* if this fails, we really can't do a thing about it
   * and it's not a meaningful error
   */
  g_dir_close (dp);

  g_free (fullpath);
  g_free (markup_dir);

  return TRUE;
}

MarkupEntry*
markup_dir_lookup_entry (MarkupDir   *dir,
                         const char  *relative_key,
                         GError     **err)
{
  GSList *tmp;

  load_entries (dir);
  
  tmp = dir->entries;
  while (tmp != NULL)
    {
      MarkupEntry *entry = tmp->data;

      if (strcmp (relative_key, entry->name) == 0)
        return entry;
      
      tmp = tmp->next;
    }

  return NULL;
}

MarkupEntry*
markup_dir_ensure_entry (MarkupDir   *dir,
                         const char  *relative_key,
                         GError     **err)
{
  MarkupEntry *entry;
  GError *tmp_err;
  
  tmp_err = NULL;
  entry = markup_dir_lookup_entry (dir, relative_key, &tmp_err);
  if (tmp_err != NULL)
    {
      g_propagate_error (err, tmp_err);
      return NULL;
    }
  
  if (entry != NULL)
    return entry;

  g_return_val_if_fail (dir->entries_loaded, NULL);
  
  /* Create a new entry */
  entry = markup_entry_new (dir, relative_key);

  /* Need to save this */
  markup_dir_set_entries_need_save (dir);
  markup_dir_queue_sync (dir);
  
  return entry;
}

MarkupDir*
markup_dir_lookup_subdir (MarkupDir   *dir,
                          const char  *relative_key,
                          GError     **err)
{
  GSList *tmp;
  
  load_subdirs (dir);

  tmp = dir->subdirs;
  while (tmp != NULL)
    {
      MarkupDir *subdir = tmp->data;

      if (strcmp (subdir->name, relative_key) == 0)
        return subdir;

      tmp = tmp->next;
    }

  return NULL;
}

MarkupDir*
markup_dir_ensure_subdir (MarkupDir   *dir,
                          const char  *relative_key,
                          GError     **err)
{
  MarkupDir *subdir;
  GError *tmp_err;
  
  tmp_err = NULL;
  subdir = markup_dir_lookup_subdir (dir, relative_key, &tmp_err);
  if (tmp_err != NULL)
    {
      g_propagate_error (err, tmp_err);
      return NULL;
    }

  if (subdir != NULL)
    return subdir;

  g_return_val_if_fail (dir->subdirs_loaded, NULL);
      
  subdir = markup_dir_new (dir->tree, dir, relative_key);
  markup_dir_set_entries_need_save (subdir); /* so we save empty %gconf.xml */
      
  /* we don't need to load stuff, since we know the dir didn't exist */
  subdir->entries_loaded = TRUE;
  subdir->subdirs_loaded = TRUE;
      
  return subdir;
}

GSList*
markup_dir_list_entries (MarkupDir   *dir,
                         GError     **err)
{
  load_entries (dir);

  return dir->entries;
}

GSList*
markup_dir_list_subdirs (MarkupDir   *dir,
                         GError     **err)
{
  load_subdirs (dir);

  return dir->subdirs;
}

const char*
markup_dir_get_name (MarkupDir   *dir)
{
  return dir->name;
}

static gboolean
markup_dir_needs_sync (MarkupDir *dir)
{
  return dir->entries_need_save || dir->some_subdir_needs_sync;
}

static void
markup_dir_set_entries_need_save (MarkupDir *dir)
{
  dir->entries_need_save = TRUE;
  
  if (dir->not_in_filesystem)
    {
      /* root must be a filesystem dir */
      g_assert (dir->parent);

      markup_dir_set_entries_need_save (dir->parent);
    }
}

/* Get rid of any local_schema that no longer apply */
static void
clean_old_local_schemas (MarkupEntry *entry)
{
  GSList *tmp;
  GSList *kept_schemas;

  kept_schemas = NULL;
  
  tmp = entry->local_schemas;
  while (tmp != NULL)
    {
      LocalSchemaInfo *local_schema = tmp->data;
      gboolean dead = FALSE;
          
      local_schema = tmp->data;

      if (entry->value &&
          entry->value->type != GCONF_VALUE_SCHEMA)
        dead = TRUE;
      else if (local_schema->default_value &&
               entry->value &&
               gconf_value_get_schema (entry->value) &&
               gconf_schema_get_type (gconf_value_get_schema (entry->value)) !=
               local_schema->default_value->type)
        {
          dead = TRUE;
        }
          
      if (dead)
        {
          local_schema_info_free (local_schema);
        }
      else
        {
          kept_schemas = g_slist_prepend (kept_schemas, local_schema);
        }

      tmp = tmp->next;
    }

  g_slist_free (entry->local_schemas);
  
  entry->local_schemas = g_slist_reverse (kept_schemas);
}

static void
clean_old_local_schemas_recurse (MarkupDir *dir,
				 gboolean   recurse)
{
  GSList *tmp;

  if (recurse)
    {
      tmp = dir->subdirs;
      while (tmp)
        {
          MarkupDir *subdir = tmp->data;

          clean_old_local_schemas_recurse (subdir, TRUE);

          tmp = tmp->next;
        }
    }

  tmp = dir->entries;
  while (tmp != NULL)
    {
      MarkupEntry *entry = tmp->data;

      clean_old_local_schemas (entry);

      tmp = tmp->next;
    }
}

static gboolean
create_filesystem_dir (const char *name,
                       guint       dir_mode)
{
  if (g_mkdir (name, dir_mode) < 0)
    {
      if (errno == EEXIST)
        return TRUE;

      gconf_log (GCL_WARNING,
                 _("Could not make directory \"%s\": %s"),
                 name, g_strerror (errno));

      return FALSE;
    }

  return TRUE;
}

static gboolean
delete_useless_subdirs (MarkupDir *dir)
{
  GSList *tmp;
  GSList *kept_subdirs;
  gboolean some_deleted;

  some_deleted = FALSE;
  kept_subdirs = NULL;
  
  tmp = dir->subdirs;
  while (tmp != NULL)
    {
      MarkupDir *subdir = tmp->data;
      
      if (subdir->entries_loaded && subdir->entries == NULL &&
          subdir->subdirs_loaded && subdir->subdirs == NULL)
        {
	  if (!subdir->not_in_filesystem)
	    {
	      char *fs_dirname;
	      char *fs_filename;
          
	      fs_dirname = markup_dir_build_dir_path (subdir, TRUE);
	      fs_filename = markup_dir_build_file_path (subdir,
							subdir->save_as_subtree,
							NULL);

	      if (g_unlink (fs_filename) < 0)
		{
		  gconf_log (GCL_WARNING,
			     _("Could not remove \"%s\": %s\n"),
			     fs_filename, g_strerror (errno));
		}

	      if (g_rmdir (fs_dirname) < 0)
		{
		  gconf_log (GCL_WARNING,
			     _("Could not remove \"%s\": %s\n"),
			     fs_dirname, g_strerror (errno));
		}
          
	      g_free (fs_dirname);
	      g_free (fs_filename);
	    }

          markup_dir_free (subdir);

          some_deleted = TRUE;
        }
      else
        {
          kept_subdirs = g_slist_prepend (kept_subdirs, subdir);
        }
      
      tmp = tmp->next;
    }
 
  g_slist_free (dir->subdirs);
  dir->subdirs = g_slist_reverse (kept_subdirs);

  return some_deleted;
}

static gboolean
delete_useless_subdirs_recurse (MarkupDir *dir)
{
  GSList *tmp;
  gboolean retval = FALSE;

  tmp = dir->subdirs;
  while (tmp)
    {
      MarkupDir *subdir = tmp->data;

      if (delete_useless_subdirs_recurse (subdir))
	retval = TRUE;

      tmp = tmp->next;
    }

  if (delete_useless_subdirs (dir))
    retval = TRUE;

  return retval;
}

static gboolean
delete_useless_entries (MarkupDir *dir)
{
  GSList *tmp;
  GSList *kept_entries;
  gboolean some_deleted;

  some_deleted = FALSE;
  
  kept_entries = NULL;

  tmp = dir->entries;
  while (tmp != NULL)
    {
      MarkupEntry *entry = tmp->data;

      /* mod_user and mod_time don't keep an entry alive */
      
      if (entry->value == NULL &&
          entry->local_schemas == NULL &&
          entry->schema_name == NULL)
        {
          markup_entry_free (entry);
          some_deleted = TRUE;
        }
      else
        {
          kept_entries = g_slist_prepend (kept_entries, entry);
        }

      tmp = tmp->next;
    }

  g_slist_free (dir->entries);
  dir->entries = g_slist_reverse (kept_entries);

  return some_deleted;
}

static gboolean
delete_useless_entries_recurse (MarkupDir *dir)
{
  GSList *tmp;
  gboolean retval = FALSE;

  tmp = dir->subdirs;
  while (tmp)
    {
      MarkupDir *subdir = tmp->data;

      if (delete_useless_entries_recurse (subdir))
        retval = TRUE;

      tmp = tmp->next;
    }

  if (delete_useless_entries (dir))
    retval = TRUE;

  return retval;
}

static void
recursively_load_subtree (MarkupDir *dir)
{
  GSList *tmp;

  load_entries (dir);
  load_subdirs (dir);

  tmp = dir->subdirs;
  while (tmp != NULL)
    {
      MarkupDir *subdir = tmp->data;

      recursively_load_subtree (subdir);
      subdir->not_in_filesystem = TRUE;

      tmp = tmp->next;
    }
}

static gboolean
markup_dir_sync (MarkupDir *dir)
{
  char *fs_dirname;
  char *fs_filename;
  char *fs_subtree;
  gboolean some_useless_entries;
  gboolean some_useless_subdirs;

  some_useless_entries = FALSE;
  some_useless_subdirs = FALSE;

  /* We assume our parent directories have all been synced, before
   * we are synced. So we don't need to mkdir() parent directories.
   */

  /* We must have been synced already */
  if (dir->not_in_filesystem)
    return TRUE;

  /* Sanitize the entries */
  clean_old_local_schemas_recurse (dir, dir->save_as_subtree);

  if (!dir->save_as_subtree && dir->tree->merged)
    {
      dir->save_as_subtree = TRUE;
      recursively_load_subtree (dir);
    }
  
  fs_dirname = markup_dir_build_dir_path (dir, TRUE);
  fs_filename = markup_dir_build_file_path (dir, FALSE, NULL);
  fs_subtree = markup_dir_build_file_path (dir, TRUE, NULL);

  /* For a dir to be loaded as a subdir, it must have a
   * %gconf.xml file, even if it has no entries in that
   * file.  Thus when creating a new dir, we set dir->entries_need_save
   * even though the entry list is initially empty.
   */
  
  if (dir->entries_need_save ||
      (dir->some_subdir_needs_sync && dir->save_as_subtree))
    {
      GError *err;
      
      g_return_val_if_fail (dir->entries_loaded, FALSE);

      if (!dir->save_as_subtree)
	{
	  if (delete_useless_entries (dir))
	    some_useless_entries = TRUE;
	}
      else
	{
	  if (delete_useless_entries_recurse (dir))
	    some_useless_entries = TRUE;
	}
      
      /* Be sure the directory exists */
      if (!dir->filesystem_dir_probably_exists)
        {
          if (create_filesystem_dir (fs_dirname, dir->tree->dir_mode))
            dir->filesystem_dir_probably_exists = TRUE;
        }
      
      /* Now write the file */
      err = NULL;
      save_tree (dir, dir->save_as_subtree, dir->tree->file_mode, &err);
      if (err != NULL)
        {
          gconf_log (GCL_WARNING,
                     _("Failed to write \"%s\": %s\n"),
                     !dir->save_as_subtree ? fs_filename : fs_subtree, err->message);
          
          g_error_free (err);
        }
      else
        {
          dir->entries_need_save = FALSE;
	  if (dir->save_as_subtree)
	    dir->some_subdir_needs_sync = FALSE;
        }
    }

  if (dir->some_subdir_needs_sync && !dir->save_as_subtree)
    {
      GSList *tmp;
      gboolean one_failed;

      g_return_val_if_fail (dir->subdirs_loaded, FALSE);
      
      one_failed = FALSE;
      
      tmp = dir->subdirs;
      while (tmp != NULL)
        {
          MarkupDir *subdir = tmp->data;

          if (markup_dir_needs_sync (subdir))
            {
              /* Be sure the directory exists (may not have
               * had to save entries, if not we won't have done
               * this there)
               */
              if (!dir->filesystem_dir_probably_exists)
                {
                  if (create_filesystem_dir (fs_dirname, dir->tree->dir_mode))
                    dir->filesystem_dir_probably_exists = TRUE;
                }
              
              if (!markup_dir_sync (subdir))
                one_failed = TRUE;
            }

          tmp = tmp->next;
        }

      if (!one_failed)
        dir->some_subdir_needs_sync = FALSE;
    }

  /* Now if we've synced everything and some subdirs have no entries
   * and no subdirs, they have become useless - so we can delete
   * them. Note that this happens _after_ recursing
   * subdirectories, so the deletion happens first on
   * the leaves, and then on the root. Also note that since
   * we're deleting our subdirs, the root dir (tree->root)
   * never gets deleted - this is intentional.
   */

  if (!dir->save_as_subtree)
    {
      if (delete_useless_subdirs (dir))
	some_useless_subdirs = TRUE;
    }
  else
    {
      /* We haven't recursively synced subdirs so we need
       * to now recursively delete useless subdirs.
       */
      if (delete_useless_subdirs_recurse (dir))
	some_useless_subdirs = TRUE;
    }
  
  g_free (fs_dirname);
  g_free (fs_filename);
  g_free (fs_subtree);

  /* If we deleted an entry or subdir from this directory, and hadn't
   * fully loaded this directory, we now don't know whether the entry
   * or subdir was the last thing making the directory worth keeping
   * around. So we need to load so we can be established as useless if
   * necessary.
   */
  if (some_useless_entries && !dir->subdirs_loaded)
    {
      g_assert (dir->entries_loaded);
      load_subdirs (dir);
    }
  if (some_useless_subdirs && !dir->entries_loaded)
    {
      g_assert (dir->subdirs_loaded);
      load_entries (dir);
    }

  return !markup_dir_needs_sync (dir);
}

static char*
markup_dir_build_path (MarkupDir  *dir,
                       gboolean    filesystem_path,
                       gboolean    with_data_file,
                       gboolean    subtree_data_file,
                       const char *locale)
{
  GString *name;
  GSList *components;
  GSList *tmp;
  MarkupDir *iter;

  g_assert (filesystem_path || !with_data_file);

  components = NULL;
  iter = dir;
  while (iter->parent != NULL) /* exclude root dir */
    {
      components = g_slist_prepend (components, iter->name);
      iter = iter->parent;
    }

  if (filesystem_path)
    name = g_string_new (dir->tree->dirname);
  else
    name = g_string_new (components ? NULL : "/");

  tmp = components;
  while (tmp != NULL)
    {
      const char *comp = tmp->data;

      g_string_append_c (name, '/');
      g_string_append (name, comp);

      tmp = tmp->next;
    }

  g_slist_free (components);

  if (with_data_file)
    {
      if (locale == NULL)
        {
          g_string_append (name,
                           subtree_data_file ? "/%gconf-tree.xml" : "/%gconf.xml");
        }
       else
        {
          g_assert (subtree_data_file);

          g_string_append_printf (name, "/%%gconf-tree-%s.xml", locale);
        }
    }

  return g_string_free (name, FALSE);
}

/*
 * MarkupEntry
 */

static MarkupEntry*
markup_entry_new (MarkupDir  *dir,
                  const char *name)
{
  MarkupEntry *entry;

  entry = g_new0 (MarkupEntry, 1);

  entry->name = g_strdup (name);

  entry->dir = dir;
  dir->entries = g_slist_prepend (dir->entries, entry);

  return entry;
}

static void
markup_entry_free (MarkupEntry *entry)
{
  g_free (entry->name);
  if (entry->value)
    gconf_value_free (entry->value);
  g_free (entry->schema_name);
  g_free (entry->mod_user);

  g_slist_foreach (entry->local_schemas,
                   (GFunc) local_schema_info_free,
                   NULL);

  g_slist_free (entry->local_schemas);

  g_free (entry);
}

static void
load_schema_descs_for_locale (MarkupDir  *dir,
                              const char *locale)
{
  GError *error;

  error = NULL;
  parse_tree (dir, TRUE, locale, &error);
  if (error != NULL)
    {
      char *markup_file;

      markup_file = markup_dir_build_file_path (dir, TRUE, locale);

      gconf_log (GCL_ERR,
                 _("Failed to load file \"%s\": %s"),
                 markup_file,
                 error->message);

      g_free (markup_file);
      g_error_free (error);
    }

  g_hash_table_replace (dir->available_local_descs,
                        g_strdup (locale),
                        GINT_TO_POINTER (TRUE));
}

static void
load_schema_descs_foreach (const char *locale,
                           gpointer    value,
                           MarkupDir  *dir)
{
  if (value != NULL)
    return; /* already loaded */

  load_schema_descs_for_locale (dir, locale);
}

static gboolean
find_unloaded_locale (const char *locale,
                      gpointer    value,
                      gboolean   *any_unloaded)
{
  if (value != NULL)
    return FALSE;

  *any_unloaded = TRUE;
  
  return TRUE;
}

static void
ensure_schema_descs_loaded (MarkupEntry *entry,
                            const char  *locale)
{
  MarkupDir *subtree_root;

  subtree_root = entry->dir->subtree_root;

  if (subtree_root->all_local_descs_loaded)
    return;

  if (locale == NULL)
    {
      g_hash_table_foreach (subtree_root->available_local_descs,
                            (GHFunc) load_schema_descs_foreach,
                            subtree_root);

      subtree_root->all_local_descs_loaded = TRUE;

      return;
    }
  else
    {
      gpointer value;
      gboolean any_unloaded;

      value = NULL;
      if (!g_hash_table_lookup_extended (subtree_root->available_local_descs,
                                         locale,
                                         NULL,
                                         &value))
        return; /* locale isn't available */

      if (value != NULL)
        return; /* already loaded */

      load_schema_descs_for_locale (subtree_root, locale);

      any_unloaded = FALSE;
      g_hash_table_find (subtree_root->available_local_descs,
                         (GHRFunc) find_unloaded_locale,
                         &any_unloaded);

      if (!any_unloaded)
        subtree_root->all_local_descs_loaded = TRUE;
    }
}

void
markup_entry_set_value (MarkupEntry       *entry,
                        const GConfValue  *value)
{
  /* We have to have loaded entries, because
   * someone called ensure_entry to get this
   * entry.
   */
  g_return_if_fail (entry->dir != NULL);
  g_return_if_fail (entry->dir->entries_loaded);
  g_return_if_fail (value != NULL);
  
  if (value->type != GCONF_VALUE_SCHEMA)
    {
      if (entry->value == value)
        return;

      if (entry->value)
        gconf_value_free (entry->value);

      entry->value = gconf_value_copy (value);

      /* Dump these if they exist, we aren't a schema anymore */
      if (entry->local_schemas)
        {
          g_slist_foreach (entry->local_schemas,
                           (GFunc) local_schema_info_free,
                           NULL);
          g_slist_free (entry->local_schemas);
          entry->local_schemas = NULL;
        }
    }
  else
    {
      /* For schema entries, we put the localized info
       * in a LocalSchemaInfo, and the other info
       * in the schema in the GConfValue
       */
      GSList *tmp;
      LocalSchemaInfo *local_schema;
      GConfSchema *schema;
      const char *locale;
      GConfSchema *current_schema;
      GConfValue *def_value;

      schema = gconf_value_get_schema (value);
      g_assert (schema);

      locale = gconf_schema_get_locale (schema);
      if (locale == NULL)
        locale = "C";

      ensure_schema_descs_loaded (entry, locale);

      local_schema = NULL;
      tmp = entry->local_schemas;
      while (tmp != NULL)
        {
          LocalSchemaInfo *lsi;

          lsi = tmp->data;

          if (strcmp (lsi->locale, locale) == 0)
            {
              local_schema = lsi;
              break;
            }

          tmp = tmp->next;
        }

      if (local_schema == NULL)
        {
          /* Didn't find a value for locale, make a new entry in the list */
          local_schema = local_schema_info_new ();
          local_schema->locale = g_strdup (locale);
          entry->local_schemas =
            g_slist_prepend (entry->local_schemas, local_schema);
        }

      g_free (local_schema->short_desc);
      g_free (local_schema->long_desc);
      if (local_schema->default_value)
        gconf_value_free (local_schema->default_value);

      local_schema->short_desc = g_strdup (gconf_schema_get_short_desc (schema));
      local_schema->long_desc = g_strdup (gconf_schema_get_long_desc (schema));
      def_value = gconf_schema_get_default_value (schema);
      if (def_value)
        local_schema->default_value = gconf_value_copy (def_value);
      else
        local_schema->default_value = NULL;

      /* When saving, we will check that the type of default_value is
       * consistent with the type in the entry->value schema, so that
       * we don't save something we can't load. We'll drop any
       * LocalSchemaInfo with the wrong type default value at that
       * time, more efficient than dropping it now.
       */
      if (entry->value && entry->value->type != GCONF_VALUE_SCHEMA)
        {
          gconf_value_free (entry->value);
          entry->value = NULL;
        }

      if (entry->value == NULL)
        {
          entry->value = gconf_value_new (GCONF_VALUE_SCHEMA);
          current_schema = gconf_schema_new ();
          gconf_value_set_schema_nocopy (entry->value, current_schema);
        }
      else
        {
          current_schema = gconf_value_get_schema (entry->value);
        }

      /* Don't save localized info in the main schema */
      gconf_schema_set_locale (current_schema, NULL);
      gconf_schema_set_short_desc (current_schema, NULL);
      gconf_schema_set_long_desc (current_schema, NULL);

      /* But everything else goes in the main schema */
      gconf_schema_set_list_type (current_schema,
                                  gconf_schema_get_list_type (schema));
      gconf_schema_set_car_type (current_schema,
                                 gconf_schema_get_car_type (schema));
      gconf_schema_set_cdr_type (current_schema,
                                 gconf_schema_get_cdr_type (schema));
      gconf_schema_set_type (current_schema,
                             gconf_schema_get_type (schema));
      gconf_schema_set_owner (current_schema,
                              gconf_schema_get_owner (schema));
    }

  /* Update mod time */
  entry->mod_time = time (NULL);

  /* Need to save to disk */
  markup_dir_set_entries_need_save (entry->dir);
  markup_dir_queue_sync (entry->dir);
}

void
markup_entry_unset_value (MarkupEntry *entry,
                          const char  *locale)
{
  /* We have to have loaded entries, because
   * someone called ensure_entry to get this
   * entry.
   */
  g_return_if_fail (entry->dir != NULL);
  g_return_if_fail (entry->dir->entries_loaded);

  if (entry->value == NULL)
    {
      /* nothing to do */
      return;
    }
  else if (entry->value->type == GCONF_VALUE_SCHEMA)
    {
      if (locale == NULL)
        {
          /* blow it all away */
          gconf_value_free (entry->value);
          entry->value = NULL;

          ensure_schema_descs_loaded (entry, NULL);

          g_slist_foreach (entry->local_schemas,
                           (GFunc) local_schema_info_free,
                           NULL);
          
          g_slist_free (entry->local_schemas);
          
          entry->local_schemas = NULL;
        }
      else
        {
          /* Just blow away any matching local schema */
          GSList *tmp;

          ensure_schema_descs_loaded (entry, locale);

          tmp = entry->local_schemas;
          while (tmp != NULL)
            {
              LocalSchemaInfo *local_schema = tmp->data;

              if (strcmp (local_schema->locale, locale) == 0)
                {
                  entry->local_schemas =
                    g_slist_remove (entry->local_schemas,
                                    local_schema);

                  local_schema_info_free (local_schema);
                  break;
                }

              tmp = tmp->next;
            }
        }
    }
  else
    {
      gconf_value_free (entry->value);
      entry->value = NULL;
    }

  /* Update mod time */
  entry->mod_time = time (NULL);

  /* Need to save to disk */
  markup_dir_set_entries_need_save (entry->dir);
  markup_dir_queue_sync (entry->dir);
}

void
markup_entry_set_schema_name (MarkupEntry *entry,
                              const char  *schema_name)
{
  /* We have to have loaded entries, because
   * someone called ensure_entry to get this
   * entry.
   */
  g_return_if_fail (entry->dir != NULL);
  g_return_if_fail (entry->dir->entries_loaded);

  /* schema_name may be NULL to unset it */
  
  g_free (entry->schema_name);
  entry->schema_name = g_strdup (schema_name);
  
  /* Update mod time */
  entry->mod_time = time (NULL);

  /* Need to save to disk */
  markup_dir_set_entries_need_save (entry->dir);
  markup_dir_queue_sync (entry->dir);
}

GConfValue*
markup_entry_get_value (MarkupEntry *entry,
                        const char **locales)
{
  /* We have to have loaded entries, because
   * someone called ensure_entry to get this
   * entry.
   */
  g_return_val_if_fail (entry->dir != NULL, NULL);
  g_return_val_if_fail (entry->dir->entries_loaded, NULL);

  if (entry->value == NULL)
    {
      return NULL;
    }
  else if (entry->value->type != GCONF_VALUE_SCHEMA)
    {
      return gconf_value_copy (entry->value);
    }
  else
    {
      GConfValue *retval;
      GConfSchema *schema;
      static const char *fallback_locales[2] = {
        "C", NULL
      };
      LocalSchemaInfo *best;
      LocalSchemaInfo *c_local_schema;
      int i;

      retval = gconf_value_copy (entry->value);
      schema = gconf_value_get_schema (retval);
      g_return_val_if_fail (schema != NULL, NULL);

      /* Find the best local schema */

      if (locales == NULL || locales[0] == NULL)
        locales = fallback_locales;

      best = NULL;
      c_local_schema = NULL;

      i = 0;
      while (locales[i] != NULL)
        {
          GSList *tmp;

          ensure_schema_descs_loaded (entry, locales[i]);

          tmp = entry->local_schemas;
          while (tmp != NULL)
            {
              LocalSchemaInfo *lsi = tmp->data;

              if (c_local_schema == NULL &&
                  strcmp (lsi->locale, "C") == 0)
                {
                  c_local_schema = lsi;
                  if (best != NULL)
                    break;
                }

              if (best == NULL &&
                  strcmp (locales[i], lsi->locale) == 0)
                {
                  best = lsi;
                  if (c_local_schema != NULL)
                    break;
                }

              tmp = tmp->next;
            }

          /* Quit as soon as we have the best possible locale */
          if (best != NULL && c_local_schema != NULL)
            break;

          ++i;
        }

      /* If we found localized info, add it to the return value,
       * fall back to C locale if we can
       */

      if (best && best->locale)
	gconf_schema_set_locale (schema, best->locale);
      else
	gconf_schema_set_locale (schema, "C");

      if (best && best->default_value)
        gconf_schema_set_default_value (schema, best->default_value);
      else if (c_local_schema && c_local_schema->default_value)
        gconf_schema_set_default_value (schema, c_local_schema->default_value);

      if (best && best->short_desc)
        gconf_schema_set_short_desc (schema, best->short_desc);
      else if (c_local_schema && c_local_schema->short_desc)
        gconf_schema_set_short_desc (schema, c_local_schema->short_desc);

      if (best && best->long_desc)
        gconf_schema_set_long_desc (schema, best->long_desc);
      else if (c_local_schema && c_local_schema->long_desc)
        gconf_schema_set_long_desc (schema, c_local_schema->long_desc);

      return retval;
    }
}

const char*
markup_entry_get_name (MarkupEntry *entry)
{
  g_return_val_if_fail (entry->dir != NULL, NULL);
  g_return_val_if_fail (entry->dir->entries_loaded, NULL);

  return entry->name;
}

const char*
markup_entry_get_schema_name (MarkupEntry  *entry)
{
  g_return_val_if_fail (entry->dir != NULL, NULL);
  g_return_val_if_fail (entry->dir->entries_loaded, NULL);

  return entry->schema_name;
}

const char*
markup_entry_get_mod_user (MarkupEntry *entry)
{
  g_return_val_if_fail (entry->dir != NULL, NULL);
  g_return_val_if_fail (entry->dir->entries_loaded, NULL);

  return entry->mod_user;
}

GTime
markup_entry_get_mod_time (MarkupEntry *entry)
{
  g_return_val_if_fail (entry->dir != NULL, 0);
  g_return_val_if_fail (entry->dir->entries_loaded, 0);

  return entry->mod_time;
}

static void
markup_entry_set_mod_user (MarkupEntry *entry,
                           const char  *muser)
{
  if (muser == entry->mod_user)
    return;

  g_free (entry->mod_user);
  entry->mod_user = g_strdup (muser);
}

static void
markup_entry_set_mod_time (MarkupEntry *entry,
                           GTime        mtime)
{
  entry->mod_time = mtime;
}

/*
 * Parser
 */


/* The GConf XML format is on a lot of crack. When I wrote it,
 * I didn't know what I was doing, and now we're stuck with it.
 * Apologies.
 */

typedef enum
{
  STATE_START,
  STATE_GCONF,
  STATE_DIR,
  STATE_ENTRY,
  STATE_STRINGVALUE,
  STATE_LONGDESC,

  STATE_LOCAL_SCHEMA,

  /* these all work just like <entry> in storing a value but have no
   * name/muser/mtime/owner and in the case of car/cdr/li can only
   * store primitive values.
   */

  STATE_DEFAULT,
  STATE_CAR,
  STATE_CDR,
  STATE_LI
} ParseState;

typedef struct
{
  GSList      *states;

  MarkupDir   *root;
  GSList      *dir_stack;
 
  MarkupEntry *current_entry;  
  GSList      *value_stack;
  GSList      *value_freelist;

  /* Collected while parsing a schema entry */
  GSList      *local_schemas;

  char        *locale;

  guint        allow_subdirs : 1;
  guint        parsing_local_descs : 1;
} ParseInfo;

static void set_error (GError             **err,
                       GMarkupParseContext *context,
                       int                  error_code,
                       const char          *format,
                       ...) G_GNUC_PRINTF (4, 5);

static void dir_stack_push (ParseInfo *info,
			    MarkupDir *dir);

static void start_element_handler (GMarkupParseContext  *context,
                                   const gchar          *element_name,
                                   const gchar         **attribute_names,
                                   const gchar         **attribute_values,
                                   gpointer              user_data,
                                   GError              **error);
static void end_element_handler   (GMarkupParseContext  *context,
                                   const gchar          *element_name,
                                   gpointer              user_data,
                                   GError              **error);
static void text_handler          (GMarkupParseContext  *context,
                                   const gchar          *text,
                                   gsize                 text_len,
                                   gpointer              user_data,
                                   GError              **error);

static GMarkupParser gconf_parser = {
  start_element_handler,
  end_element_handler,
  text_handler,
  NULL,
  NULL
};

static void
set_error (GError             **err,
           GMarkupParseContext *context,
           int                  error_code,
           const char          *format,
           ...)
{
  int line, ch;
  va_list args;
  char *str;

  g_markup_parse_context_get_position (context, &line, &ch);

  va_start (args, format);
  str = g_strdup_vprintf (format, args);
  va_end (args);

  g_set_error (err, GCONF_ERROR, error_code,
               _("Line %d character %d: %s"),
               line, ch, str);

  g_free (str);
}

static void
parse_info_init (ParseInfo  *info,
                 MarkupDir  *root,
                 gboolean    allow_subdirs,
                 const char *locale)
{
  info->states = g_slist_prepend (NULL, GINT_TO_POINTER (STATE_START));

  info->root = root;
  info->dir_stack = NULL;

  info->current_entry = NULL;
  info->value_stack = NULL;
  info->value_freelist = NULL;

  info->local_schemas = NULL;

  info->locale = g_strdup (locale);

  info->allow_subdirs = allow_subdirs != FALSE;
  info->parsing_local_descs = info->locale != NULL;

  dir_stack_push (info, root);
}

static void
parse_info_free (ParseInfo *info)
{
  g_free (info->locale);

  g_slist_free (info->dir_stack);
  
  /* Don't free current_entry - always owned by tree */

  g_slist_foreach (info->local_schemas,
                   (GFunc) local_schema_info_free,
                   NULL);
  g_slist_free (info->local_schemas);

  /* only free values on the freelist, not those on the stack,
   * but all values in the freelist are also in the stack.
   */
  g_slist_foreach (info->value_freelist, (GFunc) gconf_value_free, NULL);
  g_slist_free (info->value_freelist);
  g_slist_free (info->value_stack);

  g_slist_free (info->states);
}

static void
push_state (ParseInfo  *info,
            ParseState  state)
{
  info->states = g_slist_prepend (info->states, GINT_TO_POINTER (state));
}

static void
pop_state (ParseInfo *info)
{
  g_return_if_fail (info->states != NULL);

  info->states = g_slist_remove (info->states, info->states->data);
}

static ParseState
peek_state (ParseInfo *info)
{
  g_return_val_if_fail (info->states != NULL, STATE_START);

  return GPOINTER_TO_INT (info->states->data);
}


/* add_to_freelist means that if the parse is aborted
 * while the value is on the stack, free that value
 */
static void
value_stack_push (ParseInfo  *info,
                  GConfValue *value,
                  gboolean    add_to_freelist)
{
  info->value_stack = g_slist_prepend (info->value_stack, value);
  if (add_to_freelist)
    info->value_freelist = g_slist_prepend (info->value_freelist, value);
}

static GConfValue*
value_stack_peek (ParseInfo *info)
{
  return info->value_stack ? info->value_stack->data : NULL;
}

static GConfValue*
value_stack_pop (ParseInfo *info)
{
  GConfValue *retval;

  if (!info->value_stack)
    return NULL;

  retval = info->value_stack->data;

  info->value_freelist = g_slist_remove (info->value_freelist, retval);
  info->value_stack    = g_slist_remove (info->value_stack,    retval);

  return retval;
}

static void
dir_stack_push (ParseInfo *info,
		MarkupDir *dir)
{
  info->dir_stack = g_slist_prepend (info->dir_stack, dir);
}

static MarkupDir*
dir_stack_peek (ParseInfo *info)
{
  g_return_val_if_fail (info->dir_stack != NULL, NULL);

  return info->dir_stack->data;
}

static MarkupDir*
dir_stack_pop (ParseInfo *info)
{
  MarkupDir *retval;

  g_return_val_if_fail (info->dir_stack != NULL, NULL);

  retval = info->dir_stack->data;
  info->dir_stack = g_slist_remove (info->dir_stack, retval);

  return retval;
}

#define ELEMENT_IS(name) (strcmp (element_name, (name)) == 0)

typedef struct
{
  const char  *name;
  const char **retloc;
} LocateAttr;

static gboolean
locate_attributes (GMarkupParseContext *context,
                   const char  *element_name,
                   const char **attribute_names,
                   const char **attribute_values,
                   GError     **error,
                   const char  *first_attribute_name,
                   const char **first_attribute_retloc,
                   ...)
{
  va_list args;
  const char *name;
  const char **retloc;
  int n_attrs;
#define MAX_ATTRS 24
  LocateAttr attrs[MAX_ATTRS];
  gboolean retval;
  int i;

  g_return_val_if_fail (first_attribute_name != NULL, FALSE);
  g_return_val_if_fail (first_attribute_retloc != NULL, FALSE);

  n_attrs = 1;
  attrs[0].name = first_attribute_name;
  attrs[0].retloc = first_attribute_retloc;
  *first_attribute_retloc = NULL;

  va_start (args, first_attribute_retloc);

  name = va_arg (args, const char*);
  retloc = va_arg (args, const char**);

  while (name != NULL)
    {
      g_return_val_if_fail (retloc != NULL, FALSE);

      g_assert (n_attrs < MAX_ATTRS);

      attrs[n_attrs].name = name;
      attrs[n_attrs].retloc = retloc;
      n_attrs += 1;
      *retloc = NULL;

      name = va_arg (args, const char*);
      retloc = va_arg (args, const char**);
    }

  va_end (args);

  retval = TRUE;

  for (i = 0; attribute_names[i]; i++)
    {
      int j;

      for (j = 0; j < n_attrs; j++)
        {
	  /* already matched */
	  if (attrs[j].name == NULL)
	    continue;

          if (strcmp (attrs[j].name, attribute_names[i]) == 0)
            {
              retloc = attrs[j].retloc;
	      attrs[j].name = NULL;

	      /* if this fails we passed the same retloc twice */
	      g_assert (*retloc == NULL);

              *retloc = attribute_values[i];
	      break;
            }
        }

      if (j >= n_attrs)
        {
          set_error (error, context,
                     GCONF_ERROR_PARSE_ERROR,
                     _("Attribute \"%s\" is invalid on <%s> element in this context"),
                     attribute_names[i], element_name);
          retval = FALSE;
	  break;
        }
    }

  return retval;
}

static gboolean
check_no_attributes (GMarkupParseContext *context,
                     const char  *element_name,
                     const char **attribute_names,
                     const char **attribute_values,
                     GError     **error)
{
  if (attribute_names[0] != NULL)
    {
      set_error (error, context,
                 GCONF_ERROR_PARSE_ERROR,
                 _("Attribute \"%s\" is invalid on <%s> element in this context"),
                 attribute_names[0], element_name);
      return FALSE;
    }

  return TRUE;
}

static gboolean
int_from_string (GMarkupParseContext *context,
                 const char          *str,
                 int                 *val,
                 GError             **error)
{
  char* endptr = NULL;
  glong result;

  *val = 0;

  errno = 0;
  result = strtol (str, &endptr, 10);

  if (endptr == str)
    {
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Didn't understand `%s' (expected integer)"),
                 str);
      return FALSE;
    }
  else if (errno == ERANGE || result < G_MININT || result > G_MAXINT)
    {
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Integer `%s' is too large or small"),
                 str);
      return FALSE;
    }
  else
    {
      *val = result;
      return TRUE;
    }
}

static gboolean
bool_from_string (GMarkupParseContext *context,
                  const char          *str,
                  gboolean            *val,
                  GError             **error)
{
  if (strcmp (str, "true") == 0)
    {
      *val = TRUE;
      return TRUE;
    }
  else if (strcmp (str, "false") == 0)
    {
      *val = FALSE;
      return TRUE;
    }
  else
    {
      *val = FALSE;

      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Didn't understand `%s' (expected true or false)"),
                 str);
      return FALSE;
    }
}


static gboolean
float_from_string (GMarkupParseContext *context,
                   const char          *str,
                   double              *val,
                   GError             **error)
{
  double num;

  if (gconf_string_to_double (str, &num))
    {
      *val = num;
      return TRUE;
    }
  else
    {
      *val = 0.0;
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Didn't understand `%s' (expected real number)"),
                 str);
      return FALSE;
    }
}

static void
parse_value_element (GMarkupParseContext  *context,
                     const gchar          *element_name,
                     const gchar         **attribute_names,
                     const gchar         **attribute_values,
                     GConfValue          **retval,
                     GError              **error)
{
  const char *type;
  const char *stype;
  const char *car_type;
  const char *cdr_type;
  const char *value;
  /* check out the crack; "ltype" is for nodes storing a list,
   * and "list_type" is for nodes storing a schema
   */
  const char *ltype;
  const char *list_type;
  const char *owner;
  GConfValueType vtype;
  const char *dummy1, *dummy2, *dummy3, *dummy4;
  
#if 0
  g_assert (ELEMENT_IS ("entry") ||
            ELEMENT_IS ("default") ||
            ELEMENT_IS ("cdr") ||
            ELEMENT_IS ("car") ||
            ELEMENT_IS ("li"));
#endif

  *retval = NULL;

  value = NULL;
  type = NULL;
  stype = NULL;
  ltype = NULL;
  list_type = NULL;
  car_type = NULL;
  cdr_type = NULL;
  owner = NULL;

  if (!locate_attributes (context, element_name, attribute_names, attribute_values,
                          error,
                          "value", &value,
                          "type", &type,
                          "stype", &stype,
                          "ltype", &ltype,
                          "list_type", &list_type,
                          "car_type", &car_type,
                          "cdr_type", &cdr_type,
                          "owner", &owner,

                          /* And these are just to eat any error messages */
                          "name", &dummy1,
                          "muser", &dummy2,
                          "mtime", &dummy3,
                          "schema", &dummy4,

                          NULL))
    return;

  if (type == NULL)
    {
	/* in fact this is a rather common case */
/*      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("No \"%s\" attribute on element <%s>"),
                 "type", element_name); */
      return;
    }
  
  vtype = gconf_value_type_from_string (type);
  if (vtype == GCONF_VALUE_INVALID)
    {
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Unknown value \"%s\" for \"%s\" attribute on element <%s>"),
                 type, "type", element_name);
      return;
    }

  switch (vtype)
    {
    case GCONF_VALUE_STRING:
      {
        *retval = gconf_value_new (GCONF_VALUE_STRING);
      }
      break;

    case GCONF_VALUE_LIST:
      {
        GConfValueType lvtype;

        if (ltype == NULL)
          {
            set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                       _("No \"%s\" attribute on element <%s>"),
                       "ltype", element_name);
            return;
          }

        lvtype = gconf_value_type_from_string (ltype);

        switch (lvtype)
          {
          case GCONF_VALUE_INVALID:
          case GCONF_VALUE_LIST:
          case GCONF_VALUE_PAIR:
          case GCONF_VALUE_SCHEMA:
            set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                       _("Invalid ltype \"%s\" on <%s>"),
                       ltype, element_name);
            return;
            break;
          default:
            break;
          }

        *retval = gconf_value_new (GCONF_VALUE_LIST);

        gconf_value_set_list_type (*retval,
                                   lvtype);
      }
      break;

    case GCONF_VALUE_SCHEMA:
      {
        GConfValueType schema_vtype;
        GConfSchema *schema;
        GConfValueType car_vtype;
        GConfValueType cdr_vtype;
        GConfValueType list_vtype;

        if (stype == NULL)
          {
            set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                       _("No \"%s\" attribute on element <%s>"),
                       "stype", element_name);
            return;
          }

        /* init for compiler warnings */
        car_vtype = GCONF_VALUE_INVALID;
        cdr_vtype = GCONF_VALUE_INVALID;
        list_vtype = GCONF_VALUE_INVALID;

        schema_vtype = gconf_value_type_from_string (stype);

        if (schema_vtype == GCONF_VALUE_PAIR)
          {
            
#if 0
            /* We have to allow missing car_type/cdr_type because
             * old versions of gconf would write it out that way
             * (if a schema was provided by an app and the
             *  schema was missing these fields)
             */ 
            if (car_type == NULL)
              {
                set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                           _("No \"%s\" attribute on element <%s>"),
                           "car_type", element_name);
                return;
              }

            if (cdr_type == NULL)
              {
                set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                           _("No \"%s\" attribute on element <%s>"),
                           "cdr_type", element_name);
                return;
              }
#endif

            if (car_type)
              car_vtype = gconf_value_type_from_string (car_type);
            else
              car_vtype = GCONF_VALUE_INVALID;

            if (cdr_type)
              cdr_vtype = gconf_value_type_from_string (cdr_type);
            else
              cdr_vtype = GCONF_VALUE_INVALID;

            switch (car_vtype)
              {
              case GCONF_VALUE_LIST:
              case GCONF_VALUE_PAIR:
              case GCONF_VALUE_SCHEMA:
                set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                           _("Invalid first-element type \"%s\" on <%s>"),
                           car_type, element_name);
                return;
                break;
              default:
                break;
              }

            switch (cdr_vtype)
              {
              case GCONF_VALUE_LIST:
              case GCONF_VALUE_PAIR:
              case GCONF_VALUE_SCHEMA:
                set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                           _("Invalid cdr_type \"%s\" on <%s>"),
                           cdr_type, element_name);
                return;
                break;
              default:
                break;
              }
          }
        else if (schema_vtype == GCONF_VALUE_LIST)
          {
#if 0
            /* We have to allow missing list_type because
             * old versions of gconf would write it out that way
             * (if a schema was provided by an app and the
             *  schema was missing these fields)
             */ 
            if (list_type == NULL)
              {
                set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                           _("No \"%s\" attribute on element <%s>"),
                           "list_type", element_name);
                return;
              }
#endif

            if (list_type)
              list_vtype = gconf_value_type_from_string (list_type);
            else
              list_vtype = GCONF_VALUE_INVALID;

            switch (list_vtype)
              {
              case GCONF_VALUE_LIST:
              case GCONF_VALUE_PAIR:
              case GCONF_VALUE_SCHEMA:
                set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                           _("Invalid list_type \"%s\" on <%s>"),
                           list_type, element_name);
                return;
                break;
              default:
                break;
              }
          }

        *retval = gconf_value_new (GCONF_VALUE_SCHEMA);

        schema = gconf_schema_new ();
        gconf_schema_set_type (schema, schema_vtype);

        if (schema_vtype == GCONF_VALUE_PAIR)
          {
            gconf_schema_set_car_type (schema, car_vtype);
            gconf_schema_set_cdr_type (schema, cdr_vtype);
          }
        else if (schema_vtype == GCONF_VALUE_LIST)
          {
            gconf_schema_set_list_type (schema, list_vtype);
          }

        if (owner)
          gconf_schema_set_owner (schema, owner);

        gconf_value_set_schema_nocopy (*retval, schema);
      }
      break;

    case GCONF_VALUE_PAIR:
      {
        *retval = gconf_value_new (GCONF_VALUE_PAIR);
      }
      break;

    case GCONF_VALUE_INT:
    case GCONF_VALUE_BOOL:
    case GCONF_VALUE_FLOAT:
      {
        double fval = 0.0;
        gboolean bval = FALSE;
        int ival = 0;

        if (value == NULL)
          {
            set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                       _("No \"%s\" attribute on element <%s>"),
                       "value", element_name);
            return;
          }

        switch (vtype)
          {
          case GCONF_VALUE_INT:
            if (!int_from_string (context, value, &ival, error))
              return;
            break;

          case GCONF_VALUE_BOOL:
            if (!bool_from_string (context, value, &bval, error))
              return;
            break;

          case GCONF_VALUE_FLOAT:
            if (!float_from_string (context, value, &fval, error))
              return;
            break;

          default:
            g_assert_not_reached ();
          }

        *retval = gconf_value_new (vtype);

        switch (vtype)
          {
          case GCONF_VALUE_INT:
            gconf_value_set_int (*retval, ival);
            break;

          case GCONF_VALUE_BOOL:
            gconf_value_set_bool (*retval, bval);
            break;

          case GCONF_VALUE_FLOAT:
            gconf_value_set_float (*retval, fval);
            break;

          default:
            g_assert_not_reached ();
          }
      }
      break;

    case GCONF_VALUE_INVALID:
      g_assert_not_reached ();
      break;
    }
}

static void
parse_entry_element (GMarkupParseContext  *context,
                     const gchar          *element_name,
                     const gchar         **attribute_names,
                     const gchar         **attribute_values,
                     ParseInfo            *info,
                     GError              **error)
{
  MarkupEntry *entry;

  g_return_if_fail (peek_state (info) == STATE_GCONF || peek_state (info) == STATE_DIR);
  g_return_if_fail (ELEMENT_IS ("entry"));
  g_return_if_fail (info->current_entry == NULL);

  push_state (info, STATE_ENTRY);

  if (!info->parsing_local_descs)
    {
      const char *name;
      const char *muser;
      const char *mtime;
      const char *schema;
      const char *type;
      const char *dummy1, *dummy2, *dummy3, *dummy4;
      const char *dummy5, *dummy6, *dummy7;
      GConfValue *value;
      GError *tmp_err;

      name = NULL;
      muser = NULL;
      mtime = NULL;
      schema = NULL;
      type = NULL;

      if (!locate_attributes (context, element_name, attribute_names, attribute_values,
                              error,
                              "name", &name,
                              "muser", &muser,
                              "mtime", &mtime,
                              "schema", &schema,
                              "type", &type,
                          
                              /* These are allowed but we don't use them until
                               * parse_value_element
                               */
                              "value", &dummy1,
                              "stype", &dummy2,
                              "ltype", &dummy3,
                              "list_type", &dummy4,
                              "car_type", &dummy5,
                              "cdr_type", &dummy6,
                              "owner", &dummy7,
                              NULL))
        return;

      if (name == NULL)
        {
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("No \"%s\" attribute on element <%s>"),
                     "name", element_name);
          return;
        }

      /* Entries can exist just for the schema name,
       * lacking a value element. But if the entry has a type
       * attribute, it's supposed to have a value.
       */
      value = NULL;
      tmp_err = NULL;
      parse_value_element (context, element_name, attribute_names,
                           attribute_values, &value,
                           &tmp_err);

      if (tmp_err)
        {
          if (type != NULL)
            {
              g_propagate_error (error, tmp_err);
              return;
            }
          else
            g_error_free (tmp_err);
        }
  
      entry = markup_entry_new (dir_stack_peek (info), name);
      if (value != NULL)
        {
          entry->value = value;
          value_stack_push (info, value, FALSE); /* FALSE since entry owns it */
        }
      
      if (muser)
        markup_entry_set_mod_user (entry, muser);

      if (mtime)
        {
          GTime vmtime;

          vmtime = gconf_string_to_gulong (mtime);
      
          markup_entry_set_mod_time (entry, vmtime);
        }

      /* don't use markup_entry_set_schema_name because it would
       * mess up the modtime
       */
      if (schema)
        entry->schema_name = g_strdup (schema);
    }
  else
    {
      MarkupDir  *dir;
      GSList     *tmp;
      const char *name;
  
      name = NULL;

      if (!locate_attributes (context, element_name, attribute_names, attribute_values,
                              error,
                              "name", &name,
                              NULL))
        return;

      if (name == NULL)
        {
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("No \"%s\" attribute on element <%s>"),
                     "name", element_name);
          return;
        }

      dir = dir_stack_peek (info);

      entry = NULL;

      tmp = dir->entries;
      while (tmp != NULL)
        {
          entry = tmp->data;

          if (strcmp (entry->name, name) == 0)
            break;
          else
            entry = NULL;

          tmp = tmp->next;
        }

      /* Note: entry can be NULL here, in which case we'll discard
       * the LocalSchemaInfo once we've finished parsing this entry
       */
    }

  info->current_entry = entry;
}

static void
parse_dir_element (GMarkupParseContext  *context,
		   const gchar          *element_name,
		   const gchar         **attribute_names,
		   const gchar         **attribute_values,
		   ParseInfo            *info,
		   GError              **error)
{
  MarkupDir  *parent;
  MarkupDir  *dir;
  const char *name;
  
  g_return_if_fail (peek_state (info) == STATE_GCONF || peek_state (info) == STATE_DIR);
  g_return_if_fail (ELEMENT_IS ("dir"));

  push_state (info, STATE_DIR);

  name = NULL;

  if (!locate_attributes (context, element_name, attribute_names, attribute_values,
                          error,
                          "name", &name,
                          NULL))
    return;

  if (name == NULL)
    {
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("No \"%s\" attribute on element <%s>"),
                 "name", element_name);
      return;
    }

  dir = NULL;
  parent = dir_stack_peek (info);

  if (!info->parsing_local_descs)
    {
      dir = markup_dir_new (info->root->tree, parent, name);

      dir->not_in_filesystem = TRUE;
      dir->entries_loaded    = TRUE;
      dir->subdirs_loaded    = TRUE;
    }
  else
    {
      GSList *tmp;

      tmp = parent->subdirs;
      while (tmp != NULL)
        {
          dir = tmp->data;

          if (strcmp (dir->name, name) == 0)
            break;
          else
            dir = NULL;

          tmp = tmp->next;
        }

      if (dir == NULL)
        {
          dir = markup_dir_new (info->root->tree, parent, name);

          /* This is a dummy directory which will be deleted when
           * we've finised parsing the contents of this element.
           */
          dir->is_parser_dummy = TRUE;
        }
    }

  g_assert (dir != NULL);

  dir_stack_push (info, dir);
}

static void
parse_local_schema_child_element (GMarkupParseContext  *context,
                                  const gchar          *element_name,
                                  const gchar         **attribute_names,
                                  const gchar         **attribute_values,
                                  ParseInfo            *info,
                                  GError              **error)
{
  LocalSchemaInfo *local_schema;

  g_return_if_fail (peek_state (info) == STATE_LOCAL_SCHEMA);

  local_schema = info->local_schemas->data;

  if (ELEMENT_IS ("default") && !info->parsing_local_descs)
    {
      GConfValue *value;
      
      push_state (info, STATE_DEFAULT);

      value = NULL;
      parse_value_element (context, element_name, attribute_names,
                           attribute_values, &value,
                           error);
      if (value == NULL)
        return;

      if (local_schema->default_value != NULL)
        {
          gconf_value_free (value);
          set_error (error, context,
                     GCONF_ERROR_PARSE_ERROR,
                     _("Two <default> elements below a <local_schema>"));
          return;
        }

      local_schema->default_value = value;
      value_stack_push (info, value, FALSE); /* local_schema owns it */
    }
  else if (ELEMENT_IS ("longdesc"))
    {
      push_state (info, STATE_LONGDESC);

      if (local_schema->long_desc != NULL)
        {
          set_error (error, context,
                     GCONF_ERROR_PARSE_ERROR,
                     _("Two <longdesc> elements below a <local_schema>"));
        }
    }
  else
    {
      set_error (error, context,
                 GCONF_ERROR_PARSE_ERROR,
                 _("Element <%s> is not allowed below <%s>"),
                 element_name, "local_schema");
    }
}

static void
parse_local_schema_element (GMarkupParseContext  *context,
                            const gchar          *element_name,
                            const gchar         **attribute_names,
                            const gchar         **attribute_values,
                            ParseInfo            *info,
                            GError              **error)
{
  const char *locale;
  const char *short_desc;
  LocalSchemaInfo *local_schema;

  g_return_if_fail (ELEMENT_IS ("local_schema"));

  if (!info->parsing_local_descs &&
      (info->current_entry == NULL ||
       info->current_entry->value == NULL ||
       info->current_entry->value->type != GCONF_VALUE_SCHEMA))
    {
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("<%s> provided but current element does not have type %s"),
                 "local_schema", "schema");
      return;
    }

  push_state (info, STATE_LOCAL_SCHEMA);

  locale = NULL;
  short_desc = NULL;

  if (!info->parsing_local_descs)
    {
      if (!locate_attributes (context, element_name, attribute_names, attribute_values,
                              error,
                              "locale", &locale,
                              "short_desc", &short_desc,
                              NULL))
        return;

      if (locale == NULL)
        {
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("No \"%s\" attribute on element <%s>"),
                     "locale", element_name);
          return;
        }
    }
  else
    {
      if (!locate_attributes (context, element_name, attribute_names, attribute_values,
                              error,
                              "short_desc", &short_desc,
                              NULL))
        return;

      locale = info->locale;
    }

  local_schema = local_schema_info_new ();
  local_schema->locale = g_strdup (locale);
  local_schema->short_desc = g_strdup (short_desc);

  info->local_schemas = g_slist_prepend (info->local_schemas,
                                         local_schema);
}

static void
parse_car_or_cdr_element (GMarkupParseContext  *context,
                          const gchar          *element_name,
                          const gchar         **attribute_names,
                          const gchar         **attribute_values,
                          ParseInfo            *info,
                          GError              **error)
{
  ParseState current_state;
  GConfValue *value;
  GConfValue *pair;

  current_state = ELEMENT_IS ("car") ? STATE_CAR : STATE_CDR;
  push_state (info, current_state);

  value = NULL;
  parse_value_element (context, element_name, attribute_names,
                       attribute_values, &value,
                       error);
  if (value == NULL)
    return;

  pair = value_stack_peek (info);

  if (pair->type == GCONF_VALUE_PAIR)
    {
      if (current_state == STATE_CAR)
        {
          if (gconf_value_get_car (pair) == NULL)
            {
              gconf_value_set_car_nocopy (pair, value);
              value_stack_push (info, value, FALSE); /* pair owns it */
            }
          else
            {
              gconf_value_free (value);
              set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                         _("Two <car> elements given for same pair"));
            }
        }
      else
        {
          if (gconf_value_get_cdr (pair) == NULL)
            {
              gconf_value_set_cdr_nocopy (pair, value);
              value_stack_push (info, value, FALSE); /* pair owns it */
            }
          else
            {
              gconf_value_free (value);
              set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                         _("Two <cdr> elements given for same pair"));
            }
        }
    }
  else
    {
      gconf_value_free (value);
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("<%s> provided but current element does not have type %s"),
                 current_state == STATE_CAR ? "car" : "cdr", "pair");
    }
}

static void
parse_li_element (GMarkupParseContext  *context,
                  const gchar          *element_name,
                  const gchar         **attribute_names,
                  const gchar         **attribute_values,
                  ParseInfo            *info,
                  GError              **error)
{
  ParseState current_state;
  GConfValue *value;
  GConfValue *list;

  current_state = peek_state (info);

  push_state (info, STATE_LI);
  
  value = NULL;
  parse_value_element (context, element_name, attribute_names,
                       attribute_values, &value,
                       error);
  if (value == NULL)
    return;

  list = value_stack_peek (info);

  if (list->type == GCONF_VALUE_LIST)
    {
      if (value->type == gconf_value_get_list_type (list))
        {
          GSList *slist;

          slist = gconf_value_steal_list (list);
          slist = g_slist_append (slist, value);
          gconf_value_set_list_nocopy (list, slist);
          
          value_stack_push (info, value, FALSE); /* FALSE since list owns it */
        }
      else
        {
          gconf_value_free (value);
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("<li> has wrong type %s"),
                     gconf_value_type_to_string (value->type));
        }
    }
  else
    {
      gconf_value_free (value);
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("<%s> provided but current element does not have type %s"),
                 "li", "list");
    }
}

static void
parse_value_child_element (GMarkupParseContext  *context,
                           const gchar          *element_name,
                           const gchar         **attribute_names,
                           const gchar         **attribute_values,
                           ParseInfo            *info,
                           GError              **error)
{
  ParseState current_state;

  current_state = peek_state (info);

  if (!info->parsing_local_descs)
    {
      if (current_state == STATE_ENTRY &&
          info->current_entry->value == NULL)
        {
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("<%s> provided but parent <entry> does not have a value"),
                     element_name);
          return;
        }
      else if (current_state == STATE_ENTRY)
        {
          g_assert (info->current_entry->value == value_stack_peek (info));
        }
    }
  
  if (ELEMENT_IS ("stringvalue") && !info->parsing_local_descs)
    {
      GConfValue *value;

      value = value_stack_peek (info);

      if (value->type == GCONF_VALUE_STRING)
        {
          push_state (info, STATE_STRINGVALUE);

          /* Put in an empty string, since <stringvalue></stringvalue>
           * doesn't result in a text_handler callback
           */
          gconf_value_set_string (value, "");
        }
      else
        {
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("<%s> provided but current element does not have type %s"),
                     "stringvalue", "string");
        }
    }
  else if (ELEMENT_IS ("local_schema"))
    {
      switch (current_state)
        {
        case STATE_CAR:
        case STATE_CDR:
        case STATE_LI:
        case STATE_DEFAULT:
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("Element <%s> is not allowed inside current element"),
                     element_name);
          break;
        case STATE_ENTRY:
          parse_local_schema_element (context, element_name,
                                      attribute_names, attribute_values,
                                      info, error);
          break;
        default:
          g_assert_not_reached ();
          break;
        }
    }
  else if ((ELEMENT_IS ("car") ||
            ELEMENT_IS ("cdr")) &&
           !info->parsing_local_descs)
    {
      switch (current_state)
        {
        case STATE_CAR:
        case STATE_CDR:
        case STATE_LI:
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("Element <%s> is not allowed inside current element"),
                     element_name);
          break;
        case STATE_DEFAULT:
        case STATE_ENTRY:
          parse_car_or_cdr_element (context, element_name,
                                    attribute_names, attribute_values,
                                    info, error);
          break;
        default:
          g_assert_not_reached ();
          break;
        }
    }
  else if (ELEMENT_IS ("li") && !info->parsing_local_descs)
    {
      switch (current_state)
        {
        case STATE_CAR:
        case STATE_CDR:
        case STATE_LI:
          set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                     _("Element <%s> is not allowed inside current element"),
                     element_name);
          break;
        case STATE_DEFAULT:
        case STATE_ENTRY:
          parse_li_element (context, element_name,
                            attribute_names, attribute_values,
                            info, error);
          break;
        default:
          g_assert_not_reached ();
          break;
        }
    }
  else
    {
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Element <%s> is not allowed inside current element"),
                 element_name);
    }
}

static void
start_element_handler (GMarkupParseContext *context,
                       const gchar         *element_name,
                       const gchar        **attribute_names,
                       const gchar        **attribute_values,
                       gpointer             user_data,
                       GError             **error)
{
  ParseInfo *info = user_data;
  ParseState current_state;

  current_state = peek_state (info);

  switch (current_state)
    {
    case STATE_START:
      if (ELEMENT_IS ("gconf"))
        {
          if (!check_no_attributes (context, element_name,
                                    attribute_names, attribute_values,
                                    error))
            return;

          push_state (info, STATE_GCONF);
        }
      else
        set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                   _("Outermost element in menu file must be <gconf> not <%s>"),
                   element_name);
      break;

    case STATE_GCONF:
    case STATE_DIR:      
      if (ELEMENT_IS ("entry"))
        {
          parse_entry_element (context, element_name,
                               attribute_names, attribute_values,
                               info, error);
        }
      else if (ELEMENT_IS ("dir") && info->allow_subdirs)
	{
	  parse_dir_element (context, element_name,
			     attribute_names, attribute_values,
			     info, error);
	}
      else
        set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                   _("Element <%s> is not allowed inside a <%s> element"),
                   element_name, current_state == STATE_GCONF ? "gconf" : "dir");
      break;

    case STATE_ENTRY:
    case STATE_DEFAULT:
    case STATE_CAR:
    case STATE_CDR:
    case STATE_LI:
      parse_value_child_element (context, element_name,
                                 attribute_names, attribute_values,
                                 info, error);
      break;

    case STATE_LOCAL_SCHEMA:
      parse_local_schema_child_element (context, element_name,
                                        attribute_names, attribute_values,
                                        info, error);
      break;
      
    case STATE_STRINGVALUE:
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Element <%s> is not allowed inside a <%s> element"),
                 element_name, "stringvalue");
      break;
    case STATE_LONGDESC:
      set_error (error, context, GCONF_ERROR_PARSE_ERROR,
                 _("Element <%s> is not allowed inside a <%s> element"),
                 element_name, "longdesc");
      break;
    }
}

static void
end_element_handler (GMarkupParseContext *context,
                     const gchar         *element_name,
                     gpointer             user_data,
                     GError             **error)
{
  ParseInfo *info = user_data;

  switch (peek_state (info))
    {
    case STATE_START:
      break;
      
    case STATE_ENTRY:
      if (!info->parsing_local_descs)
        {
          g_assert (info->current_entry);
          g_assert (info->current_entry->local_schemas == NULL);

          info->current_entry->local_schemas = g_slist_reverse (info->local_schemas);
          info->local_schemas = NULL;

          if (info->current_entry->value != NULL)
            value_stack_pop (info);
        }
      else if (info->local_schemas != NULL)
        {
          LocalSchemaInfo *local_schema;

          g_assert (g_slist_length (info->local_schemas) == 1);

          local_schema = info->local_schemas->data;

          g_slist_free (info->local_schemas);
          info->local_schemas = NULL;

          if (info->current_entry != NULL &&
              info->current_entry->value != NULL &&
              info->current_entry->value->type == GCONF_VALUE_SCHEMA)
            {
              GSList *tmp;

              tmp = info->current_entry->local_schemas;
              while (tmp != NULL)
                {
                  LocalSchemaInfo *lsi = tmp->data;

                  if (strcmp (local_schema->locale, lsi->locale) == 0)
                    {
                      g_free (lsi->short_desc);
                      lsi->short_desc = local_schema->short_desc;
                      local_schema->short_desc = NULL;

                      g_free (lsi->long_desc);
                      lsi->long_desc = local_schema->short_desc;
                      local_schema->long_desc = NULL;

                      local_schema_info_free (local_schema);

                      break;
                    }

                  tmp = tmp->next;
                }

              if (tmp == NULL)
                {
                  info->current_entry->local_schemas =
                    g_slist_append (info->current_entry->local_schemas,
                                    local_schema);
                }
            }
          else
            {
              local_schema_info_free (local_schema);
            }
        }
      
      info->current_entry = NULL;

      pop_state (info);
      break;

    case STATE_DEFAULT:
      {
        GConfValue *value;
        LocalSchemaInfo *local_schema;

        local_schema = info->local_schemas->data;

        /* Default should already be in a LocalSchemaInfo */
        value = value_stack_peek (info);

        g_assert (value == local_schema->default_value);

        value_stack_pop (info);

        pop_state (info);
      }
      break;

    case STATE_CAR:
    case STATE_CDR:
    case STATE_LI:
      value_stack_pop (info);
      pop_state (info);
      break;

    case STATE_DIR:
    case STATE_GCONF:
      {
	MarkupDir *dir;
      
	dir = dir_stack_pop (info);

        if (!info->parsing_local_descs)
          {
            dir->entries = g_slist_reverse (dir->entries);
            dir->subdirs = g_slist_reverse (dir->subdirs);
          }
        else if (dir->is_parser_dummy)
          {
            dir->parent->subdirs = g_slist_remove (dir->parent->subdirs, dir);
            markup_dir_free (dir);
          }

	pop_state (info);
      }
      break;

    case STATE_LOCAL_SCHEMA:
    case STATE_LONGDESC:
    case STATE_STRINGVALUE:
      pop_state (info);
      break;
    }
}

#define NO_TEXT(element_name) set_error (error, context, GCONF_ERROR_PARSE_ERROR, _("No text is allowed inside element <%s>"), element_name)

static gboolean
all_whitespace (const char *text,
                int         text_len)
{
  const char *p;
  const char *end;

  p = text;
  end = text + text_len;

  while (p != end)
    {
      if (G_UNLIKELY (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\r'))
	return FALSE;
      p++;
    }

  return TRUE;
}

static void
text_handler (GMarkupParseContext *context,
              const gchar         *text,
              gsize                text_len,
              gpointer             user_data,
              GError             **error)
{
  ParseInfo *info = user_data;

  if (all_whitespace (text, text_len))
    return;

  /* FIXME http://bugzilla.gnome.org/show_bug.cgi?id=70448 would
   * allow a nice cleanup here.
   */

  switch (peek_state (info))
    {
    case STATE_START:
      g_assert_not_reached (); /* gmarkup shouldn't do this */
      break;
    case STATE_STRINGVALUE:
      {
        GConfValue *value;

        value = value_stack_peek (info);
        g_assert (value->type == GCONF_VALUE_STRING);

        gconf_value_set_string_nocopy (value,
                                       g_strndup (text, text_len));
      }
      break;
    case STATE_LONGDESC:
      {
        LocalSchemaInfo *local_schema;

        local_schema = info->local_schemas->data;

        local_schema->long_desc = g_strndup (text, text_len);
      }
      break;
    case STATE_GCONF:
      NO_TEXT ("gconf");
      break;
    case STATE_DIR:
      NO_TEXT ("dir");
      break;
    case STATE_ENTRY:
      NO_TEXT ("entry");
      break;
    case STATE_LOCAL_SCHEMA:
      NO_TEXT ("local_schema");
      break;
    case STATE_DEFAULT:
      NO_TEXT ("default");
      break;
    case STATE_CAR:
      NO_TEXT ("car");
      break;
    case STATE_CDR:
      NO_TEXT ("cdr");
      break;
    case STATE_LI:
      NO_TEXT ("li");
      break;
    }
}

static void
parse_tree (MarkupDir   *root,
            gboolean     parse_subtree,
            const char  *locale,
            GError     **err)
{
  GMarkupParseContext *context = NULL;
  GError *error;
  ParseInfo info;
  char *filename;
  FILE *f;

  if (!parse_subtree)
    g_assert (locale == NULL);

  filename = markup_dir_build_file_path (root, parse_subtree, locale);
  
  parse_info_init (&info, root, parse_subtree, locale);

  error = NULL;

  f = g_fopen (filename, "rb");
  if (f == NULL)
    {
      char *str;

      str = g_strdup_printf (_("Failed to open \"%s\": %s\n"),
			     filename, g_strerror (errno));
      error = g_error_new_literal (GCONF_ERROR,
				   GCONF_ERROR_FAILED,
				   str);
      g_free (str);

      goto out;
    }

  context = g_markup_parse_context_new (&gconf_parser,
                                        0, &info, NULL);

  while (!feof (f))
    {
      char  text[4096];
      gsize n_bytes;
      
      n_bytes = fread (text, 1, sizeof (text), f);
      if (n_bytes > 0)
	{
	  error = NULL;
	  if (!g_markup_parse_context_parse (context, text, n_bytes, &error))
	    goto out;
	}

      if (ferror (f))
	{
	  char *str;

	  str = g_strdup_printf (_("Error reading \"%s\": %s\n"),
                                 filename, g_strerror (errno));
	  error = g_error_new_literal (GCONF_ERROR,
				       GCONF_ERROR_FAILED,
				       str);
	  g_free (str);

	  goto out;
	}
    }

  error = NULL;
  if (!g_markup_parse_context_end_parse (context, &error))
    goto out;

 out:

  if (context)
    g_markup_parse_context_free (context);
  g_free (filename);

  if (f != NULL)
    fclose (f);

  parse_info_free (&info);

  if (error)
    g_propagate_error (err, error);
}

/*
 * Save
 */

#define INDENT_SPACES 1

static gboolean write_list_children   (GConfValue  *value,
                                       FILE        *f,
                                       int          indent);
static gboolean write_pair_children   (GConfValue  *value,
                                       FILE        *f,
                                       int          indent);
static gboolean write_schema_children (GConfValue  *value,
                                       FILE        *f,
                                       int          indent,
                                       GSList      *local_schemas,
                                       gboolean     save_as_subtree);

/* the common case - before we start interning */
static const char write_indents_static[] = 
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"  /* 16 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"; /* 32 */

static const char *make_whitespace (int indent)
{
  int idx = MAX (sizeof (write_indents_static) - 1 - indent, 0);
  return &write_indents_static[idx];
}

static gboolean
write_value_element (GConfValue *value,
                     const char *closing_element,
                     FILE       *f,
                     int         indent,
                     GSList     *local_schemas,
                     gboolean    save_as_subtree)
{
  gboolean single_element = FALSE;
  /* We are at the "<foo bar="whatever"" stage here,
   * <foo> still missing the closing >
   */
  
  if (fprintf (f, " type=\"%s\"",
               gconf_value_type_to_string (value->type)) < 0)
    return FALSE;
  
  switch (value->type)
    {          
    case GCONF_VALUE_LIST:
      if (fprintf (f, " ltype=\"%s\"",
                   gconf_value_type_to_string (gconf_value_get_list_type (value))) < 0)
        return FALSE;
      break;
      
    case GCONF_VALUE_SCHEMA:
      {
        GConfSchema *schema;
        GConfValueType stype;
        const char *owner;
        
        schema = gconf_value_get_schema (value);

        stype = gconf_schema_get_type (schema);
        
        if (fprintf (f, " stype=\"%s\"",
                     gconf_value_type_to_string (stype)) < 0)
          return FALSE;

        owner = gconf_schema_get_owner (schema);

        if (owner)
          {
            char *s;

            s = g_markup_escape_text (owner, -1);
            
            if (fprintf (f, " owner=\"%s\"", s) < 0)
              {
                g_free (s);
                return FALSE;
              }
            
            g_free (s);
          }
        
        if (stype == GCONF_VALUE_LIST)
          {
            GConfValueType list_type = gconf_schema_get_list_type (schema);

            if (list_type != GCONF_VALUE_INVALID)
              {
                if (fprintf (f, " list_type=\"%s\"",
                             gconf_value_type_to_string (list_type)) < 0)
                  return FALSE;
              }
          }

        if (stype == GCONF_VALUE_PAIR)
          {
            GConfValueType car_type;
            GConfValueType cdr_type;

            car_type = gconf_schema_get_car_type (schema);
            cdr_type = gconf_schema_get_cdr_type (schema);

            if (car_type != GCONF_VALUE_INVALID)
              {
                if (fprintf (f, " car_type=\"%s\"",
                             gconf_value_type_to_string (car_type)) < 0)
                  return FALSE;
              }

            if (cdr_type != GCONF_VALUE_INVALID)
              {
                if (fprintf (f, " cdr_type=\"%s\"",
                             gconf_value_type_to_string (cdr_type)) < 0)
                  return FALSE;
              }
          }
      }
      break;

    case GCONF_VALUE_INT:
      if (fprintf (f, " value=\"%d\"",
                   gconf_value_get_int (value)) < 0)
        return FALSE;
      break;

    case GCONF_VALUE_BOOL:
      if (fprintf (f, " value=\"%s\"",
                   gconf_value_get_bool (value) ? "true" : "false") < 0)
        return FALSE;
      break;

    case GCONF_VALUE_FLOAT:
      {
        char *s;

        s = gconf_double_to_string (gconf_value_get_float (value));
        if (fprintf (f, " value=\"%s\"", s) < 0)
          {
            g_free (s);
            return FALSE;
          }
        g_free (s);
      }
      break;

    case GCONF_VALUE_INVALID:
    case GCONF_VALUE_STRING:
    case GCONF_VALUE_PAIR:
      break;
    }
    
  switch (value->type)
    {
    case GCONF_VALUE_STRING:
      {
        char *s;
        
        s = g_markup_escape_text (gconf_value_get_string (value),
                                  -1);
        if (fprintf (f, ">\n%s<stringvalue>%s</stringvalue>\n",
                     make_whitespace (indent + INDENT_SPACES), s) < 0)
          {
            g_free (s);
            return FALSE;
          }
        
        g_free (s);
      }
      break;
      
    case GCONF_VALUE_LIST:
      if (fputs (">\n", f) < 0)
	return FALSE;
      if (!write_list_children (value, f, indent + INDENT_SPACES))
        return FALSE;
      break;
      
    case GCONF_VALUE_PAIR:
      if (fputs (">\n", f) < 0)
	return FALSE;
      if (!write_pair_children (value, f, indent + INDENT_SPACES))
        return FALSE;
      break;
      
    case GCONF_VALUE_SCHEMA:
      if (fputs (">\n", f) < 0)
	return FALSE;
      if (!write_schema_children (value,
                                  f,
                                  indent + INDENT_SPACES,
                                  local_schemas,
                                  save_as_subtree))
        return FALSE;
      break;

    case GCONF_VALUE_INT:
    case GCONF_VALUE_BOOL:
    case GCONF_VALUE_FLOAT:
    case GCONF_VALUE_INVALID:
      if (fputs ("/>\n", f) < 0)
	return FALSE;
      single_element = TRUE;
      break;
    }

  if (!single_element)
    if (fprintf (f, "%s</%s>\n", make_whitespace (indent), closing_element) < 0)
      return FALSE;

  return TRUE;
}    

static gboolean
write_list_children (GConfValue  *value,
                     FILE        *f,
                     int          indent)
{
  GSList *tmp;
  gboolean retval = FALSE;

  tmp = gconf_value_get_list (value);
  while (tmp != NULL)
    {
      GConfValue *li = tmp->data;

      if (fputs (make_whitespace (indent), f) < 0)
	goto out;
      
      if (fputs ("<li", f) < 0)
	goto out;

      if (!write_value_element (li, "li", f, indent, NULL, FALSE))
	goto out;

      tmp = tmp->next;
    }

  retval = TRUE;

 out:

  return retval;
}

static gboolean
write_pair_children (GConfValue  *value,
                     FILE        *f,
                     int          indent)
{
  GConfValue *child;
  gboolean retval = FALSE;

  child = gconf_value_get_car (value);

  if (child != NULL)
    {
      if (fputs (make_whitespace (indent), f) < 0)
	goto out;

      if (fputs ("<car", f) < 0)
	goto out;

      if (!write_value_element (child, "car", f, indent, NULL, FALSE))
	goto out;
    }

  child = gconf_value_get_cdr (value);

  if (child != NULL)
    {
      if (fputs (make_whitespace (indent), f) < 0)
	goto out;
      
      if (fputs ("<cdr", f) < 0)
	goto out;

      if (!write_value_element (child, "cdr", f, indent, NULL, FALSE))
	goto out;
    }

  retval = TRUE;
 
 out:

  return retval;
}

static gboolean
write_local_schema_info (LocalSchemaInfo *local_schema,
                         FILE            *f,
                         int              indent,
                         gboolean         is_locale_file,
                         gboolean         write_descs)
{
  gboolean retval;
  const char *whitespace1, *whitespace2;
  char *s;

  if (!write_descs && local_schema->default_value == NULL)
    return TRUE;

  retval = FALSE;

  whitespace1 = make_whitespace (indent);
  whitespace2 = make_whitespace (indent + INDENT_SPACES);

  if (fputs (whitespace1, f) < 0)
    goto out;

  if (fputs ("<local_schema", f) < 0)
    goto out;

  if (!is_locale_file)
    {
      g_assert (local_schema->locale);
      
      s = g_markup_escape_text (local_schema->locale, -1);

      if (fprintf (f, " locale=\"%s\"", s) < 0)
        {
          g_free (s);
          goto out;
        }
      
      g_free (s);
    }

  if (write_descs && local_schema->short_desc)
    {
      s = g_markup_escape_text (local_schema->short_desc, -1);

      if (fprintf (f, " short_desc=\"%s\"", s) < 0)
        {
          g_free (s);
          goto out;
        }
          
      g_free (s);
    }

  if (fputs (">\n", f) < 0)
    goto out;

  if (!is_locale_file && local_schema->default_value)
    {
      if (fputs (whitespace2, f) < 0)
        goto out;

      if (fputs ("<default", f) < 0)
        goto out;

      if (!write_value_element (local_schema->default_value,
                                "default",
                                f,
                                indent + INDENT_SPACES,
                                NULL,
                                FALSE))
        goto out;
    }

  if (write_descs && local_schema->long_desc)
    {
      if (fprintf (f, "%s<longdesc>", whitespace2) < 0)
        goto out;

      s = g_markup_escape_text (local_schema->long_desc, -1);
          
      if (fputs (s, f) < 0)
        {
          g_free (s);
          goto out;
        }
          
      g_free (s);

      if (fputs ("</longdesc>\n", f) < 0)
        goto out;
    }

  if (fputs (whitespace1, f) < 0)
    goto out;

  if (fputs ("</local_schema>\n", f) < 0)
    goto out;

  retval = TRUE;

 out:

  return retval;
}

static gboolean
write_schema_children (GConfValue *value,
                       FILE       *f,
                       int         indent,
                       GSList     *local_schemas,
		       gboolean    save_as_subtree)
{
  /* Here we write each local_schema, in turn a local_schema can
   * contain <default> and <longdesc> and have locale and short_desc
   * attributes
   */
  GSList *tmp;

  tmp = local_schemas;
  while (tmp != NULL)
    {
      LocalSchemaInfo *local_schema = tmp->data;
      gboolean write_descs;

      write_descs = TRUE;

      if (save_as_subtree &&
	  strcmp (local_schema->locale, "C") != 0)
	write_descs = FALSE;

      if (!write_local_schema_info (local_schema,
				    f,
				    indent,
				    FALSE,
				    write_descs))
	return FALSE;
      
      tmp = tmp->next;
    }
  
  return TRUE;
}

static void
get_non_c_desc_locales (MarkupEntry *entry,
			GHashTable  *non_c_desc_locales)
{
  GSList *tmp;

  tmp = entry->local_schemas;
  while (tmp != NULL)
    {
      LocalSchemaInfo *local_schema = tmp->data;

      if (strcmp (local_schema->locale, "C") != 0 &&
	  local_schema->short_desc != NULL &&
	  local_schema->long_desc != NULL)
	{
	  g_hash_table_replace (non_c_desc_locales,
				(char *) local_schema->locale,
				GINT_TO_POINTER (TRUE));
	}

      tmp = tmp->next;
    }
}

static LocalSchemaInfo *
get_local_schema_info (MarkupEntry *entry,
		       const char  *locale)
{
  GSList *tmp;

  tmp = entry->local_schemas;
  while (tmp != NULL)
    {
      LocalSchemaInfo *local_schema = tmp->data;

      if (strcmp (local_schema->locale, locale) == 0)
	{
	  return local_schema;
	}

      tmp = tmp->next;
    }

  return NULL;
}

static gboolean
write_entry (MarkupEntry *entry,
             FILE        *f,
	     int          indent,
	     gboolean     save_as_subtree,
	     const char  *locale,
	     GHashTable  *other_locales)
{
  LocalSchemaInfo *local_schema_info;
  gboolean         retval;

  retval = FALSE;
  local_schema_info = NULL;

  if (save_as_subtree)
    {
      if (locale == NULL)
	{
	  g_assert (other_locales != NULL);
	  get_non_c_desc_locales (entry, other_locales);
	}
      else
	{
	  if ((local_schema_info = get_local_schema_info (entry, locale)) == NULL)
	    return TRUE;
	}
    }

  g_assert (entry->name != NULL);
  
  if (fprintf (f, "%s<entry name=\"%s\"", make_whitespace (indent), entry->name) < 0)
    goto out;

  if (local_schema_info == NULL)
    {
      if (fprintf (f, " mtime=\"%lu\"", (unsigned long) entry->mod_time) < 0)
	goto out;
  
      if (entry->schema_name)
	{
	  if (fprintf (f, " schema=\"%s\"", entry->schema_name) < 0)
	    goto out;
	}

      if (entry->mod_user)
	{
	  if (fprintf (f, " muser=\"%s\"", entry->mod_user) < 0)
	    goto out;
	}

      if (entry->value != NULL)
        {
          if (!write_value_element (entry->value,
                                    "entry",
                                    f,
                                    indent,
                                    entry->local_schemas,
                                    save_as_subtree))
            goto out;
        }
      else
        {
          if (fputs ("/>\n", f) < 0)
            goto out;
        }
    }
  else
    {
      if (fputs (">\n", f) < 0)
        goto out;

      if (!write_local_schema_info (local_schema_info,
                                    f,
                                    indent + INDENT_SPACES,
                                    TRUE,
                                    TRUE))
        goto out;
                                    
      if (fprintf (f, "%s</entry>\n", make_whitespace (indent)) < 0)
        goto out;
    }

  retval = TRUE;

 out:

  return retval;
}

static gboolean
write_dir (MarkupDir  *dir,
	   FILE       *f,
	   int         indent,
	   gboolean    save_as_subtree,
	   const char *locale,
	   GHashTable *other_locales)
{
  GSList *tmp;
  gboolean retval = FALSE;

  dir->not_in_filesystem = TRUE;

  if (save_as_subtree && locale != NULL && dir->is_dir_empty)
    return TRUE;

  g_assert (dir->name != NULL);
  
  if (fprintf (f, "%s<dir name=\"%s\">\n",
	       make_whitespace (indent), dir->name) < 0)
    goto out;

  tmp = dir->entries;
  while (tmp != NULL)
    {
      MarkupEntry *entry = tmp->data;
      
      if (!write_entry (entry,
			f,
			indent + INDENT_SPACES,
			save_as_subtree,
			locale,
			other_locales))
	goto out;
        
      tmp = tmp->next;
    }

  tmp = dir->subdirs;
  while (tmp != NULL)
    {
      MarkupDir *subdir = tmp->data;
      
      if (!write_dir (subdir,
		      f,
		      indent + INDENT_SPACES,
		      save_as_subtree,
		      locale,
		      other_locales))
	goto out;
        
      tmp = tmp->next;
    }

  if (fprintf (f, "%s</dir>\n", make_whitespace (indent)) < 0)
    return FALSE;

  retval = TRUE;

 out:

  return retval;
}

static gboolean
init_is_dir_empty_flags (MarkupDir  *dir,
                         const char *locale)
{
  GSList *tmp;

  dir->is_dir_empty = TRUE;

  tmp = dir->entries;
  while (tmp != NULL)
    {
      MarkupEntry *entry = tmp->data;

      if (get_local_schema_info (entry, locale) != NULL)
        {
          dir->is_dir_empty = FALSE;
          break;
        }

      tmp = tmp->next;
    }

  tmp = dir->subdirs;
  while (tmp != NULL)
    {
      MarkupDir *subdir = tmp->data;

      if (!init_is_dir_empty_flags (subdir, locale))
        dir->is_dir_empty = FALSE;

      tmp = tmp->next;
    }

  return dir->is_dir_empty;
}

static void
save_tree_with_locale (MarkupDir  *dir,
		       gboolean    save_as_subtree,
		       const char *locale,
		       GHashTable *other_locales,
		       guint       file_mode,
		       GError    **err)
{
  /* We save to a secondary file then copy over, to handle
   * out-of-disk-space robustly
   */
  FILE *f;
  int new_fd;
  char *filename;
  char *new_filename;
#ifdef G_OS_WIN32
  char *tmp_filename;
  gboolean target_renamed;
#endif
  char *err_str;
  gboolean write_failed;
  GSList *tmp;
  struct stat st;

  write_failed = FALSE;
  err_str = NULL;
  new_fd = -1;
  f = NULL;

  filename = markup_dir_build_file_path (dir, save_as_subtree, locale);
  
  new_filename = g_strconcat (filename, ".new", NULL);
#ifdef G_OS_WIN32
  tmp_filename = g_strconcat (filename, ".tmp", NULL);
#endif
  new_fd = g_open (new_filename, O_WRONLY | O_CREAT, file_mode);
  if (new_fd < 0)
    {
      err_str = g_strdup_printf (_("Failed to open \"%s\": %s\n"),
                                 new_filename, g_strerror (errno));
      goto out;
    }

  /* Leave the file empty to avoid parsing it later
   * if there are no entries in it.
   */
  if (dir->entries == NULL && (!save_as_subtree || dir->subdirs == NULL))
    {
      fsync (new_fd);
      close (new_fd);
      new_fd = -1;
      goto done_writing;
    }
  
  f = fdopen (new_fd, "w");
  if (f == NULL)
    {
      err_str = g_strdup_printf (_("Failed to open \"%s\": %s\n"),
                                 new_filename, g_strerror (errno));
      goto out;
    }

  new_fd = -1; /* owned by the FILE* now */

  write_failed = FALSE;

  if (fputs ("<?xml version=\"1.0\"?>\n", f) < 0)
    {
      write_failed = TRUE;
      goto done_writing;
    }
  
  if (fputs ("<gconf>\n", f) < 0)
    {
      write_failed = TRUE;
      goto done_writing;
    }

    
  tmp = dir->entries;
  while (tmp != NULL)
    {
      MarkupEntry *entry = tmp->data;
      
      if (!write_entry (entry,
			f,
			INDENT_SPACES,
			save_as_subtree,
			locale,
			other_locales))
	{
	  write_failed = TRUE;
	  goto done_writing;
	}
        
      tmp = tmp->next;
    }

  if (save_as_subtree)
    {
      if (locale != NULL)
        init_is_dir_empty_flags (dir, locale);

      tmp = dir->subdirs;
      while (tmp != NULL)
	{
	  MarkupDir *dir = tmp->data;

	  if (!write_dir (dir,
			  f,
			  INDENT_SPACES,
			  save_as_subtree,
			  locale,
			  other_locales))
	    {
	      write_failed = TRUE;
	      goto done_writing;
	    }

	  tmp = tmp->next;
	}
    }

  if (fputs ("</gconf>\n", f) < 0)
    {
      write_failed = TRUE;
      goto done_writing;
    }

  if (fflush (f) != 0 || fsync (fileno (f)) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Could not flush file '%s' to disk: %s"),
                 new_filename, g_strerror (errno));
    }

  if (fclose (f) < 0)
    {
      f = NULL; /* f is still freed even if fclose fails according to the
                 * linux man page
                 */
      write_failed = TRUE;
      goto done_writing;
    }

  f = NULL;
  
 done_writing:
  
  if (write_failed)
    {
      err_str = g_strdup_printf (_("Error writing file \"%s\": %s"),
                                 new_filename, g_strerror (errno));
      goto out;
    }
  
#ifdef G_OS_WIN32
  g_remove (tmp_filename);
  target_renamed = (g_rename (filename, tmp_filename) == 0);
#endif

#ifndef G_OS_WIN32
  if (g_stat (filename, &st) == 0) {
      /* Restore permissions. There is not much error checking we can do
       * here. The final data is saved anyways. Note the order:
       * mode, uid+gid, gid, uid, mode.
       */
      chmod (new_filename, st.st_mode);
      if (chown (new_filename, st.st_uid, st.st_gid) < 0)
        {
          /* We cannot set both. Maybe we can set one.  */
          chown (new_filename, -1, st.st_gid);
          chown (new_filename, st.st_uid, -1);
        }
        chmod (new_filename, st.st_mode);
    }
#endif 

  if (g_rename (new_filename, filename) < 0)
    {
      err_str = g_strdup_printf (_("Failed to move temporary file \"%s\" to final location \"%s\": %s"),                                 
                                 new_filename, filename, g_strerror (errno));
#ifdef G_OS_WIN32
      if (target_renamed)
	g_rename (tmp_filename, filename);
#endif
      goto out;
    }

#ifdef G_OS_WIN32
  if (target_renamed)
    g_remove (tmp_filename);
#endif
  
 out:
#ifdef G_OS_WIN32
  g_free (tmp_filename);
#endif
  g_free (new_filename);
  g_free (filename);
  
  if (err_str)
    {
      if (err)
        *err = g_error_new_literal (GCONF_ERROR,
                                    GCONF_ERROR_FAILED,
                                    err_str);

      g_free (err_str);
    }
  
  if (new_fd >= 0)
    close (new_fd);

  if (f != NULL)
    fclose (f);
}

typedef struct
{
  MarkupDir *dir;
  guint file_mode;
  GError *first_error;
} OtherLocalesForeachData;

static void
other_locales_foreach (const char *locale,
		       gpointer    dummy,
		       OtherLocalesForeachData *data)
{
  GError *error;

  error = NULL;
  save_tree_with_locale (data->dir,
                         TRUE,
                         locale,
                         NULL,
                         data->file_mode,
                         &error);
  if (error != NULL)
    {
      if (data->first_error != NULL)
        data->first_error = error;
      else
        g_error_free (error);
    }
}

static void
save_tree (MarkupDir  *dir,
	   gboolean    save_as_subtree,
	   guint       file_mode,
	   GError    **err)
{
  if (!save_as_subtree)
    {
      save_tree_with_locale (dir, FALSE, NULL, NULL, file_mode, err);
    }
  else
    {
      OtherLocalesForeachData other_locales_foreach_data;
      GHashTable *other_locales;

      /* First save %gconf-tree.xml with all values and C locale
       * schema descriptions; then save schema descriptions for
       * all other locales in %gconf-tree-$(locale).xml
       */

      other_locales = g_hash_table_new (g_str_hash, g_str_equal);

      save_tree_with_locale (dir,
                             TRUE,
                             NULL,
                             other_locales,
                             file_mode,
                             err);

      other_locales_foreach_data.dir         = dir;
      other_locales_foreach_data.file_mode   = file_mode;
      other_locales_foreach_data.first_error = NULL;

      g_hash_table_foreach (other_locales,
                            (GHFunc) other_locales_foreach,
                            &other_locales_foreach_data);

      if (other_locales_foreach_data.first_error != NULL)
        {
          if (err != NULL && *err == NULL)
            *err = other_locales_foreach_data.first_error;
          else
            g_error_free (other_locales_foreach_data.first_error);
        }

      g_hash_table_destroy (other_locales);
    }
}

/*
 * Local schema
 */

static LocalSchemaInfo*
local_schema_info_new (void)
{
  LocalSchemaInfo *info;

  info = g_new0 (LocalSchemaInfo, 1);

  return info;
}

static void
local_schema_info_free (LocalSchemaInfo *info)
{
  g_free (info->locale);
  g_free (info->short_desc);
  g_free (info->long_desc);
  if (info->default_value)
    gconf_value_free (info->default_value);
  g_free (info);
}
