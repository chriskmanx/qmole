/* GConf
 * Copyright (C) 2002 Red Hat Inc.
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

#ifndef MARKUP_TREE_H
#define MARKUP_TREE_H

#include <glib.h>
#include "gconf/gconf-value.h"

typedef struct _MarkupTree  MarkupTree;
typedef struct _MarkupDir   MarkupDir;
typedef struct _MarkupEntry MarkupEntry;

/* Tree */

MarkupTree* markup_tree_get        (const char *root_dir,
                                    guint       dir_mode,
                                    guint       file_mode,
                                    gboolean    merged);
void        markup_tree_unref      (MarkupTree *tree);
void        markup_tree_rebuild    (MarkupTree *tree);
MarkupDir*  markup_tree_lookup_dir (MarkupTree *tree,
                                    const char *full_key,
                                    GError    **err);
MarkupDir*  markup_tree_ensure_dir (MarkupTree *tree,
                                    const char *full_key,
                                    GError    **err);

gboolean    markup_tree_sync       (MarkupTree *tree,
                                    GError    **err);

/* Directories in the tree */

MarkupEntry* markup_dir_lookup_entry  (MarkupDir   *dir,
                                       const char  *relative_key,
                                       GError     **err);
MarkupEntry* markup_dir_ensure_entry  (MarkupDir   *dir,
                                       const char  *relative_key,
                                       GError     **err);
MarkupDir*   markup_dir_lookup_subdir (MarkupDir   *dir,
                                       const char  *relative_key,
                                       GError     **err);
MarkupDir*   markup_dir_ensure_subdir (MarkupDir   *dir,
                                       const char  *relative_key,
                                       GError     **err);
GSList*      markup_dir_list_entries  (MarkupDir   *dir,
                                       GError     **err);
GSList*      markup_dir_list_subdirs  (MarkupDir   *dir,
                                       GError     **err);
const char*  markup_dir_get_name      (MarkupDir   *dir);

/* Value entries in the directory */
/* get_value returns a newly-generated GConfValue, caller owns it */
GConfValue* markup_entry_get_value       (MarkupEntry       *entry,
                                          const char       **locales);
void        markup_entry_set_value       (MarkupEntry       *entry,
                                          const GConfValue  *value);
void        markup_entry_unset_value     (MarkupEntry       *entry,
                                          const char        *locale);
void        markup_entry_set_schema_name (MarkupEntry       *entry,
                                          const char        *schema_name);
const char* markup_entry_get_name        (MarkupEntry       *entry);
const char* markup_entry_get_schema_name (MarkupEntry       *entry);
const char* markup_entry_get_mod_user    (MarkupEntry       *entry);
GTime       markup_entry_get_mod_time    (MarkupEntry       *entry);

#endif
