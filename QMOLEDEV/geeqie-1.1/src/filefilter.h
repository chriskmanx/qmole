/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef FILEFILTER_H
#define FILEFILTER_H


typedef struct _FilterEntry FilterEntry;
struct _FilterEntry {
	gchar *key;
	gchar *description;
	gchar *extensions;
	FileFormatClass file_class;
	gboolean enabled;
	gboolean writable;
	gboolean allow_sidecar;
};

/* you can change, but not add or remove entries from the returned list */
GList *filter_get_list(void);
void filter_remove_entry(FilterEntry *fe);

void filter_add(const gchar *key, const gchar *description, const gchar *extensions, FileFormatClass file_class, gboolean writable, gboolean allow_sidecar, gboolean enabled);
void filter_add_unique(const gchar *description, const gchar *extensions, FileFormatClass file_class, gboolean writable, gboolean allow_sidecar, gboolean enabled);
void filter_add_defaults(void);
void filter_reset(void);
void filter_rebuild(void);
GList *filter_to_list(const gchar *extensions);

const gchar *registered_extension_from_path(const gchar *name);
gboolean filter_name_exists(const gchar *name);
gboolean filter_file_class(const gchar *name, FileFormatClass file_class);
gboolean filter_name_is_writable(const gchar *name);
gboolean filter_name_allow_sidecar(const gchar *name);

void filter_write_list(GString *outstr, gint indent);
void filter_load_file_type(const gchar **attribute_names, const gchar **attribute_values);


void sidecar_ext_parse(const gchar *text);
gchar *sidecar_ext_to_string(void);
GList *sidecar_ext_get_list(void);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
