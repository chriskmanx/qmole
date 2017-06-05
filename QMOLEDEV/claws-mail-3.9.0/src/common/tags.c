/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2012 The Claws Mail Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#include <signal.h>
#include <unistd.h>

#include "defs.h"
#include "utils.h"
#include "tags.h"

static GHashTable *tags_table = NULL;
static GHashTable *tags_reverse_table = NULL;

static int tag_max_id = 0;

void tags_read_tags(void)
{
	gchar *file = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			TAGS_RC, NULL);
	gchar tmp[255];
	gint id;
	FILE *fp = g_fopen(file, "rb");
	
	g_free(file);

	if (tags_table == NULL)
		tags_table = g_hash_table_new_full(
				g_direct_hash, g_direct_equal,
				NULL, g_free);
	if (tags_reverse_table == NULL)
		tags_reverse_table = g_hash_table_new_full(
				g_str_hash, g_str_equal,
				g_free, NULL);
		
	if (!fp) 
		return;
	if (fscanf(fp, "max_id %d\n", &tag_max_id) != 1) {
		fclose(fp);
		return;
	}
	while (fgets(tmp, sizeof(tmp), fp) != NULL) {
		gchar *sep = strchr(tmp, '\t');
		gchar *tag_name = sep?(sep+1):NULL;
		
		if (!tag_name || !sep)
			continue;
		g_strstrip(tag_name);
		*(sep) = '\0';
		if (IS_NOT_RESERVED_TAG(tag_name)) {
			id = atoi(tmp);
			g_hash_table_insert(tags_table,
					    GINT_TO_POINTER(id), g_strdup(tag_name));
			g_hash_table_insert(tags_reverse_table,
					    g_strdup(tag_name), GINT_TO_POINTER(id));
		}
	}
	
	fclose(fp);
}

typedef struct _TagWriteData
{
	FILE *fp;
	gboolean error;
} TagWriteData;

static void tag_write(gpointer key, gpointer value, gpointer user_data)
{
	TagWriteData *data = (TagWriteData *)user_data;
	const gchar *str = value;
	gint id = GPOINTER_TO_INT(key);

	if (data->error)
		return;
		
	if (fprintf(data->fp, "%d\t%s\n", id, str) <= 0) {
		FILE_OP_ERROR("tagsrc", "fprintf");
		data->error = TRUE;
	}
}

void tags_write_tags(void)
{
	gchar *file = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			TAGS_RC, ".tmp", NULL);
	gchar *file_new = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			TAGS_RC, NULL);
	TagWriteData data;

	FILE *fp = g_fopen(file, "wb");
			
	if (!fp) {
		FILE_OP_ERROR(file, "g_fopen");
		g_free(file);
		g_free(file_new);
		return;
	}
	
	data.fp = fp;
	data.error = FALSE;

	if (fprintf(data.fp, "max_id %d\n", tag_max_id) <= 0) {
		FILE_OP_ERROR("tagsrc", "fprintf");
		data.error = TRUE;
	} else {
		g_hash_table_foreach(tags_table, tag_write, &data);
	}

	if (data.error) {
		fclose(fp);
		g_free(file);
		g_free(file_new);
		return;
	}
	
	if (fclose(fp) == EOF) {
		FILE_OP_ERROR(file, "fclose");
		g_free(file);
		g_free(file_new);
		return;
	}

	if (rename_force(file, file_new) < 0) {
		FILE_OP_ERROR(file, "rename_force");
	}

	g_free(file);
	g_free(file_new);
}

gint tags_add_tag(const gchar *tag)
{
	if (!tag || !(*tag))
		return -1;

	if (g_hash_table_lookup(tags_reverse_table, tag))
		return -1;

	if (IS_NOT_RESERVED_TAG(tag)) {
		tag_max_id++;
		g_hash_table_insert(tags_table, GINT_TO_POINTER(tag_max_id), 
			g_strdup(tag));
		g_hash_table_insert(tags_reverse_table, g_strdup(tag),
			GINT_TO_POINTER(tag_max_id));

		return tag_max_id;
	} else {
		return -1;
	}
}

void tags_remove_tag(gint id)
{
	gchar *old_tag = g_hash_table_lookup(tags_table, GINT_TO_POINTER(id));

	if (old_tag) {
		g_hash_table_remove(tags_reverse_table, old_tag);
	}
	g_hash_table_remove(tags_table, GINT_TO_POINTER(id));
}

/* extern decl. to avoid including ../prefs_filtering.h */
extern void prefs_filtering_rename_tag(const gchar *old_tag, const gchar *new_tag);

void tags_update_tag(gint id, const gchar *tag)
{
	gchar *old_tag = g_hash_table_lookup(tags_table, GINT_TO_POINTER(id));

	if (IS_NOT_RESERVED_TAG(tag)) {
		if (old_tag) {
			prefs_filtering_rename_tag(old_tag, tag);
			g_hash_table_remove(tags_reverse_table, old_tag);
		}

		g_hash_table_replace(tags_table, GINT_TO_POINTER(id), 
			g_strdup(tag));
		g_hash_table_insert(tags_reverse_table, g_strdup(tag),
			GINT_TO_POINTER(id));
	}
}

const gchar *tags_get_tag(gint id)
{
	return (const gchar *)g_hash_table_lookup(tags_table,
				GINT_TO_POINTER(id));
}

gint tags_get_id_for_str(const gchar *str)
{
	gpointer id_ptr;
	if ((id_ptr = g_hash_table_lookup(tags_reverse_table, str)) != NULL)
		return GPOINTER_TO_INT(id_ptr);
	else
		return -1;
}

typedef struct _TagListData {
	GSList *list;
} TagListData;

static void tag_add_list(gpointer key, gpointer value, gpointer user_data)
{
	TagListData *data = (TagListData *)user_data;

	data->list = g_slist_prepend(data->list, GINT_TO_POINTER(key));
}

GSList *tags_get_list(void)
{
	TagListData data;
	data.list = NULL;

	g_hash_table_foreach(tags_table, tag_add_list, &data);

	data.list = g_slist_reverse(data.list);
	
	return data.list;
}

guint tags_get_size(void)
{
	return g_hash_table_size(tags_table);
}
