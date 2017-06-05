/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __GKM_DATA_FILE_H__
#define __GKM_DATA_FILE_H__

#include <glib-object.h>

#include "gkm-data-types.h"
#include "gkm-secret.h"

enum {
	GKM_DATA_FILE_SECTION_PUBLIC  = 0x01,
	GKM_DATA_FILE_SECTION_PRIVATE = 0x02,
};

#define GKM_TYPE_DATA_FILE               (gkm_data_file_get_type ())
#define GKM_DATA_FILE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_DATA_FILE, GkmDataFile))
#define GKM_DATA_FILE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_DATA_FILE, GkmDataFileClass))
#define GKM_IS_DATA_FILE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_DATA_FILE))
#define GKM_IS_DATA_FILE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_DATA_FILE))
#define GKM_DATA_FILE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_DATA_FILE, GkmDataFileClass))

typedef struct _GkmDataFile GkmDataFile;
typedef struct _GkmDataFileClass GkmDataFileClass;

struct _GkmDataFileClass {
	GObjectClass parent_class;

	/* signals */

	void (*entry_added) (GkmDataFile *store, const gchar *identifier);

	void (*entry_changed) (GkmDataFile *store, const gchar *identifier, CK_ATTRIBUTE_TYPE type);

	void (*entry_removed) (GkmDataFile *store, const gchar *identifier);
};

typedef void (*GkmDataFileFunc) (GkmDataFile *file, const gchar *identifier, gpointer user_data);

GType                     gkm_data_file_get_type               (void);

GkmDataFile*              gkm_data_file_new                    (void);

GkmDataResult             gkm_data_file_read_fd                (GkmDataFile *self,
                                                                int fd,
                                                                GkmSecret *login);

GkmDataResult             gkm_data_file_write_fd               (GkmDataFile *self,
                                                                int fd,
                                                                GkmSecret *login);

gboolean                  gkm_data_file_have_section           (GkmDataFile *self,
                                                                guint section);

gboolean                  gkm_data_file_lookup_entry           (GkmDataFile *self,
                                                                const gchar *identifier,
                                                                guint *section);

void                      gkm_data_file_foreach_entry          (GkmDataFile *self,
                                                                GkmDataFileFunc func,
                                                                gpointer user_data);

GkmDataResult             gkm_data_file_unique_entry           (GkmDataFile *self,
                                                                gchar **identifier);

GkmDataResult             gkm_data_file_create_entry           (GkmDataFile *self,
                                                                const gchar *identifier,
                                                                guint section);

GkmDataResult             gkm_data_file_destroy_entry          (GkmDataFile *self,
                                                                const gchar *identifier);

GkmDataResult             gkm_data_file_write_value            (GkmDataFile *self,
                                                                const gchar *identifier,
                                                                gulong type,
                                                                gconstpointer value,
                                                                gsize n_value);

GkmDataResult             gkm_data_file_read_value             (GkmDataFile *self,
                                                                const gchar *identifier,
                                                                gulong type,
                                                                gconstpointer *value,
                                                                gsize *n_value);

void                      gkm_data_file_foreach_value          (GkmDataFile *self,
                                                                const gchar *identifier);

void                      gkm_data_file_dump                   (GkmDataFile *self);

#endif /* __GKM_DATA_FILE_H__ */
