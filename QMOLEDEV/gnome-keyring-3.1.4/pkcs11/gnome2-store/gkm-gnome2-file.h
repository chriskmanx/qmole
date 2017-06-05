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

#ifndef __GKM_GNOME2_FILE_H__
#define __GKM_GNOME2_FILE_H__

#include <glib-object.h>

#include "gkm/gkm-data-types.h"
#include "gkm/gkm-secret.h"

enum {
	GKM_GNOME2_FILE_SECTION_PUBLIC  = 0x01,
	GKM_GNOME2_FILE_SECTION_PRIVATE = 0x02,
};

#define GKM_TYPE_GNOME2_FILE               (gkm_gnome2_file_get_type ())
#define GKM_GNOME2_FILE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_GNOME2_FILE, GkmGnome2File))
#define GKM_GNOME2_FILE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_GNOME2_FILE, GkmGnome2FileClass))
#define GKM_IS_GNOME2_FILE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_GNOME2_FILE))
#define GKM_IS_GNOME2_FILE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_GNOME2_FILE))
#define GKM_GNOME2_FILE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_GNOME2_FILE, GkmGnome2FileClass))

typedef struct _GkmGnome2File GkmGnome2File;
typedef struct _GkmGnome2FileClass GkmGnome2FileClass;

struct _GkmGnome2FileClass {
	GObjectClass parent_class;

	/* signals */

	void (*entry_added) (GkmGnome2File *store, const gchar *identifier);

	void (*entry_changed) (GkmGnome2File *store, const gchar *identifier, CK_ATTRIBUTE_TYPE type);

	void (*entry_removed) (GkmGnome2File *store, const gchar *identifier);
};

typedef void (*GkmGnome2FileFunc) (GkmGnome2File *file, const gchar *identifier, gpointer user_data);

GType                     gkm_gnome2_file_get_type               (void);

GkmGnome2File*            gkm_gnome2_file_new                    (void);

GkmDataResult             gkm_gnome2_file_read_fd                (GkmGnome2File *self,
                                                                  int fd,
                                                                  GkmSecret *login);

GkmDataResult             gkm_gnome2_file_write_fd               (GkmGnome2File *self,
                                                                  int fd,
                                                                  GkmSecret *login);

gboolean                  gkm_gnome2_file_have_section           (GkmGnome2File *self,
                                                                  guint section);

gboolean                  gkm_gnome2_file_lookup_entry           (GkmGnome2File *self,
                                                                  const gchar *identifier,
                                                                  guint *section);

void                      gkm_gnome2_file_foreach_entry          (GkmGnome2File *self,
                                                                  GkmGnome2FileFunc func,
                                                                  gpointer user_data);

GkmDataResult             gkm_gnome2_file_unique_entry           (GkmGnome2File *self,
                                                                  gchar **identifier);

GkmDataResult             gkm_gnome2_file_create_entry           (GkmGnome2File *self,
                                                                  const gchar *identifier,
                                                                  guint section);

GkmDataResult             gkm_gnome2_file_destroy_entry          (GkmGnome2File *self,
                                                                  const gchar *identifier);

GkmDataResult             gkm_gnome2_file_write_value            (GkmGnome2File *self,
                                                                  const gchar *identifier,
                                                                  gulong type,
                                                                  gconstpointer value,
                                                                  gsize n_value);

GkmDataResult             gkm_gnome2_file_read_value             (GkmGnome2File *self,
                                                                  const gchar *identifier,
                                                                  gulong type,
                                                                  gconstpointer *value,
                                                                  gsize *n_value);

void                      gkm_gnome2_file_foreach_value          (GkmGnome2File *self,
                                                                  const gchar *identifier);

void                      gkm_gnome2_file_dump                   (GkmGnome2File *self);

#endif /* __GKM_GNOME2_FILE_H__ */
