/*
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#if !defined (__GCR_H_INSIDE__) && !defined (GCR_COMPILATION)
#error "Only <gcr/gcr.h> can be included directly."
#endif

#ifndef GCR_GNUPG_KEY_H
#define GCR_GNUPG_KEY_H

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-column.h"
#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_GNUPG_KEY_COLUMNS            (_gcr_gnupg_key_get_columns ())
#define GCR_TYPE_GNUPG_KEY               (_gcr_gnupg_key_get_type ())
#define GCR_GNUPG_KEY(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_GNUPG_KEY, GcrGnupgKey))
#define GCR_GNUPG_KEY_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_GNUPG_KEY, GcrGnupgKeyClass))
#define GCR_IS_GNUPG_KEY(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_GNUPG_KEY))
#define GCR_IS_GNUPG_KEY_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_GNUPG_KEY))
#define GCR_GNUPG_KEY_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_GNUPG_KEY, GcrGnupgKeyClass))

typedef struct _GcrGnupgKey GcrGnupgKey;
typedef struct _GcrGnupgKeyClass GcrGnupgKeyClass;
typedef struct _GcrGnupgKeyPrivate GcrGnupgKeyPrivate;

struct _GcrGnupgKey {
	GObject parent;

	/*< private >*/
	GcrGnupgKeyPrivate *pv;
};

struct _GcrGnupgKeyClass {
	GObjectClass parent_class;
};

GType               _gcr_gnupg_key_get_type                      (void);

const GcrColumn*    _gcr_gnupg_key_get_columns                   (void);

GcrGnupgKey*        _gcr_gnupg_key_new                           (GPtrArray *pubset,
                                                                  GPtrArray *secset);

const gchar*        _gcr_gnupg_key_get_keyid                     (GcrGnupgKey *self);

GPtrArray*          _gcr_gnupg_key_get_public_records            (GcrGnupgKey *self);

void                _gcr_gnupg_key_set_public_records            (GcrGnupgKey *self,
                                                                  GPtrArray *records);

GPtrArray*          _gcr_gnupg_key_get_secret_records            (GcrGnupgKey *self);

void                _gcr_gnupg_key_set_secret_records            (GcrGnupgKey *self,
                                                                  GPtrArray *records);

GIcon*              _gcr_gnupg_key_get_icon                      (GcrGnupgKey *self);

const gchar*        _gcr_gnupg_key_get_keyid_for_records         (GPtrArray *records);

const gchar*        _gcr_gnupg_key_get_fingerprint_for_records   (GPtrArray *records);

G_END_DECLS

#endif /* __GCR_GNUPG_KEY_H__ */
