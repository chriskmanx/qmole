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

#ifndef __GKM_STORE_H__
#define __GKM_STORE_H__

#include <glib-object.h>

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

enum {
	GKM_STORE_IS_INTERNAL = 0x01,
	GKM_STORE_IS_SENSITIVE = 0x02
};

#define GKM_TYPE_STORE               (gkm_store_get_type ())
#define GKM_STORE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_STORE, GkmStore))
#define GKM_STORE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_STORE, GkmStoreClass))
#define GKM_IS_STORE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_STORE))
#define GKM_IS_STORE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_STORE))
#define GKM_STORE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_STORE, GkmStoreClass))

typedef struct _GkmStoreClass GkmStoreClass;
typedef struct _GkmStorePrivate GkmStorePrivate;

struct _GkmStore {
	GObject parent;
	GkmStorePrivate *pv;
};

struct _GkmStoreClass {
	GObjectClass parent_class;

	/* Virtual methods */

	CK_RV (*read_value) (GkmStore *self, GkmObject *object, CK_ATTRIBUTE_PTR attr);

	void (*write_value) (GkmStore *self, GkmTransaction *transaction, GkmObject *object, CK_ATTRIBUTE_PTR attr);
};

typedef CK_RV         (*GkmStoreValidator)                (GkmObject *object,
                                                           CK_ATTRIBUTE_PTR attr);

GType                 gkm_store_get_type                  (void);

gboolean              gkm_store_lookup_schema             (GkmStore *self,
                                                           CK_ATTRIBUTE_TYPE type,
                                                           guint *flags);

void                  gkm_store_register_schema           (GkmStore *self,
                                                           CK_ATTRIBUTE_PTR type_and_default,
                                                           GkmStoreValidator validator,
                                                           guint flags);

void                  gkm_store_set_attribute             (GkmStore *self,
                                                           GkmTransaction *transaction,
                                                           GkmObject *object,
                                                           CK_ATTRIBUTE_PTR attr);

void                  gkm_store_write_value               (GkmStore *self,
                                                           GkmTransaction *transaction,
                                                           GkmObject *object,
                                                           CK_ATTRIBUTE_PTR attr);

CK_RV                 gkm_store_get_attribute             (GkmStore *self,
                                                           GkmObject *object,
                                                           CK_ATTRIBUTE_PTR attr);

gconstpointer         gkm_store_read_value                (GkmStore *self,
                                                           GkmObject *object,
                                                           CK_ATTRIBUTE_TYPE type,
                                                           gsize *n_value);

gchar*                gkm_store_read_string               (GkmStore *self,
                                                           GkmObject *object,
                                                           CK_ATTRIBUTE_TYPE type);

void                  gkm_store_notify_attribute          (GkmStore *self,
                                                           GkmObject *object,
                                                           CK_ATTRIBUTE_TYPE type);

#endif /* __GKM_STORE_H__ */
