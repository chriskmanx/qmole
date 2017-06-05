/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __GKM_MANAGER_H__
#define __GKM_MANAGER_H__

#include <gcrypt.h>
#include <glib-object.h>

#include "gkm-object.h"

/*
 * GkmManager
 *
 * A GkmManager tracks a set of GkmObject objects. It does not own
 * those objects. Once an object is registered with the manager it gets
 * an identifier.
 *
 * An object will unregister itself from the manager when it is destroyed or
 * it can be done explicitely.
 *
 * A singleton GkmManager exists for token objects, those stored in
 * persistent storage. This manager lasts for the lifetime of the daemon.
 *
 * Other GkmManager objects can exist per client for session or
 * temporary objects. Multiple requests for a manager for the same client
 * will return the same manager. Once all references dissappear this
 * manager will go away.
 */

G_BEGIN_DECLS

#include <glib-object.h>

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#define GKM_TYPE_MANAGER             (gkm_manager_get_type ())
#define GKM_MANAGER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_MANAGER, GkmManager))
#define GKM_MANAGER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_MANAGER, GkmManager))
#define GKM_IS_MANAGER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_MANAGER))
#define GKM_IS_MANAGER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_MANAGER))
#define GKM_MANAGER_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_MANAGER, GkmManagerClass))

typedef struct _GkmManagerClass GkmManagerClass;
typedef struct _GkmManagerPrivate GkmManagerPrivate;

struct _GkmManager {
	 GObject parent;
	 GkmManagerPrivate *pv;
};

struct _GkmManagerClass {
	GObjectClass parent_class;

	/* signals */

	void (*object_added) (GkmManager *self, GkmObject *object);

	void (*object_removed) (GkmManager *self, GkmObject *object);

	void (*attribute_changed) (GkmManager *self, GkmObject *object, CK_ATTRIBUTE_TYPE type);
};

GType                   gkm_manager_get_type                    (void) G_GNUC_CONST;

GkmManager*             gkm_manager_for_template                (CK_ATTRIBUTE_PTR attrs,
                                                                 CK_ULONG n_attrs,
                                                                 GkmSession *session);

gboolean                gkm_manager_get_for_token               (GkmManager *self);

void                    gkm_manager_add_attribute_index         (GkmManager *self,
                                                                 CK_ATTRIBUTE_TYPE attr,
                                                                 gboolean unique);

void                    gkm_manager_add_property_index          (GkmManager *self,
                                                                 const gchar *property,
                                                                 gboolean unique);

GkmObject*              gkm_manager_find_by_handle              (GkmManager *self,
                                                                 CK_OBJECT_HANDLE obj);

GList*                  gkm_manager_find_by_number_property     (GkmManager *self,
                                                                 const gchar *property,
                                                                 gulong value);

GkmObject*              gkm_manager_find_one_by_number_property (GkmManager *self,
                                                                 const gchar *property,
                                                                 gulong value);

GList*                  gkm_manager_find_by_string_property     (GkmManager *self,
                                                                 const gchar *property,
                                                                 const gchar *value);

GkmObject*              gkm_manager_find_one_by_string_property (GkmManager *self,
                                                                 const gchar *property,
                                                                 const gchar *value);

GList*                  gkm_manager_find_by_attributes          (GkmManager *self,
                                                                 GkmSession *session,
                                                                 CK_ATTRIBUTE_PTR template,
                                                                 CK_ULONG n_attrs);

GList*                  gkm_manager_find_by_class               (GkmManager *self,
                                                                 GkmSession *session,
                                                                 CK_OBJECT_CLASS klass);

GkmObject*              gkm_manager_find_one_by_attributes      (GkmManager *self,
                                                                 GkmSession *session,
                                                                 CK_ATTRIBUTE_PTR template,
                                                                 CK_ULONG n_attrs);

GkmObject*              gkm_manager_find_related                (GkmManager *self,
                                                                 GkmSession *session,
                                                                 CK_OBJECT_CLASS klass,
                                                                 GkmObject *related_to);

CK_RV                   gkm_manager_find_handles                (GkmManager *self,
                                                                 GkmSession *session,
                                                                 gboolean include_private,
                                                                 CK_ATTRIBUTE_PTR template,
                                                                 CK_ULONG count,
                                                                 GArray *found);

G_END_DECLS

#endif /* __GKM_MANAGER_H__ */
