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

#ifndef __GKM_PUBLIC_XSA_KEY_H__
#define __GKM_PUBLIC_XSA_KEY_H__

#include <glib-object.h>

#include "gkm-sexp-key.h"
#include "gkm-types.h"

#define GKM_FACTORY_PUBLIC_XSA_KEY            (gkm_public_xsa_key_get_factory ())

#define GKM_TYPE_PUBLIC_XSA_KEY               (gkm_public_xsa_key_get_type ())
#define GKM_PUBLIC_XSA_KEY(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_PUBLIC_XSA_KEY, GkmPublicXsaKey))
#define GKM_PUBLIC_XSA_KEY_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_PUBLIC_XSA_KEY, GkmPublicXsaKeyClass))
#define GKM_IS_PUBLIC_XSA_KEY(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_PUBLIC_XSA_KEY))
#define GKM_IS_PUBLIC_XSA_KEY_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_PUBLIC_XSA_KEY))
#define GKM_PUBLIC_XSA_KEY_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_PUBLIC_XSA_KEY, GkmPublicXsaKeyClass))

typedef struct _GkmPublicXsaKeyClass GkmPublicXsaKeyClass;
typedef struct _GkmPublicXsaKeyPrivate GkmPublicXsaKeyPrivate;

struct _GkmPublicXsaKey {
	GkmSexpKey parent;
	GkmPublicXsaKeyPrivate *pv;
};

struct _GkmPublicXsaKeyClass {
	GkmSexpKeyClass parent_class;
};

GType                     gkm_public_xsa_key_get_type           (void);

GkmFactory*               gkm_public_xsa_key_get_factory        (void);

GkmSexp*                  gkm_public_xsa_key_create_sexp        (GkmSession *session,
                                                                 GkmTransaction *transaction,
                                                                 CK_ATTRIBUTE_PTR attrs,
                                                                 CK_ULONG n_attrs);

#endif /* __GKM_PUBLIC_XSA_KEY_H__ */
