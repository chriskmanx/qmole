/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#ifndef __GKM_XDG_TRUST_H__
#define __GKM_XDG_TRUST_H__

#include <glib-object.h>

#include "gkm/gkm-object.h"
#include "gkm/gkm-trust.h"

#define GKM_XDG_FACTORY_TRUST            (gkm_xdg_trust_get_factory ())
#define GKM_XDG_TYPE_TRUST               (gkm_xdg_trust_get_type ())
#define GKM_XDG_TRUST(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_XDG_TYPE_TRUST, GkmXdgTrust))
#define GKM_XDG_TRUST_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_XDG_TYPE_TRUST, GkmXdgTrustClass))
#define GKM_XDG_IS_TRUST(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_XDG_TYPE_TRUST))
#define GKM_XDG_IS_TRUST_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_XDG_TYPE_TRUST))
#define GKM_XDG_TRUST_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_XDG_TYPE_TRUST, GkmXdgTrustClass))

typedef struct _GkmXdgTrust GkmXdgTrust;
typedef struct _GkmXdgTrustClass GkmXdgTrustClass;
typedef struct _GkmXdgTrustPrivate GkmXdgTrustPrivate;

struct _GkmXdgTrust {
	GkmTrust parent;
	GkmXdgTrustPrivate *pv;
};

struct _GkmXdgTrustClass {
	GkmTrustClass parent_class;
};

GType                 gkm_xdg_trust_get_type               (void);

GkmXdgTrust*          gkm_xdg_trust_create_for_assertion   (GkmModule *module,
                                                            GkmManager *manager,
                                                            GkmTransaction *transaction,
                                                            CK_ATTRIBUTE_PTR attrs,
                                                            CK_ULONG n_attrs);

void                  gkm_xdg_trust_replace_assertion      (GkmXdgTrust *trust,
                                                            GkmAssertion *assertion,
                                                            GkmTransaction *transaction);

void                  gkm_xdg_trust_remove_assertion       (GkmXdgTrust *trust,
                                                            GkmAssertion *assertion,
                                                            GkmTransaction *transaction);

gboolean              gkm_xdg_trust_have_assertion         (GkmXdgTrust *trust);

#endif /* __GKM_XDG_TRUST_H__ */
