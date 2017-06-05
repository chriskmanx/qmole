/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 * Copyright (C) 2010 Collabora Ltd
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

#ifndef __GKM_XDG_ASSERTION_H__
#define __GKM_XDG_ASSERTION_H__

#include <glib-object.h>

#include "gkm/gkm-assertion.h"

#define GKM_XDG_FACTORY_ASSERTION            (gkm_xdg_assertion_get_factory ())
#define GKM_XDG_TYPE_ASSERTION               (gkm_xdg_assertion_get_type ())
#define GKM_XDG_ASSERTION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_XDG_TYPE_ASSERTION, GkmXdgAssertion))
#define GKM_XDG_ASSERTION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_XDG_TYPE_ASSERTION, GkmXdgAssertionClass))
#define GKM_XDG_IS_ASSERTION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_XDG_TYPE_ASSERTION))
#define GKM_XDG_IS_ASSERTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_XDG_TYPE_ASSERTION))
#define GKM_XDG_ASSERTION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_XDG_TYPE_ASSERTION, GkmXdgAssertionClass))

typedef struct _GkmXdgAssertion GkmXdgAssertion;
typedef struct _GkmXdgAssertionClass GkmXdgAssertionClass;
typedef struct _GkmXdgAssertionPrivate GkmXdgAssertionPrivate;

struct _GkmXdgAssertion {
	GkmAssertion parent;
	GkmXdgAssertionPrivate *pv;
};

struct _GkmXdgAssertionClass {
	GkmAssertionClass parent_class;
};

GType                 gkm_xdg_assertion_get_type               (void);

GkmFactory*           gkm_xdg_assertion_get_factory            (void);

#endif /* __GKM_XDG_ASSERTION_H__ */
