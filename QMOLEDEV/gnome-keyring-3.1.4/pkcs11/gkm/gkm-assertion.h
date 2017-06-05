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

#ifndef __GKM_ASSERTION_H__
#define __GKM_ASSERTION_H__

#include <glib-object.h>

#include "gkm-object.h"
#include "gkm-types.h"

#define GKM_FACTORY_ASSERTION            (gkm_assertion_get_factory ())
#define GKM_TYPE_ASSERTION               (gkm_assertion_get_type ())
#define GKM_ASSERTION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_ASSERTION, GkmAssertion))
#define GKM_ASSERTION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_ASSERTION, GkmAssertionClass))
#define GKM_IS_ASSERTION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_ASSERTION))
#define GKM_IS_ASSERTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_ASSERTION))
#define GKM_ASSERTION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_ASSERTION, GkmAssertionClass))

typedef struct _GkmAssertionClass GkmAssertionClass;
typedef struct _GkmAssertionPrivate GkmAssertionPrivate;

struct _GkmAssertion {
	GkmObject parent;
	GkmAssertionPrivate *pv;
};

struct _GkmAssertionClass {
	GkmObjectClass parent_class;
};

GType                 gkm_assertion_get_type               (void);

GkmAssertion*         gkm_assertion_new                    (GkmTrust *trust,
                                                            gulong trust_type,
                                                            const gchar *purpose,
                                                            const gchar *remote);

const gchar*          gkm_assertion_get_purpose            (GkmAssertion *self);

const gchar*          gkm_assertion_get_peer               (GkmAssertion *self);

gulong                gkm_assertion_get_trust_type         (GkmAssertion *self);

GkmTrust*             gkm_assertion_get_trust_object       (GkmAssertion *self);

#endif /* __GKM_ASSERTION_H__ */
