/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef __MOCK_LOCKED_OBJECT_H__
#define __MOCK_LOCKED_OBJECT_H__

#include <glib-object.h>

#include "gkm/gkm-object.h"
#include "gkm/gkm-types.h"

#define MOCK_TYPE_LOCKED_OBJECT               (mock_locked_object_get_type ())
#define MOCK_LOCKED_OBJECT(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), MOCK_TYPE_LOCKED_OBJECT, MockLockedObject))
#define MOCK_LOCKED_OBJECT_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), MOCK_TYPE_LOCKED_OBJECT, MockLockedObjectClass))
#define MOCK_IS_LOCKED_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), MOCK_TYPE_LOCKED_OBJECT))
#define MOCK_IS_LOCKED_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), MOCK_TYPE_LOCKED_OBJECT))
#define MOCK_LOCKED_OBJECT_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), MOCK_TYPE_LOCKED_OBJECT, MockLockedObjectClass))

typedef struct _MockLockedObject MockLockedObject;
typedef struct _MockLockedObjectClass MockLockedObjectClass;

struct _MockLockedObject {
	GkmObject parent;
};

struct _MockLockedObjectClass {
	GkmObjectClass parent_class;
};

GType                      mock_locked_object_get_type               (void);

GkmObject*                 mock_locked_object_new                    (GkmModule *module, GkmManager *manager);

#endif /* __MOCK_LOCKED_OBJECT_H__ */
