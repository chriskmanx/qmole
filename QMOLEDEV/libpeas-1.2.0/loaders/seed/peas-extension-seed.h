/*
 * peas-extension-seed.h
 * This file is part of libpeas
 *
 * Copyright (C) 2010 - Steve Frécinaux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __PEAS_EXTENSION_SEED_H__
#define __PEAS_EXTENSION_SEED_H__

#include <libpeas/peas-extension-wrapper.h>
#include <seed.h>

G_BEGIN_DECLS

#define PEAS_TYPE_EXTENSION_SEED            (peas_extension_seed_get_type ())
#define PEAS_EXTENSION_SEED(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), PEAS_TYPE_EXTENSION_SEED, PeasExtensionSeed))
#define PEAS_EXTENSION_SEED_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), PEAS_TYPE_EXTENSION_SEED, PeasExtensionSeedClass))
#define PEAS_IS_EXTENSION_SEED(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PEAS_TYPE_EXTENSION_SEED))
#define PEAS_IS_EXTENSION_SEED_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), PEAS_TYPE_EXTENSION_SEED))
#define PEAS_EXTENSION_SEED_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), PEAS_TYPE_EXTENSION_SEED, PeasExtensionSeedClass))

typedef struct _PeasExtensionSeed       PeasExtensionSeed;
typedef struct _PeasExtensionSeedClass  PeasExtensionSeedClass;

struct _PeasExtensionSeed {
  PeasExtensionWrapper parent;

  SeedContext js_context;
  SeedObject js_object;
};

struct _PeasExtensionSeedClass {
  PeasExtensionWrapperClass parent_class;
};

GType            peas_extension_seed_get_type (void) G_GNUC_CONST;

GObject         *peas_extension_seed_new      (GType           exten_type,
                                               SeedContext     js_context,
                                               SeedObject      js_object);

G_END_DECLS

#endif /* __PEAS_EXTENSION_SEED_H__ */

