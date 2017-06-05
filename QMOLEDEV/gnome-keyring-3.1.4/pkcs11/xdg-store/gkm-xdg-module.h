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

#ifndef __GKM_XDG_MODULE_H__
#define __GKM_XDG_MODULE_H__

#include <glib-object.h>

#include "gkm/gkm-module.h"

#define GKM_TYPE_XDG_MODULE               (gkm_xdg_module_get_type ())
#define GKM_XDG_MODULE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_XDG_MODULE, GkmXdgModule))
#define GKM_XDG_MODULE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_XDG_MODULE, GkmXdgModuleClass))
#define GKM_IS_XDG_MODULE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_XDG_MODULE))
#define GKM_IS_XDG_MODULE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_XDG_MODULE))
#define GKM_XDG_MODULE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_XDG_MODULE, GkmXdgModuleClass))

typedef struct _GkmXdgModule GkmXdgModule;
typedef struct _GkmXdgModuleClass GkmXdgModuleClass;

struct _GkmXdgModuleClass {
	GkmModuleClass parent_class;
};

GType               gkm_xdg_module_get_type               (void);

#endif /* __GKM_XDG_MODULE_H__ */
