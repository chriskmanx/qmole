/*
 * Copyright (C) 2010 Novell, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Author: Vincent Untz <vuntz@gnome.org>
 */

#ifndef _gconfsettingsbackend_h_
#define _gconfsettingsbackend_h_

#define G_SETTINGS_ENABLE_BACKEND
#include <gio/gsettingsbackend.h>

G_BEGIN_DECLS

#define GCONF_TYPE_SETTINGS_BACKEND                   (gconf_settings_backend_get_type ())
#define GCONF_SETTINGS_BACKEND(inst)                  (G_TYPE_CHECK_INSTANCE_CAST ((inst),    \
                                                       GCONF_TYPE_SETTINGS_BACKEND,           \
                                                       GConfSettingsBackend))
#define GCONF_SETTINGS_BACKEND_CLASS(class)           (G_TYPE_CHECK_CLASS_CAST ((class),      \
                                                       GCONF_TYPE_SETTINGS_BACKEND,           \
                                                       GConfSettingsBackendClass))
#define GCONF_IS_SETTINGS_BACKEND(inst)               (G_TYPE_CHECK_INSTANCE_TYPE ((inst),    \
                                                       GCONF_TYPE_SETTINGS_BACKEND))
#define GCONF_IS_SETTINGS_BACKEND_CLASS(class)        (G_TYPE_CHECK_CLASS_TYPE ((class),      \
                                                       GCONF_TYPE_SETTINGS_BACKEND))
#define GCONF_SETTINGS_BACKEND_GET_CLASS(inst)        (G_TYPE_INSTANCE_GET_CLASS ((inst),     \
                                                       GCONF_TYPE_SETTINGS_BACKEND,           \
                                                       GConfSettingsBackendClass))

/**
 * GConfSettingsBackend:
 *
 * A backend to GSettings that stores the settings in gconf.
 **/
typedef struct _GConfSettingsBackendPrivate               GConfSettingsBackendPrivate;
typedef struct _GConfSettingsBackendClass                 GConfSettingsBackendClass;
typedef struct _GConfSettingsBackend                      GConfSettingsBackend;

struct _GConfSettingsBackendClass
{
  GSettingsBackendClass parent_class;
};

struct _GConfSettingsBackend
{
  GSettingsBackend parent_instance;

  /*< private >*/
  GConfSettingsBackendPrivate *priv;
};

GType gconf_settings_backend_get_type (void);
void  gconf_settings_backend_register (GIOModule *module);

G_END_DECLS

#endif /* _gconfsettingsbackend_h_ */
