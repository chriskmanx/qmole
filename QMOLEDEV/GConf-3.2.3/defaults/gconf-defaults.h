/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2008  Matthias Clasen  <mclasen@redhat.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#ifndef GCONF_DEFAULTS_H
#define GCONF_DEFAULTS_H

#include <glib-object.h>
#include <dbus/dbus-glib.h>

G_BEGIN_DECLS

#define GCONF_TYPE_DEFAULTS         (gconf_defaults_get_type ())
#define GCONF_DEFAULTS(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GCONF_TYPE_DEFAULTS, GConfDefaults))
#define GCONF_DEFAULTS_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), GCONF_TYPE_DEFAULTS, GConfDefaultsClass))
#define GCONF_IS_DEFAULTS(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GCONF_TYPE_DEFAULTS))
#define GCONF_IS_DEFAULTS_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GCONF_TYPE_DEFAULTS))
#define GCONF_DEFAULTS_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), GCONF_TYPE_DEFAULTS, GConfDefaultsClass))

typedef struct GConfDefaultsPrivate GConfDefaultsPrivate;

typedef struct
{
        GObject        parent;
        GConfDefaultsPrivate *priv;
} GConfDefaults;

typedef struct
{
        GObjectClass   parent_class;

	void (* system_set) (GConfDefaults  *defaults,
                             const char    **keys);

} GConfDefaultsClass;

typedef enum
{
        GCONF_DEFAULTS_ERROR_GENERAL,
        GCONF_DEFAULTS_ERROR_NOT_PRIVILEGED,
        GCONF_DEFAULTS_NUM_ERRORS
} GConfDefaultsError;

#define GCONF_DEFAULTS_ERROR gconf_defaults_error_quark ()

GType gconf_defaults_error_get_type (void);
#define GCONF_DEFAULTS_TYPE_ERROR (gconf_defaults_error_get_type ())


GQuark         gconf_defaults_error_quark    (void);
GType          gconf_defaults_get_type       (void);
GConfDefaults *gconf_defaults_new            (void);

/* exported methods */
void           gconf_defaults_set_system          (GConfDefaults          *mechanism,
                                                   const char            **includes,
                                                   const char            **excludes,
                                                   DBusGMethodInvocation  *context);

void           gconf_defaults_set_system_value    (GConfDefaults          *mechanism,
                                                   const char             *path,
                                                   const char             *value,
                                                   DBusGMethodInvocation  *context);

void           gconf_defaults_set_mandatory       (GConfDefaults          *mechanism,
                                                   const char            **includes,
                                                   const char            **excludes,
                                                   DBusGMethodInvocation  *context);

void           gconf_defaults_set_mandatory_value (GConfDefaults          *mechanism,
                                                   const char             *path,
                                                   const char             *value,
                                                   DBusGMethodInvocation  *context);

void           gconf_defaults_unset_mandatory     (GConfDefaults          *mechanism,
                                                   const char            **includes,
                                                   const char            **excludes,
                                                   DBusGMethodInvocation  *context);

void		gconf_defaults_can_set_system    (GConfDefaults         *mechanism,
						  const char	       **includes,
                                                  DBusGMethodInvocation  *context);

void		gconf_defaults_can_set_mandatory (GConfDefaults         *mechanism,
						  const char	       **includes,
                                                  DBusGMethodInvocation  *context);

G_END_DECLS

#endif /* GCONF_DEFAULTS_H */
