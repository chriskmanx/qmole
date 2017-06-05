/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef __GKD_SECRET_PROPERTY_H__
#define __GKD_SECRET_PROPERTY_H__

#include "gkd-secret-types.h"

#include "gck/gck.h"

#include <dbus/dbus.h>

gboolean               gkd_secret_property_get_type               (const gchar *property,
                                                                   CK_ATTRIBUTE_TYPE *type);

gboolean               gkd_secret_property_append_variant         (DBusMessageIter *iter,
                                                                   GckAttribute *attr);

gboolean               gkd_secret_property_append_all             (DBusMessageIter *array,
                                                                   GckAttributes *attrs);

gboolean               gkd_secret_property_parse_variant          (DBusMessageIter *iter,
                                                                   const gchar *property,
                                                                   GckAttribute *attr);

gboolean               gkd_secret_property_parse_fields           (DBusMessageIter *iter,
                                                                   GckAttribute *attr);

gboolean               gkd_secret_property_parse_all              (DBusMessageIter *array,
                                                                   GckAttributes *attrs);

#endif /* __GKD_SECRET_PROPERTY_H__ */
