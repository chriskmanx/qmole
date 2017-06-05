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

#ifndef __GKD_SECRET_UTIL_H__
#define __GKD_SECRET_UTIL_H__

#include "gkd-secret-types.h"

#include <glib.h>

#include <dbus/dbus.h>

gboolean          gkd_secret_util_parse_path                            (const gchar *path,
                                                                         gchar **collection,
                                                                         gchar **item);

gchar*            gkd_secret_util_build_path                            (const gchar *base,
                                                                         gconstpointer identifier,
                                                                         gssize n_identifier);

#endif /* __GKD_SECRET_UTIL_H__ */
