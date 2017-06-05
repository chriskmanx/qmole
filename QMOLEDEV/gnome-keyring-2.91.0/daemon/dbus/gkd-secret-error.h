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

#ifndef __GKD_SECRET_ERROR_H__
#define __GKD_SECRET_ERROR_H__

#include "gkd-secret-types.h"

#include <dbus/dbus.h>

DBusMessage*      gkd_secret_error_no_such_object                       (DBusMessage *message);

DBusMessage*      gkd_secret_error_to_reply                             (DBusMessage *message,
                                                                         DBusError *derr);

#endif /* __GKD_SECRET_ERROR_H__ */
