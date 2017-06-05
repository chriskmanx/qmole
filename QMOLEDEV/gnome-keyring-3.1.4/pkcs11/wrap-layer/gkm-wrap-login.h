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

#ifndef __GKM_WRAP_LOGIN_H__
#define __GKM_WRAP_LOGIN_H__

#include <glib.h>

gboolean      gkm_wrap_login_is_usable                (void);

gboolean      gkm_wrap_login_did_unlock_fail          (void);

gchar*        gkm_wrap_login_steal_failed_password    (void);

void          gkm_wrap_login_attach_secret            (const gchar *label,
                                                       const gchar *secret,
                                                       const gchar *first,
                                                       ...);

gchar*        gkm_wrap_login_lookup_secret            (const gchar *first,
                                                       ...);

void          gkm_wrap_login_remove_secret            (const gchar *first,
                                                       ...);

#endif /* __GKM_WRAP_LOGIN_H__ */
