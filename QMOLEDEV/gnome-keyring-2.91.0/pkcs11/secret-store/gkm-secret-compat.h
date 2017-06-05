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

#ifndef __GKM_SECRET_COMPAT_H__
#define __GKM_SECRET_COMPAT_H__

#include <glib.h>

typedef enum {
	GKM_SECRET_ACCESS_READ = 1 << 0,
	GKM_SECRET_ACCESS_WRITE = 1 << 1,
	GKM_SECRET_ACCESS_REMOVE = 1 << 2
} GkmSecretAccessType;

typedef struct _GkmSecretAccess {
	char *display_name;
	char *pathname;
	GkmSecretAccessType types_allowed;
} GkmSecretAccess;

void           gkm_secret_compat_access_free         (gpointer ac);

void           gkm_secret_compat_acl_free            (gpointer acl);

guint          gkm_secret_compat_parse_item_type     (const gchar *value);

const gchar*   gkm_secret_compat_format_item_type    (guint value);

#endif /* __GKM_SECRET_COMPAT_H__ */
