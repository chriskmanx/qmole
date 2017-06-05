/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2008-2012 Ricardo Mones and the Claws Mail team
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __AUTOFACES_H__
#define __AUTOFACES_H__

#define MAX_XFACE_LEN	990
#define MAX_FACE_LEN	990
#define AUTOFACES_DIR   "autofaces"

gint get_default_xface	(gchar *buf, gint len);
gint get_default_face	(gchar *buf, gint len);
gint get_account_xface	(gchar *buf, gint len, gchar *name);
gint get_account_face	(gchar *buf, gint len, gchar *name);

#endif /* __AUTOFACES_H__ */
