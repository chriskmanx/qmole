/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-dn.h - ASN.1 helper routines

   Copyright (C) 2010 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef EGG_DN_H_
#define EGG_DN_H_

#include <glib.h>

gchar*             egg_dn_read                            (GNode *node);

gchar*             egg_dn_read_part                       (GNode *node,
                                                           const gchar *match);

typedef void       (*EggDnCallback)                       (guint index,
                                                           GQuark oid,
                                                           const guchar *value,
                                                           gsize n_value,
                                                           gpointer user_data);

gboolean           egg_dn_parse                           (GNode *node,
                                                           EggDnCallback callback,
                                                           gpointer user_data);

gchar*             egg_dn_print_value                     (GQuark oid,
                                                           const guchar *value,
                                                           gsize n_value);

#endif /* EGG_DN_H_ */
