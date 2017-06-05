/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-oid.h - OID helper routines

   Copyright (C) 2007 Stefan Walter

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

#ifndef EGGOID_H_
#define EGGOID_H_

#include <glib.h>
enum {
	EGG_OID_PRINTABLE = 0x01,
	EGG_OID_IS_CHOICE = 0x02
};

const gchar*       egg_oid_get_name                   (GQuark oid);

guint              egg_oid_get_flags                  (GQuark oid);

const gchar*       egg_oid_get_description            (GQuark oid);

#endif /* EGGOID_H_ */
