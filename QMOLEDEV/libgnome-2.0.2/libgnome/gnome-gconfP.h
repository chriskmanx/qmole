/*  -*- Mode: C; c-set-style: linux; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/* GNOME Library - gnome-gconf.h
 * Copyright (C) 2000  Red Hat Inc.,
 * All rights reserved.
 *
 * Author: Jonathan Blandford  <jrb@redhat.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Cambridge, MA 02139, USA.
 */
/*
  @NOTATION@
*/

#ifndef GNOME_GCONFP_H
#define GNOME_GCONFP_H

#include "gnome-gconf.h"

/* These two methods are very very private to libgnome* only,
 * do not use them */

#define gnome_gconf_lazy_init _gnome_gconf_lazy_init
void			_gnome_gconf_lazy_init		(void);

#define gnome_gconf_module_info_get _gnome_gconf_module_info_get
const GnomeModuleInfo *	_gnome_gconf_module_info_get	(void) G_GNUC_CONST;

#endif /* GNOME_GCONFP_H */
