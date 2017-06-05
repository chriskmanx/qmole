/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

/*
 * Handles i18n for the Gnome libraries. Libraries need to use
 * dgettext in order to use a non-default translation domain.
 * Author: Tom Tromey <tromey@creche.cygnus.com>
 */

#ifndef __GNOME_I18NP_H__
#define __GNOME_I18NP_H__ 1

#ifndef GNOME_EXPLICIT_TRANSLATION_DOMAIN
#  define GNOME_EXPLICIT_TRANSLATION_DOMAIN GETTEXT_PACKAGE
#endif
#include "gnome-i18n.h"

#endif /* __GNOME_I18NP_H__ */
