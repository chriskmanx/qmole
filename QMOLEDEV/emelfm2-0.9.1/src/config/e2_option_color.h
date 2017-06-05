/* $Id: e2_option_color.h 2352 2011-05-26 03:53:01Z tpgww $

Copyright (C) 2004-2011 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_OPTION_COLOR_H__
#define __E2_OPTION_COLOR_H__

#include "e2_option.h"
#include "e2_option_color.h"

#ifdef E2_RAINBOW
void e2_option_color_filetypes_sync (void);
#endif
E2_OptionSet *e2_option_color_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gchar *def, E2_OptionFlags flags);
GdkColor *e2_option_color_get (const gchar *name);
#ifdef USE_GTK3_0
gboolean e2_option_color_get_RGBA (const gchar *name, GdkRGBA *color);
#endif
gboolean e2_option_color_set_str (gchar *name, gchar *value);
gboolean e2_option_color_set_str_direct (E2_OptionSet *set, gchar *value);

#endif //ndef __E2_OPTION_COLOR_H__
