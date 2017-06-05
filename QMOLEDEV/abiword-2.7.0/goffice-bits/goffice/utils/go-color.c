/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-color.c :
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include "go-color.h"

#include <stdio.h>

/**
 * go_color_from_str :
 * @str :
 * @res :
 * 
 * Returns TRUE if @str can be parsed as a color of the form R:G:B:A and the
 * result is stored in @res.
 **/
gboolean
go_color_from_str (gchar const *str, GOColor *res)
{
	unsigned r, g, b, a;

	if (sscanf (str, "%X:%X:%X:%X", &r, &g, &b, &a) == 4) {
		*res = RGBA_TO_UINT (r, g, b, a);
		return TRUE;
	}
	return FALSE;
}

gchar *
go_color_as_str (GOColor color)
{
	unsigned r, g, b, a;

	UINT_TO_RGBA (color, &r, &g, &b, &a);
	return g_strdup_printf ("%X:%X:%X:%X", r, g, b, a);
}

PangoAttribute *
go_color_to_pango (GOColor color, gboolean is_fore)
{
	guint16 r, g, b;
	r  = UINT_RGBA_R (color);
	r |= (r << 8);
	g  = UINT_RGBA_G (color);
	g |= (g << 8);
	b  = UINT_RGBA_B (color);
	b |= (b << 8);

	if (is_fore)
		return pango_attr_foreground_new (r, g, b);
	else
		return pango_attr_background_new (r, g, b);
}

#ifdef GOFFICE_WITH_GTK
#include <gdk/gdkcolor.h>

GdkColor *
go_color_to_gdk	(GOColor color, GdkColor *res)
{
	res->red    = UINT_RGBA_R (color);
	res->red   |= (res->red << 8);
	res->green  = UINT_RGBA_G (color);
	res->green |= (res->green << 8);
	res->blue   = UINT_RGBA_B (color);
	res->blue  |= (res->blue << 8);

	return res;
}
#endif /* GOFFICE_WITH_GTK */
