/* -*- Mode: C; c-basic-offset: 4 -*-
 * libglade - a library for building interfaces from XML files at runtime
 * Copyright (C) 1998-2001  James Henstridge <james@daa.com.au>
 * Copyright 2001 Ximian, Inc.
 *
 * glade-canvas.c: support for canvas widgets in libglade.
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
 * License along with this library; if not, write to the 
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 *
 * Authors:
 *    Jacob Berkman <jacob@ximian.com>
 *    James Henstridge <james@daa.com.au>
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <glade/glade-init.h>
#include <glade/glade-build.h>
#include <libgnomecanvas/gnome-canvas.h>

/* this macro puts a version check function into the module */
GLADE_MODULE_CHECK_INIT

static void
set_aa (GladeXML *xml, GtkWidget *w,
	const char *name, const char *value)
{
    GNOME_CANVAS (w)->aa = (*value == 'y') ? 1 : 0;
}

#define SET_SCROLL(corner)                                                        \
static void                                                                       \
set_scroll_##corner (GladeXML *xml, GtkWidget *w,                                 \
		     const char *name, const char *value)                         \
{                                                                                 \
    double x1, y1, x2, y2;                                                        \
    gnome_canvas_get_scroll_region (GNOME_CANVAS (w), &x1, &y1, &x2, &y2);        \
    corner = strtod (value, NULL);                                                \
    gnome_canvas_set_scroll_region (GNOME_CANVAS (w), x1, y1, x2, y2);            \
}

SET_SCROLL (x1)
SET_SCROLL (y1)
SET_SCROLL (x2)
SET_SCROLL (y2)

static void
set_pixels_per_unit (GladeXML *xml, GtkWidget *w,
		     const char *name, const char *value)
{
    gnome_canvas_set_pixels_per_unit (GNOME_CANVAS (w), 
				      strtod (value, NULL));
}

void
glade_module_register_widgets (void)
{
    glade_register_custom_prop (GNOME_TYPE_CANVAS, "anti_aliased", set_aa);
    glade_register_custom_prop (GNOME_TYPE_CANVAS, "scroll_x1", set_scroll_x1);
    glade_register_custom_prop (GNOME_TYPE_CANVAS, "scroll_y1", set_scroll_y1);
    glade_register_custom_prop (GNOME_TYPE_CANVAS, "scroll_x2", set_scroll_x2);
    glade_register_custom_prop (GNOME_TYPE_CANVAS, "scroll_y2", set_scroll_y2);
    glade_register_custom_prop (GNOME_TYPE_CANVAS, "pixels_per_unit", set_pixels_per_unit);
    

    glade_register_widget (GNOME_TYPE_CANVAS, glade_standard_build_widget, NULL, NULL);
    glade_provide ("canvas");
}
