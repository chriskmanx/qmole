/* GdkPixbuf library - Internationalization
 *
 * Copyright (C) 2000 Havoc Pennington
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#ifndef __GDKPIXBUFINTL_H__
#define __GDKPIXBUFINTL_H__

#include "config.h"
#include <glib.h>

#ifdef ENABLE_NLS
#define _(String) gdk_pixbuf_gettext(String)
#define P_(String) gdk_pixbuf_gettext(String)
#define N_(String) (String)
#else
#define _(String) (String)
#define P_(String) (String)
#define N_(String) (String)
#endif

const gchar *
gdk_pixbuf_gettext (const gchar *msgid) G_GNUC_FORMAT(1);

#endif
