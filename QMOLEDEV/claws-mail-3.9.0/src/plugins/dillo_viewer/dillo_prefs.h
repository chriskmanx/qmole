/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 the Claws Mail Team
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

/*
 * The structure of this file has been borrowed from the structure of
 * the image_viewer plugin file. I also used it as an example of how to
 * build the preferences for the dillo plugin.
 */

#ifndef DILLOPREFS_H
#define DILLOPREFS_H

#include <glib.h>

typedef struct _DilloBrowserPrefs	DilloBrowserPrefs;

struct _DilloBrowserPrefs
{
	gboolean local;   /**< local browsing */
	gboolean whitelist_ab;
	gchar *whitelist_ab_folder;
	gboolean full;    /**< use full window */
};

extern DilloBrowserPrefs dillo_prefs;

void dillo_prefs_init(void);
void dillo_prefs_done(void);

#endif
