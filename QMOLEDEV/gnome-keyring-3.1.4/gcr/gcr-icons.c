/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gcr.h"
#include "gcr-types.h"
#include "gcr-icons.h"
#include "gcr-internal.h"

static gboolean registered_icons = FALSE;
static const gchar *themed_icons[] = {
	GCR_ICON_CERTIFICATE,
	GCR_ICON_KEY,
	GCR_ICON_KEY_PAIR,
	NULL,
};

static void
add_theme_source (GtkIconSet *iconset, const gchar *icon, GtkIconSize size)
{
	GtkIconSource *source;

	source = gtk_icon_source_new ();
	gtk_icon_source_set_icon_name (source, icon);
	gtk_icon_source_set_direction_wildcarded (source, TRUE);
	gtk_icon_source_set_state_wildcarded (source, TRUE);

	if (size == -1) {
		gtk_icon_source_set_size_wildcarded (source, TRUE);
	} else {
		gtk_icon_source_set_size_wildcarded (source, FALSE);
		gtk_icon_source_set_size (source, size);
	}

	gtk_icon_set_add_source (iconset, source);
	gtk_icon_source_free (source);
}

void
_gcr_icons_register (void)
{
	GtkIconFactory *factory;
	GtkIconSet *iconset;
	const gchar **name;

	if (registered_icons)
		return;

	/* Setup the icon factory. */
	factory = gtk_icon_factory_new ();
	gtk_icon_factory_add_default (factory);

	for (name = themed_icons; name && *name; name++) {
		iconset = gtk_icon_set_new ();
		add_theme_source (iconset, *name, GTK_ICON_SIZE_BUTTON);
		add_theme_source (iconset, *name, GTK_ICON_SIZE_MENU);
		add_theme_source (iconset, *name, GTK_ICON_SIZE_LARGE_TOOLBAR);
		add_theme_source (iconset, *name, GTK_ICON_SIZE_SMALL_TOOLBAR);
		add_theme_source (iconset, *name, GTK_ICON_SIZE_DIALOG);
		add_theme_source (iconset, *name, -1);
		gtk_icon_factory_add (factory, *name, iconset);
		gtk_icon_set_unref (iconset);
	}

	g_object_unref (factory);
	registered_icons = TRUE;
}
