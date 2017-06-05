/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*

   Copyright (C) 2011 Collabora Ltd.

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include "gnome-keyring.h"
#include <glib.h>

int
main (int argc, char **argv)
{
	GnomeKeyringResult res;
	gchar *date;
	gchar *display;
	GList *results, *l;
	GnomeKeyringFound *found;
	GnomeKeyringAttributeList *attributes;
	GnomeKeyringItemInfo *info;
	GDateTime *datetime;

	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("frob-list-modified");

	attributes = gnome_keyring_attribute_list_new ();

	res = gnome_keyring_find_items_sync (GNOME_KEYRING_ITEM_GENERIC_SECRET,
	                                     attributes, &results);
	g_assert_cmpint (res, ==, GNOME_KEYRING_RESULT_OK);

	gnome_keyring_attribute_list_free (attributes);

	for (l = results; l != NULL; l = g_list_next (l)) {
		found = l->data;
		res = gnome_keyring_item_get_info_sync (found->keyring, found->item_id, &info);
		g_assert_cmpint (res, ==, GNOME_KEYRING_RESULT_OK);

		datetime = g_date_time_new_from_unix_utc (gnome_keyring_item_info_get_mtime (info));
		date = g_date_time_format (datetime, "%Y%m%dT%H%M%S");
		display = gnome_keyring_item_info_get_display_name (info);
		g_print ("%lu %s: %s/%u: %s\n", gnome_keyring_item_info_get_mtime (info),
		         date, found->keyring, found->item_id, display);
		g_date_time_unref (datetime);
		g_free (date);
		g_free (display);

		gnome_keyring_item_info_free (info);
	}

	g_list_free (results);
	return 0;
}
