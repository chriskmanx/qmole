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

	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("frob-unlock-keyring");

	if (argc < 2) {
		g_printerr ("specify keyring to unlock\n");
		return 2;
	}

	res = gnome_keyring_unlock_sync (argv[1], NULL);
	if (res == GNOME_KEYRING_RESULT_OK) {
		g_printerr ("ok\n");
		return 0;
	} else if (res == GNOME_KEYRING_RESULT_CANCELLED) {
		g_printerr ("cancel\n");
		return 0;
	} else {
		g_printerr ("failed: %s\n", gnome_keyring_result_to_message (res));
		return 1;
	}
}
