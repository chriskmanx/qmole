/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-tool-version.c: Version display

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

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gkr-tool.h"

#include "gck/gck.h"
#include "gcr/gcr.h"

#include "egg/egg-hex.h"

static GOptionEntry version_entries[] = {
	{ NULL }
};

int
gkr_tool_version (int argc, char *argv[])
{
	int ret;

	ret = gkr_tool_parse_options (&argc, &argv, version_entries);
	if (ret != 0)
		return ret;

	g_print ("gnome-keyring: %s\n", VERSION);
	return 0;
}
