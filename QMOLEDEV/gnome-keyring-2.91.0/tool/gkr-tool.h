/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-tool.h: Command line utility

   Copyright (C) 2008 Stefan Walter

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

#ifndef GKRTOOL_H_
#define GKRTOOL_H_

#include <glib.h>

/* -------------------------------------------------------------------------------
 * GENERAL HELPERS
 */

extern gboolean gkr_tool_mode_quiet;

#define GKR_TOOL_BASIC_OPTIONS \
	{ "quiet", 'q', 0, G_OPTION_ARG_NONE, &gkr_tool_mode_quiet, "Don't print unnecessary output", NULL }, 

void gkr_tool_handle_error (GError **error, const gchar *message, ...);

int gkr_tool_parse_options (int *argc, char** argv[], GOptionEntry *options);

/* -------------------------------------------------------------------------------
 * VARIOUS COMMAND HANDLERS 
 */
int gkr_tool_import (int argc, char *argv[]);

#endif /* GKRTOOL_H_ */
