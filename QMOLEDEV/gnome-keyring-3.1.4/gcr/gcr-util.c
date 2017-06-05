/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "gcr-util.h"

#include <string.h>

/**
 * _gcr_util_parse_lines:
 * @string: The string to parse lines from, will be modified
 * @last_line: Whether or not we should run for last line or not
 * @callback: Call for each line
 * @user_data: Data for callback
 *
 * Calls callback for each line. If last_line, also sends the remainder
 * data that comes after the last line break. \n and \r\n are line separators.
 * Neither are included in data passed to callback.
 */
void
_gcr_util_parse_lines (GString *string, gboolean last_line,
                       GcrLineCallback callback, gpointer user_data)
{
	gchar *ptr;
	gchar *prev;

	g_return_if_fail (string);
	g_return_if_fail (callback);

	/* Print all stderr lines as messages */
	while ((ptr = strchr (string->str, '\n')) != NULL) {
		*ptr = '\0';
		prev = ptr - 1;
		if (*prev == '\r')
			*prev = '\0';

		(callback) (string->str, user_data);
		g_string_erase (string, 0, ptr - string->str + 1);
	}

	if (last_line && string->len) {
		(callback) (string->str, user_data);
		g_string_erase (string, 0, string->len);
	}
}
