/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2; -*- */
/*
 * Copyright (C) 2007 Collabora Ltd.
 * Copyright (C) 2007 Nokia Corporation
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "config.h"

#include "gkr-debug.h"

#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>
#include <glib/gstdio.h>

#ifdef WITH_DEBUG

#if FOR_WHEN_ALL_ELSE_FAILS
#include <syslog.h>
#endif

static GkrDebugFlags current_flags = 0;

static GDebugKey keys[] = {
	{ "operation", GKR_DEBUG_OPERATION },
	{ 0, }
};

static void
debug_set_flags (GkrDebugFlags new_flags)
{
	current_flags |= new_flags;
}

void
gkr_debug_set_flags (const gchar *flags_string)
{
	guint nkeys;

	for (nkeys = 0; keys[nkeys].value; nkeys++);

	if (flags_string)
		debug_set_flags (g_parse_debug_string (flags_string, keys, nkeys));
}

gboolean
gkr_debug_flag_is_set (GkrDebugFlags flag)
{
	return (flag & current_flags) != 0;
}

void
gkr_debug_message (GkrDebugFlags flag, const gchar *format, ...)
{
	static gsize initialized_flags = 0;
	gchar *message;
	va_list args;

	if (g_once_init_enter (&initialized_flags)) {
		gkr_debug_set_flags (g_getenv ("GKR_DEBUG"));
		g_once_init_leave (&initialized_flags, 1);

#if FOR_WHEN_ALL_ELSE_FAILS
		openlog ("libgnome-keyring", LOG_NDELAY | LOG_PID, LOG_AUTHPRIV);
		gkr_debug_set_flags ("all");
#endif
	}

	va_start (args, format);
	message = g_strdup_vprintf (format, args);
	va_end (args);

	if (flag & current_flags)
		g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", message);

#if FOR_WHEN_ALL_ELSE_FAILS
	syslog (LOG_ERR, "%s", message);
#endif
	g_free (message);
}

#else /* !WITH_DEBUG */

gboolean
gkr_debug_flag_is_set (GkrDebugFlags flag)
{
	return FALSE;
}

void
gkr_debug_message (GkrDebugFlags flag, const gchar *format, ...)
{
}

void
gkr_debug_set_flags (const gchar *flags_string)
{
}

#endif /* !WITH_DEBUG */
