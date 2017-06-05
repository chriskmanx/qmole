/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-capability.c - the security-critical initial phase of the daemon
 *
 * Copyright (C) 2011 Steve Grubb
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
 * Author: Steve Grubb <sgrubb@redhat.com>
 */

#include "config.h"

#include "gkd-capability.h"

#ifdef HAVE_LIBCAPNG
#include <cap-ng.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_LIBCAPNG

/* No logging, no gettext */
static void
early_error (const char *err_string)
{
	fprintf (stderr, "gnome-keyring-daemon: %s, aborting\n", err_string);
	exit (1);
}

static void
early_warning (const char *warn_string)
{
	fprintf (stderr, "gnome-keyring-daemon: %s\n", warn_string);
}

#endif /* HAVE_LIPCAPNG */

/*
 * This program needs the CAP_IPC_LOCK posix capability.
 * We want to allow either setuid root or file system based capabilies
 * to work. If file system based capabilities, this is a no-op unless
 * the root user is running the program. In that case we just drop
 * capabilities down to IPC_LOCK. If we are setuid root, then change to the
 * invoking user retaining just the IPC_LOCK capability. The application
 * is aborted if for any reason we are unable to drop privileges.
 * Note: even gettext is unavailable!
 */
void
gkd_capability_obtain_capability_and_drop_privileges (void)
{
#ifdef HAVE_LIBCAPNG
	capng_get_caps_process ();
	switch (capng_have_capabilities (CAPNG_SELECT_CAPS))
	{
		case CAPNG_FULL:
			/* We are either setuid root or the root user */
			capng_clear (CAPNG_SELECT_CAPS);
			capng_update (CAPNG_ADD,
					CAPNG_EFFECTIVE|CAPNG_PERMITTED,
					CAP_IPC_LOCK);
			if (capng_change_id (getuid (), getgid (), 0))
				early_error ("failed dropping capabilities");
			break;
		case CAPNG_FAIL:
			early_error ("error getting process capabilities");
			break;
		case CAPNG_NONE:
			early_warning ("insufficient process capabilities, unsecure memory might get used");
			break;
		case CAPNG_PARTIAL: /* File system based capabilities */
			if (!capng_have_capability (CAPNG_EFFECTIVE, CAP_IPC_LOCK)) {
				early_warning ("insufficient process capabilities, unsecure memory might get used");
				/* Drop all capabilities */
				capng_clear (CAPNG_SELECT_BOTH);
				capng_apply (CAPNG_SELECT_BOTH);
				break;
			}

			/* Drop all capabilities except ipc_lock */
			capng_clear (CAPNG_SELECT_BOTH);
			if (capng_update (CAPNG_ADD,
					  CAPNG_EFFECTIVE|CAPNG_PERMITTED,
					  CAP_IPC_LOCK) != 0)
				early_error ("error dropping process capabilities");
			if (capng_apply (CAPNG_SELECT_BOTH) != 0)
				early_error ("error dropping process capabilities");
			break;
	}
#endif /* HAVE_LIBCAPNG */
}
