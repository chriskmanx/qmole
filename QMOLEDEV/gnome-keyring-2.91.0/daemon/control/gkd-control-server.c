/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "gkd-control.h"
#include "gkd-control-codes.h"

#include "gkd-main.h"
#include "gkd-util.h"

#include "egg/egg-buffer.h"
#include "egg/egg-cleanup.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-unix-credentials.h"

#include "login/gkd-login.h"

#include <errno.h>
#include <fcntl.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>

typedef struct _ControlData {
	EggBuffer buffer;
	gsize position;
} ControlData;

/* -----------------------------------------------------------------------------------
 * CONTROL SERVER
 */

static ControlData*
control_data_new (void)
{
	ControlData *cdata = g_slice_new0 (ControlData);
	egg_buffer_init_full (&cdata->buffer, 128, egg_secure_realloc);
	cdata->position = 0;
	return cdata;
}

static void
control_data_free (gpointer data)
{
	ControlData *cdata = data;
	egg_buffer_uninit (&cdata->buffer);
	g_slice_free (ControlData, cdata);
}

static guint32
control_unlock_login (EggBuffer *buffer)
{
	gchar *master;
	gsize offset = 8;
	guint32 res;

	if (!egg_buffer_get_string (buffer, offset, &offset, &master, egg_secure_realloc))
		return GKD_CONTROL_RESULT_FAILED;

	if (gkd_login_unlock (master))
		res = GKD_CONTROL_RESULT_OK;
	else
		res = GKD_CONTROL_RESULT_DENIED;

	egg_secure_strfree (master);
	return res;
}

static guint32
control_change_login (EggBuffer *buffer)
{
	gsize offset = 8;
	guint32 res;
	gchar *master;
	gchar *original;

	if (!egg_buffer_get_string (buffer, offset, &offset, &original, egg_secure_realloc)) {
		return GKD_CONTROL_RESULT_FAILED;
	}

	if (!egg_buffer_get_string (buffer, offset, &offset, &master, egg_secure_realloc)) {
		egg_secure_strfree (original);
		return GKD_CONTROL_RESULT_FAILED;
	}

	if (gkd_login_change_lock (original, master))
		res = GKD_CONTROL_RESULT_OK;
	else
		res = GKD_CONTROL_RESULT_DENIED;

	egg_secure_strfree (master);
	egg_secure_strfree (original);
	return res;
}

static guint32
control_quit (EggBuffer *buffer)
{
	gkd_main_quit ();
	return GKD_CONTROL_RESULT_OK;
}

static guint32
control_initialize_components (EggBuffer *buffer)
{
	gchar *components;
	gchar **environment, **e;
	gsize offset = 8;
	gchar *x;
	int i;

	if (!egg_buffer_get_string (buffer, offset, &offset, &components, g_realloc))
		return GKD_CONTROL_RESULT_FAILED;

	if (!egg_buffer_get_stringv (buffer, offset, &offset, &environment, g_realloc)) {
		g_free (components);
		return GKD_CONTROL_RESULT_FAILED;
	}

	/* Accept environment from outside */
	for (e = environment; *e; ++e) {
		x = strchr (*e, '=');
		if (x) {
			*(x++) = 0;
			/* We're only interested in these environment variables */
			for (i = 0; GKD_UTIL_IN_ENVIRONMENT[i] != NULL; ++i) {
				if (g_str_equal (*e, GKD_UTIL_IN_ENVIRONMENT[i])) {
					g_setenv (*e, x, FALSE);
					break;
				}
			}
		}
	}

	g_strfreev (environment);

	/*
	 * We've now definitely received everything we need to run. Ask
	 * the daemon to complete the initialization.
	 */
	gkd_main_complete_initialization (components);
	g_free (components);

	return GKD_CONTROL_RESULT_OK;
}

static gboolean
control_output (GIOChannel *channel, GIOCondition cond, gpointer user_data)
{
	ControlData *cdata = user_data;
	EggBuffer *buffer = &cdata->buffer;
	int fd, res;

	fd = g_io_channel_unix_get_fd (channel);
	g_assert (cdata->position < buffer->len);

	if (cond & G_IO_OUT) {
		res = write (fd, buffer->buf + cdata->position, buffer->len - cdata->position);
		if (res < 0) {
			if (errno != EAGAIN && errno != EINTR)
				cdata->position = buffer->len;
		} else if (res == 0) {
			cdata->position = buffer->len;
		} else {
			cdata->position += res;
			g_assert (cdata->position <= buffer->len);
		}
	}

	if (cdata->position == buffer->len)
		cond |= G_IO_HUP;

	return (cond & G_IO_HUP) == 0;
}

static void
control_process (EggBuffer *req, GIOChannel *channel)
{
	ControlData *cdata = NULL;
	guint32 res;
	guint32 op;

	if (!egg_buffer_get_uint32 (req, 4, NULL, &op)) {
		g_message ("invalid operation sent to control socket");
		return;
	}

	switch (op) {
	case GKD_CONTROL_OP_UNLOCK:
		res = control_unlock_login (req);
		cdata = control_data_new ();
		egg_buffer_add_uint32 (&cdata->buffer, 0);
		egg_buffer_add_uint32 (&cdata->buffer, res);
		break;
	case GKD_CONTROL_OP_CHANGE:
		res = control_change_login (req);
		cdata = control_data_new ();
		egg_buffer_add_uint32 (&cdata->buffer, 0);
		egg_buffer_add_uint32 (&cdata->buffer, res);
		break;
	case GKD_CONTROL_OP_INITIALIZE:
		res = control_initialize_components (req);
		cdata = control_data_new ();
		egg_buffer_add_uint32 (&cdata->buffer, 0);
		egg_buffer_add_uint32 (&cdata->buffer, res);
		egg_buffer_add_stringv (&cdata->buffer, gkd_util_get_environment ());
		break;
	case GKD_CONTROL_OP_QUIT:
		res = control_quit (req);
		cdata = control_data_new ();
		egg_buffer_add_uint32 (&cdata->buffer, 0);
		egg_buffer_add_uint32 (&cdata->buffer, res);
		break;
	default:
		g_message ("received unsupported request operation on control socket: %d", (int)op);
		break;
	}

	if (cdata) {
		g_return_if_fail (!egg_buffer_has_error (&cdata->buffer));
		egg_buffer_set_uint32 (&cdata->buffer, 0, cdata->buffer.len);
		g_io_add_watch_full (channel, G_PRIORITY_DEFAULT, G_IO_OUT | G_IO_HUP,
		                     control_output, cdata, control_data_free);
	}
}

static gboolean
control_input (GIOChannel *channel, GIOCondition cond, gpointer user_data)
{
	ControlData *cdata = user_data;
	EggBuffer *buffer = &cdata->buffer;
	guint32 packet_size = 0;
	gboolean finished = FALSE;
	int fd, res;
	pid_t pid;
	uid_t uid;

	fd = g_io_channel_unix_get_fd (channel);

	if (cond & G_IO_IN) {

		/* Time for reading credentials */
		if (cdata->position == 0) {
			if (egg_unix_credentials_read (fd, &pid, &uid) < 0) {
				if (errno != EAGAIN && errno != EINTR)
					finished = TRUE;
			} else if (getuid () != uid) {
				g_message ("control request from bad uid: %u, should be %u\n", uid, getuid ());
				finished = TRUE;
			} else {
				cdata->position = 1;
			}

		/* Time for reading a packet size */
		} else if (egg_buffer_length (buffer) < 4) {
			egg_buffer_reserve (buffer, 4);
			res = read (fd, buffer->buf + buffer->len, 4 - buffer->len);
			if (res < 0) {
				if (errno != EAGAIN && errno != EINTR)
					finished = TRUE;
			} else if (res == 0) {
				finished = TRUE;
			} else {
				buffer->len += res;
			}

		/* Time for reading the packet */
		} else {
			if (!egg_buffer_get_uint32 (buffer, 0, NULL, &packet_size) || packet_size < 4) {
				g_message ("invalid packet size in control request");
				finished = TRUE;
			} else {
				g_assert (buffer->len < packet_size);
				egg_buffer_reserve (buffer, packet_size);
				res = read (fd, buffer->buf + buffer->len, packet_size - buffer->len);
				if (res < 0) {
					if (errno != EAGAIN && errno != EINTR)
						finished = TRUE;
				} else if (res == 0) {
					finished = TRUE;
				} else {
					buffer->len += res;
					g_assert (buffer->len <= packet_size);
				}
			}
		}

		/* Received a full packet, process */
		if (packet_size && buffer->len == packet_size) {
			control_process (buffer, channel);
			finished = TRUE;
		}
	}

	if (finished)
		cond |= G_IO_HUP;

	return (cond & G_IO_HUP) == 0;
}

static gboolean
control_accept (GIOChannel *channel, GIOCondition cond, gpointer callback_data)
{
	struct sockaddr_un addr;
	socklen_t addrlen;
	ControlData *cdata;
	GIOChannel *new_channel;
	int fd, new_fd;
	int val;

	fd = g_io_channel_unix_get_fd (channel);

	addrlen = sizeof (addr);
	new_fd = accept (fd, (struct sockaddr *) &addr, &addrlen);
	if (new_fd < 0) {
		g_warning ("couldn't accept new control request: %s", g_strerror (errno));
		return TRUE;
	}

	val = fcntl (new_fd, F_GETFL, 0);
	if (val < 0) {
		g_warning ("can't get control request fd flags: %s", g_strerror (errno));
		close (new_fd);
		return TRUE;
	}

	if (fcntl (new_fd, F_SETFL, val | O_NONBLOCK) < 0) {
		g_warning ("can't set control request to non-blocking io: %s", g_strerror (errno));
		close (new_fd);
		return TRUE;
	}

	cdata = control_data_new ();
	new_channel = g_io_channel_unix_new (new_fd);
	g_io_channel_set_close_on_unref (new_channel, TRUE);
	g_io_add_watch_full (new_channel, G_PRIORITY_DEFAULT, G_IO_IN | G_IO_HUP,
	                     control_input, cdata, control_data_free);
	g_io_channel_unref (new_channel);

	return TRUE;
}

static void
control_cleanup_channel (gpointer user_data)
{
	gchar *path = user_data;
	unlink (path);
	g_free (path);
}

gboolean
gkd_control_listen (void)
{
	struct sockaddr_un addr;
	GIOChannel *channel;
	gchar *path;
	int sock;

	path = g_strdup_printf ("%s/control", gkd_util_get_master_directory ());
	egg_cleanup_register (control_cleanup_channel, path);

	unlink (path);

	sock = socket (AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		g_warning ("couldn't open socket: %s", g_strerror (errno));
		return FALSE;
	}

	memset (&addr, 0, sizeof (addr));
	addr.sun_family = AF_UNIX;
	g_strlcpy (addr.sun_path, path, sizeof (addr.sun_path));
	if (bind (sock, (struct sockaddr*) &addr, sizeof (addr)) < 0) {
		g_warning ("couldn't bind to control socket: %s: %s", path, g_strerror (errno));
		close (sock);
		return FALSE;
	}

	if (listen (sock, 128) < 0) {
		g_warning ("couldn't listen on control socket: %s: %s", path, g_strerror (errno));
		close (sock);
		return FALSE;
	}

	if (!egg_unix_credentials_setup (sock) < 0) {
		close (sock);
		return FALSE;
	}

	channel = g_io_channel_unix_new (sock);
	g_io_add_watch (channel, G_IO_IN | G_IO_HUP, control_accept, NULL);
	g_io_channel_set_close_on_unref (channel, TRUE);
	egg_cleanup_register ((GDestroyNotify)g_io_channel_unref, channel);

	return TRUE;
}
