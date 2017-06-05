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

#include "egg/egg-buffer.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-unix-credentials.h"

#include <errno.h>
#include <fcntl.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/stat.h>

static int
control_connect (const gchar *path)
{
	struct sockaddr_un addr;
	struct stat st;
	int sock;

	/* First a bunch of checks to make sure nothing funny is going on */
	if (lstat (path, &st) < 0) {
		g_message ("couldn't access conrol socket: %s: %s", path, g_strerror (errno));
		return -1;

	} else if (st.st_uid != geteuid ()) {
		g_message("The control socket is not owned with the same "
		          "credentials as the user login: %s", path);
		return -1;

	} else if (S_ISLNK (st.st_mode) || !S_ISSOCK (st.st_mode)) {
		g_message ("The control socket is not a valid simple non-linked socket");
		return -1;
	}

	addr.sun_family = AF_UNIX;
	g_strlcpy (addr.sun_path, path, sizeof (addr.sun_path));

	/* Now we connect */
	sock = socket (AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		g_warning ("couldn't create control socket: %s", g_strerror (errno));
		return -1;
	}

	/* Close on exec */
	fcntl (sock, F_SETFD, 1);

	if (connect (sock, (struct sockaddr*) &addr, sizeof (addr)) < 0) {
		g_message ("couldn't connect to control socket at: %s: %s",
		           addr.sun_path, g_strerror (errno));
		close (sock);
		return -1;
	}

	/* This lets the server verify us */
	for (;;) {
		if (egg_unix_credentials_write (sock) < 0) {
			if (errno == EINTR || errno == EAGAIN)
				continue;
			g_message ("couldn't send credentials to control socket: %s",
			           g_strerror (errno));
			close (sock);
			return -1;
		}

		return sock;
	}
}

static gboolean
control_write (int fd, EggBuffer *buf)
{
	gsize bytes = 0;
	gssize res;

	while (bytes < buf->len) {
		res = write (fd, buf->buf + bytes, buf->len - bytes);
		if (res < 0) {
			if (errno != EINTR && errno != EAGAIN) {
				g_warning ("couldn't write all bytes to control socket: %s",
				           g_strerror (errno));
				return FALSE;
			}
		} else {
			bytes += res;
		}
	}

	return TRUE;
}

static gsize
control_read_raw (int fd, guchar *buf, size_t len)
{
	gsize bytes;
	gssize res;

	bytes = 0;
	while (bytes < len) {
		res = read (fd, buf + bytes, len - bytes);
		if (res <= 0) {
			if (res == 0)
				res = -1;
			else if (errno == EAGAIN)
				continue;
			else
				g_warning ("couldn't read %u bytes from control socket: %s",
					   (unsigned int)len, g_strerror (errno));
			return res;
		}
		bytes += res;
	}
	return bytes;
}


static gboolean
control_read (int fd, EggBuffer *buffer)
{
	guint32 packet_size;

	egg_buffer_resize (buffer, 4);
	if (control_read_raw (fd, buffer->buf, 4) != 4)
		return FALSE;

	if (!egg_buffer_get_uint32 (buffer, 0, NULL, &packet_size) ||
	    packet_size < 4)
		return FALSE;

	egg_buffer_resize (buffer, packet_size);
	if (control_read_raw (fd, buffer->buf + 4, packet_size - 4) != packet_size - 4)
		return FALSE;

	return TRUE;
}

static gboolean
control_chat (const gchar *directory, EggBuffer *buffer)
{
	gboolean ret;
	gchar *path;
	int sock;

	path = g_strdup_printf ("%s/control", directory);
	sock = control_connect (path);
	g_free (path);

	if (sock < 0)
		return FALSE;

	ret = control_write (sock, buffer) && control_read (sock, buffer);
	close (sock);
	return ret;
}


gchar**
gkd_control_initialize (const gchar *directory, const gchar *components,
                        const gchar **envp)
{
	gchar **env = NULL;
	EggBuffer buffer;
	gsize offset = 4;
	gboolean ret;
	guint32 res;

	egg_buffer_init_full (&buffer, 128, g_realloc);
	egg_buffer_add_uint32 (&buffer, 0);
	egg_buffer_add_uint32 (&buffer, GKD_CONTROL_OP_INITIALIZE);
	egg_buffer_add_string (&buffer, components);
	egg_buffer_add_stringv (&buffer, (const char**)envp);
	egg_buffer_set_uint32 (&buffer, 0, buffer.len);

	g_return_val_if_fail (!egg_buffer_has_error (&buffer), FALSE);

	ret = control_chat (directory, &buffer);

	if (ret)
		ret = egg_buffer_get_uint32 (&buffer, offset, &offset, &res);
	if (ret && res == GKD_CONTROL_RESULT_OK)
	      ret = egg_buffer_get_stringv (&buffer, offset, &offset, &env, g_realloc);

	egg_buffer_uninit (&buffer);

	if (!ret || res != GKD_CONTROL_RESULT_OK)
		return NULL;

	return env;
}

gboolean
gkd_control_unlock (const gchar *directory, const gchar *password)
{
	EggBuffer buffer;
	gsize offset = 4;
	gboolean ret;
	guint32 res;

	egg_buffer_init_full (&buffer, 128, egg_secure_realloc);
	egg_buffer_add_uint32 (&buffer, 0);
	egg_buffer_add_uint32 (&buffer, GKD_CONTROL_OP_UNLOCK);
	egg_buffer_add_string (&buffer, password);
	egg_buffer_set_uint32 (&buffer, 0, buffer.len);

	g_return_val_if_fail (!egg_buffer_has_error (&buffer), FALSE);

	ret = control_chat (directory, &buffer);

	if (ret)
		ret = egg_buffer_get_uint32 (&buffer, offset, &offset, &res);

	egg_buffer_uninit (&buffer);

	if (!ret || res != GKD_CONTROL_RESULT_OK) {
		g_message ("couldn't unlock login keyring");
		return FALSE;
	}

	return TRUE;
}

gboolean
gkd_control_change_lock (const gchar *directory, const gchar *original,
                         const gchar *password)
{
	EggBuffer buffer;
	gsize offset = 4;
	gboolean ret;
	guint32 res;

	egg_buffer_init_full (&buffer, 128, egg_secure_realloc);
	egg_buffer_add_uint32 (&buffer, 0);
	egg_buffer_add_uint32 (&buffer, GKD_CONTROL_OP_CHANGE);
	egg_buffer_add_string (&buffer, original);
	egg_buffer_add_string (&buffer, password);
	egg_buffer_set_uint32 (&buffer, 0, buffer.len);

	g_return_val_if_fail (!egg_buffer_has_error (&buffer), FALSE);

	ret = control_chat (directory, &buffer);

	if (ret)
		ret = egg_buffer_get_uint32 (&buffer, offset, &offset, &res);

	egg_buffer_uninit (&buffer);

	if (!ret || res != GKD_CONTROL_RESULT_OK) {
		g_message ("couldn't change lock on login keyring");
		return FALSE;
	}

	return TRUE;
}

gboolean
gkd_control_quit (const gchar *directory)
{
	EggBuffer buffer;
	gsize offset = 4;
	gboolean ret;
	guint32 res;

	egg_buffer_init_full (&buffer, 128, egg_secure_realloc);
	egg_buffer_add_uint32 (&buffer, 0);
	egg_buffer_add_uint32 (&buffer, GKD_CONTROL_OP_QUIT);
	egg_buffer_set_uint32 (&buffer, 0, buffer.len);

	g_return_val_if_fail (!egg_buffer_has_error (&buffer), FALSE);

	ret = control_chat (directory, &buffer);

	if (ret)
		ret = egg_buffer_get_uint32 (&buffer, offset, &offset, &res);

	egg_buffer_uninit (&buffer);

	if (!ret || res != GKD_CONTROL_RESULT_OK) {
		g_message ("couldn't quit running keyring daemon");
		return FALSE;
	}

	return TRUE;
}
