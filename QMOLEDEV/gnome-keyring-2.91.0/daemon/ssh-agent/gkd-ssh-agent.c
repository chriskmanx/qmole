/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-ssh-agent.c - handles SSH i/o from the clients

   Copyright (C) 2007 Stefan Walter

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "gkd-ssh-agent.h"
#include "gkd-ssh-agent-private.h"

#include "egg/egg-buffer.h"
#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#ifndef HAVE_SOCKLEN_T
#define socklen_t int
#endif

/* The loaded PKCS#11 modules */
static GList *pkcs11_modules = NULL;

static gboolean
read_all (int fd, guchar *buf, int len)
{
	int all = len;
	int res;

	while (len > 0) {

		res = read (fd, buf, len);

		if (res < 0) {
			if (errno == EAGAIN || errno == EINTR)
				continue;
			g_warning ("couldn't read %u bytes from client: %s", all,
			           g_strerror (errno));
			return FALSE;
		} else if (res == 0) {
			return FALSE;
		} else  {
			len -= res;
			buf += res;
		}
	}

	return TRUE;
}

static gboolean
write_all (int fd, const guchar *buf, int len)
{
	int all = len;
	int res;

	while (len > 0) {

		res = write (fd, buf, len);
		if (res < 0) {
			if (errno == EAGAIN && errno == EINTR)
				continue;
			if (errno != EPIPE)
				g_warning ("couldn't write %u bytes to client: %s", all,
				           g_strerror (errno));
			return FALSE;
		} else if (res == 0) {
			g_warning ("couldn't write %u bytes to client", all);
			return FALSE;
		} else  {
			len -= res;
			buf += res;
		}
	}

	return TRUE;
}

static gboolean
read_packet_with_size (GkdSshAgentCall *call)
{
	int fd;
	guint32 packet_size;

	fd = call->sock;

	egg_buffer_resize (call->req, 4);
	if (!read_all (fd, call->req->buf, 4))
		return FALSE;

	if (!egg_buffer_get_uint32 (call->req, 0, NULL, &packet_size) ||
	    packet_size < 1) {
		g_warning ("invalid packet size from client");
		return FALSE;
	}

	egg_buffer_resize (call->req, packet_size + 4);
	if (!read_all (fd, call->req->buf + 4, packet_size))
		return FALSE;

	return TRUE;
}

static gpointer
run_client_thread (gpointer data)
{
	gint *socket = data;
	GkdSshAgentCall call;
	EggBuffer req;
	EggBuffer resp;
	guchar op;

	memset (&call, 0, sizeof (call));
	call.sock = g_atomic_int_get (socket);
	g_assert (call.sock != -1);

	egg_buffer_init_full (&req, 128, egg_secure_realloc);
	egg_buffer_init_full (&resp, 128, (EggBufferAllocator)g_realloc);
	call.req = &req;
	call.resp = &resp;
	call.modules = gck_list_ref_copy (pkcs11_modules);

	for (;;) {

		egg_buffer_reset (call.req);

		/* 1. Read in the request */
		if (!read_packet_with_size (&call))
			break;

		/* 2. Now decode the operation */
		if (!egg_buffer_get_byte (call.req, 4, NULL, &op))
			break;
		if (op >= GKD_SSH_OP_MAX)
			break;
		g_assert (gkd_ssh_agent_operations[op]);

		/* 3. Execute the right operation */
		egg_buffer_reset (call.resp);
		egg_buffer_add_uint32 (call.resp, 0);
		if (!(gkd_ssh_agent_operations[op]) (&call))
			break;
		if (!egg_buffer_set_uint32 (call.resp, 0, call.resp->len - 4))
			break;

		/* 4. Write the reply back out */
		if (!write_all (call.sock, call.resp->buf, call.resp->len))
			break;
	}

	egg_buffer_uninit (&req);
	egg_buffer_uninit (&resp);
	gck_list_unref_free (call.modules);
	call.modules = NULL;

	close (call.sock);
	g_atomic_int_set (socket, -1);

	return NULL;
}

/* --------------------------------------------------------------------------------------
 * SESSION MANAGEMENT
 */

/* The main PKCS#11 session that owns objects, and the mutex/cond for waiting on it */
static GckSession *pkcs11_main_session = NULL;
static gboolean pkcs11_main_checked = FALSE;
static GMutex *pkcs11_main_mutex = NULL;
static GCond *pkcs11_main_cond = NULL;

GckSession*
gkd_ssh_agent_checkout_main_session (void)
{
	GckSession *result;

	g_mutex_lock (pkcs11_main_mutex);

		g_assert (GCK_IS_SESSION (pkcs11_main_session));
		while (pkcs11_main_checked)
			g_cond_wait (pkcs11_main_cond, pkcs11_main_mutex);
		pkcs11_main_checked = TRUE;
		result = g_object_ref (pkcs11_main_session);

	g_mutex_unlock (pkcs11_main_mutex);

	return result;
}

void
gkd_ssh_agent_checkin_main_session (GckSession *session)
{
	g_assert (GCK_IS_SESSION (session));

	g_mutex_lock (pkcs11_main_mutex);

		g_assert (session == pkcs11_main_session);
		g_assert (pkcs11_main_checked);

		g_object_unref (session);
		pkcs11_main_checked = FALSE;
		g_cond_signal (pkcs11_main_cond);

	g_mutex_unlock (pkcs11_main_mutex);
}

/* --------------------------------------------------------------------------------------
 * MAIN THREAD
 */

typedef struct _Client {
	GThread *thread;
	gint sock;
} Client;

/* Each client thread in this list */
static GList *socket_clients = NULL;

/* The main socket we listen on */
static int socket_fd = -1;

/* The path of the socket listening on */
static char socket_path[1024] = { 0, };

void
gkd_ssh_agent_accept (void)
{
	Client *client;
	struct sockaddr_un addr;
	socklen_t addrlen;
	GError *error = NULL;
	GList *l;
	int new_fd;

	g_return_if_fail (socket_fd != -1);

	/* Cleanup any completed dispatch threads */
	for (l = socket_clients; l; l = g_list_next (l)) {
		client = l->data;
		if (g_atomic_int_get (&client->sock) == -1) {
			g_thread_join (client->thread);
			g_slice_free (Client, client);
			l->data = NULL;
		}
	}
	socket_clients = g_list_remove_all (socket_clients, NULL);

	addrlen = sizeof (addr);
	new_fd = accept (socket_fd, (struct sockaddr*) &addr, &addrlen);
	if (socket_fd < 0) {
		g_warning ("cannot accept SSH agent connection: %s", strerror (errno));
		return;
	}

	client = g_slice_new0 (Client);
	client->sock = new_fd;

	/* And create a new thread/process */
	client->thread = g_thread_create (run_client_thread, &client->sock, TRUE, &error);
	if (!client->thread) {
		g_warning ("couldn't create thread SSH agent connection: %s",
		           egg_error_message (error));
		g_slice_free (Client, client);
		return;
	}

	socket_clients = g_list_append (socket_clients, client);
}

void
gkd_ssh_agent_shutdown (void)
{
	Client *client;
	GList *l;

	if (socket_fd != -1)
		close (socket_fd);

	if (*socket_path)
		unlink (socket_path);

	/* Stop all of the dispatch threads */
	for (l = socket_clients; l; l = g_list_next (l)) {
		client = l->data;

		/* Forcibly shutdown the connection */
		if (client->sock != -1)
			shutdown (client->sock, SHUT_RDWR);
		g_thread_join (client->thread);

		/* This is always closed by client thread */
		g_assert (client->sock == -1);
		g_slice_free (Client, client);
	}

	g_list_free (socket_clients);
	socket_clients = NULL;
}

void
gkd_ssh_agent_uninitialize (void)
{
	gboolean ret;

	g_assert (pkcs11_main_mutex);
	ret = g_mutex_trylock (pkcs11_main_mutex);
	g_assert (ret);

		g_assert (GCK_IS_SESSION (pkcs11_main_session));
		g_assert (!pkcs11_main_checked);
		g_object_unref (pkcs11_main_session);
		pkcs11_main_session = NULL;

	g_mutex_unlock (pkcs11_main_mutex);
	g_mutex_free (pkcs11_main_mutex);
	g_cond_free (pkcs11_main_cond);

	gck_list_unref_free (pkcs11_modules);
	pkcs11_modules = NULL;
}

int
gkd_ssh_agent_initialize (CK_FUNCTION_LIST_PTR funcs)
{
	GckModule *module;
	gboolean ret;

	g_return_val_if_fail (funcs, -1);

	module = gck_module_new (funcs, 0);
	ret = gkd_ssh_agent_initialize_with_module (module);
	g_object_unref (module);
	return ret;
}

gboolean
gkd_ssh_agent_initialize_with_module (GckModule *module)
{
	GckSession *session = NULL;
	GList *slots, *l;
	GckMechanisms *mechs;
	GError *error = NULL;

	g_assert (GCK_IS_MODULE (module));

	/* Find a good slot for our session keys */
	slots = gck_module_get_slots (module, TRUE);
	for (l = slots; session == NULL && l; l = g_list_next (l)) {

		/* Check that it has the mechanisms we need */
		mechs = gck_slot_get_mechanisms (l->data);
		if (gck_mechanisms_check (mechs, CKM_RSA_PKCS, CKM_DSA, GCK_INVALID)) {

			/* Try and open a session */
			session = gck_slot_open_session (l->data, GCK_SESSION_AUTHENTICATE, NULL, &error);
			if (!session) {
				g_warning ("couldn't create pkcs#11 session: %s", egg_error_message (error));
				g_clear_error (&error);
			}
		}

		gck_mechanisms_free (mechs);
	}

	gck_list_unref_free (slots);

	if (!session) {
		g_warning ("couldn't select a usable pkcs#11 slot for the ssh agent to use");
		return FALSE;
	}

	g_assert (!pkcs11_modules);
	pkcs11_modules = g_list_append (NULL, g_object_ref (module));

	pkcs11_main_mutex = g_mutex_new ();
	pkcs11_main_cond = g_cond_new ();
	pkcs11_main_checked = FALSE;
	pkcs11_main_session = session;

	return TRUE;
}

int
gkd_ssh_agent_startup (const gchar *prefix)
{
	struct sockaddr_un addr;
	int sock;

	g_return_val_if_fail (prefix, -1);

	snprintf (socket_path, sizeof (socket_path), "%s/ssh", prefix);
	unlink (socket_path);

	sock = socket (AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		g_warning ("couldn't create socket: %s", g_strerror (errno));
		return -1;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy (addr.sun_path, socket_path, sizeof (addr.sun_path));
	if (bind (sock, (struct sockaddr *) & addr, sizeof (addr)) < 0) {
		g_warning ("couldn't bind to socket: %s: %s", socket_path, g_strerror (errno));
		close (sock);
		return -1;
	}

	if (listen (sock, 128) < 0) {
		g_warning ("couldn't listen on socket: %s", g_strerror (errno));
		close (sock);
		return -1;
	}

	g_setenv ("SSH_AUTH_SOCK", socket_path, TRUE);

	socket_fd = sock;
	return sock;
}
