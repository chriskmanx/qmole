/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "gkd-gpg-agent.h"
#include "gkd-gpg-agent-private.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t;
#endif

/* The loaded PKCS#11 module */
static GckModule *pkcs11_module = NULL;

#ifndef KL
#define KL(s)               ((sizeof(s) - 1) / sizeof(s[0]))
#endif

static gboolean
write_all (int fd, const gchar *buf, int len)
{
	int all = len;
	int res;

	if (len == -1)
		all = len = strlen (buf);

	while (len > 0) {

		res = write (fd, buf, len);

		if (res <= 0) {
			if (errno == EAGAIN && errno == EINTR)
				continue;
			if (errno != EPIPE)
				g_warning ("couldn't write %u bytes to client: %s", all,
				           res < 0 ? g_strerror (errno) : "");
			return FALSE;
		} else  {
			len -= res;
			buf += res;
		}
	}

	return TRUE;
}

/* Called when seahorse-actions has a response to send back */
gboolean
gkd_gpg_agent_send_reply (GkdGpgAgentCall *call, gboolean ok, const gchar *response)
{
	int fd = g_io_channel_unix_get_fd (call->channel);
	if (!write_all (fd, ok ? GPG_AGENT_OK : GPG_AGENT_ERR, ok ? KL (GPG_AGENT_OK) : KL (GPG_AGENT_ERR)) ||
	    (response && !write_all (fd, response, -1)) ||
	    !write_all (fd, "\n", 1))
		return FALSE;

	return TRUE;
}

gboolean
gkd_gpg_agent_send_data (GkdGpgAgentCall *call, const gchar *data)
{
	int fd = g_io_channel_unix_get_fd (call->channel);
	if (!write_all (fd, GPG_AGENT_DATA, KL (GPG_AGENT_DATA)) ||
	    !write_all (fd, data, -1) ||
	    !write_all (fd, "\n", 1))
		return FALSE;

	return TRUE;
}

/* Process a request line from client */
static gboolean
process_line (GkdGpgAgentCall *call, gchar *line)
{
	gchar *args;

	g_assert (call);
	g_assert (line);

	g_strstrip (line);

	if (!*line)
		return TRUE;

	/* Split the command off from the args */
	args = strchr (line, ' ');
	if (args) {
		*args = 0;
		args++;
	} else {
		/* Pointer to the end, empty string */
		args = line + strlen (line);
	}

	if (g_ascii_strcasecmp (line, GPG_AGENT_OPTION) == 0)
		return gkd_gpg_agent_ops_options (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_GETPASS) == 0)
		return gkd_gpg_agent_ops_getpass (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_CLRPASS) == 0)
		return gkd_gpg_agent_ops_clrpass (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_GETINFO) == 0)
		return gkd_gpg_agent_ops_getinfo (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_NOP) == 0)
		return gkd_gpg_agent_ops_nop (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_BYE) == 0)
		return gkd_gpg_agent_ops_bye (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_RESET) == 0)
		return gkd_gpg_agent_ops_reset (call, args);

	else if (g_ascii_strcasecmp (line, GPG_AGENT_ID) == 0)
		return gkd_gpg_agent_ops_id (call, args);

	else {
		g_message ("unrecognized command: %s", line);
		return gkd_gpg_agent_send_reply (call, FALSE, "103 unknown command");
	}
}

static gpointer
run_client_thread (gpointer data)
{
	gint *socket = data;
	GError *error = NULL;
	GkdGpgAgentCall call;
	GIOStatus status;
	gboolean cont = TRUE;
	gchar *line;
	gsize n_line;

	g_assert (GCK_IS_MODULE (pkcs11_module));

	call.sock = g_atomic_int_get (socket);
	call.channel = g_io_channel_unix_new (call.sock);
	g_io_channel_set_encoding (call.channel, NULL, NULL);
	g_io_channel_set_close_on_unref (call.channel, FALSE);
	call.module = g_object_ref (pkcs11_module);

	/* Initial response on the connection */
	gkd_gpg_agent_send_reply (&call, TRUE, "your orders please");

	while (cont) {
		line = NULL;
		n_line = 0;

		/* Read in a line */
		status = g_io_channel_read_line (call.channel, &line, &n_line, NULL, &error);
		switch (status) {
		case G_IO_STATUS_ERROR:
			g_critical ("gpg agent couldn't read from socket: %s",
			            egg_error_message (error));
			g_clear_error (&error);
			cont = FALSE;
			break;
		case G_IO_STATUS_NORMAL:
			cont = process_line (&call, line);
			g_free (line);
			break;
		case G_IO_STATUS_EOF:
			cont = FALSE;
			break;
		case G_IO_STATUS_AGAIN:
			break;
		default:
			g_return_val_if_reached (NULL);
			break;
		};
	}

	g_io_channel_shutdown (call.channel, FALSE, NULL);
	g_object_unref (call.module);

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
gkd_gpg_agent_checkout_main_session (void)
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
gkd_gpg_agent_checkin_main_session (GckSession *session)
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
 * SETTINGS
 */

/* The cache settings */
static GSettings *cache_settings = NULL;

GSettings*
gkd_gpg_agent_settings (void)
{
	g_return_val_if_fail (cache_settings, NULL);
	return cache_settings;
}

/* --------------------------------------------------------------------------------------
 * MAIN THREAD
 */

typedef struct _Client {
	GThread *thread;
	gint sock;
	gchar *buffer;
	gsize n_buffer;
} Client;

/* Each client thread in this list */
static GList *socket_clients = NULL;

/* The main socket we listen on */
static int socket_fd = -1;

/* The path of the socket listening on */
static char socket_path[1024] = { 0, };

void
gkd_gpg_agent_accept (void)
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
	if (new_fd < 0) {
		g_warning ("cannot accept GPG agent connection: %s", strerror (errno));
		return;
	}

	client = g_slice_new0 (Client);
	client->sock = new_fd;

	/* And create a new thread/process */
	client->thread = g_thread_create (run_client_thread, &client->sock, TRUE, &error);
	if (!client->thread) {
		g_warning ("couldn't create thread GPG agent connection: %s",
		           error && error->message ? error->message : "");
		g_slice_free (Client, client);
		return;
	}

	socket_clients = g_list_append (socket_clients, client);
}

void
gkd_gpg_agent_shutdown (void)
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
gkd_gpg_agent_uninitialize (void)
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

	g_assert (pkcs11_module);
	g_object_unref (pkcs11_module);
	pkcs11_module = NULL;

	g_assert (cache_settings);
	g_object_unref (cache_settings);
	cache_settings = NULL;
}

int
gkd_gpg_agent_initialize (CK_FUNCTION_LIST_PTR funcs)
{
	GckModule *module;
	gboolean ret;

	g_return_val_if_fail (funcs, -1);

	module = gck_module_new (funcs);
	ret = gkd_gpg_agent_initialize_with_module (module);
	g_object_unref (module);
	return ret;
}

gboolean
gkd_gpg_agent_initialize_with_module (GckModule *module)
{
	GckSession *session = NULL;
	GckSlot *slot;
	GError *error = NULL;
	GList *modules;

	g_assert (GCK_IS_MODULE (module));

	/*
	 * Find the right slot.
	 */
	modules = g_list_append (NULL, module);
	slot = gck_modules_token_for_uri (modules, "pkcs11:token=Secret%20Store", &error);
	g_list_free (modules);

	if (!slot) {
		g_warning ("couldn't find secret store module: %s", egg_error_message (error));
		g_clear_error (&error);
		return FALSE;
	}

	/* Try and open a session */
	session = gck_slot_open_session (slot, GCK_SESSION_READ_WRITE | GCK_SESSION_AUTHENTICATE, NULL, &error);
	g_object_unref (slot);

	if (!session) {
		g_warning ("couldn't select a usable pkcs#11 slot for the gpg agent to use");
		g_clear_error (&error);
		return FALSE;
	}

	pkcs11_module = g_object_ref (module);

	pkcs11_main_mutex = g_mutex_new ();
	pkcs11_main_cond = g_cond_new ();
	pkcs11_main_checked = FALSE;
	pkcs11_main_session = session;

	cache_settings = g_settings_new ("org.gnome.crypto.cache");

	return TRUE;
}

int
gkd_gpg_agent_startup (const gchar *prefix)
{
	struct sockaddr_un addr;
	gchar *agent_info;
	int sock;

	g_return_val_if_fail (prefix, -1);

	snprintf (socket_path, sizeof (socket_path), "%s/gpg", prefix);
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

	/*
	 * TODO: This should be <socket>:<pid>:<protocol_version>
	 * Need to figure out a way to get the PID in there.
	 */
	agent_info = g_strdup_printf ("%s:0:1", socket_path);
	g_setenv ("GPG_AGENT_INFO", agent_info, TRUE);
	g_free (agent_info);

	socket_fd = sock;
	return sock;
}
