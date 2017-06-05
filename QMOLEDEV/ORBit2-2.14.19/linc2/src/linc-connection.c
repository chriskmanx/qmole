/*
 * linc-connection.c: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#include <config.h>
#include <stdarg.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#ifdef LINK_SSL_SUPPORT
#  include <openssl/ssl.h>
#endif

#include "linc-private.h"
#include "linc-compat.h"
#include <linc/linc-config.h>
#include <linc/linc-connection.h>

static GObjectClass *parent_class = NULL;
static guint _link_timeout = 0;

enum {
	BROKEN,
	BLOCKING,
	LAST_SIGNAL
};
static guint  signals [LAST_SIGNAL];
static GList *cnx_list = NULL;

#define CNX_LOCK(cnx)    G_STMT_START { link_lock();   } G_STMT_END
#define CNX_UNLOCK(cnx)  G_STMT_START { link_unlock(); } G_STMT_END
#define CNX_LIST_LOCK()      CNX_LOCK(0);   /* for now */
#define CNX_LIST_UNLOCK()    CNX_UNLOCK(0); /* for now */
#define CNX_AND_LIST_LOCK(cnx)   CNX_LOCK(cnx);   /* for now */
#define CNX_AND_LIST_UNLOCK(cnx) CNX_UNLOCK(cnx); /* for now */
#define CNX_IS_LOCKED(cnx) link_is_locked()

static gboolean link_connection_io_handler (GIOChannel  *gioc,
					    GIOCondition condition,
					    gpointer     data);

static inline LinkConnection*
link_connection_ref_T (LinkConnection *cnx) 
{
	return LINK_CONNECTION (g_object_ref (G_OBJECT (cnx)));
}

LinkConnection *
link_connection_ref (LinkConnection *cnx)
{
	CNX_AND_LIST_LOCK (cnx);
	g_object_ref (G_OBJECT (cnx));
	CNX_AND_LIST_UNLOCK (cnx);

	return cnx;
}

/* Only call if we are _certain_ that we don't hold the last ref */
static void
link_connection_unref_T_ (LinkConnection *cnx)
{
	g_assert (((GObject *)cnx)->ref_count > 1);
	g_object_unref (G_OBJECT (cnx));
}

static void
link_connection_unref_unlock (LinkConnection *cnx)
{
	gboolean tail_unref = FALSE;

	if (((GObject *)cnx)->ref_count > 1)
		g_object_unref (G_OBJECT (cnx));

	else {
		cnx_list = g_list_remove (cnx_list, cnx);
		tail_unref = TRUE;
	}

	CNX_AND_LIST_UNLOCK (cnx);

	if (tail_unref) {
		LinkCommandCnxUnref cmd[1];

		cmd->cmd.cmd.type = LINK_COMMAND_CNX_UNREF;
		cmd->cmd.complete = FALSE;
		cmd->cnx = cnx;
		link_exec_command ((LinkCommand *) cmd);
	}
}

void
link_connection_exec_cnx_unref (LinkCommandCnxUnref *cmd, gboolean immediate)
{
	d_printf ("Exec defered unref on %p\n", cmd->cnx);

	if (immediate) /* In I/O thread - with just 1 ref left */
		g_object_unref (G_OBJECT (cmd->cnx));
	else {
		CNX_AND_LIST_LOCK (cmd->cnx);
		link_connection_unref_unlock (cmd->cnx);
	}
}

void
link_connection_unref (LinkConnection *cnx)
{
	g_return_if_fail (cnx != NULL);

	CNX_AND_LIST_LOCK (cnx);

	link_connection_unref_unlock (cnx);
}

static void
link_close_fd (LinkConnection *cnx)
{
	if (cnx->priv->fd >= 0) {
		d_printf ("link_close_fd: closing %d\n", cnx->priv->fd);
		LINK_CLOSE_SOCKET (cnx->priv->fd);
	}
	cnx->priv->fd = -1;
}

typedef struct {
	LinkBrokenCallback fn;
	gpointer           user_data;
} BrokenCallback;

static void
link_connection_emit_broken (LinkConnection *cnx, GSList *callbacks)
{
	GSList *l;

	for (l = callbacks; l; l = l->next) {
		BrokenCallback *bc = l->data;
		bc->fn (cnx, bc->user_data);
		g_free (bc);
	}
	g_slist_free (callbacks);
}

/*
 * Unfortunate to have a global list, but we need to know
 * if this is being processed in the main thread & if so
 * simply append to it.
 */
static GSList *idle_broken_cnxs = NULL;

static gboolean
link_connection_broken_idle (gpointer dummy)
{
	GSList *callbacks;
	LinkConnection *cnx;

	d_printf ("Connection broken idle ...\n");

	do {
		link_lock();
		cnx = NULL;
		if (idle_broken_cnxs != NULL) {
			cnx = idle_broken_cnxs->data;
			idle_broken_cnxs = g_slist_delete_link (idle_broken_cnxs, idle_broken_cnxs);
		}
		if (cnx) {
			callbacks = cnx->idle_broken_callbacks;
			cnx->idle_broken_callbacks = NULL;
			cnx->inhibit_reconnect = FALSE;
			link_signal ();
		}
		link_unlock ();

		if (cnx) {
			link_connection_emit_broken (cnx, callbacks);
			link_connection_unref (cnx);
		}
	} while (cnx != NULL);

	return FALSE;
}

static void
add_idle_broken_for_cnx_T (LinkConnection *cnx)
{
	if (idle_broken_cnxs != NULL) {
		fprintf (stderr, "Deadlock potential - avoiding evil bug!\n");
		/*
		 * just append ourself & exit - we don't want to
		 * inhibit re-connection because of the horrendous
		 * deadlock possible, cf. g#534351#
		 */
		if (g_slist_find (idle_broken_cnxs, cnx) != NULL)
			return;
	} else {
		/* inhibit reconnect while we emit 'broken' */
		cnx->inhibit_reconnect = TRUE;
		g_idle_add (link_connection_broken_idle, NULL);
	}
	link_connection_ref_T (cnx);
	idle_broken_cnxs = g_slist_prepend (idle_broken_cnxs, cnx);
}

static void
link_source_remove (LinkConnection *cnx)
{
	if (cnx->priv->tag) {
		LinkWatch *thewatch = cnx->priv->tag;
		cnx->priv->tag = NULL;
		link_io_remove_watch (thewatch);
		d_printf ("Removed watch on %d\n", cnx->priv->fd);
	}
}

static void
link_source_add (LinkConnection *cnx,
		 GIOCondition    condition)
{
	g_assert (cnx->priv->tag == NULL);

	cnx->priv->tag = link_io_add_watch_fd (
		cnx->priv->fd, condition,
		link_connection_io_handler, cnx);

	d_printf ("Added watch on %d (0x%x)\n",
		 cnx->priv->fd, condition);
}

typedef struct {
	guchar       *data;

	struct iovec *vecs;
	int           nvecs;
	struct iovec  single_vec;
} QueuedWrite;

static void
queued_write_free (QueuedWrite *qw)
{
	g_free (qw->data);
	g_free (qw);
}

static void
queue_free (LinkConnection *cnx)
{
	GList *l;

	for (l = cnx->priv->write_queue; l; l = l->next)
		queued_write_free (l->data);

	g_list_free (cnx->priv->write_queue);
	cnx->priv->write_queue = NULL;
}

static void
dispatch_callbacks_drop_lock (LinkConnection *cnx)
{
	GSList *callbacks;

	callbacks = cnx->idle_broken_callbacks;
	cnx->idle_broken_callbacks = NULL;

	CNX_UNLOCK (cnx);
	link_connection_emit_broken (cnx, callbacks);
	CNX_LOCK (cnx);
}

/*
 * link_connection_class_state_changed:
 * @cnx: a #LinkConnection
 * @status: a #LinkConnectionStatus value.
 *
 * Set up linc's #GSources if the connection is in the #LINK_CONNECTED
 * or #LINK_CONNECTING state.
 *
 * Remove the #GSources if the state has channged to #LINK_DISCONNECTED,
 * close the socket and a gobject broken signal which may be caught by
 * the application.
 *
 * Also perform SSL specific operations if the connection has move into
 * the #LINK_CONNECTED state.
 */
static void
link_connection_state_changed_T_R (LinkConnection      *cnx,
				   LinkConnectionStatus status)
{
	gboolean changed;
	LinkConnectionClass *klass;

	g_assert (CNX_IS_LOCKED (cnx));

	d_printf ("State changing from '%s' to '%s' on fd %d\n",
		 STATE_NAME (cnx->status), STATE_NAME (status),
		 cnx->priv->fd);

	changed = cnx->status != status;

	cnx->status = status;

	switch (status) {
	case LINK_CONNECTED:
#ifdef LINK_SSL_SUPPORT
		if (cnx->options & LINK_CONNECTION_SSL) {
			if (cnx->was_initiated)
				SSL_connect (cnx->priv->ssl);
			else
				SSL_accept (cnx->priv->ssl);
		}
#endif
		if (!cnx->priv->tag)
			link_source_add (cnx, LINK_ERR_CONDS | LINK_IN_CONDS);
		break;

	case LINK_CONNECTING:

		if (cnx->priv->tag) /* re-connecting */
			link_watch_set_condition (
				cnx->priv->tag,
				G_IO_OUT | LINK_ERR_CONDS);
		else
			link_source_add (cnx, G_IO_OUT | LINK_ERR_CONDS);
		break;

	case LINK_DISCONNECTED:
	case LINK_TIMEOUT:
		link_source_remove (cnx);
		link_close_fd (cnx);
		queue_free (cnx);
		/* don't free pending queue - we could get re-connected */
		if (changed) {

			if (!cnx->priv->was_disconnected) {
				d_printf ("Emitting the broken signal on %p\n", cnx);
				CNX_UNLOCK (cnx);
				g_signal_emit (cnx, signals [BROKEN], 0);
				CNX_LOCK (cnx);
			}

			if (cnx->idle_broken_callbacks) {
				if (!link_thread_io ()) {
					d_printf ("Immediate broken callbacks at immediately\n");
				
					dispatch_callbacks_drop_lock (cnx);
				} else {
					d_printf ("Queuing broken callbacks at idle\n");
					add_idle_broken_for_cnx_T (cnx);
				}
			}
		}
		break;
	}

	klass = (LinkConnectionClass *)G_OBJECT_GET_CLASS (cnx);

	if (klass->state_changed) {
		link_signal ();
		CNX_UNLOCK (cnx);
		klass->state_changed (cnx, status);
		CNX_LOCK (cnx);
	}
}

static void
queue_signal_T_R (LinkConnection *cnx,
		  glong           delta)
{
	gulong old_size;
	gulong new_size;

	d_printf ("Queue signal %ld bytes, delta %ld, max %ld\n",
		  cnx->priv->write_queue_bytes, delta,
		  cnx->priv->max_buffer_bytes);

	old_size = cnx->priv->write_queue_bytes;
	cnx->priv->write_queue_bytes += delta;
	new_size = cnx->priv->write_queue_bytes;

	if (cnx->options & LINK_CONNECTION_BLOCK_SIGNAL) {
		if (new_size == 0 ||
		    (old_size < (cnx->priv->max_buffer_bytes >> 1) &&
		     new_size >= (cnx->priv->max_buffer_bytes >> 1)) ||
		    new_size >= cnx->priv->max_buffer_bytes) {
			CNX_UNLOCK (cnx);
			g_signal_emit (cnx, signals [BLOCKING], 0, new_size);
			CNX_LOCK (cnx);
		}
	}

	if (cnx->priv->max_buffer_bytes &&
	    cnx->priv->write_queue_bytes >= cnx->priv->max_buffer_bytes)
		link_connection_state_changed_T_R (cnx, LINK_DISCONNECTED);
}

static gulong
calc_size (struct iovec *src_vecs,
	   int           nvecs)
{
	int i;
	gulong total_size = 0;

	for (i = 0; i < nvecs; i++)
		total_size += src_vecs [i].iov_len;

	return total_size;
}

static void
queue_flattened_T_R (LinkConnection *cnx,
		     struct iovec   *src_vecs,
		     int             nvecs,
		     gboolean        update_poll)
{
	int     i;
	guchar *p;
	gulong  total_size;
	gboolean new_queue;
	QueuedWrite *qw = g_new (QueuedWrite, 1);

	total_size = calc_size (src_vecs, nvecs);

	p = g_malloc (total_size);

	qw->data  = p;
	qw->vecs  = &qw->single_vec;
	qw->nvecs = 1;

	qw->vecs->iov_base = p;
	qw->vecs->iov_len = total_size;

	for (i = 0; i < nvecs; i++) {
		memcpy (p, src_vecs [i].iov_base, src_vecs [i].iov_len);
		p += src_vecs [i].iov_len;
	}
	g_assert (p == (qw->data + total_size));

	d_printf ("Queueing write of %ld bytes on fd %d\n",
		  total_size, cnx->priv->fd);

	new_queue = cnx->priv->write_queue == NULL;
	cnx->priv->write_queue = g_list_append (cnx->priv->write_queue, qw);
	queue_signal_T_R (cnx, total_size);

	if (update_poll && new_queue) {
		LinkCommandSetCondition *cmd;

		cmd = g_new (LinkCommandSetCondition, 1);
		cmd->cmd.type = LINK_COMMAND_SET_CONDITION;
		cmd->cnx = link_connection_ref_T (cnx);
		cmd->condition = (LINK_ERR_CONDS | LINK_IN_CONDS | G_IO_OUT);
		link_exec_command (&cmd->cmd);
	}
}

static void
link_connection_from_fd_T (LinkConnection         *cnx,
			   int                     fd,
			   const LinkProtocolInfo *proto,
			   gchar                  *remote_host_info,
			   gchar                  *remote_serv_info,
			   gboolean                was_initiated,
			   LinkConnectionStatus    status,
			   LinkConnectionOptions   options)
{
	cnx->was_initiated = was_initiated;
	cnx->is_auth       = (proto->flags & LINK_PROTOCOL_SECURE);
	cnx->proto         = proto;
	cnx->options       = options;
	cnx->priv->fd      = fd;

	g_free (cnx->remote_host_info);
	cnx->remote_host_info = remote_host_info;
	g_free (cnx->remote_serv_info);
	cnx->remote_serv_info = remote_serv_info;

	switch (cnx->proto->family) {
	case AF_INET:
#ifdef AF_INET6
	case AF_INET6:
#endif
		if (_link_timeout && !cnx->timeout_msec) /* this should'nt happen twice but I'm always paranoid... */
			cnx->timeout_msec = _link_timeout;
		break;
	default:
		break;
	}

	d_printf ("Cnx from fd (%d) '%s', '%s', '%s'\n",
		 fd, proto->name, 
		 remote_host_info ? remote_host_info : "<Null>",
		 remote_serv_info ? remote_serv_info : "<Null>");

	if (proto->setup)
		proto->setup (fd, options);

#ifdef LINK_SSL_SUPPORT
	if (options & LINK_CONNECTION_SSL) {
		cnx->priv->ssl = SSL_new (link_ssl_ctx);
		SSL_set_fd (cnx->priv->ssl, fd);
	}
#endif
	g_assert (CNX_IS_LOCKED (0));
	link_connection_state_changed_T_R (cnx, status);

	if (!g_list_find (cnx_list, cnx))
		cnx_list = g_list_prepend (cnx_list, cnx);
}

/*
 * link_connection_from_fd:
 * @cnx: a #LinkConnection.
 * @fd: a connected/connecting file descriptor.
 * @proto: a #LinkProtocolInfo.
 * @remote_host_info: protocol dependant host information; gallocation swallowed
 * @remote_serv_info: protocol dependant service information(e.g. port number). gallocation swallowed
 * @was_initiated: #TRUE if the connection was initiated by us.
 * @status: a #LinkConnectionStatus value.
 * @options: combination of #LinkConnectionOptions.
 *
 * Fill in @cnx, call protocol specific initialisation methonds and then
 * call link_connection_state_changed.
 *
 * Return Value: #TRUE if the function succeeds, #FALSE otherwise.
 */
void
link_connection_from_fd (LinkConnection         *cnx,
			 int                     fd,
			 const LinkProtocolInfo *proto,
			 gchar                  *remote_host_info,
			 gchar                  *remote_serv_info,
			 gboolean                was_initiated,
			 LinkConnectionStatus    status,
			 LinkConnectionOptions   options)
{
	CNX_LOCK (cnx);

	link_connection_from_fd_T (cnx, fd, proto,
				   remote_serv_info, remote_serv_info,
				   was_initiated, status, options);
	CNX_UNLOCK (cnx);
}

#ifndef G_OS_WIN32
static void
fix_permissions (const char *filename)
{
	char *tmp_dir = g_strdup (filename);
	char *p;
	struct stat stat_buf;

	if (!tmp_dir)
		return;
	p = strrchr (tmp_dir, '/');
	if (p) {
		*p = '\0';
		stat (tmp_dir, &stat_buf);
		chown (filename, stat_buf.st_uid, -1);
	}
}
#endif

static gboolean
link_connection_do_initiate (LinkConnection        *cnx,
			     const char            *proto_name,
			     const char            *host,
			     const char            *service,
			     LinkConnectionOptions  options)
{
	const LinkProtocolInfo *proto;
	int                     rv;
	int                     fd;
	gboolean                retval = FALSE;
	struct sockaddr        *saddr;
	LinkSockLen		saddr_len;

	proto = link_protocol_find (proto_name);

	if (!proto)
		return FALSE;

	saddr = link_protocol_get_sockaddr (
		proto, host, service, &saddr_len);

	if (!saddr && (strcmp (proto_name, "IPv6") ==0)) {/* Falling back to IPv4 */
		proto = link_protocol_find ("IPv4");
		
		saddr = link_protocol_get_sockaddr (
			proto, host, service, &saddr_len);
	}

	if (!saddr)
		return FALSE;

	fd = socket (proto->family, SOCK_STREAM, 
		     proto->stream_proto_num);
#ifdef HAVE_WINSOCK2_H
	if (fd == INVALID_SOCKET) {
		fd = -1;
		link_map_winsock_error_to_errno ();
	}
#endif

	if (fd < 0) {
		d_printf ("socket() failed: %s\n", link_strerror (errno));
		goto out;
	}

	if (options & LINK_CONNECTION_NONBLOCKING) {
#ifdef HAVE_WINSOCK2_H
		u_long yes = 1;
		if (ioctlsocket (fd, FIONBIO, &yes) == SOCKET_ERROR) {
			link_map_winsock_error_to_errno ();
			d_printf ("ioctlsocket(FIONBIO) failed: %s\n",
				  link_strerror (errno));
			goto out;
		}
#else
		if (fcntl (fd, F_SETFL, O_NONBLOCK) < 0)
			goto out;
#endif
	}

#if defined (F_SETFD) && defined (FD_CLOEXEC)
	if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0)
		goto out;
#endif
#ifdef HAVE_WINSOCK2_H
	{
		SOCKET newfd;

		if (!DuplicateHandle (GetCurrentProcess (), (HANDLE) fd,
				      GetCurrentProcess (), (LPHANDLE) &newfd,
				      0, FALSE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE)) {
			d_printf ("DuplicateHandle failed: %s\n", link_strerror (WSAGetLastError ()));
			return FALSE;
		}
		fd = newfd;
	}
#endif	

#ifndef G_OS_WIN32
	if (!strcmp (proto_name, "UNIX") && getuid() == 0) {
		fix_permissions (service);
	}
#endif

	LINK_TEMP_FAILURE_RETRY_SOCKET (connect (fd, saddr, saddr_len), rv);
#ifdef HAVE_WINSOCK2_H
	if (rv == SOCKET_ERROR) {
		if ((options & LINK_CONNECTION_NONBLOCKING) &&
		    WSAGetLastError () == WSAEWOULDBLOCK) {
			/* connect() for nonblocking sockets always
			 * fails with WSAEWOULDBLOCK. We have to
			 * select() to wait for actual status.
			 */
			fd_set write_fds, except_fds;

			FD_ZERO (&write_fds);
			FD_SET (fd, &write_fds);
			
			FD_ZERO (&except_fds);
			FD_SET (fd, &except_fds);
			
			rv  = select (1, NULL, &write_fds, &except_fds, NULL);
			if (rv == SOCKET_ERROR) {
				rv = -1;
				link_map_winsock_error_to_errno ();
			} else if (FD_ISSET (fd, &write_fds)) {
				rv = 0;
			} else if (FD_ISSET (fd, &except_fds)) {
				rv = -1;
				errno = WSAECONNREFUSED;
			}
		} else {
			rv = -1;
			link_map_winsock_error_to_errno ();
		}
	}
#endif
	if (rv && errno != EINPROGRESS)
		goto out;

	d_printf ("initiate 'connect' on new fd %d\n", fd);

	g_assert (CNX_IS_LOCKED (0));
	link_connection_from_fd_T
		(cnx, fd, proto, 
		 g_strdup (host), g_strdup (service),
		 TRUE, rv ? LINK_CONNECTING : LINK_CONNECTED,
		 options);
	retval = TRUE;

 out:
	if (!retval && fd >= 0) {
		d_printf ("initiation failed: %s\n", link_strerror (errno));
		d_printf ("closing %d\n", fd);
		LINK_CLOSE_SOCKET (fd);
	}

	g_free (saddr);

	return retval;
}

static LinkConnectionStatus
link_connection_wait_connected_T (LinkConnection *cnx)
{
	while (cnx && cnx->status == LINK_CONNECTING)
		link_wait ();

	return cnx ? cnx->status : LINK_DISCONNECTED;
}

LinkConnectionStatus
link_connection_try_reconnect (LinkConnection *cnx)
{
	LinkConnectionStatus status;

	g_return_val_if_fail (LINK_IS_CONNECTION (cnx), LINK_DISCONNECTED);

	CNX_LOCK (cnx);

	d_printf ("Try for reconnection on %p: %d\n",
		  cnx, cnx->inhibit_reconnect);

	while (cnx->inhibit_reconnect) {
		if (g_main_context_acquire (NULL)) {
			d_printf ("Dispatch callbacks in 'main' (mainloop owning) thread\n");
			cnx->inhibit_reconnect = FALSE;
			dispatch_callbacks_drop_lock (cnx);
			g_main_context_release (NULL);
		} else 
			link_wait ();
	}

	switch (cnx->status) {
	case LINK_DISCONNECTED :
	case LINK_TIMEOUT :
		link_connection_do_initiate
			(cnx, cnx->proto->name, cnx->remote_host_info,
			 cnx->remote_serv_info, cnx->options);
		break;
	default :
		g_warning ("trying to re-connect connected cnx.");
		break;
	}

	cnx->priv->was_disconnected = TRUE;
	status = link_connection_wait_connected_T (cnx);
	cnx->priv->was_disconnected = FALSE;

	CNX_UNLOCK (cnx);

	return status;
}

/**
 * link_connection_initiate_list:
 * @derived_type: a #LinkConnection derived type
 * @proto_name: the name of the protocol to use.
 * @host: protocol dependant host information.
 * @service: protocol dependant service information(e.g. port number).
 * @options: combination of #LinkConnectionOptions.
 * @opt_construct_fn: optional constructor fn for new cnx's or NULL
 * @user_data: optional user data for constructor
 * 
 * Looks up a connection in our cnx. list to see if we already
 * have a matching connection; if so returns it, otherwise
 * constructs a new cnx. and retursn that
 * 
 * Return value: an incremented cnx ref.
 **/
LinkConnection *
link_connection_initiate (GType                 derived_type,
			  const char           *proto_name,
			  const char           *remote_host_info,
			  const char           *remote_serv_info,
			  LinkConnectionOptions options,
			  const char           *first_property,
			  ...)
{
	va_list args;
	GList *l;
	gboolean initiated = TRUE;
	LinkConnection *cnx = NULL;
	const LinkProtocolInfo *proto;

	va_start (args, first_property);

	proto = link_protocol_find (proto_name);

	CNX_LIST_LOCK();

	/* FIXME: hash this if it's slow */
	for (l = cnx_list; l; l = l->next) {
		cnx = l->data;

		if (cnx->was_initiated && cnx->proto == proto &&
		    cnx->status != LINK_DISCONNECTED &&
		    ((cnx->options & LINK_CONNECTION_SSL) == (options & LINK_CONNECTION_SSL)) &&
		    !strcmp (remote_host_info, cnx->remote_host_info) &&
		    !strcmp (remote_serv_info, cnx->remote_serv_info)) {
			cnx = link_connection_ref_T (cnx);
			break;
		}
	}

	cnx = l ? l->data : NULL;

	if (!cnx) {
		cnx = LINK_CONNECTION
			(g_object_new_valist (derived_type, first_property, args));

		initiated = link_connection_do_initiate
			(cnx, proto_name, remote_host_info,
			 remote_serv_info, options);
	}
	
	CNX_LIST_UNLOCK();

	if (!initiated) {
		link_connection_unref (cnx);
		cnx = NULL;
	}

	va_end (args);

	return cnx;
}

/*
 * link_connection_state_changed:
 * @cnx: a #LinkConnection.
 * @status: a #LinkConnectionStatus.
 *
 * A wrapper for the #LinkConnectionClass's state change method.
 */
void
link_connection_state_changed (LinkConnection      *cnx,
			       LinkConnectionStatus status)
{
	CNX_LOCK (cnx);
	link_connection_state_changed_T_R (cnx, status);
	CNX_UNLOCK (cnx);
}

/**
 * link_connection_read:
 * @cnx: the connection to write to
 * @buf: a pointer to the start of an array of bytes to read data into
 * @len: the length of the array in bytes to read ingo
 * @block_for_full_read: whether to block for a full read
 * 
 * Warning, block_for_full_read is of limited usefullness.
 *
 * Return value: number of bytes written on success; negative on error.
 **/
glong
link_connection_read (LinkConnection *cnx,
		      guchar         *buf,
		      int             len,
		      gboolean        block_for_full_read)
{
	int bytes_read = 0;

	d_printf ("Read up to %d bytes from fd %d\n", len, cnx->priv->fd);

	if (!len)
		return 0;

	CNX_LOCK (cnx);

	if (cnx->status != LINK_CONNECTED)
		goto fatal_error;

	do {
		int n;

#ifdef LINK_SSL_SUPPORT
		if (cnx->options & LINK_CONNECTION_SSL)
			n = SSL_read (cnx->priv->ssl, buf, len);
		else
#endif
#ifdef HAVE_WINSOCK2_H
			if ((n = recv (cnx->priv->fd, buf, len, 0)) == SOCKET_ERROR) {
				n = -1;
				link_map_winsock_error_to_errno ();
				d_printf ("recv failed: %s\n",
					  link_strerror (errno));
			}
#else
			LINK_TEMP_FAILURE_RETRY_SYSCALL (read (cnx->priv->fd, 
							       buf, 
							       len), n);
#endif
		g_assert (n <= len);

		if (n < 0) {
#ifdef LINK_SSL_SUPPORT
			if (cnx->options & LINK_CONNECTION_SSL) {
				gulong rv;

				rv = SSL_get_error (cnx->priv->ssl, n);

				if ((rv == SSL_ERROR_WANT_READ ||
				     rv == SSL_ERROR_WANT_WRITE) &&
				    (cnx->options & LINK_CONNECTION_NONBLOCKING))
					goto out;
				else
					goto fatal_error;
			} else
#endif
			{
				if (errno == EINTR)
					continue;

				else if (errno == EAGAIN &&
					 (cnx->options & LINK_CONNECTION_NONBLOCKING))
					goto out;

				else if (errno == EBADF) {
					g_warning ("Serious fd usage error %d", cnx->priv->fd);
					goto fatal_error;

				} else
					goto fatal_error;
			}

		} else if (n == 0) {
			d_printf ("we got EOF on fd %d\n", cnx->priv->fd);
			bytes_read = LINK_IO_FATAL_ERROR;
			goto out;
		} else {
			buf += n;
			len -= n;
			bytes_read += n;
#ifdef CONNECTION_DEBUG
			cnx->priv->total_read_bytes += n;
#endif
		}
	} while (len > 0 && block_for_full_read);

#ifdef CONNECTION_DEBUG
	d_printf ("we read %d bytes (total %"G_GUINT64_FORMAT")\n",
		  bytes_read, cnx->priv->total_read_bytes);
#endif

 out:
	CNX_UNLOCK (cnx);
	return bytes_read;

 fatal_error:
	CNX_UNLOCK (cnx);
	return LINK_IO_FATAL_ERROR;
}

/* Determine the maximum size of the iovec vector */

#if defined (MAXIOV) /* HPUX */
# define LINK_IOV_MAX (MAXIOV)
#elif defined (IOV_MAX) /* AIX */
# define LINK_IOV_MAX (IOV_MAX)
#elif defined (_SC_IOV_MAX) /* SGI */
# define LINK_IOV_MAX_INIT (sysconf (_SC_IOV_MAX))
#elif defined (__APPLE__)
/* Even though the write(2) man page mentions it, UIO_MAXIOV is only
 * available if KERNEL is defined on MacOS X 10.1
 */
#  define LINK_IOV_MAX 1024
#elif defined (UIO_MAXIOV) /* Glibc */
# define LINK_IOV_MAX (UIO_MAXIOV)
#else /* Safe Guess */
# define LINK_IOV_MAX 16
#endif

/* If the value requires initialization, define the function here */
#if defined (LINK_IOV_MAX_INIT)
# define LINK_IOV_MAX link_iov_max
  static guint link_iov_max = 0;
  static inline void
  link_iov_max_init () 
  {
    if (link_iov_max == 0)
      {
        gint max;
        G_LOCK_DEFINE_STATIC (link_iov_max);
        G_LOCK (link_iov_max);
        if (link_iov_max == 0)
          {
            max = LINK_IOV_MAX_INIT;
            if (max <= 0)
              max = 16;
            link_iov_max = max;
          }
        G_UNLOCK (link_iov_max);
      }
  }
#else
# define link_iov_max_init()
#endif

static glong
write_data_T (LinkConnection *cnx, QueuedWrite *qw)
{
	glong bytes_written = 0;

	g_return_val_if_fail (cnx->status == LINK_CONNECTED,
			      LINK_IO_FATAL_ERROR);

	link_iov_max_init ();

	while ((qw->nvecs > 0) && (qw->vecs->iov_len > 0)) {
		int n;

		d_printf ("write_data %ld bytes to fd %d - ",
			  calc_size (qw->vecs, qw->nvecs), cnx->priv->fd);

#ifdef LINK_SSL_SUPPORT
		if (cnx->options & LINK_CONNECTION_SSL)
			n = SSL_write (cnx->priv->ssl, qw->vecs->iov_base,
				       qw->vecs->iov_len);
		else
#endif
#ifdef HAVE_WINSOCK2_H
			{
				if (WSASend (cnx->priv->fd, qw->vecs,
					     MIN (qw->nvecs, LINK_IOV_MAX),
					     (LPDWORD) &n, 0, NULL, NULL) == SOCKET_ERROR) {
					if (WSAGetLastError () == WSAEWOULDBLOCK)
						link_win32_watch_set_write_wouldblock (cnx->priv->tag, TRUE);
					n = -1;
					link_map_winsock_error_to_errno ();
					d_printf ("WSASend failed: %s\n",
						  link_strerror (errno));
				} else {
					link_win32_watch_set_write_wouldblock (cnx->priv->tag, FALSE);
				}
			}
#else
			LINK_TEMP_FAILURE_RETRY_SOCKET (writev (cnx->priv->fd,
								qw->vecs,
								MIN (qw->nvecs, LINK_IOV_MAX)), n);
#endif
#ifdef CONNECTION_DEBUG
		d_printf ("wrote %d bytes (total %"G_GUINT64_FORMAT")\n",
			  n,
			  (cnx->priv->total_written_bytes += ((n > 0) ? n : 0),
			   cnx->priv->total_written_bytes));
#endif
		if (n < 0) {
#ifdef LINK_SSL_SUPPORT
			if (cnx->options & LINK_CONNECTION_SSL) {
				gulong rv;
					
				rv = SSL_get_error (cnx->priv->ssl, n);
					
				if ((rv == SSL_ERROR_WANT_READ || 
				     rv == SSL_ERROR_WANT_WRITE) &&
				    cnx->options & LINK_CONNECTION_NONBLOCKING)
					return LINK_IO_QUEUED_DATA;
				else
					return LINK_IO_FATAL_ERROR;
			} else
#endif
			{
				if (errno == EINTR)
					continue;

				else if (errno == EAGAIN &&
					 (cnx->options & LINK_CONNECTION_NONBLOCKING))
					return LINK_IO_QUEUED_DATA;

				else if (errno == EBADF)
					g_warning ("Serious fd usage error %d", cnx->priv->fd);
				
				return LINK_IO_FATAL_ERROR; /* Unhandlable error */
			}

		} else if (n == 0) /* CHECK: is this really an error condition */
			return LINK_IO_FATAL_ERROR;

		else {
			bytes_written += n;

			while (qw->nvecs > 0 && n >= qw->vecs->iov_len) {
				n -= qw->vecs->iov_len;
				qw->nvecs--;
				qw->vecs++;
			}

			if (n) {
				qw->vecs->iov_len  -= n;
				qw->vecs->iov_base = (guchar *)qw->vecs->iov_base + n;
			}
		}
	}

	return bytes_written;
}

static gboolean
link_connection_should_block (LinkConnection      *cnx,
			      const LinkWriteOpts *opt_write_opts)
{
	if (!opt_write_opts)
		return TRUE;

	if (opt_write_opts->block_on_write)
		return TRUE;

	return FALSE;
}

/* Always called in the I/O thread */
static void
link_connection_flush_write_queue_T_R (LinkConnection *cnx)
{
	gboolean done_writes = TRUE;

	if (cnx->priv->write_queue) {
		glong        status;
		QueuedWrite *qw = cnx->priv->write_queue->data;

		status = write_data_T (cnx, qw);

		d_printf ("Wrote queue %ld on fd %d\n", status, cnx->priv->fd);

		if (status >= LINK_IO_OK) {
			cnx->priv->write_queue = g_list_delete_link
				(cnx->priv->write_queue, cnx->priv->write_queue);
			queued_write_free (qw);

			queue_signal_T_R (cnx, -status);
			
			done_writes = (cnx->priv->write_queue == NULL);

		} else {
			if (status == LINK_IO_FATAL_ERROR) {
				d_printf ("Fatal error on queued write\n");
				link_connection_state_changed_T_R (cnx, LINK_DISCONNECTED);
				
			} else {
				d_printf ("Write blocked\n");
				done_writes = FALSE;
			}
		}
	}

	d_printf ("Blocked write queue %s\n", done_writes ?
		  "flushed & empty" : "still active");

	if (done_writes) /* drop G_IO_OUT */
		link_watch_set_condition
			(cnx->priv->tag,
			 LINK_ERR_CONDS | LINK_IN_CONDS);
	else
		link_watch_set_condition
			(cnx->priv->tag,
			 LINK_ERR_CONDS | LINK_IN_CONDS | G_IO_OUT);
}

void
link_connection_exec_set_condition (LinkCommandSetCondition *cmd, gboolean immediate)
{
	d_printf ("Exec defered set condition on %p -> 0x%x\n",
		  cmd->cnx, cmd->condition);

	if (!immediate)
		CNX_LOCK (cmd->cnx);

	link_watch_set_condition (cmd->cnx->priv->tag, cmd->condition);

	if (!immediate)
		link_connection_unref_unlock (cmd->cnx);

	else /* special */
		link_connection_unref_T_ (cmd->cnx);

	g_free (cmd);
}

/**
 * link_connection_writev:
 * @cnx: the connection to write to
 * @vecs: a structure of iovecs to write - this is altered.
 * @nvecs: the number of populated iovecs
 * @opt_write_opts: optional write options, or NULL
 * 
 * This routine writes data to the abstract connection.
 * FIXME: it allows re-enterancy via link_connection_iterate
 *        in certain cases.
 * FIXME: on this basis, the connection can die underneath
 *        our feet.
 * 
 * Return value: 0 on success, non 0 on error.
 **/
LinkIOStatus
link_connection_writev (LinkConnection       *cnx,
			struct iovec         *vecs,
			int                   nvecs,
			const LinkWriteOpts  *opt_write_opts)
{
	QueuedWrite qw;
	int         status;

	CNX_LOCK (cnx);
	link_connection_ref_T (cnx);

	if (link_thread_safe ()) {
		d_printf ("Thread safe writev\n");
		if (cnx->status == LINK_CONNECTING) {
			queue_flattened_T_R (cnx, vecs, nvecs, TRUE);
			link_connection_unref_unlock (cnx);
			return LINK_IO_QUEUED_DATA;
		}
	} else if (cnx->options & LINK_CONNECTION_NONBLOCKING)
		link_connection_wait_connected (cnx);

	if (cnx->status == LINK_DISCONNECTED) {
		link_connection_unref_unlock (cnx);
		return LINK_IO_FATAL_ERROR;
	}

	if (cnx->priv->write_queue) {
		/* FIXME: we should really retry the write here, but we'll
		 * get a POLLOUT for this lot at some stage anyway */
		queue_flattened_T_R (cnx, vecs, nvecs, FALSE);
		link_connection_unref_unlock (cnx);
		return LINK_IO_QUEUED_DATA;
	}

	qw.vecs  = vecs;
	qw.nvecs = nvecs;

 continue_write:
	status = write_data_T (cnx, &qw);

	if (status == LINK_IO_QUEUED_DATA) {
		if (link_thread_safe ()) {
			queue_flattened_T_R (cnx, qw.vecs, qw.nvecs, TRUE);
			link_connection_unref_unlock (cnx);
			return LINK_IO_QUEUED_DATA;
		}

		/* Queue data & listen for buffer space */
		link_watch_set_condition
			(cnx->priv->tag,
			 LINK_ERR_CONDS | LINK_IN_CONDS | G_IO_OUT);

		if (!link_connection_should_block (cnx, opt_write_opts)) {
			queue_flattened_T_R (cnx, qw.vecs, qw.nvecs, FALSE);
			link_connection_unref_unlock (cnx);
			return LINK_IO_QUEUED_DATA;

		} else {
			link_main_iteration (TRUE);
			goto continue_write;
		}

	} else if (status >= LINK_IO_OK)
		status = LINK_IO_OK;

	link_connection_unref_unlock (cnx);

	return status;
}

/**
 * link_connection_write:
 * @cnx: the connection to write to
 * @buf: a pointer to the start of an array of bytes
 * @len: the length of the array in bytes
 * @opt_write_opts: optional write options, or NULL
 * 
 * Writes a contiguous block of data to the abstract connection.
 * 
 * FIXME: it allows re-enterancy via link_connection_iterate
 *        in certain cases.
 * FIXME: on this basis, the connection can die underneath
 *        our feet eg. between the main_iteration and the
 *        g_return_if_fail.
 *
 * Return value: 0 on success, non 0 on error.
 **/
LinkIOStatus
link_connection_write (LinkConnection       *cnx,
		       const guchar         *buf,
		       gulong                len,
		       const LinkWriteOpts  *opt_write_opts)
{
	struct iovec vec;

	vec.iov_base = (guchar *) buf;
	vec.iov_len  = len;

	return link_connection_writev (cnx, &vec, 1, opt_write_opts);
}

static void
link_connection_dispose (GObject *obj)
{
	LinkConnection *cnx = (LinkConnection *)obj;

	d_printf ("dispose connection %p\n", obj);

	link_source_remove (cnx);
	queue_free (cnx);

	parent_class->dispose (obj);
}

static void
link_connection_finalize (GObject *obj)
{
	GSList *l;
	LinkConnection *cnx = (LinkConnection *)obj;

	link_close_fd (cnx);

	for (l = cnx->idle_broken_callbacks; l; l = l->next)
		g_free (l->data);
	g_slist_free (cnx->idle_broken_callbacks);

	g_free (cnx->remote_host_info);
	g_free (cnx->remote_serv_info);

	g_free (cnx->priv);

	if (cnx->timeout_mutex)
		g_mutex_free (cnx->timeout_mutex);
	if (cnx->timeout_source_id) 
		link_io_thread_remove_timeout (cnx->timeout_source_id);

	parent_class->finalize (obj);
}

static void
link_connection_init (LinkConnection *cnx)
{
	d_printf ("create new connection %p\n", cnx);

	cnx->priv = g_new0 (LinkConnectionPrivate, 1);
	cnx->priv->fd = -1;
	cnx->priv->was_disconnected = FALSE;

	cnx->timeout_mutex = NULL;
	cnx->timeout_msec = 0;
	cnx->timeout_source_id = 0;
	cnx->timeout_status = LINK_TIMEOUT_UNKNOWN;
	cnx->tdata = NULL; 

#ifdef CONNECTION_DEBUG
	cnx->priv->total_read_bytes = 0;
	cnx->priv->total_written_bytes = 0;
#endif
}

static void
link_connection_class_init (LinkConnectionClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;

	object_class->dispose  = link_connection_dispose;
	object_class->finalize = link_connection_finalize;

	signals [BROKEN] =
		g_signal_new ("broken",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (LinkConnectionClass, broken),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);

	signals [BLOCKING] =
		g_signal_new ("blocking",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (LinkConnectionClass, blocking),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__ULONG,
			      G_TYPE_NONE, 1, G_TYPE_ULONG);

	parent_class = g_type_class_peek_parent (klass);
}

GType
link_connection_get_type (void)
{
	static GType object_type = 0;

	if (!object_type) {
		static const GTypeInfo object_info = {
			sizeof (LinkConnectionClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) link_connection_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (LinkConnection),
			0,              /* n_preallocs */
			(GInstanceInitFunc) link_connection_init
		};
      
		object_type = g_type_register_static (G_TYPE_OBJECT,
						      "LinkConnection",
						      &object_info,
						      0);
	}  

	return object_type;
}


LinkWriteOpts *
link_write_options_new (gboolean block_on_write)
{
	LinkWriteOpts *write_opts = g_new0 (LinkWriteOpts, 1);

	write_opts->block_on_write = block_on_write;

	return write_opts;
}

void
link_write_options_free (LinkWriteOpts *write_opts)
{
	g_free (write_opts);
}

void
link_connection_set_max_buffer (LinkConnection *cnx,
				gulong          max_buffer_bytes)
{
	g_return_if_fail (cnx != NULL);

	CNX_LOCK (cnx);
	/* FIXME: we might want to check the current buffer size */
	cnx->priv->max_buffer_bytes = max_buffer_bytes;

	CNX_UNLOCK (cnx);
}

static gboolean
link_connection_io_handler (GIOChannel  *gioc,
			    GIOCondition condition,
			    gpointer     data)
{
	LinkConnection      *cnx = data;
	LinkConnectionClass *klass;

	d_printf ("link_connection_io_handler fd %d, 0x%x\n",
		  cnx->priv->fd, condition);

	CNX_LOCK (cnx);
	link_connection_ref_T (cnx);

	klass = (LinkConnectionClass *) G_TYPE_INSTANCE_GET_CLASS (
		data, LINK_TYPE_CONNECTION, LinkConnection);

	if (cnx->status == LINK_CONNECTED &&
	    condition & LINK_IN_CONDS && klass->handle_input) {
		
		d_printf ("Handle input on fd %d\n", cnx->priv->fd);

		CNX_UNLOCK (cnx);
		klass->handle_input (cnx);
		CNX_LOCK (cnx);
	}

	if (cnx->status == LINK_CONNECTED && condition & G_IO_OUT) {
		d_printf ("IO Out - buffer space free ...\n");
		link_connection_flush_write_queue_T_R (cnx);
	}

	if (condition & (LINK_ERR_CONDS | G_IO_OUT)) {
		int rv, n;
		LinkSockLen n_size = sizeof (n);

		switch (cnx->status) {
		case LINK_CONNECTING:
			n = 0;
			rv = getsockopt (cnx->priv->fd, SOL_SOCKET, SO_ERROR, (char *) &n, &n_size);
			if (!rv && !n && condition == G_IO_OUT) {
				d_printf ("State changed to connected on %d\n", cnx->priv->fd);

				link_watch_set_condition (
					cnx->priv->tag,
					LINK_ERR_CONDS | LINK_IN_CONDS);

				link_connection_state_changed_T_R (cnx, LINK_CONNECTED);

				if (cnx->priv->write_queue) {
					d_printf ("Connected, with queued writes, start flush ...\n");
					link_connection_flush_write_queue_T_R (cnx);
				}
			} else {
				d_printf ("Error connecting %d %d %d on fd %d\n",
					   rv, n, errno, cnx->priv->fd);
				link_connection_state_changed_T_R (cnx, LINK_DISCONNECTED);
			}
			break;
		case LINK_CONNECTED: {
			if (condition & LINK_ERR_CONDS) {
				d_printf ("Disconnect on err: %d\n", cnx->priv->fd);
				link_connection_state_changed_T_R (cnx, LINK_DISCONNECTED);
			}
			break;
		}
		default:
			break;
		}
	}

	link_connection_unref_unlock (cnx);

	return TRUE;
}

LinkConnectionStatus
link_connection_get_status (LinkConnection *cnx)
{
	LinkConnectionStatus status;

	CNX_LOCK (cnx);
	status = cnx->status;
	CNX_UNLOCK (cnx);

	d_printf ("Get status on %p = %d\n", cnx, status);

	return status;
}

void
link_connection_exec_disconnect (LinkCommandDisconnect *cmd, gboolean immediate)
{
	d_printf ("Exec defered disconnect on %p\n", cmd->cnx);

	link_connection_state_changed (cmd->cnx, LINK_DISCONNECTED);

	link_connection_unref (cmd->cnx);
	g_free (cmd);
}

void
link_connection_disconnect (LinkConnection *cnx)
{
	LinkCommandDisconnect *cmd;

	cmd = g_new (LinkCommandDisconnect, 1);
	cmd->cmd.type = LINK_COMMAND_DISCONNECT;
	cmd->cnx = link_connection_ref (cnx);

	link_exec_command ((LinkCommand *) cmd);
}

LinkConnectionStatus
link_connection_wait_connected (LinkConnection *cnx)
{
	LinkConnectionStatus status;

	CNX_LOCK (cnx);

	status = link_connection_wait_connected_T (cnx);

	CNX_UNLOCK (cnx);

	return status;
}

void
link_connections_move_io_T (gboolean to_io_thread)
{
	GList *l;
	for (l = cnx_list; l; l = l->next) {
		LinkConnection *cnx = l->data;
		link_watch_move_io (cnx->priv->tag, to_io_thread);
	}
}

void
link_connection_add_broken_cb (LinkConnection    *cnx,
			       LinkBrokenCallback fn,
			       gpointer           user_data)
{
	BrokenCallback *bc = g_new0 (BrokenCallback, 1);

	g_return_if_fail (fn != NULL);

	bc->fn = fn;
	bc->user_data = user_data;

	cnx->idle_broken_callbacks = g_slist_prepend (cnx->idle_broken_callbacks, bc);
}

static gboolean
broken_callback_match (BrokenCallback    *bc,
		       LinkBrokenCallback fn,
		       gpointer           user_data)
{
	return ( (!fn || bc->fn == fn) &&
		 (!user_data || bc->user_data == user_data) );
}

void
link_connection_remove_broken_cb (LinkConnection    *cnx,
				  LinkBrokenCallback opt_fn,
				  gpointer           opt_user_data)
{
	GSList *l, *next;

	CNX_LOCK (cnx);

	for (l = cnx->idle_broken_callbacks; l; l = next) {
		next = l->next;
		if (broken_callback_match (l->data, opt_fn, opt_user_data)) {
			g_free (l->data);
			cnx->idle_broken_callbacks =
				g_slist_delete_link (cnx->idle_broken_callbacks,
						     l);
		}
	}	

	CNX_UNLOCK (cnx);
}

void
link_connections_close (void)
{
	GList *cnx, *l;

	if (!link_in_io_thread ())
		return;

	CNX_LIST_LOCK();
	cnx = cnx_list;
	cnx_list = NULL;
	CNX_LIST_UNLOCK();

	if (!cnx)
		return;

	/* FIXME: Need to shutdown linc connections ... */

	for (l = cnx; l; l = l->next)
		g_object_run_dispose (l->data);

	g_list_free (cnx);
}

void
link_set_timeout (guint msec)
{
	_link_timeout = msec;
}

