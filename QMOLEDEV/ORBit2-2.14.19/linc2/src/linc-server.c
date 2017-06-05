/*
 * linc-server.c: This file is part of the linc library.
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
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include <linc/linc.h>
#include <linc/linc-server.h>
#include <linc/linc-connection.h>

#include "linc-private.h"
#include "linc-compat.h"

enum {
	NEW_CONNECTION,
	LAST_SIGNAL
};
static guint server_signals [LAST_SIGNAL] = { 0 };

static GList *server_list = NULL;
static GObjectClass *parent_class = NULL;

static void
my_cclosure_marshal_VOID__OBJECT (GClosure     *closure,
                                  GValue       *return_value,
                                  guint         n_param_values,
                                  const GValue *param_values,
                                  gpointer      invocation_hint,
                                  gpointer      marshal_data)
{
	typedef void (*GSignalFunc_VOID__OBJECT) (gpointer     data1,
						  GObject     *arg_1,
						  gpointer     data2);
	register GSignalFunc_VOID__OBJECT callback;
	register GCClosure *cc = (GCClosure*) closure;
	register gpointer data1, data2;

	g_return_if_fail (n_param_values >= 2);

	if (G_CCLOSURE_SWAP_DATA (closure)) {
		data1 = closure->data;
		data2 = g_value_peek_pointer (param_values + 0);
	} else {
		data1 = g_value_peek_pointer (param_values + 0);
		data2 = closure->data;
	}
	callback = (GSignalFunc_VOID__OBJECT) (
		marshal_data ? marshal_data : cc->callback);

	callback (data1,
		  g_value_peek_pointer (param_values + 1),
		  data2);
}

static void
link_server_init (LinkServer *srv)
{
	srv->priv = g_new0 (LinkServerPrivate, 1);

	srv->priv->fd = -1;
}


static void
link_server_client_connection_broken (LinkConnection *connection,
				      gpointer        user_data)
{
	LinkServer *server = user_data;

	server->priv->connections =
		g_slist_remove (server->priv->connections, connection);

	link_connection_unref (connection);
}

static void
link_server_dispose (GObject *obj)
{
	GSList     *l;
	LinkServer *srv = (LinkServer *) obj;

	server_list = g_list_remove (server_list, srv);

	d_printf ("Dispose / close server fd %d\n", srv->priv->fd);

	if (srv->priv->tag) {
		LinkWatch *thewatch = srv->priv->tag;
		srv->priv->tag = NULL;
		link_io_remove_watch (thewatch);
	}

	link_protocol_destroy_cnx (srv->proto,
				   srv->priv->fd, 
				   srv->local_host_info,
				   srv->local_serv_info);
	srv->priv->fd = -1;

	while ((l = srv->priv->connections)) {
		GObject *o = l->data;

		g_signal_handlers_disconnect_by_func (o,
						      link_server_client_connection_broken,
						      srv);

		srv->priv->connections = l->next;
		g_slist_free_1 (l);
		link_connection_unref (LINK_CONNECTION (o));
	}

	parent_class->dispose (obj);
}

static void
link_server_finalize (GObject *obj)
{
	LinkServer *srv = (LinkServer *)obj;

	g_free (srv->local_host_info);
	g_free (srv->local_serv_info);

	g_free (srv->priv);

	parent_class->finalize (obj);
}

static LinkConnection *
link_server_create_connection (LinkServer *srv)
{
	return g_object_new (link_connection_get_type (), NULL);
}

static gboolean
link_server_accept_connection (LinkServer      *server,
			       LinkConnection **connection)
{
	LinkServerClass *klass;
	struct sockaddr *saddr;
	int              addrlen, fd;
	
	g_return_val_if_fail (connection != NULL, FALSE);

	*connection = NULL;

	addrlen = server->proto->addr_len;
	saddr = g_alloca (addrlen);

	LINK_TEMP_FAILURE_RETRY_SOCKET (accept (server->priv->fd, 
						saddr, 
						&addrlen), fd);
#ifdef HAVE_WINSOCK2_H
	if (fd == INVALID_SOCKET) {
		fd = -1;
		link_map_winsock_error_to_errno ();
	}
#endif
	if (fd < 0) {
		d_printf ("accept on %d failed: %s\n",
			  server->priv->fd, link_strerror (errno));
		return FALSE;
	}

	if (server->create_options & LINK_CONNECTION_LOCAL_ONLY &&
	    !link_protocol_is_local (server->proto, saddr, addrlen)) {
		d_printf ("link_server_accept_connection: not local, closing %d\n", fd);
		LINK_CLOSE_SOCKET (fd);
		return FALSE;
	}

	if (server->create_options & LINK_CONNECTION_NONBLOCKING) {
#ifdef HAVE_WINSOCK2_H
		u_long yes = 1;
		if (ioctlsocket (fd, FIONBIO, &yes) == SOCKET_ERROR) {
			link_map_winsock_error_to_errno ();
			d_printf ("failed to set FIONBIO on %d: %s\n",
				  fd, link_strerror (errno));
			d_printf ("closing %d\n", fd);
			LINK_CLOSE_SOCKET (fd);
			return FALSE;
		}
#else
		if (fcntl (fd, F_SETFL, O_NONBLOCK) < 0) {
			d_printf ("failed to set O_NONBLOCK on %d: %s\n",
				  fd, link_strerror (errno));
			d_printf ("closing %d\n", fd);
			LINK_CLOSE_SOCKET (fd);
			return FALSE;
		}
#endif
	}
#if defined (F_SETFD) && defined (FD_CLOEXEC)
	if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0) {
		d_printf ("failed to set cloexec on %d: %s\n",
			  fd, link_strerror (errno));
		d_printf ("closing %d\n", fd);
		LINK_CLOSE_SOCKET (fd);
		return FALSE;
	}
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

	klass = (LinkServerClass *) G_OBJECT_GET_CLASS (server);

	g_assert (klass->create_connection);
	*connection = klass->create_connection (server);

	g_return_val_if_fail (*connection != NULL, FALSE);

	d_printf ("accepted a new connection (%d) on server %d\n",
		 fd, server->priv->fd);

	link_connection_from_fd
		(*connection, fd, server->proto, NULL, NULL,
		 FALSE, LINK_CONNECTED, server->create_options);

	g_signal_connect (*connection, "broken",
			  G_CALLBACK (link_server_client_connection_broken), server);
	
	server->priv->connections = g_slist_prepend (
		server->priv->connections, *connection);

	return TRUE;
}

static gboolean
link_server_handle_io (GIOChannel  *gioc,
		       GIOCondition condition,
		       gpointer     data)
{
	gboolean        accepted;
	LinkServer     *server = data;
	LinkConnection *connection = NULL;

	if (!(condition & LINK_IN_CONDS)) {
		/*
		 * This call to g_warning was changed from g_error to avoid
		 * a program crash. See bug #126209.
		 */
		d_printf ("error condition on server fd is %#x\n", condition);
		return TRUE;
	}	

	accepted = link_server_accept_connection (server, &connection);

	if (!accepted) {
		GValue parms[2];

		memset (parms, 0, sizeof (parms));
		g_value_init (parms, G_OBJECT_TYPE (server));
		g_value_set_object (parms, G_OBJECT (server));
		g_value_init (parms + 1, G_TYPE_OBJECT);

		/* FIXME: this connection is always NULL */
		g_value_set_object (parms + 1, (GObject *) connection);

		d_printf ("p %d, Non-accepted input on fd %d\n",
			  getpid (), server->priv->fd);
		
		g_signal_emitv (parms, server_signals [NEW_CONNECTION], 0, NULL);
		
		g_value_unset (parms);
		g_value_unset (parms + 1);
	}

	return TRUE;
}

/**
 * link_server_setup:
 * @srv: the connection to setup
 * @proto_name: the protocol to use
 * @local_host_info: the local hsot
 * @local_serv_info: remote server info
 * @create_options: various create options
 * 
 *   Setup the server object. You should create a server object
 * via g_object_new and then set it up, using this method.
 * 
 * Return value: the initialized server
 **/
gboolean
link_server_setup (LinkServer            *srv,
		   const char            *proto_name,
		   const char            *local_host_info,
		   const char            *local_serv_info,
		   LinkConnectionOptions  create_options)
{
	const LinkProtocolInfo *proto;
	int                     fd, n;
	struct sockaddr        *saddr;
	LinkSockLen             saddr_len;
	const char             *local_host;
	char                   *service, *hostname;

#ifndef LINK_SSL_SUPPORT
	if (create_options & LINK_CONNECTION_SSL)
		return FALSE;
#endif

	proto = link_protocol_find (proto_name);
	if (!proto) {
		d_printf ("Can't find proto '%s'\n", proto_name);
		return FALSE;
	}

	srv->proto = proto;

	if (local_host_info)
		local_host = local_host_info;
	else
		local_host = link_get_local_hostname ();

address_in_use:

	saddr = link_protocol_get_sockaddr (
		proto, local_host, local_serv_info, &saddr_len);

	if (!saddr) {
		d_printf ("Can't get_sockaddr proto '%s' '%s'\n",
			  local_host, local_serv_info ? local_serv_info : "(null)");
		return FALSE;
	}

	fd = socket (proto->family, SOCK_STREAM, 
		     proto->stream_proto_num);
#ifdef HAVE_WINSOCK2_H
	if (fd == INVALID_SOCKET) {
		fd = -1;
		link_map_winsock_error_to_errno ();
	}
#endif
	if (fd < 0) {
		g_free (saddr);
		d_printf ("socket (%d, SOCK_STREAM, %d) failed: %s\n",
			  proto->family, proto->stream_proto_num,
			  link_strerror (errno));
		return FALSE;
	}

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

	{
		static const int oneval = 1;

		setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, (const char *) &oneval, sizeof (oneval));
	}
    
	n = 0;
	errno = 0;

	if ((proto->flags & LINK_PROTOCOL_NEEDS_BIND) || local_serv_info)
		n = bind (fd, saddr, saddr_len);

	if (proto->post_create)
		(*proto->post_create) (fd, saddr);

#ifdef HAVE_WINSOCK2_H
	if (n == SOCKET_ERROR) {
		n = -1;
		link_map_winsock_error_to_errno ();
	}
#endif
	if (n && errno == EADDRINUSE) {
               LINK_CLOSE_SOCKET(fd);
               if (!local_serv_info) {
                   d_printf ("bind failed; retrying\n");
                   goto address_in_use;
               }
	}

	if (n)
		d_printf ("bind failed: %s\n", link_strerror (errno));

	if (!n) {
		n = listen (fd, 10);
#ifdef HAVE_WINSOCK2_H
		if (n == SOCKET_ERROR) {
			n = -1;
			link_map_winsock_error_to_errno ();
		}
#endif
		if (n)
			d_printf ("listen failed: %s\n", link_strerror (errno));
	}

	if (!n &&
	    create_options & LINK_CONNECTION_NONBLOCKING) {
#ifdef HAVE_WINSOCK2_H
		u_long yes = 1;
		if (ioctlsocket (fd, FIONBIO, &yes) == SOCKET_ERROR) {
			n = -1;
			link_map_winsock_error_to_errno ();
		}
#else
		n = fcntl (fd, F_SETFL, O_NONBLOCK);
#endif
		if (n)
			d_printf ("failed to set nonblock on %d: %s\n",
				  fd, link_strerror (errno));
	}

#if defined (F_SETFD) && defined (FD_CLOEXEC)
	if (!n) {
		n = fcntl (fd, F_SETFD, FD_CLOEXEC);
		if (n)
			d_printf ("failed to set cloexec on %d: %s",
				  fd, link_strerror (errno));
	}
#endif

	if (!n) {
		n = getsockname (fd, saddr, &saddr_len);
#ifdef HAVE_WINSOCK2_H
		if (n == SOCKET_ERROR) {
			n = -1;
			link_map_winsock_error_to_errno ();
		}
#endif
		if (n) {
			link_protocol_destroy_addr (proto, fd, saddr);
			d_printf ("getsockname failed: %s\n",
				  link_strerror (errno));
			return FALSE;
		}
	}

	if (!link_protocol_get_sockinfo (proto, saddr, &hostname, &service)) {
		link_protocol_destroy_addr (proto, fd, saddr);
		d_printf ("link_getsockinfo failed.\n");
		return FALSE;
	}

	g_free (saddr);

	srv->priv->fd = fd;

	if (create_options & LINK_CONNECTION_NONBLOCKING) {
		g_assert (srv->priv->tag == NULL);

		srv->priv->tag = link_io_add_watch_fd (
			fd, LINK_IN_CONDS | LINK_ERR_CONDS,
			link_server_handle_io, srv);
	}

	srv->create_options = create_options;

	if (local_host_info) {
		g_free (hostname);
		srv->local_host_info = g_strdup (local_host_info);
	} else
		srv->local_host_info = hostname;

	srv->local_serv_info = service;

	server_list = g_list_prepend (server_list, srv);

	d_printf ("Created a new server fd (%d) '%s', '%s', '%s'\n",
		 fd, proto->name, 
		 hostname ? hostname : "<Null>",
		 service ? service : "<Null>");

	return TRUE;
}

static void
link_server_class_init (LinkServerClass *klass)
{
	GType         ptype;
	GClosure     *closure;
	GObjectClass *object_class = (GObjectClass *) klass;

	object_class->dispose    = link_server_dispose;
	object_class->finalize   = link_server_finalize;
	klass->create_connection = link_server_create_connection;

	parent_class = g_type_class_peek_parent (klass);
	closure = g_signal_type_cclosure_new (
		G_OBJECT_CLASS_TYPE (klass),
		G_STRUCT_OFFSET (LinkServerClass, new_connection));

	ptype = G_TYPE_OBJECT;
	server_signals [NEW_CONNECTION] = g_signal_newv (
		"new_connection",
		G_OBJECT_CLASS_TYPE (klass),
		G_SIGNAL_RUN_LAST, closure,
		NULL, NULL,
		my_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,
		1, &ptype);
}

GType
link_server_get_type (void)
{
	static GType object_type = 0;

	if (!object_type) {
		static const GTypeInfo object_info = {
			sizeof (LinkServerClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) link_server_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (LinkServer),
			0,              /* n_preallocs */
			(GInstanceInitFunc) link_server_init
		};
      
		object_type = g_type_register_static (
			G_TYPE_OBJECT, "LinkServer",
			&object_info, 0);
	}  

	return object_type;
}

void
link_servers_move_io_T (gboolean to_io_thread)
{
	GList *l;

	for (l = server_list; l; l = l->next) {
		LinkServer *srv = l->data;
		link_watch_move_io (srv->priv->tag, to_io_thread);
	}
}
