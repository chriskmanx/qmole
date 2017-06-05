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
#include <netdb.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include <linc/linc.h>
#include <linc/linc-server.h>
#include <linc/linc-connection.h>

#include "linc-debug.h"
#include "linc-private.h"
#include "linc-compat.h"

enum {
	NEW_CONNECTION,
	LAST_SIGNAL
};
static guint server_signals [LAST_SIGNAL] = { 0 };

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
linc_server_init (LINCServer *cnx)
{
	cnx->priv = g_new0 (LINCServerPrivate, 1);

	cnx->priv->mutex = linc_mutex_new ();
	cnx->priv->fd = -1;
}

static void
linc_server_dispose (GObject *obj)
{
	GSList     *l;
	LINCServer *cnx = (LINCServer *) obj;

	d_printf ("Dispose / close server fd %d\n", cnx->priv->fd);

#ifdef G_THREADS_ENABLED
	if (cnx->priv->mutex) {
		g_mutex_free (cnx->priv->mutex);
		cnx->priv->mutex = NULL;
	}
#endif
	if (cnx->priv->tag) {
		LincWatch *thewatch = cnx->priv->tag;
		cnx->priv->tag = NULL;
		linc_io_remove_watch (thewatch);
	}

	linc_protocol_destroy_cnx (cnx->proto,
				   cnx->priv->fd, 
				   cnx->local_host_info,
				   cnx->local_serv_info);
	cnx->priv->fd = -1;

	while ((l = cnx->priv->connections)) {
		GObject *o = l->data;

		cnx->priv->connections = l->next;
		g_slist_free_1 (l);
		g_object_unref (o);
	}

	parent_class->dispose (obj);
}

static void
linc_server_finalize (GObject *obj)
{
	LINCServer *cnx = (LINCServer *)obj;

	g_free (cnx->local_host_info);
	g_free (cnx->local_serv_info);

	g_free (cnx->priv);

	parent_class->finalize (obj);
}

static LINCConnection *
linc_server_create_connection (LINCServer *cnx)
{
	return g_object_new (linc_connection_get_type (), NULL);
}

static gboolean
linc_server_accept_connection (LINCServer      *server,
			       LINCConnection **connection)
{
	LINCServerClass *klass;
	struct sockaddr *saddr;
	int              addrlen, fd;
	
	g_return_val_if_fail (connection != NULL, FALSE);

	*connection = NULL;

	addrlen = server->proto->addr_len;
	saddr = g_alloca (addrlen);

	fd = accept (server->priv->fd, saddr, &addrlen);
	if (fd < 0) {
		d_printf ("accept on %d failed %d", server->priv->fd, errno);
		return FALSE; /* error */
	}

	if (server->create_options & LINC_CONNECTION_LOCAL_ONLY &&
	    !linc_protocol_is_local (server->proto, saddr, addrlen)) {
		LINC_CLOSE (fd);
		return FALSE;
	}

	if (server->create_options & LINC_CONNECTION_NONBLOCKING)
		if (fcntl (fd, F_SETFL, O_NONBLOCK) < 0) {
			d_printf ("failed to set O_NONBLOCK on %d", fd);
			LINC_CLOSE (fd);
			return FALSE;
		}

	if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0) {
		d_printf ("failed to set cloexec on %d", fd);
		LINC_CLOSE (fd);
		return FALSE;
	}

	klass = (LINCServerClass *) G_OBJECT_GET_CLASS (server);

	g_assert (klass->create_connection);
	*connection = klass->create_connection (server);

	g_return_val_if_fail (*connection != NULL, FALSE);

	d_printf ("accepted a new connection (%d) on server %d\n",
		 fd, server->priv->fd);

	if (!linc_connection_from_fd (
		*connection, fd, server->proto, NULL, NULL,
		FALSE, LINC_CONNECTED, server->create_options)) {
		
		g_object_unref (G_OBJECT (*connection));
		*connection = NULL;
		LINC_CLOSE (fd);
		return FALSE;
	}

	server->priv->connections = g_slist_prepend (
		server->priv->connections, *connection);

	return TRUE;
}

static gboolean
linc_server_handle_io (GIOChannel  *gioc,
		       GIOCondition condition,
		       gpointer     data)
{
	gboolean        accepted;
	LINCServer     *server = data;
	LINCConnection *connection = NULL;

	if (!(condition & LINC_IN_CONDS))
		g_error ("error condition on server fd is %#x", condition);

	LINC_MUTEX_LOCK (server->priv->mutex);

	accepted = linc_server_accept_connection (server, &connection);

	LINC_MUTEX_UNLOCK (server->priv->mutex);

	if (!accepted) {
		GValue parms[2];

		memset (parms, 0, sizeof (parms));
		g_value_init (parms, G_OBJECT_TYPE (server));
		g_value_set_object (parms, G_OBJECT (server));
		g_value_init (parms + 1, G_TYPE_OBJECT);

		/* FIXME: this connection is always NULL */
		g_value_set_object (parms + 1, (GObject *) connection);

		d_printf ("p %d, Non-accepted input on fd %d",
			  getpid (), server->priv->fd);
		
		g_signal_emitv (parms, server_signals [NEW_CONNECTION], 0, NULL);
		
		g_value_unset (parms);
		g_value_unset (parms + 1);
	}

	return TRUE;
}

/**
 * linc_server_setup:
 * @cnx: the connection to setup
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
linc_server_setup (LINCServer            *cnx,
		   const char            *proto_name,
		   const char            *local_host_info,
		   const char            *local_serv_info,
		   LINCConnectionOptions  create_options)
{
	const LINCProtocolInfo *proto;
	int                     fd, n;
	struct sockaddr        *saddr;
	LincSockLen             saddr_len;
	const char             *local_host;
	char                   *service, *hostname;

#if !LINC_SSL_SUPPORT
	if (create_options & LINC_CONNECTION_SSL)
		return FALSE;
#endif

	proto = linc_protocol_find (proto_name);
	if (!proto) {
		d_printf ("Can't find proto '%s'\n", proto_name);
		return FALSE;
	}

	if (local_host_info)
		local_host = local_host_info;
	else
		local_host = linc_get_local_hostname ();

 address_in_use:

	saddr = linc_protocol_get_sockaddr (
		proto, local_host, local_serv_info, &saddr_len);

	if (!saddr) {
		d_printf ("Can't get_sockaddr proto '%s' '%s'\n",
			  local_host, local_serv_info ? local_serv_info : "(null)");
		return FALSE;
	}

	fd = socket (proto->family, SOCK_STREAM, 
		     proto->stream_proto_num);
	if (fd < 0) {
		g_free (saddr);
		d_printf ("socket (%d, %d, %d) failed\n",
			 proto->family, SOCK_STREAM, 
			 proto->stream_proto_num);
		return FALSE;
	}

	{
		static const int oneval = 1;

		setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, &oneval, sizeof (oneval));
	}
    
	n = 0;
	errno = 0;

	if ((proto->flags & LINC_PROTOCOL_NEEDS_BIND) || local_serv_info)
		n = bind (fd, saddr, saddr_len);

	if (n && errno == EADDRINUSE) {
		d_printf ("bind failed; retrying");
		goto address_in_use;
	}

	if (!n)
		n = listen (fd, 10);
	else
		d_printf ("bind really failed errno: %d\n", errno);

	if (!n)
		n = fcntl (fd, F_SETFD, FD_CLOEXEC);
	else
		d_printf ("listen failed errno: %d\n", errno);

	if (!n)
		n = getsockname (fd, saddr, &saddr_len);
	else
		d_printf ("failed to set cloexec on %d", fd);

	if (n) {
		linc_protocol_destroy_addr (proto, fd, saddr);
		d_printf ("get_sockname failed errno: %d\n", errno);
		return FALSE;
	}

	if (!linc_protocol_get_sockinfo (proto, saddr, &hostname, &service)) {
		linc_protocol_destroy_addr (proto, fd, saddr);
		d_printf ("linc_getsockinfo failed.\n");
		return FALSE;
	}

	g_free (saddr);

	cnx->proto = proto;
	cnx->priv->fd = fd;

	if (create_options & LINC_CONNECTION_NONBLOCKING) {
		g_assert (cnx->priv->tag == NULL);

		cnx->priv->tag = linc_io_add_watch_fd (
			fd, LINC_IN_CONDS | LINC_ERR_CONDS,
			linc_server_handle_io, cnx);
	}

	cnx->create_options = create_options;

	if (local_host_info) {
		g_free (hostname);
		cnx->local_host_info = g_strdup (local_host_info);
	} else
		cnx->local_host_info = hostname;

	cnx->local_serv_info = service;

	d_printf ("Created a new server fd (%d) '%s', '%s', '%s'\n",
		 fd, proto->name, 
		 hostname ? hostname : "<Null>",
		 service ? service : "<Null>");

	return TRUE;
}

static void
linc_server_class_init (LINCServerClass *klass)
{
	GType         ptype;
	GClosure     *closure;
	GObjectClass *object_class = (GObjectClass *) klass;

	object_class->dispose    = linc_server_dispose;
	object_class->finalize   = linc_server_finalize;
	klass->create_connection = linc_server_create_connection;

	parent_class = g_type_class_peek_parent (klass);
	closure = g_signal_type_cclosure_new (
		G_OBJECT_CLASS_TYPE (klass),
		G_STRUCT_OFFSET (LINCServerClass, new_connection));

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
linc_server_get_type (void)
{
	static GType object_type = 0;

	if (!object_type) {
		static const GTypeInfo object_info = {
			sizeof (LINCServerClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) linc_server_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (LINCServer),
			0,              /* n_preallocs */
			(GInstanceInitFunc) linc_server_init,
		};
      
		object_type = g_type_register_static (
			G_TYPE_OBJECT, "LINCServer",
			&object_info, 0);
	}  

	return object_type;
}
