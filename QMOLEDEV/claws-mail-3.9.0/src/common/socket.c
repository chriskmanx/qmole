/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#if (defined (_XOPEN_SOURCE) && !defined (_BSD_SOURCE))
#define _BSD_SOURCE
#endif

#include <glib.h>
#include <glib/gi18n.h>

#include <sys/time.h>
#include <sys/types.h>
#ifdef G_OS_WIN32
#  include <winsock2.h>
#  ifndef EINPROGRESS
#    define EINPROGRESS WSAEINPROGRESS
#  endif
#  include "w32lib.h"
#else
#  if HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#  endif
#  include <sys/socket.h>
#  include <sys/stat.h>
#  include <sys/un.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <resolv.h>
#  include <netdb.h>
#endif /* G_OS_WIN32 */
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#if HAVE_SYS_SELECT_H
#  include <sys/select.h>
#endif

#include "socket.h"
#include "utils.h"
#include "log.h"
#ifdef USE_GNUTLS
#  include "ssl.h"
#endif

#if USE_GIO
#error USE_GIO is currently not supported
#endif

#if G_IO_WIN32
#define BUFFSIZE	8191
#else
#define BUFFSIZE	8192
#endif


typedef gint (*SockAddrFunc)	(GList		*addr_list,
				 gpointer	 data);

typedef struct _SockConnectData	SockConnectData;
typedef struct _SockLookupData	SockLookupData;
typedef struct _SockAddrData	SockAddrData;
typedef struct _SockSource	SockSource;

struct _SockConnectData {
	gint id;
	gchar *hostname;
	gushort port;
	GList *addr_list;
	GList *cur_addr;
	SockLookupData *lookup_data;
	GIOChannel *channel;
	guint io_tag;
	SockConnectFunc func;
	gpointer data;
	gchar *canonical_name;
};

struct _SockLookupData {
	gchar *hostname;
	pid_t child_pid;
	GIOChannel *channel;
	guint io_tag;
	SockAddrFunc func;
	gpointer data;
	gushort port;
        gint pipe_fds[2];
	gchar *canonical_name;
};

struct _SockAddrData {
	gint family;
	gint socktype;
	gint protocol;
	gint addr_len;
	struct sockaddr *addr;
};

struct _SockSource {
	GSource parent;
	SockInfo *sock;
};

static guint io_timeout = 60;

static GList *sock_connect_data_list = NULL;

static gboolean ssl_sock_prepare	(GSource	*source,
					 gint		*timeout);
static gboolean ssl_sock_check		(GSource	*source);
static gboolean ssl_sock_dispatch	(GSource	*source,
					 GSourceFunc	 callback,
					 gpointer	 user_data);

#ifdef USE_GNUTLS
GSourceFuncs ssl_watch_funcs = {
	ssl_sock_prepare,
	ssl_sock_check,
	ssl_sock_dispatch,
	NULL
};
#endif

static gint sock_connect_with_timeout	(gint			 sock,
					 const struct sockaddr	*serv_addr,
					 gint			 addrlen,
					 guint			 timeout_secs);

#ifndef INET6
static gint sock_connect_by_hostname	(gint		 sock,
					 const gchar	*hostname,
					 gushort	 port);
#else
static gint sock_connect_by_getaddrinfo	(const gchar	*hostname,
					 gushort	 port);
#endif

static SockInfo *sockinfo_from_fd(const gchar *hostname,
				  gushort port,
				  gint sock);
static void sock_address_list_free		(GList		*addr_list);

static gboolean sock_connect_async_cb		(GIOChannel	*source,
						 GIOCondition	 condition,
						 gpointer	 data);
static gint sock_connect_async_get_address_info_cb
						(GList		*addr_list,
						 gpointer	 data);

static gint sock_connect_address_list_async	(SockConnectData *conn_data);

static gboolean sock_get_address_info_async_cb	(GIOChannel	*source,
						 GIOCondition	 condition,
						 gpointer	 data);
static SockLookupData *sock_get_address_info_async
						(const gchar	*hostname,
						 gushort	 port,
						 SockAddrFunc	 func,
						 gpointer	 data);
static gint sock_get_address_info_async_cancel	(SockLookupData	*lookup_data);


gint sock_init(void)
{
#ifdef G_OS_WIN32
	WSADATA wsadata;
	gint result;

	result = WSAStartup(MAKEWORD(2, 2), &wsadata);
	if (result != NO_ERROR) {
		g_warning("WSAStartup() failed\n");
		return -1;
	}
#endif
	return 0;
}

gint sock_cleanup(void)
{
#ifdef G_OS_WIN32
	WSACleanup();
#endif
	return 0;
}

gint sock_set_io_timeout(guint sec)
{
	io_timeout = sec;
	return 0;
}

void refresh_resolvers(void)
{
#ifdef G_OS_UNIX
	static time_t resolv_conf_changed = (time_t)NULL;
	struct stat s;

	/* This makes the glibc re-read resolv.conf, if it changed
	 * since our startup. Maybe that should be #ifdef'ed, I don't
	 * know if it'd work on BSDs.
	 * Why doesn't the glibc do it by itself?
	 */
	if (stat("/etc/resolv.conf", &s) == 0) {
		if (s.st_mtime > resolv_conf_changed) {
			resolv_conf_changed = s.st_mtime;
			res_init();
		}
	} /* else
		we'll have bigger problems. */
#endif /*G_OS_UNIX*/
}

#ifdef G_OS_WIN32
#define SOCKET_IS_VALID(s)      ((s) != INVALID_SOCKET)
#else
#define SOCKET_IS_VALID(s) 	TRUE
#endif

/* Due to the fact that socket under Windows are not represented by
   standard file descriptors, we sometimes need to check whether a
   given file descriptor is actually a socket.  This is done by
   testing for an error.  Returns true under W32 if FD is a socket. */
static int fd_is_w32_socket(gint fd)
{
#ifdef G_OS_WIN32
        gint optval;
        gint retval = sizeof(optval);
        
        return !getsockopt(fd, SOL_SOCKET, SO_TYPE, (char*)&optval, &retval);
#else
        return 0;
#endif 
}

gint fd_connect_inet(gushort port)
{
	gint sock;
	struct sockaddr_in addr;

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (!SOCKET_IS_VALID(sock)) {
#ifdef G_OS_WIN32
		debug_print("fd_connect_inet(): socket() failed: %d\n",
			  WSAGetLastError());
#else
		perror("fd_connect_inet(): socket");
#endif
		return -1;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

	if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		fd_close(sock);
		return -1;
	}

	return sock;
}
gint fd_open_inet(gushort port)
{
	gint sock;
	struct sockaddr_in addr;
	gint val;

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (!SOCKET_IS_VALID(sock)) {
#ifdef G_OS_WIN32
		g_warning("fd_open_inet(): socket() failed: %d\n",
			  WSAGetLastError());
#else
		perror("fd_open_inet(): socket");
#endif
		return -1;
	}

	val = 1;
	if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&val,
		       sizeof(val)) < 0) {
		perror("setsockopt");
		fd_close(sock);
		return -1;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

	if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		perror("bind");
		fd_close(sock);
		return -1;
	}

	if (listen(sock, 1) < 0) {
		perror("listen");
		fd_close(sock);
		return -1;
	}

	return sock;
}

gint fd_connect_unix(const gchar *path)
{
#ifdef G_OS_UNIX
	gint sock;
	struct sockaddr_un addr;

	sock = socket(PF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("sock_connect_unix(): socket");
		return -1;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1);

	if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		close(sock);
		return -1;
	}

	return sock;
#else
	return -1;
#endif
}

gint fd_open_unix(const gchar *path)
{
#ifdef G_OS_UNIX
	gint sock;
	struct sockaddr_un addr;

	sock = socket(PF_UNIX, SOCK_STREAM, 0);

	if (sock < 0) {
		perror("sock_open_unix(): socket");
		return -1;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1);

	if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		gchar *buf = g_strdup_printf("can't bind to %s", path);
		perror(buf);
		g_free(buf);
		close(sock);
		return -1;
	}

	if (listen(sock, 1) < 0) {
		gchar *buf = g_strdup_printf("can't listen on %s", path);
		perror(buf);
		g_free(buf);
		close(sock);
		return -1;		
	}

	return sock;
#else
	return -1;
#endif
}

gint fd_accept(gint sock)
{
	struct sockaddr_in caddr;
	guint caddr_len;

	caddr_len = sizeof(caddr);
	return accept(sock, (struct sockaddr *)&caddr, &caddr_len);
}


static gint set_nonblocking_mode(gint fd, gboolean nonblock)
{
#ifdef G_OS_UNIX
	gint flags;

	flags = fcntl(fd, F_GETFL, 0);
	if (flags < 0) {
		perror("fcntl");
		return -1;
	}

	if (nonblock)
		flags |= O_NONBLOCK;
	else
		flags &= ~O_NONBLOCK;

	return fcntl(fd, F_SETFL, flags);
#else
	return -1;
#endif
}

gint sock_set_nonblocking_mode(SockInfo *sock, gboolean nonblock)
{
	cm_return_val_if_fail(sock != NULL, -1);

	return set_nonblocking_mode(sock->sock, nonblock);
}

static gboolean is_nonblocking_mode(gint fd)
{
#ifdef G_OS_UNIX
	gint flags;

	flags = fcntl(fd, F_GETFL, 0);
	if (flags < 0) {
		perror("fcntl");
		return FALSE;
	}

	return ((flags & O_NONBLOCK) != 0);
#else
	return FALSE;
#endif
}

gboolean sock_is_nonblocking_mode(SockInfo *sock)
{
	cm_return_val_if_fail(sock != NULL, FALSE);

	return is_nonblocking_mode(sock->sock);
}


#ifdef USE_GNUTLS
static gboolean ssl_sock_prepare(GSource *source, gint *timeout)
{
	*timeout = 1;
	return FALSE;
}

static gboolean ssl_sock_check(GSource *source)
{
	SockInfo *sock = ((SockSource *)source)->sock;
	struct timeval timeout = {0, 0};
	fd_set fds;
	GIOCondition condition = 0;
        
	if (!sock || !sock->sock)
		return FALSE;

	condition = sock->condition;

	if ((condition & G_IO_IN) == G_IO_IN &&
	    gnutls_record_check_pending(sock->ssl) != 0)
		return TRUE;

	FD_ZERO(&fds);
	FD_SET(sock->sock, &fds);

	select(sock->sock + 1,
	       (condition & G_IO_IN)  ? &fds : NULL,
	       (condition & G_IO_OUT) ? &fds : NULL,
	       NULL, &timeout);

	return FD_ISSET(sock->sock, &fds) != 0;
}

static gboolean ssl_sock_dispatch(GSource *source, GSourceFunc callback,
			      gpointer user_data)
{
	SockInfo *sock = ((SockSource *)source)->sock;

	if (!sock || !sock->callback || !sock->data)
		return FALSE;

	return sock->callback(sock, sock->condition, sock->data);
}
#endif

static gboolean sock_watch_cb(GIOChannel *source, GIOCondition condition,
			      gpointer data)
{
	SockInfo *sock = (SockInfo *)data;

	if ((condition & sock->condition) == 0)
		return TRUE;

	return sock->callback(sock, sock->condition, sock->data);
}

guint sock_add_watch(SockInfo *sock, GIOCondition condition, SockFunc func,
		     gpointer data)
{
	if (!sock)
		return FALSE;

	sock->callback = func;
	sock->condition = condition;
	sock->data = data;

#ifdef USE_GNUTLS
	if (sock->ssl)
	{
		GSource *source = g_source_new(&ssl_watch_funcs,
					       sizeof(SockSource));
		((SockSource *) source)->sock = sock;
		g_source_set_priority(source, G_PRIORITY_DEFAULT);
		g_source_set_can_recurse(source, FALSE);
		sock->g_source = g_source_attach(source, NULL);
		g_source_unref (source); /* Refcount back down to 1 */
		return sock->g_source;
	}
#endif

	return g_io_add_watch(sock->sock_ch, condition, sock_watch_cb, sock);
}

static gint fd_check_io(gint fd, GIOCondition cond)
{
	struct timeval timeout;
	fd_set fds;

	if (is_nonblocking_mode(fd))
		return 0;

	timeout.tv_sec  = io_timeout;
	timeout.tv_usec = 0;

	FD_ZERO(&fds);
	FD_SET(fd, &fds);

	if (cond == G_IO_IN) {
		select(fd + 1, &fds, NULL, NULL,
		       io_timeout > 0 ? &timeout : NULL);
	} else {
		select(fd + 1, NULL, &fds, NULL,
		       io_timeout > 0 ? &timeout : NULL);
	}

	if (FD_ISSET(fd, &fds)) {
		return 0;
	} else {
		g_warning("Socket IO timeout\n");
		log_error(LOG_PROTOCOL, _("Socket IO timeout.\n"));
		return -1;
	}
}

#ifdef G_OS_UNIX
static sigjmp_buf jmpenv;

static void timeout_handler(gint sig)
{
	siglongjmp(jmpenv, 1);
}
#endif /*G_OS_UNIX*/

static gint sock_connect_with_timeout(gint sock,
				      const struct sockaddr *serv_addr,
				      gint addrlen,
				      guint timeout_secs)
{
	gint ret;
#ifdef G_OS_UNIX
	void (*prev_handler)(gint);
	
	alarm(0);
	prev_handler = signal(SIGALRM, timeout_handler);
	if (sigsetjmp(jmpenv, 1)) {
		alarm(0);
		signal(SIGALRM, prev_handler);
		errno = ETIMEDOUT;
		log_error(LOG_PROTOCOL, _("Connection timed out.\n"));
		return -1;
	}
	alarm(timeout_secs);
#endif

	ret = connect(sock, serv_addr, addrlen);

#ifdef G_OS_UNIX
	alarm(0);
	signal(SIGALRM, prev_handler);
#endif

	return ret;
}

struct hostent *my_gethostbyname(const gchar *hostname)
{
	struct hostent *hp;
#ifdef G_OS_UNIX
	void (*prev_handler)(gint);
	
	alarm(0);
	prev_handler = signal(SIGALRM, timeout_handler);
	if (sigsetjmp(jmpenv, 1)) {
		alarm(0);
		signal(SIGALRM, prev_handler);
		g_printerr("%s: host lookup timed out.\n", hostname);
		log_error(LOG_PROTOCOL, _("%s: host lookup timed out.\n"), hostname);
		errno = 0;
		return NULL;
	}
	alarm(io_timeout);
#endif

	if ((hp = gethostbyname(hostname)) == NULL) {
#ifdef G_OS_UNIX
		alarm(0);
		signal(SIGALRM, prev_handler);
#endif
		g_printerr("%s: unknown host.\n", hostname);
		log_error(LOG_PROTOCOL, _("%s: unknown host.\n"), hostname);
		errno = 0;
		return NULL;
	}

#ifdef G_OS_UNIX
	alarm(0);
	signal(SIGALRM, prev_handler);
#endif

	return hp;
}

#ifndef INET6
static gint my_inet_aton(const gchar *hostname, struct in_addr *inp)
{
#if HAVE_INET_ATON
	return inet_aton(hostname, inp);
#else
#if HAVE_INET_ADDR
	guint32 inaddr;

	inaddr = inet_addr(hostname);
	if (inaddr != -1) {
		memcpy(inp, &inaddr, sizeof(inaddr));
		return 1;
	} else
		return 0;
#else
	return 0;
#endif
#endif /* HAVE_INET_ATON */
}

static gint sock_connect_by_hostname(gint sock, const gchar *hostname,
				     gushort port)
{
	struct hostent *hp;
	struct sockaddr_in ad;

	memset(&ad, 0, sizeof(ad));
	ad.sin_family = AF_INET;
	ad.sin_port = htons(port);

	refresh_resolvers();

	if (!my_inet_aton(hostname, &ad.sin_addr)) {
		if ((hp = my_gethostbyname(hostname)) == NULL) {
			g_printerr("%s: unknown host.\n", hostname);
			errno = 0;
			return -1;
		}

		if (hp->h_length != 4 && hp->h_length != 8) {
			g_printerr("illegal address length received for host %s\n", hostname);
			errno = 0;
			return -1;
		}

		memcpy(&ad.sin_addr, hp->h_addr, hp->h_length);
	}

	return sock_connect_with_timeout(sock, (struct sockaddr *)&ad,
					 sizeof(ad), io_timeout);
}

#else /* INET6 */
static gint sock_connect_by_getaddrinfo(const gchar *hostname, gushort	port)
{
	gint sock = -1, gai_error;
	struct addrinfo hints, *res, *ai;
	gchar port_str[6];

	refresh_resolvers();

	memset(&hints, 0, sizeof(hints));
	/* hints.ai_flags = AI_CANONNAME; */
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = IPPROTO_TCP;

	/* convert port from integer to string. */
	g_snprintf(port_str, sizeof(port_str), "%d", port);

	if ((gai_error = getaddrinfo(hostname, port_str, &hints, &res)) != 0) {
		g_printerr("getaddrinfo for %s:%s failed: %s\n",
			hostname, port_str, gai_strerror(gai_error));
		return -1;
	}

	for (ai = res; ai != NULL; ai = ai->ai_next) {
		sock = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
		if (sock < 0)
			continue;

		if (sock_connect_with_timeout
			(sock, ai->ai_addr, ai->ai_addrlen, io_timeout) == 0)
			break;

		close(sock);
	}

	if (res != NULL)
		freeaddrinfo(res);

	if (ai == NULL)
		return -1;

	return sock;
}
#endif /* !INET6 */

SockInfo *sock_connect(const gchar *hostname, gushort port)
{
#ifdef G_OS_WIN32
	SOCKET sock;
#else
	gint sock;
#endif

#ifdef INET6
	if ((sock = sock_connect_by_getaddrinfo(hostname, port)) < 0)
		return NULL;
#else
#ifdef G_OS_WIN32
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
		g_warning("socket() failed: %d\n", WSAGetLastError());
#else
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror("socket");
#endif /* G_OS_WIN32 */
		return NULL;
	}

	if (sock_connect_by_hostname(sock, hostname, port) < 0) {
		if (errno != 0) perror("connect");
		close(sock);
		return NULL;
	}
#endif /* INET6 */

	return sockinfo_from_fd(hostname, port, sock);
}


static void sock_address_list_free(GList *addr_list)
{
	GList *cur;

	for (cur = addr_list; cur != NULL; cur = cur->next) {
		SockAddrData *addr_data = (SockAddrData *)cur->data;
		g_free(addr_data->addr);
		g_free(addr_data);
	}

	g_list_free(addr_list);
}

/* asynchronous TCP connection */

static gboolean sock_connect_async_cb(GIOChannel *source,
				      GIOCondition condition, gpointer data)
{
	SockConnectData *conn_data = (SockConnectData *)data;
	gint fd;
	gint val;
	guint len;
	SockInfo *sockinfo;

	if (conn_data->io_tag == 0 && conn_data->channel == NULL)
		return FALSE;

	fd = g_io_channel_unix_get_fd(source);

	conn_data->io_tag = 0;
	conn_data->channel = NULL;
	g_io_channel_unref(source);

	len = sizeof(val);
	if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &val, &len) < 0) {
		perror("getsockopt");
		close(fd);
		sock_connect_address_list_async(conn_data);
		return FALSE;
	}

	if (val != 0) {
		close(fd);
		log_error(LOG_PROTOCOL, _("%s:%d: connection failed (%s).\n"),
			  conn_data->hostname, conn_data->port,
			  strerror(val));
		sock_connect_address_list_async(conn_data);
		return FALSE;
	}

	sockinfo = g_new0(SockInfo, 1);
	sockinfo->sock = fd;
#ifndef G_OS_WIN32
	sockinfo->sock_ch = g_io_channel_unix_new(fd);
#else
	sockinfo->sock_ch = g_io_channel_win32_new_socket(fd);
#endif
	sockinfo->hostname = g_strdup(conn_data->hostname);
	sockinfo->port = conn_data->port;
	sockinfo->state = CONN_ESTABLISHED;
	sockinfo->canonical_name = g_strdup(conn_data->canonical_name);

	conn_data->func(sockinfo, conn_data->data);

	sock_connect_async_cancel(conn_data->id);

	return FALSE;
}

static gint sock_connect_async_get_address_info_cb(GList *addr_list,
						   gpointer data)
{
	SockConnectData *conn_data = (SockConnectData *)data;

	conn_data->addr_list = addr_list;
	conn_data->cur_addr = addr_list;
	if (conn_data->lookup_data) {
		conn_data->canonical_name = conn_data->lookup_data->canonical_name;
		conn_data->lookup_data->canonical_name = NULL;
		conn_data->lookup_data = NULL;
	}
	return sock_connect_address_list_async(conn_data);
}

gint sock_connect_async(const gchar *hostname, gushort port,
			SockConnectFunc func, gpointer data)
{
	static gint id = 1;
	SockConnectData *conn_data;

	conn_data = g_new0(SockConnectData, 1);
	conn_data->id = id++;
	conn_data->hostname = g_strdup(hostname);
	conn_data->port = port;
	conn_data->addr_list = NULL;
	conn_data->cur_addr = NULL;
	conn_data->io_tag = 0;
	conn_data->func = func;
	conn_data->data = data;

	conn_data->lookup_data = sock_get_address_info_async
		(hostname, port, sock_connect_async_get_address_info_cb,
		 conn_data);

	if (conn_data->lookup_data == NULL) {
		g_free(conn_data->hostname);
		g_free(conn_data);
		return -1;
	}

	sock_connect_data_list = g_list_append(sock_connect_data_list,
					       conn_data);

	return conn_data->id;
}

gint sock_connect_async_cancel(gint id)
{
	SockConnectData *conn_data = NULL;
	GList *cur;

	for (cur = sock_connect_data_list; cur != NULL; cur = cur->next) {
		if (((SockConnectData *)cur->data)->id == id) {
			conn_data = (SockConnectData *)cur->data;
			break;
		}
	}

	if (conn_data) {
		sock_connect_data_list = g_list_remove(sock_connect_data_list,
						       conn_data);

		if (conn_data->lookup_data)
			sock_get_address_info_async_cancel
				(conn_data->lookup_data);

		if (conn_data->io_tag > 0)
			g_source_remove(conn_data->io_tag);
		if (conn_data->channel) {
			GError *err = NULL;
			g_io_channel_shutdown(conn_data->channel, TRUE, &err);
			if (err)
				g_error_free(err);
			g_io_channel_unref(conn_data->channel);
		}

		sock_address_list_free(conn_data->addr_list);
		g_free(conn_data->canonical_name);
		g_free(conn_data->hostname);
		g_free(conn_data);
	} else {
		g_warning("sock_connect_async_cancel: id %d not found.\n", id);
		return -1;
	}

	return 0;
}

static gint sock_connect_address_list_async(SockConnectData *conn_data)
{
	SockAddrData *addr_data;
	gint sock = -1;

	for (; conn_data->cur_addr != NULL;
	     conn_data->cur_addr = conn_data->cur_addr->next) {
		addr_data = (SockAddrData *)conn_data->cur_addr->data;

		if ((sock = socket(addr_data->family, addr_data->socktype,
				   addr_data->protocol)) < 0) {
			perror("socket");

			continue;
		}

		set_nonblocking_mode(sock, TRUE);

		if (connect(sock, addr_data->addr, addr_data->addr_len) < 0) {
			if (EINPROGRESS == errno) {
				break;
			} else {
				perror("connect");
				close(sock);
			}
		} else {
			break;
		}
	}

	if (conn_data->cur_addr == NULL) {
		conn_data->func(NULL, conn_data->data);
		sock_connect_async_cancel(conn_data->id);
		return -1;
	}

	conn_data->cur_addr = conn_data->cur_addr->next;

#ifndef G_OS_WIN32
	conn_data->channel = g_io_channel_unix_new(sock);
#else
	conn_data->channel = g_io_channel_win32_new_socket(sock);
#endif
	conn_data->io_tag = g_io_add_watch(conn_data->channel, G_IO_IN|G_IO_OUT,
					   sock_connect_async_cb, conn_data);

	return 0;
}

/* asynchronous DNS lookup */

static gboolean sock_get_address_info_async_cb(GIOChannel *source,
					       GIOCondition condition,
					       gpointer data)
{
	SockLookupData *lookup_data = (SockLookupData *)data;
	GList *addr_list = NULL;
	SockAddrData *addr_data;
	gsize bytes_read;
	gint ai_member[4];
	struct sockaddr *addr;
	gchar *canonical_name = NULL;
	gchar len = 0;
	GError *err = NULL;
	
	g_io_channel_set_encoding(source, NULL, &err);
	if (err) {
		g_warning("can unset encoding: %s\n", err->message);
		g_error_free(err);
		return FALSE;
	}
	g_io_channel_set_buffered(source, FALSE);
	if (g_io_channel_read_chars(source, &len, sizeof(len),
			      &bytes_read, &err) == G_IO_STATUS_NORMAL) {
		if (err != NULL) {
			g_warning("g_io_channel_read_chars: %s\n", err->message);
			g_error_free(err);
			return FALSE;
		} 
		if (bytes_read == sizeof(len) && len > 0) {
			gchar *cur = NULL;
			gint todo = len;
			canonical_name = g_malloc0(len + 1);
			cur = canonical_name;
			while (todo > 0) {
				if (g_io_channel_read_chars(source, cur, todo,
				      &bytes_read, &err) != G_IO_STATUS_NORMAL) {
					if (err) {
					      g_warning("canonical name not read %s\n", err->message);
					      g_free(canonical_name);
					      canonical_name = NULL;
					      g_error_free(err);
					      err = NULL;
					      break;
					}
				} else {
					cur += bytes_read;
					todo -= bytes_read;
				}
				if (bytes_read == 0) {
				      g_warning("canonical name not read\n");
				      g_free(canonical_name);
				      canonical_name = NULL;
				      break;
				}
			}
		}	      
	}
	for (;;) {
		if (g_io_channel_read_chars(source, (gchar *)ai_member,
				      sizeof(ai_member), &bytes_read, &err) 
		    != G_IO_STATUS_NORMAL) {
			if (err != NULL) {
				g_warning("g_io_channel_read_chars: addr len %s\n", err->message);
				g_error_free(err);
				err = NULL;
				break;
			} 
		}

		if (bytes_read == 0 || bytes_read != sizeof(ai_member))
			break;

		if (ai_member[0] == AF_UNSPEC) {
			g_warning("DNS lookup failed\n");
			log_error(LOG_PROTOCOL, _("%s:%d: unknown host.\n"),
				lookup_data->hostname, lookup_data->port);
			break;
		}

		addr = g_malloc(ai_member[3]);
		if (g_io_channel_read_chars(source, (gchar *)addr, ai_member[3],
				      &bytes_read, &err) 
		    != G_IO_STATUS_NORMAL) {
			if (err != NULL) {
				g_warning("g_io_channel_read_chars: addr data read %s\n", err->message);
				g_error_free(err);
				err = NULL;
				g_free(addr);
				break;
			} 
		}

		if (bytes_read != ai_member[3]) {
			g_warning("sock_get_address_info_async_cb: "
				  "incomplete address data\n");
			g_free(addr);
			break;
		}

		addr_data = g_new0(SockAddrData, 1);
		addr_data->family = ai_member[0];
		addr_data->socktype = ai_member[1];
		addr_data->protocol = ai_member[2];
		addr_data->addr_len = ai_member[3];
		addr_data->addr = addr;

		addr_list = g_list_append(addr_list, addr_data);
	}

	g_io_channel_shutdown(source, TRUE, &err);
	if (err)
		g_error_free(err);
	g_io_channel_unref(source);

#ifdef G_OS_WIN32
        /* FIXME: We would need to cancel the thread. */
#else
	kill(lookup_data->child_pid, SIGKILL);
	waitpid(lookup_data->child_pid, NULL, 0);
#endif
	lookup_data->canonical_name = canonical_name;

	lookup_data->func(addr_list, lookup_data->data);

	g_free(lookup_data->canonical_name);
	g_free(lookup_data->hostname);
	g_free(lookup_data);

	return FALSE;
}


/* For better readability we use a separate function to implement the
   child code of sock_get_address_info_async.  Note, that under W32
   this is actually not a child but a thread and this is the reason
   why we pass only a void pointer. */
static void address_info_async_child(void *opaque)
{
        SockLookupData *parm = opaque;
#ifdef INET6
        gint gai_err;
        struct addrinfo hints, *res, *ai;
        gchar port_str[6];
#else /* !INET6 */
        struct hostent *hp;
        gchar **addr_list_p;
        struct sockaddr_in ad;
#endif /* INET6 */
        gint ai_member[4] = {AF_UNSPEC, 0, 0, 0};

#ifndef G_OS_WIN32
        close(parm->pipe_fds[0]);
        parm->pipe_fds[0] = -1;
#endif

#ifdef INET6
        memset(&hints, 0, sizeof(hints));
        hints.ai_flags = AI_CANONNAME;
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_protocol = IPPROTO_TCP;

        g_snprintf(port_str, sizeof(port_str), "%d", parm->port);

        gai_err = getaddrinfo(parm->hostname, port_str, &hints, &res);
        if (gai_err != 0) {
		gchar len = 0;
                g_warning("getaddrinfo for %s:%s failed: %s\n",
                          parm->hostname, port_str, gai_strerror(gai_err));
		log_error(LOG_PROTOCOL, _("%s:%s: host lookup failed (%s).\n"),
			  parm->hostname, port_str, gai_strerror(gai_err));
	        fd_write_all(parm->pipe_fds[1], &len,
                     sizeof(len));
                fd_write_all(parm->pipe_fds[1], (gchar *)ai_member,
                             sizeof(ai_member));
                close(parm->pipe_fds[1]);
                parm->pipe_fds[1] = -1;
#ifdef G_OS_WIN32
                _endthread();
#else
                _exit(1);
#endif
        }

	if (res != NULL) {
		if (res->ai_canonname && strlen(res->ai_canonname) < 255) {
			gchar len = strlen(res->ai_canonname);
	                fd_write_all(parm->pipe_fds[1], &len,
                             sizeof(len));
	                fd_write_all(parm->pipe_fds[1], res->ai_canonname,
                             len);			 
		} else {
			gchar len = 0;
	                fd_write_all(parm->pipe_fds[1], &len,
                             sizeof(len));
		}
	} else {
		gchar len = 0;
	        fd_write_all(parm->pipe_fds[1], &len,
                     sizeof(len));
	}

        for (ai = res; ai != NULL; ai = ai->ai_next) {
                ai_member[0] = ai->ai_family;
                ai_member[1] = ai->ai_socktype;
                ai_member[2] = ai->ai_protocol;
                ai_member[3] = ai->ai_addrlen;

                fd_write_all(parm->pipe_fds[1], (gchar *)ai_member,
                             sizeof(ai_member));
                fd_write_all(parm->pipe_fds[1], (gchar *)ai->ai_addr,
                             ai->ai_addrlen);
        }

        if (res != NULL)
                freeaddrinfo(res);
#else /* !INET6 */
        hp = my_gethostbyname(parm->hostname);
        if (hp == NULL || hp->h_addrtype != AF_INET) {
		gchar len = 0;
 	        fd_write_all(parm->pipe_fds[1], &len,
                     sizeof(len));
                fd_write_all(parm->pipe_fds[1], (gchar *)ai_member,
                             sizeof(ai_member));
               close(parm->pipe_fds[1]);
                parm->pipe_fds[1] = -1;
#ifdef G_OS_WIN32
                _endthread();
#else
                _exit(1);
#endif
        }

        ai_member[0] = AF_INET;
        ai_member[1] = SOCK_STREAM;
        ai_member[2] = IPPROTO_TCP;
        ai_member[3] = sizeof(ad);

        memset(&ad, 0, sizeof(ad));
        ad.sin_family = AF_INET;
        ad.sin_port = htons(parm->port);

	if (hp->h_name && strlen(hp->h_name) < 255) {
		gchar len = strlen(hp->h_name);
	        fd_write_all(parm->pipe_fds[1], &len,
                     sizeof(len));
	        fd_write_all(parm->pipe_fds[1], hp->h_name,
                     len);			 
	} else {
		gchar len = 0;
 	        fd_write_all(parm->pipe_fds[1], &len,
                     sizeof(len));
	}
        for (addr_list_p = hp->h_addr_list; *addr_list_p != NULL;
             addr_list_p++) {
                memcpy(&ad.sin_addr, *addr_list_p, hp->h_length);
                fd_write_all(parm->pipe_fds[1], (gchar *)ai_member,
                             sizeof(ai_member));
                fd_write_all(parm->pipe_fds[1], (gchar *)&ad, sizeof(ad));
        }
#endif /* INET6 */

        close(parm->pipe_fds[1]);
        parm->pipe_fds[1] = -1;

#ifdef G_OS_WIN32
        _endthread();
#else
        _exit(0);
#endif
}

static SockLookupData *sock_get_address_info_async(const gchar *hostname,
						   gushort port,
						   SockAddrFunc func,
						   gpointer data)
{
	SockLookupData *lookup_data = NULL;
	
	refresh_resolvers();

        lookup_data = g_new0(SockLookupData, 1);
        lookup_data->hostname = g_strdup(hostname);
        lookup_data->func = func;
        lookup_data->data = data;
        lookup_data->port = port;
        lookup_data->child_pid = (pid_t)(-1);
        lookup_data->pipe_fds[0] = -1;
        lookup_data->pipe_fds[1] = -1;

	if (pipe(lookup_data->pipe_fds) < 0) {
		perror("pipe");
		func(NULL, data);
                g_free (lookup_data->hostname);
                g_free (lookup_data);
		return NULL;
	}

#ifndef G_OS_WIN32
	if ((lookup_data->child_pid = fork()) < 0) {
		perror("fork");
		func(NULL, data);
                g_free (lookup_data->hostname);
                g_free (lookup_data);
		return NULL;
	}

	if (lookup_data->child_pid == 0) {
                /* Child process. */
                address_info_async_child (lookup_data);
                g_assert_not_reached ();
	}
        /* Parent process. */
        close(lookup_data->pipe_fds[1]);
        lookup_data->pipe_fds[1] = -1;
#endif  /*!G_OS_WIN32 */
        
#ifndef G_OS_WIN32
        lookup_data->channel = g_io_channel_unix_new(lookup_data->pipe_fds[0]);
#else
        lookup_data->channel = g_io_channel_win32_new_fd(lookup_data->pipe_fds[0]);
#endif
        lookup_data->io_tag = g_io_add_watch(lookup_data->channel, G_IO_IN,
                                             sock_get_address_info_async_cb,
                                             lookup_data);
#ifdef G_OS_WIN32
	lookup_data->child_pid = _beginthread(
		address_info_async_child, 0, lookup_data);
#endif

	return lookup_data;
}

static gint sock_get_address_info_async_cancel(SockLookupData *lookup_data)
{
	if (lookup_data->io_tag > 0)
		g_source_remove(lookup_data->io_tag);
	if (lookup_data->channel) {
		GError *err = NULL;
		g_io_channel_shutdown(lookup_data->channel, TRUE, &err);
		if (err)
			g_error_free(err);

		g_io_channel_unref(lookup_data->channel);
	}

	if (lookup_data->child_pid > 0) {
#ifdef G_OS_WIN32
                /* FIXME: Need a way to cancel the thread. */
#else
		kill(lookup_data->child_pid, SIGKILL);
		waitpid(lookup_data->child_pid, NULL, 0);
#endif
	}

	g_free(lookup_data->canonical_name);
	g_free(lookup_data->hostname);
	g_free(lookup_data);

	return 0;
}


static SockInfo *sockinfo_from_fd(const gchar *hostname,
				  gushort port,
				  gint sock)
{
	SockInfo *sockinfo;

	sockinfo = g_new0(SockInfo, 1);
	sockinfo->sock = sock;
#ifndef G_OS_WIN32
	sockinfo->sock_ch = g_io_channel_unix_new(sock);
#else
	sockinfo->sock_ch = g_io_channel_win32_new_socket(sock);
#endif
	sockinfo->hostname = g_strdup(hostname);
	sockinfo->port = port;
	sockinfo->state = CONN_ESTABLISHED;

	return sockinfo;
}

static gint fd_read(gint fd, gchar *buf, gint len)
{
	if (fd_check_io(fd, G_IO_IN) < 0)
		return -1;

        if (fd_is_w32_socket(fd))
                return recv(fd, buf, len, 0);
	return read(fd, buf, len);
}

#if USE_GNUTLS
static gint ssl_read(gnutls_session ssl, gchar *buf, gint len)
{
	gint r;

	if (gnutls_record_check_pending(ssl) == 0) {
		if (fd_check_io(GPOINTER_TO_INT(gnutls_transport_get_ptr(ssl)), G_IO_IN) < 0)
			return -1;
	}

	while (1) {
		r = gnutls_record_recv(ssl, buf, len);
		if (r > 0)
			return r;

		switch (r) {
		case 0: /* closed connection */
			return -1;

		case GNUTLS_E_REHANDSHAKE:
			do {
				r = gnutls_handshake(ssl);
			} while (r == GNUTLS_E_AGAIN || r == GNUTLS_E_INTERRUPTED);
			break; /* re-receive */
		case GNUTLS_E_AGAIN:
		case GNUTLS_E_INTERRUPTED:
			errno = EAGAIN;
			return -1;

		default:
			return -1;
		}
	}

}
#endif

gint sock_read(SockInfo *sock, gchar *buf, gint len)
{
	gint ret;

	cm_return_val_if_fail(sock != NULL, -1);

#ifdef USE_GNUTLS
	if (sock->ssl)
		ret = ssl_read(sock->ssl, buf, len);
	else
#endif
		ret = fd_read(sock->sock, buf, len);
	
	if (ret < 0)
		sock->state = CONN_DISCONNECTED;
	return ret;
}

gint fd_write(gint fd, const gchar *buf, gint len)
{
	if (fd_check_io(fd, G_IO_OUT) < 0)
		return -1;

        if (fd_is_w32_socket (fd))
                return send(fd, buf, len, 0);
	return write(fd, buf, len);
}

#if USE_GNUTLS
static gint ssl_write(gnutls_session ssl, const gchar *buf, gint len)
{
	gint ret;

	if (fd_check_io(GPOINTER_TO_INT(gnutls_transport_get_ptr(ssl)), G_IO_OUT) < 0)
		return -1;

	ret = gnutls_record_send(ssl, buf, len);

	switch (ret) {
	case 0:
		return -1;
	case GNUTLS_E_AGAIN:
	case GNUTLS_E_INTERRUPTED:
		return 0;

	default:
		return ret;
	}
}

#endif

gint sock_write(SockInfo *sock, const gchar *buf, gint len)
{
	gint ret;

	cm_return_val_if_fail(sock != NULL, -1);

#ifdef USE_GNUTLS
	if (sock->ssl)
		ret = ssl_write(sock->ssl, buf, len);
	else
#endif
		ret = fd_write(sock->sock, buf, len);

	if (ret < 0)
		sock->state = CONN_DISCONNECTED;
	return ret;
}

gint fd_write_all(gint fd, const gchar *buf, gint len)
{
	gint n, wrlen = 0;

	while (len) {
		if (fd_check_io(fd, G_IO_OUT) < 0)
			return -1;
#ifndef G_OS_WIN32
		signal(SIGPIPE, SIG_IGN);
#endif
		if (fd_is_w32_socket(fd))
			n = send(fd, buf, len, 0);
		else
                        n = write(fd, buf, len);

		if (n <= 0) {
			log_error(LOG_PROTOCOL, _("write on fd%d: %s\n"), fd, strerror(errno));
			return -1;
		}
		len -= n;
		wrlen += n;
		buf += n;
	}

	return wrlen;
}

#ifdef USE_GNUTLS
static gint ssl_write_all(gnutls_session ssl, const gchar *buf, gint len)
{
	gint n, wrlen = 0;

	while (len) {
		n = ssl_write(ssl, buf, len);
		if (n <= 0)
			return -1;
		len -= n;
		wrlen += n;
		buf += n;
	}

	return wrlen;
}
#endif

gint sock_write_all(SockInfo *sock, const gchar *buf, gint len)
{
	gint ret;

	cm_return_val_if_fail(sock != NULL, -1);

#ifdef USE_GNUTLS
	if (sock->ssl)
		ret = ssl_write_all(sock->ssl, buf, len);
	else
#endif
		ret = fd_write_all(sock->sock, buf, len);

	if (ret < 0)
		sock->state = CONN_DISCONNECTED;
	return ret;
}

static gint fd_recv(gint fd, gchar *buf, gint len, gint flags)
{
	if (fd_check_io(fd, G_IO_IN) < 0)
		return -1;

	return recv(fd, buf, len, flags);
}

gint fd_gets(gint fd, gchar *buf, gint len)
{
	gchar *newline, *bp = buf;
	gint n;

	if (--len < 1)
		return -1;

#ifdef G_OS_WIN32
	fd_check_io(fd, G_IO_IN);
	do {
/*
XXX:tm try nonblock
MSKB Article ID: Q147714 
Windows Sockets 2 Service Provider Interface Limitations
Polling with recv(MSG_PEEK) to determine when a complete message 
has arrived.
    Reason and Workaround not available.

Single-byte send() and recv(). 
    Reason: Couple one-byte sends with Nagle disabled.
    Workaround: Send modest amounts and receive as much as possible.
(still unused)
*/
		if (recv(fd, bp, 1, 0) <= 0)
			return -1;
		if (*bp == '\n')
			break;
		bp++;
		len--;
	} while (0 < len);
#else /*!G_OS_WIN32*/
	do {
		if ((n = fd_recv(fd, bp, len, MSG_PEEK)) <= 0)
			return -1;
		if ((newline = memchr(bp, '\n', n)) != NULL)
			n = newline - bp + 1;
		if ((n = fd_read(fd, bp, n)) < 0)
			return -1;
		bp += n;
		len -= n;
	} while (!newline && len);
#endif /*!G_OS_WIN32*/

	*bp = '\0';
	return bp - buf;
}

gint sock_close(SockInfo *sock)
{
	gint ret;

	if (!sock)
		return 0;

	if (sock->sock_ch)
		g_io_channel_unref(sock->sock_ch);

#ifdef USE_GNUTLS
	if (sock->ssl)
		ssl_done_socket(sock);
	if (sock->g_source != 0)
		g_source_remove(sock->g_source);
	sock->g_source = 0;
#endif
#ifdef G_OS_WIN32
	shutdown(sock->sock, 1); /* complete transfer before close */
	ret = closesocket(sock->sock);
#else
	ret = fd_close(sock->sock); 
#endif

	g_free(sock->canonical_name);
	g_free(sock->hostname);
	g_free(sock);

	return ret;
}

gint fd_close(gint fd)
{
	return close(fd);
}
