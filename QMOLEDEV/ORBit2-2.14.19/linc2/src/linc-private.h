/*
 * linc-private.h: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#ifndef _LINK_PRIVATE_H_
#define _LINK_PRIVATE_H_

#include "config.h"
#include <linc/linc.h>

#include "linc-debug.h"

#ifdef LINK_SSL_SUPPORT

#include <openssl/ssl.h>
#include <openssl/bio.h>
extern SSL_METHOD *link_ssl_method;
extern SSL_CTX *link_ssl_ctx;

#endif /* LINK_SSL_SUPPORT */

typedef struct {
	enum {
		LINK_COMMAND_DISCONNECT,
		LINK_COMMAND_SET_CONDITION,
		LINK_COMMAND_SET_IO_THREAD,
		LINK_COMMAND_CNX_UNREF
	} type;
} LinkCommand;

typedef struct {
	LinkCommand     cmd;
	gboolean        complete;
} LinkSyncCommand;

typedef struct {
	LinkCommand     cmd;
	LinkConnection *cnx;
	GIOCondition    condition;
} LinkCommandSetCondition;

typedef struct {
	LinkCommand     cmd;
	LinkConnection *cnx;
} LinkCommandDisconnect;

typedef struct {
	LinkSyncCommand     cmd;
	LinkConnection *cnx;
} LinkCommandCnxUnref;

void link_exec_command (LinkCommand *cmd);
void link_connection_exec_disconnect (LinkCommandDisconnect *cmd, gboolean immediate);
void link_connection_exec_set_condition (LinkCommandSetCondition *cmd, gboolean immediate);
void link_connection_exec_cnx_unref (LinkCommandCnxUnref *cmd, gboolean immediate);

/*
 * Really raw internals, exported for the tests
 */

struct _LinkServerPrivate {
	int        fd;
	LinkWatch *tag;
	GSList    *connections;
};

struct _LinkWriteOpts {
	gboolean block_on_write;
};

struct _LinkConnectionPrivate {
#ifdef LINK_SSL_SUPPORT
	SSL         *ssl;
#endif
	LinkWatch   *tag;
	int          fd;

	gulong       max_buffer_bytes;
	gulong       write_queue_bytes;
	GList       *write_queue;
	/*
	 * This flag is used after a LincConnection is disconnected when
	 * an attempt to made to retry the connection. If the attempt returns
	 * EINPROGRESS and subsequently is reported as disconnected we want
	 * to avoid emitting another "broken" signal.
	 */
	gboolean     was_disconnected;
#ifdef CONNECTION_DEBUG
	guint64      total_read_bytes;
	guint64      total_written_bytes;
#endif
};

typedef struct {
	GSource       source;

        GIOChannel   *channel;
	GPollFD       pollfd;
#ifdef G_OS_WIN32
	LinkWatch    *link_watch;
	SOCKET	      socket;
	int	      event_mask;
	gboolean      write_would_have_blocked;
#endif
	GIOCondition  condition;
	GIOFunc       callback;
	gpointer      user_data;
} LinkUnixWatch;

struct _LinkWatch {
	GSource *main_source;
	GSource *link_source;
#ifdef G_OS_WIN32
	LinkUnixWatch *last_polled_source;
#endif
};

#define LINK_ERR_CONDS (G_IO_ERR|G_IO_HUP|G_IO_NVAL)
#define LINK_IN_CONDS  (G_IO_PRI|G_IO_IN)

#ifdef G_OS_WIN32

/* The functions in the Microsoft C library that correspond to
 * system calls in Unix don't have any EINTR failure mode.
 */
#define LINK_TEMP_FAILURE_RETRY_SYSCALL(expression, val) \
    { val = (int) (expression); }

#else

/* taken from  glibc  */ 
#define LINK_TEMP_FAILURE_RETRY_SYSCALL(expression, val) \
    { long int __result;                                 \
       do __result = (long int) (expression);            \
       while (__result == -1L && errno == EINTR);        \
       val = __result; }

#endif

#ifdef HAVE_WINSOCK2_H

/* In WinSock2 WSAEINTR can't happen according to the docs, but
 * it doesn't hurt to check anyway, does it?
 */
#define LINK_TEMP_FAILURE_RETRY_SOCKET(expression, val)                   \
    { int __result;                                                       \
       do __result = (int) (expression);                                  \
       while (__result == SOCKET_ERROR && WSAGetLastError() == WSAEINTR); \
       val = __result; }

#define LINK_CLOSE_SOCKET(fd)  while (closesocket (fd) == SOCKET_ERROR && WSAGetLastError() == WSAEINTR)

#else

/* On Unix socket API calls are no different from normal system calls */

#define LINK_TEMP_FAILURE_RETRY_SOCKET(expression, val) \
    LINK_TEMP_FAILURE_RETRY_SYSCALL(expression, val)

#define LINK_CLOSE_SOCKET(fd)  while (close (fd) < 0 && errno == EINTR)

#endif

struct sockaddr *link_protocol_get_sockaddr (const LinkProtocolInfo *proto,
					     const char             *hostname,
					     const char             *service,
					     LinkSockLen            *saddr_len);

gboolean         link_protocol_get_sockinfo (const LinkProtocolInfo *proto,
					     const struct sockaddr  *saddr,
					     gchar                 **hostname,
					     gchar                 **service);

gboolean         link_protocol_is_local     (const LinkProtocolInfo  *proto,
					     const struct sockaddr   *saddr,
					     LinkSockLen              saddr_len);

void             link_protocol_destroy_cnx  (const LinkProtocolInfo  *proto,
					     int                      fd,
					     const char              *host,
					     const char              *service);

void             link_protocol_destroy_addr (const LinkProtocolInfo  *proto,
					     int                      fd,
					     struct sockaddr         *saddr);

LinkWatch       *link_io_add_watch_fd       (int                     fd,
					     GIOCondition            condition,
					     GIOFunc                 func,
					     gpointer                user_data);

void             link_io_remove_watch       (LinkWatch              *w);
void             link_watch_set_condition   (LinkWatch              *w,
					     GIOCondition            condition);
void             link_watch_move_io         (LinkWatch              *w,
					     gboolean                to_io_thread);

GMainContext    *link_main_get_context      (void);
GMainContext    *link_thread_io_context     (void);
gboolean         link_in_io_thread          (void);
gboolean         link_mutex_is_locked       (GMutex *lock);
void             link_lock                  (void);
void             link_unlock                (void);
gboolean         link_is_locked             (void);
void             link_servers_move_io_T     (gboolean to_io_thread);
void             link_connections_move_io_T (gboolean to_io_thread);

#ifdef G_OS_WIN32
void		 link_win32_watch_set_write_wouldblock (LinkWatch    *watch,
							gboolean      flag);
#endif

#endif /* _LINK_PRIVATE_H */
