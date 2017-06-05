/*
 * linc.c: This file is part of the linc library.
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
#include <errno.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <signal.h>
#include <fcntl.h>
#include "linc-private.h"
#include "linc-compat.h"

#include <glib/gstdio.h>

/* whether we do locking or not */
static gboolean link_is_thread_safe = TRUE;
/* an inferior loop/context for std. processing */
GMainLoop             *link_loop = NULL;
static GMainContext   *link_context = NULL;
/* an inferior context for the I/O thread */
static GThread        *link_io_thread = NULL;
static GMainLoop      *link_thread_loop = NULL;
static GMainContext   *link_thread_context = NULL;
static gboolean        link_is_io_in_thread = FALSE;

/* a big global lock for link */
static GMutex  *link_main_lock;
static GCond   *link_main_cond;
/* command dispatch to the I/O loop */
static GMutex  *link_cmd_queue_lock = NULL;
static GCond   *link_cmd_queue_cond = NULL;
static GList   *link_cmd_queue = NULL;

static int link_wakeup_fds[2] = { -1, -1 };
#define LINK_WAKEUP_POLL  link_wakeup_fds [0]
#define LINK_WAKEUP_WRITE link_wakeup_fds [1]
static GSource *link_main_source = NULL;

#ifdef LINK_SSL_SUPPORT
SSL_METHOD *link_ssl_method;
SSL_CTX    *link_ssl_ctx;
#endif

static void link_dispatch_command (gpointer data, gboolean immediate);

gboolean
link_thread_io (void)
{
	gboolean result;

	/* FIXME: re-factor this to avoid locking */
	result = link_io_thread != NULL;

	return result;
}

gboolean
link_thread_safe (void)
{
	return link_is_thread_safe;
}

static gboolean
cmd_is_sync (LinkCommand *cmd)
{
	return (cmd->type == LINK_COMMAND_SET_IO_THREAD) ||
		(cmd->type == LINK_COMMAND_CNX_UNREF);
}

static gboolean
link_mainloop_handle_input (GIOChannel   *source,
			    GIOCondition  condition,
			    gpointer      data)
{
	char c;
	GList *l, *queue;

	g_mutex_lock (link_cmd_queue_lock);

#ifdef HAVE_WINSOCK2_H
	recv (LINK_WAKEUP_POLL, &c, sizeof (c), 0);
#else
	read (LINK_WAKEUP_POLL, &c, sizeof (c));
#endif
	queue = link_cmd_queue;
	link_cmd_queue = NULL;

	g_mutex_unlock (link_cmd_queue_lock);

	for (l = queue; l; l = l->next) {
		gboolean sync;

		sync = cmd_is_sync (l->data);

		link_dispatch_command (l->data, FALSE);

		if (sync) {
			g_mutex_lock (link_cmd_queue_lock);
			((LinkSyncCommand *)l->data)->complete = TRUE;
			g_cond_broadcast (link_cmd_queue_cond);
			g_mutex_unlock (link_cmd_queue_lock);
		}
	}

	g_list_free (queue);

	return TRUE;
}

void
link_exec_command (LinkCommand *cmd)
{
	int  res = 0;

	if (link_in_io_thread ()) {
		link_dispatch_command (cmd, TRUE);
		return;
	}

	LINK_MUTEX_LOCK (link_cmd_queue_lock);

	if (LINK_WAKEUP_WRITE == -1) { /* shutdown main loop */
		LINK_MUTEX_UNLOCK (link_cmd_queue_lock);
		link_dispatch_command (cmd, TRUE);
		return;
	}

	if (!link_cmd_queue) {
		char c = 'L'; /* magic */
#ifdef HAVE_WINSOCK2_H
		while ((res = send (LINK_WAKEUP_WRITE, &c, sizeof (c), 0)) == SOCKET_ERROR  &&
		       (WSAGetLastError () == WSAEWOULDBLOCK));
#else
		while ((res = write (LINK_WAKEUP_WRITE, &c, sizeof (c))) < 0  &&
		       (errno == EAGAIN || errno == EINTR));
#endif
	}

	link_cmd_queue = g_list_append (link_cmd_queue, cmd);

	if (cmd_is_sync (cmd))
		while (!((LinkSyncCommand *)cmd)->complete)
			g_cond_wait (link_cmd_queue_cond,
				     link_cmd_queue_lock);

	LINK_MUTEX_UNLOCK (link_cmd_queue_lock);

	if (res < 0)
		g_error ("Failed to write to linc wakeup socket %d 0x%x(%d) (%d)",
			 res, errno, errno, LINK_WAKEUP_WRITE);
}

#if defined (CONNECTION_DEBUG) && defined (CONNECTION_DEBUG_FLAG)
gboolean link_connection_debug_flag = FALSE;
#endif

/**
 * link_init:
 * @thread_safe: if we want thread safety enabled.
 * 
 * Initialize linc.
 **/
void
link_init (gboolean thread_safe)
{
#if defined (CONNECTION_DEBUG) && defined (CONNECTION_DEBUG_FLAG)
	if (getenv ("LINK_CONNECTION_DEBUG"))
		link_connection_debug_flag = TRUE;
	if (link_connection_debug_flag &&
	    getenv ("LINK_PER_PROCESS_STDERR") &&
	    fileno (stderr) >= 0) {
		char *stderr_file = g_build_filename (g_get_tmp_dir (),
						      g_strdup_printf ("link_debug.%d", getpid ()),
						      NULL);
		int fd;
		fd = g_open (stderr_file, O_WRONLY|O_CREAT, 0666);
		if (fd >= 0) {
			char *prgname = g_get_prgname ();
			d_printf ("Redirecting stderr of %s to %s\n",
				  (prgname ? prgname : "this process"), stderr_file);
			dup2 (fd, fileno (stderr));
			close (fd);
		}
	        d_printf ("stderr redirected here\n");
	}
#endif

	if (thread_safe && !g_thread_supported ())
		g_thread_init (NULL);

	link_is_thread_safe = (thread_safe && g_thread_supported());

	g_type_init ();

#ifdef SIGPIPE
	/*
	 * Link's raison d'etre is for ORBit2 and Bonobo
	 *
	 * In Bonobo, components and containers must not crash if the
	 * remote end crashes.  If a remote server crashes and then we
	 * try to make a CORBA call on it, we may get a SIGPIPE.  So,
	 * for lack of a better solution, we ignore SIGPIPE here.  This
	 * is open for reconsideration in the future.
	 *
	 * When SIGPIPE is ignored, write() calls which would
	 * ordinarily trigger a signal will instead return -1 and set
	 * errno to EPIPE.  So linc will be able to catch these
	 * errors instead of letting them kill the component.
	 *
	 * Possibilities are the MSG_PEEK trick, where you test if the
	 * connection is dead right before doing the writev().  That
	 * approach has two problems:
	 *
	 *   1. There is the possibility of a race condition, where
	 *      the remote end calls right after the test, and right
	 *      before the writev().
	 * 
	 *   2. An extra system call per write might be regarded by
	 *      some as a performance hit.
	 *
	 * Another possibility is to surround the call to writev() in
	 * link_connection_writev (linc-connection.c) with something like
	 * this:
	 *
	 *		link_ignore_sigpipe = 1;
	 *
	 *		result = writev ( ... );
	 *
	 *		link_ignore_sigpipe = 0;
	 *
	 * The SIGPIPE signal handler will check the global
	 * link_ignore_sigpipe variable and ignore the signal if it
	 * is 1.  If it is 0, it can proxy to the user's original
	 * signal handler.  This is a real possibility.
	 */
	signal (SIGPIPE, SIG_IGN);
#endif
	
	link_context = g_main_context_new ();
	link_loop    = g_main_loop_new (link_context, TRUE);
	
#ifdef LINK_SSL_SUPPORT
	SSLeay_add_ssl_algorithms ();
	link_ssl_method = SSLv23_method ();
	link_ssl_ctx = SSL_CTX_new (link_ssl_method);
#endif

	link_main_lock = link_mutex_new ();
	link_cmd_queue_lock = link_mutex_new ();
	if (link_is_thread_safe) {
		link_main_cond = g_cond_new ();
		link_cmd_queue_cond = g_cond_new ();
	}

#ifdef HAVE_WINSOCK2_H
	{
		WSADATA wsadata;
		if (WSAStartup (MAKEWORD (2, 0), &wsadata) != 0)
			g_error ("Windows Sockets could not be initialized");
	}
#endif
}

/**
 * link_main_iteration:
 * @block_for_reply: whether we should wait for a reply
 * 
 * This routine iterates the linc mainloop, which has
 * only the linc sources registered against it.
 **/
void
link_main_iteration (gboolean block_for_reply)
{
	g_main_context_iteration (
		link_context, block_for_reply);
}

/**
 * link_main_pending:
 * 
 * determines if the linc mainloop has any pending work to process.
 * 
 * Return value: TRUE if the linc mainloop has any pending work to process.
 **/
gboolean
link_main_pending (void)
{
	return g_main_context_pending (link_context);
}

/**
 * link_main_loop_run:
 * 
 * Runs the linc mainloop; blocking until the loop is exited.
 **/
void
link_main_loop_run (void)
{
	g_main_loop_run (link_loop);
}

/**
 * link_mutex_new:
 * 
 * Creates a mutex, iff threads are supported, initialized etc.
 * 
 * Return value: a new GMutex, or NULL if one is not required.
 **/
GMutex *
link_mutex_new (void)
{
	if (link_is_thread_safe)
		return g_mutex_new ();
	else
		return NULL;
}

gboolean
link_in_io_thread (void)
{
	return (!link_io_thread ||
		g_thread_self() == link_io_thread);
}

GMainContext *
link_main_get_context (void)
{
	return link_context;
}

/*
 *   This method is unreliable, and for use
 * only for debugging.
 */
gboolean
link_mutex_is_locked (GMutex *lock)
{
#ifdef __GLIBC__
	gboolean result = TRUE;

	if (lock && g_mutex_trylock (lock)) {
		result = FALSE;
		g_mutex_unlock (lock);
	}

	return result;
#else
	/*
	 * On at least Solaris & BSD if we link our
	 * app without -lthread, and pull in ORBit2
	 * with threading enabled, we get NOP pthread
	 * operations. This is fine mostly, but we get
	 * bogus return values from trylock which screws
	 * our debugging.
	 */
	d_printf ("hosed system is_lock-ing\n");
	return TRUE;
#endif
}

void
link_shutdown (void)
{
	if (link_loop) /* break into the linc loop */
		g_main_loop_quit (link_loop);

	if (link_thread_loop)
		g_main_loop_quit (link_thread_loop);

	if (link_io_thread) {
		g_thread_join (link_io_thread);
		link_io_thread = NULL;
	}
}

GMainContext *
link_thread_io_context (void)
{
	return link_thread_context;
}

static gpointer
link_io_thread_fn (gpointer data)
{
	g_main_loop_run (link_thread_loop);

	/* FIXME: need to be able to quit without waiting ... */

	/* Asked to quit - so ...
	 * a) stop accepting inputs [ kill servers ]
	 * b) flush outgoing queued data etc. (oneways)
	 * c) unref all leakable resources.
	 */

	link_connections_close ();

	/* A tad of shutdown */
	LINK_MUTEX_LOCK (link_cmd_queue_lock);
	if (LINK_WAKEUP_WRITE >= 0) {
#ifdef HAVE_WINSOCK2_H
		closesocket (LINK_WAKEUP_WRITE);
		closesocket (LINK_WAKEUP_POLL);
#else
		close (LINK_WAKEUP_WRITE);
		close (LINK_WAKEUP_POLL);
#endif
		LINK_WAKEUP_WRITE = -1;
		LINK_WAKEUP_POLL = -1;
	}
	LINK_MUTEX_UNLOCK (link_cmd_queue_lock);

	if (link_main_source) {
		g_source_destroy (link_main_source);
		g_source_unref (link_main_source);
		link_main_source = NULL;
	}

	return NULL;
}

static void
link_exec_set_io_thread (gpointer data, gboolean immediate)
{
	GError *error = NULL;
	gboolean to_io_thread = TRUE;

	link_lock ();
	if (link_is_io_in_thread) {
		link_unlock ();
		return;
	}

	g_mutex_lock (link_cmd_queue_lock);

	link_is_io_in_thread = TRUE;
	
	link_thread_context = g_main_context_new ();
	link_thread_loop = g_main_loop_new (link_thread_context, TRUE);

	link_connections_move_io_T (to_io_thread);
	link_servers_move_io_T     (to_io_thread);

	if (link_pipe (link_wakeup_fds) < 0)
		g_error ("Can't create CORBA main-thread wakeup pipe");

	link_main_source = link_source_create_watch
		(link_thread_context, LINK_WAKEUP_POLL,
		 NULL, (G_IO_IN | G_IO_PRI),
		 link_mainloop_handle_input, NULL);
	
	link_io_thread = g_thread_create_full
		(link_io_thread_fn, NULL, 256 * 1024, TRUE, FALSE,
		 G_THREAD_PRIORITY_NORMAL, &error);
	
	if (!link_io_thread || error)
		g_error ("Failed to create linc worker thread");

	g_main_loop_quit (link_loop);

	g_mutex_unlock (link_cmd_queue_lock);
	link_unlock ();
}

void
link_set_io_thread (gboolean io_in_thread)
{
	LinkSyncCommand cmd = { { 0 }, 0 };

	cmd.cmd.type = LINK_COMMAND_SET_IO_THREAD;

	link_exec_command (&cmd.cmd);
}

static void
link_dispatch_command (gpointer data, gboolean immediate)
{
	LinkCommand *cmd = data;
	switch (cmd->type) {
	case LINK_COMMAND_SET_CONDITION:
		link_connection_exec_set_condition (data, immediate);
		break;
	case LINK_COMMAND_DISCONNECT:
		link_connection_exec_disconnect (data, immediate);
		break;
	case LINK_COMMAND_SET_IO_THREAD:
		link_exec_set_io_thread (data, immediate);
		break;
	case LINK_COMMAND_CNX_UNREF:
		link_connection_exec_cnx_unref (data, immediate);
		break;
	default:
		g_error ("Unimplemented (%d)", cmd->type);
		break;
	}
}

void
link_lock (void)
{
	if (link_main_lock)
		g_mutex_lock (link_main_lock);
}

void
link_unlock (void)
{
	if (link_main_lock)
		g_mutex_unlock (link_main_lock);
}

void
link_signal (void)
{
	if (link_is_thread_safe && link_is_io_in_thread) {
		g_assert (link_main_cond != NULL);
		g_assert (link_is_locked ());
		g_cond_broadcast (link_main_cond);
	}
}

void
link_wait (void)
{
	if (!(link_is_thread_safe && link_is_io_in_thread)) {
		link_unlock ();
		link_main_iteration (TRUE);
		link_lock ();
	} else {
		g_assert (link_main_cond != NULL);
		g_cond_wait (link_main_cond, link_main_lock);
	}
}



gboolean
link_is_locked (void)
{
	return link_mutex_is_locked (link_main_lock);
}

/* Hack */
guint
link_io_thread_add_timeout (guint       interval,
                            GSourceFunc function,
                            gpointer    data)
{
	guint id;
	GSource *tsrc;

	if (!link_thread_safe())
		return 0;

	tsrc = g_timeout_source_new (interval);
	g_source_set_priority (tsrc, G_PRIORITY_HIGH_IDLE);
	g_source_set_callback (tsrc, function, data, NULL);
	g_source_set_can_recurse (tsrc, TRUE);
	id = g_source_attach (tsrc, link_thread_context);
	g_source_unref (tsrc);

	return id;
}

void
link_io_thread_remove_timeout (guint source_id)
{
	GSource *tsrc;

	if (!source_id)
		return;

	tsrc = g_main_context_find_source_by_id (link_thread_context, source_id);
	g_source_destroy (tsrc);
}
