/*
 * linc-source.c: This file is part of the linc library.
 *
 * Authors:
 *    Owen Taylor   (owen@redhat.com)
 *    Michael Meeks (michael@ximian.com)
 *    Tor Lillqvist (tml@novell.com)
 *
 * Copyright 1998, 2001, 2005, Red Hat, Inc., Ximian, Inc., Novell, Inc.
 */

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#include <glib.h>

#include "linc-compat.h"
#include "linc-private.h"

#if defined (G_OS_WIN32) && defined (CONNECTION_DEBUG)

static char *
fd_mask (int mask)
{
  static char buf[100];
  int checked_bits = 0;
  char *bufp = buf;

  if (mask == 0)
    return "";

#define BIT(n) checked_bits |= FD_##n; if (mask & FD_##n) bufp += sprintf (bufp, "%s" #n, (bufp>buf ? "|" : ""))

  BIT (READ);
  BIT (WRITE);
  BIT (OOB);
  BIT (ACCEPT);
  BIT (CONNECT);
  BIT (CLOSE);
  BIT (QOS);
  BIT (GROUP_QOS);
  BIT (ROUTING_INTERFACE_CHANGE);
  BIT (ADDRESS_LIST_CHANGE);
  
#undef BIT

  if ((mask & ~checked_bits) != 0)
	  bufp += sprintf (bufp, "|%#x", mask & ~checked_bits);
  
  return buf;
}

#endif

static gboolean 
link_source_prepare (GSource *source,
		     gint    *timeout)
{
#ifdef G_OS_WIN32
	LinkUnixWatch *watch = (LinkUnixWatch *) source;
	int event_mask = 0;

	if (watch->condition & G_IO_IN)
		event_mask |= (FD_READ | FD_ACCEPT);
	if (watch->condition & G_IO_OUT)
		event_mask |= (FD_WRITE | FD_CONNECT);
	if (watch->condition & G_IO_HUP)
		event_mask |= FD_CLOSE;

	if (watch->link_watch != NULL &&
	    watch->link_watch->last_polled_source != watch) {
		watch->link_watch->last_polled_source = watch;
		watch->event_mask = 0;
	}

	if (watch->event_mask != event_mask) {
#ifdef CONNECTION_DEBUG
		d_printf ("prepare: WSAEventSelect(%d, %#x, {%s})\n",
			  watch->socket, watch->pollfd.fd, fd_mask (event_mask));
#endif
		if (WSAEventSelect (watch->socket, (HANDLE) watch->pollfd.fd,
				    event_mask) == SOCKET_ERROR)
			d_printf ("WSAEventSelect() failed: %s\n",
				  link_strerror (WSAGetLastError ()));
		else
			watch->event_mask = event_mask;
	}
#endif

	*timeout = -1;

	return FALSE;
}

static gboolean 
link_source_check (GSource *source)
{
	LinkUnixWatch *watch = (LinkUnixWatch *)source;

#ifdef G_OS_WIN32
	WSANETWORKEVENTS events;

	d_printf ("check: sock=%d handle=%#x revents=%x condition=%x ",
		  watch->socket, watch->pollfd.fd,
		  watch->pollfd.revents, watch->condition);

	if (WSAEnumNetworkEvents (watch->socket,
				  /* (HANDLE) watch->pollfd.fd, */ 0,
				  &events) == SOCKET_ERROR)
		d_printf ("\nWSAEnumNetworkEvents failed: %s\n",
			  link_strerror (WSAGetLastError ()));
	else {
#ifdef CONNECTION_DEBUG
		d_printf ("events={%s}\n", fd_mask (events.lNetworkEvents));
#endif
		if (watch->pollfd.revents != 0 &&
		    events.lNetworkEvents == 0) {
			watch->event_mask = 0;
			d_printf ("check: WSAEventSelect(%d, %#x, {})\n",
				  watch->socket, watch->pollfd.fd);
			WSAEventSelect (watch->socket, (HANDLE) watch->pollfd.fd, 0);
			d_printf ("check: ResetEvent(%#x)\n",
				  watch->pollfd.fd);
			ResetEvent ((HANDLE) watch->pollfd.fd);
		}
		watch->pollfd.revents = 0;
		if (events.lNetworkEvents & (FD_READ | FD_ACCEPT))
			watch->pollfd.revents |= G_IO_IN;
		if (events.lNetworkEvents & (FD_WRITE | FD_CONNECT))
			watch->pollfd.revents |= G_IO_OUT;
		if (events.lNetworkEvents & (FD_CLOSE))
			watch->pollfd.revents |= G_IO_HUP;
	}

	if (!watch->write_would_have_blocked && (watch->event_mask & FD_WRITE))
		watch->pollfd.revents |= G_IO_OUT; /* This sucks but... */
#endif

	return watch->pollfd.revents & watch->condition;
}

static gboolean
link_source_dispatch (GSource    *source,
		      GSourceFunc callback,
		      gpointer    user_data)

{
	GIOFunc    func;
	LinkUnixWatch *watch = (LinkUnixWatch *) source;

	if (!callback)
		g_error ("No callback");
  
	func = (GIOFunc) callback;

	return (*func) (watch->channel,
			watch->pollfd.revents & watch->condition,
			user_data);
}

static void
link_source_finalize (GSource *source)
{
	d_printf ("Finalize source %p\n", source);
}

static GSourceFuncs link_source_watch_funcs = {
	link_source_prepare,
	link_source_check,
	link_source_dispatch,
	link_source_finalize
};

/**
 * link_source_set_condition:
 * @source: a source created with #link_source_create_watch
 * @condition: a new condition.
 * 
 *     This sets a new IO condition on an existing
 * source very rapidly.
 **/
void
link_source_set_condition (GSource      *source,
			   GIOCondition  condition)
{
	LinkUnixWatch *watch = (LinkUnixWatch *) source;

	if (watch) {
		watch->pollfd.events = condition;
		watch->condition     = condition;
	}
}

/**
 * link_source_create_watch:
 * @context: context to add to (or NULL for default)
 * @fd: file descriptor to poll on
 * @opt_channel: channel, handed to the callback (can be NULL)
 * @condition: IO condition eg. G_IO_IN|G_IO_PRI
 * @func: callback when condition is met
 * @user_data: callback closure.
 * 
 * This adds a new source to the specified context.
 * 
 * Return value: the source handle so you can remove it later.
 **/
GSource *
link_source_create_watch (GMainContext *context,
			  int           fd,
			  GIOChannel   *opt_channel,
			  GIOCondition  condition,
			  GIOFunc       func,
			  gpointer      user_data)
{
	GSource       *source;
	LinkUnixWatch *watch;

	source = g_source_new (&link_source_watch_funcs,
			       sizeof (LinkUnixWatch));
	watch = (LinkUnixWatch *) source;

#ifdef G_OS_WIN32
	watch->pollfd.fd = (int) WSACreateEvent ();
	d_printf ("WSACreateEvent(): for socket %d: %#x\n", fd, watch->pollfd.fd);
	watch->link_watch = NULL;
	watch->socket = fd;
	watch->event_mask = 0;
	watch->write_would_have_blocked = FALSE;
#else
	watch->pollfd.fd = fd;
#endif
	watch->channel   = opt_channel;
	watch->condition = condition;
	watch->callback  = func;
	watch->user_data = user_data;

	link_source_set_condition (source, condition);

	g_source_set_can_recurse (source, TRUE);
	g_source_add_poll (source, &watch->pollfd);

	g_source_set_callback (source, (GSourceFunc) func,
			       user_data, NULL);
	g_source_attach (source, context);

	return source;
}

static GSource *
link_source_create_watch_for_watch (LinkWatch    *watch,
				    GMainContext *context,
				    int           fd,
				    GIOChannel   *opt_channel,
				    GIOCondition  condition,
				    GIOFunc       func,
				    gpointer      user_data)
{
	GSource *retval =
		link_source_create_watch (context, fd, opt_channel,
					  condition, func, user_data);

#ifdef G_OS_WIN32
	((LinkUnixWatch *) retval)->link_watch = watch;
#endif

	return retval;
}

#ifdef G_OS_WIN32

void
link_win32_watch_set_write_wouldblock (LinkWatch *w,
				       gboolean   flag)
{
	if (w->link_source)
		((LinkUnixWatch *)w->link_source)->write_would_have_blocked = flag;
	if (w->main_source)
		((LinkUnixWatch *)w->main_source)->write_would_have_blocked = flag;
}

#endif

LinkWatch *
link_io_add_watch_fd (int          fd,
		      GIOCondition condition,
		      GIOFunc      func,
		      gpointer     user_data)
{
	LinkWatch *w;
	GMainContext *thread_ctx;

	w = g_new0 (LinkWatch, 1);

	if ((thread_ctx = link_thread_io_context ())) {
		/* Have a dedicated I/O worker thread */
		w->link_source = link_source_create_watch_for_watch
			(w, thread_ctx, fd, NULL, condition, func, user_data);

	} else {
		/* Have an inferior and hook into the glib context */

		/* Link loop */
		w->link_source = link_source_create_watch_for_watch
			(w, link_main_get_context (), fd, NULL,
			 condition, func, user_data);
		
		w->main_source = link_source_create_watch_for_watch
			(w, NULL, fd, NULL,
			 condition, func, user_data);
	}

	return w;
}

static void
link_watch_unlisten (LinkWatch *w)
{
	if (w->main_source) {
		link_source_set_condition (w->main_source, 0);
#ifdef G_OS_WIN32
		d_printf ("CloseHandle(%#x)\n",
			  ((LinkUnixWatch *) w->main_source)->pollfd.fd);
		if (!CloseHandle ((HANDLE) ((LinkUnixWatch *) w->main_source)->pollfd.fd))
			d_printf ("CloseHandle failed: %ld\n", GetLastError ());
#endif
		g_source_destroy (w->main_source);
		g_source_unref   (w->main_source);
		w->main_source = NULL;
	}

	if (w->link_source) {
		link_source_set_condition (w->link_source, 0);
#ifdef G_OS_WIN32
		d_printf ("CloseHandle(%#x)\n",
			  ((LinkUnixWatch *) w->link_source)->pollfd.fd);
		if (!CloseHandle ((HANDLE) ((LinkUnixWatch *) w->link_source)->pollfd.fd))
			d_printf ("CloseHandle failed: %ld\n", GetLastError ());
#endif
		g_source_destroy (w->link_source);
		g_source_unref   (w->link_source);
		w->link_source = NULL;
	}
#ifdef G_OS_WIN32
	w->last_polled_source = NULL;
#endif
}

void
link_io_remove_watch (LinkWatch *w)
{
	if (!w)
		return;

	link_watch_unlisten (w);
	g_free (w);
}

void
link_watch_set_condition (LinkWatch   *w,
			  GIOCondition condition)
{
	if (w) {
		link_source_set_condition (
			w->link_source, condition);

		link_source_set_condition (
			w->main_source, condition);
	}
}

/*
 * Migrates the source to/from the main thread.
 */
void
link_watch_move_io (LinkWatch *w,
		    gboolean to_io_thread)
{
	LinkUnixWatch w_cpy;

	if (!w)
		return;

	g_assert (to_io_thread); /* FIXME */

	w_cpy = *(LinkUnixWatch *)w->link_source;

	link_watch_unlisten (w);

	w->link_source = link_source_create_watch_for_watch
		(w, link_thread_io_context (),
#ifdef G_OS_WIN32
		 w_cpy.socket,
#else
		 w_cpy.pollfd.fd,
#endif
		 w_cpy.channel, w_cpy.condition,
		 w_cpy.callback, w_cpy.user_data);
}
