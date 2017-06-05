/*
 * linc-source.c: This file is part of the linc library.
 *
 * Authors:
 *    Owen Taylor   (owen@redhat.com)
 *    Michael Meeks (michael@ximian.com)
 *
 * Copyright 1998, 2001, Red Hat, Inc., Ximian, Inc.,
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#include <glib.h>
#include "linc-debug.h"
#include "linc-private.h"

static gboolean 
linc_source_prepare (GSource *source,
		     gint    *timeout)
{
	*timeout = -1;

	return FALSE;
}

static gboolean 
linc_source_check (GSource *source)
{
	LincUnixWatch *watch = (LincUnixWatch *)source;

	return watch->pollfd.revents & watch->condition;
}

static gboolean
linc_source_dispatch (GSource    *source,
		      GSourceFunc callback,
		      gpointer    user_data)

{
	GIOFunc    func;
	LincUnixWatch *watch = (LincUnixWatch *) source;

	if (!callback)
		g_error ("No callback");
  
	func = (GIOFunc) callback;

	return (*func) (watch->channel,
			watch->pollfd.revents & watch->condition,
			user_data);
}

static void
linc_source_finalize (GSource *source)
{
	d_printf ("Finalize source %p", source);
}

GSourceFuncs linc_source_watch_funcs = {
	linc_source_prepare,
	linc_source_check,
	linc_source_dispatch,
	linc_source_finalize
};

static void
linc_source_set_condition (LincUnixWatch *watch,
			   GIOCondition  condition)
{
	if (watch) {
		watch->pollfd.events = condition;
		watch->condition     = condition;
	}
}

static GSource *
linc_source_create_watch (GMainContext *context,
			  int           fd,
			  GIOChannel   *channel,
			  GIOCondition  condition,
			  GIOFunc       func,
			  gpointer      user_data)
{
	GSource       *source;
	LincUnixWatch *watch;

	source = g_source_new (&linc_source_watch_funcs,
			       sizeof (LincUnixWatch));
	watch = (LincUnixWatch *) source;

	watch->pollfd.fd = fd;
	watch->channel   = channel;

	linc_source_set_condition (watch, condition);

	g_source_set_can_recurse (source, TRUE);
	g_source_add_poll (source, &watch->pollfd);

	g_source_set_callback (source, (GSourceFunc) func,
			       user_data, NULL);
	g_source_attach (source, context);

	return source;
}

/*
 * Wrappers to make listening on two mainloops simpler
 */

LincWatch *
linc_io_add_watch_fd (int          fd,
		      GIOCondition condition,
		      GIOFunc      func,
		      gpointer     user_data)
{
	LincWatch *w;
  
	w = g_new (LincWatch, 1);

	/* Linc loop */
	w->linc_source = linc_source_create_watch (
		linc_main_get_context (), fd, NULL,
		condition, func, user_data);

	/* Main loop */
	w->main_source = linc_source_create_watch (
		NULL, fd, NULL, condition, func, user_data);

	return w;
}

/**
 * linc_io_add_watch:
 * @channel: the GIOChannel to watch
 * @condition: the condition mask to watch for
 * @func: the function to invoke when a condition is met
 * @user_data: a user data closure
 * 
 * This routine creates a watch on an IO channel that operates both in
 * the standard glib mainloop, but also in the 'linc' mainloop so we
 * can iterate that without causing re-enterancy.
 *
 * This method is deprecated.
 * 
 * Return value: a pointer identifying the watch.
 **/
LincWatch *
linc_io_add_watch (GIOChannel    *channel,
		   GIOCondition   condition,
		   GIOFunc        func,
		   gpointer       user_data)
{
	LincWatch *w;
	int       fd = g_io_channel_unix_get_fd(channel);

	w = g_new (LincWatch, 1);

	/* Linc loop */
	w->linc_source = linc_source_create_watch (
		linc_main_get_context (), fd, channel,
		condition, func, user_data);

	/* Main loop */
	w->main_source = linc_source_create_watch (
		NULL, fd, channel, condition, func, user_data);

	return w;
}

/**
 * linc_io_remove_watch:
 * @watch: the handle of a watch on a GIOChannel
 * 
 * This removes a watch by it's handle in @w
 **/
void
linc_io_remove_watch (LincWatch *w)
{
	if (w) {
		linc_source_set_condition ((LincUnixWatch *) w->main_source, 0);
		linc_source_set_condition ((LincUnixWatch *) w->linc_source, 0);

		g_source_destroy (w->main_source);
		g_source_unref   (w->main_source);

		g_source_destroy (w->linc_source);
		g_source_unref   (w->linc_source);

		g_free (w);
	}
}

void
linc_watch_set_condition (LincWatch   *w,
			  GIOCondition condition)
{
	if (w) {
		linc_source_set_condition (
			(LincUnixWatch *) w->linc_source,
			condition);

		linc_source_set_condition (
			(LincUnixWatch *) w->main_source,
			condition);
	}
}
