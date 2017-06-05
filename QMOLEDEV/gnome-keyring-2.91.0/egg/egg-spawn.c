/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "egg-spawn.h"

#include <glib/gi18n-lib.h>

#include <errno.h>
#include <string.h>
#include <unistd.h>

#include <sys/select.h>
#include <sys/wait.h>

typedef struct _CallbackSource {
	GSource source;
	EggSpawnCallbacks callbacks;
	GPollFD polls[3];
} CallbackSource;

static void
close_fd (int *fd)
{
	g_assert (fd);
	if (*fd >= 0)
		close (*fd);
	*fd = -1;
}

static void
close_poll (GSource *source, GPollFD *poll)
{
	g_source_remove_poll (source, poll);
	close_fd (&poll->fd);
	poll->revents = 0;
}

static gboolean
unused_callback (gpointer data)
{
	/* Never called */
	g_assert_not_reached ();
	return FALSE;
}

static gboolean
cb_source_prepare (GSource *source, gint *timeout_)
{
	CallbackSource *cb_source = (CallbackSource*)source;
	gint i;

	for (i = 0; i < 3; ++i) {
		if (cb_source->polls[i].fd >= 0)
			return FALSE;
	}

	/* If none of the FDs are valid, then process immediately */
	return TRUE;
}

static gboolean
cb_source_check (GSource *source)
{
	CallbackSource *cb_source = (CallbackSource*)source;
	gint i;

	for (i = 0; i < 3; ++i) {
		if (cb_source->polls[i].fd >= 0 && cb_source->polls[i].revents != 0)
			return TRUE;
	}
	return FALSE;
}

static void
cb_source_finalize (GSource *source)
{
	CallbackSource *cb_source = (CallbackSource*)source;
	gint i;

	for (i = 0; i < 3; ++i)
		close_fd (&cb_source->polls[i].fd);
}

static gboolean
cb_source_dispatch (GSource *source, GSourceFunc unused, gpointer user_data)
{
	CallbackSource *cb_source = (CallbackSource*)source;
	GPollFD *poll;
	gint i;

	/* Standard input */
	poll = &cb_source->polls[0];
	if (poll->fd >= 0 && poll->revents != 0) {
		g_assert (cb_source->callbacks.standard_input);
		if (!(cb_source->callbacks.standard_input) (poll->fd, user_data))
			close_poll (source, poll);
	}

	/* Standard output */
	poll = &cb_source->polls[1];
	if (poll->fd >= 0 && poll->revents != 0) {
		g_assert (cb_source->callbacks.standard_output);
		if (!(cb_source->callbacks.standard_output) (poll->fd, user_data))
			close_poll (source, poll);
	}

	/* Standard error */
	poll = &cb_source->polls[2];
	if (poll->fd >= 0 && poll->revents != 0) {
		g_assert (cb_source->callbacks.standard_error);
		if (!(cb_source->callbacks.standard_error) (poll->fd, user_data))
			close_poll (source, poll);
	}

	for (i = 0; i < 3; ++i) {
		if (cb_source->polls[i].fd >= 0)
			return TRUE;
	}

	/* All input and output is done */
	if (cb_source->callbacks.completed)
		(cb_source->callbacks.completed) (user_data);
	return FALSE;
}

static GSourceFuncs cb_source_funcs = {
	cb_source_prepare,
	cb_source_check,
	cb_source_dispatch,
	cb_source_finalize,
};

guint
egg_spawn_async_with_callbacks (const gchar *working_directory, gchar **argv,
                                gchar **envp, GSpawnFlags flags, GPid *child_pid,
                                EggSpawnCallbacks *cbs, gpointer user_data,
                                GMainContext *context, GError **error)
{
	gint in_fd, out_fd, err_fd;
	CallbackSource *cb_source;
	GSource *source;
	guint tag;

	g_return_val_if_fail (argv != NULL, FALSE);
	g_return_val_if_fail ((cbs && cbs->standard_input == NULL) ||
	                      !(flags & G_SPAWN_CHILD_INHERITS_STDIN), 0);
	g_return_val_if_fail ((cbs && cbs->standard_output == NULL) ||
	                      !(flags & G_SPAWN_STDOUT_TO_DEV_NULL), 0);
	g_return_val_if_fail ((cbs && cbs->standard_error == NULL) ||
	                      !(flags & G_SPAWN_STDERR_TO_DEV_NULL), 0);

	in_fd = out_fd = err_fd = -1;

	if (!g_spawn_async_with_pipes (working_directory, argv, envp, flags,
	                               cbs ? cbs->child_setup : NULL,
	                               user_data, child_pid,
	                               cbs && cbs->standard_input ? &in_fd : NULL,
	                               cbs && cbs->standard_output ? &out_fd : NULL,
	                               cbs && cbs->standard_error ? &err_fd : NULL,
	                               error))
		return 0;

	source = g_source_new (&cb_source_funcs, sizeof (CallbackSource));

	cb_source = (CallbackSource*)source;
	if (cbs != NULL)
		memcpy (&cb_source->callbacks, cbs, sizeof (EggSpawnCallbacks));

	cb_source->polls[0].fd = in_fd;
	if (in_fd >= 0) {
		g_assert (cb_source->callbacks.standard_input);
		cb_source->polls[0].events = G_IO_ERR | G_IO_OUT;
		g_source_add_poll (source, &cb_source->polls[0]);
	}
	cb_source->polls[1].fd = out_fd;
	if (out_fd >= 0) {
		g_assert (cb_source->callbacks.standard_output);
		cb_source->polls[1].events = G_IO_ERR | G_IO_HUP | G_IO_IN;
		g_source_add_poll (source, &cb_source->polls[1]);
	}
	cb_source->polls[2].fd = err_fd;
	if (err_fd >= 0) {
		g_assert (cb_source->callbacks.standard_error);
		cb_source->polls[2].events = G_IO_ERR | G_IO_HUP | G_IO_IN;
		g_source_add_poll (source, &cb_source->polls[2]);
	}

	if (context == NULL)
		context = g_main_context_default ();
	g_source_set_callback (source, unused_callback, user_data,
	                       cbs ? cbs->finalize_func : NULL);
	tag = g_source_attach (source, context);
	g_source_unref (source);

	return tag;
}

gboolean
egg_spawn_sync_with_callbacks  (const gchar *working_directory, gchar **argv,
                                gchar **envp, GSpawnFlags flags, GPid *child_pid,
                                EggSpawnCallbacks *cbs, gpointer user_data,
                                gint *exit_status, GError **error)
{
	gint in_fd, out_fd, err_fd, max_fd;
	fd_set read_fds, write_fds;
	gboolean failed = FALSE;
	gint status;
	GPid pid;
	gint ret;

	g_return_val_if_fail (argv != NULL, FALSE);
	g_return_val_if_fail ((cbs && cbs->standard_input == NULL) ||
	                      !(flags & G_SPAWN_CHILD_INHERITS_STDIN), 0);
	g_return_val_if_fail ((cbs && cbs->standard_output == NULL) ||
	                      !(flags & G_SPAWN_STDOUT_TO_DEV_NULL), 0);
	g_return_val_if_fail ((cbs && cbs->standard_error == NULL) ||
	                      !(flags & G_SPAWN_STDERR_TO_DEV_NULL), 0);

	in_fd = out_fd = err_fd = -1;

	if (exit_status)
		flags |= G_SPAWN_DO_NOT_REAP_CHILD;

	if (!g_spawn_async_with_pipes (working_directory, argv, envp, flags,
	                               cbs ? cbs->child_setup : NULL,
	                               user_data, &pid,
	                               cbs && cbs->standard_input ? &in_fd : NULL,
	                               cbs && cbs->standard_output ? &out_fd : NULL,
	                               cbs && cbs->standard_error ? &err_fd : NULL,
	                               error))
		return FALSE;

	if (child_pid)
		*child_pid = pid;

	max_fd = MAX (in_fd, MAX (out_fd, err_fd)) + 1;

	while (in_fd >= 0 || out_fd >= 0 || err_fd >= 0) {

		FD_ZERO (&write_fds);
		if (in_fd >= 0)
			FD_SET (in_fd, &write_fds);
		FD_ZERO (&read_fds);
		if (out_fd >= 0)
			FD_SET (out_fd, &read_fds);
		if (err_fd >= 0)
			FD_SET (err_fd, &read_fds);

		ret = select (max_fd, &read_fds, &write_fds, NULL, NULL);
		if (ret < 0 && errno != EINTR) {
			failed = TRUE;
			g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_READ,
			             _("Unexpected error in select() reading data from a child process (%s)"),
			             g_strerror (errno));
			break;
		}

		if (in_fd >= 0 && FD_ISSET (in_fd, &write_fds)) {
			g_assert (cbs && cbs->standard_input);
			if (!(cbs->standard_input) (in_fd, user_data))
				close_fd (&in_fd);
		}
		if (out_fd >= 0 && FD_ISSET (out_fd, &read_fds)) {
			g_assert (cbs && cbs->standard_output);
			if (!(cbs->standard_output) (out_fd, user_data))
				close_fd (&out_fd);
		}
		if (err_fd >= 0 && FD_ISSET (err_fd, &read_fds)) {
			g_assert (cbs && cbs->standard_error);
			if (!(cbs->standard_error) (err_fd, user_data))
				close_fd (&err_fd);
		}
	}

	if (in_fd >= 0)
		close_fd (&in_fd);
	if (out_fd >= 0)
		close_fd (&out_fd);
	if (err_fd >= 0)
		close_fd (&err_fd);

	if (!failed) {
		if (cbs && cbs->completed)
			(cbs->completed) (user_data);
	}

again:
	ret = waitpid (pid, &status, 0);
	if (ret < 0) {
		if (errno == EINTR)
			goto again;
		else if (errno == ECHILD) {
			if (exit_status)
				g_warning ("In call to g_spawn_sync(), exit status of a child process was requested but SIGCHLD action was set to SIG_IGN and ECHILD was received by waitpid(), so exit status can't be returned. This is a bug in the program calling g_spawn_sync(); either don't request the exit status, or don't set the SIGCHLD action.");
			else
				; /* We don't need the exit status. */
		} else if (!failed) { /* avoid error pileups */
			failed = TRUE;
			g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_READ,
			             _("Unexpected error in waitpid() (%s)"),
			             g_strerror (errno));
		}
	} else {
		if (exit_status)
			*exit_status = status;
	}

	if (!child_pid)
		g_spawn_close_pid (pid);

	if (cbs && cbs->finalize_func)
		(cbs->finalize_func) (user_data);

	return !failed;
}

gssize
egg_spawn_write_input (int fd, gconstpointer data, gsize n_data)
{
	gssize result;

	g_return_val_if_fail (fd >= 0, -1);

	for (;;) {
		result = write (fd, data, n_data);
		if (result < 0) {
			if (errno == EINTR)
				continue;
			else if (errno == EAGAIN)
				result = 0;
		}
		break;
	}

	return result;
}

gssize
egg_spawn_read_output (int fd, gpointer data, gsize n_data)
{
	gssize result;

	g_return_val_if_fail (fd >= 0, -1);

	for (;;) {
		result = read (fd, data, n_data);
		if (result < 0) {
			if (errno == EINTR)
				continue;
			else if (errno == EAGAIN)
				result = 0;
		}
		break;
	}

	return result;
}

