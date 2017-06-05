/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#define DEBUG_FLAG GCR_DEBUG_GNUPG
#include "gcr-debug.h"
#include "gcr-gnupg-process.h"
#include "gcr-marshal.h"
#include "gcr-util.h"

#include <glib/gi18n-lib.h>

#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

/**
 * GcrGnupgProcessFlags:
 * @GCR_GNUPG_PROCESS_NONE: No flags
 * @GCR_GNUPG_PROCESS_RESPECT_LOCALE: Respect the user's locale when running gnupg.
 * @GCR_GNUPG_PROCESS_WITH_STATUS: Ask the process to send status records.
 * @GCR_GNUPG_PROCESS_WITH_ATTRIBUTES: Ask the process to output attribute data.
 *
 * Flags for running a gnupg process.
 */

enum {
	PROP_0,
	PROP_DIRECTORY,
	PROP_EXECUTABLE
};

enum {
	FD_INPUT,
	FD_OUTPUT,
	FD_ERROR,
	FD_STATUS,
	FD_ATTRIBUTE,
	NUM_FDS
};

enum {
	OUTPUT_DATA,
	ERROR_LINE,
	STATUS_RECORD,
	ATTRIBUTE_DATA,
	NUM_SIGNALS
};

static gint signals[NUM_SIGNALS] = { 0, };

typedef struct _GnupgSource {
	GSource source;
	GPollFD polls[NUM_FDS];         /* The various fd's we're listening to */

	GcrGnupgProcess *process;       /* Pointer back to the process object */

	GString *error_buf;
	GString *status_buf;

	GPid child_pid;
	guint child_sig;

	GCancellable *cancellable;
	guint cancel_sig;
} GnupgSource;

struct _GcrGnupgProcessPrivate {
	gchar *directory;
	gchar *executable;

	gboolean running;
	gboolean complete;
	GError *error;

	guint source_sig;

	GAsyncReadyCallback async_callback;
	gpointer user_data;
};

/* Forward declarations */
static void _gcr_gnupg_process_init_async (GAsyncResultIface *iface);

G_DEFINE_TYPE_WITH_CODE (GcrGnupgProcess, _gcr_gnupg_process, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (G_TYPE_ASYNC_RESULT, _gcr_gnupg_process_init_async));

static void
_gcr_gnupg_process_init (GcrGnupgProcess *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_GNUPG_PROCESS,
	                                        GcrGnupgProcessPrivate);
}

static void
_gcr_gnupg_process_constructed (GObject *obj)
{
	GcrGnupgProcess *self = GCR_GNUPG_PROCESS (obj);

	if (G_OBJECT_CLASS (_gcr_gnupg_process_parent_class)->constructed)
		G_OBJECT_CLASS (_gcr_gnupg_process_parent_class)->constructed (obj);

	if (!self->pv->executable)
		self->pv->executable = g_strdup (GPG_EXECUTABLE);
}

static void
_gcr_gnupg_process_get_property (GObject *obj, guint prop_id, GValue *value,
                                 GParamSpec *pspec)
{
	GcrGnupgProcess *self = GCR_GNUPG_PROCESS (obj);

	switch (prop_id) {
	case PROP_DIRECTORY:
		g_value_set_string (value, self->pv->directory);
		break;
	case PROP_EXECUTABLE:
		g_value_set_string (value, self->pv->executable);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_gnupg_process_set_property (GObject *obj, guint prop_id, const GValue *value,
                                 GParamSpec *pspec)
{
	GcrGnupgProcess *self = GCR_GNUPG_PROCESS (obj);

	switch (prop_id) {
	case PROP_DIRECTORY:
		g_return_if_fail (!self->pv->directory);
		self->pv->directory = g_value_dup_string (value);
		break;
	case PROP_EXECUTABLE:
		g_return_if_fail (!self->pv->executable);
		self->pv->executable = g_value_dup_string (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_gnupg_process_finalize (GObject *obj)
{
	GcrGnupgProcess *self = GCR_GNUPG_PROCESS (obj);

	g_assert (!self->pv->running);
	g_free (self->pv->directory);
	g_free (self->pv->executable);
	g_clear_error (&self->pv->error);

	G_OBJECT_CLASS (_gcr_gnupg_process_parent_class)->finalize (obj);
}

static void
_gcr_gnupg_process_class_init (GcrGnupgProcessClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructed = _gcr_gnupg_process_constructed;
	gobject_class->get_property = _gcr_gnupg_process_get_property;
	gobject_class->set_property = _gcr_gnupg_process_set_property;
	gobject_class->finalize = _gcr_gnupg_process_finalize;

	/**
	 * GcrGnupgProcess:directory:
	 *
	 * Directory to run as gnupg home directory, or %NULL for default
	 * ~/.gnupg/ directory.
	 */
	g_object_class_install_property (gobject_class, PROP_DIRECTORY,
	           g_param_spec_string ("directory", "Directory", "Gnupg Directory",
	                                NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GcrGnupgProcess:executable:
	 *
	 * Path to the gnupg executable, or %NULL for default.
	 */
	g_object_class_install_property (gobject_class, PROP_EXECUTABLE,
	           g_param_spec_string ("executable", "Executable", "Gnupg Executable",
	                                GPG_EXECUTABLE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GcrGnupgProcess::output-data:
	 * @data: a #GByteArray of output data.
	 *
	 * Signal emitted when normal output data is available from the gnupg
	 * process. The data does not necessarily come on line boundaries, and
	 * won't be null-terminated.
	 */
	signals[OUTPUT_DATA] = g_signal_new ("output-data", GCR_TYPE_GNUPG_PROCESS,
	           G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GcrGnupgProcessClass, output_data),
	           NULL, NULL, _gcr_marshal_VOID__BOXED,
	           G_TYPE_NONE, 1, G_TYPE_BYTE_ARRAY);

	/**
	 * GcrGnupgProcess::error-line:
	 * @line: a line of error output.
	 *
	 * Signal emitted when a line of error output is available from the
	 * gnupg process.
	 */
	signals[ERROR_LINE] = g_signal_new ("error-line", GCR_TYPE_GNUPG_PROCESS,
	           G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GcrGnupgProcessClass, error_line),
	           NULL, NULL, _gcr_marshal_VOID__STRING,
	           G_TYPE_NONE, 1, G_TYPE_STRING);

	/**
	 * GcrGnupgProcess::status-record:
	 * @record: a status record.
	 *
	 * Signal emitted when a status record is available from the gnupg process.
	 */
	signals[STATUS_RECORD] = g_signal_new ("status-record", GCR_TYPE_GNUPG_PROCESS,
	           G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GcrGnupgProcessClass, status_record),
	           NULL, NULL, _gcr_marshal_VOID__BOXED,
	           G_TYPE_NONE, 1, GCR_TYPE_RECORD);

	/**
	 * GcrGnupgProcess::attribute-data:
	 * @data: a #GByteArray of attribute data.
	 *
	 * Signal emitted when attribute data is available from the gnupg
	 * process.
	 */
	signals[ATTRIBUTE_DATA] = g_signal_new ("attribute-data", GCR_TYPE_GNUPG_PROCESS,
	           G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GcrGnupgProcessClass, attribute_data),
	           NULL, NULL, _gcr_marshal_VOID__BOXED,
	           G_TYPE_NONE, 1, G_TYPE_BYTE_ARRAY);

	g_type_class_add_private (gobject_class, sizeof (GcrGnupgProcessPrivate));
}

static gpointer
_gcr_gnupg_process_get_user_data (GAsyncResult *result)
{
	g_return_val_if_fail (GCR_IS_GNUPG_PROCESS (result), NULL);
	return GCR_GNUPG_PROCESS (result)->pv->user_data;
}

static GObject*
_gcr_gnupg_process_get_source_object (GAsyncResult *result)
{
	g_return_val_if_fail (GCR_IS_GNUPG_PROCESS (result), NULL);
	return G_OBJECT (result);
}

static void
_gcr_gnupg_process_init_async (GAsyncResultIface *iface)
{
	iface->get_source_object = _gcr_gnupg_process_get_source_object;
	iface->get_user_data = _gcr_gnupg_process_get_user_data;
}

/**
 * _gcr_gnupg_process_new:
 * @directory: (allow-none): The gnupg home directory
 * @executable: (allow-none): The gpg executable
 *
 * Create a new GcrGnupgProcess.
 *
 * The gnupg home directory is where the keyring files live. If directory is
 * %NULL then the default gnupg home directory is used.
 *
 * The executable will default to the compiled in path if a %NULL executable
 * argument is used.
 *
 * Returns: (transfer full): A newly allocated process.
 */
GcrGnupgProcess*
_gcr_gnupg_process_new (const gchar *directory, const gchar *executable)
{
	return g_object_new (GCR_TYPE_GNUPG_PROCESS,
	                     "directory", directory,
	                     "executable", executable,
	                     NULL);
}

static void
run_async_ready_callback (GcrGnupgProcess *self)
{
	GAsyncReadyCallback callback;
	gpointer user_data;

	_gcr_debug ("running async callback");

	/* Remove these before completing */
	callback = self->pv->async_callback;
	user_data = self->pv->user_data;
	self->pv->async_callback = NULL;
	self->pv->user_data = NULL;

	if (callback != NULL)
		(callback) (G_OBJECT (self), G_ASYNC_RESULT (self), user_data);
}

static gboolean
on_run_async_ready_callback_later (gpointer user_data)
{
	run_async_ready_callback (GCR_GNUPG_PROCESS (user_data));
	return FALSE; /* Don't run this callback again */
}

static void
run_async_ready_callback_later (GcrGnupgProcess *self)
{
	_gcr_debug ("running async callback later");
	g_idle_add_full (G_PRIORITY_DEFAULT, on_run_async_ready_callback_later,
	                 g_object_ref (self), g_object_unref);
}

static void
complete_run_process (GcrGnupgProcess *self)
{
	g_return_if_fail (self->pv->running);
	g_return_if_fail (!self->pv->complete);

	self->pv->running = FALSE;
	self->pv->complete = TRUE;

	if (self->pv->source_sig) {
		g_source_remove (self->pv->source_sig);
		self->pv->source_sig = 0;
	}

	if (self->pv->error == NULL) {
		_gcr_debug ("completed process");
	} else {
		_gcr_debug ("completed process with error: %s",
		            self->pv->error->message);
	}
}

static gboolean
complete_if_source_is_done (GnupgSource *gnupg_source)
{
	gint i;

	for (i = 0; i < NUM_FDS; ++i) {
		if (gnupg_source->polls[i].fd >= 0)
			return FALSE;
	}

	if (gnupg_source->child_pid)
		return FALSE;

	_gcr_debug ("all fds closed and process exited, completing");

	complete_run_process (gnupg_source->process);
	run_async_ready_callback (gnupg_source->process);

	/* All done, the source can go away now */
	g_source_unref ((GSource*)gnupg_source);
	return TRUE;
}

static void
close_fd (int *fd)
{
	g_assert (fd);
	if (*fd >= 0) {
		_gcr_debug ("closing fd: %d", *fd);
		close (*fd);
	}
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
on_gnupg_source_prepare (GSource *source, gint *timeout_)
{
	GnupgSource *gnupg_source = (GnupgSource*)source;
	gint i;

	for (i = 0; i < NUM_FDS; ++i) {
		if (gnupg_source->polls[i].fd >= 0)
			return FALSE;
	}

	/* If none of the FDs are valid, then process immediately */
	return TRUE;
}

static gboolean
on_gnupg_source_check (GSource *source)
{
	GnupgSource *gnupg_source = (GnupgSource*)source;
	gint i;

	for (i = 0; i < NUM_FDS; ++i) {
		if (gnupg_source->polls[i].fd >= 0 && gnupg_source->polls[i].revents != 0)
			return TRUE;
	}
	return FALSE;
}

static void
on_gnupg_source_finalize (GSource *source)
{
	GnupgSource *gnupg_source = (GnupgSource*)source;
	gint i;

	if (gnupg_source->cancel_sig)
		g_signal_handler_disconnect (gnupg_source->cancellable, gnupg_source->cancel_sig);
	if (gnupg_source->cancellable)
		g_object_unref (gnupg_source->cancellable);

	for (i = 0; i < NUM_FDS; ++i)
		close_fd (&gnupg_source->polls[i].fd);

	g_object_unref (gnupg_source->process);
	g_string_free (gnupg_source->error_buf, TRUE);
	g_string_free (gnupg_source->status_buf, TRUE);

	g_assert (!gnupg_source->child_pid);
	g_assert (!gnupg_source->child_sig);
}

static gboolean
read_output (int fd, GByteArray *buffer)
{
	guchar block[1024];
	gssize result;

	g_return_val_if_fail (fd >= 0, FALSE);

	do {
		result = read (fd, block, sizeof (block));
		if (result < 0) {
			if (errno == EINTR || errno == EAGAIN)
				continue;
			return FALSE;
		} else {
			g_byte_array_append (buffer, block, result);
		}
	} while (result == sizeof (block));

	return TRUE;
}

static void
emit_status_for_each_line (const gchar *line, gpointer user_data)
{
	GcrRecord *record;

	if (g_str_has_prefix (line, "[GNUPG:] ")) {
		_gcr_debug ("received status line: %s", line);
		line += 9;
	} else {
		g_message ("gnupg status record was not prefixed appropriately: %s", line);
		return;
	}

	record = _gcr_record_parse_spaces (line, -1);
	if (!record) {
		g_message ("couldn't parse status record: %s", line);
		return;
	}

	g_signal_emit (GCR_GNUPG_PROCESS (user_data), signals[STATUS_RECORD], 0, record);
	_gcr_record_free (record);
}

static void
emit_error_for_each_line (const gchar *line, gpointer user_data)
{
	_gcr_debug ("received error line: %s", line);
	g_signal_emit (GCR_GNUPG_PROCESS (user_data), signals[ERROR_LINE], 0, line);
}

static gboolean
on_gnupg_source_dispatch (GSource *source, GSourceFunc unused, gpointer user_data)
{
	GnupgSource *gnupg_source = (GnupgSource*)source;
	GByteArray *buffer;
	GPollFD *poll;

	/* Standard input, no support yet */
	poll = &gnupg_source->polls[FD_INPUT];
	if (poll->fd >= 0 && poll->revents != 0) {
		close_poll (source, poll);
	}

	/* Status output */
	poll = &gnupg_source->polls[FD_STATUS];
	if (poll->fd >= 0) {
		if (poll->revents & G_IO_IN) {
			buffer = g_byte_array_new ();
			if (!read_output (poll->fd, buffer)) {
				g_warning ("couldn't read status data from gnupg process");
			} else {
				g_string_append_len (gnupg_source->status_buf, (gchar*)buffer->data, buffer->len);
				_gcr_util_parse_lines (gnupg_source->status_buf, buffer->len == 0,
				                       emit_status_for_each_line, gnupg_source->process);
			}
			g_byte_array_unref (buffer);
		}
		if (poll->revents & G_IO_HUP)
			close_poll (source, poll);
		poll->revents = 0;
	}

	/* Attribute output */
	poll = &gnupg_source->polls[FD_ATTRIBUTE];
	if (poll->fd >= 0) {
		if (poll->revents & G_IO_IN) {
			buffer = g_byte_array_new ();
			if (!read_output (poll->fd, buffer)) {
				g_warning ("couldn't read attribute data from gnupg process");
			} else if (buffer->len > 0) {
				_gcr_debug ("received %d bytes of attribute data", (gint)buffer->len);
				g_signal_emit (gnupg_source->process, signals[ATTRIBUTE_DATA], 0, buffer);
			}
			g_byte_array_unref (buffer);
		}
		if (poll->revents & G_IO_HUP)
			close_poll (source, poll);
		poll->revents = 0;
	}

	/* Standard output */
	poll = &gnupg_source->polls[FD_OUTPUT];
	if (poll->fd >= 0) {
		if (poll->revents & G_IO_IN) {
			buffer = g_byte_array_new ();
			if (!read_output (poll->fd, buffer)) {
				g_warning ("couldn't read output data from gnupg process");
			} else if (buffer->len > 0) {
				_gcr_debug ("received %d bytes of output data", (gint)buffer->len);
				g_signal_emit (gnupg_source->process, signals[OUTPUT_DATA], 0, buffer);
			}
			g_byte_array_unref (buffer);
		}
		if (poll->revents & G_IO_HUP)
			close_poll (source, poll);
		poll->revents = 0;
	}

	/* Standard error */
	poll = &gnupg_source->polls[FD_ERROR];
	if (poll->fd >= 0) {
		if (poll->revents & G_IO_IN) {
			buffer = g_byte_array_new ();
			if (!read_output (poll->fd, buffer)) {
				g_warning ("couldn't read error data from gnupg process");
			} else {
				g_string_append_len (gnupg_source->error_buf, (gchar*)buffer->data, buffer->len);
				_gcr_util_parse_lines (gnupg_source->error_buf, (poll->revents & G_IO_HUP) ? TRUE : FALSE,
						       emit_error_for_each_line, gnupg_source->process);
			}
			g_byte_array_unref (buffer);
		}
		if (poll->revents & G_IO_HUP)
			close_poll (source, poll);
		poll->revents = 0;
	}

	if (complete_if_source_is_done (gnupg_source))
		return FALSE; /* Disconnect this source */

	return TRUE;
}

static GSourceFuncs gnupg_source_funcs = {
	on_gnupg_source_prepare,
	on_gnupg_source_check,
	on_gnupg_source_dispatch,
	on_gnupg_source_finalize,
};

static void
on_gnupg_process_child_exited (GPid pid, gint status, gpointer user_data)
{
	GnupgSource *gnupg_source = user_data;
	GcrGnupgProcess *self = gnupg_source->process;
	GError *error = NULL;
	gint code;

	_gcr_debug ("process exited: %d", (int)pid);

	g_spawn_close_pid (gnupg_source->child_pid);
	gnupg_source->child_pid = 0;
	gnupg_source->child_sig = 0;

	if (WIFEXITED (status)) {
		code = WEXITSTATUS (status);
		if (code != 0) {
			error = g_error_new (G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
			                     _("Gnupg process exited with code: %d"), code);
		}
	} else if (WIFSIGNALED (status)) {
		code = WTERMSIG (status);
		/* Ignore cases where we've signaled the process because we were cancelled */
		if (!g_error_matches (self->pv->error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
			error = g_error_new (G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
			                     _("Gnupg process was terminated with signal: %d"), code);
	}

	/* Take this as the async result error */
	if (error && !self->pv->error) {
		_gcr_debug ("%s", error->message);
		self->pv->error = error;

	/* Already have an error, just print out message */
	} else if (error) {
		g_warning ("%s", error->message);
		g_error_free (error);
	}

	complete_if_source_is_done (gnupg_source);
}

static void
on_gnupg_process_child_setup (gpointer user_data)
{
	int *child_fds = user_data;
	long val;
	guint i;

	/*
	 * Clear close-on-exec flag for these file descriptors, so that
	 * gnupg can write to them
	 */

	for (i = 0; i < NUM_FDS; i++) {
		if (child_fds[i] >= 0) {
			val = fcntl (child_fds[i], F_GETFD);
			fcntl (child_fds[i], F_SETFD, val & ~FD_CLOEXEC);
		}
	}
}

static void
on_cancellable_cancelled (GCancellable *cancellable, gpointer user_data)
{
	GnupgSource *gnupg_source = user_data;

	g_assert (gnupg_source->process);

	_gcr_debug ("process cancelled");

	/* Try and kill the child process */
	if (gnupg_source->child_pid) {
		_gcr_debug ("sending term signal to process: %d",
		            (int)gnupg_source->child_pid);
		kill (gnupg_source->child_pid, SIGTERM);
	}

	/* Set an error, which is respected when this actually completes. */
	if (gnupg_source->process->pv->error == NULL)
		gnupg_source->process->pv->error = g_error_new_literal (G_IO_ERROR, G_IO_ERROR_CANCELLED,
		                                                        _("The operation was cancelled"));

	complete_if_source_is_done (gnupg_source);
}

/**
 * _gcr_gnupg_process_run_async:
 * @self: The process
 * @argv: (array zero-terminated=1): The arguments for the process, not including executable, terminated with %NULL.
 * @envp: (allow-none) (array zero-terminated=1): The environment for new process, terminated with %NULL.
 * @flags: Flags for starting the process.
 * @cancellable: (allow-none): Cancellation object
 * @callback: Will be called when operation completes.
 * @user_data: (closure): Data passed to callback.
 *
 * Run the gpg process. Only one 'run' operation can run per GcrGnupgProcess
 * object. The GcrGnupgProcess:output_data and GcrGnupgProcess:error_line
 * signals will be emitted when data is received from the gpg process.
 *
 * Unless the %GCR_GNUPG_PROCESS_RESPECT_LOCALE flag is specified, the process
 * will be run in the 'C' locale. If the %GCR_GNUPG_PROCESS_WITH_STATUS or
 * %GCR_GNUPG_PROCESS_WITH_ATTRIBUTES flags are set, then the gpg process
 * will be status and attribute output respectively. The
 * GcrGnupgProcess:status_record and GcrGnupgProcess:attribute_data signals
 * will provide this data.
 */
void
_gcr_gnupg_process_run_async (GcrGnupgProcess *self, const gchar **argv, const gchar **envp,
                              GcrGnupgProcessFlags flags, GCancellable *cancellable,
                              GAsyncReadyCallback callback, gpointer user_data)
{
	GError *error = NULL;
	GPtrArray *args;
	GPtrArray *envs;
	int child_fds[NUM_FDS];
	int status_fds[2] = { -1, -1 };
	int attribute_fds[2] = { -1, -1 };
	int output_fd = -1;
	int error_fd = -1;
	GnupgSource *gnupg_source;
	GSource *source;
	GPid pid;
	guint i;

	g_return_if_fail (GCR_IS_GNUPG_PROCESS (self));
	g_return_if_fail (argv);
	g_return_if_fail (callback);
	g_return_if_fail (cancellable == NULL || G_IS_CANCELLABLE (cancellable));

	g_return_if_fail (self->pv->running == FALSE);
	g_return_if_fail (self->pv->complete == FALSE);
	g_return_if_fail (self->pv->executable);

	self->pv->async_callback = callback;
	self->pv->user_data = user_data;

	for (i = 0; i < NUM_FDS; i++)
		child_fds[i] = -1;

	/* The command needs to be updated with these status and attribute fds */
	args = g_ptr_array_new_with_free_func (g_free);
	g_ptr_array_add (args, g_strdup (self->pv->executable));

	/* Spawn/child will close all other attributes, besides thesthose in child_fds */
	child_fds[FD_OUTPUT] = 1;
	child_fds[FD_ERROR] = 2;

	if (flags & GCR_GNUPG_PROCESS_WITH_STATUS) {
		if (pipe (status_fds) < 0)
			g_return_if_reached ();
		child_fds[FD_STATUS] = status_fds[1];
		g_ptr_array_add (args, g_strdup ("--status-fd"));
		g_ptr_array_add (args, g_strdup_printf ("%d", child_fds[FD_STATUS]));
	}
	if (flags & GCR_GNUPG_PROCESS_WITH_ATTRIBUTES) {
		if (pipe (attribute_fds) < 0)
			g_return_if_reached ();
		child_fds[FD_ATTRIBUTE] = attribute_fds[1];
		g_ptr_array_add (args, g_strdup ("--attribute-fd"));
		g_ptr_array_add (args, g_strdup_printf ("%d", child_fds[FD_ATTRIBUTE]));
	}

	if (self->pv->directory) {
		g_ptr_array_add (args, g_strdup ("--homedir"));
		g_ptr_array_add (args, g_strdup (self->pv->directory));
	}

	/* All the remaining arguments */
	for (i = 0; argv[i] != NULL; i++)
		g_ptr_array_add (args, g_strdup (argv[i]));
	g_ptr_array_add (args, NULL);

	envs = g_ptr_array_new ();
	for (i = 0; envp && envp[i] != NULL; i++) {
		if (flags & GCR_GNUPG_PROCESS_RESPECT_LOCALE ||
		    !g_str_has_prefix (envp[i], "LOCALE="))
			g_ptr_array_add (envs, (gpointer)envp[i]);
	}
	if (!(flags & GCR_GNUPG_PROCESS_RESPECT_LOCALE))
		g_ptr_array_add (envs, (gpointer)"LOCALE=C");
	g_ptr_array_add (envs, NULL);

	if (_gcr_debugging) {
		gchar *command = g_strjoinv (" ", (gchar**)args->pdata);
		gchar *environ = g_strjoinv (", ", (gchar**)envs->pdata);
		_gcr_debug ("running command: %s", command);
		_gcr_debug ("process environment: %s", environ);
		g_free (command);
		g_free (environ);
	}

	g_spawn_async_with_pipes (self->pv->directory, (gchar**)args->pdata,
	                          (gchar**)envs->pdata, G_SPAWN_DO_NOT_REAP_CHILD,
	                          on_gnupg_process_child_setup,
	                          child_fds, &pid, NULL, &output_fd, &error_fd, &error);

	g_ptr_array_free (args, TRUE);
	g_ptr_array_free (envs, TRUE);

	/* Close 'wrong' ends of extra file descriptors */
	close_fd (&(status_fds[1]));
	close_fd (&(attribute_fds[1]));

	self->pv->complete = FALSE;
	self->pv->running = TRUE;

	if (error) {
		close_fd (&(status_fds[0]));
		close_fd (&(attribute_fds[0]));
		g_assert (!self->pv->error);
		self->pv->error = error;
		complete_run_process (self);
		run_async_ready_callback_later (self);
		return;
	}

	_gcr_debug ("process started: %d", (int)pid);

	source = g_source_new (&gnupg_source_funcs, sizeof (GnupgSource));

	/* Initialize the source */
	gnupg_source = (GnupgSource*)source;
	for (i = 0; i < NUM_FDS; i++)
		gnupg_source->polls[i].fd = -1;
	gnupg_source->error_buf = g_string_sized_new (128);
	gnupg_source->status_buf = g_string_sized_new (128);
	gnupg_source->process = g_object_ref (self);
	gnupg_source->child_pid = pid;

	gnupg_source->polls[FD_OUTPUT].fd = output_fd;
	if (output_fd >= 0) {
		gnupg_source->polls[FD_OUTPUT].events = G_IO_HUP | G_IO_IN;
		g_source_add_poll (source, &gnupg_source->polls[FD_OUTPUT]);
	}
	gnupg_source->polls[FD_ERROR].fd = error_fd;
	if (error_fd >= 0) {
		gnupg_source->polls[FD_ERROR].events = G_IO_HUP | G_IO_IN;
		g_source_add_poll (source, &gnupg_source->polls[FD_ERROR]);
	}
	gnupg_source->polls[FD_STATUS].fd = status_fds[0];
	if (status_fds[0] >= 0) {
		gnupg_source->polls[FD_STATUS].events = G_IO_HUP | G_IO_IN;
		g_source_add_poll (source, &gnupg_source->polls[FD_STATUS]);
	}
	gnupg_source->polls[FD_ATTRIBUTE].fd = attribute_fds[0];
	if (attribute_fds[0] >= 0) {
		gnupg_source->polls[FD_ATTRIBUTE].events = G_IO_HUP | G_IO_IN;
		g_source_add_poll (source, &gnupg_source->polls[FD_ATTRIBUTE]);
	}

	if (cancellable) {
		gnupg_source->cancellable = g_object_ref (cancellable);
		gnupg_source->cancel_sig = g_cancellable_connect (cancellable,
		                                                  G_CALLBACK (on_cancellable_cancelled),
		                                                  g_source_ref (source),
		                                                  (GDestroyNotify)g_source_unref);
	}

	g_assert (!self->pv->source_sig);
	g_source_set_callback (source, unused_callback, NULL, NULL);
	self->pv->source_sig = g_source_attach (source, g_main_context_default ());

	/* This assumes the outstanding reference to source */
	g_assert (!gnupg_source->child_sig);
	gnupg_source->child_sig = g_child_watch_add_full (G_PRIORITY_DEFAULT, pid,
	                                                  on_gnupg_process_child_exited,
	                                                  g_source_ref (source),
	                                                  (GDestroyNotify)g_source_unref);

	/* source is unreffed in complete_if_source_is_done() */
}

/**
 * _gcr_gnupg_process_run_finish:
 * @self: The process
 * @result: The result passed to the callback
 * @error: Location to raise an error on failure.
 *
 * Get the result of running a gnupg process.
 *
 * Return value: Whether the Gnupg process was run or not.
 */
gboolean
_gcr_gnupg_process_run_finish (GcrGnupgProcess *self, GAsyncResult *result,
                               GError **error)
{
	g_return_val_if_fail (GCR_IS_GNUPG_PROCESS (self), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);
	g_return_val_if_fail (G_ASYNC_RESULT (self) == result, FALSE);
	g_return_val_if_fail (self->pv->complete, FALSE);

	/* This allows the process to run again... */
	self->pv->complete = FALSE;

	g_assert (!self->pv->running);
	g_assert (!self->pv->async_callback);
	g_assert (!self->pv->user_data);
	g_assert (!self->pv->source_sig);

	if (self->pv->error) {
		g_propagate_error (error, self->pv->error);
		self->pv->error = NULL;
		return FALSE;
	}

	return TRUE;
}
