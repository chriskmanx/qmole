/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-keyring.c - library for talking to the keyring daemon.

   Copyright (C) 2003 Red Hat, Inc
   Copyright (C) 2007 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Alexander Larsson <alexl@redhat.com>
   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#define DEBUG_FLAG GKR_DEBUG_OPERATION
#include "gkr-debug.h"
#include "gkr-misc.h"
#include "gkr-operation.h"
#include "gkr-session.h"

#include "gnome-keyring.h"
#include "gnome-keyring-private.h"

#include "egg/egg-dbus.h"

/* Exposed in gkr-operation.h */
gboolean gkr_inited = FALSE;

static DBusConnection *dbus_connection = NULL;
G_LOCK_DEFINE_STATIC(dbus_connection);

enum {
	INCOMPLETE = -1,
};

struct _GkrOperation {
	/* First two only atomically accessed */
	gint refs;
	gint result;

	/* Call information */
	DBusConnection *conn;
	gboolean prompting;
	DBusMessage *request;

	/* Asynchronous calls */
	gboolean asynchronous;
	DBusPendingCall *pending;

	/* Some strange status fields */
	gboolean was_keyring;

	/* Callback information */
	GQueue callbacks;
	GSList *completed;
};

void
gkr_operation_init (void)
{
	static gsize onced = 0;

	if (g_once_init_enter (&onced)) {
		gkr_inited = TRUE;

		/*
		 * Make sure we initialize dbus thread primitives as apps may
		 * (and do) call g-k in threads. This isn't 100% safe, because
		 * someone might have been doing dbus calls before without
		 * initializing threads. However, there is no safe solution to
		 * this other than just making all such places initialze threads
		 * (or port to GDBus), so we do it anyway. (GVfs does it too, so
		 * most apps end up doing it that way.)
		 */

		dbus_threads_init_default ();

		g_once_init_leave (&onced, 1);
	}
}

GkrOperation*
gkr_operation_ref (GkrOperation *op)
{
	g_assert (op);
	g_atomic_int_inc (&op->refs);
	return op;
}

static void
operation_clear_callbacks (GkrOperation *op)
{
	GSList *l;

	g_assert (op);
	gkr_debug ("%p", op);

	while (!g_queue_is_empty (&op->callbacks))
		gkr_callback_free (g_queue_pop_head (&op->callbacks));
	g_queue_clear (&op->callbacks);
	for (l = op->completed; l; l = g_slist_next (l))
		gkr_callback_free (l->data);
	g_slist_free (op->completed);
	op->completed = NULL;
}

static gboolean
operation_unref (gpointer data)
{
	GkrOperation *op = data;
	g_assert (op);

	if (!g_atomic_int_dec_and_test (&op->refs))
		return FALSE;

	if (op->pending) {
		gkr_debug ("%p: cancelling: %p", op, op->pending);
		dbus_pending_call_cancel (op->pending);
		dbus_pending_call_unref (op->pending);
		op->pending = NULL;
	}

	operation_clear_callbacks (op);

	gkr_debug ("%p: freeing", op);

	if (op->conn) {
		dbus_connection_unref (op->conn);
		op->conn = NULL;
	}

	g_slice_free (GkrOperation, op);
	return TRUE;
}

void
gkr_operation_unref (gpointer data)
{
	GkrOperation *op = data;
	g_assert (op);
	operation_unref (op);
}

GnomeKeyringResult
gkr_operation_unref_get_result (GkrOperation *op)
{
	GnomeKeyringResult res = gkr_operation_get_result (op);
	gkr_operation_unref (op);
	return res;
}

GkrOperation*
gkr_operation_new (gpointer callback, GkrCallbackType callback_type,
                   gpointer user_data, GDestroyNotify destroy_user_data)
{
	GkrOperation *op;

	op = g_slice_new0 (GkrOperation);
	gkr_debug ("%p", op);

	op->refs = 1;
	op->result = INCOMPLETE;
	g_queue_init (&op->callbacks);
	op->completed = NULL;

	gkr_operation_push (op, callback, callback_type, user_data, destroy_user_data);

	return op;
}

GkrCallback*
gkr_operation_push (GkrOperation *op, gpointer callback,
                    GkrCallbackType callback_type,
                    gpointer user_data, GDestroyNotify destroy_func)
{
	GkrCallback *cb = gkr_callback_new (op, callback, callback_type, user_data, destroy_func);
	g_assert (op);
	g_queue_push_head (&op->callbacks, cb);
	return cb;
}

GkrCallback*
gkr_operation_pop (GkrOperation *op)
{
	GkrCallback *cb;

	g_assert (op);

	cb = g_queue_pop_head (&op->callbacks);
	g_assert (cb);
	op->completed = g_slist_prepend (op->completed, cb);
	return cb;
}

GnomeKeyringResult
gkr_operation_get_result (GkrOperation *op)
{
	g_assert (op);
	return g_atomic_int_get (&op->result);
}

gboolean
gkr_operation_set_result (GkrOperation *op, GnomeKeyringResult res)
{
	g_assert (op);
	g_assert ((int) res != INCOMPLETE);
	g_atomic_int_compare_and_exchange (&op->result, INCOMPLETE, res);
	return g_atomic_int_get (&op->result) == res; /* Success when already set to res */
}

static void
on_complete (GkrOperation *op)
{
	GkrCallback *cb;

	g_assert (op);

	cb = g_queue_pop_tail (&op->callbacks);
	g_assert (cb);

	gkr_debug ("%p", op);

	/* Free all the other callbacks */
	operation_clear_callbacks (op);

	gkr_callback_invoke_res (cb, gkr_operation_get_result (op));
	gkr_callback_free (cb);
}

static gboolean
on_complete_later (gpointer data)
{
	GkrOperation *op = data;

	/* Often we've already responded by the time the callback hits */
	if (!g_queue_is_empty (&op->callbacks))
		on_complete (op);

	return FALSE; /* Don't run idle handler again */
}

void
gkr_operation_complete (GkrOperation *op, GnomeKeyringResult res)
{
	g_return_if_fail (op);
	if (gkr_operation_set_result (op, res))
		on_complete (op);
}

void
gkr_operation_complete_later (GkrOperation *op, GnomeKeyringResult res)
{
	g_return_if_fail (op);
	if (gkr_operation_set_result (op, res)) {
		gkr_debug ("%p", op);
		g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, on_complete_later,
		                 gkr_operation_ref (op), gkr_operation_unref);
	}
}

static DBusHandlerResult
on_connection_filter (DBusConnection *connection,
                      DBusMessage *message,
                      void *user_data)
{
	const char *object_name;
	const char *new_owner;
	const char *old_owner;

	/* org.freedesktop.DBus.Local.Disconnected() */
	if (dbus_message_is_signal (message, DBUS_INTERFACE_LOCAL, "Disconnected")) {
		g_warning ("dbus connection disconnected for unknown reason");
		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	}

	/* org.freedesktop.DBus.NameOwnerChanged(STRING name, STRING old_owner, STRING new_owner) */
	if (dbus_message_is_signal (message, DBUS_INTERFACE_DBUS, "NameOwnerChanged") &&
	    dbus_message_get_args (message, NULL, DBUS_TYPE_STRING, &object_name,
	                           DBUS_TYPE_STRING, &old_owner, DBUS_TYPE_STRING, &new_owner,
	                           DBUS_TYPE_INVALID)) {

		/* See if it's the secret service going away */
		if (object_name && g_str_equal (gkr_service_name (), object_name) &&
		    new_owner && g_str_equal ("", new_owner)) {

			/* Clear any session, when the service goes away */
			gkr_debug ("secret service went away");
			gkr_session_clear ();
		}

		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	}

	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusConnection*
connect_to_service (void)
{
	DBusError derr = DBUS_ERROR_INIT;
	DBusConnection *conn;
	const gchar *rule;

	/*
	 * TODO: We currently really have no way to close this connection or do
	 * cleanup, and it's unclear how and whether we need to.
	 */

	if (!dbus_connection) {
		if (!g_getenv ("DBUS_SESSION_BUS_ADDRESS")) {
			gkr_debug ("no DBUS_SESSION_BUS_ADDRESS var set");
			return NULL;
		}

		conn = dbus_bus_get_private (DBUS_BUS_SESSION, &derr);
		if (conn == NULL) {
			g_message ("couldn't connect to dbus session bus: %s", derr.message);
			dbus_error_free (&derr);
			return NULL;
		}

		dbus_connection_set_exit_on_disconnect (conn, FALSE);

		/* Listen for the completed signal */
		rule = "type='signal',interface='org.gnome.secrets.Prompt',member='Completed'";
		dbus_bus_add_match (conn, rule, NULL);

		/* Listen for name owner changed signals */
		rule = "type='signal',member='NameOwnerChanged',interface='org.freedesktop.DBus'";
		dbus_bus_add_match (conn, rule, NULL);
		dbus_connection_add_filter (conn, on_connection_filter, NULL, NULL);

		G_LOCK (dbus_connection);
		{
			if (dbus_connection) {
				dbus_connection_unref (conn);
				gkr_debug ("race. already have a dbus connection");
			} else {
				egg_dbus_connect_with_mainloop (conn, NULL);
				dbus_connection = conn;
				gkr_debug ("created and initialized dbus connection");
			}
		}
		G_UNLOCK (dbus_connection);
	}

	return dbus_connection_ref (dbus_connection);
}

static void
callback_with_message (GkrOperation *op, DBusMessage *message)
{
	GkrCallback *cb;

	g_assert (op);
	g_assert (message);

	cb = g_queue_peek_head (&op->callbacks);
	g_assert (cb);

	/* A handler that knows what to do with a DBusMessage */
	if (cb->type == GKR_CALLBACK_OP_MSG)
		gkr_callback_invoke_op_msg (gkr_operation_pop (op), message);

	/* We hope this is a simple handler, invoke will check */
	else if (!gkr_operation_handle_errors (op, message))
		gkr_callback_invoke_res (gkr_operation_pop (op), GNOME_KEYRING_RESULT_OK);
}

static void
on_pending_call_notify (DBusPendingCall *pending, void *user_data)
{
	GkrOperation *op = user_data;
	DBusMessage *reply;

	gkr_debug ("%p: notified: %p", op, pending);
	g_assert (pending == op->pending);

	reply = dbus_pending_call_steal_reply (pending);
	g_return_if_fail (reply);

	gkr_operation_ref (op);

	dbus_pending_call_unref (op->pending);
	op->pending = NULL;

	callback_with_message (op, reply);
	dbus_message_unref (reply);

	gkr_operation_unref (op);
}

static void
send_with_pending (GkrOperation *op)
{
	int timeout = -1;

	g_assert (op);
	g_assert (op->request);
	g_assert (!op->pending);

#if WITH_TESTABLE
	timeout = INT_MAX;
#endif

	if (!op->conn)
		op->conn = connect_to_service ();

	if (op->conn) {
		gkr_debug ("%p: sending request", op);
		if (!dbus_connection_send_with_reply (op->conn, op->request,
		                                      &op->pending, timeout))
			g_return_if_reached ();
		dbus_message_unref (op->request);
		op->request = NULL;
	}

	if (op->pending) {
		gkr_debug ("%p: has pending: %p", op, op->pending);
		dbus_pending_call_set_notify (op->pending, on_pending_call_notify,
		                              gkr_operation_ref (op), gkr_operation_unref);
	} else {
		gkr_operation_complete_later (op, GNOME_KEYRING_RESULT_IO_ERROR);
	}
}

void
gkr_operation_request (GkrOperation *op, DBusMessage *req)
{
	g_return_if_fail (req);

	g_assert (op);
	g_assert (op->request == NULL);
	g_assert (op->pending == NULL);

	op->request = dbus_message_ref (req);

	if (gkr_decode_is_keyring (dbus_message_get_path (req)))
		gkr_operation_set_keyring_hint (op);

	/*
	 * The first requests waits until we know if we're asynchronous or not.
	 * Thereafter all async requests are sent directly from here. See
	 * gkr_operation_pending_and_unref() and gkr_operation_block_and_unref().
	 */

	if (op->asynchronous)
		send_with_pending (op);
}

gpointer
gkr_operation_pending_and_unref (GkrOperation *op)
{
	g_assert (op);

	/*
	 * Only the first asynch requests waits until here to be sent. Further
	 * requests on this operation get sent from gkr_operation_request() directly.
	 */

	g_assert (!op->asynchronous);
	op->asynchronous = TRUE;

	if (op->request)
		send_with_pending (op);

	/* Return op, if not the last reference */
	if (!operation_unref (op))
		return op;

	/* Not sure what to do here, but at least we can print a message */
	g_message ("a libgnome-keyring operation completed unsafely before "
	           "the function starting the operation returned.");
	return NULL;
}

static DBusMessage*
send_with_reply_and_block (DBusConnection *conn, DBusMessage *message,
                           int timeout)
{
	DBusMessage *reply;
	DBusPendingCall *pending;

	g_assert (conn);
	g_assert (message);

	if (!dbus_connection_send_with_reply (conn, message, &pending, timeout))
		g_return_val_if_reached (NULL);

	if (pending == NULL) {
		gkr_debug ("couldn't send message, dbus connection disconnected");
		return NULL;
	}

	dbus_pending_call_block (pending);
	reply = dbus_pending_call_steal_reply (pending);
	dbus_pending_call_unref (pending);

	return reply;
}

GnomeKeyringResult
gkr_operation_block_and_unref (GkrOperation *op)
{
	DBusMessage *reply = NULL;
	int timeout = -1;

	g_return_val_if_fail (op, BROKEN);

#if WITH_TESTABLE
	timeout = INT_MAX;
#endif

	gkr_debug ("%p: processing", op);

	g_assert (!op->pending);
	g_assert (!op->asynchronous);

	while ((int) gkr_operation_get_result (op) == INCOMPLETE) {

		/* No connection then connect */
		if (!op->conn) {
			op->conn = connect_to_service ();
			if (!op->conn)
				gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);

		/* A dbus request, then send and wait for reply */
		} else if (op->request) {
			gkr_debug ("%p: blocking request", op);
			reply = send_with_reply_and_block (op->conn, op->request, timeout);
			dbus_message_unref (op->request);
			op->request = NULL;

			if (reply != NULL) {
				callback_with_message (op, reply);
				dbus_message_unref (reply);
			} else {
				gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
			}

		/* Prompting then wait until prompting done */
		} else if (op->prompting) {
			dbus_connection_flush (op->conn);
			gkr_debug ("%p: blocking on prompt", op);
			while (op->prompting && (int) gkr_operation_get_result (op) == INCOMPLETE) {
				if (!dbus_connection_read_write_dispatch (op->conn, 200))
					break;
			}

		/* Not prompting and not requesting == bug in the code */
		} else {
			g_assert_not_reached ();
		}
	}

	/* Make sure we have run our callbacks if complete */
	if (!g_queue_is_empty (&op->callbacks))
		on_complete (op);

	gkr_debug ("%p: done", op);

	return gkr_operation_unref_get_result (op);
}

void
gkr_operation_set_keyring_hint (GkrOperation *op)
{
	op->was_keyring = TRUE;
}

gboolean
gkr_operation_handle_errors (GkrOperation *op, DBusMessage *reply)
{
	DBusError derr = DBUS_ERROR_INIT;
	GnomeKeyringResult res;
	gboolean was_keyring;

	g_assert (op);
	g_assert (reply);

	was_keyring = op->was_keyring;
	op->was_keyring = FALSE;

	if (dbus_set_error_from_message (&derr, reply)) {
		if (dbus_error_has_name (&derr, ERROR_NO_SUCH_OBJECT)) {
			gkr_debug ("%p: no-such-object", op);
			if (was_keyring)
				res = GNOME_KEYRING_RESULT_NO_SUCH_KEYRING;
			else
				res = GNOME_KEYRING_RESULT_BAD_ARGUMENTS;
		} else {
			g_message ("secret service operation failed: %s", derr.message);
			res = GNOME_KEYRING_RESULT_IO_ERROR;
		}

		dbus_error_free (&derr);
		gkr_operation_complete (op, res);
		return TRUE;
	}

	return FALSE;
}

typedef struct _on_prompt_args {
	GkrOperation *op;
	gchar *path;
} on_prompt_args;

static void
on_prompt_completed (void *user_data)
{
	on_prompt_args *args = user_data;

	g_return_if_fail (args->op->prompting);
	gkr_debug ("%p", args->op);

	gkr_operation_unref (args->op);
	args->op->prompting = FALSE;
}

static DBusHandlerResult
on_prompt_signal (DBusConnection *connection, DBusMessage *message, void *user_data)
{
	on_prompt_args *args = user_data;
	DBusMessageIter iter;
	dbus_bool_t dismissed;
	GkrOperation *op;
	const char *object_name;
	const char *new_owner;
	const char *old_owner;

	g_assert (args);

	if (!args->path || !args->op->prompting) {
		gkr_debug ("%p: received prompt signal while not prompting", args->op);
		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	}

	/* org.freedesktop.Secret.Prompt.Completed(BOOLEAN dismissed, VARIANT result) */
	if (dbus_message_has_path (message, args->path) &&
	    dbus_message_is_signal (message, PROMPT_INTERFACE, "Completed")) {

		/* Only one call, even if daemon decides to be strange */
		g_free (args->path);
		args->path = NULL;

		if (!dbus_message_iter_init (message, &iter) ||
		    dbus_message_iter_get_arg_type (&iter) != DBUS_TYPE_BOOLEAN)
			g_return_val_if_reached (BROKEN);
		dbus_message_iter_get_basic (&iter, &dismissed);

		op = gkr_operation_ref (args->op);

		if (dismissed) {
			gkr_debug ("%p: prompt was dismissed", op);
			gkr_operation_complete (op, GNOME_KEYRING_RESULT_CANCELLED);
		} else {
			gkr_debug ("%p: prompt was completed", op);
			callback_with_message (op, message);
		}

		if (op->prompting)
			dbus_connection_remove_filter (args->op->conn, on_prompt_signal, args);
		gkr_operation_unref (op);

		return DBUS_HANDLER_RESULT_HANDLED;
	}

	/* org.freedesktop.DBus.NameOwnerChanged(STRING name, STRING old_owner, STRING new_owner) */
	if (dbus_message_is_signal (message, DBUS_INTERFACE_DBUS, "NameOwnerChanged") &&
	    dbus_message_get_args (message, NULL, DBUS_TYPE_STRING, &object_name,
	                           DBUS_TYPE_STRING, &old_owner, DBUS_TYPE_STRING, &new_owner,
	                           DBUS_TYPE_INVALID)) {

		/* See if it's the secret service going away */
		if (object_name && g_str_equal (gkr_service_name (), object_name) &&
		    new_owner && g_str_equal ("", new_owner)) {

			g_message ("secret service disappeared while waiting for prompt");
			op = gkr_operation_ref (args->op);
			gkr_operation_complete (op, GNOME_KEYRING_RESULT_IO_ERROR);
			if (op->prompting)
				dbus_connection_remove_filter (args->op->conn, on_prompt_signal, args);
			gkr_operation_unref (op);
		}

		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	}

	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static void
on_prompt_result (GkrOperation *op, DBusMessage *message, gpointer user_data)
{
	gkr_operation_handle_errors (op, message);
}

static void
on_prompt_free (gpointer data)
{
	on_prompt_args *args = data;
	g_assert (args);
	g_assert (args->op);
	if (args->op->prompting)
		dbus_connection_remove_filter (args->op->conn, on_prompt_signal, args);
	g_free (args->path);
	g_slice_free (on_prompt_args, args);
}

void
gkr_operation_prompt (GkrOperation *op, const gchar *prompt)
{
	on_prompt_args *args;
	DBusMessage *req;
	const char *window_id;

	g_return_if_fail (prompt);
	g_assert (op);

	/*
	 * args becomes owned by the operation. In addition in its
	 * destroy_func it disconnects the connection filter. So keep
	 * that in mind with the lack of references below.
	 */

	args = g_slice_new (on_prompt_args);
	args->path = g_strdup (prompt);

	/* This reference and flag is cleared by on_prompt_completed */
	args->op = gkr_operation_ref (op);
	args->op->prompting = TRUE;
	dbus_connection_add_filter (op->conn, on_prompt_signal, args,
	                            on_prompt_completed);

	req = dbus_message_new_method_call (gkr_service_name (), prompt,
	                                    PROMPT_INTERFACE, "Prompt");

	window_id = "";
	dbus_message_append_args (req, DBUS_TYPE_STRING, &window_id, DBUS_TYPE_INVALID);

	gkr_debug ("%p: calling prompt method", op);

	gkr_operation_push (op, on_prompt_result, GKR_CALLBACK_OP_MSG, args, on_prompt_free);
	gkr_operation_request (op, req);
	dbus_message_unref (req);
}
