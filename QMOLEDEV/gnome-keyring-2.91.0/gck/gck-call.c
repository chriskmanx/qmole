/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-call.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2008, Stefan Walter

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

   Author: Stef Walter <nielsen@memberwebs.com>
*/

#include "config.h"

#include "gck-private.h"

#include <string.h>

typedef struct _GckCallSource GckCallSource;

static gpointer _gck_call_parent_class = NULL;

struct _GckCall {
	GObject parent;
	GckModule *module;

	/* For making the call */
	GckPerformFunc perform;
	GckCompleteFunc complete;
	GckArguments *args;
	GCancellable *cancellable;
	GDestroyNotify destroy;
	CK_RV rv;

	/* For result callback only */
	gpointer object;
	GAsyncReadyCallback callback;
	gpointer user_data;
};

struct _GckCallClass {
	GObjectClass parent;
	GThreadPool *thread_pool;
	GAsyncQueue *completed_queue;
	guint completed_id;
};

struct _GckCallSource {
	GSource source;
	GckCallClass *klass;
};

/* ----------------------------------------------------------------------------
 * HELPER FUNCTIONS
 */

static CK_RV
perform_call (GckPerformFunc func, GCancellable *cancellable, GckArguments *args)
{
	CK_RV rv;

	/* Double check a few things */
	g_assert (func);
	g_assert (args);

	if (cancellable) {
		if (g_cancellable_is_cancelled (cancellable)) {
			return CKR_FUNCTION_CANCELED;
		}

		/* Push for the notify callback */
		g_object_ref (cancellable);
		g_cancellable_push_current (cancellable);
	}

	rv = (func) (args);

	if (cancellable) {
		g_cancellable_pop_current (cancellable);
		g_object_unref (cancellable);
	}

	return rv;
}

static gboolean
complete_call (GckCompleteFunc func, GckArguments *args, CK_RV result)
{
	/* Double check a few things */
	g_assert (args);

	/* If no complete function, then just ignore */
	if (!func)
		return TRUE;

	return (func) (args, result);
}


static void
process_async_call (gpointer data, GckCallClass *klass)
{
	GckCall *call = GCK_CALL (data);

	g_assert (GCK_IS_CALL (call));

	call->rv = perform_call (call->perform, call->cancellable, call->args);

	g_async_queue_push (klass->completed_queue, call);

	/* Wakeup main thread if on a separate thread */
	g_main_context_wakeup (NULL);
}

static void
process_result (GckCall *call, gpointer unused)
{
	gboolean stop = FALSE;

	/* Double check a few things */
	g_assert (GCK_IS_CALL (call));

	if (call->cancellable) {
		/* Don't call the callback when cancelled */
		if (g_cancellable_is_cancelled (call->cancellable)) {
			call->rv = CKR_FUNCTION_CANCELED;
			stop = TRUE;
		}
	}

	/*
	 * Hmmm, does the function want to actually be done?
	 * If not, then queue this call again.
	 */
	if (!stop && !complete_call (call->complete, call->args, call->rv)) {
		g_object_ref (call);
		g_thread_pool_push (GCK_CALL_GET_CLASS (call)->thread_pool, call, NULL);

	/* All done, finish processing */
	} else if (call->callback) {
		g_assert (G_IS_OBJECT (call->object));
		(call->callback) (G_OBJECT (call->object), G_ASYNC_RESULT (call),
				  call->user_data);
	}
}

static gboolean
process_completed (GckCallClass *klass)
{
	gpointer call;

	g_assert (klass->completed_queue);

	call = g_async_queue_try_pop (klass->completed_queue);
	if (call) {
		process_result (call, NULL);
		g_object_unref (call);
		return TRUE;
	}

	return FALSE;
}

static gboolean
completed_prepare(GSource* base, gint *timeout)
{
	GckCallSource *source = (GckCallSource*)base;
	gboolean have;

	g_assert (source->klass->completed_queue);
	have = g_async_queue_length (source->klass->completed_queue) > 0;
	*timeout = have ? 0 : -1;
	return have;
}

static gboolean
completed_check(GSource* base)
{
	GckCallSource *source = (GckCallSource*)base;
	g_assert (source->klass->completed_queue);
	return g_async_queue_length (source->klass->completed_queue) > 0;
}

static gboolean
completed_dispatch(GSource* base, GSourceFunc callback, gpointer user_data)
{
	GckCallSource *source = (GckCallSource*)base;
	process_completed (source->klass);
	return TRUE;
}

static void
completed_finalize(GSource* base)
{

}

static GSourceFuncs completed_functions = {
	completed_prepare,
	completed_check,
	completed_dispatch,
	completed_finalize
};

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static void
_gck_call_init (GckCall *call)
{
	call->rv = CKR_OK;
}

static void
_gck_call_finalize (GObject *obj)
{
	GckCall *call = GCK_CALL (obj);

	if (call->module)
		g_object_unref (call->module);
	call->module = NULL;

	if (call->object)
		g_object_unref (call->object);
	call->object = NULL;

	if (call->cancellable)
		g_object_unref (call->cancellable);
	call->cancellable = NULL;

	if (call->destroy)
		(call->destroy) (call->args);
	call->destroy = NULL;
	call->args = NULL;

	G_OBJECT_CLASS (_gck_call_parent_class)->finalize (obj);
}

static gpointer
_gck_call_get_user_data (GAsyncResult *async_result)
{
	g_return_val_if_fail (GCK_IS_CALL (async_result), NULL);
	return GCK_CALL (async_result)->user_data;
}

static GObject*
_gck_call_get_source_object (GAsyncResult *async_result)
{
	g_return_val_if_fail (GCK_IS_CALL (async_result), NULL);
	return GCK_CALL (async_result)->object;
}

static void
_gck_call_implement_async_result (GAsyncResultIface *iface)
{
	iface->get_user_data = _gck_call_get_user_data;
	iface->get_source_object = _gck_call_get_source_object;
}

static void
_gck_call_class_init (GckCallClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;

	_gck_call_parent_class = g_type_class_peek_parent (klass);
	gobject_class->finalize = _gck_call_finalize;
}

static void
_gck_call_base_init (GckCallClass *klass)
{
	GckCallSource *source;
	GMainContext *context;
	GError *err = NULL;

	klass->thread_pool = g_thread_pool_new ((GFunc)process_async_call, klass, 16, FALSE, &err);
	if (!klass->thread_pool) {
		g_critical ("couldn't create thread pool: %s",
		            err && err->message ? err->message : "");
		return;
	}

	klass->completed_queue = g_async_queue_new_full (g_object_unref);
	g_assert (klass->completed_queue);

	context = g_main_context_default ();
	g_assert (context);

	/* Add our idle handler which processes other tasks */
	source = (GckCallSource*)g_source_new (&completed_functions, sizeof (GckCallSource));
	source->klass = klass;
	klass->completed_id = g_source_attach ((GSource*)source, context);
	g_source_set_callback ((GSource*)source, NULL, NULL, NULL);
	g_source_unref ((GSource*)source);
}

static void
_gck_call_base_finalize (GckCallClass *klass)
{
	GMainContext *context;
	GSource *src;

	if (klass->thread_pool) {
		g_assert (g_thread_pool_unprocessed (klass->thread_pool) == 0);
		g_thread_pool_free (klass->thread_pool, FALSE, TRUE);
		klass->thread_pool = NULL;
	}

	if (klass->completed_id) {
		context = g_main_context_default ();
		g_return_if_fail (context);

		src = g_main_context_find_source_by_id (context, klass->completed_id);
		g_assert (src);
		g_source_destroy (src);
		klass->completed_id = 0;
	}

	if (klass->completed_queue) {
		g_assert (g_async_queue_length (klass->completed_queue));
		g_async_queue_unref (klass->completed_queue);
		klass->completed_queue = NULL;
	}
}

GType
_gck_call_get_type (void)
{
	static volatile gsize type_id__volatile = 0;

	if (g_once_init_enter (&type_id__volatile)) {

		static const GTypeInfo type_info = {
			sizeof (GckCallClass),
			(GBaseInitFunc)_gck_call_base_init,
			(GBaseFinalizeFunc)_gck_call_base_finalize,
			(GClassInitFunc)_gck_call_class_init,
			(GClassFinalizeFunc)NULL,
			NULL,   // class_data
			sizeof (GckCall),
			0,      // n_preallocs
			(GInstanceInitFunc)_gck_call_init,
		};

		static const GInterfaceInfo interface_info = {
			(GInterfaceInitFunc)_gck_call_implement_async_result
		};

		GType type_id = g_type_register_static (G_TYPE_OBJECT, "_GckCall", &type_info, 0);
		g_type_add_interface_static (type_id, G_TYPE_ASYNC_RESULT, &interface_info);

		g_once_init_leave (&type_id__volatile, type_id);
	}

	return type_id__volatile;
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

void
_gck_call_uninitialize (void)
{

}

gboolean
_gck_call_sync (gpointer object, gpointer perform, gpointer complete,
                 gpointer data, GCancellable *cancellable, GError **err)
{
	GckArguments *args = (GckArguments*)data;
	GckModule *module = NULL;
	CK_RV rv;

	g_assert (!object || G_IS_OBJECT (object));
	g_assert (perform);
	g_assert (args);

	if (object) {
		g_object_get (object, "module", &module, "handle", &args->handle, NULL);
		g_assert (GCK_IS_MODULE (module));

		/* We now hold a reference to module until below */
		args->pkcs11 = gck_module_get_functions (module);
		g_assert (args->pkcs11);
	}

	do {
		rv = perform_call (perform, cancellable, args);
		if (rv == CKR_FUNCTION_CANCELED)
			break;

	} while (!complete_call (complete, args, rv));

	if (module)
		g_object_unref (module);

	if (rv == CKR_OK)
		return TRUE;

	g_set_error (err, GCK_ERROR, rv, "%s", gck_message_from_rv (rv));
	return FALSE;
}

gpointer
_gck_call_async_prep (gpointer object, gpointer cb_object, gpointer perform,
                       gpointer complete, gsize args_size, gpointer destroy)
{
	GckArguments *args;
	GckCall *call;

	g_assert (!object || G_IS_OBJECT (object));
	g_assert (perform);

	if (!destroy)
		destroy = g_free;

	if (args_size == 0)
		args_size = sizeof (GckArguments);
	g_assert (args_size >= sizeof (GckArguments));

	args = g_malloc0 (args_size);
	call = g_object_new (GCK_TYPE_CALL, NULL);
	call->destroy = (GDestroyNotify)destroy;
	call->perform = (GckPerformFunc)perform;
	call->complete = (GckCompleteFunc)complete;
	call->object = cb_object;
	g_object_ref (cb_object);

	/* Hook the two together */
	call->args = args;
	call->args->call = call;

	/* Setup call object if available */
	if (object != NULL)
		_gck_call_async_object (call, object);

	return args;
}

void
_gck_call_async_object (GckCall *call, gpointer object)
{
	g_assert (GCK_IS_CALL (call));
	g_assert (call->args);

	if (call->module)
		g_object_unref (call->module);
	call->module = NULL;

	g_object_get (object, "module", &call->module, "handle", &call->args->handle, NULL);
	g_assert (GCK_IS_MODULE (call->module));
	call->args->pkcs11 = gck_module_get_functions (call->module);

	/* We now hold a reference on module until finalize */
}

GckCall*
_gck_call_async_ready (gpointer data, GCancellable *cancellable,
                        GAsyncReadyCallback callback, gpointer user_data)
{
	GckArguments *args = (GckArguments*)data;
	g_assert (GCK_IS_CALL (args->call));

	args->call->cancellable = cancellable;
	if (cancellable) {
		g_assert (G_IS_CANCELLABLE (cancellable));
		g_object_ref (cancellable);
	}

	args->call->callback = callback;
	args->call->user_data = user_data;

	return args->call;
}

void
_gck_call_async_go (GckCall *call)
{
	g_assert (GCK_IS_CALL (call));

	/* To keep things balanced, process at one completed event */
	process_completed(GCK_CALL_GET_CLASS (call));

	g_assert (GCK_CALL_GET_CLASS (call)->thread_pool);
	g_thread_pool_push (GCK_CALL_GET_CLASS (call)->thread_pool, call, NULL);
}

void
_gck_call_async_ready_go (gpointer data, GCancellable *cancellable,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	GckCall *call = _gck_call_async_ready (data, cancellable, callback, user_data);
	_gck_call_async_go (call);
}

gboolean
_gck_call_basic_finish (GAsyncResult *result, GError **err)
{
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_CALL (result), FALSE);

	rv = GCK_CALL (result)->rv;
	if (rv == CKR_OK)
		return TRUE;

	g_set_error (err, GCK_ERROR, rv, "%s", gck_message_from_rv (rv));
	return FALSE;
}

void
_gck_call_async_short (GckCall *call, CK_RV rv)
{
	g_assert (GCK_IS_CALL (call));

	call->rv = rv;

	/* Already complete, so just push it for processing in main loop */
	g_assert (GCK_CALL_GET_CLASS (call)->completed_queue);
	g_async_queue_push (GCK_CALL_GET_CLASS (call)->completed_queue, call);
	g_main_context_wakeup (NULL);
}

gpointer
_gck_call_get_arguments (GckCall *call)
{
	g_assert (GCK_IS_CALL (call));
	return call->args;
}
