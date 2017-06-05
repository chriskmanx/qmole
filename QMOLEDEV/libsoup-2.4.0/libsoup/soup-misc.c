/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-misc.c: Miscellaneous functions

 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#include <ctype.h>
#include <string.h>

#include "soup-misc.h"

/**
 * SECTION:soup-misc
 * @short_description: Miscellaneous functions
 *
 **/

/**
 * soup_str_case_hash:
 * @key: ASCII string to hash
 *
 * Hashes @key in a case-insensitive manner.
 *
 * Return value: the hash code.
 **/
guint
soup_str_case_hash (gconstpointer key)
{
	const char *p = key;
	guint h = g_ascii_toupper(*p);

	if (h)
		for (p += 1; *p != '\0'; p++)
			h = (h << 5) - h + g_ascii_toupper(*p);

	return h;
}

/**
 * soup_str_case_equal:
 * @v1: an ASCII string
 * @v2: another ASCII string
 *
 * Compares @v1 and @v2 in a case-insensitive manner
 *
 * Return value: %TRUE if they are equal (modulo case)
 **/
gboolean
soup_str_case_equal (gconstpointer v1,
		     gconstpointer v2)
{
	const char *string1 = v1;
	const char *string2 = v2;

	return g_ascii_strcasecmp (string1, string2) == 0;
}

typedef struct {
	gpointer instance;
	guint    signal_id;
} SoupSignalOnceData;

static void
signal_once_object_destroyed (gpointer ssod, GObject *ex_object)
{
	g_slice_free (SoupSignalOnceData, ssod);
}

static void
signal_once_metamarshal (GClosure *closure, GValue *return_value,
			 guint n_param_values, const GValue *param_values,
			 gpointer invocation_hint, gpointer marshal_data)
{
	SoupSignalOnceData *ssod = marshal_data;

	closure->marshal (closure, return_value, n_param_values,
			  param_values, invocation_hint,
			  ((GCClosure *)closure)->callback);

	if (g_signal_handler_is_connected (ssod->instance, ssod->signal_id))
		g_signal_handler_disconnect (ssod->instance, ssod->signal_id);
	g_object_weak_unref (G_OBJECT (ssod->instance), signal_once_object_destroyed, ssod);
	g_slice_free (SoupSignalOnceData, ssod);
}

/* No longer prototyped in soup-misc.h, because it's only used by
 * soup-connection.c, and will be going away once that usage is removed.
 */
guint soup_signal_connect_once  (gpointer instance, const char *detailed_signal,
				 GCallback c_handler, gpointer data);

guint
soup_signal_connect_once (gpointer instance, const char *detailed_signal,
			  GCallback c_handler, gpointer data)
{
	SoupSignalOnceData *ssod;
	GClosure *closure;

	g_return_val_if_fail (G_TYPE_CHECK_INSTANCE (instance), 0);
	g_return_val_if_fail (detailed_signal != NULL, 0);
	g_return_val_if_fail (c_handler != NULL, 0);

	ssod = g_slice_new0 (SoupSignalOnceData);
	ssod->instance = instance;
	g_object_weak_ref (G_OBJECT (instance), signal_once_object_destroyed, ssod);

	closure = g_cclosure_new (c_handler, data, NULL);
	g_closure_set_meta_marshal (closure, ssod, signal_once_metamarshal);

	ssod->signal_id = g_signal_connect_closure (instance, detailed_signal,
						    closure, FALSE);
	return ssod->signal_id;
}

/**
 * soup_add_io_watch:
 * @async_context: the #GMainContext to dispatch the I/O watch in, or
 * %NULL for the default context
 * @chan: the #GIOChannel to watch
 * @condition: the condition to watch for
 * @function: the callback to invoke when @condition occurs
 * @data: user data to pass to @function
 *
 * Adds an I/O watch as with g_io_add_watch(), but using the given
 * @async_context.
 *
 * Return value: a #GSource, which can be removed from @async_context
 * with g_source_destroy().
 **/
GSource *
soup_add_io_watch (GMainContext *async_context,
		   GIOChannel *chan, GIOCondition condition,
		   GIOFunc function, gpointer data)
{
	GSource *watch = g_io_create_watch (chan, condition);
	g_source_set_callback (watch, (GSourceFunc) function, data, NULL);
	g_source_attach (watch, async_context);
	g_source_unref (watch);
	return watch;
}

/**
 * soup_add_idle:
 * @async_context: the #GMainContext to dispatch the idle event in, or
 * %NULL for the default context
 * @function: the callback to invoke at idle time
 * @data: user data to pass to @function
 *
 * Adds an idle event as with g_idle_add(), but using the given
 * @async_context.
 *
 * Return value: a #GSource, which can be removed from @async_context
 * with g_source_destroy().
 **/
GSource *
soup_add_idle (GMainContext *async_context,
	       GSourceFunc function, gpointer data)
{
	GSource *source = g_idle_source_new ();
	g_source_set_callback (source, function, data, NULL);
	g_source_attach (source, async_context);
	g_source_unref (source);
	return source;
}

/**
 * soup_add_timeout:
 * @async_context: the #GMainContext to dispatch the timeout in, or
 * %NULL for the default context
 * @interval: the timeout interval, in milliseconds
 * @function: the callback to invoke at timeout time
 * @data: user data to pass to @function
 *
 * Adds a timeout as with g_timeout_add(), but using the given
 * @async_context.
 *
 * Return value: a #GSource, which can be removed from @async_context
 * with g_source_destroy().
 **/
GSource *
soup_add_timeout (GMainContext *async_context,
		  guint interval,
		  GSourceFunc function, gpointer data)
{
	GSource *source = g_timeout_source_new (interval);
	g_source_set_callback (source, function, data, NULL);
	g_source_attach (source, async_context);
	g_source_unref (source);
	return source;
}
