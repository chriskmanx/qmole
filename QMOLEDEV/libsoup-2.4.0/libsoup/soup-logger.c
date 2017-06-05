/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-logger.c
 *
 * Copyright (C) 2001-2004 Novell, Inc.
 * Copyright (C) 2008 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>

#include "soup-logger.h"
#include "soup-message.h"
#include "soup-uri.h"

/**
 * SECTION:soup-logger
 * @short_description: Debug logging support
 *
 * #SoupLogger watches a #SoupSession and logs the HTTP traffic that
 * it generates, for debugging purposes. Many applications use an
 * environment variable to determine whether or not to use
 * #SoupLogger, and to determine the amount of debugging output.
 *
 * To use #SoupLogger, first create a logger with soup_logger_new(),
 * optionally configure it with soup_logger_set_request_filter(),
 * soup_logger_set_response_filter(), and soup_logger_set_printer(),
 * and then attach it to a session (or multiple sessions) with
 * soup_logger_attach().
 *
 * By default, the debugging output is sent to %stdout, and looks
 * something like:
 *
 * <informalexample><screen>
 * > POST /unauth HTTP/1.1
 * > Soup-Debug-Timestamp: 1200171744
 * > Soup-Debug: SoupSessionAsync 1 (0x612190), SoupMessage 1 (0x617000), SoupSocket 1 (0x612220)
 * > Host: localhost
 * > Content-Type: text/plain
 * > Connection: close
 * > 
 * > This is a test.
 *   
 * &lt; HTTP/1.1 201 Created
 * &lt; Soup-Debug-Timestamp: 1200171744
 * &lt; Soup-Debug: SoupMessage 1 (0x617000)
 * &lt; Date: Sun, 12 Jan 2008 21:02:24 GMT
 * &lt; Content-Length: 0
 * </screen></informalexample>
 *
 * The <literal>Soup-Debug-Timestamp</literal> line gives the time (as
 * a #time_t) when the request was sent, or the response fully
 * received.
 *
 * The <literal>Soup-Debug</literal> line gives further debugging
 * information about the #SoupSession, #SoupMessage, and #SoupSocket
 * involved; the hex numbers are the addresses of the objects in
 * question (which may be useful if you are running in a debugger).
 * The decimal IDs are simply counters that uniquely identify objects
 * across the lifetime of the #SoupLogger. In particular, this can be
 * used to identify when multiple messages are sent across the same
 * connection.
 **/

G_DEFINE_TYPE (SoupLogger, soup_logger, G_TYPE_OBJECT)

typedef struct {
	/* We use a mutex so that if requests are being run in
	 * multiple threads, we don't mix up the output.
	 */
	GMutex             *lock;

	GQuark              tag;
	GHashTable         *ids;

	SoupLoggerLogLevel  level;
	int                 max_body_size;

	SoupLoggerFilter    request_filter;
	gpointer            request_filter_data;
	GDestroyNotify      request_filter_dnotify;

	SoupLoggerFilter    response_filter;
	gpointer            response_filter_data;
	GDestroyNotify      response_filter_dnotify;

	SoupLoggerPrinter   printer;
	gpointer            printer_data;
	GDestroyNotify      printer_dnotify;
} SoupLoggerPrivate;
#define SOUP_LOGGER_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_LOGGER, SoupLoggerPrivate))

static void
soup_logger_init (SoupLogger *logger)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	priv->lock = g_mutex_new ();
	priv->tag = g_quark_from_static_string (g_strdup_printf ("SoupLogger-%p", logger));
	priv->ids = g_hash_table_new (NULL, NULL);
}

static void
finalize (GObject *object)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (object);

	g_hash_table_destroy (priv->ids);

	if (priv->request_filter_dnotify)
		priv->request_filter_dnotify (priv->request_filter_data);
	if (priv->response_filter_dnotify)
		priv->response_filter_dnotify (priv->response_filter_data);
	if (priv->printer_dnotify)
		priv->printer_dnotify (priv->printer_data);

	g_mutex_free (priv->lock);

	G_OBJECT_CLASS (soup_logger_parent_class)->finalize (object);
}

static void
soup_logger_class_init (SoupLoggerClass *logger_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (logger_class);

	g_type_class_add_private (logger_class, sizeof (SoupLoggerPrivate));

	object_class->finalize = finalize;
}

/**
 * SoupLoggerLogLevel:
 * @SOUP_LOGGER_LOG_NONE: No logging
 * @SOUP_LOGGER_LOG_MINIMAL: Log the Request-Line or Status-Line and
 * the Soup-Debug pseudo-headers
 * @SOUP_LOGGER_LOG_HEADERS: Log the full request/response headers
 * @SOUP_LOGGER_LOG_BODY: Log the full headers and request/response
 * bodies.
 *
 * Describes the level of logging output to provide.
 **/

/**
 * soup_logger_new:
 * @level: the debug level
 * @max_body_size: the maximum body size to output, or -1
 *
 * Creates a new #SoupLogger with the given debug level. If @level is
 * %SOUP_LOGGER_LOG_BODY, @max_body_size gives the maximum number of
 * bytes of the body that will be logged. (-1 means "no limit".)
 *
 * If you need finer control over what message parts are and aren't
 * logged, use soup_logger_set_request_filter() and
 * soup_logger_set_response_filter().
 *
 * Returns: a new #SoupLogger
 **/
SoupLogger *
soup_logger_new (SoupLoggerLogLevel level, int max_body_size) 
{
	SoupLogger *logger;
	SoupLoggerPrivate *priv;

	logger = g_object_new (SOUP_TYPE_LOGGER, NULL);

	priv = SOUP_LOGGER_GET_PRIVATE (logger);
	priv->level = level;
	priv->max_body_size = max_body_size;

	return logger;
}

/**
 * SoupLoggerFilter:
 * @logger: the #SoupLogger
 * @msg: the message being logged
 * @user_data: the data passed to soup_logger_set_request_filter()
 * or soup_logger_set_response_filter()
 *
 * The prototype for a logging filter. The filter callback will be
 * invoked for each request or response, and should analyze it and
 * return a #SoupLoggerLogLevel value indicating how much of the
 * message to log. Eg, it might choose between %SOUP_LOGGER_LOG_BODY
 * and %SOUP_LOGGER_LOG_HEADERS depending on the Content-Type.
 *
 * Return value: a #SoupLoggerLogLevel value indicating how much of
 * the message to log
 **/

/**
 * soup_logger_set_request_filter:
 * @logger: a #SoupLogger
 * @request_filter: the callback for request debugging
 * @filter_data: data to pass to the callback
 * @destroy: a #GDestroyNotify to free @filter_data
 *
 * Sets up a filter to determine the log level for a given request.
 * For each HTTP request @logger will invoke @request_filter to
 * determine how much (if any) of that request to log. (If you do not
 * set a request filter, @logger will just always log requests at the
 * level passed to soup_logger_new().)
 **/
void
soup_logger_set_request_filter (SoupLogger       *logger,
				SoupLoggerFilter  request_filter,
				gpointer          filter_data,
				GDestroyNotify    destroy)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	priv->request_filter         = request_filter;
	priv->request_filter_data    = filter_data;
	priv->request_filter_dnotify = destroy;
}

/**
 * soup_logger_set_response_filter:
 * @logger: a #SoupLogger
 * @response_filter: the callback for response debugging
 * @filter_data: data to pass to the callback
 * @destroy: a #GDestroyNotify to free @filter_data
 *
 * Sets up a filter to determine the log level for a given response.
 * For each HTTP response @logger will invoke @response_filter to
 * determine how much (if any) of that response to log. (If you do not
 * set a response filter, @logger will just always log responses at
 * the level passed to soup_logger_new().)
 **/
void
soup_logger_set_response_filter (SoupLogger       *logger,
				 SoupLoggerFilter  response_filter,
				 gpointer          filter_data,
				 GDestroyNotify    destroy)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	priv->response_filter         = response_filter;
	priv->response_filter_data    = filter_data;
	priv->response_filter_dnotify = destroy;
}

/**
 * SoupLoggerPrinter:
 * @logger: the #SoupLogger
 * @level: the level of the information being printed.
 * @direction: a single-character prefix to @data
 * @data: data to print
 * @user_data: the data passed to soup_logger_set_printer()
 *
 * The prototype for a custom printing callback.
 *
 * @level indicates what kind of information is being printed. Eg, it
 * will be %SOUP_LOGGER_LOG_HEADERS if @data is header data.
 *
 * @direction is either '<', '>', or ' ', and @data is the single line
 * to print; the printer is expected to add a terminating newline.
 *
 * To get the effect of the default printer, you would do:
 *
 * <informalexample><programlisting>
 *	printf ("%c %s\n", direction, data);
 * </programlisting></informalexample>
 **/

/**
 * soup_logger_set_printer:
 * @logger: a #SoupLogger
 * @printer: the callback for printing logging output
 * @printer_data: data to pass to the callback
 * @destroy: a #GDestroyNotify to free @printer_data
 *
 * Sets up an alternate log printing routine, if you don't want
 * the log to go to %stdout.
 **/
void
soup_logger_set_printer (SoupLogger        *logger,
			 SoupLoggerPrinter  printer,
			 gpointer           printer_data,
			 GDestroyNotify     destroy)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	priv->printer         = printer;
	priv->printer_data    = printer_data;
	priv->printer_dnotify = destroy;
}

static guint
soup_logger_get_id (SoupLogger *logger, gpointer object)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	return GPOINTER_TO_UINT (g_object_get_qdata (object, priv->tag));
}

static guint
soup_logger_set_id (SoupLogger *logger, gpointer object)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);
	gpointer klass = G_OBJECT_GET_CLASS (object);
	gpointer id;

	id = g_hash_table_lookup (priv->ids, klass);
	id = (char *)id + 1;
	g_hash_table_insert (priv->ids, klass, id);

	g_object_set_qdata (object, priv->tag, id);
	return GPOINTER_TO_UINT (id);
}

static void
soup_logger_clear_id (SoupLogger *logger, gpointer object)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	g_object_set_qdata (object, priv->tag, NULL);
}

static void request_started (SoupSession *session, SoupMessage *msg,
			     SoupSocket *socket, gpointer user_data);

static void
weak_notify_unref (gpointer logger, GObject *ex_session)
{
	g_object_unref (logger);
}

/**
 * soup_logger_attach:
 * @logger: a #SoupLogger
 * @session: a #SoupSession
 *
 * Sets @logger to watch @session and print debug information for
 * its messages.
 *
 * (The session will take a reference on @logger, which will be
 * removed when you call soup_logger_detach(), or when the session is
 * destroyed.)
 **/
void
soup_logger_attach (SoupLogger  *logger,
		    SoupSession *session)
{
	if (!soup_logger_get_id (logger, session))
		soup_logger_set_id (logger, session);
	g_signal_connect (session, "request_started",
			  G_CALLBACK (request_started), logger);

	g_object_weak_ref (G_OBJECT (session),
			   weak_notify_unref, g_object_ref (logger));
}

/**
 * soup_logger_detach:
 * @logger: a #SoupLogger
 * @session: a #SoupSession
 *
 * Stops @logger from watching @session.
 **/
void
soup_logger_detach (SoupLogger  *logger,
		    SoupSession *session)
{
	g_signal_handlers_disconnect_by_func (session, request_started, logger);

	g_object_weak_unref (G_OBJECT (session),
			     weak_notify_unref, logger);
}

static void
soup_logger_print (SoupLogger *logger, SoupLoggerLogLevel level,
		   char direction, const char *format, ...)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);
	va_list args;
	char *data, *line, *end;

	va_start (args, format);
	data = g_strdup_vprintf (format, args);
	va_end (args);

	if (level == SOUP_LOGGER_LOG_BODY && priv->max_body_size > 0) {
		if (strlen (data) > priv->max_body_size + 6)
			strcpy (data + priv->max_body_size, "\n[...]");
	}

	line = data;
	do {
		end = strchr (line, '\n');
		if (end)
			*end = '\0';
		if (priv->printer) {
			priv->printer (logger, level, direction,
				       line, priv->printer_data);
		} else
			printf ("%c %s\n", direction, line);

		line = end + 1;
	} while (end && *line);

	g_free (data);
}

static void
soup_logger_print_basic_auth (SoupLogger *logger, const char *value)
{
	char *decoded, *p;
	gsize len;

	decoded = (char *)g_base64_decode (value + 6, &len);
	if (!decoded)
		decoded = g_strdup (value);
	p = strchr (decoded, ':');
	if (p) {
		while (++p < decoded + len)
			*p = '*';
	}
	soup_logger_print (logger, SOUP_LOGGER_LOG_HEADERS, '>',
			   "Authorization: Basic [%.*s]", len, decoded);
	g_free (decoded);
}

static void
print_request (SoupLogger *logger, SoupMessage *msg,
	       SoupSession *session, SoupSocket *socket,
	       gboolean restarted)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);
	SoupLoggerLogLevel log_level;
	SoupMessageHeadersIter iter;
	const char *name, *value;
	SoupURI *uri;

	if (priv->request_filter) {
		log_level = priv->request_filter (logger, msg,
						  priv->request_filter_data);
	} else
		log_level = priv->level;

	if (log_level == SOUP_LOGGER_LOG_NONE)
		return;

	uri = soup_message_get_uri (msg);
	if (msg->method == SOUP_METHOD_CONNECT) {
		soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '>',
				   "CONNECT %s:%u HTTP/1.%d",
				   uri->host, uri->port,
				   soup_message_get_http_version (msg));
	} else {
		soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '>',
				   "%s %s%s%s HTTP/1.%d",
				   msg->method, uri->path,
				   uri->query ? "?" : "",
				   uri->query ? uri->query : "",
				   soup_message_get_http_version (msg));
	}

	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '>',
			   "Soup-Debug-Timestamp: %lu",
			   (unsigned long)time (0));
	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '>',
			   "Soup-Debug: %s %u (%p), %s %u (%p), %s %u (%p)%s",
			   g_type_name_from_instance ((GTypeInstance *)session),
			   soup_logger_get_id (logger, session), session,
			   g_type_name_from_instance ((GTypeInstance *)msg),
			   soup_logger_get_id (logger, msg), msg,
			   g_type_name_from_instance ((GTypeInstance *)socket),
			   soup_logger_get_id (logger, socket), socket,
			   restarted ? ", restarted" : "");

	if (log_level == SOUP_LOGGER_LOG_MINIMAL)
		return;

	soup_logger_print (logger, SOUP_LOGGER_LOG_HEADERS, '>',
			   "Host: %s", uri->host);
	soup_message_headers_iter_init (&iter, msg->request_headers);
	while (soup_message_headers_iter_next (&iter, &name, &value)) {
		if (!g_ascii_strcasecmp (name, "Authorization") &&
		    !g_ascii_strncasecmp (value, "Basic ", 6))
			soup_logger_print_basic_auth (logger, value);
		else {
			soup_logger_print (logger, SOUP_LOGGER_LOG_HEADERS, '>',
					   "%s: %s", name, value);
		}
	}
	if (log_level == SOUP_LOGGER_LOG_HEADERS)
		return;

	if (msg->request_body->length) {
		SoupBuffer *request;

		request = soup_message_body_flatten (msg->request_body);
		soup_buffer_free (request);

		if (soup_message_headers_get_expectations (msg->request_headers) != SOUP_EXPECTATION_CONTINUE) {
			soup_logger_print (logger, SOUP_LOGGER_LOG_BODY, '>',
					   "\n%s", msg->request_body->data);
		}
	}
}

static void
print_response (SoupLogger *logger, SoupMessage *msg)
{
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);
	SoupLoggerLogLevel log_level;
	SoupMessageHeadersIter iter;
	const char *name, *value;

	if (priv->response_filter) {
		log_level = priv->response_filter (logger, msg,
						   priv->response_filter_data);
	} else
		log_level = priv->level;

	if (log_level == SOUP_LOGGER_LOG_NONE)
		return;

	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '<',
			   "HTTP/1.%d %u %s\n",
			   soup_message_get_http_version (msg),
			   msg->status_code, msg->reason_phrase);

	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '<',
			   "Soup-Debug-Timestamp: %lu",
			   (unsigned long)time (0));
	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '<',
			   "Soup-Debug: %s %u (%p)",
			   g_type_name_from_instance ((GTypeInstance *)msg),
			   soup_logger_get_id (logger, msg), msg);

	if (log_level == SOUP_LOGGER_LOG_MINIMAL)
		return;

	soup_message_headers_iter_init (&iter, msg->response_headers);
	while (soup_message_headers_iter_next (&iter, &name, &value)) {
		soup_logger_print (logger, SOUP_LOGGER_LOG_HEADERS, '<',
				   "%s: %s", name, value);
	}
	if (log_level == SOUP_LOGGER_LOG_HEADERS)
		return;

	if (msg->response_body->length) {
		soup_logger_print (logger, SOUP_LOGGER_LOG_BODY, '<',
				   "\n%s", msg->response_body->data);
	}
}

static void
got_informational (SoupMessage *msg, gpointer user_data)
{
	SoupLogger *logger = user_data;
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	g_mutex_lock (priv->lock);

	print_response (logger, msg);
	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, ' ', "");

	if (msg->status_code == SOUP_STATUS_CONTINUE && msg->request_body->data) {
		SoupLoggerLogLevel log_level;

		soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, '>',
				   "[Now sending request body...]");

		if (priv->request_filter) {
			log_level = priv->request_filter (logger, msg,
							  priv->request_filter_data);
		} else
			log_level = priv->level;

		if (log_level == SOUP_LOGGER_LOG_BODY) {
			soup_logger_print (logger, SOUP_LOGGER_LOG_BODY, '>',
					   "%s", msg->request_body->data);
		}

		soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, ' ', "");
	}

	g_mutex_unlock (priv->lock);
}

static void
got_body (SoupMessage *msg, gpointer user_data)
{
	SoupLogger *logger = user_data;
	SoupLoggerPrivate *priv = SOUP_LOGGER_GET_PRIVATE (logger);

	g_mutex_lock (priv->lock);

	print_response (logger, msg);
	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, ' ', "");

	g_mutex_unlock (priv->lock);
}

static void
finished_handler (SoupMessage *msg, gpointer user_data)
{
	SoupLogger *logger = user_data;

	g_signal_handlers_disconnect_by_func (msg, got_informational, logger);
	g_signal_handlers_disconnect_by_func (msg, got_body, logger);
	g_signal_handlers_disconnect_by_func (msg, finished_handler, logger);

	soup_logger_clear_id (logger, msg);
}

static void
request_started (SoupSession *session, SoupMessage *msg,
		 SoupSocket *socket, gpointer user_data)
{
	SoupLogger *logger = user_data;
	gboolean restarted;
	guint msg_id;

	msg_id = soup_logger_get_id (logger, msg);
	if (msg_id)
		restarted = TRUE;
	else {
		msg_id = soup_logger_set_id (logger, msg);
		restarted = FALSE;

		g_signal_connect (msg, "got-informational",
				  G_CALLBACK (got_informational),
				  logger);
		g_signal_connect (msg, "got-body",
				  G_CALLBACK (got_body),
				  logger);
		g_signal_connect (msg, "finished",
				  G_CALLBACK (finished_handler),
				  logger);
	}

	if (!soup_logger_get_id (logger, socket))
		soup_logger_set_id (logger, socket);

	print_request (logger, msg, session, socket, restarted);
	soup_logger_print (logger, SOUP_LOGGER_LOG_MINIMAL, ' ', "");
}
