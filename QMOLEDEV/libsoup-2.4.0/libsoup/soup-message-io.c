/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-message-io.c: HTTP message I/O
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "soup-connection.h"
#include "soup-message.h"
#include "soup-message-private.h"
#include "soup-misc.h"
#include "soup-socket.h"
#include "soup-ssl.h"

typedef enum {
	SOUP_MESSAGE_IO_CLIENT,
	SOUP_MESSAGE_IO_SERVER
} SoupMessageIOMode;

typedef enum {
	SOUP_MESSAGE_IO_STATE_NOT_STARTED,
	SOUP_MESSAGE_IO_STATE_HEADERS,
	SOUP_MESSAGE_IO_STATE_BLOCKING,
	SOUP_MESSAGE_IO_STATE_BODY,
	SOUP_MESSAGE_IO_STATE_CHUNK_SIZE,
	SOUP_MESSAGE_IO_STATE_CHUNK,
	SOUP_MESSAGE_IO_STATE_CHUNK_END,
	SOUP_MESSAGE_IO_STATE_TRAILERS,
	SOUP_MESSAGE_IO_STATE_FINISHING,
	SOUP_MESSAGE_IO_STATE_DONE
} SoupMessageIOState;

#define SOUP_MESSAGE_IO_STATE_ACTIVE(state) \
	(state != SOUP_MESSAGE_IO_STATE_NOT_STARTED && \
	 state != SOUP_MESSAGE_IO_STATE_BLOCKING && \
	 state != SOUP_MESSAGE_IO_STATE_DONE)

typedef struct {
	SoupSocket           *sock;
	SoupConnection       *conn;
	SoupMessageIOMode     mode;

	SoupMessageIOState    read_state;
	SoupEncoding          read_encoding;
	GByteArray           *read_meta_buf;
	SoupMessageBody      *read_body;
	guint                 read_length;

	SoupMessageIOState    write_state;
	SoupEncoding          write_encoding;
	GString              *write_buf;
	SoupMessageBody      *write_body;
	SoupBuffer           *write_chunk;
	gsize                 write_body_offset;
	guint                 written;

	guint read_tag, write_tag, err_tag;

	SoupMessageGetHeadersFn   get_headers_cb;
	SoupMessageParseHeadersFn parse_headers_cb;
	gpointer                  user_data;
} SoupMessageIOData;
	

/* Put these around callback invocation if there is code afterward
 * that depends on the IO having not been cancelled.
 */
#define dummy_to_make_emacs_happy {
#define SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK { gboolean cancelled; g_object_ref (msg);
#define SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED cancelled = (priv->io_data != io); g_object_unref (msg); if (cancelled || (!io->read_tag && !io->write_tag)) return; }
#define SOUP_MESSAGE_IO_RETURN_VAL_IF_CANCELLED_OR_PAUSED(val) cancelled = (priv->io_data != io); g_object_unref (msg); if (cancelled || (!io->read_tag && !io->write_tag)) return val; }

#define RESPONSE_BLOCK_SIZE 8192

void
soup_message_io_cleanup (SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io;

	soup_message_io_stop (msg);

	io = priv->io_data;
	if (!io)
		return;
	priv->io_data = NULL;

	if (io->sock)
		g_object_unref (io->sock);
	if (io->conn)
		g_object_unref (io->conn);

	g_byte_array_free (io->read_meta_buf, TRUE);

	g_string_free (io->write_buf, TRUE);
	if (io->write_chunk)
		soup_buffer_free (io->write_chunk);

	g_slice_free (SoupMessageIOData, io);
}

/**
 * soup_message_io_stop:
 * @msg: a #SoupMessage
 *
 * Immediately stops I/O on msg; if the connection would be left in an
 * inconsistent state, it will be closed.
 *
 * Note: this is a low-level function that does not cause any signals
 * to be emitted on @msg; it is up to the caller to make sure that
 * @msg doesn't get "stranded".
 **/
void
soup_message_io_stop (SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;

	if (!io)
		return;

	if (io->read_tag) {
		g_signal_handler_disconnect (io->sock, io->read_tag);
		io->read_tag = 0;
	}
	if (io->write_tag) {
		g_signal_handler_disconnect (io->sock, io->write_tag);
		io->write_tag = 0;
	}
	if (io->err_tag) {
		g_signal_handler_disconnect (io->sock, io->err_tag);
		io->err_tag = 0;
	}

	if (io->read_state < SOUP_MESSAGE_IO_STATE_FINISHING)
		soup_socket_disconnect (io->sock);
	else if (io->conn) {
		SoupConnection *conn = io->conn;
		io->conn = NULL;
		soup_connection_release (conn);
		g_object_unref (conn);
	}
}

#define SOUP_MESSAGE_IO_EOL            "\r\n"
#define SOUP_MESSAGE_IO_EOL_LEN        2
#define SOUP_MESSAGE_IO_DOUBLE_EOL     "\r\n\r\n"
#define SOUP_MESSAGE_IO_DOUBLE_EOL_LEN 4

static void
soup_message_io_finished (SoupMessage *msg)
{
	g_object_ref (msg);
	soup_message_io_cleanup (msg);
	if (SOUP_MESSAGE_IS_STARTING (msg))
		soup_message_restarted (msg);
	else
		soup_message_finished (msg);
	g_object_unref (msg);
}

static void io_read (SoupSocket *sock, SoupMessage *msg);

static void
io_error (SoupSocket *sock, SoupMessage *msg, GError *error)
{
	if (!SOUP_STATUS_IS_TRANSPORT_ERROR (msg->status_code)) {
		if (error && error->domain == SOUP_SSL_ERROR) {
			soup_message_set_status_full (msg,
						      SOUP_STATUS_SSL_FAILED,
						      error->message);
		} else
			soup_message_set_status (msg, SOUP_STATUS_IO_ERROR);
	}
	if (error)
		g_error_free (error);

	soup_message_io_finished (msg);
}

static void
io_disconnected (SoupSocket *sock, SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;

	/* Closing the connection to signify EOF is sometimes ok */
	if (io->read_state == SOUP_MESSAGE_IO_STATE_BODY &&
	    io->read_encoding == SOUP_ENCODING_EOF) {
		io->read_state = SOUP_MESSAGE_IO_STATE_FINISHING;
		io_read (sock, msg);
		return;
	}

	io_error (sock, msg, NULL);
}

/* Reads data from io->sock into io->read_meta_buf up until @boundary.
 * (This function is used to read metadata, and read_body_chunk() is
 * used to read the message body contents.)
 *
 * read_metadata, read_body_chunk, and write_data all use the same
 * convention for return values: if they return %TRUE, it means
 * they've completely finished the requested read/write, and the
 * caller should move on to the next step. If they return %FALSE, it
 * means that either (a) the socket returned SOUP_SOCKET_WOULD_BLOCK,
 * so the caller should give up for now and wait for the socket to
 * emit a signal, or (b) the socket returned an error, and io_error()
 * was called to process it and cancel the I/O. So either way, if the
 * function returns %FALSE, the caller should return immediately.
 */
static gboolean
read_metadata (SoupMessage *msg, const char *boundary)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;
	SoupSocketIOStatus status;
	guchar read_buf[RESPONSE_BLOCK_SIZE];
	guint boundary_len = strlen (boundary);
	gsize nread;
	gboolean done;
	GError *error = NULL;

	do {
		status = soup_socket_read_until (io->sock, read_buf,
						 sizeof (read_buf),
						 boundary, boundary_len,
						 &nread, &done, NULL, &error);
		switch (status) {
		case SOUP_SOCKET_OK:
			g_byte_array_append (io->read_meta_buf, read_buf, nread);
			break;

		case SOUP_SOCKET_ERROR:
		case SOUP_SOCKET_EOF:
			io_error (io->sock, msg, error);
			return FALSE;

		case SOUP_SOCKET_WOULD_BLOCK:
			return FALSE;
		}
	} while (!done);

	return TRUE;
}

/* Reads as much message body data as is available on io->sock (but no
 * further than the end of the current message body or chunk). On a
 * successful read, emits "got_chunk" (possibly multiple times), and
 * if %SOUP_MESSAGE_OVERWRITE_CHUNKS wasn't set, appends the chunk
 * to io->read_body.
 *
 * See the note at read_metadata() for an explanation of the return
 * value.
 */
static gboolean
read_body_chunk (SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;
	SoupSocketIOStatus status;
	guchar *stack_buf = NULL;
	gsize len;
	gboolean read_to_eof = (io->read_encoding == SOUP_ENCODING_EOF);
	gsize nread;
	GError *error = NULL;
	SoupBuffer *buffer;

	while (read_to_eof || io->read_length > 0) {
		if (priv->chunk_allocator) {
			buffer = priv->chunk_allocator (msg, io->read_length, priv->chunk_allocator_data);
			if (!buffer) {
				soup_message_io_pause (msg);
				return FALSE;
			}
		} else {
			if (!stack_buf)
				stack_buf = alloca (RESPONSE_BLOCK_SIZE);
			buffer = soup_buffer_new (SOUP_MEMORY_TEMPORARY,
						  stack_buf,
						  RESPONSE_BLOCK_SIZE);
		}

		if (read_to_eof)
			len = buffer->length;
		else
			len = MIN (buffer->length, io->read_length);

		status = soup_socket_read (io->sock,
					   (guchar *)buffer->data, len,
					   &nread, NULL, &error);

		if (status == SOUP_SOCKET_OK && nread) {
			buffer->length = nread;
			if (!(priv->msg_flags & SOUP_MESSAGE_OVERWRITE_CHUNKS))
				soup_message_body_append_buffer (io->read_body, buffer);

			io->read_length -= nread;

			SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
			soup_message_got_chunk (msg, buffer);
			soup_buffer_free (buffer);
			SOUP_MESSAGE_IO_RETURN_VAL_IF_CANCELLED_OR_PAUSED (FALSE);
			continue;
		}

		soup_buffer_free (buffer);
		switch (status) {
		case SOUP_SOCKET_OK:
			break;

		case SOUP_SOCKET_EOF:
			if (read_to_eof)
				return TRUE;
			/* else fall through */

		case SOUP_SOCKET_ERROR:
			io_error (io->sock, msg, error);
			return FALSE;

		case SOUP_SOCKET_WOULD_BLOCK:
			return FALSE;
		}
	}

	return TRUE;
}

/* Attempts to write @len bytes from @data. See the note at
 * read_metadata() for an explanation of the return value.
 */
static gboolean
write_data (SoupMessage *msg, const char *data, guint len)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;
	SoupSocketIOStatus status;
	gsize nwrote;
	GError *error = NULL;

	while (len > io->written) {
		status = soup_socket_write (io->sock,
					    data + io->written,
					    len - io->written,
					    &nwrote, NULL, &error);
		switch (status) {
		case SOUP_SOCKET_EOF:
		case SOUP_SOCKET_ERROR:
			io_error (io->sock, msg, error);
			return FALSE;

		case SOUP_SOCKET_WOULD_BLOCK:
			return FALSE;

		case SOUP_SOCKET_OK:
			io->written += nwrote;
			break;
		}
	}

	io->written = 0;
	return TRUE;
}

static inline SoupMessageIOState
io_body_state (SoupEncoding encoding)
{
	if (encoding == SOUP_ENCODING_CHUNKED)
		return SOUP_MESSAGE_IO_STATE_CHUNK_SIZE;
	else
		return SOUP_MESSAGE_IO_STATE_BODY;
}

/*
 * There are two request/response formats: the basic request/response,
 * possibly with one or more unsolicited informational responses (such
 * as the WebDAV "102 Processing" response):
 *
 *     Client                            Server
 *      W:HEADERS  / R:NOT_STARTED    ->  R:HEADERS  / W:NOT_STARTED
 *      W:BODY     / R:NOT_STARTED    ->  R:BODY     / W:NOT_STARTED
 *     [W:DONE     / R:HEADERS (1xx)  <-  R:DONE     / W:HEADERS (1xx) ...]
 *      W:DONE     / R:HEADERS        <-  R:DONE     / W:HEADERS
 *      W:DONE     / R:BODY           <-  R:DONE     / W:BODY
 *      W:DONE     / R:DONE               R:DONE     / W:DONE
 *     
 * and the "Expect: 100-continue" request/response, with the client
 * blocking halfway through its request, and then either continuing or
 * aborting, depending on the server response:
 *
 *     Client                            Server
 *      W:HEADERS  / R:NOT_STARTED    ->  R:HEADERS  / W:NOT_STARTED
 *      W:BLOCKING / R:HEADERS        <-  R:BLOCKING / W:HEADERS
 *     [W:BODY     / R:BLOCKING       ->  R:BODY     / W:BLOCKING]
 *     [W:DONE     / R:HEADERS        <-  R:DONE     / W:HEADERS]
 *      W:DONE     / R:BODY           <-  R:DONE     / W:BODY
 *      W:DONE     / R:DONE               R:DONE     / W:DONE
 */

static void
io_write (SoupSocket *sock, SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;

 write_more:
	switch (io->write_state) {
	case SOUP_MESSAGE_IO_STATE_NOT_STARTED:
		return;


	case SOUP_MESSAGE_IO_STATE_HEADERS:
		if (!io->write_buf->len) {
			io->get_headers_cb (msg, io->write_buf,
					    &io->write_encoding,
					    io->user_data);
			if (!io->write_buf->len) {
				soup_message_io_pause (msg);
				return;
			}
		}

		if (!write_data (msg, io->write_buf->str, io->write_buf->len))
			return;

		g_string_truncate (io->write_buf, 0);

		if (io->mode == SOUP_MESSAGE_IO_SERVER &&
		    SOUP_STATUS_IS_INFORMATIONAL (msg->status_code)) {
			if (msg->status_code == SOUP_STATUS_CONTINUE) {
				/* Stop and wait for the body now */
				io->write_state =
					SOUP_MESSAGE_IO_STATE_BLOCKING;
				io->read_state = io_body_state (io->read_encoding);
			} else {
				/* We just wrote a 1xx response
				 * header, so stay in STATE_HEADERS.
				 * (The caller will pause us from the
				 * wrote_informational callback if he
				 * is not ready to send the final
				 * response.)
				 */
			}
		} else if (io->mode == SOUP_MESSAGE_IO_CLIENT &&
			   soup_message_headers_get_expectations (msg->request_headers) & SOUP_EXPECTATION_CONTINUE) {
			/* Need to wait for the Continue response */
			io->write_state = SOUP_MESSAGE_IO_STATE_BLOCKING;
			io->read_state = SOUP_MESSAGE_IO_STATE_HEADERS;
		} else {
			io->write_state = io_body_state (io->write_encoding);

			/* If the client was waiting for a Continue
			 * but we sent something else, then they're
			 * now done writing.
			 */
			if (io->mode == SOUP_MESSAGE_IO_SERVER &&
			    io->read_state == SOUP_MESSAGE_IO_STATE_BLOCKING)
				io->read_state = SOUP_MESSAGE_IO_STATE_FINISHING;
		}

		SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
		if (SOUP_STATUS_IS_INFORMATIONAL (msg->status_code)) {
			soup_message_wrote_informational (msg);
			soup_message_cleanup_response (msg);
		} else
			soup_message_wrote_headers (msg);
		SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;
		break;


	case SOUP_MESSAGE_IO_STATE_BLOCKING:
		io_read (sock, msg);

		/* If io_read reached a point where we could write
		 * again, it would have recursively called io_write.
		 * So (a) we don't need to try to keep writing, and
		 * (b) we can't anyway, because msg may have been
		 * destroyed.
		 */
		return;


	case SOUP_MESSAGE_IO_STATE_BODY:
		if (!io->write_chunk)
			io->write_chunk = soup_message_body_flatten (io->write_body);
		if (!write_data (msg, io->write_chunk->data,
				 io->write_chunk->length))
			return;
		soup_buffer_free (io->write_chunk);
		io->write_chunk = NULL;

		io->write_state = SOUP_MESSAGE_IO_STATE_FINISHING;

		SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
		soup_message_wrote_body (msg);
		SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;
		break;


	case SOUP_MESSAGE_IO_STATE_CHUNK_SIZE:
		if (!io->write_chunk) {
			io->write_chunk = soup_message_body_get_chunk (io->write_body, io->write_body_offset);
			if (!io->write_chunk) {
				soup_message_io_pause (msg);
				return;
			}
			g_string_append_printf (io->write_buf, "%lx\r\n",
						(unsigned long) io->write_chunk->length);
			io->write_body_offset += io->write_chunk->length;
		}

		if (!write_data (msg, io->write_buf->str, io->write_buf->len))
			return;

		g_string_truncate (io->write_buf, 0);

		if (io->write_chunk->length == 0) {
			/* The last chunk has no CHUNK_END... */
			io->write_state = SOUP_MESSAGE_IO_STATE_TRAILERS;
			break;
		}

		io->write_state = SOUP_MESSAGE_IO_STATE_CHUNK;
		/* fall through */


	case SOUP_MESSAGE_IO_STATE_CHUNK:
		if (!write_data (msg, io->write_chunk->data,
				 io->write_chunk->length))
			return;

		soup_buffer_free (io->write_chunk);
		io->write_chunk = NULL;

		io->write_state = SOUP_MESSAGE_IO_STATE_CHUNK_END;

		SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
		soup_message_wrote_chunk (msg);
		SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;

		/* fall through */


	case SOUP_MESSAGE_IO_STATE_CHUNK_END:
		if (!write_data (msg, SOUP_MESSAGE_IO_EOL,
				 SOUP_MESSAGE_IO_EOL_LEN))
			return;

		io->write_state = SOUP_MESSAGE_IO_STATE_CHUNK_SIZE;
		break;


	case SOUP_MESSAGE_IO_STATE_TRAILERS:
		if (!write_data (msg, SOUP_MESSAGE_IO_EOL,
				 SOUP_MESSAGE_IO_EOL_LEN))
			return;

		io->write_state = SOUP_MESSAGE_IO_STATE_FINISHING;

		SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
		soup_message_wrote_body (msg);
		SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;
		/* fall through */


	case SOUP_MESSAGE_IO_STATE_FINISHING:
		if (io->write_tag) {
			g_signal_handler_disconnect (io->sock, io->write_tag);
			io->write_tag = 0;
		}
		io->write_state = SOUP_MESSAGE_IO_STATE_DONE;

		if (io->mode == SOUP_MESSAGE_IO_CLIENT) {
			io->read_state = SOUP_MESSAGE_IO_STATE_HEADERS;
			io_read (sock, msg);
		} else
			soup_message_io_finished (msg);
		return;


	case SOUP_MESSAGE_IO_STATE_DONE:
	default:
		g_return_if_reached ();
	}

	goto write_more;
}

static void
io_read (SoupSocket *sock, SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;
	guint status;

 read_more:
	switch (io->read_state) {
	case SOUP_MESSAGE_IO_STATE_NOT_STARTED:
		return;


	case SOUP_MESSAGE_IO_STATE_HEADERS:
		if (!read_metadata (msg, SOUP_MESSAGE_IO_DOUBLE_EOL))
			return;

		io->read_meta_buf->len -= SOUP_MESSAGE_IO_EOL_LEN;
		io->read_meta_buf->data[io->read_meta_buf->len] = '\0';
		status = io->parse_headers_cb (msg, (char *)io->read_meta_buf->data,
					       io->read_meta_buf->len,
					       &io->read_encoding,
					       io->user_data);
		g_byte_array_set_size (io->read_meta_buf, 0);

		if (status != SOUP_STATUS_OK) {
			/* Either we couldn't parse the headers, or they
			 * indicated something that would mean we wouldn't
			 * be able to parse the body. (Eg, unknown
			 * Transfer-Encoding.). Skip the rest of the
			 * reading, and make sure the connection gets
			 * closed when we're done.
			 */
			soup_message_set_status (msg, status);
			soup_message_headers_append (msg->request_headers,
						     "Connection", "close");
			io->read_state = SOUP_MESSAGE_IO_STATE_FINISHING;
			break;
		}

		if (io->read_encoding == SOUP_ENCODING_CONTENT_LENGTH) {
			SoupMessageHeaders *hdrs =
				(io->mode == SOUP_MESSAGE_IO_CLIENT) ?
				msg->response_headers : msg->request_headers;
			io->read_length = soup_message_headers_get_content_length (hdrs);
		}

		if (io->mode == SOUP_MESSAGE_IO_CLIENT &&
		    SOUP_STATUS_IS_INFORMATIONAL (msg->status_code)) {
			if (msg->status_code == SOUP_STATUS_CONTINUE &&
			    io->write_state == SOUP_MESSAGE_IO_STATE_BLOCKING) {
				/* Pause the reader, unpause the writer */
				io->read_state =
					SOUP_MESSAGE_IO_STATE_BLOCKING;
				io->write_state =
					io_body_state (io->write_encoding);
			} else {
				/* Just stay in HEADERS */
				io->read_state = SOUP_MESSAGE_IO_STATE_HEADERS;
			}
		} else if (io->mode == SOUP_MESSAGE_IO_SERVER &&
			   soup_message_headers_get_expectations (msg->request_headers) & SOUP_EXPECTATION_CONTINUE) {
			/* The client requested a Continue response. The
			 * got_headers handler may change this to something
			 * else though.
			 */
			soup_message_set_status (msg, SOUP_STATUS_CONTINUE);
			io->write_state = SOUP_MESSAGE_IO_STATE_HEADERS;
			io->read_state = SOUP_MESSAGE_IO_STATE_BLOCKING;
		} else {
			io->read_state = io_body_state (io->read_encoding);

			/* If the client was waiting for a Continue
			 * but got something else, then it's done
			 * writing.
			 */
			if (io->mode == SOUP_MESSAGE_IO_CLIENT &&
			    io->write_state == SOUP_MESSAGE_IO_STATE_BLOCKING)
				io->write_state = SOUP_MESSAGE_IO_STATE_FINISHING;
		}

		if (io->mode == SOUP_MESSAGE_IO_CLIENT &&
		    SOUP_STATUS_IS_INFORMATIONAL (msg->status_code)) {
			SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
			soup_message_got_informational (msg);
			soup_message_cleanup_response (msg);
			SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;
		} else {
			SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
			soup_message_got_headers (msg);
			SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;
		}
		break;


	case SOUP_MESSAGE_IO_STATE_BLOCKING:
		io_write (sock, msg);

		/* As in the io_write case, we *must* return here. */
		return;


	case SOUP_MESSAGE_IO_STATE_BODY:
		if (!read_body_chunk (msg))
			return;

	got_body:
		io->read_state = SOUP_MESSAGE_IO_STATE_FINISHING;

		SOUP_MESSAGE_IO_PREPARE_FOR_CALLBACK;
		soup_message_got_body (msg);
		SOUP_MESSAGE_IO_RETURN_IF_CANCELLED_OR_PAUSED;
		break;


	case SOUP_MESSAGE_IO_STATE_CHUNK_SIZE:
		if (!read_metadata (msg, SOUP_MESSAGE_IO_EOL))
			return;

		io->read_length = strtoul ((char *)io->read_meta_buf->data, NULL, 16);
		g_byte_array_set_size (io->read_meta_buf, 0);

		if (io->read_length > 0)
			io->read_state = SOUP_MESSAGE_IO_STATE_CHUNK;
		else
			io->read_state = SOUP_MESSAGE_IO_STATE_TRAILERS;
		break;


	case SOUP_MESSAGE_IO_STATE_CHUNK:
		if (!read_body_chunk (msg))
			return;

		io->read_state = SOUP_MESSAGE_IO_STATE_CHUNK_END;
		break;


	case SOUP_MESSAGE_IO_STATE_CHUNK_END:
		if (!read_metadata (msg, SOUP_MESSAGE_IO_EOL))
			return;

		g_byte_array_set_size (io->read_meta_buf, 0);
		io->read_state = SOUP_MESSAGE_IO_STATE_CHUNK_SIZE;
		break;


	case SOUP_MESSAGE_IO_STATE_TRAILERS:
		if (!read_metadata (msg, SOUP_MESSAGE_IO_EOL))
			return;

		if (io->read_meta_buf->len == SOUP_MESSAGE_IO_EOL_LEN)
			goto got_body;

		/* FIXME: process trailers */
		g_byte_array_set_size (io->read_meta_buf, 0);
		break;


	case SOUP_MESSAGE_IO_STATE_FINISHING:
		if (io->read_tag) {
			g_signal_handler_disconnect (io->sock, io->read_tag);
			io->read_tag = 0;
		}
		io->read_state = SOUP_MESSAGE_IO_STATE_DONE;

		if (io->mode == SOUP_MESSAGE_IO_SERVER) {
			io->write_state = SOUP_MESSAGE_IO_STATE_HEADERS;
			io_write (sock, msg);
		} else
			soup_message_io_finished (msg);
		return;


	case SOUP_MESSAGE_IO_STATE_DONE:
	default:
		g_return_if_reached ();
	}

	goto read_more;
}

static SoupMessageIOData *
new_iostate (SoupMessage *msg, SoupSocket *sock, SoupMessageIOMode mode,
	     SoupMessageGetHeadersFn get_headers_cb,
	     SoupMessageParseHeadersFn parse_headers_cb,
	     gpointer user_data)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io;

	io = g_slice_new0 (SoupMessageIOData);
	io->sock = g_object_ref (sock);
	io->mode = mode;
	io->get_headers_cb   = get_headers_cb;
	io->parse_headers_cb = parse_headers_cb;
	io->user_data        = user_data;

	io->read_meta_buf    = g_byte_array_new ();
	io->write_buf        = g_string_new (NULL);

	io->read_tag  = g_signal_connect (io->sock, "readable",
					  G_CALLBACK (io_read), msg);
	io->write_tag = g_signal_connect (io->sock, "writable",
					  G_CALLBACK (io_write), msg);
	io->err_tag   = g_signal_connect (io->sock, "disconnected",
					  G_CALLBACK (io_disconnected), msg);

	io->read_state  = SOUP_MESSAGE_IO_STATE_NOT_STARTED;
	io->write_state = SOUP_MESSAGE_IO_STATE_NOT_STARTED;

	if (priv->io_data)
		soup_message_io_cleanup (msg);
	priv->io_data = io;
	return io;
}

void
soup_message_io_client (SoupMessage *msg, SoupSocket *sock,
			SoupConnection *conn,
			SoupMessageGetHeadersFn get_headers_cb,
			SoupMessageParseHeadersFn parse_headers_cb,
			gpointer user_data)
{
	SoupMessageIOData *io;

	io = new_iostate (msg, sock, SOUP_MESSAGE_IO_CLIENT,
			  get_headers_cb, parse_headers_cb, user_data);

	if (conn)
		io->conn = g_object_ref (conn);

	io->read_body       = msg->response_body;
	io->write_body      = msg->request_body;

	io->write_state     = SOUP_MESSAGE_IO_STATE_HEADERS;
	io_write (sock, msg);
}

void
soup_message_io_server (SoupMessage *msg, SoupSocket *sock,
			SoupMessageGetHeadersFn get_headers_cb,
			SoupMessageParseHeadersFn parse_headers_cb,
			gpointer user_data)
{
	SoupMessageIOData *io;

	io = new_iostate (msg, sock, SOUP_MESSAGE_IO_SERVER,
			  get_headers_cb, parse_headers_cb, user_data);

	io->read_body       = msg->request_body;
	io->write_body      = msg->response_body;

	io->read_state      = SOUP_MESSAGE_IO_STATE_HEADERS;
	io_read (sock, msg);
}

void  
soup_message_io_pause (SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;

	g_return_if_fail (io != NULL);

	if (io->write_tag) {
		g_signal_handler_disconnect (io->sock, io->write_tag);
		io->write_tag = 0;
	}
	if (io->read_tag) {
		g_signal_handler_disconnect (io->sock, io->read_tag);
		io->read_tag = 0;
	}
}

static gboolean
io_unpause_internal (gpointer msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;

	g_return_val_if_fail (io != NULL, FALSE);

	if (io->write_tag || io->read_tag)
		return FALSE;

	if (io->write_state != SOUP_MESSAGE_IO_STATE_DONE) {
		io->write_tag = g_signal_connect (io->sock, "writable",
						  G_CALLBACK (io_write), msg);
	}

	if (io->read_state != SOUP_MESSAGE_IO_STATE_DONE) {
		io->read_tag = g_signal_connect (io->sock, "readable",
						 G_CALLBACK (io_read), msg);
	}

	if (SOUP_MESSAGE_IO_STATE_ACTIVE (io->write_state))
		io_write (io->sock, msg);
	else if (SOUP_MESSAGE_IO_STATE_ACTIVE (io->read_state))
		io_read (io->sock, msg);

	return FALSE;
}

void
soup_message_io_unpause (SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);
	SoupMessageIOData *io = priv->io_data;
	gboolean non_blocking;
	GMainContext *async_context;

	g_return_if_fail (io != NULL);

	g_object_get (io->sock,
		      SOUP_SOCKET_FLAG_NONBLOCKING, &non_blocking,
		      SOUP_SOCKET_ASYNC_CONTEXT, &async_context,
		      NULL);
	if (non_blocking)
		soup_add_idle (async_context, io_unpause_internal, msg);
	else
		io_unpause_internal (msg);
	if (async_context)
		g_main_context_unref (async_context);
}

/**
 * soup_message_io_in_progress:
 * @msg: a #SoupMessage
 *
 * Tests whether or not I/O is currently in progress on @msg.
 *
 * Return value: whether or not I/O is currently in progress.
 **/
gboolean
soup_message_io_in_progress (SoupMessage *msg)
{
	SoupMessagePrivate *priv = SOUP_MESSAGE_GET_PRIVATE (msg);

	return priv->io_data != NULL;
}
