/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifndef SOUP_MESSAGE_PRIVATE_H
#define SOUP_MESSAGE_PRIVATE_H 1

#include "soup-message.h"
#include "soup-auth.h"
#include "soup-connection.h"

typedef enum {
	SOUP_MESSAGE_IO_STATUS_IDLE,
	SOUP_MESSAGE_IO_STATUS_QUEUED,
        SOUP_MESSAGE_IO_STATUS_CONNECTING,
        SOUP_MESSAGE_IO_STATUS_RUNNING,
	SOUP_MESSAGE_IO_STATUS_FINISHED
} SoupMessageIOStatus;

typedef struct {
	gpointer           io_data;
	SoupMessageIOStatus io_status;

	SoupChunkAllocator chunk_allocator;
	gpointer           chunk_allocator_data;
	GDestroyNotify     chunk_allocator_dnotify;

	guint              msg_flags;

	SoupHTTPVersion    http_version;

	SoupURI           *uri;

	SoupAuth          *auth, *proxy_auth;
} SoupMessagePrivate;
#define SOUP_MESSAGE_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_MESSAGE, SoupMessagePrivate))

#define SOUP_MESSAGE_IS_STARTING(msg) (SOUP_MESSAGE_GET_PRIVATE (msg)->io_status == SOUP_MESSAGE_IO_STATUS_QUEUED || SOUP_MESSAGE_GET_PRIVATE (msg)->io_status == SOUP_MESSAGE_IO_STATUS_CONNECTING)

void             soup_message_cleanup_response (SoupMessage      *req);


typedef void     (*SoupMessageGetHeadersFn)  (SoupMessage      *msg,
					      GString          *headers,
					      SoupEncoding     *encoding,
					      gpointer          user_data);
typedef guint    (*SoupMessageParseHeadersFn)(SoupMessage      *msg,
					      char             *headers,
					      guint             header_len,
					      SoupEncoding     *encoding,
					      gpointer          user_data);

void           soup_message_send_request        (SoupMessage       *req,
						 SoupSocket        *sock,
						 SoupConnection    *conn,
						 gboolean           via_proxy);
void           soup_message_read_request        (SoupMessage       *req,
						 SoupSocket        *sock);

void soup_message_io_client  (SoupMessage               *msg,
			      SoupSocket                *sock,
			      SoupConnection            *conn,
			      SoupMessageGetHeadersFn    get_headers_cb,
			      SoupMessageParseHeadersFn  parse_headers_cb,
			      gpointer                   user_data);
void soup_message_io_server  (SoupMessage               *msg,
			      SoupSocket                *sock,
			      SoupMessageGetHeadersFn    get_headers_cb,
			      SoupMessageParseHeadersFn  parse_headers_cb,
			      gpointer                   user_data);
void soup_message_io_cleanup (SoupMessage               *msg);

/* Auth handling */
void           soup_message_set_auth       (SoupMessage *msg,
					    SoupAuth    *auth);
SoupAuth      *soup_message_get_auth       (SoupMessage *msg);
void           soup_message_set_proxy_auth (SoupMessage *msg,
					    SoupAuth    *auth);
SoupAuth      *soup_message_get_proxy_auth (SoupMessage *msg);

/* I/O */
void                soup_message_set_io_status  (SoupMessage          *msg,
						 SoupMessageIOStatus  status);
SoupMessageIOStatus soup_message_get_io_status  (SoupMessage          *msg);
void                soup_message_io_stop        (SoupMessage          *msg);
void                soup_message_io_pause       (SoupMessage          *msg);
void                soup_message_io_unpause     (SoupMessage          *msg);
gboolean            soup_message_io_in_progress (SoupMessage          *msg);

#endif /* SOUP_MESSAGE_PRIVATE_H */
