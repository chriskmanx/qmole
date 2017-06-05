/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2006, 2007, 2009, 2010 Red Hat, Inc.
 * Copyright (C) 2010 Igalia, S.L.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __SOUP_HTTP_INPUT_STREAM_H__
#define __SOUP_HTTP_INPUT_STREAM_H__

#include <gio/gio.h>
#include <libsoup/soup-types.h>

G_BEGIN_DECLS

#define SOUP_TYPE_HTTP_INPUT_STREAM         (soup_http_input_stream_get_type ())
#define SOUP_HTTP_INPUT_STREAM(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), SOUP_TYPE_HTTP_INPUT_STREAM, SoupHTTPInputStream))
#define SOUP_HTTP_INPUT_STREAM_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST ((k), SOUP_TYPE_HTTP_INPUT_STREAM, SoupHTTPInputStreamClass))
#define SOUP_IS_HTTP_INPUT_STREAM(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), SOUP_TYPE_HTTP_INPUT_STREAM))
#define SOUP_IS_HTTP_INPUT_STREAM_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), SOUP_TYPE_HTTP_INPUT_STREAM))
#define SOUP_HTTP_INPUT_STREAM_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), SOUP_TYPE_HTTP_INPUT_STREAM, SoupHTTPInputStreamClass))

typedef struct SoupHTTPInputStream SoupHTTPInputStream;
typedef struct SoupHTTPInputStreamClass SoupHTTPInputStreamClass;

struct SoupHTTPInputStream {
	GInputStream parent;
};

struct SoupHTTPInputStreamClass {
	GInputStreamClass parent_class;

	/* Padding for future expansion */
	void (*_g_reserved1)(void);
	void (*_g_reserved2)(void);
	void (*_g_reserved3)(void);
	void (*_g_reserved4)(void);
	void (*_g_reserved5)(void);
};

GType soup_http_input_stream_get_type (void) G_GNUC_CONST;

SoupHTTPInputStream *soup_http_input_stream_new         (SoupSession          *session,
							 SoupMessage          *msg);

gboolean             soup_http_input_stream_send        (SoupHTTPInputStream  *httpstream,
							 GCancellable         *cancellable,
							 GError              **error);

void                 soup_http_input_stream_send_async  (SoupHTTPInputStream  *httpstream,
							 int                   io_priority,
							 GCancellable         *cancellable,
							 GAsyncReadyCallback   callback,
							 gpointer              user_data);
gboolean             soup_http_input_stream_send_finish (SoupHTTPInputStream  *httpstream,
							 GAsyncResult         *result,
							 GError              **error);

SoupMessage         *soup_http_input_stream_get_message (SoupHTTPInputStream  *httpstream);

G_END_DECLS

#endif /* __SOUP_HTTP_INPUT_STREAM_H__ */
