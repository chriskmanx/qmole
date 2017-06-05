/* GIO - GLib Input, Output and Streaming Library
 *
 * Copyright © 2010 Red Hat, Inc.
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

#include "config.h"
#include "gtlsoutputstream-gnutls.h"

static void g_tls_output_stream_gnutls_pollable_iface_init (GPollableOutputStreamInterface *iface);

G_DEFINE_TYPE_WITH_CODE (GTlsOutputStreamGnutls, g_tls_output_stream_gnutls, G_TYPE_OUTPUT_STREAM,
			 G_IMPLEMENT_INTERFACE (G_TYPE_POLLABLE_OUTPUT_STREAM, g_tls_output_stream_gnutls_pollable_iface_init)
			 )

struct _GTlsOutputStreamGnutlsPrivate
{
  GTlsConnectionGnutls *conn;

  /* pending operation metadata */
  GCancellable *cancellable;
  gconstpointer buffer;
  gsize count;
};

static void
g_tls_output_stream_gnutls_finalize (GObject *object)
{
  GTlsOutputStreamGnutls *stream = G_TLS_OUTPUT_STREAM_GNUTLS (object);

  if (stream->priv->conn)
    g_object_unref (stream->priv->conn);

  G_OBJECT_CLASS (g_tls_output_stream_gnutls_parent_class)->finalize (object);
}

static gssize
g_tls_output_stream_gnutls_write (GOutputStream  *stream,
				  const void     *buffer,
				  gsize           count,
				  GCancellable   *cancellable,
				  GError        **error)
{
  GTlsOutputStreamGnutls *tls_stream = G_TLS_OUTPUT_STREAM_GNUTLS (stream);

  return g_tls_connection_gnutls_write (tls_stream->priv->conn,
					buffer, count, TRUE,
					cancellable, error);
}

static gboolean
g_tls_output_stream_gnutls_write_ready (GIOStreamAdapter *adapter,
					gpointer          user_data)
{
  GTlsOutputStreamGnutls *tls_stream;
  GSimpleAsyncResult *simple = user_data;
  gssize nwrote;
  GError *error = NULL;

  tls_stream = G_TLS_OUTPUT_STREAM_GNUTLS (g_async_result_get_source_object (G_ASYNC_RESULT (simple)));
  g_object_unref (tls_stream);

  nwrote = g_tls_connection_gnutls_write (tls_stream->priv->conn,
					  tls_stream->priv->buffer,
					  tls_stream->priv->count, FALSE,
					  tls_stream->priv->cancellable,
					  &error);
  if (nwrote == -1 &&
      g_error_matches (error, G_IO_ERROR, G_IO_ERROR_WOULD_BLOCK))
    {
      g_error_free (error);
      return TRUE;
    }

  if (error)
    {
      g_simple_async_result_set_from_error (simple, error);
      g_error_free (error);
    }
  else
    g_simple_async_result_set_op_res_gssize (simple, nwrote);
  g_simple_async_result_complete (simple);
  g_object_unref (simple);

  return FALSE;
}

static void
g_tls_output_stream_gnutls_write_async (GOutputStream        *stream,
					const void           *buffer,
					gsize                 count,
					gint                  io_priority,
					GCancellable         *cancellable,
					GAsyncReadyCallback   callback,
					gpointer              user_data)
{
  GTlsOutputStreamGnutls *tls_stream = G_TLS_OUTPUT_STREAM_GNUTLS (stream);
  GSimpleAsyncResult *simple;
  gssize nwrote;
  GError *error = NULL;
  GSource *source;

  simple = g_simple_async_result_new (G_OBJECT (stream), callback, user_data,
				      g_tls_output_stream_gnutls_write_async);
  nwrote = g_tls_connection_gnutls_write (tls_stream->priv->conn,
					  buffer, count, FALSE,
					  cancellable, &error);

  if (nwrote >= 0 ||
      !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_WOULD_BLOCK))
    {
      if (error)
	{
	  g_simple_async_result_set_from_error (simple, error);
	  g_error_free (error);
	}
      else
	g_simple_async_result_set_op_res_gssize (simple, nwrote);
      g_simple_async_result_complete_in_idle (simple);
      g_object_unref (simple);
      return;
    }

  tls_stream->priv->cancellable = cancellable ? g_object_ref (cancellable) : NULL;
  tls_stream->priv->buffer = buffer;
  tls_stream->priv->count = count;

  source = g_tls_connection_gnutls_create_source (tls_stream->priv->conn,
						  G_IO_OUT,
						  tls_stream->priv->cancellable);
  g_source_set_callback (source,
			 (GSourceFunc) g_tls_output_stream_gnutls_write_ready,
			 simple, NULL);
  g_source_attach (source, g_main_context_get_thread_default ());
  g_source_unref (source);
}

static gssize
g_tls_output_stream_gnutls_write_finish (GOutputStream  *stream,
					 GAsyncResult   *result,
					 GError        **error)
{
  g_return_val_if_fail (G_IS_TLS_OUTPUT_STREAM_GNUTLS (stream), -1);
  g_return_val_if_fail (g_simple_async_result_is_valid (result, G_OBJECT (stream), g_tls_output_stream_gnutls_write_async), -1);

  return g_simple_async_result_get_op_res_gssize (G_SIMPLE_ASYNC_RESULT (result));
}

static gboolean
g_tls_output_stream_gnutls_pollable_is_writable (GPollableOutputStream *pollable)
{
  GTlsOutputStreamGnutls *tls_stream = G_TLS_OUTPUT_STREAM_GNUTLS (pollable);

  return g_tls_connection_gnutls_check (tls_stream->priv->conn, G_IO_OUT); 
}

static GSource *
g_tls_output_stream_gnutls_pollable_create_source (GPollableOutputStream *pollable,
						   GCancellable         *cancellable)
{
  GTlsOutputStreamGnutls *tls_stream = G_TLS_OUTPUT_STREAM_GNUTLS (pollable);

  return g_tls_connection_gnutls_create_source (tls_stream->priv->conn,
						G_IO_OUT,
						cancellable);
}

static gssize
g_tls_output_stream_gnutls_pollable_write_nonblocking (GPollableOutputStream  *pollable,
						       const void             *buffer,
						       gsize                   size,
						       GError                **error)
{
  GTlsOutputStreamGnutls *tls_stream = G_TLS_OUTPUT_STREAM_GNUTLS (pollable);

  return g_tls_connection_gnutls_write (tls_stream->priv->conn,
					buffer, size, FALSE,
					NULL, error);
}

static void
g_tls_output_stream_gnutls_class_init (GTlsOutputStreamGnutlsClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GOutputStreamClass *output_stream_class = G_OUTPUT_STREAM_CLASS (klass);

  g_type_class_add_private (klass, sizeof (GTlsOutputStreamGnutlsPrivate));

  gobject_class->finalize = g_tls_output_stream_gnutls_finalize;

  output_stream_class->write_fn = g_tls_output_stream_gnutls_write;
  output_stream_class->write_async = g_tls_output_stream_gnutls_write_async;
  output_stream_class->write_finish = g_tls_output_stream_gnutls_write_finish;
}

static void
g_tls_output_stream_gnutls_pollable_iface_init (GPollableOutputStreamInterface *iface)
{
  iface->is_writable = g_tls_output_stream_gnutls_pollable_is_writable;
  iface->create_source = g_tls_output_stream_gnutls_pollable_create_source;
  iface->write_nonblocking = g_tls_output_stream_gnutls_pollable_write_nonblocking;
}

static void
g_tls_output_stream_gnutls_init (GTlsOutputStreamGnutls *stream)
{
  stream->priv = G_TYPE_INSTANCE_GET_PRIVATE (stream, G_TYPE_TLS_OUTPUT_STREAM_GNUTLS, GTlsOutputStreamGnutlsPrivate);
}

GOutputStream *
g_tls_output_stream_gnutls_new (GTlsConnectionGnutls *conn)
{
  GTlsOutputStreamGnutls *tls_stream;

  tls_stream = g_object_new (G_TYPE_TLS_OUTPUT_STREAM_GNUTLS, NULL);
  tls_stream->priv->conn = g_object_ref (conn);
  return G_OUTPUT_STREAM (tls_stream);
}
