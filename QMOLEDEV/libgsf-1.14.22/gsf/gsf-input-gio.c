/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-gio.c:
 *
 * Copyright (C) 2007 Dom Lachowicz <cinamod@hotmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-input-gio.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-output-memory.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

struct _GsfInputGio {
	GsfInput     input;
	GFile        *file;
	GInputStream *stream;
	guint8       *buf;
	size_t       buf_size;
};

typedef struct {
	GsfInputClass input_class;
} GsfInputGioClass;

static gboolean
can_seek (GInputStream *stream)
{
	if (!G_IS_SEEKABLE (stream))
		return FALSE;

	return g_seekable_can_seek (G_SEEKABLE (stream));
}

static void
set_name_from_file (GsfInput *input, GFile *file)
{
	GFileInfo *info = g_file_query_info
		(file, G_FILE_ATTRIBUTE_STANDARD_NAME, 0, NULL, NULL);
	if (info) {
		gsf_input_set_name (input, g_file_info_get_name (info));
		g_object_unref (info);
	}
}

static GsfInput *
make_local_copy (GFile *file, GInputStream *stream)
{
	GsfOutput *out;
	GsfInput  *copy;
	GFileInfo *info;

	out = gsf_output_memory_new ();

	while (1) {
		guint8 buf[4096];
		gssize nread;

		nread = g_input_stream_read (stream, buf, sizeof(buf), NULL, NULL);

		if (nread > 0) {
			if (!gsf_output_write (out, nread, buf)) {
				copy = NULL;
				goto cleanup_and_exit;
			}
		}
		else if (nread == 0)
			break;
		else {
			copy = NULL;
			goto cleanup_and_exit;
		}
	}

	copy = gsf_input_memory_new_clone
		(gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (out)),
		 gsf_output_size (out));

	if (copy != NULL) {
		info = g_file_query_info (file, G_FILE_ATTRIBUTE_STANDARD_NAME, 0, NULL, NULL);
		if (info) {
			gsf_input_set_name (GSF_INPUT (copy), g_file_info_get_name (info));
			g_object_unref (info);
		}
	}

 cleanup_and_exit:

	gsf_output_close (out);
	g_object_unref (out);

	g_input_stream_close (stream, NULL, NULL);
	g_object_unref (stream);

	set_name_from_file (copy, file);

	return copy;
}

/**
 * gsf_input_gio_new:
 * @file:
 * @err: optionally NULL.
 *
 * Returns: A new #GsfInputGio or NULL
 */
GsfInput *
gsf_input_gio_new (GFile *file, GError **err)
{
	GsfInputGio *input;
	GInputStream *stream;
	gsf_off_t filesize;

	g_return_val_if_fail (file != NULL, NULL);

	stream = (GInputStream *)g_file_read (file, NULL, err);
	if (stream == NULL)
		return NULL;

	if (!can_seek (stream))
		return make_local_copy (file, stream);

	{
		GFileInfo *info =
			g_file_query_info (file,
					   G_FILE_ATTRIBUTE_STANDARD_SIZE,
					   0, NULL, NULL);
		if (!info)
			return make_local_copy (file, stream);
		filesize = g_file_info_get_size (info);
		g_object_unref (info);
	}

	input = g_object_new (GSF_INPUT_GIO_TYPE, NULL);
	if (G_UNLIKELY (NULL == input)) {
		g_input_stream_close (stream, NULL, NULL);
		g_object_unref (stream);
		return NULL;
	}

	gsf_input_set_size (GSF_INPUT (input), filesize);

	g_object_ref (file);

	input->stream = stream;
	input->file = file;
	input->buf  = NULL;
	input->buf_size = 0;

	set_name_from_file (GSF_INPUT (input), file);
	return GSF_INPUT (input);
}

/**
 * gsf_input_gio_new_for_path:
 * @path:
 * @err: optionally NULL.
 *
 * Returns: A new #GsfInputGio or NULL
 */
GsfInput *
gsf_input_gio_new_for_path (char const *path, GError **err)
{
	GFile *file;
	GsfInput *input;

	g_return_val_if_fail (path != NULL, NULL);

	file = g_file_new_for_path (path);
	input = gsf_input_gio_new (file, err);
	g_object_unref (file);

	return input;
}

/**
 * gsf_input_gio_new_for_uri:
 * @uri:
 * @err: optionally NULL.
 *
 * Returns: A new #GsfInputGio or NULL
 */
GsfInput *
gsf_input_gio_new_for_uri (char const *uri, GError **err)
{
	GFile *file;
	GsfInput *input;

	g_return_val_if_fail (uri != NULL, NULL);

	file = g_file_new_for_uri (uri);
	input = gsf_input_gio_new (file, err);
	g_object_unref (file);

	return input;
}

static void
gsf_input_gio_finalize (GObject *obj)
{
	GObjectClass *parent_class;
	GsfInputGio *input = (GsfInputGio *)obj;

	g_input_stream_close (input->stream, NULL, NULL);
	g_object_unref (input->stream);
	input->stream = NULL;

	g_object_unref (input->file);
	input->file = NULL;

	if (input->buf != NULL) {
		g_free (input->buf);
		input->buf  = NULL;
		input->buf_size = 0;
	}

	parent_class = g_type_class_peek (GSF_INPUT_TYPE);
	if (parent_class && parent_class->finalize)
		parent_class->finalize (obj);
}

static GsfInput *
gsf_input_gio_dup (GsfInput *src_input, GError **err)
{
	GsfInputGio *src = (GsfInputGio *)src_input;
	GFile *clone;

	g_return_val_if_fail (src_input != NULL, NULL);
	g_return_val_if_fail (src->file != NULL, NULL);

	clone = g_file_dup (src->file);
	if (clone != NULL) {
		GsfInput *dst = gsf_input_gio_new (clone, err);

		/*
		 * gsf_input_gio_new() adds a ref, or fails to create a new
		 * file.  in any case, we need to unref the clone
		 */
	        g_object_unref (clone);

		return dst;
	}

	return NULL;
}

static guint8 const *
gsf_input_gio_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	GsfInputGio *gio = GSF_INPUT_GIO (input);
	size_t total_read = 0;

	g_return_val_if_fail (gio != NULL, NULL);
	g_return_val_if_fail (gio->stream != NULL, NULL);

	if (buffer == NULL) {
		if (gio->buf_size < num_bytes) {
			gio->buf_size = num_bytes;
			g_free (gio->buf);
			gio->buf = g_new (guint8, gio->buf_size);
		}
		buffer = gio->buf;
	}

	while (total_read < num_bytes) {
		gssize try_to_read = MIN (G_MAXSSIZE, num_bytes - total_read);
		gssize nread = g_input_stream_read (gio->stream,
						    buffer + total_read,
						    try_to_read,
						    NULL, NULL);

		if (nread > 0) {
			total_read += nread;
		} else {
			/*
			 * Getting zero means EOF which isn't supposed to
			 * happen.   Negative means error.
			 */
			return NULL;
		}
	}

	return buffer;
}

static gboolean
gsf_input_gio_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputGio *gio = GSF_INPUT_GIO (input);

	g_return_val_if_fail (gio != NULL, TRUE);
	g_return_val_if_fail (gio->stream != NULL, TRUE);
	g_return_val_if_fail (can_seek (gio->stream), TRUE);

	return (g_seekable_seek (G_SEEKABLE (gio->stream), offset, whence, NULL, NULL) ? FALSE : TRUE);
}

static void
gsf_input_gio_init (GObject *obj)
{
	GsfInputGio *gio = GSF_INPUT_GIO (obj);

	gio->file = NULL;
	gio->stream = NULL;
	gio->buf  = NULL;
	gio->buf_size = 0;
}

static void
gsf_input_gio_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_gio_finalize;
	input_class->Dup	= gsf_input_gio_dup;
	input_class->Read	= gsf_input_gio_read;
	input_class->Seek	= gsf_input_gio_seek;
}

GSF_CLASS (GsfInputGio, gsf_input_gio,
	   gsf_input_gio_class_init, gsf_input_gio_init, GSF_INPUT_TYPE)

/***************************************************************************/
/***************************************************************************/
