/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-blob.c: a chunk of data
 *
 * Copyright (C) 2006 Novell Inc
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

#include "gsf-config.h"
#include "gsf-blob.h"

#include "gsf-utils.h"
#include "gsf-impl-utils.h"

#include <glib/gi18n-lib.h>
#include <string.h>

/* Private part of the GsfBlob structure */
struct _GsfBlobPrivate {
	gsize size;
	gpointer data;
};

static GObjectClass *gsf_blob_parent_class;
static void
gsf_blob_finalize (GObject *object)
{
	GsfBlob *blob;
	GsfBlobPrivate *priv;

	blob = GSF_BLOB (object);
	priv = blob->priv;

	g_free (priv->data);
	g_free (priv);

	gsf_blob_parent_class->finalize (object);
}

static void
gsf_blob_class_init (GObjectClass *gobject_class)
{
	gobject_class->finalize = gsf_blob_finalize;

	gsf_blob_parent_class = g_type_class_peek_parent (gobject_class);
}

static void
gsf_blob_init (GsfBlob *blob)
{
	GsfBlobPrivate *priv;

	priv = g_new0 (GsfBlobPrivate, 1);
	blob->priv = priv;
}

GSF_CLASS (GsfBlob, gsf_blob,
	   gsf_blob_class_init, gsf_blob_init,
	   G_TYPE_OBJECT);


/**
 * gsf_blob_new:
 * @size: Size of the data in bytes.
 * @data_to_copy: Data which will be copied into the blob, or %NULL if @size is zero.
 * @error: location to store error, or %NULL.
 *
 * Creates a new #GsfBlob object to hold the specified data.  The blob can then
 * be used as a facility for reference-counting for the data.  The data is
 * copied internally, so the blob does not hold references to external chunks
 * of memory.
 *
 * Return value: A newly-created #GsfBlob, or %NULL if the data could not be copied.
 *
 * Error domain: #GSF_ERROR
 *
 * Possible errors: #GSF_ERROR_OUT_OF_MEMORY if the @data_to_copy could not be copied.
 **/
GsfBlob *
gsf_blob_new (gsize size, gconstpointer data_to_copy, GError **error)
{
	GsfBlob *blob;
	GsfBlobPrivate *priv;
	gpointer data;

	g_return_val_if_fail ((size > 0 && data_to_copy != NULL) || (size == 0 && data_to_copy == NULL), NULL);
	g_return_val_if_fail (error == NULL || *error == NULL, NULL);

	if (data_to_copy) {
		data = g_try_malloc (size);
		if (!data) {
			gchar *size_str;

			size_str = g_strdup_printf ("%" G_GSIZE_FORMAT, size);
			g_set_error (error,
				     GSF_ERROR,
				     GSF_ERROR_OUT_OF_MEMORY,
				     _("Not enough memory to copy %s bytes of data"),
				     size_str);
			g_free (size_str);
			return NULL;
		}

		memcpy (data, data_to_copy, size);
	} else
		data = NULL;

	blob = g_object_new (GSF_TYPE_BLOB, NULL);
	if (G_UNLIKELY (NULL == blob)) return NULL;

	priv = blob->priv;

	priv->size = size;
	priv->data = data;

	return blob;
}

/**
 * gsf_blob_get_size:
 * @blob: A #GsfBlob.
 *
 * Queries the size in bytes of the data stored in the blob.
 *
 * Return value: Size in bytes, or 0 if the data is %NULL.
 **/
gsize
gsf_blob_get_size (GsfBlob const *blob)
{
	GsfBlobPrivate *priv;

	g_return_val_if_fail (GSF_IS_BLOB (blob), 0);

	priv = blob->priv;
	return priv->size;
}

/**
 * gsf_blob_peek_data:
 * @blob: A #GsfBlob.
 *
 * Queries a pointer to the data stored in the blob.  This does not copy the data
 * for you; it returns a pointer to the actual buffer which the blob uses internally,
 * so you should not free this buffer on your own.
 *
 * Return value: Pointer to the data stored in the blob, or %NULL if the size
 * of the data is zero.
 **/
gconstpointer
gsf_blob_peek_data (GsfBlob const *blob)
{
	GsfBlobPrivate *priv;

	g_return_val_if_fail (GSF_IS_BLOB (blob), NULL);

	priv = blob->priv;
	return priv->data;
}
