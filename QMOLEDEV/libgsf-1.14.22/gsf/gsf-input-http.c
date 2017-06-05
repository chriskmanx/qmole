/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-http.c: retrieves input via HTTP
 *
 * Copyright (C) 2006 Michael Lawrence (lawremi@iastate.edu)
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
#include <gsf/gsf-input-http.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <libxml/nanohttp.h>

struct _GsfInputHTTP {
        GsfInput input;
        /*< private > */
        gchar *url;
        gchar *content_type;
        gpointer ctx;
        guint8 *buf;
        gsize buf_size;
};

typedef struct {
        GsfInputClass input;
} GsfInputHTTPClass;

static void gsf_input_http_set_property (GObject *object, guint property_id,
                                         GValue const *value,
                                         GParamSpec *pspec);
static void gsf_input_http_get_property (GObject *object, guint property_id,
                                         GValue *value, GParamSpec *pspec);
static void gsf_input_http_init (GObject *obj);
static void gsf_input_http_class_init (GsfInputHTTPClass *c);
static GsfInput *gsf_input_http_dup (GsfInput *src, GError **err);
static guint8 const *gsf_input_http_read (GsfInput *input, size_t num_bytes,
                                          guint8 *buffer);
static gboolean gsf_input_http_seek (GsfInput *input, gsf_off_t offset,
                                     GSeekType whence);

enum
{
        PROP_0,
        PROP_URL,
        PROP_CONTENT_TYPE
};

/* pointer to the class of our parent */
static GsfInputClass *parent_class = NULL;

static void
gsf_input_http_finalize (GObject *obj_input)
{
        GsfInputHTTP *input = GSF_INPUT_HTTP (obj_input);

	g_free (input->url);
	input->url = NULL;

	g_free (input->content_type);
	input->content_type = NULL;

        if (input->ctx) {
                xmlNanoHTTPClose ((gpointer) input->ctx);
                input->ctx = NULL;
        }

	g_free (input->buf);
	input->buf = NULL;

        ((GObjectClass *)parent_class)->finalize (obj_input);
}

static void
gsf_input_http_init (GObject *obj)
{
	GsfInputHTTP *http = GSF_INPUT_HTTP (obj);

        http->url = NULL;
        http->content_type = NULL;
        http->ctx = NULL;
        http->buf = NULL;
        http->buf_size = 0;
}

static void
gsf_input_http_class_init (GsfInputHTTPClass *c)
{
        GObjectClass *g_object_class = (GObjectClass *) c;
        GsfInputClass *gsf_input_class = (GsfInputClass *) c;
        GParamSpec *param_spec;

        parent_class = g_type_class_ref (gsf_input_get_type ());

        gsf_input_class->Dup = gsf_input_http_dup;
        gsf_input_class->Read = gsf_input_http_read;
        gsf_input_class->Seek = gsf_input_http_seek;
        g_object_class->finalize = gsf_input_http_finalize;
        g_object_class->get_property = gsf_input_http_get_property;
        g_object_class->set_property = gsf_input_http_set_property;

        param_spec = g_param_spec_string ("url" /* name */ ,
                                          "URL" /* nick */ ,
                                          "HTTP URL accessed by this stream"
                                          /* blurb */ ,
                                          NULL /* default_value */ ,
                                          (GParamFlags) (G_PARAM_READABLE |
                                                         G_PARAM_WRITABLE |
                                                         G_PARAM_CONSTRUCT_ONLY));
        g_object_class_install_property (g_object_class, PROP_URL,
                                         param_spec);

        param_spec = g_param_spec_string ("content_type" /* name */ ,
                                          "mime type" /* nick */ ,
                                          "Content-Type in HTTP header" /* blurb */
                                          , NULL /* default_value */ ,
                                          (GParamFlags) (G_PARAM_READABLE |
                                                         G_PARAM_WRITABLE |
                                                         G_PARAM_CONSTRUCT_ONLY));
        g_object_class_install_property (g_object_class, PROP_CONTENT_TYPE,
                                         param_spec);
}

static void
gsf_input_http_set_property (GObject *object,
                             guint property_id,
                             GValue const *VAL, GParamSpec *pspec)
{
        GsfInputHTTP *input;
        char *old;

        input = GSF_INPUT_HTTP (object);

        switch (property_id) {
        case PROP_URL:
                old = input->url;
                input->url = g_value_dup_string (VAL);
                g_free (old);
                break;
        case PROP_CONTENT_TYPE:
                old = input->content_type;
                input->content_type = g_value_dup_string (VAL);
                g_free (old);
                break;
        default:
                G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id,
                                                   pspec);
                break;
        }
}

static void
gsf_input_http_get_property (GObject *object,
                             guint property_id,
                             GValue *VAL, GParamSpec *pspec)
{
        GsfInputHTTP *input;

        input = GSF_INPUT_HTTP (object);

        switch (property_id) {
        case PROP_URL:
                g_value_set_string (VAL, input->url);
                break;
        case PROP_CONTENT_TYPE:
                g_value_set_string (VAL, input->content_type);
                break;
        default:
                G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id,
                                                   pspec);
                break;
        }
}

/**
 * gsf_input_http_get_url :
 * @input: #GsfInputHTTP
 *
 * Returns: an allocated string containing the URL used for input.
 **/
gchar *
gsf_input_http_get_url (GsfInputHTTP *input)
{
        gchar *url;
        g_return_val_if_fail(GSF_IS_INPUT_HTTP(input), NULL);
        g_object_get (G_OBJECT (input), "url", &url, NULL);
        return url;
}

/**
 * gsf_input_http_get_content_type :
 * @input: #GsfInputHTTP
 *
 * Returns: an allocated string containing the Content-Type field of the HTTP response.
 **/
gchar *
gsf_input_http_get_content_type (GsfInputHTTP *input)
{
        gchar *content_type;
        g_return_val_if_fail(GSF_IS_INPUT_HTTP(input), NULL);
        g_object_get (G_OBJECT (input), "content_type", &content_type, NULL);
        return content_type;
}

/**
 * gsf_input_http_new :
 * @url: A string containing the URL to retrieve
 * @error: Holds any errors encountered when establishing the HTTP connection
 *
 * Returns: an open HTTP connection, ready for reading.
 **/
GsfInput *
gsf_input_http_new (gchar const * url, GError **error G_GNUC_UNUSED)
{
        GObject *obj;
        gpointer ctx;
        char *content_type;

        g_return_val_if_fail(url != NULL, NULL);

        ctx = xmlNanoHTTPOpen (url, &content_type);
        if (!ctx)               /* no meaningful errors provided by nanohttp */
                return NULL;

        obj = g_object_new (GSF_INPUT_HTTP_TYPE,
		"url",		url,
		"content-type", content_type,
		NULL);
	if (G_UNLIKELY (NULL == obj)) return NULL;

        gsf_input_set_size (GSF_INPUT (obj), xmlNanoHTTPContentLength (ctx));
        GSF_INPUT_HTTP (obj)->ctx = ctx;

        return GSF_INPUT (obj);
}

static GsfInput *
gsf_input_http_dup (GsfInput *src, GError **err)
{
        return gsf_input_http_new (GSF_INPUT_HTTP (src)->url, err);
}

static guint8 const *
gsf_input_http_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
        int nread;
        size_t total_read;
        gpointer ctx = GSF_INPUT_HTTP (input)->ctx;
        GsfInputHTTP *input_http = GSF_INPUT_HTTP (input);

        if (buffer == NULL) {
                if (input_http->buf_size < num_bytes) {
                        input_http->buf_size = num_bytes;
                        g_free (input_http->buf);
                        input_http->buf = g_new (guint8, input_http->buf_size);
                }
                buffer = input_http->buf;
        }

        for (total_read = 0; total_read < num_bytes; total_read += nread) {
                nread = xmlNanoHTTPRead (ctx, buffer, num_bytes - total_read);
                if (nread <= 0)
                        return NULL;
        }
        return buffer;
}

static gboolean
gsf_input_http_seek (GsfInput *input G_GNUC_UNUSED,
		     gsf_off_t offset G_GNUC_UNUSED, GSeekType whence G_GNUC_UNUSED)
{
        return FALSE;
}

GSF_CLASS (GsfInputHTTP, gsf_input_http,
	   gsf_input_http_class_init, gsf_input_http_init,
	   GSF_INPUT_TYPE)
