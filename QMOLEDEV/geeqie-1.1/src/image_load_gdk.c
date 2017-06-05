/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2011 The Geeqie Team
 *
 * Author: Vladimir Nadvornik
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include "main.h"
#include "image-load.h"
#include "image_load_gdk.h"


static gchar* image_loader_gdk_get_format_name(gpointer loader)
{
	return gdk_pixbuf_format_get_name(gdk_pixbuf_loader_get_format(GDK_PIXBUF_LOADER(loader)));
}
static gchar** image_loader_gdk_get_format_mime_types(gpointer loader)
{
	return gdk_pixbuf_format_get_mime_types(gdk_pixbuf_loader_get_format(GDK_PIXBUF_LOADER(loader)));
}

static gpointer image_loader_gdk_new(ImageLoaderBackendCbAreaUpdated area_updated_cb, ImageLoaderBackendCbSize size_cb, ImageLoaderBackendCbAreaPrepared area_prepared_cb, gpointer data)
{
        GdkPixbufLoader *loader = gdk_pixbuf_loader_new();
        
	g_signal_connect(G_OBJECT(loader), "area_updated", G_CALLBACK(area_updated_cb), data);
	g_signal_connect(G_OBJECT(loader), "size_prepared", G_CALLBACK(size_cb), data);
	g_signal_connect(G_OBJECT(loader), "area_prepared", G_CALLBACK(area_prepared_cb), data);
	return (gpointer) loader;
}

static void image_loader_gdk_abort(gpointer loader)
{
}

static void image_loader_gdk_free(gpointer loader)
{
	g_object_unref(G_OBJECT(loader));
}

void image_loader_backend_set_default(ImageLoaderBackend *funcs)
{
	funcs->loader_new = image_loader_gdk_new;
	funcs->set_size = (ImageLoaderBackendFuncSetSize) gdk_pixbuf_loader_set_size;
	funcs->load = NULL;
	funcs->write = (ImageLoaderBackendFuncWrite) gdk_pixbuf_loader_write;
	funcs->get_pixbuf = (ImageLoaderBackendFuncGetPixbuf) gdk_pixbuf_loader_get_pixbuf;
	funcs->close = (ImageLoaderBackendFuncClose) gdk_pixbuf_loader_close;
	funcs->abort = image_loader_gdk_abort;
	funcs->free = image_loader_gdk_free;
	
	funcs->get_format_name = image_loader_gdk_get_format_name;
	funcs->get_format_mime_types = image_loader_gdk_get_format_mime_types;
}



