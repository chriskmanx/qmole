/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-image.c: Image formats
 *
 * Copyright (C) 2004, 2005 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-image.h>
#include <glib/gi18n-lib.h>
#include <string.h>
#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n-lib.h>

/**
 * go_mime_to_image_format:
 * @mime_type: a mime type string
 *
 * returns: file extension for the given mime type.
 **/

char *
go_mime_to_image_format (char const *mime_type)
{
 	guint i;
	const char *suffix;
	const char* exceptions[] = {
		"svg+xml", "svg",
		"x-wmf", "wmf",
		"x-emf", "emf",
	};

	if (strncmp (mime_type, "image/", 6) != 0)
		return NULL;
	suffix = mime_type + 6;
	for (i = 0; i < G_N_ELEMENTS (exceptions); i +=2)
		if (strcmp (suffix, exceptions[i]) == 0)
			return g_strdup (exceptions[i+1]);

	return g_strdup (suffix);
}

/**
 * go_image_format_to_mime:
 * @format: a file extension string
 *
 * returns: corresponding mime type.
 **/

char *
go_image_format_to_mime (char const *format)
{
	char *ret = NULL;
 	guint i;
#ifdef GOFFICE_WITH_GTK
	GSList *ptr, *pixbuf_fmts;
	GdkPixbufFormat *pfmt;
	gchar *name;
	int cmp;
	gchar **mimes;
#endif
	const char* formats[] = {
		"svg", "image/svg,image/svg+xml",
		"wmf", "x-wmf",
		"emf", "x-emf",
	};
	
	if (format == NULL)
		return NULL;

	for (i = 0; i < G_N_ELEMENTS (formats); i +=2)
		if (strcmp (format, formats[i]) == 0)
			return g_strdup (formats[i+1]);

#ifdef GOFFICE_WITH_GTK
	/* Not a format we have special knowledge about - ask gdk-pixbuf */
	pixbuf_fmts = gdk_pixbuf_get_formats ();
	for (ptr = pixbuf_fmts; ptr != NULL; ptr = ptr->next) {
		pfmt = (GdkPixbufFormat *)ptr->data;
		name = gdk_pixbuf_format_get_name (pfmt);
		cmp = strcmp (format, name);
		g_free (name);
		if (cmp == 0) {
			mimes = gdk_pixbuf_format_get_mime_types (pfmt);
			ret = g_strjoinv (",", mimes);
			g_strfreev (mimes);
			break;
		}
	}
	g_slist_free (pixbuf_fmts);
#endif

	return ret;
}

static GOImageFormatInfo const image_format_infos[GO_IMAGE_FORMAT_UNKNOWN] = {
	{GO_IMAGE_FORMAT_SVG, (char *) "svg",  (char *) N_("SVG (vector graphics)"), 	 
		(char *) "svg", FALSE, FALSE, TRUE},
	{GO_IMAGE_FORMAT_PNG, (char *) "png",  (char *) N_("PNG (raster graphics)"), 	 
		(char *) "png", TRUE,  TRUE, TRUE},
	{GO_IMAGE_FORMAT_JPG, (char *) "jpeg", (char *) N_("JPEG (photograph)"),     	 
		(char *) "jpg", TRUE,  TRUE, FALSE},
	{GO_IMAGE_FORMAT_PDF, (char *) "pdf",  (char *) N_("PDF (portable document format)"), 
		(char *) "pdf", FALSE, FALSE, TRUE},
	{GO_IMAGE_FORMAT_PS,  (char *) "ps",   (char *) N_("PS (postscript)"), 		 
		(char *) "ps",  FALSE, TRUE, TRUE},
	{GO_IMAGE_FORMAT_EMF, (char *) "emf",  (char *) N_("EMF (extended metafile)"),
		(char *) "emf", FALSE, FALSE, TRUE},
	{GO_IMAGE_FORMAT_WMF, (char *) "wmf",  (char *) N_("WMF (windows metafile)"), 
		(char *) "wmf", FALSE, FALSE, TRUE}
};

static GOImageFormatInfo *pixbuf_image_format_infos = NULL;
static unsigned pixbuf_format_nbr = 0;
static gboolean pixbuf_format_done = FALSE;

#define PIXBUF_IMAGE_FORMAT_OFFSET (1+GO_IMAGE_FORMAT_UNKNOWN)

static void
go_image_build_pixbuf_format_infos (void)
{
#ifdef GOFFICE_WITH_GTK
	GdkPixbufFormat *fmt;
	GSList *l, *pixbuf_fmts;
	GOImageFormatInfo *format_info;
	gchar **exts;
	unsigned i;

	if (pixbuf_format_done)
		return;
	
	pixbuf_fmts = gdk_pixbuf_get_formats ();
	pixbuf_format_nbr = g_slist_length (pixbuf_fmts);
	
	if (pixbuf_format_nbr > 0) {
		pixbuf_image_format_infos = g_new (GOImageFormatInfo, pixbuf_format_nbr);

		for (l = pixbuf_fmts, i = 1, format_info = pixbuf_image_format_infos; 
		     l != NULL; 
		     l = l->next, i++, format_info++) {
			fmt = (GdkPixbufFormat *)l->data;

			format_info->format = GO_IMAGE_FORMAT_UNKNOWN + i;
			format_info->name = gdk_pixbuf_format_get_name (fmt);
			format_info->desc = gdk_pixbuf_format_get_description (fmt);
			exts = gdk_pixbuf_format_get_extensions (fmt);
			format_info->ext = g_strdup (exts[0]);
			if (format_info->ext == NULL)
				format_info->ext = format_info->name;
			g_strfreev (exts);
			format_info->has_pixbuf_saver = gdk_pixbuf_format_is_writable (fmt);
			format_info->is_dpi_useful = FALSE;
			format_info->alpha_support = FALSE;
		}
	}

	g_slist_free (pixbuf_fmts);
#endif /* GOFFICE_WITH_GTK */
	pixbuf_format_done = TRUE;
}

/**
 * go_image_get_format_info:
 * @format: a #GOImageFormat
 *
 * Retrieves infromation associated to @format.
 * 
 * returns: a #GOImageFormatInfo struct.
 **/

GOImageFormatInfo const *
go_image_get_format_info (GOImageFormat format)
{
	if (format > GO_IMAGE_FORMAT_UNKNOWN)
		go_image_build_pixbuf_format_infos ();
		
	g_return_val_if_fail (format >= 0 && 
			      format != GO_IMAGE_FORMAT_UNKNOWN &&
			      format <= GO_IMAGE_FORMAT_UNKNOWN + pixbuf_format_nbr, NULL);
	if (format < GO_IMAGE_FORMAT_UNKNOWN)	
		return &image_format_infos[format];

	return &pixbuf_image_format_infos[format - PIXBUF_IMAGE_FORMAT_OFFSET];
}

/**
 * go_image_get_format_from_name:
 * @name: a string
 *
 * returns: corresponding #GOImageFormat.
 **/

GOImageFormat	 
go_image_get_format_from_name (char const *name)
{
	unsigned i;

	go_image_build_pixbuf_format_infos ();
	
	for (i = 0; i < GO_IMAGE_FORMAT_UNKNOWN; i++) {
		if (strcmp (name, image_format_infos[i].name) == 0)
			return image_format_infos[i].format;
	}

	for (i = 0; i < pixbuf_format_nbr; i++) {
		if (strcmp (name, pixbuf_image_format_infos[i].name) == 0)
			return pixbuf_image_format_infos[i].format;
	}

	g_warning ("[GOImage::get_format_from_name] Unknown format name (%s)", name);
	return GO_IMAGE_FORMAT_UNKNOWN;
}

/**
 * go_image_get_formats_with_pixbuf_saver:
 *
 * returns: a list of #GOImageFormat that can be created from a pixbuf.
 **/

GSList *
go_image_get_formats_with_pixbuf_saver (void)
{
	GSList *list = NULL;
	unsigned i;

	for (i = 0; i < GO_IMAGE_FORMAT_UNKNOWN; i++) 
		if (image_format_infos[i].has_pixbuf_saver)
			list = g_slist_prepend (list, GUINT_TO_POINTER (i));

	/* TODO: before enabling this code, we must remove duplicate in pixbuf_image_format_infos */
#if 0	
	go_image_build_pixbuf_format_infos ();

	for (i = 0; i < pixbuf_format_nbr; i++) {
		if (pixbuf_image_format_infos[i].has_pixbuf_saver)
			list = g_slist_prepend (list, GUINT_TO_POINTER (i + PIXBUF_IMAGE_FORMAT_OFFSET));
	}
#endif

	return list;
}

/*********************************
 * GOImage object implementation *
 *********************************/

static GObjectClass *parent_klass;

struct _GOImage {
	GObject parent;
	guint8 *data;
	guint width, height, rowstride;
	gboolean target_cairo;
#ifdef GOFFICE_WITH_CAIRO
	cairo_t *cairo;
#endif
#ifdef GOFFICE_WITH_GTK
	GdkPixbuf *pixbuf;
#endif
};

enum {
	IMAGE_PROP_0,
	IMAGE_PROP_WIDTH,
	IMAGE_PROP_HEIGHT,
#ifdef GOFFICE_WITH_GTK
	IMAGE_PROP_PIXBUF,
#endif
};

#ifdef GOFFICE_WITH_CAIRO
static void
pixbuf_to_cairo (GOImage *image)
{
	guint i,j, rowstride;
	guint t;
	unsigned char *src, *dst;

	g_return_if_fail (IS_GO_IMAGE (image) && image->data && image->pixbuf);
	
#define MULT(d,c,a,t) G_STMT_START { t = c * a + 0x7f; d = ((t >> 8) + t) >> 8; } G_STMT_END

	src = gdk_pixbuf_get_pixels (image->pixbuf);
	dst = image->data;
	rowstride = gdk_pixbuf_get_rowstride (image->pixbuf);

	for (i = 0; i < image->height; i++) {
		for (j = 0; j < image->width; j++) {
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
			MULT(dst[0], src[2], src[3], t);
			MULT(dst[1], src[1], src[3], t);
			MULT(dst[2], src[0], src[3], t);
			dst[3] = src[3];
#else	  
			MULT(dst[3], src[2], src[3], t);
			MULT(dst[2], src[1], src[3], t);
			MULT(dst[1], src[0], src[3], t);
			dst[0] = src[3];
#endif
			src += 4;
			dst += 4;
		}
		dst += image->rowstride - image->width * 4;
		src += rowstride - image->width * 4;
	}
#undef MULT
}
#endif

#ifdef GOFFICE_WITH_GTK
static void
cairo_to_pixbuf (GOImage *image)
{
	guint i,j, rowstride;
	unsigned char *src, *dst;
	guint t;
	
	g_return_if_fail (IS_GO_IMAGE (image) && image->data && image->pixbuf);

#define MULT(d,c,a,t) G_STMT_START { t = (a)? c * 255 / a: 0; d = t;} G_STMT_END

	dst = gdk_pixbuf_get_pixels (image->pixbuf);
	rowstride = gdk_pixbuf_get_rowstride (image->pixbuf);
	src = image->data;

	for (i = 0; i < image->height; i++) {
		for (j = 0; j < image->width; j++) {
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
			MULT(dst[0], src[2], src[3], t);
			MULT(dst[1], src[1], src[3], t);
			MULT(dst[2], src[0], src[3], t);
			dst[3] = src[3];
#else	  
			MULT(dst[3], src[2], src[3], t);
			MULT(dst[2], src[1], src[3], t);
			MULT(dst[1], src[0], src[3], t);
			dst[0] = src[3];
#endif
			src += 4;
			dst += 4;
		}
		dst += rowstride - image->width * 4;
		src += image->rowstride - image->width * 4;
	}
#undef MULT
}
#endif

static void
go_image_set_property (GObject *obj, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GOImage *image = GO_IMAGE (obj);
	gboolean size_changed = FALSE;
	guint n;

	switch (param_id) {
	case IMAGE_PROP_WIDTH:
		n = g_value_get_uint (value);
		if (n != image->width) {
			image->width = n;
			size_changed = TRUE;
		}
		break;
	case IMAGE_PROP_HEIGHT:
		n = g_value_get_uint (value);
		if (n != image->height) {
			image->height = n;
			size_changed = TRUE;
		}
		break;
#ifdef GOFFICE_WITH_GTK
	case IMAGE_PROP_PIXBUF: {
			GdkPixbuf *pixbuf = GDK_PIXBUF (g_value_get_object (value));
			if (!GDK_IS_PIXBUF (pixbuf))
				break;
			if (!gdk_pixbuf_get_has_alpha (pixbuf))
				pixbuf = gdk_pixbuf_add_alpha (pixbuf, FALSE, 0, 0, 0);
			else
				g_object_ref (pixbuf);
			if (image->pixbuf)
				g_object_unref (image->pixbuf);
			image->pixbuf = pixbuf;
			if (image->data != NULL) {
				g_free (image->data);
				image->data = NULL;
			}
			image->width = gdk_pixbuf_get_width (pixbuf);
			image->height = gdk_pixbuf_get_height (pixbuf);
			image->rowstride = gdk_pixbuf_get_rowstride (pixbuf);
			image->target_cairo = FALSE;
		}
		break;
#endif

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}

	if (size_changed) {
		if (image->pixbuf) {
			g_object_unref (image->pixbuf);
			image->pixbuf = NULL;
		}
		if (image->data != NULL)
			g_free (image->data);
		/* GOImage only supports pixbuf with alpha values at the moment */
		image->rowstride = image->width * 4;
		image->data = g_new0 (guint8, image->height * image->rowstride);
		image->target_cairo = TRUE;
	}
}

static void
go_image_get_property (GObject *obj, guint param_id,
		       GValue *value, GParamSpec *pspec)
{
	GOImage *image = GO_IMAGE (obj);

	switch (param_id) {
	case IMAGE_PROP_WIDTH:
		g_value_set_uint (value, image->width);
		break;
	case IMAGE_PROP_HEIGHT:
		g_value_set_uint (value, image->height);
		break;
#ifdef GOFFICE_WITH_GTK
	case IMAGE_PROP_PIXBUF:
		if (image->target_cairo && image->pixbuf) {
			cairo_to_pixbuf (image);
			image->target_cairo = FALSE;
		}
		g_value_set_object (value, image->pixbuf);
		break;
#endif

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
}

static void
go_image_finalize (GObject *obj)
{
	GOImage *image = GO_IMAGE	(obj);
	if (image->data != NULL)
		g_free (image->data);
#ifdef GOFFICE_WITH_GTK
	if (image->pixbuf)
		g_object_unref (image->pixbuf);
#endif
	(parent_klass->finalize) (obj);
}

typedef GObjectClass GOImageClass;

static void
go_image_class_init (GOImageClass *klass)
{
	klass->finalize = go_image_finalize;
	klass->set_property = go_image_set_property;
	klass->get_property = go_image_get_property;
	parent_klass = g_type_class_peek_parent (klass);
	g_object_class_install_property (klass, IMAGE_PROP_WIDTH,
		g_param_spec_uint ("width", _("Width"),
			_("Image width in pixels"),
			0, G_MAXUINT16, 0, G_PARAM_READWRITE));
	g_object_class_install_property (klass, IMAGE_PROP_HEIGHT,
		g_param_spec_uint ("height", _("Height"),
			_("Image height in pixels"),
			0, G_MAXUINT16, 0, G_PARAM_READWRITE));
#ifdef GOFFICE_WITH_GTK
	g_object_class_install_property (klass, IMAGE_PROP_PIXBUF,
		g_param_spec_object ("pixbuf", _("Pixbuf"),
			_("GdkPixbuf object from which the GOImage is built"),
			GDK_TYPE_PIXBUF, G_PARAM_READWRITE));
#endif
}

GSF_CLASS (GOImage, go_image,
		  go_image_class_init, NULL,
		  G_TYPE_OBJECT)

#ifdef GOFFICE_WITH_CAIRO
cairo_t *
go_image_get_cairo (GOImage *image)
{
	cairo_surface_t *surface ;
	cairo_t *cairo;

	g_return_val_if_fail (IS_GO_IMAGE (image), NULL);
	if (image->data == NULL && image->pixbuf == NULL)
		return NULL;
	if (image->data == NULL) {
		/* image built from a pixbuf */
		image->data = g_new0 (guint8, image->height * image->rowstride);
	}
	if (!image->target_cairo) {
		pixbuf_to_cairo (image);
		image->target_cairo = TRUE;
	}
	surface = cairo_image_surface_create_for_data (
              				image->data,
							CAIRO_FORMAT_ARGB32,
							image->width, image->height, 
               				image->rowstride);
	cairo = cairo_create (surface);
	cairo_surface_destroy (surface);
	image->target_cairo = TRUE;
	return cairo;
}

cairo_pattern_t *go_image_create_cairo_pattern (GOImage *image)
{
	cairo_surface_t *surface ;
	cairo_pattern_t *pat;

	g_return_val_if_fail (IS_GO_IMAGE (image), NULL);
	if (image->data == NULL && image->pixbuf == NULL)
		return NULL;
	if (image->data == NULL) {
		/* image built from a pixbuf */
		image->data = g_new0 (guint8, image->height * image->rowstride);
	}
	if (!image->target_cairo) {
		pixbuf_to_cairo (image);
		image->target_cairo = TRUE;
	}
	surface = cairo_image_surface_create_for_data (
              				image->data,
							CAIRO_FORMAT_ARGB32,
							image->width, image->height, 
               				image->rowstride);
	pat = cairo_pattern_create_for_surface (surface);
	cairo_surface_destroy (surface);
	return pat;
}

#endif

#ifdef GOFFICE_WITH_GTK
GOImage *
go_image_new_from_pixbuf (GdkPixbuf *pixbuf)
{
	return g_object_new (GO_IMAGE_TYPE, "pixbuf", pixbuf, NULL);
}

GdkPixbuf *
go_image_get_pixbuf (GOImage *image)
{
	g_return_val_if_fail (image != NULL, NULL);
	if (!image->pixbuf) {
		if (image->width == 0 || image->height == 0 || image->data == NULL)
			return NULL;
		image->pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8,
								image->width, image->height);
	}
	if (image->target_cairo) {
		cairo_to_pixbuf (image);
		image->target_cairo = FALSE;
	}
	return image->pixbuf;
}
#endif

GOImage *
go_image_new_from_file (const char *filename, GError **error)
{
#ifdef GOFFICE_WITH_GTK
	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file (filename, error);
	GOImage *image = g_object_new (GO_IMAGE_TYPE, "pixbuf", pixbuf, NULL);
	g_object_unref (pixbuf);
	image->target_cairo = FALSE;
	return image;
#else
	g_warning ("go_image_new_from_file not implemented!");
	return NULL;
#endif
}

guint8 *
go_image_get_pixels (GOImage *image)
{
	g_return_val_if_fail (image, NULL);
	return image->data;
}

int
go_image_get_rowstride (GOImage *image)
{
	g_return_val_if_fail (image, 0);
	return image->rowstride;
}

void
go_image_fill (GOImage *image, GOColor color)
{
	guint32 val;
	guint8 *dst;
	unsigned i, j;
	g_return_if_fail (image);

	dst = image->data;
	if (image->target_cairo)
		val = (UINT_RGBA_R (color) << 8) + (UINT_RGBA_G (color) << 16)
			+ (UINT_RGBA_B (color) << 24) + UINT_RGBA_A (color);
	else
		val = color;
	for (i = 0; i < image->height; i++) {
		for (j = 0; j < image->width; j++)
			*((guint32*) dst) = val;
		dst += image->rowstride - image->width * 4;
	}
}
