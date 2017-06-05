/* LIBGIMP - The GIMP Library
 * Copyright (C) 1995-1997 Peter Mattis and Spencer Kimball
 *
 * Thumbnail handling according to the Thumbnail Managing Standard
 * (see http://jens.triq.net/thumbnail-spec/index.html)
 * This file has code snippets extracted from a revised libgimpthumb 2.6.7.
 * The changes make the library multi-user-safe and thread-safe.
 *
 * Portions copyright (C) 2001-2003 Sven Neumann <sven@gimp.org>
 * Portions copyright (C) 2001-2003 Michael Natterer <mitch@gimp.org>
 * Portions copyright (C) 2009 tooar <tooar@emelfm2.net>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * (The original libgimpthumb was covered by version 2 or later.)
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see http://www.gnu.org/licenses.
 */

#include <glib/gstdio.h>

#define USE_XDG_DIR

#if defined (GIMP_THUMB_DEBUG) && defined (__GNUC__)
# define GIMP_THUMB_DEBUG_CALL(t) \
        g_printerr ("%s: %s\n", \
                     __FUNCTION__, t->image_uri ? t->image_uri : "(null)")
#else
# define GIMP_THUMB_DEBUG_CALL(t)
#endif

typedef enum
{
  GIMP_THUMB_FILE_TYPE_NONE,
  GIMP_THUMB_FILE_TYPE_REGULAR,
  GIMP_THUMB_FILE_TYPE_FOLDER,
  GIMP_THUMB_FILE_TYPE_SPECIAL
} GimpThumbFileType;
#define GIMP_THUMB_FILE_TYPE_COUNT GIMP_THUMB_FILE_TYPE_SPECIAL + 1

typedef enum
{
  GIMP_THUMB_SIZE_UNKOWN = -1, /* not included in count */
  GIMP_THUMB_SIZE_FAIL   = 0,
  GIMP_THUMB_SIZE_NORMAL = 128,
  GIMP_THUMB_SIZE_LARGE  = 256
} GimpThumbSize;
#define GIMP_THUMB_SIZE_COUNT 3

typedef enum
{
  GIMP_THUMB_STATE_UNKNOWN,
  GIMP_THUMB_STATE_REMOTE,
  GIMP_THUMB_STATE_FOLDER,
  GIMP_THUMB_STATE_SPECIAL,
  GIMP_THUMB_STATE_NOT_FOUND,
  GIMP_THUMB_STATE_EXISTS,
  GIMP_THUMB_STATE_OLD,
  GIMP_THUMB_STATE_FAILED,
  GIMP_THUMB_STATE_OK
} GimpThumbState;
#define GIMP_THUMB_STATE_COUNT GIMP_THUMB_STATE_OK + 1

typedef enum
{
  GIMP_THUMB_ERROR_OPEN,         /*  open failed                            */
  GIMP_THUMB_ERROR_OPEN_ENOENT,  /*  file does not exist                    */
  GIMP_THUMB_ERROR_MKDIR         /*  mkdir failed                           */
} GimpThumbError;


#define GIMP_THUMB_ERROR (gimp_thumb_error_quark ())

GQuark  gimp_thumb_error_quark (void) G_GNUC_CONST;

typedef struct _GimpThumbConnection GimpThumbConnection;

GimpThumbConnection *gimp_thumbconnection_new (const gchar *creator,
                          const gchar *thumb_basedir);
gchar *gimp_thumbconnection_name_from_uri (GimpThumbConnection *conn,
                                    const gchar   *uri,
                                    GimpThumbSize  size);
gboolean gimp_thumbconnection_ensure_thumb_dir (GimpThumbConnection *conn,
                                       GimpThumbSize   size,
                                       GError        **error);

static guint gimp_thumbconnection_size (GimpThumbConnection *conn,
                                       gint size);
gchar *_gimp_thumbconnection_png_lookup (GimpThumbConnection *conn,
                                  const gchar   *name,
                                  const gchar   *basedir,
                                  GimpThumbSize *size);

GimpThumbFileType gimp_thumb_file_test (const gchar *filepath,
                      gint64      *mtime,
                      gint64      *size,
                      gint        *err_no);
gchar *gimp_thumb_find_thumb (GimpThumbConnection *conn,
                       const gchar   *uri,
                       GimpThumbSize *size);
gchar *_gimp_thumb_filepath_from_uri (const gchar *uri);
gchar *_gimp_thumb_png_name (const gchar *uri);

//===== FROM THUMBNAIL =====

#define TAG_DESCRIPTION           "tEXt::Description"
#define TAG_SOFTWARE              "tEXt::Software"
#define TAG_THUMB_URI             "tEXt::Thumb::URI"
#define TAG_THUMB_MTIME           "tEXt::Thumb::MTime"
#define TAG_THUMB_FILESIZE        "tEXt::Thumb::Size"
#define TAG_THUMB_MIMETYPE        "tEXt::Thumb::Mimetype"
#define TAG_THUMB_IMAGE_WIDTH     "tEXt::Thumb::Image::Width"
#define TAG_THUMB_IMAGE_HEIGHT    "tEXt::Thumb::Image::Height"
#define TAG_THUMB_GIMP_TYPE       "tEXt::Thumb::X-GIMP::Type"
#define TAG_THUMB_GIMP_LAYERS     "tEXt::Thumb::X-GIMP::Layers"

typedef struct _GimpThumbnail GimpThumbnail;

void gimp_thumbnail_set_uri (GimpThumbnail *thumbnail,
                        const gchar   *uri);
GdkPixbuf *gimp_thumbnail_load_thumb (GimpThumbConnection *conn,
                           GimpThumbnail  *thumbnail,
                           GimpThumbSize   size,
                           GError        **error);
GimpThumbState gimp_thumbnail_peek_thumb (GimpThumbConnection *conn,
                           GimpThumbnail *thumbnail,
                           GimpThumbSize  size);

static void gimp_thumbnail_set_info_from_pixbuf (GimpThumbnail *thumbnail,
                           GdkPixbuf     *pixbuf);

#ifdef THUMB_OBJECT

#define GIMP_TYPE_THUMBNAIL            (gimp_thumbnail_get_type ())
#define GIMP_THUMBNAIL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GIMP_TYPE_THUMBNAIL, GimpThumbnail))
#define GIMP_THUMBNAIL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GIMP_TYPE_THUMBNAIL, GimpThumbnailClass))
#define GIMP_IS_THUMBNAIL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GIMP_TYPE_THUMBNAIL))
#define GIMP_IS_THUMBNAIL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GIMP_TYPE_THUMBNAIL))
#define GIMP_THUMBNAIL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GIMP_TYPE_THUMBNAIL, GimpThumbnailClass))

GType gimp_thumbnail_get_type         (void) G_GNUC_CONST;

#else //ndef THUMB_OBJECT

struct _GimpThumbnail
{
//#ifdef THUMB_OBJECT
//  GObject         parent_instance;
//#endif
  GimpThumbState  image_state;
  gchar          *image_uri;
  gchar          *image_filename; /* a filepath */
  gint64          image_filesize;
  gint64          image_mtime;
  gint            image_not_found_errno;
  gint            image_width;
  gint            image_height;
  gchar          *image_type;
  gint            image_num_layers;
  GimpThumbState  thumb_state;
  GimpThumbSize   thumb_size;
  gchar          *thumb_filename; /* a filepath */
  gint64          thumb_filesize;
  gint64          thumb_mtime;
  gchar          *image_mimetype;

  gpointer        _reserved_2;
};

#endif

#ifdef THUMB_OBJECT

typedef struct _GimpThumbnailClass GimpThumbnailClass;

struct _GimpThumbnailClass
{
  GObjectClass    parent_class;
  /* Padding for future expansion */
  void (* _gimp_reserved1) (void);
  void (* _gimp_reserved2) (void);
  void (* _gimp_reserved3) (void);
  void (* _gimp_reserved4) (void);
};

enum
{
  PROP_0,
  PROP_IMAGE_STATE,
  PROP_IMAGE_URI,
  PROP_IMAGE_MTIME,
  PROP_IMAGE_FILESIZE,
  PROP_IMAGE_MIMETYPE,
  PROP_IMAGE_WIDTH,
  PROP_IMAGE_HEIGHT,
  PROP_IMAGE_TYPE,
  PROP_IMAGE_NUM_LAYERS,
  PROP_THUMB_STATE
};


static void      gimp_thumbnail_finalize     (GObject        *object);
static void      gimp_thumbnail_set_property (GObject        *object,
                                              guint           property_id,
                                              const GValue   *value,
                                              GParamSpec     *pspec);
static void      gimp_thumbnail_get_property (GObject        *object,
                                              guint           property_id,
                                              GValue         *value,
                                              GParamSpec     *pspec);
#endif //def THUMB_OBJECT

static void      gimp_thumbnail_reset_info   (GimpThumbnail  *thumbnail);

static void      gimp_thumbnail_update_image_data (GimpThumbnail  *thumbnail);

static void      gimp_thumbnail_update_thumb_data (GimpThumbConnection *conn,
                                              GimpThumbnail  *thumbnail,
                                              GimpThumbSize   size);

static gboolean  gimp_thumbnail_save         (GimpThumbConnection *conn,
                                              GimpThumbnail  *thumbnail,
                                              GimpThumbSize   size,
                                              const gchar    *filepath,
                                              GdkPixbuf      *pixbuf,
                                              const gchar    *software,
                                              GError        **error);


#ifdef THUMB_OBJECT

/* do not use G_DEFINE_TYPE macro, it can require glib capabilities later
   than our minimum, 2.6.0. Instead, we expand the macro explicitly. */

static void      gimp_thumbnail_init         (GimpThumbnail      *self);
static void      gimp_thumbnail_class_init   (GimpThumbnailClass *klass);

static gpointer  gimp_thumbnail_parent_class = NULL;

static void
gimp_thumbnail_class_intern_init (gpointer klass)
{
  gimp_thumbnail_parent_class = g_type_class_peek_parent (klass);
  gimp_thumbnail_class_init ((GimpThumbnailClass*) klass);
}

GType
gimp_thumbnail_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
#ifdef USE_GLIB2_14
  if (g_once_init_enter (&g_define_type_id__volatile))
#else
  if (G_UNLIKELY (g_define_type_id__volatile == 0))
#endif
    {
       GType g_define_type_id;
#ifdef USE_GLIB2_12
       g_define_type_id =
         g_type_register_static_simple (G_TYPE_OBJECT,
                                        "GimpThumbnail",
                                        sizeof (GimpThumbnailClass),
                                        (GClassInitFunc) gimp_thumbnail_class_intern_init,
                                        sizeof (GimpThumbnail),
                                        (GInstanceInitFunc) gimp_thumbnail_init,
                                        (GTypeFlags) 0);
#else
      GTypeInfo g_define_type_info =
      {
        sizeof (GimpThumbnailClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) gimp_thumbnail_class_intern_init,
        (GClassFinalizeFunc) NULL,
        NULL,   /* class_data */
        sizeof (GimpThumbnail),
        0,      /* n_preallocs */
        (GInstanceInitFunc) gimp_thumbnail_init,
        NULL    /* value_table */
      };

      g_define_type_id =
        g_type_register_static (G_TYPE_OBJECT,
                                "GimpThumbnail",
                                &g_define_type_info,
                                (GTypeFlags) 0);
#endif

#ifdef USE_GLIB2_14
      g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
#else
      g_define_type_id__volatile = g_define_type_id;
#endif
    }
  return g_define_type_id__volatile;
}

static void
gimp_thumbnail_class_init (GimpThumbnailClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize     = gimp_thumbnail_finalize;
  object_class->set_property = gimp_thumbnail_set_property;
  object_class->get_property = gimp_thumbnail_get_property;

  g_object_class_install_property (object_class,
                                   PROP_IMAGE_STATE,
                                   g_param_spec_uint ("image-state", NULL,
                                                      "State of the image associated to the thumbnail object",
                                                      GIMP_THUMB_STATE_UNKNOWN,
                                                      GIMP_THUMB_STATE_COUNT - 1,
													  GIMP_THUMB_STATE_UNKNOWN,
                                                      G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_URI,
                                   g_param_spec_string ("image-uri", NULL,
                                                       "URI of the image file",
                                                        NULL,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_MTIME,
                                   g_param_spec_int64 ("image-mtime", NULL,
                                                       "Modification time of the image file in seconds since the Epoch",
                                                       0, G_MAXINT64, 0,
                                                       G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_FILESIZE,
                                   g_param_spec_int64 ("image-filesize", NULL,
                                                       "Size of the image file in bytes",
                                                       0, G_MAXINT64, 0,
                                                       G_PARAM_READWRITE));
  /**
   * GimpThumbnail::image-mimetype:
   *
   * Since: 2.2
   */
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_MIMETYPE,
                                   g_param_spec_string ("image-mimetype", NULL,
                                                        "Image mimetype",
                                                        NULL,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_WIDTH,
                                   g_param_spec_uint ("image-width", NULL,
                                                      "Width of the image in pixels",
                                                      0, G_MAXUINT, GIMP_THUMB_SIZE_NORMAL,
                                                      G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_HEIGHT,
                                   g_param_spec_uint ("image-height", NULL,
                                                      "Height of the image in pixels",
                                                      0, G_MAXUINT, GIMP_THUMB_SIZE_NORMAL,
                                                      G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_TYPE,
                                   g_param_spec_string ("image-type", NULL,
                                                        "String describing the type of the image format",
                                                        NULL,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_NUM_LAYERS,
                                   g_param_spec_uint ("image-num-layers", NULL,
                                                      "The number of layers in the image",
                                                      0, G_MAXUINT, 1,
                                                      G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_THUMB_STATE,
                                   g_param_spec_uint ("thumb-state", NULL,
                                                      "State of the thumbnail file",
                                                      GIMP_THUMB_STATE_UNKNOWN,
                                                      GIMP_THUMB_STATE_COUNT - 1,
													  GIMP_THUMB_STATE_UNKNOWN,
                                                      G_PARAM_READWRITE));
}

static void
gimp_thumbnail_init (GimpThumbnail *thumbnail)
{
  thumbnail->image_state      = GIMP_THUMB_STATE_UNKNOWN;
  thumbnail->image_uri        = NULL;
  thumbnail->image_filename   = NULL;
  thumbnail->image_mtime      = 0;
  thumbnail->image_filesize   = 0;
  thumbnail->image_mimetype   = NULL;
  thumbnail->image_width      = 0;
  thumbnail->image_height     = 0;
  thumbnail->image_type       = NULL;
  thumbnail->image_num_layers = 0;

  thumbnail->thumb_state      = GIMP_THUMB_STATE_UNKNOWN;
  thumbnail->thumb_size       = GIMP_THUMB_SIZE_UNKOWN;
  thumbnail->thumb_filename   = NULL;
  thumbnail->thumb_mtime      = 0;
  thumbnail->thumb_filesize   = 0;
}

static void
gimp_thumbnail_finalize (GObject *object)
{
  GimpThumbnail *thumbnail = GIMP_THUMBNAIL (object);

  if (thumbnail->image_uri)
    {
      g_free (thumbnail->image_uri);
      thumbnail->image_uri = NULL;
    }
  if (thumbnail->image_filename)
    {
      g_free (thumbnail->image_filename);
      thumbnail->image_filename = NULL;
    }
  if (thumbnail->image_mimetype)
    {
      g_free (thumbnail->image_mimetype);
      thumbnail->image_mimetype = NULL;
    }
  if (thumbnail->image_type)
    {
      g_free (thumbnail->image_type);
      thumbnail->image_type = NULL;
    }
  if (thumbnail->thumb_filename)
    {
      g_free (thumbnail->thumb_filename);
      thumbnail->thumb_filename = NULL;
    }

  G_OBJECT_CLASS (gimp_thumbnail_parent_class)->finalize (object);
}

static void
gimp_thumbnail_set_property (GObject      *object,
                             guint         property_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
  GimpThumbnail *thumbnail = GIMP_THUMBNAIL (object);

  switch (property_id)
    {
    case PROP_IMAGE_STATE:
      thumbnail->image_state = g_value_get_uint (value);
      break;
    case PROP_IMAGE_URI:
      gimp_thumbnail_set_uri (thumbnail, g_value_get_string (value));
      break;
    case PROP_IMAGE_MTIME:
      thumbnail->image_mtime = g_value_get_int64 (value);
      break;
    case PROP_IMAGE_FILESIZE:
      thumbnail->image_filesize = g_value_get_int64 (value);
      break;
    case PROP_IMAGE_MIMETYPE:
      g_free (thumbnail->image_mimetype);
      thumbnail->image_mimetype = g_value_dup_string (value);
      break;
    case PROP_IMAGE_WIDTH:
      thumbnail->image_width = g_value_get_uint (value);
      break;
    case PROP_IMAGE_HEIGHT:
      thumbnail->image_height = g_value_get_uint (value);
      break;
    case PROP_IMAGE_TYPE:
      g_free (thumbnail->image_type);
      thumbnail->image_type = g_value_dup_string (value);
      break;
    case PROP_IMAGE_NUM_LAYERS:
      thumbnail->image_num_layers = g_value_get_uint (value);
      break;
    case PROP_THUMB_STATE:
      thumbnail->thumb_state = g_value_get_uint (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gimp_thumbnail_get_property (GObject    *object,
                             guint       property_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
  GimpThumbnail *thumbnail = GIMP_THUMBNAIL (object);

  switch (property_id)
    {
    case PROP_IMAGE_STATE:
      g_value_set_uint (value, thumbnail->image_state);
      break;
    case PROP_IMAGE_URI:
      g_value_set_string (value, thumbnail->image_uri);
      break;
    case PROP_IMAGE_MTIME:
      g_value_set_int64 (value, thumbnail->image_mtime);
      break;
    case PROP_IMAGE_FILESIZE:
      g_value_set_int64 (value, thumbnail->image_filesize);
      break;
    case PROP_IMAGE_MIMETYPE:
      g_value_set_string (value, thumbnail->image_mimetype);
      break;
    case PROP_IMAGE_WIDTH:
      g_value_set_uint (value, thumbnail->image_width);
      break;
    case PROP_IMAGE_HEIGHT:
      g_value_set_uint (value, thumbnail->image_height);
      break;
    case PROP_IMAGE_TYPE:
      g_value_set_string (value, thumbnail->image_type);
      break;
    case PROP_IMAGE_NUM_LAYERS:
      g_value_set_uint (value, thumbnail->image_num_layers);
      break;
    case PROP_THUMB_STATE:
      g_value_set_uint (value, thumbnail->thumb_state);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

#endif //def THUMB_OBJECT

GimpThumbnail *
gimp_thumbnail_new (void)
{
#ifdef THUMB_OBJECT
  return g_object_new (GIMP_TYPE_THUMBNAIL, NULL);
#else
  GimpThumbnail *thumbnail;
#if GLIB_CHECK_VERSION (2,10,0)
  thumbnail = g_slice_alloc0 (sizeof (GimpThumbnail));
#else
  thumbnail = g_new0 (GimpThumbnail, 1);
#endif
  if (thumbnail)
	thumbnail->thumb_size = GIMP_THUMB_SIZE_UNKOWN; /* all other contents are 0 */
  return thumbnail;
#endif
}

#ifndef THUMB_OBJECT
void
gimp_thumbnail_destroy (GimpThumbnail *thumbnail)
{
  if (thumbnail != NULL)
	{
      g_free (thumbnail->image_uri);
      g_free (thumbnail->image_filename);
      g_free (thumbnail->image_type);
      g_free (thumbnail->thumb_filename);
      g_free (thumbnail->image_mimetype);
#if GLIB_CHECK_VERSION (2,10,0)
      g_slice_free1 (sizeof(GimpThumbnail), thumbnail);
#else
      g_free (thumbnail);
#endif
   }
}
#endif

void
gimp_thumbnail_set_uri (GimpThumbnail *thumbnail,
                        const gchar   *uri)
{
#ifdef THUMB_OBJECT
  g_return_if_fail (GIMP_IS_THUMBNAIL (thumbnail));
#endif

  if (thumbnail->image_uri)
    g_free (thumbnail->image_uri);

  thumbnail->image_uri = g_strdup (uri);

  if (thumbnail->image_filename)
    {
      g_free (thumbnail->image_filename);
      thumbnail->image_filename = NULL;
    }

  if (thumbnail->thumb_filename)
    {
      g_free (thumbnail->thumb_filename);
      thumbnail->thumb_filename = NULL;
    }

  thumbnail->thumb_size     = GIMP_THUMB_SIZE_UNKOWN;
  thumbnail->thumb_filesize = 0;
  thumbnail->thumb_mtime    = 0;

#ifdef THUMB_OBJECT
  g_object_set (thumbnail,
                "image-state",      GIMP_THUMB_STATE_UNKNOWN,
                "image-filesize",   (gint64) 0,
                "image-mtime",      (gint64) 0,
                "image-mimetype",   NULL,
                "image-width",      0,
                "image-height",     0,
                "image-type",       NULL,
                "image-num-layers", 0,
                "thumb-state",      GIMP_THUMB_STATE_UNKNOWN,
                NULL);
#else
  thumbnail->image_state = GIMP_THUMB_STATE_UNKNOWN;
  thumbnail->image_filesize = 0;
  thumbnail->image_mtime = 0;
  g_free (thumbnail->image_mimetype);
  thumbnail->image_mimetype = NULL;
  thumbnail->image_width = 0;
  thumbnail->image_height = 0;
  g_free (thumbnail->image_type);
  thumbnail->image_type = NULL;
  thumbnail->image_num_layers = 0;
  thumbnail->thumb_state = GIMP_THUMB_STATE_UNKNOWN;
#endif
}

gboolean
gimp_thumbnail_set_filename (GimpThumbnail  *thumbnail,
                             const gchar    *filepath,
                             GError        **error)
{
  gchar *uri;

#ifdef THUMB_OBJECT
  g_return_val_if_fail (GIMP_IS_THUMBNAIL (thumbnail), FALSE);
#endif
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  if (filepath)
    uri = g_filename_to_uri (filepath, NULL, error);
  else
    uri = NULL;

  gimp_thumbnail_set_uri (thumbnail, uri);

  g_free (uri);

  return (!filepath || uri);
}

GimpThumbState
gimp_thumbnail_peek_image (GimpThumbnail *thumbnail)
{
#ifdef THUMB_OBJECT
  g_return_val_if_fail (GIMP_IS_THUMBNAIL (thumbnail),
                        GIMP_THUMB_STATE_UNKNOWN);

  g_object_freeze_notify (G_OBJECT (thumbnail));
#endif
  gimp_thumbnail_update_image_data (thumbnail);
#ifdef THUMB_OBJECT
  g_object_thaw_notify (G_OBJECT (thumbnail));
#endif
  return thumbnail->image_state;
}

GimpThumbState
gimp_thumbnail_peek_thumb (GimpThumbConnection *conn,
                           GimpThumbnail *thumbnail,
                           gint           size)
{
#ifdef THUMB_OBJECT
  g_return_val_if_fail (GIMP_IS_THUMBNAIL (thumbnail),
                        GIMP_THUMB_STATE_UNKNOWN);

  g_object_freeze_notify (G_OBJECT (thumbnail));
#endif
  gimp_thumbnail_update_image_data (thumbnail); /* update some details of thumbnail, as for gimp_thumbnail_peek_image() */
  gimp_thumbnail_update_thumb_data (conn, thumbnail, size); /* update other details of thumbnail */
#ifdef THUMB_OBJECT
  g_object_thaw_notify (G_OBJECT (thumbnail));
#endif
  return thumbnail->thumb_state;
}

GimpThumbState
gimp_thumbnail_check_thumb (GimpThumbConnection *conn,
                            GimpThumbnail *thumbnail,
                            gint           size)
{
  GdkPixbuf *pixbuf;

#ifdef THUMB_OBJECT
  g_return_val_if_fail (GIMP_IS_THUMBNAIL (thumbnail), FALSE);
#endif

  if (gimp_thumbnail_peek_thumb (conn, thumbnail, size) == GIMP_THUMB_STATE_OK)
    return GIMP_THUMB_STATE_OK;

  pixbuf = gimp_thumbnail_load_thumb (conn, thumbnail, size, NULL);

  if (pixbuf)
    g_object_unref (pixbuf);

  return thumbnail->thumb_state;
}

static void
gimp_thumbnail_update_image_data (GimpThumbnail *thumbnail)
{
  GimpThumbState  state;
  gint64          mtime    = 0;
  gint64          filesize = 0;

  if (! thumbnail->image_uri)
    return;

  state = thumbnail->image_state;

  switch (state)
    {
    case GIMP_THUMB_STATE_UNKNOWN:
      if (thumbnail->image_filename != NULL)
         g_free (thumbnail->image_filename);

      thumbnail->image_filename =
        _gimp_thumb_filepath_from_uri (thumbnail->image_uri);

      if (! thumbnail->image_filename)
        state = GIMP_THUMB_STATE_REMOTE;

      break;

    case GIMP_THUMB_STATE_REMOTE:
      break;

    default:
      g_return_if_fail (thumbnail->image_filename != NULL);
      break;
    }

  switch (state)
    {
    case GIMP_THUMB_STATE_REMOTE:
      break;

    default:
      switch (gimp_thumb_file_test (thumbnail->image_filename,
                                    &mtime, &filesize,
                                    &thumbnail->image_not_found_errno))
        {
        case GIMP_THUMB_FILE_TYPE_REGULAR:
          state = GIMP_THUMB_STATE_EXISTS;
          break;

        case GIMP_THUMB_FILE_TYPE_FOLDER:
          state = GIMP_THUMB_STATE_FOLDER;
          break;

        case GIMP_THUMB_FILE_TYPE_SPECIAL:
          state = GIMP_THUMB_STATE_SPECIAL;
          break;

        default:
          state = GIMP_THUMB_STATE_NOT_FOUND;
          break;
        }
      break;
    }

  if (state != thumbnail->image_state)
    {
#ifdef THUMB_OBJECT
      g_object_set (thumbnail,
                    "image-state", state,
                    NULL);
#else
      thumbnail->image_state = state;
#endif
    }

  if (mtime != thumbnail->image_mtime || filesize != thumbnail->image_filesize)
    {
#ifdef THUMB_OBJECT
      g_object_set (thumbnail,
                    "image-mtime",    mtime,
                    "image-filesize", filesize,
                    NULL);
#else
      thumbnail->image_mtime = mtime;
      thumbnail->image_filesize = filesize;
#endif

      if (thumbnail->thumb_state == GIMP_THUMB_STATE_OK)
#ifdef THUMB_OBJECT
        g_object_set (thumbnail,
                      "thumb-state", GIMP_THUMB_STATE_OLD,
                      NULL);
#else
	    thumbnail->thumb_state = GIMP_THUMB_STATE_OLD;
#endif
    }
}

/* Uses thumbnail->image_uri, thumbnail->thumb_state, they must be setup before */
static void
gimp_thumbnail_update_thumb_data (GimpThumbConnection *conn,
                                  GimpThumbnail *thumbnail,
                                  gint           size)
{
  gchar          *filepath;
  GimpThumbState  state;
  gint64          mtime;
  gint64          filesize;

  if (! thumbnail->image_uri)
    return;

  state = thumbnail->thumb_state;

  filepath = gimp_thumb_find_thumb (conn, thumbnail->image_uri, &size);

  if (filepath)
  {
    if (gimp_thumb_file_test (filepath, &mtime, &filesize, NULL)
		== GIMP_THUMB_FILE_TYPE_REGULAR)
	  goto check;
  }
  mtime = filesize = 0;
  state = GIMP_THUMB_STATE_NOT_FOUND;

check:
  switch (state)
    {
    case GIMP_THUMB_STATE_EXISTS:
    case GIMP_THUMB_STATE_OLD:
    case GIMP_THUMB_STATE_FAILED:
    case GIMP_THUMB_STATE_OK:
      if (thumbnail->thumb_size     == size     &&
          thumbnail->thumb_filesize == filesize &&
          thumbnail->thumb_mtime    == mtime)
        {
	      g_return_if_fail (thumbnail->thumb_filename != NULL);
		  g_return_if_fail (strcmp (thumbnail->thumb_filename, filepath) == 0);
          g_free (filepath);
          return;
        }
      break;
    default:
      break;
    }

  if (thumbnail->thumb_filename)
    g_free (thumbnail->thumb_filename);

  thumbnail->thumb_filename = filepath; /* maybe NULL */
  thumbnail->thumb_size     = size;
  thumbnail->thumb_filesize = filesize;
  thumbnail->thumb_mtime    = mtime;

  if (state != GIMP_THUMB_STATE_NOT_FOUND)
    state = (size > GIMP_THUMB_SIZE_FAIL ?
             GIMP_THUMB_STATE_EXISTS : GIMP_THUMB_STATE_FAILED);
  if (state != thumbnail->thumb_state)
    {
#ifdef THUMB_OBJECT
      g_object_freeze_notify (G_OBJECT (thumbnail));

      g_object_set (thumbnail, "thumb-state", state, NULL);
#else
	  thumbnail->thumb_state = state;
#endif
      gimp_thumbnail_reset_info (thumbnail);

#ifdef THUMB_OBJECT
      g_object_thaw_notify (G_OBJECT (thumbnail));
#endif
    }
}

static void
gimp_thumbnail_reset_info (GimpThumbnail *thumbnail)
{
#ifdef THUMB_OBJECT
  g_object_set (thumbnail,
                "image-width",      0,
                "image-height",     0,
                "image-type",       NULL,
                "image-num-layers", 0,
                NULL);
#else
  thumbnail->image_width = 0;
  thumbnail->image_height = 0;
  g_free (thumbnail->image_type);
  thumbnail->image_type = NULL;
  thumbnail->image_num_layers = 0;
#endif
}

static void
gimp_thumbnail_set_info_from_pixbuf (GimpThumbnail *thumbnail,
                                     GdkPixbuf     *pixbuf)
{
  const gchar  *option;
  gint          num;

#ifdef THUMB_OBJECT
  g_object_freeze_notify (G_OBJECT (thumbnail));
#endif
  gimp_thumbnail_reset_info (thumbnail);

  g_free (thumbnail->image_mimetype);
  thumbnail->image_mimetype =
    g_strdup (gdk_pixbuf_get_option (pixbuf, TAG_THUMB_MIMETYPE));

  option = gdk_pixbuf_get_option (pixbuf, TAG_THUMB_IMAGE_WIDTH);
  if (option && sscanf (option, "%d", &num) == 1)
    thumbnail->image_width = num;

  option = gdk_pixbuf_get_option (pixbuf, TAG_THUMB_IMAGE_HEIGHT);
  if (option && sscanf (option, "%d", &num) == 1)
    thumbnail->image_height = num;

  thumbnail->image_type =
    g_strdup (gdk_pixbuf_get_option (pixbuf, TAG_THUMB_GIMP_TYPE));

  option = gdk_pixbuf_get_option (pixbuf, TAG_THUMB_GIMP_LAYERS);
  if (option && sscanf (option, "%d", &num) == 1)
    thumbnail->image_num_layers = num;

#ifdef THUMB_OBJECT
  g_object_thaw_notify (G_OBJECT (thumbnail));
#endif
}

static gboolean
gimp_thumbnail_save (GimpThumbConnection *conn,
                     GimpThumbnail  *thumbnail,
                     GimpThumbSize   size,
                     const gchar    *filepath,
                     GdkPixbuf      *pixbuf,
                     const gchar    *software,
                     GError        **error)
{
  const gchar  *keys[12];
  gchar        *values[12];
  gchar        *basename;
  gchar        *dirname;
  gchar        *tmpname;
  gboolean      success;
  gint          i = 0;

  keys[i]   = TAG_DESCRIPTION;
  values[i] = g_strdup_printf ("Thumbnail of %s",  thumbnail->image_uri);
  i++;

  keys[i]   = TAG_SOFTWARE;
  values[i] = g_strdup (software);
  i++;

  keys[i]   = TAG_THUMB_URI;
  values[i] = g_strdup (thumbnail->image_uri);
  i++;

  keys[i]   = TAG_THUMB_MTIME;
  values[i] = g_strdup_printf ("%" G_GINT64_FORMAT, thumbnail->image_mtime);
  i++;

  keys[i]   = TAG_THUMB_FILESIZE;
  values[i] = g_strdup_printf ("%" G_GINT64_FORMAT, thumbnail->image_filesize);
  i++;

  if (thumbnail->image_mimetype)
    {
      keys[i]   = TAG_THUMB_MIMETYPE;
      values[i] = g_strdup (thumbnail->image_mimetype);
      i++;
    }

  if (thumbnail->image_width > 0)
    {
      keys[i]   = TAG_THUMB_IMAGE_WIDTH;
      values[i] = g_strdup_printf ("%d", thumbnail->image_width);
      i++;
    }

  if (thumbnail->image_height > 0)
    {
      keys[i]   = TAG_THUMB_IMAGE_HEIGHT;
      values[i] = g_strdup_printf ("%d", thumbnail->image_height);
      i++;
    }

  if (thumbnail->image_type)
    {
      keys[i]   = TAG_THUMB_GIMP_TYPE;
      values[i] = g_strdup (thumbnail->image_type);
      i++;
    }

  if (thumbnail->image_num_layers > 0)
    {
      keys[i]   = TAG_THUMB_GIMP_LAYERS;
      values[i] = g_strdup_printf ("%d", thumbnail->image_num_layers);
      i++;
    }

  keys[i]   = NULL;
  values[i] = NULL;

  basename = g_path_get_basename (filepath);
  dirname  = g_path_get_dirname (filepath);

  tmpname = g_strdup_printf ("%s%cgimp-thumb-%d-%.8s",
                             dirname, G_DIR_SEPARATOR, getpid (), basename);

  g_free (dirname);
  g_free (basename);

  success = gdk_pixbuf_savev (pixbuf, tmpname, "png",
                              (gchar **) keys, values,
                              error);

  for (i = 0; keys[i]; i++)
    g_free (values[i]);

  if (success)
    {
#ifdef GIMP_THUMB_DEBUG
      g_printerr ("thumbnail saved to temporary file %s\n", tmpname);
#endif

#ifdef G_OS_WIN32
      /* win32 rename can't overwrite */
      g_unlink (filepath);
#endif

      if (g_rename (tmpname, filepath) == -1)
        {
          if (error != NULL)
            g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                       _("Could not create thumbnail for %s: %s"),
                       thumbnail->image_uri, g_strerror (errno));

          success = FALSE;
        }
    }

  if (success)
    {
#ifdef GIMP_THUMB_DEBUG
      g_printerr ("temporary thumbnail file renamed to %s\n", filepath);
#endif

      success = (g_chmod (filepath, 0600) == 0);

      if (! success && error != NULL)
        g_set_error (error, G_FILE_ERROR, g_file_error_from_errno (errno),
                     "Could not set permissions of thumbnail for %s: %s",
                     thumbnail->image_uri, g_strerror (errno));

#ifdef THUMB_OBJECT
      g_object_freeze_notify (G_OBJECT (thumbnail));
#endif
      gimp_thumbnail_update_thumb_data (conn, thumbnail, size);

      if (success &&
          thumbnail->thumb_state == GIMP_THUMB_STATE_EXISTS &&
          strcmp (filepath, thumbnail->thumb_filename) == 0)
        {
          thumbnail->thumb_state = GIMP_THUMB_STATE_OK;
        }

#ifdef THUMB_OBJECT
      g_object_thaw_notify (G_OBJECT (thumbnail));
#endif
    }

  g_unlink (tmpname);
  g_free (tmpname);

  return success;
}

GdkPixbuf *
gimp_thumbnail_load_thumb (GimpThumbConnection *conn,
                           GimpThumbnail  *thumbnail,
                           gint            size,
                           GError        **error)
{
  GimpThumbState  state;
  GdkPixbuf      *pixbuf;
  const gchar    *option;
  gint64          image_mtime;
  gint64          image_size;

#ifdef THUMB_OBJECT
  g_return_val_if_fail (GIMP_IS_THUMBNAIL (thumbnail), NULL);
#endif
  g_return_val_if_fail (error == NULL || *error == NULL, NULL);

  if (! thumbnail->image_uri)
    return NULL;

  state = gimp_thumbnail_peek_thumb (conn, thumbnail, size);

  if (state < GIMP_THUMB_STATE_EXISTS || state == GIMP_THUMB_STATE_FAILED)
    return NULL;

  pixbuf = gdk_pixbuf_new_from_file (thumbnail->thumb_filename, error);
  if (! pixbuf)
    return NULL;

#ifdef THUMB_OBJECT
  g_object_freeze_notify (G_OBJECT (thumbnail));
#endif
  /* URI and mtime from the thumbnail need to match our file */
  option = gdk_pixbuf_get_option (pixbuf, TAG_THUMB_URI);
  if (!option)
    goto finish;

  if (strcmp (option, thumbnail->image_uri))
    {
      /*  might be a local thumbnail, try if the local part matches  */
      const gchar *baseuri = strrchr (thumbnail->image_uri, '/');

      if (!baseuri || strcmp (option, baseuri))
        goto finish;
    }

  state = GIMP_THUMB_STATE_OLD;

  option = gdk_pixbuf_get_option (pixbuf, TAG_THUMB_MTIME);
  if (!option || sscanf (option, "%" G_GINT64_FORMAT, &image_mtime) != 1)
    goto finish;

  option = gdk_pixbuf_get_option (pixbuf, TAG_THUMB_FILESIZE);
  if (option && sscanf (option, "%" G_GINT64_FORMAT, &image_size) != 1)
    goto finish;

  /* TAG_THUMB_FILESIZE is optional but must match if present */
  if (image_mtime == thumbnail->image_mtime &&
      (option == NULL || image_size == thumbnail->image_filesize))
    {
      if (thumbnail->thumb_size == GIMP_THUMB_SIZE_FAIL)
        state = GIMP_THUMB_STATE_FAILED;
      else
        state = GIMP_THUMB_STATE_OK;
    }

  if (state == GIMP_THUMB_STATE_FAILED)
    gimp_thumbnail_reset_info (thumbnail);
  else
    gimp_thumbnail_set_info_from_pixbuf (thumbnail, pixbuf);

 finish:
  if (thumbnail->thumb_size == GIMP_THUMB_SIZE_FAIL ||
      (state != GIMP_THUMB_STATE_OLD && state != GIMP_THUMB_STATE_OK))
    {
      g_object_unref (pixbuf);
      pixbuf = NULL;
    }

#ifdef THUMB_OBJECT
  g_object_set (G_OBJECT (thumbnail) thumbnail,
                "thumb-state", state,
                NULL);

  g_object_thaw_notify (G_OBJECT (thumbnail));
#else
  thumbnail->thumb_state = state;
#endif
  return pixbuf;
}

gboolean
gimp_thumbnail_save_thumb (GimpThumbConnection *conn,
                            GimpThumbnail  *thumbnail,
                            GdkPixbuf      *pixbuf,
                            const gchar    *software,
                            GError        **error)
{
  gint     size, height;
  gchar   *filepath;
  gboolean success;

#ifdef THUMB_OBJECT
  g_return_val_if_fail (GIMP_IS_THUMBNAIL (thumbnail), FALSE);
#endif
  g_return_val_if_fail (thumbnail->image_uri != NULL, FALSE);
  g_return_val_if_fail (GDK_IS_PIXBUF (pixbuf), FALSE);
  g_return_val_if_fail (software != NULL, FALSE);
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  GIMP_THUMB_DEBUG_CALL (thumbnail);

  size = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  size = MAX (size, height);
  if (size < 1)
    return TRUE;

  filepath = gimp_thumbconnection_name_from_uri (conn, thumbnail->image_uri, size);
  if (! filepath)
    return FALSE;

  if (! gimp_thumbconnection_ensure_thumb_dir (conn, size, error))
    {
      g_free (filepath);
      return FALSE;
    }

  success = gimp_thumbnail_save (conn, thumbnail,
                                 size, filepath, pixbuf, software,
                                 error);
  g_free (filepath);

  return success;
}


//=== FROM CONNECTION ===


struct _GimpThumbConnection
{
  GQuark        signature;
  gint          thumb_num_sizes;
  gint         *thumb_sizes;
  gchar       **thumb_sizenames;
  gchar        *thumb_dir;
  gchar       **thumb_subdirs;
  gchar        *thumb_fail_subdir;
};

void
gimp_thumbconnection_set_basedir (GimpThumbConnection *conn,
                                  const gchar         *thumb_basedir);

# define GIMP_THUMB_CONNECTION_SIG "_GIMP_thumbs_object_"
# define GIMP_IS_THUMBCONNECTION(obj) \
  (obj->signature == g_quark_from_string (GIMP_THUMB_CONNECTION_SIG))

static void
gimp_thumbconnection_clear (GimpThumbConnection *conn)
{
  gint i;

  for (i = 0; i < conn->thumb_num_sizes; i++)
  {
    g_free (conn->thumb_sizenames[i]);
    g_free (conn->thumb_subdirs[i]);
  }
#ifdef USE_GLIB2_10
  g_slice_free1 (sizeof (gchar *) * i, conn->thumb_subdirs);
  g_slice_free1 (sizeof (gint) * i, conn->thumb_sizes);
  g_slice_free1 (sizeof (const gchar *) * i, conn->thumb_sizenames);
#else
  g_free (conn->thumb_subdirs);
  g_free (conn->thumb_sizes);
  g_free (conn->thumb_sizenames);
#endif
  g_free (conn->thumb_dir);
  g_free (conn->thumb_fail_subdir);
}

GimpThumbConnection *
gimp_thumbconnection_new (const gchar *creator,
                          const gchar *thumb_basedir)
{
  GimpThumbConnection *conn;
  const GEnumValue    *enum_value;
  gint                 i;
  const GEnumValue THUMB_SIZE_VALUES [] =
  {
    { GIMP_THUMB_SIZE_FAIL, "GIMP_THUMB_SIZE_FAIL", "fail" },
    { GIMP_THUMB_SIZE_NORMAL, "GIMP_THUMB_SIZE_NORMAL", "normal" },
    { GIMP_THUMB_SIZE_LARGE, "GIMP_THUMB_SIZE_LARGE", "large" },
  };

  g_return_val_if_fail (creator != NULL, NULL);
  g_return_val_if_fail (thumb_basedir == NULL ||
                        g_path_is_absolute (thumb_basedir), NULL);

#ifdef USE_GLIB2_10
  conn = g_slice_alloc (sizeof (GimpThumbConnection));
#else
  conn = g_try_malloc (sizeof (GimpThumbConnection));
#endif
  g_return_val_if_fail (conn != NULL, NULL);
  conn->signature = g_quark_from_string (GIMP_THUMB_CONNECTION_SIG);

  conn->thumb_dir = NULL; /* downstream g_free()'s */
  gimp_thumbconnection_set_basedir (conn, thumb_basedir);

  conn->thumb_num_sizes = GIMP_THUMB_SIZE_COUNT;
#ifdef USE_GLIB2_10
  conn->thumb_sizes     = g_slice_alloc (sizeof (gint) * GIMP_THUMB_SIZE_COUNT);
  conn->thumb_sizenames = (gchar **) g_slice_alloc (sizeof (gchar *) * GIMP_THUMB_SIZE_COUNT);
  conn->thumb_subdirs   = (gchar **) g_slice_alloc (sizeof (gchar *) * GIMP_THUMB_SIZE_COUNT);
#else
  conn->thumb_sizes     = g_new (gint, GIMP_THUMB_SIZE_COUNT);
  conn->thumb_sizenames = (gchar **) g_new (gchar *, GIMP_THUMB_SIZE_COUNT);
  conn->thumb_subdirs   = (gchar **) g_new (gchar *, GIMP_THUMB_SIZE_COUNT);
#endif

  for (i = 0, enum_value = THUMB_SIZE_VALUES;
       i < GIMP_THUMB_SIZE_COUNT;
	   i++, enum_value++)
    {
      conn->thumb_sizes[i]     = enum_value->value;
      conn->thumb_sizenames[i] = g_strdup (enum_value->value_nick);
      conn->thumb_subdirs[i]   = g_build_filename (conn->thumb_dir,
                                 enum_value->value_nick, NULL);
    }

  conn->thumb_fail_subdir = conn->thumb_subdirs[0];
  conn->thumb_subdirs[0]  = g_build_filename (conn->thumb_fail_subdir, creator, NULL);

  return conn;
}

void
gimp_thumbconnection_destroy (GimpThumbConnection *conn)
{
  g_return_if_fail (GIMP_IS_THUMBCONNECTION (conn));
  gimp_thumbconnection_clear (conn);
#ifdef USE_GLIB2_10
  g_slice_free1 (sizeof (GimpThumbConnection), conn);
# else
  g_free (conn);
# endif
}

void
gimp_thumbconnection_set_basedir (GimpThumbConnection *conn,
                                  const gchar         *thumb_basedir)
{
  g_return_if_fail (thumb_basedir == NULL ||
                    g_path_is_absolute (thumb_basedir));
  g_free (conn->thumb_dir);

  if (thumb_basedir)
    {
      conn->thumb_dir = g_strdup (thumb_basedir);
    }
  else
    {
      const gchar *data_dir;
#ifdef USE_XDG_DIR
      data_dir = g_get_user_cache_dir (); //FDO thumbnail spec 0.8.0
      if (G_LIKELY(data_dir) &&
			(g_file_test (data_dir, G_FILE_TEST_IS_DIR)
			  || g_mkdir (data_dir, S_IRUSR | S_IWUSR | S_IXUSR) == 0))
            conn->thumb_dir = g_build_filename (data_dir, "thumbnails", NULL);
	  else
	    {
#endif
        data_dir = g_get_home_dir ();

        if (G_LIKELY(data_dir && g_file_test (data_dir, G_FILE_TEST_IS_DIR)))
          {
#ifdef USE_XDG_DIR
            conn->thumb_dir = g_build_filename (data_dir, ".cache", "thumbnails", NULL);
#else
            conn->thumb_dir = g_build_filename (data_dir, ".thumbnails", NULL);
#endif
          }
        else
          {
		    gchar *name, *creator;

            data_dir = g_get_tmp_dir ();
            name = g_filename_display_name (data_dir);

            printd (WARN, "Cannot determine a valid data directory.\n"
                       "Thumbnails will be stored in the folder for "
                       "temporary files (%s) instead", name);
            g_free (name);

		    creator = g_path_get_basename (conn->thumb_subdirs[0]);
		    name = g_strconcat ("thumbnails for ", creator, NULL);
            conn->thumb_dir = g_build_filename (data_dir, name, NULL);
		    g_free (creator);
		    g_free (name);
          }
#ifdef USE_XDG_DIR
        }
#endif
  }
}

gboolean
gimp_thumbconnection_ensure_thumb_dir (GimpThumbConnection *conn,
                                       gint       size,
                                       GError   **error)
{
  g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

  size = gimp_thumbconnection_size (conn, size);

  if (g_file_test (conn->thumb_subdirs[size], G_FILE_TEST_IS_DIR))
    return TRUE;

  if (g_file_test (conn->thumb_dir, G_FILE_TEST_IS_DIR) ||
      (g_mkdir (conn->thumb_dir, S_IRUSR | S_IWUSR | S_IXUSR) == 0))
    {
      if (size == 0)
        g_mkdir (conn->thumb_fail_subdir, S_IRUSR | S_IWUSR | S_IXUSR);

      g_mkdir (conn->thumb_subdirs[size], S_IRUSR | S_IWUSR | S_IXUSR);
    }

  if (g_file_test (conn->thumb_subdirs[size], G_FILE_TEST_IS_DIR))
    return TRUE;

  if (error != NULL)
    g_set_error (error,
               GIMP_THUMB_ERROR, GIMP_THUMB_ERROR_MKDIR,
               _("Failed to create thumbnail folder '%s'."),
               conn->thumb_subdirs[size]);

  return FALSE;
}

gchar *
gimp_thumbconnection_name_from_uri (GimpThumbConnection *conn,
                                    const gchar   *uri,
                                    gint           size)
{
  gchar *filename, *filepath;
  g_return_val_if_fail (uri != NULL, NULL);

  if (strstr (uri, conn->thumb_dir))
    return NULL;

  size = gimp_thumbconnection_size (conn, size);

  filename = _gimp_thumb_png_name (uri);
  filepath = g_build_filename (conn->thumb_subdirs[size], filename, NULL);
  g_free (filename);

  return filepath;
}

static guint
gimp_thumbconnection_size (GimpThumbConnection *conn, gint size)
{
  guint i = 0;

  if (size > 0)
    {
      for (i = 1;
           i < conn->thumb_num_sizes && conn->thumb_sizes[i] < size;
           i++)
        /* nothing */;

      if (i == conn->thumb_num_sizes)
        i--;
    }

  return i;
}

gchar *
_gimp_thumbconnection_png_lookup (GimpThumbConnection *conn,
                                  const gchar   *name,
                                  const gchar   *basedir,
                                  gint          *size)
{
  gchar  *thumb_path = NULL;
  gchar **subdirs;
  gint    width, height;
  guint   i, n;

  if (basedir) /* looking for a local thumb */
    {
      if (g_file_test (basedir, G_FILE_TEST_IS_DIR))
        {
          subdirs = g_new (gchar *, conn->thumb_num_sizes);

          subdirs[0] = NULL;  /*  skip GIMP_THUMB_SIZE_FAIL  */

          for (i = 1; i < conn->thumb_num_sizes; i++)
            subdirs[i] = g_build_filename (basedir, ".thumblocal",
                           conn->thumb_sizenames[i], NULL);
        }
      else
          subdirs = NULL;
    }
  else
    {
      subdirs = conn->thumb_subdirs;
#ifdef USE_XDG_DIR
      if (G_LIKELY(subdirs))
        {
          static gboolean converted = FALSE; /* don't care about multi-threads, for this */
          /* one-time move data from any old subdir to new one */
          if (!converted)
            {
              gchar *old_folder;
              GDir  *dir;
              old_folder = g_build_filename (g_get_home_dir (), ".thumbnails", NULL);
              dir = g_dir_open (old_folder, 0, NULL);

              if (dir)
                {
                  for (i = 0; i < conn->thumb_num_sizes; i++) /* ? skip failures i = 1 to ... */
                    {
                      GDir  *subdir;
                      gchar *subdir_path;

                      if (!subdirs[i])
                        continue; //CHECKME create it if old_folder subdir exists

                      subdir_path = g_build_filename (old_folder, conn->thumb_sizenames[i], NULL);
                      subdir = g_dir_open (subdir_path, 0, NULL);
                      if (subdir)
                      {
                        const gchar *thumb;
                        while ((thumb = g_dir_read_name (subdir)))
                        {
                          gchar *oldfile;

                          oldfile = g_build_filename (subdir_path, thumb, NULL);
                          if (g_file_test (oldfile, G_FILE_TEST_IS_REGULAR))
                          {
                            struct stat s;
                            gint64 mtime;
                            if (g_stat (oldfile, &s) == 0)
                            {
                               gchar *newfile;
                               mtime = s.st_mtime;
                               newfile = g_build_filename (subdirs[i], thumb, NULL);
                               if (g_stat(newfile, &s) == -1 || (gint64)s.st_mtime < mtime)
                                 g_rename (oldfile, newfile);
                               else
                                 g_unlink (oldfile);
                               g_free (newfile);
                             }
                           }
                           g_free (oldfile);
                        }
                      	g_dir_close (subdir);
                      }
                      g_remove (subdir_path);
                      g_free (subdir_path);
                    }
                    g_dir_close (dir);
                 }
              g_remove (old_folder);
      			  g_free (old_folder);
              converted = TRUE;
            }
        }
#endif
    }

  if (! subdirs)
    return NULL;

  n = gimp_thumbconnection_size (conn, *size);
  /* first look in dirs for same/bigger sizes */
  for (i = n; i < conn->thumb_num_sizes; i++)
    {
      if (! subdirs[i])
        continue;

      thumb_path = g_build_filename (subdirs[i], name, NULL);

      if (gimp_thumb_file_test (thumb_path,
                                NULL, NULL,
                                NULL) == GIMP_THUMB_FILE_TYPE_REGULAR)
        {
           if (gdk_pixbuf_get_file_info (thumb_path, &width, &height) != NULL
              && width <= *size && height <= *size)
             {
               *size = conn->thumb_sizes[i];
               goto finish;
             }
        }

      g_free (thumb_path);
    }
  /* then look in dirs (except failed) for smaller sizes */
  for (i = n - 1; i > 0; i--) /* guint , so NOT >= 0 */
    {
      if (! subdirs[i])
        continue;

      thumb_path = g_build_filename (subdirs[i], name, NULL);

      if (gimp_thumb_file_test (thumb_path,
                                NULL, NULL,
                                NULL) == GIMP_THUMB_FILE_TYPE_REGULAR)
        {
           if (gdk_pixbuf_get_file_info (thumb_path, &width, &height) != NULL
              && width <= *size && height <= *size)
             {
               *size = conn->thumb_sizes[i];
               goto finish;
             }
        }

      g_free (thumb_path);
    }

  thumb_path = NULL;

finish:
  if (basedir)
    {
      for (i = 0; i < conn->thumb_num_sizes; i++)
        g_free (subdirs[i]);
      g_free (subdirs);
    }

  return thumb_path;
}

//===== FROM UTILS =====

gchar *
gimp_thumb_find_thumb (GimpThumbConnection *conn,
                       const gchar   *uri,
                       gint          *size)
{
  gchar *filename;
  gchar *result;

  g_return_val_if_fail (uri != NULL, NULL);
  g_return_val_if_fail (size != NULL, NULL);
  g_return_val_if_fail (*size > GIMP_THUMB_SIZE_FAIL, NULL);

  filename = _gimp_thumb_png_name (uri);
  result = _gimp_thumbconnection_png_lookup (conn, filename, NULL, size);
  g_free (filename);

  if (!result)
    {
      gchar *filepath = _gimp_thumb_filepath_from_uri (uri);

      if (filepath)
        {
          const gchar *baseuri = strrchr (uri, '/');

          if (baseuri && baseuri[1])
            {
			  gchar *dirname = g_path_get_dirname (filepath);
			  filename = _gimp_thumb_png_name (baseuri + 1);

              result = _gimp_thumbconnection_png_lookup (conn, filename, dirname, size);

              g_free (dirname);
              g_free (filename);
            }

          g_free (filepath);
        }
    }

  return result;
}

GimpThumbFileType
gimp_thumb_file_test (const gchar *filepath,
                      gint64      *mtime,
                      gint64      *size,
                      gint        *err_no)
{
  if (filepath != NULL)
  {
    struct stat s;

    if (g_stat (filepath, &s) == 0)
	  {
      if (mtime)  *mtime  = s.st_mtime;
      if (size)   *size   = s.st_size;
      if (err_no) *err_no = 0;

      if (S_ISREG (s.st_mode))
      {
        return GIMP_THUMB_FILE_TYPE_REGULAR;
      }
      else if (S_ISDIR (s.st_mode))
      {
        return GIMP_THUMB_FILE_TYPE_FOLDER;
      }

      return GIMP_THUMB_FILE_TYPE_SPECIAL;
    }
  }
  else
    errno = 0;

  if (mtime)  *mtime  = 0;
  if (size)   *size   = 0;
  if (err_no) *err_no = errno;

  g_return_val_if_fail (filepath != NULL, GIMP_THUMB_FILE_TYPE_NONE);

  return GIMP_THUMB_FILE_TYPE_NONE;
}

gchar *
_gimp_thumb_filepath_from_uri (const gchar *uri)
{
  gchar *filename;
  gchar *hostname;

  g_return_val_if_fail (uri != NULL, NULL);

  filename = g_filename_from_uri (uri, &hostname, NULL);

  if (!filename)
    return NULL;

  if (hostname)
    {
      /*  we have a file: URI with a hostname      */
      /*  return NULL, caller should use URI then  */
      g_free (filename);
      filename = NULL;
      g_free (hostname);
    }

  return filename;
}

#ifndef USE_GLIB2_16
/********* start of borrowed code *********/

/*
This code implements the MD5 message-digest algorithm, by Ron Rivest.
This code was written by Colin Plumb in 1993, our understanding is that no
copyright is claimed and that this code is in the public domain.

Equivalent code is available from RSA Data Security, Inc.
This code has been tested against that, and is functionally equivalent.
(It is also functionally equivalent to the approach used by glib >= 2.16.)

To compute the message digest of a chunk of bytes, declare an
MD5Context structure, pass it to MD5Init, call MD5Update as
needed on buffers full of bytes, and then call MD5Final, which
will fill a supplied 16-byte array with the digest.
*/

struct MD5Context {
	u_int32_t buf[4];
	u_int32_t bits[2];
	unsigned char in[64];
};

//void MD5Init(struct MD5Context *context);
//void MD5Update(struct MD5Context *context, unsigned char const *buf, unsigned len);
//void MD5Final(unsigned char digest[16], struct MD5Context *context);
//void MD5Transform(u_int32_t buf[4], u_int32_t const in[16]);

/* MD5 setup requires knowing if we're big- or little-endian */
#if defined(__linux) || defined(__linux__)
# include <endian.h>
# if !defined(BYTE_ORDER) && defined(__BYTE_ORDER)
#  define BYTE_ORDER __BYTE_ORDER
# endif

#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
# include <sys/endian.h>
# if !defined(BYTE_ORDER) && defined(_BYTE_ORDER)
#  define BYTE_ORDER _BYTE_ORDER
# endif

#elif defined(__LINUX) //CHECKME
# ifndef __USE_BSD
#  define __USE_BSD
# endif
# include <endian.h>
# if !defined(BYTE_ORDER) && defined(__BYTE_ORDER)
#  define BYTE_ORDER __BYTE_ORDER
# endif

#elif defined (__SOLARIS)

# include <sys/isa_defs.h>
# ifdef _BIG_ENDIAN
#  define BYTE_ORDER 4321
# else
#  define BYTE_ORDER 1234
# endif

#elif defined (__APPLE__)
# include <machine/endian.h>

#endif

#ifndef BIG_ENDIAN
# define BIG_ENDIAN    4321
#endif
#ifndef LITTLE_ENDIAN
# define LITTLE_ENDIAN 1234
#endif

#if BYTE_ORDER == BIG_ENDIAN
# define HIGHFIRST
// For Tiger
# define BIG_ENDIAN_HOST
#endif //BYTE_ORDER == BIG_ENDIAN

static void MD5Transform(u_int32_t buf[4], u_int32_t const in[16]);

#ifdef HIGHFIRST
# ifdef ASM_MD5
void byteReverse(unsigned char *buf, unsigned longs);
# else
/*
Note: this code is harmless on little-endian machines.
*/
static void byteReverse(unsigned char *buf, unsigned longs)
{
	u_int32_t t;
	do {
	t = (u_int32_t) ((unsigned) buf[3] << 8 | buf[2]) << 16 |
		((unsigned) buf[1] << 8 | buf[0]);
	*(u_int32_t *) buf = t;
	buf += 4;
	} while (--longs);
}
# endif
#else
# define byteReverse(buf,len)	/* Nothing */
#endif

/*
Start MD5 accumulation.  Set bit count to 0 and buffer to mysterious
initialization constants.
*/
static void MD5Init(struct MD5Context *ctx)
{
	ctx->buf[0] = 0x67452301;
	ctx->buf[1] = 0xefcdab89;
	ctx->buf[2] = 0x98badcfe;
	ctx->buf[3] = 0x10325476;

	ctx->bits[0] = 0;
	ctx->bits[1] = 0;
}

/*
Update context to reflect the concatenation of another buffer full of bytes.
*/
static void MD5Update(struct MD5Context *ctx, unsigned char const *buf, unsigned len)
{
	u_int32_t t;

	/* Update bitcount */

	t = ctx->bits[0];
	if ((ctx->bits[0] = t + ((u_int32_t) len << 3)) < t)
	ctx->bits[1]++;		/* Carry from low to high */
	ctx->bits[1] += len >> 29;

	t = (t >> 3) & 0x3f;	/* Bytes already in shsInfo->data */

	/* Handle any leading odd-sized chunks */

	if (t) {
	unsigned char *p = (unsigned char *) ctx->in + t;

	t = 64 - t;
	if (len < t) {
		memcpy(p, buf, len);
		return;
	}
	memcpy(p, buf, t);
	byteReverse(ctx->in, 16);
	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);
	buf += t;
	len -= t;
	}
	/* Process data in 64-byte chunks */

	while (len >= 64) {
	memcpy(ctx->in, buf, 64);
	byteReverse(ctx->in, 16);
	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);
	buf += 64;
	len -= 64;
	}

	/* Handle any remaining bytes of data. */

	memcpy(ctx->in, buf, len);
}

/*
Final wrapup - pad to 64-byte boundary with the bit pattern 1 0*
(64-bit count of bits processed, MSB-first)
*/
static void MD5Final(unsigned char digest[16], struct MD5Context *ctx)
{
	unsigned count;
	unsigned char *p;

	/* Compute number of bytes mod 64 */
	count = (ctx->bits[0] >> 3) & 0x3F;

	/* Set the first char of padding to 0x80.  This is safe since there is
	   always at least one byte free */
	p = ctx->in + count;
	*p++ = 0x80;

	/* Bytes of padding needed to make 64 bytes */
	count = 64 - 1 - count;

	/* Pad out to 56 mod 64 */
	if (count < 8) {
	/* Two lots of padding:  Pad the first block to 64 bytes */
	memset(p, 0, count);
	byteReverse(ctx->in, 16);
	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);

	/* Now fill the next block with 56 bytes */
	memset(ctx->in, 0, 56);
	} else {
	/* Pad block to 56 bytes */
	memset(p, 0, count - 8);
	}
	byteReverse(ctx->in, 14);

	/* Append length in bits and transform */
	((u_int32_t *) ctx->in)[14] = ctx->bits[0];
	((u_int32_t *) ctx->in)[15] = ctx->bits[1];

	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);
	byteReverse((unsigned char *) ctx->buf, 4);
	memcpy(digest, ctx->buf, 16);

	memset(ctx, 0, sizeof(* ctx));	/* In case it's sensitive */
	/* The original version of this code omitted the asterisk. In
	   effect, only the first part of ctx was wiped with zeros, not
	   the whole thing. Bug found by Derek Jones. Original line: */
	// memset(ctx, 0, sizeof(ctx));	/* In case it's sensitive */
}

#ifndef ASM_MD5

/* The four core functions - F1 is optimized somewhat */

#define MD5STEP(f, w, x, y, z, data, s) \
	( w += f(x, y, z) + data,  w = w<<s | w>>(32-s),  w += x )
/* #define F1(x, y, z) (x & y | ~x & z) */
# define F1(x, y, z) (z ^ (x & (y ^ z)))
# define F2(x, y, z) F1(z, x, y)
# define F3(x, y, z) (x ^ y ^ z)
# define F4(x, y, z) (y ^ (x | ~z))

/*
The core of the MD5 algorithm, this alters an existing MD5 hash to
reflect the addition of 16 longwords of new data.  MD5Update blocks
the data and converts bytes into longwords for this routine.
*/
static void MD5Transform(u_int32_t buf[4], u_int32_t const in[16])
{
	register u_int32_t a, b, c, d;

	a = buf[0];
	b = buf[1];
	c = buf[2];
	d = buf[3];

	MD5STEP(F1, a, b, c, d, in[0] + 0xd76aa478, 7);
	MD5STEP(F1, d, a, b, c, in[1] + 0xe8c7b756, 12);
	MD5STEP(F1, c, d, a, b, in[2] + 0x242070db, 17);
	MD5STEP(F1, b, c, d, a, in[3] + 0xc1bdceee, 22);
	MD5STEP(F1, a, b, c, d, in[4] + 0xf57c0faf, 7);
	MD5STEP(F1, d, a, b, c, in[5] + 0x4787c62a, 12);
	MD5STEP(F1, c, d, a, b, in[6] + 0xa8304613, 17);
	MD5STEP(F1, b, c, d, a, in[7] + 0xfd469501, 22);
	MD5STEP(F1, a, b, c, d, in[8] + 0x698098d8, 7);
	MD5STEP(F1, d, a, b, c, in[9] + 0x8b44f7af, 12);
	MD5STEP(F1, c, d, a, b, in[10] + 0xffff5bb1, 17);
	MD5STEP(F1, b, c, d, a, in[11] + 0x895cd7be, 22);
	MD5STEP(F1, a, b, c, d, in[12] + 0x6b901122, 7);
	MD5STEP(F1, d, a, b, c, in[13] + 0xfd987193, 12);
	MD5STEP(F1, c, d, a, b, in[14] + 0xa679438e, 17);
	MD5STEP(F1, b, c, d, a, in[15] + 0x49b40821, 22);

	MD5STEP(F2, a, b, c, d, in[1] + 0xf61e2562, 5);
	MD5STEP(F2, d, a, b, c, in[6] + 0xc040b340, 9);
	MD5STEP(F2, c, d, a, b, in[11] + 0x265e5a51, 14);
	MD5STEP(F2, b, c, d, a, in[0] + 0xe9b6c7aa, 20);
	MD5STEP(F2, a, b, c, d, in[5] + 0xd62f105d, 5);
	MD5STEP(F2, d, a, b, c, in[10] + 0x02441453, 9);
	MD5STEP(F2, c, d, a, b, in[15] + 0xd8a1e681, 14);
	MD5STEP(F2, b, c, d, a, in[4] + 0xe7d3fbc8, 20);
	MD5STEP(F2, a, b, c, d, in[9] + 0x21e1cde6, 5);
	MD5STEP(F2, d, a, b, c, in[14] + 0xc33707d6, 9);
	MD5STEP(F2, c, d, a, b, in[3] + 0xf4d50d87, 14);
	MD5STEP(F2, b, c, d, a, in[8] + 0x455a14ed, 20);
	MD5STEP(F2, a, b, c, d, in[13] + 0xa9e3e905, 5);
	MD5STEP(F2, d, a, b, c, in[2] + 0xfcefa3f8, 9);
	MD5STEP(F2, c, d, a, b, in[7] + 0x676f02d9, 14);
	MD5STEP(F2, b, c, d, a, in[12] + 0x8d2a4c8a, 20);

	MD5STEP(F3, a, b, c, d, in[5] + 0xfffa3942, 4);
	MD5STEP(F3, d, a, b, c, in[8] + 0x8771f681, 11);
	MD5STEP(F3, c, d, a, b, in[11] + 0x6d9d6122, 16);
	MD5STEP(F3, b, c, d, a, in[14] + 0xfde5380c, 23);
	MD5STEP(F3, a, b, c, d, in[1] + 0xa4beea44, 4);
	MD5STEP(F3, d, a, b, c, in[4] + 0x4bdecfa9, 11);
	MD5STEP(F3, c, d, a, b, in[7] + 0xf6bb4b60, 16);
	MD5STEP(F3, b, c, d, a, in[10] + 0xbebfbc70, 23);
	MD5STEP(F3, a, b, c, d, in[13] + 0x289b7ec6, 4);
	MD5STEP(F3, d, a, b, c, in[0] + 0xeaa127fa, 11);
	MD5STEP(F3, c, d, a, b, in[3] + 0xd4ef3085, 16);
	MD5STEP(F3, b, c, d, a, in[6] + 0x04881d05, 23);
	MD5STEP(F3, a, b, c, d, in[9] + 0xd9d4d039, 4);
	MD5STEP(F3, d, a, b, c, in[12] + 0xe6db99e5, 11);
	MD5STEP(F3, c, d, a, b, in[15] + 0x1fa27cf8, 16);
	MD5STEP(F3, b, c, d, a, in[2] + 0xc4ac5665, 23);

	MD5STEP(F4, a, b, c, d, in[0] + 0xf4292244, 6);
	MD5STEP(F4, d, a, b, c, in[7] + 0x432aff97, 10);
	MD5STEP(F4, c, d, a, b, in[14] + 0xab9423a7, 15);
	MD5STEP(F4, b, c, d, a, in[5] + 0xfc93a039, 21);
	MD5STEP(F4, a, b, c, d, in[12] + 0x655b59c3, 6);
	MD5STEP(F4, d, a, b, c, in[3] + 0x8f0ccc92, 10);
	MD5STEP(F4, c, d, a, b, in[10] + 0xffeff47d, 15);
	MD5STEP(F4, b, c, d, a, in[1] + 0x85845dd1, 21);
	MD5STEP(F4, a, b, c, d, in[8] + 0x6fa87e4f, 6);
	MD5STEP(F4, d, a, b, c, in[15] + 0xfe2ce6e0, 10);
	MD5STEP(F4, c, d, a, b, in[6] + 0xa3014314, 15);
	MD5STEP(F4, b, c, d, a, in[13] + 0x4e0811a1, 21);
	MD5STEP(F4, a, b, c, d, in[4] + 0xf7537e82, 6);
	MD5STEP(F4, d, a, b, c, in[11] + 0xbd3af235, 10);
	MD5STEP(F4, c, d, a, b, in[2] + 0x2ad7d2bb, 15);
	MD5STEP(F4, b, c, d, a, in[9] + 0xeb86d391, 21);

	buf[0] += a;
	buf[1] += b;
	buf[2] += c;
	buf[3] += d;
}

#endif

/*************** end of borrowed code ***************/
#endif /* ndef USE_GLIB2_16 */

gchar *
_gimp_thumb_png_name (const gchar *uri)
{
  guchar     digest[16];
  gchar      name[36]; /* twice the size of digest, + 4 */
  gint       i;
#ifdef USE_GLIB2_16
  gsize      len;
  GChecksum *checksum;

  checksum = g_checksum_new (G_CHECKSUM_MD5);
  g_checksum_update (checksum, (const guchar *) uri, -1);
  len = sizeof (digest);
  g_checksum_get_digest (checksum, digest, &len);
  g_checksum_free (checksum);
#else
  struct MD5Context md;

  MD5Init (&md);
  MD5Update (&md, (const guchar *)uri, strlen (uri));
  MD5Final (digest, &md);
#endif

  for (i = 0; i < sizeof (digest); i++)
    {
      guchar n;

      n = (digest[i] >> 4) & 0xF;
      name[i * 2]     = (n > 9) ? 'a' + n - 10 : '0' + n;

      n = digest[i] & 0xF;
      name[i * 2 + 1] = (n > 9) ? 'a' + n - 10 : '0' + n;
    }
  memcpy (name + 32, ".png", 4); /* 32 = sizeof (digest)*2 */

  return g_strndup (name, 36);
}

//===== FROM ERROR =====

GQuark
gimp_thumb_error_quark (void)
{
  return g_quark_from_static_string ("gimp-thumb-error-quark");
}

