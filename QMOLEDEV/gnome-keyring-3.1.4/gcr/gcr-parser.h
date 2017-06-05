/* 
 * gnome-keyring
 * 
 * Copyright (C) 2008 Stefan Walter
 * 
 * This program is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *  
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  
 */

#if !defined (__GCR_H_INSIDE__) && !defined (GCR_COMPILATION)
#error "Only <gcr/gcr.h> can be included directly."
#endif

#ifndef __GCR_PARSER_H__
#define __GCR_PARSER_H__

#include <glib-object.h>
#include <gio/gio.h>

#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_TYPE_PARSER               (gcr_parser_get_type ())
#define GCR_PARSER(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_PARSER, GcrParser))
#define GCR_PARSER_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_PARSER, GcrParserClass))
#define GCR_IS_PARSER(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_PARSER))
#define GCR_IS_PARSER_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_PARSER))
#define GCR_PARSER_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_PARSER, GcrParserClass))

typedef struct _GcrParser GcrParser;
typedef struct _GcrParserClass GcrParserClass;
typedef struct _GcrParserPrivate GcrParserPrivate;

struct _GcrParser {
	GObject parent;

	/*< private >*/
	GcrParserPrivate *pv;
};

struct _GcrParserClass {
	GObjectClass parent_class;

	/* signals --------------------------------------------------------- */

	/* A callback for each password needed */
	gboolean (*authenticate) (GcrParser *self, gint count);
	
	void     (*parsed) (GcrParser *self);
};

GType                    gcr_parser_get_type               (void);

GcrParser*               gcr_parser_new                    (void);

void                     gcr_parser_format_enable          (GcrParser *self,
                                                            gint format_id);

void                     gcr_parser_format_disable         (GcrParser *self,
                                                            gint format_id);

gboolean                 gcr_parser_format_supported       (GcrParser *self,
                                                            gint format_id);

gboolean                 gcr_parser_parse_data             (GcrParser *self, 
                                                            gconstpointer data,
                                                            gsize n_data, 
                                                            GError **error);

gboolean                 gcr_parser_parse_stream           (GcrParser *self,
                                                            GInputStream *input,
                                                            GCancellable *cancellable,
                                                            GError **error);

void                     gcr_parser_parse_stream_async     (GcrParser *self,
                                                            GInputStream *input,
                                                            GCancellable *cancellable,
                                                            GAsyncReadyCallback callback,
                                                            gpointer user_data);

gboolean                 gcr_parser_parse_stream_finish    (GcrParser *self,
                                                            GAsyncResult *result,
                                                            GError **error);

void                     gcr_parser_add_password           (GcrParser *self,
                                                            const gchar *password);

const gchar*             gcr_parser_get_parsed_label       (GcrParser *self);

const gchar*             gcr_parser_get_parsed_description (GcrParser *self);

GckAttributes*           gcr_parser_get_parsed_attributes  (GcrParser *self);

G_END_DECLS

#endif /* __GCR_PARSER_H__ */
