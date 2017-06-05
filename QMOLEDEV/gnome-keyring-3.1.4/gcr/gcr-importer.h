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

#ifndef __GCR_IMPORTER_H__
#define __GCR_IMPORTER_H__

#include "gcr-parser.h"
#include "gcr-types.h"

#include <glib-object.h>

G_BEGIN_DECLS

typedef enum {
	GCR_IMPORTER_PROMPT_NEEDED,
	GCR_IMPORTER_PROMPT_ALWAYS,
	GCR_IMPORTER_PROMPT_NEVER
} GcrImporterPromptBehavior;

#define GCR_TYPE_IMPORTER               (gcr_importer_get_type ())
#define GCR_IMPORTER(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_IMPORTER, GcrImporter))
#define GCR_IMPORTER_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_IMPORTER, GcrImporterClass))
#define GCR_IS_IMPORTER(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_IMPORTER))
#define GCR_IS_IMPORTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_IMPORTER))
#define GCR_IMPORTER_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_IMPORTER, GcrImporterClass))

typedef struct _GcrImporter GcrImporter;
typedef struct _GcrImporterClass GcrImporterClass;
typedef struct _GcrImporterPrivate GcrImporterPrivate;

struct _GcrImporter {
	GObject parent;

	/*< private >*/
	GcrImporterPrivate *pv;
};

struct _GcrImporterClass {
	GObjectClass parent_class;

	/* signals */
	void (*queued) (GcrImporter *self, const gchar *label, GckAttributes *attrs);
	void (*imported) (GcrImporter *self, GckObject *object);
};

GType                     gcr_importer_get_type               (void);

GcrImporter*              gcr_importer_new                    (void);

GckSlot*                  gcr_importer_get_slot               (GcrImporter *self);

void                      gcr_importer_set_slot               (GcrImporter *self,
                                                               GckSlot *slot);

GcrImporterPromptBehavior gcr_importer_get_prompt_behavior    (GcrImporter *self);

void                      gcr_importer_set_prompt_behavior    (GcrImporter *self,
                                                               GcrImporterPromptBehavior behavior);

void                      gcr_importer_queue                  (GcrImporter *self,
                                                               const gchar *label,
                                                               GckAttributes *attrs);

void                      gcr_importer_listen                 (GcrImporter *self,
                                                               GcrParser *parser);

gboolean                  gcr_importer_import                 (GcrImporter *self,
                                                               GCancellable *cancellable,
                                                               GError **error);

void                      gcr_importer_import_async           (GcrImporter *self,
                                                               GCancellable *cancellable,
                                                               GAsyncReadyCallback callback,
                                                               gpointer user_data);

gboolean                  gcr_importer_import_finish          (GcrImporter *self,
                                                               GAsyncResult *result,
                                                               GError **error);

#ifndef GCR_DISABLE_DEPRECATED

GcrParser*                gcr_importer_get_parser             (GcrImporter *self);

void                      gcr_importer_set_parser             (GcrImporter *self,
                                                               GcrParser *parser);

#endif /* GCR_DISABLE_DEPRECATED */

G_END_DECLS

#endif /* __GCR_IMPORTER_H__ */
