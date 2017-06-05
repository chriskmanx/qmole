/*
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#if !defined (__GCR_H_INSIDE__) && !defined (GCR_COMPILATION)
#error "Only <gcr/gcr.h> can be included directly."
#endif

#ifndef __GCR_CERTIFICATE_EXPORTER_H__
#define __GCR_CERTIFICATE_EXPORTER_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_TYPE_CERTIFICATE_EXPORTER               (_gcr_certificate_exporter_get_type ())
#define GCR_CERTIFICATE_EXPORTER(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_CERTIFICATE_EXPORTER, GcrCertificateExporter))
#define GCR_CERTIFICATE_EXPORTER_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_CERTIFICATE_EXPORTER, GcrCertificateExporterClass))
#define GCR_IS_CERTIFICATE_EXPORTER(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_CERTIFICATE_EXPORTER))
#define GCR_IS_CERTIFICATE_EXPORTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_CERTIFICATE_EXPORTER))
#define GCR_CERTIFICATE_EXPORTER_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_CERTIFICATE_EXPORTER, GcrCertificateExporterClass))

typedef struct _GcrCertificateExporter GcrCertificateExporter;
typedef struct _GcrCertificateExporterClass GcrCertificateExporterClass;
typedef struct _GcrCertificateExporterPrivate GcrCertificateExporterPrivate;

struct _GcrCertificateExporter {
	GObject parent;
	GcrCertificateExporterPrivate *pv;
};

struct _GcrCertificateExporterClass {
	GObjectClass parent_class;
};

GType                     _gcr_certificate_exporter_get_type          (void);

GcrCertificateExporter*   _gcr_certificate_exporter_new               (GcrCertificate *certificate,
                                                                       const gchar *label,
                                                                       GtkWindow *transient_for);

void                      _gcr_certificate_exporter_export_async      (GcrCertificateExporter *self,
                                                                       GCancellable *cancellable,
                                                                       GAsyncReadyCallback callback,
                                                                       gpointer user_data);

gboolean                  _gcr_certificate_exporter_export_finish     (GcrCertificateExporter *self,
                                                                       GAsyncResult *result,
                                                                       GError **error);

G_END_DECLS

#endif /* __GCR_CERTIFICATE_EXPORTER_H__ */
