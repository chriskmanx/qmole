/* 
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

#ifndef __GCR_CERTIFICATE_DETAILS_WIDGET_H__
#define __GCR_CERTIFICATE_DETAILS_WIDGET_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-certificate.h"
#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_TYPE_CERTIFICATE_DETAILS_WIDGET               (gcr_certificate_details_widget_get_type ())
#define GCR_CERTIFICATE_DETAILS_WIDGET(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_CERTIFICATE_DETAILS_WIDGET, GcrCertificateDetailsWidget))
#define GCR_CERTIFICATE_DETAILS_WIDGET_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_CERTIFICATE_DETAILS_WIDGET, GcrCertificateDetailsWidgetClass))
#define GCR_IS_CERTIFICATE_DETAILS_WIDGET(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_CERTIFICATE_DETAILS_WIDGET))
#define GCR_IS_CERTIFICATE_DETAILS_WIDGET_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_CERTIFICATE_DETAILS_WIDGET))
#define GCR_CERTIFICATE_DETAILS_WIDGET_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_CERTIFICATE_DETAILS_WIDGET, GcrCertificateDetailsWidgetClass))

typedef struct _GcrCertificateDetailsWidget GcrCertificateDetailsWidget;
typedef struct _GcrCertificateDetailsWidgetClass GcrCertificateDetailsWidgetClass;
typedef struct _GcrCertificateDetailsWidgetPrivate GcrCertificateDetailsWidgetPrivate;
    
struct _GcrCertificateDetailsWidget {
	GtkAlignment parent;
	GcrCertificateDetailsWidgetPrivate *pv;
};

struct _GcrCertificateDetailsWidgetClass {
	GtkAlignmentClass parent_class;
};

GType                         gcr_certificate_details_widget_get_type               (void);

GcrCertificateDetailsWidget*  gcr_certificate_details_widget_new                    (GcrCertificate *cert);

GcrCertificate*               gcr_certificate_details_widget_get_certificate        (GcrCertificateDetailsWidget *details);

void                          gcr_certificate_details_widget_set_certificate        (GcrCertificateDetailsWidget *details, 
                                                                                     GcrCertificate *cert);

G_END_DECLS

#endif /* __GCR_CERTIFICATE_DETAILS_WIDGET_H__ */
