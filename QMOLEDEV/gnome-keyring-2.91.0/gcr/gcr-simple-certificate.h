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

#ifndef __GCR_SIMPLE_CERTIFICATE_H__
#define __GCR_SIMPLE_CERTIFICATE_H__

#include "gcr.h"

#include <glib-object.h>

G_BEGIN_DECLS

#define GCR_TYPE_SIMPLE_CERTIFICATE               (gcr_simple_certificate_get_type ())
#define GCR_SIMPLE_CERTIFICATE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_CERTIFICATE, GcrSimpleCertificate))
#define GCR_SIMPLE_CERTIFICATE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_CERTIFICATE, GcrSimpleCertificateClass))
#define GCR_IS_SIMPLE_CERTIFICATE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_CERTIFICATE))
#define GCR_IS_SIMPLE_CERTIFICATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_CERTIFICATE))
#define GCR_SIMPLE_CERTIFICATE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_CERTIFICATE, GcrSimpleCertificateClass))

typedef struct _GcrSimpleCertificate GcrSimpleCertificate;
typedef struct _GcrSimpleCertificateClass GcrSimpleCertificateClass;
typedef struct _GcrSimpleCertificatePrivate GcrSimpleCertificatePrivate;

struct _GcrSimpleCertificate {
	GObject parent;
	GcrSimpleCertificatePrivate *pv;
};

struct _GcrSimpleCertificateClass {
	GObjectClass parent_class;
};

GType               gcr_simple_certificate_get_type               (void);

GcrCertificate*     gcr_simple_certificate_new                    (const guchar *data,
                                                                   gsize n_data);

G_END_DECLS

#endif /* __GCR_SIMPLE_CERTIFICATE_H__ */
