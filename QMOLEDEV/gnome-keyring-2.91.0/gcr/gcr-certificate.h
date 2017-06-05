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

#ifndef __GCR_CERTIFICATE_H__
#define __GCR_CERTIFICATE_H__

#include "gcr-types.h"

#include <glib-object.h>

G_BEGIN_DECLS

#define GCR_TYPE_CERTIFICATE                 (gcr_certificate_get_type())
#define GCR_CERTIFICATE(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_CERTIFICATE, GcrCertificate))
#define GCR_IS_CERTIFICATE(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_CERTIFICATE))
#define GCR_CERTIFICATE_GET_INTERFACE(inst)  (G_TYPE_INSTANCE_GET_INTERFACE ((inst), GCR_TYPE_CERTIFICATE, GcrCertificateIface))

typedef struct _GcrCertificate      GcrCertificate;
typedef struct _GcrCertificateIface GcrCertificateIface;

struct _GcrCertificateIface {
	GTypeInterface parent;
	
	const guchar* (*get_der_data)   (GcrCertificate *self, gsize *n_data);
	
	gpointer dummy1;
	gpointer dummy2;
	gpointer dummy3;
	gpointer dummy5;
	gpointer dummy6;
	gpointer dummy7;
	gpointer dummy8;
};

GType               gcr_certificate_get_type               (void);

const guchar*       gcr_certificate_get_der_data           (GcrCertificate *self, 
                                                            gsize *n_data);

gchar*              gcr_certificate_get_issuer_cn          (GcrCertificate *self);

gchar*              gcr_certificate_get_issuer_dn          (GcrCertificate *self);

gchar*              gcr_certificate_get_issuer_part        (GcrCertificate *self, 
                                                            const gchar *part);

gchar*              gcr_certificate_get_subject_cn         (GcrCertificate *self);

gchar*              gcr_certificate_get_subject_dn         (GcrCertificate *self);

gchar*              gcr_certificate_get_subject_part       (GcrCertificate *self, 
                                                            const gchar *part);

GDate*              gcr_certificate_get_issued_date        (GcrCertificate *self);

GDate*              gcr_certificate_get_expiry_date        (GcrCertificate *self);

guchar*             gcr_certificate_get_serial_number      (GcrCertificate *self, 
                                                            gsize *n_length);

gchar*              gcr_certificate_get_serial_number_hex  (GcrCertificate *self);

guint               gcr_certificate_get_key_size           (GcrCertificate *self);

guchar*             gcr_certificate_get_fingerprint        (GcrCertificate *self, 
                                                            GChecksumType type, 
                                                            gsize *n_length);

gchar*              gcr_certificate_get_fingerprint_hex    (GcrCertificate *self, 
                                                            GChecksumType type);

G_END_DECLS

#endif /* __GCR_CERTIFICATE_H__ */
