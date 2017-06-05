/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Collabora Ltd
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

#ifndef __GCR_CERTIFICATE_CHAIN_H__
#define __GCR_CERTIFICATE_CHAIN_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-certificate.h"
#include "gcr-types.h"

G_BEGIN_DECLS

typedef enum {
	GCR_CERTIFICATE_CHAIN_UNKNOWN,
	GCR_CERTIFICATE_CHAIN_INCOMPLETE,
	GCR_CERTIFICATE_CHAIN_DISTRUSTED,
	GCR_CERTIFICATE_CHAIN_SELFSIGNED,
	GCR_CERTIFICATE_CHAIN_PINNED,
	GCR_CERTIFICATE_CHAIN_ANCHORED,
} GcrCertificateChainStatus;

typedef enum {
	GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS = 1 << 0,
} GcrCertificateChainFlags;

#define GCR_TYPE_CERTIFICATE_CHAIN               (gcr_certificate_chain_get_type ())
#define GCR_CERTIFICATE_CHAIN(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_CERTIFICATE_CHAIN, GcrCertificateChain))
#define GCR_CERTIFICATE_CHAIN_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_CERTIFICATE_CHAIN, GcrCertificateChainClass))
#define GCR_IS_CERTIFICATE_CHAIN(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_CERTIFICATE_CHAIN))
#define GCR_IS_CERTIFICATE_CHAIN_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_CERTIFICATE_CHAIN))
#define GCR_CERTIFICATE_CHAIN_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_CERTIFICATE_CHAIN, GcrCertificateChainClass))

typedef struct _GcrCertificateChain GcrCertificateChain;
typedef struct _GcrCertificateChainClass GcrCertificateChainClass;
typedef struct _GcrCertificateChainPrivate GcrCertificateChainPrivate;

struct _GcrCertificateChain {
	GObject parent;

	/*< private >*/
	GcrCertificateChainPrivate *pv;
};

struct _GcrCertificateChainClass {
	GObjectClass parent_class;
};

GType                     gcr_certificate_chain_get_type           (void) G_GNUC_CONST;

GcrCertificateChain*      gcr_certificate_chain_new                (void);

void                      gcr_certificate_chain_add                (GcrCertificateChain *self,
                                                                    GcrCertificate *certificate);

GcrCertificateChainStatus gcr_certificate_chain_get_status         (GcrCertificateChain *self);

GcrCertificate*           gcr_certificate_chain_get_anchor         (GcrCertificateChain *self);

GcrCertificate*           gcr_certificate_chain_get_endpoint       (GcrCertificateChain *self);

guint                     gcr_certificate_chain_get_length         (GcrCertificateChain *self);

GcrCertificate*           gcr_certificate_chain_get_certificate    (GcrCertificateChain *self,
                                                                    guint index);

gboolean                  gcr_certificate_chain_build              (GcrCertificateChain *self,
                                                                    const gchar *purpose,
                                                                    const gchar *peer,
                                                                    guint flags,
                                                                    GCancellable *cancellable,
                                                                    GError **error);

void                      gcr_certificate_chain_build_async        (GcrCertificateChain *self,
                                                                    const gchar *purpose,
                                                                    const gchar *peer,
                                                                    guint flags,
                                                                    GCancellable *cancellable,
                                                                    GAsyncReadyCallback callback,
                                                                    gpointer user_data);

gboolean                  gcr_certificate_chain_build_finish       (GcrCertificateChain *self,
                                                                    GAsyncResult *result,
                                                                    GError **error);

G_END_DECLS

#endif /* __GCR_CERTIFICATE_CHAIN_H__ */
