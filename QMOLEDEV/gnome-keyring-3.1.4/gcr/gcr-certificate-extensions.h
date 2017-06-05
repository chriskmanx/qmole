/*
 * gnome-keyring
 *
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

#ifndef GCR_CERTIFICATE_EXTENSIONS_H
#define GCR_CERTIFICATE_EXTENSIONS_H

#include <glib.h>

G_BEGIN_DECLS

gboolean   _gcr_certificate_extension_basic_constraints       (gconstpointer data,
                                                               gsize n_data,
                                                               gboolean *is_ca,
                                                               gint *path_len);

GQuark*    _gcr_certificate_extension_extended_key_usage      (gconstpointer data,
                                                               gsize n_data);

gpointer   _gcr_certificate_extension_subject_key_identifier  (gconstpointer data,
                                                               gsize n_data,
                                                               gsize *n_keyid);

typedef enum {
	GCR_KEY_USAGE_DIGITAL_SIGNATURE = 1 << 0,
	GCR_KEY_USAGE_NON_REPUDIATION = 1 << 1,
	GCR_KEY_USAGE_KEY_ENCIPHERMENT = 1 << 2,
	GCR_KEY_USAGE_DATA_ENCIPHERMENT = 1 << 3,
	GCR_KEY_USAGE_KEY_AGREEMENT = 1 << 4,
	GCR_KEY_USAGE_KEY_CERT_SIGN = 1 << 5,
	GCR_KEY_USAGE_CRL_SIGN = 1 << 6,
} GcrCertificateExtensionKeyUsage;

gboolean   _gcr_certificate_extension_key_usage               (gconstpointer data,
                                                               gsize n_data,
                                                               gulong *key_usage);

typedef enum {
	GCR_GENERAL_NAME_OTHER,
	GCR_GENERAL_NAME_RFC822,
	GCR_GENERAL_NAME_DNS,
	GCR_GENERAL_NAME_X400,
	GCR_GENERAL_NAME_DN,
	GCR_GENERAL_NAME_EDI,
	GCR_GENERAL_NAME_URI,
	GCR_GENERAL_NAME_IP,
	GCR_GENERAL_NAME_REGISTERED_ID,
} GcrGeneralNameType;

typedef struct {
	GcrGeneralNameType type;
	const gchar *description;
	gchar *display;
	gconstpointer raw;
	gsize n_raw;
} GcrGeneralName;

GArray *   _gcr_certificate_extension_subject_alt_name        (gconstpointer data,
                                                               gsize n_data);

void       _gcr_general_names_free                            (GArray *names);

G_END_DECLS

#endif /* GCR_CERTIFICATE_H */
