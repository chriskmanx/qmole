/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Collabora Ltd.
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

#ifndef GCRTYPES_H_
#define GCRTYPES_H_

#include <glib.h>

#ifndef GCK_API_SUBJECT_TO_CHANGE
#define GCK_API_SUBJECT_TO_CHANGE 1
#define __GCR_DEFINED_GCK_SUBJECT_TO_CHANGE__ 1
#endif

#include <gck/gck.h>

#ifdef __GCR_DEFINED_GCK_SUBJECT_TO_CHANGE__
#undef GCK_API_SUBJECT_TO_CHANGE
#endif

G_BEGIN_DECLS

#define             GCR_DATA_ERROR                    (gcr_data_error_get_domain ())

GQuark 	            gcr_data_error_get_domain         (void) G_GNUC_CONST;

#ifndef GCR_DISABLE_DEPRECATED

#define             GCR_ERROR                         (gcr_error_get_domain ())

GQuark              gcr_error_get_domain              (void) G_GNUC_CONST;

#endif /* GCR_DISABLE_DEPRECATED */

typedef enum {
	GCR_ERROR_FAILURE = -1,
	GCR_ERROR_UNRECOGNIZED = 1,
	GCR_ERROR_CANCELLED = 2,
	GCR_ERROR_LOCKED = 3
} GcrDataError;

typedef enum {
	GCR_FORMAT_INVALID = 0,

	GCR_FORMAT_DER_PRIVATE_KEY = 100,
	GCR_FORMAT_DER_PRIVATE_KEY_RSA,
	GCR_FORMAT_DER_PRIVATE_KEY_DSA,

	GCR_FORMAT_DER_CERTIFICATE_X509 = 200,

	GCR_FORMAT_DER_PKCS7 = 300,

	GCR_FORMAT_DER_PKCS8 = 400,
	GCR_FORMAT_DER_PKCS8_PLAIN,
	GCR_FORMAT_DER_PKCS8_ENCRYPTED,

	GCR_FORMAT_DER_PKCS12 = 500,

	GCR_FORMAT_PEM = 1000,
	GCR_FORMAT_PEM_PRIVATE_KEY_RSA,
	GCR_FORMAT_PEM_PRIVATE_KEY_DSA,
	GCR_FORMAT_PEM_CERTIFICATE_X509,
	GCR_FORMAT_PEM_PKCS7,
	GCR_FORMAT_PEM_PKCS8_PLAIN,
	GCR_FORMAT_PEM_PKCS8_ENCRYPTED,
	GCR_FORMAT_PEM_PKCS12
} GcrDataFormat;

G_END_DECLS

#endif /* GCRTYPES_H_ */
