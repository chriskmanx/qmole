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

#ifndef GCR_LIBRARY_H_
#define GCR_LIBRARY_H_

#include "gcr-types.h"

#include <glib.h>

GList*            gcr_pkcs11_get_modules                   (void);

void              gcr_pkcs11_set_modules                   (GList *modules);

void              gcr_pkcs11_add_module                    (GckModule *module);

gboolean          gcr_pkcs11_add_module_from_file          (const gchar *module_path,
                                                            gpointer unused,
                                                            GError **error);

GList*            gcr_pkcs11_get_trust_lookup_slots        (void);

GckSlot*          gcr_pkcs11_get_trust_store_slot          (void);

const gchar**     gcr_pkcs11_get_trust_lookup_uris         (void);

void              gcr_pkcs11_set_trust_lookup_uris         (const gchar **pkcs11_uris);

const gchar*      gcr_pkcs11_get_trust_store_uri           (void);

void              gcr_pkcs11_set_trust_store_uri           (const gchar *pkcs11_uri);

#endif /* GCR_LIBRARY_H_ */
