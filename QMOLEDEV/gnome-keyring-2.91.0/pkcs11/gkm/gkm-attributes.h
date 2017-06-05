/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef GKM_ATTRIBUTE_H_
#define GKM_ATTRIBUTE_H_

#include <glib.h>

#include <gcrypt.h>

#include "pkcs11/pkcs11.h"

CK_RV                 gkm_attribute_get_bool                           (CK_ATTRIBUTE_PTR attr,
                                                                        gboolean *value);

CK_RV                 gkm_attribute_get_ulong                          (CK_ATTRIBUTE_PTR attr,
                                                                        CK_ULONG *value);

CK_RV                 gkm_attribute_get_time                           (CK_ATTRIBUTE_PTR attr,
                                                                        glong *value);

CK_RV                 gkm_attribute_get_string                         (CK_ATTRIBUTE_PTR attr,
                                                                        gchar **value);

CK_RV                 gkm_attribute_get_mpi                            (CK_ATTRIBUTE_PTR attr,
                                                                        gcry_mpi_t *value);

CK_RV                 gkm_attribute_get_template                       (CK_ATTRIBUTE_PTR attr,
                                                                        GArray **template);

CK_RV                 gkm_attribute_set_empty                          (CK_ATTRIBUTE_PTR attr);

CK_RV                 gkm_attribute_set_bool                           (CK_ATTRIBUTE_PTR attr,
                                                                        CK_BBOOL value);

CK_RV                 gkm_attribute_set_ulong                          (CK_ATTRIBUTE_PTR attr,
                                                                        CK_ULONG value);

CK_RV                 gkm_attribute_set_string                         (CK_ATTRIBUTE_PTR attr,
                                                                        const gchar* string);

CK_RV                 gkm_attribute_set_date                           (CK_ATTRIBUTE_PTR attr,
                                                                        time_t when);

CK_RV                 gkm_attribute_set_time                           (CK_ATTRIBUTE_PTR attr,
                                                                        glong when);

CK_RV                 gkm_attribute_set_data                           (CK_ATTRIBUTE_PTR attr,
                                                                        gconstpointer value,
                                                                        gsize n_value);

CK_RV                 gkm_attribute_set_mpi                            (CK_ATTRIBUTE_PTR attr,
                                                                        gcry_mpi_t mpi);

CK_RV                 gkm_attribute_set_template                       (CK_ATTRIBUTE_PTR attr,
                                                                        GArray *template);

guint                 gkm_attribute_hash                               (gconstpointer v);

gboolean              gkm_attribute_equal                              (gconstpointer a,
                                                                        gconstpointer b);

void                  gkm_attribute_consume                            (CK_ATTRIBUTE_PTR attr);

gboolean              gkm_attribute_consumed                           (CK_ATTRIBUTE_PTR attr);



gboolean              gkm_attributes_contains                          (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        CK_ATTRIBUTE_PTR attr);

void                  gkm_attributes_consume                           (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs, ...);

CK_ATTRIBUTE_PTR      gkm_attributes_find                              (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        CK_ATTRIBUTE_TYPE type);

gboolean              gkm_attributes_find_boolean                      (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        gboolean *value);

gboolean              gkm_attributes_find_ulong                        (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        gulong *value);

gboolean              gkm_attributes_find_mpi                          (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        gcry_mpi_t *mpi);

gboolean              gkm_attributes_find_string                       (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        gchar **value);

GArray*               gkm_template_new                                 (CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs);

void                  gkm_template_set                                 (GArray *template,
                                                                        CK_ATTRIBUTE_PTR attr);

void                  gkm_template_set_value                           (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        CK_VOID_PTR value,
                                                                        CK_ULONG length);

void                  gkm_template_set_string                          (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        const gchar *value);

void                  gkm_template_set_ulong                           (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        CK_ULONG value);

void                  gkm_template_set_boolean                         (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        CK_BBOOL value);

void                  gkm_template_free                                (GArray *template);

CK_ATTRIBUTE_PTR      gkm_template_find                                (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type);

gboolean              gkm_template_find_boolean                        (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        gboolean *value);

gboolean              gkm_template_find_ulong                          (GArray *template,
                                                                        CK_ATTRIBUTE_TYPE type,
                                                                        gulong *value);

#endif /* GKM_ATTRIBUTE_H_ */
