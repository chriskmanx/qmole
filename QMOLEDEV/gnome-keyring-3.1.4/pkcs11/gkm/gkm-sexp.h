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

#ifndef GKM_SEXP_H_
#define GKM_SEXP_H_

#include <gcrypt.h>

#include <glib-object.h>

#include "gkm-types.h"

GkmSexp*       gkm_sexp_new           (gcry_sexp_t sexp);

GkmSexp*       gkm_sexp_ref           (GkmSexp *sexp);

void           gkm_sexp_unref         (gpointer sexp);

gcry_sexp_t    gkm_sexp_get           (GkmSexp *sexp);

#define        GKM_BOXED_SEXP         (gkm_sexp_boxed_type ())

GType          gkm_sexp_boxed_type    (void);


gboolean       gkm_sexp_parse_key                (gcry_sexp_t sexp,
                                                  int *algorithm,
                                                  gboolean *is_private,
                                                  gcry_sexp_t *numbers);

gboolean       gkm_sexp_key_to_public            (gcry_sexp_t sexp,
                                                  gcry_sexp_t *pub);

gboolean       gkm_sexp_extract_mpi              (gcry_sexp_t sexp,
                                                  gcry_mpi_t *mpi,
                                                  ...) G_GNUC_NULL_TERMINATED;

gcry_sexp_t    gkm_sexp_get_childv               (gcry_sexp_t sexp,
                                                  va_list va);

void           gkm_sexp_dump                     (gcry_sexp_t sexp);

#endif /* GKM_SEXP_H_ */
