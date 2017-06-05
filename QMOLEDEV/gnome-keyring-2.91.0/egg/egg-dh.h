/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef EGG_DH_H_
#define EGG_DH_H_

#include <glib.h>

#include <gcrypt.h>

gboolean   egg_dh_default_params                              (const gchar *name,
                                                               gcry_mpi_t *prime,
                                                               gcry_mpi_t *base);

gboolean   egg_dh_default_params_raw                          (const gchar *name,
                                                               gconstpointer *prime,
                                                               gsize *n_prime,
                                                               gconstpointer *base,
                                                               gsize *n_base);

gboolean   egg_dh_gen_pair                                    (gcry_mpi_t prime,
                                                               gcry_mpi_t base,
                                                               guint bits,
                                                               gcry_mpi_t *pub,
                                                               gcry_mpi_t *priv);

gpointer   egg_dh_gen_secret                                  (gcry_mpi_t peer,
                                                               gcry_mpi_t priv,
                                                               gcry_mpi_t prime,
                                                               gsize bytes);

#endif /* EGG_DH_H_ */
