/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef __GKU_PROMPT_UTIL_H__
#define __GKU_PROMPT_UTIL_H__

#include <gcrypt.h>

#include <glib.h>

void        gku_prompt_util_encode_mpi                   (GKeyFile *key_file, const gchar *section,
                                                          const gchar *field, gcry_mpi_t mpi);

void        gku_prompt_util_encode_hex                   (GKeyFile *key_file, const gchar *section,
                                                          const gchar *field, gconstpointer data, gsize n_data);

gboolean    gku_prompt_util_decode_mpi                   (GKeyFile *key_file, const gchar *section,
                                                          const gchar *field, gcry_mpi_t *mpi);

gpointer    gku_prompt_util_decode_hex                   (GKeyFile *key_file, const gchar *section,
                                                          const gchar *field, gsize *n_result);

gpointer    gku_prompt_util_encrypt_text                 (gconstpointer key, gsize n_key,
                                                          gconstpointer iv, gsize n_iv,
                                                          const gchar *text, gsize *n_result);

gchar*      gku_prompt_util_decrypt_text                 (gconstpointer key, gsize n_key,
                                                          gconstpointer iv, gsize n_iv,
                                                          gconstpointer data, gsize n_data);

#endif /* __GKU_PROMPT_H__ */
