/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-openssl.h - OpenSSL compatibility functionality

   Copyright (C) 2007 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef EGG_OPENSSL_H_
#define EGG_OPENSSL_H_

#include <glib.h>

typedef void (*EggOpensslPemCallback) (GQuark type, const guchar *data, gsize n_data,
                                       GHashTable *headers, gpointer user_data);

GHashTable*      egg_openssl_headers_new       (void);

guint            egg_openssl_pem_parse         (const guchar *data, gsize n_data, 
                                                EggOpensslPemCallback callback, 
                                                gpointer user_data);

guchar*          egg_openssl_pem_write         (const guchar *data, gsize n_data, 
                                                GQuark type, GHashTable *headers,
                                                gsize *n_result);

int              egg_openssl_parse_algo        (const gchar *name, int *mode);

gboolean         egg_openssl_encrypt_block     (const gchar *dekinfo, const gchar *password, 
                                                gssize n_password, const guchar *data, gsize n_data,
                                                guchar **encrypted, gsize *n_encrypted);

gboolean         egg_openssl_decrypt_block     (const gchar *dekinfo, const gchar *password, 
                                                gssize n_password, const guchar *data, gsize n_data, 
                                                guchar **decrypted, gsize *n_decrypted);

const gchar*     egg_openssl_get_dekinfo       (GHashTable *headers);

const gchar*     egg_openssl_prep_dekinfo      (GHashTable *headers);

#endif /* EGG_OPENSSL_H_ */
