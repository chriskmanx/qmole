/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "egg-hkdf.h"
#include "egg-secure-memory.h"

#include <gcrypt.h>

#include <string.h>

gboolean
egg_hkdf_perform (const gchar *hash_algo, gconstpointer input, gsize n_input,
                  gconstpointer salt, gsize n_salt, gconstpointer info,
                  gsize n_info, gpointer output, gsize n_output)
{
	gpointer alloc = NULL;
	gpointer buffer = NULL;
	gcry_md_hd_t md1, md2;
	guint hash_len;
	guchar i;
	gint flags, algo;
	gsize step, n_buffer;
	guchar *at;
	gcry_error_t gcry;

	algo = gcry_md_map_name (hash_algo);
	g_return_val_if_fail (algo != 0, FALSE);

	hash_len = gcry_md_get_algo_dlen (algo);
	g_return_val_if_fail (hash_len != 0, FALSE);
	g_return_val_if_fail (n_output <= 255 * hash_len, FALSE);

	/* Buffer we need to for intermediate stuff */
	if (gcry_is_secure (input)) {
		flags = GCRY_MD_FLAG_SECURE;
		buffer = gcry_malloc_secure (hash_len);
	} else {
		flags = 0;
		buffer = gcry_malloc (hash_len);
	}

	g_return_val_if_fail (buffer, FALSE);
	n_buffer = 0;

	/* Salt defaults to hash_len zeros */
	if (!salt) {
		salt = alloc = g_malloc0 (hash_len);
		n_salt = hash_len;
	}

	/* Step 1: Extract */
	gcry = gcry_md_open (&md1, algo, GCRY_MD_FLAG_HMAC | flags);
	g_return_val_if_fail (gcry == 0, FALSE);
	gcry = gcry_md_setkey (md1, salt, n_salt);
	g_return_val_if_fail (gcry == 0, FALSE);
	gcry_md_write (md1, input, n_input);

	/* Step 2: Expand */
	gcry = gcry_md_open (&md2, algo, GCRY_MD_FLAG_HMAC | flags);
	g_return_val_if_fail (gcry == 0, FALSE);
	gcry = gcry_md_setkey (md2, gcry_md_read (md1, algo), hash_len);
	g_return_val_if_fail (gcry == 0, FALSE);
	gcry_md_close (md1);

	at = output;
	for (i = 1; i < 256; ++i) {
		gcry_md_reset (md2);
		gcry_md_write (md2, buffer, n_buffer);
		gcry_md_write (md2, info, n_info);
		gcry_md_write (md2, &i, 1);

		n_buffer = hash_len;
		memcpy (buffer, gcry_md_read (md2, algo), n_buffer);

		step = MIN (n_buffer, n_output);
		memcpy (at, buffer, step);
		n_output -= step;
		at += step;

		if (!n_output)
			break;
	}

	g_free (alloc);
	gcry_free (buffer);
	return TRUE;
}
