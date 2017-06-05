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

#include "config.h"

#include "egg-padding.h"

#include <gcrypt.h>

#include "egg/egg-secure-memory.h"

/* ----------------------------------------------------------------------------
 * INTERNAL
 */

static void
fill_random_nonzero (guchar *data, gsize n_data)
{
	guchar *rnd;
	guint n_zero, i, j;

	gcry_randomize (data, n_data, GCRY_STRONG_RANDOM);

	/* Find any zeros in random data */
	n_zero = 0;
	for (i = 0; i < n_data; ++i) {
		if (data[i] == 0x00)
			++n_zero;
	}

	while (n_zero > 0) {
		rnd = gcry_random_bytes (n_zero, GCRY_STRONG_RANDOM);
		n_zero = 0;
		for (i = 0, j = 0; i < n_data; ++i) {
			if (data[i] != 0x00)
				continue;

			/* Use some of the replacement data */
			data[i] = rnd[j];
			++j;

			/* It's zero again :( */
			if (data[i] == 0x00)
				n_zero++;
		}

		gcry_free (rnd);
	}
}

static gboolean
unpad_pkcs1 (guchar bt, EggAllocator alloc, gsize block, const guchar* padded,
             gsize n_padded, gpointer *raw, gsize *n_raw)
{
	const guchar *at;

	if (block && n_padded % block != 0)
		return FALSE;

	/* Check the header */
	if (padded[0] != 0x00 || padded[1] != bt)
		return FALSE;

	/* The first zero byte after the header */
	at = memchr (padded + 2, 0x00, n_padded - 2);
	if (!at)
		return FALSE;

	if (alloc == NULL)
		alloc = g_realloc;

	++at;
	*n_raw = n_padded - (at - padded);
	if (raw) {
		*raw = (alloc) (NULL, *n_raw + 1);
		if (*raw == NULL)
			return FALSE;
		memcpy (*raw, at, *n_raw);

		/* Convenience null terminate the result */
		memset (((guchar*)*raw) + *n_raw, 0, 1);
	}

	return TRUE;
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

gboolean
egg_padding_zero_pad (EggAllocator alloc, gsize block, gconstpointer raw,
                      gsize n_raw, gpointer *padded, gsize *n_padded)
{
	guchar *pad;
	gsize n_pad;

	/*
	 * 0x00 0x00 0x00 ... 0x?? 0x?? 0x?? ...
	 *   padding               data
	 */

	g_return_val_if_fail (block != 0, FALSE);

	*n_padded = ((n_raw + (block - 1)) / block) * block;
	g_assert (n_raw <= *n_padded);
	n_pad = *n_padded - n_raw;
	g_assert (n_pad < block);

	if (alloc == NULL)
		alloc = g_realloc;

	if (padded) {
		*padded = pad = (alloc) (NULL, MAX (*n_padded, 1));
		if (pad == NULL)
			return FALSE;
		memset (pad, 0x00, n_pad);
		memcpy (pad + n_pad, raw, n_raw);
	}

	return TRUE;
}

gboolean
egg_padding_pkcs1_pad_01 (EggAllocator alloc, gsize block, gconstpointer raw,
                           gsize n_raw, gpointer *padded, gsize *n_padded)
{
	guchar *pad;
	gsize n_pad;

	/*
	 * 0x00 0x01 0xFF 0xFF ... 0x00 0x?? 0x?? 0x?? ...
	 *      type  padding              data
	 */

	g_return_val_if_fail (block != 0, FALSE);
	g_return_val_if_fail (block > 3, FALSE);

	*n_padded = ((n_raw + 3 + (block - 1)) / block) * block;
	g_assert (n_raw <= *n_padded);
	n_pad = *n_padded - n_raw;
	g_assert (n_pad <= block);
	g_assert (n_pad >= 3);

	if (alloc == NULL)
		alloc = g_realloc;

	if (padded) {
		*padded = pad = (alloc) (NULL, MAX (*n_padded, 1));
		if (pad == NULL)
			return FALSE;
		pad[0] = 0; /* Prefix */
		pad[1] = 1; /* Block type */
		memset (pad + 2, 0xFF, n_pad - 3);
		pad[n_pad - 1] = 0;
		memcpy (pad + n_pad, raw, n_raw);
	}

	return TRUE;
}

gboolean
egg_padding_pkcs1_pad_02 (EggAllocator alloc, gsize block, gconstpointer raw,
                           gsize n_raw, gpointer *padded, gsize *n_padded)
{
	guchar *pad;
	gsize n_pad;

	/*
	 * 0x00 0x01 0x?? 0x?? ... 0x00 0x?? 0x?? 0x?? ...
	 *      type  padding              data
	 */

	g_return_val_if_fail (block != 0, FALSE);
	g_return_val_if_fail (block > 3, FALSE);

	*n_padded = ((n_raw + 3 + (block - 1)) / block) * block;
	g_assert (n_raw <= *n_padded);
	n_pad = *n_padded - n_raw;
	g_assert (n_pad <= block);
	g_assert (n_pad >= 3);

	if (alloc == NULL)
		alloc = g_realloc;

	if (padded) {
		*padded = pad = (alloc) (NULL, MAX (*n_padded, 1));
		if (pad == NULL)
			return FALSE;
		pad[0] = 0; /* Prefix */
		pad[1] = 2; /* Block type */
		fill_random_nonzero (pad + 2, n_pad - 3);
		pad[n_pad - 1] = 0;
		memcpy (pad + n_pad, raw, n_raw);
	}

	return TRUE;
}

gboolean
egg_padding_pkcs1_unpad_01 (EggAllocator alloc, gsize block, gconstpointer padded,
                             gsize n_padded, gpointer *raw, gsize *n_raw)
{
	return unpad_pkcs1 (0x01, alloc, block, padded, n_padded, raw, n_raw);
}

gboolean
egg_padding_pkcs1_unpad_02 (EggAllocator alloc, gsize block, gconstpointer padded,
                             gsize n_padded, gpointer *raw, gsize *n_raw)
{
	return unpad_pkcs1 (0x02, alloc, block, padded, n_padded, raw, n_raw);
}

gboolean
egg_padding_pkcs7_pad (EggAllocator alloc, gsize block, gconstpointer raw,
                       gsize n_raw, gpointer *padded, gsize *n_padded)
{
	guchar *pad;
	gsize n_pad;

	g_return_val_if_fail (block != 0, FALSE);
	g_return_val_if_fail (block < 256, FALSE);

	*n_padded = ((n_raw + block) / block) * block;
	g_assert (n_raw < *n_padded);
	n_pad = *n_padded - n_raw;
	g_assert (n_pad > 0 && n_pad <= block);

	if (alloc == NULL)
		alloc = g_realloc;

	if (padded) {
		*padded = pad = (alloc) (NULL, MAX (*n_padded, 1));
		if (pad == NULL)
			return FALSE;
		memcpy (pad, raw, n_raw);
		memset (pad + n_raw, n_pad, n_pad);
	}

	return TRUE;
}

gboolean
egg_padding_pkcs7_unpad (EggAllocator alloc, gsize block, gconstpointer padded,
                         gsize n_padded, gpointer *raw, gsize *n_raw)
{
	const guchar *pad;
	gsize n_pad, i;

	if (n_padded == 0)
		return FALSE;

	pad = padded;
	n_pad = pad[n_padded - 1];

	/* Validate the padding */
	if (n_pad == 0 || n_pad > 256)
		return FALSE;
	if (n_pad > n_padded)
		return FALSE;
	if (block && n_pad > block)
		return FALSE;
	for (i = n_padded - n_pad; i < n_padded; ++i) {
		if (pad[i] != n_pad)
			return FALSE;
	}

	*n_raw = n_padded - n_pad;

	if (alloc == NULL)
		alloc = g_realloc;

	if (raw) {
		*raw = (alloc) (NULL, *n_raw + 1);
		if (*raw == NULL)
			return FALSE;

		/* Output the result, null terminated */
		memcpy (*raw, pad, *n_raw);
		memset (((guchar*)*raw) + *n_raw, 0, 1);
	}

	return TRUE;
}
