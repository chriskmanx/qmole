/*
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

#include "config.h"

#include "egg/egg-hex.h"

#include "gcr-gnupg-util.h"

#include <gcrypt.h>

/**
 * _gcr_gnupg_build_xa1_record:
 * @meta: Status metadata record about the attribute data.
 * @attribute: Pointer to attribute data.
 * @n_attribute: Length of attribute data.
 *
 * Build a record for attribute data. We use this records to convert attribute
 * data into something we can keep with an array of GcrRecord.
 *
 * Returns: (transfer full): The newly allocated record.
 */
GcrRecord*
_gcr_gnupg_build_xa1_record (GcrRecord *meta, gpointer attribute,
                             gsize n_attribute)
{
	gchar hash[20];
	gchar *hex;
	gchar *status = "";
	gint state, save;
	gsize length;
	gsize n_prefix, estimate;
	GString *output;
	GcrRecord *record;
	guint flags, type;
	const gchar *fingerprint, *created, *expiry;

	g_return_val_if_fail (meta, NULL);

	gcry_md_hash_buffer (GCRY_MD_RMD160, hash, attribute, n_attribute);
	hex = egg_hex_encode_full (hash, sizeof (hash), TRUE, 0, 1);

	if (!_gcr_record_get_uint (meta, GCR_RECORD_ATTRIBUTE_FLAGS, &flags))
		flags = 0;

	if (!_gcr_record_get_uint (meta, GCR_RECORD_ATTRIBUTE_TYPE, &type))
		type = 0;

	fingerprint = _gcr_record_get_raw (meta, GCR_RECORD_ATTRIBUTE_FINGERPRINT);
	if (fingerprint == NULL)
		fingerprint = "";

	created = _gcr_record_get_raw (meta, GCR_RECORD_ATTRIBUTE_TIMESTAMP);
	if (created == NULL)
		created = "0";

	expiry = _gcr_record_get_raw (meta, GCR_RECORD_ATTRIBUTE_EXPIRY);
	if (expiry == NULL)
		expiry = "";

	/* These values are from gnupg doc/DETAILS */
	if (flags & 0x02)
		status = "r";
	else if (flags & 0x04)
		status = "e";
	else if (flags & 0x01)
		status = "P";

	/* Algorithm from Glib reference */
	estimate = n_attribute * 4 / 3 + n_attribute * 4 / (3 * 65) + 7;

	output = g_string_sized_new (64 + estimate);
	g_string_append_printf (output, "xa1::%u:%u:%s:%s:%s:%s:%s:",
	                        (guint)n_attribute, type, fingerprint,
	                        created, expiry, hex, status);

	g_free (hex);

	/* Resize string to fit the base64 data. */
	n_prefix = output->len;
	g_string_set_size (output, n_prefix + estimate);

	/* The actual base64 data, without line breaks */
	state = save = 0;
	length = g_base64_encode_step ((guchar*)attribute, n_attribute, FALSE,
	                               output->str + n_prefix, &state, &save);
	length += g_base64_encode_close (TRUE, output->str + n_prefix + length,
	                                 &state, &save);

	g_assert (length <= estimate);
	g_string_set_size (output, n_prefix + length);
	record = _gcr_record_take_colons (g_string_free (output, FALSE));
	g_assert (record);

	return record;
}
