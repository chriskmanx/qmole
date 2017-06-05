/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Collabora Ltd
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

#include "egg-byte-array.h"

#include <string.h>

guint
egg_byte_array_hash (gconstpointer v)
{
	const GByteArray *array = v;
	const signed char *p;
	guint32 h = 0;
	gsize i;

	g_assert (array);
	g_assert (array->data);
	p = (signed char*)array->data;

	/* 31 bit hash function */
	for (i = 0; i < array->len; ++i, ++p)
		h = (h << 5) - h + *p;

	return h;
}

gboolean
egg_byte_array_equal (gconstpointer v1, gconstpointer v2)
{
	const GByteArray *array1 = v1;
	const GByteArray *array2 = v2;

	if (array1 == array2)
		return TRUE;
	if (!array1 || !array2)
		return FALSE;

	if (array1->len != array2->len)
		return FALSE;

	g_assert (array1->data);
	g_assert (array2->data);

	return (memcmp (array1->data, array2->data, array1->len) == 0);
}
