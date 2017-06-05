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

#include "gkm-attributes.h"
#include "gkm-util.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>


CK_RV
gkm_attribute_get_bool (CK_ATTRIBUTE_PTR attr, gboolean *value)
{
	CK_BBOOL* bool;

	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_return_val_if_fail (value, CKR_GENERAL_ERROR);

	if (attr->ulValueLen != sizeof (CK_BBOOL) || attr->pValue == NULL)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	bool = attr->pValue;
	*value = *bool ? TRUE : FALSE;
	return CKR_OK;
}

CK_RV
gkm_attribute_get_ulong (CK_ATTRIBUTE_PTR attr, CK_ULONG *value)
{
	CK_ULONG* ulong;

	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_return_val_if_fail (value, CKR_GENERAL_ERROR);

	if (attr->ulValueLen != sizeof (CK_ULONG) || attr->pValue == NULL)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	ulong = attr->pValue;
	*value = *ulong;
	return CKR_OK;
}

#ifndef HAVE_TIMEGM
static time_t
timegm (struct tm *t)
{
	time_t tl, tb;
	struct tm *tg;

	tl = mktime (t);
	if (tl == -1)
	{
		t->tm_hour--;
		tl = mktime (t);
		if (tl == -1)
			return -1; /* can't deal with output from strptime */
		tl += 3600;
	}
	tg = gmtime (&tl);
	tg->tm_isdst = 0;
	tb = mktime (tg);
	if (tb == -1)
	{
		tg->tm_hour--;
		tb = mktime (tg);
		if (tb == -1)
			return -1; /* can't deal with output from gmtime */
		tb += 3600;
	}
	return (tl - (tb - tl));
}
#endif // NOT_HAVE_TIMEGM

CK_RV
gkm_attribute_get_time (CK_ATTRIBUTE_PTR attr, glong *when)
{
	struct tm tm;
	gchar buf[15];
	time_t time;

	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_return_val_if_fail (when, CKR_GENERAL_ERROR);

	if (attr->ulValueLen == 0) {
		*when = (glong)-1;
		return CKR_OK;
	}

	if (!attr->pValue || attr->ulValueLen != 16)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	memset (&tm, 0, sizeof (tm));
	memcpy (buf, attr->pValue, 14);
	buf[14] = 0;

	if (!strptime(buf, "%Y%m%d%H%M%S", &tm))
		return CKR_ATTRIBUTE_VALUE_INVALID;

	/* Convert to seconds since epoch */
	time = timegm (&tm);
	if (time < 0)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	*when = time;
	return CKR_OK;
}

CK_RV
gkm_attribute_get_string (CK_ATTRIBUTE_PTR attr, gchar **value)
{
	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_return_val_if_fail (value, CKR_GENERAL_ERROR);

	if (attr->ulValueLen == 0) {
		*value = NULL;
		return CKR_OK;
	}

	if (!attr->pValue)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	if (!g_utf8_validate (attr->pValue, attr->ulValueLen, NULL))
		return CKR_ATTRIBUTE_VALUE_INVALID;

	*value = g_strndup (attr->pValue, attr->ulValueLen);
	return CKR_OK;
}

CK_RV
gkm_attribute_get_mpi (CK_ATTRIBUTE_PTR attr, gcry_mpi_t *value)
{
	gcry_error_t gcry;

	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_return_val_if_fail (value, CKR_GENERAL_ERROR);

	gcry = gcry_mpi_scan (value, GCRYMPI_FMT_USG, attr->pValue, attr->ulValueLen, NULL);
	if (gcry != 0)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	return CKR_OK;
}

CK_RV
gkm_attribute_get_template (CK_ATTRIBUTE_PTR attr, GArray **template)
{
	CK_ULONG n_attrs;

	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);

	/* Validate everything first */
	if (attr->ulValueLen % sizeof (CK_ATTRIBUTE) != 0)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	n_attrs = attr->ulValueLen / sizeof (CK_ATTRIBUTE);
	if (n_attrs != 0 && !attr->pValue)
		return CKR_ATTRIBUTE_VALUE_INVALID;

	*template = gkm_template_new (attr->pValue, n_attrs);
	return CKR_OK;
}

CK_RV
gkm_attribute_set_empty (CK_ATTRIBUTE_PTR attr)
{
	return gkm_attribute_set_data (attr, "", 0);
}

CK_RV
gkm_attribute_set_bool (CK_ATTRIBUTE_PTR attr, CK_BBOOL value)
{
	return gkm_attribute_set_data (attr, &value, sizeof (CK_BBOOL));
}

CK_RV
gkm_attribute_set_ulong (CK_ATTRIBUTE_PTR attr, CK_ULONG value)
{
	return gkm_attribute_set_data (attr, &value, sizeof (CK_ULONG));
}

CK_RV
gkm_attribute_set_string (CK_ATTRIBUTE_PTR attr, const gchar* string)
{
	return gkm_attribute_set_data (attr, (CK_VOID_PTR)string,
	                               string ? strlen (string) : 0);
}

CK_RV
gkm_attribute_set_date (CK_ATTRIBUTE_PTR attr, time_t time)
{
	CK_DATE date;
	struct tm tm;
	gchar buf[16];

	/* 'Empty' date as defined in PKCS#11 */
	if (time == (time_t)-1)
		return gkm_attribute_set_data (attr, NULL, 0);

	if (!attr->pValue) {
		attr->ulValueLen = sizeof (CK_DATE);
		return CKR_OK;
	}

	if (!gmtime_r (&time, &tm))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	g_assert (sizeof (date.year) == 4);
	snprintf ((char*)buf, 5, "%04d", 1900 + tm.tm_year);
	memcpy (date.year, buf, 4);

	g_assert (sizeof (date.month) == 2);
	snprintf ((char*)buf, 3, "%02d", tm.tm_mon + 1);
	memcpy (date.month, buf, 2);

	g_assert (sizeof (date.day) == 2);
	snprintf ((char*)buf, 3, "%02d", tm.tm_mday);
	memcpy (date.day, buf, 2);

	return gkm_attribute_set_data (attr, &date, sizeof (date));
}

CK_RV
gkm_attribute_set_time (CK_ATTRIBUTE_PTR attr, glong when)
{
	struct tm tm;
	gchar buf[20];

	/* 'Empty' time as defined in PKCS#11 */
	if (when == (glong)-1)
		return gkm_attribute_set_data (attr, NULL, 0);

	if (!attr->pValue) {
		attr->ulValueLen = 16;
		return CKR_OK;
	}

	time_t time = when;
	if (!gmtime_r (&time, &tm))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	if (!strftime(buf, sizeof (buf), "%Y%m%d%H%M%S00", &tm))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	return gkm_attribute_set_data (attr, buf, 16);
}

CK_RV
gkm_attribute_set_data (CK_ATTRIBUTE_PTR attr, gconstpointer value, gsize n_value)
{
	CK_RV rv = gkm_util_return_data (attr->pValue, &(attr->ulValueLen), value, n_value);
	if (rv == CKR_BUFFER_TOO_SMALL)
		attr->ulValueLen = (CK_ULONG)-1;
	return rv;
}

CK_RV
gkm_attribute_set_mpi (CK_ATTRIBUTE_PTR attr, gcry_mpi_t mpi)
{
	gsize len;
	gcry_error_t gcry;

	g_assert (attr);
	g_assert (mpi);

	/* Get the size */
	gcry = gcry_mpi_print (GCRYMPI_FMT_USG, NULL, 0, &len, mpi);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	if (!attr->pValue) {
		attr->ulValueLen = len;
		return CKR_OK;
	}

	if (len > attr->ulValueLen) {
		attr->ulValueLen = (CK_ULONG)-1;
		return CKR_BUFFER_TOO_SMALL;
	}

	/* Write in directly to attribute */
	gcry = gcry_mpi_print (GCRYMPI_FMT_USG, attr->pValue, len, &len, mpi);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	attr->ulValueLen = len;
	return CKR_OK;
}

CK_RV
gkm_attribute_set_template (CK_ATTRIBUTE_PTR attr, GArray *template)
{
	CK_ATTRIBUTE_PTR array;
	CK_ATTRIBUTE_PTR at;
	CK_RV rv;
	gulong len;
	gulong i;

	g_assert (attr);
	g_warn_if_fail ((attr->type & CKF_ARRAY_ATTRIBUTE) != 0);

	len = sizeof (CK_ATTRIBUTE) * template->len;
	if (!attr->pValue) {
		attr->ulValueLen = len;
		return CKR_OK;
	} else if (len > attr->ulValueLen) {
		attr->ulValueLen = (CK_ULONG)-1;
		return CKR_BUFFER_TOO_SMALL;
	}

	attr->ulValueLen = len;
	array = attr->pValue;
	rv = CKR_OK;

	/* Start working with individual elements */
	for (i = 0; i < template->len; ++i) {
		at = &g_array_index (template, CK_ATTRIBUTE, i);
		array[i].type = at->type;
		if (!array[i].pValue) {
			array[i].ulValueLen = at->ulValueLen;
		} else if (array[i].ulValueLen < at->ulValueLen) {
			array[i].ulValueLen = (CK_ULONG)-1;
			rv = CKR_BUFFER_TOO_SMALL;
		} else {
			memcpy(array[i].pValue, at->pValue, at->ulValueLen);
			array[i].ulValueLen = at->ulValueLen;
		}
	}

	return rv;
}

gboolean
gkm_attribute_equal (gconstpointer v1, gconstpointer v2)
{
	const CK_ATTRIBUTE *a1 = v1;
	const CK_ATTRIBUTE *a2 = v2;

	g_assert (a1);
	g_assert (a2);

	if (a1 == a2)
		return TRUE;
	if (a1->type != a2->type)
		return FALSE;
	if (a1->ulValueLen != a2->ulValueLen)
		return FALSE;
	if (a1->pValue == a2->pValue)
		return TRUE;
	if (a1->ulValueLen == 0)
		return TRUE;

	g_assert (a1->pValue);
	g_assert (a2->pValue);

	return memcmp (a1->pValue, a2->pValue, a1->ulValueLen) == 0;
}

guint
gkm_attribute_hash (gconstpointer v)
{
	const CK_ATTRIBUTE *a = v;
	const signed char *p;
	guint i, h;

	g_assert (a);

	p = (const signed char*)&(a->type);
	h = *p;
	for(i = 0; i < sizeof (CK_ATTRIBUTE_PTR); ++i)
		h = (h << 5) - h + *(p++);

	p = a->pValue;
	for(i = 0; i < a->ulValueLen; ++i)
		h = (h << 5) - h + *(p++);

	return h;
}

gboolean
gkm_attribute_consumed (CK_ATTRIBUTE_PTR attr)
{
	g_return_val_if_fail (attr, FALSE);
	return attr->type == (CK_ULONG)-1;
}

void
gkm_attribute_consume (CK_ATTRIBUTE_PTR attr)
{
	attr->type = (CK_ULONG)-1;
}

void
gkm_attributes_consume (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, ...)
{
	CK_ATTRIBUTE_TYPE type;
	GArray *types;
	guint i, j;
	va_list va;

	/* Convert the var args into an array */
	types = g_array_new (FALSE, TRUE, sizeof (CK_ATTRIBUTE_TYPE));
	va_start (va, n_attrs);
	while ((type = va_arg (va, CK_ATTRIBUTE_TYPE)) != G_MAXULONG)
		 g_array_append_val (types, type);
	va_end (va);

	/* Consume each attribute whose type was in the var args */
	for (i = 0; i < n_attrs; ++i) {
		if (gkm_attribute_consumed (&attrs[i]))
			continue;
		for (j = 0; j < types->len; ++j) {
			if (attrs[i].type == g_array_index (types, CK_ATTRIBUTE_TYPE, j)) {
				attrs[i].type = (CK_ULONG)-1;
				break;
			}
		}
	}

	g_array_free (types, TRUE);
}

gboolean
gkm_attributes_contains (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, CK_ATTRIBUTE_PTR attr)
{
	CK_ULONG i;

	g_assert (attrs || !n_attrs);
	g_assert (attr);

	for (i = 0; i < n_attrs; ++i) {
		if (gkm_attribute_equal (attr, &attrs[i]))
			return TRUE;
	}

	return FALSE;
}

CK_ATTRIBUTE_PTR
gkm_attributes_find (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, CK_ATTRIBUTE_TYPE type)
{
	CK_ULONG i;

	g_assert (attrs || !n_attrs);

	for (i = 0; i < n_attrs; ++i) {
		if(attrs[i].type == type && attrs[i].ulValueLen != (CK_ULONG)-1)
			return &attrs[i];
	}

	return NULL;
}

gboolean
gkm_attributes_find_boolean (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, CK_ATTRIBUTE_TYPE type, gboolean *value)
{
	CK_ATTRIBUTE_PTR attr;

	g_assert (attrs || !n_attrs);

	attr = gkm_attributes_find (attrs, n_attrs, type);
	if (attr == NULL)
		return FALSE;

	if (attr->ulValueLen != sizeof (CK_BBOOL))
		return FALSE;

	if (value != NULL)
		*value = *((CK_BBOOL*)attr->pValue) == CK_TRUE ? TRUE : FALSE;

	return TRUE;
}

gboolean
gkm_attributes_find_ulong (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, CK_ATTRIBUTE_TYPE type, gulong *value)
{
	CK_ATTRIBUTE_PTR attr;

	g_assert (attrs || !n_attrs);

	attr = gkm_attributes_find (attrs, n_attrs, type);
	if (attr == NULL)
		return FALSE;

	if (attr->ulValueLen != sizeof (CK_ULONG))
		return FALSE;

	if (value != NULL)
		*value = *((CK_ULONG*)attr->pValue);

	return TRUE;
}

gboolean
gkm_attributes_find_mpi (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs,
                         CK_ATTRIBUTE_TYPE type, gcry_mpi_t *value)
{
	CK_ATTRIBUTE_PTR attr;

	g_assert (attrs || !n_attrs);

	attr = gkm_attributes_find (attrs, n_attrs, type);
	if (attr == NULL)
		return FALSE;

	return gkm_attribute_get_mpi (attr, value) == CKR_OK;
}

gboolean
gkm_attributes_find_string (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs,
                            CK_ATTRIBUTE_TYPE type, gchar **value)
{
	CK_ATTRIBUTE_PTR attr;

	g_return_val_if_fail (attrs || !n_attrs, FALSE);

	attr = gkm_attributes_find (attrs, n_attrs, type);
	if (attr == NULL)
		return FALSE;

	return gkm_attribute_get_string (attr, value) == CKR_OK;
}

GArray*
gkm_template_new (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GArray *template = g_array_new (FALSE, FALSE, sizeof (CK_ATTRIBUTE));
	CK_ATTRIBUTE_PTR pat;
	gulong i;

	g_return_val_if_fail (attrs || !n_attrs, NULL);

	g_array_append_vals (template, attrs, n_attrs);
	for (i = 0; i < n_attrs; ++i) {
		pat = &g_array_index (template, CK_ATTRIBUTE, i);
		if (pat->pValue) {
			g_return_val_if_fail (pat->ulValueLen != (CK_ULONG)-1, NULL);
			pat->pValue = g_memdup (pat->pValue, pat->ulValueLen ? pat->ulValueLen : 1);
		}
	}

	return template;
}

void
gkm_template_set (GArray *template, CK_ATTRIBUTE_PTR attr)
{
	CK_ATTRIBUTE_PTR pat;
	CK_ATTRIBUTE at;
	guint i;

	g_return_if_fail (template);
	g_return_if_fail (attr);
	g_return_if_fail (attr->ulValueLen != (CK_ULONG)-1);

	pat = gkm_attributes_find ((CK_ATTRIBUTE_PTR)template->data, template->len, attr->type);

	/* Remove any previous value */
	for (i = 0; i < template->len; ++i) {
		if (g_array_index (template, CK_ATTRIBUTE, i).type == attr->type) {
			g_free (g_array_index (template, CK_ATTRIBUTE, i).pValue);
			g_array_remove_index_fast (template, i);
			break;
		}
	}

	/* Add a new attribute */
	memcpy (&at, attr, sizeof (at));
	if (at.pValue)
		at.pValue = g_memdup (at.pValue, at.ulValueLen ? at.ulValueLen : 1);
	g_array_append_vals (template, &at, 1);
}

void
gkm_template_set_value (GArray *template, CK_ATTRIBUTE_TYPE type,
                        CK_VOID_PTR value, CK_ULONG length)
{
	CK_ATTRIBUTE attr;
	g_return_if_fail (template);

	attr.type = type;
	attr.pValue = value;
	attr.ulValueLen = length;
	gkm_template_set (template, &attr);
}

void
gkm_template_set_string (GArray *template, CK_ATTRIBUTE_TYPE type, const gchar *value)
{
	CK_ATTRIBUTE attr;
	g_return_if_fail (template);
	g_return_if_fail (value);

	attr.type = type;
	attr.pValue = (CK_VOID_PTR)value;
	attr.ulValueLen = strlen (value);
	gkm_template_set (template, &attr);

}
void
gkm_template_set_ulong (GArray *template, CK_ATTRIBUTE_TYPE type, CK_ULONG value)
{
	CK_ATTRIBUTE attr;
	g_return_if_fail (template);

	attr.type = type;
	attr.pValue = &value;
	attr.ulValueLen = sizeof (value);
	gkm_template_set (template, &attr);

}

void
gkm_template_set_boolean (GArray *template, CK_ATTRIBUTE_TYPE type, CK_BBOOL value)
{
	CK_ATTRIBUTE attr;
	g_return_if_fail (template);

	attr.type = type;
	attr.pValue = &value;
	attr.ulValueLen = sizeof (value);
	gkm_template_set (template, &attr);
}

void
gkm_template_free (GArray *template)
{
	guint i;

	if (!template)
		return;

	for (i = 0; i < template->len; ++i)
		g_free (g_array_index (template, CK_ATTRIBUTE, i).pValue);
	g_array_free (template, TRUE);
}

CK_ATTRIBUTE_PTR
gkm_template_find (GArray *template, CK_ATTRIBUTE_TYPE type)
{
	g_return_val_if_fail (template, NULL);
	return gkm_attributes_find ((CK_ATTRIBUTE_PTR)template->data, template->len, type);
}

gboolean
gkm_template_find_boolean (GArray *template, CK_ATTRIBUTE_TYPE type, gboolean *value)
{
	g_return_val_if_fail (template, FALSE);
	return gkm_attributes_find_boolean ((CK_ATTRIBUTE_PTR)template->data, template->len, type, value);
}

gboolean
gkm_template_find_ulong (GArray *template, CK_ATTRIBUTE_TYPE type, gulong *value)
{
	g_return_val_if_fail (template, FALSE);
	return gkm_attributes_find_ulong ((CK_ATTRIBUTE_PTR)template->data, template->len, type, value);
}
