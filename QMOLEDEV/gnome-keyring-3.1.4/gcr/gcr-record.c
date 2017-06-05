/*
 * gnome-keyring
 *
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

#include "gcr-record.h"
#define DEBUG_FLAG GCR_DEBUG_PARSE
#include "gcr-debug.h"

#include <stdlib.h>
#include <string.h>

#define MAX_COLUMNS 32

struct _GcrRecord {
	gchar *data;
	gsize n_data;
	gchar *columns[MAX_COLUMNS];
	guint n_columns;
};

G_DEFINE_BOXED_TYPE (GcrRecord, _gcr_record, _gcr_record_copy, _gcr_record_free);

GcrRecord*
_gcr_record_copy (GcrRecord *record)
{
	GcrRecord *result;
	gchar *column;
	guint i;

	result = g_slice_new0 (GcrRecord);
	result->data = g_memdup (record->data, record->n_data);
	result->n_data = record->n_data;

	for (i = 0; i < record->n_columns; i++) {
		column = (gchar*)record->columns[i];
		g_assert (column >= record->data);
		result->columns[i] = result->data + (column - record->data);
	}
	result->n_columns = record->n_columns;
	return result;
}

static GcrRecord*
take_and_parse_internal (gchar *line, gchar delimiter, gboolean allow_empty)
{
	GcrRecord *result;
	gchar *at, *beg, *end;

	g_assert (line);

	result = g_slice_new0 (GcrRecord);
	result->data = line;
	result->n_data = strlen (line) + 1;

	_gcr_debug ("parsing line %s", line);

	at = result->data;
	for (;;) {
		if (result->n_columns >= MAX_COLUMNS) {
			_gcr_debug ("too many record (%d) in gnupg line", MAX_COLUMNS);
			_gcr_record_free (result);
			return NULL;
		}

		beg = at;
		result->columns[result->n_columns] = beg;

		at = strchr (beg, delimiter);
		if (at == NULL) {
			end = (result->data + result->n_data) - 1;
		} else {
			at[0] = '\0';
			end = at;
			at++;
		}

		if (allow_empty || end > beg)
			result->n_columns++;

		if (at == NULL)
			break;
	}

	return result;
}

GcrRecord*
_gcr_record_parse_colons (const gchar *line, gssize n_line)
{
	g_return_val_if_fail (line, NULL);
	if (n_line < 0)
		n_line = strlen (line);
	return take_and_parse_internal (g_strndup (line, n_line), ':', TRUE);
}

GcrRecord*
_gcr_record_take_colons (gchar *line)
{
	GcrRecord *record;

	g_return_val_if_fail (line, NULL);
	record = take_and_parse_internal (line, ':', TRUE);
	if (record == NULL)
		g_warning ("internal parsing of colons format failed");
	return record;
}

GcrRecord*
_gcr_record_parse_spaces (const gchar *line, gssize n_line)
{
	g_return_val_if_fail (line, NULL);
	if (n_line < 0)
		n_line = strlen (line);
	return take_and_parse_internal (g_strndup (line, n_line), ' ', FALSE);
}


GcrRecord*
_gcr_record_find (GPtrArray *records, GQuark schema)
{
	guint i;

	g_return_val_if_fail (records, NULL);
	g_return_val_if_fail (schema, NULL);

	for (i = 0; i < records->len; i++) {
		if (schema == _gcr_record_get_schema (records->pdata[i]))
			return records->pdata[i];
	}

	return NULL;
}

guint
_gcr_record_get_count (GcrRecord *record)
{
	g_return_val_if_fail (record, 0);
	return record->n_columns;
}

gchar*
_gcr_record_get_string (GcrRecord *record, guint column)
{
	const gchar *value;
	gchar *text;
	gchar *converted;

	g_return_val_if_fail (record, NULL);

	value = _gcr_record_get_raw (record, column);
	if (!value)
		return NULL;
	text = g_strcompress (value);
	if (g_utf8_validate (text, -1, NULL))
		return text;

	/* If it's not UTF-8, we guess that it's latin1 */
	converted = g_convert (text, -1, "UTF-8", "ISO-8859-1", NULL, NULL, NULL);
	g_free (text);

	/*
	 * latin1 to utf-8 conversion can't really fail, just produce
	 * garbage... so there's no need to check here.
	 */

	return converted;
}

gboolean
_gcr_record_get_uint (GcrRecord *record, guint column, guint *value)
{
	const gchar *raw;
	gint64 result;
	gchar *end = NULL;

	g_return_val_if_fail (record, FALSE);

	raw = _gcr_record_get_raw (record, column);
	if (raw == NULL)
		return FALSE;

	result = g_ascii_strtoll (raw, &end, 10);
	if (!end || end[0]) {
		_gcr_debug ("invalid unsigned integer value: %s", raw);
		return FALSE;
	}

	if (result < 0 || result > G_MAXUINT32) {
		_gcr_debug ("unsigned integer value is out of range: %s", raw);
		return FALSE;
	}

	if (value)
		*value = (guint)result;
	return TRUE;
}

/**
 * _gcr_record_get_base64:
 * @record: The record
 * @column: The column to decode.
 * @n_data: Location to return size of returned data.
 *
 * Decode a column of a record as base64 data.
 *
 * Returns: (transfer full): The decoded value, or %NULL if not found.
 */
gpointer
_gcr_record_get_base64 (GcrRecord *record, guint column, gsize *n_data)
{
	const gchar *raw;

	g_return_val_if_fail (record, NULL);

	raw = _gcr_record_get_raw (record, column);
	if (raw == NULL)
		return NULL;

	return g_base64_decode (raw, n_data);
}

const gchar*
_gcr_record_get_raw (GcrRecord *record, guint column)
{
	g_return_val_if_fail (record, NULL);

	if (column >= record->n_columns) {
		_gcr_debug ("only %d columns exist, tried to access %d",
		            record->n_columns, column);
		return NULL;
	}

	return record->columns[column];
}

void
_gcr_record_free (gpointer record)
{
	if (!record)
		return;

	g_free (((GcrRecord*)record)->data);
	g_slice_free (GcrRecord, record);
}

GQuark
_gcr_record_get_schema (GcrRecord *record)
{
	const gchar *value;

	value = _gcr_record_get_raw (record, GCR_RECORD_SCHEMA);
	if (value != NULL)
		return g_quark_try_string (value);
	return 0;
}
