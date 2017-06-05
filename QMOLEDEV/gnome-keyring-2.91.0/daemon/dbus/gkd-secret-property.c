/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
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

#include "config.h"

#include "gkd-secret-property.h"

#include "pkcs11/pkcs11i.h"

#include <string.h>


typedef enum _DataType {
	DATA_TYPE_INVALID = 0,

	/*
	 * The attribute is a CK_BBOOL.
	 * Property is DBUS_TYPE_BOOLEAN
	 */
	DATA_TYPE_BOOL,

	/*
	 * The attribute is in the format: "%Y%m%d%H%M%S00"
	 * Property is DBUS_TYPE_INT64 since 1970 epoch.
	 */
	DATA_TYPE_TIME,

	/*
	 * The attribute is a CK_UTF8_CHAR string, not null-terminated
	 * Property is a DBUS_TYPE_STRING
	 */
	DATA_TYPE_STRING,

	/*
	 * The attribute is in the format: name\0value\0name2\0value2
	 * Property is dbus dictionary of strings: a{ss}
	 */
	DATA_TYPE_FIELDS
} DataType;

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gboolean
property_to_attribute (const gchar *prop_name, CK_ATTRIBUTE_TYPE *attr_type, DataType *data_type)
{
	g_return_val_if_fail (prop_name, FALSE);
	g_assert (attr_type);
	g_assert (data_type);

	if (g_str_equal (prop_name, "Label")) {
		*attr_type = CKA_LABEL;
		*data_type = DATA_TYPE_STRING;

	} else if (g_str_equal (prop_name, "Type")) {
		*attr_type = CKA_G_SCHEMA;
		*data_type = DATA_TYPE_STRING;

	} else if (g_str_equal (prop_name, "Locked")) {
		*attr_type = CKA_G_LOCKED;
		*data_type = DATA_TYPE_BOOL;

	} else if (g_str_equal (prop_name, "Created")) {
		*attr_type = CKA_G_CREATED;
		*data_type = DATA_TYPE_TIME;

	} else if (g_str_equal (prop_name, "Modified")) {
		*attr_type = CKA_G_MODIFIED;
		*data_type = DATA_TYPE_TIME;

	} else if (g_str_equal (prop_name, "Attributes")) {
		*attr_type = CKA_G_FIELDS;
		*data_type = DATA_TYPE_FIELDS;

	} else {
		return FALSE;
	}

	return TRUE;
}

static gboolean
attribute_to_property (CK_ATTRIBUTE_TYPE attr_type, const gchar **prop_name, DataType *data_type)
{
	g_assert (prop_name);
	g_assert (data_type);

	switch (attr_type) {
	case CKA_LABEL:
		*prop_name = "Label";
		*data_type = DATA_TYPE_STRING;
		break;
	case CKA_G_SCHEMA:
		*prop_name = "Type";
		*data_type = DATA_TYPE_STRING;
		break;
	case CKA_G_LOCKED:
		*prop_name = "Locked";
		*data_type = DATA_TYPE_BOOL;
		break;
	case CKA_G_CREATED:
		*prop_name = "Created";
		*data_type = DATA_TYPE_TIME;
		break;
	case CKA_G_MODIFIED:
		*prop_name = "Modified";
		*data_type = DATA_TYPE_TIME;
		break;
	case CKA_G_FIELDS:
		*prop_name = "Attributes";
		*data_type = DATA_TYPE_FIELDS;
		break;
	default:
		return FALSE;
	};

	return TRUE;
}

typedef void (*IterAppendFunc) (DBusMessageIter*, GckAttribute*);
typedef gboolean (*IterGetFunc) (DBusMessageIter*, GckAttribute*);

static void
iter_append_string (DBusMessageIter *iter, GckAttribute *attr)
{
	gchar *value;

	g_assert (iter);
	g_assert (attr);

	if (attr->length == 0) {
		value = "";
		dbus_message_iter_append_basic (iter, DBUS_TYPE_STRING, &value);
	} else {
		value = g_strndup ((const gchar*)attr->value, attr->length);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_STRING, &value);
		g_free (value);
	}
}

static gboolean
iter_get_string (DBusMessageIter *iter, GckAttribute* attr)
{
	const char *value;

	g_assert (iter);
	g_assert (attr);

	g_return_val_if_fail (dbus_message_iter_get_arg_type (iter) == DBUS_TYPE_STRING, FALSE);
	dbus_message_iter_get_basic (iter, &value);
	if (value == NULL)
		value = "";
	gck_attribute_init_string (attr, attr->type, value);
	return TRUE;
}

static void
iter_append_bool (DBusMessageIter *iter, GckAttribute *attr)
{
	dbus_bool_t value;

	g_assert (iter);
	g_assert (attr);

	value = gck_attribute_get_boolean (attr) ? TRUE : FALSE;
	dbus_message_iter_append_basic (iter, DBUS_TYPE_BOOLEAN, &value);
}

static gboolean
iter_get_bool (DBusMessageIter *iter, GckAttribute* attr)
{
	dbus_bool_t value;

	g_assert (iter);
	g_assert (attr);

	g_return_val_if_fail (dbus_message_iter_get_arg_type (iter) == DBUS_TYPE_BOOLEAN, FALSE);
	dbus_message_iter_get_basic (iter, &value);
	gck_attribute_init_boolean (attr, attr->type, value ? TRUE : FALSE);
	return TRUE;
}

static void
iter_append_time (DBusMessageIter *iter, GckAttribute *attr)
{
	gint64 value;
	struct tm tm;
	gchar buf[15];

	g_assert (iter);
	g_assert (attr);

	if (attr->length == 0) {
		value = -1;

	} else if (!attr->value || attr->length != 16) {
		g_warning ("invalid length of time attribute");
		value = -1;

	} else {
		memset (&tm, 0, sizeof (tm));
		memcpy (buf, attr->value, 14);
		buf[14] = 0;

		if (!strptime(buf, "%Y%m%d%H%M%S", &tm)) {
			g_warning ("invalid format of time attribute");
			value = -1;
		}

		/* Convert to seconds since epoch */
		value = timegm (&tm);
		if (value < 0) {
			g_warning ("invalid time attribute");
			value = -1;
		}
	}

	dbus_message_iter_append_basic (iter, DBUS_TYPE_INT64, &value);
}

static gboolean
iter_get_time (DBusMessageIter *iter, GckAttribute* attr)
{
	time_t time;
	struct tm tm;
	gchar buf[20];
	gint64 value;

	g_assert (iter);
	g_assert (attr);

	g_return_val_if_fail (dbus_message_iter_get_arg_type (iter) == DBUS_TYPE_INT64, FALSE);
	dbus_message_iter_get_basic (iter, &value);
	if (value < 0) {
		gck_attribute_init_empty (attr, attr->type);
		return TRUE;
	}

	time = value;
	if (!gmtime_r (&time, &tm))
		g_return_val_if_reached (FALSE);

	if (!strftime (buf, sizeof (buf), "%Y%m%d%H%M%S00", &tm))
		g_return_val_if_reached (FALSE);

	gck_attribute_init (attr, attr->type, buf, 16);
	return TRUE;
}

static void
iter_append_fields (DBusMessageIter *iter, GckAttribute *attr)
{
	DBusMessageIter array;
	DBusMessageIter dict;
	const gchar *ptr;
	const gchar *last;
	const gchar *name;
	gsize n_name;
	const gchar *value;
	gsize n_value;
	gchar *string;

	g_assert (iter);
	g_assert (attr);

	ptr = (gchar*)attr->value;
	last = ptr + attr->length;
	g_return_if_fail (ptr || last == ptr);

	dbus_message_iter_open_container (iter, DBUS_TYPE_ARRAY, "{ss}", &array);

	while (ptr && ptr != last) {
		g_assert (ptr < last);

		name = ptr;
		ptr = memchr (ptr, 0, last - ptr);
		if (ptr == NULL) /* invalid */
			break;

		n_name = ptr - name;
		value = ++ptr;
		ptr = memchr (ptr, 0, last - ptr);
		if (ptr == NULL) /* invalid */
			break;

		n_value = ptr - value;
		++ptr;

		dbus_message_iter_open_container (&array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);

		string = g_strndup (name, n_name);
		dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);
		g_free (string);

		string = g_strndup (value, n_value);
		dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &string);
		g_free (string);

		dbus_message_iter_close_container (&array, &dict);
	}

	dbus_message_iter_close_container (iter, &array);
}

static gboolean
iter_get_fields (DBusMessageIter *iter, GckAttribute* attr)
{
	DBusMessageIter array;
	DBusMessageIter dict;
	GString *result;
	const gchar *string;

	g_assert (iter);

	result = g_string_new ("");

	g_return_val_if_fail (dbus_message_iter_get_arg_type (iter) == DBUS_TYPE_ARRAY, FALSE);
	dbus_message_iter_recurse (iter, &array);

	while (dbus_message_iter_get_arg_type (&array) == DBUS_TYPE_DICT_ENTRY) {
		dbus_message_iter_recurse (&array, &dict);

		/* Key */
		g_return_val_if_fail (dbus_message_iter_get_arg_type (&dict) == DBUS_TYPE_STRING, FALSE);
		dbus_message_iter_get_basic (&dict, &string);
		g_string_append (result, string);
		g_string_append_c (result, '\0');

		dbus_message_iter_next (&dict);

		/* Value */
		g_return_val_if_fail (dbus_message_iter_get_arg_type (&dict) == DBUS_TYPE_STRING, FALSE);
		dbus_message_iter_get_basic (&dict, &string);
		g_string_append (result, string);
		g_string_append_c (result, '\0');

		dbus_message_iter_next (&array);
	}

	gck_attribute_init (attr, attr->type, result->str, result->len);
	g_string_free (result, TRUE);
	return TRUE;
}

static void
iter_append_variant (DBusMessageIter *iter, DataType data_type, GckAttribute *attr)
{
	DBusMessageIter sub;
	IterAppendFunc func;
	const gchar *sig;

	g_assert (iter);
	g_assert (attr);

	switch (data_type) {
	case DATA_TYPE_STRING:
		func = iter_append_string;
		sig = DBUS_TYPE_STRING_AS_STRING;
		break;
	case DATA_TYPE_BOOL:
		func = iter_append_bool;
		sig = DBUS_TYPE_BOOLEAN_AS_STRING;
		break;
	case DATA_TYPE_TIME:
		func = iter_append_time;
		sig = DBUS_TYPE_INT64_AS_STRING;
		break;
	case DATA_TYPE_FIELDS:
		func = iter_append_fields;
		sig = "a{ss}";
		break;
	default:
		g_assert (FALSE);
		break;
	}

	dbus_message_iter_open_container (iter, DBUS_TYPE_VARIANT, sig, &sub);
	(func) (&sub, attr);
	dbus_message_iter_close_container (iter, &sub);
}

static gboolean
iter_get_variant (DBusMessageIter *iter, DataType data_type, GckAttribute *attr)
{
	DBusMessageIter variant;
	IterGetFunc func;
	gboolean ret;
	const gchar *sig;
	char *signature;

	g_assert (iter);
	g_assert (attr);

	g_return_val_if_fail (dbus_message_iter_get_arg_type (iter) == DBUS_TYPE_VARIANT, FALSE);
	dbus_message_iter_recurse (iter, &variant);

	switch (data_type) {
	case DATA_TYPE_STRING:
		func = iter_get_string;
		sig = DBUS_TYPE_STRING_AS_STRING;
		break;
	case DATA_TYPE_BOOL:
		func = iter_get_bool;
		sig = DBUS_TYPE_BOOLEAN_AS_STRING;
		break;
	case DATA_TYPE_TIME:
		func = iter_get_time;
		sig = DBUS_TYPE_INT64_AS_STRING;
		break;
	case DATA_TYPE_FIELDS:
		func = iter_get_fields;
		sig = "a{ss}";
		break;
	default:
		g_assert (FALSE);
		break;
	}

	signature = dbus_message_iter_get_signature (&variant);
	g_return_val_if_fail (signature, FALSE);
	ret = g_str_equal (sig, signature);
	dbus_free (signature);

	if (ret == FALSE)
		return FALSE;

	return (func) (&variant, attr);
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

gboolean
gkd_secret_property_get_type (const gchar *property, CK_ATTRIBUTE_TYPE *type)
{
	DataType data_type;

	g_return_val_if_fail (property, FALSE);
	g_return_val_if_fail (type, FALSE);

	return property_to_attribute (property, type, &data_type);
}

gboolean
gkd_secret_property_parse_all (DBusMessageIter *array, GckAttributes *attrs)
{
	DBusMessageIter dict;
	CK_ATTRIBUTE_TYPE attr_type;
	GckAttribute *attr;
	const char *name;
	DataType data_type;

	g_return_val_if_fail (array, FALSE);
	g_return_val_if_fail (attrs, FALSE);

	while (dbus_message_iter_get_arg_type (array) == DBUS_TYPE_DICT_ENTRY) {
		dbus_message_iter_recurse (array, &dict);

		/* Property name */
		g_return_val_if_fail (dbus_message_iter_get_arg_type (&dict) == DBUS_TYPE_STRING, FALSE);
		dbus_message_iter_get_basic (&dict, &name);
		dbus_message_iter_next (&dict);

		if (!property_to_attribute (name, &attr_type, &data_type))
			return FALSE;

		/* Property value */
		g_return_val_if_fail (dbus_message_iter_get_arg_type (&dict) == DBUS_TYPE_VARIANT, FALSE);
		attr = gck_attributes_add_empty (attrs, attr_type);
		if (!iter_get_variant (&dict, data_type, attr))
			return FALSE;

		dbus_message_iter_next (array);
	}

	return TRUE;
}

gboolean
gkd_secret_property_append_all (DBusMessageIter *array, GckAttributes *attrs)
{
	DBusMessageIter dict;
	GckAttribute *attr;
	DataType data_type;
	const gchar *name;
	gulong num, i;

	g_return_val_if_fail (array, FALSE);
	g_return_val_if_fail (attrs, FALSE);

	num = gck_attributes_count (attrs);
	for (i = 0; i < num; ++i) {
		attr = gck_attributes_at (attrs, i);
		if (!attribute_to_property (attr->type, &name, &data_type))
			g_return_val_if_reached (FALSE);

		dbus_message_iter_open_container (array, DBUS_TYPE_DICT_ENTRY, NULL, &dict);
		dbus_message_iter_append_basic (&dict, DBUS_TYPE_STRING, &name);
		iter_append_variant (&dict, data_type, attr);
		dbus_message_iter_close_container (array, &dict);
	}

	return TRUE;
}

gboolean
gkd_secret_property_append_variant (DBusMessageIter *iter, GckAttribute *attr)
{
	const gchar *property;
	DataType data_type;

	g_return_val_if_fail (attr, FALSE);
	g_return_val_if_fail (iter, FALSE);

	if (!attribute_to_property (attr->type, &property, &data_type))
		return FALSE;
	iter_append_variant (iter, data_type, attr);
	return TRUE;
}

gboolean
gkd_secret_property_parse_variant (DBusMessageIter *iter, const gchar *property,
                                   GckAttribute *attr)
{
	CK_ATTRIBUTE_TYPE attr_type;
	DataType data_type;

	g_return_val_if_fail (attr, FALSE);
	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (property, FALSE);

	if (!property_to_attribute (property, &attr_type, &data_type))
		return FALSE;

	attr->type = attr_type;
	return iter_get_variant (iter, data_type, attr);
}

gboolean
gkd_secret_property_parse_fields (DBusMessageIter *iter, GckAttribute *attr)
{
	g_return_val_if_fail (attr, FALSE);
	g_return_val_if_fail (iter, FALSE);

	attr->type = CKA_G_FIELDS;
	return iter_get_fields (iter, attr);
}
