/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-attribute.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2008, Stefan Walter

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

   Author: Stef Walter <nielsen@memberwebs.com>
*/

#include "config.h"

#include "gck.h"
#include "gck-private.h"

#include <stdlib.h>
#include <string.h>

/**
 * SECTION:gck-attribute
 * @title: GckAttribute
 * @short_description: A PKCS11 attribute.
 *
 * This structure represents a PKCS11 CK_ATTRIBUTE. These attributes contain information
 * about a PKCS11 object. Use gck_object_get() or gck_object_set() to set and retrieve
 * attributes on an object.
 */

/**
 * GckAttribute:
 * @type: The attribute type, such as CKA_LABEL.
 * @value: The value of the attribute. May be NULL.
 * @length: The length of the attribute. May be G_MAXULONG if the attribute is invalid.
 *
 * This structure represents a PKCS11 CK_ATTRIBUTE.
 */

static void
attribute_init (GckAttribute *attr, gulong attr_type,
                gconstpointer value, gsize length,
                GckAllocator allocator)
{
	g_assert (sizeof (GckAttribute) == sizeof (CK_ATTRIBUTE));
	g_assert (allocator);

	memset (attr, 0, sizeof (GckAttribute));
	attr->type = attr_type;
	attr->length = length;
	if (value) {
		attr->value = (allocator) (NULL, length ? length : 1);
		g_assert (attr->value);
		memcpy ((gpointer)attr->value, value, length);
	}
}

/**
 * gck_attribute_init:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The raw value of the attribute.
 * @length: The length of the raw value.
 *
 * Initialize a PKCS#11 attribute. This copies the value memory
 * into an internal buffer.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init (GckAttribute *attr, gulong attr_type,
                    gconstpointer value, gsize length)
{
	g_return_if_fail (attr);
	attribute_init (attr, attr_type, value, length, g_realloc);
}

/**
 * gck_attribute_init_invalid:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 *
 * Initialize a PKCS#11 attribute to an 'invalid' or 'not found'
 * state. Specifically this sets the value length to (CK_ULONG)-1
 * as specified in the PKCS#11 specification.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init_invalid (GckAttribute *attr, gulong attr_type)
{
	g_return_if_fail (attr);
	g_assert (sizeof (GckAttribute) == sizeof (CK_ATTRIBUTE));
	memset (attr, 0, sizeof (GckAttribute));
	attr->type = attr_type;
	attr->length = (gulong)-1;
}

/**
 * gck_attribute_init_empty:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 *
 * Initialize a PKCS#11 attribute to an empty state. The attribute
 * type will be set, but no data will be set.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init_empty (GckAttribute *attr, gulong attr_type)
{
	g_return_if_fail (attr);
	g_assert (sizeof (GckAttribute) == sizeof (CK_ATTRIBUTE));
	memset (attr, 0, sizeof (GckAttribute));
	attr->type = attr_type;
	attr->length = 0;
	attr->value = 0;
}

static void
attribute_init_boolean (GckAttribute *attr, gulong attr_type,
                        gboolean value, GckAllocator allocator)
{
	CK_BBOOL bvalue = value ? CK_TRUE : CK_FALSE;
	attribute_init (attr, attr_type, &bvalue, sizeof (bvalue), allocator);
}

/**
 * gck_attribute_init_boolean:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The boolean value of the attribute.
 *
 * Initialize a PKCS#11 attribute to boolean. This will result
 * in a CK_BBOOL attribute from the PKCS#11 specs.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init_boolean (GckAttribute *attr, gulong attr_type,
                             gboolean value)
{
	g_return_if_fail (attr);
	attribute_init_boolean (attr, attr_type, value, g_realloc);
}

static void
attribute_init_date (GckAttribute *attr, gulong attr_type,
                     const GDate *value, GckAllocator allocator)
{
	gchar buffer[9];
	CK_DATE date;
	g_assert (value);
	g_snprintf (buffer, sizeof (buffer), "%04d%02d%02d",
	            (int)g_date_get_year (value),
	            (int)g_date_get_month (value),
	            (int)g_date_get_day (value));
	memcpy (&date.year, buffer + 0, 4);
	memcpy (&date.month, buffer + 4, 2);
	memcpy (&date.day, buffer + 6, 2);
	attribute_init (attr, attr_type, &date, sizeof (CK_DATE), allocator);
}

/**
 * gck_attribute_init_date:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The date value of the attribute.
 *
 * Initialize a PKCS#11 attribute to a date. This will result
 * in a CK_DATE attribute from the PKCS#11 specs.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init_date (GckAttribute *attr, gulong attr_type,
                          const GDate *value)
{
	g_return_if_fail (attr);
	g_return_if_fail (value);
	attribute_init_date (attr, attr_type, value, g_realloc);
}

static void
attribute_init_ulong (GckAttribute *attr, gulong attr_type,
                      gulong value, GckAllocator allocator)
{
	CK_ULONG uvalue = value;
	attribute_init (attr, attr_type, &uvalue, sizeof (uvalue), allocator);
}

/**
 * gck_attribute_init_ulong:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The ulong value of the attribute.
 *
 * Initialize a PKCS#11 attribute to a unsigned long. This will result
 * in a CK_ULONG attribute from the PKCS#11 specs.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init_ulong (GckAttribute *attr, gulong attr_type,
                           gulong value)
{
	g_return_if_fail (attr);
	attribute_init_ulong (attr, attr_type, value, g_realloc);
}

static void
attribute_init_string (GckAttribute *attr, gulong attr_type,
                       const gchar *value, GckAllocator allocator)
{
	gsize len = value ? strlen (value) : 0;
	attribute_init (attr, attr_type, (gpointer)value, len, allocator);
}

/**
 * gck_attribute_init_string:
 * @attr: An uninitialized attribute.
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The null terminated string value of the attribute.
 *
 * Initialize a PKCS#11 attribute to a string. This will result
 * in an attribute containing the text, but not the null terminator.
 * The text in the attribute will be of the same encoding as you pass
 * to this function.
 *
 * When done with the attribute you should use gck_attribute_clear()
 * to free the internal memory.
 **/
void
gck_attribute_init_string (GckAttribute *attr, gulong attr_type,
                            const gchar *value)
{
	g_return_if_fail (attr);
	attribute_init_string (attr, attr_type, value, g_realloc);
}

/**
 * gck_attribute_new:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The raw value of the attribute.
 * @length: The length of the attribute.
 *
 * Create a new PKCS#11 attribute. The value will be copied
 * into the new attribute.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 **/
GckAttribute*
gck_attribute_new (gulong attr_type, gpointer value, gsize length)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	attribute_init (attr, attr_type, value, length, g_realloc);
	return attr;
}

/**
 * gck_attribute_new_invalid:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 *
 * Create a new PKCS#11 attribute as 'invalid' or 'not found'
 * state. Specifically this sets the value length to (CK_ULONG)-1
 * as specified in the PKCS#11 specification.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 **/
GckAttribute*
gck_attribute_new_invalid (gulong attr_type)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	gck_attribute_init_invalid (attr, attr_type);
	return attr;
}

/**
 * gck_attribute_new_empty:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 *
 * Create a new PKCS#11 attribute with empty data.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 */
GckAttribute*
gck_attribute_new_empty (gulong attr_type)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	gck_attribute_init_empty (attr, attr_type);
	return attr;
}

/**
 * gck_attribute_new_boolean:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The boolean value of the attribute.
 *
 * Initialize a PKCS#11 attribute to boolean. This will result
 * in a CK_BBOOL attribute from the PKCS#11 specs.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 **/
GckAttribute*
gck_attribute_new_boolean (gulong attr_type, gboolean value)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	attribute_init_boolean (attr, attr_type, value, g_realloc);
	return attr;
}

/**
 * gck_attribute_new_date:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The date value of the attribute.
 *
 * Initialize a PKCS#11 attribute to a date. This will result
 * in a CK_DATE attribute from the PKCS#11 specs.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 **/
GckAttribute*
gck_attribute_new_date (gulong attr_type, const GDate *value)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	attribute_init_date (attr, attr_type, value, g_realloc);
	return attr;
}

/**
 * gck_attribute_new_ulong:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The ulong value of the attribute.
 *
 * Initialize a PKCS#11 attribute to a unsigned long. This will result
 * in a CK_ULONG attribute from the PKCS#11 specs.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 **/
GckAttribute*
gck_attribute_new_ulong (gulong attr_type, gulong value)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	attribute_init_ulong (attr, attr_type, value, g_realloc);
	return attr;
}

/**
 * gck_attribute_new_string:
 * @attr_type: The PKCS#11 attribute type to set on the attribute.
 * @value: The null terminated string value of the attribute.
 *
 * Initialize a PKCS#11 attribute to a string. This will result
 * in an attribute containing the text, but not the null terminator.
 * The text in the attribute will be of the same encoding as you pass
 * to this function.
 *
 * Return value: The new attribute. When done with the attribute use
 * gck_attribute_free() to free it.
 **/
GckAttribute*
gck_attribute_new_string (gulong attr_type, const gchar *value)
{
	GckAttribute *attr = g_slice_new0 (GckAttribute);
	attribute_init_string (attr, attr_type, value, g_realloc);
	return attr;
}

/**
 * gck_attribute_is_invalid:
 * @attr: The attribute to check.
 *
 * Check if the PKCS#11 attribute represents 'invalid' or 'not found'
 * according to the PKCS#11 spec. That is, having length
 * of (CK_ULONG)-1.
 *
 * Return value: Whether the attribute represents invalid or not.
 */
gboolean
gck_attribute_is_invalid (GckAttribute *attr)
{
	g_return_val_if_fail (attr, TRUE);
	return attr->length == (gulong)-1;
}

/**
 * gck_attribute_get_boolean:
 * @attr: The attribute to retrieve value from.
 *
 * Get the CK_BBOOL of a PKCS#11 attribute. No conversion
 * is performed. It is an error to pass an attribute to this
 * function unless you're know it's supposed to contain a
 * boolean value.
 *
 * Return value: The boolean value of the attribute.
 */
gboolean
gck_attribute_get_boolean (GckAttribute *attr)
{
	gboolean value;

	g_return_val_if_fail (attr, FALSE);
	if (gck_attribute_is_invalid (attr))
		return FALSE;
	if (!gck_value_to_boolean (attr->value, attr->length, &value))
		g_return_val_if_reached (FALSE);
	return value;
}

/**
 * gck_attribute_get_ulong:
 * @attr: The attribute to retrieve value from.
 *
 * Get the CK_ULONG value of a PKCS#11 attribute. No
 * conversion is performed. It is an error to pass an attribute
 * to this function unless you're know it's supposed to contain
 * a value of the right type.
 *
 * Return value: The ulong value of the attribute.
 */
gulong
gck_attribute_get_ulong (GckAttribute *attr)
{
	gulong value;

	g_return_val_if_fail (attr, FALSE);
	if (gck_attribute_is_invalid (attr))
		return 0;
	if (!gck_value_to_ulong (attr->value, attr->length, &value))
		g_return_val_if_reached ((gulong)-1);
	return value;
}

/**
 * gck_attribute_get_string:
 * @attr: The attribute to retrieve value from.
 *
 * Get the string value of a PKCS#11 attribute. No
 * conversion is performed. It is an error to pass an attribute
 * to this function unless you're know it's supposed to contain
 * a value of the right type.
 *
 * Return value: A null terminated string, to be freed with g_free(),
 * or NULL if the value contained a NULL string.
 */
gchar*
gck_attribute_get_string (GckAttribute *attr)
{
	g_return_val_if_fail (attr, NULL);

	if (gck_attribute_is_invalid (attr))
		return NULL;
	if (!attr->value)
		return NULL;

	return g_strndup ((gchar*)attr->value, attr->length);
}

/**
 * gck_attribute_get_date:
 * @attr: The attribute to retrieve value from.
 * @value: The date value to fill in with the parsed date.
 *
 * Get the CK_DATE of a PKCS#11 attribute. No
 * conversion is performed. It is an error to pass an attribute
 * to this function unless you're know it's supposed to contain
 * a value of the right type.
 */
void
gck_attribute_get_date (GckAttribute *attr, GDate *value)
{
	guint year, month, day;
	gchar buffer[5];
	CK_DATE *date;
	gchar *end;

	g_return_if_fail (attr);

	if (gck_attribute_is_invalid (attr)) {
		g_date_clear (value, 1);
		return;
	}

	g_return_if_fail (attr->length == sizeof (CK_DATE));
	g_return_if_fail (attr->value);
	date = (CK_DATE*)attr->value;

	memset (&buffer, 0, sizeof (buffer));
	memcpy (buffer, date->year, 4);
	year = strtol (buffer, &end, 10);
	g_return_if_fail (end != buffer && !*end);

	memset (&buffer, 0, sizeof (buffer));
	memcpy (buffer, date->month, 2);
	month = strtol (buffer, &end, 10);
	g_return_if_fail (end != buffer && !*end);

	memset (&buffer, 0, sizeof (buffer));
	memcpy (buffer, date->day, 2);
	day = strtol (buffer, &end, 10);
	g_return_if_fail (end != buffer && !*end);

	g_date_set_dmy (value, day, month, year);
}

/**
 * gck_attribute_dup:
 * @attr: The attribute to duplicate.
 *
 * Duplicate the PKCS#11 attribute. All value memory is
 * also copied.
 *
 * Return value: The duplicated attribute. Use gck_attribute_free()
 * to free it.
 */
GckAttribute*
gck_attribute_dup (GckAttribute *attr)
{
	GckAttribute *copy;

	if (!attr)
		return NULL;

	copy = g_slice_new0 (GckAttribute);
	gck_attribute_init_copy (copy, attr);
	return copy;
}

static void
attribute_init_copy (GckAttribute *dest, const GckAttribute *src, GckAllocator allocator)
{
	g_assert (dest);
	g_assert (src);
	g_assert (allocator);

	/*
	 * TODO: Handle stupid, dumb, broken, special cases like
	 * CKA_WRAP_TEMPLATE and CKA_UNWRAP_TEMPLATE.
	 */

	memcpy (dest, src, sizeof (GckAttribute));
	if (src->value) {
		dest->value = (allocator) (NULL, src->length ? src->length : 1);
		g_assert (dest->value);
		memcpy ((gpointer)dest->value, src->value, src->length);
	}
}

/**
 * gck_attribute_init_copy:
 * @dest: An uninitialized attribute.
 * @src: An attribute to copy.
 *
 * Initialize a PKCS#11 attribute as a copy of another attribute.
 * This copies the value memory as well.
 *
 * When done with the copied attribute you should use
 * gck_attribute_clear() to free the internal memory.
 **/
void
gck_attribute_init_copy (GckAttribute *dest, const GckAttribute *src)
{
	g_return_if_fail (dest);
	g_return_if_fail (src);
	attribute_init_copy (dest, src, g_realloc);
}

static void
attribute_clear (GckAttribute *attr, GckAllocator allocator)
{
	g_assert (attr);
	g_assert (allocator);
	if (attr->value)
		(allocator) ((gpointer)attr->value, 0);
	attr->value = NULL;
	attr->length = 0;
}

/**
 * gck_attribute_clear:
 * @attr: Attribute to clear.
 *
 * Clear allocated memory held by a statically allocated attribute.
 * These are usually initialized with gck_attribute_init() or a
 * similar function.
 *
 * The type of the attribute will remain set.
 **/
void
gck_attribute_clear (GckAttribute *attr)
{
	g_return_if_fail (attr);
	attribute_clear (attr, g_realloc);
}

/**
 * gck_attribute_free:
 * @attr: Attribute to free.
 *
 * Free an attribute and its allocated memory. These is usually
 * used with attributes that are allocated by gck_attribute_new()
 * or a similar function.
 **/
void
gck_attribute_free (GckAttribute *attr)
{
	if (attr) {
		attribute_clear (attr, g_realloc);
		g_slice_free (GckAttribute, attr);
	}
}

/**
 * SECTION:gck-attributes
 * @title: GckAttributes
 * @short_description: A set of PKCS11 attributes.
 *
 * A set of GckAttribute structures. These attributes contain information
 * about a PKCS11 object. Use gck_object_get() or gck_object_set() to set and retrieve
 * attributes on an object.
 */

/**
 * GckAttributes:
 *
 * A set of GckAttribute structures.
 */
struct _GckAttributes {
	GArray *array;
	GckAllocator allocator;
	gboolean locked;
	gint refs;
};

/**
 * gck_BOOLEAN:
 *
 * The attribute data is a gboolean. Used with variable argument functions.
 **/

/**
 * gck_ULONG:
 *
 * The attribute data is a gulong. Used with variable argument functions.
 **/

/**
 * gck_STRING:
 *
 * The attribute data is a gchar. Used with variable argument functions.
 **/

/**
 * gck_DATE:
 *
 * The attribute data is a GDate. Used with variable argument functions.
 **/

/**
 * gck_DATE:
 *
 * Signifies that no more attributes follow. Used with variable argument functions.
 **/

/**
 * gck_ATTRIBUTES_TYPE:
 *
 * A boxed type that can be used to hold a GckAttributes object.
 **/

/**
 * GckAllocator:
 * @data: Memory to allocate or deallocate.
 * @length: New length of memory.
 *
 * An allocator used to allocate data for the attributes in this GckAttributes set.
 *
 * This is a function that acts like g_realloc. Specifically it frees when length is
 * set to zero, it allocates when data is set to NULL, and it reallocates when both
 * are valid.
 *
 * Returns: The allocated memory, or NULL when freeing.
 **/

/**
 * gck_attributes_get_boxed_type:
 *
 * Get the boxed type representing a GckAttributes array.
 *
 * Return value: The boxed type.
 **/
GType
gck_attributes_get_boxed_type (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static ("GckAttributes",
		                                     (GBoxedCopyFunc)gck_attributes_ref,
		                                     (GBoxedFreeFunc)gck_attributes_unref);
	return type;
}

/**
 * gck_attributes_new:
 *
 * Create a new GckAttributes array.
 *
 * Return value: The new attributes array. When done with the array
 * release it with gck_attributes_unref().
 **/
GckAttributes*
gck_attributes_new (void)
{
	return gck_attributes_new_full (g_realloc);
}

/**
 * gck_attributes_new_full:
 * @allocator: Memory allocator for attribute data, or NULL for default.
 *
 * Create a new GckAttributes array.
 *
 * Return value: The new attributes array. When done with the array
 * release it with gck_attributes_unref().
 **/
GckAttributes*
gck_attributes_new_full (GckAllocator allocator)
{
	GckAttributes *attrs;

	if (!allocator)
		allocator = g_realloc;

	g_assert (sizeof (GckAttribute) == sizeof (CK_ATTRIBUTE));
	attrs = g_slice_new0 (GckAttributes);
	attrs->array = g_array_new (0, 1, sizeof (GckAttribute));
	attrs->allocator = allocator;
	attrs->refs = 1;
	attrs->locked = FALSE;
	return attrs;
}

/**
 * gck_attributes_new_empty:
 * @attr_type: The first attribute type to add as empty.
 * @...: The arguments should be values of attribute types, terminated with gck_INVALID.
 *
 * Creates an GckAttributes array with empty attributes. The arguments
 * should be values of attribute types, terminated with gck_INVALID.
 *
 * Return value: The new attributes array. When done with the array
 * release it with gck_attributes_unref().
 **/
GckAttributes*
gck_attributes_new_empty (gulong attr_type, ...)
{
	GckAttributes *attrs = gck_attributes_new_full (g_realloc);
	va_list va;

	va_start (va, attr_type);

	while (attr_type != GCK_INVALID) {
		gck_attributes_add_empty (attrs, attr_type);
		attr_type = va_arg (va, gulong);
	}

	va_end (va);

	return attrs;
}

/**
 * gck_attributes_at:
 * @attrs: The attributes array.
 * @index: The attribute index to retrieve.
 *
 * Get attribute at the specified index in the attribute array.
 *
 * Use gck_attributes_count() to determine how many attributes are
 * in the array.
 *
 * Return value: The specified attribute.
 **/
GckAttribute*
gck_attributes_at (GckAttributes *attrs, guint index)
{
	g_return_val_if_fail (attrs && attrs->array, NULL);
	g_return_val_if_fail (index < attrs->array->len, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	return &g_array_index (attrs->array, GckAttribute, index);
}

static GckAttribute*
attributes_push (GckAttributes *attrs)
{
	GckAttribute attr;
	g_assert (!attrs->locked);
	memset (&attr, 0, sizeof (attr));
	g_array_append_val (attrs->array, attr);
	return &g_array_index (attrs->array, GckAttribute, attrs->array->len - 1);
}

/**
 * gck_attributes_add:
 * @attrs: The attributes array to add to
 * @attr: The attribute to add.
 *
 * Add the specified attribute to the array.
 *
 * The value stored in the attribute will be copied.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add (GckAttributes *attrs, GckAttribute *attr)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs && attrs->array, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	g_return_val_if_fail (attr, NULL);
	added = attributes_push (attrs);
	attribute_init_copy (added, attr, attrs->allocator);
	return added;
}

/**
 * gck_attributes_add_data:
 * @attrs: The attributes array to add to.
 * @attr_type: The type of attribute to add.
 * @value: The raw memory of the attribute value.
 * @length: The length of the attribute value.
 *
 * Add an attribute with the specified type and value to the array.
 *
 * The value stored in the attribute will be copied.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_data (GckAttributes *attrs, gulong attr_type,
                          gconstpointer value, gsize length)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	attribute_init (added, attr_type, value, length, attrs->allocator);
	return added;
}

/**
 * gck_attributes_add_invalid:
 * @attrs: The attributes array to add to.
 * @attr_type: The type of attribute to add.
 *
 * Add an attribute with the specified type and an 'invalid' value to the array.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_invalid (GckAttributes *attrs, gulong attr_type)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	gck_attribute_init_invalid (added, attr_type);
	return added;
}

/**
 * gck_attributes_add_empty:
 * @attrs: The attributes array to add.
 * @attr_type: The type of attribute to add.
 *
 * Add an attribute with the specified type, with empty data.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_empty (GckAttributes *attrs, gulong attr_type)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	gck_attribute_init_empty (added, attr_type);
	return added;
}

/**
 * gck_attributes_add_boolean:
 * @attrs: The attributes array to add to.
 * @attr_type: The type of attribute to add.
 * @value: The boolean value to add.
 *
 * Add an attribute with the specified type and value to the array.
 *
 * The value will be stored as a CK_BBOOL PKCS#11 style attribute.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_boolean (GckAttributes *attrs, gulong attr_type, gboolean value)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	attribute_init_boolean (added, attr_type, value, attrs->allocator);
	return added;
}

/**
 * gck_attributes_add_string:
 * @attrs: The attributes array to add to.
 * @attr_type: The type of attribute to add.
 * @value: The null terminated string value to add.
 *
 * Add an attribute with the specified type and value to the array.
 *
 * The value will be copied into the attribute.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_string (GckAttributes *attrs, gulong attr_type, const gchar *value)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	attribute_init_string (added, attr_type, value, attrs->allocator);
	return added;
}

/**
 * gck_attributes_add_date:
 * @attrs: The attributes array to add to.
 * @attr_type: The type of attribute to add.
 * @value: The GDate value to add.
 *
 * Add an attribute with the specified type and value to the array.
 *
 * The value will be stored as a CK_DATE PKCS#11 style attribute.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_date (GckAttributes *attrs, gulong attr_type, const GDate *value)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	attribute_init_date (added, attr_type, value, attrs->allocator);
	return added;
}

/**
 * gck_attributes_add_ulong:
 * @attrs: The attributes array to add to.
 * @attr_type: The type of attribute to add.
 * @value: The gulong value to add.
 *
 * Add an attribute with the specified type and value to the array.
 *
 * The value will be stored as a CK_ULONG PKCS#11 style attribute.
 *
 * Return value: The attribute that was added.
 **/
GckAttribute*
gck_attributes_add_ulong (GckAttributes *attrs, gulong attr_type, gulong value)
{
	GckAttribute *added;
	g_return_val_if_fail (attrs, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);
	added = attributes_push (attrs);
	attribute_init_ulong (added, attr_type, value, attrs->allocator);
	return added;
}

void
gck_attributes_add_all (GckAttributes *attrs, GckAttributes *from)
{
	GckAttribute *attr;
	guint i;

	g_return_if_fail (attrs && attrs->array);
	g_return_if_fail (from && from->array);
	g_return_if_fail (!attrs->locked);

	for (i = 0; i < from->array->len; ++i) {
		attr = &g_array_index (from->array, GckAttribute, i);
		gck_attributes_add (attrs, attr);
	}
}

/**
 * gck_attributes_count:
 * @attrs: The attributes array to count.
 *
 * Get the number of attributes in this attribute array.
 *
 * Return value: The number of contained attributes.
 **/
gulong
gck_attributes_count (GckAttributes *attrs)
{
	g_return_val_if_fail (attrs, 0);
	g_return_val_if_fail (!attrs->locked, 0);
	return attrs->array->len;
}

/**
 * gck_attributes_find:
 * @attrs: The attributes array to search.
 * @attr_type: The type of attribute to find.
 *
 * Find an attribute with the specified type in the array.
 *
 * Return value: The first attribute found with the specified type, or NULL.
 **/
GckAttribute*
gck_attributes_find (GckAttributes *attrs, gulong attr_type)
{
	GckAttribute *attr;
	guint i;

	g_return_val_if_fail (attrs && attrs->array, NULL);
	g_return_val_if_fail (!attrs->locked, NULL);

	for (i = 0; i < attrs->array->len; ++i) {
		attr = gck_attributes_at (attrs, i);
		if (attr->type == attr_type)
			return attr;
	}

	return NULL;
}

/**
 * gck_attributes_find_boolean:
 * @attrs: The attributes array to search.
 * @attr_type: The type of attribute to find.
 * @value: The resulting gboolean value.
 *
 * Find an attribute with the specified type in the array.
 *
 * The attribute (if found) must be of the right size to store
 * a boolean value (ie: CK_BBOOL). If the attribute is marked invalid
 * then it will be treated as not found.
 *
 * Return value: Whether a value was found or not.
 **/
gboolean
gck_attributes_find_boolean (GckAttributes *attrs, gulong attr_type, gboolean *value)
{
	GckAttribute *attr;

	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!attrs->locked, FALSE);

	attr = gck_attributes_find (attrs, attr_type);
	if (!attr || gck_attribute_is_invalid (attr))
		return FALSE;
	*value = gck_attribute_get_boolean (attr);
	return TRUE;
}

/**
 * gck_attributes_find_ulong:
 * @attrs: The attributes array to search.
 * @attr_type: The type of attribute to find.
 * @value: The resulting gulong value.
 *
 * Find an attribute with the specified type in the array.
 *
 * The attribute (if found) must be of the right size to store
 * a unsigned long value (ie: CK_ULONG). If the attribute is marked invalid
 * then it will be treated as not found.
 *
 * Return value: Whether a value was found or not.
 **/
gboolean
gck_attributes_find_ulong (GckAttributes *attrs, gulong attr_type, gulong *value)
{
	GckAttribute *attr;

	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!attrs->locked, FALSE);

	attr = gck_attributes_find (attrs, attr_type);
	if (!attr || gck_attribute_is_invalid (attr))
		return FALSE;
	*value = gck_attribute_get_ulong (attr);
	return TRUE;
}

/**
 * gck_attributes_find_string:
 * @attrs: The attributes array to search.
 * @attr_type: The type of attribute to find.
 * @value: The resulting string value.
 *
 * Find an attribute with the specified type in the array.
 *
 * If the attribute is marked invalid then it will be treated as not found.
 * The resulting string will be null-terminated, and must be freed by the caller
 * using g_free().
 *
 * Return value: Whether a value was found or not.
 **/
gboolean
gck_attributes_find_string (GckAttributes *attrs, gulong attr_type, gchar **value)
{
	GckAttribute *attr;

	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!attrs->locked, FALSE);

	attr = gck_attributes_find (attrs, attr_type);
	if (!attr || gck_attribute_is_invalid (attr))
		return FALSE;
	*value = gck_attribute_get_string (attr);
	return TRUE;
}

/**
 * gck_attributes_find_date:
 * @attrs: The attributes array to search.
 * @attr_type: The type of attribute to find.
 * @value: The resulting GDate value.
 *
 * Find an attribute with the specified type in the array.
 *
 * The attribute (if found) must be of the right size to store
 * a date value (ie: CK_DATE). If the attribute is marked invalid
 * then it will be treated as not found.
 *
 * Return value: Whether a value was found or not.
 **/
gboolean
gck_attributes_find_date (GckAttributes *attrs, gulong attr_type, GDate *value)
{
	GckAttribute *attr;

	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (!attrs->locked, FALSE);

	attr = gck_attributes_find (attrs, attr_type);
	if (!attr || gck_attribute_is_invalid (attr))
		return FALSE;
	gck_attribute_get_date (attr, value);
	return TRUE;
}

/**
 * gck_attributes_ref:
 * @attrs: An attribute array
 *
 * Reference this attributes array.
 *
 * Returns: The attributes.
 **/
GckAttributes*
gck_attributes_ref (GckAttributes *attrs)
{
	g_return_val_if_fail (attrs, NULL);
	g_atomic_int_inc (&attrs->refs);
	return attrs;
}

/**
 * gck_attributes_unref:
 * @attrs: An attribute array
 *
 * Unreference this attribute array.
 *
 * When all outstanding references are NULL, the array will be freed.
 */
void
gck_attributes_unref (GckAttributes *attrs)
{
	guint i;

	if (!attrs)
		return;

	if (g_atomic_int_dec_and_test (&attrs->refs)) {
		g_return_if_fail (attrs->array);
		g_return_if_fail (!attrs->locked);
		for (i = 0; i < attrs->array->len; ++i)
			attribute_clear (gck_attributes_at (attrs, i), attrs->allocator);
		g_array_free (attrs->array, TRUE);
		attrs->array = NULL;
		g_slice_free (GckAttributes, attrs);
	}
}

/* -------------------------------------------------------------------------------------------
 * INTERNAL
 *
 * The idea is that while we're processing a GckAttributes array (via PKCS#11
 * C_GetAtributeValue for example) the calling application shouldn't access those
 * attributes at all, except to ref or unref them.
 *
 * We try to help debug this with our 'locked' states. The various processing
 * functions that accept GckAttributes lock the attributes while handing
 * them off to be processed (perhaps in a different thread). We check this locked
 * flag in all public functions accessing GckAttributes.
 *
 * The reason we don't use thread safe or atomic primitives here, is because:
 *  a) The attributes are 'locked' by the same thread that prepares the call.
 *  b) This is a debugging feature, and should not be relied on for correctness.
 */

void
_gck_attributes_lock (GckAttributes *attrs)
{
	g_assert (attrs);
	g_assert (!attrs->locked);
	attrs->locked = TRUE;
}

void
_gck_attributes_unlock (GckAttributes *attrs)
{
	g_assert (attrs);
	g_assert (attrs->locked);
	attrs->locked = FALSE;
}

CK_ATTRIBUTE_PTR
_gck_attributes_prepare_in (GckAttributes *attrs, CK_ULONG_PTR n_attrs)
{
	GckAttribute *attr;
	guint i;

	g_assert (attrs);
	g_assert (n_attrs);
	g_assert (attrs->locked);

	/* Prepare the attributes to receive their length */

	for (i = 0; i < attrs->array->len; ++i) {
		attr = &g_array_index (attrs->array, GckAttribute, i);
		attribute_clear (attr, attrs->allocator);
	}

	*n_attrs = attrs->array->len;
	return (CK_ATTRIBUTE_PTR)attrs->array->data;
}

CK_ATTRIBUTE_PTR
_gck_attributes_commit_in (GckAttributes *attrs, CK_ULONG_PTR n_attrs)
{
	GckAttribute *attr;
	guint i;

	g_assert (attrs);
	g_assert (n_attrs);
	g_assert (attrs->locked);

	/* Allocate each attribute with the length that was set */

	for (i = 0; i < attrs->array->len; ++i) {
		attr = &g_array_index (attrs->array, GckAttribute, i);
		g_assert (!attr->value);
		if (attr->length != 0 && attr->length != (gulong)-1) {
			attr->value = (attrs->allocator) (NULL, attr->length);
			g_assert (attr->value);
		}
	}

	*n_attrs = attrs->array->len;
	return (CK_ATTRIBUTE_PTR)attrs->array->data;
}

CK_ATTRIBUTE_PTR
_gck_attributes_commit_out (GckAttributes *attrs, CK_ULONG_PTR n_attrs)
{
	g_assert (attrs);
	g_assert (n_attrs);
	g_assert (attrs->locked);

	*n_attrs = attrs->array->len;
	return (CK_ATTRIBUTE_PTR)attrs->array->data;
}
