
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gck-test.h"

#define ATTR_TYPE 55
#define ATTR_DATA "TEST DATA"
#define N_ATTR_DATA ((gsize)9)

DEFINE_TEST(init_memory)
{
	GckAttribute attr;

	g_assert (sizeof (attr) == sizeof (CK_ATTRIBUTE));

	gck_attribute_init (&attr, ATTR_TYPE, ATTR_DATA, N_ATTR_DATA);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == N_ATTR_DATA);
	g_assert (memcmp (attr.value, ATTR_DATA, attr.length) == 0);

	gck_attribute_clear (&attr);
}

DEFINE_TEST(value_to_boolean)
{
	CK_BBOOL data = CK_TRUE;
	gboolean result = FALSE;

	if (!gck_value_to_boolean (&data, sizeof (data), &result))
		g_assert_not_reached ();

	g_assert (result == TRUE);

	if (!gck_value_to_boolean (&data, sizeof (data), NULL))
		g_assert_not_reached ();

	/* Should fail */
	if (gck_value_to_boolean (&data, 0, NULL))
		g_assert_not_reached ();
	if (gck_value_to_boolean (&data, 2, NULL))
		g_assert_not_reached ();
	if (gck_value_to_boolean (&data, (CK_ULONG)-1, NULL))
		g_assert_not_reached ();
}

DEFINE_TEST(value_to_ulong)
{
	CK_ULONG data = 34343;
	gulong result = 0;

	if (!gck_value_to_ulong (&data, sizeof (data), &result))
		g_assert_not_reached ();

	g_assert (result == 34343);

	if (!gck_value_to_ulong (&data, sizeof (data), NULL))
		g_assert_not_reached ();

	/* Should fail */
	if (gck_value_to_ulong (&data, 0, NULL))
		g_assert_not_reached ();
	if (gck_value_to_ulong (&data, 2, NULL))
		g_assert_not_reached ();
	if (gck_value_to_ulong (&data, (CK_ULONG)-1, NULL))
		g_assert_not_reached ();
}

DEFINE_TEST(init_boolean)
{
	GckAttribute attr;

	gck_attribute_init_boolean (&attr, ATTR_TYPE, TRUE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_BBOOL));
	g_assert (*((CK_BBOOL*)attr.value) == CK_TRUE);

	gck_attribute_clear (&attr);
}

DEFINE_TEST(init_date)
{
	GckAttribute attr;
	CK_DATE ck_date;
	GDate *date;

	date = g_date_new_dmy(05, 06, 1960);
	memcpy (ck_date.year, "1960", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	gck_attribute_init_date (&attr, ATTR_TYPE, date);
	g_date_free (date);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_DATE));
	g_assert (memcmp (attr.value, &ck_date, attr.length) == 0);

	gck_attribute_clear (&attr);
}

DEFINE_TEST(init_ulong)
{
	GckAttribute attr;

	gck_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG*)attr.value) == 88);

	gck_attribute_clear (&attr);
}

DEFINE_TEST(init_string)
{
	GckAttribute attr;

	gck_attribute_init_string (&attr, ATTR_TYPE, "a test string");
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == strlen ("a test string"));
	g_assert (memcmp (attr.value, "a test string", attr.length) == 0);

	gck_attribute_clear (&attr);
}

DEFINE_TEST(init_invalid)
{
	GckAttribute attr;

	gck_attribute_init_invalid (&attr, ATTR_TYPE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == (gulong)-1);
	g_assert (attr.value == NULL);

	g_assert (gck_attribute_is_invalid (&attr));
	gck_attribute_clear (&attr);
}

DEFINE_TEST(init_empty)
{
	GckAttribute attr;

	gck_attribute_init_empty (&attr, ATTR_TYPE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == 0);
	g_assert (attr.value == NULL);

	gck_attribute_clear (&attr);
}

DEFINE_TEST(new_memory)
{
	GckAttribute *attr;

	attr = gck_attribute_new (ATTR_TYPE, ATTR_DATA, N_ATTR_DATA);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, attr->length) == 0);

	gck_attribute_free (attr);
}

DEFINE_TEST(new_boolean)
{
	GckAttribute *attr;

	attr = gck_attribute_new_boolean (ATTR_TYPE, TRUE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_BBOOL));
	g_assert (*((CK_BBOOL*)attr->value) == CK_TRUE);

	gck_attribute_free (attr);
}

DEFINE_TEST(new_date)
{
	GckAttribute *attr;
	CK_DATE ck_date;
	GDate *date;

	date = g_date_new_dmy(05, 06, 1800);
	memcpy (ck_date.year, "1800", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	attr = gck_attribute_new_date (ATTR_TYPE, date);
	g_date_free (date);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_DATE));
	g_assert (memcmp (attr->value, &ck_date, attr->length) == 0);

	gck_attribute_free (attr);
}

DEFINE_TEST(new_ulong)
{
	GckAttribute *attr;

	attr = gck_attribute_new_ulong (ATTR_TYPE, 88);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG*)attr->value) == 88);

	gck_attribute_free (attr);
}

DEFINE_TEST(new_string)
{
	GckAttribute *attr;

	attr = gck_attribute_new_string (ATTR_TYPE, "a test string");
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == strlen ("a test string"));
	g_assert (memcmp (attr->value, "a test string", attr->length) == 0);

	gck_attribute_free (attr);
}

DEFINE_TEST(new_invalid)
{
	GckAttribute *attr;

	attr = gck_attribute_new_invalid (ATTR_TYPE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == (gulong)-1);
	g_assert (attr->value == NULL);

	g_assert (gck_attribute_is_invalid (attr));

	gck_attribute_free (attr);
}

DEFINE_TEST(new_empty)
{
	GckAttribute *attr;

	attr = gck_attribute_new_empty (ATTR_TYPE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == 0);
	g_assert (attr->value == NULL);

	gck_attribute_free (attr);
}

DEFINE_TEST(get_boolean)
{
	GckAttribute *attr;

	attr = gck_attribute_new_boolean (ATTR_TYPE, TRUE);
	g_assert (gck_attribute_get_boolean (attr) == TRUE);
	gck_attribute_free (attr);
}

DEFINE_TEST(get_date)
{
	GckAttribute *attr;
	CK_DATE ck_date;
	GDate date, date2;

	g_date_set_dmy(&date, 05, 06, 1800);
	memcpy (ck_date.year, "1800", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	attr = gck_attribute_new_date (ATTR_TYPE, &date);
	gck_attribute_get_date (attr, &date2);
	g_assert (g_date_compare (&date, &date2) == 0);
	gck_attribute_free (attr);
}

DEFINE_TEST(get_ulong)
{
	GckAttribute *attr;

	attr = gck_attribute_new_ulong (ATTR_TYPE, 88);
	g_assert (gck_attribute_get_ulong (attr) == 88);
	gck_attribute_free (attr);
}

DEFINE_TEST(get_string)
{
	GckAttribute *attr;
	gchar *value;

	attr = gck_attribute_new_string (ATTR_TYPE, "a test string");
	value = gck_attribute_get_string (attr);
	g_assert (strcmp ("a test string", value) == 0);
	g_free (value);
	gck_attribute_free (attr);

	/* Should be able to store null strings */
	attr = gck_attribute_new_string (ATTR_TYPE, NULL);
	value = gck_attribute_get_string (attr);
	g_assert (value == NULL);
	gck_attribute_free (attr);
}

DEFINE_TEST(dup_attribute)
{
	GckAttribute attr, *dup;

	gck_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	dup = gck_attribute_dup (&attr);
	gck_attribute_clear (&attr);
	g_assert (gck_attribute_get_ulong (dup) == 88);
	g_assert (dup->type == ATTR_TYPE);
	gck_attribute_free (dup);

	/* Should be able to dup null */
	dup = gck_attribute_dup (NULL);
	g_assert (dup == NULL);
}

DEFINE_TEST(copy_attribute)
{
	GckAttribute attr, copy;

	gck_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	gck_attribute_init_copy (&copy, &attr);
	gck_attribute_clear (&attr);
	g_assert (gck_attribute_get_ulong (&copy) == 88);
	g_assert (copy.type == ATTR_TYPE);
	gck_attribute_clear (&copy);
}

DEFINE_TEST(new_attributes)
{
	GckAttributes *attrs;

	attrs = gck_attributes_new ();
	g_assert (attrs != NULL);
	g_assert (gck_attributes_count (attrs) == 0);

	gck_attributes_ref (attrs);
	gck_attributes_unref (attrs);

	gck_attributes_unref (attrs);

	/* Can unref NULL */
	gck_attributes_unref (NULL);
}

static void
test_attributes_contents (GckAttributes *attrs, gboolean extras)
{
	GckAttribute *attr;
	gchar *value;
	GDate date, *check;
	guint count;

	g_assert (attrs != NULL);
	count = extras ? 7 : 5;
	g_assert_cmpuint (gck_attributes_count (attrs), ==, count);

	attr = gck_attributes_at (attrs, 0);
	g_assert (attr->type == 0);
	g_assert (gck_attribute_get_boolean (attr) == TRUE);

	attr = gck_attributes_at (attrs, 1);
	g_assert (attr->type == 101);
	g_assert (gck_attribute_get_ulong (attr) == 888);

	attr = gck_attributes_at (attrs, 2);
	g_assert (attr->type == 202);
	value = gck_attribute_get_string (attr);
	g_assert (strcmp (value, "string") == 0);
	g_free (value);

	attr = gck_attributes_at (attrs, 3);
	g_assert (attr->type == 303);
	check = g_date_new_dmy (11, 12, 2008);
	gck_attribute_get_date (attr, &date);
	g_assert (g_date_compare (&date, check) == 0);
	g_date_free (check);

	attr = gck_attributes_at (attrs, 4);
	g_assert (attr->type == 404);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, N_ATTR_DATA) == 0);

	if (!extras)
		return;

	attr = gck_attributes_at (attrs, 5);
	g_assert (attr->type == 505);
	g_assert (attr->length == (gulong)-1);
	g_assert (attr->value == NULL);
	g_assert (gck_attribute_is_invalid (attr));

	attr = gck_attributes_at (attrs, 6);
	g_assert (attr->type == 606);
	g_assert (attr->length == 0);
	g_assert (attr->value == NULL);
}

DEFINE_TEST(new_empty_attributes)
{
	GckAttributes *attrs = gck_attributes_new_empty (101UL, 202UL, 303UL, 404UL, GCK_INVALID);
	GckAttribute *attr;
	guint i;

	g_assert_cmpuint (gck_attributes_count (attrs), ==, 4);
	for (i = 0; i < gck_attributes_count (attrs); ++i) {
		attr = gck_attributes_at (attrs, i);
		g_assert (attr->type == ((i + 1) * 100) + i + 1);
		g_assert (attr->value == NULL);
		g_assert (attr->length == 0);
	}
}

DEFINE_TEST(add_data_attributes)
{
	GckAttributes *attrs;
	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, 0UL, TRUE);
	gck_attributes_add_ulong (attrs, 101UL, 888);
	gck_attributes_add_string (attrs, 202UL, "string");
	gck_attributes_add_date (attrs, 303UL, date);
	g_date_free (date);
	gck_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);
	gck_attributes_add_invalid (attrs, 505UL);
	gck_attributes_add_empty (attrs, 606UL);
	test_attributes_contents (attrs, TRUE);
	gck_attributes_unref (attrs);
}

DEFINE_TEST(add_attributes)
{
	GckAttributes *attrs;
	GckAttribute attr;

	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gck_attributes_new ();

	gck_attribute_init_boolean (&attr, 0UL, TRUE);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_ulong (&attr, 101UL, 888);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);
	gck_attribute_init_string (&attr, 202UL, "string");
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_date (&attr, 303UL, date);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);
	g_date_free (date);

	gck_attribute_init (&attr, 404UL, ATTR_DATA, N_ATTR_DATA);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_invalid (&attr, 505UL);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	gck_attribute_init_empty (&attr, 606UL);
	gck_attributes_add (attrs, &attr);
	gck_attribute_clear (&attr);

	test_attributes_contents (attrs, TRUE);
	gck_attributes_unref (attrs);
}

DEFINE_TEST(add_all_attributes)
{
	GckAttributes *attrs;
	GckAttributes *copy;
	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, 0UL, TRUE);
	gck_attributes_add_ulong (attrs, 101UL, 888);
	gck_attributes_add_string (attrs, 202UL, "string");
	gck_attributes_add_date (attrs, 303UL, date);
	g_date_free (date);
	gck_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);
	gck_attributes_add_invalid (attrs, 505UL);
	gck_attributes_add_empty (attrs, 606UL);

	copy = gck_attributes_new ();
	gck_attributes_add_all (copy, attrs);
	test_attributes_contents (copy, TRUE);

	gck_attributes_unref (attrs);
	gck_attributes_unref (copy);
}


DEFINE_TEST(find_attributes)
{
	GckAttribute *attr;
	GDate check, *date = g_date_new_dmy (13, 12, 2008);
	gboolean bvalue, ret;
	gulong uvalue;
	gchar *svalue;

	GckAttributes *attrs = gck_attributes_new ();
	gck_attributes_add_boolean (attrs, 0UL, TRUE);
	gck_attributes_add_ulong (attrs, 101UL, 888UL);
	gck_attributes_add_string (attrs, 202UL, "string");
	gck_attributes_add_date (attrs, 303UL, date);
	gck_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);

	attr = gck_attributes_find (attrs, 404);
	g_assert (attr != NULL);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, N_ATTR_DATA) == 0);

	ret = gck_attributes_find_boolean (attrs, 0UL, &bvalue);
	g_assert (ret == TRUE);
	g_assert (bvalue == TRUE);

	ret = gck_attributes_find_ulong (attrs, 101UL, &uvalue);
	g_assert (ret == TRUE);
	g_assert (uvalue == 888);

	ret = gck_attributes_find_string (attrs, 202UL, &svalue);
	g_assert (ret == TRUE);
	g_assert (svalue != NULL);
	g_assert (strcmp (svalue, "string") == 0);
	g_free (svalue);

	ret = gck_attributes_find_date (attrs, 303UL, &check);
	g_assert (ret == TRUE);
	g_assert (g_date_compare (date, &check) == 0);

	gck_attributes_unref (attrs);
	g_date_free (date);
}
