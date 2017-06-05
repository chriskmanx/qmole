
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gp11-test.h"

#define ATTR_TYPE 55
#define ATTR_DATA "TEST DATA"
#define N_ATTR_DATA ((gsize)9)

DEFINE_TEST(init_memory)
{
	GP11Attribute attr;
	
	g_assert (sizeof (attr) == sizeof (CK_ATTRIBUTE));
	
	gp11_attribute_init (&attr, ATTR_TYPE, ATTR_DATA, N_ATTR_DATA);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == N_ATTR_DATA);
	g_assert (memcmp (attr.value, ATTR_DATA, attr.length) == 0);
	
	gp11_attribute_clear (&attr);
}

DEFINE_TEST(init_boolean)
{
	GP11Attribute attr;

	gp11_attribute_init_boolean (&attr, ATTR_TYPE, TRUE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_BBOOL));
	g_assert (*((CK_BBOOL*)attr.value) == CK_TRUE);

	gp11_attribute_clear (&attr);
}

DEFINE_TEST(init_date)
{
	GP11Attribute attr;
	CK_DATE ck_date;
	GDate *date;

	date = g_date_new_dmy(05, 06, 1960);
	memcpy (ck_date.year, "1960", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	gp11_attribute_init_date (&attr, ATTR_TYPE, date);
	g_date_free (date);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_DATE));
	g_assert (memcmp (attr.value, &ck_date, attr.length) == 0);
	
	gp11_attribute_clear (&attr);
}

DEFINE_TEST(init_ulong)
{
	GP11Attribute attr;
	
	gp11_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG*)attr.value) == 88);

	gp11_attribute_clear (&attr);
}

DEFINE_TEST(init_string)
{
	GP11Attribute attr;
	
	gp11_attribute_init_string (&attr, ATTR_TYPE, "a test string");
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == strlen ("a test string"));
	g_assert (memcmp (attr.value, "a test string", attr.length) == 0);

	gp11_attribute_clear (&attr);
}

DEFINE_TEST(init_invalid)
{
	GP11Attribute attr;
	
	gp11_attribute_init_invalid (&attr, ATTR_TYPE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == (gulong)-1);
	g_assert (attr.value == NULL);

	g_assert (gp11_attribute_is_invalid (&attr));
	gp11_attribute_clear (&attr);
}

DEFINE_TEST(init_empty)
{
	GP11Attribute attr;
	
	gp11_attribute_init_empty (&attr, ATTR_TYPE);
	g_assert (attr.type == ATTR_TYPE);
	g_assert (attr.length == 0);
	g_assert (attr.value == NULL);

	gp11_attribute_clear (&attr);
}
	
DEFINE_TEST(new_memory)
{
	GP11Attribute *attr;
	
	attr = gp11_attribute_new (ATTR_TYPE, ATTR_DATA, N_ATTR_DATA);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, attr->length) == 0);
	
	gp11_attribute_free (attr);
}

DEFINE_TEST(new_boolean)
{
	GP11Attribute *attr;

	attr = gp11_attribute_new_boolean (ATTR_TYPE, TRUE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_BBOOL));
	g_assert (*((CK_BBOOL*)attr->value) == CK_TRUE);

	gp11_attribute_free (attr);
}

DEFINE_TEST(new_date)
{
	GP11Attribute *attr;
	CK_DATE ck_date;
	GDate *date;

	date = g_date_new_dmy(05, 06, 1800);
	memcpy (ck_date.year, "1800", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	attr = gp11_attribute_new_date (ATTR_TYPE, date);
	g_date_free (date);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_DATE));
	g_assert (memcmp (attr->value, &ck_date, attr->length) == 0);
	
	gp11_attribute_free (attr);
}

DEFINE_TEST(new_ulong)
{
	GP11Attribute *attr;
	
	attr = gp11_attribute_new_ulong (ATTR_TYPE, 88);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == sizeof (CK_ULONG));
	g_assert (*((CK_ULONG*)attr->value) == 88);

	gp11_attribute_free (attr);
}

DEFINE_TEST(new_string)
{
	GP11Attribute *attr;
	
	attr = gp11_attribute_new_string (ATTR_TYPE, "a test string");
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == strlen ("a test string"));
	g_assert (memcmp (attr->value, "a test string", attr->length) == 0);

	gp11_attribute_free (attr);
}

DEFINE_TEST(new_invalid)
{
	GP11Attribute *attr;
	
	attr = gp11_attribute_new_invalid (ATTR_TYPE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == (gulong)-1);
	g_assert (attr->value == NULL);

	g_assert (gp11_attribute_is_invalid (attr));
	
	gp11_attribute_free (attr);
}

DEFINE_TEST(new_empty)
{
	GP11Attribute *attr;
	
	attr = gp11_attribute_new_empty (ATTR_TYPE);
	g_assert (attr->type == ATTR_TYPE);
	g_assert (attr->length == 0);
	g_assert (attr->value == NULL);

	gp11_attribute_free (attr);
}

DEFINE_TEST(get_boolean)
{
	GP11Attribute *attr;

	attr = gp11_attribute_new_boolean (ATTR_TYPE, TRUE);
	g_assert (gp11_attribute_get_boolean (attr) == TRUE);
	gp11_attribute_free (attr);
}

DEFINE_TEST(get_date)
{
	GP11Attribute *attr;
	CK_DATE ck_date;
	GDate date, date2;

	g_date_set_dmy(&date, 05, 06, 1800);
	memcpy (ck_date.year, "1800", 4);
	memcpy (ck_date.month, "06", 2);
	memcpy (ck_date.day, "05", 2);
	attr = gp11_attribute_new_date (ATTR_TYPE, &date);
	gp11_attribute_get_date (attr, &date2);
	g_assert (g_date_compare (&date, &date2) == 0);
	gp11_attribute_free (attr);
}

DEFINE_TEST(get_ulong)
{
	GP11Attribute *attr;
	
	attr = gp11_attribute_new_ulong (ATTR_TYPE, 88);
	g_assert (gp11_attribute_get_ulong (attr) == 88);
	gp11_attribute_free (attr);
}

DEFINE_TEST(get_string)
{
	GP11Attribute *attr;
	gchar *value;
	
	attr = gp11_attribute_new_string (ATTR_TYPE, "a test string");
	value = gp11_attribute_get_string (attr);
	g_assert (strcmp ("a test string", value) == 0);
	g_free (value);
	gp11_attribute_free (attr);

	/* Should be able to store null strings */
	attr = gp11_attribute_new_string (ATTR_TYPE, NULL);
	value = gp11_attribute_get_string (attr);
	g_assert (value == NULL);
	gp11_attribute_free (attr);
}

DEFINE_TEST(dup_attribute)
{
	GP11Attribute attr, *dup;

	gp11_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	dup = gp11_attribute_dup (&attr);
	gp11_attribute_clear (&attr);
	g_assert (gp11_attribute_get_ulong (dup) == 88);
	g_assert (dup->type == ATTR_TYPE);
	gp11_attribute_free (dup);
	
	/* Should be able to dup null */
	dup = gp11_attribute_dup (NULL);
	g_assert (dup == NULL);
}

DEFINE_TEST(copy_attribute)
{
	GP11Attribute attr, copy;

	gp11_attribute_init_ulong (&attr, ATTR_TYPE, 88);
	gp11_attribute_init_copy (&copy, &attr);
	gp11_attribute_clear (&attr);
	g_assert (gp11_attribute_get_ulong (&copy) == 88);
	g_assert (copy.type == ATTR_TYPE);
	gp11_attribute_clear (&copy);
}

DEFINE_TEST(new_attributes)
{
	GP11Attributes *attrs;
	
	attrs = gp11_attributes_new ();
	g_assert (attrs != NULL);
	g_assert (gp11_attributes_count (attrs) == 0);
	
	gp11_attributes_ref (attrs);
	gp11_attributes_unref (attrs);
	
	gp11_attributes_unref (attrs);
	
	/* Can unref NULL */
	gp11_attributes_unref (NULL);
}

static void
test_attributes_contents (GP11Attributes *attrs, gboolean extras)
{
	GP11Attribute *attr;
	gchar *value;
	GDate date, *check;
	guint count;
	
	g_assert (attrs != NULL);
	count = extras ? 7 : 5;
	g_assert_cmpuint (gp11_attributes_count (attrs), ==, count);
	
	attr = gp11_attributes_at (attrs, 0);
	g_assert (attr->type == 0);
	g_assert (gp11_attribute_get_boolean (attr) == TRUE);
	
	attr = gp11_attributes_at (attrs, 1);
	g_assert (attr->type == 101);
	g_assert (gp11_attribute_get_ulong (attr) == 888);

	attr = gp11_attributes_at (attrs, 2);
	g_assert (attr->type == 202);
	value = gp11_attribute_get_string (attr);
	g_assert (strcmp (value, "string") == 0);
	g_free (value);

	attr = gp11_attributes_at (attrs, 3);
	g_assert (attr->type == 303);
	check = g_date_new_dmy (11, 12, 2008);
	gp11_attribute_get_date (attr, &date);
	g_assert (g_date_compare (&date, check) == 0);
	g_date_free (check);
	
	attr = gp11_attributes_at (attrs, 4);
	g_assert (attr->type == 404);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, N_ATTR_DATA) == 0);
	
	if (!extras)
		return;
	
	attr = gp11_attributes_at (attrs, 5);
	g_assert (attr->type == 505);
	g_assert (attr->length == (gulong)-1);
	g_assert (attr->value == NULL);
	g_assert (gp11_attribute_is_invalid (attr));

	attr = gp11_attributes_at (attrs, 6);
	g_assert (attr->type == 606);
	g_assert (attr->length == 0);
	g_assert (attr->value == NULL);
}

DEFINE_TEST(newv_attributes)
{
	GDate *date = g_date_new_dmy (11, 12, 2008);
	GP11Attributes *attrs;
	attrs = gp11_attributes_newv (0UL, GP11_BOOLEAN, TRUE, 
	                              101UL, GP11_ULONG, 888UL,
	                              202UL, GP11_STRING, "string",
	                              303UL, GP11_DATE, date,
	                              404UL, N_ATTR_DATA, ATTR_DATA,
	                              GP11_INVALID);
	g_date_free (date);

	test_attributes_contents (attrs, FALSE);
	gp11_attributes_unref (attrs);
	
	/* An empty one */
	attrs = gp11_attributes_newv (GP11_INVALID);
	gp11_attributes_unref (attrs);
}

DEFINE_TEST(new_empty_attributes)
{
	GP11Attributes *attrs = gp11_attributes_new_empty (101UL, 202UL, 303UL, 404UL, GP11_INVALID);
	GP11Attribute *attr;
	guint i;
	
	g_assert_cmpuint (gp11_attributes_count (attrs), ==, 4);
	for (i = 0; i < gp11_attributes_count (attrs); ++i) {
		attr = gp11_attributes_at (attrs, i);
		g_assert (attr->type == ((i + 1) * 100) + i + 1);
		g_assert (attr->value == NULL);
		g_assert (attr->length == 0);
	}
}

static GP11Attributes*
help_attributes_valist (int dummy, ...)
{
	GP11Attributes *attrs;
	va_list va;
	
	va_start (va, dummy);
	attrs = gp11_attributes_new_valist (NULL, va);
	va_end (va);
	
	return attrs;
}

DEFINE_TEST(new_valist_attributes)
{
	GP11Attributes *attrs;
	GDate *date = g_date_new_dmy (11, 12, 2008);
	
	attrs = help_attributes_valist (232434243, /* Not used */
	                                0UL, GP11_BOOLEAN, TRUE, 
	                                101UL, GP11_ULONG, 888UL,
	                                202UL, GP11_STRING, "string",
	                                303UL, GP11_DATE, date,
	                                404UL, N_ATTR_DATA, ATTR_DATA,
	                                GP11_INVALID);
	
	g_date_free (date);
	test_attributes_contents (attrs, FALSE);
	gp11_attributes_unref (attrs);	
}

#if 0
DEFINE_ABORT(bad_length)
{
	GP11Attributes *attrs;
	
	/* We should catch this with a warning */
	attrs = gp11_attributes_newv (1UL, G_MAXSSIZE + 500U, GP11_ULONG, "invalid data",
	                              GP11_INVALID);
	
	gp11_attributes_unref (attrs);
}
#endif

DEFINE_TEST(add_data_attributes)
{
	GP11Attributes *attrs;
	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gp11_attributes_new ();
	gp11_attributes_add_boolean (attrs, 0UL, TRUE);
	gp11_attributes_add_ulong (attrs, 101UL, 888);
	gp11_attributes_add_string (attrs, 202UL, "string");
	gp11_attributes_add_date (attrs, 303UL, date);
	g_date_free (date);
	gp11_attributes_add_data (attrs, 404UL, ATTR_DATA, N_ATTR_DATA);
	gp11_attributes_add_invalid (attrs, 505UL);
	gp11_attributes_add_empty (attrs, 606UL);
	test_attributes_contents (attrs, TRUE);
	gp11_attributes_unref (attrs);
}

DEFINE_TEST(add_attributes)
{
	GP11Attributes *attrs;
	GP11Attribute attr;
	
	GDate *date = g_date_new_dmy (11, 12, 2008);
	attrs = gp11_attributes_new ();
	
	gp11_attribute_init_boolean (&attr, 0UL, TRUE);
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);
	
	gp11_attribute_init_ulong (&attr, 101UL, 888);
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);
	
	gp11_attribute_init_string (&attr, 202UL, "string");
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);

	gp11_attribute_init_date (&attr, 303UL, date);
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);
	g_date_free (date);
	
	gp11_attribute_init (&attr, 404UL, ATTR_DATA, N_ATTR_DATA);
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);
	
	gp11_attribute_init_invalid (&attr, 505UL);
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);

	gp11_attribute_init_empty (&attr, 606UL);
	gp11_attributes_add (attrs, &attr);
	gp11_attribute_clear (&attr);

	test_attributes_contents (attrs, TRUE);
	gp11_attributes_unref (attrs);
}


DEFINE_TEST(find_attributes)
{
	GP11Attribute *attr;
	GDate check, *date = g_date_new_dmy (13, 12, 2008);
	gboolean bvalue, ret;
	gulong uvalue;
	gchar *svalue;
	
	GP11Attributes *attrs;
	attrs = gp11_attributes_newv (0UL, GP11_BOOLEAN, TRUE, 
	                              101UL, GP11_ULONG, 888UL,
	                              202UL, GP11_STRING, "string",
	                              303UL, GP11_DATE, date,
	                              404UL, N_ATTR_DATA, ATTR_DATA,
	                              GP11_INVALID);

	attr = gp11_attributes_find (attrs, 404);
	g_assert (attr != NULL);
	g_assert (attr->length == N_ATTR_DATA);
	g_assert (memcmp (attr->value, ATTR_DATA, N_ATTR_DATA) == 0);
	
	ret = gp11_attributes_find_boolean (attrs, 0UL, &bvalue);
	g_assert (ret == TRUE);
	g_assert (bvalue == TRUE);
	
	ret = gp11_attributes_find_ulong (attrs, 101UL, &uvalue);
	g_assert (ret == TRUE);
	g_assert (uvalue == 888);

	ret = gp11_attributes_find_string (attrs, 202UL, &svalue);
	g_assert (ret == TRUE);
	g_assert (svalue != NULL);
	g_assert (strcmp (svalue, "string") == 0);
	g_free (svalue);
	
	ret = gp11_attributes_find_date (attrs, 303UL, &check);
	g_assert (ret == TRUE);
	g_assert (g_date_compare (date, &check) == 0);
	
	gp11_attributes_unref (attrs);
	g_date_free (date);
}
