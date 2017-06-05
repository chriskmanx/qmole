/* $Id: e2_utf8.c 2746 2013-09-19 22:59:03Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>
Portion _maybe_ copyright (C) 2004-2005 Tarot Osuji

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/utils/e2_utf8.c
@brief utf8 string utilities

This file contains utilitiy functions for utf8 string handling.
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_utf8.h"
#include "e2_cl_option.h"


/**
@brief get position in string @a str

This function acts like g_utf8_offset_to_pointer() except that if it finds a
decomposable character it consumes the decomposition length from the given
offset.  So it's useful when the offset was calculated for the normalized
version of str, but we need a pointer to str itself

@param str string to scan
@param offset character offset within @a str

@return the resulting pointer
*/
static const gchar *_e2_utf8_pointer_from_offset_skipping_decomp
	(const gchar *str, gint offset)
{
	const gchar *p;

	p = str;
	while (offset > 0)
	{
		gsize decomp_len;
#ifdef USE_GLIB2_30
		decomp_len = g_unichar_fully_decompose(g_utf8_get_char (p), FALSE, NULL, 0);
#else
		gunichar *decomp;
		decomp = g_unicode_canonical_decomposition (g_utf8_get_char (p), &decomp_len);
		g_free (decomp);
#endif
		offset -= decomp_len;
		p = g_utf8_next_char (p);
	}
	return p;
}
/**
@brief find first occurrence of @a needle in @a haystack, regardless of case of @a haystack

@param haystack string to scan
@param needle string to find

@return pointer to @a needle in @a haystack, or NULL if not found
*/
const gchar *e2_utf8_strcasestr (const gchar *haystack, const gchar *needle)
{
	g_return_val_if_fail (haystack != NULL, NULL);
	g_return_val_if_fail (needle != NULL, NULL);

	gsize haystack_len, needle_len;

	needle_len = g_utf8_strlen (needle, -1);
	if (needle_len == 0)
		return haystack;

	gchar *p, *caseless_haystack;
	const gchar *retval = NULL;

	p = g_utf8_casefold (haystack, -1);
	caseless_haystack = g_utf8_normalize (p, -1, G_NORMALIZE_ALL);
	g_free (p);

	haystack_len = g_utf8_strlen (caseless_haystack, -1);
	if (haystack_len < needle_len)
		goto cleanup;

	p = caseless_haystack;
	needle_len = strlen (needle);
	gint i = 0;

	while (*p)
	{
		if ((strncmp (p, needle, needle_len) == 0))
		{
			retval = _e2_utf8_pointer_from_offset_skipping_decomp (haystack, i);
			goto cleanup;
		}

		p = g_utf8_next_char (p);
		i++;
	}

cleanup:
	g_free (caseless_haystack);

	return retval;
}
/**
@brief find last occurrence of @a needle in @a haystack, regardless of case of @a haystack

@param haystack string to scan
@param needle string to find

@return pointer to @a needle in @a haystack, or NULL if not found
*/
const gchar *e2_utf8_strrcasestr (const gchar *haystack, const gchar *needle)
{
	g_return_val_if_fail (haystack != NULL, NULL);
	g_return_val_if_fail (needle != NULL, NULL);

	gsize haystack_len, needle_len;

	needle_len = g_utf8_strlen (needle, -1);
	if (needle_len == 0)
		return haystack;

	gchar *p, *caseless_haystack;
	const gchar *retval = NULL;

	p = g_utf8_casefold (haystack, -1);
	caseless_haystack = g_utf8_normalize (p, -1, G_NORMALIZE_ALL);
	g_free (p);

	haystack_len = g_utf8_strlen (caseless_haystack, -1);
	if (haystack_len < needle_len)
		goto cleanup;

	gint i = haystack_len - needle_len;
	p = g_utf8_offset_to_pointer (caseless_haystack, i);
	needle_len = strlen (needle);

	while (p >= caseless_haystack)
	{
		if (strncmp (p, needle, needle_len) == 0)
		{
			retval = _e2_utf8_pointer_from_offset_skipping_decomp (haystack, i);
			goto cleanup;
		}

		p = g_utf8_prev_char (p);
		i--;
	}

cleanup:
	g_free (caseless_haystack);

	return retval;
}
/**
@brief compare utf8 strings @a s1 and @a s2, without reference to case

@param s1 string to compare
@param s2 other string to compare
@param n1 length of @a s1 (in bytes) or -1 if @a s1 is NULL-terminated
@param n2 length of @a s2 (in bytes) or -1 if @a s2 is NULL-terminated

@return TRUE if @a s1 matches @a s2, apart from any case-difference
*/
gboolean e2_utf8_caseless_match (const gchar *s1, const gchar *s2,
		gssize n1, gssize n2)
{
	gchar *casefold;
	gchar *normalized_s1;
	gchar *normalized_s2;
//	gint len_s1;
//	gint len_s2;
	gboolean ret;

	g_return_val_if_fail (s1 != NULL, FALSE);
	g_return_val_if_fail (s2 != NULL, FALSE);
	g_return_val_if_fail (n1 >= -1, FALSE);
	g_return_val_if_fail (n2 >= -1, FALSE);

	if (n1 == 0 && n2 == 0)
		return TRUE;
	else if (n1 == 0 || n2 == 0)
		return FALSE;

	casefold = g_utf8_casefold (s1, n1);
	normalized_s1 = g_utf8_normalize (casefold, -1, G_NORMALIZE_ALL);
	g_free (casefold);
//	len_s1 = strlen (normalized_s1);

	casefold = g_utf8_casefold (s2, n2);
	normalized_s2 = g_utf8_normalize (casefold, -1, G_NORMALIZE_ALL);
	g_free (casefold);
//	len_s2 = strlen (normalized_s2);

//	ret = (len_s1 == len_s2) ?
//		(strcmp (normalized_s1, normalized_s2) == 0) : FALSE;
	ret = (strcmp (normalized_s1, normalized_s2) == 0);

	g_free (normalized_s1);
	g_free (normalized_s2);

	return ret;
}
/**
@brief duplicate UTF-8 string, truncated after @a num characters if the string is longer than that
TODO error message assumes BGL closed
@param str the string to be duplicated
@param num maximum no. of characters in @a str to be processed
@return the duplicated string, or NULL upon memory error
*/
gchar *e2_utf8_ndup (const gchar *str, glong num)
{
	glong size = g_utf8_strlen (str, -1);
	if (num > size)
		num = size;
	gchar *end = g_utf8_offset_to_pointer (str, num);
	glong byte_size = end - str + 1;
	gchar *utf8 = g_try_malloc (byte_size);
	CHECKALLOCATEDWARN (utf8, return NULL;); //TODO depends on closed BGL
	if (utf8 != NULL)
		return g_utf8_strncpy (utf8, str, num);
	return NULL;
}
/**
@brief escape all instances of character @a c in utf-8 string @a str
TODO memory-allocation error message assumes BGL closed
@param str the string to be processed
@param c the character to be escaped if found in @a str
@return newly-allocated string with any occurrence of @a c escaped with a '\', or NULL upon memory error
*/
gchar *e2_utf8_escape (const gchar *str, gunichar c)
{
	glong clen = g_utf8_strlen (str, -1);
	glong i;
	gint count = 0;
	const gchar *tmp = str;

	for (i = 0; i < clen; i++)
	{
		if (g_utf8_get_char (tmp) == c)
			count++;
		tmp = g_utf8_next_char (tmp);
	}

	if (count > 0)
	{
		gint len = strlen (str);
		gint size = (len + count + 1) * sizeof (gchar);
		gchar *ret = g_try_malloc (size);
		CHECKALLOCATEDWARN (ret, return NULL;); //TODO assumes BGL closed
		if (ret != NULL)
		{
			count = 0;
			for (i = 0; i < clen; i++)
			{
				gchar *cur = g_utf8_offset_to_pointer (str, i);
				if (g_utf8_get_char (cur) == c)
					g_utf8_strncpy (g_utf8_offset_to_pointer (ret, count++), "\\", 1);
				g_utf8_strncpy (g_utf8_offset_to_pointer (ret, count++), cur, 1);
			}
			ret[size - 1] = '\0';
		}
		return ret;
	}
	else
		return g_strdup (str);
}
/**
@brief unescape all instances of character @a c in utf-8 string @a str
TODO allocation-error message assumes BGL closed
@param str the string to be processed
@param c the character to be unescaped if found in @a str
@return newly-allocated string without any occurrence of '\'in front of @a c, or NULL after error
*/
gchar *e2_utf8_unescape (const gchar *str, gunichar c)
{
	glong count = 0;
	gint size = (strlen (str) + 1) * sizeof (gchar);
	gchar *ret = g_try_malloc (size);
	CHECKALLOCATEDWARN (ret, return NULL;); //TODO assumes BGL closed
	*ret = '\0';	//in case str is empty
	const gchar *tmp = str;
	while (*tmp != '\0')
	{
		if ((g_utf8_get_char (tmp)) == '\\')
		{
			gchar *next = g_utf8_next_char (tmp);
			if ((next != NULL) && g_utf8_get_char (next) == c)
				tmp = next;
		}
		g_utf8_strncpy (g_utf8_offset_to_pointer (ret, count++), tmp, 1);
		tmp = g_utf8_next_char (tmp);
	}
/* not needed - each g_utf8_strncpy() appends a '\0'
	if (count < g_utf8_strlen (str, -1))
		g_utf8_strncpy (g_utf8_offset_to_pointer (ret, count), "\0" , 1);
	else
		ret[size - 1] = '\0';
*/
	printd (DEBUG, "ret: %s", ret);
	return ret;
}
/**
@brief if a request has been logged, adjust func pointers to enable or disable file path/name encoding conversion
*/
void e2_utf8_set_name_conversion_if_requested (void)
{
	if (app.reconvert_requested)
	{
		printd (DEBUG, "repoint encoding conversion funcs if necessary, before action starts");
		e2_utf8_set_name_conversion (app.pane1.view.convert || app.pane2.view.convert);
		app.reconvert_requested = FALSE;
	}
}
/**
@brief set func pointers to en/disable file path/name encoding conversion
@param convert TRUE if conversion to/from UTF-8 is to be done henceforth

@return
*/
void e2_utf8_set_name_conversion (gboolean convert)
{
	if (convert)
	{
		e2_display_from_locale = g_filename_display_name;
		e2_fname_to_locale = e2_fname_dupto_locale = e2_utf8_filename_to_locale;
		e2_fname_from_locale = e2_fname_dupfrom_locale = e2_utf8_filename_from_locale;
//		e2_fname_free = g_free;
	}
	else
	{
		e2_display_from_locale = e2_fname_to_locale = e2_fname_from_locale
			= e2_utf8_not_converted;
 		e2_fname_dupto_locale = e2_fname_dupfrom_locale = g_strdup;
//		e2_fname_free = e2_utf8_not_freed;
	}
}

//inline
gchar *e2_utf8_not_converted (const gchar *d)
{
	return (gchar *) d;
}

//void e2_utf8_not_freed (gpointer dummy)
//{}

/**
@brief cleanup @a freeme if it's not the same as @a original
This is robust to a change of encoding-functions (hence string allocation)
between creation and cleanup of @a freeme.
@param freeme the string to be free'd, can be NULL
@param original the string from which @a freeme was derived

@return
*/
void e2_utf8_fname_free (gchar *freeme, const gchar *original)
{
	if (freeme != (gchar *)original)
		g_free (freeme);
}

//#ifndef E2_FILES_UTF8ONLY
//with run-time checking, a pointer may be set to this func, so no inline
//inline
gchar *e2_utf8_filename_from_locale (const gchar *d)
{
	GError *error = NULL;
	gchar *ret = g_filename_to_utf8 (d, -1, NULL, NULL, &error);
	if (error == NULL)
		return ret;
	printd (WARN, "locale filename to UTF8 conversion failed: %s",
		error->message);
	g_error_free (error);
//	return e2_utf8_filename_from_locale_backup (d);
//}

//gchar *e2_utf8_filename_from_locale_backup (const gchar *d)
//{
	printd (WARN, "falling back to '%s'", e2_cl_options.fallback_encoding);
//	GError *
	error = NULL;
//	gchar *
	ret = g_convert_with_fallback (d, -1, "UTF-8", e2_cl_options.fallback_encoding,
			"?", NULL, NULL, &error);
	if (error != NULL)
	{
		printd (WARN, "fallback locale filename to UTF8 conversion failed: %s",
			error->message);
		g_error_free (error);
		ret = g_strdup(_("Unknown"));
	}
	return ret;
}

//with run-time checking, a pointer may be set to this func, so no inline
//inline
gchar *e2_utf8_filename_to_locale (const gchar *d)
{
	GError *error = NULL;
	gchar *ret = g_filename_from_utf8 (d, -1, NULL, NULL, &error);
	if (error == NULL)
		return ret;
	printd (WARN, "UTF8 filename to locale conversion failed: %s", error->message);
	g_error_free (error);
//	return e2_utf8_filename_to_locale_backup (d);
//}

//gchar *e2_utf8_filename_to_locale_backup (const gchar *d)
//{
	printd (WARN, "falling back to '%s'", e2_cl_options.fallback_encoding);
//	GError *
	error = NULL;
//	gchar *
	ret = g_convert_with_fallback (d, -1, e2_cl_options.fallback_encoding,
		"UTF-8", "?", NULL, NULL, &error);
	if (error != NULL)
	{
		printd (WARN, "fallback UTF8 filename to locale conversion failed: %s", error->message);
		g_error_free (error);
		ret = g_strdup(_("Unknown"));
	}
	return ret;
}
//#endif //ndef E2_FILES_UTF8ONLY

gchar *e2_utf8_from_locale (const gchar *d)
{
	if (g_utf8_validate (d, -1, NULL))
		return g_strdup (d);
	gchar *ret = g_locale_to_utf8 (d, -1, NULL, NULL, NULL);
	if (ret != NULL)
		return ret;
	printd (WARN, "initial conversion of localised string %s to UTF8 failed", d);
	return e2_utf8_from_locale_fallback (d);
}

gchar *e2_utf8_from_locale_fallback (const gchar *d)
{
	printd (WARN, "falling back to '%s'", e2_cl_options.fallback_encoding);
	GError *error = NULL;
	gchar *ret = g_convert_with_fallback (d, -1, "UTF-8",
		e2_cl_options.fallback_encoding, "?", NULL, NULL, &error);
	if (error != NULL)
	{
		printd (WARN, "fallback locale string to UTF8 conversion failed: %s",
			error->message);
		g_error_free (error);
		ret = g_strconcat("\357\277\275 ",	//U+FFFD inverted '?' char
			_("unknown encoding"), NULL);
	}
	return ret;
}

//#if 0
//UNUSED
//inline
gchar *e2_utf8_to_locale (const gchar *d)
{
	GError *error = NULL;
	gchar *ret = g_locale_from_utf8 (d, -1, NULL, NULL, &error);
	if (error == NULL)
		return ret;
	printd (WARN, "UTF8 string to locale conversion failed: %s", error->message);
	g_error_free (error);
	return e2_utf8_to_locale_fallback (d);
}

gchar *e2_utf8_to_locale_fallback (const gchar *d)
{
	printd (WARN, "falling back to '%s'", e2_cl_options.fallback_encoding);
	GError *error = NULL;
	gchar *ret = g_convert_with_fallback (d, -1, e2_cl_options.fallback_encoding,
		"UTF-8",	"?", NULL, NULL, &error);
	if (error != NULL)
	{
		printd (WARN, "fallback UTF8 string to locale conversion failed: %s", error->message);
		g_error_free (error);
		error = NULL;
		ret = g_strdup(_("Unknown"));
	}
	return ret;
}
//#endif //0

/*************************************
encoding-detection code adapted from leafpad 0.8.7 by Tarot Osuji
*/

/* A complete list of charsets officially supported in glibc 2.2.3 is:
ISO-8859-{1,2,3,5,6,7,8,9,13,15}, CP1251, UTF-8, EUC-{KR,JP,TW}, KOI8-{R,U},
GB2312, GB18030, GBK, BIG5, BIG5-HKSCS and TIS-620
(Romanian may be switching to ISO-8859-16)
*/

//up to this many locales per encoding
#define MAX_LOCALES 10

const gchar *locale_table[ENCODE_COUNT][MAX_LOCALES] =
{
/* LATIN2 */	{"cs", "de", "hr", "hu", "ro", "pl", "sk", "sq", "sr", "uz"},	//this effectively sets MAX_LOCALES
/* LATIN3 */	{"eo", "gl", "mt", NULL},
// / * LATIN4 * /	{"et", "mi", NULL}, Estonian, Latvian, and Lithuanian essentially obsolete;
/* LATIN5 */	{"az", "tr", NULL},
/* LATIN6 */	{"et", "ga", "is", "kl", "mi", NULL},
/* LATIN7 */	{"da", "fi", "lt", "lv", "no", "pl", "se", "sl", "sv", NULL},
/* LATIN9 */	{NULL},
/* ARABIC */	{"ar", "fa", "ur", NULL},
/* CYRILIC */	{"be", "bg", "ky", "mk", "mn", "ru", "tt", NULL},
/* CYRILIC_TJ */{"tg", NULL},
/* CYRILIC_UA */{"uk", NULL},
/* GREEK */		{"el", NULL},	//modern
/* HEBREW */	{"he", "iw", "yi", NULL}, //modern
/* CHINESE_CN */{"zh_CN", NULL},
/* CHINESE_HK */{"zh_HK", NULL},
/* CHINESE_TW */{"zh_TW", NULL},
/* JAPANESE */	{"ja", NULL},
/* KOREAN */	{"ko", NULL},
/* VIETNAMESE */{"vi", NULL},
/* THAI */		{"th", NULL},
/* GEORGIAN */	{"ka", NULL}
};

/*******************************************
/ * LATIN8 * /	{ "ISO-8859-14",  , },	//a.k.a celtic adds missing gaelic and welsh (cy) to latin1 mainly Old Gaelic, also ok for Albanian, Breton, Catalan, Danish, Dutch, English, Finnish, French, Gaelic, German, Irish, Italian, Norwegian, Portuguese, Swedish and Welsh.
/ * LATIN9 * /	{ "ISO-8859-15",  , },	//supersedes latin1 with euro and some missing things
/ * LATIN10 * /	{ "ISO-8859-16",  , },	//for Albanian, Croatian, Hungarian, Polish, Romanian and Slovenian, but also French, German, Italian and Irish Gaelic (new orthography). It differs from the other
/ * ARABIC * /	{ "ISO-8859-6-E",  , },	//explicit bi-directional vartiant
/ * ARABIC * /	{ "ISO-8859-6-I",  , }, //implicit bi-directional variant
/ * HEBREW * /	{ "ISO-8859-8-E",  , }, //explicit bi-directional variant
/ * HEBREW * /	{ "ISO-8859-8-I",  , }, //implicit bi-directional variant
******************************************/

const gchar *encoding_table [ENCODE_COUNT][ENCODING_TYPECOUNT] =
{
				/*  IANA          OpenI18N       Codepage */
// / * LATIN1 * /	{ "ISO-8859-1",  "ISO-8859-15", "CP1252" }, //superseded by latin9 (with euro symbol etc)
/* LATIN2 */	{ "ISO-8859-2",  "ISO-8859-16", "CP1250" },
/* LATIN3 */	{ "ISO-8859-3",  NULL,          NULL },
// / * LATIN4 * /	{ "ISO-8859-4",  "ISO-8859-13", "CP1257" },
/* LATIN5 */	{ "ISO-8859-9",  NULL,          "CP1254" },
/* LATIN6 */	{ "ISO-8859-10", NULL,          "CP1257" },
/* LATIN7 */	{ "ISO-8859-13", NULL,          "CP1257" },
/* LATIN9 */	{ "ISO-8859-1",  "ISO-8859-15", "CP1252" }, //CHECKME ISO-8859-1 is superseded
/* ARABIC */	{ "ISO-8859-6",  NULL,          "CP1256" },
//ISO-8859-5 for e.g. bulgarian, belarusian, russian and macedonian, not ukrainian, "never really caught on"
/* CYRILIC */	{ "ISO-8859-5",  "KOI8-R",      "CP1251" },	//russian, macedonian
/* CYRILIC_TJ */{ "ISO-8859-5",  "KOI8-T",      "CP1251" }, //tajic
/* CYRILIC_UA */{ "ISO-8859-5",  "KOI8-U",      "CP1251" },	//ukrainian
/* GREEK */		{ "ISO-8859-7",  NULL,          "CP1253" },	//modern
/* HEBREW */	{ "ISO-8859-8",  NULL,          "CP1255" }, //modern
/* CHINESE_CN */{ "GB2312",      "GB18030",     "CP936" },
/* CHINESE_HK */{ "BIG5",        "BIG5-HKSCS",  "CP950" },
/* CHINESE_TW */{ "BIG5",        "EUC-TW",      "CP950" },
/* JAPANESE */	{ "ISO-2022-JP", "EUC-JP",      "CP932" },
/* KOREAN */	{ "ISO-2022-KR", "EUC-KR",      "CP949" },
/* VIETNAMESE */{ NULL,          "VISCII",      "CP1258" },
/* THAI */		{ "ISO-8859-11", "TIS-620",     "CP874" }, //ISO-8859-11 is unofficial
/* GEORGIAN */	{ NULL,          "GEORGIAN-PS", NULL }
};


static guint get_locale_index (void)
{
	static guint code = ENCODE_COUNT;

	if (code == ENCODE_COUNT)	//we only look this up once
	{
		const gchar *env = g_getenv ("LC_ALL");
		if (env == NULL)
			env = g_getenv ("LANG");
		if (env != NULL && strlen (env) >= 2)
		{
			guint i, j = 1;
			while (code == ENCODE_COUNT && j < ENCODE_COUNT)
			{
				for (i = 0; i < MAX_LOCALES; i++)
				{
					if (locale_table[j][i] == NULL)
						break;
					if (strncmp (env, locale_table[j][i], strlen(locale_table[j][i])) == 0)
					{
						code = j;
						break;
					}
				}
				j++;
			}
		}
		if (code == ENCODE_COUNT)
			code = LATIN9;	//default if we can't find anything supported
	}

	return code;
}

static const gchar *detect_charset_cyrillic (const guchar *text)
{
	guchar c;
	gboolean noniso = FALSE;
	guint32 xc = 0, xd = 0, xef = 0;
	const gchar *charset;

	while ((c = *text++) != '\0')
	{
		if (c >= 0x80 && c <= 0x9F)
			noniso = TRUE;
		else if (c >= 0xC0 && c <= 0xCF)
			xc++;
		else if (c >= 0xD0 && c <= 0xDF)
			xd++;
		else if (c >= 0xE0)
			xef++;
	}

	if (!noniso && ((xc + xef) < xd))
		charset = "ISO-8859-5";
	else if ((xc + xd) < xef)
		charset = "CP1251";
	else
		charset = encoding_table [get_locale_index()][OPENI18N];

	return charset;
}

static const gchar *detect_charset_chinese (const guchar *text)
{
	guchar c;

	const gchar *charset = NULL;

	while ((c = *text++) != '\0')
	{
		if (c >= 0x81 && c <= 0x87)
		{
			charset = "GB18030";
			break;
		}
		else if (c >= 0x88 && c <= 0xA0)
		{
			c = *text++;
			if ((c >= 0x30 && c <= 0x39) || (c >= 0x80 && c <= 0xA0))
			{
				charset = "GB18030";
				break;
			} //else GBK/Big5-HKSCS cannot determine
		}
		else if ((c >= 0xA1 && c <= 0xC6) || (c >= 0xC9 && c <= 0xF9))
		{
			c = *text++;
			if (c >= 0x40 && c <= 0x7E)
			{
				charset = "BIG5";
				break;
			}
			else if ((c >= 0x30 && c <= 0x39) || (c >= 0x80 && c <= 0xA0))
			{
				charset = "GB18030";
				break;
			}
		}
		else if (c >= 0xC7)
		{
			c = *text++;
			if ((c >= 0x30 && c <= 0x39) || (c >= 0x80 && c <= 0xA0))
			{
				charset = "GB18030";
				break;
			}
		}
	}

	if (charset == NULL)
		charset = encoding_table [get_locale_index()][IANA];

	return charset;
}

static const gchar *detect_charset_japanese (const guchar *text)
{
	guchar c;
	gchar *charset = NULL;

	while ((c = *text++) != '\0')
	{
		if (c >= 0x81 && c <= 0x9F)
		{
			if (c == 0x8E) /* SS2 */
			{
				c = *text++;
				if ((c >= 0x40 && c <= 0xA0) || (c >= 0xE0 && c <= 0xFC))
				{
					charset = "CP932";
					break;
				}
			}
			else if (c == 0x8F) /* SS3 */
			{
				c = *text++;
				if (c >= 0x40 && c <= 0xA0)
				{
					charset = "CP932";
					break;
				}
				else if (c >= 0xFD)
					break;
			}
			else
			{
				charset = "CP932";
				break;
			}
		}
		else if (c >= 0xA1 && c <= 0xDF)
		{
			c = *text++;
			if (c <= 0x9F)
			{
				charset = "CP932";
				break;
			}
			else if (c >= 0xFD)
				break;
		}
		else if (c >= 0xE0 && c <= 0xEF)
		{
			c = *text++;
			if (c >= 0x40 && c <= 0xA0)
			{
				charset = "CP932";
				break;
			}
			else if (c >= 0xFD)
				break;
		}
		else if (c >= 0xF0)
			break;
	}

	if (charset == NULL)
		charset = "EUC-JP";

	return charset;
}

static const gchar *detect_charset_korean (const guchar *text)
{
	guchar c;
	gboolean noneuc = FALSE;
	gboolean nonjohab = FALSE;
	gchar *charset = NULL;

	while ((c = *text++) != '\0')
	{
		if (c >= 0x81 && c < 0x84)
		{
			charset = "CP949";
			break;
		}
		else if (c >= 0x84 && c < 0xA1)
		{
			noneuc = TRUE;
			c = *text++;
			if ((c > 0x5A && c < 0x61) || (c > 0x7A && c < 0x81))
			{
				charset = "CP1361";
				break;
			}
			else if (c == 0x52 || c == 0x72 || c == 0x92 || (c > 0x9D && c < 0xA1)
				|| c == 0xB2 || (c > 0xBD && c < 0xC1) || c == 0xD2
				|| (c > 0xDD && c < 0xE1) || c == 0xF2 || c == 0xFE)
			{
				charset = "CP949";
				break;
			}
		}
		else if (c >= 0xA1 && c <= 0xC6)
		{
			c = *text++;
			if (c < 0xA1)
			{
				noneuc = TRUE;
				if ((c > 0x5A && c < 0x61) || (c > 0x7A && c < 0x81))
				{
					charset = "CP1361";
					break;
				}
				else if (c == 0x52 || c == 0x72 || c == 0x92 || (c > 0x9D && c < 0xA1))
				{
					charset = "CP949";
					break;
				}
				else if (c == 0xB2 || (c > 0xBD && c < 0xC1) || c == 0xD2
					|| (c > 0xDD && c < 0xE1) || c == 0xF2 || c == 0xFE)
					nonjohab = TRUE;
			}
		}
		else if (c > 0xC6 && c <= 0xD3)
		{
			c = *text++;
			if (c < 0xA1)
			{
				charset = "CP1361";
				break;
			}
		}
		else if (c > 0xD3 && c < 0xD8)
		{
			nonjohab = TRUE;
			c = *text++;
		}
		else if (c >= 0xD8)
		{
			c = *text++;
			if (c < 0xA1)
			{
				charset = "CP1361";
				break;
			}
		}
		if (noneuc && nonjohab)
		{
			charset = "CP949";
			break;
		}
	}

	if (charset == NULL)
	{
		if (noneuc)
			charset = "CP949";
		else
			charset = "EUC-KR";
	}

	return charset;
}

static gboolean detect_noniso (const guchar *text)
{
	guchar c;

	while ((c = *text++) != '\0')
	{
		if (c >= 0x80 && c <= 0x9F)
			return TRUE;
	}
	return FALSE;
}
/**
@brief detect any recognised byte-order-mark at start of @a text
@param text the string to be processed
Note
There's no distinction between flavours of UTF-16 and the corresponding
UCS-2's, or between UTF-32's and corresponding UCS-4's
No checking for file length, particularly in the case of UTF-32
@return encoding name if any is recognised, or NULL
*/
static const gchar *detect_charset_BOM (const guchar *text)
{
	guchar c;
	gint count;
	guchar array[3];

	count = 0;
	switch (*text++)
	{
		case '+':
			//2B 2F 76 and one of the following bytes: [ 3C | 3D | 3E | 3F ]
			array[0] = '/';
			array[1] = 'v';
			while (count < 2 && (c = *text++) == array[count]) {count++;}
			if (count == 2 && ((c = *text) >= '\\' && c <= '_'))
				return "UTF-7";
			break;
		case 0xEF:
			//EF BB BF
			array[0] = 0xBB;
			array[1] = 0xBF;
			while (count < 2 && (c = *text++) == array[count]) {count++;}
			if (count == 2)
				return "UTF-8";
			break;
		/*UCS-2, UCS-2, UCS-2BE, UCS-2LE (often conflated with UTF-16 but they're not the same)
		  have same BOM as corresponding UTF-16. Ditto for UCS-4's and UTF-32's	*/
		case 0xFE:
			//Big Endian FE FF
			if (*text == 0xFF)
				return "UTF-16BE"; //could actually be UCS-2BE, not quite the same
			break;
		case 0xFF:
			//Little Endian	FF FE 00 00
			//Little Endian FF FE
			array[0] = 0xFE;
			array[1] = 0;
			array[2] = 0;
			while (count < 3 && (c = *text++) == array[count]) {count++;}
			if (count == 3)
				return "UTF-32LE";
			if (count == 1)
				return "UTF-16LE"; //could actually be UCS-2LE, not quite the same
			break;
		case 0: //CHECKME if file is empty, next checks may be a buffer overflow ?
			//Big Endian 00 00 FE FF
			array[0] = 0;
			array[1] = 0xFE;
			array[2] = 0xFF;
			while (count < 3 && (c = *text++) == array[count]) {count++;}
			if (count == 3)
				return "UTF-32BE";
			break;
		case 0x0E:
			//0E FE FF
			array[0] = 0xFE;
			array[1] = 0xFF;
			while (count < 2 && (c = *text++) == array[count]) {count++;}
			if (count == 2)
				return "SCSU";
			break;
		case 0xDD:
			//DD 73 66 73
			array[0] = 's';
			array[1] = 'f';
			array[2] = 's';
			while (count < 3 && (c = *text++) == array[count]) {count++;}
			if (count == 3)
				return "UTF-EBCDIC";
			break;
		case 0xFB:
			//FB EE 28
			array[0] = 0xEE;
			array[1] = 0x28;
			while (count < 2 && (c = *text++) == array[count]) {count++;}
			if (count == 2)
				return "BOCU-1";
		default:
			break;
	}
	return NULL;
}
/**
@brief detect (not guess) whether @a text has one of several recognized character-encodings
@a text may be validated as UTF-8 in spite of a nominally different coding
@param text 0-terminated string to be processed
@param isutf8 store for T/F, whether @a text is found to be valid UTF-8 (which includes plain ascii)
@return name of detected charset, maybe NULL
*/
const gchar *e2_utf8_detect_charset (const guchar *text, gboolean *isutf8)
{
	const gchar *charset;
	guchar c;

	charset = detect_charset_BOM (text);
	if (charset != NULL)	//FIXME test for short file length if UTF-32*
	{
		//CHECKME aliases for this encoding ?
		*isutf8 = !strcmp (charset, "UTF-8");
		return charset;
	}

	*isutf8 = FALSE;
	while ((c = *text++) != '\0')
	{
		if (c > 0x7F)
		{
			if ((c & 0xC0) == 0xC0)
				*isutf8 = g_utf8_validate ((gchar *)--text, -1, NULL);
			break;
		}
		else if (c == '~')
		{
			c = *text++;
			if (c == '{')
			{
				c = *text++;	//maybe 1st GB2312 (chinese) char
				if (c != '\0')
				{
					c = *text++;	//maybe } or 2nd GB2312 (chinese) char
					if (c == '}' && *text == '~')
					{
						charset = "HZ";
						break;
					}
					else
					{
						c = *text++;	//maybe } after 2nd GB2312 (chinese) char
						if (c == '}' && *text == '~' && *(text-2) > 0x7F)
						{
							charset = "HZ";
							break;
						}
					}
				}
			}
		}
		else if (c == 0x1B) /* ESC */
		{
			c = *text++;
			if (c == '$')
			{
				c = *text++;
				switch (c)
				{
					case 'B': // JIS X 0208-1983
					case '@': // JIS X 0208-1978
						charset = "ISO-2022-JP";
						break;
					case 'A': // GB2312-1980
						charset = "ISO-2022-JP-2";
						break;
					case '(':
						c = *text++;
						switch (c)
						{
							case 'C': // KSC5601-1987
							case 'D': // JIS X 0212-1990
								charset = "ISO-2022-JP-2";
						}
						break;
					case ')':
						c = *text++;
						if (c == 'C')
							charset = "ISO-2022-KR"; // KSC5601-1987
					default:
						break;
				}
				if (charset != NULL)
					break;
			}
		}
	}

	if (*isutf8)
	{	//validation succeeded
		if (!g_get_charset (&charset))	//use default encoding if it's utf8-compatible
			charset = "UTF-8";
	}
	else	//no validation requested, or validation failed
		if (c == '\0')	//reached end of text without detection
	{	//it's almost certainly ASCII, assume the default encoding applies
		//FIXME except if that's incompatible with ascii
//		printd (DEBUG, "Specific UTF8 detection failed, reverted to DEFAULT CHARSET %s", *charset);
		e2_utils_get_charset (&charset);
		*isutf8 = TRUE;	//no conversion needed
	}
	return charset;
}
/**
@brief get encoding name for a supported language
Sometimes this uses a simple lookup, sometimes there's some checking of @a text,
sometimes just the user's default encoding
@param text 0-terminated text string to evaluate
@return string representing the encoding, or NULL
*/
const gchar *e2_utf8_guess_charset (const guchar *text)
{
	guint langcode = get_locale_index();
	switch (langcode)
	{
		case CYRILIC:
		case CYRILIC_UA:
		case CYRILIC_TJ:
			return (detect_charset_cyrillic(text)); // fuzzy...
		case CHINESE_CN:
		case CHINESE_TW:
		case CHINESE_HK:
			return (detect_charset_chinese(text));
		case JAPANESE:
			return (detect_charset_japanese(text));
		case KOREAN:
			return (detect_charset_korean(text));
		case VIETNAMESE:
		case THAI:
		case GEORGIAN:
			return (encoding_table [langcode][OPENI18N]);
		default:
		{
			const gchar *charset;
			e2_utils_get_charset (&charset);
			if (charset != NULL)
			{
				//CHECKME c.f. _e2_fs_charset_is_utf()
				if (strcmp (charset, "UTF-8") == 0)
				{
					//CHECKME relevance of this when the text isn't compliant
					if (detect_noniso (text))
						charset = encoding_table [langcode][CODEPAGE];
					else
						charset = encoding_table [langcode][OPENI18N];
				}
				if (charset == NULL)
					return (encoding_table [langcode][IANA]);
			}
			return charset;
		}
	}
}
