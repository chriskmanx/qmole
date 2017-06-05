/* $Id: e2_utf8.h 2743 2013-09-19 22:29:00Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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
@file src/utils/e2_utf8.h
@brief utf8 string utilities header

This is the header file for the utf8 string utility functions.
*/

#ifndef __E2_UTF8_H__
#define __E2_UTF8_H__

enum
{
	IANA = 0,
	OPENI18N,
	CODEPAGE,
	ENCODING_TYPECOUNT
};

enum
{
	LATIN2 = 0,
	LATIN3,
//	LATIN4,
	LATIN5,
	LATIN6,
	LATIN7,
	LATIN9, //supersedes LATIN1
	ARABIC,
	CYRILIC,
	CYRILIC_TJ,
	CYRILIC_UA,
	GREEK,
	HEBREW,
	CHINESE_CN,
	CHINESE_HK,
	CHINESE_TW,
	JAPANESE,
	KOREAN,
	VIETNAMESE,
	THAI,
	GEORGIAN,
	ENCODE_COUNT,
};
#define	LATIN8 LATIN9	//un-supported
#define LATIN10 LATIN9

//support for file paths/names ONLY in utf-8
//GLib and GTK+ by default assume that filenames are
//encoded in UTF-8 rather than some other encoding that
//is specific to a locale. ASCII is compatible with UTF-8
// the ..FILENAME_FROM.. macros make a utf8 name from a locale-encoded one
// the ..FILENAME_TO.. macros make a locale-encoded name from a utf8 one
//#define E2_FILES_UTF8ONLY - see Makefile

/* #ifdef E2_FILES_UTF8ONLY
#define F_DISPLAYNAME_FROM_LOCALE(d) (gchar*)d
//the D_ versions are for when a duplicate is needed
//regardless of whether E2_FILES_UTF8ONLY is defined or not
#define D_FILENAME_FROM_LOCALE(d) g_strdup(d)
#define D_FILENAME_TO_LOCALE(d) g_strdup(d)
//the F_ versions are for when there is an associated free of
//the created duplicate, when E2_FILES_UTF8ONLY is not defined
#define F_FILENAME_FROM_LOCALE(d) (gchar*)d
#define F_FILENAME_TO_LOCALE(d) (gchar*)d
#define F_FREE(d)
#else */

/* this version of defines applies when build-time conversion code is used
#define D_FILENAME_FROM_LOCALE(d) e2_utf8_filename_from_locale(d)
#define D_FILENAME_TO_LOCALE(d) e2_utf8_filename_to_locale(d)
//#define F_FILENAME_FROM_LOCALE(d) _free_me_=e2_utf8_filename_from_locale(d)
//#define F_FILENAME_TO_LOCALE(d) _free_me_=e2_utf8_filename_to_locale(d)
//#define F_FREE g_free(_free_me_)
#define F_FILENAME_FROM_LOCALE(d) e2_utf8_filename_from_locale(d)
#define F_FILENAME_TO_LOCALE(d) e2_utf8_filename_to_locale(d)
#define F_FREE(d) g_free(d)
gchar *e2_utf8_filename_from_locale_backup (const gchar *d);
gchar *e2_utf8_filename_to_locale_backup (const gchar *d);
*/
// this version of defines applies when run-time conversion code is used
#define F_DISPLAYNAME_FROM_LOCALE(d) (*e2_display_from_locale)(d)
#define D_FILENAME_FROM_LOCALE(d) (*e2_fname_dupfrom_locale)(d)
#define D_FILENAME_TO_LOCALE(d) (*e2_fname_dupto_locale)(d)
#define F_FILENAME_FROM_LOCALE(d) (*e2_fname_from_locale)(d)
#define F_FILENAME_TO_LOCALE(d) (*e2_fname_to_locale)(d)
//#define F_FREE(d) (*e2_fname_free)(d)
#define F_FREE e2_utf8_fname_free
//pointers to functions used to convert (or not) coding of file path/name strings
gchar *(*e2_display_from_locale) (const gchar *);
gchar *(*e2_fname_to_locale) (const gchar *);
gchar *(*e2_fname_from_locale) (const gchar *);
gchar *(*e2_fname_dupto_locale) (const gchar *);
gchar *(*e2_fname_dupfrom_locale) (const gchar *);
//void (*e2_fname_free) (gpointer);
//pointers set to these functions when coding conversion is not needed
gchar *e2_utf8_not_converted (const gchar *);
//void e2_utf8_not_freed (gpointer);
void e2_utf8_fname_free (gchar *freeme, const gchar *original);
//#endif //def E2_FILES_UTF8ONLY

void e2_utf8_set_name_conversion_if_requested (void);
void e2_utf8_set_name_conversion (gboolean convert);

//#define NCHR(p) p=g_utf8_next_char(p)
//#define PCHR(p) p=g_utf8_prev_char(p)

//inline
gchar *e2_utf8_filename_from_locale (const gchar *d) G_GNUC_MALLOC;
//inline
gchar *e2_utf8_filename_to_locale (const gchar *d) G_GNUC_MALLOC;
//inline
gchar *e2_utf8_to_locale (const gchar *d) G_GNUC_MALLOC;
gchar *e2_utf8_to_locale_fallback (const gchar *d) G_GNUC_MALLOC;
gchar *e2_utf8_from_locale (const gchar *d) G_GNUC_MALLOC;
gchar *e2_utf8_from_locale_fallback (const gchar *d) G_GNUC_MALLOC;
//macro for inlining speedier conversions
#define e2_utf8_from_locale_fast(d) \
 (g_utf8_validate (d, -1, NULL) ) ? g_strdup (d) : \
 ((__utf__ = g_locale_to_utf8 (d, -1, NULL, NULL, NULL)) != NULL) ? __utf__ : \
 e2_utf8_from_locale_fallback (d);

const gchar *e2_utf8_strcasestr (const gchar *haystack, const gchar *needle);
const gchar *e2_utf8_strrcasestr (const gchar *haystack, const gchar *needle);
gboolean e2_utf8_caseless_match (const gchar *s1, const gchar *s2, gssize n1,
	gssize n2);
gchar *e2_utf8_ndup (const gchar *str, glong num) G_GNUC_MALLOC;
gchar *e2_utf8_escape (const gchar *str, gunichar c) G_GNUC_MALLOC;
gchar *e2_utf8_unescape (const gchar *str, gunichar c) G_GNUC_MALLOC;

const gchar *e2_utf8_detect_charset (const guchar *text, gboolean *isutf8);
const gchar *e2_utf8_guess_charset (const guchar *text);

#endif // ndef __E2_UTF8_H__
