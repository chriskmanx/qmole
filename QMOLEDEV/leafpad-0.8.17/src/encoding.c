/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <glib.h>
#include <string.h>
#include "encoding.h"

#define MAX_COUNTRY_NUM 10

enum {
	LATIN1 = 0,
	LATIN2,
	LATIN3,
	LATIN4,
	LATINC,
	LATINC_UA,
	LATINC_TJ,
	LATINA,
	LATING,
	LATINH,
	LATIN5,
	CHINESE_CN,
	CHINESE_TW,
	CHINESE_HK,
	JAPANESE,
	KOREAN,
	VIETNAMESE,
	THAI,
	GEORGIAN,
	END_CODE
};

static const gchar *country_table[][MAX_COUNTRY_NUM] =
{
	/* LATIN1 */        {NULL},
	/* LATIN2 */        {"cs", "hr", "hu", "pl", "ro", "sk", "sl", "sq", "sr", "uz"},
	/* LATIN3 */        {"eo", "mt", NULL},
	/* LATIN4 */        {"et", "lt", "lv", "mi", NULL},
	/* LATINC */        {"be", "bg", "ky", "mk", "mn", "ru", "tt", NULL},
	/* LATINC_UA */     {"uk", NULL},
	/* LATINC_TJ */     {"tg", NULL},
	/* LATINA */        {"ar", "fa", "ur", NULL},
	/* LATING */        {"el", NULL},
	/* LATINH */        {"he", "yi", NULL},
	/* LATIN5 */        {"az", "tr", NULL},
	/* CHINESE_CN */    {"zh_CN", NULL},
	/* CHINESE_TW */    {"zh_TW", NULL},
	/* CHINESE_HK */    {"zh_HK", NULL},
	/* JAPANESE */      {"ja", NULL},
	/* KOREAN */        {"ko", NULL},
	/* VIETNAMESE */    {"vi", NULL},
	/* THAI */          {"th", NULL},
	/* GEORGIAN */      {"ka", NULL},
};

static const gchar *encoding_table[][ENCODING_MAX_ITEM_NUM] =
{
	                  /*  IANA           OpenI18N       Codepage */
	/* LATIN1 */        { "ISO-8859-1",  "ISO-8859-15", "CP1252" },
	/* LATIN2 */        { "ISO-8859-2",  "ISO-8859-16", "CP1250" },
	/* LATIN3 */        { "ISO-8859-3",  NULL,          NULL },
	/* LATIN4 */        { "ISO-8859-4",  "ISO-8859-13", "CP1257" },
	/* LATINC */        { "ISO-8859-5",  "KOI8-R",      "CP1251" },
	/* LATINC_UA */     { "ISO-8859-5",  "KOI8-U",      "CP1251" },
	/* LATINC_TJ */     { "ISO-8859-5",  "KOI8-T",      "CP1251" },
	/* LATINA */        { "ISO-8859-6",  NULL,          "CP1256" },
	/* LATING */        { "ISO-8859-7",  NULL,          "CP1253" },
	/* LATINH */        { "ISO-8859-8",  NULL,          "CP1255" },
	/* LATIN5 */        { "ISO-8859-9",  NULL,          "CP1254" },
	/* CHINESE_CN */    { "GB2312",      "GB18030",     "CP936" },
	/* CHINESE_TW */    { "BIG5",        "EUC-TW",      "CP950" },
	/* CHINESE_HK */    { "BIG5",        "BIG5-HKSCS",  "CP950" },
	/* JAPANESE */      { "ISO-2022-JP", "EUC-JP",      "CP932" },
	/* KOREAN */        { "ISO-2022-KR", "EUC-KR",      "CP949" },
	/* VIETNAMESE */    { NULL,          "VISCII",      "CP1258" },
	/* THAI */          { NULL,          "TIS-620",     "CP874" },
	/* GEORGIAN */      { NULL,          "GEORGIAN-PS", NULL },
};

guint get_encoding_code(void)
{
	static guint code = END_CODE;
	const gchar *env;
	guint i, j = 1;
	
	if (code == END_CODE) {
		env = g_getenv("LC_ALL");
		if (!env)
			env = g_getenv("LANG");
		if (env && strlen(env) >= 2)
			while (code == END_CODE && j < END_CODE) {
				for (i = 0; i < MAX_COUNTRY_NUM; i++) {
					if (!country_table[j][i])
						break;
					if (strncmp(env, country_table[j][i], strlen(country_table[j][i])) == 0) {
						code = j;
						break;
					}
				}
				j++;
			}
		if (code == END_CODE)
			code = 0;
	}
	
	return code;
}

EncArray *get_encoding_items(guint code)
{
	gint i;
	static EncArray *array = NULL;
	
	if (!array) {
		array = g_malloc(sizeof(EncArray));
		for (i = 0; i < ENCODING_MAX_ITEM_NUM; i++)
			array->item[i] = encoding_table[code][i] ?
				encoding_table[code][i] : NULL;
	}
	
	return array;
}

const gchar *get_default_charset(void)
{
	const gchar *charset;
	
	g_get_charset(&charset);
	
	return charset;
}

/*
Imported from KEdit (for BeOS, NOT KDE).
based on
http://examples.oreilly.com/cjkvinfo/Ch7/DetectCodeType.c
*/
void convert_line_ending_to_lf(gchar *text)
{
	gint i, j;
	
	for (i = 0, j = 0; TRUE; i++, j++) {
		if (*(text + i) == CR) {
			*(text + j) = LF;
			if (*(text + i + 1) == LF)
				i++;
		} else {
			*(text + j) = *(text + i);
			if (*(text + j) == '\0')
				break;
		}
	}
}

void convert_line_ending(gchar **text, gint retcode)
{
	gchar *buf, *str = *text;
	const gint len = strlen(str);
	gint i, j, LFNum = 0;
	
	switch (retcode) {
	case CR:
		while (*str != '\0') {
			if (*str == LF)
				*str = CR;
			str++;
		}
		break;
	case CR+LF:
		for (i = 0; *(str + i) != '\0'; i++) {
			if (*(str + i) == LF)
				LFNum++;
		}
		buf = g_new(gchar, len + LFNum + 1);
		for (i= 0, j = 0;; i++, j++) {
			if (*(str + j) == LF) {
				*(buf + i) = CR;
				*(buf + (++i)) = LF;
			} else
				*(buf + i) = *(str + j);
			if (*(str + j) == '\0')
				break;
		}
		g_free(*text);
		*text = buf;
	}
}

gint detect_line_ending(const gchar *text)
{
	while (*(text++) != '\0') {
		if (*text == LF)
			break;
		if (*text == CR) {
			if (*(++text) == LF)
				return CR+LF;
			else
				return CR;
		}
	}
	return LF;
}

static const gchar *detect_charset_cylillic(const gchar *text)
{
	guint8 c = *text;
	gboolean noniso = FALSE;
	guint32 xc = 0, xd = 0, xef = 0;
	
	const gchar *charset = get_encoding_items(get_encoding_code())->item[OPENI18N];
	
	while ((c = *text++) != '\0') {
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
	
	return charset;
}

static const gchar *detect_charset_chinese(const gchar *text)
{
	guint8 c = *text;
	
	const gchar *charset = get_encoding_items(get_encoding_code())->item[IANA];
	
	while ((c = *text++) != '\0') {
		if (c >= 0x81 && c <= 0x87) {
			charset = "GB18030";
			break;
		}
		else if (c >= 0x88 && c <= 0xA0) {
			c = *text++;
			if ((c >= 0x30 && c <= 0x39) || (c >= 0x80 && c <= 0xA0)) {
				charset = "GB18030";
				break;
			} //else GBK/Big5-HKSCS cannot determine
		}
		else if ((c >= 0xA1 && c <= 0xC6) || (c >= 0xC9 && c <= 0xF9)) {
			c = *text++;
			if (c >= 0x40 && c <= 0x7E)
				charset = "BIG5";
			else if ((c >= 0x30 && c <= 0x39) || (c >= 0x80 && c <= 0xA0)) {
				charset = "GB18030";
				break;
			}
		}
		else if (c >= 0xC7) {
			c = *text++;
			if ((c >= 0x30 && c <= 0x39) || (c >= 0x80 && c <= 0xA0)) {
				charset = "GB18030";
				break;
			}
		}
	}
	
	return charset;
}

static const gchar *detect_charset_japanese(const gchar *text)
{
	guint8 c = *text;
	gchar *charset = NULL;
	
	while (charset == NULL && (c = *text++) != '\0') {
		if (c >= 0x81 && c <= 0x9F) {
			if (c == 0x8E) /* SS2 */ {
				c = *text++;
				if ((c >= 0x40 && c <= 0xA0) || (c >= 0xE0 && c <= 0xFC))
					charset = "CP932";
			}
			else if (c == 0x8F) /* SS3 */ {
				c = *text++;
				if (c >= 0x40 && c <= 0xA0)
					charset = "CP932";
				else if (c >= 0xFD)
					break;
			}
			else
				charset = "CP932";
		}
		else if (c >= 0xA1 && c <= 0xDF) {
			c = *text++;
			if (c <= 0x9F)
				charset = "CP932";
			else if (c >= 0xFD)
				break;
		}
		else if (c >= 0xE0 && c <= 0xEF) {
			c = *text++;
			if (c >= 0x40 && c <= 0xA0)
				charset = "CP932";
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

static const gchar *detect_charset_korean(const gchar *text)
{
	guint8 c = *text;
	gboolean noneuc = FALSE;
	gboolean nonjohab = FALSE;
	gchar *charset = NULL;
	
	while (charset == NULL && (c = *text++) != '\0') {
		if (c >= 0x81 && c < 0x84) {
			charset = "CP949";
		}
		else if (c >= 0x84 && c < 0xA1) {
			noneuc = TRUE;
			c = *text++;
			if ((c > 0x5A && c < 0x61) || (c > 0x7A && c < 0x81))
				charset = "CP1361";
			else if (c == 0x52 || c == 0x72 || c == 0x92 || (c > 0x9D && c < 0xA1)
				|| c == 0xB2 || (c > 0xBD && c < 0xC1) || c == 0xD2
				|| (c > 0xDD && c < 0xE1) || c == 0xF2 || c == 0xFE)
				charset = "CP949";
		}
		else if (c >= 0xA1 && c <= 0xC6) {
			c = *text++;
			if (c < 0xA1) {
				noneuc = TRUE;
				if ((c > 0x5A && c < 0x61) || (c > 0x7A && c < 0x81))
					charset = "CP1361";
				else if (c == 0x52 || c == 0x72 || c == 0x92 || (c > 0x9D && c < 0xA1))
					charset = "CP949";
				else if (c == 0xB2 || (c > 0xBD && c < 0xC1) || c == 0xD2
					|| (c > 0xDD && c < 0xE1) || c == 0xF2 || c == 0xFE)
					nonjohab = TRUE;
			}
		}
		else if (c > 0xC6 && c <= 0xD3) {
			c = *text++;
			if (c < 0xA1)
				charset = "CP1361";
		}
		else if (c > 0xD3 && c < 0xD8) {
			nonjohab = TRUE;
			c = *text++;
		}
		else if (c >= 0xD8) {
			c = *text++;
			if (c < 0xA1)
				charset = "CP1361";
		}
		if (noneuc && nonjohab)
			charset = "CP949";
	}
	
	if (charset == NULL) {
		if (noneuc)
			charset = "CP949";
		else
			charset = "EUC-KR";
	}
	
	return charset;
}

static gboolean detect_noniso(const gchar *text)
{
	guint8 c = *text;
	
	while ((c = *text++) != '\0') {
		if (c >= 0x80 && c <= 0x9F)
			return TRUE;
	}
	return FALSE;
}

const gchar *detect_charset(const gchar *text)
{
	guint8 c = *text;
	const gchar *charset = NULL;
	
	if (g_utf8_validate(text, -1, NULL)) {
		while ((c = *text++) != '\0') {
			if (c > 0x7F) {
				charset = "UTF-8";
				break;
			}
			if (c == 0x1B) /* ESC */ {
				c = *text++;
				if (c == '$') {
					c = *text++;
					switch (c) {
					case 'B': // JIS X 0208-1983
					case '@': // JIS X 0208-1978
						charset = "ISO-2022-JP";
						continue;
					case 'A': // GB2312-1980
						charset = "ISO-2022-JP-2";
						break;
					case '(':
						c = *text++;
						switch (c) {
						case 'C': // KSC5601-1987
						case 'D': // JIS X 0212-1990
							charset = "ISO-2022-JP-2";
						}
						break;
					case ')':
						c = *text++;
						if (c == 'C')
							charset = "ISO-2022-KR"; // KSC5601-1987
					}
					break;
				}
			}
		}
		if (!charset)
			charset = get_default_charset();
	}
	
	if (!charset) {
		switch (get_encoding_code()) {
		case LATINC:
		case LATINC_UA:
		case LATINC_TJ:
			charset = detect_charset_cylillic(text); // fuzzy...
			break;
		case CHINESE_CN:
		case CHINESE_TW:
		case CHINESE_HK:
			charset = detect_charset_chinese(text);
			break;
		case JAPANESE:
			charset = detect_charset_japanese(text);
			break;
		case KOREAN:
			charset = detect_charset_korean(text);
			break;
		case VIETNAMESE:
		case THAI:
		case GEORGIAN:
			charset = get_encoding_items(get_encoding_code())->item[OPENI18N];
			break;
		default:
			if (strcmp(get_default_charset(), "UTF-8") != 0)
				charset = get_default_charset();
			else if (detect_noniso(text))
				charset = get_encoding_items(get_encoding_code())->item[CODEPAGE];
			else
				charset = get_encoding_items(get_encoding_code())->item[OPENI18N];
			if (!charset)
				charset = get_encoding_items(get_encoding_code())->item[IANA];					
		}
	}
	
	return charset;
}
