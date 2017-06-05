/*--------------------------------*-C-*--------------------------------------*
 * File:	  encoding.c
 *---------------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (C) 2001	   Tomohiro KUBOTA <kubota@debian.org>
 * Copyright (C) 2004	   Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *--------------------------------------------------------------------------*/

/*
** $Id: encoding.c,v 1.49 2005/04/20 22:56:34 cvs Exp $
*/


#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


struct KNOWN_ENCODINGS {
	char*			name;
	enum enc_label	method;
	void (*func) (unsigned char*, int);
};

#ifdef MULTICHAR_SET
static struct KNOWN_ENCODINGS known_encodings[] = {
	{"SHIFTJIS",	ENC_SJIS,		rxvt_decode_sjis2jis},
	{"SJIS",		ENC_SJIS,		rxvt_decode_sjis2jis},
	{"EUC-JISX0213",ENC_SJIS,		rxvt_decode_sjis2jis},

	{"EUCJ",		ENC_EUCJ,		rxvt_decode_euc2jis},
	{"EUCJP",		ENC_EUCJ,		rxvt_decode_euc2jis},
	{"EUC-JP",		ENC_EUCJ,		rxvt_decode_euc2jis},
	{"UJIS",		ENC_EUCJ,		rxvt_decode_euc2jis},

	{"EUCKR",		ENC_EUCKR,		rxvt_decode_euc2jis},
	{"EUC-KR",		ENC_EUCKR,		rxvt_decode_euc2jis},
	{"KR",			ENC_EUCKR,		rxvt_decode_dummy},

	{"EUCCN",		ENC_GB,			rxvt_decode_euc2jis},
	{"EUC-CN",		ENC_GB,			rxvt_decode_euc2jis},
	{"GB2312",		ENC_GB,			rxvt_decode_euc2jis},
	{"GB",			ENC_GB,			rxvt_decode_euc2jis},

	{"GB18030",		ENC_GB18030,	rxvt_decode_gb180302jis},
	{"GBK",			ENC_GBK,		rxvt_decode_dummy},

	{"BIG5",		ENC_BIG5,		rxvt_decode_dummy},
	{"BIGFIVE",		ENC_BIG5,		rxvt_decode_dummy},
	{"BIG5HKSCS",	ENC_BIG5,		rxvt_decode_dummy},

	{"KOI8R",		ENC_KOI8R,		rxvt_decode_dummy},
	{"KOI8-R",		ENC_KOI8R,		rxvt_decode_dummy},
	{"KOI8U",		ENC_KOI8U,		rxvt_decode_dummy},
	{"KOI8-U",		ENC_KOI8U,		rxvt_decode_dummy},

	{"ISO8859-1",	ENC_ISO8859_1,	rxvt_decode_dummy},
	{"ISO8859-2",	ENC_ISO8859_2,	rxvt_decode_dummy},
	{"ISO8859-3",	ENC_ISO8859_3,	rxvt_decode_dummy},
	{"ISO8859-4",	ENC_ISO8859_4,	rxvt_decode_dummy},
	{"ISO8859-5",	ENC_ISO8859_5,	rxvt_decode_dummy},
	{"ISO8859-6",	ENC_ISO8859_6,	rxvt_decode_dummy},
	{"ISO8859-7",	ENC_ISO8859_7,	rxvt_decode_dummy},
	{"ISO8859-8",	ENC_ISO8859_8,	rxvt_decode_dummy},
	{"ISO8859-9",	ENC_ISO8859_9,	rxvt_decode_dummy},
	{"ISO8859-10",	ENC_ISO8859_10,	rxvt_decode_dummy},
	{"ISO8859-11",	ENC_ISO8859_11,	rxvt_decode_dummy},
	{"ISO8859-12",	ENC_ISO8859_12,	rxvt_decode_dummy},
	{"ISO8859-13",	ENC_ISO8859_13,	rxvt_decode_dummy},
	{"ISO8859-14",	ENC_ISO8859_14,	rxvt_decode_dummy},
	{"ISO8859-15",	ENC_ISO8859_15,	rxvt_decode_dummy},
	{"ISO88591",	ENC_ISO8859_1,	rxvt_decode_dummy},
	{"ISO88592",	ENC_ISO8859_2,	rxvt_decode_dummy},
	{"ISO88593",	ENC_ISO8859_3,	rxvt_decode_dummy},
	{"ISO88594",	ENC_ISO8859_4,	rxvt_decode_dummy},
	{"ISO88595",	ENC_ISO8859_5,	rxvt_decode_dummy},
	{"ISO88596",	ENC_ISO8859_6,	rxvt_decode_dummy},
	{"ISO88597",	ENC_ISO8859_7,	rxvt_decode_dummy},
	{"ISO88598",	ENC_ISO8859_8,	rxvt_decode_dummy},
	{"ISO88599",	ENC_ISO8859_9,	rxvt_decode_dummy},
	{"ISO885910",	ENC_ISO8859_10,	rxvt_decode_dummy},
	{"ISO885911",	ENC_ISO8859_11,	rxvt_decode_dummy},
	{"ISO885912",	ENC_ISO8859_12,	rxvt_decode_dummy},
	{"ISO885913",	ENC_ISO8859_13,	rxvt_decode_dummy},
	{"ISO885914",	ENC_ISO8859_14,	rxvt_decode_dummy},
	{"ISO885915",	ENC_ISO8859_15,	rxvt_decode_dummy},
	{"ISO_8859-1",	ENC_ISO8859_1,	rxvt_decode_dummy},
	{"ISO_8859-2",	ENC_ISO8859_2,	rxvt_decode_dummy},
	{"ISO_8859-3",	ENC_ISO8859_3,	rxvt_decode_dummy},
	{"ISO_8859-4",	ENC_ISO8859_4,	rxvt_decode_dummy},
	{"ISO_8859-5",	ENC_ISO8859_5,	rxvt_decode_dummy},
	{"ISO_8859-6",	ENC_ISO8859_6,	rxvt_decode_dummy},
	{"ISO_8859-7",	ENC_ISO8859_7,	rxvt_decode_dummy},
	{"ISO_8859-8",	ENC_ISO8859_8,	rxvt_decode_dummy},
	{"ISO_8859-9",	ENC_ISO8859_9,	rxvt_decode_dummy},
	{"ISO_8859-10",	ENC_ISO8859_10,	rxvt_decode_dummy},
	{"ISO_8859-11",	ENC_ISO8859_11,	rxvt_decode_dummy},
	{"ISO_8859-12",	ENC_ISO8859_12,	rxvt_decode_dummy},
	{"ISO_8859-13",	ENC_ISO8859_13,	rxvt_decode_dummy},
	{"ISO_8859-14",	ENC_ISO8859_14,	rxvt_decode_dummy},
	{"ISO_8859-15",	ENC_ISO8859_15,	rxvt_decode_dummy},

	{"NOENC",		ENC_NOENC,		rxvt_decode_dummy},
	{"",			ENC_NOENC,		rxvt_decode_dummy},
	{NULL,			-1,				NULL},
};
#endif


struct NFONT_LIST {
	enum enc_label	encoding;
	char*			font[MAX_NFONTS];
};

static struct NFONT_LIST nfont_list[] = {
	{ENC_NOENC,		 {NFONT_LIST_NULL}},
#ifdef MULTICHAR_SET
	{ENC_SJIS,		 {NFONT_LIST_EUCJ}},
	{ENC_EUCJ,		 {NFONT_LIST_EUCJ}},
	{ENC_GB,		 {NFONT_LIST_GB}},
	{ENC_GBK,		 {NFONT_LIST_GBK}},
	{ENC_GB18030,	 {NFONT_LIST_GB18030}},
	{ENC_BIG5,		 {NFONT_LIST_BIG5}},
	{ENC_EUCKR,		 {NFONT_LIST_EUCKR}},
#endif
	{ENC_KOI8R,		 {NFONT_LIST_KOI8R}},
	{ENC_KOI8U,		 {NFONT_LIST_KOI8U}},
	{ENC_ISO8859_1,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_2,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_3,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_4,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_5,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_6,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_7,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_8,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_9,  {NFONT_LIST_NULL}},
	{ENC_ISO8859_10, {NFONT_LIST_NULL}},
	{ENC_ISO8859_11, {NFONT_LIST_NULL}},
	{ENC_ISO8859_12, {NFONT_LIST_NULL}},
	{ENC_ISO8859_13, {NFONT_LIST_NULL}},
	{ENC_ISO8859_14, {NFONT_LIST_NULL}},
	{ENC_ISO8859_15, {NFONT_LIST_NULL}},
};
static char* isofont[] = {NFONT_LIST_ISO8859X};

#ifdef MULTICHAR_SET
/* Multicharacter font names, roman fonts sized to match */
struct MFONT_LIST {
	enum enc_label	encoding;
	char*			mfont[MAX_NFONTS];
};

static struct MFONT_LIST mfont_list[] = {
	{ENC_NOENC,		 {MFONT_LIST_NULL}},
	{ENC_SJIS,		 {MFONT_LIST_EUCJ}},
	{ENC_EUCJ,		 {MFONT_LIST_EUCJ}},
	{ENC_GB,		 {MFONT_LIST_GB}},
	{ENC_GBK,		 {MFONT_LIST_GBK}},
	{ENC_GB18030,	 {MFONT_LIST_GB18030}},
	{ENC_BIG5,		 {MFONT_LIST_BIG5}},
	{ENC_EUCKR,		 {MFONT_LIST_EUCKR}},

	{ENC_KOI8R,		 {MFONT_LIST_NULL}},
	{ENC_KOI8U,		 {MFONT_LIST_NULL}},
	{ENC_ISO8859_1,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_2,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_3,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_4,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_5,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_6,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_7,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_8,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_9,  {MFONT_LIST_NULL}},
	{ENC_ISO8859_10, {MFONT_LIST_NULL}},
	{ENC_ISO8859_11, {MFONT_LIST_NULL}},
	{ENC_ISO8859_12, {MFONT_LIST_NULL}},
	{ENC_ISO8859_13, {MFONT_LIST_NULL}},
	{ENC_ISO8859_14, {MFONT_LIST_NULL}},
	{ENC_ISO8859_15, {MFONT_LIST_NULL}},
};
#endif	/* MULTICHAR_SET */


struct FALLBACK_FONT_LIST {
	enum enc_label	encoding;
	char*			fontname;
};

struct ENCODING_NAME {
	enum enc_label	encoding;
	char*			encname;
};

static struct ENCODING_NAME encoding_name[] = {
	{ENC_NOENC,		 "NOENC"},
# ifdef MULTICHAR_SET
	{ENC_SJIS,		 "SJIS"},
	{ENC_EUCJ,		 "EUCJP"},
	{ENC_GB,		 "GB2312"},
	{ENC_GBK,		 "GBK"},
	{ENC_GB18030,	 "GB18030"},
	{ENC_BIG5,		 "BIG5"},
	{ENC_EUCKR,		 "EUCKR"},
# endif
	{ENC_KOI8R,		 "KOI8R"},
	{ENC_KOI8U,		 "KOI8U"},
	{ENC_ISO8859_1,  "ISO8859-1"},
	{ENC_ISO8859_2,  "ISO8859-2"},
	{ENC_ISO8859_3,  "ISO8859-3"},
	{ENC_ISO8859_4,  "ISO8859-4"},
	{ENC_ISO8859_5,  "ISO8859-5"},
	{ENC_ISO8859_6,  "ISO8859-6"},
	{ENC_ISO8859_7,  "ISO8859-7"},
	{ENC_ISO8859_8,  "ISO8859-8"},
	{ENC_ISO8859_9,  "ISO8859-9"},
	{ENC_ISO8859_10, "ISO8859-10"},
	{ENC_ISO8859_11, "ISO8859-11"},
	{ENC_ISO8859_12, "ISO8859-12"},
	{ENC_ISO8859_13, "ISO8859-13"},
	{ENC_ISO8859_14, "ISO8859-14"},
	{ENC_ISO8859_15, "ISO8859-15"},
};


#ifdef XFT_SUPPORT
static struct FALLBACK_FONT_LIST fallback_mfont_list_xft[] = {
	{ENC_NOENC,		 "Luxi Mono"},
# ifdef MULTICHAR_SET
	{ENC_SJIS,		 "Kochi Gothic"},
	{ENC_EUCJ,		 "Kochi Gothic"},
	{ENC_GB,		 "SimSun"},
	{ENC_GBK,		 "SimSun"},
	{ENC_GB18030,	 "SimSun"},
	{ENC_BIG5,		 "MingLiU"},
	{ENC_EUCKR,		 "Luxi Mono"},
# endif
	{ENC_KOI8R,		 "Courier New"},
	{ENC_KOI8U,		 "Courier New"},
	{ENC_ISO8859_1,  "Luxi Mono"},
	{ENC_ISO8859_2,  "Luxi Mono"},
	{ENC_ISO8859_3,  "Luxi Mono"},
	{ENC_ISO8859_4,  "Luxi Mono"},
	{ENC_ISO8859_5,  "Luxi Mono"},
	{ENC_ISO8859_6,  "Luxi Mono"},
	{ENC_ISO8859_7,  "Luxi Mono"},
	{ENC_ISO8859_8,  "Luxi Mono"},
	{ENC_ISO8859_9,  "Luxi Mono"},
	{ENC_ISO8859_10, "Luxi Mono"},
	{ENC_ISO8859_11, "Luxi Mono"},
	{ENC_ISO8859_12, "Luxi Mono"},
	{ENC_ISO8859_13, "Luxi Mono"},
	{ENC_ISO8859_14, "Luxi Mono"},
	{ENC_ISO8859_15, "Luxi Mono"},
};
#endif /* XFT_SUPPORT */

static struct FALLBACK_FONT_LIST fallback_mfont_list_x11[] = {
	{ENC_NOENC,		 "-*-*-*-r-*-*-*-*-*-c-*-iso8859-1"},
# ifdef MULTICHAR_SET
	{ENC_SJIS,		 "-*-*-*-r-*-*-*-*-*-c-*-jisx0208*-0"},
	{ENC_EUCJ,		 "-*-*-*-r-*-*-*-*-*-c-*-jisx0208*-0"},
	{ENC_GB,		 "-*-*-*-*-*-*-*-*-*-*-*-*-gb2312*-0"},
	{ENC_GBK,		 "-*-*-*-*-*-*-*-*-*-*-*-*-gbk-0"},
	{ENC_GB18030,	 "-*-*-*-*-*-*-*-*-*-*-*-*-gb18030*-0"},
	{ENC_BIG5,		 "-*-*-*-*-*-*-*-*-*-*-c-*-big5-0"},
	{ENC_EUCKR,		 "-*-*-*-*-*-*-*-*-*-*-c-*-ksc5601*-0"},
# endif
	{ENC_KOI8R,		 "-*-*-*-r-*-*-*-*-*-c-*-koi8-r"},
	{ENC_KOI8U,		 "-*-*-*-r-*-*-*-*-*-c-*-koi8-u"},
	{ENC_ISO8859_1,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-1"},
	{ENC_ISO8859_2,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-2"},
	{ENC_ISO8859_3,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-3"},
	{ENC_ISO8859_4,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-4"},
	{ENC_ISO8859_5,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-5"},
	{ENC_ISO8859_6,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-6"},
	{ENC_ISO8859_7,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-7"},
	{ENC_ISO8859_8,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-8"},
	{ENC_ISO8859_9,  "-*-*-*-r-*-*-*-*-*-c-*-iso8859-9"},
	{ENC_ISO8859_10, "-*-*-*-r-*-*-*-*-*-c-*-iso8859-10"},
	{ENC_ISO8859_11, "-*-*-*-r-*-*-*-*-*-c-*-iso8859-11"},
	{ENC_ISO8859_12, "-*-*-*-r-*-*-*-*-*-c-*-iso8859-12"},
	{ENC_ISO8859_13, "-*-*-*-r-*-*-*-*-*-c-*-iso8859-13"},
	{ENC_ISO8859_14, "-*-*-*-r-*-*-*-*-*-c-*-iso8859-14"},
	{ENC_ISO8859_15, "-*-*-*-r-*-*-*-*-*-c-*-iso8859-15"},
};


char**		def_fontName;
char**		def_mfontName;



#ifdef MULTICHAR_SET
/* EXTPROTO */
void
rxvt_decode_euc2jis (unsigned char* str, int len)
{
	register int	i;

	DBG_MSG(2, (stderr, "rxvt_decode_euc2jis (%s : %d)\n", str, len));

	for (i = 0; i < len; i++)
		str[i] &= 0x7F;
}


/* EXTPROTO */
void
rxvt_decode_sjis2jis (unsigned char* str, int len)
{
	register int	i;
	unsigned char  *high, *low;

	DBG_MSG(1, (stderr, "rxvt_decode_sjis2jis\n"));

	for (i = 0; i < len; i += 2, str += 2) {
		high = str;
		low = str + 1;
		(*high) -= (*high > 0x9F ? 0xB1 : 0x71);
		*high = (*high) * 2 + 1;
		if (*low > 0x9E) {
			*low -= 0x7E;
			(*high)++;
		}
		else {
			if (*low > 0x7E)
				(*low)--;
			*low -= 0x1F;
		}
	}
}


/* EXTPROTO */
void
rxvt_decode_gb180302jis (unsigned char* str, int len)
{
	register int	i;

	DBG_MSG(2, (stderr, "rxvt_decode_gb180302jis\n"));

	for (i = 0; i < len; i++)
		str[i] &= 0x7F;
}


/* EXTPROTO */
void
rxvt_set_multichar_encoding (rxvt_t* r, const char* str)
{
	struct KNOWN_ENCODINGS*	a;

	assert (NULL != str);
	DBG_MSG(1,(stderr,"set multichar encoding to %s\n", str));

	a = (struct KNOWN_ENCODINGS*) known_encodings;
	for (; a->name; a++) {
		if (0 == STRCASECMP (str, a->name))	{
			r->encoding_method = a->method;
			r->h->multichar_decode = a->func;
			break;
		}
	}
	/* not a known encoding method */
	if (NULL == a->name)	{
		r->encoding_method = ENC_NOENC;
		r->h->multichar_decode = rxvt_decode_dummy;
	}

#ifdef XFT_SUPPORT
# ifdef HAVE_ICONV_H
	if ((iconv_t) -1 != r->TermWin.xfticonv)	{
		iconv_close (r->TermWin.xfticonv);
		r->TermWin.xfticonv = (iconv_t) -1;
	}
	/*
	** If encoding method is set AND mfont is loaded, open the
	** iconv. Otherwise, xfticonv is -1
	*/
	if (ENC_NOENC != r->encoding_method &&
		!(r->Options2 & Opt2_xftNomFont))
		r->TermWin.xfticonv = iconv_open ("UTF-8",
								rxvt_encoding_name(r));
# endif
#endif
}

#endif				/* MULTICHAR_SET */


/* EXTPROTO */
void
rxvt_decode_dummy (unsigned char* str, int len)
{
	DBG_MSG(2, (stderr, "rxvt_decode_dummy\n"));
}


/* EXTPROTO */
void
rxvt_set_default_locale (rxvt_t* r)
{
	char*	locale;
#if defined(HAVE_SETLOCALE) || defined(HAVE_XSETLOCALE)
	char*	lc;
#endif

	locale = getenv ("LC_ALL");
	if (NULL == locale)
		locale = getenv ("LC_CTYPE");
	if (NULL == locale)
		locale = getenv ("LANG");
	
#if defined(HAVE_SETLOCALE) || defined(HAVE_XSETLOCALE)
	lc = setlocale(LC_CTYPE, "");
	if (NULL == locale)
		locale = lc;
#endif

	DBG_MSG(1,(stderr,"set default locale to %s\n",
		locale ? locale : "none"));
	r->h->locale = locale;
}


/* EXTPROTO */
char*
rxvt_encoding_name (rxvt_t* r)
{
	assert (r->encoding_method >= 0);
	assert (r->encoding_method <= ENC_ISO8859_15);

	return encoding_name[r->encoding_method].encname;
}


#ifdef XFT_SUPPORT
/* Fallback XFT fonts */
/* EXTPROTO */
char*
rxvt_fallback_mfont_xft (rxvt_t* r)
{
	assert (r->encoding_method >= 0);
	assert (r->encoding_method <= ENC_ISO8859_15);

	return fallback_mfont_list_xft[r->encoding_method].fontname;
}


/* EXTPROTO */
void
rxvt_set_default_font_xft (rxvt_t* r)
{
	if ((r->Options & Opt_xft) && 
		(NULL == r->h->rs[Rs_xftfont]))
		r->h->rs[Rs_xftfont] = "Luxi Mono";

# ifdef MULTICHAR_SET
	if ((r->Options & Opt_xft) && 
		(NULL == r->h->rs[Rs_xftmfont]))
		r->h->rs[Rs_xftmfont] = rxvt_fallback_mfont_xft (r);
# endif
}
#endif	/* XFT_SUPPORT */


/* Fallback X11 fonts */
/* EXTPROTO */
char*
rxvt_fallback_mfont_x11 (rxvt_t* r)
{
	assert (r->encoding_method >= 0);
	assert (r->encoding_method <= ENC_ISO8859_15);

	return fallback_mfont_list_x11[r->encoding_method].fontname;
}


/* EXTPROTO */
void
rxvt_set_default_font_x11 (rxvt_t* r)
{
	register int	i;


	DBG_MSG(1,(stderr,"rxvt_set_default_font_x11\n"));

	/* Set default fonts */
	def_fontName = (char**) nfont_list[r->encoding_method].font;

#ifdef MULTICHAR_SET
	switch (r->encoding_method)	{
	case ENC_SJIS :
		def_mfontName = (char**) mfont_list[ENC_SJIS].mfont;
		break;
	case ENC_EUCJ :
		def_mfontName = (char**) mfont_list[ENC_EUCJ].mfont;
		break;
	case ENC_GB   :
		def_mfontName = (char**) mfont_list[ENC_GB].mfont;
		break;
	case ENC_GBK  :
		def_mfontName = (char**) mfont_list[ENC_GBK].mfont;
		break;
	case ENC_GB18030  :
		def_mfontName = (char**) mfont_list[ENC_GB18030].mfont;
		break;
	case ENC_BIG5 :
		def_mfontName = (char**) mfont_list[ENC_BIG5].mfont;
		break;
	case ENC_EUCKR:
		def_mfontName = (char**) mfont_list[ENC_EUCKR].mfont;
		break;
	default:
		def_mfontName = (char**) mfont_list[r->encoding_method].mfont;
		break;
	}

	/* Found no mfont, fall back to ISO8859-X font */
	if (NULL == def_mfontName[0])	{
		for (i = 0; i < MAX_NFONTS; i ++)	{
			char*	ptr = rxvt_malloc (STRLEN(isofont[i])+4);
			if (r->encoding_method >= ENC_ISO8859_1 &&
				r->encoding_method <= ENC_ISO8859_15)
				sprintf (ptr, isofont[i],
					r->encoding_method - ENC_ISO8859_1 + 1);
			else
				sprintf (ptr, isofont[i], 1);
			def_mfontName[i] = ptr;
		}
	}
#endif

	/* Found no font, fall back to ISO8859-X font */
	if (NULL == def_fontName[0])	{
		for (i = 0; i < MAX_NFONTS; i ++)	{
			char*	ptr = rxvt_malloc (STRLEN(isofont[i])+4);
#ifdef MULTICHAR_SET
			if (r->encoding_method >= ENC_ISO8859_1 &&
				r->encoding_method <= ENC_ISO8859_15)
				sprintf (ptr, isofont[i],
					r->encoding_method - ENC_ISO8859_1 + 1);
			else
#endif
			sprintf (ptr, isofont[i], 1);
			def_fontName[i] = ptr;
		}
	}

	/* Overrided by -km option or X resources */
	for (i = 0; i < MAX_NFONTS; i ++)	{
		if (NULL == r->h->rs[Rs_font +i])
			r->h->rs[Rs_font +i] = def_fontName[i];
#ifdef MULTICHAR_SET
		if (NULL == r->h->rs[Rs_mfont +i])
			r->h->rs[Rs_mfont +i] = def_mfontName[i];
#endif
	}
}

/*----------------------- end-of-file (C source) -----------------------*/
