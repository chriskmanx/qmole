/*--------------------------------*-H-*---------------------------------*
 * File:	encoding.h
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (C) 2001        Tomohiro KUBOTA <kubota@debian.org>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
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
 *----------------------------------------------------------------------*/
/*
** $Id: encoding.h,v 1.14 2004/12/18 07:15:02 cvs Exp $
*/

#ifndef __ENCODING_H__
#define __ENCODING_H__


/* 
 * Font definitions for supported encodings
 *
 * Font Usage Policy (suggested):
 *    1. Use fonts available from XFree86 as much as possible.
 *    2. Use popular fonts in the language community as much as possible.
 *    3. Use "OpenSource" (by Open Source Definition,
 *       http://www.opensource.org/) fonts as much as possible.
 *
 * Comments are the source of these fonts.
 * xf      XFree86 distribution
 * pd      public domain fonts from ftp://ftp.gnu.org/pub/gnu/
 * ak      public domain "a12k12" fonts
 * na      "naga10" from http://gondow-www.cs.titech.ac.jp/~snagao/fonts/
 * cr      "Xcyr" fonts from http://sawsoft.newmail.ru/LS/
 * ba      "baekmuk" fonts from ftp://ftp.mizi.co.kr/pub/baekmuk
 *
 * These definitions should be brushed up by native speakers.
 */

#define MAX_NFONTS		(6)
#define FONT0_IDX		(0)


#ifdef MULTICHAR_SET
#define NFONT_LIST_EUCJ \
  "7x14", "8x16", "9x18", "12x24",\
  "-misc-fixed-medium-r-normal--10-90-75-75-c-50-iso8859-1",\
  "6x12"
#define MFONT_LIST_EUCJ \
  "-misc-fixed-medium-r-normal--14-130-75-75-c-140-jisx0208.1983-0",\
  "-misc-fixed-medium-r-normal--10-90-75-75-c-100-jisx0208.1983-0",\
  "-misc-fixed-medium-r-normal--12-110-75-75-c-120-jisx0208.1983-0",\
  "-jis-fixed-medium-r-normal--16-150-75-75-c-160-jisx0208.1983-0",\
  "-jis-gothic-medium-r-normal--18-170-75-75-c-180-jisx0208.1983-0",\
  "-jis-fixed-medium-r-normal--24-230-75-75-c-240-jisx0208.1983-0"


#define NFONT_LIST_GB "8x16", "8x16", "12x24", "8x16", "8x16", "12x24"
#define MFONT_LIST_GB \
  "-isas-song ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0",\
  "-isas-fangsong ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0",\
  "-isas-song ti-medium-r-normal--24-240-72-72-c-240-gb2312.1980-0",\
  "-isas-song ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0",\
  "-isas-fangsong ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0",\
  "-isas-song ti-medium-r-normal--24-240-72-72-c-240-gb2312.1980-0"


#define NFONT_LIST_GBK "7x14", "8x16", "6x12", "7x14", "8x16", "6x12"
#define MFONT_LIST_GBK \
  "-xtm-songti-medium-r-normal--14-140-75-75-c-140-gbk-0",\
  "-xtm-songti-medium-r-normal--16-160-75-75-c-160-gbk-0",\
  "-xtm-songti-medium-r-normal--12-120-75-75-c-120-gbk-0",\
  "-xtm-songti-medium-r-normal--14-140-75-75-c-140-gbk-0",\
  "-xtm-songti-medium-r-normal--16-160-75-75-c-160-gbk-0",\
  "-xtm-songti-medium-r-normal--12-120-75-75-c-120-gbk-0"


#define NFONT_LIST_GB18030 "8x16", "8x16", "8x16", "8x16", "8x16", "8x16"
#define MFONT_LIST_GB18030 \
  "-misc-simsun-medium-r-normal--16-0-0-0-p-0-gb18030.2000-0",\
  "-misc-simhei-medium-r-normal--16-0-0-0-p-0-gb18030.2000-0",\
  "-misc-fzyuanti-medium-i-normal--16-0-0-0-p-0-gb18030.2000-0",\
  "-misc-simsun-medium-r-normal--16-0-0-0-p-0-gb18030.2000-0",\
  "-misc-simhei-medium-r-normal--16-0-0-0-p-0-gb18030.2000-0",\
  "-misc-fzyuanti-medium-i-normal--16-0-0-0-p-0-gb18030.2000-0"


#define NFONT_LIST_BIG5 "8x16", "10x20", "12x24", "8x16", "10x20", "12x24"
#define MFONT_LIST_BIG5 \
  "-taipei-fixed-medium-r-normal--16-150-75-75-c-160-big5-0",\
  "-taipei-fixed-medium-r-normal--20-200-75-75-c-200-big5-0",\
  "-taipei-fixed-medium-r-normal--24-240-75-75-c-240-big5-0",\
  "-eten-fixed-medium-r-normal--16-150-75-75-c-160-big5-0",\
  "-eten-fixed-medium-r-normal--20-200-75-75-c-200-big5-0",\
  "-eten-fixed-medium-r-normal--24-230-75-75-c-240-big5-0"


#define NFONT_LIST_EUCKR "8x16", "9x18", "10x20", "12x24", "6x12", "7x14"
#define MFONT_LIST_EUCKR \
  "-daewoo-mincho-medium-r-normal--16-120-100-100-c-160-ksc5601.1987-0",\
  "-baekmuk-batang-medium-r-normal--18-180-75-75-m-180-ksc5601.1987-0",\
  "-baekmuk-batang-medium-r-normal--20-200-75-75-m-200-ksc5601.1987-0",\
  "-daewoo-mincho-medium-r-normal--24-170-100-100-c-240-ksc5601.1987-0",\
  "-baekmuk-batang-medium-r-normal--12-120-75-75-m-120-ksc5601.1987-0",\
  "-baekmuk-batang-medium-r-normal--14-140-75-75-m-140-ksc5601.1987-0"

#define MFONT_LIST_NULL		NULL,NULL,NULL,NULL,NULL,NULL
#endif /* MULTICHAR_SET */


#define NFONT_LIST_KOI8R \
  "-misc-fixed-medium-r-normal--14-130-75-75-c-70-koi8-r",\
  "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-koi8-r",\
  "-misc-fixed-medium-r-normal--13-120-75-75-c-80-koi8-r",\
  "-misc-fixed-medium-r-normal--15-140-75-75-c-90-koi8-r",\
  "-misc-fixed-medium-r-normal--18-120-100-100-c-90-koi8-r",\
  "-misc-fixed-medium-r-normal--20-200-75-75-c-100-koi8-r"

#define NFONT_LIST_KOI8U \
  "-cronyx-fixed-medium-r-normal--14-130-75-75-c-70-koi8-u",\
  "-cronyx-fixed-medium-r-semicondensed--13-120-75-75-c-60-koi8-u",\
  "-cronyx-fixed-medium-r-normal--13-120-75-75-c-80-koi8-u",\
  "-cronyx-fixed-medium-r-normal--15-140-75-75-c-90-koi8-u",\
  "-cronyx-fixed-medium-r-normal--18-120-100-100-c-90-koi8-u",\
  "-cronyx-fixed-medium-r-normal--20-200-75-75-c-100-koi8-u"

/* special common rule for ISO-8859-* */
#define NFONT_LIST_ISO8859X \
  "-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso8859-%d",\
  "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-%d",\
  "-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso8859-%d",\
  "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-%d",\
  "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-%d",\
  "-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso8859-%d"

#define NFONT_LIST_NULL		NULL,NULL,NULL,NULL,NULL,NULL


enum enc_label {
	ENC_NOENC,
#ifdef MULTICHAR_SET
	ENC_SJIS , ENC_EUCJ, ENC_GB, ENC_GBK, ENC_GB18030,
	ENC_BIG5, ENC_EUCKR,
#endif
	ENC_KOI8R, ENC_KOI8U,
	ENC_ISO8859_1, ENC_ISO8859_2, ENC_ISO8859_3, ENC_ISO8859_4,
	ENC_ISO8859_5, ENC_ISO8859_6, ENC_ISO8859_7, ENC_ISO8859_8,
	ENC_ISO8859_9, ENC_ISO8859_10, ENC_ISO8859_11, ENC_ISO8859_12,
	ENC_ISO8859_13, ENC_ISO8859_14, ENC_ISO8859_15,
};


#endif /* __ENCODING_H__ */
