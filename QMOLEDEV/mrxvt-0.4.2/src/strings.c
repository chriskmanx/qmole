/*--------------------------------*-C-*---------------------------------*
 * File:	strings.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2003-2004   Marc Lehmann <pcg@goof.com>
 * Copyright (c) 2004-2005   Jingmin Zhou <jimmyzhou@users.sourceforge.net>
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
** $Id: strings.c,v 1.19 2005/03/21 00:42:08 cvs Exp $
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


#ifdef HAVE_WCHAR_H
/* EXTPROTO */
char*
rxvt_wcstombs (const wchar_t* str, int len)
{
	mbstate_t	mbs;
	char*	r;
	char*	dst;

	if (len < 0)
		len = wcslen (str);

	memset (&mbs, 0, sizeof (mbs));
	r = (char *)rxvt_malloc (len * MB_CUR_MAX + 1);
	dst = r;

	while (len--) {
		int l = wcrtomb (dst, *str++, &mbs);
		if (l < 0)
			*dst++ = '?';
		else
			dst += l;
	}

	*dst++ = 0;

	return r;
}


/* EXTPROTO */
wchar_t*
rxvt_mbstowcs (const char* str)
{
	wchar_t*	r;
	int			len = STRLEN (str);

	r = (wchar_t *)rxvt_malloc ((len + 1) * sizeof (wchar_t));

	if (mbstowcs (r, str, len + 1) < 0)
		*r = 0;

	return r;
}


/* EXTPROTO */
char*
rxvt_wcstoutf8 (const wchar_t* str)
{
	char*	r;
	char*	p;
	int		len;
	
	len = wcslen (str);

	r = (char *)rxvt_malloc (len * 4 + 1);
	p = r;

	while (len--) {
		unicode_t w = *str++ & UNICODE_MASK;

		if      (w < 0x000080)
			*p++ = w;
		else if (w < 0x000800)	{
			*p++ = 0xc0 | ( w >> 6);
			*p++ = 0x80 | ( w & 0x3f);
		}
		else if (w < 0x010000)	{
			*p++ = 0xe0 | ( w >> 12 );
			*p++ = 0x80 | ((w >> 6) & 0x3f);
			*p++ = 0x80 | ( w & 0x3f);
		}
		else if (w < 0x110000)	{
			*p++ = 0xf0 | ( w >> 18);
			*p++ = 0x80 | ((w >> 12) & 0x3f);
			*p++ = 0x80 | ((w >> 6) & 0x3f);
			*p++ = 0x80 | ( w & 0x3f);
		}
		else
			*p++ = '?';
	}

	*p = 0;

	return r;
}


/* EXTPROTO */
wchar_t*
rxvt_utf8towcs (const char* str)
{
	wchar_t*	r;
	wchar_t*	p;
	unsigned char*	s;
	unsigned char*	e;
	int			len = STRLEN(str);


	r = (wchar_t *)rxvt_malloc ((len + 1) * sizeof (wchar_t)),
	p = r;

	s = (unsigned char*)str,
	e = s + len;

	while (1) {
		len = e - s;

		if (len == 0)
			break;
		else if (s[0] < 0x80)
			*p++ = *s++;
		else if (len >= 2 &&
			(s[0] >= 0xc2 && s[0] <= 0xdf) &&
			(s[1] & 0xc0) == 0x80) {
			*p++ = ((s[0] & 0x1f) << 6) | (s[1] & 0x3f);
			s += 2;
		}
		else if (len >= 3 &&
			((s[0] == 0xe0 && s[1] >= 0xa0 && s[1] <= 0xbf) ||
			 (s[0] >= 0xe1 && s[0] <= 0xec && s[1] >= 0x80 && s[1] <= 0xbf) ||
			 (s[0] == 0xed && s[1] >= 0x80 && s[1] <= 0x9f) ||
			 (s[0] >= 0xee && s[0] <= 0xef && s[1] >= 0x80 && s[1] <= 0xbf) ) &&
			(s[2] & 0xc0) == 0x80)	{
			*p++ =	((s[0] & 0x0f) << 12) |
					((s[1] & 0x3f) <<  6) | 
					(s[2] & 0x3f);
			s += 3;
		}
		else if (len >= 4 &&
			((s[0] == 0xf0 && s[1] >= 0x90 && s[1] <= 0xbf) ||
			 (s[0] >= 0xf1 && s[0] <= 0xf3 && s[1] >= 0x80 && s[1] <= 0xbf) ||
			 (s[0] == 0xf4 && s[1] >= 0x80 && s[1] <= 0x8f)) &&
			(s[2] & 0xc0) == 0x80 && (s[3] & 0xc0) == 0x80) {
			*p++ =	((s[0] & 0x07) << 18) |
					((s[1] & 0x3f) << 12) |
					((s[2] & 0x3f) <<  6) |
					(s[3] & 0x3f);
			s += 4;
		}
		else {
			*p++ = 0xfffd;
			s++;
		}
	}	/* while (1) */

	*p = 0;

	return r;
}
#endif	/* HAVE_WCHAR_H */


/*----------------------------------------------------------------------*/
/*
 * a replacement for strcasecmp() to avoid linking an entire library.
 * Mark Olesen added this in 2.15 but for which OS & library? - Geoff Wing
 */
/* EXTPROTO */
int
ma_strcasecmp(const char *s1, const char *s2)
{
    for ( ; tolower((int) *s1) == tolower((int) *s2); s1++, s2++)
	if (!*s1)
	    return 0;
    return (int)(tolower((int) *s1) - tolower((int) *s2));
}

/* EXTPROTO */
int
ma_strncasecmp(const char *s1, const char *s2, size_t n)
{
    for ( ; n-- && (tolower((int) *s1) == tolower((int) *s2)); s1++, s2++)
	if (!*s1)
	    return 0;
    if (n == 0)
	return 0;
    return (int)(tolower((int) *s1) - tolower((int) *s2));
}

/* EXTPROTO */
char*
ma_strcpy(char *d, const char *s)
{
    char          *r = d;

    for ( ; (*r++ = *s++) != '\0'; )
		;
    return d;
}

/* EXTPROTO */
char*
ma_strncpy(char *d, const char* s, size_t len)
{
    char          *r = d;

    if (len)
		for ( ; len; len--)
		    if ((*r++ = *s++) == '\0') {
				for ( ; --len; )
				    *r++ = '\0';
				break;
		    }
    return d;
}

/* EXTPROTO */
int
ma_strcmp(const char *s1, const char *s2)
{
    for ( ; (*s1 == *s2++); )
		if (*s1++ == '\0')
		    return 0;
    return (int) ((unsigned char) *s1 - (unsigned char) *--s2);
}

/* EXTPROTO */
int
ma_strncmp(const char *s1, const char *s2, size_t len)
{
    if (len) {
		for ( ; len-- && (*s1++ == *s2++); )
			;
		if (++len)
		    return (int) ((unsigned char) *--s1 - (unsigned char) *--s2);
    }
    return 0;
}

/* EXTPROTO */
char*
ma_strcat(char *s1, const char *s2)
{
    char           *r = s1;

    if (*r != '\0')
		for ( ; *++r != '\0'; )
			;
    for ( ; (*r++ = *s2++) != '\0'; )
		;

    return s1;
}

/* EXTPROTO */
char*
ma_strncat(char *s1, const char *s2, size_t len)
{
    char           *r = s1;

    if (*r != '\0')
		for ( ; *++r != '\0'; )
			;
    for ( ; len-- && ((*r++ = *s2++) != '\0'); )
		;
    *r = '\0';

    return s1;
}

/* EXTPROTO */
size_t
ma_strlen(const char *s)
{
    size_t         len = 0;

    for ( ; *s++ != '\0'; len++)
		;
    return len;
}


/* EXTPROTO */
char*
ma_strdup(const char *s)
{
	int			len = STRLEN(s) + 1;
    char*		c;

	assert (len > 0);	/* possible integer overflow? */
    if ((c = rxvt_malloc(len)) != NULL)
		MEMCPY(c, s, len);
    return c;
}


/* EXTPROTO */
char UNTAINTED *
ma_strndup(const char TAINTED * s, size_t sz)
{
    char*		c;
	int			len = sz + 1;

	assert (len > 0);	/* possible integer overflow? */
    if ((c = rxvt_malloc(len)) != NULL)
		STRNCPY(c, s, sz);
	c[sz] = '\0';
    return c;
}


/* EXTPROTO */
char*
ma_index(const char* s, int c)
{
    return STRCHR(s, c);
}

/* EXTPROTO */
char*
ma_strchr(const char* s, int c)
{
    char          *p = NULL;

    for (;;) {
		if (*s == (char)c) {
		    p = (char *)s;
		    break;
		}
		if (*s++ == '\0')
		    break;
    }
    return p;

}

/* EXTPROTO */
char*
ma_rindex(const char* s, int c)
{
    return STRRCHR(s, c);
}

/* EXTPROTO */
char*
ma_strrchr(const char* s, int c)
{
    char          *p = NULL;

    for (;;) {
		if (*s == (char)c)
		    p = (char *)s;
		if (*s++ == '\0')
		    break;
    }
    return p;
}

/* EXTPROTO */
void*
ma_memcpy(void *s1, const void *s2, size_t len)
{
    /* has extra stack and time but less code space */
    return MEMMOVE(s1, s2, len);
}

/*--------------------------------------------------------------------------*
 * Possibly faster memmove() by Geoff Wing <mason@primenet.com.au>
 *--------------------------------------------------------------------------*/
/* EXTPROTO */
void*
ma_memmove(void *d, const void *s, size_t len)
{
    u_intp_t        i;
	u_intp_t*		pdst;
	u_intp_t*		psrc;
    unsigned char*	dst = (unsigned char *)d;
    unsigned char*	src = (unsigned char *)s;

    if (len && d != s) {
		if ((u_intp_t)d < (u_intp_t)s) {
			/* forwards */
		    i = (-(u_intp_t)dst) & (SIZEOF_INT_P - 1);
		    if (len >= 16 &&
				i == ((-(u_intp_t)src) & (SIZEOF_INT_P - 1))) {
			    /* speed up since src & dst are offset correctly */
				len -= (size_t)i;
				for ( ; i--; )
				    *dst++ = *src++;
				/* assign the src/dst to psrc/pdst */
				pdst = (u_intp_t*) dst;
				psrc = (u_intp_t*) src;
				for (i = (u_intp_t)(len / SIZEOF_INT_P); i--; )
					*pdst++ = *psrc++;
				len &= (SIZEOF_INT_P - 1);
				/* assign back the src/dst */
				dst = (unsigned char*) pdst;
				src = (unsigned char*) psrc;
			}
			/* the left bytes */
		    for ( ; len--; )
				*dst++ = *src++;
		}
		else {
			/* backwards */
		    dst += len;
		    src += len;
		    i = ((u_intp_t)dst) & (SIZEOF_INT_P - 1);
		    if (len >= 16 &&
				i == (((u_intp_t)src) & (SIZEOF_INT_P - 1))) {
			    /* speed up since src & dst are offset correctly */
				len -= (size_t)i;
				for ( ; i--; )
				    *--dst = *--src;
				/* assign the src/dst to psrc/pdst */
				pdst = (u_intp_t*) dst;
				psrc = (u_intp_t*) src;
				for (i = (u_intp_t)(len / SIZEOF_INT_P); i--; )
				    *--pdst = *--psrc;
				len &= (SIZEOF_INT_P - 1);
				/* assign back the src/dst */
				dst = (unsigned char*) pdst;
				src = (unsigned char*) psrc;
			}
			/* the left bytes */
		    for ( ; len--; )
				*--dst = *--src;
		}
    }
    return d;
}

/*--------------------------------------------------------------------------*
 * Possibly faster memset() by Geoff Wing <mason@primenet.com.au>
 * presumptions:
 *   1) intp_t write the best
 *   2) SIZEOF_INT_P == power of 2
 *--------------------------------------------------------------------------*/

/* EXTPROTO */
void
ma_bzero(void* buf, size_t len)
{
    MEMSET(buf, 0, len);
}

/* EXTPROTO */
void*
ma_memset(void *p, int c1, size_t len)
{
    u_intp_t        i, val;
	u_intp_t*		pdst;
    unsigned char   c = (unsigned char) c1;
    unsigned char  *lp = (unsigned char *) p;

    if (len) {
		if (len >= 16) {
			/*
			** < 16 probably not worth all the calculations
			** write out preceding characters so we align on an
			** integer boundary
			*/
		    if ((i = ((-(u_intp_t)p) & (SIZEOF_INT_P - 1)))) {
				len -= (size_t)i;
				for (; i--;)
				    *lp++ = c;
		    }

			/* do the fast writing */
		    val = (c << 8) + c;
#if SIZEOF_INT_P >= 4
		    val |= (val << 16);
#endif
#if SIZEOF_INT_P >= 8
		    val |= (val << 32);
#endif
#if SIZEOF_INT_P == 16
		    val |= (val << 64);
#endif
			/* assign the lp to pdst */
			pdst = (u_intp_t*) lp;
		    for (i = (u_intp_t)(len / SIZEOF_INT_P); i--;)
				*pdst++ = val;
		    len &= (SIZEOF_INT_P - 1);
			/* assign back the pdst to lp */
			lp = (unsigned char*) pdst;
		}
		/* write trailing characters */
		for (; len--;)
		    *lp++ = c;
    }
    return p;
}
/*----------------------- end-of-file (C source) -----------------------*/
