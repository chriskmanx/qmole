/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef PLAN9
#include	<u.h>
#include	<libc.h>
#include	<bio.h>
#else
#ifndef _WIN32
#include <sys/types.h>
#endif
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>
/* already done in wv.h
 #include "config.h"
*/
#include	"plan9.h"
#endif
#include	"hdr.h"
#include 	"wv.h"
#include 	"utf.h"

enum {
    Char1 = Runeself, Rune1 = Runeself,
    Char21 = 0xA1, Rune21 = 0x0100,
    Char22 = 0xF6, Rune22 = 0x4016,
    Char3 = 0xFC, Rune3 = 0x10000,	/* really 0x38E2E */
    Esc = 0xBE, Bad = Runeerror
};

#ifdef PLAN9
int errno;
#endif

enum {
    T1 = 0x00,
    Tx = 0x80,
    T2 = 0xC0,
    T3 = 0xE0,
    T4 = 0xF0,
    T5 = 0xF8,
    T6 = 0xFC,

    Bit1 = 7,
    Bitx = 6,
    Bit2 = 5,
    Bit3 = 4,
    Bit4 = 3,
    Bit5 = 2,
    Bit6 = 2,

    Mask1 = (1 << Bit1) - 1,
    Maskx = (1 << Bitx) - 1,
    Mask2 = (1 << Bit2) - 1,
    Mask3 = (1 << Bit3) - 1,
    Mask4 = (1 << Bit4) - 1,
    Mask5 = (1 << Bit5) - 1,
    Mask6 = (1 << Bit6) - 1,

    Wchar1 = (1UL << Bit1) - 1,
    Wchar2 = (1UL << (Bit2 + Bitx)) - 1,
    Wchar3 = (1UL << (Bit3 + 2 * Bitx)) - 1,
    Wchar4 = (1UL << (Bit4 + 3 * Bitx)) - 1,
    Wchar5 = (1UL << (Bit5 + 4 * Bitx)) - 1
#ifndef	EILSEQ
	,			/* we hate ansi c's comma rules */
    EILSEQ = 123
#endif				/* PLAN9 */
};

int
our_wctomb (char *s, U16 wc)
{
    if (s == 0)
	return 0;		/* no shift states */
    if (wc & ~Wchar2)
      {
	  if (wc & ~Wchar4)
	    {
		if (wc & ~Wchar5)
		  {
		      /* 6 bytes */
		      s[0] = T6 | ((wc >> 5 * Bitx) & Mask6);
		      s[1] = Tx | ((wc >> 4 * Bitx) & Maskx);
		      s[2] = Tx | ((wc >> 3 * Bitx) & Maskx);
		      s[3] = Tx | ((wc >> 2 * Bitx) & Maskx);
		      s[4] = Tx | ((wc >> 1 * Bitx) & Maskx);
		      s[5] = Tx | (wc & Maskx);
		      return 6;
		  }
		/* 5 bytes */
		s[0] = T5 | (wc >> 4 * Bitx);
		s[1] = Tx | ((wc >> 3 * Bitx) & Maskx);
		s[2] = Tx | ((wc >> 2 * Bitx) & Maskx);
		s[3] = Tx | ((wc >> 1 * Bitx) & Maskx);
		s[4] = Tx | (wc & Maskx);
		return 5;
	    }
	  if (wc & ~Wchar3)
	    {
		/* 4 bytes */
		s[0] = T4 | (wc >> 3 * Bitx);
		s[1] = Tx | ((wc >> 2 * Bitx) & Maskx);
		s[2] = Tx | ((wc >> 1 * Bitx) & Maskx);
		s[3] = Tx | (wc & Maskx);
		return 4;
	    }
	  /* 3 bytes */
	  s[0] = T3 | (wc >> 2 * Bitx);
	  s[1] = Tx | ((wc >> 1 * Bitx) & Maskx);
	  s[2] = Tx | (wc & Maskx);
	  return 3;
      }
    if (wc & ~Wchar1)
      {
	  /* 2 bytes */
	  s[0] = T2 | (wc >> 1 * Bitx);
	  s[1] = Tx | (wc & Maskx);
	  return 2;
      }
    /* 1 byte */
    s[0] = T1 | wc;
    return 1;
}

int
our_mbtowc (U16 * p, char *s, unsigned n)
{
    U8 *us;
    int c0, c1, c2, c3, c4, c5;
    U16 wc;

    if (s == 0)
	return 0;		/* no shift states */

    if (n < 1)
	goto badlen;
    us = (U8 *) s;
    c0 = us[0];
    if (c0 >= T3)
      {
	  if (n < 3)
	      goto badlen;
	  c1 = us[1] ^ Tx;
	  c2 = us[2] ^ Tx;
	  if ((c1 | c2) & T2)
	      goto bad;
	  if (c0 >= T5)
	    {
		if (n < 5)
		    goto badlen;
		c3 = us[3] ^ Tx;
		c4 = us[4] ^ Tx;
		if ((c3 | c4) & T2)
		    goto bad;
		if (c0 >= T6)
		  {
		      /* 6 bytes */
		      if (n < 6)
			  goto badlen;
		      c5 = us[5] ^ Tx;
		      if (c5 & T2)
			  goto bad;
		      wc = ((((((((((c0 & Mask6) << Bitx) |
				   c1) << Bitx) | c2) << Bitx) |
			       c3) << Bitx) | c4) << Bitx) | c5;
		      if (wc <= Wchar5)
			  goto bad;
		      *p = wc;
		      return 6;
		  }
		/* 5 bytes */
		wc = ((((((((c0 & Mask5) << Bitx) |
			   c1) << Bitx) | c2) << Bitx) | c3) << Bitx) | c4;
		if (wc <= Wchar4)
		    goto bad;
		*p = wc;
		return 5;
	    }
	  if (c0 >= T4)
	    {
		/* 4 bytes */
		if (n < 4)
		    goto badlen;
		c3 = us[3] ^ Tx;
		if (c3 & T2)
		    goto bad;
		wc = ((((((c0 & Mask4) << Bitx) |
			 c1) << Bitx) | c2) << Bitx) | c3;
		if (wc <= Wchar3)
		    goto bad;
		*p = wc;
		return 4;
	    }
	  /* 3 bytes */
	  wc = ((((c0 & Mask3) << Bitx) | c1) << Bitx) | c2;
	  if (wc <= Wchar2)
	      goto bad;
	  *p = wc;
	  return 3;
      }
    if (c0 >= T2)
      {
	  /* 2 bytes */
	  if (n < 2)
	      goto badlen;
	  c1 = us[1] ^ Tx;
	  if (c1 & T2)
	      goto bad;
	  wc = ((c0 & Mask2) << Bitx) | c1;
	  if (wc <= Wchar1)
	      goto bad;
	  *p = wc;
	  return 2;
      }
    /* 1 byte */
    if (c0 >= Tx)
	goto bad;
    *p = c0;
    return 1;

  bad:
    errno = EILSEQ;
    return -1;
  badlen:
    return -2;
}
