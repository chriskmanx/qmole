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

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "wv.h"

void
wvGetFILETIME (FILETIME * ft, wvStream * fd)
{
    ft->dwLowDateTime = read_32ubit (fd);
    ft->dwHighDateTime = read_32ubit (fd);
}

void
wvInitFILETIME (FILETIME * ft)
{
    ft->dwLowDateTime = 0;
    ft->dwHighDateTime = 0;
}

int
wvFileTimeToDosDateTime (const FILETIME * ft, U16 * fatdate, U16 * fattime)
{
    time_t unixtime = wvDOSFS_FileTimeToUnixTime (ft, NULL);
    struct tm *tm = localtime (&unixtime);
    if (fattime)
	*fattime = (tm->tm_hour << 11) + (tm->tm_min << 5) + (tm->tm_sec / 2);
    if (fatdate)
	*fatdate = ((tm->tm_year - 80) << 9) + ((tm->tm_mon + 1) << 5)
	    + tm->tm_mday;
    return (1);
}


/***********************************************************************
 *           DOSFS_FileTimeToUnixTime
 *
 * Convert a FILETIME format to Unix time.
 * If not NULL, 'remainder' contains the fractional part of the filetime,
 * in the range of [0..9999999] (even if time_t is negative).
 */
time_t
wvDOSFS_FileTimeToUnixTime (const FILETIME * filetime, U32 * remainder)
{
    U32 a0;			/* 16 bit, low    bits */
    U32 a1;			/* 16 bit, medium bits */
    U32 a2;			/* 32 bit, high   bits */
    U32 r;			/* remainder of division */
    unsigned int carry;		/* carry bit for subtraction */
    int negative;		/* whether a represents a negative value */

    /* Copy the time values to a2/a1/a0 */
    a2 = filetime->dwHighDateTime;
    a1 = ((U32) filetime->dwLowDateTime) >> 16;
    a0 = ((U32) filetime->dwLowDateTime) & 0xffff;

    /* Subtract the time difference */
    if (a0 >= 32768)
	a0 -= 32768, carry = 0;
    else
	a0 += (1 << 16) - 32768, carry = 1;

    if (a1 >= 54590 + carry)
	a1 -= 54590 + carry, carry = 0;
    else
	a1 += (1 << 16) - 54590 - carry, carry = 1;

    a2 -= 27111902 + carry;

    /* If a is negative, replace a by (-1-a) */
    negative = (a2 >= ((U32) 1) << 31);
    if (negative)
      {
	  /* Set a to -a - 1 (a is a2/a1/a0) */
	  a0 = 0xffff - a0;
	  a1 = 0xffff - a1;
	  a2 = ~a2;
      }

    /* Divide a by 10000000 (a = a2/a1/a0), put the rest into r.
       Split the divisor into 10000 * 1000 which are both less than 0xffff. */
    a1 += (a2 % 10000) << 16;
    a2 /= 10000;
    a0 += (a1 % 10000) << 16;
    a1 /= 10000;
    r = a0 % 10000;
    a0 /= 10000;

    a1 += (a2 % 1000) << 16;
    a2 /= 1000;
    a0 += (a1 % 1000) << 16;
    a1 /= 1000;
    r += (a0 % 1000) * 10000;
    a0 /= 1000;

    /* If a was negative, replace a by (-1-a) and r by (9999999 - r) */
    if (negative)
      {
	  /* Set a to -a - 1 (a is a2/a1/a0) */
	  a0 = 0xffff - a0;
	  a1 = 0xffff - a1;
	  a2 = ~a2;

	  r = 9999999 - r;
      }

    if (remainder)
	*remainder = r;
/* Do not replace this by << 32, it gives a compiler warning and it does
       not work. */
    return ((((time_t) a2) << 16) << 16) + (a1 << 16) + a0;

}
