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
#include <string.h>
#include "wv.h"

void
wvGetDTTM (DTTM * item, wvStream * fd)
{
    U16 a = read_16ubit (fd);
    U16 b = read_16ubit (fd);
    wvCreateDTTM (item, a, b);
}

void
wvGetDTTMFromBucket (DTTM * item, U8 * pointer)
{
    U16 a = dread_16ubit (NULL, &pointer);
    U16 b = dread_16ubit (NULL, &pointer);
    wvCreateDTTM (item, a, b);
}


void
wvCreateDTTM (DTTM * dttm, U16 temp1_16, U16 temp2_16)
{
    dttm->mint = temp1_16 & 0x003F;
    dttm->hr = (temp1_16 & 0x07C0) >> 6;
    dttm->dom = (temp1_16 & 0xF800) >> 11;

    dttm->mon = temp2_16 & 0x000F;
    dttm->yr = (temp2_16 & 0x1FF0) >> 4;
    dttm->wdy = (temp2_16 & 0xE000) >> 13;
}


void
wvInitDTTM (DTTM * dttm)
{
    dttm->mint = 0;
    dttm->hr = 0;
    dttm->dom = 0;

    dttm->mon = 0;
    dttm->yr = 0;
    dttm->wdy = 0;
}

void
wvCopyDTTM (DTTM * dest, DTTM * src)
{
    memcpy (dest, src, sizeof (DTTM));
}

void
wvListDTTM (DTTM * src)
{
    wvError (("min is %d\n", src->mint));
    wvError (("hr is %d\n", src->hr));
    wvError (("dom is %d\n", src->dom));
    wvError (("mon is %d\n", src->mon));
    wvError (("yr is %d\n", src->yr));
    wvError (("wdy is %d\n", src->wdy));
}

/* 
 * dont free the char * back from this, its engine is
 * ctime
*/
char *
wvDTTMtoUnix (DTTM * src)
{
    /* 
       im concerned with the lack of yday to i run it
       through the system to fill it in before using asctime
     */
    time_t t;
    struct tm out;
    wvListDTTM (src);
    out.tm_sec = 0;
    out.tm_min = src->mint;
    out.tm_hour = src->hr;
    out.tm_mday = src->dom;
    out.tm_mon = src->mon - 1;
    out.tm_year = src->yr;
    out.tm_wday = src->wdy;
    out.tm_yday = 0;
    out.tm_isdst = -1;
    t = mktime (&out);
    if (t == -1)
      {
	  wvWarning ("Bad Time!!, not critical error\n");
	  return (NULL);
      }
    return (ctime (&t));
}
