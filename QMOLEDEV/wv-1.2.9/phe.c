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
#include "wv.h"

void
wvGetPHE6 (PHE * dest, U8 * page, U16 * pos)
{
    U8 temp8;

#ifdef PURIFY
    wvInitPHE (dest, 0);
#endif

    temp8 = bread_8ubit (&(page[*pos]), pos);
    dest->var1.fSpare = temp8 & 0x01;
    dest->var1.fUnk = (temp8 & 0x02) >> 1;

    dest->var1.fDiffLines = (temp8 & 0x04) >> 2;
    dest->var1.reserved1 = (temp8 & 0xf8) >> 3;

    dest->var1.clMac = bread_8ubit (&(page[*pos]), pos);

    dest->var1.dxaCol = (S16) bread_16ubit (&(page[*pos]), pos);
    dest->var1.dymHeight = (S32) bread_16ubit (&(page[*pos]), pos);
}

void
wvGetPHE (PHE * dest, int which, U8 * page, U16 * pos)
{
    U8 temp8;
    U32 temp32;

#ifdef PURIFY
    wvInitPHE (dest, which);
#endif

    if (which)
      {
	  temp32 = bread_32ubit (&(page[*pos]), pos);
	  dest->var2.fSpare = temp32 & 0x0001;
	  dest->var2.fUnk = (temp32 & 0x0002) >> 1;
	  dest->var2.dcpTtpNext = (temp32 & 0xfffffffc) >> 2;
	  dest->var2.dxaCol = (S32) bread_32ubit (&(page[*pos]), pos);
	  dest->var2.dymHeight = (S32) bread_32ubit (&(page[*pos]), pos);
      }
    else
      {
	  temp8 = bread_8ubit (&(page[*pos]), pos);
	  dest->var1.fSpare = temp8 & 0x01;
	  dest->var1.fUnk = (temp8 & 0x02) >> 1;

	  dest->var1.fDiffLines = (temp8 & 0x04) >> 2;
	  dest->var1.reserved1 = (temp8 & 0xf8) >> 3;

	  dest->var1.clMac = bread_8ubit (&(page[*pos]), pos);

	  dest->var1.reserved2 = bread_16ubit (&(page[*pos]), pos);

	  dest->var1.dxaCol = (S32) bread_32ubit (&(page[*pos]), pos);
	  dest->var1.dymHeight = (S32) bread_32ubit (&(page[*pos]), pos);
      }
}

void
wvCopyPHE (PHE * dest, PHE * src, int which)
{
    if (which)
      {
	  dest->var2.fSpare = src->var2.fSpare;
	  dest->var2.fUnk = src->var2.fUnk;
	  dest->var2.dcpTtpNext = src->var2.dcpTtpNext;
	  dest->var2.dxaCol = src->var2.dxaCol;
	  dest->var2.dymHeight = src->var2.dymHeight;
      }
    else
      {
	  dest->var1.fSpare = src->var1.fSpare;
	  dest->var1.fUnk = src->var1.fUnk;

	  dest->var1.fDiffLines = src->var1.fDiffLines;
	  dest->var1.reserved1 = src->var1.reserved1;
	  dest->var1.clMac = src->var1.clMac;
	  dest->var1.reserved2 = src->var1.reserved2;

	  dest->var1.dxaCol = src->var1.dxaCol;
	  dest->var1.dymHeight = src->var1.dymHeight;
      }
}

void
wvInitPHE (PHE * item, int which)
{
    if (which)
      {
	  item->var2.fSpare = 0;
	  item->var2.fUnk = 0;
	  item->var2.dcpTtpNext = 0;
	  item->var2.dxaCol = 0;
	  item->var2.dymHeight = 0;
      }
    else
      {
	  item->var1.fSpare = 0;
	  item->var1.fUnk = 0;

	  item->var1.fDiffLines = 0;
	  item->var1.reserved1 = 0;
	  item->var1.clMac = 0;
	  item->var1.reserved2 = 0;

	  item->var1.dxaCol = 0;
	  item->var1.dymHeight = 0;
      }
}
