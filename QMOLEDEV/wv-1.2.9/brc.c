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
#include "wvinternal.h"

void
wvGetBRC_internal (BRC * abrc, wvStream * infd, U8 * pointer)
{
    U8 temp8;

#ifdef PURIFY
    wvInitBRC (abrc);
#endif

    abrc->dptLineWidth = dread_8ubit (infd, &pointer);
    abrc->brcType = dread_8ubit (infd, &pointer);
    abrc->ico = dread_8ubit (infd, &pointer);
    temp8 = dread_8ubit (infd, &pointer);
    abrc->dptSpace = temp8 & 0x1f;
    abrc->fShadow = (temp8 & 0x20) >> 5;
    abrc->fFrame = (temp8 & 0x40) >> 6;
    abrc->reserved = (temp8 & 0x80) >> 7;
}

void
wvGetBRC (wvVersion ver, BRC * abrc, wvStream * infd)
{
    if (ver == WORD8)
	wvGetBRC_internal (abrc, infd, NULL);
    else
	wvGetBRC_internal6 (abrc, infd, NULL);
}

void
wvGetBRC_internal6 (BRC * abrc, wvStream * infd, U8 * pointer)
{
    U16 temp16;

#ifdef PURIFY
    wvInitBRC (abrc);
#endif

    temp16 = dread_16ubit (infd, &pointer);

    abrc->dptLineWidth = (temp16 & 0x0007);
    abrc->brcType = (temp16 & 0x0018) >> 3;
    abrc->fShadow = (temp16 & 0x0020) >> 5;
    abrc->ico = (temp16 & 0x07C0) >> 6;
    abrc->dptSpace = (temp16 & 0xF800) >> 11;
}


int
wvGetBRCFromBucket (wvVersion ver, BRC * abrc, U8 * pointer)
{
    if (ver == WORD8)
	wvGetBRC_internal (abrc, NULL, pointer);
    else
      {
	  wvGetBRC_internal6 (abrc, NULL, pointer);
	  return (cb6BRC);
      }
    return (cbBRC);
}

void
wvInitBRC10 (BRC10 * item)
{
    item->dxpLine2Width = 0;
    item->dxpSpaceBetween = 0;
    item->dxpLine1Width = 0;
    item->dxpSpace = 0;
    item->fShadow = 0;
    item->fSpare = 0;
}

void
wvGetBRC10_internal (BRC10 * item, wvStream * infd, U8 * pointer)
{
    U16 temp16;
    temp16 = dread_16ubit (infd, &pointer);
#ifdef PURIFY
    wvInitBRC10 (item);
#endif
    item->dxpLine2Width = (temp16 & 0x0007);
    item->dxpSpaceBetween = (temp16 & 0x0038) >> 3;
    item->dxpLine1Width = (temp16 & 0x01C0) >> 6;
    item->dxpSpace = (temp16 & 0x3E00) >> 9;
    item->fShadow = (temp16 & 0x4000) >> 14;
    item->fSpare = (temp16 & 0x8000) >> 15;
}

int
wvGetBRC10FromBucket (BRC10 * abrc10, U8 * pointer)
{
    wvGetBRC10_internal (abrc10, NULL, pointer);
    return (cbBRC10);
}

void
wvInitBRC (BRC * abrc)
{
    abrc->dptLineWidth = 0;
    abrc->brcType = 0;
    abrc->ico = 0;
    abrc->dptSpace = 0;
    abrc->fShadow = 0;
    abrc->fFrame = 0;
    abrc->reserved = 0;
}

int
wvEqualBRC (BRC * a, BRC * b)
{
    if (a->dptLineWidth == b->dptLineWidth)
	if (a->brcType == b->brcType)
	    if (a->ico == b->ico)
		if (a->dptSpace == b->dptSpace)
		    if (a->fShadow == b->fShadow)
			if (a->fFrame == b->fFrame)
			    if (a->reserved == b->reserved)
				return (1);
    return (0);
}

void
wvCopyBRC (BRC * dest, BRC * src)
{
    dest->dptLineWidth = src->dptLineWidth;
    dest->brcType = src->brcType;
    dest->ico = src->ico;
    dest->dptSpace = src->dptSpace;
    dest->fShadow = src->fShadow;
    dest->fFrame = src->fFrame;
    dest->reserved = src->reserved;
}

/* 
I'm not certain as to how this should work, but it will probably
never occur, its in here for the sake of completeness
*/
void
wvConvertBRC10ToBRC (BRC * item, BRC10 * in)
{
    wvInitBRC (item);
    item->dptSpace = in->dxpSpace;
    item->fShadow = in->fShadow;
    /*
       The border lines and their brc10 settings follow:

       line type        dxpLine1Width               dxpSpaceBetween dxpLine2Width

       no border        0                           0               0

       single line      1                           0               0
       border

       two single line  1                           1               1
       border

       fat solid border 4                           0               0

       thick solid      2                           0               0
       border

       dotted border    6 (special value meaning    0               0
       dotted line)

       hairline border  7(special value meaning     0               0
       hairline)
     */
    if ((in->dxpLine1Width == 0) && (in->dxpSpaceBetween == 0)
	&& (in->dxpLine2Width == 0))
	item->brcType = 0;
    else if ((in->dxpLine1Width == 1) && (in->dxpSpaceBetween == 0)
	     && (in->dxpLine2Width == 0))
	item->brcType = 1;
    else if ((in->dxpLine1Width == 1) && (in->dxpSpaceBetween == 1)
	     && (in->dxpLine2Width == 1))
	item->brcType = 3;
    else if ((in->dxpLine1Width == 4) && (in->dxpSpaceBetween == 0)
	     && (in->dxpLine2Width == 0))
	item->brcType = 3;
    else if ((in->dxpLine1Width == 2) && (in->dxpSpaceBetween == 0)
	     && (in->dxpLine2Width == 0))
	item->brcType = 2;
    else if ((in->dxpLine1Width == 6) && (in->dxpSpaceBetween == 0)
	     && (in->dxpLine2Width == 0))
	item->brcType = 6;
    else if ((in->dxpLine1Width == 7) && (in->dxpSpaceBetween == 0)
	     && (in->dxpLine2Width == 0))
	item->brcType = 5;
    else
	item->brcType = 0;
}
