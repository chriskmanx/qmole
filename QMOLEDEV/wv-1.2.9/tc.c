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

int
wvGetTC_internal (wvVersion ver, TC * tc, wvStream * infd, U8 * pointer)
{
    U16 temp16;
    BRC10 brc10;
    wvTrace (("TC ver %d\n", ver));

#ifdef PURIFY
    wvInitTC (tc);
#endif
    temp16 = dread_16ubit (infd, &pointer);
    wvTrace (("temp16 is %x\n", temp16));

    if (ver == WORD8)
      {
	  tc->fFirstMerged = temp16 & 0x0001;
	  tc->fMerged = (temp16 & 0x0002) >> 1;
	  tc->fVertical = (temp16 & 0x0004) >> 2;
	  tc->fBackward = (temp16 & 0x0008) >> 3;
	  tc->fRotateFont = (temp16 & 0x0010) >> 4;
	  tc->fVertMerge = (temp16 & 0x0020) >> 5;
	  tc->fVertRestart = (temp16 & 0x0040) >> 6;
	  tc->vertAlign = (temp16 & 0x0180) >> 7;
	  tc->fUnused = (temp16 & 0xFE00) >> 9;

	  tc->wUnused = dread_16ubit (infd, &pointer);
	  wvGetBRC_internal (&tc->brcTop, infd, pointer);
	  pointer += cbBRC;
	  wvGetBRC_internal (&tc->brcLeft, infd, pointer);
	  pointer += cbBRC;
	  wvGetBRC_internal (&tc->brcBottom, infd, pointer);
	  pointer += cbBRC;
	  wvGetBRC_internal (&tc->brcRight, infd, pointer);
	  pointer += cbBRC;
      }
    else
      {
	  wvInitTC (tc);
	  tc->fFirstMerged = temp16 & 0x0001;
	  tc->fMerged = (temp16 & 0x0002) >> 1;
	  /* 
	     This + word 6 205 in sprm.c I thought
	     would make sense together, until I get
	     another example I have disabled the two
	     of them
	   */
	  tc->fVertical = (temp16 & 0x0004) >> 2;
	  tc->fBackward = (temp16 & 0x0008) >> 3;
	  tc->fRotateFont = (temp16 & 0x0010) >> 4;
	  tc->fVertMerge = (temp16 & 0x0020) >> 5;
	  tc->fVertRestart = (temp16 & 0x0040) >> 6;
	  tc->vertAlign = (temp16 & 0x0180) >> 7;
	  tc->fUnused = (temp16 & 0xFE00) >> 9;

	  wvGetBRC10_internal (&brc10, infd, pointer);
	  wvConvertBRC10ToBRC (&tc->brcTop, &brc10);
	  pointer += cb6BRC;
	  wvGetBRC10_internal (&brc10, infd, pointer);
	  wvConvertBRC10ToBRC (&tc->brcLeft, &brc10);
	  pointer += cb6BRC;
	  wvGetBRC10_internal (&brc10, infd, pointer);
	  wvConvertBRC10ToBRC (&tc->brcBottom, &brc10);
	  pointer += cb6BRC;
	  wvGetBRC10_internal (&brc10, infd, pointer);
	  wvConvertBRC10ToBRC (&tc->brcRight, &brc10);
	  pointer += cb6BRC;
	  return (cb6TC);
      }
    return (cbTC);
}

int
wvGetTCFromBucket (wvVersion ver, TC * abrc, U8 * pointer)
{
    return (wvGetTC_internal (ver, abrc, NULL, pointer));
}


void
wvCopyTC (TC * dest, TC * src)
{
    dest->fFirstMerged = src->fFirstMerged;
    dest->fMerged = src->fMerged;
    dest->fVertical = src->fVertical;
    dest->fBackward = src->fBackward;
    dest->fRotateFont = src->fRotateFont;
    dest->fVertMerge = src->fVertMerge;
    dest->fVertRestart = src->fVertRestart;
    dest->vertAlign = src->vertAlign;
    dest->fUnused = src->fUnused;
    dest->wUnused = src->wUnused;

    wvCopyBRC (&src->brcTop, &dest->brcTop);
    wvCopyBRC (&src->brcLeft, &dest->brcLeft);
    wvCopyBRC (&src->brcBottom, &dest->brcBottom);
    wvCopyBRC (&src->brcRight, &dest->brcRight);
}

void
wvInitTC (TC * item)
{
    item->fFirstMerged = 0;
    item->fMerged = 0;
    item->fVertical = 0;
    item->fBackward = 0;
    item->fRotateFont = 0;
    item->fVertMerge = 0;
    item->fVertRestart = 0;
    item->vertAlign = 0;
    item->fUnused = 0;
    item->wUnused = 0;

    wvInitBRC (&item->brcTop);
    wvInitBRC (&item->brcLeft);
    wvInitBRC (&item->brcBottom);
    wvInitBRC (&item->brcRight);
}
