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
wvInitANLV (ANLV * item)
{
    item->nfc = 0;
    item->cxchTextBefore = 0;
    item->cxchTextAfter = 0;
    item->jc = 0;
    item->fPrev = 0;
    item->fHang = 0;
    item->fSetBold = 0;
    item->fSetItalic = 0;
    item->fSetSmallCaps = 0;
    item->fSetCaps = 0;
    item->fSetStrike = 0;
    item->fSetKul = 0;
    item->fPrevSpace = 0;
    item->fBold = 0;
    item->fItalic = 0;
    item->fSmallCaps = 0;
    item->fCaps = 0;
    item->fStrike = 0;
    item->kul = 0;
    item->ico = 0;
    item->ftc = 0;
    item->hps = 0;
    item->iStartAt = 0;
    item->dxaIndent = 0;
    item->dxaSpace = 0;
}

void
wvGetANLV_internal (ANLV * item, wvStream * fd, U8 * pointer)
{
    U8 temp8;
    item->nfc = dread_8ubit (fd, &pointer);
    item->cxchTextBefore = dread_8ubit (fd, &pointer);
    item->cxchTextAfter = dread_8ubit (fd, &pointer);
    temp8 = dread_8ubit (fd, &pointer);
    item->jc = temp8 & 0x03;
    item->fPrev = (temp8 & 0x04) >> 2;
    item->fHang = (temp8 & 0x08) >> 3;
    item->fSetBold = (temp8 & 0x10) >> 4;
    item->fSetItalic = (temp8 & 0x20) >> 5;
    item->fSetSmallCaps = (temp8 & 0x40) >> 6;
    item->fSetCaps = (temp8 & 0x80) >> 7;
    temp8 = dread_8ubit (fd, &pointer);
    item->fSetStrike = temp8 & 0x01;
    item->fSetKul = (temp8 & 0x02) >> 1;
    item->fPrevSpace = (temp8 & 0x04) >> 2;
    item->fBold = (temp8 & 0x08) >> 3;
    item->fItalic = (temp8 & 0x10) >> 4;
    item->fSmallCaps = (temp8 & 0x20) >> 5;
    item->fCaps = (temp8 & 0x40) >> 6;
    item->fStrike = (temp8 & 0x80) >> 7;
    temp8 = dread_8ubit (fd, &pointer);
    item->kul = temp8 & 0x07;
    item->ico = (temp8 & 0xF1) >> 3;
    item->ftc = (S16) dread_16ubit (fd, &pointer);
    item->hps = dread_16ubit (fd, &pointer);
    item->iStartAt = dread_16ubit (fd, &pointer);
    item->dxaIndent = dread_16ubit (fd, &pointer);
    item->dxaSpace = (S16) dread_16ubit (fd, &pointer);
}


void
wvGetANLV (ANLV * item, wvStream * fd)
{
    wvGetANLV_internal (item, fd, NULL);
}

void
wvGetANLVFromBucket (ANLV * item, U8 * pointer)
{
    wvGetANLV_internal (item, NULL, pointer);
}
