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
#include "crc32.h"

void
wvGetANLD (wvVersion ver, ANLD * item, wvStream * fd)
{
    U8 temp8;
    int i;
#ifdef PURIFY
    wvInitANLD (item);
#endif
    item->nfc = read_8ubit (fd);
    item->cxchTextBefore = read_8ubit (fd);
    item->cxchTextAfter = read_8ubit (fd);
    temp8 = read_8ubit (fd);
    item->jc = temp8 & 0x03;
    item->fPrev = (temp8 & 0x04) >> 2;
    item->fHang = (temp8 & 0x08) >> 3;
    item->fSetBold = (temp8 & 0x10) >> 4;
    item->fSetItalic = (temp8 & 0x20) >> 5;
    item->fSetSmallCaps = (temp8 & 0x40) >> 6;
    item->fSetCaps = (temp8 & 0x80) >> 7;
    temp8 = read_8ubit (fd);
    item->fSetStrike = temp8 & 0x01;
    item->fSetKul = (temp8 & 0x02) >> 1;
    item->fPrevSpace = (temp8 & 0x04) >> 2;
    item->fBold = (temp8 & 0x08) >> 3;
    item->fItalic = (temp8 & 0x10) >> 4;
    item->fSmallCaps = (temp8 & 0x20) >> 5;
    item->fCaps = (temp8 & 0x40) >> 6;
    item->fStrike = (temp8 & 0x80) >> 7;
    temp8 = read_8ubit (fd);
    item->kul = temp8 & 0x07;
    item->ico = (temp8 & 0xF1) >> 3;
    item->ftc = (S16) read_16ubit (fd);
    item->hps = read_16ubit (fd);
    item->iStartAt = read_16ubit (fd);
    item->dxaIndent = (S16) read_16ubit (fd);
    item->dxaSpace = read_16ubit (fd);
    item->fNumber1 = read_8ubit (fd);
    item->fNumberAcross = read_8ubit (fd);
    item->fRestartHdn = read_8ubit (fd);
    item->fSpareX = read_8ubit (fd);
    for (i = 0; i < 32; i++)
      {
	  if (ver == WORD8)
	      item->rgxch[i] = read_16ubit (fd);
	  else
	      item->rgxch[i] = read_8ubit (fd);
      }

}

void
wvGetANLD_FromBucket (wvVersion ver, ANLD * item, U8 * pointer8)
{
    U8 temp8;
    int i;
#ifdef PURIFY
    wvInitANLD (item);
#endif
    item->nfc = dread_8ubit (NULL, &pointer8);
    item->cxchTextBefore = dread_8ubit (NULL, &pointer8);
    item->cxchTextAfter = dread_8ubit (NULL, &pointer8);
    temp8 = dread_8ubit (NULL, &pointer8);
    item->jc = temp8 & 0x03;
    item->fPrev = (temp8 & 0x04) >> 2;
    item->fHang = (temp8 & 0x08) >> 3;
    item->fSetBold = (temp8 & 0x10) >> 4;
    item->fSetItalic = (temp8 & 0x20) >> 5;
    item->fSetSmallCaps = (temp8 & 0x40) >> 6;
    item->fSetCaps = (temp8 & 0x80) >> 7;
    temp8 = dread_8ubit (NULL, &pointer8);
    item->fSetStrike = temp8 & 0x01;
    item->fSetKul = (temp8 & 0x02) >> 1;
    item->fPrevSpace = (temp8 & 0x04) >> 2;
    item->fBold = (temp8 & 0x08) >> 3;
    item->fItalic = (temp8 & 0x10) >> 4;
    item->fSmallCaps = (temp8 & 0x20) >> 5;
    item->fCaps = (temp8 & 0x40) >> 6;
    item->fStrike = (temp8 & 0x80) >> 7;
    temp8 = dread_8ubit (NULL, &pointer8);
    item->kul = temp8 & 0x07;
    item->ico = (temp8 & 0xF1) >> 3;
    item->ftc = (S16) dread_16ubit (NULL, &pointer8);
    item->hps = dread_16ubit (NULL, &pointer8);
    item->iStartAt = dread_16ubit (NULL, &pointer8);
    item->dxaIndent = (S16) dread_16ubit (NULL, &pointer8);
    item->dxaSpace = dread_16ubit (NULL, &pointer8);
    item->fNumber1 = dread_8ubit (NULL, &pointer8);
#if 0
    if (item->fNumber1 == 46)
	wvTrace (
		 ("This level has not been modified, so you can't believe its nfc\n"));
#endif
    item->fNumberAcross = dread_8ubit (NULL, &pointer8);
    item->fRestartHdn = dread_8ubit (NULL, &pointer8);
    item->fSpareX = dread_8ubit (NULL, &pointer8);
    for (i = 0; i < 32; i++)
      {
	  if (ver == WORD8)
	      item->rgxch[i] = dread_16ubit (NULL, &pointer8);
	  else
	      item->rgxch[i] = dread_8ubit (NULL, &pointer8);
      }
}

void
wvCopyANLD (ANLD * dest, ANLD * src)
{
    memcpy (dest, src, sizeof (ANLD));
}

void
wvInitANLD (ANLD * item)
{
    int i;
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
    item->fNumber1 = 0;
    item->fNumberAcross = 0;
    item->fRestartHdn = 0;
    item->fSpareX = 0;
    for (i = 0; i < 32; i++)
	item->rgxch[i] = 0;
}

U32 wvCheckSumANLD (ANLD * item)
{
    return (CalcCRC32 ((unsigned char *) item, cbANLD, cbANLD, 0));
}
