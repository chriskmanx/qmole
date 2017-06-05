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
wvGetPGD (wvVersion ver, PGD * item, wvStream * fd)
{
    U16 temp16;

    temp16 = read_16ubit (fd);
    item->fContinue = temp16 & 0x0001;
    item->fUnk = (temp16 & 0x0002) >> 1;
    item->fRight = (temp16 & 0x0004) >> 2;
    item->fPgnRestart = (temp16 & 0x0008) >> 3;
    item->fEmptyPage = (temp16 & 0x0010) >> 4;
    item->fAllFtn = (temp16 & 0x0020) >> 5;
    item->fColOnly = (temp16 & 0x0040) >> 6;
    item->fTableBreaks = (temp16 & 0x0080) >> 7;
    item->fMarked = (temp16 & 0x0100) >> 8;
    item->fColumnBreaks = (temp16 & 0x0200) >> 9;
    item->fTableHeader = (temp16 & 0x0400) >> 10;
    item->fNewPage = (temp16 & 0x0800) >> 11;
    item->bkc = (temp16 & 0xF000) >> 12;

    item->lnn = read_16ubit (fd);
    item->pgn = read_16ubit (fd);
    if (ver == WORD8)
	item->dym = (S32) read_32ubit (fd);
    else
	item->dym = 0;
}
