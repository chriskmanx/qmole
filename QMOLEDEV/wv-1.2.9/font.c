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
wvGetFONTSIGNATURE (FONTSIGNATURE * fs, wvStream * fd)
{
    int i;
    for (i = 0; i < 4; i++)
	fs->fsUsb[i] = read_32ubit (fd);
    for (i = 0; i < 2; i++)
	fs->fsCsb[i] = read_32ubit (fd);
}

void
wvInitFONTSIGNATURE (FONTSIGNATURE * fs)
{
    int i;
    for (i = 0; i < 4; i++)
	fs->fsUsb[i] = 0;
    for (i = 0; i < 2; i++)
	fs->fsCsb[i] = 0;
}

void
wvGetPANOSE (PANOSE * item, wvStream * fd)
{
    item->bFamilyType = read_8ubit (fd);
    item->bSerifStyle = read_8ubit (fd);
    item->bWeight = read_8ubit (fd);
    item->bProportion = read_8ubit (fd);
    item->bContrast = read_8ubit (fd);
    item->bStrokeVariation = read_8ubit (fd);
    item->bArmStyle = read_8ubit (fd);
    item->bLetterform = read_8ubit (fd);
    item->bMidline = read_8ubit (fd);
    item->bXHeight = read_8ubit (fd);
}

void
wvInitPANOSE (PANOSE * item)
{
    item->bFamilyType = 0;
    item->bSerifStyle = 0;
    item->bWeight = 0;
    item->bProportion = 0;
    item->bContrast = 0;
    item->bStrokeVariation = 0;
    item->bArmStyle = 0;
    item->bLetterform = 0;
    item->bMidline = 0;
    item->bXHeight = 0;
}
