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

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include "wv.h"

void
wvGetDOGRID (DOGRID * dog, wvStream * fd)
{
    U16 temp16;
    dog->xaGrid = read_16ubit (fd);
    dog->yaGrid = read_16ubit (fd);
    dog->dxaGrid = read_16ubit (fd);
    dog->dyaGrid = read_16ubit (fd);

    temp16 = read_16ubit (fd);

    dog->dyGridDisplay = temp16 & 0x007F;
    dog->fTurnItOff = (temp16 & 0x0080) >> 7;
    dog->dxGridDisplay = (temp16 & 0x7F00) >> 8;
    dog->fFollowMargins = (temp16 & 0x8000) >> 15;
}

void
wvInitDOGRID (DOGRID * dog)
{
    dog->xaGrid = 0;
    dog->yaGrid = 0;
    dog->dxaGrid = 0;
    dog->dyaGrid = 0;
    dog->dyGridDisplay = 0;
    dog->fTurnItOff = 0;
    dog->dxGridDisplay = 0;
    dog->fFollowMargins = 0;
}
