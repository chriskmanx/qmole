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
wvGetASUMYI (ASUMYI * asu, wvStream * fd)
{
    U16 temp16 = read_16ubit (fd);

    asu->fValid = temp16 & 0x0001;
    asu->fView = (temp16 & 0x0002) >> 1;
    asu->iViewBy = (temp16 & 0x000C) >> 2;
    asu->fUpdateProps = (temp16 & 0x0010) >> 4;
    asu->reserved = (temp16 & 0xFFE0) >> 5;

    asu->wDlgLevel = read_16ubit (fd);
    asu->lHighestLevel = read_32ubit (fd);
    asu->lCurrentLevel = read_32ubit (fd);
}


void
wvInitASUMYI (ASUMYI * asu)
{
    asu->fValid = 0;
    asu->fView = 0;
    asu->iViewBy = 0;
    asu->fUpdateProps = 0;
    asu->reserved = 0;
    asu->wDlgLevel = 0;
    asu->lHighestLevel = 0;
    asu->lCurrentLevel = 0;
}
