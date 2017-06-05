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
wvGetSHD_internal (SHD * item, wvStream * fd, U8 * pointer)
{
    U16 temp16;
#ifdef PURIFY
    wvInitSHD (item);
#endif
    temp16 = dread_16ubit (fd, &pointer);
    item->icoFore = temp16 & 0x001F;
    item->icoBack = (temp16 & 0x03E0) >> 5;
    item->ipat = (temp16 & 0xFC00) >> 10;
}

void
wvGetSHD (SHD * item, wvStream * fd)
{
    wvGetSHD_internal (item, fd, NULL);
}

void
wvGetSHDFromBucket (SHD * item, U8 * pointer)
{
    wvGetSHD_internal (item, NULL, pointer);
}

void
wvInitSHD (SHD * item)
{
    item->icoFore = 0;
    item->icoBack = 0;
    item->ipat = 0;
}

void
wvCopySHD (SHD * dest, SHD * src)
{
    memcpy (dest, src, sizeof (SHD));
}
