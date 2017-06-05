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
wvGetDCS_internal (DCS * item, wvStream * fd, U8 * pointer)
{
    U16 temp16;
    temp16 = dread_16ubit (fd, &pointer);
    item->fdct = temp16 & 0x0007;
    item->count = (temp16 & 0x00F8) >> 3;
    item->reserved = (temp16 & 0xff00) >> 8;
}

void
wvGetDCS (DCS * item, wvStream * fd)
{
    wvGetDCS_internal (item, fd, NULL);
}

void
wvGetDCSFromBucket (DCS * item, U8 * pointer)
{
    wvGetDCS_internal (item, NULL, pointer);
}

void
wvCopyDCS (DCS * dest, DCS * src)
{
    memcpy (dest, src, sizeof (DCS));
}

void
wvInitDCS (DCS * item)
{
    item->fdct = 0;
    item->count = 0;
    item->reserved = 0;
}
