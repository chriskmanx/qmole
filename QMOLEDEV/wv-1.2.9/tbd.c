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
#include "wvinternal.h"

void
wvInitTBD (TBD * item)
{
    item->jc = 0;
    item->tlc = 0;
    item->reserved = 0;
}

void
wvCopyTBD (TBD * dest, TBD * src)
{
    memcpy (dest, src, sizeof (TBD));
}

void
wvGetTBD (TBD * item, wvStream * fd)
{
    wvGetTBD_internal (item, fd, NULL);
}

void
wvGetTBDFromBucket (TBD * item, U8 * pointer)
{
    wvGetTBD_internal (item, NULL, pointer);
}


void
wvGetTBD_internal (TBD * item, wvStream * fd, U8 * pointer)
{
    U8 temp8;
    temp8 = dread_8ubit (fd, &pointer);
#ifdef PURIFY
    wvInitTBD (item);
#endif
    item->jc = temp8 & 0x07;
    item->tlc = (temp8 & 0x38) >> 3;
    item->reserved = (temp8 & 0xC0) >> 6;
}
