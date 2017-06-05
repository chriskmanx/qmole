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

/*
The NUMRM structure is used to track revision marking data for paragraph
numbers, and is stored in the PAP for each numbered paragraph. When revision
marking tracking is turned on, we fill out the NUMRM for each number with
the data required to recreate the number's text. Then at display time, that
string is compared with the current paragraph number string, and displayed
as changed (old deleted, current inserted) if the strings differ. The string
construction algorithm is the same as for an LVL structure.
*/

void
wvGetNUMRM_internal (NUMRM * item, wvStream * fd, U8 * pointer)
{
    int i;
    item->fNumRM = dread_8ubit (fd, &pointer);
    item->Spare1 = dread_8ubit (fd, &pointer);
    item->ibstNumRM = (S16) dread_16ubit (fd, &pointer);
    if (fd != NULL)
	wvGetDTTM (&(item->dttmNumRM), fd);
    else
      {
	  wvGetDTTMFromBucket (&(item->dttmNumRM), pointer);
	  pointer += cbDTTM;
      }
    for (i = 0; i < 9; i++)
	item->rgbxchNums[i] = dread_8ubit (fd, &pointer);
    for (i = 0; i < 9; i++)
	item->rgnfc[i] = dread_8ubit (fd, &pointer);
    item->Spare2 = (S16) dread_16ubit (fd, &pointer);
    for (i = 0; i < 9; i++)
	item->PNBR[i] = (S32) dread_32ubit (fd, &pointer);
    for (i = 0; i < 32; i++)
	item->xst[i] = dread_16ubit (fd, &pointer);
}

void
wvGetNUMRM (NUMRM * item, wvStream * fd)
{
    wvGetNUMRM_internal (item, fd, NULL);
}

void
wvGetNUMRMFromBucket (NUMRM * item, U8 * pointer)
{
    wvGetNUMRM_internal (item, NULL, pointer);
}

void
wvCopyNUMRM (NUMRM * dest, NUMRM * src)
{
    memcpy (dest, src, sizeof (NUMRM));
}

void
wvInitNUMRM (NUMRM * item)
{
    int i;
    item->fNumRM = 0;
    item->Spare1 = 0;
    item->ibstNumRM = 0;
    wvInitDTTM (&(item->dttmNumRM));
    for (i = 0; i < 9; i++)
	item->rgbxchNums[i] = 0;
    for (i = 0; i < 9; i++)
	item->rgnfc[i] = 0;
    item->Spare2 = 0;
    for (i = 0; i < 9; i++)
	item->PNBR[i] = 0;
    for (i = 0; i < 32; i++)
	item->xst[i] = 0;
}
