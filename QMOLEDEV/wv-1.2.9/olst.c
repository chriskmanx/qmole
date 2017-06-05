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
wvInitOLST (OLST * item)
{
    U8 i;
    for (i = 0; i < 9; i++)
	wvInitANLV (&item->rganlv[i]);
    item->fRestartHdr = 0;
    item->fSpareOlst2 = 0;
    item->fSpareOlst3 = 0;
    item->fSpareOlst4 = 0;
    for (i = 0; i < 64; i++)
	item->rgxch[i] = 0;
}


void
wvGetOLST_internal (wvVersion ver, OLST * item, wvStream * fd, U8 * pointer)
{
    U8 i;
    for (i = 0; i < 9; i++)
	wvGetANLV_internal (&item->rganlv[i], fd, pointer);
    item->fRestartHdr = dread_8ubit (fd, &pointer);
    item->fSpareOlst2 = dread_8ubit (fd, &pointer);
    item->fSpareOlst3 = dread_8ubit (fd, &pointer);
    item->fSpareOlst4 = dread_8ubit (fd, &pointer);
    if (ver == WORD8)
      {
	  for (i = 0; i < 32; i++)
	      item->rgxch[i] = dread_16ubit (fd, &pointer);
      }
    else
      {
	  for (i = 0; i < 64; i++)
	      item->rgxch[i] = dread_8ubit (fd, &pointer);
      }
}

void
wvGetOLST (wvVersion ver, OLST * item, wvStream * fd)
{
    wvGetOLST_internal (ver, item, fd, NULL);
}

void
wvGetOLSTFromBucket (wvVersion ver, OLST * item, U8 * pointer)
{
    wvGetOLST_internal (ver, item, NULL, pointer);
}
