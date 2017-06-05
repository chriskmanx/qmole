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
wvGetPCD (PCD * item, wvStream * fd)
{
    U8 temp8;
    temp8 = read_8ubit (fd);
#ifdef PURIFY
    wvInitPCD (item);
#endif
    item->fNoParaLast = temp8 & 0x01;
    item->fPaphNil = (temp8 & 0x02) >> 1;
    item->fCopied = (temp8 & 0x04) >> 2;
    item->reserved = (temp8 & 0xf8) >> 3;
    item->fn = read_8ubit (fd);
    item->fc = read_32ubit (fd);
    wvGetPRM (&item->prm, fd);
}

void
wvInitPCD (PCD * item)
{
    item->fNoParaLast = 0;
    item->fPaphNil = 0;
    item->fCopied = 0;
    item->reserved = 0;
    item->fn = 0;
    item->fc = 0;
    wvInitPRM (&item->prm);
}


int
wvReleasePCD_PLCF (PCD * pcd, U32 * pos)
{
    wvFree (pcd);
    wvFree (pos);
    return (0);
}


int
wvGetPCD_PLCF (PCD ** pcd, U32 ** pos, U32 * nopcd, U32 offset, U32 len,
	       wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *pcd = NULL;
	  *pos = NULL;
	  *nopcd = 0;
      }
    else
      {
	  *nopcd = (len - 4) / (cbPCD + 4);
	  *pos = (U32 *) wvMalloc ((*nopcd + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*nopcd + 1) * sizeof (U32)));
		return (1);
	    }

	  *pcd = (PCD *) wvMalloc (*nopcd * sizeof (PCD));
	  if (*pcd == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *nopcd * sizeof (PCD)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i <= *nopcd; i++)
	    {
		(*pos)[i] = read_32ubit (fd);
		wvTrace (("pcd pos is %x\n", (*pos)[i]));
	    }
	  for (i = 0; i < *nopcd; i++)
	    {
		wvGetPCD (&((*pcd)[i]), fd);
		wvTrace (
			 ("pcd fc is %x, complex is %d, index is %d\n",
			  (*pcd)[i].fc, (*pcd)[i].prm.fComplex,
			  (*pcd)[i].prm.para.var2.igrpprl));
	    }
      }
    return (0);
}
