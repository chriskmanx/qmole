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
wvGetBTE (BTE * bte, wvStream * fd)
{
    U32 temp32 = read_32ubit (fd);
#ifdef PURIFY
    wvInitBTE (bte);
#endif
    bte->pn = temp32 & 0x003fffffL;
    bte->unused = (temp32 & 0xffc00000L) >> 22;
}

void
wvInitBTE (BTE * bte)
{
    bte->pn = 0;
    bte->unused = 0;
}

void
wvListBTE_PLCF (BTE ** bte, U32 ** pos, U32 * nobte)
{
    U32 i = 0;
    for (i = 0; i < *nobte; i++)
	wvError (
		 ("range %x %x is pn %d\n", (*pos)[i], (*pos)[i + 1],
		  (*bte)[i].pn));
}

int
wvGetBTE_PLCF6 (BTE ** bte, U32 ** pos, U32 * nobte, U32 offset, U32 len,
		wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *bte = NULL;
	  *pos = NULL;
	  *nobte = 0;
      }
    else
      {
	  wvTrace (("offset is %x, len is %d\n", offset, len));
	  *nobte = (len - 4) / (cb6BTE + 4);
	  wvTrace (("no of bte is %d at %x\n", *nobte, offset));
	  *pos = (U32 *) wvMalloc ((*nobte + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*nobte + 1) * sizeof (U32)));
		return (1);
	    }

	  *bte = (BTE *) wvMalloc (*nobte * sizeof (BTE));
	  if (*bte == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *nobte * sizeof (BTE)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i <= *nobte; i++)
	    {
		(*pos)[i] = read_32ubit (fd);
		wvTrace (("pos is %x\n", (*pos)[i]));
	    }
	  for (i = 0; i < *nobte; i++)
	    {
		wvInitBTE (&((*bte)[i]));
		(*bte)[i].pn = read_16ubit (fd);
	    }
      }
    return (0);
}

int
wvGetBTE_PLCF (BTE ** bte, U32 ** pos, U32 * nobte, U32 offset, U32 len,
	       wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *bte = NULL;
	  *pos = NULL;
	  *nobte = 0;
      }
    else
      {
	  *nobte = (len - 4) / (cbBTE + 4);
	  wvTrace (("no of bte is %d at %x\n", *nobte, offset));
	  *pos = (U32 *) wvMalloc ((*nobte + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*nobte + 1) * sizeof (U32)));
		return (1);
	    }

	  *bte = (BTE *) wvMalloc (*nobte * sizeof (BTE));
	  if (*bte == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *nobte * sizeof (BTE)));
		wvFree (*pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i <= *nobte; i++)
	      (*pos)[i] = read_32ubit (fd);
	  for (i = 0; i < *nobte; i++)
	      wvGetBTE (&((*bte)[i]), fd);
      }
    return (0);
}

void
wvCopyBTE (BTE * dest, BTE * src)
{
    memcpy (dest, src, sizeof (BTE));
}

int
wvGetBTE_FromFC (BTE * bte, U32 currentfc, BTE * list, U32 * fcs, int nobte)
{
    int i = 0;
    while (i < nobte)
      {
	  if ((currentfc >= wvNormFC (fcs[i], NULL))
	      && (currentfc < wvNormFC (fcs[i + 1], NULL)))
	    {
		wvTrace (("valid\n"));
		wvCopyBTE (bte, &list[i]);
		return (0);
	    }
	  i++;
      }
    wvCopyBTE (bte, &list[i - 1]);
    return (0);
    /*
       return(1);
     */
}
