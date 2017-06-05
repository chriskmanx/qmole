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
wvListFTXBXS (FTXBXS * item)
{
    wvError (("%d\n", item->cTxbx_iNextReuse));
    wvError (("%d\n", item->cReusable));
    wvError (("%d\n", item->fReusable));
    wvError (("%d\n", item->reserved));
    wvError (("%d\n", item->lid));
    wvError (("%d\n", item->txidUndo));
}


void
wvGetFTXBXS (FTXBXS * item, wvStream * fd)
{
    item->cTxbx_iNextReuse = (S32) read_32ubit (fd);
    item->cReusable = (S32) read_32ubit (fd);
    item->fReusable = (S16) read_16ubit (fd);
    item->reserved = (S32) read_32ubit (fd);
    item->lid = (S32) read_32ubit (fd);
    item->txidUndo = (S32) read_32ubit (fd);
    /* wvListFTXBXS (item); */
}

int
wvGetFTXBXS_PLCF (FTXBXS ** ftxbxs, U32 ** pos, U32 * noftxbxs, U32 offset,
		  U32 len, wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *ftxbxs = NULL;
	  *pos = NULL;
	  *noftxbxs = 0;
      }
    else
      {
	  *noftxbxs = (len - 4) / (cbFTXBXS + 4);
	  *pos = (U32 *) wvMalloc ((*noftxbxs + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*noftxbxs + 1) * sizeof (U32)));
		return (1);
	    }

	  *ftxbxs = (FTXBXS *) wvMalloc ((*noftxbxs + 1) * sizeof (FTXBXS));
	  if (*ftxbxs == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *noftxbxs * sizeof (FTXBXS)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i < *noftxbxs + 1; i++)
	      (*pos)[i] = read_32ubit (fd);
	  for (i = 0; i < *noftxbxs; i++)
	      wvGetFTXBXS (&((*ftxbxs)[i]), fd);
      }
    return (0);
}
