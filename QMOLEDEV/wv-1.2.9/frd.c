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
wvGetFRD (FRD * item, wvStream * fd)
{
    item->frd = (S16) read_16ubit (fd);
}

int
wvGetFRD_PLCF (FRD ** frd, U32 ** pos, U32 * nofrd, U32 offset, U32 len,
	       wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *frd = NULL;
	  *pos = NULL;
	  *nofrd = 0;
      }
    else
      {
	  *nofrd = (len - 4) / 6;
	  *pos = (U32 *) wvMalloc ((*nofrd + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*nofrd + 1) * sizeof (U32)));
		return (1);
	    }

	  *frd = (FRD *) wvMalloc (*nofrd * sizeof (FRD));
	  if (*frd == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *nofrd * sizeof (FRD)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i <= *nofrd; i++)
	      (*pos)[i] = read_32ubit (fd);
	  for (i = 0; i < *nofrd; i++)
	      wvGetFRD (&((*frd)[i]), fd);
      }
    return (0);
}
