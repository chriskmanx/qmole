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
wvGetBKD (BKD * item, wvStream * fd)
{
    U16 temp16;
    item->ipgd_itxbxs = (S16) read_16ubit (fd);
    item->dcpDepend = (S16) read_16ubit (fd);
    temp16 = read_16ubit (fd);
    item->icol = temp16 & 0x00FF;
    item->fTableBreak = (temp16 & 0x0100) >> 8;
    item->fColumnBreak = (temp16 & 0x0200) >> 9;
    item->fMarked = (temp16 & 0x0400) >> 10;
    item->fUnk = (temp16 & 0x0800) >> 11;
    item->fTextOverflow = (temp16 & 0x1000) >> 12;
    item->reserved1 = (temp16 & 0xE000) >> 13;
}

int
wvGetBKD_PLCF (BKD ** bkd, U32 ** pos, U32 * nobkd, U32 offset, U32 len,
	       wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *bkd = NULL;
	  *pos = NULL;
	  *nobkd = 0;
      }
    else
      {
	  *nobkd = (len - 4) / (cbBKD + 4);
	  *pos = (U32 *) wvMalloc ((*nobkd + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*nobkd + 1) * sizeof (U32)));
		return (1);
	    }

	  *bkd = (BKD *) wvMalloc (*nobkd * sizeof (BKD));
	  if (*bkd == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *nobkd * sizeof (BKD)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i < *nobkd + 1; i++)
	    {
		(*pos)[i] = read_32ubit (fd);
	    }
	  for (i = 0; i < *nobkd; i++)
	    {
		wvGetBKD (&((*bkd)[i]), fd);
	    }
      }
    return (0);
}
