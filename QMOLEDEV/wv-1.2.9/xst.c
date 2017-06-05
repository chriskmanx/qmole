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
wvFreeXst (Xst ** xst)
{
    Xst *freegroup;
    if ((xst == NULL) || (*xst == NULL))
	return;

    while (*xst != NULL)
      {
	  freegroup = *xst;
	  *xst = (*xst)->next;
	  if (freegroup->u16string != NULL)
	      wvFree (freegroup->u16string);
	  wvFree (freegroup);
      }
}

void
wvGetXst (Xst ** xst, U32 offset, U32 len, wvStream * fd)
{
    U16 clen, i;
    U32 count = 0;
    Xst *authorlist;
    Xst *current = NULL;

    if ((len == 0) || (xst == NULL))
      {
	  *xst = NULL;
	  return;
      }

    wvStream_goto (fd, offset);
    *xst = (Xst *) wvMalloc (sizeof (Xst));
    authorlist = *xst;

    if (authorlist == NULL)
      {
	  wvError (("not enough mem for annotation group\n"));
	  return;
      }

    authorlist->next = NULL;
    authorlist->u16string = NULL;
    authorlist->noofstrings = 0;
    current = authorlist;

    while (count < len)
      {
	  clen = read_16ubit (fd);
	  count += 2;
	  current->u16string = (U16 *) wvMalloc ((clen + 1) * sizeof (U16));
	  authorlist->noofstrings++;
	  if (current->u16string == NULL)
	    {
		wvError (
			 ("not enough mem for author string of clen %d\n",
			  clen));
		break;
	    }
	  for (i = 0; i < clen; i++)
	    {
		current->u16string[i] = read_16ubit (fd);
		count += 2;
	    }
	  current->u16string[i] = '\0';

	  if (count < len)
	    {
		current->next = (Xst *) wvMalloc (sizeof (Xst));
		if (current->next == NULL)
		  {
		      wvError (("not enough mem for annotation group\n"));
		      break;
		  }
		current = current->next;
		current->next = NULL;
		current->u16string = NULL;
	    }
      }
}
