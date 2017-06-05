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
wvGetATRD (ATRD * item, wvStream * fd)
{
    int i;
    for (i = 0; i < 10; i++)
	item->xstUsrInitl[i] = read_16ubit (fd);
    item->ibst = (S16) read_16ubit (fd);
    item->ak = read_16ubit (fd);
    item->grfbmc = read_16ubit (fd);
    item->lTagBkmk = (S32) read_32ubit (fd);
}

int
wvGetATRD_PLCF (ATRD ** atrd, U32 ** pos, U32 * noatrd, U32 offset, U32 len,
		wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *atrd = NULL;
	  *pos = NULL;
	  *noatrd = 0;
      }
    else
      {
	  *noatrd = (len - 4) / (cbATRD + 4);
	  *pos = (U32 *) wvMalloc ((*noatrd + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*noatrd + 1) * sizeof (U32)));
		return (1);
	    }

	  *atrd = (ATRD *) wvMalloc ((*noatrd + 1) * sizeof (ATRD));
	  if (*atrd == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *noatrd * sizeof (ATRD)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i < *noatrd + 1; i++)
	      (*pos)[i] = read_32ubit (fd);
	  for (i = 0; i < *noatrd; i++)
	      wvGetATRD (&((*atrd)[i]), fd);
      }
    return (0);
}

ATRD *
wvGetCommentBounds (U32 * comment_cpFirst, U32 * comment_cpLim, U32 currentcp,
		    ATRD * atrd, U32 * pos, U32 noatrd, STTBF * bookmarks,
		    BKF * bkf, U32 * posBKF, U32 bkf_intervals, BKL * bkl,
		    U32 * posBKL, U32 bkl_intervals)
{
    U32 i, j;
    S32 id;

    for (i = 0; i < noatrd; i++)
      {
	  if (pos[i] > currentcp)
	    {
		/*
		   when not -1, this tag identifies the annotation bookmark that locates the
		   range of CPs in the main document which this annotation references.
		 */
		if ((atrd[i].lTagBkmk != -1) && (bookmarks != NULL) && (bookmarks->nostrings > 0) && (bookmarks->extradata))
		  {
		    for (j = 0; j < bookmarks->nostrings; j++)
			{
			    id =
				(S32) sread_32ubit (bookmarks->extradata[j] +
						    2);
			    if (id == atrd[i].lTagBkmk)
			      {
				  wvTrace (("bingo, index is %d!!\n", j));
				  *comment_cpFirst = posBKF[i];
				  *comment_cpLim = posBKL[bkf[i].ibkl];
				  wvTrace (
					   ("begin end are %d %d\n",
					    *comment_cpFirst, *comment_cpLim));
				  return (&(atrd[i]));
			      }
			}
		  }

		/* in case we find nothing, at least we won't blow up, we create a
		   point comment */
		*comment_cpFirst = pos[i];
		*comment_cpLim = pos[i] + 1;
		return (&(atrd[i]));
	    }
      }

    *comment_cpLim = 0xfffffffeL;
    return (NULL);
}
