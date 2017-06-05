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
wvInitFFN (FFN * item)
{
    U8 i;
    item->cbFfnM1 = 0;
    item->prq = 0;
    item->fTrueType = 0;
    item->reserved1 = 0;
    item->ff = 0;
    item->reserved2 = 0;
    item->wWeight = 0;
    item->chs = 0;
    item->ixchSzAlt = 0;
    wvInitPANOSE (&item->panose);
    wvInitFONTSIGNATURE (&item->fs);
    for (i = 0; i < 65; i++)
	item->xszFfn[i] = 0;
}

void
wvGetFFN6 (FFN * item, wvStream * fd)
{
    int len, i;
    U8 temp8;

#ifdef PURIFY
    wvInitFFN (item);
#endif

    item->cbFfnM1 = read_8ubit (fd);
    temp8 = read_8ubit (fd);
    item->prq = temp8 & 0x03;
    item->fTrueType = (temp8 & 0x04) >> 2;
    item->reserved1 = (temp8 & 0x08) >> 3;
    item->ff = (temp8 & 0x70) >> 4;
    item->reserved2 = (temp8 & 0x80) >> 7;
    item->wWeight = (S16) read_16ubit (fd);
    item->chs = read_8ubit (fd);
    item->ixchSzAlt = read_8ubit (fd);
    wvInitPANOSE (&(item->panose));
    wvInitFONTSIGNATURE (&(item->fs));
    len = item->cbFfnM1 - 5;
    if (len > 65)
	len = 65;
    for (i = 0; i < len; i++)
	item->xszFfn[i] = read_8ubit (fd);
}


void
wvGetFFN (FFN * item, wvStream * fd)
{
    int len, i;
    U8 temp8;

#ifdef PURIFY
    wvInitFFN (item);
#endif

    item->cbFfnM1 = read_8ubit (fd);
    temp8 = read_8ubit (fd);
    item->prq = temp8 & 0x03;
    item->fTrueType = (temp8 & 0x04) >> 2;
    item->reserved1 = (temp8 & 0x08) >> 3;
    item->ff = (temp8 & 0x70) >> 4;
    item->reserved2 = (temp8 & 0x80) >> 7;
    item->wWeight = (S16) read_16ubit (fd);
    item->chs = read_8ubit (fd);
    item->ixchSzAlt = read_8ubit (fd);
    wvGetPANOSE (&(item->panose), fd);
    wvGetFONTSIGNATURE (&(item->fs), fd);
    len = item->cbFfnM1 - 39;
    len = len / 2;
    /*
       item->xszFfn = (U16) wvMalloc(sizeof(U16) * len));
     */
    if (len > 65)
	len = 65;
    for (i = 0; i < len; i++)
	item->xszFfn[i] = read_16ubit (fd);
}

void
wvGetFFN_STTBF (FFN_STTBF * item, U32 offset, U32 len, wvStream * fd)
{
    int i;
    wvTrace (("reading fonts...\n"));
    wvTrace (("seeking to %x, len %d\n", offset, len));
    if (len == 0)
      {
	  item->nostrings = 0;
	  item->ffn = NULL;
      }
    else
      {
	  wvStream_goto (fd, offset);
	  item->extendedflag = read_16ubit (fd);
	  if (item->extendedflag == 0xFFFF)
	      item->nostrings = read_16ubit (fd);
	  else
	      item->nostrings = item->extendedflag;
	  item->extradatalen = read_16ubit (fd);
	  item->ffn = (FFN *) wvMalloc (item->nostrings * sizeof (FFN));
	  for (i = 0; i < item->nostrings; i++)
	    {
#ifdef DEBUG
		char *dbg;
#endif
		wvGetFFN (&(item->ffn[i]), fd);
#ifdef DEBUG
		dbg = wvWideStrToMB (item->ffn[i].xszFfn);
		wvTrace (("font %d: %s\n", i, dbg));
		if (dbg)
		    wvFree (dbg);
#endif
	    }
      }

    wvTrace (("done reading fonts.\n"));
}

void
wvGetFFN_STTBF6 (FFN_STTBF * item, U32 offset, U32 len, wvStream * fd)
{
    U32 count = 0;
    int noffn = 0;
    wvTrace (("reading fonts 6...\n"));
    wvTrace (("seeking to %x, len %d\n", offset, len));
    if (len == 0)
      {
	  item->nostrings = 0;
	  item->ffn = NULL;
	  return;
      }
    wvStream_goto (fd, offset);
    item->extendedflag = 0;
    item->nostrings = 5;	/* lets just set a val to start off with */
    item->extradatalen = 0;
    item->ffn = (FFN *) wvMalloc (item->nostrings * sizeof (FFN));
    if (len != read_16ubit (fd))
	wvError (("FFN STTBF lens differ\n"));
    count += 2;

    while (count < len)
      {
#ifdef DEBUG
	  char *dbg;
#endif
	  if (noffn == item->nostrings)
	    {
		/* need to extend the array just in case */
		item->nostrings += 5;
		item->ffn =
		    (FFN *) realloc (item->ffn, item->nostrings * sizeof (FFN));
	    }
	  wvGetFFN6 (&(item->ffn[noffn]), fd);
	  count += (item->ffn[noffn].cbFfnM1 + 1);
#ifdef DEBUG
	  dbg = wvWideStrToMB (item->ffn[noffn].xszFfn);
	  wvTrace (("font %d: %s\n", noffn, dbg));
	  if (dbg)
	      wvFree (dbg);
#endif
	  noffn++;
      }

    if (item->nostrings != noffn)
	item->nostrings = noffn;

    wvTrace (("done reading fonts 6.\n"));
}

void
wvReleaseFFN_STTBF (FFN_STTBF * item)
{
    if (item->ffn != NULL)
	wvFree (item->ffn);
}


/* 
This has to be extended in the future to return a list of possible 
font names, as the FFN spec mentions. It currently on does the first
one.
*/
char *
wvGetFontnameFromCode (FFN_STTBF * item, int fontcode)
{
    if (fontcode >= item->nostrings)
	return (NULL);

    return (wvWideStrToMB (item->ffn[fontcode].xszFfn));
}


#if 0
int
wvGetFRD_PLCF (FRD ** frd, U32 ** pos, int *nofrd, U32 offset, U32 len,
	       wvStream * fd)
{
    int i;
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
	  for (i = 0; i < *nofrd + 1; i++)
	      (*pos)[i] = read_32ubit (fd);
	  for (i = 0; i < *nofrd; i++)
	      wvGetFRD (&((*frd)[i]), fd);
      }
    return (0);
}
#endif
