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

#include <stdio.h>
#include <stdlib.h>
#include "wv.h"
#include "wvinternal.h"

U32
wvGetFAnchor (FAnchor * item, wvStream * fd)
{
    /* It is supposed to be a RECT, but its only 4 long so... */
    item->left = read_8ubit (fd);
    item->right = read_8ubit (fd);
    item->top = read_8ubit (fd);
    item->bottom = read_8ubit (fd);
    return (4);
}

void
wvInitFOPTEArray (FOPTE ** fopte)
{
    *fopte = NULL;
}

void
wvReleaseFOPTEArray (FOPTE ** fopte)
{
    U32 i = 0;
    if (*fopte)
      {
	  while ((*fopte)[i].pid != 0)
	    {
		wvFree ((*fopte)[i].entry);
		i++;
	    }
	  wvFree (*fopte);
      }
}

U32
wvGetFOPTEArray (FOPTE ** fopte, MSOFBH * msofbh, wvStream * fd)
{
    U32 i, j, count = 0;
    U32 no = msofbh->cbLength / 6;
    *fopte = (FOPTE *) wvMalloc (sizeof (FOPTE) * no);
    no = 0;
    while (count < msofbh->cbLength)
      {
	  wvTrace (
		   ("count %x %x, pos %x\n", count, msofbh->cbLength,
		    wvStream_tell (fd)));
	  count += wvGetFOPTE (&(*fopte)[no], fd);
	  no++;
      }
    *fopte = (FOPTE *) realloc (*fopte, sizeof (FOPTE) * (no + 1));
    for (i = 0; i < no; i++)
      {
	  if ((*fopte)[i].fComplex)
	      for (j = 0; j < (*fopte)[i].op; j++)
		  (*fopte)[i].entry[j] = read_8ubit (fd);
      }
    (*fopte)[i].pid = 0;
    wvTrace (("returning %x\n", count));
    return (count);
}


void
wvReleaseFOPTE (FOPTE * afopte)
{
    wvFree (afopte->entry);
}

U32
wvGetFOPTE (FOPTE * afopte, wvStream * fd)
{
    U16 dtemp;
    wvTrace (("pos is %x\n", wvStream_tell (fd)));
    dtemp = read_16ubit (fd);
    wvTrace (("dtemp is %x\n", dtemp));
#ifdef PURIFY
    afopte->pid = 0;
    afopte->fBid = 0;
    afopte->fComplex = 0;
#endif
    afopte->pid = (dtemp & 0x3fff);
    afopte->entry = 0;
    afopte->fBid = ((dtemp & 0x4000) >> 14);
    afopte->fComplex = ((dtemp & 0x8000) >> 15);
    afopte->op = read_32ubit (fd);

    wvTrace (
	     ("orig %x,pid is %x %d, val is %x\n", dtemp, afopte->pid,
	      afopte->pid, afopte->op));
    if (afopte->fComplex)
      {
	  wvTrace (("1 complex len is %d (%x)\n", afopte->op, afopte->op));
	  afopte->entry = (U8 *) wvMalloc (afopte->op);
	  return (afopte->op + 6);
      }

    afopte->entry = NULL;
    return (6);
}

void
wvPutFAnchor (FAnchor * item, wvStream * fd)
{
    /* It is supposed to be a RECT, but its only 4 long so... */
    write_8ubit (fd, (U8) item->left);
    write_8ubit (fd, (U8) item->right);
    write_8ubit (fd, (U8) item->top);
    write_8ubit (fd, (U8) item->bottom);
}

void
wvPutFOPTE (FOPTE * afopte, wvStream * fd)
{
    U16 dtemp = (U16) 0;

    dtemp |= afopte->pid;
    dtemp |= afopte->fBid << 14;
    dtemp |= afopte->fComplex << 15;

    write_16ubit (fd, dtemp);
    write_32ubit (fd, afopte->op);
}

/* TODO: check to make sure that this is correct */

void
wvPutFOPTEArray (FOPTE ** fopte, MSOFBH * msofbh, wvStream * fd)
{
    U32 i, j, count = 0;
    U32 no = 0;

    while (count < msofbh->cbLength)
      {
    /*lvm007@aha.ru fix*/
	  count += 6;
	  wvPutFOPTE (&(*fopte)[no], fd);
	  no++;
      }

    for (i = 0; i < no; i++)
      {
	  if ((*fopte)[i].fComplex)
	      for (j = 0; j < (*fopte)[i].op; j++)
		  write_8ubit (fd, (*fopte)[i].entry[j]);
      }
}
