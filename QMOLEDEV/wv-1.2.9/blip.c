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
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "wv.h"

U32
twvGetFOPTE (FOPTE * afopte, wvStream * infd)
{
    U32 ret = 0;
    U16 dtemp;
    dtemp = read_16ubit (infd);
#ifdef PURIFY
    afopte->pid = 0;
    afopte->fBid = 0;
    afopte->fComplex = 0;
#endif
    afopte->pid = (dtemp & 0x3fff);
    afopte->fBid = ((dtemp & 0x4000) >> 14);
    afopte->fComplex = ((dtemp & 0x8000) >> 15);
    afopte->op = read_32ubit (infd);

    if (afopte->fComplex)
      {
	  wvTrace (("1 complex len is %d (%x)\n", afopte->op, afopte->op));
	  ret = afopte->op;
      }
#if 0
    else if (afopte->fBid)
	wvTrace (
		 ("great including graphic number %d %d\n", afopte->op,
		  afopte->op));
#endif
    wvTrace (
	     ("orig %x,pid is %x, val is %x\n", dtemp, afopte->pid,
	      afopte->op));
    return (ret);
}

fbse_list *
wvGetSPID (U32 spid, fsp_list * afsp_list, fbse_list * afbse_list)
{
    fopte_list *temp;
    U32 i;

    while (afsp_list != NULL)
      {
	  if (afsp_list->afsp.spid == spid)
	    {
		wvTrace (("found the correct spid\n"));
		temp = afsp_list->afopte_list;
		while (temp != NULL)
		  {
		      if ((temp->afopte.fBid) && (!(temp->afopte.fComplex)))
			{
			    wvTrace (
				     ("found a graphic to go with the spid, no %d\n",
				      temp->afopte.op));
			    for (i = 1; i < temp->afopte.op; i++)
				afbse_list = afbse_list->next;
			    return (afbse_list);
			    break;
			}
		      temp = temp->next;
		  }
		break;
	    }
	  afsp_list = afsp_list->next;
      }

    return (NULL);
}


U32
wvGetSPIDfromCP (U32 cp, textportions * portions)
{
    U32 i;
    for (i = 0; i < portions->noofficedraw; i++)
	if (cp == portions->officedrawcps[i])
	    return (portions->fspas[i].spid);
    return (0xffffffffL);
}

void
wvGetBITMAP (BITMAP * bmp, wvStream * infd)
{
    int i;
    for (i = 0; i < 14; i++)
	bmp->bm[i] = read_8ubit (infd);
}

void
wvGetrc (rc * arc, wvStream * infd)
{
    int i;
    for (i = 0; i < 14; i++)
	arc->bm[i] = read_8ubit (infd);
}


U32
twvGetFBSE (FBSE * afbse, wvStream * infd)
{
    int i;
    afbse->btWin32 = read_8ubit (infd);
    afbse->btMacOS = read_8ubit (infd);
    for (i = 0; i < 16; i++)
	afbse->rgbUid[i] = read_8ubit (infd);
    afbse->tag = read_16ubit (infd);
    afbse->size = read_32ubit (infd);
    afbse->cRef = read_32ubit (infd);
    afbse->foDelay = read_32ubit (infd);
    afbse->usage = read_8ubit (infd);
    afbse->cbName = read_8ubit (infd);
    afbse->unused2 = read_8ubit (infd);
    afbse->unused3 = read_8ubit (infd);
    return (36);
}
