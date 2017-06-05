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

#include "wvinternal.h"

static PAPX_FKP wvPAPX_FKP_previous;
static U32 wvPAPX_pn_previous = 0;
static CHPX_FKP wvCHPX_FKP_previous;
static U32 wvCHPX_pn_previous = 0;


void
external_wvReleasePAPX_FKP (void)
{
        if (wvPAPX_pn_previous != 0)
        {
                wvPAPX_pn_previous = 0;
        }
}

void
external_wvReleaseCHPX_FKP (void)
{
        if (wvCHPX_pn_previous != 0)
        {
                wvCHPX_pn_previous = 0;
        }
}

void
internal_wvReleasePAPX_FKP (PAPX_FKP * fkp)
{
    int i;
    wvFree (fkp->rgfc);
    fkp->rgfc = NULL;
    wvFree (fkp->rgbx);
    fkp->rgbx = NULL;
    for (i = 0; i < fkp->crun; i++)
	wvReleasePAPX (&(fkp->grppapx[i]));
    fkp->crun = 0;
    wvFree (fkp->grppapx);
    fkp->grppapx = NULL;
}

/*
At offset
511 is a 1-byte count named crun, which is a count of paragraphs in PAPX 
FKPs. Beginning at offset 0 of the FKP is an array of crun+1 FCs, named 
rgfc, which records the beginning and limit FCs of crun paragraphs.

immediately following the fkp.rgfc is an array of 13 byte
entries called BXs. This array called the rgbx is in 1-to-1 correspondence
with the rgfc. The first byte of the ith BX entry contains a single byte
field which gives the word offset of the PAPX that belongs to the paragraph
whose beginning in FC space is rgfc[i] and whose limit is rgfc[i+1] in FC
space. The last 12 bytes of the ith BX entry contain a PHE structure that
stores the current paragraph height of the paragraph whose beginning in FC
space is rgfc[i] and whose limit is rgfc[i+1] in FC space.
*/

/*
The first byte of each BX is the word offset of the PAPX recorded for
the paragraph corresponding to this BX. .. If the byte stored is 0,
this represents a 1 line paragraph 15 pixels high with Normal style
(stc == 0) whose column width is 7980 dxas. The last 12 bytes of
the BX is a PHE structure which stores the current paragraph height
for the paragraph corresponding to the BX. If a plcfphe has an entry 
that maps to the FC for this paragraph, that entry's PHE overrides the PHE
stored in the FKP.11*fkp.crun+4 unused space. As new runs/paragraphs
are recorded in the FKP, unused space is reduced by 17 if CHPX/PAPX
is already recorded and is reduced by 17+sizeof(PAPX) if property is not
already recorded.
*/

void
wvGetPAPX_FKP (wvVersion ver, PAPX_FKP * fkp, U32 pn, wvStream * fd)
{
    int i;
    U8 page[WV_PAGESIZE];
    U16 pos = 0;
    /*size_t bytes_read; */

    /* brian.ewins@bt.com */
    /* there seem to be a lot of repeat calls... */
    /* pn=0 is safe because thats the index block, not a PAPX_FKP */
    if (pn != 0 && pn == wvPAPX_pn_previous)
      {
	  memcpy (fkp, &wvPAPX_FKP_previous, sizeof (PAPX_FKP));
	  return;
      }

    wvTrace (
	     ("seeking to %x to get crun\n",
	      pn * WV_PAGESIZE + (WV_PAGESIZE - 1)));
    wvStream_goto (fd, pn * WV_PAGESIZE);
    /*bytes_read= */ wvStream_read (page, WV_PAGESIZE, 1, fd);
    fkp->crun = (U8) page[WV_PAGESIZE - 1];
    fkp->rgfc = (U32 *) wvMalloc (sizeof (U32) * (fkp->crun + 1));
    fkp->rgbx = (BX *) wvMalloc (sizeof (BX) * (fkp->crun));
    fkp->grppapx = (PAPX *) wvMalloc (sizeof (PAPX) * (fkp->crun));
    for (i = 0; i < fkp->crun + 1; i++)
      {
	  fkp->rgfc[i] = bread_32ubit (&(page[pos]), &pos);
	  wvTrace (("rgfc is %x\n", fkp->rgfc[i]));
      }

    for (i = 0; i < fkp->crun; i++)
      {
	  if (ver == WORD8)
	      wvGetBX (&fkp->rgbx[i], page, &pos);
	  else
	      wvGetBX6 (&fkp->rgbx[i], page, &pos);
      }

    for (i = 0; i < fkp->crun; i++)
      {
	  if (fkp->rgbx[i].offset == 0)
	    {
		wvTrace (("i is %d, using clear papx\n", i));
		wvInitPAPX (&(fkp->grppapx[i]));
	    }
	  else
	    {
		wvTrace (
			 ("papx index i is %d, offset is %x\n", i,
			  pn * WV_PAGESIZE + fkp->rgbx[i].offset * 2));
		pos = fkp->rgbx[i].offset * 2;
		wvGetPAPX (ver, &(fkp->grppapx[i]), page, &pos);
	    }
      }
    if (wvPAPX_pn_previous != 0)
	internal_wvReleasePAPX_FKP (&wvPAPX_FKP_previous);
    memcpy (&wvPAPX_FKP_previous, fkp, sizeof (PAPX_FKP));
    wvPAPX_pn_previous = pn;
}

/*
Using the FC, search the FCs FKP for the largest FC less than the character's FC,
    call it fcTest.
*/
U32
wvSearchNextLargestFCPAPX_FKP (PAPX_FKP * fkp, U32 currentfc)
{
    U32 i = 0;
    U8 until = fkp->crun + 1;
    U32 fcTest = 0;


    while (i < until)
      {
	  wvTrace (("searching fkp %x %x\n", currentfc, fkp->rgfc[i]));
	  if ((wvNormFC (fkp->rgfc[i], NULL) < currentfc)
	      && (wvNormFC (fkp->rgfc[i], NULL) > fcTest))
	      fcTest = wvNormFC (fkp->rgfc[i], NULL);
	  else if (wvNormFC (fkp->rgfc[i], NULL) == currentfc)
	      fcTest = currentfc + 1;
	  i++;
      }

    /*for the first paragraph return the current pos as the beginning */
    /*
       if (fcTest == 0)
       fcTest = currentfc+1;
     */

    return (fcTest);
}

U32
wvSearchNextLargestFCCHPX_FKP (CHPX_FKP * fkp, U32 currentfc)
{
    U32 i = 0;
    U8 until = fkp->crun + 1;
    U32 fcTest = 0;


    while (i < until)
      {
	  wvTrace (("searching fkp %x %x\n", currentfc, fkp->rgfc[i]));
	  if ((wvNormFC (fkp->rgfc[i], NULL) <= currentfc)
	      && (wvNormFC (fkp->rgfc[i], NULL) > fcTest))
	      fcTest = wvNormFC (fkp->rgfc[i], NULL);
	  i++;
      }

    /*for the first paragraph return the current pos as the beginning */
    /*
       if (fcTest == 0)
       fcTest = currentfc+1;
     */

    return (fcTest);
}

/*
Using the FC of the character, first search the FKP that describes the
character to find the smallest FC in the rgfc that is larger than the character
FC.
*/
U32
wvSearchNextSmallestFCPAPX_FKP (PAPX_FKP * fkp, U32 currentfc)
{
    U32 i = 0;
    U32 fcTest = 0xffffffffL;
    U8 until = fkp->crun + 1;

    while (i < until)
      {
	  wvTrace (
		   ("Smallest %x, %x %x\n", currentfc,
		    wvNormFC (fkp->rgfc[i], NULL), wvNormFC (fkp->rgfc[i],
							     NULL)));
	  if ((wvNormFC (fkp->rgfc[i], NULL) > currentfc)
	      && (wvNormFC (fkp->rgfc[i], NULL) < fcTest))
	      fcTest = wvNormFC (fkp->rgfc[i], NULL);
	  i++;
      }
    return (fcTest);
}

void
wvReleasePAPX_FKP (PAPX_FKP * fkp)
{
    return;
}


void
wvInitPAPX_FKP (PAPX_FKP * fkp)
{
    fkp->rgfc = NULL;
    fkp->rgbx = NULL;
    fkp->crun = 0;
    fkp->grppapx = NULL;
}

int
wvGetIndexFCInFKP_PAPX (PAPX_FKP * fkp, U32 currentfc)
{
    U32 i = 1;			/*was 0, there is something slightly out of sync in the system */
    U8 until = fkp->crun + 1;

    while (i < until)
      {
	  wvTrace (
		   ("current fc is %x, %x, %x\n", currentfc,
		    wvNormFC (fkp->rgfc[i], NULL), fkp->rgfc[i]));
	  if (wvNormFC (fkp->rgfc[i], NULL) == currentfc)
	      return (i);
	  i++;
      }
    /*
       basically read 
       Algorithm to determine paragraph properties for a paragraph &
       Formatted Disk Page for PAPXs, somehow the currentfc sent in was wrong
       or my understanding is !
     */
    wvTrace (("Shite, fix me %x %x\n", currentfc, fkp->rgfc[0]));
    /*return 1 to make things continue on their merry way */
    return (1);
}

void
internal_wvReleaseCHPX_FKP (CHPX_FKP * fkp)
{
    int i;
    wvTrace (("chpx fkp b freeed\n"));
    wvFree (fkp->rgfc);
    fkp->rgfc = NULL;
    wvFree (fkp->rgb);
    fkp->rgb = NULL;
    for (i = 0; i < fkp->crun; i++)
	wvReleaseCHPX (&(fkp->grpchpx[i]));
    fkp->crun = 0;
    wvFree (fkp->grpchpx);
    fkp->grpchpx = NULL;
    wvTrace (("chpx fkp e freeed\n"));
}


/* Character properties 
 * -basically just like PAPX FKPs above
 * however, rather than an array of BX structs in rgbx,
 * there is an array of bytes (giving the word offset to the CHPX) in rgb
 * -JB
 */
void
wvGetCHPX_FKP (wvVersion ver, CHPX_FKP * fkp, U32 pn, wvStream * fd)
{
    int i;
    U8 page[WV_PAGESIZE];
    U16 pos = 0;
    /*size_t bytes_read; */

    /* brian.ewins@bt.com */
    /* there seem to be a lot of repeat calls... */
    /* pn=0 is safe because thats the index block, not a CHPX_FKP */
    if (pn != 0 && pn == wvCHPX_pn_previous)
      {
	  memcpy (fkp, &wvCHPX_FKP_previous, sizeof (CHPX_FKP));
	  return;
      }
    wvStream_goto (fd, pn * WV_PAGESIZE);
    /*bytes_read= */ wvStream_read (page, WV_PAGESIZE, 1, fd);
    fkp->crun = (U8) page[WV_PAGESIZE - 1];
    wvTrace (("chpx fkp gone to %x\n", pn * WV_PAGESIZE + (WV_PAGESIZE - 1)));
    wvTrace (("crun is %d\n", fkp->crun));
    fkp->rgfc = (U32 *) wvMalloc (sizeof (U32) * (fkp->crun + 1));
    fkp->rgb = (U8 *) wvMalloc (sizeof (U8) * (fkp->crun));
    fkp->grpchpx = (CHPX *) wvMalloc (sizeof (CHPX) * (fkp->crun));
    wvStream_goto (fd, pn * WV_PAGESIZE);
    wvTrace (("offset is %x\n", pn * WV_PAGESIZE));
    for (i = 0; i < fkp->crun + 1; i++)
      {
	  fkp->rgfc[i] = bread_32ubit (&(page[pos]), &pos);
	  wvTrace (("rgfc is %x\n", fkp->rgfc[i]));
      }

    for (i = 0; i < fkp->crun; i++)
	fkp->rgb[i] = bread_8ubit (&(page[pos]), &pos);

    for (i = 0; i < fkp->crun; i++)
      {
	  if (fkp->rgb[i] == 0)
	    {
		wvTrace (("i is %d, using clear chpx\n", i));
		wvInitCHPX (&(fkp->grpchpx[i]));
	    }
	  else
	    {
		wvTrace (
			 ("chpx index i is %d, offset is %x\n", i,
			  (pn * WV_PAGESIZE) + (fkp->rgb[i] * 2)));
		pos = fkp->rgb[i] * 2;
		wvGetCHPX (ver, &(fkp->grpchpx[i]), page, &pos);
	    }
      }
    if (wvCHPX_pn_previous != 0)
	internal_wvReleaseCHPX_FKP (&wvCHPX_FKP_previous);
    memcpy (&wvCHPX_FKP_previous, fkp, sizeof (CHPX_FKP));
    wvCHPX_pn_previous = pn;
}

void
wvReleaseCHPX_FKP (CHPX_FKP * fkp)
{
    return;
}


void
wvInitCHPX_FKP (CHPX_FKP * fkp)
{
    fkp->rgfc = NULL;
    fkp->rgb = NULL;
    fkp->crun = 0;
    fkp->grpchpx = NULL;
}
