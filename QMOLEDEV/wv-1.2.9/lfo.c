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

/*
Word writes out the pllfo first by writing out a PL of LFO structures. 
It then enumerates through each LFO to figure out how many LFOLVLs each 
one has (LFO.clfolvl), and writes out, in order, each LFOLVL structure 
followed by its corresponding LVL structure (if LFOLVL.fFormatting is set).
*/

static int
multiplication_will_overflow(U32 a, U32 b)
{
  if((a > 0) && (b > 0) && (G_MAXUINT / a) >= b) {
    return 0;
  }

  return 1;
}

int
wvGetLFO_records (LFO ** lfo, LFOLVL ** lfolvl, LVL ** lvl, U32 * nolfo,
		  U32 * nooflvl, U32 offset, U32 len, wvStream * fd)
{
    U32 i;
    U32 end;
    *nooflvl = 0;
    wvTrace (("lfo begins at %x len %d\n", offset, len));
    wvStream_offset_from_end (fd, 0);
    end = wvStream_tell (fd);
    wvGetLFO_PLF (lfo, nolfo, offset, len, fd);

    for (i = 0; i < *nolfo; i++)
	*nooflvl += (*lfo)[i].clfolvl;
    wvTrace (("pos %x %d\n", wvStream_tell (fd), *nooflvl));
    wvTrace (("nolfo is %d nooflvl is %d\n", *nolfo, *nooflvl));

    if ((*nooflvl == 0) ||
	multiplication_will_overflow(sizeof (LFOLVL), *nooflvl) ||
	multiplication_will_overflow(sizeof (LVL), *nooflvl))
      {
	  *lfolvl = NULL;
	  *lvl = NULL;
	  return (0);
      }

    *lfolvl = (LFOLVL *) wvMalloc (sizeof (LFOLVL) * *nooflvl);
    *lvl = (LVL *) wvMalloc (sizeof (LVL) * *nooflvl);

    i = 0;
    while (i < *nooflvl)
      {
	  wvInitLVL (&((*lvl)[i]));
	  wvTrace (("%d pos now %x %d\n", i, wvStream_tell (fd), *nooflvl));
	  if (wvStream_tell (fd) == end)
	    {
		wvWarning
		    ("LFOLVL off the end of the file, continuing anyway\n");
		i++;
		continue;
	    }
	  wvGetLFOLVL (&((*lfolvl)[i]), fd);
#if 0
	  if (wvInvalidLFOLVL (&((*lfolvl)[i])))
	      continue;
#endif
	  if ((*lfolvl)[i].fFormatting)
	    {
		wvTrace (("formatting set\n"));
		wvGetLVL (&((*lvl)[i]), fd);
	    }
	  i++;
      }
    return (0);
}

int
wvGetLFO_PLF (LFO ** lfo, U32 * nolfo, U32 offset, U32 len, wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *lfo = NULL;
	  *nolfo = 0;
      }
    else
      {
	  wvStream_goto (fd, offset);
	  *nolfo = read_32ubit (fd);
	  wvTrace (("%d\n", *nolfo));

	  /* check for integer overflow */
	  if (multiplication_will_overflow(*nolfo, sizeof(LFO))) {
	    wvError (("Malicious document!\n"));			
	    *nolfo = 0;
	    return (1);
	  } else {
	    *lfo = (LFO *) wvMalloc (*nolfo * sizeof(LFO));
	    if (*lfo == NULL)
	      {
		wvError (("NO MEM 1, failed to alloc %d bytes\n",
			  *nolfo * sizeof (LFO)));
		return (1);
	      }
	    for (i = 0; i < *nolfo; i++)
	      wvGetLFO (&((*lfo)[i]), fd);
	  }
      }
    return (0);
}

void
wvGetLFO (LFO * item, wvStream * fd)
{
    int i;
    item->lsid = read_32ubit (fd);
    item->reserved1 = read_32ubit (fd);
    item->reserved2 = read_32ubit (fd);
    item->clfolvl = read_8ubit (fd);
    for (i = 0; i < 3; i++)
	item->reserved3[i] = read_8ubit (fd);
}

void
wvInitLFO (LFO * item)
{
    int i;
    item->lsid = 0;
    item->reserved1 = 0;
    item->reserved2 = 0;
    item->clfolvl = 0;
    for (i = 0; i < 3; i++)
	item->reserved3[i] = 0;
}

void
wvGetLFOLVL (LFOLVL * item, wvStream * fd)
{
    U8 temp8;
#ifdef PURIFY
    wvInitLFOLVL (item);
#endif
    item->iStartAt = read_32ubit (fd);

    while (wvInvalidLFOLVL (item))
      {
	  wvTrace (("pos %x\n", wvStream_tell (fd)));
	  item->iStartAt = read_32ubit (fd);
      }

    temp8 = read_8ubit (fd);
    item->ilvl = temp8 & 0x0F;
    item->fStartAt = (temp8 & 0x10) >> 4;
    item->fFormatting = (temp8 & 0x20) >> 5;
    item->reserved1 = (temp8 & 0xC0) >> 6;
    item->reserved2 = read_8ubit (fd);
    item->reserved3 = read_8ubit (fd);
    item->reserved4 = read_8ubit (fd);
}

void
wvInitLFOLVL (LFOLVL * item)
{
    item->iStartAt = 0;
    item->ilvl = 0;
    item->fStartAt = 0;
    item->fFormatting = 0;
    item->reserved1 = 0;
    item->reserved2 = 0;
    item->reserved3 = 0;
    item->reserved4 = 0;
}

int
wvInvalidLFOLVL (LFOLVL * item)
{

    if (item->iStartAt != 0xffffffff)
	return (0);
#if 0
    /* 
       a bloody russian doc, from Sergey V. Udaltsov <svu@pop.convey.ru> caused 
       the removal of this section 
     */
    if (item->ilvl != 0xf)
	return (0);
    if (item->fStartAt != 1)
	return (0);
    if (item->fFormatting != 1)
	return (0);
    if (item->reserved1 != 0x3)
	return (0);
    if (item->reserved2 != 0xff)
	return (0);
    if (item->reserved3 != 0xff)
	return (0);
    if (item->reserved4 != 0xff)
	return (0);
#endif
    wvWarning (("invalid list entry, trucking along happily anyway\n"));
    return (1);
}

int
wvReleaseLFO_records (LFO ** lfo, LFOLVL ** lfolvl, LVL ** lvl, U32 nooflvl)
{
    U32 i;
    wvTrace (("releasing %d lvl records\n", nooflvl));
    wvFree (*lfo);
    wvFree (*lfolvl);
    for (i = 0; i < nooflvl; i++)
	wvReleaseLVL (&((*lvl)[i]));
    wvFree (*lvl);
    return (0);
}
