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


float
wvRelativeWidth (S16 width, SEP * asep)
{
    float fwidth;
    if (asep == NULL)
	return (100.00);
    fwidth = (float) (asep->xaPage - asep->dxaLeft - asep->dxaRight);
    fwidth = width / fwidth * 100;
    if (fwidth > 100.00)
	fwidth = 100.00;
    return (fwidth);
}

float
wvRelativeHeight (S16 height, SEP * asep)
{
    float fheight;
    if (asep == NULL)
	return (100.00);
    fheight = (float) (asep->yaPage - asep->dyaTop - asep->dyaBottom);
    fheight = height / fheight * 100;
    if (fheight > 100.00)
	fheight = 100.00;
    return (fheight);
}

/*
The standard SEP is all zeros except as follows:
 bkc           2 (new page)
 dyaPgn        720 twips (equivalent to .5 in)
 dxaPgn        720 twips
 fEndnote      1 (True)
 fEvenlySpaced 1 (True)
 xaPage        12240 twips
 yaPage        15840 twips
 xaPageNUp     12240 twips
 yaPageNUp     15840 twips
 dyaHdrTop     720 twips
 dyaHdrBottom  720 twips
 dmOrientPage  1 (portrait orientation)
 dxaColumns    720 twips
 dyaTop        1440 twips
 dxaLeft       1800 twips
 dyaBottom     1440 twips
 dxaRight      1800 twips
 pgnStart      1
cbSEP (count of bytes of SEP) is 704(decimal), 2C0(hex).
*/

void
wvInitSEP (SEP * item)
{
    U8 i;
    item->bkc = 2;
    item->fTitlePage = 0;
    item->fAutoPgn = 0;
    item->nfcPgn = 0;
    item->fUnlocked = 0;
    item->cnsPgn = 0;
    item->fPgnRestart = 0;
    item->fEndNote = 1;
    item->lnc = 0;
    item->grpfIhdt = 0;
    item->nLnnMod = 0;
    item->dxaLnn = 0;
    item->dxaPgn = 720;
    item->dyaPgn = 720;
    item->fLBetween = 0;
    item->vjc = 0;
    item->dmBinFirst = 0;
    item->dmBinOther = 0;
    item->dmPaperReq = 0;

    wvInitBRC (&item->brcTop);
    wvInitBRC (&item->brcLeft);
    wvInitBRC (&item->brcBottom);
    wvInitBRC (&item->brcRight);

    item->fPropRMark = 0;
    item->ibstPropRMark = 0;

    wvInitDTTM (&item->dttmPropRMark);

    item->dxtCharSpace = 0;
    item->dyaLinePitch = 0;
    item->clm = 0;
    item->reserved1 = 0;
    item->dmOrientPage = 0;
    item->iHeadingPgn = 0;
    item->pgnStart = 1;
    item->lnnMin = 0;
    item->wTextFlow = 0;
    item->reserved2 = 0;
    item->pgbProp = 0;
    item->pgbApplyTo = 0;
    item->pgbPageDepth = 0;
    item->pgbOffsetFrom = 0;
    item->reserved = 0;
    item->xaPage = 12240;
    item->yaPage = 15840;
    item->xaPageNUp = 12240;
    item->yaPageNUp = 15840;
    item->dxaLeft = 1800;
    item->dxaRight = 1800;
    item->dyaTop = 1440;
    item->dyaBottom = 1440;
    item->dzaGutter = 0;
    item->dyaHdrTop = 720;
    item->dyaHdrBottom = 720;
    item->ccolM1 = 0;
    item->fEvenlySpaced = 1;
    item->reserved3 = 0;
    item->dxaColumns = 720;
    for (i = 0; i < 89; i++)
	item->rgdxaColumnWidthSpacing[i] = 0;
    item->dxaColumnWidth = 0;
    item->dmOrientFirst = 0;
    item->fLayout = 0;
    item->reserved4 = 0;
    wvInitOLST (&item->olstAnm);
	item->fBidi = 0;
}

void
wvGetSEPX (wvVersion ver, SEPX * item, wvStream * fd)
{
    U16 i;
    item->cb = read_16ubit (fd);

    if (item->cb)
	item->grpprl = (U8 *) wvMalloc (item->cb);
    else
	item->grpprl = NULL;

    for (i = 0; i < item->cb; i++)
      {
	  item->grpprl[i] = read_8ubit (fd);
	  wvTrace (("sep is %x\n", item->grpprl[i]));
      }
}

void
wvReleaseSEPX (SEPX * item)
{
    wvFree (item->grpprl);
}


int
wvAddSEPXFromBucket (SEP * asep, SEPX * item, STSH * stsh)
{
    U8 *pointer;
    U16 i = 0;
    U16 sprm;
    int ret = 0;
    Sprm RetSprm;
#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < item->cb)
      {
	  fprintf (stderr, "%x (%d) ", *(item->grpprl + i),
		   *(item->grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif
    while (i < item->cb - 2)
      {
	  sprm = bread_16ubit (item->grpprl + i, &i);
	  pointer = item->grpprl + i;
	  RetSprm =
	      wvApplySprmFromBucket (WORD8, sprm, NULL, NULL, asep, stsh,
				     pointer, &i, NULL);
	  if (RetSprm.sgc == sgcSep)
	      ret = 1;
      }
    return (ret);
}

int
wvAddSEPXFromBucket6 (SEP * asep, SEPX * item, STSH * stsh)
{
    U8 *pointer;
    U16 i = 0;
    int ret = 0;
    U8 sprm8;
    U16 sprm;
    Sprm RetSprm;
#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < item->cb)
      {
	  fprintf (stderr, "%x (%d) ", *(item->grpprl + i),
		   *(item->grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif
    while (i < item->cb)
      {
	  sprm8 = bread_8ubit (item->grpprl + i, &i);
#ifdef SPRMTEST
	  wvError (("sep word 6 sprm is %x (%d)\n", sprm8, sprm8));
#endif
	  sprm = (U16) wvGetrgsprmWord6 (sprm8);
#ifdef SPRMTEST
	  wvTrace (("sep word 6 sprm is converted to %x\n", sprm));
#endif
	  pointer = item->grpprl + i;
	  RetSprm =
	      wvApplySprmFromBucket (WORD6, sprm, NULL, NULL, asep, stsh,
				     pointer, &i, NULL);
	  if (RetSprm.sgc == sgcSep)
	      ret = 1;
      }
    return (ret);
}
