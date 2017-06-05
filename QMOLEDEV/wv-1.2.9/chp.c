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
#include "bintree.h"

/*
 * To apply a UPX.chpx to a UPE.chp, apply the UPX.chpx.grpprl to
 * UPE.chp. Note that a UPE.chp for a paragraph style should always have
 * UPE.chp.istd == istdNormalChar.
 */
void
wvAddCHPXFromBucket (CHP * achp, UPXF * upxf, STSH * stsh)
{
    U8 *pointer;
    U16 i = 0;
    U16 sprm;

#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < upxf->cbUPX)
      {
	  fprintf (stderr, "%x (%d) ", *(upxf->upx.chpx.grpprl + i),
		   *(upxf->upx.chpx.grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif
    while (i + 2 < upxf->cbUPX) /* is this check sufficient ?? */
      {
	  sprm = bread_16ubit (upxf->upx.chpx.grpprl + i, &i);
#ifdef SPRMTEST
	  wvError (("sprm is %x, i is %d\n", sprm, i));
#endif
	  pointer = upxf->upx.chpx.grpprl + i;
	  wvApplySprmFromBucket (WORD8, sprm, NULL, achp, NULL, stsh, pointer,
				 &i, NULL);
      }
}

void
wvApplyCHPXFromBucket (CHP * achp, CHPX * chpx, STSH * stsh)
{
    U8 *pointer;
    U16 i = 0;
    U16 sprm;
#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < chpx->cbGrpprl)
      {
	  fprintf (stderr, "%x (%d) ", *(chpx->grpprl + i),
		   *(chpx->grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif
    while (i < chpx->cbGrpprl)
      {
	  sprm = bread_16ubit (chpx->grpprl + i, &i);
	  wvTrace (("the sprm is %d\n", sprm));
	  pointer = chpx->grpprl + i;
	  wvApplySprmFromBucket (WORD8, sprm, NULL, achp, NULL, stsh, pointer,
				 &i, NULL);
      }
    achp->istd = chpx->istd;
}

void
wvAddCHPXFromBucket6 (CHP * achp, UPXF * upxf, STSH * stsh)
{
    U8 *pointer;
    U16 i = 0;
    U8 sprm8;
    U16 sprm;
    wvTrace (("cbUPX word 6 is %d\n", upxf->cbUPX));

#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < upxf->cbUPX)
      {
	  fprintf (stderr, "%x (%d) ", *(upxf->upx.chpx.grpprl + i),
		   *(upxf->upx.chpx.grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif
    while (i < upxf->cbUPX)
      {
	  sprm8 = bread_8ubit (upxf->upx.chpx.grpprl + i, &i);
#ifdef SPRMTEST
	  wvError (("chp word 6 sprm is %x (%d)\n", sprm8, sprm8));
#endif
	  sprm = (U16) wvGetrgsprmWord6 (sprm8);
#ifdef SPRMTEST
	  wvError (("chp word 6 sprm is converted to %x\n", sprm));
#endif

	  pointer = upxf->upx.chpx.grpprl + i;
	  wvApplySprmFromBucket (WORD6, sprm, NULL, achp, NULL, stsh, pointer,
				 &i, NULL);
      }
}


void
wvInitCHPFromIstd (CHP * achp, U16 istdBase, STSH * stsh)
{
    wvTrace (("initing from %d\n", istdBase));
    if (istdBase == istdNil) {
 	wvInitCHP (achp);

	/* Set the Nil style's fonts from the defaults. */
	achp->ftcAscii = stsh->Stshi.rgftcStandardChpStsh[0];
	achp->ftcFE = stsh->Stshi.rgftcStandardChpStsh[1];
	achp->ftcOther = stsh->Stshi.rgftcStandardChpStsh[2];
    }
    else
      {
	  if (istdBase >= stsh->Stshi.cstd)
	    {
		wvError (
			 ("ISTD out of bounds, requested %d of %d\n",
			  istdBase, stsh->Stshi.cstd));
		wvInitCHP (achp);	/*it can't hurt to try and start with a blank istd */
		return;
	    }
	  else
	    {
		if (stsh->std[istdBase].cupx == 0)	/*empty slot in the array, i don't think this should happen */
		  {
		      wvTrace (("Empty style slot used (chp)\n"));
		      wvInitCHP (achp);
		  }
		else
		  {
		      wvTrace (("type is %d\n", stsh->std[istdBase].sgc));
		      switch (stsh->std[istdBase].sgc)
			{
			case sgcPara:
			    wvCopyCHP (achp,
				       &(stsh->std[istdBase].grupe[1].achp));
			    break;
			case sgcChp:
			    wvInitCHP (achp);
			    wvApplyCHPXFromBucket (achp,
						   &(stsh->std[istdBase].
						     grupe[0].chpx), stsh);
				strncpy(achp->stylename,stsh->std[istdBase].xstzName, sizeof(achp->stylename));
			    break;
			}
		  }
	    }
      }

}



/*
The standard CHP is all zeros except:

hps               20 half-points
fcPic             -1
istd              10 (the standard character style)
lidDefault, lidFE 0x0400 (no proofing)
wCharScale        100
fUsePgsuSettings  -1
*/

void
wvInitCHP (CHP * item)
{
    int i;

    item->fBold = 0;
    item->fItalic = 0;
    item->fRMarkDel = 0;
    item->fOutline = 0;
    item->fFldVanish = 0;
    item->fSmallCaps = 0;
    item->fCaps = 0;
    item->fVanish = 0;
    item->fRMark = 0;
    item->fSpec = 0;
    item->fStrike = 0;
    item->fObj = 0;
    item->fShadow = 0;
    item->fLowerCase = 0;
    item->fData = 0;
    item->fOle2 = 0;
    item->fEmboss = 0;
    item->fImprint = 0;
    item->fDStrike = 0;
    item->fUsePgsuSettings = -1;     /*-1 ? */
    item->reserved1 = 0;
    item->reserved2 = 0;
    item->reserved11 = 0;
    item->ftc = 0;
    item->ftcAscii = 0;
    item->ftcFE = 0;
    item->ftcOther = 0;
    item->hps = 20;
    item->dxaSpace = 0;
    item->iss = 0;
    item->kul = 0;
    item->fSpecSymbol = 0;
    item->ico = 0;
    item->reserved3 = 0;
    item->fSysVanish = 0;
    item->hpsPos = 0;
    item->super_sub = 0;
    item->lid = 0;
    item->lidDefault = 0x0400;
    item->lidFE = 0x0400;
    item->idct = 0;
    item->idctHint = 0;
    item->wCharScale = 100;
    item->fcPic_fcObj_lTagObj = -1;
    item->ibstRMark = 0;
    item->ibstRMarkDel = 0;

    wvInitDTTM (&item->dttmRMark);
    wvInitDTTM (&item->dttmRMarkDel);

    item->reserved4 = 0;
    item->istd = istdNormalChar;
    item->ftcSym = 0;
    item->xchSym = 0;
    item->idslRMReason = 0;
    item->idslReasonDel = 0;
    item->ysr = 0;
    item->chYsr = 0;
    item->cpg = 0;
    item->hpsKern = 0;
    item->icoHighlight = 0;
    item->fHighlight = 0;
    item->kcd = 0;
    item->fNavHighlight = 0;
    item->fChsDiff = 0;
    item->fMacChs = 0;
    item->fFtcAsciSym = 0;
    item->reserved5 = 0;
    item->fPropRMark = 0;
    item->ibstPropRMark = 0;

    wvInitDTTM (&item->dttmPropRMark);

    item->sfxtText = 0;
    item->reserved6 = 0;
    item->reserved7 = 0;
    item->reserved8 = 0;
    item->reserved9 = 0;

    wvInitDTTM (&item->reserved10);

    item->fDispFldRMark = 0;
    item->ibstDispFldRMark = 0;

    wvInitDTTM (&item->dttmDispFldRMark);

    for (i = 0; i < 16; i++)
	item->xstDispFldRMark[i] = 0;

    wvInitSHD (&item->shd);

    wvInitBRC (&item->brc);

    /* bidi */
    item->fBidi = 0;
    item->fBoldBidi = 0;
    item->fItalicBidi = 0;
    item->ftcBidi = 0;
    item->hpsBidi = 0;
    item->icoBidi = 0;
    item->lidBidi = 0;

	item->stylename[0] = 0;
}

void
wvCopyCHP (CHP * dest, CHP * src)
{
    int i;

    dest->fBold = src->fBold;
    dest->fItalic = src->fItalic;
    dest->fRMarkDel = src->fRMarkDel;
    dest->fOutline = src->fOutline;
    dest->fFldVanish = src->fFldVanish;
    dest->fSmallCaps = src->fSmallCaps;
    dest->fCaps = src->fCaps;
    dest->fVanish = src->fVanish;
    dest->fRMark = src->fRMark;
    dest->fSpec = src->fSpec;
    dest->fStrike = src->fStrike;
    dest->fObj = src->fObj;
    dest->fShadow = src->fShadow;
    dest->fLowerCase = src->fLowerCase;
    dest->fData = src->fData;
    dest->fOle2 = src->fOle2;
    dest->fEmboss = src->fEmboss;
    dest->fImprint = src->fImprint;
    dest->fDStrike = src->fDStrike;
    dest->fUsePgsuSettings = src->fUsePgsuSettings;
    dest->reserved1 = src->reserved1;
    dest->reserved2 = src->reserved2;
    dest->reserved11 = src->reserved11;
    dest->ftc = src->ftc;
    dest->ftcAscii = src->ftcAscii;
    dest->ftcFE = src->ftcFE;
    dest->ftcOther = src->ftcOther;
    dest->hps = src->hps;
    dest->dxaSpace = src->dxaSpace;
    dest->iss = src->iss;
    dest->kul = src->kul;
    dest->fSpecSymbol = src->fSpecSymbol;
    dest->ico = src->ico;
    dest->reserved3 = src->reserved3;
    dest->fSysVanish = src->fSysVanish;
    dest->hpsPos = src->hpsPos;
    dest->super_sub = src->super_sub;
    dest->lid = src->lid;
    dest->lidDefault = src->lidDefault;
    dest->lidFE = src->lidFE;
    dest->idct = src->idct;
    dest->idctHint = src->idctHint;
    dest->wCharScale = src->wCharScale;
    dest->fcPic_fcObj_lTagObj = src->fcPic_fcObj_lTagObj;
    dest->ibstRMark = src->ibstRMark;
    dest->ibstRMarkDel = src->ibstRMarkDel;

    wvCopyDTTM (&dest->dttmRMark, &src->dttmRMark);
    wvCopyDTTM (&dest->dttmRMarkDel, &src->dttmRMarkDel);

    dest->reserved4 = src->reserved4;
    dest->istd = src->istd;
    dest->ftcSym = src->ftcSym;
    dest->xchSym = src->xchSym;
    dest->idslRMReason = src->idslRMReason;
    dest->idslReasonDel = src->idslReasonDel;
    dest->ysr = src->ysr;
    dest->chYsr = src->chYsr;
    dest->cpg = src->cpg;
    dest->hpsKern = src->hpsKern;
    dest->icoHighlight = src->icoHighlight;
    dest->fHighlight = src->fHighlight;
    dest->kcd = src->kcd;
    dest->fNavHighlight = src->fNavHighlight;
    dest->fChsDiff = src->fChsDiff;
    dest->fMacChs = src->fMacChs;
    dest->fFtcAsciSym = src->fFtcAsciSym;
    dest->reserved5 = src->reserved5;
    dest->fPropRMark = src->fPropRMark;
    dest->ibstPropRMark = src->ibstPropRMark;

    wvCopyDTTM (&dest->dttmPropRMark, &src->dttmPropRMark);

    dest->sfxtText = src->sfxtText;
    dest->reserved6 = src->reserved6;
    dest->reserved7 = src->reserved7;
    dest->reserved8 = src->reserved8;
    dest->reserved9 = src->reserved9;

    wvCopyDTTM (&dest->reserved10, &src->reserved10);

    dest->fDispFldRMark = src->fDispFldRMark;
    dest->ibstDispFldRMark = src->ibstDispFldRMark;

    wvCopyDTTM (&dest->dttmDispFldRMark, &src->dttmDispFldRMark);

    for (i = 0; i < 16; i++)
	dest->xstDispFldRMark[i] = src->xstDispFldRMark[i];

    wvCopySHD (&dest->shd, &src->shd);

    wvCopyBRC (&dest->brc, &src->brc);

    /* bidi */
    dest->fBidi = src->fBidi;
    dest->fBoldBidi = src->fBoldBidi;
    dest->fItalicBidi = src->fItalicBidi;
    dest->ftcBidi = src->ftcBidi;
    dest->hpsBidi = src->hpsBidi;
    dest->icoBidi = src->icoBidi;
    dest->lidBidi = src->lidBidi;

	strcpy(dest->stylename,src->stylename);
}

/*
 * The chpx for the null style has an istd of zero, a cbGrpprl of zero 
 * (and an empty grpprl) this only exists in the UPD/UPE
 */
void
wvInitCHPX (CHPX * item)
{
    item->istd = 0;
    item->cbGrpprl = 0;
    item->grpprl = NULL;
}

/*
 * For a character style, the UPE.chpx can be constructed by starting with 
 * the first UPE from the based-on style (std.istdBase).
*/
void
wvInitCHPXFromIstd (CHPX * chpx, U16 istdBase, STSH * stsh)
{
    if (istdBase == istdNil)
	wvInitCHPX (chpx);
    else
      {
	  if (istdBase >= stsh->Stshi.cstd)
	    {
		wvError (
			 ("ISTD out of bounds, requested %d of %d\n",
			  istdBase, stsh->Stshi.cstd));
		wvInitCHPX (chpx);	/*it can't hurt to try and start with a blank istd */
		return;
	    }
	  else
	      wvCopyCHPX (chpx, &(stsh->std[istdBase].grupe[0].chpx));
      }
}

void
wvCopyCHPX (CHPX * dest, CHPX * src)
{
    int i;
    dest->istd = src->istd;
    dest->cbGrpprl = src->cbGrpprl;
    if (dest->cbGrpprl)
	dest->grpprl = (U8 *) wvMalloc (dest->cbGrpprl);
    else
	dest->grpprl = NULL;
    if (dest->grpprl == NULL || src->grpprl == NULL)
	return;
    for (i = 0; i < dest->cbGrpprl; i++)
	dest->grpprl[i] = src->grpprl[i];
}

void
wvReleaseCHPX (CHPX * item)
{
    wvFree (item->grpprl);
}

int
wvCompLT (void *a, void *b)
{
    U8 *a2, *b2;
    U16 sprm1, sprm2;
    a2 = (U8 *) a;
    b2 = (U8 *) b;
    sprm1 = sread_16ubit (a2);
    sprm2 = sread_16ubit (b2);
    return (sprm1 < sprm2);
}

int
wvCompEQ (void *a, void *b)
{
    U8 *a2, *b2;
    U16 sprm1, sprm2;
    a2 = (U8 *) a;
    b2 = (U8 *) b;
    sprm1 = sread_16ubit (a2);
    sprm2 = sread_16ubit (b2);
    return (sprm1 == sprm2);
}


/*
Apply the first UPX (UPX.chpx) in std.grupx to the UPE. 

To apply a UPX.chpx to a UPE.chpx, take the grpprl in UPE.chpx.grpprl (which 
has a length of UPE.chpx.cbGrpprl) and merge the grpprl in UPX.chpx.grpprl 
into it. 

Merging grpprls is a tricky business, but for character styles it is easy 
because no prls in character style grpprls should interact with each other. 
Each prl from the source (the UPX.chpx.grpprl) should be inserted into the 
destination (the UPE.chpx.grpprl) so that the sprm of each prl is in increasing 
order, and any prls that have the same sprm are replaced by the prl in the 
source. 

UPE.chpx.cbGrpprl is then set to the length of resulting grpprl, and 
UPE.chpx.istd is set to the style's istd.
*/
void
wvMergeCHPXFromBucket (CHPX * dest, UPXF * src)
{
    BintreeInfo tree;
    Node *testn, *testp;
    U16 i = 0, j;
    U16 sprm;
    U8 len = 0;
    U8 temp;
    Node *test = NULL;

    U8 *pointer, *dpointer;
    U8 *grpprl = NULL;

    /*
       use a binary tree ala the wmf stuff and first insert every dest sprm into it,
       then insert every src sprm into it, take the full count and take them out of
       the tree and create the list from them
     */
    InitBintree (&tree, wvCompLT, wvCompEQ);
    pointer = dest->grpprl;

    while (i < dest->cbGrpprl)
      {
	  wvTrace (("gotcha the sprm is %x\n", *((U16 *) pointer)));
	  test = InsertNode (&tree, (void *) pointer);
	  sprm = dread_16ubit (NULL, &pointer);
	  wvTrace (("the sprm is %x\n", sprm));
	  temp = wvEatSprm (sprm, pointer, &i);
	  pointer += temp;
	  i += 2;
	  if (test)
	      len += temp + 2;
      }

    i = 0;
    pointer = src->upx.chpx.grpprl;
    i = 0;
    while (i < src->cbUPX)
      {
	  /*wvTrace(("gotcha 2 the sprm is %x\n",*((U16 *)pointer))); */
	  test = InsertNode (&tree, (void *) pointer);
    if(!pointer)
      break;
	  sprm = dread_16ubit (NULL, &pointer);
	  i += 2;
	  wvTrace (("the sprm is %x\n", sprm));
	  temp = wvEatSprm (sprm, pointer, &i);
	  wvTrace (("len of op is %d\n", temp));
	  pointer += temp;
	  wvTrace (("p dis is %d\n", pointer - src->upx.chpx.grpprl));
	  if (test)
	      len += temp + 2;
      }

    if (len != 0)
	grpprl = (U8 *) wvMalloc (len);
    else
	return;


    dpointer = grpprl;

    testn = NextNode (&tree, NULL);
    while (testn != NULL)
      {
	  pointer = (U8 *) testn->Data;
	  sprm = sread_16ubit (pointer);
	  wvTrace (("methinks the sprm is %x\n", sprm));
	  pointer += 2;

	  i = 0;
	  wvEatSprm (sprm, pointer, &i);
	  wvTrace (("i is now %d\n", i));

	  pointer = (U8 *) testn->Data;
	  for (j = 0; j < i + 2; j++)
	      *dpointer++ = *pointer++;

	  testp = NextNode (&tree, testn);
	  wvDeleteNode (&tree, testn);
	  testn = testp;
      }
    wvFree (dest->grpprl);
    dest->grpprl = grpprl;
    dest->cbGrpprl = len;

    /*test */
    i = 0;
    pointer = dest->grpprl;
    while (i < dest->cbGrpprl)
      {
	  sprm = dread_16ubit (NULL, &pointer);
	  wvTrace (("final test the sprm is %x\n", sprm));
	  temp = wvEatSprm (sprm, pointer, &i);
	  pointer += temp;
	  i += 2;
	  if (test)
	      len += temp + 2;
      }
}


void
wvUpdateCHPXBucket (UPXF * src)
{
    U16 i = 0, j;
    U16 sprm;
    U8 sprm8;
    U16 len = 0;
    int temp;

    U8 *pointer, *dpointer;
    U8 *grpprl = NULL;

    i = 0;
    if (src->cbUPX == 0)
	return;
    pointer = src->upx.chpx.grpprl;
    wvTrace (("Msrc->cbUPX len is %d\n", src->cbUPX));
    for (i = 0; i < src->cbUPX; i++)
	wvTrace (("%x\n", src->upx.chpx.grpprl[i]));
    wvTrace (("Mend\n"));
    i = 0;
    len = 0;
    while (i < src->cbUPX)
      {
	  sprm8 = dread_8ubit (NULL, &pointer);
	  wvTrace (("Mpre the sprm is %x\n", sprm8));
	  sprm = (U16) wvGetrgsprmWord6 (sprm8);
	  wvTrace (("Mpost the sprm is %x\n", sprm));
	  i++;
	  len += 2;
	  temp = wvEatSprm (sprm, pointer, &i);
	  wvTrace (("Mlen of op is %d\n", temp));
	  pointer += temp;
	  wvTrace (("Mp dis is %d\n", pointer - src->upx.chpx.grpprl));
	  len += temp;
      }
    wvTrace (("Mlen ends up as %d\n", len));

    if (len == 0)
	return;

    grpprl = (U8 *) wvMalloc (len);

    dpointer = grpprl;

    i = 0;
    pointer = src->upx.chpx.grpprl;
    while (i < src->cbUPX)
      {
	  sprm8 = dread_8ubit (NULL, &pointer);
	  sprm = (U16) wvGetrgsprmWord6 (sprm8);
	  i++;
	  *dpointer++ = (sprm & 0x00FF);
	  *dpointer++ = (sprm & 0xff00) >> 8;
	  temp = wvEatSprm (sprm, pointer, &i);
	  for (j = 0; j < temp; j++)
	      *dpointer++ = *pointer++;
	  wvTrace (("Mlen of op is %d\n", temp));
      }
    wvFree (src->upx.chpx.grpprl);
    src->upx.chpx.grpprl = grpprl;
    src->cbUPX = len;
    for (i = 0; i < src->cbUPX; i++)
	wvTrace (("%x\n", src->upx.chpx.grpprl[i]));
}

/*
 * taken from wvAssembleSimplePAP in pap.c and modified
 * to handle CHP's
 * -JB
 */

int
wvAssembleSimpleCHP (wvVersion ver, CHP * achp, const PAP * apap, U32 fc, CHPX_FKP * fkp,
		     STSH * stsh)
{
    CHPX *chpx;
    int index;
    UPXF upxf;
    int ret = 0;
    U16 tistd;

	
    /* initialize CHP to para's stylesheet character properties this
       * should have resolved all the other stylesheet dependencies
       * for us, when the stsh's were initialized. */

	wvInitCHPFromIstd (achp, apap->istd, stsh);
	/* having done this, we want to set the achp->istd to nil, since
    any char istd value stored in the para style is not applicable
    (its a para style */
	achp->istd = istdNil;
	tistd = istdNil;
	
 apply_chpx:
	/* get CHPX */
	if(fkp)
	{
		/* the PAPX version of the function only looks at rgfc's, which are
		 * the same for CHPX and PAPX FKPs, so we'll reuse the function */
		index = wvGetIndexFCInFKP_PAPX ((PAPX_FKP *) fkp, fc);

		wvTrace (("index is %d, using %d\n", index, index - 1));

		chpx = &(fkp->grpchpx[index - 1]);

		/* apply CHPX from FKP */
		if ((chpx) && (chpx->cbGrpprl > 0))
		{
			ret = 1;
			/* for (i = 0; i < chpx->cbGrpprl; i++) */
			upxf.cbUPX = chpx->cbGrpprl;
			upxf.upx.chpx.grpprl = chpx->grpprl;
			if (ver == WORD8)
				wvAddCHPXFromBucket (achp, &upxf, stsh);
			else
				wvAddCHPXFromBucket6 (achp, &upxf, stsh);
		}

		if(achp->istd < stsh->Stshi.cstd) {
		  if (0 != stsh->std[achp->istd].xstzName) {
		    strncpy(achp->stylename,stsh->std[achp->istd].xstzName, sizeof(achp->stylename));
		  }
		  else {
		    wvError (("trying to copy null string\n"));
		  }
		}

		if(achp->istd != tistd)
		{
			/* the chpx contained instruction to apply character
			   style: we have to start all over again, since we init
			   the chp from the wrong (paragraph) style */
			tistd = achp->istd;
			wvInitCHPFromIstd (achp, achp->istd, stsh);
			goto apply_chpx;
		}
	}
	
    return (ret);
}


void
wvGetCHPX (wvVersion ver, CHPX * item, U8 * page, U16 * pos)
{
    U8 i;
    item->cbGrpprl = bread_8ubit (&(page[*pos]), pos);
    if (item->cbGrpprl > 0)
      {
	  item->grpprl = (U8 *) wvMalloc (item->cbGrpprl);
	  memcpy (item->grpprl, &(page[*pos]), item->cbGrpprl);
      }
    else
	item->grpprl = NULL;

    item->istd = 0;		/* I have no idea what to set this to... --
						   nothing; the istd is contained in the grpprl*/

    for (i = 0; i < item->cbGrpprl; i++)
	wvTrace (("chpx byte is %x\n", item->grpprl[i]));
}
