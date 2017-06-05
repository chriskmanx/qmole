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

/*
To apply a UPX.papx to a UPE.pap, set UPE.pap.istd equal to UPX.papx.istd, and
then apply the UPX.papx.grpprl to UPE.pap.
*/
void
wvAddPAPXFromBucket (PAP * apap, UPXF * upxf, STSH * stsh, wvStream * data)
{
    U8 *pointer;
    U16 i = 0;
    U16 sprm;
    apap->istd = upxf->upx.papx.istd;
    if (upxf->cbUPX <= 2)
	return;
    wvTrace (("no is %d\n", upxf->cbUPX));
#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < upxf->cbUPX - 2)
      {
	  fprintf (stderr, "%x (%d) ", *(upxf->upx.papx.grpprl + i),
		   *(upxf->upx.papx.grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif
    /*
       while (i < upxf->cbUPX-2)
     */
    while (i < upxf->cbUPX - 4)	/* the end of the list is at -2, but there has to be a full sprm of
				   len 2 as well */
      {
	  sprm = bread_16ubit (upxf->upx.papx.grpprl + i, &i);
#ifdef SPRMTEST
	  wvError (("sprm is %x\n", sprm));
#endif
	  pointer = upxf->upx.papx.grpprl + i;
	  if (i < upxf->cbUPX - 2)
	      wvApplySprmFromBucket (WORD8, sprm, apap, NULL, NULL, stsh,
				     pointer, &i, data);
      }
}

void
wvAddPAPXFromBucket6 (PAP * apap, UPXF * upxf, STSH * stsh)
{
    U8 *pointer;
    U16 i = 0;
    U16 sprm;
    U8 sprm8;
    apap->istd = upxf->upx.papx.istd;
    if (upxf->cbUPX <= 2)
	return;
    wvTrace (("no is %d\n", upxf->cbUPX));

#ifdef SPRMTEST
    fprintf (stderr, "\n");
    while (i < upxf->cbUPX - 2)
      {
	  fprintf (stderr, "%x (%d) ", *(upxf->upx.papx.grpprl + i),
		   *(upxf->upx.papx.grpprl + i));
	  i++;
      }
    fprintf (stderr, "\n");
    i = 0;
#endif

    while (i < upxf->cbUPX - 3)	/* the end of the list is at -2, but there has to be a full sprm of
				   len 1 as well */
      {
	  sprm8 = bread_8ubit (upxf->upx.papx.grpprl + i, &i);
#ifdef SPRMTEST
	  wvError (("pap word 6 sprm is %x (%d)\n", sprm8, sprm8));
#endif
	  sprm = (U16) wvGetrgsprmWord6 (sprm8);
#ifdef SPRMTEST
	  wvError (("pap word 6 sprm is converted to %x\n", sprm));
#endif
	  pointer = upxf->upx.papx.grpprl + i;
	  /* hmm, maybe im wrong here, but there appears to be corrupt
	   * word 6 sprm lists being stored in the file
	   */
	  if (i < upxf->cbUPX - 2)
	      wvApplySprmFromBucket (WORD6, sprm, apap, NULL, NULL, stsh,
				     pointer, &i, NULL);
      }
}


void
wvInitPAPFromIstd (PAP * apap, U16 istdBase, STSH * stsh)
{
    if (istdBase == istdNil)
	wvInitPAP (apap);
    else
      {
	  if (istdBase >= stsh->Stshi.cstd)
	    {
		wvError (
			 ("ISTD out of bounds, requested %d of %d\n",
			  istdBase, stsh->Stshi.cstd));
		wvInitPAP (apap);	/*it can't hurt to try and start with a blank istd */
		return;
	    }
	  else
	    {
		if (stsh->std[istdBase].cupx == 0)	/*empty slot in the array, i don't think this should happen */
		  {
		      wvTrace (("Empty style slot used (chp)\n"));
		      wvInitPAP (apap);
		  }
		else
		  {
		    wvCopyPAP (apap, &(stsh->std[istdBase].grupe[0].apap));
		    strncpy(apap->stylename,stsh->std[istdBase].xstzName, sizeof(apap->stylename));
		  }
	    }
      }
}

void
wvCopyPAP (PAP * dest, PAP * src)
{
    memcpy (dest, src, sizeof (PAP));
}


void
wvInitPAP (PAP * item)
{
    int i;
    item->istd = 0;
    item->jc = 0;
    item->fKeep = 0;
    item->fKeepFollow = 0;
    item->fPageBreakBefore = 0;
    item->fBrLnAbove = 0;
    item->fBrLnBelow = 0;
    item->fUnused = 0;
    item->pcVert = 0;
    item->pcHorz = 0;
    item->brcp = 0;
    item->brcl = 0;
    item->reserved1 = 0;
    item->ilvl = 0;
    item->fNoLnn = 0;
    item->ilfo = 0;
    item->nLvlAnm = 0;
    item->reserved2 = 0;
    item->fSideBySide = 0;
    item->reserved3 = 0;
    item->fNoAutoHyph = 0;
    item->fWidowControl = 1;
    item->dxaRight = 0;
    item->dxaLeft = 0;
    item->dxaLeft1 = 0;
    /*
       wvInitLSPD(&item->lspd);
     */
    item->lspd.fMultLinespace = 1;
    item->lspd.dyaLine = 240;

    item->dyaBefore = 0;
    item->dyaAfter = 0;

    wvInitPHE (&item->phe, 0);

    item->fCrLf = 0;
    item->fUsePgsuSettings = 0;
    item->fAdjustRight = 0;
    item->reserved4 = 0;
    item->fKinsoku = 0;
    item->fWordWrap = 0;
    item->fOverflowPunct = 0;
    item->fTopLinePunct = 0;
    item->fAutoSpaceDE = 0;
    item->fAtuoSpaceDN = 0;
    item->wAlignFont = 4;
    item->fVertical = 0;
    item->fBackward = 0;
    item->fRotateFont = 0;
    item->reserved5 = 0;
    item->reserved6 = 0;
    item->fInTable = 0;
    item->fTtp = 0;
    item->wr = 0;
    item->fLocked = 0;

    wvInitTAP (&item->ptap);

    item->dxaAbs = 0;
    item->dyaAbs = 0;
    item->dxaWidth = 0;

    wvInitBRC (&item->brcTop);
    wvInitBRC (&item->brcLeft);
    wvInitBRC (&item->brcBottom);
    wvInitBRC (&item->brcRight);
    wvInitBRC (&item->brcBetween);
    wvInitBRC (&item->brcBar);

    item->dxaFromText = 0;
    item->dyaFromText = 0;
    item->dyaHeight = 0;
    item->fMinHeight = 0;

    wvInitSHD (&item->shd);
    wvInitDCS (&item->dcs);
    item->lvl = 9;
    item->fNumRMIns = 0;
    wvInitANLD (&item->anld);
    item->fPropRMark = 0;
    item->ibstPropRMark = 0;
    wvInitDTTM (&item->dttmPropRMark);
    wvInitNUMRM (&item->numrm);
    item->itbdMac = 0;
    for (i = 0; i < itbdMax; i++)
	item->rgdxaTab[i] = 0;
    for (i = 0; i < itbdMax; i++)
	wvInitTBD (&item->rgtbd[i]);

    item->fBidi = 0;
	item->stylename[0] = 0;

	memset(&item->linfo,0,sizeof(item->linfo));
}

/*
1) Having found the index i of the FC in an FKP that marks the character stored
in the file immediately after the paragraph's paragraph mark,

1 is done in Simple mode through wvGetSimpleParaBounds which places this index
in fcLim by default

2) it is necessary to use the word offset stored in the first byte of the
fkp.rgbx[i - 1] to find the PAPX for the paragraph.

3) Using papx.istd to index into the properties stored for the style sheet ,

4) the paragraph properties of the style are copied to a local PAP.

5) Then the grpprl stored in the PAPX is applied to the local PAP,

6) and papx.istd along with fkp.rgbx.phe are moved into the local PAP.

7) The process thus far has created a PAP that describes what the paragraph properties
of the paragraph were at the last full save.
*/

int
wvAssembleSimplePAP (wvVersion ver, PAP * apap, U32 fc, PAPX_FKP * fkp, wvParseStruct * ps)
{
    PAPX *papx;
    int index;
    UPXF upxf;
    int ret = 0;

	/* list processing vars */
	U32 myListId = 0;
	LVLF * myLVLF = NULL;
	LVL * myLVL = NULL;
	LFO * myLFO = NULL;
	LST * myLST = NULL;
	LFOLVL * myLFOLVL = NULL;

	S32 myStartAt = -1;
	U8 * mygPAPX = NULL;
	U8 * mygCHPX = NULL;
	XCHAR * myNumberStr = NULL;
	S32 myNumberStr_count = 0;
	U32 mygPAPX_count = 0, mygCHPX_count = 0;

	PAPX myPAPX;
	CHPX myCHPX;

	S32 i = 0, j = 0, k = 0;

	int bNeedLST_LVL;
	int bLST_LVL_format;
	
	LVL * prevLVL;
	LVLF * prevLVLF;

    /*index is the i in the text above */
    index = wvGetIndexFCInFKP_PAPX (fkp, fc);

    wvTrace (("index is %d, using %d\n", index, index - 1));
    papx = &(fkp->grppapx[index - 1]);

    if (papx)
      {
	  wvTrace (("istd index is %d\n", papx->istd));
	  wvInitPAPFromIstd (apap, papx->istd, &ps->stsh);
      }
    else
	wvInitPAPFromIstd (apap, istdNil, &ps->stsh);

    if ((papx) && (papx->cb > 2))
      {
	  ret = 1;
#ifdef SPRMTEST
	  fprintf (stderr, "cbUPX is %d\n", papx->cb);
	  for (i = 0; i < papx->cb - 2; i++)
	      fprintf (stderr, "%x ", papx->grpprl[i]);
	  fprintf (stderr, "\n");
#endif
	  upxf.cbUPX = papx->cb;
	  upxf.upx.papx.istd = papx->istd;
	  upxf.upx.papx.grpprl = papx->grpprl;
	  if (ver == WORD8)
	      wvAddPAPXFromBucket (apap, &upxf, &ps->stsh, ps->data);
	  else
	      wvAddPAPXFromBucket6 (apap, &upxf, &ps->stsh);
      }

    if (papx)
	apap->istd = papx->istd;

    if (fkp->rgbx != NULL)
      wvCopyPHE (&apap->phe, &(fkp->rgbx[index - 1].phe), apap->fTtp);

	/*
	  By now we have assembled the paragraph properties based on the
	  info in the style associated with this pap and also in any of the
	  PAPX overrides; next step is to see if this paragraph is a part of
	  a list, and if so, to apply any list-specific overrides

	  The MS documentation on lists really sucks, but we've been able to decipher
	  some meaning from it and get simple lists to sorta work. This code mostly prints out
	  debug messages with useful information in them, but it will also append a list
	  and add a given paragraph to a given list
	*/

	if (!apap->ilfo)
		return ret;

	/* This is really silly, but it would seem that if there are both
	PAPX for the paragraph and the list, the paragraph ones take
	priority (basically, when a list is applied to a custom indented
	block, the block's indents become part of the list PAPX; if the
	indents of the block are subsequently modified, the PAPX of the
	list stays the same, and the PAPX of the block changes); this
	means that we now have to apply the list PAPX over what we have
	and then reapply the block PAPX (we had to apply the block's PAPX
	in order to find out if we are in a list !!!)*/

	if (!ps->lfo)
	  return ret;
	
	wvTrace(("list: ilvl %d, ilfo %d\n",apap->ilvl,apap->ilfo));	/* ilvl is the list level */

	/* first, get the LFO, and then find the lfovl for this paragraph */
	if (ps->lfo) 
	  myLFO = &ps->lfo[apap->ilfo - 1];

	while(i < (S32)apap->ilfo - 1 && i < (S32)ps->nolfo)
	{
		j += ps->lfo[i].clfolvl;
		i++;
	}

	/* 	remember how many overrides are there for this record */
        if (ps->lfo)
	  k = ps->lfo[i].clfolvl;
	else
	  k = 0;

	/* 	if there are any overrides, then see if one of them applies to this level */
	if(k && ps->lfolvl)
	{
		i = 0;
		while(i < k && ps->lfolvl[j].ilvl != apap->ilvl)
		{
			j++;
			i++;
		}

		if(i >= k)
		{
			wvTrace(("list: no LFOLVL found for this level (1)\n"));
			myLFOLVL = NULL;
		}
		else
		{
			myLFOLVL = &ps->lfolvl[j];
			wvTrace(("list: lfovl: iStartAt %d, fStartAt\n", myLFOLVL->iStartAt,myLFOLVL->fStartAt,myLFOLVL->fFormatting));
			if(!myLFOLVL->fFormatting && myLFOLVL->fStartAt)
				myStartAt = myLFOLVL->iStartAt;
		}
	}
	else
	{
		wvTrace(("list: no LFOLVL found for this level (2)\n"));
		myLFOLVL = NULL;
	}

	/* now that we might have the LFOLVL, let's see if we should use
	   the LVL from the LFO */
	bNeedLST_LVL = (!myLFOLVL || !myLFOLVL->fStartAt || !myLFOLVL->fFormatting);
	bLST_LVL_format = 1;
	
	if(myLFOLVL)
	{
		/* this branch has not been (thoroughly) debugged
		   Abi bugs 2205 and 2393 exhibit this behavior */
		wvTrace(("list: using the LVL from LFO\n"));
		myListId = myLFOLVL->iStartAt;
		i = 0;
		wvTrace(("list: number of LSTs %d, my lsid %d\n", ps->noofLST,myListId));
		while(i < ps->noofLST && ps->lst[i].lstf.lsid != myListId)
		{
			i++;
			wvTrace(("list: lsid in LST %d\n", ps->lst[i-1].lstf.lsid));
		}

		if(i == ps->noofLST || ps->lst[i].lstf.lsid != myListId)
		{
			wvTrace(("error: could not locate LST entry\n"));
			goto list_error;
		}

		myLST = &ps->lst[i];
		myLVL = &myLST->lvl[apap->ilvl];

		/* now we should have the LVL */
		if(!myLVL)
			return ret;

		myLVLF = &myLVL->lvlf;

		if(!myLVLF)
			return ret;

		myStartAt = myLFOLVL->fStartAt ? (S32)(myLVLF->iStartAt) : -1;

		mygPAPX = myLFOLVL->fFormatting ? myLVL->grpprlPapx : NULL;
		mygPAPX_count = myLFOLVL->fFormatting ? myLVLF->cbGrpprlPapx : 0;

		/* not sure about this, the CHPX applies to the number, so it
		   might be that we should take this if the fStartAt is set --
		   the docs are not clear */
		mygCHPX = myLFOLVL->fFormatting ? myLVL->grpprlChpx : NULL;
		mygCHPX_count = myLFOLVL->fFormatting ? myLVLF->cbGrpprlChpx : 0;

		myNumberStr = myLFOLVL->fStartAt && myLVL->numbertext ? myLVL->numbertext + 1 : NULL;
		myNumberStr_count = myNumberStr ? *(myLVL->numbertext) : 0;

		if(myLFOLVL->fFormatting)
			bLST_LVL_format = 0;

	}

	if(bNeedLST_LVL)
	{
		prevLVL = myLVL;
		prevLVLF = myLVLF;
		myListId = myLFO ? myLFO->lsid : 0;
		wvTrace(("list: using the LVL from LST\n"));
		i = 0;
		
		wvTrace(("list: number of LSTs %d, my lsid %d\n", ps->noofLST,myListId));
		while(i < ps->noofLST && ps->lst[i].lstf.lsid != myListId)
		{
			i++;
			wvTrace(("list: lsid in LST %d\n", ps->lst[i-1].lstf.lsid));
		}

		if(i == ps->noofLST || ps->lst[i].lstf.lsid != myListId)
		{
			wvTrace(("error: could not locate LST entry\n"));
			goto list_error;
		}

		myLST = &ps->lst[i];
		wvTrace(("is a simple list? %d - requested level %d\n", myLST->lstf.fSimpleList, apap->ilvl));
		if(myLST->lstf.fSimpleList)
			myLVL = myLST->lvl;
		else
			myLVL = &myLST->lvl[apap->ilvl];

		/* now we should have the correct LVL */
		if(!myLVL)
			return ret;
		
		myLVLF = &myLVL->lvlf;

		if(!myLVLF)
			return ret;

		/* retrieve any stuff we need from here (i.e., only what we
		   did not get from the LFO LVL) */
		myStartAt = myStartAt == -1 ? myLVLF->iStartAt : myStartAt;

		mygPAPX_count = !mygPAPX ? myLVLF->cbGrpprlPapx : mygPAPX_count;
		mygPAPX = !mygPAPX ? myLVL->grpprlPapx : mygPAPX;

		mygCHPX_count = !mygCHPX ? myLVLF->cbGrpprlChpx : mygCHPX_count;
		mygCHPX = !mygCHPX ? myLVL->grpprlChpx : mygCHPX;

		myNumberStr_count = !myNumberStr && myLVL->numbertext ? *(myLVL->numbertext) : myNumberStr_count;
		myNumberStr = !myNumberStr && myLVL->numbertext ? myLVL->numbertext + 1 : myNumberStr;


		/* if there was a valid LFO LVL record that pertained to
		   formatting then we will set the myLVL and myLVLF variables
		   back to this record so that it can be used */
		if(!bLST_LVL_format && prevLVL && prevLVLF)
		{
			myLVL = prevLVL;
			myLVLF = prevLVLF;
		}
	}

	wvTrace(("list: number text len %d, papx len %d, chpx len%d\n",myNumberStr_count,mygPAPX_count,mygCHPX_count));
	myPAPX.cb = mygPAPX_count;
	myPAPX.grpprl = mygPAPX;
	myPAPX.istd = apap->istd;

	/*
	  IMPORTANT now we have the list formatting sutff retrieved; it is found in several
	  different places:
	  apap->ilvl - the level of this list (0-8)

	  myStartAt	- the value at which the numbering for this listshould start
	  (i.e., the number of the first item on the list)

	  myListId	- the id of this list, we need this to know to which list this
	  paragraph belongs; unfortunately, there seem to be some cases where separate
	  lists *share* the same id, for instance when two lists, of different formatting,
	  are separated by only empty paragraphs. As a hack, AW will add the format number
	  to the list id, so gaining different id for different formattings (it is not foolproof,
	  for if id1 + format1 == id2 + format2 then we get two lists joined, but the probability
	  of that should be small). Further problem is that in AW, list id refers to the set of
	  list elements on the same level, while in Word the id is that of the entire list. The
	  easiest way to tranform the Word id to AW id is to add the level to the id

	  PAPX - the formatting information that needs to be added to the
	  format of this list

	  CHPX - the formatting of the list number

	  myNumberStr - the actual number string to display (XCHAR *); we probably need
	  this to work out the number separator, since there does not seem
	  to be any reference to this anywhere

	  myNumberStr_count - length of the number string

	  myLVLF->nfc - number format (see the enum below)

	  myLVLF->jc	- number alignment [0: lft, 1: rght, 2: cntr]

	  myLVLF->ixchFollow - what character stands between the number and the para
	  [0:= tab, 1: spc, 2: none]

	  we shall copy this info, except the ilvl, to the wv extension of
	  the PAP structure

	*/
	wvTrace(("list: id %d \n",myListId));
	wvTrace(("list: iStartAt %d\n", myStartAt));
	wvTrace(("list: lvlf: format %d\n",myLVLF->nfc)); /* see the comment above for nfc values */
	wvTrace(("list: lvlf: number align %d [0: lft, 1: rght, 2: cntr]\n",myLVLF->jc));
	wvTrace(("list: lvlf: ixchFollow %d [0:= tab, 1: spc, 2: none]\n",myLVLF->ixchFollow));

	apap->linfo.id = myListId;
	apap->linfo.start = myStartAt;
	apap->linfo.numberstr = myNumberStr;
	apap->linfo.numberstr_size = myNumberStr_count;
	apap->linfo.format = myLVLF->nfc;
	apap->linfo.align = myLVLF->jc;
	apap->linfo.ixchFollow = myLVLF->ixchFollow;

	/* the number formatting */
	myCHPX.cbGrpprl = mygCHPX_count;
	myCHPX.grpprl = mygCHPX;
	myCHPX.istd = 4095; 

	/* next we need to apply the list PAPX to our PAP */
    if (myPAPX.cb > 2)
	{
		ret = 1;
		upxf.cbUPX = myPAPX.cb;
		upxf.upx.papx.istd = myPAPX.istd;
		upxf.upx.papx.grpprl = myPAPX.grpprl;
		if (ver == WORD8)
			wvAddPAPXFromBucket (apap, &upxf, &ps->stsh, ps->data);
		else
			wvAddPAPXFromBucket6 (apap, &upxf, &ps->stsh);

		/* now we have to reapply the original PAPX, see note at top
		   of the list code */
		if((papx) && (papx->cb > 2))
		{
			ret = 1;
			upxf.cbUPX = papx->cb;
			upxf.upx.papx.istd = papx->istd;
			upxf.upx.papx.grpprl = papx->grpprl;
			if (ver == WORD8)
				wvAddPAPXFromBucket (apap, &upxf, &ps->stsh, ps->data);
			else
				wvAddPAPXFromBucket6 (apap, &upxf, &ps->stsh);
		}
	}

	/* next see if the list number comes with
	   additional char formatting information; if it does, we will
	   stre it the linfo.chp */

	if(myCHPX.cbGrpprl)
	{
		ret = 1;

		wvAssembleSimpleCHP(ver, &apap->linfo.chp, apap, 0, NULL, &ps->stsh);
		upxf.cbUPX = myCHPX.cbGrpprl;
		upxf.upx.chpx.grpprl = myCHPX.grpprl;
		if (ver == WORD8)
			wvAddCHPXFromBucket (&apap->linfo.chp, &upxf, &ps->stsh);
		else
			wvAddCHPXFromBucket6 (&apap->linfo.chp, &upxf, &ps->stsh);
	}
	


    if (myPAPX.istd != istdNil)
		apap->istd = myPAPX.istd;
	
list_error:
    return (ret);
}

void
wvReleasePAPX (PAPX * item)
{
    item->cb = 0;
    item->istd = 0;
    wvFree (item->grpprl);
    item->grpprl = NULL;
}

void
wvInitPAPX (PAPX * item)
{
    item->cb = 0;
    item->istd = 0;
    item->grpprl = NULL;
}

void
wvGetPAPX (wvVersion ver, PAPX * item, U8 * page, U16 * pos)
{
    U16 cw;
    cw = bread_8ubit (&(page[*pos]), pos);
    if ((cw == 0) && (ver == WORD8))	/* only do this for word 97 */
      {
	  wvTrace (("cw was pad %d\n", cw));
	  cw = bread_8ubit (&(page[*pos]), pos);
	  wvTrace (("cw was %d\n", cw));
      }
    item->cb = cw * 2;
    item->istd = bread_16ubit (&(page[*pos]), pos);
    wvTrace (("papx istd is %x\n", item->istd));
    wvTrace (("no of bytes is %d\n", item->cb));
    if (item->cb > 2)
      {
	  item->grpprl = (U8 *) wvMalloc (item->cb - 2);
	  memcpy (item->grpprl, &(page[*pos]), (item->cb) - 2);
      }
    else
	item->grpprl = NULL;
}


int
isPAPConform (PAP * current, PAP * previous)
{
    if ((current) && (previous))
	if (wvEqualBRC (&current->brcLeft, &previous->brcLeft))
	    if (wvEqualBRC (&current->brcRight, &previous->brcRight))
		if (current->dxaWidth == previous->dxaWidth)
		    if (current->fInTable == previous->fInTable)
			return (1);
    return (0);
}




void
wvCopyConformPAP (PAP * dest, PAP * src)
{
    if (src)
      {
#ifdef PURIFY
	  wvInitPAP (dest);
#endif
	  dest->brcLeft = src->brcLeft;
	  dest->brcRight = src->brcRight;
	  dest->dxaWidth = src->dxaWidth;
	  dest->fInTable = src->fInTable;
      }
    else
	wvInitPAP (dest);
}
