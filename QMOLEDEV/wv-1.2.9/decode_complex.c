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
To find the beginning of the paragraph containing a character in a complex
document, it's first necessary to 

1) search for the piece containing the character in the piece table. 

2) Then calculate the FC in the file that stores the character from the piece 
	table information. 
	
3) Using the FC, search the FCs FKP for the largest FC less than the character's 
	FC, call it fcTest. 
	
4) If the character at fcTest-1 is contained in the current piece, then the 
	character corresponding to that FC in the piece is the first character of 
	the paragraph. 
	
5) If that FC is before or marks the beginning of the piece, scan a piece at a 
time towards the beginning of the piece table until a piece is found that 
contains a paragraph mark. 

(This can be done by using the end of the piece FC, finding the largest FC in 
its FKP that is less than or equal to the end of piece FC, and checking to see 
if the character in front of the FKP FC (which must mark a paragraph end) is 
within the piece.)

6) When such an FKP FC is found, the FC marks the first byte of paragraph text.
*/

/*
To find the end of a paragraph for a character in a complex format file,
again 

1) it is necessary to know the piece that contains the character and the
FC assigned to the character. 

2) Using the FC of the character, first search the FKP that describes the 
character to find the smallest FC in the rgfc that is larger than the character 
FC. 

3) If the FC found in the FKP is less than or equal to the limit FC of the 
piece, the end of the paragraph that contains the character is at the FKP FC 
minus 1. 

4) If the FKP FC that was found was greater than the FC of the end of the 
piece, scan piece by piece toward the end of the document until a piece is 
found that contains a paragraph end mark. 

5) It's possible to check if a piece contains a paragraph mark by using the 
FC of the beginning of the piece to search in the FKPs for the smallest FC in 
the FKP rgfc that is greater than the FC of the beginning of the piece. 

If the FC found is less than or equal to the limit FC of the
piece, then the character that ends the paragraph is the character
immediately before the FKP FC.
*/
int
wvGetComplexParaBounds (wvVersion ver, PAPX_FKP * fkp, U32 * fcFirst,
			U32 * fcLim, U32 currentfc, CLX * clx, BTE * bte,
			U32 * pos, int nobte, U32 piece, wvStream * fd)
{
    /*
       U32 currentfc;
     */
    BTE entry;
    long currentpos;

    if (currentfc == 0xffffffffL)
      {
	  wvError (
		   ("Para Bounds not found !, this is ok if this is the last para, otherwise its a disaster\n"));
	  return (-1);
      }

    if (0 != wvGetBTE_FromFC (&entry, currentfc, bte, pos, nobte))
      {
	  wvError (("BTE not found !\n"));
	  return (-1);
      }
    currentpos = wvStream_tell (fd);
    /*The pagenumber of the FKP is entry.pn */

    wvTrace (("the entry.pn is %d\n", entry.pn));
    wvGetPAPX_FKP (ver, fkp, entry.pn, fd);

    wvGetComplexParafcFirst (ver, fcFirst, currentfc, clx, bte, pos, nobte,
			     piece, fkp, fd);

    wvReleasePAPX_FKP (fkp);
    wvTrace (("BREAK\n"));
    wvGetPAPX_FKP (ver, fkp, entry.pn, fd);

    piece =
	wvGetComplexParafcLim (ver, fcLim, currentfc, clx, bte, pos, nobte,
			       piece, fkp, fd);

    wvStream_goto (fd, currentpos);
    return (piece);
}

int
wvGetComplexParafcLim (wvVersion ver, U32 * fcLim, U32 currentfc, CLX * clx,
		       BTE * bte, U32 * pos, int nobte, U32 piece,
		       PAPX_FKP * fkp, wvStream * fd)
{
    U32 fcTest, beginfc;
    BTE entry;
    *fcLim = 0xffffffffL;
    wvTrace (("here is fcLim, currentfc is %x\n", currentfc));
    fcTest = wvSearchNextSmallestFCPAPX_FKP (fkp, currentfc);

    wvTrace (
	     ("fcTest is %x, end is %x\n", fcTest,
	      wvGetEndFCPiece (piece, clx)));


    if (fcTest <= wvGetEndFCPiece (piece, clx))
      {
	  *fcLim = fcTest;
      }
    else
      {
	  /*get end fc of previous piece */
	  piece++;
	  while (piece < clx->nopcd)
	    {
		wvTrace (("piece is %d\n", piece));
		beginfc = wvNormFC (clx->pcd[piece].fc, NULL);
		if (0 != wvGetBTE_FromFC (&entry, beginfc, bte, pos, nobte))
		  {
		      wvError (("BTE not found !\n"));
		      return (-1);
		  }
		wvReleasePAPX_FKP (fkp);
		wvGetPAPX_FKP (ver, fkp, entry.pn, fd);
		fcTest = wvSearchNextSmallestFCPAPX_FKP (fkp, beginfc);
		wvTrace (
			 ("fcTest(t) is %x, end is %x\n", fcTest,
			  wvGetEndFCPiece (piece, clx)));
		if (fcTest <= wvGetEndFCPiece (piece, clx))
		  {
		      *fcLim = fcTest;
		      break;
		  }
		piece++;
	    }
      }
    wvTrace (("fcLim is %x\n", *fcLim));
    if (piece == clx->nopcd)
      {
	  wvTrace (("failed to find a solution to end of paragraph\n"));
	  *fcLim = fcTest;
	  return (clx->nopcd - 1);	/* test using this */
      }
    return (piece);
}


int
wvGetComplexParafcFirst (wvVersion ver, U32 * fcFirst, U32 currentfc,
			 CLX * clx, BTE * bte, U32 * pos, int nobte,
			 U32 piece, PAPX_FKP * fkp, wvStream * fd)
{
    U32 fcTest, endfc;
    BTE entry;
    fcTest = wvSearchNextLargestFCPAPX_FKP (fkp, currentfc);

    wvTrace (("fcTest (s) is %x\n", fcTest));

    if (wvQuerySamePiece (fcTest - 1, clx, piece))
      {
	  wvTrace (("same piece\n"));
	  *fcFirst = fcTest - 1;
      }
    else
      {
	  /*
	     get end fc of previous piece ??, or use the end of the current piece
	   */
	  piece--;
	  while (piece != 0xffffffffL)
	    {
		wvTrace (("piece is %d\n", piece));
		endfc = wvGetEndFCPiece (piece, clx);
		wvTrace (("endfc is %x\n", endfc));
		if (0 != wvGetBTE_FromFC (&entry, endfc, bte, pos, nobte))
		  {
		      wvError (("BTE not found !\n"));
		      return (-1);
		  }
		wvReleasePAPX_FKP (fkp);
		wvGetPAPX_FKP (ver, fkp, entry.pn, fd);
		fcTest = wvSearchNextLargestFCPAPX_FKP (fkp, endfc);
		wvTrace (("fcTest(ft) is %x\n", fcTest));
		if (wvQuerySamePiece (fcTest - 1, clx, piece))
		  {
		      *fcFirst = fcTest - 1;
		      break;
		  }
		piece--;
	    }

      }
    if (piece == 0xffffffffL)
      {
	  wvTrace (
		   ("failed to find a solution to the beginning of the paragraph\n"));
	  *fcFirst = currentfc;
      }
    wvTrace (("fcFirst is finally %x\n", *fcFirst));
    return (0);
}


/* char properties version of the above -JB */
/* only difference is that we're using CHPX FKP pages,
 * and specifically just the Get and Release functions are
 * different between the two. We might be able to 
 * abstract the necessary functions to avoid duplicating them... */

int
wvGetComplexCharBounds (wvVersion ver, CHPX_FKP * fkp, U32 * fcFirst,
			U32 * fcLim, U32 currentfc, CLX * clx, BTE * bte,
			U32 * pos, int nobte, U32 piece, wvStream * fd)
{
    BTE entry;
    long currentpos;

    wvTrace (("current fc is %x\n", currentfc));

    if (currentfc == 0xffffffffL)
      {
	  wvTrace (
		   ("Char Bounds not found !, this is ok if this is the last char, otherwise its a disaster\n"));
	  return (-1);
      }

    if (0 != wvGetBTE_FromFC (&entry, currentfc, bte, pos, nobte))
      {
	  wvError (("BTE not found !\n"));
	  return (-1);
      }
    currentpos = wvStream_tell (fd);
    /*The pagenumber of the FKP is entry.pn */

    wvGetCHPX_FKP (ver, fkp, entry.pn, fd);

    wvGetComplexCharfcFirst (ver, fcFirst, currentfc, clx, bte, pos, nobte,
			     piece, fkp, fd);
    wvTrace (("BEFORE PIECE is %d\n", piece));

    wvReleaseCHPX_FKP (fkp);
    wvGetCHPX_FKP (ver, fkp, entry.pn, fd);

    piece =
	wvGetComplexCharfcLim (ver, fcLim, currentfc, clx, bte, pos, nobte,
			       piece, fkp, fd);
    wvTrace (("AFTER PIECE is %d\n", piece));

    wvStream_goto (fd, currentpos);
    return (piece);
}

int
wvGetComplexCharfcLim (wvVersion ver, U32 * fcLim, U32 currentfc, CLX * clx,
		       BTE * bte, U32 * pos, int nobte, U32 piece,
		       CHPX_FKP * fkp, wvStream * fd)
{
    U32 fcTest;
    /*
       BTE entry;
     */
    *fcLim = 0xffffffffL;
    /* this only works with the initial rgfc array, which is the
     * same for both CHPX and PAPX FKPs */
    fcTest = wvSearchNextSmallestFCPAPX_FKP ((PAPX_FKP *) fkp, currentfc);

    wvTrace (("fcTest is %x\n", fcTest));

    /*
       this single line replaces all the rest, is it conceivable that i overengineered,
       careful rereading of the spec makes no mention of repeating the para process to
       find the boundaries of the exception text runs
     */
    *fcLim = fcTest;
    wvTrace (("fcLim is %x\n", *fcLim));
    if (piece == clx->nopcd)
	return (clx->nopcd - 1);	/* test using this */
    return (piece);
}


int
wvGetComplexCharfcFirst (wvVersion ver, U32 * fcFirst, U32 currentfc,
			 CLX * clx, BTE * bte, U32 * pos, int nobte,
			 U32 piece, CHPX_FKP * fkp, wvStream * fd)
{
    U32 fcTest /*,endfc */ ;
    /*BTE entry; */
    /* this only works with the initial rgfc array, which is the */
    fcTest = wvSearchNextLargestFCCHPX_FKP (fkp, currentfc);

    wvTrace (("fcTest (s) is %x\n", fcTest));

    /*
       this single line replaces all the rest, is it conceivable that i overengineered,
       careful rereading of the spec makes no mention of repeating the para process to
       find the boundaries of the exception text runs
     */
    *fcFirst = fcTest;
    return (0);
}

/*
how this works,
we seek to the beginning of the text, we loop for a count of charaters that is stored in the fib.

the piecetable divides the text up into various sections, we keep track of our location vs
the next entry in that table, when we reach that location, we seek to the position that
the table tells us to go.

there are special cases for coming to the end of a section, and for the beginning and ends of
pages. for the purposes of headers and footers etc.
*/
void
wvDecodeComplex (wvParseStruct * ps)
{
    U32 piececount = 0, i, j, spiece = 0;
    U32 beginfc, endfc;
	U32 stream_size;
    U32 begincp, endcp;
    int ichartype;
    U8  chartype;
    U16 eachchar;
    U32 para_fcFirst, para_fcLim = 0xffffffffL;
    U32 dummy, nextpara_fcLim = 0xffffffffL;
    U32 char_fcFirst, char_fcLim = 0xffffffffL;
    U32 section_fcFirst, section_fcLim = 0xffffffffL;
    U32 comment_cpFirst = 0xffffffffL, comment_cpLim = 0xffffffffL;
    BTE *btePapx = NULL, *bteChpx = NULL;
    U32 *posPapx = NULL, *posChpx = NULL;
    U32 para_intervals, char_intervals, section_intervals, atrd_intervals;
    int cpiece = 0, npiece = 0;
    PAPX_FKP para_fkp;
    PAP apap;
    CHPX_FKP char_fkp;
    CHP achp;
    int para_pendingclose = 0, comment_pendingclose = 0, char_pendingclose =
	0, section_pendingclose = 0;
    int para_dirty = 0, char_dirty = 0, section_dirty = 0;
    SED *sed;
    SEP sep;
    U32 *posSedx;
    ATRD *atrd, *catrd = NULL;
    U32 *posAtrd;
    STTBF grpXstAtnOwners, SttbfAtnbkmk;
    BKF *bkf;
    U32 *posBKF;
    U32 bkf_intervals;
    BKL *bkl;
    U32 *posBKL;
    U32 bkl_intervals;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);
    external_wvReleasePAPX_FKP ();
    external_wvReleaseCHPX_FKP ();

    /*dop */
    wvGetDOP (ver, &ps->dop, ps->fib.fcDop,
	      ps->fib.lcbDop, ps->tablefd);

#if 0
/* 
this is the versioning name information, the first 22 bytes of each sttbf entry are 
unknown, the rest is a ordinary unicode string, is the time and date and saved by
encoded into the first 22 bytes.
*/
    STTBF versioning;
    if (ver == 0)
      {
	  U16 *str;
	  wvError (("into the versions\n"));
	  wvGetSTTBF (&versioning, ps->fib.fcSttbfUssr, ps->fib.lcbSttbfUssr,
		      ps->tablefd);
	  str = UssrStrBegin (&versioning, 0);
	  wvError (("versioning text is %s\n", wvWideStrToMB (str)));
      }
#endif

    wvGetATRD_PLCF (&atrd, &posAtrd, &atrd_intervals, ps->fib.fcPlcfandRef,
		    ps->fib.lcbPlcfandRef, ps->tablefd);
    wvGetGrpXst (&grpXstAtnOwners, ps->fib.fcGrpXstAtnOwners,
		 ps->fib.lcbGrpXstAtnOwners, ps->tablefd);
    wvTrace (
	     ("offset is %x, len is %d\n", ps->fib.fcSttbfAtnbkmk,
	      ps->fib.lcbSttbfAtnbkmk));
    wvGetSTTBF (&SttbfAtnbkmk, ps->fib.fcSttbfAtnbkmk,
		ps->fib.lcbSttbfAtnbkmk, ps->tablefd);
    wvGetBKF_PLCF (&bkf, &posBKF, &bkf_intervals, ps->fib.fcPlcfAtnbkf,
		   ps->fib.lcbPlcfAtnbkf, ps->tablefd);
    wvGetBKL_PLCF (&bkl, &posBKL, &bkl_intervals, ps->fib.fcPlcfAtnbkl,
           ps->fib.lcbPlcfAtnbkl, ps->fib.fcPlcfAtnbkf, ps->fib.lcbPlcfAtnbkf,
           ps->tablefd);

    /*we will need the stylesheet to do anything useful with layout and look */
    wvGetSTSH (&ps->stsh, ps->fib.fcStshf, ps->fib.lcbStshf, ps->tablefd);

    /* get font list */
    if ((ver == WORD6)
	|| (ver == WORD7))
	wvGetFFN_STTBF6 (&ps->fonts, ps->fib.fcSttbfffn, ps->fib.lcbSttbfffn,
			 ps->tablefd);
    else
	wvGetFFN_STTBF (&ps->fonts, ps->fib.fcSttbfffn, ps->fib.lcbSttbfffn,
			ps->tablefd);

    /*we will need the table of names to answer questions like the name of the doc */
    if ((ver == WORD6)
	|| (ver == WORD7))
      {
	  wvGetSTTBF6 (&ps->anSttbfAssoc, ps->fib.fcSttbfAssoc,
		       ps->fib.lcbSttbfAssoc, ps->tablefd);
	  wvGetSTTBF6 (&ps->Sttbfbkmk, ps->fib.fcSttbfbkmk,
		       ps->fib.lcbSttbfbkmk, ps->tablefd);
      }
    else
      {
	  wvGetSTTBF (&ps->anSttbfAssoc, ps->fib.fcSttbfAssoc,
		      ps->fib.lcbSttbfAssoc, ps->tablefd);
	  wvGetSTTBF (&ps->Sttbfbkmk, ps->fib.fcSttbfbkmk,
		      ps->fib.lcbSttbfbkmk, ps->tablefd);
      }

    /*Extract all the list information that we will need to handle lists later on */
    wvGetLST (&ps->lst, &ps->noofLST, ps->fib.fcPlcfLst, ps->fib.lcbPlcfLst,
	      ps->tablefd);
    wvGetLFO_records (&ps->lfo, &ps->lfolvl, &ps->lvl, &ps->nolfo,
		      &ps->nooflvl, ps->fib.fcPlfLfo, ps->fib.lcbPlfLfo,
		      ps->tablefd);
    /* init the starting list number table */
    if (ps->nolfo)
      {
	  ps->liststartnos = (U32 *) wvMalloc (9 * ps->nolfo * sizeof (U32));
	  ps->listnfcs = (U8 *) wvMalloc (9 * ps->nolfo);
	  ps->finallvl = (LVL *) wvMalloc (9 * ps->nolfo * sizeof (LVL));
	  for (i = 0; i < 9 * ps->nolfo; i++)
	    {
		ps->liststartnos[i] = 0xffffffffL;
		ps->listnfcs[i] = 0xff;
		wvInitLVL (&(ps->finallvl[i]));
	    }
      }
    else
      {
	  ps->liststartnos = NULL;
	  ps->listnfcs = NULL;
	  ps->finallvl = NULL;
      }

    /*Extract Graphic Information */
    wvGetFSPA_PLCF (&ps->fspa, &ps->fspapos, &ps->nooffspa,
		    ps->fib.fcPlcspaMom, ps->fib.lcbPlcspaMom, ps->tablefd);
    wvGetFDOA_PLCF (&ps->fdoa, &ps->fdoapos, &ps->nooffdoa,
		    ps->fib.fcPlcdoaMom, ps->fib.lcbPlcdoaMom, ps->tablefd);

    wvGetCLX (ver, &ps->clx,
	      (U32) ps->fib.fcClx, ps->fib.lcbClx, (U8) ps->fib.fExtChar,
	      ps->tablefd);

    para_fcFirst = char_fcFirst = section_fcFirst =
	wvConvertCPToFC (0, &ps->clx);

#ifdef DEBUG
    if ((ps->fib.ccpFtn) || (ps->fib.ccpHdr))
	wvTrace (("Special ending\n"));
#endif

    /*
       we will need the paragraph and character bounds table to make decisions as 
       to where a table begins and ends
     */
    if ((ver == WORD6)
	|| (ver == WORD7))
      {
	  wvGetBTE_PLCF6 (&btePapx, &posPapx, &para_intervals,
			  ps->fib.fcPlcfbtePapx, ps->fib.lcbPlcfbtePapx,
			  ps->tablefd);
	  wvGetBTE_PLCF6 (&bteChpx, &posChpx, &char_intervals,
			  ps->fib.fcPlcfbteChpx, ps->fib.lcbPlcfbteChpx,
			  ps->tablefd);
      }
    else
      {
	  wvGetBTE_PLCF (&btePapx, &posPapx, &para_intervals,
			 ps->fib.fcPlcfbtePapx, ps->fib.lcbPlcfbtePapx,
			 ps->tablefd);
	  wvGetBTE_PLCF (&bteChpx, &posChpx, &char_intervals,
			 ps->fib.fcPlcfbteChpx, ps->fib.lcbPlcfbteChpx,
			 ps->tablefd);
      }

    wvGetSED_PLCF (&sed, &posSedx, &section_intervals, ps->fib.fcPlcfsed,
		   ps->fib.lcbPlcfsed, ps->tablefd);
    wvTrace (("section_intervals is %d\n", section_intervals));

    wvInitPAPX_FKP (&para_fkp);
    wvInitCHPX_FKP (&char_fkp);

    if(wvHandleDocument (ps, DOCBEGIN))
		goto  finish_processing;

	/*get stream size for bounds checking*/
	stream_size = wvStream_size(ps->mainfd);

    /*for each piece */
    for (piececount = 0; piececount < ps->clx.nopcd; piececount++)
      {
	  ichartype =
	      wvGetPieceBoundsFC (&beginfc, &endfc, &ps->clx, piececount);
	  if(ichartype==-1)
		  break;
	  chartype = (U8) ichartype;
	  /*lvm007@aha.ru fix antiloop: check stream size */
	  if(beginfc>stream_size || endfc>stream_size){
		  wvError (
		   ("Piece Bounds out of range!, its a disaster\n"));
		  continue;
	  }
	  wvStream_goto (ps->mainfd, beginfc);
	  /*lvm007@aha.ru fix antiloop fix*/
	  if(wvGetPieceBoundsCP (&begincp, &endcp, &ps->clx, piececount)==-1)
		  break;
	  wvTrace (
		   ("piece begins at %x and ends just before %x. the char end is %x\n",
		    beginfc, endfc, char_fcLim));

	  /*
	     text that is not in the same piece is not guaranteed to have the same properties as
	     the rest of the exception run, so force a stop and restart of these properties.
	   */
	  char_fcLim = beginfc;

	  for (i = begincp, j = beginfc; (i < endcp /*&& i<ps->fib.ccpText */ );
	       i++, j += wvIncFC (chartype))
	    {
		ps->currentcp = i;
		/* character properties */
		if (j == char_fcLim)
		  {
		      wvHandleElement (ps, CHARPROPEND, (void *) &achp,
				       char_dirty);
		      char_pendingclose = 0;
		  }

		/* comment ending location */
		if (i == comment_cpLim)
		  {
		      wvHandleElement (ps, COMMENTEND, (void *) catrd, 0);
		      comment_pendingclose = 0;
		  }

		/* paragraph properties */
		if (j == para_fcLim)
		  {
		      wvHandleElement (ps, PARAEND, (void *) &apap, para_dirty);
		      para_pendingclose = 0;
		  }

		/* section properties */
		if (j == section_fcLim)
		  {
		      wvHandleElement (ps, SECTIONEND, (void *) &sep,
				       section_dirty);
		      section_pendingclose = 0;
		  }

		if ((section_fcLim == 0xffffffff) || (section_fcLim == j))
		  {
		      section_dirty =
			  wvGetSimpleSectionBounds (ver, ps,
						    &sep, &section_fcFirst,
						    &section_fcLim, i,
						    &ps->clx, sed, &spiece,
						    posSedx,
						    section_intervals,
						    &ps->stsh, ps->mainfd);
		      section_dirty =
			  (wvGetComplexSEP
			   (ver, &sep, spiece,
			    &ps->stsh, &ps->clx) ? 1 : section_dirty);
		  }

		if (j == section_fcFirst)
		  {
		      wvHandleElement (ps, SECTIONBEGIN, (void *) &sep,
				       section_dirty);
		      section_pendingclose = 1;
		  }


		if ((para_fcLim == 0xffffffffL) || (para_fcLim == j))
		  {
		      wvReleasePAPX_FKP (&para_fkp);
		      wvTrace (
			       ("cp and fc are %x(%d) %x\n", i, i,
				wvConvertCPToFC (i, &ps->clx)));
		      cpiece =
			  wvGetComplexParaBounds (ver, &para_fkp,
						  &para_fcFirst, &para_fcLim,
						  wvConvertCPToFC (i,
								   &ps->clx),
						  &ps->clx, btePapx, posPapx,
						  para_intervals, piececount,
						  ps->mainfd);
		      wvTrace (
			       ("para begin and end is %x %x\n", para_fcFirst,
				para_fcLim));

		      if (0 == para_pendingclose)
			{
			    /*
			       if there's no paragraph open, but there should be then I believe that the fcFirst search
			       has failed me, so I set it to now. I need to investigate this further. I believe it occurs
			       when a the last piece ended simultaneously with the last paragraph, and that the algorithm
			       for finding the beginning of a para breaks under that condition. I need more examples to
			       be sure, but it happens is very large complex files so its hard to find
			     */
			    if (j != para_fcFirst)
			      {
				  wvWarning (
					     ("There is no paragraph due to open but one should be, plugging the gap.\n"));
				  para_fcFirst = j;
			      }
			}

		  }

		if (j == para_fcFirst)
		  {
		      para_dirty =
			  wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);
		      para_dirty =
			  (wvAssembleComplexPAP
			   (ver, &apap, cpiece, ps) ? 1 : para_dirty);
#ifdef SPRMTEST
		      {
			  int p;
			  wvTrace (("Assembled Complex\n"));
			  for (p = 0; p < apap.itbdMac; p++)
			      wvError (
				       ("Tab stop positions are %f inches (%d)\n",
					((float) (apap.rgdxaTab[p])) / 1440,
					apap.rgdxaTab[p]));
		      }
#endif

		      /* test section */
		      wvReleasePAPX_FKP (&para_fkp);
		      wvTrace (
			       ("cp and fc are %x(%d) %x\n", i, i,
				wvConvertCPToFC (i, &ps->clx)));
		      npiece =
			  wvGetComplexParaBounds (ver, &para_fkp,
						  &dummy, &nextpara_fcLim,
						  para_fcLim, &ps->clx,
						  btePapx, posPapx,
						  para_intervals, piececount,
						  ps->mainfd);
		      wvTrace (
			       ("para begin and end is %x %x\n", para_fcFirst,
				para_fcLim));
		      if (npiece > -1)
			{
			    wvAssembleSimplePAP (ver, &ps->nextpap, nextpara_fcLim, &para_fkp, ps);
			    wvAssembleComplexPAP (ver, &ps->nextpap, npiece,ps);
			}
		      else
			  wvInitPAP (&ps->nextpap);
		      /* end test section */

		      if ((apap.fInTable) && (!apap.fTtp))
			{
			    wvGetComplexFullTableInit (ps, para_intervals,
						       btePapx, posPapx,
						       piececount);
			    wvGetComplexRowTap (ps, &apap, para_intervals,
						btePapx, posPapx, piececount);
			}
		      else if (apap.fInTable == 0)
			  ps->intable = 0;

		      wvHandleElement (ps, PARABEGIN, (void *) &apap,
				       para_dirty);

		      char_fcLim = j;
		      para_pendingclose = 1;
		  }


		if ((comment_cpLim == 0xffffffffL) || (comment_cpLim == i))
		  {
		      wvTrace (
			       ("searching for the next comment begin cp is %d\n",
				i));
		      catrd =
			  wvGetCommentBounds (&comment_cpFirst,
					      &comment_cpLim, i, atrd,
					      posAtrd, atrd_intervals,
					      &SttbfAtnbkmk, bkf, posBKF,
					      bkf_intervals, bkl, posBKL,
					      bkl_intervals);
		      wvTrace (
			       ("begin and end are %d %d\n", comment_cpFirst,
				comment_cpLim));
		  }

		if (i == comment_cpFirst)
		  {
		      wvHandleElement (ps, COMMENTBEGIN, (void *) catrd, 0);
		      comment_pendingclose = 1;
		  }


		if ((char_fcLim == 0xffffffffL) || (char_fcLim == j))
		  {
		      wvReleaseCHPX_FKP (&char_fkp);
		      /*try this without using the piece of the end char for anything */
		      wvGetComplexCharBounds (ver, &char_fkp,
					      &char_fcFirst, &char_fcLim,
					      wvConvertCPToFC (i, &ps->clx),
					      &ps->clx, bteChpx, posChpx,
					      char_intervals, piececount,
					      ps->mainfd);
		      wvTrace (
			       ("Bounds from %x to %x\n", char_fcFirst,
				char_fcLim));
		      if (char_fcLim == char_fcFirst)
			  wvError (
				   ("I believe that this is an error, and you might see incorrect character properties\n"));
		      if (0 == char_pendingclose)
			{
			    /*
			       if there's no character run open, but there should be then I believe that the fcFirst search
			       has failed me, so I set it to now. I need to investigate this further.
			     */
			    if (j != char_fcFirst)
			      {
				  wvTrace (
					   ("There is no character run due to open but one should be, plugging the gap.\n"));
				  char_fcFirst = j;
			      }

			}
		      else{
  			 /* lvm007@aha.ru fix: if currentfc>fcFirst but CHARPROP's changed look examples/charprops.doc for decode_simple*/
			 if(char_fcFirst< j)
				char_fcFirst = j;
		       }
		  }

		if (j == char_fcFirst)
		  {
		      /* a CHP's base style is in the para style */
		      /*achp.istd = apap.istd;*/
		      wvTrace (("getting chp\n"));
		      char_dirty =
				  wvAssembleSimpleCHP (ver, &achp, &apap,
					       char_fcLim, &char_fkp,
					       &ps->stsh);
		      wvTrace (("getting complex chp\n"));
		      char_dirty =
			  (wvAssembleComplexCHP
			   (ver, &achp, cpiece,
			    &ps->stsh, &ps->clx) ? 1 : char_dirty);
		      wvHandleElement (ps, CHARPROPBEGIN, (void *) &achp,
				       char_dirty);
		      char_pendingclose = 1;
		  }


		eachchar = wvGetChar (ps->mainfd, chartype);

		/* previously, in place of ps there was a NULL,
		 * but it was crashing Abiword. Was it NULL for a
		 * reason? -JB */
		/* 
		   nah, it was a oversight from when i didn't actually
		   use ps in this function
		   C.
		 */
		if ((eachchar == 0x07) && (!achp.fSpec))
		    ps->endcell = 1;

		wvTrace (("char pos is %x %x\n", j, eachchar));
		wvOutputTextChar (eachchar, chartype, ps, &achp);
	    }

	  if (j == para_fcLim)
	    {
		wvHandleElement (ps, PARAEND, (void *) &apap, para_dirty);
		para_pendingclose = 0;
		para_fcLim = 0xffffffffL;
	    }

	  if (i == comment_cpLim)
	    {
		wvHandleElement (ps, COMMENTEND, (void *) catrd, 0);
		comment_pendingclose = 0;
		comment_cpLim = 0xffffffffL;
	    }

	  if (j == char_fcLim)
	    {
		wvHandleElement (ps, CHARPROPEND, (void *) &achp, char_dirty);
		char_pendingclose = 0;
		char_fcLim = 0xffffffffL;
	    }

#if 0
	  /*      
	     I might have to rethink this closing tag enforcer for complex mode, have to think the
	     flow out a bit more, this section one is plain wrong, im leaving it here so i won't
	     forget and be tempted to put it back in :-)
	     if (j == section_fcLim)
	     {
	     wvHandleElement(ps, SECTIONEND, (void*)&sep,section_dirty);
	     section_pendingclose=0;
	     }
	   */
#endif
      }

 finish_processing:
    if (char_pendingclose)
      {
	  wvInitCHP (&achp);
	  wvHandleElement (ps, CHARPROPEND, (void *) &achp, char_dirty);
      }

    if (comment_pendingclose)
	wvHandleElement (ps, COMMENTEND, (void *) catrd, 0);

    if (para_pendingclose)
      {
	  wvInitPAP (&apap);
	  wvHandleElement (ps, PARAEND, (void *) &apap, para_dirty);
      }

    if (section_pendingclose)
	wvHandleElement (ps, SECTIONEND, (void *) &sep, section_dirty);

    wvFree (ps->fspa);
    wvFree (ps->fspapos);
    wvFree (ps->fdoa);
    wvFree (ps->fdoapos);

    wvFree (posBKL);
    wvFree (bkl);
    wvFree (posBKF);
    wvFree (bkf);
    wvFree (posAtrd);
    wvFree (atrd);

    wvReleasePAPX_FKP (&para_fkp);
    wvReleaseCHPX_FKP (&char_fkp);

    wvHandleDocument (ps, DOCEND);
    wvFree (posSedx);
    wvFree (sed);

    wvFree (ps->liststartnos);
    wvFree (ps->listnfcs);
    for (i = 0; i < 9 * ps->nolfo; i++)
	wvReleaseLVL (&(ps->finallvl[i]));
    wvFree (ps->finallvl);

    wvReleaseLST (&ps->lst, ps->noofLST);
    wvReleaseLFO_records (&ps->lfo, &ps->lfolvl, &ps->lvl, ps->nooflvl);
    wvReleaseSTTBF (&ps->anSttbfAssoc);

    wvFree (btePapx);
    wvFree (posPapx);
    wvFree (bteChpx);
    wvFree (posChpx);
    wvReleaseCLX (&ps->clx);
    wvReleaseFFN_STTBF (&ps->fonts);
    wvReleaseSTSH (&ps->stsh);
    wvReleaseSTTBF (&SttbfAtnbkmk);
    wvReleaseSTTBF (&grpXstAtnOwners);
    if (ps->vmerges)
      {
	  for (i = 0; i < ps->norows; i++)
	      wvFree (ps->vmerges[i]);
	  wvFree (ps->vmerges);
      }
    wvFree (ps->cellbounds);
	wvOLEFree(ps);
    tokenTreeFreeAll ();
}

/*
 The process thus far has created a SEP that describes what the section properties of 
 the section at the last full save. 

 1) Now apply any section sprms that were linked to the piece that contains the 
 section's section mark. 
 
 2) If pcd.prm.fComplex is 0, pcd.prm contains 1 sprm which should be applied to 
 the local SEP if it is a section sprm. 
 
 3) If pcd.prm.fComplex is 1, pcd.prm.igrpprl is the index of a grpprl in the CLX. 
 If that grpprl contains any section sprms, they should be applied to the local SEP
*/
int
wvGetComplexSEP (wvVersion ver, SEP * sep, U32 cpiece, STSH * stsh, CLX * clx)
{
    int ret = 0;
    U16 sprm, pos = 0, i = 0;
    U8 *pointer;
    U16 index;
    U8 val;
    Sprm RetSprm;

    if (clx->pcd[cpiece].prm.fComplex == 0)
      {
	  val = clx->pcd[cpiece].prm.para.var1.val;
	  pointer = &val;
#ifdef SPRMTEST
	  wvError (("singleton\n", clx->pcd[cpiece].prm.para.var1.isprm));
#endif
	  RetSprm =
	      wvApplySprmFromBucket (ver,
				     (U16) wvGetrgsprmPrm ( (U16) clx->pcd[cpiece].prm.
						     para.var1.isprm), NULL,
				     NULL, sep, stsh, pointer, &pos, NULL);
	  if (RetSprm.sgc == sgcSep)
	      ret = 1;
      }
    else
      {
	  index = clx->pcd[cpiece].prm.para.var2.igrpprl;
#ifdef SPRMTEST
	  fprintf (stderr, "\n");
	  while (i < clx->cbGrpprl[index])
	    {
		fprintf (stderr, "%x (%d)\n", *(clx->grpprl[index] + i),
			 *(clx->grpprl[index] + i));
		i++;
	    }
	  fprintf (stderr, "\n");
	  i = 0;
#endif
	  while (i < clx->cbGrpprl[index])
	    {
		if (ver == WORD8)
		    sprm = bread_16ubit (clx->grpprl[index] + i, &i);
		else
		  {
		      sprm = bread_8ubit (clx->grpprl[index] + i, &i);
		      sprm = (U8) wvGetrgsprmWord6 ( (U8) sprm);
		  }
		pointer = clx->grpprl[index] + i;
		RetSprm =
		    wvApplySprmFromBucket (ver, sprm, NULL, NULL, sep, stsh,
					   pointer, &i, NULL);
		if (RetSprm.sgc == sgcSep)
		    ret = 1;
	    }
      }
    return (ret);
}

/*
The process thus far has created a PAP that describes
what the paragraph properties of the paragraph were at the last full save.

1) Now it's necessary to apply any paragraph sprms that were linked to the
piece that contains the paragraph's paragraph mark. 

2) If pcd.prm.fComplex is 0, pcd.prm contains 1 sprm which should only be 
applied to the local PAP if it is a paragraph sprm. 

3) If pcd.prm.fComplex is 1, pcd.prm.igrpprl is the index of a grpprl in the 
CLX.  If that grpprl contains any paragraph sprms, they should be applied to 
the local PAP.
*/
int
wvAssembleComplexPAP (wvVersion ver, PAP * apap, U32 cpiece, wvParseStruct *ps)
{
    int ret = 0;
    U16 sprm, pos = 0, i = 0;
    U8 sprm8;
    U8 *pointer;
    U16 index;
    U8 val;
    Sprm RetSprm;

    if (ps->clx.pcd[cpiece].prm.fComplex == 0)
      {
	  val = ps->clx.pcd[cpiece].prm.para.var1.val;
	  pointer = &val;
#ifdef SPRMTEST
	  wvError (("singleton\n", ps->clx.pcd[cpiece].prm.para.var1.isprm));
#endif
	  RetSprm =
	      wvApplySprmFromBucket (ver,
				     (U16) wvGetrgsprmPrm ( (U16) ps->clx.pcd[cpiece].prm.
						     para.var1.isprm), apap,
				     NULL, NULL, &ps->stsh, pointer, &pos, ps->data);
	  if (RetSprm.sgc == sgcPara)
	      ret = 1;
      }
    else
      {
	  index = ps->clx.pcd[cpiece].prm.para.var2.igrpprl;
#ifdef SPRMTEST
	  wvError (("HERE-->\n"));
	  fprintf (stderr, "\n");
	  for (i = 0; i < ps->clx.cbGrpprl[index]; i++)
	      fprintf (stderr, "%x ", *(ps->clx.grpprl[index] + i));
	  fprintf (stderr, "\n");
	  i = 0;
#endif
	  while (i < ps->clx.cbGrpprl[index])
	    {
		if (ver == WORD8)
		    sprm = bread_16ubit (ps->clx.grpprl[index] + i, &i);
		else
		  {
		      sprm8 = bread_8ubit (ps->clx.grpprl[index] + i, &i);
		      sprm = (U16) wvGetrgsprmWord6 (sprm8);
		      wvTrace (("sprm is %x\n", sprm));
		  }
		pointer = ps->clx.grpprl[index] + i;
		RetSprm =
		    wvApplySprmFromBucket (ver, sprm, apap, NULL, NULL, &ps->stsh,
					   pointer, &i, ps->data);
		if (RetSprm.sgc == sgcPara)
		    ret = 1;
	    }
      }
    return (ret);
}

/* CHP version of the above. follows the same rules -JB */
int
wvAssembleComplexCHP (wvVersion ver, CHP * achp, U32 cpiece, STSH * stsh,
		      CLX * clx)
{
    int ret = 0;
    U16 sprm, pos = 0, i = 0;
    U8 sprm8;
    U8 *pointer;
    U16 index;
    U8 val;
    Sprm RetSprm;

    if (clx->pcd[cpiece].prm.fComplex == 0)
      {
	  val = clx->pcd[cpiece].prm.para.var1.val;
	  pointer = &val;
#ifdef SPRMTEST
	  wvError (("singleton %d\n", clx->pcd[cpiece].prm.para.var1.isprm));
#endif
	  RetSprm =
	      wvApplySprmFromBucket (ver,
				     (U16) wvGetrgsprmPrm ( (U16) clx->pcd[cpiece].prm.
						     para.var1.isprm), NULL,
				     achp, NULL, stsh, pointer, &pos, NULL);
	  if (RetSprm.sgc == sgcChp)
	      ret = 1;
      }
    else
      {
	  index = clx->pcd[cpiece].prm.para.var2.igrpprl;
#ifdef SPRMTEST
	  fprintf (stderr, "\n");
	  for (i = 0; i < clx->cbGrpprl[index]; i++)
	      fprintf (stderr, "%x ", *(clx->grpprl[index] + i));
	  fprintf (stderr, "\n");
	  i = 0;
#endif
	  while (i < clx->cbGrpprl[index])
	    {
		if (ver == WORD8)
		    sprm = bread_16ubit (clx->grpprl[index] + i, &i);
		else
		  {
		      sprm8 = bread_8ubit (clx->grpprl[index] + i, &i);
		      sprm = (U16) wvGetrgsprmWord6 (sprm8);
		  }
		pointer = clx->grpprl[index] + i;
		RetSprm =
		    wvApplySprmFromBucket (ver, sprm, NULL, achp, NULL, stsh,
					   pointer, &i, NULL);
		if (RetSprm.sgc == sgcChp)
		    ret = 1;
	    }
      }
    return (ret);
}
