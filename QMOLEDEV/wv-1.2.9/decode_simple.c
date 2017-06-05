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
#include "wvinternal.h"

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
wvDecodeSimple (wvParseStruct * ps, subdocument whichdoc)
{
    PAPX_FKP para_fkp;
    CHPX_FKP char_fkp;
    PAP apap;
    CHP achp;
    U32 piececount = 0, i, j = 0, spiece;
    U32 beginfc, endfc;
	U32 stream_size;
    U32 begincp, endcp;
    int ichartype;
    U8  chartype;
    U16 eachchar;
    U32 para_fcFirst, para_fcLim = 0xffffffff;
    U32 dummy, nextpara_fcLim = 0xffffffff;
    U32 char_fcFirst, char_fcLim = 0xffffffff;
    U32 section_fcFirst, section_fcLim = 0xffffffff;
    U32 comment_cpFirst = 0xffffffffL, comment_cpLim = 0xffffffffL;
    BTE *btePapx, *bteChpx;
    U32 *posPapx, *posChpx;
    U32 para_intervals, char_intervals, section_intervals, atrd_intervals;
    int para_pendingclose = 0, char_pendingclose = 0, section_pendingclose =
	0, comment_pendingclose = 0;
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
    FTXBXS *ftxbx;
    U32 *txbxTxt;
    U32 txbxTxt_intervals;
    BKD *bkd;
    U32 *posBKD;
    U32 bkd_intervals;
    wvVersion ver;

    external_wvReleasePAPX_FKP ();
    external_wvReleaseCHPX_FKP ();

    ver = wvQuerySupported (&ps->fib, NULL);


    /* 
       despite what some parts of the spec might have you believe you still need to 
       get the piecetable from even simple files, some simple files can have 8bit
       chars in one part, and 16bit chars in another, so you have to watch out for
       that
     */
    wvGetCLX (ver, &ps->clx, ps->fib.fcClx, (U32) ps->fib.lcbClx,
	      (U8) ps->fib.fExtChar, ps->tablefd);
    /* for word 6 and just in case */
    if (ps->clx.nopcd == 0)
	wvBuildCLXForSimple6 (&ps->clx, &ps->fib);

    para_fcFirst = char_fcFirst = section_fcFirst = wvGetBeginFC (ps, whichdoc);
    /*we will need the stylesheet to do anything useful with layout and look */
    wvGetSTSH (&ps->stsh, ps->fib.fcStshf, ps->fib.lcbStshf, ps->tablefd);

    /*dop */
    wvGetDOP (ver, &ps->dop, ps->fib.fcDop, ps->fib.lcbDop, ps->tablefd);
    wvTrace (("tabstops are every %d twips\n", ps->dop.dxaTab));

    /*textbox information */
    wvGetFTXBXS_PLCF (&ftxbx, &txbxTxt, &txbxTxt_intervals,
		      ps->fib.fcPlcftxbxTxt, ps->fib.lcbPlcftxbxTxt,
		      ps->tablefd);
    wvGetBKD_PLCF (&bkd, &posBKD, &bkd_intervals, ps->fib.fcPlcftxbxBkd,
		   ps->fib.lcbPlcftxbxBkd, ps->tablefd);


    /* this mountain of informatio is just to get comments organized */
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
           ps->fib.lcbPlcfAtnbkl,ps->fib.fcPlcfAtnbkf, ps->fib.lcbPlcfAtnbkf,
           ps->tablefd);


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
    else			/*word 97 */
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



    /*
       we will need the paragraph and character bounds table to make decisions as 
       to where a para/char run begins and ends
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
    else			/* word 97 */
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

    /*
       The text of the file starts at fib.fcMin, but we will use the piecetable 
       records rather than this basic seek.
       fseek(ps->mainfd,ps->fib.fcMin,SEEK_SET);
     */

    /*
       If !fib.fComplex, the document text stream is represented by the text
       beginning at fib.fcMin up to (but not including) fib.fcMac.
     */

    if ((ver == WORD2) && !ps->fib.fComplex)
      {
	  wvHandleDocument (ps, DOCBEGIN);
	  wvStream_goto (ps->mainfd, ps->fib.fcMin);
	  for (i = ps->fib.fcMin; i < ps->fib.fcMac; i++)
	    {
		eachchar = wvGetChar (ps->mainfd, 1);
		(*(ps->charhandler)) (ps, eachchar, 1, ps->fib.lid);
		/* (*(ps->scharhandler))(ps,eachchar,&achp;  no go */
		/* wvOutputTextChar(eachchar, 1, ps, &achp); no go */
		/* Formatting still lacking. This is just a start. */
	    }
	  wvHandleDocument (ps, DOCEND);
	  wvReleaseSTTBF (&ps->anSttbfAssoc);
	  wvReleaseSTTBF (&ps->Sttbfbkmk);
	  wvFree (posChpx);
	  wvFree (bteChpx);
	  wvFree (btePapx);
	  wvReleaseCLX (&ps->clx);
	  wvReleaseSTSH (&ps->stsh);
	  return;
      }


#ifdef DEBUG
    if (ps->fib.fcMac != wvGetEndFCPiece (ps->clx.nopcd - 1, &ps->clx))
	wvTrace (
		 ("fcMac is not the same as the piecetable %x %x!\n",
		  ps->fib.fcMac, wvGetEndFCPiece (ps->clx.nopcd - 1,
						  &ps->clx)));
#endif

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

	  wvTrace (("SEEK %x\n", beginfc));

	  /*lvm007@aha.ru fix antiloop fix*/
	  if(wvGetPieceBoundsCP (&begincp, &endcp, &ps->clx, piececount)==-1)
		  break;

	  /*
	     text that is not in the same piece is not guaranteed to have the same properties as
	     the rest of the exception run, so force a stop and restart of these properties.
	   */
	  char_fcLim = beginfc;
	  wvTrace (("%d %d %d\n", begincp, endcp, ps->fib.ccpText));
	  for (i = begincp, j = beginfc; (i < endcp /*&& i<ps->fib.ccpText */ );
	       i++, j += wvIncFC (chartype))
	    {
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

		if (j == section_fcLim)
		  {
		      wvHandleElement (ps, SECTIONEND, (void *) &sep,
				       section_dirty);
		      section_pendingclose = 0;
		  }

		if ((section_fcLim == 0xffffffff) || (section_fcLim == j))
		  {
		      wvTrace (("j i is %x %d\n", j, i));
		      section_dirty =
			  wvGetSimpleSectionBounds (ver, ps,
						    &sep, &section_fcFirst,
						    &section_fcLim, i,
						    &ps->clx, sed, &spiece,
						    posSedx,
						    section_intervals,
						    &ps->stsh, ps->mainfd);
		      wvTrace (
			       ("section begins at %x ends %x\n",
				section_fcFirst, section_fcLim));
		  }

		if (j == section_fcFirst)
		  {
		      wvHandleElement (ps, SECTIONBEGIN, (void *) &sep,
				       section_dirty);
		      section_pendingclose = 1;
		  }

		if ((para_fcLim == 0xffffffff) || (para_fcLim == j))
		  {
		      wvReleasePAPX_FKP (&para_fkp);
		      wvGetSimpleParaBounds (ver, &para_fkp,
					     &para_fcFirst, &para_fcLim,
					     wvConvertCPToFC (i, &ps->clx),
					     btePapx, posPapx, para_intervals,
					     ps->mainfd);
		      wvTrace (
			       ("Para from %x to %x, j is %x\n", para_fcFirst,
				para_fcLim, j));

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

		      /* test section */
		      wvReleasePAPX_FKP (&para_fkp);
		      wvGetSimpleParaBounds (ver, &para_fkp,
					     &dummy, &nextpara_fcLim,
					     para_fcLim, btePapx, posPapx,
					     para_intervals, ps->mainfd);
		      wvAssembleSimplePAP (ver, &ps->nextpap, nextpara_fcLim, &para_fkp, ps);
		      /* end test section */

		      if ((apap.fInTable) && (!apap.fTtp))
			{
			    wvGetFullTableInit (ps, para_intervals, btePapx,
						posPapx);
			    wvGetRowTap (ps, &apap, para_intervals, btePapx,
					 posPapx);
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


		if ((char_fcLim == 0xffffffff) || (char_fcLim == j))
		  {
		      wvTrace (("j i is %x %d\n", j, i));
		      wvReleaseCHPX_FKP (&char_fkp);
		      wvGetSimpleCharBounds (ver, &char_fkp,
					     &char_fcFirst, &char_fcLim, i,
					     &ps->clx, bteChpx, posChpx,
					     char_intervals, ps->mainfd);
		      wvTrace (
			       ("char begins at %x ends %x, j is %x\n",
				char_fcFirst, char_fcLim, j));
		      if (0 == char_pendingclose)
			{
			    /*
			       if there's no character run open, but there should be then I believe that the fcFirst search
			       has failed me, so I set it to now. I need to investigate this further. 
			     */
			    if (j != char_fcFirst)
			      {
				  wvWarning (
					     ("There is no character run due to open but one should be, plugging the gap.\n"));
				  char_fcFirst = j;
			      }

			}
		      else{
  			/* lvm007@aha.ru fix: if currentfc>fcFirst but CHARPROP's changed look examples/charprops.doc*/
			if(char_fcFirst< j)
				char_fcFirst = j;
		       }
		  }

		if (j == char_fcFirst)
		  {
		      wvTrace (("assembling CHP...\n"));
		      /* a CHP's base style is in the para style */
		      /* achp.istd = apap.istd; */
		      char_dirty =
				  wvAssembleSimpleCHP (ver, &achp, &apap,
					       char_fcLim, &char_fkp,
					       &ps->stsh);
		      wvTrace (("CHP assembled.\n"));
		      wvTrace (("font is %d\n", achp.ftcAscii));
		      wvTrace (("char spec is %d\n", achp.ftcSym));
		      wvHandleElement (ps, CHARPROPBEGIN, (void *) &achp,
				       char_dirty);
		      wvTrace (("char lid is %x\n", achp.lidDefault));
		      char_pendingclose = 1;
		  }

		eachchar = wvGetChar (ps->mainfd, chartype);

		if ((eachchar == 0x07) && (!achp.fSpec))
		    ps->endcell = 1;

		ps->currentcp = i;
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

    internal_wvReleasePAPX_FKP (&para_fkp);
    internal_wvReleaseCHPX_FKP (&char_fkp);
    wvHandleDocument (ps, DOCEND);
    wvFree (posSedx);
    wvFree (sed);
    wvFree(bkd);
    wvFree(posBKD);
    wvFree(ftxbx);
    wvFree(txbxTxt); 
#if defined(WIN32)
   wvReleasePAPX_FKP (&para_fkp);
   wvReleaseCHPX_FKP (&char_fkp);   

   external_wvReleasePAPX_FKP ();
   external_wvReleaseCHPX_FKP();
#endif


    wvFree (ps->liststartnos);
    wvFree (ps->listnfcs);
    for (i = 0; i < 9 * ps->nolfo; i++)
	wvReleaseLVL (&(ps->finallvl[i]));
    wvFree (ps->finallvl);

    wvReleaseLST (&ps->lst, ps->noofLST);
    wvReleaseLFO_records (&ps->lfo, &ps->lfolvl, &ps->lvl, ps->nooflvl);
    wvReleaseSTTBF (&ps->anSttbfAssoc);
    wvReleaseSTTBF (&ps->Sttbfbkmk);
    wvFree (btePapx);
    wvFree (posPapx);
    wvFree (bteChpx);
    wvFree (posChpx);
    wvFree (bkd);
    wvFree (posBKD);
    wvFree (txbxTxt);
    wvFree (ftxbx);
#if 0
    /* 
       so what, this is meaningless 
       C. 
     */
    if (ps->fib.fcMac != ftell (ps->mainfd))
	wvError (("fcMac did not match end of input !\n"));
#endif
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
When a document is recorded in non-complex format, the bounds of the
paragraph that contains a particular character can be found by 

1) calculating the FC coordinate of the character, 

2) searching the bin table to find an FKP page that describes that FC, 

3) fetching that FKP, and 

4) then searching the FKP to find the interval in the rgfc that encloses the character. 

5) The bounds of the interval are the fcFirst and fcLim of the containing paragraph. 

Every character greater than or equal to fcFirst and less than fcLim is part of
the containing paragraph.

*/
int
wvGetSimpleParaBounds (wvVersion ver, PAPX_FKP * fkp, U32 * fcFirst,
		       U32 * fcLim, U32 currentfc, BTE * bte, U32 * pos,
		       int nobte, wvStream * fd)
{
    BTE entry;
    long currentpos;

    /*
       currentfc = wvConvertCPToFC(currentcp,clx);
     */

    wvTrace (("currentfc is %x\n", currentfc));
    if (currentfc == 0xffffffffL)
      {
	  wvError (("Para Bounds not found !\n"));
	  return (1);
      }

    if (0 != wvGetBTE_FromFC (&entry, currentfc, bte, pos, nobte))
      {
	  wvError (("BTE not found !\n"));
	  return (1);
      }
    currentpos = wvStream_tell (fd);
    /*The pagenumber of the FKP is entry.pn */

    wvTrace (("pn is %d\n", entry.pn));
    wvGetPAPX_FKP (ver, fkp, entry.pn, fd);
    wvTrace (("last entry is %x\n", fkp->rgfc[fkp->crun]));
    while (fkp->rgfc[fkp->crun] <= currentfc)
      {
	  if ((fkp->rgfc[fkp->crun] == currentfc) && (currentfc == pos[nobte]))
	      break;
	  
	  /* Bad things man... */
	  wvError (("Alert, insane repeat \"insane\" paragraph structure,"
		    "making wild stab in the dark to attempt to continue\n"));
	  wvReleasePAPX_FKP (fkp);
	  entry.pn++;
	  wvGetPAPX_FKP (ver, fkp, entry.pn, fd);
      }

    wvStream_goto (fd, currentpos);

    return (wvGetIntervalBounds
	    (fcFirst, fcLim, currentfc, fkp->rgfc, fkp->crun + 1));
}

int
wvGetSimpleCharBounds (wvVersion ver, CHPX_FKP * fkp, U32 * fcFirst,
		       U32 * fcLim, U32 currentcp, CLX * clx, BTE * bte,
		       U32 * pos, int nobte, wvStream * fd)
{
    U32 currentfc;
    BTE entry;
    long currentpos;

    currentfc = wvConvertCPToFC (currentcp, clx);

    if (currentfc == 0xffffffffL)
      {
	  wvError (("Char Bounds not found !\n"));
	  return (1);
      }

    wvTrace (("char fc is %x\n", currentfc));

    if (0 != wvGetBTE_FromFC (&entry, currentfc, bte, pos, nobte))
      {
	  wvError (("BTE not found !\n"));
	  return (1);
      }
    currentpos = wvStream_tell (fd);
    /*The pagenumber of the FKP is entry.pn */

    wvTrace (("pn is %d\n", entry.pn));
    wvGetCHPX_FKP (ver, fkp, entry.pn, fd);

    while (fkp->rgfc[fkp->crun] <= currentfc)
      {
	  if ((fkp->rgfc[fkp->crun] == currentfc) && (currentfc == pos[nobte]))
	      break;

	  /* Bad things man... */
	  wvError (("Alert, insane repeat \"insane\" character run structure,"
		    "making wild stab in the dark to attempt to continue\n"));
	  wvReleaseCHPX_FKP (fkp);
	  entry.pn++;
	  wvGetCHPX_FKP (ver, fkp, entry.pn, fd);
      }

    wvStream_goto (fd, currentpos);

    return (wvGetIntervalBounds
	    (fcFirst, fcLim, currentfc, fkp->rgfc, fkp->crun + 1));
}

int
wvGetIntervalBounds (U32 * fcFirst, U32 * fcLim, U32 currentfc, U32 * rgfc,
		     U32 nopos)
{
    U32 i = 0;
    while (i < nopos - 1)
      {
	  wvTrace (
		   ("searching...%x %x %x\n", currentfc,
		    wvNormFC (rgfc[i], NULL), wvNormFC (rgfc[i + 1], NULL)));
	  /*
	     if ( (wvNormFC(rgfc[i],NULL) >= currentfc) && (currentfc <= wvNormFC(rgfc[i+1],NULL)) )
	   */
	  if ((currentfc >= wvNormFC (rgfc[i], NULL))
	      && (currentfc < wvNormFC (rgfc[i + 1], NULL)))
	    {
		*fcFirst = wvNormFC (rgfc[i], NULL);
		*fcLim = wvNormFC (rgfc[i + 1], NULL);
		return (0);
	    }
	  i++;
      }
    *fcFirst = wvNormFC (rgfc[nopos - 2], NULL);
    *fcLim = wvNormFC (rgfc[nopos - 1], NULL);
    wvTrace (("I'd rather not see this happen at all :-)\n"));
    return (0);
}

/*
it is necessary to use the CP of the character to search the
plcfsed for the index i of the largest CP that is less than or equal to the
character's CP. 

plcfsed.rgcp[i] is the CP of the first character of the
section and plcfsed.rgcp[i+1] is the CP of the character following the
section mark that terminates the section (call it cpLim). 

Then retrieve plcfsed.rgsed[i]. The FC in this SED gives the location where the SEPX for
the section is stored. 

Then create a local SEP with default section properties. If the 
sed.fc != 0xFFFFFFFF, then the sprms within the SEPX that is stored at offset 
sed.fc must be applied to the local SEP. The process thus far has created a 
SEP that describes what the section properties of the section at the last 
full save. 
*/
int
wvGetSimpleSectionBounds (wvVersion ver, wvParseStruct * ps, SEP * sep,
			  U32 * fcFirst, U32 * fcLim, U32 cp, CLX * clx,
			  SED * sed, U32 * spiece, U32 * posSedx,
			  U32 section_intervals, STSH * stsh, wvStream * fd)
{
    U32 i = 0;
    int ret = 0;
    SEPX sepx;
    long pos = wvStream_tell (fd);
    U32 cpTest = 0, j, dummy;

    if (section_intervals == 0)
      {
	  wvGetPieceBoundsFC (fcFirst, &dummy, &ps->clx, 0);
	  wvGetPieceBoundsFC (&dummy, fcLim, &ps->clx, ps->clx.nopcd);
	  return (0);
      }

    j = section_intervals - 1;

    if (cp == 0)
	j = 0;
    while (i < section_intervals)
      {
	  wvTrace (("searching for sep %d %d\n", posSedx[i], cp));
	  if ((posSedx[i] <= cp) && (posSedx[i] > cpTest))
	    {
		cpTest = posSedx[i];
		j = i;
		*spiece = wvGetPieceFromCP (cpTest, clx);
	    }
	  i++;
      }

    wvTrace (("found at %d %d\n", posSedx[j], posSedx[j + 1]));
    *fcFirst = wvConvertCPToFC (posSedx[j], clx);
    *fcLim = wvConvertCPToFC (posSedx[j + 1], clx);
    wvTrace (("found at %x %x\n", *fcFirst, *fcLim));

    wvInitSEP (sep);

    if (sed[j].fcSepx != 0xffffffffL)
      {
	  wvStream_goto (fd, wvNormFC (sed[j].fcSepx, NULL));
	  wvGetSEPX (ver, &sepx, fd);
	  if (ver == WORD8)
	      ret = wvAddSEPXFromBucket (sep, &sepx, stsh);
	  else
	      ret = wvAddSEPXFromBucket6 (sep, &sepx, stsh);
	  wvReleaseSEPX (&sepx);
      }

    wvStream_goto (fd, pos);
    return (ret);
}

U32
wvGetBeginFC (wvParseStruct * ps, subdocument whichdoc)
{
    U32 para_fcFirst = 0x400;
    switch (whichdoc)
      {
      case Dmain:
      default:
	  para_fcFirst = wvConvertCPToFC (0, &ps->clx);
	  break;
      case Dfootnote:
	  para_fcFirst = wvConvertCPToFC (ps->fib.ccpText, &ps->clx);
	  break;
      case Dheader:
	  para_fcFirst = wvConvertCPToFC (ps->fib.ccpText + ps->fib.ccpFtn,
					  &ps->clx);
	  break;
      case Dannotation:
	  para_fcFirst = wvConvertCPToFC (ps->fib.ccpText + ps->fib.ccpFtn +
					  ps->fib.ccpHdr, &ps->clx);
	  break;
      case Dendnote:
	  para_fcFirst = wvConvertCPToFC (ps->fib.ccpText + ps->fib.ccpFtn +
					  ps->fib.ccpHdr + ps->fib.ccpAtn,
					  &ps->clx);
	  break;
      case Dtextbox:
	  para_fcFirst = wvConvertCPToFC (ps->fib.ccpText + ps->fib.ccpFtn +
					  ps->fib.ccpHdr + ps->fib.ccpAtn +
					  ps->fib.ccpEdn, &ps->clx);
	  break;
      case Dheader_textbox:
	  para_fcFirst = wvConvertCPToFC (ps->fib.ccpText + ps->fib.ccpFtn +
					  ps->fib.ccpHdr + ps->fib.ccpAtn +
					  ps->fib.ccpEdn + ps->fib.ccpTxbx,
					  &ps->clx);
	  break;
      }
    return (para_fcFirst);
}
