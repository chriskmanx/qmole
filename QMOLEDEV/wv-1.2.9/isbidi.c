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
    This file contains implementation of wvIsBidiDocument() which can
    be used to determine if the document requires any bidi
    support. Please note, that for the function to work, the
    wvParseStruct must be already initialized; basically you should
    call this function from inside of your document callback procedure.

    As there is no simple way to determine this, we crawl over all the
    formatting records (section, paragraph, character) and look for
    the fBidi attribute being set.

    The bulk of the code was ripped from decode_simple.c and
    decode_complex.c, and stripped of the unnecessary bits and pieces.
    
    Tomas <tomas@frydrych.uklinux.net>, Feb 1, 2003
*/

/* function prototypes */
int wvIsBidiDocumentSimple(wvParseStruct * ps, subdocument whichdoc);
int wvIsBidiDocumentComplex(wvParseStruct * ps);

int wvGetComplexParaBounds (wvVersion ver, PAPX_FKP * fkp, U32 * fcFirst,
							U32 * fcLim, U32 currentfc, CLX * clx, BTE * bte,
							U32 * pos, int nobte, U32 piece, wvStream * fd);

int wvGetComplexCharBounds (wvVersion ver, CHPX_FKP * fkp, U32 * fcFirst,
							U32 * fcLim, U32 currentfc, CLX * clx, BTE * bte,
							U32 * pos, int nobte, U32 piece, wvStream * fd);

void internal_wvReleasePAPX_FKP (PAPX_FKP * fkp);
void internal_wvReleaseCHPX_FKP (CHPX_FKP * fkp);

int wvIsBidiDocument(wvParseStruct * ps)
{
    if (ps->fib.fComplex)
		return wvIsBidiDocumentComplex (ps);
    else
		return wvIsBidiDocumentSimple (ps, Dmain);
}

int wvIsBidiDocumentComplex (wvParseStruct * ps)
{
	int ret = 0;
    U32 piececount = 0, i, j, spiece = 0;
    U32 beginfc, endfc;
	U32 stream_size;
    U32 begincp, endcp;
    int ichartype;
    U8  chartype;
    U32 para_fcFirst, para_fcLim = 0xffffffffL;
    U32 nextpara_fcLim = 0xffffffffL;
    U32 char_fcFirst, char_fcLim = 0xffffffffL;
    U32 section_fcFirst, section_fcLim = 0xffffffffL;
    BTE *btePapx = NULL, *bteChpx = NULL;
    U32 *posPapx = NULL, *posChpx = NULL;
    U32 para_intervals, char_intervals, section_intervals;
    int cpiece = 0, npiece = 0;
    PAPX_FKP para_fkp;
    PAP apap;
    CHPX_FKP char_fkp;
    CHP achp;
    int para_dirty = 0, char_dirty = 0, section_dirty = 0;
    SED *sed = NULL;
    SEP sep;
    U32 *posSedx = NULL;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);

	external_wvReleasePAPX_FKP ();
    external_wvReleaseCHPX_FKP ();

    para_fcFirst = char_fcFirst = section_fcFirst =
		wvConvertCPToFC (0, &ps->clx);
	

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

				if(sep.fBidi)
				{
					ret = 1;
					goto complex_return;
				}
				
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
			}

			if (j == para_fcFirst)
			{
				para_dirty =
					wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);
				para_dirty =
					(wvAssembleComplexPAP
					 (ver, &apap, cpiece,ps) ? 1 : para_dirty);
#if 0
				/* not sure what this is about, Tomas*/
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
					wvAssembleSimplePAP (ver, &ps->nextpap, nextpara_fcLim,
										 &para_fkp, &ps->stsh,
										 ps->data);
					wvAssembleComplexPAP (ver, &ps->nextpap, npiece,
										  &ps->stsh, &ps->clx,
										  ps->data);
				}
				else
					wvInitPAP (&ps->nextpap);
				/* end test section */
#endif
				if(apap.fBidi)
				{
					ret = 1;
					goto complex_return;
				}
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
			}

			if (j == char_fcFirst)
			{
				/* a CHP's base style is in the para style */
				/* achp.istd = apap.istd; */
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

				if(achp.fBidi)
				{
					ret = 1;
					goto complex_return;
				}
			}
	    }
	}

 complex_return:
    wvReleasePAPX_FKP (&para_fkp);
    wvReleaseCHPX_FKP (&char_fkp);

    wvFree (posSedx);
    wvFree (sed);

    wvFree (btePapx);
    wvFree (posPapx);
    wvFree (bteChpx);
    wvFree (posChpx);
	return ret;
}

int wvIsBidiDocumentSimple(wvParseStruct * ps, subdocument whichdoc)
{
	int ret = 0;
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
    U32 para_fcFirst, para_fcLim = 0xffffffff;
    U32 /*dummy,*/ nextpara_fcLim = 0xffffffff;
    U32 char_fcFirst, char_fcLim = 0xffffffff;
    U32 section_fcFirst, section_fcLim = 0xffffffff;
    BTE *btePapx = NULL, *bteChpx = NULL;
    U32 *posPapx = NULL, *posChpx = NULL;
    U32 para_intervals, char_intervals, section_intervals;
    int para_dirty = 0, char_dirty = 0, section_dirty = 0;
    SED *sed = NULL;
    SEP sep;
    U32 *posSedx = NULL;
    wvVersion ver;

    external_wvReleasePAPX_FKP ();
    external_wvReleaseCHPX_FKP ();

    ver = wvQuerySupported (&ps->fib, NULL);

    para_fcFirst = char_fcFirst = section_fcFirst = wvGetBeginFC (ps, whichdoc);
	

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
		/* the formatting handler is not implemented, assume LTR
		   (there will not be many of these around anyway)*/ 
		goto simple_return;
	}

    wvInitPAPX_FKP (&para_fkp);
    wvInitCHPX_FKP (&char_fkp);

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
			if ((section_fcLim == 0xffffffff) || (section_fcLim == j))
			{
				wvTrace (("j i is %x %d\n", j, i));
				/* the following call inits sep */
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
				if(sep.fBidi)
				{
					ret = 1;
					goto simple_return;
				}
				
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

			}

			if (j == para_fcFirst)
			{
				para_dirty =
					wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);

				if(apap.fBidi)
				{
					ret = 1;
					goto simple_return;
				}
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

				if(achp.fBidi)
				{
					ret = 1;
					goto simple_return;
				}
			}
	    }
	}

 simple_return:
    internal_wvReleasePAPX_FKP (&para_fkp);
    internal_wvReleaseCHPX_FKP (&char_fkp);
    wvFree (posSedx);
    wvFree (sed);
#if defined(WIN32)
	wvReleasePAPX_FKP (&para_fkp);
	wvReleaseCHPX_FKP (&char_fkp);   

	external_wvReleasePAPX_FKP ();
	external_wvReleaseCHPX_FKP();
#endif

    wvFree (btePapx);
    wvFree (posPapx);
    wvFree (bteChpx);
    wvFree (posChpx);

	return ret;
}
