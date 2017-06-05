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
#include "wv.h"

int
wvIsListEntry (PAP * apap, wvVersion ver)
{
    if (ver == WORD8)
      {
	  if (apap->ilfo)
	      return (1);
	  else
	      return (0);
      }
    else
      {
	  if (apap->nLvlAnm)
	    {
		wvTrace (("old style pap, thats a list\n"));
		return (1);
	    }
	  else
	      return (0);
      }
    return (0);
}

/*
Paragraph List Formatting

Given a paragraph and its corresponding PAP, the following process must be
followed to find out the paragraph's list information:

   * Using the pap.ilfo, look up the LFO record in the pllfo with that
     (1-based) index.

   * Using the LFO, and the pap.ilvl, check to see if there are any
     overrides for this particular level. If so, and if the override
     pertains to both formatting and start-at value, use the LVL record from
     the correct LFOLVL in the LFO, and skip to step 5.

   * If the override does not pertain to either formatting or start-at
     value, we must look up the LST for this list. Using the LFO's List ID,
     search the rglst for the LST with that List ID.

   * Now, take from this LST any information (formatting or start-at value)
     we still need after consulting the LFO.

   * Once we've got the correct LVL record, apply the lvl.grpprlPapx to the
     PAP. It may adjust the indents and tab settings for the paragraph.

   * Use the other information in the LVL, such as the start at, number
     text, and grpprlChpx, to determine the appearance of the actual
     paragraph number text.
*/
int
wvGetListEntryInfo (wvVersion ver, LVL ** finallvl, U32 ** nos, U8 ** nfcs,
		    LVL * retlvl, LFO ** retlfo, PAP * apap, LFO ** lfo,
		    LFOLVL * lfolvl, LVL * lvl, U32 * nolfo, LST ** lst,
		    U16 * noofLST)
{
    LST *alst = NULL;
    U32 i, number = 0;
    S32 j;
    U32 oldno;
    U32 fakeid;

    wvTrace (("given ilfo of %d %d\n", apap->ilfo, apap->ilvl));
    if (apap->ilfo < 0)
      {
	  apap->ilfo = abs (apap->ilfo);
	  wvWarning
	      ("Insane negative ilfo value, normalizing to %d and hoping for the best\n",
	       apap->ilfo);
      }

    if ((apap->ilfo == 2047) || (ver != WORD8))
      {
	  retlvl->lvlf.iStartAt = apap->anld.iStartAt;
	  retlvl->lvlf.nfc = apap->anld.nfc;
	  wvTrace (
		   ("start %d,type is %d\n", apap->anld.iStartAt,
		    apap->anld.nfc));
	  retlvl->lvlf.jc = apap->anld.jc;
	  retlvl->lvlf.fLegal = 0;	/*? */
	  retlvl->lvlf.fNoRestart = 0;	/*? */
	  retlvl->lvlf.fPrev = apap->anld.fPrev;
	  retlvl->lvlf.fPrevSpace = apap->anld.fPrevSpace;
	  retlvl->lvlf.fWord6 = 1;
	  retlvl->lvlf.rgbxchNums[0] = 0;	/*wrong for now */
	  retlvl->lvlf.ixchFollow = 2;	/*wrong for now */
	  retlvl->lvlf.dxaSpace = apap->anld.dxaSpace;
	  retlvl->lvlf.dxaIndent = apap->anld.dxaIndent;
	  retlvl->lvlf.cbGrpprlChpx = 0;	/* wrong */
	  retlvl->lvlf.cbGrpprlPapx = 0;	/* wrong */
	  retlvl->lvlf.reserved1 = 0;
	  retlvl->lvlf.reserved2 = 0;
	  retlvl->grpprlChpx = NULL;	/* wrong */
	  retlvl->grpprlPapx = NULL;	/* wrong */



	  /* wrong: begin of numbertext twiddling */
	  wvTrace (("before len %d\n", apap->anld.cxchTextBefore));
	  wvTrace (("after len %d\n", apap->anld.cxchTextAfter));
	  retlvl->numbertext = (XCHAR *) wvMalloc (sizeof (XCHAR) * 64);
	  i = 0;
	  for (; i < apap->anld.cxchTextBefore; i++)
	      retlvl->numbertext[i] = apap->anld.rgxch[i];

	  retlvl->numbertext[i] = 2;

	  for (i = apap->anld.cxchTextBefore; i < apap->anld.cxchTextAfter; i++)
	      retlvl->numbertext[i + 1] = apap->anld.rgxch[i];

	  retlvl->numbertext[i + 1] = '\0';
	  /* end of numbertext twiddling */


	  /* temp test */
	  if (retlvl->lvlf.nfc > 5)
	      retlvl->numbertext[0] = 0;


	  /*word 6 anld, parse that instead */
	  fakeid = wvCheckSumANLD (&apap->anld);
	  wvTrace (("creating a fake id of %x\n", fakeid));
	  for (i = 0; i < *nolfo; i++)
	    {
		if (fakeid == (*lfo)[i].lsid)
		  {
		      wvTrace (
			       ("This is not the first time we've seen this list\n"));
		      apap->ilfo = i + 1;

		      if (apap->nLvlAnm >= 10)
			  apap->nLvlAnm -= 10;

		      if ((apap->nLvlAnm == 1) || (apap->nLvlAnm == 0))
			  apap->ilvl = 0;
		      else
			  apap->ilvl = apap->nLvlAnm - 1;

		      if (apap->ilvl >= 10)
			  apap->ilvl -= 10;

		      for (i = 0; i < 9; i++)
			  (*nos)[(apap->ilfo - 1) * 9 + i] = 0xffffffffL;
		      for (i = 0; i < 9; i++)
			  (*nfcs)[(apap->ilfo - 1) * 9 + i] = 0xff;

		      wvTrace (("ilvl %d\n", apap->ilvl));


		      /* if this anld is a dodgy one 0x2e */
		      if ((apap->ilvl) && (apap->anld.fNumber1 == 0x2e))
			{
			    wvTrace (("Suspicious\n"));
			    switch (apap->ilvl)
			      {
			      case 1:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 4;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 3;
				  break;
			      case 2:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 2;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 0;
				  break;
			      case 3:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 4;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 4;
				  break;
			      case 4:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 0;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 0;
				  break;
			      case 5:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 4;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 4;
				  break;
			      case 6:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 2;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 2;
				  break;
			      case 7:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 4;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 4;
				  break;
			      case 8:
				  if (retlvl->lvlf.nfc == 0)
				      retlvl->lvlf.nfc = 2;
				  else if (retlvl->lvlf.nfc == 1)
				      retlvl->lvlf.nfc = 2;
				  break;
			      }
			}
		      return (0);
		  }
	    }

	  wvTrace (("This is the first time we've seen this list\n"));

	  oldno = *nolfo;
	  (*nolfo)++;

	  /*
	     realloc the lfo list to be one bigger, 
	   */
	  *lfo = (LFO *) realloc (*lfo, sizeof (LFO) * (*nolfo));
	  *nos = (U32 *) realloc (*nos, sizeof (U32) * 9 * (*nolfo));
	  *nfcs = (U8 *) realloc (*nfcs, 9 * (*nolfo));
	  wvTrace (("nos is now %d long\n", 9 * (*nolfo)));
	  *finallvl = (LVL *) realloc (*finallvl, 9 * (*nolfo) * sizeof (LVL));

	  apap->ilfo = *nolfo;

	  wvTrace (("ilvl is %d, nLvlAnm is %d\n", apap->ilvl, apap->nLvlAnm));
	  if (apap->nLvlAnm >= 10)
	      apap->nLvlAnm -= 10;

	  if ((apap->nLvlAnm == 1) || (apap->nLvlAnm == 0))
	      apap->ilvl = 0;
	  else
	      apap->ilvl = apap->nLvlAnm - 1;

	  wvTrace (("ilfo set to %d\n", apap->ilfo));

	  /* begin new test */
	  (*noofLST)++;
	  *lst = (LST *) realloc (*lst, sizeof (LST) * (*noofLST));
	  wvInitLST (&(((*lst)[(*noofLST) - 1])));
	  (*lst)[(*noofLST) - 1].lstf.lsid = fakeid;
	  wvTrace (("ilvl is %d\n", apap->ilvl));
	  wvCopyLVL (&(((*lst)[(*noofLST) - 1]).lvl[apap->ilvl]), retlvl);
	  /* end new test */
	  wvTrace (("End\n"));


	  wvInitLFO (&((*lfo)[apap->ilfo - 1]));
	  (*lfo)[apap->ilfo - 1].lsid = fakeid;	/*how about this? */
	  *retlfo = &((*lfo)[apap->ilfo - 1]);
	  for (i = 0; i < 9; i++)
	    {
		(*nos)[(apap->ilfo - 1) * 9 + i] = 0xffffffffL;
		(*nfcs)[(apap->ilfo - 1) * 9 + i] = 0xff;
		wvInitLVL (&((*finallvl)[(apap->ilfo - 1) * 9 + i]));
		wvCopyLVL (&((*finallvl)[(apap->ilfo - 1) * 9 + i]), retlvl);
	    }


	  /*
	     and set the ilfo and ilvl of the list to point to that fake entry instead
	     and we'll have to repeat the procedure for the liststartnos
	   */
	  return (0);
      }
    else if (apap->ilfo == 0)
      {
	  /* no number */
	  return (0);
      }
    if (apap->ilfo > (S32) (*nolfo))
      {
	  wvWarning
	      ("ilfo no %d, is greater than the number of existing lfo's (%d)\n",
	       apap->ilfo, *nolfo);
	  return (1);
      }

    /*
       Using the pap.ilfo, look up the LFO record in the pllfo with that
       (1-based) index. == (*lfo)[apap->ilfo]
     */

    *retlfo = &((*lfo)[apap->ilfo - 1]);

    wvTrace (("looking for id %x\n", (*retlfo)->lsid));

    if ((*lfo)[apap->ilfo - 1].clfolvl)
      {
	  /* 
	     Using the LFO, and the pap.ilvl, check to see if there are any 
	     overrides for this particular level. If so, and if the override
	     pertains to both formatting and start-at value, use the LVL record 
	     from the correct LFOLVL in the LFO, and skip to step 5.
	   */
	  wvTrace (("some overridden levels, ilfo %d\n", apap->ilfo));

	  /* 
	     there are some levels overridden, find out if the level being overridden
	     is the same as the level the paragraph wants
	   */
	  for (j = 0; j < apap->ilfo - 1; j++)
	      number += (*lfo)[j].clfolvl;

	  for (i = 0; i < (*lfo)[apap->ilfo - 1].clfolvl; i++)
	    {
		if (lfolvl[i + number].ilvl == apap->ilvl)
		  {
		      /* the requested level is overridden */
		      if ((lfolvl[i + number].fFormatting)
			  && (lfolvl[i + number].fStartAt))
			{
			    /*save the existing lvl and swap in this new one instead */
			    alst =
				wvSearchLST ((*lfo)[apap->ilfo - 1].lsid,
					     *lst, *noofLST);

			    /*use the LVL record from the correct LFOLVL in the LFO */
			    wvCopyLVL (retlvl, &(lvl[i + number]));
			}
		      else if (lfolvl[i + number].fStartAt)
			{
			    alst =
				wvSearchLST ((*lfo)[apap->ilfo - 1].lsid,
					     *lst, *noofLST);

			    /* the lvl is the standard one with a new startat value */
			    wvCopyLVL (retlvl, &(alst->lvl[apap->ilvl]));
			    retlvl->lvlf.iStartAt = lfolvl[i + number].iStartAt;
			}
		      else if (lfolvl[i + number].fFormatting)
			{
			    alst =
				wvSearchLST ((*lfo)[apap->ilfo - 1].lsid,
					     *lst, *noofLST);

			    /* the lvl is the overridden one, with the original startat */
			    wvCopyLVL (retlvl, &(lvl[i + number]));
			    retlvl->lvlf.iStartAt =
				alst->lvl[apap->ilvl].lvlf.iStartAt;
			}
		  }
	    }
      }

    if (alst == NULL)
      {
	  /* 
	     if there no overridden levels i assume that we 
	     search for the appropiate LST 
	   */
	  alst = wvSearchLST ((*lfo)[apap->ilfo - 1].lsid, *lst, *noofLST);
	  if (alst != NULL)
	    {
		wvTrace (("ilvl is %d\n", apap->ilvl));
		if ((alst->lstf.fSimpleList) && (apap->ilvl))
		  {
		      wvWarning
			  ("Level %d requested from list with 1 level\n",
			   apap->ilvl + 1);
		      wvCopyLVL (retlvl, &(alst->lvl[0]));
		  }
		else
		    wvCopyLVL (retlvl, &(alst->lvl[apap->ilvl]));
		wvTrace (("string len is %d", retlvl->numbertext[0]));
		wvTrace (("offset is %d\n", retlvl->lvlf.rgbxchNums[0]));
	    }
      }

    if (alst == NULL)
      {
	  wvError (("No LST found for list\n"));
	  return (1);
      }

    return (0);
}
