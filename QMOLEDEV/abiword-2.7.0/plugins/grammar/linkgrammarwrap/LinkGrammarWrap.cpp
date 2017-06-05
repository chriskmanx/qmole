/*
 * Copyright (C) 2005 Martin Sevior <msevior@physics.unimelb.edu.au>
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
/********************************************************************************/
/* Copyright (c) 2004
*/
/* Daniel Sleator, David Temperley, and John Lafferty
*/
/* All rights reserved
*/
/*
*/
/* Use of the link grammar parsing system is subject to the terms of the
*/
/* license set forth in the LICENSE file included with this software,
*/
/* and also available at http://www.link.cs.cmu.edu/link/license.html
*/
/* This license allows free redistribution and use in source and binary
*/
/* forms, with or without modification, subject to certain conditions.
*/
/*
*/
/********************************************************************************/

#include "ut_string_class.h"
#include "ut_types.h"
#include "ut_debugmsg.h"
#include "ut_vector.h"
#include "ut_string.h"
#include "../xp/AbiGrammarUtil.h"

#include "LinkGrammarWrap.h"

LinkGrammarWrap::LinkGrammarWrap(void) 
{
  m_Opts = parse_options_create();
  m_Dict = dictionary_create((char*)"en/4.0.dict", (char*)"en/4.0.knowledge",
	(char*)"en/4.0.constituent-knowledge", (char*)"en/4.0.affix");
  parse_options_set_max_parse_time(m_Opts, 1); // 1 second max parse time
}

LinkGrammarWrap::~LinkGrammarWrap(void)
{
  if(m_Dict)
    dictionary_delete(m_Dict);
  if(m_Opts)
    parse_options_delete(m_Opts);
}

bool LinkGrammarWrap::parseSentence(PieceOfText * pT)
{
  if(!m_Dict)
  {
    UT_DEBUGMSG(("No dictionary!!\n"));
    return true; // default to no grammar checking.
  }
  //  UT_DEBUGMSG(("Sentence received |%s|\n",pT->sText.utf8_str()));
  Sentence  sent = sentence_create(const_cast<char *>(pT->sText.utf8_str()),m_Dict);
  if (!sent) return true;

  /* First parse with cost 0 or 1 and no null links */
  parse_options_set_disjunct_cost(m_Opts, 2);
  parse_options_set_min_null_count(m_Opts, 0);
  parse_options_set_max_null_count(m_Opts, 0);
  parse_options_set_islands_ok(m_Opts, 0);
  parse_options_set_panic_mode(m_Opts, TRUE);
  parse_options_reset_resources(m_Opts);
  UT_sint32 num_linkages = sentence_parse(sent, m_Opts);
  bool res =  (num_linkages >= 1);
  if(TRUE == parse_options_timer_expired(m_Opts))
  {
    UT_DEBUGMSG(("!!! Timer expired! Mark valid anyway!\n"));
    res= true; // Mark valid if it's too hard. FIXME. We can attempt to recover
               // by tweaking paramters once we know what we're doing.
  }
  UT_UTF8String errStr = "";
  if(!res && (num_linkages == 0))
  {
    // Now proces with NULL links. to find out what went wrong.
    parse_options_set_min_null_count(m_Opts, 1);
    parse_options_set_max_null_count(m_Opts, sentence_length(sent));
    parse_options_set_islands_ok(m_Opts, 1);
    parse_options_reset_resources(m_Opts);
    num_linkages = sentence_parse(sent, m_Opts);
  }
  pT->m_bGrammarChecked = true;
  pT->m_bGrammarOK = res;
  if(!res)
  {
    UT_GenericVector<AbiGrammarError *> vecMapOfWords;
    //
    // Get first linkage
    //
    AbiGrammarError * pErr = NULL;
    if(num_linkages > 0)
    {
      Linkage linkage = linkage_create(0, sent, m_Opts);
      if(linkage != NULL)
      {
	UT_sint32 i = 0;
	UT_sint32 iLow= 0;
	UT_sint32 iHigh= 0;
	UT_sint32 iOff = pT->iInLow;
	const char * szSent = pT->sText.utf8_str();
	UT_sint32 totlen = strlen(szSent);
	for (i=1; i<sentence_length(sent) && (iLow < totlen); i++) 
	{
	  //
	  // NULL link island.
	  //
	  //    UT_DEBUGMSG((" iLow %d szSent[iLow] %c\n",iLow,szSent[iLow]));
	  while((szSent[iLow] == ' ') && (iLow < totlen))
	  {
	    //UT_DEBUGMSG((" iLow %d szSent[iLow] %c\n",iLow,szSent[i]));
	    iLow++;
	  }
	  if(iLow >= totlen)
	  {
	    //UT_DEBUGMSG(("Error ! ran off the end! iLow %d \n Text |%s|\n",iLow,szSent));
	    break;
	  }
	  AbiGrammarError * pWordMap = new  AbiGrammarError();
	  pWordMap->m_iErrLow = iLow;
	  pWordMap->m_iErrHigh = iLow + strlen(sentence_get_nth_word(sent, i));
	  pWordMap->m_iWordNum = i;
	  vecMapOfWords.addItem(pWordMap);
	  bool bNew = false;
	  if(!sentence_nth_word_has_disjunction(sent, i))
	  {
	    //UT_DEBUGMSG(("|%s| NULL LINK\n",sent->word[i].string));
	    if(pErr == NULL)
	    {
	      pErr = new AbiGrammarError();
	      bNew = true;
	    }
	    if(bNew || (pErr->m_iWordNum + 1 < i))
	    {
	      if(!bNew)
	      {
		pErr = new AbiGrammarError();
	      }
	      iHigh = iLow + strlen(sentence_get_nth_word(sent, i));
	      pErr->m_iErrLow = iLow + iOff -1;
	      pErr->m_iErrHigh = iHigh + iOff -1;
	      if(pErr->m_iErrLow < 0)
	      {
		pErr->m_iErrLow = 0;
	      }
	      if(pErr->m_iErrHigh < totlen-1)
	      {
		  pErr->m_iErrHigh += 1;
	      }
	      pErr->m_iWordNum = i;
	      // UT_DEBUGMSG(("Add Error %x low %d High %d\n",pErr,pErr->m_iErrLow,pErr->m_iErrHigh));
	      pT->m_vecGrammarErrors.addItem(pErr);
	    }
	    else
	    {
	      //
	      // Expand the sqiggle
	      //
	      iHigh = iLow + strlen(sentence_get_nth_word(sent, i)) + iOff;
	      pErr->m_iErrHigh = iHigh;
	      if(pErr->m_iErrHigh < totlen-1)
	      {
		  pErr->m_iErrHigh += 1;
	      }
	      pErr->m_iWordNum = i;
	    }
	  }
	  iLow += strlen(sentence_get_nth_word(sent, i));
	}
	//
	// No NULL links but still an error , mark the whole sentence bad.
	//
	if(pT->m_vecGrammarErrors.getItemCount() == 0)
	{
	  pErr = new AbiGrammarError();
	  pErr->m_iErrLow = pT->iInLow;
	  pErr->m_iErrHigh = pT->iInHigh;
	  if(pErr->m_iErrLow < 0)
	  {
	    pErr->m_iErrLow = 0;
	  }
	  // UT_DEBUGMSG(("Add Error %x low %d High %d\n",pErr,pErr->m_iErrLow,pErr->m_iErrHigh));
	  pT->m_vecGrammarErrors.addItem(pErr);
	  pErr->m_sErrorDesc = linkage_get_violation_name(linkage);
	  //UT_DEBUGMSG(("Complete Sentence had error %s\n",pErr->m_sErrorDesc.utf8_str()));
	}

	//	  for(i=0; i< pT->m_vecGrammarErrors.getItemCount(); i++)
	// {
	//    pErr = pT->m_vecGrammarErrors.getNthItem(i);
	//    UT_DEBUGMSG((" err %d iLow %d iHigh %d\n",i,pErr->m_iErrLow,pErr->m_iErrHigh));
	//  }
	UT_UTF8String sErr = linkage_get_violation_name(linkage);
	//	UT_DEBUGMSG(("Top Level error message |%s|\n",sErr.utf8_str()));
	UT_sint32 count = linkage_get_num_sublinkages(linkage);
	//
	// Find linkages with violations
	//
	for(i=0; i<count;i++)
	{
	  UT_sint32 iok = linkage_set_current_sublinkage(linkage, i);
	  if(iok == 0)
	    continue;
	  UT_sint32 j = 0;
	  UT_sint32 iNum = linkage_get_num_links(linkage);
	  for(j=0;j< iNum;j++)
	    //	  char * szViolation = linkage_get_violation_name(linkage);
	  //if(szViolation != NULL)
	  {
            // debug disabled
	    //UT_sint32 iLword = linkage_get_link_lword(linkage,j);
	    //const char * szLword =  linkage_get_word(linkage,iLword);
	    //UT_sint32 iRword = linkage_get_link_rword(linkage,j);
	    //const char * szRword =  linkage_get_word(linkage,iRword);
	    //UT_DEBUGMSG(("Link %d |%s| and |%s|\n",i,szLword,szRword));
	    //UT_DEBUGMSG(("Left Word num %d Right Word num %d\n",iLword,iRword));
	  }
	}
	linkage_delete(linkage);
	for(i=0; i<  vecMapOfWords.getItemCount(); i++)
	{
	  AbiGrammarError * p = vecMapOfWords.getNthItem(i);
	  delete p;
	}
      }
    }
    else
    {
      pErr = new AbiGrammarError();
      pErr->m_iErrLow = pT->iInLow;
      pErr->m_iErrHigh = pT->iInHigh;
      if(pErr->m_iErrLow < 0)
      {
	pErr->m_iErrLow = 0;
      }
      //      UT_DEBUGMSG(("Final Add Error %x low %d High %d\n",pErr,pErr->m_iErrLow,pErr->m_iErrHigh));
      pT->m_vecGrammarErrors.addItem(pErr);

    }
  }
  sentence_delete(sent);
  return res;
}

bool LinkGrammarWrap::clear(void)
{
  return true;
}
