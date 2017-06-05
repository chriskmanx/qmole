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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pd_Document.h"
#include "fl_BlockLayout.h"
#include "AbiGrammarCheck.h"
#include "ut_mbtowc.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "fp_Run.h"
#include "fp_TextRun.h"
#include "ap_Strings.h"
#include "ut_sleep.h"
#include <sys/types.h>  
#include <sys/stat.h>
#ifdef TOOLKIT_WIN
#include <windows.h>
#else
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include "ut_files.h"
#endif
#include "../linkgrammarwrap/LinkGrammarWrap.h"
#include "AbiGrammarUtil.h"
#include "ut_growbuf.h"
#include "fl_Squiggles.h"

Abi_GrammarCheck::Abi_GrammarCheck(void) :
  m_GrammarWrap(NULL)
{
}

Abi_GrammarCheck::~Abi_GrammarCheck(void)
{
  delete m_GrammarWrap;
  UT_sint32 i =0;
  for(i= 0; i< m_vecSentences.getItemCount();i++)
  {
    PieceOfText * pPT = m_vecSentences.getNthItem(i);
    delete pPT;
  }
  m_vecSentences.clear();
}

bool Abi_GrammarCheck::CheckBlock(fl_BlockLayout * pB)
{
  if(m_GrammarWrap == NULL)
  {
    m_GrammarWrap = new LinkGrammarWrap();
  }
  if(pB == NULL)
  {
    return false;
  }
  bool bSomeText = GetEnglishText(pB);
  if(!bSomeText)
  {
    return true;
  }
  pB->getGrammarSquiggles()->deleteAll();
  if(m_vecSentences.getItemCount() == 1)
  {
    PieceOfText * pTxt = m_vecSentences.getNthItem(0);
    pTxt->countWords();
    if(!pTxt->bHasStop && pTxt->nWords <8) // Likely a heading
    {
      return true;
    }
    if(pTxt->bHasStop && pTxt->nWords <3)
    {
      return true;
    }

  }
  for(UT_sint32 j=0; j< m_vecSentences.getItemCount(); j++)
  {
    PieceOfText * pTxt = m_vecSentences.getNthItem(j);
    //    printf("Original Sentence Low %d High %d |%s|\n",pTxt->iInLow,pTxt->iInHigh,pTxt->sText.utf8_str());
    
    if(isSentenceBlank(pTxt->sText.utf8_str()))
    {
      continue;
    }
    bool bValid = m_GrammarWrap->parseSentence(pTxt);
    if(!bValid)
    {
      // printf("Wrong Grammar|%s|\n LowOff %d HighOff %d \n",pTxt->sText.utf8_str(),	     pTxt->iInLow,pTxt->iInHigh);

      //
      // Add an invisible POB to cover the entire entence. We use this
      // to turn off squiggles when editing the sentence.
      //
      fl_PartOfBlock * pPOB1 = new fl_PartOfBlock(pTxt->iInLow,(pTxt->iInHigh- pTxt->iInLow +1));
      pPOB1->setInvisible();
      pB->getGrammarSquiggles()->add(pPOB1);
      //
      // Insert the Part Of Block Into the GrammarSquiggles
      //
      UT_sint32 i = 0;
      for(i=0; i< pTxt->m_vecGrammarErrors.getItemCount();i++)
      {
	//	printf("i=  %d no errors %d \n",i,pTxt->m_vecGrammarErrors.getItemCount());
	AbiGrammarError * pErr = pTxt->m_vecGrammarErrors.getNthItem(i);
	fl_PartOfBlock * pPOB = new fl_PartOfBlock(pErr->m_iErrLow, (pErr->m_iErrHigh- pErr->m_iErrLow +1));
	pB->getGrammarSquiggles()->add(pPOB);
      }

      
    }
  }
  return true;
}

/*!
 * Need to break the paragraph into sentences.
 */
bool Abi_GrammarCheck::GetEnglishText(fl_BlockLayout * pB)
{
  fp_Run * pRun = pB->getFirstRun();
  UT_sint32 i =0;
  for(i= 0; i< m_vecSentences.getItemCount();i++)
  {
    PieceOfText * pPT = m_vecSentences.getNthItem(i);
    delete pPT;
  }
  m_vecSentences.clear();
  //
  // First fill a growbuf with text. Take account of runs with length
  // 1 by inserting space in their place. This keeps a 1:1 correspondence
  // between text and it's offset inthe block. We'll need these offsets
  // in order to mark the ungrammatical regions of the text.
  //
  // Append the text to a 4-byte buffer at first. Later when we scan for
  // full-stops we'll fill an 8-bit string with the ascii codes of English
  // text.
  UT_GrowBuf Text;
  UT_GrowBufElement space = static_cast<UT_GrowBufElement>(' ');
  bool bSomeText = false;
  UT_UTF8String sEng("en");
  const gchar * szLang =  NULL;
  UT_UTF8String sLang("");
  fp_TextRun * pTRun = NULL;
  while(pRun)
  {
    
    if((pRun->getType() == FPRUN_TEXT) && pRun->getLength() > 0)
    {
      pTRun = static_cast<fp_TextRun *>(pRun);
      //
      // For some strange compiler reason, this line szLang= NULL is required
      // otherwise szLang is undefined on the line afterwards!!!!
      //
      szLang = NULL;
      szLang =  pTRun->getLanguage();
      //      printf("Block %x pTRun %x Language %x \n",pB,pTRun,szLang);
      if(szLang != NULL && *szLang != '\0')
      {
	  sLang = szLang;
      }
      if(szLang && sLang.substr(0,2) == sEng) // Only grammar check english
      {
	  pTRun->appendTextToBuf(Text);
	  bSomeText = true;
      }
      else
      {
	return false; // Only fully english paragraphs are grammar checked
      }
    }
    else if(pRun->getLength() == 1)
    {
      Text.append(&space,1);
    }
    pRun = pRun->getNextRun();
  }
  if(!bSomeText)
  {
    return false;
  }
  //
  // OK! Now we have a growbuf full of ucs4 text with a 1:1 correspondance
  // between it's position in the growbuf and it's offset in the Block.
  // Now we fill the sentence classes with text and their offsets.
  // A sentence simply starts at the start of growbuf and is terminated
  // when we come accros a full-stop.
  //
  // Remember that the first character of text is at blockoffset 1, so we
  // add 1 everywhere
  //
  UT_uint32 iTotLen = Text.getLength();
  if(iTotLen == 0)
  {
    return false; // No Text in the buffer.
  }
  UT_uint32 iCurText = 0;
  UT_uint32 iCurStart = 0;
  UT_GrowBufElement * pUCS4 = Text.getPointer(0);
  PieceOfText * pCurSent = new PieceOfText();
  m_vecSentences.addItem(pCurSent);
  pCurSent->iInLow = iCurStart;
  char cCur[2];
  cCur[1] = 0;
  while(iCurText < iTotLen)
  {
    cCur[0] = static_cast<char>(*pUCS4);
    pCurSent->sText += reinterpret_cast<const char *>(cCur);
    pUCS4++;
    iCurText++;
    if(((cCur[0] == '.') || (cCur[0] == '?') || (cCur[0] == '!')) && (iCurText < iTotLen))
    {
      pCurSent->iInHigh = iCurText-1;
      pCurSent = new PieceOfText();
      m_vecSentences.addItem(pCurSent);
      pCurSent->iInLow = iCurText;
    }
    else if(iCurText == iTotLen)
    {
      pCurSent->iInHigh = iCurText-1;
    }
  }
  //
  // we're done!
  //
  return true;
}

bool Abi_GrammarCheck::isSentenceBlank(const char * szSent)
{
  const char * szp = szSent;
  while(*szp == ' ' && *szp != 0)
    szp++;
  if(*szp == 0)
  {
    return true;
  }
  return false;
}
