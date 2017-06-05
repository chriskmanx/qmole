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
#include "ut_types.h"
#include "ut_debugmsg.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
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
#include "AbiGrammarUtil.h"

PieceOfText::PieceOfText(void):
  iInLow(0),
  iInHigh(0),
  nWords(0),
  bHasStop(false),
  sText(""),
  m_bGrammarChecked(false),
  m_bGrammarOK(false)
{
}

PieceOfText::~PieceOfText(void)
{
}

UT_sint32 PieceOfText::countWords(void)
{
        const char * szSent = sText.utf8_str();
	UT_sint32 totlen = strlen(szSent);
	bool bNewWord = false;
	UT_sint32 i = 0;
	for (i=0; i< totlen; i++) 
	{
	       bool bFoundSpace = false;
	       while(((szSent[i] == ' ') || szSent[i]==';' || szSent[i] ==':' 
		      || szSent[i]==','  || szSent[i] ==static_cast<char>(UCS_TAB)) && (i < totlen))
	       {
		     i++;
		     bFoundSpace = true;
	       }
	       if(szSent[i] == '.')
	       {
		     if( (i> 0)&&((szSent[i-1] >= '0')&&(szSent[i-1] <= '9')))
		     {
		           continue;
		     }
		     bHasStop = true;
		     continue;
	       }
	       if(bFoundSpace)
	       {
	             nWords++;
		     bNewWord = true;
	       }
	       if(bNewWord && (szSent[i] >= '0' && szSent[i] <= '9'))
	       {
		     nWords--;
		     bNewWord = false;
	       }
	}

	return nWords;
}

AbiGrammarError::AbiGrammarError(void):
  m_iErrLow(0),
  m_iErrHigh(0),
  m_iWordNum(0),
  m_sErrorDesc("")
{
}


AbiGrammarError::~AbiGrammarError(void)
{
}
