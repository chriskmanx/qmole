/* AbiWord
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

#ifndef __Abi_GrammarUtil_h__
#define __Abi_GrammarUtil_h__
#include "ut_string_class.h"
#include "ut_types.h"
#include "ut_vector.h"

class AbiGrammarError 
{
 public:
  AbiGrammarError(void);
  virtual ~AbiGrammarError(void);
  UT_sint32 m_iErrLow;
  UT_sint32 m_iErrHigh;
  UT_sint32 m_iWordNum;
  UT_UTF8String m_sErrorDesc;
};

class PieceOfText
{
 public:
  PieceOfText(void);
  virtual ~PieceOfText(void);
  UT_sint32 iInLow;
  UT_sint32 iInHigh;
  UT_sint32 nWords;
  bool      bHasStop;
  UT_UTF8String sText;
  bool      m_bGrammarChecked;
  bool      m_bGrammarOK;
  UT_GenericVector<AbiGrammarError *> m_vecGrammarErrors;
  UT_UTF8String m_sSuggestion;
  UT_sint32 countWords(void);
};

#endif // __Abi_GrammarUtil_h__
