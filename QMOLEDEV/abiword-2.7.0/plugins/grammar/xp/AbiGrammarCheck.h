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

#ifndef __Abi_GrammarCheck_h__
#define __Abi_GrammarCheck_h__
#include "ut_string_class.h"
#include "ut_types.h"
#include "ut_vector.h"

class fl_BlockLayout;
class LinkGrammarWrap;
class PieceOfText;

class Abi_GrammarCheck
{
 public:
  Abi_GrammarCheck(void);
  virtual ~Abi_GrammarCheck(void);
  bool   CheckBlock(fl_BlockLayout * pB);
  bool   GetEnglishText(fl_BlockLayout * pB);
  bool   isSentenceBlank(const char * szSent);
 private:
  LinkGrammarWrap *  m_GrammarWrap;
  UT_GenericVector<PieceOfText *> m_vecSentences;
};

#endif // __Abi_GrammarCheck_h__
