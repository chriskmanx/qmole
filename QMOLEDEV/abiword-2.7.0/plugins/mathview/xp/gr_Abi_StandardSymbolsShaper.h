/* AbiWord
 * Copyright (C) 2004 Luca Padovani <lpadovan@cs.unibo.it>
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

#ifndef __gr_Abi_StandardSymbolsShaper_h__
#define __gr_Abi_StandardSymbolsShaper_h__

#include <MathView/StandardSymbolsShaper.hh>

#include "ut_types.h" // for UT_UCS4Char
#include "gr_Graphics.h"

class GR_Abi_StandardSymbolsShaper : public StandardSymbolsShaper
{
protected:
  GR_Abi_StandardSymbolsShaper(void);
  virtual ~GR_Abi_StandardSymbolsShaper();

public:
  static SmartPtr<GR_Abi_StandardSymbolsShaper> create(void)
  { return new GR_Abi_StandardSymbolsShaper(); }

  void setGraphics(class GR_Graphics*);
  class GR_Graphics* getGraphics(void) const { return m_pGraphics; }

protected:
  virtual AreaRef getGlyphArea(const SmartPtr<class AreaFactory>&, Char8, const scaled&) const;
   
  struct AbiTextProperties
  {
    MathVariant variant;
    const char* style;
    const char* weight;
  };

  static const AbiTextProperties& getTextProperties(MathVariant = NORMAL_VARIANT);

  AreaRef shapeChar(MathVariant, const class MathFormattingContext&, UT_UCS4Char) const;

  class GR_Graphics* m_pGraphics;
};

#endif // __gr_Abi_StandardSymbolsShaper_h__
