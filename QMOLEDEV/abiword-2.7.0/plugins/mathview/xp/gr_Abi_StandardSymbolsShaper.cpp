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

#include <MathView/ShapingContext.hh>
#include <MathView/ShaperManager.hh>
#include <MathView/MathGraphicDevice.hh>
#include <MathView/MathMLElement.hh>
#include <MathView/MathVariantMap.hh>

#include "gr_Abi_AreaFactory.h"
#include "gr_Abi_StandardSymbolsShaper.h"

#define NORMAL_INDEX 0
#define MAPPED_BASE_INDEX 1

GR_Abi_StandardSymbolsShaper::GR_Abi_StandardSymbolsShaper()
{ }

GR_Abi_StandardSymbolsShaper::~GR_Abi_StandardSymbolsShaper()
{ }

AreaRef
GR_Abi_StandardSymbolsShaper::getGlyphArea(const SmartPtr<AreaFactory>& f,
					   Char8 index, const scaled& size) const
{
  SmartPtr<GR_Abi_AreaFactory> factory = smart_cast<GR_Abi_AreaFactory>(f);
  assert(factory);

  static char fontSize[128];
  sprintf(fontSize, "%dpt", static_cast<int>(size.toFloat() + 0.5f));

  GR_Font* font = m_pGraphics->findFont("Symbol", "normal", "", "normal", "", fontSize, "");
  UT_ASSERT(font);
  UT_UCS4Char ch = static_cast<UT_UCS4Char>(static_cast<unsigned char>(index));
  return factory->charArea(getGraphics(), font, size, ch);
}

void
GR_Abi_StandardSymbolsShaper::setGraphics(GR_Graphics* pGr)
{
  m_pGraphics = pGr;
}
