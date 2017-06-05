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

#include "ut_types.h" // for UT_UCS4Char
#include "gr_Graphics.h"

#include <MathView/AbstractLogger.hh>
#include <MathView/Configuration.hh>

#include "gr_Abi_CharArea.h"
#include "gr_Abi_ComputerModernShaper.h"

GR_Abi_ComputerModernShaper::GR_Abi_ComputerModernShaper(const SmartPtr<AbstractLogger>& l,
							 const SmartPtr<Configuration>& conf)
  : ComputerModernShaper(l, conf)
{ }

GR_Abi_ComputerModernShaper::~GR_Abi_ComputerModernShaper()
{ }


SmartPtr<GR_Abi_ComputerModernShaper>
GR_Abi_ComputerModernShaper::create(const SmartPtr<AbstractLogger>& l,
				    const SmartPtr<Configuration>& conf)
{ return new GR_Abi_ComputerModernShaper(l, conf); }

AreaRef
GR_Abi_ComputerModernShaper::getGlyphArea(ComputerModernFamily::FontNameId fontName,
					  ComputerModernFamily::FontSizeId designSize,
					  UChar8 index, int size) const
{
  static char fontSize[128];
  sprintf(fontSize, "%dpt", size);

  static char fontFamily[128];
  sprintf(fontFamily, "%s", getFamily()->nameOfFont(fontName, designSize).c_str());

  GR_Font* font = m_pGraphics->findFont(fontFamily, "normal", "", "normal", "", fontSize, "");
  UT_ASSERT(font);

  return GR_Abi_CharArea::create(getGraphics(), font, size, toTTFGlyphIndex(getFamily()->encIdOfFontNameId(fontName), index));
}

void
GR_Abi_ComputerModernShaper::setGraphics(GR_Graphics* pGr)
{
  m_pGraphics = pGr;
}
