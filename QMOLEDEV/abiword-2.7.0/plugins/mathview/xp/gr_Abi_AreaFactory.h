/* AbiWord
 * Copyright (C) 2004 Luca Padovani
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

#ifndef __gr_Abi_AreaFactory_h__
#define __gr_Abi_AreaFactory_h__

#include <MathView/AreaFactory.hh>

#include "gr_Abi_ColorArea.h"
//#include "gr_Abi_BackgroundArea.h"
#include "gr_Abi_InkArea.h"
#include "gr_Abi_CharArea.h"

class GR_Abi_AreaFactory : public AreaFactory
{
protected:
  GR_Abi_AreaFactory(void) { }
  virtual ~GR_Abi_AreaFactory() { }

public:
  static SmartPtr<GR_Abi_AreaFactory> create(void)
  { return new GR_Abi_AreaFactory(); }

  // redefined methods

  virtual SmartPtr<ColorArea> color(const AreaRef& area, const RGBColor& _color) const
  { return GR_Abi_ColorArea::create(area, _color); }
  virtual SmartPtr<InkArea> ink(const AreaRef& area) const
  { return GR_Abi_InkArea::create(area); }
#if 0
  virtual AreaRef background(const AreaRef& area, const RGBColor& _color) const
  { return GR_Abi_BackgroundArea::create(area, _color); }
#endif

  // new methods

  virtual SmartPtr<GR_Abi_CharArea> charArea(class GR_Graphics* g, class GR_Font* f, const scaled& size, UT_UCS4Char ch) const
  { return GR_Abi_CharArea::create(g, f, size, ch); }
};

#endif // __gr_Abi_AreaFactory_h__
