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

#ifndef __gr_Abi_ColorArea_h__
#define __gr_Abi_ColorArea_h__

#include <MathView/ColorArea.hh>

class GR_Abi_ColorArea : public ColorArea
{
protected:
  GR_Abi_ColorArea(const AreaRef& area, const RGBColor& c) : ColorArea(area, c) { }
  virtual ~GR_Abi_ColorArea() { }

public:
  static SmartPtr<GR_Abi_ColorArea> create(const AreaRef& area, const RGBColor& c)
  { return new GR_Abi_ColorArea(area, c); }
  virtual AreaRef clone(const AreaRef& area) const { return create(area, getColor()); }

  virtual void render(RenderingContext&, const scaled&, const scaled&) const;
};

#endif // __gr_Abi_ColorArea_h__
