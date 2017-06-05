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

#ifndef __gr_Abi_InkArea_h__
#define __gr_Abi_InkArea_h__

#include <MathView/InkArea.hh>

class GR_Abi_InkArea : public InkArea
{
protected:
  GR_Abi_InkArea(const AreaRef& area) : InkArea(area) { }
  virtual ~GR_Abi_InkArea() { }

public:
  static SmartPtr<GR_Abi_InkArea> create(const AreaRef& area)
  { return new GR_Abi_InkArea(area); }
  virtual AreaRef clone(const AreaRef& area) const { return create(area); }

  virtual void render(RenderingContext&, const scaled&, const scaled&) const;
};

#endif // __gr_Abi_InkArea_h__
