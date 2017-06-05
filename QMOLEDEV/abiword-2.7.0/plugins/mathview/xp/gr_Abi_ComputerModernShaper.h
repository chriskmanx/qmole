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

#ifndef __gr_Abi_ComputerModernShaper_h__
#define __gr_Abi_ComputerModernShaper_h__

#include <MathView/ComputerModernShaper.hh>

class GR_Abi_ComputerModernShaper : public ComputerModernShaper
{
protected:
  GR_Abi_ComputerModernShaper(const SmartPtr<class AbstractLogger>&, const SmartPtr<class Configuration>&);
  virtual ~GR_Abi_ComputerModernShaper();

public:
  static SmartPtr<GR_Abi_ComputerModernShaper> create(const SmartPtr<class AbstractLogger>&, const SmartPtr<class Configuration>&);

  void setGraphics(class GR_Graphics*);
  class GR_Graphics* getGraphics(void) const { return m_pGraphics; }

protected:
  virtual AreaRef getGlyphArea(ComputerModernFamily::FontNameId,
		               ComputerModernFamily::FontSizeId, UChar8, int) const;

  class GR_Graphics* m_pGraphics;
};

#endif // __gr_Abi_ComputerModernShaper_h__
