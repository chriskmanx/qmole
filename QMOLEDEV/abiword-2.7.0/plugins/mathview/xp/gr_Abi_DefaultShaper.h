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

#ifndef __gr_Abi_DefaultShaper_h__
#define __gr_Abi_DefaultShaper_h__

#include <MathView/MathVariant.hh>
#include <MathView/Shaper.hh>

#include "ut_types.h" // for UT_UCS4Char
#include "gr_Graphics.h"

class GR_Abi_DefaultShaper : public Shaper
{
protected:
  GR_Abi_DefaultShaper(void);
  virtual ~GR_Abi_DefaultShaper();

public:
  static SmartPtr<GR_Abi_DefaultShaper> create(void)
  { return new GR_Abi_DefaultShaper(); }

  virtual void registerShaper(const SmartPtr<class ShaperManager>&, unsigned);
  virtual void unregisterShaper(const SmartPtr<class ShaperManager>&, unsigned);
  virtual void shape(class ShapingContext&) const;

  void setGraphics(class GR_Graphics*);
  class GR_Graphics* getGraphics(void) const { return m_pGraphics; }

protected:
  struct AbiTextProperties
  {
    MathVariant variant;
    const char* family;
    const char* style;
    const char* weight;
  };

  static const AbiTextProperties& getTextProperties(MathVariant = NORMAL_VARIANT);

  AreaRef shapeChar(MathVariant, const class ShapingContext&, UT_UCS4Char) const;

  class GR_Graphics* m_pGraphics;
};

#endif // __gr_Abi_DefaultShaper_h__
