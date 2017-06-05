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

#ifndef __gr_Abi_MathGraphicDevice_h__
#define __gr_Abi_MathGraphicDevice_h__

#include <MathView/MathGraphicDevice.hh>
#include <MathView/ShaperManager.hh>

class GR_Abi_MathGraphicDevice : public MathGraphicDevice
{
protected:
  GR_Abi_MathGraphicDevice(const SmartPtr<class AbstractLogger>&,
			   const SmartPtr<class Configuration>&,
			   class GR_Graphics*);
  virtual ~GR_Abi_MathGraphicDevice();

public:
  static SmartPtr<GR_Abi_MathGraphicDevice> create(const SmartPtr<class AbstractLogger>& pLogger,
						   const SmartPtr<class Configuration>&,
						   class GR_Graphics* pGr);

  virtual scaled defaultLineThickness(const class FormattingContext&) const;
private:
  SmartPtr<class GR_Abi_AreaFactory> m_abiFactory;
};

#endif // __gr_Abi_MathGraphicDevice_h__
