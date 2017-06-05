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

#include "gr_Abi_ColorArea.h"
#include "gr_Abi_RenderingContext.h"

#include "gr_Graphics.h"

void
GR_Abi_ColorArea::render(RenderingContext& c, const scaled& x, const scaled& y) const
{
  GR_Abi_RenderingContext& context = dynamic_cast<GR_Abi_RenderingContext&>(c);

  UT_DEBUGMSG(("setting color %d %d %d\n", getColor().red, getColor().green, getColor().blue));

  UT_RGBColor oldColor;
  context.getColor(oldColor);
  context.setColor(getColor());
  getChild()->render(context, x, y);
  context.setColor(oldColor);
}
