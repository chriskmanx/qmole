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
#include "gr_Abi_DefaultShaper.h"

#define NORMAL_INDEX 0
#define MAPPED_BASE_INDEX 1

GR_Abi_DefaultShaper::GR_Abi_DefaultShaper()
{ }

GR_Abi_DefaultShaper::~GR_Abi_DefaultShaper()
{ }

void
GR_Abi_DefaultShaper::registerShaper(const SmartPtr<ShaperManager>& sm, unsigned shaperId)
{
  // normal characters are not registered because this shaper is supposed to
  // be the default one. It will be called anyway as soon as there's a
  // Unicode char that cannot be shaped otherwise

  // Variant characters however are mapped so that it is possible to
  // recover their "normal" Unicode number in the BMP and the appropriate
  // AbiWord properties can be applied when asking for a font
  for (unsigned i = NORMAL_VARIANT; i <= MONOSPACE_VARIANT; i++)
    {
      for (Char16 ch = 0x21; ch < 0x80; ch++)
	{
	  Char32 vch = mapMathVariant(MathVariant(i), ch);
	  if (vch != ch)
	    sm->registerChar(vch, GlyphSpec(shaperId, MAPPED_BASE_INDEX + i - NORMAL_VARIANT, ch));
	}
    }

}

void
GR_Abi_DefaultShaper::unregisterShaper(const SmartPtr<class ShaperManager>&, unsigned)
{
  // nothing to do
}

#define DEFAULT_SERIF_FAMILY "Times"
#define DEFAULT_SANS_SERIF_FAMILY "Helvetica"
#define DEFAULT_MONOSPACE "Courier"

const GR_Abi_DefaultShaper::AbiTextProperties&
GR_Abi_DefaultShaper::getTextProperties(MathVariant variant)
{
  UT_ASSERT(variant >= NORMAL_VARIANT && variant <= MONOSPACE_VARIANT);
  static const AbiTextProperties variantDesc[] =
    {
      { NORMAL_VARIANT, DEFAULT_SERIF_FAMILY, "normal", "normal" },
      { BOLD_VARIANT, DEFAULT_SERIF_FAMILY, "normal", "bold" },
      { ITALIC_VARIANT, DEFAULT_SERIF_FAMILY, "italic", "normal" },
      { BOLD_ITALIC_VARIANT, DEFAULT_SERIF_FAMILY, "italic", "bold" }, 
      { DOUBLE_STRUCK_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "normal", "bold" },
      { BOLD_FRAKTUR_VARIANT, DEFAULT_SERIF_FAMILY, "normal", "bold" },
      { SCRIPT_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "normal", "normal" },
      { BOLD_SCRIPT_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "normal", "bold" },
      { FRAKTUR_VARIANT, DEFAULT_SERIF_FAMILY, "normal", "bold" },
      { SANS_SERIF_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "normal", "normal" },
      { BOLD_SANS_SERIF_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "normal", "bold" },
      { SANS_SERIF_ITALIC_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "italic", "normal" },
      { SANS_SERIF_BOLD_ITALIC_VARIANT, DEFAULT_SANS_SERIF_FAMILY, "italic", "bold" },
      { MONOSPACE_VARIANT, DEFAULT_MONOSPACE, "normal", "normal" }
    };
  return variantDesc[variant - NORMAL_VARIANT];
}

void
GR_Abi_DefaultShaper::shape(ShapingContext& context) const
{
  const GlyphSpec spec = context.getSpec();
  if (spec.getFontId() == NORMAL_INDEX)
    context.pushArea(1, shapeChar(NORMAL_VARIANT, context, context.thisChar()));
  else
    context.pushArea(1, shapeChar(MathVariant(spec.getFontId() - MAPPED_BASE_INDEX + NORMAL_VARIANT),
				  context, spec.getGlyphId()));
}

AreaRef
GR_Abi_DefaultShaper::shapeChar(MathVariant variant, const ShapingContext& context, UT_UCS4Char ch) const
{
  // the "variant" parameter overrides the variant value in the formatting context

  static char fontSize[128];
  sprintf(fontSize, "%dpt", static_cast<int>(context.getSize().toFloat() + 0.5f));

  const AbiTextProperties& props = getTextProperties(variant);
  GR_Font* font = m_pGraphics->findFont(props.family, props.style, "", props.weight, "", fontSize, "");
  UT_ASSERT(font);

  SmartPtr<GR_Abi_AreaFactory> factory = smart_cast<GR_Abi_AreaFactory>(context.getFactory());
  return factory->charArea(m_pGraphics, font, context.getSize(), ch);
}

void
GR_Abi_DefaultShaper::setGraphics(GR_Graphics* pGr)
{
  m_pGraphics = pGr;
}
