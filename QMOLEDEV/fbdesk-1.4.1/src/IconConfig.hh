// IconConfig.hh
// Copyright (c) 2006 Henrik Kinnunen (fluxgen at users.sourceforge.net)
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

// $Id$

#ifndef ICONCONFIG_HH
#define ICONCONFIG_HH

#include "Icon.hh"
#include "FbTk/Color.hh"
#include "FbTk/Font.hh"

namespace FbDesk {
// helper class for icon configuration
// a quick hack...
class IconConfig {
public:
    explicit IconConfig(int screen_num);
    static IconConfig &instance();

    void setFont(FbTk::Font& font) { m_font = &font; }
    void setTextColor(const FbTk::Color &color);
    void setTextBackground(const FbTk::Color &color);
    void setTextPlacement(Icon::TextPlacement place);
    void setTextAlpha(int alpha) { m_text_alpha = alpha; }
    void setIconAlpha(int alpha) { m_icon_alpha = alpha; }

    GC gc() const { return m_gc; }    
    FbTk::Color &background() { return m_background; }
    FbTk::Font &font() { return *m_font; }
    int textAlpha() const { return m_text_alpha; }
    int iconAlpha() const { return m_icon_alpha; }
    Icon::TextPlacement textPlacement() const { return m_textplacement; }
private:
    Icon::TextPlacement m_textplacement;
    FbTk::Font *m_font; ///< all icons have the same font
    GC m_gc; ///< graphic context for text drawing
    Atom m_xdnd_atom; ///< for drag and drop
    int m_text_alpha, m_icon_alpha;
    FbTk::Color m_background;
    static IconConfig *s_instance;
};

}

#endif // ICONCONFIG_HH
