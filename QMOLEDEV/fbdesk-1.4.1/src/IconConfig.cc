// IconConfig.cc
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

#include "IconConfig.hh"

#include "FbTk/App.hh"

namespace FbDesk {

IconConfig *IconConfig::s_instance = 0;


IconConfig &IconConfig::instance() {
    if (s_instance == 0)
        throw std::string("Must create one instance of IconConfig first");

    return *s_instance;
}



IconConfig::IconConfig(int screen_num):
    m_font(0),
    m_gc(DefaultGC(FbTk::App::instance()->display(), screen_num )) {
    if ( s_instance != 0 )
        throw std::string("Can not create one instance of IconConfig");
    s_instance = this;
}


void IconConfig::setTextPlacement(Icon::TextPlacement place) { 
    m_textplacement = place;
}

void IconConfig::setTextBackground(const FbTk::Color &color) {
    m_background = color;
}

void IconConfig::setTextColor(const FbTk::Color &color) {
    Display *disp = FbTk::App::instance()->display();
    XGCValues gcval;
    gcval.foreground = color.pixel();
    XChangeGC(disp, m_gc, GCForeground, &gcval);
}

}
