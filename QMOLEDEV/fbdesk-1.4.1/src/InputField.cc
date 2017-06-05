// InputField.cc for FbDesk
// Copyright (c) 2003 Henrik Kinnunen (fluxgen at users.sourceforge.net)
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

#include "InputField.hh"
#include "FbTk/Font.hh"

#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <assert.h>

namespace {

void setNoResize(FbTk::FbWindow &win) {
    // we don't need to maximize this window
    XSizeHints sh;
    sh.flags = PMaxSize | PMinSize;
    sh.max_width = win.width();
    sh.max_height = win.height();
    sh.min_width = win.width();
    sh.min_height = win.height();
    XSetWMNormalHints(win.display(), win.window(), &sh);

}

};
namespace FbDesk {

InputField::InputField(int screen_num, const FbTk::Font &font, GC gc, const FbTk::Color &bgcolor):
    FbTk::TextBox(screen_num, font, "") {

    setBackgroundColor(bgcolor);
    setGC(gc);
    updateSize();
    // setNoResize(*this);


    Atom wm_delete_window = XInternAtom(display(),"WM_DELETE_WINDOW", false);
    XSetWMProtocols(display(), window(), &wm_delete_window, 1);
}

InputField::~InputField() {

}

void InputField::setFont(const FbTk::Font &font) {
    FbTk::TextBox::setFont(font);
    updateSize();
}

void InputField::setText(const std::string &text) {
    FbTk::TextBox::setText(text);
    updateSize();
}

void InputField::setTitle(const std::string &text) {
    setName(text.c_str());
    updateSize();
}

void InputField::updateSize() {
    resize(100, font().height() + 2);
}


}; // end namespace FbDesk
