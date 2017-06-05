// InputField.hh for FbDesk
// Copyright (c) 2003-2004 Henrik Kinnunen (fluxgen at users.sourceforge.net)
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

#ifndef FBDESK_INPUTFIELD_HH
#define FBDESK_INPUTFIELD_HH

#include "FbTk/TextBox.hh"
#include "FbTk/Command.hh"

#include <string>

namespace FbDesk {
// simple input field
class InputField:public FbTk::TextBox {
public:
    InputField(int screen_num, const FbTk::Font &font, GC gc, const FbTk::Color &bgcolor);
    virtual ~InputField();

    void enter() {
        if (m_on_enter.get() != 0)
            m_on_enter->execute();
    }
    void escape() {
        hide();
    }
    void setFont(const FbTk::Font &font);
    void setText(const std::string &text);
    void setOnEnter(FbTk::Command *on_enter) { m_on_enter.reset(on_enter); }
    void setTitle(const std::string &label);

private:
    void updateSize();

    std::auto_ptr<FbTk::Command> m_on_enter; ///< what to do when we're done with input
};

}; // end namespace FbDesk

#endif // FBDESK_INPUTFIELD_HH

