// Icon.hh for fbdesk
// Copyright (c) 2002 Henrik Kinnunen (fluxgen at users.sourceforge.net)
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

#ifndef FBDESK_ICON_HH
#define FBDESK_ICON_HH

#include "FbWindow.hh"
#include "Font.hh"

#include <string>
#include <list>

namespace FbTk {
class Color;
};



namespace FbDesk {

class IconConfig;

class Icon {
public:
    enum TextPlacement { TEXTPLACE_TOP, TEXTPLACE_BOTTOM,
                         TEXTPLACE_LEFT, TEXTPLACE_RIGHT};

    Icon(const std::string &imagefilename, const std::string &command, 
         const std::string &label);
    ~Icon();

    void show();

    void setCommand(const std::string &command);
    void setImage(const std::string &imagefilename);
    void setLabel(const std::string &label);
    /// move this icon to X, Y pos
    void move(int x, int y);


    const std::string &command() const { return m_command; }
    const std::string &imageFilename() const { return m_image_filename; }
    const std::string &label() const { return m_label; }

    const FbTk::FbWindow &iconWindow() const { return m_icon_window; }
    const FbTk::FbWindow &labelWindow() const { return m_label_window; }
    unsigned int width() const { return m_width; }
    unsigned int height() const { return m_height; }
    int x() const { return m_x; }
    int y() const { return m_y; }
    void redrawLabel();
    void update();
    void raise();
    void lower();
private:
    void setupDragAndDrop();
    void setupLayer();

    struct Cache {
        std::string m_filename; // the associated filename
        Pixmap m_pixmap; // the pixmap
        Mask m_mask;  // mask 
        int m_count; // number of times this cache is used
        unsigned int m_width, m_height; // size of pixmap
    };
    typedef std::list<Cache *> CacheList;
	
    Cache * const findCache(const std::string &filename);
    void removeFromCache(const std::string &filename);
    void resize(unsigned int width, unsigned int height);
    bool loadImage(const std::string &image_filename);

    void updateLabel();
    void updateWindow();
    void updateTextAlignment();

    std::string m_command;
    std::string m_image_filename;
    std::string m_label;

    unsigned int m_width, m_height; // size of icon
    int m_x, m_y;          // top left position of icon
	
    FbTk::FbWindow m_icon_window; 
    FbTk::FbWindow m_label_window;
    Pixmap m_icon_pixmap;
    Pixmap m_icon_mask;
    static CacheList m_cachelist;
    IconConfig &m_conf;
};

}; // end namespace FbDesk

#endif // FBDESK_ICON_HH
