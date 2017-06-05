// Surface.hh for fbtk
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

#ifndef FBTK_SURFACE_HH
#define FBTK_SURFACE_HH

#include <X11/Xlib.h>

namespace FbTk {

/**
   X Image wrapper
*/
class Surface {
public:
    /// create a surface from a drawable, ie "screenshot" 
    Surface(Drawable src_drawable, int x, int y, 
            unsigned int width, unsigned int height, int bpp);
    /// create surface either from data or realloc
    Surface(unsigned int width, unsigned int height, int bpp, char *data=0);
    ~Surface();
    /// put single pixel on surface
    void putPixel(int x, int y, unsigned int color);
    /// blit to X Drawable
    void blit(Drawable dest_drawable, GC gc, int src_x, int src_y, int dest_x, int dest_y,
              unsigned int width, unsigned int height);
    void blit(Surface &dest_srf, int src_x, int src_y, int dest_x, int dest_y, 
              unsigned int width, unsigned int height);
    void setColorKey(unsigned long colorkey, bool use);
    bool hasColorKey() const { return m_use_color_key; }
    unsigned long getColorKey() const { return m_color_key; }
    unsigned int width() const;
    unsigned int height() const;
    int bpp() const;
    unsigned char *data();
    const unsigned char *data() const;
    unsigned int bytesPerLine() const;

    void createPixmap(Drawable d, Pixmap &pm, Pixmap &mask) const;
private:
    XImage *m_ximage;
    int m_bpp;
    unsigned long m_color_key;
    bool m_use_color_key;
};

};

#endif // FBTK_SURFACE_HH
