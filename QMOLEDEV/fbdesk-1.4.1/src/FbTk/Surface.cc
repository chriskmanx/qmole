// Surface.cc for FbTk
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

#include "Surface.hh"

#include "App.hh"

#include <X11/Xutil.h>

#include <string>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <cstring>

using namespace std;

namespace FbTk {

Surface::Surface(unsigned int width, unsigned int height, int bpp, char *data):
    m_ximage(0), m_color_key(0), m_use_color_key(false) {
    // check for invalid values
    if (bpp == 0 || width == 0 || height == 0) {
        cerr<<"Invalid argument for Surface"<<endl;
        return;
    }

    Display *disp = FbTk::App::instance()->display();

    // get valid formats
    int formatcount;
    XPixmapFormatValues *formatlist = XListPixmapFormats(disp, &formatcount);
        
    if (formatlist == 0)
        throw std::string("FbTk::Surface > Cant get screen format list");

    int format = -1;
    for (int i=0; i<formatcount; ++i) {
        if (formatlist[i].depth == bpp) {
            format = i;
            break;
        }
    }

    if (format < 0) {
        cerr<<"no valid formats for surface!"<<endl;
        return;
    }
    
    char *imgdata = data;

    if (imgdata == 0) {
        imgdata = new char[width * height * formatlist[format].bits_per_pixel];

#ifdef DEBUG
        cerr<<"formatlist bits per pixel = "<<formatlist[format].bits_per_pixel<<endl;
        cerr<<"wanted bpp = "<<bpp<<endl;
#endif // DEBUG
    }

    m_ximage = XCreateImage(disp,
                            DefaultVisual(disp, DefaultScreen(disp)),
                            bpp,
                            ZPixmap, 0,
                            imgdata,
                            width, height,
                            32, 0);

    XFree(formatlist);

    if (m_ximage != 0)
        XInitImage(m_ximage);
    m_bpp = bpp; // set interal bpp
#ifdef DEBUG
    cerr<<"m_ximage="<<m_ximage<<endl;
    fprintf(stderr, "m_ximage->data = %p\n", m_ximage->data);
    cerr<<"bytes per line = "<<bytesPerLine()<<endl;
#endif // DEBUG
}

Surface::~Surface() {
    if (m_ximage != 0)
        XDestroyImage(m_ximage);
}

void Surface::putPixel(int x, int y, unsigned int color) {
    if (m_ximage == 0)
        return;
    XPutPixel(m_ximage, x, y, color);

}

void Surface::blit(Drawable dest_drawable, GC gc, 
                   int src_x, int src_y, int dest_x, int dest_y,
                   unsigned int width, unsigned int height) {
    if (m_ximage == 0)
        return;

    XPutImage(FbTk::App::instance()->display(), dest_drawable, gc,
              m_ximage,
              src_x, src_y,
              dest_x, dest_y,
              width, height);
}

//TODO: more checks
void Surface::blit(Surface &dest_srf, int src_x, int src_y, int dest_x, int dest_y, 
                   unsigned int width, unsigned int height) {
    if (m_ximage == 0 || dest_srf.m_ximage == 0 || width == 0 || height == 0)
        return;

    unsigned int max_width = std::min(dest_srf.width(), static_cast<unsigned int>(m_ximage->width));
    unsigned int max_height = std::min(dest_srf.height(), static_cast<unsigned int>(m_ximage->height));

    if (width > max_width)
        width = max_width;
    if (height > max_height)
        height = max_height;

    if (src_x < 0)
        src_x = 0;
    if (src_y < 0)
        src_y = 0;

    for (int y = src_y; y < height; ++y) {
        for (int x = src_x; x < width; ++x) {
            long pixel = XGetPixel(m_ximage, x, y);
            // convert bpp down
            if (m_bpp == 32 && dest_srf.m_bpp == 16 && pixel != 0) {
                register int red = ((pixel & 0xFF0000) >> 8);
                register int green = ((pixel & 0xFF00) >> 5);
                register int blue = ((pixel & 0xFF) >> 3);
                int alpha = (pixel & 0xFF000000);
                pixel = (red & dest_srf.m_ximage->red_mask) | 
                    (green & dest_srf.m_ximage->green_mask) | 
                    (blue & dest_srf.m_ximage->blue_mask);
                pixel |= alpha;
            }
                
            XPutPixel(dest_srf.m_ximage, x, y, pixel);

        }
    }
}

void Surface::setColorKey(unsigned long colorkey, bool use) {
    m_color_key = colorkey;
    m_use_color_key = use;
}

int Surface::bpp() const {
    if (m_ximage == 0)
        return 0;
    return m_ximage->bits_per_pixel;
}

unsigned char *Surface::data() {
    if (m_ximage == 0)
        return 0;
    return (unsigned char *)(m_ximage->data);
}

const unsigned char *Surface::data() const {
    if (m_ximage == 0)
        return 0;
    return (unsigned char *)(m_ximage->data);
}

unsigned int Surface::bytesPerLine() const {
    if (m_ximage == 0)
        return 0;
    return m_ximage->bytes_per_line;
}

unsigned int Surface::width() const {
    if (m_ximage == 0)
        return 0;
    return m_ximage->width;
}

unsigned int Surface::height() const {
    if (m_ximage == 0)
        return 0;
    return m_ximage->height;
}

/**
   Create a pixmap with a mask from this surface 
*/
void Surface::createPixmap(Drawable d, Pixmap &pixmap, Pixmap &mask) const {
    // create a pixmap from this surface
    Display *disp = FbTk::App::instance()->display();
    pixmap = XCreatePixmap(disp, d, width(), height(), m_bpp);

    XPutImage(disp, pixmap,
              DefaultGC(FbTk::App::instance()->display(), 0),
              const_cast<XImage *>(m_ximage),
              0, 0,
              0, 0,
              width(), height());

    mask = 0;

    if (!m_use_color_key)
        return;
    // create mask if we have color key

    // creat 1 bit surface
    Surface one_bit(width(), height(), 1);
    for (int y=0; y<height(); y++) {
        for (int x=0; x<width(); x++) {
            if (XGetPixel(m_ximage, x, y) == m_color_key)
                XPutPixel(one_bit.m_ximage, x, y, 0);
            else
                XPutPixel(one_bit.m_ximage, x, y, 1);
        }
    }
    // set fg and bg in case we have an XYBitmap
    XGCValues values;
    values.foreground = 1;
    values.background = 0;
    mask = XCreatePixmap(disp, d, width(), height(), 1);
    GC gc = XCreateGC(disp, mask,
                      GCForeground | GCBackground, &values);

        
    XPutImage(disp, mask, gc, one_bit.m_ximage, 0, 0, 0, 0,
              width(), height());

    XFreeGC(disp, gc);

}

}; // end namespace FbTk
