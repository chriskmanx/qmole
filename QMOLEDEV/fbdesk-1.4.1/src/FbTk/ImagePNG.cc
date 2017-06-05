// ImagePNG.cc for FbTk
// Copyright (c) 2002 - 2003 Henrik Kinnunen (fluxgen at users.sourceforge.net)
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

#include "ImagePNG.hh"

#include "Surface.hh"
#include "App.hh"
#include "PixmapWithMask.hh"

#include <png.h>
#include <cstdio>
#include <iostream>
using namespace std;

namespace {

void convert16to32(char *data, FbTk::Surface &srf) {
    unsigned short *realdata = (unsigned short *)data;
    for (int y=0, offset=0; y<srf.height(); ++y) {
        for (int x=0; x < srf.width(); ++x, ++offset) {
            srf.putPixel(x, y, realdata[offset]);
        }
    }
}

class PngHelper {
public:
    PngHelper():png_ptr(0), info_ptr(0) {
        png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
        if (png_ptr == 0)
            return;
        info_ptr = png_create_info_struct(png_ptr);
    }

    ~PngHelper() {
        png_destroy_read_struct(&png_ptr, &info_ptr, 0);
    }

    inline png_structp png() { return png_ptr; }
    inline png_infop info() { return info_ptr; }
private:
    png_structp png_ptr;
    png_infop info_ptr;
};

}

namespace FbTk {

ImagePNG::ImagePNG() {
    FbTk::Image::registerType("PNG", *this);
}

ImagePNG::~ImagePNG() {
    FbTk::Image::remove(*this);
}

PixmapWithMask *ImagePNG::load(const std::string &filename, int screen_num) const {
    if (filename.empty())
        return 0;
#ifdef DEBUG
    cerr<<"Image PNG loading: "<<filename<<endl;
#endif // DEBUG
    FILE *fp = fopen(filename.c_str(), "rb");
    if (fp == 0)
        return 0;

    // check header
    unsigned char tag[4];
    fread(tag, 1, 4, fp);
    if (!png_check_sig(tag, 4)) {
        fclose(fp);
        return 0;
    }

    fseek(fp, 0, 0);

    // get png struct
    PngHelper png;
    if (png.png() == 0 || png.info() == 0) {
        fclose(fp);
        return 0;
    }

    if (setjmp(png.png()->jmpbuf)) {
        fclose(fp);
        return 0;
    }
 
    png_init_io(png.png(), fp);
    png_read_info(png.png(), png.info());

    int bit_depth, color_type, interlace_type;
    png_uint_32 w, h;
    png_get_IHDR(png.png(), png.info(), &w, &h,
                 &bit_depth, &color_type,
                 &interlace_type, 0, 0);
#ifdef DEBUG    
    cerr<<png.info()->width<<", "<<png.info()->height<<endl;
    cerr<<"bit_depth = "<<(int)png.info()->bit_depth<<endl;
    cerr<<"bytes per pixel = "<<((int)png.info()->bit_depth>>3)<<endl;
    cerr<<"pixel depth = "<<(int)png.info()->pixel_depth<<endl;
    cerr<<"rowbytes = "<<png.info()->rowbytes<<endl;
    cerr<<"Color type = ";
    switch (color_type) {
    case PNG_COLOR_TYPE_GRAY:
        cerr<<"PNG_COLOR_TYPE_GRAY"<<endl;
        break;
    case PNG_COLOR_TYPE_GRAY_ALPHA:
        cerr<<"PNG_COLOR_TYPE_GRAY_ALPHA"<<endl;
        break;
    case PNG_COLOR_TYPE_PALETTE:
        cerr<<"PNG_COLOR_TYPE_PALETTE"<<endl;
        break;
    case PNG_COLOR_TYPE_RGB:
        cerr<<"PNG_COLOR_TYPE_RGB"<<endl;
        break;
    case PNG_COLOR_TYPE_RGB_ALPHA:
        cerr<<"PNG_COLOR_TYPE_RGB_ALPHA"<<endl;
        break;
    }
#endif // DEBUG

    

    // convert to rgb
    if (color_type == PNG_COLOR_TYPE_PALETTE && bit_depth <= 8) {
        png_set_expand(png.png());
        png.info()->pixel_depth = 8;
    }
    // convert to rgb
    if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8) {
        png_set_expand(png.png());
        color_type = PNG_COLOR_TYPE_RGB;
    }

    // convert gray to rgb
    if (color_type == PNG_COLOR_TYPE_GRAY ||
        color_type == PNG_COLOR_TYPE_GRAY_ALPHA) {
        png_set_gray_to_rgb(png.png());
        png.info()->pixel_depth = 8;
        color_type = PNG_COLOR_TYPE_RGB;
    }

    // swap order of rgb to bgr
    if (color_type == PNG_COLOR_TYPE_RGB ||
        color_type == PNG_COLOR_TYPE_RGB_ALPHA)
        png_set_bgr(png.png());

    // expand to 4 bytes
    if (bit_depth == 8 && color_type == PNG_COLOR_TYPE_RGB) {
        png_set_filler(png.png(), 0, PNG_FILLER_AFTER);
        png.info()->rowbytes = png.info()->width * 4;
        png.info()->pixel_depth = 32;
    }
    
    if (png.info()->pixel_depth == 4 ||
        png.info()->pixel_depth == 8)
        return 0;

    // create memory to hold rows
    png_bytep *row_pointers = new (nothrow) png_bytep[png.info()->height];
    if (row_pointers == 0) {
        fclose(fp);
        return 0;
    }

    for (unsigned int row = 0; row < png.info()->height; ++row) {
        row_pointers[row] = new png_byte[png.info()->rowbytes];
    }

    // get transparent pixel
    png_bytep trans = 0;
    int num_trans = 0;
    png_color_16p trans_values = 0;
    int trans_color = 0;

    if (png_get_valid(png.png(), png.info(), PNG_INFO_tRNS)) { 
        png_get_tRNS(png.png(), png.info(), &trans, &num_trans, &trans_values); 
        for(int i = 0; i < num_trans; i++) {
            if(trans[i] == 0) {
                trans_color = i;
                break;
            }
        }
                    
    }
    // read image from file
    png_read_image(png.png(), row_pointers);
    

    png_read_end(png.png(), png.info());    

    fclose(fp);

    // clear linear memory
    char *data = new char[png.info()->rowbytes * png.info()->height];
    for (int offset=0, y = 0; y < png.info()->height; y++) {
        for (int x = 0; x < png.info()->rowbytes; x++, offset++) {
            data[offset] = row_pointers[y][x];
        }
    }

    FbTk::Surface *srf = new (nothrow) FbTk::Surface(png.info()->width, png.info()->height,
                                                     png.info()->pixel_depth);

    if (srf == 0)
        return 0;


    // finaly copy data to surface
    switch (png.info()->pixel_depth) {
    case 16:
        convert16to32(data, *srf);
        break;
    case 24:
    case 32:
        memcpy(srf->data(), data, png.info()->height * png.info()->rowbytes);
        break;
    default:
        cerr<<"ImagePNG: Can't convert from "<<(int)png.info()->pixel_depth<<" to 32bpp."<<endl;
        break;
    }
    
    delete data;

    for (unsigned int row = 0; row < srf->height(); ++row) {
        delete [] row_pointers[row];
    }

    delete [] row_pointers;
    Pixmap pm, mask;
    Drawable screen_win = RootWindow(FbTk::App::instance()->display(), screen_num);
    int screen_depth = DefaultDepth(FbTk::App::instance()->display(), screen_num);
    FbTk::Surface screen_surf(srf->width(), srf->height(), screen_depth);
    
    srf->blit(screen_surf, 0, 0, 0, 0, 
              srf->width(), srf->height());

    if (num_trans > 0) {
        screen_surf.setColorKey(trans_color, true);
    } else {

        long i = 0x44332211;
        unsigned char* a = (unsigned char*) &i;
        bool big_endian = (*a != 0x11);
        int alphacolor = ((png.info()->channels != 4) ? 0xFF000000 : 0);
        if (big_endian) {
            int shift = ((png.info()->channels == 4) ? 0 : 8);
            alphacolor = (0xFF >> shift);    
        }
        screen_surf.setColorKey(alphacolor, true);
    }

   
    screen_surf.createPixmap(screen_win, pm, mask);
    delete srf;

    return new PixmapWithMask(pm, mask);
}

} // end namespace FbTk
