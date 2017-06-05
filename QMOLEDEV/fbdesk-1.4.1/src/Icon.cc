// Icon.cc for fbdesk
// Copyright (c) 2002 - 2004 Henrik Kinnunen (fluxgen at fluxbox.org)
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

#include "Icon.hh"

#include "FbTk/Font.hh"
#include "FbTk/FbWindow.hh"
#include "FbTk/Color.hh"
#include "FbTk/App.hh"
#include "FbTk/Image.hh"
#include "FbTk/PixmapWithMask.hh"

#include "default.xpm"

#include "IconConfig.hh"

#include <X11/xpm.h>
#include <X11/Xatom.h>
#include <assert.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif // HAVE_CONFIG_H

#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif // HAVE_SHAPE

#include <iostream>
using namespace std;

namespace FbDesk {

using namespace FbTk;

Icon::CacheList Icon::m_cachelist;

Icon::Icon(const std::string &imagefilename, 
           const std::string &command, const std::string &label):
    m_command(command),
    m_image_filename(imagefilename),
    m_label(label),
    m_width(32), m_height(32), // default size
    m_x(0), m_y(0),            // default pos
    m_icon_window(0, // screen number
                  0, 0, // pos
                  32, 32, // size
                  // event mask
                  ButtonReleaseMask | ButtonPressMask | PointerMotionMask | ExposureMask,
                  //                 PropertyChangeMask
                  false),
    m_label_window(0, // screen number
                   0, 0, // pos
                   10, 10, // size
                   // event mask
                   ButtonPressMask | ButtonReleaseMask | PointerMotionMask | ExposureMask,
                   false), //override redirect
    m_icon_pixmap(0), m_icon_mask(0),
    m_conf(IconConfig::instance() ) {
 
    setupDragAndDrop();
    setupLayer();

    m_icon_window.setAlpha(m_conf.iconAlpha());
    m_label_window.setAlpha(m_conf.textAlpha());
    m_label_window.setBackgroundColor(m_conf.background());
    loadImage(imagefilename); // load file
    
}

Icon::~Icon() {
    removeFromCache(m_image_filename);
}

void Icon::show() {
    m_icon_window.show();
    m_label_window.show();
    move(m_x, m_y);
}

void Icon::setCommand(const std::string &command) {
    m_command = command;
}

void Icon::setImage(const std::string &imagefilename) {
	
    if (loadImage(imagefilename)) {
        removeFromCache(m_image_filename); // remove from cache and set new filename
        m_image_filename = imagefilename;
    }

}

void Icon::setLabel(const std::string &label) {
    m_label = label;
    updateTextAlignment(); // realign it
    updateLabel();
}

// move this icon to X, Y pos
void Icon::move(int x, int y) {
    m_icon_window.move(x, y);
    m_x = x;
    m_y = y;
    updateTextAlignment();
    if (m_label_window.alpha() < 255)
        updateLabel();
    if (m_icon_window.alpha() < 255) {
        m_icon_window.clear();
        m_icon_window.updateTransparent();
    }
}

void Icon::resize(unsigned int width, unsigned int height) {
    if (width == 0)
        width = 1;
    if (height == 0)
        height = 1;
    m_width = width;
    m_height = height;
    m_icon_window.resize(width, height);
    
}

void Icon::update() {
    m_icon_window.setAlpha(m_conf.iconAlpha());
    m_label_window.setAlpha(m_conf.textAlpha());

    updateLabel();
    updateWindow();
}

void Icon::raise() {
    m_label_window.raise();
    m_icon_window.raise();

}

void Icon::lower() {
    m_label_window.lower();
    m_icon_window.lower();
}

void Icon::redrawLabel() {
    m_label_window.clear();
    m_label_window.updateTransparent();

    if (m_label.size() != 0) {
        // setup start position of text
        int y_start = m_conf.font().ascent();
        int x_start = 2;
        if (m_conf.textPlacement() == TEXTPLACE_LEFT ||
            m_conf.textPlacement() == TEXTPLACE_RIGHT) {
            x_start = y_start;
            y_start = m_conf.font().textWidth(m_label.c_str(), m_label.size());
        }

        m_conf.font().drawText(m_label_window, m_label_window.screenNumber(), 
                               m_conf.gc(),
                               m_label.c_str(), m_label.size(),
                               x_start, y_start);
    }
}

void Icon::updateLabel() {
    if (m_conf.textPlacement() == TEXTPLACE_LEFT ||
        m_conf.textPlacement() == TEXTPLACE_RIGHT) {
        // with two extra pixels on each side 
        m_label_window.resize(m_conf.font().height(), 
                              m_conf.font().textWidth(m_label.c_str(), m_label.size()) + 4);
    } else {
        // with two extra pixels on each side 
        m_label_window.resize(m_conf.font().textWidth(m_label.c_str(), 
                                                m_label.size()) + 4, m_conf.font().height()); 
    }
    redrawLabel();
}

void Icon::updateWindow() {
    m_icon_window.resize(m_width, m_height);
    if (m_icon_pixmap != 0) {
        m_icon_window.setBackgroundPixmap(m_icon_pixmap);
    }
    
    m_icon_window.clear();
    m_icon_window.updateTransparent();
    
}

bool Icon::loadImage(const std::string &image_filename) {
    Display *disp = FbTk::App::instance()->display();

    Cache *cache_item = findCache(image_filename);
    if (cache_item != 0) {
        cache_item->m_count++;
        m_icon_pixmap = cache_item->m_pixmap;
        m_icon_mask = cache_item->m_mask;

        resize(cache_item->m_width, cache_item->m_height);
#ifdef HAVE_SHAPE
        XShapeCombineMask(disp, m_icon_window.window(),
                          ShapeBounding, 0, 0, cache_item->m_mask, ShapeSet);
#endif // HAVE_SHAPE

        return true;
    }

    Pixmap pixmap, mask;
    unsigned int pm_width = 1, pm_height = 1;
    
    Window rootwindow = m_icon_window.window();

    // try loading filename first
    std::auto_ptr<FbTk::PixmapWithMask> img(FbTk::Image::load(image_filename, 
                                                              m_icon_window.screenNumber()));
    if (img.get() == 0) {
        XpmAttributes xpm_attr;
        xpm_attr.valuemask = 0;
        mask = 0;
        //        pixmap = 0;
        // lets try xpm loading
        int retvalue = XpmCreatePixmapFromData(disp,
                                               rootwindow,
                                               default_xpm, &pixmap, &mask, &xpm_attr);
        
        cerr<<"Load pixmap failed! ("<<image_filename<<")"<<endl;
        cerr<<"Using default pixmap"<<endl;
        
        if (pixmap == None || retvalue != XpmSuccess) // fatal error
            throw string("FbDesk::Icon: Can't load default pixmap!");

        pm_width = xpm_attr.width;
        pm_height = xpm_attr.height;
        
    } else {
        // ok image loading successfull
        pm_width = img->pixmap().width();
        pm_height = img->pixmap().height();
        pixmap = img->pixmap().release();
        mask = img->mask().release();
    }

    // create new cache item and add it to cachelist
    Cache *new_cache_item = new Cache;
    new_cache_item->m_pixmap = pixmap;
    new_cache_item->m_mask = mask;
    new_cache_item->m_filename = image_filename;
    new_cache_item->m_count = 1;
    new_cache_item->m_width = pm_width;
    new_cache_item->m_height = pm_height;

    m_cachelist.push_back(new_cache_item);

    m_icon_pixmap = pixmap;
    m_icon_mask = mask;

    resize(pm_width, pm_height);

#ifdef HAVE_SHAPE
    XShapeCombineMask(m_icon_window.display(), m_icon_window.window(),
                      ShapeBounding, 0, 0, mask, ShapeSet);
#endif // HAVE_SHAPE

    return true;
}

Icon::Cache * const Icon::findCache(const std::string &filename) {
    CacheList::iterator cache_item = m_cachelist.begin();
    CacheList::iterator cache_item_end = m_cachelist.end();
    for (; cache_item != cache_item_end; ++cache_item) {
        if ((*cache_item)->m_filename == filename)
            return *cache_item;
    }
	
    return 0; // we didn't find filename in cache
}

void Icon::removeFromCache(const std::string &filename) {
    Cache *item = findCache(filename);
    if (item) {
        item->m_count--;
        if (item->m_count == 0) { // if nobody else is using this one remove it from the list
            m_cachelist.remove(item);
            XFreePixmap(App::instance()->display(), item->m_pixmap);
            XFreePixmap(App::instance()->display(), item->m_mask);
            delete item;
            item = 0;
        }
    }
}

void Icon::setupDragAndDrop() {
    /*
    static int xnd_version = 3;
    Display *disp = FbTk::App::instance()->display();

    if (s_xdnd_atom == 0)
        s_xdnd_atom = XInternAtom(disp, "XdndAware", false);


    m_icon_window.changeProperty(s_xdnd_atom, XA_ATOM,
                                 32, PropModeReplace,
                                 (unsigned char *)(&xnd_version), 1);
                    
    */
}

void Icon::setupLayer() {
    Display *disp = FbTk::App::instance()->display();

    Atom type = XInternAtom(disp, "_NET_WM_WINDOW_TYPE", false);
    Atom state = XInternAtom(disp, "_NET_WM_STATE", false);
    
    Atom state_atoms[] = {
        XInternAtom(disp, "_NET_WM_STATE_BELOW", false),
        XInternAtom(disp, "_NET_WM_STATE_HIDDEN", false),
        XInternAtom(disp, "_NET_WM_STATE_SKIP_PAGER", false),
        XInternAtom(disp, "_NET_WM_STATE_SKIP_TASKBAR", false),
        XInternAtom(disp, "_NET_WM_STATE_STICKY", false)
    };

    Atom type_atoms =  XInternAtom(disp, "_NET_WM_WINDOW_TYPE_DESKTOP", false);

    m_icon_window.changeProperty(state, XA_ATOM,
                                 32, PropModeReplace,
                                 (unsigned char *)(state_atoms), 5);
    m_icon_window.changeProperty(type, XA_ATOM,
                                 32, PropModeReplace,
                                 (unsigned char *)(&type_atoms), 1);

    m_label_window.changeProperty(state, XA_ATOM,
                                  32, PropModeReplace,
                                  (unsigned char *)(state_atoms), 5);
    
    m_label_window.changeProperty(type, XA_ATOM,
                                  32, PropModeReplace,
                                  (unsigned char *)(&type_atoms), 1);

}

void Icon::updateTextAlignment() {

    int textwidth = m_conf.font().textWidth(m_label.c_str(), m_label.size());
    if (m_label.size() == 0 || textwidth == 0)
        return;

    switch (m_conf.textPlacement()) {
    case TEXTPLACE_BOTTOM:
        // center text under icon	
        m_label_window.move(m_x + m_icon_window.width()/2 - textwidth/2, m_y + height());
        break;
    case TEXTPLACE_TOP:
        // center text above icon
        m_label_window.move(m_x + m_icon_window.width()/2 - textwidth/2, m_y - m_conf.font().height());
        break;
    case TEXTPLACE_LEFT:
        m_label_window.move(m_x - m_conf.font().height(), m_y + m_icon_window.height()/2 - textwidth/2);
        break;
    case TEXTPLACE_RIGHT:
        m_label_window.move(m_x + width(), m_y + m_icon_window.height()/2 - textwidth/2);
        break;
    }

}


}; // end namespace FbDesk
