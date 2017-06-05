// FbDesk.cc
// Copyright (c) 2002-2004 Henrik Kinnunen (fluxgen at users.sourceforge.net)
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

#ifndef FBDESK_HH
#define FBDESK_HH


#include "InputField.hh"
#include "Icon.hh"
#include "IconConfig.hh"

#include "FbTk/Font.hh"
#include "FbTk/Resource.hh"
#include "FbTk/EventHandler.hh"
#include "FbTk/NotCopyable.hh"
#include "FbTk/Menu.hh"
#include "FbTk/MenuTheme.hh"
#include "FbTk/ImageControl.hh"


#include <string>
#include <list>

namespace FbDesk {

class FbDesk:private FbTk::NotCopyable, public FbTk::EventHandler {
public:
    explicit FbDesk(const char *config_filename=0);
    ~FbDesk();

    bool load(const std::string &config_filename);

    /// event loop
    void handleEvent(XEvent &event);
    void buttonPressEvent(XButtonEvent &ev);
    void buttonReleaseEvent(XButtonEvent &ev);
    void exposeEvent(XExposeEvent &ee);
    void motionNotifyEvent(XMotionEvent &ev);
    void clientMessageEvent(XClientMessageEvent &ev);
    /// execute command
    void execute(const std::string &commando) const;
    FbTk::Font &font() { return m_font; }

private:
    void updateIcons();
    void loadTheme();
    Icon *findIcon(Window w);
    bool loadIcons(const std::string &filename);
    bool saveIcons(const std::string &filename) const;
    void deleteIcons();
    void reloadConfig();

    /// create a new icon with default values
    void createIcon();
    /// delete last selected icon
    void deleteLastIcon();
    /// initiate iconmenu
    void setupIconMenu();
    void setCommandFromInput();
    void setLabelFromInput();
    void setIconLabel();
    void setIconCommand();
    void updateAlpha();
    void lockPositions();

    FbTk::Font m_font;
    typedef std::list<Icon *> IconList;
    IconList m_iconlist;

    Icon *m_selected_icon; ///< current icon selected for motion 
    Icon *m_last_icon; ///< last icon selected
    
    std::string m_config_filename;
    int m_button_pos_x, m_button_pos_y;
    int m_screen_width, m_screen_height;

    unsigned int m_grid_width, m_grid_height;
    FbTk::MenuTheme m_menutheme;
    FbTk::ImageControl m_imagectrl;
    FbTk::Menu m_iconmenu;
    InputField m_inputfield;

    /// the text color 
    FbTk::Color m_textcolor;
    /// the background color the text label
    FbTk::Color m_text_background_color;

    FbTk::ResourceManager m_resmanager;
    FbTk::Resource<std::string> m_themefile;
    FbTk::Resource<int> m_doubleclick_interval;
    FbTk::Resource<std::string> m_iconfile;
    // TODO: some of these should probably be moved to IconConfig
    FbTk::Resource<std::string> m_fontname;
    FbTk::Resource<std::string> m_textcolorname;
    FbTk::Resource<std::string> m_textbackgroundname;
    FbTk::Resource<int> m_grid_snap_x, m_grid_snap_y;
    FbTk::Resource<Icon::TextPlacement> m_textplacement;
    FbTk::Resource<int> m_text_alpha, m_icon_alpha;
    FbTk::Resource<bool> m_lock_positions;
    IconConfig m_icon_config;
}; 

} // end namespace FbDesk

#endif // FBDESK_HH
