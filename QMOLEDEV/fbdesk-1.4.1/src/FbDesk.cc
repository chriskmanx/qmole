// FbDesk.cc
// Copyright (c) 2002 - 2006 Henrik Kinnunen (fluxgen at users.sourceforge.net)
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

#include "FbDesk.hh"

#include "InputField.hh"
#include "IntResMenuItem.hh"

#include "FbTk/App.hh"
#include "FbTk/Font.hh"
#include "FbTk/EventManager.hh"
#include "FbTk/SimpleCommand.hh"
#include "FbTk/StringUtil.hh"
#include "FbTk/XrmDatabaseHelper.hh"
#include "FbTk/MenuSeparator.hh"

#include <iostream>
#include <fstream>
#include <algorithm>
#include <unistd.h>
#include <cstdio>
#include <string.h>

using namespace std;
using namespace FbTk;


namespace FbDesk {

FbDesk::FbDesk(const char *config_filename):
    m_font("fixed"),
    m_selected_icon(0),
    m_last_icon(0),
    m_config_filename(config_filename ? config_filename : ""),
    m_button_pos_x(32),
    m_button_pos_y(32),
    m_menutheme(0), // menu theme on screen 0
    m_imagectrl(0), // image control on screen 0
    m_iconmenu(m_menutheme, m_imagectrl),
    m_inputfield(0, m_menutheme.frameFont(), m_menutheme.frameTextGC().gc(),
                 m_menutheme.frameTexture().color()),
    m_textcolor("black", 0),
    m_resmanager(0, false),
    m_themefile(m_resmanager, "", "session.styleFile", "Session.StyleFile"),
    m_doubleclick_interval(m_resmanager, 200, "fbdesk.doubleClickInterval", "FbDesk.DoubleClickInterval"),
    m_iconfile(m_resmanager, "~/.fluxbox/fbdesk.icons", "fbdesk.iconFile", "FbDesk.IconFile"),
    m_fontname(m_resmanager, "fixed", "fbdesk.font", "FbDesk.Font"),
    m_textcolorname(m_resmanager, "black", "fbdesk.textColor", "FbDesk.TextColor"),
    m_textbackgroundname(m_resmanager, "white", "fbdesk.textBackground", "FbDesk.TextBackground"),
    m_grid_snap_x(m_resmanager, 5, "fbdesk.snapX", "FbDesk.SnapX"),
    m_grid_snap_y(m_resmanager, 5, "fbdesk.snapY", "FbDesk.SnapY"),
    m_textplacement(m_resmanager, Icon::TEXTPLACE_BOTTOM, "fbdesk.textPlacement", "FbDesk.TextPlacement"),
    m_text_alpha(m_resmanager, 0, "fbdesk.textAlpha", "FbDesk.TextAlpha"),
    m_icon_alpha(m_resmanager, 255, "fbdesk.iconAlpha", "FbDesk.IconAlpha"),
    m_lock_positions(m_resmanager, false, "fbdesk.lockPositions", "FbDesk.LockPositions"),
    m_icon_config(m_menutheme.screenNum()) {

    m_icon_config.setFont(m_font);
    reloadConfig();
    setupIconMenu();

    Screen *scr = ScreenOfDisplay(FbTk::App::instance()->display(), 0);
    m_screen_width = WidthOfScreen(scr);
    m_screen_height = HeightOfScreen(scr);


}

FbDesk::~FbDesk() {
    // save icon data
    saveIcons(*m_iconfile);
    // save to default resource file
    m_resmanager.save( 0 );

    // delete all icons
    deleteIcons();
}


void FbDesk::loadTheme() {
    cerr<<"loading style: " << *m_themefile <<endl;

    if (!FbTk::ThemeManager::instance().load(*m_themefile, ""))
        cerr<<"Failed to load style: "<< *m_themefile<<endl;

    // update for new theme
    m_iconmenu.reconfigure();
}

/**
   Destroy old icons and reload icons data and recreate icons
*/
bool FbDesk::loadIcons(const std::string &config_filename) {
  cerr<< "loading icons from file " << config_filename << endl;
    ifstream iconfile(StringUtil::expandFilename(config_filename).c_str());

    if (!m_iconlist.empty()) {
        // delete old icons
        deleteIcons();
    }

    vector<int> iconpos;

    while (iconfile && !iconfile.eof()) {
        string line;
        getline(iconfile, line);

        const char* cLine = line.c_str();
        char cSafeLine[1024];
        memset(cSafeLine,0,1024);
        strcpy(cSafeLine,cLine);
        line = cSafeLine;

        int iLen = strlen(cLine);
        int iDesktoplen = strlen("[Desktop Entry]");
        cerr << "Read: " << cLine << endl;
        if (strcmp("[Desktop Entry]", cLine)==0) {
	    cerr<<"found a desktop entry"<<endl;
            //continue;
        }

        int icon_x = 0, icon_y = 0; // position of icon
        string command;       // command for icon
        string label;         // icon label
        string icon_filename; // icon filename

        while (!iconfile.eof()) {
            line = "";
            getline(iconfile, line); // get next line

            cerr << "Read next line: " << line << endl;

            //remove any whitespace in the begining
            FbTk::StringUtil::removeFirstWhitespace(line);

            // get first position after option
            size_t first_pos = line.find_first_of("=");
			
            string option;
            if (first_pos != string::npos)
                option = line.substr(0, first_pos);
            else
                option = line;

            if (option.size() == 0)
                continue;

            // get option value
            string option_value;
            if (first_pos != string::npos && first_pos != line.size() - 1) {                
                option_value = static_cast<const char *>(line.c_str() + first_pos + 1);
            }

            FbTk::StringUtil::removeFirstWhitespace(option);
            FbTk::StringUtil::removeTrailingWhitespace(option);

            cerr << "Option: [" << option << "] Value: [" << option_value <<"]" << endl;

            char cOption[1024];
            memset(cOption,0,1024);
            char cValue[1024];
            memset(cValue,0,1024);
            strcpy(cOption,option.c_str());
            if (strcmp(cOption,"Name")==0) {
                label = option_value;
            } else if (strcmp(cOption,"Exec")==0) {
                command = option_value;
            } else if (strcmp(cOption,"Icon")==0) {
                icon_filename = option_value;
            } else if (strcmp(cOption,"Pos")==0) {
                sscanf(option_value.c_str(), "%d %d", &icon_x, &icon_y);
            } else if (strcmp(cOption,"[end]")==0) {
                if (command.size() == 0 && icon_filename.size() == 0
                    && label.size() == 0) {
                    cerr<<"Warning: All options for icon are empty. Icon not added."<<endl;
                    continue;
                }

                // create new icon from current settings
                cerr << "Create an icon for " << label << " from " << icon_filename << endl;
                Icon *i = new Icon(StringUtil::expandFilename(icon_filename).c_str(),
                                   command, label);
                // add to event manager
                FbTk::EventManager::instance()->add(*this, i->iconWindow());
                FbTk::EventManager::instance()->add(*this, i->labelWindow());
                // save position
                iconpos.push_back(icon_x);
                iconpos.push_back(icon_y);
                // add it to list
                m_iconlist.push_back(i);
                // clear options
                icon_filename = label = command = "";
                icon_x = icon_y = 0;
            } // end if 

        } // end while
		
    } // end while

    // show all icons
    cerr << "show " << m_iconlist.size() << " icons" << endl;
    IconList::iterator icon_it = m_iconlist.begin();
    IconList::iterator icon_it_end = m_iconlist.end();
    for (int i = 0; icon_it != icon_it_end; ++icon_it, i += 2) {
      int xpos = iconpos[i];
      int ypos = iconpos[i+1];
      
      (*icon_it)->show();

      cerr << "Move icon to x y " << xpos << " " << ypos << endl;
      (*icon_it)->move(xpos, ypos);
    }

    // create at lest one icon
    if (m_iconlist.empty()) {
        cerr<<"Warning: Empty icon file, creating one icon."<<endl;
        createIcon();
    }

    return true;
}

void FbDesk::handleEvent(XEvent &event) {
    switch (event.type) {
    case ClientMessage:
        clientMessageEvent(event.xclient);
        break;
    default:
        break;
    }
}

void FbDesk::buttonPressEvent(XButtonEvent &event) {
    m_selected_icon = findIcon(event.window);
    if (!m_selected_icon)
        return;
    m_selected_icon->raise();
    // save last mouse position
    m_button_pos_x = event.x_root;
    m_button_pos_y = event.y_root;
    
}

void FbDesk::buttonReleaseEvent(XButtonEvent &event) {
    static int button_time = 0;
    if (m_selected_icon == 0)
        return;

    m_last_icon = m_selected_icon;

    if (event.button == 3) {
        // show menu
        m_iconmenu.raise();
        m_iconmenu.move(event.x_root, event.y_root);
        m_iconmenu.show();

    } else if (event.button == 1) {
        // did we doubleclick?
        if (event.time - button_time <= *m_doubleclick_interval) {
            execute(m_selected_icon->command());
        } /* C.K. */
        else {
	  execute(m_selected_icon->command()); /* C.K. run anyway */
        }

        // don't show the icon over another window and make sure it's visible
        m_selected_icon->lower();
        int new_x = -1, new_y = -1;
        if (m_selected_icon->x() < 0)
            new_x = 0;
        if (m_selected_icon->x() >= m_screen_width)
            new_x = m_screen_width - m_selected_icon->width();

        if (m_selected_icon->y() < 0)
            new_y = 0;
        if (m_selected_icon->y() >= m_screen_height)
            new_y = m_screen_height - m_selected_icon->height();

        if (new_x >= 0)
            m_selected_icon->move(new_x, m_selected_icon->y());
        if (new_y >= 0)
            m_selected_icon->move(m_selected_icon->x(), new_y);
        // update label background
        m_selected_icon->redrawLabel();
    }

    m_selected_icon = 0; // unselect icon

    button_time = event.time; // save last timestamp
    saveIcons(*m_iconfile);
}

void FbDesk::exposeEvent(XExposeEvent &event) {
    Icon *exposed_icon = findIcon( event.window );
    if (exposed_icon != 0)
        exposed_icon->update();
}

void FbDesk::motionNotifyEvent(XMotionEvent &event) {

    // need a selected icon first, and not locked position
    if (*m_lock_positions || m_selected_icon == 0) 
        return;

    Display *disp = FbTk::App::instance()->display();
    Screen *scr = ScreenOfDisplay(disp, 0);

    // calculate new position, using inside pos and last mouse pos
    
    // snap to grid
    if (*m_grid_snap_x == 0)
        *m_grid_snap_x = 1;

    if (*m_grid_snap_y == 0)
        *m_grid_snap_y = 1;

    event.x_root /= *m_grid_snap_x;
    event.x_root *= *m_grid_snap_x;

    event.y_root /= *m_grid_snap_y;
    event.y_root *= *m_grid_snap_y;

    int new_x = m_selected_icon->x() - m_button_pos_x + event.x_root;
    int new_y = m_selected_icon->y() - m_button_pos_y + event.y_root;

    // set the last known position of mouse
    m_button_pos_x = event.x_root;
    m_button_pos_y = event.y_root;

    // Make sure the icon is inside visible area
    if (new_x < 0)
        new_x = 0;
    else if (new_x >= m_screen_width)
        new_x = m_screen_width - 1;

    if (new_y < 0)
        new_y = 0;
    else if (new_y >= m_screen_height)
        new_y = m_screen_height - 1;

    // move the icon the new position
    if (m_selected_icon->x() != new_x ||
        m_selected_icon->y() != new_y) {
        m_selected_icon->move(new_x, new_y);
    }
}

void FbDesk::clientMessageEvent(XClientMessageEvent &event) {
    // TODO drag'n'drop
}

void FbDesk::execute(const std::string &commando) const {
    if (! fork()) {
        string displaystring("DISPLAY=");
        displaystring += DisplayString(FbTk::App::instance()->display());
        setsid();
        putenv((char *)displaystring.c_str());
        execl("/bin/sh", "/bin/sh", "-c", commando.c_str(), 0);
        exit(0);
    }
}

Icon *FbDesk::findIcon(Window w) {
    IconList::iterator it = m_iconlist.begin();
    IconList::iterator it_end = m_iconlist.end();
    for (; it != it_end; ++it) {
        if ((*it)->iconWindow() == w ||
            (*it)->labelWindow() == w) {
            return (*it);
        }
    }

    return 0; // we didn't find any icon
}

bool FbDesk::saveIcons(const std::string &filename) const {
    string realfilename(StringUtil::expandFilename(filename));
    ofstream ofile(realfilename.c_str());
    if (!ofile) {
        cerr<<"Warning: Failed to save icons to file: "<<realfilename<<endl;
        return false; // faild to open file
    }
    IconList::const_iterator it = m_iconlist.begin();
    IconList::const_iterator it_end = m_iconlist.end();
    for (; it != it_end; ++it) {		
        ofile<<"[Desktop Entry]"<<endl
             <<"Name="<<(*it)->label()<<endl
             <<"Exec="<<(*it)->command()<<endl
             <<"Icon="<<(*it)->imageFilename()<<endl
             <<"Pos= "<<(*it)->x()<<" "<<(*it)->y()<<endl
             <<"[end]"<<endl<<endl;
    }

    return true;
}

void FbDesk::deleteIcons() {	
    // destroy iconlist
    while (!m_iconlist.empty()) {
        FbTk::EventManager::instance()->remove(m_iconlist.back()->iconWindow());
        FbTk::EventManager::instance()->remove(m_iconlist.back()->labelWindow());

        delete m_iconlist.back();
        m_iconlist.pop_back();
    }
}

void FbDesk::reloadConfig() {

    m_resmanager.load(StringUtil::expandFilename(m_config_filename).c_str());
    // if style file is empty read fluxbox style
    if ( (*m_themefile).empty() ) {
        string filename(StringUtil::expandFilename("~/.fluxbox/init").c_str());
        ResourceManager rm(filename.c_str(), false);
        rm.addResource( m_themefile );
        rm.load(filename.c_str()); 
    }

    m_textcolor.setFromString(m_textcolorname->c_str(), 0);
    if (!m_text_background_color.
        setFromString(m_textbackgroundname->c_str(), 0)) {
        m_text_background_color.setFromString("white", 0);
    }

    // setup icon values
    m_icon_config.setTextColor(m_textcolor);
    m_icon_config.setTextBackground(m_text_background_color);
    m_icon_config.setTextPlacement(*m_textplacement);
    m_icon_config.setTextAlpha(*m_text_alpha);
    m_icon_config.setIconAlpha(*m_icon_alpha);

    if (*m_textplacement == Icon::TEXTPLACE_LEFT ||
        *m_textplacement == Icon::TEXTPLACE_RIGHT)
        m_font.rotate(90);
    else
        m_font.rotate(0);

    loadTheme();
    loadIcons(*m_iconfile); // reload icons
}

void FbDesk::createIcon() {
    // create a new icon where we last pressed
    Icon *ico = new Icon("", "", "default");
    FbTk::EventManager::instance()->add(*this, ico->iconWindow());
    FbTk::EventManager::instance()->add(*this, ico->labelWindow());
    ico->move(m_button_pos_x, m_button_pos_y);
    ico->show();
    m_iconlist.push_back(ico);
    m_selected_icon = 0;
}

void FbDesk::deleteLastIcon() {
    if (m_last_icon != 0 && 
        find(m_iconlist.begin(), m_iconlist.end(), m_last_icon) != m_iconlist.end()) {
        m_iconlist.remove(m_last_icon);
        delete m_last_icon;
        m_last_icon = 0;
    }
}

void FbDesk::setIconCommand() {
    m_iconmenu.hide();
    m_inputfield.setTitle("Set Command");
    if (m_last_icon != 0 &&
        find(m_iconlist.begin(), m_iconlist.end(), m_last_icon) != m_iconlist.end()) {
        m_inputfield.setText(m_last_icon->command());
    }

    m_inputfield.setOnEnter(new FbTk::SimpleCommand<FbDesk>(*this, &FbDesk::setCommandFromInput));
    m_inputfield.show();
    m_inputfield.move(m_button_pos_x, m_button_pos_y);
    m_inputfield.raise();
}

void FbDesk::setIconLabel() {
    m_iconmenu.hide();

    m_inputfield.setTitle("Set Label");
    if (m_last_icon != 0 &&
        find(m_iconlist.begin(), m_iconlist.end(), m_last_icon) != m_iconlist.end()) {
        m_inputfield.setText(m_last_icon->label());
    }

    m_inputfield.setOnEnter(new FbTk::SimpleCommand<FbDesk>(*this, &FbDesk::setLabelFromInput));
    m_inputfield.show();
    m_inputfield.move(m_button_pos_x, m_button_pos_y);
    m_inputfield.raise();
}

void FbDesk::setLabelFromInput() {
    if (m_last_icon != 0 &&
        find(m_iconlist.begin(), m_iconlist.end(), m_last_icon) != m_iconlist.end()) {
        m_last_icon->setLabel(m_inputfield.text());
        saveIcons(*m_iconfile);
    }
    m_inputfield.hide();
}

void FbDesk::setCommandFromInput() {
    if (m_last_icon != 0 &&
        find(m_iconlist.begin(), m_iconlist.end(), m_last_icon) != m_iconlist.end()) { 
        m_last_icon->setCommand(m_inputfield.text());
        saveIcons(*m_iconfile);
    }
    m_inputfield.hide();
}


class ToggleMenuItem:public MenuItem {
public:
    explicit ToggleMenuItem(const char *label,
                            bool &val ):
        MenuItem(label),
        m_value( val ) {
        setToggleItem(true);
    }  
    void click(int button, int time) { 
        setSelected(!m_value); 
        FbTk::MenuItem::click(button, time); 
    }

    void setSelected(bool val) {
        m_value = val;
    }
    bool isSelected() const {
        return m_value;
    }

private:
    bool &m_value;
};

void FbDesk::setupIconMenu() {
    using namespace FbTk;
    // setup menu items
    typedef RefCount<Command> CommandRef;
    //    RefCount<Command> show_iconbrowser(new SimpleCommand<IconBrowser>(m_iconbrowser, &IconBrowser::show));
    typedef SimpleCommand<FbDesk> FbDeskCmd;
    CommandRef new_icon(new FbDeskCmd(*this, &FbDesk::createIcon));
    CommandRef del_last_icon(new FbDeskCmd(*this, &FbDesk::deleteLastIcon));
    CommandRef set_command(new FbDeskCmd(*this, &FbDesk::setIconCommand));
    CommandRef set_label(new FbDeskCmd(*this, &FbDesk::setIconLabel));
    CommandRef reload_style(new FbDeskCmd(*this, &FbDesk::reloadConfig));
    CommandRef exit_fbdesk(new SimpleCommand<App>(*App::instance(), &App::end));
    //        select_icon_image_hide(select_icon_macro);

    m_iconmenu.setLabel("Icon menu");

    m_iconmenu.insert("Set command", set_command);
    m_iconmenu.insert("Set label", set_label);
    m_iconmenu.insert(new FbTk::MenuSeparator());
    m_iconmenu.insert("New icon", new_icon);
    m_iconmenu.insert("Delete icon", del_last_icon);
    m_iconmenu.insert(new FbTk::MenuSeparator());    
    // setup alpha menu
    FbTk::MenuItem *item = new IntResMenuItem("Text alpha", m_text_alpha, 
                                              0, 255, 
                                              m_iconmenu);
    CommandRef update_alpha(new FbTk::SimpleCommand<FbDesk>(*this, &FbDesk::updateAlpha));
    item->setCommand(update_alpha);
    m_iconmenu.insert(item);
    item = new IntResMenuItem("Icon alpha", m_icon_alpha, 
                              0, 255,
                              m_iconmenu);
    item->setCommand(update_alpha);
    m_iconmenu.insert(item);
    m_iconmenu.insert("Reload config", reload_style);
    m_iconmenu.insert(new FbTk::MenuSeparator());    
    m_iconmenu.insert(new ToggleMenuItem("Lock positions", *m_lock_positions) );
    m_iconmenu.insert(new FbTk::MenuSeparator());
    m_iconmenu.insert("Exit FbDesk", exit_fbdesk);
    // m_iconmenu.insert("Select icon image", select_icon_image_hide);
    m_iconmenu.reconfigure(); // update graphics
}

void FbDesk::updateAlpha() {
    m_resmanager.save(StringUtil::expandFilename(m_config_filename).c_str(),
                      StringUtil::expandFilename(m_config_filename).c_str()); // merge
    m_icon_config.setTextAlpha(*m_text_alpha);
    m_icon_config.setIconAlpha(*m_icon_alpha);
    updateIcons();
}

void FbDesk::updateIcons() {
    for_each(m_iconlist.begin(),
             m_iconlist.end(),
             mem_fun(&Icon::update));
}

}; // end namespace FbDesk

namespace FbTk {

template <>
void Resource<int>::setFromString(const char *str) {
    sscanf(str, "%d", &*(*this));
}

template <>
std::string Resource<int>::getString() const {
    char strval[256];
    sprintf(strval, "%d", *(*this));
    return std::string(strval);
}

template <>
void Resource<std::string>::setFromString(const char *str) {
    *(*this) = (str ? str : "");
}

template <>
std::string Resource<std::string>::getString() const {
    return (*this)->c_str();
}



template <>
std::string Resource<FbDesk::Icon::TextPlacement>::getString() const {
    switch (*(*this)) {
    case FbDesk::Icon::TEXTPLACE_LEFT:
        return "Left";
    case FbDesk::Icon::TEXTPLACE_RIGHT:
        return "Right";
    case FbDesk::Icon::TEXTPLACE_TOP:
        return "Top";
    case FbDesk::Icon::TEXTPLACE_BOTTOM:
        return "Bottom";
    }
    return "Bottom";

}

template <>
void Resource<FbDesk::Icon::TextPlacement>::setFromString(const char *str) {
    if (strcmp("Left", str) == 0) {
        *(*this) = FbDesk::Icon::TEXTPLACE_LEFT;
    } else if (strcmp("Right", str) == 0) {
        *(*this) = FbDesk::Icon::TEXTPLACE_RIGHT;
    } else if (strcmp("Top", str) == 0) 
        *(*this) = FbDesk::Icon::TEXTPLACE_TOP;
    else  if (strcmp("Bottom", str) == 0)
        *(*this) = FbDesk::Icon::TEXTPLACE_BOTTOM;
    else 
        *(*this) = FbDesk::Icon::TEXTPLACE_BOTTOM;
}

template<>
std::string FbTk::Resource<bool>::
getString() const {
    return std::string(**this == true ? "true" : "false");
}


template<>
void FbTk::Resource<bool>::
setFromString(char const *strval) {
    *this = (bool)!strcasecmp(strval, "true");
}

} //end namespace FbTk
