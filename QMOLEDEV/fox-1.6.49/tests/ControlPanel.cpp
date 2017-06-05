/*******************************************************************************
*                    FOX Desktop Setup - FOX Desktop Enviroment                *
********************************************************************************
*            Copyright (C) 2004-2005 Sander Jansen. All Rights Reserved        *
*                               ---                                            *
* This program is free software; you can redistribute it and/or modify         *
* it under the terms of the GNU General Public License as published by         *
* the Free Software Foundation; either version 2 of the License, or            *
* (at your option) any later version.                                          *
*                                                                              *
* This program is distributed in the hope that it will be useful,              *
* but WITHOUT ANY WARRANTY; without even the implied warranty of               *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                *
* GNU General Public License for more details.                                 *
* You should have received a copy of the GNU General Public License            *
* along with this program; if not, write to the Free Software                  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA    *
*******************************************************************************/
#include <xincs.h>
#include <fx.h>
#include "controlpanelicon.h"

#define DEFAULT_SPACING 6

struct ColorTheme {
  const FXchar* name;
  FXColor       base;
  FXColor       border;
  FXColor       back;
  FXColor       fore;
  FXColor       selback;
  FXColor       selfore;
  FXColor       tipback;
  FXColor       tipfore;
  FXColor       menuback;
  FXColor       menufore;
  };

const ColorTheme ColorThemes[]={
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
//|        Name        |        Base      |       Border     |       Back       |      Fore        |      Selback     |      Selfore     |      Tipback     |     Tipfore      |      Menuback    |      Menufore    |
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
  {"FOX"               ,FXRGB(212,208,200),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB( 10, 36,106),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 10, 36,106),FXRGB(255,255,255)},
  {"Gnome"             ,FXRGB(214,215,214),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255)},
  {"Atlas Green"       ,FXRGB(175,180,159),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(111,122, 99),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(111,122, 99),FXRGB(255,255,255)},
  {"BeOS"              ,FXRGB(217,217,217),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(168,168,168),FXRGB(  0,  0,  0),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(168,168,168),FXRGB(  0,  0,  0)},
  {"Blue Slate"        ,FXRGB(239,239,239),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255)},
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
//|        Name        |        Base      |       Border     |       Back       |      Fore        |      Selback     |      Selfore     |      Tipback     |     Tipfore      |      Menuback    |      Menufore    |
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
  {"CDE"	       ,FXRGB(156,153,156),FXRGB(  0,  0,  0),FXRGB(131,129,131),FXRGB(255,255,255),FXRGB( 49, 97,131),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 49, 97,131),FXRGB(255,255,255)},
  {"Digital CDE"       ,FXRGB( 74,121,131),FXRGB(  0,  0,  0),FXRGB( 55, 77, 78),FXRGB(255,255,255),FXRGB( 82,102,115),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 82,102,115),FXRGB(255,255,255)},
  {"Dark Blue"         ,FXRGB( 66,103,148),FXRGB(  0,  0,  0),FXRGB(  0, 42, 78),FXRGB(255,255,255),FXRGB( 92,179,255),FXRGB(  0,  0,  0),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 92,179,255),FXRGB(  0,  0,  0)},
  {"Desert FOX"        ,FXRGB(214,205,187),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(128,  0,  0),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(128,  0,  0),FXRGB(255,255,255)},
  {"EveX"              ,FXRGB(230,222,220),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB( 10, 95,137),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 10, 95,137),FXRGB(255,255,255)},
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
//|        Name        |        Base      |       Border     |       Back       |      Fore        |      Selback     |      Selfore     |      Tipback     |     Tipfore      |      Menuback    |      Menufore    |
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
  {"Galaxy"            ,FXRGB(239,239,239),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255)},
  {"iMac"              ,FXRGB(205,206,205),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255)},
  {"KDE 1"             ,FXRGB(192,192,192),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255)},
  {"KDE 2"             ,FXRGB(220,220,220),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB( 10, 95,137),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 10, 95,137),FXRGB(255,255,255)},
  {"KDE 3"             ,FXRGB(238,238,230),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(255,221,118),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(255,221,118),FXRGB(255,255,255)},
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
//|        Name        |        Base      |       Border     |       Back       |      Fore        |      Selback     |      Selfore     |      Tipback     |     Tipfore      |      Menuback    |      Menufore    |
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
  {"Keramik"           ,FXRGB(234,233,232),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(169,209,255),FXRGB(  0,  0,  0),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(169,209,255),FXRGB(  0,  0,  0)},
  {"Keramik Emerald"   ,FXRGB(238,238,230),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(134,204,134),FXRGB(  0,  0,  0),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(134,204,134),FXRGB(  0,  0,  0)},
  {"Keramik White"     ,FXRGB(239,239,239),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255)},
  {"Mandrake"          ,FXRGB(230,231,230),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB( 33, 68,156),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 33, 68,156),FXRGB(255,255,255)},
  {"Media Peach"       ,FXRGB(239,239,239),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255)},
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
//|        Name        |        Base      |       Border     |       Back       |      Fore        |      Selback     |      Selfore     |      Tipback     |     Tipfore      |      Menuback    |      Menufore    |
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
  {"Next"              ,FXRGB(168,168,168),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0,  0,  0),FXRGB(255,255,255)},
  {"Pale Gray"         ,FXRGB(214,214,214),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0,  0,  0),FXRGB(255,255,255)},
  {"Plastik"           ,FXRGB(239,239,239),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(103,141,178),FXRGB(255,255,255)},
  {"Pumpkin"           ,FXRGB(238,216,174),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(205,133, 63),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(205,133, 63),FXRGB(255,255,255)},
  {"Redmond 95"        ,FXRGB(195,195,195),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0,  0,128),FXRGB(255,255,255)},
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
//|        Name        |        Base      |       Border     |       Back       |      Fore        |      Selback     |      Selfore     |      Tipback     |     Tipfore      |      Menuback    |      Menufore    |
//|--------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------+------------------|
  {"Redmond 2000"      ,FXRGB(212,208,200),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(  0, 36,104),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(  0, 36,104),FXRGB(255,255,255)},
  {"Redmond XP"        ,FXRGB(238,238,230),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB( 74,121,205),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 74,121,205),FXRGB(255,255,255)},
  {"Solaris"           ,FXRGB(174,178,195),FXRGB(  0,  0,  0),FXRGB(147,151,165),FXRGB(  0,  0,  0),FXRGB(113,139,165),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(113,139,165),FXRGB(255,255,255)},
  {"Storm"             ,FXRGB(192,192,192),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB(139,  0,139),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB(139,  0,139),FXRGB(255,255,255)},
  {"Sea Sky"           ,FXRGB(165,178,198),FXRGB(  0,  0,  0),FXRGB(255,255,255),FXRGB(  0,  0,  0),FXRGB( 49,101,156),FXRGB(255,255,255),FXRGB(255,255,225),FXRGB(  0,  0,  0),FXRGB( 49,101,156),FXRGB(255,255,255)},
  };

const FXint numThemes=ARRAYNUMBER(ColorThemes);


struct FXFileBinding {
  FXString key;
  FXString description;
  FXString command;
  FXString bigname;
  FXString bignameopen;
  FXString name;
  FXString nameopen;
  FXString mime;
  };


class FXDesktopSetup : public FXMainWindow {
  FXDECLARE(FXDesktopSetup)
private:
  FXSettings         desktopsettings;   // Desktop Settings Registry
  FXFont            *titlefont;
  FXIcon            *desktopicon;
  FXIcon            *icon_colors;
  FXIcon            *icon_settings;
  FXIcon            *icon_filebinding;
private:
  FXListBox         *list;
  FXList            *filebindinglist;
  FXComboBox        *mimetypelist;
  FXButton          *button_bigname;
  FXButton          *button_bignameopen;
  FXButton          *button_name;
  FXButton          *button_nameopen;
private:
  FXToolTip         *tooltip;
  FXTabBook         *tabbook;
  FXTabItem         *tabitem;
  FXVerticalFrame   *tabframe;
  FXVerticalFrame   *mainframe;
  FXVerticalFrame   *menuframe;
  FXHorizontalFrame *labeltextframe1;
  FXHorizontalFrame *labeltextframe2;
  FXHorizontalFrame *textframe1;
  FXHorizontalFrame *textframe2;
  FXHorizontalFrame *tabsubframe;
  FXGroupBox        *grpbox1;
  FXGroupBox        *grpbox2;
  FXLabel           *label1;
  FXLabel           *label2;
  FXLabel           *label3;
  FXLabel           *label4;
  FXLabel           *label5;
  FXLabel           *menulabels[6];
  FXTextField       *textfield1;
  FXButton          *button1;
  FXButton          *fontbutton;
  FXSeparator       *sep1;
  FXSeparator       *sep2;
  FXSeparator       *sep3;
private:
  ColorTheme        theme_current;  // Current Settings
  ColorTheme        theme_user;     // Theme User may have set, which is different from the other themes
  FXColor           hilite;
  FXColor           shadow;
private:
  FXFont           *font;
  FXbool            hascurrent;
  FXString          applicationname;
  FXString          vendorname;
  FXString          iconpath;
  FXuint            typingSpeed;
  FXuint            clickSpeed;
  FXuint            scrollSpeed;
  FXuint            scrollDelay;
  FXuint            blinkSpeed;
  FXuint            animSpeed;
  FXuint            menuPause;
  FXuint            tooltipPause;
  FXuint            tooltipTime;
  FXuint            maxcolors;
  FXint             dragDelta;
  FXint             wheelLines;
  FXfloat           gamma;
  FXFileBinding     filebinding;
private:
  FXDataTarget      target_base;
  FXDataTarget      target_back;
  FXDataTarget      target_border;
  FXDataTarget      target_fore;
  FXDataTarget      target_hilite;
  FXDataTarget      target_shadow;
  FXDataTarget      target_selfore;
  FXDataTarget      target_selback;
  FXDataTarget      target_tipfore;
  FXDataTarget      target_tipback;
  FXDataTarget      target_menufore;
  FXDataTarget      target_menuback;
  FXDataTarget      target_typingspeed;
  FXDataTarget      target_clickspeed;
  FXDataTarget      target_scrollspeed;
  FXDataTarget      target_scrolldelay;
  FXDataTarget      target_blinkspeed;
  FXDataTarget      target_animspeed;
  FXDataTarget      target_menupause;
  FXDataTarget      target_tooltippause;
  FXDataTarget      target_tooltiptime;
  FXDataTarget      target_dragdelta;
  FXDataTarget      target_wheellines;
  FXDataTarget      target_maxcolors;
  FXDataTarget      target_gamma;
  FXDataTarget      target_filebinding_description;
  FXDataTarget      target_filebinding_command;
  FXDataTarget      target_iconpath;
private:
  void setup();
  void setupFont();
  void setupColors();
  FXbool writeDesktop();
  void initColors();
  void saveFileBinding();
  FXString getOutputFile();
private:
  FXDesktopSetup(){}
  FXDesktopSetup(const FXDesktopSetup&);
  FXDesktopSetup& operator=(const FXDesktopSetup&);
public:
  enum {
    ID_COLORS =FXMainWindow::ID_LAST,
    ID_COLOR_THEME,
    ID_CHOOSE_FONT,
    ID_SELECT_COMMAND,
    ID_CREATE_FILEBINDING,
    ID_REMOVE_FILEBINDING,
    ID_RENAME_FILEBINDING,
    ID_SELECT_FILEBINDING,
    ID_SELECT_ICON_NAME,
    ID_SELECT_ICON_BIGNAME,
    ID_SELECT_ICON_NAMEOPEN,
    ID_SELECT_ICON_BIGNAMEOPEN,
    ID_SELECT_MIMETYPE,
    ID_DESKTOPWINDOW,
    };
public:
  long onCmdClose(FXObject*,FXSelector,void*);
  long onColorChanged(FXObject*,FXSelector,void*);
  long onColorTheme(FXObject*,FXSelector,void*);
  long onChooseFont(FXObject*,FXSelector,void*);
  long onCmdFileBinding(FXObject*,FXSelector,void*);
  long onCmdMimeType(FXObject*,FXSelector,void*);
  long onCmdCreateFileBinding(FXObject*,FXSelector,void*);
  long onCmdRemoveFileBinding(FXObject*,FXSelector,void*);
  long onCmdRenameFileBinding(FXObject*,FXSelector,void*);
  long onCmdSelectCommand(FXObject*,FXSelector,void*);
  long onCmdSelectIcon(FXObject*,FXSelector,void*);
public:

  // Constructor
  FXDesktopSetup(FXApp *app);

  virtual void create();

  // Destructor
  virtual ~FXDesktopSetup();
  };


FXDEFMAP(FXDesktopSetup) FXDesktopSetupMap[]={
  FXMAPFUNC(SEL_CLOSE,FXDesktopSetup::ID_DESKTOPWINDOW,FXDesktopSetup::onCmdClose),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_COLORS,FXDesktopSetup::onColorChanged),
  FXMAPFUNC(SEL_CHANGED,FXDesktopSetup::ID_COLORS,FXDesktopSetup::onColorChanged),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_COLOR_THEME,FXDesktopSetup::onColorTheme),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_CHOOSE_FONT,FXDesktopSetup::onChooseFont),
  FXMAPFUNC(SEL_CHANGED,FXDesktopSetup::ID_SELECT_FILEBINDING,FXDesktopSetup::onCmdFileBinding),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_SELECT_COMMAND,FXDesktopSetup::onCmdSelectCommand),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_SELECT_MIMETYPE,FXDesktopSetup::onCmdMimeType),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_CREATE_FILEBINDING,FXDesktopSetup::onCmdCreateFileBinding),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_REMOVE_FILEBINDING,FXDesktopSetup::onCmdRemoveFileBinding),
  FXMAPFUNC(SEL_COMMAND,FXDesktopSetup::ID_RENAME_FILEBINDING,FXDesktopSetup::onCmdRenameFileBinding),
  FXMAPFUNCS(SEL_COMMAND,FXDesktopSetup::ID_SELECT_ICON_NAME,FXDesktopSetup::ID_SELECT_ICON_BIGNAMEOPEN,FXDesktopSetup::onCmdSelectIcon),

  };


// Object implementation
FXIMPLEMENT(FXDesktopSetup,FXMainWindow,FXDesktopSetupMap,ARRAYNUMBER(FXDesktopSetupMap))


FXDesktopSetup::FXDesktopSetup(FXApp * app) : FXMainWindow(app,"FOX Desktop Setup",NULL,NULL,DECOR_ALL,0,0,0,0) {

  setTarget(this);
  setSelector(ID_DESKTOPWINDOW);


  const char *const *argv=getApp()->getArgv();
  if(getApp()->getArgc()>1){
    applicationname = argv[1];
    if (getApp()->getArgc()>2){
      vendorname=argv[2];
      }
    }

  FXString desktopdir=FXSystem::getHomeDirectory() + PATHSEPSTRING +".foxrc";
  FXString desktopfile;

  if(FXStat::exists(desktopdir)){
    if (applicationname.empty()){
      desktopfile = desktopdir + PATHSEPSTRING + "Desktop";
      }
    else if (vendorname.empty()){
      desktopfile = desktopdir + PATHSEPSTRING + applicationname;
      }
    else {
      desktopfile = desktopdir + PATHSEPSTRING + vendorname + PATHSEPSTRING + applicationname;
      }
    if (FXStat::exists(desktopfile)){
      desktopsettings.parseFile(desktopfile,TRUE);
      }
    }

  tooltip = new FXToolTip(getApp());

  hilite	= getApp()->getHiliteColor();
  shadow	= getApp()->getShadowColor();

  /// Retrieve Current Color Settings
  theme_current.base		= getApp()->getBaseColor();
  theme_current.back		= getApp()->getBackColor();
  theme_current.border	= getApp()->getBorderColor();
  theme_current.fore		= getApp()->getForeColor();
  theme_current.selfore	= getApp()->getSelforeColor();
  theme_current.selback	= getApp()->getSelbackColor();
  theme_current.tipfore	= getApp()->getTipforeColor();
  theme_current.tipback	= getApp()->getTipbackColor();
  theme_current.menufore= getApp()->getSelMenuTextColor();
  theme_current.menuback= getApp()->getSelMenuBackColor();

  typingSpeed =getApp()->getTypingSpeed();
  clickSpeed  =getApp()->getClickSpeed();
  scrollSpeed =getApp()->getScrollSpeed();
  scrollDelay =getApp()->getScrollDelay();
  blinkSpeed  =getApp()->getBlinkSpeed();
  animSpeed   =getApp()->getAnimSpeed();
  menuPause   =getApp()->getMenuPause();
  tooltipPause=getApp()->getTooltipPause();
  tooltipTime =getApp()->getTooltipTime();
  dragDelta   =getApp()->getDragDelta();
  wheelLines  =getApp()->getWheelLines();

  gamma=getApp()->reg().readRealEntry("SETTINGS","displaygamma",1.0);
  maxcolors=getApp()->reg().readUnsignedEntry("SETTINGS","maxcolors",125);
  iconpath = getApp()->reg().readStringEntry("SETTINGS","iconpath");


  /// Setup the Datatargets
  target_base.connect(theme_current.base);
  target_back.connect(theme_current.back);
  target_border.connect(theme_current.border);
  target_fore.connect(theme_current.fore);
  target_hilite.connect(hilite);
  target_shadow.connect(shadow);
  target_selfore.connect(theme_current.selfore);
  target_selback.connect(theme_current.selback);
  target_tipfore.connect(theme_current.tipfore);
  target_tipback.connect(theme_current.tipback);
  target_menufore.connect(theme_current.menufore);
  target_menuback.connect(theme_current.menuback);


  target_typingspeed.connect(typingSpeed);
  target_clickspeed.connect(clickSpeed);
  target_scrollspeed.connect(scrollSpeed);
  target_scrolldelay.connect(scrollDelay);
  target_blinkspeed.connect(blinkSpeed);
  target_animspeed.connect(animSpeed);
  target_menupause.connect(menuPause);
  target_tooltippause.connect(tooltipPause);
  target_tooltiptime.connect(tooltipTime);
  target_dragdelta.connect(dragDelta);
  target_wheellines.connect(wheelLines);
  target_maxcolors.connect(maxcolors);
  target_gamma.connect(gamma);

  target_filebinding_description.connect(filebinding.description);
  target_filebinding_command.connect(filebinding.command);
  target_iconpath.connect(iconpath);

  target_base.setTarget(this);
  target_back.setTarget(this);
  target_border.setTarget(this);
  target_fore.setTarget(this);
  target_hilite.setTarget(this);
  target_shadow.setTarget(this);
  target_selfore.setTarget(this);
  target_selback.setTarget(this);
  target_tipfore.setTarget(this);
  target_tipback.setTarget(this);
  target_menufore.setTarget(this);
  target_menuback.setTarget(this);

  target_base.setSelector(ID_COLORS);
  target_back.setSelector(ID_COLORS);
  target_border.setSelector(ID_COLORS);
  target_fore.setSelector(ID_COLORS);
  target_hilite.setSelector(ID_COLORS);
  target_shadow.setSelector(ID_COLORS);
  target_selfore.setSelector(ID_COLORS);
  target_selback.setSelector(ID_COLORS);
  target_tipfore.setSelector(ID_COLORS);
  target_tipback.setSelector(ID_COLORS);
  target_menufore.setSelector(ID_COLORS);
  target_menuback.setSelector(ID_COLORS);

  desktopicon=new FXGIFIcon(getApp(),controlpanel_gif);
  icon_colors=new FXGIFIcon(getApp(),colors_gif);
  icon_settings=new FXGIFIcon(getApp(),settings_gif);
  icon_filebinding=new FXGIFIcon(getApp(),filebinding_gif);

  setup();
  }


FXDesktopSetup::~FXDesktopSetup(){
  delete titlefont;
  delete font;
  delete desktopicon;
  delete icon_colors;
  delete icon_settings;
  delete icon_filebinding;
  }

void FXDesktopSetup::create(){
  FXMainWindow::create();
  initColors();
  }


void FXDesktopSetup::setup(){

  getApp()->getNormalFont()->create();
  FXFontDesc fontdescription;
  getApp()->getNormalFont()->getFontDesc(fontdescription);

  font = new FXFont(getApp(),fontdescription);
  font->create();

  fontdescription.size = (FXuint) (((double)fontdescription.size) * 1.5);
  titlefont = new FXFont(getApp(),fontdescription);
  titlefont->create();

  FXHorizontalFrame * hframe=NULL;
  FXVerticalFrame   * frame=NULL;
  FXVerticalFrame   * vframe=NULL;
  FXMatrix          * matrix=NULL;
  FXLabel           * label=NULL;
  FXSpinner         * spinner=NULL;

  FXVerticalFrame * main = new FXVerticalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);


  // Assuming we have a icon of size 48x48, using different spacing will give us about the same size header.
  const FXint spacing=(desktopicon ? 5 : 15);

  // Create nice header
  label = new FXLabel(main,"FOX Desktop Setup",desktopicon,LAYOUT_FILL_X|JUSTIFY_LEFT|TEXT_AFTER_ICON,0,0,0,0,spacing,spacing,spacing,spacing);
  label->setBackColor(FXRGB(255,255,255));
  label->setTextColor(FXRGB(  0,  0,  0));
  label->setFont(titlefont);

  new FXSeparator(main,SEPARATOR_GROOVE|LAYOUT_FILL_X);


  FXHorizontalFrame * hmainframe = new FXHorizontalFrame(main,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);

  FXVerticalFrame * buttonframe = new FXVerticalFrame(hmainframe,LAYOUT_FILL_Y|LAYOUT_LEFT|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0, DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);

  new FXSeparator(hmainframe,SEPARATOR_GROOVE|LAYOUT_FILL_Y);


  FXSwitcher * switcher = new FXSwitcher(hmainframe,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0);

  vframe = new FXVerticalFrame(buttonframe,FRAME_SUNKEN,0,0,0,0,0,0,0,0);
  new FXButton(vframe,"Themes",icon_colors,switcher,FXSwitcher::ID_OPEN_FIRST,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL);
  vframe = new FXVerticalFrame(buttonframe,FRAME_SUNKEN,0,0,0,0,0,0,0,0);
  new FXButton(vframe,"Bindings",icon_filebinding,switcher,FXSwitcher::ID_OPEN_SECOND,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL);
  vframe = new FXVerticalFrame(buttonframe,FRAME_SUNKEN,0,0,0,0,0,0,0,0);
  new FXButton(vframe,"General",icon_settings,switcher,FXSwitcher::ID_OPEN_THIRD,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL);




  vframe = new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);

  hframe = new FXHorizontalFrame(vframe,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);
  new FXSeparator(vframe,SEPARATOR_GROOVE|LAYOUT_FILL_X);


  frame  = new FXVerticalFrame(hframe,LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);
  new FXSeparator(hframe,SEPARATOR_GROOVE|LAYOUT_FILL_Y);

  FXVerticalFrame * themeframe = new FXVerticalFrame(frame,LAYOUT_FILL_X,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(themeframe,"Theme: ",NULL,LAYOUT_CENTER_Y);
  list = new FXListBox(themeframe,this,ID_COLOR_THEME,LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK);
  list->setNumVisible(9);

  new FXSeparator(frame,SEPARATOR_GROOVE|LAYOUT_FILL_X);

  matrix = new FXMatrix(frame,2,LAYOUT_FILL_Y|MATRIX_BY_COLUMNS,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,1,1);

  new FXColorWell(matrix,FXRGB(0,0,255),&target_base,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Base Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_border,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Border Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_fore,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Text Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_back,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Background Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_selfore,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Selected Text Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_selback,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Selected Background Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_menufore,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Selected Menu Text Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_menuback,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Selected Menu Background Color");

  new FXColorWell(matrix,FXRGB(0,0,255),&target_tipfore,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Tip Text Color");

   new FXColorWell(matrix,FXRGB(0,0,255),&target_tipback,FXDataTarget::ID_VALUE);
  label	  = new FXLabel(matrix,"Tip Background Color");

  frame = new FXVerticalFrame(hframe,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,0,0);

  tabbook = new FXTabBook(frame,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0);
  tabitem  = new FXTabItem(tabbook," Item 1 ");
  tabframe = new FXVerticalFrame(tabbook,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_THICK|FRAME_RAISED);

  labeltextframe1 = new FXHorizontalFrame(tabframe,LAYOUT_FILL_X);
  label1 = new FXLabel(labeltextframe1,"Label with Text",NULL);
  textfield1 = new FXTextField(labeltextframe1,30,NULL,0,LAYOUT_FILL_X|FRAME_THICK|FRAME_SUNKEN);
  textfield1->setText("Select this text, to see the selected colors");

  labeltextframe2 = new FXHorizontalFrame(tabframe,LAYOUT_FILL_X);
  textframe1 = new FXHorizontalFrame(labeltextframe2,LAYOUT_FILL_X|FRAME_THICK|FRAME_SUNKEN,0,0,0,0,2,2,2,2,0,0);
  label3 = new FXLabel(textframe1,"Selected Text (with focus)",NULL,LAYOUT_FILL_X,0,0,0,0,1,1,1,1);
  textframe2 = new FXHorizontalFrame(labeltextframe2,LAYOUT_FILL_X|FRAME_THICK|FRAME_SUNKEN,0,0,0,0,2,2,2,2,0,0);
  label4 = new FXLabel(textframe2,"Selected Text (no focus)",NULL,LAYOUT_FILL_X,0,0,0,0,1,1,1,1);

  sep1 = new FXSeparator(tabframe,LAYOUT_FILL_X|SEPARATOR_LINE);

  tabsubframe = new FXHorizontalFrame(tabframe,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  grpbox1 = new FXGroupBox(tabsubframe,"MenuPane",FRAME_GROOVE|LAYOUT_FILL_Y|LAYOUT_FILL_X);

  menuframe = new FXVerticalFrame(grpbox1,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y,0,0,0,0,0,0,0,0,0,0);
  menulabels[0]=new FXLabel(menuframe,"&Open",NULL,LABEL_NORMAL,0,0,0,0,16,4);
  menulabels[1]=new FXLabel(menuframe,"S&ave",NULL,LABEL_NORMAL,0,0,0,0,16,4);
  sep2 = new FXSeparator(menuframe,LAYOUT_FILL_X|SEPARATOR_GROOVE);
  menulabels[2]=new FXLabel(menuframe,"I&mport",NULL,LABEL_NORMAL,0,0,0,0,16,4);
  menulabels[4]=new FXLabel(menuframe,"Selected Menu Entry",NULL,LABEL_NORMAL,0,0,0,0,16,4);
  menulabels[3]=new FXLabel(menuframe,"Print",NULL,LABEL_NORMAL,0,0,0,0,16,4);
  sep3 = new FXSeparator(menuframe,LAYOUT_FILL_X|SEPARATOR_GROOVE);
  menulabels[5]=new FXLabel(menuframe,"&Quit",NULL,LABEL_NORMAL,0,0,0,0,16,4);

  grpbox2 = new FXGroupBox(tabsubframe,"Tooltips",FRAME_GROOVE|LAYOUT_FILL_Y|LAYOUT_FILL_X);

  label2 = new FXLabel(grpbox2,"Sample Tooltip",NULL,FRAME_LINE|LAYOUT_CENTER_X);
  label5 = new FXLabel(grpbox2,"Multiline Sample\n Tooltip",NULL,FRAME_LINE|LAYOUT_CENTER_X);

  hframe = new FXHorizontalFrame(vframe,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);

  new FXLabel(hframe,"Normal Font: ",NULL,LAYOUT_CENTER_Y);
  fontbutton = new FXButton(hframe," ",NULL,this,ID_CHOOSE_FONT,LAYOUT_CENTER_Y|FRAME_RAISED|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_X);


  vframe = new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);

  hframe = new FXHorizontalFrame(vframe,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(hframe,"Icon Search Path",NULL,LAYOUT_CENTER_Y);
  new FXTextField(hframe,2,&target_iconpath,FXDataTarget::ID_VALUE,LAYOUT_SIDE_LEFT|LAYOUT_FILL_X|LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK);

  new FXSeparator(vframe,SEPARATOR_GROOVE|LAYOUT_FILL_X);


  hframe = new FXHorizontalFrame(vframe,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);
  vframe = new FXVerticalFrame(hframe,LAYOUT_FILL_Y,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXSeparator(hframe,SEPARATOR_GROOVE|LAYOUT_FILL_Y);


  new FXLabel(vframe,"File Binding: ");
  frame = new FXVerticalFrame(vframe,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0,0,0,0,0);

  filebindinglist = new FXList(frame,this,ID_SELECT_FILEBINDING,LAYOUT_FILL_Y|LAYOUT_FILL_X|LIST_BROWSESELECT);
  filebindinglist->setSortFunc(FXList::ascending);
  FXHorizontalFrame * listbuttonframe = new FXHorizontalFrame(vframe,PACK_UNIFORM_WIDTH|LAYOUT_FILL_X,0,0,0,0,0,0,0,0);
  new FXButton(listbuttonframe,"&New\tAdd New Binding",NULL,this,ID_CREATE_FILEBINDING);
  new FXButton(listbuttonframe,"&Delete\tDelete Selected Binding",NULL,this,ID_REMOVE_FILEBINDING);
  new FXButton(listbuttonframe,"&Edit\tEdit Binding Name",NULL,this,ID_RENAME_FILEBINDING);





  vframe = new FXVerticalFrame(hframe,LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0,0,0,0,0,0,0);

  FXPacker * packer = new FXPacker(vframe,LAYOUT_FILL_X,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(packer,"Description:");
  new FXTextField(packer,2,&target_filebinding_description,FXDataTarget::ID_VALUE,LAYOUT_SIDE_LEFT|LAYOUT_FILL_X|LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK);
  new FXSeparator(vframe,SEPARATOR_GROOVE|LAYOUT_FILL_X);

  packer = new FXPacker(vframe,LAYOUT_FILL_X,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(packer,"Command:");
  FXCheckButton * checkbutton1 = new FXCheckButton(packer,"Run in terminal",NULL,0,ICON_BEFORE_TEXT|LAYOUT_LEFT|LAYOUT_SIDE_BOTTOM);
  FXCheckButton * checkbutton2 = new FXCheckButton(packer,"Change directory",NULL,0,ICON_BEFORE_TEXT|LAYOUT_LEFT|LAYOUT_SIDE_BOTTOM);
  checkbutton1->disable();
  checkbutton2->disable();

  new FXButton(packer,"...",NULL,this,ID_SELECT_COMMAND,LAYOUT_SIDE_RIGHT|LAYOUT_CENTER_Y|FRAME_RAISED|FRAME_THICK);
  new FXTextField(packer,2,&target_filebinding_command,FXDataTarget::ID_VALUE,LAYOUT_SIDE_LEFT|LAYOUT_FILL_X|LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK);

  new FXSeparator(vframe,SEPARATOR_GROOVE|LAYOUT_FILL_X);

  packer = new FXPacker(vframe,LAYOUT_FILL_X,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(packer,"Mime Type:");

  mimetypelist = new FXComboBox(packer,1,this,ID_SELECT_MIMETYPE,LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK|COMBOBOX_NO_REPLACE);
  mimetypelist->setSortFunc(FXList::ascending);
  mimetypelist->setNumVisible(9);
  mimetypelist->appendItem(" ");

  new FXSeparator(vframe,SEPARATOR_GROOVE|LAYOUT_FILL_X);

  FXMatrix *iconsmatrix=new FXMatrix(vframe,4,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(iconsmatrix,"Small",NULL,LAYOUT_CENTER_X|LAYOUT_FILL_COLUMN);
  new FXLabel(iconsmatrix,"Big",NULL,LAYOUT_CENTER_X|LAYOUT_FILL_COLUMN);
  new FXLabel(iconsmatrix,"Small Open",NULL,LAYOUT_CENTER_X|LAYOUT_FILL_COLUMN);
  new FXLabel(iconsmatrix,"Big Open",NULL,LAYOUT_CENTER_X|LAYOUT_FILL_COLUMN);

  button_name = new FXButton(iconsmatrix,"\tChange icon",NULL,this,ID_SELECT_ICON_NAME,FRAME_RAISED|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,68,68, 1,1,1,1);
  button_bigname = new FXButton(iconsmatrix,"\tChange icon",NULL,this,ID_SELECT_ICON_BIGNAME,FRAME_RAISED|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,68,68, 1,1,1,1);
  button_nameopen = new FXButton(iconsmatrix,"\tChange icon",NULL,this,ID_SELECT_ICON_NAMEOPEN,FRAME_RAISED|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,68,68, 1,1,1,1);
  button_bignameopen = new FXButton(iconsmatrix,"\tChange icon",NULL,this,ID_SELECT_ICON_BIGNAMEOPEN,FRAME_RAISED|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,68,68, 1,1,1,1);

  FXString data;
  FXString mime;
  FXStringDict *prefs;
  prefs = desktopsettings.find("FILETYPES");
  if(prefs){
    for(FXint e=prefs->first(); e<prefs->size(); e=prefs->next(e)){
      filebindinglist->appendItem(prefs->key(e),NULL,NULL,TRUE);
      data = prefs->data(e);
      mime = data.section(";",4);
      if (!mime.empty() && (mimetypelist->findItem(mime)==-1)){
        mimetypelist->appendItem(mime);
        }
      }
    }

  filebindinglist->sortItems();
  mimetypelist->sortItems();

  FXString labelname="Desktop Settings";

  if(!applicationname.empty()){
    labelname = applicationname;
    if(!vendorname.empty()){
      labelname += " [ " + vendorname + " ]";
      }
    labelname+= " Settings";
    }




  hframe = new FXHorizontalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);

  matrix = new FXMatrix(hframe,3,LAYOUT_FILL_Y|MATRIX_BY_COLUMNS,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);

  const FXuint spinnerstyle=FRAME_SUNKEN|FRAME_THICK;

  new FXLabel(matrix,"Typing Speed\t\tTyping Speed",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_typingspeed,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(500);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);


  new FXLabel(matrix,"Double Click Speed\t\tDouble Click Speed",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_clickspeed,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(100);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Scroll Speed\t\tScroll Speed",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_scrollspeed,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(10);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Scroll Delay\t\tScroll Delay",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_scrolldelay,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(100);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Cursor Blink Speed\t\tCursor Blink Speed",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_blinkspeed,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(100);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Animation Speed\t\tAnimation Speed",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  new FXSpinner(matrix,4,&target_animspeed,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(1);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Cascade Menu Popup Delay\t\tAmount of delay before cascading menu is shown",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_menupause,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(100);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Tooltip Popup Delay\t\tAmount of Delay before tooltip is shown ",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_tooltippause,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(100);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Tooltip Time\t\tTime that tooltips are shown",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_tooltiptime,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(500);
  new FXLabel(matrix,"ms",NULL,LAYOUT_CENTER_Y);

  new FXLabel(matrix,"Drag Delta\t\tMinimum distance considered a mouse move",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_dragdelta,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,9999);
  spinner->setIncrement(1);
  new FXFrame(matrix,FRAME_NONE);

  new FXLabel(matrix,"Wheel Lines",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  spinner=new FXSpinner(matrix,4,&target_wheellines,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,100);

  new FXSeparator(hframe,SEPARATOR_GROOVE|LAYOUT_FILL_Y);

  matrix = new FXMatrix(hframe,2,LAYOUT_FILL_Y|MATRIX_BY_COLUMNS,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  new FXLabel(matrix,"Maximum Colors Allocated");
  spinner = new FXSpinner(matrix,3,&target_maxcolors,FXDataTarget::ID_VALUE,spinnerstyle);
  spinner->setRange(1,256);

  new FXLabel(matrix,"Gamma Correction");
  FXRealSpinner * rspinner = new FXRealSpinner(matrix,3,&target_gamma,FXDataTarget::ID_VALUE,spinnerstyle);
  rspinner->setRange(0.0,5.0);
  rspinner->setIncrement(0.1);


  new FXSeparator(main,SEPARATOR_GROOVE|LAYOUT_FILL_X);
  FXHorizontalFrame *closebox=new FXHorizontalFrame(main,LAYOUT_BOTTOM|LAYOUT_FILL_X,0,0,0,0,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING);
  label = new FXLabel(closebox,labelname,NULL,LAYOUT_FILL_X|LAYOUT_CENTER_Y|JUSTIFY_LEFT,0,0,0,0,15);
  label->disable();

  new FXButton(closebox,"&Close",NULL,this,FXTopWindow::ID_CLOSE,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0,20,20);

  setupColors();
  setupFont();
  }



static FXIcon * createIconFromName(FXApp * app,const FXString & name, const FXString & iconpath){
  FXIcon * icon = NULL;
  FXIconSource iconsource(app);
  FXString fullpath;
  if(FXStat::exists(name)){
    fullpath = name;
    }
  else{
    fullpath = FXPath::search(iconpath,name);
    if(fullpath.empty()) return NULL;
    }
  icon = iconsource.loadIconFile(fullpath);
  if (icon) {
    icon->blend(app->getBaseColor());
    icon->create();
    return icon;
    }
  return NULL;
  }


static void updateIcon(FXApp*app,FXButton * button, const FXString & path){
  FXIconSource iconsource(app);
  FXIcon * icon = NULL;

  if (button->getIcon()){
    icon = button->getIcon();
    button->setIcon(NULL);
    delete icon;
    icon=NULL;
    }

  icon = iconsource.loadIconFile(path);
  if (icon) {
    icon->blend(app->getBaseColor());
    icon->create();
    button->setIcon(icon);
    }
  }




/*
static FXint findItemInList(FXList * list,FXListItem * item){
        for(FXint i=0;i<list->getNumItems();i++){
                if (list->getItem(i)==item) return i;
                }
        return -1;
        }
*/


void FXDesktopSetup::saveFileBinding(){
  ///fxmessage("Saving File Binding: %s\n",filebinding.key.text());
  if (filebinding.key.empty()) return;

  FXString entry;

  entry = filebinding.command + ";" + filebinding.description + ";";
  if (filebinding.bignameopen.empty())
  entry	+= filebinding.bigname + ";";
  else
    entry += filebinding.bigname + ":" + filebinding.bignameopen + ";";

  if (filebinding.nameopen.empty())
  entry	+= filebinding.name + ";";
  else
          entry += filebinding.name + ":" + filebinding.nameopen + ";";

  entry += filebinding.mime;

  desktopsettings.writeStringEntry("FILETYPES",filebinding.key.text(),entry.text());
  }


long FXDesktopSetup::onCmdSelectCommand(FXObject*,FXSelector,void*){
  FXString path = (filebinding.command.empty()) ? "/usr/bin/" : filebinding.command.text();

  FXString command = FXFileDialog::getOpenFilename(this,"Select Command",path,"*");
  if (!command.empty()){
    filebinding.command = command;
    }
  return 1;
  }


long FXDesktopSetup::onCmdMimeType(FXObject*,FXSelector,void*ptr){
  FXString mime = (FXchar*)ptr;
  if (!mime.empty() && (mimetypelist->findItem(mime)==-1)){
    mimetypelist->appendItem(mime);
    mimetypelist->sortItems();
    }
  filebinding.mime = mime;
  return 1;
  }

long FXDesktopSetup::onCmdFileBinding(FXObject*,FXSelector,void*){
  saveFileBinding();

  FXIcon * icon;

  FXString iconname;
  FXString association;
  filebinding.key = filebindinglist->getItemText(filebindinglist->getCurrentItem());
  if (filebinding.key.empty()) return 1;

  association = desktopsettings.readStringEntry("FILETYPES",filebinding.key.text());

  filebinding.command = association.section(";",0);
  filebinding.description = association.section(";",1);

  iconname = association.section(";",2);
  filebinding.bigname = iconname.section(":",0);
  filebinding.bignameopen = iconname.section(":",1);

  iconname = association.section(";",3);
  filebinding.name = iconname.section(":",0);
  filebinding.nameopen = iconname.section(":",1);

  filebinding.mime = association.section(";",4);
  if (!filebinding.mime.empty()){
    FXint no = mimetypelist->findItem(filebinding.mime);
    mimetypelist->setCurrentItem(no);
    }
  else {
    FXint no = mimetypelist->findItem(" ");
    mimetypelist->setCurrentItem(no);
    }

  if (button_bigname->getIcon()){
    icon = button_bigname->getIcon();
    button_bigname->setIcon(NULL);
    delete icon;
    }
  icon = createIconFromName(getApp(),filebinding.bigname,iconpath);
  button_bigname->setIcon(icon);

  if (button_bignameopen->getIcon()){
    icon = button_bignameopen->getIcon();
    button_bignameopen->setIcon(NULL);
    delete icon;
    }
  icon = createIconFromName(getApp(),filebinding.bignameopen,iconpath);
  button_bignameopen->setIcon(icon);

  if (button_name->getIcon()){
    icon = button_name->getIcon();
    button_name->setIcon(NULL);
    delete icon;
    }
  icon = createIconFromName(getApp(),filebinding.name,iconpath);
  button_name->setIcon(icon);

  if (button_nameopen->getIcon()){
    icon = button_nameopen->getIcon();
    button_nameopen->setIcon(NULL);
    delete icon;
    }
  icon = createIconFromName(getApp(),filebinding.nameopen,iconpath);
  button_nameopen->setIcon(icon);

  return 1;
  }


long FXDesktopSetup::onCmdCreateFileBinding(FXObject*,FXSelector,void*){
  FXString result;
  if (FXInputDialog::getString(result,this,"New File Binding","Please enter filebinding key:",NULL)){
    if (result.empty()) return 1;
    if ((filebindinglist->findItem(result))>=0){
      FXMessageBox::question(this,MBOX_OK,"Duplicate Binding","The given key %s already exists.",result.text());
      return 1;
      }

    /// Save Current
    saveFileBinding();

    filebinding.key = result;
    filebinding.command = "";
    filebinding.description = "";
    filebinding.bigname = "";
    filebinding.bignameopen = "";
    filebinding.name = "";
    filebinding.nameopen = "";
    filebinding.mime = "";

    /// Save New
    saveFileBinding();

    FXint no = filebindinglist->appendItem(filebinding.key);
    filebindinglist->setCurrentItem(no,TRUE);
    filebindinglist->sortItems();
    filebindinglist->makeItemVisible(filebindinglist->getCurrentItem());
    }
  return 1;
  }

long FXDesktopSetup::onCmdRemoveFileBinding(FXObject*,FXSelector,void*){
  if (filebinding.key.empty()) return 1;
  if (FXMessageBox::question(this,MBOX_OK_CANCEL,"Delete Filebinding?","Are you sure you want to delete\nthe filebinding for %s",filebinding.key.text())==MBOX_CLICKED_OK){
    desktopsettings.deleteEntry("FILETYPES",filebinding.key.text());
    filebinding.key="";
    filebindinglist->removeItem(filebindinglist->getCurrentItem(),TRUE);
    }
  return 1;
  }


long FXDesktopSetup::onCmdRenameFileBinding(FXObject*,FXSelector,void*){
  FXint no;
  FXString result = filebinding.key;
  if (FXInputDialog::getString(result,this,"Rename File Binding","Rename File Binding",NULL)){
    if (result==filebinding.key) return 1;
    if ((no=filebindinglist->findItem(result))>=0){
      if (FXMessageBox::question(this,MBOX_YES_NO,"Replace Binding?","Would you like to replace the existing binding?")!=MBOX_CLICKED_YES){
        return 1;
        }
      }

    /// Remove Old Key
    desktopsettings.deleteEntry("FILETYPES",filebinding.key.text());

    filebinding.key = result;
    saveFileBinding();
    if (no>=0){
      FXint previtem = filebindinglist->getCurrentItem();
      filebindinglist->setCurrentItem(no,TRUE);
      filebindinglist->removeItem(previtem);
      }
    else {
      filebindinglist->setItemText(filebindinglist->getCurrentItem(),filebinding.key);
      }
    }
  return 1;
  }


long FXDesktopSetup::onCmdSelectIcon(FXObject*,FXSelector sel,void*){
  FXString name,selected;

  switch(FXSELID(sel)){
    case ID_SELECT_ICON_NAME: name=filebinding.name; break;
    case ID_SELECT_ICON_BIGNAME: name=filebinding.bigname; break;
    case ID_SELECT_ICON_NAMEOPEN: name=filebinding.nameopen; break;
    case ID_SELECT_ICON_BIGNAMEOPEN: name=filebinding.bignameopen; break;
    }

  if(!FXStat::exists(name)){
    name=FXPath::search(iconpath,name);
    }

  if(name.empty()){
    if(!iconpath.empty())
      name=iconpath.section(PATHLISTSEP,0)+PATHSEPSTRING;
    else
      name=FXSystem::getCurrentDirectory();
    }

//  FXString selected=FXFileDialog::getOpenFilename(this,"Select Icon",name);
  FXFileDialog opendialog(this,"Select Icon");
  opendialog.setSelectMode(SELECTFILE_EXISTING);
  opendialog.setFilename(name);
  opendialog.showImages(TRUE);
  if(opendialog.execute()){
    selected=opendialog.getFilename();
//  if(!selected.empty()){
    FXString path;
    FXString relpath;
    FXString iconname=selected;
    FXint length=selected.length();
    FXint num_sections=iconpath.contains(PATHLISTSEP)+1;
    for(FXint i=0; i<num_sections; i++){
      path=iconpath.section(PATHLISTSEP,i);
      relpath=FXPath::relative(path,selected);
      if(relpath.length()<length){
        iconname=relpath;
        length=relpath.length();
        }
      }
    //fxmessage("Final Icon Name: %s\n",iconname.text());
    switch(FXSELID(sel)){
      case ID_SELECT_ICON_NAME:
        updateIcon(getApp(),button_name,selected);
        filebinding.name=iconname;
        break;
      case ID_SELECT_ICON_BIGNAME:
        updateIcon(getApp(),button_bigname,selected);
        filebinding.bigname=iconname;
        break;
      case ID_SELECT_ICON_NAMEOPEN:
        updateIcon(getApp(),button_nameopen,selected);
        filebinding.nameopen=iconname;
        break;
      case ID_SELECT_ICON_BIGNAMEOPEN:
        updateIcon(getApp(),button_bignameopen,selected);
        filebinding.bignameopen=iconname;
        break;
      }
    }
  return 1;
  }


long FXDesktopSetup::onColorChanged(FXObject*,FXSelector,void*){
  list->setCurrentItem(list->getNumItems()-1);
  setupColors();
  return 1;
  }


long FXDesktopSetup::onColorTheme(FXObject*,FXSelector,void* ptr){
  FXint no=(FXint)(FXival)ptr;
  ColorTheme * theme_selected = reinterpret_cast<ColorTheme*>(list->getItemData(no));
  if (theme_selected) {
    /// Set new colors from selected theme
    theme_current.base     = theme_selected->base;
    theme_current.border   = theme_selected->border;
    theme_current.back     = theme_selected->back;
    theme_current.fore     = theme_selected->fore;
    theme_current.selfore  = theme_selected->selfore;
    theme_current.selback  = theme_selected->selback;
    theme_current.tipfore  = theme_selected->tipfore;
    theme_current.tipback  = theme_selected->tipback;
    theme_current.menufore = theme_selected->menufore;
    theme_current.menuback = theme_selected->menuback;

    /// Apply New Colors to Widgets
    setupColors();
    }
  return 1;
  }


long FXDesktopSetup::onChooseFont(FXObject*,FXSelector,void*){
  FXFontDialog dialog(this,"Select Normal Font");
  FXFontDesc fontdescription;
  font->getFontDesc(fontdescription);
  strncpy(fontdescription.face,font->getActualName().text(),sizeof(fontdescription.face));
  dialog.setFontSelection(fontdescription);
  if(dialog.execute(PLACEMENT_SCREEN)){
    FXFont *oldfont=font;
    dialog.getFontSelection(fontdescription);
    font=new FXFont(getApp(),fontdescription);
    font->create();
    delete oldfont;
    setupFont();
    }
  return 1;
  }



long FXDesktopSetup::onCmdClose(FXObject*,FXSelector,void*){
  FXint result = FXMessageBox::question(this,MBOX_SAVE_CANCEL_DONTSAVE,"Save Changes?","Do you want to save changes to the FOX Registry\nbefore closing?\n\nIf you don't save, your changes will be lost.");
  if(result==MBOX_CLICKED_SAVE){
    saveFileBinding();
    writeDesktop();
    return 0;
    }
  else if(result==MBOX_CLICKED_CANCEL){
    return 1;
    }
  else{
    return 0;
    }
  return 0;
  }

static FXString weightToString(FXuint weight){
  switch(weight){
    case FXFont::Thin      : return "thin"; break;
    case FXFont::ExtraLight: return "extralight"; break;
    case FXFont::Light     : return "light"; break;
    case FXFont::Normal    : return "normal"; break;
    case FXFont::Medium    : return "medium"; break;
    case FXFont::DemiBold  : return "demibold"; break;
    case FXFont::Bold      : return "bold"; break;
    case FXFont::ExtraBold : return "extrabold"; break;
    case FXFont::Black     : return "heavy"; break;
    default: return ""; break;
    }
  return "";
  }

static FXString slantToString(FXuint slant){
  switch(slant){
    case FXFont::Straight       : return "regular"; break;
    case FXFont::Italic         : return "italic"; break;
    case FXFont::Oblique        : return "oblique"; break;
    case FXFont::ReverseItalic  : return "reverse italic"; break;
    case FXFont::ReverseOblique : return "reverse oblique"; break;
    default : return ""; break;
    }
  return "";
  }

void FXDesktopSetup::setupFont(){
  FXString fontname = font->getActualName() +", " + FXStringVal(font->getSize()/10);
  if(font->getWeight()!=0 && font->getWeight()!=FXFont::Normal){
    fontname += ", " + weightToString(font->getWeight());
    }
  if (font->getSlant()!=0 && font->getSlant()!=FXFont::Straight){
    fontname += ", " + slantToString(font->getSlant());
    }

  tabitem->setFont(font);
  label1->setFont(font);
  label2->setFont(font);
  label3->setFont(font);
  label4->setFont(font);
  label5->setFont(font);

  menulabels[0]->setFont(font);
  menulabels[1]->setFont(font);
  menulabels[2]->setFont(font);
  menulabels[3]->setFont(font);
  menulabels[4]->setFont(font);
  menulabels[5]->setFont(font);

  textfield1->setFont(font);
  fontbutton->setText(fontname);
  }


void FXDesktopSetup::initColors(){
  FXint i,scheme=-1;

  /// Find the correct current scheme
  for(i=0;i<numThemes;i++){
    if((theme_current.base==ColorThemes[i].base) &&
       (theme_current.border==ColorThemes[i].border) &&
       (theme_current.back==ColorThemes[i].back) &&
       (theme_current.fore==ColorThemes[i].fore) &&
       (theme_current.selfore==ColorThemes[i].selfore) &&
       (theme_current.selback==ColorThemes[i].selback) &&
       (theme_current.menufore==ColorThemes[i].menufore) &&
       (theme_current.menuback==ColorThemes[i].menuback) &&
       (theme_current.tipfore==ColorThemes[i].tipfore) &&
       (theme_current.tipback==ColorThemes[i].tipback)){
      scheme=i;
      break;
      }
    }

  if (scheme==-1) {
    theme_user.base = theme_current.base;
    theme_user.border = theme_current.border;
    theme_user.back = theme_current.back;
    theme_user.fore = theme_current.fore;
    theme_user.selfore = theme_current.selfore;
    theme_user.selback = theme_current.selback;
    theme_user.menufore = theme_current.menufore;
    theme_user.menuback = theme_current.menuback;
    theme_user.tipfore = theme_current.tipfore;
    theme_user.tipback = theme_current.tipback;
    list->appendItem("Current",NULL,&theme_user);
    }

  /// Add Standard Themes to List
  for(i=0; i<numThemes; i++){
    list->appendItem(ColorThemes[i].name,NULL,(void*)&ColorThemes[i]);
    }
  list->appendItem("User Defined");
  list->setCurrentItem(scheme);
  }


void FXDesktopSetup::setupColors(){
  shadow = makeShadowColor(theme_current.base);
  hilite = makeHiliteColor(theme_current.base);

  tabitem->setBorderColor(theme_current.border);
  tabitem->setBaseColor(theme_current.base);
  tabitem->setBackColor(theme_current.base);
  tabitem->setTextColor(theme_current.fore);
  tabitem->setShadowColor(shadow);
  tabitem->setHiliteColor(hilite);

  tabframe->setBorderColor(theme_current.border);
  tabframe->setBaseColor(theme_current.base);
  tabframe->setBackColor(theme_current.base);
  tabframe->setShadowColor(shadow);
  tabframe->setHiliteColor(hilite);


  tabsubframe->setBorderColor(theme_current.border);
  tabsubframe->setBaseColor(theme_current.base);
  tabsubframe->setBackColor(theme_current.base);
  tabsubframe->setShadowColor(shadow);
  tabsubframe->setHiliteColor(hilite);

  menuframe->setBorderColor(theme_current.border);
  menuframe->setBaseColor(theme_current.base);
  menuframe->setBackColor(theme_current.base);
  menuframe->setShadowColor(shadow);
  menuframe->setHiliteColor(hilite);

  grpbox1->setBorderColor(theme_current.border);
  grpbox1->setBaseColor(theme_current.base);
  grpbox1->setBackColor(theme_current.base);
  grpbox1->setShadowColor(shadow);
  grpbox1->setHiliteColor(hilite);
  grpbox1->setTextColor(theme_current.fore);

  grpbox2->setBorderColor(theme_current.border);
  grpbox2->setBaseColor(theme_current.base);
  grpbox2->setBackColor(theme_current.base);
  grpbox2->setShadowColor(shadow);
  grpbox2->setHiliteColor(hilite);
  grpbox2->setTextColor(theme_current.fore);

  sep1->setBorderColor(theme_current.border);
  sep1->setBaseColor(theme_current.base);
  sep1->setBackColor(theme_current.base);
  sep1->setShadowColor(shadow);
  sep1->setHiliteColor(hilite);

  sep2->setBorderColor(theme_current.border);
  sep2->setBaseColor(theme_current.base);
  sep2->setBackColor(theme_current.base);
  sep2->setShadowColor(shadow);
  sep2->setHiliteColor(hilite);

  sep3->setBorderColor(theme_current.border);
  sep3->setBaseColor(theme_current.base);
  sep3->setBackColor(theme_current.base);
  sep3->setShadowColor(shadow);
  sep3->setHiliteColor(hilite);

  labeltextframe1->setBorderColor(theme_current.border);
  labeltextframe1->setBaseColor(theme_current.base);
  labeltextframe1->setBackColor(theme_current.base);
  labeltextframe1->setShadowColor(shadow);
  labeltextframe1->setHiliteColor(hilite);


  labeltextframe2->setBorderColor(theme_current.border);
  labeltextframe2->setBaseColor(theme_current.base);
  labeltextframe2->setBackColor(theme_current.base);
  labeltextframe2->setShadowColor(shadow);
  labeltextframe2->setHiliteColor(hilite);

  label1->setBorderColor(theme_current.border);
  label1->setBaseColor(theme_current.base);
  label1->setBackColor(theme_current.base);
  label1->setTextColor(theme_current.fore);
  label1->setShadowColor(shadow);
  label1->setHiliteColor(hilite);

  label2->setBorderColor(theme_current.tipfore);
  label2->setBaseColor(theme_current.tipback);
  label2->setBackColor(theme_current.tipback);
  label2->setTextColor(theme_current.tipfore);
  label2->setShadowColor(shadow);
  label2->setHiliteColor(hilite);

  label3->setBorderColor(theme_current.border);
  label3->setBaseColor(theme_current.base);
  label3->setBackColor(theme_current.selback);
  label3->setTextColor(theme_current.selfore);
  label3->setShadowColor(shadow);
  label3->setHiliteColor(hilite);

  label4->setBorderColor(theme_current.border);
  label4->setBaseColor(theme_current.base);
  label4->setBackColor(theme_current.base);
  label4->setTextColor(theme_current.fore);
  label4->setShadowColor(shadow);
  label4->setHiliteColor(hilite);

  label5->setBorderColor(theme_current.tipfore);
  label5->setBaseColor(theme_current.tipback);
  label5->setBackColor(theme_current.tipback);
  label5->setTextColor(theme_current.tipfore);
  label5->setShadowColor(shadow);
  label5->setHiliteColor(hilite);

  for (int i=0;i<6;i++){
    menulabels[i]->setBorderColor(theme_current.border);
    menulabels[i]->setBaseColor(theme_current.base);
    menulabels[i]->setBackColor(theme_current.base);
    menulabels[i]->setTextColor(theme_current.fore);
    menulabels[i]->setShadowColor(shadow);
    menulabels[i]->setHiliteColor(hilite);
    }

  menulabels[4]->setBorderColor(theme_current.border);
  menulabels[4]->setBaseColor(theme_current.menuback);
  menulabels[4]->setBackColor(theme_current.menuback);
  menulabels[4]->setTextColor(theme_current.menufore);
  menulabels[4]->setShadowColor(shadow);
  menulabels[4]->setHiliteColor(hilite);

  textframe1->setBorderColor(theme_current.border);
  textframe1->setBaseColor(theme_current.base);
  textframe1->setBackColor(theme_current.back);
  textframe1->setShadowColor(shadow);
  textframe1->setHiliteColor(hilite);

  textframe2->setBorderColor(theme_current.border);
  textframe2->setBaseColor(theme_current.base);
  textframe2->setBackColor(theme_current.back);
  textframe2->setShadowColor(shadow);
  textframe2->setHiliteColor(hilite);

  textfield1->setBorderColor(theme_current.border);
  textfield1->setBackColor(theme_current.back);
  textfield1->setBaseColor(theme_current.base);
  textfield1->setTextColor(theme_current.fore);
  textfield1->setSelTextColor(theme_current.selfore);
  textfield1->setSelBackColor(theme_current.selback);
  textfield1->setCursorColor(theme_current.fore);
  textfield1->setShadowColor(shadow);
  textfield1->setHiliteColor(hilite);

  tooltip->setTextColor(theme_current.tipfore);
  tooltip->setBackColor(theme_current.tipback);
  }


FXString FXDesktopSetup::getOutputFile(){
  FXString desktopfile=FXSystem::getHomeDirectory() + PATHSEPSTRING +".foxrc";

  if(!FXStat::exists(desktopfile)){
    if(!FXDir::create(desktopfile)){
      return FXString::null;
      }
    }

  if(applicationname.empty()){
    desktopfile = desktopfile + PATHSEPSTRING + "Desktop";
    }
  else if(vendorname.empty()){
    desktopfile = desktopfile + PATHSEPSTRING + applicationname;
    }
  else{
    desktopfile = desktopfile + PATHSEPSTRING + vendorname;
    if(!FXStat::exists(desktopfile)){
      if(!FXDir::create(desktopfile)){
        return FXString::null;
        }
      }
    desktopfile += PATHSEPSTRING + applicationname;
    }
  return desktopfile;
  }




FXbool FXDesktopSetup::writeDesktop(){
/*
  fxmessage("{\"\",FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d),FXRGB(%d,%d,%d)}\n",
    FXREDVAL(base),FXGREENVAL(base),FXBLUEVAL(base),
    FXREDVAL(border),FXGREENVAL(border),FXBLUEVAL(border),
    FXREDVAL(back),FXGREENVAL(back),FXBLUEVAL(back),
    FXREDVAL(fore),FXGREENVAL(fore),FXBLUEVAL(fore),
    FXREDVAL(selback),FXGREENVAL(selback),FXBLUEVAL(selback),
    FXREDVAL(selfore),FXGREENVAL(selfore),FXBLUEVAL(selfore),
    FXREDVAL(tipback),FXGREENVAL(tipback),FXBLUEVAL(tipback),
    FXREDVAL(tipfore),FXGREENVAL(tipfore),FXBLUEVAL(tipfore),
    FXREDVAL(menuback),FXGREENVAL(menuback),FXBLUEVAL(menuback),
    FXREDVAL(menufore),FXGREENVAL(menufore),FXBLUEVAL(menufore)

    );
*/

        FXString desktopfile=getOutputFile();

  // Save Colors
  desktopsettings.writeColorEntry("SETTINGS","basecolor",theme_current.base);
  desktopsettings.writeColorEntry("SETTINGS","bordercolor",theme_current.border);
  desktopsettings.writeColorEntry("SETTINGS","backcolor",theme_current.back);
  desktopsettings.writeColorEntry("SETTINGS","forecolor",theme_current.fore);
  desktopsettings.writeColorEntry("SETTINGS","hilitecolor",hilite);
  desktopsettings.writeColorEntry("SETTINGS","shadowcolor",shadow);
  desktopsettings.writeColorEntry("SETTINGS","selforecolor",theme_current.selfore);
  desktopsettings.writeColorEntry("SETTINGS","selbackcolor",theme_current.selback);
  desktopsettings.writeColorEntry("SETTINGS","tipforecolor",theme_current.tipfore);
  desktopsettings.writeColorEntry("SETTINGS","tipbackcolor",theme_current.tipback);
  desktopsettings.writeColorEntry("SETTINGS","selmenutextcolor",theme_current.menufore);
  desktopsettings.writeColorEntry("SETTINGS","selmenubackcolor",theme_current.menuback);

  // Save General Settings
  desktopsettings.writeUnsignedEntry("SETTINGS","typingspeed",typingSpeed);
  desktopsettings.writeUnsignedEntry("SETTINGS","clickspeed",clickSpeed);
  desktopsettings.writeUnsignedEntry("SETTINGS","scrollspeed",scrollSpeed);
  desktopsettings.writeUnsignedEntry("SETTINGS","scrolldelay",scrollDelay);
  desktopsettings.writeUnsignedEntry("SETTINGS","blinkspeed",blinkSpeed);
  desktopsettings.writeUnsignedEntry("SETTINGS","animspeed",animSpeed);
  desktopsettings.writeUnsignedEntry("SETTINGS","menupause",menuPause);
  desktopsettings.writeUnsignedEntry("SETTINGS","tippause",tooltipPause);
  desktopsettings.writeUnsignedEntry("SETTINGS","tiptime",tooltipTime);
  desktopsettings.writeUnsignedEntry("SETTINGS","maxcolors",maxcolors);

  desktopsettings.writeIntEntry("SETTINGS","dragdelta",dragDelta);
  desktopsettings.writeIntEntry("SETTINGS","wheellines",wheelLines);

  desktopsettings.writeRealEntry("SETTINGS","displaygamma",gamma);
  desktopsettings.writeStringEntry("SETTINGS","iconpath",iconpath.text());


  FXString fontspec=font->getFont();

  desktopsettings.writeStringEntry("SETTINGS","normalfont",fontspec.text());

  if(!desktopsettings.unparseFile(desktopfile))  return FALSE;

  return TRUE;
  }



int main(int argc,char **argv){

  if (argc>1 && ( (compare(argv[1],"-h")==0) || (compare(argv[1],"--help")==0))) {
    fprintf(stderr,"Usage: ControlPanel [applicationname] [vendorname]\n");
    fprintf(stderr,"Setup desktop appearance for FOX applications. Saves settings to ~/.foxrc/Desktop unless applicationname and optionally vendorname are given.\n");
    return 0;
    }

  FXApp application("FOX Desktop Setup","FOX-DESKTOP");
  application.init(argc,argv);

  FXDesktopSetup * setup = new FXDesktopSetup(&application);
  application.create();

  setup->show(PLACEMENT_SCREEN);

  return application.run();
  }
