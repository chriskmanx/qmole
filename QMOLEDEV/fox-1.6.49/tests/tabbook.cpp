/********************************************************************************
*                                                                               *
*                                 Test Tab Book                                 *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997 by Jeroen van der Zijp.   All Rights Reserved.             *
*********************************************************************************
* $Id: tabbook.cpp,v 1.23 2006/02/06 03:06:51 fox Exp $                         *
********************************************************************************/
#include "fx.h"


// Main Window
class TabBookWindow : public FXMainWindow {
  FXDECLARE(TabBookWindow)
protected:

  // Member data
  FXMenuBar*         menubar;
  FXMenuPane*        filemenu;
  FXMenuPane*        tabmenu;
  FXHorizontalFrame* contents;
  FXTabBook*         tabbook;
  FXTabItem*         tab1;
  FXTabItem*         tab2;
  FXTabItem*         tab3;
  FXTabItem*         tab4;
  FXHorizontalFrame* listframe;
  FXHorizontalFrame* fileframe;
  FXHorizontalFrame* dirframe;
  FXHorizontalFrame* textframe;
  FXList*            simplelist;
  FXFileList*        filelist;
  FXDirList*         dirlist;
  FXText*            text;

protected:

  TabBookWindow(){}

public:

  // Message handlers
  long onCmdTabOrient(FXObject*,FXSelector,void*);
  long onCmdPackUniformWidth(FXObject*,FXSelector,void*);
  long onCmdPackNonUniformWidth(FXObject*,FXSelector,void*);
  long onCmdHideShow(FXObject*,FXSelector,void*);
  long onCmdPanel(FXObject*,FXSelector,void*);

public:

  // Messages
  enum{
    ID_TABS_TOP=FXMainWindow::ID_LAST,
    ID_TABS_BOTTOM,
    ID_TABS_LEFT,
    ID_TABS_RIGHT,
    ID_HIDESHOW,
    ID_PANEL,
	ID_PACK_UNIFORM_WIDTH,
	ID_PACK_NON_UNIFORM_WIDTH,
    };
public:
  TabBookWindow(FXApp* a);
  virtual void create();
  virtual ~TabBookWindow();
  };


/*******************************************************************************/

// Map
FXDEFMAP(TabBookWindow) TabBookWindowMap[]={
  FXMAPFUNCS(SEL_COMMAND,TabBookWindow::ID_TABS_TOP,TabBookWindow::ID_TABS_RIGHT,TabBookWindow::onCmdTabOrient),
  FXMAPFUNC(SEL_COMMAND,TabBookWindow::ID_HIDESHOW,TabBookWindow::onCmdHideShow),
  FXMAPFUNC(SEL_COMMAND,TabBookWindow::ID_PANEL,TabBookWindow::onCmdPanel),
  FXMAPFUNC(SEL_COMMAND,TabBookWindow::ID_PACK_UNIFORM_WIDTH,TabBookWindow::onCmdPackUniformWidth),
  FXMAPFUNC(SEL_COMMAND,TabBookWindow::ID_PACK_NON_UNIFORM_WIDTH,TabBookWindow::onCmdPackNonUniformWidth),
  };


// Object implementation
FXIMPLEMENT(TabBookWindow,FXMainWindow,TabBookWindowMap,ARRAYNUMBER(TabBookWindowMap))


/*******************************************************************************/


// Make some windows
TabBookWindow::TabBookWindow(FXApp *a):FXMainWindow(a,"Tab Book Test",NULL,NULL,DECOR_ALL,0,0,600,400){
  FXHorizontalFrame *boxframe;

  // Tooltip
  new FXToolTip(getApp());

  // Menubar
  menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  // Separator
  new FXHorizontalSeparator(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|SEPARATOR_GROOVE);

  // Contents
  contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH);

  // Switcher
  tabbook=new FXTabBook(contents,this,ID_PANEL,PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);

  // First item is a list
  tab1=new FXTabItem(tabbook,"&Simple List",NULL);

  listframe=new FXHorizontalFrame(tabbook,FRAME_THICK|FRAME_RAISED);
  boxframe=new FXHorizontalFrame(listframe,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 0,0,0,0);
  simplelist=new FXList(boxframe,NULL,0,LIST_EXTENDEDSELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  simplelist->appendItem("First Entry");
  simplelist->appendItem("Second Entry");
  simplelist->appendItem("Third Entry");
  simplelist->appendItem("Fourth Entry");

  // Second item is a file list
  tab2=new FXTabItem(tabbook,"F&ile List",NULL);
  fileframe=new FXHorizontalFrame(tabbook,FRAME_THICK|FRAME_RAISED);
  boxframe=new FXHorizontalFrame(fileframe,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 0,0,0,0);
  filelist=new FXFileList(boxframe,NULL,0,ICONLIST_EXTENDEDSELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // Third item is a directory list
  tab3=new FXTabItem(tabbook,"T&ree List",NULL);
  dirframe=new FXHorizontalFrame(tabbook,FRAME_THICK|FRAME_RAISED);
  boxframe=new FXHorizontalFrame(dirframe,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 0,0,0,0);
  dirlist=new FXDirList(boxframe,NULL,0,DIRLIST_SHOWFILES|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // Fourth item is text
  tab4=new FXTabItem(tabbook,"Text",NULL);
  textframe=new FXHorizontalFrame(tabbook,FRAME_THICK|FRAME_RAISED);
  boxframe=new FXHorizontalFrame(textframe,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 0,0,0,0);
  text=new FXText(boxframe,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // File Menu
  filemenu=new FXMenuPane(this);
  new FXMenuCommand(filemenu,"&Simple List",NULL,tabbook,FXTabBar::ID_OPEN_FIRST+0);
  new FXMenuCommand(filemenu,"F&ile List",NULL,tabbook,FXTabBar::ID_OPEN_FIRST+1);
  new FXMenuCommand(filemenu,"T&ree List",NULL,tabbook,FXTabBar::ID_OPEN_FIRST+2);
  new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);

  // Tab side
  tabmenu=new FXMenuPane(this);
  new FXMenuCommand(tabmenu,"Hide/Show Tab 2",NULL,this,TabBookWindow::ID_HIDESHOW);
  new FXMenuCommand(tabmenu,"&Top Tabs",NULL,this,TabBookWindow::ID_TABS_TOP);
  new FXMenuCommand(tabmenu,"&Bottom Tabs",NULL,this,TabBookWindow::ID_TABS_BOTTOM);
  new FXMenuCommand(tabmenu,"&Left Tabs",NULL,this,TabBookWindow::ID_TABS_LEFT);
  new FXMenuCommand(tabmenu,"&Right Tabs",NULL,this,TabBookWindow::ID_TABS_RIGHT);
  new FXMenuCommand(tabmenu,"&Uniform Width Tabs",NULL,this,TabBookWindow::ID_PACK_UNIFORM_WIDTH);
  new FXMenuCommand(tabmenu,"&Nonuniform Width Tabs",NULL,this,TabBookWindow::ID_PACK_NON_UNIFORM_WIDTH);
  new FXMenuTitle(menubar,"&Tab Placement",NULL,tabmenu);

  }


// Clean up
TabBookWindow::~TabBookWindow(){
  delete tabmenu;
  delete filemenu;
  }


// Switch tab orientations
long TabBookWindow::onCmdTabOrient(FXObject*,FXSelector sel,void*){
  FXuint sid=FXSELID(sel);
  switch(sid){
    case ID_TABS_TOP:
      tabbook->setTabStyle(TABBOOK_TOPTABS);
      tab1->setTabOrientation(TAB_TOP);
      tab2->setTabOrientation(TAB_TOP);
      tab3->setTabOrientation(TAB_TOP);
      tab4->setTabOrientation(TAB_TOP);
      break;
    case ID_TABS_BOTTOM:
      tabbook->setTabStyle(TABBOOK_BOTTOMTABS);
      tab1->setTabOrientation(TAB_BOTTOM);
      tab2->setTabOrientation(TAB_BOTTOM);
      tab3->setTabOrientation(TAB_BOTTOM);
      tab4->setTabOrientation(TAB_BOTTOM);
      break;
    case ID_TABS_LEFT:
      tabbook->setTabStyle(TABBOOK_LEFTTABS);
      tab1->setTabOrientation(TAB_LEFT);
      tab2->setTabOrientation(TAB_LEFT);
      tab3->setTabOrientation(TAB_LEFT);
      tab4->setTabOrientation(TAB_LEFT);
      break;
    case ID_TABS_RIGHT:
      tabbook->setTabStyle(TABBOOK_RIGHTTABS);
      tab1->setTabOrientation(TAB_RIGHT);
      tab2->setTabOrientation(TAB_RIGHT);
      tab3->setTabOrientation(TAB_RIGHT);
      tab4->setTabOrientation(TAB_RIGHT);
      break;
    }
 return 1;
  }

// Calculate tab header widths based on largest tab label string
long TabBookWindow::onCmdPackUniformWidth(FXObject*,FXSelector sel,void*){
  FXuint sid=FXSELID(sel);
  FXuint packing_hints = tabbook->getPackingHints();
  packing_hints |= PACK_UNIFORM_WIDTH;
  tabbook->setPackingHints(packing_hints);
  return 1;
  }

// Calculate tab header width individually for each tab label string
long TabBookWindow::onCmdPackNonUniformWidth(FXObject*,FXSelector sel,void*){
  FXuint sid=FXSELID(sel);
  FXuint packing_hints = tabbook->getPackingHints();
  packing_hints &= ~PACK_UNIFORM_WIDTH;
  tabbook->setPackingHints(packing_hints);
  return 1;
  }


// Hide of show a panel
long TabBookWindow::onCmdHideShow(FXObject*,FXSelector,void*){
  if(tab2->shown()){
    tab2->hide();
    fileframe->hide();
    }
  else{
    tab2->show();
    fileframe->show();
    }
  tab2->recalc();
  fileframe->recalc();
  return 1;
  }


// Active panel switched
long TabBookWindow::onCmdPanel(FXObject*,FXSelector,void* ptr){
  FXTRACE((1,"Panel = %d\n",(FXint)(FXival)ptr));
  return 1;
  }


// Show up
void TabBookWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("TabBook","FoxTest");

  // Open display; this reads registry
  application.init(argc,argv);

  // Build a window
  new TabBookWindow(&application);

  // Create app
  application.create();

  // Run
  return application.run();
  }

