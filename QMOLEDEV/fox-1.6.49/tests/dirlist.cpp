/********************************************************************************
*                                                                               *
*                  D i r e c t o r y   L i s t   C o n t r o l                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: dirlist.cpp,v 1.16 2006/01/22 17:58:59 fox Exp $                         *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#ifndef WIN32
#include <unistd.h>
#endif




/*******************************************************************************/


// Main Window
class DirListWindow : public FXMainWindow {
  FXDECLARE(DirListWindow)
protected:
  FXMenuBar*         menubar;
  FXMenuPane*        filemenu;
  FXMenuPane*        helpmenu;
  FXDirList*         contents;
  FXTextField*       text;
protected:
  DirListWindow(){}
public:
  long onCmdAbout(FXObject*,FXSelector,void*);
public:
  enum{
    ID_ABOUT=FXMainWindow::ID_LAST,
    ID_LAST
    };
public:
  DirListWindow(FXApp* a);
  virtual void create();
  virtual ~DirListWindow();
  };



/*******************************************************************************/

// Map
FXDEFMAP(DirListWindow) DirListWindowMap[]={
  FXMAPFUNC(SEL_COMMAND, DirListWindow::ID_ABOUT, DirListWindow::onCmdAbout),
  };


// Object implementation
FXIMPLEMENT(DirListWindow,FXMainWindow,DirListWindowMap,ARRAYNUMBER(DirListWindowMap))


// Make some windows
DirListWindow::DirListWindow(FXApp* a):FXMainWindow(a,"Directory List",NULL,NULL,DECOR_ALL,0,0,800,600){

  // Make menu bar
  menubar=new FXMenuBar(this,LAYOUT_FILL_X);
  filemenu=new FXMenuPane(this);
    new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT);
    new FXMenuTitle(menubar,"&File",NULL,filemenu);
  helpmenu=new FXMenuPane(this);
    new FXMenuCommand(helpmenu,"&About FOX...",NULL,this,ID_ABOUT,0);
    new FXMenuTitle(menubar,"&Help",NULL,helpmenu,LAYOUT_RIGHT);

  // Text field at bottom
  text=new FXTextField(this,10,NULL,0,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK);

  // Make contents
  contents=new FXDirList(this,NULL,0,HSCROLLING_OFF|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|TREELIST_BROWSESELECT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0);

  text->setTarget(contents);
  text->setSelector(FXWindow::ID_SETVALUE);
  }


DirListWindow::~DirListWindow(){
  delete filemenu;
  delete helpmenu;
  }


// About
long DirListWindow::onCmdAbout(FXObject*,FXSelector,void*){
  FXMessageBox::information(this,MBOX_OK,"About FOX","FOX is a really, really cool C++ library!");
  return 1;
  }


// Start
void DirListWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("DirList","FoxTest");

  // Open display
  application.init(argc,argv);

  // Make window
  new DirListWindow(&application);

  // Create app
  application.create();

  // Run
  return application.run();
  }


