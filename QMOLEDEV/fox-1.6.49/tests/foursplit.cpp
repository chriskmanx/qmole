/********************************************************************************
*                                                                               *
*                          Test 4-Way  Splitter  Widget                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: foursplit.cpp,v 1.21 2006/02/20 03:32:13 fox Exp $                       *
********************************************************************************/
#include "fx.h"



/*******************************************************************************/


// Mini application object
class FourSplitWindow : public FXMainWindow {
  FXDECLARE(FourSplitWindow)
protected:
  FXMenuBar*         menubar;
  FXMenuPane*        filemenu;
  FXMenuPane*        expandmenu;
  FX4Splitter*       splitter;
  FX4Splitter*       subsplitter;
protected:
  FourSplitWindow(){}
public:
  FourSplitWindow(FXApp *a);
  virtual void create();
  virtual ~FourSplitWindow();
  };



/*******************************************************************************/


// Object implementation
FXIMPLEMENT(FourSplitWindow,FXMainWindow,NULL,0)


// Make some windows
FourSplitWindow::FourSplitWindow(FXApp *a):FXMainWindow(a,"4-Way Splitter Test",NULL,NULL,DECOR_ALL,0,0,900,700,0,0){
  FXButton *temp;

  // Menu bar
  menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  // Status bar
  new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|STATUSBAR_WITH_DRAGCORNER);

  FXHorizontalFrame* hf=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);

  splitter=new FX4Splitter(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FOURSPLITTER_TRACKING);

  // File menu
  filemenu=new FXMenuPane(this);
  new FXMenuCommand(filemenu,"&Quit\tCtl-Q\tQuit the application.",NULL,getApp(),FXApp::ID_QUIT);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);

  // Expand menu
  expandmenu=new FXMenuPane(this);
  new FXMenuCheck(expandmenu,"All four",splitter,FX4Splitter::ID_EXPAND_ALL);
  new FXMenuCheck(expandmenu,"Top/left",splitter,FX4Splitter::ID_EXPAND_TOPLEFT);
  new FXMenuCheck(expandmenu,"Top/right",splitter,FX4Splitter::ID_EXPAND_TOPRIGHT);
  new FXMenuCheck(expandmenu,"Bottom/left",splitter,FX4Splitter::ID_EXPAND_BOTTOMLEFT);
  new FXMenuCheck(expandmenu,"Bottom/right",splitter,FX4Splitter::ID_EXPAND_BOTTOMRIGHT);
  new FXMenuTitle(menubar,"&Expand",NULL,expandmenu);


  // Four widgets in the four splitter
  FXButton *tl=new FXButton(splitter,"Top &Left\tThis splitter tracks",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);
  FXButton *tr=new FXButton(splitter,"Top &Right\tThis splitter tracks",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);

  // To check bug from Tony <verant@hotpop.com>
  FXSplitter* spl2=new FXSplitter(splitter,SPLITTER_VERTICAL|SPLITTER_TRACKING|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXSplitter* spl3=new FXSplitter(spl2,SPLITTER_HORIZONTAL|SPLITTER_TRACKING|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXButton(spl3,"In SPLITTER_HORIZONTAL",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);
  new FXButton(spl3,"In SPLITTER_HORIZONTAL",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);
  new FXButton(spl2,"In SPLITTER_VERTICAL",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);

  subsplitter=new FX4Splitter(splitter,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  temp=new FXButton(subsplitter,"&Of course\tThis splitter does NOT track",NULL,NULL,0,FRAME_SUNKEN|FRAME_THICK);
  temp->setBackColor(FXRGB(0,128,0));
  temp->setTextColor(FXRGB(255,255,255));
  temp=new FXButton(subsplitter,"the&y CAN\tThis splitter does NOT track",NULL,NULL,0,FRAME_SUNKEN|FRAME_THICK);
  temp->setBackColor(FXRGB(128,0,0));
  temp->setTextColor(FXRGB(255,255,255));
  temp=new FXButton(subsplitter,"be &NESTED\tThis splitter does NOT track",NULL,NULL,0,FRAME_SUNKEN|FRAME_THICK);
  temp->setBackColor(FXRGB(0,0,200));
  temp->setTextColor(FXRGB(255,255,255));
  temp=new FXButton(subsplitter,"&arbitrarily!\tThis splitter does NOT track",NULL,NULL,0,FRAME_SUNKEN|FRAME_THICK);
  temp->setBackColor(FXRGB(128,128,0));
  temp->setTextColor(FXRGB(255,255,255));

  new FXLabel(hf,"Hide: ",NULL,LAYOUT_CENTER_Y|LAYOUT_LEFT);
  new FXCheckButton(hf,"Top Left",tl,ID_TOGGLESHOWN,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_LEFT);
  new FXCheckButton(hf,"Top Right",tr,ID_TOGGLESHOWN,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_LEFT);
  new FXCheckButton(hf,"Bottom Left",spl2,ID_TOGGLESHOWN,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_LEFT);
  new FXCheckButton(hf,"Bottom Right",subsplitter,ID_TOGGLESHOWN,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_LEFT);

  new FXCheckButton(hf,"Bottom Right",splitter,FX4Splitter::ID_EXPAND_BOTTOMRIGHT,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
  new FXCheckButton(hf,"Bottom Left",splitter,FX4Splitter::ID_EXPAND_BOTTOMLEFT,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
  new FXCheckButton(hf,"Top Right",splitter,FX4Splitter::ID_EXPAND_TOPRIGHT,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
  new FXCheckButton(hf,"Top Left",splitter,FX4Splitter::ID_EXPAND_TOPLEFT,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
  new FXCheckButton(hf,"All",splitter,FX4Splitter::ID_EXPAND_ALL,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
  new FXLabel(hf,"Expand: ",NULL,LAYOUT_CENTER_Y|LAYOUT_RIGHT);
  new FXToolTip(getApp());
  }


// Clean up
FourSplitWindow::~FourSplitWindow(){
  delete filemenu;
  delete expandmenu;
  }


// Start
void FourSplitWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){
  FXApp application("FourSplit","FoxTest");
  application.init(argc,argv);
  new FourSplitWindow(&application);
  application.create();
  return application.run();
  }


