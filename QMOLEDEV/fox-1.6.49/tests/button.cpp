/********************************************************************************
*                                                                               *
*                                 Button Test                                   *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: button.cpp,v 1.22 2006/01/22 17:58:59 fox Exp $                          *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*******************************************************************************/


// Mini application object
class ButtonWindow : public FXMainWindow {
  FXDECLARE(ButtonWindow)
protected:
  FXButton*          button;        // Object being tested
  FXStatusBar*       statusbar;     // Status line
  FXHorizontalFrame* contents;      // Container for button
  FXVerticalFrame*   controls;      // Switchs to set various modes
  FXGroupBox*        group0;
  FXGroupBox*        group1;
  FXGroupBox*        group2;
  FXGroupBox*        group3;
  FXGroupBox*        group4;
  FXIcon*            icon;
protected:
  ButtonWindow(){}
public:
  enum {

    // Messages to change icon/text relationship
    ID_ICON_BEFORE_TEXT=FXMainWindow::ID_LAST,
    ID_ICON_AFTER_TEXT,
    ID_ICON_CENTER_HOR,
    ID_ICON_ABOVE_TEXT,
    ID_ICON_BELOW_TEXT,
    ID_ICON_CENTER_VER,

    // Messages to change justification
    ID_JUST_CENTER_X,
    ID_JUST_LEFT,
    ID_JUST_RIGHT,
    ID_JUST_HOR_APART,
    ID_JUST_CENTER_Y,
    ID_JUST_TOP,
    ID_JUST_BOTTOM,
    ID_JUST_VER_APART,

    // Message to change style
    ID_TOOLBAR_STYLE,

    // Message to quit application
    ID_QUIT
    };
public:
  long onCmdIconTextRelation(FXObject*,FXSelector,void*);
  long onUpdIconTextRelation(FXObject*,FXSelector,void*);

  long onCmdJustification(FXObject*,FXSelector,void*);
  long onUpdJustification(FXObject*,FXSelector,void*);

  long onCmdToolbarStyle(FXObject*,FXSelector,void*);

  long onCmdQuit(FXObject*,FXSelector,void*);
public:
  ButtonWindow(FXApp* a);
  void create();
  virtual void save(FXStream& store) const;
  virtual void load(FXStream& store);
  virtual ~ButtonWindow();
  };


/*******************************************************************************/

// Icon data
const unsigned char bigpenguin[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x30,0x00,0x39,0x00,0xf3,0x00,0x00,0xb2,0xc0,0xdc,
  0x5a,0x52,0x41,0x0e,0x0b,0x0b,0x47,0x3b,0x26,0x71,0x6e,0x67,0xb7,0xb5,0xb0,0xd4,
  0xd2,0xce,0xf9,0xf7,0xf7,0x99,0x8d,0x77,0xa3,0x77,0x1b,0xee,0xba,0x12,0xbe,0xa2,
  0x14,0x8b,0x5e,0x07,0xb3,0x8b,0x27,0xee,0xc7,0x1c,0xd2,0x98,0x0f,0x2c,0x00,0x00,
  0x00,0x00,0x30,0x00,0x39,0x00,0x00,0x04,0xfe,0x10,0xc8,0x49,0xab,0x0d,0x22,0x8b,
  0x61,0xbb,0xff,0x20,0xa6,0x8d,0x1c,0x68,0x9e,0xd3,0x30,0xae,0x19,0x81,0xbe,0x9d,
  0xca,0x66,0x81,0xe8,0xc2,0x38,0x31,0x6b,0x84,0x28,0xe0,0x38,0xd9,0x6e,0x15,0x00,
  0xbe,0x84,0xc3,0x91,0x11,0x95,0x9c,0xdd,0x96,0x9f,0xe1,0x20,0x30,0x40,0x16,0xa1,
  0x51,0x01,0xa1,0xe0,0x1b,0x14,0x0a,0x06,0x9d,0xe6,0x8a,0xed,0x6c,0x0c,0x85,0x83,
  0xcc,0x70,0x40,0x18,0x0c,0xa4,0xb2,0x67,0x5a,0x2d,0x00,0x32,0x07,0x4c,0x40,0xad,
  0x29,0xc9,0x2b,0x74,0x5a,0x77,0x02,0x06,0x6b,0x5d,0x7f,0x16,0x2a,0x05,0x02,0x09,
  0x0a,0x0a,0x0b,0x0b,0x0c,0x1b,0x7c,0x19,0x7e,0x88,0x12,0x2a,0x08,0x0d,0x0a,0x0e,
  0x9d,0x9d,0x0a,0x12,0x2b,0x97,0x14,0x18,0x09,0x0f,0x9c,0x9e,0xa9,0xa2,0xa3,0x12,
  0x54,0x9b,0xa8,0x9e,0x0a,0x0f,0x0f,0x2b,0x4f,0x97,0x18,0x0c,0x8e,0xb1,0x0d,0xb3,
  0xb4,0x1b,0x34,0xac,0x00,0x18,0x08,0x0c,0x0f,0x90,0x09,0xa6,0xc6,0x00,0x2a,0x45,
  0x19,0xc1,0x3a,0x05,0x08,0xc9,0x0f,0x0d,0x0d,0x60,0x68,0x2d,0x95,0xac,0x19,0x6f,
  0xd2,0xc8,0x0d,0x00,0x6c,0x07,0x94,0x3c,0xb7,0x93,0x68,0x5f,0x5f,0xe2,0xe3,0x8b,
  0x71,0x88,0xdc,0xec,0xea,0xe3,0xf3,0xe3,0x48,0x82,0x72,0x2d,0xf4,0xeb,0xf4,0xec,
  0x2c,0x72,0x32,0xfc,0x02,0xf2,0x63,0x61,0x09,0x88,0x88,0x3d,0x02,0x13,0xda,0xdb,
  0xb0,0x44,0xcc,0x8f,0x7d,0x01,0xdf,0x48,0x44,0xe0,0xc4,0x88,0x10,0x30,0x09,0xe7,
  0x49,0x4c,0xb3,0xf0,0x07,0x90,0x3e,0xfe,0x10,0x33,0x1e,0xe8,0x56,0x11,0x86,0x8f,
  0x34,0x22,0x05,0xc2,0x99,0x81,0x63,0x4c,0xca,0x8c,0x77,0xec,0xd9,0x02,0xe1,0x30,
  0xe4,0xcb,0x71,0x2b,0x7f,0x69,0x43,0x21,0x03,0xe1,0x4d,0x81,0x3b,0x5e,0x68,0x40,
  0xf9,0x33,0x60,0xc7,0x99,0x1d,0x1c,0x16,0x55,0x38,0x83,0x4c,0x16,0x3c,0x4b,0x03,
  0x3a,0x74,0xf7,0x41,0xc8,0x80,0xa8,0x52,0x83,0x82,0xe8,0x73,0x15,0x2b,0xbd,0x41,
  0xfe,0xb6,0x56,0xf2,0xe9,0xf5,0xc0,0xd4,0x3e,0x26,0x04,0x30,0x48,0xa0,0xa5,0xec,
  0x3c,0x1f,0x95,0x18,0x82,0x38,0xe5,0x48,0x00,0xd1,0xa8,0x9d,0xe0,0xfe,0x72,0x6a,
  0x81,0x97,0x23,0x59,0x02,0x10,0x60,0x35,0xc0,0xa9,0x01,0x8b,0x04,0x0b,0x3e,0x20,
  0x98,0xf5,0x57,0x01,0xdb,0xae,0x45,0x09,0x2b,0x90,0x34,0x82,0x41,0xa7,0x0e,0xd2,
  0xfc,0x7e,0xe2,0xa4,0x02,0xf2,0x4d,0x00,0x8e,0x12,0x08,0xc9,0xe5,0x88,0x82,0xa6,
  0x5e,0x8d,0x1b,0x2f,0xa8,0xf4,0xd3,0x00,0xe8,0x59,0xc8,0xd6,0xd2,0x55,0x80,0x60,
  0x82,0xdf,0xd4,0x8d,0x3b,0xb1,0xcd,0x70,0x37,0xa1,0xeb,0x53,0xbd,0x18,0x73,0x72,
  0x54,0x1b,0x00,0x02,0x5d,0xa9,0x1d,0xa4,0x76,0xf9,0xb2,0xc0,0x2b,0x47,0xb3,0x1b,
  0x4b,0x58,0x3c,0x3c,0x77,0xee,0xdd,0x64,0x33,0x32,0x78,0x8e,0xbb,0x31,0x02,0xea,
  0xdd,0xab,0x73,0x4a,0xd0,0x5b,0xe5,0x86,0xe8,0x7f,0x95,0xa7,0x06,0x8f,0x5b,0x7d,
  0x63,0x9b,0xf4,0xda,0x05,0xa0,0xeb,0xde,0xba,0x72,0xf6,0xe1,0xab,0x27,0x10,0xe9,
  0xb0,0x58,0xfa,0xff,0x8d,0xf1,0x67,0x82,0x1e,0x72,0xb8,0x89,0x16,0xce,0x1b,0xd1,
  0x10,0x60,0x8f,0x7f,0xf9,0xc9,0x52,0x5b,0x70,0xe1,0xb9,0xf7,0x80,0x68,0x4d,0x64,
  0xc0,0x60,0x7d,0x8e,0x34,0x50,0x9c,0x71,0xdf,0x04,0x27,0x5c,0x63,0x13,0xea,0xb5,
  0xc3,0x76,0xe8,0x51,0x83,0x94,0x04,0x04,0x10,0x80,0xcc,0x8a,0x02,0xce,0xd2,0x40,
  0x02,0x0c,0x58,0x55,0xc5,0x8c,0x55,0xd0,0x90,0x40,0x8b,0xd4,0x30,0x70,0xa2,0x05,
  0x29,0x32,0xb0,0xd6,0x8a,0x3e,0x56,0xb1,0x23,0x05,0x03,0xfc,0x08,0xe4,0x86,0x2f,
  0xf4,0xe0,0x63,0x90,0x7c,0x7d,0x40,0xc5,0x92,0x0c,0x34,0x39,0x41,0x04,0x00,0x3b
  };

/*******************************************************************************/


// Map
FXDEFMAP(ButtonWindow) ButtonWindowMap[]={

  // Quit
  FXMAPFUNC(SEL_CLOSE,    0,                                 ButtonWindow::onCmdQuit),
  FXMAPFUNC(SEL_COMMAND,  ButtonWindow::ID_QUIT,             ButtonWindow::onCmdQuit),

  // Change or update icon/text relationship
  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_ICON_BEFORE_TEXT, ButtonWindow::ID_ICON_CENTER_VER, ButtonWindow::onCmdIconTextRelation),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_ICON_BEFORE_TEXT, ButtonWindow::ID_ICON_CENTER_VER, ButtonWindow::onUpdIconTextRelation),

  // Change or update justification
  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_JUST_CENTER_X,    ButtonWindow::ID_JUST_VER_APART,  ButtonWindow::onCmdJustification),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_JUST_CENTER_X,    ButtonWindow::ID_JUST_VER_APART,  ButtonWindow::onUpdJustification),

  FXMAPFUNC(SEL_COMMAND,  ButtonWindow::ID_TOOLBAR_STYLE,    ButtonWindow::onCmdToolbarStyle),
  };


// ButtonApp implementation
FXIMPLEMENT(ButtonWindow,FXMainWindow,ButtonWindowMap,ARRAYNUMBER(ButtonWindowMap))



// Make some windows
ButtonWindow::ButtonWindow(FXApp* a):FXMainWindow(a,"Button Test",NULL,NULL,DECOR_TITLE|DECOR_MINIMIZE|DECOR_MAXIMIZE|DECOR_CLOSE|DECOR_BORDER|DECOR_STRETCHABLE|DECOR_MENU,100,100,800,600){

  // Tooltip
  new FXToolTip(getApp());

  // Status bar
  statusbar=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|STATUSBAR_WITH_DRAGCORNER);

  // Controls on right
  controls=new FXVerticalFrame(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH);

  // Separator
  new FXVerticalSeparator(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y|SEPARATOR_GROOVE);

  // Contents
  contents=new FXHorizontalFrame(this,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH,0,0,0,0,20,20,20,20);

  icon=new FXGIFIcon(getApp(),bigpenguin,0,IMAGE_KEEP);

  // The button
  button=new FXButton(contents,
                      "&This is a multi-line label on\na button to show off the full\ncapabilities of the button object\tIt also has a tooltip\n[which by the way can be multi-line also]\tAnd some helpful message for the status line.",
                      icon,
                      NULL,0,
                      FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,
                      0,0,300,200);

  group0=new FXGroupBox(controls,"Style",GROUPBOX_TITLE_CENTER|FRAME_RIDGE);
  new FXCheckButton(group0,"Toolbar\tCool ``poppy'' style buttons",this,ID_TOOLBAR_STYLE);

  group1=new FXGroupBox(controls,"Horizontal Placement",GROUPBOX_TITLE_CENTER|FRAME_RIDGE);
  new FXRadioButton(group1,"Before Text",this,ID_ICON_BEFORE_TEXT);
  new FXRadioButton(group1,"After Text",this,ID_ICON_AFTER_TEXT);
  new FXRadioButton(group1,"Centered",this,ID_ICON_CENTER_HOR);

  group2=new FXGroupBox(controls,"Vertical Placement",GROUPBOX_TITLE_CENTER|FRAME_RIDGE);
  new FXRadioButton(group2,"Above Text",this,ID_ICON_ABOVE_TEXT);
  new FXRadioButton(group2,"Below Text",this,ID_ICON_BELOW_TEXT);
  new FXRadioButton(group2,"Centered",this,ID_ICON_CENTER_VER);

  group3=new FXGroupBox(controls,"Horizontal Justify",GROUPBOX_TITLE_CENTER|FRAME_RIDGE);
  new FXRadioButton(group3,"Center",this,ID_JUST_CENTER_X);
  new FXRadioButton(group3,"Left",this,ID_JUST_LEFT);
  new FXRadioButton(group3,"Right",this,ID_JUST_RIGHT);
  new FXRadioButton(group3,"Apart",this,ID_JUST_HOR_APART);

  group4=new FXGroupBox(controls,"Vertical Justify",GROUPBOX_TITLE_CENTER|FRAME_RIDGE);
  new FXRadioButton(group4,"Center",this,ID_JUST_CENTER_Y);
  new FXRadioButton(group4,"Top",this,ID_JUST_TOP);
  new FXRadioButton(group4,"Bottom",this,ID_JUST_BOTTOM);
  new FXRadioButton(group4,"Apart",this,ID_JUST_VER_APART);

  new FXButton(controls,"&Quit",NULL,this,ID_QUIT,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_BOTTOM);
  }


// Free up icon
ButtonWindow::~ButtonWindow(){
  delete icon;
  }


// Start
void ButtonWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


// Change the icon/text relationship
long ButtonWindow::onCmdIconTextRelation(FXObject*,FXSelector sel,void*){
  FXuint style=button->getIconPosition();
  switch(FXSELID(sel)){
    case ID_ICON_BEFORE_TEXT:
      style|=ICON_BEFORE_TEXT; style&=~ICON_AFTER_TEXT;
      break;
    case ID_ICON_AFTER_TEXT:
      style|=ICON_AFTER_TEXT; style&=~ICON_BEFORE_TEXT;
      break;
    case ID_ICON_CENTER_HOR:
      style&=~ICON_AFTER_TEXT; style&=~ICON_BEFORE_TEXT;
      break;
    case ID_ICON_ABOVE_TEXT:
      style|=ICON_ABOVE_TEXT; style&=~ICON_BELOW_TEXT;
      break;
    case ID_ICON_BELOW_TEXT:
      style|=ICON_BELOW_TEXT; style&=~ICON_ABOVE_TEXT;
      break;
    case ID_ICON_CENTER_VER:
      style&=~ICON_ABOVE_TEXT; style&=~ICON_BELOW_TEXT;
      break;
    }
  button->setIconPosition(style);
  return 1;
  }


// Update icon/text relationship radio buttons
long ButtonWindow::onUpdIconTextRelation(FXObject* sender,FXSelector sel,void*){
  FXSelector updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);
  FXuint style=button->getIconPosition();
  switch(FXSELID(sel)){
    case ID_ICON_BEFORE_TEXT:
      if((style&ICON_BEFORE_TEXT) && !(style&ICON_AFTER_TEXT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_ICON_AFTER_TEXT:
      if(!(style&ICON_BEFORE_TEXT) && (style&ICON_AFTER_TEXT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_ICON_CENTER_HOR:
      if(!(style&ICON_BEFORE_TEXT) && !(style&ICON_AFTER_TEXT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_ICON_ABOVE_TEXT:
      if((style&ICON_ABOVE_TEXT) && !(style&ICON_BELOW_TEXT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_ICON_BELOW_TEXT:
      if(!(style&ICON_ABOVE_TEXT) && (style&ICON_BELOW_TEXT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_ICON_CENTER_VER:
      if(!(style&ICON_ABOVE_TEXT) && !(style&ICON_BELOW_TEXT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    }
  sender->handle(this,updatemessage,NULL);
  return 1;
  }


// Change justification
long ButtonWindow::onCmdJustification(FXObject*,FXSelector sel,void*){
  FXuint style=button->getJustify();
  switch(FXSELID(sel)){
    case ID_JUST_CENTER_X:
      style&=~JUSTIFY_HZ_APART;
      break;
    case ID_JUST_LEFT:
      style&=~JUSTIFY_HZ_APART;
      style|=JUSTIFY_LEFT;
      break;
    case ID_JUST_RIGHT:
      style&=~JUSTIFY_HZ_APART;
      style|=JUSTIFY_RIGHT;
      break;
    case ID_JUST_HOR_APART:
      style|=JUSTIFY_HZ_APART;
      break;
    case ID_JUST_CENTER_Y:
      style&=~JUSTIFY_VT_APART;
      break;
    case ID_JUST_TOP:
      style&=~JUSTIFY_VT_APART;
      style|=JUSTIFY_TOP;
      break;
    case ID_JUST_BOTTOM:
      style&=~JUSTIFY_VT_APART;
      style|=JUSTIFY_BOTTOM;
      break;
    case ID_JUST_VER_APART:
      style|=JUSTIFY_VT_APART;
      break;
    }
  button->setJustify(style);
  return 1;
  }

// Update justification radio buttons
long ButtonWindow::onUpdJustification(FXObject* sender,FXSelector sel,void*){
  FXSelector updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);
  FXuint style=button->getJustify();
  switch(FXSELID(sel)){
    case ID_JUST_CENTER_X:
      if(!(style&JUSTIFY_LEFT) && !(style&JUSTIFY_RIGHT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_LEFT:
      if((style&JUSTIFY_LEFT) && !(style&JUSTIFY_RIGHT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_RIGHT:
      if(!(style&JUSTIFY_LEFT) && (style&JUSTIFY_RIGHT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_HOR_APART:
      if((style&JUSTIFY_LEFT) && (style&JUSTIFY_RIGHT)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_CENTER_Y:
      if(!(style&JUSTIFY_TOP) && !(style&JUSTIFY_BOTTOM)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_TOP:
      if((style&JUSTIFY_TOP) && !(style&JUSTIFY_BOTTOM)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_BOTTOM:
      if(!(style&JUSTIFY_TOP) && (style&JUSTIFY_BOTTOM)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    case ID_JUST_VER_APART:
      if((style&JUSTIFY_TOP) && (style&JUSTIFY_BOTTOM)) updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
      break;
    }
  sender->handle(this,updatemessage,NULL);
  return 1;
  }


// Set to toolbar style
long ButtonWindow::onCmdToolbarStyle(FXObject*,FXSelector,void* ptr){
  FXuint style=button->getButtonStyle();
  if(ptr){
    style|=BUTTON_TOOLBAR;
    button->setFrameStyle(FRAME_RAISED);
    }
  else{
    style&=~BUTTON_TOOLBAR;
    button->setFrameStyle(FRAME_RAISED|FRAME_THICK);
    }
  button->setButtonStyle(style);
  return 1;
  }


// Quit the application
long ButtonWindow::onCmdQuit(FXObject*,FXSelector,void*){
//  FXFileStream stream;
//  stream.open("button.gui",FXStreamSave);
//  getApp()->writeWindow(stream,this);
//  stream.close();
  getApp()->exit(0);
//  getApp()->dumpWidgets();
  return 1;
  }


// Save
void ButtonWindow::save(FXStream& store) const {
  FXMainWindow::save(store);
  store << button;
  store << controls;
  store << statusbar;
  store << contents;
  store << group0;
  store << group1;
  store << group2;
  store << group3;
  store << group4;
  store << icon;
  }


// Load
void ButtonWindow::load(FXStream& store){
  FXMainWindow::load(store);
  store >> button;
  store >> controls;
  store >> statusbar;
  store >> contents;
  store >> group0;
  store >> group1;
  store >> group2;
  store >> group3;
  store >> group4;
  store >> icon;
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("Button","FoxTest");

  // Open display
  application.init(argc,argv);

//FXWindow *buttons=NULL;

//  if(argc>1){
//    FXFileStream stream;
//    stream.open(argv[1],FXStreamLoad);
//    application.readWindow(stream,buttons,application.getRootWindow(),application.getRootWindow());
//    stream.close();
//    application.dumpWidgets();
//    }

//  else{
    // Main window
    new ButtonWindow(&application);
//   }


  // Create app
  application.create();
//if(buttons) buttons->show();

  // Run
  return application.run();
  }


