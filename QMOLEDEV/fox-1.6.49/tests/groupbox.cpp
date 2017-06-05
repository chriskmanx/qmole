/********************************************************************************
*                                                                               *
*                                 Test Group Box                                *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: groupbox.cpp,v 1.115.2.1 2008/04/29 17:18:23 fox Exp $                       *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "xincs.h"

/*******************************************************************************/


// Mini application object
class GroupWindow : public FXMainWindow {
  FXDECLARE(GroupWindow)
protected:

  // Member data
  FXToolTip*         tooltip;
  FXMenuBar*         menubar;
  FXMenuPane*        filemenu;
  FXMenuPane*        popupmenu;
  FXMenuPane*        helpmenu;
  FXMenuPane*        editmenu;
  FXMenuPane*        submenu1;
  FXScrollPane*      scrollpane;
  FXPopup*           pop;
  FXPopup*           coolpop;
  FXHorizontalFrame* contents;
  FXPacker*          group1;
  FXGroupBox*        group2;
  FXGroupBox*        group3;
  FXIcon            *doc;
  FXIcon            *folder_open;
  FXIcon            *folder_closed;
  FXFont            *bigfont;
  FXDataTarget       radiotarget;
  FXuint             choice;

protected:
  GroupWindow(){}

public:

  // Message handlers
  long onCmdFileDlgAny(FXObject*,FXSelector,void*);
  long onCmdFileDlgExisting(FXObject*,FXSelector,void*);
  long onCmdFileDlgMultiple(FXObject*,FXSelector,void*);
  long onCmdFileDlgDirectory(FXObject*,FXSelector,void*);
  long onCmdFileDlgMultipleAll(FXObject*,FXSelector,void*);
  long onCmdDirDlg(FXObject*,FXSelector,void*);
  long onCmdDownSize(FXObject*,FXSelector,void*);
  long onCmdDelete(FXObject*,FXSelector,void*);
  long onCmdAbout(FXObject*,FXSelector,void*);
  long onCmdRadio(FXObject*,FXSelector,void*);
  long onCmdPopup(FXObject*,FXSelector,void*);
  long onUpdRadio(FXObject*,FXSelector,void*);
  long onCmdOption(FXObject*,FXSelector,void*);
  long onCmdIconify(FXObject*,FXSelector,void*);
  long onCmdDeiconify(FXObject*,FXSelector,void*);
  long onCmdChoice(FXObject*,FXSelector,void*);

public:

  // Messages
  enum {
    ID_DOWNSIZE=FXMainWindow::ID_LAST,
    ID_POPUP,
    ID_ABOUT,
    ID_ICONIFY,
    ID_DEICONIFY,
    ID_DELETE,
    ID_FILEDLG_ANY,
    ID_FILEDLG_EXISTING,
    ID_FILEDLG_MULTIPLE,
    ID_FILEDLG_DIRECTORY,
    ID_FILEDLG_MULTIPLE_ALL,
    ID_DIRDLG,
    ID_OPTION1,
    ID_OPTION2,
    ID_OPTION3,
    ID_OPTION4,
    ID_RADIO1,
    ID_RADIO2,
    ID_RADIO3,
    ID_CHOICE
    };

public:
  GroupWindow(FXApp* a);
  virtual void create();
  virtual ~GroupWindow();
  };



/*******************************************************************************/

// Map
FXDEFMAP(GroupWindow) GroupWindowMap[]={

  //__Message_Type__________ID___________________________________________________Message_Handler_____
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_DOWNSIZE,                          GroupWindow::onCmdDownSize),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_DELETE,                            GroupWindow::onCmdDelete),
  FXMAPFUNCS(SEL_COMMAND, GroupWindow::ID_RADIO1,GroupWindow::ID_RADIO3,     GroupWindow::onCmdRadio),
  FXMAPFUNCS(SEL_UPDATE,  GroupWindow::ID_RADIO1,GroupWindow::ID_RADIO3,     GroupWindow::onUpdRadio),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_POPUP,                             GroupWindow::onCmdPopup),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_ABOUT,                             GroupWindow::onCmdAbout),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_FILEDLG_ANY,                       GroupWindow::onCmdFileDlgAny),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_FILEDLG_EXISTING,                  GroupWindow::onCmdFileDlgExisting),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_FILEDLG_MULTIPLE,                  GroupWindow::onCmdFileDlgMultiple),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_FILEDLG_MULTIPLE_ALL,              GroupWindow::onCmdFileDlgMultipleAll),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_FILEDLG_DIRECTORY,                 GroupWindow::onCmdFileDlgDirectory),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_DIRDLG,                            GroupWindow::onCmdDirDlg),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_ICONIFY,                           GroupWindow::onCmdIconify),
  FXMAPFUNC(SEL_TIMEOUT,  GroupWindow::ID_DEICONIFY,                         GroupWindow::onCmdDeiconify),
  FXMAPFUNCS(SEL_COMMAND, GroupWindow::ID_OPTION1,GroupWindow::ID_OPTION4,   GroupWindow::onCmdOption),
  FXMAPFUNC(SEL_COMMAND,  GroupWindow::ID_CHOICE,                            GroupWindow::onCmdChoice),
  };


// Object implementation
FXIMPLEMENT(GroupWindow,FXMainWindow,GroupWindowMap,ARRAYNUMBER(GroupWindowMap))



/*******************************************************************************/


const unsigned char minidoc1[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf1,0x00,0x00,0xbf,0xbf,0xbf,
  0x00,0x00,0x00,0xff,0xff,0xff,0x7f,0x7f,0x7f,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,
  0x10,0x00,0x00,0x02,0x39,0x84,0x8f,0x89,0xc1,0x1d,0x7a,0x82,0x98,0x93,0x41,0x20,
  0x87,0x16,0xf2,0x29,0x49,0x71,0xcd,0x27,0x68,0x9b,0x16,0x0c,0x09,0x18,0x56,0xea,
  0x52,0x9a,0x5b,0xba,0xb6,0x14,0x0d,0xcb,0xf3,0x1b,0xd9,0x6e,0xad,0x1b,0x70,0x78,
  0x06,0x56,0x0b,0x17,0x71,0x28,0x89,0x86,0xa0,0xec,0x02,0x05,0x14,0x00,0x00,0x3b
  };

const unsigned char minifolderopen[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf2,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0x7f,0x7f,0x7f,0xff,0xff,0xff,0xd9,0xd9,0xd9,0xff,0xff,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,0x00,0x03,
  0x42,0x08,0xba,0xdc,0x2c,0x10,0xba,0x37,0x6a,0x15,0x13,0x88,0x41,0x4a,0x27,0x43,
  0x14,0x29,0x9b,0x67,0x82,0x56,0x18,0x68,0xdc,0xe9,0x12,0x42,0x20,0xce,0x62,0x11,
  0x6f,0x69,0x1e,0xc3,0x72,0xfb,0xb9,0xb2,0x18,0xeb,0x47,0xbc,0xad,0x4a,0xc4,0x93,
  0x6c,0xc5,0x7a,0x99,0x62,0x4c,0x1a,0x2d,0xc0,0x04,0x50,0xaf,0x58,0x6c,0x66,0xcb,
  0x6d,0x24,0x00,0x00,0x3b
  };

/* Generated by reswrap from file minifolderclosed.gif */
const unsigned char minifolderclosed[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf2,0x00,0x00,0xb2,0xc0,0xdc,
  0x80,0x80,0x80,0xc0,0xc0,0xc0,0xff,0xff,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0x00,
  0x00,0x00,0x00,0x00,0x00,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,0x00,0x03,
  0x3b,0x08,0xba,0xdc,0x1b,0x10,0x3a,0x16,0xc4,0xb0,0x22,0x4c,0x50,0xaf,0xcf,0x91,
  0xc4,0x15,0x64,0x69,0x92,0x01,0x31,0x7e,0xac,0x95,0x8e,0x58,0x7b,0xbd,0x41,0x21,
  0xc7,0x74,0x11,0xef,0xb3,0x5a,0xdf,0x9e,0x1c,0x6f,0x97,0x03,0xba,0x7c,0xa1,0x64,
  0x48,0x05,0x20,0x38,0x9f,0x50,0xe8,0x66,0x4a,0x75,0x24,0x00,0x00,0x3b
  };


// Make some windows
GroupWindow::GroupWindow(FXApp* a):FXMainWindow(a,"Group Box Test",NULL,NULL,DECOR_ALL,0,0,0,0),radiotarget(choice){

  tooltip=new FXToolTip(getApp(),0,100,100);

  doc=new FXGIFIcon(getApp(),minidoc1);
  folder_open=new FXGIFIcon(getApp(),minifolderopen);
  folder_closed=new FXGIFIcon(getApp(),minifolderclosed);

  bigfont=new FXFont(getApp(),"helvetica,240,bold,italic");

  // Menubar
  menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  filemenu=new FXMenuPane(this);
    new FXMenuCommand(filemenu,"Open any",folder_open,this,ID_FILEDLG_ANY);
    new FXMenuCommand(filemenu,"Open existing",folder_open,this,ID_FILEDLG_EXISTING);
    new FXMenuCommand(filemenu,"Open multiple",folder_open,this,ID_FILEDLG_MULTIPLE);
    new FXMenuCommand(filemenu,"Open multiple all",folder_open,this,ID_FILEDLG_MULTIPLE_ALL);
    new FXMenuCommand(filemenu,"Open directory",folder_open,this,ID_FILEDLG_DIRECTORY);
    new FXMenuCommand(filemenu,"Open directory dialog",folder_open,this,ID_DIRDLG);
    new FXMenuCommand(filemenu,"Open choice dialog",NULL,this,ID_CHOICE);
    new FXMenuRadio(filemenu,"Radio&1",&radiotarget,FXDataTarget::ID_OPTION+1);
    new FXMenuRadio(filemenu,"Radio&2",&radiotarget,FXDataTarget::ID_OPTION+2);
    new FXMenuRadio(filemenu,"Radio&3",&radiotarget,FXDataTarget::ID_OPTION+3);

    new FXMenuCommand(filemenu,"Delete\tCtl-X",NULL,this,ID_DELETE,0);
    new FXMenuCommand(filemenu,"Downsize\tF5\tResize to minimum",NULL,this,ID_DOWNSIZE,0);
    new FXMenuCommand(filemenu,"&Size",NULL,this,ID_DOWNSIZE,0);
    new FXMenuCommand(filemenu,"Maximize",NULL,this,ID_MAXIMIZE,0);     // TEST
    new FXMenuCommand(filemenu,"Minimize",NULL,this,ID_ICONIFY,0);     // TEST
    new FXMenuCommand(filemenu,"Restore",NULL,this,ID_RESTORE,0);       // TEST
    new FXMenuCommand(filemenu,"Dump Widgets",NULL,getApp(),FXApp::ID_DUMP);

    // Make edit popup menu
    editmenu=new FXMenuPane(this);
      new FXMenuCommand(editmenu,"Undo");
      new FXMenuCommand(editmenu,"Cut");
        submenu1=new FXMenuPane(this);
          new FXMenuCommand(submenu1,"&One");
          new FXMenuCommand(submenu1,"&Two");
          new FXMenuCommand(submenu1,"Th&ree");
          new FXMenuCommand(submenu1,"&Four");
      new FXMenuCascade(editmenu,"&Submenu1",NULL,submenu1);
    new FXMenuCascade(filemenu,"&Edit",NULL,editmenu);
    new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT,0);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);


  helpmenu=new FXMenuPane(this);
    new FXMenuCommand(helpmenu,"&About FOX...",NULL,this,ID_ABOUT,0);
  new FXMenuTitle(menubar,"&Help",NULL,helpmenu,LAYOUT_RIGHT);

  popupmenu=new FXMenuPane(this);
    FXTextField* poptext=new FXTextField(popupmenu,10,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP,0,0,0,0);
    poptext->setText("Popup with text");

  scrollpane=new FXScrollPane(this,10,PACK_UNIFORM_HEIGHT);
  new FXMenuCommand(scrollpane,"One");
  new FXMenuCommand(scrollpane,"Two");
  new FXMenuCommand(scrollpane,"Three");
  new FXMenuCommand(scrollpane,"Four");
  new FXMenuCommand(scrollpane,"Five");
  new FXMenuCommand(scrollpane,"Six");
  new FXMenuCommand(scrollpane,"Seven");
  new FXMenuCommand(scrollpane,"Eight");
  new FXMenuCommand(scrollpane,"Nine");
  new FXMenuCommand(scrollpane,"Ten");
  new FXMenuCommand(scrollpane,"Eleven");
  new FXMenuCommand(scrollpane,"Twelve");
  new FXMenuCommand(scrollpane,"Thirteen");
  new FXMenuCommand(scrollpane,"Fourteen");
  new FXMenuCommand(scrollpane,"Fifteen");
  new FXMenuCommand(scrollpane,"Sixteen");
  new FXMenuCommand(scrollpane,"Seventeen");
  new FXMenuCommand(scrollpane,"Eighteen");
  new FXMenuCommand(scrollpane,"Nineteen");
  new FXMenuCommand(scrollpane,"Twenty");
  new FXMenuTitle(menubar,"&Scroll",NULL,scrollpane);

  // Status bar
  FXStatusBar *status=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|STATUSBAR_WITH_DRAGCORNER);

  new FXLabel(status,"10:15 PM",NULL,LAYOUT_FILL_Y|LAYOUT_RIGHT|FRAME_SUNKEN);

  // Content
  contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  group1=new FXGroupBox(contents,"Title Left",GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  group2=new FXGroupBox(contents,"Slider Tests",GROUPBOX_TITLE_CENTER|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  group3=new FXGroupBox(contents,"Title Right",GROUPBOX_TITLE_RIGHT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  FXLabel *testlabel=new FXLabel(group1,"Big Font",NULL,LAYOUT_CENTER_X|JUSTIFY_CENTER_X);
  testlabel->setFont(bigfont);

  new FXButton(group1,"Small &Button",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);
  new FXButton(group1,"Big Fat Wide Button\nComprising\nthree lines",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);
  new FXToggleButton(group1,"C&losed\tTooltip for closed\tHelp for closed","O&pen\nState\tTooltip for open\tHelp for open",folder_closed,folder_open,NULL,0,ICON_BEFORE_TEXT|JUSTIFY_LEFT|FRAME_RAISED|FRAME_THICK);
  FXTriStateButton *tsb=new FXTriStateButton(group1,"False","True","Maybe",folder_closed,folder_open,doc,NULL,0,ICON_BEFORE_TEXT|JUSTIFY_LEFT|FRAME_RAISED|FRAME_THICK);
  tsb->setState(MAYBE);

  pop=new FXPopup(this);

  new FXOption(pop,"&First\tTip #1\tHelp first",NULL,this,ID_OPTION1,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pop,"&Second\tTip #2\tHelp second",NULL,this,ID_OPTION2,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pop,"Th&ird\tTip #3\tHelp third",NULL,this,ID_OPTION3,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pop,"F&ourth\tTip #4\tHelp fourth",NULL,this,ID_OPTION4,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);

  new FXOptionMenu(group1,pop,LAYOUT_TOP|FRAME_RAISED|FRAME_THICK|JUSTIFY_HZ_APART|ICON_AFTER_TEXT);

  new FXLabel(group1,"Te&kstje",NULL,LAYOUT_TOP|JUSTIFY_LEFT);
  new FXButton(group1,"Add an `&&' by doubling\tTooltip\tHelp text for status",NULL,NULL,0,LAYOUT_TOP|FRAME_RAISED|FRAME_THICK);
  new FXButton(group1,"Te&kstje",NULL,this,ID_POPUP,LAYOUT_TOP|FRAME_RAISED|FRAME_THICK);

  new FXMenuButton(group1,"&Menu",NULL,filemenu,MENUBUTTON_ATTACH_BOTH|MENUBUTTON_DOWN|JUSTIFY_HZ_APART|LAYOUT_TOP|FRAME_RAISED|FRAME_THICK|ICON_AFTER_TEXT);
  new FXMenuButton(group1,"&Menu",NULL,filemenu,MENUBUTTON_UP|LAYOUT_TOP|FRAME_RAISED|FRAME_THICK|ICON_AFTER_TEXT);

  coolpop=new FXPopup(this,POPUP_HORIZONTAL);
  new FXButton(coolpop,"A\tTipA",NULL,NULL,0,FRAME_THICK|FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,30,30);
  new FXButton(coolpop,"B\tTipB",NULL,NULL,0,FRAME_THICK|FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,30,30);
  new FXButton(coolpop,"C\tTipC",NULL,NULL,0,FRAME_THICK|FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,30,30);
  new FXButton(coolpop,"D\tTipD",NULL,NULL,0,FRAME_THICK|FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,30,30);
  new FXMenuButton(group1,"&S\tSideways",NULL,coolpop,MENUBUTTON_ATTACH_BOTH|MENUBUTTON_LEFT|MENUBUTTON_NOARROWS|LAYOUT_TOP|FRAME_RAISED|FRAME_THICK|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,30,30);

  FXMatrix* matrix=new FXMatrix(group1,3,FRAME_RAISED|LAYOUT_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  new FXButton(matrix,"A",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_FILL_ROW);
  new FXButton(matrix,"&Wide button",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X);
  new FXButton(matrix,"A",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X);

  new FXButton(matrix,"BBBB",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXButton(matrix,"B",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
  new FXButton(matrix,"BB",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);

  new FXButton(matrix,"C",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_CENTER_X|LAYOUT_FILL_ROW);
  new FXButton(matrix,"&wide",NULL,NULL,0,FRAME_RAISED|FRAME_THICK);
  new FXButton(matrix,"CC",NULL,NULL,0,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT);

  FXSlider *slider;
  new FXLabel(group2,"No Arrow");
  slider=new FXSlider(group2,NULL,0,LAYOUT_TOP|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|SLIDER_HORIZONTAL,0,0,200,30);

  new FXLabel(group2,"Up Arrow");
  slider=new FXSlider(group2,NULL,0,LAYOUT_TOP|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|SLIDER_HORIZONTAL|SLIDER_ARROW_UP|SLIDER_TICKS_TOP,0,0,200,30);
  slider->setRange(0,10);

  new FXLabel(group2,"Down Arrow");
  slider=new FXSlider(group2,NULL,0,LAYOUT_TOP|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|SLIDER_HORIZONTAL|SLIDER_ARROW_DOWN|SLIDER_TICKS_BOTTOM,0,0,200,30);
  slider->setRange(0,10);

  new FXLabel(group2,"Inside Bar");
  slider=new FXSlider(group2,NULL,0,LAYOUT_TOP|LAYOUT_FILL_X|LAYOUT_FIX_HEIGHT|SLIDER_HORIZONTAL|SLIDER_INSIDE_BAR|SLIDER_TICKS_BOTTOM,0,0,200,20);
  slider->setRange(0,3);

  FXHorizontalFrame *frame=new FXHorizontalFrame(group2,LAYOUT_FILL_X|LAYOUT_FILL_Y);


  slider=new FXSlider(frame,NULL,0,LAYOUT_FIX_HEIGHT|SLIDER_VERTICAL|SLIDER_TICKS_LEFT|SLIDER_TICKS_RIGHT,0,0,30,200);
  slider->setRange(0,10);
  slider=new FXSlider(frame,NULL,0,LAYOUT_FIX_HEIGHT|SLIDER_VERTICAL|SLIDER_ARROW_RIGHT|SLIDER_TICKS_RIGHT,0,0,30,200);
  slider->setRange(0,10);
  slider=new FXSlider(frame,NULL,0,LAYOUT_FIX_HEIGHT|SLIDER_VERTICAL|SLIDER_ARROW_LEFT|SLIDER_TICKS_LEFT,0,0,30,200);
  slider->setRange(0,10);
  slider=new FXSlider(frame,NULL,0,LAYOUT_FIX_HEIGHT|SLIDER_VERTICAL|SLIDER_INSIDE_BAR|SLIDER_TICKS_LEFT,0,0,20,200);
  slider->setRange(0,7);
  slider->setTickDelta(7);
  new FXScrollBar(frame,NULL,0,SCROLLBAR_VERTICAL|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,20,300);

  FXVerticalFrame *vframe1=new FXVerticalFrame(frame,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXArrowButton(vframe1,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_UP);
  new FXArrowButton(vframe1,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN);
  new FXArrowButton(vframe1,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_LEFT);
  new FXArrowButton(vframe1,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_RIGHT);
  FXVerticalFrame *vframe2=new FXVerticalFrame(frame,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXArrowButton(vframe2,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_UP|ARROW_TOOLBAR);
  new FXArrowButton(vframe2,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);
  new FXArrowButton(vframe2,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_LEFT|ARROW_TOOLBAR);
  new FXArrowButton(vframe2,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_RIGHT|ARROW_TOOLBAR);

  FXGroupBox *gp=new FXGroupBox(group3,"Group Box",LAYOUT_SIDE_TOP|FRAME_GROOVE|LAYOUT_FILL_X, 0,0,0,0);
  new FXRadioButton(gp,"Radio &1",&radiotarget,FXDataTarget::ID_OPTION+1,ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);
  new FXRadioButton(gp,"Radio &2",&radiotarget,FXDataTarget::ID_OPTION+2,ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);
  new FXRadioButton(gp,"Radio &3",&radiotarget,FXDataTarget::ID_OPTION+3,ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);

  FXPacker *vv=new FXGroupBox(group3,"Group Box",LAYOUT_SIDE_TOP|FRAME_GROOVE|LAYOUT_FILL_X, 0,0,0,0);
  new FXCheckButton(vv,"Hilversum 1",NULL,0,ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);
  new FXCheckButton(vv,"Hilversum 2",NULL,0,ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);
  FXCheckButton *chk1=new FXCheckButton(vv,"One multi-line\nCheckbox Widget",NULL,0,CHECKBUTTON_PLUS|JUSTIFY_LEFT|JUSTIFY_TOP|ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);
  chk1->setCheck(MAYBE);
  FXCheckButton *chk2=new FXCheckButton(vv,fromAscii("Ouvres votre fen\\u00EAtre"),NULL,0,ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP);
  chk2->setCheck(MAYBE);

  FXSpinner *spinner=new FXSpinner(group3,20,NULL,0,SPIN_NORMAL|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP);
  spinner->setRange(1,20);
  spinner->setTipText("tip");
  spinner->setHelpText("help");

  FXRealSpinner *realspinner=new FXRealSpinner(group3,20,NULL,0,REALSPIN_CYCLIC|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP);
  realspinner->setRange(1.0,2.0);
  realspinner->setValue(1.0);
  realspinner->setIncrement(0.1);

  FXComboBox* combobox=new FXComboBox(group3,5,NULL,0,COMBOBOX_INSERT_LAST|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP);
  combobox->setNumVisible(5);
  combobox->appendItem("Very Wide Item");
  for(int i=0; i<3; i++){
    char name[50];
    sprintf(name,"%04d",i);
    combobox->appendItem(name);
    }

  FXTreeListBox *treebox=new FXTreeListBox(group3,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP,0,0,200,0);
  FXTreeItem *branch,*twig,*leaf,*topmost,*topmost2;

  treebox->setNumVisible(10);
  topmost=treebox->appendItem(NULL,"Top",folder_open,folder_closed);
  topmost2=treebox->appendItem(NULL,"Top2",folder_open,folder_closed);
           treebox->appendItem(topmost2,"First",doc,doc);

  treebox->appendItem(topmost,"First",doc,doc);
  treebox->appendItem(topmost,"Second",doc,doc);
  treebox->appendItem(topmost,"Third",doc,doc);
  branch=treebox->appendItem(topmost,"Fourth",folder_open,folder_closed);
    treebox->appendItem(branch,"Fourth-First",doc,doc);
    treebox->appendItem(branch,"Fourth-Second",doc,doc);
    twig=treebox->appendItem(branch,"Fourth-Third",folder_open,folder_closed);
      treebox->appendItem(twig,"Fourth-Third-First",doc,doc);
      treebox->appendItem(twig,"Fourth-Third-Second",doc,doc);
      treebox->appendItem(twig,"Fourth-Third-Third",doc,doc);
      leaf=treebox->appendItem(twig,"Fourth-Third-Fourth",folder_open,folder_closed);
        treebox->appendItem(leaf,"Fourth-Third-Fourth-First",doc,doc);
        treebox->appendItem(leaf,"Fourth-Third-Fourth-Second",doc,doc);
        treebox->appendItem(leaf,"Fourth-Third-Fourth-Third",doc,doc);
    twig=treebox->appendItem(branch,"Fourth-Fourth",folder_open,folder_closed);
      treebox->appendItem(twig,"Fourth-Fourth-First",doc,doc);
      treebox->appendItem(twig,"Fourth-Fourth-Second",doc,doc);
      treebox->appendItem(twig,"Fourth-Fourth-Third",doc,doc);

  new FXLabel(group3,"H&it the hotkey",NULL,LAYOUT_CENTER_X|JUSTIFY_CENTER_X|FRAME_RAISED);
  FXTextField* textfield0=new FXTextField(group3,20,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  textfield0->setText("Normal Text Field");
  FXTextField* textfield1=new FXTextField(group3,20,NULL,0,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  textfield1->setText("Reverse Text Field");
  FXTextField* textfield5=new FXTextField(group3,20,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  textfield5->setText("Centered Text Field");
  textfield5->setJustify(JUSTIFY_CENTER_X);
  FXTextField* textfield2=new FXTextField(group3,20,NULL,0,JUSTIFY_RIGHT|TEXTFIELD_PASSWD|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  textfield2->setText("Password");
  FXTextField* textfield3=new FXTextField(group3,20,NULL,0,TEXTFIELD_READONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  textfield3->setText("Read Only");
  FXTextField* textfield4=new FXTextField(group3,20,NULL,0,TEXTFIELD_READONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  textfield4->setText("Grayed out");
  textfield4->disable();

  FXTextField* realnumber=new FXTextField(group3,20,NULL,0,TEXTFIELD_REAL|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FIX_HEIGHT,0,0,0,30);
  realnumber->setText("1.0E+3");
  FXTextField* intnumber=new FXTextField(group3,20,NULL,0,TEXTFIELD_INTEGER|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FIX_HEIGHT,0,0,0,30);
  intnumber->setText("1000");

  FXDial *dial2=new FXDial(group3,NULL,0,DIAL_CYCLIC|DIAL_HAS_NOTCH|DIAL_HORIZONTAL|LAYOUT_FILL_X|FRAME_RAISED|FRAME_THICK,0,0,120,0);
  new FXScrollBar(group3,NULL,0,SCROLLBAR_HORIZONTAL|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,300,20);

  FXProgressBar *pbar=new FXProgressBar(group3,NULL,0,LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK|PROGRESSBAR_PERCENTAGE);
  pbar->setProgress(48);
  pbar->setTotal(360);
  FXProgressBar *pbar2=new FXProgressBar(group3,NULL,0,LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK|PROGRESSBAR_VERTICAL|PROGRESSBAR_PERCENTAGE|LAYOUT_SIDE_LEFT);
  pbar2->setTotal(360);
  FXDial *dial1=new FXDial(group3,NULL,0,DIAL_CYCLIC|DIAL_HAS_NOTCH|DIAL_VERTICAL|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_SIDE_LEFT);
  pbar2->setProgress(48);
  dial1->setTarget(pbar2);
  dial1->setSelector(FXWindow::ID_SETVALUE);
  dial2->setTarget(pbar);
  dial2->setSelector(FXWindow::ID_SETVALUE);
  choice=0;

  }

GroupWindow::~GroupWindow(){
  delete filemenu;
  delete helpmenu;
  delete editmenu;
  delete submenu1;
  delete pop;
  delete coolpop;
  delete popupmenu;
  delete doc;
  delete folder_open;
  delete folder_closed;
  delete bigfont;
  }



static const FXchar sourcefiles[]="All Files (*)\nC++ Source Files (*.cpp,*.cxx,*.cc)\nC Source Files (*.c)\nC++ Header Files (*.hpp,*.hxx,*.hh,*.h)\n*.o\nAny Extension (*.*)\nThree Letter (*.\?\?\?)\nREADME*";


// Open any file
long GroupWindow::onCmdFileDlgAny(FXObject*,FXSelector,void*){
  FXString file=FXFileDialog::getSaveFilename(this,"Save file","../tests/groupbox.cpp",sourcefiles,1);
  fxmessage("File=\"%s\"\n",file.text());
  return 1;
  }


// Open existing file
long GroupWindow::onCmdFileDlgExisting(FXObject*,FXSelector,void*){
  FXString file=FXFileDialog::getOpenFilename(this,"Open file","../tests/dippy.h",sourcefiles,3);
  fxmessage("File=\"%s\"\n",file.text());
  return 1;
  }


// Open multiple
long GroupWindow::onCmdFileDlgMultiple(FXObject*,FXSelector,void*){
  FXString* files=FXFileDialog::getOpenFilenames(this,"Open files","../tests/groupbox.cpp",sourcefiles);
  if(files){
    for(int i=0; !files[i].empty(); i++){
      fxmessage("Files=\"%s\"\n",files[i].text());
      }
    delete [] files;
    }
  return 1;
  }


// Open multiple all
long GroupWindow::onCmdFileDlgMultipleAll(FXObject*,FXSelector,void*){
  FXFileDialog open(this,"Open files or directories");
  open.setSelectMode(SELECTFILE_MULTIPLE_ALL);
  if(open.execute(PLACEMENT_CURSOR)){
    FXString* files=open.getFilenames();
    if(files){
      for(int i=0; !files[i].empty(); i++){
        fxmessage("Files=\"%s\"\n",files[i].text());
        }
      delete [] files;
      }
    }
  return 1;
  }


// Open existing directory
long GroupWindow::onCmdFileDlgDirectory(FXObject*,FXSelector,void*){
  FXString dir=FXFileDialog::getOpenDirectory(this,"Open directory",PATHSEPSTRING);
  fxmessage("Dir=\"%s\"\n",dir.text());
  return 1;
  }


// Open
long GroupWindow::onCmdDirDlg(FXObject*,FXSelector,void*){
  FXDirDialog open(this,"Open directory");
  open.showFiles(TRUE);
  if(open.execute()){
    fxmessage("Dir=%s\n",open.getDirectory().text());
    }
  return 1;
  }


// Option
long GroupWindow::onCmdOption(FXObject*,FXSelector sel,void*){
  fprintf(stderr,"Chose option %d\n",FXSELID(sel)-ID_OPTION1+1);
  return 1;
  }


// Test something
long GroupWindow::onCmdDownSize(FXObject*,FXSelector,void*){
  resize(getDefaultWidth(),getDefaultHeight());
  return 1;
  }


// Test delete
long GroupWindow::onCmdDelete(FXObject*,FXSelector,void*){
  delete group2;
  group2=NULL;
  return 1;
  }


// Pop up menu
long GroupWindow::onCmdPopup(FXObject*,FXSelector,void*){
  FXint x,y; FXuint buttons;
  getRoot()->getCursorPosition(x,y,buttons);
  popupmenu->popup(NULL,x,y);
  return 1;
  }


// Set choice
long GroupWindow::onCmdRadio(FXObject*,FXSelector sel,void*){
  choice=FXSELID(sel);
  return 1;
  }

// Test of iconify
long GroupWindow::onCmdIconify(FXObject*,FXSelector,void*){
  minimize();
  getApp()->addTimeout(this,ID_DEICONIFY,2000);
  FXTRACE((1,"iconify\n"));
  return 1;
  }


// Test of deiconify
long GroupWindow::onCmdDeiconify(FXObject*,FXSelector,void*){
  restore();
  FXTRACE((1,"deiconify\n"));
  return 1;
  }


// Update menu
long GroupWindow::onUpdRadio(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,(FXSELID(sel)==choice)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),(void*)&choice);
  return 1;
  }


// About
long GroupWindow::onCmdAbout(FXObject*,FXSelector,void*){
  FXMessageBox::information(this,MBOX_OK,"About FOX:- An intentionally long title","FOX is a really, really cool C++ library!\nExample written by Jeroen");
  return 1;
  }


// Set choice
long GroupWindow::onCmdChoice(FXObject*,FXSelector,void*){
  FXGIFIcon icon(getApp(),minifolderclosed);
  FXint choice=FXChoiceBox::ask(this,DECOR_RESIZE,"Choose","What is your choice?",&icon,"One\nTwo\nThree\nFour\nFive\nSix\nSeven\nOne very very very very very long entry");
  FXTRACE((1,"choice=%d\n",choice));
  return 1;
  }


// Start
void GroupWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("Groupbox","FoxTest");

  // Open display
  application.init(argc,argv);

  // Make window
  new GroupWindow(&application);

  // Create app
  application.create();

  // Run
  return application.run();
  }


