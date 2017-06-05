/********************************************************************************
*                                                                               *
*                                 Test Dialog Box                               *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: dialog.cpp,v 1.45 2006/01/22 17:58:59 fox Exp $                          *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



/*******************************************************************************/

class FXTestDialog : public FXDialogBox {
  FXDECLARE(FXTestDialog)
protected:
  FXHorizontalFrame* contents;
  FXHorizontalFrame* buttons;
  FXMenuPane*        menu;
  FXMenuPane*        submenu;
  FXPopup*           pane;
private:
  FXTestDialog(){}
public:
  FXTestDialog(FXWindow* owner);
  virtual ~FXTestDialog();
  };


/*******************************************************************************/


// Mini application object
class DialogTester : public FXMainWindow {
  FXDECLARE(DialogTester)
protected:

  // Member data
  FXMenuBar         *menubar;
  FXMenuPane        *filemenu;
  FXHorizontalFrame *contents;
  FXTestDialog      *dialog;

protected:
  DialogTester(){}

public:

  // Message handlers
  long onCmdShowDialog(FXObject*,FXSelector,void*);
  long onCmdShowDialogModal(FXObject*,FXSelector,void*);

public:

  // Messages
  enum {
    ID_SHOWDIALOG=FXMainWindow::ID_LAST,
    ID_SHOWDIALOGMODAL
    };

public:
  DialogTester(FXApp *app);
  virtual void create();
  virtual ~DialogTester();
  };


/*******************************************************************************/


// FXTestDialog implementation
FXIMPLEMENT(FXTestDialog,FXDialogBox,NULL,0)


// Construct a dialog box
FXTestDialog::FXTestDialog(FXWindow* owner):
  FXDialogBox(owner,"Test of Dialog Box",DECOR_TITLE|DECOR_BORDER){

  // Bottom buttons
  buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|FRAME_NONE|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,40,40,20,20);

  // Separator
  new FXHorizontalSeparator(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|SEPARATOR_GROOVE);

  // Contents
  contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH);

  submenu=new FXMenuPane(this);
  new FXMenuCommand(submenu,"One");
  new FXMenuCommand(submenu,"Two");
  new FXMenuCommand(submenu,"Three");

  // Menu
  menu=new FXMenuPane(this);
  new FXMenuCommand(menu,"&Accept",NULL,this,ID_ACCEPT);
  new FXMenuCommand(menu,"&Cancel",NULL,this,ID_CANCEL);
  new FXMenuCascade(menu,"Submenu",NULL,submenu);
  new FXMenuCommand(menu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT);

  // Popup menu
  pane=new FXPopup(this);
  new FXOption(pane,"One",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Two",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Three",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Four",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Five",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Six",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Seven",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Eight",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Nine",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(pane,"Ten",NULL,NULL,0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);

  FXComboBox* combobox=new FXComboBox(contents,10,NULL,0,COMBOBOX_STATIC|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP);
  combobox->setNumVisible(4);
  combobox->appendItem("One");
  combobox->appendItem("Two");
  combobox->appendItem("Three");
  combobox->appendItem("Four");

  // Option menu
  new FXOptionMenu(contents,pane,FRAME_RAISED|FRAME_THICK|JUSTIFY_HZ_APART|ICON_AFTER_TEXT|LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  // Button to pop menu
  new FXMenuButton(contents,"&Menu",NULL,menu,MENUBUTTON_DOWN|JUSTIFY_LEFT|LAYOUT_TOP|FRAME_RAISED|FRAME_THICK|ICON_AFTER_TEXT|LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  // Accept
 new FXButton(buttons,"&Accept",NULL,this,ID_ACCEPT,BUTTON_DEFAULT|BUTTON_INITIAL|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y);

  // Cancel
  new FXButton(buttons,"&Cancel",NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y);
  }


// Must delete the menus
FXTestDialog::~FXTestDialog(){
  delete menu;
  delete submenu;
  delete pane;
  }

/*******************************************************************************/

// Map
FXDEFMAP(DialogTester) DialogTesterMap[]={
  FXMAPFUNC(SEL_COMMAND,  DialogTester::ID_SHOWDIALOG,      DialogTester::onCmdShowDialog),
  FXMAPFUNC(SEL_COMMAND,  DialogTester::ID_SHOWDIALOGMODAL, DialogTester::onCmdShowDialogModal),
  };


// FXDialogBoxApp implementation
FXIMPLEMENT(DialogTester,FXMainWindow,DialogTesterMap,ARRAYNUMBER(DialogTesterMap))



/*******************************************************************************/


// Make some windows
DialogTester::DialogTester(FXApp* a):FXMainWindow(a,"Group Box Test",NULL,NULL,DECOR_ALL,0,0,400,200){

  // Tooltip
  new FXToolTip(getApp());

  // Menubar
  menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  // File Menu
  filemenu=new FXMenuPane(this);
  new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT,0);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);

  // Separator
  new FXHorizontalSeparator(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|SEPARATOR_GROOVE);

  // Contents
  contents=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|FRAME_NONE|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);

  // Button to pop normal dialog
  new FXButton(contents,"&Non-Modal Dialog...\tDisplay normal dialog",NULL,this,ID_SHOWDIALOG,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  // Button to pop modal dialog
  new FXButton(contents,"&Modal Dialog...\tDisplay modal dialog",NULL,this,ID_SHOWDIALOGMODAL,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  // Build a dialog box
  dialog=new FXTestDialog(this);

  // Separator
  new FXHorizontalSeparator(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|SEPARATOR_GROOVE);

  }


// Clean up
DialogTester::~DialogTester(){
  delete filemenu;
  }


// Open
long DialogTester::onCmdShowDialog(FXObject*,FXSelector,void*){
  dialog->show(PLACEMENT_OWNER);
  return 1;
  }


// Option
long DialogTester::onCmdShowDialogModal(FXObject*,FXSelector,void*){
  FXTestDialog modaldialog(this);
  modaldialog.execute(PLACEMENT_OWNER);
  return 1;
  }


// Start
void DialogTester::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp  application("Dialog","FoxTest");

  // Open display
  application.init(argc,argv);

  new DialogTester(&application);

  // Create app
  application.create();

  // Run
  return application.run();
  }


