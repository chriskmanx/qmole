/********************************************************************************
*                                                                               *
*                                 Data Target Test                              *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: datatarget.cpp,v 1.49 2006/02/10 03:53:48 fox Exp $                      *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif

/*******************************************************************************/


// Mini application object
class DataTargetWindow : public FXMainWindow {
  FXDECLARE(DataTargetWindow)
protected:
  FXMenuBar*         menubar;
  FXMenuPane*        filemenu;
  FXMenuPane*        optionmenu;
  FXPopup*           popup;
  FXMatrix*          matrix;
  FXint              some_int;
  FXdouble           some_double;
  FXint              some_option;
  FXString           some_string;
  FXColor            some_color;
  FXint              some_progress;
  FXDataTarget       int_target;
  FXDataTarget       double_target;
  FXDataTarget       string_target;
  FXDataTarget       option_target;
  FXDataTarget       color_target;
  FXDataTarget       progress_target;
  FXProgressDialog  *progressdialog;
public:
  long onCmdTimer(FXObject*,FXSelector,void*);
  long onCmdQuit(FXObject*,FXSelector,void*);
  long onCmdProgress(FXObject*,FXSelector,void*);
public:
  DataTargetWindow(){}
public:
  enum {
    ID_TIMER=FXMainWindow::ID_LAST,
    ID_PROGRESS,
    ID_QUIT
    };
public:
  DataTargetWindow(FXApp *a);
  void create();
  virtual ~DataTargetWindow();
  };



/*******************************************************************************/

// Map
FXDEFMAP(DataTargetWindow) DataTargetWindowMap[]={
  FXMAPFUNC(SEL_CLOSE,  0,                             DataTargetWindow::onCmdQuit),
  FXMAPFUNC(SEL_SIGNAL, DataTargetWindow::ID_QUIT,     DataTargetWindow::onCmdQuit),
  FXMAPFUNC(SEL_TIMEOUT,DataTargetWindow::ID_TIMER,    DataTargetWindow::onCmdTimer),
  FXMAPFUNC(SEL_COMMAND,DataTargetWindow::ID_PROGRESS, DataTargetWindow::onCmdProgress),
  };


// Object implementation
FXIMPLEMENT(DataTargetWindow,FXMainWindow,DataTargetWindowMap,ARRAYNUMBER(DataTargetWindowMap))



// Make some windows
DataTargetWindow::DataTargetWindow(FXApp* a):FXMainWindow(a,"Data Target Test",NULL,NULL,DECOR_ALL,20,20,700,460){

  // Initialize some simple variables
  some_int = 10;
  some_double = 3.1415927;
  some_string = "FOX";
  some_color = FXRGB(255,0,0);
  some_option = 0;
  some_progress = 0;


  // Connect INTEGER target
  int_target.connect(some_int);

  // Connect DOUBLE target
  double_target.connect(some_double);

  // Connect STRING target
  string_target.connect(some_string);

  // Connect COLOR target
  color_target.connect(some_color);

  // Connect option target
  option_target.connect(some_option);

  // Connect progress target
  progress_target.connect(some_progress);

  // Create progress dialog
  progressdialog=new FXProgressDialog(this,"Progress","We zijn druk, we zijn druk\nWe zijn ongelooflijk druk.",PROGRESSDIALOG_CANCEL|DECOR_BORDER|DECOR_RESIZE|DECOR_TITLE);
  progressdialog->setTarget(&int_target);
  progressdialog->setSelector(FXDataTarget::ID_VALUE);

  // Menubar
  menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  // File menu
  filemenu=new FXMenuPane(this);
    new FXMenuCommand(filemenu,"Progress dialog...",NULL,this,ID_PROGRESS);
    new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);

  // Option menu
  optionmenu=new FXMenuPane(this);

    // The menu radios change the "some_option" variable via the option_target
    new FXMenuRadio(optionmenu,"Option 1",&option_target,FXDataTarget::ID_OPTION+0);
    new FXMenuRadio(optionmenu,"Option 2",&option_target,FXDataTarget::ID_OPTION+1);
    new FXMenuRadio(optionmenu,"Option 3",&option_target,FXDataTarget::ID_OPTION+2);
    new FXMenuRadio(optionmenu,"Option 4",&option_target,FXDataTarget::ID_OPTION+3);

  new FXMenuTitle(menubar,"&Option",NULL,optionmenu);


  // Lone progress bar at the bottom, which reflects the value of variable "some_progress"
  new FXProgressBar(this,&progress_target,FXDataTarget::ID_VALUE,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK);

  new FXHorizontalSeparator(this,LAYOUT_SIDE_TOP|SEPARATOR_GROOVE|LAYOUT_FILL_X);

  FXHorizontalFrame *horframe=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  new FXLabel(horframe,
    "FXDataTarget can be used to connect a Widget to an application variable without any of the\n"
    "tradional \"glue\" programming code.\n\n"
    "The widgets below are connected (via FXDataTarget) to an integer, real, string, option, and\n"
    "color variable, respectively.\n\n"
    "Changing one of them will cause all widgets connected to the same FXDataTarget to \n"
    "update so as to reflect the value of the application variable.\n\n"
    "The progress bar below shows a time-varying variable, demonstrating that widgets\n"
    "can be updated via FXDataTarget's regardless how the variables are changed.\n\n"
    "Note that the \"Option\" pulldown menu is also connected to the option variable!",
    NULL,LAYOUT_LEFT|JUSTIFY_LEFT);

  new FXProgressBar(horframe,&int_target,FXDataTarget::ID_VALUE,PROGRESSBAR_PERCENTAGE|PROGRESSBAR_DIAL|LAYOUT_RIGHT|LAYOUT_FILL_Y|LAYOUT_FILL_X);

  new FXHorizontalSeparator(this,LAYOUT_SIDE_TOP|SEPARATOR_GROOVE|LAYOUT_FILL_X);
  new FXSlider(this,&int_target,FXDataTarget::ID_VALUE,SLIDER_VERTICAL|SLIDER_INSIDE_BAR|LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH,0,0,20,0);

  // Arange nicely
  matrix=new FXMatrix(this,9,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // First row
  new FXLabel(matrix,"&Integer",NULL,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|JUSTIFY_RIGHT|LAYOUT_FILL_ROW);

  // The value of variable "some_int" may be changed by any of these widgets below
  new FXTextField(matrix,10,&int_target,FXDataTarget::ID_VALUE,TEXTFIELD_INTEGER|JUSTIFY_RIGHT|LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  new FXTextField(matrix,10,&int_target,FXDataTarget::ID_VALUE,TEXTFIELD_INTEGER|JUSTIFY_RIGHT|LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  new FXSlider(matrix,&int_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_ROW|LAYOUT_FIX_WIDTH,0,0,100);
  new FXDial(matrix,&int_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_ROW|LAYOUT_FIX_WIDTH|DIAL_HORIZONTAL|DIAL_HAS_NOTCH,0,0,100);
  new FXKnob(matrix,&int_target,FXDataTarget::ID_VALUE,KNOB_TICKS|LAYOUT_CENTER_Y|LAYOUT_CENTER_X);
  new FXSpinner(matrix,5,&int_target,FXDataTarget::ID_VALUE,SPIN_CYCLIC|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW);
  new FXProgressBar(matrix,&int_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK|PROGRESSBAR_PERCENTAGE|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  FX7Segment *seven=new FX7Segment(matrix,FXString::null,SEVENSEGMENT_SHADOW|JUSTIFY_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
  seven->setTarget(&int_target);
  seven->setSelector(FXDataTarget::ID_VALUE);


  // Second row
  new FXLabel(matrix,"&Real",NULL,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|JUSTIFY_RIGHT|LAYOUT_FILL_ROW);

  // The value of variable "some_double" may be changed by the widgets below
  new FXTextField(matrix,10,&double_target,FXDataTarget::ID_VALUE,TEXTFIELD_REAL|JUSTIFY_RIGHT|LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  new FXTextField(matrix,10,&double_target,FXDataTarget::ID_VALUE,TEXTFIELD_REAL|JUSTIFY_RIGHT|LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  FXRealSlider *rslider=new FXRealSlider(matrix,&double_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FIX_WIDTH,0,0,100);
  rslider->setRange(0.0,10.0);
  new FXDial(matrix,&double_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FIX_WIDTH|DIAL_HORIZONTAL|DIAL_HAS_NOTCH,0,0,100);

  FXRealSpinner *rspinner=new FXRealSpinner(matrix,8,&double_target,FXDataTarget::ID_VALUE,REALSPIN_CYCLIC|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW);
  rspinner->setRange(-10.0,10.0);
  rspinner->setIncrement(0.01);
  rspinner->setGranularity(0.01);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  // Third row
  new FXLabel(matrix,"&String",NULL,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|JUSTIFY_RIGHT|LAYOUT_FILL_ROW);

  // The string variable "some_string" can be changed by these text fields
  new FXTextField(matrix,10,&string_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  new FXTextField(matrix,10,&string_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  // Fourth row
  new FXLabel(matrix,"&Option",NULL,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|JUSTIFY_RIGHT|LAYOUT_FILL_ROW);

  // The variable "some_option" is changed by the following widgets
  new FXTextField(matrix,10,&option_target,FXDataTarget::ID_VALUE,TEXTFIELD_INTEGER|LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_ROW);
  new FXRadioButton(matrix,"Option &1",&option_target,FXDataTarget::ID_OPTION+0,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|ICON_BEFORE_TEXT);
  new FXRadioButton(matrix,"Option &2",&option_target,FXDataTarget::ID_OPTION+1,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|ICON_BEFORE_TEXT);
  new FXRadioButton(matrix,"Option &3",&option_target,FXDataTarget::ID_OPTION+2,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|ICON_BEFORE_TEXT);
  new FXRadioButton(matrix,"Option &4",&option_target,FXDataTarget::ID_OPTION+3,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|ICON_BEFORE_TEXT);

  // Even option menus can be hooked up
  popup=new FXPopup(this);
  new FXOption(popup,"First",NULL,&option_target,FXDataTarget::ID_OPTION+0,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(popup,"Second",NULL,&option_target,FXDataTarget::ID_OPTION+1,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(popup,"Third",NULL,&option_target,FXDataTarget::ID_OPTION+2,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  new FXOption(popup,"Fourth",NULL,&option_target,FXDataTarget::ID_OPTION+3,JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  FXOptionMenu *options=new FXOptionMenu(matrix,popup,LAYOUT_TOP|FRAME_RAISED|FRAME_THICK|JUSTIFY_HZ_APART|ICON_AFTER_TEXT);
  options->setTarget(&option_target);
  options->setSelector(FXDataTarget::ID_VALUE);

  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  // Fifth
  new FXLabel(matrix,"&Color",NULL,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|JUSTIFY_RIGHT|LAYOUT_FILL_ROW);

  // Two colorwells connect to the variable "some_color"
  new FXColorWell(matrix,0,&color_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXColorWell(matrix,0,&color_target,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  // Install an accelerator
  getAccelTable()->addAccel(parseAccel("Ctl-Q"),getApp(),FXSEL(SEL_COMMAND,FXApp::ID_QUIT));

  }


// Clean up
DataTargetWindow::~DataTargetWindow(){
  getApp()->removeTimeout(this,ID_TIMER);
  delete progressdialog;
  delete filemenu;
  delete optionmenu;
  delete popup;
  }


// Timer
long DataTargetWindow::onCmdTimer(FXObject*,FXSelector,void*){

  // Increment modulo 100
  some_progress=(some_progress+1)%100;

  // Reset timer for next time
  getApp()->addTimeout(this,ID_TIMER,80);
  return 1;
  }


// Quit
long DataTargetWindow::onCmdQuit(FXObject*,FXSelector,void*){
  getApp()->exit(0);
  return 1;
  }


// Show progress
long DataTargetWindow::onCmdProgress(FXObject*,FXSelector,void*){
  progressdialog->show(PLACEMENT_OWNER);
  return 1;
  }


// Start
void DataTargetWindow::create(){

  // Create windows
  FXMainWindow::create();

  // Kick off the timer
  getApp()->addTimeout(this,ID_TIMER,80);

  // Show
  show(PLACEMENT_SCREEN);
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("DataTarget","FoxTest");

  // Open display
  application.init(argc,argv);

  // Main window
  DataTargetWindow* window=new DataTargetWindow(&application);

  // Handle interrupt to save stuff nicely
  application.addSignal(SIGINT,window,DataTargetWindow::ID_QUIT);

  // Create app
  application.create();

  // Run
  return application.run();
  }
