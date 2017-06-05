/*****************************************************************************
*                                                                            *
*                                 Layout Example                             *
*                                                                            *
******************************************************************************
* Copyright (C) 2004,2006 by Bill Baxter.   All Rights Reserved.             *
******************************************************************************
* $Id: layout.cpp,v 1.7 2006/02/09 03:42:07 fox Exp $                        *
*****************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
  Notes:
  - Contributed by Bill Baxter.
  - Some coding style changes by Jeroen van der Zijp.
*/

/****************************************************************************/


// Main Window
class ButtonWindow : public FXMainWindow {
  FXDECLARE(ButtonWindow)
protected:
  FXComposite*     contents;            // Container for buttons
  FXVerticalFrame* controls;            // Switchs to set various modes
  FXButton*        lastButton;          // Current child
  FXint            nextButtonNum;
  FXuint           layoutHints;
protected:
  ButtonWindow(){}
public:
  enum {
    ID_BUTTON=FXMainWindow::ID_LAST,

    // Horizontal alignment
    ID_LAYOUT_LEFT,             // Stick on left (default)
    ID_LAYOUT_RIGHT,            // Stick on right
    ID_LAYOUT_CENTER_X,         // Center horizontally
    ID_LAYOUT_FIX_X,            // X fixed

    // Vertical alignment
    ID_LAYOUT_TOP,              // Stick on top (default)
    ID_LAYOUT_BOTTOM,           // Stick on bottom
    ID_LAYOUT_CENTER_Y,         // Center vertically
    ID_LAYOUT_FIX_Y,            // Y fixed

    // Width
    ID_LAYOUT_MIN_WIDTH,        // Minimum width is the default
    ID_LAYOUT_FIX_WIDTH,        // Width fixed
    ID_LAYOUT_FILL_X,           // Stretch or shrink horizontally

    // Height
    ID_LAYOUT_MIN_HEIGHT,       // Minimum height is the default
    ID_LAYOUT_FIX_HEIGHT,       // height fixed
    ID_LAYOUT_FILL_Y,           // Stretch or shrink vertically

    // In a matrix
    ID_LAYOUT_FILL_ROW,         // Matrix row is stretchable
    ID_LAYOUT_FILL_COLUMN,      // Matrix column is stretchable

    // Side in a packer
    ID_LAYOUT_SIDE_TOP,         // Pack on top side (default)
    ID_LAYOUT_SIDE_BOTTOM,      // Pack on bottom side
    ID_LAYOUT_SIDE_LEFT,        // Pack on left side
    ID_LAYOUT_SIDE_RIGHT,       // Pack on right side

    ID_PACK_UNIFORM_HEIGHT,     // Uniform height
    ID_PACK_UNIFORM_WIDTH,      // Uniform width

    ID_MATRIX_BY_ROWS,          // Uniform width
    ID_MATRIX_BY_COLUMNS,       // Uniform width

    ID_CONTAINER_COMPOSITE,
    ID_CONTAINER_HORIZONTAL_FRAME,
    ID_CONTAINER_VERTICAL_FRAME,
    ID_CONTAINER_PACKER,
    ID_CONTAINER_MATRIX,

    ID_FIX_X_VAL,
    ID_FIX_Y_VAL,
    ID_FIX_W_VAL,
    ID_FIX_H_VAL,
    ID_MATRIX_ROWS_COLS,

    ID_ADD_BUTTON,
    ID_CLEAR_BUTTON
    };
public:
  long onCmdAddButton(FXObject*,FXSelector,void*);
  long onCmdClearButton(FXObject*,FXSelector,void*);
  long onUpdClearButton(FXObject*,FXSelector,void*);
  long onCmdLayoutSide(FXObject*,FXSelector,void*);
  long onUpdLayoutSide(FXObject*,FXSelector,void*);
  long onCmdLayoutHorz(FXObject*,FXSelector,void*);
  long onUpdLayoutHorz(FXObject*,FXSelector,void*);
  long onCmdLayoutVert(FXObject*,FXSelector,void*);
  long onUpdLayoutVert(FXObject*,FXSelector,void*);
  long onCmdLayoutWidth(FXObject*,FXSelector,void*);
  long onUpdLayoutWidth(FXObject*,FXSelector,void*);
  long onCmdLayoutHeight(FXObject*,FXSelector,void*);
  long onUpdLayoutHeight(FXObject*,FXSelector,void*);
  long onCmdLayoutMatrix(FXObject*,FXSelector,void*);
  long onUpdLayoutMatrix(FXObject*,FXSelector,void*);
  long onCmdPackHints(FXObject*,FXSelector,void*);
  long onUpdPackHints(FXObject*,FXSelector,void*);

  long onCmdMatrixMode(FXObject*,FXSelector,void*);
  long onUpdMatrixMode(FXObject*,FXSelector,void*);

  long onCmdContainerType(FXObject*,FXSelector,void*);
  long onUpdContainerType(FXObject*,FXSelector,void*);
  long onCmdFixVal(FXObject*,FXSelector,void*);
  long onChgFixVal(FXObject*,FXSelector,void*);
  long onUpdFixVal(FXObject*,FXSelector,void*);

  long onCmdMatVal(FXObject*,FXSelector,void*);
  long onChgMatVal(FXObject*,FXSelector,void*);
  long onUpdMatVal(FXObject*,FXSelector,void*);

  long onCmdButton(FXObject*,FXSelector,void*);
public:
  ButtonWindow(FXApp* a);
  void create();
  };


/*******************************************************************************/


// Map
FXDEFMAP(ButtonWindow) ButtonWindowMap[]={

  FXMAPFUNC(SEL_COMMAND,  ButtonWindow::ID_BUTTON,           ButtonWindow::onCmdButton),

  FXMAPFUNC(SEL_COMMAND,  ButtonWindow::ID_MATRIX_ROWS_COLS, ButtonWindow::onChgMatVal),
  FXMAPFUNC(SEL_CHANGED,  ButtonWindow::ID_MATRIX_ROWS_COLS, ButtonWindow::onChgMatVal),
  FXMAPFUNC(SEL_UPDATE,   ButtonWindow::ID_MATRIX_ROWS_COLS, ButtonWindow::onUpdMatVal),

  FXMAPFUNC(SEL_COMMAND,  ButtonWindow::ID_ADD_BUTTON,       ButtonWindow::onCmdAddButton),
  FXMAPFUNC(SEL_COMMAND,  ButtonWindow::ID_CLEAR_BUTTON,     ButtonWindow::onCmdClearButton),
  FXMAPFUNC(SEL_UPDATE,   ButtonWindow::ID_CLEAR_BUTTON,     ButtonWindow::onUpdClearButton),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_LAYOUT_SIDE_TOP,  ButtonWindow::ID_LAYOUT_SIDE_RIGHT, ButtonWindow::onCmdLayoutSide),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_LAYOUT_SIDE_TOP,  ButtonWindow::ID_LAYOUT_SIDE_RIGHT, ButtonWindow::onUpdLayoutSide),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_LAYOUT_LEFT,      ButtonWindow::ID_LAYOUT_FIX_X,      ButtonWindow::onCmdLayoutHorz),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_LAYOUT_LEFT,      ButtonWindow::ID_LAYOUT_FIX_X,      ButtonWindow::onUpdLayoutHorz),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_LAYOUT_TOP,       ButtonWindow::ID_LAYOUT_FIX_Y,      ButtonWindow::onCmdLayoutVert),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_LAYOUT_TOP,       ButtonWindow::ID_LAYOUT_FIX_Y,      ButtonWindow::onUpdLayoutVert),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_LAYOUT_MIN_WIDTH,  ButtonWindow::ID_LAYOUT_FILL_X,    ButtonWindow::onCmdLayoutWidth),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_LAYOUT_MIN_WIDTH,  ButtonWindow::ID_LAYOUT_FILL_X,    ButtonWindow::onUpdLayoutWidth),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_LAYOUT_MIN_HEIGHT, ButtonWindow::ID_LAYOUT_FILL_Y,    ButtonWindow::onCmdLayoutHeight),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_LAYOUT_MIN_HEIGHT, ButtonWindow::ID_LAYOUT_FILL_Y,    ButtonWindow::onUpdLayoutHeight),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_LAYOUT_FILL_ROW,  ButtonWindow::ID_LAYOUT_FILL_COLUMN,ButtonWindow::onCmdLayoutMatrix),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_LAYOUT_FILL_ROW,  ButtonWindow::ID_LAYOUT_FILL_COLUMN,ButtonWindow::onUpdLayoutMatrix),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_PACK_UNIFORM_HEIGHT, ButtonWindow::ID_PACK_UNIFORM_WIDTH,   ButtonWindow::onCmdPackHints),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_PACK_UNIFORM_HEIGHT, ButtonWindow::ID_PACK_UNIFORM_WIDTH,   ButtonWindow::onUpdPackHints),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_CONTAINER_COMPOSITE, ButtonWindow::ID_CONTAINER_MATRIX,   ButtonWindow::onCmdContainerType),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_CONTAINER_COMPOSITE, ButtonWindow::ID_CONTAINER_MATRIX,   ButtonWindow::onUpdContainerType),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_MATRIX_BY_ROWS, ButtonWindow::ID_MATRIX_BY_COLUMNS,   ButtonWindow::onCmdMatrixMode),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_MATRIX_BY_ROWS, ButtonWindow::ID_MATRIX_BY_COLUMNS,   ButtonWindow::onUpdMatrixMode),

  FXMAPFUNCS(SEL_COMMAND, ButtonWindow::ID_FIX_X_VAL,         ButtonWindow::ID_FIX_H_VAL,          ButtonWindow::onChgFixVal),
  FXMAPFUNCS(SEL_CHANGED, ButtonWindow::ID_FIX_X_VAL,         ButtonWindow::ID_FIX_H_VAL,          ButtonWindow::onChgFixVal),
  FXMAPFUNCS(SEL_UPDATE,  ButtonWindow::ID_FIX_X_VAL,         ButtonWindow::ID_FIX_H_VAL,          ButtonWindow::onUpdFixVal)
  };


// ButtonApp implementation
FXIMPLEMENT(ButtonWindow,FXMainWindow,ButtonWindowMap,ARRAYNUMBER(ButtonWindowMap))


// Layout side
const FXuint LAYOUT_SIDE_MASK=(LAYOUT_SIDE_TOP|LAYOUT_SIDE_BOTTOM|LAYOUT_SIDE_LEFT|LAYOUT_SIDE_RIGHT);

// Horizontal alignment
const FXuint LAYOUT_HORZ_MASK=(LAYOUT_LEFT|LAYOUT_RIGHT|LAYOUT_CENTER_X|LAYOUT_FIX_X);

// Vertical alignment
const FXuint LAYOUT_VERT_MASK=(LAYOUT_TOP|LAYOUT_BOTTOM|LAYOUT_CENTER_Y|LAYOUT_FIX_Y);

// Width
const FXuint LAYOUT_WIDTH_MASK=(LAYOUT_MIN_WIDTH|LAYOUT_FIX_WIDTH|LAYOUT_FILL_X);

// Height
const FXuint LAYOUT_HEIGHT_MASK=(LAYOUT_MIN_HEIGHT|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_Y);


// Make some windows
ButtonWindow::ButtonWindow(FXApp* a):FXMainWindow(a,"Layout Test",NULL,NULL,DECOR_ALL,0,0,900,900){

  // Tooltip
  new FXToolTip(getApp());

  nextButtonNum=1;
  lastButton=NULL;
  layoutHints=0;

  // Controls on right
  controls=new FXVerticalFrame(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y);

  // Separator
  new FXVerticalSeparator(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y|SEPARATOR_GROOVE);

  // Contents
  contents=new FXPacker(this,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  contents->setBackColor(FXRGB(192,128,128));

  new FXLabel(controls,"Widget Layout Styles\tLayout flags for added buttons",NULL,LAYOUT_CENTER_X);
  new FXHorizontalSeparator(controls);

  new FXLabel(controls,"Packing Side:",NULL,LAYOUT_CENTER_X);
  new FXRadioButton(controls,"LAYOUT_SIDE_LEFT",this,ID_LAYOUT_SIDE_LEFT);
  new FXRadioButton(controls,"LAYOUT_SIDE_RIGHT",this,ID_LAYOUT_SIDE_RIGHT);
  new FXRadioButton(controls,"LAYOUT_SIDE_TOP",this,ID_LAYOUT_SIDE_TOP);
  new FXRadioButton(controls,"LAYOUT_SIDE_BOTTOM",this,ID_LAYOUT_SIDE_BOTTOM);

  new FXLabel(controls,"Horizontal alignment:",NULL,LAYOUT_CENTER_X);
  new FXRadioButton(controls,"LAYOUT_LEFT",this,ID_LAYOUT_LEFT);
  new FXRadioButton(controls,"LAYOUT_RIGHT",this,ID_LAYOUT_RIGHT);
  new FXRadioButton(controls,"LAYOUT_CENTER_X",this,ID_LAYOUT_CENTER_X);
  FXComposite *xposcontrols=new FXHorizontalFrame(controls,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
    new FXRadioButton(xposcontrols,"LAYOUT_FIX_X:",this,ID_LAYOUT_FIX_X,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y);
    FXSpinner *xspin=new FXSpinner(xposcontrols,3,this,ID_FIX_X_VAL,FRAME_SUNKEN|FRAME_THICK|SPIN_NOMAX|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
    xspin->setIncrement(10);
    xspin->setRange(0,10000);

  new FXLabel(controls,"Vertical alignment:",NULL,LAYOUT_CENTER_X);
  new FXRadioButton(controls,"LAYOUT_TOP",this,ID_LAYOUT_TOP);
  new FXRadioButton(controls,"LAYOUT_BOTTOM",this,ID_LAYOUT_BOTTOM);
  new FXRadioButton(controls,"LAYOUT_CENTER_Y",this,ID_LAYOUT_CENTER_Y);
  FXComposite *yposcontrols=new FXHorizontalFrame(controls,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
    new FXRadioButton(yposcontrols,"LAYOUT_FIX_Y:",this,ID_LAYOUT_FIX_Y,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y);
    FXSpinner *yspin=new FXSpinner(yposcontrols,3,this,ID_FIX_Y_VAL,FRAME_SUNKEN|FRAME_THICK|SPIN_NOMAX|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
    yspin->setIncrement(10);
    yspin->setRange(0,10000);

  new FXLabel(controls,"Width control:",NULL,LAYOUT_CENTER_X);
  new FXRadioButton(controls,"LAYOUT_MIN_WIDTH",this,ID_LAYOUT_MIN_WIDTH);
  new FXRadioButton(controls,"LAYOUT_FILL_X",this,ID_LAYOUT_FILL_X);
  FXComposite *wcontrols=new FXHorizontalFrame(controls,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
    new FXRadioButton(wcontrols,"LAYOUT_FIX_WIDTH:",this,ID_LAYOUT_FIX_WIDTH,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y);
    FXSpinner *wspin=new FXSpinner(wcontrols,3,this,ID_FIX_W_VAL,FRAME_SUNKEN|FRAME_THICK|SPIN_NOMAX|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
    wspin->setIncrement(10);
    wspin->setRange(0,10000);

  new FXLabel(controls,"Height control:",NULL,LAYOUT_CENTER_X);
  new FXRadioButton(controls,"LAYOUT_MIN_HEIGHT",this,ID_LAYOUT_MIN_HEIGHT);
  new FXRadioButton(controls,"LAYOUT_FILL_Y",this,ID_LAYOUT_FILL_Y);
  FXComposite *hcontrols=new FXHorizontalFrame(controls,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
    new FXRadioButton(hcontrols,"LAYOUT_FIX_HEIGHT:",this,ID_LAYOUT_FIX_HEIGHT,ICON_BEFORE_TEXT|LAYOUT_CENTER_Y);
    FXSpinner *hspin=new FXSpinner(hcontrols,3,this,ID_FIX_H_VAL,FRAME_SUNKEN|FRAME_THICK|SPIN_NOMAX|LAYOUT_CENTER_Y|LAYOUT_RIGHT);
    hspin->setIncrement(10);
    hspin->setRange(0,10000);

  new FXLabel(controls,"Matrix flags:",NULL,LAYOUT_CENTER_X);
  new FXCheckButton(controls,"LAYOUT_FILL_COLUMN",this,ID_LAYOUT_FILL_COLUMN);
  new FXCheckButton(controls,"LAYOUT_FILL_ROW",this,ID_LAYOUT_FILL_ROW);


  new FXHorizontalSeparator(controls);
  new FXLabel(controls,"Container Packing Styles\tPacking flags for container",NULL,LAYOUT_CENTER_X);

  FXComposite *ctrlsB = new FXHorizontalFrame(controls,LAYOUT_FILL);
  FXGroupBox *group=new FXGroupBox(ctrlsB,"Container Type",GROUPBOX_TITLE_CENTER|FRAME_RIDGE|LAYOUT_FILL_X);
  new FXRadioButton(group,"FXComposite\tMake left pane an FXComposite",this,ID_CONTAINER_COMPOSITE);
  new FXRadioButton(group,"FXHorizonalFrame\tMake left pane an FXHorizontalFrame",this,ID_CONTAINER_HORIZONTAL_FRAME);
  new FXRadioButton(group,"FXVerticalFrame\tMake left pane an FXVerticalFrame",this,ID_CONTAINER_VERTICAL_FRAME);
  new FXRadioButton(group,"FXPacker\tMake left pane an FXPacker",this,ID_CONTAINER_PACKER);
  new FXRadioButton(group,"FXMatrix\tMake left pane an FXMatrix",this,ID_CONTAINER_MATRIX);

  FXComposite *checksBR=new FXVerticalFrame(ctrlsB,LAYOUT_SIDE_RIGHT|LAYOUT_FILL);
  new FXCheckButton(checksBR,"PACK_UNIFORM_HEIGHT", this, ID_PACK_UNIFORM_HEIGHT);
  new FXCheckButton(checksBR,"PACK_UNIFORM_WIDTH", this, ID_PACK_UNIFORM_WIDTH);
  new FXRadioButton(checksBR,"MATRIX_BY_ROWS", this, ID_MATRIX_BY_ROWS);
  new FXRadioButton(checksBR,"MATRIX_BY_COLUMNS", this, ID_MATRIX_BY_COLUMNS);
  FXComposite *hf = new FXHorizontalFrame(checksBR,LAYOUT_FILL_X);
  new FXLabel(hf, "Matrix Rows/cols:");
  new FXSpinner(hf, 4, this, ID_MATRIX_ROWS_COLS,FRAME_SUNKEN|FRAME_THICK|SPIN_NOMAX|LAYOUT_CENTER_Y);

  FXComposite *bframe = new FXHorizontalFrame(controls,LAYOUT_BOTTOM|LAYOUT_FILL_X);
  new FXButton(bframe,"&Add\tAdd a button",NULL,this,ID_ADD_BUTTON,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X);
  new FXButton(bframe,"&Clear\tRemove all buttons",NULL,this,ID_CLEAR_BUTTON,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X);
  new FXButton(bframe,"&Quit\tQuit program",NULL,getApp(),FXApp::ID_QUIT,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_BOTTOM);
  }


// Start
void ButtonWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }


// Change side of item inside packer
long ButtonWindow::onCmdLayoutSide(FXObject*,FXSelector sel,void*){
  if(lastButton){
    layoutHints=lastButton->getLayoutHints()&~LAYOUT_SIDE_MASK;
    switch(FXSELID(sel)){
      case ID_LAYOUT_SIDE_TOP:    layoutHints|=LAYOUT_SIDE_TOP; break;
      case ID_LAYOUT_SIDE_BOTTOM: layoutHints|=LAYOUT_SIDE_BOTTOM; break;
      case ID_LAYOUT_SIDE_LEFT:   layoutHints|=LAYOUT_SIDE_LEFT; break;
      case ID_LAYOUT_SIDE_RIGHT:  layoutHints|=LAYOUT_SIDE_RIGHT; break;
      }
    lastButton->setLayoutHints(layoutHints);
    }
  return 1;
  }


// Update side of item inside packer
long ButtonWindow::onUpdLayoutSide(FXObject* sender,FXSelector sel,void*){
  if(lastButton){
    FXuint hints=lastButton->getLayoutHints()&LAYOUT_SIDE_MASK;
    FXint equal=FALSE;
    switch(FXSELID(sel)){
      case ID_LAYOUT_SIDE_TOP:    equal=(hints==LAYOUT_SIDE_TOP); break;
      case ID_LAYOUT_SIDE_BOTTOM: equal=(hints==LAYOUT_SIDE_BOTTOM); break;
      case ID_LAYOUT_SIDE_LEFT:   equal=(hints==LAYOUT_SIDE_LEFT); break;
      case ID_LAYOUT_SIDE_RIGHT:  equal=(hints==LAYOUT_SIDE_RIGHT); break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }

// Change horizontal alignment
long ButtonWindow::onCmdLayoutHorz(FXObject*,FXSelector sel,void*){
  if(lastButton){
    layoutHints=lastButton->getLayoutHints()&~LAYOUT_HORZ_MASK;
    switch(FXSELID(sel)){
      case ID_LAYOUT_LEFT:     layoutHints|=LAYOUT_LEFT; break;
      case ID_LAYOUT_RIGHT:    layoutHints|=LAYOUT_RIGHT; break;
      case ID_LAYOUT_CENTER_X: layoutHints|=LAYOUT_CENTER_X; break;
      case ID_LAYOUT_FIX_X:    layoutHints|=LAYOUT_FIX_X; break;
      }
    lastButton->setLayoutHints(layoutHints);
    }
  return 1;
  }


// Update horizontal alignment
long ButtonWindow::onUpdLayoutHorz(FXObject* sender,FXSelector sel,void*){
  if(lastButton){
    FXuint hints=lastButton->getLayoutHints()&LAYOUT_HORZ_MASK;
    FXint equal=FALSE;
    switch(FXSELID(sel)){
      case ID_LAYOUT_LEFT:     equal=(hints==LAYOUT_LEFT); break;
      case ID_LAYOUT_RIGHT:    equal=(hints==LAYOUT_RIGHT); break;
      case ID_LAYOUT_CENTER_X: equal=(hints==LAYOUT_CENTER_X); break;
      case ID_LAYOUT_FIX_X:    equal=(hints==LAYOUT_FIX_X); break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change vertical alignment
long ButtonWindow::onCmdLayoutVert(FXObject*,FXSelector sel,void*){
  if(lastButton){
    layoutHints=lastButton->getLayoutHints()&~LAYOUT_VERT_MASK;
    switch(FXSELID(sel)){
      case ID_LAYOUT_TOP:      layoutHints|=LAYOUT_TOP; break;
      case ID_LAYOUT_BOTTOM:   layoutHints|=LAYOUT_BOTTOM; break;
      case ID_LAYOUT_CENTER_Y: layoutHints|=LAYOUT_CENTER_Y; break;
      case ID_LAYOUT_FIX_Y:    layoutHints|=LAYOUT_FIX_Y; break;
      }
    lastButton->setLayoutHints(layoutHints);
    }
  return 1;
  }


// Update vertical alignment
long ButtonWindow::onUpdLayoutVert(FXObject* sender,FXSelector sel,void*){
  if(lastButton){
    FXuint hints=lastButton->getLayoutHints()&LAYOUT_VERT_MASK;
    FXint equal=FALSE;
    switch(FXSELID(sel)){
      case ID_LAYOUT_TOP:      equal=(hints==LAYOUT_TOP); break;
      case ID_LAYOUT_BOTTOM:   equal=(hints==LAYOUT_BOTTOM); break;
      case ID_LAYOUT_CENTER_Y: equal=(hints==LAYOUT_CENTER_Y); break;
      case ID_LAYOUT_FIX_Y:    equal=(hints==LAYOUT_FIX_Y); break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change width
long ButtonWindow::onCmdLayoutWidth(FXObject*,FXSelector sel,void*){
  if(lastButton){
    layoutHints=lastButton->getLayoutHints()&~LAYOUT_WIDTH_MASK;
    switch(FXSELID(sel)){
      case ID_LAYOUT_MIN_WIDTH: layoutHints|=LAYOUT_MIN_WIDTH; break;
      case ID_LAYOUT_FIX_WIDTH: layoutHints|=LAYOUT_FIX_WIDTH; break;
      case ID_LAYOUT_FILL_X:    layoutHints|=LAYOUT_FILL_X; break;
      }
    lastButton->setLayoutHints(layoutHints);
    }
  return 1;
  }


// Update width
long ButtonWindow::onUpdLayoutWidth(FXObject* sender,FXSelector sel,void*){
  if(lastButton){
    FXuint hints=lastButton->getLayoutHints()&LAYOUT_WIDTH_MASK;
    FXint equal=FALSE;
    switch(FXSELID(sel)){
      case ID_LAYOUT_MIN_WIDTH: equal=(hints==LAYOUT_MIN_WIDTH); break;
      case ID_LAYOUT_FIX_WIDTH: equal=(hints==LAYOUT_FIX_WIDTH); break;
      case ID_LAYOUT_FILL_X:    equal=(hints==LAYOUT_FILL_X); break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }

// Change height
long ButtonWindow::onCmdLayoutHeight(FXObject*,FXSelector sel,void*){
  if(lastButton){
    layoutHints=lastButton->getLayoutHints()&~LAYOUT_HEIGHT_MASK;
    switch(FXSELID(sel)){
      case ID_LAYOUT_MIN_HEIGHT: layoutHints|=LAYOUT_MIN_HEIGHT; break;
      case ID_LAYOUT_FIX_HEIGHT: layoutHints|=LAYOUT_FIX_HEIGHT; break;
      case ID_LAYOUT_FILL_Y:     layoutHints|=LAYOUT_FILL_Y; break;
      }
    lastButton->setLayoutHints(layoutHints);
    }
  return 1;
  }


// Update height
long ButtonWindow::onUpdLayoutHeight(FXObject* sender,FXSelector sel,void*){
  if(lastButton){
    FXuint hints=lastButton->getLayoutHints()&LAYOUT_HEIGHT_MASK;
    FXint equal=FALSE;
    switch(FXSELID(sel)){
      case ID_LAYOUT_MIN_HEIGHT: equal=(hints==LAYOUT_MIN_HEIGHT); break;
      case ID_LAYOUT_FIX_HEIGHT: equal=(hints==LAYOUT_FIX_HEIGHT); break;
      case ID_LAYOUT_FILL_Y:     equal=(hints==LAYOUT_FILL_Y); break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }

// Change matrix layout
long ButtonWindow::onCmdLayoutMatrix(FXObject*,FXSelector sel,void*){
  if(lastButton){
    layoutHints=lastButton->getLayoutHints();
    switch(FXSELID(sel)){
      case ID_LAYOUT_FILL_COLUMN: layoutHints^=LAYOUT_FILL_COLUMN; break;
      case ID_LAYOUT_FILL_ROW:  layoutHints^=LAYOUT_FILL_ROW; break;
      }
    lastButton->setLayoutHints(layoutHints);
    }
  return 1;
  }


// Update matrix layout
long ButtonWindow::onUpdLayoutMatrix(FXObject* sender,FXSelector sel,void*){
  if(lastButton){
    FXuint hints=lastButton->getLayoutHints();
    FXuint equal=0;
    switch(FXSELID(sel)){
      case ID_LAYOUT_FILL_COLUMN: equal=(hints&LAYOUT_FILL_COLUMN)!=0; break;
      case ID_LAYOUT_FILL_ROW:    equal=(hints&LAYOUT_FILL_ROW)!=0; break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change packing
long ButtonWindow::onCmdPackHints(FXObject*,FXSelector sel,void*){
  FXPacker *packer=dynamic_cast<FXPacker*>(contents);
  if(packer){
    FXuint hints=packer->getPackingHints();
    switch(FXSELID(sel)){
      case ID_PACK_UNIFORM_HEIGHT: hints^=PACK_UNIFORM_HEIGHT; break;
      case ID_PACK_UNIFORM_WIDTH:  hints^=PACK_UNIFORM_WIDTH; break;
      }
    packer->setPackingHints(hints);
    }
  return 1;
  }


// Update packing
long ButtonWindow::onUpdPackHints(FXObject* sender,FXSelector sel,void*){
  FXPacker *packer=dynamic_cast<FXPacker*>(contents);
  if(packer){
    FXuint hints=packer->getPackingHints()&(PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH);
    FXuint equal=0;
    switch(FXSELID(sel)){
      case ID_PACK_UNIFORM_HEIGHT: equal=(hints&PACK_UNIFORM_HEIGHT)!=0; break;
      case ID_PACK_UNIFORM_WIDTH:  equal=(hints&PACK_UNIFORM_WIDTH)!=0; break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change Matrix mode
long ButtonWindow::onCmdMatrixMode(FXObject*,FXSelector sel,void*){
  FXMatrix *matrix=dynamic_cast<FXMatrix*>(contents);
  if(matrix){
    FXuint style=matrix->getMatrixStyle();
    switch(FXSELID(sel)){
      case ID_MATRIX_BY_COLUMNS: style|=MATRIX_BY_COLUMNS; break;
      case ID_MATRIX_BY_ROWS:    style&=~MATRIX_BY_COLUMNS; break;
      }
    matrix->setMatrixStyle(style);
    }
  return 1;
  }


// Update Matrix mode
long ButtonWindow::onUpdMatrixMode(FXObject* sender,FXSelector sel,void*){
  FXMatrix *matrix=dynamic_cast<FXMatrix*>(contents);
  if(matrix){
    FXuint style=matrix->getMatrixStyle();
    FXuint equal=0;
    switch(FXSELID(sel)){
      case ID_MATRIX_BY_COLUMNS: equal=(style&MATRIX_BY_COLUMNS)!=0; break;
      case ID_MATRIX_BY_ROWS:    equal=(style&MATRIX_BY_COLUMNS)==0; break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&equal);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change container type, reparent existing widgets into it
long ButtonWindow::onCmdContainerType(FXObject*,FXSelector sel,void*){
  FXComposite *old=contents;
  switch(FXSELID(sel)){
    case ID_CONTAINER_COMPOSITE:
      contents=new FXComposite(this,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
      contents->create();
      break;
    case ID_CONTAINER_HORIZONTAL_FRAME:
      contents=new FXHorizontalFrame(this,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH,0,0,0,0, 0,0,0,0, 0,0);
      contents->create();
      break;
    case ID_CONTAINER_VERTICAL_FRAME:
      contents=new FXVerticalFrame(this,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
      contents->create();
      break;
    case ID_CONTAINER_PACKER:
      contents=new FXPacker(this,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
      contents->create();
      break;
    case ID_CONTAINER_MATRIX:
      contents=new FXMatrix(this,3,LAYOUT_SIDE_LEFT|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
      contents->create();
      break;
    }
  while(old->getFirst()){
    old->getFirst()->reparent(contents);
    }
  contents->setBackColor(FXRGB(192,128,128));
  contents->recalc();
  delete old;
  return 1;
  }


// Update container type
long ButtonWindow::onUpdContainerType(FXObject* sender,FXSelector sel,void*){
  const FXMetaClass *mc=contents->getMetaClass();
  switch(FXSELID(sel)){
    case ID_CONTAINER_COMPOSITE:
      sender->handle(this,(mc==&FXComposite::metaClass)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
      break;
    case ID_CONTAINER_HORIZONTAL_FRAME:
      sender->handle(this,(mc==&FXHorizontalFrame::metaClass)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
      break;
    case ID_CONTAINER_VERTICAL_FRAME:
      sender->handle(this,(mc==&FXVerticalFrame::metaClass)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
      break;
    case ID_CONTAINER_PACKER:
      sender->handle(this,(mc==&FXPacker::metaClass)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
      break;
    case ID_CONTAINER_MATRIX:
      sender->handle(this,(mc==&FXMatrix::metaClass)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
      break;
    }
  return 1;
  }


// Change fixed stuff
long ButtonWindow::onChgFixVal(FXObject*sender,FXSelector sel,void*){
  FXint val=0;
  if(lastButton){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&val);
    switch(FXSELID(sel)){
      case ID_FIX_X_VAL: lastButton->setX(val); break;
      case ID_FIX_Y_VAL: lastButton->setY(val); break;
      case ID_FIX_W_VAL: lastButton->setWidth(val); break;
      case ID_FIX_H_VAL: lastButton->setHeight(val); break;
      }
    contents->recalc();
    }
  return 1;
  }


// Update fixed stuff
long ButtonWindow::onUpdFixVal(FXObject*sender,FXSelector sel,void*){
  if(lastButton){
    FXint val=0;
    switch(FXSELID(sel)){
      case ID_FIX_X_VAL: val=lastButton->getX(); break;
      case ID_FIX_Y_VAL: val=lastButton->getY(); break;
      case ID_FIX_W_VAL: val=lastButton->getWidth(); break;
      case ID_FIX_H_VAL: val=lastButton->getHeight(); break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&val);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change number of rows or columns
long ButtonWindow::onChgMatVal(FXObject*sender,FXSelector,void*){
  FXMatrix *matrix=dynamic_cast<FXMatrix*>(contents);
  if(matrix){
    FXint val;
    sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&val);
    if(matrix->getMatrixStyle()&MATRIX_BY_COLUMNS){
      matrix->setNumColumns(val);
      }
    else{
      matrix->setNumRows(val);
      }
    }
  return 1;
  }


// Update number of rows or columns
long ButtonWindow::onUpdMatVal(FXObject* sender,FXSelector,void*){
  FXMatrix *matrix=dynamic_cast<FXMatrix*>(contents);
  if(matrix){
    FXint val;
    if(matrix->getMatrixStyle()&MATRIX_BY_COLUMNS){
      val=matrix->getNumColumns();
      }
    else{
      val=matrix->getNumRows();
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),&val);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Clear buttons from the container
long ButtonWindow::onCmdClearButton(FXObject*,FXSelector,void*){
  while(contents->getFirst()){
    delete contents->getFirst();
    }
  contents->recalc();
  lastButton=NULL;
  nextButtonNum=0;
  return 1;
  }


long ButtonWindow::onUpdClearButton(FXObject* sender,FXSelector,void*){
  sender->handle(this,contents->getFirst()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }

// Add a button to the container
long ButtonWindow::onCmdAddButton(FXObject*,FXSelector,void*){
  static const FXchar *titles[]={"Button %d\tClick to make active", "Wide Button %d\tClick to make active", "Tall\nButton %d\tClick to make active"};
  FXString string;
  string.format(titles[nextButtonNum%3],nextButtonNum);
  lastButton=new FXButton(contents,string,NULL,this,ID_BUTTON,FRAME_THICK|FRAME_RAISED|layoutHints);
  lastButton->create();
  lastButton->recalc();
  nextButtonNum++;
  return 1;
  }


// Make this one the active button
long ButtonWindow::onCmdButton(FXObject* sender,FXSelector,void*){
  lastButton=(FXButton*)sender;
  layoutHints=lastButton->getLayoutHints();
  return 1;
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("Layout","FoxTest");

  // Open display
  application.init(argc,argv);

  // Main window
  new ButtonWindow(&application);

  // Create app
  application.create();

  // Run
  return application.run();
  }


