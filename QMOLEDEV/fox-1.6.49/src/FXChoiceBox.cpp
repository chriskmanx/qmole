/********************************************************************************
*                                                                               *
*                            C h o i c e   B o x                                *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXChoiceBox.cpp,v 1.14 2006/01/22 17:58:20 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObjectList.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXIcon.h"
#include "FXSeparator.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXHorizontalFrame.h"
#include "FXVerticalFrame.h"
#include "FXScrollBar.h"
#include "FXList.h"
#include "FXChoiceBox.h"
#include "icons.h"

/*
  Notes:
  - Maybe allow setting initial value which selected.
  - Maybe have multiple choice capability.
  - Maybe have list of icons for each list item.
*/

// Padding for message box buttons
#define HORZ_PAD 30
#define VERT_PAD 2

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXChoiceBox) FXChoiceBoxMap[]={
  FXMAPFUNC(SEL_COMMAND,FXChoiceBox::ID_CANCEL,FXChoiceBox::onCmdCancel),
  FXMAPFUNC(SEL_COMMAND,FXChoiceBox::ID_ACCEPT,FXChoiceBox::onCmdClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,FXChoiceBox::ID_CLICKED,FXChoiceBox::onCmdClicked),
  };



// Object implementation
FXIMPLEMENT(FXChoiceBox,FXDialogBox,FXChoiceBoxMap,ARRAYNUMBER(FXChoiceBoxMap))


// Construct choice box with given caption, icon, message text, and with choices from array of strings
FXChoiceBox::FXChoiceBox(FXWindow* owner,const FXString& caption,const FXString& text,FXIcon* icon,const FXchar** choices,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,caption,opts|DECOR_TITLE|DECOR_BORDER,x,y,w,h,10,10,10,10, 10,10){
  register FXint n;
  initialize(text,icon);
  n=list->fillItems(choices);
  list->setNumVisible(FXMIN(n,5));
  }


// Construct choice box with given caption, icon, message text, and with choices from newline separated strings
FXChoiceBox::FXChoiceBox(FXWindow* owner,const FXString& caption,const FXString& text,FXIcon* icon,const FXString& choices,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,caption,opts|DECOR_TITLE|DECOR_BORDER,x,y,w,h,10,10,10,10, 10,10){
  register FXint n;
  initialize(text,icon);
  n=list->fillItems(choices);
  list->setNumVisible(FXMIN(n,5));
  }


// Construct free floating choice box with given caption, icon, message text, and with choices from array of strings
FXChoiceBox::FXChoiceBox(FXApp* a,const FXString& caption,const FXString& text,FXIcon* icon,const FXchar** choices,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(a,caption,opts|DECOR_TITLE|DECOR_BORDER,x,y,w,h,10,10,10,10, 10,10){
  register FXint n;
  initialize(text,icon);
  n=list->fillItems(choices);
  list->setNumVisible(FXMIN(n,5));
  }


// Construct free floating choice box with given caption, icon, message text, and with choices from newline separated strings
FXChoiceBox::FXChoiceBox(FXApp* a,const FXString& caption,const FXString& text,FXIcon* icon,const FXString& choices,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(a,caption,opts|DECOR_TITLE|DECOR_BORDER,x,y,w,h,10,10,10,10, 10,10){
  register FXint n;
  initialize(text,icon);
  n=list->fillItems(choices);
  list->setNumVisible(FXMIN(n,5));
  }


// Build contents
void FXChoiceBox::initialize(const FXString& text,FXIcon* icon){
  FXHorizontalFrame* buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,0,0,0,0);
  new FXButton(buttons,tr("&OK"),NULL,this,ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  new FXButton(buttons,tr("&Cancel"),NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  FXHorizontalFrame* toppart=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0, 10,10);
  new FXLabel(toppart,FXString::null,icon,ICON_BEFORE_TEXT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_Y|LAYOUT_FILL_X);
  new FXLabel(toppart,text,NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X);
  FXHorizontalFrame* midpart=new FXHorizontalFrame(this,FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 10,10);
  list=new FXList(midpart,this,ID_CLICKED,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLING_OFF);
  }


// Close dialog when double-clicked in list or hit accept
long FXChoiceBox::onCmdClicked(FXObject*,FXSelector,void*){
  getApp()->stopModal(this,list->getCurrentItem());
  hide();
  return 1;
  }


// Close dialog with a cancel
long FXChoiceBox::onCmdCancel(FXObject*,FXSelector,void*){
  getApp()->stopModal(this,-1);
  hide();
  return 1;
  }


// Save object to stream
void FXChoiceBox::save(FXStream& store) const {
  FXDialogBox::save(store);
  store << list;
  }


// Load object from stream
void FXChoiceBox::load(FXStream& store){
  FXDialogBox::load(store);
  store >> list;
  }


// Destroy choice box
FXChoiceBox::~FXChoiceBox(){
  list=(FXList*)-1L;
  }


/*******************************************************************************/


// Show a modal choice dialog
FXint FXChoiceBox::ask(FXWindow* owner,FXuint opts,const FXString& caption,const FXString& text,FXIcon* icon,const FXchar** choices){
  FXChoiceBox box(owner,caption,text,icon,choices,opts);
  return box.execute(PLACEMENT_OWNER);
  }


// Show a modal choice dialog
FXint FXChoiceBox::ask(FXWindow* owner,FXuint opts,const FXString& caption,const FXString& text,FXIcon* icon,const FXString& choices){
  FXChoiceBox box(owner,caption,text,icon,choices,opts);
  return box.execute(PLACEMENT_OWNER);
  }


// Show a modal choice dialog, in free floating window
FXint FXChoiceBox::ask(FXApp* app,FXuint opts,const FXString& caption,const FXString& text,FXIcon* icon,const FXchar** choices){
  FXChoiceBox box(app,caption,text,icon,choices,opts);
  return box.execute(PLACEMENT_SCREEN);
  }


// Show a modal choice dialog, in free floating window
FXint FXChoiceBox::ask(FXApp* app,FXuint opts,const FXString& caption,const FXString& text,FXIcon* icon,const FXString& choices){
  FXChoiceBox box(app,caption,text,icon,choices,opts);
  return box.execute(PLACEMENT_SCREEN);
  }

}

