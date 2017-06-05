/********************************************************************************
*                                                                               *
*                 S h u t t e r   C o n t a i n e r   W i d g e t               *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Charles W. Warren.   All Rights Reserved.          *
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
* $Id: FXShutter.cpp,v 1.43 2006/01/22 17:58:41 fox Exp $                       *
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
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXIcon.h"
#include "FXFrame.h"
#include "FXPacker.h"
#include "FXVerticalFrame.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXScrollBar.h"
#include "FXScrollWindow.h"
#include "FXShutter.h"

/*
  Notes:
  - Works now by means of integers, i.e. setCurrent(0) instead of having
    to hang on to pointers to specific shutter items.
    Advantage: You can make it connect to other widgets.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXShutterItem) FXShutterItemMap[]={
  FXMAPFUNC(SEL_FOCUS_UP,0,FXShutterItem::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXShutterItem::onFocusDown),
  FXMAPFUNC(SEL_COMMAND,FXShutterItem::ID_SHUTTERITEM_BUTTON,FXShutterItem::onCmdButton),
  };


// Object implementation
FXIMPLEMENT(FXShutterItem,FXVerticalFrame,FXShutterItemMap,ARRAYNUMBER(FXShutterItemMap))


FXShutterItem::FXShutterItem(FXShutter* p,const FXString& text,FXIcon* icon,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXVerticalFrame(p,(opts&~(PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH)),x,y,w,h,0,0,0,0,0,0){
  button=new FXButton(this,text,icon,this,FXShutterItem::ID_SHUTTERITEM_BUTTON,FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,0,0,0,0);
  scrollWindow=new FXScrollWindow(this,VSCROLLER_NEVER|HSCROLLER_NEVER|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_LEFT|LAYOUT_TOP,0,0,0,0);
  content=new FXVerticalFrame(scrollWindow,LAYOUT_FILL_X|LAYOUT_FILL_Y|(opts&(PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH)),0,0,0,0,pl,pr,pt,pb,hs,vs);
  content->setBackColor(getApp()->getShadowColor());
  }


// Button Pressed
long FXShutterItem::onCmdButton(FXObject*,FXSelector,void* ptr){
  getParent()->handle(this,FXSEL(SEL_COMMAND,FXShutter::ID_OPEN_SHUTTERITEM),ptr);
  return 1;
  }


// Focus moved up
long FXShutterItem::onFocusUp(FXObject* sender,FXSelector sel,void* ptr){
  return FXVerticalFrame::onFocusPrev(sender,sel,ptr);
  }


// Focus moved down
long FXShutterItem::onFocusDown(FXObject* sender,FXSelector sel,void* ptr){
  return FXVerticalFrame::onFocusNext(sender,sel,ptr);
  }


// Change help text
void FXShutterItem::setHelpText(const FXString& text){
  button->setHelpText(text);
  }


// Get help text
FXString FXShutterItem::getHelpText() const {
  return button->getHelpText();
  }


// Change tip text
void FXShutterItem::setTipText(const FXString& text){
  button->setTipText(text);
  }


// Get tip text
FXString FXShutterItem::getTipText() const {
  return button->getTipText();
  }



// Thrash it
FXShutterItem::~FXShutterItem(){
  button=(FXButton*)-1L;
  scrollWindow=(FXScrollWindow*)-1L;
  content=(FXVerticalFrame*)-1L;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXShutter) FXShutterMap[]={
  FXMAPFUNC(SEL_FOCUS_UP,0,FXShutter::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXShutter::onFocusDown),
  FXMAPFUNCS(SEL_UPDATE,FXShutter::ID_OPEN_FIRST,FXShutter::ID_OPEN_LAST,FXShutter::onUpdOpen),
  FXMAPFUNC(SEL_TIMEOUT,FXShutter::ID_SHUTTER_TIMEOUT,FXShutter::onTimeout),
  FXMAPFUNC(SEL_COMMAND,FXShutter::ID_OPEN_SHUTTERITEM,FXShutter::onOpenItem),
  FXMAPFUNC(SEL_COMMAND,FXShutter::ID_SETVALUE,FXShutter::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXShutter::ID_SETINTVALUE,FXShutter::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXShutter::ID_GETINTVALUE,FXShutter::onCmdGetIntValue),
  FXMAPFUNCS(SEL_COMMAND,FXShutter::ID_OPEN_FIRST,FXShutter::ID_OPEN_LAST,FXShutter::onCmdOpen),
  };


// Object implementation
FXIMPLEMENT(FXShutter,FXVerticalFrame,FXShutterMap,ARRAYNUMBER(FXShutterMap))


// Make shutter
FXShutter::FXShutter(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXVerticalFrame(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  target=tgt;
  message=sel;
  heightIncrement=1;
  closingHeight=0;
  closingHadScrollbar=FALSE;
  current=0;
  closing=-1;
  }


// Focus moved up
long FXShutter::onFocusUp(FXObject* sender,FXSelector sel,void* ptr){
  return FXVerticalFrame::onFocusPrev(sender,sel,ptr);
  }


// Focus moved down
long FXShutter::onFocusDown(FXObject* sender,FXSelector sel,void* ptr){
  return FXVerticalFrame::onFocusNext(sender,sel,ptr);
  }

// Update value from a message
long FXShutter::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrent((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXShutter::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCurrent(*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXShutter::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=current;
  return 1;
  }


// Open item
long FXShutter::onCmdOpen(FXObject*,FXSelector sel,void*){
  setCurrent(FXSELID(sel)-ID_OPEN_FIRST);
  return 1;
  }


// Update the nth button
long FXShutter::onUpdOpen(FXObject* sender,FXSelector sel,void* ptr){
  sender->handle(this,((FXSELID(sel)-ID_OPEN_FIRST)==current) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),ptr);
  return 1;
  }


// The sender of the message is the item to open up
long FXShutter::onOpenItem(FXObject* sender,FXSelector,void*){
  FXint which=indexOfChild((FXWindow*)sender);
  FXuint speed=getApp()->getAnimSpeed();
  FXShutterItem *closingItem;
  if(current==which) which--;     // Clicking on title button of currently active item should close it; "Markus Fleck" <fleck@gnu.org>
  if(0<=which){
    if(speed){
      closing=current;
      heightIncrement=1;
      closingItem=(FXShutterItem*)childAtIndex(closing);
      closingHeight=closingItem->getHeight();
      closingHadScrollbar=closingItem->scrollWindow->verticalScrollBar()->shown();
      getApp()->addTimeout(this,ID_SHUTTER_TIMEOUT,speed);
      }
    current=which;
    recalc();
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)current);
    }
  return 1;
  }


// Shutter Item Animation
long FXShutter::onTimeout(FXObject*,FXSelector,void*){

  // Closing item got deleted
  if(closing<0) return 0;

  // Shrink closing item a bit more
  closingHeight-=heightIncrement;
  heightIncrement+=5;

  // Force layout again
  recalc();

  // Still not fully closed?
  if(closingHeight>0){
    getApp()->addTimeout(this,ID_SHUTTER_TIMEOUT,getApp()->getAnimSpeed());
    return 1;
    }

  // Now fully closed
  closing=-1;

  return 1;
  }


// Layout
void FXShutter::layout(){
  register FXShutterItem* child;
  register FXint index,numchildren;

  numchildren=numChildren();

  // One of the children may have disappeared
  if(current>=numchildren) current=numchildren-1;
  if(current==-1 && numchildren>0) current=0;         // Fix by "Martin Welch" <mwelch@totalise.co.uk>
  if(closing>=numchildren) closing=-1;

  // Force only one of the children to be open
  for(child=(FXShutterItem*)getFirst(),index=0; child; child=(FXShutterItem*)child->getNext(),index++){
    if(child->shown()){
      if(index==current){
        child->setLayoutHints(LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_LEFT|LAYOUT_TOP);
        child->scrollWindow->setScrollStyle((closing>=0) ? (VSCROLLER_NEVER|HSCROLLER_NEVER) : HSCROLLER_NEVER);
        child->scrollWindow->show();
        }
      else if(index==closing){
        child->setLayoutHints(LAYOUT_FILL_X|LAYOUT_FIX_HEIGHT|LAYOUT_LEFT|LAYOUT_TOP);
        child->scrollWindow->setScrollStyle(closingHadScrollbar ? (VSCROLLER_ALWAYS|HSCROLLER_NEVER) : (VSCROLLER_NEVER|HSCROLLER_NEVER));
        child->setHeight(closingHeight);
        }
      else{
        child->setLayoutHints(LAYOUT_FILL_X|LAYOUT_LEFT|LAYOUT_TOP);
        child->scrollWindow->hide();
        }
      }
    }

  // Then layout normally
  FXVerticalFrame::layout();
  flags&=~FLAG_DIRTY;
  }


// Set current subwindow
void FXShutter::setCurrent(FXint panel){
  if(0<=panel && current!=panel){
    current=panel;
    recalc();
    }
  }


// Clean up
FXShutter::~FXShutter() {
  getApp()->removeTimeout(this,ID_SHUTTER_TIMEOUT);
  }

}

