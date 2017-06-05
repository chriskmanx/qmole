/********************************************************************************
*                                                                               *
*                C o m p o s i t e   W i n d o w   O b j e c t                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXComposite.cpp,v 1.54.2.2 2007/04/29 14:31:43 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXComposite.h"


/*
  Notes:
  - Rather a slim class.
  - Focus should be assigned to a window via SEL_FOCUSELF message.
    Composite widgets won't have focus so SEL_FOCUSELF should return 0
    and do nothing.
  - Maybe add flag to exempt a widget from maxChildWidth() and/or maxChildHeight()
    so that things like separators can stay small when everything else gets
    as big as the biggest child.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXComposite) FXCompositeMap[]={
  FXMAPFUNC(SEL_KEYPRESS,0,FXComposite::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXComposite::onKeyRelease),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXComposite::onFocusNext),
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXComposite::onFocusPrev),
  FXMAPFUNC(SEL_FOCUS_UP,0,FXComposite::onFocusPrev),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXComposite::onFocusNext),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FXComposite::onFocusPrev),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FXComposite::onFocusNext),
  FXMAPFUNC(SEL_COMMAND,FXComposite::ID_UPDATE,FXComposite::onCmdUpdate),
  };


// Object implementation
FXIMPLEMENT(FXComposite,FXWindow,FXCompositeMap,ARRAYNUMBER(FXCompositeMap))


// Only used for Root Window
FXComposite::FXComposite(FXApp* a,FXVisual *vis):FXWindow(a,vis){
  }


// Only used for Shell Window
FXComposite::FXComposite(FXApp* a,FXWindow* own,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXWindow(a,own,opts,x,y,w,h){
  }


// Create empty composite window
FXComposite::FXComposite(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXWindow(p,opts,x,y,w,h){
  }


// Is widget a composite
bool FXComposite::isComposite() const {
  return true;
  }


// Create window
void FXComposite::create(){
  FXWindow::create();
  for(FXWindow *c=getFirst(); c; c=c->getNext()) c->create();
  }


// Detach window
void FXComposite::detach(){
  for(FXWindow *c=getFirst(); c; c=c->getNext()) c->detach();
  FXWindow::detach();
  }


// Destroy window
void FXComposite::destroy(){
  for(FXWindow *c=getFirst(); c; c=c->getNext()) c->destroy();
  FXWindow::destroy();
  }


// Get width
FXint FXComposite::getDefaultWidth(){
  register FXWindow *child;
  FXint t,w=0;
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      t=child->getX()+child->getWidth();
      if(w<t) w=t;
      }
    }
  return w;
  }


// Get height
FXint FXComposite::getDefaultHeight(){
  register FXWindow *child;
  FXint t,h=0;
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      t=child->getY()+child->getHeight();
      if(h<t) h=t;
      }
    }
  return h;
  }


// Get maximum child width
FXint FXComposite::maxChildWidth() const {
  register FXWindow* child;
  register FXuint hints;
  register FXint t,m;
  for(m=0,child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_WIDTH) t=child->getWidth();
      else t=child->getDefaultWidth();
      if(m<t) m=t;
      }
    }
  return m;
  }


// Get maximum child height
FXint FXComposite::maxChildHeight() const {
  register FXWindow* child;
  register FXuint hints;
  register FXint t,m;
  for(m=0,child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_HEIGHT) t=child->getHeight();
      else t=child->getDefaultHeight();
      if(m<t) m=t;
      }
    }
  return m;
  }


// Just tell server where the windows are!
void FXComposite::layout(){
  register FXWindow *child;
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      child->position(child->getX(),child->getY(),child->getWidth(),child->getHeight());
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Update all subwindows
long FXComposite::onCmdUpdate(FXObject* sender,FXSelector,void* ptr){
  register FXWindow *child;
  update();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()) child->handle(sender,FXSEL(SEL_COMMAND,ID_UPDATE),ptr);
    }
  return 1;
  }


// Focus moved to next
long FXComposite::onFocusNext(FXObject*,FXSelector sel,void* ptr){
  FXWindow *child;
  if(getFocus())
    child=getFocus()->getNext();
  else
    child=getFirst();
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,sel,ptr)) return 1;
      }
    child=child->getNext();
    }
  return 0;
  }


// Focus moved to previous
long FXComposite::onFocusPrev(FXObject*,FXSelector sel,void* ptr){
  FXWindow *child;
  if(getFocus())
    child=getFocus()->getPrev();
  else
    child=getLast();
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,sel,ptr)) return 1;
      }
    child=child->getPrev();
    }
  return 0;
  }


// Keyboard press
long FXComposite::onKeyPress(FXObject* sender,FXSelector sel,void* ptr){
  register FXEvent* event=(FXEvent*)ptr;

  FXTRACE((200,"%p->%s::onKeyPress keysym=0x%04x state=%04x\n",this,getClassName(),((FXEvent*)ptr)->code,((FXEvent*)ptr)->state));

  // Bounce to focus widget
  if(getFocus() && getFocus()->handle(sender,sel,ptr)) return 1;

  // Try target first
  if(isEnabled() && target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;

  // Check the accelerators
  if(getAccelTable() && getAccelTable()->handle(this,sel,ptr)) return 1;

  // Otherwise, perform the default keyboard processing
  switch(MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK))){
    case KEY_Tab:
    case KEY_Next:
      return handle(this,FXSEL(SEL_FOCUS_NEXT,0),ptr);
    case KEY_Prior:
    case KEY_ISO_Left_Tab:
    case MKUINT(KEY_ISO_Left_Tab,SHIFTMASK): 
    case MKUINT(KEY_Tab,SHIFTMASK):    
      return handle(this,FXSEL(SEL_FOCUS_PREV,0),ptr);
    case KEY_Up:
    case KEY_KP_Up:
      return handle(this,FXSEL(SEL_FOCUS_UP,0),ptr);
    case KEY_Down:
    case KEY_KP_Down:
      return handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr);
    case KEY_Left:
    case KEY_KP_Left:
      return handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr);
    case KEY_Right:
    case KEY_KP_Right:
      return handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr);
    }
  return 0;
  }


// Keyboard release
long FXComposite::onKeyRelease(FXObject* sender,FXSelector sel,void* ptr){

  FXTRACE((200,"%p->%s::onKeyRelease keysym=0x%04x state=%04x\n",this,getClassName(),((FXEvent*)ptr)->code,((FXEvent*)ptr)->state));

  // Bounce to focus widget
  if(getFocus() && getFocus()->handle(sender,sel,ptr)) return 1;

  // Try target first
  if(isEnabled() && target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;

  // Check the accelerators
  if(getAccelTable() && getAccelTable()->handle(this,sel,ptr)) return 1;

  return 0;
  }


// Dispose of all the children
FXComposite::~FXComposite(){
  while(getFirst()){ delete getFirst(); }
  }

}
