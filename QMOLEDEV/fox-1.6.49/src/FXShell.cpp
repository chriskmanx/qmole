/********************************************************************************
*                                                                               *
*                     S h e l l   W i n d o w   O b j e c t                     *
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
* $Id: FXShell.cpp,v 1.81 2006/01/22 17:58:41 fox Exp $                         *
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
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXShell.h"


/*
  Notes:
  - FXShell handles keys to implement focus change messages.
  - The initial size should probably be determined not in create(), but in show().
  - Note that Shell is base class for transient ``popup'' override-redirect windows.
    For top level windows, we should use size hints rather than force the size.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXShell) FXShellMap[]={
  FXMAPFUNC(SEL_CONFIGURE,0,FXShell::onConfigure),
  FXMAPFUNC(SEL_KEYPRESS,0,FXShell::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXShell::onKeyRelease),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXShell::onFocusNext),
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXShell::onFocusPrev),
  FXMAPFUNC(SEL_CHORE,FXShell::ID_LAYOUT,FXShell::onLayout),
  };


// Object implementation
FXIMPLEMENT(FXShell,FXComposite,FXShellMap,ARRAYNUMBER(FXShellMap))


// Create a toplevel window
FXShell::FXShell(FXApp* a,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXComposite(a,NULL,opts,x,y,w,h){
  }


// Create a toplevel window
FXShell::FXShell(FXWindow* own,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXComposite(own->getApp(),own,opts,x,y,w,h){
  }


// Create X window
void FXShell::create(){
  FXint w,h;

  // Create this widget and all of its children
  FXComposite::create();

  // Adjust size if necessary
  w=(1<width) ? width : getDefaultWidth();
  h=(1<height) ? height : getDefaultHeight();

  // Resize this widget
  resize(w,h);
  }


// Schedule layout to be peformed during idle time
void FXShell::recalc(){
  getApp()->removeChore(this,ID_LAYOUT);
  getApp()->addChore(this,ID_LAYOUT);
  flags|=FLAG_DIRTY;
  }


// Shell into the focus chain
void FXShell::setFocus(){
  flags|=FLAG_HELP;
  }


// Shell out of focus chain
void FXShell::killFocus(){
  if(getFocus()) getFocus()->killFocus();
  flags&=~FLAG_HELP;
  flags|=FLAG_UPDATE;
  }


// Perform layout; return 0 because no GUI update is needed
long FXShell::onLayout(FXObject*,FXSelector,void*){
  layout();
  return 0;
  }


// Handle configure notify
long FXShell::onConfigure(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXComposite::onConfigure(sender,sel,ptr);
  xpos=ev->rect.x;
  ypos=ev->rect.y;
  if((ev->rect.w!=width) || (ev->rect.h!=height)){
    width=ev->rect.w;               // Record new size
    height=ev->rect.h;
    // Delayed layout optimization. The delayed layout optimization
    // currently only works on UNIX.  On Windows, the program enters
    // a modal loop during a window-resize operation.  During this
    // modal loop, which is somewhere inside WIN32 code, we are completely
    // deaf to other event sources such as timers, chores, file i/o, and
    // are unable to perform idle processing.  So the chore we would set
    // in recalc() would never fire until we're all done with the resizing.
    // We'd love to have a fix for this, but it seems difficult because of
    // the need to pass "non-client" events over to the DefWindowProc...
#ifndef WIN32
    recalc();           // On UNIX, we process idle messages during a resize
#else
    layout();           // On Windows, we are in a modal loop and we have to force it
#endif
    }
  return 1;
  }


// Focus moved to next
long FXShell::onFocusNext(FXObject* sender,FXSelector,void* ptr){
  FXWindow *child;
  if(getFocus()){
    child=getFocus()->getNext();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(sender,FXSEL(SEL_FOCUS_NEXT,0),ptr)) return 1;
        }
      child=child->getNext();
      }
    getFocus()->killFocus();
    }
  child=getFirst();
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(sender,FXSEL(SEL_FOCUS_NEXT,0),ptr)) return 1;
      }
    child=child->getNext();
    }
  return 0;
  }


// Focus moved to previous
long FXShell::onFocusPrev(FXObject* sender,FXSelector,void* ptr){
  FXWindow *child;
  if(getFocus()){
    child=getFocus()->getPrev();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(sender,FXSEL(SEL_FOCUS_PREV,0),ptr)) return 1;
        }
      child=child->getPrev();
      }
    getFocus()->killFocus();
    }
  child=getLast();
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(sender,FXSEL(SEL_FOCUS_PREV,0),ptr)) return 1;
      }
    child=child->getPrev();
    }
  return 0;
  }


// Keyboard press
long FXShell::onKeyPress(FXObject* sender,FXSelector sel,void* ptr){

  // Try to handle normally
  if(FXComposite::onKeyPress(sender,sel,ptr)) return 1;

  // If not handled yet, try the default button
  if(((FXEvent*)ptr)->code==KEY_Return || ((FXEvent*)ptr)->code==KEY_KP_Enter){

    // Find default widget
    FXWindow *def=findDefault(this);

    // Handle default key
    if(def && def->handle(sender,sel,ptr)) return 1;
    }
  return 0;
  }


// Keyboard release
long FXShell::onKeyRelease(FXObject* sender,FXSelector sel,void* ptr){

  // Try to handle normally
  if(FXComposite::onKeyRelease(sender,sel,ptr)) return 1;

  // If not handled yet, try the default button
  if(((FXEvent*)ptr)->code==KEY_Return || ((FXEvent*)ptr)->code==KEY_KP_Enter){

    // Find default widget
    FXWindow *def=findDefault(this);

    // Handle default key
    if(def && def->handle(sender,sel,ptr)) return 1;
    }
  return 0;
  }


// Destruct
FXShell::~FXShell(){
  getApp()->removeChore(this,ID_LAYOUT);
  }

}


