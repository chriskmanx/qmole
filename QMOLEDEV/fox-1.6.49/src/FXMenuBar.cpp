/********************************************************************************
*                                                                               *
*                         M e n u   B a r   W i d g e t                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXMenuBar.cpp,v 1.26 2006/01/22 17:58:35 fox Exp $                       *
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
#include "FXButton.h"
#include "FXMenuBar.h"


/*
  Notes:
  - Hittin Alt- key only should attract focus to the first item in the menubar.
  - If width of menu gets too small, expand the height to make it multiple rows.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuBar) FXMenuBarMap[]={
  FXMAPFUNC(SEL_ENTER,0,FXMenuBar::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXMenuBar::onLeave),
  FXMAPFUNC(SEL_MOTION,0,FXMenuBar::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXMenuBar::onButtonPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXMenuBar::onButtonRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXMenuBar::onButtonPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXMenuBar::onButtonRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXMenuBar::onButtonPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXMenuBar::onButtonRelease),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FXMenuBar::onFocusRight),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FXMenuBar::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXMenuBar::onDefault),
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXMenuBar::onDefault),
  FXMAPFUNC(SEL_FOCUS_UP,0,FXMenuBar::onDefault),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXMenuBar::onDefault),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNPOST,FXMenuBar::onCmdUnpost),
  };


// Object implementation
FXIMPLEMENT(FXMenuBar,FXToolBar,FXMenuBarMap,ARRAYNUMBER(FXMenuBarMap))


// Make a floatable menubar
FXMenuBar::FXMenuBar(FXComposite* p,FXComposite* q,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXToolBar(p,q,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  flags|=FLAG_ENABLED;
  dragCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  }


// Make a non-floatable menubar
FXMenuBar::FXMenuBar(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXToolBar(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  flags|=FLAG_ENABLED;
  dragCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  }


// Focus moved to right
long FXMenuBar::onFocusRight(FXObject*,FXSelector,void* ptr){
  FXWindow *child;
  if(getFocus()){
    child=getFocus()->getNext();
    while(child){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      child=child->getNext();
      }
    child=getFirst();
    while(child){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      child=child->getNext();
      }
    }
  return 0;
  }


// Focus moved to left
long FXMenuBar::onFocusLeft(FXObject*,FXSelector,void* ptr){
  FXWindow *child;
  if(getFocus()){
    child=getFocus()->getPrev();
    while(child){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      child=child->getPrev();
      }
    child=getLast();
    while(child){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      child=child->getPrev();
      }
    }
  return 0;
  }


// Enter:- when inside the popup, all is normal!
long FXMenuBar::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  FXint px, py;
  FXToolBar::onEnter(sender,sel,ptr);
  if(!getFocus() || !getFocus()->isActive()) return 1;
  if(((FXEvent*)ptr)->code==CROSSINGNORMAL){
    translateCoordinatesTo(px,py,getParent(),ev->win_x,ev->win_y);
    if(contains(px,py) && grabbed()) ungrab();
    //if(grabbed()) ungrab();
    }
  return 1;
  }


// Leave:- when outside the popup, a click will hide the popup!
long FXMenuBar::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  FXint px,py;
  FXToolBar::onLeave(sender,sel,ptr);
  if(!getFocus() || !getFocus()->isActive()) return 1;
  if(((FXEvent*)ptr)->code==CROSSINGNORMAL){
    translateCoordinatesTo(px,py,getParent(),ev->win_x,ev->win_y);
    if(!contains(px,py) && !grabbed()) grab();
//#ifndef WIN32
//    if(!grabbed()) grab();
//#endif
    }
  return 1;
  }


// We're considered inside the menu bar when either
// in the bar or in any active menus
bool FXMenuBar::contains(FXint parentx,FXint parenty) const {
  FXint x,y;
  if(FXComposite::contains(parentx,parenty)) return true;
  if(getFocus()){
    getParent()->translateCoordinatesTo(x,y,this,parentx,parenty);
    if(getFocus()->contains(x,y)) return true;
    }
  return false;
  }


// Moved while outside
// We need to do this because the definition of ``inside'' means
// that we're inside even though possibly we're not in THIS window!!!
long FXMenuBar::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  FXint px,py;
  if(!getFocus() || !getFocus()->isActive()) return 0;
  translateCoordinatesTo(px,py,getParent(),ev->win_x,ev->win_y);
  if(contains(px,py)){
    if(grabbed()) ungrab();
    }
  else{
    if(!grabbed()) grab();
    }
  return 0;
  }


// Button pressed
long FXMenuBar::onButtonPress(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onButtonPress %p\n",getClassName(),this));
  handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  return 1;
  }


// Button released
long FXMenuBar::onButtonRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  FXTRACE((200,"%s::onButtonRelease %p\n",getClassName(),this));
  if(ev->moved){ handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL); }
  return 1;
  }


// Unpost the menu
long FXMenuBar::onCmdUnpost(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onCmdUnpost %p\n",getClassName(),this));
  if(getFocus()) getFocus()->killFocus();
  //killFocus();
  return 1;
  }

}
