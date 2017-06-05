/********************************************************************************
*                                                                               *
*                       D o c k H a n d l e r   W i d g e t                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXDockHandler.cpp,v 1.8 2006/01/22 17:58:23 fox Exp $                    *
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
#include "FXApp.h"
#include "FXDockHandler.h"


/*
  Notes:
  - On MS-Windows, this works just fine, without fanfare.
  - On X11, some quaintness in the X-Server causes havoc if reparent()
    is called on a window while it was grabbed.
  - Not to be deterred, we implement here the following workaround:

    1) We create a temporary dummy window, just 1x1 pixels in size, in the
       upper left corner of the screen [yes, that was not a dead pixel
       after all!].

    2) We add this to the hash table, so now events from the "true" window
       as well those from the dummy window are dispatched to the toolbar grip.

    3) We temporarily replace the xid of the true window with the dummy one,
       then invoke grab() to grab the mouse, then restore the original xid.

    4) Now you can wave your mouse around and dock or undock toolbars.

    5) When we're done, we replace the xid again with the dummy window,
       call ungrab(), then restore the original xid again.

    6) Then, delete the dummy window.

  - The only downside of this method is that the win_x and win_y member
    data in FXEvent is unreliable; fortunately, the standard toolbar docking
    algorithms do not use these members.
  - Of course, we'd rather not have to do all this; so don't hesitate
    to inform us if you have a better way!

*/




using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXDockHandler) FXDockHandlerMap[]={
  FXMAPFUNC(SEL_MOTION,0,FXDockHandler::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXDockHandler::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXDockHandler::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXDockHandler::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXDockHandler::onKeyRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXDockHandler::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXDockHandler::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXDockHandler::ID_SETHELPSTRING,FXDockHandler::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXDockHandler::ID_GETHELPSTRING,FXDockHandler::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXDockHandler::ID_SETTIPSTRING,FXDockHandler::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXDockHandler::ID_GETTIPSTRING,FXDockHandler::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT_ABSTRACT(FXDockHandler,FXFrame,FXDockHandlerMap,ARRAYNUMBER(FXDockHandlerMap))


// Deserialization
FXDockHandler::FXDockHandler(){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  xxx=0;
  }


// Construct and init
FXDockHandler::FXDockHandler(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_SHOWN|FLAG_ENABLED;
  dragCursor=getApp()->getDefaultCursor(DEF_MOVE_CURSOR);
  target=tgt;
  message=sel;
  xxx=0;
  }


// Can have focus
bool FXDockHandler::canFocus() const { return true; }


// Moved
long FXDockHandler::onMotion(FXObject*,FXSelector,void* ptr){
  if(flags&FLAG_DODRAG){
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }
  if((flags&FLAG_TRYDRAG) && ((FXEvent*)ptr)->moved){
    if(handle(this,FXSEL(SEL_BEGINDRAG,0),ptr)) flags|=FLAG_DODRAG;
    flags&=~FLAG_TRYDRAG;
    return 1;
    }
  return 0;
  }


// Pressed LEFT button
long FXDockHandler::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    flags=(flags&~(FLAG_UPDATE|FLAG_DODRAG))|FLAG_TRYDRAG;
#ifndef WIN32
    Display *display=(Display*)getApp()->getDisplay();
    const unsigned long mask=CWBackPixmap|CWWinGravity|CWBitGravity|CWBorderPixel|CWOverrideRedirect|CWSaveUnder|CWEventMask|CWDontPropagate|CWColormap|CWCursor;
    XSetWindowAttributes wattr;
    FXID tempxid=xid;
    wattr.background_pixmap=None;
    wattr.background_pixel=0;
    wattr.border_pixmap=None;
    wattr.border_pixel=0;
    wattr.bit_gravity=ForgetGravity;
    wattr.win_gravity=NorthWestGravity;
    wattr.backing_store=NotUseful;
    wattr.backing_planes=0;
    wattr.backing_pixel=0;
    wattr.save_under=FALSE;
    wattr.event_mask=ButtonPressMask|ButtonReleaseMask|PointerMotionMask|KeyPressMask|KeyReleaseMask | FocusChangeMask|StructureNotifyMask | StructureNotifyMask|ExposureMask|PropertyChangeMask|EnterWindowMask|LeaveWindowMask;
    wattr.do_not_propagate_mask=KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|PointerMotionMask|ButtonMotionMask;
    wattr.override_redirect=TRUE;
    wattr.colormap=DefaultColormap(display,DefaultScreen(display));
    wattr.cursor=None;
    xxx=XCreateWindow(display,RootWindow(display,DefaultScreen(display)),0,0,1,1,0,DefaultDepth(display,DefaultScreen(display)),InputOutput,DefaultVisual(display,DefaultScreen(display)),mask,&wattr);
    getApp()->hash.insert((void*)xxx,this);
    XMapWindow(display,xxx);
    xid=xxx;
    grab();
    xid=tempxid;
#else
    grab();
#endif
    update();
    }
  return 1;
  }


// Released LEFT button
long FXDockHandler::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_ENDDRAG,0),ptr);}
    flags=(flags&~(FLAG_TRYDRAG|FLAG_DODRAG))|FLAG_UPDATE;
#ifndef WIN32
    Display *display=(Display*)getApp()->getDisplay();
    FXID tempxid=xid;
    xid=xxx;
    ungrab();
    xid=tempxid;
    getApp()->hash.remove((void*)xxx);
    XDestroyWindow(display,xxx);
    xxx=0;
#else
    ungrab();
#endif
    update();
    }
  return 1;
  }


// Key Press
long FXDockHandler::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=static_cast<FXEvent*>(ptr);
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_Control_L || event->code==KEY_Control_R){
      if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXDockHandler::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=static_cast<FXEvent*>(ptr);
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_Control_L || event->code==KEY_Control_R){
      if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
      return 1;
      }
    }
  return 0;
  }


// We were asked about tip text
long FXDockHandler::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXDockHandler::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Set tip using a message
long FXDockHandler::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXDockHandler::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// Set help using a message
long FXDockHandler::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXDockHandler::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Save data
void FXDockHandler::save(FXStream& store) const {
  FXFrame::save(store);
  store << tip;
  store << help;
  }


// Load data
void FXDockHandler::load(FXStream& store){
  FXFrame::load(store);
  store >> tip;
  store >> help;
  }


}
