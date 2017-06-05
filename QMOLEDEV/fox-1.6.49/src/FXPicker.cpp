/********************************************************************************
*                                                                               *
*                          P i c k e r   B u t t o n                            *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXPicker.cpp,v 1.21.2.2 2008/09/22 20:53:57 fox Exp $                        *
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
#include "FXDCWindow.h"
#include "FXIcon.h"
#include "FXPicker.h"


/*
  Notes:
*/

using namespace FX;


/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXPicker) FXPickerMap[]={
  FXMAPFUNC(SEL_MOTION,0,FXPicker::onMotion),
  FXMAPFUNC(SEL_ENTER,0,FXPicker::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXPicker::onLeave),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXPicker::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXPicker::onLeftBtnRelease),
  };


// Object implementation
FXIMPLEMENT(FXPicker,FXButton,FXPickerMap,ARRAYNUMBER(FXPickerMap))



// Construct and init
FXPicker::FXPicker(FXComposite* p,const FXString& text,FXIcon* ic,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXButton(p,text,ic,tgt,sel,opts,x,y,w,h,pl,pr,pt,pb){
  dragCursor=getApp()->getDefaultCursor(DEF_CROSSHAIR_CURSOR);
  }


// Entered button
long FXPicker::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled() && (options&BUTTON_TOOLBAR)) update();
  return 1;
  }


// Left button
long FXPicker::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled() && (options&BUTTON_TOOLBAR)) update();
  return 1;
  }


// Mouse moved
long FXPicker::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(state==STATE_DOWN){
    FXPoint point(event->root_x,event->root_y);
    if(target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&point); }
    return 1;
    }
  return 0;
  }


// Pressed mouse button
long FXPicker::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled()){
    if(state==STATE_UP){
      grab();
      setState(STATE_DOWN);
      flags&=~FLAG_UPDATE;
      }
    else{
      setState(STATE_UP);
      }
    return 1;
    }
  return 0;
  }


// Released mouse button
long FXPicker::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(state==STATE_UP){
      ungrab();
      flags|=FLAG_UPDATE;
      FXPoint point(event->root_x,event->root_y);
      if(target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)&point); }
      }
    return 1;
    }
  return 0;
  }

}

