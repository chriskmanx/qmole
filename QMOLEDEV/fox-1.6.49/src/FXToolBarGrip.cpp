/********************************************************************************
*                                                                               *
*                       T o o l B a r G r i p   W i d g e t                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXToolBarGrip.cpp,v 1.29 2006/01/22 17:58:47 fox Exp $                   *
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
#include "FXToolBar.h"
#include "FXToolBarGrip.h"


/*
  Notes:
  - Tool bar grip is a small grabber contained in the toolbar which lets
    a user move the toolbar around, dock and undock it, and so on.
  - The convention is to let single-rebar tool bar grips rearrange the
    bars in a dock site, and double-rebar grips dock and undock.
    This convention is recommended but not enforced.
*/


// Size
#define GRIP_SINGLE  3          // Single grip for arrangable toolbars
#define GRIP_DOUBLE  7          // Double grip for dockable toolbars

#define JUSTIFY_MASK (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)


using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXToolBarGrip) FXToolBarGripMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXToolBarGrip::onPaint),
  FXMAPFUNC(SEL_ENTER,0,FXToolBarGrip::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXToolBarGrip::onLeave),
  FXMAPFUNC(SEL_MOTION,0,FXToolBarGrip::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXToolBarGrip::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXToolBarGrip::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXToolBarGrip::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXToolBarGrip::onKeyRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXToolBarGrip::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXToolBarGrip::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXToolBarGrip::ID_SETHELPSTRING,FXToolBarGrip::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXToolBarGrip::ID_GETHELPSTRING,FXToolBarGrip::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXToolBarGrip::ID_SETTIPSTRING,FXToolBarGrip::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXToolBarGrip::ID_GETTIPSTRING,FXToolBarGrip::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXToolBarGrip,FXDockHandler,FXToolBarGripMap,ARRAYNUMBER(FXToolBarGripMap))


// Deserialization
FXToolBarGrip::FXToolBarGrip(){
  activeColor=0;
  }


// Construct and init
FXToolBarGrip::FXToolBarGrip(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXDockHandler(p,tgt,sel,opts,x,y,w,h,pl,pr,pt,pb){
  activeColor=FXRGB(0,0,255);
  }



// Get default width
FXint FXToolBarGrip::getDefaultWidth(){
  return padleft+padright+(border<<1)+((options&TOOLBARGRIP_DOUBLE)?GRIP_DOUBLE:GRIP_SINGLE);
  }


// Get default height
FXint FXToolBarGrip::getDefaultHeight(){
  return padtop+padbottom+(border<<1)+((options&TOOLBARGRIP_DOUBLE)?GRIP_DOUBLE:GRIP_SINGLE);
  }


// Can have focus
bool FXToolBarGrip::canFocus() const { return false; }


// Change toolbar orientation
void FXToolBarGrip::setDoubleBar(FXbool dbl){
  FXuint opts=dbl?(options|TOOLBARGRIP_DOUBLE):(options&~TOOLBARGRIP_DOUBLE);
  if(opts!=options){
    options=opts;
    recalc();
    }
  }


// Return TRUE if toolbar grip is displayed as a double bar
FXbool FXToolBarGrip::isDoubleBar() const {
  return (options&TOOLBARGRIP_DOUBLE)!=0;
  }


// Handle repaint
long FXToolBarGrip::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=static_cast<FXEvent*>(ptr);
  FXDCWindow dc(this,event);
  FXint xx,yy,ww,hh;
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));
  ww=width-padleft-padright-(border<<1);
  hh=height-padtop-padbottom-(border<<1);
  if(width>height){
    xx=border+padleft;
    if(options&TOOLBARGRIP_DOUBLE){     // =
      yy=border+padtop+(hh-GRIP_DOUBLE)/2;
      dc.setForeground(hiliteColor);
      dc.fillRectangle(xx,yy,1,2);
      dc.fillRectangle(xx,yy+4,1,2);
      dc.fillRectangle(xx,yy,ww-1,1);
      dc.fillRectangle(xx,yy+4,ww-1,1);
      dc.setForeground(shadowColor);
      dc.fillRectangle(xx+ww-1,yy,1,3);
      dc.fillRectangle(xx+ww-1,yy+4,1,3);
      dc.fillRectangle(xx,yy+2,ww-1,1);
      dc.fillRectangle(xx,yy+6,ww-1,1);
      if(flags&(FLAG_ACTIVE|FLAG_TRYDRAG|FLAG_DODRAG)){
        dc.setForeground(activeColor);
        dc.fillRectangle(xx+1,yy+1,ww-2,1);
        dc.fillRectangle(xx+1,yy+5,ww-2,1);
        }
      }
    else{                               // -
      yy=border+padtop+(hh-GRIP_SINGLE)/2;
      dc.setForeground(hiliteColor);
      dc.fillRectangle(xx,yy,1,2);
      dc.fillRectangle(xx,yy,ww-1,1);
      dc.setForeground(shadowColor);
      dc.fillRectangle(xx+ww-1,yy,1,3);
      dc.fillRectangle(xx,yy+2,ww-1,1);
      if(flags&(FLAG_ACTIVE|FLAG_TRYDRAG|FLAG_DODRAG)){
        dc.setForeground(activeColor);
        dc.fillRectangle(xx+1,yy+1,ww-2,1);
        }
      }
    }
  else{
    yy=border+padtop;
    if(options&TOOLBARGRIP_DOUBLE){     // ||
      xx=border+padleft+(ww-GRIP_DOUBLE)/2;
      dc.setForeground(hiliteColor);
      dc.fillRectangle(xx,yy,2,1);
      dc.fillRectangle(xx+4,yy,2,1);
      dc.fillRectangle(xx,yy,1,hh-1);
      dc.fillRectangle(xx+4,yy,1,hh-1);
      dc.setForeground(shadowColor);
      dc.fillRectangle(xx,yy+hh-1,3,1);
      dc.fillRectangle(xx+4,yy+hh-1,3,1);
      dc.fillRectangle(xx+2,yy,1,hh-1);
      dc.fillRectangle(xx+6,yy,1,hh-1);
      if(flags&(FLAG_ACTIVE|FLAG_TRYDRAG|FLAG_DODRAG)){
        dc.setForeground(activeColor);
        dc.fillRectangle(xx+1,yy+1,1,hh-2);
        dc.fillRectangle(xx+5,yy+1,1,hh-2);
        }
      }
    else{                               // |
      xx=border+padleft+(ww-GRIP_SINGLE)/2;
      dc.setForeground(hiliteColor);
      dc.fillRectangle(xx,yy,2,1);
      dc.fillRectangle(xx,yy,1,hh-1);
      dc.setForeground(shadowColor);
      dc.fillRectangle(xx,yy+hh-1,3,1);
      dc.fillRectangle(xx+2,yy,1,hh-1);
      if(flags&(FLAG_ACTIVE|FLAG_TRYDRAG|FLAG_DODRAG)){
        dc.setForeground(activeColor);
        dc.fillRectangle(xx+1,yy+1,1,hh-2);
        }
      }
    }
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Entered button
long FXToolBarGrip::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXDockHandler::onEnter(sender,sel,ptr);
  if(isEnabled()){ flags|=FLAG_ACTIVE; update(); }
  return 1;
  }


// Leave button
long FXToolBarGrip::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXDockHandler::onLeave(sender,sel,ptr);
  if(isEnabled()){ flags&=~FLAG_ACTIVE; update(); }
  return 1;
  }



// Set active color
void FXToolBarGrip::setActiveColor(FXColor clr){
  if(clr!=activeColor){
    activeColor=clr;
    update();
    }
  }


// Save data
void FXToolBarGrip::save(FXStream& store) const {
  FXDockHandler::save(store);
  store << activeColor;
  }


// Load data
void FXToolBarGrip::load(FXStream& store){
  FXDockHandler::load(store);
  store >> activeColor;
  }


}
