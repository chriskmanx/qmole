/********************************************************************************
*                                                                               *
*                    B i t m a p   V i e w   W i d g e t                        *
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
* $Id: FXBitmapView.cpp,v 1.16 2006/01/22 17:58:18 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXThread.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXBitmap.h"
#include "FXIcon.h"
#include "FXComposite.h"
#include "FXCanvas.h"
#include "FXButton.h"
#include "FXScrollBar.h"
#include "FXBitmapView.h"


/*
  Notes:
  - Should implement DND drags/drops, cut/paste
  - Right-mouse scroll.
*/

#define MOUSE_NONE    0                // None in effect
#define MOUSE_SCROLL  1                // Scrolling mode

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXBitmapView) FXBitmapViewMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXBitmapView::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXBitmapView::onMotion),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXBitmapView::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXBitmapView::onRightBtnRelease),
  };


// Object implementation
FXIMPLEMENT(FXBitmapView,FXScrollArea,FXBitmapViewMap,ARRAYNUMBER(FXBitmapViewMap))


// Deserialization
FXBitmapView::FXBitmapView(){
  flags|=FLAG_ENABLED;
  bitmap=NULL;
  onColor=0;
  offColor=0;
  grabx=0;
  graby=0;
  }


// Construct and init
FXBitmapView::FXBitmapView(FXComposite* p,FXBitmap* bmp,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  bitmap=bmp;
  onColor=FXRGB(0,0,0);
  offColor=backColor;
  grabx=0;
  graby=0;
  }


// Create window
void FXBitmapView::create(){
  FXScrollArea::create();
  if(bitmap) bitmap->create();
  }


// Detach window
void FXBitmapView::detach(){
  FXScrollArea::detach();
  if(bitmap) bitmap->detach();
  }


// Can have focus
bool FXBitmapView::canFocus() const { return true; }


// Determine content width of scroll area
FXint FXBitmapView::getContentWidth(){
  return bitmap ? bitmap->getWidth() : 1;
  }


// Determine content height of scroll area
FXint FXBitmapView::getContentHeight(){
  return bitmap ? bitmap->getHeight() : 1;
  }


// Recalculate layout
void FXBitmapView::layout(){

  // Layout scroll bars and viewport
  FXScrollArea::layout();

  update();
  flags&=~FLAG_DIRTY;
  }


// Draw visible part of bitmap
long FXBitmapView::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint xx,yy,ww,hh;
  FXint xl,xr,yt,yb;
  if(bitmap){
    ww=bitmap->getWidth();
    hh=bitmap->getHeight();
    xx=pos_x;
    yy=pos_y;
    if(ww<viewport_w){
      if(options&BITMAPVIEW_LEFT) xx=0;
      else if(options&BITMAPVIEW_RIGHT) xx=viewport_w-ww;
      else xx=(viewport_w-ww)/2;
      }
    if(hh<viewport_h){
      if(options&BITMAPVIEW_TOP) yy=0;
      else if(options&BITMAPVIEW_BOTTOM) yy=viewport_h-hh;
      else yy=(viewport_h-hh)/2;
      }
    dc.setForeground(onColor);
    dc.setBackground(offColor);
    dc.drawBitmap(bitmap,xx,yy);
    dc.setForeground(backColor);
    xl=xx; xr=xx+ww;
    yt=yy; yb=yy+hh;
    if(xl<0) xl=0; if(xr>viewport_w) xr=viewport_w;
    if(yt<0) yt=0; if(yb>viewport_h) yb=viewport_h;
    dc.fillRectangle(0,0,xr,yt);
    dc.fillRectangle(0,yt,xl,viewport_h-yt);
    dc.fillRectangle(xr,0,viewport_w-xr,yb);
    dc.fillRectangle(xl,yb,viewport_w-xl,viewport_h-yb);
    }
  else{
    dc.setForeground(backColor);
    dc.fillRectangle(0,0,width,height);
    }
  return 1;
  }


// Pressed right button
long FXBitmapView::onRightBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    flags|=FLAG_PRESSED|FLAG_SCROLLING;
    grabx=ev->win_x-pos_x;
    graby=ev->win_y-pos_y;
    return 1;
    }
  return 0;
  }


// Released right button
long FXBitmapView::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    ungrab();
    flags&=~(FLAG_PRESSED|FLAG_SCROLLING);
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONRELEASE,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }


// Handle real or simulated mouse motion
long FXBitmapView::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(flags&FLAG_SCROLLING){
    setPosition(ev->win_x-grabx,ev->win_y-graby);
    return 1;
    }
  return 0;
  }


// Change bitmap
void FXBitmapView::setBitmap(FXBitmap* bmp){
  bitmap=bmp;
  recalc();
  update();
  }


// Set on color
void FXBitmapView::setOnColor(FXColor clr){
  if(clr!=onColor){
    onColor=clr;
    update();
    }
  }


// Set off color
void FXBitmapView::setOffColor(FXColor clr){
  if(clr!=offColor){
    offColor=clr;
    update();
    }
  }


// Set the current alignment.
void FXBitmapView::setAlignment(FXuint mode){
  FXuint opts=(options&~(BITMAPVIEW_LEFT|BITMAPVIEW_RIGHT|BITMAPVIEW_TOP|BITMAPVIEW_BOTTOM)) | (mode&(BITMAPVIEW_LEFT|BITMAPVIEW_RIGHT|BITMAPVIEW_TOP|BITMAPVIEW_BOTTOM));
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get the current alignment.
FXuint FXBitmapView::getAlignment() const {
  return (options&(BITMAPVIEW_LEFT|BITMAPVIEW_RIGHT|BITMAPVIEW_TOP|BITMAPVIEW_BOTTOM));
  }


// Save object to stream
void FXBitmapView::save(FXStream& store) const {
  FXScrollArea::save(store);
  store << bitmap;
  }


// Load object from stream
void FXBitmapView::load(FXStream& store){
  FXScrollArea::load(store);
  store >> bitmap;
  }


// Destroy
FXBitmapView::~FXBitmapView(){
  bitmap=(FXBitmap*)-1L;
  }

}
