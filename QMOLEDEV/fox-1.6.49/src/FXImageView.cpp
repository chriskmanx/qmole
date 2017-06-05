/********************************************************************************
*                                                                               *
*                       I m a g e   V i e w   W i d g e t                       *
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
* $Id: FXImageView.cpp,v 1.38 2006/01/22 17:58:32 fox Exp $                     *
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
#include "FXDCWindow.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXComposite.h"
#include "FXCanvas.h"
#include "FXButton.h"
#include "FXScrollBar.h"
#include "FXImageView.h"


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
FXDEFMAP(FXImageView) FXImageViewMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXImageView::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXImageView::onMotion),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXImageView::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXImageView::onRightBtnRelease),
  };


// Object implementation
FXIMPLEMENT(FXImageView,FXScrollArea,FXImageViewMap,ARRAYNUMBER(FXImageViewMap))


// Deserialization
FXImageView::FXImageView(){
  flags|=FLAG_ENABLED;
  image=NULL;
  grabx=0;
  graby=0;
  }


// Construct and init
FXImageView::FXImageView(FXComposite* p,FXImage* img,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  image=img;
  }


// Create window
void FXImageView::create(){
  FXScrollArea::create();
  if(image) image->create();
  }


// Detach window
void FXImageView::detach(){
  FXScrollArea::detach();
  if(image) image->detach();
  }


// Can have focus
bool FXImageView::canFocus() const { return true; }


// Determine content width of scroll area
FXint FXImageView::getContentWidth(){
  return image ? image->getWidth() : 1;
  }


// Determine content height of scroll area
FXint FXImageView::getContentHeight(){
  return image ? image->getHeight() : 1;
  }


// Recalculate layout
void FXImageView::layout(){

  // Layout scroll bars and viewport
  FXScrollArea::layout();

  update();
  flags&=~FLAG_DIRTY;
  }


// Draw visible part of image
long FXImageView::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint xx,yy,ww,hh;
  FXint xl,xr,yt,yb;
  if(image){
    ww=image->getWidth();
    hh=image->getHeight();
    xx=pos_x;
    yy=pos_y;
    if(ww<viewport_w){
      if(options&IMAGEVIEW_LEFT) xx=0;
      else if(options&IMAGEVIEW_RIGHT) xx=viewport_w-ww;
      else xx=(viewport_w-ww)/2;
      }
    if(hh<viewport_h){
      if(options&IMAGEVIEW_TOP) yy=0;
      else if(options&IMAGEVIEW_BOTTOM) yy=viewport_h-hh;
      else yy=(viewport_h-hh)/2;
      }
    dc.drawImage(image,xx,yy);
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
long FXImageView::onRightBtnPress(FXObject*,FXSelector,void* ptr){
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
long FXImageView::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
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
long FXImageView::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(flags&FLAG_SCROLLING){
    setPosition(ev->win_x-grabx,ev->win_y-graby);
    return 1;
    }
  return 0;
  }


// Change image
void FXImageView::setImage(FXImage* img){
  image=img;
  recalc();
  update();
  }


// Set the current alignment.
void FXImageView::setAlignment(FXuint mode){
  FXuint opts=(options&~(IMAGEVIEW_LEFT|IMAGEVIEW_RIGHT|IMAGEVIEW_TOP|IMAGEVIEW_BOTTOM)) | (mode&(IMAGEVIEW_LEFT|IMAGEVIEW_RIGHT|IMAGEVIEW_TOP|IMAGEVIEW_BOTTOM));
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get the current alignment.
FXuint FXImageView::getAlignment() const {
  return (options&(IMAGEVIEW_LEFT|IMAGEVIEW_RIGHT|IMAGEVIEW_TOP|IMAGEVIEW_BOTTOM));
  }


// Save object to stream
void FXImageView::save(FXStream& store) const {
  FXScrollArea::save(store);
  store << image;
  }


// Load object from stream
void FXImageView::load(FXStream& store){
  FXScrollArea::load(store);
  store >> image;
  }


// Destroy
FXImageView::~FXImageView(){
  image=(FXImage*)-1L;
  }

}
