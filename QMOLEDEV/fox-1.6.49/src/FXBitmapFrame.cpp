/********************************************************************************
*                                                                               *
*                       I m a g e   F r a m e   W i d g e t                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by H. J. Daniel III. All Rights Reserved.             *
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
* $Id: FXBitmapFrame.cpp,v 1.9 2006/01/22 17:58:18 fox Exp $                    *
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
#include "FXBitmap.h"
#include "FXBitmapFrame.h"


/*
  Notes:
  - Hacked around so as to support padding also.
  - Fixed layout to center image if frame is larger.
  - Fixed serialization also.
  - Now supports various justification modes
*/


#define JUSTIFY_MASK    (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXBitmapFrame) FXBitmapFrameMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXBitmapFrame::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXBitmapFrame,FXFrame,FXBitmapFrameMap,ARRAYNUMBER(FXBitmapFrameMap))


// Deserialization
FXBitmapFrame::FXBitmapFrame(){
  bitmap=NULL;
  onColor=0;
  offColor=0;
  }


// Construct it
FXBitmapFrame::FXBitmapFrame(FXComposite* p,FXBitmap *bmp,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  bitmap=bmp;
  onColor=FXRGB(0,0,0);
  offColor=backColor;
  }


// Create it all
void FXBitmapFrame::create(){
  FXFrame::create();
  if(bitmap) bitmap->create();
  }


// Get default width
FXint FXBitmapFrame::getDefaultWidth(){
  register FXint w=0;
  if(bitmap) w=bitmap->getWidth();
  return w+padleft+padright+(border<<1);
  }


// Get default height
FXint FXBitmapFrame::getDefaultHeight(){
  register FXint h=0;
  if(bitmap) h=bitmap->getHeight();
  return h+padtop+padbottom+(border<<1);
  }


// Draw the image
long FXBitmapFrame::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint imgx,imgy,imgw,imgh;
  dc.setForeground(backColor);
  if(bitmap){
    imgw=bitmap->getWidth();
    imgh=bitmap->getHeight();
    if(options&JUSTIFY_LEFT) imgx=padleft+border;
    else if(options&JUSTIFY_RIGHT) imgx=width-padright-border-imgw;
    else imgx=border+padleft+(width-padleft-padright-(border<<1)-imgw)/2;
    if(options&JUSTIFY_TOP) imgy=padtop+border;
    else if(options&JUSTIFY_BOTTOM) imgy=height-padbottom-border-imgh;
    else imgy=border+padtop+(height-padbottom-padtop-(border<<1)-imgh)/2;
    dc.fillRectangle(border,border,imgx-border,height-(border<<1));
    dc.fillRectangle(imgx+imgw,border,width-border-imgx-imgw,height-(border<<1));
    dc.fillRectangle(imgx,border,imgw,imgy-border);
    dc.fillRectangle(imgx,imgy+imgh,imgw,height-border-imgy-imgh);
    dc.setForeground(onColor);
    dc.setBackground(offColor);
    dc.drawBitmap(bitmap,imgx,imgy);
    }
  else{
    dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));
    }
  drawFrame(dc,0,0,width,height);
  return 1;
  }



// Change image
void FXBitmapFrame::setBitmap(FXBitmap* bmp){
  bitmap=bmp;
  recalc();
  update();
  }


// Set on color
void FXBitmapFrame::setOnColor(FXColor clr){
  if(clr!=onColor){
    onColor=clr;
    update();
    }
  }


// Set off color
void FXBitmapFrame::setOffColor(FXColor clr){
  if(clr!=offColor){
    offColor=clr;
    update();
    }
  }



// Set text justify style
void FXBitmapFrame::setJustify(FXuint style){
  FXuint opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get text justify style
FXuint FXBitmapFrame::getJustify() const {
  return (options&JUSTIFY_MASK);
  }



// Save data
void FXBitmapFrame::save(FXStream& store) const {
  FXFrame::save(store);
  store << bitmap;
  }


// Load data
void FXBitmapFrame::load(FXStream& store){
  FXFrame::load(store);
  store >> bitmap;
  }


// Destructor
FXBitmapFrame::~FXBitmapFrame(){
  bitmap=(FXBitmap*)-1L;
  }

}
