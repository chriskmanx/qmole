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
* $Id: FXImageFrame.cpp,v 1.14 2006/01/22 17:58:32 fox Exp $                    *
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
#include "FXImageFrame.h"


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
FXDEFMAP(FXImageFrame) FXImageFrameMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXImageFrame::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXImageFrame,FXFrame,FXImageFrameMap,ARRAYNUMBER(FXImageFrameMap))


// Deserialization
FXImageFrame::FXImageFrame(){
  image=NULL;
  }


// Construct it
FXImageFrame::FXImageFrame(FXComposite* p,FXImage *img,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  image=img;
  }


// Create it all
void FXImageFrame::create(){
  FXFrame::create();
  if(image) image->create();
  }


// Get default width
FXint FXImageFrame::getDefaultWidth(){
  register FXint w=0;
  if(image) w=image->getWidth();
  return w+padleft+padright+(border<<1);
  }


// Get default height
FXint FXImageFrame::getDefaultHeight(){
  register FXint h=0;
  if(image) h=image->getHeight();
  return h+padtop+padbottom+(border<<1);
  }


// Draw the image
long FXImageFrame::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint imgx,imgy,imgw,imgh;
  dc.setForeground(backColor);
  if(image){
    imgw=image->getWidth();
    imgh=image->getHeight();
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
    dc.drawImage(image,imgx,imgy);
    }
  else{
    dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));
    }
  drawFrame(dc,0,0,width,height);
  return 1;
  }



// Change image
void FXImageFrame::setImage(FXImage* img){
  image=img;
  recalc();
  update();
  }


// Set text justify style
void FXImageFrame::setJustify(FXuint style){
  FXuint opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get text justify style
FXuint FXImageFrame::getJustify() const {
  return (options&JUSTIFY_MASK);
  }



// Save data
void FXImageFrame::save(FXStream& store) const {
  FXFrame::save(store);
  store << image;
  }


// Load data
void FXImageFrame::load(FXStream& store){
  FXFrame::load(store);
  store >> image;
  }


// Destructor
FXImageFrame::~FXImageFrame(){
  image=(FXImage*)-1L;
  }

}
