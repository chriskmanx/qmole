/********************************************************************************
*                                                                               *
*                      S e p a r a t o r   W i d g e t s                        *
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
* $Id: FXSeparator.cpp,v 1.28 2006/01/22 17:58:41 fox Exp $                     *
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
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXSeparator.h"



/*
  Notes:
  - When changing icon/font/etc, we should only recalc and update when it's different.
  - When text changes, do we delete the hot key, or parse it from the new label?
  - It makes sense for certain ``passive'' widgets such as labels to have onUpdate;
    for example, to show/hide/whatever based on changing data structures.
  - Why not just have one type of separator, orientation simply being based on the
    widest dimension?
*/

#define SEPARATOR_EXTRA 2

#define SEPARATOR_MASK  (SEPARATOR_NONE|SEPARATOR_GROOVE|SEPARATOR_RIDGE|SEPARATOR_LINE)


using namespace FX;

/*******************************************************************************/


namespace FX {


// Map
FXDEFMAP(FXSeparator) FXSeparatorMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXSeparator::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXSeparator,FXFrame,FXSeparatorMap,ARRAYNUMBER(FXSeparatorMap))


// Construct and init
FXSeparator::FXSeparator(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  }


// Get default size
FXint FXSeparator::getDefaultWidth(){
  FXint w=(options&(SEPARATOR_GROOVE|SEPARATOR_RIDGE)) ? 2 : 1;
  return w+padleft+padright+(border<<1);
  }


FXint FXSeparator::getDefaultHeight(){
  FXint h=(options&(SEPARATOR_GROOVE|SEPARATOR_RIDGE)) ? 2 : 1;
  return h+padtop+padbottom+(border<<1);
  }


// Handle repaint
long FXSeparator::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  register FXint kk,ll;

  // Draw background
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);

  // Draw frame
  drawFrame(dc,0,0,width,height);

  // Horizonal orientation
  if((height-padbottom-padtop) < (width-padleft-padright)){
    kk=(options&(SEPARATOR_GROOVE|SEPARATOR_RIDGE)) ? 2 : 1;
    ll=border+padtop+(height-padbottom-padtop-(border<<1)-kk)/2;
    if(options&SEPARATOR_GROOVE){
      dc.setForeground(shadowColor);
      dc.fillRectangle(border+padleft,ll,width-padright-padleft-(border<<1),1);
      dc.setForeground(hiliteColor);
      dc.fillRectangle(border+padleft,ll+1,width-padright-padleft-(border<<1),1);
      }
    else if(options&SEPARATOR_RIDGE){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(border+padleft,ll,width-padright-padleft-(border<<1),1);
      dc.setForeground(shadowColor);
      dc.fillRectangle(border+padleft,ll+1,width-padright-padleft-(border<<1),1);
      }
    else if(options&SEPARATOR_LINE){
      dc.setForeground(borderColor);
      dc.fillRectangle(border+padleft,ll,width-padright-padleft-(border<<1),1);
      }
    }

  // Vertical orientation
  else{
    kk=(options&(SEPARATOR_GROOVE|SEPARATOR_RIDGE)) ? 2 : 1;
    ll=border+padleft+(width-padleft-padright-(border<<1)-kk)/2;
    if(options&SEPARATOR_GROOVE){
      dc.setForeground(shadowColor);
      dc.fillRectangle(ll,padtop+border,1,height-padtop-padbottom-(border<<1));
      dc.setForeground(hiliteColor);
      dc.fillRectangle(ll+1,padtop+border,1,height-padtop-padbottom-(border<<1));
      }
    else if(options&SEPARATOR_RIDGE){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(ll,padtop+border,1,height-padtop-padbottom-(border<<1));
      dc.setForeground(shadowColor);
      dc.fillRectangle(ll+1,padtop+border,1,height-padtop-padbottom-(border<<1));
      }
    else if(options&SEPARATOR_LINE){
      dc.setForeground(borderColor);
      dc.fillRectangle(ll,padtop+border,1,height-padtop-padbottom-(border<<1));
      }
    }
  return 1;
  }


// Change separator style
void FXSeparator::setSeparatorStyle(FXuint style){
  FXuint opts=(options&~SEPARATOR_MASK) | (style&SEPARATOR_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get separator style
FXuint FXSeparator::getSeparatorStyle() const {
  return (options&SEPARATOR_MASK);
  }


/*******************************************************************************/


// Object implementation
FXIMPLEMENT(FXHorizontalSeparator,FXSeparator,0,0)


// Construct and init
FXHorizontalSeparator::FXHorizontalSeparator(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXSeparator(p,opts,x,y,w,h,pl,pr,pt,pb){
  }


/*******************************************************************************/


// Object implementation
FXIMPLEMENT(FXVerticalSeparator,FXSeparator,0,0)


// Construct and init
FXVerticalSeparator::FXVerticalSeparator(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXSeparator(p,opts,x,y,w,h,pl,pr,pt,pb){
  }


}

