/********************************************************************************
*                                                                               *
*                         C o l o r   L i s t   W i d g e t                     *
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
* $Id: FXColorList.cpp,v 1.4 2006/01/22 17:58:20 fox Exp $                      *
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
#include "FXObjectList.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXScrollBar.h"
#include "FXColorList.h"



/*

  Notes:
*/


#define ICON_SPACING             4    // Spacing between icon and label
#define SIDE_SPACING             6    // Left or right spacing between items
#define LINE_SPACING             4    // Line spacing between items

#define SWATCH_WIDTH            24    // Swatch size
#define SWATCH_HEIGHT           12

using namespace FX;


/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXColorItem,FXListItem,NULL,0)


// Draw item
void FXColorItem::draw(const FXList* list,FXDC& dc,FXint xx,FXint yy,FXint ww,FXint hh){
  register FXFont *font=list->getFont();
  register FXint th=0;
  if(!label.empty()) th=font->getFontHeight();
  if(isSelected())
    dc.setForeground(list->getSelBackColor());
  else
    dc.setForeground(list->getBackColor());
  dc.fillRectangle(xx,yy,ww,hh);
  if(hasFocus()){
    dc.drawFocusRectangle(xx+1,yy+1,ww-2,hh-2);
    }
  xx+=SIDE_SPACING/2;
  dc.setForeground(color);
  dc.fillRectangle(xx,yy+(hh-SWATCH_HEIGHT)/2,SWATCH_WIDTH,SWATCH_HEIGHT);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawRectangle(xx,yy+(hh-SWATCH_HEIGHT)/2,SWATCH_WIDTH,SWATCH_HEIGHT);
  xx+=ICON_SPACING+SWATCH_WIDTH;
  if(!label.empty()){
    dc.setFont(font);
    if(!isEnabled())
      dc.setForeground(makeShadowColor(list->getBackColor()));
    else if(isSelected())
      dc.setForeground(list->getSelTextColor());
    else
      dc.setForeground(list->getTextColor());
    dc.drawText(xx,yy+(hh-th)/2+font->getFontAscent(),label);
    }
  }


// See if item got hit, and where: 0 is outside, 1 is icon, 2 is text
FXint FXColorItem::hitItem(const FXList* list,FXint xx,FXint yy) const {
  register FXint tw=0,th=0,ix,iy,tx,ty,h;
  register FXFont *font=list->getFont();
  if(!label.empty()){
    tw=4+font->getTextWidth(label);
    th=4+font->getFontHeight();
    }
  h=LINE_SPACING+FXMAX(th,SWATCH_HEIGHT);
  ix=SIDE_SPACING/2;
  tx=SIDE_SPACING/2+SWATCH_WIDTH+ICON_SPACING;
  iy=(h-SWATCH_HEIGHT)/2;
  ty=(h-th)/2;

  // In icon?
  if(ix<=xx && iy<=yy && xx<ix+SWATCH_WIDTH && yy<iy+SWATCH_HEIGHT) return 1;

  // In text?
  if(tx<=xx && ty<=yy && xx<tx+tw && yy<ty+th) return 2;

  // Outside
  return 0;
  }


// Get width of item
FXint FXColorItem::getWidth(const FXList* list) const {
  register FXFont *font=list->getFont();
  register FXint w=SWATCH_WIDTH;
  if(!label.empty()) w+=ICON_SPACING+font->getTextWidth(label);
  return SIDE_SPACING+w;
  }


// Get height of item
FXint FXColorItem::getHeight(const FXList* list) const {
  register FXFont *font=list->getFont();
  register FXint h=0;
  if(!label.empty()) h=font->getFontHeight();
  return LINE_SPACING+FXMAX(h,SWATCH_HEIGHT);
  }


/*******************************************************************************/


// Object implementation
FXIMPLEMENT(FXColorList,FXList,NULL,0)


// List
FXColorList::FXColorList(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):FXList(p,tgt,sel,opts,x,y,w,h){
  }


// Create custom item
FXListItem *FXColorList::createItem(const FXString& text,FXIcon*,void* ptr){
  return new FXColorItem(text,FXRGB(0,0,0),ptr);
  }


// Fill list by appending color items from array of strings and array of colors
FXint FXColorList::fillItems(const FXchar** strings,FXColor *colors,void* ptr,FXbool notify){
  register FXint n=0;
  if(strings){
    while(strings[n]){
      appendItem(strings[n],colors[n],ptr,notify);
      n++;
      }
    }
  return n;
  }


// Insert item at index with given text, color, and user-data pointer
FXint FXColorList::insertItem(FXint index,const FXString& text,FXColor color,void* ptr,FXbool notify){
  FXint pos=FXList::insertItem(index,text,NULL,ptr,notify);
  setItemColor(pos,color);
  return pos;
  }


// Append new item with given text, color, and user-data pointer
FXint FXColorList::appendItem(const FXString& text,FXColor color,void* ptr,FXbool notify){
  FXint pos=FXList::appendItem(text,NULL,ptr,notify);
  setItemColor(pos,color);
  return pos;
  }

// Prepend new item with given text, color, and user-data pointer
FXint FXColorList::prependItem(const FXString& text,FXColor color,void* ptr,FXbool notify){
  FXint pos=FXList::prependItem(text,NULL,ptr,notify);
  setItemColor(pos,color);
  return pos;
  }


// Change item color
void FXColorList::setItemColor(FXint index,FXColor color){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemData: index out of range.\n",getClassName()); }
  ((FXColorItem*)items[index])->setColor(color);
  }


// Return item color
FXColor FXColorList::getItemColor(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemData: index out of range.\n",getClassName()); }
  return ((FXColorItem*)items[index])->getColor();
  }

}

