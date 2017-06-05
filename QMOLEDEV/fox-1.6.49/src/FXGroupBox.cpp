/********************************************************************************
*                                                                               *
*                G r o u p  B o x   W i n d o w   O b j e c t                   *
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
* $Id: FXGroupBox.cpp,v 1.38 2006/01/22 17:58:30 fox Exp $                      *
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDC.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXLabel.h"
#include "FXGroupBox.h"


/*
  Notes:

  - Radio behaviour of groupbox has been dropped; groupbox is now
    purely a decorative layout manager.
*/


#define FRAME_MASK           (FRAME_SUNKEN|FRAME_RAISED|FRAME_THICK)
#define GROUPBOX_TITLE_MASK  (GROUPBOX_TITLE_LEFT|GROUPBOX_TITLE_CENTER|GROUPBOX_TITLE_RIGHT)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXGroupBox) FXGroupBoxMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXGroupBox::onPaint),
  FXMAPFUNC(SEL_COMMAND,FXGroupBox::ID_SETVALUE,FXGroupBox::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXGroupBox::ID_SETSTRINGVALUE,FXGroupBox::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXGroupBox::ID_GETSTRINGVALUE,FXGroupBox::onCmdGetStringValue),
  };


// Object implementation
FXIMPLEMENT(FXGroupBox,FXPacker,FXGroupBoxMap,ARRAYNUMBER(FXGroupBoxMap))


// Deserialization
FXGroupBox::FXGroupBox(){
  flags|=FLAG_ENABLED;
  font=(FXFont*)-1L;
  textColor=0;
  }


// Make a groupbox
FXGroupBox::FXGroupBox(FXComposite* p,const FXString& text,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXPacker(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs),label(text){
  flags|=FLAG_ENABLED;
  font=getApp()->getNormalFont();
  textColor=getApp()->getForeColor();
  }


// Create window
void FXGroupBox::create(){
  FXPacker::create();
  font->create();
  }


// Detach window
void FXGroupBox::detach(){
  FXPacker::detach();
  font->detach();
  }


// Enable the window
void FXGroupBox::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXPacker::enable();
    update();
    }
  }


// Disable the window
void FXGroupBox::disable(){
  if(flags&FLAG_ENABLED){
    FXPacker::disable();
    update();
    }
  }


// Change the font
void FXGroupBox::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Get default width
FXint FXGroupBox::getDefaultWidth(){
  FXint cw=FXPacker::getDefaultWidth();
  if(!label.empty()){
    FXint tw=font->getTextWidth(label)+16;
    return FXMAX(cw,tw);
    }
  return cw;
  }


// Get default height
FXint FXGroupBox::getDefaultHeight(){
  FXint ch=FXPacker::getDefaultHeight();
  if(!label.empty()){
    return ch+font->getFontHeight()+4-border;           // Have text instead of border
    }
  return ch;
  }


// Recompute layout
void FXGroupBox::layout(){
  FXint tmp=padtop;
  if(!label.empty()){
    padtop=padtop+font->getFontHeight()+4-border;       // Have text instead of border
    }
  FXPacker::layout();
  flags&=~FLAG_DIRTY;
  padtop=tmp;
  }


// Update value from a message
long FXGroupBox::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setText((const FXchar*)ptr);
  return 1;
  }


// Update value from a message
long FXGroupBox::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setText(*((FXString*)ptr));
  return 1;
  }


// Obtain value from text field
long FXGroupBox::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getText();
  return 1;
  }


// Handle repaint
long FXGroupBox::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint tw,th,yy,xx;

  xx=0;
  yy=0;

  // Paint background
  dc.setForeground(backColor);
  dc.fillRectangle(event->rect.x,event->rect.y,event->rect.w,event->rect.h);

  // Draw label if there is one
  if(!label.empty()){
    yy=2+font->getFontAscent()/2;
    }

  // We should really just draw what's exposed!
  switch(options&FRAME_MASK) {
    case FRAME_LINE: drawBorderRectangle(dc,0,yy,width,height-yy); break;
    case FRAME_SUNKEN: drawSunkenRectangle(dc,0,yy,width,height-yy); break;
    case FRAME_RAISED: drawRaisedRectangle(dc,0,yy,width,height-yy); break;
    case FRAME_GROOVE: drawGrooveRectangle(dc,0,yy,width,height-yy); break;
    case FRAME_RIDGE: drawRidgeRectangle(dc,0,yy,width,height-yy); break;
    case FRAME_SUNKEN|FRAME_THICK: drawDoubleSunkenRectangle(dc,0,yy,width,height-yy); break;
    case FRAME_RAISED|FRAME_THICK: drawDoubleRaisedRectangle(dc,0,yy,width,height-yy); break;
    }

  // Draw label
  if(!label.empty()){
    tw=font->getTextWidth(label);
    th=font->getFontHeight()+4;
    if(options&GROUPBOX_TITLE_RIGHT) xx=width-tw-12;
    else if(options&GROUPBOX_TITLE_CENTER) xx=(width-tw)/2-4;
    else xx=4;
    if(xx<4) xx=4;
    if(tw+16>width) tw=width-16;
    if(0<tw){
      dc.setForeground(backColor);
      dc.setFont(font);
      dc.fillRectangle(xx,yy,tw+8,2);
      dc.setClipRectangle(xx+4,0,tw,th);
      if(isEnabled()){
        dc.setForeground(textColor);
        dc.drawText(xx+4,2+font->getFontAscent(),label);
        }
      else{
        dc.setForeground(hiliteColor);
        dc.drawText(xx+5,3+font->getFontAscent(),label);
        dc.setForeground(shadowColor);
        dc.drawText(xx+4,2+font->getFontAscent(),label);
        }
      }
    }
  return 1;
  }


// Get group box style
FXuint FXGroupBox::getGroupBoxStyle() const {
  return (options&GROUPBOX_TITLE_MASK);
  }


// Set group box style
void FXGroupBox::setGroupBoxStyle(FXuint style){
  FXuint opts=(options&~GROUPBOX_TITLE_MASK) | (style&GROUPBOX_TITLE_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Change text
void FXGroupBox::setText(const FXString& text){
  if(label!=text){
    label=text;
    recalc();
    update();
    }
  }


// Set text color
void FXGroupBox::setTextColor(FXColor clr){
  if(textColor!=clr){
    textColor=clr;
    update();
    }
  }


// Save object to stream
void FXGroupBox::save(FXStream& store) const {
  FXPacker::save(store);
  store << label;
  store << font;
  store << textColor;
  }


// Load object from stream
void FXGroupBox::load(FXStream& store){
  FXPacker::load(store);
  store >> label;
  store >> font;
  store >> textColor;
  }

}
