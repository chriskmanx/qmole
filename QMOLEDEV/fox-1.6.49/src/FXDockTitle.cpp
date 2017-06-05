/********************************************************************************
*                                                                               *
*                         D o c k T i t l e   W i d g e t                       *
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
* $Id: FXDockTitle.cpp,v 1.6 2006/01/22 17:58:23 fox Exp $                      *
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
#include "FXFont.h"
#include "FXDCWindow.h"
#include "FXDockTitle.h"


/*
  Notes:
*/


#define JUSTIFY_MASK (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)


using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXDockTitle) FXDockTitleMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXDockTitle::onPaint),
  FXMAPFUNC(SEL_COMMAND,FXDockTitle::ID_SETVALUE,FXDockTitle::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXDockTitle::ID_SETSTRINGVALUE,FXDockTitle::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXDockTitle::ID_GETSTRINGVALUE,FXDockTitle::onCmdGetStringValue),
  };


// Object implementation
FXIMPLEMENT(FXDockTitle,FXDockHandler,FXDockTitleMap,ARRAYNUMBER(FXDockTitleMap))


// Deserialization
FXDockTitle::FXDockTitle(){
  font=(FXFont*)-1L;
  captionColor=0;
  }


// Construct and init
FXDockTitle::FXDockTitle(FXComposite* p,const FXString& text,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXDockHandler(p,tgt,sel,opts,x,y,w,h,pl,pr,pt,pb),caption(text){
  font=getApp()->getNormalFont();
  captionColor=getApp()->getSelforeColor();
  backColor=getApp()->getSelbackColor();
  }


// Create window
void FXDockTitle::create(){
  FXFrame::create();
  font->create();
  }


// Detach window
void FXDockTitle::detach(){
  FXFrame::detach();
  font->detach();
  }


// Get default width
FXint FXDockTitle::getDefaultWidth(){
  register FXint w=0;
  if(!caption.empty()) w=font->getTextWidth(caption.text(),caption.length());
  return padleft+padright+(border<<1)+w;
  }


// Get default height
FXint FXDockTitle::getDefaultHeight(){
  register FXint h=0;
  if(!caption.empty()) h=font->getFontHeight();
  return padtop+padbottom+(border<<1)+h;
  }


// Handle repaint
long FXDockTitle::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=static_cast<FXEvent*>(ptr);
  FXDCWindow dc(this,event);
  FXint tw,th,tx,ty;
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));
  if(!caption.empty()){
    dc.setFont(font);
    tw=font->getTextWidth(caption.text(),caption.length());
    th=font->getFontHeight();
    if(options&JUSTIFY_LEFT) tx=padleft+border;
    else if(options&JUSTIFY_RIGHT) tx=width-padright-border-tw;
    else tx=border+padleft+(width-padleft-padright-(border<<1)-tw)/2;
    if(options&JUSTIFY_TOP) ty=border+padtop;
    else if(options&JUSTIFY_BOTTOM) ty=height-padbottom-border-th;
    else ty=border+padtop+(height-padbottom-padtop-(border<<1)-th)/2;
    dc.setForeground(captionColor);
    dc.drawText(tx,ty+font->getFontAscent(),caption);
    }
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Update value from a message
long FXDockTitle::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCaption((const FXchar*)ptr);
  return 1;
  }


// Update value from a message
long FXDockTitle::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setCaption(*((FXString*)ptr));
  return 1;
  }


// Obtain value from text field
long FXDockTitle::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getCaption();
  return 1;
  }


// Change caption
void FXDockTitle::setCaption(const FXString& text){
  if(caption!=text){
    caption=text;
    recalc();
    update();
    }
  }


// Change the font
void FXDockTitle::setFont(FXFont *fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set caption color
void FXDockTitle::setCaptionColor(FXColor clr){
  if(clr!=captionColor){
    captionColor=clr;
    update();
    }
  }


// Set text justify style
void FXDockTitle::setJustify(FXuint style){
  FXuint opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get text justify style
FXuint FXDockTitle::getJustify() const {
  return (options&JUSTIFY_MASK);
  }


// Save data
void FXDockTitle::save(FXStream& store) const {
  FXDockHandler::save(store);
  store << caption;
  store << font;
  store << captionColor;
  }


// Load data
void FXDockTitle::load(FXStream& store){
  FXDockHandler::load(store);
  store >> caption;
  store >> font;
  store >> captionColor;
  }


// Destroy
FXDockTitle::~FXDockTitle(){
  font=(FXFont*)-1L;
  }


}
