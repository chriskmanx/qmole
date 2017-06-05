/********************************************************************************
*                                                                               *
*                       S t a t u s   L i n e   W i d g e t                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXStatusLine.cpp,v 1.28 2006/01/22 17:58:42 fox Exp $                    *
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
#include "FXFont.h"
#include "FXHorizontalFrame.h"
#include "FXDragCorner.h"
#include "FXStatusLine.h"



/*
  Notes:
  - Fallback text is displayed when neither cursor window or
    target object supply temporary help string.
  - Show only status line information from same topwindow as where
    status line is
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXStatusLine) FXStatusLineMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXStatusLine::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXStatusLine::onUpdate),
  FXMAPFUNC(SEL_COMMAND,FXStatusLine::ID_SETSTRINGVALUE,FXStatusLine::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXStatusLine::ID_GETSTRINGVALUE,FXStatusLine::onCmdGetStringValue),
  };


// Object implementation
FXIMPLEMENT(FXStatusLine,FXFrame,FXStatusLineMap,ARRAYNUMBER(FXStatusLineMap))


// Deserialization
FXStatusLine::FXStatusLine(){
  flags|=FLAG_SHOWN;
  }


// Construct and init
FXStatusLine::FXStatusLine(FXComposite* p,FXObject* tgt,FXSelector sel):
  FXFrame(p,FRAME_SUNKEN|LAYOUT_LEFT|LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 4,4,2,2){
  flags|=FLAG_SHOWN;
  status=normal=tr("Ready.");
  font=getApp()->getNormalFont();
  textColor=getApp()->getForeColor();
  textHighlightColor=getApp()->getForeColor();
  target=tgt;
  message=sel;
  }


// Create Window
void FXStatusLine::create(){
  FXFrame::create();
  font->create();
  }


// Detach Window
void FXStatusLine::detach(){
  FXFrame::detach();
  font->detach();
  }


// Get default width; as text changes often, exact content does not matter
FXint FXStatusLine::getDefaultWidth(){
  return padleft+padright+(border<<1)+8;
  }


// Get default height; just care about font height
FXint FXStatusLine::getDefaultHeight(){
  return font->getFontHeight()+padtop+padbottom+(border<<1);
  }


// Slightly different from Frame border
long FXStatusLine::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint ty=padtop+(height-padtop-padbottom-font->getFontHeight())/2;
  FXint pos,len;
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);
  if(!status.empty()){
    dc.setFont(font);
    pos=status.find('\n');
    len=status.length();
    if(pos>=0){
      dc.setForeground(textHighlightColor);
      dc.drawText(padleft,ty+font->getFontAscent(),status.text(),pos);
      dc.setForeground(textColor);
      dc.drawText(padleft+font->getTextWidth(status.text(),pos),ty+font->getFontAscent(),status.text()+pos+1,len-pos-1);
      }
    else{
      dc.setForeground(textColor);
      dc.drawText(padleft,ty+font->getFontAscent(),status.text(),len);
      }
    }
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// If the cursor is inside a widget, flash its help text;
// Otherwise, unflash back to the regular status message.
long FXStatusLine::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  FXWindow *helpsource=getApp()->getCursorWindow();

  // Regular GUI update
  FXFrame::onUpdate(sender,sel,ptr);

  // Ask the help source for a new status text first, but only if the
  // statusline's shell is a direct or indirect owner of the help source
  if(helpsource && getShell()->isOwnerOf(helpsource) && helpsource->handle(this,FXSEL(SEL_QUERY_HELP,0),NULL)){
    return 1;
    }

  // Otherwise, supply normal message
  setText(normal);
  return 1;
  }


// Update value from a message
long FXStatusLine::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setText(*((FXString*)ptr));
  return 1;
  }


// Obtain value from text field
long FXStatusLine::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getText();
  return 1;
  }


// Set currently displayed message
void FXStatusLine::setText(const FXString& text){
  if(status!=text){
    status=text;
    update(border,border,width-(border<<1),height-(border<<1));
    repaint(border,border,width-(border<<1),height-(border<<1));
    getApp()->flush();
    }
  }


// Set permanently displayed message
void FXStatusLine::setNormalText(const FXString& text){
  if(normal!=text){
    normal=text;
    update(border,border,width-(border<<1),height-(border<<1));
    repaint(border,border,width-(border<<1),height-(border<<1));
    getApp()->flush();
    }
  }


// Change the font
void FXStatusLine::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set text color
void FXStatusLine::setTextColor(FXColor clr){
  if(textColor!=clr){
    textColor=clr;
    update(border,border,width-(border<<1),height-(border<<1));
    }
  }


// Set text highlight color
void FXStatusLine::setTextHighlightColor(FXColor clr){
  if(textHighlightColor!=clr){
    textHighlightColor=clr;
    update(border,border,width-(border<<1),height-(border<<1));
    }
  }


// Save object to stream
void FXStatusLine::save(FXStream& store) const {
  FXFrame::save(store);
  store << status;
  store << normal;
  store << font;
  store << textColor;
  store << textHighlightColor;
  }


// Load object from stream
void FXStatusLine::load(FXStream& store){
  FXFrame::load(store);
  store >> status;
  store >> normal;
  store >> font;
  store >> textColor;
  store >> textHighlightColor;
  }


// Destruct
FXStatusLine::~FXStatusLine(){
  font=(FXFont*)-1L;
  }

}
