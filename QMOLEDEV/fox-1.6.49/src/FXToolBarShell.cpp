/********************************************************************************
*                                                                               *
*                    T o o l   B a r   S h e l l   W i d g e t                  *
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
* $Id: FXToolBarShell.cpp,v 1.16 2006/01/22 17:58:48 fox Exp $                  *
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
#include "FXCursor.h"
#include "FXToolBarShell.h"

/*
  Notes:
  - Managed by Window Manager because it needs to stay on top of window.
  - Window manager may hide it when application does not have focus.
  - If it has a child and the child is shown, it will show, otherwise it'll hide.
  - Need some code to allow grabbing of edges to resize [w/o intervention of WM].
  - Allow reshaping FXToolbarShell by pulling on edges.
*/


#define FRAME_MASK        (FRAME_SUNKEN|FRAME_RAISED|FRAME_THICK)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXToolBarShell) FXToolBarShellMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXToolBarShell::onPaint),
  };



// Object implementation
FXIMPLEMENT(FXToolBarShell,FXTopWindow,FXToolBarShellMap,ARRAYNUMBER(FXToolBarShellMap))


// Make toolbar shell
FXToolBarShell::FXToolBarShell(FXWindow* owner,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint hs,FXint vs):
  FXTopWindow(owner,FXString::null,NULL,NULL,(opts|DECOR_SHRINKABLE|DECOR_STRETCHABLE)&~(DECOR_TITLE|DECOR_MINIMIZE|DECOR_MAXIMIZE|DECOR_CLOSE|DECOR_BORDER|DECOR_MENU),x,y,w,h,0,0,0,0,hs,vs){
  baseColor=getApp()->getBaseColor();
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  borderColor=getApp()->getBorderColor();
  border=(options&FRAME_THICK)?2:(options&(FRAME_SUNKEN|FRAME_RAISED))?1:0;
  }


// Create window
void FXToolBarShell::create(){
  FXTopWindow::create();
  if(getFirst() && getFirst()->shown()) show();
  }



void FXToolBarShell::drawBorderRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(borderColor);
  dc.drawRectangle(x,y,w-1,h-1);
  }


void FXToolBarShell::drawRaisedRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(shadowColor);
  dc.fillRectangle(x,y+h-1,w,1);
  dc.fillRectangle(x+w-1,y,1,h);
  dc.setForeground(hiliteColor);
  dc.fillRectangle(x,y,w,1);
  dc.fillRectangle(x,y,1,h);
  }


void FXToolBarShell::drawSunkenRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(shadowColor);
  dc.fillRectangle(x,y,w,1);
  dc.fillRectangle(x,y,1,h);
  dc.setForeground(hiliteColor);
  dc.fillRectangle(x,y+h-1,w,1);
  dc.fillRectangle(x+w-1,y,1,h);
  }


void FXToolBarShell::drawRidgeRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(hiliteColor);
  dc.fillRectangle(x,y,w,1);
  dc.fillRectangle(x,y,1,h);
  dc.fillRectangle(x+1,y+h-2,w-2,1);
  dc.fillRectangle(x+w-2,y+1,1,h-2);
  dc.setForeground(shadowColor);
  dc.fillRectangle(x+1,y+1,w-3,1);
  dc.fillRectangle(x+1,y+1,1,h-3);
  dc.fillRectangle(x,y+h-1,w,1);
  dc.fillRectangle(x+w-1,y,1,h);
  }


void FXToolBarShell::drawGrooveRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(shadowColor);
  dc.fillRectangle(x,y,w,1);
  dc.fillRectangle(x,y,1,h);
  dc.fillRectangle(x+1,y+h-2,w-2,1);
  dc.fillRectangle(x+w-2,y+1,1,h-2);
  dc.setForeground(hiliteColor);
  dc.fillRectangle(x+1,y+1,w-3,1);
  dc.fillRectangle(x+1,y+1,1,h-3);
  dc.fillRectangle(x,y+h-1,w,1);
  dc.fillRectangle(x+w-1,y,1,h);
  }


void FXToolBarShell::drawDoubleRaisedRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(baseColor);
  dc.fillRectangle(x,y,w-1,1);
  dc.fillRectangle(x,y,1,h-1);
  dc.setForeground(hiliteColor);
  dc.fillRectangle(x+1,y+1,w-2,1);
  dc.fillRectangle(x+1,y+1,1,h-2);
  dc.setForeground(shadowColor);
  dc.fillRectangle(x+1,y+h-2,w-2,1);
  dc.fillRectangle(x+w-2,y+1,1,h-1);
  dc.setForeground(borderColor);
  dc.fillRectangle(x,y+h-1,w,1);
  dc.fillRectangle(x+w-1,y,1,h);
  }


void FXToolBarShell::drawDoubleSunkenRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(shadowColor);
  dc.fillRectangle(x,y,w-1,1);
  dc.fillRectangle(x,y,1,h-1);
  dc.setForeground(borderColor);
  dc.fillRectangle(x+1,y+1,w-3,1);
  dc.fillRectangle(x+1,y+1,1,h-3);
  dc.setForeground(hiliteColor);
  dc.fillRectangle(x,y+h-1,w,1);
  dc.fillRectangle(x+w-1,y,1,h);
  dc.setForeground(baseColor);
  dc.fillRectangle(x+1,y+h-2,w-2,1);
  dc.fillRectangle(x+w-2,y+1,1,h-2);
  }


// Draw border
void FXToolBarShell::drawFrame(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  switch(options&FRAME_MASK){
    case FRAME_LINE: drawBorderRectangle(dc,x,y,w,h); break;
    case FRAME_SUNKEN: drawSunkenRectangle(dc,x,y,w,h); break;
    case FRAME_RAISED: drawRaisedRectangle(dc,x,y,w,h); break;
    case FRAME_GROOVE: drawGrooveRectangle(dc,x,y,w,h); break;
    case FRAME_RIDGE: drawRidgeRectangle(dc,x,y,w,h); break;
    case FRAME_SUNKEN|FRAME_THICK: drawDoubleSunkenRectangle(dc,x,y,w,h); break;
    case FRAME_RAISED|FRAME_THICK: drawDoubleRaisedRectangle(dc,x,y,w,h); break;
    }
  }


// Handle repaint
long FXToolBarShell::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Get width
FXint FXToolBarShell::getDefaultWidth(){
  register FXWindow* child=getFirst();
  register FXuint hints;
  register FXint w=0;
  if(child && child->shown()){
    hints=child->getLayoutHints();
    if(hints&LAYOUT_FIX_WIDTH){       // Fixed width
      w=child->getWidth();
      }
    else if(hints&LAYOUT_SIDE_LEFT){  // Vertical
      w=child->getWidthForHeight((hints&LAYOUT_FIX_HEIGHT) ? child->getHeight() : child->getDefaultHeight());
      }
    else{                             // Horizontal
      w=child->getDefaultWidth();
      }
    }
  return w+(border<<1);
  }


// Get height
FXint FXToolBarShell::getDefaultHeight(){
  register FXWindow* child=getFirst();
  register FXuint hints;
  register FXint h=0;
  if(child && child->shown()){
    hints=child->getLayoutHints();
    if(hints&LAYOUT_FIX_HEIGHT){      // Fixed height
      h=child->getHeight();
      }
    else if(hints&LAYOUT_SIDE_LEFT){  // Vertical
      h=child->getDefaultHeight();
      }
    else{                             // Horizontal
      h=child->getHeightForWidth((hints&LAYOUT_FIX_WIDTH) ? child->getWidth() : child->getDefaultWidth());
      }
    }
  return h+(border<<1);
  }


// Recalculate layout
void FXToolBarShell::layout(){
  if(getFirst()){
    if(getFirst()->shown()){
      getFirst()->position(border,border,width-(border<<1),height-(border<<1));
      show();
      }
    else{
      hide();
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Change frame border style
void FXToolBarShell::setFrameStyle(FXuint style){
  FXuint opts=(options&~FRAME_MASK) | (style&FRAME_MASK);
  if(options!=opts){
    FXint b=(opts&FRAME_THICK) ? 2 : (opts&(FRAME_SUNKEN|FRAME_RAISED)) ? 1 : 0;
    options=opts;
    if(border!=b){
      border=b;
      recalc();
      }
    update();
    }
  }


// Get frame style
FXuint FXToolBarShell::getFrameStyle() const {
  return (options&FRAME_MASK);
  }


// Set base color
void FXToolBarShell::setBaseColor(FXColor clr){
  if(clr!=baseColor){
    baseColor=clr;
    update();
    }
  }


// Set highlight color
void FXToolBarShell::setHiliteColor(FXColor clr){
  if(clr!=hiliteColor){
    hiliteColor=clr;
    update();
    }
  }


// Set shadow color
void FXToolBarShell::setShadowColor(FXColor clr){
  if(clr!=shadowColor){
    shadowColor=clr;
    update();
    }
  }


// Set border color
void FXToolBarShell::setBorderColor(FXColor clr){
  if(clr!=borderColor){
    borderColor=clr;
    update();
    }
  }


// Save data
void FXToolBarShell::save(FXStream& store) const {
  FXTopWindow::save(store);
  store << baseColor;
  store << hiliteColor;
  store << shadowColor;
  store << borderColor;
  store << border;
  }


// Load data
void FXToolBarShell::load(FXStream& store){
  FXTopWindow::load(store);
  store >> baseColor;
  store >> hiliteColor;
  store >> shadowColor;
  store >> borderColor;
  store >> border;
  }

}
