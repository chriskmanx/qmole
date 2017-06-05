/********************************************************************************
*                                                                               *
*                P a c k e r   C o n t a i n e r   O b j e c t                  *
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
* $Id: FXPacker.cpp,v 1.46 2006/01/22 17:58:37 fox Exp $                        *
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
#include "FXPacker.h"


/*
  To do:
  - Now observes LAYOUT_FIX_X and LAYOUT_FIX_Y hints.
  - LAYOUT_FIX_WIDTH and LAYOUT_FIX_HEIGHT take precedence over PACK_UNIFORM_WIDTH and
    PACK_UNIFORM_HEIGHT!
  - Tabbing order takes widget layout into account
*/

// Side layout modes
#define LAYOUT_SIDE_MASK  (LAYOUT_SIDE_LEFT|LAYOUT_SIDE_RIGHT|LAYOUT_SIDE_TOP|LAYOUT_SIDE_BOTTOM)


// Layout modes
#define LAYOUT_MASK       (LAYOUT_SIDE_MASK|LAYOUT_RIGHT|LAYOUT_CENTER_X|LAYOUT_BOTTOM|LAYOUT_CENTER_Y|LAYOUT_FIX_X|LAYOUT_FIX_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y)


// Frame styles
#define FRAME_MASK        (FRAME_SUNKEN|FRAME_RAISED|FRAME_THICK)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXPacker) FXPackerMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXPacker::onPaint),
  FXMAPFUNC(SEL_FOCUS_UP,0,FXPacker::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXPacker::onFocusDown),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FXPacker::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FXPacker::onFocusRight),
  };


// Object implementation
FXIMPLEMENT(FXPacker,FXComposite,FXPackerMap,ARRAYNUMBER(FXPackerMap))


// Deserialization
FXPacker::FXPacker(){
  flags|=FLAG_SHOWN;
  }


// Create child frame window
FXPacker::FXPacker(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXComposite(p,opts,x,y,w,h){
  flags|=FLAG_SHOWN;
  baseColor=getApp()->getBaseColor();
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  borderColor=getApp()->getBorderColor();
  padtop=pt;
  padbottom=pb;
  padleft=pl;
  padright=pr;
  hspacing=hs;
  vspacing=vs;
  border=(options&FRAME_THICK)?2:(options&(FRAME_SUNKEN|FRAME_RAISED))?1:0;
  }


void FXPacker::drawBorderRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(borderColor);
  dc.drawRectangle(x,y,w-1,h-1);
  }


void FXPacker::drawRaisedRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    }
  }


void FXPacker::drawSunkenRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    }
  }


void FXPacker::drawRidgeRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    if(1<w && 1<h){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      dc.setForeground(shadowColor);
      dc.fillRectangle(x+1,y+1,w-3,1);
      dc.fillRectangle(x+1,y+1,1,h-3);
      }
    }
  }


void FXPacker::drawGrooveRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    if(1<w && 1<h){
      dc.setForeground(shadowColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      dc.setForeground(hiliteColor);
      dc.fillRectangle(x+1,y+1,w-3,1);
      dc.fillRectangle(x+1,y+1,1,h-3);
      }
    }
  }


void FXPacker::drawDoubleRaisedRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(borderColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y,w-1,1);
    dc.fillRectangle(x,y,1,h-1);
    if(1<w && 1<h){
      dc.setForeground(baseColor);
      dc.fillRectangle(x+1,y+1,w-2,1);
      dc.fillRectangle(x+1,y+1,1,h-2);
      dc.setForeground(shadowColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      }
    }
  }

void FXPacker::drawDoubleSunkenRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y,w-1,1);
    dc.fillRectangle(x,y,1,h-1);
    if(1<w && 1<h){
      dc.setForeground(borderColor);
      dc.fillRectangle(x+1,y+1,w-3,1);
      dc.fillRectangle(x+1,y+1,1,h-3);
      dc.setForeground(baseColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      }
    }
  }



// Draw border
void FXPacker::drawFrame(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  switch(options&FRAME_MASK) {
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
long FXPacker::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Change frame border style
void FXPacker::setFrameStyle(FXuint style){
  options=(options&~FRAME_MASK) | (style&FRAME_MASK);
  border=(options&FRAME_THICK) ? 2 : (options&(FRAME_SUNKEN|FRAME_RAISED)) ? 1 : 0;
  recalc();
  update();
  }


// Get frame style
FXuint FXPacker::getFrameStyle() const {
  return (options&FRAME_MASK);
  }


// Change packing hints
void FXPacker::setPackingHints(FXuint ph){
  FXuint opts=(options&~(PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH)) | (ph&(PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH));
  if(opts!=options){
    options=opts;
    recalc();
    update();
    }
  }


// Get packing hints
FXuint FXPacker::getPackingHints() const {
  return (options&(PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH));
  }


// Set base color
void FXPacker::setBaseColor(FXColor clr){
  if(baseColor!=clr){
    baseColor=clr;
    update();
    }
  }


// Set highlight color
void FXPacker::setHiliteColor(FXColor clr){
  if(hiliteColor!=clr){
    hiliteColor=clr;
    update();
    }
  }


// Set shadow color
void FXPacker::setShadowColor(FXColor clr){
  if(shadowColor!=clr){
    shadowColor=clr;
    update();
    }
  }


// Set border color
void FXPacker::setBorderColor(FXColor clr){
  if(borderColor!=clr){
    borderColor=clr;
    update();
    }
  }


// Change top padding
void FXPacker::setPadTop(FXint pt){
  if(padtop!=pt){
    padtop=pt;
    recalc();
    update();
    }
  }


// Change bottom padding
void FXPacker::setPadBottom(FXint pb){
  if(padbottom!=pb){
    padbottom=pb;
    recalc();
    update();
    }
  }


// Change left padding
void FXPacker::setPadLeft(FXint pl){
  if(padleft!=pl){
    padleft=pl;
    recalc();
    update();
    }
  }


// Change right padding
void FXPacker::setPadRight(FXint pr){
  if(padright!=pr){
    padright=pr;
    recalc();
    update();
    }
  }


// Change horizontal spacing
void FXPacker::setHSpacing(FXint hs){
  if(hspacing!=hs){
    hspacing=hs;
    recalc();
    update();
    }
  }


// Change vertical spacing
void FXPacker::setVSpacing(FXint vs){
  if(vspacing!=vs){
    vspacing=vs;
    recalc();
    update();
    }
  }


// Focus moved up
long FXPacker::onFocusUp(FXObject*,FXSelector,void* ptr){
  FXWindow *child,*c;
  FXint cury,childy;
  if(getFocus()){
    cury=getFocus()->getY();
    while(1){
      child=NULL;
      childy=-10000000;
      for(c=getFirst(); c; c=c->getNext()){
        if(c->shown() && c->getY()<cury && childy<c->getY()){ childy=c->getY(); child=c; }
        }
      if(!child) return 0;
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
      cury=childy;
      }
    }
  else{
    child=getLast();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
        }
      child=child->getPrev();
      }
    }
  return 0;
  }


// Focus moved down
long FXPacker::onFocusDown(FXObject*,FXSelector,void* ptr){
  FXWindow *child,*c;
  FXint cury,childy;
  if(getFocus()){
    cury=getFocus()->getY();
    while(1){
      child=NULL;
      childy=10000000;
      for(c=getFirst(); c; c=c->getNext()){
        if(c->shown() && cury<c->getY() && c->getY()<childy){ childy=c->getY(); child=c; }
        }
      if(!child) return 0;
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
      cury=childy;
      }
    }
  else{
    child=getFirst();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
        }
      child=child->getNext();
      }
    }
  return 0;
  }


// Focus moved to left
long FXPacker::onFocusLeft(FXObject*,FXSelector,void* ptr){
  FXWindow *child,*c;
  FXint curx,childx;
  if(getFocus()){
    curx=getFocus()->getX();
    while(1){
      child=NULL;
      childx=-10000000;
      for(c=getFirst(); c; c=c->getNext()){
        if(c->shown() && c->getX()<curx && childx<c->getX()){ childx=c->getX(); child=c; }
        }
      if(!child) return 0;
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
      curx=childx;
      }
    }
  else{
    child=getLast();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
        }
      child=child->getPrev();
      }
    }
  return 0;
  }


// Focus moved to right
long FXPacker::onFocusRight(FXObject*,FXSelector,void* ptr){
  FXWindow *child,*c;
  FXint curx,childx;
  if(getFocus()){
    curx=getFocus()->getX();
    while(1){
      child=NULL;
      childx=10000000;
      for(c=getFirst(); c; c=c->getNext()){
        if(c->shown() && curx<c->getX() && c->getX()<childx){ childx=c->getX(); child=c; }
        }
      if(!child) return 0;
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
      curx=childx;
      }
    }
  else{
    child=getFirst();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
        }
      child=child->getNext();
      }
    }
  return 0;
  }


// Compute minimum width based on child layout hints
FXint FXPacker::getDefaultWidth(){
  register FXint w,wcum,wmax,mw;
  register FXWindow* child;
  register FXuint hints;
  wmax=wcum=mw=0;
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  for(child=getLast(); child; child=child->getPrev()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      if((hints&LAYOUT_RIGHT)&&(hints&LAYOUT_CENTER_X)){        // Fixed X
        w=child->getX()+w;
        if(w>wmax) wmax=w;
        }
      else if(hints&LAYOUT_SIDE_LEFT){                          // Left or right
        if(child->getNext()) wcum+=hspacing;
        wcum+=w;
        }
      else{
        if(w>wcum) wcum=w;
        }
      }
    }
  wcum+=padleft+padright+(border<<1);
  return FXMAX(wcum,wmax);
  }


// Compute minimum height based on child layout hints
FXint FXPacker::getDefaultHeight(){
  register FXint h,hcum,hmax,mh;
  register FXWindow* child;
  register FXuint hints;
  hmax=hcum=mh=0;
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();
  for(child=getLast(); child; child=child->getPrev()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      if((hints&LAYOUT_BOTTOM)&&(hints&LAYOUT_CENTER_Y)){       // Fixed Y
        h=child->getY()+h;
        if(h>hmax) hmax=h;
        }
      else if(!(hints&LAYOUT_SIDE_LEFT)){                       // Top or bottom
        if(child->getNext()) hcum+=vspacing;
        hcum+=h;
        }
      else{
        if(h>hcum) hcum=h;
        }
      }
    }
  hcum+=padtop+padbottom+(border<<1);
  return FXMAX(hcum,hmax);
  }


// Recalculate layout
void FXPacker::layout(){
  register FXint left,right,top,bottom,x,y,w,h;
  register FXint mw=0,mh=0;
  register FXWindow* child;
  register FXuint hints;

  // Placement rectangle; right/bottom non-inclusive
  left=border+padleft;
  right=width-border-padright;
  top=border+padtop;
  bottom=height-border-padbottom;

  // Get maximum child size
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();

  // Pack them in the cavity
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      x=child->getX();
      y=child->getY();

      // Height
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else if(hints&LAYOUT_FILL_Y) h=bottom-top;
      else h=child->getDefaultHeight();

      // Width
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else if(hints&LAYOUT_FILL_X) w=right-left;
      else w=child->getDefaultWidth();

      // Vertical
      if(hints&LAYOUT_SIDE_LEFT){

        // Y
        if(!((hints&LAYOUT_BOTTOM)&&(hints&LAYOUT_CENTER_Y))){
          if(hints&LAYOUT_CENTER_Y) y=top+(bottom-top-h)/2;
          else if(hints&LAYOUT_BOTTOM) y=bottom-h;
          else y=top;
          }

        // X
        if(!((hints&LAYOUT_RIGHT)&&(hints&LAYOUT_CENTER_X))){
          if(hints&LAYOUT_CENTER_X) x=left+(right-left-w)/2;
          else if(hints&LAYOUT_SIDE_BOTTOM){
            x=right-w;
            right-=(w+hspacing);
            }
          else{
            x=left;
            left+=(w+hspacing);
            }
          }
        }

      // Horizontal
      else{

        // X
        if(!((hints&LAYOUT_RIGHT)&&(hints&LAYOUT_CENTER_X))){
          if(hints&LAYOUT_CENTER_X) x=left+(right-left-w)/2;
          else if(hints&LAYOUT_RIGHT) x=right-w;
          else x=left;
          }

        // Y
        if(!((hints&LAYOUT_BOTTOM)&&(hints&LAYOUT_CENTER_Y))){
          if(hints&LAYOUT_CENTER_Y) y=top+(bottom-top-h)/2;
          else if(hints&LAYOUT_SIDE_BOTTOM){
            y=bottom-h;
            bottom-=(h+vspacing);
            }
          else{
            y=top;
            top+=(h+vspacing);
            }
          }
        }
      child->position(x,y,w,h);
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Save object to stream
void FXPacker::save(FXStream& store) const {
  FXComposite::save(store);
  store << baseColor;
  store << hiliteColor;
  store << shadowColor;
  store << borderColor;
  store << padtop << padbottom << padleft << padright;
  store << hspacing << vspacing;
  store << border;
  }


// Load object from stream
void FXPacker::load(FXStream& store){
  FXComposite::load(store);
  store >> baseColor;
  store >> hiliteColor;
  store >> shadowColor;
  store >> borderColor;
  store >> padtop >> padbottom >> padleft >> padright;
  store >> hspacing >> vspacing;
  store >> border;
  }

}
