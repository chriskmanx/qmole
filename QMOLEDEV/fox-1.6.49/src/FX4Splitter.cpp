/********************************************************************************
*                                                                               *
*                       F o u r - W a y   S p l i t t e r                       *
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
* $Id: FX4Splitter.cpp,v 1.52 2006/02/20 03:32:13 fox Exp $                     *
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
#include "FXDCWindow.h"
#include "FX4Splitter.h"

/*
  Notes:
  - 4Splitter always splits into four partitions.
  - 4Splitter determines pane sizes by split fraction, i.e. if 4Splitter
    resizes, each sub pane gets proportionally resized also.
  - Should we send SEL_CHANGED and SEL_COMMAND also when splitter arrangement
    was changed programmatically?
  - Slightly complex code which takes care of expanding subsets of panels judiciously
    added 2/8/2006.
*/


// Splitter styles
#define FOURSPLITTER_MASK     FOURSPLITTER_TRACKING

// Modes
#define NOWHERE      0
#define ONVERTICAL   1
#define ONHORIZONTAL 2
#define ONCENTER     (ONVERTICAL|ONHORIZONTAL)


using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FX4Splitter) FX4SplitterMap[]={
  FXMAPFUNC(SEL_MOTION,0,FX4Splitter::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FX4Splitter::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FX4Splitter::onLeftBtnRelease),
  FXMAPFUNC(SEL_FOCUS_UP,0,FX4Splitter::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FX4Splitter::onFocusDown),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FX4Splitter::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FX4Splitter::onFocusRight),
  FXMAPFUNCS(SEL_UPDATE,FX4Splitter::ID_EXPAND_NONE,FX4Splitter::ID_EXPAND_ALL,FX4Splitter::onUpdExpand),
  FXMAPFUNCS(SEL_COMMAND,FX4Splitter::ID_EXPAND_NONE,FX4Splitter::ID_EXPAND_ALL,FX4Splitter::onCmdExpand),
  };


// Object implementation
FXIMPLEMENT(FX4Splitter,FXComposite,FX4SplitterMap,ARRAYNUMBER(FX4SplitterMap))


// Make a splitter
FX4Splitter::FX4Splitter(){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  splitx=0;
  splity=0;
  barsize=4;
  fhor=5000;
  fver=5000;
  offx=0;
  offy=0;
  mode=NOWHERE;
  }


// Make a splitter; it has no interior padding, and no borders
FX4Splitter::FX4Splitter(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h):FXComposite(p,opts,x,y,w,h){
  defaultCursor=getApp()->getDefaultCursor(DEF_ARROW_CURSOR);
  dragCursor=defaultCursor;
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  splitx=0;
  splity=0;
  barsize=4;
  fhor=5000;
  fver=5000;
  offx=0;
  offy=0;
  mode=NOWHERE;
  }


// Make a splitter; it has no interior padding, and no borders
FX4Splitter::FX4Splitter(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):FXComposite(p,opts,x,y,w,h){
  defaultCursor=getApp()->getDefaultCursor(DEF_ARROW_CURSOR);
  dragCursor=defaultCursor;
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  target=tgt;
  message=sel;
  splitx=0;
  splity=0;
  barsize=4;
  fhor=5000;
  fver=5000;
  offx=0;
  offy=0;
  mode=NOWHERE;
  }


// Get top left child
FXWindow *FX4Splitter::getTopLeft() const {
  return getFirst();
  }


// Get top right child
FXWindow *FX4Splitter::getTopRight() const {
  if(!getTopLeft()) return NULL;
  return getTopLeft()->getNext();
  }


// Get bottom left child
FXWindow *FX4Splitter::getBottomLeft() const {
  if(!getTopRight()) return NULL;
  return getTopRight()->getNext();
  }


// Get bottom right child
FXWindow *FX4Splitter::getBottomRight() const {
  if(!getBottomLeft()) return NULL;
  return getBottomLeft()->getNext();
  }


// Get default width
FXint FX4Splitter::getDefaultWidth(){
  FXWindow *tl=getTopLeft();
  FXWindow *tr=getTopRight();
  FXWindow *bl=getBottomLeft();
  FXWindow *br=getBottomRight();
  FXint tlw=0,blw=0,trw=0,brw=0,set=0;
  if(tl && tl->shown()){ tlw=tl->getDefaultWidth(); set|=ExpandTopLeft; }
  if(tr && tr->shown()){ trw=tr->getDefaultWidth(); set|=ExpandTopRight; }
  if(bl && bl->shown()){ blw=bl->getDefaultWidth(); set|=ExpandBottomLeft; }
  if(br && br->shown()){ brw=br->getDefaultWidth(); set|=ExpandBottomRight; }
  switch(set){
    case ExpandTopLeft: return tlw;
    case ExpandTopRight: return trw;
    case ExpandBottomRight: return brw;
    case ExpandBottomLeft: return blw;

    case ExpandTopLeft|ExpandTopRight: return trw+tlw+barsize;
    case ExpandBottomLeft|ExpandBottomRight: return brw+blw+barsize;

    case ExpandBottomLeft|ExpandTopLeft: return FXMAX(tlw,blw);
    case ExpandBottomLeft|ExpandTopRight: return FXMAX(blw,trw);
    case ExpandBottomRight|ExpandTopLeft: return FXMAX(brw,tlw);
    case ExpandBottomRight|ExpandTopRight: return FXMAX(brw,trw);

    case ExpandBottomLeft|ExpandTopLeft|ExpandTopRight: return FXMAX(trw+tlw+barsize,blw);
    case ExpandBottomRight|ExpandTopLeft|ExpandTopRight: return FXMAX(trw+tlw+barsize,brw);
    case ExpandTopLeft|ExpandBottomLeft|ExpandBottomRight: return FXMAX(brw+blw+barsize,tlw);
    case ExpandTopRight|ExpandBottomLeft|ExpandBottomRight: return FXMAX(brw+blw+barsize,trw);

    case ExpandTopLeft|ExpandBottomLeft|ExpandTopRight|ExpandBottomRight: return barsize+FXMAX(tlw,blw)+FXMAX(trw,brw);
    }
  return 0;
  }


// Get default height
FXint FX4Splitter::getDefaultHeight(){
  FXWindow *tl=getTopLeft();
  FXWindow *tr=getTopRight();
  FXWindow *bl=getBottomLeft();
  FXWindow *br=getBottomRight();
  FXint tlh=0,blh=0,trh=0,brh=0,set=0;
  if(tl && tl->shown()){ tlh=tl->getDefaultHeight(); set|=ExpandTopLeft; }
  if(tr && tr->shown()){ trh=tr->getDefaultHeight(); set|=ExpandTopRight; }
  if(bl && bl->shown()){ blh=bl->getDefaultHeight(); set|=ExpandBottomLeft; }
  if(br && br->shown()){ brh=br->getDefaultHeight(); set|=ExpandBottomRight; }
  switch(set){
    case ExpandTopLeft: return tlh;
    case ExpandTopRight: return trh;
    case ExpandBottomRight: return brh;
    case ExpandBottomLeft: return blh;

    case ExpandTopLeft|ExpandTopRight: return FXMAX(tlh,trh);
    case ExpandBottomLeft|ExpandBottomRight: return FXMAX(blh,brh);

    case ExpandBottomLeft|ExpandTopLeft: return blh+tlh+barsize;
    case ExpandBottomLeft|ExpandTopRight: return blh+trh+barsize;
    case ExpandBottomRight|ExpandTopLeft: return brh+tlh+barsize;
    case ExpandBottomRight|ExpandTopRight: return brh+trh+barsize;

    case ExpandBottomLeft|ExpandTopLeft|ExpandTopRight: return FXMAX(tlh,trh)+blh+barsize;
    case ExpandBottomRight|ExpandTopLeft|ExpandTopRight: return FXMAX(tlh,trh)+brh+barsize;
    case ExpandTopLeft|ExpandBottomLeft|ExpandBottomRight: return FXMAX(blh,brh)+tlh+barsize;
    case ExpandTopRight|ExpandBottomLeft|ExpandBottomRight: return FXMAX(blh,brh)+trh+barsize;

    case ExpandTopLeft|ExpandBottomLeft|ExpandTopRight|ExpandBottomRight: return barsize+FXMAX(tlh,trh)+FXMAX(blh,brh);
    }
  return 0;
  }


// Recompute layout
void FX4Splitter::layout(){
  FXWindow *tl=getTopLeft();
  FXWindow *tr=getTopRight();
  FXWindow *bl=getBottomLeft();
  FXWindow *br=getBottomRight();
  FXuint set=getExpanded();
  FXint tsx,bsx,osy;

  FXASSERT(0<=fhor && fhor<=10000);
  FXASSERT(0<=fver && fver<=10000);

  // Proposed split location
  splitx=(fhor*(width-barsize))/10000;
  splity=(fver*(height-barsize))/10000;

  tsx=bsx=splitx;
  osy=splity;

  switch(set){
    case ExpandTopLeft: tsx=bsx=width; osy=height; break;
    case ExpandTopRight: tsx=bsx=-barsize; osy=height; break;
    case ExpandBottomRight: tsx=bsx=-barsize; osy=-barsize; break;
    case ExpandBottomLeft: tsx=bsx=width; osy=-barsize; break;

    case ExpandTopLeft|ExpandTopRight: tsx=bsx=splitx; osy=height; break;
    case ExpandBottomLeft|ExpandBottomRight: tsx=bsx=splitx; osy=-barsize; break;

    case ExpandBottomLeft|ExpandTopLeft: tsx=bsx=width; osy=splity; break;
    case ExpandBottomLeft|ExpandTopRight: tsx=-barsize; bsx=width; osy=splity; break;
    case ExpandBottomRight|ExpandTopLeft: tsx=width; bsx=-barsize; osy=splity; break;
    case ExpandBottomRight|ExpandTopRight: tsx=bsx=-barsize; osy=splity; break;

    case ExpandBottomLeft|ExpandTopLeft|ExpandTopRight: tsx=splitx; bsx=width; osy=splity; break;
    case ExpandBottomRight|ExpandTopLeft|ExpandTopRight: tsx=splitx; bsx=-barsize; osy=splity; break;
    case ExpandTopLeft|ExpandBottomLeft|ExpandBottomRight: tsx=width; bsx=splitx; osy=splity; break;
    case ExpandTopRight|ExpandBottomLeft|ExpandBottomRight: tsx=-barsize; bsx=splitx; osy=splity; break;

    case ExpandTopLeft|ExpandBottomLeft|ExpandTopRight|ExpandBottomRight: tsx=bsx=splitx; osy=splity; break;
    }

  // Arrange the kids
  if(tl) tl->position(0,0,tsx,osy);
  if(tr) tr->position(tsx+barsize,0,width-tsx-barsize,osy);
  if(bl) bl->position(0,osy+barsize,bsx,height-osy-barsize);
  if(br) br->position(bsx+barsize,osy+barsize,width-bsx-barsize,height-osy-barsize);

  // Layout ok now
  flags&=~FLAG_DIRTY;
  }


// Determine split mode
FXuchar FX4Splitter::getMode(FXint x,FXint y){
  register FXuchar mm=ONCENTER;
  if(x<splitx) mm&=~ONVERTICAL;
  if(y<splity) mm&=~ONHORIZONTAL;
  if(x>=splitx+barsize) mm&=~ONVERTICAL;
  if(y>=splity+barsize) mm&=~ONHORIZONTAL;
  return mm;
  }


// Move the split intelligently
void FX4Splitter::moveSplit(FXint x,FXint y){
  if(x<0) x=0;
  if(y<0) y=0;
  if(x>width-barsize) x=width-barsize;
  if(y>height-barsize) y=height-barsize;
  splitx=x;
  splity=y;
  }


// Draw the horizontal split
void FX4Splitter::drawSplit(FXint x,FXint y,FXuint m){
  FXDCWindow dc(this);
  dc.clipChildren(FALSE);
  dc.setFunction(BLT_NOT_DST);
  if(m&ONVERTICAL){
    dc.fillRectangle(x,0,barsize,height);
    }
  if(m&ONHORIZONTAL){
    dc.fillRectangle(0,y,width,barsize);
    }
  }


// Adjust layout
void FX4Splitter::adjustLayout(){
  fhor=(width>barsize) ? (10000*splitx+(width-barsize-1))/(width-barsize) : 0;
  fver=(height>barsize) ? (10000*splity+(height-barsize-1))/(height-barsize) : 0;
  recalc();
  }


// Button being pressed
long FX4Splitter::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    mode=getMode(ev->win_x,ev->win_y);
    if(mode){
      offx=ev->win_x-splitx;
      offy=ev->win_y-splity;
      if(!(options&FOURSPLITTER_TRACKING)){
        drawSplit(splitx,splity,mode);
        }
      flags&=~FLAG_UPDATE;
      }
    return 1;
    }
  return 0;
  }


// Button being released
long FX4Splitter::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint f=flags;
  FXuint m=mode;
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_CHANGED;
    mode=NOWHERE;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(m){
      if(!(options&FOURSPLITTER_TRACKING)){
        drawSplit(splitx,splity,m);
        adjustLayout();
        if(f&FLAG_CHANGED){
          if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
          }
        }
      if(f&FLAG_CHANGED){
        if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),NULL);
        }
      }
    return 1;
    }
  return 0;
  }


// Button being released
long FX4Splitter::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  FXint oldsplitx=splitx;
  FXint oldsplity=splity;
  switch(mode){
    case ONCENTER:
      moveSplit(ev->win_x-offx,ev->win_y-offy);
      break;
    case ONVERTICAL:
      moveSplit(ev->win_x-offx,splity);
      break;
    case ONHORIZONTAL:
      moveSplit(splitx,ev->win_y-offy);
      break;
    default:
      switch(getMode(ev->win_x,ev->win_y)){
        case ONCENTER:
          setDefaultCursor(getApp()->getDefaultCursor(DEF_XSPLIT_CURSOR));
          setDragCursor(getApp()->getDefaultCursor(DEF_XSPLIT_CURSOR));
          break;
        case ONVERTICAL:
          setDefaultCursor(getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR));
          setDragCursor(getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR));
          break;
        case ONHORIZONTAL:
          setDefaultCursor(getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR));
          setDragCursor(getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR));
          break;
        default:
          setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
          setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
          break;
        }
      return 1;
    }
  if((oldsplitx!=splitx) || (oldsplity!=splity)){
    flags|=FLAG_CHANGED;
    if(!(options&FOURSPLITTER_TRACKING)){
      drawSplit(oldsplitx,oldsplity,mode);
      drawSplit(splitx,splity,mode);
      }
    else{
      adjustLayout();
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
      }
    }
  return 1;
  }


// Focus moved up
long FX4Splitter::onFocusUp(FXObject*,FXSelector,void* ptr){
  FXWindow *child=NULL;
  if(getFocus()){
    if(getFocus()==getBottomLeft()) child=getTopLeft();
    else if(getFocus()==getBottomRight()) child=getTopRight();
    }
  else{
    child=getLast();
    }
  if(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
      }
    }
  return 0;
  }


// Focus moved down
long FX4Splitter::onFocusDown(FXObject*,FXSelector,void* ptr){
  FXWindow *child=NULL;
  if(getFocus()){
    if(getFocus()==getTopLeft()) child=getBottomLeft();
    else if(getFocus()==getTopRight()) child=getBottomRight();
    }
  else{
    child=getFirst();
    }
  if(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
      }
    }
  return 0;
  }


// Focus moved to left
long FX4Splitter::onFocusLeft(FXObject*,FXSelector,void* ptr){
  FXWindow *child=NULL;
  if(getFocus()){
    if(getFocus()==getTopRight()) child=getTopLeft();
    else if(getFocus()==getBottomRight()) child=getBottomLeft();
    }
  else{
    child=getLast();
    }
  if(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
      }
    }
  return 0;
  }


// Focus moved to right
long FX4Splitter::onFocusRight(FXObject*,FXSelector,void* ptr){
  FXWindow *child=NULL;
  if(getFocus()){
    if(getFocus()==getTopLeft()) child=getTopRight();
    else if(getFocus()==getBottomLeft()) child=getBottomRight();
    }
  else{
    child=getFirst();
    }
  if(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
      }
    }
  return 0;
  }


// Show the pane(s)
long FX4Splitter::onCmdExpand(FXObject*,FXSelector sel,void*){
  FXuint ex=FXSELID(sel)-ID_EXPAND_NONE;
  setExpanded(ex);
  return 1;
  }


// Update show pane
long FX4Splitter::onUpdExpand(FXObject* sender,FXSelector sel,void*){
  register FXuint ex=FXSELID(sel)-ID_EXPAND_NONE;
  sender->handle(this,(getExpanded()==ex)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change horizontal split [fraction*10000]
void FX4Splitter::setHSplit(FXint s){
  if(s<0) s=0;
  if(s>10000) s=10000;
  if(s!=fhor){
    fhor=s;
    recalc();
    }
  }


// Change vertical split [fraction*10000]
void FX4Splitter::setVSplit(FXint s){
  if(s<0) s=0;
  if(s>10000) s=10000;
  if(s!=fver){
    fver=s;
    recalc();
    }
  }


// Save object to stream
void FX4Splitter::save(FXStream& store) const {
  FXComposite::save(store);
  store << barsize;
  store << fhor;
  store << fver;
  }



// Load object from stream
void FX4Splitter::load(FXStream& store){
  FXComposite::load(store);
  store >> barsize;
  store >> fhor;
  store >> fver;
  }


// Return splitter style
FXuint FX4Splitter::getSplitterStyle() const {
  return (options&FOURSPLITTER_MASK);
  }


// Change mode
void FX4Splitter::setSplitterStyle(FXuint style){
  options=(options&~FOURSPLITTER_MASK) | (style&FOURSPLITTER_MASK);
  }


// Expand one or all of the four panes
void FX4Splitter::setExpanded(FXuint set){
  FXWindow *tl=getTopLeft();
  FXWindow *tr=getTopRight();
  FXWindow *bl=getBottomLeft();
  FXWindow *br=getBottomRight();
  if(tl){ if(set&ExpandTopLeft) tl->show(); else tl->hide(); }
  if(tr){ if(set&ExpandTopRight) tr->show(); else tr->hide(); }
  if(bl){ if(set&ExpandBottomLeft) bl->show(); else bl->hide(); }
  if(br){ if(set&ExpandBottomRight) br->show(); else br->hide(); }
  recalc();
  }


// Get set of expanded children
FXuint FX4Splitter::getExpanded() const {
  FXWindow *tl=getTopLeft();
  FXWindow *tr=getTopRight();
  FXWindow *bl=getBottomLeft();
  FXWindow *br=getBottomRight();
  FXuint set=0;
  if(tl && tl->shown()) set|=ExpandTopLeft;
  if(tr && tr->shown()) set|=ExpandTopRight;
  if(bl && bl->shown()) set|=ExpandBottomLeft;
  if(br && br->shown()) set|=ExpandBottomRight;
  return set;
  }


// Change bar size
void FX4Splitter::setBarSize(FXint bs){
  if(bs<1) bs=1;
  if(bs!=barsize){
    barsize=bs;
    recalc();
    }
  }

}
