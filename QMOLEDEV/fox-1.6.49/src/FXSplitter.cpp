/********************************************************************************
*                                                                               *
*                S p l i t t e r   W i n d o w   O b j e c t                    *
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
* $Id: FXSplitter.cpp,v 1.55 2006/02/20 03:32:13 fox Exp $                      *
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
#include "FXSplitter.h"


/*
  Notes:
  - Reversed split option starts parting out from right [bottom].
  - Minimal partition of a split is 0 pixels.
  - Minimum width of horizontal splitter is determined by sum of
    all visible children's default widths, and height by maximum
    of all visible children's default heights; analogous for vertical
    splitter of course.
    [This can be done because the default width (height) does not depend
    on the widget's current user-specified actual width (height)].
  - For convenience, width/height of <=1 replaced by minimum width/height,
    but only if both width and height of child is <=1 at the same time.
  - Should we send SEL_CHANGED and SEL_COMMAND also when splitter arrangement
    was changed programmatically?  When bar size changed?
  - FXSplitter interprets layout hints on some of the children so that certain
    children have a minimum width (height) constraint on them.
    Note that this rule must have one exception: the last (or first if the option
    SPLITTER_REVERSED has been passed) must necessarily be allowed to be
    any size.
  - If we're just re-sizing a split, do we need to incur a GUI-Update?
  - Do we need to somehow insure that the sum of the sizes of all
    partitions never exceeds the size of the splitter itself?
  - Should we drop default parameters on 2nd constructor so as to
    prevent conflicts on compilers where NULL is defined as 0 instead
    of __null?
*/

// Splitter styles
#define SPLITTER_MASK     (SPLITTER_REVERSED|SPLITTER_VERTICAL|SPLITTER_TRACKING)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXSplitter) FXSplitterMap[]={
  FXMAPFUNC(SEL_MOTION,0,FXSplitter::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXSplitter::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXSplitter::onLeftBtnRelease),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXSplitter::onFocusNext),
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXSplitter::onFocusPrev),
  FXMAPFUNC(SEL_FOCUS_UP,0,FXSplitter::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXSplitter::onFocusDown),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FXSplitter::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FXSplitter::onFocusRight),
  };


// Object implementation
FXIMPLEMENT(FXSplitter,FXComposite,FXSplitterMap,ARRAYNUMBER(FXSplitterMap))


// Make a splitter
FXSplitter::FXSplitter(){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  window=NULL;
  split=0;
  offset=0;
  barsize=4;
  }


// Make a splitter; it has no interior padding, and no borders
FXSplitter::FXSplitter(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXComposite(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  defaultCursor=(options&SPLITTER_VERTICAL) ? getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR) : getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR);
  dragCursor=defaultCursor;
  window=NULL;
  split=0;
  offset=0;
  barsize=4;
  }


// Make a splitter; it has no interior padding, and no borders
FXSplitter::FXSplitter(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXComposite(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  defaultCursor=(options&SPLITTER_VERTICAL) ? getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR) : getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR);
  dragCursor=defaultCursor;
  target=tgt;
  message=sel;
  window=NULL;
  split=0;
  offset=0;
  barsize=4;
  }


// Get default width
FXint FXSplitter::getDefaultWidth(){
  register FXWindow* child;
  register FXint wmax,w,numc;
  wmax=numc=0;
  if(options&SPLITTER_VERTICAL){
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        w=child->getDefaultWidth();
        if(wmax<w) wmax=w;
        }
      }
    }
  else{
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        wmax+=child->getDefaultWidth();
        numc++;
        }
      }
    if(numc>1) wmax+=(numc-1)*barsize;
    }
  return wmax;
  }


// Get default height
FXint FXSplitter::getDefaultHeight(){
  register FXWindow* child;
  register FXint hmax,h,numc;
  hmax=numc=0;
  if(options&SPLITTER_VERTICAL){
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hmax+=child->getDefaultHeight();
        numc++;
        }
      }
    if(numc>1) hmax+=(numc-1)*barsize;
    }
  else{
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        h=child->getDefaultHeight();
        if(hmax<h) hmax=h;
        }
      }
    }
  return hmax;
  }


// Recompute layout
void FXSplitter::layout(){
  FXWindow *child,*stretcher;
  FXint pos,w,h,t;
  FXuint hints;
  if(options&SPLITTER_VERTICAL){          // Vertical
    if(options&SPLITTER_REVERSED){
      pos=height;
      child=getLast();
      stretcher=getFirst();
      while(stretcher && !stretcher->shown()){
        stretcher=stretcher->getNext();
        }
      while(child){
        if(child->shown()){
          w=child->getWidth();
          h=child->getHeight();
          hints=child->getLayoutHints();
          if((hints&LAYOUT_FILL_Y)&&(hints&LAYOUT_FIX_HEIGHT)){
            if((t=child->getDefaultHeight())>h) h=t;
            }
          if(w<=1 && h<=1) h=child->getDefaultHeight();
          if(child==stretcher){ h=pos; if(h<0) h=0; }
          child->position(0,pos-h,width,h);
          pos=pos-h-barsize;
          }
        child=child->getPrev();
        }
      }
    else{
      pos=0;
      child=getFirst();
      stretcher=getLast();
      while(stretcher && !stretcher->shown()){
        stretcher=stretcher->getPrev();
        }
      while(child){
        if(child->shown()){
          w=child->getWidth();
          h=child->getHeight();
          hints=child->getLayoutHints();
          if((hints&LAYOUT_FILL_Y)&&(hints&LAYOUT_FIX_HEIGHT)){
            if((t=child->getDefaultHeight())>h) h=t;
            }
          if(w<=1 && h<=1) h=child->getDefaultHeight();
          if(child==stretcher){ h=height-pos; if(h<0) h=0; }
          child->position(0,pos,width,h);
          pos=pos+h+barsize;
          }
        child=child->getNext();
        }
      }
    }
  else{                                   // Horizontal
    if(options&SPLITTER_REVERSED){
      pos=width;
      child=getLast();
      stretcher=getFirst();
      while(stretcher && !stretcher->shown()){
        stretcher=stretcher->getNext();
        }
      while(child){
        if(child->shown()){
          w=child->getWidth();
          h=child->getHeight();
          hints=child->getLayoutHints();
          if((hints&LAYOUT_FILL_X)&&(hints&LAYOUT_FIX_WIDTH)){
            if((t=child->getDefaultWidth())>w) w=t;
            }
          if(w<=1 && h<=1) w=child->getDefaultWidth();
          if(child==stretcher){ w=pos; if(w<0) w=0; }
          child->position(pos-w,0,w,height);
          pos=pos-w-barsize;
          }
        child=child->getPrev();
        }
      }
    else{
      pos=0;
      child=getFirst();
      stretcher=getLast();
      while(stretcher && !stretcher->shown()){
        stretcher=stretcher->getPrev();
        }
      while(child){
        if(child->shown()){
          w=child->getWidth();
          h=child->getHeight();
          hints=child->getLayoutHints();
          if((hints&LAYOUT_FILL_X)&&(hints&LAYOUT_FIX_WIDTH)){
            if((t=child->getDefaultWidth())>w) w=t;
            }
          if(w<=1 && h<=1) w=child->getDefaultWidth();
          if(child==stretcher){ w=width-pos; if(w<0) w=0; }
          child->position(pos,0,w,height);
          pos=pos+w+barsize;
          }
        child=child->getNext();
        }
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Adjust horizontal layout
void FXSplitter::adjustHLayout(){
  FXWindow *child,*stretcher;
  FXint w,h,pos;
  FXASSERT(window);
  if(options&SPLITTER_REVERSED){
    pos=window->getX()+window->getWidth();
    window->position(split,0,pos-split,height);
    pos=split-barsize;
    for(stretcher=getFirst(); stretcher && !stretcher->shown(); stretcher=stretcher->getNext());
    for(child=window->getPrev(); child; child=child->getPrev()){
      if(child->shown()){
        w=child->getWidth();
        h=child->getHeight();
        if(w<=1 && h<=1) w=child->getDefaultWidth();
        if(child==stretcher){ w=pos; if(w<0) w=0; }
        child->position(pos-w,0,w,height);
        pos=pos-w-barsize;
        }
      }
    }
  else{
    pos=window->getX();
    window->position(pos,0,split-pos,height);
    pos=split+barsize;
    for(stretcher=getLast(); stretcher && !stretcher->shown(); stretcher=stretcher->getPrev());
    for(child=window->getNext(); child; child=child->getNext()){
      if(child->shown()){
        w=child->getWidth();
        h=child->getHeight();
        if(w<=1 && h<=1) w=child->getDefaultWidth();
        if(child==stretcher){ w=width-pos; if(w<0) w=0; }
        child->position(pos,0,w,height);
        pos=pos+w+barsize;
        }
      }
    }
  }


// Adjust vertical layout
void FXSplitter::adjustVLayout(){
  FXWindow *child,*stretcher;
  FXint w,h,pos;
  FXASSERT(window);
  if(options&SPLITTER_REVERSED){
    pos=window->getY()+window->getHeight();
    window->position(0,split,width,pos-split);
    pos=split-barsize;
    for(stretcher=getFirst(); stretcher && !stretcher->shown(); stretcher=stretcher->getNext());
    for(child=window->getPrev(); child; child=child->getPrev()){
      if(child->shown()){
        w=child->getWidth();
        h=child->getHeight();
        if(w<=1 && h<=1) h=child->getDefaultHeight();
        if(child==stretcher){ h=pos; if(h<0) h=0; }
        child->position(0,pos-h,width,h);
        pos=pos-h-barsize;
        }
      }
    }
  else{
    pos=window->getY();
    window->position(0,pos,width,split-pos);
    pos=split+barsize;
    for(stretcher=getLast(); stretcher && !stretcher->shown(); stretcher=stretcher->getPrev());
    for(child=window->getNext(); child; child=child->getNext()){
      if(child->shown()){
        w=child->getWidth();
        h=child->getHeight();
        if(w<=1 && h<=1) h=child->getDefaultHeight();
        if(child==stretcher){ h=height-pos; if(h<0) h=0; }
        child->position(0,pos,width,h);
        pos=pos+h+barsize;
        }
      }
    }
  }


// Find child just before split
FXWindow* FXSplitter::findHSplit(FXint pos){
  register FXWindow* child=getFirst();
  if(options&SPLITTER_REVERSED){
    while(child){
      if(child->shown()){
        if(child->getX()-barsize<=pos && pos<child->getX()) return child;
        }
      child=child->getNext();
      }
    }
  else{
    while(child){
      if(child->shown()){
        if(child->getX()+child->getWidth()<=pos && pos<child->getX()+child->getWidth()+barsize) return child;
        }
      child=child->getNext();
      }
    }
  return NULL;
  }


// Find child just before split
FXWindow* FXSplitter::findVSplit(FXint pos){
  register FXWindow* child=getFirst();
  if(options&SPLITTER_REVERSED){
    while(child){
      if(child->shown()){
        if(child->getY()-barsize<=pos && pos<child->getY()) return child;
        }
      child=child->getNext();
      }
    }
  else{
    while(child){
      if(child->shown()){
        if(child->getY()+child->getHeight()<=pos && pos<child->getY()+child->getHeight()+barsize) return child;
        }
      child=child->getNext();
      }
    }
  return NULL;
  }


// Move the horizontal split intelligently
void FXSplitter::moveHSplit(FXint pos){
  register FXint smin,smax;
  register FXuint hints;
  FXASSERT(window);
  hints=window->getLayoutHints();
  if(options&SPLITTER_REVERSED){
    smin=barsize;
    smax=window->getX()+window->getWidth();
    if((hints&LAYOUT_FILL_X)&&(hints&LAYOUT_FIX_WIDTH)) smax-=window->getDefaultWidth();
    }
  else{
    smin=window->getX();
    smax=width-barsize;
    if((hints&LAYOUT_FILL_X)&&(hints&LAYOUT_FIX_WIDTH)) smin+=window->getDefaultWidth();
    }
  split=pos;
  if(split<smin) split=smin;
  if(split>smax) split=smax;
  }


// Move the vertical split intelligently
void FXSplitter::moveVSplit(FXint pos){
  register FXint smin,smax;
  register FXuint hints;
  FXASSERT(window);
  hints=window->getLayoutHints();
  if(options&SPLITTER_REVERSED){
    smin=barsize;
    smax=window->getY()+window->getHeight();
    if((hints&LAYOUT_FILL_Y)&&(hints&LAYOUT_FIX_HEIGHT)) smax-=window->getDefaultHeight();
    }
  else{
    smin=window->getY();
    smax=height-barsize;
    if((hints&LAYOUT_FILL_Y)&&(hints&LAYOUT_FIX_HEIGHT)) smin+=window->getDefaultHeight();
    }
  split=pos;
  if(split<smin) split=smin;
  if(split>smax) split=smax;
  }


// Button being pressed
long FXSplitter::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(options&SPLITTER_VERTICAL){
      window=findVSplit(ev->win_y);
      if(window){
        if(options&SPLITTER_REVERSED)
          split=window->getY();
        else
          split=window->getY()+window->getHeight();
        offset=ev->win_y-split;
        if(!(options&SPLITTER_TRACKING)){
          drawVSplit(split);
          }
        flags|=FLAG_PRESSED;
        flags&=~FLAG_UPDATE;
        }
      }
    else{
      window=findHSplit(ev->win_x);
      if(window){
        if(options&SPLITTER_REVERSED)
          split=window->getX();
        else
          split=window->getX()+window->getWidth();
        offset=ev->win_x-split;
        if(!(options&SPLITTER_TRACKING)){
          drawHSplit(split);
          }
        flags|=FLAG_PRESSED;
        flags&=~FLAG_UPDATE;
        }
      }
    return 1;
    }
  return 0;
  }


// Button being released
long FXSplitter::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint flgs=flags;
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_CHANGED;
    flags&=~FLAG_PRESSED;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(flgs&FLAG_PRESSED){
      if(!(options&SPLITTER_TRACKING)){
        if(options&SPLITTER_VERTICAL){
          drawVSplit(split);
          adjustVLayout();
          }
        else{
          drawHSplit(split);
          adjustHLayout();
          }
        if(flgs&FLAG_CHANGED){
          if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),window);
          }
        }
      if(flgs&FLAG_CHANGED){
        if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),window);
        }
      }
    return 1;
    }
  return 0;
  }


// Button being released
long FXSplitter::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  FXint oldsplit;
  if(flags&FLAG_PRESSED){
    oldsplit=split;
    if(options&SPLITTER_VERTICAL){
      moveVSplit(ev->win_y-offset);
      if(split!=oldsplit){
        if(!(options&SPLITTER_TRACKING)){
          drawVSplit(oldsplit);
          drawVSplit(split);
          }
        else{
          adjustVLayout();
          if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),window);
          }
        flags|=FLAG_CHANGED;
        }
      }
    else{
      moveHSplit(ev->win_x-offset);
      if(split!=oldsplit){
        if(!(options&SPLITTER_TRACKING)){
          drawHSplit(oldsplit);
          drawHSplit(split);
          }
        else{
          adjustHLayout();
          if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),window);
          }
        flags|=FLAG_CHANGED;
        }
      }
    return 1;
    }
  return 0;
  }


// Focus moved to next
long FXSplitter::onFocusNext(FXObject* sender,FXSelector sel,void* ptr){
  return (options&SPLITTER_VERTICAL) ? onFocusDown(sender,sel,ptr) : onFocusRight(sender,sel,ptr);
  }


// Focus moved to previous
long FXSplitter::onFocusPrev(FXObject* sender,FXSelector sel,void* ptr){
  return (options&SPLITTER_VERTICAL) ? onFocusUp(sender,sel,ptr) : onFocusLeft(sender,sel,ptr);
  }


// Focus moved up
long FXSplitter::onFocusUp(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getLast();
  if(getFocus()){
    if(getFocus()->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
    if(!(options&SPLITTER_VERTICAL)) return 0;
    child=getFocus()->getPrev();
    }
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
      }
    child=child->getPrev();
    }
  return 0;
  }


// Focus moved down
long FXSplitter::onFocusDown(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getFirst();
  if(getFocus()){
    if(getFocus()->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
    if(!(options&SPLITTER_VERTICAL)) return 0;
    child=getFocus()->getNext();
    }
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
      }
    child=child->getNext();
    }
  return 0;
  }


// Focus moved to left
long FXSplitter::onFocusLeft(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getLast();
  if(getFocus()){
    if(getFocus()->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
    if(options&SPLITTER_VERTICAL) return 0;
    child=getFocus()->getPrev();
    }
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
      }
    child=child->getPrev();
    }
  return 0;
  }


// Focus moved to right
long FXSplitter::onFocusRight(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getFirst();
  if(getFocus()){
    if(getFocus()->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
    if(options&SPLITTER_VERTICAL) return 0;
    child=getFocus()->getNext();
    }
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
      }
    child=child->getNext();
    }
  return 0;
  }


// Draw the horizontal split
void FXSplitter::drawHSplit(FXint pos){
  FXDCWindow dc(this);
  dc.clipChildren(FALSE);
  dc.setFunction(BLT_NOT_DST);   
  dc.fillRectangle(pos,0,barsize,height);
  }


// Draw the vertical split
void FXSplitter::drawVSplit(FXint pos){
  FXDCWindow dc(this);
  dc.clipChildren(FALSE);
  dc.setFunction(BLT_NOT_DST);  
  dc.fillRectangle(0,pos,width,barsize);
  }


// Return size of the panel at index
FXint FXSplitter::getSplit(FXint index) const {
  FXWindow *win=childAtIndex(index);
  if(win){
    if(options&SPLITTER_VERTICAL){
      return win->getHeight();
      }
    else{
      return win->getWidth();
      }
    }
  return 0;
  }


// Change the size of panel at the given index
void FXSplitter::setSplit(FXint index,FXint size){
  FXWindow *win=childAtIndex(index);
  if(win){
    if(options&SPLITTER_VERTICAL){
      win->setHeight(size);
      }
    else{
      win->setWidth(size);
      }
    win->recalc();
    }
  }


// Return splitter style
FXuint FXSplitter::getSplitterStyle() const {
  return (options&SPLITTER_MASK);
  }


// Set horizontal or vertical
void FXSplitter::setSplitterStyle(FXuint style){
  FXuint opts=(options&~SPLITTER_MASK) | (style&SPLITTER_MASK);
  if(options!=opts){

    // Split direction changed; need re-layout of everything
    if((opts&SPLITTER_VERTICAL)!=(options&SPLITTER_VERTICAL)){
      for(FXWindow *child=getFirst(); child; child=child->getNext()){
        if(child->shown()){
          child->setWidth(0);
          child->setHeight(0);
          }
        }
      setDefaultCursor((opts&SPLITTER_VERTICAL) ? getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR) : getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR));
      setDragCursor(getDefaultCursor());
      recalc();
      }

    // Split mode reversal; re-layout first and last only
    if((opts&SPLITTER_REVERSED)!=(options&SPLITTER_REVERSED)){
      if(getFirst()){
        getFirst()->setWidth(0);
        getFirst()->setHeight(0);
        getLast()->setWidth(0);
        getLast()->setHeight(0);
        }
      recalc();
      }
    options=opts;
    }
  }


// Change bar size
void FXSplitter::setBarSize(FXint bs){
  if(bs!=barsize){
    barsize=bs;
    recalc();
    }
  }


// Save object to stream
void FXSplitter::save(FXStream& store) const {
  FXComposite::save(store);
  store << barsize;
  }


// Load object from stream
void FXSplitter::load(FXStream& store){
  FXComposite::load(store);
  store >> barsize;
  }


// Zap it
FXSplitter::~FXSplitter(){
  window=(FXWindow*)-1L;
  }

}
