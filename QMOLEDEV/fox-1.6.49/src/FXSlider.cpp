/********************************************************************************
*                                                                               *
*                           S l i d e r   W i d g e t                           *
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
* $Id: FXSlider.cpp,v 1.65.2.2 2007/08/09 00:37:06 fox Exp $                        *
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
#include "FXDCWindow.h"
#include "FXSlider.h"




/*
  Notes:
  - Maybe add bindings for arrow keys for value changes.
*/

#define TICKSIZE        4           // Length of ticks
#define OVERHANG        4           // Default amount of overhang
#define MINOVERHANG     3           // Minimal amount of overhang
#define HEADINSIDEBAR   20          // Default for inside bar head size
#define HEADOVERHANGING 9           // Default for overhanging head size

#define SLIDER_MASK (SLIDER_VERTICAL|SLIDER_ARROW_UP|SLIDER_ARROW_DOWN|SLIDER_INSIDE_BAR|SLIDER_TICKS_TOP|SLIDER_TICKS_BOTTOM)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXSlider) FXSliderMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXSlider::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXSlider::onMotion),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXSlider::onMouseWheel),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXSlider::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXSlider::onLeftBtnRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXSlider::onMiddleBtnPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXSlider::onMiddleBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXSlider::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXSlider::onKeyRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXSlider::onUngrabbed),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXSlider::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXSlider::onQueryHelp),
  FXMAPFUNC(SEL_TIMEOUT,FXSlider::ID_AUTOSLIDE,FXSlider::onAutoSlide),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETVALUE,FXSlider::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETINTVALUE,FXSlider::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETREALVALUE,FXSlider::onCmdSetRealValue),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_GETINTVALUE,FXSlider::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_GETREALVALUE,FXSlider::onCmdGetRealValue),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETINTRANGE,FXSlider::onCmdSetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_GETINTRANGE,FXSlider::onCmdGetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETREALRANGE,FXSlider::onCmdSetRealRange),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_GETREALRANGE,FXSlider::onCmdGetRealRange),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETHELPSTRING,FXSlider::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_GETHELPSTRING,FXSlider::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_SETTIPSTRING,FXSlider::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXSlider::ID_GETTIPSTRING,FXSlider::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXSlider,FXFrame,FXSliderMap,ARRAYNUMBER(FXSliderMap))


// Make a slider
FXSlider::FXSlider(){
  flags|=FLAG_ENABLED;
  range[0]=0;
  range[1]=0;
  pos=0;
  incr=1;
  delta=0;
  headpos=0;
  headsize=0;
  slotsize=0;
  slotColor=0;
  dragpoint=0;
  }


// Make a slider
FXSlider::FXSlider(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  baseColor=getApp()->getBaseColor();
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  borderColor=getApp()->getBorderColor();
  slotColor=getApp()->getBackColor();
  target=tgt;
  message=sel;
  range[0]=0;
  range[1]=100;
  pos=50;
  incr=1;
  delta=0;
  headpos=0;
  headsize=(options&SLIDER_INSIDE_BAR)?HEADINSIDEBAR:HEADOVERHANGING;
  slotsize=5;
  dragpoint=0;
  }


// Enable the window
void FXSlider::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXFrame::enable();
    update();
    }
  }


// Disable the window
void FXSlider::disable(){
  if(flags&FLAG_ENABLED){
    FXFrame::disable();
    update();
    }
  }


// Get default size
FXint FXSlider::getDefaultWidth(){
  FXint w;
  if(options&SLIDER_VERTICAL){
    if(options&SLIDER_INSIDE_BAR) w=4+headsize/2;
    else if(options&(SLIDER_ARROW_LEFT|SLIDER_ARROW_RIGHT)) w=slotsize+MINOVERHANG*2+headsize/2;
    else w=slotsize+MINOVERHANG*2;
    if(options&SLIDER_TICKS_LEFT) w+=TICKSIZE;
    if(options&SLIDER_TICKS_RIGHT) w+=TICKSIZE;
    }
  else{
    w=headsize+4;
    }
  return w+padleft+padright+(border<<1);
  }


FXint FXSlider::getDefaultHeight(){
  FXint h;
  if(options&SLIDER_VERTICAL){
    h=headsize+4;
    }
  else{
    if(options&SLIDER_INSIDE_BAR) h=4+headsize/2;
    else if(options&(SLIDER_ARROW_UP|SLIDER_ARROW_DOWN)) h=slotsize+2*MINOVERHANG+headsize/2;
    else h=slotsize+MINOVERHANG*2;
    if(options&SLIDER_TICKS_TOP) h+=TICKSIZE;
    if(options&SLIDER_TICKS_BOTTOM) h+=TICKSIZE;
    }
  return h+padtop+padbottom+(border<<1);
  }


// Returns true because a slider can receive focus
bool FXSlider::canFocus() const { return true; }


// Layout changed; even though the position is still
// the same, the head may have to be moved.
void FXSlider::layout(){
  setValue(pos);
  flags&=~FLAG_DIRTY;
  }


// Set help using a message
long FXSlider::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXSlider::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXSlider::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXSlider::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXSlider::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXSlider::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Update value from a message
long FXSlider::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXSlider::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setValue(*((FXint*)ptr));
  return 1;
  }


// Update value from a message
long FXSlider::onCmdSetRealValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)*((FXdouble*)ptr));
  return 1;
  }


// Obtain value from text field
long FXSlider::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getValue();
  return 1;
  }


// Obtain value with a message
long FXSlider::onCmdGetRealValue(FXObject*,FXSelector,void* ptr){
  *((FXdouble*)ptr)=(FXdouble)getValue();
  return 1;
  }


// Update range from a message
long FXSlider::onCmdSetIntRange(FXObject*,FXSelector,void* ptr){
  setRange(((FXint*)ptr)[0],((FXint*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXSlider::onCmdGetIntRange(FXObject*,FXSelector,void* ptr){
  ((FXint*)ptr)[0]=range[0];
  ((FXint*)ptr)[1]=range[1];
  return 1;
  }


// Update range from a message
long FXSlider::onCmdSetRealRange(FXObject*,FXSelector,void* ptr){
  setRange((FXint)((FXdouble*)ptr)[0],(FXint)((FXdouble*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXSlider::onCmdGetRealRange(FXObject*,FXSelector,void* ptr){
  ((FXdouble*)ptr)[0]=(FXdouble)range[0];
  ((FXdouble*)ptr)[1]=(FXdouble)range[1];
  return 1;
  }


// Pressed LEFT button
long FXSlider::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint p=pos;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    getApp()->removeTimeout(this,ID_AUTOSLIDE);
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    if(options&SLIDER_VERTICAL){
      if(event->win_y<headpos){
        getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollDelay(),(void*)(FXival)incr);
        p=pos+incr;
        }
      else if(event->win_y>(headpos+headsize)){
        getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollDelay(),(void*)(FXival)-incr);
        p=pos-incr;
        }
      else{
        dragpoint=event->win_y-headpos;
        flags|=FLAG_PRESSED;
        }
      }
    else{
      if(event->win_x<headpos){
        getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollDelay(),(void*)(FXival)-incr);
        p=pos-incr;
        }
      else if(event->win_x>(headpos+headsize)){
        getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollDelay(),(void*)(FXival)incr);
        p=pos+incr;
        }
      else{
        dragpoint=event->win_x-headpos;
        flags|=FLAG_PRESSED;
        }
      }
    if(p<range[0]) p=range[0];
    if(p>range[1]) p=range[1];
    if(p!=pos){
      setValue(p);
      flags|=FLAG_CHANGED;
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Released Left button
long FXSlider::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  register FXuint flgs=flags;
  if(isEnabled()){
    ungrab();
    getApp()->removeTimeout(this,ID_AUTOSLIDE);
    setValue(pos);                                                 // Hop to exact position
    flags&=~FLAG_PRESSED;
    flags&=~FLAG_CHANGED;
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(flgs&FLAG_CHANGED){
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Moving
long FXSlider::onMotion(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint xx,yy,ww,hh,lo,hi,p,h,travel;
  if(!isEnabled()) return 0;
  if(flags&FLAG_PRESSED){
    yy=border+padtop+2;
    xx=border+padleft+2;
    hh=height-(border<<1)-padtop-padbottom-4;
    ww=width-(border<<1)-padleft-padright-4;
    if(options&SLIDER_VERTICAL){
      h=event->win_y-dragpoint;
      travel=hh-headsize;
      if(h<yy) h=yy;
      if(h>yy+travel) h=yy+travel;
      if(h!=headpos){
        FXMINMAX(lo,hi,headpos,h);
        headpos=h;
        update(border,lo-1,width-(border<<1),hi+headsize+2-lo);
        }
      if(travel>0)
        p=range[0]+((range[1]-range[0])*(yy+travel-h)+travel/2)/travel;    // Use rounding!!
      else
        p=range[0];
      }
    else{
      h=event->win_x-dragpoint;
      travel=ww-headsize;
      if(h<xx) h=xx;
      if(h>xx+travel) h=xx+travel;
      if(h!=headpos){
        FXMINMAX(lo,hi,headpos,h);
        headpos=h;
        update(lo-1,border,hi+headsize+2-lo,height-(border<<1));
        }
      if(travel>0)
        p=range[0]+((range[1]-range[0])*(h-xx)+travel/2)/travel;    // Use rounding!!
      else
        p=range[0];
      }
    if(p<range[0]) p=range[0];
    if(p>range[1]) p=range[1];
    if(pos!=p){
      pos=p;
      flags|=FLAG_CHANGED;
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Pressed middle or right
long FXSlider::onMiddleBtnPress(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint xx,yy,ww,hh,lo,hi,p,h,travel;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONPRESS,message),ptr)) return 1;
    dragpoint=headsize/2;
    yy=border+padtop+2;
    xx=border+padleft+2;
    hh=height-(border<<1)-padtop-padbottom-4;
    ww=width-(border<<1)-padleft-padright-4;
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    if(options&SLIDER_VERTICAL){
      h=event->win_y-dragpoint;
      travel=hh-headsize;
      if(h<yy) h=yy;
      if(h>yy+travel) h=yy+travel;
      if(h!=headpos){
        FXMINMAX(lo,hi,headpos,h);
        headpos=h;
        update(border,lo-1,width-(border<<1),hi+headsize+2-lo);
        }
      if(travel>0)
        p=range[0]+((range[1]-range[0])*(yy+travel-h)+travel/2)/travel;    // Use rounding!!
      else
        p=range[0];
      }
    else{
      h=event->win_x-dragpoint;
      travel=ww-headsize;
      if(h<xx) h=xx;
      if(h>xx+travel) h=xx+travel;
      if(h!=headpos){
        FXMINMAX(lo,hi,headpos,h);
        headpos=h;
        update(lo-1,border,hi+headsize+2-lo,height-(border<<1));
        }
      if(travel>0)
        p=range[0]+((range[1]-range[0])*(h-xx)+travel/2)/travel;    // Use rounding!!
      else
        p=range[0];
      }
    if(p<range[0]) p=range[0];
    if(p>range[1]) p=range[1];
    if(p!=pos){
      pos=p;
      flags|=FLAG_CHANGED;
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Released middle button
long FXSlider::onMiddleBtnRelease(FXObject*,FXSelector,void* ptr){
  register FXuint flgs=flags;
  if(isEnabled()){
    ungrab();
    getApp()->removeTimeout(this,ID_AUTOSLIDE);
    flags&=~FLAG_PRESSED;
    flags&=~FLAG_CHANGED;
    flags|=FLAG_UPDATE;
    setValue(pos);                                                 // Hop to exact position
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONRELEASE,message),ptr)) return 1;
    if(flgs&FLAG_CHANGED){
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Mouse wheel
long FXSlider::onMouseWheel(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint p=pos+(event->code*incr)/120;
  if(p<range[0]) p=range[0];
  if(p>range[1]) p=range[1];
  if(pos!=p){
    setValue(p);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
    }
  return 1;
  }


// Keyboard press
long FXSlider::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    switch(event->code){
      case KEY_Left:
      case KEY_KP_Left:
        if(!(options&SLIDER_VERTICAL)) goto dec;
        break;
      case KEY_Right:
      case KEY_KP_Right:
        if(!(options&SLIDER_VERTICAL)) goto inc;
        break;
      case KEY_Up:
      case KEY_KP_Up:
        if(options&SLIDER_VERTICAL) goto inc;
        break;
      case KEY_Down:
      case KEY_KP_Down:
        if(options&SLIDER_VERTICAL) goto dec;
        break;
      case KEY_plus:
      case KEY_KP_Add:
inc:    setValue(pos+incr,true);
        return 1;
      case KEY_minus:
      case KEY_KP_Subtract:
dec:    setValue(pos-incr,true);
        return 1;
      }
    }
  return 0;
  }


// Keyboard release
long FXSlider::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_Left:
      case KEY_KP_Left:
      case KEY_Right:
      case KEY_KP_Right:
        if(!(options&SLIDER_VERTICAL)) return 1;
        break;
      case KEY_Up:
      case KEY_KP_Up:
      case KEY_Down:
      case KEY_KP_Down:
        if(options&SLIDER_VERTICAL) return 1;
        break;
      case KEY_plus:
      case KEY_KP_Add:
      case KEY_KP_Subtract:
      case KEY_minus:
        return 1;
      }
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXSlider::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_AUTOSLIDE);
  flags&=~FLAG_PRESSED;
  flags&=~FLAG_CHANGED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Automatically move slider while holding down mouse
long FXSlider::onAutoSlide(FXObject*,FXSelector,void* ptr){
  register FXint inc=(FXint)(FXival)ptr;
  register FXint p=pos+inc;
  if(p<=range[0]){
    p=range[0];
    }
  else if(p>=range[1]){
    p=range[1];
    }
  else{
    getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollSpeed(),(void*)(FXival)inc);
    }
  if(p!=pos){
    setValue(p);
    flags|=FLAG_CHANGED;
    if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
    return 1;
    }
  return 0;
  }


// Draw horizontal ticks
void FXSlider::drawHorzTicks(FXDCWindow& dc,FXint,FXint y,FXint,FXint){
  register FXint interval=range[1]-range[0];
  register FXint travel,offset,v,d,p;
  if(0<interval){
    d=delta;
    if(d<=0) d=incr;
    dc.setForeground(FXRGB(0,0,0));
    travel=width-(border<<1)-padleft-padright-headsize-4;
    offset=border+padleft+2+headsize/2;
    for(v=range[0]; v<=range[1]; v+=d){
      p=offset+(travel*(v-range[0]))/interval;
      dc.fillRectangle(p,y,1,TICKSIZE);
      }
    }
  }


// Draw vertical ticks
void FXSlider::drawVertTicks(FXDCWindow& dc,FXint x,FXint,FXint,FXint){
  register FXint interval=range[1]-range[0];
  register FXint travel,offset,v,d,p;
  if(0<interval){
    d=delta;
    if(d<=0) d=incr;
    dc.setForeground(FXRGB(0,0,0));
    travel=height-(border<<1)-padtop-padbottom-headsize-4;
    offset=height-border-padbottom-2-headsize/2;
    for(v=range[0]; v<=range[1]; v+=d){
      p=offset-(travel*(v-range[0]))/interval;
      dc.fillRectangle(x,p,TICKSIZE,1);
      }
    }
  }


// Draw slider head
void FXSlider::drawSliderHead(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  FXint m;
  dc.setForeground(baseColor);
  dc.fillRectangle(x,y,w,h);
  if(options&SLIDER_VERTICAL){
    m=(h>>1);
    if(options&SLIDER_ARROW_LEFT){
      dc.setForeground(hiliteColor);
      dc.drawLine(x+m,y,x+w-1,y);
      dc.drawLine(x,y+m,x+m,y);
      dc.setForeground(shadowColor);
      dc.drawLine(x+1,y+h-m-1,x+m+1,y+h-1);
      dc.drawLine(x+m,y+h-2,x+w-1,y+h-2);
      dc.drawLine(x+w-2,y+1,x+w-2,y+h-1);
      dc.setForeground(borderColor);
      dc.drawLine(x,y+h-m-1,x+m,y+h-1);
      dc.drawLine(x+w-1,y+h-1,x+w-1,y);
      dc.fillRectangle(x+m,y+h-1,w-m,1);
      }
    else if(options&SLIDER_ARROW_RIGHT){
      dc.setForeground(hiliteColor);
      dc.drawLine(x,y,x+w-m-1,y);
      dc.drawLine(x,y+1,x,y+h-1);
      dc.drawLine(x+w-1,y+m,x+w-m-1,y);
#ifndef WIN32
      dc.setForeground(shadowColor);
      dc.drawLine(x+w-2,y+h-m-1,x+w-m-2,y+h-1);
      dc.drawLine(x+1,y+h-2,x+w-m-1,y+h-2);
      dc.setForeground(borderColor);
      dc.drawLine(x+w-1,y+h-m-1,x+w-m-1,y+h-1);
      dc.drawLine(x,y+h-1,x+w-m-1,y+h-1);
#else
      dc.setForeground(shadowColor);
      dc.drawLine(x+w-1,y+h-m-2,x+w-m-2,y+h-1);
      dc.drawLine(x+1,y+h-2,x+w-m-1,y+h-2);
      dc.setForeground(borderColor);
      dc.drawLine(x+w,y+h-m-2,x+w-m-1,y+h-1);
      dc.drawLine(x,y+h-1,x+w-m-1,y+h-1);
#endif
      }
    else if(options&SLIDER_INSIDE_BAR){
      drawDoubleRaisedRectangle(dc,x,y,w,h);
      dc.setForeground(shadowColor);
      dc.drawLine(x+1,y+m-1,x+w-2,y+m-1);
      dc.setForeground(hiliteColor);
      dc.drawLine(x+1,y+m,x+w-2,y+m);
      }
    else{
      drawDoubleRaisedRectangle(dc,x,y,w,h);
      }
    }
  else{
    m=(w>>1);
    if(options&SLIDER_ARROW_UP){
      dc.setForeground(hiliteColor);
      dc.drawLine(x,y+m,x+m,y);
      dc.drawLine(x,y+m,x,y+h-1);
      dc.setForeground(shadowColor);
      dc.drawLine(x+w-1,y+m+1,x+w-m-1,y+1);
      dc.drawLine(x+w-2,y+m+1,x+w-2,y+h-1);
      dc.drawLine(x+1,y+h-2,x+w-2,y+h-2);
      dc.setForeground(borderColor);
      dc.drawLine(x+w-1,y+m,x+w-m-1,y);
      dc.drawLine(x+w-1,y+m,x+w-1,y+h-1);
      dc.fillRectangle(x,y+h-1,w,1);
      }
    else if(options&SLIDER_ARROW_DOWN){
      dc.setForeground(hiliteColor);
      dc.drawLine(x,y,x+w-1,y);
      dc.drawLine(x,y+1,x,y+h-m-1);
      dc.drawLine(x,y+h-m-1,x+m,y+h-1);
      dc.setForeground(shadowColor);
      dc.drawLine(x+w-2,y+1,x+w-2,y+h-m-1);
      dc.drawLine(x+w-1,y+h-m-2,x+w-m-1,y+h-2);
      dc.setForeground(borderColor);
      dc.drawLine(x+w-1,y+h-m-1,x+w-m-1,y+h-1);
      dc.fillRectangle(x+w-1,y,1,h-m);
      }
    else if(options&SLIDER_INSIDE_BAR){
      drawDoubleRaisedRectangle(dc,x,y,w,h);
      dc.setForeground(shadowColor);
      dc.drawLine(x+m-1,y+1,x+m-1,y+h-2);
      dc.setForeground(hiliteColor);
      dc.drawLine(x+m,y+1,x+m,y+h-1);
      }
    else{
      drawDoubleRaisedRectangle(dc,x,y,w,h);
      }
    }
  }


// Handle repaint
long FXSlider::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXint tx,ty,hhs=headsize/2;
  FXint xx,yy,ww,hh;
  FXDCWindow dc(this,event);

  // Repaint background
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);

  // Repaint border
  drawFrame(dc,0,0,width,height);

  // Slot placement
  xx=border+padleft;
  yy=border+padtop;
  ww=width-(border<<1)-padleft-padright;
  hh=height-(border<<1)-padtop-padbottom;
  FXASSERT(range[0]<=pos && pos<=range[1]);

  // Draw the slot
  if(options&SLIDER_VERTICAL){

    // Adjust slot placement for tickmarks
    if(options&SLIDER_TICKS_LEFT){ xx+=TICKSIZE; ww-=TICKSIZE; }
    if(options&SLIDER_TICKS_RIGHT){ ww-=TICKSIZE; }

    // Draw slider
    if(options&SLIDER_INSIDE_BAR){
      drawDoubleSunkenRectangle(dc,xx,yy,ww,hh);
      dc.setStipple(STIPPLE_GRAY);
      dc.setForeground(slotColor);
      dc.setBackground(baseColor);
      dc.setFillStyle(FILL_OPAQUESTIPPLED);
      dc.fillRectangle(xx+2,yy+2,ww-4,hh-4);
      dc.setFillStyle(FILL_SOLID);
      if(options&SLIDER_TICKS_LEFT) drawVertTicks(dc,border+padleft,yy,ww,hh);
      if(options&SLIDER_TICKS_RIGHT) drawVertTicks(dc,width-padright-border-TICKSIZE,yy,ww,hh);
      if(isEnabled()) drawSliderHead(dc,xx+2,headpos,ww-4,headsize);
      }
    else{
      if(options&SLIDER_ARROW_LEFT) tx=xx+hhs+(ww-slotsize-hhs)/2;
      else if(options&SLIDER_ARROW_RIGHT) tx=xx+(ww-slotsize-hhs)/2;
      else tx=xx+(ww-slotsize)/2;
      drawDoubleSunkenRectangle(dc,tx,yy,slotsize,hh);
      dc.setForeground(slotColor);
      dc.fillRectangle(tx+2,yy+2,slotsize-4,hh-4);
      if(options&SLIDER_TICKS_LEFT) drawVertTicks(dc,border+padleft,yy,ww,hh);
      if(options&SLIDER_TICKS_RIGHT) drawVertTicks(dc,width-padright-border-TICKSIZE,yy,ww,hh);
      if(isEnabled()) drawSliderHead(dc,xx,headpos,ww,headsize);
      }
    }
  else{

    // Adjust slot placement for tickmarks
    if(options&SLIDER_TICKS_TOP){ yy+=TICKSIZE; hh-=TICKSIZE; }
    if(options&SLIDER_TICKS_BOTTOM){ hh-=TICKSIZE; }

    // Draw slider
    if(options&SLIDER_INSIDE_BAR){
      drawDoubleSunkenRectangle(dc,xx,yy,ww,hh);
      dc.setForeground(slotColor);
      dc.setStipple(STIPPLE_GRAY);
      dc.setForeground(slotColor);
      dc.setBackground(baseColor);
      dc.setFillStyle(FILL_OPAQUESTIPPLED);
      dc.fillRectangle(xx+2,yy+2,ww-4,hh-4);
      dc.setFillStyle(FILL_SOLID);
      if(options&SLIDER_TICKS_TOP) drawHorzTicks(dc,xx,border+padtop,ww,hh);
      if(options&SLIDER_TICKS_BOTTOM) drawHorzTicks(dc,xx,height-border-padbottom-TICKSIZE,ww,hh);
      if(isEnabled()) drawSliderHead(dc,headpos,yy+2,headsize,hh-4);
      }
    else{
      if(options&SLIDER_ARROW_UP) ty=yy+hhs+(hh-slotsize-hhs)/2;
      else if(options&SLIDER_ARROW_DOWN) ty=yy+(hh-slotsize-hhs)/2;
      else ty=yy+(hh-slotsize)/2;
      drawDoubleSunkenRectangle(dc,xx,ty,ww,slotsize);
      dc.setForeground(slotColor);
      dc.fillRectangle(xx+2,ty+2,ww-4,slotsize-4);
      if(options&SLIDER_TICKS_TOP) drawHorzTicks(dc,xx,border+padtop,ww,hh);
      if(options&SLIDER_TICKS_BOTTOM) drawHorzTicks(dc,xx,height-border-padbottom-TICKSIZE,ww,hh);
      if(isEnabled()) drawSliderHead(dc,headpos,yy,headsize,hh);
      }
    }
  return 1;
  }


// Set slider range; this also revalidates the position,
// and possibly moves the head [even if the position was still OK,
// the head might still have to be moved to the exact position].
void FXSlider::setRange(FXint lo,FXint hi,FXbool notify){
  if(lo>hi){ fxerror("%s::setRange: trying to set negative range.\n",getClassName()); }
  if(range[0]!=lo || range[1]!=hi){
    range[0]=lo;
    range[1]=hi;
    setValue(pos,notify);
    }
  }


// Set position; this should always cause the head to reflect
// the exact [discrete] value representing pos, even if several
// head positions may represent the same position!
// Also, the minimal amount is repainted, as one sometimes as very
// large/wide sliders.
void FXSlider::setValue(FXint p,FXbool notify){
  register FXint interval=range[1]-range[0];
  register FXint travel,lo,hi,h;
  if(p<range[0]) p=range[0];
  if(p>range[1]) p=range[1];
  if(options&SLIDER_VERTICAL){
    travel=height-(border<<1)-padtop-padbottom-headsize-4;
    h=height-border-padbottom-2-headsize;
    if(0<interval) h-=(travel*(p-range[0]))/interval;
    if(h!=headpos){
      FXMINMAX(lo,hi,headpos,h);
      headpos=h;
      update(border,lo-1,width-(border<<1),hi+headsize+2-lo);
      }
    }
  else{
    travel=width-(border<<1)-padleft-padright-headsize-4;
    h=border+padleft+2;
    if(0<interval) h+=(travel*(p-range[0]))/interval;
    if(h!=headpos){
      FXMINMAX(lo,hi,headpos,h);
      headpos=h;
      update(lo-1,border,hi+headsize+2-lo,height-(border<<1));
      }
    }
  if(pos!=p){
    pos=p;
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);}
    }
  }


// Get slider options
FXuint FXSlider::getSliderStyle() const {
  return (options&SLIDER_MASK);
  }


// Set slider options
void FXSlider::setSliderStyle(FXuint style){
  register FXuint opts=(options&~SLIDER_MASK) | (style&SLIDER_MASK);
  if(options!=opts){
    headsize=(opts&SLIDER_INSIDE_BAR)?HEADINSIDEBAR:HEADOVERHANGING;
    options=opts;
    recalc();
    update();
    }
  }


// Set head size
void FXSlider::setHeadSize(FXint hs){
  if(headsize!=hs){
    headsize=hs;
    recalc();
    update();
    }
  }


// Set slot size
void FXSlider::setSlotSize(FXint bs){
  if(slotsize!=bs){
    slotsize=bs;
    recalc();
    update();
    }
  }


// Set increment
void FXSlider::setIncrement(FXint inc){
  incr=inc;
  }


// Set slot color
void FXSlider::setSlotColor(FXColor clr){
  if(slotColor!=clr){
    slotColor=clr;
    update();
    }
  }


// Change the delta between ticks
void FXSlider::setTickDelta(FXint dist){
  if(dist<0) dist=0;
  if(delta!=dist){
    delta=dist;
    if(options&(SLIDER_TICKS_TOP|SLIDER_TICKS_BOTTOM)){
      recalc();
      }
    }
  }


// Save object to stream
void FXSlider::save(FXStream& store) const {
  FXFrame::save(store);
  store << range[0] << range[1];
  store << pos;
  store << incr;
  store << delta;
  store << slotColor;
  store << headsize;
  store << slotsize;
  store << help;
  store << tip;
  }


// Load object from stream
void FXSlider::load(FXStream& store){
  FXFrame::load(store);
  store >> range[0] >> range[1];
  store >> pos;
  store >> incr;
  store >> delta;
  store >> slotColor;
  store >> headsize;
  store >> slotsize;
  store >> help;
  store >> tip;
  }


// Delete
FXSlider::~FXSlider(){
  getApp()->removeTimeout(this,ID_AUTOSLIDE);
  }

}
