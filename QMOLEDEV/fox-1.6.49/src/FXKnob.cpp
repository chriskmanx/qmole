/********************************************************************************
*                                                                               *
*                             K n o b   W i d g e t                             *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Leandro Nini.   All Rights Reserved.               *
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
* $Id: FXKnob.cpp,v 1.15 2006/01/22 17:58:32 fox Exp $                          *
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
#include "FXKnob.h"

/*
  Notes:
  - Contributed by: "Leandro Nini" <m.l.n@katamail.com>
  - Some minor tweaks by Jeroen:
    - Added notify parameter to setValue [all valuators have
      this now as of 7/05].
    - setValue() should clamp input, not reject when out of
      range; all other valuators do this, and its more predictable
      if FXKnob would do this as well:- allowing sliders to be replaced
      by FXKnob with no change to application code.
    - Removed duplicate member data [FXFrame already has shadowColor etc.].
    - Added getter/setter for indicator needle color.
    - Fixed drawing such that knob is centered in area left over after
      accounting for padding, even if padding not equal on all sides.
    - Changed trucation to rounding in floating point conversion.
  - Some of the other valuators should probably also have similar
    keyboard bindings.
  - Cyclic mode like dial has would be nice since we may want to turn
    something 360 degrees.
  - Maybe a value-jump-if-clicked can be replaced by a relative movement;
    for example, if you grab a slider head it doesn't jump-move but stays
    relative to the initial mouse position.  This allows for accurate
    positioning.  Likewise, we could do the circular equivalent here
    [but keep the jump-if-clicked more for middle-mouse].
  - Need to have triangular arrow.
  - Need to have ability to set starting/ending angle, plus full 360 degree
    mode.
  - Need to have optional knob; with just tick-marks and needle, the control
    can function as a nice vu-meter.
  - Incidentally, find my old source code for vu-meter drawing and use it
    here....
  - Plus, swept triangle thingy!
*/


#define KNOBSIZE  30

#define KNOB_MASK (KNOB_NEEDLE|KNOB_DOT|KNOB_TICKS|KNOB_INDICATOR)


using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXKnob) FXKnobMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXKnob::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXKnob::onMotion),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXKnob::onMouseWheel),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXKnob::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXKnob::onLeftBtnRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXKnob::onMiddleBtnPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXKnob::onMiddleBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXKnob::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXKnob::onKeyRelease),
  FXMAPFUNC(SEL_FOCUSIN,0,FXKnob::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXKnob::onFocusOut),
  FXMAPFUNC(SEL_UNGRABBED,0,FXKnob::onUngrabbed),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXKnob::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXKnob::onQueryHelp),
  FXMAPFUNC(SEL_TIMEOUT,FXKnob::ID_AUTOSLIDE,FXKnob::onAutoSlide),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETVALUE,FXKnob::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETINTVALUE,FXKnob::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETREALVALUE,FXKnob::onCmdSetRealValue),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_GETINTVALUE,FXKnob::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_GETREALVALUE,FXKnob::onCmdGetRealValue),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETINTRANGE,FXKnob::onCmdSetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_GETINTRANGE,FXKnob::onCmdGetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETREALRANGE,FXKnob::onCmdSetRealRange),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_GETREALRANGE,FXKnob::onCmdGetRealRange),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETHELPSTRING,FXKnob::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_GETHELPSTRING,FXKnob::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_SETTIPSTRING,FXKnob::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXKnob::ID_GETTIPSTRING,FXKnob::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXKnob,FXFrame,FXKnobMap,ARRAYNUMBER(FXKnobMap))


// Make a knob
FXKnob::FXKnob(){
  flags|=FLAG_ENABLED;
  range[0]=0;
  range[1]=0;
  limits[0]=0.0;
  limits[1]=0.0;
  lineColor=0;
  incr=0;
  pos=0;
  delta=0;
  }


// Make a knob
FXKnob::FXKnob(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  lineColor=getApp()->getForeColor();
  target=tgt;
  message=sel;
  range[0]=0;
  range[1]=100;
  limits[0]=-1.0/3.0;
  limits[1]=4.0/3.0;
  incr=1;
  pos=0;
  delta=10;
  }


// Knob can have focus
bool FXKnob::canFocus() const { return true; }


// Enable the knob
void FXKnob::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXFrame::enable();
    update();
    }
  }


// Disable the knob
void FXKnob::disable(){
  if(flags&FLAG_ENABLED){
    FXFrame::disable();
    update();
    }
  }


// Get default size
FXint FXKnob::getDefaultWidth(){
  register FXint w=KNOBSIZE;
  if(options&KNOB_TICKS) w+=4;
  return w+padleft+padright+(border<<1);
  }


FXint FXKnob::getDefaultHeight(){
  register FXint h=KNOBSIZE;
  if(options&KNOB_TICKS) h+=4;
  return h+padtop+padbottom+(border<<1);
  }


// Layout changed
void FXKnob::layout(){
  setValue(pos);
  flags&=~FLAG_DIRTY;
  }


// Gained focus
long FXKnob::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onFocusIn(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Lost focus
long FXKnob::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onFocusOut(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Set help using a message
long FXKnob::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXKnob::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXKnob::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXKnob::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXKnob::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXKnob::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Update value from a message
long FXKnob::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXKnob::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setValue(*((FXint*)ptr));
  return 1;
  }


// Update value from a message
long FXKnob::onCmdSetRealValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)*((FXdouble*)ptr));
  return 1;
  }


// Obtain value from text field
long FXKnob::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getValue();
  return 1;
  }


// Obtain value with a message
long FXKnob::onCmdGetRealValue(FXObject*,FXSelector,void* ptr){
  *((FXdouble*)ptr)=(FXdouble)getValue();
  return 1;
  }


// Update range from a message
long FXKnob::onCmdSetIntRange(FXObject*,FXSelector,void* ptr){
  setRange(((FXint*)ptr)[0],((FXint*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXKnob::onCmdGetIntRange(FXObject*,FXSelector,void* ptr){
  ((FXint*)ptr)[0]=range[0];
  ((FXint*)ptr)[1]=range[1];
  return 1;
  }


// Update range from a message
long FXKnob::onCmdSetRealRange(FXObject*,FXSelector,void* ptr){
  setRange((FXint)((FXdouble*)ptr)[0],(FXint)((FXdouble*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXKnob::onCmdGetRealRange(FXObject*,FXSelector,void* ptr){
  ((FXdouble*)ptr)[0]=(FXdouble)range[0];
  ((FXdouble*)ptr)[1]=(FXdouble)range[1];
  return 1;
  }


// Pressed LEFT button
long FXKnob::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint p,tol;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    getApp()->removeTimeout(this,ID_AUTOSLIDE);
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    p=calcValue(event->win_x,event->win_y);
    tol=(range[1]-range[0])/10;
    if(p>pos+tol){
      p=pos+incr;
      getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollDelay(),(void*)(FXival)incr);
      }
    else if(p<pos-tol){
      p=pos-incr;
      getApp()->addTimeout(this,ID_AUTOSLIDE,getApp()->getScrollDelay(),(void*)(FXival)-incr);
      }
    else{
      flags|=FLAG_PRESSED;
      }
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
long FXKnob::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  register FXuint flgs=flags;
  if(isEnabled()){
    ungrab();
    setValue(pos);
    flags&=~FLAG_PRESSED;
    flags&=~FLAG_CHANGED;
    flags|=FLAG_UPDATE;
    getApp()->removeTimeout(this,ID_AUTOSLIDE);
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(flgs&FLAG_CHANGED){
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Moving
long FXKnob::onMotion(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint p;
  if(!isEnabled()) return 0;
  if(flags&FLAG_PRESSED){
    p=calcValue(event->win_x,event->win_y);
    if(pos!=p){
      setValue(p);
      flags|=FLAG_CHANGED;
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Pressed middle button
long FXKnob::onMiddleBtnPress(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint p;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    flags|=FLAG_PRESSED;
    p=calcValue(event->win_x,event->win_y);
    if(p!=pos){
      setValue(p);
      flags|=FLAG_CHANGED;
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
      }
    return 1;
    }
  return 0;
  }


// Released middle button
long FXKnob::onMiddleBtnRelease(FXObject*,FXSelector,void* ptr){
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
long FXKnob::onMouseWheel(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint p=pos+(event->code*incr)/120;
  setValue(p,TRUE);
  return 1;
  }


// The widget lost the grab for some reason
long FXKnob::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_AUTOSLIDE);
  flags&=~FLAG_PRESSED;
  flags&=~FLAG_CHANGED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Automatically move knob while holding down mouse
long FXKnob::onAutoSlide(FXObject*,FXSelector,void* ptr){
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


// Keyboard press
long FXKnob::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    switch(event->code){
      case KEY_Up:
      case KEY_KP_Up:
        setValue(pos+incr,TRUE);
        return 1;
      case KEY_Down:
      case KEY_KP_Down:
        setValue(pos-incr,TRUE);
        return 1;
      }
    }
  return 0;
  }


// Keyboard release
long FXKnob::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_Up:
      case KEY_KP_Up:
      case KEY_Down:
      case KEY_KP_Down:
        return 1;
      }
    }
  return 0;
  }


// Handle repaint
long FXKnob::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  FXint xx,yy,ww,hh,cx,cy,rr,px,py,lw;

  // Draw frame
  drawFrame(dc,0,0,width,height);

  // Draw background
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));

  // Compute available space for knob
  ww=width-(border<<1)-padleft-padright;
  hh=height-(border<<1)-padtop-padbottom;
  cx=border+padleft+(ww>>1);
  cy=border+padtop+(hh>>1);

  // Make sure the knob is round
  rr=FXMIN(ww,hh)/2;

  if(hasFocus()){
    dc.drawFocusRectangle(cx-rr,cy-rr,rr+rr,rr+rr);
    }

  if(options&KNOB_TICKS) rr-=3;

  // Knob border width
  lw=rr/6;
  rr-=lw;
  xx=cx-rr;
  yy=cy-rr;
  FXASSERT(range[0]<=pos && pos<=range[1]);
  FXdouble p=(((double)(pos-range[0])/(double)(range[1]-range[0]))*(limits[1]-limits[0])+limits[0])*PI;

  // Draw knob
  dc.setLineWidth(lw);
  if(!(options&KNOB_INDICATOR)){
    dc.setForeground(hiliteColor);
    dc.drawArc(xx,yy,rr+rr,rr+rr,45*64,180*64);
    dc.setForeground(shadowColor);
    dc.drawArc(xx,yy,rr+rr,rr+rr,225*64,180*64);
    }

  // Draw indicator
  dc.setForeground(lineColor);

  if(!(options&KNOB_DOT)){
    px=(FXint)(-cos(p)*rr+0.5)+cx;
    py=(FXint)(-sin(p)*rr+0.5)+cy;
    dc.drawLine(cx,cy,px,py);
    }
  else{
    px=(FXint)(-cos(p)*(rr-lw*2)+0.5)+cx;
    py=(FXint)(-sin(p)*(rr-lw*2)+0.5)+cy;
    dc.fillEllipse(px-lw,py-lw,lw*2,lw*2);
    }

  // Draw ticks
  if(options&KNOB_TICKS){
    rr+=4;
    p=PI*limits[0];
    FXint numTicks=(range[1]-range[0])/delta;
    FXdouble d=(limits[1]-limits[0])/numTicks;
    numTicks++;
    FXASSERT(numTicks<1024);        // FIXME this needs to be done differently
    FXPoint points[1024];
    for(FXint i=0; i<numTicks; i++){
      px=(FXint)(-cos(p)*rr+0.5)+cx-1;
      py=(FXint)(-sin(p)*rr+0.5)+cy-1;
      points[i]=FXPoint(px,py);
      p+=PI*d;
      }
    dc.drawPoints(points,numTicks);
    }
  return 1;
  }


// Set knob range
void FXKnob::setRange(FXint lo,FXint hi,FXbool notify){
  if(lo>hi){ fxerror("%s::setRange: trying to set negative range.\n",getClassName()); }
  if(range[0]!=lo || range[1]!=hi){
    range[0]=lo;
    range[1]=hi;
    setValue(pos,notify);
    }
  }


// Set knob limits
void FXKnob::setLimits(FXint start,FXint end,FXbool notify){
  if(start>end || start<0 || end>360){ fxerror("%s::setLimits: invalid values.\n",getClassName()); }
  if(limits[0]!=start || limits[1]!=end){
    limits[0]=((FXdouble)start/PI*DTOR)-0.5;
    limits[1]=((FXdouble)end/PI*DTOR)-0.5;
    setValue(pos,notify);
    }
  }


// Set knob limits
void FXKnob::getLimits(FXint& start,FXint& end){
  start=(FXint)((limits[0]+0.5)*PI*RTOD+0.5);
  end=(FXint)((limits[1]+0.5)*PI*RTOD+0.5);
  }


// Set position
void FXKnob::setValue(FXint p,FXbool notify){
  if(p<range[0]) p=range[0];
  if(p>range[1]) p=range[1];
  if(p!=pos){
    pos=p;
    update(border,border,width-(border<<1),height-(border<<1));
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);}
    }
  }


// Get knob style
FXuint FXKnob::getKnobStyle() const {
  return (options&KNOB_MASK);
  }


// Set knob style
void FXKnob::setKnobStyle(FXuint style){
  register FXuint opts=(options&~KNOB_MASK) | (style&KNOB_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Change the delta between ticks
void FXKnob::setTickDelta(FXint dist){
  if(delta!=dist){
    delta=dist;
    update();
    }
  }


// Calculate value from position relative to center
FXint FXKnob::calcValue(FXint x,FXint y){
  register FXint cx=(width+padleft-padright)>>1;
  register FXint cy=(height+padtop-padbottom)>>1;
  register FXdouble angle=atan2((FXdouble)(cy-y),(FXdouble)(x-cx))/PI;
  if(angle<-0.5) angle+=2.0;
  angle=(1.0-angle-limits[0])/(limits[1]-limits[0]);
  return (FXint)(angle*(range[1]-range[0])+0.5)+range[0];
  }


// Change the indicator needle color
void FXKnob::setLineColor(FXColor clr){
  if(clr!=lineColor){
    lineColor=clr;
    update();
    }
  }


// Save object to stream
void FXKnob::save(FXStream& store) const {
  FXFrame::save(store);
  store << range[0] << range[1];
  store << limits[0] << limits[1];
  store << lineColor;
  store << pos;
  store << incr;
  store << delta;
  store << help;
  store << tip;
  }


// Load object from stream
void FXKnob::load(FXStream& store){
  FXFrame::load(store);
  store >> range[0] >> range[1];
  store >> limits[0] >> limits[1];
  store >> lineColor;
  store >> pos;
  store >> incr;
  store >> delta;
  store >> help;
  store >> tip;
  }


// On delete, remove the timer
FXKnob::~FXKnob(){
  getApp()->removeTimeout(this,ID_AUTOSLIDE);
  }

}

