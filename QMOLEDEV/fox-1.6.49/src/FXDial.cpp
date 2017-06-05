/********************************************************************************
*                                                                               *
*                                D i a l   W i d g e t                          *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXDial.cpp,v 1.50.2.1 2007/09/22 04:28:37 fox Exp $                          *
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXDial.h"


/*
  Notes:
  - Contributed by: Guoqing Tian.
  - Position decoupled from angle.
  - Add some API's.
  - Properly handle cyclic/non cyclic stuff.
  - Callbacks should report position in the void* ptr.
  - Keep notchangle>=0, as % of negative numbers is implementation defined.
  - Not yet happy with keyboard/wheel mode valuator.
  - Visual cue for focus:- please no ugly border!
*/

#define DIALWIDTH     12
#define DIALDIAMETER  40
#define NUMSIDECOLORS 16
#define DIAL_MASK     (DIAL_HORIZONTAL|DIAL_CYCLIC|DIAL_HAS_NOTCH)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXDial) FXDialMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXDial::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXDial::onMotion),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXDial::onMouseWheel),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXDial::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXDial::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXDial::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXDial::onKeyRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXDial::onUngrabbed),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXDial::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXDial::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETVALUE,FXDial::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETINTVALUE,FXDial::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETREALVALUE,FXDial::onCmdSetRealValue),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_GETINTVALUE,FXDial::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_GETREALVALUE,FXDial::onCmdGetRealValue),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETINTRANGE,FXDial::onCmdSetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_GETINTRANGE,FXDial::onCmdGetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETREALRANGE,FXDial::onCmdSetRealRange),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_GETREALRANGE,FXDial::onCmdGetRealRange),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETHELPSTRING,FXDial::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_GETHELPSTRING,FXDial::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_SETTIPSTRING,FXDial::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXDial::ID_GETTIPSTRING,FXDial::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXDial,FXFrame,FXDialMap,ARRAYNUMBER(FXDialMap))

FXDial::FXDial(){
  flags|=FLAG_ENABLED;
  range[0]=0;
  range[1]=0;
  notchangle=0;
  notchspacing=0;
  notchoffset=0;
  notchColor=0;
  dragpoint=0;
  dragpos=0;
  incr=0;
  pos=0;
  }


// Make a window
FXDial::FXDial(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  range[0]=0;
  range[1]=359;
  notchangle=0;
  notchspacing=90;
  notchoffset=0;
  notchColor=FXRGB(255,128,0);
  dragpoint=0;
  dragpos=0;
  incr=360;
  pos=0;
  }


// Get minimum width
FXint FXDial::getDefaultWidth(){
  register FXint w=(options&DIAL_HORIZONTAL)?DIALDIAMETER:DIALWIDTH;
  return w+padleft+padright+(border<<1);
  }


// Get minimum height
FXint FXDial::getDefaultHeight(){
  register FXint h=(options&DIAL_HORIZONTAL)?DIALWIDTH:DIALDIAMETER;
  return h+padtop+padbottom+(border<<1);
  }


// Returns true because a dial can receive focus
bool FXDial::canFocus() const { return true; }


// Set help using a message
long FXDial::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXDial::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXDial::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXDial::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXDial::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXDial::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Update value from a message
long FXDial::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXDial::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setValue(*((FXint*)ptr));
  return 1;
  }


// Update value from a message
long FXDial::onCmdSetRealValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)*((FXdouble*)ptr));
  return 1;
  }


// Obtain value from text field
long FXDial::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr) = getValue();
  return 1;
  }


// Obtain value from text field
long FXDial::onCmdGetRealValue(FXObject*,FXSelector,void* ptr){
  *((FXdouble*)ptr) = (FXdouble)getValue();
  return 1;
  }


// Update range from a message
long FXDial::onCmdSetIntRange(FXObject*,FXSelector,void* ptr){
  setRange(((FXint*)ptr)[0],((FXint*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXDial::onCmdGetIntRange(FXObject*,FXSelector,void* ptr){
  getRange(((FXint*)ptr)[0],((FXint*)ptr)[1]);
  return 1;
  }


// Update range from a message
long FXDial::onCmdSetRealRange(FXObject*,FXSelector,void* ptr){
  setRange((FXint) ((FXdouble*)ptr)[0],(FXint) ((FXdouble*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXDial::onCmdGetRealRange(FXObject*,FXSelector,void* ptr){
  ((FXdouble*)ptr)[0]=(FXdouble)range[0];
  ((FXdouble*)ptr)[1]=(FXdouble)range[1];
  return 1;
  }


// Pressed LEFT button
long FXDial::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(options&DIAL_HORIZONTAL)
      dragpoint=event->win_x;
    else
      dragpoint=event->win_y;
    dragpos=pos;
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released LEFT button
long FXDial::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint changed=(flags&FLAG_CHANGED);
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    flags&=~FLAG_CHANGED;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(changed && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
    return 1;
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXDial::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  flags&=~FLAG_PRESSED;
  flags&=~FLAG_CHANGED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Moving
long FXDial::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXint travel,size,delta,newpos,tmp;
  if(flags&FLAG_PRESSED){
    if(options&DIAL_HORIZONTAL){
      size=width-(border<<1);
      travel=event->win_x-dragpoint;
      }
    else{
      size=height-(border<<1);
      travel=dragpoint-event->win_y;
      }
    if(size<100) size=100;
    if(travel){
      delta=(incr*travel)/(2*size);
      if(options&DIAL_CYCLIC){
        tmp=dragpos+delta-range[0];
        while(tmp<0) tmp+=(range[1]-range[0]+1);
        newpos=range[0]+tmp%(range[1]-range[0]+1);
        }
      else{
        if(dragpos+delta<range[0]) newpos=range[0];
        else if(dragpos+delta>range[1]) newpos=range[1];
        else newpos=dragpos+delta;
        }
      if(pos!=newpos){
        pos=newpos;
        FXASSERT(range[0]<=pos && pos<=range[1]);
        notchangle=(notchoffset+(3600*(pos-range[0]))/incr)%3600;
        update(border+padleft+1,border+padtop+1,width-(border<<1)-padleft-padright-2,height-(border<<1)-padtop-padbottom-2);
        flags|=FLAG_CHANGED;
        if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
        return 1;
        }
      }
    }
  return 0;
  }


// Mouse wheel (Thanks to  "Lyle Johnson" <lyle@knology.net>)
long FXDial::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXint delta,newpos,tmp,mod;

  // Determine the change in dial units; this probably still needs
  // tweaking. The formula below adjusts the dial position by 1/36
  // of a revolution for each "hop" of the mousewheel.
  delta=(event->code*incr)/4320;

  // Determine new dial position
  if(options&DIAL_CYCLIC){
    mod=range[1]-range[0]+1;
    tmp=pos+delta-range[0];
    while(tmp<0) tmp+=mod;
    newpos=range[0]+tmp%mod;        // FIXME small problem if range[1]-range[0]+1 is UINT_MAX
    }
  else{
    if(pos+delta<range[0]) newpos=range[0];
    else if(pos+delta>range[1]) newpos=range[1];
    else newpos=pos+delta;
    }
  if(pos!=newpos){
    pos=newpos;
    FXASSERT(range[0]<=pos && pos<=range[1]);
    notchangle=(notchoffset+(3600*(pos-range[0]))/incr)%3600;
    update(border+padleft+1,border+padtop+1,width-(border<<1)-padleft-padright-2,height-(border<<1)-padtop-padbottom-2);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
    }
  return 1;
  }


// Keyboard press
long FXDial::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    switch(event->code){
      case KEY_Left:
      case KEY_KP_Left:
        if(options&DIAL_HORIZONTAL) goto dec;
        break;
      case KEY_Right:
      case KEY_KP_Right:
        if(options&DIAL_HORIZONTAL) goto inc;
        break;
      case KEY_Up:
      case KEY_KP_Up:
        if(!(options&DIAL_HORIZONTAL)) goto inc;
        break;
      case KEY_Down:
      case KEY_KP_Down:
        if(!(options&DIAL_HORIZONTAL)) goto dec;
        break;
      case KEY_plus:
      case KEY_KP_Add:
inc:    setValue(pos+1,TRUE);
        return 1;
      case KEY_minus:
      case KEY_KP_Subtract:
dec:    setValue(pos-1,TRUE);
        return 1;
      }
    }
  return 0;
  }


// Keyboard release
long FXDial::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_Left:
      case KEY_KP_Left:
      case KEY_Right:
      case KEY_KP_Right:
        if(options&DIAL_HORIZONTAL) return 1;
        break;
      case KEY_Up:
      case KEY_KP_Up:
      case KEY_Down:
      case KEY_KP_Down:
        if(!(options&DIAL_HORIZONTAL)) return 1;
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


// Handle repaint
long FXDial::onPaint(FXObject*,FXSelector,void* ptr){
  const FXdouble fac=0.5*PI/((FXdouble)(NUMSIDECOLORS-1));
  FXEvent *event=(FXEvent*)ptr;
  FXint i,size,u,d,lu,ld,t,r,fm,to,off,ang;
  FXuint rmax,gmax,bmax,red,green,blue;
  FXint lt,rt,tp,bm;
  FXdouble mid,tmp;
  FXDCWindow dc(this,event);

  // Paint background
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);

  off=(notchangle+3600)%notchspacing;
  fm=off/notchspacing;
  to=(off+1800-notchspacing+1)/notchspacing;

  // Rectangle of dial
  lt=border+padleft+1;
  rt=width-border-padright-2;
  tp=border+padtop+1;
  bm=height-border-padbottom-2;

  // Colors for sides
  rmax=(126*FXREDVAL(backColor))/100;
  gmax=(126*FXGREENVAL(backColor))/100;
  bmax=(126*FXBLUEVAL(backColor))/100;
  rmax=FXMIN(rmax,255);
  gmax=FXMIN(gmax,255);
  bmax=FXMIN(bmax,255);

  // Horizontal dial
  if(options&DIAL_HORIZONTAL){
    size=rt-lt;
    r=size/2-1;
    mid=0.5*(lt+rt);
    for(i=fm; i<=to; i++){
      ang=i*notchspacing+off;
      t=(FXint)(mid-r*cos(0.1*DTOR*ang));
      if((options&DIAL_HAS_NOTCH) && (ang+3600)%3600==notchangle){
        dc.setForeground(hiliteColor);
        dc.drawLine(t-1,tp,t-1,bm);
        dc.setForeground(notchColor);
        dc.drawLine(t,tp,t,bm);
        dc.drawLine(t+1,tp,t+1,bm);
        dc.setForeground(borderColor);
        dc.drawLine(t+2,tp,t+2,bm);
        }
      else{
        if(ang<200){
          dc.setForeground(shadowColor);
          dc.drawLine(t,tp,t,bm);
          dc.setForeground(borderColor);
          dc.drawLine(t+1,tp,t+1,bm);
          }
        else if(ang<300){
          dc.setForeground(borderColor);
          dc.drawLine(t,tp,t,bm);
          }
        else if(ang<600){
          dc.setForeground(hiliteColor);
          dc.drawLine(t,tp,t,bm);
          dc.setForeground(borderColor);
          dc.drawLine(t+1,tp,t+1,bm);
          }
        else if(ang<1200){
          dc.setForeground(hiliteColor);
          dc.drawLine(t-1,tp,t-1,bm);
          dc.drawLine(t,tp,t,bm);
          dc.setForeground(borderColor);
          dc.drawLine(t+1,tp,t+1,bm);
          }
        else if(ang<1500){
          dc.setForeground(hiliteColor);
          dc.drawLine(t,tp,t,bm);
          dc.setForeground(borderColor);
          dc.drawLine(t+1,tp,t+1,bm);
          }
        else if(ang<1600){
          dc.setForeground(borderColor);
          dc.drawLine(t,tp,t,bm);
          }
        else{
          dc.setForeground(shadowColor);
          dc.drawLine(t,tp,t,bm);
          dc.setForeground(borderColor);
          dc.drawLine(t-1,tp,t-1,bm);
          }
        }
      }
    dc.drawLine(lt,tp,lt,bm);
    dc.drawLine(rt,tp,rt,bm);
    lu=lt;
    ld=rt;
    for(i=0; i<NUMSIDECOLORS; i++){
      tmp=r*cos(fac*i);
      u=(FXint)(mid-tmp);
      d=(FXint)(mid+tmp);
      red=(rmax*i)/(NUMSIDECOLORS-1);
      green=(gmax*i)/(NUMSIDECOLORS-1);
      blue=(bmax*i)/(NUMSIDECOLORS-1);
      dc.setForeground(FXRGB(red,green,blue));
      dc.drawLine(lu,tp,u,tp);
      dc.drawLine(ld,tp,d,tp);
      dc.drawLine(lu,bm,u,bm);
      dc.drawLine(ld,bm,d,bm);
      lu=u;
      ld=d;
      }
    dc.drawLine(lu,tp,ld,tp);
    dc.drawLine(lu,bm,ld,bm);
    }

  // Vertical dial
  else{
    size=bm-tp;
    r=size/2-1;
    mid=0.5*(tp+bm);
    for(i=fm; i<=to; i++){
      ang=i*notchspacing+off;
      t=(FXint)(mid+r*cos(0.1*DTOR*ang));
      if((options&DIAL_HAS_NOTCH) && (ang+3600)%3600==notchangle){
        dc.setForeground(hiliteColor);
        dc.drawLine(lt,t-1,rt,t-1);
        dc.setForeground(notchColor);
        dc.drawLine(lt,t,rt,t);
        dc.drawLine(lt,t+1,rt,t+1);
        dc.setForeground(borderColor);
        dc.drawLine(lt,t+2,rt,t+2);
        }
      else{
        if(ang<200){
          dc.setForeground(borderColor);
          dc.drawLine(lt,t,rt,t);
          dc.setForeground(shadowColor);
          dc.drawLine(lt,t-1,rt,t-1);
          }
        else if(ang<300){
          dc.setForeground(borderColor);
          dc.drawLine(lt,t,rt,t);
          }
        else if(ang<600){
          dc.setForeground(hiliteColor);
          dc.drawLine(lt,t,rt,t);
          dc.setForeground(borderColor);
          dc.drawLine(lt,t+1,rt,t+1);
          }
        else if(ang<1200){
          dc.setForeground(hiliteColor);
          dc.drawLine(lt,t-1,rt,t-1);
          dc.drawLine(lt,t,rt,t);
          dc.setForeground(borderColor);
          dc.drawLine(lt,t+1,rt,t+1);
          }
        else if(ang<1500){
          dc.setForeground(hiliteColor);
          dc.drawLine(lt,t,rt,t);
          dc.setForeground(borderColor);
          dc.drawLine(lt,t+1,rt,t+1);
          }
        else if(ang<1600){
          dc.setForeground(borderColor);
          dc.drawLine(lt,t,rt,t);
          }
        else{
          dc.setForeground(borderColor);
          dc.drawLine(lt,t,rt,t);
          dc.setForeground(shadowColor);
          dc.drawLine(lt,t+1,rt,t+1);
          }
        }
      }
    dc.drawLine(lt,tp,rt,tp);
    dc.drawLine(lt,bm,rt,bm);
    lu=tp;
    ld=bm;
    for(i=0; i<NUMSIDECOLORS; i++){
      tmp=r*cos(fac*i);
      u=(FXint)(mid-tmp);
      d=(FXint)(mid+tmp);
      red=(rmax*i)/(NUMSIDECOLORS-1);
      green=(gmax*i)/(NUMSIDECOLORS-1);
      blue=(bmax*i)/(NUMSIDECOLORS-1);
      dc.setForeground(FXRGB(red,green,blue));
      dc.drawLine(lt,lu,lt,u);
      dc.drawLine(lt,ld,lt,d);
      dc.drawLine(rt,lu,rt,u);
      dc.drawLine(rt,ld,rt,d);
      lu=u;
      ld=d;
      }
    dc.drawLine(lt,lu,lt,ld);
    dc.drawLine(rt,lu,rt,ld);
    }

  // Border
  drawFrame(dc,0,0,width,height);

  // Inner rectangle
  dc.setForeground(shadowColor);
  dc.drawRectangle(lt-1,tp-1,rt-lt+2,bm-tp+2);
  return 1;
  }


// Set dial range
void FXDial::setRange(FXint lo,FXint hi,FXbool notify){
  if(lo>hi){ fxerror("%s::setRange: trying to set negative range.\n",getClassName()); }
  if(range[0]!=lo || range[1]!=hi){
    range[0]=lo;
    range[1]=hi;
    setValue(pos,notify);
    }
  }


// Set dial value
void FXDial::setValue(FXint p,FXbool notify){
  register FXint n;
  if(p<range[0]) p=range[0];
  if(p>range[1]) p=range[1];
  n=(notchoffset+(3600*(p-range[0]))/incr)%3600;
  if(n!=notchangle){
    notchangle=n;
    update();
    }
  if(p!=pos){
    pos=p;
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);}
    }
  }


// Change increment, i.e. the amount of pos change per revolution
void FXDial::setRevolutionIncrement(FXint i){
  incr=FXMAX(1,i);
  notchangle=(notchoffset+(3600*(pos-range[0]))/incr)%3600;
  update();
  }


// Change notch spacing
void FXDial::setNotchSpacing(FXint spacing){
  if(spacing<1) spacing=1;
  if(spacing>3600) spacing=3600;
  while(3600%spacing) spacing--;    // Should be a divisor of 3600
  if(notchspacing!=spacing){
    notchspacing=spacing;
    update();
    }
  }


// Change notch offset
void FXDial::setNotchOffset(FXint offset){
  if(offset>3600) offset=3600;
  if(offset<-3600) offset=-3600;
  offset=(offset+3600)%3600;
  if(offset!=notchoffset){
    notchoffset=offset;
    notchangle=(notchoffset+(3600*(pos-range[0]))/incr)%3600;
    update();
    }
  }


// Get dial options
FXuint FXDial::getDialStyle() const {
  return (options&DIAL_MASK);
  }


// Set dial options
void FXDial::setDialStyle(FXuint style){
  FXuint opts=(options&~DIAL_MASK) | (style&DIAL_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    }
  }


// Save object to stream
void FXDial::save(FXStream& store) const {
  FXFrame::save(store);
  store << range[0] << range[1];
  store << notchColor;
  store << notchangle;
  store << notchspacing;
  store << notchoffset;
  store << incr;
  store << pos;
  store << help;
  store << tip;
  }


// Load object from stream
void FXDial::load(FXStream& store){
  FXFrame::load(store);
  store >> range[0] >> range[1];
  store >> notchColor;
  store >> notchangle;
  store >> notchspacing;
  store >> notchoffset;
  store >> incr;
  store >> pos;
  store >> help;
  store >> tip;
  }


// Change the Center Notch color
void FXDial::setNotchColor(FXColor clr){
  if(clr!=notchColor){
    notchColor=clr;
    update();
    }
  }


// Change help text
void FXDial::setHelpText(const FXString& text){
  help=text;
  }


// Change tip text
void FXDial::setTipText(const FXString& text){
  tip=text;
  }

}

