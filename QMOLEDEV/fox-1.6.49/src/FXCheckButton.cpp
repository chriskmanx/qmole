/********************************************************************************
*                                                                               *
*                    C h e c k   B u t t o n    O b j e c t                     *
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
* $Id: FXCheckButton.cpp,v 1.58 2006/01/22 17:58:20 fox Exp $                   *
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
#include "FXCheckButton.h"

/*
  Notes:
  - Works as intended.
*/

#define CHECKBUTTON_MASK  (CHECKBUTTON_AUTOGRAY|CHECKBUTTON_AUTOHIDE|CHECKBUTTON_PLUS)

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXCheckButton) FXCheckButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXCheckButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXCheckButton::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXCheckButton::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXCheckButton::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXCheckButton::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXCheckButton::onFocusOut),
  FXMAPFUNC(SEL_UNGRABBED,0,FXCheckButton::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXCheckButton::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXCheckButton::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXCheckButton::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXCheckButton::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXCheckButton::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXCheckButton::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_CHECK,FXCheckButton::onCheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNCHECK,FXCheckButton::onUncheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNKNOWN,FXCheckButton::onUnknown),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXCheckButton::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXCheckButton::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXCheckButton::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXCheckButton,FXLabel,FXCheckButtonMap,ARRAYNUMBER(FXCheckButtonMap))



// Deserialization
FXCheckButton::FXCheckButton(){
  checkColor=0;
  boxColor=0;
  check=FALSE;
  oldcheck=FALSE;
  }


// Make a check button
FXCheckButton::FXCheckButton(FXComposite* p,const FXString& text,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,text,NULL,opts,x,y,w,h,pl,pr,pt,pb){
  checkColor=getApp()->getForeColor();
  boxColor=getApp()->getBackColor();
  target=tgt;
  message=sel;
  check=FALSE;
  oldcheck=FALSE;
  }


// If window can have focus
bool FXCheckButton::canFocus() const { return true; }


// Get default width
FXint FXCheckButton::getDefaultWidth(){
  FXint tw=0,s=0,w;
  if(!label.empty()){
    tw=labelWidth(label);
    s=4;
    }
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w=FXMAX(tw,13); else w=tw+13+s;
  return padleft+padright+w+(border<<1);
  }


// Get default height
FXint FXCheckButton::getDefaultHeight(){
  FXint th=0,h;
  if(!label.empty()){
    th=labelHeight(label);
    }
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h=FXMAX(th,13); else h=th+13;
  return padtop+padbottom+h+(border<<1);
  }


// Check button
void FXCheckButton::setCheck(FXbool state,FXbool notify){
  if(check!=state){
    check=state;
    update();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check);}
    }
  }


// Change state to checked
long FXCheckButton::onCheck(FXObject*,FXSelector,void*){
  setCheck(TRUE);
  return 1;
  }


// Change state to unchecked
long FXCheckButton::onUncheck(FXObject*,FXSelector,void*){
  setCheck(FALSE);
  return 1;
  }


// Change state to indeterminate
long FXCheckButton::onUnknown(FXObject*,FXSelector,void*){
  setCheck(MAYBE);
  return 1;
  }


// Update value from a message
long FXCheckButton::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)(FXuval)ptr);
  return 1;
  }


// Update value from a message
long FXCheckButton::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXCheckButton::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCheck();
  return 1;
  }


// Implement auto-hide or auto-gray modes
long FXCheckButton::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXLabel::onUpdate(sender,sel,ptr)){
    if(options&CHECKBUTTON_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&CHECKBUTTON_AUTOGRAY){disable();}
    }
  return 1;
  }


// Gained focus
long FXCheckButton::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusIn(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Lost focus
long FXCheckButton::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusOut(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Entered button
long FXCheckButton::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled() && (flags&FLAG_PRESSED)) setCheck(!oldcheck);
  return 1;
  }


// Left button
long FXCheckButton::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled() && (flags&FLAG_PRESSED)) setCheck(oldcheck);
  return 1;
  }


// Pressed mouse button
long FXCheckButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    oldcheck=check;
    setCheck(!oldcheck);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released mouse button
long FXCheckButton::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled() && (flags&FLAG_PRESSED)){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(check!=oldcheck && target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check); }
    return 1;
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXCheckButton::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onUngrabbed(sender,sel,ptr);
  setCheck(oldcheck);
  flags&=~FLAG_PRESSED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Key Press
long FXCheckButton::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      oldcheck=check;
      setCheck(!oldcheck);
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXCheckButton::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      if(check!=oldcheck && target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check); }
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXCheckButton::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    oldcheck=check;
    setCheck(!oldcheck);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    }
  return 1;
  }


// Hot key combination released
long FXCheckButton::onHotKeyRelease(FXObject*,FXSelector,void*){
  if(isEnabled() && (flags&FLAG_PRESSED)){
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(check!=oldcheck && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check);
    }
  return 1;
  }


// Handle repaint
long FXCheckButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXint tw=0,th=0,tx,ty,ix,iy;
  FXDCWindow dc(this,ev);

  // Figure text size
  if(!label.empty()){
    tw=labelWidth(label);
    th=labelHeight(label);
    }

  // Placement
  just_x(tx,ix,tw,13);
  just_y(ty,iy,th,13);

  // Widget background
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);

  // Check background
  if(check==MAYBE || !isEnabled())
    dc.setForeground(baseColor);
  else
    dc.setForeground(boxColor);
  dc.fillRectangle(ix+2,iy+2,9,9);

  // Check border
  if(options&CHECKBUTTON_PLUS){
    dc.setForeground(textColor);
    dc.drawRectangle(ix+2,iy+2,8,8);
    }
  else{
    dc.setForeground(shadowColor);
    dc.fillRectangle(ix,iy,12,1);
    dc.fillRectangle(ix,iy,1,12);
    dc.setForeground(borderColor);
    dc.fillRectangle(ix+1,iy+1,10,1);
    dc.fillRectangle(ix+1,iy+1,1,10);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(ix,iy+12,13,1);
    dc.fillRectangle(ix+12,iy,1,13);
    dc.setForeground(baseColor);
    dc.fillRectangle(ix+1,iy+11,11,1);
    dc.fillRectangle(ix+11,iy+1,1,11);
    }

  // Check color
  if(check==MAYBE || !isEnabled())
    dc.setForeground(shadowColor);
  else
    dc.setForeground(checkColor);

  // Show as +
  if(options&CHECKBUTTON_PLUS){
    if(check!=TRUE){
      dc.fillRectangle(ix+6,iy+4,1,5);
      }
    dc.fillRectangle(ix+4,iy+6,5,1);
    }

  // Show as v
  else{
    if(check!=FALSE){
      FXSegment seg[6];
#ifndef WIN32
      seg[0].x1=3+ix; seg[0].y1=5+iy; seg[0].x2=5+ix; seg[0].y2=7+iy;
      seg[1].x1=3+ix; seg[1].y1=6+iy; seg[1].x2=5+ix; seg[1].y2=8+iy;
      seg[2].x1=3+ix; seg[2].y1=7+iy; seg[2].x2=5+ix; seg[2].y2=9+iy;
      seg[3].x1=5+ix; seg[3].y1=7+iy; seg[3].x2=9+ix; seg[3].y2=3+iy;
      seg[4].x1=5+ix; seg[4].y1=8+iy; seg[4].x2=9+ix; seg[4].y2=4+iy;
      seg[5].x1=5+ix; seg[5].y1=9+iy; seg[5].x2=9+ix; seg[5].y2=5+iy;
#else
      seg[0].x1=3+ix; seg[0].y1=5+iy; seg[0].x2=5+ix; seg[0].y2=7+iy;
      seg[1].x1=3+ix; seg[1].y1=6+iy; seg[1].x2=5+ix; seg[1].y2=8+iy;
      seg[2].x1=3+ix; seg[2].y1=7+iy; seg[2].x2=5+ix; seg[2].y2=9+iy;
      seg[3].x1=5+ix; seg[3].y1=7+iy; seg[3].x2=10+ix; seg[3].y2=2+iy;
      seg[4].x1=5+ix; seg[4].y1=8+iy; seg[4].x2=10+ix; seg[4].y2=3+iy;
      seg[5].x1=5+ix; seg[5].y1=9+iy; seg[5].x2=10+ix; seg[5].y2=4+iy;
#endif
      dc.drawLineSegments(seg,6);
      }
    }

  // Text
  if(!label.empty()){
    dc.setFont(font);
    if(isEnabled()){
      dc.setForeground(textColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      if(hasFocus()){
        dc.drawFocusRectangle(tx-1,ty-1,tw+2,th+2);
        }
      }
    else{
      dc.setForeground(hiliteColor);
      drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      }
    }

  // Frame
  drawFrame(dc,0,0,width,height);

  return 1;
  }


// Set check color
void FXCheckButton::setCheckColor(FXColor clr){
  if(clr!=checkColor){
    checkColor=clr;
    update();
    }
  }


// Set box color
void FXCheckButton::setBoxColor(FXColor clr){
  if(clr!=boxColor){
    boxColor=clr;
    update();
    }
  }


// Change check button style
void FXCheckButton::setCheckButtonStyle(FXuint style){
  FXuint opts=(options&~CHECKBUTTON_MASK) | (style&CHECKBUTTON_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Return current check button style
FXuint FXCheckButton::getCheckButtonStyle() const {
  return (options&CHECKBUTTON_MASK);
  }


// Save object to stream
void FXCheckButton::save(FXStream& store) const {
  FXLabel::save(store);
  store << checkColor;
  store << boxColor;
  }


// Load object from stream
void FXCheckButton::load(FXStream& store){
  FXLabel::load(store);
  store >> checkColor;
  store >> boxColor;
  }

}
