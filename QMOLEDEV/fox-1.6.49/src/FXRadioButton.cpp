/********************************************************************************
*                                                                               *
*                  R a d i o   B u t t o n    O b j e c t                       *
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
* $Id: FXRadioButton.cpp,v 1.64 2006/01/22 17:58:38 fox Exp $                   *
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
#include "FXRadioButton.h"

/*
  To do:
  - Need check-style also (stay in when pressed, pop out when unpressed).
  - Who owns the icon(s)?
  - Arrow buttons should auto-repeat with a timer of some kind
  - "&Label\tTooltip\tHelptext\thttp://server/application/helponitem.html"
  - CheckButton should send SEL_COMMAND.
  - Default button mode:- should somehow get focus.
  - Weird state change still possible using both keyboard and mouse if two
    radio buttons are involved.
*/


#define RADIOBUTTON_MASK  (RADIOBUTTON_AUTOGRAY|RADIOBUTTON_AUTOHIDE)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXRadioButton) FXRadioButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXRadioButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXRadioButton::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXRadioButton::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXRadioButton::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXRadioButton::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXRadioButton::onFocusOut),
  FXMAPFUNC(SEL_UNGRABBED,0,FXRadioButton::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXRadioButton::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXRadioButton::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXRadioButton::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXRadioButton::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXRadioButton::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXRadioButton::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXRadioButton::ID_CHECK,FXRadioButton::onCheck),
  FXMAPFUNC(SEL_COMMAND,FXRadioButton::ID_UNCHECK,FXRadioButton::onUncheck),
  FXMAPFUNC(SEL_COMMAND,FXRadioButton::ID_UNKNOWN,FXRadioButton::onUnknown),
  FXMAPFUNC(SEL_COMMAND,FXRadioButton::ID_SETVALUE,FXRadioButton::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXRadioButton::ID_SETINTVALUE,FXRadioButton::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXRadioButton::ID_GETINTVALUE,FXRadioButton::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXRadioButton,FXLabel,FXRadioButtonMap,ARRAYNUMBER(FXRadioButtonMap))


// Deserialization
FXRadioButton::FXRadioButton(){
  radioColor=0;
  diskColor=0;
  check=FALSE;
  oldcheck=FALSE;
  }


// Make a check button
FXRadioButton::FXRadioButton(FXComposite* p,const FXString& text,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,text,NULL,opts,x,y,w,h,pl,pr,pt,pb){
  radioColor=getApp()->getForeColor();
  diskColor=getApp()->getBackColor();
  target=tgt;
  message=sel;
  check=FALSE;
  oldcheck=FALSE;
  }


// If window can have focus
bool FXRadioButton::canFocus() const { return true; }


// Get default width
FXint FXRadioButton::getDefaultWidth(){
  FXint tw=0,s=0,w;
  if(!label.empty()){
    tw=labelWidth(label);
    s=4;
    }
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w=FXMAX(tw,13); else w=tw+13+s;
  return padleft+padright+w+(border<<1);
  }


// Get default height
FXint FXRadioButton::getDefaultHeight(){
  FXint th=0,h;
  if(!label.empty()){
    th=labelHeight(label);
    }
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h=FXMAX(th,13); else h=th+13;
  return padtop+padbottom+h+(border<<1);
  }


// Check button
void FXRadioButton::setCheck(FXbool s,FXbool notify){
  if(check!=s){
    check=s;
    update();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check);}
    }
  }


// Change state to checked
long FXRadioButton::onCheck(FXObject*,FXSelector,void*){
  setCheck(TRUE);
  return 1;
  }


// Change state to unchecked
long FXRadioButton::onUncheck(FXObject*,FXSelector,void*){
  setCheck(FALSE);
  return 1;
  }


// Change state to indeterminate
long FXRadioButton::onUnknown(FXObject*,FXSelector,void*){
  setCheck(MAYBE);
  return 1;
  }


// Update value from a message
long FXRadioButton::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)(FXuval)ptr);
  return 1;
  }


// Update value from a message
long FXRadioButton::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXRadioButton::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCheck();
  return 1;
  }


// Implement auto-hide or auto-gray modes
long FXRadioButton::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXLabel::onUpdate(sender,sel,ptr)){
    if(options&RADIOBUTTON_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&RADIOBUTTON_AUTOGRAY){disable();}
    }
  return 1;
  }


// Gained focus
long FXRadioButton::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusIn(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Lost focus
long FXRadioButton::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusOut(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Entered button
long FXRadioButton::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled() && (flags&FLAG_PRESSED)) setCheck(TRUE);
  return 1;
  }


// Left button
long FXRadioButton::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled() && (flags&FLAG_PRESSED)) setCheck(oldcheck);
  return 1;
  }


// Pressed mouse button
long FXRadioButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    oldcheck=check;
    setCheck(TRUE);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released mouse button
long FXRadioButton::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled() && (flags&FLAG_PRESSED)){
    ungrab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(check!=oldcheck && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE);
    return 1;
    }
  return 0;
  }


// Lost the grab for some reason
long FXRadioButton::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onUngrabbed(sender,sel,ptr);
  setCheck(oldcheck);
  flags&=~FLAG_PRESSED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Key Press
long FXRadioButton::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      oldcheck=check;
      setCheck(TRUE);
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXRadioButton::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      if(check!=oldcheck && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXRadioButton::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    oldcheck=check;
    setCheck(TRUE);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    }
  return 1;
  }


// Hot key combination released
long FXRadioButton::onHotKeyRelease(FXObject*,FXSelector,void*){
  flags&=~FLAG_TIP;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(check!=oldcheck && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE);
    }
  return 1;
  }


// Handle repaint
long FXRadioButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXint tw=0,th=0,tx,ty,ix,iy;
  FXRectangle recs[6];
  FXDCWindow dc(this,ev);

  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);

  if(!label.empty()){
    tw=labelWidth(label);
    th=labelHeight(label);
    }

  just_x(tx,ix,tw,13);
  just_y(ty,iy,th,13);


/*
      012345678901

   0      SSSS      0
   1    SSBBBBSS    1
   2   SBB    BBW   2
   3   SB  BB  OW   3
   4  SB  BBBB  OW  4
   5  SB BBBBBB OW  5
   6  SB BBBBBB OW  6
   7  SB  BBBB  OW  7
   8   SB  BB  OW   8
   9   SBO    OOW   9
   0    WWOOOOWW    0
   1      WWWW      1

      012345678901
*/

  // Inside
  recs[0].x=ix+4; recs[0].y=iy+2; recs[0].w=4; recs[0].h=1;
  recs[1].x=ix+3; recs[1].y=iy+3; recs[1].w=6; recs[1].h=1;
  recs[2].x=ix+2; recs[2].y=iy+4; recs[2].w=8; recs[2].h=4;
  recs[3].x=ix+3; recs[3].y=iy+8; recs[3].w=6; recs[3].h=1;
  recs[4].x=ix+4; recs[4].y=iy+9; recs[4].w=4; recs[4].h=1;
  if(!isEnabled())                   // fix by Daniel Gehriger (gehriger@linkcad.com)
    dc.setForeground(baseColor);
  else
    dc.setForeground(diskColor);
  dc.fillRectangles(recs,5);

  // Top left outside
  recs[0].x=ix+4; recs[0].y=iy+0; recs[0].w=4; recs[0].h=1;
  recs[1].x=ix+2; recs[1].y=iy+1; recs[1].w=2; recs[1].h=1;
  recs[2].x=ix+8; recs[2].y=iy+1; recs[2].w=2; recs[2].h=1;
  recs[3].x=ix+1; recs[3].y=iy+2; recs[3].w=1; recs[3].h=2;
  recs[4].x=ix+0; recs[4].y=iy+4; recs[4].w=1; recs[4].h=4;
  recs[5].x=ix+1; recs[5].y=iy+8; recs[5].w=1; recs[5].h=2;
  dc.setForeground(shadowColor);
  dc.fillRectangles(recs,6);

  // Top left inside
  recs[0].x=ix+4; recs[0].y=iy+1; recs[0].w=4; recs[0].h=1;
  recs[1].x=ix+2; recs[1].y=iy+2; recs[1].w=2; recs[1].h=1;
  recs[2].x=ix+8; recs[2].y=iy+2; recs[2].w=2; recs[2].h=1;
  recs[3].x=ix+2; recs[3].y=iy+3; recs[3].w=1; recs[3].h=1;
  recs[4].x=ix+1; recs[4].y=iy+4; recs[4].w=1; recs[4].h=4;
  recs[5].x=ix+2; recs[5].y=iy+8; recs[5].w=1; recs[5].h=2;
  dc.setForeground(borderColor);
  dc.fillRectangles(recs,6);

  // Bottom right outside
  recs[0].x=ix+10;recs[0].y=iy+2; recs[0].w=1; recs[0].h=2;
  recs[1].x=ix+11;recs[1].y=iy+4; recs[1].w=1; recs[1].h=4;
  recs[2].x=ix+10;recs[2].y=iy+8; recs[2].w=1; recs[2].h=2;
  recs[3].x=ix+8; recs[3].y=iy+10;recs[3].w=2; recs[3].h=1;
  recs[4].x=ix+2; recs[4].y=iy+10;recs[4].w=2; recs[4].h=1;
  recs[5].x=ix+4; recs[5].y=iy+11;recs[5].w=4; recs[5].h=1;
  dc.setForeground(hiliteColor);
  dc.fillRectangles(recs,6);

  // Bottom right inside
  recs[0].x=ix+9; recs[0].y=iy+3; recs[0].w=1; recs[0].h=1;
  recs[1].x=ix+10;recs[1].y=iy+4; recs[1].w=1; recs[1].h=4;
  recs[2].x=ix+9; recs[2].y=iy+8; recs[2].w=1; recs[2].h=1;
  recs[3].x=ix+8; recs[3].y=iy+9; recs[3].w=2; recs[3].h=1;
  recs[4].x=ix+3; recs[4].y=iy+9; recs[4].w=1; recs[4].h=1;
  recs[5].x=ix+4; recs[5].y=iy+10;recs[5].w=4; recs[5].h=1;
  dc.setForeground(baseColor);
  dc.fillRectangles(recs,6);

  // Ball inside
  if(check!=FALSE){
    recs[0].x=ix+5; recs[0].y=iy+4; recs[0].w=2; recs[0].h=1;
    recs[1].x=ix+4; recs[1].y=iy+5; recs[1].w=4; recs[1].h=2;
    recs[2].x=ix+5; recs[2].y=iy+7; recs[2].w=2; recs[2].h=1;
    if(isEnabled())
      dc.setForeground(radioColor);
    else
      dc.setForeground(shadowColor);
    dc.fillRectangles(recs,3);
    }

  // Label
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
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Set radio color
void FXRadioButton::setRadioColor(FXColor clr){
  if(radioColor!=clr){
    radioColor=clr;
    update();
    }
  }


// Set disk color
void FXRadioButton::setDiskColor(FXColor clr){
  if(clr!=diskColor){
    diskColor=clr;
    update();
    }
  }


// Change radio button style
void FXRadioButton::setRadioButtonStyle(FXuint style){
  FXuint opts=(options&~RADIOBUTTON_MASK) | (style&RADIOBUTTON_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Return current radio button style
FXuint FXRadioButton::getRadioButtonStyle() const {
  return (options&RADIOBUTTON_MASK);
  }


// Save object to stream
void FXRadioButton::save(FXStream& store) const {
  FXLabel::save(store);
  store << radioColor;
  store << diskColor;
  }


// Load object from stream
void FXRadioButton::load(FXStream& store){
  FXLabel::load(store);
  store >> radioColor;
  store >> diskColor;
  }

}

