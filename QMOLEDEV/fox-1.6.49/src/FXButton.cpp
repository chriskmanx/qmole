/********************************************************************************
*                                                                               *
*                           B u t t o n    O b j e c t s                        *
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
* $Id: FXButton.cpp,v 1.67 2006/01/22 17:58:18 fox Exp $                        *
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
#include "FXIcon.h"
#include "FXShell.h"
#include "FXButton.h"


/*
  Notes:
  - Use flags for button instead of a whole integer
  - Add ``flat'' toolbar style also
  - Need check-style also (stay in when pressed, pop out when unpressed).
  - Who owns the icon(s)?
  - Arrow buttons should auto-repeat with a timer of some kind
  - "&Label\tTooltip\tHelptext\thttp://server/application/helponitem.html"
  - CheckButton should send SEL_COMMAND.
  - Default button mode:- should somehow get focus.
  - Add button multiple-click translations elsewhere
  - Button should be able to behave like a check (radio) button.
  - Need to draw ``around'' the icon etc. So it doesn't flash to background.
*/

// Button styles
#define BUTTON_MASK (BUTTON_AUTOGRAY|BUTTON_AUTOHIDE|BUTTON_TOOLBAR|BUTTON_DEFAULT|BUTTON_INITIAL)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXButton) FXButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXButton::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXButton::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXButton::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXButton::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXButton::onFocusOut),
  FXMAPFUNC(SEL_UNGRABBED,0,FXButton::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXButton::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXButton::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXButton::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXButton::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXButton::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXButton::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_CHECK,FXButton::onCheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNCHECK,FXButton::onUncheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXButton::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXButton::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXButton::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXButton,FXLabel,FXButtonMap,ARRAYNUMBER(FXButtonMap))


// Deserialization
FXButton::FXButton(){
  state=STATE_UP;
  }


// Construct and init
FXButton::FXButton(FXComposite* p,const FXString& text,FXIcon* ic,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,text,ic,opts,x,y,w,h,pl,pr,pt,pb){
  target=tgt;
  message=sel;
  state=STATE_UP;
  if(options&BUTTON_INITIAL){
    setInitial(TRUE);
    setDefault(TRUE);
    }
  }


// If window can have focus
bool FXButton::canFocus() const { return true; }


// Set focus to this widget
void FXButton::setFocus(){
  FXLabel::setFocus();
  if(options&BUTTON_DEFAULT) setDefault(TRUE);
  update();
  }


// Kill focus to this widget
void FXButton::killFocus(){
  FXLabel::killFocus();
  if(options&BUTTON_DEFAULT) setDefault(MAYBE);
  update();
  }


// Make widget drawn as default
void FXButton::setDefault(FXbool enable){
  FXLabel::setDefault(enable);
  update();
  }


// Set button state
void FXButton::setState(FXuint s){
  if(state!=s){
    state=s;
    update();
    }
  }


// Update value from a message
long FXButton::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setState((FXuint)(FXuval)ptr);
  return 1;
  }


// Update value from a message
long FXButton::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setState(*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXButton::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getState();
  return 1;
  }


// Check the menu button
long FXButton::onCheck(FXObject*,FXSelector,void*){
  setState(STATE_ENGAGED);
  return 1;
  }


// Check the menu button
long FXButton::onUncheck(FXObject*,FXSelector,void*){
  setState(STATE_UP);
  return 1;
  }


// Implement auto-hide or auto-gray modes
long FXButton::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXLabel::onUpdate(sender,sel,ptr)){
    if(options&BUTTON_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&BUTTON_AUTOGRAY){disable();}
    }
  return 1;
  }


// Gained focus
long FXButton::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusIn(sender,sel,ptr);
  update();
  return 1;
  }


// Lost focus
long FXButton::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusOut(sender,sel,ptr);
  update();
  return 1;
  }


// Entered button
long FXButton::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled()){
    if((flags&FLAG_PRESSED) && (state!=STATE_ENGAGED)) setState(STATE_DOWN);
    if(options&BUTTON_TOOLBAR) update();
    }
  return 1;
  }


// Left button
long FXButton::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled()){
    if((flags&FLAG_PRESSED) && (state!=STATE_ENGAGED)) setState(STATE_UP);
    if(options&BUTTON_TOOLBAR) update();
    }
  return 1;
  }


// Pressed mouse button
long FXButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(state!=STATE_ENGAGED) setState(STATE_DOWN);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released mouse button
long FXButton::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXbool click=(state==STATE_DOWN);
  if(isEnabled() && (flags&FLAG_PRESSED)){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(state!=STATE_ENGAGED) setState(STATE_UP);
    if(click && target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1); }
    return 1;
    }
  return 0;
  }


// Lost the grab for some reason
long FXButton::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onUngrabbed(sender,sel,ptr);
  if(state!=STATE_ENGAGED) setState(STATE_UP);
  flags&=~FLAG_PRESSED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Key Press
long FXButton::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if((event->code==KEY_space || event->code==KEY_KP_Space) || (isDefault() && (event->code==KEY_Return || event->code==KEY_KP_Enter))){
      if(state!=STATE_ENGAGED) setState(STATE_DOWN);
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXButton::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXbool click=(state==STATE_DOWN);
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if((event->code==KEY_space || event->code==KEY_KP_Space) || (isDefault() && (event->code==KEY_Return || event->code==KEY_KP_Enter))){
      if(state!=STATE_ENGAGED) setState(STATE_UP);
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      if(click && target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1); }
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXButton::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(state!=STATE_ENGAGED) setState(STATE_DOWN);
    flags&=~FLAG_UPDATE;
    flags|=FLAG_PRESSED;
    }
  return 1;
  }


// Hot key combination released
long FXButton::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXuint click=(state==STATE_DOWN);
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(state!=STATE_ENGAGED) setState(STATE_UP);
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(click && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
    }
  return 1;
  }


// Handle repaint
long FXButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXint tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
  FXEvent *ev=(FXEvent*)ptr;

  // Start drawing
  FXDCWindow dc(this,ev);

  // Got a border at all?
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){

    // Toolbar style
    if(options&BUTTON_TOOLBAR){

      // Enabled and cursor inside, and up
      if(isEnabled() && underCursor() && (state==STATE_UP)){
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
        else drawRaisedRectangle(dc,0,0,width,height);
        }

      // Enabled and cursor inside and down
      else if(isEnabled() && underCursor() && (state==STATE_DOWN)){
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
        else drawSunkenRectangle(dc,0,0,width,height);
        }

      // Enabled and checked
      else if(isEnabled() && (state==STATE_ENGAGED)){
        dc.setForeground(hiliteColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
        else drawSunkenRectangle(dc,0,0,width,height);
        }

      // Disabled or unchecked or not under cursor
      else{
        dc.setForeground(backColor);
        dc.fillRectangle(0,0,width,height);
        }
      }

    // Normal style
    else{

      // Default
      if(isDefault()){

        // Draw in up state if disabled or up
        if(!isEnabled() || (state==STATE_UP)){
          dc.setForeground(backColor);
          dc.fillRectangle(border+1,border+1,width-border*2-1,height-border*2-1);
          if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,1,1,width-1,height-1);
          else drawRaisedRectangle(dc,1,1,width-1,height-1);
          }

        // Draw sunken if enabled and either checked or pressed
        else{
          if(state==STATE_ENGAGED) dc.setForeground(hiliteColor); else dc.setForeground(backColor);
          dc.fillRectangle(border,border,width-border*2-1,height-border*2-1);
          if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width-1,height-1);
          else drawSunkenRectangle(dc,0,0,width-1,height-1);
          }

        // Black default border
        drawBorderRectangle(dc,0,0,width,height);
        }

      // Non-Default
      else{

        // Draw in up state if disabled or up
        if(!isEnabled() || (state==STATE_UP)){
          dc.setForeground(backColor);
          dc.fillRectangle(border,border,width-border*2,height-border*2);
          if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
          else drawRaisedRectangle(dc,0,0,width,height);
          }

        // Draw sunken if enabled and either checked or pressed
        else{
          if(state==STATE_ENGAGED) dc.setForeground(hiliteColor); else dc.setForeground(backColor);
          dc.fillRectangle(border,border,width-border*2,height-border*2);
          if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
          else drawSunkenRectangle(dc,0,0,width,height);
          }
        }
      }
    }

  // No borders
  else{
    if(isEnabled() && (state==STATE_ENGAGED)){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(0,0,width,height);
      }
    else{
      dc.setForeground(backColor);
      dc.fillRectangle(0,0,width,height);
      }
    }

  // Place text & icon
  if(!label.empty()){
    tw=labelWidth(label);
    th=labelHeight(label);
    }
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }

  just_x(tx,ix,tw,iw);
  just_y(ty,iy,th,ih);

  // Shift a bit when pressed
  if(state && (options&(FRAME_RAISED|FRAME_SUNKEN))){ ++tx; ++ty; ++ix; ++iy; }

  // Draw enabled state
  if(isEnabled()){
    if(icon){
      dc.drawIcon(icon,ix,iy);
      }
    if(!label.empty()){
      dc.setFont(font);
      dc.setForeground(textColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      }
    if(hasFocus()){
      dc.drawFocusRectangle(border+1,border+1,width-2*border-2,height-2*border-2);
      }
    }

  // Draw grayed-out state
  else{
    if(icon){
      dc.drawIconSunken(icon,ix,iy);
      }
    if(!label.empty()){
      dc.setFont(font);
      dc.setForeground(hiliteColor);
      drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      }
    }
  return 1;
  }


// Set button style
void FXButton::setButtonStyle(FXuint style){
  FXuint opts=(options&~BUTTON_MASK) | (style&BUTTON_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get button style
FXuint FXButton::getButtonStyle() const {
  return (options&BUTTON_MASK);
  }

}
