/********************************************************************************
*                                                                               *
*                   T o g g l e    B u t t o n    O b j e c t                   *
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
* $Id: FXToggleButton.cpp,v 1.63.2.1 2006/12/11 15:57:26 fox Exp $                  *
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
#include "FXAccelTable.h"
#include "FXDCWindow.h"
#include "FXIcon.h"
#include "FXToggleButton.h"


/*
  Notes:
  - If the altlabel is empty, the normal label will be used;
    likewise for the icon.
  - Setting the alt label will also change the alt hotkey.
  - The fallback logic needs to be properly adhered to in the size computation.
*/


// ToggleButton styles
#define TOGGLEBUTTON_MASK (TOGGLEBUTTON_AUTOGRAY|TOGGLEBUTTON_AUTOHIDE|TOGGLEBUTTON_TOOLBAR|TOGGLEBUTTON_KEEPSTATE)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXToggleButton) FXToggleButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXToggleButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXToggleButton::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXToggleButton::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXToggleButton::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXToggleButton::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXToggleButton::onFocusOut),
  FXMAPFUNC(SEL_UNGRABBED,0,FXToggleButton::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXToggleButton::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXToggleButton::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXToggleButton::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXToggleButton::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXToggleButton::ID_HOTKEY,FXToggleButton::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXToggleButton::ID_HOTKEY,FXToggleButton::onHotKeyRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXToggleButton::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXToggleButton::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXToggleButton::ID_CHECK,FXToggleButton::onCheck),
  FXMAPFUNC(SEL_COMMAND,FXToggleButton::ID_UNCHECK,FXToggleButton::onUncheck),
  FXMAPFUNC(SEL_COMMAND,FXToggleButton::ID_SETVALUE,FXToggleButton::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXToggleButton::ID_SETINTVALUE,FXToggleButton::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXToggleButton::ID_GETINTVALUE,FXToggleButton::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXToggleButton,FXLabel,FXToggleButtonMap,ARRAYNUMBER(FXToggleButtonMap))


// Deserialization
FXToggleButton::FXToggleButton(){
  alticon=(FXIcon*)-1L;
  state=FALSE;
  down=FALSE;
  }


// Construct and init
FXToggleButton::FXToggleButton(FXComposite* p,const FXString& text1,const FXString& text2,FXIcon* icon1,FXIcon* icon2,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,text1,icon1,opts,x,y,w,h,pl,pr,pt,pb){
  FXString string=text2.section('\t',0);
  target=tgt;
  message=sel;
  altlabel=stripHotKey(string);
  alttip=text2.section('\t',1);
  althelp=text2.section('\t',2);
  alticon=icon2;
  althotkey=parseHotKey(string);
  althotoff=findHotKey(string);
  addHotKey(althotkey);
  state=FALSE;
  down=FALSE;
  }


// Create window
void FXToggleButton::create(){
  FXLabel::create();
  if(alticon) alticon->create();
  }


// Detach window
void FXToggleButton::detach(){
  FXLabel::detach();
  if(alticon) alticon->detach();
  }


// Get default width
FXint FXToggleButton::getDefaultWidth(){
  FXint tw,iw,s,w1,w2;

  tw=iw=s=0;
  if(!label.empty()) tw=labelWidth(label);
  if(icon) iw=icon->getWidth();
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w1=FXMAX(tw,iw); else w1=tw+iw+s;

  if(!altlabel.empty()) tw=labelWidth(altlabel);
  if(alticon) iw=alticon->getWidth();
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w2=FXMAX(tw,iw); else w2=tw+iw+s;

  return FXMAX(w1,w2)+padleft+padright+(border<<1);
  }


// Get default height
FXint FXToggleButton::getDefaultHeight(){
  FXint th,ih,h1,h2;

  th=ih=0;
  if(!label.empty()) th=labelHeight(label);
  if(icon) ih=icon->getHeight();
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h1=FXMAX(th,ih); else h1=th+ih;

  if(!altlabel.empty()) th=labelHeight(altlabel);
  if(alticon) ih=alticon->getHeight();
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h2=FXMAX(th,ih); else h2=th+ih;

  return FXMAX(h1,h2)+padtop+padbottom+(border<<1);
  }


// Set button state
void FXToggleButton::setState(FXbool s,FXbool notify){
  if(state!=s){
    state=s;
    update();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)state);}
    }
  }


// Press button
void FXToggleButton::press(FXbool dn){
  if(down!=dn){
    down=dn;
    update();
    }
  }


// If window can have focus
bool FXToggleButton::canFocus() const { return true; }


// Update value from a message
long FXToggleButton::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setState((FXint)(FXuval)ptr);
  return 1;
  }


// Update value from a message
long FXToggleButton::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setState(*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXToggleButton::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getState();
  return 1;
  }


// Check the menu button
long FXToggleButton::onCheck(FXObject*,FXSelector,void*){
  setState(TRUE);
  return 1;
  }


// Check the menu button
long FXToggleButton::onUncheck(FXObject*,FXSelector,void*){
  setState(FALSE);
  return 1;
  }


// Implement auto-hide or auto-gray modes
long FXToggleButton::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXLabel::onUpdate(sender,sel,ptr)){
    if(options&TOGGLEBUTTON_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&TOGGLEBUTTON_AUTOGRAY){disable();}
    }
  return 1;
  }


// Gained focus
long FXToggleButton::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusIn(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Lost focus
long FXToggleButton::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusOut(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Entered button
long FXToggleButton::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled()){
    if(flags&FLAG_PRESSED) press(TRUE);
    if(options&TOGGLEBUTTON_TOOLBAR) update();
    }
  return 1;
  }


// Left button
long FXToggleButton::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled()){
    if(flags&FLAG_PRESSED) press(FALSE);
    if(options&TOGGLEBUTTON_TOOLBAR) update();
    }
  return 1;
  }


// Pressed mouse button
long FXToggleButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    press(TRUE);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released mouse button
long FXToggleButton::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXbool click=down;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    press(FALSE);
    if(click){
      setState(!state);
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)state);
      }
    return 1;
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXToggleButton::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onUngrabbed(sender,sel,ptr);
  press(FALSE);
  flags&=~FLAG_PRESSED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Key Press
long FXToggleButton::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      press(TRUE);
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXToggleButton::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      press(FALSE);
      setState(!state);
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)state);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXToggleButton::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  FXTRACE((100,"FXToggleButton::onHotKeyPress\n"));
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    press(TRUE);
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    }
  return 1;
  }


// Hot key combination released
long FXToggleButton::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXTRACE((100,"FXToggleButton::onHotKeyRelease\n"));
  if(isEnabled() && (flags&FLAG_PRESSED)){
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    press(FALSE);
    setState(!state);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)state);
    }
  return 1;
  }


// We were asked about status text
long FXToggleButton::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if(flags&FLAG_HELP){
    if(state){
      if(!althelp.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&althelp);
        return 1;
        }
      }
    if(!help.empty()){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
      return 1;
      }
    }
  return 0;
  }


// We were asked about tip text
long FXToggleButton::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if(flags&FLAG_TIP){
    if(state){
      if(!alttip.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&alttip);
        return 1;
        }
      }
    if(!tip.empty()){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
      return 1;
      }
    }
  return 0;
  }


// Handle repaint
long FXToggleButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXint tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);

  // Got a border at all?
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){

    // Toolbar style
    if(options&TOGGLEBUTTON_TOOLBAR){

      // Enabled and cursor inside and down
      if(down || ((options&TOGGLEBUTTON_KEEPSTATE) && state)){
        dc.setForeground(hiliteColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
        else drawSunkenRectangle(dc,0,0,width,height);
        }

      // Enabled and cursor inside, and up
      else if(isEnabled() && underCursor()){
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
        else drawRaisedRectangle(dc,0,0,width,height);
        }

      // Disabled or unchecked or not under cursor
      else{
        dc.setForeground(backColor);
        dc.fillRectangle(0,0,width,height);
        }
      }

    // Normal style
    else{

      // Draw sunken if pressed
      if(down || ((options&TOGGLEBUTTON_KEEPSTATE) && state)){
        dc.setForeground(hiliteColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
        else drawSunkenRectangle(dc,0,0,width,height);
        }

      // Draw raised if not currently pressed down
      else{
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
        else drawRaisedRectangle(dc,0,0,width,height);
        }

      }
    }

  // No borders
  else{
    dc.setForeground(backColor);
    dc.fillRectangle(0,0,width,height);
    }

  // Place text & icon
  if(state && !altlabel.empty()){
    tw=labelWidth(altlabel);
    th=labelHeight(altlabel);
    }
  else if(!label.empty()){
    tw=labelWidth(label);
    th=labelHeight(label);
    }
  if(state && alticon){
    iw=alticon->getWidth();
    ih=alticon->getHeight();
    }
  else if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }

  just_x(tx,ix,tw,iw);
  just_y(ty,iy,th,ih);

  // Shift a bit when pressed
  if((down || ((options&TOGGLEBUTTON_KEEPSTATE) && state)) && (options&(FRAME_RAISED|FRAME_SUNKEN))){ ++tx; ++ty; ++ix; ++iy; }

  // Draw enabled state
  if(isEnabled()){
    if(state && alticon){
      dc.drawIcon(alticon,ix,iy);
      }
    else if(icon){
      dc.drawIcon(icon,ix,iy);
      }
    if(state && !altlabel.empty()){
      dc.setFont(font);
      dc.setForeground(textColor);
      drawLabel(dc,altlabel,althotoff,tx,ty,tw,th);
      }
    else if(!label.empty()){
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
    if(state && alticon){
      dc.drawIconSunken(alticon,ix,iy);
      }
    else if(icon){
      dc.drawIconSunken(icon,ix,iy);
      }
    if(state && !altlabel.empty()){
      dc.setFont(font);
      dc.setForeground(hiliteColor);
      drawLabel(dc,altlabel,althotoff,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,altlabel,althotoff,tx,ty,tw,th);
      }
    else if(!label.empty()){
      dc.setFont(font);
      dc.setForeground(hiliteColor);
      drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      }
    }
  return 1;
  }


// Change text
void FXToggleButton::setAltText(const FXString& text){
  FXString string=stripHotKey(text);
  FXHotKey hkey=parseHotKey(text);
  FXint hoff=findHotKey(text);
  if(altlabel!=string || althotkey!=hkey || althotoff!=hoff){
    altlabel.adopt(string);
    remHotKey(althotkey);
    althotkey=hkey;
    althotoff=hoff;
    addHotKey(althotkey);
    recalc();
    update();
    }
  }


// Change icon
void FXToggleButton::setAltIcon(FXIcon* ic){
  if(alticon!=ic){
    alticon=ic;
    recalc();
    update();
    }
  }


// Change help text
void FXToggleButton::setAltHelpText(const FXString& text){
  althelp=text;
  }


// Change tip text
void FXToggleButton::setAltTipText(const FXString& text){
  alttip=text;
  }


// Set icon positioning
void FXToggleButton::setToggleStyle(FXuint style){
  FXuint opts=(options&~TOGGLEBUTTON_MASK) | (style&TOGGLEBUTTON_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get icon positioning
FXuint FXToggleButton::getToggleStyle() const {
  return (options&TOGGLEBUTTON_MASK);
  }


// Save object to stream
void FXToggleButton::save(FXStream& store) const {
  FXLabel::save(store);
  store << altlabel;
  store << alticon;
  store << althotkey;
  store << althotoff;
  store << alttip;
  store << althelp;
  store << state;
  }



// Load object from stream
void FXToggleButton::load(FXStream& store){
  FXLabel::load(store);
  store >> altlabel;
  store >> alticon;
  store >> althotkey;
  store >> althotoff;
  store >> alttip;
  store >> althelp;
  store >> state;
  }


// Destruct
FXToggleButton::~FXToggleButton(){
  remHotKey(althotkey);
  alticon=(FXIcon*)-1L;
  }

}
