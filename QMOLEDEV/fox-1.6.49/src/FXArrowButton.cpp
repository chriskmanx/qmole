/********************************************************************************
*                                                                               *
*                     A r r o w   B u t t o n    O b j e c t                    *
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
* $Id: FXArrowButton.cpp,v 1.50 2006/01/22 17:58:17 fox Exp $                   *
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
#include "FXArrowButton.h"


/*
  Notes:
  - Automatic mode works by simply hovering cursor over arrow button; it is
    used for scrolling menus.
  - Possible interactions between auto mode and manual pressing..
*/


// Justification
#define JUSTIFY_MASK  (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)

// Arrow styles
#define ARROW_MASK    (ARROW_UP|ARROW_DOWN|ARROW_LEFT|ARROW_RIGHT|ARROW_AUTO|ARROW_REPEAT|ARROW_AUTOGRAY|ARROW_AUTOHIDE|ARROW_TOOLBAR)

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXArrowButton) FXArrowButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXArrowButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXArrowButton::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXArrowButton::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXArrowButton::onLeave),
  FXMAPFUNC(SEL_TIMEOUT,FXArrowButton::ID_AUTO,FXArrowButton::onAuto),
  FXMAPFUNC(SEL_TIMEOUT,FXArrowButton::ID_REPEAT,FXArrowButton::onRepeat),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXArrowButton::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXArrowButton::onLeftBtnRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXArrowButton::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXArrowButton::onQueryHelp),
  FXMAPFUNC(SEL_UNGRABBED,0,FXArrowButton::onUngrabbed),
  FXMAPFUNC(SEL_KEYPRESS,0,FXArrowButton::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXArrowButton::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXArrowButton::ID_HOTKEY,FXArrowButton::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXArrowButton::ID_HOTKEY,FXArrowButton::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXArrowButton::ID_SETHELPSTRING,FXArrowButton::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXArrowButton::ID_GETHELPSTRING,FXArrowButton::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXArrowButton::ID_SETTIPSTRING,FXArrowButton::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXArrowButton::ID_GETTIPSTRING,FXArrowButton::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXArrowButton,FXFrame,FXArrowButtonMap,ARRAYNUMBER(FXArrowButtonMap))


// For deserialization
FXArrowButton::FXArrowButton(){
  flags|=FLAG_ENABLED;
  arrowColor=0;
  arrowSize=9;
  state=FALSE;
  fired=FALSE;
  }


// Make a text button
FXArrowButton::FXArrowButton(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  arrowColor=getApp()->getForeColor();
  arrowSize=9;
  state=FALSE;
  fired=FALSE;
  }


// Get default size
FXint FXArrowButton::getDefaultWidth(){
  return padleft+padright+arrowSize+(border<<1);
  }


FXint FXArrowButton::getDefaultHeight(){
  return padtop+padbottom+arrowSize+(border<<1);
  }


// Enable the window
void FXArrowButton::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXFrame::enable();
    update();
    }
  }


// Disable the window
void FXArrowButton::disable(){
  if(flags&FLAG_ENABLED){
    FXFrame::disable();
    update();
    }
  }


// Set button state
void FXArrowButton::setState(FXbool s){
  if(state!=s){
    state=s;
    update();
    }
  }


// If window can have focus
bool FXArrowButton::canFocus() const { return true; }


// Implement auto-hide or auto-gray modes
long FXArrowButton::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXFrame::onUpdate(sender,sel,ptr)){
    if(options&ARROW_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&ARROW_AUTOGRAY){disable();}
    }
  return 1;
  }


// Press automatically
long FXArrowButton::onAuto(FXObject*,FXSelector,void*){
  setState(TRUE);
  getApp()->addTimeout(this,ID_REPEAT,getApp()->getScrollSpeed());
  flags&=~FLAG_UPDATE;
  fired=FALSE;
  return 1;
  }


// Entered button
long FXArrowButton::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onEnter(sender,sel,ptr);
  if(isEnabled()){
    if(flags&FLAG_PRESSED){
      setState(TRUE);
      }
    else if(options&ARROW_AUTO){
      if(options&ARROW_REPEAT) getApp()->addTimeout(this,ID_AUTO,getApp()->getScrollDelay());
      }
    if(options&ARROW_TOOLBAR) update();
    }
  return 1;
  }


// Left button
long FXArrowButton::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onLeave(sender,sel,ptr);
  if(isEnabled()){
    if(flags&FLAG_PRESSED){
      setState(FALSE);
      }
    else if(options&ARROW_AUTO){
      setState(FALSE);
      if(options&ARROW_REPEAT) getApp()->removeTimeout(this,ID_AUTO);
      flags|=FLAG_UPDATE;
      fired=FALSE;
      }
    if(options&ARROW_TOOLBAR) update();
    }
  return 1;
  }


// Pressed mouse button
long FXArrowButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    setState(TRUE);
    getApp()->removeTimeout(this,ID_AUTO);
    if(options&ARROW_REPEAT) getApp()->addTimeout(this,ID_REPEAT,getApp()->getScrollDelay());
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    fired=FALSE;
    return 1;
    }
  return 0;
  }


// Released mouse button
long FXArrowButton::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXbool click=(!fired && state);
  if(isEnabled() && (flags&FLAG_PRESSED)){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    fired=FALSE;
    getApp()->removeTimeout(this,ID_REPEAT);
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    setState(FALSE);
    if(click && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
    return 1;
    }
  return 0;
  }


// Lost the grab for some reason
long FXArrowButton::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  setState(FALSE);
  getApp()->removeTimeout(this,ID_REPEAT);
  flags&=~FLAG_PRESSED;
  flags|=FLAG_UPDATE;
  fired=FALSE;
  return 1;
  }


// Repeat a click automatically
long FXArrowButton::onRepeat(FXObject*,FXSelector,void*){
  getApp()->addTimeout(this,ID_REPEAT,getApp()->getScrollSpeed());
  if(state && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
  fired=TRUE;
  return 1;
  }


// Key Press
long FXArrowButton::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      setState(TRUE);
      getApp()->removeTimeout(this,ID_AUTO);
      if(options&ARROW_REPEAT) getApp()->addTimeout(this,ID_REPEAT,getApp()->getScrollDelay());
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      fired=FALSE;
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXArrowButton::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXbool click=(!fired && state);
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      setState(FALSE);
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      fired=FALSE;
      getApp()->removeTimeout(this,ID_REPEAT);
      if(click && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXArrowButton::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    setState(TRUE);
    getApp()->removeTimeout(this,ID_AUTO);
    if(options&ARROW_REPEAT) getApp()->addTimeout(this,ID_REPEAT,getApp()->getScrollDelay());
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    fired=FALSE;
    }
  return 1;
  }


// Hot key combination released
long FXArrowButton::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXbool click=(!fired && state);
  if(isEnabled() && (flags&FLAG_PRESSED)){
    setState(FALSE);
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    fired=FALSE;
    getApp()->removeTimeout(this,ID_REPEAT);
    if(click && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
    }
  return 1;
  }


// Set help using a message
long FXArrowButton::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXArrowButton::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXArrowButton::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXArrowButton::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXArrowButton::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXArrowButton::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Handle repaint
long FXArrowButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent   *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXPoint    points[3];
  FXint      xx,yy,ww,hh,q;


  // With borders
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){

    // Toolbar style
    if(options&ARROW_TOOLBAR){

      // Enabled and cursor inside, and up
      if(isEnabled() && underCursor() && !state){
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
        else drawRaisedRectangle(dc,0,0,width,height);
        }

      // Enabled and cursor inside and down
      else if(isEnabled() && state){
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

      // Draw sunken if enabled and pressed
      if(isEnabled() && state){
        dc.setForeground(hiliteColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
        else drawSunkenRectangle(dc,0,0,width,height);
        }

      // Draw in up state if disabled or up
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
    if(isEnabled() && state){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(0,0,width,height);
      }
    else{
      dc.setForeground(backColor);
      dc.fillRectangle(0,0,width,height);
      }
    }

  // Compute size of the arrows....
  ww=width-padleft-padright-(border<<1);
  hh=height-padtop-padbottom-(border<<1);
  if(options&(ARROW_UP|ARROW_DOWN)){
    q=ww|1; if(q>(hh<<1)) q=(hh<<1)-1;
    ww=q; hh=q>>1;
    }
  else{
    q=hh|1; if(q>(ww<<1)) q=(ww<<1)-1;
    ww=q>>1; hh=q;
    }

  if(options&JUSTIFY_LEFT) xx=padleft+border;
  else if(options&JUSTIFY_RIGHT) xx=width-ww-padright-border;
  else xx=(width-ww)/2;

  if(options&JUSTIFY_TOP) yy=padtop+border;
  else if(options&JUSTIFY_BOTTOM) yy=height-hh-padbottom-border;
  else yy=(height-hh)/2;

  if(state){ ++xx; ++yy; }

  if(isEnabled())
    dc.setForeground(arrowColor);
  else
    dc.setForeground(shadowColor);

  // NB Size of arrow should stretch
  if(options&ARROW_UP){
    points[0].x=xx+(ww>>1);
    points[0].y=yy-1;
    points[1].x=xx;
    points[1].y=yy+hh;
    points[2].x=xx+ww;
    points[2].y=yy+hh;
    dc.fillPolygon(points,3);
    }
  else if(options&ARROW_DOWN){
    points[0].x=xx+1;
    points[0].y=yy;
    points[1].x=xx+ww-1;
    points[1].y=yy;
    points[2].x=xx+(ww>>1);
    points[2].y=yy+hh;
    dc.fillPolygon(points,3);
    }
  else if(options&ARROW_LEFT){
    points[0].x=xx+ww;
    points[0].y=yy;
    points[1].x=xx+ww;
    points[1].y=yy+hh-1;
    points[2].x=xx;
    points[2].y=yy+(hh>>1);
    dc.fillPolygon(points,3);
    }
  else if(options&ARROW_RIGHT){
    points[0].x=xx;
    points[0].y=yy;
    points[1].x=xx;
    points[1].y=yy+hh-1;
    points[2].x=xx+ww;
    points[2].y=yy+(hh>>1);
    dc.fillPolygon(points,3);
    }
  return 1;
  }


// Set arrow style
void FXArrowButton::setArrowStyle(FXuint style){
  FXuint opts=(options&~ARROW_MASK) | (style&ARROW_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get arrow style
FXuint FXArrowButton::getArrowStyle() const {
  return (options&ARROW_MASK);
  }


// Set default arrow size
void FXArrowButton::setArrowSize(FXint size){
  if(size!=arrowSize){
    arrowSize=size;
    recalc();
    }
  }


// Set text color
void FXArrowButton::setArrowColor(FXColor clr){
  if(clr!=arrowColor){
    arrowColor=clr;
    update();
    }
  }


// Set text justify style
void FXArrowButton::setJustify(FXuint style){
  FXuint opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get text justify style
FXuint FXArrowButton::getJustify() const {
  return (options&JUSTIFY_MASK);
  }


// Save object to stream
void FXArrowButton::save(FXStream& store) const {
  FXFrame::save(store);
  store << arrowColor;
  store << arrowSize;
  }


// Load object from stream
void FXArrowButton::load(FXStream& store){
  FXFrame::load(store);
  store >> arrowColor;
  store >> arrowSize;
  }


// Kill the timer
FXArrowButton::~FXArrowButton(){
  getApp()->removeTimeout(this,ID_AUTO);
  getApp()->removeTimeout(this,ID_REPEAT);
  }

}
