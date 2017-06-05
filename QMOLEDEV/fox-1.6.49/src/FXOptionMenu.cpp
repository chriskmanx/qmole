/********************************************************************************
*                                                                               *
*                             O p t i o n   M e n u                             *
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
* $Id: FXOptionMenu.cpp,v 1.68 2006/02/06 02:04:28 fox Exp $                    *
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
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXLabel.h"
#include "FXPopup.h"
#include "FXButton.h"
#include "FXMenuButton.h"
#include "FXToolTip.h"
#include "FXOptionMenu.h"

/*
  Notes:
  - Need API to inquire whether an FXOption is selected or not.
  - Should it grab first before POST?
  - FXOptionMenu should send the message, SEL_CHANGED when changing,
    SEL_COMMAND at the end, and SEL_UPDATE during gui update.
  - FXOptionMenu should understand ID_SETINTVALUE and ID_GETINTVALUE
    so you may substitute FXOptionMenu for a slider or dial.
  - Perhaps support arrow keys to select by means of keyboard.
  - Right-click to cycle through options (as suggested by
    Stephane Ancelot <sancelot@crosswinds.net>) would be nice.
  - Hotkey's don't work in FXOption.
  - Zecchini Mauro <mauro.zecchini@realtimesrl.com> donated code
    to add FXDataTarget get/set connectivity here also.
  - Do we need API's other than those based on current item
    as an index?
*/


#define MENUGLYPH_WIDTH  10
#define MENUGLYPH_HEIGHT 5

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXOption) FXOptionMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXOption::onPaint),
  FXMAPFUNC(SEL_ENTER,0,FXOption::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXOption::onLeave),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXOption::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXOption::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXOption::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXOption::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXOption::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXOption::onHotKeyRelease),
  };


// Object implementation
FXIMPLEMENT(FXOption,FXLabel,FXOptionMap,ARRAYNUMBER(FXOptionMap))


// For serialization
FXOption::FXOption(){
  seltextColor=0;
  selbackColor=0;
  }

// Make option menu entry
FXOption::FXOption(FXComposite* p,const FXString& text,FXIcon* ic,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,text,ic,opts,x,y,w,h,pl,pr,pt,pb){
  target=tgt;
  message=sel;
  seltextColor=getApp()->getSelMenuTextColor();
  selbackColor=getApp()->getSelMenuBackColor();
  defaultCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  }


// If window can have focus
bool FXOption::canFocus() const { return true; }


// Get default width
FXint FXOption::getDefaultWidth(){
  FXint tw=0,iw=MENUGLYPH_WIDTH,s=0,w;
  if(!label.empty()){
    tw=labelWidth(label);
    }
  if(icon){
    iw=icon->getWidth();
    }
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w=FXMAX(tw,iw); else w=tw+iw+s;
  return padleft+padright+(border<<1)+w;
  }


// Get default height
FXint FXOption::getDefaultHeight(){
  FXint th=0,ih=MENUGLYPH_HEIGHT,h;
  if(!label.empty()){
    th=labelHeight(label);
    }
  if(icon){
    ih=icon->getHeight();
    }
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h=FXMAX(th,ih); else h=th+ih;
  return padtop+padbottom+(border<<1)+h;
  }


// Handle repaint
long FXOption::onPaint(FXObject*,FXSelector,void* ptr){
  FXint tw=0,th=0,iw=MENUGLYPH_WIDTH,ih=MENUGLYPH_HEIGHT,tx,ty,ix,iy;
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
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
  dc.setForeground(isActive() ? selbackColor : backColor);
  dc.fillRectangle(border,border,width-border*2,height-border*2);
  if(icon){
    dc.drawIcon(icon,ix,iy);
    }
  else if(isActive()){
    drawDoubleRaisedRectangle(dc,ix,iy,MENUGLYPH_WIDTH,MENUGLYPH_HEIGHT);
    }
  if(!label.empty()){
    dc.setFont(font);
    if(isEnabled()){
      dc.setForeground(isActive() ? seltextColor : textColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
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


// Enter
long FXOption::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled() && canFocus()) setFocus();
  return 1;
  }


// Leave
long FXOption::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled() && canFocus()) killFocus();
  return 1;
  }


// Pressed left button; always unposts menu
long FXOption::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),this);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
    return 1;
    }
  return 0;
  }


// Released left button; unpost menu if cursor has moved
long FXOption::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(((FXEvent*)ptr)->moved){
      getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),this);
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
      }
    return 1;
    }
  return 0;
  }


// Keyboard press; forward to menu pane
long FXOption::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      return 1;
      }
    }
  return 0;
  }


// Keyboard release; forward to menu pane
long FXOption::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),this);
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXOption::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  FXTRACE((100,"FXOption::onHotKeyPress\n"));
  return 1;
  }


// Hot key combination released
long FXOption::onHotKeyRelease(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  FXTRACE((100,"FXOption::onHotKeyRelease\n"));
  if(isEnabled()){
    getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),this);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
    }
  return 1;
  }


// Into focus chain
void FXOption::setFocus(){
  FXLabel::setFocus();
  flags|=FLAG_ACTIVE;
  flags&=~FLAG_UPDATE;
  update();
  }


// Out of focus chain
void FXOption::killFocus(){
  FXLabel::killFocus();
  flags&=~FLAG_ACTIVE;
  flags|=FLAG_UPDATE;
  update();
  }


// Set select background color
void FXOption::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXOption::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    update();
    }
  }


// Delete
FXOption::~FXOption(){
  }


/*******************************************************************************/


// Map
FXDEFMAP(FXOptionMenu) FXOptionMenuMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXOptionMenu::onPaint),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXOptionMenu::onMouseWheel),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXOptionMenu::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXOptionMenu::onLeftBtnRelease),
  FXMAPFUNC(SEL_FOCUSIN,0,FXOptionMenu::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXOptionMenu::onFocusOut),
  FXMAPFUNC(SEL_MOTION,0,FXOptionMenu::onMotion),
  FXMAPFUNC(SEL_KEYPRESS,0,FXOptionMenu::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXOptionMenu::onKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_POST,FXOptionMenu::onCmdPost),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNPOST,FXOptionMenu::onCmdUnpost),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXOptionMenu::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXOptionMenu::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXOptionMenu::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXOptionMenu::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXOptionMenu::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXOptionMenu,FXLabel,FXOptionMenuMap,ARRAYNUMBER(FXOptionMenuMap))


// Make a option menu button
FXOptionMenu::FXOptionMenu(FXComposite* p,FXPopup* pup,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,FXString::null,NULL,opts,x,y,w,h,pl,pr,pt,pb){
  dragCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  pane=pup;
  current=NULL;
  if(pane){
    current=dynamic_cast<FXOption*>(pane->getFirst());
    if(current){
      label=current->getText();
      icon=current->getIcon();
      }
    }
  }


// Create window
void FXOptionMenu::create(){
  FXLabel::create();
  if(pane) pane->create();
  }


// Detach window
void FXOptionMenu::detach(){
  FXLabel::detach();
  if(pane) pane->detach();
  }


// Destroy window
void FXOptionMenu::destroy(){
  FXLabel::destroy();
  }


// Get default width
FXint FXOptionMenu::getDefaultWidth(){
  FXint w=0;
  if(pane){ w=pane->getDefaultWidth(); }
  return (border<<1)+w;
  }


// Get default height
FXint FXOptionMenu::getDefaultHeight(){
  FXint h=0;
  if(pane && pane->getFirst()){
    h=pane->getFirst()->getDefaultHeight();
    }
  return (border<<1)+h;
  }


// Gained focus
long FXOptionMenu::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusIn(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Lost focus
long FXOptionMenu::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusOut(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Handle repaint
long FXOptionMenu::onPaint(FXObject*,FXSelector,void* ptr){
  FXint tw=0,th=0,iw=MENUGLYPH_WIDTH,ih=MENUGLYPH_HEIGHT,tx,ty,ix,iy;
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);

  drawFrame(dc,0,0,width,height);

  // Draw background
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,width-border*2,height-border*2);

  // Position text & icon
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

  // Draw enabled state
  if(isEnabled()){
    if(icon){
      dc.drawIcon(icon,ix,iy);
      }
    else{
      drawDoubleRaisedRectangle(dc,ix,iy,MENUGLYPH_WIDTH,MENUGLYPH_HEIGHT);
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
    else{
      drawDoubleRaisedRectangle(dc,ix,iy,MENUGLYPH_WIDTH,MENUGLYPH_HEIGHT);
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


// Keyboard press; forward to menu pane
long FXOptionMenu::onKeyPress(FXObject*,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(pane && pane->shown() && pane->handle(pane,sel,ptr)) return 1;
    switch(event->code){
      case KEY_space:
      case KEY_KP_Space:
        return 1;
      }
    }
  return 0;
  }


// Keyboard release; forward to menu pane
long FXOptionMenu::onKeyRelease(FXObject*,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(pane && pane->shown() && pane->handle(pane,sel,ptr)) return 1;
    switch(event->code){
      case KEY_space:
      case KEY_KP_Space:
        if(pane){
          if(pane->shown()){
            handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
            }
          else{
            handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
            }
          }
        return 1;
      }
    }
  return 0;
  }


// Pressed left button
long FXOptionMenu::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(pane){
      if(pane->shown()){
        handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
        }
      }
    return 1;
    }
  return 0;
  }


// Released left button
long FXOptionMenu::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(ev->moved && pane){ handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL); }
    return 1;
    }
  return 0;
  }


// If we moved over the pane, we'll ungrab again, or re-grab
// when outside of the plane
long FXOptionMenu::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(pane && pane->shown()){
    if(pane->contains(ev->root_x,ev->root_y)){
      if(grabbed()) ungrab();
      }
    else{
      if(!grabbed()) grab();
      }
    return 1;
    }
  return 0;
  }


// Use wheel to change option; suggested by Jon Sargeant
long FXOptionMenu::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(isEnabled()){
    if(ev->code>0)
      setCurrentNo((getCurrentNo()-1+getNumOptions())%getNumOptions(),TRUE);
    else
      setCurrentNo((getCurrentNo()+1)%getNumOptions(),TRUE);
    return 1;
    }
  return 0;
  }


// Post the menu
long FXOptionMenu::onCmdPost(FXObject*,FXSelector,void*){
  if(pane && !pane->shown()){
    FXint x,y;
    if(!current) current=dynamic_cast<FXOption*>(pane->getFirst());
    if(!current) return 1;
    translateCoordinatesTo(x,y,getRoot(),0,0);
    pane->position(x,y,width,pane->getDefaultHeight());
    y+=2-current->getY();
    pane->popup(this,x,y,width,pane->getDefaultHeight());
    current->setFocus();
    if(!grabbed()) grab();
    flags&=~FLAG_UPDATE;
    }
  return 1;
  }


// Unpost the menu
// Sender was the original option that sent the message
long FXOptionMenu::onCmdUnpost(FXObject*,FXSelector,void* ptr){
  if(pane && pane->shown()){
    pane->popdown();
    if(grabbed()) ungrab();
    flags|=FLAG_UPDATE;
    if(ptr){
      setCurrent((FXOption*)ptr,TRUE);
      }
    }
  return 1;
  }


// Update value from a message
long FXOptionMenu::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrentNo((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXOptionMenu::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCurrentNo(*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXOptionMenu::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCurrentNo();
  return 1;
  }


// Layout
void FXOptionMenu::layout(){
  FXLabel::layout();
  if(!current && pane && pane->getFirst()){
    setCurrent(dynamic_cast<FXOption*>(pane->getFirst()));
    }
  flags&=~FLAG_DIRTY;
  }


// Logically inside pane
bool FXOptionMenu::contains(FXint parentx,FXint parenty) const {
  if(pane && pane->shown() && pane->contains(parentx,parenty)) return true;
  return false;
  }


// Out of focus chain
void FXOptionMenu::killFocus(){
  FXLabel::killFocus();
  handle(current,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  }


// If window can have focus
bool FXOptionMenu::canFocus() const { return true; }


// Set current selection
void FXOptionMenu::setCurrent(FXOption *win,FXbool notify){
  if(current!=win){
    current=win;
    if(win){
      setText(current->getText());
      setIcon(current->getIcon());
      }
    else{
      setText(FXString::null);
      setIcon(NULL);
      }
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)getCurrentNo());}
    }
  }


// Set current option
void FXOptionMenu::setCurrentNo(FXint no,FXbool notify){
  register FXOption *win=NULL;
  if(pane) win=dynamic_cast<FXOption*>(pane->childAtIndex(no));
  setCurrent(win,notify);
  }


// Get current option
FXint FXOptionMenu::getCurrentNo() const {
  return pane ? pane->indexOfChild(current) : -1;
  }


// Get the number of options in this menu
FXint FXOptionMenu::getNumOptions() const {
  return pane ? pane->numChildren() : 0;
  }


// Change popup
void FXOptionMenu::setMenu(FXPopup *pup){
  register FXOption *win;
  if(pup!=pane){
    pane=pup;
    if(pane){
      win=dynamic_cast<FXOption*>(pane->getFirst());
      if(win){
        setText(win->getText());
        setIcon(win->getIcon());
        }
      current=win;
      }
    recalc();
    }
  }


// The current option's tip is returned, unless there is no tip,
// in which case the option menu's tip is returned
long FXOptionMenu::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if(flags&FLAG_TIP){
    if(current){
      FXString optiontip=current->getTipText();
      if(!optiontip.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&optiontip);
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


// The current option's help is returned, unless there is no help,
// in which case the option menu's help is returned
long FXOptionMenu::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if(flags&FLAG_HELP){
    if(current){
      FXString optionhelp=current->getHelpText();
      if(!optionhelp.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&optionhelp);
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


// True if popped up
FXbool FXOptionMenu::isPopped() const {
  return pane && pane->shown();
  }


// Save object to stream
void FXOptionMenu::save(FXStream& store) const {
  FXLabel::save(store);
  store << pane;
  store << current;
  }


// Load object from stream
void FXOptionMenu::load(FXStream& store){
  FXLabel::load(store);
  store >> pane;
  store >> current;
  }


// Delete it
FXOptionMenu::~FXOptionMenu(){
  pane=(FXPopup*)-1L;
  current=(FXOption*)-1L;
  }

}
