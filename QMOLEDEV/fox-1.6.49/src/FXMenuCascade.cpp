/********************************************************************************
*                                                                               *
*                       M e n u   C a s c a d e   W i d g e t                   *
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
* $Id: FXMenuCascade.cpp,v 1.55 2006/01/22 17:58:35 fox Exp $                   *
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
#include "FXFont.h"
#include "FXIcon.h"
#include "FXMenuPane.h"
#include "FXMenuCascade.h"

/*
  Notes:
  - Accelerators.
  - Help text from constructor is third part; second part should be
    accelerator key combination.
  - When menu label changes, hotkey might have to be adjusted.
  - Fix it so menu stays up when after Alt-F, you press Alt-E.
  - MenuItems should be derived from FXLabel.
  - FXMenuCascade should send ID_POST/IDUNPOST to self.
  - Look into SEL_FOCUS_SELF some more...
*/


#define LEADSPACE   22
#define TRAILSPACE  16

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuCascade) FXMenuCascadeMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuCascade::onPaint),
  FXMAPFUNC(SEL_ENTER,0,FXMenuCascade::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXMenuCascade::onLeave),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXMenuCascade::onButtonPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXMenuCascade::onButtonRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXMenuCascade::onButtonPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXMenuCascade::onButtonRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXMenuCascade::onButtonPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXMenuCascade::onButtonRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXMenuCascade::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXMenuCascade::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXMenuCascade::ID_HOTKEY,FXMenuCascade::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXMenuCascade::ID_HOTKEY,FXMenuCascade::onHotKeyRelease),
  FXMAPFUNC(SEL_TIMEOUT,FXMenuCascade::ID_MENUTIMER,FXMenuCascade::onCmdPost),
  FXMAPFUNC(SEL_COMMAND,FXMenuCascade::ID_POST,FXMenuCascade::onCmdPost),
  FXMAPFUNC(SEL_COMMAND,FXMenuCascade::ID_UNPOST,FXMenuCascade::onCmdUnpost),
  };


// Object implementation
FXIMPLEMENT(FXMenuCascade,FXMenuCaption,FXMenuCascadeMap,ARRAYNUMBER(FXMenuCascadeMap))


// Make cascade menu button
FXMenuCascade::FXMenuCascade(){
  flags|=FLAG_ENABLED;
  pane=NULL;
  }


// Make cascade menu button
FXMenuCascade::FXMenuCascade(FXComposite* p,const FXString& text,FXIcon* ic,FXPopup* pup,FXuint opts):
  FXMenuCaption(p,text,ic,opts){
  defaultCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  flags|=FLAG_ENABLED;
  pane=pup;
  }


// Create window
void FXMenuCascade::create(){
  FXMenuCaption::create();
  if(pane) pane->create();
  }


// Detach X window
void FXMenuCascade::detach(){
  getApp()->removeTimeout(this,ID_MENUTIMER);
  FXMenuCaption::detach();
  if(pane) pane->detach();
  }


// Destroy window
void FXMenuCascade::destroy(){
  getApp()->removeTimeout(this,ID_MENUTIMER);
  FXMenuCaption::destroy();
  }


// If window can have focus
bool FXMenuCascade::canFocus() const { return true; }


// Pressed button
long FXMenuCascade::onButtonPress(FXObject*,FXSelector,void*){
  if(!isEnabled()) return 0;
  handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
  return 1;
  }


// Released button
long FXMenuCascade::onButtonRelease(FXObject*,FXSelector,void* ptr){
  if(!isEnabled()) return 0;
  if(((FXEvent*)ptr)->moved){
    getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
    }
  return 1;
  }


// Keyboard press; forward to submenu pane
long FXMenuCascade::onKeyPress(FXObject*,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(!isEnabled()) return 0;
  FXTRACE((200,"%s::onKeyPress %p keysym=0x%04x state=%04x\n",getClassName(),this,event->code,event->state));
  if(pane && pane->shown() && pane->handle(pane,sel,ptr)) return 1;
  switch(event->code){
    case KEY_Right:
      if(pane && !pane->shown()){
        FXint x,y;
        getApp()->removeTimeout(this,ID_MENUTIMER);
        translateCoordinatesTo(x,y,getRoot(),width,0);
        pane->popup(((FXMenuPane*)getParent())->getGrabOwner(),x,y);
        return 1;
        }
      break;
    case KEY_Left:
      if(pane && pane->shown()){
        getApp()->removeTimeout(this,ID_MENUTIMER);
        pane->popdown();
        return 1;
        }
      break;
    case KEY_KP_Enter:
    case KEY_Return:
    case KEY_space:
    case KEY_KP_Space:
      handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
      return 1;
    }
  return 0;
  }


// Keyboard release; forward to submenu pane
long FXMenuCascade::onKeyRelease(FXObject*,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(!isEnabled()) return 0;
  FXTRACE((200,"%s::onKeyRelease %p keysym=0x%04x state=%04x\n",getClassName(),this,event->code,event->state));
  if(pane && pane->shown() && pane->handle(pane,sel,ptr)) return 1;
  switch(event->code){
    case KEY_Right:
    case KEY_Left:
      return 1;
    case KEY_KP_Enter:
    case KEY_Return:
    case KEY_space:
    case KEY_KP_Space:
      return 1;
    }
  return 0;
  }


// Hot key combination pressed
long FXMenuCascade::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  FXTRACE((200,"%s::onHotKeyPress %p\n",getClassName(),this));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
    }
  return 1;
  }


// Hot key combination released
long FXMenuCascade::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onHotKeyRelease %p\n",getClassName(),this));
  return 1;
  }


// Post the menu
long FXMenuCascade::onCmdPost(FXObject*,FXSelector,void*){
  FXint x,y;
  getApp()->removeTimeout(this,ID_MENUTIMER);
  if(pane && !pane->shown()){
    translateCoordinatesTo(x,y,getRoot(),width,0);
    pane->popup(((FXMenuPane*)getParent())->getGrabOwner(),x,y);
    }
  return 1;
  }


// Unpost the menu
long FXMenuCascade::onCmdUnpost(FXObject*,FXSelector,void*){
  getApp()->removeTimeout(this,ID_MENUTIMER);
  if(pane && pane->shown()){
    pane->popdown();
    }
  return 1;
  }


// Into focus chain
void FXMenuCascade::setFocus(){
  FXMenuCaption::setFocus();
  flags|=FLAG_ACTIVE;
  flags&=~FLAG_UPDATE;
  update();
  }


// Out of focus chain; hide submenu if it was up
void FXMenuCascade::killFocus(){
  FXMenuCaption::killFocus();
  handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  flags&=~FLAG_ACTIVE;
  flags|=FLAG_UPDATE;
  update();
  }


// Enter; set timer for delayed popup
long FXMenuCascade::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXMenuCaption::onEnter(sender,sel,ptr);
  if(isEnabled() && canFocus()){
    getApp()->addTimeout(this,ID_MENUTIMER,getApp()->getMenuPause());
    setFocus();
    }
  return 1;
  }


// Leave
long FXMenuCascade::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXMenuCaption::onLeave(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_MENUTIMER);
  return 1;
  }


// Handle repaint
long FXMenuCascade::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint xx,yy;

  xx=LEADSPACE;

  // Grayed out
  if(!isEnabled()){
    dc.setForeground(backColor);
    dc.fillRectangle(0,0,width,height);
    if(icon){
      dc.drawIconSunken(icon,3,(height-icon->getHeight())/2);
      if(icon->getWidth()+5>xx) xx=icon->getWidth()+5;
      }
    if(!label.empty()){
      yy=font->getFontAscent()+(height-font->getFontHeight())/2;
      dc.setFont(font);
      dc.setForeground(hiliteColor);
      dc.drawText(xx+1,yy+1,label);
      dc.setForeground(shadowColor);
      dc.drawText(xx,yy,label);
      if(0<=hotoff){
        dc.fillRectangle(xx+1+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
        }
      }
    yy=(height-8)/2;
    dc.setForeground(shadowColor);
    drawTriangle(dc,width-TRAILSPACE+4,yy,width-TRAILSPACE+4+6,yy+8);
    }

  // Active
  else if(isActive()){
    dc.setForeground(selbackColor);
    dc.fillRectangle(0,0,width,height);
    if(icon){
      dc.drawIcon(icon,3,(height-icon->getHeight())/2);
      if(icon->getWidth()+5>xx) xx=icon->getWidth()+5;
      }
    if(!label.empty()){
      yy=font->getFontAscent()+(height-font->getFontHeight())/2;
      dc.setFont(font);
      dc.setForeground(isEnabled() ? seltextColor : shadowColor);
      dc.drawText(xx,yy,label);
      if(0<=hotoff){
        dc.fillRectangle(xx+1+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
        }
      }
    yy=(height-8)/2;
    dc.setForeground(seltextColor);
    drawTriangle(dc,width-TRAILSPACE+4,yy,width-TRAILSPACE+4+6,yy+8);
    }

  // Normal
  else{
    dc.setForeground(backColor);
    dc.fillRectangle(0,0,width,height);
    if(icon){
      dc.drawIcon(icon,3,(height-icon->getHeight())/2);
      if(icon->getWidth()+5>xx) xx=icon->getWidth()+5;
      }
    if(!label.empty()){
      yy=font->getFontAscent()+(height-font->getFontHeight())/2;
      dc.setFont(font);
      dc.setForeground(textColor);
      dc.drawText(xx,yy,label);
      if(0<=hotoff){
        dc.fillRectangle(xx+1+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
        }
      }
    yy=(height-8)/2;
    dc.setForeground(textColor);
    drawTriangle(dc,width-TRAILSPACE+4,yy,width-TRAILSPACE+4+6,yy+8);
    }
  return 1;
  }


// Draw triangle
void FXMenuCascade::drawTriangle(FXDCWindow& dc,FXint l,FXint t,FXint,FXint b){
  FXPoint points[3];
  int m=(t+b)/2;
  points[0].x=l;
  points[0].y=t;
  points[1].x=l;
  points[1].y=b;
  points[2].x=l+(b-t)/2;
  points[2].y=m;
  dc.fillPolygon(points,3);
  }


// Test if logically inside
bool FXMenuCascade::contains(FXint parentx,FXint parenty) const {
  FXint x,y;
  if(FXMenuCaption::contains(parentx,parenty)) return true;
  if(getMenu() && getMenu()->shown()){
    getParent()->translateCoordinatesTo(x,y,getRoot(),parentx,parenty);
    if(getMenu()->contains(x,y)) return true;
    }
  return false;
  }


// Save object to stream
void FXMenuCascade::save(FXStream& store) const {
  FXMenuCaption::save(store);
  store << pane;
  }


// Load object from stream
void FXMenuCascade::load(FXStream& store){
  FXMenuCaption::load(store);
  store >> pane;
  }


// Delete it
FXMenuCascade::~FXMenuCascade(){
  getApp()->removeTimeout(this,ID_MENUTIMER);
  pane=(FXMenuPane*)-1L;
  }

}
