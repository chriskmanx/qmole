/********************************************************************************
*                                                                               *
*                       M e n u    B u t t o n    O b j e c t                   *
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
* $Id: FXMenuButton.cpp,v 1.50 2006/01/22 17:58:35 fox Exp $                    *
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
#include "FXMenuButton.h"
#include "FXMenuPane.h"

#include "FXMenuBar.h"

/*
  Notes:
  - You can turn off arrows.
  - You can pop the pane right,left,up,or down.
  - For each of the above, you can align the pane four ways; for
    the case of popping it down:

    MENUBUTTON_ATTACH_LEFT:     MENUBUTTON_ATTACH_RIGHT:

     +------+                        +------+
     |Button|                        |Button|
     +------+---+                +---+------+
     |   Pane   |                |   Pane   |
     +----------+                +----------+


     MENUBUTTON_ATTACH_CENTER:  MENUBUTTON_ATTACH_BOTH:

       +------+                  +----------+
       |Button|                  |  Button  |
     +-+------+-+                +----------+
     |   Pane   |                |   Pane   |
     +----------+                +----------+

  - The width (height) of the pane is taken into account by getDefaultWidth()
    (getDefaultHeight()), so that the button will stretch to fit the pane.
  - You can specify horizontal or vertical offset to position the pane away
    from the button a bit.
  - Should it grab first before doing the POST?
  - GUI update disabled while menu is popped up.
*/

#define MENUBUTTONARROW_WIDTH   11
#define MENUBUTTONARROW_HEIGHT  5

#define MENUBUTTON_MASK         (MENUBUTTON_AUTOGRAY|MENUBUTTON_AUTOHIDE|MENUBUTTON_TOOLBAR|MENUBUTTON_NOARROWS)
#define POPUP_MASK              (MENUBUTTON_UP|MENUBUTTON_LEFT)
#define ATTACH_MASK             (MENUBUTTON_ATTACH_RIGHT|MENUBUTTON_ATTACH_CENTER)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuButton) FXMenuButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXMenuButton::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXMenuButton::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXMenuButton::onLeave),
  FXMAPFUNC(SEL_MOTION,0,FXMenuButton::onMotion),
  FXMAPFUNC(SEL_FOCUSIN,0,FXMenuButton::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXMenuButton::onFocusOut),
  FXMAPFUNC(SEL_UNGRABBED,0,FXMenuButton::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXMenuButton::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXMenuButton::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXMenuButton::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXMenuButton::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXMenuButton::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXMenuButton::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_POST,FXMenuButton::onCmdPost),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNPOST,FXMenuButton::onCmdUnpost),
  };


// Object implementation
FXIMPLEMENT(FXMenuButton,FXLabel,FXMenuButtonMap,ARRAYNUMBER(FXMenuButtonMap))


// Deserialization
FXMenuButton::FXMenuButton(){
  pane=(FXPopup*)-1L;
  offsetx=0;
  offsety=0;
  state=FALSE;
  }


// Make a check button
FXMenuButton::FXMenuButton(FXComposite* p,const FXString& text,FXIcon* ic,FXPopup* pup,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXLabel(p,text,ic,opts,x,y,w,h,pl,pr,pt,pb){
  pane=pup;
  offsetx=0;
  offsety=0;
  state=FALSE;
  }


// Create window
void FXMenuButton::create(){
  FXLabel::create();
  if(pane) pane->create();
  }


// Detach window
void FXMenuButton::detach(){
  FXLabel::detach();
  if(pane) pane->detach();
  }


// If window can have focus
bool FXMenuButton::canFocus() const { return true; }


// Get default width
FXint FXMenuButton::getDefaultWidth(){
  FXint tw=0,iw=0,s=0,w,pw;
  if(!label.empty()){ tw=labelWidth(label); s=4; }
  if(!(options&MENUBUTTON_NOARROWS)){
    if(options&MENUBUTTON_LEFT) iw=MENUBUTTONARROW_HEIGHT; else iw=MENUBUTTONARROW_WIDTH;
    }
  if(icon) iw=icon->getWidth();
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w=FXMAX(tw,iw); else w=tw+iw+s;
  w=padleft+padright+(border<<1)+w;
  if(!(options&MENUBUTTON_LEFT) && (options&MENUBUTTON_ATTACH_RIGHT) && (options&MENUBUTTON_ATTACH_CENTER)){
    if(pane){ pw=pane->getDefaultWidth(); if(pw>w) w=pw; }
    }
  return w;
  }


// Get default height
FXint FXMenuButton::getDefaultHeight(){
  FXint th=0,ih=0,h,ph;
  if(!label.empty()){ th=labelHeight(label); }
  if(!(options&MENUBUTTON_NOARROWS)){
    if(options&MENUBUTTON_LEFT) ih=MENUBUTTONARROW_WIDTH; else ih=MENUBUTTONARROW_HEIGHT;
    }
  if(icon) ih=icon->getHeight();
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h=FXMAX(th,ih); else h=th+ih;
  h=padtop+padbottom+(border<<1)+h;
  if((options&MENUBUTTON_LEFT) && (options&MENUBUTTON_ATTACH_BOTTOM)&&(options&MENUBUTTON_ATTACH_CENTER)){
    if(pane){ ph=pane->getDefaultHeight(); if(ph>h) h=ph; }
    }
  return h;
  }


// Implement auto-hide or auto-gray modes
long FXMenuButton::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXLabel::onUpdate(sender,sel,ptr)){
    if(options&MENUBUTTON_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&MENUBUTTON_AUTOGRAY){disable();}
    }
  return 1;
  }


// Gained focus
long FXMenuButton::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusIn(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Lost focus
long FXMenuButton::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onFocusOut(sender,sel,ptr);
  update(border,border,width-(border<<1),height-(border<<1));
  return 1;
  }


// Inside the button
long FXMenuButton::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onEnter(sender,sel,ptr);
  if(isEnabled()){
    if(options&MENUBUTTON_TOOLBAR) update();
    }
  return 1;
  }


// Outside the button
long FXMenuButton::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onLeave(sender,sel,ptr);
  if(isEnabled()){
    if(options&MENUBUTTON_TOOLBAR) update();
    }
  return 1;
  }


// Pressed left button
long FXMenuButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(state)
      handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
    else
      handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
    return 1;
    }
  return 0;
  }


// Released left button
long FXMenuButton::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(ev->moved){ handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL); }
    return 1;
    }
  return 0;
  }


// If we moved over the pane, we'll ungrab again, or re-grab
// when outside of the plane
long FXMenuButton::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* ev=(FXEvent*)ptr;
  if(state){
    if(pane){
      if(pane->contains(ev->root_x,ev->root_y)){
        if(grabbed()) ungrab();
        }
      else{
        if(!grabbed()) grab();
        }
      return 1;
      }
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXMenuButton::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXLabel::onUngrabbed(sender,sel,ptr);
  handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  return 1;
  }


// Keyboard press; forward to menu pane, or handle it here
long FXMenuButton::onKeyPress(FXObject*,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(pane && pane->shown() && pane->handle(pane,sel,ptr)) return 1;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      if(state)
        handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
      else
        handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
      return 1;
      }
    }
  return 0;
  }


// Keyboard release; forward to menu pane, or handle here
long FXMenuButton::onKeyRelease(FXObject*,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(pane && pane->shown() && pane->handle(pane,sel,ptr)) return 1;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXMenuButton::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  FXTRACE((200,"%s::onHotKeyPress %p\n",getClassName(),this));
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    if(state)
      handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
    else
      handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);
    }
  return 1;
  }


// Hot key combination released
long FXMenuButton::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onHotKeyRelease %p\n",getClassName(),this));
  return 1;
  }


// Post the menu
long FXMenuButton::onCmdPost(FXObject*,FXSelector,void*){
  if(!state){
    if(pane){
      FXint x,y,w,h;
      translateCoordinatesTo(x,y,getRoot(),0,0);
      w=pane->getShrinkWrap() ? pane->getDefaultWidth() : pane->getWidth();
      h=pane->getShrinkWrap() ? pane->getDefaultHeight() : pane->getHeight();
      if((options&MENUBUTTON_LEFT)&&(options&MENUBUTTON_UP)){   // Right
        if((options&MENUBUTTON_ATTACH_BOTTOM)&&(options&MENUBUTTON_ATTACH_CENTER)){
          h=height;
          }
        else if(options&MENUBUTTON_ATTACH_CENTER){
          y=y+(height-h)/2;
          }
        else if(options&MENUBUTTON_ATTACH_BOTTOM){
          y=y+height-h;
          }
        x=x+offsetx+width;
        y=y+offsety;
        }
      else if(options&MENUBUTTON_LEFT){                         // Left
        if((options&MENUBUTTON_ATTACH_BOTTOM)&&(options&MENUBUTTON_ATTACH_CENTER)){
          h=height;
          }
        else if(options&MENUBUTTON_ATTACH_CENTER){
          y=y+(height-h)/2;
          }
        else if(options&MENUBUTTON_ATTACH_BOTTOM){
          y=y+height-h;
          }
        x=x-offsetx-w;
        y=y+offsety;
        }
      else if(options&MENUBUTTON_UP){                           // Up
        if((options&MENUBUTTON_ATTACH_RIGHT)&&(options&MENUBUTTON_ATTACH_CENTER)){
          w=width;
          }
        else if(options&MENUBUTTON_ATTACH_CENTER){
          x=x+(width-w)/2;
          }
        else if(options&MENUBUTTON_ATTACH_RIGHT){
          x=x+width-w;
          }
        x=x+offsetx;
        y=y-offsety-h;
        }
      else{                                                     // Down
        if((options&MENUBUTTON_ATTACH_RIGHT)&&(options&MENUBUTTON_ATTACH_CENTER)){
          w=width;
          }
        else if(options&MENUBUTTON_ATTACH_CENTER){
          x=x+(width-w)/2;
          }
        else if(options&MENUBUTTON_ATTACH_RIGHT){
          x=x+width-w;
          }
        x=x+offsetx;
        y=y+offsety+height;
        }
      pane->popup(this,x,y,w,h);
      if(!grabbed()) grab();
      }
    flags&=~FLAG_UPDATE;
    state=TRUE;
    update();
    }
  return 1;
  }


// Unpost the menu
long FXMenuButton::onCmdUnpost(FXObject*,FXSelector,void*){
  if(state){
    if(pane){
      pane->popdown();
      if(grabbed()) ungrab();
      }
    flags|=FLAG_UPDATE;
    state=FALSE;
    update();
    }
  return 1;
  }


// Handle repaint
long FXMenuButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXint tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
  FXEvent *ev=(FXEvent*)ptr;
  FXPoint points[3];
  FXDCWindow dc(this,ev);

  // Got a border at all?
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){

    // Toolbar style
    if(options&MENUBUTTON_TOOLBAR){

      // Enabled and cursor inside, and not popped up
      if(isEnabled() && underCursor() && !state){
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
        else drawRaisedRectangle(dc,0,0,width,height);
        }

      // Enabled and popped up
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

      // Draw in up state if disabled or up
      if(!isEnabled() || !state){
        dc.setForeground(backColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
        else drawRaisedRectangle(dc,0,0,width,height);
        }

      // Draw sunken if enabled and either checked or pressed
      else{
        dc.setForeground(hiliteColor);
        dc.fillRectangle(border,border,width-border*2,height-border*2);
        if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
        else drawSunkenRectangle(dc,0,0,width,height);
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

  // Position text & icon
  if(!label.empty()){
    tw=labelWidth(label);
    th=labelHeight(label);
    }

  // Icon?
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }

  // Arrows?
  else if(!(options&MENUBUTTON_NOARROWS)){
    if(options&MENUBUTTON_LEFT){
      ih=MENUBUTTONARROW_WIDTH;
      iw=MENUBUTTONARROW_HEIGHT;
      }
    else{
      iw=MENUBUTTONARROW_WIDTH;
      ih=MENUBUTTONARROW_HEIGHT;
      }
    }

  // Keep some room for the arrow!
  just_x(tx,ix,tw,iw);
  just_y(ty,iy,th,ih);

  // Move a bit when pressed
  if(state){ ++tx; ++ty; ++ix; ++iy; }

  // Draw icon
  if(icon){
    if(isEnabled())
      dc.drawIcon(icon,ix,iy);
    else
      dc.drawIconSunken(icon,ix,iy);
    }

  // Draw arrows
  else if(!(options&MENUBUTTON_NOARROWS)){

    // Right arrow
    if((options&MENUBUTTON_RIGHT)==MENUBUTTON_RIGHT){
      if(isEnabled())
        dc.setForeground(textColor);
      else
        dc.setForeground(shadowColor);
      points[0].x=ix;
      points[0].y=iy;
      points[1].x=ix;
      points[1].y=iy+MENUBUTTONARROW_WIDTH-1;
      points[2].x=ix+MENUBUTTONARROW_HEIGHT;
      points[2].y=(FXshort)(iy+(MENUBUTTONARROW_WIDTH>>1));
      dc.fillPolygon(points,3);
      }

    // Left arrow
    else if(options&MENUBUTTON_LEFT){
      if(isEnabled())
        dc.setForeground(textColor);
      else
        dc.setForeground(shadowColor);
      points[0].x=ix+MENUBUTTONARROW_HEIGHT;
      points[0].y=iy;
      points[1].x=ix+MENUBUTTONARROW_HEIGHT;
      points[1].y=iy+MENUBUTTONARROW_WIDTH-1;
      points[2].x=ix;
      points[2].y=(FXshort)(iy+(MENUBUTTONARROW_WIDTH>>1));
      dc.fillPolygon(points,3);
      }

    // Up arrow
    else if(options&MENUBUTTON_UP){
      if(isEnabled())
        dc.setForeground(textColor);
      else
        dc.setForeground(shadowColor);
      points[0].x=(FXshort)(ix+(MENUBUTTONARROW_WIDTH>>1));
      points[0].y=iy-1;
      points[1].x=ix;
      points[1].y=iy+MENUBUTTONARROW_HEIGHT;
      points[2].x=ix+MENUBUTTONARROW_WIDTH;
      points[2].y=iy+MENUBUTTONARROW_HEIGHT;
      dc.fillPolygon(points,3);
      }

    // Down arrow
    else{
      if(isEnabled())
        dc.setForeground(textColor);
      else
        dc.setForeground(shadowColor);
      points[0].x=ix+1;
      points[0].y=iy;
      points[2].x=ix+MENUBUTTONARROW_WIDTH-1;
      points[2].y=iy;
      points[1].x=(FXshort)(ix+(MENUBUTTONARROW_WIDTH>>1));
      points[1].y=iy+MENUBUTTONARROW_HEIGHT;
      dc.fillPolygon(points,3);
      }
    }

  // Draw text
  if(!label.empty()){
    dc.setFont(font);
    if(isEnabled()){
      dc.setForeground(textColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      }
    else{
      dc.setForeground(hiliteColor);
      drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,label,hotoff,tx,ty,tw,th);
      }
    }

  // Draw focus
  if(hasFocus()){
    if(isEnabled()){
      dc.drawFocusRectangle(border+1,border+1,width-2*border-2,height-2*border-2);
      }
    }
  return 1;
  }


// Out of focus chain
void FXMenuButton::killFocus(){
  FXLabel::killFocus();
  handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  }


// Logically inside pane
bool FXMenuButton::contains(FXint parentx,FXint parenty) const {
  if(pane && pane->shown() && pane->contains(parentx,parenty)) return true;
  return false;
  }



// Change the popup menu
void FXMenuButton::setMenu(FXPopup *pup){
  if(pup!=pane){
    pane=pup;
    recalc();
    }
  }


// Set icon positioning
void FXMenuButton::setButtonStyle(FXuint style){
  FXuint opts=(options&~MENUBUTTON_MASK) | (style&MENUBUTTON_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get icon positioning
FXuint FXMenuButton::getButtonStyle() const {
  return (options&MENUBUTTON_MASK);
  }


// Set menu button popup style
void FXMenuButton::setPopupStyle(FXuint style){
  FXuint opts=(options&~POPUP_MASK) | (style&POPUP_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get menu button popup style
FXuint FXMenuButton::getPopupStyle() const {
  return (options&POPUP_MASK);
  }


// Change pane attachment
void FXMenuButton::setAttachment(FXuint att){
  FXuint opts=(options&~ATTACH_MASK) | (att&ATTACH_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get pane attachment
FXuint FXMenuButton::getAttachment() const {
  return (options&ATTACH_MASK);
  }


// Save object to stream
void FXMenuButton::save(FXStream& store) const {
  FXLabel::save(store);
  store << pane;
  store << offsetx;
  store << offsety;
  }


// Load object from stream
void FXMenuButton::load(FXStream& store){
  FXLabel::load(store);
  store >> pane;
  store >> offsetx;
  store >> offsety;
  }


// Delete it
FXMenuButton::~FXMenuButton(){
  pane=(FXPopup*)-1L;
  }

}
