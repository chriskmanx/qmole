/********************************************************************************
*                                                                               *
*                         M e n u   C o m m a n d    W i d g e t                *
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
* $Id: FXMenuCommand.cpp,v 1.72 2006/01/22 17:58:36 fox Exp $                   *
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
#include "FXMenuCommand.h"

/*
  Notes:
  - Help text from constructor is third part; second part should be
    accelerator key combination.
  - When menu label changes, hotkey might have to be adjusted.
  - Fix it so menu stays up when after Alt-F, you press Alt-E.
  - MenuItems should be derived from FXLabel.
  - FXMenuCascade should send ID_POST/IDUNPOST to self.
  - Look into SEL_FOCUS_SELF some more...
  - We handle left, middle, right mouse buttons exactly the same;
    this permits popup menus posted by any mouse button.
  - MenuCommand should flip state when invoked, and send new state along
    in ptr in callback.
*/


#define LEADSPACE   22
#define TRAILSPACE  16

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuCommand) FXMenuCommandMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuCommand::onPaint),
  FXMAPFUNC(SEL_ENTER,0,FXMenuCommand::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXMenuCommand::onLeave),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXMenuCommand::onButtonPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXMenuCommand::onButtonRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXMenuCommand::onButtonPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXMenuCommand::onButtonRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXMenuCommand::onButtonPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXMenuCommand::onButtonRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXMenuCommand::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXMenuCommand::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXMenuCommand::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXMenuCommand::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_ACCEL,FXMenuCommand::onCmdAccel),
  };


// Object implementation
FXIMPLEMENT(FXMenuCommand,FXMenuCaption,FXMenuCommandMap,ARRAYNUMBER(FXMenuCommandMap))


// Command menu item
FXMenuCommand::FXMenuCommand(){
  flags|=FLAG_ENABLED;
  acckey=0;
  }


// Command menu item
FXMenuCommand::FXMenuCommand(FXComposite* p,const FXString& text,FXIcon* ic,FXObject* tgt,FXSelector sel,FXuint opts):
  FXMenuCaption(p,text,ic,opts){
  FXAccelTable *table;
  FXWindow *own;
  flags|=FLAG_ENABLED;
  defaultCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  target=tgt;
  message=sel;
  accel=text.section('\t',1);
  acckey=parseAccel(accel);
  if(acckey){
    own=getShell()->getOwner();
    if(own){
      table=own->getAccelTable();
      if(table){
        table->addAccel(acckey,this,FXSEL(SEL_COMMAND,ID_ACCEL));
        }
      }
    }
  }


// Get default width
FXint FXMenuCommand::getDefaultWidth(){
  FXint tw,aw,iw;
  tw=aw=iw=0;
  if(!label.empty()) tw=font->getTextWidth(label.text(),label.length());
  if(!accel.empty()) aw=font->getTextWidth(accel.text(),accel.length());
  if(aw && tw) aw+=5;
  if(icon) iw=icon->getWidth()+5;
  return FXMAX(iw,LEADSPACE)+tw+aw+TRAILSPACE;
  }


// Get default height
FXint FXMenuCommand::getDefaultHeight(){
  FXint th,ih;
  th=ih=0;
  if(!label.empty() || !accel.empty()) th=font->getFontHeight()+5;
  if(icon) ih=icon->getHeight()+5;
  return FXMAX(th,ih);
  }


// If window can have focus
bool FXMenuCommand::canFocus() const { return true; }


// Enter
long FXMenuCommand::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXMenuCaption::onEnter(sender,sel,ptr);
  if(isEnabled() && canFocus()) setFocus();
  return 1;
  }


// Leave
long FXMenuCommand::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXMenuCaption::onLeave(sender,sel,ptr);
  if(isEnabled() && canFocus()) killFocus();
  return 1;
  }


// Pressed button
long FXMenuCommand::onButtonPress(FXObject*,FXSelector,void*){
  if(!isEnabled()) return 0;
  return 1;
  }


// Released button
long FXMenuCommand::onButtonRelease(FXObject*,FXSelector,void*){
  FXbool active=isActive();
  if(!isEnabled()) return 0;
  getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  if(active && target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1); }
  return 1;
  }


// Keyboard press
long FXMenuCommand::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    FXTRACE((200,"%s::onKeyPress %p keysym=0x%04x state=%04x\n",getClassName(),this,event->code,event->state));
    if(event->code==KEY_space || event->code==KEY_KP_Space || event->code==KEY_Return || event->code==KEY_KP_Enter){
      flags|=FLAG_PRESSED;
      return 1;
      }
    }
  return 0;
  }


// Keyboard release
long FXMenuCommand::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    FXTRACE((200,"%s::onKeyRelease %p keysym=0x%04x state=%04x\n",getClassName(),this,event->code,event->state));
    if(event->code==KEY_space || event->code==KEY_KP_Space || event->code==KEY_Return || event->code==KEY_KP_Enter){
      flags&=~FLAG_PRESSED;
      getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXMenuCommand::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  FXTRACE((200,"%s::onHotKeyPress %p\n",getClassName(),this));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    flags|=FLAG_PRESSED;
    }
  return 1;
  }


// Hot key combination released
long FXMenuCommand::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onHotKeyRelease %p\n",getClassName(),this));
  if(isEnabled() && (flags&FLAG_PRESSED)){
    flags&=~FLAG_PRESSED;
    getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
    }
  return 1;
  }


// Accelerator activated
long FXMenuCommand::onCmdAccel(FXObject*,FXSelector,void*){
  if(isEnabled()){
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)1);
    return 1;
    }
  return 0;
  }


// Into focus chain
void FXMenuCommand::setFocus(){
  FXMenuCaption::setFocus();
  flags|=FLAG_ACTIVE;
  flags&=~FLAG_UPDATE;
  update();
  }


// Out of focus chain
void FXMenuCommand::killFocus(){
  FXMenuCaption::killFocus();
  flags&=~FLAG_ACTIVE;
  flags|=FLAG_UPDATE;
  update();
  }


// Handle repaint
long FXMenuCommand::onPaint(FXObject*,FXSelector,void* ptr){
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
      if(!accel.empty()) dc.drawText(width-TRAILSPACE-font->getTextWidth(accel)+1,yy+1,accel);
      if(0<=hotoff) dc.fillRectangle(xx+font->getTextWidth(&label[0],hotoff)+1,yy+2,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
      dc.setForeground(shadowColor);
      dc.drawText(xx,yy,label);
      if(!accel.empty()) dc.drawText(width-TRAILSPACE-font->getTextWidth(accel),yy,accel);
      if(0<=hotoff) dc.fillRectangle(xx+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
      }
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
      if(!accel.empty()) dc.drawText(width-TRAILSPACE-font->getTextWidth(accel),yy,accel);
      if(0<=hotoff) dc.fillRectangle(xx+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
      }
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
      if(!accel.empty()) dc.drawText(width-TRAILSPACE-font->getTextWidth(accel),yy,accel);
      if(0<=hotoff) dc.fillRectangle(xx+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
      }
    }
  return 1;
  }


// Change accelerator text; note this just changes the text!
// This is because the accelerator's target may be different from
// the MenuCommands and we don't want to blow it away.
void FXMenuCommand::setAccelText(const FXString& text){
  if(accel!=text){
    accel=text;
    recalc();
    update();
    }
  }


// Save object to stream
void FXMenuCommand::save(FXStream& store) const {
  FXMenuCaption::save(store);
  store << accel;
  store << acckey;
  }


// Load object from stream
void FXMenuCommand::load(FXStream& store){
  FXMenuCaption::load(store);
  store >> accel;
  store >> acckey;
  }


// Need to uninstall accelerator
FXMenuCommand::~FXMenuCommand(){
  FXAccelTable *table;
  FXWindow *own;
  if(acckey){
    own=getShell()->getOwner();
    if(own){
      table=own->getAccelTable();
      if(table){
        table->removeAccel(acckey);
        }
      }
    }
  }

}
