/********************************************************************************
*                                                                               *
*                         M e n u R a d i o   W i d g e t                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXMenuRadio.cpp,v 1.33 2006/01/22 17:58:36 fox Exp $                     *
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
#include "FXMenuRadio.h"

/*
  Notes:
  - FXMenuRadio should set state when invoked, and send new state along
    in ptr in callback.
*/


#define LEADSPACE   22
#define TRAILSPACE  16

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuRadio) FXMenuRadioMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuRadio::onPaint),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXMenuRadio::onButtonPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXMenuRadio::onButtonRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXMenuRadio::onButtonPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXMenuRadio::onButtonRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXMenuRadio::onButtonPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXMenuRadio::onButtonRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXMenuRadio::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXMenuRadio::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXMenuRadio::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXMenuRadio::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_CHECK,FXMenuRadio::onCheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNCHECK,FXMenuRadio::onUncheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNKNOWN,FXMenuRadio::onUnknown),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXMenuRadio::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXMenuRadio::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXMenuRadio::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_ACCEL,FXMenuRadio::onCmdAccel),
  };


// Object implementation
FXIMPLEMENT(FXMenuRadio,FXMenuCommand,FXMenuRadioMap,ARRAYNUMBER(FXMenuRadioMap))


// Command menu item
FXMenuRadio::FXMenuRadio(){
  radioColor=0;
  check=FALSE;
  }


// Command menu item
FXMenuRadio::FXMenuRadio(FXComposite* p,const FXString& text,FXObject* tgt,FXSelector sel,FXuint opts):
  FXMenuCommand(p,text,NULL,tgt,sel,opts){
  radioColor=getApp()->getBackColor();
  check=FALSE;
  }


// Get default width
FXint FXMenuRadio::getDefaultWidth(){
  FXint tw,aw;
  tw=aw=0;
  if(!label.empty()) tw=font->getTextWidth(label.text(),label.length());
  if(!accel.empty()) aw=font->getTextWidth(accel.text(),accel.length());
  if(aw && tw) aw+=5;
  return LEADSPACE+tw+aw+TRAILSPACE;
  }


// Get default height
FXint FXMenuRadio::getDefaultHeight(){
  FXint th=0;
  if(!label.empty() || !accel.empty()) th=font->getFontHeight()+5;
  return FXMAX(th,20);
  }


// Check button
void FXMenuRadio::setCheck(FXbool s){
  if(check!=s){
    check=s;
    update();
    }
  }


// Change state to checked
long FXMenuRadio::onCheck(FXObject*,FXSelector,void*){
  setCheck(TRUE);
  return 1;
  }


// Change state to unchecked
long FXMenuRadio::onUncheck(FXObject*,FXSelector,void*){
  setCheck(FALSE);
  return 1;
  }


// Change state to indeterminate
long FXMenuRadio::onUnknown(FXObject*,FXSelector,void*){
  setCheck(MAYBE);
  return 1;
  }


// Update value from a message
long FXMenuRadio::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)(FXuval)ptr);
  return 1;
  }


// Update value from a message
long FXMenuRadio::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXMenuRadio::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCheck();
  return 1;
  }


// Pressed button
long FXMenuRadio::onButtonPress(FXObject*,FXSelector,void*){
  if(!isEnabled()) return 0;
  return 1;
  }


// Released button
long FXMenuRadio::onButtonRelease(FXObject*,FXSelector,void*){
  FXbool active=isActive();
  if(!isEnabled()) return 0;
  getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  if(active){
    setCheck(TRUE);
    if(target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE); }
    }
  return 1;
  }


// Keyboard press
long FXMenuRadio::onKeyPress(FXObject*,FXSelector,void* ptr){
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
long FXMenuRadio::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    FXTRACE((200,"%s::onKeyRelease %p keysym=0x%04x state=%04x\n",getClassName(),this,event->code,event->state));
    if(event->code==KEY_space || event->code==KEY_KP_Space || event->code==KEY_Return || event->code==KEY_KP_Enter){
      flags&=~FLAG_PRESSED;
      setCheck(TRUE);
      getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXMenuRadio::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  FXTRACE((200,"%s::onHotKeyPress %p\n",getClassName(),this));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    flags|=FLAG_PRESSED;
    }
  return 1;
  }


// Hot key combination released
long FXMenuRadio::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onHotKeyRelease %p\n",getClassName(),this));
  if(isEnabled() && (flags&FLAG_PRESSED)){
    flags&=~FLAG_PRESSED;
    setCheck(TRUE);
    getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE);
    }
  return 1;
  }


// Accelerator activated
long FXMenuRadio::onCmdAccel(FXObject*,FXSelector,void*){
  if(isEnabled()){
    setCheck(TRUE);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)TRUE);
    return 1;
    }
  return 0;
  }


// Handle repaint
long FXMenuRadio::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint xx,yy;

  xx=LEADSPACE;

  // Grayed out
  if(!isEnabled()){
    dc.setForeground(backColor);
    dc.fillRectangle(0,0,width,height);
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
    if(!label.empty()){
      yy=font->getFontAscent()+(height-font->getFontHeight())/2;
      dc.setFont(font);
      dc.setForeground(textColor);
      dc.drawText(xx,yy,label);
      if(!accel.empty()) dc.drawText(width-TRAILSPACE-font->getTextWidth(accel),yy,accel);
      if(0<=hotoff) dc.fillRectangle(xx+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
      }
    }

  // Draw the radio
  xx=5;
  yy=(height-9)/2;
  if(!isEnabled())
    dc.setForeground(backColor);
  else
    dc.setForeground(radioColor);
  dc.fillArc(xx,yy,9,9,0,360*64);
  dc.setForeground(shadowColor);
  dc.drawArc(xx,yy,9,9,0,360*64);

  // Draw the bullit
  if(check!=FALSE){
    FXRectangle recs[3];
    recs[0].x=xx+4; recs[0].y=yy+3; recs[0].w=2; recs[0].h=1;
    recs[1].x=xx+3; recs[1].y=yy+4; recs[1].w=4; recs[1].h=2;
    recs[2].x=xx+4; recs[2].y=yy+6; recs[2].w=2; recs[2].h=1;
    if(isEnabled()){
      if(check==MAYBE)
        dc.setForeground(shadowColor);
      else
        dc.setForeground(textColor);
      }
    else{
      dc.setForeground(shadowColor);
      }
    dc.fillRectangles(recs,3);
    }

  return 1;
  }


// Set radio color
void FXMenuRadio::setRadioColor(FXColor clr){
  if(radioColor!=clr){
    radioColor=clr;
    update();
    }
  }


// Save object to stream
void FXMenuRadio::save(FXStream& store) const {
  FXMenuCommand::save(store);
  store << check;
  store << radioColor;
  }


// Load object from stream
void FXMenuRadio::load(FXStream& store){
  FXMenuCommand::load(store);
  store >> check;
  store >> radioColor;
  }

}
