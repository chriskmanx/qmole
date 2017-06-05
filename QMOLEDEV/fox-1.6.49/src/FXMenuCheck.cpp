/********************************************************************************
*                                                                               *
*                          M e n u C h e c k   W i d g e t                      *
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
* $Id: FXMenuCheck.cpp,v 1.30 2006/01/22 17:58:36 fox Exp $                     *
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
#include "FXMenuCheck.h"

/*
  Notes:
  - FXMenuCheck should flip state when invoked, and send new state along
    in ptr in callback.
*/


#define LEADSPACE   22
#define TRAILSPACE  16

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuCheck) FXMenuCheckMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuCheck::onPaint),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXMenuCheck::onButtonPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXMenuCheck::onButtonRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXMenuCheck::onButtonPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXMenuCheck::onButtonRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXMenuCheck::onButtonPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXMenuCheck::onButtonRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXMenuCheck::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXMenuCheck::onKeyRelease),
  FXMAPFUNC(SEL_KEYPRESS,FXWindow::ID_HOTKEY,FXMenuCheck::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXWindow::ID_HOTKEY,FXMenuCheck::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_CHECK,FXMenuCheck::onCheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNCHECK,FXMenuCheck::onUncheck),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNKNOWN,FXMenuCheck::onUnknown),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXMenuCheck::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXMenuCheck::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXMenuCheck::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_ACCEL,FXMenuCheck::onCmdAccel),
  };


// Object implementation
FXIMPLEMENT(FXMenuCheck,FXMenuCommand,FXMenuCheckMap,ARRAYNUMBER(FXMenuCheckMap))


// Command menu item
FXMenuCheck::FXMenuCheck(){
  boxColor=0;
  check=FALSE;
  }


// Command menu item
FXMenuCheck::FXMenuCheck(FXComposite* p,const FXString& text,FXObject* tgt,FXSelector sel,FXuint opts):
  FXMenuCommand(p,text,NULL,tgt,sel,opts){
  boxColor=getApp()->getBackColor();
  check=FALSE;
  }


// Get default width
FXint FXMenuCheck::getDefaultWidth(){
  FXint tw,aw;
  tw=aw=0;
  if(!label.empty()) tw=font->getTextWidth(label.text(),label.length());
  if(!accel.empty()) aw=font->getTextWidth(accel.text(),accel.length());
  if(aw && tw) aw+=5;
  return LEADSPACE+tw+aw+TRAILSPACE;
  }


// Get default height
FXint FXMenuCheck::getDefaultHeight(){
  FXint th=0;
  if(!label.empty() || !accel.empty()) th=font->getFontHeight()+5;
  return FXMAX(th,20);
  }


// Check button
void FXMenuCheck::setCheck(FXbool s){
  if(check!=s){
    check=s;
    update();
    }
  }


// Change state to checked
long FXMenuCheck::onCheck(FXObject*,FXSelector,void*){
  setCheck(TRUE);
  return 1;
  }


// Change state to unchecked
long FXMenuCheck::onUncheck(FXObject*,FXSelector,void*){
  setCheck(FALSE);
  return 1;
  }


// Change state to indeterminate
long FXMenuCheck::onUnknown(FXObject*,FXSelector,void*){
  setCheck(MAYBE);
  return 1;
  }


// Update value from a message
long FXMenuCheck::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)(FXuval)ptr);
  return 1;
  }


// Update value from a message
long FXMenuCheck::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCheck((FXbool)*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXMenuCheck::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCheck();
  return 1;
  }


// Pressed button
long FXMenuCheck::onButtonPress(FXObject*,FXSelector,void*){
  if(!isEnabled()) return 0;
  return 1;
  }


// Released button
long FXMenuCheck::onButtonRelease(FXObject*,FXSelector,void*){
  FXbool active=isActive();
  if(!isEnabled()) return 0;
  getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  if(active){
    setCheck(!check);
    if(target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check); }
    }
  return 1;
  }


// Keyboard press
long FXMenuCheck::onKeyPress(FXObject*,FXSelector,void* ptr){
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
long FXMenuCheck::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    FXTRACE((200,"%s::onKeyRelease %p keysym=0x%04x state=%04x\n",getClassName(),this,event->code,event->state));
    if(event->code==KEY_space || event->code==KEY_KP_Space || event->code==KEY_Return || event->code==KEY_KP_Enter){
      flags&=~FLAG_PRESSED;
      setCheck(!check);
      getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
      if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check);
      return 1;
      }
    }
  return 0;
  }


// Hot key combination pressed
long FXMenuCheck::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  FXTRACE((200,"%s::onHotKeyPress %p\n",getClassName(),this));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    flags|=FLAG_PRESSED;
    }
  return 1;
  }


// Hot key combination released
long FXMenuCheck::onHotKeyRelease(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onHotKeyRelease %p\n",getClassName(),this));
  if(isEnabled() && (flags&FLAG_PRESSED)){
    flags&=~FLAG_PRESSED;
    setCheck(!check);
    getParent()->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check);
    }
  return 1;
  }


// Accelerator activated
long FXMenuCheck::onCmdAccel(FXObject*,FXSelector,void*){
  if(isEnabled()){
    setCheck(!check);
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)check);
    return 1;
    }
  return 0;
  }


// Handle repaint
long FXMenuCheck::onPaint(FXObject*,FXSelector,void* ptr){
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

  // Draw the box
  xx=5;
  yy=(height-9)/2;
  if(!isEnabled())
    dc.setForeground(backColor);
  else
    dc.setForeground(boxColor);
  dc.fillRectangle(xx+1,yy+1,8,8);
  dc.setForeground(shadowColor);
  dc.drawRectangle(xx,yy,9,9);

  // Draw the check
  if(check!=FALSE){
    FXSegment seg[6];
    seg[0].x1=2+xx; seg[0].y1=4+yy; seg[0].x2=4+xx; seg[0].y2=6+yy;
    seg[1].x1=2+xx; seg[1].y1=5+yy; seg[1].x2=4+xx; seg[1].y2=7+yy;
    seg[2].x1=2+xx; seg[2].y1=6+yy; seg[2].x2=4+xx; seg[2].y2=8+yy;
    seg[3].x1=4+xx; seg[3].y1=6+yy; seg[3].x2=8+xx; seg[3].y2=2+yy;
    seg[4].x1=4+xx; seg[4].y1=7+yy; seg[4].x2=8+xx; seg[4].y2=3+yy;
    seg[5].x1=4+xx; seg[5].y1=8+yy; seg[5].x2=8+xx; seg[5].y2=4+yy;
    if(isEnabled()){
      if(check==MAYBE)
        dc.setForeground(shadowColor);
      else
        dc.setForeground(textColor);
      }
    else{
      dc.setForeground(shadowColor);
      }
    dc.drawLineSegments(seg,6);
    }

  return 1;
  }


// Set box color
void FXMenuCheck::setBoxColor(FXColor clr){
  if(clr!=boxColor){
    boxColor=clr;
    update();
    }
  }


// Save object to stream
void FXMenuCheck::save(FXStream& store) const {
  FXMenuCommand::save(store);
  store << check;
  store << boxColor;
  }


// Load object from stream
void FXMenuCheck::load(FXStream& store){
  FXMenuCommand::load(store);
  store >> check;
  store >> boxColor;
  }


}
