/********************************************************************************
*                                                                               *
*                     T o o l   B a r   T a b   O b j e c t                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXToolBarTab.cpp,v 1.22 2006/01/22 17:58:48 fox Exp $                    *
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
#include "FXToolBarTab.h"


/*
  To do:
*/


// Size
#define TOOLBARTAB_WIDTH    9       // Width for horizontal toolbar tab
#define TOOLBARTAB_HEIGHT   24      // Height for horizontal toolbar tab

// Tool Bar Tab styles
#define TOOLBARTAB_MASK        TOOLBARTAB_VERTICAL

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXToolBarTab) FXToolBarTabMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXToolBarTab::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXToolBarTab::onUpdate),
  FXMAPFUNC(SEL_ENTER,0,FXToolBarTab::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXToolBarTab::onLeave),
  FXMAPFUNC(SEL_UNGRABBED,0,FXToolBarTab::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXToolBarTab::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXToolBarTab::onLeftBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXToolBarTab::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXToolBarTab::onKeyRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXToolBarTab::onQueryTip),
  FXMAPFUNC(SEL_UPDATE,FXToolBarTab::ID_COLLAPSE,FXToolBarTab::onUpdCollapse),
  FXMAPFUNC(SEL_UPDATE,FXToolBarTab::ID_UNCOLLAPSE,FXToolBarTab::onUpdUncollapse),
  FXMAPFUNC(SEL_COMMAND,FXToolBarTab::ID_COLLAPSE,FXToolBarTab::onCmdCollapse),
  FXMAPFUNC(SEL_COMMAND,FXToolBarTab::ID_UNCOLLAPSE,FXToolBarTab::onCmdUncollapse),
  FXMAPFUNC(SEL_COMMAND,FXToolBarTab::ID_SETTIPSTRING,FXToolBarTab::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXToolBarTab::ID_GETTIPSTRING,FXToolBarTab::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXToolBarTab,FXFrame,FXToolBarTabMap,ARRAYNUMBER(FXToolBarTabMap))


// Deserialization
FXToolBarTab::FXToolBarTab(){
  flags|=FLAG_ENABLED;
  activeColor=FXRGB(150,156,224);
  collapsed=FALSE;
  down=FALSE;
  }


// Construct and init
FXToolBarTab::FXToolBarTab(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXFrame(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
  activeColor=FXRGB(150,156,224);
  target=tgt;
  message=sel;
  collapsed=FALSE;
  down=FALSE;
  }


// If window can have focus
bool FXToolBarTab::canFocus() const { return true; }


// Enable the window
void FXToolBarTab::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXFrame::enable();
    update();
    }
  }


// Disable the window
void FXToolBarTab::disable(){
  if(flags&FLAG_ENABLED){
    FXFrame::disable();
    update();
    }
  }


// Get default width
FXint FXToolBarTab::getDefaultWidth(){
  FXWindow *sibling=getNext() ? getNext() : getPrev();
  FXint w;
  if(options&TOOLBARTAB_VERTICAL){          // Vertical
    if(collapsed){
      w=TOOLBARTAB_WIDTH;
      }
    else{
      w=TOOLBARTAB_HEIGHT;
      if(sibling) w=sibling->getDefaultWidth();
      }
    }
  else{                                     // Horizontal
    if(collapsed){
      w=TOOLBARTAB_HEIGHT;
      if(sibling) w=sibling->getDefaultHeight();
      }
    else{
      w=TOOLBARTAB_WIDTH;
      }
    }
  return w;
  }


// Get default height
FXint FXToolBarTab::getDefaultHeight(){
  FXWindow *sibling=getNext() ? getNext() : getPrev();
  FXint h;
  if(options&TOOLBARTAB_VERTICAL){          // Vertical
    if(collapsed){
      h=TOOLBARTAB_HEIGHT;
      if(sibling) h=sibling->getDefaultWidth();
      }
    else{
      h=TOOLBARTAB_WIDTH;
      }
    }
  else{                                     // Horizontal
    if(collapsed){
      h=TOOLBARTAB_WIDTH;
      }
    else{
      h=TOOLBARTAB_HEIGHT;
      if(sibling) h=sibling->getDefaultHeight();
      }
    }
  return h;
  }


// Collapse or uncollapse
void FXToolBarTab::collapse(FXbool fold,FXbool notify){
  FXWindow *sibling;
  if(fold!=collapsed){
    sibling=getNext() ? getNext() : getPrev();
    if(sibling){
      if(fold){
        sibling->hide();
        }
      else{
        sibling->show();
        }
      }
    collapsed=fold;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)fold);
    }
  }


// Update
long FXToolBarTab::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  FXWindow *sibling=getNext() ? getNext() : getPrev();
  FXFrame::onUpdate(sender,sel,ptr);
  if(sibling){
    if(sibling->shown() && collapsed){
      collapsed=FALSE;
      update();
      recalc();
      }
    else if(!sibling->shown() && !collapsed){
      collapsed=TRUE;
      update();
      recalc();
      }
    }
  return 1;
  }


// Entered button
long FXToolBarTab::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onEnter(sender,sel,ptr);
  if(isEnabled()){
    if(flags&FLAG_PRESSED) down=TRUE;
    update();
    }
  return 1;
  }


// Leave button
long FXToolBarTab::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onLeave(sender,sel,ptr);
  if(isEnabled()){
    if(flags&FLAG_PRESSED) down=FALSE;
    update();
    }
  return 1;
  }


// Pressed mouse button
long FXToolBarTab::onLeftBtnPress(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXFrame::onLeftBtnPress(sender,sel,ptr)){
    if(isEnabled() && !(flags&FLAG_PRESSED)){
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      down=TRUE;
      update();
      return 1;
      }
    }
  return 0;
  }


// Released mouse button
long FXToolBarTab::onLeftBtnRelease(FXObject* sender,FXSelector sel,void* ptr){
  FXbool click=down;
  if(!FXFrame::onLeftBtnRelease(sender,sel,ptr)){
    if(isEnabled() && (flags&FLAG_PRESSED)){
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      down=FALSE;
      update();
      if(click) collapse(!collapsed,TRUE);
      return 1;
      }
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXToolBarTab::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  flags&=~FLAG_PRESSED;
  flags|=FLAG_UPDATE;
  down=FALSE;
  update();
  return 1;
  }


// Key Press
long FXToolBarTab::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled() && !(flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      down=TRUE;
      update();
      flags|=FLAG_PRESSED;
      flags&=~FLAG_UPDATE;
      return 1;
      }
    }
  return 0;
  }


// Key Release
long FXToolBarTab::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && (flags&FLAG_PRESSED)){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    if(event->code==KEY_space || event->code==KEY_KP_Space){
      down=FALSE;
      update();
      flags|=FLAG_UPDATE;
      flags&=~FLAG_PRESSED;
      collapse(!collapsed,TRUE);
      return 1;
      }
    }
  return 0;
  }


// Collapse
long FXToolBarTab::onCmdCollapse(FXObject*,FXSelector,void*){
  collapse(TRUE,TRUE);
  return 1;
  }


// Update collapse
long FXToolBarTab::onUpdCollapse(FXObject* sender,FXSelector,void*){
  if(collapsed)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Uncollapse
long FXToolBarTab::onCmdUncollapse(FXObject*,FXSelector,void*){
  collapse(FALSE,TRUE);
  return 1;
  }


// Update uncollapse
long FXToolBarTab::onUpdUncollapse(FXObject* sender,FXSelector,void*){
  if(!collapsed)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// We were asked about tip text
long FXToolBarTab::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// Set tip using a message
long FXToolBarTab::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXToolBarTab::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// Draw horizontal speckles
void FXToolBarTab::drawHSpeckles(FXDCWindow& dc,FXint x,FXint w){
  register FXint i;
  dc.setForeground(hiliteColor);
  for(i=0; i<w-5; i+=4){dc.drawPoint(x+i,2);dc.drawPoint(x+i+1,5);}
  dc.setForeground(shadowColor);
  for(i=0; i<w-5; i+=4){dc.drawPoint(x+i+1,3);dc.drawPoint(x+i+2,6);}
  }


// Draw vertical speckles
void FXToolBarTab::drawVSpeckles(FXDCWindow& dc,FXint y,FXint h){
  register FXint i;
  dc.setForeground(hiliteColor);
  for(i=0; i<h-5; i+=3){dc.drawPoint(2,y+i+1);dc.drawPoint(5,y+i);}
  dc.setForeground(shadowColor);
  for(i=0; i<h-5; i+=3){dc.drawPoint(6,y+i+1);dc.drawPoint(3,y+i+2);}
  }


// Draw up arrow
void FXToolBarTab::drawUpArrow(FXDCWindow& dc){
  dc.setForeground(borderColor);
  dc.drawLine(2,height-5,6,height-5);
  dc.drawPoint(3,height-6);
  dc.drawPoint(4,height-7);
  dc.drawPoint(5,height-6);
  dc.drawPoint(4,height-6);
  }


// Draw down arrow
void FXToolBarTab::drawDownArrow(FXDCWindow& dc){
  dc.setForeground(borderColor);
  dc.drawLine(2,4,6,4);
  dc.drawPoint(3,5);
  dc.drawPoint(4,6);
  dc.drawPoint(5,5);
  dc.drawPoint(4,5);
  }


// Draw left arrow
void FXToolBarTab::drawLeftArrow(FXDCWindow& dc){
  dc.setForeground(borderColor);
  dc.drawLine(width-5,2,width-5,6);
  dc.drawPoint(width-6,3);
  dc.drawPoint(width-7,4);
  dc.drawPoint(width-6,5);
  dc.drawPoint(width-6,4);
  }


// Draw right arrow
void FXToolBarTab::drawRightArrow(FXDCWindow& dc){
  dc.setForeground(borderColor);
  dc.drawLine(4,2,4,6);
  dc.drawPoint(5,3);
  dc.drawPoint(6,4);
  dc.drawPoint(5,5);
  dc.drawPoint(5,4);
  }


// Handle repaint
long FXToolBarTab::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);

  // Got a border at all?
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){

    // Draw sunken if enabled and either checked or pressed
    if(isEnabled() && down){
      if(down) dc.setForeground(hiliteColor); else dc.setForeground(backColor);
      dc.fillRectangle(border,border,width-border*2,height-border*2);
      if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width,height);
      else drawSunkenRectangle(dc,0,0,width,height);
      }

    // Draw in up state if disabled or up
    else{
      if(underCursor())
        dc.setForeground(activeColor);
      else
        dc.setForeground(backColor);
      dc.fillRectangle(border,border,width-border*2,height-border*2);
      if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
      else drawRaisedRectangle(dc,0,0,width,height);
      }
    }

  // No borders
  else{
    if(isEnabled() && down){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(0,0,width,height);
      }
    else{
      if(underCursor())
        dc.setForeground(activeColor);
      else
        dc.setForeground(backColor);
      dc.fillRectangle(0,0,width,height);
      }
    }

  // Draw spickles
  if(options&TOOLBARTAB_VERTICAL){          // Vertical
    if(collapsed){
      if(options&LAYOUT_BOTTOM){
        drawVSpeckles(dc,3,height-10);
        drawUpArrow(dc);
        }
      else{
        drawVSpeckles(dc,10,height-10);
        drawDownArrow(dc);
        }
      }
    else{
      if(options&LAYOUT_RIGHT){
        drawHSpeckles(dc,3,width-10);
        drawLeftArrow(dc);
        }
      else{
        drawHSpeckles(dc,10,width-10);
        drawRightArrow(dc);
        }
      }
    }
  else{                                     // Horizontal
    if(collapsed){
      if(options&LAYOUT_RIGHT){
        drawHSpeckles(dc,3,width-10);
        drawLeftArrow(dc);
        }
      else{
        drawHSpeckles(dc,10,width-10);
        drawRightArrow(dc);
        }
      }
    else{
      if(options&LAYOUT_BOTTOM){
        drawVSpeckles(dc,3,height-10);
        drawUpArrow(dc);
        }
      else{
        drawVSpeckles(dc,10,height-10);
        drawDownArrow(dc);
        }
      }
    }
  return 1;
  }


// Change tab style
void FXToolBarTab::setTabStyle(FXuint style){
  FXuint opts=(options&~TOOLBARTAB_MASK) | (style&TOOLBARTAB_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get tab style
FXuint FXToolBarTab::getTabStyle() const {
  return (options&TOOLBARTAB_MASK);
  }


// Set text color
void FXToolBarTab::setActiveColor(FXColor clr){
  if(clr!=activeColor){
    activeColor=clr;
    update();
    }
  }


// Save object to stream
void FXToolBarTab::save(FXStream& store) const {
  FXFrame::save(store);
  store << activeColor;
  store << tip;
  store << collapsed;
  }



// Load object from stream
void FXToolBarTab::load(FXStream& store){
  FXFrame::load(store);
  store >> activeColor;
  store >> tip;
  store >> collapsed;
  }

}
