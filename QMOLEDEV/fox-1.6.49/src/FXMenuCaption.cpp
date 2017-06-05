/********************************************************************************
*                                                                               *
*                       M e n u   C a p t i o n   W i d g e t                   *
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
* $Id: FXMenuCaption.cpp,v 1.53.2.1 2006/12/11 15:57:26 fox Exp $                   *
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
#include "FXMenuCaption.h"

/*
  Notes:
  - Accelerators.
  - Help text from constructor is third part; second part should be
    accelerator key combination.
  - When menu label changes, hotkey might have to be adjusted.
  - Fix it so menu stays up when after Alt-F, you press Alt-E.
  - FXMenuCascade should send ID_POST/ID_UNPOST to self.
  - Look into SEL_FOCUS_SELF some more:- menus should not
    get focus, or at least, return the focus to the original
    widget.
  - Want to support arbitrary large icons.
*/


#define LEADSPACE   22
#define TRAILSPACE  16
#define MENU_MASK   (MENU_AUTOGRAY|MENU_AUTOHIDE)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuCaption) FXMenuCaptionMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuCaption::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXMenuCaption::onUpdate),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXMenuCaption::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXMenuCaption::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_SETSTRINGVALUE,FXMenuCaption::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_GETSTRINGVALUE,FXMenuCaption::onCmdGetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_SETICONVALUE,FXMenuCaption::onCmdSetIconValue),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_GETICONVALUE,FXMenuCaption::onCmdGetIconValue),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_SETHELPSTRING,FXMenuCaption::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_GETHELPSTRING,FXMenuCaption::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_SETTIPSTRING,FXMenuCaption::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXMenuCaption::ID_GETTIPSTRING,FXMenuCaption::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXMenuCaption,FXWindow,FXMenuCaptionMap,ARRAYNUMBER(FXMenuCaptionMap))


// Deserialization
FXMenuCaption::FXMenuCaption(){
  flags|=FLAG_SHOWN;
  }


// Menu entry
FXMenuCaption::FXMenuCaption(FXComposite* p,const FXString& text,FXIcon* ic,FXuint opts):
  FXWindow(p,opts,0,0,0,0){
  FXString string=text.section('\t',0);
  flags|=FLAG_SHOWN;
  label=stripHotKey(string);
  help=text.section('\t',2);
  icon=ic;
  font=getApp()->getNormalFont();
  hotkey=parseHotKey(string);
  hotoff=findHotKey(string);
  addHotKey(hotkey);
  textColor=getApp()->getForeColor();
  seltextColor=getApp()->getSelMenuTextColor();
  selbackColor=getApp()->getSelMenuBackColor();
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  }


// Create Window
void FXMenuCaption::create(){
  FXWindow::create();
  font->create();
  if(icon) icon->create();
  }


// Detach Window
void FXMenuCaption::detach(){
  FXWindow::detach();
  font->detach();
  if(icon) icon->detach();
  }


// Enable the menu entry
void FXMenuCaption::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXWindow::enable();
    update();
    }
  }


// Disable the menu entry
void FXMenuCaption::disable(){
  if(flags&FLAG_ENABLED){
    FXWindow::disable();
    update();
    }
  }


// Get default width
FXint FXMenuCaption::getDefaultWidth(){
  FXint tw,iw;
  tw=iw=0;
  if(!label.empty()) tw=font->getTextWidth(label.text(),label.length());
  if(icon) iw=icon->getWidth()+5;
  return FXMAX(iw,LEADSPACE)+tw+TRAILSPACE;
  }


// Get default height
FXint FXMenuCaption::getDefaultHeight(){
  FXint th,ih;
  th=ih=0;
  if(!label.empty()) th=font->getFontHeight()+5;
  if(icon) ih=icon->getHeight()+5;
  return FXMAX(th,ih);
  }


// Set tip using a message
long FXMenuCaption::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXMenuCaption::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXMenuCaption::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// Set help using a message
long FXMenuCaption::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXMenuCaption::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// We were asked about status text
long FXMenuCaption::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Implement auto-hide or auto-gray modes
long FXMenuCaption::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  if(!FXWindow::onUpdate(sender,sel,ptr)){
    if(options&MENU_AUTOHIDE){if(shown()){hide();recalc();}}
    if(options&MENU_AUTOGRAY){disable();}
    }
  return 1;
  }


// Handle repaint
long FXMenuCaption::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint xx,yy;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  xx=LEADSPACE;
  if(icon){
    dc.drawIcon(icon,3,(height-icon->getHeight())/2);
    if(icon->getWidth()+5>xx) xx=icon->getWidth()+5;
    }
  if(!label.empty()){
    dc.setFont(font);
    dc.setForeground(textColor);
    yy=font->getFontAscent()+(height-font->getFontHeight())/2;
    dc.drawText(xx,yy,label);
    if(0<=hotoff){
      dc.fillRectangle(xx+1+font->getTextWidth(&label[0],hotoff),yy+1,font->getTextWidth(&label[hotoff],wclen(&label[hotoff])),1);
      }
    }
  return 1;
  }


// Update value from a message
long FXMenuCaption::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setText(*((FXString*)ptr));
  return 1;
  }


// Obtain value from text field
long FXMenuCaption::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getText();
  return 1;
  }


// Update icon from a message
long FXMenuCaption::onCmdSetIconValue(FXObject*,FXSelector,void* ptr){
  setIcon(*((FXIcon**)ptr));
  return 1;
  }


// Obtain icon from text field
long FXMenuCaption::onCmdGetIconValue(FXObject*,FXSelector,void* ptr){
  *((FXIcon**)ptr)=getIcon();
  return 1;
  }


// Change help text
void FXMenuCaption::setHelpText(const FXString& text){
  help=text;
  }


// Change text, and scan this text to replace accelerators
void FXMenuCaption::setText(const FXString& text){
  FXString string=stripHotKey(text);
  FXHotKey hkey=parseHotKey(text);
  FXint hoff=findHotKey(text);
  if(label!=string || hkey!=hotkey || hotoff!=hoff){
    label.adopt(string);
    remHotKey(hotkey);
    hotkey=hkey;
    hotoff=hoff;
    addHotKey(hotkey);
    recalc();
    update();
    }
  }


// Change icon
void FXMenuCaption::setIcon(FXIcon* ic){
  if(icon!=ic){
    icon=ic;
    recalc();
    update();
    }
  }


// Change font
void FXMenuCaption::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set menu caption style
void FXMenuCaption::setMenuStyle(FXuint style){
  FXuint opts=(options&~MENU_MASK) | (style&MENU_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get menu caption style
FXuint FXMenuCaption::getMenuStyle() const {
  return (options&MENU_MASK);
  }



// Set text color
void FXMenuCaption::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set select background color
void FXMenuCaption::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXMenuCaption::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    update();
    }
  }


// Set highlight color
void FXMenuCaption::setHiliteColor(FXColor clr){
  if(clr!=hiliteColor){
    hiliteColor=clr;
    update();
    }
  }


// Set shadow color
void FXMenuCaption::setShadowColor(FXColor clr){
  if(clr!=shadowColor){
    shadowColor=clr;
    update();
    }
  }



// Save object to stream
void FXMenuCaption::save(FXStream& store) const {
  FXWindow::save(store);
  store << label;
  store << help;
  store << tip;
  store << icon;
  store << font;
  store << hotoff;
  store << hotkey;
  store << textColor;
  store << selbackColor;
  store << seltextColor;
  store << hiliteColor;
  store << shadowColor;
  }


// Load object from stream
void FXMenuCaption::load(FXStream& store){
  FXWindow::load(store);
  store >> label;
  store >> help;
  store >> tip;
  store >> icon;
  store >> font;
  store >> hotoff;
  store >> hotkey;
  store >> textColor;
  store >> selbackColor;
  store >> seltextColor;
  store >> hiliteColor;
  store >> shadowColor;
  }


// Zap it
FXMenuCaption::~FXMenuCaption(){
  remHotKey(hotkey);
  font=(FXFont*)-1L;
  icon=(FXIcon*)-1L;
  }

}
