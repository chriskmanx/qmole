/********************************************************************************
*                                                                               *
*               T r i - S t a t e    B u t t o n    W i d g e t                 *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Charles Warren.   All Rights Reserved.             *
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
* $Id: FXTriStateButton.cpp,v 1.15 2006/01/22 17:58:50 fox Exp $                *
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
#include "FXIcon.h"
#include "FXToggleButton.h"
#include "FXTriStateButton.h"

/*
  Notes:

  - Small change from Charles' original code:- normal label and
    icon are now used as fallback; this means its possible for example
    to just specify three icons and use same text for all, or vice-versa
    just specify three labels and use same icons for all.
  - Likewise, help text and tip text follow similar fallback scheme.
  - Since there is no way INTO the MAYBE state except programmatically,
    there is no attempt to install a hotkey for this!
  - The fallback logic needs to be properly adhered to in the size computation.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXTriStateButton) FXTriStateButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXTriStateButton::onPaint),
  FXMAPFUNC(SEL_UPDATE,0,FXTriStateButton::onUpdate),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXTriStateButton::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXTriStateButton::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_UNKNOWN,FXTriStateButton::onUnknown)
  };


// Object implementation
FXIMPLEMENT(FXTriStateButton,FXToggleButton,FXTriStateButtonMap,ARRAYNUMBER(FXTriStateButtonMap))


// Deserialization
FXTriStateButton::FXTriStateButton(){
  maybeicon=(FXIcon*)-1L;
  }


// Construct and init
FXTriStateButton::FXTriStateButton(FXComposite* p,const FXString& text1,const FXString& text2,const FXString& text3,FXIcon* icon1,FXIcon* icon2,FXIcon* icon3,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXToggleButton(p,text1,text2,icon1,icon2,tgt,sel,opts,x,y,w,h,pl,pr,pt,pb){
  maybelabel=text3.section('\t',0);
  maybetip=text3.section('\t',1);
  maybehelp=text3.section('\t',2);
  maybeicon=icon3;
  }


// Create window
void FXTriStateButton::create(){
  FXToggleButton::create();
  if(maybeicon) maybeicon->create();
  }


// Detach window
void FXTriStateButton::detach(){
  FXToggleButton::detach();
  if(maybeicon) maybeicon->detach();
  }


// Get default width
FXint FXTriStateButton::getDefaultWidth(){
  FXint tw,iw,s,w1,w2,w3;

  tw=iw=s=0;
  if(!label.empty()) tw=labelWidth(label);
  if(icon) iw=icon->getWidth();
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w1=FXMAX(tw,iw); else w1=tw+iw+s;

  tw=iw=s=0;
  if(!altlabel.empty()) tw=labelWidth(altlabel);
  if(alticon) iw=alticon->getWidth();
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w2=FXMAX(tw,iw); else w2=tw+iw+s;

  tw=iw=s=0;
  if(!maybelabel.empty()) tw=labelWidth(maybelabel);
  if(maybeicon) iw=maybeicon->getWidth();
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w3=FXMAX(tw,iw); else w3=tw+iw+s;

  return FXMAX3(w1,w2,w3)+padleft+padright+(border<<1);
  }


// Get default height
FXint FXTriStateButton::getDefaultHeight(){
  FXint th,ih,h1,h2,h3;

  th=ih=0;
  if(!label.empty()) th=labelHeight(label);
  if(icon) ih=icon->getHeight();
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h1=FXMAX(th,ih); else h1=th+ih;

  th=ih=0;
  if(!altlabel.empty()) th=labelHeight(altlabel);
  if(alticon) ih=alticon->getHeight();
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h2=FXMAX(th,ih); else h2=th+ih;

  th=ih=0;
  if(!maybelabel.empty()) th=labelHeight(maybelabel);
  if(maybeicon) ih=maybeicon->getHeight();
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h3=FXMAX(th,ih); else h3=th+ih;

  return FXMAX3(h1,h2,h3)+padtop+padbottom+(border<<1);
  }


// Check the menu button
long FXTriStateButton::onUnknown(FXObject*,FXSelector,void*){
  setState(MAYBE);
  return 1;
  }


// We were asked about status text
long FXTriStateButton::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if(flags&FLAG_HELP){
    if(state==TRUE){
      if(!althelp.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&althelp);
        return 1;
        }
      }
    else if(state==MAYBE){
      if(!maybehelp.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&maybehelp);
        return 1;
        }
      }
    if(!help.empty()){
      if(!help.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
        return 1;
        }
      }
    }
  return 0;
  }


// We were asked about tip text
long FXTriStateButton::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if(flags&FLAG_TIP){
    if(state==TRUE){
      if(!alttip.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&alttip);
        return 1;
        }
      }
    else if(state==MAYBE){
      if(!maybetip.empty()){
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&maybetip);
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
long FXTriStateButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXint tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);

  // Got a border at all?
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){

    // Toolbar style
    if(options&TOGGLEBUTTON_TOOLBAR){

      // Enabled and cursor inside and down
      if(down){
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
      if(down){
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
  if(state==TRUE && !altlabel.empty()){
    tw=labelWidth(altlabel);
    th=labelHeight(altlabel);
    }
  else if(state==MAYBE && !maybelabel.empty()){
    tw=labelWidth(maybelabel);
    th=labelHeight(maybelabel);
    }
  else if(!label.empty()){
    tw=labelWidth(label);
    th=labelHeight(label);
    }
  if(state==TRUE && alticon){
    iw=alticon->getWidth();
    ih=alticon->getHeight();
    }
  else if(state==MAYBE && maybeicon){
    iw=maybeicon->getWidth();
    ih=maybeicon->getHeight();
    }
  else if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }

  just_x(tx,ix,tw,iw);
  just_y(ty,iy,th,ih);

  // Shift a bit when pressed
  if(down && (options&(FRAME_RAISED|FRAME_SUNKEN))){ ++tx; ++ty; ++ix; ++iy; }

  // Draw enabled state
  if(isEnabled()){

    // Paint the right icon
    if(state==TRUE && alticon){
      dc.drawIcon(alticon,ix,iy);
      }
    else if(state==MAYBE &&maybeicon){
      dc.drawIcon(maybeicon,ix,iy);
      }
    else if(icon){
      dc.drawIcon(icon,ix,iy);
      }

    // Paint the right text
    if(state==TRUE && !altlabel.empty()){
      dc.setFont(font);
      dc.setForeground(textColor);
      drawLabel(dc,altlabel,althotoff,tx,ty,tw,th);
      }
    else if(state==MAYBE && !maybelabel.empty()){
      dc.setFont(font);
      dc.setForeground(textColor);
      drawLabel(dc,maybelabel,-1,tx,ty,tw,th);
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

    // Paint the right icon
    if(state==TRUE && alticon){
      dc.drawIconSunken(alticon,ix,iy);
      }
    else if(state==MAYBE &&maybeicon){
      dc.drawIconSunken(maybeicon,ix,iy);
      }
    else if(icon){
      dc.drawIconSunken(icon,ix,iy);
      }

    // Paint the right text
    if(state==TRUE && !altlabel.empty()){
      dc.setFont(font);
      dc.setForeground(hiliteColor);
      drawLabel(dc,altlabel,althotoff,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,altlabel,althotoff,tx,ty,tw,th);
      }
    else if(state==MAYBE && !maybelabel.empty()){
      dc.setFont(font);
      dc.setForeground(hiliteColor);
      drawLabel(dc,maybelabel,-1,tx+1,ty+1,tw,th);
      dc.setForeground(shadowColor);
      drawLabel(dc,maybelabel,-1,tx,ty,tw,th);
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
void FXTriStateButton::setMaybeText(const FXString& text){
  if(maybelabel!=text){
    maybelabel=text;
    recalc();
    update();
    }
  }


// Change icon
void FXTriStateButton::setMaybeIcon(FXIcon* ic){
  if(maybeicon!=ic){
    maybeicon=ic;
    recalc();
    update();
    }
  }


// Change help text
void FXTriStateButton::setMaybeHelpText(const FXString& text){
  maybehelp=text;
  }


// Change tip text
void FXTriStateButton::setMaybeTipText(const FXString& text){
  maybetip=text;
  }


// Save object to stream
void FXTriStateButton::save(FXStream& store) const {
  FXToggleButton::save(store);
  store << maybelabel;
  store << maybeicon;
  store << maybetip;
  store << maybehelp;
  }


// Load object from stream
void FXTriStateButton::load(FXStream& store){
  FXToggleButton::load(store);
  store >> maybelabel;
  store >> maybeicon;
  store >> maybetip;
  store >> maybehelp;
  }


// Destruct
FXTriStateButton::~FXTriStateButton(){
  maybeicon=(FXIcon*)-1L;
  }

}
