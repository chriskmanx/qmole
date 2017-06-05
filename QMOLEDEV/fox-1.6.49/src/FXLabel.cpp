/********************************************************************************
*                                                                               *
*                            L a b e l   W i d g e t                            *
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
* $Id: FXLabel.cpp,v 1.59.2.1 2006/12/11 15:57:26 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXAccelTable.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXLabel.h"



/*
  Notes:
  - When changing icon/font/etc, we should only recalc and update when it's different.
  - When text changes, do we delete the hot key, or parse it from the new label?
  - It makes sense for certain ``passive'' widgets such as labels to have onUpdate;
    for example, to show/hide/whatever based on changing data structures.
*/

#define JUSTIFY_MASK    (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)
#define ICON_TEXT_MASK  (ICON_AFTER_TEXT|ICON_BEFORE_TEXT|ICON_ABOVE_TEXT|ICON_BELOW_TEXT)

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXLabel) FXLabelMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXLabel::onPaint),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXLabel::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXLabel::onQueryHelp),
  FXMAPFUNC(SEL_KEYPRESS,FXLabel::ID_HOTKEY,FXLabel::onHotKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,FXLabel::ID_HOTKEY,FXLabel::onHotKeyRelease),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_SETVALUE,FXLabel::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_SETSTRINGVALUE,FXLabel::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_GETSTRINGVALUE,FXLabel::onCmdGetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_SETICONVALUE,FXLabel::onCmdSetIconValue),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_GETICONVALUE,FXLabel::onCmdGetIconValue),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_SETHELPSTRING,FXLabel::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_GETHELPSTRING,FXLabel::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_SETTIPSTRING,FXLabel::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXLabel::ID_GETTIPSTRING,FXLabel::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXLabel,FXFrame,FXLabelMap,ARRAYNUMBER(FXLabelMap))


// Deserialization
FXLabel::FXLabel(){
  flags|=FLAG_ENABLED;
  icon=(FXIcon*)-1L;
  font=(FXFont*)-1L;
  hotkey=0;
  hotoff=0;
  textColor=0;
  }


// Make a label
FXLabel::FXLabel(FXComposite* p,const FXString& text,FXIcon* ic,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  FXString string=text.section('\t',0);
  flags|=FLAG_ENABLED;
  label=stripHotKey(string);
  tip=text.section('\t',1);
  help=text.section('\t',2);
  icon=ic;
  font=getApp()->getNormalFont();
  textColor=getApp()->getForeColor();
  hotkey=parseHotKey(string);
  hotoff=findHotKey(string);
  addHotKey(hotkey);
  }


// Create window
void FXLabel::create(){
  FXFrame::create();
  font->create();
  if(icon) icon->create();
  }


// Detach window
void FXLabel::detach(){
  FXFrame::detach();
  font->detach();
  if(icon) icon->detach();
  }


// Enable the window
void FXLabel::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXFrame::enable();
    update();
    }
  }


// Disable the window
void FXLabel::disable(){
  if(flags&FLAG_ENABLED){
    FXFrame::disable();
    update();
    }
  }


// Get height of multi-line label
FXint FXLabel::labelHeight(const FXString& text) const {
  register FXint beg,end;
  register FXint th=0;
  beg=0;
  do{
    end=beg;
    while(end<text.length() && text[end]!='\n') end++;
    th+=font->getFontHeight();
    beg=end+1;
    }
  while(end<text.length());
  return th;
  }


// Get width of multi-line label
FXint FXLabel::labelWidth(const FXString& text) const {
  register FXint beg,end;
  register FXint w,tw=0;
  beg=0;
  do{
    end=beg;
    while(end<text.length() && text[end]!='\n') end++;
    if((w=font->getTextWidth(&text[beg],end-beg))>tw) tw=w;
    beg=end+1;
    }
  while(end<text.length());
  return tw;
  }


// Justify stuff in x-direction
void FXLabel::just_x(FXint& tx,FXint& ix,FXint tw,FXint iw){
  FXint s=0;
  if(iw && tw) s=4;
  if((options&JUSTIFY_LEFT) && (options&JUSTIFY_RIGHT)){
    if(options&ICON_BEFORE_TEXT){ ix=padleft+border; tx=width-padright-border-tw; }
    else if(options&ICON_AFTER_TEXT){ tx=padleft+border; ix=width-padright-border-iw; }
    else{ ix=border+padleft; tx=border+padleft; }
    }
  else if(options&JUSTIFY_LEFT){
    if(options&ICON_BEFORE_TEXT){ ix=padleft+border; tx=ix+iw+s; }
    else if(options&ICON_AFTER_TEXT){ tx=padleft+border; ix=tx+tw+s; }
    else{ ix=border+padleft; tx=border+padleft; }
    }
  else if(options&JUSTIFY_RIGHT){
    if(options&ICON_BEFORE_TEXT){ tx=width-padright-border-tw; ix=tx-iw-s; }
    else if(options&ICON_AFTER_TEXT){ ix=width-padright-border-iw; tx=ix-tw-s; }
    else{ ix=width-padright-border-iw; tx=width-padright-border-tw; }
    }
  else{
    if(options&ICON_BEFORE_TEXT){ ix=border+padleft+(width-padleft-padright-(border<<1)-tw-iw-s)/2; tx=ix+iw+s; }
    else if(options&ICON_AFTER_TEXT){ tx=border+padleft+(width-padleft-padright-(border<<1)-tw-iw-s)/2; ix=tx+tw+s; }
    else{ ix=border+padleft+(width-padleft-padright-(border<<1)-iw)/2; tx=border+padleft+(width-padleft-padright-(border<<1)-tw)/2; }
    }
  }


// Justify stuff in y-direction
void FXLabel::just_y(FXint& ty,FXint& iy,FXint th,FXint ih){
  if((options&JUSTIFY_TOP) && (options&JUSTIFY_BOTTOM)){
    if(options&ICON_ABOVE_TEXT){ iy=padtop+border; ty=height-padbottom-border-th; }
    else if(options&ICON_BELOW_TEXT){ ty=padtop+border; iy=height-padbottom-border-ih; }
    else{ iy=border+padtop; ty=border+padtop; }
    }
  else if(options&JUSTIFY_TOP){
    if(options&ICON_ABOVE_TEXT){ iy=padtop+border; ty=iy+ih; }
    else if(options&ICON_BELOW_TEXT){ ty=padtop+border; iy=ty+th; }
    else{ iy=border+padtop; ty=border+padtop; }
    }
  else if(options&JUSTIFY_BOTTOM){
    if(options&ICON_ABOVE_TEXT){ ty=height-padbottom-border-th; iy=ty-ih; }
    else if(options&ICON_BELOW_TEXT){ iy=height-padbottom-border-ih; ty=iy-th; }
    else{ iy=height-padbottom-border-ih; ty=height-padbottom-border-th; }
    }
  else{
    if(options&ICON_ABOVE_TEXT){ iy=border+padtop+(height-padbottom-padtop-(border<<1)-th-ih)/2; ty=iy+ih; }
    else if(options&ICON_BELOW_TEXT){ ty=border+padtop+(height-padbottom-padtop-(border<<1)-th-ih)/2; iy=ty+th; }
    else{ iy=border+padtop+(height-padbottom-padtop-(border<<1)-ih)/2; ty=border+padtop+(height-padbottom-padtop-(border<<1)-th)/2; }
    }
  }




// Draw multi-line label, with underline for hotkey
void FXLabel::drawLabel(FXDCWindow& dc,const FXString& text,FXint hot,FXint tx,FXint ty,FXint tw,FXint){
  register FXint beg,end;
  register FXint xx,yy;
  yy=ty+font->getFontAscent();
  beg=0;
  do{
    end=beg;
    while(end<text.length() && text[end]!='\n') end++;
    if(options&JUSTIFY_LEFT) xx=tx;
    else if(options&JUSTIFY_RIGHT) xx=tx+tw-font->getTextWidth(&text[beg],end-beg);
    else xx=tx+(tw-font->getTextWidth(&text[beg],end-beg))/2;
    dc.drawText(xx,yy,&text[beg],end-beg);
    if(beg<=hot && hot<end){
      dc.fillRectangle(xx+font->getTextWidth(&text[beg],hot-beg),yy+1,font->getTextWidth(&text[hot],wclen(&text[hot])),1);
      }
    yy+=font->getFontHeight();
    beg=end+1;
    }
  while(end<text.length());
  }


// Get default width
FXint FXLabel::getDefaultWidth(){
  FXint tw=0,iw=0,s=0,w;
  if(!label.empty()){
    tw=labelWidth(label);
    }
  if(icon){
    iw=icon->getWidth();
    }
  if(iw && tw) s=4;
  if(!(options&(ICON_AFTER_TEXT|ICON_BEFORE_TEXT))) w=FXMAX(tw,iw); else w=tw+iw+s;
  return w+padleft+padright+(border<<1);
  }


// Get default height
FXint FXLabel::getDefaultHeight(){
  FXint th=0,ih=0,h;
  if(!label.empty()){
    th=labelHeight(label);
    }
  if(icon){
    ih=icon->getHeight();
    }
  if(!(options&(ICON_ABOVE_TEXT|ICON_BELOW_TEXT))) h=FXMAX(th,ih); else h=th+ih;
  return h+padtop+padbottom+(border<<1);
  }


// Update value from a message
long FXLabel::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setText((const FXchar*)ptr);
  return 1;
  }


// Update value from a message
long FXLabel::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setText(*((FXString*)ptr));
  return 1;
  }


// Obtain value from text field
long FXLabel::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getText();
  return 1;
  }


// Update icon from a message
long FXLabel::onCmdSetIconValue(FXObject*,FXSelector,void* ptr){
  setIcon(*((FXIcon**)ptr));
  return 1;
  }


// Obtain icon from text field
long FXLabel::onCmdGetIconValue(FXObject*,FXSelector,void* ptr){
  *((FXIcon**)ptr)=getIcon();
  return 1;
  }


// Handle repaint
long FXLabel::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent   *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXint      tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
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
  if(icon){
    if(isEnabled())
      dc.drawIcon(icon,ix,iy);
    else
      dc.drawIconSunken(icon,ix,iy);
    }
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
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Move the focus to the next focusable child following the
// Label widget.  Thus, placing a label with accelerator in front
// of e.g. a TextField gives a convenient method for getting to it.
long FXLabel::onHotKeyPress(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getNext();
  while(child){
    if(child->shown()){
      if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
      if(child->handle(this,FXSEL(SEL_FOCUS_NEXT,0),ptr)) return 1;
      }
    child=child->getNext();
    }
  return 1;
  }


// Nothing much happens here...
long FXLabel::onHotKeyRelease(FXObject*,FXSelector,void*){
  return 1;
  }


// Set help using a message
long FXLabel::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXLabel::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXLabel::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXLabel::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXLabel::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXLabel::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Change text
void FXLabel::setText(const FXString& text){
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
void FXLabel::setIcon(FXIcon* ic){
  if(icon!=ic){
    icon=ic;
    recalc();
    update();
    }
  }


// Change the font
void FXLabel::setFont(FXFont *fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set text color
void FXLabel::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set text justify style
void FXLabel::setJustify(FXuint style){
  FXuint opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get text justify style
FXuint FXLabel::getJustify() const {
  return (options&JUSTIFY_MASK);
  }


// Set icon positioning
void FXLabel::setIconPosition(FXuint mode){
  FXuint opts=(options&~ICON_TEXT_MASK) | (mode&ICON_TEXT_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get icon positioning
FXuint FXLabel::getIconPosition() const {
  return (options&ICON_TEXT_MASK);
  }


// Save object to stream
void FXLabel::save(FXStream& store) const {
  FXFrame::save(store);
  store << label;
  store << icon;
  store << font;
  store << hotkey;
  store << hotoff;
  store << textColor;
  store << tip;
  store << help;
  }


// Load object from stream
void FXLabel::load(FXStream& store){
  FXFrame::load(store);
  store >> label;
  store >> icon;
  store >> font;
  store >> hotkey;
  store >> hotoff;
  store >> textColor;
  store >> tip;
  store >> help;
  }


// Destroy label
FXLabel::~FXLabel(){
  remHotKey(hotkey);
  icon=(FXIcon*)-1L;
  font=(FXFont*)-1L;
  }

}
