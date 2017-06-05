/********************************************************************************
*                                                                               *
*                               H e a d e r   O b j e c t                       *
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
* $Id: FXHeader.cpp,v 1.103.2.4 2006/11/21 19:01:53 fox Exp $                       *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXObjectList.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXHeader.h"




/*
  Notes:
  - Perhaps, some way to drive via keyboard?
  - Allow some header items to be stretchable.
  - Add minimum, maximum size constraints to items.
  - Should up/down arrows go sideways when vertically oriented?
  - Perhaps perform drawing of border in item also.
  - Like for FXScrollArea, pos is always <=0 when scrolled.
  - Maybe handy feature to initialize size to default item width
    if initial size = 0.
  - Maybe allow attach from right instead of only from left; perhaps
    also attach both (or none).
*/


#define FUDGE         4
#define ICON_SPACING  4
#define HEADER_MASK   (HEADER_BUTTON|HEADER_TRACKING|HEADER_VERTICAL|HEADER_RESIZE)

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXHeaderItem,FXObject,NULL,0)


// Draw item
void FXHeaderItem::draw(const FXHeader* header,FXDC& dc,FXint x,FXint y,FXint w,FXint h){
  register FXint tx,ty,tw,th,ix,iy,iw,ih,s,ml,mr,mt,mb,beg,end,t,xx,yy,bb,aa,ax,ay;
  register FXFont *font=header->getFont();

  // Get border width and padding
  bb=header->getBorderWidth();
  ml=header->getPadLeft()+bb;
  mr=header->getPadRight()+bb;
  mt=header->getPadTop()+bb;
  mb=header->getPadBottom()+bb;

  // Shrink by margins
  x+=ml; w-=ml+mr;
  y+=mt; h-=mt+mb;

  // Initial clip rectangle
  dc.setClipRectangle(x,y,w,h);

  // Text width and height
  tw=th=iw=ih=beg=s=0;
  do{
    end=beg;
    while(end<label.length() && label[end]!='\n') end++;
    if((t=font->getTextWidth(&label[beg],end-beg))>tw) tw=t;
    th+=font->getFontHeight();
    beg=end+1;
    }
  while(end<label.length());

  // Icon size
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }

  // Icon-text spacing
  if(iw && tw) s=ICON_SPACING;

  // Draw arrows
  if(state&(ARROW_UP|ARROW_DOWN)){
    aa=(font->getFontHeight()-5)|1;
    ay=y+(h-aa)/2;
    ax=x+w-aa-2;
    if(state&ARROW_UP){
      dc.setForeground(header->getHiliteColor());
      dc.drawLine(ax+aa/2,ay,ax+aa-1,ay+aa);
      dc.drawLine(ax,ay+aa,ax+aa,ay+aa);
      dc.setForeground(header->getShadowColor());
      dc.drawLine(ax+aa/2,ay,ax,ay+aa);
      }
    else{
      dc.setForeground(header->getHiliteColor());
      dc.drawLine(ax+aa/2,ay+aa,ax+aa-1,ay);
      dc.setForeground(header->getShadowColor());
      dc.drawLine(ax+aa/2,ay+aa,ax,ay);
      dc.drawLine(ax,ay,ax+aa,ay);
      }
    w-=aa+4;
    dc.setClipRectangle(x,y,w,h);
    }

  // Fix x coordinate
  if(state&LEFT){
    if(state&BEFORE){ ix=x; tx=ix+iw+s; }
    else if(state&AFTER){ tx=x; ix=tx+tw+s; }
    else{ ix=x; tx=x; }
    }
  else if(state&RIGHT){
    if(state&BEFORE){ tx=x+w-tw; ix=tx-iw-s; }
    else if(state&AFTER){ ix=x+w-iw; tx=ix-tw-s; }
    else{ ix=x+w-iw; tx=x+w-tw; }
    }
  else{
    if(state&BEFORE){ ix=x+(w-tw-iw-s)/2; tx=ix+iw+s; }
    else if(state&AFTER){ tx=x+(w-tw-iw-s)/2; ix=tx+tw+s; }
    else{ ix=x+(w-iw)/2; tx=x+(w-tw)/2; }
    }

  // Fix y coordinate
  if(state&TOP){
    if(state&ABOVE){ iy=y; ty=iy+ih; }
    else if(state&BELOW){ ty=y; iy=ty+th; }
    else{ iy=y; ty=y; }
    }
  else if(state&BOTTOM){
    if(state&ABOVE){ ty=y+h-th; iy=ty-ih; }
    else if(state&BELOW){ iy=y+h-ih; ty=iy-th; }
    else{ iy=y+h-ih; ty=y+h-th; }
    }
  else{
    if(state&ABOVE){ iy=y+(h-th-ih)/2; ty=iy+ih; }
    else if(state&BELOW){ ty=y+(h-th-ih)/2; iy=ty+th; }
    else{ iy=y+(h-ih)/2; ty=y+(h-th)/2; }
    }

  // Offset a bit when pressed
  if(state&PRESSED){ tx++; ty++; ix++; iy++; }

  // Paint icon
  if(icon){
    dc.drawIcon(icon,ix,iy);
    }

  // Text color
  dc.setForeground(header->getTextColor());

  // Draw text
  yy=ty+font->getFontAscent();
  beg=0;
  do{
    end=beg;
    while(end<label.length() && label[end]!='\n') end++;
    if(state&LEFT) xx=tx;
    else if(state&RIGHT) xx=tx+tw-font->getTextWidth(&label[beg],end-beg);
    else xx=tx+(tw-font->getTextWidth(&label[beg],end-beg))/2;
    dc.drawText(xx,yy,&label[beg],end-beg);
    yy+=font->getFontHeight();
    beg=end+1;
    }
  while(end<label.length());

  // Restore original clip path
  dc.clearClipRectangle();
  }


// Create icon
void FXHeaderItem::create(){ if(icon) icon->create(); }


// No op, we don't own icon
void FXHeaderItem::destroy(){ /*if(icon) icon->destroy();*/ }


// Detach from icon resource
void FXHeaderItem::detach(){ if(icon) icon->detach(); }


// Change sort direction
void FXHeaderItem::setArrowDir(FXbool dir){
  state&=~(ARROW_UP|ARROW_DOWN);
  if(dir==TRUE) state|=ARROW_UP; else if(dir==FALSE) state|=ARROW_DOWN;
  }


// Change sort direction
FXbool FXHeaderItem::getArrowDir() const {
  return (state&ARROW_UP) ? TRUE : (state&ARROW_DOWN) ? FALSE : MAYBE;
  }


// Change justify mode
void FXHeaderItem::setJustify(FXuint justify){
  state=(state&~(RIGHT|LEFT|TOP|BOTTOM)) | (justify&(RIGHT|LEFT|TOP|BOTTOM));
  }

// Change icon positioning
void FXHeaderItem::setIconPosition(FXuint mode){
  state=(state&~(BEFORE|AFTER|ABOVE|BELOW)) | (mode&(BEFORE|AFTER|ABOVE|BELOW));
  }


// Set button state
void FXHeaderItem::setPressed(FXbool pressed){
  if(pressed) state|=PRESSED; else state&=~PRESSED;
  }


// Get width of item
FXint FXHeaderItem::getWidth(const FXHeader* header) const {
  register FXint ml=header->getPadLeft()+header->getBorderWidth();
  register FXint mr=header->getPadRight()+header->getBorderWidth();
  register FXFont *font=header->getFont();
  register FXint beg,end,tw,iw,s,t,w;
  tw=iw=beg=s=0;
  if(icon) iw=icon->getWidth();
  do{
    end=beg;
    while(end<label.length() && label[end]!='\n') end++;
    if((t=font->getTextWidth(&label[beg],end-beg))>tw) tw=t;
    beg=end+1;
    }
  while(end<label.length());
  if(iw && tw) s=4;
  if(state&(BEFORE|AFTER))
    w=iw+tw+s;
  else
    w=FXMAX(iw,tw);
  return ml+mr+w;
  }


// Get height of item
FXint FXHeaderItem::getHeight(const FXHeader* header) const {
  register FXint mt=header->getPadTop()+header->getBorderWidth();
  register FXint mb=header->getPadBottom()+header->getBorderWidth();
  register FXFont *font=header->getFont();
  register FXint beg,end,th,ih,h;
  th=ih=beg=0;
  if(icon) ih=icon->getHeight();
  do{
    end=beg;
    while(end<label.length() && label[end]!='\n') end++;
    th+=font->getFontHeight();
    beg=end+1;
    }
  while(end<label.length());
  if(state&(ABOVE|BELOW))
    h=ih+th;
  else
    h=FXMAX(ih,th);
  return h+mt+mb;
  }


// Change item's text label
void FXHeaderItem::setText(const FXString& txt){
  label=txt;
  }


// Change item's icon
void FXHeaderItem::setIcon(FXIcon* icn){
  icon=icn;
  }


// Save data
void FXHeaderItem::save(FXStream& store) const {
  FXObject::save(store);
  store << label;
  store << icon;
  store << size;
  store << pos;
  store << state;
  }


// Load data
void FXHeaderItem::load(FXStream& store){
  FXObject::load(store);
  store >> label;
  store >> icon;
  store >> size;
  store >> pos;
  store >> state;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXHeader) FXHeaderMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXHeader::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXHeader::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXHeader::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXHeader::onLeftBtnRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXHeader::onUngrabbed),
  FXMAPFUNC(SEL_TIMEOUT,FXHeader::ID_TIPTIMER,FXHeader::onTipTimer),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXHeader::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXHeader::onQueryHelp),
  };


// Object implementation
FXIMPLEMENT(FXHeader,FXFrame,FXHeaderMap,ARRAYNUMBER(FXHeaderMap))


// Make a Header
FXHeader::FXHeader(){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  textColor=0;
  font=NULL;
  pos=0;
  active=-1;
  activepos=0;
  activesize=0;
  offset=0;
  }


// Make a Header
FXHeader::FXHeader(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  target=tgt;
  message=sel;
  textColor=getApp()->getForeColor();
  font=getApp()->getNormalFont();
  pos=0;
  active=-1;
  activepos=0;
  activesize=0;
  offset=0;
  }


// Create window
void FXHeader::create(){
  register FXint i;
  FXFrame::create();
  for(i=0; i<items.no(); i++){items[i]->create();}
  font->create();
  }


// Detach window
void FXHeader::detach(){
  register FXint i;
  FXFrame::detach();
  for(i=0; i<items.no(); i++){items[i]->detach();}
  font->detach();
  }


// Get default width
FXint FXHeader::getDefaultWidth(){
  register FXint i,t,w=0;
  if(options&HEADER_VERTICAL){
    for(i=0; i<items.no(); i++){
      if((t=items[i]->getWidth(this))>w) w=t;
      }
    }
  else{
    for(i=0; i<items.no(); i++){
      w+=items[i]->getSize();
      }
    }
  return w;
  }


// Get default height
FXint FXHeader::getDefaultHeight(){
  register FXint i,t,h=0;
  if(options&HEADER_VERTICAL){
    for(i=0; i<items.no(); i++){
      h+=items[i]->getSize();
      }
    }
  else{
    for(i=0; i<items.no(); i++){
      if((t=items[i]->getHeight(this))>h) h=t;
      }
    }
  return h;
  }



// Return total size
FXint FXHeader::getTotalSize() const {
  return items.no() ? items[items.no()-1]->getPos()+items[items.no()-1]->getSize()-items[0]->getPos() : 0;
  }


// Create custom item
FXHeaderItem *FXHeader::createItem(const FXString& text,FXIcon* icon,FXint size,void* ptr){
  return new FXHeaderItem(text,icon,size,ptr);
  }


// Retrieve item
FXHeaderItem *FXHeader::getItem(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItem: index out of range.\n",getClassName()); }
  return items[index];
  }


// Replace item with another
FXint FXHeader::setItem(FXint index,FXHeaderItem* item,FXbool notify){

  // Must have item
  if(!item){ fxerror("%s::setItem: item is NULL.\n",getClassName()); }

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::setItem: index out of range.\n",getClassName()); }

  // Notify item will be replaced
  if(notify && target){target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)(FXival)index);}

  // Copy the size over
  item->setSize(items[index]->getSize());
  item->setPos(items[index]->getPos());

  // Delete old
  delete items[index];

  // Add new
  items[index]=item;

  // Redo layout
  recalc();
  return index;
  }


// Replace item with another
FXint FXHeader::setItem(FXint index,const FXString& text,FXIcon *icon,FXint size,void* ptr,FXbool notify){
  return setItem(index,createItem(text,icon,FXMAX(size,0),ptr),notify);
  }


// Insert item
FXint FXHeader::insertItem(FXint index,FXHeaderItem* item,FXbool notify){
  register FXint i,d;

  // Must have item
  if(!item){ fxerror("%s::insertItem: item is NULL.\n",getClassName()); }

  // Must be in range
  if(index<0 || items.no()<index){ fxerror("%s::insertItem: index out of range.\n",getClassName()); }

  // New item position
  item->setPos((0<index)?items[index-1]->getPos()+items[index-1]->getSize():0);

  // Move over remaining items
  for(i=index,d=item->getSize(); i<items.no(); i++) items[i]->setPos(items[i]->getPos()+d);

  // Add item to list
  items.insert(index,item);

  // Notify item has been inserted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)(FXival)index);}

  // Redo layout
  recalc();

  return index;
  }


// Insert item
FXint FXHeader::insertItem(FXint index,const FXString& text,FXIcon *icon,FXint size,void* ptr,FXbool notify){
  return insertItem(index,createItem(text,icon,FXMAX(size,0),ptr),notify);
  }


// Append item
FXint FXHeader::appendItem(FXHeaderItem* item,FXbool notify){
  return insertItem(items.no(),item,notify);
  }


// Append item
FXint FXHeader::appendItem(const FXString& text,FXIcon *icon,FXint size,void* ptr,FXbool notify){
  return insertItem(items.no(),createItem(text,icon,FXMAX(size,0),ptr),notify);
  }


// Prepend item
FXint FXHeader::prependItem(FXHeaderItem* item,FXbool notify){
  return insertItem(0,item,notify);
  }

// Prepend item
FXint FXHeader::prependItem(const FXString& text,FXIcon *icon,FXint size,void* ptr,FXbool notify){
  return insertItem(0,createItem(text,icon,FXMAX(size,0),ptr),notify);
  }


// Fill list by appending items from array of strings
FXint FXHeader::fillItems(const FXchar** strings,FXIcon *icon,FXint size,void* ptr,FXbool notify){
  register FXint n=0;
  if(strings){
    while(strings[n]){
      appendItem(strings[n++],icon,size,ptr,notify);
      }
    }
  return n;
  }


// Fill list by appending items from newline separated strings
FXint FXHeader::fillItems(const FXString& strings,FXIcon *icon,FXint size,void* ptr,FXbool notify){
  register FXint n=0;
  FXString text;
  while(!(text=strings.section('\n',n)).empty()){
    appendItem(text,icon,size,ptr,notify);
    n++;
    }
  return n;
  }


// Extract node from list
FXHeaderItem* FXHeader::extractItem(FXint index,FXbool notify){
  register FXHeaderItem *result;
  register FXint i,d;

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::extractItem: index out of range.\n",getClassName()); }

  // Notify item will be deleted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}

  // Adjust remaining columns
  for(i=index+1,d=items[index]->getSize(); i<items.no(); i++) items[i]->setPos(items[i]->getPos()-d);

  // Delete item
  result=items[index];

  // Remove item from list
  items.erase(index);

  // Redo layout
  recalc();

  // Return item
  return result;
  }


// Remove node from list
void FXHeader::removeItem(FXint index,FXbool notify){
  register FXint i,d;

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::removeItem: index out of range.\n",getClassName()); }

  // Notify item will be deleted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}

  // Adjust remaining columns
  for(i=index+1,d=items[index]->getSize(); i<items.no(); i++) items[i]->setPos(items[i]->getPos()-d);

  // Delete item
  delete items[index];

  // Remove item from list
  items.erase(index);

  // Redo layout
  recalc();
  }


// Remove all items
void FXHeader::clearItems(FXbool notify){

  // Delete items
  for(FXint index=items.no()-1; 0<=index; index--){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}
    delete items[index];
    }

  // Free array
  items.clear();

  // Redo layout
  recalc();
  }


// Change item's text
void FXHeader::setItemText(FXint index,const FXString& text){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemText: index out of range.\n",getClassName()); }
  if(items[index]->getText()!=text){
    items[index]->setText(text);
    recalc();
    }
  }


// Get item's text
FXString FXHeader::getItemText(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemText: index out of range.\n",getClassName()); }
  return items[index]->getText();
  }


// Change item's icon
void FXHeader::setItemIcon(FXint index,FXIcon* icon){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemIcon: index out of range.\n",getClassName()); }
  if(items[index]->getIcon()!=icon){
    items[index]->setIcon(icon);
    recalc();
    }
  }


// Get item's icon
FXIcon* FXHeader::getItemIcon(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemIcon: index out of range.\n",getClassName()); }
  return items[index]->getIcon();
  }


// Change item's size
void FXHeader::setItemSize(FXint index,FXint size){
  register FXint i,d;
  if(index<0 || items.no()<=index){ fxerror("%s::setItemSize: index out of range.\n",getClassName()); }
  if(size<0) size=0;
  d=size-items[index]->getSize();
  if(d!=0){
    items[index]->setSize(size);
    for(i=index+1; i<items.no(); i++) items[i]->setPos(items[i]->getPos()+d);
    recalc();
    }
  }


// Get item's size
FXint FXHeader::getItemSize(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemSize: index out of range.\n",getClassName()); }
  return items[index]->getSize();
  }


// Get item's offset
FXint FXHeader::getItemOffset(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemOffset: index out of range.\n",getClassName()); }
  return pos+items[index]->getPos();
  }


// Get index of item at offset
FXint FXHeader::getItemAt(FXint coord) const {
  register FXint h=items.no()-1,l=0,m;
  coord=coord-pos;
  if(l<=h){
    if(coord<items[l]->getPos()) return -1;
    if(coord>=items[h]->getPos()+items[h]->getSize()) return items.no();
    do{
      m=(h+l)>>1;
      if(coord<items[m]->getPos()) h=m-1;
      else if(coord>=items[m]->getPos()+items[m]->getSize()) l=m+1;
      else break;
      }
    while(h>=l);
    return m;
    }
  return coord<0 ? -1 : 0;
  }


// Set item data
void FXHeader::setItemData(FXint index,void* ptr){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemData: index out of range.\n",getClassName()); }
  items[index]->setData(ptr);
  }


// Get item data
void* FXHeader::getItemData(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemData: index out of range.\n",getClassName()); }
  return items[index]->getData();
  }


// Change sort direction
void FXHeader::setArrowDir(FXint index,FXbool dir){
  if(index<0 || items.no()<=index){ fxerror("%s::setArrowDir: index out of range.\n",getClassName()); }
  if(items[index]->getArrowDir()!=dir){
    items[index]->setArrowDir(dir);
    updateItem(index);
    }
  }


// Return sort direction
FXbool FXHeader::getArrowDir(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getArrowDir: index out of range.\n",getClassName()); }
  return items[index]->getArrowDir();
  }


// Change item justification
void FXHeader::setItemJustify(FXint index,FXuint justify){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemJustify: index out of range.\n",getClassName()); }
  if(items[index]->getJustify()!=justify){
    items[index]->setJustify(justify);
    updateItem(index);
    }
  }


// Return item justification
FXuint FXHeader::getItemJustify(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemJustify: index out of range.\n",getClassName()); }
  return items[index]->getJustify();
  }


// Change relative position of icon and text of item
void FXHeader::setItemIconPosition(FXint index,FXuint mode){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemIconPosition: index out of range.\n",getClassName()); }
  if(items[index]->getIconPosition()!=mode){
    items[index]->setIconPosition(mode);
    recalc();
    }
  }


// Return relative icon and text position
FXuint FXHeader::getItemIconPosition(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemIconPosition: index out of range.\n",getClassName()); }
  return items[index]->getIconPosition();
  }


// Changed button item's pressed state
void FXHeader::setItemPressed(FXint index,FXbool pressed){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemPressed: index out of range.\n",getClassName()); }
  if(pressed!=items[index]->isPressed()){
    items[index]->setPressed(pressed);
    updateItem(index);
    }
  }


// Return TRUE if button item is pressed in
FXbool FXHeader::isItemPressed(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemPressed: index out of range.\n",getClassName()); }
  return items[index]->isPressed();
  }


// Scroll to make given item visible
void FXHeader::makeItemVisible(FXint index){
  register FXint newpos,ioffset,isize,space;
  if(xid){
    newpos=pos;
    if(0<=index && index<items.no()){
      space=(options&HEADER_VERTICAL)?height:width;
      ioffset=items[index]->getPos();
      isize=items[index]->getSize();
      newpos=pos;
      if(newpos+ioffset+isize>=space) newpos=space-ioffset-isize;
      if(newpos+ioffset<=0) newpos=-ioffset;
      }
    setPosition(newpos);
    }
  }


// Repaint cell at index
void FXHeader::updateItem(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::updateItem: index out of range.\n",getClassName()); }
  if(options&HEADER_VERTICAL)
    update(0,pos+items[index]->getPos(),width,items[index]->getSize());
  else
    update(pos+items[index]->getPos(),0,items[index]->getSize(),height);
  }


// Do layout
void FXHeader::layout(){

  // Force repaint
  update();

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Handle repaint
long FXHeader::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  register FXint x,y,w,h,i,ilo,ihi;

  // Set font
  dc.setFont(font);

  // Paint background
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);

  // Vertical
  if(options&HEADER_VERTICAL){

    // Determine affected items
    ilo=getItemAt(ev->rect.y);
    ihi=getItemAt(ev->rect.y+ev->rect.h);

    // Fragment below first item
    if(ilo<0){
      y=pos;
      if(0<items.no()){
        y=pos+items[0]->getPos();
        }
      if(0<y){
        if(options&FRAME_THICK)
          drawDoubleRaisedRectangle(dc,0,0,width,y);
        else if(options&FRAME_RAISED)
          drawRaisedRectangle(dc,0,0,width,y);
        }
      ilo=0;
      }

    // Fragment above last item
    if(ihi>=items.no()){
      y=pos;
      if(0<items.no()){
        y=pos+items[items.no()-1]->getPos()+items[items.no()-1]->getSize();
        }
      if(y<height){
        if(options&FRAME_THICK)
          drawDoubleRaisedRectangle(dc,0,y,width,height-y);
        else if(options&FRAME_RAISED)
          drawRaisedRectangle(dc,0,y,width,height-y);
        }
      ihi=items.no()-1;
      }

    // Draw only affected items
    for(i=ilo; i<=ihi; i++){
      y=pos+items[i]->getPos();
      h=items[i]->getSize();
      if(items[i]->isPressed()){
        if(options&FRAME_THICK)
          drawDoubleSunkenRectangle(dc,0,y,width,h);
        else if(options&FRAME_RAISED)
          drawSunkenRectangle(dc,0,y,width,h);
        }
      else{
        if(options&FRAME_THICK)
          drawDoubleRaisedRectangle(dc,0,y,width,h);
        else if(options&FRAME_RAISED)
          drawRaisedRectangle(dc,0,y,width,h);
        }
      items[i]->draw(this,dc,0,y,width,h);
      }
    }

  // Horizontal
  else{

    // Determine affected items
    ilo=getItemAt(ev->rect.x);
    ihi=getItemAt(ev->rect.x+ev->rect.w);

    // Fragment below first item
    if(ilo<0){
      x=pos;
      if(0<items.no()){
        x=pos+items[0]->getPos();
        }
      if(0<x){
        if(options&FRAME_THICK)
          drawDoubleRaisedRectangle(dc,0,0,x,height);
        else if(options&FRAME_RAISED)
          drawRaisedRectangle(dc,0,0,x,height);
        }
      ilo=0;
      }

    // Fragment above last item
    if(ihi>=items.no()){
      x=pos;
      if(0<items.no()){
        x=pos+items[items.no()-1]->getPos()+items[items.no()-1]->getSize();
        }
      if(x<width){
        if(options&FRAME_THICK)
          drawDoubleRaisedRectangle(dc,x,0,width-x,height);
        else if(options&FRAME_RAISED)
          drawRaisedRectangle(dc,x,0,width-x,height);
        }
      ihi=items.no()-1;
      }

    // Draw only the affected items
    for(i=ilo; i<=ihi; i++){
      x=pos+items[i]->getPos();
      w=items[i]->getSize();
      if(items[i]->isPressed()){
        if(options&FRAME_THICK)
          drawDoubleSunkenRectangle(dc,x,0,w,height);
        else if(options&FRAME_RAISED)
          drawSunkenRectangle(dc,x,0,w,height);
        }
      else{
        if(options&FRAME_THICK)
          drawDoubleRaisedRectangle(dc,x,0,w,height);
        else if(options&FRAME_RAISED)
          drawRaisedRectangle(dc,x,0,w,height);
        }
      items[i]->draw(this,dc,x,0,w,height);
      }
    }
  return 1;
  }


// We were asked about tip text
long FXHeader::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if(flags&FLAG_TIP){
    FXint index,cx,cy; FXuint btns;
    getCursorPosition(cx,cy,btns);
    index=getItemAt((options&HEADER_VERTICAL)?cy:cx);
    if(0<=index && index<items.no()){
      FXString string=items[index]->getText();
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
      return 1;
      }
    }
  return 0;
  }


// We were asked about status text
long FXHeader::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// We timed out, i.e. the user didn't move for a while
long FXHeader::onTipTimer(FXObject*,FXSelector,void*){
  FXTRACE((250,"%s::onTipTimer %p\n",getClassName(),this));
  flags|=FLAG_TIP;
  return 1;
  }


// Button being pressed
long FXHeader::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint coord;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();

    // First change callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;

    // Where clicked
    coord=(options&HEADER_VERTICAL)?event->win_y:event->win_x;
    active=getItemAt(coord);
    if(0<=active && active<items.no()){
      if((options&HEADER_RESIZE) && (active<items.no()) && (pos+items[active]->getPos()+items[active]->getSize()-FUDGE<coord)){
        activepos=pos+items[active]->getPos();
        activesize=items[active]->getSize();
        offset=coord-activepos-activesize;
        setDragCursor((options&HEADER_VERTICAL)?getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR):getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR));
        flags|=FLAG_PRESSED|FLAG_TRYDRAG;
        }
      else if((options&HEADER_RESIZE) && (0<active) && (coord<pos+items[active-1]->getPos()+items[active-1]->getSize()+FUDGE)){
        active--;
        activepos=pos+items[active]->getPos();
        activesize=items[active]->getSize();
        offset=coord-activepos-activesize;
        setDragCursor((options&HEADER_VERTICAL)?getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR):getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR));
        flags|=FLAG_PRESSED|FLAG_TRYDRAG;
        }
      else if((options&HEADER_BUTTON) && (active<items.no())){
        activepos=pos+items[active]->getPos();
        activesize=items[active]->getSize();
        setItemPressed(active,TRUE);
        flags|=FLAG_PRESSED;
        }
      }
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Button being released
long FXHeader::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint flg=flags;
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~(FLAG_PRESSED|FLAG_DODRAG|FLAG_TRYDRAG);

    // First chance callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;

    // Set cursor back
    setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));

    // Clicked on split
    if(flg&FLAG_TRYDRAG){
      if(target) target->tryHandle(this,FXSEL(SEL_CLICKED,message),(void*)(FXival)active);
      return 1;
      }

    // Dragged split
    if(flg&FLAG_DODRAG){
      if(!(options&HEADER_TRACKING)){
        drawSplit(activepos+activesize);
        setItemSize(active,activesize);
        if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)active);
        }
      return 1;
      }

    // Pressed button
    if(flg&FLAG_PRESSED){
      if(items[active]->isPressed()){
        setItemPressed(active,FALSE);
        if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)active);
        }
      return 1;
      }
    }
  return 0;
  }


// Button being moved
long FXHeader::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint oldsplit,newsplit,index;
  FXuint flg=flags;

  // Kill the tip
  flags&=~FLAG_TIP;

  // Kill the tip timer
  getApp()->removeTimeout(this,ID_TIPTIMER);

  // Tentatively dragging split
  if(flags&FLAG_TRYDRAG){
    if(!(options&HEADER_TRACKING)) drawSplit(activepos+activesize);
    flags&=~FLAG_TRYDRAG;
    flags|=FLAG_DODRAG;
    return 1;
    }

  // Dragging split
  if(flags&FLAG_DODRAG){
    oldsplit=activepos+activesize;
    if(options&HEADER_VERTICAL)
      activesize=event->win_y-offset-activepos;
    else
      activesize=event->win_x-offset-activepos;
    if(activesize<0) activesize=0;
    newsplit=activepos+activesize;
    if(newsplit!=oldsplit){
      if(!(options&HEADER_TRACKING)){
        drawSplit(oldsplit);
        drawSplit(newsplit);
        }
      else{
        setItemSize(active,activesize);
        if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)active);
        }
      }
    return 1;
    }

  // Had the button pressed
  if(flags&FLAG_PRESSED){
    if(options&HEADER_VERTICAL){
      if(activepos<=event->win_y && event->win_y<activepos+activesize && 0<=event->win_x && event->win_x<width){
        setItemPressed(active,TRUE);
        }
      else{
        setItemPressed(active,FALSE);
        }
      }
    else{
      if(activepos<=event->win_x && event->win_x<activepos+activesize && 0<=event->win_y && event->win_y<height){
        setItemPressed(active,TRUE);
        }
      else{
        setItemPressed(active,FALSE);
        }
      }
    return 1;
    }

  // When hovering over a split, show the split cursor
  if(options&HEADER_RESIZE){
    if(options&HEADER_VERTICAL){
      index=getItemAt(event->win_y-FUDGE);
      if(0<=index && index<items.no() && pos+items[index]->getPos()+items[index]->getSize()-FUDGE<event->win_y){
        setDefaultCursor(getApp()->getDefaultCursor(DEF_VSPLIT_CURSOR));
        }
      else{
        setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
        }
      }
    else{
      index=getItemAt(event->win_x-FUDGE);
      if(0<=index && index<items.no() && pos+items[index]->getPos()+items[index]->getSize()-FUDGE<event->win_x){
        setDefaultCursor(getApp()->getDefaultCursor(DEF_HSPLIT_CURSOR));
        }
      else{
        setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
        }
      }
    }

  // Reset tip timer if nothing's going on
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());

  // Force GUI update only when needed
  return (flg&FLAG_TIP);
  }


// The widget lost the grab for some reason
long FXHeader::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  flags&=~FLAG_PRESSED;
  flags&=~FLAG_CHANGED;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Draw the split
void FXHeader::drawSplit(FXint p){
  FXDCWindow dc(getParent());
  FXint px,py;
  translateCoordinatesTo(px,py,getParent(),p,p);
  dc.clipChildren(FALSE);
  dc.setFunction(BLT_NOT_DST);
  if(options&HEADER_VERTICAL){
    dc.fillRectangle(0,py,getParent()->getWidth(),2);
    }
  else{
    dc.fillRectangle(px,0,2,getParent()->getHeight());
    }
  }


// Set position, scrolling contents
void FXHeader::setPosition(FXint p){
  if(pos!=p){
    if(options&HEADER_VERTICAL)
      scroll(0,0,width,height,0,p-pos);
    else
      scroll(0,0,width,height,p-pos,0);
    pos=p;
    }
  }


// Change the font
void FXHeader::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set text color
void FXHeader::setTextColor(FXColor clr){
  if(textColor!=clr){
    textColor=clr;
    update();
    }
  }


// Header style change
void FXHeader::setHeaderStyle(FXuint style){
  FXuint opts=(options&~HEADER_MASK) | (style&HEADER_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get header style
FXuint FXHeader::getHeaderStyle() const {
  return (options&HEADER_MASK);
  }


// Change help text
void FXHeader::setHelpText(const FXString& text){
  help=text;
  }


// Save object to stream
void FXHeader::save(FXStream& store) const {
  FXFrame::save(store);
  items.save(store);
  store << textColor;
  store << font;
  store << help;
  store << pos;
  }



// Load object from stream
void FXHeader::load(FXStream& store){
  FXFrame::load(store);
  items.load(store);
  store >> textColor;
  store >> font;
  store >> help;
  store >> pos;
  }


// Clean up
FXHeader::~FXHeader(){
  getApp()->removeTimeout(this,ID_TIPTIMER);
  clearItems();
  font=(FXFont*)-1L;
  }
}
