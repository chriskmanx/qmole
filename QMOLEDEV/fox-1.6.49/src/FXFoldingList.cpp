/********************************************************************************
*                                                                               *
*                    F o l d i n g   L i s t   W i d g e t                      *
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
* $Id: FXFoldingList.cpp,v 1.66 2006/02/17 03:06:59 fox Exp $                   *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "fxascii.h"
#include "fxunicode.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXObjectList.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXButton.h"
#include "FXScrollBar.h"
#include "FXScrollArea.h"
#include "FXHeader.h"
#include "FXFoldingList.h"


/*
  Notes:
  - Tooltip should pop up exactly on top of current item.
  - Clicking on + does not make it current.
  - Need translate right-clicks into message with item figured out...
  - In autoselect mode, all items are expanded.
  - Sortfunc's will be hard to serialize.
  - As with FXIconList, it probably shouldn't autoscroll when draggin icons.
  - Maybe moving (dragging) items around in the treelist is something that should
    be supported?
  - Click outside of list perhaps should also change current item?
  - Since '\0' is no longer special in FXString, perhaps we can replace the function
    of '\t' with '\0'.  This would be significantly more efficient.
*/


#define ICON_SPACING        4   // Spacing between parent and child in x direction
#define TEXT_SPACING        4   // Spacing between icon and text
#define SIDE_SPACING        4   // Spacing between side and item
#define DEFAULT_INDENT      8   // Indent between parent and child
#define HALFBOX_SIZE        4   // Half box size
#define BOX_FUDGE           3   // Fudge border around box


#define SELECT_MASK         (FOLDINGLIST_SINGLESELECT|FOLDINGLIST_BROWSESELECT)
#define FOLDINGLIST_MASK    (SELECT_MASK|FOLDINGLIST_AUTOSELECT|FOLDINGLIST_SHOWS_LINES|FOLDINGLIST_SHOWS_BOXES|FOLDINGLIST_ROOT_BOXES)


using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXFoldingItem,FXObject,NULL,0)


// Draw item
void FXFoldingItem::draw(const FXFoldingList* list,FXDC& dc,FXint x,FXint y,FXint,FXint h) const {
  register FXHeader *header=list->getHeader();
  register FXIcon *icon=(state&OPENED)?openIcon:closedIcon;
  register FXFont *font=list->getFont();
  register FXint th=0,tw=0,ih=0,iw=0,yt,xb,beg,end,hi,drw,space,used,dw,xx;
  if(header->getNumItems()==0) return;
  xx=x+SIDE_SPACING/2;
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    dc.setClipRectangle(header->getItemOffset(0),y,header->getItemSize(0),h);
    dc.drawIcon(icon,xx,y+(h-ih)/2);
    dc.clearClipRectangle();
    xx+=ICON_SPACING+iw;
    }
  if(!label.empty()){
    th=font->getFontHeight();
    dw=font->getTextWidth("...",3);
    xb=header->getItemOffset(0)+header->getItemSize(0);
    if(xb>xx) xb=xx;
    yt=y+(h-th-4)/2;
    if(isSelected()){
      dc.setForeground(list->getSelBackColor());
      dc.fillRectangle(xb,y,header->getTotalSize()-xb,h);
      }
    if(hasFocus()){
      dc.drawFocusRectangle(xb+1,y+1,header->getTotalSize()-xb-2,h-2);
      }
    if(!isEnabled())
      dc.setForeground(makeShadowColor(list->getBackColor()));
    else if(isSelected())
      dc.setForeground(list->getSelTextColor());
    else
      dc.setForeground(list->getTextColor());
    used=xx-header->getItemOffset(0);
    for(hi=beg=0; beg<label.length() && hi<header->getNumItems(); hi++,beg=end+1){
      space=header->getItemSize(hi)-used;
      for(end=beg; end<label.length() && label[end]!='\t'; end++);
      if(end>beg){
        drw=end-beg;
        tw=font->getTextWidth(&label[beg],drw);
        if(tw>space-4){
          while((tw=font->getTextWidth(&label[beg],drw))+dw>space-4 && drw>1) drw=label.dec(drw);
          dc.setClipRectangle(xx,y,space,h);
          dc.drawText(xx+2,yt+font->getFontAscent()+2,&label[beg],drw);
          dc.drawText(xx+tw+2,yt+font->getFontAscent()+2,"...",3);
          dc.clearClipRectangle();
          }
        else{
          dc.drawText(xx+2,yt+font->getFontAscent()+2,&label[beg],drw);
          }
        }
      xx+=space;
      used=0;
      }
    }
  }


// See if item got hit, and where:- 1 is icon, 2 is text
FXint FXFoldingItem::hitItem(const FXFoldingList* list,FXint x,FXint y) const {
  register FXint oiw=0,ciw=0,oih=0,cih=0,tw=0,th=0,iw,ih,ix,iy,tx,ty,h;
  register FXFont *font=list->getFont();
  if(openIcon){
    oiw=openIcon->getWidth();
    oih=openIcon->getHeight();
    }
  if(closedIcon){
    ciw=closedIcon->getWidth();
    cih=closedIcon->getHeight();
    }
  if(!label.empty()){
    if(!list->getHeader()->getNumItems())
      tw=4+font->getTextWidth(label.text(),label.length());
    else
      tw=4+list->getHeader()->getDefaultWidth();
    th=4+font->getFontHeight();
    }
  iw=FXMAX(oiw,ciw);
  ih=FXMAX(oih,cih);
  h=FXMAX(th,ih);
  ix=SIDE_SPACING/2;
  tx=SIDE_SPACING/2;
  if(iw) tx+=iw+ICON_SPACING;
  iy=(h-ih)/2;
  ty=(h-th)/2;

  // In icon?
  if(ix<=x && iy<=y && x<ix+iw && y<iy+ih) return 1;

  // In text?
  if(tx<=x && ty<=y && x<tx+tw && y<ty+th) return 2;

  // Outside
  return 0;
  }


// Set or kill focus
void FXFoldingItem::setFocus(FXbool focus){
  if(focus) state|=FOCUS; else state&=~FOCUS;
  }

// Select or deselect item
void FXFoldingItem::setSelected(FXbool selected){
  if(selected) state|=SELECTED; else state&=~SELECTED;
  }

// Set item opened
void FXFoldingItem::setOpened(FXbool opened){
  if(opened) state|=OPENED; else state&=~OPENED;
  }

// Set item expanded
void FXFoldingItem::setExpanded(FXbool expanded){
  if(expanded) state|=EXPANDED; else state&=~EXPANDED;
  }

// Enable or disable the item
void FXFoldingItem::setEnabled(FXbool enabled){
  if(enabled) state&=~DISABLED; else state|=DISABLED;
  }

// Icon is draggable
void FXFoldingItem::setDraggable(FXbool draggable){
  if(draggable) state|=DRAGGABLE; else state&=~DRAGGABLE;
  }


// Change item label
void FXFoldingItem::setText(const FXString& txt){
  label=txt;
  }


// Change item's open icon
void FXFoldingItem::setOpenIcon(FXIcon* icn,FXbool owned){
  if(openIcon && (state&OPENICONOWNED)){
    if(openIcon!=icn) delete openIcon;
    state&=~OPENICONOWNED;
    }
  openIcon=icn;
  if(openIcon && owned){
    state|=OPENICONOWNED;
    }
  }


// Change item's mini icon
void FXFoldingItem::setClosedIcon(FXIcon* icn,FXbool owned){
  if(closedIcon && (state&CLOSEDICONOWNED)){
    if(closedIcon!=icn) delete closedIcon;
    state&=~CLOSEDICONOWNED;
    }
  closedIcon=icn;
  if(closedIcon && owned){
    state|=CLOSEDICONOWNED;
    }
  }


// Change has items flag
void FXFoldingItem::setHasItems(FXbool flag){
  if(flag) state|=HASITEMS; else state&=~HASITEMS;
  }


// Create icon
void FXFoldingItem::create(){
  if(openIcon) openIcon->create();
  if(closedIcon) closedIcon->create();
  }


// Destroy icon
void FXFoldingItem::destroy(){
  if((state&OPENICONOWNED) && openIcon) openIcon->destroy();
  if((state&CLOSEDICONOWNED) && closedIcon) closedIcon->destroy();
  }


// Detach from icon resource
void FXFoldingItem::detach(){
  if(openIcon) openIcon->detach();
  if(closedIcon) closedIcon->detach();
  }


// Get number of child items
FXint FXFoldingItem::getNumChildren() const {
  register FXFoldingItem *item=first;
  register FXint n=0;
  while(item){item=item->next;n++;}
  return n;
  }



// Get item (logically) below this one
FXFoldingItem* FXFoldingItem::getBelow() const {
  register FXFoldingItem* item=(FXFoldingItem*)this;
  if(first) return first;
  while(!item->next && item->parent) item=item->parent;
  return item->next;
  }


// Get item (logically) above this one
FXFoldingItem* FXFoldingItem::getAbove() const {
  register FXFoldingItem* item=prev;
  if(!item) return parent;
  while(item->last) item=item->last;
  return item;
  }


// Return true if child of parent item
FXbool FXFoldingItem::isChildOf(const FXFoldingItem* item) const {
  register const FXFoldingItem* child=this;
  while(child){ child=child->parent; if(child==item) return TRUE; }
  return FALSE;
  }


// Return true if parent of child item
FXbool FXFoldingItem::isParentOf(const FXFoldingItem* item) const {
  register const FXFoldingItem* child=item;
  while(child){ child=child->parent; if(child==this) return TRUE; }
  return FALSE;
  }


// Get item width
FXint FXFoldingItem::getWidth(const FXFoldingList*) const {
  return SIDE_SPACING;
  }


// Get item height
FXint FXFoldingItem::getHeight(const FXFoldingList* list) const {
  register FXint th=0,oih=0,cih=0;
  if(openIcon) oih=openIcon->getHeight();
  if(closedIcon) cih=closedIcon->getHeight();
  if(!label.empty()) th=4+list->getFont()->getFontHeight();
  return FXMAX3(th,oih,cih);
  }


// Save data
void FXFoldingItem::save(FXStream& store) const {
  FXObject::save(store);
  store << prev;
  store << next;
  store << parent;
  store << first;
  store << last;
  store << label;
  store << openIcon;
  store << closedIcon;
  store << state;
  }


// Load data
void FXFoldingItem::load(FXStream& store){
  FXObject::load(store);
  store >> prev;
  store >> next;
  store >> parent;
  store >> first;
  store >> last;
  store >> label;
  store >> openIcon;
  store >> closedIcon;
  store >> state;
  }


// Delete icons if owned
FXFoldingItem::~FXFoldingItem(){
  if(state&OPENICONOWNED) delete openIcon;
  if(state&CLOSEDICONOWNED) delete closedIcon;
  parent=(FXFoldingItem*)-1L;
  prev=(FXFoldingItem*)-1L;
  next=(FXFoldingItem*)-1L;
  first=(FXFoldingItem*)-1L;
  last=(FXFoldingItem*)-1L;
  openIcon=(FXIcon*)-1L;
  closedIcon=(FXIcon*)-1L;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXFoldingList) FXFoldingListMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXFoldingList::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXFoldingList::onMotion),
  FXMAPFUNC(SEL_TIMEOUT,FXWindow::ID_AUTOSCROLL,FXFoldingList::onAutoScroll),
  FXMAPFUNC(SEL_TIMEOUT,FXFoldingList::ID_TIPTIMER,FXFoldingList::onTipTimer),
  FXMAPFUNC(SEL_TIMEOUT,FXFoldingList::ID_LOOKUPTIMER,FXFoldingList::onLookupTimer),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXFoldingList::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXFoldingList::onLeftBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXFoldingList::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXFoldingList::onRightBtnRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXFoldingList::onUngrabbed),
  FXMAPFUNC(SEL_KEYPRESS,0,FXFoldingList::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXFoldingList::onKeyRelease),
  FXMAPFUNC(SEL_ENTER,0,FXFoldingList::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXFoldingList::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXFoldingList::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXFoldingList::onFocusOut),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXFoldingList::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXFoldingList::onQueryHelp),
  FXMAPFUNC(SEL_CLICKED,0,FXFoldingList::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXFoldingList::onDoubleClicked),
  FXMAPFUNC(SEL_TRIPLECLICKED,0,FXFoldingList::onTripleClicked),
  FXMAPFUNC(SEL_COMMAND,0,FXFoldingList::onCommand),
  FXMAPFUNC(SEL_CHANGED,FXFoldingList::ID_HEADER_CHANGE,FXFoldingList::onHeaderChanged),
  };


// Object implementation
FXIMPLEMENT(FXFoldingList,FXScrollArea,FXFoldingListMap,ARRAYNUMBER(FXFoldingListMap))


/*******************************************************************************/


// Tree List
FXFoldingList::FXFoldingList(){
  flags|=FLAG_ENABLED;
  header=(FXHeader*)-1L;
  firstitem=NULL;
  lastitem=NULL;
  anchoritem=NULL;
  currentitem=NULL;
  extentitem=NULL;
  cursoritem=NULL;
  viewableitem=NULL;
  font=(FXFont*)-1L;
  sortfunc=NULL;
  textColor=0;
  selbackColor=0;
  seltextColor=0;
  lineColor=0;
  treeWidth=0;
  treeHeight=0;
  visible=0;
  indent=DEFAULT_INDENT;
  grabx=0;
  graby=0;
  state=FALSE;
  }


// Tree List
FXFoldingList::FXFoldingList(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
  header=new FXHeader(this,this,FXFoldingList::ID_HEADER_CHANGE,HEADER_TRACKING|HEADER_BUTTON|HEADER_RESIZE|FRAME_RAISED|FRAME_THICK);
  target=tgt;
  message=sel;
  firstitem=NULL;
  lastitem=NULL;
  anchoritem=NULL;
  currentitem=NULL;
  extentitem=NULL;
  cursoritem=NULL;
  viewableitem=NULL;
  font=getApp()->getNormalFont();
  sortfunc=NULL;
  textColor=getApp()->getForeColor();
  selbackColor=getApp()->getSelbackColor();
  seltextColor=getApp()->getSelforeColor();
  lineColor=getApp()->getShadowColor();
  treeWidth=0;
  treeHeight=0;
  visible=0;
  indent=DEFAULT_INDENT;
  grabx=0;
  graby=0;
  state=FALSE;
  }


// Create window
void FXFoldingList::create(){
  register FXFoldingItem *item=firstitem;
  FXScrollArea::create();
  while(item){
    item->create();
    if(item->first){item=item->first;continue;}
    while(!item->next && item->parent){item=item->parent;}
    item=item->next;
    }
  font->create();
  }


// Detach window
void FXFoldingList::detach(){
  register FXFoldingItem *item=firstitem;
  FXScrollArea::detach();
  while(item){
    item->detach();
    if(item->first){item=item->first;continue;}
    while(!item->next && item->parent){item=item->parent;}
    item=item->next;
    }
  font->detach();
  }


// Can have focus
bool FXFoldingList::canFocus() const { return true; }


// Into focus chain
void FXFoldingList::setFocus(){
  FXScrollArea::setFocus();
  setDefault(TRUE);
  }


// Out of focus chain
void FXFoldingList::killFocus(){
  FXScrollArea::killFocus();
  setDefault(MAYBE);
  }


// Get default width
FXint FXFoldingList::getDefaultWidth(){
  return FXScrollArea::getDefaultWidth();
  }


// Get default height
FXint FXFoldingList::getDefaultHeight(){
  if(visible) return visible*(4+font->getFontHeight())+header->getDefaultHeight();
  return FXScrollArea::getDefaultHeight();
  }


// Propagate size change
void FXFoldingList::recalc(){
  FXScrollArea::recalc();
  flags|=FLAG_RECALC;
  cursoritem=NULL;
  }

// Move content
void FXFoldingList::moveContents(FXint x,FXint y){
  FXint dx=x-pos_x;
  FXint dy=y-pos_y;
  pos_x=x;
  pos_y=y;
  header->setPosition(x);
  scroll(0,header->getHeight(),viewport_w,viewport_h,dx,dy);
  }


// List is multiple of nitems
void FXFoldingList::setNumVisible(FXint nvis){
  if(nvis<0) nvis=0;
  if(visible!=nvis){
    visible=nvis;
    recalc();
    }
  }


// Get number of toplevel items
FXint FXFoldingList::getNumItems() const {
  register FXFoldingItem *item=firstitem;
  register FXint n=0;
  while(item){
    item=item->next;
    n++;
    }
  return n;
  }


// Recompute interior
void FXFoldingList::recompute(){
  register FXFoldingItem* item;
  register FXint x,y,h;
  x=y=0;
  treeWidth=0;
  treeHeight=0;
  item=firstitem;
  if(options&FOLDINGLIST_ROOT_BOXES) x+=(4+indent);
  while(item){
    item->x=x;
    item->y=y;
    h=item->getHeight(this);
    y+=h;
    if(item->first && ((options&FOLDINGLIST_AUTOSELECT) || item->isExpanded())){
      x+=(indent+h/2);
      item=item->first;
      continue;
      }
    while(!item->next && item->parent){
      item=item->parent;
      x-=(indent+item->getHeight(this)/2);
      }
    item=item->next;
    }
  treeWidth=header->getDefaultWidth();
  treeHeight=header->getDefaultHeight()+y;
  flags&=~FLAG_RECALC;
  }


// Determine content width of tree list
FXint FXFoldingList::getContentWidth(){
  if(flags&FLAG_RECALC) recompute();
  return treeWidth;
  }


// Determine content height of tree list
FXint FXFoldingList::getContentHeight(){
  if(flags&FLAG_RECALC) recompute();
  return treeHeight;
  }


// Recalculate layout
void FXFoldingList::layout(){

  // Calculate contents
  FXScrollArea::layout();

  // Place header control
  header->position(0,0,viewport_w,header->getDefaultHeight());

  // Set line size based on item size
  if(firstitem){
    vertical->setLine(firstitem->getHeight(this));
    horizontal->setLine(firstitem->getWidth(this)/10);
    }

  // We were supposed to make this item viewable
  if(viewableitem){
    makeItemVisible(viewableitem);
    }

  // Force repaint
  update();

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Header changed but content size didn't
long FXFoldingList::onHeaderChanged(FXObject*,FXSelector,void*){
//  flags&=~FLAG_RECALC;
  return 1;
  }


// Set headers from array of strings
void FXFoldingList::setHeaders(const FXchar** strings,FXint size){
  header->clearItems();
  header->fillItems(strings,NULL,size);
  }


// Set headers from newline separated strings
void FXFoldingList::setHeaders(const FXString& strings,FXint size){
  header->clearItems();
  header->fillItems(strings,NULL,size);
  }

// Append header caption
void FXFoldingList::appendHeader(const FXString& text,FXIcon *icon,FXint size){
  header->appendItem(text,icon,size);
  }


// Remove header caption
void FXFoldingList::removeHeader(FXint index){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::removeHeader: index out of range.\n",getClassName()); }
  header->removeItem(index);
  }


// Change header caption
void FXFoldingList::setHeaderText(FXint index,const FXString& text){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::setHeaderText: index out of range.\n",getClassName()); }
  header->setItemText(index,text);
  }


// Get header caption
FXString FXFoldingList::getHeaderText(FXint index) const {
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::getHeaderText: index out of range.\n",getClassName()); }
  return header->getItemText(index);
  }


// Change header icon
void FXFoldingList::setHeaderIcon(FXint index,FXIcon *icon){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::setHeaderIcon: index out of range.\n",getClassName()); }
  header->setItemIcon(index,icon);
  }


// Get header icon
FXIcon* FXFoldingList::getHeaderIcon(FXint index) const {
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::getHeaderIcon: index out of range.\n",getClassName()); }
  return header->getItemIcon(index);
  }


// Change header size
void FXFoldingList::setHeaderSize(FXint index,FXint size){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::setHeaderSize: index out of range.\n",getClassName()); }
  header->setItemSize(index,size);
  }


// Get header size
FXint FXFoldingList::getHeaderSize(FXint index) const {
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::getHeaderSize: index out of range.\n",getClassName()); }
  return header->getItemSize(index);
  }


// Return number of headers
FXint FXFoldingList::getNumHeaders() const {
  return header->getNumItems();
  }


// Set item text
void FXFoldingList::setItemText(FXFoldingItem* item,const FXString& text){
  if(item==NULL){ fxerror("%s::setItemText: item is NULL.\n",getClassName()); }
  if(item->getText()!=text){
    item->setText(text);
    recalc();
    }
  }


// Get item text
FXString FXFoldingList::getItemText(const FXFoldingItem* item) const {
  if(item==NULL){ fxerror("%s::getItemText: item is NULL.\n",getClassName()); }
  return item->getText();
  }


// Set item open icon
void FXFoldingList::setItemOpenIcon(FXFoldingItem* item,FXIcon* icon,FXbool owned){
  if(item==NULL){ fxerror("%s::setItemOpenIcon: item is NULL.\n",getClassName()); }
  if(item->getOpenIcon()!=icon) recalc();
  item->setOpenIcon(icon,owned);
  }


// Get item open icon
FXIcon* FXFoldingList::getItemOpenIcon(const FXFoldingItem* item) const {
  if(item==NULL){ fxerror("%s::getItemOpenIcon: item is NULL.\n",getClassName()); }
  return item->getOpenIcon();
  }


// Set item closed icon
void FXFoldingList::setItemClosedIcon(FXFoldingItem* item,FXIcon* icon,FXbool owned){
  if(item==NULL){ fxerror("%s::setItemClosedIcon: item is NULL.\n",getClassName()); }
  if(item->getClosedIcon()!=icon) recalc();
  item->setClosedIcon(icon,owned);
  }


// Get item closed icon
FXIcon* FXFoldingList::getItemClosedIcon(const FXFoldingItem* item) const {
  if(item==NULL){ fxerror("%s::getItemClosedIcon: item is NULL.\n",getClassName()); }
  return item->getClosedIcon();
  }


// Set item data
void FXFoldingList::setItemData(FXFoldingItem* item,void* ptr) const {
  if(item==NULL){ fxerror("%s::setItemData: item is NULL.\n",getClassName()); }
  item->setData(ptr);
  }


// Get item data
void* FXFoldingList::getItemData(const FXFoldingItem* item) const {
  if(item==NULL){ fxerror("%s::getItemData: item is NULL.\n",getClassName()); }
  return item->getData();
  }


// True if item is selected
FXbool FXFoldingList::isItemSelected(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemSelected: item is NULL.\n",getClassName()); }
  return item->isSelected();
  }


// True if item is current
FXbool FXFoldingList::isItemCurrent(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemCurrent: item is NULL.\n",getClassName()); }
  return currentitem==item;
  }


// Check if item is expanded
FXbool FXFoldingList::isItemExpanded(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemExpanded: item is NULL.\n",getClassName()); }
  return (options&FOLDINGLIST_AUTOSELECT) || item->isExpanded();
  }


// Is item a leaf item
FXbool FXFoldingList::isItemLeaf(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemLeaf: item is NULL.\n",getClassName()); }
  return item->first==NULL;
  }


// Check if item is enabled
FXbool FXFoldingList::isItemEnabled(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemEnabled: item is NULL.\n",getClassName()); }
  return item->isEnabled();
  }


// Check item is open
FXbool FXFoldingList::isItemOpened(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemOpen: item is NULL.\n",getClassName()); }
  return item->isOpened();
  }


// True if item (partially) visible
FXbool FXFoldingList::isItemVisible(const FXFoldingItem* item) const {
  if(!item){ fxerror("%s::isItemVisible: item is NULL.\n",getClassName()); }
  return 0<pos_y+header->getHeight()+item->y+item->getHeight(this) && pos_y+header->getHeight()+item->y<viewport_h;
  }


// Make item fully visible
void FXFoldingList::makeItemVisible(FXFoldingItem* item){
  register FXint hh=header->getHeight();
  register FXFoldingItem *par;
  register FXint y,h;
  if(item){

    // Remember for later
    viewableitem=item;

    // Expand parents of this node
    if(!(options&FOLDINGLIST_AUTOSELECT)){
      for(par=item->parent; par; par=par->parent){
        expandTree(par);
        }
      }

    // Now we adjust the scrolled position to fit everything
    if(xid){

      // Force layout if dirty
      if(flags&FLAG_RECALC) layout();

      y=pos_y;
      h=item->getHeight(this);

      if(viewport_h<=y+item->y+h+hh) y=viewport_h-item->y-h-hh;
      if(y+item->y<=0) y=-item->y;

      // Scroll into view
      setPosition(pos_x,y);

      // Done it
      viewableitem=NULL;
      }
    }
  }


// Get item at position x,y
FXFoldingItem* FXFoldingList::getItemAt(FXint,FXint y) const {
  register FXint hh=header->getHeight();
  register FXFoldingItem* item=firstitem;
  register FXint ix,iy,ih;
  ix=pos_x;
  iy=pos_y+hh;
  if(options&FOLDINGLIST_ROOT_BOXES) ix+=(4+indent);
  while(item && iy<=y){
    ih=item->getHeight(this);
    if(y<iy+ih) return item;
    iy+=ih;
    if(item->first && ((options&FOLDINGLIST_AUTOSELECT) || item->isExpanded())){
      ix+=(indent+ih/2);
      item=item->first;
      continue;
      }
    while(!item->next && item->parent){
      item=item->parent;
      ix-=(indent+item->getHeight(this)/2);
      }
    item=item->next;
    }
  return NULL;
  }


// Did we hit the item, and which part of it did we hit (0=outside, 1=icon, 2=text, 3=box)
FXint FXFoldingList::hitItem(const FXFoldingItem* item,FXint x,FXint y) const {
  register FXint hh=header->getHeight();
  register FXint ix,iy,ih,xh,yh,hit=0;
  if(item){
    x-=pos_x;
    y-=pos_y;
    ix=item->x;
    iy=item->y+hh;
    ih=item->getHeight(this);
    if(iy<=y && y<iy+ih){
      if((options&FOLDINGLIST_SHOWS_BOXES) && (item->hasItems() || item->getFirst())){
        xh=ix-indent+(SIDE_SPACING/2);
        yh=iy+ih/2;
        if(xh-HALFBOX_SIZE-BOX_FUDGE<=x && x<=xh+HALFBOX_SIZE+BOX_FUDGE && yh-HALFBOX_SIZE-BOX_FUDGE<=y && y<=yh+HALFBOX_SIZE+BOX_FUDGE) return 3;
        }
      hit=item->hitItem(this,x-ix,y-iy);
      }
    }
  return hit;
  }


// Repaint
void FXFoldingList::updateItem(FXFoldingItem* item){
  if(item) update(0,pos_y+item->y+header->getHeight(),width,item->getHeight(this));
  }


// Enable one item
FXbool FXFoldingList::enableItem(FXFoldingItem* item){
  if(!item){ fxerror("%s::enableItem: item is NULL.\n",getClassName()); }
  if(!item->isEnabled()){
    item->setEnabled(TRUE);
    updateItem(item);
    return TRUE;
    }
  return FALSE;
  }


// Disable one item
FXbool FXFoldingList::disableItem(FXFoldingItem* item){
  if(!item){ fxerror("%s::disableItem: item is NULL.\n",getClassName()); }
  if(item->isEnabled()){
    item->setEnabled(FALSE);
    updateItem(item);
    return TRUE;
    }
  return FALSE;
  }


// Select one item
FXbool FXFoldingList::selectItem(FXFoldingItem* item,FXbool notify){
  if(!item){ fxerror("%s::selectItem: NULL argument.\n",getClassName()); }
  if(!item->isSelected()){
    switch(options&SELECT_MASK){
      case FOLDINGLIST_SINGLESELECT:
      case FOLDINGLIST_BROWSESELECT:
        killSelection(notify);
      case FOLDINGLIST_EXTENDEDSELECT:
      case FOLDINGLIST_MULTIPLESELECT:
        item->setSelected(TRUE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)item);}
        break;
      }
    return TRUE;
    }
  return FALSE;
  }


// Deselect one item
FXbool FXFoldingList::deselectItem(FXFoldingItem* item,FXbool notify){
  if(!item){ fxerror("%s::deselectItem: item is NULL.\n",getClassName()); }
  if(item->isSelected()){
    switch(options&SELECT_MASK){
      case FOLDINGLIST_EXTENDEDSELECT:
      case FOLDINGLIST_MULTIPLESELECT:
      case FOLDINGLIST_SINGLESELECT:
        item->setSelected(FALSE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)item);}
        break;
      }
    return TRUE;
    }
  return FALSE;
  }


// toggle one item
FXbool FXFoldingList::toggleItem(FXFoldingItem* item,FXbool notify){
  if(!item){ fxerror("%s::toggleItem: item is NULL.\n",getClassName()); }
  switch(options&SELECT_MASK){
    case FOLDINGLIST_BROWSESELECT:
      if(!item->isSelected()){
        killSelection(notify);
        item->setSelected(TRUE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)item);}
        }
      break;
    case FOLDINGLIST_SINGLESELECT:
      if(!item->isSelected()){
        killSelection(notify);
        item->setSelected(TRUE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)item);}
        }
      else{
        item->setSelected(FALSE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)item);}
        }
      break;
    case FOLDINGLIST_EXTENDEDSELECT:
    case FOLDINGLIST_MULTIPLESELECT:
      if(!item->isSelected()){
        item->setSelected(TRUE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)item);}
        }
      else{
        item->setSelected(FALSE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)item);}
        }
      break;
    }
  return TRUE;
  }



// Extend selection
FXbool FXFoldingList::extendSelection(FXFoldingItem* item,FXbool notify){
  register FXFoldingItem *it,*i1,*i2,*i3;
  register FXbool changes=FALSE;
  if(item && anchoritem && extentitem){
    it=firstitem;
    i1=i2=i3=NULL;

    // Find segments
    while(it){
      if(it==item){i1=i2;i2=i3;i3=it;}
      if(it==anchoritem){i1=i2;i2=i3;i3=it;}
      if(it==extentitem){i1=i2;i2=i3;i3=it;}
      it=it->getBelow();
      }

    FXASSERT(i1 && i2 && i3);

    // First segment
    it=i1;
    while(it!=i2){

      // item = extent - anchor
      // item = anchor - extent
      if(i1==item){
        if(!it->isSelected()){
          it->setSelected(TRUE);
          updateItem(it);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)it);}
          }
        }

      // extent = anchor - item
      // extent = item   - anchor
      else if(i1==extentitem){
        if(it->isSelected()){
          it->setSelected(FALSE);
          updateItem(it);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)it);}
          }
        }
      it=it->getBelow();
      }

    // Second segment
    it=i2;
    while(it!=i3){
      it=it->getBelow();

      // extent - anchor = item
      // anchor - extent = item
      if(i3==item){
        if(!it->isSelected()){
          it->setSelected(TRUE);
          updateItem(it);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)it);}
          }
        }

      // item   - anchor = extent
      // anchor - item   = extent
      else if(i3==extentitem){
        if(it->isSelected()){
          it->setSelected(FALSE);
          updateItem(it);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)it);}
          }
        }
      }
    extentitem=item;
    }
  return changes;
  }


// Kill selection
FXbool FXFoldingList::killSelection(FXbool notify){
  register FXFoldingItem *item=firstitem;
  register FXbool changes=FALSE;
  while(item){
    if(item->isSelected()){
      item->setSelected(FALSE);
      updateItem(item);
      changes=TRUE;
      if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)item);}
      }
    item=item->getBelow();
    }
  return changes;
  }


// Open item
FXbool FXFoldingList::openItem(FXFoldingItem* item,FXbool notify){
  if(item==NULL){ fxerror("%s::openItem: item is NULL.\n",getClassName()); }
  if(!item->isOpened()){
    item->setOpened(TRUE);
    updateItem(item);
    if(notify && target){target->tryHandle(this,FXSEL(SEL_OPENED,message),(void*)item);}
    return TRUE;
    }
  return FALSE;
  }


// Close item
FXbool FXFoldingList::closeItem(FXFoldingItem* item,FXbool notify){
  if(item==NULL){ fxerror("%s::closeItem: item is NULL.\n",getClassName()); }
  if(item->isOpened()){
    item->setOpened(FALSE);
    updateItem(item);
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CLOSED,message),(void*)item);}
    return TRUE;
    }
  return FALSE;
  }


// Collapse all subtrees under item
FXbool FXFoldingList::collapseTree(FXFoldingItem* tree,FXbool notify){
  if(tree==NULL){ fxerror("%s::collapseTree: tree is NULL.\n",getClassName()); }
  if(tree->isExpanded()){
    tree->setExpanded(FALSE);
    if(!(options&FOLDINGLIST_AUTOSELECT)){     // In autoselect, already shown as expanded!
      if(tree->first){
        recalc();
        }
      else{
        updateItem(tree);
        }
      }
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COLLAPSED,message),(void*)tree);}
    return TRUE;
    }
  return FALSE;
  }


// Expand subtree under item
FXbool FXFoldingList::expandTree(FXFoldingItem* tree,FXbool notify){
  if(tree==NULL){ fxerror("%s::expandTree: tree is NULL.\n",getClassName()); }
  if(!tree->isExpanded()){
    tree->setExpanded(TRUE);
    if(!(options&FOLDINGLIST_AUTOSELECT)){     // In autoselect, already shown as expanded!
      if(tree->first){
        recalc();
        }
      else{
        updateItem(tree);
        }
      }
    if(notify && target){target->tryHandle(this,FXSEL(SEL_EXPANDED,message),(void*)tree);}
    return TRUE;
    }
  return FALSE;
  }


// Start motion timer while in this window
long FXFoldingList::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onEnter(sender,sel,ptr);
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
  cursoritem=NULL;
  return 1;
  }


// Stop motion timer when leaving window
long FXFoldingList::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onLeave(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_TIPTIMER);
  cursoritem=NULL;
  return 1;
  }


// We timed out, i.e. the user didn't move for a while
long FXFoldingList::onTipTimer(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onTipTimer %p\n",getClassName(),this));
  flags|=FLAG_TIP;
  return 1;
  }


// We were asked about tip text
long FXFoldingList::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !(options&FOLDINGLIST_AUTOSELECT)){   // No tip when autoselect!
    FXint x,y; FXuint buttons;
    getCursorPosition(x,y,buttons);
    FXFoldingItem *item=getItemAt(x,y);
    if(item){
      FXString string=item->getText().section('\t',0);
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
      return 1;
      }
    }
  return 0;
  }


// We were asked about status text
long FXFoldingList::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Gained focus
long FXFoldingList::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusIn(sender,sel,ptr);
  if(currentitem){
    currentitem->setFocus(TRUE);
    updateItem(currentitem);
    }
  return 1;
  }


// Lost focus
long FXFoldingList::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusOut(sender,sel,ptr);
  if(currentitem){
    currentitem->setFocus(FALSE);
    updateItem(currentitem);
    }
  return 1;
  }


// Draw item list
long FXFoldingList::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXFoldingItem* item=firstitem;
  FXFoldingItem* p;
  FXint yh,xh,x,y,w,h,xp,hh;
  FXDCWindow dc(this,event);
  dc.setFont(font);
  if(header->getNumItems()==0){
    dc.setForeground(backColor);
    dc.fillRectangle(0,0,width,height);
    return 1;
    }
  x=pos_x;
  y=pos_y+header->getHeight();
  if(options&FOLDINGLIST_ROOT_BOXES) x+=(4+indent);
  while(item && y<event->rect.y+event->rect.h){
    w=item->getWidth(this);
    h=item->getHeight(this);
    if(event->rect.y<=y+h){

      // Draw item
      dc.setForeground(backColor);
      dc.fillRectangle(0,y,width,h);
      item->draw(this,dc,x,y,w,h);

      // Show other paraphernalia such as dotted lines and expand-boxes
      if((options&(FOLDINGLIST_SHOWS_LINES|FOLDINGLIST_SHOWS_BOXES)) && (item->parent || (options&FOLDINGLIST_ROOT_BOXES))){
        dc.setClipRectangle(header->getX(),y,header->getItemSize(0),h);
        hh=h/2;
        yh=y+hh;
        xh=x-indent+(SIDE_SPACING/2);
        dc.setForeground(lineColor);
        dc.setStipple(STIPPLE_GRAY,pos_x&1,pos_y&1);
        if(options&FOLDINGLIST_SHOWS_LINES){                   // Connect items with lines
          p=item->parent;
          xp=xh;
          dc.setFillStyle(FILL_STIPPLED);
          while(p){
            xp-=(indent+p->getHeight(this)/2);
            if(p->next) dc.fillRectangle(xp,y,1,h);
            p=p->parent;
            }
          if((options&FOLDINGLIST_SHOWS_BOXES) && (item->hasItems() || item->getFirst())){
            if(item->prev || item->parent) dc.fillRectangle(xh,y,1,yh-y-HALFBOX_SIZE);
            if(item->next) dc.fillRectangle(xh,yh+HALFBOX_SIZE,1,y+h-yh-HALFBOX_SIZE);
            }
          else{
            if(item->prev || item->parent) dc.fillRectangle(xh,y,1,hh);
            if( item->next) dc.fillRectangle(xh,yh,1,h);
            dc.fillRectangle(xh,yh,x+(SIDE_SPACING/2)-2-xh,1);
            }
          dc.setFillStyle(FILL_SOLID);
          }

        // Boxes before items for expand/collapse of item
        if((options&FOLDINGLIST_SHOWS_BOXES) && (item->hasItems() || item->getFirst())){
          dc.setFillStyle(FILL_STIPPLED);
          dc.fillRectangle(xh+4,yh,(SIDE_SPACING/2)-2,1);
          dc.setFillStyle(FILL_SOLID);
          dc.drawRectangle(xh-HALFBOX_SIZE,yh-HALFBOX_SIZE,HALFBOX_SIZE+HALFBOX_SIZE,HALFBOX_SIZE+HALFBOX_SIZE);
          dc.setForeground(textColor);
          dc.fillRectangle(xh-HALFBOX_SIZE+2,yh,HALFBOX_SIZE+HALFBOX_SIZE-3,1);
          if(!(options&FOLDINGLIST_AUTOSELECT) && !item->isExpanded()){
            dc.fillRectangle(xh,yh-HALFBOX_SIZE+2,1,HALFBOX_SIZE+HALFBOX_SIZE-3);
            }
          }
        dc.clearClipRectangle();
        }
      }

    y+=h;

    // Move on to the next item
    if(item->first && ((options&FOLDINGLIST_AUTOSELECT) || item->isExpanded())){
      x+=(indent+h/2);
      item=item->first;
      continue;
      }
    while(!item->next && item->parent){
      item=item->parent;
      x-=(indent+item->getHeight(this)/2);
      }
    item=item->next;
    }
  if(y<event->rect.y+event->rect.h){
    dc.setForeground(backColor);
    dc.fillRectangle(event->rect.x,y,event->rect.w,event->rect.y+event->rect.h-y);
    }
  return 1;
  }


// Zero out lookup string
long FXFoldingList::onLookupTimer(FXObject*,FXSelector,void*){
  lookup=FXString::null;
  return 1;
  }


// Key Press
long FXFoldingList::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXFoldingItem *item=currentitem;
  FXFoldingItem *succ;
  FXint page;
  flags&=~FLAG_TIP;
  if(!isEnabled()) return 0;
  if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
  if(item==NULL) item=firstitem;
  switch(event->code){
    case KEY_Control_L:
    case KEY_Control_R:
    case KEY_Shift_L:
    case KEY_Shift_R:
    case KEY_Alt_L:
    case KEY_Alt_R:
      if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
      return 1;
    case KEY_Page_Up:
    case KEY_KP_Page_Up:
      for(succ=item,page=verticalScrollBar()->getPage(); succ && 0<page; ){
        item=succ;
        page-=succ->getHeight(this);
        if(succ->prev){
          succ=succ->prev;
          while(succ->last && ((options&FOLDINGLIST_AUTOSELECT) || succ->isExpanded())) succ=succ->last;
          }
        else if(succ->parent){
          succ=succ->parent;
          }
        }
      goto hop;
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
      for(succ=item,page=verticalScrollBar()->getPage(); succ && 0<page; ){
        item=succ;
        page-=succ->getHeight(this);
        if(succ->first && ((options&FOLDINGLIST_AUTOSELECT) || succ->isExpanded())){
          succ=succ->first;
          }
        else{
          while(!succ->next && succ->parent) succ=succ->parent;
          succ=succ->next;
          }
        }
      goto hop;
    case KEY_Up:                          // Move up
    case KEY_KP_Up:
      if(item){
        if(item->prev){
          item=item->prev;
          while(item->first && ((options&FOLDINGLIST_AUTOSELECT) || item->isExpanded())) item=item->last;
          }
        else if(item->parent){
          item=item->parent;
          }
        }
      goto hop;
    case KEY_Down:                        // Move down
    case KEY_KP_Down:
      if(item){
        if(item->first && ((options&FOLDINGLIST_AUTOSELECT) || item->isExpanded())){
          item=item->first;
          }
        else{
          while(!item->next && item->parent) item=item->parent;
          item=item->next;
          }
        }
      goto hop;
    case KEY_Right:                       // Move right/down and open subtree
    case KEY_KP_Right:
      if(item){
        if(!(options&FOLDINGLIST_AUTOSELECT) && !item->isExpanded() && (item->hasItems() || item->getFirst())){
          expandTree(item,TRUE);
          }
        else if(item->first){
          item=item->first;
          }
        else{
          while(!item->next && item->parent) item=item->parent;
          item=item->next;
          }
        }
      goto hop;
    case KEY_Left:                        // Move left/up and close subtree
    case KEY_KP_Left:
      if(item){
        if(!(options&FOLDINGLIST_AUTOSELECT) && item->isExpanded() && (item->hasItems() || item->getFirst())){
          collapseTree(item,TRUE);
          }
        else if(item->parent){
          item=item->parent;
          }
        else if(item->prev){
          item=item->prev;
          }
        }
      goto hop;
    case KEY_Home:                        // Move to first
    case KEY_KP_Home:
      item=firstitem;
      goto hop;
    case KEY_End:                         // Move to last
    case KEY_KP_End:
      item=lastitem;
      while(item){
        if(item->last && ((options&FOLDINGLIST_AUTOSELECT) || item->isExpanded())){
          item=item->last;
          }
        else if(item->next){
          item=item->next;
          }
        else{
          break;
          }
        }
hop:  lookup=FXString::null;
      if(item){
        setCurrentItem(item,TRUE);
        makeItemVisible(item);
        if((options&SELECT_MASK)==FOLDINGLIST_EXTENDEDSELECT){
          if(item->isEnabled()){
            if(event->state&SHIFTMASK){
              if(anchoritem){
                selectItem(anchoritem,TRUE);
                extendSelection(item,TRUE);
                }
              else{
                selectItem(item,TRUE);
                setAnchorItem(item);
                }
              }
            else if(!(event->state&CONTROLMASK)){
              killSelection(TRUE);
              selectItem(item,TRUE);
              setAnchorItem(item);
              }
            }
          }
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
      if(currentitem && currentitem->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        }
      return 1;
    case KEY_space:
    case KEY_KP_Space:
      lookup=FXString::null;
      if(item && item->isEnabled()){
        switch(options&SELECT_MASK){
          case FOLDINGLIST_EXTENDEDSELECT:
            if(event->state&SHIFTMASK){
              if(anchoritem){
                selectItem(anchoritem,TRUE);
                extendSelection(item,TRUE);
                }
              else{
                selectItem(item,TRUE);
                }
              }
            else if(event->state&CONTROLMASK){
              toggleItem(item,TRUE);
              }
            else{
              killSelection(TRUE);
              selectItem(item,TRUE);
              }
            break;
          case FOLDINGLIST_MULTIPLESELECT:
          case FOLDINGLIST_SINGLESELECT:
            toggleItem(item,TRUE);
            break;
          }
        setAnchorItem(item);
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
      if(currentitem && currentitem->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        }
      return 1;
    case KEY_Return:
    case KEY_KP_Enter:
      lookup=FXString::null;
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)currentitem);
      if(currentitem && currentitem->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        }
      return 1;
    default:
      if((FXuchar)event->text[0]<' ') return 0;
      if(event->state&(CONTROLMASK|ALTMASK)) return 0;
      if(!Ascii::isPrint(event->text[0])) return 0;
      lookup.append(event->text);
      getApp()->addTimeout(this,ID_LOOKUPTIMER,getApp()->getTypingSpeed());
      item=findItem(lookup,currentitem,SEARCH_FORWARD|SEARCH_WRAP|SEARCH_PREFIX);
      if(item){
	setCurrentItem(item,TRUE);
	makeItemVisible(item);
	if((options&SELECT_MASK)==FOLDINGLIST_EXTENDEDSELECT){
	  if(item->isEnabled()){
	    killSelection(TRUE);
	    selectItem(item,TRUE);
	    }
	  }
	setAnchorItem(item);
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
      if(currentitem && currentitem->isEnabled()){
	handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
	}
      return 1;
    }
  return 0;
  }


// Key Release
long FXFoldingList::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(!isEnabled()) return 0;
  if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
  switch(event->code){
    case KEY_Shift_L:
    case KEY_Shift_R:
    case KEY_Control_L:
    case KEY_Control_R:
    case KEY_Alt_L:
    case KEY_Alt_R:
      if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
      return 1;
    }
  return 0;
  }


// Scroll timer
long FXFoldingList::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXFoldingItem *item;
  FXint xx,yy;

  // Scroll the content
  FXScrollArea::onAutoScroll(sender,sel,ptr);

  // Drag and drop mode
  if(flags&FLAG_DODRAG){
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }

  // In autoselect mode, stop scrolling when mouse outside window
  if((flags&FLAG_PRESSED) || (options&FOLDINGLIST_AUTOSELECT)){

    // Validated position
    xx=event->win_x; if(xx<0) xx=0; else if(xx>=viewport_w) xx=viewport_w-1;
    yy=event->win_y; if(yy<0) yy=0; else if(yy>=viewport_h) yy=viewport_h-1;

    // Find item
    item=getItemAt(xx,yy);

    // Got item and different from last time
    if(item && item!=currentitem){

      // Make it the current item
      setCurrentItem(item,TRUE);

      // Extend the selection
      if((options&SELECT_MASK)==FOLDINGLIST_EXTENDEDSELECT){
        state=FALSE;
        extendSelection(item,TRUE);
        }
      }
    return 1;
    }
  return 0;
  }


// Mouse motion
long FXFoldingList::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXFoldingItem *oldcursoritem=cursoritem;
  FXuint flg=flags;
  FXFoldingItem *item;

  // Kill the tip
  flags&=~FLAG_TIP;

  // Kill the tip timer
  getApp()->removeTimeout(this,ID_TIPTIMER);

  // Right mouse scrolling
  if(flags&FLAG_SCROLLING){
    setPosition(event->win_x-grabx,event->win_y-graby);
    return 1;
    }

  // Drag and drop mode
  if(flags&FLAG_DODRAG){
    if(startAutoScroll(event,TRUE)) return 1;
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }

  // Tentative drag and drop
  if((flags&FLAG_TRYDRAG) && event->moved){
    flags&=~FLAG_TRYDRAG;
    if(handle(this,FXSEL(SEL_BEGINDRAG,0),ptr)){
      flags|=FLAG_DODRAG;
      }
    return 1;
    }

  // Normal operation
  if((flags&FLAG_PRESSED) || (options&FOLDINGLIST_AUTOSELECT)){

    // Start auto scrolling?
    if(startAutoScroll(event,FALSE)) return 1;

    // Find item
    item=getItemAt(event->win_x,event->win_y);

    // Got an item different from before
    if(item && item!=currentitem){

      // Make it the current item
      setCurrentItem(item,TRUE);

      // Extend the selection
      if((options&SELECT_MASK)==FOLDINGLIST_EXTENDEDSELECT){
        state=FALSE;
        extendSelection(item,TRUE);
        }
      }
    return 1;
    }

  // Reset tip timer if nothing's going on
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());

  // Get item we're over
  cursoritem=getItemAt(event->win_x,event->win_y);

  // Force GUI update only when needed
  return (cursoritem!=oldcursoritem)||(flg&FLAG_TIP);
  }


// Pressed a button
long FXFoldingList::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXFoldingItem *item;
  FXint code;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;

    // First chance callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;

    // Not autoselect mode
    if(options&FOLDINGLIST_AUTOSELECT) return 1;

    // Locate item
    item=getItemAt(event->win_x,event->win_y);

    // No item
    if(item==NULL){
      if((options&SELECT_MASK)==FOLDINGLIST_EXTENDEDSELECT){
        if(!(event->state&(SHIFTMASK|CONTROLMASK))){
          killSelection(TRUE);
          }
        }
      return 1;
      }

    // Find out where hit
    code=hitItem(item,event->win_x,event->win_y);

    // Maybe clicked on box
    if(code==3){
      if(isItemExpanded(item))
        collapseTree(item,TRUE);
      else
        expandTree(item,TRUE);
      return 1;
      }

    // Change current item
    setCurrentItem(item,TRUE);

    // Change item selection
    state=item->isSelected();
    switch(options&SELECT_MASK){
      case FOLDINGLIST_EXTENDEDSELECT:
        if(event->state&SHIFTMASK){
          if(anchoritem){
            if(anchoritem->isEnabled()) selectItem(anchoritem,TRUE);
            extendSelection(item,TRUE);
            }
          else{
            if(item->isEnabled()) selectItem(item,TRUE);
            setAnchorItem(item);
            }
          }
        else if(event->state&CONTROLMASK){
          if(item->isEnabled() && !state) selectItem(item,TRUE);
          setAnchorItem(item);
          }
        else{
          if(item->isEnabled() && !state){ killSelection(TRUE); selectItem(item,TRUE); }
          setAnchorItem(item);
          }
        break;
      case FOLDINGLIST_MULTIPLESELECT:
      case FOLDINGLIST_SINGLESELECT:
        if(item->isEnabled() && !state) selectItem(item,TRUE);
        break;
      }

    // Start drag if actually pressed text or icon only
    if(code && item->isSelected() && item->isDraggable()){
      flags|=FLAG_TRYDRAG;
      }

    flags|=FLAG_PRESSED;
    return 1;
    }
  return 0;
  }


// Released button
long FXFoldingList::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXuint flg=flags;
  if(isEnabled()){
    ungrab();
    stopAutoScroll();
    flags|=FLAG_UPDATE;
    flags&=~(FLAG_PRESSED|FLAG_TRYDRAG|FLAG_DODRAG);

    // First chance callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;

    // No activity
    if(!(flg&FLAG_PRESSED) && !(options&FOLDINGLIST_AUTOSELECT)) return 1;

    // Was dragging
    if(flg&FLAG_DODRAG){
      handle(this,FXSEL(SEL_ENDDRAG,0),ptr);
      return 1;
      }

    // Select only enabled item
    switch(options&SELECT_MASK){
      case FOLDINGLIST_EXTENDEDSELECT:
        if(currentitem && currentitem->isEnabled()){
          if(event->state&CONTROLMASK){
            if(state) deselectItem(currentitem,TRUE);
            }
          else if(!(event->state&SHIFTMASK)){
            if(state){ killSelection(TRUE); selectItem(currentitem,TRUE); }
            }
          }
        break;
      case FOLDINGLIST_MULTIPLESELECT:
      case FOLDINGLIST_SINGLESELECT:
        if(currentitem && currentitem->isEnabled()){
          if(state) deselectItem(currentitem,TRUE);
          }
        break;
      }

    // Scroll to make item visibke
    makeItemVisible(currentitem);

    // Update anchor
    setAnchorItem(currentitem);

    // Generate clicked callbacks
    if(event->click_count==1){
      handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
      }
    else if(event->click_count==2){
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)currentitem);
      }
    else if(event->click_count==3){
      handle(this,FXSEL(SEL_TRIPLECLICKED,0),(void*)currentitem);
      }

    // Command callback only when clicked on item
    if(currentitem && currentitem->isEnabled()){
      handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
      }
    return 1;
    }
  return 0;
  }


// Pressed right button
long FXFoldingList::onRightBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONPRESS,message),ptr)) return 1;
    flags|=FLAG_SCROLLING;
    grabx=event->win_x-pos_x;
    graby=event->win_y-pos_y;
    return 1;
    }
  return 0;
  }


// Released right button
long FXFoldingList::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    ungrab();
    flags&=~FLAG_SCROLLING;
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONRELEASE,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }



// The widget lost the grab for some reason
long FXFoldingList::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onUngrabbed(sender,sel,ptr);
  flags&=~(FLAG_DODRAG|FLAG_TRYDRAG|FLAG_PRESSED|FLAG_CHANGED|FLAG_SCROLLING);
  flags|=FLAG_UPDATE;
  stopAutoScroll();
  return 1;
  }


// Command message
long FXFoldingList::onCommand(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
  }


// Clicked in list
long FXFoldingList::onClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr);
  }


// Double clicked in list; ptr may or may not point to an item
long FXFoldingList::onDoubleClicked(FXObject*,FXSelector,void* ptr){

  // Double click anywhere in the widget
  if(target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr)) return 1;

  // Double click on an item
  if(ptr){
    if(isItemExpanded((FXFoldingItem*)ptr))
      collapseTree((FXFoldingItem*)ptr,TRUE);
    else
      expandTree((FXFoldingItem*)ptr,TRUE);
    }
  return 0;
  }


// Triple clicked in list; ptr may or may not point to an item
long FXFoldingList::onTripleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
  }


// Compare sectioned strings
FXint FXFoldingList::compareSection(const FXchar *p,const FXchar* q,FXint s){
  register FXint c1,c2,x;
  for(x=s; x && *p; x-=(*p++=='\t'));
  for(x=s; x && *q; x-=(*q++=='\t'));
  do{
    c1=FXuchar(*p++);
    c2=FXuchar(*q++);
    }
  while('\t'<c1 && (c1==c2));
  return c1-c2;
  }


// Compare sectioned strings, case-insensitive
FXint FXFoldingList::compareSectionCase(const FXchar *p,const FXchar* q,FXint s){
  register FXint c1,c2,x;
  for(x=s; x && *p; x-=(*p++=='\t'));
  for(x=s; x && *q; x-=(*q++=='\t'));
  do{
    if((*p & 0x80) && (*q & 0x80)){
      c1=Unicode::toLower(wc(p)); p+=wclen(p);
      c2=Unicode::toLower(wc(q)); q+=wclen(q);
      }
    else{
      c1=Ascii::toLower(*p); p+=1;
      c2=Ascii::toLower(*q); q+=1;
      }
    }
  while('\t'<c1 && (c1==c2));
  return c1-c2;
  }


// Sort items in ascending order
FXint FXFoldingList::ascending(const FXFoldingItem* a,const FXFoldingItem* b){
  return compareSection(a->getText().text(),b->getText().text(),0);
  }


// Sort items in descending order
FXint FXFoldingList::descending(const FXFoldingItem* a,const FXFoldingItem* b){
  return compareSection(b->getText().text(),a->getText().text(),0);
  }


// Sort ascending order, case insensitive
FXint FXFoldingList::ascendingCase(const FXFoldingItem* a,const FXFoldingItem* b){
  return compareSectionCase(a->getText().text(),b->getText().text(),0);
  }


// Sort descending order, case insensitive
FXint FXFoldingList::descendingCase(const FXFoldingItem* a,const FXFoldingItem* b){
  return compareSectionCase(b->getText().text(),a->getText().text(),0);
  }


// Sort items
void FXFoldingList::sort(FXFoldingItem*& f1,FXFoldingItem*& t1,FXFoldingItem*& f2,FXFoldingItem*& t2,int n){
  FXFoldingItem *ff1,*tt1,*ff2,*tt2,*q;
  FXint m;
  if(f2==NULL){
    f1=NULL;
    t1=NULL;
    return;
    }
  if(n>1){
    m=n/2;
    n=n-m;
    sort(ff1,tt1,f2,t2,n);  // 1 or more
    sort(ff2,tt2,f2,t2,m);  // 0 or more
    FXASSERT(ff1);
    if(ff2 && sortfunc(ff1,ff2)>0){
      f1=ff2;
      ff2->prev=NULL;
      ff2=ff2->next;
      }
    else{
      f1=ff1;
      ff1->prev=NULL;
      ff1=ff1->next;
      }
    t1=f1;
    t1->next=NULL;
    while(ff1 || ff2){
      if(ff1==NULL){ t1->next=ff2; ff2->prev=t1; t1=tt2; break; }
      if(ff2==NULL){ t1->next=ff1; ff1->prev=t1; t1=tt1; break; }
      if(sortfunc(ff1,ff2)>0){
        t1->next=ff2;
        ff2->prev=t1;
        t1=ff2;
        ff2=ff2->next;
        }
      else{
        t1->next=ff1;
        ff1->prev=t1;
        t1=ff1;
        ff1=ff1->next;
        }
      t1->next=NULL;
      }
    return;
    }
  FXASSERT(f2);
  f1=f2;
  t1=f2;
  f2=f2->next;
  while(f2){
    f2->prev=NULL;
    if(sortfunc(f2,t1)>0){
      t1->next=f2;
      f2->prev=t1;
      t1=f2;
      f2=f2->next;
      continue;
      }
    if(sortfunc(f1,f2)>0){
      q=f2;
      f2=f2->next;
      q->next=f1;
      f1->prev=q;
      f1=q;
      continue;
      }
    break;
    }
  FXASSERT(f1);
  FXASSERT(t1);
  f1->prev=NULL;
  t1->next=NULL;
  }


// Sort the items based on the sort function
void FXFoldingList::sortRootItems(){
  if(sortfunc){
    FXFoldingItem* f=firstitem;
    FXFoldingItem* l=lastitem;
    sort(firstitem,lastitem,f,l,getNumItems());
    recalc();
    }
  }


// Sort child items
void FXFoldingList::sortChildItems(FXFoldingItem* item){
  if(sortfunc){
    FXFoldingItem* f=item->first;
    FXFoldingItem* l=item->last;
    sort(item->first,item->last,f,l,item->getNumChildren());
    if(item->isExpanded()) recalc();     // No need to recalc if it ain't visible!
    }
  }


// Sort all items recursively
void FXFoldingList::sortItems(){
  register FXFoldingItem *item;
  if(sortfunc){
    sortRootItems();
    item=firstitem;
    while(item){
      sortChildItems(item);
      if(item->first){item=item->first;continue;}
      while(!item->next && item->parent){item=item->parent;}
      item=item->next;
      }
    }
  }


// Set current item
void FXFoldingList::setCurrentItem(FXFoldingItem* item,FXbool notify){
  if(item!=currentitem){

    // Deactivate old item
    if(currentitem){

      // No visible change if it doen't have the focus
      if(hasFocus()){
        currentitem->setFocus(FALSE);
        updateItem(currentitem);
        }

      // Close old item
      closeItem(currentitem,notify);
      }

    currentitem=item;

    // Activate new item
    if(currentitem){

      // No visible change if it doen't have the focus
      if(hasFocus()){
        currentitem->setFocus(TRUE);
        updateItem(currentitem);
        }

      // Open new item
      openItem(currentitem,notify);
      }

    // Notify item change
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)currentitem);}
    }

  // Select if browse mode
  if((options&SELECT_MASK)==FOLDINGLIST_BROWSESELECT && currentitem && currentitem->isEnabled()){
    selectItem(currentitem,notify);
    }
  }


// Set anchor item
void FXFoldingList::setAnchorItem(FXFoldingItem* item){
  anchoritem=item;
  extentitem=item;
  }



// Create item
FXFoldingItem* FXFoldingList::createItem(const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr){
  return new FXFoldingItem(text,oi,ci,ptr);
  }


// Insert item under father before other item
FXFoldingItem* FXFoldingList::insertItem(FXFoldingItem* other,FXFoldingItem* father,FXFoldingItem* item,FXbool notify){
  register FXFoldingItem* olditem=currentitem;

  // Verify correctness of arguments
  if(!item){ fxerror("%s::insertItem: NULL item argument.\n",getClassName()); }
  if(other && other->parent!=father){ fxerror("%s::insertItem: bad argument.\n",getClassName()); }

  // Hang item into the list
  if(father){
    if(other){
      item->next=other;
      item->prev=other->prev;
      other->prev=item;
      }
    else{
      item->next=NULL;
      item->prev=father->last;
      father->last=item;
      }
    if(item->prev) item->prev->next=item; else father->first=item;
    }
  else{
    if(other){
      item->next=other;
      item->prev=other->prev;
      other->prev=item;
      }
    else{
      item->next=NULL;
      item->prev=lastitem;
      lastitem=item;
      }
    if(item->prev) item->prev->next=item; else firstitem=item;
    }

  // Fill in the rest
  item->parent=father;
  item->first=NULL;
  item->last=NULL;
  item->x=0;
  item->y=0;

  // Make current if just added
  if(!currentitem && item==lastitem) currentitem=item;

  // Notify item has been inserted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)item);}

  // Current item may have changed
  if(olditem!=currentitem){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)currentitem);}
    }

  // Was new item
  if(currentitem==item){
    if(hasFocus()){
      currentitem->setFocus(TRUE);
      }
    if((options&SELECT_MASK)==FOLDINGLIST_BROWSESELECT && currentitem->isEnabled()){
      selectItem(currentitem,notify);
      }
    }

  // Redo layout
  recalc();
  return item;
  }


// Insert item under father before other item
FXFoldingItem* FXFoldingList::insertItem(FXFoldingItem* other,FXFoldingItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  return insertItem(other,father,createItem(text,oi,ci,ptr),notify);
  }


// Append item under father
FXFoldingItem* FXFoldingList::appendItem(FXFoldingItem* father,FXFoldingItem* item,FXbool notify){
  return insertItem(NULL,father,item,notify);
  }


// Append item under father
FXFoldingItem* FXFoldingList::appendItem(FXFoldingItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  return insertItem(NULL,father,createItem(text,oi,ci,ptr),notify);
  }


// Prepend item under father
FXFoldingItem* FXFoldingList::prependItem(FXFoldingItem* father,FXFoldingItem* item,FXbool notify){
  return insertItem(father?father->first:firstitem,father,item,notify);
  }

// Prepend item under father
FXFoldingItem* FXFoldingList::prependItem(FXFoldingItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  return insertItem(father?father->first:firstitem,father,createItem(text,oi,ci,ptr),notify);
  }


// Fill list by appending items from array of strings
FXint FXFoldingList::fillItems(FXFoldingItem* father,const FXchar** strings,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  register FXint n=0;
  if(strings){
    while(strings[n]){
      appendItem(father,strings[n++],oi,ci,ptr,notify);
      }
    }
  return n;
  }


// Fill list by appending items from newline separated strings
FXint FXFoldingList::fillItems(FXFoldingItem* father,const FXString& strings,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  register FXint n=0;
  FXString text;
  while(!(text=strings.section('\n',n)).empty()){
    appendItem(father,text,oi,ci,ptr,notify);
    n++;
    }
  return n;
  }


// Move item under father before other item
FXFoldingItem *FXFoldingList::moveItem(FXFoldingItem* other,FXFoldingItem* father,FXFoldingItem* item){

  // Verify arguments
  if(!item){ fxerror("%s::moveItem: NULL item argument.\n",getClassName()); }
  if(other && other->parent!=father){ fxerror("%s::moveItem: bad argument.\n",getClassName()); }

  // Can't move in front of itself
  if(item!=other){

    // Unlink from current spot
    if(item->prev) item->prev->next=item->next; else if(item->parent) item->parent->first=item->next; else firstitem=item->next;
    if(item->next) item->next->prev=item->prev; else if(item->parent) item->parent->last=item->prev; else lastitem=item->prev;

    // Hang item into the list
    if(father){
      if(other){
        item->next=other;
        item->prev=other->prev;
        other->prev=item;
        }
      else{
        item->next=NULL;
        item->prev=father->last;
        father->last=item;
        }
      if(item->prev) item->prev->next=item; else father->first=item;
      }
    else{
      if(other){
        item->next=other;
        item->prev=other->prev;
        other->prev=item;
        }
      else{
        item->next=NULL;
        item->prev=lastitem;
        lastitem=item;
        }
      if(item->prev) item->prev->next=item; else firstitem=item;
      }

    // Fill in the rest
    item->parent=father;

    // Redo layout
    recalc();
    }
  return item;
  }


// Extract node from list
FXFoldingItem* FXFoldingList::extractItem(FXFoldingItem* item,FXbool notify){
  register FXFoldingItem *olditem=currentitem;
  register FXFoldingItem *result=item;
  register FXFoldingItem *prv;
  register FXFoldingItem *nxt;
  register FXFoldingItem *par;
  if(item){

    // Remember hookups
    nxt=result->next;
    prv=result->prev;
    par=result->parent;

    // Unlink item from tree
    if(prv) prv->next=nxt; else if(par) par->first=nxt; else firstitem=nxt;
    if(nxt) nxt->prev=prv; else if(par) par->last=prv; else lastitem=prv;

    // Is now unhooked
    result->parent=NULL;
    result->next=NULL;
    result->prev=NULL;

    // Successor value
    if(prv) par=prv;
    if(nxt) par=nxt;

    // Visit all children
    while(item){

       // Adjust pointers
      if(anchoritem==item) anchoritem=par;
      if(currentitem==item) currentitem=par;
      if(extentitem==item) extentitem=par;
      if(viewableitem==item) viewableitem=par;

      // Next item
      if(item->first){
        item=item->first;
        continue;
        }
      while(!item->next && item->parent){
        item=item->parent;
        }
      item=item->next;
      }

    // Current item has changed
    if(olditem!=currentitem){
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)currentitem);}
      }

    // Extracted current item
    if(currentitem && currentitem!=olditem){
      if(hasFocus()){
        currentitem->setFocus(TRUE);
        }
      if((options&SELECT_MASK)==FOLDINGLIST_BROWSESELECT && currentitem->isEnabled()){
        selectItem(currentitem,notify);
        }
      }

    // Redo layout
    recalc();
    }
  return result;
  }


// Remove all siblings from [fm,to]
void FXFoldingList::removeItems(FXFoldingItem* fm,FXFoldingItem* to,FXbool notify){
  register FXFoldingItem *olditem=currentitem;
  register FXFoldingItem *prv;
  register FXFoldingItem *nxt;
  register FXFoldingItem *par;
  if(fm && to){
    if(fm->parent!=to->parent){ fxerror("%s::removeItems: arguments have different parent.\n",getClassName()); }

    // Delete items
    while(1){

      // Scan till end
      while(to->last) to=to->last;

      do{

        // Notify item will be deleted
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)to);}

        // Remember hookups
        nxt=to->next;
        prv=to->prev;
        par=to->parent;

         // Adjust pointers; suggested by Alan Ott <ott@acusoft.com>
        if(anchoritem==to){ anchoritem=par; if(prv) anchoritem=prv; if(nxt) anchoritem=nxt; }
        if(extentitem==to){ extentitem=par; if(prv) extentitem=prv; if(nxt) extentitem=nxt; }
        if(currentitem==to){ currentitem=par; if(prv) currentitem=prv; if(nxt) currentitem=nxt; }
        if(viewableitem==to){ viewableitem=par; if(prv) viewableitem=prv; if(nxt) viewableitem=nxt; }

        // Remove item from list
        if(prv) prv->next=nxt; else if(par) par->first=nxt; else firstitem=nxt;
        if(nxt) nxt->prev=prv; else if(par) par->last=prv; else lastitem=prv;

        // Delete it
        delete to;

        // Was last one?
        if(to==fm) goto x;
        to=par;
        }
      while(!prv);
      to=prv;
      }

    // Current item has changed
x:  if(olditem!=currentitem){
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)currentitem);}
      }

    // Deleted current item
    if(currentitem && currentitem!=olditem){
      if(hasFocus()){
        currentitem->setFocus(TRUE);
        }
      if((options&SELECT_MASK)==FOLDINGLIST_BROWSESELECT && currentitem->isEnabled()){
        selectItem(currentitem,notify);
        }
      }

    // Redo layout
    recalc();
    }
  }


// Remove node from list
void FXFoldingList::removeItem(FXFoldingItem* item,FXbool notify){
  removeItems(item,item,notify);
  }


// Remove all items
void FXFoldingList::clearItems(FXbool notify){
  removeItems(firstitem,lastitem,notify);
  }


typedef FXint (*FXCompareFunc)(const FXString&,const FXString &,FXint);


// Get item by name
FXFoldingItem* FXFoldingList::findItem(const FXString& text,FXFoldingItem* start,FXuint flgs) const {
  register FXCompareFunc comparefunc;
  register FXFoldingItem *item;
  register FXint len;
  if(firstitem){
    comparefunc=(flgs&SEARCH_IGNORECASE) ? (FXCompareFunc)comparecase : (FXCompareFunc)compare;
    len=(flgs&SEARCH_PREFIX)?text.length():2147483647;
    if(flgs&SEARCH_BACKWARD){
      item=start;
      while(item!=NULL){
        if((*comparefunc)(item->getText(),text,len)==0) return item;
        item=item->getAbove();
        }
      if(start && !(flgs&SEARCH_WRAP)) return NULL;
      for(item=lastitem; item->getLast(); item=item->getLast());
      while(item!=start){
        if((*comparefunc)(item->getText(),text,len)==0) return item;
        item=item->getAbove();
        }
      }
    else{
      item=start;
      while(item!=NULL){
        if((*comparefunc)(item->getText(),text,len)==0) return item;
        item=item->getBelow();
        }
      if(start && !(flgs&SEARCH_WRAP)) return NULL;
      item=firstitem;
      while(item!=start){
        if((*comparefunc)(item->getText(),text,len)==0) return item;
        item=item->getBelow();
        }
      }
    }
  return NULL;
  }


// Get item by data
FXFoldingItem* FXFoldingList::findItemByData(const void *ptr,FXFoldingItem* start,FXuint flgs) const {
  register FXFoldingItem *item;
  if(firstitem){
    if(flgs&SEARCH_BACKWARD){
      item=start;
      while(item!=NULL){
        if(item->getData()==ptr) return item;
        item=item->getAbove();
        }
      if(start && !(flgs&SEARCH_WRAP)) return NULL;
      for(item=lastitem; item->getLast(); item=item->getLast());
      while(item!=start){
        if(item->getData()==ptr) return item;
        item=item->getAbove();
        }
      }
    else{
      item=start;
      while(item!=NULL){
        if(item->getData()==ptr) return item;
        item=item->getBelow();
        }
      if(start && !(flgs&SEARCH_WRAP)) return NULL;
      item=firstitem;
      while(item!=start){
        if(item->getData()==ptr) return item;
        item=item->getBelow();
        }
      }
    }
  return NULL;
  }


// Change the font
void FXFoldingList::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Change help text
void FXFoldingList::setHelpText(const FXString& text){
  help=text;
  }


// Set text color
void FXFoldingList::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set select background color
void FXFoldingList::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXFoldingList::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    update();
    }
  }


// Set line color
void FXFoldingList::setLineColor(FXColor clr){
  if(clr!=lineColor){
    lineColor=clr;
    update();
    }
  }


// Set parent to child indent amount
void FXFoldingList::setIndent(FXint in){
  if(indent!=in){
    indent=in;
    recalc();
    }
  }


// Change list style
void FXFoldingList::setListStyle(FXuint style){
  FXuint opts=(options&~FOLDINGLIST_MASK) | (style&FOLDINGLIST_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    }
  }


// Get list style
FXuint FXFoldingList::getListStyle() const {
  return (options&FOLDINGLIST_MASK);
  }


// Save data
void FXFoldingList::save(FXStream& store) const {
  FXScrollArea::save(store);
  store << header;
  store << firstitem;
  store << lastitem;
  store << anchoritem;
  store << currentitem;
  store << extentitem;
  store << font;
  store << textColor;
  store << selbackColor;
  store << seltextColor;
  store << lineColor;
  store << treeWidth;
  store << treeHeight;
  store << visible;
  store << indent;
  store << help;
  }


// Load data
void FXFoldingList::load(FXStream& store){
  FXScrollArea::load(store);
  store >> header;
  store >> firstitem;
  store >> lastitem;
  store >> anchoritem;
  store >> currentitem;
  store >> extentitem;
  store >> font;
  store >> textColor;
  store >> selbackColor;
  store >> seltextColor;
  store >> lineColor;
  store >> treeWidth;
  store >> treeHeight;
  store >> visible;
  store >> indent;
  store >> help;
  }


// Cleanup
FXFoldingList::~FXFoldingList(){
  getApp()->removeTimeout(this,ID_TIPTIMER);
  getApp()->removeTimeout(this,ID_LOOKUPTIMER);
  clearItems(FALSE);
  header=(FXHeader*)-1L;
  firstitem=(FXFoldingItem*)-1L;
  lastitem=(FXFoldingItem*)-1L;
  anchoritem=(FXFoldingItem*)-1L;
  currentitem=(FXFoldingItem*)-1L;
  extentitem=(FXFoldingItem*)-1L;
  font=(FXFont*)-1L;
  }

}

