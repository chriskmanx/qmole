/********************************************************************************
*                                                                               *
*                          T r e e L i s t   O b j e c t                        *
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
* $Id: FXTreeList.cpp,v 1.167 2006/02/17 03:07:00 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "fxascii.h"
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
#include "FXIcon.h"
#include "FXScrollBar.h"
#include "FXTreeList.h"


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
*/


#define ICON_SPACING        4   // Spacing between parent and child in x direction
#define TEXT_SPACING        4   // Spacing between icon and text
#define SIDE_SPACING        4   // Spacing between side and item
#define DEFAULT_INDENT      8   // Indent between parent and child
#define HALFBOX_SIZE        4   // Half box size
#define BOX_FUDGE           3   // Fudge border around box


#define SELECT_MASK         (TREELIST_SINGLESELECT|TREELIST_BROWSESELECT)
#define TREELIST_MASK       (SELECT_MASK|TREELIST_AUTOSELECT|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|TREELIST_ROOT_BOXES)


using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXTreeItem,FXObject,NULL,0)



// Draw item
void FXTreeItem::draw(const FXTreeList* list,FXDC& dc,FXint xx,FXint yy,FXint,FXint hh) const {
  register FXIcon *icon=(state&OPENED)?openIcon:closedIcon;
  register FXFont *font=list->getFont();
  register FXint th=0,tw=0,ih=0,iw=0;
  xx+=SIDE_SPACING/2;
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    dc.drawIcon(icon,xx,yy+(hh-ih)/2);
    xx+=ICON_SPACING+iw;
    }
  if(!label.empty()){
    tw=4+font->getTextWidth(label.text(),label.length());
    th=4+font->getFontHeight();
    yy+=(hh-th)/2;
    if(isSelected()){
      dc.setForeground(list->getSelBackColor());
      dc.fillRectangle(xx,yy,tw,th);
      }
    if(hasFocus()){
      dc.drawFocusRectangle(xx+1,yy+1,tw-2,th-2);
      }
    if(!isEnabled())
      dc.setForeground(makeShadowColor(list->getBackColor()));
    else if(isSelected())
      dc.setForeground(list->getSelTextColor());
    else
      dc.setForeground(list->getTextColor());
    dc.drawText(xx+2,yy+font->getFontAscent()+2,label);
    }
  }


// See if item got hit, and where:- 1 is icon, 2 is text
FXint FXTreeItem::hitItem(const FXTreeList* list,FXint xx,FXint yy) const {
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
    tw=4+font->getTextWidth(label.text(),label.length());
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
  if(ix<=xx && iy<=yy && xx<ix+iw && yy<iy+ih) return 1;

  // In text?
  if(tx<=xx && ty<=yy && xx<tx+tw && yy<ty+th) return 2;

  // Outside
  return 0;
  }


// Set or kill focus
void FXTreeItem::setFocus(FXbool focus){
  if(focus) state|=FOCUS; else state&=~FOCUS;
  }

// Select or deselect item
void FXTreeItem::setSelected(FXbool selected){
  if(selected) state|=SELECTED; else state&=~SELECTED;
  }

// Set item opened
void FXTreeItem::setOpened(FXbool opened){
  if(opened) state|=OPENED; else state&=~OPENED;
  }

// Set item expanded
void FXTreeItem::setExpanded(FXbool expanded){
  if(expanded) state|=EXPANDED; else state&=~EXPANDED;
  }

// Enable or disable the item
void FXTreeItem::setEnabled(FXbool enabled){
  if(enabled) state&=~DISABLED; else state|=DISABLED;
  }

// Icon is draggable
void FXTreeItem::setDraggable(FXbool draggable){
  if(draggable) state|=DRAGGABLE; else state&=~DRAGGABLE;
  }


// Change item's text label
void FXTreeItem::setText(const FXString& txt){
  label=txt;
  }


// Change open icon
void FXTreeItem::setOpenIcon(FXIcon* icon,FXbool owned){
  if(openIcon && (state&OPENICONOWNED)){
    if(openIcon!=icon) delete openIcon;
    state&=~OPENICONOWNED;
    }
  openIcon=icon;
  if(openIcon && owned){
    state|=OPENICONOWNED;
    }
  }


// Change closed icon
void FXTreeItem::setClosedIcon(FXIcon* icon,FXbool owned){
  if(closedIcon && (state&CLOSEDICONOWNED)){
    if(closedIcon!=icon) delete closedIcon;
    state&=~CLOSEDICONOWNED;
    }
  closedIcon=icon;
  if(closedIcon && owned){
    state|=CLOSEDICONOWNED;
    }
  }


// Change has items flag
void FXTreeItem::setHasItems(FXbool flag){
  if(flag) state|=HASITEMS; else state&=~HASITEMS;
  }


// Create icon
void FXTreeItem::create(){
  if(openIcon) openIcon->create();
  if(closedIcon) closedIcon->create();
  }


// Destroy icon
void FXTreeItem::destroy(){
  if((state&OPENICONOWNED) && openIcon) openIcon->destroy();
  if((state&CLOSEDICONOWNED) && closedIcon) closedIcon->destroy();
  }


// Detach from icon resource
void FXTreeItem::detach(){
  if(openIcon) openIcon->detach();
  if(closedIcon) closedIcon->detach();
  }


// Get number of child items
FXint FXTreeItem::getNumChildren() const {
  register FXTreeItem *item=first;
  register FXint n=0;
  while(item){item=item->next;n++;}
  return n;
  }



// Get item (logically) below this one
FXTreeItem* FXTreeItem::getBelow() const {
  register FXTreeItem* item=(FXTreeItem*)this;
  if(first) return first;
  while(!item->next && item->parent) item=item->parent;
  return item->next;
  }


// Get item (logically) above this one
FXTreeItem* FXTreeItem::getAbove() const {
  register FXTreeItem* item=prev;
  if(!item) return parent;
  while(item->last) item=item->last;
  return item;
  }


// Return true if child of parent item
FXbool FXTreeItem::isChildOf(const FXTreeItem* item) const {
  register const FXTreeItem* child=this;
  while(child){ child=child->parent; if(child==item) return TRUE; }
  return FALSE;
  }


// Return true if parent of child item
FXbool FXTreeItem::isParentOf(const FXTreeItem* item) const {
  register const FXTreeItem* child=item;
  while(child){ child=child->parent; if(child==this) return TRUE; }
  return FALSE;
  }


// Get item width
FXint FXTreeItem::getWidth(const FXTreeList* list) const {
  register FXint w=0,oiw=0,ciw=0;
  if(openIcon) oiw=openIcon->getWidth();
  if(closedIcon) ciw=closedIcon->getWidth();
  w=FXMAX(oiw,ciw);
  if(!label.empty()){
    if(w) w+=ICON_SPACING;
    w+=4+list->getFont()->getTextWidth(label.text(),label.length());
    }
  return SIDE_SPACING+w;
  }


// Get item height
FXint FXTreeItem::getHeight(const FXTreeList* list) const {
  register FXint th=0,oih=0,cih=0;
  if(openIcon) oih=openIcon->getHeight();
  if(closedIcon) cih=closedIcon->getHeight();
  if(!label.empty()) th=4+list->getFont()->getFontHeight();
  return FXMAX3(th,oih,cih);
  }


// Save data
void FXTreeItem::save(FXStream& store) const {
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
void FXTreeItem::load(FXStream& store){
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
FXTreeItem::~FXTreeItem(){
  if(state&OPENICONOWNED) delete openIcon;
  if(state&CLOSEDICONOWNED) delete closedIcon;
  parent=(FXTreeItem*)-1L;
  prev=(FXTreeItem*)-1L;
  next=(FXTreeItem*)-1L;
  first=(FXTreeItem*)-1L;
  last=(FXTreeItem*)-1L;
  openIcon=(FXIcon*)-1L;
  closedIcon=(FXIcon*)-1L;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXTreeList) FXTreeListMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXTreeList::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXTreeList::onMotion),
  FXMAPFUNC(SEL_TIMEOUT,FXWindow::ID_AUTOSCROLL,FXTreeList::onAutoScroll),
  FXMAPFUNC(SEL_TIMEOUT,FXTreeList::ID_TIPTIMER,FXTreeList::onTipTimer),
  FXMAPFUNC(SEL_TIMEOUT,FXTreeList::ID_LOOKUPTIMER,FXTreeList::onLookupTimer),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXTreeList::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXTreeList::onLeftBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXTreeList::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXTreeList::onRightBtnRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXTreeList::onUngrabbed),
  FXMAPFUNC(SEL_KEYPRESS,0,FXTreeList::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXTreeList::onKeyRelease),
  FXMAPFUNC(SEL_ENTER,0,FXTreeList::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXTreeList::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXTreeList::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXTreeList::onFocusOut),
  FXMAPFUNC(SEL_CLICKED,0,FXTreeList::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXTreeList::onDoubleClicked),
  FXMAPFUNC(SEL_TRIPLECLICKED,0,FXTreeList::onTripleClicked),
  FXMAPFUNC(SEL_COMMAND,0,FXTreeList::onCommand),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXTreeList::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXTreeList::onQueryHelp),
  };


// Object implementation
FXIMPLEMENT(FXTreeList,FXScrollArea,FXTreeListMap,ARRAYNUMBER(FXTreeListMap))


/*******************************************************************************/


// Tree List
FXTreeList::FXTreeList(){
  flags|=FLAG_ENABLED;
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
FXTreeList::FXTreeList(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
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
void FXTreeList::create(){
  register FXTreeItem *item=firstitem;
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
void FXTreeList::detach(){
  register FXTreeItem *item=firstitem;
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
bool FXTreeList::canFocus() const { return true; }


// Into focus chain
void FXTreeList::setFocus(){
  FXScrollArea::setFocus();
  setDefault(TRUE);
  }


// Out of focus chain
void FXTreeList::killFocus(){
  FXScrollArea::killFocus();
  setDefault(MAYBE);
  }


// Get default width
FXint FXTreeList::getDefaultWidth(){
  return FXScrollArea::getDefaultWidth();
  }


// Get default height
FXint FXTreeList::getDefaultHeight(){
  if(visible) return visible*(4+font->getFontHeight());
  return FXScrollArea::getDefaultHeight();
  }


// Propagate size change
void FXTreeList::recalc(){
  FXScrollArea::recalc();
  flags|=FLAG_RECALC;
  cursoritem=NULL;
  }


// List is multiple of nitems
void FXTreeList::setNumVisible(FXint nvis){
  if(nvis<0) nvis=0;
  if(visible!=nvis){
    visible=nvis;
    recalc();
    }
  }


// Get number of toplevel items
FXint FXTreeList::getNumItems() const {
  register FXTreeItem *item=firstitem;
  register FXint n=0;
  while(item){
    item=item->next;
    n++;
    }
  return n;
  }


// Recompute interior
void FXTreeList::recompute(){
  register FXTreeItem* item;
  register FXint x,y,w,h;
  x=y=0;
  treeWidth=0;
  treeHeight=0;
  item=firstitem;
  if(options&TREELIST_ROOT_BOXES) x+=(4+indent);
  while(item){
    item->x=x;
    item->y=y;
    w=item->getWidth(this);
    h=item->getHeight(this);
    if(x+w>treeWidth) treeWidth=x+w;
    y+=h;
    if(item->first && ((options&TREELIST_AUTOSELECT) || item->isExpanded())){
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
  treeHeight=y;
  flags&=~FLAG_RECALC;
  }


// Determine content width of tree list
FXint FXTreeList::getContentWidth(){
  if(flags&FLAG_RECALC) recompute();
  return treeWidth;
  }


// Determine content height of tree list
FXint FXTreeList::getContentHeight(){
  if(flags&FLAG_RECALC) recompute();
  return treeHeight;
  }


// Recalculate layout
void FXTreeList::layout(){

  // Calculate contents
  FXScrollArea::layout();

  // Set line size based on item size
  if(firstitem){
    vertical->setLine(firstitem->getHeight(this));
    horizontal->setLine(firstitem->getWidth(this)/10);
    }

  // Force repaint
  update();

  // We were supposed to make this item viewable
  if(viewableitem){
    makeItemVisible(viewableitem);
    }

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Set item text
void FXTreeList::setItemText(FXTreeItem* item,const FXString& text){
  if(item==NULL){ fxerror("%s::setItemText: NULL argument.\n",getClassName()); }
  if(item->getText()!=text){
    item->setText(text);
    recalc();
    }
  }


// Get item text
FXString FXTreeList::getItemText(const FXTreeItem* item) const {
  if(item==NULL){ fxerror("%s::getItemText: NULL argument.\n",getClassName()); }
  return item->getText();
  }


// Set item open icon
void FXTreeList::setItemOpenIcon(FXTreeItem* item,FXIcon* icon,FXbool owned){
  if(item==NULL){ fxerror("%s::setItemOpenIcon: NULL argument.\n",getClassName()); }
  if(item->getOpenIcon()!=icon) recalc();
  item->setOpenIcon(icon,owned);
  }


// Get item open icon
FXIcon* FXTreeList::getItemOpenIcon(const FXTreeItem* item) const {
  if(item==NULL){ fxerror("%s::getItemOpenIcon: NULL argument.\n",getClassName()); }
  return item->getOpenIcon();
  }


// Set item closed icon
void FXTreeList::setItemClosedIcon(FXTreeItem* item,FXIcon* icon,FXbool owned){
  if(item==NULL){ fxerror("%s::setItemClosedIcon: NULL argument.\n",getClassName()); }
  if(item->getClosedIcon()!=icon) recalc();
  item->setClosedIcon(icon,owned);
  }


// Get item closed icon
FXIcon* FXTreeList::getItemClosedIcon(const FXTreeItem* item) const {
  if(item==NULL){ fxerror("%s::getItemClosedIcon: NULL argument.\n",getClassName()); }
  return item->getClosedIcon();
  }


// Set item data
void FXTreeList::setItemData(FXTreeItem* item,void* ptr) const {
  if(item==NULL){ fxerror("%s::setItemData: NULL argument.\n",getClassName()); }
  item->setData(ptr);
  }


// Get item data
void* FXTreeList::getItemData(const FXTreeItem* item) const {
  if(item==NULL){ fxerror("%s::getItemData: NULL argument.\n",getClassName()); }
  return item->getData();
  }


// True if item is selected
FXbool FXTreeList::isItemSelected(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemSelected: NULL argument.\n",getClassName()); }
  return item->isSelected();
  }


// True if item is current
FXbool FXTreeList::isItemCurrent(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemCurrent: NULL argument.\n",getClassName()); }
  return currentitem==item;
  }


// Check if item is expanded
FXbool FXTreeList::isItemExpanded(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemExpanded: NULL argument.\n",getClassName()); }
  return (options&TREELIST_AUTOSELECT) || item->isExpanded();
  }


// Is item a leaf item
FXbool FXTreeList::isItemLeaf(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemLeaf: NULL argument.\n",getClassName()); }
  return item->first==NULL;
  }


// Check if item is enabled
FXbool FXTreeList::isItemEnabled(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemEnabled: NULL argument.\n",getClassName()); }
  return item->isEnabled();
  }


// Check item is open
FXbool FXTreeList::isItemOpened(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemOpen: NULL argument.\n",getClassName()); }
  return item->isOpened();
  }


// True if item (partially) visible
FXbool FXTreeList::isItemVisible(const FXTreeItem* item) const {
  if(!item){ fxerror("%s::isItemVisible: NULL argument.\n",getClassName()); }
  return 0<pos_y+item->y+item->getHeight(this) && pos_y+item->y<viewport_h;
  }


// Make item fully visible
void FXTreeList::makeItemVisible(FXTreeItem* item){
  register FXTreeItem *par;
  register FXint x,y,h,w;
  if(item){

    // Remember for later
    viewableitem=item;

    // Expand parents of this node
    if(!(options&TREELIST_AUTOSELECT)){
      for(par=item->parent; par; par=par->parent){
        expandTree(par);
        }
      }

    // Was realized
    if(xid){

      // Force layout if dirty
      if(flags&FLAG_RECALC) layout();

      x=pos_x;
      y=pos_y;
      w=item->getWidth(this);
      h=item->getHeight(this);

      // Horizontal scroll to ensure visibility; the +-box, if shown, should also be visible
      if(viewport_w<=x+item->x+w) x=viewport_w-item->x-w;
      if((options&(TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES)) && (item->parent || (options&TREELIST_ROOT_BOXES))){
        if(x+item->x-indent-HALFBOX_SIZE<=0) x=-item->x+indent+HALFBOX_SIZE;
        }
      else{
        if(x+item->x<=0) x=-item->x;
        }

      // Vertical scroll to ensure visibility
      if(viewport_h<=y+item->y+h) y=viewport_h-item->y-h;
      if(y+item->y<=0) y=-item->y;

      // Scroll into view
      setPosition(x,y);

      // Done it
      viewableitem=NULL;
      }
    }
  }


// Get item at position x,y
FXTreeItem* FXTreeList::getItemAt(FXint,FXint y) const {
  register FXTreeItem* item=firstitem;
  register FXint ix,iy,ih;
  ix=pos_x;
  iy=pos_y;
  if(options&TREELIST_ROOT_BOXES) ix+=(4+indent);
  while(item && iy<=y){
    ih=item->getHeight(this);
    if(y<iy+ih) return item;
    iy+=ih;
    if(item->first && ((options&TREELIST_AUTOSELECT) || item->isExpanded())){
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
FXint FXTreeList::hitItem(const FXTreeItem* item,FXint x,FXint y) const {
  register FXint ix,iy,ih,xh,yh,hit=0;
  if(item){
    x-=pos_x;
    y-=pos_y;
    ix=item->x;
    iy=item->y;
    ih=item->getHeight(this);
    if(iy<=y && y<iy+ih){
      if((options&TREELIST_SHOWS_BOXES) && (item->hasItems() || item->getFirst())){
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
void FXTreeList::updateItem(FXTreeItem* item) const {
  if(item) update(0,pos_y+item->y,width,item->getHeight(this));
  }


// Enable one item
FXbool FXTreeList::enableItem(FXTreeItem* item){
  if(!item){ fxerror("%s::enableItem: NULL argument.\n",getClassName()); }
  if(!item->isEnabled()){
    item->setEnabled(TRUE);
    updateItem(item);
    return TRUE;
    }
  return FALSE;
  }


// Disable one item
FXbool FXTreeList::disableItem(FXTreeItem* item){
  if(!item){ fxerror("%s::disableItem: NULL argument.\n",getClassName()); }
  if(item->isEnabled()){
    item->setEnabled(FALSE);
    updateItem(item);
    return TRUE;
    }
  return FALSE;
  }


// Select one item
FXbool FXTreeList::selectItem(FXTreeItem* item,FXbool notify){
  if(!item){ fxerror("%s::selectItem: NULL argument.\n",getClassName()); }
  if(!item->isSelected()){
    switch(options&SELECT_MASK){
      case TREELIST_SINGLESELECT:
      case TREELIST_BROWSESELECT:
        killSelection(notify);
      case TREELIST_EXTENDEDSELECT:
      case TREELIST_MULTIPLESELECT:
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
FXbool FXTreeList::deselectItem(FXTreeItem* item,FXbool notify){
  if(!item){ fxerror("%s::deselectItem: NULL argument.\n",getClassName()); }
  if(item->isSelected()){
    switch(options&SELECT_MASK){
      case TREELIST_EXTENDEDSELECT:
      case TREELIST_MULTIPLESELECT:
      case TREELIST_SINGLESELECT:
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
FXbool FXTreeList::toggleItem(FXTreeItem* item,FXbool notify){
  if(!item){ fxerror("%s::toggleItem: NULL argument.\n",getClassName()); }
  switch(options&SELECT_MASK){
    case TREELIST_BROWSESELECT:
      if(!item->isSelected()){
        killSelection(notify);
        item->setSelected(TRUE);
        updateItem(item);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)item);}
        }
      break;
    case TREELIST_SINGLESELECT:
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
    case TREELIST_EXTENDEDSELECT:
    case TREELIST_MULTIPLESELECT:
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
FXbool FXTreeList::extendSelection(FXTreeItem* item,FXbool notify){
  register FXTreeItem *it,*i1,*i2,*i3;
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
FXbool FXTreeList::killSelection(FXbool notify){
  register FXTreeItem *item=firstitem;
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
FXbool FXTreeList::openItem(FXTreeItem* item,FXbool notify){
  if(item==NULL){ fxerror("%s::openItem: NULL argument.\n",getClassName()); }
  if(!item->isOpened()){
    item->setOpened(TRUE);
    updateItem(item);
    if(notify && target){target->tryHandle(this,FXSEL(SEL_OPENED,message),(void*)item);}
    return TRUE;
    }
  return FALSE;
  }


// Close item
FXbool FXTreeList::closeItem(FXTreeItem* item,FXbool notify){
  if(item==NULL){ fxerror("%s::closeItem: NULL argument.\n",getClassName()); }
  if(item->isOpened()){
    item->setOpened(FALSE);
    updateItem(item);
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CLOSED,message),(void*)item);}
    return TRUE;
    }
  return FALSE;
  }


// Collapse all subtrees under item
FXbool FXTreeList::collapseTree(FXTreeItem* tree,FXbool notify){
  if(tree==NULL){ fxerror("%s::collapseTree: NULL argument.\n",getClassName()); }
  if(tree->isExpanded()){
    tree->setExpanded(FALSE);
    if(!(options&TREELIST_AUTOSELECT)){     // In autoselect, already shown as expanded!
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
FXbool FXTreeList::expandTree(FXTreeItem* tree,FXbool notify){
  if(tree==NULL){ fxerror("%s::expandTree: NULL argument.\n",getClassName()); }
  if(!tree->isExpanded()){
    tree->setExpanded(TRUE);
    if(!(options&TREELIST_AUTOSELECT)){     // In autoselect, already shown as expanded!
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
long FXTreeList::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onEnter(sender,sel,ptr);
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
  cursoritem=NULL;
  return 1;
  }


// Stop motion timer when leaving window
long FXTreeList::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onLeave(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_TIPTIMER);
  cursoritem=NULL;
  return 1;
  }


// We timed out, i.e. the user didn't move for a while
long FXTreeList::onTipTimer(FXObject*,FXSelector,void*){
  FXTRACE((200,"%s::onTipTimer %p\n",getClassName(),this));
  flags|=FLAG_TIP;
  return 1;
  }


// We were asked about tip text
long FXTreeList::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !(options&TREELIST_AUTOSELECT)){   // No tip when autoselect!
    FXint x,y; FXuint buttons;
    getCursorPosition(x,y,buttons);
    FXTreeItem *item=getItemAt(x,y);
    if(item){
      FXString string=item->getText();
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
      return 1;
      }
    }
  return 0;
  }


// We were asked about status text
long FXTreeList::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if(!help.empty() && (flags&FLAG_HELP)){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Gained focus
long FXTreeList::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusIn(sender,sel,ptr);
  if(currentitem){
    currentitem->setFocus(TRUE);
    updateItem(currentitem);
    }
  return 1;
  }


// Lost focus
long FXTreeList::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusOut(sender,sel,ptr);
  if(currentitem){
    currentitem->setFocus(FALSE);
    updateItem(currentitem);
    }
  return 1;
  }


// Draw item list
long FXTreeList::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTreeItem* item=firstitem;
  FXTreeItem* p;
  FXint yh,xh,x,y,w,h,xp,hh;
  FXDCWindow dc(this,event);
  dc.setFont(font);
  x=pos_x;
  y=pos_y;
  if(options&TREELIST_ROOT_BOXES) x+=(4+indent);
  while(item && y<event->rect.y+event->rect.h){
    w=item->getWidth(this);
    h=item->getHeight(this);
    if(event->rect.y<=y+h){

      // Draw item
      dc.setForeground(backColor);
      dc.fillRectangle(0,y,width,h);
      item->draw(this,dc,x,y,w,h);

      // Show other paraphernalia such as dotted lines and expand-boxes
      if((options&(TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES)) && (item->parent || (options&TREELIST_ROOT_BOXES))){
        hh=h/2;
        yh=y+hh;
        xh=x-indent+(SIDE_SPACING/2);
        dc.setForeground(lineColor);
        dc.setBackground(backColor);
        dc.setStipple(STIPPLE_GRAY,pos_x&1,pos_y&1);
        if(options&TREELIST_SHOWS_LINES){                   // Connect items with lines
          p=item->parent;
          xp=xh;
          dc.setFillStyle(FILL_OPAQUESTIPPLED);
          while(p){
            xp-=(indent+p->getHeight(this)/2);
            if(p->next) dc.fillRectangle(xp,y,1,h);
            p=p->parent;
            }
          if((options&TREELIST_SHOWS_BOXES) && (item->hasItems() || item->getFirst())){
            if(item->prev || item->parent) dc.fillRectangle(xh,y,1,yh-y-HALFBOX_SIZE);
            if(item->next) dc.fillRectangle(xh,yh+HALFBOX_SIZE,1,y+h-yh-HALFBOX_SIZE);
            }
          else{
            if(item->prev || item->parent) dc.fillRectangle(xh,y,1,hh);
            if(item->next) dc.fillRectangle(xh,yh,1,h);
            dc.fillRectangle(xh,yh,x+(SIDE_SPACING/2)-2-xh,1);
            }
          dc.setFillStyle(FILL_SOLID);
          }

        // Boxes before items for expand/collapse of item
        if((options&TREELIST_SHOWS_BOXES) && (item->hasItems() || item->getFirst())){
          dc.setFillStyle(FILL_OPAQUESTIPPLED);
          dc.fillRectangle(xh+4,yh,(SIDE_SPACING/2)-2,1);
          dc.setFillStyle(FILL_SOLID);
          dc.drawRectangle(xh-HALFBOX_SIZE,yh-HALFBOX_SIZE,HALFBOX_SIZE+HALFBOX_SIZE,HALFBOX_SIZE+HALFBOX_SIZE);
          dc.setForeground(textColor);
          dc.fillRectangle(xh-HALFBOX_SIZE+2,yh,HALFBOX_SIZE+HALFBOX_SIZE-3,1);
          if(!(options&TREELIST_AUTOSELECT) && !item->isExpanded()){
            dc.fillRectangle(xh,yh-HALFBOX_SIZE+2,1,HALFBOX_SIZE+HALFBOX_SIZE-3);
            }
          }
        }
      }

    y+=h;

    // Move on to the next item
    if(item->first && ((options&TREELIST_AUTOSELECT) || item->isExpanded())){
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
long FXTreeList::onLookupTimer(FXObject*,FXSelector,void*){
  lookup=FXString::null;
  return 1;
  }


// Key Press
long FXTreeList::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTreeItem *item=currentitem;
  FXTreeItem *succ;
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
          while(succ->last && ((options&TREELIST_AUTOSELECT) || succ->isExpanded())) succ=succ->last;
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
        if(succ->first && ((options&TREELIST_AUTOSELECT) || succ->isExpanded())){
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
          while(item->last && ((options&TREELIST_AUTOSELECT) || item->isExpanded())) item=item->last;
          }
        else if(item->parent){
          item=item->parent;
          }
        }
      goto hop;
    case KEY_Down:                        // Move down
    case KEY_KP_Down:
      if(item){
        if(item->first && ((options&TREELIST_AUTOSELECT) || item->isExpanded())){
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
        if(!(options&TREELIST_AUTOSELECT) && !item->isExpanded() && (item->hasItems() || item->getFirst())){
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
        if(!(options&TREELIST_AUTOSELECT) && item->isExpanded() && (item->hasItems() || item->getFirst())){
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
        if(item->last && ((options&TREELIST_AUTOSELECT) || item->isExpanded())){
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
        if((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT){
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
          case TREELIST_EXTENDEDSELECT:
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
          case TREELIST_MULTIPLESELECT:
          case TREELIST_SINGLESELECT:
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
	if((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT){
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
long FXTreeList::onKeyRelease(FXObject*,FXSelector,void* ptr){
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
long FXTreeList::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTreeItem *item;
  FXint xx,yy;

  // Scroll the content
  FXScrollArea::onAutoScroll(sender,sel,ptr);

  // Drag and drop mode
  if(flags&FLAG_DODRAG){
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }

  // In autoselect mode, stop scrolling when mouse outside window
  if((flags&FLAG_PRESSED) || (options&TREELIST_AUTOSELECT)){

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
      if((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT){
        state=FALSE;
        extendSelection(item,TRUE);
        }
      }
    return 1;
    }
  return 0;
  }


// Mouse motion
long FXTreeList::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTreeItem *oldcursoritem=cursoritem;
  FXuint flg=flags;
  FXTreeItem *item;

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
  if((flags&FLAG_PRESSED) || (options&TREELIST_AUTOSELECT)){

    // Start auto scrolling?
    if(startAutoScroll(event,FALSE)) return 1;

    // Find item
    item=getItemAt(event->win_x,event->win_y);

    // Got an item different from before
    if(item && item!=currentitem){

      // Make it the current item
      setCurrentItem(item,TRUE);

      // Extend the selection
      if((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT){
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
long FXTreeList::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTreeItem *item;
  FXint code;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;

    // First chance callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;

    // Not autoselect mode
    if(options&TREELIST_AUTOSELECT) return 1;

    // Locate item
    item=getItemAt(event->win_x,event->win_y);

    // No item
    if(item==NULL){
      if((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT){
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
      case TREELIST_EXTENDEDSELECT:
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
      case TREELIST_MULTIPLESELECT:
      case TREELIST_SINGLESELECT:
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
long FXTreeList::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
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
    if(!(flg&FLAG_PRESSED) && !(options&TREELIST_AUTOSELECT)) return 1;

    // Was dragging
    if(flg&FLAG_DODRAG){
      handle(this,FXSEL(SEL_ENDDRAG,0),ptr);
      return 1;
      }

    // Select only enabled item
    switch(options&SELECT_MASK){
      case TREELIST_EXTENDEDSELECT:
        if(currentitem && currentitem->isEnabled()){
          if(event->state&CONTROLMASK){
            if(state) deselectItem(currentitem,TRUE);
            }
          else if(!(event->state&SHIFTMASK)){
            if(state){ killSelection(TRUE); selectItem(currentitem,TRUE); }
            }
          }
        break;
      case TREELIST_MULTIPLESELECT:
      case TREELIST_SINGLESELECT:
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
long FXTreeList::onRightBtnPress(FXObject*,FXSelector,void* ptr){
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
long FXTreeList::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
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
long FXTreeList::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onUngrabbed(sender,sel,ptr);
  flags&=~(FLAG_DODRAG|FLAG_TRYDRAG|FLAG_PRESSED|FLAG_CHANGED|FLAG_SCROLLING);
  flags|=FLAG_UPDATE;
  stopAutoScroll();
  return 1;
  }


// Command message
long FXTreeList::onCommand(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
  }


// Clicked in list
long FXTreeList::onClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr);
  }


// Double clicked in list; ptr may or may not point to an item
long FXTreeList::onDoubleClicked(FXObject*,FXSelector,void* ptr){

  // Double click anywhere in the widget
  if(target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr)) return 1;

  // Double click on an item
  if(ptr){
    if(isItemExpanded((FXTreeItem*)ptr))
      collapseTree((FXTreeItem*)ptr,TRUE);
    else
      expandTree((FXTreeItem*)ptr,TRUE);
    }
  return 0;
  }


// Triple clicked in list; ptr may or may not point to an item
long FXTreeList::onTripleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
  }


// Sort items in ascending order
FXint FXTreeList::ascending(const FXTreeItem* a,const FXTreeItem* b){
  return compare(a->getText(),b->getText());
  }


// Sort items in descending order
FXint FXTreeList::descending(const FXTreeItem* a,const FXTreeItem* b){
  return compare(b->getText(),a->getText());
  }


// Sort ascending order, case insensitive
FXint FXTreeList::ascendingCase(const FXTreeItem* a,const FXTreeItem* b){
  return comparecase(a->getText(),b->getText());
  }


// Sort descending order, case insensitive
FXint FXTreeList::descendingCase(const FXTreeItem* a,const FXTreeItem* b){
  return comparecase(b->getText(),a->getText());
  }


// Sort items
void FXTreeList::sort(FXTreeItem*& f1,FXTreeItem*& t1,FXTreeItem*& f2,FXTreeItem*& t2,int n){
  FXTreeItem *ff1,*tt1,*ff2,*tt2,*q;
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


// Sort root items
void FXTreeList::sortRootItems(){
  if(sortfunc){
    FXTreeItem* f=firstitem;
    FXTreeItem* l=lastitem;
    sort(firstitem,lastitem,f,l,getNumItems());
    recalc();
    }
  }


// Sort child items
void FXTreeList::sortChildItems(FXTreeItem* item){
  if(sortfunc){
    FXTreeItem* f=item->first;
    FXTreeItem* l=item->last;
    sort(item->first,item->last,f,l,item->getNumChildren());
    if(item->isExpanded()) recalc();     // No need to recalc if it ain't visible!
    }
  }


// Sort all items recursively
void FXTreeList::sortItems(){
  register FXTreeItem *item;
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
void FXTreeList::setCurrentItem(FXTreeItem* item,FXbool notify){
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
  if((options&SELECT_MASK)==TREELIST_BROWSESELECT && currentitem && currentitem->isEnabled()){
    selectItem(currentitem,notify);
    }
  }


// Set anchor item
void FXTreeList::setAnchorItem(FXTreeItem* item){
  anchoritem=item;
  extentitem=item;
  }



// Create item
FXTreeItem* FXTreeList::createItem(const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr){
  return new FXTreeItem(text,oi,ci,ptr);
  }


// Insert item under father before other item
FXTreeItem* FXTreeList::insertItem(FXTreeItem* other,FXTreeItem* father,FXTreeItem* item,FXbool notify){
  register FXTreeItem* olditem=currentitem;

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
    if((options&SELECT_MASK)==TREELIST_BROWSESELECT && currentitem->isEnabled()){
      selectItem(currentitem,notify);
      }
    }

  // Redo layout
  recalc();
  return item;
  }


// Insert item under father before other item
FXTreeItem* FXTreeList::insertItem(FXTreeItem* other,FXTreeItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  return insertItem(other,father,createItem(text,oi,ci,ptr),notify);
  }


// Append item under father
FXTreeItem* FXTreeList::appendItem(FXTreeItem* father,FXTreeItem* item,FXbool notify){
  return insertItem(NULL,father,item,notify);
  }


// Append item under father
FXTreeItem* FXTreeList::appendItem(FXTreeItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  return insertItem(NULL,father,createItem(text,oi,ci,ptr),notify);
  }


// Prepend item under father
FXTreeItem* FXTreeList::prependItem(FXTreeItem* father,FXTreeItem* item,FXbool notify){
  return insertItem(father?father->first:firstitem,father,item,notify);
  }

// Prepend item under father
FXTreeItem* FXTreeList::prependItem(FXTreeItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  return insertItem(father?father->first:firstitem,father,createItem(text,oi,ci,ptr),notify);
  }


// Fill list by appending items from array of strings
FXint FXTreeList::fillItems(FXTreeItem* father,const FXchar** strings,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  register FXint n=0;
  if(strings){
    while(strings[n]){
      appendItem(father,strings[n++],oi,ci,ptr,notify);
      }
    }
  return n;
  }


// Fill list by appending items from newline separated strings
FXint FXTreeList::fillItems(FXTreeItem* father,const FXString& strings,FXIcon* oi,FXIcon* ci,void* ptr,FXbool notify){
  register FXint n=0;
  FXString text;
  while(!(text=strings.section('\n',n)).empty()){
    appendItem(father,text,oi,ci,ptr,notify);
    n++;
    }
  return n;
  }


// Move item under father before other item
FXTreeItem *FXTreeList::moveItem(FXTreeItem* other,FXTreeItem* father,FXTreeItem* item){

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
FXTreeItem* FXTreeList::extractItem(FXTreeItem* item,FXbool notify){
  register FXTreeItem *olditem=currentitem;
  register FXTreeItem *result=item;
  register FXTreeItem *prv;
  register FXTreeItem *nxt;
  register FXTreeItem *par;
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
      if((options&SELECT_MASK)==TREELIST_BROWSESELECT && currentitem->isEnabled()){
        selectItem(currentitem,notify);
        }
      }

    // Redo layout
    recalc();
    }
  return result;
  }


// Remove all siblings from [fm,to]
void FXTreeList::removeItems(FXTreeItem* fm,FXTreeItem* to,FXbool notify){
  register FXTreeItem *olditem=currentitem;
  register FXTreeItem *prv;
  register FXTreeItem *nxt;
  register FXTreeItem *par;
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
        if(currentitem==to){ currentitem=par; if(prv) currentitem=prv; if(nxt) currentitem=nxt; }
        if(extentitem==to){ extentitem=par; if(prv) extentitem=prv; if(nxt) extentitem=nxt; }
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
      if((options&SELECT_MASK)==TREELIST_BROWSESELECT && currentitem->isEnabled()){
        selectItem(currentitem,notify);
        }
      }

    // Redo layout
    recalc();
    }
  }


// Remove node from list
void FXTreeList::removeItem(FXTreeItem* item,FXbool notify){
  removeItems(item,item,notify);
  }


// Remove all items
void FXTreeList::clearItems(FXbool notify){
  removeItems(firstitem,lastitem,notify);
  }


typedef FXint (*FXCompareFunc)(const FXString&,const FXString &,FXint);


// Get item by name
FXTreeItem* FXTreeList::findItem(const FXString& text,FXTreeItem* start,FXuint flgs) const {
  register FXCompareFunc comparefunc;
  register FXTreeItem *item;
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
FXTreeItem* FXTreeList::findItemByData(const void *ptr,FXTreeItem* start,FXuint flgs) const {
  register FXTreeItem *item;
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
void FXTreeList::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL argument.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Change help text
void FXTreeList::setHelpText(const FXString& text){
  help=text;
  }


// Set text color
void FXTreeList::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set select background color
void FXTreeList::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXTreeList::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    update();
    }
  }


// Set line color
void FXTreeList::setLineColor(FXColor clr){
  if(clr!=lineColor){
    lineColor=clr;
    update();
    }
  }


// Set parent to child indent amount
void FXTreeList::setIndent(FXint in){
  if(indent!=in){
    indent=in;
    recalc();
    }
  }


// Change list style
void FXTreeList::setListStyle(FXuint style){
  FXuint opts=(options&~TREELIST_MASK) | (style&TREELIST_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    }
  }


// Get list style
FXuint FXTreeList::getListStyle() const {
  return (options&TREELIST_MASK);
  }


// Save data
void FXTreeList::save(FXStream& store) const {
  FXScrollArea::save(store);
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
void FXTreeList::load(FXStream& store){
  FXScrollArea::load(store);
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
FXTreeList::~FXTreeList(){
  getApp()->removeTimeout(this,ID_TIPTIMER);
  getApp()->removeTimeout(this,ID_LOOKUPTIMER);
  clearItems(FALSE);
  firstitem=(FXTreeItem*)-1L;
  lastitem=(FXTreeItem*)-1L;
  anchoritem=(FXTreeItem*)-1L;
  currentitem=(FXTreeItem*)-1L;
  extentitem=(FXTreeItem*)-1L;
  currentitem=(FXTreeItem*)-1L;
  font=(FXFont*)-1L;
  }

}
