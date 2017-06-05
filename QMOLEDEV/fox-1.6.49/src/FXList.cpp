/********************************************************************************
*                                                                               *
*                            L i s t   O b j e c t                              *
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
* $Id: FXList.cpp,v 1.159.2.3 2006/07/28 00:56:31 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxkeys.h"
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
#include "FXScrollBar.h"
#include "FXList.h"



/*

  Notes:
  - Draw stuff differently when disabled.
  - Move active item by means of focus navigation, or first letters typed.
  - PageUp/PageDn should also change currentitem.
  - Should issue callbacks from keyboard also.
  - Need distinguish various callbacks better:
     - Selection changes (all selected/unselected items or just changes???)
     - Changes of currentitem
     - Clicks on currentitem
  - Return key simulates double click.
  - Autoselect mode selects whatever is under the cursor, and gives callback
    on mouse release.
  - Sortfunc's will be hard to serialize, and hard to write w/o secretly #including
    the FXListItem header!
  - Should replace hard-wired SIDE_SPACING etc by flexible padding; we'll
    do this when FXScrollArea gets derived from FXFrame, so we'll have internal
    padding, and inter-item padding.
  - When in single/browse mode, current item always the one that's selected
  - It may be convenient to have ways to move items around.
  - Need insertSorted() API to add item in the right place based on current
    sort function.
  - Need to add support for justification (similar to FXTableItem).  Perhaps
    multi-line labels too while we're at it.
  - Sort API should change; it should become:

        sortItems(FXListSortFunc func=FXList::ascending)

    this way we will not need to keep track of sort func!

  - Should generate SEL_COMMAND, even if outside of item; this way,
    a single handler can determine item selection(s) or deselection(s).
  - As with FXIconList, it probably shouldn't autoscroll when draggin icons.
  - Even/odd background colors would be nice, but may look funny when
    not all items are same size.
  - Item justification and icon/text relationships would be nice, too.
*/


#define ICON_SPACING             4    // Spacing between icon and label
#define SIDE_SPACING             6    // Left or right spacing between items
#define LINE_SPACING             4    // Line spacing between items

#define SELECT_MASK (LIST_SINGLESELECT|LIST_BROWSESELECT)
#define LIST_MASK   (SELECT_MASK|LIST_AUTOSELECT)

using namespace FX;


/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXListItem,FXObject,NULL,0)


// Draw item
void FXListItem::draw(const FXList* list,FXDC& dc,FXint xx,FXint yy,FXint ww,FXint hh){
  register FXFont *font=list->getFont();
  register FXint ih=0,th=0;
  if(icon) ih=icon->getHeight();
  if(!label.empty()) th=font->getFontHeight();
  if(isSelected())
    dc.setForeground(list->getSelBackColor());
  else
    dc.setForeground(list->getBackColor());     // FIXME maybe paint background in onPaint?
  dc.fillRectangle(xx,yy,ww,hh);
  if(hasFocus()){
    dc.drawFocusRectangle(xx+1,yy+1,ww-2,hh-2);
    }
  xx+=SIDE_SPACING/2;
  if(icon){
    dc.drawIcon(icon,xx,yy+(hh-ih)/2);
    xx+=ICON_SPACING+icon->getWidth();
    }
  if(!label.empty()){
    dc.setFont(font);
    if(!isEnabled())
      dc.setForeground(makeShadowColor(list->getBackColor()));
    else if(isSelected())
      dc.setForeground(list->getSelTextColor());
    else
      dc.setForeground(list->getTextColor());
    dc.drawText(xx,yy+(hh-th)/2+font->getFontAscent(),label);
    }
  }


// See if item got hit, and where: 0 is outside, 1 is icon, 2 is text
FXint FXListItem::hitItem(const FXList* list,FXint xx,FXint yy) const {
  register FXint iw=0,ih=0,tw=0,th=0,ix,iy,tx,ty,h;
  register FXFont *font=list->getFont();
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }
  if(!label.empty()){
    tw=4+font->getTextWidth(label.text(),label.length());
    th=4+font->getFontHeight();
    }
  h=LINE_SPACING+FXMAX(th,ih);
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
void FXListItem::setFocus(FXbool focus){
  if(focus) state|=FOCUS; else state&=~FOCUS;
  }

// Select or deselect item
void FXListItem::setSelected(FXbool selected){
  if(selected) state|=SELECTED; else state&=~SELECTED;
  }


// Enable or disable the item
void FXListItem::setEnabled(FXbool enabled){
  if(enabled) state&=~DISABLED; else state|=DISABLED;
  }


// Icon is draggable
void FXListItem::setDraggable(FXbool draggable){
  if(draggable) state|=DRAGGABLE; else state&=~DRAGGABLE;
  }


// Change item's text label
void FXListItem::setText(const FXString& txt){
  label=txt;
  }


// Change item's icon
void FXListItem::setIcon(FXIcon* icn,FXbool owned){
  if(icon && (state&ICONOWNED)){
    if(icon!=icn) delete icon;
    state&=~ICONOWNED;
    }
  icon=icn;
  if(icon && owned){
    state|=ICONOWNED;
    }
  }


// Create icon
void FXListItem::create(){
  if(icon) icon->create();
  }


// Destroy icon
void FXListItem::destroy(){
  if((state&ICONOWNED) && icon) icon->destroy();
  }


// Detach from icon resource
void FXListItem::detach(){
  if(icon) icon->detach();
  }


// Get width of item
FXint FXListItem::getWidth(const FXList* list) const {
  register FXFont *font=list->getFont();
  register FXint w=0;
  if(icon){
    w=icon->getWidth();
    }
  if(!label.empty()){
    if(w) w+=ICON_SPACING;
    w+=font->getTextWidth(label.text(),label.length());
    }
  return SIDE_SPACING+w;
  }


// Get height of item
FXint FXListItem::getHeight(const FXList* list) const {
  register FXFont *font=list->getFont();
  register FXint th=0,ih=0;
  if(icon){
    ih=icon->getHeight();
    }
  if(!label.empty()){
    th=font->getFontHeight();
    }
  return LINE_SPACING+FXMAX(th,ih);
  }


// Save data
void FXListItem::save(FXStream& store) const {
  FXObject::save(store);
  store << label;
  store << icon;
  store << state;
  }


// Load data
void FXListItem::load(FXStream& store){
  FXObject::load(store);
  store >> label;
  store >> icon;
  store >> state;
  }


// Delete icon if owned
FXListItem::~FXListItem(){
  if(state&ICONOWNED) delete icon;
  icon=(FXIcon*)-1L;
  }


/*******************************************************************************/


// Map
FXDEFMAP(FXList) FXListMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXList::onPaint),
  FXMAPFUNC(SEL_ENTER,0,FXList::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXList::onLeave),
  FXMAPFUNC(SEL_MOTION,0,FXList::onMotion),
  FXMAPFUNC(SEL_TIMEOUT,FXWindow::ID_AUTOSCROLL,FXList::onAutoScroll),
  FXMAPFUNC(SEL_TIMEOUT,FXList::ID_TIPTIMER,FXList::onTipTimer),
  FXMAPFUNC(SEL_TIMEOUT,FXList::ID_LOOKUPTIMER,FXList::onLookupTimer),
  FXMAPFUNC(SEL_UNGRABBED,0,FXList::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXList::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXList::onLeftBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXList::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXList::onRightBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXList::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXList::onKeyRelease),
  FXMAPFUNC(SEL_FOCUSIN,0,FXList::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXList::onFocusOut),
  FXMAPFUNC(SEL_CLICKED,0,FXList::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXList::onDoubleClicked),
  FXMAPFUNC(SEL_TRIPLECLICKED,0,FXList::onTripleClicked),
  FXMAPFUNC(SEL_COMMAND,0,FXList::onCommand),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXList::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXList::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXList::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXList::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXList::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXList,FXScrollArea,FXListMap,ARRAYNUMBER(FXListMap))


// List
FXList::FXList(){
  flags|=FLAG_ENABLED;
  anchor=-1;
  current=-1;
  extent=-1;
  cursor=-1;
  viewable=-1;
  font=(FXFont*)-1L;
  textColor=0;
  selbackColor=0;
  seltextColor=0;
  listWidth=0;
  listHeight=0;
  visible=0;
  sortfunc=NULL;
  grabx=0;
  graby=0;
  state=FALSE;
  }


// List
FXList::FXList(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  anchor=-1;
  current=-1;
  extent=-1;
  cursor=-1;
  viewable=-1;
  font=getApp()->getNormalFont();
  textColor=getApp()->getForeColor();
  selbackColor=getApp()->getSelbackColor();
  seltextColor=getApp()->getSelforeColor();
  listWidth=0;
  listHeight=0;
  visible=0;
  sortfunc=NULL;
  grabx=0;
  graby=0;
  state=FALSE;
  }


// Create window
void FXList::create(){
  register FXint i;
  FXScrollArea::create();
  for(i=0; i<items.no(); i++){items[i]->create();}
  font->create();
  }


// Detach window
void FXList::detach(){
  register FXint i;
  FXScrollArea::detach();
  for(i=0; i<items.no(); i++){items[i]->detach();}
  font->detach();
  }


// Can have focus
bool FXList::canFocus() const { return true; }


// Into focus chain
void FXList::setFocus(){
  FXScrollArea::setFocus();
  setDefault(TRUE);
  }


// Out of focus chain
void FXList::killFocus(){
  FXScrollArea::killFocus();
  setDefault(MAYBE);
  }


// Get default width
FXint FXList::getDefaultWidth(){
  return FXScrollArea::getDefaultWidth();
  }


// Get default height
FXint FXList::getDefaultHeight(){
  if(visible) return visible*(LINE_SPACING+font->getFontHeight());
  return FXScrollArea::getDefaultHeight();
  }


// Propagate size change
void FXList::recalc(){
  FXScrollArea::recalc();
  flags|=FLAG_RECALC;
  cursor=-1;
  }


// List is multiple of nitems
void FXList::setNumVisible(FXint nvis){
  if(nvis<0) nvis=0;
  if(visible!=nvis){
    visible=nvis;
    recalc();
    }
  }


// Recompute interior
void FXList::recompute(){
  register FXint x,y,w,h,i;
  x=0;
  y=0;
  listWidth=0;
  listHeight=0;
  for(i=0; i<items.no(); i++){
    items[i]->x=x;
    items[i]->y=y;
    w=items[i]->getWidth(this);
    h=items[i]->getHeight(this);
    if(w>listWidth) listWidth=w;
    y+=h;
    }
  listHeight=y;
  flags&=~FLAG_RECALC;
  }


// Determine content width of list
FXint FXList::getContentWidth(){
  if(flags&FLAG_RECALC) recompute();
  return listWidth;
  }


// Determine content height of list
FXint FXList::getContentHeight(){
  if(flags&FLAG_RECALC) recompute();
  return listHeight;
  }


// Recalculate layout determines item locations and sizes
void FXList::layout(){

  // Calculate contents
  FXScrollArea::layout();

  // Determine line size for scroll bars
  if(0<items.no()){
    vertical->setLine(items[0]->getHeight(this));
    horizontal->setLine(items[0]->getWidth(this)/10);
    }

  update();

  // We were supposed to make this item viewable
  if(0<=viewable){
    makeItemVisible(viewable);
    }

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Change item text
void FXList::setItemText(FXint index,const FXString& text){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemText: index out of range.\n",getClassName()); }
  if(items[index]->getText()!=text){
    items[index]->setText(text);
    recalc();
    }
  }


// Get item text
FXString FXList::getItemText(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemText: index out of range.\n",getClassName()); }
  return items[index]->getText();
  }


// Set item icon
void FXList::setItemIcon(FXint index,FXIcon* icon,FXbool owned){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemIcon: index out of range.\n",getClassName()); }
  if(items[index]->getIcon()!=icon) recalc();
  items[index]->setIcon(icon,owned);
  }


// Get item icon
FXIcon* FXList::getItemIcon(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemIcon: index out of range.\n",getClassName()); }
  return items[index]->getIcon();
  }


// Set item data
void FXList::setItemData(FXint index,void* ptr){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemData: index out of range.\n",getClassName()); }
  items[index]->setData(ptr);
  }


// Get item data
void* FXList::getItemData(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemData: index out of range.\n",getClassName()); }
  return items[index]->getData();
  }


// True if item is selected
FXbool FXList::isItemSelected(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemSelected: index out of range.\n",getClassName()); }
  return items[index]->isSelected();
  }


// True if item is current
FXbool FXList::isItemCurrent(FXint index) const {
  return (0<=index) && (index==current);
  }


// True if item is enabled
FXbool FXList::isItemEnabled(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemEnabled: index out of range.\n",getClassName()); }
  return items[index]->isEnabled();
  }


// True if item (partially) visible
FXbool FXList::isItemVisible(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemVisible: index out of range.\n",getClassName()); }
  return (0 < (pos_y+items[index]->y+items[index]->getHeight(this))) && ((pos_y+items[index]->y) < viewport_h);
  }


// Make item fully visible
void FXList::makeItemVisible(FXint index){
  register FXint y,h;
  if(0<=index && index<items.no()){

    // Remember for later
    viewable=index;

    // Was realized
    if(xid){

      // Force layout if dirty
      if(flags&FLAG_RECALC) layout();

      y=pos_y;
      h=items[index]->getHeight(this);
      if(viewport_h<=y+items[index]->y+h) y=viewport_h-items[index]->y-h;
      if(y+items[index]->y<=0) y=-items[index]->y;

      // Scroll into view
      setPosition(pos_x,y);

      // Done it
      viewable=-1;
      }
    }
  }


// Return item width
FXint FXList::getItemWidth(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemWidth: index out of range.\n",getClassName()); }
  return items[index]->getWidth(this);
  }


// Return item height
FXint FXList::getItemHeight(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemHeight: index out of range.\n",getClassName()); }
  return items[index]->getHeight(this);
  }


// Get item at position x,y
FXint FXList::getItemAt(FXint,FXint y) const {
  register FXint index;
  y-=pos_y;
  for(index=0; index<items.no(); index++){
    if(items[index]->y<=y && y<items[index]->y+items[index]->getHeight(this)){
      return index;
      }
    }
  return -1;
  }


// Did we hit the item, and which part of it did we hit (0=outside, 1=icon, 2=text)
FXint FXList::hitItem(FXint index,FXint x,FXint y) const {
  FXint ix,iy,hit=0;
  if(0<=index && index<items.no()){
    x-=pos_x;
    y-=pos_y;
    ix=items[index]->x;
    iy=items[index]->y;
    hit=items[index]->hitItem(this,x-ix,y-iy);
    }
  return hit;
  }


// Repaint
void FXList::updateItem(FXint index) const {
  if(0<=index && index<items.no()){
    update(0,pos_y+items[index]->y,viewport_w,items[index]->getHeight(this));
    }
  }


// Enable one item
FXbool FXList::enableItem(FXint index){
  if(index<0 || items.no()<=index){ fxerror("%s::enableItem: index out of range.\n",getClassName()); }
  if(!items[index]->isEnabled()){
    items[index]->setEnabled(TRUE);
    updateItem(index);
    return TRUE;
    }
  return FALSE;
  }


// Disable one item
FXbool FXList::disableItem(FXint index){
  if(index<0 || items.no()<=index){ fxerror("%s::disableItem: index out of range.\n",getClassName()); }
  if(items[index]->isEnabled()){
    items[index]->setEnabled(FALSE);
    updateItem(index);
    return TRUE;
    }
  return FALSE;
  }


// Select one item
FXbool FXList::selectItem(FXint index,FXbool notify){
  if(index<0 || items.no()<=index){ fxerror("%s::selectItem: index out of range.\n",getClassName()); }
  if(!items[index]->isSelected()){
    switch(options&SELECT_MASK){
      case LIST_SINGLESELECT:
      case LIST_BROWSESELECT:
        killSelection(notify);
      case LIST_EXTENDEDSELECT:
      case LIST_MULTIPLESELECT:
        items[index]->setSelected(TRUE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);}
        break;
      }
    return TRUE;
    }
  return FALSE;
  }


// Deselect one item
FXbool FXList::deselectItem(FXint index,FXbool notify){
  if(index<0 || items.no()<=index){ fxerror("%s::deselectItem: index out of range.\n",getClassName()); }
  if(items[index]->isSelected()){
    switch(options&SELECT_MASK){
      case LIST_EXTENDEDSELECT:
      case LIST_MULTIPLESELECT:
      case LIST_SINGLESELECT:
        items[index]->setSelected(FALSE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)index);}
        break;
      }
    return TRUE;
    }
  return FALSE;
  }


// Toggle one item
FXbool FXList::toggleItem(FXint index,FXbool notify){
  if(index<0 || items.no()<=index){ fxerror("%s::toggleItem: index out of range.\n",getClassName()); }
  switch(options&SELECT_MASK){
    case LIST_BROWSESELECT:
      if(!items[index]->isSelected()){
        killSelection(notify);
        items[index]->setSelected(TRUE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);}
        }
      break;
    case LIST_SINGLESELECT:
      if(!items[index]->isSelected()){
        killSelection(notify);
        items[index]->setSelected(TRUE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);}
        }
      else{
        items[index]->setSelected(FALSE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)index);}
        }
      break;
    case LIST_EXTENDEDSELECT:
    case LIST_MULTIPLESELECT:
      if(!items[index]->isSelected()){
        items[index]->setSelected(TRUE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);}
        }
      else{
        items[index]->setSelected(FALSE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)index);}
        }
      break;
    }
  return TRUE;
  }


// Extend selection
FXbool FXList::extendSelection(FXint index,FXbool notify){
  register FXbool changes=FALSE;
  FXint i1,i2,i3,i;
  if(0<=index && 0<=anchor && 0<=extent){

    // Find segments
    i1=index;
    if(anchor<i1){i2=i1;i1=anchor;}
    else{i2=anchor;}
    if(extent<i1){i3=i2;i2=i1;i1=extent;}
    else if(extent<i2){i3=i2;i2=extent;}
    else{i3=extent;}

    // First segment
    for(i=i1; i<i2; i++){

      // item===extent---anchor
      // item===anchor---extent
      if(i1==index){
        if(!items[i]->isSelected()){
          items[i]->setSelected(TRUE);
          updateItem(i);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)i);}
          }
        }

      // extent===anchor---item
      // extent===item-----anchor
      else if(i1==extent){
        if(items[i]->isSelected()){
          items[i]->setSelected(FALSE);
          updateItem(i);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)i);}
          }
        }
      }

    // Second segment
    for(i=i2+1; i<=i3; i++){

      // extent---anchor===item
      // anchor---extent===item
      if(i3==index){
        if(!items[i]->isSelected()){
          items[i]->setSelected(TRUE);
          updateItem(i);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)i);}
          }
        }

      // item-----anchor===extent
      // anchor---item=====extent
      else if(i3==extent){
        if(items[i]->isSelected()){
          items[i]->setSelected(FALSE);
          updateItem(i);
          changes=TRUE;
          if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)i);}
          }
        }
      }
    extent=index;
    }
  return changes;
  }


// Kill selection
FXbool FXList::killSelection(FXbool notify){
  register FXbool changes=FALSE;
  register FXint i;
  for(i=0; i<items.no(); i++){
    if(items[i]->isSelected()){
      items[i]->setSelected(FALSE);
      updateItem(i);
      changes=TRUE;
      if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)i);}
      }
    }
  return changes;
  }


// Update value from a message
long FXList::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrentItem((FXint)(FXival)ptr);
  return 1;
  }


// Obtain value from list
long FXList::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCurrentItem();
  return 1;
  }


// Update value from a message
long FXList::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCurrentItem(*((FXint*)ptr));
  return 1;
  }


// Enter window
long FXList::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onEnter(sender,sel,ptr);
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
  cursor=-1;
  return 1;
  }


// Leave window
long FXList::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onLeave(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_TIPTIMER);
  cursor=-1;
  return 1;
  }


// Gained focus
long FXList::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusIn(sender,sel,ptr);
  if(0<=current){
    FXASSERT(current<items.no());
    items[current]->setFocus(TRUE);
    updateItem(current);
    }
  return 1;
  }


// We timed out, i.e. the user didn't move for a while
long FXList::onTipTimer(FXObject*,FXSelector,void*){
  flags|=FLAG_TIP;
  return 1;
  }


// We were asked about tip text
long FXList::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !(options&LIST_AUTOSELECT) && (0<=cursor)){    // No tip when autoselect!
    FXString string=items[cursor]->getText();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXList::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }



// Lost focus
long FXList::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusOut(sender,sel,ptr);
  if(0<=current){
    FXASSERT(current<items.no());
    items[current]->setFocus(FALSE);
    updateItem(current);
    }
  return 1;
  }


// Draw item list
long FXList::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint i,y,h;

  // Paint items
  y=pos_y;
  for(i=0; i<items.no(); i++){
    h=items[i]->getHeight(this);
    if(event->rect.y<=y+h && y<event->rect.y+event->rect.h){
      items[i]->draw(this,dc,pos_x,y,FXMAX(listWidth,viewport_w),h);
      }
    y+=h;
    }

  // Paint blank area below items
  if(y<event->rect.y+event->rect.h){
    dc.setForeground(backColor);
    dc.fillRectangle(event->rect.x,y,event->rect.w,event->rect.y+event->rect.h-y);
    }
  return 1;
  }


// Zero out lookup string
long FXList::onLookupTimer(FXObject*,FXSelector,void*){
  lookup=FXString::null;
  return 1;
  }


// Key Press
long FXList::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint index=current;
  flags&=~FLAG_TIP;
  if(!isEnabled()) return 0;
  if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
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
      lookup=FXString::null;
      setPosition(pos_x,pos_y+verticalScrollBar()->getPage());
      return 1;
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
      lookup=FXString::null;
      setPosition(pos_x,pos_y-verticalScrollBar()->getPage());
      return 1;
    case KEY_Up:
    case KEY_KP_Up:
      index-=1;
      goto hop;
    case KEY_Down:
    case KEY_KP_Down:
      index+=1;
      goto hop;
    case KEY_Home:
    case KEY_KP_Home:
      index=0;
      goto hop;
    case KEY_End:
    case KEY_KP_End:
      index=items.no()-1;
hop:  lookup=FXString::null;
      if(0<=index && index<items.no()){
        setCurrentItem(index,TRUE);
        makeItemVisible(index);
        if(items[index]->isEnabled()){
          if((options&SELECT_MASK)==LIST_EXTENDEDSELECT){
            if(event->state&SHIFTMASK){
              if(0<=anchor){
                selectItem(anchor,TRUE);
                extendSelection(index,TRUE);
                }
              else{
                selectItem(index,TRUE);
                setAnchorItem(index);
                }
              }
            else if(!(event->state&CONTROLMASK)){
              killSelection(TRUE);
              selectItem(index,TRUE);
              setAnchorItem(index);
              }
            }
          }
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
      if(0<=current && items[current]->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
        }
      return 1;
    case KEY_space:
    case KEY_KP_Space:
      lookup=FXString::null;
      if(0<=current && items[current]->isEnabled()){
        switch(options&SELECT_MASK){
          case LIST_EXTENDEDSELECT:
            if(event->state&SHIFTMASK){
              if(0<=anchor){
                selectItem(anchor,TRUE);
                extendSelection(current,TRUE);
                }
              else{
                selectItem(current,TRUE);
                }
              }
            else if(event->state&CONTROLMASK){
              toggleItem(current,TRUE);
              }
            else{
              killSelection(TRUE);
              selectItem(current,TRUE);
              }
            break;
          case LIST_MULTIPLESELECT:
          case LIST_SINGLESELECT:
            toggleItem(current,TRUE);
            break;
          }
        setAnchorItem(current);
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
      if(0<=current && items[current]->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)(FXival)current);
        }
      return 1;
    case KEY_Return:
    case KEY_KP_Enter:
      lookup=FXString::null;
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)(FXival)current);
      if(0<=current && items[current]->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
        }
      return 1;
    default:
      if((FXuchar)event->text[0]<' ') return 0;
      if(event->state&(CONTROLMASK|ALTMASK)) return 0;
      if(!Ascii::isPrint(event->text[0])) return 0;
      lookup.append(event->text);
      getApp()->addTimeout(this,ID_LOOKUPTIMER,getApp()->getTypingSpeed());
      index=findItem(lookup,current,SEARCH_FORWARD|SEARCH_WRAP|SEARCH_PREFIX);
      if(0<=index){
	setCurrentItem(index,TRUE);
	makeItemVisible(index);
	if((options&SELECT_MASK)==LIST_EXTENDEDSELECT){
	  if(items[index]->isEnabled()){
	    killSelection(TRUE);
	    selectItem(index,TRUE);
	    }
	  }
	setAnchorItem(index);
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
      if(0<=current && items[current]->isEnabled()){
	handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
	}
      return 1;
    }
  return 0;
  }


// Key Release
long FXList::onKeyRelease(FXObject*,FXSelector,void* ptr){
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


// Automatic scroll
long FXList::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint index;

  // Scroll the window
  FXScrollArea::onAutoScroll(sender,sel,ptr);

  // Drag and drop mode
  if(flags&FLAG_DODRAG){
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }

  // In autoselect mode, stop scrolling when mouse outside window
  if((flags&FLAG_PRESSED) || (options&LIST_AUTOSELECT)){

    // Validated position
    FXint xx=event->win_x; if(xx<0) xx=0; else if(xx>=viewport_w) xx=viewport_w-1;
    FXint yy=event->win_y; if(yy<0) yy=0; else if(yy>=viewport_h) yy=viewport_h-1;

    // Find item
    index=getItemAt(xx,yy);

    // Got item and different from last time
    if(0<=index && index!=current){

      // Make it the current item
      setCurrentItem(index,TRUE);

      // Extend the selection
      if((options&SELECT_MASK)==LIST_EXTENDEDSELECT){
        state=FALSE;
        extendSelection(index,TRUE);
        }
      }
    return 1;
    }
  return 0;
  }


// Mouse moved
long FXList::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint oldcursor=cursor;
  FXuint flg=flags;

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
  if((flags&FLAG_PRESSED) || (options&LIST_AUTOSELECT)){

    // Start auto scrolling?
    if(startAutoScroll(event,FALSE)) return 1;

    // Find item
    FXint index=getItemAt(event->win_x,event->win_y);

    // Got an item different from before
    if(0<=index && index!=current){

      // Make it the current item
      setCurrentItem(index,TRUE);

      // Extend the selection
      if((options&SELECT_MASK)==LIST_EXTENDEDSELECT){
        state=FALSE;
        extendSelection(index,TRUE);
        }
      return 1;
      }
    }

  // Reset tip timer if nothing's going on
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());

  // Get item we're over
  cursor=getItemAt(event->win_x,event->win_y);

  // Force GUI update only when needed
  return (cursor!=oldcursor)||(flg&FLAG_TIP);
  }


// Pressed a button
long FXList::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint index,code;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;

    // First change callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;

    // Autoselect mode
    if(options&LIST_AUTOSELECT) return 1;

    // Locate item
    index=getItemAt(event->win_x,event->win_y);

    // No item
    if(index<0){
      if((options&SELECT_MASK)==LIST_EXTENDEDSELECT){
        if(!(event->state&(SHIFTMASK|CONTROLMASK))){
          killSelection(TRUE);
          }
        }
      return 1;
      }

    // Find out where hit
    code=hitItem(index,event->win_x,event->win_y);

    // Change current item
    setCurrentItem(index,TRUE);

    // Change item selection
    state=items[index]->isSelected();
    switch(options&SELECT_MASK){
      case LIST_EXTENDEDSELECT:
        if(event->state&SHIFTMASK){
          if(0<=anchor){
            if(items[anchor]->isEnabled()) selectItem(anchor,TRUE);
            extendSelection(index,TRUE);
            }
          else{
            if(items[index]->isEnabled()) selectItem(index,TRUE);
            setAnchorItem(index);
            }
          }
        else if(event->state&CONTROLMASK){
          if(items[index]->isEnabled() && !state) selectItem(index,TRUE);
          setAnchorItem(index);
          }
        else{
          if(items[index]->isEnabled() && !state){ killSelection(TRUE); selectItem(index,TRUE); }
          setAnchorItem(index);
          }
        break;
      case LIST_MULTIPLESELECT:
      case LIST_SINGLESELECT:
        if(items[index]->isEnabled() && !state) selectItem(index,TRUE);
        break;
      }

    // Start drag if actually pressed text or icon only
    if(code && items[index]->isSelected() && items[index]->isDraggable()){
      flags|=FLAG_TRYDRAG;
      }

    flags|=FLAG_PRESSED;
    return 1;
    }
  return 0;
  }


// Released button
long FXList::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
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
    if(!(flg&FLAG_PRESSED) && !(options&LIST_AUTOSELECT)) return 1;

    // Was dragging
    if(flg&FLAG_DODRAG){
      handle(this,FXSEL(SEL_ENDDRAG,0),ptr);
      return 1;
      }

    // Selection change
    switch(options&SELECT_MASK){
      case LIST_EXTENDEDSELECT:
        if(0<=current && items[current]->isEnabled()){
          if(event->state&CONTROLMASK){
            if(state) deselectItem(current,TRUE);
            }
          else if(!(event->state&SHIFTMASK)){
            if(state){ killSelection(TRUE); selectItem(current,TRUE); }
            }
          }
        break;
      case LIST_MULTIPLESELECT:
      case LIST_SINGLESELECT:
        if(0<=current && items[current]->isEnabled()){
          if(state) deselectItem(current,TRUE);
          }
        break;
      }

    // Scroll to make item visibke
    makeItemVisible(current);

    // Update anchor
    setAnchorItem(current);

    // Generate clicked callbacks
    if(event->click_count==1){
      handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
      }
    else if(event->click_count==2){
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)(FXival)current);
      }
    else if(event->click_count==3){
      handle(this,FXSEL(SEL_TRIPLECLICKED,0),(void*)(FXival)current);
      }

    // Command callback only when clicked on item
    if(0<=current && items[current]->isEnabled()){
      handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
      }
    return 1;
    }
  return 0;
  }


// Pressed right button
long FXList::onRightBtnPress(FXObject*,FXSelector,void* ptr){
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
long FXList::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
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
long FXList::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onUngrabbed(sender,sel,ptr);
  flags&=~(FLAG_DODRAG|FLAG_TRYDRAG|FLAG_PRESSED|FLAG_CHANGED|FLAG_SCROLLING);
  flags|=FLAG_UPDATE;
  stopAutoScroll();
  return 1;
  }


// Command message
long FXList::onCommand(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
  }


// Clicked in list
long FXList::onClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr);
  }


// Double clicked in list; ptr may or may not point to an item
long FXList::onDoubleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr);
  }


// Triple clicked in list; ptr may or may not point to an item
long FXList::onTripleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
  }


// Sort items in ascending order
FXint FXList::ascending(const FXListItem* a,const FXListItem* b){
  return compare(a->getText(),b->getText());
  }


// Sort items in descending order
FXint FXList::descending(const FXListItem* a,const FXListItem* b){
  return compare(b->getText(),a->getText());
  }


// Sort ascending order, case insensitive
FXint FXList::ascendingCase(const FXListItem* a,const FXListItem* b){
  return comparecase(a->getText(),b->getText());
  }


// Sort descending order, case insensitive
FXint FXList::descendingCase(const FXListItem* a,const FXListItem* b){
  return comparecase(b->getText(),a->getText());
  }


// Sort the items based on the sort function.
void FXList::sortItems(){
  register FXListItem *v,*c=0;
  register FXbool exch=FALSE;
  register FXint i,j,h;
  if(sortfunc){
    if(0<=current){
      c=items[current];
      }
    for(h=1; h<=items.no()/9; h=3*h+1);
    for(; h>0; h/=3){
      for(i=h+1;i<=items.no();i++){
        v=items[i-1];
        j=i;
        while(j>h && sortfunc(items[j-h-1],v)>0){
          items[j-1]=items[j-h-1];
          exch=TRUE;
          j-=h;
          }
        items[j-1]=v;
        }
      }
    if(0<=current){
      for(i=0; i<items.no(); i++){
        if(items[i]==c){ current=i; break; }
        }
      }
    if(exch) recalc();
    }
  }


// Set current item
void FXList::setCurrentItem(FXint index,FXbool notify){
  if(index<-1 || items.no()<=index){ fxerror("%s::setCurrentItem: index out of range.\n",getClassName()); }
  if(index!=current){

    // Deactivate old item
    if(0<=current){

      // No visible change if it doen't have the focus
      if(hasFocus()){
        items[current]->setFocus(FALSE);
        updateItem(current);
        }
      }

    current=index;

    // Activate new item
    if(0<=current){

      // No visible change if it doen't have the focus
      if(hasFocus()){
        items[current]->setFocus(TRUE);
        updateItem(current);
        }
      }

    // Notify item change
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }

  // In browse select mode, select this item
  if((options&SELECT_MASK)==LIST_BROWSESELECT && 0<=current && items[current]->isEnabled()){
    selectItem(current,notify);
    }
  }


// Set anchor item
void FXList::setAnchorItem(FXint index){
  if(index<-1 || items.no()<=index){ fxerror("%s::setAnchorItem: index out of range.\n",getClassName()); }
  anchor=index;
  extent=index;
  }


// Create custom item
FXListItem *FXList::createItem(const FXString& text,FXIcon* icon,void* ptr){
  return new FXListItem(text,icon,ptr);
  }


// Retrieve item
FXListItem *FXList::getItem(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItem: index out of range.\n",getClassName()); }
  return items[index];
  }


// Replace item with another
FXint FXList::setItem(FXint index,FXListItem* item,FXbool notify){

  // Must have item
  if(!item){ fxerror("%s::setItem: item is NULL.\n",getClassName()); }

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::setItem: index out of range.\n",getClassName()); }

  // Notify item will be replaced
  if(notify && target){target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)(FXival)index);}

  // Copy the state over
  item->state=items[index]->state;

  // Delete old
  delete items[index];

  // Add new
  items[index]=item;

  // Redo layout
  recalc();
  return index;
  }


// Replace item with another
FXint FXList::setItem(FXint index,const FXString& text,FXIcon *icon,void* ptr,FXbool notify){
  return setItem(index,createItem(text,icon,ptr),notify);
  }


// Insert item
FXint FXList::insertItem(FXint index,FXListItem* item,FXbool notify){
  register FXint old=current;

  // Must have item
  if(!item){ fxerror("%s::insertItem: item is NULL.\n",getClassName()); }

  // Must be in range
  if(index<0 || items.no()<index){ fxerror("%s::insertItem: index out of range.\n",getClassName()); }

  // Add item to list
  items.insert(index,item);

  // Adjust indices
  if(anchor>=index) anchor++;
  if(extent>=index) extent++;
  if(current>=index) current++;
  if(viewable>=index) viewable++;
  if(current<0 && items.no()==1) current=0;

  // Notify item has been inserted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)(FXival)index);}

  // Current item may have changed
  if(old!=current){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }

  // Was new item
  if(0<=current && current==index){
    if(hasFocus()){
      items[current]->setFocus(TRUE);
      }
    if((options&SELECT_MASK)==LIST_BROWSESELECT && items[current]->isEnabled()){
      selectItem(current,notify);
      }
    }

  // Redo layout
  recalc();
  return index;
  }


// Insert item
FXint FXList::insertItem(FXint index,const FXString& text,FXIcon *icon,void* ptr,FXbool notify){
  return insertItem(index,createItem(text,icon,ptr),notify);
  }


// Append item
FXint FXList::appendItem(FXListItem* item,FXbool notify){
  return insertItem(items.no(),item,notify);
  }


// Append item
FXint FXList::appendItem(const FXString& text,FXIcon *icon,void* ptr,FXbool notify){
  return insertItem(items.no(),createItem(text,icon,ptr),notify);
  }


// Prepend item
FXint FXList::prependItem(FXListItem* item,FXbool notify){
  return insertItem(0,item,notify);
  }

// Prepend item
FXint FXList::prependItem(const FXString& text,FXIcon *icon,void* ptr,FXbool notify){
  return insertItem(0,createItem(text,icon,ptr),notify);
  }


// Fill list by appending items from array of strings
FXint FXList::fillItems(const FXchar** strings,FXIcon *icon,void* ptr,FXbool notify){
  register FXint n=0;
  if(strings){
    while(strings[n]){
      appendItem(strings[n++],icon,ptr,notify);
      }
    }
  return n;
  }


// Fill list by appending items from newline separated strings
FXint FXList::fillItems(const FXString& strings,FXIcon *icon,void* ptr,FXbool notify){
  register FXint n=0;
  FXString text;
  while(!(text=strings.section('\n',n)).empty()){
    appendItem(text,icon,ptr,notify);
    n++;
    }
  return n;
  }


// Move item from oldindex to newindex
FXint FXList::moveItem(FXint newindex,FXint oldindex,FXbool notify){
  register FXint old=current;
  register FXListItem *item;

  // Must be in range
  if(newindex<0 || oldindex<0 || items.no()<=newindex || items.no()<=oldindex){ fxerror("%s::moveItem: index out of range.\n",getClassName()); }

  // Did it change?
  if(oldindex!=newindex){

    // Move item
    item=items[oldindex];
    items.erase(oldindex);
    items.insert(newindex,item);

    // Move item down
    if(newindex<oldindex){
      if(newindex<=anchor && anchor<oldindex) anchor++;
      if(newindex<=extent && extent<oldindex) extent++;
      if(newindex<=current && current<oldindex) current++;
      if(newindex<=viewable && viewable<oldindex) viewable++;
      }

    // Move item up
    else{
      if(oldindex<anchor && anchor<=newindex) anchor--;
      if(oldindex<extent && extent<=newindex) extent--;
      if(oldindex<current && current<=newindex) current--;
      if(oldindex<viewable && viewable<=newindex) viewable--;
      }

    // Adjust if it was equal
    if(anchor==oldindex) anchor=newindex;
    if(extent==oldindex) extent=newindex;
    if(current==oldindex) current=newindex;
    if(viewable==oldindex) viewable=newindex;

    // Current item may have changed
    if(old!=current){
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
      }

    // Redo layout
    recalc();
    }
  return newindex;
  }


// Extract node from list
FXListItem* FXList::extractItem(FXint index,FXbool notify){
  register FXListItem *result;
  register FXint old=current;

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::extractItem: index out of range.\n",getClassName()); }

  // Notify item will be deleted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}

  // Extract item
  result=items[index];

  // Remove item from list
  items.erase(index);

  // Adjust indices
  if(anchor>index || anchor>=items.no())  anchor--;
  if(extent>index || extent>=items.no())  extent--;
  if(current>index || current>=items.no()) current--;
  if(viewable>index || viewable>=items.no()) viewable--;

  // Current item has changed
  if(index<=old){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }

  // Deleted current item
  if(0<=current && index==old){
    if(hasFocus()){
      items[current]->setFocus(TRUE);
      }
    if((options&SELECT_MASK)==LIST_BROWSESELECT && items[current]->isEnabled()){
      selectItem(current,notify);
      }
    }

  // Redo layout
  recalc();

  // Return the item
  return result;
  }


// Remove node from list
void FXList::removeItem(FXint index,FXbool notify){
  register FXint old=current;

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::removeItem: index out of range.\n",getClassName()); }

  // Notify item will be deleted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}

  // Delete item
  delete items[index];

  // Remove item from list
  items.erase(index);

  // Adjust indices
  if(anchor>index || anchor>=items.no())  anchor--;
  if(extent>index || extent>=items.no())  extent--;
  if(current>index || current>=items.no()) current--;
  if(viewable>index || viewable>=items.no()) viewable--;

  // Current item has changed
  if(index<=old){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }

  // Deleted current item
  if(0<=current && index==old){
    if(hasFocus()){
      items[current]->setFocus(TRUE);
      }
    if((options&SELECT_MASK)==LIST_BROWSESELECT && items[current]->isEnabled()){
      selectItem(current,notify);
      }
    }

  // Redo layout
  recalc();
  }


// Remove all items
void FXList::clearItems(FXbool notify){
  register FXint old=current;

  // Delete items
  for(FXint index=items.no()-1; 0<=index; index--){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}
    delete items[index];
    }

  // Free array
  items.clear();

  // Adjust indices
  current=-1;
  anchor=-1;
  extent=-1;
  viewable=-1;

  // Current item has changed
  if(old!=-1){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);}
    }

  // Redo layout
  recalc();
  }


typedef FXint (*FXCompareFunc)(const FXString&,const FXString&,FXint);


// Get item by name
FXint FXList::findItem(const FXString& text,FXint start,FXuint flgs) const {
  register FXCompareFunc comparefunc;
  register FXint index,len;
  if(0<items.no()){
    comparefunc=(flgs&SEARCH_IGNORECASE) ? (FXCompareFunc)comparecase : (FXCompareFunc)compare;
    len=(flgs&SEARCH_PREFIX)?text.length():2147483647;
    if(flgs&SEARCH_BACKWARD){
      if(start<0) start=items.no()-1;
      for(index=start; 0<=index; index--){
        if((*comparefunc)(items[index]->getText(),text,len)==0) return index;
        }
      if(!(flgs&SEARCH_WRAP)) return -1;
      for(index=items.no()-1; start<index; index--){
        if((*comparefunc)(items[index]->getText(),text,len)==0) return index;
        }
      }
    else{
      if(start<0) start=0;
      for(index=start; index<items.no(); index++){
        if((*comparefunc)(items[index]->getText(),text,len)==0) return index;
        }
      if(!(flgs&SEARCH_WRAP)) return -1;
      for(index=0; index<start; index++){
        if((*comparefunc)(items[index]->getText(),text,len)==0) return index;
        }
      }
    }
  return -1;
  }


// Get item by data
FXint FXList::findItemByData(const void *ptr,FXint start,FXuint flgs) const {
  register FXint index;
  if(0<items.no()){
    if(flgs&SEARCH_BACKWARD){
      if(start<0) start=items.no()-1;
      for(index=start; 0<=index; index--){
        if(items[index]->getData()==ptr) return index;
        }
      if(!(flgs&SEARCH_WRAP)) return -1;
      for(index=items.no()-1; start<index; index--){
        if(items[index]->getData()==ptr) return index;
        }
      }
    else{
      if(start<0) start=0;
      for(index=start; index<items.no(); index++){
        if(items[index]->getData()==ptr) return index;
        }
      if(!(flgs&SEARCH_WRAP)) return -1;
      for(index=0; index<start; index++){
        if(items[index]->getData()==ptr) return index;
        }
      }
    }
  return -1;
  }


// Change the font
void FXList::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set text color
void FXList::setTextColor(FXColor clr){
  if(textColor!=clr){
    textColor=clr;
    update();
    }
  }


// Set select background color
void FXList::setSelBackColor(FXColor clr){
  if(selbackColor!=clr){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXList::setSelTextColor(FXColor clr){
  if(seltextColor!=clr){
    seltextColor=clr;
    update();
    }
  }


// Change list style
void FXList::setListStyle(FXuint style){
  options=(options&~LIST_MASK) | (style&LIST_MASK);
  }


// Get list style
FXuint FXList::getListStyle() const {
  return (options&LIST_MASK);
  }


// Change help text
void FXList::setHelpText(const FXString& text){
  help=text;
  }


// Save data
void FXList::save(FXStream& store) const {
  FXScrollArea::save(store);
  items.save(store);
  store << anchor;
  store << current;
  store << extent;
  store << textColor;
  store << selbackColor;
  store << seltextColor;
  store << listWidth;
  store << listHeight;
  store << visible;
  store << font;
  store << help;
  }


// Load data
void FXList::load(FXStream& store){
  FXScrollArea::load(store);
  items.load(store);
  store >> anchor;
  store >> current;
  store >> extent;
  store >> textColor;
  store >> selbackColor;
  store >> seltextColor;
  store >> listWidth;
  store >> listHeight;
  store >> visible;
  store >> font;
  store >> help;
  }


// Clean up
FXList::~FXList(){
  getApp()->removeTimeout(this,ID_TIPTIMER);
  getApp()->removeTimeout(this,ID_LOOKUPTIMER);
  clearItems(FALSE);
  font=(FXFont*)-1L;
  }

}

