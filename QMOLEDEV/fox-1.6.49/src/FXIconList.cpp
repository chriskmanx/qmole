/********************************************************************************
*                                                                               *
*                          I c o n L i s t   O b j e c t                        *
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
* $Id: FXIconList.cpp,v 1.197.2.5 2006/08/02 01:31:09 fox Exp $                     *
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
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXButton.h"
#include "FXScrollBar.h"
#include "FXScrollArea.h"
#include "FXHeader.h"
#include "FXIconList.h"


/*
  To do:
  - In detail-mode, some items should be left, some right, and some centered in the field.
  - Typing first few letters of an item should make that item current.
  - Maybe need item-by-name access?
  - Return key simulates double click.
  - Need method to set header columns.
  - Sortfunc's will be hard to serialize, and hard to write w/o secretly #including
    the FXTreeItem header!
  - Rapid key actions are wrongly interpreted as double clicks
  - Upgrade later to accomodate heterogeneous item sizes (this means a more
    complex layout algorithm, and likely also explicit x,y recorded in each item).
  - In all list widgets, get rid of this complex marking business.
  - Should adding/removing items send SEL_INSERTED and SEL_DELETED callbacks.
  - Need to add support for arbitrary icon sizes same as FXTreeList already has;
    layout needs to be such that each column is as wide as widest item in that
    column only (and not as wide as the widest item in the list).
  - It may be convenient to have ways to move items around.
  - Need insertSorted() API to add item in the right place based on current
    sort function.
  - Changing icon should NOT cause recalc() when size does not change.
  - When XDND, autoscrolling happens a bit too fast because we get timers
    from motion as well as dnd-motion events; probably, it should not
    autoscroll when dragging icons.
  - Perhaps drawDetails() should ignore x coordinate and just look at getItemOffest()
    from the header instead.
  - Perhaps the ICONLIST_AUTOSIZE mode should be set with a separate API so that
    the visual stuff changed setListStyle().
  - Since '\0' is no longer special in FXString, perhaps we can replace the function
    of '\t' with '\0'.  This would be significantly more efficient.
*/



#define SIDE_SPACING             4    // Left or right spacing between items
#define DETAIL_TEXT_SPACING      2    // Spacing between text and icon in detail icon mode
#define MINI_TEXT_SPACING        2    // Spacing between text and icon in mini icon mode
#define BIG_LINE_SPACING         6    // Line spacing in big icon mode
#define BIG_TEXT_SPACING         2    // Spacing between text and icon in big icon mode
#define ITEM_SPACE             128    // Default space for item

#define SELECT_MASK   (ICONLIST_EXTENDEDSELECT|ICONLIST_SINGLESELECT|ICONLIST_BROWSESELECT|ICONLIST_MULTIPLESELECT)
#define ICONLIST_MASK (SELECT_MASK|ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS|ICONLIST_COLUMNS|ICONLIST_AUTOSIZE)

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXIconItem,FXObject,NULL,0)


// Draw item
void FXIconItem::draw(const FXIconList* list,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXuint options=list->getListStyle();
  if(options&ICONLIST_BIG_ICONS) drawBigIcon(list,dc,x,y,w,h);
  else if(options&ICONLIST_MINI_ICONS) drawMiniIcon(list,dc,x,y,w,h);
  else drawDetails(list,dc,x,y,w,h);
  }


// Draw big icon
void FXIconItem::drawBigIcon(const FXIconList* list,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXint len,dw,s,space,xt,yt,xi,yi;
  register FXFont *font=list->getFont();
  register FXint iw=0,ih=0,tw=0,th=0,ss=0;
  space=w-SIDE_SPACING;
  if(!label.empty()){
    for(len=0; len<label.length() && label[len]!='\t'; len++);
    tw=4+font->getTextWidth(label.text(),len);
    th=4+font->getFontHeight();
    yt=y+h-th-BIG_LINE_SPACING/2;
    dw=0;
    if(tw>space){
      dw=font->getTextWidth("...",3);
      s=space-dw;
      while((tw=4+font->getTextWidth(label.text(),len))>s && len>1) len=label.dec(len);
      if(tw>s) dw=0;
      }
    if(tw<=space){         // FIXME as below in drawDetails
      xt=x+(w-tw-dw)/2;
      if(isSelected()){
        dc.setForeground(list->getSelBackColor());
        dc.fillRectangle(xt,yt,tw+dw,th);
        }
      if(!isEnabled())
        dc.setForeground(makeShadowColor(list->getBackColor()));
      else if(isSelected())
        dc.setForeground(list->getSelTextColor());
      else
        dc.setForeground(list->getTextColor());
      dc.drawText(xt+2,yt+font->getFontAscent()+2,label.text(),len);
      if(dw) dc.drawText(xt+tw-2,yt+font->getFontAscent()+2,"...",3);
      if(hasFocus()){
        dc.drawFocusRectangle(xt+1,yt+1,tw+dw-2,th-2);
        }
      }
    ss=BIG_TEXT_SPACING;    // Space between text and icon only added if we have both icon and text
    }
  if(bigIcon){
    iw=bigIcon->getWidth();
    ih=bigIcon->getHeight();
    xi=x+(w-iw)/2;
    yi=y+BIG_LINE_SPACING/2+(h-th-BIG_LINE_SPACING-ss-ih)/2;
    if(isSelected()){
      dc.drawIconShaded(bigIcon,xi,yi);
      }
    else{
      dc.drawIcon(bigIcon,xi,yi);
      }
    }
  }


// Draw mini icon
void FXIconItem::drawMiniIcon(const FXIconList* list,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXFont *font=list->getFont();
  register FXint iw=0,ih=0,tw=0,th=0;
  register FXint len,dw,s,space;
  x+=SIDE_SPACING/2;
  space=w-SIDE_SPACING;
  if(miniIcon){
    iw=miniIcon->getWidth();
    ih=miniIcon->getHeight();
    if(isSelected()){
      dc.drawIconShaded(miniIcon,x,y+(h-ih)/2);
      }
    else{
      dc.drawIcon(miniIcon,x,y+(h-ih)/2);
      }
    x+=iw+MINI_TEXT_SPACING;
    space-=iw+MINI_TEXT_SPACING;
    }
  if(!label.empty()){
    for(len=0; len<label.length() && label[len]!='\t'; len++);
    tw=4+font->getTextWidth(label.text(),len);
    th=4+font->getFontHeight();
    dw=font->getTextWidth("...",3);
    y+=(h-th)/2;
    dw=0;
    if(tw>space){                  // FIXME as below in drawDetails
      dw=font->getTextWidth("...",3);
      s=space-dw;
      while((tw=4+font->getTextWidth(label.text(),len))>s && len>1) len=label.dec(len);
      if(tw>s) dw=0;
      }
    if(tw<=space){
      if(isSelected()){
        dc.setForeground(list->getSelBackColor());
        dc.fillRectangle(x,y,tw+dw,th);
        }
      if(!isEnabled())
        dc.setForeground(makeShadowColor(list->getBackColor()));
      else if(isSelected())
        dc.setForeground(list->getSelTextColor());
      else
        dc.setForeground(list->getTextColor());
      dc.drawText(x+2,y+font->getFontAscent()+2,label.text(),len);
      if(dw) dc.drawText(x+tw-2,y+font->getFontAscent()+2,"...",3);
      if(hasFocus()){
        dc.drawFocusRectangle(x+1,y+1,tw+dw-2,th-2);
        }
      }
    }
  }


// Draw detail
void FXIconItem::drawDetails(const FXIconList* list,FXDC& dc,FXint x,FXint y,FXint,FXint h) const {
  register FXHeader *header=list->getHeader();
  register FXFont *font=list->getFont();
  register FXint iw=0,ih=0,tw=0,th=0,yt,beg,end,hi,drw,space,used,dw,xx;
  if(header->getNumItems()==0) return;
  if(isSelected()){
    dc.setForeground(list->getSelBackColor());
    dc.fillRectangle(x,y,header->getTotalSize(),h);
    }
  if(hasFocus()){
    dc.drawFocusRectangle(x+1,y+1,header->getTotalSize()-2,h-2);
    }
  xx=x+SIDE_SPACING/2;
  if(miniIcon){
    iw=miniIcon->getWidth();
    ih=miniIcon->getHeight();
    dc.setClipRectangle(x,y,header->getItemSize(0),h);
    dc.drawIcon(miniIcon,xx,y+(h-ih)/2);
    dc.clearClipRectangle();
    xx+=iw+DETAIL_TEXT_SPACING;
    }
  if(!label.empty()){
    th=font->getFontHeight();
    dw=font->getTextWidth("...",3);
    yt=y+(h-th-4)/2;
    if(!isEnabled())
      dc.setForeground(makeShadowColor(list->getBackColor()));
    else if(isSelected())
      dc.setForeground(list->getSelTextColor());
    else
      dc.setForeground(list->getTextColor());
    used=iw+DETAIL_TEXT_SPACING+SIDE_SPACING/2;
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


// See if item got hit and where: 0 is outside, 1 is icon, 2 is text
FXint FXIconItem::hitItem(const FXIconList* list,FXint rx,FXint ry,FXint rw,FXint rh) const {
  register FXint iw=0,tw=0,ih=0,th=0,ss=0,ix,iy,tx,ty,w,h,sp,tlen;
  register FXuint options=list->getListStyle();
  register FXFont *font=list->getFont();
  for(tlen=0; tlen<label.length() && label[tlen]!='\t'; tlen++);
  if(options&ICONLIST_BIG_ICONS){
    w=list->getItemSpace();
    h=list->getItemHeight();
    sp=w-SIDE_SPACING;
    if(!label.empty()){
      tw=4+font->getTextWidth(label.text(),tlen);
      th=4+font->getFontHeight();
      if(tw>sp) tw=sp;
      if(bigIcon) ss=BIG_TEXT_SPACING;
      }
    if(bigIcon){
      iw=bigIcon->getWidth();
      ih=bigIcon->getHeight();
      }
    ty=h-th-BIG_LINE_SPACING/2;
    iy=BIG_LINE_SPACING/2+(h-th-BIG_LINE_SPACING-ss-ih)/2;
    ix=(w-iw)/2;
    tx=(w-tw)/2;
    }
  else if(options&ICONLIST_MINI_ICONS){
    sp=list->getItemSpace()-SIDE_SPACING;
    ix=SIDE_SPACING/2;
    tx=SIDE_SPACING/2;
    if(miniIcon){
      iw=miniIcon->getWidth();
      ih=miniIcon->getHeight();
      tx+=iw+MINI_TEXT_SPACING;
      sp=sp-iw-MINI_TEXT_SPACING;
      }
    if(!label.empty()){
      tw=4+font->getTextWidth(label.text(),tlen);
      th=4+font->getFontHeight();
      if(tw>sp) tw=sp;
      }
    h=list->getItemHeight();
    iy=(h-ih)/2;
    ty=(h-th)/2;
    }
  else{
    ix=SIDE_SPACING/2;
    tx=SIDE_SPACING/2;
    if(miniIcon){
      iw=miniIcon->getWidth();
      ih=miniIcon->getHeight();
      tx+=iw+DETAIL_TEXT_SPACING;
      }
    if(!label.empty()){
      tw=10000000;
      th=4+font->getFontHeight();
      }
    h=list->getItemHeight();
    iy=(h-ih)/2;
    ty=(h-th)/2;
    }

  // In icon?
  if(ix<=rx+rw && iy<=ry+rh && rx<ix+iw && ry<iy+ih) return 1;

  // In text?
  if(tx<=rx+rw && ty<=ry+rh && rx<tx+tw && ry<ty+th) return 2;

  // Outside
  return 0;
  }


// Set or kill focus
void FXIconItem::setFocus(FXbool focus){
  if(focus) state|=FOCUS; else state&=~FOCUS;
  }

// Select or deselect item
void FXIconItem::setSelected(FXbool selected){
  if(selected) state|=SELECTED; else state&=~SELECTED;
  }

// Enable or disable the item
void FXIconItem::setEnabled(FXbool enabled){
  if(enabled) state&=~DISABLED; else state|=DISABLED;
  }

// Icon is draggable
void FXIconItem::setDraggable(FXbool draggable){
  if(draggable) state|=DRAGGABLE; else state&=~DRAGGABLE;
  }


// Change item's text label
void FXIconItem::setText(const FXString& txt){
  label=txt;
  }


// Change item's big icon
void FXIconItem::setBigIcon(FXIcon* icn,FXbool owned){
  if(bigIcon && (state&BIGICONOWNED)){
    if(bigIcon!=icn) delete bigIcon;
    state&=~BIGICONOWNED;
    }
  bigIcon=icn;
  if(bigIcon && owned){
    state|=BIGICONOWNED;
    }
  }


// Change item's mini icon
void FXIconItem::setMiniIcon(FXIcon* icn,FXbool owned){
  if(miniIcon && (state&MINIICONOWNED)){
    if(miniIcon!=icn) delete miniIcon;
    state&=~MINIICONOWNED;
    }
  miniIcon=icn;
  if(miniIcon && owned){
    state|=MINIICONOWNED;
    }
  }


// Create icon
void FXIconItem::create(){
  if(bigIcon) bigIcon->create();
  if(miniIcon) miniIcon->create();
  }


// Destroy icon
void FXIconItem::destroy(){
  if((state&BIGICONOWNED) && bigIcon) bigIcon->destroy();
  if((state&MINIICONOWNED) && miniIcon) miniIcon->destroy();
  }


// Detach from icon resource
void FXIconItem::detach(){
  if(bigIcon) bigIcon->detach();
  if(miniIcon) miniIcon->detach();
  }


// Get item width
FXint FXIconItem::getWidth(const FXIconList* list) const {
  register FXuint options=list->getListStyle();
  register FXFont *font=list->getFont();
  register FXint iw=0,tw=0,w=0,tlen;
  for(tlen=0; tlen<label.length() && label[tlen]!='\t'; tlen++);
  if(options&ICONLIST_BIG_ICONS){
    if(bigIcon) iw=bigIcon->getWidth();
    if(!label.empty()) tw=4+font->getTextWidth(label.text(),tlen);
    w=SIDE_SPACING+FXMAX(tw,iw);
    }
  else if(options&ICONLIST_MINI_ICONS){
    if(miniIcon) iw=miniIcon->getWidth();
    if(!label.empty()) tw=4+font->getTextWidth(label.text(),tlen);
    if(iw && tw) iw+=MINI_TEXT_SPACING;
    w=SIDE_SPACING+iw+tw;
    }
  else{
    w=SIDE_SPACING;
    }
  return w;
  }


// Get item height
FXint FXIconItem::getHeight(const FXIconList* list) const {
  register FXuint options=list->getListStyle();
  register FXint ih=0,th=0,h=0;
  if(options&ICONLIST_BIG_ICONS){
    if(bigIcon) ih=bigIcon->getHeight();
    if(!label.empty()) th=4+list->getFont()->getFontHeight();
    if(ih && th) ih+=BIG_TEXT_SPACING;
    h=BIG_LINE_SPACING+ih+th;
    }
  else if(options&ICONLIST_MINI_ICONS){
    if(miniIcon) ih=miniIcon->getHeight();
    if(!label.empty()) th=4+list->getFont()->getFontHeight();
    h=FXMAX(ih,th);
    }
  else{
    if(miniIcon) ih=miniIcon->getHeight();
    if(!label.empty()) th=4+list->getFont()->getFontHeight();
    h=FXMAX(ih,th);
    }
  return h;
  }


// Save data
void FXIconItem::save(FXStream& store) const {
  FXObject::save(store);
  store << label;
  store << bigIcon;
  store << miniIcon;
  store << state;
  }


// Load data
void FXIconItem::load(FXStream& store){
  FXObject::load(store);
  store >> label;
  store >> bigIcon;
  store >> miniIcon;
  store >> state;
  }


// Delete icons if owned
FXIconItem::~FXIconItem(){
  if(state&BIGICONOWNED) delete bigIcon;
  if(state&MINIICONOWNED) delete miniIcon;
  bigIcon=(FXIcon*)-1L;
  miniIcon=(FXIcon*)-1L;
  }

/*******************************************************************************/

// Map
FXDEFMAP(FXIconList) FXIconListMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXIconList::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXIconList::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXIconList::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXIconList::onLeftBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXIconList::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXIconList::onRightBtnRelease),
  FXMAPFUNC(SEL_TIMEOUT,FXWindow::ID_AUTOSCROLL,FXIconList::onAutoScroll),
  FXMAPFUNC(SEL_TIMEOUT,FXIconList::ID_TIPTIMER,FXIconList::onTipTimer),
  FXMAPFUNC(SEL_TIMEOUT,FXIconList::ID_LOOKUPTIMER,FXIconList::onLookupTimer),
  FXMAPFUNC(SEL_UNGRABBED,0,FXIconList::onUngrabbed),
  FXMAPFUNC(SEL_KEYPRESS,0,FXIconList::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXIconList::onKeyRelease),
  FXMAPFUNC(SEL_ENTER,0,FXIconList::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXIconList::onLeave),
  FXMAPFUNC(SEL_FOCUSIN,0,FXIconList::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXIconList::onFocusOut),
  FXMAPFUNC(SEL_CLICKED,0,FXIconList::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXIconList::onDoubleClicked),
  FXMAPFUNC(SEL_TRIPLECLICKED,0,FXIconList::onTripleClicked),
  FXMAPFUNC(SEL_COMMAND,0,FXIconList::onCommand),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXIconList::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXIconList::onQueryHelp),
  FXMAPFUNC(SEL_CHANGED,FXIconList::ID_HEADER_CHANGE,FXIconList::onHeaderChanged),
  FXMAPFUNC(SEL_CLICKED,FXIconList::ID_HEADER_CHANGE,FXIconList::onHeaderResize),
  FXMAPFUNC(SEL_UPDATE,FXIconList::ID_SHOW_DETAILS,FXIconList::onUpdShowDetails),
  FXMAPFUNC(SEL_UPDATE,FXIconList::ID_SHOW_MINI_ICONS,FXIconList::onUpdShowMiniIcons),
  FXMAPFUNC(SEL_UPDATE,FXIconList::ID_SHOW_BIG_ICONS,FXIconList::onUpdShowBigIcons),
  FXMAPFUNC(SEL_UPDATE,FXIconList::ID_ARRANGE_BY_ROWS,FXIconList::onUpdArrangeByRows),
  FXMAPFUNC(SEL_UPDATE,FXIconList::ID_ARRANGE_BY_COLUMNS,FXIconList::onUpdArrangeByColumns),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_SHOW_DETAILS,FXIconList::onCmdShowDetails),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_SHOW_MINI_ICONS,FXIconList::onCmdShowMiniIcons),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_SHOW_BIG_ICONS,FXIconList::onCmdShowBigIcons),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_ARRANGE_BY_ROWS,FXIconList::onCmdArrangeByRows),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_ARRANGE_BY_COLUMNS,FXIconList::onCmdArrangeByColumns),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_SELECT_ALL,FXIconList::onCmdSelectAll),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_DESELECT_ALL,FXIconList::onCmdDeselectAll),
  FXMAPFUNC(SEL_COMMAND,FXIconList::ID_SELECT_INVERSE,FXIconList::onCmdSelectInverse),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXIconList::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,FXIconList::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,FXIconList::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXIconList,FXScrollArea,FXIconListMap,ARRAYNUMBER(FXIconListMap))


/*******************************************************************************/


// Serialization
FXIconList::FXIconList(){
  flags|=FLAG_ENABLED;
  header=(FXHeader*)-1L;
  nrows=1;
  ncols=1;
  anchor=-1;
  current=-1;
  extent=-1;
  cursor=-1;
  viewable=-1;
  font=(FXFont*)-1L;
  sortfunc=NULL;
  textColor=0;
  selbackColor=0;
  seltextColor=0;
  itemSpace=ITEM_SPACE;
  itemWidth=1;
  itemHeight=1;
  anchorx=0;
  anchory=0;
  currentx=0;
  currenty=0;
  grabx=0;
  graby=0;
  state=FALSE;
  }


// Icon List
FXIconList::FXIconList(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED;
  header=new FXHeader(this,this,FXIconList::ID_HEADER_CHANGE,HEADER_TRACKING|HEADER_BUTTON|HEADER_RESIZE|FRAME_RAISED|FRAME_THICK);
  target=tgt;
  message=sel;
  nrows=1;
  ncols=1;
  anchor=-1;
  current=-1;
  extent=-1;
  cursor=-1;
  viewable=-1;
  font=getApp()->getNormalFont();
  sortfunc=NULL;
  textColor=getApp()->getForeColor();
  selbackColor=getApp()->getSelbackColor();
  seltextColor=getApp()->getSelforeColor();
  itemSpace=ITEM_SPACE;
  itemWidth=1;
  itemHeight=1;
  anchorx=0;
  anchory=0;
  currentx=0;
  currenty=0;
  grabx=0;
  graby=0;
  state=FALSE;
  }


// Create window
void FXIconList::create(){
  register FXint i;
  FXScrollArea::create();
  for(i=0; i<items.no(); i++){items[i]->create();}
  font->create();
  }


// Detach window
void FXIconList::detach(){
  register FXint i;
  FXScrollArea::detach();
  for(i=0; i<items.no(); i++){items[i]->detach();}
  font->detach();
  }


// If window can have focus
bool FXIconList::canFocus() const { return true; }


// Into focus chain
void FXIconList::setFocus(){
  FXScrollArea::setFocus();
  setDefault(TRUE);
  }


// Out of focus chain
void FXIconList::killFocus(){
  FXScrollArea::killFocus();
  setDefault(MAYBE);
  }



// Move content
void FXIconList::moveContents(FXint x,FXint y){
  FXint dx=x-pos_x;
  FXint dy=y-pos_y;
  FXint top=0;
  pos_x=x;
  pos_y=y;
  if(!(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))){
    top=header->getDefaultHeight();
    header->setPosition(x);
    }
  scroll(0,top,viewport_w,viewport_h,dx,dy);
  }


// Propagate size change
void FXIconList::recalc(){
  FXScrollArea::recalc();
  flags|=FLAG_RECALC;
  cursor=-1;
  }


// Recompute interior
void FXIconList::recompute(){
  register FXint w,h,i;

  itemWidth=1;
  itemHeight=1;

  // Measure the items
  for(i=0; i<items.no(); i++){
    w=items[i]->getWidth(this);
    h=items[i]->getHeight(this);
    if(w>itemWidth) itemWidth=w;
    if(h>itemHeight) itemHeight=h;
    }

  // Automatically size item spacing
  if(options&ICONLIST_AUTOSIZE) itemSpace=FXMAX(itemWidth,1);

  // Adjust for detail mode
  if(!(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))) itemWidth=header->getDefaultWidth();

  // Get number of rows or columns
  getrowscols(nrows,ncols,width,height);

  //FXTRACE((100,"%s::recompute: itemWidth=%d itemHeight=%d nrows=%d ncols=%d\n",getClassName(),itemWidth,itemHeight,nrows,ncols));

  // Done
  flags&=~FLAG_RECALC;
  }


// Determine number of columns and number of rows
void FXIconList::getrowscols(FXint& nr,FXint& nc,FXint w,FXint h) const {
  if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
    if(options&ICONLIST_COLUMNS){
      nc=w/itemSpace;
      if(nc<1) nc=1;
      nr=(items.no()+nc-1)/nc;
      if(nr*itemHeight > h){
        nc=(w-vertical->getDefaultWidth())/itemSpace;
        if(nc<1) nc=1;
        nr=(items.no()+nc-1)/nc;
        }
      if(nr<1) nr=1;
      }
    else{
      nr=h/itemHeight;
      if(nr<1) nr=1;
      nc=(items.no()+nr-1)/nr;
      if(nc*itemSpace > w){
        nr=(h-horizontal->getDefaultHeight())/itemHeight;
        if(nr<1) nr=1;
        nc=(items.no()+nr-1)/nr;
        }
      if(nc<1) nc=1;
      }
    }
  else{
    nr=items.no();
    nc=1;
    }
  }


// Size of a possible column caption
FXint FXIconList::getViewportHeight(){
  return (options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS)) ? height : height-header->getDefaultHeight();
  }


// Determine content width of icon list
FXint FXIconList::getContentWidth(){
  if(flags&FLAG_RECALC) recompute();
  if(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS)) return ncols*itemSpace;
  return header->getDefaultWidth();
  }


// Determine content height of icon list
FXint FXIconList::getContentHeight(){
  if(flags&FLAG_RECALC) recompute();
  return nrows*itemHeight;
  }


// Recalculate layout
void FXIconList::layout(){

  // Update scroll bars
  FXScrollArea::layout();

  // In detail mode
  if(!(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))){
    header->position(0,0,viewport_w,header->getDefaultHeight());
    header->show();
    }
  else{
    header->hide();
    }

  // Set line size
  vertical->setLine(itemHeight);
  horizontal->setLine(itemSpace);

  // We were supposed to make this item viewable
  if(0<=viewable){
    makeItemVisible(viewable);
    }

  // Force repaint
  update();

  flags&=~FLAG_DIRTY;
  }


// Changed size:- this is a bit tricky, because
// we don't want to re-measure the items, but the content
// size has changed because the number of rows/columns has...
void FXIconList::resize(FXint w,FXint h){
  FXint nr=nrows;
  FXint nc=ncols;
  if(w!=width || h!=height){
    getrowscols(nrows,ncols,w,h);
    if(nr!=nrows || nc!=ncols) update();
    }
  //FXTRACE((100,"%s::resize: nrows=%d ncols=%d\n",getClassName(),nrows,ncols));
  FXScrollArea::resize(w,h);
  }


// Changed size and/or pos:- this is a bit tricky, because
// we don't want to re-measure the items, but the content
// size has changed because the number of rows/columns has...
void FXIconList::position(FXint x,FXint y,FXint w,FXint h){
  FXint nr=nrows;
  FXint nc=ncols;
  if(w!=width || h!=height){
    getrowscols(nrows,ncols,w,h);
    if(nr!=nrows || nc!=ncols) update();
    }
  //FXTRACE((100,"%s::position: nrows=%d ncols=%d\n",getClassName(),nrows,ncols));
  FXScrollArea::position(x,y,w,h);
  }


// Header subdivision has changed:- this is a bit tricky,
// we want to update the content size w/o re-measuring the items...
long FXIconList::onHeaderChanged(FXObject*,FXSelector,void*){
  flags&=~FLAG_RECALC;
  return 1;
  }


// Header subdivision resize has been requested;
// we want to set the width of the header column
// to that of the widest item.
long FXIconList::onHeaderResize(FXObject*,FXSelector,void* ptr){
  register FXint hi=(FXint)(FXival)ptr;
  register FXint i,iw,tw,w,nw=0;
  FXString text;

  // For detailed icon list
  if(!(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))){
    for(i=0; i<items.no(); i++){
      w=0;

      // The first header item may have an icon
      if(hi==0){
        if(items[i]->miniIcon){
          iw=items[i]->miniIcon->getWidth();
          w+=iw+DETAIL_TEXT_SPACING+SIDE_SPACING/2;
          }
        }

      // Measure section of text
      text=items[i]->label.section('\t',hi);
      if(!text.empty()){
        tw=font->getTextWidth(text.text(),text.length());
        w+=tw+SIDE_SPACING+2;
        }

      // Keep the max
      if(w>nw) nw=w;
      }

    // Set new header width
    if(nw>0 && nw!=header->getItemSize(hi)){
      header->setItemSize(hi,nw);
      flags&=~FLAG_RECALC;
      }
    }
  return 1;
  }


// Set headers from array of strings
void FXIconList::setHeaders(const FXchar** strings,FXint size){
  header->clearItems();
  header->fillItems(strings,NULL,size);
  }


// Set headers from newline separated strings
void FXIconList::setHeaders(const FXString& strings,FXint size){
  header->clearItems();
  header->fillItems(strings,NULL,size);
  }


// Append header caption
void FXIconList::appendHeader(const FXString& text,FXIcon *icon,FXint size){
  header->appendItem(text,icon,size);
  }


// Remove header caption
void FXIconList::removeHeader(FXint index){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::removeHeader: index out of range.\n",getClassName()); }
  header->removeItem(index);
  }


// Change header caption
void FXIconList::setHeaderText(FXint index,const FXString& text){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::setHeaderText: index out of range.\n",getClassName()); }
  header->setItemText(index,text);
  }


// Get header caption
FXString FXIconList::getHeaderText(FXint index) const {
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::getHeaderText: index out of range.\n",getClassName()); }
  return header->getItemText(index);
  }


// Change header icon
void FXIconList::setHeaderIcon(FXint index,FXIcon *icon){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::setHeaderIcon: index out of range.\n",getClassName()); }
  header->setItemIcon(index,icon);
  }


// Get header icon
FXIcon* FXIconList::getHeaderIcon(FXint index) const {
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::getHeaderIcon: index out of range.\n",getClassName()); }
  return header->getItemIcon(index);
  }


// Change header size
void FXIconList::setHeaderSize(FXint index,FXint size){
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::setHeaderSize: index out of range.\n",getClassName()); }
  header->setItemSize(index,size);
  }


// Get header size
FXint FXIconList::getHeaderSize(FXint index) const {
  if(index<0 || header->getNumItems()<=index){ fxerror("%s::getHeaderSize: index out of range.\n",getClassName()); }
  return header->getItemSize(index);
  }


// Return number of headers
FXint FXIconList::getNumHeaders() const {
  return header->getNumItems();
  }


// Change item text
void FXIconList::setItemText(FXint index,const FXString& text){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemText: index out of range.\n",getClassName()); }
  if(items[index]->getText()!=text){
    items[index]->setText(text);
    recalc();
    }
  }


// Get item text
FXString FXIconList::getItemText(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemText: index out of range.\n",getClassName()); }
  return items[index]->getText();
  }


// Set item icon
void FXIconList::setItemBigIcon(FXint index,FXIcon* icon,FXbool owned){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemBigIcon: index out of range.\n",getClassName()); }
  if(items[index]->getBigIcon()!=icon) recalc();
  items[index]->setBigIcon(icon,owned);
  }


// Get item icon
FXIcon* FXIconList::getItemBigIcon(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemBigIcon: index out of range.\n",getClassName()); }
  return items[index]->getBigIcon();
  }


// Set item icon
void FXIconList::setItemMiniIcon(FXint index,FXIcon* icon,FXbool owned){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemMiniIcon: index out of range.\n",getClassName()); }
  if(items[index]->getMiniIcon()!=icon) recalc();
  items[index]->setMiniIcon(icon,owned);
  }


// Get item icon
FXIcon* FXIconList::getItemMiniIcon(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemMiniIcon: index out of range.\n",getClassName()); }
  return items[index]->getMiniIcon();
  }


// Set item data
void FXIconList::setItemData(FXint index,void* ptr){
  if(index<0 || items.no()<=index){ fxerror("%s::setItemData: index out of range.\n",getClassName()); }
  items[index]->setData(ptr);
  }


// Get item data
void* FXIconList::getItemData(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItemData: index out of range.\n",getClassName()); }
  return items[index]->getData();
  }


// True if item is selected
FXbool FXIconList::isItemSelected(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemSelected: index out of range.\n",getClassName()); }
  return items[index]->isSelected();
  }


// True if item is current
FXbool FXIconList::isItemCurrent(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemCurrent: index out of range.\n",getClassName()); }
  return index==current;
  }


// True if item is enabled
FXbool FXIconList::isItemEnabled(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::isItemEnabled: index out of range.\n",getClassName()); }
  return items[index]->isEnabled();
  }


// True if item (partially) visible
FXbool FXIconList::isItemVisible(FXint index) const {
  register FXbool vis=FALSE;
  register FXint x,y,hh;
  if(index<0 || items.no()<=index){ fxerror("%s::isItemVisible: index out of range.\n",getClassName()); }
  if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
    if(options&ICONLIST_COLUMNS){
      FXASSERT(ncols>0);
      x=pos_x+itemSpace*(index%ncols);
      y=pos_y+itemHeight*(index/ncols);
      }
    else{
      FXASSERT(nrows>0);
      x=pos_x+itemSpace*(index/nrows);
      y=pos_y+itemHeight*(index%nrows);
      }
    if(0<x+itemSpace && x<viewport_w && 0<y+itemHeight && y<viewport_h) vis=TRUE;
    }
  else{
    hh=header->getDefaultHeight();
    y=pos_y+hh+index*itemHeight;
    if(hh<y+itemHeight && y<viewport_h) vis=TRUE;
    }
  return vis;
  }


// Make item fully visible
void FXIconList::makeItemVisible(FXint index){
  register FXint x,y,hh,px,py;
  if(0<=index && index<items.no()){


    // Remember for later
    viewable=index;

    // Was realized
    if(xid){

      // Force layout if dirty
      if(flags&FLAG_RECALC) layout();

      px=pos_x;
      py=pos_y;

      // Showing icon view
      if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
        if(options&ICONLIST_COLUMNS){
          FXASSERT(ncols>0);
          x=itemSpace*(index%ncols);
          y=itemHeight*(index/ncols);
          }
        else{
          FXASSERT(nrows>0);
          x=itemSpace*(index/nrows);
          y=itemHeight*(index%nrows);
          }
        if(px+x+itemSpace >= viewport_w) px=viewport_w-x-itemSpace;
        if(px+x <= 0) px=-x;
        if(py+y+itemHeight >= viewport_h) py=viewport_h-y-itemHeight;
        if(py+y <= 0) py=-y;
        }

      // Showing list view
      else{
        hh=header->getDefaultHeight();
        y=hh+index*itemHeight;
        if(py+y+itemHeight >= viewport_h+hh) py=hh+viewport_h-y-itemHeight;
        if(py+y <= hh) py=hh-y;
        }

      // Scroll into view
      setPosition(px,py);

      // Done it
      viewable=-1;
      }
    }
  }


// Get item at position x,y
FXint FXIconList::getItemAt(FXint x,FXint y) const {
  register FXint ix,iy;
  register FXint r,c,index;
  y-=pos_y;
  x-=pos_x;
  if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
    c=x/itemSpace;
    r=y/itemHeight;
    if(c<0 || c>=ncols || r<0 || r>=nrows) return -1;
    index=(options&ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
    if(index<0 || index>=items.no()) return -1;
    ix=itemSpace*c;
    iy=itemHeight*r;
    if(items[index]->hitItem(this,x-ix,y-iy)==0) return -1;
    }
  else{
    y-=header->getDefaultHeight();
    c=0;
    index=y/itemHeight;
    if(index<0 || index>=items.no()) return -1;
    }
  return index;
  }


// Compare strings up to n
static FXint comp(const FXString& s1,const FXString& s2,FXint n){
  register const FXuchar *p1=(const FXuchar *)s1.text();
  register const FXuchar *p2=(const FXuchar *)s2.text();
  register FXint c1,c2;
  if(0<n){
    do{
      c1=*p1++; if(c1=='\t') c1=0;
      c2=*p2++; if(c2=='\t') c2=0;
      }
    while(--n && c1 && (c1==c2));
    return c1-c2;
    }
  return 0;
  }


// Compare strings case insensitive up to n
static FXint compcase(const FXString& s1,const FXString& s2,FXint n){
  register const FXuchar *p1=(const FXuchar *)s1.text();
  register const FXuchar *p2=(const FXuchar *)s2.text();
  register FXint c1,c2;
  if(0<n){
    do{
      c1=Ascii::toLower(*p1++); if(c1=='\t') c1=0;      // FIXME UTF8 version
      c2=Ascii::toLower(*p2++); if(c2=='\t') c2=0;
      }
    while(--n && c1 && (c1==c2));
    return c1-c2;
    }
  return 0;
  }


typedef FXint (*FXCompareFunc)(const FXString&,const FXString&,FXint);


// Get item by name
FXint FXIconList::findItem(const FXString& text,FXint start,FXuint flgs) const {
  register FXCompareFunc comparefunc;
  register FXint index,len;
  if(0<items.no()){
    comparefunc=(flgs&SEARCH_IGNORECASE) ? (FXCompareFunc)compcase : (FXCompareFunc)comp;
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
FXint FXIconList::findItemByData(const void *ptr,FXint start,FXuint flgs) const {
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


// Did we hit the item, and which part of it did we hit
FXint FXIconList::hitItem(FXint index,FXint x,FXint y,FXint ww,FXint hh) const {
  FXint ix,iy,r,c,hit=0;
  if(0<=index && index<items.no()){
    x-=pos_x;
    y-=pos_y;
    if(!(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS))) y-=header->getDefaultHeight();
    if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
      if(options&ICONLIST_COLUMNS){
        r=index/ncols;
        c=index%ncols;
        }
      else{
        c=index/nrows;
        r=index%nrows;
        }
      }
    else{
      r=index;
      c=0;
      }
    ix=itemSpace*c;
    iy=itemHeight*r;
    hit=items[index]->hitItem(this,x-ix,y-iy,ww,hh);
    }
  return hit;
  }


// Repaint
void FXIconList::updateItem(FXint index) const {
  if(xid && 0<=index && index<items.no()){
    if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
      if(options&ICONLIST_COLUMNS){
        FXASSERT(ncols>0);
        update(pos_x+itemSpace*(index%ncols),pos_y+itemHeight*(index/ncols),itemSpace,itemHeight);
        }
      else{
        FXASSERT(nrows>0);
        update(pos_x+itemSpace*(index/nrows),pos_y+itemHeight*(index%nrows),itemSpace,itemHeight);
        }
      }
    else{
      update(0,pos_y+header->getDefaultHeight()+index*itemHeight,width,itemHeight);
      }
    }
  }


// Enable one item
FXbool FXIconList::enableItem(FXint index){
  if(index<0 || items.no()<=index){ fxerror("%s::enableItem: index out of range.\n",getClassName()); }
  if(!items[index]->isEnabled()){
    items[index]->setEnabled(TRUE);
    updateItem(index);
    return TRUE;
    }
  return FALSE;
  }


// Disable one item
FXbool FXIconList::disableItem(FXint index){
  if(index<0 || items.no()<=index){ fxerror("%s::disableItem: index out of range.\n",getClassName()); }
  if(items[index]->isEnabled()){
    items[index]->setEnabled(FALSE);
    updateItem(index);
    return TRUE;
    }
  return FALSE;
  }


// Select one item
FXbool FXIconList::selectItem(FXint index,FXbool notify){
  if(index<0 || items.no()<=index){ fxerror("%s::selectItem: index out of range.\n",getClassName()); }
  if(!items[index]->isSelected()){
    switch(options&SELECT_MASK){
      case ICONLIST_SINGLESELECT:
      case ICONLIST_BROWSESELECT:
        killSelection(notify);
      case ICONLIST_EXTENDEDSELECT:
      case ICONLIST_MULTIPLESELECT:
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
FXbool FXIconList::deselectItem(FXint index,FXbool notify){
  if(index<0 || items.no()<=index){ fxerror("%s::deselectItem: index out of range.\n",getClassName()); }
  if(items[index]->isSelected()){
    switch(options&SELECT_MASK){
      case ICONLIST_EXTENDEDSELECT:
      case ICONLIST_MULTIPLESELECT:
      case ICONLIST_SINGLESELECT:
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
FXbool FXIconList::toggleItem(FXint index,FXbool notify){
  if(index<0 || items.no()<=index){ fxerror("%s::toggleItem: index out of range.\n",getClassName()); }
  switch(options&SELECT_MASK){
    case ICONLIST_BROWSESELECT:
      if(!items[index]->isSelected()){
        killSelection(notify);
        items[index]->setSelected(TRUE);
        updateItem(index);
        if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);}
        }
      break;
    case ICONLIST_SINGLESELECT:
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
    case ICONLIST_EXTENDEDSELECT:
    case ICONLIST_MULTIPLESELECT:
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


// Select items in rectangle
FXbool FXIconList::selectInRectangle(FXint x,FXint y,FXint w,FXint h,FXbool notify){
  register FXint r,c,index;
  register FXbool changed=FALSE;
  if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
    for(r=0; r<nrows; r++){
      for(c=0; c<ncols; c++){
        index=(options&ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
        if(index<items.no()){
          if(hitItem(index,x,y,w,h)){
            changed|=selectItem(index,notify);
            }
          }
        }
      }
    }
  else{
    for(index=0; index<items.no(); index++){
      if(hitItem(index,x,y,w,h)){
        changed|=selectItem(index,notify);
        }
      }
    }
  return changed;
  }


// Extend selection
FXbool FXIconList::extendSelection(FXint index,FXbool notify){
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
FXbool FXIconList::killSelection(FXbool notify){
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


// Lasso changed, so select/unselect items based on difference between new and old lasso box
void FXIconList::lassoChanged(FXint ox,FXint oy,FXint ow,FXint oh,FXint nx,FXint ny,FXint nw,FXint nh,FXbool notify){
  register FXint r,c;
  FXint ohit,nhit,index;
  if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){
    for(r=0; r<nrows; r++){
      for(c=0; c<ncols; c++){
        index=(options&ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
        if(index<items.no()){
          ohit=hitItem(index,ox,oy,ow,oh);
          nhit=hitItem(index,nx,ny,nw,nh);
          if(ohit && !nhit){      // In old rectangle and not in new rectangle
            deselectItem(index,notify);
            }
          else if(!ohit && nhit){ // Not in old rectangle and in new rectangle
            selectItem(index,notify);
            }
          }
        }
      }
    }
  else{
    for(index=0; index<items.no(); index++){
      ohit=hitItem(index,ox,oy,ow,oh);
      nhit=hitItem(index,nx,ny,nw,nh);
      if(ohit && !nhit){          // Was in old, not in new
        deselectItem(index,notify);
        }
      else if(!ohit && nhit){     // Not in old, but in new
        selectItem(index,notify);
        }
      }
    }
  }


// Update value from a message
long FXIconList::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrentItem((FXint)(FXival)ptr);
  return 1;
  }


// Obtain value from list
long FXIconList::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCurrentItem();
  return 1;
  }


// Update value from a message
long FXIconList::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCurrentItem(*((FXint*)ptr));
  return 1;
  }


// Start motion timer while in this window
long FXIconList::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onEnter(sender,sel,ptr);
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
  cursor=-1;
  return 1;
  }


// Stop motion timer when leaving window
long FXIconList::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onLeave(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_TIPTIMER);
  cursor=-1;
  return 1;
  }


// We timed out, i.e. the user didn't move for a while
long FXIconList::onTipTimer(FXObject*,FXSelector,void*){
  FXTRACE((250,"%s::onTipTimer %p\n",getClassName(),this));
  flags|=FLAG_TIP;
  return 1;
  }


// We were asked about tip text
long FXIconList::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && (0<=cursor)){
    FXString string=items[cursor]->getText().section('\t',0);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXIconList::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Gained focus
long FXIconList::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusIn(sender,sel,ptr);
  if(0<=current){
    FXASSERT(current<items.no());
    items[current]->setFocus(TRUE);
    updateItem(current);
    }
  return 1;
  }


// Lost focus
long FXIconList::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusOut(sender,sel,ptr);
  if(0<=current){
    FXASSERT(current<items.no());
    items[current]->setFocus(FALSE);
    updateItem(current);
    }
  return 1;
  }


// Draw item list
long FXIconList::onPaint(FXObject*,FXSelector,void* ptr){
  register FXint rlo,rhi,clo,chi,yy,xx;
  register FXint x,y,r,c,index;
  FXEvent* event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);

  // Set font
  dc.setFont(font);

  // Icon mode
  if(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS)){

    // Exposed rows
    rlo=(event->rect.y-pos_y)/itemHeight;
    rhi=(event->rect.y+event->rect.h-pos_y)/itemHeight;
    if(rlo<0) rlo=0;
    if(rhi>=nrows) rhi=nrows-1;

    // Exposed columns
    clo=(event->rect.x-pos_x)/itemSpace;
    chi=(event->rect.x+event->rect.w-pos_x)/itemSpace;
    if(clo<0) clo=0;
    if(chi>=ncols) chi=ncols-1;

    // Big Icons
    if(options&ICONLIST_BIG_ICONS){
      for(r=rlo; r<=rhi; r++){
        y=pos_y+r*itemHeight;
        for(c=clo; c<=chi; c++){
          x=pos_x+c*itemSpace;
          index=(options&ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
          dc.setForeground(backColor);
          dc.fillRectangle(x,y,itemSpace,itemHeight);
          if(index<items.no()){
            items[index]->draw(this,dc,x,y,itemSpace,itemHeight);
            }
          }
        }
      }

    // Mini icons
    else{
      for(r=rlo; r<=rhi; r++){
        y=pos_y+r*itemHeight;
        for(c=clo; c<=chi; c++){
          x=pos_x+c*itemSpace;
          index=(options&ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
          dc.setForeground(backColor);
          dc.fillRectangle(x,y,itemSpace,itemHeight);
          if(index<items.no()){
            items[index]->draw(this,dc,x,y,itemSpace,itemHeight);
            }
          }
        }
      }

    // Repaint left-over background
    yy=(rhi+1)*itemHeight;
    if(yy<event->rect.y+event->rect.h){
      dc.setForeground(backColor);
      dc.fillRectangle(event->rect.x,yy,event->rect.w,event->rect.y+event->rect.h-yy);
      }
    xx=(chi+1)*itemSpace;
    if(xx<event->rect.x+event->rect.w){
      dc.setForeground(backColor);
      dc.fillRectangle(xx,event->rect.y,event->rect.x+event->rect.w-xx,event->rect.h);
      }
    }

  // Detail mode
  else{

    // Exposed rows
    rlo=(event->rect.y-pos_y-header->getDefaultHeight())/itemHeight;
    rhi=(event->rect.y+event->rect.h-pos_y-header->getDefaultHeight())/itemHeight;
    if(rlo<0) rlo=0;
    if(rhi>=items.no()) rhi=items.no()-1;

    // Repaint the items
    y=pos_y+rlo*itemHeight+header->getDefaultHeight();
    for(index=rlo; index<=rhi; index++,y+=itemHeight){
      dc.setForeground(backColor);
      dc.fillRectangle(0,y,width,itemHeight);
      items[index]->draw(this,dc,pos_x,y,width,itemHeight);
      }

    // Repaint left-over background
    if(y<event->rect.y+event->rect.h){
      dc.setForeground(backColor);
      dc.fillRectangle(event->rect.x,y,event->rect.w,event->rect.y+event->rect.h-y);
      }
    }

  return 1;
  }


// Draw Lasso rectangle
void FXIconList::drawLasso(FXint x0,FXint y0,FXint x1,FXint y1){
  FXDCWindow dc(this);
  dc.setFunction(BLT_NOT_DST);
  x0+=pos_x;
  x1+=pos_x;
  y0+=pos_y;
  y1+=pos_y;
  dc.drawLine(x0,y0,x1,y0);
  dc.drawLine(x1,y0,x1,y1);
  dc.drawLine(x1,y1,x0,y1);
  dc.drawLine(x0,y1,x0,y0);
  }


// Arrange by rows
long FXIconList::onCmdArrangeByRows(FXObject*,FXSelector,void*){
  options&=~ICONLIST_COLUMNS;
  recalc();
  return 1;
  }


// Update sender
long FXIconList::onUpdArrangeByRows(FXObject* sender,FXSelector,void*){
  sender->handle(this,(options&ICONLIST_COLUMNS)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  sender->handle(this,(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Arrange by columns
long FXIconList::onCmdArrangeByColumns(FXObject*,FXSelector,void*){
  options|=ICONLIST_COLUMNS;
  recalc();
  return 1;
  }


// Update sender
long FXIconList::onUpdArrangeByColumns(FXObject* sender,FXSelector,void*){
  sender->handle(this,(options&ICONLIST_COLUMNS)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  sender->handle(this,(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Show detailed list
long FXIconList::onCmdShowDetails(FXObject*,FXSelector,void*){
  options&=~ICONLIST_MINI_ICONS;
  options&=~ICONLIST_BIG_ICONS;
  recalc();
  return 1;
  }


// Update sender
long FXIconList::onUpdShowDetails(FXObject* sender,FXSelector,void*){
  sender->handle(this,(options&(ICONLIST_MINI_ICONS|ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Show big icons
long FXIconList::onCmdShowBigIcons(FXObject*,FXSelector,void*){
  options&=~ICONLIST_MINI_ICONS;
  options|=ICONLIST_BIG_ICONS;
  recalc();
  return 1;
  }


// Update sender
long FXIconList::onUpdShowBigIcons(FXObject* sender,FXSelector,void*){
  sender->handle(this,(options&ICONLIST_BIG_ICONS)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Show small icons
long FXIconList::onCmdShowMiniIcons(FXObject*,FXSelector,void*){
  options|=ICONLIST_MINI_ICONS;
  options&=~ICONLIST_BIG_ICONS;
  recalc();
  return 1;
  }


// Update sender
long FXIconList::onUpdShowMiniIcons(FXObject* sender,FXSelector,void*){
  sender->handle(this,(options&ICONLIST_MINI_ICONS)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Select all items
long FXIconList::onCmdSelectAll(FXObject*,FXSelector,void*){
  for(int i=0; i<items.no(); i++) selectItem(i,TRUE);
  return 1;
  }


// Deselect all items
long FXIconList::onCmdDeselectAll(FXObject*,FXSelector,void*){
  for(int i=0; i<items.no(); i++) deselectItem(i,TRUE);
  return 1;
  }


// Select inverse of current selection
long FXIconList::onCmdSelectInverse(FXObject*,FXSelector,void*){
  for(int i=0; i<items.no(); i++) toggleItem(i,TRUE);
  return 1;
  }


// Compare sectioned strings
FXint FXIconList::compareSection(const FXchar *p,const FXchar* q,FXint s){
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
FXint FXIconList::compareSectionCase(const FXchar *p,const FXchar* q,FXint s){
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
FXint FXIconList::ascending(const FXIconItem* a,const FXIconItem* b){
  return compareSection(a->getText().text(),b->getText().text(),0);
  }


// Sort items in descending order
FXint FXIconList::descending(const FXIconItem* a,const FXIconItem* b){
  return compareSection(b->getText().text(),a->getText().text(),0);
  }


// Sort items in ascending order, case insensitive
FXint FXIconList::ascendingCase(const FXIconItem* a,const FXIconItem* b){
  return compareSectionCase(a->getText().text(),b->getText().text(),0);
  }


// Sort items in descending order, case insensitive
FXint FXIconList::descendingCase(const FXIconItem* a,const FXIconItem* b){
  return compareSectionCase(b->getText().text(),a->getText().text(),0);
  }


// Sort the items based on the sort function
void FXIconList::sortItems(){
  register FXIconItem *v,*c=0;
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
void FXIconList::setCurrentItem(FXint index,FXbool notify){
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

  // In browse selection mode, select item
  if((options&SELECT_MASK)==ICONLIST_BROWSESELECT && 0<=current && items[current]->isEnabled()){
    selectItem(current,notify);
    }
  }


// Set anchor item
void FXIconList::setAnchorItem(FXint index){
  if(index<-1 || items.no()<=index){ fxerror("%s::setAnchorItem: index out of range.\n",getClassName()); }
  anchor=index;
  extent=index;
  }


// Zero out lookup string
long FXIconList::onLookupTimer(FXObject*,FXSelector,void*){
  lookup=FXString::null;
  return 1;
  }


// Key Press
long FXIconList::onKeyPress(FXObject*,FXSelector,void* ptr){
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
    case KEY_Right:
    case KEY_KP_Right:
      if(!(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS))){
        setPosition(pos_x-10,pos_y);
        return 1;
        }
      if(options&ICONLIST_COLUMNS) index+=1; else index+=nrows;
      goto hop;
    case KEY_Left:
    case KEY_KP_Left:
      if(!(options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS))){
        setPosition(pos_x+10,pos_y);
        return 1;
        }
      if(options&ICONLIST_COLUMNS) index-=1; else index-=nrows;
      goto hop;
    case KEY_Up:
    case KEY_KP_Up:
      if(options&ICONLIST_COLUMNS) index-=ncols; else index-=1;
      goto hop;
    case KEY_Down:
    case KEY_KP_Down:
      if(options&ICONLIST_COLUMNS) index+=ncols; else index+=1;
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
          if((options&SELECT_MASK)==ICONLIST_EXTENDEDSELECT){
            if(event->state&SHIFTMASK){
              if(0<=anchor){
                selectItem(anchor,TRUE);
                extendSelection(index,TRUE);
                }
              else{
                selectItem(index,TRUE);
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
          case ICONLIST_EXTENDEDSELECT:
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
          case ICONLIST_MULTIPLESELECT:
          case ICONLIST_SINGLESELECT:
            toggleItem(current,TRUE);
            break;
          }
        setAnchorItem(current);
        }
      handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
      if(0<=current && items[current]->isEnabled()){
        handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
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
	if(items[index]->isEnabled()){
	  if((options&SELECT_MASK)==ICONLIST_EXTENDEDSELECT){
	    killSelection(TRUE);
	    selectItem(index,TRUE);
	    }
	  setAnchorItem(index);
	  }
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
long FXIconList::onKeyRelease(FXObject*,FXSelector,void* ptr){
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


// Autoscrolling timer
long FXIconList::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint olx,orx,oty,oby,nlx,nrx,nty,nby;

  // Lasso mode
  if(flags&FLAG_LASSO){

    // Hide the lasso before scrolling
    drawLasso(anchorx,anchory,currentx,currenty);

    // Scroll the content
    FXScrollArea::onAutoScroll(sender,sel,ptr);

    // Select items in lasso
    FXMINMAX(olx,orx,anchorx,currentx);
    FXMINMAX(oty,oby,anchory,currenty);
    currentx=event->win_x-pos_x;
    currenty=event->win_y-pos_y;
    FXMINMAX(nlx,nrx,anchorx,currentx);
    FXMINMAX(nty,nby,anchory,currenty);
    lassoChanged(pos_x+olx,pos_y+oty,orx-olx+1,oby-oty+1,pos_x+nlx,pos_y+nty,nrx-nlx+1,nby-nty+1,TRUE);

    // Force repaint on this window
    repaint();

    // Show lasso again
    drawLasso(anchorx,anchory,currentx,currenty);
    return 1;
    }

  // Scroll the content
  FXScrollArea::onAutoScroll(sender,sel,ptr);

  // Content scrolled, so perhaps something else under cursor
  if(flags&FLAG_DODRAG){
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }

  return 0;
  }


// Mouse moved
long FXIconList::onMotion(FXObject*,FXSelector,void* ptr){
  FXint olx,orx,oty,oby,nlx,nrx,nty,nby;
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

  // Lasso selection mode
  if(flags&FLAG_LASSO){
    if(startAutoScroll(event,FALSE)) return 1;

    // Hide lasso
    drawLasso(anchorx,anchory,currentx,currenty);

    // Select items in lasso
    FXMINMAX(olx,orx,anchorx,currentx);
    FXMINMAX(oty,oby,anchory,currenty);
    currentx=event->win_x-pos_x;
    currenty=event->win_y-pos_y;
    FXMINMAX(nlx,nrx,anchorx,currentx);
    FXMINMAX(nty,nby,anchory,currenty);
    lassoChanged(pos_x+olx,pos_y+oty,orx-olx+1,oby-oty+1,pos_x+nlx,pos_y+nty,nrx-nlx+1,nby-nty+1,TRUE);

    // Force repaint on this window
    repaint();

    // Show lasso again
    drawLasso(anchorx,anchory,currentx,currenty);
    return 1;
    }

  // Drag and drop mode
  if(flags&FLAG_DODRAG){
    if(startAutoScroll(event,TRUE)) return 1;
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }

  // Tentative drag and drop
  if(flags&FLAG_TRYDRAG){
    if(event->moved){
      flags&=~FLAG_TRYDRAG;
      if(handle(this,FXSEL(SEL_BEGINDRAG,0),ptr)){
        flags|=FLAG_DODRAG;
        }
      }
    return 1;
    }

  // Reset tip timer if nothing's going on
  getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());

  // Get item we're over
  cursor=getItemAt(event->win_x,event->win_y);

  // Force GUI update only when needed
  return (cursor!=oldcursor)||(flg&FLAG_TIP);
  }


// Pressed a button
long FXIconList::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint index,code;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;

    // First change callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;

    // Locate item
    index=getItemAt(event->win_x,event->win_y);

    // No item
    if(index<0){

      // Start lasso
      if((options&SELECT_MASK)==ICONLIST_EXTENDEDSELECT){

        // Kill selection
        if(!(event->state&(SHIFTMASK|CONTROLMASK))){
          killSelection(TRUE);
          }

        anchorx=currentx=event->win_x-pos_x;
        anchory=currenty=event->win_y-pos_y;
        drawLasso(anchorx,anchory,currentx,currenty);
        flags|=FLAG_LASSO;
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
      case ICONLIST_EXTENDEDSELECT:
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
      case ICONLIST_MULTIPLESELECT:
      case ICONLIST_SINGLESELECT:
        if(items[index]->isEnabled() && !state) selectItem(index,TRUE);
        break;
      }

    // Are we dragging?
    if(code && items[index]->isSelected() && items[index]->isDraggable()){
      flags|=FLAG_TRYDRAG;
      }

    flags|=FLAG_PRESSED;
    return 1;
    }
  return 0;
  }


// Released button
long FXIconList::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXuint flg=flags;
  if(isEnabled()){
    ungrab();
    stopAutoScroll();
    flags|=FLAG_UPDATE;
    flags&=~(FLAG_PRESSED|FLAG_TRYDRAG|FLAG_LASSO|FLAG_DODRAG);

    // First chance callback
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;

    // Was lassoing
    if(flg&FLAG_LASSO){
      drawLasso(anchorx,anchory,currentx,currenty);
      return 1;
      }

    // Was dragging
    if(flg&FLAG_DODRAG){
      handle(this,FXSEL(SEL_ENDDRAG,0),ptr);
      return 1;
      }

    // Must have pressed
    if(flg&FLAG_PRESSED){

      // Selection change
      switch(options&SELECT_MASK){
        case ICONLIST_EXTENDEDSELECT:
          if(0<=current && items[current]->isEnabled()){
            if(event->state&CONTROLMASK){
              if(state) deselectItem(current,TRUE);
              }
            else if(!(event->state&SHIFTMASK)){
              if(state){ killSelection(TRUE); selectItem(current,TRUE); }
              }
            }
          break;
        case ICONLIST_MULTIPLESELECT:
        case ICONLIST_SINGLESELECT:
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
      }
    return 1;
    }
  return 0;
  }


// Pressed right button
long FXIconList::onRightBtnPress(FXObject*,FXSelector,void* ptr){
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
long FXIconList::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
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
long FXIconList::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onUngrabbed(sender,sel,ptr);
  flags&=~(FLAG_DODRAG|FLAG_LASSO|FLAG_TRYDRAG|FLAG_PRESSED|FLAG_CHANGED|FLAG_SCROLLING);
  flags|=FLAG_UPDATE;
  stopAutoScroll();
  return 1;
  }


// Command message
long FXIconList::onCommand(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
  }


// Clicked in list
long FXIconList::onClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr);
  }


// Double Clicked in list; ptr may or may not point to an item
long FXIconList::onDoubleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr);
  }


// Triple Clicked in list; ptr may or may not point to an item
long FXIconList::onTripleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
  }


// Create custom item
FXIconItem *FXIconList::createItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr){
  return new FXIconItem(text,big,mini,ptr);
  }


// Retrieve item
FXIconItem *FXIconList::getItem(FXint index) const {
  if(index<0 || items.no()<=index){ fxerror("%s::getItem: index out of range.\n",getClassName()); }
  return items[index];
  }


// Replace item with another
FXint FXIconList::setItem(FXint index,FXIconItem* item,FXbool notify){

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
FXint FXIconList::setItem(FXint index,const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify){
  return setItem(index,createItem(text,big,mini,ptr),notify);
  }


// Insert item
FXint FXIconList::insertItem(FXint index,FXIconItem* item,FXbool notify){
  register FXint old=current;

  // Must have item
  if(!item){ fxerror("%s::insertItem: item is NULL.\n",getClassName()); }

  // Must be in range
  if(index<0 || items.no()<index){ fxerror("%s::insertItem: index out of range.\n",getClassName()); }

  // Add item to list
  items.insert(index,item);

  // Adjust indices
  if(anchor>=index)  anchor++;
  if(extent>=index)  extent++;
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
    if((options&SELECT_MASK)==ICONLIST_BROWSESELECT && items[current]->isEnabled()){
      selectItem(current,notify);
      }
    }

  // Redo layout
  recalc();
  return index;
  }


// Insert item
FXint FXIconList::insertItem(FXint index,const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify){
  return insertItem(index,createItem(text,big,mini,ptr),notify);
  }


// Append item
FXint FXIconList::appendItem(FXIconItem* item,FXbool notify){
  return insertItem(items.no(),item,notify);
  }


// Append item
FXint FXIconList::appendItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify){
  return insertItem(items.no(),createItem(text,big,mini,ptr),notify);
  }


// Prepend item
FXint FXIconList::prependItem(FXIconItem* item,FXbool notify){
  return insertItem(0,item,notify);
  }


// Prepend item
FXint FXIconList::prependItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify){
  return insertItem(0,createItem(text,big,mini,ptr),notify);
  }


// Fill list by appending items from array of strings
FXint FXIconList::fillItems(const FXchar** strings,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify){
  register FXint n=0;
  if(strings){
    while(strings[n]){
      appendItem(strings[n++],big,mini,ptr,notify);
      }
    }
  return n;
  }


// Fill list by appending items from newline separated strings
FXint FXIconList::fillItems(const FXString& strings,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify){
  register FXint n=0;
  FXString text;
  while(!(text=strings.section('\n',n)).empty()){
    appendItem(text,big,mini,ptr,notify);
    n++;
    }
  return n;
  }


// Move item from oldindex to newindex
FXint FXIconList::moveItem(FXint newindex,FXint oldindex,FXbool notify){
  register FXint old=current;
  register FXIconItem *item;

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
FXIconItem* FXIconList::extractItem(FXint index,FXbool notify){
  register FXIconItem *result;
  register FXint old=current;

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::extractItem: index out of range.\n",getClassName()); }

  // Notify item will be deleted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}

  // Extract item
  result=items[index];

  // Remove from list
  items.erase(index);

  // Adjust indices
  if(anchor>index || anchor>=items.no())  anchor--;
  if(extent>index || extent>=items.no())  extent--;
  if(current>index || current>=items.no()) current--;
  if(viewable>index || viewable>=items.no())  viewable--;

  // Current item has changed
  if(index<=old){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }

  // Deleted current item
  if(0<=current && index==old){
    if(hasFocus()){
      items[current]->setFocus(TRUE);
      }
    if((options&SELECT_MASK)==ICONLIST_BROWSESELECT && items[current]->isEnabled()){
      selectItem(current,notify);
      }
    }

  // Redo layout
  recalc();

  // Return item
  return result;
  }


// Remove node from list
void FXIconList::removeItem(FXint index,FXbool notify){
  register FXint old=current;

  // Must be in range
  if(index<0 || items.no()<=index){ fxerror("%s::removeItem: index out of range.\n",getClassName()); }

  // Notify item will be deleted
  if(notify && target){target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);}

  // Delete item
  delete items[index];

  // Remove from list
  items.erase(index);

  // Adjust indices
  if(anchor>index || anchor>=items.no())  anchor--;
  if(extent>index || extent>=items.no())  extent--;
  if(current>index || current>=items.no()) current--;
  if(viewable>index || viewable>=items.no())  viewable--;

  // Current item has changed
  if(index<=old){
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }

  // Deleted current item
  if(0<=current && index==old){
    if(hasFocus()){
      items[current]->setFocus(TRUE);
      }
    if((options&SELECT_MASK)==ICONLIST_BROWSESELECT && items[current]->isEnabled()){
      selectItem(current,notify);
      }
    }

  // Redo layout
  recalc();
  }


// Remove all items
void FXIconList::clearItems(FXbool notify){
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


// Change the font
void FXIconList::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Set text color
void FXIconList::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set select background color
void FXIconList::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXIconList::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    update();
    }
  }


// Set text width
void FXIconList::setItemSpace(FXint s){
  if(s<1) s=1;
  if(itemSpace!=s){
    itemSpace=s;
    recalc();
    }
  }


// Change list style
void FXIconList::setListStyle(FXuint style){
  FXuint opts=(options&~ICONLIST_MASK) | (style&ICONLIST_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    }
  }


// Get list style
FXuint FXIconList::getListStyle() const {
  return (options&ICONLIST_MASK);
  }


// Change help text
void FXIconList::setHelpText(const FXString& text){
  help=text;
  }


// Save data
void FXIconList::save(FXStream& store) const {
  FXScrollArea::save(store);
  store << header;
  items.save(store);
  store << nrows;
  store << ncols;
  store << anchor;
  store << current;
  store << extent;
  store << font;
  store << textColor;
  store << selbackColor;
  store << seltextColor;
  store << itemSpace;
  store << itemWidth;
  store << itemHeight;
  store << help;
  }


// Load data
void FXIconList::load(FXStream& store){
  FXScrollArea::load(store);
  store >> header;
  items.load(store);
  store >> nrows;
  store >> ncols;
  store >> anchor;
  store >> current;
  store >> extent;
  store >> font;
  store >> textColor;
  store >> selbackColor;
  store >> seltextColor;
  store >> itemSpace;
  store >> itemWidth;
  store >> itemHeight;
  store >> help;
  }


// Cleanup
FXIconList::~FXIconList(){
  getApp()->removeTimeout(this,ID_TIPTIMER);
  getApp()->removeTimeout(this,ID_LOOKUPTIMER);
  clearItems(FALSE);
  header=(FXHeader*)-1L;
  font=(FXFont*)-1L;
  }

}
