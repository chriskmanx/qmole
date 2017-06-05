/********************************************************************************
*                                                                               *
*                            T a b l e   W i d g e t                            *
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
* $Id: FXTable.cpp,v 1.245.2.6 2009/01/17 10:22:51 fox Exp $                        *
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
#include "FXObjectList.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXScrollBar.h"
#include "FXTextField.h"
#include "FXButton.h"
#include "FXHeader.h"
#include "FXTable.h"
#include "FX88591Codec.h"
#include "FXCP1252Codec.h"
#include "FXUTF16Codec.h"
#include "FXList.h"
#include "FXComboBox.h"
#include "FXSpinner.h"


/*
  Notes:

  - Table looks like:

    +--------+--------+--------+--------+
    |        | ColHdr | ColHdr | ColHdr |
    +--------+--------+--------+--------+
    | RowHdr |    3.14|        |Pi      |
    +--------+--------+--------+--------+
    | RowHdr |        |        |        |
    +--------+--------+--------+--------+
    | RowHdr |        |        |        |
    +--------+--------+--------+--------+

  - Grid line have different styles [Similar to frame styles]; normally dotted lines or light grey.
  - Cells have margins around the text.
  - Column headers are optional.
  - Row headers are optional.
  - Headers stay in place, i.e. column headers stay on top, row headers stay on right.
  - Cells can contain string or icon or number.
  - Justification and formatting [for numbers]:

      - Format same for whole table
      - Format same for column
      - Format same for row
      - Format different for each cell

  - Resizing columns [same for rows]:

      - Off, no resizing allowed.
      - Column bounds.
      - Adjustment of subsequent columns (proportional to old sizes).
      - Adjustment of prior columns (proportional to old sizes).
      - Adjustment of all columns (proportional to old sizes).
      - Adjustment of first/last column.
      - Uniform column width, or per-column defined width.

  - Selection:

      - Disabled.
      - Select rows only.
      - Select columns only.
      - Select rows and columns.
      - Select cells only.

  - Selection ranges:

      - Single entity (i.e. single row, column, etc.)
      - Range of entities (multiple rows, multiple columns, blocks)

  - Reordering:

      - Disabled.
      - Reordering of columns allowed.
      - Reordering of rows allowed.
      - Both.

  - Alternating colors:

      - All the same color
      - Alternate background/foreground every other row
      - Alternate background/foreground every other column

  - Header buttons:

      - Column header button to select whole column.
      - Row header button to select whole row.

  - Fixed columns or rows:

      - First n columns and last m columns. (e.g. to show totals).
      - First n rows and last m rows.

  - Virtual storage capability for HUGE arrays:

    o When exposing virtual cells, we ask to supply content
      for the exposed cells.

    o Keep track of part of table which is visible; this is
      the actual table.

    o The actual table keeps REAL cells for those virtual cells
      which are visible [so you can manipulate them, and for
      quick repainting].

    o When scrolling, we roll over the cells as follows:

      +---------+      +---------+      +---------+
      |XXXXXXXBB|      |BBXXXXXXX|      |DDCCCCCCC|
      |XXXXXXXBB|      |BBXXXXXXX|      |BBXXXXXXX|
      |XXXXXXXBB|  ->  |BBXXXXXXX|  ->  |BBXXXXXXX|
      |XXXXXXXBB|      |BBXXXXXXX|      |BBXXXXXXX|
      |CCCCCCCDD|      |DDCCCCCCC|      |BBXXXXXXX|
      +---------+      +---------+      +---------+

      Then of course we ask to refill the cells marked B, D,
      and C.

    o When resizing, we resize the actual table, and ask to
      refill the cells on the bottom/right [or top/left, if
      we're at the maximum scrolled position and new cells are
      introduced at the top!]

    o Virtual cell from actual one:

       vr = ar+firstrow (0<=ar<visiblerows)
       vc = ac+firstcol (0<=ac<visiblecols)

    o Actual cell from virtual one:

       ar = vr-firstrow (firstrow<=vr<firstrow+visiblerows)
       ac = vc-firstcol (colstart<=vr<firstcol+visiblecols)

      In virtual mode, virtual cells outside the actual table should probably
      return NULL.

      Perhaps we can do it as follows:

      ar = (vr-firstrow)%visiblerows
      ac = (vc-firstcol)%visiblecols

      and just update nrows and ncols during scrolling.

    - Need cells which could span multiple rows/columns

    - Need multi-line cells.

    - Set cell width/height in terms of character width/font height.

  - Selection modes:
    Browse, single, multiple, extended. (like FXList).
    Column only, Row only, Both rows and columns, no selectability.
  - Current item has icon and label and data.  Virtualizing could
    bring down the cost/item from about 20 bytes/item to about 8 bytes/item
    or basically, the state+vtable.  The normal table would then use
    abstract FXTableItems, and the substitute items with string, icon,
    and void* data elsewhere.
  - Have a mode where (certain) columns and rows resize when the table does.
  - Maybe have a mode where all cells sized as wide (tall) as largest cell.
  - See FXIconList about dragging and autoscrolling.
  - Maybe special sentinel values in table to indicate spanning? Problem:-
    they'd have to be all different from each other!
*/


#define DEFAULTCOLWIDTH     100     // Initial value for defColWidth
#define DEFAULTROWHEIGHT    20      // Initial value for defRowHeight
#define FUDGE               1

#define TABLE_MASK          (TABLE_COL_SIZABLE|TABLE_ROW_SIZABLE|TABLE_NO_COLSELECT|TABLE_NO_ROWSELECT)

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXTableItem,FXObject,NULL,0)


// Draw background behind the cell
void FXTableItem::drawBackground(const FXTable* table,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXbool hg=table->isHorzGridShown();
  register FXbool vg=table->isVertGridShown();
  dc.fillRectangle(x+vg,y+hg,w-vg,h-hg);
  }


// Draw hatch pattern
void FXTableItem::drawPattern(const FXTable* table,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  if(state&0x1f00){
    register FXbool hg=table->isHorzGridShown();
    register FXbool vg=table->isVertGridShown();
    dc.setStipple((FXStipplePattern)((state&0x1f00)>>8),x,y);
    dc.setFillStyle(FILL_STIPPLED);
    dc.setForeground(table->getStippleColor());
    dc.fillRectangle(x+vg,y+hg,w-vg,h-hg);
    dc.setFillStyle(FILL_SOLID);
    }
  }


// Draw borders
void FXTableItem::drawBorders(const FXTable* table,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  if(state&(LBORDER|RBORDER|TBORDER|BBORDER)){
    register FXint bb=table->getCellBorderWidth();
    register FXbool hg=table->isHorzGridShown();
    register FXbool vg=table->isVertGridShown();
    dc.setForeground(table->getCellBorderColor());
    if(state&LBORDER) dc.fillRectangle(x,y,bb,h+hg);
    if(state&RBORDER) dc.fillRectangle(x+w+vg-bb,y,bb,h+hg);
    if(state&TBORDER) dc.fillRectangle(x,y,w+vg,bb);
    if(state&BBORDER) dc.fillRectangle(x,y+h+hg-bb,w+vg,bb);
    }
  }


// Draw content; grid lines count on left/top side but not on right/bottom side
void FXTableItem::drawContent(const FXTable* table,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXint tx,ty,tw,th,ix,iy,iw,ih,s,beg,end,t,xx,yy;
  register FXbool hg=table->isHorzGridShown();
  register FXbool vg=table->isVertGridShown();
  register FXint ml=table->getMarginLeft()+vg;
  register FXint mt=table->getMarginTop()+hg;
  register FXint mr=table->getMarginRight();
  register FXint mb=table->getMarginBottom();
  register FXFont *font=dc.getFont();
  FXString lbl=getText();
  FXIcon  *icn=getIcon();

  // Text width and height
  beg=tw=th=0;
  do{
    end=beg;
    while(end<lbl.length() && lbl[end]!='\n') end++;
    if((t=font->getTextWidth(&lbl[beg],end-beg))>tw) tw=t;
    th+=font->getFontHeight();
    beg=end+1;
    }
  while(end<lbl.length());

  // Icon size
  iw=ih=0;
  if(icn){
    iw=icn->getWidth();
    ih=icn->getHeight();
    }

  // Icon-text spacing
  s=0;
  if(iw && tw) s=4;

  // Fix x coordinate
  if(state&LEFT){
    if(state&BEFORE){ ix=x+ml; tx=ix+iw+s; }
    else if(state&AFTER){ tx=x+ml; ix=tx+tw+s; }
    else{ ix=x+ml; tx=x+ml; }
    }
  else if(state&RIGHT){
    if(state&BEFORE){ tx=x+w-mr-tw; ix=tx-iw-s; }
    else if(state&AFTER){ ix=x+w-mr-iw; tx=ix-tw-s; }
    else{ ix=x+w-mr-iw; tx=x+w-mr-tw; }
    }
  else{
    if(state&BEFORE){ ix=x+(ml+w-mr)/2-(tw+iw+s)/2; tx=ix+iw+s; }
    else if(state&AFTER){ tx=x+(ml+w-mr)/2-(tw+iw+s)/2; ix=tx+tw+s; }
    else{ ix=x+(ml+w-mr)/2-iw/2; tx=x+(ml+w-mr)/2-tw/2; }
    }

  // Fix y coordinate
  if(state&TOP){
    if(state&ABOVE){ iy=y+mt; ty=iy+ih; }
    else if(state&BELOW){ ty=y+mt; iy=ty+th; }
    else{ iy=y+mt; ty=y+mt; }
    }
  else if(state&BOTTOM){
    if(state&ABOVE){ ty=y+h-mb-th; iy=ty-ih; }
    else if(state&BELOW){ iy=y+h-mb-ih; ty=iy-th; }
    else{ iy=y+h-mb-ih; ty=y+h-mb-th; }
    }
  else{
    if(state&ABOVE){ iy=y+(mt+h-mb)/2-(th+ih)/2; ty=iy+ih; }
    else if(state&BELOW){ ty=y+(mt+h-mb)/2-(th+ih)/2; iy=ty+th; }
    else{ iy=y+(mt+h-mb)/2-ih/2; ty=y+(mt+h-mb)/2-th/2; }
    }

  // Paint icon
  if(icn){
    dc.drawIcon(icn,ix,iy);
    }

  // Text color
  if(state&SELECTED)
    dc.setForeground(table->getSelTextColor());
  else
    dc.setForeground(table->getTextColor());

  // Draw text
  yy=ty+font->getFontAscent();
  beg=0;
  do{
    end=beg;
    while(end<lbl.length() && lbl[end]!='\n') end++;
    if(state&LEFT) xx=tx;
    else if(state&RIGHT) xx=tx+tw-font->getTextWidth(&lbl[beg],end-beg);
    else xx=tx+(tw-font->getTextWidth(&lbl[beg],end-beg))/2;
    dc.drawText(xx,yy,&lbl[beg],end-beg);
    yy+=font->getFontHeight();
    beg=end+1;
    }
  while(end<lbl.length());
  }


// Draw item
void FXTableItem::draw(const FXTable* table,FXDC& dc,FXint x,FXint y,FXint w,FXint h) const {

  // Draw background
  drawBackground(table,dc,x,y,w,h);

  // Draw hatch pattern
  drawPattern(table,dc,x,y,w,h);

  // Draw cell content
  drawContent(table,dc,x,y,w,h);

  // Draw borders
  drawBorders(table,dc,x,y,w,h);
  }


// Create input control for editing this item
FXWindow *FXTableItem::getControlFor(FXTable* table){
  register FXTextField *field;
  register FXuint justify=0;
  field=new FXTextField(table,1,NULL,0,TEXTFIELD_ENTER_ONLY,0,0,0,0,table->getMarginLeft(),table->getMarginRight(),table->getMarginTop(),table->getMarginBottom());
  if(state&LEFT) justify|=JUSTIFY_LEFT;
  if(state&RIGHT) justify|=JUSTIFY_RIGHT;
  if(state&TOP) justify|=JUSTIFY_TOP;
  if(state&BOTTOM) justify|=JUSTIFY_BOTTOM;
  field->create();
  field->setJustify(justify);
  field->setFont(table->getFont());
  field->setBackColor(table->getBackColor());
  field->setTextColor(table->getTextColor());
  field->setSelBackColor(table->getSelBackColor());
  field->setSelTextColor(table->getSelTextColor());
  field->setText(label);
  field->selectAll();
  return field;
  }


// Set value from input control
void FXTableItem::setFromControl(FXWindow *control){
  register FXTextField *field=static_cast<FXTextField*>(control);
  setText(field->getText());
  }


// Set or kill focus
void FXTableItem::setFocus(FXbool focus){
  if(focus) state|=FOCUS; else state&=~FOCUS;
  }

// Select or deselect item
void FXTableItem::setSelected(FXbool selected){
  if(selected) state|=SELECTED; else state&=~SELECTED;
  }


// Enable or disable the item
void FXTableItem::setEnabled(FXbool enabled){
  if(enabled) state&=~DISABLED; else state|=DISABLED;
  }


// Icon is draggable
void FXTableItem::setDraggable(FXbool draggable){
  if(draggable) state|=DRAGGABLE; else state&=~DRAGGABLE;
  }


// Change item's text label
void FXTableItem::setText(const FXString& txt){
  label=txt;
  }


// Change item's icon, deleting the old icon if it was owned
void FXTableItem::setIcon(FXIcon* icn,FXbool owned){
  if(icon && (state&ICONOWNED)){
    if(icon!=icn) delete icon;
    state&=~ICONOWNED;
    }
  icon=icn;
  if(icon && owned){
    state|=ICONOWNED;
    }
  }


// Change justify mode
void FXTableItem::setJustify(FXuint justify){
  state=(state&~(RIGHT|LEFT|TOP|BOTTOM)) | (justify&(RIGHT|LEFT|TOP|BOTTOM));
  }

// Change icon positioning
void FXTableItem::setIconPosition(FXuint mode){
  state=(state&~(BEFORE|AFTER|ABOVE|BELOW)) | (mode&(BEFORE|AFTER|ABOVE|BELOW));
  }


// Change border mode
void FXTableItem::setBorders(FXuint borders){
  state=(state&~(LBORDER|RBORDER|TBORDER|BBORDER)) | (borders&(LBORDER|RBORDER|TBORDER|BBORDER));
  }


// Set stipple pattern
void FXTableItem::setStipple(FXStipplePattern pattern) {
  state=(state&0xffffe0ff)|((pattern<<8));
  }


// Get stipple pattern
FXStipplePattern FXTableItem::getStipple() const {
  return (FXStipplePattern)((state>>8)&0x1f);
  }


// Create icon
void FXTableItem::create(){
  if(icon) icon->create();
  }


// Destroy icon
void FXTableItem::destroy(){
  if((state&ICONOWNED) && icon) icon->destroy();
  }


// Detach from icon resource
void FXTableItem::detach(){
  if(icon) icon->detach();
  }


// Get width of item
FXint FXTableItem::getWidth(const FXTable* table) const {
  register FXFont *font=table->getFont();
  register FXint beg,end,tw,iw,s,w,t;
  register FXint ml=table->getMarginLeft();
  register FXint mr=table->getMarginRight();
  FXString lbl=getText();
  FXIcon  *icn=getIcon();
  tw=iw=beg=s=0;
  if(icn) iw=icn->getWidth();
  do{
    end=beg;
    while(end<lbl.length() && lbl[end]!='\n') end++;
    if((t=font->getTextWidth(&lbl[beg],end-beg))>tw) tw=t;
    beg=end+1;
    }
  while(end<lbl.length());
  if(iw && tw) s=4;
  if(state&(BEFORE|AFTER))
    w=iw+tw+s;
  else
    w=FXMAX(iw,tw);
  return ml+mr+w;
  }


// Get height of item
FXint FXTableItem::getHeight(const FXTable* table) const {
  register FXFont *font=table->getFont();
  register FXint beg,end,th,ih,h;
  register FXint mt=table->getMarginTop();
  register FXint mb=table->getMarginBottom();
  FXString lbl=getText();
  FXIcon  *icn=getIcon();
  th=ih=beg=0;
  if(icn) ih=icn->getHeight();
  do{
    end=beg;
    while(end<lbl.length() && lbl[end]!='\n') end++;
    th+=font->getFontHeight();
    beg=end+1;
    }
  while(end<lbl.length());
  if(state&(ABOVE|BELOW))
    h=ih+th;
  else
    h=FXMAX(ih,th);
  return h+mt+mb;
  }



// Save data
void FXTableItem::save(FXStream& store) const {
  FXObject::save(store);
  store << label;
  store << icon;
  store << state;
  }


// Load data
void FXTableItem::load(FXStream& store){
  FXObject::load(store);
  store >> label;
  store >> icon;
  store >> state;
  }


// Delete icon if owned
FXTableItem::~FXTableItem(){
  if(state&ICONOWNED) delete icon;
  icon=(FXIcon*)-1L;
  }


/*******************************************************************************/


// Object implementation
FXIMPLEMENT(FXComboTableItem,FXTableItem,NULL,0)


// Construct new table item
FXComboTableItem::FXComboTableItem(const FXString& text,FXIcon* ic,void* ptr):FXTableItem(FXString::null,ic,ptr){
  setSelections(text);
  }


// Set selections as newline-separated strings
void FXComboTableItem::setSelections(const FXString& strings){
  selections=strings;
  setText(selections.section('\n',0));
  }


// Create input control for editing this item
FXWindow *FXComboTableItem::getControlFor(FXTable* table){
  register FXComboBox *combo;
  register FXuint justify=0;
  combo=new FXComboBox(table,1,NULL,0,COMBOBOX_STATIC,0,0,0,0,table->getMarginLeft(),table->getMarginRight(),table->getMarginTop(),table->getMarginBottom());
  if(state&LEFT) justify|=JUSTIFY_LEFT;
  if(state&RIGHT) justify|=JUSTIFY_RIGHT;
  if(state&TOP) justify|=JUSTIFY_TOP;
  if(state&BOTTOM) justify|=JUSTIFY_BOTTOM;
  combo->create();
  combo->setJustify(justify);
  combo->setFont(table->getFont());
  combo->setBackColor(table->getBackColor());
  combo->setTextColor(table->getTextColor());
  combo->setSelBackColor(table->getSelBackColor());
  combo->setSelTextColor(table->getSelTextColor());
  combo->fillItems(selections);
  combo->setText(label);
  combo->setNumVisible(FXMIN(20,combo->getNumItems()));
  return combo;
  }


// Set value from input control
void FXComboTableItem::setFromControl(FXWindow *control){
  register FXComboBox *combo=static_cast<FXComboBox*>(control);
  setText(combo->getText());
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXTable) FXTableMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXTable::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXTable::onMotion),
  FXMAPFUNC(SEL_TIMEOUT,FXWindow::ID_AUTOSCROLL,FXTable::onAutoScroll),
  FXMAPFUNC(SEL_UNGRABBED,0,FXTable::onUngrabbed),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXTable::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXTable::onLeftBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXTable::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXTable::onRightBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,FXTable::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXTable::onKeyRelease),
  FXMAPFUNC(SEL_FOCUSIN,0,FXTable::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXTable::onFocusOut),
  FXMAPFUNC(SEL_SELECTION_LOST,0,FXTable::onSelectionLost),
  FXMAPFUNC(SEL_SELECTION_GAINED,0,FXTable::onSelectionGained),
  FXMAPFUNC(SEL_SELECTION_REQUEST,0,FXTable::onSelectionRequest),
  FXMAPFUNC(SEL_CLIPBOARD_LOST,0,FXTable::onClipboardLost),
  FXMAPFUNC(SEL_CLIPBOARD_GAINED,0,FXTable::onClipboardGained),
  FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,FXTable::onClipboardRequest),
  FXMAPFUNC(SEL_CLICKED,0,FXTable::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXTable::onDoubleClicked),
  FXMAPFUNC(SEL_TRIPLECLICKED,0,FXTable::onTripleClicked),
  FXMAPFUNC(SEL_COMMAND,0,FXTable::onCommand),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_HORZ_GRID,FXTable::onUpdHorzGrid),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_VERT_GRID,FXTable::onUpdVertGrid),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_HORZ_GRID,FXTable::onCmdHorzGrid),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_VERT_GRID,FXTable::onCmdVertGrid),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_DELETE_COLUMN,FXTable::onCmdDeleteColumn),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_DELETE_COLUMN,FXTable::onUpdDeleteColumn),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_DELETE_ROW,FXTable::onCmdDeleteRow),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_DELETE_ROW,FXTable::onUpdDeleteRow),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_INSERT_COLUMN,FXTable::onUpdInsertColumn),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_INSERT_COLUMN,FXTable::onCmdInsertColumn),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_INSERT_ROW,FXTable::onUpdInsertRow),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_INSERT_ROW,FXTable::onCmdInsertRow),

  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_LEFT,FXTable::onCmdMoveLeft),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_RIGHT,FXTable::onCmdMoveRight),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_UP,FXTable::onCmdMoveUp),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_DOWN,FXTable::onCmdMoveDown),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_HOME,FXTable::onCmdMoveHome),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_END,FXTable::onCmdMoveEnd),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_TOP,FXTable::onCmdMoveTop),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_BOTTOM,FXTable::onCmdMoveBottom),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_PAGEDOWN,FXTable::onCmdMovePageDown),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MOVE_PAGEUP,FXTable::onCmdMovePageUp),

  FXMAPFUNC(SEL_COMMAND,FXTable::ID_SELECT_ROW_INDEX,FXTable::onCmdSelectRowIndex),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_SELECT_COLUMN_INDEX,FXTable::onCmdSelectColumnIndex),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_SELECT_COLUMN,FXTable::onUpdSelectColumn),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_SELECT_COLUMN,FXTable::onCmdSelectColumn),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_SELECT_ROW,FXTable::onUpdSelectRow),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_SELECT_ROW,FXTable::onCmdSelectRow),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_SELECT_CELL,FXTable::onUpdSelectCell),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_SELECT_CELL,FXTable::onCmdSelectCell),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_SELECT_ALL,FXTable::onUpdSelectAll),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_SELECT_ALL,FXTable::onCmdSelectAll),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_DESELECT_ALL,FXTable::onUpdDeselectAll),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_DESELECT_ALL,FXTable::onCmdDeselectAll),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_MARK,FXTable::onCmdMark),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_EXTEND,FXTable::onCmdExtend),

  FXMAPFUNC(SEL_COMMAND,FXTable::ID_CUT_SEL,FXTable::onCmdCutSel),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_CUT_SEL,FXTable::onUpdHaveSelection),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_COPY_SEL,FXTable::onCmdCopySel),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_COPY_SEL,FXTable::onUpdHaveSelection),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_DELETE_SEL,FXTable::onCmdDeleteSel),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_DELETE_SEL,FXTable::onUpdHaveSelection),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_PASTE_SEL,FXTable::onCmdPasteSel),

  FXMAPFUNC(SEL_COMMAND,FXTable::ID_START_INPUT,FXTable::onCmdStartInput),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_START_INPUT,FXTable::onUpdStartInput),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_CANCEL_INPUT,FXTable::onCmdCancelInput),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_CANCEL_INPUT,FXTable::onUpdAcceptInput),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_ACCEPT_INPUT,FXTable::onCmdAcceptInput),
  FXMAPFUNC(SEL_UPDATE,FXTable::ID_ACCEPT_INPUT,FXTable::onUpdAcceptInput),

  FXMAPFUNC(SEL_UPDATE,FXTable::ID_TOGGLE_EDITABLE,FXTable::onUpdToggleEditable),
  FXMAPFUNC(SEL_COMMAND,FXTable::ID_TOGGLE_EDITABLE,FXTable::onCmdToggleEditable),
  };


// Object implementation
FXIMPLEMENT(FXTable,FXScrollArea,FXTableMap,ARRAYNUMBER(FXTableMap))

// So we can cut and paste into MS EXCEL
const FXchar FXTable::csvTypeName[]="Csv";

// Drag types
FXDragType FXTable::csvType=0;


/*******************************************************************************/

// Serialization
FXTable::FXTable(){
  flags|=FLAG_ENABLED;
  colHeader=NULL;
  rowHeader=NULL;
  cornerButton=NULL;
  FXCALLOC(&cells,FXTableItem*,1);
  editor=NULL;
  font=NULL;
  nrows=0;
  ncols=0;
  visiblerows=0;
  visiblecols=0;
  margintop=0;
  marginbottom=0;
  marginleft=0;
  marginright=0;
  textColor=0;
  baseColor=0;
  hiliteColor=0;
  shadowColor=0;
  borderColor=0;
  selbackColor=0;
  seltextColor=0;
  gridColor=0;
  stippleColor=0;
  cellBorderColor=0;
  cellBorderWidth=0;
  cellBackColor[0][0]=0;
  cellBackColor[0][1]=0;
  cellBackColor[1][0]=0;
  cellBackColor[1][1]=0;
  defColWidth=DEFAULTCOLWIDTH;
  defRowHeight=DEFAULTROWHEIGHT;
  current.row=-1;
  current.col=-1;
  anchor.row=-1;
  anchor.col=-1;
  input.fm.row=-1;
  input.fm.col=-1;
  input.to.row=-1;
  input.to.col=-1;
  selection.fm.row=-1;
  selection.fm.col=-1;
  selection.to.row=-1;
  selection.to.col=-1;
  mode=MOUSE_NONE;
  vgrid=TRUE;
  hgrid=TRUE;
  grabx=0;
  graby=0;
  rowcol=0;
  }


// Build table
FXTable::FXTable(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXScrollArea(p,opts,x,y,w,h){
  FXuint colhs=HEADER_HORIZONTAL|HEADER_TRACKING|HEADER_BUTTON|FRAME_RAISED|FRAME_THICK|LAYOUT_FIX_HEIGHT;
  FXuint rowhs=HEADER_VERTICAL|HEADER_TRACKING|HEADER_BUTTON|FRAME_RAISED|FRAME_THICK|LAYOUT_FIX_WIDTH;
  if(options&TABLE_COL_SIZABLE) colhs|=HEADER_RESIZE;
  if(options&TABLE_NO_COLSELECT) colhs&=~HEADER_BUTTON;
  if(options&TABLE_ROW_SIZABLE) rowhs|=HEADER_RESIZE;
  if(options&TABLE_NO_ROWSELECT) rowhs&=~HEADER_BUTTON;
  colHeader=new FXHeader(this,this,FXTable::ID_SELECT_COLUMN_INDEX,colhs,0,0,0,DEFAULTROWHEIGHT);
  rowHeader=new FXHeader(this,this,FXTable::ID_SELECT_ROW_INDEX,rowhs,0,0,DEFAULTCOLWIDTH,0);
  cornerButton=new FXButton(this,FXString::null,NULL,this,FXTable::ID_SELECT_ALL,FRAME_RAISED|FRAME_THICK);
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  editor=NULL;
  FXCALLOC(&cells,FXTableItem*,1);
  font=getApp()->getNormalFont();
  nrows=0;
  ncols=0;
  visiblerows=0;
  visiblecols=0;
  margintop=pt;
  marginbottom=pb;
  marginleft=pl;
  marginright=pr;
  textColor=getApp()->getForeColor();
  baseColor=getApp()->getBaseColor();
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  borderColor=getApp()->getBorderColor();
  selbackColor=getApp()->getSelbackColor();
  seltextColor=getApp()->getSelforeColor();
  gridColor=getApp()->getBaseColor();
  stippleColor=FXRGB(255,0,0);
  cellBorderColor=getApp()->getBorderColor();
  cellBorderWidth=2;
  cellBackColor[0][0]=getApp()->getBackColor(); // Even row, even column
  cellBackColor[0][1]=getApp()->getBackColor(); // Even row, odd column
  cellBackColor[1][0]=getApp()->getBackColor(); // Odd row, even column
  cellBackColor[1][1]=getApp()->getBackColor(); // Odd row, odd column
  defColWidth=DEFAULTCOLWIDTH;
  defRowHeight=DEFAULTROWHEIGHT;
  current.row=-1;
  current.col=-1;
  anchor.row=-1;
  anchor.col=-1;
  input.fm.row=-1;
  input.fm.col=-1;
  input.to.row=-1;
  input.to.col=-1;
  selection.fm.row=-1;
  selection.fm.col=-1;
  selection.to.row=-1;
  selection.to.col=-1;
  vgrid=TRUE;
  hgrid=TRUE;
  mode=MOUSE_NONE;
  grabx=0;
  graby=0;
  rowcol=0;
  }


// Create item
FXTableItem* FXTable::createItem(const FXString& text,FXIcon* icon,void* ptr){
  return new FXTableItem(text,icon,ptr);
  }


// Create window
void FXTable::create(){
  register FXint n=nrows*ncols;
  register FXint i;
  FXScrollArea::create();
  if(!deleteType){ deleteType=getApp()->registerDragType(deleteTypeName); }
  if(!textType){ textType=getApp()->registerDragType(textTypeName); }
  if(!utf8Type){ utf8Type=getApp()->registerDragType(utf8TypeName); }
  if(!utf16Type){ utf16Type=getApp()->registerDragType(utf16TypeName); }
  if(!csvType){ csvType=getApp()->registerDragType(csvTypeName); }
  for(i=0; i<n; i++){ if(cells[i]) cells[i]->create(); }
  font->create();
  }


// Detach window
void FXTable::detach(){
  register FXint n=nrows*ncols;
  register FXint i;
  FXScrollArea::detach();
  for(i=0; i<n; i++){ if(cells[i]) cells[i]->detach(); }
  font->detach();
  deleteType=0;
  textType=0;
  csvType=0;
  }


// Get default width; logic is as follows:
// If we don't honor the visible columns, calculate as FXScrollArea.
// If we honor visible columns, calculate the size based on that;
// if there MAY be a horizontal scroller AND its minimum size exceeds
// this, then that will determine its size.
// Finally, if there MAY be a vertical scroller we assume it will be
// included in the calculation.
FXint FXTable::getDefaultWidth(){
  register FXint w,t;
  if(0<visiblecols){
    w=visiblecols*defColWidth+vgrid;
    if(rowHeader->getLayoutHints()&LAYOUT_FIX_WIDTH) w+=rowHeader->getWidth();
    else w+=rowHeader->getDefaultWidth();
    if(!(options&HSCROLLER_NEVER) && (t=horizontal->getDefaultWidth())>w) w=t;
    if(!(options&VSCROLLER_NEVER)) w+=vertical->getDefaultWidth();
    return w;
    }
  return FXScrollArea::getDefaultWidth();
  }


// Get default height; similar logic as above
FXint FXTable::getDefaultHeight(){
  register FXint h,t;
  if(0<visiblerows){
    h=visiblerows*defRowHeight+hgrid;
    if(colHeader->getLayoutHints()&LAYOUT_FIX_HEIGHT) h+=colHeader->getHeight();
    else h+=colHeader->getDefaultHeight();
    if(!(options&VSCROLLER_NEVER) && (t=vertical->getDefaultHeight())>h) h=t;
    if(!(options&HSCROLLER_NEVER)) h+=horizontal->getDefaultHeight();
    return h;
    }
  return FXScrollArea::getDefaultHeight();
  }


// Can have focus
bool FXTable::canFocus() const { return true; }


// Into focus chain
void FXTable::setFocus(){
  FXScrollArea::setFocus();
  setDefault(TRUE);
  }


// Out of focus chain
void FXTable::killFocus(){
  FXScrollArea::killFocus();
  setDefault(MAYBE);
  acceptInput(TRUE);
  }


// Notification that focus moved to new child
void FXTable::changeFocus(FXWindow *child){
  FXWindow::changeFocus(child);

/*
  // Focus on child
  if(child){

    // Location of item
    FXint r=rowAtY(child->getY());
    FXint c=colAtX(child->getX());

FXTRACE((100,"changeFocus: x=%d y=%d r=%d c=%d\n",child->getX(),child->getY(),r,c));

    // Item inside one of the cells
    if(0<=r && r<nrows && 0<=c && c<ncols){
      FXTableItem *item;

FXTRACE((100,"changeFocus: yy=%d\n",rowHeader->getY()+rowHeader->getItemOffset(r)));
FXTRACE((100,"changeFocus: xx=%d\n",colHeader->getX()+colHeader->getItemOffset(c)));

      // Deactivate old item
      if(0<=current.row && 0<=current.col){
        FXASSERT(current.row<nrows);
        FXASSERT(current.col<ncols);
        item=cells[current.row*ncols+current.col];
        if(item){
          if(hasFocus()){
            item->setFocus(FALSE);
            updateItem(current.row,current.col);
            }
          }
        }

      // Now is current item
      current.row=r;
      current.col=c;

      // Activate new item
      if(0<=current.row && 0<=current.col){
        FXASSERT(current.row<nrows);
        FXASSERT(current.col<ncols);
        item=cells[current.row*ncols+current.col];
        if(item){
          if(hasFocus()){
            item->setFocus(TRUE);
            updateItem(current.row,current.col);
            }
          }
        }

      // Notify item change
      if(target){
        target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current);
        }
      }
    }
*/
  }


// Determine scrollable content width
FXint FXTable::getContentWidth(){
  register FXint w=colHeader->getTotalSize()+vgrid;
  if(rowHeader->getLayoutHints()&LAYOUT_FIX_WIDTH) w+=rowHeader->getWidth();
  else w+=rowHeader->getDefaultWidth();
  return w;
  }


// Determine scrollable content height
FXint FXTable::getContentHeight(){
  register FXint h=rowHeader->getTotalSize()+hgrid;
  if(colHeader->getLayoutHints()&LAYOUT_FIX_HEIGHT) h+=colHeader->getHeight();
  else h+=colHeader->getDefaultHeight();
  return h;
  }


// Starting row of multi-column cell
FXint FXTable::startRow(FXint row,FXint col) const {
  register FXTableItem *item=cells[row*ncols+col];
  if(item){ while(0<row && cells[(row-1)*ncols+col]==item) row--; }
  FXASSERT(0<=row && row<nrows);
  return row;
  }


// Starting column of multi-column cell
FXint FXTable::startCol(FXint row,FXint col) const {
  register FXTableItem *item=cells[row*ncols+col];
  if(item){ while(0<col && cells[row*ncols+col-1]==item) col--; }
  FXASSERT(0<=col && col<ncols);
  return col;
  }


// Ending row of multi-column cell
FXint FXTable::endRow(FXint row,FXint col) const {
  register FXTableItem *item=cells[row*ncols+col];
  if(item){ while(row<nrows-1 && cells[(row+1)*ncols+col]==item) row++; }
  FXASSERT(0<=row && row<nrows);
  return row;
  }


// Ending column of multi-column cell
FXint FXTable::endCol(FXint row,FXint col) const {
  register FXTableItem *item=cells[row*ncols+col];
  if(item){ while(col<ncols-1 && cells[row*ncols+col+1]==item) col++; }
  FXASSERT(0<=col && col<ncols);
  return col;
  }


// Propagate size change
void FXTable::recalc(){
  FXScrollArea::recalc();
  flags|=FLAG_RECALC;
  }


// Move content
void FXTable::moveContents(FXint x,FXint y){
  register FXint dx=x-pos_x;
  register FXint dy=y-pos_y;

  // Update position
  pos_x=x;
  pos_y=y;

  // Scroll headers
  colHeader->setPosition(x);
  rowHeader->setPosition(y);

  // Scroll table
  scroll(colHeader->getX(),rowHeader->getY(),FXMIN(colHeader->getTotalSize()+vgrid,width),FXMIN(rowHeader->getTotalSize()+hgrid,height),dx,dy);

  // Place editor control
  if(editor){
    editor->move(getColumnX(input.fm.col)+vgrid,getRowY(input.fm.row)+hgrid);
    }
  }


// Recalculate layout determines item locations and sizes
void FXTable::layout(){
  register FXint roww,colh,x,y,w,h;

  // Calculate contents
  FXScrollArea::layout();

  // Size up column header height
  if(colHeader->getLayoutHints()&LAYOUT_FIX_HEIGHT) colh=colHeader->getHeight();
  else colh=colHeader->getDefaultHeight();

  // Size up row header width
  if(rowHeader->getLayoutHints()&LAYOUT_FIX_WIDTH) roww=rowHeader->getWidth();
  else roww=rowHeader->getDefaultWidth();

  // Place headers
  colHeader->position(roww,0,viewport_w-roww,colh);
  rowHeader->position(0,colh,roww,viewport_h-colh);
  cornerButton->position(0,0,roww,colh);
  cornerButton->raise();
  colHeader->raise();
  rowHeader->raise();

  // Determine line size for scroll bars
  vertical->setLine(defRowHeight);
  horizontal->setLine(defColWidth);

  // Place editor control
  if(editor){
    x=getColumnX(input.fm.col)+vgrid;
    y=getRowY(input.fm.row)+hgrid;
    w=getColumnX(input.to.col)+getColumnWidth(input.to.col)-x;
    h=getRowY(input.to.row)+getRowHeight(input.to.row)-y;
    editor->position(x,y,w,h);
    }

  // Force repaint
  update();

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Get column containing x
FXint FXTable::colAtX(FXint x) const {
  return colHeader->getItemAt(x-colHeader->getX());
  }


// Get row containing y
FXint FXTable::rowAtY(FXint y) const {
  return rowHeader->getItemAt(y-rowHeader->getY());
  }


// Return TRUE if its a spanning cell
FXbool FXTable::isItemSpanning(FXint r,FXint c) const {
  register FXTableItem *item=cells[r*ncols+c];
  return item && ((0<r && cells[(r-1)*ncols+c]==item) || (r<nrows-1 && cells[(r+1)*ncols+c]==item) || (0<c && cells[r*ncols+c-1]==item) || (c<ncols-1 && cells[r*ncols+c+1]==item));
  }


// Force position to become fully visible
void FXTable::makePositionVisible(FXint r,FXint c){
  register FXint xlo,xhi,ylo,yhi,px,py;
  if(xid){
    px=pos_x;
    py=pos_y;
    if(0<=c && c<ncols){
      xlo=colHeader->getItem(c)->getPos();
      xhi=colHeader->getItem(c)->getSize()+xlo;
      if(px+xhi >= viewport_w-colHeader->getX()) px=viewport_w-colHeader->getX()-xhi;
      if(px+xlo <= 0) px=-xlo;
      }
    if(0<=r && r<nrows){
      ylo=rowHeader->getItem(r)->getPos();
      yhi=rowHeader->getItem(r)->getSize()+ylo;
      if(py+yhi >= viewport_h-rowHeader->getY()) py=viewport_h-rowHeader->getY()-yhi;
      if(py+ylo <= 0) py=-ylo;
      }
    setPosition(px,py);
    }
  }


// True if item (partially) visible
FXbool FXTable::isItemVisible(FXint r,FXint c) const {
  register FXint xl,xr,yt,yb;
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::isItemVisible: index out of range.\n",getClassName()); }
  xl=colHeader->getItem(c)->getPos();
  xr=colHeader->getItem(c)->getSize()+xl;
  yt=rowHeader->getItem(r)->getPos();
  yb=rowHeader->getItem(r)->getSize()+yt;
  return 0<pos_x+xr && 0<pos_y+yb && pos_x+xl<viewport_w-colHeader->getX() && pos_y+yt<viewport_h-rowHeader->getY();
  }


// Repaint cells between grid lines sr,er and grid lines sc,ec
void FXTable::updateRange(FXint sr,FXint er,FXint sc,FXint ec) const {
  FXint xl,xr,yt,yb;
  if(sr<0 || sc<0 || nrows<=er || ncols<=ec){ fxerror("%s::updateRange: index out of range.\n",getClassName()); }
  if(sr<=er && sc<=ec){
    xl=colHeader->getX()+colHeader->getItemOffset(sc);
    xr=colHeader->getX()+colHeader->getItemOffset(ec)+colHeader->getItemSize(ec);
    yt=rowHeader->getY()+rowHeader->getItemOffset(sr);
    yb=rowHeader->getY()+rowHeader->getItemOffset(er)+rowHeader->getItemSize(er);
    update(xl,yt,xr-xl+vgrid,yb-yt+hgrid);
    }
  }


// Repaint
void FXTable::updateItem(FXint r,FXint c) const {
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::updateItem: index out of range.\n",getClassName()); }
  updateRange(startRow(r,c),endRow(r,c),startCol(r,c),endCol(r,c));
  }



// Update column numbers
void FXTable::updateColumnNumbers(FXint lo,FXint hi){
  FXString label;
  for(FXint c=lo; c<hi; c++){
    setColumnText(c,label.format("%u",c+1));
    }
  }


// Update row numbers
void FXTable::updateRowNumbers(FXint lo,FXint hi){
  FXString label;
  for(FXint r=lo; r<hi; r++){
    setRowText(r,label.format("%u",r+1));
    }
  }


// Change item text
void FXTable::setItemText(FXint r,FXint c,const FXString& text,FXbool notify){
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::setItemText: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  if(item->getText()!=text){
    if(notify && target){
      FXTableRange tablerange;
      tablerange.fm.row=startRow(r,c);
      tablerange.fm.col=startCol(r,c);
      tablerange.to.row=endRow(r,c);
      tablerange.to.col=endCol(r,c);
      target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&tablerange);
      }
    item->setText(text);
    updateItem(r,c);
    }
  }


// Get item text
FXString FXTable::getItemText(FXint r,FXint c) const {
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::getItemText: index out of range.\n",getClassName()); }
  if(cells[r*ncols+c]) return cells[r*ncols+c]->getText();
  return FXString::null;
  }


// Set item icon
void FXTable::setItemIcon(FXint r,FXint c,FXIcon* icon,FXbool owned,FXbool notify){
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::setItemIcon: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  if(item->getIcon()!=icon){
    if(notify && target){
      FXTableRange tablerange;
      tablerange.fm.row=startRow(r,c);
      tablerange.fm.col=startCol(r,c);
      tablerange.to.row=endRow(r,c);
      tablerange.to.col=endCol(r,c);
      target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&tablerange);
      }
    updateItem(r,c);
    }
  item->setIcon(icon,owned);
  }


// Get item icon
FXIcon* FXTable::getItemIcon(FXint r,FXint c) const {
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::getItemIcon: index out of range.\n",getClassName()); }
  return cells[r*ncols+c] ? cells[r*ncols+c]->getIcon() : NULL;
  }


// Set item data
void FXTable::setItemData(FXint r,FXint c,void* ptr){
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::setItemData: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  item->setData(ptr);
  }


// Get item data
void* FXTable::getItemData(FXint r,FXint c) const {
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::getItemData: index out of range.\n",getClassName()); }
  return cells[r*ncols+c] ? cells[r*ncols+c]->getData() : NULL;
  }


// True if item is current
FXbool FXTable::isItemCurrent(FXint r,FXint c) const {
  if(r<0 || c<0 || nrows<=r || ncols<=c){ fxerror("%s::isItemCurrent: index out of range.\n",getClassName()); }
  return current.row==r && current.col==c;
  }


// True if item is enabled
FXbool FXTable::isItemEnabled(FXint r,FXint c) const {
  return (0<=r && 0<=c && r<nrows && c<ncols && (!cells[r*ncols+c] || cells[r*ncols+c]->isEnabled()));
  }


// Enable one item
FXbool FXTable::enableItem(FXint r,FXint c){
  if(0<=r && 0<=c && r<nrows && c<ncols){
    register FXTableItem* item=cells[r*ncols+c];
    if(item==NULL){
      cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
      if(isItemSelected(r,c)) item->setSelected(FALSE);
      }
    if(!item->isEnabled()){
      item->setEnabled(TRUE);
      updateItem(r,c);
      return TRUE;
      }
    }
  return FALSE;
  }


// Disable one item
FXbool FXTable::disableItem(FXint r,FXint c){
  if(0<=r && 0<=c && r<nrows && c<ncols){
    register FXTableItem* item=cells[r*ncols+c];
    if(item==NULL){
      cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
      if(isItemSelected(r,c)) item->setSelected(FALSE);
      }
    if(item->isEnabled()){
      item->setEnabled(FALSE);
      updateItem(r,c);
      return TRUE;
      }
    }
  return FALSE;
  }


// Change item justification
void FXTable::setItemJustify(FXint r,FXint c,FXuint justify){
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::setItemJustify: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  if(item->getJustify()!=justify){
    item->setJustify(justify);
    updateItem(r,c);
    }
  }


// Return item justification
FXuint FXTable::getItemJustify(FXint r,FXint c) const {
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::getItemJustify: index out of range.\n",getClassName()); }
  return cells[r*ncols+c] ? cells[r*ncols+c]->getJustify() : 0;
  }


// Change relative position of icon and text of item
void FXTable::setItemIconPosition(FXint r,FXint c,FXuint mode){
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::setItemIconPosition: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  if(item->getIconPosition()!=mode){
    item->setIconPosition(mode);
    updateItem(r,c);
    }
  }


// Return relative icon and text position
FXuint FXTable::getItemIconPosition(FXint r,FXint c) const {
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::getItemIconPosition: index out of range.\n",getClassName()); }
  return cells[r*ncols+c] ? cells[r*ncols+c]->getIconPosition() : 0;
  }


// Change item border style
void FXTable::setItemBorders(FXint r,FXint c,FXuint borders){
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::setItemBorders: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  if(item->getBorders()!=borders){
    item->setBorders(borders);
    updateItem(r,c);
    }
  }


// Return item border style
FXuint FXTable::getItemBorders(FXint r,FXint c) const {
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::getItemBorders: index out of range.\n",getClassName()); }
  return cells[r*ncols+c] ? cells[r*ncols+c]->getBorders() : 0;
  }


// Change item background stipple style
void FXTable::setItemStipple(FXint r,FXint c,FXStipplePattern pattern){
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::setItemStipple: index out of range.\n",getClassName()); }
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  if(item->getStipple()!=pattern){
    item->setStipple(pattern);
    updateItem(r,c);
    }
  }


// return item background stipple style
FXStipplePattern FXTable::getItemStipple(FXint r,FXint c) const {
  if(r<0 || nrows<=r || c<0 || ncols<=c){ fxerror("%s::getItemStipple: index out of range.\n",getClassName()); }
  return cells[r*ncols+c] ? cells[r*ncols+c]->getStipple() : STIPPLE_NONE;
  }


// Extract cells from given range as text
void FXTable::extractText(FXchar*& text,FXint& size,FXint startrow,FXint endrow,FXint startcol,FXint endcol,const FXchar* cs,const FXchar* rs) const {
  register FXint ncs,nrs,sz,r,c;
  register FXchar *ptr;
  FXString string;

  // Verify arguments
  if(cs==NULL || rs==NULL){ fxerror("%s::extractText: bad argument.\n",getClassName()); }

  // Verify range
  if(startrow<0 || startcol<0 || nrows<=endrow || ncols<=endcol){ fxerror("%s::extractText: index out of range.\n",getClassName()); }

  // Initialize
  text=NULL;
  size=0;

  // Non-empty range
  if(startrow<=endrow && startcol<=endcol){

    ncs=strlen(cs);
    nrs=strlen(rs);

    // Space for separators
    sz=(endrow-startrow)*((endcol-startcol)*ncs+nrs);

    // Space for  each cell
    for(r=startrow; r<=endrow; r++){
      for(c=startcol; c<=endcol; c++){
        sz+=getItemText(r,c).length();
        }
      }

    // Fill text from cells
    if(FXMALLOC(&text,FXchar,sz+1)){
      size=sz;
      ptr=text;
      for(r=startrow; r<=endrow; r++){
        for(c=startcol; c<=endcol; c++){
          string=getItemText(r,c);
          memcpy(ptr,string.text(),string.length());
          ptr+=string.length();
          if(c==endcol){
            memcpy(ptr,rs,nrs);
            ptr+=nrs;
            }
          else{
            memcpy(ptr,cs,ncs);
            ptr+=ncs;
            }
          }
        }
      *ptr='\0';        // Its there but not accounted for...
      }
    }
  }


// Extract cells from given range as text
void FXTable::extractText(FXString& text,FXint startrow,FXint endrow,FXint startcol,FXint endcol,const FXchar* cs,const FXchar* rs) const {
  register FXint ncs,nrs,sz,r,c,p;
  FXString string;

  // Verify arguments
  if(cs==NULL || rs==NULL){ fxerror("%s::extractText: bad argument.\n",getClassName()); }

  // Verify range
  if(startrow<0 || startcol<0 || nrows<=endrow || ncols<=endcol){ fxerror("%s::extractText: index out of range.\n",getClassName()); }

  // Empty it
  text.clear();

  // Non-empty range
  if(startrow<=endrow && startcol<=endcol){

    ncs=strlen(cs);
    nrs=strlen(rs);

    // Space for separators
    sz=(endrow-startrow)*((endcol-startcol)*ncs+nrs);

    // Space for  each cell
    for(r=startrow; r<=endrow; r++){
      for(c=startcol; c<=endcol; c++){
        sz+=getItemText(r,c).length();
        }
      }

    // Fill text from cells
    text.length(sz);
    for(r=startrow,p=0; r<=endrow; r++){
      for(c=startcol; c<=endcol; c++){
        string=getItemText(r,c);
        text.replace(p,string.length(),string);
        p+=string.length();
        if(c==endcol){
          text.replace(p,nrs,rs,nrs);
          p+=nrs;
          }
        else{
          text.replace(p,ncs,cs,ncs);
          p+=ncs;
          }
        }
      }
    }
  }


// Count rows and columns of a block of text
void FXTable::countText(FXint& nr,FXint& nc,const FXchar* text,FXint size,const FXchar* cs,const FXchar* rs) const {
  register FXint c=0,i=0,item=0;

  // Verify arguments
  if(size<0 || text==NULL || cs==NULL || rs==NULL){ fxerror("%s::countText: bad argument.\n",getClassName()); }

  nr=nc=0;

  // Count the text
  while(1){
    if(i>=size || text[i]=='\0'){
      if(item){
        c++;
        if(c>nc) nc=c;
        nr++;
        }
      break;
      }
    else if(strchr(rs,text[i])){
      item=0;
      c++;
      if(c>nc) nc=c;
      nr++;
      c=0;
      }
    else if(strchr(cs,text[i])){
      item=1;
      c++;
      }
    else{
      item=1;
      }
    i++;
    }
  FXTRACE((100,"countText nr=%d nc=%d\n",nr,nc));
  }


// Count rows and columns of a block of text
void FXTable::countText(FXint& nr,FXint& nc,const FXString& text,const FXchar* cs,const FXchar* rs) const {
  countText(nr,nc,text.text(),text.length(),cs,rs);
  }


// Overlay text over given cell range
void FXTable::overlayText(FXint startrow,FXint endrow,FXint startcol,FXint endcol,const FXchar* text,FXint size,const FXchar* cs,const FXchar* rs,FXbool notify){
  register FXint beg=0,end=0,r,c;

  // Verify arguments
  if(size<0 || text==NULL || cs==NULL || rs==NULL){ fxerror("%s::overlayText: bad argument.\n",getClassName()); }

  // Verify range
  if(startrow<0 || startcol<0 || startrow>endrow || startcol>endcol){ fxerror("%s::overlayText: index out of range.\n",getClassName()); }

  // Expand number of rows if required
  if(endrow>=nrows){
    insertRows(nrows,endrow-nrows+1,notify);
    }

  // Expand number of columns if required
  if(endcol>=ncols){
    insertColumns(ncols,endcol-ncols+1,notify);
    }

  // Insert point
  r=startrow;
  c=startcol;

  // Overlay new data
  while(1){
    if(end>=size || text[end]=='\0'){
      if(r<=endrow && c<=endcol){
        setItemText(r,c,FXString(&text[beg],end-beg),notify);
        }
      break;
      }
    else if(strchr(rs,text[end])){
      if(r<=endrow && c<=endcol){
        setItemText(r,c,FXString(&text[beg],end-beg),notify);
        }
      beg=end+1;
      c=startcol;
      r++;
      }
    else if(strchr(cs,text[end])){
      if(r<=endrow && c<=endcol){
        setItemText(r,c,FXString(&text[beg],end-beg),notify);
        }
      beg=end+1;
      c++;
      }
    end++;
    }
  }


// Overlay text over given cell range
void FXTable::overlayText(FXint startrow,FXint endrow,FXint startcol,FXint endcol,const FXString& text,const FXchar* cs,const FXchar* rs,FXbool notify){
  overlayText(startrow,endrow,startcol,endcol,text.text(),text.length(),cs,rs,notify);
  }


// Set current item
void FXTable::setCurrentItem(FXint r,FXint c,FXbool notify){
  register FXTableItem* item;

  // Verify input indices
  r=FXCLAMP(-1,r,nrows-1);
  c=FXCLAMP(-1,c,ncols-1);

  // End editing
  acceptInput(notify);

  // Did it change
  if(r!=current.row || c!=current.col){

    // Deactivate old item
    if(0<=current.row && 0<=current.col){
      FXASSERT(current.row<nrows);
      FXASSERT(current.col<ncols);
      item=cells[current.row*ncols+current.col];
      if(item){
        if(hasFocus()){
          item->setFocus(FALSE);
          }
        }
      updateItem(current.row,current.col);
      }

    current.row=r;
    current.col=c;

    // Activate new item
    if(0<=current.row && 0<=current.col){
      FXASSERT(current.row<nrows);
      FXASSERT(current.col<ncols);
      item=cells[current.row*ncols+current.col];
      if(item){
        if(hasFocus()){
          item->setFocus(TRUE);
          }
        }
      updateItem(current.row,current.col);
      }

    // Notify item change
    if(notify && target){
      target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current);
      }
    }
  }


// Set anchor item
void FXTable::setAnchorItem(FXint r,FXint c){
  anchor.row=FXCLAMP(-1,r,nrows-1);
  anchor.col=FXCLAMP(-1,c,ncols-1);
  }


// Extend range to include spanning cells
void FXTable::spanningRange(FXint& sr,FXint& er,FXint& sc,FXint& ec,FXint anchrow,FXint anchcol,FXint currow,FXint curcol){
  register FXint i,t;

  // Current position in table
  FXASSERT(0<=currow && currow<nrows);
  FXASSERT(0<=curcol && curcol<ncols);

  // Anchor position in table
  FXASSERT(0<=anchrow && anchrow<nrows);
  FXASSERT(0<=anchcol && anchcol<ncols);

  // Initial block just the rectangle
  FXMINMAX(sr,er,anchrow,currow);
  FXMINMAX(sc,ec,anchcol,curcol);

  // Expand sr:er sc:ec to fully enclose all spanning cells
a:for(i=sr; i<=er; i++){
    if((t=startCol(i,sc))<sc){ sc=t; goto a; }
    }
  for(i=sr; i<=er; i++){
    if((t=endCol(i,ec))>ec){ ec=t; goto a; }
    }
  for(i=sc; i<=ec; i++){
    if((t=startRow(sr,i))<sr){ sr=t; goto a; }
    }
  for(i=sc; i<=ec; i++){
    if((t=endRow(er,i))>er){ er=t; goto a; }
    }

  FXASSERT(0<=sr && sr<=er && er<nrows);
  FXASSERT(0<=sc && sc<=ec && ec<ncols);
  }


// True if item is selected
FXbool FXTable::isItemSelected(FXint r,FXint c) const {
  return selection.fm.row<=r && r<=selection.to.row && selection.fm.col<=c && c<=selection.to.col;
  }


// Is row of cells selected
FXbool FXTable::isRowSelected(FXint r) const {
  return selection.fm.row<=r && r<=selection.to.row && selection.fm.col==0 && selection.to.col==ncols-1;
  }


// Is column selected
FXbool FXTable::isColumnSelected(FXint c) const {
  return selection.fm.row==0 && selection.to.row==nrows-1 && selection.fm.col<=c && c<=selection.to.col;
  }


// Is anything selected
FXbool FXTable::isAnythingSelected() const {
  return 0<=selection.fm.row && 0<=selection.to.row && 0<=selection.fm.col && 0<=selection.to.col;
  }


// Select a row
FXbool FXTable::selectRow(FXint row,FXbool notify){
  return selectRange(row,row,0,ncols-1,notify);
  }


// Select a column
FXbool FXTable::selectColumn(FXint col,FXbool notify){
  return selectRange(0,nrows-1,col,col,notify);
  }


// Extend selection
FXbool FXTable::extendSelection(FXint r,FXint c,FXbool notify){
  return selectRange(anchor.row,r,anchor.col,c,notify);
  }


// Select range
FXbool FXTable::selectRange(FXint startrow,FXint endrow,FXint startcol,FXint endcol,FXbool notify){
  FXint orlo,orhi,oclo,ochi,nrlo,nrhi,nclo,nchi,rlo,rhi,clo,chi,inold,innew;
  FXTablePos tablepos;
  FXTableItem *item;

  // Verify arguments
  if(0<=startrow && 0<=startcol && 0<=endrow && 0<=endcol && startrow<nrows && startcol<ncols && endrow<nrows && endcol<ncols){

    // Determine new selection rectangle
    spanningRange(nrlo,nrhi,nclo,nchi,startrow,startcol,endrow,endcol);

    // Rectangle
    rlo=nrlo;
    rhi=nrhi;
    clo=nclo;
    chi=nchi;

    // Just to be safe
    orlo=orhi=oclo=ochi=-1;

    // Did have old selection
    if(isAnythingSelected()){

      // Old selection rectangle
      orlo=selection.fm.row;
      oclo=selection.fm.col;
      ochi=selection.to.col;
      orhi=selection.to.row;

      // Maximum of old and new rectangle
      if(orlo<rlo) rlo=orlo;
      if(orhi>rhi) rhi=orhi;
      if(oclo<clo) clo=oclo;
      if(ochi>chi) chi=ochi;
      }

    // Hopefully
    FXASSERT(0<=rlo && rlo<=rhi && rhi<nrows);
    FXASSERT(0<=clo && clo<=chi && chi<ncols);

    // New selection rectangle
    selection.fm.row=nrlo;
    selection.fm.col=nclo;
    selection.to.row=nrhi;
    selection.to.col=nchi;

    // Change items
    for(tablepos.row=rlo; tablepos.row<=rhi; tablepos.row++){
      for(tablepos.col=clo; tablepos.col<=chi; tablepos.col++){
        item=cells[tablepos.row*ncols+tablepos.col];
        inold=(orlo<=tablepos.row && tablepos.row<=orhi && oclo<=tablepos.col && tablepos.col<=ochi);
        innew=(nrlo<=tablepos.row && tablepos.row<=nrhi && nclo<=tablepos.col && tablepos.col<=nchi);
        if(inold && !innew){
          if(item){ item->setSelected(FALSE); }
          updateItem(tablepos.row,tablepos.col);
          if(notify && target) target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)&tablepos);
          }
        else if(!inold && innew){
          if(item){ item->setSelected(TRUE); }
          updateItem(tablepos.row,tablepos.col);
          if(notify && target) target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)&tablepos);
          }
        }
      }
    return TRUE;
    }
  return FALSE;
  }


// Kill selection
FXbool FXTable::killSelection(FXbool notify){
  FXTablePos tablepos;
  FXTableItem *item;
  if(isAnythingSelected()){
    for(tablepos.row=selection.fm.row; tablepos.row<=selection.to.row; tablepos.row++){
      for(tablepos.col=selection.fm.col; tablepos.col<=selection.to.col; tablepos.col++){
        item=cells[tablepos.row*ncols+tablepos.col];
        if(item){ item->setSelected(FALSE); }
        updateItem(tablepos.row,tablepos.col);
        if(notify && target) target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)&tablepos);
        }
      }
    selection.fm.row=-1;
    selection.fm.col=-1;
    selection.to.row=-1;
    selection.to.col=-1;
    return TRUE;
    }
  return FALSE;
  }


// Get input control to edit the item
FXWindow *FXTable::getControlForItem(FXint r,FXint c){
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  return item->getControlFor(this);
  }


// Set the item from the input control
void FXTable::setItemFromControl(FXint r,FXint c,FXWindow *control){
  register FXTableItem* item=cells[r*ncols+c];
  if(item==NULL){
    cells[r*ncols+c]=item=createItem(FXString::null,NULL,NULL);
    if(isItemSelected(r,c)) item->setSelected(FALSE);
    }
  item->setFromControl(control);
  }


// Start to edit a cell
void FXTable::startInput(FXint r,FXint c){
  if(0<=r && 0<=c && !editor){
    editor=getControlForItem(r,c);
    if(editor){
      input.fm.row=startRow(r,c);
      input.fm.col=startCol(r,c);
      input.to.row=endRow(r,c);
      input.to.col=endCol(r,c);
      editor->setTarget(this);
      editor->setSelector(ID_ACCEPT_INPUT);
      editor->setFocus();
      recalc();
      }
    }
  }


// Cancel editing cell
void FXTable::cancelInput(){
  if(editor){
    delete editor;
    input.fm.row=-1;
    input.to.row=-1;
    input.fm.col=-1;
    input.to.col=-1;
    editor=NULL;
    }
  }


// Done with editing cell
void FXTable::acceptInput(FXbool notify){
  if(editor){
    FXTableRange tablerange=input;
    setItemFromControl(input.fm.row,input.fm.col,editor);
    cancelInput();
    if(notify && target){
      target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&tablerange);
      }
    }
  }


// Start edit of current cell
long FXTable::onCmdStartInput(FXObject*,FXSelector,void*){
  if(isEditable() && isItemEnabled(current.row,current.col)){
    startInput(current.row,current.col);
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Update start edit
long FXTable::onUpdStartInput(FXObject* sender,FXSelector,void*){
  sender->handle(this,(isEditable() && isItemEnabled(current.row,current.col) && !editor)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Cancel edit
long FXTable::onCmdCancelInput(FXObject*,FXSelector,void*){
  cancelInput();
  return 1;
  }


// End edit
long FXTable::onCmdAcceptInput(FXObject*,FXSelector,void*){
  acceptInput(TRUE);
  return 1;
  }


// Update end edit
long FXTable::onUpdAcceptInput(FXObject* sender,FXSelector,void*){
  sender->handle(this,editor ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Gained focus
long FXTable::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  register FXTableItem *item;
  FXScrollArea::onFocusIn(sender,sel,ptr);
  if(0<=current.row && 0<=current.col){
    FXASSERT(current.row<nrows);
    FXASSERT(current.col<ncols);
    item=cells[current.row*ncols+current.col];
    if(item) item->setFocus(TRUE);
    updateItem(current.row,current.col);
    }
  return 1;
  }


// Lost focus
long FXTable::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  register FXTableItem *item;
  FXScrollArea::onFocusOut(sender,sel,ptr);
  if(0<=current.row && 0<=current.col){
    FXASSERT(current.row<nrows);
    FXASSERT(current.col<ncols);
    item=cells[current.row*ncols+current.col];
    if(item) item->setFocus(FALSE);
    updateItem(current.row,current.col);
    }
  return 1;
  }


// We have the selection
long FXTable::onSelectionGained(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onSelectionGained(sender,sel,ptr);
  return 1;
  }


// We lost the selection
long FXTable::onSelectionLost(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onSelectionLost(sender,sel,ptr);
  killSelection(TRUE);
  return 1;
  }


// Somebody wants our selection
long FXTable::onSelectionRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Perhaps the target wants to supply its own data for the selection
  if(FXScrollArea::onSelectionRequest(sender,sel,ptr)) return 1;

  // Recognize the request?
  if(event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type){
    FXString string;

    // Get selected fragment
    extractText(string,selection.fm.row,selection.to.row,selection.fm.col,selection.to.col);

    // Return text of the selection as UTF-8
    if(event->target==utf8Type){
      FXTRACE((100,"Request UTF8\n"));
      setDNDData(FROM_SELECTION,event->target,string);
      return 1;
      }

    // Return text of the selection translated to 8859-1
    if(event->target==stringType || event->target==textType){
      FX88591Codec ascii;
      FXTRACE((100,"Request ASCII\n"));
      string=ascii.utf2mb(string);
      setDNDData(FROM_SELECTION,event->target,string);
      return 1;
      }

    // Return text of the selection translated to UTF-16
    if(event->target==utf16Type){
      FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
      FXTRACE((100,"Request UTF16\n"));
      string=unicode.utf2mb(string);
      setDNDData(FROM_SELECTION,event->target,string);
      return 1;
      }
    }

  return 0;
  }


// We now really do have the selection
long FXTable::onClipboardGained(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onClipboardGained(sender,sel,ptr);
  return 1;
  }


// We lost the selection somehow
long FXTable::onClipboardLost(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onClipboardLost(sender,sel,ptr);
  clipped.clear();
  return 1;
  }


// Somebody wants our selection
long FXTable::onClipboardRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Try handling it in base class first
  if(FXScrollArea::onClipboardRequest(sender,sel,ptr)) return 1;

  // Requested data from clipboard
  if(event->target==csvType || event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type){
    FXString string=clipped;

    // Expand newlines to CRLF on Windows
#ifdef WIN32
    unixToDos(string);
#endif

    // Return clipped text as CSV
    if(event->target==csvType){
      FXTRACE((100,"Request CSV\n"));
      string.substitute('\t',',',true);
      setDNDData(FROM_CLIPBOARD,event->target,string);
      return 1;
      }

    // Return clipped text as UTF-8
    if(event->target==utf8Type){
      FXTRACE((100,"Request UTF8\n"));
      setDNDData(FROM_CLIPBOARD,event->target,string);
      return 1;
      }

    // Return clipped text translated to 8859-1
    if(event->target==stringType || event->target==textType){
      FX88591Codec ascii;
      FXTRACE((100,"Request ASCII\n"));
      setDNDData(FROM_CLIPBOARD,event->target,ascii.utf2mb(clipped));
      return 1;
      }

    // Return text of the selection translated to UTF-16
    if(event->target==utf16Type){
      FXUTF16LECodec unicode;             // FIXME maybe other endianness for unix
      FXTRACE((100,"Request UTF16\n"));
      setDNDData(FROM_CLIPBOARD,event->target,unicode.utf2mb(clipped));
      return 1;
      }
    }
  return 0;
  }


// Update items that operate on the selection
long FXTable::onUpdHaveSelection(FXObject* sender,FXSelector,void*){
  sender->handle(this,isAnythingSelected()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Cut selection
long FXTable::onCmdCutSel(FXObject*,FXSelector,void*){
  if(isEditable()){
    if(isAnythingSelected()){
      FXDragType types[5];
      types[0]=stringType;
      types[1]=textType;
      types[2]=csvType;
      types[3]=utf8Type;
      types[4]=utf16Type;
      if(acquireClipboard(types,5)){
        extractText(clipped,selection.fm.row,selection.to.row,selection.fm.col,selection.to.col);
        removeRange(selection.fm.row,selection.to.row,selection.fm.col,selection.to.col,TRUE);
        }
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Copy selection
long FXTable::onCmdCopySel(FXObject*,FXSelector,void*){
  if(isAnythingSelected()){
    FXDragType types[5];
    types[0]=stringType;
    types[1]=textType;
    types[2]=csvType;
    types[3]=utf8Type;
    types[4]=utf16Type;
    if(acquireClipboard(types,5)){
      extractText(clipped,selection.fm.row,selection.to.row,selection.fm.col,selection.to.col);
      }
    }
  return 1;
  }


// Delete or clear selection
long FXTable::onCmdDeleteSel(FXObject*,FXSelector,void*){
  if(isEditable()){
    if(isAnythingSelected()){
      removeRange(selection.fm.row,selection.to.row,selection.fm.col,selection.to.col,TRUE);
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }




/*
        // FIXME we need to add another API called insertText which
        // optionally extends the table to fit ALL of the paste data
        // instead of only overlaying the selection
        //PROPOSED FIXME...
        FXint nr,nc;
        // Get rows and columns in the text
        countText(nr,nc,string,len,'\t','\n');
        //adjust the selection
        selection.to.row = selection.fm.row + nr;
        selection.to.col = selection.fm.col + nc;
        //ensure that the range does not go beyond the table limits
        if (selection.to.row>=nrows) selection.to.row = nrows-1;
        if (selection.to.col>=ncols) selection.to.col = ncols-1;
        //...PROPOSED FIXME

    // Delete existing selection
    if(hasSelection()){
      handle(this,FXSEL(SEL_COMMAND,ID_DELETE_SEL),NULL);
      }

   Paste ways:

   (1) Normal means figure size of data to paste, then replace that many
       cells from current cell toward right/bottom; create more rows and
       columns as needed.
   (2) Paste over selection means "overlay selection".
   (3) Insert paste, means move data down/rightward from current point
       where selection is pasted. If selected one or more rows, remove
       these rows and replace with new rows of clipboard.  If selected
       columns, remove selected columns and replace with columns of
       clipboard.  If selected block, clear block and fill with new
       rows and columns from clipboard, moving stuff right of the block
       more to the right, and stuff below block more to the bottom by
       inserting as many columns/rows as needed [but do not remove
       rows if new data from clipboard is smaller.
*/


// Paste selection
long FXTable::onCmdPasteSel(FXObject*,FXSelector,void*){
  if(isEditable()){
    if(isAnythingSelected()){
      FXTableRange range;
      FXString string;
      FXint nr,nc;

      // Where to paste
      range.fm=current;
      if(isAnythingSelected()){
        range.fm=selection.fm;
        }

      // First, try csv
      if(getDNDData(FROM_CLIPBOARD,csvType,string)){
        FXTRACE((100,"Paste CSV\n"));
#ifdef WIN32
        dosToUnix(string);
#endif
        countText(nr,nc,string,"\t,""\n");
        range.to.row=range.fm.row+nr-1;
        range.to.col=range.fm.col+nc-1;
        FXTRACE((100,"range.fm.row=%d range.to.row=%d range.fm.col=%d range.to.col=%d\n",range.fm.row,range.to.row,range.fm.col,range.to.col));
        overlayText(range.fm.row,range.to.row,range.fm.col,range.to.col,string,"\t,","\n",TRUE);
        selectRange(range.fm.row,range.to.row,range.fm.col,range.to.col,TRUE);
        return 1;
        }

      // First, try UTF-8
      if(getDNDData(FROM_CLIPBOARD,utf8Type,string)){
        FXTRACE((100,"Paste UTF8\n"));
#ifdef WIN32
        dosToUnix(string);
#endif
        countText(nr,nc,string,"\t,""\n");
        range.to.row=range.fm.row+nr-1;
        range.to.col=range.fm.col+nc-1;
        FXTRACE((100,"range.fm.row=%d range.to.row=%d range.fm.col=%d range.to.col=%d\n",range.fm.row,range.to.row,range.fm.col,range.to.col));
        overlayText(range.fm.row,range.to.row,range.fm.col,range.to.col,string,"\t,","\n",TRUE);
        selectRange(range.fm.row,range.to.row,range.fm.col,range.to.col,TRUE);
        return 1;
        }

      // Next, try UTF-16
      if(getDNDData(FROM_CLIPBOARD,utf16Type,string)){
        FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
        FXTRACE((100,"Paste UTF16\n"));
        string=unicode.mb2utf(string);
#ifdef WIN32
        dosToUnix(string);
#endif
        countText(nr,nc,string,"\t,""\n");
        range.to.row=range.fm.row+nr-1;
        range.to.col=range.fm.col+nc-1;
        FXTRACE((100,"range.fm.row=%d range.to.row=%d range.fm.col=%d range.to.col=%d\n",range.fm.row,range.to.row,range.fm.col,range.to.col));
        overlayText(range.fm.row,range.to.row,range.fm.col,range.to.col,string,"\t,","\n",TRUE);
        selectRange(range.fm.row,range.to.row,range.fm.col,range.to.col,TRUE);
        return 1;
        }

      // Next, try good old Latin-1
      if(getDNDData(FROM_CLIPBOARD,stringType,string)){
        FX88591Codec ascii;
        FXTRACE((100,"Paste ASCII\n"));
        string=ascii.mb2utf(string);
#ifdef WIN32
        dosToUnix(string);
#endif
        countText(nr,nc,string,"\t,""\n");
        range.to.row=range.fm.row+nr-1;
        range.to.col=range.fm.col+nc-1;
        FXTRACE((100,"range.fm.row=%d range.to.row=%d range.fm.col=%d range.to.col=%d\n",range.fm.row,range.to.row,range.fm.col,range.to.col));
        overlayText(range.fm.row,range.to.row,range.fm.col,range.to.col,string,"\t,","\n",TRUE);
        selectRange(range.fm.row,range.to.row,range.fm.col,range.to.col,TRUE);
        return 1;
        }
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Draw single cell, possibly spanning multiple rows,columns
void FXTable::drawCell(FXDC& dc,FXint sr,FXint er,FXint sc,FXint ec){
  register FXTableItem *item=cells[sr*ncols+sc];
  register FXint xl,xr,yt,yb;

  // Verify some stuff
  FXASSERT(0<=sc && sc<=ec && ec<ncols);
  FXASSERT(0<=sr && sr<=er && er<nrows);

  // Get cell bounds
  yt=rowHeader->getY()+rowHeader->getItemOffset(sr);
  yb=rowHeader->getY()+rowHeader->getItemOffset(er)+rowHeader->getItemSize(er);
  xl=colHeader->getX()+colHeader->getItemOffset(sc);
  xr=colHeader->getX()+colHeader->getItemOffset(ec)+colHeader->getItemSize(ec);

  // Non-empty
  if(xl<xr && yt<yb){

    // Drawing is clipped against cell rectangle AND the event
    // rectangle; note, grid lines are included this rectangle!
    dc.setClipRectangle(xl,yt,xr-xl+vgrid,yb-yt+hgrid);

    // Set background color
    if(isItemSelected(sr,sc)){
      dc.setForeground(selbackColor);                 // Selected item
      }
    else if(sr==er && sc==ec){
      dc.setForeground(cellBackColor[sr&1][sc&1]);    // Singular item
      }
    else{
      dc.setForeground(backColor);                    // Spanning item
      }

    // Draw the item, if there is one
    if(!item){
      dc.fillRectangle(xl+vgrid,yt+hgrid,xr-xl-vgrid,yb-yt-hgrid);
      }
    else{
      item->draw(this,dc,xl,yt,xr-xl,yb-yt);
      }

    // If focus in current cell, draw the focus
    if(hasFocus()){
      if(sr<=current.row && current.row<=er && sc<=current.col && current.col<=ec){
        dc.drawFocusRectangle(xl+2,yt+2,xr+vgrid-xl-4,yb+hgrid-yt-4);
        }
      }
    }
  }


// Draw range of cells
void FXTable::drawRange(FXDC& dc,FXint rlo,FXint rhi,FXint clo,FXint chi){
  register FXTableItem *item;
  register FXint r,c;
  for(r=rlo; r<=rhi; r++){
    for(c=clo; c<=chi; c++){
      item=cells[r*ncols+c];
      if(item){
        if((r!=rlo && cells[(r-1)*ncols+c]==item) || (c!=clo && cells[r*ncols+c-1]==item)) continue;
        drawCell(dc,startRow(r,c),endRow(r,c),startCol(r,c),endCol(r,c));
        }
      else{
        drawCell(dc,r,r,c,c);
        }
      }
    }
  }


// Draw horizontal grid lines
void FXTable::drawHGrid(FXDC& dc,FXint rlo,FXint rhi,FXint clo,FXint chi){
  register FXint r,c,xx,yy,ww,hh;
  register FXTableItem *item,*meti;
  dc.setForeground(gridColor);
  for(c=clo; c<=chi; c++){
    meti=item=NULL;
    xx=colHeader->getX()+colHeader->getItemOffset(c);
    ww=colHeader->getItemSize(c);
    for(r=rlo; r<=rhi; r++){
      yy=rowHeader->getY()+rowHeader->getItemOffset(r);
      hh=rowHeader->getItemSize(r);
      if(r==0 || (item=cells[r*ncols+c])==NULL || item!=meti){
        dc.fillRectangle(xx,yy,ww,1);
        }
      if(rhi==nrows-1){
        dc.fillRectangle(xx,yy+hh,ww,1);
        }
      meti=item;
      }
    }
  }


// Draw horizontal vertical lines
void FXTable::drawVGrid(FXDC& dc,FXint rlo,FXint rhi,FXint clo,FXint chi){
  register FXint r,c,xx,yy,ww,hh;
  register FXTableItem *item,*meti;
  dc.setForeground(gridColor);
  for(r=rlo; r<=rhi; r++){
    meti=item=NULL;
    yy=rowHeader->getY()+rowHeader->getItemOffset(r);
    hh=rowHeader->getItemSize(r);
    for(c=clo; c<=chi; c++){
      xx=colHeader->getX()+colHeader->getItemOffset(c);
      ww=colHeader->getItemSize(c);
      if(c==0 || (item=cells[r*ncols+c])==NULL || item!=meti){
        dc.fillRectangle(xx,yy,1,hh);
        }
      if(chi==ncols-1){
        dc.fillRectangle(xx+ww,yy,1,hh);
        }
      meti=item;
      }
    }
  }


// Draw table fragment
void FXTable::drawContents(FXDC& dc,FXint x,FXint y,FXint w,FXint h){
  register FXint fr,lr,fc,lc;

  // Find dirty part of table; note we need to back up to one row and
  // one column before the current cell, because of overlapping of
  // cell borders when grid lines are turned on.
  fc=colAtX(x)-1; lc=colAtX(x+w);
  fr=rowAtY(y)-1; lr=rowAtY(y+h);

  // Fix ranges
  if(fc<0) fc=0;
  if(fr<0) fr=0;
  if(lc>=ncols) lc=ncols-1;
  if(lr>=nrows) lr=nrows-1;

 // FXTRACE((100,"fc=%d lc=%d fr=%d lr=%d\n",fc,lc,fr,lr));

  FXASSERT(0<=fc && lc<ncols);
  FXASSERT(0<=fr && lr<nrows);

  // Draw horizontal grid lines
  if(hgrid){
    drawHGrid(dc,fr,lr,fc,lc);
    }

  // Draw vertical grid lines
  if(vgrid){
    drawVGrid(dc,fr,lr,fc,lc);
    }

  // Draw the cells
  drawRange(dc,fr,lr,fc,lc);
  }


// Draw exposed part of table
long FXTable::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint tablew,tableh;

  dc.setFont(font);

//dc.setForeground(FXRGB(255,0,0));
//dc.fillRectangle(event->rect.x,event->rect.y,event->rect.w,event->rect.h);

  // Left/bottom part of table
  tablew=colHeader->getX()+colHeader->getTotalSize();
  tableh=rowHeader->getY()+rowHeader->getTotalSize();

  // Fill background right and below the table
  dc.setForeground(backColor);
  dc.fillRectangle(tablew,0,width-tablew,height);
  dc.fillRectangle(0,tableh,tablew,height-tableh);

  // Draw main part
  drawContents(dc,event->rect.x,event->rect.y,event->rect.w,event->rect.h);
  return 1;
  }


// Key Press
long FXTable::onKeyPress(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;

  // Bounce to focus widget
  if(getFocus() && getFocus()->handle(sender,sel,ptr)) return 1;

  if(!isEnabled()) return 0;

  // Try target first
  if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;

  // Eat keystroke
  switch(event->code){
    case KEY_Control_L:
    case KEY_Control_R:
    case KEY_Shift_L:
    case KEY_Shift_R:
    case KEY_Alt_L:
    case KEY_Alt_R:
      //if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
      return 1;
    case KEY_Home:
    case KEY_KP_Home:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      if(event->state&CONTROLMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_MOVE_TOP),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MOVE_HOME),NULL);
        }
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_End:
    case KEY_KP_End:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      if(event->state&CONTROLMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_MOVE_BOTTOM),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MOVE_END),NULL);
        }
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Page_Up:
    case KEY_KP_Page_Up:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_PAGEUP),NULL);
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_PAGEDOWN),NULL);
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Up:
    case KEY_KP_Up:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_UP),NULL);
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Down:
    case KEY_KP_Down:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_DOWN),NULL);
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Tab:
      handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
      if(event->state&SHIFTMASK)
        handle(this,FXSEL(SEL_COMMAND,ID_MOVE_LEFT),NULL);
      else
        handle(this,FXSEL(SEL_COMMAND,ID_MOVE_RIGHT),NULL);
      handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
      return 1;
    case KEY_ISO_Left_Tab:
      handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_LEFT),NULL);
      handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
      return 1;
    case KEY_Right:
    case KEY_KP_Right:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_RIGHT),NULL);
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Left:
    case KEY_KP_Left:
      if(!(event->state&SHIFTMASK)){
        handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_MOVE_LEFT),NULL);
      if(event->state&SHIFTMASK){
        handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
        }
      else{
        handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
        }
      return 1;
    case KEY_Return:
    case KEY_KP_Enter:
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)&current);
      return 1;
    case KEY_F2:
      handle(this,FXSEL(SEL_COMMAND,ID_START_INPUT),NULL);
      return 1;
    case KEY_Escape:
      handle(this,FXSEL(SEL_COMMAND,ID_CANCEL_INPUT),NULL);
      return 1;
    case KEY_a:
      if(!(event->state&CONTROLMASK)) goto ins;
      handle(this,FXSEL(SEL_COMMAND,ID_SELECT_ALL),NULL);
      return 1;
    case KEY_x:
      if(!(event->state&CONTROLMASK)) goto ins;
    case KEY_F20:                               // Sun Cut key
      handle(this,FXSEL(SEL_COMMAND,ID_CUT_SEL),NULL);
      return 1;
    case KEY_c:
      if(!(event->state&CONTROLMASK)) goto ins;
    case KEY_F16:                               // Sun Copy key
      handle(this,FXSEL(SEL_COMMAND,ID_COPY_SEL),NULL);
      return 1;
    case KEY_v:
      if(!(event->state&CONTROLMASK)) goto ins;
    case KEY_F18:                               // Sun Paste key
      handle(this,FXSEL(SEL_COMMAND,ID_PASTE_SEL),NULL);
      return 1;
    default:
ins:  if((event->state&(CONTROLMASK|ALTMASK)) || ((FXuchar)event->text[0]<32)) return 0;
      handle(this,FXSEL(SEL_COMMAND,ID_START_INPUT),NULL);
      if(getFocus() && getFocus()->handle(sender,sel,ptr)) return 1;
      return 1;
    }
  return 0;
  }


// Key Release
long FXTable::onKeyRelease(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;

  // Bounce to focus widget
  if(getFocus() && getFocus()->handle(sender,sel,ptr)) return 1;

  if(!isEnabled()) return 0;

  flags|=FLAG_UPDATE;

  // Try target first
  if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;

  // Eat keystroke
  switch(event->code){
    case KEY_Control_L:
    case KEY_Control_R:
    case KEY_Shift_L:
    case KEY_Shift_R:
    case KEY_Alt_L:
    case KEY_Alt_R:
      //if(flags&FLAG_DODRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
      return 1;
    case KEY_Home:
    case KEY_KP_Home:
    case KEY_End:
    case KEY_KP_End:
    case KEY_Page_Up:
    case KEY_KP_Page_Up:
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
    case KEY_Left:
    case KEY_KP_Left:
    case KEY_Right:
    case KEY_KP_Right:
    case KEY_Up:
    case KEY_KP_Up:
    case KEY_Down:
    case KEY_KP_Down:
    case KEY_Tab:
    case KEY_ISO_Left_Tab:
      return 1;
    case KEY_Return:
    case KEY_KP_Enter:
    case KEY_Escape:
    case KEY_F2:
      return 1;
    case KEY_a:
    case KEY_F20:                             // Sun Cut key
    case KEY_F16:                             // Sun Copy key
    case KEY_F18:                             // Sun Paste key
      return 1;
    case KEY_x:
    case KEY_c:
    case KEY_v:
      if(event->state&CONTROLMASK) return 1;
    default:
      if((event->state&(CONTROLMASK|ALTMASK)) || ((FXuchar)event->text[0]<32)) return 0;
      return 1;
    }
  return 0;
  }


// Automatic scroll
long FXTable::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint r,c;
  FXScrollArea::onAutoScroll(sender,sel,ptr);
  switch(mode){
    case MOUSE_SELECT:
      c=colAtX(event->win_x);
      r=rowAtY(event->win_y);
      if(0<=r && 0<=c && r<nrows && c<ncols && (current.row!=r || current.col!=c)){
        extendSelection(r,c,TRUE);
        setCurrentItem(r,c,TRUE);
        }
      return 1;
    case MOUSE_DRAG:
      return 1;
    }
  return 1;
  }


// Mouse moved
long FXTable::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint r,c;
  switch(mode){
    case MOUSE_NONE:
      return 0;
    case MOUSE_SCROLL:
      setPosition(event->win_x-grabx,event->win_y-graby);
      return 1;
    case MOUSE_DRAG:
      return 1;
    case MOUSE_SELECT:
      if(startAutoScroll(event,FALSE)) return 1;  // FIXME scroll when near edge of scrollable area
      c=colAtX(event->win_x);
      r=rowAtY(event->win_y);
      if(0<=r && 0<=c && r<nrows && c<ncols && (current.row!=r || current.col!=c)){
        extendSelection(r,c,TRUE);
        setCurrentItem(r,c,TRUE);
        }
      return 1;
    }
  return 0;
  }


// Pressed button
long FXTable::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTablePos tablepos;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;

    // Cell being clicked on
    tablepos.row=rowAtY(event->win_y);
    tablepos.col=colAtX(event->win_x);

    // Outside table
    if(tablepos.row<0 || tablepos.row>=nrows || tablepos.col<0 || tablepos.col>=ncols){
      setCurrentItem(current.row,current.col,true);
      return 1;
      }

    // Change current item
    setCurrentItem(tablepos.row,tablepos.col,TRUE);

    // Select or deselect
    if(event->state&SHIFTMASK){
      if(0<=anchor.row && 0<=anchor.col){
        if(isItemEnabled(anchor.row,anchor.col)){
          extendSelection(current.row,current.col,TRUE);
          }
        }
      else{
        setAnchorItem(current.row,current.col);
        if(isItemEnabled(current.row,current.col)){
          extendSelection(current.row,current.col,TRUE);
          }
        }
      mode=MOUSE_SELECT;
      }
/*
    else if(event->state&CONTROLMASK){
      if(isItemEnabled(current.row,current.col)){
//        toggleItem(current.row,current.col,TRUE);
        }
      setAnchorItem(current.row,current.col);
      mode=MOUSE_SELECT;
      }
*/
    else{
      if(isItemEnabled(current.row,current.col)){
        killSelection(TRUE);
        setAnchorItem(current.row,current.col);
        extendSelection(current.row,current.col,TRUE);
        }
      else{
        setAnchorItem(current.row,current.col);
        }
      mode=MOUSE_SELECT;
      }
    flags&=~FLAG_UPDATE;
    flags|=FLAG_PRESSED;
    return 1;
    }
  return 0;
  }


// Released button
long FXTable::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    ungrab();
    flags&=~FLAG_PRESSED;
    flags|=FLAG_UPDATE;
    mode=MOUSE_NONE;
    stopAutoScroll();
    setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;

    // Scroll to make item visibke
    makePositionVisible(current.row,current.col);

    // Update anchor
    //setAnchorItem(current.row,current.col); // FIXME look into the selection stuff

    // Generate clicked callbacks
    if(event->click_count==1){
      handle(this,FXSEL(SEL_CLICKED,0),(void*)&current);
      }
    else if(event->click_count==2){
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)&current);
      }
    else if(event->click_count==3){
      handle(this,FXSEL(SEL_TRIPLECLICKED,0),(void*)&current);
      }

    // Command callback only when clicked on item
    if(0<=current.row && 0<=current.col && isItemEnabled(current.row,current.col)){
      handle(this,FXSEL(SEL_COMMAND,0),(void*)&current);
      }
    return 1;
    }
  return 0;
  }


// Pressed right button
long FXTable::onRightBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    flags|=FLAG_PRESSED;
    grabx=event->win_x-pos_x;
    graby=event->win_y-pos_y;
    mode=MOUSE_SCROLL;
    return 1;
    }
  return 0;
  }


// Released right button
long FXTable::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    ungrab();
    flags&=~FLAG_PRESSED;
    flags|=FLAG_UPDATE;
    mode=MOUSE_NONE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONRELEASE,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXTable::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onUngrabbed(sender,sel,ptr);
  flags&=~(FLAG_DODRAG|FLAG_TRYDRAG|FLAG_PRESSED|FLAG_CHANGED|FLAG_SCROLLING);
  flags|=FLAG_UPDATE;
  mode=MOUSE_NONE;
  stopAutoScroll();
  return 1;
  }


// Command message
long FXTable::onCommand(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
  }


// Clicked in list
long FXTable::onClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr);
  }


// Double clicked in list; ptr may or may not point to an item
long FXTable::onDoubleClicked(FXObject*,FXSelector,void* ptr){
  if(target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr)) return 1;
  handle(this,FXSEL(SEL_COMMAND,ID_START_INPUT),NULL);
  return 1;
  }


// Triple clicked in list; ptr may or may not point to an item
long FXTable::onTripleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
  }


// Toggle horizontal grid lines
long FXTable::onCmdHorzGrid(FXObject*,FXSelector,void*){
  showHorzGrid(!hgrid);
  return 1;
  }


long FXTable::onUpdHorzGrid(FXObject* sender,FXSelector,void*){
  sender->handle(this,hgrid?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle vertical grid lines
long FXTable::onCmdVertGrid(FXObject*,FXSelector,void*){
  showVertGrid(!vgrid);
  return 1;
  }


long FXTable::onUpdVertGrid(FXObject* sender,FXSelector,void*){
  sender->handle(this,vgrid?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Delete current column
long FXTable::onCmdDeleteColumn(FXObject*,FXSelector,void*){
  if(!isEditable() || current.col<0) return 1;
  removeColumns(current.col,1,TRUE);
  setCurrentItem(current.row,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Update delete current column
long FXTable::onUpdDeleteColumn(FXObject* sender,FXSelector,void*){
  if(0<=current.col && current.col<ncols && 0<ncols && isEditable())
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Delete current row
long FXTable::onCmdDeleteRow(FXObject*,FXSelector,void*){
  if(!isEditable() || current.row<0) return 1;
  removeRows(current.row,1,TRUE);
  setCurrentItem(current.row,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Update delete current row
long FXTable::onUpdDeleteRow(FXObject* sender,FXSelector,void*){
  if(0<=current.row && current.row<nrows && 0<nrows && isEditable())
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Insert new column at current
long FXTable::onCmdInsertColumn(FXObject*,FXSelector,void*){
  if(!isEditable()) return 1;
  insertColumns(current.col<0?ncols:current.col,1,TRUE);
  setCurrentItem(current.row,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Update insert column
long FXTable::onUpdInsertColumn(FXObject* sender,FXSelector,void*){
  sender->handle(this,isEditable()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Insert new row at current
long FXTable::onCmdInsertRow(FXObject*,FXSelector,void*){
  if(!isEditable()) return 1;
  insertRows(current.row<0?nrows:current.row,1,TRUE);
  setCurrentItem(current.row,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Update insert row
long FXTable::onUpdInsertRow(FXObject* sender,FXSelector,void*){
  sender->handle(this,isEditable()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Move to previous column
long FXTable::onCmdMoveLeft(FXObject*,FXSelector,void*){
  if(current.col<1) return 1;
  setCurrentItem(current.row,current.col-1,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to next column
long FXTable::onCmdMoveRight(FXObject*,FXSelector,void*){
  if(current.col>ncols-2) return 1;
  setCurrentItem(current.row,current.col+1,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to previous row
long FXTable::onCmdMoveUp(FXObject*,FXSelector,void*){
  if(current.row<1) return 1;
  setCurrentItem(current.row-1,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to next row
long FXTable::onCmdMoveDown(FXObject*,FXSelector,void*){
  if(current.row>nrows-2) return 1;
  setCurrentItem(current.row+1,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move begin of row
long FXTable::onCmdMoveHome(FXObject*,FXSelector,void*){
  setCurrentItem(current.row,0,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to end of row
long FXTable::onCmdMoveEnd(FXObject*,FXSelector,void*){
  setCurrentItem(current.row,ncols-1,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to top
long FXTable::onCmdMoveTop(FXObject*,FXSelector,void*){
  setCurrentItem(0,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to bottom
long FXTable::onCmdMoveBottom(FXObject*,FXSelector,void*){
  setCurrentItem(nrows-1,current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to next page
long FXTable::onCmdMovePageDown(FXObject*,FXSelector,void*){
  FXint nr=10;
  setCurrentItem(FXMIN(current.row+nr,nrows-1),current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Move to previous page
long FXTable::onCmdMovePageUp(FXObject*,FXSelector,void*){
  FXint nr=10;
  setCurrentItem(FXMAX(current.row-nr,0),current.col,TRUE);
  makePositionVisible(current.row,current.col);
  return 1;
  }


// Update select cell
long FXTable::onUpdSelectCell(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=current.row && 0<=current.col && current.row<nrows && current.col<ncols) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Select cell
long FXTable::onCmdSelectCell(FXObject*,FXSelector,void*){
  setAnchorItem(current.row,current.col);
  extendSelection(current.row,current.col,TRUE);
  return 1;
  }


// Update select row
long FXTable::onUpdSelectRow(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=current.row && current.row<nrows) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Select row
long FXTable::onCmdSelectRow(FXObject*,FXSelector,void*){
  if(!(options&TABLE_NO_ROWSELECT)){
    selectRow(current.row,TRUE);
    }
  return 1;
  }


// Update select column
long FXTable::onUpdSelectColumn(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=current.col && current.col<ncols) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Select column
long FXTable::onCmdSelectColumn(FXObject*,FXSelector,void*){
  if(!(options&TABLE_NO_COLSELECT)){
    selectColumn(current.col,TRUE);
    }
  return 1;
  }


// Select row with index
long FXTable::onCmdSelectRowIndex(FXObject*,FXSelector,void* ptr){
  if(!(options&TABLE_NO_ROWSELECT)){
    selectRow((FXint)(FXival)ptr,TRUE);
    }
  return 1;
  }


// Select column with index
long FXTable::onCmdSelectColumnIndex(FXObject*,FXSelector,void* ptr){
  if(!(options&TABLE_NO_COLSELECT)){
    selectColumn((FXint)(FXival)ptr,TRUE);
    }
  return 1;
  }


// Update select all
long FXTable::onUpdSelectAll(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<ncols && 0<nrows) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Select all cells
long FXTable::onCmdSelectAll(FXObject*,FXSelector,void*){
  setAnchorItem(0,0);
  extendSelection(nrows-1,ncols-1,TRUE);
  return 1;
  }


// Update deselect all
long FXTable::onUpdDeselectAll(FXObject* sender,FXSelector,void*){
  sender->handle(this,isAnythingSelected() ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }

// Deselect all cells
long FXTable::onCmdDeselectAll(FXObject*,FXSelector,void*){
  killSelection(TRUE);
  return 1;
  }


// Mark
long FXTable::onCmdMark(FXObject*,FXSelector,void*){
  setAnchorItem(current.row,current.col);
  return 1;
  }


// Extend
long FXTable::onCmdExtend(FXObject*,FXSelector,void*){
  extendSelection(current.row,current.col,TRUE);
  return 1;
  }


// Editable toggle
long FXTable::onCmdToggleEditable(FXObject*,FXSelector,void*){
  setEditable(!isEditable());
  return 1;
  }


// Update editable toggle
long FXTable::onUpdToggleEditable(FXObject* sender,FXSelector,void*){
  sender->handle(this,isEditable()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


// Change table size to nr x nc
void FXTable::setTableSize(FXint nr,FXint nc,FXbool notify){
  register FXTableItem *item;
  register FXint r,c;
  FXTableRange tablerange;

  // Must be in range
  if(nr<0 || nc<0){ fxerror("%s::setTableSize: argument out of range.\n",getClassName()); }

  // End editing
  cancelInput();

  // Notify items will be deleted
  if(notify && target){
    tablerange.fm.row=0;
    tablerange.fm.col=0;
    tablerange.to.row=nrows-1;
    tablerange.to.col=ncols-1;
    target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)&tablerange);
    }

  // Free all cells
  for(r=0; r<nrows; r++){
    for(c=0; c<ncols; c++){
      item=cells[r*ncols+c];
      if(item && (r==0 || cells[(r-1)*ncols+c]!=item) && (c==0 || cells[r*ncols+c-1]!=item)){
        delete item;
        }
      }
    }

  // Clear headers
  rowHeader->clearItems();
  colHeader->clearItems();

  // Resize it now
  if(!FXRESIZE(&cells,FXTableItem*,nr*nc+1)){
    fxerror("%s::setTableSize: out of memory.\n",getClassName());
    }

  // Initialize cell array
  for(r=0; r<nr; r++){
    for(c=0; c<nc; c++){
      cells[r*nc+c]=NULL;
      }
    }

  // Initialize row headers
  for(r=0; r<nr; r++){
    rowHeader->appendItem(FXString::null,NULL,defRowHeight);
    }

  // Initialize column headers
  for(c=0; c<nc; c++){
    colHeader->appendItem(FXString::null,NULL,defColWidth);
    }

  // Update row headers
  if(options&TABLE_ROW_RENUMBER) updateRowNumbers(0,nr);

  // Update column headers
  if(options&TABLE_COL_RENUMBER) updateColumnNumbers(0,nc);

  // Set size
  nrows=nr;
  ncols=nc;

  // Fix up anchor, extent and current
  anchor.col=-1;
  anchor.row=-1;
  current.col=-1;
  current.row=-1;

  // Fix up selection
  selection.fm.row=-1;
  selection.fm.col=-1;
  selection.to.row=-1;
  selection.to.col=-1;

  // Notify items have been inserted
  if(notify && target){
    tablerange.fm.row=0;
    tablerange.fm.col=0;
    tablerange.to.row=nrows-1;
    tablerange.to.col=ncols-1;
    target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)&tablerange);
    }

  // Current item have changed
  if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current); }

  // Redo layout
  recalc();
  }


// Insert a row
void FXTable::insertRows(FXint row,FXint nr,FXbool notify){
  register FXint oldrow=current.row;
  register FXint r,c,n;
  FXTableItem **oldcells=cells;
  FXTableRange tablerange;

  // Nothing to do
  if(nr<1) return;

  // Must be in range
  if(row<0 || row>nrows){ fxerror("%s::insertRows: row out of range.\n",getClassName()); }

  // Space for nr new rows
  n=nrows+nr;

  // Initialize row headers
  for(r=row; r<row+nr; r++){
    rowHeader->insertItem(r,FXString::null,NULL,defRowHeight);
    }

  // Update row headers
  if(options&TABLE_ROW_RENUMBER) updateRowNumbers(row,n);

  // Allocate new table
  if(!FXMALLOC(&cells,FXTableItem*,n*ncols+1)){
    fxerror("%s::insertRows: out of memory.\n",getClassName());
    }

  // Copy first part
  for(r=0; r<row; r++){
    for(c=0; c<ncols; c++){
      cells[r*ncols+c]=oldcells[r*ncols+c];
      }
    }

  // Initialize middle part; cells spanning over current row are not split
  for(c=0; c<ncols; c++){
    if(0<row && row<nrows && oldcells[(row-1)*ncols+c]==oldcells[row*ncols+c]){
      for(r=row; r<row+nr; r++){
        cells[r*ncols+c]=oldcells[row*ncols+c];
        }
      }
    else{
      for(r=row; r<row+nr; r++){
        cells[r*ncols+c]=NULL;
        }
      }
    }

  // Copy last part
  for(r=row; r<nrows; r++){
    for(c=0; c<ncols; c++){
      cells[(r+nr)*ncols+c]=oldcells[r*ncols+c];
      }
    }

  // Free old table
  FXFREE(&oldcells);

  nrows=n;

  FXTRACE((100,"nrows=%d\n",nrows));

  // Fix up anchor, extent, and current
  if(anchor.row>=row) anchor.row+=nr;
  if(current.row>=row) current.row+=nr;
  if(current.row<0 && nrows==nr) current.row=0;

  // Fix up extent of edited cell
  if(input.fm.row>=row) input.fm.row+=nr;
  if(input.to.row>=row) input.to.row+=nr;

  // Fix up selection
  if(selection.fm.row>=row) selection.fm.row+=nr;
  if(selection.to.row>=row) selection.to.row+=nr;

  FXASSERT(-1<=anchor.row && anchor.row<nrows);
  FXASSERT(-1<=current.row && current.row<nrows);

  // Notify items have been inserted
  if(notify && target){
    tablerange.fm.row=row;
    tablerange.fm.col=0;
    tablerange.to.row=row+nr-1;
    tablerange.to.col=ncols-1;
    target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)&tablerange);
    }

  // Current item may have changed
  if(oldrow!=current.row){
    if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current); }
    }

  // Redo layout
  recalc();
  }


// Insert a column
void FXTable::insertColumns(FXint col,FXint nc,FXbool notify){
  register FXint oldcol=current.col;
  register FXint r,c,n;
  FXTableItem **oldcells=cells;
  FXTableRange tablerange;

  // Nothing to do
  if(nc<1) return;

  // Must be in range
  if(col<0 || col>ncols){ fxerror("%s::insertColumns: column out of range.\n",getClassName()); }

  // Space for nr new rows
  n=ncols+nc;

  // Initialize column headers
  for(c=col; c<col+nc; c++){
    colHeader->insertItem(c,FXString::null,NULL,defColWidth);
    }

  // Update column headers
  if(options&TABLE_COL_RENUMBER) updateColumnNumbers(col,n);

  // Allocate new table
  if(!FXMALLOC(&cells,FXTableItem*,nrows*n+1)){
    fxerror("%s::insertColumns: out of memory.\n",getClassName());
    }

  // Copy first part
  for(c=0; c<col; c++){
    for(r=0; r<nrows; r++){
      cells[r*n+c]=oldcells[r*ncols+c];
      }
    }

  // Initialize middle part; cells spanning over current column are not split
  for(r=0; r<nrows; r++){
    if(0<col && col<ncols && oldcells[r*ncols+col-1]==oldcells[r*ncols+col]){
      for(c=col; c<col+nc; c++){
        cells[r*n+c]=oldcells[r*ncols+col];
        }
      }
    else{
      for(c=col; c<col+nc; c++){
        cells[r*n+c]=NULL;
        }
      }
    }

  // Copy last part
  for(c=col; c<ncols; c++){
    for(r=0; r<nrows; r++){
      cells[r*n+nc+c]=oldcells[r*ncols+c];
      }
    }

  // Free old table
  FXFREE(&oldcells);

  ncols=n;

  FXTRACE((100,"ncols=%d\n",ncols));

  // Fix up anchor, extent, and current
  if(anchor.col>=col) anchor.col+=nc;
  if(current.col>=col) current.col+=nc;
  if(current.col<0 && ncols==nc) current.col=0;

  // Fix up extent of edited cell
  if(input.fm.col>=col) input.fm.col+=nc;
  if(input.to.col>=col) input.to.col+=nc;

  // Fix up selection
  if(selection.fm.col>=col) selection.fm.col+=nc;
  if(selection.to.col>=col) selection.to.col+=nc;

  FXASSERT(-1<=anchor.col && anchor.col<ncols);
  FXASSERT(-1<=current.col && current.col<ncols);

  // Notify items have been inserted
  if(notify && target){
    tablerange.fm.row=0;
    tablerange.fm.col=col;
    tablerange.to.row=nrows-1;
    tablerange.to.col=col+nc-1;
    target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)&tablerange);
    }

  // Current item may have changed
  if(oldcol!=current.col){
    if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current); }
    }

  // Redo layout
  recalc();
  }


// Remove rows of cells
void FXTable::removeRows(FXint row,FXint nr,FXbool notify){
  register FXint oldrow=current.row;
  register FXTableItem *item;
  register FXint r,c,n;
  FXTableItem **oldcells=cells;
  FXTableRange tablerange;

  // Nothing to do
  if(nr<1) return;

  // Must be in range
  if(row<0 || row+nr>nrows){ fxerror("%s::removeRows: row out of range.\n",getClassName()); }

  // End editing
  if(row<=input.fm.row && input.to.row<row+nr){
    cancelInput();
    }

  // Notify items will be deleted
  if(notify && target){
    tablerange.fm.row=row;
    tablerange.fm.col=0;
    tablerange.to.row=row+nr-1;
    tablerange.to.col=ncols-1;
    target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)&tablerange);
    }

  // Number removed
  n=nrows-nr;

  // Allocate new table
  if(!FXMALLOC(&cells,FXTableItem*,n*ncols+1)){
    fxerror("%s::removeRows: out of memory.\n",getClassName());
    }

  // Copy first part
  for(r=0; r<row; r++){
    for(c=0; c<ncols; c++){
      cells[r*ncols+c]=oldcells[r*ncols+c];
      }
    }

  // Delete those items fully contained in the deleted range
  for(r=row; r<row+nr; r++){
    for(c=0; c<ncols; c++){
      item=oldcells[r*ncols+c];
      if(item && (r==0 || oldcells[(r-1)*ncols+c]!=item) && (c==0 || oldcells[r*ncols+c-1]!=item) && (row+nr==nrows || oldcells[(row+nr)*ncols+c]!=item)){
        FXTRACE((150,"delete item %s\n",item->getText().text()));
        delete item;
        }
      }
    }

  // Copy last part
  for(r=row+nr; r<nrows; r++){
    for(c=0; c<ncols; c++){
      cells[(r-nr)*ncols+c]=oldcells[r*ncols+c];
      }
    }

  // Free old table
  FXFREE(&oldcells);

  // Remove row headers
  for(r=row+nr-1; r>=row; r--){
    rowHeader->removeItem(r);
    }

  // Update row headers
  if(options&TABLE_ROW_RENUMBER) updateRowNumbers(row,n);

  // Fix up anchor and current
  if(anchor.row>=row+nr) anchor.row-=nr; else if(anchor.row>=n) anchor.row=n-1;
  if(current.row>=row+nr) current.row-=nr; else if(current.row>=n) current.row=n-1;

  // Fix up edited cell
  if(input.fm.row>=row+nr) input.fm.row-=nr; else if(input.fm.row>=n) input.fm.row=n-1;
  if(input.to.row>=row+nr) input.to.row-=nr; else if(input.to.row>=n) input.to.row=n-1;

  // Fix up selection
  if(row+nr<=selection.to.row){
    selection.to.row-=nr;
    if(row+nr<=selection.fm.row) selection.fm.row-=nr;
    else if(row<=selection.fm.row) selection.fm.row=row;
    }
  else if(selection.fm.row<row){
    if(row<=selection.to.row) selection.to.row=row-1;
    }
  else{
    selection.fm.row=-1;
    selection.to.row=-1;
    }

  nrows=n;

  FXTRACE((100,"nrows=%d\n",nrows));

  FXASSERT(-1<=anchor.row && anchor.row<nrows);
  FXASSERT(-1<=current.row && current.row<nrows);

  // Current item may have changed
  if(row<=oldrow){
    if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current); }
    }

  // Redo layout
  recalc();
  }


// Remove columns of cells
void FXTable::removeColumns(FXint col,FXint nc,FXbool notify){
  register FXint oldcol=current.col;
  register FXTableItem *item;
  register FXint r,c,n;
  FXTableItem **oldcells=cells;
  FXTableRange tablerange;

  // Nothing to do
  if(nc<1) return;

  // Must be in range
  if(col<0 || col+nc>ncols){ fxerror("%s::removeColumns: column out of range.\n",getClassName()); }

  // End editing
  if(col<=input.fm.col && input.to.col<col+nc){
    cancelInput();
    }

  // Notify items will be deleted
  if(notify && target){
    tablerange.fm.row=0;
    tablerange.fm.col=col;
    tablerange.to.row=nrows-1;
    tablerange.to.col=col+nc-1;
    target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)&tablerange);
    }

  // Number removed
  n=ncols-nc;

  // Allocate new table
  if(!FXMALLOC(&cells,FXTableItem*,nrows*n+1)){
    fxerror("%s::removeColumns: out of memory.\n",getClassName());
    }

  // Copy first part
  for(r=0; r<nrows; r++){
    for(c=0; c<col; c++){
      cells[r*n+c]=oldcells[r*ncols+c];
      }
    }

  // Delete those items fully contained in the deleted range
  for(r=0; r<nrows; r++){
    for(c=col; c<col+nc; c++){
      item=oldcells[r*ncols+c];
      if(item && (r==0 || oldcells[(r-1)*ncols+c]!=item) && (c==0 || oldcells[r*ncols+c-1]!=item) && (col+nc==ncols || oldcells[r*ncols+col+nc]!=item)){
        FXTRACE((150,"delete item %s\n",item->getText().text()));
        delete item;
        }
      }
    }

  // Copy last part
  for(r=0; r<nrows; r++){
    for(c=col+nc; c<ncols; c++){
      cells[r*n+c-nc]=oldcells[r*ncols+c];
      }
    }

  // Free old table
  FXFREE(&oldcells);

  // Remove column headers
  for(c=col+nc-1; c>=col; c--){
    colHeader->removeItem(c);
    }

  // Update column headers
  if(options&TABLE_COL_RENUMBER) updateColumnNumbers(col,n);

  // Fix up anchor and current
  if(anchor.col>=col+nc) anchor.col-=nc; else if(anchor.col>=n) anchor.col=n-1;
  if(current.col>=col+nc) current.col-=nc; else if(current.col>=n) current.col=n-1;

  // Fix up edited cell
  if(input.fm.col>=col+nc) input.fm.col-=nc; else if(input.fm.col>=n) input.fm.col=n-1;
  if(input.to.col>=col+nc) input.to.col-=nc; else if(input.to.col>=n) input.to.col=n-1;

  // Fix up selection
  if(col+nc<=selection.to.col){
    selection.to.col-=nc;
    if(col+nc<=selection.fm.col) selection.fm.col-=nc;
    else if(col<=selection.fm.col) selection.fm.col=col;
    }
  else if(selection.fm.col<col){
    if(col<=selection.to.col) selection.to.col=col-1;
    }
  else{
    selection.fm.col=-1;
    selection.to.col=-1;
    }

  ncols=n;

  FXTRACE((100,"ncols=%d\n",ncols));

  FXASSERT(-1<=anchor.col && anchor.col<ncols);
  FXASSERT(-1<=current.col && current.col<ncols);

  // Current item may have changed
  if(col<=oldcol){
    if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current); }
    }

  // Redo layout
  recalc();
  }


// Return the item at the given index
FXTableItem *FXTable::getItem(FXint row,FXint col) const {
  if(row<0 || col<0 || nrows<row || ncols<=col){ fxerror("%s::getItem: index out of range.\n",getClassName()); }
  return cells[row*ncols+col];
  }


// Replace item with another (may be NULL)
void FXTable::setItem(FXint row,FXint col,FXTableItem* item,FXbool notify){
  register FXint sr,er,sc,ec,r,c;
  FXTableRange tablerange;

  // Must be in range
  if(row<0 || col<0 || nrows<row || ncols<=col){ fxerror("%s::setItem: index out of range.\n",getClassName()); }

  // Extent of cell
  sr=startRow(row,col); er=endRow(row,col);
  sc=startCol(row,col); ec=endCol(row,col);

  // End editing
  if(sr<=input.fm.row && sc<=input.fm.col && input.to.row<=er && input.to.col<=ec){
    cancelInput();
    }

  // Notify item will be replaced
  if(notify && target){
    tablerange.fm.row=sr;
    tablerange.fm.col=sc;
    tablerange.to.row=er;
    tablerange.to.col=ec;
    target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&tablerange);
    }

  // Delete cell
  delete cells[sr*ncols+sc];

  // Assign new cell
  for(r=sr; r<=er; r++){
    for(c=sc; c<=ec; c++){
      cells[r*ncols+c]=item;
      }
    }

  // Repaint these cells
  updateRange(sr,er,sc,ec);
  }


// Extract item from table
FXTableItem* FXTable::extractItem(FXint row,FXint col,FXbool notify){
  register FXint sr,er,sc,ec,r,c;
  register FXTableItem *result;
  FXTableRange tablerange;

  // Must be in range
  if(row<0 || col<0 || nrows<=row || ncols<=col){ fxerror("%s::extractItem: index out of range.\n",getClassName()); }

  // Extent of cell
  sr=startRow(row,col); er=endRow(row,col);
  sc=startCol(row,col); ec=endCol(row,col);

  // End editing
  if(sr<=input.fm.row && sc<=input.fm.col && input.to.row<=er && input.to.col<=ec){
    cancelInput();
    }

  // Notify item will be replaced
  if(notify && target){
    tablerange.fm.row=sr; tablerange.to.row=er;
    tablerange.fm.col=sc; tablerange.to.col=ec;
    target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&tablerange);
    }

  // Delete cell
  result=cells[sr*ncols+sc];

  // Clear entries
  for(r=sr; r<=er; r++){
    for(c=sc; c<=ec; c++){
      cells[r*ncols+c]=NULL;
      }
    }

  // Repaint these cells
  updateRange(sr,er,sc,ec);

  // Return item
  return result;
  }


// Remove cell, i.e. replace cell by NULL
void FXTable::removeItem(FXint row,FXint col,FXbool notify){
  register FXint sr,er,sc,ec,r,c;
  FXTableRange tablerange;

  // Must be in range
  if(row<0 || col<0 || nrows<=row || ncols<=col){ fxerror("%s::removeItem: index out of range.\n",getClassName()); }

  // Extent of cell
  sr=startRow(row,col); er=endRow(row,col);
  sc=startCol(row,col); ec=endCol(row,col);

  // End editing
  if(sr<=input.fm.row && sc<=input.fm.col && input.to.row<=er && input.to.col<=ec){
    cancelInput();
    }

  // Notify item will be replaced
  if(notify && target){
    tablerange.fm.row=sr; tablerange.to.row=er;
    tablerange.fm.col=sc; tablerange.to.col=ec;
    target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&tablerange);
    }

  // Delete cell
  delete cells[sr*ncols+sc];

  // Clear entries
  for(r=sr; r<=er; r++){
    for(c=sc; c<=ec; c++){
      cells[r*ncols+c]=NULL;
      }
    }

  // Repaint these cells
  updateRange(sr,er,sc,ec);
  }


// Clear all cells in the given range
void FXTable::removeRange(FXint startrow,FXint endrow,FXint startcol,FXint endcol,FXbool notify){
  register FXint r,c;

  // Verify range
  if(startrow<0 || startcol<0 || nrows<=endrow || ncols<=endcol){ fxerror("%s::removeRange: index out of range.\n",getClassName()); }

  // Free all cells
  for(r=startrow; r<=endrow; r++){
    for(c=startcol; c<=endcol; c++){
      removeItem(r,c,notify);
      }
    }
  }


// Clear all items from table
void FXTable::clearItems(FXbool notify){
  register FXTableItem *item;
  register FXint r,c;
  FXTableRange tablerange;

  // End editing
  if(0<=input.fm.row && 0<=input.fm.col){
    cancelInput();
    }

  // Notify item will be deleted
  if(notify && target){
    tablerange.fm.row=0; tablerange.to.row=nrows-1;
    tablerange.fm.col=0; tablerange.to.col=ncols-1;
    target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)&tablerange);
    }

  // Free all cells
  for(r=0; r<nrows; r++){
    for(c=0; c<ncols; c++){
      item=cells[r*ncols+c];
      if(item && (r==0 || cells[(r-1)*ncols+c]!=item) && (c==0 || cells[r*ncols+c-1]!=item)){
        delete item;
        }
      }
    }

  // Clear headers
  rowHeader->clearItems();
  colHeader->clearItems();

  // Resize arrays
  FXRESIZE(&cells,FXTableItem*,1);

  // Fix up arrays
  cells[0]=NULL;

  // Number of rows and columns
  nrows=0;
  ncols=0;

  // Fix up anchor, extent and current
  anchor.col=-1;
  anchor.row=-1;
  current.col=-1;
  current.row=-1;

  // Fix up selection
  selection.fm.row=-1;
  selection.fm.col=-1;
  selection.to.row=-1;
  selection.to.col=-1;

  // Notify of change of current item
  if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)&current); }

  // Redo layout
  recalc();
  }


/*

  /// Merge cells into one single multi-column cell
  virtual void mergeCols(FXint row,FXint col,FXint nc,FXbool notify=FALSE);

  /// Merge cells into one single multi-row cell
  virtual void mergeRows(FXint row,FXint col,FXint nr,FXbool notify=FALSE);

  /// Split multi-column cell into single cells
  virtual void splitCols(FXint row,FXint col,FXbool notify=FALSE);

  /// Split multi-row cell into single cells
  virtual void splitRows(FXint row,FXint col,FXbool notify=FALSE);

// Merge cells into one single multi-column cell
void FXTable::mergeCols(FXint row,FXint col,FXint nc,FXbool notify){
  register FXint sr,er,ec,r,c;
  register FXTableItem *item,*it;
  if(row<0 || row>=nrows || col<0 || nc<1 || col+nc>ncols){ fxerror("%s::mergeCols: argument out of range\n",getClassName()); }
  sr=startRow(row,col);
  er=endRow(row,col);
  ec=col+nc-1;
  item=cells[row*ncols+col];
  for(r=sr; r<=er; r++){
    for(c=col+1; c<=ec; c++){
      it=cells[r*ncols+c];
      if(it && it!=item){
        removeCell(r,c,notify);
        }
      cells[r*ncols+c]=item;
      }
    }
  }


// Merge cells into one single multi-row cell
void FXTable::mergeRows(FXint row,FXint col,FXint nr,FXbool notify){
  register FXint er,sc,ec,r,c;
  register FXTableItem *item,*it;
  if(col<0 || col>=ncols || row<0 || nr<1 || row+nr>nrows){ fxerror("%s::mergeRows: argument out of range\n",getClassName()); }
  sc=startCol(row,col);
  ec=endCol(row,col);
  er=row+nr-1;
  item=cells[row*ncols+col];
  for(r=row+1; r<=er; r++){
    for(c=sc; c<=ec; c++){
      it=cells[r*ncols+c];
      if(it && it!=item){
        removeCell(r,c,notify);
        }
      cells[r*ncols+c]=item;
      }
    }
  }


// Split multi-column cell into single cells
void FXTable::splitCols(FXint row,FXint col,FXbool notify){
  if(row<0 || row>=nrows || col<0 || col>=ncols){ fxerror("%s::splitCols: argument out of range\n",getClassName()); }
  }


// Split multi-row cell into single cells
void FXTable::splitRows(FXint row,FXint col,FXbool notify){
  if(col<0 || col>=ncols || row<0 || row>=nrows){ fxerror("%s::splitRows: argument out of range\n",getClassName()); }
  }

*/


// Change column header height mode to fixed or variable
void FXTable::setColumnHeaderMode(FXuint hint){
  register FXuint hints=(colHeader->getLayoutHints()&~LAYOUT_FIX_HEIGHT) | (hint&LAYOUT_FIX_HEIGHT);
  colHeader->setLayoutHints(hints);
  }

// Return column header height mode
FXuint FXTable::getColumnHeaderMode() const {
  return (colHeader->getLayoutHints()&LAYOUT_FIX_HEIGHT);
  }


// Change row header width mode to fixed or variable
void FXTable::setRowHeaderMode(FXuint hint){
  register FXuint hints=(rowHeader->getLayoutHints()&~LAYOUT_FIX_WIDTH) | (hint&LAYOUT_FIX_WIDTH);
  rowHeader->setLayoutHints(hints);
  }

// Return row header width mode
FXuint FXTable::getRowHeaderMode() const {
  return (rowHeader->getLayoutHints()&LAYOUT_FIX_WIDTH);
  }


// Change column header height
void FXTable::setColumnHeaderHeight(FXint h){
  if(colHeader->getHeight()!=h){
    colHeader->setHeight(h);
    recalc();
    }
  }

// Return column header height
FXint FXTable::getColumnHeaderHeight() const {
  return colHeader->getHeight();
  }


// Change row header width
void FXTable::setRowHeaderWidth(FXint w){
  if(rowHeader->getWidth()!=w){
    rowHeader->setWidth(w);
    recalc();
    }
  }


// Return row header width
FXint FXTable::getRowHeaderWidth() const {
  return rowHeader->getWidth();
  }


// X coordinate of column c
FXint FXTable::getColumnX(FXint col) const {
  return colHeader->getX()+colHeader->getItemOffset(col);
  }


// Y coordinate of row r
FXint FXTable::getRowY(FXint row) const {
  return rowHeader->getY()+rowHeader->getItemOffset(row);
  }


// Change width of custom column
void FXTable::setColumnWidth(FXint col,FXint cwidth){
  if(colHeader->getItemSize(col)!=cwidth){
    colHeader->setItemSize(col,cwidth);
    update();
    }
  }


// Change height of custom row
void FXTable::setRowHeight(FXint row,FXint rheight){
  if(rowHeader->getItemSize(row)!=rheight){
    rowHeader->setItemSize(row,rheight);
    update();
    }
  }


// Get width of custom column
FXint FXTable::getColumnWidth(FXint col) const {
  return colHeader->getItemSize(col);
  }


// Get height of custom row
FXint FXTable::getRowHeight(FXint row) const {
  return rowHeader->getItemSize(row);
  }


// Change default column width
void FXTable::setDefColumnWidth(FXint cwidth){
  if(defColWidth!=cwidth){
    defColWidth=cwidth;
    recalc();
    }
  }


// Change default row height
void FXTable::setDefRowHeight(FXint rheight){
  if(defRowHeight!=rheight){
    defRowHeight=rheight;
    recalc();
    }
  }


// Return minimum row height
FXint FXTable::getMinRowHeight(FXint r) const {
  register FXTableItem *item;
  register FXint h,c,t;
  if(r<0 || r>=nrows){ fxerror("%s::getMinRowHeight: row out of range\n",getClassName()); }
  for(c=0,h=0; c<ncols; c++){
    item=cells[r*ncols+c];
    if(item && (r==0 || cells[(r-1)*ncols+c]!=item) && (r==nrows-1 || cells[(r+1)*ncols+c]!=item)){
      t=item->getHeight(this);
      if(t>h) h=t;
      }
    }
  return h;
  }


// Return minimum column width
FXint FXTable::getMinColumnWidth(FXint c) const {
  register FXTableItem *item;
  register FXint w,r,t;
  if(c<0 || c>=ncols){ fxerror("%s::getMinColumnWidth: column out of range\n",getClassName()); }
  for(r=0,w=0; r<nrows; r++){
    item=cells[r*ncols+c];
    if(item && (c==0 || cells[r*ncols+c-1]!=item) && (c==ncols-1 || cells[r*ncols+c+1]!=item)){
      t=item->getWidth(this);
      if(t>w) w=t;
      }
    }
  return w;
  }


// Fit row heights to contents
void FXTable::fitRowsToContents(FXint row,FXint nr){
  register FXint r;
  for(r=row; r<row+nr; r++){
    setRowHeight(r,getMinRowHeight(r));
    }
  }


// Fit column widths to contents
void FXTable::fitColumnsToContents(FXint col,FXint nc){
  register FXint c;
  for(c=col; c<col+nc; c++){
    setColumnWidth(c,getMinColumnWidth(c));
    }
  }


// Change the font
void FXTable::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    }
  }


// Change top margin
void FXTable::setMarginTop(FXint mt){
  if(margintop!=mt){
    margintop=mt;
    recalc();
    update();
    }
  }


// Change bottom margin
void FXTable::setMarginBottom(FXint mb){
  if(marginbottom!=mb){
    marginbottom=mb;
    recalc();
    update();
    }
  }


// Change left margin
void FXTable::setMarginLeft(FXint ml){
  if(marginleft!=ml){
    marginleft=ml;
    recalc();
    update();
    }
  }


// Change right margin
void FXTable::setMarginRight(FXint mr){
  if(marginright!=mr){
    marginright=mr;
    recalc();
    update();
    }
  }


// Set text color
void FXTable::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set base color
void FXTable::setBaseColor(FXColor clr){
  if(clr!=baseColor){
    baseColor=clr;
    update();
    }
  }


// Set highlight color
void FXTable::setHiliteColor(FXColor clr){
  if(clr!=hiliteColor){
    hiliteColor=clr;
    update();
    }
  }


// Set shadow color
void FXTable::setShadowColor(FXColor clr){
  if(clr!=shadowColor){
    shadowColor=clr;
    update();
    }
  }


// Set border color
void FXTable::setBorderColor(FXColor clr){
  if(clr!=borderColor){
    borderColor=clr;
    update();
    }
  }


// Set select background color
void FXTable::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    update();
    }
  }


// Set selected text color
void FXTable::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    update();
    }
  }


// Change grid color
void FXTable::setGridColor(FXColor clr){
  if(clr!=gridColor){
    gridColor=clr;
    update();
    }
  }


// Change stipple color
void FXTable::setStippleColor(FXColor clr){
  if(clr!=stippleColor){
    stippleColor=clr;
    update();
    }
  }

// Change cell border color
void FXTable::setCellBorderColor(FXColor clr){
  if(clr!=cellBorderColor){
    cellBorderColor=clr;
    update();
    }
  }

// Set cell color
void FXTable::setCellColor(FXint r,FXint c,FXColor clr){
  if(clr!=cellBackColor[r&1][c&1]){
    cellBackColor[r&1][c&1]=clr;
    update();
    }
  }


// Get cell color
FXColor FXTable::getCellColor(FXint r,FXint c) const {
  return cellBackColor[r&1][c&1];
  }


// Change list style
void FXTable::setTableStyle(FXuint style){
  FXuint opts=(options&~TABLE_MASK) | (style&TABLE_MASK);
  FXuint hs;
  if(opts!=options){
    hs=HEADER_HORIZONTAL|HEADER_TRACKING|HEADER_BUTTON;
    if(opts&TABLE_COL_SIZABLE) hs|=HEADER_RESIZE;
    if(opts&TABLE_NO_COLSELECT) hs&=~HEADER_BUTTON;
    colHeader->setHeaderStyle(hs);
    hs=HEADER_VERTICAL|HEADER_TRACKING|HEADER_BUTTON;
    if(opts&TABLE_ROW_SIZABLE) hs|=HEADER_RESIZE;
    if(opts&TABLE_NO_ROWSELECT) hs&=~HEADER_BUTTON;
    rowHeader->setHeaderStyle(hs);
    options=opts;
    }
  }


// Get list style
FXuint FXTable::getTableStyle() const {
  return (options&TABLE_MASK);
  }


// Set column renumbering
void FXTable::setColumnRenumbering(FXbool flag){
  FXuint opts=flag?(options|TABLE_COL_RENUMBER):(options&~TABLE_COL_RENUMBER);
  if(options!=opts){
    options=opts;
    if(flag) updateColumnNumbers(0,ncols);
    }
  }


// Get column renumbering
FXbool FXTable::getColumnRenumbering() const {
  return (options&TABLE_COL_RENUMBER)!=0;
  }


// Set row renumbering
void FXTable::setRowRenumbering(FXbool flag){
  FXuint opts=flag?(options|TABLE_ROW_RENUMBER):(options&~TABLE_ROW_RENUMBER);
  if(options!=opts){
    options=opts;
    if(flag) updateRowNumbers(0,nrows);
    }
  }


// Get row renumbering
FXbool FXTable::getRowRenumbering() const {
  return (options&TABLE_ROW_RENUMBER)!=0;
  }


// Change cell border width
void FXTable::setCellBorderWidth(FXint borderwidth){
  if(borderwidth!=cellBorderWidth){
    cellBorderWidth=borderwidth;
    update();
    }
  }


// Change column header
void FXTable::setColumnText(FXint index,const FXString& text){
  colHeader->setItemText(index,text);
  }


// Return text of column header at index
FXString FXTable::getColumnText(FXint index) const {
  return colHeader->getItemText(index);
  }


// Change row header
void FXTable::setRowText(FXint index,const FXString& text){
  rowHeader->setItemText(index,text);
  }


// Return text of row header at index
FXString FXTable::getRowText(FXint index) const{
  return rowHeader->getItemText(index);
  }


// Change column header icon
void FXTable::setColumnIcon(FXint index,FXIcon* icon){
  colHeader->setItemIcon(index,icon);
  }


// Return icon of column header at index
FXIcon* FXTable::getColumnIcon(FXint index) const {
  return colHeader->getItemIcon(index);
  }

// Change row header icon
void FXTable::setRowIcon(FXint index,FXIcon* icon){
  rowHeader->setItemIcon(index,icon);
  }


// Return icon of row header at index
FXIcon* FXTable::getRowIcon(FXint index) const {
  return rowHeader->getItemIcon(index);
  }


// Change column header icon position
void FXTable::setColumnIconPosition(FXint index,FXuint mode){
  colHeader->setItemIconPosition(index,mode);
  }

// Return icon position of column header at index
FXuint FXTable::getColumnIconPosition(FXint index) const {
  return colHeader->getItemIconPosition(index);
  }

// Change row header icon position
void FXTable::setRowIconPosition(FXint index,FXuint mode){
  rowHeader->setItemIconPosition(index,mode);
  }

// Return icon position of row header at index
FXuint FXTable::getRowIconPosition(FXint index) const {
  return rowHeader->getItemIconPosition(index);
  }


// Change column header icon position
void FXTable::setColumnJustify(FXint index,FXuint justify){
  colHeader->setItemJustify(index,justify);
  }

// Return icon position of column header at index
FXuint FXTable::getColumnJustify(FXint index) const {
  return colHeader->getItemJustify(index);
  }

// Change row header icon position
void FXTable::setRowJustify(FXint index,FXuint justify){
  rowHeader->setItemJustify(index,justify);
  }


// Return icon position of row header at index
FXuint FXTable::getRowJustify(FXint index) const {
  return rowHeader->getItemJustify(index);
  }


// Set column header font
void FXTable::setColumnHeaderFont(FXFont* fnt){
  colHeader->setFont(fnt);
  }

// Return column header font
FXFont* FXTable::getColumnHeaderFont() const{
  return colHeader->getFont();
  }

// Set row header font
void FXTable::setRowHeaderFont(FXFont* fnt){
  rowHeader->setFont(fnt);
  }

// Return row header font
FXFont* FXTable::getRowHeaderFont() const {
  return rowHeader->getFont();
  }


// Change visible rows
void FXTable::setVisibleRows(FXint nvrows){
  if(nvrows<0) nvrows=0;
  if(visiblerows!=nvrows){
    visiblerows=nvrows;
    recalc();
    }
  }


// Change visible columns
void FXTable::setVisibleColumns(FXint nvcols){
  if(nvcols<0) nvcols=0;
  if(visiblecols!=nvcols){
    visiblecols=nvcols;
    recalc();
    }
  }


// Show or hide horizontal grid
void FXTable::showHorzGrid(FXbool on){
  if(hgrid!=on){
    hgrid=on;
    recalc();
    }
  }


// Show or hide vertical grid
void FXTable::showVertGrid(FXbool on){
  if(vgrid!=on){
    vgrid=on;
    recalc();
    }
  }


// Return true if editable
FXbool FXTable::isEditable() const {
  return (options&TABLE_READONLY)==0;
  }


// Set widget is editable or not
void FXTable::setEditable(FXbool edit){
  if(edit) options&=~TABLE_READONLY; else options|=TABLE_READONLY;
  }


// Save data
void FXTable::save(FXStream& store) const {
  register FXint i;
  FXScrollArea::save(store);
  store << nrows;
  store << ncols;
  for(i=0; i<nrows*ncols; i++) store << cells[i];
  store << visiblerows;
  store << visiblecols;
  store << margintop;
  store << marginbottom;
  store << marginleft;
  store << marginright;
  store << textColor;
  store << baseColor;
  store << hiliteColor;
  store << shadowColor;
  store << borderColor;
  store << selbackColor;
  store << seltextColor;
  store << gridColor;
  store << cellBackColor[0][0];
  store << cellBackColor[0][1];
  store << cellBackColor[1][0];
  store << cellBackColor[1][1];
  store << font;
  store << help;
  }


// Load data
void FXTable::load(FXStream& store){
  register FXint i;
  FXScrollArea::load(store);
  store >> nrows;
  store >> ncols;
  FXMALLOC(&cells,FXTableItem*,nrows*ncols+1);
  for(i=0; i<nrows*ncols; i++) store >> cells[i];
  store >> visiblerows;
  store >> visiblecols;
  store >> margintop;
  store >> marginbottom;
  store >> marginleft;
  store >> marginright;
  store >> textColor;
  store >> baseColor;
  store >> hiliteColor;
  store >> shadowColor;
  store >> borderColor;
  store >> selbackColor;
  store >> seltextColor;
  store >> gridColor;
  store >> cellBackColor[0][0];
  store >> cellBackColor[0][1];
  store >> cellBackColor[1][0];
  store >> cellBackColor[1][1];
  store >> font;
  store >> help;
  }


// Clean up
FXTable::~FXTable(){
  for(FXint r=0; r<nrows; r++){
    for(FXint c=0; c<ncols; c++){
      FXTableItem* item=cells[r*ncols+c];
      if(item && (r==0 || cells[(r-1)*ncols+c]!=item) && (c==0 || cells[r*ncols+c-1]!=item)){
        delete item;
        }
      }
    }
  FXFREE(&cells);
  font=(FXFont*)-1L;
  editor=(FXWindow*)-1L;
  cells=(FXTableItem**)-1L;
  colHeader=(FXHeader*)-1L;
  rowHeader=(FXHeader*)-1L;
  cornerButton=(FXButton*)-1L;
  }

}

