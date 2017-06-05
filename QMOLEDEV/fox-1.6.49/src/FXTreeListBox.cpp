/********************************************************************************
*                                                                               *
*                       T r e e  L i s t  B o x  O b j e c t                    *
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
* $Id: FXTreeListBox.cpp,v 1.60.2.2 2007/06/07 20:17:57 fox Exp $                   *
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
#include "FXFont.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXTextField.h"
#include "FXButton.h"
#include "FXMenuButton.h"
#include "FXPopup.h"
#include "FXScrollBar.h"
#include "FXTreeList.h"
#include "FXTreeListBox.h"


/*
  Notes:
  - Handling typed text:
    a) Pass string to target only.
    b) Pass string to target & add to list [begin, after/before current, or end].
    c) Pass string to target & replace current item's label.
  - In most other respects, it behaves like a FXTextField.
  - Need to catch up/down arrow keys.
  - Need to have mode to pass item* instead of char*.
  - TreeListBox turns OFF GUI Updating while being manipulated.
  - Fix this one [and FXComboBox also] the height is the height as determined
    by the TreeList's item height...
  - Perhaps may add some access API's to FXTreeItem?
  - The default height of the treelist box is not good yet.
  - Perhaps use TWO tree lists, one in the pane, and one in the box;
    we can then rest assured that the metrics are always computed properly.
  - Still need some code to make sure always one item shows in box.
  - No reaction to up and down arrow while disabled.
*/

#define TREELISTBOX_MASK       (0)

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXTreeListBox) FXTreeListBoxMap[]={
  FXMAPFUNC(SEL_FOCUS_UP,0,FXTreeListBox::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXTreeListBox::onFocusDown),
  FXMAPFUNC(SEL_FOCUS_SELF,0,FXTreeListBox::onFocusSelf),
  FXMAPFUNC(SEL_UPDATE,FXTreeListBox::ID_TREE,FXTreeListBox::onTreeUpdate),
  FXMAPFUNC(SEL_CHANGED,FXTreeListBox::ID_TREE,FXTreeListBox::onTreeChanged),
  FXMAPFUNC(SEL_CLICKED,FXTreeListBox::ID_TREE,FXTreeListBox::onTreeClicked),
  FXMAPFUNC(SEL_COMMAND,FXTreeListBox::ID_TREE,FXTreeListBox::onTreeClicked),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,FXTreeListBox::ID_FIELD,FXTreeListBox::onFieldButton),
  FXMAPFUNC(SEL_MOUSEWHEEL,FXTreeListBox::ID_FIELD,FXTreeListBox::onMouseWheel),
  };


// Object implementation
FXIMPLEMENT(FXTreeListBox,FXPacker,FXTreeListBoxMap,ARRAYNUMBER(FXTreeListBoxMap))


// List box
FXTreeListBox::FXTreeListBox(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXPacker(p,opts,x,y,w,h, 0,0,0,0, 0,0){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  field=new FXButton(this," ",NULL,this,FXTreeListBox::ID_FIELD,ICON_BEFORE_TEXT|JUSTIFY_LEFT, 0,0,0,0, pl,pr,pt,pb);
  field->setBackColor(getApp()->getBackColor());
  pane=new FXPopup(this,FRAME_LINE);
  tree=new FXTreeList(pane,this,FXTreeListBox::ID_TREE,TREELIST_BROWSESELECT|TREELIST_AUTOSELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y|SCROLLERS_TRACK|HSCROLLING_OFF);
  tree->setIndent(0);
  button=new FXMenuButton(this,FXString::null,NULL,pane,FRAME_RAISED|FRAME_THICK|MENUBUTTON_DOWN|MENUBUTTON_ATTACH_RIGHT, 0,0,0,0, 0,0,0,0);
  button->setXOffset(border);
  button->setYOffset(border);
  flags&=~FLAG_UPDATE;  // Never GUI update
  }


// Create window
void FXTreeListBox::create(){
  FXPacker::create();
  pane->create();
  }

// Detach window
void FXTreeListBox::detach(){
  pane->detach();
  FXPacker::detach();
  }


// Destroy window
void FXTreeListBox::destroy(){
  pane->destroy();
  FXPacker::destroy();
  }


// Enable the window
void FXTreeListBox::enable(){
  if(!isEnabled()){
    FXPacker::enable();
    field->setBackColor(getApp()->getBackColor());
    field->enable();
    button->enable();
    }
  }


// Disable the window
void FXTreeListBox::disable(){
  if(isEnabled()){
    FXPacker::disable();
    field->setBackColor(getApp()->getBaseColor());
    field->disable();
    button->disable();
    }
  }


// Get default width
FXint FXTreeListBox::getDefaultWidth(){
  FXint ww,pw;
  ww=field->getDefaultWidth()+button->getDefaultWidth()+(border<<1);
  pw=pane->getDefaultWidth();
  return FXMAX(ww,pw);
  }


// Get default height
FXint FXTreeListBox::getDefaultHeight(){
  FXint th,bh;
  th=field->getDefaultHeight();
  bh=button->getDefaultHeight();
  return FXMAX(th,bh)+(border<<1);
  }


// Recalculate layout
void FXTreeListBox::layout(){
  FXint buttonWidth,fieldWidth,itemHeight;
  itemHeight=height-(border<<1);
  buttonWidth=button->getDefaultWidth();
  fieldWidth=width-buttonWidth-(border<<1);
  field->position(border,border,fieldWidth,itemHeight);
  button->position(border+fieldWidth,border,buttonWidth,itemHeight);
  pane->resize(width,pane->getDefaultHeight());
  flags&=~FLAG_DIRTY;
  }


// Forward clicked message from list to target
long FXTreeListBox::onTreeClicked(FXObject*,FXSelector sel,void* ptr){
  button->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  if(FXSELTYPE(sel)==SEL_COMMAND){
    field->setText(tree->getItemText((FXTreeItem*)ptr));
    field->setIcon(tree->getItemClosedIcon((FXTreeItem*)ptr));
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
    }
  return 1;
  }


// Forward changed message from list to target
long FXTreeListBox::onTreeChanged(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CHANGED,message),ptr);
  }


// Forward GUI update of tree to target; but only if pane is not popped
long FXTreeListBox::onTreeUpdate(FXObject*,FXSelector,void*){
  return target && !isPaneShown() && target->tryHandle(this,FXSEL(SEL_UPDATE,message),NULL);
  }


// Pressed left button in text field
long FXTreeListBox::onFieldButton(FXObject*,FXSelector,void*){
  button->handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);      // Post the list
  return 1;
  }


// Bounce focus to the field
long FXTreeListBox::onFocusSelf(FXObject* sender,FXSelector,void* ptr){
  return field->handle(sender,FXSEL(SEL_FOCUS_SELF,0),ptr);
  }


// Select upper item
long FXTreeListBox::onFocusUp(FXObject*,FXSelector,void*){
  if(isEnabled()){
    FXTreeItem *item=getCurrentItem();
    if(!item){ for(item=getLastItem(); item->getLast(); item=item->getLast()); }
    else if(item->getAbove()){ item=item->getAbove(); }
    if(item){
      setCurrentItem(item,TRUE);
      }
    return 1;
    }
  return 0;
  }


// Select lower item
long FXTreeListBox::onFocusDown(FXObject*,FXSelector,void*){
  if(isEnabled()){
    FXTreeItem *item=getCurrentItem();
    if(!item){ item=getFirstItem(); }
    else if(item->getBelow()){ item=item->getBelow(); }
    if(item){
      setCurrentItem(item,TRUE);
      }
    return 1;
    }
  return 0;
  }



// Mouse wheel
long FXTreeListBox::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    FXTreeItem *item=getCurrentItem();
    if(event->code<0){
      if(!item){ item=getFirstItem(); }
      else if(item->getBelow()){ item=item->getBelow(); }
      }
    else if(event->code>0){
      if(!item){ for(item=getLastItem(); item->getLast(); item=item->getLast()); }
      else if(item->getAbove()){ item=item->getAbove(); }
      }
    if(item){
      setCurrentItem(item,TRUE);
      }
    return 1;
    }
  return 0;
  }


// Is the pane shown
FXbool FXTreeListBox::isPaneShown() const {
  return pane->shown();
  }


// Set font
void FXTreeListBox::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  field->setFont(fnt);
  tree->setFont(fnt);
  recalc();
  }


// Obtain font
FXFont* FXTreeListBox::getFont() const {
  return field->getFont();
  }


// Get number of items
FXint FXTreeListBox::getNumItems() const {
  return tree->getNumItems();
  }


// Get number of visible items
FXint FXTreeListBox::getNumVisible() const {
  return tree->getNumVisible();
  }


// Set number of visible items
void FXTreeListBox::setNumVisible(FXint nvis){
  tree->setNumVisible(nvis);
  }


// Get first item
FXTreeItem* FXTreeListBox::getFirstItem() const {
  return tree->getFirstItem();
  }


// Get last item
FXTreeItem* FXTreeListBox::getLastItem() const {
  return tree->getLastItem();
  }


// Fill tree list box by appending items from array of strings
FXint FXTreeListBox::fillItems(FXTreeItem* father,const FXchar** strings,FXIcon* oi,FXIcon* ci,void* ptr){
  register FXTreeItem* currentitem=tree->getCurrentItem();
  register FXint n=tree->fillItems(father,strings,oi,ci,ptr);
  if(currentitem!=tree->getCurrentItem()){
    field->setIcon(tree->getItemClosedIcon(tree->getCurrentItem()));
    field->setText(tree->getItemText(tree->getCurrentItem()));
    }
  recalc();
  return n;
  }


// Fill tree list box by appending items from newline separated strings
FXint FXTreeListBox::fillItems(FXTreeItem* father,const FXString& strings,FXIcon* oi,FXIcon* ci,void* ptr){
  register FXTreeItem* currentitem=tree->getCurrentItem();
  register FXint n=tree->fillItems(father,strings,oi,ci,ptr);
  if(currentitem!=tree->getCurrentItem()){
    field->setIcon(tree->getItemClosedIcon(tree->getCurrentItem()));
    field->setText(tree->getItemText(tree->getCurrentItem()));
    }
  recalc();
  return n;
  }


// Insert item under father before other item
FXTreeItem* FXTreeListBox::insertItem(FXTreeItem* other,FXTreeItem* father,FXTreeItem* item){
  register FXTreeItem *newitem=tree->insertItem(other,father,item);
  if(tree->getCurrentItem()==newitem){
    field->setIcon(tree->getItemClosedIcon(newitem));
    field->setText(tree->getItemText(newitem));
    }
  recalc();
  return newitem;
  }


// Insert item with given text and optional icons, and user-data pointer under father before other item
FXTreeItem* FXTreeListBox::insertItem(FXTreeItem* other,FXTreeItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr){
  register FXTreeItem *newitem=tree->insertItem(other,father,text,oi,ci,ptr);
  if(tree->getCurrentItem()==newitem){
    field->setIcon(tree->getItemClosedIcon(newitem));
    field->setText(tree->getItemText(newitem));
    }
  recalc();
  return newitem;
  }


// Append item as last child of father
FXTreeItem* FXTreeListBox::appendItem(FXTreeItem* father,FXTreeItem* item){
  register FXTreeItem *newitem=tree->appendItem(father,item);
  if(tree->getCurrentItem()==newitem){
    field->setIcon(tree->getItemClosedIcon(newitem));
    field->setText(tree->getItemText(newitem));
    }
  recalc();
  return newitem;
  }


// Append item with given text and optional icons, and user-data pointer as last child of father
FXTreeItem* FXTreeListBox::appendItem(FXTreeItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr){
  register FXTreeItem *newitem=tree->appendItem(father,text,oi,ci,ptr);
  if(tree->getCurrentItem()==newitem){
    field->setIcon(tree->getItemClosedIcon(newitem));
    field->setText(tree->getItemText(newitem));
    }
  recalc();
  return newitem;
  }


// Prepend item as first child of father
FXTreeItem* FXTreeListBox::prependItem(FXTreeItem* father,FXTreeItem* item){
  register FXTreeItem *newitem=tree->prependItem(father,item);
  if(tree->getCurrentItem()==newitem){
    field->setIcon(tree->getItemClosedIcon(newitem));
    field->setText(tree->getItemText(newitem));
    }
  recalc();
  return newitem;
  }


// Prepend item with given text and optional icons, and user-data pointer as first child of father
FXTreeItem* FXTreeListBox::prependItem(FXTreeItem* father,const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr){
  register FXTreeItem *newitem=tree->prependItem(father,text,oi,ci,ptr);
  if(tree->getCurrentItem()==newitem){
    field->setIcon(tree->getItemClosedIcon(newitem));
    field->setText(tree->getItemText(newitem));
    }
  recalc();
  return newitem;
  }


// Move item under father before other item
FXTreeItem *FXTreeListBox::moveItem(FXTreeItem* other,FXTreeItem* father,FXTreeItem* item){
  register FXTreeItem *newitem=tree->moveItem(other,father,item);
  recalc();
  return newitem;
  }


// Extract item
FXTreeItem* FXTreeListBox::extractItem(FXTreeItem* item){
  register FXTreeItem *currentitem=tree->getCurrentItem();
  register FXTreeItem *result=tree->extractItem(item);
  if(item==currentitem){
    currentitem=tree->getCurrentItem();
    if(currentitem){
      field->setIcon(tree->getItemClosedIcon(currentitem));
      field->setText(tree->getItemText(currentitem));
      }
    else{
      field->setIcon(NULL);
      field->setText(" ");
      }
    }
  recalc();
  return result;
  }


// Remove given item
void FXTreeListBox::removeItem(FXTreeItem* item){
  register FXTreeItem *currentitem=tree->getCurrentItem();
  tree->removeItem(item);
  if(item==currentitem){
    currentitem=tree->getCurrentItem();
    if(currentitem){
      field->setIcon(tree->getItemClosedIcon(currentitem));
      field->setText(tree->getItemText(currentitem));
      }
    else{
      field->setIcon(NULL);
      field->setText(" ");
      }
    }
  recalc();
  }


// Remove sequence of items
void FXTreeListBox::removeItems(FXTreeItem* fm,FXTreeItem* to){
  tree->removeItems(fm,to);
  recalc();
  }


// Remove all items
void FXTreeListBox::clearItems(){
  tree->clearItems();
  recalc();
  }


// Get item by name
FXTreeItem* FXTreeListBox::findItem(const FXString& text,FXTreeItem* start,FXuint flgs) const {
  return tree->findItem(text,start,flgs);
  }


// Get item by data
FXTreeItem* FXTreeListBox::findItemByData(const void *ptr,FXTreeItem* start,FXuint flgs) const {
  return tree->findItemByData(ptr,start,flgs);
  }


// Is item current
FXbool FXTreeListBox::isItemCurrent(const FXTreeItem* item) const {
  return tree->isItemCurrent(item);
  }


// Is item a leaf
FXbool FXTreeListBox::isItemLeaf(const FXTreeItem* item) const {
  return tree->isItemLeaf(item);
  }


// Sort all items recursively
void FXTreeListBox::sortItems(){
  tree->sortItems();
  }


// Sort item child list
void FXTreeListBox::sortChildItems(FXTreeItem* item){
  tree->sortChildItems(item);
  }


// Sort item list
void FXTreeListBox::sortRootItems(){
  tree->sortRootItems();
  }


// Change current item
void FXTreeListBox::setCurrentItem(FXTreeItem* item,FXbool notify){
  FXTreeItem* current=tree->getCurrentItem();
  if(current!=item){
    tree->setCurrentItem(item);
    tree->makeItemVisible(item);
    if(item){
      field->setIcon(tree->getItemClosedIcon(item));
      field->setText(tree->getItemText(item));
      }
    else{
      field->setIcon(NULL);
      field->setText(FXString::null);
      }
    if(notify && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)item);
    }
  }


// Get current item
FXTreeItem* FXTreeListBox::getCurrentItem() const {
  return tree->getCurrentItem();
  }


// Set item text
void FXTreeListBox::setItemText(FXTreeItem* item,const FXString& text){
  if(item==NULL){ fxerror("%s::setItemText: item is NULL\n",getClassName()); }
  if(isItemCurrent(item)) field->setText(text);
  tree->setItemText(item,text);
  recalc();
  }


// Get item text
FXString FXTreeListBox::getItemText(const FXTreeItem* item) const {
  if(item==NULL){ fxerror("%s::getItemText: item is NULL\n",getClassName()); }
  return tree->getItemText(item);
  }


// Change open icon
void FXTreeListBox::setItemOpenIcon(FXTreeItem* item,FXIcon* icon,FXbool owned){
  tree->setItemOpenIcon(item,icon,owned);
  }


// Get open icon
FXIcon* FXTreeListBox::getItemOpenIcon(const FXTreeItem* item) const {
  return tree->getItemOpenIcon(item);
  }


// Set closed icon
void FXTreeListBox::setItemClosedIcon(FXTreeItem* item,FXIcon* icon,FXbool owned){
  tree->setItemClosedIcon(item,icon,owned);
  }


// Get closed icon
FXIcon* FXTreeListBox::getItemClosedIcon(const FXTreeItem* item) const{
  return tree->getItemClosedIcon(item);
  }


// Set item data
void FXTreeListBox::setItemData(FXTreeItem* item,void* ptr) const {
  if(item==NULL){ fxerror("%s::setItemData: item is NULL\n",getClassName()); }
  tree->setItemData(item,ptr);
  }


// Get item data
void* FXTreeListBox::getItemData(const FXTreeItem* item) const {
  if(item==NULL){ fxerror("%s::getItemData: item is NULL\n",getClassName()); }
  return tree->getItemData(item);
  }


// Get sort function
FXTreeListSortFunc FXTreeListBox::getSortFunc() const {
  return tree->getSortFunc();
  }


// Change sort function
void FXTreeListBox::setSortFunc(FXTreeListSortFunc func){
  tree->setSortFunc(func);
  }


// Change list style
void FXTreeListBox::setListStyle(FXuint mode){
  FXuint opts=(options&~TREELISTBOX_MASK)|(mode&TREELISTBOX_MASK);
  if(opts!=options){
    options=opts;
    recalc();
    }
  }


// Get list style
FXuint FXTreeListBox::getListStyle() const {
  return (options&TREELISTBOX_MASK);
  }


// Set help text
void FXTreeListBox::setHelpText(const FXString& txt){
  field->setHelpText(txt);
  }


// Get help text
const FXString& FXTreeListBox::getHelpText() const {
  return field->getHelpText();
  }


// Set tip text
void FXTreeListBox::setTipText(const FXString& txt){
  field->setTipText(txt);
  }


// Get tip text
const FXString& FXTreeListBox::getTipText() const {
  return field->getTipText();
  }


// Save object to stream
void FXTreeListBox::save(FXStream& store) const {
  FXPacker::save(store);
  store << field;
  store << button;
  store << tree;
  store << pane;
  }


// Load object from stream
void FXTreeListBox::load(FXStream& store){
  FXPacker::load(store);
  store >> field;
  store >> button;
  store >> tree;
  store >> pane;
  }


// Delete it
FXTreeListBox::~FXTreeListBox(){
  delete pane;
  pane=(FXPopup*)-1L;
  field=(FXButton*)-1L;
  button=(FXMenuButton*)-1L;
  tree=(FXTreeList*)-1L;
  }

}
