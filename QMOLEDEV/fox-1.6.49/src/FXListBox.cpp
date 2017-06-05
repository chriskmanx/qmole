/********************************************************************************
*                                                                               *
*                        L i s t   B o x   O b j e c t                          *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXListBox.cpp,v 1.63.2.2 2007/06/07 20:17:57 fox Exp $                       *
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
#include "FXApp.h"
#include "FXFont.h"
#include "FXDC.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXTextField.h"
#include "FXButton.h"
#include "FXMenuButton.h"
#include "FXComposite.h"
#include "FXPacker.h"
#include "FXShell.h"
#include "FXPopup.h"
#include "FXScrollBar.h"
#include "FXScrollArea.h"
#include "FXList.h"
#include "FXListBox.h"


/*
  Notes:
  - Need to catch up/down arrow keys.
  - Listbox turns OFF GUI Updating while being manipulated.
  - No reaction to up and down arrow while disabled.
*/

#define LISTBOX_MASK        (0)

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXListBox) FXListBoxMap[]={
  FXMAPFUNC(SEL_FOCUS_SELF,0,FXListBox::onFocusSelf),
  FXMAPFUNC(SEL_FOCUS_UP,0,FXListBox::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXListBox::onFocusDown),
  FXMAPFUNC(SEL_UPDATE,FXListBox::ID_LIST,FXListBox::onListUpdate),
  FXMAPFUNC(SEL_CLICKED,FXListBox::ID_LIST,FXListBox::onListClicked),
  FXMAPFUNC(SEL_COMMAND,FXListBox::ID_LIST,FXListBox::onListClicked),
  FXMAPFUNC(SEL_CHANGED,FXListBox::ID_LIST,FXListBox::onListChanged),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,FXListBox::ID_FIELD,FXListBox::onFieldButton),
  FXMAPFUNC(SEL_MOUSEWHEEL,FXListBox::ID_FIELD,FXListBox::onMouseWheel),
  FXMAPFUNC(SEL_COMMAND,FXListBox::ID_SETVALUE,FXListBox::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXListBox::ID_SETINTVALUE,FXListBox::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXListBox::ID_GETINTVALUE,FXListBox::onCmdGetIntValue),
  };


// Object implementation
FXIMPLEMENT(FXListBox,FXPacker,FXListBoxMap,ARRAYNUMBER(FXListBoxMap))


// List box
FXListBox::FXListBox(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXPacker(p,opts,x,y,w,h, 0,0,0,0, 0,0){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  field=new FXButton(this," ",NULL,this,FXListBox::ID_FIELD,ICON_BEFORE_TEXT|JUSTIFY_LEFT, 0,0,0,0, pl,pr,pt,pb);
  field->setBackColor(getApp()->getBackColor());
  pane=new FXPopup(this,FRAME_LINE);
  list=new FXList(pane,this,FXListBox::ID_LIST,LIST_BROWSESELECT|LIST_AUTOSELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y|SCROLLERS_TRACK|HSCROLLING_OFF);
  button=new FXMenuButton(this,FXString::null,NULL,pane,FRAME_RAISED|FRAME_THICK|MENUBUTTON_DOWN|MENUBUTTON_ATTACH_RIGHT, 0,0,0,0, 0,0,0,0);
  button->setXOffset(border);
  button->setYOffset(border);
  flags&=~FLAG_UPDATE;  // Never GUI update
  }


// Create window
void FXListBox::create(){
  FXPacker::create();
  pane->create();
  }


// Detach window
void FXListBox::detach(){
  FXPacker::detach();
  pane->detach();
  }


// Destroy window
void FXListBox::destroy(){
  pane->destroy();
  FXPacker::destroy();
  }


// Enable the window
void FXListBox::enable(){
  if(!isEnabled()){
    FXPacker::enable();
    field->setBackColor(getApp()->getBackColor());
    field->enable();
    button->enable();
    }
  }


// Disable the window
void FXListBox::disable(){
  if(isEnabled()){
    FXPacker::disable();
    field->setBackColor(getApp()->getBaseColor());
    field->disable();
    button->disable();
    }
  }


// Get default width
FXint FXListBox::getDefaultWidth(){
  FXint ww,pw;
  ww=field->getDefaultWidth()+button->getDefaultWidth()+(border<<1);
  pw=pane->getDefaultWidth();
  return FXMAX(ww,pw);
  }


// Get default height
FXint FXListBox::getDefaultHeight(){
  FXint th,bh;
  th=field->getDefaultHeight();
  bh=button->getDefaultHeight();
  return FXMAX(th,bh)+(border<<1);
  }


// Recalculate layout
void FXListBox::layout(){
  FXint buttonWidth,fieldWidth,itemHeight;
  itemHeight=height-(border<<1);
  buttonWidth=button->getDefaultWidth();
  fieldWidth=width-buttonWidth-(border<<1);
  field->position(border,border,fieldWidth,itemHeight);
  button->position(border+fieldWidth,border,buttonWidth,itemHeight);
  pane->resize(width,pane->getDefaultHeight());
  flags&=~FLAG_DIRTY;
  }


// Update value from a message
long FXListBox::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrentItem((FXint)(FXival)ptr);
  return 1;
  }


// Obtain value from list
long FXListBox::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCurrentItem();
  return 1;
  }


// Update value from a message
long FXListBox::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCurrentItem(*((FXint*)ptr));
  return 1;
  }


// Forward GUI update of list to target; but only if pane is not popped
long FXListBox::onListUpdate(FXObject*,FXSelector,void*){
  return target && !isPaneShown() && target->tryHandle(this,FXSEL(SEL_UPDATE,message),NULL);
  }


// Item in list widget changed
long FXListBox::onListChanged(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CHANGED,message),ptr);
  }


// Forward clicked message from list to target
long FXListBox::onListClicked(FXObject*,FXSelector sel,void* ptr){
  button->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);
  if(FXSELTYPE(sel)==SEL_COMMAND){
    field->setText(getItemText((FXint)(FXival)ptr));
    field->setIcon(getItemIcon((FXint)(FXival)ptr));
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
    }
  return 1;
  }



// Pressed left button in text field
long FXListBox::onFieldButton(FXObject*,FXSelector,void*){
  button->handle(this,FXSEL(SEL_COMMAND,ID_POST),NULL);    // Post the list
  return 1;
  }


// Bounce focus to the field
long FXListBox::onFocusSelf(FXObject* sender,FXSelector,void* ptr){
  return field->handle(sender,FXSEL(SEL_FOCUS_SELF,0),ptr);
  }


// Select upper item
long FXListBox::onFocusUp(FXObject*,FXSelector,void*){
  if(isEnabled()){
    FXint index=getCurrentItem();
    if(index<0) index=getNumItems()-1;
    else if(0<index) index--;
    if(0<=index && index<getNumItems()){
      setCurrentItem(index,TRUE);
      }
    return 1;
    }
  return 0;
  }


// Select lower item
long FXListBox::onFocusDown(FXObject*,FXSelector,void*){
  if(isEnabled()){
    FXint index=getCurrentItem();
    if(index<0) index=0;
    else if(index<getNumItems()-1) index++;
    if(0<=index && index<getNumItems()){
      setCurrentItem(index,TRUE);
      }
    return 1;
    }
  return 0;
  }


// Mouse wheel
long FXListBox::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    FXint index=getCurrentItem();
    if(event->code<0){
      if(index<0) index=0;
      else if(index<getNumItems()-1) index++;
      }
    else if(event->code>0){
      if(index<0) index=getNumItems()-1;
      else if(0<index) index--;
      }
    if(0<=index && index<getNumItems()){
      setCurrentItem(index,TRUE);
      }
    return 1;
    }
  return 0;
  }


// Get number of items
FXint FXListBox::getNumItems() const {
  return list->getNumItems();
  }


// Get number of visible items
FXint FXListBox::getNumVisible() const {
  return list->getNumVisible();
  }


// Set number of visible items
void FXListBox::setNumVisible(FXint nvis){
  list->setNumVisible(nvis);
  }


// Is item current
FXbool FXListBox::isItemCurrent(FXint index) const {
  return list->isItemCurrent(index);
  }


// Change current item
void FXListBox::setCurrentItem(FXint index,FXbool notify){
  FXint current=list->getCurrentItem();
  if(current!=index){
    list->setCurrentItem(index);
    list->makeItemVisible(index);
    if(0<=index){
      field->setIcon(list->getItemIcon(index));
      field->setText(list->getItemText(index));
      }
    else{
      field->setIcon(NULL);
      field->setText(" ");
      }
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)index);}
    }
  }


// Get current item
FXint FXListBox::getCurrentItem() const {
  return list->getCurrentItem();
  }


// Retrieve item
FXString FXListBox::getItem(FXint index) const {
  return list->getItem(index)->getText();
  }


// Replace text of item at index
FXint FXListBox::setItem(FXint index,const FXString& text,FXIcon* icon,void* ptr){
  if(index<0 || list->getNumItems()<=index){ fxerror("%s::setItem: index out of range.\n",getClassName()); }
  list->setItem(index,text,icon,ptr);
  if(isItemCurrent(index)){
    field->setIcon(icon);
    field->setText(text);
    }
  recalc();
  return index;
  }


// Fill list by appending items from array of strings
FXint FXListBox::fillItems(const FXchar** strings,FXIcon* icon,void* ptr){
  register FXint numberofitems=list->getNumItems();
  register FXint n=list->fillItems(strings,icon,ptr);
  if(numberofitems<=list->getCurrentItem()){
    field->setIcon(list->getItemIcon(list->getCurrentItem()));
    field->setText(list->getItemText(list->getCurrentItem()));
    }
  recalc();
  return n;
  }


// Fill list by appending items from newline separated strings
FXint FXListBox::fillItems(const FXString& strings,FXIcon* icon,void* ptr){
  register FXint numberofitems=list->getNumItems();
  register FXint n=list->fillItems(strings,icon,ptr);
  if(numberofitems<=list->getCurrentItem()){
    field->setIcon(list->getItemIcon(list->getCurrentItem()));
    field->setText(list->getItemText(list->getCurrentItem()));
    }
  recalc();
  return n;
  }


// Insert item at index
FXint FXListBox::insertItem(FXint index,const FXString& text,FXIcon* icon,void* ptr){
  if(index<0 || list->getNumItems()<index){ fxerror("%s::insertItem: index out of range.\n",getClassName()); }
  list->insertItem(index,text,icon,ptr);
  if(isItemCurrent(index)){
    field->setIcon(icon);
    field->setText(text);
    }
  recalc();
  return index;
  }


// Append item
FXint FXListBox::appendItem(const FXString& text,FXIcon* icon,void* ptr){
  list->appendItem(text,icon,ptr);
  if(isItemCurrent(getNumItems()-1)){
    field->setIcon(icon);
    field->setText(text);
    }
  recalc();
  return getNumItems()-1;
  }


// Prepend item
FXint FXListBox::prependItem(const FXString& text,FXIcon* icon,void* ptr){
  list->prependItem(text,icon,ptr);
  if(isItemCurrent(0)){
    field->setIcon(icon);
    field->setText(text);
    }
  recalc();
  return 0;
  }


// Move item from oldindex to newindex
FXint FXListBox::moveItem(FXint newindex,FXint oldindex){
  FXint current=list->getCurrentItem();
  list->moveItem(newindex,oldindex);
  if(current!=list->getCurrentItem()){
    current=list->getCurrentItem();
    if(0<=current){
      field->setIcon(list->getItemIcon(current));
      field->setText(list->getItemText(current));
      }
    else{
      field->setIcon(NULL);
      field->setText(" ");
      }
    }
  recalc();
  return newindex;
  }


// Extract item from list
FXListItem* FXListBox::extractItem(FXint index){
  register FXint current=list->getCurrentItem();
  register FXListItem *result=list->extractItem(index);
  if(index==current){
    current=list->getCurrentItem();
    if(0<=current){
      field->setIcon(list->getItemIcon(current));
      field->setText(list->getItemText(current));
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
void FXListBox::removeItem(FXint index){
  register FXint current=list->getCurrentItem();
  list->removeItem(index);
  if(index==current){
    current=list->getCurrentItem();
    if(0<=current){
      field->setIcon(list->getItemIcon(current));
      field->setText(list->getItemText(current));
      }
    else{
      field->setIcon(NULL);
      field->setText(" ");
      }
    }
  recalc();
  }


// Remove all items
void FXListBox::clearItems(){
  list->clearItems();
  field->setIcon(NULL);
  field->setText(" ");
  recalc();
  }


// Get item by name
FXint FXListBox::findItem(const FXString& text,FXint start,FXuint flgs) const {
  return list->findItem(text,start,flgs);
  }


// Get item by data
FXint FXListBox::findItemByData(const void *ptr,FXint start,FXuint flgs) const {
  return list->findItemByData(ptr,start,flgs);
  }


// Set item text
void FXListBox::setItemText(FXint index,const FXString& txt){
  if(isItemCurrent(index)) field->setText(txt);
  list->setItemText(index,txt);
  recalc();
  }


// Get item text
FXString FXListBox::getItemText(FXint index) const {
  return list->getItemText(index);
  }


// Set item icon
void FXListBox::setItemIcon(FXint index,FXIcon* icon,FXbool owned){
  if(isItemCurrent(index)) field->setIcon(icon);
  list->setItemIcon(index,icon,owned);
  recalc();
  }


// Get item icon
FXIcon* FXListBox::getItemIcon(FXint index) const {
  return list->getItemIcon(index);
  }


// Set item data
void FXListBox::setItemData(FXint index,void* ptr) const {
  list->setItemData(index,ptr);
  }


// Get item data
void* FXListBox::getItemData(FXint index) const {
  return list->getItemData(index);
  }


// Is the pane shown
FXbool FXListBox::isPaneShown() const {
  return pane->shown();
  }


// Set font
void FXListBox::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  field->setFont(fnt);
  list->setFont(fnt);
  recalc();
  }


// Obtain font
FXFont* FXListBox::getFont() const {
  return field->getFont();
  }


// Set window background color
void FXListBox::setBackColor(FXColor clr){
  field->setBackColor(clr);
  list->setBackColor(clr);
  }


// Get background color
FXColor FXListBox::getBackColor() const {
  return field->getBackColor();
  }


// Set text color
void FXListBox::setTextColor(FXColor clr){
  field->setTextColor(clr);
  list->setTextColor(clr);
  }


// Return text color
FXColor FXListBox::getTextColor() const {
  return field->getTextColor();
  }


// Set select background color
void FXListBox::setSelBackColor(FXColor clr){
  list->setSelBackColor(clr);
  }


// Return selected background color
FXColor FXListBox::getSelBackColor() const {
  return list->getSelBackColor();
  }


// Set selected text color
void FXListBox::setSelTextColor(FXColor clr){
  list->setSelTextColor(clr);
  }


// Return selected text color
FXColor FXListBox::getSelTextColor() const {
  return list->getSelTextColor();
  }


// Sort items using current sort function
void FXListBox::sortItems(){
  list->sortItems();
  }


// Return sort function
FXListSortFunc FXListBox::getSortFunc() const {
  return list->getSortFunc();
  }


// Change sort function
void FXListBox::setSortFunc(FXListSortFunc func){
  list->setSortFunc(func);
  }


// Set help text
void FXListBox::setHelpText(const FXString& txt){
  field->setHelpText(txt);
  }


// Get help text
const FXString& FXListBox::getHelpText() const {
  return field->getHelpText();
  }


// Set tip text
void FXListBox::setTipText(const FXString& txt){
  field->setTipText(txt);
  }


// Get tip text
const FXString& FXListBox::getTipText() const {
  return field->getTipText();
  }


// Save object to stream
void FXListBox::save(FXStream& store) const {
  FXPacker::save(store);
  store << field;
  store << button;
  store << list;
  store << pane;
  }


// Load object from stream
void FXListBox::load(FXStream& store){
  FXPacker::load(store);
  store >> field;
  store >> button;
  store >> list;
  store >> pane;
  }


// Delete it
FXListBox::~FXListBox(){
  delete pane;
  pane=(FXPopup*)-1L;
  field=(FXButton*)-1L;
  button=(FXMenuButton*)-1L;
  list=(FXList*)-1L;
  }

}
