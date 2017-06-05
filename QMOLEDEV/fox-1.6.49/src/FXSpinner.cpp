/********************************************************************************
*                                                                               *
*                             S p i n   B u t t o n                             *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Lyle Johnson.   All Rights Reserved.               *
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
* $Id: FXSpinner.cpp,v 1.63 2006/02/07 01:17:26 fox Exp $                       *
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
#include "FXLabel.h"
#include "FXTextField.h"
#include "FXArrowButton.h"
#include "FXSpinner.h"


/*
  To do:
  - Should this also be derived from FXTextField instead?
  - Sends SEL_COMMAND; should it send SEL_CHANGED for each repeat, then SEL_COMMAND
    at end?
  - Should block SEL_UPDATE until after sending SEL_COMMAND.
  - Make the value->text and text->value virtual.
*/

#define BUTTONWIDTH 14

#define INTMAX  2147483647
#define INTMIN  (-INTMAX-1)

#define SPINNER_MASK (SPIN_CYCLIC|SPIN_NOTEXT|SPIN_NOMAX|SPIN_NOMIN)

using namespace FX;

/*******************************************************************************/

namespace FX {


//  Message map
FXDEFMAP(FXSpinner) FXSpinnerMap[]={
  FXMAPFUNC(SEL_KEYPRESS,0,FXSpinner::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXSpinner::onKeyRelease),
  FXMAPFUNC(SEL_FOCUS_SELF,0,FXSpinner::onFocusSelf),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_ENTRY,FXSpinner::onCmdEntry),
  FXMAPFUNC(SEL_CHANGED,FXSpinner::ID_ENTRY,FXSpinner::onChgEntry),
  FXMAPFUNC(SEL_MOUSEWHEEL,FXSpinner::ID_ENTRY,FXSpinner::onWheelEntry),
  FXMAPFUNC(SEL_UPDATE,FXSpinner::ID_INCREMENT,FXSpinner::onUpdIncrement),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_INCREMENT,FXSpinner::onCmdIncrement),
  FXMAPFUNC(SEL_UPDATE,FXSpinner::ID_DECREMENT,FXSpinner::onUpdDecrement),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_DECREMENT,FXSpinner::onCmdDecrement),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_SETVALUE,FXSpinner::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_SETINTVALUE,FXSpinner::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_GETINTVALUE,FXSpinner::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_SETINTRANGE,FXSpinner::onCmdSetIntRange),
  FXMAPFUNC(SEL_COMMAND,FXSpinner::ID_GETINTRANGE,FXSpinner::onCmdGetIntRange),
  };


// Object implementation
FXIMPLEMENT(FXSpinner,FXPacker,FXSpinnerMap,ARRAYNUMBER(FXSpinnerMap))


// Construct spinner out of two buttons and a text field
FXSpinner::FXSpinner(){
  flags|=FLAG_ENABLED;
  textField=(FXTextField*)-1L;
  upButton=(FXArrowButton*)-1L;
  downButton=(FXArrowButton*)-1L;
  range[0]=INTMIN;
  range[1]=INTMAX;
  incr=1;
  pos=0;
  }


// Construct spinner out of two buttons and a text field
FXSpinner::FXSpinner(FXComposite *p,FXint cols,FXObject *tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXPacker(p,opts,x,y,w,h,0,0,0,0,0,0){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  textField=new FXTextField(this,cols,this,ID_ENTRY,TEXTFIELD_INTEGER|JUSTIFY_RIGHT,0,0,0,0,pl,pr,pt,pb);
  upButton=new FXArrowButton(this,this,FXSpinner::ID_INCREMENT,FRAME_RAISED|FRAME_THICK|ARROW_UP|ARROW_REPEAT, 0,0,0,0, 0,0,0,0);
  downButton=new FXArrowButton(this,this,FXSpinner::ID_DECREMENT,FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_REPEAT, 0,0,0,0, 0,0,0,0);
  range[0]=(options&SPIN_NOMIN) ? INTMIN : 0;
  range[1]=(options&SPIN_NOMAX) ? INTMAX : 100;
  textField->setText("0");
  incr=1;
  pos=0;
  }


// Get default width
FXint FXSpinner::getDefaultWidth(){
  FXint tw=0;
  if(!(options&SPIN_NOTEXT)) tw=textField->getDefaultWidth();
  return tw+BUTTONWIDTH+(border<<1);
  }


// Get default height
FXint FXSpinner::getDefaultHeight(){
  return textField->getDefaultHeight()+(border<<1);
  }


// Enable the widget
void FXSpinner::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXPacker::enable();
    textField->enable();
    upButton->enable();
    downButton->enable();
    }
  }


// Disable the widget
void FXSpinner::disable(){
  if(flags&FLAG_ENABLED){
    FXPacker::disable();
    textField->disable();
    upButton->disable();
    downButton->disable();
    }
  }


// Recompute layout
void FXSpinner::layout(){
  FXint buttonWidth,buttonHeight,textWidth,textHeight;

  textHeight=height-2*border;
  buttonHeight=textHeight>>1;

  // Only the buttons:- place buttons to take up the whole space!
  if(options&SPIN_NOTEXT){
    buttonWidth=width-2*border;
    upButton->position(border,border,buttonWidth,buttonHeight);
    downButton->position(border,height-buttonHeight-border,buttonWidth,buttonHeight);
    }

  // Buttons plus the text; buttons are default width, text stretches to fill the rest
  else{
    buttonWidth=BUTTONWIDTH;
    textWidth=width-buttonWidth-2*border;
    textField->position(border,border,textWidth,textHeight);
    upButton->position(border+textWidth,border,buttonWidth,buttonHeight);
    downButton->position(border+textWidth,height-buttonHeight-border,buttonWidth,buttonHeight);
    }
  flags&=~FLAG_DIRTY;
  }


// Respond to increment message
long FXSpinner::onUpdIncrement(FXObject* sender,FXSelector,void*){
  if(isEnabled() && ((options&SPIN_CYCLIC) || (pos<range[1])))
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Respond to increment message
long FXSpinner::onCmdIncrement(FXObject*,FXSelector,void*){
  if(isEnabled() && isEditable()){
    increment(TRUE);
    return 1;
    }
  return 0;
  }


// Disable decrement if at low end already
long FXSpinner::onUpdDecrement(FXObject* sender,FXSelector,void*){
  if(isEnabled() && ((options&SPIN_CYCLIC) || (range[0]<pos)))
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Respond to decrement message
long FXSpinner::onCmdDecrement(FXObject*,FXSelector,void*){
  if(isEnabled() && isEditable()){
    decrement(TRUE);
    return 1;
    }
  return 0;
  }


// Rolling mouse wheel in text field works as if hitting up or down buttons
long FXSpinner::onWheelEntry(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled() && isEditable()){
    if(event->code>0){
      if(event->state&CONTROLMASK) incrementByAmount(incr*10,TRUE);
      else increment(TRUE);
      }
    else{
      if(event->state&CONTROLMASK) decrementByAmount(incr*10,TRUE);
      else decrement(TRUE);
      }
    return 1;
    }
  return 0;
  }


// Text field changed
long FXSpinner::onChgEntry(FXObject*,FXSelector,void*){
  register FXint value=FXIntVal(textField->getText());
  if(value<range[0]) value=range[0];
  if(value>range[1]) value=range[1];
  if(value!=pos){
    pos=value;
    if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)pos);
    }
  return 1;
  }


// Text field command
long FXSpinner::onCmdEntry(FXObject*,FXSelector,void*){
  textField->setText(FXStringVal(pos));       // Put back adjusted value
  if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);
  return 1;
  }


// Keyboard press
long FXSpinner::onKeyPress(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    switch(event->code){
      case KEY_Up:
      case KEY_KP_Up:
        if(isEditable()){
          increment(TRUE);
          }
        else{
          getApp()->beep();
          }
        return 1;
      case KEY_Down:
      case KEY_KP_Down:
        if(isEditable()){
          decrement(TRUE);
          }
        else{
          getApp()->beep();
          }
        return 1;
      default:
        return textField->handle(sender,sel,ptr);
      }
    }
  return 0;
  }


// Keyboard release
long FXSpinner::onKeyRelease(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_Up:
      case KEY_KP_Up:
      case KEY_Down:
      case KEY_KP_Down:
        return 1;
      default:
        return textField->handle(sender,sel,ptr);
      }
    }
  return 0;
  }


// Force focus on the text field
long FXSpinner::onFocusSelf(FXObject* sender,FXSelector,void* ptr){
  return textField->handle(sender,FXSEL(SEL_FOCUS_SELF,0),ptr);
  }


// Update value from a message
long FXSpinner::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXSpinner::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setValue(*((FXint*)ptr));
  return 1;
  }


// Obtain value from spinner
long FXSpinner::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getValue();
  return 1;
  }


// Update range from a message
long FXSpinner::onCmdSetIntRange(FXObject*,FXSelector,void* ptr){
  setRange(((FXint*)ptr)[0],((FXint*)ptr)[1]);
  return 1;
  }


// Get range with a message
long FXSpinner::onCmdGetIntRange(FXObject*,FXSelector,void* ptr){
  getRange(((FXint*)ptr)[0],((FXint*)ptr)[1]);
  return 1;
  }


// Increment spinner
void FXSpinner::increment(FXbool notify){
  incrementByAmount(incr,notify);
  }


// Increment spinner by certain amount
void FXSpinner::incrementByAmount(FXint amount,FXbool notify){
  if(range[0]<range[1]){
    if(options&SPIN_CYCLIC){
      setValue(range[0] + (pos+amount-range[0]) % (range[1]-range[0]+1),notify);
      }
    else{
      setValue(pos+amount,notify);
      }
    }
  }


// Decrement spinner
void FXSpinner::decrement(FXbool notify){
  decrementByAmount(incr,notify);
  }


// Decrement spinner by certain amount
void FXSpinner::decrementByAmount(FXint amount,FXbool notify){
  if(range[0]<range[1]){
    if(options&SPIN_CYCLIC){
      setValue(range[0] + (pos+(range[1]-range[0]+1-amount)-range[0]) % (range[1]-range[0]+1),notify);
      }
    else{
      setValue(pos-amount,notify);
      }
    }
  }


// True if spinner is cyclic
FXbool FXSpinner::isCyclic() const {
  return (options&SPIN_CYCLIC)!=0;
  }


// Set spinner cyclic mode
void FXSpinner::setCyclic(FXbool cyclic){
  if(cyclic) options|=SPIN_CYCLIC; else options&=~SPIN_CYCLIC;
  }


// Set spinner range; this also revalidates the position,
void FXSpinner::setRange(FXint lo,FXint hi,FXbool notify){
  if(lo>hi){ fxerror("%s::setRange: trying to set negative range.\n",getClassName()); }
  if(range[0]!=lo || range[1]!=hi){
    range[0]=lo;
    range[1]=hi;
    setValue(pos,notify);
    }
  }


// Set new value
void FXSpinner::setValue(FXint value,FXbool notify){
  if(value<range[0]) value=range[0];
  if(value>range[1]) value=range[1];
  if(pos!=value){
    textField->setText(FXStringVal(value));
    pos=value;
    if(notify && target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)pos);}
    }
  }


// Change value increment
void FXSpinner::setIncrement(FXint inc){
  if(inc<=0){ fxerror("%s::setIncrement: negative or zero increment specified.\n",getClassName()); }
  incr=inc;
  }


// True if text supposed to be visible
FXbool FXSpinner::isTextVisible() const {
  return textField->shown();
  }


// Change text visibility
void FXSpinner::setTextVisible(FXbool shown){
  FXuint opts=shown?(options&~SPIN_NOTEXT):(options|SPIN_NOTEXT);
  if(options!=opts){
    options=opts;
    recalc();
    }
  }


// Set the font used in the text field|
void FXSpinner::setFont(FXFont *fnt) {
  textField->setFont(fnt);
  }


// Return the font used in the text field
FXFont *FXSpinner::getFont() const {
  return textField->getFont();
  }


// Set help text
void FXSpinner::setHelpText(const FXString&  text){
  textField->setHelpText(text);
  upButton->setHelpText(text);
  downButton->setHelpText(text);
  }


// Get help text
const FXString& FXSpinner::getHelpText() const {
  return textField->getHelpText();
  }


// Set tip text
void FXSpinner::setTipText(const FXString&  text){
  textField->setTipText(text);
  upButton->setTipText(text);
  downButton->setTipText(text);
  }



// Get tip text
const FXString& FXSpinner::getTipText() const {
  return textField->getTipText();
  }


// Change spinner style
void FXSpinner::setSpinnerStyle(FXuint style){
  FXuint opts=(options&~SPINNER_MASK) | (style&SPINNER_MASK);
  if(options!=opts){
    if(opts&SPIN_NOMIN) range[0]=INTMIN;
    if(opts&SPIN_NOMAX) range[1]=INTMAX;
    options=opts;
    recalc();
    }
  }


// Get spinner style
FXuint FXSpinner::getSpinnerStyle() const {
  return (options&SPINNER_MASK);
  }


// Allow editing of the text field
void FXSpinner::setEditable(FXbool edit){
  textField->setEditable(edit);
  }


// Return TRUE if text field is editable
FXbool FXSpinner::isEditable() const {
  return textField->isEditable();
  }

// Change color of the up arrow
void FXSpinner::setUpArrowColor(FXColor clr){
  upButton->setArrowColor(clr);
  }

// Return color of the up arrow
FXColor FXSpinner::getUpArrowColor() const {
  return upButton->getArrowColor();
  }

// Change color of the down arrow
void FXSpinner::setDownArrowColor(FXColor clr){
  downButton->setArrowColor(clr);
  }

// Return color of the the down arrow
FXColor FXSpinner::getDownArrowColor() const {
  return downButton->getArrowColor();
  }

// Change text color
void FXSpinner::setTextColor(FXColor clr){
  textField->setTextColor(clr);
  }

// Return text color
FXColor FXSpinner::getTextColor() const {
  return textField->getTextColor();
  }

// Change selected background color
void FXSpinner::setSelBackColor(FXColor clr){
  textField->setSelBackColor(clr);
  }

// Return selected background color
FXColor FXSpinner::getSelBackColor() const {
  return textField->getSelBackColor();
  }

// Change selected text color
void FXSpinner::setSelTextColor(FXColor clr){
  textField->setSelTextColor(clr);
  }

// Return selected text color
FXColor FXSpinner::getSelTextColor() const {
  return textField->getSelTextColor();
  }

// Changes the cursor color
void FXSpinner::setCursorColor(FXColor clr){
  textField->setCursorColor(clr);
  }

// Return the cursor color
FXColor FXSpinner::getCursorColor() const {
  return textField->getCursorColor();
  }


// Change number of columns
void FXSpinner::setNumColumns(FXint ncols){
  textField->setNumColumns(ncols);
  }


// Return number of columns
FXint FXSpinner::getNumColumns() const {
  return textField->getNumColumns();
  }


// Save object to stream
void FXSpinner::save(FXStream& store) const {
  FXPacker::save(store);
  store << textField;
  store << upButton;
  store << downButton;
  store << range[0] << range[1];
  store << incr;
  store << pos;
  }


// Load object from stream
void FXSpinner::load(FXStream& store){
  FXPacker::load(store);
  store >> textField;
  store >> upButton;
  store >> downButton;
  store >> range[0] >> range[1];
  store >> incr;
  store >> pos;
  }


// Destruct spinner:- trash it!
FXSpinner::~FXSpinner(){
  textField=(FXTextField*)-1L;
  upButton=(FXArrowButton*)-1L;
  downButton=(FXArrowButton*)-1L;
  }

}
