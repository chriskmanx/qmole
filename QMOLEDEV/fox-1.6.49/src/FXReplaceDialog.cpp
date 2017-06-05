/********************************************************************************
*                                                                               *
*                      T e x t   R e p l a c e   D i a l o g                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXReplaceDialog.cpp,v 1.49 2006/03/01 02:13:21 fox Exp $                 *
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
#include "FXImage.h"
#include "FXIcon.h"
#include "FXGIFIcon.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXArrowButton.h"
#include "FXSeparator.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXCheckButton.h"
#include "FXRadioButton.h"
#include "FXPacker.h"
#include "FXScrollBar.h"
#include "FXTextField.h"
#include "FXVerticalFrame.h"
#include "FXHorizontalFrame.h"
#include "FXReplaceDialog.h"



/*
  Notes:

  - Keep history of strings previously searched, and allow up/down arrows
    to "scroll" back to this history.
*/

// Padding for buttons
#define HORZ_PAD      12
#define VERT_PAD      2
#define SEARCH_MASK   (SEARCH_EXACT|SEARCH_IGNORECASE|SEARCH_REGEX)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Search and replace registry group
static const FXchar searchgroup[]="SearchReplace";
static const FXchar skey[20][3]={{'S','A','\0'},{'S','B','\0'},{'S','C','\0'},{'S','D','\0'},{'S','E','\0'},{'S','F','\0'},{'S','G','\0'},{'S','H','\0'},{'S','I','\0'},{'S','J','\0'},{'S','K','\0'},{'S','L','\0'},{'S','M','\0'},{'S','N','\0'},{'S','O','\0'},{'S','P','\0'},{'S','Q','\0'},{'S','R','\0'},{'S','S','\0'},{'S','T','\0'}};
static const FXchar rkey[20][3]={{'R','A','\0'},{'R','B','\0'},{'R','C','\0'},{'R','D','\0'},{'R','E','\0'},{'R','F','\0'},{'R','G','\0'},{'R','H','\0'},{'R','I','\0'},{'R','J','\0'},{'R','K','\0'},{'R','L','\0'},{'R','M','\0'},{'R','N','\0'},{'R','O','\0'},{'R','P','\0'},{'R','Q','\0'},{'R','R','\0'},{'R','S','\0'},{'R','T','\0'}};
static const FXchar mkey[20][3]={{'M','A','\0'},{'M','B','\0'},{'M','C','\0'},{'M','D','\0'},{'M','E','\0'},{'M','F','\0'},{'M','G','\0'},{'M','H','\0'},{'M','I','\0'},{'M','J','\0'},{'M','K','\0'},{'M','L','\0'},{'M','M','\0'},{'M','N','\0'},{'M','O','\0'},{'M','P','\0'},{'M','Q','\0'},{'M','R','\0'},{'M','S','\0'},{'M','T','\0'}};



// Map
FXDEFMAP(FXReplaceDialog) FXReplaceDialogMap[]={
  FXMAPFUNC(SEL_COMMAND,FXReplaceDialog::ID_ACCEPT,FXReplaceDialog::onCmdAccept),
  FXMAPFUNC(SEL_COMMAND,FXReplaceDialog::ID_REPLACE_TEXT,FXReplaceDialog::onCmdAccept),
  FXMAPFUNC(SEL_COMMAND,FXReplaceDialog::ID_SEARCH_TEXT,FXReplaceDialog::onCmdAccept),
  FXMAPFUNC(SEL_COMMAND,FXReplaceDialog::ID_ALL,FXReplaceDialog::onCmdAll),
  FXMAPFUNCS(SEL_COMMAND,FXReplaceDialog::ID_NEXT,FXReplaceDialog::ID_PREV,FXReplaceDialog::onCmdNext),
  FXMAPFUNC(SEL_UPDATE,FXReplaceDialog::ID_DIR,FXReplaceDialog::onUpdDir),
  FXMAPFUNC(SEL_COMMAND,FXReplaceDialog::ID_DIR,FXReplaceDialog::onCmdDir),
  FXMAPFUNCS(SEL_UPDATE,FXReplaceDialog::ID_MODE+SEARCH_EXACT,FXReplaceDialog::ID_MODE+SEARCH_REGEX,FXReplaceDialog::onUpdMode),
  FXMAPFUNCS(SEL_COMMAND,FXReplaceDialog::ID_MODE+SEARCH_EXACT,FXReplaceDialog::ID_MODE+SEARCH_REGEX,FXReplaceDialog::onCmdMode),
  FXMAPFUNC(SEL_KEYPRESS,FXReplaceDialog::ID_SEARCH_TEXT,FXReplaceDialog::onSearchKey),
  FXMAPFUNC(SEL_KEYPRESS,FXReplaceDialog::ID_REPLACE_TEXT,FXReplaceDialog::onReplaceKey),
  FXMAPFUNCS(SEL_COMMAND,FXReplaceDialog::ID_SEARCH_UP,FXReplaceDialog::ID_SEARCH_DN,FXReplaceDialog::onCmdSearchHist),
  FXMAPFUNCS(SEL_COMMAND,FXReplaceDialog::ID_REPLACE_UP,FXReplaceDialog::ID_REPLACE_DN,FXReplaceDialog::onCmdReplaceHist),
  };


// Object implementation
FXIMPLEMENT(FXReplaceDialog,FXDialogBox,FXReplaceDialogMap,ARRAYNUMBER(FXReplaceDialogMap))


// File Open Dialog
FXReplaceDialog::FXReplaceDialog(FXWindow* owner,const FXString& caption,FXIcon* ic,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,caption,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,w,h,10,10,10,10, 10,10){
  FXHorizontalFrame* buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0,0,0,0,0);
  accept=new FXButton(buttons,tr("&Replace"),NULL,this,ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  every=new FXButton(buttons,tr("Re&place All"),NULL,this,ID_ALL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,6,6,VERT_PAD,VERT_PAD);
  cancel=new FXButton(buttons,tr("&Cancel"),NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  FXHorizontalFrame* pair=new FXHorizontalFrame(buttons,LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0, 0,0,0,0);
  FXArrowButton* searchlast=new FXArrowButton(pair,this,ID_PREV,ARROW_LEFT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  FXArrowButton* searchnext=new FXArrowButton(pair,this,ID_NEXT,ARROW_RIGHT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  FXHorizontalFrame* toppart=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 10,10);
  new FXLabel(toppart,FXString::null,ic,ICON_BEFORE_TEXT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_Y|LAYOUT_FILL_X);
  FXVerticalFrame* entry=new FXVerticalFrame(toppart,LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0);
  searchlabel=new FXLabel(entry,tr("S&earch for:"),NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X);
  searchbox=new FXHorizontalFrame(entry,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 0,0);
  searchtext=new FXTextField(searchbox,26,this,ID_SEARCH_TEXT,TEXTFIELD_ENTER_ONLY|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4);
  FXVerticalFrame* searcharrows=new FXVerticalFrame(searchbox,LAYOUT_RIGHT|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  FXArrowButton* ar1=new FXArrowButton(searcharrows,this,ID_SEARCH_UP,FRAME_RAISED|FRAME_THICK|ARROW_UP|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
  FXArrowButton* ar2=new FXArrowButton(searcharrows,this,ID_SEARCH_DN,FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
  ar1->setArrowSize(3);
  ar2->setArrowSize(3);
  replacelabel=new FXLabel(entry,tr("Replace &with:"),NULL,LAYOUT_LEFT);
  replacebox=new FXHorizontalFrame(entry,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 0,0);
  replacetext=new FXTextField(replacebox,26,this,ID_REPLACE_TEXT,TEXTFIELD_ENTER_ONLY|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4);
  FXVerticalFrame* replacearrows=new FXVerticalFrame(replacebox,LAYOUT_RIGHT|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  FXArrowButton* ar3=new FXArrowButton(replacearrows,this,ID_REPLACE_UP,FRAME_RAISED|FRAME_THICK|ARROW_UP|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
  FXArrowButton* ar4=new FXArrowButton(replacearrows,this,ID_REPLACE_DN,FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
  ar3->setArrowSize(3);
  ar4->setArrowSize(3);
  FXHorizontalFrame* options1=new FXHorizontalFrame(entry,LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
  new FXRadioButton(options1,tr("Ex&act"),this,ID_MODE+SEARCH_EXACT,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
  new FXRadioButton(options1,tr("&Ignore Case"),this,ID_MODE+SEARCH_IGNORECASE,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
  new FXRadioButton(options1,tr("E&xpression"),this,ID_MODE+SEARCH_REGEX,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
  new FXCheckButton(options1,tr("&Backward"),this,ID_DIR,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
  searchlast->setTipText("Ctl-B");
  searchnext->setTipText("Ctl-F");
  searchlast->addHotKey(MKUINT(KEY_b,CONTROLMASK));
  searchnext->addHotKey(MKUINT(KEY_f,CONTROLMASK));
  searchmode=SEARCH_EXACT|SEARCH_FORWARD;
  current=0;
  }


// Set text or pattern to search for
void FXReplaceDialog::setSearchText(const FXString& text){
  searchtext->setText(text);
  }


// Return text or pattern the user has entered
FXString FXReplaceDialog::getSearchText() const {
  return searchtext->getText();
  }


// Set text or pattern to search with
void FXReplaceDialog::setReplaceText(const FXString& text){
  replacetext->setText(text);
  }


// Return text or pattern the user has entered
FXString FXReplaceDialog::getReplaceText() const {
  return replacetext->getText();
  }


// Replace all
long FXReplaceDialog::onCmdAll(FXObject*,FXSelector,void*){
  appendHistory(getSearchText(),getReplaceText(),getSearchMode());
  getApp()->stopModal(this,REPLACE_ALL);
  hide();
  return 1;
  }


// Cause return of modal dialog w/o hiding it
long FXReplaceDialog::onCmdNext(FXObject*,FXSelector sel,void*){
  if(FXSELID(sel)==ID_NEXT) searchmode&=~SEARCH_BACKWARD; else searchmode|=SEARCH_BACKWARD;
  appendHistory(getSearchText(),getReplaceText(),getSearchMode());
  getApp()->stopModal(this,REPLACE_NEXT);
  return 1;
  }


// Return from the dialog
long FXReplaceDialog::onCmdAccept(FXObject*,FXSelector,void*){
  appendHistory(getSearchText(),getReplaceText(),getSearchMode());
  getApp()->stopModal(this,REPLACE);
  hide();
  return 1;
  }


// Change search direction
long FXReplaceDialog::onCmdDir(FXObject*,FXSelector,void*){
  searchmode^=SEARCH_BACKWARD;
  return 1;
  }


// Update search direction
long FXReplaceDialog::onUpdDir(FXObject* sender,FXSelector,void*){
  sender->handle(this,(searchmode&SEARCH_BACKWARD)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change search mode
long FXReplaceDialog::onCmdMode(FXObject*,FXSelector sel,void*){
  searchmode=(searchmode&~SEARCH_MASK) | (FXSELID(sel)-ID_MODE);
  return 1;
  }


// Update search mode
long FXReplaceDialog::onUpdMode(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,((searchmode&SEARCH_MASK)==(FXuint)(FXSELID(sel)-ID_MODE))?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Append entry
void FXReplaceDialog::appendHistory(const FXString& search,const FXString& replace,FXuint mode){
  register const char* val;
  register int i;
  if(!search.empty()){
    if(search!=getApp()->reg().readStringEntry(searchgroup,skey[0],FXString::null)){
      for(i=19; i>0; i--){
        if((val=getApp()->reg().readStringEntry(searchgroup,skey[i-1],NULL))!=NULL) getApp()->reg().writeStringEntry(searchgroup,skey[i],val);
        if((val=getApp()->reg().readStringEntry(searchgroup,rkey[i-1],NULL))!=NULL) getApp()->reg().writeStringEntry(searchgroup,rkey[i],val);
        if((val=getApp()->reg().readStringEntry(searchgroup,mkey[i-1],NULL))!=NULL) getApp()->reg().writeStringEntry(searchgroup,mkey[i],val);
        }
      }
    getApp()->reg().writeStringEntry(searchgroup,skey[0],search.text());
    getApp()->reg().writeStringEntry(searchgroup,rkey[0],replace.text());
    getApp()->reg().writeUnsignedEntry(searchgroup,mkey[0],mode);
    }
  }


// Scroll back in search history
long FXReplaceDialog::onCmdSearchHist(FXObject*,FXSelector sel,void*){
  if(FXSELID(sel)==ID_SEARCH_UP){
    if(current<20 && getApp()->reg().readStringEntry(searchgroup,skey[current],NULL)) current++;
    }
  else{
    if(current>0) current--;
    }
  if(current){
    setSearchText(getApp()->reg().readStringEntry(searchgroup,skey[current-1],FXString::null));
    setReplaceText(getApp()->reg().readStringEntry(searchgroup,rkey[current-1],FXString::null));
    setSearchMode(getApp()->reg().readUnsignedEntry(searchgroup,mkey[current-1],SEARCH_EXACT|SEARCH_FORWARD));
    }
  else{
    setSearchText(FXString::null);
    setReplaceText(FXString::null);
    setSearchMode(SEARCH_EXACT|SEARCH_FORWARD);
    }
  return 1;
  }


// Scroll back in replace history
long FXReplaceDialog::onCmdReplaceHist(FXObject*,FXSelector sel,void*){
  if(FXSELID(sel)==ID_REPLACE_UP){
    if(current<20 && getApp()->reg().readStringEntry(searchgroup,skey[current],NULL)) current++;
    }
  else{
    if(current>0) current--;
    }
  if(current){
    setReplaceText(getApp()->reg().readStringEntry(searchgroup,rkey[current-1],FXString::null));
    }
  else{
    setReplaceText(FXString::null);
    }
  return 1;
  }


// Keyboard press in search text field
long FXReplaceDialog::onSearchKey(FXObject*,FXSelector,void* ptr){
  switch(((FXEvent*)ptr)->code){
    case KEY_Up:
    case KEY_KP_Up:
      onCmdSearchHist(this,FXSEL(SEL_COMMAND,ID_SEARCH_UP),NULL);
      return 1;
    case KEY_Down:
    case KEY_KP_Down:
      onCmdSearchHist(this,FXSEL(SEL_COMMAND,ID_SEARCH_DN),NULL);
      return 1;
    }
  return 0;
  }


// Keyboard press in replace text field
long FXReplaceDialog::onReplaceKey(FXObject*,FXSelector,void* ptr){
  switch(((FXEvent*)ptr)->code){
    case KEY_Up:
    case KEY_KP_Up:
      onCmdReplaceHist(this,FXSEL(SEL_COMMAND,ID_REPLACE_UP),NULL);
      return 1;
    case KEY_Down:
    case KEY_KP_Down:
      onCmdReplaceHist(this,FXSEL(SEL_COMMAND,ID_REPLACE_DN),NULL);
      return 1;
    }
  return 0;
  }


// Force the initial text to be seleced
FXuint FXReplaceDialog::execute(FXuint placement){
  create();
  searchtext->setFocus();
  show(placement);
  current=0;
  return getApp()->runModalFor(this);
  }



// Save data
void FXReplaceDialog::save(FXStream& store) const {
  FXDialogBox::save(store);
  store << searchlabel;
  store << searchtext;
  store << searchbox;
  store << replacelabel;
  store << replacetext;
  store << replacebox;
  store << accept;
  store << cancel;
  store << every;
  store << searchmode;
  }


// Load data
void FXReplaceDialog::load(FXStream& store){
  FXDialogBox::load(store);
  store >> searchlabel;
  store >> searchtext;
  store >> searchbox;
  store >> replacelabel;
  store >> replacetext;
  store >> replacebox;
  store >> accept;
  store >> cancel;
  store >> every;
  store >> searchmode;
  }


// Cleanup
FXReplaceDialog::~FXReplaceDialog(){
  searchlabel=(FXLabel*)-1L;
  searchtext=(FXTextField*)-1L;
  searchbox=(FXHorizontalFrame*)-1L;
  replacelabel=(FXLabel*)-1L;
  replacetext=(FXTextField*)-1L;
  replacebox=(FXHorizontalFrame*)-1L;
  accept=(FXButton*)-1L;
  cancel=(FXButton*)-1L;
  every=(FXButton*)-1L;
  }

}
