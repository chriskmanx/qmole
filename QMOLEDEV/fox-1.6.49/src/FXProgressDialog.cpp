/********************************************************************************
*                                                                               *
*                      P r o g r e s s   D i a l o g   B o x                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXProgressDialog.cpp,v 1.26 2006/01/22 17:58:37 fox Exp $                *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
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
#include "FXGIFIcon.h"
#include "FXFrame.h"
#include "FXSeparator.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXProgressBar.h"
#include "FXPacker.h"
#include "FXHorizontalFrame.h"
#include "FXVerticalFrame.h"
#include "FXProgressDialog.h"

/*
  Notes:
  -
*/

// Padding for buttons
#define HORZ_PAD 20
#define VERT_PAD 2

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXProgressDialog) FXProgressDialogMap[]={
  FXMAPFUNC(SEL_COMMAND,FXProgressDialog::ID_CANCEL,FXProgressDialog::onCmdCancel),
  FXMAPFUNC(SEL_COMMAND,FXProgressDialog::ID_SETVALUE,FXProgressDialog::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXProgressDialog::ID_SETINTVALUE,FXProgressDialog::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXProgressDialog::ID_GETINTVALUE,FXProgressDialog::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXProgressDialog::ID_GETSTRINGVALUE,FXProgressDialog::onCmdGetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXProgressDialog::ID_SETSTRINGVALUE,FXProgressDialog::onCmdSetStringValue),
  };


// Object implementation
FXIMPLEMENT(FXProgressDialog,FXDialogBox,FXProgressDialogMap,ARRAYNUMBER(FXProgressDialogMap))


// Serialization
FXProgressDialog::FXProgressDialog(){
  cancelled=FALSE;
  }


// Create progress dialog box
FXProgressDialog::FXProgressDialog(FXWindow* owner,const FXString& caption,const FXString& label,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,caption,opts,x,y,FXMAX(w,300),h,10,10,10,10, 10,10){
  cancel=new FXButton(this,tr("&Cancel"),NULL,this,ID_CANCEL,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_SIDE_BOTTOM|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  separator=new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  FXHorizontalFrame* toppart=new FXHorizontalFrame(this,LAYOUT_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 10,10);
  progress=new FXProgressBar(toppart,NULL,0,PROGRESSBAR_PERCENTAGE|PROGRESSBAR_DIAL|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,64,64,0,0,0,0);
  message=new FXLabel(toppart,label,NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_CENTER_Y);
  cancel->setFocus();
  if(!(opts&PROGRESSDIALOG_CANCEL)){
    cancel->hide();
    separator->hide();
    }
  cancelled=FALSE;
  }


// Close dialog, cancelling operation in progress
long FXProgressDialog::onCmdCancel(FXObject* sender,FXSelector sel,void* ptr){
  FXDialogBox::onCmdCancel(sender,sel,ptr);
  setCancelled(TRUE);
  return 1;
  }


// Change dial value
long FXProgressDialog::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setProgress((FXuint)(FXival)ptr);
  return 1;
  }


// Change dial value
long FXProgressDialog::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setProgress(*((FXint*)ptr));
  return 1;
  }


// Get dial value
long FXProgressDialog::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getProgress();
  return 1;
  }


// Change message label
long FXProgressDialog::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setMessage(*((FXString*)ptr));
  return 1;
  }

// Get message label
long FXProgressDialog::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getMessage();
  return 1;
  }


// Change the progress message; force it to be displayed immediately
void FXProgressDialog::setMessage(const FXString& msg){
  message->setText(msg);
  message->repaint();
  getApp()->flush();
  }


// Get progress message
FXString FXProgressDialog::getMessage() const {
  return message->getText();
  }


// Change style of the progress bar widget
void FXProgressDialog::setBarStyle(FXuint style){
  progress->setBarStyle(style);
  }


// Get style of the progress bar widget
FXuint FXProgressDialog::getBarStyle() const {
  return progress->getBarStyle();
  }


// Change the amount of progress
void FXProgressDialog::setProgress(FXuint value){
  progress->setProgress(value);
  }


// Get current progress
FXuint FXProgressDialog::getProgress() const {
  return progress->getProgress();
  }


// Set total amount of progress
void FXProgressDialog::setTotal(FXuint value){
  progress->setTotal(value);
  }


// Return total amount of progrss
FXuint FXProgressDialog::getTotal() const {
  return progress->getTotal();
  }


// Increment progress by given amount
void FXProgressDialog::increment(FXuint value){
  progress->increment(value);
  }


// Destroy it
FXProgressDialog::~FXProgressDialog(){
  progress=(FXProgressBar*)-1L;
  message=(FXLabel*)-1L;
  }

}
