/********************************************************************************
*                                                                               *
*                           W i z a r d   W i d g e t                           *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXWizard.cpp,v 1.19 2006/01/22 17:58:52 fox Exp $                        *
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXGIFIcon.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXImageFrame.h"
#include "FXSeparator.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXPacker.h"
#include "FXVerticalFrame.h"
#include "FXHorizontalFrame.h"
#include "FXSwitcher.h"
#include "FXWizard.h"
#include "icons.h"



/*
  Notes:
  - It may be nice to be able to sensitize/desensitize the "Next" button based on
    having whether the necessary info has been entered into a pane.
  - Need callbacks to tell target whether FXWizard has switched panes.
  - Need to allow for choices in successor panes.
*/


// Padding for buttons
#define HORZ_PAD      12
#define VERT_PAD      2

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXWizard) FXWizardMap[]={
  FXMAPFUNC(SEL_COMMAND,FXWizard::ID_NEXT,FXWizard::onCmdNext),
  FXMAPFUNC(SEL_UPDATE,FXWizard::ID_NEXT,FXWizard::onUpdNext),
  FXMAPFUNC(SEL_COMMAND,FXWizard::ID_BACK,FXWizard::onCmdBack),
  FXMAPFUNC(SEL_UPDATE,FXWizard::ID_BACK,FXWizard::onUpdBack),
  FXMAPFUNC(SEL_UPDATE,FXWizard::ID_ACCEPT,FXWizard::onUpdFinish),
  };


// Object implementation
FXIMPLEMENT(FXWizard,FXDialogBox,FXWizardMap,ARRAYNUMBER(FXWizardMap))


// Construct free-floating Wizard
FXWizard::FXWizard(FXApp* a,const FXString& name,FXImage *image,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXDialogBox(a,name,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  construct();
  setImage(image);
  }


// Construct Wizard which will always float over the owner window
FXWizard::FXWizard(FXWindow* owner,const FXString& name,FXImage *image,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXDialogBox(owner,name,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  construct();
  setImage(image);
  }


// Common part of constructor
void FXWizard::construct(){
  nexticon=new FXGIFIcon(getApp(),arrownext);
  backicon=new FXGIFIcon(getApp(),arrowprev);
  finishicon=new FXGIFIcon(getApp(),entericon);
  buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0,0,0,0,0);
  finish=new FXButton(buttons,tr("&Finish"),finishicon,this,ID_ACCEPT,ICON_AFTER_TEXT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  advance=new FXButton(buttons,tr("&Next"),nexticon,this,ID_NEXT,BUTTON_INITIAL|BUTTON_DEFAULT|ICON_AFTER_TEXT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  retreat=new FXButton(buttons,tr("&Back"),backicon,this,ID_BACK,ICON_BEFORE_TEXT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD+10,HORZ_PAD+10,VERT_PAD,VERT_PAD);
  new FXFrame(buttons,LAYOUT_FIX_WIDTH|LAYOUT_RIGHT,0,0,10,0);
  cancel=new FXButton(buttons,tr("&Cancel"),NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  sidebar=new FXImageFrame(this,NULL,FRAME_GROOVE|LAYOUT_SIDE_LEFT|LAYOUT_CENTER_Y);
  panels=new FXSwitcher(this,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  finish->hide();
  }


// Update finish panel
long FXWizard::onUpdFinish(FXObject* sender,FXSelector,void*){
  sender->handle(this,(getCurrentPanel()<getNumPanels()-1)?FXSEL(SEL_COMMAND,ID_HIDE):FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  return 1;
  }


// Advance to next page
long FXWizard::onCmdNext(FXObject*,FXSelector,void*){
  setCurrentPanel(getCurrentPanel()+1);
  return 1;
  }


// Update advance to next page
long FXWizard::onUpdNext(FXObject* sender,FXSelector,void*){
  sender->handle(this,(getCurrentPanel()<getNumPanels()-1)?FXSEL(SEL_COMMAND,ID_SHOW):FXSEL(SEL_COMMAND,ID_HIDE),NULL);
  return 1;
  }


// Revert to next page
long FXWizard::onCmdBack(FXObject*,FXSelector,void*){
  setCurrentPanel(getCurrentPanel()-1);
  return 1;
  }


// Update revert to next page
long FXWizard::onUpdBack(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<getCurrentPanel())?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Return number of panels
FXint FXWizard::getNumPanels() const {
  return panels->numChildren();
  }


// Bring the child window at index to the top
void FXWizard::setCurrentPanel(FXint index){
  panels->setCurrent(index);
  }


// Return the index of the child window currently on top
FXint FXWizard::getCurrentPanel() const {
  return panels->getCurrent();
  }


// Change the image being displayed
void FXWizard::setImage(FXImage* img){
  sidebar->setImage(img);
  }


// Return the current image
FXImage* FXWizard::getImage() const {
  return sidebar->getImage();
  }


// Save object to stream
void FXWizard::save(FXStream& store) const {
  FXDialogBox::save(store);
  store << buttons;
  store << sidebar;
  store << advance;
  store << retreat;
  store << finish;
  store << cancel;
  store << panels;
  store << finishicon;
  store << nexticon;
  store << backicon;
  }


// Load object from stream
void FXWizard::load(FXStream& store){
  FXDialogBox::load(store);
  store >> buttons;
  store >> sidebar;
  store >> advance;
  store >> retreat;
  store >> finish;
  store >> cancel;
  store >> panels;
  store >> finishicon;
  store >> nexticon;
  store >> backicon;
  }


// Destroy
FXWizard::~FXWizard(){
  delete finishicon;
  delete nexticon;
  delete backicon;
  buttons=(FXHorizontalFrame*)-1L;
  sidebar=(FXImageFrame*)-1L;
  advance=(FXButton*)-1L;
  retreat=(FXButton*)-1L;
  cancel=(FXButton*)-1L;
  finishicon=(FXIcon*)-1L;
  nexticon=(FXIcon*)-1L;
  backicon=(FXIcon*)-1L;
  }

}
