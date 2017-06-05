/********************************************************************************
*                                                                               *
*                           C o l o r   D i a l o g                             *
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
* $Id: FXColorDialog.cpp,v 1.32 2006/01/22 17:58:20 fox Exp $                   *
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
#include "FXId.h"
#include "FXDrawable.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXComposite.h"
#include "FXPacker.h"
#include "FXShell.h"
#include "FXTopWindow.h"
#include "FXDialogBox.h"
#include "FXColorSelector.h"
#include "FXColorDialog.h"


/*
  Notes:
  - Need shared instance of this dialog to pop up when double-clicking
    on a color well.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXColorDialog) FXColorDialogMap[]={
  FXMAPFUNC(SEL_CHANGED,FXColorDialog::ID_COLORSELECTOR,FXColorDialog::onChgColor),
  FXMAPFUNC(SEL_COMMAND,FXColorDialog::ID_COLORSELECTOR,FXColorDialog::onCmdColor),
  };


// Object implementation
FXIMPLEMENT(FXColorDialog,FXDialogBox,FXColorDialogMap,ARRAYNUMBER(FXColorDialogMap))



// Separator item
FXColorDialog::FXColorDialog(FXWindow* owner,const FXString& name,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE|DECOR_CLOSE,x,y,w,h,0,0,0,0,4,4){
  colorbox=new FXColorSelector(this,this,ID_COLORSELECTOR,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  colorbox->acceptButton()->setTarget(this);
  colorbox->acceptButton()->setSelector(FXDialogBox::ID_ACCEPT);
  colorbox->cancelButton()->setTarget(this);
  colorbox->cancelButton()->setSelector(FXDialogBox::ID_CANCEL);
  }


// Change RGBA color
void FXColorDialog::setRGBA(FXColor clr){
  colorbox->setRGBA(clr);
  }


// Retrieve RGBA color
FXColor FXColorDialog::getRGBA() const {
  return colorbox->getRGBA();
  }


// Forward ColorSelector color change to target [a color well]
long FXColorDialog::onChgColor(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CHANGED,message),ptr);
  }


// Forward ColorSelector color command to target [a color well]
long FXColorDialog::onCmdColor(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
  }


// Return true if only opaque colors allowed
FXbool FXColorDialog::isOpaqueOnly() const {
  return colorbox->isOpaqueOnly();
  }


// Change opaque only mode
void FXColorDialog::setOpaqueOnly(FXbool forceopaque){
  colorbox->setOpaqueOnly(forceopaque);
  }


// Save data
void FXColorDialog::save(FXStream& store) const {
  FXDialogBox::save(store);
  store << colorbox;
  }


// Load data
void FXColorDialog::load(FXStream& store){
  FXDialogBox::load(store);
  store >> colorbox;
  }


// Cleanup
FXColorDialog::~FXColorDialog(){
  colorbox=(FXColorSelector*)-1L;
  }

}
