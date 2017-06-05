/********************************************************************************
*                                                                               *
*                    F o n t   S e l e c t i o n   D i a l o g                  *
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
* $Id: FXFontDialog.cpp,v 1.23 2006/01/22 17:58:27 fox Exp $                    *
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXFont.h"
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
#include "FXFontSelector.h"
#include "FXFontDialog.h"


/*
  Notes:
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXFontDialog,FXDialogBox,NULL,0)


// Separator item
FXFontDialog::FXFontDialog(FXWindow* owner,const FXString& name,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,w,h,0,0,0,0,4,4){
  fontbox=new FXFontSelector(this,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  fontbox->acceptButton()->setTarget(this);
  fontbox->acceptButton()->setSelector(FXDialogBox::ID_ACCEPT);
  fontbox->cancelButton()->setTarget(this);
  fontbox->cancelButton()->setSelector(FXDialogBox::ID_CANCEL);
  }


// Save data
void FXFontDialog::save(FXStream& store) const {
  FXDialogBox::save(store);
  store << fontbox;
  }


// Load data
void FXFontDialog::load(FXStream& store){
  FXDialogBox::load(store);
  store >> fontbox;
  }


// Change the selected font
void FXFontDialog::setFontSelection(const FXFontDesc& fontdesc){
  fontbox->setFontSelection(fontdesc);
  }


// Return the selected font
void FXFontDialog::getFontSelection(FXFontDesc& fontdesc) const {
  fontbox->getFontSelection(fontdesc);
  }


// Cleanup
FXFontDialog::~FXFontDialog(){
  fontbox=(FXFontSelector*)-1L;
  }

}

