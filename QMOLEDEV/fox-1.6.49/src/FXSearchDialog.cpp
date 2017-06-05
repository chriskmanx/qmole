/********************************************************************************
*                                                                               *
*                       T e x t   S e a r c h   D i a l o g                     *
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
* $Id: FXSearchDialog.cpp,v 1.33 2006/01/22 17:58:41 fox Exp $                  *
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
#include "FXFont.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXPacker.h"
#include "FXHorizontalFrame.h"
#include "FXTextField.h"
#include "FXReplaceDialog.h"
#include "FXSearchDialog.h"



/*
  Notes:

  - Search dialog is essentially a FXReplaceDialog with some of the buttons
    hidden.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXSearchDialog,FXReplaceDialog,NULL,0)



// File Open Dialog
FXSearchDialog::FXSearchDialog(FXWindow* owner,const FXString& caption,FXIcon* ic,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXReplaceDialog(owner,caption,ic,opts,x,y,w,h){
  accept->setText(tr("&Search"));
  every->hide();
  replacelabel->hide();
  replacebox->hide();
  }


// Cleanup
FXSearchDialog::~FXSearchDialog(){
  }

}

