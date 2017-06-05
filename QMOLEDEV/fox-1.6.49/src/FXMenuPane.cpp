/********************************************************************************
*                                                                               *
*                        M e n u   P a n e   W i d g e t                        *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXMenuPane.cpp,v 1.23 2006/01/22 17:58:36 fox Exp $                      *
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
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXMenuPane.h"


/*
  Notes:
  - Should FXPopup still exist?
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXMenuPane,FXPopup,NULL,0)



// Build empty one
FXMenuPane::FXMenuPane(FXWindow* owner,FXuint opts):
  FXPopup(owner,opts|FRAME_RAISED|FRAME_THICK){
  accelTable=new FXAccelTable;
  }


// Cursor is considered inside when it's in this window, or in any subwindow
// that's open; we'll find the latter through the cascade menu, by asking it for
// it's popup window.
bool FXMenuPane::contains(FXint parentx,FXint parenty) const {
  FXint x,y;
  if(FXPopup::contains(parentx,parenty)) return true;
  if(getFocus()){
    getParent()->translateCoordinatesTo(x,y,this,parentx,parenty);
    if(getFocus()->contains(x,y)) return true;
    }
  return false;
  }

}
