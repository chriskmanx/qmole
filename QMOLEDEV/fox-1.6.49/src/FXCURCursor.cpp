/********************************************************************************
*                                                                               *
*                        C U R   C u r s o r    O b j e c t                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Sander Jansen.   All Rights Reserved.              *
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
* $Id: FXCURCursor.cpp,v 1.25 2006/01/22 17:58:18 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXMemoryStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXCURCursor.h"


/*
 Notes:
  - Tossed old code now that FXCursor has an RGBA representation.
*/


using namespace FX;


/*******************************************************************************/

namespace FX {


// Suggested file extension
const FXchar FXCURCursor::fileExt[]="cur";


// Object implementation
FXIMPLEMENT(FXCURCursor,FXCursor,NULL,0)


// Constructor
FXCURCursor::FXCURCursor(FXApp* a,const void *pix):FXCursor(a,NULL,0,0,0,0){
  if(pix){
    FXMemoryStream ms;
    ms.open(FXStreamLoad,(FXuchar*)pix);
    fxloadICO(ms,data,width,height,hotx,hoty);
    options|=CURSOR_OWNED;
    ms.close();
    }
  }



// Save pixel data only, in CUR format
bool FXCURCursor::savePixels(FXStream& store) const {
  if(fxsaveICO(store,data,width,height,hotx,hoty)){
    return true;
    }
  return false;
  }


// Load cursor mask and image
bool FXCURCursor::loadPixels(FXStream & store){
  if(options&CURSOR_OWNED){FXFREE(&data);}
  if(fxloadICO(store,data,width,height,hotx,hoty)){
    options|=CURSOR_OWNED;
    return true;
    }
  return false;
  }


// Destroy
FXCURCursor::~FXCURCursor(){
  }

}
