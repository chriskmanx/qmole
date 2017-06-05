/********************************************************************************
*                                                                               *
*                        X B M   I c o n   O b j e c t                          *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXXBMIcon.cpp,v 1.16 2006/01/22 17:58:52 fox Exp $                       *
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
#include "FXObject.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXDrawable.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXXBMIcon.h"


/*
  Notes:
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Suggested file extension
const FXchar FXXBMIcon::fileExt[]="xbm";


// Suggested mime type
const FXchar FXXBMIcon::mimeType[]="image/xbm";


// Object implementation
FXIMPLEMENT(FXXBMIcon,FXIcon,NULL,0)


// Initialize nicely
FXXBMIcon::FXXBMIcon(FXApp* a,const FXuchar *pixels,const FXuchar *mask,FXColor clr,FXuint opts,FXint w,FXint h):FXIcon(a,NULL,clr,opts,w,h){
  if(pixels && mask){
    fxloadXBM(data,pixels,mask,w,h);
    if(options&IMAGE_ALPHAGUESS) transp=guesstransp();
    options|=IMAGE_OWNED;
    }
  }


// Save object to stream
bool FXXBMIcon::savePixels(FXStream& store) const {
  if(fxsaveXBM(store,data,width,height,-1,-1)){
    return true;
    }
  return false;
  }


// Load object from stream
bool FXXBMIcon::loadPixels(FXStream& store){
  FXColor *pixels; FXint w,h,hotx,hoty;
  if(fxloadXBM(store,pixels,w,h,hotx,hoty)){
    setData(pixels,IMAGE_OWNED,w,h);
    if(options&IMAGE_ALPHAGUESS) transp=guesstransp();
    return true;
    }
  return false;
  }


// Clean up
FXXBMIcon::~FXXBMIcon(){
  }

}
