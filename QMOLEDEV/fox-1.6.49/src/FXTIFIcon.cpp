/********************************************************************************
*                                                                               *
*                          T I F F  I c o n   O b j e c t                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 Eric Gillet.   All Rights Reserved.                   *
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
* $Id: FXTIFIcon.cpp,v 1.29 2006/01/22 17:58:43 fox Exp $                       *
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
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXTIFIcon.h"


/*
  Notes:
  - FXTIFIcon has an alpha channel.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Suggested file extension
const FXchar FXTIFIcon::fileExt[]="tif";


// Suggested mime type
const FXchar FXTIFIcon::mimeType[]="image/tiff";


// Object implementation
FXIMPLEMENT(FXTIFIcon,FXIcon,NULL,0)


#ifdef HAVE_TIFF_H
const bool FXTIFIcon::supported=true;
#else
const bool FXTIFIcon::supported=false;
#endif


// Initialize
FXTIFIcon::FXTIFIcon(FXApp* a,const void *pix,FXColor clr,FXuint opts,FXint w,FXint h):FXIcon(a,NULL,clr,opts,w,h),codec(0){
  if(pix){
    FXMemoryStream ms;
    ms.open(FXStreamLoad,(FXuchar*)pix);
    loadPixels(ms);
    ms.close();
    }
  }


// Save pixels only
bool FXTIFIcon::savePixels(FXStream& store) const {
  if(fxsaveTIF(store,data,width,height,codec)){
    return true;
    }
  return false;
  }


// Load pixels only
bool FXTIFIcon::loadPixels(FXStream& store){
  FXColor *pixels; FXint w,h;
  if(fxloadTIF(store,pixels,w,h,codec)){
    setData(pixels,IMAGE_OWNED,w,h);
    if(options&IMAGE_ALPHAGUESS) transp=guesstransp();
    return true;
    }
  return false;
  }


// Clean up
FXTIFIcon::~FXTIFIcon(){
  }

}
