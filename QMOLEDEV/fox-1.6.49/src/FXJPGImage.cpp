/********************************************************************************
*                                                                               *
*                      J P E G   I m a g e   O b j e c t                        *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by David Tyree.   All Rights Reserved.                *
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
* $Id: FXJPGImage.cpp,v 1.30 2006/01/24 13:53:11 fox Exp $                      *
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
#include "FXJPGImage.h"


/*
  Notes:
  - Requires JPEG library.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Suggested file extension
const FXchar FXJPGImage::fileExt[]="jpg";


// Suggested mime type
const FXchar FXJPGImage::mimeType[]="image/jpeg";


// Object implementation
FXIMPLEMENT(FXJPGImage,FXImage,NULL,0)


#ifdef HAVE_JPEG_H
const bool FXJPGImage::supported=true;
#else
const bool FXJPGImage::supported=false;
#endif


// Initialize
FXJPGImage::FXJPGImage(FXApp* a,const void *pix,FXuint opts,FXint w,FXint h,FXint q):FXImage(a,NULL,opts,w,h),quality(q){
  if(pix){
    FXMemoryStream ms;
    ms.open(FXStreamLoad,(FXuchar*)pix);
    loadPixels(ms);
    ms.close();
    }
  }


// Save the pixels only
bool FXJPGImage::savePixels(FXStream& store) const {
  if(fxsaveJPG(store,data,width,height,quality)){
    return true;
    }
  return false;
  }


// Load pixels only
bool FXJPGImage::loadPixels(FXStream& store){
  FXColor *pixels; FXint w,h;
  if(fxloadJPG(store,pixels,w,h,quality)){
    setData(pixels,IMAGE_OWNED,w,h);
    return true;
    }
  return false;
  }


// Clean up
FXJPGImage::~FXJPGImage(){
  }

}
