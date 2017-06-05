/********************************************************************************
*                                                                               *
*                          P N G   I m a g e   O b j e c t                      *
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
* $Id: FXPNGImage.cpp,v 1.38 2006/01/22 17:58:37 fox Exp $                      *
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
#include "FXPNGImage.h"



/*
  Notes:
  - FXPNGImage has an alpha channel.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Suggested file extension
const FXchar FXPNGImage::fileExt[]="png";


// Suggested mime type
const FXchar FXPNGImage::mimeType[]="image/png";


// Object implementation
FXIMPLEMENT(FXPNGImage,FXImage,NULL,0)


#ifdef HAVE_PNG_H
const bool FXPNGImage::supported=true;
#else
const bool FXPNGImage::supported=false;
#endif


// Initialize
FXPNGImage::FXPNGImage(FXApp* a,const void *pix,FXuint opts,FXint w,FXint h):FXImage(a,NULL,opts,w,h){
  if(pix){
    FXMemoryStream ms;
    ms.open(FXStreamLoad,(FXuchar*)pix);
    loadPixels(ms);
    ms.close();
    }
  }


// Save the pixels only
bool FXPNGImage::savePixels(FXStream& store) const {
  if(fxsavePNG(store,data,width,height)){
    return true;
    }
  return false;
  }


// Load pixels only
bool FXPNGImage::loadPixels(FXStream& store){
  FXColor *pixels; FXint w,h;
  if(fxloadPNG(store,pixels,w,h)){
    setData(pixels,IMAGE_OWNED,w,h);
    return true;
    }
  return false;
  }


// Clean up
FXPNGImage::~FXPNGImage(){
  }

}
