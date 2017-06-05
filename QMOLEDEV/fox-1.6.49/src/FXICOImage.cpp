/********************************************************************************
*                                                                               *
*                          I C O   I m a g e   O b j e c t                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Janusz Ganczarski.   All Rights Reserved.          *
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
* $Id: FXICOImage.cpp,v 1.28 2006/01/22 17:58:31 fox Exp $                      *
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
#include "FXICOImage.h"



/*
  Notes:
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Suggested file extension
const FXchar FXICOImage::fileExt[]="ico";


// Suggested mime type
const FXchar FXICOImage::mimeType[]="image/ico";


// Object implementation
FXIMPLEMENT(FXICOImage,FXImage,NULL,0)


// Initialize
FXICOImage::FXICOImage(FXApp* a,const void *pix,FXuint opts,FXint w,FXint h):FXImage(a,NULL,opts,w,h){
  if(pix){
    FXMemoryStream ms;
    ms.open(FXStreamLoad,(FXuchar*)pix);
    loadPixels(ms);
    ms.close();
    }
  }


// Save pixel data only
bool FXICOImage::savePixels(FXStream& store) const {
  if(fxsaveICO(store,data,width,height,0,0)){
    return true;
    }
  return false;
  }


// Load pixel data only
bool FXICOImage::loadPixels(FXStream& store){
  FXColor *pixels; FXint w,h,hotx,hoty;
  if(fxloadICO(store,pixels,w,h,hotx,hoty)){
    setData(pixels,IMAGE_OWNED,w,h);
    return true;
    }
  return false;
  }


// Clean up
FXICOImage::~FXICOImage(){
  }

}
