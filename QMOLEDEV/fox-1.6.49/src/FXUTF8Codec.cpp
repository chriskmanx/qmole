/********************************************************************************
*                                                                               *
*                      U T F - 8  T e x t   C o d e c                           *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by L.Johnson & J.van der Zijp.  All Rights Reserved.  *
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
* $Id: FXUTF8Codec.cpp,v 1.19 2006/01/22 17:58:50 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXDict.h"
#include "FXString.h"
#include "FXTextCodec.h"
#include "FXUTF8Codec.h"


/*
  Notes:
  - This is the utf-8 codec used for external inputs; it takes care of
    things like BOM's being inserted.
*/

/*******************************************************************************/

namespace FX {



// Convert utf8 but strip BOM
FXint FXUTF8Codec::mb2wc(FXwchar& wc,const FXchar* src,FXint nsrc) const {
  register FXint n1,n2;
  n1=utf2wc(wc,src,nsrc);
  if(0<n1 && wc==0xFEFF){
    n2=utf2wc(wc,src,nsrc);
    if(n2<0) return -n1+n2;
    if(n2==0) return 0;
    return n1+n2;
    }
  return n1;
  }


// Convert to utf8
FXint FXUTF8Codec::wc2mb(FXchar* dst,FXint ndst,FXwchar wc) const {
  return wc2utf(dst,ndst,wc);
  }


// Return name
const FXchar* FXUTF8Codec::name() const {
  return "UTF-8";
  }

// Return the IANA mime name for this codec
const FXchar* FXUTF8Codec::mimeName() const {
  return "UTF-8";
  }


// Return code for UTF-8
FXint FXUTF8Codec::mibEnum() const {
  return 106;
  }


// Return aliases
const FXchar* const* FXUTF8Codec::aliases() const {
  static const FXchar *const list[]={"UTF-8",NULL};
  return list;
  }

}

