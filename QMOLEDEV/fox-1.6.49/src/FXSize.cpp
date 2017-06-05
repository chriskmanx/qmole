/********************************************************************************
*                                                                               *
*                               S i z e    C l a s s                            *
*                                                                               *
*********************************************************************************
* Copyright (C) 1994,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXSize.cpp,v 1.12 2006/01/22 17:58:41 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxpriv.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXSize.h"
#include "FXPoint.h"

using namespace FX;

/*******************************************************************************/

namespace FX {

// Grow by amount on all sides
FXSize& FXSize::grow(FXshort margin){
  w+=(margin+margin);
  h+=(margin+margin);
  return *this;
  }


// Grow by different amounts horizontally and vertically
FXSize& FXSize::grow(FXshort hormargin,FXshort vermargin){
  w+=(hormargin+hormargin);
  h+=(vermargin+vermargin);
  return *this;
  }


// Grow by different amounts on all sides
FXSize& FXSize::grow(FXshort leftmargin,FXshort rightmargin,FXshort topmargin,FXshort bottommargin){
  w+=(leftmargin+rightmargin);
  h+=(topmargin+bottommargin);
  return *this;
  }


// Shrink by amount
FXSize& FXSize::shrink(FXshort margin){
  w-=(margin+margin);
  h-=(margin+margin);
  return *this;
  }


// Shrink by different amounts horizontally and vertically
FXSize& FXSize::shrink(FXshort hormargin,FXshort vermargin){
  w-=(hormargin+hormargin);
  h-=(vermargin+vermargin);
  return *this;
  }


// Shrink by different amounts on all sides
FXSize& FXSize::shrink(FXshort leftmargin,FXshort rightmargin,FXshort topmargin,FXshort bottommargin){
  w-=(leftmargin+rightmargin);
  h-=(topmargin+bottommargin);
  return *this;
  }


// Save object to a stream
FXStream& operator<<(FXStream& store,const FXSize& s){
  store << s.w << s.h;
  return store;
  }

// Load object from a stream
FXStream& operator>>(FXStream& store,FXSize& s){
  store >> s.w >> s.h;
  return store;
  }

}
