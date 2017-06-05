/********************************************************************************
*                                                                               *
*                          R e c t a n g l e    C l a s s                       *
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
* $Id: FXRectangle.cpp,v 1.16 2006/01/22 17:58:39 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxpriv.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"

using namespace FX;

/*******************************************************************************/

namespace FX {

// Fast inlines
static inline FXshort _max(FXshort a,FXshort b){ return a>b?a:b; }
static inline FXshort _min(FXshort a,FXshort b){ return a<b?a:b; }


// Grow by amount
FXRectangle& FXRectangle::grow(FXshort margin){
  x-=margin;
  y-=margin;
  w+=(margin+margin);
  h+=(margin+margin);
  return *this;
  }


// Grow by different amounts horizontally and vertically
FXRectangle& FXRectangle::grow(FXshort hormargin,FXshort vermargin){
  x-=hormargin;
  y-=vermargin;
  w+=(hormargin+hormargin);
  h+=(vermargin+vermargin);
  return *this;
  }


// Grow by different amounts on all sides
FXRectangle& FXRectangle::grow(FXshort leftmargin,FXshort rightmargin,FXshort topmargin,FXshort bottommargin){
  x-=leftmargin;
  y-=topmargin;
  w+=(leftmargin+rightmargin);
  h+=(topmargin+bottommargin);
  return *this;
  }


// Shrink by amount
FXRectangle& FXRectangle::shrink(FXshort margin){
  x+=margin;
  y+=margin;
  w-=(margin+margin);
  h-=(margin+margin);
  return *this;
  }


// Shrink by different amounts horizontally and vertically
FXRectangle& FXRectangle::shrink(FXshort hormargin,FXshort vermargin){
  x+=hormargin;
  y+=vermargin;
  w-=(hormargin+hormargin);
  h-=(vermargin+vermargin);
  return *this;
  }


// Shrink by different amounts on all sides
FXRectangle& FXRectangle::shrink(FXshort leftmargin,FXshort rightmargin,FXshort topmargin,FXshort bottommargin){
  x+=leftmargin;
  y+=topmargin;
  w-=(leftmargin+rightmargin);
  h-=(topmargin+bottommargin);
  return *this;
  }


// Union with rectangle
FXRectangle& FXRectangle::operator+=(const FXRectangle &r){
  w=_max(x+w,r.x+r.w); x=_min(x,r.x); w-=x;
  h=_max(y+h,r.y+r.h); y=_min(y,r.y); h-=y;
  return *this;
  }


// Intersection with rectangle
FXRectangle& FXRectangle::operator*=(const FXRectangle &r){
  w=_min(x+w,r.x+r.w); x=_max(x,r.x); w-=x;
  h=_min(y+h,r.y+r.h); y=_max(y,r.y); h-=y;
  return *this;
  }


// Union between rectangles
FXRectangle FXRectangle::operator+(const FXRectangle& r) const {
  register FXshort xx=_min(x,r.x);
  register FXshort ww=_max(x+w,r.x+r.w)-xx;
  register FXshort yy=_min(y,r.y);
  register FXshort hh=_max(y+h,r.y+r.h)-yy;
  return FXRectangle(xx,yy,ww,hh);
  }


// Intersection between rectangles
FXRectangle FXRectangle::operator*(const FXRectangle& r) const {
  register FXshort xx=_max(x,r.x);
  register FXshort ww=_min(x+w,r.x+r.w)-xx;
  register FXshort yy=_max(y,r.y);
  register FXshort hh=_min(y+h,r.y+r.h)-yy;
  return FXRectangle(xx,yy,ww,hh);
  }



// Save object to a stream
FXStream& operator<<(FXStream& store,const FXRectangle& r){
  store << r.x << r.y << r.w << r.h;
  return store;
  }


// Load object from a stream
FXStream& operator>>(FXStream& store,FXRectangle& r){
  store >> r.x >> r.y >> r.w >> r.h;
  return store;
  }

}
