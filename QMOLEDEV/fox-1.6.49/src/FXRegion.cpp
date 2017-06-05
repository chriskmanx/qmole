/********************************************************************************
*                                                                               *
*                      C l i p p i n g   R e g i o n                            *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXRegion.cpp,v 1.31 2006/01/22 17:58:39 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegion.h"


/*
  Notes:
  - Add some more ways to create regions

*/


using namespace FX;

/*******************************************************************************/

namespace FX {

// Construct new empty region
FXRegion::FXRegion(){
#ifndef WIN32
  region=XCreateRegion();
#else
  region=(void*)CreateRectRgn(0,0,0,0);
#endif
  }


// Construct rectangle region
FXRegion::FXRegion(FXint x,FXint y,FXint w,FXint h){
#ifndef WIN32
  XRectangle r;
  r.x=x; r.y=y; r.width=w; r.height=h;
  region=XCreateRegion();
  XUnionRectWithRegion(&r,(Region)region,(Region)region);
#else
  region=(void*)CreateRectRgn(x,y,x+w,y+h);
#endif
  }


// Construct new region from rectangle rect
FXRegion::FXRegion(const FXRectangle& rect){
#ifndef WIN32
  region=XCreateRegion();
  XUnionRectWithRegion((XRectangle*)&rect,(Region)region,(Region)region);
#else
  region=(void*)CreateRectRgn(rect.x,rect.y,rect.x+rect.w,rect.y+rect.h);
#endif
  }


// Construct polygon region
FXRegion::FXRegion(const FXPoint* points,FXuint npoints,bool winding){
#ifndef WIN32
  region=XPolygonRegion((XPoint*)points,npoints,winding?WindingRule:EvenOddRule);
#else
  register FXuint i;
  POINT pts[1024];
  for(i=0; i<npoints; i++){
    pts[i].x=points[i].x;
    pts[i].y=points[i].y;
    }
  region=(void*)CreatePolygonRgn(pts,npoints,winding?WINDING:ALTERNATE);
#endif
  }


// Construct new region copied from region r
FXRegion::FXRegion(const FXRegion& r){
#ifndef WIN32
  region=XCreateRegion();
  XUnionRegion((Region)r.region,(Region)region,(Region)region);
#else
  region=(void*)CreateRectRgn(0,0,0,0);
  CombineRgn((HRGN)region,(HRGN)r.region,(HRGN)region,RGN_COPY);
#endif
  }


// Assign region r to this one
FXRegion& FXRegion::operator=(const FXRegion& r){
#ifndef WIN32
  if(region!=r.region){
    XDestroyRegion((Region)region);
    region=XCreateRegion();
    XUnionRegion((Region)r.region,(Region)region,(Region)region);
    }
#else
  CombineRgn((HRGN)region,(HRGN)r.region,(HRGN)r.region,RGN_COPY);
#endif
  return *this;
  }


// Return TRUE if region is empty
bool FXRegion::empty() const {
#ifndef WIN32
  return XEmptyRegion((Region)region);
#else
  return OffsetRgn((HRGN)region,0,0)==NULLREGION;
#endif
  }


// Return TRUE if region contains point
bool FXRegion::contains(FXint x,FXint y) const {
#ifndef WIN32
  return XPointInRegion((Region)region,x,y);
#else
  return region && PtInRegion((HRGN)region,x,y);
#endif
  }

// Return TRUE if region contains rectangle
// Contributed by Daniel Gehriger <gehriger@linkcad.com>.
bool FXRegion::contains(FXint x,FXint y,FXint w,FXint h) const {
#ifndef WIN32
  return XRectInRegion((Region)region,x,y,w,h);
#else
  RECT rect;
  rect.left   = x;
  rect.top    = y;
  rect.right  = x + w;
  rect.bottom = y + h;
  return region && RectInRegion((HRGN)region,&rect);
#endif
  }


// Return bounding box
FXRectangle FXRegion::bounds() const {
  FXRectangle result;
#ifndef WIN32
  XClipBox((Region)region,(XRectangle*)&result);
#else
  RECT rect;
  GetRgnBox((HRGN)region,&rect);
  result.x=(FXshort)rect.left;
  result.y=(FXshort)rect.top;
  result.w=(FXshort)(rect.right-rect.left);
  result.h=(FXshort)(rect.bottom-rect.top);
#endif
  return result;
  }


// Offset region by dx,dy
FXRegion& FXRegion::offset(FXint dx,FXint dy){
#ifndef WIN32
  XOffsetRegion((Region)region,dx,dy);
#else
  OffsetRgn((HRGN)region,dx,dy);
#endif
  return *this;
  }


// Return TRUE if region equal to this one
bool FXRegion::operator==(const FXRegion& r) const {
#ifndef WIN32
  return XEqualRegion((Region)region,(Region)r.region);
#else
  return EqualRgn((HRGN)region,(HRGN)r.region)!=0;
#endif
  }


// Return TRUE if region not equal to this one
bool FXRegion::operator!=(const FXRegion& r) const {
#ifndef WIN32
  return !XEqualRegion((Region)region,(Region)r.region);
#else
  return EqualRgn((HRGN)region,(HRGN)r.region)==0;
#endif
  }


// Union region r with this one
FXRegion& FXRegion::operator+=(const FXRegion& r){
#ifndef WIN32
  Region res=XCreateRegion();
  XUnionRegion((Region)region,(Region)r.region,res);
  XDestroyRegion((Region)region);
  region=res;
#else
  CombineRgn((HRGN)region,(HRGN)region,(HRGN)r.region,RGN_OR);
#endif
  return *this;
  }


// Intersect region r with this one
FXRegion& FXRegion::operator*=(const FXRegion& r){
#ifndef WIN32
  Region res=XCreateRegion();
  XIntersectRegion((Region)region,(Region)r.region,res);
  XDestroyRegion((Region)region);
  region=res;
#else
  CombineRgn((HRGN)region,(HRGN)region,(HRGN)r.region,RGN_AND);
#endif
  return *this;
  }


// Substract region r from this one
FXRegion& FXRegion::operator-=(const FXRegion& r){
#ifndef WIN32
  Region res=XCreateRegion();
  XSubtractRegion((Region)region,(Region)r.region,res);
  XDestroyRegion((Region)region);
  region=res;
#else
  CombineRgn((HRGN)region,(HRGN)region,(HRGN)r.region,RGN_DIFF);
#endif
  return *this;
  }


// Xor region r with this one
FXRegion& FXRegion::operator^=(const FXRegion& r){
#ifndef WIN32
  Region res=XCreateRegion();
  XXorRegion((Region)region,(Region)r.region,res);
  XDestroyRegion((Region)region);
  region=res;
#else
  CombineRgn((HRGN)region,(HRGN)region,(HRGN)r.region,RGN_XOR);
#endif
  return *this;
  }


// Union region r with this one
FXRegion FXRegion::operator+(const FXRegion& r) const {
  FXRegion res;
#ifndef WIN32
  XUnionRegion((Region)region,(Region)r.region,(Region)res.region);
#else
  CombineRgn((HRGN)res.region,(HRGN)region,(HRGN)r.region,RGN_OR);
#endif
  return res;
  }


// Intersect region r with this one
FXRegion FXRegion::operator*(const FXRegion& r) const {
  FXRegion res;
#ifndef WIN32
  XIntersectRegion((Region)region,(Region)r.region,(Region)res.region);
#else
  CombineRgn((HRGN)res.region,(HRGN)region,(HRGN)r.region,RGN_AND);
#endif
  return res;
  }


// Subtract region r from this one
FXRegion FXRegion::operator-(const FXRegion& r) const {
  FXRegion res;
#ifndef WIN32
  XSubtractRegion((Region)region,(Region)r.region,(Region)res.region);
#else
  CombineRgn((HRGN)res.region,(HRGN)region,(HRGN)r.region,RGN_DIFF);
#endif
  return res;
  }


// Xor region r with this one
FXRegion FXRegion::operator^(const FXRegion& r) const {
  FXRegion res;
#ifndef WIN32
  XXorRegion((Region)region,(Region)r.region,(Region)res.region);
#else
  CombineRgn((HRGN)res.region,(HRGN)region,(HRGN)r.region,RGN_XOR);
#endif
  return res;
  }


// Reset region to empty
void FXRegion::reset(){
#ifndef WIN32
  XDestroyRegion((Region)region);
  region=XCreateRegion();
#else
  DeleteObject((HRGN)region);
  region=(void*)CreateRectRgn(0,0,0,0);
#endif
  }


// Destroy region
FXRegion::~FXRegion(){
#ifndef WIN32
  XDestroyRegion((Region)region);
#else
  DeleteObject((HRGN)region);
#endif
  }

}

