/********************************************************************************
*                                                                               *
*       S i n g l e - P r e c i s i o n   4 - E l e m e n t   V e c t o r       *
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
* $Id: FXVec4f.cpp,v 1.17 2006/01/22 17:58:51 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXObject.h"
#include "FXVec2f.h"
#include "FXVec3f.h"
#include "FXVec4f.h"
#include "FXQuatf.h"
#include "FXMat4f.h"


using namespace FX;

/*******************************************************************************/

namespace FX {

FXVec4f::FXVec4f(FXColor color){
  x=0.003921568627f*FXREDVAL(color);
  y=0.003921568627f*FXGREENVAL(color);
  z=0.003921568627f*FXBLUEVAL(color);
  w=0.003921568627f*FXALPHAVAL(color);
  }


FXVec4f& FXVec4f::operator=(FXColor color){
  x=0.003921568627f*FXREDVAL(color);
  y=0.003921568627f*FXGREENVAL(color);
  z=0.003921568627f*FXBLUEVAL(color);
  w=0.003921568627f*FXALPHAVAL(color);
  return *this;
  }


FXVec4f::operator FXColor() const {
  return FXRGBA((x*255.0f),(y*255.0f),(z*255.0f),(w*255.0f));
  }


// Normalize vector
FXVec4f normalize(const FXVec4f& v){
  register FXfloat t=v.length();
  if(t>0.0f){ return FXVec4f(v.x/t,v.y/t,v.z/t,v.w/t); }
  return FXVec4f(0.0f,0.0f,0.0f,0.0f);
  }


// Compute plane equation from 3 points a,b,c
FXVec4f plane(const FXVec3f& a,const FXVec3f& b,const FXVec3f& c){
  FXVec3f nm(normal(a,b,c));
  return FXVec4f(nm,-(nm.x*a.x+nm.y*a.y+nm.z*a.z));
  }


// Compute plane equation from vector and distance
FXVec4f plane(const FXVec3f& vec,FXfloat dist){
  FXVec3f nm(normalize(vec));
  return FXVec4f(nm,-dist);
  }


// Compute plane equation from vector and point on plane
FXVec4f plane(const FXVec3f& vec,const FXVec3f& p){
  FXVec3f nm(normalize(vec));
  return FXVec4f(nm,-(nm.x*p.x+nm.y*p.y+nm.z*p.z));
  }


// Compute plane equation from 4 vector
FXVec4f plane(const FXVec4f& vec){
  register FXfloat t=sqrtf(vec.x*vec.x+vec.y*vec.y+vec.z*vec.z);
  return FXVec4f(vec.x/t,vec.y/t,vec.z/t,vec.w/t);
  }


// Signed distance normalized plane and point
FXfloat FXVec4f::distance(const FXVec3f& p) const {
  return x*p.x+y*p.y+z*p.z+w;
  }


// Return true if edge a-b crosses plane
bool FXVec4f::crosses(const FXVec3f& a,const FXVec3f& b) const {
  return (distance(a)>=0.0f) ^ (distance(b)>=0.0f);
  }


// Vector times matrix
FXVec4f FXVec4f::operator*(const FXMat4f& m) const {
  return FXVec4f(x*m[0][0]+y*m[1][0]+z*m[2][0]+w*m[3][0], x*m[0][1]+y*m[1][1]+z*m[2][1]+w*m[3][1], x*m[0][2]+y*m[1][2]+z*m[2][2]+w*m[3][2], x*m[0][3]+y*m[1][3]+z*m[2][3]+w*m[3][3]);
  }


// Save vector to stream
FXStream& operator<<(FXStream& store,const FXVec4f& v){
  store << v.x << v.y << v.z << v.w;
  return store;
  }


// Load vector from stream
FXStream& operator>>(FXStream& store,FXVec4f& v){
  store >> v.x >> v.y >> v.z >> v.w;
  return store;
  }

}
