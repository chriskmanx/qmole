/********************************************************************************
*                                                                               *
*       D o u b l e - P r e c i s i o n   4 - E l e m e n t   V e c t o r       *
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
* $Id: FXVec4d.cpp,v 1.15 2006/01/22 17:58:51 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXObject.h"
#include "FXVec2d.h"
#include "FXVec3d.h"
#include "FXVec4d.h"
#include "FXQuatd.h"
#include "FXMat4d.h"


using namespace FX;

/*******************************************************************************/

namespace FX {

FXVec4d::FXVec4d(FXColor color){
  x=0.003921568627*FXREDVAL(color);
  y=0.003921568627*FXGREENVAL(color);
  z=0.003921568627*FXBLUEVAL(color);
  w=0.003921568627*FXALPHAVAL(color);
  }


FXVec4d& FXVec4d::operator=(FXColor color){
  x=0.003921568627*FXREDVAL(color);
  y=0.003921568627*FXGREENVAL(color);
  z=0.003921568627*FXBLUEVAL(color);
  w=0.003921568627*FXALPHAVAL(color);
  return *this;
  }


FXVec4d::operator FXColor() const {
  return FXRGBA((x*255.0),(y*255.0),(z*255.0),(w*255.0));
  }


FXVec4d normalize(const FXVec4d& v){
  register FXdouble t=v.length();
  if(t>0.0){ return FXVec4d(v.x/t,v.y/t,v.z/t,v.w/t); }
  return FXVec4d(0.0,0.0,0.0,0.0);
  }


// Compute plane equation from 3 points a,b,c
FXVec4d plane(const FXVec3d& a,const FXVec3d& b,const FXVec3d& c){
  FXVec3d nm(normal(a,b,c));
  return FXVec4d(nm,-(nm.x*a.x+nm.y*a.y+nm.z*a.z));
  }


// Compute plane equation from vector and distance
FXVec4d plane(const FXVec3d& vec,FXdouble dist){
  FXVec3d nm(normalize(vec));
  return FXVec4d(nm,-dist);
  }


// Compute plane equation from vector and point on plane
FXVec4d plane(const FXVec3d& vec,const FXVec3d& p){
  FXVec3d nm(normalize(vec));
  return FXVec4d(nm,-(nm.x*p.x+nm.y*p.y+nm.z*p.z));
  }


// Compute plane equation from 4 vector
FXVec4d plane(const FXVec4d& vec){
  register FXdouble t=sqrt(vec.x*vec.x+vec.y*vec.y+vec.z*vec.z);
  return FXVec4d(vec.x/t,vec.y/t,vec.z/t,vec.w/t);
  }


// Signed distance normalized plane and point
FXdouble FXVec4d::distance(const FXVec3d& p) const {
  return x*p.x+y*p.y+z*p.z+w;
  }


// Return true if edge a-b crosses plane
bool FXVec4d::crosses(const FXVec3d& a,const FXVec3d& b) const {
  return (distance(a)>=0.0) ^ (distance(b)>=0.0);
  }


// Vector times matrix
FXVec4d FXVec4d::operator*(const FXMat4d& m) const {
  return FXVec4d(x*m[0][0]+y*m[1][0]+z*m[2][0]+w*m[3][0], x*m[0][1]+y*m[1][1]+z*m[2][1]+w*m[3][1], x*m[0][2]+y*m[1][2]+z*m[2][2]+w*m[3][2], x*m[0][3]+y*m[1][3]+z*m[2][3]+w*m[3][3]);
  }


// Save vector to stream
FXStream& operator<<(FXStream& store,const FXVec4d& v){
  store << v.x << v.y << v.z << v.w;
  return store;
  }


// Load vector from stream
FXStream& operator>>(FXStream& store,FXVec4d& v){
  store >> v.x >> v.y >> v.z >> v.w;
  return store;
  }

}
