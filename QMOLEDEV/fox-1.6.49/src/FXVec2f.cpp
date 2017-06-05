/********************************************************************************
*                                                                               *
*       S i n g l e - P r e c i s i o n   2 - E l e m e n t   V e c t o r       *
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
* $Id: FXVec2f.cpp,v 1.9 2006/01/22 17:58:51 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXObject.h"
#include "FXVec2f.h"
#include "FXVec3f.h"
#include "FXMat3f.h"


using namespace FX;

/*******************************************************************************/

namespace FX {


FXVec2f normalize(const FXVec2f& v){
  register FXfloat t=v.length();
  if(t>0.0f){ return FXVec2f(v.x/t,v.y/t); }
  return FXVec2f(0.0f,0.0f);
  }


// Vector times matrix
FXVec2f FXVec2f::operator*(const FXMat3f& m) const {
  FXASSERT(m[0][2]==0.0f && m[1][2]==0.0f && m[2][2]==1.0f);
  return FXVec2f(x*m[0][0]+y*m[1][0]+m[2][0], x*m[0][1]+y*m[1][1]+m[2][1]);
  }


FXStream& operator<<(FXStream& store,const FXVec2f& v){
  store << v.x << v.y;
  return store;
  }


FXStream& operator>>(FXStream& store,FXVec2f& v){
  store >> v.x >> v.y;
  return store;
  }

}
