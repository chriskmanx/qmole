/********************************************************************************
*                                                                               *
*            D o u b l e - P r e c i s i o n   4 x 4   M a t r i x              *
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
* $Id: FXMat4d.cpp,v 1.17 2006/01/22 17:58:35 fox Exp $                         *
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
#include "FXMat3d.h"
#include "FXMat4d.h"


/*
  Notes:
  - Transformations pre-multiply.
  - Goal is same effect as OpenGL.
*/


#define DET2(a00,a01, \
             a10,a11) ((a00)*(a11)-(a10)*(a01))

#define DET3(a00,a01,a02, \
             a10,a11,a12, \
             a20,a21,a22) ((a00)*DET2(a11,a12,a21,a22) - \
                           (a10)*DET2(a01,a02,a21,a22) + \
                           (a20)*DET2(a01,a02,a11,a12))

#define DET4(a00,a01,a02,a03, \
             a10,a11,a12,a13, \
             a20,a21,a22,a23, \
             a30,a31,a32,a33) ((a00)*DET3(a11,a12,a13,a21,a22,a23,a31,a32,a33) - \
                               (a10)*DET3(a01,a02,a03,a21,a22,a23,a31,a32,a33) + \
                               (a20)*DET3(a01,a02,a03,a11,a12,a13,a31,a32,a33) - \
                               (a30)*DET3(a01,a02,a03,a11,a12,a13,a21,a22,a23))

using namespace FX;

/*******************************************************************************/

namespace FX {

// Build matrix from constant
FXMat4d::FXMat4d(FXdouble w){
  m[0][0]=w; m[0][1]=w; m[0][2]=w; m[0][3]=w;
  m[1][0]=w; m[1][1]=w; m[1][2]=w; m[1][3]=w;
  m[2][0]=w; m[2][1]=w; m[2][2]=w; m[2][3]=w;
  m[3][0]=w; m[3][1]=w; m[3][2]=w; m[3][3]=w;
  }


// Build matrix from scalars
FXMat4d::FXMat4d(FXdouble a00,FXdouble a01,FXdouble a02,FXdouble a03,
                 FXdouble a10,FXdouble a11,FXdouble a12,FXdouble a13,
                 FXdouble a20,FXdouble a21,FXdouble a22,FXdouble a23,
                 FXdouble a30,FXdouble a31,FXdouble a32,FXdouble a33){
  m[0][0]=a00; m[0][1]=a01; m[0][2]=a02; m[0][3]=a03;
  m[1][0]=a10; m[1][1]=a11; m[1][2]=a12; m[1][3]=a13;
  m[2][0]=a20; m[2][1]=a21; m[2][2]=a22; m[2][3]=a23;
  m[3][0]=a30; m[3][1]=a31; m[3][2]=a32; m[3][3]=a33;
  }


// Build matrix from four vectors
FXMat4d::FXMat4d(const FXVec4d& a,const FXVec4d& b,const FXVec4d& c,const FXVec4d& d){
  m[0][0]=a[0]; m[0][1]=a[1]; m[0][2]=a[2]; m[0][3]=a[3];
  m[1][0]=b[0]; m[1][1]=b[1]; m[1][2]=b[2]; m[1][3]=b[3];
  m[2][0]=c[0]; m[2][1]=c[1]; m[2][2]=c[2]; m[2][3]=c[3];
  m[3][0]=d[0]; m[3][1]=d[1]; m[3][2]=d[2]; m[3][3]=d[3];
  }


// Copy constructor
FXMat4d::FXMat4d(const FXMat4d& other){
  m[0]=other[0];
  m[1]=other[1];
  m[2]=other[2];
  m[3]=other[3];
  }


// Assignment operator
FXMat4d& FXMat4d::operator=(const FXMat4d& other){
  m[0]=other[0];
  m[1]=other[1];
  m[2]=other[2];
  m[3]=other[3];
  return *this;
  }


// Set matrix to constant
FXMat4d& FXMat4d::operator=(FXdouble w){
  m[0][0]=w; m[0][1]=w; m[0][2]=w; m[0][3]=w;
  m[1][0]=w; m[1][1]=w; m[1][2]=w; m[1][3]=w;
  m[2][0]=w; m[2][1]=w; m[2][2]=w; m[2][3]=w;
  m[3][0]=w; m[3][1]=w; m[3][2]=w; m[3][3]=w;
  return *this;
  }


// Set value from another matrix
FXMat4d& FXMat4d::set(const FXMat4d& other){
  m[0]=other[0];
  m[1]=other[1];
  m[2]=other[2];
  m[3]=other[3];
  return *this;
  }


// Construct from scalar number
FXMat4d& FXMat4d::set(FXdouble w){
  m[0][0]=w; m[0][1]=w; m[0][2]=w; m[0][3]=w;
  m[1][0]=w; m[1][1]=w; m[1][2]=w; m[1][3]=w;
  m[2][0]=w; m[2][1]=w; m[2][2]=w; m[2][3]=w;
  m[3][0]=w; m[3][1]=w; m[3][2]=w; m[3][3]=w;
  return *this;
  }


// Construct from components
FXMat4d& FXMat4d::set(FXdouble a00,FXdouble a01,FXdouble a02,FXdouble a03,
                      FXdouble a10,FXdouble a11,FXdouble a12,FXdouble a13,
                      FXdouble a20,FXdouble a21,FXdouble a22,FXdouble a23,
                      FXdouble a30,FXdouble a31,FXdouble a32,FXdouble a33){
  m[0][0]=a00; m[0][1]=a01; m[0][2]=a02; m[0][3]=a03;
  m[1][0]=a10; m[1][1]=a11; m[1][2]=a12; m[1][3]=a13;
  m[2][0]=a20; m[2][1]=a21; m[2][2]=a22; m[2][3]=a23;
  m[3][0]=a30; m[3][1]=a31; m[3][2]=a32; m[3][3]=a33;
  return *this;
  }


// Construct matrix from three vectors
FXMat4d& FXMat4d::set(const FXVec4d& a,const FXVec4d& b,const FXVec4d& c,const FXVec4d& d){
  m[0][0]=a[0]; m[0][1]=a[1]; m[0][2]=a[2]; m[0][3]=a[3];
  m[1][0]=b[0]; m[1][1]=b[1]; m[1][2]=b[2]; m[1][3]=b[3];
  m[2][0]=c[0]; m[2][1]=c[1]; m[2][2]=c[2]; m[2][3]=c[3];
  m[3][0]=d[0]; m[3][1]=d[1]; m[3][2]=d[2]; m[3][3]=d[3];
  return *this;
  }


// Add matrices
FXMat4d& FXMat4d::operator+=(const FXMat4d& w){
  m[0][0]+=w[0][0]; m[0][1]+=w[0][1]; m[0][2]+=w[0][2]; m[0][3]+=w[0][3];
  m[1][0]+=w[1][0]; m[1][1]+=w[1][1]; m[1][2]+=w[1][2]; m[1][3]+=w[1][3];
  m[2][0]+=w[2][0]; m[2][1]+=w[2][1]; m[2][2]+=w[2][2]; m[2][3]+=w[2][3];
  m[3][0]+=w[3][0]; m[3][1]+=w[3][1]; m[3][2]+=w[3][2]; m[3][3]+=w[3][3];
  return *this;
  }


// Substract matrices
FXMat4d& FXMat4d::operator-=(const FXMat4d& w){
  m[0][0]-=w[0][0]; m[0][1]-=w[0][1]; m[0][2]-=w[0][2]; m[0][3]-=w[0][3];
  m[1][0]-=w[1][0]; m[1][1]-=w[1][1]; m[1][2]-=w[1][2]; m[1][3]-=w[1][3];
  m[2][0]-=w[2][0]; m[2][1]-=w[2][1]; m[2][2]-=w[2][2]; m[2][3]-=w[2][3];
  m[3][0]-=w[3][0]; m[3][1]-=w[3][1]; m[3][2]-=w[3][2]; m[3][3]-=w[3][3];
  return *this;
  }


// Multiply matrix by scalar
FXMat4d& FXMat4d::operator*=(FXdouble w){
  m[0][0]*=w; m[0][1]*=w; m[0][2]*=w; m[0][3]*=w;
  m[1][0]*=w; m[1][1]*=w; m[1][2]*=w; m[2][3]*=w;
  m[2][0]*=w; m[2][1]*=w; m[2][2]*=w; m[3][3]*=w;
  m[3][0]*=w; m[3][1]*=w; m[3][2]*=w; m[3][3]*=w;
  return *this;
  }


// Multiply matrix by matrix
FXMat4d& FXMat4d::operator*=(const FXMat4d& w){
  register FXdouble x,y,z,h;
  x=m[0][0]; y=m[0][1]; z=m[0][2]; h=m[0][3];
  m[0][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  m[0][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  m[0][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  m[0][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  x=m[1][0]; y=m[1][1]; z=m[1][2]; h=m[1][3];
  m[1][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  m[1][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  m[1][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  m[1][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  x=m[2][0]; y=m[2][1]; z=m[2][2]; h=m[2][3];
  m[2][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  m[2][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  m[2][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  m[2][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  x=m[3][0]; y=m[3][1]; z=m[3][2]; h=m[3][3];
  m[3][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  m[3][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  m[3][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  m[3][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  return *this;
  }


// Divide matric by scalar
FXMat4d& FXMat4d::operator/=(FXdouble w){
  m[0][0]/=w; m[0][1]/=w; m[0][2]/=w; m[0][3]/=w;
  m[1][0]/=w; m[1][1]/=w; m[1][2]/=w; m[1][3]/=w;
  m[2][0]/=w; m[2][1]/=w; m[2][2]/=w; m[2][3]/=w;
  m[3][0]/=w; m[3][1]/=w; m[3][2]/=w; m[3][3]/=w;
  return *this;
  }


// Unary minus
FXMat4d FXMat4d::operator-() const {
  return FXMat4d(-m[0][0],-m[0][1],-m[0][2],-m[0][3],
                 -m[1][0],-m[1][1],-m[1][2],-m[1][3],
                 -m[2][0],-m[2][1],-m[2][2],-m[2][3],
                 -m[3][0],-m[3][1],-m[3][2],-m[3][3]);
  }



// Add matrices
FXMat4d FXMat4d::operator+(const FXMat4d& w) const {
  return FXMat4d(m[0][0]+w[0][0],m[0][1]+w[0][1],m[0][2]+w[0][2],m[0][3]+w[0][3],
                 m[1][0]+w[1][0],m[1][1]+w[1][1],m[1][2]+w[1][2],m[1][3]+w[1][3],
                 m[2][0]+w[2][0],m[2][1]+w[2][1],m[2][2]+w[2][2],m[2][3]+w[2][3],
                 m[3][0]+w[3][0],m[3][1]+w[3][1],m[3][2]+w[3][2],m[3][3]+w[3][3]);
  }


// Substract matrices
FXMat4d FXMat4d::operator-(const FXMat4d& w) const {
  return FXMat4d(m[0][0]-w[0][0],m[0][1]-w[0][1],m[0][2]-w[0][2],m[0][3]-w[0][3],
                 m[1][0]-w[1][0],m[1][1]-w[1][1],m[1][2]-w[1][2],m[1][3]-w[1][3],
                 m[2][0]-w[2][0],m[2][1]-w[2][1],m[2][2]-w[2][2],m[2][3]-w[2][3],
                 m[3][0]-w[3][0],m[3][1]-w[3][1],m[3][2]-w[3][2],m[3][3]-w[3][3]);
  }


// Multiply matrices
FXMat4d FXMat4d::operator*(const FXMat4d& w) const {
  register FXdouble x,y,z,h;
  FXMat4d r;
  x=m[0][0]; y=m[0][1]; z=m[0][2]; h=m[0][3];
  r[0][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  r[0][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  r[0][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  r[0][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  x=m[1][0]; y=m[1][1]; z=m[1][2]; h=m[1][3];
  r[1][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  r[1][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  r[1][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  r[1][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  x=m[2][0]; y=m[2][1]; z=m[2][2]; h=m[2][3];
  r[2][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  r[2][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  r[2][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  r[2][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  x=m[3][0]; y=m[3][1]; z=m[3][2]; h=m[3][3];
  r[3][0]=x*w[0][0]+y*w[1][0]+z*w[2][0]+h*w[3][0];
  r[3][1]=x*w[0][1]+y*w[1][1]+z*w[2][1]+h*w[3][1];
  r[3][2]=x*w[0][2]+y*w[1][2]+z*w[2][2]+h*w[3][2];
  r[3][3]=x*w[0][3]+y*w[1][3]+z*w[2][3]+h*w[3][3];
  return r;
  }


// Multiply scalar by matrix
FXMat4d operator*(FXdouble x,const FXMat4d& a){
  return FXMat4d(x*a[0][0],x*a[0][1],x*a[0][2],a[0][3],
                 x*a[1][0],x*a[1][1],x*a[1][2],a[1][3],
                 x*a[2][0],x*a[2][1],x*a[2][2],a[2][3],
                 x*a[3][0],x*a[3][1],x*a[3][2],a[3][3]);
  }


// Multiply matrix by scalar
FXMat4d operator*(const FXMat4d& a,FXdouble x){
  return FXMat4d(a[0][0]*x,a[0][1]*x,a[0][2]*x,a[0][3],
                 a[1][0]*x,a[1][1]*x,a[1][2]*x,a[1][3],
                 a[2][0]*x,a[2][1]*x,a[2][2]*x,a[2][3],
                 a[3][0]*x,a[3][1]*x,a[3][2]*x,a[3][3]);
  }


// Divide scalar by matrix
FXMat4d operator/(FXdouble x,const FXMat4d& a){
  return FXMat4d(x/a[0][0],x/a[0][1],x/a[0][2],a[0][3],
                 x/a[1][0],x/a[1][1],x/a[1][2],a[1][3],
                 x/a[2][0],x/a[2][1],x/a[2][2],a[2][3],
                 x/a[3][0],x/a[3][1],x/a[3][2],a[3][3]);
  }


// Divide matrix by scalar
FXMat4d operator/(const FXMat4d& a,FXdouble x){
  return FXMat4d(a[0][0]/x,a[0][1]/x,a[0][2]/x,a[0][3],
                 a[1][0]/x,a[1][1]/x,a[1][2]/x,a[1][3],
                 a[2][0]/x,a[2][1]/x,a[2][2]/x,a[2][3],
                 a[3][0]/x,a[3][1]/x,a[3][2]/x,a[3][3]);
  }


// Matrix times vector
FXVec4d FXMat4d::operator*(const FXVec4d& v) const {
  register FXdouble x=v.x,y=v.y,z=v.z,w=v.w;
  return FXVec4d(x*m[0][0]+y*m[0][1]+z*m[0][2]+w*m[0][3], x*m[1][0]+y*m[1][1]+z*m[1][2]+w*m[1][3], x*m[2][0]+y*m[2][1]+z*m[2][2]+w*m[2][3], x*m[3][0]+y*m[3][1]+z*m[3][2]+w*m[3][3]);
  }


// Matrix times vector
FXVec3d FXMat4d::operator*(const FXVec3d& v) const {
  register FXdouble x=v.x,y=v.y,z=v.z;
  FXASSERT(m[0][3]==0.0 && m[1][3]==0.0 && m[2][3]==0.0 && m[3][3]==1.0);
  return FXVec3d(x*m[0][0]+y*m[0][1]+z*m[0][2]+m[0][3], x*m[1][0]+y*m[1][1]+z*m[1][2]+m[1][3], x*m[2][0]+y*m[2][1]+z*m[2][2]+m[2][3]);
  }


// Make unit matrix
FXMat4d& FXMat4d::eye(){
  m[0][0]=1.0; m[0][1]=0.0; m[0][2]=0.0; m[0][3]=0.0;
  m[1][0]=0.0; m[1][1]=1.0; m[1][2]=0.0; m[1][3]=0.0;
  m[2][0]=0.0; m[2][1]=0.0; m[2][2]=1.0; m[2][3]=0.0;
  m[3][0]=0.0; m[3][1]=0.0; m[3][2]=0.0; m[3][3]=1.0;
  return *this;
  }


// Orthographic projection
FXMat4d& FXMat4d::ortho(FXdouble left,FXdouble right,FXdouble bottom,FXdouble top,FXdouble hither,FXdouble yon){
  register FXdouble x,y,z,tx,ty,tz,rl,tb,yh,r0,r1,r2,r3;
  rl=right-left;
  tb=top-bottom;
  yh=yon-hither;
  FXASSERT(rl && tb && yh);         // Throw exception in future
  x= 2.0/rl;
  y= 2.0/tb;
  z=-2.0/yh;
  tx=-(right+left)/rl;
  ty=-(top+bottom)/tb;
  tz=-(yon+hither)/yh;
  r0=m[0][0];
  r1=m[1][0];
  r2=m[2][0];
  r3=m[3][0];
  m[0][0]=x*r0;
  m[1][0]=y*r1;
  m[2][0]=z*r2;
  m[3][0]=tx*r0+ty*r1+tz*r2+r3;
  r0=m[0][1];
  r1=m[1][1];
  r2=m[2][1];
  r3=m[3][1];
  m[0][1]=x*r0;
  m[1][1]=y*r1;
  m[2][1]=z*r2;
  m[3][1]=tx*r0+ty*r1+tz*r2+r3;
  r0=m[0][2];
  r1=m[1][2];
  r2=m[2][2];
  r3=m[3][2];
  m[0][2]=x*r0;
  m[1][2]=y*r1;
  m[2][2]=z*r2;
  m[3][2]=tx*r0+ty*r1+tz*r2+r3;
  r0=m[0][3];
  r1=m[1][3];
  r2=m[2][3];
  r3=m[3][3];
  m[0][3]=x*r0;
  m[1][3]=y*r1;
  m[2][3]=z*r2;
  m[3][3]=tx*r0+ty*r1+tz*r2+r3;
  return *this;
  }


// Perspective projection
FXMat4d& FXMat4d::frustum(FXdouble left,FXdouble right,FXdouble bottom,FXdouble top,FXdouble hither,FXdouble yon){
  register FXdouble x,y,a,b,c,d,rl,tb,yh,r0,r1,r2,r3;
  FXASSERT(0.0<hither && hither<yon);  // Throw exception in future
  rl=right-left;
  tb=top-bottom;
  yh=yon-hither;
  FXASSERT(rl && tb);                   // Throw exception in future
  x= 2.0*hither/rl;
  y= 2.0*hither/tb;
  a= (right+left)/rl;
  b= (top+bottom)/tb;
  c=-(yon+hither)/yh;
  d=-(2.0*yon*hither)/yh;
  r0=m[0][0];
  r1=m[1][0];
  r2=m[2][0];
  r3=m[3][0];
  m[0][0]=x*r0;
  m[1][0]=y*r1;
  m[2][0]=a*r0+b*r1+c*r2-r3;
  m[3][0]=d*r2;
  r0=m[0][1];
  r1=m[1][1];
  r2=m[2][1];
  r3=m[3][1];
  m[0][1]=x*r0;
  m[1][1]=y*r1;
  m[2][1]=a*r0+b*r1+c*r2-r3;
  m[3][1]=d*r2;
  r0=m[0][2];
  r1=m[1][2];
  r2=m[2][2];
  r3=m[3][2];
  m[0][2]=x*r0;
  m[1][2]=y*r1;
  m[2][2]=a*r0+b*r1+c*r2-r3;
  m[3][2]=d*r2;
  r0=m[0][3];
  r1=m[1][3];
  r2=m[2][3];
  r3=m[3][3];
  m[0][3]=x*r0;
  m[1][3]=y*r1;
  m[2][3]=a*r0+b*r1+c*r2-r3;
  m[3][3]=d*r2;
  return *this;
  }


// Make left hand matrix
FXMat4d& FXMat4d::left(){
  m[2][0]= -m[2][0];
  m[2][1]= -m[2][1];
  m[2][2]= -m[2][2];
  m[2][3]= -m[2][3];
  return *this;
  }



// Rotate using quaternion
FXMat4d& FXMat4d::rot(const FXQuatd& q){
  register FXdouble x,y,z;

  // Get rotation matrix
  FXMat3d r(q);

  // Pre-multiply
  x=m[0][0]; y=m[1][0]; z=m[2][0];
  m[0][0]=x*r[0][0]+y*r[0][1]+z*r[0][2];
  m[1][0]=x*r[1][0]+y*r[1][1]+z*r[1][2];
  m[2][0]=x*r[2][0]+y*r[2][1]+z*r[2][2];
  x=m[0][1]; y=m[1][1]; z=m[2][1];
  m[0][1]=x*r[0][0]+y*r[0][1]+z*r[0][2];
  m[1][1]=x*r[1][0]+y*r[1][1]+z*r[1][2];
  m[2][1]=x*r[2][0]+y*r[2][1]+z*r[2][2];
  x=m[0][2]; y=m[1][2]; z=m[2][2];
  m[0][2]=x*r[0][0]+y*r[0][1]+z*r[0][2];
  m[1][2]=x*r[1][0]+y*r[1][1]+z*r[1][2];
  m[2][2]=x*r[2][0]+y*r[2][1]+z*r[2][2];
  x=m[0][3]; y=m[1][3]; z=m[2][3];
  m[0][3]=x*r[0][0]+y*r[0][1]+z*r[0][2];
  m[1][3]=x*r[1][0]+y*r[1][1]+z*r[1][2];
  m[2][3]=x*r[2][0]+y*r[2][1]+z*r[2][2];
  return *this;
  }


// Rotate by angle (cos,sin) about arbitrary vector
FXMat4d& FXMat4d::rot(const FXVec3d& v,FXdouble c,FXdouble s){
  register FXdouble xx,yy,zz,xy,yz,zx,xs,ys,zs,t;
  register FXdouble r00,r01,r02,r10,r11,r12,r20,r21,r22;
  register FXdouble x=v.x;
  register FXdouble y=v.y;
  register FXdouble z=v.z;
  register FXdouble mag=x*x+y*y+z*z;
  FXASSERT(-1.00001<c && c<1.00001 && -1.00001<s && s<1.00001);
  if(mag<=1.0E-30) return *this;         // Rotation about 0-length axis
  mag=sqrt(mag);
  x/=mag;
  y/=mag;
  z/=mag;
  xx=x*x;
  yy=y*y;
  zz=z*z;
  xy=x*y;
  yz=y*z;
  zx=z*x;
  xs=x*s;
  ys=y*s;
  zs=z*s;
  t=1.0-c;
  r00=t*xx+c;  r10=t*xy-zs; r20=t*zx+ys;
  r01=t*xy+zs; r11=t*yy+c;  r21=t*yz-xs;
  r02=t*zx-ys; r12=t*yz+xs; r22=t*zz+c;
  x=m[0][0];
  y=m[1][0];
  z=m[2][0];
  m[0][0]=x*r00+y*r01+z*r02;
  m[1][0]=x*r10+y*r11+z*r12;
  m[2][0]=x*r20+y*r21+z*r22;
  x=m[0][1];
  y=m[1][1];
  z=m[2][1];
  m[0][1]=x*r00+y*r01+z*r02;
  m[1][1]=x*r10+y*r11+z*r12;
  m[2][1]=x*r20+y*r21+z*r22;
  x=m[0][2];
  y=m[1][2];
  z=m[2][2];
  m[0][2]=x*r00+y*r01+z*r02;
  m[1][2]=x*r10+y*r11+z*r12;
  m[2][2]=x*r20+y*r21+z*r22;
  x=m[0][3];
  y=m[1][3];
  z=m[2][3];
  m[0][3]=x*r00+y*r01+z*r02;
  m[1][3]=x*r10+y*r11+z*r12;
  m[2][3]=x*r20+y*r21+z*r22;
  return *this;
  }


// Rotate by angle (in radians) about arbitrary vector
FXMat4d& FXMat4d::rot(const FXVec3d& v,FXdouble phi){
  return rot(v,cos(phi),sin(phi));
  }


// Rotate about x-axis
FXMat4d& FXMat4d::xrot(FXdouble c,FXdouble s){
  register FXdouble u,v;
  FXASSERT(-1.00001<c && c<1.00001 && -1.00001<s && s<1.00001);
  u=m[1][0]; v=m[2][0]; m[1][0]=c*u+s*v; m[2][0]=c*v-s*u;
  u=m[1][1]; v=m[2][1]; m[1][1]=c*u+s*v; m[2][1]=c*v-s*u;
  u=m[1][2]; v=m[2][2]; m[1][2]=c*u+s*v; m[2][2]=c*v-s*u;
  u=m[1][3]; v=m[2][3]; m[1][3]=c*u+s*v; m[2][3]=c*v-s*u;
  return *this;
  }


// Rotate by angle about x-axis
FXMat4d& FXMat4d::xrot(FXdouble phi){
  return xrot(cos(phi),sin(phi));
  }


// Rotate about y-axis
FXMat4d& FXMat4d::yrot(FXdouble c,FXdouble s){
  register FXdouble u,v;
  FXASSERT(-1.00001<c && c<1.00001 && -1.00001<s && s<1.00001);
  u=m[0][0]; v=m[2][0]; m[0][0]=c*u-s*v; m[2][0]=c*v+s*u;
  u=m[0][1]; v=m[2][1]; m[0][1]=c*u-s*v; m[2][1]=c*v+s*u;
  u=m[0][2]; v=m[2][2]; m[0][2]=c*u-s*v; m[2][2]=c*v+s*u;
  u=m[0][3]; v=m[2][3]; m[0][3]=c*u-s*v; m[2][3]=c*v+s*u;
  return *this;
  }


// Rotate by angle about y-axis
FXMat4d& FXMat4d::yrot(FXdouble phi){
  return yrot(cos(phi),sin(phi));
  }


// Rotate about z-axis
FXMat4d& FXMat4d::zrot(FXdouble c,FXdouble s){
  register FXdouble u,v;
  FXASSERT(-1.00001<c && c<1.00001 && -1.00001<s && s<1.00001);
  u=m[0][0]; v=m[1][0]; m[0][0]=c*u+s*v; m[1][0]=c*v-s*u;
  u=m[0][1]; v=m[1][1]; m[0][1]=c*u+s*v; m[1][1]=c*v-s*u;
  u=m[0][2]; v=m[1][2]; m[0][2]=c*u+s*v; m[1][2]=c*v-s*u;
  u=m[0][3]; v=m[1][3]; m[0][3]=c*u+s*v; m[1][3]=c*v-s*u;
  return *this;
  }


// Rotate by angle about z-axis
FXMat4d& FXMat4d::zrot(FXdouble phi){
  return zrot(cos(phi),sin(phi));
  }


// Translate
FXMat4d& FXMat4d::trans(FXdouble tx,FXdouble ty,FXdouble tz){
  m[3][0]=m[3][0]+tx*m[0][0]+ty*m[1][0]+tz*m[2][0];
  m[3][1]=m[3][1]+tx*m[0][1]+ty*m[1][1]+tz*m[2][1];
  m[3][2]=m[3][2]+tx*m[0][2]+ty*m[1][2]+tz*m[2][2];
  m[3][3]=m[3][3]+tx*m[0][3]+ty*m[1][3]+tz*m[2][3];
  return *this;
  }


// Translate over vector
FXMat4d& FXMat4d::trans(const FXVec3d& v){
  return trans(v[0],v[1],v[2]);
  }


// Scale unqual
FXMat4d& FXMat4d::scale(FXdouble sx,FXdouble sy,FXdouble sz){
  m[0][0]*=sx; m[0][1]*=sx; m[0][2]*=sx; m[0][3]*=sx;
  m[1][0]*=sy; m[1][1]*=sy; m[1][2]*=sy; m[1][3]*=sy;
  m[2][0]*=sz; m[2][1]*=sz; m[2][2]*=sz; m[2][3]*=sz;
  return *this;
  }


// Scale uniform
FXMat4d& FXMat4d::scale(FXdouble s){
  return scale(s,s,s);
  }


// Scale matrix
FXMat4d& FXMat4d::scale(const FXVec3d& v){
  return scale(v[0],v[1],v[2]);
  }


// Calculate determinant
FXdouble FXMat4d::det() const {
  return DET4(m[0][0],m[0][1],m[0][2],m[0][3],
              m[1][0],m[1][1],m[1][2],m[1][3],
              m[2][0],m[2][1],m[2][2],m[2][3],
              m[3][0],m[3][1],m[3][2],m[3][3]);
  }


// Transpose matrix
FXMat4d FXMat4d::transpose() const {
  return FXMat4d(m[0][0],m[1][0],m[2][0],m[3][0],
                 m[0][1],m[1][1],m[2][1],m[3][1],
                 m[0][2],m[1][2],m[2][2],m[3][2],
                 m[0][3],m[1][3],m[2][3],m[3][3]);
  }


// Invert matrix
FXMat4d FXMat4d::invert() const {
  FXMat4d r(1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0);
  FXMat4d x(*this);
  register FXdouble pvv,t;
  register int i,j,pvi;
  for(i=0; i<4; i++){
    pvv=x[i][i];
    pvi=i;
    for(j=i+1; j<4; j++){   // Find pivot (largest in column i)
      if(fabs(x[j][i])>fabs(pvv)){
        pvi=j;
        pvv=x[j][i];
        }
      }
    FXASSERT(pvv != 0.0);  // Should not be singular
    if(pvi!=i){             // Swap rows i and pvi
      FXSWAP(r[i][0],r[pvi][0],t); FXSWAP(r[i][1],r[pvi][1],t); FXSWAP(r[i][2],r[pvi][2],t); FXSWAP(r[i][3],r[pvi][3],t);
      FXSWAP(x[i][0],x[pvi][0],t); FXSWAP(x[i][1],x[pvi][1],t); FXSWAP(x[i][2],x[pvi][2],t); FXSWAP(x[i][3],x[pvi][3],t);
      }
    x[i][0]/=pvv; x[i][1]/=pvv; x[i][2]/=pvv; x[i][3]/=pvv;
    r[i][0]/=pvv; r[i][1]/=pvv; r[i][2]/=pvv; r[i][3]/=pvv;
    for(j=0; j<4; j++){     // Eliminate column i
      if(j!=i){
        t=x[j][i];
        x[j][0]-=x[i][0]*t; x[j][1]-=x[i][1]*t; x[j][2]-=x[i][2]*t; x[j][3]-=x[i][3]*t;
        r[j][0]-=r[i][0]*t; r[j][1]-=r[i][1]*t; r[j][2]-=r[i][2]*t; r[j][3]-=r[i][3]*t;
        }
      }
    }
  return r;
  }


// Look at
FXMat4d& FXMat4d::look(const FXVec3d& eye,const FXVec3d& cntr,const FXVec3d& vup){
  register FXdouble x0,x1,x2,tx,ty,tz;
  FXVec3d rz,rx,ry;
  rz=normalize(eye-cntr);
  rx=normalize(vup^rz);
  ry=normalize(rz^rx);
  tx= -eye[0]*rx[0]-eye[1]*rx[1]-eye[2]*rx[2];
  ty= -eye[0]*ry[0]-eye[1]*ry[1]-eye[2]*ry[2];
  tz= -eye[0]*rz[0]-eye[1]*rz[1]-eye[2]*rz[2];
  x0=m[0][0]; x1=m[0][1]; x2=m[0][2];
  m[0][0]=rx[0]*x0+rx[1]*x1+rx[2]*x2+tx*m[0][3];
  m[0][1]=ry[0]*x0+ry[1]*x1+ry[2]*x2+ty*m[0][3];
  m[0][2]=rz[0]*x0+rz[1]*x1+rz[2]*x2+tz*m[0][3];
  x0=m[1][0]; x1=m[1][1]; x2=m[1][2];
  m[1][0]=rx[0]*x0+rx[1]*x1+rx[2]*x2+tx*m[1][3];
  m[1][1]=ry[0]*x0+ry[1]*x1+ry[2]*x2+ty*m[1][3];
  m[1][2]=rz[0]*x0+rz[1]*x1+rz[2]*x2+tz*m[1][3];
  x0=m[2][0]; x1=m[2][1]; x2=m[2][2];
  m[2][0]=rx[0]*x0+rx[1]*x1+rx[2]*x2+tx*m[2][3];
  m[2][1]=ry[0]*x0+ry[1]*x1+ry[2]*x2+ty*m[2][3];
  m[2][2]=rz[0]*x0+rz[1]*x1+rz[2]*x2+tz*m[2][3];
  x0=m[3][0]; x1=m[3][1]; x2=m[3][2];
  m[3][0]=rx[0]*x0+rx[1]*x1+rx[2]*x2+tx*m[3][3];
  m[3][1]=ry[0]*x0+ry[1]*x1+ry[2]*x2+ty*m[3][3];
  m[3][2]=rz[0]*x0+rz[1]*x1+rz[2]*x2+tz*m[3][3];
  return *this;
  }


// Save to archive
FXStream& operator<<(FXStream& store,const FXMat4d& m){
  store << m[0] << m[1] << m[2] << m[3];
  return store;
  }


// Load from archive
FXStream& operator>>(FXStream& store,FXMat4d& m){
  store >> m[0] >> m[1] >> m[2] >> m[3];
  return store;
  }

}
