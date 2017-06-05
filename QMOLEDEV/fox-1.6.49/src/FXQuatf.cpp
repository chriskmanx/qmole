/********************************************************************************
*                                                                               *
*              S i n g l e - P r e c i s i o n  Q u a t e r n i o n             *
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
* $Id: FXQuatf.cpp,v 1.31 2006/01/22 17:58:37 fox Exp $                         *
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
#include "FXMat3f.h"


using namespace FX;

/*******************************************************************************/

namespace FX {

// Construct from angle and axis
FXQuatf::FXQuatf(const FXVec3f& axis,FXfloat phi){
  setAxisAngle(axis,phi);
  }


// Construct from roll, pitch, yaw
FXQuatf::FXQuatf(FXfloat roll,FXfloat pitch,FXfloat yaw){
  setRollPitchYaw(roll,pitch,yaw);
  }


// Construct quaternion from two unit vectors
FXQuatf::FXQuatf(const FXVec3f& fr,const FXVec3f& to){
  arc(fr,to);
  }


// Construct quaternion from axes
FXQuatf::FXQuatf(const FXVec3f& ex,const FXVec3f& ey,const FXVec3f& ez){
  setAxes(ex,ey,ez);
  }


// Construct quaternion from 3x3 matrix
FXQuatf::FXQuatf(const FXMat3f& mat){
  setAxes(mat[0],mat[1],mat[2]);
  }


// Adjust quaternion length
FXQuatf& FXQuatf::adjust(){
  register FXfloat t=x*x+y*y+z*z+w*w;
  register FXfloat f;
  if(t>0.0f){
    f=1.0f/sqrtf(t);
    x*=f;
    y*=f;
    z*=f;
    w*=f;
    }
  return *this;
  }


// Set axis and angle
void FXQuatf::setAxisAngle(const FXVec3f& axis,FXfloat phi){
  register FXfloat a=0.5f*phi;
  register FXfloat s=sinf(a)/axis.length();
  x=axis.x*s;
  y=axis.y*s;
  z=axis.z*s;
  w=cosf(a);
  }


// Obtain axis and angle
// Remeber that: q = sin(A/2)*(x*i+y*j+z*k)+cos(A/2)
// for unit quaternion |q| == 1
void FXQuatf::getAxisAngle(FXVec3f& axis,FXfloat& phi) const {
  register FXfloat n=sqrtf(x*x+y*y+z*z);
  if(n>0.0f){
    axis.x=x/n;
    axis.y=y/n;
    axis.z=z/n;
    phi=2.0f*acosf(w);
    }
  else{
    axis.x=1.0f;
    axis.y=0.0f;
    axis.z=0.0f;
    phi=0.0f;
    }
  }


// Set quaternion from roll (x), pitch (y), yaw (z)
void FXQuatf::setRollPitchYaw(FXfloat roll,FXfloat pitch,FXfloat yaw){
  register FXfloat sr,cr,sp,cp,sy,cy;
  register FXfloat rr=0.5f*roll;
  register FXfloat pp=0.5f*pitch;
  register FXfloat yy=0.5f*yaw;
  sr=sinf(rr); cr=cosf(rr);
  sp=sinf(pp); cp=cosf(pp);
  sy=sinf(yy); cy=cosf(yy);
  x=sr*cp*cy-cr*sp*sy;
  y=cr*sp*cy+sr*cp*sy;
  z=cr*cp*sy-sr*sp*cy;
  w=cr*cp*cy+sr*sp*sy;
  }


// Set quaternion from yaw (z), pitch (y), roll (x)
void FXQuatf::setYawPitchRoll(FXfloat yaw,FXfloat pitch,FXfloat roll){
  register FXfloat sr,cr,sp,cp,sy,cy;
  register FXfloat rr=0.5f*roll;
  register FXfloat pp=0.5f*pitch;
  register FXfloat yy=0.5f*yaw;
  sr=sinf(rr); cr=cosf(rr);
  sp=sinf(pp); cp=cosf(pp);
  sy=sinf(yy); cy=cosf(yy);
  x=sr*cp*cy+cr*sp*sy;
  y=cr*sp*cy-sr*cp*sy;
  z=cr*cp*sy+sr*sp*cy;
  w=cr*cp*cy-sr*sp*sy;
  }


// Set quaternion from roll (x), yaw (z), pitch (y)
void FXQuatf::setRollYawPitch(FXfloat roll,FXfloat yaw,FXfloat pitch){
  register FXfloat sr,cr,sp,cp,sy,cy;
  register FXfloat rr=0.5f*roll;
  register FXfloat pp=0.5f*pitch;
  register FXfloat yy=0.5f*yaw;
  sr=sinf(rr); cr=cosf(rr);
  sp=sinf(pp); cp=cosf(pp);
  sy=sinf(yy); cy=cosf(yy);
  x=cp*cy*sr+sp*sy*cr;
  y=sp*cy*cr+cp*sy*sr;
  z=cp*sy*cr-sp*cy*sr;
  w=cp*cy*cr-sp*sy*sr;
  }


// Set quaternion from pitch (y), roll (x),yaw (z)
void FXQuatf::setPitchRollYaw(FXfloat pitch,FXfloat roll,FXfloat yaw){
  register FXfloat sr,cr,sp,cp,sy,cy;
  register FXfloat rr=0.5f*roll;
  register FXfloat pp=0.5f*pitch;
  register FXfloat yy=0.5f*yaw;
  sr=sinf(rr); cr=cosf(rr);
  sp=sinf(pp); cp=cosf(pp);
  sy=sinf(yy); cy=cosf(yy);
  x=cy*sr*cp-sy*cr*sp;
  y=cy*cr*sp+sy*sr*cp;
  z=cy*sr*sp+sy*cr*cp;
  w=cy*cr*cp-sy*sr*sp;
  }


// Set quaternion from pitch (y), yaw (z), roll (x)
void FXQuatf::setPitchYawRoll(FXfloat pitch,FXfloat yaw,FXfloat roll){
  register FXfloat sr,cr,sp,cp,sy,cy;
  register FXfloat rr=0.5f*roll;
  register FXfloat pp=0.5f*pitch;
  register FXfloat yy=0.5f*yaw;
  sr=sinf(rr); cr=cosf(rr);
  sp=sinf(pp); cp=cosf(pp);
  sy=sinf(yy); cy=cosf(yy);
  x=sr*cy*cp-cr*sy*sp;
  y=cr*cy*sp-sr*sy*cp;
  z=sr*cy*sp+cr*sy*cp;
  w=cr*cy*cp+sr*sy*sp;
  }


// Set quaternion from yaw (z), roll (x), pitch (y)
void FXQuatf::setYawRollPitch(FXfloat yaw,FXfloat roll,FXfloat pitch){
  register FXfloat sr,cr,sp,cp,sy,cy;
  register FXfloat rr=0.5f*roll;
  register FXfloat pp=0.5f*pitch;
  register FXfloat yy=0.5f*yaw;
  sr=sinf(rr); cr=cosf(rr);
  sp=sinf(pp); cp=cosf(pp);
  sy=sinf(yy); cy=cosf(yy);
  x=cp*sr*cy+sp*cr*sy;
  y=sp*cr*cy-cp*sr*sy;
  z=cp*cr*sy-sp*sr*cy;
  w=cp*cr*cy+sp*sr*sy;
  }



// Obtain roll, pitch, yaw
// Math is from "3D Game Engine Design" by David Eberly pp 19-20.
// However, instead of testing asin(Sy) against -PI/2 and PI/2, I
// test Sy against -1 and 1; this is numerically more stable, as
// asin doesn't like arguments outside [-1,1].
void FXQuatf::getRollPitchYaw(FXfloat& roll,FXfloat& pitch,FXfloat& yaw) const {
  register FXfloat s=-2.0f*(x*z-w*y);
  if(s<1.0f){
    if(-1.0f<s){
      roll=atan2f(2.0f*(y*z+w*x),1.0f-2.0f*(x*x+y*y));
      pitch=asinf(s);
      yaw=atan2f(2.0f*(x*y+w*z),1.0f-2.0f*(y*y+z*z));
      }
    else{
      roll=0.0f;
      pitch=-1.57079632679489661923f;
      yaw=-atan2f(-2.0f*(x*y-w*z),2.0f*(x*z+w*y));
      }
    }
  else{
    roll=0.0f;
    pitch=1.57079632679489661923f;
    yaw=atan2f(-2.0f*(x*y-w*z),2.0f*(x*z+w*y));
    }
  }


// Obtain yaw, pitch, and roll
void FXQuatf::getYawPitchRoll(FXfloat& yaw,FXfloat& pitch,FXfloat& roll) const {
  register FXfloat s=2.0f*(x*z+w*y);
  if(s<1.0f){
    if(-1.0f<s){
      yaw=atan2f(-2.0f*(x*y-w*z),1.0f-2.0f*(y*y+z*z));
      pitch=asinf(s);
      roll=atan2f(-2.0f*(y*z-w*x),1.0f-2.0f*(x*x+y*y));
      }
    else{
      yaw=0.0f;
      pitch=-1.57079632679489661923f;
      roll=-atan2f(2.0f*(x*y+w*z),1.0f-2.0f*(x*x+z*z));
      }
    }
  else{
    yaw=0.0f;
    pitch=1.57079632679489661923f;
    roll=atan2f(2.0f*(x*y+w*z),1.0f-2.0f*(x*x+z*z));
    }
  }


// Obtain roll, yaw, pitch
void FXQuatf::getRollYawPitch(FXfloat& roll,FXfloat& yaw,FXfloat& pitch) const {
  register FXfloat s=2.0f*(x*y+w*z);
  if(s<1.0f){
    if(-1.0f<s){
      roll=atan2f(-2.0f*(y*z-w*x),1.0f-2.0f*(x*x+z*z));
      yaw=asinf(s);
      pitch=atan2f(-2.0f*(x*z-w*y),1.0f-2.0f*(y*y+z*z));
      }
    else{
      roll=0.0f;
      yaw=-1.57079632679489661923f;
      pitch=-atan2f(2.0f*(y*z+w*x),1.0f-2.0f*(x*x+y*y));
      }
    }
  else{
    roll=0.0f;
    yaw=1.57079632679489661923f;
    pitch=atan2f(2.0f*(y*z+w*x),1.0f-2.0f*(x*x+y*y));
    }
  }


// Obtain pitch, roll, yaw
void FXQuatf::getPitchRollYaw(FXfloat& pitch,FXfloat& roll,FXfloat& yaw) const {
  register FXfloat s=2.0f*(y*z+w*x);
  if(s<1.0f){
    if(-1.0f<s){
      pitch=atan2f(-2.0f*(x*z-w*y),1.0f-2.0f*(x*x+y*y));
      roll=asinf(s);
      yaw=atan2f(-2.0f*(x*y-w*z),1.0f-2.0f*(x*x+z*z));
      }
    else{
      pitch=0.0f;
      roll=-1.57079632679489661923f;
      yaw=-atan2f(2.0f*(x*z+w*y),1.0f-2.0f*(y*y+z*z));
      }
    }
  else{
    pitch=0.0f;
    roll=1.57079632679489661923f;
    yaw=atan2f(2.0f*(x*z+w*y),1.0f-2.0f*(y*y+z*z));
    }
  }


// Obtain pitch, yaw, roll
void FXQuatf::getPitchYawRoll(FXfloat& pitch,FXfloat& yaw,FXfloat& roll) const {
  register FXfloat s=-2.0f*(x*y-w*z);
  if(s<1.0f){
    if(-1.0f<s){
      pitch=atan2f(2.0f*(x*z+w*y),1.0f-2.0f*(y*y+z*z));
      yaw=asinf(s);
      roll=atan2f(2.0f*(y*z+w*x),1.0f-2.0f*(x*x+z*z));
      }
    else{
      pitch=0.0f;
      yaw=-1.57079632679489661923f;
      roll=-atan2f(-2.0f*(x*z-w*y),1.0f-2.0f*(x*x+y*y));
      }
    }
  else{
    pitch=0.0f;
    yaw=1.57079632679489661923f;
    roll=atan2f(-2.0f*(x*z-w*y),1.0f-2.0f*(x*x+y*y));
    }
  }


// Obtain yaw, roll, pitch
void FXQuatf::getYawRollPitch(FXfloat& yaw,FXfloat& roll,FXfloat& pitch) const {
  register FXfloat s=-2.0f*(y*z-w*x);
  if(s<1.0f){
    if(-1.0f<s){
      yaw=atan2f(2.0f*(x*y+w*z),1.0f-2.0f*(x*x+z*z));
      roll=asinf(s);
      pitch=atan2f(2.0f*(x*z+w*y),1.0f-2.0f*(x*x+y*y));
      }
    else{
      yaw=0.0f;
      roll=-1.57079632679489661923f;
      pitch=-atan2f(-2.0f*(x*y-w*z),1.0f-2.0f*(y*y+z*z));
      }
    }
  else{
    yaw=0.0f;
    roll=1.57079632679489661923f;
    pitch=atan2f(-2.0f*(x*y-w*z),1.0f-2.0f*(y*y+z*z));
    }
  }


// Set quaternion from axes
void FXQuatf::setAxes(const FXVec3f& ex,const FXVec3f& ey,const FXVec3f& ez){
  register FXfloat trace=ex.x+ey.y+ez.z;
  register FXfloat scale;
  if(trace>0.0f){
    scale=sqrtf(1.0f+trace);
    w=0.5f*scale;
    scale=0.5f/scale;
    x=(ey.z-ez.y)*scale;
    y=(ez.x-ex.z)*scale;
    z=(ex.y-ey.x)*scale;
    }
  else if(ex.x>ey.y && ex.x>ez.z){
    scale=2.0f*sqrtf(1.0f+ex.x-ey.y-ez.z);
    x=0.25f*scale;
    y=(ex.y+ey.x)/scale;
    z=(ex.z+ez.x)/scale;
    w=(ey.z-ez.y)/scale;
    }
  else if(ey.y>ez.z){
    scale=2.0f*sqrtf(1.0f+ey.y-ex.x-ez.z);
    y=0.25f*scale;
    x=(ex.y+ey.x)/scale;
    z=(ey.z+ez.y)/scale;
    w=(ez.x-ex.z)/scale;
    }
  else{
    scale=2.0f*sqrtf(1.0f+ez.z-ex.x-ey.y);
    z=0.25f*scale;
    x=(ex.z+ez.x)/scale;
    y=(ey.z+ez.y)/scale;
    w=(ex.y-ey.x)/scale;
    }
  }


// Get quaternion axes
void FXQuatf::getAxes(FXVec3f& ex,FXVec3f& ey,FXVec3f& ez) const {
  register FXfloat tx=2.0f*x;
  register FXfloat ty=2.0f*y;
  register FXfloat tz=2.0f*z;
  register FXfloat twx=tx*w;
  register FXfloat twy=ty*w;
  register FXfloat twz=tz*w;
  register FXfloat txx=tx*x;
  register FXfloat txy=ty*x;
  register FXfloat txz=tz*x;
  register FXfloat tyy=ty*y;
  register FXfloat tyz=tz*y;
  register FXfloat tzz=tz*z;
  ex.x=1.0f-tyy-tzz;
  ex.y=txy+twz;
  ex.z=txz-twy;
  ey.x=txy-twz;
  ey.y=1.0f-txx-tzz;
  ey.z=tyz+twx;
  ez.x=txz+twy;
  ez.y=tyz-twx;
  ez.z=1.0f-txx-tyy;
  }


// Obtain local x axis
FXVec3f FXQuatf::getXAxis() const {
  register FXfloat ty=2.0f*y;
  register FXfloat tz=2.0f*z;
  return FXVec3f(1.0f-ty*y-tz*z,ty*x+tz*w,tz*x-ty*w);
  }


// Obtain local y axis
FXVec3f FXQuatf::getYAxis() const {
  register FXfloat tx=2.0f*x;
  register FXfloat tz=2.0f*z;
  return FXVec3f(tx*y-tz*w,1.0f-tx*x-tz*z,tz*y+tx*w);
  }


// Obtain local z axis
FXVec3f FXQuatf::getZAxis() const {
  register FXfloat tx=2.0f*x;
  register FXfloat ty=2.0f*y;
  return FXVec3f(tx*z+ty*w,ty*z-tx*w,1.0f-tx*x-ty*y);
  }


// Exponentiate unit quaternion
// Given q = theta*(x*i+y*j+z*k), where length of (x,y,z) is 1,
// then exp(q) = sin(theta)*(x*i+y*j+z*k)+cos(theta).
FXQuatf FXQuatf::exp() const {
  register FXfloat theta=sqrtf(x*x+y*y+z*z);
  register FXfloat scale;
  FXQuatf result(x,y,z,cosf(theta));
  if(theta>0.000001f){
    scale=sinf(theta)/theta;
    result.x*=scale;
    result.y*=scale;
    result.z*=scale;
    }
  return result;
  }


// Take logarithm of unit quaternion
// Given q = sin(theta)*(x*i+y*j+z*k)+cos(theta), length of (x,y,z) is 1,
// then log(q) = theta*(x*i+y*j+z*k).
FXQuatf FXQuatf::log() const {
  register FXfloat scale=sqrtf(x*x+y*y+z*z);
  register FXfloat theta=atan2f(scale,w);
  FXQuatf result(x,y,z,0.0f);
  if(scale>0.0f){
    scale=theta/scale;
    result.x*=scale;
    result.y*=scale;
    result.z*=scale;
    }
  return result;
  }


// Invert quaternion
FXQuatf FXQuatf::invert() const {
  register FXfloat n=x*x+y*y+z*z+w*w;
  return FXQuatf(-x/n,-y/n,-z/n,w/n);
  }


// Invert unit quaternion
FXQuatf FXQuatf::unitinvert() const {
  return FXQuatf(-x,-y,-z,w);
  }


// Conjugate quaternion
FXQuatf FXQuatf::conj() const {
  return FXQuatf(-x,-y,-z,w);
  }


// Construct quaternion from arc a->b on unit sphere.
//
// Explanation: a quaternion which rotates by angle theta about unit axis a
// is specified as:
//
//   q = (a * sin(theta/2), cos(theta/2)).
//
// Assuming is f and t are unit length, we have:
//
//  sin(theta) = | f x t |
//
// and
//
//  cos(theta) = f . t
//
// Using sin(2 * x) = 2 * sin(x) * cos(x), we get:
//
//  a * sin(theta/2) = (f x t) * sin(theta/2) / (2 * sin(theta/2) * cos(theta/2))
//
//                   = (f x t) / (2 * cos(theta/2))
//
// Using cos^2(x)=(1 + cos(2 * x)) / 2, we get:
//
//  4 * cos^2(theta/2) = 2 + 2 * cos(theta)
//
//                     = 2 + 2 * (f . t)
// Ergo:
//
//  2 * cos(theta/2)   = sqrt(2 + 2 * (f . t))
//
FXQuatf& FXQuatf::arc(const FXVec3f& f,const FXVec3f& t){
  register FXfloat dot=f.x*t.x+f.y*t.y+f.z*t.z,div;
  if(dot> 0.999999f){           // Unit quaternion
    x= 0.0f;
    y= 0.0f;
    z= 0.0f;
    w= 1.0f;
    }
  else if(dot<-0.999999f){      // 180 quaternion
    if(fabsf(f.z)<fabsf(f.x) && fabsf(f.z)<fabs(f.y)){  // x, y largest magnitude
      x= f.x*f.z-f.z*f.y;
      y= f.z*f.x+f.y*f.z;
      z=-f.y*f.y-f.x*f.x;
      }
    else if(fabsf(f.y)<fabsf(f.x)){                     // y, z largest magnitude
      x= f.y*f.z-f.x*f.y;
      y= f.x*f.x+f.z*f.z;
      z=-f.z*f.y-f.y*f.x;
      }
    else{                                               // x, z largest magnitude
      x=-f.z*f.z-f.y*f.y;
      y= f.y*f.x-f.x*f.z;
      z= f.x*f.y+f.z*f.x;
      }
    dot=x*x+y*y+z*z;
    div=sqrtf(dot);
    x/=div;
    y/=div;
    z/=div;
    w=0.0f;
    }
  else{
    div=sqrtf((dot+1.0f)*2.0f);
    x=(f.y*t.z-f.z*t.y)/div;
    y=(f.z*t.x-f.x*t.z)/div;
    z=(f.x*t.y-f.y*t.x)/div;
    w=div*0.5f;
    }
  return *this;
  }


// Spherical lerp
FXQuatf& FXQuatf::lerp(const FXQuatf& u,const FXQuatf& v,FXfloat f){
  register FXfloat alpha,beta,theta,sin_t,cos_t;
  register FXint flip=0;
  cos_t = u.x*v.x+u.y*v.y+u.z*v.z+u.w*v.w;
  if(cos_t<0.0f){ cos_t = -cos_t; flip=1; }
  if((1.0f-cos_t)<0.000001f){
    beta = 1.0f-f;
    alpha = f;
    }
  else{
    theta = acosf(cos_t);
    sin_t = sinf(theta);
    beta = sinf(theta-f*theta)/sin_t;
    alpha = sinf(f*theta)/sin_t;
    }
  if(flip) alpha = -alpha;
  x=beta*u.x+alpha*v.x;
  y=beta*u.y+alpha*v.y;
  z=beta*u.z+alpha*v.z;
  w=beta*u.w+alpha*v.w;
  return *this;
  }


// Multiply quaternions
FXQuatf FXQuatf::operator*(const FXQuatf& q) const {
  return FXQuatf(w*q.x+x*q.w+y*q.z-z*q.y, w*q.y+y*q.w+z*q.x-x*q.z, w*q.z+z*q.w+x*q.y-y*q.x, w*q.w-x*q.x-y*q.y-z*q.z);
  }


/*

According to someone on gdalgorithms list, this is faster:

V' = V - 2 * cross( cross( q.xyz, V ) - q.w * V ), q.xyz )

==
v' = q.v.q*

expand to get:

v' = (s.v)s + w(wv + s x v) + s x (wv + s x v)

Where s is the vector part of the quaternion and w is the scalar part (q = (w, s))

Expanding this out further gives:

v' = (s.v)s + (w^2)v + 2w(s x v) + s x (s x v)

Use the vector triple product identity:

a x (b x c) = b(a.c) - c(a.b)

To give

v' + s(s.v) - v(s.s) = (s.v)s + (w^2)v + 2w(s x v) + 2(s x (s x v))

Cancelling to:

v' = (w^2 + s.s)v + 2(s x (s x v + wv))

From the definition of a quaternion representing a rotation,
w = cos(a/2), s = sin(a/2)*r where r is a unit vector representing the
axis of the rotation. So (w^2 + s.s) = (cos(a/2))^2 + (sin(a/2))^2 = 1,
giving us the final formula:

v' = v + 2(s x (s x v + vw))
*/


// Rotation of a vector by a quaternion; this is defined as q.v.q*
// where q* is the conjugate of q.
FXVec3f FXQuatf::operator*(const FXVec3f& v) const {
  return v*FXMat3f(*this);
  }


}

