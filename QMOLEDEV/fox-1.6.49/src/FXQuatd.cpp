/********************************************************************************
*                                                                               *
*              D o u b l e - P r e c i s i o n  Q u a t e r n i o n             *
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
* $Id: FXQuatd.cpp,v 1.28 2006/01/22 17:58:37 fox Exp $                         *
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


using namespace FX;

/*******************************************************************************/

namespace FX {

// Construct from angle and axis
FXQuatd::FXQuatd(const FXVec3d& axis,FXdouble phi){
  setAxisAngle(axis,phi);
  }


// Construct from roll, pitch, yaw
FXQuatd::FXQuatd(FXdouble roll,FXdouble pitch,FXdouble yaw){
  setRollPitchYaw(roll,pitch,yaw);
  }


// Construct quaternion from two unit vectors
FXQuatd::FXQuatd(const FXVec3d& fr,const FXVec3d& to){
  arc(fr,to);
  }


// Construct quaternion from axes
FXQuatd::FXQuatd(const FXVec3d& ex,const FXVec3d& ey,const FXVec3d& ez){
  setAxes(ex,ey,ez);
  }


// Construct quaternion from 3x3 matrix
FXQuatd::FXQuatd(const FXMat3d& mat){
  setAxes(mat[0],mat[1],mat[2]);
  }


// Adjust quaternion length
FXQuatd& FXQuatd::adjust(){
  register FXdouble t=x*x+y*y+z*z+w*w;
  register FXdouble f;
  if(t>0.0){
    f=1.0/sqrt(t);
    x*=f;
    y*=f;
    z*=f;
    w*=f;
    }
  return *this;
  }


// Set axis and angle
void FXQuatd::setAxisAngle(const FXVec3d& axis,FXdouble phi){
  register FXdouble a=0.5*phi;
  register FXdouble s=sin(a)/axis.length();
  x=axis.x*s;
  y=axis.y*s;
  z=axis.z*s;
  w=cos(a);
  }


// Obtain axis and angle
// Remeber that: q = sin(A/2)*(x*i+y*j+z*k)+cos(A/2)
// for unit quaternion |q| == 1
void FXQuatd::getAxisAngle(FXVec3d& axis,FXdouble& phi) const {
  register FXdouble n=sqrt(x*x+y*y+z*z);
  if(n>0.0){
    axis.x=x/n;
    axis.y=y/n;
    axis.z=z/n;
    phi=2.0*acos(w);
    }
  else{
    axis.x=1.0;
    axis.y=0.0;
    axis.z=0.0;
    phi=0.0;
    }
  }


// Set quaternion from roll (x), pitch (y), yaw (z)
void FXQuatd::setRollPitchYaw(FXdouble roll,FXdouble pitch,FXdouble yaw){
  register FXdouble sr,cr,sp,cp,sy,cy;
  register FXdouble rr=0.5*roll;
  register FXdouble pp=0.5*pitch;
  register FXdouble yy=0.5*yaw;
  sr=sin(rr); cr=cos(rr);
  sp=sin(pp); cp=cos(pp);
  sy=sin(yy); cy=cos(yy);
  x=sr*cp*cy-cr*sp*sy;
  y=cr*sp*cy+sr*cp*sy;
  z=cr*cp*sy-sr*sp*cy;
  w=cr*cp*cy+sr*sp*sy;
  }


// Set quaternion from yaw (z), pitch (y), roll (x)
void FXQuatd::setYawPitchRoll(FXdouble yaw,FXdouble pitch,FXdouble roll){
  register FXdouble sr,cr,sp,cp,sy,cy;
  register FXdouble rr=0.5*roll;
  register FXdouble pp=0.5*pitch;
  register FXdouble yy=0.5*yaw;
  sr=sin(rr); cr=cos(rr);
  sp=sin(pp); cp=cos(pp);
  sy=sin(yy); cy=cos(yy);
  x=sr*cp*cy+cr*sp*sy;
  y=cr*sp*cy-sr*cp*sy;
  z=cr*cp*sy+sr*sp*cy;
  w=cr*cp*cy-sr*sp*sy;
  }


// Set quaternion from roll (x), yaw (z), pitch (y)
void FXQuatd::setRollYawPitch(FXdouble roll,FXdouble yaw,FXdouble pitch){
  register FXdouble sr,cr,sp,cp,sy,cy;
  register FXdouble rr=0.5*roll;
  register FXdouble pp=0.5*pitch;
  register FXdouble yy=0.5*yaw;
  sr=sin(rr); cr=cos(rr);
  sp=sin(pp); cp=cos(pp);
  sy=sin(yy); cy=cos(yy);
  x=cp*cy*sr+sp*sy*cr;
  y=sp*cy*cr+cp*sy*sr;
  z=cp*sy*cr-sp*cy*sr;
  w=cp*cy*cr-sp*sy*sr;
  }


// Set quaternion from pitch (y), roll (x),yaw (z)
void FXQuatd::setPitchRollYaw(FXdouble pitch,FXdouble roll,FXdouble yaw){
  register FXdouble sr,cr,sp,cp,sy,cy;
  register FXdouble rr=0.5*roll;
  register FXdouble pp=0.5*pitch;
  register FXdouble yy=0.5*yaw;
  sr=sin(rr); cr=cos(rr);
  sp=sin(pp); cp=cos(pp);
  sy=sin(yy); cy=cos(yy);
  x=cy*sr*cp-sy*cr*sp;
  y=cy*cr*sp+sy*sr*cp;
  z=cy*sr*sp+sy*cr*cp;
  w=cy*cr*cp-sy*sr*sp;
  }


// Set quaternion from pitch (y), yaw (z), roll (x)
void FXQuatd::setPitchYawRoll(FXdouble pitch,FXdouble yaw,FXdouble roll){
  register FXdouble sr,cr,sp,cp,sy,cy;
  register FXdouble rr=0.5*roll;
  register FXdouble pp=0.5*pitch;
  register FXdouble yy=0.5*yaw;
  sr=sin(rr); cr=cos(rr);
  sp=sin(pp); cp=cos(pp);
  sy=sin(yy); cy=cos(yy);
  x=sr*cy*cp-cr*sy*sp;
  y=cr*cy*sp-sr*sy*cp;
  z=sr*cy*sp+cr*sy*cp;
  w=cr*cy*cp+sr*sy*sp;
  }


// Set quaternion from yaw (z), roll (x), pitch (y)
void FXQuatd::setYawRollPitch(FXdouble yaw,FXdouble roll,FXdouble pitch){
  register FXdouble sr,cr,sp,cp,sy,cy;
  register FXdouble rr=0.5*roll;
  register FXdouble pp=0.5*pitch;
  register FXdouble yy=0.5*yaw;
  sr=sin(rr); cr=cos(rr);
  sp=sin(pp); cp=cos(pp);
  sy=sin(yy); cy=cos(yy);
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
void FXQuatd::getRollPitchYaw(FXdouble& roll,FXdouble& pitch,FXdouble& yaw) const {
  register FXdouble s=-2.0f*(x*z-w*y);
  if(s<1.0){
    if(-1.0<s){
      roll=atan2(2.0*(y*z+w*x),1.0-2.0*(x*x+y*y));
      pitch=asin(s);
      yaw=atan2(2.0*(x*y+w*z),1.0-2.0*(y*y+z*z));
      }
    else{
      roll=0.0;
      pitch=-1.57079632679489661923;
      yaw=-atan2(-2.0*(x*y-w*z),2.0*(x*z+w*y));
      }
    }
  else{
    roll=0.0;
    pitch=1.57079632679489661923;
    yaw=atan2(-2.0*(x*y-w*z),2.0*(x*z+w*y));
    }
  }


// Obtain yaw, pitch, and roll
void FXQuatd::getYawPitchRoll(FXdouble& yaw,FXdouble& pitch,FXdouble& roll) const {
  register FXdouble s=2.0*(x*z+w*y);
  if(s<1.0){
    if(-1.0<s){
      yaw=atan2(-2.0*(x*y-w*z),1.0-2.0*(y*y+z*z));
      pitch=asin(s);
      roll=atan2(-2.0*(y*z-w*x),1.0-2.0*(x*x+y*y));
      }
    else{
      yaw=0.0;
      pitch=-1.57079632679489661923;
      roll=-atan2(2.0*(x*y+w*z),1.0-2.0*(x*x+z*z));
      }
    }
  else{
    yaw=0.0;
    pitch=1.57079632679489661923;
    roll=atan2(2.0*(x*y+w*z),1.0-2.0*(x*x+z*z));
    }
  }


// Obtain roll, yaw, pitch
void FXQuatd::getRollYawPitch(FXdouble& roll,FXdouble& yaw,FXdouble& pitch) const {
  register FXdouble s=2.0*(x*y+w*z);
  if(s<1.0){
    if(-1.0<s){
      roll=atan2(-2.0*(y*z-w*x),1.0-2.0*(x*x+z*z));
      yaw=asin(s);
      pitch=atan2(-2.0*(x*z-w*y),1.0-2.0*(y*y+z*z));
      }
    else{
      roll=0.0;
      yaw=-1.57079632679489661923;
      pitch=-atan2(2.0*(y*z+w*x),1.0-2.0*(x*x+y*y));
      }
    }
  else{
    roll=0.0;
    yaw=1.57079632679489661923;
    pitch=atan2(2.0*(y*z+w*x),1.0-2.0*(x*x+y*y));
    }
  }


// Obtain pitch, roll, yaw
void FXQuatd::getPitchRollYaw(FXdouble& pitch,FXdouble& roll,FXdouble& yaw) const {
  register FXdouble s=2.0*(y*z+w*x);
  if(s<1.0){
    if(-1.0<s){
      pitch=atan2(-2.0*(x*z-w*y),1.0-2.0*(x*x+y*y));
      roll=asin(s);
      yaw=atan2(-2.0*(x*y-w*z),1.0-2.0*(x*x+z*z));
      }
    else{
      pitch=0.0;
      roll=-1.57079632679489661923;
      yaw=-atan2(2.0*(x*z+w*y),1.0-2.0*(y*y+z*z));
      }
    }
  else{
    pitch=0.0;
    roll=1.57079632679489661923;
    yaw=atan2(2.0*(x*z+w*y),1.0-2.0*(y*y+z*z));
    }
  }


// Obtain pitch, yaw, roll
void FXQuatd::getPitchYawRoll(FXdouble& pitch,FXdouble& yaw,FXdouble& roll) const {
  register FXdouble s=-2.0*(x*y-w*z);
  if(s<1.0){
    if(-1.0<s){
      pitch=atan2(2.0*(x*z+w*y),1.0-2.0*(y*y+z*z));
      yaw=asin(s);
      roll=atan2(2.0*(y*z+w*x),1.0-2.0*(x*x+z*z));
      }
    else{
      pitch=0.0;
      yaw=-1.57079632679489661923;
      roll=-atan2(-2.0*(x*z-w*y),1.0-2.0*(x*x+y*y));
      }
    }
  else{
    pitch=0.0;
    yaw=1.57079632679489661923;
    roll=atan2(-2.0*(x*z-w*y),1.0-2.0*(x*x+y*y));
    }
  }


// Obtain yaw, roll, pitch
void FXQuatd::getYawRollPitch(FXdouble& yaw,FXdouble& roll,FXdouble& pitch) const {
  register FXdouble s=-2.0*(y*z-w*x);
  if(s<1.0){
    if(-1.0<s){
      yaw=atan2(2.0*(x*y+w*z),1.0-2.0*(x*x+z*z));
      roll=asin(s);
      pitch=atan2(2.0*(x*z+w*y),1.0-2.0*(x*x+y*y));
      }
    else{
      yaw=0.0;
      roll=-1.57079632679489661923;
      pitch=-atan2(-2.0*(x*y-w*z),1.0-2.0*(y*y+z*z));
      }
    }
  else{
    yaw=0.0;
    roll=1.57079632679489661923;
    pitch=atan2(-2.0*(x*y-w*z),1.0-2.0*(y*y+z*z));
    }
  }


// Set quaternion from axes
void FXQuatd::setAxes(const FXVec3d& ex,const FXVec3d& ey,const FXVec3d& ez){
  register FXdouble trace=ex.x+ey.y+ez.z;
  register FXdouble scale;
  if(trace>0.0){
    scale=sqrt(1.0+trace);
    w=0.5*scale;
    scale=0.5/scale;
    x=(ey.z-ez.y)*scale;
    y=(ez.x-ex.z)*scale;
    z=(ex.y-ey.x)*scale;
    }
  else if(ex.x>ey.y && ex.x>ez.z){
    scale=2.0*sqrt(1.0+ex.x-ey.y-ez.z);
    x=0.25*scale;
    y=(ex.y+ey.x)/scale;
    z=(ex.z+ez.x)/scale;
    w=(ey.z-ez.y)/scale;
    }
  else if(ey.y>ez.z){
    scale=2.0*sqrt(1.0+ey.y-ex.x-ez.z);
    y=0.25*scale;
    x=(ex.y+ey.x)/scale;
    z=(ey.z+ez.y)/scale;
    w=(ez.x-ex.z)/scale;
    }
  else{
    scale=2.0*sqrt(1.0+ez.z-ex.x-ey.y);
    z=0.25*scale;
    x=(ex.z+ez.x)/scale;
    y=(ey.z+ez.y)/scale;
    w=(ex.y-ey.x)/scale;
    }
  }


// Get quaternion axes
void FXQuatd::getAxes(FXVec3d& ex,FXVec3d& ey,FXVec3d& ez) const {
  register FXdouble tx=2.0*x;
  register FXdouble ty=2.0*y;
  register FXdouble tz=2.0*z;
  register FXdouble twx=tx*w;
  register FXdouble twy=ty*w;
  register FXdouble twz=tz*w;
  register FXdouble txx=tx*x;
  register FXdouble txy=ty*x;
  register FXdouble txz=tz*x;
  register FXdouble tyy=ty*y;
  register FXdouble tyz=tz*y;
  register FXdouble tzz=tz*z;
  ex.x=1.0-tyy-tzz;
  ex.y=txy+twz;
  ex.z=txz-twy;
  ey.x=txy-twz;
  ey.y=1.0-txx-tzz;
  ey.z=tyz+twx;
  ez.x=txz+twy;
  ez.y=tyz-twx;
  ez.z=1.0-txx-tyy;
  }


// Obtain local x axis
FXVec3d FXQuatd::getXAxis() const {
  register FXdouble ty=2.0*y;
  register FXdouble tz=2.0*z;
  return FXVec3d(1.0-ty*y-tz*z,ty*x+tz*w,tz*x-ty*w);
  }


// Obtain local y axis
FXVec3d FXQuatd::getYAxis() const {
  register FXdouble tx=2.0*x;
  register FXdouble tz=2.0*z;
  return FXVec3d(tx*y-tz*w,1.0-tx*x-tz*z,tz*y+tx*w);
  }


// Obtain local z axis
FXVec3d FXQuatd::getZAxis() const {
  register FXdouble tx=2.0*x;
  register FXdouble ty=2.0*y;
  return FXVec3d(tx*z+ty*w,ty*z-tx*w,1.0-tx*x-ty*y);
  }


// Exponentiate unit quaternion
// Given q = theta*(x*i+y*j+z*k), where length of (x,y,z) is 1,
// then exp(q) = sin(theta)*(x*i+y*j+z*k)+cos(theta).
FXQuatd FXQuatd::exp() const {
  register FXdouble theta=sqrt(x*x+y*y+z*z);
  register FXdouble scale;
  FXQuatd result(x,y,z,cos(theta));
  if(theta>0.000001){
    scale=sin(theta)/theta;
    result.x*=scale;
    result.y*=scale;
    result.z*=scale;
    }
  return result;
  }


// Take logarithm of unit quaternion
// Given q = sin(theta)*(x*i+y*j+z*k)+cos(theta), length of (x,y,z) is 1,
// then log(q) = theta*(x*i+y*j+z*k).
FXQuatd FXQuatd::log() const {
  register FXdouble scale=sqrt(x*x+y*y+z*z);
  register FXdouble theta=atan2(scale,w);
  FXQuatd result(x,y,z,0.0);
  if(scale>0.0){
    scale=theta/scale;
    result.x*=scale;
    result.y*=scale;
    result.z*=scale;
    }
  return result;
  }


// Invert quaternion
FXQuatd FXQuatd::invert() const {
  register FXdouble n=x*x+y*y+z*z+w*w;
  return FXQuatd(-x/n,-y/n,-z/n,w/n);
  }


// Invert unit quaternion
FXQuatd FXQuatd::unitinvert() const {
  return FXQuatd(-x,-y,-z,w);
  }


// Conjugate quaternion
FXQuatd FXQuatd::conj() const {
  return FXQuatd(-x,-y,-z,w);
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
FXQuatd& FXQuatd::arc(const FXVec3d& f,const FXVec3d& t){
  register FXdouble dot=f.x*t.x+f.y*t.y+f.z*t.z,div;
  if(dot> 0.999999){            // Unit quaternion
    x= 0.0;
    y= 0.0;
    z= 0.0;
    w= 1.0;
    }
  else if(dot<-0.999999){       // 180 quaternion
    if(fabs(f.z)<fabs(f.x) && fabs(f.z)<fabs(f.y)){     // x, y largest magnitude
      x= f.x*f.z-f.z*f.y;
      y= f.z*f.x+f.y*f.z;
      z=-f.y*f.y-f.x*f.x;
      }
    else if(fabs(f.y)<fabs(f.x)){                       // y, z largest magnitude
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
    div=sqrt(dot);
    x/=div;
    y/=div;
    z/=div;
    w=0.0;
    }
  else{
    div=sqrt((dot+1.0)*2.0);
    x=(f.y*t.z-f.z*t.y)/div;
    y=(f.z*t.x-f.x*t.z)/div;
    z=(f.x*t.y-f.y*t.x)/div;
    w=div*0.5;
    }
  return *this;
  }


// Spherical lerp
FXQuatd& FXQuatd::lerp(const FXQuatd& u,const FXQuatd& v,FXdouble f){
  register FXdouble alpha,beta,theta,sin_t,cos_t;
  register FXint flip=0;
  cos_t = u.x*v.x+u.y*v.y+u.z*v.z+u.w*v.w;
  if(cos_t<0.0){ cos_t = -cos_t; flip=1; }
  if((1.0-cos_t)<0.000001){
    beta = 1.0-f;
    alpha = f;
    }
  else{
    theta = acos(cos_t);
    sin_t = sin(theta);
    beta = sin(theta-f*theta)/sin_t;
    alpha = sin(f*theta)/sin_t;
    }
  if(flip) alpha = -alpha;
  x=beta*u.x+alpha*v.x;
  y=beta*u.y+alpha*v.y;
  z=beta*u.z+alpha*v.z;
  w=beta*u.w+alpha*v.w;
  return *this;
  }


// Multiply quaternions
FXQuatd FXQuatd::operator*(const FXQuatd& q) const {
  return FXQuatd(w*q.x+x*q.w+y*q.z-z*q.y, w*q.y+y*q.w+z*q.x-x*q.z, w*q.z+z*q.w+x*q.y-y*q.x, w*q.w-x*q.x-y*q.y-z*q.z);
  }


// Rotation of a vector by a quaternion; this is defined as q.v.q*
// where q* is the conjugate of q.
FXVec3d FXQuatd::operator*(const FXVec3d& v) const {
  return v*FXMat3d(*this);
  }


}

