/********************************************************************************
*                                                                               *
*           D o u b l e - P r e c i s i o n    R a n g e    C l a s s           *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXRanged.cpp,v 1.15 2006/01/22 17:58:38 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXVec2d.h"
#include "FXVec3d.h"
#include "FXVec4d.h"
#include "FXSphered.h"
#include "FXRanged.h"

/*
  Notes:
  - Serializes in the same order old FXRange.
*/


using namespace FX;

/**************************  R a n g e   C l a s s   *************************/

namespace FX {

// Initialize from bounding sphere
FXRanged::FXRanged(const FXSphered& sphere):
  lower(sphere.center.x-sphere.radius,sphere.center.y-sphere.radius,sphere.center.z-sphere.radius),
  upper(sphere.center.x+sphere.radius,sphere.center.y+sphere.radius,sphere.center.z+sphere.radius){
  }


// Longest side
FXdouble FXRanged::longest() const {
  register FXdouble x=upper.x-lower.x;
  register FXdouble y=upper.y-lower.y;
  register FXdouble z=upper.z-lower.z;
  return FXMAX3(x,y,z);
  }


// Shortest side
FXdouble FXRanged::shortest() const {
  register FXdouble x=upper.x-lower.x;
  register FXdouble y=upper.y-lower.y;
  register FXdouble z=upper.z-lower.z;
  return FXMIN3(x,y,z);
  }


// Length of diagonal
FXdouble FXRanged::diameter() const {
  register FXdouble x=upper.x-lower.x;
  register FXdouble y=upper.y-lower.y;
  register FXdouble z=upper.z-lower.z;
  return sqrt(x*x+y*y+z*z);
  }


// Get radius of box
FXdouble FXRanged::radius() const {
  return diameter()*0.5;
  }


// Get diagonal of box
FXVec3d FXRanged::diagonal() const {
  return upper-lower;
  }


// Get center of box
FXVec3d FXRanged::center() const {
  return 0.5*(upper+lower);
  }


// Test if empty
bool FXRanged::empty() const {
  return upper.x<lower.x || upper.y<lower.y || upper.z<lower.z;
  }


// Test if box contains point x,y,z
bool FXRanged::contains(FXdouble x,FXdouble y,FXdouble z) const {
  return lower.x<=x && x<=upper.x && lower.y<=y && y<=upper.y && lower.z<=z && z<=upper.z;
  }


// Test if box contains point p
bool FXRanged::contains(const FXVec3d& p) const {
  return lower.x<=p.x && p.x<=upper.x && lower.y<=p.y && p.y<=upper.y && lower.z<=p.z && p.z<=upper.z;
  }


// Test if box contains another box
bool FXRanged::contains(const FXRanged& bounds) const {
  return lower.x<=bounds.lower.x && bounds.upper.x<=upper.x && lower.y<=bounds.lower.y && bounds.upper.y<=upper.y && lower.z<=bounds.lower.z && bounds.upper.z<=upper.z;
  }


// Test if box contains sphere
bool FXRanged::contains(const FXSphered& sphere) const {
  return lower.x<=sphere.center.x-sphere.radius && sphere.center.x+sphere.radius<=upper.x && lower.y<=sphere.center.y-sphere.radius && sphere.center.y+sphere.radius<=upper.y && lower.z<=sphere.center.z-sphere.radius && sphere.center.z+sphere.radius<=upper.z;
  }


// Include point into range
FXRanged& FXRanged::include(FXdouble x,FXdouble y,FXdouble z){
  if(x<lower.x) lower.x=x; if(x>upper.x) upper.x=x;
  if(y<lower.y) lower.y=y; if(y>upper.y) upper.y=y;
  if(z<lower.z) lower.z=z; if(z>upper.z) upper.z=z;
  return *this;
  }


// Include point into range
FXRanged& FXRanged::include(const FXVec3d& v){
  return include(v.x,v.y,v.z);
  }


// Include given box into box's range
FXRanged& FXRanged::include(const FXRanged& box){
  if(box.lower.x<lower.x) lower.x=box.lower.x; if(box.upper.x>upper.x) upper.x=box.upper.x;
  if(box.lower.y<lower.y) lower.y=box.lower.y; if(box.upper.y>upper.y) upper.y=box.upper.y;
  if(box.lower.z<lower.z) lower.z=box.lower.z; if(box.upper.z>upper.z) upper.z=box.upper.z;
  return *this;
  }


// Include given sphere into this box
FXRanged& FXRanged::include(const FXSphered& sphere){
  FXVec3d lo(sphere.center.x-sphere.radius,sphere.center.y-sphere.radius,sphere.center.z-sphere.radius);
  FXVec3d hi(sphere.center.x+sphere.radius,sphere.center.y+sphere.radius,sphere.center.z+sphere.radius);
  if(lo.x<lower.x) lower.x=lo.x; if(hi.x>upper.x) upper.x=hi.x;
  if(lo.y<lower.y) lower.y=lo.y; if(hi.y>upper.y) upper.y=hi.y;
  if(lo.z<lower.z) lower.z=lo.z; if(hi.z>upper.z) upper.z=hi.z;
  return *this;
  }


// Test if overlap
bool overlap(const FXRanged& a,const FXRanged& b){
  return a.upper.x>=b.lower.x && a.lower.x<=b.upper.x && a.upper.y>=b.lower.y && a.lower.y<=b.upper.y && a.upper.z>=b.lower.z && a.lower.z<=b.upper.z;
  }


// Union of two boxes
FXRanged unite(const FXRanged& a,const FXRanged& b){
  return FXRanged(lo(a.lower,b.lower),hi(a.upper,b.upper));
  }


// Intersection of two boxes
FXRanged intersect(const FXRanged& a,const FXRanged& b){
  return FXRanged(hi(a.lower,b.lower),lo(a.upper,b.upper));
  }


// Intersect box with normalized plane ax+by+cz+w; returns -1,0,+1
FXint FXRanged::intersect(const FXVec4d& plane) const {
  FXVec3d lo;
  FXVec3d hi;

  // Diagonal
  if(plane.x>0.0){
    lo.x=lower.x;
    hi.x=upper.x;
    }
  else{
    lo.x=upper.x;
    hi.x=lower.x;
    }

  if(plane.y>0.0){
    lo.y=lower.y;
    hi.y=upper.y;
    }
  else{
    lo.y=upper.y;
    hi.y=lower.y;
    }

  if(plane.z>0.0){
    lo.z=lower.z;
    hi.z=upper.z;
    }
  else{
    lo.z=upper.z;
    hi.z=lower.z;
    }

  // Lower point on positive side of plane
  if(plane.x*lo.x+plane.y*lo.y+plane.z*lo.z+plane.w>=0.0) return 1;

  // Upper point on negative side of plane
  if(plane.x*hi.x+plane.y*hi.y+plane.z*hi.z+plane.w<=0.0) return -1;

  // Overlap
  return 0;
  }


// Intersect box with ray u-v
bool FXRanged::intersect(const FXVec3d& u,const FXVec3d& v){
  register FXdouble d,ni,fi,t;
  register FXdouble f= DBL_MAX;
  register FXdouble n=-DBL_MAX;
  d = v.x-u.x;
  if(d==0.0){
    if((upper.x<u.x) || (u.x<lower.x)) return false;
    }
  else{
    ni = (lower.x-u.x)/d;
    fi = (upper.x-u.x)/d;
    if(ni>fi) FXSWAP(ni,fi,t);
    if(ni>n) n=ni;
    if(fi<f) f=fi;
    if(n>f) return false;
    }
  d = v.y-u.y;
  if(d==0.0){
    if((upper.y<u.y) || (u.y<lower.y)) return false;
    }
  else{
    ni = (lower.y-u.y)/d;
    fi = (upper.y-u.y)/d;
    if(ni>fi) FXSWAP(ni,fi,t);
    if(ni>n) n=ni;
    if(fi<f) f=fi;
    if(n>f) return false;
    }
  d = v.z-u.z;
  if(d==0.0){
    if((upper.z<u.z) || (u.z<lower.z)) return false;
    }
  else{
    ni = (lower.z-u.z)/d;
    fi = (upper.z-u.z)/d;
    if(ni>fi) FXSWAP(ni,fi,t);
    if(ni>n) n=ni;
    if(fi<f) f=fi;
    if(n>f) return false;
    }
  return true;
  }


// Saving
FXStream& operator<<(FXStream& store,const FXRanged& bounds){
  store << bounds.lower.x << bounds.upper.x;
  store << bounds.lower.y << bounds.upper.y;
  store << bounds.lower.z << bounds.upper.z;
  return store;
  }


// Loading
FXStream& operator>>(FXStream& store,FXRanged& bounds){
  store >> bounds.lower.x >> bounds.upper.x;
  store >> bounds.lower.y >> bounds.upper.y;
  store >> bounds.lower.z >> bounds.upper.z;
  return store;
  }

}

