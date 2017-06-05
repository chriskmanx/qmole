/********************************************************************************
*                                                                               *
*           S i n g l e - P r e c i s i o n    S p h e r e    C l a s s         *
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
* $Id: FXSpheref.cpp,v 1.20.2.2 2006/04/05 15:21:08 fox Exp $                       *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXVec2f.h"
#include "FXVec3f.h"
#include "FXVec4f.h"
#include "FXSpheref.h"
#include "FXRangef.h"

/*
  Notes:
  - Negative radius represents empty bounding sphere.
*/


using namespace FX;

/**************************  S p h e r e   C l a s s   *************************/

namespace FX {


inline FXfloat sqrf(FXfloat x){ return x*x; }


// Initialize from bounding box
FXSpheref::FXSpheref(const FXRangef& bounds):center(bounds.center()),radius(bounds.diameter()*0.5f){
  }


// Test if sphere contains point x,y,z
bool FXSpheref::contains(FXfloat x,FXfloat y,FXfloat z) const {
  return 0.0f<=radius && sqrf(center.x-x)+sqrf(center.y-y)+sqrf(center.z-z)<=sqrf(radius);
  }


// Test if sphere contains point p
bool FXSpheref::contains(const FXVec3f& p) const {
  return contains(p.x,p.y,p.z);
  }


// Test if sphere contains another box
bool FXSpheref::contains(const FXRangef& box) const {
  if(box.lower.x<=box.upper.x && box.lower.y<=box.upper.y && box.lower.z<=box.upper.z){
    return contains(box.corner(0)) && contains(box.corner(1)) && contains(box.corner(2)) && contains(box.corner(3)) && contains(box.corner(4)) && contains(box.corner(5)) && contains(box.corner(6)) && contains(box.corner(7));
    }
  return FALSE;
  }


// Test if sphere properly contains another sphere
bool FXSpheref::contains(const FXSpheref& sphere) const {
  if(0.0f<=sphere.radius && sphere.radius<=radius){
    register FXfloat dx=center.x-sphere.center.x;
    register FXfloat dy=center.y-sphere.center.y;
    register FXfloat dz=center.z-sphere.center.z;
    return sphere.radius+sqrtf(dx*dx+dy*dy+dz*dz)<=radius;
    }
  return FALSE;
  }


// Include point
FXSpheref& FXSpheref::include(FXfloat x,FXfloat y,FXfloat z){
  register FXfloat dx,dy,dz,dist,delta,newradius;
  if(0.0f<=radius){
    dx=x-center.x;
    dy=y-center.y;
    dz=z-center.z;
    dist=sqrtf(dx*dx+dy*dy+dz*dz);
    if(radius<dist){
      newradius=0.5f*(radius+dist);
      delta=(newradius-radius);
      center.x+=delta*dx/dist;
      center.y+=delta*dy/dist;
      center.z+=delta*dz/dist;
      radius=newradius;
      }
    return *this;
    }
  center.x=x;
  center.y=y;
  center.z=z;
  radius=0.0f;
  return *this;
  }


// Include point
FXSpheref& FXSpheref::include(const FXVec3f& p){
  return include(p.x,p.y,p.z);
  }


// Expand radius to include point
FXSpheref& FXSpheref::includeInRadius(FXfloat x,FXfloat y,FXfloat z){
  register FXfloat dx,dy,dz,dist;
  if(0.0f<=radius){
    dx=x-center.x;
    dy=y-center.y;
    dz=z-center.z;
    dist=sqrtf(dx*dx+dy*dy+dz*dz);
    if(radius<dist) radius=dist;
    return *this;
    }
  center.x=x;
  center.y=y;
  center.z=z;
  radius=0.0f;
  return *this;
  }


// Expand radius to include point
FXSpheref& FXSpheref::includeInRadius(const FXVec3f& p){
  return includeInRadius(p.x,p.y,p.z);
  }


// Include given range into this one
FXSpheref& FXSpheref::include(const FXRangef& box){
  if(box.lower.x<=box.upper.x && box.lower.y<=box.upper.y && box.lower.z<=box.upper.z){
    if(0.0f<=radius){
      include(box.corner(0));
      include(box.corner(1));
      include(box.corner(2));
      include(box.corner(3));
      include(box.corner(4));
      include(box.corner(5));
      include(box.corner(6));
      include(box.corner(7));
      return *this;
      }
    center=box.center();
    radius=box.radius();
    }
  return *this;
  }


// Expand radius to include box
FXSpheref& FXSpheref::includeInRadius(const FXRangef& box){
  if(box.lower.x<=box.upper.x && box.lower.y<=box.upper.y && box.lower.z<=box.upper.z){
    if(0.0f<=radius){
      includeInRadius(box.corner(0));
      includeInRadius(box.corner(1));
      includeInRadius(box.corner(2));
      includeInRadius(box.corner(3));
      includeInRadius(box.corner(4));
      includeInRadius(box.corner(5));
      includeInRadius(box.corner(6));
      includeInRadius(box.corner(7));
      return *this;
      }
    center=box.center();
    radius=box.radius();
    }
  return *this;
  }


// Include given sphere into this one
FXSpheref& FXSpheref::include(const FXSpheref& sphere){
  register FXfloat dx,dy,dz,dist,delta,newradius;
  if(0.0f<=sphere.radius){
    if(0.0f<=radius){
      dx=sphere.center.x-center.x;
      dy=sphere.center.y-center.y;
      dz=sphere.center.z-center.z;
      dist=sqrtf(dx*dx+dy*dy+dz*dz);
      if(sphere.radius<dist+radius){
        if(radius<dist+sphere.radius){
          newradius=0.5f*(radius+dist+sphere.radius);
          delta=(newradius-radius);
          center.x+=delta*dx/dist;
          center.y+=delta*dy/dist;
          center.z+=delta*dz/dist;
          radius=newradius;
          }
        return *this;
        }
      }
    center=sphere.center;
    radius=sphere.radius;
    }
  return *this;
  }


// Expand radius to include sphere
FXSpheref& FXSpheref::includeInRadius(const FXSpheref& sphere){
  register FXfloat dx,dy,dz,dist;
  if(0.0f<=sphere.radius){
    if(0.0f<=radius){
      dx=sphere.center.x-center.x;
      dy=sphere.center.y-center.y;
      dz=sphere.center.z-center.z;
      dist=sqrtf(dx*dx+dy*dy+dz*dz)+sphere.radius;
      if(radius<dist) radius=dist;
      return *this;
      }
    center=sphere.center;
    radius=sphere.radius;
    }
  return *this;
  }


// Intersect sphere with normalized plane ax+by+cz+w; returns -1,0,+1
FXint FXSpheref::intersect(const FXVec4f& plane) const {
  register FXfloat dist=plane.distance(center);

  // Lower point on positive side of plane
  if(dist>=radius) return 1;

  // Upper point on negative side of plane
  if(dist<=-radius) return -1;

  // Overlap
  return 0;
  }


// Intersect sphere with ray u-v
bool FXSpheref::intersect(const FXVec3f& u,const FXVec3f& v) const {
  if(0.0f<=radius){
    FXfloat rr=radius*radius;
    FXVec3f uc=center-u;        // Vector from u to center
    FXfloat dd=uc.length2();
    if(dd>rr){                  // Ray start point outside sphere
      FXVec3f uv=v-u;           // Vector from u to v
      FXfloat hh=uc*uv;         // If hh<0, uv points away from center
      if(0.0f<=hh){             // Not away from sphere
        FXfloat kk=uv.length2();
        FXfloat disc=hh*hh-kk*(dd-rr);  // FIXME this needs to be checked again!
        if(disc<=0.0) return FALSE;
        return TRUE;
        }
      return FALSE;
      }
    return TRUE;
    }
  return FALSE;
  }


// Test if box overlaps with sphere; algorithm due to Arvo (GEMS I)
bool overlap(const FXSpheref& a,const FXRangef& b){
  if(0.0f<=a.radius){
    register FXfloat dd=0.0f;

    if(a.center.x<b.lower.x)
      dd+=sqrf(a.center.x-b.lower.x);
    else if(a.center.x>b.upper.x)
      dd+=sqrf(a.center.x-b.upper.x);

    if(a.center.y<b.lower.y)
      dd+=sqrf(a.center.y-b.lower.y);
    else if(a.center.y>b.upper.y)
      dd+=sqrf(a.center.y-b.upper.y);

    if(a.center.z<b.lower.z)
      dd+=sqrf(a.center.z-b.lower.z);
    else if(a.center.z>b.upper.z)
      dd+=sqrf(a.center.z-b.upper.z);

    return dd<=a.radius*a.radius;
    }
  return FALSE;
  }


// Test if box overlaps with sphere; algorithm due to Arvo (GEMS I)
bool overlap(const FXRangef& a,const FXSpheref& b){
  return overlap(b,a);
  }


// Test if spheres overlap
bool overlap(const FXSpheref& a,const FXSpheref& b){
  if(0.0f<=a.radius && 0.0f<=b.radius){
    register FXfloat dx=a.center.x-b.center.x;
    register FXfloat dy=a.center.y-b.center.y;
    register FXfloat dz=a.center.z-b.center.z;
    return (dx*dx+dy*dy+dz*dz)<sqrf(a.radius+b.radius);
    }
  return FALSE;
  }


// Saving
FXStream& operator<<(FXStream& store,const FXSpheref& sphere){
  store << sphere.center << sphere.radius;
  return store;
  }


// Loading
FXStream& operator>>(FXStream& store,FXSpheref& sphere){
  store >> sphere.center >> sphere.radius;
  return store;
  }

}
