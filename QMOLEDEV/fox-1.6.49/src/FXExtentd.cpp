/********************************************************************************
*                                                                               *
*           D o u b l e - P r e c i s i o n    E x t e n t    C l a s s         *
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
* $Id: FXExtentd.cpp,v 1.3 2006/01/22 17:58:25 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXVec2d.h"
#include "FXExtentd.h"

/*
  Notes:
*/


using namespace FX;

/**************************  E x t e n t   C l a s s   *************************/

namespace FX {

// Longest side
FXdouble FXExtentd::longest() const {
  register FXdouble x=upper.x-lower.x;
  register FXdouble y=upper.y-lower.y;
  return FXMAX(x,y);
  }


// Shortest side
FXdouble FXExtentd::shortest() const {
  register FXdouble x=upper.x-lower.x;
  register FXdouble y=upper.y-lower.y;
  return FXMIN(x,y);
  }


// Length of diagonal
FXdouble FXExtentd::diameter() const {
  register FXdouble x=upper.x-lower.x;
  register FXdouble y=upper.y-lower.y;
  return sqrt(x*x+y*y);
  }


// Get radius of box
FXdouble FXExtentd::radius() const {
  return diameter()*0.5;
  }


// Get diagonal of box
FXVec2d FXExtentd::diagonal() const {
  return upper-lower;
  }


// Get center of box
FXVec2d FXExtentd::center() const {
  return 0.5*(upper+lower);
  }


// Test if empty
bool FXExtentd::empty() const {
  return upper.x<lower.x || upper.y<lower.y;
  }

// Test if box contains point
bool FXExtentd::contains(FXdouble x,FXdouble y) const {
  return lower.x<=x && x<=upper.x && lower.y<=y && y<=upper.y;
  }


// Test if box contains point p
bool FXExtentd::contains(const FXVec2d& p) const {
  return lower.x<=p.x && p.x<=upper.x && lower.y<=p.y && p.y<=upper.y;
  }


// Test if box contains another box
bool FXExtentd::contains(const FXExtentd& ext) const {
  return lower.x<=ext.lower.x && ext.upper.x<=upper.x && lower.y<=ext.lower.y && ext.upper.y<=upper.y;
  }


// Include point into range
FXExtentd& FXExtentd::include(FXdouble x,FXdouble y){
  if(x<lower.x) lower.x=x; if(x>upper.x) upper.x=x;
  if(y<lower.y) lower.y=y; if(y>upper.y) upper.y=y;
  return *this;
  }


// Include point into range
FXExtentd& FXExtentd::include(const FXVec2d& v){
  return include(v.x,v.y);
  }


// Include given box into box's range
FXExtentd& FXExtentd::include(const FXExtentd& ext){
  if(ext.lower.x<lower.x) lower.x=ext.lower.x; if(ext.upper.x>upper.x) upper.x=ext.upper.x;
  if(ext.lower.y<lower.y) lower.y=ext.lower.y; if(ext.upper.y>upper.y) upper.y=ext.upper.y;
  return *this;
  }


// Test if overlap
bool overlap(const FXExtentd& a,const FXExtentd& b){
  return a.upper.x>=b.lower.x && a.lower.x<=b.upper.x && a.upper.y>=b.lower.y && a.lower.y<=b.upper.y;
  }


// Union of two boxes
FXExtentd unite(const FXExtentd& a,const FXExtentd& b){
  return FXExtentd(lo(a.lower,b.lower),hi(a.upper,b.upper));
  }


// Intersection of two boxes
FXExtentd intersect(const FXExtentd& a,const FXExtentd& b){
  return FXExtentd(hi(a.lower,b.lower),lo(a.upper,b.upper));
  }


// Saving
FXStream& operator<<(FXStream& store,const FXExtentd& ext){
  store << ext.lower.x << ext.upper.x;
  store << ext.lower.y << ext.upper.y;
  return store;
  }


// Loading
FXStream& operator>>(FXStream& store,FXExtentd& ext){
  store >> ext.lower.x >> ext.upper.x;
  store >> ext.lower.y >> ext.upper.y;
  return store;
  }

}

