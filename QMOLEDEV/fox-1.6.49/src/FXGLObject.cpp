/********************************************************************************
*                                                                               *
*                           O p e n G L   O b j e c t                           *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXGLObject.cpp,v 1.37.2.3 2006/07/28 00:56:30 fox Exp $                      *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXVec2f.h"
#include "FXVec3f.h"
#include "FXVec4f.h"
#include "FXQuatf.h"
#include "FXMat4f.h"
#include "FXRangef.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXObjectList.h"
#include "FXApp.h"
#include "FXGLViewer.h"
#include "FXGLObject.h"
#include "FXGLVisual.h"
#include "FXFont.h"

// GLU versions prior to 1.1 have GLUquadric
#if !defined(GLU_VERSION_1_1) && !defined(GLU_VERSION_1_2) && !defined(GLU_VERSION_1_3)
#define GLUquadricObj GLUquadric
#endif

/*
  Notes:
  - Leaf objects don't push any names!
  - Group objects should do focus traversal.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXGLObject,FXObject,NULL,0)


// Get bounding box
void FXGLObject::bounds(FXRangef& box){
  box.upper.x=box.lower.x=0.0f;
  box.upper.y=box.lower.y=0.0f;
  box.upper.z=box.lower.z=0.0f;
  }


// Draw the GL scene
void FXGLObject::draw(FXGLViewer*){ }


// Hit objects
void FXGLObject::hit(FXGLViewer* viewer){ draw(viewer); }


// Copy
FXGLObject* FXGLObject::copy(){ return new FXGLObject(*this); }


// Identify object by its path
FXGLObject* FXGLObject::identify(FXuint*){ return this; }


// Return true if it can be dragged
FXbool FXGLObject::canDrag() const { return FALSE; }


// Return true if OK to delete object
FXbool FXGLObject::canDelete() const { return FALSE; }


// Drag the object
FXbool FXGLObject::drag(FXGLViewer*,FXint,FXint,FXint,FXint){ return FALSE; }



/*******************************************************************************/



// Object implementation
FXIMPLEMENT(FXGLGroup,FXGLObject,NULL,0)


// Get bounding box
void FXGLGroup::bounds(FXRangef& box){
  register FXint i;
  FXRangef b;
  box.lower.x=box.lower.y=box.lower.z=0.0f;
  box.upper.x=box.upper.y=box.upper.z=0.0f;
  if(0<list.no()){
    box.lower.x=box.lower.y=box.lower.z= FLT_MAX;
    box.upper.x=box.upper.y=box.upper.z=-FLT_MAX;
    for(i=0; i<list.no(); i++){
      list[i]->bounds(b);
      box.include(b);
      }
    }
  }


// Draw
void FXGLGroup::draw(FXGLViewer* viewer){
  for(FXint i=0; i<list.no(); i++) list[i]->draw(viewer);
  }


// Draw for hit
void FXGLGroup::hit(FXGLViewer* viewer){
#ifdef HAVE_GL_H
  glPushName(0xffffffff);
  for(FXint i=0; i<list.no(); i++){
    glLoadName(i);
    list[i]->hit(viewer);
    }
  glPopName();
#endif
  }


// Copy
FXGLObject* FXGLGroup::copy(){
  return new FXGLGroup(*this);
  }



// Identify object by its path
FXGLObject* FXGLGroup::identify(FXuint* path){
  FXASSERT(path);
  FXASSERT((FXint)path[0]<list.no());
  return list[path[0]]->identify(path+1);
  }


// Return true if it can be dragged
FXbool FXGLGroup::canDrag() const { return TRUE; }


// Drag group object
FXbool FXGLGroup::drag(FXGLViewer* viewer,FXint fx,FXint fy,FXint tx,FXint ty){
  for(FXint i=0; i<list.no(); i++){
    list[i]->drag(viewer,fx,fy,tx,ty);
    }
  return TRUE;
  }


// Save object to stream
void FXGLGroup::save(FXStream& store) const {
  FXGLObject::save(store);
  list.save(store);
  }


// Load object from stream
void FXGLGroup::load(FXStream& store){
  FXGLObject::load(store);
  list.load(store);
  }


// Delete members of the group
FXGLGroup::~FXGLGroup(){
  for(FXint i=0; i<list.no(); i++) delete list[i];
  }


/*******************************************************************************/

#define HANDLE_SIZE 4.0


// Object implementation
FXIMPLEMENT(FXGLPoint,FXGLObject,NULL,0)


// Create point
FXGLPoint::FXGLPoint():pos(0.0f,0.0f,0.0f){
  }

// Copy constructor
FXGLPoint::FXGLPoint(const FXGLPoint& orig):FXGLObject(orig),pos(orig.pos){
  }

// Create initialized point
FXGLPoint::FXGLPoint(FXfloat x,FXfloat y,FXfloat z):pos(x,y,z){
  }


// Get bounding box
void FXGLPoint::bounds(FXRangef& box){
  box.upper.x=box.lower.x=pos.x;
  box.upper.y=box.lower.y=pos.y;
  box.upper.z=box.lower.z=pos.z;
  }


// Draw
void FXGLPoint::draw(FXGLViewer* ){
#ifdef HAVE_GL_H
  glColor3f(0.0,0.0,1.0);
  glPointSize(HANDLE_SIZE);
  glBegin(GL_POINTS);
  glVertex3fv(pos);
  glEnd();
#endif
  }


// Draw for hit
void FXGLPoint::hit(FXGLViewer* ){
#ifdef HAVE_GL_H
  glBegin(GL_POINTS);
  glVertex3fv(pos);
  glEnd();
#endif
  }


// Copy
FXGLObject* FXGLPoint::copy(){
  return new FXGLPoint(*this);
  }

// Save object to stream
void FXGLPoint::save(FXStream& store) const {
  FXGLObject::save(store);
  store << pos;
  }


// Load object from stream
void FXGLPoint::load(FXStream& store){
  FXGLObject::load(store);
  store >> pos;
  }


/*******************************************************************************/

// Object implementation
FXIMPLEMENT(FXGLLine,FXGLObject,NULL,0)


// Create line
FXGLLine::FXGLLine():fm(-0.5f,0.0f,0.0f),to(0.5f,0.0f,0.0f){
  }


// Copy constructor
FXGLLine::FXGLLine(const FXGLLine& orig):FXGLObject(orig),fm(orig.fm),to(orig.to){
  }


// Create inittialized line
FXGLLine::FXGLLine(FXfloat fx,FXfloat fy,FXfloat fz,FXfloat tx,FXfloat ty,FXfloat tz):fm(fx,fy,fz),to(tx,ty,tz){
  }


// Get bounding box
void FXGLLine::bounds(FXRangef& box){
  FXMINMAX(box.lower.x,box.upper.x,fm.pos.x,to.pos.x);
  FXMINMAX(box.lower.y,box.upper.y,fm.pos.y,to.pos.y);
  FXMINMAX(box.lower.z,box.upper.z,fm.pos.z,to.pos.z);
  }


// Draw
void FXGLLine::draw(FXGLViewer*){
#ifdef HAVE_GL_H
  glColor3f(1.0,0.0,0.0);
  glPointSize(HANDLE_SIZE);
  glBegin(GL_LINES);
  glVertex3fv(fm.pos);
  glVertex3fv(to.pos);
  glEnd();
#endif
  }


// Draw for hit
void FXGLLine::hit(FXGLViewer* ){
#ifdef HAVE_GL_H
  glBegin(GL_LINES);
  glVertex3fv(fm.pos);
  glVertex3fv(to.pos);
  glEnd();
#endif
  }


// Copy
FXGLObject* FXGLLine::copy(){
  return new FXGLLine(*this);
  }


// Save object to stream
void FXGLLine::save(FXStream& store) const {
  FXGLObject::save(store);
  fm.save(store);
  to.save(store);
  }


// Load object from stream
void FXGLLine::load(FXStream& store){
  FXGLObject::load(store);
  fm.load(store);
  to.load(store);
  }

}
