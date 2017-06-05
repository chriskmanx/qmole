/********************************************************************************
*                                                                               *
*                      O p e n G L   C y l i n d e r   O b j e c t              *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* Contributed by: Angel-Ventura Mendo Gomez <ventura@labri.u-bordeaux.fr>       *
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
* $Id: FXGLCylinder.cpp,v 1.27 2006/01/22 17:58:28 fox Exp $                    *
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
#include "FXGLCylinder.h"


// GLU versions prior to 1.1 have GLUquadric
#if !defined(GLU_VERSION_1_1) && !defined(GLU_VERSION_1_2) && !defined(GLU_VERSION_1_3)
#define GLUquadricObj GLUquadric
#endif


// Cylinder fidelity
#define FXGLCYLINDER_SLICES_NUMBER		20
#define FXGLCYLINDER_STACKS_NUMBER		20
#define FXGLCYLINDER_LOOPS			4

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLCylinder,FXGLShape,NULL,0)


// Create cylinder
FXGLCylinder::FXGLCylinder():height(1.0f),radius(1.0f){
  FXTRACE((100,"FXGLCylinder::FXGLCylinder\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=0.0f;    range.upper.y=height;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Create initialized cylinder
FXGLCylinder::FXGLCylinder(FXfloat x,FXfloat y,FXfloat z,FXfloat h,FXfloat r):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE),height(h),radius(r){
  FXTRACE((100,"FXGLCylinder::FXGLCylinder\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=0.0f;    range.upper.y=height;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Create initialized cylinder
FXGLCylinder::FXGLCylinder(FXfloat x,FXfloat y,FXfloat z,FXfloat h,FXfloat r,const FXMaterial& mtl):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE,mtl,mtl),height(h),radius(r){
  FXTRACE((100,"FXGLCylinder::FXGLCylinder\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=0.0f;    range.upper.y=height;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Copy constructor
FXGLCylinder::FXGLCylinder(const FXGLCylinder& orig):FXGLShape(orig){
  FXTRACE((100,"FXGLCylinder::FXGLCylinder\n"));
  height=orig.height;
  radius=orig.radius;
  }


// Draw
void FXGLCylinder::drawshape(FXGLViewer*){
#ifdef HAVE_GL_H
  GLUquadricObj* quad=gluNewQuadric();
  gluQuadricDrawStyle(quad,(GLenum)GLU_FILL);
  /*
    gluQuadricNormals(quad,GLU_SMOOTH);
    gluQuadricOrientation(quad,GLU_OUTSIDE);
  */
  glPushMatrix();
  glRotatef(-90.0f,1.0f,0.0f,0.0f);
  gluCylinder(quad,radius,radius,height,FXGLCYLINDER_SLICES_NUMBER,FXGLCYLINDER_STACKS_NUMBER);

  gluQuadricOrientation(quad,(GLenum)GLU_INSIDE);
  gluDisk(quad,0,radius,FXGLCYLINDER_SLICES_NUMBER,FXGLCYLINDER_LOOPS);

  glTranslatef(0.0f,0.0f,height);
  gluQuadricOrientation(quad,(GLenum)GLU_OUTSIDE);
  gluDisk(quad,0,radius,FXGLCYLINDER_SLICES_NUMBER,FXGLCYLINDER_LOOPS);
  glPopMatrix();
  gluDeleteQuadric(quad);
#endif
  }


// Copy this object
FXGLObject* FXGLCylinder::copy(){
  return new FXGLCylinder(*this);
  }


// Save object to stream
void FXGLCylinder::save(FXStream& store) const {
  FXGLShape::save(store);
  store << height << radius;
  }


// Load object from stream
void FXGLCylinder::load(FXStream& store){
  FXGLShape::load(store);
  store >> height >> radius;
  }


// Destroy
FXGLCylinder::~FXGLCylinder(){
  FXTRACE((100,"FXGLCylinder::~FXGLCylinder\n"));
  }

}
