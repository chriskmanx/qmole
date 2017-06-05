/********************************************************************************
*                                                                               *
*                      O p e n G L   S p h e r e   O b j e c t                  *
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
* $Id: FXGLSphere.cpp,v 1.26 2006/01/22 17:58:28 fox Exp $                      *
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
#include "FXGLSphere.h"


// GLU versions prior to 1.1 have GLUquadric
#if !defined(GLU_VERSION_1_1) && !defined(GLU_VERSION_1_2) && !defined(GLU_VERSION_1_3)
#define GLUquadricObj GLUquadric
#endif


// Sphere fidelity
#define SPHERE_SLICES  20
#define SPHERE_STACKS  20

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLSphere,FXGLShape,NULL,0)


// Create sphere
FXGLSphere::FXGLSphere(void):radius(0.5f),slices(SPHERE_SLICES),stacks(SPHERE_STACKS){
  FXTRACE((100,"FXGLSphere::FXGLSphere\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=-radius; range.upper.y=radius;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Create initialized sphere
FXGLSphere::FXGLSphere(FXfloat x,FXfloat y,FXfloat z,FXfloat r):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE),radius(r),slices(SPHERE_SLICES),stacks(SPHERE_STACKS){
  FXTRACE((100,"FXGLSphere::FXGLSphere\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=-radius; range.upper.y=radius;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Create initialized sphere
FXGLSphere::FXGLSphere(FXfloat x,FXfloat y,FXfloat z,FXfloat r,const FXMaterial& mtl):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE,mtl,mtl),radius(r),slices(SPHERE_SLICES),stacks(SPHERE_STACKS){
  FXTRACE((100,"FXGLSphere::FXGLSphere\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=-radius; range.upper.y=radius;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Copy constructor
FXGLSphere::FXGLSphere(const FXGLSphere& orig):FXGLShape(orig){
  FXTRACE((100,"FXGLSphere::FXGLSphere\n"));
  radius=orig.radius;
  slices=orig.slices;
  stacks=orig.stacks;
  }


// Draw
void FXGLSphere::drawshape(FXGLViewer*){
#ifdef HAVE_GL_H
  GLUquadricObj* quad=gluNewQuadric();
  gluQuadricDrawStyle(quad,(GLenum)GLU_FILL);
  /*
    if (shading==FXGLShape::ID_SHADESMOOTH){
    gluQuadricNormals(quad,(GLenum)GLU_SMOOTH);
    gluQuadricOrientation(quad,(GLenum)GLU_OUTSIDE);
    }
  */
  gluSphere(quad,radius,slices,stacks);
  gluDeleteQuadric(quad);
#endif
  }


// Copy this object
FXGLObject* FXGLSphere::copy(){
  return new FXGLSphere(*this);
  }


// Save object to stream
void FXGLSphere::save(FXStream& store) const {
  FXGLShape::save(store);
  store << radius;
  store << slices;
  store << stacks;
  }


// Load object from stream
void FXGLSphere::load(FXStream& store){
  FXGLShape::load(store);
  store >> radius;
  store >> slices;
  store >> stacks;
  }


// Destroy
FXGLSphere::~FXGLSphere(){
  FXTRACE((100,"FXGLSphere::~FXGLSphere\n"));
  }

}
