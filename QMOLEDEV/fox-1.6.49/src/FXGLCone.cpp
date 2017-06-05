/********************************************************************************
*                                                                               *
*                      O p e n G L   C o n e   O b j e c t                      *
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
* $Id: FXGLCone.cpp,v 1.28 2006/01/22 17:58:28 fox Exp $                        *
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
#include "FXGLCone.h"

// GLU versions prior to 1.1 have GLUquadric
#if !defined(GLU_VERSION_1_1) && !defined(GLU_VERSION_1_2) && !defined(GLU_VERSION_1_3)
#define GLUquadricObj GLUquadric
#endif


// Cone fidelity
#define FXGLCONE_SLICES_NUMBER		20
#define FXGLCONE_STACKS_NUMBER		20
#define FXGLCONE_LOOPS			4

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLCone,FXGLShape,NULL,0)


// Create cone
FXGLCone::FXGLCone():height(1.0f),radius(1.0f){
  FXTRACE((100,"FXGLCone::FXGLCone\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=0.0f;    range.upper.y=height;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Create cone
FXGLCone::FXGLCone(FXfloat x,FXfloat y,FXfloat z,FXfloat h,FXfloat r):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE),height(h),radius(r){
  FXTRACE((100,"FXGLCone::FXGLCone\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=0.0f;    range.upper.y=height;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Create cone
FXGLCone::FXGLCone(FXfloat x,FXfloat y,FXfloat z,FXfloat h, FXfloat r,const FXMaterial& mtl):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE,mtl,mtl),height(h),radius(r){
  FXTRACE((100,"FXGLCone::FXGLCone\n"));
  range.lower.x=-radius; range.upper.x=radius;
  range.lower.y=0.0f;    range.upper.y=height;
  range.lower.z=-radius; range.upper.z=radius;
  }


// Copy constructor
FXGLCone::FXGLCone(const FXGLCone& orig):FXGLShape(orig){
  FXTRACE((100,"FXGLCone::FXGLCone\n"));
  height=orig.height;
  radius=orig.radius;
  }


// Draw
void FXGLCone::drawshape(FXGLViewer*){
#ifdef HAVE_GL_H
  GLUquadricObj* quad=gluNewQuadric();
  gluQuadricDrawStyle(quad,(GLenum)GLU_FILL);
  /*
    gluQuadricNormals(quad,(GLenum)GLU_SMOOTH);
    gluQuadricOrientation(quad,(GLenum)GLU_OUTSIDE);
  */
  glPushMatrix();
  glRotatef(-90.0f,1.0f,0.0f,0.0f);
  gluCylinder(quad,radius,0,height,FXGLCONE_SLICES_NUMBER,FXGLCONE_STACKS_NUMBER);
  gluQuadricOrientation(quad,(GLenum)GLU_INSIDE);
  gluDisk(quad,0,radius,FXGLCONE_SLICES_NUMBER,FXGLCONE_LOOPS);
  gluDeleteQuadric(quad);
  glPopMatrix();
#endif
  }


// Copy this object
FXGLObject* FXGLCone::copy(){
  return new FXGLCone(*this);
  }


// Save object to stream
void FXGLCone::save(FXStream& store) const {
  FXGLShape::save(store);
  store << height << radius;
  }


// Load object from stream
void FXGLCone::load(FXStream& store){
  FXGLShape::load(store);
  store >> height >> radius;
  }


// Destroy
FXGLCone::~FXGLCone(){
  FXTRACE((100,"FXGLCone::~FXGLCone\n"));
  }

}
