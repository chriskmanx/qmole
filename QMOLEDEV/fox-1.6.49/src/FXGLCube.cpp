/********************************************************************************
*                                                                               *
*                      O p e n G L   C u b e   O b j e c t                      *
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
* $Id: FXGLCube.cpp,v 1.28 2006/01/22 17:58:28 fox Exp $                        *
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
#include "FXGLCube.h"


using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLCube,FXGLShape,NULL,0)


// Create cube
FXGLCube::FXGLCube():width(1.0f),height(1.0f),depth(1.0f){
  FXTRACE((100,"FXGLCube::FXGLCube\n"));
  range.lower.x=-0.5f*width;  range.upper.x=0.5f*width;
  range.lower.y=-0.5f*height; range.upper.y=0.5f*height;
  range.lower.z=-0.5f*depth;  range.upper.z=0.5f*depth;
  }


// Create cube
FXGLCube::FXGLCube(FXfloat x,FXfloat y,FXfloat z,FXfloat w,FXfloat h,FXfloat d):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE),width(w),height(h),depth(d){
  FXTRACE((100,"FXGLCube::FXGLCube\n"));
  range.lower.x=-0.5f*width;  range.upper.x=0.5f*width;
  range.lower.y=-0.5f*height; range.upper.y=0.5f*height;
  range.lower.z=-0.5f*depth;  range.upper.z=0.5f*depth;
  }


// Create initialized line
FXGLCube::FXGLCube(FXfloat x,FXfloat y,FXfloat z,FXfloat w,FXfloat h,FXfloat d,const FXMaterial& mtl):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE,mtl,mtl),width(w),height(h),depth(d){
  FXTRACE((100,"FXGLCube::FXGLCube\n"));
  range.lower.x=-0.5f*width;  range.upper.x=0.5f*width;
  range.lower.y=-0.5f*height; range.upper.y=0.5f*height;
  range.lower.z=-0.5f*depth;  range.upper.z=0.5f*depth;
  }


// Copy constructor
FXGLCube::FXGLCube(const FXGLCube& orig):FXGLShape(orig){
  FXTRACE((100,"FXGLCube::FXGLCube\n"));
  width=orig.width;
  height=orig.height;
  depth=orig.depth;
  }


// Draw
void FXGLCube::drawshape(FXGLViewer*){
#ifdef HAVE_GL_H
  FXfloat xmin =-0.5f*width;
  FXfloat xmax = 0.5f*width;
  FXfloat ymin =-0.5f*height;
  FXfloat ymax = 0.5f*height;
  FXfloat zmin =-0.5f*depth;
  FXfloat zmax = 0.5f*depth;
  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.0f,0.0f,-1.0f);
    glVertex3f(xmin, ymin, zmin);
    glVertex3f(xmin, ymax, zmin);
    glVertex3f(xmax, ymin, zmin);
    glVertex3f(xmax, ymax, zmin);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(1.0f,0.0f,0.0f);
    glVertex3f(xmax, ymin, zmin);
    glVertex3f(xmax, ymax, zmin);
    glVertex3f(xmax, ymin, zmax);
    glVertex3f(xmax, ymax, zmax);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.0f,0.0f,1.0f);
    glVertex3f(xmax, ymin, zmax);
    glVertex3f(xmax, ymax, zmax);
    glVertex3f(xmin, ymin, zmax);
    glVertex3f(xmin, ymax, zmax);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(-1.0f,0.0f,0.0f);
    glVertex3f(xmin, ymin, zmax);
    glVertex3f(xmin, ymax, zmax);
    glVertex3f(xmin, ymin, zmin);
    glVertex3f(xmin, ymax, zmin);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.0f,1.0f,0.0f);
    glVertex3f(xmin, ymax, zmin);
    glVertex3f(xmin, ymax, zmax);
    glVertex3f(xmax, ymax, zmin);
    glVertex3f(xmax, ymax, zmax);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.0f,-1.0f,0.0f);
    glVertex3f(xmin, ymin, zmax);
    glVertex3f(xmin, ymin, zmin);
    glVertex3f(xmax, ymin, zmax);
    glVertex3f(xmax, ymin, zmin);
  glEnd();
#endif
  }


// Copy this object
FXGLObject* FXGLCube::copy(){
  return new FXGLCube(*this);
  }


// Save object to stream
void FXGLCube::save(FXStream& store) const {
  FXGLShape::save(store);
  store << width << height << depth;
  }


// Load object from stream
void FXGLCube::load(FXStream& store){
  FXGLShape::load(store);
  store >> width >> height >> depth;
  }


// Destroy
FXGLCube::~FXGLCube(){
  FXTRACE((100,"FXGLCube::~FXGLCube\n"));
  }

}
