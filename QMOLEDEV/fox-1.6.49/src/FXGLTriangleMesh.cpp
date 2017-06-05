/********************************************************************************
*                                                                               *
*               O p e n G L   T r i a n g l e   M e s h   O b j e c t           *
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
* $Id: FXGLTriangleMesh.cpp,v 1.30 2006/01/22 17:58:28 fox Exp $                *
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
#include "FXGLTriangleMesh.h"



/*
  Notes:

  - This is actually not a mesh at all but an just a bunch
    of individual triangles.

  - To do: make this into a true mesh with shared vertices,
    so we can actually see some speedup from using the vertex-
    array primitives.

  - Once we have a true mesh, revisit generatenormals() and make
    it spit out smooth normals based on mesh connectivity.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLTriangleMesh,FXGLShape,NULL,0)


// Create cube
FXGLTriangleMesh::FXGLTriangleMesh():
  vertexBuffer(NULL),colorBuffer(NULL),normalBuffer(NULL),textureBuffer(NULL),vertexNumber(0){
  FXTRACE((100,"FXGLTriangleMesh::FXGLTriangleMesh\n"));
  range.lower.x=range.upper.x=0.0f;
  range.lower.y=range.upper.y=0.0f;
  range.lower.z=range.upper.z=0.0f;
  }


// Create initialized tmesh
FXGLTriangleMesh::FXGLTriangleMesh(FXfloat x,FXfloat y,FXfloat z,FXint nv,FXfloat *v,FXfloat *n,FXfloat *c,FXfloat *t):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE),
  vertexBuffer(v),colorBuffer(c),normalBuffer(n),textureBuffer(t),vertexNumber(nv){
  FXTRACE((100,"FXGLTriangleMesh::FXGLTriangleMesh\n"));
  if(!normalBuffer){ generatenormals(); }
  recomputerange();
  }


// Create initialized tmesh
FXGLTriangleMesh::FXGLTriangleMesh(FXfloat x,FXfloat y,FXfloat z,FXint nv,FXfloat *v,FXfloat *n,FXfloat *c,FXfloat *t,const FXMaterial& mtl):
  FXGLShape(x,y,z,SHADING_SMOOTH|STYLE_SURFACE,mtl,mtl),
  vertexBuffer(v),colorBuffer(c),normalBuffer(n),textureBuffer(t),vertexNumber(nv){
  FXTRACE((100,"FXGLTriangleMesh::FXGLTriangleMesh\n"));
  if(!normalBuffer){ generatenormals(); }
  recomputerange();
  }


// Copy constructor
FXGLTriangleMesh::FXGLTriangleMesh(const FXGLTriangleMesh& orig):FXGLShape(orig){
  FXTRACE((100,"FXGLTriangleMesh::FXGLTriangleMesh\n"));
  FXMEMDUP(&vertexBuffer,orig.vertexBuffer,FXfloat,3*orig.vertexNumber);
  FXMEMDUP(&colorBuffer,orig.colorBuffer,FXfloat,4*orig.vertexNumber);
  FXMEMDUP(&normalBuffer,orig.normalBuffer,FXfloat,3*orig.vertexNumber);
  FXMEMDUP(&textureBuffer,orig.textureBuffer,FXfloat,2*orig.vertexNumber);
  vertexNumber=orig.vertexNumber;
  }


// Change vertex buffer means recompute range
void FXGLTriangleMesh::setVertexBuffer(FXfloat *vertices){
  vertexBuffer=vertices;
  recomputerange();
  }


// Recompute range
void FXGLTriangleMesh::recomputerange(){
  register FXint i,n;
  register FXfloat t;
  range.lower.x=range.upper.x=0.0f;
  range.lower.y=range.upper.y=0.0f;
  range.lower.z=range.upper.z=0.0f;
  if(vertexBuffer && vertexNumber>0){
    range.lower.x=range.upper.x=vertexBuffer[0];
    range.lower.y=range.upper.y=vertexBuffer[0];
    range.lower.z=range.upper.z=vertexBuffer[0];
    for(i=0,n=0; n<vertexNumber; n++){
      t=vertexBuffer[i++];
      if(t<range.lower.x) range.lower.x=t;
      if(t>range.upper.x) range.upper.x=t;
      t=vertexBuffer[i++];
      if(t<range.lower.y) range.lower.y=t;
      if(t>range.upper.y) range.upper.y=t;
      t=vertexBuffer[i++];
      if(t<range.lower.z) range.lower.z=t;
      if(t>range.upper.z) range.upper.z=t;
      }
    }
  }


// Draw
void FXGLTriangleMesh::drawshape(FXGLViewer*){
  if(!vertexBuffer || vertexNumber<=0) return;
#ifdef HAVE_GL_H

#if !defined(GL_VERSION_1_1) && !defined(GL_VERSION_1_2)

  register FXint i;

  // Yes we know this is not efficient but hell, OpenGL 1.0 is old...
  glBegin(GL_TRIANGLES);
  for(i=0; i<vertexNumber; i++){
    if(textureBuffer) glTexCoord2fv(&textureBuffer[2*i]);
    if(normalBuffer) glNormal3fv(&normalBuffer[3*i]);
    if(colorBuffer) glColor4fv(&colorBuffer[4*i]);
    glVertex3fv(&vertexBuffer[3*i]);
    }
  glEnd();

#else

  // Setting buffers
  glVertexPointer(3,GL_FLOAT,0,vertexBuffer);
  glEnableClientState(GL_VERTEX_ARRAY);

  if(normalBuffer){
    glNormalPointer(GL_FLOAT,0,normalBuffer);
    glEnableClientState(GL_NORMAL_ARRAY);
    }
  else{
    glDisableClientState(GL_NORMAL_ARRAY);
    }

  if(colorBuffer){
    glColorPointer(4,GL_FLOAT,0,colorBuffer);
    glEnableClientState(GL_COLOR_ARRAY);
    }
  else{
    glDisableClientState(GL_COLOR_ARRAY);
    }

  if(textureBuffer){
    glTexCoordPointer(2,GL_FLOAT,0,textureBuffer);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    }
  else{
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    }

  // drawing
  glDrawArrays(GL_TRIANGLES,0,vertexNumber);

#endif
#endif
  }


// Routine to calculate FLAT normals at each triangle
void FXGLTriangleMesh::generatenormals(){
  register FXint i;
  FXVec3f vec,a,b,c;
  FXASSERT(vertexBuffer);
  if(!normalBuffer){
    FXMALLOC(&normalBuffer,FXfloat,vertexNumber*3);
    }
  for(i=0; i<vertexNumber*3; i+=9){
    a.set(vertexBuffer[i+0],vertexBuffer[i+1],vertexBuffer[i+2]);
    b.set(vertexBuffer[i+3],vertexBuffer[i+4],vertexBuffer[i+5]);
    c.set(vertexBuffer[i+6],vertexBuffer[i+7],vertexBuffer[i+8]);
    vec=normal(a,b,c);
    normalBuffer[i+0]=vec.x;
    normalBuffer[i+1]=vec.y;
    normalBuffer[i+2]=vec.z;

    normalBuffer[i+3]=vec.x;
    normalBuffer[i+4]=vec.y;
    normalBuffer[i+5]=vec.z;

    normalBuffer[i+6]=vec.x;
    normalBuffer[i+7]=vec.y;
    normalBuffer[i+8]=vec.z;
    }
  }



// Copy this object
FXGLObject* FXGLTriangleMesh::copy(){
  return new FXGLTriangleMesh(*this);
  }


// Save object to stream
void FXGLTriangleMesh::save(FXStream& store) const {
  FXuchar hadvertices,hadcolors,hadnormals,hadtextures;
  FXGLShape::save(store);
  store << vertexNumber;
  hadvertices=(vertexBuffer!=NULL);
  hadcolors=(colorBuffer!=NULL);
  hadnormals=(normalBuffer!=NULL);
  hadtextures=(textureBuffer!=NULL);
  store << hadvertices << hadcolors << hadnormals << hadtextures;
  if(hadvertices){ store.save(vertexBuffer,vertexNumber); }
  if(hadcolors){ store.save(colorBuffer,vertexNumber); }
  if(hadnormals){ store.save(normalBuffer,vertexNumber); }
  if(hadtextures){ store.save(textureBuffer,vertexNumber); }
  }


// Load object from stream
void FXGLTriangleMesh::load(FXStream& store){
  FXuchar hadvertices,hadcolors,hadnormals,hadtextures;
  FXGLShape::load(store);
  store >> vertexNumber;
  store >> hadvertices >> hadcolors >> hadnormals >> hadtextures;
  if(hadvertices){ FXMALLOC(&vertexBuffer,FXfloat,3*vertexNumber); store.load(vertexBuffer,3*vertexNumber); }
  if(hadcolors){ FXMALLOC(&colorBuffer,FXfloat,4*vertexNumber); store.load(colorBuffer,4*vertexNumber); }
  if(hadnormals){ FXMALLOC(&normalBuffer,FXfloat,3*vertexNumber); store.load(normalBuffer,3*vertexNumber); }
  if(hadtextures){ FXMALLOC(&textureBuffer,FXfloat,2*vertexNumber); store.load(textureBuffer,2*vertexNumber); }
  }


// Zap object
FXGLTriangleMesh::~FXGLTriangleMesh(){
  FXTRACE((100,"FXGLTriangleMesh::~FXGLTriangleMesh\n"));
  FXFREE(&vertexBuffer);
  FXFREE(&colorBuffer);
  FXFREE(&normalBuffer);
  FXFREE(&textureBuffer);
  }

}
