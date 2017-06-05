/********************************************************************************
*                                                                               *
*                      O p e n G L   S h a p e   O b j e c t                    *
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
* $Id: FXGLShape.cpp,v 1.42 2006/01/22 17:58:28 fox Exp $                       *
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
#include "FXGLShape.h"


#define FACTOR  0.5f
#define BIAS    0.002f

using namespace FX;

/*******************************************************************************/

namespace FX {

// Drop
FXDEFMAP(FXGLShape) FXGLShapeMap[]={
  FXMAPFUNC(SEL_DND_DROP,0,FXGLShape::onDNDDrop),
  FXMAPFUNC(SEL_DND_MOTION,0,FXGLShape::onDNDMotion),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXGLShape::onQueryTip),
  FXMAPFUNC(SEL_COMMAND,FXGLShape::ID_SHADEOFF,FXGLShape::onCmdShadeOff),
  FXMAPFUNC(SEL_COMMAND,FXGLShape::ID_SHADEON,FXGLShape::onCmdShadeOn),
  FXMAPFUNC(SEL_COMMAND,FXGLShape::ID_SHADESMOOTH,FXGLShape::onCmdShadeSmooth),
  FXMAPFUNC(SEL_UPDATE,FXGLShape::ID_SHADEOFF,FXGLShape::onUpdShadeOff),
  FXMAPFUNC(SEL_UPDATE,FXGLShape::ID_SHADEON,FXGLShape::onUpdShadeOn),
  FXMAPFUNC(SEL_UPDATE,FXGLShape::ID_SHADESMOOTH,FXGLShape::onUpdShadeSmooth),
  FXMAPFUNC(SEL_COMMAND,FXGLShape::ID_FRONT_MATERIAL,FXGLShape::onCmdFrontMaterial),
  FXMAPFUNC(SEL_UPDATE,FXGLShape::ID_FRONT_MATERIAL,FXGLShape::onUpdFrontMaterial),
  FXMAPFUNC(SEL_COMMAND,FXGLShape::ID_BACK_MATERIAL,FXGLShape::onCmdBackMaterial),
  FXMAPFUNC(SEL_UPDATE,FXGLShape::ID_BACK_MATERIAL,FXGLShape::onUpdBackMaterial),
  FXMAPFUNCS(SEL_COMMAND,FXGLShape::ID_STYLE_POINTS,FXGLShape::ID_STYLE_BOUNDINGBOX,FXGLShape::onCmdDrawingStyle),
  FXMAPFUNCS(SEL_UPDATE,FXGLShape::ID_STYLE_POINTS,FXGLShape::ID_STYLE_BOUNDINGBOX,FXGLShape::onUpdDrawingStyle)
  };


// Object implementation
FXIMPLEMENT_ABSTRACT(FXGLShape,FXGLObject,FXGLShapeMap,ARRAYNUMBER(FXGLShapeMap))



// Serialization
FXGLShape::FXGLShape(){
  position.x=0.0f;
  position.y=0.0f;
  position.z=0.0f;
  material[0].ambient=FXVec4f(0.2f,0.2f,0.2f,1.0f);
  material[0].diffuse=FXVec4f(0.8f,0.8f,0.8f,1.0f);
  material[0].specular=FXVec4f(1.0f,1.0f,1.0f,1.0f);
  material[0].emission=FXVec4f(0.0f,0.0f,0.0f,1.0f);
  material[0].shininess=30.0f;
  material[1].ambient=FXVec4f(0.2f,0.2f,0.2f,1.0f);
  material[1].diffuse=FXVec4f(0.8f,0.8f,0.8f,1.0f);
  material[1].specular=FXVec4f(1.0f,1.0f,1.0f,1.0f);
  material[1].emission=FXVec4f(0.0f,0.0f,0.0f,1.0f);
  material[1].shininess=30.0;
  range.lower.x=-1.0f;
  range.lower.y=-1.0f;
  range.lower.z=-1.0f;
  range.upper.x= 1.0f;
  range.upper.y= 1.0f;
  range.upper.z= 1.0f;
  options=SHADING_SMOOTH|STYLE_SURFACE;
  }


// Create initialized shape
FXGLShape::FXGLShape(FXfloat x,FXfloat y,FXfloat z,FXuint opts){
  position.x=x;
  position.y=y;
  position.z=z;
  material[0].ambient=FXVec4f(0.2f,0.2f,0.2f,1.0f);
  material[0].diffuse=FXVec4f(0.8f,0.8f,0.8f,1.0f);
  material[0].specular=FXVec4f(1.0f,1.0f,1.0f,1.0f);
  material[0].emission=FXVec4f(0.0f,0.0f,0.0f,1.0f);
  material[0].shininess=30.0f;
  material[1].ambient=FXVec4f(0.2f,0.2f,0.2f,1.0f);
  material[1].diffuse=FXVec4f(0.8f,0.8f,0.8f,1.0f);
  material[1].specular=FXVec4f(1.0f,1.0f,1.0f,1.0f);
  material[1].emission=FXVec4f(0.0f,0.0f,0.0f,1.0f);
  material[1].shininess=30.0f;
  range.lower.x=-1.0f;
  range.lower.y=-1.0f;
  range.lower.z=-1.0f;
  range.upper.x= 1.0f;
  range.upper.y= 1.0f;
  range.upper.z= 1.0f;
  options=opts;
  }


// Create initialized shape
FXGLShape::FXGLShape(FXfloat x,FXfloat y,FXfloat z,FXuint opts,const FXMaterial& front,const FXMaterial& back){
  position.x=x;
  position.y=y;
  position.z=z;
  material[0]=front;
  material[1]=back;
  range.lower.x=-1.0f;
  range.lower.y=-1.0f;
  range.lower.z=-1.0f;
  range.upper.x= 1.0f;
  range.upper.y= 1.0f;
  range.upper.z= 1.0f;
  options=opts;
  }


// Copy constructor
FXGLShape::FXGLShape(const FXGLShape& orig):FXGLObject(orig){
  position=orig.position;
  material[0]=orig.material[0];
  material[1]=orig.material[1];
  range=orig.range;
  options=orig.options;
  tip=orig.tip;
  }


// Copy this object
FXGLObject* FXGLShape::copy(){
  return new FXGLShape(*this);
  }


// Return true if it can be dragged
FXbool FXGLShape::canDrag() const { return TRUE; }


// Object may be deleted
FXbool FXGLShape::canDelete() const { return TRUE; }


// Handle drag-and-drop drop
long FXGLShape::onDNDDrop(FXObject* sender,FXSelector,void*){
  FXushort *clr; FXuint len; FXVec4f color;
  if(((FXWindow*)sender)->getDNDData(FROM_DRAGNDROP,FXWindow::colorType,(FXuchar*&)clr,len)){
    color[0]=clr[0]/65535.0f;
    color[1]=clr[1]/65535.0f;
    color[2]=clr[2]/65535.0f;
    color[3]=clr[3]/65535.0f;
    FXFREE(&clr);
    material[0].ambient=color;
    material[0].diffuse=color;
    material[1].ambient=color;
    material[1].diffuse=color;
    return 1;
    }
  return 0;
  }


// Cursor got dragged over here.
long FXGLShape::onDNDMotion(FXObject*,FXSelector,void*){
  return 1;
  }


// We were asked about tip text
long FXGLShape::onQueryTip(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&tip);
  return 1;
  }


// Shading off
long FXGLShape::onCmdShadeOff(FXObject*,FXSelector,void*){
  options&=~(SHADING_SMOOTH|SHADING_FLAT);
  return 1;
  }


// Shading on [flat]
long FXGLShape::onCmdShadeOn(FXObject*,FXSelector,void*){
  options&=~SHADING_SMOOTH;
  options|=SHADING_FLAT;
  return 1;
  }


// Shading on [smoooooooooth]
long FXGLShape::onCmdShadeSmooth(FXObject*,FXSelector,void*){
  options&=~SHADING_FLAT;
  options|=SHADING_SMOOTH;
  return 1;
  }

// Update shading off button
long FXGLShape::onUpdShadeOff(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
  sender->handle(this,(options&(SHADING_FLAT|SHADING_SMOOTH))?FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK):FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),NULL);
  return 1;
  }

// Update shading on button
long FXGLShape::onUpdShadeOn(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
  sender->handle(this,(options&SHADING_FLAT)?FXSEL(SEL_COMMAND,FXWindow::ID_CHECK):FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
  return 1;
  }

// Update shading smooth button
long FXGLShape::onUpdShadeSmooth(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
  sender->handle(this,(options&SHADING_SMOOTH)?FXSEL(SEL_COMMAND,FXWindow::ID_CHECK):FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
  return 1;
  }


// Drag shape around
FXbool FXGLShape::drag(FXGLViewer* viewer,FXint fx,FXint fy,FXint tx,FXint ty){
  FXfloat zz=viewer->worldToEyeZ(position);
  FXVec3f wf=viewer->eyeToWorld(viewer->screenToEye(fx,fy,zz));
  FXVec3f wt=viewer->eyeToWorld(viewer->screenToEye(tx,ty,zz));
  position+=wt-wf;
  return TRUE;
  }


// Draw
void FXGLShape::draw(FXGLViewer* viewer){
#ifdef HAVE_GL_H

  // Save attributes and matrix
//  glPushAttrib(GL_ENABLE_BIT|GL_CURRENT_BIT|GL_LIGHTING_BIT|GL_POINT_BIT|GL_LINE_BIT|GL_POLYGON_BIT);
//  glPushAttrib(GL_ENABLE_BIT|GL_CURRENT_BIT|GL_LIGHTING_BIT|GL_POINT_BIT|GL_LINE_BIT);
  glPushAttrib(GL_CURRENT_BIT|GL_LIGHTING_BIT|GL_POINT_BIT|GL_LINE_BIT);
  glPushMatrix();

  // Object position
  glTranslatef(position[0],position[1],position[2]);

  // Draw full object
  if(!viewer->doesTurbo()){

    // Shading
    if(options&(SHADING_SMOOTH|SHADING_FLAT)){
      glEnable(GL_LIGHTING);
      if(options&SHADING_SMOOTH){
        glEnable(GL_AUTO_NORMAL);
        glShadeModel(GL_SMOOTH);
        }
      else{
        glDisable(GL_AUTO_NORMAL);
        glShadeModel(GL_FLAT);
        }
      }
    else{
      glDisable(GL_LIGHTING);
      }

    // Material
    if(options&SURFACE_DUALSIDED){
      glMaterialfv(GL_FRONT,GL_AMBIENT,material[0].ambient);
      glMaterialfv(GL_FRONT,GL_DIFFUSE,material[0].diffuse);
      glMaterialfv(GL_FRONT,GL_SPECULAR,material[0].specular);
      glMaterialfv(GL_FRONT,GL_EMISSION,material[0].emission);
      glMaterialf(GL_FRONT,GL_SHININESS,material[0].shininess);
      glMaterialfv(GL_BACK,GL_AMBIENT,material[1].ambient);
      glMaterialfv(GL_BACK,GL_DIFFUSE,material[1].diffuse);
      glMaterialfv(GL_BACK,GL_SPECULAR,material[1].specular);
      glMaterialfv(GL_BACK,GL_EMISSION,material[1].emission);
      glMaterialf(GL_BACK,GL_SHININESS,material[1].shininess);
      }
    else{
      glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,material[0].ambient);
      glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,material[0].diffuse);
      glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,material[0].specular);
      glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,material[0].emission);
      glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,material[0].shininess);
      }

    // Surface
    if(options&STYLE_SURFACE){
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      if(options&FACECULLING_ON)
        glEnable(GL_CULL_FACE);
      else
        glDisable(GL_CULL_FACE);
      drawshape(viewer);
      }

    // Wire frame
    if(options&STYLE_WIREFRAME){
      glDisable(GL_LIGHTING);
      glShadeModel(GL_FLAT);
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      if(options&STYLE_SURFACE){                  // Wires over surface
#if GL_EXT_polygon_offset
        glEnable(GL_POLYGON_OFFSET_EXT);
#ifdef GL_VERSION_1_2
        glPolygonOffset(FACTOR, BIAS);
#else
        glPolygonOffsetEXT(FACTOR, BIAS);
#endif
        drawshape(viewer);
        glDisable(GL_POLYGON_OFFSET_EXT);
#elif GLU_VERSION_1_1
        glEnable(GL_POLYGON_OFFSET_LINE);
        glPolygonOffset(FACTOR, BIAS);
        drawshape(viewer);
        glDisable(GL_POLYGON_OFFSET_LINE);
#endif
        }
      else{                                       // Wires by themselves
        glDisable(GL_CULL_FACE);
        drawshape(viewer);
        }
      }

    // Points
    if(options&STYLE_POINTS){
      glDisable(GL_LIGHTING);
      glShadeModel(GL_FLAT);
      glPolygonMode(GL_FRONT_AND_BACK,GL_POINT);
      if(options&STYLE_SURFACE){                  // Points over surface
#if GL_EXT_polygon_offset
        glEnable(GL_POLYGON_OFFSET_EXT);
#ifdef GL_VERSION_1_2
        glPolygonOffset(FACTOR, BIAS);
#else
        glPolygonOffsetEXT(FACTOR, BIAS);
#endif
        drawshape(viewer);
        glDisable(GL_POLYGON_OFFSET_EXT);
#elif GLU_VERSION_1_1
        glEnable(GL_POLYGON_OFFSET_POINT);
        glPolygonOffset(FACTOR, BIAS);
        drawshape(viewer);
        glDisable(GL_POLYGON_OFFSET_POINT);
#endif
        }
      else{
        glDisable(GL_CULL_FACE);
        drawshape(viewer);
        }
      }
    }

  // Box
//  if((options&STYLE_BOUNDBOX) || selected){
//    glDisable(GL_LIGHTING);
//    glShadeModel(GL_FLAT);
//    if(selected){
//      glColor3f(0.0,1.0,0.0);
//      drawbox();
//      glPointSize(4.0);
//      drawhandles();
//      }
//    else{
//      glColor3f(0.7f,0.7f,0.7f);
//      drawbox();
//      }
//    }

  // Restore attributes and matrix
  glPopMatrix();
  glPopAttrib();
#endif
  }


// Draw for hit
void FXGLShape::hit(FXGLViewer* viewer){
  draw(viewer);
  }


// Get bounding box
void FXGLShape::bounds(FXRangef& box){
  box.lower.x=position.x+range.lower.x; box.upper.x=position.x+range.upper.x;
  box.lower.y=position.y+range.lower.y; box.upper.y=position.y+range.upper.y;
  box.lower.z=position.z+range.lower.z; box.upper.z=position.z+range.upper.z;
  }


// Draw a box
void FXGLShape::drawbox(){
#ifdef HAVE_GL_H
  glBegin(GL_LINE_LOOP);
  glVertex3f(range.upper.x, range.lower.y, range.lower.z);
  glVertex3f(range.upper.x, range.lower.y, range.upper.z);
  glVertex3f(range.lower.x, range.lower.y, range.upper.z);
  glVertex3f(range.lower.x, range.upper.y, range.upper.z);
  glVertex3f(range.upper.x, range.upper.y, range.upper.z);
  glVertex3f(range.upper.x, range.upper.y, range.lower.z);
  glVertex3f(range.lower.x, range.upper.y, range.lower.z);
  glVertex3f(range.lower.x, range.lower.y, range.lower.z);
  glEnd();
  glBegin(GL_LINES);
  glVertex3f(range.lower.x, range.lower.y, range.lower.z);
  glVertex3f(range.lower.x, range.lower.y, range.upper.z);
  glVertex3f(range.lower.x, range.upper.y, range.lower.z);
  glVertex3f(range.lower.x, range.upper.y, range.upper.z);
  glVertex3f(range.upper.x, range.lower.y, range.lower.z);
  glVertex3f(range.upper.x, range.upper.y, range.lower.z);
  glVertex3f(range.upper.x, range.lower.y, range.upper.z);
  glVertex3f(range.upper.x, range.upper.y, range.upper.z);
  glEnd();
#endif
  }


// Draw handles
void FXGLShape::drawhandles(){
#ifdef HAVE_GL_H
  glBegin(GL_POINTS);
  glVertex3f(range.lower.x, range.lower.y, range.lower.z);
  glVertex3f(range.lower.x, range.lower.y, range.upper.z);
  glVertex3f(range.lower.x, range.upper.y, range.lower.z);
  glVertex3f(range.lower.x, range.upper.y, range.upper.z);
  glVertex3f(range.upper.x, range.lower.y, range.lower.z);
  glVertex3f(range.upper.x, range.lower.y, range.upper.z);
  glVertex3f(range.upper.x, range.upper.y, range.lower.z);
  glVertex3f(range.upper.x, range.upper.y, range.upper.z);
  glEnd();
#endif
  }


// Command from material editor
long FXGLShape::onCmdFrontMaterial(FXObject*,FXSelector,void *ptr){
  setMaterial(0,*((FXMaterial*)ptr));
  return 1;
  }


// Update material editor
long FXGLShape::onUpdFrontMaterial(FXObject *sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETVALUE),(void*)&material[0]);
  return 1;
  }


// Command from material editor
long FXGLShape::onCmdBackMaterial(FXObject*,FXSelector,void *ptr){
  setMaterial(1,*((FXMaterial*)ptr));
  return 1;
  }


// Update material editor
long FXGLShape::onUpdBackMaterial(FXObject *sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETVALUE),(void*)&material[1]);
  return 1;
  }


// Change material of side of surface
void FXGLShape::setMaterial(FXint side,const FXMaterial& mtl){
  material[side]=mtl;
  }


// Obtain material of surface
void FXGLShape::getMaterial(FXint side,FXMaterial& mtl) const {
  mtl=material[side];
  }


// Drawing style toggles
long FXGLShape::onCmdDrawingStyle(FXObject*,FXSelector sel,void*){
  switch(FXSELID(sel)){
    case ID_STYLE_SURFACE: options^=STYLE_SURFACE; break;
    case ID_STYLE_POINTS: options^=STYLE_POINTS; break;
    case ID_STYLE_WIREFRAME: options^=STYLE_WIREFRAME; break;
    case ID_STYLE_BOUNDINGBOX: options^=STYLE_BOUNDBOX; break;
    }
  return 1;
  }


// Update drawing style toggles
long FXGLShape::onUpdDrawingStyle(FXObject *sender,FXSelector sel,void*){
  FXSelector msg=FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK);
  switch(FXSELID(sel)){
    case ID_STYLE_SURFACE: if(options&STYLE_SURFACE) msg=FXSEL(SEL_COMMAND,FXWindow::ID_CHECK); break;
    case ID_STYLE_POINTS: if(options&STYLE_POINTS) msg=FXSEL(SEL_COMMAND,FXWindow::ID_CHECK); break;
    case ID_STYLE_WIREFRAME: if(options&STYLE_WIREFRAME) msg=FXSEL(SEL_COMMAND,FXWindow::ID_CHECK); break;
    case ID_STYLE_BOUNDINGBOX: if(options&STYLE_BOUNDBOX) msg=FXSEL(SEL_COMMAND,FXWindow::ID_CHECK); break;
    }
  sender->handle(this,msg,NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
  return 1;
  }


// Save object to stream
void FXGLShape::save(FXStream& store) const {
  FXGLObject::save(store);
  store << position;
  store << material[0].ambient;
  store << material[0].diffuse;
  store << material[0].specular;
  store << material[0].emission;
  store << material[0].shininess;
  store << material[1].ambient;
  store << material[1].diffuse;
  store << material[1].specular;
  store << material[1].emission;
  store << material[1].shininess;
  store << range;
  store << options;
  store << tip;
  }


// Load object from stream
void FXGLShape::load(FXStream& store){
  FXGLObject::load(store);
  store >> position;
  store >> material[0].ambient;
  store >> material[0].diffuse;
  store >> material[0].specular;
  store >> material[0].emission;
  store >> material[0].shininess;
  store >> material[1].ambient;
  store >> material[1].diffuse;
  store >> material[1].specular;
  store >> material[1].emission;
  store >> material[1].shininess;
  store >> range;
  store >> options;
  store >> tip;
  }

}
