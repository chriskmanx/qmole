/********************************************************************************
*                                                                               *
*                           O p e n G L   V i e w e r                           *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXGLViewer.cpp,v 1.156.2.1 2006/08/02 01:31:08 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXObjectList.h"
#include "FXApp.h"
#include "FXVisual.h"
#include "FXGLVisual.h"
#include "FXDC.h"
#include "FXDCWindow.h"
#include "FXDCPrint.h"
#include "FXMessageBox.h"
#include "FXToolTip.h"
#include "FXCursor.h"
#include "FXGLContext.h"
#include "FXGLViewer.h"
#include "FXGLObject.h"
#include "FXPrintDialog.h"
#include "jitter.h"

/*
  To Do:
  - Initialize GL to fastest of everything for drawing lines.
  - Group object needs current element.
  - use app->getDragDelta() for motion tolerance.
  - Default op to noop mode; all returns 0 in noop mode.
  - GLuint unfortunately not always big enough to store a pointer...
  - The selection seems to get lost with multiple viewers into.
    the same scene.  If you select a cube in one view, then select another
    cube in another view, both seem to get selected.  Can we push the
    "selection" member from the view to the scene object?
  - Instead of select/deselect, do focus gain/lost type deal.
  - Add methods for inquire of pick-ray.
  - Fix FXGLGroup to identify child or itself..
  - Need some way of updating ALL viewers.
  - Need a document/view type concept?
  - Load/Save need to save more...
  - Build mini display lists for offset/surface drawing.
  - Pass clicked/double-clicked/triple-clicked messages to object.
  - Distinguish between current object and selected ones.
    only one is current, but many may be selected.
  - When object(s) deleted, need to fix up selection...
  - GLViewer should source some messages to its target for important events.
  - Zoom-lasso feature.
  - Basic mouse actions:

    State     Event      Modifiers         Where           State          Meaning
    --------------------------------------------------------------------------------------------
    HOVERING  Left       --                outside         PICKING        Pick an object if no move
    PICKING   Motion     --                --              ROTATING       Rotate camera about target point
    HOVERING  Left       --                inside object   DRAGGING       Drag object
    HOVERING  Left       Shift             --              LASSOSELECT    Select
    HOVERING  Left       Control           --              LASSOSELECT    Toggle selection
    HOVERING  Left       Right             --              ZOOMING        Zoom in
    HOVERING  Left       Right + Shift     --              TRUCKING       Trucking camera
    HOVERING  Middle     --                --              ZOOMING        Zoom in/out
    HOVERING  Middle     Shift             --              TRUCKING       Trucking camera
    HOVERING  Right      --                --              POSTING        Post popup menu if no move
    POSTING   Motion     --                --              TRANSLATING    Translate camera
    HOVERING  Right      Shift             --              GYRATING       Rotate object about camera
    HOVERING  Right      Control           --              FOVING         Change field of view
    HOVERING  Right      Left              --              ZOOMING        Zoom in
    HOVERING  Right      Left +Shift       --              TRUCKING       Trucking camera

    FIXME FIXME FIXME FIXME FIXME FIXME FIXME

    Should remove "selection" member.  FXGLViewer should have no knowledge
    of any GL object besides scene.

    Should delegate all messages it does not understand to "target" and not
    to current object.

    Picking, selection should pass list of records in callback; this list
    should NOT contain zmin, zmax, but just, for each record: number of
    names, and the names.  List is ended by 0 (no names) record.

    The list should be sorted based on zmin, zmax, with first record in list
    being the one closest to the eye.

    Should add FXGLLight objects, which can be added to FXGLGroups.

    Should add subclass of FXGLGroup which pushes/pops attributes.

    Note that we will keep the camera state in the FXGLViewer widget, i.e.
    won't have camera objects.

    FIXME FIXME FIXME FIXME FIXME FIXME FIXME

  - Need 3 dials to rotate about model (not screen) axes.

  - Zoom centered on point at which you started the zoom operation.
*/


// Size of pick buffer
#define MAX_PICKBUF    1024

// Maximum length of selection path
#define MAX_SELPATH    64

// Rotation tolerance
#define EPS            1.0E-2

// Pick tolerance
#define PICK_TOL       3

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXGLViewer) FXGLViewerMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXGLViewer::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXGLViewer::onMotion),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXGLViewer::onMouseWheel),
  FXMAPFUNC(SEL_TIMEOUT,FXGLViewer::ID_TIPTIMER,FXGLViewer::onTipTimer),
  FXMAPFUNC(SEL_DND_ENTER,0,FXGLViewer::onDNDEnter),
  FXMAPFUNC(SEL_DND_LEAVE,0,FXGLViewer::onDNDLeave),
  FXMAPFUNC(SEL_DND_DROP,0,FXGLViewer::onDNDDrop),
  FXMAPFUNC(SEL_DND_MOTION,0,FXGLViewer::onDNDMotion),
  FXMAPFUNC(SEL_ENTER,0,FXGLViewer::onEnter),
  FXMAPFUNC(SEL_LEAVE,0,FXGLViewer::onLeave),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXGLViewer::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXGLViewer::onLeftBtnRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXGLViewer::onMiddleBtnPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXGLViewer::onMiddleBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXGLViewer::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXGLViewer::onRightBtnRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXGLViewer::onUngrabbed),
  FXMAPFUNC(SEL_KEYPRESS,0,FXGLViewer::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXGLViewer::onKeyRelease),
  FXMAPFUNC(SEL_FOCUSIN,0,FXGLViewer::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXGLViewer::onFocusOut),
  FXMAPFUNC(SEL_CHANGED,0,FXGLViewer::onChanged),
  FXMAPFUNC(SEL_CLICKED,0,FXGLViewer::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXGLViewer::onDoubleClicked),
  FXMAPFUNC(SEL_TRIPLECLICKED,0,FXGLViewer::onTripleClicked),
  FXMAPFUNC(SEL_LASSOED,0,FXGLViewer::onLassoed),
  FXMAPFUNC(SEL_SELECTED,0,FXGLViewer::onSelected),
  FXMAPFUNC(SEL_DESELECTED,0,FXGLViewer::onDeselected),
  FXMAPFUNC(SEL_INSERTED,0,FXGLViewer::onInserted),
  FXMAPFUNC(SEL_DELETED,0,FXGLViewer::onDeleted),
  FXMAPFUNC(SEL_PICKED,0,FXGLViewer::onPick),
  FXMAPFUNC(SEL_CLIPBOARD_LOST,0,FXGLViewer::onClipboardLost),
  FXMAPFUNC(SEL_CLIPBOARD_GAINED,0,FXGLViewer::onClipboardGained),
  FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,FXGLViewer::onClipboardRequest),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXGLViewer::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXGLViewer::onQueryHelp),
  FXMAPFUNCS(SEL_UPDATE,FXGLViewer::ID_DIAL_X,FXGLViewer::ID_DIAL_Z,FXGLViewer::onUpdXYZDial),
  FXMAPFUNCS(SEL_CHANGED,FXGLViewer::ID_DIAL_X,FXGLViewer::ID_DIAL_Z,FXGLViewer::onCmdXYZDial),
  FXMAPFUNCS(SEL_COMMAND,FXGLViewer::ID_DIAL_X,FXGLViewer::ID_DIAL_Z,FXGLViewer::onCmdXYZDial),
  FXMAPFUNCS(SEL_UPDATE,FXGLViewer::ID_ROLL,FXGLViewer::ID_YAW,FXGLViewer::onUpdRollPitchYaw),
  FXMAPFUNCS(SEL_COMMAND,FXGLViewer::ID_ROLL,FXGLViewer::ID_YAW,FXGLViewer::onCmdRollPitchYaw),
  FXMAPFUNCS(SEL_CHANGED,FXGLViewer::ID_ROLL,FXGLViewer::ID_YAW,FXGLViewer::onCmdRollPitchYaw),
  FXMAPFUNCS(SEL_UPDATE,FXGLViewer::ID_SCALE_X,FXGLViewer::ID_SCALE_Z,FXGLViewer::onUpdXYZScale),
  FXMAPFUNCS(SEL_COMMAND,FXGLViewer::ID_SCALE_X,FXGLViewer::ID_SCALE_Z,FXGLViewer::onCmdXYZScale),
  FXMAPFUNCS(SEL_CHANGED,FXGLViewer::ID_SCALE_X,FXGLViewer::ID_SCALE_Z,FXGLViewer::onCmdXYZScale),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_PERSPECTIVE,FXGLViewer::onUpdPerspective),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_PERSPECTIVE,FXGLViewer::onCmdPerspective),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_PARALLEL,FXGLViewer::onUpdParallel),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_PARALLEL,FXGLViewer::onCmdParallel),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_FRONT,FXGLViewer::onUpdFront),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_FRONT,FXGLViewer::onCmdFront),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_BACK,FXGLViewer::onUpdBack),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_BACK,FXGLViewer::onCmdBack),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LEFT,FXGLViewer::onUpdLeft),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LEFT,FXGLViewer::onCmdLeft),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_RIGHT,FXGLViewer::onUpdRight),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_RIGHT,FXGLViewer::onCmdRight),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_TOP,FXGLViewer::onUpdTop),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_TOP,FXGLViewer::onCmdTop),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_BOTTOM,FXGLViewer::onUpdBottom),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_BOTTOM,FXGLViewer::onCmdBottom),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_RESETVIEW,FXWindow::onUpdYes),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_RESETVIEW,FXGLViewer::onCmdResetView),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_FITVIEW,FXWindow::onUpdYes),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_FITVIEW,FXGLViewer::onCmdFitView),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_CUT_SEL,FXGLViewer::onUpdDeleteSel),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_CUT_SEL,FXGLViewer::onCmdCutSel),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_COPY_SEL,FXGLViewer::onUpdCurrent),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_COPY_SEL,FXGLViewer::onCmdCopySel),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_PASTE_SEL,FXGLViewer::onUpdYes),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_PASTE_SEL,FXGLViewer::onCmdPasteSel),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_DELETE_SEL,FXGLViewer::onUpdDeleteSel),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_DELETE_SEL,FXGLViewer::onCmdDeleteSel),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_BACK_COLOR,FXGLViewer::onUpdBackColor),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_BACK_COLOR,FXGLViewer::onCmdBackColor),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_BACK_COLOR,FXGLViewer::onCmdBackColor),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_AMBIENT_COLOR,FXGLViewer::onUpdAmbientColor),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_AMBIENT_COLOR,FXGLViewer::onCmdAmbientColor),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_AMBIENT_COLOR,FXGLViewer::onCmdAmbientColor),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LIGHTING,FXGLViewer::onUpdLighting),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LIGHTING,FXGLViewer::onCmdLighting),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_FOG,FXGLViewer::onUpdFog),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_DITHER,FXGLViewer::onUpdDither),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LIGHT_AMBIENT,FXGLViewer::onUpdLightAmbient),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LIGHT_AMBIENT,FXGLViewer::onCmdLightAmbient),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_LIGHT_AMBIENT,FXGLViewer::onCmdLightAmbient),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LIGHT_DIFFUSE,FXGLViewer::onUpdLightDiffuse),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LIGHT_DIFFUSE,FXGLViewer::onCmdLightDiffuse),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_LIGHT_DIFFUSE,FXGLViewer::onCmdLightDiffuse),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LIGHT_SPECULAR,FXGLViewer::onUpdLightSpecular),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LIGHT_SPECULAR,FXGLViewer::onCmdLightSpecular),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_LIGHT_SPECULAR,FXGLViewer::onCmdLightSpecular),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_TURBO,FXGLViewer::onUpdTurbo),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_TURBO,FXGLViewer::onCmdTurbo),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_PRINT_IMAGE,FXGLViewer::onUpdYes),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_PRINT_VECTOR,FXGLViewer::onUpdYes),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LASSO_ZOOM,FXGLViewer::onUpdYes),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LASSO_ZOOM,FXGLViewer::onCmdLassoZoom),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_LASSO_SELECT,FXGLViewer::onUpdYes),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_LASSO_SELECT,FXGLViewer::onCmdLassoSelect),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_FOG,FXGLViewer::onCmdFog),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_DITHER,FXGLViewer::onCmdDither),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_FOV,FXGLViewer::onUpdFov),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_FOV,FXGLViewer::onCmdFov),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_FOV,FXGLViewer::onCmdFov),
  FXMAPFUNC(SEL_UPDATE,FXGLViewer::ID_ZOOM,FXGLViewer::onUpdZoom),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_ZOOM,FXGLViewer::onCmdZoom),
  FXMAPFUNC(SEL_CHANGED,FXGLViewer::ID_ZOOM,FXGLViewer::onCmdZoom),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_PRINT_IMAGE,FXGLViewer::onCmdPrintImage),
  FXMAPFUNC(SEL_COMMAND,FXGLViewer::ID_PRINT_VECTOR,FXGLViewer::onCmdPrintVector),
  FXMAPFUNCS(SEL_UPDATE,FXGLViewer::ID_TOP_COLOR,FXGLViewer::ID_BOTTOM_COLOR,FXGLViewer::onUpdGradientBackColor),
  FXMAPFUNCS(SEL_COMMAND,FXGLViewer::ID_TOP_COLOR,FXGLViewer::ID_BOTTOM_COLOR,FXGLViewer::onCmdGradientBackColor),
  FXMAPFUNCS(SEL_CHANGED,FXGLViewer::ID_TOP_COLOR,FXGLViewer::ID_BOTTOM_COLOR,FXGLViewer::onCmdGradientBackColor),
  };


// Object implementation
FXIMPLEMENT(FXGLViewer,FXGLCanvas,FXGLViewerMap,ARRAYNUMBER(FXGLViewerMap))


/*******************************************************************************/

// Drag type names for generic object
const FXchar FXGLViewer::objectTypeName[]="application/x-globject";


// Drag type for generic object
FXDragType FXGLViewer::objectType=0;


/*******************************************************************************/


// For deserialization
FXGLViewer::FXGLViewer(){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  dial[0]=0;
  dial[1]=0;
  dial[2]=0;
  dropped=NULL;
  selection=NULL;
  zsortfunc=NULL;
  doesturbo=FALSE;
  mode=HOVERING;
  }


// Construct GL viewer widget with private display list
FXGLViewer::FXGLViewer(FXComposite* p,FXGLVisual *vis,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXGLCanvas(p,vis,NULL,tgt,sel,opts,x,y,w,h){
  initialize();
  }


// Construct GL viewer widget with shared display list
FXGLViewer::FXGLViewer(FXComposite* p,FXGLVisual *vis,FXGLViewer* sharegroup,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXGLCanvas(p,vis,sharegroup,tgt,sel,opts,x,y,w,h){
  initialize();
  }


// Common initialization for constructor
void FXGLViewer::initialize(){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  defaultCursor=getApp()->getDefaultCursor(DEF_CROSSHAIR_CURSOR);
  dragCursor=getApp()->getDefaultCursor(DEF_CROSSHAIR_CURSOR);
  projection=PERSPECTIVE;                       // Projection
  zoom=1.0;                                     // Zoom factor
  fov=30.0;                                     // Field of view (1 to 90)
  wvt.left=-1.0;                                // Init world box
  wvt.right=1.0;
  wvt.top=1.0;
  wvt.bottom=-1.0;
  wvt.hither=0.1;
  wvt.yon=1.0;
  wvt.w=100;                                    // Viewport width
  wvt.h=100;                                    // Viewport height
  diameter=2.0;                                 // Size of model
  distance=7.464116;                            // Distance of PRP to target
  rotation.x=0.0f;                              // Orientation
  rotation.y=0.0f;
  rotation.z=0.0f;
  rotation.w=1.0f;
  center.x=0.0f;                                // Model center
  center.y=0.0f;
  center.z=0.0f;
  scale.x=1.0f;                                 // Model scaling
  scale.y=1.0f;
  scale.z=1.0f;
  updateProjection();                           // Initial projection
  updateTransform();                            // Set transformation
  maxhits=512;                                  // Maximum hit buffer size
  background[0][0]=0.5f;                        // Top background
  background[0][1]=0.5f;
  background[0][2]=1.0f;
  background[0][3]=1.0f;
  background[1][0]=1.0f;                        // Bottom background
  background[1][1]=1.0f;
  background[1][2]=1.0f;
  background[1][3]=1.0f;
  ambient[0]=0.2f;                              // Scene ambient
  ambient[1]=0.2f;
  ambient[2]=0.2f;
  ambient[3]=1.0f;
  light.ambient[0]=0.0f;                        // Light ambient
  light.ambient[1]=0.0f;
  light.ambient[2]=0.0f;
  light.ambient[3]=1.0f;
  light.diffuse[0]=1.0f;                        // Light diffuse
  light.diffuse[1]=1.0f;
  light.diffuse[2]=1.0f;
  light.diffuse[3]=1.0f;
  light.specular[0]=0.0f;                       // Light specular
  light.specular[1]=0.0f;
  light.specular[2]=0.0f;
  light.specular[3]=1.0f;
  light.position[0]=-2.0f;                      // Light position
  light.position[1]= 2.0f;
  light.position[2]= 5.0f;
  light.position[3]= 0.0f;
  light.direction[0]= 0.0f;                     // Light direction
  light.direction[1]= 0.0f;
  light.direction[2]=-1.0f;
  light.exponent=0.0f;                          // Light spot exponent
  light.cutoff=180.0f;                          // Light spot cutoff
  light.c_attn=1.0f;                            // Light constant attenuation
  light.l_attn=0.0f;                            // Light linear attenuation
  light.q_attn=0.0f;                            // Light quadratic attenuation
  material.ambient[0]=0.2f;                     // Material ambient reflectivity
  material.ambient[1]=0.2f;
  material.ambient[2]=0.2f;
  material.ambient[3]=1.0f;
  material.diffuse[0]=0.8f;                     // Material diffuse reflectivity
  material.diffuse[1]=0.8f;
  material.diffuse[2]=0.8f;
  material.diffuse[3]=1.0f;
  material.specular[0]=1.0f;                    // Material specular reflectivity
  material.specular[1]=1.0f;
  material.specular[2]=1.0f;
  material.specular[3]=1.0f;
  material.emission[0]=0.0f;                    // Material emissivity
  material.emission[1]=0.0f;
  material.emission[2]=0.0f;
  material.emission[3]=1.0f;
  material.shininess=30.0f;                     // Material shininess
  dial[0]=0;                                    // Old dial position
  dial[1]=0;                                    // Old dial position
  dial[2]=0;                                    // Old dial position
  dropped=NULL;                                 // Nobody being dropped on
  selection=NULL;                               // No initial selection
  zsortfunc=NULL;                               // Routine to sort feedback buffer
  scene=NULL;                                   // Scene to look at
  doesturbo=FALSE;                              // In interaction
  turbomode=FALSE;                              // Turbo mode
  mode=HOVERING;                                // Mouse operation
  }


// Create window
void FXGLViewer::create(){
  FXRangef r(-1.0f,1.0f,-1.0f,1.0f,-1.0f,1.0f);

  // We would like to have this be true
#ifdef HAVE_GL_H
  FXASSERT(sizeof(FXuint)==sizeof(GLuint));
#endif

  // Create Window
  FXGLCanvas::create();

  // Set up OpenGL environment
  glsetup();

  // Register drag type for color
  if(!colorType){colorType=getApp()->registerDragType(colorTypeName);}
  if(!objectType){objectType=getApp()->registerDragType(objectTypeName);}

  // If have scene already, get correct bounds
  if(scene) scene->bounds(r);

  // Set initial viewing volume
  setBounds(r);
  }


// Detach window
void FXGLViewer::detach(){
  FXGLCanvas::detach();
  colorType=0;
  objectType=0;
  }


/*********************  V i e w i n g   S e t u p  ***************************/


// Set up GL context
void FXGLViewer::glsetup(){
#ifdef HAVE_GL_H

  // Make GL context current
  if(makeCurrent()){

    // Initialize GL context
    glRenderMode(GL_RENDER);

    // Fast hints
    glHint(GL_POLYGON_SMOOTH_HINT,GL_FASTEST);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST);
    glHint(GL_FOG_HINT,GL_FASTEST);
    glHint(GL_LINE_SMOOTH_HINT,GL_FASTEST);
    glHint(GL_POINT_SMOOTH_HINT,GL_FASTEST);

    // Z-buffer test on
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glDepthRange(0.0,1.0);
    glClearDepth(1.0);
    glClearColor(background[0][0],background[0][1],background[0][2],background[0][3]);

    // No face culling
    glDisable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glFrontFace(GL_CCW);

    // Two sided lighting
    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,TRUE);
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambient);

    // Preferred blend over background
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

    // Light on
    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0,GL_AMBIENT,light.ambient);
    glLightfv(GL_LIGHT0,GL_DIFFUSE,light.diffuse);
    glLightfv(GL_LIGHT0,GL_SPECULAR,light.specular);
    glLightfv(GL_LIGHT0,GL_POSITION,light.position);
    glLightfv(GL_LIGHT0,GL_SPOT_DIRECTION,light.direction);
    glLightf(GL_LIGHT0,GL_SPOT_EXPONENT,light.exponent);
    glLightf(GL_LIGHT0,GL_SPOT_CUTOFF,light.cutoff);
    glLightf(GL_LIGHT0,GL_CONSTANT_ATTENUATION,light.c_attn);
    glLightf(GL_LIGHT0,GL_LINEAR_ATTENUATION,light.l_attn);
    glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,light.q_attn);

    // Viewer is close
    glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,TRUE);

    // Material colors
    glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,material.ambient);
    glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,material.diffuse);
    glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,material.specular);
    glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,material.emission);
    glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,material.shininess);

    // Vertex colors change both diffuse and ambient
    glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
    glDisable(GL_COLOR_MATERIAL);

    // Simplest and fastest drawing is default
    glShadeModel(GL_FLAT);
    glDisable(GL_BLEND);
    glDisable(GL_LINE_SMOOTH);
    glDisable(GL_POINT_SMOOTH);
    glDisable(GL_COLOR_MATERIAL);

    // Lighting
    glDisable(GL_LIGHTING);

    // No normalization of normals (it's broken on some machines anyway)
    glDisable(GL_NORMALIZE);

    // Dithering if needed
    glDisable(GL_DITHER);
    makeNonCurrent();
    }
#endif
  }


// Render all the graphics into a world box
void FXGLViewer::drawWorld(FXViewport& wv){
#ifdef HAVE_GL_H

  // Set viewport
  glViewport(0,0,wv.w,wv.h);

  // Reset important stuff
  glShadeModel(GL_SMOOTH);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glDisable(GL_LIGHTING);
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_DITHER);
  glDisable(GL_FOG);
  glDisable(GL_LOGIC_OP);
  glDisable(GL_POLYGON_SMOOTH);
  glDisable(GL_POLYGON_STIPPLE);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_CULL_FACE);
  glDisable(GL_COLOR_MATERIAL);

  // Reset matrices
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  // Clear to solid background
  glClearDepth(1.0);
  glClearColor(background[0][0],background[0][1],background[0][2],background[0][3]);
  if(background[0]==background[1]){
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    }

  // Clear to gradient background
  else{
    glClear(GL_DEPTH_BUFFER_BIT);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(GL_FALSE);
    glBegin(GL_TRIANGLE_STRIP);
    glColor4fv(background[1]); glVertex3f(-1.0f,-1.0f,0.0f); glVertex3f( 1.0f,-1.0f,0.0f);
    glColor4fv(background[0]); glVertex3f(-1.0f, 1.0f,0.0f); glVertex3f( 1.0f, 1.0f,0.0f);
    glEnd();
    }

  // Depth test on by default
  glDepthMask(GL_TRUE);
  glEnable(GL_DEPTH_TEST);

  // Switch to projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  switch(projection){
    case PARALLEL:
      glOrtho(wv.left,wv.right,wv.bottom,wv.top,wv.hither,wv.yon);
      break;
    case PERSPECTIVE:
      glFrustum(wv.left,wv.right,wv.bottom,wv.top,wv.hither,wv.yon);
      break;
    }

  // Switch to model matrix
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  // Set light parameters
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0,GL_AMBIENT,light.ambient);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,light.diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,light.specular);
  glLightfv(GL_LIGHT0,GL_POSITION,light.position);
  glLightfv(GL_LIGHT0,GL_SPOT_DIRECTION,light.direction);
  glLightf(GL_LIGHT0,GL_SPOT_EXPONENT,light.exponent);
  glLightf(GL_LIGHT0,GL_SPOT_CUTOFF,light.cutoff);
  glLightf(GL_LIGHT0,GL_CONSTANT_ATTENUATION,light.c_attn);
  glLightf(GL_LIGHT0,GL_LINEAR_ATTENUATION,light.l_attn);
  glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,light.q_attn);

  // Default material colors
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,material.ambient);
  glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,material.diffuse);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,material.specular);
  glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,material.emission);
  glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,material.shininess);

  // Color commands change both
  glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);

  // Global ambient light
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambient);

  // Enable fog
  if(options&VIEWER_FOG){
    glEnable(GL_FOG);
    glFogfv(GL_FOG_COLOR,background[0]);                // Disappear into the background
    //glFogf(GL_FOG_DENSITY,1.0f);
    glFogf(GL_FOG_START,(GLfloat)(distance-diameter));  // Range tight around model position
    glFogf(GL_FOG_END,(GLfloat)(distance+diameter));    // Far place same as clip plane:- clipped stuff is in the mist!
    glFogi(GL_FOG_MODE,GL_LINEAR);	                // Simple linear depth cueing
    }

  // Dithering
  if(options&VIEWER_DITHER){
    glEnable(GL_DITHER);
    }

  // Enable lighting
  if(options&VIEWER_LIGHTING){
    glEnable(GL_LIGHTING);
    }

  // Set model matrix
  glLoadMatrixf(transform);

  // Draw what's visible
  if(scene) scene->draw(this);
#endif
  }


// Render with anti-aliasing
void FXGLViewer::drawAnti(FXViewport& wv){
#ifdef HAVE_GL_H
  FXViewport jt=wv;
  FXdouble d=0.5*worldpx;
  register FXuint i;
  glClearAccum(0.0f,0.0f,0.0f,0.0f);
  glClear(GL_ACCUM_BUFFER_BIT);
  for(i=0; i<ARRAYNUMBER(jitter); i++){
    jt.left = wv.left+jitter[i][0]*d;
    jt.right = wv.right+jitter[i][0]*d;
    jt.top = wv.top+jitter[i][1]*d;
    jt.bottom = wv.bottom+jitter[i][1]*d;
    drawWorld(jt);
    glAccum(GL_ACCUM,1.0/ARRAYNUMBER(jitter));
    }
  glAccum(GL_RETURN,1.0);
#endif
  }


// Fill select buffer with hits in rectangle
FXint FXGLViewer::selectHits(FXuint*& hits,FXint& nhits,FXint x,FXint y,FXint w,FXint h){
#ifdef HAVE_GL_H
  register FXfloat pickx,picky,pickw,pickh;
  register FXint mh=maxhits;
  hits=NULL;
  nhits=0;
  if(makeCurrent()){

    // Where to pick
    pickx=(wvt.w-2.0f*x-w)/((FXfloat)w);
    picky=(2.0f*y+h-wvt.h)/((FXfloat)h);
    pickw=wvt.w/((FXfloat)w);
    pickh=wvt.h/((FXfloat)h);

    // Set pick projection matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glTranslatef(pickx,picky,0.0f);
    glScalef(pickw,pickh,1.0f);
    switch(projection){
      case PARALLEL:
        glOrtho(wvt.left,wvt.right,wvt.bottom,wvt.top,wvt.hither,wvt.yon);
        break;
      case PERSPECTIVE:
        glFrustum(wvt.left,wvt.right,wvt.bottom,wvt.top,wvt.hither,wvt.yon);
        break;
      }

    // Model matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(transform);

    // Loop until room enough to fit
    do{
      nhits=0;
      if(!FXRESIZE(&hits,FXuint,mh)) break;
      glSelectBuffer(mh,(GLuint*)hits);
      glRenderMode(GL_SELECT);
      glInitNames();
      glPushName(0);
      if(scene) scene->hit(this);
      glPopName();
      nhits=glRenderMode(GL_RENDER);
      mh<<=1;
      }
    while(nhits<0);
    makeNonCurrent();
    if(!nhits) FXFREE(&hits);
    return nhits;
    }
#endif
  return 0;
  }


// Process picks
FXGLObject* FXGLViewer::processHits(FXuint *pickbuffer,FXint nhits){
  FXuint d1,d2,i,n,zmin,zmax,sel=0;
  if(0<=nhits){
    for(i=0,zmin=zmax=4294967295U; nhits>0; i+=n+3,nhits--){
      n=pickbuffer[i];
      d1=pickbuffer[1+i];
      d2=pickbuffer[2+i];
      if(d1<zmin || (d1==zmin && d2<=zmax)){
        zmin=d1;
        zmax=d2;
        sel=i;
        }
      }
    return scene->identify(&pickbuffer[4+sel]);
    }
  return NULL;
  }


// Build NULL-terminated list of ALL picked objects overlapping rectangle
FXGLObject** FXGLViewer::select(FXint x,FXint y,FXint w,FXint h){
  FXGLObject *obj,**objects=NULL;
  FXint nhits,i,j;
  FXuint *hits;
  if(scene && maxhits){
    if(selectHits(hits,nhits,x,y,w,h)){     // FIXME leak
      FXMALLOC(&objects,FXGLObject*,nhits+1);
      for(i=j=0; nhits>0; i+=hits[i]+3,nhits--){
        if((obj=scene->identify(&hits[4+i]))!=NULL) objects[j++]=obj;
        }
      objects[j]=NULL;
      FXFREE(&hits);
      }
    }
  return objects;
  }


// Lasso objects
FXGLObject** FXGLViewer::lasso(FXint x1,FXint y1,FXint x2,FXint y2){
  FXint xlo,xhi,ylo,yhi;
  FXMINMAX(xlo,xhi,x1,x2);
  FXMINMAX(ylo,yhi,y1,y2);
  return select(xlo,ylo,xhi-xlo+1,yhi-ylo+1);
  }


// Pick ONE object at x,y
FXGLObject* FXGLViewer::pick(FXint x,FXint y){
  FXGLObject *obj=NULL;
  FXuint *hits;
  FXint nhits;
  if(scene && maxhits){
    if(selectHits(hits,nhits,x-PICK_TOL,y-PICK_TOL,PICK_TOL*2,PICK_TOL*2)){     // FIXME leak
      obj=processHits(hits,nhits);
      FXFREE(&hits);
      }
    }
  return obj;
  }


// Repaint the GL window
long FXGLViewer::onPaint(FXObject*,FXSelector,void*){
#ifdef HAVE_GL_H
  FXGLVisual *vis=(FXGLVisual*)getVisual();
  FXASSERT(xid);
  if(makeCurrent()){
    drawWorld(wvt);
    if(vis->isDoubleBuffer()) swapBuffers();
    makeNonCurrent();
    }
#endif
  return 1;
  }


// Handle configure notify
void FXGLViewer::layout(){
  wvt.w=width;
  wvt.h=height;
  updateProjection();
  flags&=~FLAG_DIRTY;
  }


// Start motion timer while in this window
long FXGLViewer::onEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onEnter(sender,sel,ptr);
  if(isEnabled()){
    getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
    }
  return 1;
  }


// Stop motion timer when leaving window
long FXGLViewer::onLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onLeave(sender,sel,ptr);
  if(isEnabled()){
    getApp()->removeTimeout(this,ID_TIPTIMER);
    }
  return 1;
  }


// Gained focus
long FXGLViewer::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onFocusIn(sender,sel,ptr);
  if(selection && selection->handle(this,FXSEL(SEL_FOCUSIN,0),ptr)){
    update();
    }
  return 1;
  }


// Lost focus
long FXGLViewer::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onFocusOut(sender,sel,ptr);
  if(selection && selection->handle(this,FXSEL(SEL_FOCUSOUT,0),ptr)){
    update();
    }
  return 1;
  }


// Change scene
void FXGLViewer::setScene(FXGLObject* sc){
  scene=sc;
  update();
  }


// Change field of view
void FXGLViewer::setFieldOfView(FXdouble fv){
  FXdouble tn;
  fov=FXCLAMP(2.0,fv,90.0);
  tn=tan(0.5*DTOR*fov);
  FXASSERT(tn>0.0);
  distance=diameter/tn;
  FXASSERT(distance>0.0);
  updateProjection();
  updateTransform();
  update();
  }


// Change eye distance
void FXGLViewer::setDistance(FXdouble d){
  if(d<diameter) d=diameter;
  if(d>114.0*diameter) d=114.0*diameter;
  if(d!=distance){
    distance=d;
    FXASSERT(distance>0.0);
    fov=2.0*RTOD*atan2(diameter,distance);
    updateProjection();
    updateTransform();
    update();
    }
  }


// Change zoom factor
void FXGLViewer::setZoom(FXdouble zm){
  if(zm<1.0E-30) zm=1.0E-30;
  if(zm!=zoom){
    zoom=zm;
    updateProjection();
    update();
    }
  }


// Change scale factors
void FXGLViewer::setScale(FXVec3f s){
  if(s.x<0.000001f) s.x=0.000001f;
  if(s.y<0.000001f) s.y=0.000001f;
  if(s.z<0.000001f) s.z=0.000001f;
  if(scale!=s){
    scale=s;
    updateTransform();
    update();
    }
  }


// Change orientation to new quaternion
void FXGLViewer::setOrientation(FXQuatf rot){
  if(rot!=rotation){
    rotation=rot;
    rotation.adjust();
    updateTransform();
    update();
    }
  }


// Change world projection
void FXGLViewer::updateProjection(){
  FXdouble hither_fac,r,aspect;

  // Should be non-0 size viewport
  if(wvt.w>0 && wvt.h>0){

    // Aspect ratio of viewer
    aspect = (FXdouble)wvt.h / (FXdouble)wvt.w;

    // Get world box
    r=0.5*diameter/zoom;
    if(wvt.w<=wvt.h){
      wvt.left=-r;
      wvt.right=r;
      wvt.bottom=-r*aspect;
      wvt.top=r*aspect;
      }
    else{
      wvt.left=-r/aspect;
      wvt.right=r/aspect;
      wvt.bottom=-r;
      wvt.top=r;
      }
    FXASSERT(distance>0.0);
    FXASSERT(diameter>0.0);

    // A more intelligent method for setting the
    // clip planes might be interesting...
    wvt.yon=distance+diameter;
    wvt.hither=0.1*wvt.yon;
    //wvt.hither=distance-diameter;
  //  if(wvt.hither<distance-diameter) wvt.hither=distance-diameter;

    // New window
    FXTRACE((100,"wvt.left=%g wvt.right=%g wvt.top=%g wvt.bottom=%g wvt.hither=%g wvt.yon=%g\n",wvt.left,wvt.right,wvt.top,wvt.bottom,wvt.hither,wvt.yon));

    // Size of a pixel in world and model
    worldpx=(wvt.right-wvt.left)/wvt.w;
    modelpx=worldpx*diameter;

    // Precalc stuff for view->world backmapping
    ax=wvt.left;
    ay=wvt.top-worldpx;

    // Report factors
    FXTRACE((100,"worldpx=%g modelpx=%g\n",worldpx,modelpx));

    // Correction for perspective
    if(projection==PERSPECTIVE){
      FXASSERT(distance>0.0);
      hither_fac=wvt.hither/distance;
      wvt.left*=hither_fac;
      wvt.right*=hither_fac;
      wvt.top*=hither_fac;
      wvt.bottom*=hither_fac;
      }
    }
  }


// Change transformation matrix
void FXGLViewer::updateTransform(){
  transform.eye();
  transform.trans(0.0f,0.0f,(FXfloat)-distance);
  transform.rot(rotation);
  transform.scale(scale);
  transform.trans(-center);
  itransform=transform.invert();
//   FXTRACE((150,"itrans=%11.8f %11.8f %11.8f %11.8f\n",itransform[0][0],itransform[0][1],itransform[0][2],itransform[0][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",itransform[1][0],itransform[1][1],itransform[1][2],itransform[1][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",itransform[2][0],itransform[2][1],itransform[2][2],itransform[2][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",itransform[3][0],itransform[3][1],itransform[3][2],itransform[3][3]));
//   FXTRACE((150,"\n"));
//   FXTRACE((150," trans=%11.8f %11.8f %11.8f %11.8f\n",transform[0][0],transform[0][1],transform[0][2],transform[0][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",transform[1][0],transform[1][1],transform[1][2],transform[1][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",transform[2][0],transform[2][1],transform[2][2],transform[2][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",transform[3][0],transform[3][1],transform[3][2],transform[3][3]));
//   FXTRACE((150,"\n"));
//   FXHMat check=itransform*transform;
//   FXTRACE((150," check=%11.8f %11.8f %11.8f %11.8f\n",check[0][0],check[0][1],check[0][2],check[0][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",check[1][0],check[1][1],check[1][2],check[1][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",check[2][0],check[2][1],check[2][2],check[2][3]));
//   FXTRACE((150,"       %11.8f %11.8f %11.8f %11.8f\n",check[3][0],check[3][1],check[3][2],check[3][3]));
//   FXTRACE((150,"\n"));
  }


// Set model bounding box
FXbool FXGLViewer::setBounds(const FXRangef& r){
//   FXTRACE((100,"xlo=%g xhi=%g ylo=%g yhi=%g zlo=%g zhi=%g\n",r.lower.x,r.upper.x,r.lower.y,r.upper.y,r.lower.z,r.upper.z));

  // Model center
  center=r.center();

  // Model size
  diameter=r.longest();

  // Fix zero size model
  if(diameter<1.0E-30) diameter=1.0;

  // Set equal scaling initially
  scale=FXVec3f(1.0f,1.0f,1.0f);

  // Reset distance (and thus field of view)
  setDistance(1.1*diameter);

  return TRUE;
  }


// Fit view to new bounds
FXbool FXGLViewer::fitToBounds(const FXRangef& box){
  FXRangef r(FLT_MAX,-FLT_MAX,FLT_MAX,-FLT_MAX,FLT_MAX,-FLT_MAX);
  FXMat4f m;

  // Get rotation of model
  m.eye();
  m.rot(rotation);
  m.trans(-box.center());

  // Transform box
  for(int i=0; i<8; i++){
    r.include(box.corner(i)*m);
    }

  setBounds(r);
  return TRUE;
  }


// Obtain viewport
void FXGLViewer::getViewport(FXViewport& v) const {
  v=wvt;
  }


// Set material
void FXGLViewer::setMaterial(const FXMaterial &mtl){
  material=mtl;
  update();
  }


// Get material
void FXGLViewer::getMaterial(FXMaterial &mtl) const {
  mtl=material;
  }


// Get screen point from eye coordinate
void FXGLViewer::eyeToScreen(FXint& sx,FXint& sy,FXVec3f e){
  register double xp,yp;
  if(projection==PERSPECTIVE){
    if(e.z==0.0f){ fxerror("%s::eyeToScreen: cannot transform point.\n",getClassName()); }
    xp=-distance*e.x/e.z;
    yp=-distance*e.y/e.z;
    }
  else{
    xp=e.x;
    yp=e.y;
    }
  sx=(int)((xp-ax)/worldpx);
  sy=(int)((ay-yp)/worldpx);
  }


// Convert screen point to eye coords
FXVec3f FXGLViewer::screenToEye(FXint sx,FXint sy,FXfloat eyez){
  register float xp,yp;
  FXVec3f e;
  xp=(float)(worldpx*sx+ax);
  yp=(float)(ay-worldpx*sy);
  if(projection==PERSPECTIVE){
    FXASSERT(distance>0.0);
    e.x=(FXfloat)(-eyez*xp/distance);
    e.y=(FXfloat)(-eyez*yp/distance);
    e.z=eyez;
    }
  else{
    e.x=xp;
    e.y=yp;
    e.z=eyez;
    }
  return e;
  }


// Convert screen to eye, on projection plane
FXVec3f FXGLViewer::screenToTarget(FXint sx,FXint sy){
  return FXVec3f((FXfloat)(worldpx*sx+ax), (FXfloat)(ay-worldpx*sy), (FXfloat)-distance);
  }


// Convert world to eye coords
FXVec3f FXGLViewer::worldToEye(FXVec3f w){
  return w*transform;
  }


// Get eye Z-coordinate of world point
FXfloat FXGLViewer::worldToEyeZ(FXVec3f w){
  return w.x*transform[0][2]+w.y*transform[1][2]+w.z*transform[2][2]+transform[3][2];
  }


// Convert eye to world coords
FXVec3f FXGLViewer::eyeToWorld(FXVec3f e){
  return e*itransform;
  }


// Get world vector
FXVec3f FXGLViewer::worldVector(FXint fx,FXint fy,FXint tx,FXint ty){
  FXVec3f wfm,wto,vec;
  wfm=screenToTarget(fx,fy);
  wto=screenToTarget(tx,ty);
  vec=wto*itransform-wfm*itransform;
  return vec;
  }


// Get a bore vector
FXbool FXGLViewer::getBoreVector(FXint sx,FXint sy,FXVec3f& point,FXVec3f& dir){
  FXVec3f p=eyeToWorld(screenToEye(sx,sy,(FXfloat)(diameter-distance)));
  if(PARALLEL==projection)
    point=eyeToWorld(screenToEye(sx,sy,0.0f));
  else
    point=eyeToWorld(FXVec3f(0.0f,0.0f,0.0f));
  dir=normalize(p-point);
  return TRUE;
  }


// Get eye viewing direction (non-normalized)
FXVec3f FXGLViewer::getEyeVector() const {
  return FXVec3f(-itransform[2][0],-itransform[2][1],-itransform[2][2]);
  }


// Get eye position
FXVec3f FXGLViewer::getEyePosition() const{
  return FXVec3f(itransform[3][0],itransform[3][1],itransform[3][2]);
  }


// Change model center
void FXGLViewer::setCenter(FXVec3f cntr){
  if(center!=cntr){
    center=cntr;
    updateTransform();
    update();
    }
  }


// Translate in world
void FXGLViewer::translate(FXVec3f vec){
  center+=vec;
  updateTransform();
  update();
  }


// Change selection
void FXGLViewer::setSelection(FXGLObject* sel){
  if(selection!=sel){
    selection=sel;
    update();
    }
  }


// Change help text
void FXGLViewer::setHelpText(const FXString& text){
  help=text;
  }


// Change tip text
void FXGLViewer::setTipText(const FXString& text){
  tip=text;
  }


// Translate point into unit-sphere coordinate
FXVec3f FXGLViewer::spherePoint(FXint px,FXint py){
  FXfloat d,t,screenmin;
  FXVec3f v;
  if(wvt.w>wvt.h)
    screenmin=(FXfloat)wvt.h;
  else
    screenmin=(FXfloat)wvt.w;
  v.x=2.0f*(px-0.5f*wvt.w)/screenmin;
  v.y=2.0f*(0.5f*wvt.h-py)/screenmin;
  d=v.x*v.x+v.y*v.y;
  if(d<0.75){
    v.z=sqrtf(1.0-d);
    }
  else if(d<3.0f){
    d=1.7320508008f-sqrtf(d);
    t=1.0f-d*d;
    if(t<0.0f) t=0.0f;
    v.z=1.0f-sqrtf(t);
    }
  else{
    v.z=0.0f;
    }
  return normalize(v);
  }


// Turn camera; simpler now that arc() is changed
FXQuatf FXGLViewer::turn(FXint fx,FXint fy,FXint tx,FXint ty){
  return FXQuatf(spherePoint(fx,fy),spherePoint(tx,ty));
  }


// Draw non-destructive lasso box; drawing twice will erase it again
void FXGLViewer::drawLasso(FXint x0,FXint y0,FXint x1,FXint y1){
#ifdef HAVE_GL_H
  FXGLVisual *vis=(FXGLVisual*)getVisual();

  // With OpenGL, first change back to 1:1 projection mode
  if(makeCurrent()){

    // Save state
    glPushAttrib(GL_COLOR_BUFFER_BIT|GL_ENABLE_BIT|GL_DEPTH_BUFFER_BIT|GL_LINE_BIT);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();

    // Fix xform
    glLoadIdentity();
    glOrtho(0.0,width-1.0,0.0,height-1.0,0.0,1.0);

    // Draw in front buffer, so we see it w/o blowing
    // away the drawing by calling swapBuffers.
    if(vis->isDoubleBuffer()) glDrawBuffer(GL_FRONT);

    // Fancy stuff off
    glLineWidth(1.0);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_COLOR_MATERIAL);
    glDisable(GL_LIGHTING);
    glShadeModel(GL_FLAT);
    glDepthMask(FALSE);
    glDisable(GL_DITHER);

    // Windows
#ifdef WIN32

#ifndef _ALPHA_
    // MS-Windows has logic ops, and they seem to work:- at least
    // with the unaccelerated software rendering they seem to...
    glEnable(GL_COLOR_LOGIC_OP);
    glLogicOp(GL_INVERT);

#else
    // ALPHA CPU's don't have logic ops; or at least they're broken :-(
    glBlendFunc(GL_ONE_MINUS_DST_COLOR,GL_ZERO);
    glEnable(GL_BLEND);

#endif

    // UNIX
#else
#if !defined(GL_VERSION_1_1) || !defined(GL_VERSION_1_2)

    // If you don't have OpenGL 1.1 or better, blending
    // to invert the lasso is your only choice...
    glBlendFunc(GL_ONE_MINUS_DST_COLOR,GL_ZERO);
    glEnable(GL_BLEND);

#else

    // You have OpenGL 1.1 or better, but chances are it
    // still doesn't work, because you may have an incomplete
    // implementation [DEC], or perhaps broken hardware.

    // If it works for you, uncomment the lines below,
    // and comment the ones above...
    glEnable(GL_COLOR_LOGIC_OP);
    glLogicOp(GL_INVERT);
#endif
#endif

    glBegin(GL_LINE_LOOP);
    glColor4ub(255,255,255,255);
    glVertex2i(x0,wvt.h-y0-1);
    glVertex2i(x0,wvt.h-y1-1);
    glVertex2i(x1,wvt.h-y1-1);
    glVertex2i(x1,wvt.h-y0-1);
    glEnd();
    glFinish();         // Moved back here because of driver issues

    // Restore to old state
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    glPopAttrib();
    makeNonCurrent();
    }
#endif
  }

/*************************  Mouse Actions in Viewer  ***************************/


// Current object changed
long FXGLViewer::onChanged(FXObject*,FXSelector,void* ptr){
  setSelection((FXGLObject*)ptr);
  if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),ptr);
  return 1;
  }


// Message that indicates where user picked
long FXGLViewer::onPick(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_PICKED,message),ptr);
  }


// Clicked in widget
long FXGLViewer::onClicked(FXObject*,FXSelector,void* ptr){
  if(target){
    if(target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr)) return 1;
    if(ptr && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr)) return 1;
    }
  return 1;
  }


// Double clicked in widget; ptr may or may not point to an object
long FXGLViewer::onDoubleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr);
  }


// Triple clicked in widget; ptr may or may not point to an object
long FXGLViewer::onTripleClicked(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
  }


// Lassoed object(s)
long FXGLViewer::onLassoed(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXGLObject **objlist;

  // Notify target of lasso
  if(target && target->tryHandle(this,FXSEL(SEL_LASSOED,message),ptr)) return 1;

  // Grab all objects in lasso
  objlist=lasso(event->click_x,event->click_y,event->win_x,event->win_y);

  // Add selection mode
  if(event->state&SHIFTMASK){
    handle(this,FXSEL(SEL_SELECTED,0),(void*)objlist);
    }

  // Toggle selection mode
  else if(event->state&CONTROLMASK){
    handle(this,FXSEL(SEL_DESELECTED,0),(void*)objlist);
    }

  // Free list
  FXFREE(&objlist);

  return 1;
  }


// Selected object(s)
long FXGLViewer::onSelected(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_SELECTED,message),ptr);
  }


// Deselected object(s)
long FXGLViewer::onDeselected(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_DESELECTED,message),ptr);
  }


// Inserted object(s)
long FXGLViewer::onInserted(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_INSERTED,message),ptr);
  }


// Deleted object(s)
long FXGLViewer::onDeleted(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_DELETED,message),ptr);
  }


// Change operation
void FXGLViewer::setOp(FXuint o){
  if(mode!=o){
    switch(o){
      case HOVERING:
        setDragCursor(getDefaultCursor());
        FXTRACE((100,"HOVERING\n"));
        if(doesturbo) update();
        doesturbo=FALSE;
        break;
      case PICKING:
        FXTRACE((100,"PICKING\n"));
        setDragCursor(getDefaultCursor());
        break;
      case ROTATING:
        FXTRACE((100,"ROTATING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_ROTATE_CURSOR));
        break;
      case POSTING:
        FXTRACE((100,"POSTING\n"));
        setDragCursor(getDefaultCursor());
        break;
      case TRANSLATING:
        FXTRACE((100,"TRANSLATING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_MOVE_CURSOR));
        break;
      case ZOOMING:
        FXTRACE((100,"ZOOMING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
        break;
      case FOVING:
        FXTRACE((100,"FOVING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
        break;
      case DRAGGING:
        FXTRACE((100,"DRAGGING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_MOVE_CURSOR));
        break;
      case TRUCKING:
        FXTRACE((100,"TRUCKING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
        break;
      case GYRATING:
        FXTRACE((100,"GYRATING\n"));
        doesturbo=turbomode;
        setDragCursor(getApp()->getDefaultCursor(DEF_ROTATE_CURSOR));
        break;
      case DO_LASSOSELECT:
        if(mode==LASSOSELECT) return;
        FXTRACE((100,"LASSOSELECT\n"));
        setDefaultCursor(getApp()->getDefaultCursor(DEF_CORNERNW_CURSOR));
        /// FIXME grab
        break;
      case LASSOSELECT:
        FXTRACE((100,"LASSOSELECT\n"));
        setDefaultCursor(getDragCursor());
        setDragCursor(getApp()->getDefaultCursor(DEF_CORNERNW_CURSOR));
        break;
      case DO_LASSOZOOM:
        if(mode==LASSOZOOM) return;
        FXTRACE((100,"LASSOZOOM\n"));
        setDefaultCursor(getApp()->getDefaultCursor(DEF_CORNERNW_CURSOR));
        /// FIXME grab
        break;
      case LASSOZOOM:
        FXTRACE((100,"LASSOZOOM\n"));
        setDefaultCursor(getDragCursor());
        setDragCursor(getApp()->getDefaultCursor(DEF_CORNERNW_CURSOR));
        break;
      }
    mode=o;
    }
  }


// Perform the usual mouse manipulation
long FXGLViewer::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  FXTRACE((100,"onLeftBtnPress Mask=%08x\n",event->state));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(event->state&RIGHTBUTTONMASK){
      if(event->state&SHIFTMASK)
        setOp(TRUCKING);
      else
        setOp(ZOOMING);
      }
    else if(event->state&MIDDLEBUTTONMASK){
      setOp(ROTATING);
      }
    else if(mode==DO_LASSOZOOM){
      if(0<=event->click_x && 0<=event->click_y && event->click_x<width && event->click_y<height){
        drawLasso(event->click_x,event->click_y,event->win_x,event->win_y);
        setOp(LASSOZOOM);
        }
      else{
        getApp()->beep();
        }
      }
    else if(mode==DO_LASSOSELECT){
      if(0<=event->click_x && 0<=event->click_y && event->click_x<width && event->click_y<height){
        drawLasso(event->click_x,event->click_y,event->win_x,event->win_y);
        setOp(LASSOSELECT);
        }
      else{
        getApp()->beep();
        }
      }
    else if(event->state&(SHIFTMASK|CONTROLMASK)){
      setOp(PICKING);
      }
    else if(selection && selection->canDrag() && selection==pick(event->click_x,event->click_y)){
      setOp(DRAGGING);
      }
    else{
      setOp(PICKING);
      }
    }
  return 1;
  }


// Left mouse button released
long FXGLViewer::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXGLObject *objects[2];
  FXint new_x,new_y,cx,cy,xl,xh,yl,yh;
  FXVec3f vec;
  FXTRACE((100,"onLeftBtnRelease Mask=%08x\n",event->state));
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(event->state&RIGHTBUTTONMASK){
      if(event->state&SHIFTMASK){
        setOp(GYRATING);
        }
      else if(event->state&CONTROLMASK){
        setOp(FOVING);
        }
      else{
        setOp(TRANSLATING);
        }
      grab();
      }
    else if(event->state&MIDDLEBUTTONMASK){
      if(event->state&SHIFTMASK){
        setOp(TRUCKING);
        }
      else{
        setOp(ZOOMING);
        }
      grab();
      }
    else if(mode==LASSOZOOM){
      new_x=FXCLAMP(0,event->win_x,(width-1));
      new_y=FXCLAMP(0,event->win_y,(height-1));
      drawLasso(event->click_x,event->click_y,new_x,new_y);
      xl=FXMIN(new_x,event->click_x);
      xh=FXMAX(new_x,event->click_x);
      yl=FXMIN(new_y,event->click_y);
      yh=FXMAX(new_y,event->click_y);
      if(xh>xl && yh>yl){
        cx=(getWidth()-(xl+xh))/2;
        cy=(getHeight()-(yl+yh))/2;
        vec=worldVector(0,0,cx,cy);
        translate(-vec);
        setZoom(zoom*getWidth()/(xh-xl));
        }
      setOp(HOVERING);
      }
    else if(mode==LASSOSELECT){           // FIXME interpret control and shift for selection changes
      new_x=FXCLAMP(0,event->win_x,(width-1));
      new_y=FXCLAMP(0,event->win_y,(height-1));
      drawLasso(event->click_x,event->click_y,new_x,new_y);
      handle(this,FXSEL(SEL_LASSOED,0),ptr);
      setOp(HOVERING);
      }
    else if(mode==PICKING){               // FIXME interpret control and shift for selection changes
      setOp(HOVERING);
      if(!handle(this,FXSEL(SEL_PICKED,0),ptr)){
        objects[0]=pick(event->click_x,event->click_y);
        objects[1]=NULL;
        handle(this,FXSEL(SEL_CHANGED,0),(void*)objects[0]);        // FIXME want multiple objects
        handle(this,FXSEL(SEL_SELECTED,0),(void*)objects);
        }
      }
    else if(mode==DRAGGING){
      if(target) target->tryHandle(this,FXSEL(SEL_DRAGGED,message),selection);
      setOp(HOVERING);
      }
    else{
      setOp(HOVERING);
      }
    if(event->click_count==1){
      handle(this,FXSEL(SEL_CLICKED,0),(void*)selection);
      }
    else if(event->click_count==2){
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)selection);
      }
    else if(event->click_count==3){
      handle(this,FXSEL(SEL_TRIPLECLICKED,0),(void*)selection);
      }
    }
  return 1;
  }


// Pressed middle mouse button
long FXGLViewer::onMiddleBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  FXTRACE((100,"onMiddleBtnPress Mask=%08x\n",event->state));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONPRESS,message),ptr)) return 1;
    if(event->state&SHIFTMASK){
      setOp(TRUCKING);
      }
    else{
      setOp(ZOOMING);
      }
    }
  return 1;
  }


// Released middle mouse button
long FXGLViewer::onMiddleBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXTRACE((100,"onMiddleBtnRelease Mask=%08x\n",event->state));
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONRELEASE,message),ptr)) return 1;
    if(event->state&LEFTBUTTONMASK){
      setOp(ROTATING);
      grab();
      }
    else if(event->state&RIGHTBUTTONMASK){
      if(event->state&SHIFTMASK){
        setOp(GYRATING);
        }
      else if(event->state&CONTROLMASK){
        setOp(FOVING);
        }
      else{
        setOp(TRANSLATING);
        }
      grab();
      }
    else{
      setOp(HOVERING);
      }
    }
  return 1;
  }


// Pressed right button
long FXGLViewer::onRightBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  FXTRACE((100,"onRightBtnPress Mask=%08x\n",event->state));
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    flags&=~FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONPRESS,message),ptr)) return 1;
    if(event->state&LEFTBUTTONMASK){
      if(event->state&SHIFTMASK){
        setOp(TRUCKING);
        }
      else{
        setOp(ZOOMING);
        }
      }
    else if(event->state&MIDDLEBUTTONMASK){
      if(event->state&SHIFTMASK){
        setOp(GYRATING);
        }
      else if(event->state&CONTROLMASK){
        setOp(FOVING);
        }
      else{
        setOp(TRANSLATING);
        }
      }
    else{
      if(event->state&SHIFTMASK){
        setOp(GYRATING);
        }
      else if(event->state&CONTROLMASK){
        setOp(FOVING);
        }
      else{
        setOp(POSTING);
        }
      }
    }
  return 1;
  }


// Microsoft Visual C++: Disable compiler warnings for empty "if"
// statements below in FXGLViewer::onRightBtnRelease()
#ifdef _MSC_VER
#pragma warning( disable : 4390 )
#endif


// Released right button
long FXGLViewer::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXGLObject *hit;
  FXTRACE((100,"onRightBtnRelease Mask=%08x\n",event->state));
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONRELEASE,message),ptr)) return 1;
    if(event->state&LEFTBUTTONMASK){
      setOp(ROTATING);
      grab();
      }
    else if(event->state&MIDDLEBUTTONMASK){
      if(event->state&SHIFTMASK){
        setOp(TRUCKING);
        }
      else{
        setOp(ZOOMING);
        }
      grab();
      }
    else{
      if(mode==POSTING){
        setOp(HOVERING);
        hit=pick(event->click_x,event->click_y);
        if(hit && hit->handle(this,FXSEL(SEL_COMMAND,ID_QUERY_MENU),ptr))
          ;
        else if(target && target->tryHandle(this,FXSEL(SEL_COMMAND,ID_QUERY_MENU),ptr))
          ;
        }
      setOp(HOVERING);
      }
    }
  return 1;
  }


// Mouse moved
long FXGLViewer::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint new_x,new_y,old_x,old_y;
  long changed=(flags&FLAG_TIP)!=0;
  FXdouble delta;
  FXfloat tmp;
  FXVec3f vec;
  FXQuatf q;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_MOTION,message),ptr)) return 1;
    getApp()->removeTimeout(this,ID_TIPTIMER);
    switch(mode){
      case HOVERING:            // Reset the timer each time we moved the cursor
        getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
        break;
      case PICKING:             // Picking
        if(!event->moved){                              // Keep picking mode for now
          break;
          }
        if(event->state&(SHIFTMASK|CONTROLMASK)){       // Lasso mode if modifier held down
          drawLasso(event->click_x,event->click_y,event->win_x,event->win_y);
          setOp(LASSOSELECT);
          break;
          }
        setOp(ROTATING);                                // Go into rotation mode
      case ROTATING:            // Rotating camera around target
        q=turn(event->last_x,event->last_y,event->win_x,event->win_y) * getOrientation();
        setOrientation(q);
        changed=1;
        break;
      case POSTING:             // Posting right-mouse menu; if moving more than delta, we go to translate mode
        if(!event->moved) break;
        setOp(TRANSLATING);
      case TRANSLATING:         // Translating camera
        vec=worldVector(event->last_x,event->last_y,event->win_x,event->win_y);
        translate(-vec);
        changed=1;
        break;
      case ZOOMING:             // Zooming camera
        delta=0.005*(event->win_y-event->last_y);
        setZoom(getZoom()*pow(2.0,delta));
        changed=1;
        break;
      case FOVING:              // Change FOV
        setFieldOfView(getFieldOfView()+90.0*(event->win_y-event->last_y)/(double)wvt.h);
        changed=1;
        break;
      case DRAGGING:            // Dragging a shape
        if(selection && selection->drag(this,event->last_x,event->last_y,event->win_x,event->win_y)){
          //// Perhaps callback here for the target to be notified of the new object position
          update();
          }
        changed=1;
        break;
      case TRUCKING:            // Trucking camera forward or backward
        tmp=(FXfloat)(worldpx*(event->win_y-event->last_y));
        vec=normalize(getEyeVector());
        translate(tmp*vec);
        changed=1;
        break;
      case GYRATING:            // Rotating camera around eye
        {
          FXMat4f mm;
          FXQuatf qq;
          qq=turn(event->win_x,event->win_y,event->last_x,event->last_y);
          mm.eye();
          mm.trans(0.0f,0.0f,(FXfloat)-distance); // FIXME This aint it yet...
          mm.rot(qq);
          mm.trans(0.0f,0.0f,(FXfloat)distance);
          center=center*mm;
          q=qq * getOrientation();
          setOrientation(q);
          update();
          changed=1;
        }
        break;
      case LASSOSELECT:         // Dragging a lasso
      case LASSOZOOM:
        old_x=FXCLAMP(0,event->last_x,(width-1));
        old_y=FXCLAMP(0,event->last_y,(height-1));
        new_x=FXCLAMP(0,event->win_x,(width-1));
        new_y=FXCLAMP(0,event->win_y,(height-1));
        drawLasso(event->click_x,event->click_y,old_x,old_y);
        drawLasso(event->click_x,event->click_y,new_x,new_y);
        if(new_x>event->click_x){
          if(new_y>event->click_y)
            setDragCursor(getApp()->getDefaultCursor(DEF_CORNERSE_CURSOR));
          else
            setDragCursor(getApp()->getDefaultCursor(DEF_CORNERNE_CURSOR));
          }
        else{
          if(new_y>event->click_y)
            setDragCursor(getApp()->getDefaultCursor(DEF_CORNERSW_CURSOR));
          else
            setDragCursor(getApp()->getDefaultCursor(DEF_CORNERNW_CURSOR));
          }
        changed=1;
        break;
      }
    }
  return changed;
  }


// Mouse wheel
long FXGLViewer::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_MOUSEWHEEL,message),ptr)) return 1;
    setZoom(getZoom()*pow(2.0,-0.1*event->code/120.0));
    return 1;
    }
  return 0;
  }


// Handle keyboard press/release
long FXGLViewer::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    switch(event->code){
      case KEY_Shift_L:
      case KEY_Shift_R:

        // We do not switch modes unless something was going on already
        if(mode!=HOVERING){
          if((event->state&MIDDLEBUTTONMASK) || ((event->state&LEFTBUTTONMASK) && (event->state&RIGHTBUTTONMASK))){
            setOp(TRUCKING);
            }
          else if(event->state&RIGHTBUTTONMASK){
            setOp(GYRATING);
            }
          }
        return 1;
      case KEY_Control_L:
      case KEY_Control_R:

        // We do not switch modes unless something was going on already
        if(mode!=HOVERING){
          if(event->state&RIGHTBUTTONMASK){
            setOp(FOVING);
            }
          }
        return 1;
      }
    }
  return 0;
  }


// Key release
long FXGLViewer::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_Shift_L:
      case KEY_Shift_R:

        // We do not switch modes unless something was going on already
        if(mode!=HOVERING){
          if((event->state&MIDDLEBUTTONMASK) || ((event->state&LEFTBUTTONMASK) && (event->state&RIGHTBUTTONMASK))){
            setOp(ZOOMING);
            }
          else if(event->state&RIGHTBUTTONMASK){
            setOp(TRANSLATING);
            }
          }
        return 1;
      case KEY_Control_L:
      case KEY_Control_R:

        // We do not switch modes unless something was going on already
        if(mode!=HOVERING){
          if(event->state&RIGHTBUTTONMASK){
            setOp(TRANSLATING);
            }
          }
        return 1;
      }
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXGLViewer::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onUngrabbed(sender,sel,ptr);
  flags&=~FLAG_PRESSED;
  flags&=~FLAG_CHANGED;
  flags|=FLAG_UPDATE;
  setOp(HOVERING);
  doesturbo=FALSE;
  return 1;
  }


// We timed out, i.e. the user didn't move for a while
long FXGLViewer::onTipTimer(FXObject*,FXSelector,void*){
  FXTRACE((250,"%s::onTipTimer %p\n",getClassName(),this));
  flags|=FLAG_TIP;
  return 1;
  }


// We were asked about status text
long FXGLViewer::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// We were asked about tip text
long FXGLViewer::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if(flags&FLAG_TIP){
    FXint x,y; FXuint state;
    getCursorPosition(x,y,state);
    FXGLObject *hit=pick(x,y);
    if(hit && hit->handle(sender,sel,ptr)) return 1;
    if(!tip.empty()){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
      return 1;
      }
    }
  return 0;
  }


/*****************************  Switch Projection  *****************************/


// Switch to perspective mode
long FXGLViewer::onCmdPerspective(FXObject*,FXSelector,void*){
  setProjection(PERSPECTIVE);
  return 1;
  }


// Update sender
long FXGLViewer::onUpdPerspective(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(projection==PERSPECTIVE)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Switch to parallel mode
long FXGLViewer::onCmdParallel(FXObject*,FXSelector,void*){
  setProjection(PARALLEL);
  return 1;
  }


// Update sender
long FXGLViewer::onUpdParallel(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(projection==PARALLEL)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


/*****************************  Switch Viewpoints  *****************************/

// View front
long FXGLViewer::onCmdFront(FXObject*,FXSelector,void*){
  rotation=FXQuatf(0.0f,0.0f,0.0f,1.0f);
  updateTransform();
  update();
  return 1;
  }


// Update sender
long FXGLViewer::onUpdFront(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(EPS>fabs(rotation[0]) && EPS>fabs(rotation[1]) && EPS>fabs(rotation[2]) && EPS>fabs(rotation[3]-1.0)) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// View back
long FXGLViewer::onCmdBack(FXObject*,FXSelector,void*){
  rotation=FXQuatf(0.0f,-1.0f,0.0f,0.0f);
  updateTransform();
  update();
  return 1;
  }


// Update sender
long FXGLViewer::onUpdBack(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(EPS>fabs(rotation[0]) && EPS>fabs(rotation[1]+1.0) && EPS>fabs(rotation[2]) && EPS>fabs(rotation[3])) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// View left
long FXGLViewer::onCmdLeft(FXObject*,FXSelector,void*){
  rotation=FXQuatf(0.0f,0.7071067811865f,0.0f,0.7071067811865f);
  updateTransform();
  update();
  return 1;
  }


// Update sender
long FXGLViewer::onUpdLeft(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(EPS>fabs(rotation[0]) && EPS>fabs(rotation[1]-0.7071067811865) && EPS>fabs(rotation[2]) && EPS>fabs(rotation[3]-0.7071067811865)) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// View right
long FXGLViewer::onCmdRight(FXObject*,FXSelector,void*){
  rotation=FXQuatf(0.0f,-0.7071067811865f,0.0f,0.7071067811865f);
  updateTransform();
  update();
  return 1;
  }


// Update sender
long FXGLViewer::onUpdRight(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(EPS>fabs(rotation[0]) && EPS>fabs(rotation[1]+0.7071067811865) && EPS>fabs(rotation[2]) && EPS>fabs(rotation[3]-0.7071067811865)) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// View top
long FXGLViewer::onCmdTop(FXObject*,FXSelector,void*){
  rotation=FXQuatf(0.7071067811865f,0.0f,0.0f,0.7071067811865f);
  updateTransform();
  update();
  return 1;
  }


// Update sender
long FXGLViewer::onUpdTop(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(EPS>fabs(rotation[0]-0.7071067811865) && EPS>fabs(rotation[1]) && EPS>fabs(rotation[2]) && EPS>fabs(rotation[3]-0.7071067811865)) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// View bottom
long FXGLViewer::onCmdBottom(FXObject*,FXSelector,void*){
  rotation=FXQuatf(-0.7071067811865f,0.0f,0.0f,0.7071067811865f);
  updateTransform();
  update();
  return 1;
  }


// Update sender
long FXGLViewer::onUpdBottom(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(EPS>fabs(rotation[0]+0.7071067811865) && EPS>fabs(rotation[1]) && EPS>fabs(rotation[2]) && EPS>fabs(rotation[3]-0.7071067811865)) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Reset view
long FXGLViewer::onCmdResetView(FXObject*,FXSelector,void*){
  FXRangef r(-1.0f,1.0f,-1.0f,1.0f,-1.0f,1.0f);
  rotation=FXQuatf(0.0f,0.0f,0.0f,1.0f);
  zoom=1.0;
  scale=FXVec3f(1.0f,1.0f,1.0f);
  if(scene) scene->bounds(r);
  setBounds(r);
  updateProjection();
  updateTransform();
  update();
  return 1;
  }


// Fit view
long FXGLViewer::onCmdFitView(FXObject*,FXSelector,void*){
  FXRangef r(-1.0f,1.0f,-1.0f,1.0f,-1.0f,1.0f);
  if(scene) scene->bounds(r);
  setBounds(r);
  update();
  return 1;
  }


// Update zoom
long FXGLViewer::onUpdZoom(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETREALVALUE),(void*)&zoom);
  return 1;
  }


// Change zoom
long FXGLViewer::onCmdZoom(FXObject* sender,FXSelector sel,void*){
  FXdouble z=zoom;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETREALVALUE),(void*)&z);
  doesturbo=(FXSELTYPE(sel)==SEL_CHANGED)?turbomode:FALSE;
  setZoom(z);
  return 1;
  }


// Update field of view
long FXGLViewer::onUpdFov(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETREALVALUE),(void*)&fov);
  return 1;
  }


// Change field of view
long FXGLViewer::onCmdFov(FXObject* sender,FXSelector sel,void*){
  FXdouble f=fov;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETREALVALUE),(void*)&f);
  doesturbo=(FXSELTYPE(sel)==SEL_CHANGED)?turbomode:FALSE;
  setFieldOfView(f);
  return 1;
  }


// Scale model
long FXGLViewer::onCmdXYZScale(FXObject* sender,FXSelector sel,void*){
  FXVec3f s=scale;
  FXdouble value;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETREALVALUE),&value);
  s[FXSELID(sel)-ID_SCALE_X]=(FXfloat)value;
  doesturbo=(FXSELTYPE(sel)==SEL_CHANGED)?turbomode:FALSE;
  setScale(s);
  return 1;
  }


// Update scale value
long FXGLViewer::onUpdXYZScale(FXObject* sender,FXSelector sel,void*){
  FXdouble value=scale[FXSELID(sel)-ID_SCALE_X];
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETREALVALUE),(void*)&value);
  return 1;
  }


// Rotate camera about model by means of dials
long FXGLViewer::onCmdXYZDial(FXObject*,FXSelector sel,void* ptr){
  const FXVec3f xaxis(1.0f,0.0f,0.0f);
  const FXVec3f yaxis(0.0f,1.0f,0.0f);
  const FXVec3f zaxis(0.0f,0.0f,1.0f);
  FXint dialnew=(FXint)(FXival)ptr;
  FXfloat ang;
  FXQuatf q;
  if(FXSELTYPE(sel)==SEL_CHANGED){
    doesturbo=turbomode;
    FXASSERT(ID_DIAL_X<=FXSELID(sel) && FXSELID(sel)<=ID_DIAL_Z);
    switch(FXSELID(sel)){
      case ID_DIAL_X:
        ang=(FXfloat)(DTOR*(dialnew-dial[0]));
        q.setAxisAngle(xaxis,-ang);
        dial[0]=dialnew;
        break;
      case ID_DIAL_Y:
        ang=(FXfloat)(DTOR*(dialnew-dial[1]));
        q.setAxisAngle(yaxis, ang);
        dial[1]=dialnew;
        break;
      case ID_DIAL_Z:
        ang=(FXfloat)(DTOR*(dialnew-dial[2]));
        q.setAxisAngle(zaxis, ang);
        dial[2]=dialnew;
        break;
      }
    setOrientation(q*getOrientation());
    }
  else if(doesturbo){
    doesturbo=FALSE;
    update();
    }
  return 1;
  }


// Update dial value
long FXGLViewer::onUpdXYZDial(FXObject* sender,FXSelector sel,void*){
  FXASSERT(ID_DIAL_X<=FXSELID(sel) && FXSELID(sel)<=ID_DIAL_Z);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&dial[FXSELID(sel)-ID_DIAL_X]);
  return 1;
  }


// Update roll pitch yaw
long FXGLViewer::onCmdRollPitchYaw(FXObject* sender,FXSelector sel,void*){
  FXASSERT(ID_ROLL<=FXSELID(sel) && FXSELID(sel)<=ID_YAW);
  FXfloat rpy[3];
  FXdouble ang;
  rotation.getRollPitchYaw(rpy[0],rpy[1],rpy[2]);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETREALVALUE),(void*)&ang);
  rpy[FXSELID(sel)-ID_ROLL]=(FXfloat)(DTOR*ang);
  doesturbo=(FXSELTYPE(sel)==SEL_CHANGED)?turbomode:FALSE;
  setOrientation(FXQuatf(rpy[0],rpy[1],rpy[2]));
  update();
  return 1;
  }


// Update roll pitch yaw
long FXGLViewer::onUpdRollPitchYaw(FXObject* sender,FXSelector sel,void*){
  FXASSERT(ID_ROLL<=FXSELID(sel) && FXSELID(sel)<=ID_YAW);
  FXfloat rpy[3];
  rotation.getRollPitchYaw(rpy[0],rpy[1],rpy[2]);
  FXdouble ang=RTOD*rpy[FXSELID(sel)-ID_ROLL];
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETREALVALUE),(void*)&ang);
  return 1;
  }


/******************************  Printing Support  *****************************/


// Read back pixels
// Derived from code contributed by <sancelot@crosswinds.net>
FXbool FXGLViewer::readPixels(FXColor*& buffer,FXint x,FXint y,FXint w,FXint h){
#ifdef HAVE_GL_H
  if(1<=w && 1<=h){
    GLint swapbytes,lsbfirst,rowlength,skiprows,skippixels,alignment,oldbuf;
    register FXColor *p,*q,*pp,*qq,t;

    // Try allocate buffer
    if(FXMALLOC(&buffer,FXColor,w*h)){

      // Make context current
      makeCurrent();

      // Save old pixel formats
      glGetIntegerv(GL_PACK_SWAP_BYTES,&swapbytes);
      glGetIntegerv(GL_PACK_LSB_FIRST,&lsbfirst);
      glGetIntegerv(GL_PACK_ROW_LENGTH,&rowlength);
      glGetIntegerv(GL_PACK_SKIP_ROWS,&skiprows);
      glGetIntegerv(GL_PACK_SKIP_PIXELS,&skippixels);
      glGetIntegerv(GL_PACK_ALIGNMENT,&alignment);
      glGetIntegerv(GL_READ_BUFFER,&oldbuf);

      // Set pixel readback formats
      glPixelStorei(GL_PACK_SWAP_BYTES,GL_FALSE);
      glPixelStorei(GL_PACK_LSB_FIRST,GL_FALSE);
      glPixelStorei(GL_PACK_ROW_LENGTH,0);
      glPixelStorei(GL_PACK_SKIP_ROWS,0);
      glPixelStorei(GL_PACK_SKIP_PIXELS,0);
      glPixelStorei(GL_PACK_ALIGNMENT,1);

      // Read from the right buffer
      glReadBuffer((GLenum)GL_FRONT);

      // Read the pixels
      glReadPixels(x,y,w,h,GL_RGBA,GL_UNSIGNED_BYTE,(GLvoid*)buffer);

      // Flip image upside down
      pp=buffer;
      qq=buffer+(h-1)*w;
      do{
        p=pp; pp+=w;
        q=qq; qq-=w;
        do{
          FXSWAP(*p,*q,t);
          p++;
          q++;
          }
        while(p<pp);
        }
      while(pp<qq);

      // Restore old formats
      glPixelStorei(GL_PACK_SWAP_BYTES,swapbytes);
      glPixelStorei(GL_PACK_LSB_FIRST,lsbfirst);
      glPixelStorei(GL_PACK_ROW_LENGTH,rowlength);
      glPixelStorei(GL_PACK_SKIP_ROWS,skiprows);
      glPixelStorei(GL_PACK_SKIP_PIXELS,skippixels);
      glPixelStorei(GL_PACK_ALIGNMENT,alignment);
      glReadBuffer((GLenum)oldbuf);

      // Make context non-current
      makeNonCurrent();
      return TRUE;
      }
    }
#endif
  return FALSE;
  }


// Print the window by grabbing pixels
long FXGLViewer::onCmdPrintImage(FXObject*,FXSelector,void*){
  FXColor *buffer;

  // First, ensure window is fully painted
  repaint();
  getApp()->flush(TRUE);

  // Then try grab the pixels
  if(readPixels(buffer,0,0,width,height)){
//    FXFileStream outfile;
//    if(outfile.open("testje.bmp",FXStreamSave)){
//      fxsaveBMP(outfile,buffer,width,height);
//      outfile.close();
//      }

    // Open print dialog
    FXPrintDialog dlg(this,tr("Print Scene"));

    // Run dialog
    if(dlg.execute()){
      FXPrinter printer;

      // Get the printer
      dlg.getPrinter(printer);

      // Printer device context
      FXDCPrint pdc(getApp());

      // Try open printer
      if(!pdc.beginPrint(printer)){
        FXMessageBox::error(this,MBOX_OK,tr("Printer Error"),tr("Unable to print."));
        return 1;
        }

      // Page header
      pdc.beginPage(1);

      // This is very ad-hoc; please don't look
      pdc.outf("/picstr %d string def\n",width*3);
      pdc.outf("%d %d translate\n",50,50);
      pdc.outf("%d %d scale\n",width,height);
      pdc.outf("%d %d %d\n",width,height,8);
      pdc.outf("[%d 0 0 -%d 0 %d]\n",width,height,height);
      pdc.outf("{currentfile picstr readhexstring pop}\n");
      pdc.outf("false %d\n",3);
      pdc.outf("colorimage\n");
      for(int i=0; i<width*height; i++){
        pdc.outhex(FXREDVAL(buffer[i]));
        pdc.outhex(FXGREENVAL(buffer[i]));
        pdc.outhex(FXBLUEVAL(buffer[i]));
        }
      pdc.outf("\n");

      // Page trailer
      pdc.endPage();
      pdc.endPrint();
      }

    // Free the pixels
    FXFREE(&buffer);
    }
  return 1;
  }


// Render
FXint FXGLViewer::renderFeedback(FXfloat *buffer,FXint x,FXint y,FXint w,FXint h,FXint maxbuffer){
#ifdef HAVE_GL_H
  FXint used;
  makeCurrent();
  glFeedbackBuffer(maxbuffer,GL_3D_COLOR,buffer);
  glRenderMode(GL_FEEDBACK);
  drawWorld(wvt);
  used=glRenderMode(GL_RENDER);
  makeNonCurrent();
  return used;
#else
  return -1;
#endif
  }


// Read feedback buffer
FXbool FXGLViewer::readFeedback(FXfloat*& buffer,FXint& used,FXint& size,FXint x,FXint y,FXint w,FXint h){
  FXbool ok=FALSE;
  buffer=NULL;
  used=0;
  size=10000;
  while(1){

    // Allocate buffer
    FXMALLOC(&buffer,FXfloat,size);

    // It got too big, give up
    if(!buffer) break;

    // Try to render scene into it
    used=renderFeedback(buffer,x,y,w,h,size);

    // No errors, got our stuff
    if(0<used){
      ok=TRUE;
      break;
      }

    // It didn't fit, lets double the buffer and try again
    FXFREE(&buffer);
    size*=2;
    continue;
    }
  return ok;
  }


// Draw feedback buffer into dc
void FXGLViewer::drawFeedback(FXDCPrint& pdc,const FXfloat* buffer,FXint used){
#ifdef HAVE_GL_H
  FXint nvertices,smooth,token,i,p;

  // Draw background
  pdc.outf("%g %g %g C\n",background[0][0],background[0][1],background[0][2]);
  pdc.outf("newpath\n");
  pdc.outf("%g %g moveto\n",0.0,0.0);
  pdc.outf("%g %g lineto\n",0.0,(double)height);
  pdc.outf("%g %g lineto\n",(double)width,(double)height);
  pdc.outf("%g %g lineto\n",(double)width,0.0);
  pdc.outf("closepath fill\n");

  pdc.outf("1 setlinewidth\n");

  // Crank out primitives
  p=0;
  while(p<used){
    token=(FXint)buffer[p++];
    switch(token){

      // Point primitive
      case GL_POINT_TOKEN:
        pdc.outf("%g %g %g %g %g P\n",buffer[p+0],buffer[p+1],buffer[p+3],buffer[p+4],buffer[p+5]);
        p+=7;             // Each vertex element in the feedback buffer is 7 floats
        break;

      // Line primitive
      case GL_LINE_RESET_TOKEN:
      case GL_LINE_TOKEN:
        if(fabs(buffer[p+3]-buffer[p+7+3])<1E-4 || fabs(buffer[p+4]-buffer[p+7+4])<1E-4 || fabs(buffer[p+5]-buffer[p+7+5])<1E-4){
          pdc.outf("%g %g %g %g %g %g %g %g %g %g SL\n",buffer[p+0],buffer[p+1],buffer[p+3],buffer[p+4],buffer[p+5], buffer[p+7+0],buffer[p+7+1],buffer[p+7+3],buffer[p+7+4],buffer[p+7+5]);
          }
        else{
          pdc.outf("%g %g %g %g %g %g %g L\n",buffer[p+0],buffer[p+1],buffer[p+7+0],buffer[p+7+1],buffer[p+3],buffer[p+4],buffer[p+5]);
          }
        p+=14;            // Each vertex element in the feedback buffer is 7 GLfloats
        break;

      // Polygon primitive
      case GL_POLYGON_TOKEN:
        nvertices = (FXint)buffer[p++];
        if(nvertices==3){ // We assume polybusting has taken place already!
          smooth=0;
          for(i=1; i<nvertices; i++){
            if(fabs(buffer[p+3]-buffer[p+i*7+3])<1E-4 || fabs(buffer[p+4]-buffer[p+i*7+4])<1E-4 || fabs(buffer[p+5]-buffer[p+i*7+5])<1E-4){ smooth=1; break; }
            }
          if(smooth){
            pdc.outf("%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g ST\n",buffer[p+0],buffer[p+1],buffer[p+3],buffer[p+4],buffer[p+5], buffer[p+7+0],buffer[p+7+1],buffer[p+7+3],buffer[p+7+4],buffer[p+7+5], buffer[p+14+0],buffer[p+14+1],buffer[p+14+3],buffer[p+14+4],buffer[p+14+5]);
            }
          else{
            pdc.outf("%g %g %g %g %g %g %g %g %g T\n",buffer[p+0],buffer[p+1], buffer[p+7+0],buffer[p+7+1], buffer[p+14+0],buffer[p+14+1], buffer[p+3],buffer[p+4],buffer[p+5]);
            }
          }
        p+=nvertices*7;   // Each vertex element in the feedback buffer is 7 GLfloats
        break;

      // Skip these, don't deal with it here
      case GL_BITMAP_TOKEN:
      case GL_DRAW_PIXEL_TOKEN:
      case GL_COPY_PIXEL_TOKEN:
        p+=7;
        break;

      // Skip passthrough tokens
      case GL_PASS_THROUGH_TOKEN:
        p++;
        break;

      // Bad token, this is the end
      default:
        return;
      }
    }
#endif
  }


// Print the window by means of feedback buffer
long FXGLViewer::onCmdPrintVector(FXObject*,FXSelector,void*){
  FXPrintDialog dlg(this,tr("Print Scene"));
  FXPrinter printer;
  FXfloat *buffer;
  FXint used,size;

  // Run dialog
  if(dlg.execute()){
    dlg.getPrinter(printer);
    FXDCPrint pdc(getApp());
    if(!pdc.beginPrint(printer)){
      FXMessageBox::error(this,MBOX_OK,tr("Printer Error"),tr("Unable to print."));
      return 1;
      }

    // Repaint now
    repaint();

    // Flush commands
    getApp()->flush(TRUE);

    // Page header
    pdc.beginPage(1);

    // Read feedback
    if(readFeedback(buffer,used,size,0,0,width,height)){
      if(zsortfunc) (*zsortfunc)(buffer,used,size);   // FIXME:- may throw exception
      drawFeedback(pdc,buffer,used);
      }

    // Page trailer
    pdc.endPage();
    pdc.endPrint();
    }
  return 1;
  }


// Zoom into lasso rectangle
long FXGLViewer::onCmdLassoZoom(FXObject*,FXSelector,void*){
  setOp(DO_LASSOZOOM);
  return 1;
  }

// Select objects in lasso rectangle
long FXGLViewer::onCmdLassoSelect(FXObject*,FXSelector,void*){
  setOp(DO_LASSOSELECT);
  return 1;
  }

/*****************************  Selection Support  *****************************/

// We now really do have the selection
long FXGLViewer::onClipboardGained(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onClipboardGained(sender,sel,ptr);
  return 1;
  }


// We lost the selection somehow
long FXGLViewer::onClipboardLost(FXObject* sender,FXSelector sel,void* ptr){
  FXGLCanvas::onClipboardLost(sender,sel,ptr);
  return 1;
  }


// Somebody wants our selection
long FXGLViewer::onClipboardRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
//  FXuchar *data; FXuint len;

  // Try handling it in base class first
  if(FXGLCanvas::onClipboardRequest(sender,sel,ptr)) return 1;

  // Requested data from clipboard
  if(event->target==objectType){
    FXTRACE((100,"requested objectType\n"));
//    FXMemoryStream stream;
//    stream.open(NULL,0,FXStreamSave);
//    stream.takeBuffer(data,len);
//    stream.close();
//    setDNDData(FROM_CLIPBOARD,objectType,data,len);
    return 1;
    }

  return 0;
  }

// Cut selected object
long FXGLViewer::onCmdCutSel(FXObject*,FXSelector,void*){
  // Serialize object into temp buffer
  // Delete object, tell target it was deleted
  //fxwarning("%s::onCmdCutSel: unimplemented.\n",getClassName());
  return 1;
  }


// Copy selected object
long FXGLViewer::onCmdCopySel(FXObject*,FXSelector,void*){
  // Serialize object into buffer
  //fxwarning("%s::onCmdCopySel: unimplemented.\n",getClassName());
  return 1;
  }


// Paste object
long FXGLViewer::onCmdPasteSel(FXObject*,FXSelector,void*){
  // Ask clipboard for object data [What type?]
  // Deserialize data [type?]
  // Tell target about the data?
  //fxwarning("%s::onCmdPasteSel: unimplemented.\n",getClassName());
  return 1;
  }


// Delete selected object
long FXGLViewer::onCmdDeleteSel(FXObject*,FXSelector,void*){
  FXGLObject *obj[2];
  obj[0]=selection;
  obj[1]=NULL;
  if(obj[0] && obj[0]->canDelete()){
    handle(this,FXSEL(SEL_CHANGED,0),NULL);
    handle(this,FXSEL(SEL_DELETED,0),(void*)obj);
    //delete obj[0];
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Update delete object
long FXGLViewer::onUpdDeleteSel(FXObject* sender,FXSelector,void*){
  if(selection && selection->canDelete()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    return 1;
    }
  return 0;
  }


// Update for current object
long FXGLViewer::onUpdCurrent(FXObject* sender,FXSelector,void*){
  if(selection){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    return 1;
    }
  return 0;
  }


// Set background color
long FXGLViewer::onCmdBackColor(FXObject*,FXSelector sel,void* ptr){
  FXColor color=(FXColor)(FXuval)ptr;
  background[0]=background[1]=color;
  if(FXSELTYPE(sel)==SEL_COMMAND || !turbomode){
    update();
    }
  return 1;
  }


// Update background color
long FXGLViewer::onUpdBackColor(FXObject* sender,FXSelector,void*){
  FXColor clr=background[0];
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETVALUE),(void*)(FXuval)clr);
  return 1;
  }


// Set gradient background color
long FXGLViewer::onCmdGradientBackColor(FXObject*,FXSelector sel,void* ptr){
  FXColor color=(FXColor)(FXuval)ptr;
  background[FXSELID(sel)-ID_TOP_COLOR]=color;
  if(FXSELTYPE(sel)==SEL_COMMAND || !turbomode){
    update();
    }
  return 1;
  }


// Update gradient background color
long FXGLViewer::onUpdGradientBackColor(FXObject* sender,FXSelector sel,void*){
  FXColor clr=background[FXSELID(sel)-ID_TOP_COLOR];
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETVALUE),(void*)(FXuval)clr);
  return 1;
  }


// Set ambient light color
long FXGLViewer::onCmdAmbientColor(FXObject*,FXSelector sel,void* ptr){
  FXColor color=(FXColor)(FXuval)ptr;
  ambient=color;
  if(FXSELTYPE(sel)==SEL_COMMAND || !turbomode){
    update();
    }
  return 1;
  }


// Update ambient light color
long FXGLViewer::onUpdAmbientColor(FXObject* sender,FXSelector,void*){
  FXColor clr=ambient;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETVALUE),(void*)(FXuval)clr);
  return 1;
  }


// Set ambient light color
long FXGLViewer::onCmdLightAmbient(FXObject*,FXSelector sel,void* ptr){
  FXColor color=(FXColor)(FXuval)ptr;
  light.ambient=color;
  if(FXSELTYPE(sel)==SEL_COMMAND || !turbomode){
    update();
    }
  return 1;
  }


// Update ambient light color
long FXGLViewer::onUpdLightAmbient(FXObject* sender,FXSelector,void*){
  FXColor clr=light.ambient;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETVALUE),(void*)(FXuval)clr);
  return 1;
  }


// Set diffuse light color
long FXGLViewer::onCmdLightDiffuse(FXObject*,FXSelector sel,void* ptr){
  FXColor color=(FXColor)(FXuval)ptr;
  light.diffuse=color;
  if(FXSELTYPE(sel)==SEL_COMMAND || !turbomode){
    update();
    }
  return 1;
  }


// Update diffuse light color
long FXGLViewer::onUpdLightDiffuse(FXObject* sender,FXSelector,void*){
  FXColor clr=light.diffuse;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETVALUE),(void*)(FXuval)clr);
  return 1;
  }


// Set specular light color
long FXGLViewer::onCmdLightSpecular(FXObject*,FXSelector sel,void* ptr){
  FXColor color=(FXColor)(FXuval)ptr;
  light.specular=color;
  if(FXSELTYPE(sel)==SEL_COMMAND || !turbomode){
    update();
    }
  return 1;
  }


// Update specular light color
long FXGLViewer::onUpdLightSpecular(FXObject* sender,FXSelector,void*){
  FXColor clr=light.specular;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETVALUE),(void*)(FXuval)clr);
  return 1;
  }


// Toggle Turbo Mode
long FXGLViewer::onCmdTurbo(FXObject*,FXSelector,void*){
  setTurboMode(!getTurboMode());
  return 1;
  }


// Update Turbo Mode
long FXGLViewer::onUpdTurbo(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,getTurboMode() ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle lighting
long FXGLViewer::onCmdLighting(FXObject*,FXSelector,void*){
  options^=VIEWER_LIGHTING;
  update();
  return 1;
  }


// Update lighting
long FXGLViewer::onUpdLighting(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(options&VIEWER_LIGHTING) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle fog
long FXGLViewer::onCmdFog(FXObject*,FXSelector,void*){
  options^=VIEWER_FOG;
  update();
  return 1;
  }


// Update fog
long FXGLViewer::onUpdFog(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(options&VIEWER_FOG) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle dithering
long FXGLViewer::onCmdDither(FXObject*,FXSelector,void*){
  options^=VIEWER_DITHER;
  update();
  return 1;
  }


// Update dithering
long FXGLViewer::onUpdDither(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  sender->handle(this,(options&VIEWER_DITHER) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


/*******************************  Drag and Drop  *******************************/


// Handle drag-and-drop enter
long FXGLViewer::onDNDEnter(FXObject* sender,FXSelector sel,void* ptr){
  if(FXGLCanvas::onDNDEnter(sender,sel,ptr)) return 1;
  dropped=NULL;
  return 1;
  }

// Handle drag-and-drop leave
long FXGLViewer::onDNDLeave(FXObject* sender,FXSelector sel,void* ptr){
  if(FXGLCanvas::onDNDLeave(sender,sel,ptr)) return 1;
  dropped=NULL;
  return 1;
  }


// Handle drag-and-drop motion
long FXGLViewer::onDNDMotion(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;

  // Handled elsewhere
  if(FXGLCanvas::onDNDMotion(sender,sel,ptr)) return 1;

  // Dropped on some object
  if((dropped=pick(event->win_x,event->win_y))!=NULL){

    // Object agrees with drop type
    if(dropped->handle(this,sel,ptr)){
      acceptDrop(DRAG_COPY);
      return 1;
      }

    // Forget about the whole thing
    dropped=NULL;
    return 0;
    }

  // Dropped in viewer background; hope its a color
  if(offeredDNDType(FROM_DRAGNDROP,colorType)){
    acceptDrop(DRAG_COPY);
    return 1;
    }

  // Won't accept drop, dont know what it is
  return 0;
  }


// Handle drag-and-drop drop
long FXGLViewer::onDNDDrop(FXObject* sender,FXSelector sel,void* ptr){
  FXushort *clr; FXuint len;

  // Try base class first
  if(FXGLCanvas::onDNDDrop(sender,sel,ptr)) return 1;

  // Dropped on object?
  if(dropped){

    // Object handled drop; so probably want to repaint
    if(dropped->handle(this,sel,ptr)){
      update();
      return 1;
      }

    // We're done
    return 0;
    }

  // Dropped on viewer
  if(getDNDData(FROM_DRAGNDROP,FXGLViewer::colorType,(FXuchar*&)clr,len)){
    setBackgroundColor(FXVec4f(clr[0]/65535.0f,clr[1]/65535.0f,clr[2]/65535.0f,1.0f));
    FXFREE(&clr);
    update();
    return 1;
    }
  return 0;
  }


// Change projection
void FXGLViewer::setProjection(FXuint proj){
  projection=proj;
  updateProjection();
  update();
  }


// Set background
void FXGLViewer::setBackgroundColor(const FXVec4f& clr,FXbool bottom){
  if(bottom==MAYBE){
    background[0]=background[1]=clr;
    }
  else{
    background[bottom]=clr;
    }
  update();
  }


// Set ambient color
void FXGLViewer::setAmbientColor(const FXVec4f& clr){
  ambient=clr;
  update();
  }


// Delegate all other messages to the GL Object
long FXGLViewer::onDefault(FXObject* sender,FXSelector sel,void* ptr){
  return selection && selection->handle(sender,sel,ptr);
  }


// Change turbo mode
void FXGLViewer::setTurboMode(FXbool turbo){
  if(!turbo) doesturbo=FALSE;
  turbomode=turbo;
  }


// Return light settings
void FXGLViewer::getLight(FXLight& lite) const {
  lite=light;
  }


// Change light settings
void FXGLViewer::setLight(const FXLight& lite) {
  light=lite;
  update();
  }


// Save object to stream
void FXGLViewer::save(FXStream& store) const {
  FXGLCanvas::save(store);
  store << wvt.w;
  store << wvt.h;
  store << wvt.left;
  store << wvt.right;
  store << wvt.bottom;
  store << wvt.top;
  store << wvt.hither;
  store << wvt.yon;
  store << transform;
  store << itransform;
  store << projection;
  store << rotation;
  store << fov;
  store << zoom;
  store << center;
  store << scale;
  store << worldpx;
  store << modelpx;
  store << maxhits;
  store << diameter;
  store << distance;
  store << background[0];
  store << background[1];
  store << ambient;
  store << turbomode;
  store << help;
  }



// Load object from stream
void FXGLViewer::load(FXStream& store){
  FXGLCanvas::load(store);
  store >> wvt.w;
  store >> wvt.h;
  store >> wvt.left;
  store >> wvt.right;
  store >> wvt.bottom;
  store >> wvt.top;
  store >> wvt.hither;
  store >> wvt.yon;
  store >> transform;
  store >> itransform;
  store >> projection;
  store >> rotation;
  store >> fov;
  store >> zoom;
  store >> center;
  store >> scale;
  store >> worldpx;
  store >> modelpx;
  store >> maxhits;
  store >> diameter;
  store >> distance;
  store >> background[0];
  store >> background[1];
  store >> ambient;
  store >> turbomode;
  store >> help;
  }


// Close and release any resources
FXGLViewer::~FXGLViewer(){
  getApp()->removeTimeout(this,ID_TIPTIMER);
  dropped=(FXGLObject*)-1L;
  selection=(FXGLObject*)-1L;
  scene=(FXGLObject*)-1L;
  }

}
