/********************************************************************************
*                                                                               *
*               D e v i c e   C o n t e x t   B a s e   C l a s s               *
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
* $Id: FXDC.cpp,v 1.38 2006/01/22 17:58:21 fox Exp $                            *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXVisual.h"
#include "FXRegion.h"
#include "FXDC.h"


/*
  Notes:

  - Major Contributions for Windows NT by Lyle Johnson.

  - This is not an abstract base class; rather, its a NULL-implementation,
    i.e. drawing commands to FXDC will go to into the bit bucket.

  - All functions in the DC are virtuals.

  - A DC is associated with a certain drawing surface, using:

      FXDC::begin(FXDrawable* drawable)

    and disassociated with:

      FXDC::end()

  - While associated with a certain surface, no other association may be made
    until after the association is broken.

  - One possible exception to the above rule COULD be made: repeated locks
    on the same surface might be OK; the surface should only be unlocked
    when an equal number of unlocks takes place [[[]][]].

  - One DC object is created for all windows of the same depth and other attributes.

  - We like the OpenGL model of write-only, i.e. we [typically] don't read
    back any info from the DC. [Except stuff like device characteristics].

  - We envision the following flavors of DC's:

      1) A NULL context, in which all output goes to /dev/null

      2) A Window context, in which output goes to a on- or off-screen
         window or pixmap.

      3) A printer context, in which output is rendered e.g. as PostScript.

  - Implementations of DC's for specific target devices may cache various
    things such as patterns, stipples, colors, etc, so as to optimize performance.

  - Since DC's may be shared between windows, you should leave the DC in the
    same state as you found it before releasing it.

  - You acquire a DC by asking the drawable surface on which you want to draw
    for a DC.  You receive a subclass of FXDC, and have no knowledge of device
    specific details, except as revealed by the basic FXDC API's.

  - After you're through with drawing, you should release the DC.

  - Motto: Nobody knows about any other DC that FXDC!!!  The other ones
    are implementations, not directly accessible by any but a few select
    FOX implementation files.

  - In FOX, a FXDC is NOT a wrapper class:- we do NOT assume the O.S. provides
    its own DC abstraction:- hence all those virtuals.
    This also means it is possible to make your own, simply by subclassing FXDC!
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Initialize nicely
FXDC::FXDC(FXApp* a):app(a){
  ctx=NULL;
  font=NULL;
  pattern=STIPPLE_NONE;
  stipple=NULL;
  tile=NULL;
  mask=NULL;
  clip.x=0;
  clip.y=0;
  clip.w=32767;
  clip.h=32767;
  fg=0;
  bg=1;
  width=0;
  cap=CAP_BUTT;
  join=JOIN_MITER;
  style=LINE_SOLID;
  fill=FILL_SOLID;
  rule=RULE_EVEN_ODD;
  rop=BLT_SRC;
  dashpat[0]=4;
  dashpat[1]=4;
  dashlen=2;
  dashoff=0;
  tx=0;
  ty=0;
  cx=0;
  cy=0;
  }


// Read back pixel
FXColor FXDC::readPixel(FXint,FXint){ return FXRGBA(0,0,0,0); }


// Draw a point in the current pen color
void FXDC::drawPoint(FXint,FXint){ }


// Draw points in the current pen color.
// Each point's position is relative to the drawable's origin (as usual).
void FXDC::drawPoints(const FXPoint*,FXuint){ }


// Draw points in the current pen color. The first point's position is
// relative to the drawable's origin, but each subsequent point's position
// is relative to the previous point's position; each FXPoint defines
// the relative coordinates. Think LOGO.
void FXDC::drawPointsRel(const FXPoint*,FXuint){ }


// Draw a line
void FXDC::drawLine(FXint,FXint,FXint,FXint){ }



// Draw multiple lines. All points are drawn connected.
// Each point is specified relative to Drawable's origin.
void FXDC::drawLines(const FXPoint*,FXuint){ }



// Draw multiple lines. All points are drawn connected.
// First point's coordinate is relative to drawable's origin, but
// subsequent points' coordinates are relative to previous point.
void FXDC::drawLinesRel(const FXPoint*,FXuint){ }


// Draw unconnected line segments
void FXDC::drawLineSegments(const FXSegment*,FXuint){ }


// Draw unfilled rectangle
void FXDC::drawRectangle(FXint,FXint,FXint,FXint){ }


// Draw unfilled rectangles
void FXDC::drawRectangles(const FXRectangle*,FXuint){ }


// Draw unfilled rounded rectangle
void FXDC::drawRoundRectangle(FXint,FXint,FXint,FXint,FXint,FXint){ }


// Draw arc
void FXDC::drawArc(FXint,FXint,FXint,FXint,FXint,FXint){ }


// Draw arcs
void FXDC::drawArcs(const FXArc*,FXuint){ }


// Draw ellipse
void FXDC::drawEllipse(FXint,FXint,FXint,FXint){ }


// Filled rectangle
void FXDC::fillRectangle(FXint,FXint,FXint,FXint){ }


// Filled rectangles
void FXDC::fillRectangles(const FXRectangle*,FXuint){ }


// Filled rounded rectangle
void FXDC::fillRoundRectangle(FXint,FXint,FXint,FXint,FXint,FXint){ }


// Fill chord
void FXDC::fillChord(FXint,FXint,FXint,FXint,FXint,FXint){ }


// Fill chords
void FXDC::fillChords(const FXArc*,FXuint){ }


// Fill arc
void FXDC::fillArc(FXint,FXint,FXint,FXint,FXint,FXint){ }


// Fill arcs
void FXDC::fillArcs(const FXArc*,FXuint){ }


// Fill ellipse
void FXDC::fillEllipse(FXint,FXint,FXint,FXint){ }


// Filled simple polygon
void FXDC::fillPolygon(const FXPoint*,FXuint){ }


// Fill concave polygon
void FXDC::fillConcavePolygon(const FXPoint*,FXuint){ }


// Fill complex (self-intersecting) polygon
void FXDC::fillComplexPolygon(const FXPoint*,FXuint){ }


// Filled simple polygon with relative points
void FXDC::fillPolygonRel(const FXPoint*,FXuint){ }


// Fill concave polygon
void FXDC::fillConcavePolygonRel(const FXPoint*,FXuint){ }


// Fill complex (self-intersecting) polygon
void FXDC::fillComplexPolygonRel(const FXPoint*,FXuint){ }


// Draw string with base line starting at x, y
void FXDC::drawText(FXint,FXint,const FXchar*,FXuint){
  }


// Draw string with base line starting at x, y
void FXDC::drawText(FXint,FXint,const FXString&){
  }


// Draw text starting at x, y over filled background
void FXDC::drawImageText(FXint,FXint,const FXchar*,FXuint){
  }


// Draw text starting at x, y over filled background
void FXDC::drawImageText(FXint,FXint,const FXString&){
  }


// Draw area from source
void FXDC::drawArea(const FXDrawable*,FXint,FXint,FXint,FXint,FXint,FXint){ }


// Draw area stretched area from source
void FXDC::drawArea(const FXDrawable*,FXint,FXint,FXint,FXint,FXint,FXint,FXint,FXint){ }


// Draw image
void FXDC::drawImage(const FXImage*,FXint,FXint){ }


// Draw bitmap
void FXDC::drawBitmap(const FXBitmap*,FXint,FXint){ }


// Draw icon
void FXDC::drawIcon(const FXIcon*,FXint,FXint){ }


// Draw icon shaded
void FXDC::drawIconShaded(const FXIcon*,FXint,FXint){ }


// Draw icon sunken
void FXDC::drawIconSunken(const FXIcon*,FXint,FXint){ }


// Draw hashed box
void FXDC::drawHashBox(FXint,FXint,FXint,FXint,FXint){ }


// Draw focus rectangle
void FXDC::drawFocusRectangle(FXint,FXint,FXint,FXint){ }


// Set foreground drawing color (brush)
void FXDC::setForeground(FXColor clr){
  fg=clr;
  }


// Set background drawing color (brush)
void FXDC::setBackground(FXColor clr){
  bg=clr;
  }


// Set dash pattern
void FXDC::setDashes(FXuint dashoffset,const FXchar *dashpattern,FXuint dashlength){
  register FXuint len,i;
  for(i=len=0; i<dashlength; i++){
    dashpat[i]=dashpattern[i];
    len+=(FXuint)dashpattern[i];
    }
  dashlen=dashlength;
  dashoff=dashoffset%len;
  }


// Set line width
void FXDC::setLineWidth(FXuint linewidth){
  width=linewidth;
  }


// Set line cap style
void FXDC::setLineCap(FXCapStyle capstyle){
  cap=capstyle;
  }


// Set line join style
void FXDC::setLineJoin(FXJoinStyle joinstyle){
  join=joinstyle;
  }


// Set line style
void FXDC::setLineStyle(FXLineStyle linestyle){
  style=linestyle;
  }


// Set fill style
void FXDC::setFillStyle(FXFillStyle fillstyle){
  fill=fillstyle;
  }


// Set fill rule
void FXDC::setFillRule(FXFillRule fillrule){
  rule=fillrule;
  }


// Set blit function
void FXDC::setFunction(FXFunction func){
  rop=func;
  }


// Set tile image
void FXDC::setTile(FXImage* image,FXint dx,FXint dy){
  tile=image;
  tx=dx;
  ty=dy;
  }


// Set stipple bitmap
void FXDC::setStipple(FXBitmap* bitmap,FXint dx,FXint dy){
  stipple=bitmap;
  pattern=STIPPLE_NONE;
  tx=dx;
  ty=dy;
  }


// Set stipple pattern
void FXDC::setStipple(FXStipplePattern pat,FXint dx,FXint dy){
  pattern=pat;
  stipple=NULL;
  tx=dx;
  ty=dy;
  }


// Set clip region
void FXDC::setClipRegion(const FXRegion&){
  }


// Set clip rectangle
void FXDC::setClipRectangle(FXint x,FXint y,FXint w,FXint h){
  clip.x=x;
  clip.y=y;
  clip.w=w;
  clip.h=h;
  }


// Set clip rectangle
void FXDC::setClipRectangle(const FXRectangle& rectangle){
  clip=rectangle;
  }


// Clear clipping
void FXDC::clearClipRectangle(){
  clip.x=0;
  clip.y=0;
  clip.w=32767;
  clip.h=32767;
  }


// Set clip mask
void FXDC::setClipMask(FXBitmap* bitmap,FXint dx,FXint dy){
  mask=bitmap;
  cx=dx;
  cy=dy;
  }


// Clear clip mask
void FXDC::clearClipMask(){
  mask=NULL;
  cx=0;
  cy=0;
  }


// Set font to draw text with
void FXDC::setFont(FXFont *fnt){
  font=fnt;
  }


// Change clip-against-child windows mode
void FXDC::clipChildren(FXbool){ }


// Clean up
FXDC::~FXDC(){ }

}
