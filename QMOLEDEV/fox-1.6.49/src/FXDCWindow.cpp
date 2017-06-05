/********************************************************************************
*                                                                               *
*  D e v i c e   C o n t e x t   F o r   W i n d o w s   a n d   I m a g e s    *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXDCWindow.cpp,v 1.163.2.3 2007/02/23 21:22:31 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXObject.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXVisual.h"
#include "FXFont.h"
#include "FXCursor.h"
#include "FXDrawable.h"
#include "FXImage.h"
#include "FXBitmap.h"
#include "FXIcon.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXComposite.h"
#include "FXRootWindow.h"
#include "FXShell.h"
#include "FXRegion.h"
#include "FXDC.h"
#include "FXDCWindow.h"


/*
  Notes:

  - Associate a DC with a surface before you begin using it:

     long SomeWidget::onPaint(FXObject*,FXSelector,void* ptr){
       FXDCWindow dc(this,ptr);
       dc.drawLine(...);
       ... jadajadajada ...
       return 1;
       }

    The association is automatically broken when you go out of scope; the
    destructor of the FXDCWindow does this for you.

  - Optimizations: only perform style/attribute changes just before an actual
    drawing command takes place:- X-Windows apparently already does this;
    MS-Windows also?

  - We assume the following initial state:

    BLIT Function:        BLT_SRC
    Foreground:           black (0)
    Background:           white (1)
    Line Width:           0 (meaning thinnest/fastest, no guaranteed pixelation)
    Cap Style:            CAP_BUTT
    Join Style:           JOIN_MITER
    Line Style:           LINE_SOLID
    Fill Style:           FILL_SOLID
    Fill Rule:            RULE_EVEN_ODD
    Font:                 None
    Other Paremeters:     To Be Determined

  - Under X-Windows, end() will restore the GC to the state above; flags
    keeps track of which changes have been made to minimize the necessary
    updating.

  - Under X, graphics_exposures should be OFF:- at least some SGI IRIX machines
    have broken implementations of graphics_exposures.

  - Try suggestions from "Kevin Radke" <kmradke@isualum.com> below:

    Sorry about the huge delay with this code.  Work has been horribly busy
    and I wasn't able to dig up the CD with the original code.  However, if I
    remember
    correctly (and this code snippet I found was from that test), I changed from
    using
    cosmetic pens to use geometric pens and specifying a line join style of
    PS_JOIN_BEVEL.

    I.E.

        LOGBRUSH logBrush;
        logBrush.lbStyle = BS_SOLID;
        logBrush.lbColor = RGB(red, green, blue);
        logBrush.lbHatch = HS_CROSS;  // Not used

        // NYI Only solid lines are valid on Windows 95.  style is ignored
        return ExtCreatePen (PS_GEOMETRIC | PS_JOIN_BEVEL | style,
                                        width, &logBrush, 0, NULL);


    this returns an HPEN.

    I remember this being significantly slower than cosmetic pens (under NT4)
    and at the time that I scrapped the code, and dealt with the drawing
    differences
    between X and Win32 at a higher level.

    This won't work on Win95/Win98 (without using paths), and after a closer
    look at
    the docs here it makes more sense to specify PS_ENDCAP_SQUARE to draw
    the last pixel instead of the line join style.  I remember experimenting
    with both, so
    the code I found may have been in intermediate (unworking) version.

    In any case, it isn't too hard to experiment to see which has the required
    behavior.  I've unfortunately been away from FOX work for a few months
    or I'd try it myself.

  - Device caps for DirectX:
    http://www.molybdenium.de/devicecaps/e_index.html
*/

#define DISPLAY(app) ((Display*)((app)->display))

using namespace FX;

namespace FX {


/********************************************************************************
*                                    X-Windows                                  *
********************************************************************************/

#ifndef WIN32


// Construct for expose event painting
FXDCWindow::FXDCWindow(FXDrawable* drawable,FXEvent* event):FXDC(drawable->getApp()){
#ifdef HAVE_XFT_H
  xftDraw=NULL;
#endif
  begin(drawable);
  rect.x=clip.x=event->rect.x;
  rect.y=clip.y=event->rect.y;
  rect.w=clip.w=event->rect.w;
  rect.h=clip.h=event->rect.h;
  XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted);
#ifdef HAVE_XFT_H
  XftDrawSetClipRectangles((XftDraw*)xftDraw,0,0,(XRectangle*)&clip,1);
#endif
  flags|=GCClipMask;
  }


// Construct for normal painting
FXDCWindow::FXDCWindow(FXDrawable* drawable):FXDC(drawable->getApp()){
#ifdef HAVE_XFT_H
  xftDraw=NULL;
#endif
  begin(drawable);
  }


// Destruct
FXDCWindow::~FXDCWindow(){
  end();
  }


// Begin locks in a drawable surface
void FXDCWindow::begin(FXDrawable *drawable){
  if(!drawable){ fxerror("FXDCWindow::begin: NULL drawable.\n"); }
  if(!drawable->id()){ fxerror("FXDCWindow::begin: drawable not created yet.\n"); }
  surface=drawable;
  visual=drawable->getVisual();
  rect.x=clip.x=0;
  rect.y=clip.y=0;
  rect.w=clip.w=drawable->getWidth();
  rect.h=clip.h=drawable->getHeight();
  devfg=~0;
  devbg=0;
  ctx=visual->gc;
  flags=0;
#ifdef HAVE_XFT_H
  xftDraw=(void*)XftDrawCreate(DISPLAY(drawable->getApp()),(Drawable)surface->id(),(Visual*)visual->visual,(Colormap)visual->colormap);
#endif
  }


// End unlock the drawable surface; restore it
void FXDCWindow::end(){
  if(flags){
    XGCValues gcv;
    if(flags&GCFunction) gcv.function=BLT_SRC;
    if(flags&GCForeground) gcv.foreground=BlackPixel(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
    if(flags&GCBackground) gcv.background=WhitePixel(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
    if(flags&GCLineWidth) gcv.line_width=0;
    if(flags&GCCapStyle) gcv.cap_style=CAP_BUTT;
    if(flags&GCJoinStyle) gcv.join_style=JOIN_MITER;
    if(flags&GCLineStyle) gcv.line_style=LINE_SOLID;
    if(flags&GCFillStyle) gcv.fill_style=FILL_SOLID;
    if(flags&GCStipple) gcv.stipple=getApp()->stipples[STIPPLE_WHITE];    // Needed for IRIX6.4 bug workaround!
    if(flags&GCFillRule) gcv.fill_rule=RULE_EVEN_ODD;
#ifndef HAVE_XFT_H
    if(flags&GCFont) gcv.font=getApp()->getNormalFont()->id();
#endif
    if(flags&GCClipMask) gcv.clip_mask=None;
    if(flags&GCClipXOrigin) gcv.clip_x_origin=0;
    if(flags&GCClipYOrigin) gcv.clip_y_origin=0;
    if(flags&GCDashOffset) gcv.dash_offset=0;
    if(flags&GCDashList) gcv.dashes=4;
    if(flags&GCTileStipXOrigin) gcv.ts_x_origin=0;
    if(flags&GCTileStipYOrigin) gcv.ts_y_origin=0;
    if(flags&GCGraphicsExposures) gcv.graphics_exposures=True;
    if(flags&GCSubwindowMode) gcv.subwindow_mode=ClipByChildren;
    XChangeGC(DISPLAY(getApp()),(GC)ctx,flags,&gcv);
    flags=0;
    }
  surface=NULL;
#ifdef HAVE_XFT_H
  if(xftDraw){ XftDrawDestroy((XftDraw*)xftDraw); xftDraw=NULL; }
#endif
  }


// Read back pixel
FXColor FXDCWindow::readPixel(FXint x,FXint y){
  FXColor color=FXRGBA(0,0,0,0);
  if(!surface){ fxerror("FXDCWindow::readPixel: DC not connected to drawable.\n"); }
  if(0<=x && 0<=y && x<surface->getWidth() && y<surface->getHeight()){
    XImage* xim=XGetImage(DISPLAY(getApp()),surface->id(),x,y,1,1,AllPlanes,ZPixmap);
    if(xim && xim->data){
      color=visual->getColor(XGetPixel(xim,0,0));
      XDestroyImage(xim);
      }
    }
  return color;
  }


// Draw point
void FXDCWindow::drawPoint(FXint x,FXint y){
  if(!surface){ fxerror("FXDCWindow::drawPoint: DC not connected to drawable.\n"); }
  XDrawPoint(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y);
  }


// Draw points
void FXDCWindow::drawPoints(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::drawPoints: DC not connected to drawable.\n"); }
  XDrawPoints(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,CoordModeOrigin);
  }


// Draw points relative
void FXDCWindow::drawPointsRel(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::drawPointsRel: DC not connected to drawable.\n"); }
  XDrawPoints(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,CoordModePrevious);
  }


// Draw line
void FXDCWindow::drawLine(FXint x1,FXint y1,FXint x2,FXint y2){
  if(!surface){ fxerror("FXDCWindow::drawLine: DC not connected to drawable.\n"); }
  XDrawLine(DISPLAY(getApp()),surface->id(),(GC)ctx,x1,y1,x2,y2);
  }


// Draw lines
void FXDCWindow::drawLines(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::drawLines: DC not connected to drawable.\n"); }
  XDrawLines(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,CoordModeOrigin);
  }


// Draw lines relative
void FXDCWindow::drawLinesRel(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::drawLinesRel: DC not connected to drawable.\n"); }
  XDrawLines(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,CoordModePrevious);
  }


// Draw line segments
void FXDCWindow::drawLineSegments(const FXSegment* segments,FXuint nsegments){
  if(!surface){ fxerror("FXDCWindow::drawLineSegments: DC not connected to drawable.\n"); }
  XDrawSegments(DISPLAY(getApp()),surface->id(),(GC)ctx,(XSegment*)segments,nsegments);
  }


// Draw rectangle
void FXDCWindow::drawRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::drawRectangle: DC not connected to drawable.\n"); }
  XDrawRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h);
  }


// Draw rectangles
void FXDCWindow::drawRectangles(const FXRectangle* rectangles,FXuint nrectangles){
  if(!surface){ fxerror("FXDCWindow::drawRectangles: DC not connected to drawable.\n"); }
  XDrawRectangles(DISPLAY(getApp()),surface->id(),(GC)ctx,(XRectangle*)rectangles,nrectangles);
  }


// Draw round rectangle
void FXDCWindow::drawRoundRectangle(FXint x,FXint y,FXint w,FXint h,FXint ew,FXint eh){
  XArc arcs[4]; XSegment segs[4]; XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::drawRoundRectangle: DC not connected to drawable.\n"); }
  if(ew+ew>w) ew=w>>1;
  if(eh+eh>h) eh=h>>1;
  arcs[0].x=arcs[2].x=x;
  arcs[0].y=arcs[1].y=y;
  arcs[1].x=arcs[3].x=x+w-(ew<<1);
  arcs[2].y=arcs[3].y=y+h-(eh<<1);
  arcs[0].width=arcs[1].width=arcs[2].width=arcs[3].width=ew<<1;
  arcs[0].height=arcs[1].height=arcs[2].height=arcs[3].height=eh<<1;
  arcs[0].angle1=5760;
  arcs[0].angle2=5760;
  arcs[1].angle1=0;
  arcs[1].angle2=5760;
  arcs[2].angle1=11520;
  arcs[2].angle2=5760;
  arcs[3].angle1=17280;
  arcs[3].angle2=5760;
  segs[0].x1=segs[1].x1=x+ew;
  segs[0].x2=segs[1].x2=x+w-ew;
  segs[0].y1=segs[0].y2=y;
  segs[1].y1=segs[1].y2=y+h;
  segs[2].x1=segs[2].x2=x;
  segs[3].x1=segs[3].x2=x+w;
  segs[2].y1=segs[3].y1=y+eh;
  segs[2].y2=segs[3].y2=y+h-eh;
  gcv.cap_style=CapButt;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCCapStyle,&gcv);
  XDrawArcs(DISPLAY(getApp()),surface->id(),(GC)ctx,arcs,4);
  XDrawSegments(DISPLAY(getApp()),surface->id(),(GC)ctx,segs,4);
  gcv.cap_style=cap;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCCapStyle,&gcv);
  }


// Draw arc
void FXDCWindow::drawArc(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  if(!surface){ fxerror("FXDCWindow::drawArc: DC not connected to drawable.\n"); }
  XDrawArc(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h,ang1,ang2);
  }


// Draw arcs
void FXDCWindow::drawArcs(const FXArc* arcs,FXuint narcs){
  if(!surface){ fxerror("FXDCWindow::drawArcs: DC not connected to drawable.\n"); }
  XDrawArcs(DISPLAY(getApp()),surface->id(),(GC)ctx,(XArc*)arcs,narcs);
  }


// Draw ellipse
void FXDCWindow::drawEllipse(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::drawEllipse: DC not connected to drawable.\n"); }
  XDrawArc(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h,0,23040);
  }


// Fill rectangle
void FXDCWindow::fillRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::fillRectangle: DC not connected to drawable.\n"); }
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h);
  }


// Fill rectangles
void FXDCWindow::fillRectangles(const FXRectangle* rectangles,FXuint nrectangles){
  if(!surface){ fxerror("FXDCWindow::fillRectangles: DC not connected to drawable.\n"); }
  XFillRectangles(DISPLAY(getApp()),surface->id(),(GC)ctx,(XRectangle*)rectangles,nrectangles);
  }


// Fill rounded rectangle
void FXDCWindow::fillRoundRectangle(FXint x,FXint y,FXint w,FXint h,FXint ew,FXint eh){
  XArc arcs[4]; XRectangle recs[3];
  if(!surface){ fxerror("FXDCWindow::fillRoundRectangle: DC not connected to drawable.\n"); }
  if(ew+ew>w) ew=w>>1;
  if(eh+eh>h) eh=h>>1;
  arcs[0].x=arcs[2].x=x;
  arcs[0].y=arcs[1].y=y;
  arcs[1].x=arcs[3].x=x+w-(ew<<1);
  arcs[2].y=arcs[3].y=y+h-(eh<<1);
  arcs[0].width=arcs[1].width=arcs[2].width=arcs[3].width=ew<<1;
  arcs[0].height=arcs[1].height=arcs[2].height=arcs[3].height=eh<<1;
  arcs[0].angle1=5760;
  arcs[0].angle2=5760;
  arcs[1].angle1=0;
  arcs[1].angle2=5760;
  arcs[2].angle1=11520;
  arcs[2].angle2=5760;
  arcs[3].angle1=17280;
  arcs[3].angle2=5760;
  recs[0].x=recs[2].x=x+ew;
  recs[0].width=recs[2].width=w-(ew<<1);
  recs[0].height=recs[2].height=eh;
  recs[0].y=y;
  recs[2].y=y+h-eh;
  recs[1].x=x;
  recs[1].y=y+eh;
  recs[1].width=w;
  recs[1].height=h-(eh<<1);
  XFillArcs(DISPLAY(getApp()),surface->id(),(GC)ctx,arcs,4);
  XFillRectangles(DISPLAY(getApp()),surface->id(),(GC)ctx,recs,3);
  }


// Fill chord
void FXDCWindow::fillChord(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  if(!surface){ fxerror("FXDCWindow::fillChord: DC not connected to drawable.\n"); }
  XSetArcMode(DISPLAY(getApp()),(GC)ctx,ArcChord);
  XFillArc(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h,ang1,ang2);
  XSetArcMode(DISPLAY(getApp()),(GC)ctx,ArcPieSlice);
  }


// Fill chords
void FXDCWindow::fillChords(const FXArc* chords,FXuint nchords){
  if(!surface){ fxerror("FXDCWindow::fillChords: DC not connected to drawable.\n"); }
  XSetArcMode(DISPLAY(getApp()),(GC)ctx,ArcChord);
  XFillArcs(DISPLAY(getApp()),surface->id(),(GC)ctx,(XArc*)chords,nchords);
  XSetArcMode(DISPLAY(getApp()),(GC)ctx,ArcPieSlice);
  }


// Fill arc
void FXDCWindow::fillArc(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  if(!surface){ fxerror("FXDCWindow::fillArc: DC not connected to drawable.\n"); }
  XFillArc(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h,ang1,ang2);
  }


// Fill arcs
void FXDCWindow::fillArcs(const FXArc* arcs,FXuint narcs){
  if(!surface){ fxerror("FXDCWindow::fillArcs: DC not connected to drawable.\n"); }
  XFillArcs(DISPLAY(getApp()),surface->id(),(GC)ctx,(XArc*)arcs,narcs);
  }


// Fill ellipse
void FXDCWindow::fillEllipse(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::fillEllipse: DC not connected to drawable.\n"); }
  XFillArc(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w,h,0,23040);
  }


// Fill polygon
void FXDCWindow::fillPolygon(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::fillArcs: DC not connected to drawable.\n"); }
  XFillPolygon(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,Convex,CoordModeOrigin);
  }


// Fill concave polygon
void FXDCWindow::fillConcavePolygon(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::fillConcavePolygon: DC not connected to drawable.\n"); }
  XFillPolygon(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,Nonconvex,CoordModeOrigin);
  }


// Fill complex polygon
void FXDCWindow::fillComplexPolygon(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::fillComplexPolygon: DC not connected to drawable.\n"); }
  XFillPolygon(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,Complex,CoordModeOrigin);
  }


// Fill polygon relative
void FXDCWindow::fillPolygonRel(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::fillPolygonRel: DC not connected to drawable.\n"); }
  XFillPolygon(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,Convex,CoordModePrevious);
  }


// Fill concave polygon relative
void FXDCWindow::fillConcavePolygonRel(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::fillConcavePolygonRel: DC not connected to drawable.\n"); }
  XFillPolygon(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,Nonconvex,CoordModePrevious);
  }


// Fill complex polygon relative
void FXDCWindow::fillComplexPolygonRel(const FXPoint* points,FXuint npoints){
  if(!surface){ fxerror("FXDCWindow::fillComplexPolygonRel: DC not connected to drawable.\n"); }
  XFillPolygon(DISPLAY(getApp()),surface->id(),(GC)ctx,(XPoint*)points,npoints,Complex,CoordModePrevious);
  }


// Set text font
void FXDCWindow::setFont(FXFont *fnt){
  if(!surface){ fxerror("FXDCWindow::setFont: DC not connected to drawable.\n"); }
  if(!fnt || !fnt->id()){ fxerror("FXDCWindow::setFont: illegal or NULL font specified.\n"); }
#ifndef HAVE_XFT_H
  XSetFont(DISPLAY(getApp()),(GC)ctx,fnt->id());
  flags|=GCFont;
#endif
  font=fnt;
  }


/*

 We eventually want subclassable fonts.
 FXDCWindow knows about surface, but does not know about font type.
 Thus, drawText() here should vector to new API's in FXFont.
 New API gets FXDC* (or FXDC&) so that it can obtain colors, &c.
 Thus, all knowledge of font-technology is kept inside FXFont.
 Knowledge of FXDCWindow surface is kept inside FXDCWindow.

 But FXDC may have some responsibility for layout of characters.

 Of course, certain font types can only draw on certain DC types...


void FXDCWindow::drawText(FXint x,FXint y,const FXchar* string,FXuint length){
  if(!surface){ fxerror("FXDCWindow::drawText: DC not connected to drawable.\n"); }
  if(!font){ fxerror("FXDCWindow::drawText: no font selected.\n"); }
  font->drawText(this,x,y,string,length);
  }
*/



#define FS ((XFontStruct*)(font->font))


static FXint utf2db(XChar2b *dst,const FXchar *src,FXint n){
  register FXint len,p;
  register FXwchar w;
  for(p=len=0; p<n; p+=wclen(src+p),len++){
    w=wc(src+p);
    dst[len].byte1=(w>>8);
    dst[len].byte2=(w&255);
    }
  return len;
  }


// Draw string with base line starting at x, y
void FXDCWindow::drawText(FXint x,FXint y,const FXchar* string,FXuint length){
  if(!surface){ fxerror("FXDCWindow::drawText: DC not connected to drawable.\n"); }
  if(!font){ fxerror("FXDCWindow::drawText: no font selected.\n"); }
#ifdef HAVE_XFT_H
  XftColor color;
  color.pixel=devfg;
  color.color.red=FXREDVAL(fg)*257;
  color.color.green=FXGREENVAL(fg)*257;
  color.color.blue=FXBLUEVAL(fg)*257;
  color.color.alpha=FXALPHAVAL(fg)*257;
  XftDrawStringUtf8((XftDraw*)xftDraw,&color,(XftFont*)font->font,x,y,(const FcChar8*)string,length);
#else
  register FXint count,escapement,defwidth,ww,size,i;
  register FXdouble ang,ux,uy;
  register FXuchar r,c;
  XChar2b sbuffer[4096];
  count=utf2db(sbuffer,string,FXMIN(length,4096));
  if(font->getAngle()){
    ang=font->getAngle()*0.00027270769562411399179;
    defwidth=FS->min_bounds.width;
    ux=cos(ang);
    uy=sin(ang);
    if(FS->per_char){
      r=FS->default_char>>8;
      c=FS->default_char&255;
      size=(FS->max_char_or_byte2-FS->min_char_or_byte2+1);
      if(FS->min_char_or_byte2<=c && c<=FS->max_char_or_byte2 && FS->min_byte1<=r && r<=FS->max_byte1){
        defwidth=FS->per_char[(r-FS->min_byte1)*size+(c-FS->min_char_or_byte2)].width;
        }
      for(i=escapement=0; i<count; i++){
        XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,(FXint)(x+escapement*ux),(FXint)(y-escapement*uy),&sbuffer[i],1);
        r=sbuffer[i].byte1;
        c=sbuffer[i].byte2;
        escapement+=defwidth;
        if(FS->min_char_or_byte2<=c && c<=FS->max_char_or_byte2 && FS->min_byte1<=r && r<=FS->max_byte1){
          if((ww=FS->per_char[(r-FS->min_byte1)*size+(c-FS->min_char_or_byte2)].width)!=0) escapement+=ww-defwidth;
          }
        }
      }
    else{
      for(i=escapement=0; i<count; i++){
        XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,(FXint)(x+escapement*ux),(FXint)(y-escapement*uy),&sbuffer[i],1);
        escapement+=defwidth;
        }
      }
    }
  else{
    XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,sbuffer,count);
    }
#endif
  }


// Draw text starting at x, y over filled background
void FXDCWindow::drawImageText(FXint x,FXint y,const FXchar* string,FXuint length){
  if(!surface){ fxerror("FXDCWindow::drawImageText: DC not connected to drawable.\n"); }
  if(!font){ fxerror("FXDCWindow::drawImageText: no font selected.\n"); }
#ifdef HAVE_XFT_H
  XGlyphInfo extents;
  XftColor fgcolor,bgcolor;
  fgcolor.pixel=devfg;
  fgcolor.color.red=FXREDVAL(fg)*257;
  fgcolor.color.green=FXGREENVAL(fg)*257;
  fgcolor.color.blue=FXBLUEVAL(fg)*257;
  fgcolor.color.alpha=FXALPHAVAL(fg)*257;
  bgcolor.pixel=devbg;
  bgcolor.color.red=FXREDVAL(bg)*257;
  bgcolor.color.green=FXGREENVAL(bg)*257;
  bgcolor.color.blue=FXBLUEVAL(bg)*257;
  bgcolor.color.alpha=FXALPHAVAL(bg)*257;

  // Area to blank
  XftTextExtents8(DISPLAY(getApp()),(XftFont*)font->font,(const FcChar8*)string,length,&extents);

  // Erase around text [FIXME wrong location]
  XftDrawRect((XftDraw*)xftDraw,&bgcolor,x,y-font->getFontAscent(),extents.width,extents.height);
//  XftDrawRect((XftDraw*)xftDraw,&bgcolor,x+cache->xoff,y-xftfs->ascent,cache->x2off-cache->xoff,xftfs->ascent+xftfs->descent);
//XftDrawRect((XftDraw*)xftDraw,&bgcolor,x+cache->xoff,y-((XftFont*)font->font)->ascent,cache->x2off-cache->xoff,((XftFont*)font->font)->ascent+((XftFont*)font->font)->descent);
  XftDrawStringUtf8((XftDraw*)xftDraw,&fgcolor,(XftFont*)font->font,x,y,(const FcChar8*)string,length);
#else
  register FXint count,escapement,defwidth,ww,size,i;
  register FXdouble ang,ux,uy;
  register FXuchar r,c;
  XChar2b sbuffer[4096];
  count=utf2db(sbuffer,string,FXMIN(length,4096));
  if(font->getAngle()){
    ang=font->getAngle()*0.00027270769562411399179;
    defwidth=FS->min_bounds.width;
    ux=cos(ang);
    uy=sin(ang);
    if(FS->per_char){
      r=FS->default_char>>8;
      c=FS->default_char&255;
      size=(FS->max_char_or_byte2-FS->min_char_or_byte2+1);
      if(FS->min_char_or_byte2<=c && c<=FS->max_char_or_byte2 && FS->min_byte1<=r && r<=FS->max_byte1){
        defwidth=FS->per_char[(r-FS->min_byte1)*size+(c-FS->min_char_or_byte2)].width;
        }
      for(i=escapement=0; i<count; i++){
        XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,(FXint)(x+escapement*ux),(FXint)(y-escapement*uy),&sbuffer[i],1);
        r=sbuffer[i].byte1;
        c=sbuffer[i].byte2;
        escapement+=defwidth;
        if(FS->min_char_or_byte2<=c && c<=FS->max_char_or_byte2 && FS->min_byte1<=r && r<=FS->max_byte1){
          if((ww=FS->per_char[(r-FS->min_byte1)*size+(c-FS->min_char_or_byte2)].width)!=0) escapement+=ww-defwidth;
          }
        }
      }
    else{
      for(i=escapement=0; i<count; i++){
        XDrawImageString16(DISPLAY(getApp()),surface->id(),(GC)ctx,(FXint)(x+escapement*ux),(FXint)(y-escapement*uy),&sbuffer[i],1);
        escapement+=defwidth;
        }
      }
    }
  else{
    XDrawImageString16(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,sbuffer,count);
    }
#endif
  }

#undef FS



// Draw string with base line starting at x, y
void FXDCWindow::drawText(FXint x,FXint y,const FXString& string){
  drawText(x,y,string.text(),string.length());
  }


// Draw text starting at x, y over filled background
void FXDCWindow::drawImageText(FXint x,FXint y,const FXString& string){
  drawImageText(x,y,string.text(),string.length());
  }


// Draw area
void FXDCWindow::drawArea(const FXDrawable* source,FXint sx,FXint sy,FXint sw,FXint sh,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawArea: DC not connected to drawable.\n"); }
  if(!source || !source->id()){ fxerror("FXDCWindow::drawArea: illegal source specified.\n"); }
  XCopyArea(DISPLAY(getApp()),source->id(),surface->id(),(GC)ctx,sx,sy,sw,sh,dx,dy);
  }


// Draw area stretched area from source; FIXME this works but it's like molasses!
void FXDCWindow::drawArea(const FXDrawable* source,FXint sx,FXint sy,FXint sw,FXint sh,FXint dx,FXint dy,FXint dw,FXint dh){
  register FXint i,j,x,y,xs,ys;
  if(!surface){ fxerror("FXDCWindow::drawArea: DC not connected to drawable.\n"); }
  if(!source || !source->id()){ fxerror("FXDCWindow::drawArea: illegal source specified.\n"); }
  xs=(sw<<16)/dw;
  ys=(sh<<16)/dh;
  i=0;
  y=ys>>1;
  do{
    j=0;
    x=xs>>1;
    do{
      XCopyArea(DISPLAY(getApp()),source->id(),surface->id(),(GC)ctx,sx+(x>>16),sy+(y>>16),1,1,dx+j,dy+i);
      x+=xs;
      }
    while(++j<dw);
    y+=ys;
    }
  while(++i<dh);
  }


// Draw image
void FXDCWindow::drawImage(const FXImage* image,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawImage: DC not connected to drawable.\n"); }
  if(!image || !image->id()){ fxerror("FXDCWindow::drawImage: illegal image specified.\n"); }
  XCopyArea(DISPLAY(getApp()),image->id(),surface->id(),(GC)ctx,0,0,image->width,image->height,dx,dy);
  }


// Draw bitmap
void FXDCWindow::drawBitmap(const FXBitmap* bitmap,FXint dx,FXint dy) {
  if(!surface) fxerror("FXDCWindow::drawBitmap: DC not connected to drawable.\n");
  if(!bitmap || !bitmap->id()) fxerror("FXDCWindow::drawBitmap: illegal bitmap specified.\n");
  XCopyPlane(DISPLAY(getApp()),bitmap->id(),surface->id(),(GC)ctx,0,0,bitmap->width,bitmap->height,dx,dy,1);
  }


// Draw a vanilla icon
void FXDCWindow::drawIcon(const FXIcon* icon,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawIcon: DC not connected to drawable.\n"); }
  if(!icon || !icon->id() || !icon->shape){ fxerror("FXDCWindow::drawIcon: illegal icon specified.\n"); }
  FXRectangle d=clip*FXRectangle(dx,dy,icon->width,icon->height);
  if(d.w>0 && d.h>0){
    if(icon->getOptions()&IMAGE_OPAQUE){
      XCopyArea(DISPLAY(getApp()),icon->id(),surface->id(),(GC)ctx,d.x-dx,d.y-dy,d.w,d.h,d.x,d.y);
      }
    else{
      XGCValues gcv;
      gcv.clip_mask=icon->shape;
      gcv.clip_x_origin=dx;
      gcv.clip_y_origin=dy;
      XChangeGC(DISPLAY(getApp()),(GC)ctx,GCClipMask|GCClipXOrigin|GCClipYOrigin,&gcv);
      XCopyArea(DISPLAY(getApp()),icon->id(),surface->id(),(GC)ctx,d.x-dx,d.y-dy,d.w,d.h,d.x,d.y);
      XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted); // Restore old clip rectangle
      flags|=GCClipMask;
      }
    }
  }


// Draw a shaded icon, like when it is selected
void FXDCWindow::drawIconShaded(const FXIcon* icon,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawIconShaded: DC not connected to drawable.\n"); }
  if(!icon || !icon->id() || !icon->shape){ fxerror("FXDCWindow::drawIconShaded: illegal icon specified.\n"); }
  FXRectangle d=clip*FXRectangle(dx,dy,icon->width,icon->height);
  if(d.w>0 && d.h>0){
    XGCValues gcv;
    gcv.clip_mask=icon->shape;
    gcv.clip_x_origin=dx;
    gcv.clip_y_origin=dy;
    XChangeGC(DISPLAY(getApp()),(GC)ctx,GCClipMask|GCClipXOrigin|GCClipYOrigin,&gcv);
    XCopyArea(DISPLAY(getApp()),icon->id(),surface->id(),(GC)ctx,d.x-dx,d.y-dy,d.w,d.h,d.x,d.y);
    gcv.function=BLT_SRC;
    gcv.stipple=getApp()->stipples[STIPPLE_GRAY];
    gcv.fill_style=FILL_STIPPLED;
    gcv.ts_x_origin=dx;
    gcv.ts_y_origin=dy;
    gcv.foreground=visual->getPixel(getApp()->getSelbackColor());
    XChangeGC(DISPLAY(getApp()),(GC)ctx,GCForeground|GCFunction|GCTileStipXOrigin|GCTileStipYOrigin|GCStipple|GCFillStyle,&gcv);
    XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,d.x,d.y,d.w,d.h);
    gcv.function=rop;
    gcv.fill_style=fill;
    gcv.ts_x_origin=tx;
    gcv.ts_y_origin=ty;
    XChangeGC(DISPLAY(getApp()),(GC)ctx,GCTileStipXOrigin|GCTileStipYOrigin|GCFunction|GCFillStyle,&gcv);  // Restore old raster op function and fill style
    XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted); // Restore old clip rectangle
    flags|=GCClipMask;
    }
  }


// This draws a sunken icon
void FXDCWindow::drawIconSunken(const FXIcon* icon,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawIconSunken: DC not connected to drawable.\n"); }
  if(!icon || !icon->id() || !icon->etch){ fxerror("FXDCWindow::drawIconSunken: illegal icon specified.\n"); }
  XGCValues gcv;
  FXColor base=getApp()->getBaseColor();
  FXColor clr=FXRGB((85*FXREDVAL(base))/100,(85*FXGREENVAL(base))/100,(85*FXBLUEVAL(base))/100);

  // Erase to black
  gcv.background=0;
  gcv.foreground=0xffffffff;
  gcv.function=BLT_NOT_SRC_AND_DST;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCForeground|GCBackground|GCFunction,&gcv);
  XCopyPlane(DISPLAY(getApp()),icon->etch,surface->id(),(GC)ctx,0,0,icon->width,icon->height,dx+1,dy+1,1);

  // Paint highlight part
  gcv.function=BLT_SRC_OR_DST;
  gcv.foreground=visual->getPixel(getApp()->getHiliteColor());
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCForeground|GCFunction,&gcv);
  XCopyPlane(DISPLAY(getApp()),icon->etch,surface->id(),(GC)ctx,0,0,icon->width,icon->height,dx+1,dy+1,1);

  // Erase to black
  gcv.foreground=0xffffffff;
  gcv.function=BLT_NOT_SRC_AND_DST;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCForeground|GCFunction,&gcv);
  XCopyPlane(DISPLAY(getApp()),icon->etch,surface->id(),(GC)ctx,0,0,icon->width,icon->height,dx,dy,1);

  // Paint shadow part
  gcv.function=BLT_SRC_OR_DST;
  gcv.foreground=visual->getPixel(clr);
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCForeground|GCFunction,&gcv);
  XCopyPlane(DISPLAY(getApp()),icon->etch,surface->id(),(GC)ctx,0,0,icon->width,icon->height,dx,dy,1);

  // Restore stuff
  gcv.foreground=devfg;
  gcv.background=devbg;
  gcv.function=rop;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCForeground|GCBackground|GCFunction,&gcv);
  }


// Draw hash box
void FXDCWindow::drawHashBox(FXint x,FXint y,FXint w,FXint h,FXint b){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::drawHashBox: DC not connected to drawable.\n"); }
  gcv.stipple=getApp()->stipples[STIPPLE_GRAY];
  gcv.fill_style=FILL_STIPPLED;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCStipple|GCFillStyle,&gcv);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w-b,b);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x+w-b,y,b,h-b);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x+b,y+h-b,w-b,b);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y+b,b,h-b);
  gcv.stipple=getApp()->stipples[STIPPLE_WHITE];    // Needed for IRIX6.4 bug workaround!
  gcv.fill_style=fill;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCStipple|GCFillStyle,&gcv);
  }


// Draw focus rectangle
void FXDCWindow::drawFocusRectangle(FXint x,FXint y,FXint w,FXint h){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::drawFocusRectangle: DC not connected to drawable.\n"); }
  gcv.stipple=getApp()->stipples[STIPPLE_GRAY];
  gcv.fill_style=FILL_STIPPLED;
  gcv.background=0;
  gcv.foreground=0xffffffff;    // Maybe should use FILL_OPAQUESTIPPLED and current fg/bg color and BLT_SRC
  gcv.function=BLT_SRC_XOR_DST; // This would be more flexible
  gcv.ts_x_origin=x;
  gcv.ts_y_origin=y;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCTileStipXOrigin|GCTileStipYOrigin|GCForeground|GCBackground|GCFunction|GCStipple|GCFillStyle,&gcv);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,w-1,1);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x+w-1,y,1,h-1);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x+1,y+h-1,w-1,1);
  XFillRectangle(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y+1,1,h-1);
  gcv.stipple=getApp()->stipples[STIPPLE_WHITE];    // Needed for IRIX6.4 bug workaround!
  gcv.fill_style=fill;
  gcv.background=devbg;
  gcv.foreground=devfg;
  gcv.function=rop;
  gcv.ts_x_origin=tx;
  gcv.ts_y_origin=ty;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCTileStipXOrigin|GCTileStipYOrigin|GCForeground|GCBackground|GCFunction|GCStipple|GCFillStyle,&gcv);
  }


// Set foreground color
void FXDCWindow::setForeground(FXColor clr){
  if(!surface){ fxerror("FXDCWindow::setForeground: DC not connected to drawable.\n"); }
  devfg=visual->getPixel(clr);
  XSetForeground(DISPLAY(getApp()),(GC)ctx,devfg);
  flags|=GCForeground;
  fg=clr;
  }


// Set background color
void FXDCWindow::setBackground(FXColor clr){
  if(!surface){ fxerror("FXDCWindow::setBackground: DC not connected to drawable.\n"); }
  devbg=visual->getPixel(clr);
  XSetBackground(DISPLAY(getApp()),(GC)ctx,devbg);
  flags|=GCBackground;
  bg=clr;
  }


// Set dashes
void FXDCWindow::setDashes(FXuint dashoffset,const FXchar *dashpattern,FXuint dashlength){
  register FXuint len,i;
  if(!surface){ fxerror("FXDCWindow::setDashes: DC not connected to drawable.\n"); }
  for(i=len=0; i<dashlength; i++){
    dashpat[i]=dashpattern[i];
    len+=(FXuint)dashpattern[i];
    }
  dashlen=dashlength;
  dashoff=dashoffset%len;
  XSetDashes(DISPLAY(getApp()),(GC)ctx,dashoff,(char*)dashpat,dashlen);
  flags|=(GCDashList|GCDashOffset);
  }


// Set line width
void FXDCWindow::setLineWidth(FXuint linewidth){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setLineWidth: DC not connected to drawable.\n"); }
  gcv.line_width=linewidth;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCLineWidth,&gcv);
  flags|=GCLineWidth;
  width=linewidth;
  }


// Set line cap style
void FXDCWindow::setLineCap(FXCapStyle capstyle){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setLineCap: DC not connected to drawable.\n"); }
  gcv.cap_style=capstyle;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCCapStyle,&gcv);
  flags|=GCCapStyle;
  cap=capstyle;
  }


// Set line join style
void FXDCWindow::setLineJoin(FXJoinStyle joinstyle){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setLineJoin: DC not connected to drawable.\n"); }
  gcv.join_style=joinstyle;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCJoinStyle,&gcv);
  flags|=GCJoinStyle;
  join=joinstyle;
  }


// Set line style
void FXDCWindow::setLineStyle(FXLineStyle linestyle){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setLineStyle: DC not connected to drawable.\n"); }
  gcv.line_style=linestyle;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCLineStyle,&gcv);
  flags|=GCLineStyle;
  style=linestyle;
  }


// Set fill style
void FXDCWindow::setFillStyle(FXFillStyle fillstyle){
  if(!surface){ fxerror("FXDCWindow::setFillStyle: DC not connected to drawable.\n"); }
  XSetFillStyle(DISPLAY(getApp()),(GC)ctx,fillstyle);
  flags|=GCFillStyle;
  fill=fillstyle;
  }


// Set polygon fill rule
void FXDCWindow::setFillRule(FXFillRule fillrule){
  if(!surface){ fxerror("FXDCWindow::setFillRule: DC not connected to drawable.\n"); }
  XSetFillRule(DISPLAY(getApp()),(GC)ctx,fillrule);
  flags|=GCFillRule;
  rule=fillrule;
  }


// Set raster function
void FXDCWindow::setFunction(FXFunction func){
  if(!surface){ fxerror("FXDCWindow::setFunction: DC not connected to drawable.\n"); }
  XSetFunction(DISPLAY(getApp()),(GC)ctx,func);
  flags|=GCFunction;
  rop=func;
  }


// Set tile pattern
void FXDCWindow::setTile(FXImage* image,FXint dx,FXint dy){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setTile: DC not connected to drawable.\n"); }
  if(!image || !image->id()){ fxerror("FXDCWindow::setTile: illegal image specified.\n"); }
  gcv.tile=image->id();
  gcv.ts_x_origin=dx;
  gcv.ts_y_origin=dy;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCTileStipXOrigin|GCTileStipYOrigin|GCTile,&gcv);
  if(dx) flags|=GCTileStipXOrigin;
  if(dy) flags|=GCTileStipYOrigin;
  tile=image;
  tx=dx;
  ty=dy;
  }


// Set stipple bitmap
void FXDCWindow::setStipple(FXBitmap* bitmap,FXint dx,FXint dy){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setStipple: DC not connected to drawable.\n"); }
  if(!bitmap || !bitmap->id()){ fxerror("FXDCWindow::setStipple: illegal image specified.\n"); }
  gcv.stipple=bitmap->id();
  gcv.ts_x_origin=dx;
  gcv.ts_y_origin=dy;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCTileStipXOrigin|GCTileStipYOrigin|GCStipple,&gcv);
  if(dx) flags|=GCTileStipXOrigin;
  if(dy) flags|=GCTileStipYOrigin;
  flags|=GCStipple;
  stipple=bitmap;
  pattern=STIPPLE_NONE;
  tx=dx;
  ty=dy;
  }


// Set stipple pattern
void FXDCWindow::setStipple(FXStipplePattern pat,FXint dx,FXint dy){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setStipple: DC not connected to drawable.\n"); }
  if(pat>STIPPLE_CROSSDIAG) pat=STIPPLE_CROSSDIAG;
  FXASSERT(getApp()->stipples[pat]);
  gcv.stipple=getApp()->stipples[pat];
  gcv.ts_x_origin=dx;
  gcv.ts_y_origin=dy;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCTileStipXOrigin|GCTileStipYOrigin|GCStipple,&gcv);
  if(dx) flags|=GCTileStipXOrigin;
  if(dy) flags|=GCTileStipYOrigin;
  stipple=NULL;
  pattern=pat;
  flags|=GCStipple;
  tx=dx;
  ty=dy;
  }


// Set clip region
void FXDCWindow::setClipRegion(const FXRegion& region){
  if(!surface){ fxerror("FXDCWindow::setClipRegion: DC not connected to drawable.\n"); }
  XSetRegion(DISPLAY(getApp()),(GC)ctx,(Region)region.region);///// Should intersect region and rect??
#ifdef HAVE_XFT_H
  XftDrawSetClip((XftDraw*)xftDraw,(Region)region.region);
#endif
  flags|=GCClipMask;
  }


// Set clip rectangle
void FXDCWindow::setClipRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::setClipRectangle: DC not connected to drawable.\n"); }
  clip.x=FXMAX(x,rect.x);
  clip.y=FXMAX(y,rect.y);
  clip.w=FXMIN(x+w,rect.x+rect.w)-clip.x;
  clip.h=FXMIN(y+h,rect.y+rect.h)-clip.y;
  if(clip.w<=0) clip.w=0;
  if(clip.h<=0) clip.h=0;
  XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted);
#ifdef HAVE_XFT_H
  XftDrawSetClipRectangles((XftDraw*)xftDraw,0,0,(XRectangle*)&clip,1);
#endif
  flags|=GCClipMask;
  }


// Set clip rectangle
void FXDCWindow::setClipRectangle(const FXRectangle& rectangle){
  if(!surface){ fxerror("FXDCWindow::setClipRectangle: DC not connected to drawable.\n"); }
  clip.x=FXMAX(rectangle.x,rect.x);
  clip.y=FXMAX(rectangle.y,rect.y);
  clip.w=FXMIN(rectangle.x+rectangle.w,rect.x+rect.w)-clip.x;
  clip.h=FXMIN(rectangle.y+rectangle.h,rect.y+rect.h)-clip.y;
  if(clip.w<=0) clip.w=0;
  if(clip.h<=0) clip.h=0;
  XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted);
#ifdef HAVE_XFT_H
  XftDrawSetClipRectangles((XftDraw*)xftDraw,0,0,(XRectangle*)&clip,1);
#endif
  flags|=GCClipMask;
  }


// Clear clip rectangle
void FXDCWindow::clearClipRectangle(){
  if(!surface){ fxerror("FXDCWindow::clearClipRectangle: DC not connected to drawable.\n"); }
  clip=rect;
  XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted);
#ifdef HAVE_XFT_H
  XftDrawSetClipRectangles((XftDraw*)xftDraw,0,0,(XRectangle*)&clip,1);
#endif
  flags|=GCClipMask;
  }


// Set clip mask
void FXDCWindow::setClipMask(FXBitmap* bitmap,FXint dx,FXint dy){
  XGCValues gcv;
  if(!surface){ fxerror("FXDCWindow::setClipMask: DC not connected to drawable.\n"); }
  if(!bitmap || !bitmap->id()){ fxerror("FXDCWindow::setClipMask: illegal mask specified.\n"); }
  gcv.clip_mask=bitmap->id();
  gcv.clip_x_origin=dx;
  gcv.clip_y_origin=dy;
  XChangeGC(DISPLAY(getApp()),(GC)ctx,GCClipMask|GCClipXOrigin|GCClipYOrigin,&gcv);
  if(dx) flags|=GCClipXOrigin;
  if(dy) flags|=GCClipYOrigin;
  flags|=GCClipMask;
  mask=bitmap;
  cx=dx;
  cy=dy;
  }


// Clear clip mask
void FXDCWindow::clearClipMask(){
  if(!surface){ fxerror("FXDCWindow::clearClipMask: DC not connected to drawable.\n"); }
  clip=rect;
  XSetClipRectangles(DISPLAY(getApp()),(GC)ctx,0,0,(XRectangle*)&clip,1,Unsorted);
  flags|=GCClipMask;
  mask=NULL;
  cx=0;
  cy=0;
  }


// Set clip child windows
void FXDCWindow::clipChildren(FXbool yes){
  if(!surface){ fxerror("FXDCWindow::clipChildren: window has not yet been created.\n"); }
  if(yes){
    XSetSubwindowMode(DISPLAY(getApp()),(GC)ctx,ClipByChildren);
#ifdef HAVE_XFT_H
    XftDrawSetSubwindowMode((XftDraw*)xftDraw,ClipByChildren);
#endif
    flags&=~GCSubwindowMode;
    }
  else{
    XSetSubwindowMode(DISPLAY(getApp()),(GC)ctx,IncludeInferiors);
#ifdef HAVE_XFT_H
    XftDrawSetSubwindowMode((XftDraw*)xftDraw,IncludeInferiors);
#endif
    flags|=GCSubwindowMode;
    }
  }


/********************************************************************************
*                                   MS-Windows                                  *
********************************************************************************/

#else

// This one is not defined in the Cygwin header files
#ifndef PS_JOIN_MASK
#define PS_JOIN_MASK 0x0000F000
#endif

// Construct for expose event painting
FXDCWindow::FXDCWindow(FXDrawable* drawable,FXEvent* event):FXDC(drawable->getApp()){
  oldpalette=NULL;
  oldbrush=NULL;
  oldpen=NULL;
  needsNewBrush=FALSE;
  needsNewPen=FALSE;
  needsPath=FALSE;
  needsClipReset=FALSE;
  begin(drawable);
  rect.x=clip.x=event->rect.x;
  rect.y=clip.y=event->rect.y;
  rect.w=clip.w=event->rect.w;
  rect.h=clip.h=event->rect.h;
  HRGN hrgn=CreateRectRgn(clip.x,clip.y,clip.x+clip.w,clip.y+clip.h);
  SelectClipRgn((HDC)ctx,hrgn);
  DeleteObject(hrgn);
  }


// Construct for normal painting
FXDCWindow::FXDCWindow(FXDrawable* drawable):FXDC(drawable->getApp()){
  oldpalette=NULL;
  oldbrush=NULL;
  oldpen=NULL;
  needsNewBrush=FALSE;
  needsNewPen=FALSE;
  needsPath=FALSE;
  needsClipReset=FALSE;
  begin(drawable);
  }


// Destruct
FXDCWindow::~FXDCWindow(){
  end();
  }


// Begin locks in a drawable surface
void FXDCWindow::begin(FXDrawable *drawable){
  if(!drawable){ fxerror("FXDCWindow::begin: NULL drawable.\n"); }
  if(!drawable->id()){ fxerror("FXDCWindow::begin: drawable not created yet.\n"); }

  surface=drawable;// Careful:- surface->id() can be HWND or HBITMAP depending on drawable
  visual=drawable->getVisual();
  ctx=drawable->GetDC();
  rect.x=clip.x=0;
  rect.y=clip.y=0;
  rect.w=clip.w=drawable->getWidth();
  rect.h=clip.h=drawable->getHeight();

  // Select and realize palette, if necessary
  if(visual->colormap){
    oldpalette=::SelectPalette((HDC)ctx,(HPALETTE)visual->colormap,false);
    ::RealizePalette((HDC)ctx);
    }

  devfg=~0;
  devbg=0;

  // Create our default pen (black, solid, one pixel wide)
  LOGBRUSH lb;
  lb.lbStyle=BS_SOLID;
  lb.lbColor=PALETTERGB(0,0,0);
  lb.lbHatch=0;
  oldpen=::SelectObject((HDC)ctx,ExtCreatePen(PS_GEOMETRIC|PS_SOLID|PS_ENDCAP_FLAT|PS_JOIN_MITER,1,&lb,0,NULL));

  // Create our default brush (solid white, for fills)
  lb.lbStyle=BS_SOLID;
  lb.lbColor=PALETTERGB(255,255,255);
  lb.lbHatch=0;
  oldbrush=SelectObject((HDC)ctx,CreateBrushIndirect(&lb));

  // Text alignment
  ::SetTextAlign((HDC)ctx,TA_BASELINE|TA_LEFT);

  // Polygon fill mode
  ::SetPolyFillMode((HDC)ctx,ALTERNATE);

  // Reset flags
  needsNewBrush=FALSE;
  needsNewPen=FALSE;
  needsPath=FALSE;
  needsClipReset=FALSE;
  }


// End unlocks the drawable surface
void FXDCWindow::end(){
  if(ctx){
    ::DeleteObject(::SelectObject((HDC)ctx,oldpen));
    ::DeleteObject(::SelectObject((HDC)ctx,oldbrush));
    if(visual->colormap){
      SelectPalette((HDC)ctx,(HPALETTE)oldpalette,false);
      }
    surface->ReleaseDC((HDC)ctx);
    if(needsClipReset){
      DWORD dwFlags=GetWindowLong((HWND)surface->id(),GWL_STYLE);
      SetWindowLong((HWND)surface->id(),GWL_STYLE,dwFlags|WS_CLIPCHILDREN);
      }
    ctx=NULL;
    }
  surface=NULL;
  }


// Read back pixel
FXColor FXDCWindow::readPixel(FXint x,FXint y){
  FXColor color=FXRGBA(0,0,0,0);
  if(!surface){ fxerror("FXDCWindow::readPixel: DC not connected to drawable.\n"); }
  if(0<=x && 0<=y && x<surface->getWidth() && y<surface->getHeight()){
    COLORREF clr=GetPixel((HDC)ctx,x,y);
    color=FXRGB(GetRValue(clr),GetGValue(clr),GetBValue(clr));
    }
  return color;
  }


// Draw pixel in current foreground color
void FXDCWindow::drawPoint(FXint x,FXint y){
  if(!surface){ fxerror("FXDCWindow::drawPoint: DC not connected to drawable.\n"); }
  ::SetPixel((HDC)ctx,x,y,devfg);
  }


// Draw points
void FXDCWindow::drawPoints(const FXPoint* points,FXuint npoints){
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::drawPoints: DC not connected to drawable.\n"); }
  for(i=0; i<npoints; i++){
    ::SetPixel((HDC)ctx,points[i].x,points[i].y,devfg);
    }
  }


// Draw points relative
void FXDCWindow::drawPointsRel(const FXPoint* points,FXuint npoints){
  register int x=0,y=0;
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::drawPointsRel: DC not connected to drawable.\n"); }
  for(i=0; i<npoints; i++){
    x+=points[i].x;
    y+=points[i].y;
    ::SetPixel((HDC)ctx,x,y,devfg);
    }
  }


// Draw line
void FXDCWindow::drawLine(FXint x1,FXint y1,FXint x2,FXint y2){
  if(!surface){ fxerror("FXDCWindow::drawLine: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  if(needsPath){
    ::BeginPath((HDC)ctx);
    }
  POINT pts[2];
  pts[0].x=x1; pts[0].y=y1;
  pts[1].x=x2; pts[1].y=y2;
  ::Polyline((HDC)ctx,pts,2);
  if(needsPath){
    ::EndPath((HDC)ctx);
    ::StrokePath((HDC)ctx);
    }
  }


// Draw lines
void FXDCWindow::drawLines(const FXPoint* points,FXuint npoints){
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::drawLines: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  if(needsPath){
    ::BeginPath((HDC)ctx);
    }
  if(1360<=npoints){
    ::MoveToEx((HDC)ctx,points[0].x,points[0].y,NULL);
    for(i=1; i<npoints; i++) ::LineTo((HDC)ctx,points[i].x,points[i].y);
    }
  else{
    for(i=0; i<npoints; i++){
      pts[i].x=points[i].x;
      pts[i].y=points[i].y;
      }
    ::Polyline((HDC)ctx,pts,npoints);
    }
  if(needsPath){
    ::EndPath((HDC)ctx);
    ::StrokePath((HDC)ctx);
    }
  }


// Draw lines relative
void FXDCWindow::drawLinesRel(const FXPoint* points,FXuint npoints){
  register int x=0,y=0;
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::drawLinesRel: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  if(needsPath){
    ::BeginPath((HDC)ctx);
    }
  if(1360<=npoints){
    ::MoveToEx((HDC)ctx,points[0].x,points[0].y,NULL);
    for(i=1; i<npoints; i++){
      x+=points[i].x;
      y+=points[i].y;
      ::LineTo((HDC)ctx,x,y);
      }
    }
  else{
    for(i=0; i<npoints; i++){
      x+=points[i].x; pts[i].x=x;
      y+=points[i].y; pts[i].y=y;
      }
    ::Polyline((HDC)ctx,pts,npoints);
    }
  if(needsPath){
    ::EndPath((HDC)ctx);
    ::StrokePath((HDC)ctx);
    }
  }


// Draw line segments
void FXDCWindow::drawLineSegments(const FXSegment* segments,FXuint nsegments){
  register FXuint i;
  POINT pts[2];
  if(!surface){ fxerror("FXDCWindow::drawLineSegments: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  if(needsPath){
    ::BeginPath((HDC)ctx);
    }
  for(i=0; i<nsegments; i++){
    pts[0].x=segments[i].x1; pts[0].y=segments[i].y1;
    pts[1].x=segments[i].x2; pts[1].y=segments[i].y2;
    ::Polyline((HDC)ctx,pts,2);
    }
  if(needsPath){
    ::EndPath((HDC)ctx);
    ::StrokePath((HDC)ctx);
    }
  }


// Unfilled rectangle
void FXDCWindow::drawRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::drawRectangle: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  HBRUSH hbrush=(HBRUSH)::SelectObject((HDC)ctx,(HBRUSH)GetStockObject(NULL_BRUSH));
  ::Rectangle((HDC)ctx,x,y,x+w+1,y+h+1);
  ::SelectObject((HDC)ctx,hbrush);
  }


// Draw unfilled rectangles
void FXDCWindow::drawRectangles(const FXRectangle* rectangles,FXuint nrectangles){
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::drawRectangles: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  HBRUSH hbrush=(HBRUSH)::SelectObject((HDC)ctx,(HBRUSH)GetStockObject(NULL_BRUSH));
  for(i=0; i<nrectangles; i++){
    ::Rectangle((HDC)ctx,rectangles[i].x,rectangles[i].y,rectangles[i].x+rectangles[i].w+1,rectangles[i].y+rectangles[i].h+1);
    }
  ::SelectObject((HDC)ctx,hbrush);
  }


// Unfilled rounded rectangle
void FXDCWindow::drawRoundRectangle(FXint x,FXint y,FXint w,FXint h,FXint ew,FXint eh){
  if(!surface){ fxerror("FXDCWindow::drawRoundRectangle: DC not connected to drawable.\n"); }
  if(needsNewPen) updatePen();
  HBRUSH hbrush=(HBRUSH)::SelectObject((HDC)ctx,(HBRUSH)GetStockObject(NULL_BRUSH));
  if(ew+ew>w) ew=w>>1;
  if(eh+eh>h) eh=h>>1;
  ::RoundRect((HDC)ctx,x,y,x+w+1,y+h+1,ew,eh);
  ::SelectObject((HDC)ctx,hbrush);
  }


// Draw arc; angles in degrees*64, ang2 relative to ang1
// If angle is negative flip the start and end; also, if ang2 is zero,
// don't draw anything at all (patch: Sander Jansen <sander@knology.net>).
void FXDCWindow::drawArc(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  register FXbool reversed=(ang2<0);
  if(!surface){ fxerror("FXDCWindow::drawArc: DC not connected to drawable.\n"); }
  if(ang2==0) return;
  if(needsNewPen) updatePen();
  ang2+=ang1;
  w+=1;
  h+=1;
  int xStart=int(x+0.5*w+w*cos(ang1*PI/(180.0*64.0)));
  int yStart=int(y+0.5*h-h*sin(ang1*PI/(180.0*64.0)));
  int xEnd=int(x+0.5*w+w*cos(ang2*PI/(180.0*64.0)));
  int yEnd=int(y+0.5*h-h*sin(ang2*PI/(180.0*64.0)));
  if(needsPath){
    ::BeginPath((HDC)ctx);
    }
  if(reversed)
    ::Arc((HDC)ctx,x,y,x+w,y+h,xEnd,yEnd,xStart,yStart);
  else
    ::Arc((HDC)ctx,x,y,x+w,y+h,xStart,yStart,xEnd,yEnd);
  if(needsPath){
    ::EndPath((HDC)ctx);
    ::StrokePath((HDC)ctx);
    }
  }


// Draw arcs
void FXDCWindow::drawArcs(const FXArc* arcs,FXuint narcs){
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::drawArcs: DC not connected to drawable.\n"); }
  for(i=0; i<narcs; i++){
    drawArc(arcs[i].x,arcs[i].y,arcs[i].w,arcs[i].h,arcs[i].a,arcs[i].b);
    }
  }


// Draw ellipse
void FXDCWindow::drawEllipse(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::drawEllipse: DC not connected to drawable.\n"); }
  if(needsNewBrush) updateBrush();
  if(needsNewPen) updatePen();
  w+=1;
  h+=1;
  if(needsPath){
    ::BeginPath((HDC)ctx);
    }
  ::Arc((HDC)ctx,x,y,x+w,y+h,x+(w>>1),y+(h>>1),x+(w>>1),y+(h>>1));
  if(needsPath){
    ::EndPath((HDC)ctx);
    ::StrokePath((HDC)ctx);
    }
  }


// Fill using currently selected ROP code
void FXDCWindow::fillRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::fillRectangle: DC not connected to drawable.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  ::Rectangle((HDC)ctx,x,y,x+w+1,y+h+1);
  ::SelectObject((HDC)ctx,hpen);
  }


// Fill using currently selected ROP code
void FXDCWindow::fillRectangles(const FXRectangle* rectangles,FXuint nrectangles){
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::fillRectangles: DC not connected to drawable.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<nrectangles; i++){
    ::Rectangle((HDC)ctx,rectangles[i].x,rectangles[i].y,rectangles[i].x+rectangles[i].w+1,rectangles[i].y+rectangles[i].h+1);
    }
  ::SelectObject((HDC)ctx,hpen);
  }


// Fill using currently selected ROP mode
void FXDCWindow::fillRoundRectangle(FXint x,FXint y,FXint w,FXint h,FXint ew,FXint eh){
  if(!surface){ fxerror("FXDCWindow::fillRoundRectangle: DC not connected to drawable.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  if(ew+ew>w) ew=w>>1;
  if(eh+eh>h) eh=h>>1;
  ::RoundRect((HDC)ctx,x,y,x+w+1,y+h+1,ew,eh);
  ::SelectObject((HDC)ctx,hpen);
  }


// Fill chord
void FXDCWindow::fillChord(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  register FXbool reversed=(ang2<0);
  if(!surface){ fxerror("FXDCWindow::fillChord: DC not connected to drawable.\n"); }
  if(ang2==0) return;
  if(needsNewBrush) updateBrush();
  ang2+=ang1;
  w+=1;
  h+=1;
  int xStart=int(x+0.5*w+w*cos(ang1*PI/(180.0*64.0)));
  int yStart=int(y+0.5*h-h*sin(ang1*PI/(180.0*64.0)));
  int xEnd=int(x+0.5*w+w*cos(ang2*PI/(180.0*64.0)));
  int yEnd=int(y+0.5*h-h*sin(ang2*PI/(180.0*64.0)));
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  if(reversed)
    ::Chord((HDC)ctx,x,y,x+w,y+h,xEnd,yEnd,xStart,yStart);
  else
    ::Chord((HDC)ctx,x,y,x+w,y+h,xStart,yStart,xEnd,yEnd);
  ::SelectObject((HDC)ctx,hpen);
  }


// Fill chords
void FXDCWindow::fillChords(const FXArc* chords,FXuint nchords){
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::fillChords: DC not connected to drawable.\n"); }
  for(i=0; i<nchords; i++){
    fillChord(chords[i].x,chords[i].y,chords[i].w,chords[i].h,chords[i].a,chords[i].b);
    }
  }


// Draw filled arc; angles are in degrees*64; ang2 is relative from ang1
// If angle is negative flip the start and end; also, if ang2 is zero,
// don't draw anything at all (patch: Sander Jansen <sander@knology.net>).
void FXDCWindow::fillArc(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  register FXbool reversed=(ang2<0);
  if(!surface){ fxerror("FXDCWindow::fillArc: DC not connected to drawable.\n"); }
  if(ang2==0) return;
  if(needsNewBrush) updateBrush();
  ang2+=ang1;
  w+=1;
  h+=1;
  int xStart=int(x+0.5*w+w*cos(ang1*PI/(180.0*64.0)));
  int yStart=int(y+0.5*h-h*sin(ang1*PI/(180.0*64.0)));
  int xEnd=int(x+0.5*w+w*cos(ang2*PI/(180.0*64.0)));
  int yEnd=int(y+0.5*h-h*sin(ang2*PI/(180.0*64.0)));
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  if(reversed)
    ::Pie((HDC)ctx,x,y,x+w,y+h,xEnd,yEnd,xStart,yStart);
  else
    ::Pie((HDC)ctx,x,y,x+w,y+h,xStart,yStart,xEnd,yEnd);
  ::SelectObject((HDC)ctx,hpen);
  }

//Ellipse((HDC)ctx,x,y,x+w,y+h);

// Fill arcs
void FXDCWindow::fillArcs(const FXArc* arcs,FXuint narcs){
  register FXuint i;
  if(!surface){ fxerror("FXDCWindow::fillArcs: DC not connected to drawable.\n"); }
  for(i=0; i<narcs; i++){
    fillArc(arcs[i].x,arcs[i].y,arcs[i].w,arcs[i].h,arcs[i].a,arcs[i].b);
    }
  }


// Fill ellipse
void FXDCWindow::fillEllipse(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::fillEllipse: DC not connected to drawable.\n"); }
  if(needsNewBrush) updateBrush();
  w+=1;
  h+=1;
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  ::Pie((HDC)ctx,x,y,x+w,y+h,x+(w>>1),y+(h>>1),x+(w>>1),y+(h>>1));
  ::SelectObject((HDC)ctx,hpen);
  }


// Filled simple polygon
void FXDCWindow::fillPolygon(const FXPoint* points,FXuint npoints){
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::fillPolygon: DC not connected to drawable.\n"); }
  if(npoints>=1360){ fxerror("FXDCWindow::fillPolygon: too many points.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<npoints; i++){
    pts[i].x=points[i].x;
    pts[i].y=points[i].y;
    }
  ::Polygon((HDC)ctx,pts,npoints);
  ::SelectObject((HDC)ctx,hpen);
  }


// Filled concave polygon
void FXDCWindow::fillConcavePolygon(const FXPoint* points,FXuint npoints){
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::fillConcavePolygon: DC not connected to drawable.\n"); }
  if(npoints>=1360){ fxerror("FXDCWindow::fillConcavePolygon: too many points.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<npoints; i++){
    pts[i].x=points[i].x;
    pts[i].y=points[i].y;
    }
  ::Polygon((HDC)ctx,pts,npoints);
  ::SelectObject((HDC)ctx,hpen);
  }


// Filled complex polygon relative
void FXDCWindow::fillComplexPolygon(const FXPoint* points,FXuint npoints){
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::fillComplexPolygon: DC not connected to drawable.\n"); }
  if(npoints>=1360){ fxerror("FXDCWindow::fillComplexPolygon: too many points.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<npoints; i++){
    pts[i].x=points[i].x;
    pts[i].y=points[i].y;
    }
  ::Polygon((HDC)ctx,pts,npoints);
  ::SelectObject((HDC)ctx,hpen);
  }


// Filled simple polygon with relative points
void FXDCWindow::fillPolygonRel(const FXPoint* points,FXuint npoints){
  register int x=0,y=0;
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::fillPolygonRel: DC not connected to drawable.\n"); }
  if(npoints>=1360){ fxerror("FXDCWindow::fillPolygonRel: too many points.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<npoints; i++){
    x+=points[i].x; pts[i].x=x;
    y+=points[i].y; pts[i].y=y;
    }
  ::Polygon((HDC)ctx,pts,npoints);
  ::SelectObject((HDC)ctx,hpen);
  }


// Filled concave polygon relative
void FXDCWindow::fillConcavePolygonRel(const FXPoint* points,FXuint npoints){
  register int x=0,y=0;
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::fillConcavePolygonRel: DC not connected to drawable.\n"); }
  if(npoints>=1360){ fxerror("FXDCWindow::fillConcavePolygonRel: too many points.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<npoints; i++){
    x+=points[i].x; pts[i].x=x;
    y+=points[i].y; pts[i].y=y;
    }
  ::Polygon((HDC)ctx,pts,npoints);
  ::SelectObject((HDC)ctx,hpen);
  }


// Filled complex polygon relative
void FXDCWindow::fillComplexPolygonRel(const FXPoint* points,FXuint npoints){
  register int x=0,y=0;
  register FXuint i;
  POINT pts[1360];      // Worst case limit according to MSDN
  if(!surface){ fxerror("FXDCWindow::fillComplexPolygonRel: DC not connected to drawable.\n"); }
  if(npoints>=1360){ fxerror("FXDCWindow::fillComplexPolygonRel: too many points.\n"); }
  if(needsNewBrush) updateBrush();
  HPEN hpen=(HPEN)::SelectObject((HDC)ctx,GetStockObject(NULL_PEN));
  for(i=0; i<npoints; i++){
    x+=points[i].x; pts[i].x=x;
    y+=points[i].y; pts[i].y=y;
    }
  ::Polygon((HDC)ctx,pts,npoints);
  ::SelectObject((HDC)ctx,hpen);
  }


// Set text font
void FXDCWindow::setFont(FXFont *fnt){
  if(!surface){ fxerror("FXDCWindow::setFont: DC not connected to drawable.\n"); }
  if(!fnt || !fnt->id()){ fxerror("FXDCWindow::setFont: illegal or NULL font specified.\n"); }
  ::SelectObject((HDC)ctx,fnt->id());
  font=fnt;
  }


// Draw string with base line starting at x, y
void FXDCWindow::drawText(FXint x,FXint y,const FXchar* string,FXuint length){
  if(!surface){ fxerror("FXDCWindow::drawText: DC not connected to drawable.\n"); }
  if(!font){ fxerror("FXDCWindow::drawText: no font selected.\n"); }
  FXnchar sbuffer[4096];
  FXint count=utf2ncs(sbuffer,string,FXMIN(length,4096));
  FXASSERT(count<=length);
  FXint bkmode=::SetBkMode((HDC)ctx,TRANSPARENT);
  ::TextOutW((HDC)ctx,x,y,sbuffer,count);
  ::SetBkMode((HDC)ctx,bkmode);
  }


// Draw text starting at x, y over filled background
void FXDCWindow::drawImageText(FXint x,FXint y,const FXchar* string,FXuint length){
  if(!surface){ fxerror("FXDCWindow::drawImageText: DC not connected to drawable.\n"); }
  if(!font){ fxerror("FXDCWindow::drawImageText: no font selected.\n"); }
  FXnchar sbuffer[4096];
  FXint count=utf2ncs(sbuffer,string,FXMIN(length,4096));
  FXASSERT(count<=length);
  FXint bkmode=::SetBkMode((HDC)ctx,OPAQUE);
  ::TextOutW((HDC)ctx,x,y,sbuffer,count);
//    RECT r;
//    r.left=clip.x; r.top=clip.y; r.right=clip.x+clip.w; r.bottom=clip.y+clip.h;
//    ExtTextOutW((HDC)ctx,x,y,ETO_OPAQUE|ETO_CLIPPED,&r,sbuffer,count,NULL);
  ::SetBkMode((HDC)ctx,bkmode);
  }


// Draw string with base line starting at x, y
void FXDCWindow::drawText(FXint x,FXint y,const FXString& string){
  drawText(x,y,string.text(),string.length());
  }


// Draw text starting at x, y over filled background
void FXDCWindow::drawImageText(FXint x,FXint y,const FXString& string){
  drawImageText(x,y,string.text(),string.length());
  }


// Draw area from source
// Some of these ROP codes do not have names; the full list can be found in the MSDN docs
// at Platform SDK/Reference/Appendixes/Win32 Appendixes/Raster Operation Codes/Ternary Raster Operations
void FXDCWindow::drawArea(const FXDrawable* source,FXint sx,FXint sy,FXint sw,FXint sh,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawArea: DC not connected to drawable.\n"); }
  if(!source || !source->id()){ fxerror("FXDCWindow::drawArea: illegal source specified.\n"); }
  HDC shdc=(HDC)source->GetDC();
  switch(rop){
    case BLT_CLR:                     // D := 0
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,BLACKNESS);
      break;
    case BLT_SRC_AND_DST:             // D := S & D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,SRCAND);
      break;
    case BLT_SRC_AND_NOT_DST:         // D := S & ~D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,SRCERASE);
      break;
    case BLT_SRC:                     // D := S
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,SRCCOPY);
      break;
    case BLT_NOT_SRC_AND_DST:         // D := ~S & D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,0x220326);
      break;
    case BLT_DST:                     // D := D
      break;
    case BLT_SRC_XOR_DST:             // D := S ^ D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,SRCINVERT);
      break;
    case BLT_SRC_OR_DST:              // D := S | D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,SRCPAINT);
      break;
    case BLT_NOT_SRC_AND_NOT_DST:     // D := ~S & ~D  ==  D := ~(S | D)
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,NOTSRCERASE);
      break;
    case BLT_NOT_SRC_XOR_DST:         // D := ~S ^ D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,0x990066); // Not sure about this one
      break;
    case BLT_NOT_DST:                 // D := ~D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,DSTINVERT);
      break;
    case BLT_SRC_OR_NOT_DST:          // D := S | ~D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,0xDD0228);
      break;
    case BLT_NOT_SRC:                 // D := ~S
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,NOTSRCCOPY);
      break;
    case BLT_NOT_SRC_OR_DST:          // D := ~S | D
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,MERGEPAINT);
      break;
    case BLT_NOT_SRC_OR_NOT_DST:      // D := ~S | ~D  ==  ~(S & D)
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,0x7700E6);
      break;
    case BLT_SET:                     // D := 1
      ::BitBlt((HDC)ctx,dx,dy,sw,sh,shdc,sx,sy,WHITENESS);
      break;
    }
  source->ReleaseDC(shdc);
  }


// Draw area stretched area from source
void FXDCWindow::drawArea(const FXDrawable* source,FXint sx,FXint sy,FXint sw,FXint sh,FXint dx,FXint dy,FXint dw,FXint dh){
  if(!surface){ fxerror("FXDCWindow::drawArea: DC not connected to drawable.\n"); }
  if(!source || !source->id()){ fxerror("FXDCWindow::drawArea: illegal source specified.\n"); }
  HDC shdc=(HDC)source->GetDC();
  switch(rop){
    case BLT_CLR:                     // D := 0
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,BLACKNESS);
      break;
    case BLT_SRC_AND_DST:             // D := S & D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,SRCAND);
      break;
    case BLT_SRC_AND_NOT_DST:         // D := S & ~D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,SRCERASE);
      break;
    case BLT_SRC:                     // D := S
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,SRCCOPY);
      break;
    case BLT_NOT_SRC_AND_DST:         // D := ~S & D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,0x220326);
      break;
    case BLT_DST:                     // D := D
      break;
    case BLT_SRC_XOR_DST:             // D := S ^ D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,SRCINVERT);
      break;
    case BLT_SRC_OR_DST:              // D := S | D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,SRCPAINT);
      break;
    case BLT_NOT_SRC_AND_NOT_DST:     // D := ~S & ~D ==  D := ~(S | D)
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,NOTSRCERASE);
      break;
    case BLT_NOT_SRC_XOR_DST:         // D := ~S ^ D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,0x990066); // Not sure about this one
      break;
    case BLT_NOT_DST:                 // D := ~D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,DSTINVERT);
      break;
    case BLT_SRC_OR_NOT_DST:          // D := S | ~D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,0xDD0228);
      break;
    case BLT_NOT_SRC:                 // D := ~S
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,NOTSRCCOPY);
      break;
    case BLT_NOT_SRC_OR_DST:          // D := ~S | D
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,MERGEPAINT);
      break;
    case BLT_NOT_SRC_OR_NOT_DST:      // D := ~S | ~D ==  ~(S & D)
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,0x7700E6);
      break;
    case BLT_SET:                     // D := 1
      ::StretchBlt((HDC)ctx,dx,dy,dw,dh,shdc,sx,sy,sw,sh,WHITENESS);
      break;
    }
  source->ReleaseDC(shdc);
  }


// Draw image
void FXDCWindow::drawImage(const FXImage* image,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawImage: DC not connected to drawable.\n"); }
  if(!image || !image->id()){ fxerror("FXDCWindow::drawImage: illegal image specified.\n"); }
  HDC dcMem=(HDC)image->GetDC();
  switch(rop){
    case BLT_CLR:                     // D := 0
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,BLACKNESS);
      break;
    case BLT_SRC_AND_DST:             // D := S & D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,SRCAND);
      break;
    case BLT_SRC_AND_NOT_DST:         // D := S & ~D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,SRCERASE);
      break;
    case BLT_SRC:                     // D := S
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,SRCCOPY);
      break;
    case BLT_NOT_SRC_AND_DST:         // D := ~S & D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,0x220326);
      break;
    case BLT_DST:                     // D := D
      break;
    case BLT_SRC_XOR_DST:             // D := S ^ D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,SRCINVERT);
      break;
    case BLT_SRC_OR_DST:              // D := S | D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,SRCPAINT);
      break;
    case BLT_NOT_SRC_AND_NOT_DST:     // D := ~S & ~D  ==  D := ~(S | D)
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,NOTSRCERASE);
      break;
    case BLT_NOT_SRC_XOR_DST:         // D := ~S ^ D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,0x990066); // Not sure about this one
      break;
    case BLT_NOT_DST:                 // D := ~D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,DSTINVERT);
      break;
    case BLT_SRC_OR_NOT_DST:          // D := S | ~D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,0xDD0228);
      break;
    case BLT_NOT_SRC:                 // D := ~S
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,NOTSRCCOPY);
      break;
    case BLT_NOT_SRC_OR_DST:          // D := ~S | D
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,MERGEPAINT);
      break;
    case BLT_NOT_SRC_OR_NOT_DST:      // D := ~S | ~D  ==  ~(S & D)
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,0x7700E6);
      break;
    case BLT_SET:                     // D := 1
      ::BitBlt((HDC)ctx,dx,dy,image->width,image->height,dcMem,0,0,WHITENESS);
      break;
    }
  image->ReleaseDC(dcMem);
  }


// Draw bitmap
void FXDCWindow::drawBitmap(const FXBitmap* bitmap,FXint dx,FXint dy) {
  if(!surface) fxerror("FXDCWindow::drawBitmap: DC not connected to drawable.\n");
  if(!bitmap || !bitmap->id()) fxerror("FXDCWindow::drawBitmap: illegal bitmap specified.\n");
  HDC dcMem=(HDC)bitmap->GetDC();
  switch(rop){
    case BLT_CLR:                     // D := 0
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,BLACKNESS);
      break;
    case BLT_SRC_AND_DST:             // D := S & D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,SRCAND);
      break;
    case BLT_SRC_AND_NOT_DST:         // D := S & ~D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,SRCERASE);
      break;
    case BLT_SRC:                     // D := S
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,SRCCOPY);
      break;
    case BLT_NOT_SRC_AND_DST:         // D := ~S & D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,0x220326);
      break;
    case BLT_DST:                     // D := D
      break;
    case BLT_SRC_XOR_DST:             // D := S ^ D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,SRCINVERT);
      break;
    case BLT_SRC_OR_DST:              // D := S | D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,SRCPAINT);
      break;
    case BLT_NOT_SRC_AND_NOT_DST:     // D := ~S & ~D  ==  D := ~(S | D)
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,NOTSRCERASE);
      break;
    case BLT_NOT_SRC_XOR_DST:         // D := ~S ^ D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,0x990066); // Not sure about this one
      break;
    case BLT_NOT_DST:                 // D := ~D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,DSTINVERT);
      break;
    case BLT_SRC_OR_NOT_DST:          // D := S | ~D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,0xDD0228);
      break;
    case BLT_NOT_SRC:                 // D := ~S
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,NOTSRCCOPY);
      break;
    case BLT_NOT_SRC_OR_DST:          // D := ~S | D
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,MERGEPAINT);
      break;
    case BLT_NOT_SRC_OR_NOT_DST:      // D := ~S | ~D  ==  ~(S & D)
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,0x7700E6);
      break;
    case BLT_SET:                     // D := 1
      ::BitBlt((HDC)ctx,dx,dy,bitmap->width,bitmap->height,dcMem,0,0,WHITENESS);
      break;
    }
  bitmap->ReleaseDC(dcMem);
  }


// Draw icon
void FXDCWindow::drawIcon(const FXIcon* icon,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawIcon: DC not connected to drawable.\n"); }
  if(!icon || !icon->id() || !icon->shape){ fxerror("FXDCWindow::drawIcon: illegal icon specified.\n"); }
  HDC hdcsrc=(HDC)icon->GetDC();
  if(icon->getOptions()&IMAGE_OPAQUE){
    ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcsrc,0,0,SRCCOPY);
    }
  else{
    HDC hdcmsk=::CreateCompatibleDC((HDC)ctx);
    HBITMAP holdbmp=(HBITMAP)::SelectObject(hdcmsk,(HBITMAP)icon->shape);
    COLORREF coldback=::SetBkColor((HDC)ctx,RGB(255,255,255));
    COLORREF coldtext=::SetTextColor((HDC)ctx,RGB(0,0,0));
    ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcmsk,0,0,SRCAND);
    ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcsrc,0,0,SRCPAINT);
    ::SelectObject(hdcmsk,holdbmp);
    ::DeleteDC(hdcmsk);
    ::SetBkColor((HDC)ctx,coldback);
    ::SetTextColor((HDC)ctx,coldtext);
    }
  icon->ReleaseDC(hdcsrc);
  }


// This may be done faster, I suspect; but I'm tired of looking at this now;
// at least it's correct as it stands..
void FXDCWindow::drawIconShaded(const FXIcon* icon,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawIconShaded: DC not connected to drawable.\n"); }
  if(!icon || !icon->id() || !icon->shape){ fxerror("FXDCWindow::drawIconShaded: illegal icon specified.\n"); }
  FXColor selbackColor=getApp()->getSelbackColor();
  HDC hdcsrc=(HDC)icon->GetDC();
  HDC hdcmsk=::CreateCompatibleDC((HDC)ctx);

  // Set shape mask
  HBITMAP holdbmp=(HBITMAP)::SelectObject(hdcmsk,(HBITMAP)icon->shape);

  // Set colors
  COLORREF coldback=::SetBkColor((HDC)ctx,RGB(255,255,255));
  COLORREF coldtext=::SetTextColor((HDC)ctx,RGB(0,0,0));

  // Paint icon
  ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcmsk,0,0,SRCAND);
  ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcsrc,0,0,SRCPAINT);

  // Select brush
  HBRUSH hbrush=::CreatePatternBrush((HBITMAP)getApp()->stipples[STIPPLE_GRAY]);
  HBRUSH holdbrush=(HBRUSH)::SelectObject((HDC)ctx,hbrush);
  ::SetBrushOrgEx((HDC)ctx,dx,dy,NULL);

  // Make black where pattern is 0 and shape is 0 [DPSoa]
  ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcmsk,0,0,0x00A803A9);

  // Set colors
  ::SetTextColor((HDC)ctx,RGB(FXREDVAL(selbackColor),FXGREENVAL(selbackColor),FXBLUEVAL(selbackColor)));
  ::SetBkColor((HDC)ctx,RGB(0,0,0));

  // Make selbackcolor where pattern is 0 and shape is 0 [DPSoo]
  ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcmsk,0,0,0x00FE02A9);

  // Resetore ctx
  ::SelectObject(hdcmsk,holdbmp);
  ::DeleteDC(hdcmsk);
  ::SelectObject((HDC)ctx,holdbrush);
  ::DeleteObject(hbrush);
  ::SetBkColor((HDC)ctx,coldback);
  ::SetTextColor((HDC)ctx,coldtext);
  ::SetBrushOrgEx((HDC)ctx,tx,ty,NULL);
  icon->ReleaseDC(hdcsrc);
  }



// Draw a sunken or etched-in icon.
void FXDCWindow::drawIconSunken(const FXIcon* icon,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::drawIconSunken: DC not connected to drawable.\n"); }
  if(!icon || !icon->id() || !icon->shape){ fxerror("FXDCWindow::drawIconSunken: illegal icon specified.\n"); }
  FXColor shadowColor=getApp()->getShadowColor();
  FXColor hiliteColor=getApp()->getHiliteColor();
  HDC hdcsrc=(HDC)icon->GetDC();
  HDC hdcmono=::CreateCompatibleDC((HDC)ctx);

  // Set etch mask
  HBITMAP holdbmp=(HBITMAP)::SelectObject(hdcmono,(HBITMAP)icon->etch);

  // Set colors
  COLORREF coldback=::SetBkColor((HDC)ctx,RGB(255,255,255));
  COLORREF coldtext=::SetTextColor((HDC)ctx,RGB(0,0,0));

  // While brush colors apply to the pattern
  HBRUSH hbrhilite=::CreateSolidBrush(RGB(FXREDVAL(hiliteColor),FXGREENVAL(hiliteColor),FXBLUEVAL(hiliteColor)));
  HBRUSH holdbrush=(HBRUSH)::SelectObject((HDC)ctx,hbrhilite);

  // BitBlt the black bits in the monochrome bitmap into highlight colors
  // in the destination DC (offset a bit). This BitBlt(), and the next one,
  // use an unnamed raster op (0xB8074a) whose effect is D := ((D ^ P) & S) ^ P.
  // Or at least I think it is ;) The code = PSDPxax, so that's correct JVZ
  ::BitBlt((HDC)ctx,dx+1,dy+1,icon->getWidth(),icon->getHeight(),hdcmono,0,0,0xB8074A);
  HBRUSH hbrshadow=::CreateSolidBrush(RGB(FXREDVAL(shadowColor),FXGREENVAL(shadowColor),FXBLUEVAL(shadowColor)));
  ::SelectObject((HDC)ctx,hbrshadow);

  // Now BitBlt the black bits in the monochrome bitmap into the
  // shadow color on the destination DC.
  ::BitBlt((HDC)ctx,dx,dy,icon->getWidth(),icon->getHeight(),hdcmono,0,0,0xB8074A);

  // Resetore ctx
  ::SelectObject(hdcmono,holdbmp);
  ::DeleteDC(hdcmono);
  ::SelectObject((HDC)ctx,holdbrush);
  ::DeleteObject(hbrhilite);
  ::DeleteObject(hbrshadow);
  ::SetBkColor((HDC)ctx,coldback);
  ::SetTextColor((HDC)ctx,coldtext);
  icon->ReleaseDC(hdcsrc);
  }


// Draw hash box
void FXDCWindow::drawHashBox(FXint x,FXint y,FXint w,FXint h,FXint b){
  if(!surface){ fxerror("FXDCWindow::drawHashBox: DC not connected to drawable.\n"); }
  HBRUSH hbrush=::CreatePatternBrush((HBITMAP)getApp()->stipples[STIPPLE_GRAY]);
  HBRUSH holdbrush=(HBRUSH)::SelectObject((HDC)ctx,hbrush);
  COLORREF coldback=::SetBkColor((HDC)ctx,RGB(255,255,255));
  COLORREF coldtext=::SetTextColor((HDC)ctx,RGB(0,0,0));
  ::PatBlt((HDC)ctx,x,y,w-b,b,PATINVERT);
  ::PatBlt((HDC)ctx,x+w-b,y,b,h-b,PATINVERT);
  ::PatBlt((HDC)ctx,x+b,y+h-b,w-b,b,PATINVERT);
  ::PatBlt((HDC)ctx,x,y+b,b,h-b,PATINVERT);
  ::SelectObject((HDC)ctx,holdbrush);
  ::DeleteObject(hbrush);
  ::SetBkColor((HDC)ctx,coldback);
  ::SetTextColor((HDC)ctx,coldtext);
  }


// Draw focus rectangle
void FXDCWindow::drawFocusRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::drawFocusRectangle: DC not connected to drawable.\n"); }
  HBRUSH hbrush=::CreatePatternBrush((HBITMAP)getApp()->stipples[STIPPLE_GRAY]);
  HBRUSH holdbrush=(HBRUSH)::SelectObject((HDC)ctx,hbrush);
  COLORREF coldback=::SetBkColor((HDC)ctx,RGB(255,255,255));
  COLORREF coldtext=::SetTextColor((HDC)ctx,RGB(0,0,0));
  ::SetBrushOrgEx((HDC)ctx,x,y,NULL);
  ::PatBlt((HDC)ctx,x,y,w-1,1,PATINVERT);
  ::PatBlt((HDC)ctx,x+w-1,y,1,h-1,PATINVERT);
  ::PatBlt((HDC)ctx,x+1,y+h-1,w-1,1,PATINVERT);
  ::PatBlt((HDC)ctx,x,y+1,1,h-1,PATINVERT);
  ::SelectObject((HDC)ctx,holdbrush);
  ::DeleteObject(hbrush);
  ::SetBkColor((HDC)ctx,coldback);
  ::SetTextColor((HDC)ctx,coldtext);
  ::SetBrushOrgEx((HDC)ctx,tx,ty,NULL);
  }


static DWORD FXStipplePattern2Hatch(FXStipplePattern pat){
  switch(pat){
    case STIPPLE_HORZ: return HS_HORIZONTAL;
    case STIPPLE_VERT: return HS_VERTICAL;
    case STIPPLE_CROSS: return HS_CROSS;
    case STIPPLE_DIAG: return HS_BDIAGONAL;
    case STIPPLE_REVDIAG: return HS_FDIAGONAL;
    case STIPPLE_CROSSDIAG: return HS_DIAGCROSS;
    default: return HS_CROSS;
    }
  }


void FXDCWindow::updatePen(){
  DWORD dashes[32];
  DWORD penstyle,i;
  LOGBRUSH lb;

  // Setup brush of this pen
  switch(fill){
    case FILL_SOLID:
      lb.lbStyle=BS_SOLID;
      lb.lbColor=devfg;
      lb.lbHatch=0;
      break;
    case FILL_TILED:
      lb.lbStyle=BS_SOLID;
      lb.lbColor=devfg;
      lb.lbHatch=0;
      break;
    case FILL_STIPPLED:
      if(stipple){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)stipple->id();    // This should be a HBITMAP
        }
      else if(pattern>=STIPPLE_0 && pattern<=STIPPLE_16){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)getApp()->stipples[pattern];
        }
      else{
        lb.lbStyle=BS_HATCHED;
        lb.lbColor=devfg;
        lb.lbHatch=FXStipplePattern2Hatch(pattern);
        }
      break;
    case FILL_OPAQUESTIPPLED:
      if(stipple){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)stipple->id();    // This should be a HBITMAP
        }
      else if(pattern>=STIPPLE_0 && pattern<=STIPPLE_16){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)getApp()->stipples[pattern];
        }
      else{
        lb.lbStyle=BS_HATCHED;
        lb.lbColor=devfg;
        lb.lbHatch=FXStipplePattern2Hatch(pattern);
        }
      break;
    }

  penstyle=0;

  // Cap style
  if(cap==CAP_ROUND)
    penstyle|=PS_JOIN_ROUND;
  else if(cap==CAP_PROJECTING)
    penstyle|=PS_ENDCAP_SQUARE;
  else
    penstyle|=PS_ENDCAP_FLAT;

  // Join style
  if(join==JOIN_MITER)
    penstyle|=PS_JOIN_MITER;
  else if(join==JOIN_ROUND)
    penstyle|=PS_JOIN_ROUND;
  else
    penstyle|=PS_JOIN_BEVEL;

  // Kind of pen
  //if(width<=1 && fill==FILL_SOLID)
  //  penstyle|=PS_COSMETIC;
  //else
    penstyle|=PS_GEOMETRIC;

  // Line style
  if(style==LINE_SOLID){
    penstyle|=PS_SOLID;
    ::DeleteObject(::SelectObject((HDC)ctx,::ExtCreatePen(penstyle,width,&lb,0,NULL)));
    }
  else if(dashoff==0 && dashlen==2 && dashpat[0]==1 && dashpat[1]==1){
    penstyle|=PS_DOT;
    ::DeleteObject(::SelectObject((HDC)ctx,::ExtCreatePen(penstyle,width,&lb,0,NULL)));
    }
  else if(dashoff==0 && dashlen==2 && dashpat[0]==3 && dashpat[1]==1){
    penstyle|=PS_DASH;
    ::DeleteObject(::SelectObject((HDC)ctx,::ExtCreatePen(penstyle,width,&lb,0,NULL)));
    }
  else if(dashoff==0 && dashlen==4 && dashpat[0]==3 && dashpat[1]==1 && dashpat[2]==1 && dashpat[3]==1){
    penstyle|=PS_DASHDOT;
    ::DeleteObject(::SelectObject((HDC)ctx,::ExtCreatePen(penstyle,width,&lb,0,NULL)));
    }
  else if(dashoff==0 && dashlen==6 && dashpat[0]==3 && dashpat[1]==1 && dashpat[2]==1 && dashpat[3]==1 && dashpat[4]==1 && dashpat[5]==1){
    penstyle|=PS_DASHDOTDOT;
    ::DeleteObject(::SelectObject((HDC)ctx,::ExtCreatePen(penstyle,width,&lb,0,NULL)));
    }
  else{
    penstyle|=PS_USERSTYLE;
    for(i=0; i<dashlen; i++){ dashes[i]=dashpat[(i+dashoff)%dashlen]; }
    ::DeleteObject(::SelectObject((HDC)ctx,::ExtCreatePen(penstyle,width,&lb,dashlen,dashes)));
    }
  if(fill==FILL_STIPPLED){
    ::SetBkMode((HDC)ctx,TRANSPARENT);         // Alas, only works for BS_HATCHED...
    }
  else{
    ::SetBkMode((HDC)ctx,OPAQUE);
    }
  if(fill!=FILL_SOLID){
    ::SetBrushOrgEx((HDC)ctx,tx,ty,NULL);
    }
  needsPath=(width>1);
  needsNewPen=FALSE;
  }


void FXDCWindow::updateBrush(){
  LOGBRUSH lb;
  switch(fill){
    case FILL_SOLID:
      lb.lbStyle=BS_SOLID;
      lb.lbColor=devfg;
      lb.lbHatch=0;
      ::DeleteObject(::SelectObject((HDC)ctx,::CreateBrushIndirect(&lb)));
      break;
    case FILL_TILED:
      if(tile){
        ::DeleteObject(::SelectObject((HDC)ctx,::CreatePatternBrush((HBITMAP)tile->id())));
        }
      else{
        lb.lbStyle=BS_SOLID;
        lb.lbColor=devfg;
        lb.lbHatch=0;
        ::DeleteObject(::SelectObject((HDC)ctx,::CreateBrushIndirect(&lb)));
        }
      break;
    case FILL_STIPPLED:
      if(stipple){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)stipple->id();     // This should be a HBITMAP
        }
      else if(pattern>=STIPPLE_0 && pattern<=STIPPLE_16){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)getApp()->stipples[pattern];
        }
      else{
        lb.lbStyle=BS_HATCHED;
        lb.lbColor=devfg;
        lb.lbHatch=FXStipplePattern2Hatch(pattern);
        }
      ::DeleteObject(::SelectObject((HDC)ctx,::CreateBrushIndirect(&lb)));
      break;
    case FILL_OPAQUESTIPPLED:
      if(stipple){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)stipple->id();     // This should be a HBITMAP
        }
      else if(pattern>=STIPPLE_0 && pattern<=STIPPLE_16){
        lb.lbStyle=BS_PATTERN;
        lb.lbColor=devfg;
        lb.lbHatch=(FXuval)getApp()->stipples[pattern];
        }
      else{
        lb.lbStyle=BS_HATCHED;
        lb.lbColor=devfg;
        lb.lbHatch=FXStipplePattern2Hatch(pattern);
        }
      ::DeleteObject(::SelectObject((HDC)ctx,::CreateBrushIndirect(&lb)));
      break;
    }
  if(fill==FILL_STIPPLED){
    ::SetBkMode((HDC)ctx,TRANSPARENT);         // Alas, only works for BS_HATCHED...
    }
  else{
    ::SetBkMode((HDC)ctx,OPAQUE);
    }
  if(fill!=FILL_SOLID){
    ::SetBrushOrgEx((HDC)ctx,tx,ty,NULL);
    }
  needsNewBrush=FALSE;
  }


// Set foreground color
void FXDCWindow::setForeground(FXColor clr){
  if(!surface){ fxerror("FXDCWindow::setForeground: DC not connected to drawable.\n"); }
  devfg=visual->getPixel(clr);
  needsNewPen=TRUE;
  needsNewBrush=TRUE;
  ::SetTextColor((HDC)ctx,devfg);
  fg=clr;
  }


// Set background color
void FXDCWindow::setBackground(FXColor clr){
  if(!surface){ fxerror("FXDCWindow::setBackground: DC not connected to drawable.\n"); }
  devbg=visual->getPixel(clr);
  ::SetBkColor((HDC)ctx,devbg);
  bg=clr;
  }


// Set dash pattern (for the LINE_ONOFF_DASH line style)
void FXDCWindow::setDashes(FXuint dashoffset,const FXchar *dashpattern,FXuint dashlength){
  register FXuint len,i;
  if(!surface){ fxerror("FXDCWindow::setDashes: DC not connected to drawable.\n"); }
  for(i=len=0; i<dashlength; i++){
    dashpat[i]=dashpattern[i];
    len+=(FXuint)dashpattern[i];
    }
  dashlen=dashlength;
  dashoff=dashoffset%len;
  needsNewPen=TRUE;
  }


// Set line width
void FXDCWindow::setLineWidth(FXuint linewidth){
  if(!surface){ fxerror("FXDCWindow::setLineWidth: DC not connected to drawable.\n"); }
  width=linewidth;
  needsNewPen=TRUE;
  }


// Set line cap style
void FXDCWindow::setLineCap(FXCapStyle capstyle){
  if(!surface){ fxerror("FXDCWindow::setLineCap: DC not connected to drawable.\n"); }
  cap=capstyle;
  needsNewPen=TRUE;
  }


// Set line join style
void FXDCWindow::setLineJoin(FXJoinStyle joinstyle){
  if(!surface){ fxerror("FXDCWindow::setLineJoin: DC not connected to drawable.\n"); }
  join=joinstyle;
  needsNewPen=TRUE;
  }


// Set line style
void FXDCWindow::setLineStyle(FXLineStyle linestyle){
  if(!surface){ fxerror("FXDCWindow::setLineStyle: DC not connected to drawable.\n"); }
  style=linestyle;
  needsNewPen=TRUE;
  }


// Set fill style
void FXDCWindow::setFillStyle(FXFillStyle fillstyle){
  if(!surface){ fxerror("FXDCWindow::setFillStyle: DC not connected to drawable.\n"); }
  fill=fillstyle;
  needsNewBrush=TRUE;
  needsNewPen=TRUE;
  }


// Set fill rule
void FXDCWindow::setFillRule(FXFillRule fillrule){
  if(!surface){ fxerror("FXDCWindow::setFillRule: DC not connected to drawable.\n"); }
  if(fillrule==RULE_EVEN_ODD)
    ::SetPolyFillMode((HDC)ctx,ALTERNATE);
  else
    ::SetPolyFillMode((HDC)ctx,WINDING);
  rule=fillrule;
  }


// Set blit function
void FXDCWindow::setFunction(FXFunction func){
  if(!surface){ fxerror("FXDCWindow::setFunction: DC not connected to drawable.\n"); }
  rop=func;

  // Also set ROP2 code for lines
  switch(rop){
    case BLT_CLR:                     // D := 0
      ::SetROP2((HDC)ctx,R2_BLACK);
      break;
    case BLT_SRC_AND_DST:             // D := S & D
      ::SetROP2((HDC)ctx,R2_MASKPEN);
      break;
    case BLT_SRC_AND_NOT_DST:         // D := S & ~D
      ::SetROP2((HDC)ctx,R2_MASKPENNOT);
      break;
    case BLT_SRC:                     // D := S
      ::SetROP2((HDC)ctx,R2_COPYPEN);
      break;
    case BLT_NOT_SRC_AND_DST:         // D := ~S & D
      ::SetROP2((HDC)ctx,R2_MASKNOTPEN);
      break;
    case BLT_DST:                     // D := D
      break;
    case BLT_SRC_XOR_DST:             // D := S ^ D
      ::SetROP2((HDC)ctx,R2_XORPEN);
      break;
    case BLT_SRC_OR_DST:              // D := S | D
      ::SetROP2((HDC)ctx,R2_MERGEPEN);
      break;
    case BLT_NOT_SRC_AND_NOT_DST:     // D := ~S & ~D  ==  D := ~(S | D)
      ::SetROP2((HDC)ctx,R2_NOTMERGEPEN);
      break;
    case BLT_NOT_SRC_XOR_DST:         // D := ~S ^ D
      ::SetROP2((HDC)ctx,R2_NOTXORPEN); // Is this the right one?
      break;
    case BLT_NOT_DST:                 // D := ~D
      ::SetROP2((HDC)ctx,R2_NOT);
      break;
    case BLT_SRC_OR_NOT_DST:          // D := S | ~D
      ::SetROP2((HDC)ctx,R2_MERGEPENNOT);
      break;
    case BLT_NOT_SRC:                 // D := ~S
      ::SetROP2((HDC)ctx,R2_NOTCOPYPEN);
      break;
    case BLT_NOT_SRC_OR_DST:          // D := ~S | D
      ::SetROP2((HDC)ctx,R2_MERGENOTPEN);
      break;
    case BLT_NOT_SRC_OR_NOT_DST:      // D := ~S | ~D  ==  ~(S & D)
      ::SetROP2((HDC)ctx,R2_NOTMASKPEN);
      break;
    case BLT_SET:                     // D := 1
      ::SetROP2((HDC)ctx,R2_WHITE);
      break;
    }
  }


// Set tile image
void FXDCWindow::setTile(FXImage* image,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::setTile: DC not connected to drawable.\n"); }
  tile=image;
  tx=dx;
  ty=dy;
  }


// Set stipple pattern
void FXDCWindow::setStipple(FXBitmap* bitmap,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::setStipple: DC not connected to drawable.\n"); }
  stipple=bitmap;
  pattern=STIPPLE_NONE;
  needsNewBrush=TRUE;
  needsNewPen=TRUE;
  tx=dx;
  ty=dy;
  }


void FXDCWindow::setStipple(FXStipplePattern pat,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::setStipple: DC not connected to drawable.\n"); }
  stipple=NULL;
  pattern=pat;
  needsNewBrush=TRUE;
  needsNewPen=TRUE;
  tx=dx;
  ty=dy;
  }


// Patch from "Dimitris Servis" <servis@deslab.ntua.gr>
// The new clip rectangle should be the intersect of the region
// boundary rectangle and the paint rectangle.
// Another patch from Ivan Markov <ivan.markov@wizcom.bg> to delete
// the region which must be disposed off explicitly.
void FXDCWindow::setClipRegion(const FXRegion& region){
  if(!surface){ fxerror("FXDCWindow::setClipRegion: DC not connected to drawable.\n"); }
  FXRectangle rectangle=region.bounds();
  clip.x=FXMAX(rectangle.x,rect.x);
  clip.y=FXMAX(rectangle.y,rect.y);
  clip.w=FXMIN(rectangle.x+rectangle.w,rect.x+rect.w)-clip.x;
  clip.h=FXMIN(rectangle.y+rectangle.h,rect.y+rect.h)-clip.y;
  if(clip.w<=0) clip.w=0;
  if(clip.h<=0) clip.h=0;
  HRGN hrgn=::CreateRectRgn(clip.x,clip.y,clip.x+clip.w,clip.y+clip.h);
  ::CombineRgn(hrgn,hrgn,(HRGN)region.region,RGN_AND);
  ::SelectClipRgn((HDC)ctx,hrgn);
  ::DeleteObject(hrgn);
  }


// Set clip rectangle
void FXDCWindow::setClipRectangle(FXint x,FXint y,FXint w,FXint h){
  if(!surface){ fxerror("FXDCWindow::setClipRectangle: DC not connected to drawable.\n"); }
  clip.x=FXMAX(x,rect.x);
  clip.y=FXMAX(y,rect.y);
  clip.w=FXMIN(x+w,rect.x+rect.w)-clip.x;
  clip.h=FXMIN(y+h,rect.y+rect.h)-clip.y;
  if(clip.w<=0) clip.w=0;
  if(clip.h<=0) clip.h=0;
  HRGN hrgn=::CreateRectRgn(clip.x,clip.y,clip.x+clip.w,clip.y+clip.h);
  ::SelectClipRgn((HDC)ctx,hrgn);
  ::DeleteObject(hrgn);
  }


// Set clip rectangle
void FXDCWindow::setClipRectangle(const FXRectangle& rectangle){
  if(!surface){ fxerror("FXDCWindow::setClipRectangle: DC not connected to drawable.\n"); }
  clip.x=FXMAX(rectangle.x,rect.x);
  clip.y=FXMAX(rectangle.y,rect.y);
  clip.w=FXMIN(rectangle.x+rectangle.w,rect.x+rect.w)-clip.x;
  clip.h=FXMIN(rectangle.y+rectangle.h,rect.y+rect.h)-clip.y;
  if(clip.w<=0) clip.w=0;
  if(clip.h<=0) clip.h=0;
  HRGN hrgn=::CreateRectRgn(clip.x,clip.y,clip.x+clip.w,clip.y+clip.h);
  ::SelectClipRgn((HDC)ctx,hrgn);
  ::DeleteObject(hrgn);
  }


// Clear clip rectangle
void FXDCWindow::clearClipRectangle(){
  if(!surface){ fxerror("FXDCWindow::clearClipRectangle: DC not connected to drawable.\n"); }
  clip=rect;
  HRGN hrgn=::CreateRectRgn(clip.x,clip.y,clip.x+clip.w,clip.y+clip.h);
  ::SelectClipRgn((HDC)ctx,hrgn);
  ::DeleteObject(hrgn);
  }



// Set clip mask
void FXDCWindow::setClipMask(FXBitmap* bitmap,FXint dx,FXint dy){
  if(!surface){ fxerror("FXDCWindow::setClipMask: DC not connected to drawable.\n"); }
  FXASSERT(FALSE);
  mask=bitmap;
  cx=dx;
  cy=dy;
  }


// Clear clip mask
void FXDCWindow::clearClipMask(){
  if(!surface){ fxerror("FXDCWindow::clearClipMask: DC not connected to drawable.\n"); }
  FXASSERT(FALSE);
  mask=NULL;
  cx=0;
  cy=0;
  }


// Window will clip against child windows
void FXDCWindow::clipChildren(FXbool yes){
  if(!surface){ fxerror("FXDCWindow::clipChildren: window has not yet been created.\n"); }
  DWORD    dwFlags=::GetWindowLong((HWND)surface->id(),GWL_STYLE);
  HPEN     hPen;
  HBRUSH   hBrush;
  HFONT    hFont;
  COLORREF textcolor;
  COLORREF backcolor;
  FXint    fillmode;
  if(yes){
    if(!(dwFlags&WS_CLIPCHILDREN)){
      if((HWND)surface->id()!=GetDesktopWindow()){
        hPen=(HPEN)SelectObject((HDC)ctx,::GetStockObject(NULL_PEN));
        hBrush=(HBRUSH)::SelectObject((HDC)ctx,::GetStockObject(NULL_BRUSH));
        hFont=(HFONT)::SelectObject((HDC)ctx,::GetStockObject(SYSTEM_FONT));
        textcolor=::GetTextColor((HDC)ctx);
        backcolor=::GetBkColor((HDC)ctx);
        fillmode=::GetPolyFillMode((HDC)ctx);

        ::ReleaseDC((HWND)surface->id(),(HDC)ctx);
        ::SetWindowLong((HWND)surface->id(),GWL_STYLE,dwFlags|WS_CLIPCHILDREN);
        ctx=::GetDC((HWND)surface->id());

        ::SelectObject((HDC)ctx,hFont);
        ::SelectObject((HDC)ctx,hPen);
        ::SelectObject((HDC)ctx,hBrush);

        if(visual->colormap){
          ::SelectPalette((HDC)ctx,(HPALETTE)visual->colormap,false);
          ::RealizePalette((HDC)ctx);
          }
        ::SetTextAlign((HDC)ctx,TA_BASELINE|TA_LEFT);
        ::SetTextColor((HDC)ctx,textcolor);
        ::SetBkColor((HDC)ctx,backcolor);
        ::SetPolyFillMode((HDC)ctx,fillmode);
        needsClipReset=false;
        }
      }
    }
  else{
    if(dwFlags&WS_CLIPCHILDREN){
      if((HWND)surface->id()!=GetDesktopWindow()){
        hPen=(HPEN)::SelectObject((HDC)ctx,::GetStockObject(NULL_PEN));
        hBrush=(HBRUSH)::SelectObject((HDC)ctx,::GetStockObject(NULL_BRUSH));
        hFont=(HFONT)::SelectObject((HDC)ctx,::GetStockObject(SYSTEM_FONT));
        textcolor=::GetTextColor((HDC)ctx);
        backcolor=::GetBkColor((HDC)ctx);
        fillmode=::GetPolyFillMode((HDC)ctx);

        ::ReleaseDC((HWND)surface->id(),(HDC)ctx);
        ::SetWindowLong((HWND)surface->id(),GWL_STYLE,dwFlags&~WS_CLIPCHILDREN);
        ctx=::GetDC((HWND)surface->id());

        ::SelectObject((HDC)ctx,hFont);
        ::SelectObject((HDC)ctx,hPen);
        ::SelectObject((HDC)ctx,hBrush);

        if(visual->colormap){
          ::SelectPalette((HDC)ctx,(HPALETTE)visual->colormap,false);
          ::RealizePalette((HDC)ctx);
          }
        ::SetTextAlign((HDC)ctx,TA_BASELINE|TA_LEFT);
        ::SetTextColor((HDC)ctx,textcolor);
        ::SetBkColor((HDC)ctx,backcolor);
        ::SetPolyFillMode((HDC)ctx,fillmode);
        needsClipReset=true;
        }
      }
    }
  }

#endif

}
