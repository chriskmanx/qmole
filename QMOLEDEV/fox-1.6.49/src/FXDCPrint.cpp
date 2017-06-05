/********************************************************************************
*                                                                               *
*           D e v i c e   C o n t e x t   F o r   P r i n t i n g               *
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
* $Id: FXDCPrint.cpp,v 1.54 2006/01/22 17:58:21 fox Exp $                       *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXId.h"
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
#include "FXDCPrint.h"


/*
  Notes:
  - Contributed by celer@ipro.lug.usf.edu.
  - Coordinate system starts in upper left corner, same as screen [which is
    different from the way PostScript normally does things].
  - Implement the many missing functions.
  - Make it EPS compatible.
  - Allow user to override PostScript Functions.
  - Usage:
    psdc.beginPrint(paper desc);
    psdc.beginPage(pageno)
    ....
    drawing commands
    ....
    psdc.endPage();
    psdc.endPrint();
  - Perhaps feed into FXStream instead of FILE* this might be
    cool to drag and drop [E]PS into apps...
  - Do we still need the enum's in FXMediaSize if mediasize
    indexes into the registry database's paper size list?
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Construct
FXDCPrint::FXDCPrint(FXApp* a):FXDC(a){
  font=getApp()->getNormalFont();
  psout=NULL;   // FIXME use ctx for this
  mediawidth=0.0;
  mediaheight=0.0;
  mediabb.xmin=0.0;
  mediabb.xmax=0.0;
  mediabb.ymin=0.0;
  mediabb.ymax=0.0;
  docbb.xmin=0.0;
  docbb.xmax=0.0;
  docbb.ymin=0.0;
  docbb.ymax=0.0;
  pagebb.xmin=0.0;
  pagebb.xmax=0.0;
  pagebb.ymin=0.0;
  pagebb.ymax=0.0;
  pagecount=0;
  nchars=0;
  }


// Destruct
FXDCPrint::~FXDCPrint(){
  }


// Output hex number
void FXDCPrint::outhex(FXuint hex){
  if(!psout){ fxerror("FXDCPrint: no output device has been selected.\n"); }
  fprintf((FILE*)psout,"%02x",hex);
  if(++nchars>35){fputc('\n',(FILE*)psout);nchars=0;}
  }


// Output formatted stuff
void FXDCPrint::outf(const char* format,...){
  va_list arguments;
  if(!psout){ fxerror("FXDCPrint: no output device has been selected.\n"); }
  va_start(arguments,format);
  vfprintf((FILE*)psout,format,arguments);
  va_end(arguments);
  }


// Extends bounding box with point x,y
void FXDCPrint::bbox(FXfloat x,FXfloat y){
  if(x<pagebb.xmin) pagebb.xmin=x;
  if(pagebb.xmax<x) pagebb.xmax=x;
  if(y<pagebb.ymin) pagebb.ymin=y;
  if(pagebb.ymax<y) pagebb.ymax=y;
  }


// Send the range of coordinates that will be sent
FXbool FXDCPrint::setContentRange(FXint pxminArg, FXint pyminArg, FXint pxmaxArg, FXint pymaxArg){
  if(flags&PRINT_LANDSCAPE){
    pxmin=pyminArg;
    pymin=pxminArg;
    pxmax=pymaxArg;
    pymax=pxmaxArg;
    }
  else{
    pxmin=pxminArg;
    pymin=pyminArg;
    pxmax=pxmaxArg;
    pymax=pymaxArg;
    }
  return TRUE;    // Should we check for appropriate ranges?
  }


// Transform point
void FXDCPrint::tfm(FXfloat& xo,FXfloat& yo,FXfloat xi,FXfloat yi){
/*
  if(flags&PRINT_LANDSCAPE){
    xo=yi;
    yo=(FXfloat)(mediaheight-xi);
    }
  else{
    xo=xi;
    yo=(FXfloat)(mediaheight-yi);
    }
*/
  FXfloat pxrange=static_cast<FXfloat>(pxmax-pxmin);
  FXfloat pyrange=static_cast<FXfloat>(pymax-pymin);
  FXfloat mxmin,mxmax,mymin,mymax,mxrange,myrange;

  if(flags&PRINT_LANDSCAPE){
    mxmin=static_cast<FXfloat>(mediabb.ymin);
    mxmax=static_cast<FXfloat>(mediabb.ymax);
    //mymin=static_cast<FXfloat>(mediawidth-mediabb.xmax);
    //mymax=static_cast<FXfloat>(mediawidth-mediabb.xmin);
    mymin=static_cast<FXfloat>(mediabb.xmin);
    mymax=static_cast<FXfloat>(mediabb.xmax);
    mxrange=mxmax-mxmin;
    myrange=mymax-mymin;
    //xo=xi;
    //yo=mediawidth-yi;
    }
  else{
    mxmin=static_cast<FXfloat>(mediabb.xmin);
    mxmax=static_cast<FXfloat>(mediabb.xmax);
    mymin=static_cast<FXfloat>(mediabb.ymin);
    mymax=static_cast<FXfloat>(mediabb.ymax);
    mxrange=mxmax-mxmin;
    myrange=mymax-mymin;
    }

  if (pyrange/pxrange<=myrange/mxrange) { // short/wide
    xo=mxmin+((xi-pxmin)/pxrange)*mxrange;
    yo=mymin+.5f*(myrange-pyrange*(mxrange/pxrange))+(pyrange-yi)*(mxrange/pxrange);
    }
  else {// tall/thin
    xo=mxmin+.5f*(mxrange-pxrange*(myrange/pyrange))+xi*(myrange/pyrange);
    yo=mymin+((pyrange-yi)/pyrange)*myrange;
    }
  }


// Generate print job prolog
FXbool FXDCPrint::beginPrint(FXPrinter& job){
  int numpages;

  Yr=792;  //480 // This is essentially the height of the page(used so that the upper left hand corner is the origin)
  Xr=0;

  // Print to file
  if(job.flags&PRINT_DEST_FILE){
    psout=fopen(job.name.text(),"w");
    if(!psout) return FALSE;
    }

  // Print to printer
  else{
    char buffer[1000];
    const FXchar* printercmd=getApp()->reg().readStringEntry("PRINTER","command","lpr -P%s -#%d");
    sprintf(buffer,printercmd,job.name.text(),job.numcopies);
#ifdef WIN32
#ifndef _WINDOWS
#ifdef __CYGWIN__
    psout=popen(buffer,"w");
#else
    psout=_popen(buffer,"w"); // _popen() available for console apps only!
#endif
#else
    psout=0;
#endif
#else
    psout=popen(buffer,"w");
#endif
    if(!psout) return FALSE;
    }

  // Copy flags
  flags=job.flags;

  // This determines transformations
  mediawidth=(FXfloat)job.mediawidth;
  mediaheight=(FXfloat)job.mediaheight;

  // Set media bb; this determines transformation
  mediabb.xmin=(FXfloat)job.leftmargin;
  mediabb.xmax=(FXfloat)(job.mediawidth-job.rightmargin);
  mediabb.ymin=(FXfloat)job.bottommargin;
  mediabb.ymax=(FXfloat)(job.mediaheight-job.topmargin);

  // Initialize page and document bb from media bb
  pagebb=mediabb;
  docbb=mediabb;

  // Begin header
  outf("%%!PS-Adobe-3.0\n");
  outf("%%%%Title: Print Job\n");
  outf("%%%%Creator: FOX GUI Toolkit Application\n");

  // Bounding box
  if(flags&PRINT_NOBOUNDS){
    docbb.xmin= 1000000.0;
    docbb.xmax=-1000000.0;
    docbb.ymin= 1000000.0;
    docbb.ymax=-1000000.0;
    outf("%%%%BoundingBox: (atend)\n");
    }
  else{
    docbb.xmin=(FXfloat)job.leftmargin;
    docbb.xmax=(FXfloat)(job.mediawidth-job.rightmargin);
    docbb.ymin=(FXfloat)job.bottommargin;
    docbb.ymax=(FXfloat)(job.mediaheight-job.topmargin);
    outf("%%%%BoundingBox: %d %d %d %d\n",(int)docbb.xmin,(int)docbb.ymin,(int)docbb.xmax,(int)docbb.ymax);
    }
  setContentRange((int)docbb.xmin, (int)docbb.ymin, (int)docbb.xmax, (int)docbb.ymax);

  // Calculate number of pages
  numpages=0;
  if(flags&PRINT_PAGES_ODD){
    numpages=1+(job.topage-job.frompage)/2;
    }
  else if(flags&PRINT_PAGES_EVEN){
    numpages=1+(job.topage-job.frompage)/2;
    }
  else if(flags&PRINT_PAGES_RANGE){
    numpages=1+job.topage-job.frompage;
    }

  // How many pages are coming
  if(numpages==0){
    outf("%%%%Pages: (atend)\n");
    }
  else{
    outf("%%%%Pages: %d\n",numpages);
    }

  outf("%%%%DocumentFonts:\n");
  outf("%%%%EndComments\n");

  // Procedure definitions
  outf("%%%%BeginProlog\n\n\n");

  // Various definitions
  outf("%% h w x y drawRect\n");
  outf("/drawRect {\n\tnewpath moveto dup 0 rlineto exch dup 0 exch\n\trlineto exch neg 0 rlineto neg 0 exch rlineto\n\tclosepath stroke\n} def\n");
  outf("%% h w x y fillRect\n");
  outf("/fillRect {\n\tnewpath moveto dup 0 rlineto exch dup 0 exch\n\trlineto exch neg 0 rlineto neg 0 exch rlineto\n\tclosepath fill stroke\n} def\n");
  outf("%% x y a b drawLine\n");
  outf("/drawLine {\n\tnewpath moveto lineto stroke\n} def\n");
  outf("%% x y ..... npoints drawLines\n");
  outf("/drawLines {\n\t3 1 roll newpath moveto {lineto} repeat stroke\n} def\n");
  outf("%% x y a b ..... nsegments drawSegmt\n");
  outf("/drawSegmt {\n\tnewpath {\n\t\tmoveto lineto\n\t} repeat stroke\n} def\n");
  outf("%% x y drawPoint\n");
  outf("/drawPoint {\n\ttranslate 1 1 scale 8 8 1 [ 8 0 0 8 0 0 ] {<0000>} image\n} def\n");
  outf("%% centerx centery  startAngle endAngle radiusX radiusY drawArc\n");
  outf("/drawArc {\n\tgsave dup 3 1 roll div dup 1 scale 6 -1 roll\n\texch div 5 1 roll  3 -2 roll arc stroke grestore\n} def\n");
  outf("%% (string) x y height drawText\n");
  outf("/drawText {\n\tgsave findfont exch scalefont setfont moveto\n\tshow grestore\n} def\n");

  // Image operator
  outf("/bwproc\n");
  outf(" {  rgbproc\n");
  outf("    dup length 3 idiv string 0 3 0\n");
  outf("    5 -1 roll\n");
  outf("    { add 2 1 roll 1 sub dup 0 eq\n");
  outf("      { pop 3 idiv 3 -1 roll dup 4 -1 roll dup\n");
  outf("        3 1 roll 5 -1 roll put 1 add 3 0 }\n");
  outf("      { 2 1 roll } ifelse\n");
  outf("    } forall\n");
  outf("    pop pop pop\n");
  outf("} def\n");
  outf("systemdict /colorimage known not\n");
  outf(" { /colorimage\n");
  outf("     { pop pop /rgbproc exch def\n");
  outf("     { bwproc } image\n");
  outf(" } def\n");
  outf("} if\n");

  // For 3D
  outf("%% Color -  r g b C\n");
  outf("/C { setrgbcolor } bind def\n");

  outf("%% Point -  x y r g b P\n");
  outf("/P { C newpath 0.5 0.0 360.0 arc closepath fill } bind def\n");

  outf("%% Flat Shaded Line -  x2 y2 x1 y1 r g b L\n");
  outf("/L { C newpath moveto lineto stroke } bind def\n");

  outf("%% Smooth-shaded line -  x2 y2 r2 g2 b2 x1 y1 r1 g1 b1 SL\n");
  outf("/SL {\n");
  outf(" /b1 exch def\n");
  outf(" /g1 exch def\n");
  outf(" /r1 exch def\n");
  outf(" /y1 exch def\n");
  outf(" /x1 exch def\n");
  outf(" /b2 exch def\n");
  outf(" /g2 exch def\n");
  outf(" /r2 exch def\n");
  outf(" /y2 exch def\n");
  outf(" /x2 exch def\n");
  outf("\n");
  outf(" b2 b1 sub abs 0.01 gt\n");
  outf(" g2 g1 sub abs 0.005 gt\n");
  outf(" r2 r1 sub abs 0.008 gt\n");
  outf("     or or {\n");
  outf("         /bm b1 b2 add 0.5 mul def\n");
  outf("         /gm g1 g2 add 0.5 mul def\n");
  outf("         /rm r1 r2 add 0.5 mul def\n");
  outf("         /ym y1 y2 add 0.5 mul def\n");
  outf("         /xm x1 x2 add 0.5 mul def\n");
  outf("\n");
  outf("         x1 y1 r1 g1 b1 xm ym rm gm bm SL\n");
  outf("         xm ym rm gm bm x2 y2 r2 g2 b2 SL\n");
  outf(" } {\n");
  outf("         x1 y1 x2 y2 r1 g1 b1 L\n");
  outf(" } ifelse\n");
  outf("} bind def\n");

  outf("%% Flat-shaded triangle - x3 y3 x2 y2 x1 y1 r g b T\n");
  outf("/T { C newpath moveto lineto lineto closepath fill } bind def\n");

  outf("%% Smooth-shaded triangle - x3 y3 r3 g3 b3 x2 y2 r2 g2 b2 x1 y1 r1 g1 b1 ST\n");
  outf("/ST {\n");
  outf(" /b1 exch def\n");
  outf(" /g1 exch def\n");
  outf(" /r1 exch def\n");
  outf(" /y1 exch def\n");
  outf(" /x1 exch def\n");
  outf(" /b2 exch def\n");
  outf(" /g2 exch def\n");
  outf(" /r2 exch def\n");
  outf(" /y2 exch def\n");
  outf(" /x2 exch def\n");
  outf(" /b3 exch def\n");
  outf(" /g3 exch def\n");
  outf(" /r3 exch def\n");
  outf(" /y3 exch def\n");
  outf(" /x3 exch def\n");
  outf("\n");
  outf(" b2 b1 sub abs 0.05 gt\n");
  outf(" g2 g1 sub abs 0.017 gt\n");
  outf(" r2 r1 sub abs 0.032 gt\n");
  outf(" b3 b1 sub abs 0.05 gt\n");
  outf(" g3 g1 sub abs 0.017 gt\n");
  outf(" r3 r1 sub abs 0.032 gt\n");
  outf(" b2 b3 sub abs 0.05 gt\n");
  outf(" g2 g3 sub abs 0.017 gt\n");
  outf(" r2 r3 sub abs 0.032 gt\n");
  outf(" or or or or or or or or {\n");
  outf("         /b12 b1 b2 add 0.5 mul def\n");
  outf("         /g12 g1 g2 add 0.5 mul def\n");
  outf("         /r12 r1 r2 add 0.5 mul def\n");
  outf("         /y12 y1 y2 add 0.5 mul def\n");
  outf("         /x12 x1 x2 add 0.5 mul def\n");
  outf("\n");
  outf("         /b13 b1 b3 add 0.5 mul def\n");
  outf("         /g13 g1 g3 add 0.5 mul def\n");
  outf("         /r13 r1 r3 add 0.5 mul def\n");
  outf("         /y13 y1 y3 add 0.5 mul def\n");
  outf("         /x13 x1 x3 add 0.5 mul def\n");
  outf("\n");
  outf("         /b32 b3 b2 add 0.5 mul def\n");
  outf("         /g32 g3 g2 add 0.5 mul def\n");
  outf("         /r32 r3 r2 add 0.5 mul def\n");
  outf("         /y32 y3 y2 add 0.5 mul def\n");
  outf("         /x32 x3 x2 add 0.5 mul def\n");
  outf("\n");
  outf("         x1 y1 r1 g1 b1 x12 y12 r12 g12 b12 x13 y13 r13 g13 b13\n");
  outf("         x2 y2 r2 g2 b2 x12 y12 r12 g12 b12 x32 y32 r32 g32 b32\n");
  outf("         x3 y3 r3 g3 b3 x32 y32 r32 g32 b32 x13 y13 r13 g13 b13\n");
  outf("         x32 y32 r32 g32 b32 x12 y12 r12 g12 b12 x13 y13 r13 g13 b13\n");
  outf("         ST ST ST ST\n");
  outf(" } {\n");
  outf("         x1 y1 x2 y2 x3 y3 r1 g1 b1 T\n");
  outf(" } ifelse\n");
  outf("} bind def\n");

  // End of prologue
  outf("%%%%EndProlog\n");


  // Document setup
  outf("%%%%BeginSetup\n");
  outf("/#copies %d def\n",job.numcopies);
  outf("%%%%EndSetup\n");

  // Keep track of #pages
  pagecount=0;

  return TRUE;
  }


// Generate print job epilog
FXbool FXDCPrint::endPrint(){
  outf("%%%%Trailer\n");

  // We now know the bounding box
  if(flags&PRINT_NOBOUNDS){
    if(docbb.xmin<docbb.xmax && docbb.ymin<docbb.ymax){
      outf("%%%%BoundingBox: %d %d %d %d\n",(int)docbb.xmin,(int)docbb.ymin,(int)docbb.xmax,(int)docbb.ymax);
      }
    else{
      outf("%%%%BoundingBox: 0 0 100 100\n");   // Gotta come up with something...
      }
    }

  // We now know the page count
  if(!(flags&(PRINT_PAGES_ODD|PRINT_PAGES_EVEN|PRINT_PAGES_RANGE))){
    outf("%%%%Pages: %d\n",pagecount);
    }

  // Done!
  outf("%%%%EOF\n");
  fclose((FILE*)psout);
  return TRUE;
  }


// Generate begin of page
FXbool FXDCPrint::beginPage(FXuint page){

  // Output page number
  outf("%%%%Page: %d\n",page);

  // Reset page bounding box
  if(flags&PRINT_NOBOUNDS){
    pagebb.xmin= 1000000;
    pagebb.xmax=-1000000;
    pagebb.ymin= 1000000;
    pagebb.ymax=-1000000;
    outf("%%%%PageBoundingBox: (atend)\n");
    }

  // Use the doc bounding box
  else{
    pagebb.xmin=docbb.xmin;
    pagebb.xmax=docbb.xmax;
    pagebb.ymin=docbb.ymin;
    pagebb.ymax=docbb.ymax;
    outf("%%%%PageBoundingBox: %d %d %d %d\n",(int)pagebb.xmin,(int)pagebb.ymin,(int)pagebb.xmax,(int)pagebb.ymax);
    }

  // Page setup
  outf("%%%%BeginPageSetup\n");
  outf("%%%%EndPageSetup\n");
  outf("gsave\n");

  // Maybe in landscape?
  if(flags&PRINT_LANDSCAPE){
    outf("%g %g translate\n",mediawidth,0.0);
    outf("90 rotate\n");
    }

  return TRUE;
  }


// Generate end of page
FXbool FXDCPrint::endPage(){
  /*
  outf("0 0 0 setcolor\n");
  outf("newpath\n");
  outf("%g %g moveto\n",mediabb.xmin,mediabb.ymin);
  outf("%g %g lineto\n",mediabb.xmin,mediabb.ymax);
  outf("%g %g lineto\n",mediabb.xmax,mediabb.ymax);
  outf("%g %g lineto\n",mediabb.xmax,mediabb.ymin);
  outf("%g %g lineto\n",mediabb.xmin,mediabb.ymin);
  outf("stroke\n");
  outf("newpath\n");
  outf("%g %g moveto\n",mediabb.xmin,mediabb.ymin);
  outf("%g %g lineto\n",mediabb.xmax,mediabb.ymax);
  outf("stroke\n");
  outf("newpath\n");
  outf("%g %g moveto\n",mediabb.xmin,mediabb.ymax);
  outf("%g %g lineto\n",mediabb.xmax,mediabb.ymin);
  outf("stroke\n");
  */
  outf("%%%%PageTrailer\n");

  // We now know the bounding box
  if(flags&PRINT_NOBOUNDS){
    if(pagebb.xmin<pagebb.xmax && pagebb.ymin<pagebb.ymax){
      outf("%%%%BoundingBox: %d %d %d %d\n",(int)pagebb.xmin,(int)pagebb.ymin,(int)pagebb.xmax,(int)pagebb.ymax);
      }
    else{
      outf("%%%%BoundingBox: 0 0 100 100\n");   // Gotta come up with something...
      }
    }
  outf("showpage\n");
  outf("grestore\n");
  pagecount++;
  return TRUE;
  }


// Draw a point in the current pen color
void FXDCPrint::drawPoint(FXint x,FXint y){
  FXfloat xx,yy;
  tfm(xx,yy,(FXfloat)x,(FXfloat)y);
  bbox(xx,yy);
  outf("%g %g 0.5 0 360 arc fill\n",xx,yy);
  }


// Draw points in the current pen color.
// Each point's position is relative to the drawable's origin (as usual).
void FXDCPrint::drawPoints(const FXPoint* points,FXuint npoints){
  register FXuint i;
  FXfloat xx,yy;
  for(i=0; i<npoints; i++){
    tfm(xx,yy,points[i].x,points[i].y);
    bbox(xx,yy);
    outf("%g %g 0.5 0 360 arc fill\n",xx,yy);
    }
  }


// Draw points in the current pen color. The first point's position is
// relative to the drawable's origin, but each subsequent point's position
// is relative to the previous point's position; each FXPoint defines
// the relative coordinates. Think LOGO.
void FXDCPrint::drawPointsRel(const FXPoint*,FXuint){
  }


// Draw a line
void FXDCPrint::drawLine(FXint x1,FXint y1,FXint x2,FXint y2){
  FXfloat xx1,yy1,xx2,yy2;
  tfm(xx1,yy1,(FXfloat)x1,(FXfloat)y1);
  tfm(xx2,yy2,(FXfloat)x2,(FXfloat)y2);
  bbox(xx1,yy1);
  bbox(xx2,yy2);
  outf("newpath %g %g moveto %g %g lineto stroke\n",xx1,yy1,xx2,yy2);
  }


// Draw multiple lines. All points are drawn connected.
// Each point is specified relative to Drawable's origin.
void FXDCPrint::drawLines(const FXPoint* points,FXuint npoints){
  register FXuint i;
  FXfloat xx,yy;
  if(npoints<2) return;
  tfm(xx,yy,points[0].x,points[0].y);
  bbox(xx,yy);
  outf("newpath %g %g moveto",xx,yy);
  for(i=1; i<npoints; i++){
    tfm(xx,yy,points[i].x,points[i].y);
    bbox(xx,yy);
    outf(" %g %g lineto",xx,yy);
    }
  outf(" stroke\n");
  }


// Draw multiple lines. All points are drawn connected.
// First point's coordinate is relative to drawable's origin, but
// subsequent points' coordinates are relative to previous point.
void FXDCPrint::drawLinesRel(const FXPoint* points,FXuint npoints){
  register FXuint i,x,y;
  FXfloat xx,yy;
  if(npoints<2) return;
  x=points[0].x;
  y=points[0].y;
  tfm(xx,yy,(FXfloat)x,(FXfloat)y);
  bbox(xx,yy);
  outf("newpath %g %g moveto",xx,yy);
  for(i=1; i<npoints; i++){
    x+=points[i].x;
    y+=points[i].y;
    tfm(xx,yy,(FXfloat)x,(FXfloat)y);
    bbox(xx,yy);
    outf(" %g %g lineto",xx,yy);
    }
  outf(" stroke\n");
  }


// Draw unconnected line segments
void FXDCPrint::drawLineSegments(const FXSegment* segments,FXuint nsegments){
  register FXuint i;
  for(i=0; i<=nsegments; i++)
    outf(" %d %d %d %d",
		segments[i].x1,Yr-segments[i].y1,
		segments[i].x2,Yr-segments[i].y2);
  outf(" %d drawSegmt\n",nsegments);
  }


// Draw unfilled rectangle
void FXDCPrint::drawRectangle(FXint x,FXint y,FXint w,FXint h){
  FXfloat xl,xr,yt,yb;
  tfm(xl,yt,(FXfloat)x,(FXfloat)y);
  tfm(xr,yb,(FXfloat)(x+w-1),(FXfloat)(y+h-1));
  bbox(xl,yt);
  bbox(xr,yb);
  outf("newpath %g %g moveto %g %g lineto %g %g lineto %g %g lineto %g %g lineto stroke\n",xl,yt,xr,yt,xr,yb,xl,yb,xl,yt);
  }


// Draw unfilled rectangles
void FXDCPrint::drawRectangles(const FXRectangle* rectangles,FXuint nrectangles){
  register FXuint i;
  for(i=0; i<nrectangles; i++){
    drawRectangle(rectangles[i].x,rectangles[i].y,rectangles[i].w,rectangles[i].h);
    }
  }


// Unfilled rounded rectangle
void FXDCPrint::drawRoundRectangle(FXint,FXint,FXint,FXint,FXint,FXint){
  }


// Draw arc (patch from: sancelot@crosswinds.net)
void FXDCPrint::drawArc(FXint x,FXint y,FXint w,FXint h,FXint ang1,FXint ang2){
  FXfloat startAngle,endAngle,xx,yy,yr;
  startAngle=((float)ang1)/64.0f;
  endAngle=startAngle+(((float) ang2)/64.0f);
  tfm(xx,yy,(FXfloat)x,(FXfloat)y);
  tfm(xx,yr,(FXfloat)x,(FXfloat)Yr);
  outf("%1.3f %1.3f %1.3f %1.3f %1.3f %1.3f drawArc\n",xx+(w/2.0),yy-(h/2.0),startAngle,endAngle,w/2.0,h/2.0);
  }



// Draw arcs
void FXDCPrint::drawArcs(const FXArc*,FXuint){
  }


// Draw ellipse
void FXDCPrint::drawEllipse(FXint,FXint,FXint,FXint){
  }


// Filled rectangle
void FXDCPrint::fillRectangle(FXint x,FXint y,FXint w,FXint h){
  FXfloat xl,xr,yt,yb;
  tfm(xl,yt,(FXfloat)x,(FXfloat)y);
  tfm(xr,yb,(FXfloat)(x+w-1),(FXfloat)(y+h-1));
  bbox(xl,yt);
  bbox(xr,yb);
  outf("newpath %g %g moveto %g %g lineto %g %g lineto %g %g lineto %g %g lineto fill\n",xl,yt,xr,yt,xr,yb,xl,yb,xl,yt);
  }


// Filled rectangles
void FXDCPrint::fillRectangles(const FXRectangle* rectangles,FXuint nrectangles){
  register FXuint i;
  for(i=0; i<nrectangles; i++){
    fillRectangle(rectangles[i].x,rectangles[i].y,rectangles[i].w,rectangles[i].h);
    }
  }


// Fill using currently selected ROP mode
void FXDCPrint::fillRoundRectangle(FXint,FXint,FXint,FXint,FXint,FXint){
  }


// Fill chord
void FXDCPrint::fillChord(FXint,FXint,FXint,FXint,FXint,FXint){
  }


// Fill chords
void FXDCPrint::fillChords(const FXArc*,FXuint){
  }



// Fill arc
void FXDCPrint::fillArc(FXint,FXint,FXint,FXint,FXint,FXint){
  }


// Fill arcs
void FXDCPrint::fillArcs(const FXArc*,FXuint){
  }


// Fill ellipse
void FXDCPrint::fillEllipse(FXint,FXint,FXint,FXint){
  }


// Filled simple polygon
void FXDCPrint::fillPolygon(const FXPoint* points,FXuint npoints){
  register FXuint i;
  FXfloat xx,yy;
  if(npoints<2) return;
  tfm(xx,yy,points[0].x,points[0].y);
  bbox(xx,yy);
  outf("newpath %g %g moveto",xx,yy);
  for(i=1; i<npoints; i++){
    tfm(xx,yy,points[i].x,points[i].y);
    bbox(xx,yy);
    outf(" %g %g lineto",xx,yy);
    }
  outf(" fill\n");
  }


// Fill concave polygon
void FXDCPrint::fillConcavePolygon(const FXPoint*,FXuint){
  }


// Fill complex (self-intersecting) polygon
void FXDCPrint::fillComplexPolygon(const FXPoint*,FXuint){
  }


// Filled simple polygon with relative points
void FXDCPrint::fillPolygonRel(const FXPoint*,FXuint){
  }


// Fill concave polygon
void FXDCPrint::fillConcavePolygonRel(const FXPoint*,FXuint){
  }


// Fill complex (self-intersecting) polygon
void FXDCPrint::fillComplexPolygonRel(const FXPoint*,FXuint){
  }


// Draw string with base line starting at x, y
void FXDCPrint::drawText(FXint x,FXint y,const FXchar* string,FXuint len){
/*
  FXfloat xx,yy;
  tfm(xx,yy,(FXfloat)x,(FXfloat)y);
  bbox(xx,yy);
  FXFontDesc fontdesc;
  font->getFontDesc(fontdesc);
  outf("gsave /%s findfont\n",font->getName().text());
  outf("%d scalefont\n",font->getSize()/10);
  outf("setfont\n");
  outf("newpath\n%g %g moveto\n(",xx,yy);
  for(FXuint i=0; i<len; i++){
    if(string[i]=='(') outf("\\050");
    else if(string[i]==')') outf("\\051");
    else outf("%c",string[i]);
    }
  outf(") show\n");
  outf("grestore\n");
*/
  FXfloat xx,yy;
  tfm(xx,yy,(FXfloat)x,(FXfloat)y);

  // old font size was hardcoded...
  //outf("(%s) %g %g %d /Courier drawText\n",string,xx,yy,15);

  //FXfloat pxrange=static_cast<FXfloat>(pxmax-pxmin);
  //FXfloat pyrange=static_cast<FXfloat>(pymax-pymin);
  //FXfloat mxmin,mxmax,mymin,mymax,mxrange,myrange;

/*  if(flags&PRINT_LANDSCAPE){
    mxmin=static_cast<FXfloat>(mediabb.ymin);
    mxmax=static_cast<FXfloat>(mediabb.ymax);
    //mymin=static_cast<FXfloat>(mediawidth-mediabb.xmax);
    //mymax=static_cast<FXfloat>(mediawidth-mediabb.xmin);
    mymin=static_cast<FXfloat>(mediabb.xmin);
    mymax=static_cast<FXfloat>(mediabb.xmax);
    mxrange=mxmax-mxmin;
    myrange=mymax-mymin;
    }
  else{
    mxmin=static_cast<FXfloat>(mediabb.xmin);
    mxmax=static_cast<FXfloat>(mediabb.xmax);
    mymin=static_cast<FXfloat>(mediabb.ymin);
    mymax=static_cast<FXfloat>(mediabb.ymax);
    mxrange=mxmax-mxmin;
    myrange=mymax-mymin;
    }
*/
  FXfloat fsize=0.1f*font->getSize();
  // Hack...
  // Account for dpi and scale up or down with graph...
  // Perhaps override screen resolution via registry
  //  FXint screenres=getApp()->reg().readUnsignedEntry("SETTINGS","screenres",100);

  // Validate
/*  if(screenres<50) screenres=50;
  if(screenres>200) screenres=200;

  if(pyrange/pxrange<=myrange/mxrange){ // short/wide
    fsize *= (mxrange/pxrange)*(screenres/72.f);
    }
  else{// tall/thin
    fsize *= (myrange/pyrange)*(screenres/72.f);
    }
*/

  FXString fname=font->getName();
  if(fname=="times"){
    fname="Times";
    }
  else if(fname=="helvetica"){
    fname="Helvetica";
    }
  else if(fname=="courier"){
    fname="Courier";
    }
  else{
    fname="Courier";
    }
  if(font->getWeight()==FXFont::Bold){
    if(font->getSlant()==FXFont::Italic){
      fname+="-BoldItalic";
      }
    else if(font->getSlant()==FXFont::Oblique){
      fname+="-BoldOblique";
      }
    else {
      fname+="-Bold";
      }
    }
  else{
    if(font->getSlant()==FXFont::Italic){
      fname+="-Italic";
      }
    else if(font->getSlant()==FXFont::Oblique){
      fname+="-Oblique";
      }
    }
  if(fname=="Times"){
    fname+="-Roman";
    }

  outf("(%s) %g %g %d /%s drawText\n",string,xx,yy,(int)fsize,fname.text());
  }


// Draw string with base line starting at x, y
void FXDCPrint::drawText(FXint x,FXint y,const FXString& string){
  drawText(x,y,string.text(),string.length());
  }


// Draw string with base line starting at x, y over filled background
void FXDCPrint::drawImageText(FXint,FXint,const FXchar*,FXuint){
  }


// Draw string with base line starting at x, y over filled background
void FXDCPrint::drawImageText(FXint x,FXint y,const FXString& string){
  drawImageText(x,y,string.text(),string.length());
  }


// Draw area from source
void FXDCPrint::drawArea(const FXDrawable*,FXint,FXint,FXint,FXint,FXint,FXint){
  }


// Draw area stretched area from source
void FXDCPrint::drawArea(const FXDrawable*,FXint,FXint,FXint,FXint,FXint,FXint,FXint,FXint){
  }


// Draw image
// Contibuted by dwalz@cs.uni-magdeburg.de
void FXDCPrint::drawImage(const FXImage *img,FXint dx,FXint dy){
  FXuint opts=img->getOptions();
  if(opts&IMAGE_OWNED){
    FXint    ww  = img->getWidth();
    FXint    hh = img->getHeight();
    FXuchar *buffer = (FXuchar*)img->getData();

    outf("/picstr %d string def\n",ww*3);
    outf("%d %d translate\n",dx,hh-dy);
    outf("%d %d scale\n",ww,-hh);
    outf("%d %d %d\n",ww,hh,8);
    outf("[%d 0 0 -%d 0 %d]\n",ww,hh,hh);
    outf("{currentfile picstr readhexstring pop}\n");
    outf("false %d\n",3);
    outf("colorimage\n");

    int end=ww*hh;
    for(int i=0; i<end ; i+=4){
      outhex(buffer[i]);
      outhex(buffer[i+1]);
      outhex(buffer[i+2]);
      }
    outf("\n");
    }
  }


// Draw bitmap
void FXDCPrint::drawBitmap(const FXBitmap*,FXint,FXint){
  }


// Draw icon
void FXDCPrint::drawIcon(const FXIcon*,FXint,FXint){
  }


// Draw icon shaded
void FXDCPrint::drawIconShaded(const FXIcon*,FXint,FXint){
  }


// Draw icon sunken
void FXDCPrint::drawIconSunken(const FXIcon*,FXint,FXint){
  }


// Draw hashed box
void FXDCPrint::drawHashBox(FXint,FXint,FXint,FXint,FXint){
  }


// Set foreground drawing color (brush)
void FXDCPrint::setForeground(FXColor clr){
  outf("%g %g %g setrgbcolor\n",FXREDVAL(clr)/255.0,FXGREENVAL(clr)/255.0,FXBLUEVAL(clr)/255.0);
  fg=clr;
  }


// Set background drawing color (brush)
void FXDCPrint::setBackground(FXColor clr){
  bg=clr;
  }


// Set dash pattern
void FXDCPrint::setDashes(FXuint,const FXchar *,FXuint){
  }


// Set line width
void FXDCPrint::setLineWidth(FXuint linewidth){
  outf("%d setlinewidth\n",linewidth);
  width=linewidth;
  }


// Set line cap style
void FXDCPrint::setLineCap(FXCapStyle capstyle){
  FXint ncap=0;

  if(CAP_BUTT==capstyle) ncap=0;
  if(CAP_ROUND==capstyle) ncap=1;
  if(CAP_PROJECTING==capstyle) ncap=3;

  outf("%d setlinecap\n",ncap);
  cap=capstyle;
  }


// Set line join style
void FXDCPrint::setLineJoin(FXJoinStyle joinstyle){
  join=joinstyle;
  }


// Set line style
void FXDCPrint::setLineStyle(FXLineStyle linestyle){
  style=linestyle;
  }


// Set fill style
void FXDCPrint::setFillStyle(FXFillStyle fillstyle){
  fill=fillstyle;
  }


// Set fill rule
void FXDCPrint::setFillRule(FXFillRule fillrule){
  rule=fillrule;
  }


// Set blit function
void FXDCPrint::setFunction(FXFunction func){
  rop=func;
  }


// Set tile image
void FXDCPrint::setTile(FXImage* image,FXint dx,FXint dy){
  tile=image;
  tx=dx;
  ty=dy;
  }


// Set stipple pattern
void FXDCPrint::setStipple(FXBitmap* bitmap,FXint dx,FXint dy){
  stipple=bitmap;
  pattern=STIPPLE_NONE;
  tx=dx;
  ty=dy;
  }


// Set stipple pattern
void FXDCPrint::setStipple(FXStipplePattern pat,FXint dx,FXint dy){
  stipple=NULL;
  pattern=pat;
  tx=dx;
  ty=dy;
  }


// Set clip rectangle
void FXDCPrint::setClipRectangle(FXint x,FXint y,FXint w,FXint h){
  clip.x=x;
  clip.y=y;
  clip.w=w;
  clip.h=h;
  }


// Set clip rectangle
void FXDCPrint::setClipRectangle(const FXRectangle& rectangle){
  clip=rectangle;
  }


// Clear clipping
void FXDCPrint::clearClipRectangle(){
  clip.x=0;
  clip.y=0;
  clip.w=32767;
  clip.h=32767;
  }


// Set clip mask
void FXDCPrint::setClipMask(FXBitmap* bitmap,FXint dx,FXint dy){
  mask=bitmap;
  cx=dx;
  cy=dy;
  }


// Clear clip mask
void FXDCPrint::clearClipMask(){
  }



// Set font to draw text with
void FXDCPrint::setFont(FXFont *fnt){
  font=fnt;
  }


// Change clip-against-child windows mode
void FXDCPrint::clipChildren(FXbool){
  }




/*

Contrib by "Vasudeva Upadhya" <kvu@cfd1.cfdrc.com>
FXint FXSetup::DoNTPrint(FXPrinter& printInfo){
	PDEVMODE pDevMode = NULL;
	LONG     lDevModeSize;
	HANDLE   hDevMode = NULL;
	HDC      pDC = NULL;
	DOCINFO  docInfo;
	FXString msg = FXString("License setup wizard was unable to print,\nto printer:")+FXString(printInfo.name);
	FXint    xOrigin;
	FXint    yOrigin;
	FXint    xSize;
	FXint    ySize;

	lDevModeSize = DocumentProperties(NULL,NULL,printInfo.name,NULL,NULL,0);
	if(lDevModeSize > 0){
		hDevMode = GlobalAlloc(GHND,lDevModeSize);
		pDevMode = (PDEVMODE)GlobalLock(hDevMode);
		DocumentProperties(NULL,NULL,printInfo.name,pDevMode,NULL,DM_OUT_BUFFER);
	  }
	pDC = CreateDC("WINSPOOL",printInfo.name,NULL,pDevMode);
	if(pDC){
		xOrigin = GetDeviceCaps(pDC,PHYSICALOFFSETX);
		yOrigin = GetDeviceCaps(pDC,PHYSICALOFFSETY);
		xSize   = GetDeviceCaps(pDC,HORZRES);
		ySize   = GetDeviceCaps(pDC,VERTRES);

		docInfo.cbSize       = sizeof(DOCINFO);
		docInfo.lpszDocName  = printInfo.name;
		docInfo.lpszOutput   = NULL;
		docInfo.lpszDatatype = NULL;
		docInfo.fwType       = 0;

		if(StartDoc(pDC,&docInfo)){
			if(StartPage(pDC)){
				FXint yPos  = yOrigin+200;
				FXint x1Pos = xOrigin+50;
				FXint x2Pos = xOrigin+600;
				FXint n;

				TextOut(pDC,x1Pos,yPos,"Customer Information:",strlen("Customer Information:"));
				yPos = yPos+50;
				for(n=0; n < sizeof(machineInfo)/sizeof(char*);n++){
					TextOut(pDC,x1Pos,yPos,machineInfo[n],strlen(machineInfo[n]));
					TextOut(pDC,x2Pos,yPos,lockID[n].text(),lockID[n].length());
					yPos = yPos+50;
				  }
				yPos = yPos+50;
				for(n=0; n < sizeof(CustomerInfo)/sizeof(char*);n++){
					TextOut(pDC,x1Pos,yPos,CustomerInfo[n],strlen(CustomerInfo[n]));
					TextOut(pDC,x2Pos,yPos,fieldTexts[n].text(),fieldTexts[n].length());
					yPos = yPos+50;
				  }
				if(EndPage(pDC)){
					if(EndDoc(pDC)){
						GlobalFree((PDEVMODE)pDevMode);
						GlobalFree((HANDLE)hDevMode);
						DeleteDC(pDC);
						return 1;
					  }
				  }
			  }
			else{
				GlobalFree((PDEVMODE)pDevMode);
				GlobalFree((HANDLE)hDevMode);
				DeleteDC(pDC);
				FXMessageBox::error(this,MBOX_OK,"Print Error",msg.text());
				return 0;
			  }
		  }
		else{
			GlobalFree((PDEVMODE)pDevMode);
			GlobalFree((HANDLE)hDevMode);
			FXMessageBox::error(this,MBOX_OK,"Print Error",msg.text());
			return 0;
		  }
	  }
	else{
		GlobalFree((PDEVMODE)pDevMode);
		GlobalFree((HANDLE)hDevMode);
		FXMessageBox::error(this,MBOX_OK,"Print Error",msg.text());
		return 0;
	  }
	return 1;
  }

*/

}

