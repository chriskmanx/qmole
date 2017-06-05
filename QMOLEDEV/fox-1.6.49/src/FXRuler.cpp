/********************************************************************************
*                                                                               *
*                            R u l e r   W i d g e t                            *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXRuler.cpp,v 1.55 2006/01/28 20:29:30 fox Exp $                         *
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
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXRuler.h"



/*
  Notes:
  - If showing arrows for cursor position, draw them down (right)
    when ticks are centered or bottom (right).

    Metric:

        0                             1                             2
        |                             |                             |
        |              |              |              |              |
        |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  .  .  .

    English:

        0                       1                       2
        |                       |                       |
        |           |           |           |           |           |
        |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
        |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  .  .  .

  - Subdivide repeatedly when zooming in/out:

    "pi",   1.0,   1, 2, 5, 10, 25, 50, 100, 250, 500, 1000,      1, 5, 10, 50, 100
    "in",  72.0,   1, 2, 4,  8, 16, 32,  64, 128, 256,  512,      1, 2,  4,  8,  16
    "cm", 28.35,   1, 2, 5, 10, 25, 50, 100, 250, 500, 1000,      1, 5, 10, 50, 100

  - Layout:


       edgeSpacing                                                             edgeSpacing
    |<-->|                                                                   |<-->|
    |    |                                                                   |    |
    +----+-------------------------------------------------------------------+----+
    |    |                                                                   |    |
    |    |                                                                   |    |
    |    |  marginLower                                         marginUpper  |    |
    |    |<---->|                                                     |<---->|    |
    |    |      |                                                     |      |    |
    |    +------+-----------------------------------------------------+------+    |
    |    |      |                                                     |      |    |
    |    |      |    indentFirst                                      |      |    |
    |    |      |<--------->|                                         |      |    |
    |    |      |           |                                         |      |    |
    |    |      |           |                                         |      |    |
    |    |      | indentLower                             indentUpper |      |    |
    |    |      |<---->|    |                                  |<---->|      |    |
    |    |      |      |    |                                  |      |      |    |
    |    |      +------+----+----------------------------------+------+      |    |
    |    |      |      |    |                                  |      |      |    |
    |    |      |      |    We, The People of the United States,      |      |    |
    |    |      |      in Order.................................      |      |    |
    |    |      |      .........................................      |      |    |
    |    |      |      .........................................      |      |    |
    |    |      |      .........................................      |      |    |
    |    |      |      .........................................      |      |    |
    |    |      |      |                                       |      |      |    |
    |    |      +------+---------------------------------------+------+      |    |
    |    |      |                                              |      |      |    |
    |    |      |                                              |      |      |    |
    |    +------+----------------------------------------------+------+------+    |
    |    |      |                                                     |      |    |
    |    |      |<--------------------------------------------------->|      |    |
    |    |            printable (must be smaller than docSize)               |    |
    |    |                                                                   |    |
    |    |                                                                   |    |
    |    |<----------------------------------------------------------------->|    |
    |                                documentSize                                 |
    |                                                                             |
    |                                                                             |
    +-----------------------------------------------------------------------------+
    |                                                                             |
    |                                                                             |
    |<--------------------------------------------------------------------------->|
                                   contentSize


  - Values of indentFirst, indentLower, may be negative, but not less than -marginLower.

  - Likewise indentUpper may be negative, but not less than -marginUpper.

  - Content width is documentSize+2*edgeSpacing; this may exceed viewport width, i.e.
    the width of the ruler itself (we assume the horizontal ruler extends over
    the entire usable viewport area).

  - If viewport larger that content, keep document centered in view; otherwise,
    keep document away from left edge by docSpace (and then we pop scrollbars
    into the picture).

  - Tickmarks start counting from left printable margin, i.e. common setting for
    lowerPara and upperPara would be 0, for full utilization of paper.

  - Ruler items:

      o Downpointing triangle is first line indent
      o Up pointing left triangle is left indent
      o Up pointing right triangle is right indent
      o Left, right darker area is margins

  - Metrics:

    1) Document width
    2) Document margins
    3) Paragraph start/end
    4) Paragraph firstline indent
    5) Paragraph tab positions and tab types
    6) Scroll offset

  - Should we constrain indent settings to multiples of pixelPerTick?
*/

#define ARROWBASE       9       // Size of cursor arrows (must be odd)
#define ARROWLENGTH     4       // The above divided by two
#define MARKERBASE      9       // Base of marker
#define MARKERLENGTH    4       // The above divided by two
#define EXTRASPACING    3       // Spacing below/above ticks or text
#define MAJORTICKSIZE   6       // Length of major ticks
#define MEDIUMTICKSIZE  4       // Length of medium ticks
#define MINORTICKSIZE   2       // Length of minor ticks
#define EDGESPACING     20      // Default space around edges of document
#define MARGINSPACE     25      // Default margin space (0.25in x 100dpi)
//#define DOCUMENTSIZE         850     // Default document size (8.5in x 100dpi)
#define DOCUMENTSIZE    600     // Default document size (8.5in x 100dpi)
#define FUDGE           2       // Fudge proximity

// Ruler style bits
#define RULER_MASK       (RULER_HORIZONTAL|RULER_VERTICAL|RULER_TICKS_TOP|RULER_TICKS_BOTTOM|RULER_NUMBERS|RULER_ARROW|RULER_MARKERS|RULER_METRIC|RULER_ENGLISH|RULER_MARGIN_ADJUST)

// Ruler alignment bits
#define RULER_ALIGN_MASK (RULER_ALIGN_CENTER|RULER_ALIGN_LEFT|RULER_ALIGN_RIGHT|RULER_ALIGN_STRETCH)

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXRuler) FXRulerMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXRuler::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXRuler::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXRuler::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXRuler::onLeftBtnRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXRuler::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXRuler::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_SETVALUE,FXRuler::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_SETINTVALUE,FXRuler::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_GETINTVALUE,FXRuler::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_SETHELPSTRING,FXRuler::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_GETHELPSTRING,FXRuler::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_SETTIPSTRING,FXRuler::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXRuler::ID_GETTIPSTRING,FXRuler::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXRuler,FXFrame,FXRulerMap,ARRAYNUMBER(FXRulerMap))


// Deserialization
FXRuler::FXRuler(){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  font=(FXFont*)-1L;
  documentSize=DOCUMENTSIZE;
  edgeSpacing=EDGESPACING;
  marginLower=MARGINSPACE;
  marginUpper=MARGINSPACE;
  indentFirst=0;
  indentLower=0;
  indentUpper=0;
  textColor=0;
  arrowPos=0;
  pixelPerTick=10.0;
  majorTicks=10;
  mediumTicks=5;
  tinyTicks=1;
  shift=0;
  pos=0;
  off=0;
  mode=MOUSE_NONE;
  }


// Make a label
FXRuler::FXRuler(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  target=tgt;
  message=sel;
  font=getApp()->getNormalFont();
  backColor=getApp()->getBackColor();
  textColor=getApp()->getForeColor();
  documentSize=DOCUMENTSIZE;
  edgeSpacing=EDGESPACING;
  marginLower=MARGINSPACE;
  marginUpper=MARGINSPACE;
  indentFirst=0;
  indentLower=0;
  indentUpper=0;
  arrowPos=0;
  pixelPerTick=10.0;
  if(options&RULER_ENGLISH){
    numberTicks=8;
    majorTicks=8;
    mediumTicks=4;
    tinyTicks=1;
    }
  else{
    numberTicks=10;
    majorTicks=10;
    mediumTicks=5;
    tinyTicks=1;
    }
  shift=0;
  pos=0;
  off=0;
  mode=MOUSE_NONE;
  }


// Create window
void FXRuler::create(){
  FXFrame::create();
  font->create();
  }


// Detach window
void FXRuler::detach(){
  FXFrame::detach();
  font->detach();
  }


// Get lower edge of document
FXint FXRuler::getDocumentLower() const {
  return shift+pos+edgeSpacing;
  }


// Get upper edge of document
FXint FXRuler::getDocumentUpper() const {
  return shift+pos+edgeSpacing+documentSize;
  }


// Get default width
FXint FXRuler::getDefaultWidth(){
  FXint tw,th,w=0;
  if(options&RULER_VERTICAL){           // Vertical
    if(options&RULER_NUMBERS){
      tw=font->getTextWidth("0",1);     // Ruler should be same width regardless of orientation
      th=font->getFontAscent();         // Since we use numbers only, don't account for descenders
      w=FXMAX(tw,th)+2;
      }
    if(options&(RULER_TICKS_LEFT|RULER_TICKS_RIGHT)){
      if(!(options&RULER_TICKS_LEFT)) w+=MAJORTICKSIZE;         // Ticks on right
      else if(!(options&RULER_TICKS_RIGHT)) w+=MAJORTICKSIZE;   // Ticks on left
      else w=FXMAX(MAJORTICKSIZE,w);                            // Ticks centered
      }
    w+=4;
    }
  else{
    // FIXME
    }
  return w+padleft+padright+(border<<1);
  }


// Get default height
FXint FXRuler::getDefaultHeight(){
  FXint tw,th,h=0;
  if(!(options&RULER_VERTICAL)){        // Horizontal
    if(options&RULER_NUMBERS){
      tw=font->getTextWidth("0",1);     // Ruler should be same width regardless of orientation
      th=font->getFontAscent();         // Since we use numbers only, don't account for descenders
      h=FXMAX(tw,th)+2;
      }
    if(options&(RULER_TICKS_TOP|RULER_TICKS_BOTTOM)){
      if(!(options&RULER_TICKS_TOP)) h+=MAJORTICKSIZE;          // Ticks on bottom
      else if(!(options&RULER_TICKS_BOTTOM)) h+=MAJORTICKSIZE;  // Ticks on top
      else h=FXMAX(MAJORTICKSIZE,h);                            // Ticks centered
      }
    h+=4;
    }
  else{
    // FIXME
    }
  return h+padtop+padbottom+(border<<1);
  }


// Recalculate layout
void FXRuler::layout(){
  FXint space=(options&RULER_VERTICAL) ? height : width;

  // Stretched
  if((options&RULER_ALIGN_LEFT) && (options&RULER_ALIGN_RIGHT)){
    shift=0;
    setContentSize(space,TRUE);
    }

  // Left aligned
  else if(options&RULER_ALIGN_LEFT){
    shift=0;
    }

  // Right-aligned
  else if(options&RULER_ALIGN_RIGHT){
    shift=space-getContentSize();
    }

  // Centered
  else{
    shift=(space-getContentSize())>>1;
    }

  // Keep positive
  if(shift<0) shift=0;

  setValue(arrowPos);

  // Redraw
  update();

  // Clean
  flags&=~FLAG_DIRTY;
  }


// Draw left arrow, with point at x,y
void FXRuler::drawLeftArrow(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[3];
  points[0].x=x+ARROWLENGTH;
  points[0].y=y-ARROWLENGTH;
  points[1].x=x+ARROWLENGTH;
  points[1].y=y+ARROWLENGTH;
  points[2].x=x;
  points[2].y=y;
  dc.fillPolygon(points,3);
  }


// Draw right arrow, with point at x,y
void FXRuler::drawRightArrow(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[3];
  points[0].x=x-ARROWLENGTH+1;
  points[0].y=y-ARROWLENGTH;
  points[1].x=x-ARROWLENGTH+1;
  points[1].y=y+ARROWLENGTH;
  points[2].x=x+1;
  points[2].y=y;
  dc.fillPolygon(points,3);
  }


// Draw up arrow, with point at x,y
void FXRuler::drawUpArrow(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[3];
  points[0].x=x;
  points[0].y=y-1;
  points[1].x=x-ARROWLENGTH;
  points[1].y=y+ARROWLENGTH;
  points[2].x=x+ARROWLENGTH;
  points[2].y=y+ARROWLENGTH;
  dc.fillPolygon(points,3);
  }


// Draw down arrow, with point at x,y
void FXRuler::drawDownArrow(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[3];
  points[0].x=x-ARROWLENGTH+1;
  points[0].y=y-ARROWLENGTH+1;
  points[1].x=x+ARROWLENGTH;
  points[1].y=y-ARROWLENGTH+1;
  points[2].x=x;
  points[2].y=y+1;
  dc.fillPolygon(points,3);
  }


// Draw left marker
void FXRuler::drawLeftMarker(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[6];
  points[0].x=x;
  points[0].y=y;
  points[1].x=x+MARKERLENGTH;
  points[1].y=y-MARKERLENGTH;
  points[2].x=x+MARKERLENGTH+MARKERLENGTH-1;
  points[2].y=y-MARKERLENGTH;
  points[3].x=x+MARKERLENGTH+MARKERLENGTH-1;
  points[3].y=y+MARKERLENGTH;
  points[4].x=x+MARKERLENGTH;
  points[4].y=y+MARKERLENGTH;
  points[5].x=x;
  points[5].y=y;
  dc.setForeground(baseColor);
  dc.fillPolygon(points,5);
  dc.setForeground(textColor);
  dc.drawLines(points,6);
  points[0].x=x+1;
  points[0].y=y;
  points[1].x=x+MARKERLENGTH;
  points[1].y=y+MARKERLENGTH-1;
  points[2].x=x+MARKERLENGTH+MARKERLENGTH-2;
  points[2].y=y+MARKERLENGTH-1;
  points[3].x=x+MARKERLENGTH+MARKERLENGTH-2;
  points[3].y=y-MARKERLENGTH+1;
  dc.setForeground(shadowColor);
  dc.drawLines(points,4);
  points[0].x=x+1;
  points[0].y=y;
  points[1].x=x+MARKERLENGTH;
  points[1].y=y-MARKERLENGTH+1;
  points[2].x=x+MARKERLENGTH+MARKERLENGTH-2;
  points[2].y=y-MARKERLENGTH+1;
  dc.setForeground(hiliteColor);
  dc.drawLines(points,3);
  }


// Draw right marker
void FXRuler::drawRightMarker(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[6];
  points[0].x=x;
  points[0].y=y;
  points[1].x=x-MARKERLENGTH;
  points[1].y=y-MARKERLENGTH;
  points[2].x=x-MARKERLENGTH-MARKERLENGTH+1;
  points[2].y=y-MARKERLENGTH;
  points[3].x=x-MARKERLENGTH-MARKERLENGTH+1;
  points[3].y=y+MARKERLENGTH;
  points[4].x=x-MARKERLENGTH;
  points[4].y=y+MARKERLENGTH;
  points[5].x=x;
  points[5].y=y;
  dc.setForeground(baseColor);
  dc.fillPolygon(points,5);
  dc.setForeground(textColor);
  dc.drawLines(points,6);
  points[0].x=x-1;
  points[0].y=y;
  points[1].x=x-MARKERLENGTH;
  points[1].y=y+MARKERLENGTH-1;
  points[2].x=x-MARKERLENGTH-MARKERLENGTH+3;
  points[2].y=y+MARKERLENGTH-1;
  dc.setForeground(shadowColor);
  dc.drawLines(points,3);
  points[0].x=x-1;
  points[0].y=y;
  points[1].x=x-MARKERLENGTH;
  points[1].y=y-MARKERLENGTH+1;
  points[2].x=x-MARKERLENGTH-MARKERLENGTH+2;
  points[2].y=y-MARKERLENGTH+1;
  points[3].x=x-MARKERLENGTH-MARKERLENGTH+2;
  points[3].y=y+MARKERLENGTH-1;
  dc.setForeground(hiliteColor);
  dc.drawLines(points,4);
  }


// Draw up marker
void FXRuler::drawUpMarker(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[6];
  points[0].x=x;
  points[0].y=y;
  points[1].x=x-MARKERLENGTH;
  points[1].y=y+MARKERLENGTH;
  points[2].x=x-MARKERLENGTH;
  points[2].y=y+MARKERLENGTH+MARKERLENGTH-1;
  points[3].x=x+MARKERLENGTH;
  points[3].y=y+MARKERLENGTH+MARKERLENGTH-1;
  points[4].x=x+MARKERLENGTH;
  points[4].y=y+MARKERLENGTH;
  points[5].x=x;
  points[5].y=y;
  dc.setForeground(baseColor);
  dc.fillPolygon(points,5);
  dc.setForeground(textColor);
  dc.drawLines(points,6);
  points[0].x=x;
  points[0].y=y+1;
  points[1].x=x+MARKERLENGTH-1;
  points[1].y=y+MARKERLENGTH;
  points[2].x=x+MARKERLENGTH-1;
  points[2].y=y+MARKERLENGTH+MARKERLENGTH-2;
  points[3].x=x-MARKERLENGTH+1;
  points[3].y=y+MARKERLENGTH+MARKERLENGTH-2;
  dc.setForeground(shadowColor);
  dc.drawLines(points,4);
  points[0].x=x;
  points[0].y=y+1;
  points[1].x=x-MARKERLENGTH+1;
  points[1].y=y+MARKERLENGTH;
  points[2].x=x-MARKERLENGTH+1;
  points[2].y=y+MARKERLENGTH+MARKERLENGTH-3;
  dc.setForeground(hiliteColor);
  dc.drawLines(points,3);
  }


// Draw down marker
void FXRuler::drawDownMarker(FXDCWindow& dc,FXint x,FXint y){
  FXPoint points[6];
  points[0].x=x;
  points[0].y=y;
  points[1].x=x-MARKERLENGTH;
  points[1].y=y-MARKERLENGTH;
  points[2].x=x-MARKERLENGTH;
  points[2].y=y-MARKERLENGTH-MARKERLENGTH+1;
  points[3].x=x+MARKERLENGTH;
  points[3].y=y-MARKERLENGTH-MARKERLENGTH+1;
  points[4].x=x+MARKERLENGTH;
  points[4].y=y-MARKERLENGTH;
  points[5].x=x;
  points[5].y=y;
  dc.setForeground(baseColor);
  dc.fillPolygon(points,5);
  dc.setForeground(textColor);
  dc.drawLines(points,6);
  points[0].x=x;
  points[0].y=y-1;
  points[1].x=x+MARKERLENGTH-1;
  points[1].y=y-MARKERLENGTH;
  points[2].x=x+MARKERLENGTH-1;
  points[2].y=y-MARKERLENGTH-MARKERLENGTH+3;
  dc.setForeground(shadowColor);
  dc.drawLines(points,3);
  points[0].x=x;
  points[0].y=y-1;
  points[1].x=x-MARKERLENGTH+1;
  points[1].y=y-MARKERLENGTH;
  points[2].x=x-MARKERLENGTH+1;
  points[2].y=y-MARKERLENGTH-MARKERLENGTH+2;
  points[3].x=x+MARKERLENGTH-1;
  points[3].y=y-MARKERLENGTH-MARKERLENGTH+2;
  dc.setForeground(hiliteColor);
  dc.drawLines(points,4);
  }


// Handle repaint
long FXRuler::onPaint(FXObject*,FXSelector,void* ptr){
  FXint boxx,boxy,boxw,boxh,p,tick,lower,upper,th,tw;
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXchar numeral;

  // Background
  dc.setForeground(baseColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);

  // Set font for numbers
  dc.setFont(font);
  th=font->getFontAscent();

  // Vertically oriented ruler
  if(options&RULER_VERTICAL){

    // Document well size
    boxx=border+padleft;
    boxy=getDocumentLower()+marginLower;
    boxw=width-padleft-padright-border-border;
    boxh=documentSize-marginUpper-marginLower;

    // Draw cartouche
    drawGrooveRectangle(dc,boxx,boxy-marginLower-1,boxw,documentSize+2);

    // Draw document well
    dc.setForeground(backColor);
    dc.fillRectangle(boxx+2,boxy+1,boxw-4,boxh);

    dc.setForeground(shadowColor);
    dc.fillRectangle(boxx,boxy-1,boxw-2,1);
    dc.fillRectangle(boxx,boxy+boxh-1,boxw-2,1);

    dc.setForeground(borderColor);
    dc.fillRectangle(boxx+1,boxy,boxw-3,1);
    dc.fillRectangle(boxx+1,boxy,1,boxh);

    dc.setForeground(baseColor);
    dc.fillRectangle(boxx+2,boxy+boxh-1,boxw-4,1);

    // Draw ticks or numbers
    if(options&(RULER_TICKS_TOP|RULER_TICKS_BOTTOM|RULER_NUMBERS)){
      FXASSERT(pixelPerTick>0.0);

      // Determine number of ticks to draw
      lower=-(FXint)((FXdouble)marginLower/pixelPerTick);
      upper=(FXint)((FXdouble)(documentSize+pixelPerTick-marginLower-1)/pixelPerTick);

      dc.setForeground(borderColor);

      // Draw ticks and numbers
      for(tick=lower; tick<upper; tick++){

        // Skip zero, it looks ugly
        if(tick){

          // Figure position
          p=boxy+(FXint)(0.5+tick*pixelPerTick);

          // Draw number
          if((options&RULER_NUMBERS) && (tick%numberTicks==0)){

            numeral='0'+(FXint)((FXABS(tick)/majorTicks)%10);

            tw=font->getTextWidth(&numeral,1);

            if((options&RULER_TICKS_LEFT)&&(options&RULER_TICKS_RIGHT)){
              dc.drawText(boxx+((boxw-tw)>>1),p+(th>>1),&numeral,1);
              }
            else if(options&RULER_TICKS_LEFT){
              dc.drawText(boxx+2+MAJORTICKSIZE+1,p+(th>>1),&numeral,1);
              }
            else{
              dc.drawText(boxx+boxw-4-MAJORTICKSIZE-1-tw,p+(th>>1),&numeral,1);
              }
            }

          // Draw major tick
          if(tick%majorTicks==0){
            if((options&RULER_TICKS_TOP)&&(options&RULER_TICKS_RIGHT)){
              if(!(options&RULER_NUMBERS) || (tick%numberTicks)) dc.fillRectangle(boxx+(boxw>>1)-(MAJORTICKSIZE>>1),p,MAJORTICKSIZE,1);
              }
            else if(options&RULER_TICKS_LEFT){
              dc.fillRectangle(boxx+2,p,MAJORTICKSIZE,1);
              }
            else{
              dc.fillRectangle(boxx+boxw-2-MAJORTICKSIZE,p,MAJORTICKSIZE,1);
              }
            }

          // Draw medium tick
          else if(tick%mediumTicks==0){
            if((options&RULER_TICKS_LEFT)&&(options&RULER_TICKS_RIGHT)){
              dc.fillRectangle(boxx+(boxw>>1)-(MEDIUMTICKSIZE>>1),p,MEDIUMTICKSIZE,1);
              }
            else if(options&RULER_TICKS_LEFT){
              dc.fillRectangle(boxx+2,p,MEDIUMTICKSIZE,1);
              }
            else{
              dc.fillRectangle(boxx+boxw-2-MEDIUMTICKSIZE,p,MEDIUMTICKSIZE,1);
              }
            }

          // Draw tiny tick
          else if(tick%tinyTicks==0){
            if((options&RULER_TICKS_LEFT)&&(options&RULER_TICKS_RIGHT)){
              dc.fillRectangle(boxx+(boxw>>1)-(MINORTICKSIZE>>1),p,MINORTICKSIZE,1);
              }
            else if(options&RULER_TICKS_LEFT){
              dc.fillRectangle(boxx+2,p,MINORTICKSIZE,1);
              }
            else{
              dc.fillRectangle(boxx+boxw-2-MINORTICKSIZE,p,MINORTICKSIZE,1);
              }
            }
          }
        }
      }

    // Draw optional arrow to signify cursor location
    if(options&RULER_ARROW){
      dc.setForeground(textColor);
      if(options&RULER_TICKS_RIGHT)                     // Ticks on right or center
        drawRightArrow(dc,boxx+boxw-3,getDocumentLower()+arrowPos);
      else if(options&RULER_TICKS_LEFT)                 // Ticks on left
        drawLeftArrow(dc,boxx+2,getDocumentLower()+arrowPos);
      }

    // Draw optional markers for paragraph margins
    if(options&RULER_MARKERS){
      dc.setForeground(textColor);
      drawLeftMarker(dc,boxx+boxw-MARKERLENGTH-MARKERLENGTH+1,boxy+indentLower);
      drawLeftMarker(dc,boxx+boxw-MARKERLENGTH-MARKERLENGTH+1,boxy+boxh-indentUpper-1);
      }
    }

  // Horizontally oriented ruler
  else{

    // Document well size
    boxx=getDocumentLower()+marginLower;
    boxy=border+padtop;
    boxw=documentSize-marginUpper-marginLower;
    boxh=height-padtop-padbottom-border-border;

    // Draw cartouche
    drawGrooveRectangle(dc,boxx-marginLower-1,boxy,documentSize+2,boxh);

    // Draw document well
    dc.setForeground(backColor);
    dc.fillRectangle(boxx+1,boxy+2,boxw,boxh-4);
    dc.setForeground(shadowColor);
    dc.fillRectangle(boxx-1,boxy,1,boxh-2);
    dc.fillRectangle(boxx+boxw-1,boxy+1,1,boxh-2);
    dc.setForeground(borderColor);
    dc.fillRectangle(boxx,boxy+1,boxw-1,1);
    dc.fillRectangle(boxx,boxy+1,1,boxh-2);
    dc.setForeground(baseColor);
    dc.fillRectangle(boxx+1,boxy+boxh-2,boxw-2,1);

    // Draw ticks or numbers
    if(options&(RULER_TICKS_TOP|RULER_TICKS_BOTTOM|RULER_NUMBERS)){
      FXASSERT(pixelPerTick>0.0);

      // Determine number of ticks to draw
      lower=-(FXint)((FXdouble)marginLower/pixelPerTick);
      upper=(FXint)((FXdouble)(documentSize+pixelPerTick-marginLower-1)/pixelPerTick);

      dc.setForeground(borderColor);

      // Draw ticks and numbers
      for(tick=lower; tick<upper; tick++){

        // Skip zero, it looks ugly
        if(tick){

          // Figure position
          p=boxx+(FXint)(0.5+tick*pixelPerTick);

          // Draw number
          if((options&RULER_NUMBERS) && (tick%numberTicks==0)){

            numeral='0'+(FXint)((FXABS(tick)/majorTicks)%10);

            tw=font->getTextWidth(&numeral,1);

            if((options&RULER_TICKS_TOP)&&(options&RULER_TICKS_BOTTOM)){
              dc.drawText(p-(tw>>1),boxy+((boxh-th)>>1)+th-1,&numeral,1);
              }
            else if(options&RULER_TICKS_TOP){
              dc.drawText(p-(tw>>1),boxy+2+MAJORTICKSIZE+th,&numeral,1);
              }
            else{
              dc.drawText(p-(tw>>1),boxy+boxh-4-MAJORTICKSIZE,&numeral,1);
              }
            }

          // Draw major tick
          if(tick%majorTicks==0){
            if((options&RULER_TICKS_TOP)&&(options&RULER_TICKS_BOTTOM)){
              if(!(options&RULER_NUMBERS) || (tick%numberTicks)) dc.fillRectangle(p,boxy+(boxh>>1)-(MAJORTICKSIZE>>1),1,MAJORTICKSIZE);
              }
            else if(options&RULER_TICKS_TOP){
              dc.fillRectangle(p,boxy+2,1,MAJORTICKSIZE);
              }
            else{
              dc.fillRectangle(p,boxy+boxh-2-MAJORTICKSIZE,1,MAJORTICKSIZE);
              }
            }

          // Draw medium tick
          else if(tick%mediumTicks==0){
            if((options&RULER_TICKS_TOP)&&(options&RULER_TICKS_BOTTOM)){
              dc.fillRectangle(p,boxy+(boxh>>1)-(MEDIUMTICKSIZE>>1),1,MEDIUMTICKSIZE);
              }
            else if(options&RULER_TICKS_TOP){
              dc.fillRectangle(p,boxy+2,1,MEDIUMTICKSIZE);
              }
            else{
              dc.fillRectangle(p,boxy+boxh-2-MEDIUMTICKSIZE,1,MEDIUMTICKSIZE);
              }
            }

          // Draw tiny tick
          else if(tick%tinyTicks==0){
            if((options&RULER_TICKS_TOP)&&(options&RULER_TICKS_BOTTOM)){
              dc.fillRectangle(p,boxy+(boxh>>1)-(MINORTICKSIZE>>1),1,MINORTICKSIZE);
              }
            else if(options&RULER_TICKS_TOP){
              dc.fillRectangle(p,boxy+2,1,MINORTICKSIZE);
              }
            else{
              dc.fillRectangle(p,boxy+boxh-2-MINORTICKSIZE,1,MINORTICKSIZE);
              }
            }
          }
        }
      }

    // Draw optional arrow to signify cursor location
    if(options&RULER_ARROW){
      dc.setForeground(textColor);
      if(options&RULER_TICKS_BOTTOM)                    // Ticks on bottom or center
        drawDownArrow(dc,getDocumentLower()+arrowPos,boxy+boxh-3);
      else if(options&RULER_TICKS_TOP)                  // Ticks on top
        drawUpArrow(dc,getDocumentLower()+arrowPos,boxy+2);
      }

    // Draw optional markers for paragraph margins
    if(options&RULER_MARKERS){
      drawDownMarker(dc,boxx+indentFirst,boxy+MARKERLENGTH+MARKERLENGTH-2);
      drawUpMarker(dc,boxx+indentLower,boxy+boxh-MARKERLENGTH-MARKERLENGTH+1);
      drawUpMarker(dc,boxx+boxw-indentUpper-1,boxy+boxh-MARKERLENGTH-MARKERLENGTH+1);
      }
    }

  // Frame it
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Pressed LEFT button
long FXRuler::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  register FXEvent* event=(FXEvent*)ptr;
  register FXint lo,hi;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    mode=picked(event->win_x,event->win_y);
    if(mode){
      lo=pos+edgeSpacing+marginLower;
      hi=lo+documentSize-marginUpper-marginLower;
      if(options&RULER_VERTICAL){
        setDragCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
        off=event->win_y;
        }
      else{
        setDragCursor(getApp()->getDefaultCursor(DEF_DRAGV_CURSOR));
        off=event->win_x;
        }
      switch(mode){
        case MOUSE_MARG_LOWER: off-=lo; break;
        case MOUSE_MARG_UPPER: off-=hi; break;
        case MOUSE_PARA_FIRST: off-=lo+indentFirst; break;
        case MOUSE_PARA_LOWER: off-=lo+indentLower; break;
        case MOUSE_PARA_UPPER: off-=hi-indentUpper; break;
        }
      }
    return 1;
    }
  return 0;
  }


// Released Left button
long FXRuler::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    mode=MOUSE_NONE;
    setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }


// Determine what was picked
FXint FXRuler::picked(FXint x,FXint y){
  register FXint wlo,whi,lo,hi;
  lo=pos+edgeSpacing+marginLower;
  hi=lo+documentSize-marginUpper-marginLower;
  if(options&RULER_VERTICAL){
    wlo=border+padleft;
    whi=width-padright;
    if(x<wlo+MARKERLENGTH){
      }
    else if(x>whi-MARKERLENGTH){
      if(lo+indentLower-MARKERLENGTH<y && y<lo+indentLower+MARKERLENGTH) return MOUSE_PARA_LOWER;
      if(hi-indentUpper-MARKERLENGTH<y && y<hi-indentUpper+MARKERLENGTH) return MOUSE_PARA_UPPER;
      }
    else if(options&RULER_MARGIN_ADJUST){
      if(lo-FUDGE<y && y<lo+FUDGE) return MOUSE_MARG_LOWER;
      if(hi-FUDGE<y && y<hi+FUDGE) return MOUSE_MARG_UPPER;
      }
    }
  else{
    wlo=border+padtop;
    whi=height-padbottom;
    if(y<wlo+MARKERLENGTH){
      if(lo+indentFirst-MARKERLENGTH<x && x<lo+indentFirst+MARKERLENGTH) return MOUSE_PARA_FIRST;
      }
    else if(y>whi-MARKERLENGTH){
      if(lo+indentLower-MARKERLENGTH<x && x<lo+indentLower+MARKERLENGTH) return MOUSE_PARA_LOWER;
      if(hi-indentUpper-MARKERLENGTH<x && x<hi-indentUpper+MARKERLENGTH) return MOUSE_PARA_UPPER;
      }
    else if(options&RULER_MARGIN_ADJUST){
      if(lo-FUDGE<x && x<lo+FUDGE) return MOUSE_MARG_LOWER;
      if(hi-FUDGE<x && x<hi+FUDGE) return MOUSE_MARG_UPPER;
      }
    }
  return MOUSE_NONE;
  }


// Moving
long FXRuler::onMotion(FXObject*,FXSelector,void*ptr){
  register FXEvent* event=(FXEvent*)ptr;
  FXint value=(options&RULER_VERTICAL)?(event->win_y+off):(event->win_x+off);
  switch(mode){
    case MOUSE_NONE:
      if(picked(event->win_x,event->win_y)){
        if(options&RULER_VERTICAL){
          setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
          }
        else{
          setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGV_CURSOR));
          }
        }
      else{
        setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
        }
      return 0;
    case MOUSE_MARG_LOWER:
      setMarginLower(value-pos-edgeSpacing,TRUE);
      return 1;
    case MOUSE_MARG_UPPER:
      setMarginUpper(pos+edgeSpacing+documentSize-value,TRUE);
      return 1;
    case MOUSE_PARA_FIRST:
      setIndentFirst(value-pos-edgeSpacing-marginLower,TRUE);
      return 1;
    case MOUSE_PARA_LOWER:
      setIndentLower(value-pos-edgeSpacing-marginLower,TRUE);
      return 1;
    case MOUSE_PARA_UPPER:
      setIndentUpper(pos+edgeSpacing+documentSize-marginUpper-value,TRUE);
      return 1;
    }
  return 0;
  }


// Change arrow location
void FXRuler::setValue(FXint val){
  val=FXCLAMP(0,val,documentSize);
  if(options&RULER_VERTICAL){
    if(arrowPos!=val){
      if(options&RULER_ARROW){
        update(padleft+border,getDocumentLower()+arrowPos-ARROWLENGTH,width-padleft-padright-(border<<1),ARROWBASE);
        update(padleft+border,getDocumentLower()+val-ARROWLENGTH,width-padleft-padright-(border<<1),ARROWBASE);
        }
      arrowPos=val;
      }
    }
  else{
    if(arrowPos!=val){
      if(options&RULER_ARROW){
        update(getDocumentLower()+arrowPos-ARROWLENGTH,padtop+border,ARROWBASE,height-padtop-padbottom-(border<<1));
        update(getDocumentLower()+val-ARROWLENGTH,padtop+border,ARROWBASE,height-padtop-padbottom-(border<<1));
        }
      arrowPos=val;
      }
    }
  }


// Update value from a message
long FXRuler::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setValue((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXRuler::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setValue(*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXRuler::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getValue();
  return 1;
  }


// Set help using a message
long FXRuler::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXRuler::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXRuler::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXRuler::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXRuler::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXRuler::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Change content size
void FXRuler::setContentSize(FXint size,FXbool notify){
  setDocumentSize(size-edgeSpacing-edgeSpacing,notify);
  }


// Get content size
FXint FXRuler::getContentSize() const {
  return edgeSpacing+documentSize+edgeSpacing;
  }


// Set position, scrolling contents
void FXRuler::setPosition(FXint p,FXbool notify){
  if(pos!=p){
    if(options&RULER_VERTICAL)
      scroll(0,0,width,height,0,p-pos);
    else
      scroll(0,0,width,height,p-pos,0);
    arrowPos+=p-pos;
    pos=p;
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change document size
void FXRuler::setDocumentSize(FXint size,FXbool notify){
  if(size<0) size=0;
  if(documentSize!=size){
    documentSize=size;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return document edge spacing
void FXRuler::setEdgeSpacing(FXint space,FXbool notify){
  if(space<0) space=0;
  if(edgeSpacing!=space){
    edgeSpacing=space;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return lower document margin
void FXRuler::setMarginLower(FXint mgn,FXbool notify){
  if(mgn<0) mgn=0;
  if(mgn>=documentSize-marginUpper) mgn=documentSize-marginUpper-1;
  if(marginLower!=mgn){
    marginLower=mgn;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return upper document margin
void FXRuler::setMarginUpper(FXint mgn,FXbool notify){
  if(mgn<0) mgn=0;
  if(mgn>=documentSize-marginLower) mgn=documentSize-marginLower-1;
  if(marginUpper!=mgn){
    marginUpper=mgn;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return first line indent
void FXRuler::setIndentFirst(FXint ind,FXbool notify){
  if(ind<-marginLower) ind=-marginLower;
  if(ind>documentSize-marginLower) ind=documentSize-marginLower;
  if(indentFirst!=ind){
    indentFirst=ind;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return lower indent
void FXRuler::setIndentLower(FXint ind,FXbool notify){
  if(ind<-marginLower) ind=-marginLower;
  if(ind>documentSize-marginLower) ind=documentSize-marginLower;
  if(indentLower!=ind){
    indentLower=ind;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return upper indent
void FXRuler::setIndentUpper(FXint ind,FXbool notify){
  if(ind<-marginUpper) ind=-marginUpper;
  if(ind>documentSize-marginLower) ind=documentSize-marginLower;
  if(indentUpper!=ind){
    indentUpper=ind;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return document number placement
void FXRuler::setNumberTicks(FXint ticks,FXbool notify){
  if(ticks<1){ fxerror("%s::setNumberTicks: illegal tick spacing.\n",getClassName()); }
  if(numberTicks!=ticks){
    numberTicks=ticks;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return document major ticks
void FXRuler::setMajorTicks(FXint ticks,FXbool notify){
  if(ticks<1){ fxerror("%s::setMajorTicks: illegal tick spacing.\n",getClassName()); }
  if(majorTicks!=ticks){
    majorTicks=ticks;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return document medium ticks
void FXRuler::setMediumTicks(FXint ticks,FXbool notify){
  if(ticks<1){ fxerror("%s::setMediumTicks: illegal tick spacing.\n",getClassName()); }
  if(mediumTicks!=ticks){
    mediumTicks=ticks;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return document tiny ticks
void FXRuler::setTinyTicks(FXint ticks,FXbool notify){
  if(ticks<1){ fxerror("%s::setTinyTicks: illegal tick spacing.\n",getClassName()); }
  if(tinyTicks!=ticks){
    tinyTicks=ticks;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change/return pixel per tick spacing
void FXRuler::setPixelPerTick(FXdouble space,FXbool notify){
  if(space<=0.0){ fxerror("%s::setPixelPerTick: illegal pixel per tick value.\n",getClassName()); }
  if(pixelPerTick!=space){
    pixelPerTick=space;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Change the font
void FXRuler::setFont(FXFont *fnt,FXbool notify){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Set text color
void FXRuler::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update();
    }
  }


// Set ruler style
void FXRuler::setRulerStyle(FXuint style){
  FXuint opts=(options&~RULER_MASK) | (style&RULER_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get ruler style
FXuint FXRuler::getRulerStyle() const {
  return (options&RULER_MASK);
  }


// Set ruler alignment
void FXRuler::setRulerAlignment(FXuint alignment,FXbool notify){
  FXuint opts=(options&~RULER_ALIGN_MASK) | (alignment&RULER_ALIGN_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),NULL);
    }
  }


// Get ruler alignment
FXuint FXRuler::getRulerAlignment() const {
  return (options&RULER_ALIGN_MASK);
  }


// Save object to stream
void FXRuler::save(FXStream& store) const {
  FXFrame::save(store);
  store << font;
  store << documentSize;
  store << edgeSpacing;
  store << marginLower;
  store << marginUpper;
  store << indentFirst;
  store << indentLower;
  store << indentUpper;
  store << pixelPerTick;
  store << numberTicks;
  store << majorTicks;
  store << mediumTicks;
  store << tinyTicks;
  store << textColor;
  store << tip;
  store << help;
  }


// Load object from stream
void FXRuler::load(FXStream& store){
  FXFrame::load(store);
  store >> font;
  store >> documentSize;
  store >> edgeSpacing;
  store >> marginLower;
  store >> marginUpper;
  store >> indentFirst;
  store >> indentLower;
  store >> indentUpper;
  store >> pixelPerTick;
  store >> numberTicks;
  store >> majorTicks;
  store >> mediumTicks;
  store >> tinyTicks;
  store >> textColor;
  store >> tip;
  store >> help;
  }


// Destroy label
FXRuler::~FXRuler(){
  font=(FXFont*)-1L;
  }

}
