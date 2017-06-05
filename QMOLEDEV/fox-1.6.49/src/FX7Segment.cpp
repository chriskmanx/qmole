/********************************************************************************
*                                                                               *
*                7 - S e g m e n t   D i s p l a y   W i d g e t                *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FX7Segment.cpp,v 1.19.2.1 2007/08/11 00:50:32 fox Exp $                      *
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
#include "FXDCWindow.h"
#include "FXFrame.h"
#include "FX7Segment.h"


/*
  Notes:
  - Emulate old LED or LCD Display.
  - Segment numbering:

             01
           -----
       02 |     | 04      ++
          |  08 |         ++
           -----
       10 |     | 20      ++    100
          |     |         ++
           -----  o 80
             40

  - Still to add: decimal point, a few more letters.
  - Perhaps some control over cell spacing.
*/


#define JUSTIFY_MASK            (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)
#define SEVENSEGMENT_MASK       (SEVENSEGMENT_NORMAL|SEVENSEGMENT_SHADOW)


using namespace FX;

/*******************************************************************************/

namespace FX {


const FXuint segm[]={
  0x0000,         // 20   SPACE
  0x0000,         // 21   !
  0x0000,         // 22   "
  0x0000,         // 23   #
  0x0000,         // 24   $
  0x0000,         // 25   %
  0x0000,         // 26   &
  0x0000,         // 27   '
  0x0053,         // 28   (
  0x0065,         // 29   )
  0x0000,         // 2A   *
  0x0000,         // 2B   +
  0x0080,         // 2C   ,
  0x0008,         // 2D   -
  0x0080,         // 2E   .
  0x0000,         // 2F   /

  0x0077,         // 30   0
  0x0024,         // 31   1
  0x005d,         // 32   2
  0x006d,         // 33   3
  0x002e,         // 34   4
  0x006b,         // 35   5
  0x007b,         // 36   6
  0x0025,         // 37   7
  0x007f,         // 38   8
  0x006f,         // 39   9
  0x0100,         // 3A   :
  0x0100,         // 3B   ;
  0x0000,         // 3C   <
  0x0048,         // 3D   =
  0x0000,         // 3E   >
  0x0000,         // 3F   ?

  0x0000,         // 40   @
  0x003F,         // 41   A
  0x007A,         // 42   B
  0x0058,         // 43   C
  0x007C,         // 44   D
  0x005B,         // 45   E
  0x001B,         // 46   F
  0x0073,         // 47   G
  0x003A,         // 48   H
  0x0024,         // 49   I
  0x0064,         // 4A   J
  0x001A,         // 4B   K
  0x0052,         // 4C   L
  0x0037,         // 4D   M
  0x0038,         // 4E   N
  0x0078,         // 4F   O

  0x001F,         // 50   P
  0x002F,         // 51   Q
  0x0018,         // 52   R
  0x006B,         // 53   S
  0x005A,         // 54   T
  0x0070,         // 55   U
  0x0076,         // 56   V
  0x0076,         // 57   W
  0x0049,         // 58   X
  0x006E,         // 59   Y
  0x005d,         // 5A   Z
  0x0053,         // 5B   [
  0x0000,         // 5C
  0x0065,         // 5D   ]
  0x0000,         // 5E   ^
  0x0040,         // 5F   _

  0x0000,         // 60   `
  0x003F,         // 61   a
  0x007A,         // 62   b
  0x0058,         // 63   c
  0x007C,         // 64   d
  0x005b,         // 65   e
  0x001B,         // 66   f
  0x0073,         // 67   g
  0x003A,         // 68   h
  0x0024,         // 69   i
  0x0064,         // 6A   j
  0x001A,         // 6B   k
  0x0052,         // 6C   l
  0x0037,         // 6D   m
  0x0038,         // 6E   n
  0x0078,         // 6F   o

  0x001F,         // 70   p
  0x002F,         // 71   q
  0x0018,         // 72   r
  0x006B,         // 73   s
  0x005A,         // 74   t
  0x0070,         // 75   u
  0x0076,         // 76   v
  0x0076,         // 77   w
  0x0049,         // 78   x
  0x006E,         // 79   y
  0x005d,         // 7A   z
  0x0053,         // 7B   {
  0x0024,         // 7C   |
  0x0065,         // 7D   }
  0x0001,         // 7E   ~
  };


// map
FXDEFMAP(FX7Segment) FX7SegmentMap[]={
  FXMAPFUNC(SEL_PAINT,0,FX7Segment::onPaint),
  FXMAPFUNC(SEL_QUERY_TIP,0,FX7Segment::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FX7Segment::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_SETVALUE,FX7Segment::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_SETINTVALUE,FX7Segment::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_SETREALVALUE,FX7Segment::onCmdSetRealValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_SETSTRINGVALUE,FX7Segment::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_GETINTVALUE,FX7Segment::onCmdGetIntValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_GETREALVALUE,FX7Segment::onCmdGetRealValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_GETSTRINGVALUE,FX7Segment::onCmdGetStringValue),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_SETHELPSTRING,FX7Segment::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_GETHELPSTRING,FX7Segment::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_SETTIPSTRING,FX7Segment::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FX7Segment::ID_GETTIPSTRING,FX7Segment::onCmdGetTip),
  };


FXIMPLEMENT(FX7Segment,FXFrame,FX7SegmentMap,ARRAYNUMBER(FX7SegmentMap))


// For serialization
FX7Segment::FX7Segment(){
  flags|=FLAG_ENABLED;
  textColor=0;
  thickness=3;
  cellwidth=12;
  cellheight=18;
  }


// Construct 7 segment display
FX7Segment::FX7Segment(FXComposite* p,const FXString& text,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb),label(text){
  flags|=FLAG_ENABLED;
  textColor=getApp()->getForeColor();
  thickness=3;
  cellwidth=12;
  cellheight=18;
  }


// Get default width
FXint FX7Segment::getDefaultWidth(){
  register FXint w=(cellwidth+2)*label.length();
  if(label.length()) w-=2;
  return padleft+padright+(border<<1)+w;
  }


// Get default height
FXint FX7Segment::getDefaultHeight(){
  return padtop+padbottom+(border<<1)+cellheight;
  }


// Update value from a message
long FX7Segment::onCmdSetValue(FXObject*,FXSelector,void *ptr){
  setText((const FXchar*)ptr);
  return 1;
  }


// Get value as int
long FX7Segment::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=FXIntVal(label);
  return 1;
  }


// Set value from int
long FX7Segment::onCmdSetIntValue(FXObject*,FXSelector,void *ptr){
  setText(FXStringVal(*((FXint*)ptr)));
  return 1;
  }


// Get value as double
long FX7Segment::onCmdGetRealValue(FXObject*,FXSelector,void* ptr){
  *((FXdouble*)ptr)=FXDoubleVal(label);
  return 1;
  }


// Set value from double
long FX7Segment::onCmdSetRealValue(FXObject*,FXSelector,void* ptr){
  setText(FXStringVal(*((FXdouble*)ptr)));
  return 1;
  }


// Get value as string
long FX7Segment::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=label;
  return 1;
  }


// Set value from string
long FX7Segment::onCmdSetStringValue(FXObject*,FXSelector,void *ptr){
  setText(*((FXString*)ptr));
  return 1;
  }


// Set help using a message
long FX7Segment::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FX7Segment::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FX7Segment::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FX7Segment::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FX7Segment::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FX7Segment::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// draw/redraw object
long FX7Segment::onPaint(FXObject*,FXSelector,void *ptr){
  register FXEvent *event=(FXEvent*)ptr;
  register FXint tx,ty,tw,ch,cw;
  FXDCWindow dc(this,event);

  // Draw frame
  drawFrame(dc,0,0,width,height);

  // Draw background
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));

  // Non empty
  if(label.length()){

    cw=cellwidth;
    ch=cellheight;

    tw=label.length()*(cw+2);
    if(label.length()) tw-=2;

    // Justify in x
    if((options&JUSTIFY_LEFT) && (options&JUSTIFY_RIGHT)){      // FIXME not good yet
      tx=border+padleft;
      tw=width-padleft-padright-(border<<1);
      cw=tw/label.length();
      }
    else if(options&JUSTIFY_LEFT){
      tx=border+padleft;
      }
    else if(options&JUSTIFY_RIGHT){
      tx=width-padright-border-tw;
      }
    else{
      tx=border+padleft+(width-padleft-padright-(border<<1)-tw)/2;
      }

    // Justify in y
    if((options&JUSTIFY_TOP) && (options&JUSTIFY_BOTTOM)){
      ty=border+padtop;
      ch=height-padbottom-padtop-(border<<1);
      }
    else if(options&JUSTIFY_TOP){
      ty=border+padtop;
      }
    else if(options&JUSTIFY_BOTTOM){
      ty=height-padbottom-border-ch;
      }
    else{
      ty=border+padtop+(height-padbottom-padtop-(border<<1)-ch)/2;
      }

    // Draw cells with shadow
    if(options&SEVENSEGMENT_SHADOW){
      dc.setForeground(shadowColor);
      drawCells(dc,tx+1,ty+1,cw,ch);
      }

    // Draw cells normally
    dc.setForeground(textColor);
    drawCells(dc,tx,ty,cw,ch);
    }
  return 1;
  }


// Draw cells
void FX7Segment::drawCells(FXDCWindow &dc,FXint x,FXint y,FXint cw,FXint ch){
  register FXint c,t;
  for(c=0; c<label.length(); c++){
    t=(FXuchar)label[c];
    if(' '<=t && t<127){
      drawSegments(dc,x+c*(cellwidth+2),y,cw,ch,segm[t-' ']);
      }
    }
  }


// Draw segments
void FX7Segment::drawSegments(FXDCWindow &dc,FXint x,FXint y,FXint w,FXint h,FXuint segments){
  FXPoint points[6];
  if(segments&0x02){            // Upper left
    points[0].x=x;
    points[0].y=y;
    points[1].x=x+thickness;
    points[1].y=y+thickness;
    points[2].x=x+thickness;
    points[2].y=y+(h>>1)-(thickness>>1)-1;
    points[3].x=x;
    points[3].y=y+(h>>1);
    dc.fillPolygon(points,4);
    }
  if(segments&0x04){            // Upper right
    points[0].x=x+w;
    points[0].y=y;
    points[1].x=x+w;
    points[1].y=y+(h>>1);
    points[2].x=x+w-thickness;
    points[2].y=y+(h>>1)-(thickness>>1)-1;
    points[3].x=x+w-thickness;
    points[3].y=y+thickness;
    dc.fillPolygon(points,4);
    }
  if(segments&0x10){            // Lower left
    points[0].x=x;
    points[0].y=y+(h>>1);
    points[1].x=x+thickness;
    points[1].y=y+(h>>1)-(thickness>>1)+thickness;
    points[2].x=x+thickness;
    points[2].y=y+h-thickness-1;
    points[3].x=x;
    points[3].y=y+h-1;
    dc.fillPolygon(points,4);
    }
  if(segments&0x20){            // Lower right
    points[0].x=x+w;
    points[0].y=y+(h>>1);
    points[1].x=x+w;
    points[1].y=y+h-1;
    points[2].x=x+w-thickness;
    points[2].y=y+h-thickness-1;
    points[3].x=x+w-thickness;
    points[3].y=y+(h>>1)-(thickness>>1)+thickness;
    dc.fillPolygon(points,4);
    }
  if(segments&0x01){            // Top
    points[0].x=x+1;
    points[0].y=y;
    points[1].x=x+w-1;
    points[1].y=y;
    points[2].x=x+w-thickness-1;
    points[2].y=y+thickness;
    points[3].x=x+thickness+1;
    points[3].y=y+thickness;
    dc.fillPolygon(points,4);
    }
  if(segments&0x40){            // Bottom
    points[0].x=x;
    points[0].y=y+h;
    points[1].x=x+w;
    points[1].y=y+h;
    points[2].x=x+w-thickness;
    points[2].y=y+h-thickness;
    points[3].x=x+thickness;
    points[3].y=y+h-thickness;
    dc.fillPolygon(points,4);
    }
  if(segments&0x08){            // Middle
    points[0].x=x+1;
    points[0].y=y+(h>>1);
    points[1].x=x+thickness;
    points[1].y=y+(h>>1)-(thickness>>1);
    points[2].x=x+w-thickness;
    points[2].y=y+(h>>1)-(thickness>>1);
    points[3].x=x+w-1;
    points[3].y=y+(h>>1);
    points[4].x=x+w-thickness-2;
    points[4].y=y+(h>>1)-(thickness>>1)+thickness;
    points[5].x=x+thickness+1;
    points[5].y=y+(h>>1)-(thickness>>1)+thickness;
    dc.fillPolygon(points,6);
    }
  if(segments&128){             // Decimal
    }
  if(segments&256){             // Colon
    dc.fillRectangle(x+(w>>1)-(thickness>>1)-1,y+(h>>1)-(thickness>>1)-thickness-1,thickness,thickness);
    dc.fillRectangle(x+(w>>1)-(thickness>>1)-1,y+(h>>1)-(thickness>>1)+thickness+1,thickness,thickness);
    }
  }


// Change text
void FX7Segment::setText(const FXString& text){
  if(label!=text){
    if(label.length()!=text.length()) recalc();
    label=text;
    update();
    }
  }


// Set text color
void FX7Segment::setTextColor(FXColor clr){
  if(textColor!=clr){
    textColor=clr;
    update();
    }
  }


// Get/set cell width
void FX7Segment::setCellWidth(FXint w){
  if(cellwidth!=w){
    cellwidth=w;
    recalc();
    update();
    }
  }


// Get/set cell height
void FX7Segment::setCellHeight(FXint h){
  if(cellheight!=h){
    cellheight=h;
    recalc();
    update();
    }
  }


// set segment thickness
void FX7Segment::setThickness(FXint t){
  if(t<1) t=1;
  if(!(t&1)) t|=1;
  if(thickness!=t){
    thickness=t;
    recalc();
    update();
    }
  }


// Change 7 segment style
void FX7Segment::set7SegmentStyle(FXuint style){
  FXuint opts=(options&~SEVENSEGMENT_MASK) | (style&SEVENSEGMENT_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get 7 segment
FXuint FX7Segment::get7SegmentStyle() const {
  return (options&SEVENSEGMENT_MASK);
  }


// Set text justify style
void FX7Segment::setJustify(FXuint style){
  FXuint opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
  if(options!=opts){
    options=opts;
    update();
    }
  }


// Get text justify style
FXuint FX7Segment::getJustify() const {
  return (options&JUSTIFY_MASK);
  }


// Save object to stream
void FX7Segment::save(FXStream &store) const {
  FXFrame::save(store);
  store << label;
  store << textColor;
  store << thickness;
  store << cellwidth;
  store << cellheight;
  store << tip;
  store << help;
  }


// Load object from stream
void FX7Segment::load(FXStream &store) {
  FXFrame::load(store);
  store >> label;
  store >> textColor;
  store >> thickness;
  store >> cellwidth;
  store >> cellheight;
  store >> tip;
  store >> help;
  }

}
