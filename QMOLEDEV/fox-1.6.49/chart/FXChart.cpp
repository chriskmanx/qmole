/********************************************************************************
*                                                                               *
*                        C h a r t   B a s e   W i d g e t                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXChart.cpp,v 1.19 2006/01/22 18:01:13 fox Exp $                         *
********************************************************************************/
#include "fx.h"
#include "FXPNGImage.h"
#include "FXJPGImage.h"
#include "FXTIFImage.h"
#include "FXChart.h"

/*
  Notes:
*/


using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXChart) FXChartMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXChart::onPaint),
  FXMAPFUNC(SEL_CLIPBOARD_LOST,0,FXChart::onClipboardLost),
  FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,FXChart::onClipboardRequest),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXChart::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXChart::onQueryHelp),
  };


// Object implementation
FXIMPLEMENT(FXChart,FXComposite,FXChartMap,ARRAYNUMBER(FXChartMap))


/*******************************************************************************/

// Drag types
FXDragType FXChart::bmpType=0;
FXDragType FXChart::gifType=0;
FXDragType FXChart::jpgType=0;
FXDragType FXChart::pngType=0;
FXDragType FXChart::csvType=0;
FXDragType FXChart::tifType=0;


/*******************************************************************************/

// Init
FXChart::FXChart(){
  flags|=FLAG_SHOWN|FLAG_ENABLED|FLAG_DROPTARGET;
  }


// Make a chart
FXChart::FXChart(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXComposite(p,opts,x,y,w,h){
  chart=new FXImage(getApp(),NULL,IMAGE_DITHER|IMAGE_SHMI|IMAGE_SHMP,w,h);
  flags|=FLAG_SHOWN|FLAG_ENABLED|FLAG_DROPTARGET;
  target=tgt;
  message=sel;
  fill.style=FILLSTYLE_SOLID;
  fill.hatch=STIPPLE_NONE;
  fill.image=NULL;
  fill.color=FXRGB(103,103,255);
  fill.backcolor=0;
  fill.lower=FXRGB(255,255,255);
  fill.upper=FXRGB(0,0,255);
  }


// Create window; register drag types
void FXChart::create(){
  FXComposite::create();
  chart->create();
  if(fill.image) fill.image->create();
  if(!colorType) colorType=getApp()->registerDragType(colorTypeName);
  if(!textType) textType=getApp()->registerDragType(textTypeName);
  if(!bmpType) bmpType=getApp()->registerDragType(FXBMPImage::mimeType);
  if(!gifType) gifType=getApp()->registerDragType(FXGIFImage::mimeType);
  if(!jpgType) jpgType=getApp()->registerDragType(FXJPGImage::mimeType);
  if(!pngType) pngType=getApp()->registerDragType(FXPNGImage::mimeType);
  if(!tifType) tifType=getApp()->registerDragType(FXTIFImage::mimeType);
  if(!csvType) csvType=getApp()->registerDragType("Csv");
  }


// Detach window; zero out drag types
void FXChart::detach(){
  FXComposite::detach();
  chart->detach();
  textType=0;
  colorType=0;
  bmpType=0;
  gifType=0;
  jpgType=0;
  pngType=0;
  csvType=0;
  tifType=0;
  }


// Marker size is in pixels; x,y are canvas coordinates of the center
void FXChart::drawMarker(FXDC& dc,FXint x,FXint y,const Marker& m) const {
  register FXint s=m.size;
  register FXint h=s>>1;
  FXPoint p[5];
  dc.setForeground(m.color);
  switch(m.style){
    case MARKER_SQUARE:
      dc.drawRectangle(x-h,y-h,s,s);
      break;
    case MARKER_SQUARE|MARKER_SOLID:
      dc.fillRectangle(x-h,y-h,s,s);
      break;
    case MARKER_CIRCLE:
      dc.drawArc(x-h,y-h,s,s,0,23040);
      break;
    case MARKER_CIRCLE|MARKER_SOLID:
      dc.fillArc(x-h,y-h,s,s,0,23040);
      break;
    case MARKER_DIAMOND:
      p[0].x=p[2].x=p[4].x=x;
      p[1].x=x-h;
      p[3].x=p[1].x+s;
      p[0].y=p[4].y=y-h;
      p[1].y=p[3].y=y;
      p[2].y=p[0].y+s;
      dc.drawLines(p,5);
      break;
    case MARKER_DIAMOND|MARKER_SOLID:
      p[0].x=p[2].x=x;
      p[1].x=x-h;
      p[3].x=p[1].x+s;
      p[0].y=y-h;
      p[1].y=p[3].y=y;
      p[2].y=p[0].y+s;
      dc.fillPolygon(p,4);
      break;
    case MARKER_TRIANGLE_UP:
      p[0].x=p[3].x=x;
      p[1].x=x-h;
      p[2].x=x+h;
      p[0].y=p[3].y=y-h;
      p[1].y=p[2].y=y+h;
      dc.drawLines(p,4);
      break;
    case MARKER_TRIANGLE_UP|MARKER_SOLID:
      p[0].x=x;
      p[1].x=x-h;
      p[2].x=x+h;
      p[0].y=y-h;
      p[1].y=p[2].y=y+h;
      dc.fillPolygon(p,3);
      break;
    case MARKER_TRIANGLE_DN:
      p[0].x=p[3].x=x-h;
      p[1].x=x;
      p[2].x=x+h;
      p[0].y=p[2].y=p[3].y=y-h;
      p[1].y=y+h;
      dc.drawLines(p,4);
      break;
    case MARKER_TRIANGLE_DN|MARKER_SOLID:
      p[0].x=x-h;
      p[1].x=x;
      p[2].x=x+h;
      p[0].y=p[2].y=y-h;
      p[1].y=y+h;
      dc.fillPolygon(p,3);
      break;
    case MARKER_TRIANGLE_LT:
      p[0].x=p[3].x=x-h;
      p[1].x=p[2].x=x+h;
      p[0].y=p[3].y=y;
      p[1].y=y+h;
      p[2].y=y-h;
      dc.drawLines(p,4);
      break;
    case MARKER_TRIANGLE_LT|MARKER_SOLID:
      p[0].x=x-h;
      p[1].x=p[2].x=x+h;
      p[0].y=y;
      p[1].y=y+h;
      p[2].y=y-h;
      dc.fillPolygon(p,3);
      break;
    case MARKER_TRIANGLE_RT:
      p[0].x=p[3].x=x+h;
      p[1].x=p[2].x=x-h;
      p[0].y=p[3].y=y;
      p[1].y=y-h;
      p[2].y=y+h;
      dc.drawLines(p,4);
      break;
    case MARKER_TRIANGLE_RT|MARKER_SOLID:
      p[0].x=x+h;
      p[1].x=p[2].x=x-h;
      p[0].y=y;
      p[1].y=y-h;
      p[2].y=y+h;
      dc.fillPolygon(p,3);
      break;
    }
  }


#define MAXSTEPS 128

// Resize the dial
void FXChart::layout(){
  register FXint rr,gg,bb,dr,dg,db,r1,g1,b1,r2,g2,b2,xl,xr,n,t,xx,dx;

  // Do regular layout of child widgets
  FXComposite::layout();

  // Resize off-screen buffer if needed
  if(chart->getWidth()!=width || chart->getHeight()!=height){
    chart->resize(width,height);
    FXTRACE((1,"new size = %d x %d\n",width,height));
    }

  // FIXME regenerate plot
  FXDCWindow dc(chart);
  switch(fill.style){
    case FILLSTYLE_SOLID:
      dc.setStipple(STIPPLE_NONE);
      dc.setFillStyle(FILL_SOLID);
      dc.setForeground(fill.color);
      dc.fillRectangle(0,0,width,height);
      break;
    case FILLSTYLE_HATCH:
      if(fill.backcolor){
        dc.setFillStyle(FILL_OPAQUESTIPPLED);
        dc.setBackground(fill.backcolor);
        }
      else{
        dc.setFillStyle(FILL_STIPPLED);
        }
      dc.setStipple((FXStipplePattern)fill.hatch);
      dc.setForeground(fill.color);
      dc.fillRectangle(0,0,width,height);
      break;
    case FILLSTYLE_TEXTURE:
      dc.setStipple(STIPPLE_NONE);
      dc.setFillStyle(FILL_TILED);
      dc.setTile(fill.image);
      dc.fillRectangle(0,0,width,height);
      break;
    case FILLSTYLE_IMAGE:
      dc.setStipple(STIPPLE_NONE);
      dc.setFillStyle(FILL_TILED);
      dc.setTile(fill.image);
      dc.fillRectangle(0,0,width,height);
      break;
    case FILLSTYLE_HORIZONTAL:
      dc.setStipple(STIPPLE_NONE);
      dc.setFillStyle(FILL_SOLID);
dc.setForeground(FXRGB(255,0,255));
dc.fillRectangle(0,0,width,height);

      r1=FXREDVAL(fill.lower);   r2=FXREDVAL(fill.upper);   dr=r2-r1;
      g1=FXGREENVAL(fill.lower); g2=FXGREENVAL(fill.upper); dg=g2-g1;
      b1=FXBLUEVAL(fill.lower);  b2=FXBLUEVAL(fill.upper);  db=b2-b1;

      n=FXABS(dr);
      if((t=FXABS(dg))>n) n=t;
      if((t=FXABS(db))>n) n=t;
FXTRACE((1,"max(|dr|,|dg|,|db|)=%d \n",n));
      n++;
      if(n>width) n=width;
      if(n>MAXSTEPS) n=MAXSTEPS;
FXTRACE((1,"n=%d \n",n));

      rr=(r1<<16)+32767;
      gg=(g1<<16)+32767;
      bb=(b1<<16)+32767;
      xx=32767;

      dr=(dr<<16)/n;
      dg=(dg<<16)/n;
      db=(db<<16)/n;
      dx=(width<<16)/n;

      do{
        xl=xx>>16;
        xx+=dx;
        xr=xx>>16;
        dc.setForeground(FXRGB(rr>>16,gg>>16,bb>>16));
        dc.fillRectangle(xl,0,xr-xl,height);
FXTRACE((1,"fillRectangle(%d,%d,%d,%d) width=%d n=%d\n",xl,0,xr-xl,height,width,n));
        rr+=dr;
        gg+=dg;
        bb+=db;
        }
      while(xr<width);

      break;
    case FILLSTYLE_VERTICAL:
      break;
    case FILLSTYLE_DIAGONAL:
      break;
    case FILLSTYLE_RDIAGONAL:
      break;
    }

  flags&=~FLAG_DIRTY;
  }


// We were asked about tip text
long FXChart::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXChart::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Lost clipboard, so destroy data
long FXChart::onClipboardLost(FXObject*,FXSelector,void*){
  return 1;
  }


// Request for clipboard data
long FXChart::onClipboardRequest(FXObject *sender,FXSelector sel,void *ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXuval size;
  FXuchar *data;

  // Try handling it in base class first
  if(FXComposite::onClipboardRequest(sender,sel,ptr)) return 1;

  // One of the supported image types?
  if(event->target==bmpType || event->target==gifType || event->target==jpgType || event->target==pngType){
    FXMemoryStream ms;

    // Read back pixels
    chart->restore();

    // Open memory stream
    ms.open(FXStreamSave,NULL);

    // Render image to memory stream
    if(event->target==bmpType)
      fxsaveBMP(ms,chart->getData(),chart->getWidth(),chart->getHeight());
    else if(event->target==gifType)
      fxsaveGIF(ms,chart->getData(),chart->getWidth(),chart->getHeight());
    else if(event->target==jpgType)
      fxsaveJPG(ms,chart->getData(),chart->getWidth(),chart->getHeight(),75);
    else if(event->target==pngType)
      fxsavePNG(ms,chart->getData(),chart->getWidth(),chart->getHeight());
    else if(event->target==tifType)
      fxsaveTIF(ms,chart->getData(),chart->getWidth(),chart->getHeight(),0);
#ifdef WIN32
//  else if(event->target==imageType)
//    fxsaveBMP(ms,chart->getData(),chart->getWidth(),chart->getHeight());
#endif

    // Grab buffered image
    ms.takeBuffer(data,size);

    // Close memory stream
    ms.close();

    // Release pixels
    chart->release();

    // Set DND data
    setDNDData(FROM_CLIPBOARD,event->target,data,size);
    return 1;
    }

  return 0;
  }


// Handle repaint
long FXChart::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  dc.drawImage(chart,0,0);
  return 1;
  }


// Set fill style
void FXChart::setFillStyle(const FillStyle& fs){
  fill=fs;
  recalc();
  }


// Change help text
void FXChart::setHelpText(const FXString& text){
  help=text;
  }


// Change tip text
void FXChart::setTipText(const FXString& text){
  tip=text;
  }


// Save data
void FXChart::save(FXStream& store) const {
  FXComposite::save(store);
  store << chart;
  store << tip;
  store << help;
  }


// Load data
void FXChart::load(FXStream& store){
  FXComposite::load(store);
  store >> chart;
  store >> tip;
  store >> help;
  }


// Destroy
FXChart::~FXChart(){
  delete chart;
  chart=(FXImage*)-1L;
  }

}
