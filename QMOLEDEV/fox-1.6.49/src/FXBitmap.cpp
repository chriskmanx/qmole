/********************************************************************************
*                                                                               *
*                             B i t m a p    O b j e c t                        *
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
* $Id: FXBitmap.cpp,v 1.89.2.1 2006/08/01 18:04:26 fox Exp $                        *
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
#include "FXVisual.h"
#include "FXBitmap.h"
#include "FXVisual.h"
#include "FXDCWindow.h"
#include "FXException.h"


/*
  Note:
  - Try eliminate temp copy:- slap pixels into XImage directly, if possible...
  - Perhaps enforce system-native padding necessary for the above.
  - Our bitmap data is 01234567, i.e. LS-BIT first; byte order ditto.
  - Issue: should FXBitmap return the DC for drawing onto the X-Server resident
    pixmap, or the client-side image bits?

    My idea is it should be the latter:

      - Allows even richer set of drawing primitives, as everything is
        drawn in software.
      - Very useful to generate off-screen renderings, e.g. during printing.
      - Allows for building and running true-color drawing programs on
        low-end graphics hardware.
      - The only drawback I can see is it will be a fairly large implementation
        effort...

  - The scale, mirror, rotate and crop API's have been contributed by Marc Cartright,
    <macartright@hotmail.com>.
  - The XBM format maps 1 to black instead of white; perhaps this should
    be reversed.
*/

#define DISPLAY(app) ((Display*)((app)->display))

// Changable bitmap options
#define BITMAP_MASK   (BITMAP_KEEP|BITMAP_SHMI|BITMAP_SHMP)


using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXBitmap,FXDrawable,NULL,0)


// For deserialization
FXBitmap::FXBitmap(){
  data=NULL;
  bytewidth=0;
  options=0;
  }


// Initialize
FXBitmap::FXBitmap(FXApp* a,const void *pix,FXuint opts,FXint w,FXint h):FXDrawable(a,w,h){
  FXTRACE((100,"FXBitmap::FXBitmap %p\n",this));
  FXASSERT((opts&~(BITMAP_OWNED|BITMAP_MASK))==0);
  visual=getApp()->getMonoVisual();
  data=(FXuchar*)pix;
  bytewidth=(width+7)>>3;
  options=opts;
  if(!data && (options&BITMAP_OWNED)){
    if(!FXCALLOC(&data,FXuchar,height*bytewidth)){ throw FXMemoryException("unable to construct bitmap"); }
    }
  }


// Create bitmap
void FXBitmap::create(){
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));

#ifndef WIN32

      // Initialize visual
      visual->create();

      // Make pixmap
      xid=XCreatePixmap(DISPLAY(getApp()),XDefaultRootWindow(DISPLAY(getApp())),FXMAX(width,1),FXMAX(height,1),1);

#else

      // Initialize visual
      visual->create();

      // Create uninitialized shape bitmap
      xid=CreateBitmap(FXMAX(width,1),FXMAX(height,1),1,1,NULL);

#endif

      // Were we successful?
      if(!xid){ throw FXImageException("unable to create bitmap"); }

      // Render pixels
      render();

      // If we're not keeping the pixel buffer, release it
      if(!(options&BITMAP_KEEP)) release();
      }
    }
  }


// Release the client-side buffer, free it if it was owned.
void FXBitmap::release(){
  if(options&BITMAP_OWNED){
    options&=~BITMAP_OWNED;
    FXFREE(&data);
    }
  data=NULL;
  }


// Detach bitmap
void FXBitmap::detach(){
  visual->detach();
  if(xid){
    FXTRACE((100,"%s::detach %p\n",getClassName(),this));
    xid=0;
    }
  }


// Destroy bitmap
void FXBitmap::destroy(){
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::destroy %p\n",getClassName(),this));
#ifndef WIN32

      // Delete pixmap
      XFreePixmap(DISPLAY(getApp()),xid);
#else

      // Delete bitmap
      DeleteObject(xid);
#endif
      }
    xid=0;
    }
  }


#ifndef WIN32

// Find shift amount
static inline FXuint findshift(unsigned long mask){
  register FXuint sh=0;
  while(!(mask&(1<<sh))) sh++;
  return sh;
  }


// Find low bit in mask
static inline FXPixel lowbit(FXPixel mask){
  return (~mask+1)&mask;
  }


// Restore client-side pixel buffer from bitmap
void FXBitmap::restore(){
  if(xid){
    register XImage *xim=NULL;
    register FXint size,x,y;

    FXTRACE((100,"%s::restore bitmap %p\n",getClassName(),this));

    // Check for legal size
    if(width<1 || height<1){ fxerror("%s::restore: illegal bitmap size %dx%d.\n",getClassName(),width,height); }

    // Make array for data if needed
    if(!data){
      size=bytewidth*height;
      if(!FXCALLOC(&data,FXuchar,size)){ throw FXMemoryException("unable to restore bitmap"); }
      options|=BITMAP_OWNED;
      }

    // Got local buffer to receive into
    if(data){
      xim=XGetImage(DISPLAY(getApp()),xid,0,0,width,height,1,XYPixmap);
      if(!xim){ throw FXImageException("unable to restore image"); }

      // Should have succeeded
      FXASSERT(xim);

      FXTRACE((150,"bm width = %d\n",xim->width));
      FXTRACE((150,"bm height = %d\n",xim->height));
      FXTRACE((150,"bm format = %s\n",xim->format==XYBitmap?"XYBitmap":xim->format==XYPixmap?"XYPixmap":"ZPixmap"));
      FXTRACE((150,"bm byte_order = %s\n",(xim->byte_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"bm bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"bm bitmap_bit_order = %s\n",(xim->bitmap_bit_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"bm bitmap_pad = %d\n",xim->bitmap_pad));
      FXTRACE((150,"bm bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"bm depth = %d\n",xim->depth));
      FXTRACE((150,"bm bytes_per_line = %d\n",xim->bytes_per_line));
      FXTRACE((150,"bm bits_per_pixel = %d\n",xim->bits_per_pixel));

      // Grab pixels from image
      for(y=0; y<height; y++){
        for(x=0; x<width; x++){
          if(XGetPixel(xim,x,y)) data[y*bytewidth+(x>>3)]|=1<<(x&7);
          }
        }

      // Destroy image
      XDestroyImage(xim);
      }
    }
  }


// Render into pixmap
void FXBitmap::render(){
  if(xid){
    register XImage *xim=NULL;
    register Visual *vis;
    register int size;
    register FXuchar *pix;
    register int i;
    XGCValues values;
    GC gc;

    FXTRACE((100,"%s::render bitmap %p\n",getClassName(),this));

    // Fill with pixels if there is data
    if(data && 0<width && 0<height){

      // Make GC
      values.foreground=0xffffffff;
      values.background=0;
      gc=XCreateGC(DISPLAY(getApp()),xid,GCForeground|GCBackground,&values);

      // Get Visual
      vis=(Visual*)visual->visual;

      xim=XCreateImage(DISPLAY(getApp()),vis,1,XYBitmap,0,NULL,width,height,8,(width+7)>>3);
      if(!xim){ throw FXImageException("unable to render bitmap"); }

      // Try create temp pixel store
      if(!FXMALLOC(&xim->data,char,xim->bytes_per_line*height)){ throw FXMemoryException("unable to render bitmap"); }

      FXTRACE((150,"bm width = %d\n",xim->width));
      FXTRACE((150,"bm height = %d\n",xim->height));
      FXTRACE((150,"bm format = %s\n",xim->format==XYBitmap?"XYBitmap":xim->format==XYPixmap?"XYPixmap":"ZPixmap"));
      FXTRACE((150,"bm byte_order = %s\n",(xim->byte_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"bm bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"bm bitmap_bit_order = %s\n",(xim->bitmap_bit_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"bm bitmap_pad = %d\n",xim->bitmap_pad));
      FXTRACE((150,"bm bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"bm depth = %d\n",xim->depth));
      FXTRACE((150,"bm bytes_per_line = %d\n",xim->bytes_per_line));
      FXTRACE((150,"bm bits_per_pixel = %d\n",xim->bits_per_pixel));

      // Render bits into server-formatted bitmap
      size=xim->bytes_per_line*height;
      pix=(FXuchar*)xim->data;

      // Most significant bit first
      if(xim->bitmap_bit_order==MSBFirst){
        for(i=0; i<size; i++) pix[i]=FXBITREVERSE(data[i]);
        }

      // Least significant bit first
      else{
        memcpy(pix,data,size);
        }

      // Blast the image
      XPutImage(DISPLAY(getApp()),xid,gc,xim,0,0,0,0,width,height);
      FXFREE(&xim->data);
      XDestroyImage(xim);
      XFreeGC(DISPLAY(getApp()),gc);
      }
    }
  }


#else


struct BITMAPINFO256 {
  BITMAPINFOHEADER bmiHeader;
  RGBQUAD          bmiColors[256];
  };


// Restore client-side pixel buffer from bitmap
void FXBitmap::restore(){
  if(xid){
    register FXint x,y,bytes_per_line;
    register FXuchar *p,*q;
    FXuchar *pixels;

    FXTRACE((100,"%s::restore image %p\n",getClassName(),this));

    // Check for legal size
    if(width<1 || height<1){ fxerror("%s::restore: illegal image size %dx%d.\n",getClassName(),width,height); }

    // Make array for data if needed
    if(!data){
      if(!FXCALLOC(&data,FXuchar,height*bytewidth)){ throw FXMemoryException("unable to restore image"); }
      options|=BITMAP_OWNED;
      }

    // Got local buffer to receive into
    if(data){

      // Bytes per line, rounded to nearest DWORD
      bytes_per_line=((width+31)&~31)>>3;

      // Set up the bitmap info
      BITMAPINFO256 bmi;
      bmi.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
      bmi.bmiHeader.biWidth=width;
      bmi.bmiHeader.biHeight=-height;   // Negative heights means upside down!
      bmi.bmiHeader.biPlanes=1;
      bmi.bmiHeader.biBitCount=1;
      bmi.bmiHeader.biCompression=BI_RGB;
      bmi.bmiHeader.biSizeImage=0;
      bmi.bmiHeader.biXPelsPerMeter=0;
      bmi.bmiHeader.biYPelsPerMeter=0;
      bmi.bmiHeader.biClrUsed=0;
      bmi.bmiHeader.biClrImportant=0;
      bmi.bmiColors[0].rgbBlue=0;
      bmi.bmiColors[0].rgbGreen=0;
      bmi.bmiColors[0].rgbRed=0;
      bmi.bmiColors[0].rgbReserved=0;
      bmi.bmiColors[1].rgbBlue=255;
      bmi.bmiColors[1].rgbGreen=255;
      bmi.bmiColors[1].rgbRed=255;
      bmi.bmiColors[1].rgbReserved=0;

      // DIB format pads to multiples of 4 bytes...
      if(!FXMALLOC(&pixels,FXuchar,height*bytes_per_line)){ throw FXImageException("unable to restore image"); }

      // Make device context
      HDC hdcmem=::CreateCompatibleDC(NULL);
      if(!GetDIBits(hdcmem,(HBITMAP)xid,0,height,pixels,(BITMAPINFO*)&bmi,DIB_RGB_COLORS)){
        throw FXImageException("unable to restore image");
        }

      // Fill our own data from pixels
      for(y=0,p=pixels,q=data; y<height; y++){
        for(x=0; x<bytewidth; x++){
          q[x]=~FXBITREVERSE(p[x]);
          }
        q+=bytewidth;
        p+=bytes_per_line;
        }

      // Clean up
      ::DeleteDC(hdcmem);
      FXFREE(&pixels);
      }
    }
  }


// Render into pixmap
void FXBitmap::render(){
  if(xid){
    register FXint x,y,bytes_per_line;
    register FXuchar *p,*q;
    FXuchar *pixels;

    FXTRACE((100,"%s::render bitmap %p\n",getClassName(),this));

    // Fill with pixels if there is data
    if(data && 0<width && 0<height){

      // Bytes per line, rounded to nearest DWORD
      bytes_per_line=((width+31)&~31)>>3;

      // Set up the bitmap info
      BITMAPINFO256 bmi;
      bmi.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
      bmi.bmiHeader.biWidth=width;
      bmi.bmiHeader.biHeight=-height;   // Negative heights means upside down!
      bmi.bmiHeader.biPlanes=1;
      bmi.bmiHeader.biBitCount=1;
      bmi.bmiHeader.biCompression=0;
      bmi.bmiHeader.biSizeImage=0;
      bmi.bmiHeader.biXPelsPerMeter=0;
      bmi.bmiHeader.biYPelsPerMeter=0;
      bmi.bmiHeader.biClrUsed=0;
      bmi.bmiHeader.biClrImportant=0;
      bmi.bmiColors[0].rgbBlue=0;
      bmi.bmiColors[0].rgbGreen=0;
      bmi.bmiColors[0].rgbRed=0;
      bmi.bmiColors[0].rgbReserved=0;
      bmi.bmiColors[1].rgbBlue=255;
      bmi.bmiColors[1].rgbGreen=255;
      bmi.bmiColors[1].rgbRed=255;
      bmi.bmiColors[1].rgbReserved=0;

      // Fill temp array
      if(!FXCALLOC(&pixels,FXuchar,height*bytes_per_line)){ throw FXMemoryException("unable to render bitmap"); }

      // Fill pixels from our own data
      for(y=0,p=pixels,q=data; y<height; y++){
        for(x=0; x<bytewidth; x++){
          p[x]=~FXBITREVERSE(q[x]);
          }
        q+=bytewidth;
        p+=bytes_per_line;
        }

      // Get memory device context
      HDC hdcmem=::CreateCompatibleDC(NULL);
      if(!SetDIBits(hdcmem,(HBITMAP)xid,0,height,pixels,(BITMAPINFO*)&bmi,DIB_RGB_COLORS)){
        throw FXImageException("unable to render bitmap");
        }

      // Push to GDI
      ::GdiFlush();

      // Clean up
      ::DeleteDC(hdcmem);
      FXFREE(&pixels);
      }
    }
  }


#endif


// Resize bitmap to the specified width and height; the contents become undefined
void FXBitmap::resize(FXint w,FXint h){
  register FXint bw;
  if(w<1) w=1;
  if(h<1) h=1;
  FXTRACE((100,"%s::resize(%d,%d)\n",getClassName(),w,h));
  bw=(w+7)>>3;
  if(xid){

#ifndef WIN32

    // Free old pixmap
    XFreePixmap(DISPLAY(getApp()),xid);

    // Make new pixmap
    xid=XCreatePixmap(DISPLAY(getApp()),XDefaultRootWindow(DISPLAY(getApp())),w,h,1);
    if(!xid){ throw FXImageException("unable to resize bitmap"); }

#else

    // Delete old bitmap
    DeleteObject(xid);

    // Create a bitmap compatible with current display
    xid=CreateBitmap(w,h,1,1,NULL);
    if(!xid){ throw FXImageException("unable to resize bitmap"); }

#endif
    }

  // Resize data array; only do the work if the new
  // array is a different size as measured in bytes!
  if(data){
    if(!(options&BITMAP_OWNED)){        // Need to own array
      if(!FXMALLOC(&data,FXColor,h*bw)){ throw FXMemoryException("unable to resize bitmap"); }
      options|=BITMAP_OWNED;
      }
    else if(h*bw!=height*bytewidth){
      if(!FXRESIZE(&data,FXColor,h*bw)){ throw FXMemoryException("unable to resize bitmap"); }
      }
    }

  // Remember new size
  bytewidth=bw;
  width=w;
  height=h;
  }


// Fill bitmap with uniform value
void FXBitmap::fill(FXbool color){
  if(data){
    memset(data,-color,height*bytewidth);
    }
  }


// Rescale pixels to the specified width and height; just nearest
// neighbor; there ain't no such thing as anti-aliasing in bitmaps!
void FXBitmap::scale(FXint w,FXint h){
  if(w<1) w=1;
  if(h<1) h=1;
  FXTRACE((100,"%s::scale(%d,%d)\n",getClassName(),w,h));
  if(w!=width || h!=height){
    if(data){
      register FXuchar *q,*p,bits;
      register FXint xs=(width<<16)/w;
      register FXint ys=(height<<16)/h;
      register FXint bw=bytewidth;
      register FXint i,j,x,y,xx;
      FXuchar *interim;

      // Copy to old buffer
      if(!FXMEMDUP(&interim,data,FXuchar,height*bytewidth)){ throw FXMemoryException("unable to scale bitmap"); }

      // Resize the pixmap and target buffer
      resize(w,h);

      // Scale the bitmap
      i=0;
      y=ys>>1;
      p=data;
      do{
        j=0;
        x=xs>>1;
        q=interim+(y>>16)*bw;
        bits=0;
        do{
          xx=x>>16;
          bits|=((q[xx>>3]>>(xx&7))&1)<<(j&7);
          if((j&7)==7){ *p++=bits; bits=0; }
          x+=xs;
          }
        while(++j<w);
        if(j&7){ *p++=bits; }
        y+=ys;
        }
      while(++i<h);

      // Free interim buffer
      FXFREE(&interim);
      render();
      }
    else{
      resize(w,h);
      }
    }
  }



// Mirror bitmap horizontally and/or vertically
void FXBitmap::mirror(FXbool horizontal,FXbool vertical){
  FXTRACE((100,"%s::mirror(%d,%d)\n",getClassName(),horizontal,vertical));
  if(horizontal || vertical){
    if(data){
      register FXuchar *paa,*pa,*pbb,*pb;
      register FXint sa=(8-width)&7;
      register FXint sb=8-sa;
      register FXuint t;
      FXuchar line[4096];               // Maximum width is 32768/8=4096 bytes
      if(vertical && height>1){         // Mirror vertically
        paa=data;
        pbb=data+bytewidth*(height-1);
        do{
          pa=paa; paa+=bytewidth;
          pb=pbb; pbb-=bytewidth;
          do{
            t=*pa; *pa++=*pb; *pb++=t;
            }
          while(pa<paa);
          }
        while(paa<pbb);
        }
      if(horizontal && width>1){        // Mirror horizontally
        paa=data;
        pbb=data+bytewidth*height;
        do{
          pa=paa;
          pb=line+bytewidth;
          do{
            *--pb=*paa++;               // Gnarly!
            }
          while(line<pb);
          do{
            t=*pb++ << sa;
            t|=*pb >> sb;
            *pa++=FXBITREVERSE(t);
            }
          while(pa<paa);
          }
        while(paa<pbb);
        }
      render();
      }
    }
  }


// Rotate bitmap by degrees ccw
void FXBitmap::rotate(FXint degrees){
  FXTRACE((100,"%s::rotate(%d)\n",getClassName(),degrees));
  degrees=(degrees+360)%360;
  if(degrees!=0 && width>1 && height>1){
    if(data){
      register FXuchar *p,*q,bits;
      register FXint bw=bytewidth;
      register FXint i,j,x;
      FXuchar *olddata;
      if(!FXMEMDUP(&olddata,data,FXuchar,bytewidth*height)){ throw FXMemoryException("unable to rotate bitmap"); }
      switch(degrees){
        case 90:
          resize(height,width);
          i=height-1;
          p=data;
          do{
            j=0;
            q=olddata+(i>>3);
            bits=0;
            do{
              bits|=((q[0]>>(i&7))&1)<<(j&7);
              if((j&7)==7){ *p++=bits; bits=0; }
              q+=bw;
              }
            while(++j<width);
            if(j&7){ *p++=bits; }
            }
          while(--i>=0);
          break;
        case 180:               // FIXME works but not as fast as it could be
          i=height-1;
          p=data;
          q=olddata+(height-1)*bw;
          do{
            j=0;
            bits=0;
            x=width-1;
            do{
              bits|=((q[x>>3]>>(x&7))&1)<<(j&7);
              if((j&7)==7){ *p++=bits; bits=0; }
              x--;
              }
            while(++j<width);
            if(j&7){ *p++=bits; }
            q-=bw;
            }
          while(--i>=0);
          break;
        case 270:
          resize(height,width);
          i=0;
          p=data;
          do{
            j=0;
            q=olddata+(i>>3)+(width-1)*bw;
            bits=0;
            do{
              bits|=((q[0]>>(i&7))&1)<<(j&7);
              if((j&7)==7){ *p++=bits; bits=0; }
              q-=bw;
              }
            while(++j<width);
            if(j&7){ *p++=bits; }
            }
          while(++i<height);
          break;
        default:
          fxwarning("%s::rotate: rotation by %d degrees not implemented.\n",getClassName(),degrees);
          break;
        }
      FXFREE(&olddata);
      render();
      }
    else{
      switch(degrees){
        case 90:
          resize(height,width);
          break;
        case 180:
          resize(width,height);
          break;
        case 270:
          resize(height,width);
          break;
        default:
          fxwarning("%s::rotate: rotation by %d degrees not implemented.\n",getClassName(),degrees);
          break;
        }
      }
    }
  }


// Crop bitmap to given rectangle
void FXBitmap::crop(FXint x,FXint y,FXint w,FXint h,FXbool color){
  if(w<1) w=1;
  if(h<1) h=1;
  if(x>=width || y>=height || x+w<=0 || y+h<=0){ fxerror("%s::crop: bad arguments.\n",getClassName()); }
  FXTRACE((100,"%s::crop(%d,%d,%d,%d)\n",getClassName(),x,y,w,h));
  if(data){
    register FXuchar *pnn,*poo,*yyy,*pn,*po,*xx;
    register FXint oldbw=bytewidth;
    register FXint newbw=(w+7)>>3;
    register FXint cpybw;
    register FXint ow=width;
    register FXint oh=height;
    register FXint nw=w;
    register FXint nh=h;
    register FXint cw;
    register FXint ch;
    register FXint sh;
    register FXuint t;
    FXuchar *olddata;
    if(!FXMALLOC(&olddata,FXuchar,oh*bytewidth+1)){ throw FXMemoryException("unable to crop bitmap"); }
    memcpy(olddata,data,oh*bytewidth);
    resize(w,h);
    pnn=data;
    yyy=data+newbw*nh;
    do{
      *pnn++=-color;            // 1 -> 0xff, 0 -> 0xff
      }
    while(pnn<yyy);
    if(x<0){                    // x < 0
      cw=FXMIN(ow,x+nw);
      if(y<0){                  // y < 0
        pnn=data-newbw*y;
        poo=olddata;
        ch=FXMIN(oh,y+nh);
        }
      else{                     // y >= 0
        pnn=data;
        poo=olddata+oldbw*y;
        ch=FXMIN(oh,y+nh)-y;
        }
      pnn+=(-x)>>3;
      sh=8-((-x)&7);
      FXASSERT(cw>0);
      FXASSERT(ch>0);
      yyy=pnn+newbw*ch;
      cpybw=((cw-x+7)>>3)-((-x)>>3);
      //FXTRACE((100,"ow=%d oh=%d nw=%d nh=%d cw=%d ch=%d sh=%d cpybw=%d\n",ow,oh,nw,nh,cw,ch,sh,cpybw));
      do{
        pn=pnn;
        po=poo;
        xx=pnn+cpybw;
        t=(-color)&0xff;
        do{
          t|=(*po++)<<8;
          *pn++=t>>sh;
          t>>=8;
          }
        while(pn<xx);
        if(color){              // A bit ugly but it'll have to do for now...
          *(pn-1)|=0xff<<((cw-x)&7);
          }
        else{
          *(pn-1)&=~(0xff<<((cw-x)&7));
          }
        pnn+=newbw;
        poo+=oldbw;
        }
      while(pnn<yyy);
      }
    else{                       // x >= 0
      cw=FXMIN(ow,x+nw)-x;
      if(y<0){                  // y < 0
        pnn=data-newbw*y;
        poo=olddata;
        ch=FXMIN(oh,y+nh);
        }
      else{                     // y >= 0
        pnn=data;
        poo=olddata+oldbw*y;
        ch=FXMIN(oh,y+nh)-y;
        }
      poo+=x>>3;
      sh=x&7;
      FXASSERT(cw>0);
      FXASSERT(ch>0);
      yyy=pnn+newbw*ch;
      cpybw=(cw+7)>>3;
      do{
        pn=pnn;
        po=poo;
        xx=pnn+cpybw;
        do{
          t=*po++;
          t|=*po<<8;
          *pn++=t>>sh;
          }
        while(pn<xx);
        pnn+=newbw;
        poo+=oldbw;
        }
      while(pnn<yyy);
      }
    FXFREE(&olddata);
    render();
    }
  else{
    resize(w,h);
    }
  }



#ifdef WIN32

// Get the image's device context
FXID FXBitmap::GetDC() const {
  HDC hdc=::CreateCompatibleDC(NULL);
  SelectObject(hdc,(HBITMAP)xid);
  return hdc;
  }


// Release it (no-op)
int FXBitmap::ReleaseDC(FXID hdc) const {
  return ::DeleteDC((HDC)hdc);
  }

#endif


// Attach pixel buffer to bitmap, and assume ownership of it if BITMAP_OWNED is passed
void FXBitmap::setData(FXuchar *pix,FXuint opts){

  // Free old data
  if(options&BITMAP_OWNED){ FXFREE(&data); }

  // Only own pixel buffer if one was passed
  if(pix && (opts&BITMAP_OWNED)){
    options|=BITMAP_OWNED;
    }
  else{
    options&=~BITMAP_OWNED;
    }

  // Set the pointer
  data=pix;
  }


// Populate the bitmap with new pixel data
void FXBitmap::setData(FXuchar *pix,FXuint opts,FXint w,FXint h){

  // Free old data
  if(options&BITMAP_OWNED){ FXFREE(&data); }

  // Resize pixmap
  resize(w,h);

  // Only own pixel buffer if one was passed
  if(pix && (opts&BITMAP_OWNED)){
    options|=BITMAP_OWNED;
    }
  else{
    options&=~BITMAP_OWNED;
    }

  // Set the pointer
  data=pix;
  }


// Change options
void FXBitmap::setOptions(FXuint opts){
  options=(options&~BITMAP_MASK) | (opts&BITMAP_MASK);
  }


// Save pixel data only
bool FXBitmap::savePixels(FXStream& store) const {
  FXuint size=height*bytewidth;
  store.save(data,size);
  return true;
  }


// Load pixel data only
bool FXBitmap::loadPixels(FXStream& store){
  FXuint size=height*bytewidth;
  if(options&BITMAP_OWNED){ FXFREE(&data); }
  if(!FXMALLOC(&data,FXuchar,size)) return false;
  store.load(data,size);
  options|=BITMAP_OWNED;
  return true;
  }


// Save data
void FXBitmap::save(FXStream& store) const {
  FXuchar haspixels=(data!=NULL);
  FXDrawable::save(store);
  store << options;
  store << haspixels;
  if(haspixels) savePixels(store);
  }


// Load data
void FXBitmap::load(FXStream& store){
  FXuchar haspixels;
  FXDrawable::load(store);
  store >> options;
  store >> haspixels;
  if(haspixels) loadPixels(store);
  }


// Clean up
FXBitmap::~FXBitmap(){
  FXTRACE((100,"FXBitmap::~FXBitmap %p\n",this));
  destroy();
  if(options&BITMAP_OWNED){FXFREE(&data);}
  data=(FXuchar*)-1L;
  }

}

