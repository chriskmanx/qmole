/********************************************************************************
*                                                                               *
*                             I m a g e    O b j e c t                          *
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
* $Id: FXImage.cpp,v 1.148.2.1 2006/04/14 01:21:00 fox Exp $                        *
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
#include "FXVisual.h"
#include "FXImage.h"
#include "FXDCWindow.h"
#include "FXException.h"


/*
  Notes:
  - FXImage::create() renders rgb[a] data into X/GDI resident, device
    dependent pixmap.
  - Need to be able to re-render subpart of image.
  - We should implement shared pixmaps.
  - If IMAGE_KEEP, repeated rendering is usually desired; should we
    hang on to XImage, and the shared memory segment in that case?
    How about shared pixmaps...
  - Slight change in interpretation of IMAGE_OWNED flag:- if passed, the
    FXImage will own client-side pixel buffer, otherwise it will not; if
    no pixel-buffer has been passed and IMAGE_OWNED *is* passed, a pixel
    buffer will be allocated [and cleared to zero].
    No pixel buffer will be allocated if neither IMAGE_OWNED nor pixels
    are passed.
  - When using shared image/pixmaps, if IMAGE_KEEP is set, hang on to pixel buffer.
  - We need to speed up 16/15 bpp true color.
  - We need dither tables with 3bit and 2bit rounding for 5,6,5/5,5,5 modes
  - We need dither tables with 5bit, 6bit rounding for 3,3,2 mode.
  - We need to split true color from direct color, because direct color
    has random mapping, true has not.
  - Just because I always forget:

      StaticGray   0
      GrayScale    1
      StaticColor  2
      PseudoColor  3
      TrueColor    4
      DirectColor  5

  - The smooth scaling algorithm is based on the idea of keeping track which
    pixels of the source are contributing to each pixel in the destination.

    The smallest fraction of a pixel of interest is 1/(w*w'), where w is
    the old width, and w' is the new one.

    Consider scaling from 3->7 pixels, with 1/(w*w') being 1/21:

      Source Pixel 2 --------------+        Source Pixel is 7/21 units wide
      Source Pixel 1 --------+     |
      Source Pixel 0 -+      |     |
                      |      |     |
                      v      v     v
      Original:    000000011111112222222
      Scaled:      000111222333444555666
                    ^  ^  ^  ^  ^  ^  ^
                    |  |  |  |  |  |  |
      Dest Pixel 0 -+  |  |  |  |  |  |
      Dest Pixel 1 ----+  |  |  |  |  |
      Dest Pixel 2 -------+  |  |  |  |
      Dest Pixel 3 ----------+  |  |  |
      Dest Pixel 4 -------------+  |  |
      Dest Pixel 5 ----------------+  |
      Dest Pixel 6 -------------------+     Dest Pixel is 3/21 units wide

    As can be seen from the picture above, destination pixel 2 is comprised
    of 1/3 times source pixel 0, and 2/3 times source pixel 1.  Splitting
    into the individual fragments yields 9 fragments total:

      Fragment     0   1 2  3   4  5 6   7   8
      ========================================
      Original:  000 000 0 11 111 11 2 222 222
      Scaled:    000 111 2 22 333 44 4 555 666

    Note no fragment can be smaller than one unit, or 1/(w*w').

    The algorithm iterates over fragments; and for each fragment, it adds
    the source pixel multiplied by the length of the fragment.  It repeats
    until enough fragments have been collected to make one output pixel:

      fin  = w';                        The fractions fin and fout have
      fout = w;                         been multiplied by w*w'
      in   = 0;
      out  = 0;
      acc  = 0;
      while out < w' do
        if fin < fout then
          acc = acc + fin*source[in];   Add fin units of source pixel
          fout = fout - fin;
          fin = outw;
          in = in + 1;
        else
          acc = acc + fout*source[in];  Add fout (the remaining unfilled fraction) of source pixel
          dest[out] = acc / w;          Output one pixel
          acc = 0;
          fin = fin - fout;
          fout = inw;
          out = out + 1;
        end if
      end while

    Remember, you saw it here first!!!!

  - When compositing, out-of-image data behaves as if clear (0,0,0,0)
  - Absence of data behaves as if clear
  - Operations work on subrectangle of an image
  - Images and regions are at least 1x1 pixels
  - Operations of the form a = a op b, or a = op a.
  - Fast blend (0..255) colors:

      blue  = (ALPHA * (sb - db) >> 8) + db;
      green = (ALPHA * (sg - dg) >> 8) + dg;
      red   = (ALPHA * (sr - dr) >> 8) + dr;

  - Fast division by 255:

      r=(i+(i>>8)+1)>>8);

  - Rotate using 3 shear operations:

     { cos(phi) -sin(phi) }   { 1 -tan(phi/2) }   { 1         0 }   { 1  -tan(phi/2) }
     {                    } = {               } x {             } x {                }
     { sin(phi)  cos(phi) }   { 0      1      }   { sin(phi)  1 }   { 0      1       }

*/


#define DISPLAY(app) ((Display*)((app)->display))

// Changable image options
#define IMAGE_MASK   (IMAGE_KEEP|IMAGE_NEAREST|IMAGE_OPAQUE|IMAGE_ALPHACOLOR|IMAGE_SHMI|IMAGE_SHMP|IMAGE_ALPHAGUESS)

// Maximum size of the colormap; for high-end graphics systems
// you may want to define HIGHENDGRAPHICS to allow for large colormaps
#ifdef HIGHENDGRAPHICS
#define MAX_MAPSIZE 4096
#else
#define MAX_MAPSIZE 256
#endif

using namespace FX;

/*******************************************************************************/

namespace FX {

// RGB Ordering code
enum {
  RGB = 7,   // RGB 111      > | R  G  B
  BGR = 0,   // BGR 000      --+--------
  RBG = 6,   // RBG 110      R | x  4  2
  GBR = 1,   // GBR 001      G |    x  1
  BRG = 4,   // BRG 100      B |       x
  GRB = 3    // GRB 011
  };


// Object implementation
FXIMPLEMENT(FXImage,FXDrawable,NULL,0)


// For deserialization
FXImage::FXImage(){
  data=NULL;
  options=0;
  }


// Initialize
FXImage::FXImage(FXApp* a,const FXColor *pix,FXuint opts,FXint w,FXint h):FXDrawable(a,w,h){
  FXTRACE((100,"FXImage::FXImage %p\n",this));
  FXASSERT((opts&~(IMAGE_OWNED|IMAGE_MASK))==0);
  visual=getApp()->getDefaultVisual();
  data=(FXColor*)pix;
  options=opts;
  if(!data && (options&IMAGE_OWNED)){           // This is confusing use of IMAGE_OWNED
    if(!FXCALLOC(&data,FXColor,width*height)){ throw FXMemoryException("unable to construct image"); }
    }
  }


// Create image
void FXImage::create(){
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));

#ifndef WIN32

      // Initialize visual
      visual->create();

      // Get depth (should use visual!!)
      int dd=visual->getDepth();

      // Make pixmap
      xid=XCreatePixmap(DISPLAY(getApp()),XDefaultRootWindow(DISPLAY(getApp())),FXMAX(width,1),FXMAX(height,1),dd);
#else

      // Initialize visual
      visual->create();

      // Create a bitmap compatible with current display
      HDC hdc=::GetDC(GetDesktopWindow());
      xid=CreateCompatibleBitmap(hdc,FXMAX(width,1),FXMAX(height,1));
      ::ReleaseDC(GetDesktopWindow(),hdc);

#endif

      // Were we successful?
      if(!xid){ throw FXImageException("unable to create image"); }

      // Render pixels
      render();

      // Release pixel buffer
      if(!(options&IMAGE_KEEP)) release();
      }
    }
  }


// Release the client-side buffer, free it if it was owned.
void FXImage::release(){
  if(options&IMAGE_OWNED){
    options&=~IMAGE_OWNED;
    FXFREE(&data);
    }
  data=NULL;
  }


// Detach image
void FXImage::detach(){
  visual->detach();
  if(xid){
    FXTRACE((100,"%s::detach %p\n",getClassName(),this));
    xid=0;
    }
  }


// Destroy image
void FXImage::destroy(){
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::destroy %p\n",getClassName(),this));
#ifndef WIN32
      XFreePixmap(DISPLAY(getApp()),xid);
#else
      DeleteObject(xid);
#endif
      }
    xid=0;
    }
  }


// Scan the image and return false if fully opaque
bool FXImage::hasAlpha() const {
  if(data){
    register FXint i=width*height-1;
    do{
      if(((const FXuchar*)(data+i))[3]<255) return true;
      }
    while(--i>=0);
    }
  return false;
  }


#ifndef WIN32

// Find shift amount
static inline FXuint findshift(unsigned long mask){
  register FXuint sh=0;
  while(!(mask&(1UL<<sh))) sh++;
  return sh;
  }


// Find low bit in mask
static inline FXPixel lowbit(FXPixel mask){
  return (~mask+1)&mask;
  }


// Restore client-side pixel buffer from image
void FXImage::restore(){
  if(xid){
    register FXPixel red,green,blue;
    register FXPixel red1,green1,blue1;
    register FXPixel pixel;
    register FXuint  redshift,greenshift,blueshift;
    register FXPixel redmask,greenmask,bluemask;
    register int size,dd,i;
    register bool shmi=false;
    register XImage *xim=NULL;
    register Visual *vis;
    register FXint x,y;
    register FXuchar *img;
    register FXuint r,g,b;
    FXuchar rtab[MAX_MAPSIZE];
    FXuchar gtab[MAX_MAPSIZE];
    FXuchar btab[MAX_MAPSIZE];
#ifdef HAVE_XSHM_H
    XShmSegmentInfo shminfo;
#endif

    FXTRACE((100,"%s::restore image %p\n",getClassName(),this));

    // Check for legal size
    if(width<1 || height<1){ fxerror("%s::restore: illegal image size %dx%d.\n",getClassName(),width,height); }

    // Get Visual
    vis=(Visual*)visual->visual;
    dd=visual->getDepth();

    // Just in case you're on a high-end system
    FXASSERT(vis->map_entries<=MAX_MAPSIZE);

    // Make array for data if needed
    if(!data){
      size=width*height;
      if(!FXMALLOC(&data,FXColor,size)){ throw FXMemoryException("unable to restore image"); }
      options|=IMAGE_OWNED;
      }

    // Got local buffer to receive into
    if(data){

      // Turn it on iff both supported and desired
#ifdef HAVE_XSHM_H
      if(options&IMAGE_SHMI) shmi=getApp()->shmi;
#endif

      // First try XShm
#ifdef HAVE_XSHM_H
      if(shmi){
        xim=XShmCreateImage(DISPLAY(getApp()),vis,dd,(dd==1)?XYPixmap:ZPixmap,NULL,&shminfo,width,height);
        if(!xim){ shmi=0; }
        if(shmi){
          shminfo.shmid=shmget(IPC_PRIVATE,xim->bytes_per_line*xim->height,IPC_CREAT|0777);
          if(shminfo.shmid==-1){ xim->data=NULL; XDestroyImage(xim); xim=NULL; shmi=0; }
          if(shmi){
            shminfo.shmaddr=xim->data=(char*)shmat(shminfo.shmid,0,0);
            shminfo.readOnly=false;
            XShmAttach(DISPLAY(getApp()),&shminfo);
            FXTRACE((150,"RGBPixmap XSHM attached at memory=%p (%d bytes)\n",xim->data,xim->bytes_per_line*xim->height));
            XShmGetImage(DISPLAY(getApp()),xid,xim,0,0,AllPlanes);
            XSync(DISPLAY(getApp()),False);
            }
          }
        }
#endif

      // Try the old fashioned way
      if(!shmi){
        xim=XGetImage(DISPLAY(getApp()),xid,0,0,width,height,AllPlanes,ZPixmap);
        if(!xim){ throw FXImageException("unable to restore image"); }
        }

      // Should have succeeded
      FXASSERT(xim);

      FXTRACE((150,"im width = %d\n",xim->width));
      FXTRACE((150,"im height = %d\n",xim->height));
      FXTRACE((150,"im format = %s\n",xim->format==XYBitmap?"XYBitmap":xim->format==XYPixmap?"XYPixmap":"ZPixmap"));
      FXTRACE((150,"im byte_order = %s\n",(xim->byte_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"im bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"im bitmap_bit_order = %s\n",(xim->bitmap_bit_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"im bitmap_pad = %d\n",xim->bitmap_pad));
      FXTRACE((150,"im bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"im depth = %d\n",xim->depth));
      FXTRACE((150,"im bytes_per_line = %d\n",xim->bytes_per_line));
      FXTRACE((150,"im bits_per_pixel = %d\n",xim->bits_per_pixel));


      {
      XColor colors[MAX_MAPSIZE];

      // Get masks
      redmask=vis->red_mask;
      greenmask=vis->green_mask;
      bluemask=vis->blue_mask;

      // Read back the colormap and convert to more usable form
      if(vis->c_class!=TrueColor && vis->c_class!=DirectColor){
        for(i=0 ; i<vis->map_entries; i++){
          colors[i].pixel=i;
          colors[i].flags=DoRed|DoGreen|DoBlue;
          }
        }
      else{
        red=green=blue=0;
        red1=lowbit(redmask);
        green1=lowbit(greenmask);
        blue1=lowbit(bluemask);
        for(i=0; i<vis->map_entries; i++){
          colors[i].pixel=red|green|blue;
          colors[i].flags=DoRed|DoGreen|DoBlue;
          if(red<redmask) red+=red1;
          if(green<greenmask) green+=green1;
          if(blue<bluemask) blue+=blue1;
          }
        }
      XQueryColors(DISPLAY(getApp()),visual->colormap,colors,vis->map_entries);
      for(i=0; i<vis->map_entries; i++){
        rtab[i]=colors[i].red >> 8;
        gtab[i]=colors[i].green >> 8;
        btab[i]=colors[i].blue >> 8;
        }
      }

      // Now we convert the pixels back to color
      switch(xim->bits_per_pixel){
        case 0:
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
          for(y=0,img=(FXuchar*)data; y<height; y++){
            for(x=0; x<width; x++){
              pixel=XGetPixel(xim,x,y);
              img[0]=rtab[pixel];
              img[1]=gtab[pixel];
              img[2]=btab[pixel];
              img[3]=255;
              img+=4;
              }
            }
          break;
        case 15:
        case 16:
        case 24:
        case 32:
        default:
          FXASSERT(vis->c_class==TrueColor || vis->c_class==DirectColor);
          redshift=findshift(redmask);
          greenshift=findshift(greenmask);
          blueshift=findshift(bluemask);
          for(y=0,img=(FXuchar*)data; y<height; y++){
            for(x=0; x<width; x++){
              pixel=XGetPixel(xim,x,y);
              r=(pixel&redmask)>>redshift;
              g=(pixel&greenmask)>>greenshift;
              b=(pixel&bluemask)>>blueshift;
              img[0]=rtab[r];
              img[1]=gtab[g];
              img[2]=btab[b];
              img[3]=255;
              img+=4;
              }
            }
          break;
        }

      // Destroy image
#ifdef HAVE_XSHM_H
      if(shmi){
        FXTRACE((150,"RGBPixmap XSHM detached at memory=%p (%d bytes)\n",xim->data,xim->bytes_per_line*xim->height));
        XShmDetach(DISPLAY(getApp()),&shminfo);
        XDestroyImage(xim);
        shmdt(shminfo.shmaddr);
        shmctl(shminfo.shmid,IPC_RMID,0);
        }
#endif

      // Destroy image
      if(!shmi){
        XDestroyImage(xim);
        }
      }
    }
  }


#else


// Restore client-side pixel buffer from image
void FXImage::restore(){
  if(xid){
    register FXint size,bytes_per_line,skip,x,y;
    register FXuchar *pix,*img;
    FXuchar *pixels;
    BITMAPINFO bmi;
    HDC hdcmem;

    FXTRACE((100,"%s::restore image %p\n",getClassName(),this));

    // Check for legal size
    if(width<1 || height<1){ fxerror("%s::restore: illegal image size %dx%d.\n",getClassName(),width,height); }

    // Make array for data if needed
    if(!data){
      size=width*height;
      if(!FXMALLOC(&data,FXColor,size)){ throw FXMemoryException("unable to restore image"); }
      options|=IMAGE_OWNED;
      }

    // Got local buffer to receive into
    if(data){

      // Set up the bitmap info
      bytes_per_line=(width*3+3)/4*4;
      skip=bytes_per_line-width*3;

      bmi.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
      bmi.bmiHeader.biWidth=width;
      bmi.bmiHeader.biHeight=-height; // Negative heights means upside down!
      bmi.bmiHeader.biPlanes=1;
      bmi.bmiHeader.biBitCount=24;
      bmi.bmiHeader.biCompression=BI_RGB;
      bmi.bmiHeader.biSizeImage=0;
      bmi.bmiHeader.biXPelsPerMeter=0;
      bmi.bmiHeader.biYPelsPerMeter=0;
      bmi.bmiHeader.biClrUsed=0;
      bmi.bmiHeader.biClrImportant=0;

      // DIB format pads to multiples of 4 bytes...
//      if(!FXMALLOC(&pixels,FXuchar,bytes_per_line*height)){ throw FXImageException("unable to restore image"); }
      pixels=(FXuchar*)VirtualAlloc(0,bytes_per_line*height,MEM_COMMIT,PAGE_READWRITE);
      if(!pixels){ throw FXMemoryException("unable to restore image"); }

      // Make device context
      hdcmem=::CreateCompatibleDC(NULL);
      if(!GetDIBits(hdcmem,(HBITMAP)xid,0,height,pixels,&bmi,DIB_RGB_COLORS)){
        throw FXImageException("unable to restore image");
        }

      // Stuff it into our own data structure
      for(y=0,img=(FXuchar*)data,pix=pixels; y<height; y++){
        for(x=0; x<width; x++){
          img[0]=pix[2];
          img[1]=pix[1];
          img[2]=pix[0];
          img[3]=255;
          img+=4;
          pix+=3;
          }
        pix+=skip;
        }
//      FXFREE(&pixels);
      VirtualFree(pixels,0,MEM_RELEASE);
      ::DeleteDC(hdcmem);
      }
    }
  }


#endif


#ifndef WIN32



// True generic mode
void FXImage::render_true_N_fast(void *xim,FXuchar *img){
  register FXint x,y;
  FXTRACE((150,"True MSB/LSB N bpp render nearest\n"));
  y=0;
  do{
    x=0;
    do{
      XPutPixel(((XImage*)xim),x,y,visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



// True generic mode
void FXImage::render_true_N_dither(void *xim,FXuchar *img){
  register FXint x,y,d;
  FXTRACE((150,"True MSB/LSB N bpp render dither\n"));
  y=0;
  do{
    x=0;
    do{
      d=((y&3)<<2)|(x&3);
      XPutPixel(((XImage*)xim),x,y,visual->rpix[d][img[0]] | visual->gpix[d][img[1]] | visual->bpix[d][img[2]]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



// True 24 bit color
void FXImage::render_true_24(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-(width*3);
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXPixel val;
  register FXint w,h;
  if(((XImage*)xim)->byte_order==MSBFirst){    // MSB
    FXTRACE((150,"True MSB 24bpp render\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        val=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        pix[0]=(FXuchar)(val>>16);
        pix[1]=(FXuchar)(val>>8);
        pix[2]=(FXuchar)val;
        img+=4;
        pix+=3;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  else{                             // LSB
    FXTRACE((150,"True LSB 24bpp render\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        val=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        pix[0]=(FXuchar)val;
        pix[1]=(FXuchar)(val>>8);
        pix[2]=(FXuchar)(val>>16);
        img+=4;
        pix+=3;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  }



// True 32 bit color
void FXImage::render_true_32(void *xim,FXuchar *img){
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXuint jmp=((XImage*)xim)->bytes_per_line-(width<<2);
  register FXPixel val;
  register FXint w,h;

  // Byte order matches
  if(((XImage*)xim)->byte_order == FOX_BIGENDIAN){
    FXTRACE((150,"True MSB/LSB 32bpp render\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        *((FXuint*)pix)=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        img+=4;
        pix+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }

  // MSB Byte order
  else if(((XImage*)xim)->byte_order==MSBFirst){
    FXTRACE((150,"True MSB 32bpp render\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        val=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        pix[0]=(FXuchar)(val>>24);
        pix[1]=(FXuchar)(val>>16);
        pix[2]=(FXuchar)(val>>8);
        pix[3]=(FXuchar)val;
        img+=4;
        pix+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }

  // LSB Byte order
  else{
    FXTRACE((150,"True LSB 32bpp render\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        val=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        pix[0]=(FXuchar)val;
        pix[1]=(FXuchar)(val>>8);
        pix[2]=(FXuchar)(val>>16);
        pix[3]=(FXuchar)(val>>24);
        img+=4;
        pix+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  }



// True 16 bit color
void FXImage::render_true_16_fast(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-(width<<1);
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXPixel val;
  register FXint w,h;

  // Byte order matches
  if(((XImage*)xim)->byte_order == FOX_BIGENDIAN){
    FXTRACE((150,"True MSB/LSB 16bpp 5,6,5/5,5,5 render nearest\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        *((FXushort*)pix)=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        img+=4;
        pix+=2;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }

  // MSB Byte order
  else if(((XImage*)xim)->byte_order==MSBFirst){
    FXTRACE((150,"True MSB 16bpp 5,6,5/5,5,5 render nearest\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        val=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        pix[0]=(FXuchar)(val>>8);
        pix[1]=(FXuchar)val;
        img+=4;
        pix+=2;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }

  // LSB Byte order
  else{
    FXTRACE((150,"True LSB 16bpp 5,6,5/5,5,5 render nearest\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        val=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
        pix[0]=(FXuchar)val;
        pix[1]=(FXuchar)(val>>8);
        img+=4;
        pix+=2;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  }


// True 16 bit color, dithered
void FXImage::render_true_16_dither(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-(width<<1);
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXPixel val;
  register FXint w,h,d;

  // Byte order matches
  if(((XImage*)xim)->byte_order == FOX_BIGENDIAN){
    FXTRACE((150,"True MSB/LSB 16bpp 5,6,5/5,5,5 render dither\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        d=((h&3)<<2)|(w&3);
        *((FXushort*)pix)=visual->rpix[d][img[0]] | visual->gpix[d][img[1]] | visual->bpix[d][img[2]];
        img+=4;
        pix+=2;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }

  // MSB Byte order
  else if(((XImage*)xim)->byte_order==MSBFirst){
    FXTRACE((150,"True MSB 16bpp 5,6,5/5,5,5 render dither\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        d=((h&3)<<2)|(w&3);
        val=visual->rpix[d][img[0]] | visual->gpix[d][img[1]] | visual->bpix[d][img[2]];
        pix[0]=(FXuchar)(val>>8);
        pix[1]=(FXuchar)val;
        img+=4;
        pix+=2;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }

  // LSB Byte order
  else{
    FXTRACE((150,"True LSB 16bpp 5,6,5/5,5,5 render dither\n"));
    h=height-1;
    do{
      w=width-1;
      do{
        d=((h&3)<<2)|(w&3);
        val=visual->rpix[d][img[0]] | visual->gpix[d][img[1]] | visual->bpix[d][img[2]];
        pix[0]=(FXuchar)val;
        pix[1]=(FXuchar)(val>>8);
        img+=4;
        pix+=2;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  }



// True 8 bit color
void FXImage::render_true_8_fast(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXint w,h;
  FXTRACE((150,"True MSB/LSB 8bpp render nearest\n"));
  h=height-1;
  do{
    w=width-1;
    do{
      *pix=visual->rpix[1][img[0]] | visual->gpix[1][img[1]] | visual->bpix[1][img[2]];
      img+=4;
      pix++;
      }
    while(--w>=0);
    pix+=jmp;
    }
  while(--h>=0);
  }



// True 8 bit color, dithered
void FXImage::render_true_8_dither(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXint w,h,d;
  FXTRACE((150,"True MSB/LSB 8bpp render dither\n"));
  h=height-1;
  do{
    w=width-1;
    do{
      d=((h&3)<<2)|(w&3);
      *pix=visual->rpix[d][img[0]] | visual->gpix[d][img[1]] | visual->bpix[d][img[2]];
      img+=4;
      pix++;
      }
    while(--w>=0);
    pix+=jmp;
    }
  while(--h>=0);
  }



// Render 4 bit index color mode
void FXImage::render_index_4_fast(void *xim,FXuchar *img){
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXuint val,half;
  register FXint w,h;
  if(((XImage*)xim)->byte_order==MSBFirst){    // MSB
    FXTRACE((150,"Index MSB 4bpp render nearest\n"));
    h=height-1;
    do{
      w=width-1;
      half=0;
      do{
        val=visual->lut[visual->rpix[1][img[0]]+visual->gpix[1][img[1]]+visual->bpix[1][img[2]]];
        if(half) *pix++|=val;
        else *pix=val<<4;
        half^=1;
        img+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  else{                               // LSB
    FXTRACE((150,"Index LSB 4bpp render nearest\n"));
    h=height-1;
    do{
      w=width-1;
      half=0;
      do{
        val=visual->lut[visual->rpix[1][img[0]]+visual->gpix[1][img[1]]+visual->bpix[1][img[2]]];
        if(half) *pix++|=val<<4;
        else *pix=val;
        half^=1;
        img+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  }



// Render 4 bit index color mode
void FXImage::render_index_4_dither(void *xim,FXuchar *img){
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXuint val,half,d;
  register FXint w,h;
  if(((XImage*)xim)->byte_order==MSBFirst){    // MSB
    FXTRACE((150,"Index MSB 4bpp render dither\n"));
    h=height-1;
    do{
      w=width-1;
      half=0;
      do{
        d=((h&3)<<2)|(w&3);
        val=visual->lut[visual->rpix[d][img[0]]+visual->gpix[d][img[1]]+visual->bpix[d][img[2]]];
        if(half) *pix++|=val;
        else *pix=val<<4;
        half^=1;
        img+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  else{                               // LSB
    FXTRACE((150,"Index LSB 4bpp render dither\n"));
    h=height-1;
    do{
      w=width-1;
      half=0;
      do{
        d=((h&3)<<2)|(w&3);
        val=visual->lut[visual->rpix[d][img[0]]+visual->gpix[d][img[1]]+visual->bpix[d][img[2]]];
        if(half) *pix++|=val<<4;
        else *pix=val;
        half^=1;
        img+=4;
        }
      while(--w>=0);
      pix+=jmp;
      }
    while(--h>=0);
    }
  }



// Render 8 bit index color mode
void FXImage::render_index_8_fast(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXint w,h;
  FXTRACE((150,"Index MSB/LSB 8bpp render nearest\n"));
  h=height-1;
  do{
    w=width-1;
    do{
      *pix=visual->lut[visual->rpix[1][img[0]]+visual->gpix[1][img[1]]+visual->bpix[1][img[2]]];
      img+=4;
      pix++;
      }
    while(--w>=0);
    pix+=jmp;
    }
  while(--h>=0);
  }



// Render 8 bit index color mode
void FXImage::render_index_8_dither(void *xim,FXuchar *img){
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXint w,h,d;
  FXTRACE((150,"Index MSB/LSB 8bpp render dither\n"));
  h=height-1;
  do{
    w=width-1;
    do{
      d=((h&3)<<2)|(w&3);
      *pix=visual->lut[visual->rpix[d][img[0]]+visual->gpix[d][img[1]]+visual->bpix[d][img[2]]];
      img+=4;
      pix++;
      }
    while(--w>=0);
    pix+=jmp;
    }
  while(--h>=0);
  }



// Render generic N bit index color mode
void FXImage::render_index_N_fast(void *xim,FXuchar *img){
  register FXint x,y;
  FXTRACE((150,"Index MSB/LSB N bpp render nearest\n"));
  y=0;
  do{
    x=0;
    do{
      XPutPixel(((XImage*)xim),x,y,visual->lut[visual->rpix[1][img[0]]+visual->gpix[1][img[1]]+visual->bpix[1][img[2]]]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



// Render generic N bit index color mode
void FXImage::render_index_N_dither(void *xim,FXuchar *img){
  register FXint x,y,d;
  FXTRACE((150,"Index MSB/LSB N bpp render dither\n"));
  y=0;
  do{
    x=0;
    do{
      d=((y&3)<<2)|(x&3);
      XPutPixel(((XImage*)xim),x,y,visual->lut[visual->rpix[d][img[0]]+visual->gpix[d][img[1]]+visual->bpix[d][img[2]]]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



// Render 8 bit gray mode
void FXImage::render_gray_8_fast(void *xim,FXuchar *img){
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXint w,h;
  FXTRACE((150,"Gray MSB/LSB 8bpp render nearest\n"));
  h=height-1;
  do{
    w=width-1;
    do{
      *pix=visual->gpix[1][(77*img[0]+151*img[1]+29*img[2])>>8];
      img+=4;
      pix++;
      }
    while(--w>=0);
    pix+=jmp;
    }
  while(--h>=0);
  }



// Render 8 bit gray mode
void FXImage::render_gray_8_dither(void *xim,FXuchar *img){
  register FXuchar *pix=(FXuchar*)((XImage*)xim)->data;
  register FXuint jmp=((XImage*)xim)->bytes_per_line-width;
  register FXint w,h;
  FXTRACE((150,"Gray MSB/LSB 8bpp render dither\n"));
  h=height-1;
  do{
    w=width-1;
    do{
      *pix=visual->gpix[((h&3)<<2)|(w&3)][(77*img[0]+151*img[1]+29*img[2])>>8];
      img+=4;
      pix++;
      }
    while(--w>=0);
    pix+=jmp;
    }
  while(--h>=0);
  }



// Render generic N bit gray mode
void FXImage::render_gray_N_fast(void *xim,FXuchar *img){
  register FXint x,y;
  FXTRACE((150,"Gray MSB/LSB N bpp render nearest\n"));
  y=0;
  do{
    x=0;
    do{
      XPutPixel(((XImage*)xim),x,y,visual->gpix[1][(77*img[0]+151*img[1]+29*img[2])>>8]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



// Render generic N bit gray mode
void FXImage::render_gray_N_dither(void *xim,FXuchar *img){
  register FXint x,y;
  FXTRACE((150,"Gray MSB/LSB N bpp render dither\n"));
  y=0;
  do{
    x=0;
    do{
      XPutPixel(((XImage*)xim),x,y,visual->gpix[((y&3)<<2)|(x&3)][(77*img[0]+151*img[1]+29*img[2])>>8]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }




// Render monochrome mode
void FXImage::render_mono_1_fast(void *xim,FXuchar *img){
  register FXint x,y;
  FXTRACE((150,"Monochrome MSB/LSB 1bpp render nearest\n"));
  y=0;
  do{
    x=0;
    do{
      XPutPixel(((XImage*)xim),x,y,visual->gpix[1][(77*img[0]+151*img[1]+29*img[2])>>8]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



// Render monochrome mode
void FXImage::render_mono_1_dither(void *xim,FXuchar *img){
  register FXint x,y;
  FXTRACE((150,"Monochrome MSB/LSB 1bpp render dither\n"));
  y=0;
  do{
    x=0;
    do{
      XPutPixel(((XImage*)xim),x,y,visual->gpix[((y&3)<<2)|(x&3)][(77*img[0]+151*img[1]+29*img[2])>>8]);
      img+=4;
      }
    while(++x<width);
    }
  while(++y<height);
  }



#endif



#ifndef WIN32


// Render into pixmap
void FXImage::render(){
  if(xid){
    register bool shmi=false;
    register XImage *xim=NULL;
    register Visual *vis;
    register int dd;
    XGCValues values;
    GC gc;
#ifdef HAVE_XSHM_H
    XShmSegmentInfo shminfo;
#endif

    FXTRACE((100,"%s::render image %p\n",getClassName(),this));

    // Fill with pixels if there is data
    if(data && 0<width && 0<height){

      // Make GC
      values.foreground=BlackPixel(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
      values.background=WhitePixel(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
      gc=XCreateGC(DISPLAY(getApp()),xid,GCForeground|GCBackground,&values);

      // Get Visual
      vis=(Visual*)visual->visual;

      dd=visual->getDepth();

      // Turn it on iff both supported and desired
#ifdef HAVE_XSHM_H
      if(options&IMAGE_SHMI) shmi=getApp()->shmi;
#endif

      // First try XShm
#ifdef HAVE_XSHM_H
      if(shmi){
        xim=XShmCreateImage(DISPLAY(getApp()),vis,dd,(dd==1)?XYPixmap:ZPixmap,NULL,&shminfo,width,height);
        if(!xim){ shmi=0; }
        if(shmi){
          shminfo.shmid=shmget(IPC_PRIVATE,xim->bytes_per_line*xim->height,IPC_CREAT|0777);
          if(shminfo.shmid==-1){ xim->data=NULL; XDestroyImage(xim); xim=NULL; shmi=0; }
          if(shmi){
            shminfo.shmaddr=xim->data=(char*)shmat(shminfo.shmid,0,0);
            shminfo.readOnly=false;
            XShmAttach(DISPLAY(getApp()),&shminfo);
            FXTRACE((150,"RGBPixmap XSHM attached at memory=%p (%d bytes)\n",xim->data,xim->bytes_per_line*xim->height));
            }
          }
        }
#endif

      // Try the old fashioned way
      if(!shmi){
        xim=XCreateImage(DISPLAY(getApp()),vis,dd,(dd==1)?XYPixmap:ZPixmap,0,NULL,width,height,32,0);
        if(!xim){ throw FXImageException("unable to render image"); }

        // Try create temp pixel store
        if(!FXMALLOC(&xim->data,char,xim->bytes_per_line*height)){ throw FXMemoryException("unable to render image"); }
        }

      // Should have succeeded
      FXASSERT(xim);

      FXTRACE((150,"im width = %d\n",xim->width));
      FXTRACE((150,"im height = %d\n",xim->height));
      FXTRACE((150,"im format = %s\n",xim->format==XYBitmap?"XYBitmap":xim->format==XYPixmap?"XYPixmap":"ZPixmap"));
      FXTRACE((150,"im byte_order = %s\n",(xim->byte_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"im bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"im bitmap_bit_order = %s\n",(xim->bitmap_bit_order==MSBFirst)?"MSBFirst":"LSBFirst"));
      FXTRACE((150,"im bitmap_pad = %d\n",xim->bitmap_pad));
      FXTRACE((150,"im bitmap_unit = %d\n",xim->bitmap_unit));
      FXTRACE((150,"im depth = %d\n",xim->depth));
      FXTRACE((150,"im bytes_per_line = %d\n",xim->bytes_per_line));
      FXTRACE((150,"im bits_per_pixel = %d\n",xim->bits_per_pixel));

      // Determine what to do
      switch(visual->getType()){
        case VISUALTYPE_TRUE:
          switch(xim->bits_per_pixel){
            case 32:
              render_true_32(xim,(FXuchar*)data);
              break;
            case 24:
              render_true_24(xim,(FXuchar*)data);
              break;
            case 15:
            case 16:
              if(options&IMAGE_NEAREST)
                render_true_16_fast(xim,(FXuchar*)data);
              else
                render_true_16_dither(xim,(FXuchar*)data);
              break;
            case 8:
              if(options&IMAGE_NEAREST)
                render_true_8_fast(xim,(FXuchar*)data);
              else
                render_true_8_dither(xim,(FXuchar*)data);
              break;
            default:
              if(options&IMAGE_NEAREST)
                render_true_N_fast(xim,(FXuchar*)data);
              else
                render_true_N_dither(xim,(FXuchar*)data);
              break;
            }
          break;
        case VISUALTYPE_GRAY:
          switch(xim->bits_per_pixel){
            case 1:
              if(options&IMAGE_NEAREST)
                render_mono_1_fast(xim,(FXuchar*)data);
              else
                render_mono_1_dither(xim,(FXuchar*)data);
              break;
            case 8:
              if(options&IMAGE_NEAREST)
                render_gray_8_fast(xim,(FXuchar*)data);
              else
                render_gray_8_dither(xim,(FXuchar*)data);
              break;
            default:
              if(options&IMAGE_NEAREST)
                render_gray_N_fast(xim,(FXuchar*)data);
              else
                render_gray_N_dither(xim,(FXuchar*)data);
              break;
            }
          break;
        case VISUALTYPE_INDEX:
          switch(xim->bits_per_pixel){
            case 4:
              if(options&IMAGE_NEAREST)
                render_index_4_fast(xim,(FXuchar*)data);
              else
                render_index_4_dither(xim,(FXuchar*)data);
              break;
            case 8:
              if(options&IMAGE_NEAREST)
                render_index_8_fast(xim,(FXuchar*)data);
              else
                render_index_8_dither(xim,(FXuchar*)data);
              break;
            default:
              if(options&IMAGE_NEAREST)
                render_index_N_fast(xim,(FXuchar*)data);
              else
                render_index_N_dither(xim,(FXuchar*)data);
              break;
            }
          break;
        case VISUALTYPE_MONO:
          if(options&IMAGE_NEAREST)
            render_mono_1_fast(xim,(FXuchar*)data);
          else
            render_mono_1_dither(xim,(FXuchar*)data);
        case VISUALTYPE_UNKNOWN:
          break;
        }

      // Transfer image with shared memory
#ifdef HAVE_XSHM_H
      if(shmi){
        XShmPutImage(DISPLAY(getApp()),xid,gc,xim,0,0,0,0,width,height,False);
        XSync(DISPLAY(getApp()),False);
        FXTRACE((150,"RGBPixmap XSHM detached at memory=%p (%d bytes)\n",xim->data,xim->bytes_per_line*xim->height));
        XShmDetach(DISPLAY(getApp()),&shminfo);
        xim->data=NULL;
        XDestroyImage(xim);
        shmdt(shminfo.shmaddr);
        shmctl(shminfo.shmid,IPC_RMID,0);
        }
#endif

      // Transfer the image old way
      if(!shmi){
        XPutImage(DISPLAY(getApp()),xid,gc,xim,0,0,0,0,width,height);
        FXFREE(&xim->data);
        XDestroyImage(xim);
        }
      XFreeGC(DISPLAY(getApp()),gc);
      }
    }
  }


#else


void FXImage::render(){
  if(xid){
    register FXint bytes_per_line,skip,h,w;
    register FXuchar *src,*dst;
    BITMAPINFO bmi;
    FXuchar *pixels;
    HDC hdcmem;

    FXTRACE((100,"%s::render %p\n",getClassName(),this));

    // Fill with pixels if there is data
    if(data && 0<width && 0<height){

      // Set up the bitmap info
      bmi.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
      bmi.bmiHeader.biWidth=width;
      bmi.bmiHeader.biHeight=height;
      bmi.bmiHeader.biPlanes=1;
      bmi.bmiHeader.biBitCount=24;
      bmi.bmiHeader.biCompression=BI_RGB;
      bmi.bmiHeader.biSizeImage=0;
      bmi.bmiHeader.biXPelsPerMeter=0;
      bmi.bmiHeader.biYPelsPerMeter=0;
      bmi.bmiHeader.biClrUsed=0;
      bmi.bmiHeader.biClrImportant=0;

      // DIB format pads to multiples of 4 bytes...
      bytes_per_line=(width*3+3)&~3;
//      if(!FXMALLOC(&pixels,FXuchar,bytes_per_line*height)){ throw FXMemoryException("unable to render image"); }
      pixels=(FXuchar*)VirtualAlloc(0,bytes_per_line*height,MEM_COMMIT,PAGE_READWRITE);
      if(!pixels){ throw FXMemoryException("unable to render image"); }
      skip=-bytes_per_line-width*3;
      src=(FXuchar*)data;
      dst=pixels+height*bytes_per_line+width*3;
      h=height;
      do{
        dst+=skip;
        w=width;
        do{
          dst[0]=src[2];
          dst[1]=src[1];
          dst[2]=src[0];
          src+=4;
          dst+=3;
          }
        while(--w);
        }
      while(--h);
      // The MSDN documentation for SetDIBits() states that "the device context
      // identified by the (first) parameter is used only if the DIB_PAL_COLORS
      // constant is set for the (last) parameter". This may be true, but under
      // Win95 you must pass in a non-NULL hdc for the first parameter; otherwise
      // this call to SetDIBits() will fail (in contrast, it works fine under
      // Windows NT if you pass in a NULL hdc).
      hdcmem=::CreateCompatibleDC(NULL);
      if(!SetDIBits(hdcmem,(HBITMAP)xid,0,height,pixels,&bmi,DIB_RGB_COLORS)){
//    if(!StretchDIBits(hdcmem,0,0,width,height,0,0,width,height,pixels,&bmi,DIB_RGB_COLORS,SRCCOPY)){
        throw FXImageException("unable to render image");
        }
      GdiFlush();
      VirtualFree(pixels,0,MEM_RELEASE);
//      FXFREE(&pixels);
      ::DeleteDC(hdcmem);
      }
    }
  }

#endif


/*
    register FXuint r=FXREDVAL(color);
    register FXuint g=FXGREENVAL(color);
    register FXuint b=FXBLUEVAL(color);
    register FXuint a=FXALPHAVAL(color);
    register FXuchar *pix=data;
    register FXuchar *end=pix+height*width*channels;
    FXuchar  tbl[512];

    // Fill table
    for(int i=0; i<256; i++){
      tbl[255+i]=-(i*factor+127)/255;
      tbl[255-i]= (i*factor+127)/255;
      }

    // Fade
    if(channels==4){
      do{
        pix[0]=pix[0]+tbl[255+pix[0]-r];
        pix[1]=pix[1]+tbl[255+pix[1]-g];
        pix[2]=pix[2]+tbl[255+pix[2]-b];
        pix[3]=pix[3]+tbl[255+pix[3]-a];
        pix+=4;
        }
      while(pix<end);
      }
    else{
      do{
        pix[0]=pix[0]+tbl[255+pix[0]-r];
        pix[1]=pix[1]+tbl[255+pix[1]-g];
        pix[2]=pix[2]+tbl[255+pix[2]-b];
        pix+=3;
        }
      while(pix<end);
      }
    }
*/

  // FXColor ____blend(FXColor fg,FXColor bg){
//   register FXuint r,g,b,s,t,tmp;
//   s=FXALPHAVAL(fg);
//   t=~s;
//   tmp=FXREDVAL(fg)*s+FXREDVAL(bg)*t+127;     r=(tmp+(tmp>>8))>>8;
//   tmp=FXGREENVAL(fg)*s+FXGREENVAL(bg)*t+127; g=(tmp+(tmp>>8))>>8;
//   tmp=FXBLUEVAL(fg)*s+FXBLUEVAL(bg)*t+127;   b=(tmp+(tmp>>8))>>8;
//   return FXRGB(r,g,b);
//   }
/*
        s=pix[3];
        t=s^0xff;
        w=pix[0]*s+r*t; pix[0]=(w+(w>>8))>>8;
        w=pix[1]*s+g*t; pix[1]=(w+(w>>8))>>8;
        w=pix[2]*s+b*t; pix[2]=(w+(w>>8))>>8;
        s=pix[3];
*/


// Fill image with color
void FXImage::fill(FXColor color){
  if(data){
    register FXColor *pix=data;
    register FXColor *end=pix+height*width;
    do{ *pix++=color; }while(pix<end);
    }
  }


// Fade image to uniform color
void FXImage::fade(FXColor color,FXint factor){
  if(data){
    register FXuint s=factor;
    register FXuint t=~factor;
    register FXuint r=FXREDVAL(color)*t;
    register FXuint g=FXGREENVAL(color)*t;
    register FXuint b=FXBLUEVAL(color)*t;
    register FXuint a=FXALPHAVAL(color)*t;
    register FXuint w;
    register FXuchar *pix=(FXuchar*)data;
    register FXuchar *end=pix+height*width*4;
    do{
      w=pix[0]*s+r; pix[0]=(w+(w>>8))>>8;
      w=pix[1]*s+g; pix[1]=(w+(w>>8))>>8;
      w=pix[2]*s+b; pix[2]=(w+(w>>8))>>8;
      w=pix[3]*s+a; pix[3]=(w+(w>>8))>>8;
      pix+=4;
      }
    while(pix<end);
    }
  }


// Blend image over uniform color
void FXImage::blend(FXColor color){
  if(data){
    register FXuchar *pix=(FXuchar*)data;
    register FXuchar *end=pix+height*width*4;
    register FXint r=FXREDVAL(color);
    register FXint g=FXGREENVAL(color);
    register FXint b=FXBLUEVAL(color);
    register FXint s,w;
    do{
      s=pix[3];
      w=(pix[0]-r)*s; pix[0]=r+((w+(w>>8)+128)>>8);
      w=(pix[1]-g)*s; pix[1]=g+((w+(w>>8)+128)>>8);
      w=(pix[2]-b)*s; pix[2]=b+((w+(w>>8)+128)>>8);
      pix+=4;
      }
    while(pix<end);
    }
  }


// Resize pixmap to the specified width and height; the data
// array is resized also, but its contents will be undefined.
void FXImage::resize(FXint w,FXint h){
  if(w<1) w=1;
  if(h<1) h=1;
  FXTRACE((100,"%s::resize(%d,%d)\n",getClassName(),w,h));
  if(width!=w || height!=h){

    // Resize device dependent pixmap
    if(xid){
#ifndef WIN32
      int dd=visual->getDepth();
      XFreePixmap(DISPLAY(getApp()),xid);
      xid=XCreatePixmap(DISPLAY(getApp()),XDefaultRootWindow(DISPLAY(getApp())),w,h,dd);
      if(!xid){ throw FXImageException("unable to resize image"); }
#else
      DeleteObject(xid);
      HDC hdc=::GetDC(GetDesktopWindow());
      xid=CreateCompatibleBitmap(hdc,w,h);
      ::ReleaseDC(GetDesktopWindow(),hdc);
      if(!xid){ throw FXImageException("unable to resize image"); }
#endif
      }
    }

  // Resize data array
  if(data){
    if(!(options&IMAGE_OWNED)){         // Need to own array
      if(!FXMALLOC(&data,FXColor,w*h)){ throw FXMemoryException("unable to resize image"); }
      options|=IMAGE_OWNED;
      }
    else if(w*h!=width*height){
      if(!FXRESIZE(&data,FXColor,w*h)){ throw FXMemoryException("unable to resize image"); }
      }
    }

  // Remember new size
  width=w;
  height=h;
  }


static void hscalergba(FXuchar *dst,const FXuchar* src,FXint dw,FXint dh,FXint sw,FXint ){
  register FXint fin,fout,ar,ag,ab,aa;
  register FXint ss=4*sw;
  register FXint ds=4*dw;
  register FXuchar *end=dst+ds*dh;
  register FXuchar *d;
  register const FXuchar *s;
  do{
    s=src; src+=ss;
    d=dst; dst+=ds;
    fin=dw;
    fout=sw;
    ar=ag=ab=aa=0;
    while(1){
      if(fin<fout){
        ar+=fin*s[0];
        ag+=fin*s[1];
        ab+=fin*s[2];
        aa+=fin*s[3];
        fout-=fin;
        fin=dw;
        s+=4;
        }
      else{
        ar+=fout*s[0]; d[0]=ar/sw; ar=0;
        ag+=fout*s[1]; d[1]=ag/sw; ag=0;
        ab+=fout*s[2]; d[2]=ab/sw; ab=0;
        aa+=fout*s[3]; d[3]=aa/sw; aa=0;
        fin-=fout;
        fout=sw;
        d+=4;
        if(d>=dst) break;
        }
      }
    }
  while(dst<end);
  }


static void vscalergba(FXuchar *dst,const FXuchar* src,FXint dw,FXint dh,FXint sw,FXint sh){
  register FXint fin,fout,ar,ag,ab,aa;
  register FXint ss=4*sw;
  register FXint ds=4*dw;
  register FXint dss=ds*dh;
  register FXuchar *end=dst+ds;
  register FXuchar *d,*dd;
  register const FXuchar *s;
  do{
    s=src; src+=4;
    d=dst; dst+=4;
    dd=d+dss;
    fin=dh;
    fout=sh;
    ar=ag=ab=aa=0;
    while(1){
      if(fin<fout){
        ar+=fin*s[0];
        ag+=fin*s[1];
        ab+=fin*s[2];
        aa+=fin*s[3];
        fout-=fin;
        fin=dh;
        s+=ss;
        }
      else{
        ar+=fout*s[0]; d[0]=ar/sh; ar=0;
        ag+=fout*s[1]; d[1]=ag/sh; ag=0;
        ab+=fout*s[2]; d[2]=ab/sh; ab=0;
        aa+=fout*s[3]; d[3]=aa/sh; aa=0;
        fin-=fout;
        fout=sh;
        d+=ds;
        if(d>=dd) break;
        }
      }
    }
  while(dst<end);
  }


// Simple nearest neighbor scaling; fast but ugly
static void scalenearest(FXColor *dst,const FXColor* src,FXint dw,FXint dh,FXint sw,FXint sh){
  register FXint xs=(sw<<16)/dw;
  register FXint ys=(sh<<16)/dh;
  register FXint i,j,x,y;
  register const FXColor *q;
  register FXColor *p;
  i=0;
  y=ys>>1;
  p=dst;
  do{
    j=0;
    x=xs>>1;
    q=src+(y>>16)*sw;
    do{
      p[j]=q[x>>16];
      x+=xs;
      }
    while(++j<dw);
    p+=dw;
    y+=ys;
    }
  while(++i<dh);
  }

/*
  Nice resize:
    16x16 -> 1024x1024    440025862
    1024x1024 -> 16x16     25313450

  Nearest neighbor:
    16x16 -> 1024x1024     15717582
    1024x1024 -> 16x16        32508


extern FXlong fxgetticks();
static FXlong __starttick__,__endtick__;
__starttick__=fxgetticks();
__endtick__ =fxgetticks();
fprintf(stderr,"ticks=%lld\n",__endtick__-__starttick__);
*/

// Resize drawable to the specified width and height
void FXImage::scale(FXint w,FXint h,FXint quality){
  if(w<1) w=1;
  if(h<1) h=1;
  FXTRACE((100,"%s::scale(%d,%d)\n",getClassName(),w,h));
  if(w!=width || h!=height){
    if(data){
      register FXint ow=width;
      register FXint oh=height;
      FXColor *interim;

      switch(quality){
        case 0:

          // Copy to old buffer
          if(!FXMEMDUP(&interim,data,FXColor,ow*oh)){ throw FXMemoryException("unable to scale image"); }

          // Resize the pixmap and target buffer
          resize(w,h);

          // Fast nearest neighbor scale
          scalenearest(data,interim,w,h,ow,oh);

          // Free old buffer
          FXFREE(&interim);
          break;
        default:

          // Allocate interim buffer
          if(!FXMALLOC(&interim,FXColor,w*oh)){ throw FXMemoryException("unable to scale image"); }

          // Scale horizontally first, placing result into interim buffer
          if(w==ow){
            memcpy((FXuchar*)interim,(FXuchar*)data,w*oh*4);
            }
          else{
            hscalergba((FXuchar*)interim,(FXuchar*)data,w,oh,ow,oh);
            }

          // Resize the pixmap and target buffer
          resize(w,h);

          // Scale vertically from the interim buffer into target buffer
          if(h==oh){
            memcpy((FXuchar*)data,(FXuchar*)interim,w*h*4);
            }
          else{
            vscalergba((FXuchar*)data,(FXuchar*)interim,w,h,w,oh);
            }

          // Free interim buffer
          FXFREE(&interim);
          break;
        }
      render();
      }
    else{
      resize(w,h);
      }
    }
  }


// Mirror image horizontally and/or vertically
void FXImage::mirror(bool horizontal,bool vertical){
  FXTRACE((100,"%s::mirror(%d,%d)\n",getClassName(),horizontal,vertical));
  if(horizontal || vertical){
    if(data){
      register FXColor *paa,*pa,*pbb,*pb,t;
      if(vertical && height>1){     // Mirror vertically
        paa=data;
        pbb=data+width*(height-1);
        do{
          pa=paa; paa+=width;
          pb=pbb; pbb-=width;
          do{
            t=*pa; *pa++=*pb; *pb++=t;
            }
          while(pa<paa);
          }
        while(paa<pbb);
        }
      if(horizontal && width>1){    // Mirror horizontally
        paa=data;
        pbb=data+width*height;
        do{
          pa=paa; paa+=width;
          pb=paa;
          do{
            t=*--pb; *pb=*pa; *pa++=t;
            }
          while(pa<pb);
          }
        while(paa<pbb);
        }
      render();
      }
    }
  }


// Shear in X
static void shearx(FXuchar *out,FXuchar* in,FXint nwidth,FXint owidth,FXint height,FXint shear,FXColor clr){
  register FXuchar *ppp,*pp,*qq,*p,*q,*k;
  register FXuint r=FXREDVAL(clr);
  register FXuint g=FXGREENVAL(clr);
  register FXuint b=FXBLUEVAL(clr);
  register FXuint a=FXALPHAVAL(clr);
  register FXint dp=owidth<<2;
  register FXint dq=nwidth<<2;
  register FXint s,z,y,d;
  if(shear){
    if(shear>0){ y=height-1; d=-1; } else { shear=-shear; y=0; d=1; }
    pp=in;
    ppp=pp+height*dp;
    qq=out;
    do{
      p=pp; pp+=dp;
      q=qq; qq+=dq;
      z=(y*shear-1)/(height-1); y+=d;
      s=z&255;
      k=q+(z>>8)*4;
      while(q<k){
        q[0]=r;
        q[1]=g;
        q[2]=b;
        q[3]=a;
        q+=4;
        }
      q[0]=((r-p[0])*s+(p[0]<<8)+127)>>8;
      q[1]=((g-p[1])*s+(p[1]<<8)+127)>>8;
      q[2]=((b-p[2])*s+(p[2]<<8)+127)>>8;
      q[3]=((a-p[3])*s+(p[3]<<8)+127)>>8;
      q+=4;
      p+=4;
      while(p<pp){
        q[0]=((p[0-4]-p[0])*s+(p[0]<<8)+127)>>8;
        q[1]=((p[1-4]-p[1])*s+(p[1]<<8)+127)>>8;
        q[2]=((p[2-4]-p[2])*s+(p[2]<<8)+127)>>8;
        q[3]=((p[3-4]-p[3])*s+(p[3]<<8)+127)>>8;
        q+=4;
        p+=4;
        }
      q[0]=((p[0-4]-r)*s+(r<<8)+127)>>8;
      q[1]=((p[1-4]-g)*s+(g<<8)+127)>>8;
      q[2]=((p[2-4]-b)*s+(b<<8)+127)>>8;
      q[3]=((p[3-4]-a)*s+(a<<8)+127)>>8;
      q+=4;
      while(q<qq){
        q[0]=r;
        q[1]=g;
        q[2]=b;
        q[3]=a;
        q+=4;
        }
      }
    while(pp!=ppp);
    }
  else{
    memcpy(out,in,owidth*height*4);
    }
  }


// Shear in Y
static void sheary(FXuchar *out,FXuchar* in,FXint width,FXint nheight,FXint oheight,FXint shear,FXColor clr){
  register FXuchar *ppp,*pp,*qq,*p,*q,*k;
  register FXuint r=FXREDVAL(clr);
  register FXuint g=FXGREENVAL(clr);
  register FXuint b=FXBLUEVAL(clr);
  register FXuint a=FXALPHAVAL(clr);
  register FXint dp=width<<2;
  register FXint s,z,x,d;
  if(shear){
    if(shear>0){ x=width-1; d=-1; } else { shear=-shear; x=0; d=1; }
    pp=in+dp*oheight;
    ppp=pp+dp;
    qq=out+dp*nheight;
    do{
      p=pp-dp*oheight;
      q=qq-dp*nheight;
      z=(x*shear-1)/(width-1); x+=d;
      s=z&255;
      k=q+(z>>8)*dp;
      while(q<k){
        q[0]=r;
        q[1]=g;
        q[2]=b;
        q[3]=a;
        q+=dp;
        }
      q[0]=((r-p[0])*s+(p[0]<<8)+127)>>8;
      q[1]=((g-p[1])*s+(p[1]<<8)+127)>>8;
      q[2]=((b-p[2])*s+(p[2]<<8)+127)>>8;
      q[3]=((a-p[3])*s+(p[3]<<8)+127)>>8;
      q+=dp;
      p+=dp;
      while(p<pp){
        q[0]=((p[0-dp]-p[0])*s+(p[0]<<8)+127)>>8;
        q[1]=((p[1-dp]-p[1])*s+(p[1]<<8)+127)>>8;
        q[2]=((p[2-dp]-p[2])*s+(p[2]<<8)+127)>>8;
        q[3]=((p[3-dp]-p[3])*s+(p[3]<<8)+127)>>8;
        q+=dp;
        p+=dp;
        }
      q[0]=((p[0-dp]-r)*s+(r<<8)+127)>>8;
      q[1]=((p[1-dp]-g)*s+(g<<8)+127)>>8;
      q[2]=((p[2-dp]-b)*s+(b<<8)+127)>>8;
      q[3]=((p[3-dp]-a)*s+(a<<8)+127)>>8;
      q+=dp;
      while(q<qq){
        q[0]=r;
        q[1]=g;
        q[2]=b;
        q[3]=a;
        q+=dp;
        }
      pp+=4;
      qq+=4;
      }
    while(pp!=ppp);
    }
  else{
    memcpy(out,in,width*oheight*4);
    }
  }


// Rotate image by degrees ccw
void FXImage::rotate(FXint degrees){
  FXTRACE((100,"%s::rotate(%d)\n",getClassName(),degrees));
  degrees=(degrees+360)%360;
  if(degrees!=0 && width>1 && height>1){
    if(data){
      register FXColor *paa,*pbb,*end,*pa,*pb;
      register FXint size=width*height;
      FXColor *olddata;
      if(!FXMEMDUP(&olddata,data,FXColor,size)){ throw FXMemoryException("unable to rotate image"); }
      switch(degrees){
        case 90:
          resize(height,width);
          paa=data;
          pbb=olddata+(height-1);
          end=data+size;
          do{
            pa=paa; paa+=width;
            pb=pbb; pbb-=1;
            do{
              *pa=*pb;
              pa+=1;
              pb+=height;
              }
            while(pa<paa);
            }
          while(paa<end);
          break;
        case 180:
          paa=data;
          pbb=olddata+size;
          end=data+size;
          do{
            pa=paa; paa+=width;
            pb=pbb; pbb-=width;
            do{
              pb-=1;
              *pa=*pb;
              pa+=1;
              }
            while(pa<paa);
            }
          while(paa<end);
          break;
        case 270:
          resize(height,width);
          paa=data;
          pbb=olddata+height*(width-1);
          end=data+size;
          do{
            pa=paa; paa+=width;
            pb=pbb; pbb+=1;
            do{
              *pa=*pb;
              pa+=1;
              pb-=height;
              }
            while(pa<paa);
            }
          while(paa<end);
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


// Crop image to given rectangle; must have at least one pixel overlap.
void FXImage::crop(FXint x,FXint y,FXint w,FXint h,FXColor color){
  if(w<1) w=1;
  if(h<1) h=1;
  if(x>=width || y>=height || x+w<=0 || y+h<=0){ fxerror("%s::crop: bad arguments.\n",getClassName()); }
  FXTRACE((100,"%s::crop(%d,%d,%d,%d)\n",getClassName(),x,y,w,h));
  if(data){
    register FXColor *pnn,*poo,*yyy,*pn,*po,*xx;
    register FXint ow=width;
    register FXint oh=height;
    register FXint nw=w;
    register FXint nh=h;
    register FXint cw;
    register FXint ch;
    FXColor *olddata;
    if(!FXMEMDUP(&olddata,data,FXColor,width*height)){ throw FXMemoryException("unable to crop image"); }
    resize(nw,nh);
    pnn=data;
    yyy=data+nw*nh;
    do{
      *pnn++=color;
      }
    while(pnn<yyy);
    if(x<0){
      cw=FXMIN(ow,x+nw);
      if(y<0){
        pnn=data-nw*y;
        poo=olddata;
        ch=FXMIN(oh,y+nh);
        }
      else{
        pnn=data;
        poo=olddata+ow*y;
        ch=FXMIN(oh,y+nh)-y;
        }
      pnn-=x;
      }
    else{
      cw=FXMIN(ow,x+nw)-x;
      if(y<0){
        pnn=data-nw*y;
        poo=olddata;
        ch=FXMIN(oh,y+nh);
        }
      else{
        pnn=data;
        poo=olddata+ow*y;
        ch=FXMIN(oh,y+nh)-y;
        }
      poo+=x;
      }
    FXASSERT(cw>0);
    FXASSERT(ch>0);
    yyy=pnn+nw*ch;
    do{
      pn=pnn;
      po=poo;
      xx=pnn+cw;
      do{
        *pn++=*po++;
        }
      while(pn<xx);
      pnn+=nw;
      poo+=ow;
      }
    while(pnn<yyy);
    FXFREE(&olddata);
    render();
    }
  else{
    resize(w,h);
    }
  }


// Shear image horizontally
void FXImage::xshear(FXint shear,FXColor clr){
  FXint neww=width+((FXABS(shear)+255)>>8);
  FXint oldw=width;
  FXTRACE((100,"%s::xshear(%d)\n",getClassName(),shear));
  if(data){
    FXColor *olddata;
    if(!FXMEMDUP(&olddata,data,FXColor,width*height)){ throw FXMemoryException("unable to xshear image"); }
    resize(neww,height);
    shearx((FXuchar*)data,(FXuchar*)olddata,neww,oldw,height,shear,clr);
    FXFREE(&olddata);
    render();
    }
  else{
    resize(neww,height);
    }
  }


// Shear image vertically
void FXImage::yshear(FXint shear,FXColor clr){
  FXint newh=height+((FXABS(shear)+255)>>8);
  FXint oldh=height;
  FXTRACE((100,"%s::yshear(%d)\n",getClassName(),shear));
  if(data){
    FXColor *olddata;
    if(!FXMEMDUP(&olddata,data,FXColor,width*height)){ throw FXMemoryException("unable to yshear image"); }
    resize(width,newh);
    sheary((FXuchar*)data,(FXuchar*)olddata,width,newh,oldh,shear,clr);
    FXFREE(&olddata);
    render();
    }
  else{
    resize(width,newh);
    }
  }


/*


// Fill with diagonal gradient RGB
static void dgradientrgba(FXuchar *dst,FXint w,FXint h,FXint r1,FXint g1,FXint b1,FXint a1,FXint r2,FXint g2,FXint b2,FXint a2){
  register FXint rr,gg,bb,aa,drx,dgx,dbx,dax,dry,dgy,dby,day,x,y;
  register FXuchar *ptr=dst;
  FXuchar xtable[4][2048];
  FXuchar ytable[4][2048];
  FXASSERT(w>0 && h>0);
  FXASSERT(w<2048 && h<2048);
  drx=dry=((r2-r1)<<16);
  dgx=dgy=((g2-g1)<<16);
  dbx=dby=((b2-b1)<<16);
  dax=day=((a2-a1)<<16);
  rr=(r1<<16)+32768;
  gg=(g1<<16)+32768;
  bb=(b1<<16)+32768;
  aa=(a1<<16)+32768;
  drx/=(w-1)*2;
  dgx/=(w-1)*2;
  dbx/=(w-1)*2;
  dax/=(w-1)*2;
  x=w;
  do{
    --x;
    xtable[0][x]=rr>>16; rr+=drx;
    xtable[1][x]=gg>>16; gg+=dgx;
    xtable[2][x]=bb>>16; bb+=dbx;
    xtable[3][x]=aa>>16; aa+=dax;
    }
  while(x);
  rr=32768;
  gg=32768;
  bb=32768;
  aa=32768;
  dry/=(h-1)*2;
  dgy/=(h-1)*2;
  dby/=(h-1)*2;
  day/=(h-1)*2;
  y=h;
  do{
    --y;
    ytable[0][y]=rr>>16; rr+=dry;
    ytable[1][y]=gg>>16; gg+=dgy;
    ytable[2][y]=bb>>16; bb+=dby;
    ytable[3][y]=aa>>16; aa+=day;
    }
  while(y);
  y=h;
  do{
    --y;
    x=w;
    do{
      --x;
      ptr[0]=xtable[0][x]+ytable[0][y];
      ptr[1]=xtable[1][x]+ytable[1][y];
      ptr[2]=xtable[2][x]+ytable[2][y];
      ptr[3]=xtable[3][x]+ytable[3][y];
      ptr+=4;
      }
    while(x);
    }
  while(y);
  }
*/


// Fill horizontal gradient
void FXImage::hgradient(FXColor left,FXColor right){
  register FXint rr,gg,bb,aa,dr,dg,db,da,r1,g1,b1,a1,r2,g2,b2,a2,x;
  register FXuchar *ptr=(FXuchar*)data;
  register FXuchar *prv=(FXuchar*)data;
  if(ptr && width>1 && height>1){
    r1=FXREDVAL(left);
    r2=FXREDVAL(right);
    rr=(r1<<16)+32768;
    dr=((r2-r1)<<16)/(width-1);
    g1=FXGREENVAL(left);
    g2=FXGREENVAL(right);
    gg=(g1<<16)+32768;
    dg=((g2-g1)<<16)/(width-1);
    b1=FXBLUEVAL(left);
    b2=FXBLUEVAL(right);
    bb=(b1<<16)+32768;
    db=((b2-b1)<<16)/(width-1);
    a1=FXALPHAVAL(left);
    a2=FXALPHAVAL(right);
    aa=(a1<<16)+32768;
    da=((a2-a1)<<16)/(width-1);
    x=width;
    do{
      ptr[0]=rr>>16; rr+=dr;
      ptr[1]=gg>>16; gg+=dg;
      ptr[2]=bb>>16; bb+=db;
      ptr[3]=aa>>16; aa+=da;
      ptr+=4;
      }
    while(--x);
    x=width*(height-1);
    do{
      ptr[0]=prv[0];
      ptr[1]=prv[1];
      ptr[2]=prv[2];
      ptr[3]=prv[3];
      ptr+=4;
      prv+=4;
      }
    while(--x);
    }
  }


// Fill vertical gradient
void FXImage::vgradient(FXColor top,FXColor bottom){
  register FXint rr,gg,bb,aa,dr,dg,db,da,r1,g1,b1,a1,r2,g2,b2,a2,x,y;
  register FXuchar *ptr=(FXuchar*)data;
  if(ptr && width>1 && height>1){
    r1=FXREDVAL(top);
    r2=FXREDVAL(bottom);
    rr=(r1<<16)+32768;
    dr=((r2-r1)<<16)/(height-1);
    g1=FXGREENVAL(top);
    g2=FXGREENVAL(bottom);
    gg=(g1<<16)+32768;
    dg=((g2-g1)<<16)/(height-1);
    b1=FXBLUEVAL(top);
    b2=FXBLUEVAL(bottom);
    bb=(b1<<16)+32768;
    db=((b2-b1)<<16)/(height-1);
    a1=FXALPHAVAL(top);
    a2=FXALPHAVAL(bottom);
    aa=(a1<<16)+32768;
    da=((a2-a1)<<16)/(height-1);
    y=height;
    do{
      r1=rr>>16; rr+=dr;
      g1=gg>>16; gg+=dg;
      b1=bb>>16; bb+=db;
      a1=aa>>16; aa+=da;
      x=width;
      do{
        ptr[0]=r1;
        ptr[1]=g1;
        ptr[2]=b1;
        ptr[3]=a1;
        ptr+=4;
        }
      while(--x);
      }
    while(--y);
    }
  }


// Fill with gradient
void FXImage::gradient(FXColor topleft,FXColor topright,FXColor bottomleft,FXColor bottomright){
  register FXint rl,gl,bl,al,rr,gr,br,ar,drl,dgl,dbl,dal,drr,dgr,dbr,dar,r,g,b,a,dr,dg,db,da,x,y;
  register FXint rtl,gtl,btl,atl,rtr,gtr,btr,atr,rbl,gbl,bbl,abl,rbr,gbr,bbr,abr;
  register FXuchar *ptr=(FXuchar*)data;
  if(ptr && width>1 && height>1){

    rtl=FXREDVAL(topleft);
    rbl=FXREDVAL(bottomleft);
    rl=(rtl<<16)+32768; drl=((rbl-rtl)<<16)/(height-1);

    gtl=FXGREENVAL(topleft);
    gbl=FXGREENVAL(bottomleft);
    gl=(gtl<<16)+32768; dgl=((gbl-gtl)<<16)/(height-1);

    btl=FXBLUEVAL(topleft);
    bbl=FXBLUEVAL(bottomleft);
    bl=(btl<<16)+32768; dbl=((bbl-btl)<<16)/(height-1);

    rtr=FXREDVAL(topright);
    rbr=FXREDVAL(bottomright);
    rr=(rtr<<16)+32768; drr=((rbr-rtr)<<16)/(height-1);

    gtr=FXGREENVAL(topright);
    gbr=FXGREENVAL(bottomright);
    gr=(gtr<<16)+32768; dgr=((gbr-gtr)<<16)/(height-1);

    btr=FXBLUEVAL(topright);
    bbr=FXBLUEVAL(bottomright);
    br=(btr<<16)+32768; dbr=((bbr-btr)<<16)/(height-1);

    atl=FXALPHAVAL(topleft);
    abl=FXALPHAVAL(bottomleft);
    al=(atl<<16)+32768; dal=((abl-atl)<<16)/(height-1);

    atr=FXALPHAVAL(topright);
    abr=FXALPHAVAL(bottomright);
    ar=(atr<<16)+32768; dar=((abr-atr)<<16)/(height-1);

    y=height;
    do{
      r=rl; dr=(rr-rl)/(width-1);
      g=gl; dg=(gr-gl)/(width-1);
      b=bl; db=(br-bl)/(width-1);
      a=al; da=(ar-al)/(width-1);
      x=width;
      do{
        ptr[0]=r>>16; r+=dr;
        ptr[1]=g>>16; g+=dg;
        ptr[2]=b>>16; b+=db;
        ptr[3]=a>>16; a+=da;
        ptr+=4;
        }
      while(--x);
      rl+=drl;
      gl+=dgl;
      bl+=dbl;
      al+=dal;
      rr+=drr;
      gr+=dgr;
      br+=dbr;
      ar+=dar;
      }
    while(--y);
    }
  }


#ifdef WIN32

// Return the device context; the image already selected into it
FXID FXImage::GetDC() const {
  HDC hdc=::CreateCompatibleDC(NULL);
  SelectObject(hdc,(HBITMAP)xid);
  return hdc;
  }


// Release it (no-op)
int FXImage::ReleaseDC(FXID hdc) const {
  return ::DeleteDC((HDC)hdc);
  }

#endif


// Attach pixel buffer to image, and assume ownership of it if IMAGE_OWNED is passed
void FXImage::setData(FXColor *pix,FXuint opts){

  // Free old data
  if(options&IMAGE_OWNED){ FXFREE(&data); }

  // Only own pixel buffer if one was passed
  if(pix && (opts&IMAGE_OWNED)){
    options|=IMAGE_OWNED;
    }
  else{
    options&=~IMAGE_OWNED;
    }

  // Set the pointer
  data=pix;
  }


// Populate the image with new pixel data
void FXImage::setData(FXColor *pix,FXuint opts,FXint w,FXint h){

  // Free old data
  if(options&IMAGE_OWNED){ FXFREE(&data); }

  // Resize pixmap
  resize(w,h);

  // Only own pixel buffer if one was passed
  if(pix && (opts&IMAGE_OWNED)){
    options|=IMAGE_OWNED;
    }
  else{
    options&=~IMAGE_OWNED;
    }

  // Set the pointer
  data=pix;
  }


// Change options
void FXImage::setOptions(FXuint opts){
  options=(options&~IMAGE_MASK) | (opts&IMAGE_MASK);
  }


// Save pixel data only
bool FXImage::savePixels(FXStream& store) const {
  FXuint size=width*height;
  store.save(data,size);
  return true;
  }


// Load pixel data only
bool FXImage::loadPixels(FXStream& store){
  FXuint size=width*height;
  if(options&IMAGE_OWNED){FXFREE(&data);}
  if(!FXMALLOC(&data,FXColor,size)) return false;
  store.load(data,size);
  options|=IMAGE_OWNED;
  return true;
  }


// Save data
void FXImage::save(FXStream& store) const {
  FXuchar haspixels=(data!=NULL);
  FXDrawable::save(store);
  store << options;
  store << haspixels;
  if(haspixels) savePixels(store);
  }


// Load data
void FXImage::load(FXStream& store){
  FXuchar haspixels;
  FXDrawable::load(store);
  store >> options;
  store >> haspixels;
  if(haspixels) loadPixels(store);
  }


// Clean up
FXImage::~FXImage(){
  FXTRACE((100,"FXImage::~FXImage %p\n",this));
  destroy();
  if(options&IMAGE_OWNED){FXFREE(&data);}
  data=(FXColor*)-1L;
  }

}
