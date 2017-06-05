/********************************************************************************
*                                                                               *
*                         C u r s o r - O b j e c t                             *
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
* $Id: FXCursor.cpp,v 1.62.2.1 2006/06/09 00:50:16 fox Exp $                        *
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
#include "FXCursor.h"
#include "FXException.h"


/*
  Notes:
  - Cursor size should be less than or equal to 32x32; limitation in Windows!
  - Need standard glyph for "invisible" cursor.
  - Keep hotx and hoty INSIDE the cursor glyph!!
  - Thanks Niall Douglas <s_sourceforge@nedprod.com> for the changes for
    alpha-blended cursors.
*/

#define DISPLAY(app)     ((Display*)((app)->display))
#define DARKCOLOR(r,g,b) (((r)+(g)+(b))<382)
#define CURSOR_MASK      (255)


using namespace FX;

/*******************************************************************************/

namespace FX {

extern bool fxloadXBM(FXColor*& data,const FXuchar *pixels,const FXuchar *mask,FXint width,FXint height);


// Standard colors
const FXColor white=FXRGBA(255,255,255,255);
const FXColor black=FXRGBA(0,0,0,255);


// Object implementation
FXIMPLEMENT(FXCursor,FXId,NULL,0)


// Deserialization
FXCursor::FXCursor(){
  data=NULL;
  width=0;
  height=0;
  hotx=0;
  hoty=0;
  options=CURSOR_ARROW;
  }


// Make stock cursor
FXCursor::FXCursor(FXApp* a,FXStockCursor curid):FXId(a){
  FXTRACE((100,"FXCursor::FXCursor %p\n",this));
  data=NULL;
  width=0;
  height=0;
  hotx=0;
  hoty=0;
  options=curid;
  }


// Make cursor from source and mask
FXCursor::FXCursor(FXApp* a,const FXuchar* src,const FXuchar* msk,FXint w,FXint h,FXint hx,FXint hy):FXId(a){
  FXTRACE((100,"FXCursor::FXCursor %p\n",this));
  fxloadXBM(data,src,msk,w,h);
  width=w;
  height=h;
  hotx=FXCLAMP(0,hx,width-1);
  hoty=FXCLAMP(0,hy,height-1);
  options=CURSOR_OWNED;
  }


// Make cursor from FXColor pixels
FXCursor::FXCursor(FXApp* a,const FXColor *pix,FXint w,FXint h,FXint hx,FXint hy):FXId(a){
  FXTRACE((100,"FXCursor::FXCursor %p\n",this));
  data=(FXColor*)pix;
  width=w;
  height=h;
  hotx=FXCLAMP(0,hx,width-1);
  hoty=FXCLAMP(0,hy,height-1);
  options=0;
  }


// Return TRUE if color cursor
bool FXCursor::isColor() const {
  register FXint i;
  if(data){
    for(i=width*height-1; 0<=i; i--){
      if(data[i]!=black && data[i]!=white && FXALPHAVAL(data[i])!=0) return true;
      }
    }
  return false;
  }


#ifdef WIN32

static bool supportsColorCursors(){

  // Try calling GetVersionEx using the OSVERSIONINFOEX structure.
  // If that fails, try using the OSVERSIONINFO structure.
#if defined (__WATCOMC__) || (__DMC__)
  OSVERSIONINFO osvi={sizeof(OSVERSIONINFO)};
#else
  OSVERSIONINFOEX osvi={sizeof(OSVERSIONINFOEX)};
#endif
  if(!GetVersionEx((OSVERSIONINFO*)&osvi)){

    // If OSVERSIONINFOEX doesn't work, try OSVERSIONINFO.
    osvi.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
    if(!GetVersionEx((OSVERSIONINFO*)&osvi)){
      return false; // should not happen
      }
    }
  if(osvi.dwPlatformId==VER_PLATFORM_WIN32_NT){
    if(osvi.dwMajorVersion==5 && osvi.dwMinorVersion>=0 || osvi.dwMajorVersion>5){
      return true;
      }
    }

  return false;
  }


#endif

// Create cursor
void FXCursor::create(){
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));

#ifndef WIN32   // X11

      // Mapping to standard X11 cursors
      const FXuint stock[]={XC_left_ptr,XC_left_ptr,XC_right_ptr,XC_xterm,XC_watch,XC_crosshair,XC_sb_h_double_arrow,XC_sb_v_double_arrow,XC_fleur};

      // Building stock cursor
      if(options&CURSOR_MASK){
        FXTRACE((100,"%s::create: stock cursor\n",getClassName()));
        xid=XCreateFontCursor(DISPLAY(getApp()),stock[options&CURSOR_MASK]);
        }

      // Building custom cursor
      else{

        // Should have data
        if(!data){ fxerror("%s::create: cursor needs pixel data.\n",getClassName()); }

        // Let's hope it's the correct size!
        if(width>32 || height>32){ fxerror("%s::create: cursor exceeds maximum size of 32x32 pixels\n",getClassName()); }

        // We have support for color cursors and its a color cursor
#ifdef HAVE_XCURSOR_H
        if(isColor() && XcursorSupportsARGB(DISPLAY(getApp()))){
          register FXuchar *src,*dst,*end; XcursorImage *image;
          FXTRACE((100,"%s::create: custom color %dx%d cursor\n",getClassName(),width,height));
          image=XcursorImageCreate(width,height);
          image->xhot=hotx;
          image->yhot=hoty;
          dst=(FXuchar*)image->pixels;
          src=(FXuchar*)data;
          end=src+width*height*4;
          do{
#ifndef __APPLE__
            dst[0]=src[2];      // B
            dst[1]=src[1];      // G
            dst[2]=src[0];      // R
            dst[3]=src[3];      // A
#else
            // A bug in Apple's X11 implementation has alpha on
            // the wrong end and BGR wrong way round
            dst[0]=src[3];      // A
            dst[3]=src[2];      // B
            dst[2]=src[1];      // G
            dst[1]=src[0];      // R
#endif   
            dst+=4;
            src+=4;
            }
          while(src<end);
          xid=XcursorImageLoadCursor(DISPLAY(getApp()),image);
          XcursorImageDestroy(image);
          }

        // No support for color cursor or simple black/white cursor
        else{
#endif
          FXuchar shapebits[128],maskbits[128]; XColor color[2]; Pixmap srcpix,mskpix;
          register FXint srcoffset,dstoffset,dstbytes,i,j;
          FXTRACE((100,"%s::create: custom b/w %dx%d cursor\n",getClassName(),width,height));
          color[0].pixel=BlackPixel(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
          color[1].pixel=WhitePixel(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
          color[0].flags=DoRed|DoGreen|DoBlue;
          color[1].flags=DoRed|DoGreen|DoBlue;
          XQueryColors(DISPLAY(getApp()),DefaultColormap(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp()))),color,2);
          memset(shapebits,0,sizeof(shapebits));
          memset(maskbits,0,sizeof(maskbits));
          dstbytes=(width+7)/8;
          srcoffset=dstoffset=0;
          for(j=0; j<height; j++){
            for(i=0; i<width; i++){
              if(((FXuchar*)(data+srcoffset+i))[3]>=128){
                maskbits[dstoffset+(i>>3)]|=(1<<(i&7));
                if(DARKCOLOR(((FXuchar*)(data+srcoffset+i))[0],((FXuchar*)(data+srcoffset+i))[1],((FXuchar*)(data+srcoffset+i))[2])) shapebits[dstoffset+(i>>3)]|=(1<<(i&7));
                }
              }
            srcoffset+=width;
            dstoffset+=dstbytes;
            }
          srcpix=XCreateBitmapFromData(DISPLAY(getApp()),XDefaultRootWindow(DISPLAY(getApp())),(char*)shapebits,width,height);
          if(!srcpix){ throw FXImageException("unable to create cursor"); }
          mskpix=XCreateBitmapFromData(DISPLAY(getApp()),XDefaultRootWindow(DISPLAY(getApp())),(char*)maskbits,width,height);
          if(!mskpix){ throw FXImageException("unable to create cursor"); }
          xid=XCreatePixmapCursor(DISPLAY(getApp()),srcpix,mskpix,&color[0],&color[1],hotx,hoty);
          XFreePixmap(DISPLAY(getApp()),srcpix);
          XFreePixmap(DISPLAY(getApp()),mskpix);
#ifdef HAVE_XCURSOR_H
          }
#endif
        }

#else   // WIN32

      // Mapping to standard WIN32 cursors
      const LPCTSTR stock[]={IDC_ARROW,IDC_ARROW,IDC_ARROW,IDC_IBEAM,IDC_WAIT,IDC_CROSS,IDC_SIZENS,IDC_SIZEWE,IDC_SIZEALL};

      // Building stock cursor
      if(options&CURSOR_MASK){
        FXTRACE((100,"%s::create: stock cursor\n",getClassName()));
        xid=LoadCursor(NULL,stock[options&CURSOR_MASK]);
        }

      // Building custom cursor
      else{

        // Should have data
        if(!data){ fxerror("%s::create: cursor needs pixel data.\n",getClassName()); }

        // Let's hope it's the correct size!
        if(width>32 || height>32){ fxerror("%s::create: cursor exceeds maximum size of 32x32 pixels\n",getClassName()); }

        FXASSERT(GetSystemMetrics(SM_CXCURSOR)==32);
        FXASSERT(GetSystemMetrics(SM_CYCURSOR)==32);

        // We have support for color cursors and its a color cursor
        if(isColor() && supportsColorCursors()){
          const BITMAPV4HEADER bi={sizeof(BITMAPV4HEADER),32,-32,1,32,BI_BITFIELDS,0,0,0,0,0,0x00FF0000,0x0000FF00,0x000000FF,0xFF000000,0,{{0,0,0},{0,0,0},{0,0,0}},0,0,0};
          HBITMAP img,mask;
          ICONINFO ii;
          FXTRACE((100,"%s::create: custom color %dx%d cursor\n",getClassName(),width,height));

          // Make a DIB
          void *imgdata=0;
          HDC hdc=GetDC(NULL);
          img=CreateDIBSection(hdc,(BITMAPINFO*)&bi,DIB_RGB_COLORS,&imgdata,NULL,0);
          ReleaseDC(NULL,hdc);
          if(!img){ throw FXImageException("unable to create cursor"); }

          // Fill in data
          FXuint *imgptr=(FXuint*)imgdata;
          FXColor *srcimgptr=data;
          for(int y=0; y<height; y++){
            for(int x=0; x<width; x++){
              FXColor col=*srcimgptr++;
              *imgptr++=(FXALPHAVAL(col)<<24)|(FXREDVAL(col)<<16)|(FXGREENVAL(col)<<8)|(FXBLUEVAL(col));
              }
            for(int fill=width; fill<32; fill++){
              *imgptr++=0;
              }
            }
          if(height<32) memset(imgptr,0,(32-height)*32);

          // Strawman mask bitmap
          mask=CreateBitmap(32,32,1,1,NULL);
          if(!mask){ throw FXImageException("unable to create cursor"); }

          // Create cursor
          ii.fIcon=FALSE;
          ii.xHotspot=hotx;
          ii.yHotspot=hoty;
          ii.hbmMask=mask;
          ii.hbmColor=img;
          xid=CreateIconIndirect(&ii);

          // No longer needed
          DeleteObject(mask);
          DeleteObject(img);
          }

        // No support for color cursor or simple black/white cursor
        else{
          FXint i,j,srcoffset,dstoffset; FXuchar tmpxor[128],tmpand[128];
          FXTRACE((100,"%s::create: custom b/w %dx%d cursor\n",getClassName(),width,height));
          srcoffset=dstoffset=0;
          memset(tmpand,0xff,sizeof(tmpand));
          memset(tmpxor,0,sizeof(tmpxor));
          for(j=0; j<height; j++){
            for(i=0; i<width; i++){
              if(((FXuchar*)(data+srcoffset+i))[3]>=128){
                tmpand[dstoffset+(i>>3)]&=~(128>>(i&7));
                if(!DARKCOLOR(((FXuchar*)(data+srcoffset+i))[0],((FXuchar*)(data+srcoffset+i))[1],((FXuchar*)(data+srcoffset+i))[2])){
                  tmpxor[dstoffset+(i>>3)]|=(128>>(i&7));
                  }
                }
              }
            srcoffset+=width;
            dstoffset+=4;
            }
          xid=CreateCursor((HINSTANCE)(getApp()->display),hotx,hoty,32,32,tmpand,tmpxor);
          }
        }

#endif

      // Were we successful?
      if(!xid){ throw FXImageException("unable to create cursor"); }

      // Release pixel buffer
      if(!(options&CURSOR_KEEP)) release();
      }
    }
  }


// Detach cursor
void FXCursor::detach(){
  if(xid){
    FXTRACE((100,"%s::detach %p\n",getClassName(),this));
    xid=0;
    }
  }


// Release pixels buffer if it was owned
void FXCursor::release(){
  if(options&CURSOR_OWNED){
    options&=~CURSOR_OWNED;
    FXFREE(&data);
    }
  data=NULL;
  }


// Destroy cursor
void FXCursor::destroy(){
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::destroy %p\n",getClassName(),this));
#ifndef WIN32

      // Delete cursor
      XFreeCursor(DISPLAY(getApp()),xid);
#else

      // Delete cursor
      DestroyCursor((HCURSOR)xid);

#endif
      }
    xid=0;
    }
  }


// Save pixel data only
bool FXCursor::savePixels(FXStream& store) const {
  FXuint size=width*height;
  store.save(data,size);
  return true;
  }


// Load pixel data only
bool FXCursor::loadPixels(FXStream& store){
  FXuint size=width*height;
  if(options&CURSOR_OWNED){FXFREE(&data);}
  if(!FXMALLOC(&data,FXColor,size)) return false;
  store.load(data,size);
  options|=CURSOR_OWNED;
  return true;
  }


// Save cursor to stream
void FXCursor::save(FXStream& store) const {
  FXuchar haspixels=(data!=NULL);
  FXId::save(store);
  store << width << height << hotx << hoty;
  store << options;
  store << haspixels;
  if(haspixels) savePixels(store);
  }


// Load cursor from stream
void FXCursor::load(FXStream& store){
  FXuchar haspixels;
  FXId::load(store);
  store >> width >> height >> hotx >> hoty;
  store >> options;
  store >> haspixels;
  if(haspixels) loadPixels(store);
  }


// Clean up
FXCursor::~FXCursor(){
  FXTRACE((100,"FXCursor::~FXCursor %p\n",this));
  destroy();
  if(options&CURSOR_OWNED){FXFREE(&data);}
  data=(FXColor *)-1L;
  }

}
