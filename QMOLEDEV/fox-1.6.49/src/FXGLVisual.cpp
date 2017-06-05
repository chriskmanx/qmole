/********************************************************************************
*                                                                               *
*                            V i s u a l   C l a s s                            *
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
* $Id: FXGLVisual.cpp,v 1.69.2.2 2007/02/22 15:45:56 fox Exp $                      *
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
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXFont.h"
#include "FXGLVisual.h"


/*
  Notes:

  - FXGLVisual builds a visual/pixelformat suitable for GL drawing.

  - Selection of visual/pixelformat is based on a best match to a
    given set of hints, according to some heuristics:

    1) Prefer color depth close to asked ones; really like to
       get a bit MORE color, rather than LESS, however.

    2) If we wanted Z-buffer, it is STRONGLY preferred; If there's
       a choice, we prefer more color depth over more Z-depth; if
       we already have more colors than requested, we prefer to meet
       requested Z depth.

    3) If we wanted double buffer, we strongly prefer it over color and
       Z depth, but HAVING a Z-buffer is still more important.

    4) If we wanted alpha buffer, it is preferred, but Z-buffering
       and double buffering are considered more important.
       If there's a choice, we prefer to receive a few MORE bits of
       alpha buffer than we asked for, rather than LESS.

    5) If we wanted stereo, we prefer it, but almost everything except
       the color-, alpha-, and Z-depths are more important.

  - Some further tuning may be desired, but I think this should satisfy
    most cases....

  - Note that as long as OpenGL is in any way supported, you should ALWAYS
    be able to get at least some visual/pixelformat you can draw on.

  - As far as hardware acceleration goes, H/W acceleration should be
    enabled, possibly at the expense of color-, alpha-, and Z-depth;
    but NEVER at the expense of presence or absence of a requested feature.
    We only drop FEATURES which are requested if there is neither hardware
    nor software support.

    For example, we may trade in some Z-depth, but not the entire Z-buffer,
    to get a hardware accelerated visual/pixelformat.

  - SGI Infinite Reality may have up to 12 bits for red, green, blue each!
*/


#define DISPLAY(app) ((Display*)((app)->display))

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLVisual,FXVisual,NULL,0)



// Deserialization
FXGLVisual::FXGLVisual(){
  }


// Construct
FXGLVisual::FXGLVisual(FXApp* a,FXuint flgs):FXVisual(a,flgs){
  FXTRACE((100,"FXGLVisual::FXGLVisual %p\n",this));
  redSize=8;
  greenSize=8;
  blueSize=8;
  alphaSize=0;
  depthSize=24;
  stencilSize=0;
  accumRedSize=0;
  accumGreenSize=0;
  accumBlueSize=0;
  accumAlphaSize=0;
  }


/*******************************************************************************/

#ifdef WIN32

// System colors to match against
static FXuchar defSysClr[20][3] = {
    { 0,   0,   0 },
    { 0x80,0,   0 },
    { 0,   0x80,0 },
    { 0x80,0x80,0 },
    { 0,   0,   0x80 },
    { 0x80,0,   0x80 },
    { 0,   0x80,0x80 },
    { 0xC0,0xC0,0xC0 },

    { 192, 220, 192 },
    { 166, 202, 240 },
    { 255, 251, 240 },
    { 160, 160, 164 },

    { 0x80,0x80,0x80 },
    { 0xFF,0,   0 },
    { 0,   0xFF,0 },
    { 0xFF,0xFF,0 },
    { 0,   0,   0xFF },
    { 0xFF,0,   0xFF },
    { 0,   0xFF,0xFF },
    { 0xFF,0xFF,0xFF }
    };

static int defaultOverride[13] = {
  0, 3, 24, 27, 64, 67, 88, 173, 181, 236, 247, 164, 91
  };


// Make palette
static HPALETTE makeOpenGLPalette(PIXELFORMATDESCRIPTOR* info){
  int n,i,j,rr,gg,bb;
  int rmax,gmax,bmax;
  LOGPALETTE *pal;
  HPALETTE hPalette;

  // Size of palette array
  n=1<<((PIXELFORMATDESCRIPTOR*)info)->cColorBits;

  // Allocate palette array
  FXMALLOC(&pal,char,sizeof(LOGPALETTE)+sizeof(PALETTEENTRY)*n);
  pal->palVersion = 0x300;
  pal->palNumEntries = n;

  // Maximum values each color
  rmax=(1 << info->cRedBits)-1;
  gmax=(1 << info->cGreenBits)-1;
  bmax=(1 << info->cBlueBits)-1;

  // Build palette
  for(rr=0; rr<=rmax; rr++){
    for(gg=0; gg<=gmax; gg++){
      for(bb=0; bb<=bmax; bb++){
        i = (rr << info->cRedShift) | (gg << info->cGreenShift) | (bb << info->cBlueShift);
        pal->palPalEntry[i].peRed = (255*rr)/rmax;
        pal->palPalEntry[i].peGreen = (255*gg)/gmax;
        pal->palPalEntry[i].peBlue = (255*bb)/bmax;
        pal->palPalEntry[i].peFlags = PC_NOCOLLAPSE;
        }
      }
    }

  // For 8-bit palette
  if((info->cColorBits==8) && (info->cRedBits==3) && (info->cRedShift==0) && (info->cGreenBits==3) && (info->cGreenShift==3) && (info->cBlueBits==2) && (info->cBlueShift==6)){
    for(j=1; j<=12; j++){
      pal->palPalEntry[defaultOverride[j]].peRed=defSysClr[j][0];
      pal->palPalEntry[defaultOverride[j]].peGreen=defSysClr[j][1];
      pal->palPalEntry[defaultOverride[j]].peBlue=defSysClr[j][1];
      pal->palPalEntry[defaultOverride[j]].peFlags=0;
      }
    }

  // Make palette
  hPalette=CreatePalette(pal);

  // Free palette array
  FXFREE(&pal);

  return hPalette;
  }


#endif



/*******************************************************************************/


// Test if OpenGL is possible
FXbool FXGLVisual::supported(FXApp* application,int& major,int& minor){
  major=minor=0;
  if(application->isInitialized()){
#ifdef HAVE_GL_H
#ifndef WIN32
    if(!glXQueryExtension(DISPLAY(application),NULL,NULL)) return FALSE;
    if(!glXQueryVersion(DISPLAY(application),&major,&minor)) return FALSE;
#if GL_VERSION_1_5
    if(minor>5) minor=5;
#elif GL_VERSION_1_4
    if(minor>4) minor=4;
#elif GL_VERSION_1_3
    if(minor>3) minor=3;
#elif GL_VERSION_1_2
    if(minor>2) minor=2;
#elif GL_VERSION_1_1
    if(minor>1) minor=1;
#else
    if(minor>0) minor=0;
#endif
    return TRUE;
#else
    major=1;
#if GL_VERSION_1_4
    minor=4;
#elif GL_VERSION_1_3
    minor=3;
#elif GL_VERSION_1_2
    minor=2;
#elif GL_VERSION_1_1
    minor=1;
#else
    minor=0;
#endif
    return TRUE;
#endif
#endif
    }
  return FALSE;
  }



#ifndef WIN32                   // UNIX


// Initialize
void FXGLVisual::create(){
#ifdef HAVE_GL_H
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));
      int major,minor;
      int gdblbuf,gllevel,glstereo,glstencil,gldepth,glrgba;
      int glred,glgreen,glblue,glalpha;
      int glaccred,glaccgreen,glaccblue,glaccalpha;
      int glsupport;
      int bestvis,dmatch,bestmatch;
      int dred,dgreen,dblue,ddepth,dalpha,dstencil;
      int daccred,daccgreen,daccblue,daccalpha;
      XVisualInfo vitemplate;
      XVisualInfo *vi;
      int nvi,i;

      // OpenGL is available if we're talking to an OpenGL-capable X-Server
      if(!glXQueryExtension(DISPLAY(getApp()),NULL,NULL)){
        fxerror("%s::create: requested OpenGL extension not available.\n",getClassName());
        }

      // Try get OpenGL version info
      if(!glXQueryVersion(DISPLAY(getApp()),&major,&minor)){
        fxerror("%s::create: Unable to obtain OpenGL version numbers.\n",getClassName());
        }

/*
#if defined(GLX_VERSION_1_3)
      const char *glxexts="";
      glxexts=glXQueryExtensionsString(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
      GLXFBConfig *fbconfigs;
      int nfbconfigs,value;
      fbconfigs=glXGetFBConfigs(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())),&nfbconfigs);
      FXTRACE((1,"nfbconfigs: %d\n",nfbconfigs));
      for(int i=0; i<nfbconfigs; i++){
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_FBCONFIG_ID,&value);
        FXTRACE((1,"GLX_FBCONFIG_ID=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_RENDER_TYPE,&value);
        FXTRACE((1,"GLX_RENDER_TYPE=%s %s\n",(value&GLX_RGBA_BIT)?"GLX_RGBA_BIT":"",(value&GLX_COLOR_INDEX_BIT)?"GLX_COLOR_INDEX_BIT":""));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_DRAWABLE_TYPE,&value);
        FXTRACE((1,"GLX_DRAWABLE_TYPE=%s %s %s\n",(value&GLX_WINDOW_BIT)?"GLX_WINDOW_BIT":"",(value&GLX_PIXMAP_BIT)?"GLX_PIXMAP_BIT":"",(value&GLX_PBUFFER_BIT)?"GLX_PBUFFER_BIT":""));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_X_RENDERABLE,&value);
        FXTRACE((1,"GLX_X_RENDERABLE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_X_VISUAL_TYPE,&value);
        FXTRACE((1,"GLX_X_VISUAL_TYPE=0x%04x\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_VISUAL_ID,&value);
        FXTRACE((1,"GLX_VISUAL_ID=0x%02x\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_RED_SIZE,&value);
        FXTRACE((1,"GLX_RED_SIZE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_GREEN_SIZE,&value);
        FXTRACE((1,"GLX_GREEN_SIZE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_BLUE_SIZE,&value);
        FXTRACE((1,"GLX_BLUE_SIZE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_ALPHA_SIZE,&value);
        FXTRACE((1,"GLX_ALPHA_SIZE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_DEPTH_SIZE,&value);
        FXTRACE((1,"GLX_DEPTH_SIZE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_STENCIL_SIZE,&value);
        FXTRACE((1,"GLX_STENCIL_SIZE=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_LEVEL,&value);
        FXTRACE((1,"GLX_LEVEL=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_DOUBLEBUFFER,&value);
        FXTRACE((1,"GLX_DOUBLEBUFFER=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_STEREO,&value);
        FXTRACE((1,"GLX_STEREO=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_MAX_PBUFFER_WIDTH,&value);
        FXTRACE((1,"GLX_MAX_PBUFFER_WIDTH=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_MAX_PBUFFER_HEIGHT,&value);
        FXTRACE((1,"GLX_MAX_PBUFFER_HEIGHT=%d\n",value));
        glXGetFBConfigAttrib(DISPLAY(getApp()),fbconfigs[i],GLX_MAX_PBUFFER_PIXELS,&value);
        FXTRACE((1,"GLX_MAX_PBUFFER_PIXELS=%d\n",value));
        FXTRACE((1,"\n"));
        }
      FXTRACE((1,"glx extensions: %s\n",glxexts));
#endif
*/

      // Scan for all visuals of given screen
      vitemplate.screen=DefaultScreen(DISPLAY(getApp()));
      vi=XGetVisualInfo(DISPLAY(getApp()),VisualScreenMask,&vitemplate,&nvi);
      if(!vi){ fxerror("%s::create: unable to obtain any visuals.\n",getClassName()); }

      FXTRACE((150,"Found OpenGL version %d.%d; %d visuals\n",major,minor,nvi));

      // Try to find the best
      bestvis=-1;
      bestmatch=1000000000;

      for(i=0; i<nvi; i++){

        // GL RGBA Support is requested
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_USE_GL,&glsupport);
        if(!glsupport) continue;

        // Don't care about index color mode
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_RGBA,&glrgba);
        if(!glrgba) continue;

        // Don't care for overlays
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_LEVEL,&gllevel);
        if(gllevel!=0) continue;

        // Must have Single/Double buffer as requested
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_DOUBLEBUFFER,&gdblbuf);

        // Stereo support as requested
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_STEREO,&glstereo);

        // Get planes
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_RED_SIZE,&glred);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_GREEN_SIZE,&glgreen);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_BLUE_SIZE,&glblue);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_ALPHA_SIZE,&glalpha);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_DEPTH_SIZE,&gldepth);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_STENCIL_SIZE,&glstencil);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_ACCUM_RED_SIZE,&glaccred);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_ACCUM_GREEN_SIZE,&glaccgreen);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_ACCUM_BLUE_SIZE,&glaccblue);
        glXGetConfig(DISPLAY(getApp()),&vi[i],GLX_ACCUM_ALPHA_SIZE,&glaccalpha);

        // We prefer to get a few MORE bits in RGBA than we asked for
        dred   = glred-redSize;     if(dred<0)   dred   *= -100;
        dgreen = glgreen-greenSize; if(dgreen<0) dgreen *= -100;
        dblue  = glblue-blueSize;   if(dblue<0)  dblue  *= -100;
        dalpha = glalpha-alphaSize; if(dalpha<0) dalpha *= -100;

        // Prefer better Z than asked, but colors more important
        ddepth = gldepth-depthSize; if(ddepth<0) ddepth *= -10;

        // We care about colors and Z depth more than stencil depth
        dstencil = glstencil-stencilSize; if(dstencil<0) dstencil *= -1;

        // Accumulation buffers
        daccred=glaccred-accumRedSize;       if(daccred<0)   daccred   *= -1;
        daccgreen=glaccgreen-accumGreenSize; if(daccgreen<0) daccgreen *= -1;
        daccblue=glaccblue-accumBlueSize;    if(daccblue<0)  daccblue  *= -1;
        daccalpha=glaccalpha-accumAlphaSize; if(daccalpha<0) daccalpha *= -1;

        // Want the best colors, of course
        dmatch=dred+dgreen+dblue+dalpha;

        // Accumulation buffers
        dmatch+=daccred+daccgreen+daccblue+daccalpha;

        // Extra penalty for no alpha if we asked for alpha, but no
        // penalty at all if there is alpha and we didn't ask for it.
        if(alphaSize>0){
          if(glalpha<1) dmatch+=100000;
          }

        // Wanted Z-buffer
        if(depthSize>0){
          if(gldepth<1) dmatch+=10000000;
          else dmatch+=ddepth;
          }
        else{
          if(gldepth>0) dmatch+=10000000;
          }

        // Double buffering also quite strongly preferred
        if(flags&VISUAL_DOUBLEBUFFER){
          if(!gdblbuf) dmatch+=1000000;
          }
        else{
          if(gdblbuf) dmatch+=1000000;
          }

        // Stencil buffers desired
        if(stencilSize>0){
          if(glstencil<1) dmatch+=10000;
          else dmatch+=dstencil;
          }
        else{
          if(glstencil>0) dmatch+=1;
          }

        // Stereo not so important
        if(flags&VISUAL_STEREO){
          if(!glstereo) dmatch+=10000;
          }
        else{
          if(glstereo) dmatch+=10000;
          }

        // Trace
        FXTRACE((150,"Visual 0x%02x (%d) match value = %d\n",(FXuint)vi[i].visualid,i,dmatch));
        FXTRACE((150,"  red size   = %d\n",glred));
        FXTRACE((150,"  green size = %d\n",glgreen));
        FXTRACE((150,"  blue size  = %d\n",glblue));
        FXTRACE((150,"  alpha size = %d\n",glalpha));
        FXTRACE((150,"  depth size = %d\n",gldepth));
        FXTRACE((150,"  double buf = %d\n",gdblbuf));
        FXTRACE((150,"  stencil    = %d\n",glstencil));
        FXTRACE((150,"  acc red    = %d\n",glaccred));
        FXTRACE((150,"  acc green  = %d\n",glaccgreen));
        FXTRACE((150,"  acc blue   = %d\n",glaccblue));
        FXTRACE((150,"  acc alpha  = %d\n",glaccalpha));
        FXTRACE((150,"  stereo     = %d\n",glstereo));

        // May the best visual win
        if(dmatch<=bestmatch){

          // All other things being equal, we prefer default visual!
          if(dmatch<bestmatch || vi[i].visual==DefaultVisual(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())))){
            bestmatch=dmatch;
            bestvis=i;
            }
          }
        }

      // Shit out of luck
      if(bestvis<0){ fxerror("%s::create: requested OpenGL visual unavailable.\n",getClassName()); }

      // Report best visual
      FXTRACE((150,"Best Visual 0x%02x match value = %d\n",(FXuint)vi[bestvis].visualid,bestmatch));

      // Get visual, depth
      visual=vi[bestvis].visual;
      depth=vi[bestvis].depth;

      // Keep into for later
      FXMALLOC(&info,XVisualInfo,1);
      memcpy(info,&vi[bestvis],sizeof(XVisualInfo));

      // Free stuff
      XFree((char*)vi);

      // We absolutely should have a visual now
      FXASSERT(visual);

      // Initialize colormap
      setupcolormap();

      // Make GC's for this visual
      gc=setupgc(FALSE);
      scrollgc=setupgc(TRUE);

      xid=1;

      // Report what we got if tracing enabled
      FXTRACE((150,"  redSize:        %d\n",getActualRedSize()));
      FXTRACE((150,"  greenSize:      %d\n",getActualGreenSize()));
      FXTRACE((150,"  blueSize:       %d\n",getActualBlueSize()));
      FXTRACE((150,"  alphaSize:      %d\n",getActualAlphaSize()));
      FXTRACE((150,"  depthSize:      %d\n",getActualDepthSize()));
      FXTRACE((150,"  stencilSize:    %d\n",getActualStencilSize()));
      FXTRACE((150,"  accumRedSize:   %d\n",getActualAccumRedSize()));
      FXTRACE((150,"  accumGreenSize: %d\n",getActualAccumGreenSize()));
      FXTRACE((150,"  accumBlueSize:  %d\n",getActualAccumBlueSize()));
      FXTRACE((150,"  accumAlphaSize: %d\n",getActualAccumAlphaSize()));
      FXTRACE((150,"  doubleBuffer:   %d\n",isDoubleBuffer()));
      FXTRACE((150,"  stereo:         %d\n",isStereo()));
      FXTRACE((150,"  accelerated:    %d\n",isAccelerated()));
      }
    }
#endif
  }


#else                   // Windows


// Initialize
void FXGLVisual::create(){
#ifdef HAVE_GL_H
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));
      PIXELFORMATDESCRIPTOR pfd;
      HDC hdc;
      int gdblbuf,glstereo,gldepth,glaccel;
      int glred,glgreen,glblue,glalpha,glstencil;
      int glaccred,glaccgreen,glaccblue,glaccalpha;
      int glindex,glswapcopy,glcomposition;
      int bestvis,dmatch,bestmatch;
      int dred,dgreen,dblue,ddepth,dalpha,dstencil;
      int daccred,daccgreen,daccblue,daccalpha;
      int chosen,nvi,i;

      // Get some window handle
      hdc=GetDC(GetDesktopWindow());

      // Get number of supported pixel formats
      pfd.nSize=sizeof(PIXELFORMATDESCRIPTOR);
      pfd.nVersion=1;
      nvi=DescribePixelFormat(hdc,1,sizeof(PIXELFORMATDESCRIPTOR),&pfd);
      if(nvi==0){ fxerror("%s::create: no OpenGL visual available.\n",getClassName()); }

      // Try to find the best
      bestvis=-1;
      bestmatch=1000000000;

      for(i=1; i<=nvi; i++) {

        // Get info about this visual
        DescribePixelFormat(hdc,i,sizeof(PIXELFORMATDESCRIPTOR),&pfd);

        // Make sure this visual is valid
        chosen=ChoosePixelFormat(hdc,&pfd);
        if(chosen!=i) continue;

        // Draw to window is required
        if(!(pfd.dwFlags&PFD_DRAW_TO_WINDOW)) continue;

        // OpenGL support is required
        if(!(pfd.dwFlags&PFD_SUPPORT_OPENGL)) continue;

        // RGBA support is required
        if(pfd.iPixelType!=PFD_TYPE_RGBA) continue;

        // Skip accelerated formats if VISUAL_NOACCEL specified
        if((flags&VISUAL_NOACCEL) && !(pfd.dwFlags&PFD_GENERIC_FORMAT)) continue;

        // Is this a hardware-accelerated visual?
        glaccel=(pfd.dwFlags&PFD_GENERIC_FORMAT)==0;

        // Must have single/double buffer as requested
        gdblbuf=(pfd.dwFlags&PFD_DOUBLEBUFFER)!=0;

        // Stereo support as requested
        glstereo=(pfd.dwFlags&PFD_STEREO)!=0;

        // Must have RGB as requested
        glindex=(pfd.iPixelType&PFD_TYPE_COLORINDEX)!=0;

        // copy swap buffer
        glswapcopy=(pfd.dwFlags&PFD_SWAP_COPY)!=0;

#ifndef PFD_SUPPORT_COMPOSITION
#define PFD_SUPPORT_COMPOSITION 0x00008000
#endif
        // Windows Vista and Windows 7 composition support
        glcomposition=(pfd.dwFlags&PFD_SUPPORT_COMPOSITION)!=0;

        // Get planes
        glred=pfd.cRedBits;
        glgreen=pfd.cGreenBits;
        glblue=pfd.cBlueBits;
        glalpha=pfd.cAlphaBits;
        gldepth=pfd.cDepthBits;
        glstencil=pfd.cStencilBits;
        glaccred=pfd.cAccumRedBits;
        glaccgreen=pfd.cAccumGreenBits;
        glaccblue=pfd.cAccumBlueBits;
        glaccalpha=pfd.cAccumAlphaBits;

        // We prefer to get a few MORE bits in RGBA than we asked for
        dred   = glred-redSize;     if(dred<0)   dred   *= -100;
        dgreen = glgreen-greenSize; if(dgreen<0) dgreen *= -100;
        dblue  = glblue-blueSize;   if(dblue<0)  dblue  *= -100;
        dalpha = glalpha-alphaSize; if(dalpha<0) dalpha *= -100;

        // Prefer better Z than asked, but colors more important
        ddepth = gldepth-depthSize; if(ddepth<0) ddepth *= -10;

        // We care about colors and Z depth more than stencil depth
        dstencil = glstencil-stencilSize; if(dstencil<0) dstencil *= -1;

        // Accumulation buffers
        daccred=glaccred-accumRedSize;       if(daccred<0)   daccred   *= -1;
        daccgreen=glaccgreen-accumGreenSize; if(daccgreen<0) daccgreen *= -1;
        daccblue=glaccblue-accumBlueSize;    if(daccblue<0)  daccblue  *= -1;
        daccalpha=glaccalpha-accumAlphaSize; if(daccalpha<0) daccalpha *= -1;

        // Want the best colors, of course
        dmatch=dred+dgreen+dblue+dalpha;

        // Accumulation buffers
        dmatch+=daccred+daccgreen+daccblue+daccalpha;

        // Hardware accelerated a plus
        if(!glaccel){
          dmatch+=10000;
          }

        // Extra penalty for no alpha if we asked for alpha, but no
        // penalty at all if there is alpha and we didn't ask for it.
        if(alphaSize>0){
          if(glalpha<1) dmatch+=100000;
          }

        // Wanted Z-buffer
        if(depthSize>0){
          if(gldepth<1) dmatch+=100000000;
          else dmatch+=ddepth;
          }
        else{
          if(gldepth>0) dmatch+=100000000;
          }

        // Double buffering also quite strongly preferred
        if(flags&VISUAL_DOUBLEBUFFER){
          if(!gdblbuf) dmatch+=10000000;
          }
        else{
          if(gdblbuf) dmatch+=10000000;
          }

        // Color index mode only if requested
        if(flags&VISUAL_INDEXCOLOR){
          if(!glindex) dmatch+=10000000;
          }
        else{
          if(glindex) dmatch+=10000000;
          }

        // Stencil buffers desired
        if(stencilSize>0){
          if(glstencil<1) dmatch+=10000;
          else dmatch+=dstencil;
          }
        else{
          if(glstencil>0) dmatch+=1;
          }

        // Stereo not so important
        if(flags&VISUAL_STEREO){
          if(!glstereo) dmatch+=100000;
          }
        else{
          if(glstereo) dmatch+=100000;
          }

        // Swap copies also important
        if(flags&VISUAL_SWAP_COPY){
          if(!glswapcopy) dmatch+=10000000;
          }

        // Composition Support would be nice to have
        if(!glcomposition) dmatch+=100;

        // Trace
        FXTRACE((150,"Pixel Format (%d) match value = %d\n",i,dmatch));
        FXTRACE((150,"  red size    = %d\n",glred));
        FXTRACE((150,"  green size  = %d\n",glgreen));
        FXTRACE((150,"  blue size   = %d\n",glblue));
        FXTRACE((150,"  alpha size  = %d\n",glalpha));
        FXTRACE((150,"  depth size  = %d\n",gldepth));
        FXTRACE((150,"  double buf  = %d\n",gdblbuf));
        FXTRACE((150,"  stencil     = %d\n",glstencil));
        FXTRACE((150,"  acc red     = %d\n",glaccred));
        FXTRACE((150,"  acc green   = %d\n",glaccgreen));
        FXTRACE((150,"  acc blue    = %d\n",glaccblue));
        FXTRACE((150,"  acc alpha   = %d\n",glaccalpha));
        FXTRACE((150,"  stereo      = %d\n",glstereo));
        FXTRACE((150,"  accelerated = %d\n",glaccel));

        // May the best visual win
        if(dmatch<=bestmatch){
          bestmatch=dmatch;
          bestvis=i;
          }
        }

      // Still no luck
      if(bestvis<0){ fxerror("%s::create: no OpenGL visual available.\n",getClassName()); }

      // Report best visual
      FXTRACE((150,"Best Pixel Format (%d) match value = %d\n",bestvis,bestmatch));

      // Get the true h/w capabilities
      visual=(void*)(FXival)bestvis;
      FXMALLOC(&info,PIXELFORMATDESCRIPTOR,1);
      ((PIXELFORMATDESCRIPTOR*)info)->nSize=sizeof(PIXELFORMATDESCRIPTOR);
      ((PIXELFORMATDESCRIPTOR*)info)->nVersion=1;
      DescribePixelFormat(hdc,bestvis,sizeof(PIXELFORMATDESCRIPTOR),(PIXELFORMATDESCRIPTOR*)info);

      // Make a palette for it if needed
      if(((PIXELFORMATDESCRIPTOR*)info)->dwFlags&PFD_NEED_PALETTE){
        colormap=makeOpenGLPalette((PIXELFORMATDESCRIPTOR*)info);
        freemap=TRUE;
        }

      // Done with that window
      ReleaseDC(GetDesktopWindow(),hdc);

      xid=(void*)1;

      // Report what we got if tracing enabled
      FXTRACE((150,"  redSize:        %d\n",getActualRedSize()));
      FXTRACE((150,"  greenSize:      %d\n",getActualGreenSize()));
      FXTRACE((150,"  blueSize:       %d\n",getActualBlueSize()));
      FXTRACE((150,"  alphaSize:      %d\n",getActualAlphaSize()));
      FXTRACE((150,"  depthSize:      %d\n",getActualDepthSize()));
      FXTRACE((150,"  stencilSize:    %d\n",getActualStencilSize()));
      FXTRACE((150,"  accumRedSize:   %d\n",getActualAccumRedSize()));
      FXTRACE((150,"  accumGreenSize: %d\n",getActualAccumGreenSize()));
      FXTRACE((150,"  accumBlueSize:  %d\n",getActualAccumBlueSize()));
      FXTRACE((150,"  accumAlphaSize: %d\n",getActualAccumAlphaSize()));
      FXTRACE((150,"  doubleBuffer:   %d\n",isDoubleBuffer()));
      FXTRACE((150,"  stereo:         %d\n",isStereo()));
      FXTRACE((150,"  accelerated:    %d\n",isAccelerated()));
      }
    }
#endif
  }


#endif


// Detach visual
void FXGLVisual::detach(){
#ifdef HAVE_GL_H
  if(xid){
    FXTRACE((100,"%s::detach %p\n",getClassName(),this));
    colormap=0;
    freemap=FALSE;
    xid=0;
    }
#endif
  }


// Destroy visual
void FXGLVisual::destroy(){
#ifdef HAVE_GL_H
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::destroy %p\n",getClassName(),this));
#ifndef WIN32
      if(freemap){XFreeColormap(DISPLAY(getApp()),colormap);}   // Should we free?
      XFreeGC(DISPLAY(getApp()),(GC)gc);
      XFreeGC(DISPLAY(getApp()),(GC)scrollgc);
#else
      if(colormap){DeleteObject(colormap);}
#endif
      colormap=0;
      freemap=FALSE;
      }

    // Free info on the 3D layout
    if(info) FXFREE(&info);
    xid=0;
    }
#endif
  }


// Get actual red size
FXint FXGLVisual::getActualRedSize() const {
  if(!info){ fxerror("%s::getActualRedSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_RED_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cRedBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual green size
FXint FXGLVisual::getActualGreenSize() const {
  if(!info){ fxerror("%s::getActualGreenSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_GREEN_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cGreenBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual blue size
FXint FXGLVisual::getActualBlueSize() const {
  if(!info){ fxerror("%s::getActualBlueSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_BLUE_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cBlueBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual alpha size
FXint FXGLVisual::getActualAlphaSize() const {
  if(!info){ fxerror("%s::getActualAlphaSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_ALPHA_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cAlphaBits;
#endif
  return s;
#else
  return 0;
#endif
  }



// Get actual depth size
FXint FXGLVisual::getActualDepthSize() const {
  if(!info){ fxerror("%s::getActualDepthSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_DEPTH_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cDepthBits;
#endif
  return s;
#else
  return 0;
#endif
  }

// Get actual stencil size
FXint FXGLVisual::getActualStencilSize() const {
  if(!info){ fxerror("%s::getActualStencilSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_STENCIL_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cStencilBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual accum red size
FXint FXGLVisual::getActualAccumRedSize() const {
  if(!info){ fxerror("%s::getActualAccumRedSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_ACCUM_RED_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cAccumRedBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual accum green size
FXint FXGLVisual::getActualAccumGreenSize() const {
  if(!info){ fxerror("%s::getActualAccumGreenSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_ACCUM_GREEN_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cAccumGreenBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual accum blue size
FXint FXGLVisual::getActualAccumBlueSize() const {
  if(!info){ fxerror("%s::getActualAccumBlueSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_ACCUM_BLUE_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cAccumBlueBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Get actual accum alpha size
FXint FXGLVisual::getActualAccumAlphaSize() const {
  if(!info){ fxerror("%s::getActualAccumAlphaSize: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_ACCUM_ALPHA_SIZE,&s);
#else
  s=((PIXELFORMATDESCRIPTOR*)info)->cAccumAlphaBits;
#endif
  return s;
#else
  return 0;
#endif
  }


// Is it double buffer
FXbool FXGLVisual::isDoubleBuffer() const {
  if(!info){ fxerror("%s::isDoubleBuffer: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_DOUBLEBUFFER,&s);
#else
  s=(((PIXELFORMATDESCRIPTOR*)info)->dwFlags&PFD_DOUBLEBUFFER)!=0;
#endif
  return s;
#else
  return FALSE;
#endif
  }


// Is it stereo
FXbool FXGLVisual::isStereo() const {
  if(!info){ fxerror("%s::isStereo: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
  FXint s;
#ifndef WIN32
  glXGetConfig(DISPLAY(getApp()),(XVisualInfo*)info,GLX_STEREO,&s);
#else
  s=(((PIXELFORMATDESCRIPTOR*)info)->dwFlags&PFD_STEREO)!=0;
#endif
  return s;
#else
  return FALSE;
#endif
  }


// Is it hardware-accelerated?
FXbool FXGLVisual::isAccelerated() const {
  if(!info){ fxerror("%s::isHardwareAccelerated: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
#ifndef WIN32
  return TRUE;
#else
  return (((PIXELFORMATDESCRIPTOR*)info)->dwFlags&PFD_GENERIC_FORMAT)==0;
#endif
#else
  return FALSE;
#endif
  }


// Does it swap by copying instead of flipping buffers
FXbool FXGLVisual::isBufferSwapCopy() const {
 if(!info){ fxerror("%s::isBufferSwapCopy: visual not yet initialized.\n",getClassName()); }
#ifdef HAVE_GL_H
#ifndef WIN32
  return FALSE;
#else
  return (((PIXELFORMATDESCRIPTOR*)info)->dwFlags&PFD_SWAP_COPY)!=0;
#endif
#else
  return FALSE;
#endif
  }


// Save to stream
void FXGLVisual::save(FXStream& store) const {
  FXVisual::save(store);
  store << redSize;
  store << greenSize;
  store << blueSize;
  store << alphaSize;
  store << depthSize;
  store << stencilSize;
  store << accumRedSize;
  store << accumGreenSize;
  store << accumBlueSize;
  store << accumAlphaSize;
  }


// Load from stream
void FXGLVisual::load(FXStream& store){
  FXVisual::load(store);
  store >> redSize;
  store >> greenSize;
  store >> blueSize;
  store >> alphaSize;
  store >> depthSize;
  store >> stencilSize;
  store >> accumRedSize;
  store >> accumGreenSize;
  store >> accumBlueSize;
  store >> accumAlphaSize;
  }


// Destroy
FXGLVisual::~FXGLVisual(){
  FXTRACE((100,"FXGLVisual::~FXGLVisual %p\n",this));
  destroy();
  if(info) FXFREE(&info);
  }


/*******************************************************************************/


#if defined(HAVE_XFT_H) && defined(HAVE_GL_H)

// Xft version
static void glXUseXftFont(XftFont* font,int first,int count,int listBase){
  GLint swapbytes,lsbfirst,rowlength,skiprows,skippixels,alignment,list;
  GLfloat x0,y0,dx,dy;
  FT_Face face;
  FT_Error err;
  FXint i,size,x,y;
  FXuchar *glyph;

  // Save the current packing mode for bitmaps
  glGetIntegerv(GL_UNPACK_SWAP_BYTES,&swapbytes);
  glGetIntegerv(GL_UNPACK_LSB_FIRST,&lsbfirst);
  glGetIntegerv(GL_UNPACK_ROW_LENGTH,&rowlength);
  glGetIntegerv(GL_UNPACK_SKIP_ROWS,&skiprows);
  glGetIntegerv(GL_UNPACK_SKIP_PIXELS,&skippixels);
  glGetIntegerv(GL_UNPACK_ALIGNMENT,&alignment);

  // Set desired packing modes
  glPixelStorei(GL_UNPACK_SWAP_BYTES,GL_FALSE);
  glPixelStorei(GL_UNPACK_LSB_FIRST,GL_FALSE);
  glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS,0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS,0);
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);

  // Get face info
  face=XftLockFace(font);

  // Render font glyphs; use FreeType to render to bitmap
  for(i=first; i<count; i++){
    list=listBase+i;

    // Load glyph
    err=FT_Load_Glyph(face,FT_Get_Char_Index(face,i),FT_LOAD_DEFAULT);
    if(err){ fxwarning("glXUseXftFont: unable to load glyph.\n"); return; }

    // Render glyph
    err=FT_Render_Glyph(face->glyph,FT_RENDER_MODE_MONO);
    if(err){ fxwarning("glXUseXftFont: unable to render glyph.\n"); return; }

    // Pitch may be negative, its the stride between rows
    size=FXABS(face->glyph->bitmap.pitch) * face->glyph->bitmap.rows;

    // Glyph coordinates; note info in freetype is 6-bit fixed point
    x0=-(face->glyph->metrics.horiBearingX>>6);
    y0=(face->glyph->metrics.height-face->glyph->metrics.horiBearingY)>>6;
    dx=face->glyph->metrics.horiAdvance>>6;
    dy=0;

    // Allocate glyph data
    FXMALLOC(&glyph,FXuchar,size);

    // Copy into OpenGL bitmap format; note OpenGL upside down
    for(y=0; y<face->glyph->bitmap.rows; y++){
      for(x=0; x<face->glyph->bitmap.pitch; x++){
        glyph[y*face->glyph->bitmap.pitch+x]=face->glyph->bitmap.buffer[(face->glyph->bitmap.rows-y-1)*face->glyph->bitmap.pitch+x];
        }
      }

    // Put bitmap into display list
    glNewList(list,GL_COMPILE);
    glBitmap(FXABS(face->glyph->bitmap.pitch)<<3,face->glyph->bitmap.rows,x0,y0,dx,dy,glyph);
    glEndList();

    // Free glyph data
    FXFREE(&glyph);
    }

  // Restore packing modes
  glPixelStorei(GL_UNPACK_SWAP_BYTES,swapbytes);
  glPixelStorei(GL_UNPACK_LSB_FIRST,lsbfirst);
  glPixelStorei(GL_UNPACK_ROW_LENGTH,rowlength);
  glPixelStorei(GL_UNPACK_SKIP_ROWS,skiprows);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS,skippixels);
  glPixelStorei(GL_UNPACK_ALIGNMENT,alignment);

  // Unlock face
  XftUnlockFace(font);
  }


#endif


// Create a display list of bitmaps from font glyphs in a font
void glUseFXFont(FXFont* font,int first,int count,int list){
  if(!font || !font->id()){ fxerror("glUseFXFont: invalid font.\n"); }
  FXTRACE((100,"glUseFXFont: first=%d count=%d list=%d\n",first,count,list));
#ifdef HAVE_GL_H
#ifndef WIN32
#ifdef HAVE_XFT_H                       // Using XFT
  glXUseXftFont((XftFont*)font->id(),first,count,list);
#else                                   // Using XLFD
  glXUseXFont((Font)font->id(),first,count,list);
#endif
#else
  HDC hdc=wglGetCurrentDC();
  HFONT oldfont=(HFONT)SelectObject(hdc,(HFONT)font->id());
// Replace wglUseFontBitmaps() with wglUseFontBitmapsW()
// Change glCallLists() parameter:
//   len=utf2ncs(sbuffer,text.text(),text.length());
//   glCallLists(len,GL_UNSIGNED_SHORT,(GLushort*)sbuffer);
// Figure out better values for "first" and "count".
  FXbool result=wglUseFontBitmaps(hdc,first,count,list);
  SelectObject(hdc,oldfont);
#endif
#endif
  }

}
