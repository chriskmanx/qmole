/********************************************************************************
*                                                                               *
*                        G L  C o n t e x t   C l a s s                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXGLContext.cpp,v 1.38 2006/01/22 17:58:28 fox Exp $                     *
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
#include "FXVisual.h"
#include "FXGLVisual.h"
#include "FXDrawable.h"
#include "FXGLContext.h"


/*
  Notes:
  - GL Context can be shared between GL canvas widgets.
  - OpenGL documentation says that windows can share GL contexts as long as the
    visuals are the same; so we keep a reference to the GLVisual in the context
    so as to compare it to the visual of the window when the GL context is being
    locked on the window.
  - Display lists can be shared between different types of GL contexts; the display
    list has to be kept in the same place for each of the contexts that want to share
    it.
  - It may be not a bad idea to delay the creation of the context until we call begin().
    This might be much more elegant, as we then already know which surface to use, and
    we won't have to create a temporary dummy window.
  - The mental model of FXGLContext is that of FXDCWindow, except that FXGLContext lives
    long, while FXDCWindow gets created/destroyed on an as-needed basis.
  - There should be NO NEED to derive FXGLContext from FXObject; right now we do, for
    serialization, but I think that we shouldn't; because the stuff in here contains
    info which makes little sense to serialize....
  - Should GL context make sure GLVisual has been create()-ed already?
*/

//#define DISPLAY(app) ((Display*)((app)->display))

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXGLContext,FXId,NULL,0)



// Make a GL context
FXGLContext::FXGLContext(FXApp* a,FXGLVisual *vis):
  FXId(a),visual(vis),surface(NULL),ctx(NULL){
  FXTRACE((100,"FXGLContext::FXGLContext %p\n",this));
  sgnext=sgprev=this;
  }


// Make a GL context sharing a display list with another
FXGLContext::FXGLContext(FXApp* a,FXGLVisual *vis,FXGLContext *shared):
  FXId(a),visual(vis),surface(NULL),ctx(NULL){
  FXTRACE((100,"FXGLContext::FXGLContext %p\n",this));
  sgnext=shared;
  sgprev=shared->sgprev;
  shared->sgprev=this;
  sgprev->sgnext=this;
  }


// Return TRUE if it is sharing display lists
FXbool FXGLContext::isShared() const { return sgnext!=this; }


// Create GL context
void FXGLContext::create(){
  ///visual->create();    // Should we call this here?
#ifdef HAVE_GL_H
  register FXGLContext *context;
  register void *sharedctx=NULL;
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"FXGLContext::create %p\n",this));

      // The visual should be OpenGL capable
      if(!visual->getInfo()){ fxerror("FXGLContext::create(): visual unsuitable for OpenGL.\n"); }

      // Find another member of the group which is already created, and get its context
      if(sgnext!=this){
        context=sgnext;
        while(context!=this){
          sharedctx=context->ctx;
          if(sharedctx) break;
          context=context->sgnext;
          }
        }

#ifndef WIN32

      // Make context
      ctx=glXCreateContext((Display*)getApp()->getDisplay(),(XVisualInfo*)visual->getInfo(),(GLXContext)sharedctx,TRUE);
      if(!ctx){ fxerror("FXGLContext::create(): glXCreateContext() failed.\n"); }

      xid=1;
#else

      // Conceptually, the bitplane organization should be enough to create a context;
      // but on Windows, there is no concept like a visual, and hence we need to make
      // a dummy window, set its pixel format, and then create the GL context;
      // afterwards, the window is no longer needed and will be deleted.
      // Yes, I'm painfully aware that this sucks, but we intend to keep the
      // logical model clean come hell or high water....
      HWND wnd=CreateWindow(TEXT("FXPopup"),NULL,WS_CHILD|WS_VISIBLE|WS_CLIPSIBLINGS|WS_CLIPCHILDREN|WS_POPUP,0,0,2,2,GetDesktopWindow(),0,(HINSTANCE)(getApp()->display),this);
      if(!wnd){ fxerror("FXGLContext::create(): CreateWindow() failed.\n"); }

      // Get the window's device context
      HDC hdc=GetDC((HWND)wnd);

      // Set the pixel format
      if(!SetPixelFormat(hdc,(FXint)(FXival)visual->getVisual(),(PIXELFORMATDESCRIPTOR*)visual->getInfo())){
        fxerror("FXGLContext::create(): SetPixelFormat() failed.\n");
        }

      // Make the GL context
      ctx=(void*)wglCreateContext(hdc);
      if(!ctx){ fxerror("FXGLContext::create(): wglCreateContext() failed.\n"); }

      // I hope I didn't get this backward; the new context obviously has no
      // display lists yet, but the old one may have, as it has already been around
      // for a while.  If you see this fail and can't explain why, then that might
      // be what's going on.  Report this to jeroen@fox-toolkit.org
      if(sharedctx && !wglShareLists((HGLRC)sharedctx,(HGLRC)ctx)){ fxerror("FXGLContext::create(): wglShareLists() failed.\n"); }

      // Release window's device context
      ReleaseDC(wnd,hdc);

      // Destroy the temporary window
      DestroyWindow(wnd);

      xid=(void*)1;
#endif
      }
    }
#endif
  }


// Detach the GL context
void FXGLContext::detach(){
#ifdef HAVE_GL_H
  if(xid){
    FXTRACE((100,"FXGLContext::detach %p\n",this));
    xid=0;
    }
#endif
  }


// Destroy the GL context
void FXGLContext::destroy(){
#ifdef HAVE_GL_H
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"FXGLContext::destroy %p\n",this));
#ifndef WIN32
      glXDestroyContext((Display*)getApp()->getDisplay(),(GLXContext)ctx);
#else
      wglDeleteContext((HGLRC)ctx);
#endif
      }
    ctx=NULL;
    xid=0;
    }
#endif
  }



//  Make the rendering context of drawable current
FXbool FXGLContext::begin(FXDrawable *drawable){
#ifdef HAVE_GL_H
  if(!drawable){ fxerror("FXGLContext::begin: NULL drawable.\n"); }
  if(!drawable->id()){ fxerror("FXGLContext::begin: drawable not created yet.\n"); }
  if(visual!=drawable->getVisual()){ fxerror("FXGLContext::begin: visuals do not match.\n"); }
  if(xid){
#ifndef WIN32
    if(glXMakeCurrent((Display*)getApp()->getDisplay(),drawable->id(),(GLXContext)ctx)){
      surface=drawable;
      return TRUE;
      }
#else
    //HDC hdc=drawable->GetDC();  // Obtain DC in a way appropriate for the drawable!
    HDC hdc=::GetDC((HWND)drawable->id());  // FIXME:- it should probably be the line above, but need to check about ReleaseDC first!
    if(visual->colormap){
      SelectPalette(hdc,(HPALETTE)visual->colormap,FALSE);
      RealizePalette(hdc);
      }
    if(wglMakeCurrent(hdc,(HGLRC)ctx)){
      surface=drawable;
      return TRUE;
      }
#endif
    }
#endif
  return FALSE;
  }


// Make the rendering context of drawable non-current
FXbool FXGLContext::end(){
  FXbool bRet=FALSE;
#ifdef HAVE_GL_H
  if(xid){
#ifndef WIN32
    bRet=glXMakeCurrent((Display*)getApp()->getDisplay(),None,(GLXContext)NULL);
#else
    if(surface){
      // According to "Steve Granja" <sjgranja@hks.com>,
      // ::ReleaseDC is still necessary even for owned DC's.
      // So release it here to prevent resource leak.
      ::ReleaseDC((HWND)surface->id(),wglGetCurrentDC());
      }
    bRet=wglMakeCurrent(NULL,NULL);
#endif
    surface=NULL;
    }
#endif
  return bRet;
  }


// Used by GL to swap the buffers in double buffer mode, or flush a single buffer
void FXGLContext::swapBuffers(){
#ifdef HAVE_GL_H
  if(!surface){ fxerror("FXGLContext::swapBuffers: not connected to drawable.\n"); }
#ifndef WIN32
  glXSwapBuffers((Display*)getApp()->getDisplay(),surface->id());
#else
  // SwapBuffers(wglGetCurrentDC());
  // wglSwapLayerBuffers(wglGetCurrentDC(),WGL_SWAP_MAIN_PLANE);
  HDC hdc=wglGetCurrentDC();
  if(wglSwapLayerBuffers(hdc,WGL_SWAP_MAIN_PLANE)==FALSE){
    SwapBuffers(hdc);
    }
#endif
#endif
  }


// This function only available on Mesa
void FXGLContext::swapSubBuffers(FXint,FXint,FXint,FXint){

// FIXME: Put the swap hack back!!
// FIXME: bool bUseSwapHack = ! strncmp("Mesa", glGetString(GL_RENDERER), 4);

// FIXME: how to get proc address by name, since we don't know which shared lib we're going to get!

// #ifdef HAVE_GL_H
// #ifdef HAVE_MESA
// #ifdef GLX_MESA_copy_sub_buffer
//   glXCopySubBufferMESA(getApp()->display,xid,x,height-y-h-1,w,h);
// #else
//   glXSwapBuffers(getApp()->display,xid);
// #endif
// #endif
// #endif
  }


// Save object to stream
void FXGLContext::save(FXStream& store) const {
  FXId::save(store);
  store << visual;
  store << sgnext;
  store << sgprev;
  }


// Load object from stream
void FXGLContext::load(FXStream& store){
  FXId::load(store);
  store >> visual;
  store >> sgnext;
  store >> sgprev;
  }


// Close and release any resources
FXGLContext::~FXGLContext(){
  FXTRACE((100,"FXGLContext::~FXGLContext %p\n",this));
  destroy();
  sgnext->sgprev=sgprev;
  sgprev->sgnext=sgnext;
  visual=(FXGLVisual*)-1L;
  surface=(FXDrawable*)-1L;
  sgnext=(FXGLContext*)-1L;
  sgprev=(FXGLContext*)-1L;
  ctx=(void*)-1L;
  }

}
