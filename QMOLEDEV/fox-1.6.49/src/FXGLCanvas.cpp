/********************************************************************************
*                                                                               *
*                    O p e n G L   C a n v a s   O b j e c t                    *
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
* $Id: FXGLCanvas.cpp,v 1.61 2006/01/22 17:58:27 fox Exp $                      *
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
#include "FXException.h"
#include "FXVisual.h"
#include "FXGLVisual.h"
#include "FXCursor.h"
#include "FXGLContext.h"
#include "FXGLCanvas.h"


/*
  Notes:
  - Since this only adds SetPixelFormat, perhaps not a bad idea to contemplate
    moving this call to SetPixelFormat somewhere else [candidates are FXGLVisual,
    FXWindow, or FXGLContext].
  - We may opt to have GLContext be created just prior to the first use.
*/


using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXGLCanvas,FXCanvas,NULL,0)


// For serialization
FXGLCanvas::FXGLCanvas(){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  sgnext=this;
  sgprev=this;
  ctx=0;
  }


// Make a canvas
FXGLCanvas::FXGLCanvas(FXComposite* p,FXGLVisual *vis,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXCanvas(p,tgt,sel,opts,x,y,w,h){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  visual=vis;
  sgnext=this;
  sgprev=this;
  ctx=0;
  }


// Make a canvas sharing display lists
FXGLCanvas::FXGLCanvas(FXComposite* p,FXGLVisual *vis,FXGLCanvas* sharegroup,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXCanvas(p,tgt,sel,opts,x,y,w,h){
  flags|=FLAG_ENABLED|FLAG_SHOWN;
  visual=vis;
  if(sharegroup){
    sgnext=sharegroup;
    sgprev=sharegroup->sgprev;
    sharegroup->sgprev=this;
    sgprev->sgnext=this;
    }
  else{
    sgnext=this;
    sgprev=this;
    }
  ctx=0;
  }



#ifdef WIN32
const char* FXGLCanvas::GetClass() const { return "FXGLCanvas"; }
#endif


// Return TRUE if it is sharing display lists
FXbool FXGLCanvas::isShared() const { return sgnext!=this; }


// Create X window (GL CANVAS)
void FXGLCanvas::create(){
  FXWindow::create();
#ifdef HAVE_GL_H
  if(!ctx){
    void *sharedctx=NULL;

    // Must have GL info available
    if(!visual->getInfo()){
      throw FXWindowException("unable to create GL window.");
      }

    // Sharing display lists with other context
    if(sgnext!=this){

      // Find another member of the group which is already created, and get its context
      FXGLCanvas *canvas=sgnext;
      while(canvas!=this){
        sharedctx=canvas->ctx;
        if(sharedctx) break;
        canvas=canvas->sgnext;
        }

      // The visuals have to match, the book says...
      if(sgnext->getVisual()!=canvas->getVisual()){
        throw FXWindowException("unable to create GL window.");
        }
      }

#ifndef WIN32

    // Make context
    ctx=glXCreateContext((Display*)getApp()->getDisplay(),(XVisualInfo*)visual->getInfo(),(GLXContext)sharedctx,TRUE);
    if(!ctx){
      throw FXWindowException("unable to create GL window.");
      }

#else

    // Make that the pixel format of the device context
    HDC hdc=::GetDC((HWND)xid);
    if(!SetPixelFormat(hdc,(FXint)(FXival)visual->getVisual(),(PIXELFORMATDESCRIPTOR*)visual->getInfo())){
      throw FXWindowException("unable to create GL window.");
      }

    // Make context
    ctx=(void*)wglCreateContext(hdc);
    if(!ctx){
      throw FXWindowException("unable to create GL window.");
      }

    // I hope I didn't get this backward; the new context obviously has no
    // display lists yet, but the old one may have, as it has already been around
    // for a while.  If you see this fail and can't explain why, then that might
    // be what's going on.  Report this to jeroen@fox-toolkit.org
    if(sharedctx && !wglShareLists((HGLRC)sharedctx,(HGLRC)ctx)){
      throw FXWindowException("unable to create GL window.");
      }
    ::ReleaseDC((HWND)xid,hdc);

#endif
    }
#endif
  }


// Detach the GL Canvas
void FXGLCanvas::detach(){
#ifdef HAVE_GL_H
  if(ctx){
    // Will this leak memory?
    ctx=0;
    }
#endif
  FXWindow::detach();
  }


// Destroy the GL Canvas
void FXGLCanvas::destroy(){
#ifdef HAVE_GL_H
  if(ctx){
#ifndef WIN32
    glXDestroyContext((Display*)getApp()->getDisplay(),(GLXContext)ctx);
#else
    wglDeleteContext((HGLRC)ctx);
#endif
    ctx=0;
    }
#endif
  FXWindow::destroy();
  }


//  Make the rendering context of GL Canvas current
FXbool FXGLCanvas::makeCurrent(){
#ifdef HAVE_GL_H
  if(ctx){
#ifndef WIN32
    return glXMakeCurrent((Display*)getApp()->getDisplay(),xid,(GLXContext)ctx);
#else
    HDC hdc=::GetDC((HWND)xid);
    if(visual->colormap){
      SelectPalette(hdc,(HPALETTE)visual->colormap,FALSE);
      RealizePalette(hdc);
      }
    BOOL bStatus=wglMakeCurrent(hdc,(HGLRC)ctx);
    return bStatus;
#endif
    }
#endif
  return FALSE;
  }


//  Make the rendering context of GL Canvas current
FXbool FXGLCanvas::makeNonCurrent(){
#ifdef HAVE_GL_H
  if(ctx){
#ifndef WIN32
    return glXMakeCurrent((Display*)getApp()->getDisplay(),None,(GLXContext)NULL);
#else
    // According to "Steve Granja" <sjgranja@hks.com>,
    // ::ReleaseDC is still necessary even for owned DC's.
    // So release it here to prevent resource leak.
    ::ReleaseDC((HWND)xid,wglGetCurrentDC());
    BOOL bStatus=wglMakeCurrent(NULL,NULL);
    return bStatus;
#endif
    }
#endif
  return FALSE;
  }


// Return current context, if any
void* FXGLCanvas::getCurrentContext(){
#ifdef HAVE_GL_H
#ifndef WIN32
  return (void*)glXGetCurrentContext();
#else
  return (void*)wglGetCurrentContext();
#endif
#else
  return NULL;
#endif
  }


//  Return TRUE if this window's context is current
FXbool FXGLCanvas::isCurrent() const {
#ifdef HAVE_GL_H
  if(ctx){
#ifndef WIN32
    return (glXGetCurrentContext() == (GLXContext)ctx);
#else
    return (wglGetCurrentContext() == (HGLRC)ctx);
#endif
    }
#endif
  return FALSE;
  }


// Used by GL to swap the buffers in double buffer mode, or flush a single buffer
void FXGLCanvas::swapBuffers(){
#ifdef HAVE_GL_H
#ifndef WIN32
  glXSwapBuffers((Display*)getApp()->getDisplay(),xid);
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


// Save object to stream
void FXGLCanvas::save(FXStream& store) const {
  FXWindow::save(store);
  store << sgnext;
  store << sgprev;
  }


// Load object from stream
void FXGLCanvas::load(FXStream& store){
  FXWindow::load(store);
  store >> sgnext;
  store >> sgprev;
  }


// Close and release any resources
FXGLCanvas::~FXGLCanvas(){
  sgnext->sgprev=sgprev;
  sgprev->sgnext=sgnext;
  sgnext=(FXGLCanvas*)-1L;
  sgprev=(FXGLCanvas*)-1L;
#ifdef HAVE_GL_H
  if(ctx){
#ifndef WIN32
    glXDestroyContext((Display*)getApp()->getDisplay(),(GLXContext)ctx);
#else
    wglDeleteContext((HGLRC)ctx);
#endif
    }
#endif
  }

}
