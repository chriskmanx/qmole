/********************************************************************************
*                                                                               *
*                       R o o t   W i n d o w   O b j e c t                     *
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
* $Id: FXRootWindow.cpp,v 1.35 2006/01/22 17:58:40 fox Exp $                    *
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
#include "FXRootWindow.h"

/*
  Notes:

  - Size of FXRootWindow is now size of the entire virtual display, which
    is a tiled virtual area of primary and secondary display adapters.
*/


#define DISPLAY(app) ((Display*)((app)->display))

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXRootWindow,FXComposite,NULL,0)


// Construct root window
FXRootWindow::FXRootWindow(FXApp* a,FXVisual *vis):FXComposite(a,vis){
  }


#ifdef WIN32

// Returns device context
FXID FXRootWindow::GetDC() const {
  LockWindowUpdate(GetDesktopWindow());
  return GetDCEx(GetDesktopWindow(),NULL,DCX_CACHE|DCX_LOCKWINDOWUPDATE);
  }


// Release DC
int FXRootWindow::ReleaseDC(FXID hdc) const {
  int status=::ReleaseDC(GetDesktopWindow(),(HDC)hdc);
  LockWindowUpdate(NULL);
  return status;
  }

#endif


// When created, create subwindows ONLY
void FXRootWindow::create(){
  register FXWindow *child;
  if(!xid){

#ifndef WIN32

    // Got to have a visual
    if(!visual){ fxerror("%s::create: trying to create window without a visual.\n",getClassName()); }

    // Initialize visual
    visual->create();

    xid=RootWindow(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
    width=DisplayWidth(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
    height=DisplayHeight(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));

#else

    // Got to have a visual
    if(!visual){ fxerror("%s::create: trying to create window without a visual.\n",getClassName()); }

    // Initialize visual
    visual->create();

    // Get HWND of desktop window
    xid=GetDesktopWindow();

    // Obtain size
    HDC hdc=::GetDC((HWND)xid);
    width=GetDeviceCaps(hdc,HORZRES);
    height=GetDeviceCaps(hdc,VERTRES);
    ::ReleaseDC((HWND)xid,hdc);

    // The code below returns the size of the entire virtual
    // screen area instead of just that of the primary display;
    // thanks to "Steve Granja" <Steven.Granja@abaqus.com>.
    // [Apparently does not work on Win95 and WinNT...]
    //xpos=GetSystemMetrics(SM_XVIRTUALSCREEN);
    //ypos=GetSystemMetrics(SM_YVIRTUALSCREEN);
    //width=GetSystemMetrics(SM_CXVIRTUALSCREEN);
    //height=GetSystemMetrics(SM_CYVIRTUALSCREEN);
#endif

    // Normally create children
    for(child=getFirst(); child; child=child->getNext()) child->create();
    }
  }


// Detach window
void FXRootWindow::detach(){
  register FXWindow *child;
  visual->detach();
  if(xid){
    for(child=getFirst(); child; child=child->getNext()) child->detach();
    xid=0;
    }
  }


// When deleted, delete subwindows ONLY
void FXRootWindow::destroy(){
  register FXWindow *child;
  if(xid){
    for(child=getFirst(); child; child=child->getNext()) child->destroy();
    xid=0;
    }
  }


// Get default width
FXint FXRootWindow::getDefaultWidth(){
#ifndef WIN32
  return DisplayWidth(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
#else
  HDC hdc=::GetDC(GetDesktopWindow());
  FXint w=GetDeviceCaps(hdc,HORZRES);
  ::ReleaseDC(GetDesktopWindow(),hdc);
  return w;
#endif
  }


// Get default height
FXint FXRootWindow::getDefaultHeight(){
#ifndef WIN32
  return DisplayHeight(DISPLAY(getApp()),DefaultScreen(DISPLAY(getApp())));
#else
  HDC hdc=::GetDC(GetDesktopWindow());
  FXint h=GetDeviceCaps(hdc,VERTRES);
  ::ReleaseDC(GetDesktopWindow(),hdc);
  return h;
#endif
  }


// Moving root has no effect
void FXRootWindow::move(FXint,FXint){ }


// Move and resize root has no effect
void FXRootWindow::position(FXint,FXint,FXint,FXint){ }


// Resize root window has no effect
void FXRootWindow::resize(FXint,FXint){ }


// Layout of root window
void FXRootWindow::layout(){ }


// Mark as dirty
void FXRootWindow::recalc(){ }


// Root can not be focused on
void FXRootWindow::setFocus(){ }


// Root can not be unfocused
void FXRootWindow::killFocus(){ }


// Does not destroy root window
FXRootWindow::~FXRootWindow(){
  xid=0;
  }

}
