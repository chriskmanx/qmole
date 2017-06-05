/********************************************************************************
*                                                                               *
*                     S c r o l l W i n d o w   W i d g e t                     *
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
* $Id: FXScrollWindow.cpp,v 1.37 2006/01/22 17:58:41 fox Exp $                  *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXScrollBar.h"
#include "FXScrollWindow.h"


/*
  Notes:
  - Intercepts pagedn/pageup to scroll.
  - We're assuming you're not using LAYOUT_FIX_WIDTH and LAYOUT_FILL_X
    at the same time...
  - Note that content window's position is not necessarily the same as
    the scroll position pos_x and pos_y.
*/

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXScrollWindow) FXScrollWindowMap[]={
  FXMAPFUNC(SEL_KEYPRESS,0,FXScrollWindow::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXScrollWindow::onKeyRelease),
  FXMAPFUNC(SEL_FOCUS_SELF,0,FXScrollWindow::onFocusSelf),
  };


// Object implementation
FXIMPLEMENT(FXScrollWindow,FXScrollArea,FXScrollWindowMap,ARRAYNUMBER(FXScrollWindowMap))



// Construct and init
FXScrollWindow::FXScrollWindow(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXScrollArea(p,opts,x,y,w,h){
  }


// Get content window; may be NULL
FXWindow* FXScrollWindow::contentWindow() const {
  return corner->getNext();
  }


// Determine content width of scroll area
FXint FXScrollWindow::getContentWidth(){
  register FXuint hints;
  register FXint w=1;
  if(contentWindow()){
    hints=contentWindow()->getLayoutHints();
    if(hints&LAYOUT_FIX_WIDTH) w=contentWindow()->getWidth();
    else w=contentWindow()->getDefaultWidth();
    }
  return w;
  }


// Determine content height of scroll area
FXint FXScrollWindow::getContentHeight(){
  register FXuint hints;
  register FXint h=1;
  if(contentWindow()){
    hints=contentWindow()->getLayoutHints();
    if(hints&LAYOUT_FIX_HEIGHT) h=contentWindow()->getHeight();
    else h=contentWindow()->getDefaultHeight();
    }
  return h;
  }


// Move contents; moves child window
void FXScrollWindow::moveContents(FXint x,FXint y){
  register FXWindow* contents=contentWindow();
  register FXint xx,yy,ww,hh;
  register FXuint hints;
  if(contents){

    // Get hints
    hints=contents->getLayoutHints();

    // Get content size
    ww=getContentWidth();
    hh=getContentHeight();

    // Determine x-position
    xx=x;
    if(ww<viewport_w){
      if(hints&LAYOUT_FILL_X) ww=viewport_w;
      if(hints&LAYOUT_CENTER_X) xx=(viewport_w-ww)/2;
      else if(hints&LAYOUT_RIGHT) xx=viewport_w-ww;
      else xx=0;
      }

    // Determine y-position
    yy=y;
    if(hh<viewport_h){
      if(hints&LAYOUT_FILL_Y) hh=viewport_h;
      if(hints&LAYOUT_CENTER_Y) yy=(viewport_h-hh)/2;
      else if(hints&LAYOUT_BOTTOM) yy=viewport_h-hh;
      else yy=0;
      }
    contents->move(xx,yy);
    }
  pos_x=x;
  pos_y=y;
  }


// Recalculate layout
void FXScrollWindow::layout(){
  register FXWindow* contents=contentWindow();
  register FXint xx,yy,ww,hh;
  register FXuint hints;

  // Layout scroll bars and viewport
  FXScrollArea::layout();

  // Resize contents
  if(contents){

    // Get hints
    hints=contents->getLayoutHints();

    // Get content size
    ww=getContentWidth();
    hh=getContentHeight();

    // Determine x-position
    xx=pos_x;
    if(ww<viewport_w){
      if(hints&LAYOUT_FILL_X) ww=viewport_w;
      if(hints&LAYOUT_CENTER_X) xx=(viewport_w-ww)/2;
      else if(hints&LAYOUT_RIGHT) xx=viewport_w-ww;
      else xx=0;
      }

    // Determine y-position
    yy=pos_y;
    if(hh<viewport_h){
      if(hints&LAYOUT_FILL_Y) hh=viewport_h;
      if(hints&LAYOUT_CENTER_Y) yy=(viewport_h-hh)/2;
      else if(hints&LAYOUT_BOTTOM) yy=viewport_h-hh;
      else yy=0;
      }

    // Reposition content window
    contents->position(xx,yy,ww,hh);

    // Make sure its under the scroll bars
    contents->lower();
    }
  flags&=~FLAG_DIRTY;
  }


// When focus moves to scroll window, we actually force the
// focus to the content window or a child thereof.
long FXScrollWindow::onFocusSelf(FXObject* sender,FXSelector,void* ptr){
  FXWindow *child=contentWindow();      ///// FIXME see MDIChild /////
  return child && child->handle(sender,FXSEL(SEL_FOCUS_SELF,0),ptr);
  }


// Keyboard press
long FXScrollWindow::onKeyPress(FXObject* sender,FXSelector sel,void* ptr){
  if(FXScrollArea::onKeyPress(sender,sel,ptr)) return 1;
  switch(((FXEvent*)ptr)->code){
    case KEY_Page_Up:
    case KEY_KP_Page_Up:
      setPosition(pos_x,pos_y+verticalScrollBar()->getPage());
      return 1;
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
      setPosition(pos_x,pos_y-verticalScrollBar()->getPage());
      return 1;
    }
  return 0;
  }


// Keyboard release
long FXScrollWindow::onKeyRelease(FXObject* sender,FXSelector sel,void* ptr){
  if(FXScrollArea::onKeyRelease(sender,sel,ptr)) return 1;
  switch(((FXEvent*)ptr)->code){
    case KEY_Page_Up:
    case KEY_KP_Page_Up:
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
      return 1;
    }
  return 0;
  }

}
