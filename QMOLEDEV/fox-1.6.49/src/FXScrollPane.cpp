/********************************************************************************
*                                                                               *
*               S c r o l l i n g   M e n u   P a n e   W i d g e t             *
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
* $Id: FXScrollPane.cpp,v 1.21 2006/01/22 17:58:41 fox Exp $                    *
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
#include "FXFont.h"
#include "FXArrowButton.h"
#include "FXMenuPane.h"
#include "FXScrollPane.h"


/*
  Notes:
  - Should we reset the topmost item each time it is shown?
  - Don't disable arrows since not catching the event means
    it gets passed to the grab owner [at least on Windows].
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXScrollPane) FXScrollPaneMap[]={
  FXMAPFUNC(SEL_COMMAND,FXScrollPane::ID_SCROLL_DN,FXScrollPane::onCmdIncrement),
  FXMAPFUNC(SEL_COMMAND,FXScrollPane::ID_SCROLL_UP,FXScrollPane::onCmdDecrement),
  };


// Object implementation
FXIMPLEMENT(FXScrollPane,FXPopup,FXScrollPaneMap,ARRAYNUMBER(FXScrollPaneMap))


// Deserialization
FXScrollPane::FXScrollPane(){
  visible=10;
  top=0;
  }


// Build empty one
FXScrollPane::FXScrollPane(FXWindow* owner,FXint nvis,FXuint opts):FXMenuPane(owner,opts){
  up=new FXArrowButton(this,this,ID_SCROLL_UP,ARROW_UP|ARROW_AUTO|ARROW_REPEAT);
  dn=new FXArrowButton(this,this,ID_SCROLL_DN,ARROW_DOWN|ARROW_AUTO|ARROW_REPEAT);
  up->setArrowSize(7);
  dn->setArrowSize(7);
  visible=nvis;
  top=0;
  }


// Show popup and add to popup stack
void FXScrollPane::show(){
  FXMenuPane::show();
  setTopItem(0);
  }


// Get width
FXint FXScrollPane::getDefaultWidth(){
  register FXWindow* child;
  register FXint wmax,wcum,w,n;
  register FXuint hints;
  wmax=wcum=0;
  for(child=dn->getNext(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else w=child->getDefaultWidth();
      if(wmax<w) wmax=w;
      }
    }
  for(child=dn->getNext(),n=0; child && n<visible; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=wmax;
      else w=child->getDefaultWidth();
      wcum+=w;
      n++;
      }
    }
  if(options&POPUP_HORIZONTAL){
    wcum+=up->getDefaultWidth();
    wcum+=dn->getDefaultWidth();
    }
  else{
    wcum=wmax;
    }
  return wcum+(border<<1);
  }


// Get height
FXint FXScrollPane::getDefaultHeight(){
  register FXWindow* child;
  register FXint hmax,hcum,h,n;
  register FXuint hints;
  hmax=hcum=0;
  for(child=dn->getNext(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else h=child->getDefaultHeight();
      if(hmax<h) hmax=h;
      }
    }
  for(child=dn->getNext(),n=0; child && n<visible; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=hmax;
      else h=child->getDefaultHeight();
      hcum+=h;
      n++;
      }
    }
  if(options&POPUP_HORIZONTAL){
    hcum=hmax;
    }
  else{
    hcum+=up->getDefaultHeight();
    hcum+=dn->getDefaultHeight();
    }
  return hcum+(border<<1);
  }


// Recalculate layout
void FXScrollPane::layout(){
  register FXint w,h,x,y,mw,mh,arrowsize,n;
  register FXWindow *child;
  register FXuint hints;

  // Horizontal
  if(options&POPUP_HORIZONTAL){

    // Width of arrow button
    arrowsize=up->getDefaultWidth();

    // Get maximum size
    for(child=dn->getNext(),mw=0; child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
        else w=child->getDefaultWidth();
        if(mw<w) mw=w;
        }
      }

    // Do the layout
    for(child=dn->getNext(),x=border+arrowsize,n=0; child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=mw;
        else w=child->getDefaultWidth();
        if(top<=n && n<top+visible){
          child->position(x,border,w,height-(border<<1));
          x+=w;
          }
        else{
          child->position(0,height,w,height-(border<<1));
          }
        n++;
        }
      }

    // Place arrow buttons
    up->position(border,border,arrowsize,height-(border<<1));
    dn->position(width-border-arrowsize,border,arrowsize,height-(border<<1));

    // Point arrows left and right
    up->setArrowStyle(ARROW_LEFT|ARROW_AUTO|ARROW_REPEAT);
    dn->setArrowStyle(ARROW_RIGHT|ARROW_AUTO|ARROW_REPEAT);
    }

  // Vertical
  else{

    // Height of arrow button
    arrowsize=up->getDefaultHeight();

    // Get maximum size
    for(child=dn->getNext(),mh=0; child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else h=child->getDefaultHeight();
        if(mh<h) mh=h;
        }
      }

    // Do the layout
    for(child=dn->getNext(),y=border+arrowsize,n=0; child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=mh;
        else h=child->getDefaultHeight();
        if(top<=n && n<top+visible){
          child->position(border,y,width-(border<<1),h);
          y+=h;
          }
        else{
          child->position(width,0,width-(border<<1),h);   // Move off to the side
          }
        n++;
        }
      }

    // Place arrow buttons
    up->position(border,border,width-(border<<1),arrowsize);
    dn->position(border,height-border-arrowsize,width-(border<<1),arrowsize);

    // Point arrows up and down
    up->setArrowStyle(ARROW_UP|ARROW_AUTO|ARROW_REPEAT);
    dn->setArrowStyle(ARROW_DOWN|ARROW_AUTO|ARROW_REPEAT);
    }

  // Arrow buttons stay on top
  up->raise();
  dn->raise();

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Scroll contents up
long FXScrollPane::onCmdIncrement(FXObject*,FXSelector,void*){
  setTopItem(top+1);
  return 1;
  }


// Scroll contents down
long FXScrollPane::onCmdDecrement(FXObject*,FXSelector,void*){
  setTopItem(top-1);
  return 1;
  }


// List is multiple of nitems
void FXScrollPane::setNumVisible(FXint nvis){
  if(nvis<0) nvis=0;
  if(visible!=nvis){
    visible=nvis;
    setTopItem(top);                    // FIXME is this enough?
    }
  }


// Scroll item to top
void FXScrollPane::setTopItem(FXint t){
  FXint m=numChildren()-visible-2;      // FIXME should really be visible children
  if(t<0) t=0;
  if(t>=m) t=m;
  if(t!=top){
    top=t;
    recalc();
    }
  }


// Destroy it
FXScrollPane::~FXScrollPane(){
  dn=(FXArrowButton*)-1L;
  up=(FXArrowButton*)-1L;
  }

}
