/********************************************************************************
*                                                                               *
*                         D o c k   S i t e   W i d g e t                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXDockSite.cpp,v 1.76 2006/01/22 17:58:23 fox Exp $                      *
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
#include "FXPacker.h"
#include "FXDockBar.h"
#include "FXDockSite.h"


/*
  Notes:
  - Use "Box-Car" algorithm when sliding horizontally (vertically).
  - Vertical arrangement is very tricky; we don't insert in between
    galleys when dragging since its not so easy to use; but this remains
    a possibility for future expansion.
  - We can STILL do wrapping of individual toolbars inside the toolbar dock;
    normally we compute the width the standard way, but if this does not
    fit the available space and its the first widget on the galley, we
    can call getHeightForWidth() and thereby wrap the item in on the
    galley.  Thus we have both wrapping of the toobars as well as
    wrapping inside the toolbar.
  - FIXME we should look at LAYOUT_DOCK_NEXT before shown() because
    if you hide a bar, we want to keep stuff in the same place.
  - Another nice addition would be to constrain docking from adding
    extra galleys, except when unavoidable.
*/

#define FUDGE 20        // Amount to move down/up before jumping into next galley

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXDockSite) FXDockSiteMap[]={
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXDockSite::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXDockSite::onFocusRight),
  };


// Object implementation
FXIMPLEMENT(FXDockSite,FXPacker,FXDockSiteMap,ARRAYNUMBER(FXDockSiteMap))


// Make a dock site
FXDockSite::FXDockSite(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXPacker(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  }


// Change wrap option
void FXDockSite::wrapGalleys(FXbool wrap){
  if(wrap && wrapGalleys()){
    options&=~DOCKSITE_NO_WRAP;
    recalc();
    }
  else if(!wrap && !wrapGalleys()){
    options|=DOCKSITE_NO_WRAP;
    recalc();
    }
  }


// Get wrap option
FXbool FXDockSite::wrapGalleys() const {
  return (options&DOCKSITE_NO_WRAP)==0;
  }


// Compute minimum width based on child layout hints
FXint FXDockSite::getDefaultWidth(){
  register FXint total=0,galw=0,any=0,w;
  register FXWindow *child;
  register FXuint hints;

  // Vertically oriented
  if(options&LAYOUT_SIDE_LEFT){
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
        if(any && (hints&LAYOUT_DOCK_NEXT)){
          total+=galw+hspacing;
          galw=w;
          }
        else{
          if(w>galw) galw=w;
          }
        any=1;
        }
      }
    total+=galw;
    }

  // Horizontally oriented
  else{
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
        if(any && (hints&LAYOUT_DOCK_NEXT)){
          if(galw>total) total=galw;
          galw=w;
          }
        else{
          if(galw) galw+=hspacing;
          galw+=w;
          }
        any=1;
        }
      }
    if(galw>total) total=galw;
    }
  return padleft+padright+total+(border<<1);
  }


// Compute minimum height based on child layout hints
FXint FXDockSite::getDefaultHeight(){
  register FXint total=0,galh=0,any=0,h;
  register FXWindow *child;
  register FXuint hints;

  // Vertically oriented
  if(options&LAYOUT_SIDE_LEFT){
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();
        if(any && (hints&LAYOUT_DOCK_NEXT)){
          if(galh>total) total=galh;
          galh=h;
          }
        else{
          if(galh) galh+=vspacing;
          galh+=h;
          }
        any=1;
        }
      }
    if(galh>total) total=galh;
    }

  // Horizontally oriented
  else{
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();
        if(any && (hints&LAYOUT_DOCK_NEXT)){
          total+=galh+vspacing;
          galh=h;
          }
        else{
          if(h>galh) galh=h;
          }
        any=1;
        }
      }
    total+=galh;
    }
  return padtop+padbottom+total+(border<<1);
  }


// Return width for given height (vertical orientation)
FXint FXDockSite::getWidthForHeight(FXint givenheight){
  register FXint space,total,galh,galw,any,w,h;
  register FXWindow *child;
  register FXuint hints;
  total=galh=galw=any=0;
  space=givenheight-padtop-padbottom-(border<<1);
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
      h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();
      if(any && ((hints&LAYOUT_DOCK_NEXT) || ((galh+h>space) && wrapGalleys()))){
        total+=galw+hspacing;
        galw=w;
        galh=h+vspacing;
        }
      else{
        galh+=h+vspacing;
        if(w>galw) galw=w;
        }
      any=1;
      }
    }
  total+=galw;
  return padleft+padright+total+(border<<1);
  }


// Return height for given width (horizontal orientation)
FXint FXDockSite::getHeightForWidth(FXint givenwidth){
  register FXint space,total,galh,galw,any,w,h;
  register FXWindow *child;
  register FXuint hints;
  total=galh=galw=any=0;
  space=givenwidth-padleft-padright-(border<<1);
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
      h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();
      if(any && ((hints&LAYOUT_DOCK_NEXT) || ((galw+w>space) && wrapGalleys()))){
        total+=galh+vspacing;
        galw=w+hspacing;
        galh=h;
        }
      else{
        galw+=w+hspacing;
        if(h>galh) galh=h;
        }
      any=1;
      }
    }
  total+=galh;
  return padtop+padbottom+total+(border<<1);
  }


// Determine vertical galley size
FXint FXDockSite::galleyWidth(FXWindow *begin,FXWindow*& end,FXint space,FXint& require,FXint& expand) const {
  register FXint galley,any,w,h;
  register FXWindow *child;
  register FXuint hints;
  require=expand=galley=any=0;
  for(child=end=begin; child; end=child,child=child->getNext()){
    if(child->shown()){

      // Get size
      hints=child->getLayoutHints();
      w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
      h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();

      // Break for new galley?
      if(any && ((hints&LAYOUT_DOCK_NEXT) || ((require+h>space) && wrapGalleys()))) break;

      // Expanding widgets
      if(hints&LAYOUT_FILL_Y) expand+=h;

      // Figure galley size
      require+=h+vspacing;
      if(w>galley) galley=w;
      any=1;
      }
    }
  require-=vspacing;
  return galley;
  }


// Determine horizontal galley size
FXint FXDockSite::galleyHeight(FXWindow *begin,FXWindow*& end,FXint space,FXint& require,FXint& expand) const {
  register FXint galley,any,w,h;
  register FXWindow *child;
  register FXuint hints;
  require=expand=galley=any=0;
  for(child=end=begin; child; end=child,child=child->getNext()){
    if(child->shown()){

      // Get size
      hints=child->getLayoutHints();
      w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
      h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();

      // Break for new galley?
      if(any && ((hints&LAYOUT_DOCK_NEXT) || ((require+w>space) && wrapGalleys()))) break;

      // Expanding widgets
      if(hints&LAYOUT_FILL_X) expand+=w;

      // Figure galley size
      require+=w+hspacing;
      if(h>galley) galley=h;
      any=1;
      }
    }
  require-=hspacing;
  return galley;
  }


// Recalculate layout
void FXDockSite::layout(){
  FXint expand,remain,require,left,right,top,bottom,galx,galy,galw,galh,e,t,x,y,w,h;
  FXWindow *begin,*end,*child;
  FXuint hints;

  // Vertically oriented
  if(options&LAYOUT_SIDE_LEFT){

    // Galley height
    left=border+padleft;
    right=width-padright-border;

    // Loop over galleys
    for(begin=getFirst(); begin; begin=end->getNext()){

      // Space available
      top=border+padtop;
      bottom=height-padbottom-border;

      // Galley width
      galw=galleyWidth(begin,end,bottom-top,require,expand);

      // Remaining space
      remain=bottom-top-require;
      if(expand) require=bottom-top;

      // Start next galley
      galx=left;
      left+=galw+hspacing;

      // Placement of widgets on galley
      for(child=begin,e=0; child; child=child->getNext()){
        if(child->shown()){

          // Get size
          hints=child->getLayoutHints();
          w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
          h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();

          // X-filled
          if(hints&LAYOUT_FILL_X) w=galw;

          // Y-filled
          if(hints&LAYOUT_FILL_Y){
            t=h*remain;
            e+=t%expand;
            h+=t/expand+e/expand;
            e%=expand;
            }

          require-=h;

          // Determine child x-position
          x=child->getX();
          if(x<galx) x=galx;
          if(x+w>galx+galw) x=galx+galw-w;

          // Determine child y-position
          y=child->getY();
          if(y+h>bottom-require) y=bottom-require-h;
          if(y<top) y=top;
          top=y+h+vspacing;

          require-=vspacing;

          // Placement on this galley
          child->position(x,y,w,h);
          }
        if(child==end) break;
        }
      }
    }

  // Horizontally oriented
  else{

    // Galley height
    top=border+padtop;
    bottom=height-padbottom-border;

    // Loop over galleys
    for(begin=getFirst(); begin; begin=end->getNext()){

      // Space available
      left=border+padleft;
      right=width-padright-border;

      // Galley height
      galh=galleyHeight(begin,end,right-left,require,expand);

      // Remaining space
      remain=right-left-require;
      if(expand) require=right-left;

      // Start next galley
      galy=top;
      top+=galh+vspacing;

      // Placement of widgets on galley
      for(child=begin,e=0; child; child=child->getNext()){
        if(child->shown()){

          // Get size
          hints=child->getLayoutHints();
          w=(hints&LAYOUT_FIX_WIDTH)?child->getWidth():child->getDefaultWidth();
          h=(hints&LAYOUT_FIX_HEIGHT)?child->getHeight():child->getDefaultHeight();

          // Y-filled
          if(hints&LAYOUT_FILL_Y) h=galh;

          // X-filled
          if(hints&LAYOUT_FILL_X){
            t=w*remain;
            e+=t%expand;
            w+=t/expand+e/expand;
            e%=expand;
            }

          require-=w;

          // Determine child y-position
          y=child->getY();
          if(y<galy) y=galy;
          if(y+h>galy+galh) y=galy+galh-h;

          // Determine child x-position
          x=child->getX();
          if(x+w>right-require) x=right-require-w;
          if(x<left) x=left;
          left=x+w+hspacing;

          require-=hspacing;

          // Placement on this galley
          child->position(x,y,w,h);
          }
        if(child==end) break;
        }
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Move bar vertically
void FXDockSite::moveVerBar(FXWindow* bar,FXWindow *begin,FXWindow* end,FXint bx,FXint by){
  FXint minpos,maxpos,pos;
  FXWindow *child,*other;

  // Pushing up
  if(by<bar->getY()){

    // Figure minimum position
    for(child=begin,minpos=border+padtop; child; child=child->getNext()){
      if(child->shown()){ minpos+=child->getHeight()+vspacing; }
      if(child==bar) break;
      }

    // Move bars in box-car fashion
    for(child=bar,pos=by+bar->getHeight()+vspacing,other=NULL; child; child=child->getPrev()){
      if(child->shown()){
        minpos=minpos-child->getHeight()-vspacing;
        pos=pos-child->getHeight()-vspacing;
        if(child->getY()<=pos) break;
        if(by<child->getY()) other=child;
        child->move((child==bar)?bx:child->getX(),FXMAX(pos,minpos));
        }
      if(child==begin) break;
      }

    // Hop bar over other if top of bar is above of top of other
    if(other && other!=bar){

      // Hopping over first on galley:- transfer flag over to new first
      if((other==begin) && (other->getLayoutHints()&LAYOUT_DOCK_NEXT)){
        other->setLayoutHints(other->getLayoutHints()&~LAYOUT_DOCK_NEXT);
        bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
        }

      // And rearrange order of children
      bar->move(bar->getX(),other->getY());
      other->move(other->getX(),bar->getY()+bar->getHeight()+vspacing);
      bar->reparent(this,other);
      }
    }

  // Pushing down
  else if(by>bar->getY()){

    // Figure maximum position
    for(child=end,maxpos=height-padbottom-border; child; child=child->getPrev()){
      if(child->shown()){ maxpos-=child->getHeight()+vspacing; }
      if(child==bar) break;
      }

    // Move bars in box-car fashion
    for(child=bar,pos=by,other=NULL; child; child=child->getNext()){
      if(child->shown()){
        if(pos<=child->getY()) break;
        if(by+bar->getHeight()>child->getY()+child->getHeight()) other=child;
        child->move((child==bar)?bx:child->getX(),FXMIN(pos,maxpos));
        maxpos=maxpos+child->getHeight()+vspacing;
        pos=pos+child->getHeight()+vspacing;
        }
      if(child==end) break;
      }

    // Hop bar over other if bottom of bar is below of bottom of other
    if(other && other!=bar){

      // First on galley hopped over to the right:- transfer flag to new first
      if((bar==begin) && (bar->getLayoutHints()&LAYOUT_DOCK_NEXT)){
        bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
        other->setLayoutHints(other->getLayoutHints()|LAYOUT_DOCK_NEXT);
        }

      // And rearrange order of children
      bar->move(bar->getX(),other->getY()+other->getHeight()-bar->getHeight());
      other->move(other->getX(),bar->getY()-other->getHeight()-vspacing);
      bar->reparent(this,other->getNext());
      }
    }

  // Move horizontally
  else{
    bar->move(bx,bar->getY());
    }
  }


// Move bar horizontally
void FXDockSite::moveHorBar(FXWindow* bar,FXWindow *begin,FXWindow* end,FXint bx,FXint by){
  FXint minpos,maxpos,pos;
  FXWindow *child,*other;

  // Pushing left
  if(bx<bar->getX()){

    // Figure minimum position
    for(child=begin,minpos=border+padleft; child; child=child->getNext()){
      if(child->shown()){ minpos+=child->getWidth()+hspacing; }
      if(child==bar) break;
      }

    // Move bars in box-car fashion
    for(child=bar,pos=bx+bar->getWidth()+hspacing,other=NULL; child; child=child->getPrev()){
      if(child->shown()){
        minpos=minpos-child->getWidth()-hspacing;
        pos=pos-child->getWidth()-hspacing;
        if(child->getX()<=pos) break;
        if(bx<child->getX()) other=child;
        child->move(FXMAX(pos,minpos),(child==bar)?by:child->getY());
        }
      if(child==begin) break;
      }

    // Hop bar over other if left of bar is leftward of left of other
    if(other && other!=bar){

      // Hopping over first on galley:- transfer flag over to new first
      if((other==begin) && (other->getLayoutHints()&LAYOUT_DOCK_NEXT)){
        other->setLayoutHints(other->getLayoutHints()&~LAYOUT_DOCK_NEXT);
        bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
        }

      // And rearrange order of children
      bar->move(other->getX(),bar->getY());
      other->move(bar->getX()+bar->getWidth()+hspacing,other->getY());
      bar->reparent(this,other);
      }
    }

  // Pushing right
  else if(bx>bar->getX()){

    // Figure maximum position
    for(child=end,maxpos=width-padright-border; child; child=child->getPrev()){
      if(child->shown()){ maxpos-=child->getWidth()+hspacing; }
      if(child==bar) break;
      }

    // Move bars in box-car fashion
    for(child=bar,pos=bx,other=NULL; child; child=child->getNext()){
      if(child->shown()){
        if(pos<=child->getX()) break;
        if(bx+bar->getWidth()>child->getX()+child->getWidth()) other=child;
        child->move(FXMIN(pos,maxpos),(child==bar)?by:child->getY());
        maxpos=maxpos+child->getWidth()+hspacing;
        pos=pos+child->getWidth()+hspacing;
        }
      if(child==end) break;
      }

    // Hop bar over other if right of bar is rightward of right of other
    if(other && other!=bar){

      // First on galley hopped over to the right:- transfer flag to new first
      if((bar==begin) && (bar->getLayoutHints()&LAYOUT_DOCK_NEXT)){
        bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
        other->setLayoutHints(other->getLayoutHints()|LAYOUT_DOCK_NEXT);
        }

      // And rearrange order of children
      bar->move(other->getX()+other->getWidth()-bar->getWidth(),bar->getY());
      other->move(bar->getX()-other->getWidth()-hspacing,other->getY());
      bar->reparent(this,other->getNext());
      }
    }

  // Move vertically
  else{
    bar->move(bar->getX(),by);
    }
  }


// Move dock bar, changing its options to suit position
void FXDockSite::moveToolBar(FXDockBar* bar,FXint barx,FXint bary){
  FXint left,right,top,bottom,galx,galy,galw,galh,dockx,docky,barw,barh,expand,require,w,h;
  FXWindow *begin,*end,*cur,*curend,*nxt,*nxtend,*prv,*prvend;

  // We insist this bar hangs under this dock site
  if(bar && bar->getParent()==this){

    // Proposed location
    dockx=barx;
    docky=bary;

    // Bar size
    barw=bar->getWidth();
    barh=bar->getHeight();

    // Interior
    top=border+padtop;
    bottom=height-padbottom-border;
    left=border+padleft;
    right=width-padright-border;

    // Vertically oriented
    if(options&LAYOUT_SIDE_LEFT){

      // Determine galley sizes
      galx=left;
      galw=0;
      for(begin=getFirst(),cur=prv=nxt=curend=prvend=nxtend=NULL; begin; begin=end->getNext()){
        w=galleyWidth(begin,end,bottom-top,require,expand);
        if(!after(end,bar)){ if(left<=barx && barx<left+w){ prv=begin; prvend=end; } }
        else if(!after(bar,begin)){ if(left<=barx+barw && barx+barw<left+w){ nxt=begin; nxtend=end; }  }
        else{ cur=begin; curend=end; galx=left; galw=w; }
        left+=w+hspacing;
        }

      // Same bar, move vertically
      if(dockx<galx) dockx=galx;
      if(dockx+barw>galx+galw) dockx=galx+galw-barw;

      // Move bar vertically; this may change the galley start and end!
      moveVerBar(bar,cur,curend,dockx,docky);

      // Moving bar right, unless we're about to pull it out of the dock
      if(barx+barw>=galx+galw+FUDGE && (!bar->getWetDock() || barx+barw<width-padright-border)){
        if(nxt){                                  // Hang at end of next galley
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          nxt->setLayoutHints(nxt->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
          bar->reparent(this,nxtend->getNext());
          }
        else{                                     // Hang below last
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          else cur->setLayoutHints(cur->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->reparent(this,NULL);
          }
        }

      // Moving bar left, unless we're about to pull it out of the dock
      else if(barx<galx-FUDGE && (!bar->getWetDock() || barx>padleft+border)){
        if(prv){                                  // Hang at end of previous galley
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          prv->setLayoutHints(prv->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
          bar->reparent(this,prvend->getNext());
          }
        else{                                     // Hand above first
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          else cur->setLayoutHints(cur->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->reparent(this,getFirst());
          }
        }
      }

    // Horizontally oriented
    else{

      // Determine galley sizes
      galy=top;
      galh=0;
      for(begin=getFirst(),cur=prv=nxt=curend=prvend=nxtend=NULL; begin; begin=end->getNext()){
        h=galleyHeight(begin,end,right-left,require,expand);
        if(!after(end,bar)){ if(top<=bary && bary<top+h){ prv=begin; prvend=end; } }
        else if(!after(bar,begin)){ if(top<=bary+barh && bary+barh<top+h){ nxt=begin; nxtend=end; }  }
        else{ cur=begin; curend=end; galy=top; galh=h; }
        top+=h+vspacing;
        }

      // Same bar, move horizontally
      if(docky<galy) docky=galy;
      if(docky+barh>galy+galh) docky=galy+galh-barh;

      // Move bar horizontally; this may change the galley start and end!
      moveHorBar(bar,cur,curend,dockx,docky);

      // Moving bar down, unless we're about to pull it out of the dock
      if(bary+barh>=galy+galh+FUDGE && (!bar->getWetDock() || bary+barh<height-padbottom-border)){
        if(nxt){                                  // Hang at end of next galley
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          nxt->setLayoutHints(nxt->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
          bar->reparent(this,nxtend->getNext());
          }
        else{                                     // Hang below last
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          else cur->setLayoutHints(cur->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->reparent(this,NULL);
          }
        }

      // Moving bar up, unless we're about to pull it out of the dock
      else if(bary<galy-FUDGE && (!bar->getWetDock() || bary>border+padtop)){
        if(prv){                                  // Hang at end of previous galley
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          prv->setLayoutHints(prv->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
          bar->reparent(this,prvend->getNext());
          }
        else{                                     // Hand above first
          if(cur==bar && bar!=curend) cur->getNext()->setLayoutHints(cur->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
          else cur->setLayoutHints(cur->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
          bar->reparent(this,getFirst());
          }
        }
      }
    }
  }


// Fix layouts for undocking given bar
void FXDockSite::undockToolBar(FXDockBar* bar){
  FXint space,expand,require;
  FXWindow *begin,*end;

  // We insist this bar hangs under this dock site
  if(bar && bar->getParent()==this){

    // Vertically oriented
    if(options&LAYOUT_SIDE_LEFT){

      // Space
      space=height-padbottom-padtop-border-border;

      // Determine galley sizes
      for(begin=getFirst(); begin; begin=end->getNext()){
        galleyWidth(begin,end,space,require,expand);
        if(before(begin,bar) && before(bar,end)) break;
        }

      // Adjust layout options
      if(begin==bar && bar!=end)
        begin->getNext()->setLayoutHints(begin->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
      else
        begin->setLayoutHints(begin->getLayoutHints()|LAYOUT_DOCK_NEXT);
      }

    // Horizontally oriented
    else{

      // Space
      space=width-padright-padleft-border-border;

      // Determine galley sizes
      for(begin=getFirst(); begin; begin=end->getNext()){
        galleyHeight(begin,end,space,require,expand);
        if(before(begin,bar) && before(bar,end)) break;
        }

      // Adjust layout options
      if(begin==bar && bar!=end)
        begin->getNext()->setLayoutHints(begin->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
      else
        begin->setLayoutHints(begin->getLayoutHints()|LAYOUT_DOCK_NEXT);
      }

    // Fix bar's layout hints too
    bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
    }
  }


// Fix layouts for docking given bar at given position
void FXDockSite::dockToolBar(FXDockBar* bar,FXWindow* before){

  // We insist this bar hangs under this dock site
  if(bar && bar->getParent()==this){

    // New galley for bar
    bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
    if(before) before->setLayoutHints(bar->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);
    }
  }


// Fix layouts for docking given bar at given position
void FXDockSite::dockToolBar(FXDockBar* bar,FXint barx,FXint bary){
  FXint left,right,top,bottom,barw,barh,expand,require,cx,cy,w,h;
  FXWindow *begin,*end,*child;

  // We insist this bar hangs under this dock site
  if(bar && bar->getParent()==this){

    // Interior
    top=border+padtop;
    left=border+padleft;
    bottom=height-padbottom-border;
    right=width-padright-border;

    // Bar size
    barw=bar->getWidth();
    barh=bar->getHeight();

    // Vertically oriented
    if(options&LAYOUT_SIDE_LEFT){

      cx=barx+barw/2;

      // Tentatively
      bar->reparent(this,getFirst());
      bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
      if(bar->getNext()){

        // Start galley on next
        bar->getNext()->setLayoutHints(bar->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);

        // Right of the left edge
        if(left<=cx){

          // Determine galley
          for(begin=bar->getNext(); begin; begin=end->getNext()){
            w=galleyWidth(begin,end,bottom-top,require,expand);
            if(left<=cx && cx<left+w){

              // Find spot on galley
              for(child=begin; child!=end->getNext() && (!child->shown() || bary>=child->getY()); child=child->getNext());

              // At the front
              if((child==begin) && (child->getLayoutHints()&LAYOUT_DOCK_NEXT)){
                child->setLayoutHints(child->getLayoutHints()&~LAYOUT_DOCK_NEXT);
                }
              else{
                bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
                }

              // hang in front
              bar->reparent(this,child);
              goto ver;
              }
            left+=w+hspacing;
            }

          // Link at the bottom
          bar->reparent(this,NULL);
          }
        }

      // Move horizontally
ver:  bar->move(FXCLAMP(left,barx,right),bary);
      }

    // Horizontally oriented
    else{

      cy=bary+barh/2;

      // Tentatively
      bar->reparent(this,getFirst());
      bar->setLayoutHints(bar->getLayoutHints()|LAYOUT_DOCK_NEXT);
      if(bar->getNext()){

        // Start galley on next
        bar->getNext()->setLayoutHints(bar->getNext()->getLayoutHints()|LAYOUT_DOCK_NEXT);

        // Below top edge
        if(top<=cy){

          // Determine galley
          for(begin=bar->getNext(); begin; begin=end->getNext()){
            h=galleyHeight(begin,end,right-left,require,expand);
            if(top<=cy && cy<top+h){

              // Find spot on galley
              for(child=begin; child!=end->getNext() && (!child->shown() || barx>=child->getX()); child=child->getNext());

              // At the front
              if((child==begin) && (child->getLayoutHints()&LAYOUT_DOCK_NEXT)){
                child->setLayoutHints(child->getLayoutHints()&~LAYOUT_DOCK_NEXT);
                }
              else{
                bar->setLayoutHints(bar->getLayoutHints()&~LAYOUT_DOCK_NEXT);
                }

              // hang in front
              bar->reparent(this,child);
              goto hor;
              }
            top+=h+vspacing;
            }

          // Link at the bottom
          bar->reparent(this,NULL);
          }
        }

      // Move horizontally
hor:  bar->move(barx,FXCLAMP(top,bary,bottom));
      }
    }
  }


}

