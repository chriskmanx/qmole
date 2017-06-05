/********************************************************************************
*                                                                               *
*                        T o o l B a r   W i d g e t                            *
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
* $Id: FXToolBar.cpp,v 1.48.2.1 2006/06/20 13:13:06 fox Exp $                       *
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
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXGIFIcon.h"
#include "FXDCWindow.h"
#include "FXDrawable.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXComposite.h"
#include "FXPacker.h"
#include "FXPopup.h"
#include "FXMenuPane.h"
#include "FXMenuCaption.h"
#include "FXMenuCommand.h"
#include "FXMenuCascade.h"
#include "FXMenuSeparator.h"
#include "FXMenuRadio.h"
#include "FXMenuCheck.h"
#include "FXShell.h"
#include "FXSeparator.h"
#include "FXTopWindow.h"
#include "FXDockBar.h"
#include "FXToolBar.h"
#include "FXDockSite.h"
#include "FXToolBarGrip.h"
#include "FXToolBarShell.h"
#include "icons.h"


/*
  Notes:
  - May want to add support for centered layout mode.
*/


// Docking side
#define LAYOUT_SIDE_MASK (LAYOUT_SIDE_LEFT|LAYOUT_SIDE_RIGHT|LAYOUT_SIDE_TOP|LAYOUT_SIDE_BOTTOM)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXToolBar) FXToolBarMap[]={
  FXMAPFUNC(SEL_UPDATE,FXToolBar::ID_DOCK_FLIP,FXToolBar::onUpdDockFlip),
  FXMAPFUNC(SEL_COMMAND,FXToolBar::ID_DOCK_FLIP,FXToolBar::onCmdDockFlip),
  };


// Object implementation
FXIMPLEMENT(FXToolBar,FXDockBar,FXToolBarMap,ARRAYNUMBER(FXToolBarMap))


// Make a dockable and, possibly, floatable toolbar
FXToolBar::FXToolBar(FXComposite* p,FXComposite* q,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXDockBar(p,q,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  }


// Make a non-floatable toolbar
FXToolBar::FXToolBar(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXDockBar(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  }


// Compute minimum width based on child layout hints
FXint FXToolBar::getDefaultWidth(){
  register FXint total=0,mw=0,w;
  register FXWindow* child;
  register FXuint hints;
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) w=child->getDefaultWidth();
      else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      if(!(options&LAYOUT_SIDE_LEFT)){  // Horizontal
        if(total) total+=hspacing;
        total+=w;
        }
      else{                             // Vertical
        if(total<w) total=w;
        }
      }
    }
  return padleft+padright+total+(border<<1);
  }


// Compute minimum height based on child layout hints
FXint FXToolBar::getDefaultHeight(){
  register FXint total=0,mh=0,h;
  register FXWindow* child;
  register FXuint hints;
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) h=child->getDefaultHeight();
      else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      if(options&LAYOUT_SIDE_LEFT){     // Vertical
        if(total) total+=vspacing;
        total+=h;
        }
      else{                             // Horizontal
        if(total<h) total=h;
        }
      }
    }
  return padtop+padbottom+total+(border<<1);
  }


/*
// Return width for given height
FXint FXToolBar::getWidthForHeight(FXint givenheight){
  register FXint wtot,wmax,hcum,w,h,space,ngalleys,mw=0,mh=0;
  register FXWindow* child;
  register FXuint hints;
  wtot=wmax=hcum=ngalleys=0;
  space=givenheight-padtop-padbottom-(border<<1);
  if(space<1) space=1;
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(dynamic_cast<FXToolBarGrip*>(child)) w=child->getDefaultWidth();
      else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      if(dynamic_cast<FXToolBarGrip*>(child)) h=child->getDefaultHeight();
      else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      if(hcum+h>space) hcum=0;
      if(hcum==0) ngalleys++;
      hcum+=h+vspacing;
      if(wmax<w) wmax=w;
      }
    }
  wtot=wmax*ngalleys;
  return padleft+padright+wtot+(border<<1);
  }


// Return height for given width
FXint FXToolBar::getHeightForWidth(FXint givenwidth){
  register FXint htot,hmax,wcum,w,h,space,ngalleys,mw=0,mh=0;
  register FXWindow* child;
  register FXuint hints;
  htot=hmax=wcum=ngalleys=0;
  space=givenwidth-padleft-padright-(border<<1);
  if(space<1) space=1;
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(dynamic_cast<FXToolBarGrip*>(child)) w=child->getDefaultWidth();
      else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      if(dynamic_cast<FXToolBarGrip*>(child)) h=child->getDefaultHeight();
      else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      if(wcum+w>space) wcum=0;
      if(wcum==0) ngalleys++;
      wcum+=w+hspacing;
      if(hmax<h) hmax=h;
      }
    }
  htot=hmax*ngalleys;
  return padtop+padbottom+htot+(border<<1);
  }


// Recalculate layout
void FXToolBar::layout(){
  FXint galleyleft,galleyright,galleytop,galleybottom,galleywidth,galleyheight;
  FXint tleft,tright,ttop,bleft,bbottom;
  FXint ltop,lbottom,lleft,rtop,rright;
  FXWindow *child;
  FXint x,y,w,h,mw=0,mh=0;
  FXuint hints;

  // Get maximum child size
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();

  // Vertical toolbar
  if(options&LAYOUT_SIDE_LEFT){
    galleywidth=0;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(child->isMemberOf(FXMETACLASS(FXToolBarGrip))) w=child->getDefaultWidth();
        else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=mw;
        else w=child->getDefaultWidth();
        if(galleywidth<w) galleywidth=w;
        }
      }
    galleyleft=border+padleft;
    galleyright=width-border-padright;
    galleytop=border+padtop;
    galleybottom=height-border-padbottom;
    tleft=galleyleft;
    tright=galleyleft+galleywidth;
    ttop=galleytop;
    bleft=galleyright-galleywidth;
    bbottom=galleybottom;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(child->isMemberOf(FXMETACLASS(FXToolBarGrip))){
          w=galleywidth;
          h=child->getDefaultHeight();
          }
        else{
          if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
          else if(options&PACK_UNIFORM_WIDTH) w=mw;
          else w=child->getDefaultWidth();
          if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
          else if(options&PACK_UNIFORM_HEIGHT) h=mh;
          else h=child->getDefaultHeight();
          }
        if(hints&LAYOUT_BOTTOM){
          if(bbottom-h<galleytop && bbottom!=galleybottom){
            bleft-=galleywidth;
            bbottom=galleybottom;
            }
          y=bbottom-h;
          bbottom-=(h+vspacing);
          x=bleft+(galleywidth-w)/2;
          }
        else{
          if(ttop+h>galleybottom && ttop!=galleytop){
            tleft=tright;
            tright+=galleywidth;
            ttop=galleytop;
            }
          y=ttop;
          ttop+=(h+vspacing);
          x=tleft+(galleywidth-w)/2;
          }
        child->position(x,y,w,h);
        }
      }
    }

  // Horizontal toolbar
  else{
    galleyheight=0;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(child->isMemberOf(FXMETACLASS(FXToolBarGrip))) h=child->getDefaultHeight();
        else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=mh;
        else h=child->getDefaultHeight();
        if(galleyheight<h) galleyheight=h;
        }
      }
    galleyleft=border+padleft;
    galleyright=width-border-padright;
    galleytop=border+padtop;
    galleybottom=height-border-padbottom;
    ltop=galleytop;
    lbottom=galleytop+galleyheight;
    lleft=galleyleft;
    rtop=galleybottom-galleyheight;
    rright=galleyright;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(child->isMemberOf(FXMETACLASS(FXToolBarGrip))){
          w=child->getDefaultWidth();
          h=galleyheight;
          }
        else{
          if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
          else if(options&PACK_UNIFORM_WIDTH) w=mw;
          else w=child->getDefaultWidth();
          if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
          else if(options&PACK_UNIFORM_HEIGHT) h=mh;
          else h=child->getDefaultHeight();
          }
        if(hints&LAYOUT_RIGHT){
          if(rright-w<galleyleft && rright!=galleyright){
            rtop-=galleyheight;
            rright=galleyright;
            }
          x=rright-w;
          rright-=(w+hspacing);
          y=rtop+(galleyheight-h)/2;
          }
        else{
          if(lleft+w>galleyright && lleft!=galleyleft){
            ltop=lbottom;
            lbottom+=galleyheight;
            lleft=galleyleft;
            }
          x=lleft;
          lleft+=(w+hspacing);
          y=ltop+(galleyheight-h)/2;
          }
        child->position(x,y,w,h);
        }
      }
    }
  flags&=~FLAG_DIRTY;
  }

*/


// Recalculate layout
void FXToolBar::layout(){
  register FXint left,right,top,bottom,remain,expand,mw=0,mh=0,x,y,w,h,e,t;
  register FXWindow *child;
  register FXuint hints;

  // Placement rectangle; right/bottom non-inclusive
  left=border+padleft;
  right=width-border-padright;
  top=border+padtop;
  bottom=height-border-padbottom;

  // Get maximum child size
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();

  // Vertical toolbar
  if(options&LAYOUT_SIDE_LEFT){

    // Find stretch
    for(child=getFirst(),remain=bottom-top,expand=0; child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) h=child->getDefaultHeight();
        else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=mh;
        else h=child->getDefaultHeight();
	if(hints&LAYOUT_FILL_Y)
	  expand+=h;
	else
	  remain-=h;
	remain-=vspacing;
        }
      }

    // Adjust
    remain+=vspacing;

    // Placement
    for(child=getFirst(),e=0; child; child=child->getNext()){
      if(child->shown()){

        hints=child->getLayoutHints();

        // Determine child width
        if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) w=right-left;
        else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=mw;
        else if(hints&LAYOUT_FILL_X) w=right-left;
        else w=child->getDefaultWidth();

        // Determine child x-position
        if(hints&LAYOUT_CENTER_X) x=left+(right-left-w)/2;
        else if(hints&LAYOUT_RIGHT) x=right-w;
        else x=left;

        // Determine child height
        if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) h=child->getDefaultHeight();
        else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=mh;
        else h=child->getDefaultHeight();

        // Account for fill or center
	if(hints&LAYOUT_FILL_Y){
          t=h*remain;
          e+=t%expand;
          h=t/expand+e/expand;
          e%=expand;
          }

        // Determine child x-position
        if(hints&LAYOUT_BOTTOM){
          y=bottom-h;
          bottom-=h+vspacing;
          }
        else{
          y=top;
          top+=h+vspacing;
          }

        // Place it
        child->position(x,y,w,h);
        }
      }
    }

  // Horizontal toolbar
  else{

    // Find stretch
    for(child=getFirst(),remain=right-left,expand=0; child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) w=child->getDefaultWidth();
        else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=mw;
        else w=child->getDefaultWidth();
	if(hints&LAYOUT_FILL_X)
	  expand+=w;
	else
	  remain-=w;
	remain-=hspacing;
        }
      }

    // Adjust
    remain+=hspacing;

    // Placement
    for(child=getFirst(),e=0; child; child=child->getNext()){
      if(child->shown()){

        hints=child->getLayoutHints();

        // Determine child height
        if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) h=bottom-top;
        else if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=mh;
        else if(hints&LAYOUT_FILL_Y) h=bottom-top;
        else h=child->getDefaultHeight();

        // Determine child y-position
        if(hints&LAYOUT_CENTER_Y) y=top+(bottom-top-h)/2;
        else if(hints&LAYOUT_BOTTOM) y=bottom-h;
        else y=top;

        // Determine child width
        if(dynamic_cast<FXSeparator*>(child) || dynamic_cast<FXToolBarGrip*>(child)) w=child->getDefaultWidth();
        else if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=mw;
        else w=child->getDefaultWidth();

        // Account for fill or center
	if(hints&LAYOUT_FILL_X){
          t=w*remain;
          e+=t%expand;
          w=t/expand+e/expand;
          e%=expand;
          }

        // Determine child x-position
        if(hints&LAYOUT_RIGHT){
          x=right-w;
          right-=w+hspacing;
          }
        else{
          x=left;
          left+=w+hspacing;
          }

        // Place it
        child->position(x,y,w,h);
        }
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Dock the bar before other window
void FXToolBar::dock(FXDockSite* docksite,FXWindow* before,FXbool notify){
  FXDockBar::dock(docksite,before,notify);
  setDockingSide(getParent()->getLayoutHints());
  }


// Dock the bar near position in dock site
void FXToolBar::dock(FXDockSite* docksite,FXint localx,FXint localy,FXbool notify){
  FXDockBar::dock(docksite,localx,localy,notify);
  setDockingSide(getParent()->getLayoutHints());
  }


// Flip orientation
long FXToolBar::onCmdDockFlip(FXObject*,FXSelector,void*){
  if(wetdock && !isDocked()){

    // Flip orientation
    if(getDockingSide()&LAYOUT_SIDE_LEFT)
      setDockingSide(LAYOUT_SIDE_TOP);
    else
      setDockingSide(LAYOUT_SIDE_LEFT);

    // Note, this takes wetdock's interpretation of layout hints into account
    wetdock->resize(wetdock->getDefaultWidth(),wetdock->getDefaultHeight());
    }
  return 1;
  }


// Check for flip
long FXToolBar::onUpdDockFlip(FXObject* sender,FXSelector,void*){
  sender->handle(this,isDocked()?FXSEL(SEL_COMMAND,ID_DISABLE):FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


// Change toolbar orientation
void FXToolBar::setDockingSide(FXuint side){
  side&=LAYOUT_SIDE_MASK;
  if((options&LAYOUT_SIDE_MASK)!=side){

    // New orientation is vertical
    if(side&LAYOUT_SIDE_LEFT){
      if(!(options&LAYOUT_SIDE_LEFT)){    // Was horizontal
        if((options&LAYOUT_RIGHT) && (options&LAYOUT_CENTER_X)) side|=LAYOUT_FIX_Y;
        else if(options&LAYOUT_RIGHT) side|=LAYOUT_BOTTOM;
        else if(options&LAYOUT_CENTER_X) side|=LAYOUT_CENTER_Y;
        if(options&LAYOUT_FILL_X){
          if(options&LAYOUT_FILL_Y) side|=LAYOUT_FILL_X;
          side|=LAYOUT_FILL_Y;
          }
        }
      else{                               // Was vertical already
        side|=(options&(LAYOUT_BOTTOM|LAYOUT_CENTER_Y|LAYOUT_FILL_Y));
        }
      }

    // New orientation is horizontal
    else{
      if(options&LAYOUT_SIDE_LEFT){       // Was vertical
        if((options&LAYOUT_BOTTOM) && (options&LAYOUT_CENTER_Y)) side|=LAYOUT_FIX_X;
        else if(options&LAYOUT_BOTTOM) side|=LAYOUT_RIGHT;
        else if(options&LAYOUT_CENTER_Y) side|=LAYOUT_CENTER_X;
        if(options&LAYOUT_FILL_Y){
          if(options&LAYOUT_FILL_X) side|=LAYOUT_FILL_Y;
          side|=LAYOUT_FILL_X;
          }
        }
      else{
        side|=(options&(LAYOUT_RIGHT|LAYOUT_CENTER_X|LAYOUT_FILL_X));
        }
      }

    // Simply preserve these options
    side|=(options&(LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT));

    // Update the layout
    setLayoutHints(side);
    }
  }


// Get toolbar orientation
FXuint FXToolBar::getDockingSide() const {
  return (options&LAYOUT_SIDE_MASK);
  }


}
