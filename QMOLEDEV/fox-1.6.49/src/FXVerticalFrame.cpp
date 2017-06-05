/********************************************************************************
*                                                                               *
*                 V e r t i c a l   C o n t a i n e r   O b j e c t             *
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
* $Id: FXVerticalFrame.cpp,v 1.28 2006/01/22 17:58:51 fox Exp $                 *
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
#include "FXVerticalFrame.h"


/*
  Notes:
  - Filled items shrink as well as stretch.
  - Stretch is proportional to default size; this way, at default size,
    it is exactly correct.
  - Tabbing order takes widget layout into account
*/

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXVerticalFrame) FXVerticalFrameMap[]={
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXVerticalFrame::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXVerticalFrame::onFocusDown),
  };


// Object implementation
FXIMPLEMENT(FXVerticalFrame,FXPacker,FXVerticalFrameMap,ARRAYNUMBER(FXVerticalFrameMap))


// Make a vertical one
FXVerticalFrame::FXVerticalFrame(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXPacker(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  }


// Compute minimum width based on child layout hints
FXint FXVerticalFrame::getDefaultWidth(){
  register FXint w,wmax,wcum,mw;
  register FXWindow* child;
  register FXuint hints;
  wmax=wcum=mw=0;
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      if((hints&LAYOUT_RIGHT)&&(hints&LAYOUT_CENTER_X)){        // LAYOUT_FIX_X
        w=child->getX()+w;
        if(w>wmax) wmax=w;
        }
      else{
        if(w>wcum) wcum=w;
        }
      }
    }
  wcum+=padleft+padright+(border<<1);
  return FXMAX(wcum,wmax);
  }


// Compute minimum height based on child layout hints
FXint FXVerticalFrame::getDefaultHeight(){
  register FXint h,hcum,hmax,mh;
  register FXWindow* child;
  register FXuint hints;
  hcum=hmax=mh=0;
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      if((hints&LAYOUT_BOTTOM)&&(hints&LAYOUT_CENTER_Y)){       // LAYOUT_FIX_Y
        h=child->getY()+h;
        if(h>hmax) hmax=h;
        }
      else{
        if(hcum) hcum+=vspacing;
        hcum+=h;
        }
      }
    }
  hcum+=padtop+padbottom+(border<<1);
  return FXMAX(hcum,hmax);
  }


// Recalculate layout
void FXVerticalFrame::layout(){
  register FXint left,right,top,bottom,remain,extra_space,total_space,t,x,y,w,h;
  register FXWindow* child;
  register FXint sumexpand=0;
  register FXint numexpand=0;
  register FXint mw=0;
  register FXint mh=0;
  register FXint e=0;
  register FXuint hints;

  // Placement rectangle; right/bottom non-inclusive
  left=border+padleft;
  right=width-border-padright;
  top=border+padtop;
  bottom=height-border-padbottom;
  remain=bottom-top;

  // Get maximum child size
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();

  // Find number of paddable children and total height
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(!((hints&LAYOUT_BOTTOM)&&(hints&LAYOUT_CENTER_Y))){    // LAYOUT_FIX_Y
        if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=mh;
        else h=child->getDefaultHeight();
        FXASSERT(h>=0);
        if((hints&LAYOUT_CENTER_Y) || ((hints&LAYOUT_FILL_Y) && !(hints&LAYOUT_FIX_HEIGHT))){
          sumexpand+=h;
          numexpand+=1;
          }
        else{
          remain-=h;
          }
        remain-=vspacing;
        }
      }
    }

  // Child spacing correction
  remain+=vspacing;

  // Do the layout
  for(child=getFirst(); child; child=child->getNext()){
    if(child->shown()){
      hints=child->getLayoutHints();

      // Determine child width
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else if(hints&LAYOUT_FILL_X) w=right-left;
      else w=child->getDefaultWidth();

      // Determine child x-position
      if((hints&LAYOUT_RIGHT)&&(hints&LAYOUT_CENTER_X)) x=child->getX();
      else if(hints&LAYOUT_CENTER_X) x=left+(right-left-w)/2;
      else if(hints&LAYOUT_RIGHT) x=right-w;
      else x=left;

      // Layout child in Y
      y=child->getY();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      if(!((hints&LAYOUT_BOTTOM)&&(hints&LAYOUT_CENTER_Y))){    // LAYOUT_FIX_Y
        extra_space=0;
        total_space=0;
        if((hints&LAYOUT_FILL_Y) && !(hints&LAYOUT_FIX_HEIGHT)){
          if(sumexpand>0){                            // Divide space proportionally to height
            t=h*remain;
            FXASSERT(sumexpand>0);
            h=t/sumexpand;
            e+=t%sumexpand;
            if(e>=sumexpand){h++;e-=sumexpand;}
            }
          else{                                       // Divide the space equally
            FXASSERT(numexpand>0);
            h=remain/numexpand;
            e+=remain%numexpand;
            if(e>=numexpand){h++;e-=numexpand;}
            }
          }
        else if(hints&LAYOUT_CENTER_Y){
          if(sumexpand>0){                            // Divide space proportionally to height
            t=h*remain;
            FXASSERT(sumexpand>0);
            total_space=t/sumexpand-h;
            e+=t%sumexpand;
            if(e>=sumexpand){total_space++;e-=sumexpand;}
            }
          else{                                       // Divide the space equally
            FXASSERT(numexpand>0);
            total_space=remain/numexpand-h;
            e+=remain%numexpand;
            if(e>=numexpand){total_space++;e-=numexpand;}
            }
          extra_space=total_space/2;
          }
        if(hints&LAYOUT_BOTTOM){
          y=bottom-h-extra_space;
          bottom=bottom-h-hspacing-total_space;
          }
        else{
          y=top+extra_space;
          top=top+h+vspacing+total_space;
          }
        }
      child->position(x,y,w,h);
      }
    }
  flags&=~FLAG_DIRTY;
  }

}

