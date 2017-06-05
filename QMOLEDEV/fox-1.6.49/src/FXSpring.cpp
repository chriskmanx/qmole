/********************************************************************************
*                                                                               *
*                S p r i n g   C o n t a i n e r   W i d g e t                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXSpring.cpp,v 1.12 2006/01/22 17:58:42 fox Exp $                        *
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
#include "FXDCWindow.h"
#include "FXPacker.h"
#include "FXSpring.h"


/*
  Notes:
  - Based upon an idea from Amanda Ross.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXSpring,FXPacker,NULL,0)


// Create child frame window
FXSpring::FXSpring(FXComposite* p,FXuint opts,FXint relw,FXint relh,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXPacker(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  relWidth=relw;
  relHeight=relh;
  }


// Compute minimum width based on child layout hints
FXint FXSpring::getDefaultWidth(){
  return 0<relWidth ? relWidth : FXPacker::getDefaultWidth();
  }


// Compute minimum height based on child layout hints
FXint FXSpring::getDefaultHeight(){
  return 0<relHeight ? relHeight : FXPacker::getDefaultHeight();
  }


// Change relative width
void FXSpring::setRelativeWidth(FXint relw){
  if(relWidth!=relw){
    relWidth=relw;
    recalc();
    update();
    }
  }


// Change relative height
void FXSpring::setRelativeHeight(FXint relh){
  if(relHeight!=relh){
    relHeight=relh;
    recalc();
    update();
    }
  }


// Save object to stream
void FXSpring::save(FXStream& store) const {
  FXPacker::save(store);
  store << relWidth;
  store << relHeight;
  }


// Load object from stream
void FXSpring::load(FXStream& store){
  FXPacker::load(store);
  store >> relWidth;
  store >> relHeight;
  }

}
