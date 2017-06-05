/********************************************************************************
*                                                                               *
*                             D r a w a b l e   A r e a                         *
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
* $Id: FXDrawable.cpp,v 1.26 2006/01/22 17:58:24 fox Exp $                      *
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
#include "FXApp.h"
#include "FXVisual.h"
#include "FXDrawable.h"

/*
  Notes:

  - Abstract drawable surface.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT_ABSTRACT(FXDrawable,FXId,NULL,0)


// For deserialization
FXDrawable::FXDrawable(){
  visual=(FXVisual*)-1L;
  width=1;
  height=1;
  }


// Initialize nicely
FXDrawable::FXDrawable(FXApp* a,FXint w,FXint h):FXId(a){
  visual=NULL;
  width=FXMAX(w,1);
  height=FXMAX(h,1);
  }


// Change visual
void FXDrawable::setVisual(FXVisual* vis){
  if(!vis){ fxerror("%s::setVisual: NULL visual\n",getClassName()); }
  if(xid){ fxerror("%s::setVisual: visual should be set before calling create()\n",getClassName()); }
  visual=vis;
  }


// Resize drawable to the specified width and height
void FXDrawable::resize(FXint w,FXint h){
  width=FXMAX(w,1);
  height=FXMAX(h,1);
  }


// Save data
void FXDrawable::save(FXStream& store) const {
  FXId::save(store);
  store << visual;
  store << width;
  store << height;
  }


// Load data
void FXDrawable::load(FXStream& store){
  FXId::load(store);
  store >> visual;
  store >> width;
  store >> height;
  }


// Clean up
FXDrawable::~FXDrawable(){
  visual=(FXVisual*)-1L;
  xid=0;
  }

}


