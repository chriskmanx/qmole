/********************************************************************************
*                                                                               *
*                    M e n u   S e p a r a t o r   W i d g e t                  *
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
* $Id: FXMenuSeparator.cpp,v 1.31 2006/01/22 17:58:36 fox Exp $                 *
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
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXMenuSeparator.h"

/*
  Notes:
  - Accelerators.
  - Help text from constructor is third part; second part should be
    accelerator key combination.
  - When menu label changes, hotkey might have to be adjusted.
  - Fix it so menu stays up when after Alt-F, you press Alt-E.
  - MenuItems should be derived from FXLabel.
  - FXMenuCascade should send ID_POST/IDUNPOST to self.
*/


#define LEADSPACE   22
#define TRAILSPACE  16

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMenuSeparator) FXMenuSeparatorMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMenuSeparator::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXMenuSeparator,FXWindow,FXMenuSeparatorMap,ARRAYNUMBER(FXMenuSeparatorMap))


// Deserialization
FXMenuSeparator::FXMenuSeparator(){
  flags|=FLAG_SHOWN;
  }

// Separator item
FXMenuSeparator::FXMenuSeparator(FXComposite* p,FXuint opts):
  FXWindow(p,opts,0,0,0,0){
  flags|=FLAG_SHOWN;
  defaultCursor=getApp()->getDefaultCursor(DEF_RARROW_CURSOR);
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  }


// Handle repaint
long FXMenuSeparator::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);
  dc.setForeground(shadowColor);
  //dc.drawLine(1,0,width-1,0);
  dc.fillRectangle(1,0,width,1);
  dc.setForeground(hiliteColor);
  //dc.drawLine(1,1,width-1,1);
  dc.fillRectangle(1,1,width,1);
  return 1;
  }


// Get default size
FXint FXMenuSeparator::getDefaultWidth(){
  return LEADSPACE+TRAILSPACE;
  }


FXint FXMenuSeparator::getDefaultHeight(){
  return 2;
  }


// Set highlight color
void FXMenuSeparator::setHiliteColor(FXColor clr){
  if(clr!=hiliteColor){
    hiliteColor=clr;
    update();
    }
  }


// Set shadow color
void FXMenuSeparator::setShadowColor(FXColor clr){
  if(clr!=shadowColor){
    shadowColor=clr;
    update();
    }
  }


// Save object to stream
void FXMenuSeparator::save(FXStream& store) const {
  FXWindow::save(store);
  store << hiliteColor;
  store << shadowColor;
  }


// Load object from stream
void FXMenuSeparator::load(FXStream& store){
  FXWindow::load(store);
  store >> hiliteColor;
  store >> shadowColor;
  }

}

