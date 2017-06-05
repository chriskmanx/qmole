/********************************************************************************
*                                                                               *
*                        F r a m e   W i n d o w   O b j e c t                  *
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
* $Id: FXFrame.cpp,v 1.37 2006/01/22 17:58:27 fox Exp $                         *
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
#include "FXDCWindow.h"
#include "FXFrame.h"

/*
  Notes:
  - This really should become the base class for everything that has
    a border.
*/


// Frame styles
#define FRAME_MASK        (FRAME_SUNKEN|FRAME_RAISED|FRAME_THICK)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXFrame) FXFrameMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXFrame::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXFrame,FXWindow,FXFrameMap,ARRAYNUMBER(FXFrameMap))


// Deserialization
FXFrame::FXFrame(){
  flags|=FLAG_SHOWN;
  baseColor=0;
  hiliteColor=0;
  shadowColor=0;
  borderColor=0;
  border=0;
  }


// Create child frame window
FXFrame::FXFrame(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXWindow(p,opts,x,y,w,h){
  flags|=FLAG_SHOWN;
  backColor=getApp()->getBaseColor();
  baseColor=getApp()->getBaseColor();
  hiliteColor=getApp()->getHiliteColor();
  shadowColor=getApp()->getShadowColor();
  borderColor=getApp()->getBorderColor();
  padtop=pt;
  padbottom=pb;
  padleft=pl;
  padright=pr;
  border=(options&FRAME_THICK)?2:(options&(FRAME_SUNKEN|FRAME_RAISED))?1:0;
  }


// Get default width
FXint FXFrame::getDefaultWidth(){
  return padleft+padright+(border<<1);
  }


// Get default height
FXint FXFrame::getDefaultHeight(){
  return padtop+padbottom+(border<<1);
  }


void FXFrame::drawBorderRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  dc.setForeground(borderColor);
  dc.drawRectangle(x,y,w-1,h-1);
  }


void FXFrame::drawRaisedRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    }
  }


void FXFrame::drawSunkenRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    }
  }


void FXFrame::drawRidgeRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    if(1<w && 1<h){
      dc.setForeground(hiliteColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      dc.setForeground(shadowColor);
      dc.fillRectangle(x+1,y+1,w-3,1);
      dc.fillRectangle(x+1,y+1,1,h-3);
      }
    }
  }


void FXFrame::drawGrooveRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    if(1<w && 1<h){
      dc.setForeground(shadowColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      dc.setForeground(hiliteColor);
      dc.fillRectangle(x+1,y+1,w-3,1);
      dc.fillRectangle(x+1,y+1,1,h-3);
      }
    }
  }


void FXFrame::drawDoubleRaisedRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(borderColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y,w-1,1);
    dc.fillRectangle(x,y,1,h-1);
    if(1<w && 1<h){
      dc.setForeground(baseColor);
      dc.fillRectangle(x+1,y+1,w-2,1);
      dc.fillRectangle(x+1,y+1,1,h-2);
      dc.setForeground(shadowColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      }
    }
  }

void FXFrame::drawDoubleSunkenRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  if(0<w && 0<h){
    dc.setForeground(hiliteColor);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);
    dc.setForeground(shadowColor);
    dc.fillRectangle(x,y,w-1,1);
    dc.fillRectangle(x,y,1,h-1);
    if(1<w && 1<h){
      dc.setForeground(borderColor);
      dc.fillRectangle(x+1,y+1,w-3,1);
      dc.fillRectangle(x+1,y+1,1,h-3);
      dc.setForeground(baseColor);
      dc.fillRectangle(x+1,y+h-2,w-2,1);
      dc.fillRectangle(x+w-2,y+1,1,h-2);
      }
    }
  }


// Draw border
void FXFrame::drawFrame(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  switch(options&FRAME_MASK){
    case FRAME_LINE: drawBorderRectangle(dc,x,y,w,h); break;
    case FRAME_SUNKEN: drawSunkenRectangle(dc,x,y,w,h); break;
    case FRAME_RAISED: drawRaisedRectangle(dc,x,y,w,h); break;
    case FRAME_GROOVE: drawGrooveRectangle(dc,x,y,w,h); break;
    case FRAME_RIDGE: drawRidgeRectangle(dc,x,y,w,h); break;
    case FRAME_SUNKEN|FRAME_THICK: drawDoubleSunkenRectangle(dc,x,y,w,h); break;
    case FRAME_RAISED|FRAME_THICK: drawDoubleRaisedRectangle(dc,x,y,w,h); break;
    }
  }


// Handle repaint
long FXFrame::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=static_cast<FXEvent*>(ptr);
  FXDCWindow dc(this,event);
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Change frame border style
void FXFrame::setFrameStyle(FXuint style){
  FXuint opts=(options&~FRAME_MASK) | (style&FRAME_MASK);
  if(options!=opts){
    FXint b=(opts&FRAME_THICK) ? 2 : (opts&(FRAME_SUNKEN|FRAME_RAISED)) ? 1 : 0;
    options=opts;
    if(border!=b){
      border=b;
      recalc();
      }
    update();
    }
  }


// Get frame style
FXuint FXFrame::getFrameStyle() const {
  return (options&FRAME_MASK);
  }


// Set base color
void FXFrame::setBaseColor(FXColor clr){
  if(clr!=baseColor){
    baseColor=clr;
    update();
    }
  }


// Set highlight color
void FXFrame::setHiliteColor(FXColor clr){
  if(clr!=hiliteColor){
    hiliteColor=clr;
    update();
    }
  }


// Set shadow color
void FXFrame::setShadowColor(FXColor clr){
  if(clr!=shadowColor){
    shadowColor=clr;
    update();
    }
  }


// Set border color
void FXFrame::setBorderColor(FXColor clr){
  if(clr!=borderColor){
    borderColor=clr;
    update();
    }
  }


// Change top padding
void FXFrame::setPadTop(FXint pt){
  if(padtop!=pt){
    padtop=pt;
    recalc();
    update();
    }
  }


// Change bottom padding
void FXFrame::setPadBottom(FXint pb){
  if(padbottom!=pb){
    padbottom=pb;
    recalc();
    update();
    }
  }


// Change left padding
void FXFrame::setPadLeft(FXint pl){
  if(padleft!=pl){
    padleft=pl;
    recalc();
    update();
    }
  }


// Change right padding
void FXFrame::setPadRight(FXint pr){
  if(padright!=pr){
    padright=pr;
    recalc();
    update();
    }
  }


// Save data
void FXFrame::save(FXStream& store) const {
  FXWindow::save(store);
  store << baseColor;
  store << hiliteColor;
  store << shadowColor;
  store << borderColor;
  store << padtop;
  store << padbottom;
  store << padleft;
  store << padright;
  store << border;
  }


// Load data
void FXFrame::load(FXStream& store){
  FXWindow::load(store);
  store >> baseColor;
  store >> hiliteColor;
  store >> shadowColor;
  store >> borderColor;
  store >> padtop;
  store >> padbottom;
  store >> padleft;
  store >> padright;
  store >> border;
  }


}

