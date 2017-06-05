/********************************************************************************
*                                                                               *
*             M u l t i p l e   D o c u m e n t   B u t t o n                   *
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
* $Id: FXMDIButton.cpp,v 1.28 2006/01/22 17:58:33 fox Exp $                     *
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
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXGIFIcon.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXMenuButton.h"
#include "FXComposite.h"
#include "FXShell.h"
#include "FXPopup.h"
#include "FXMenuPane.h"
#include "FXMenuCaption.h"
#include "FXMenuCommand.h"
#include "FXScrollBar.h"
#include "FXScrollArea.h"
#include "FXMDIButton.h"
#include "FXMDIChild.h"
#include "FXMDIClient.h"
#include "icons.h"

#define MENUBUTTONWINDOW_WIDTH  16
#define MENUBUTTONWINDOW_HEIGHT 14

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMDIDeleteButton) FXMDIDeleteButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMDIDeleteButton::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXMDIDeleteButton,FXButton,FXMDIDeleteButtonMap,ARRAYNUMBER(FXMDIDeleteButtonMap))



// Make delete button
FXMDIDeleteButton::FXMDIDeleteButton(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXButton(p,"\tClose\tClose Window.",NULL,tgt,sel,opts,x,y,w,h,3,3,2,2){
  }


FXint FXMDIDeleteButton::getDefaultWidth(){
  return padleft+padright+8+(border<<1);
  }


FXint FXMDIDeleteButton::getDefaultHeight(){
  return padtop+padbottom+8+(border<<1);
  }


// Handle repaint
long FXMDIDeleteButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  FXint xx,yy;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){
    if(state==STATE_UP){
      if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
      else drawRaisedRectangle(dc,0,0,width,height);
      }
    else{
      if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width-1,height-1);
      else drawSunkenRectangle(dc,0,0,width,height);
      }
    }
  xx=(width-8)/2;
  yy=(height-8)/2;
  if(state){ ++xx; ++yy; }
  if(isEnabled())
    dc.setForeground(textColor);
  else
    dc.setForeground(shadowColor);
#ifndef WIN32
  dc.drawLine(xx,  yy,  xx+8,yy+8);
  dc.drawLine(xx+1,yy,  xx+8,yy+7);
  dc.drawLine(xx,  yy+1,xx+7,yy+8);
  dc.drawLine(xx+8,yy,  xx,  yy+8);
  dc.drawLine(xx+8,yy+1,xx+1,yy+8);
  dc.drawLine(xx+7,yy,  xx,  yy+7);
#else
  dc.drawLine(xx,  yy,  xx+9,yy+9);
  dc.drawLine(xx+1,yy,  xx+9,yy+8);
  dc.drawLine(xx,  yy+1,xx+8,yy+9);
  dc.drawLine(xx,  yy+8,xx+9,yy-1);
  dc.drawLine(xx+1,yy+8,xx+9,  yy);
  dc.drawLine(xx,  yy+7,xx+8,yy-1);
#endif
  return 1;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXMDIRestoreButton) FXMDIRestoreButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMDIRestoreButton::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXMDIRestoreButton,FXButton,FXMDIRestoreButtonMap,ARRAYNUMBER(FXMDIRestoreButtonMap))



// Make restore button
FXMDIRestoreButton::FXMDIRestoreButton(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXButton(p,"\tRestore\tRestore Window.",NULL,tgt,sel,opts,x,y,w,h,3,3,2,2){
  }


FXint FXMDIRestoreButton::getDefaultWidth(){
  return padleft+padright+8+(border<<1);
  }


FXint FXMDIRestoreButton::getDefaultHeight(){
  return padtop+padbottom+8+(border<<1);
  }


// Handle repaint
long FXMDIRestoreButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  FXint xx,yy;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){
    if(state==STATE_UP){
      if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
      else drawRaisedRectangle(dc,0,0,width,height);
      }
    else{
      if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width-1,height-1);
      else drawSunkenRectangle(dc,0,0,width,height);
      }
    }
  xx=(width-8)/2;
  yy=(height-8)/2;
  if(state){ ++xx; ++yy; }
  if(isEnabled())
    dc.setForeground(textColor);
  else
    dc.setForeground(shadowColor);
  dc.fillRectangle(xx+3,yy,6,2);
  dc.drawRectangle(xx+3,yy,6,5);
  dc.setForeground(backColor);
  dc.fillRectangle(xx,yy+3,6,5);
  if(isEnabled())
    dc.setForeground(textColor);
  else
    dc.setForeground(shadowColor);
  dc.fillRectangle(xx,yy+3,6,2);
  dc.drawRectangle(xx,yy+3,6,5);
  return 1;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXMDIMaximizeButton) FXMDIMaximizeButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMDIMaximizeButton::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXMDIMaximizeButton,FXButton,FXMDIMaximizeButtonMap,ARRAYNUMBER(FXMDIMaximizeButtonMap))



// Make maximize button
FXMDIMaximizeButton::FXMDIMaximizeButton(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXButton(p,"\tMaximize\tMaximize Window.",NULL,tgt,sel,opts,x,y,w,h,3,3,2,2){
  }

FXint FXMDIMaximizeButton::getDefaultWidth(){
  return padleft+padright+8+(border<<1);
  }


FXint FXMDIMaximizeButton::getDefaultHeight(){
  return padtop+padbottom+8+(border<<1);
  }


// Handle repaint
long FXMDIMaximizeButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  FXint xx,yy;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){
    if(state==STATE_UP){
      if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
      else drawRaisedRectangle(dc,0,0,width,height);
      }
    else{
      if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width-1,height-1);
      else drawSunkenRectangle(dc,0,0,width,height);
      }
    }
  xx=(width-8)/2;
  yy=(height-8)/2;
  if(state){ ++xx; ++yy; }
  if(isEnabled())
    dc.setForeground(textColor);
  else
    dc.setForeground(shadowColor);
  dc.fillRectangle(xx,yy,8,2);
  dc.drawRectangle(xx,yy,8,8);
  return 1;
  }


/*******************************************************************************/

// Map
FXDEFMAP(FXMDIMinimizeButton) FXMDIMinimizeButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMDIMinimizeButton::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXMDIMinimizeButton,FXButton,FXMDIMinimizeButtonMap,ARRAYNUMBER(FXMDIMinimizeButtonMap))



// Make minimize button
FXMDIMinimizeButton::FXMDIMinimizeButton(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXButton(p,"\tMinimize\tMinimize Window.",NULL,tgt,sel,opts,x,y,w,h,3,3,2,2){
  }


FXint FXMDIMinimizeButton::getDefaultWidth(){
  return padleft+padright+8+(border<<1);
  }


FXint FXMDIMinimizeButton::getDefaultHeight(){
  return padtop+padbottom+8+(border<<1);
  }


// Handle repaint
long FXMDIMinimizeButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  FXint xx,yy;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  if(options&(FRAME_RAISED|FRAME_SUNKEN)){
    if(state==STATE_UP){
      if(options&FRAME_THICK) drawDoubleRaisedRectangle(dc,0,0,width,height);
      else drawRaisedRectangle(dc,0,0,width,height);
      }
    else{
      if(options&FRAME_THICK) drawDoubleSunkenRectangle(dc,0,0,width-1,height-1);
      else drawSunkenRectangle(dc,0,0,width,height);
      }
    }
  xx=(width-8)/2;
  yy=(height-8)/2;
  if(state){ ++xx; ++yy; }
  if(isEnabled())
    dc.setForeground(textColor);
  else
    dc.setForeground(shadowColor);
  dc.fillRectangle(xx,yy+6,8,2);
  return 1;
  }


/*******************************************************************************/


// Map
FXDEFMAP(FXMDIWindowButton) FXMDIWindowButtonMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXMDIWindowButton::onPaint),
  };


// Object implementation
FXIMPLEMENT(FXMDIWindowButton,FXMenuButton,FXMDIWindowButtonMap,ARRAYNUMBER(FXMDIWindowButtonMap))



// Make window button
FXMDIWindowButton::FXMDIWindowButton(FXComposite* p,FXPopup* pup,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXMenuButton(p,FXString::null,NULL,pup,opts,x,y,w,h,0,0,0,0){
  tip="Menu";
  target=tgt;
  message=sel;
  }


FXint FXMDIWindowButton::getDefaultWidth(){
  FXint w=icon?icon->getWidth():MENUBUTTONWINDOW_WIDTH;
  return padleft+padright+(border<<1)+w;
  }


FXint FXMDIWindowButton::getDefaultHeight(){
  FXint h=icon?icon->getHeight():MENUBUTTONWINDOW_HEIGHT;
  return padtop+padbottom+(border<<1)+h;
  }


// Handle repaint
long FXMDIWindowButton::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  FXint ix,iy,iw,ih;
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  drawFrame(dc,0,0,width,height);
  if(icon){
    iw=icon->getWidth();
    ih=icon->getHeight();
    }
  else{
    iw=MENUBUTTONWINDOW_WIDTH;
    ih=MENUBUTTONWINDOW_HEIGHT;
    }
  ix=(width-iw)/2;
  iy=(height-ih)/2;
  if(icon){
    dc.drawIcon(icon,ix,iy);
    }
  else{
    dc.setForeground(shadowColor);
    dc.drawLine(ix,iy,MENUBUTTONWINDOW_WIDTH-1,iy);
    dc.drawLine(ix,iy,ix,MENUBUTTONWINDOW_HEIGHT-1);
    dc.drawLine(ix,iy+MENUBUTTONWINDOW_HEIGHT-2,ix+MENUBUTTONWINDOW_WIDTH-2,iy+MENUBUTTONWINDOW_HEIGHT-2);
    dc.drawLine(ix+1,iy+4,ix+MENUBUTTONWINDOW_WIDTH-2,iy+4);
    dc.drawLine(ix+MENUBUTTONWINDOW_WIDTH-2,iy,ix+MENUBUTTONWINDOW_WIDTH-2,iy+MENUBUTTONWINDOW_HEIGHT-2);
    dc.setForeground(baseColor);
    dc.drawLine(ix+1,iy+1,MENUBUTTONWINDOW_WIDTH-3,iy+1);
    dc.drawLine(ix+1,iy+1,ix+1,MENUBUTTONWINDOW_HEIGHT-3);
    dc.setForeground(textColor);
    dc.drawLine(ix,iy+MENUBUTTONWINDOW_HEIGHT-1,ix+MENUBUTTONWINDOW_WIDTH,iy+MENUBUTTONWINDOW_HEIGHT-1);
    dc.drawLine(ix+MENUBUTTONWINDOW_WIDTH-1,iy,ix+MENUBUTTONWINDOW_WIDTH-1,iy+MENUBUTTONWINDOW_HEIGHT-1);
    dc.drawLine(ix+2,iy+2,ix+MENUBUTTONWINDOW_WIDTH-2,iy+2);
    dc.drawLine(ix+2,iy+3,ix+MENUBUTTONWINDOW_WIDTH-2,iy+3);
    dc.setForeground(hiliteColor);
    dc.fillRectangle(ix+2,iy+5,MENUBUTTONWINDOW_WIDTH-4,MENUBUTTONWINDOW_HEIGHT-7);
    dc.drawPoint(ix+MENUBUTTONWINDOW_WIDTH-4,iy+3);
    dc.drawPoint(ix+MENUBUTTONWINDOW_WIDTH-6,iy+3);
    dc.drawPoint(ix+MENUBUTTONWINDOW_WIDTH-8,iy+3);
    }
  return 1;
  }




/*******************************************************************************/


// Object implementation
FXIMPLEMENT(FXMDIMenu,FXMenuPane,NULL,0)


// Convenience constructor
FXMDIMenu::FXMDIMenu(FXWindow *owner,FXObject* tgt):FXMenuPane(owner){
  closeicon=new FXGIFIcon(getApp(),winclose);
  maximizeicon=new FXGIFIcon(getApp(),winmaximize);
  minimizeicon=new FXGIFIcon(getApp(),winminimize);
  restoreicon=new FXGIFIcon(getApp(),winrestore);
  new FXMenuCommand(this,"&Next\t\tNext window.",NULL,tgt,FXWindow::ID_MDI_NEXT,0);
  new FXMenuCommand(this,"&Previous\t\tPrevious window.",NULL,tgt,FXWindow::ID_MDI_PREV,0);
  new FXMenuCommand(this,"&Restore\t\tRestore window.",restoreicon,tgt,FXWindow::ID_MDI_RESTORE,0);
  new FXMenuCommand(this,"&Minimize\t\tMinimize window.",minimizeicon,tgt,FXWindow::ID_MDI_MINIMIZE,0);
  new FXMenuCommand(this,"&Maximize\t\tMaximize window.",maximizeicon,tgt,FXWindow::ID_MDI_MAXIMIZE,0);
  new FXMenuCommand(this,"&Close\t\tClose window.",closeicon,tgt,FXWindow::ID_MDI_CLOSE,0);
  }


// Clean up
FXMDIMenu::~FXMDIMenu(){
  delete closeicon;
  delete maximizeicon;
  delete minimizeicon;
  delete restoreicon;
  closeicon=(FXIcon*)-1L;
  maximizeicon=(FXIcon*)-1L;
  minimizeicon=(FXIcon*)-1L;
  restoreicon=(FXIcon*)-1L;
  }

}

