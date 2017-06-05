/********************************************************************************
*                                                                               *
*                       C o l o r   B a r   W i d g e t                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXColorBar.cpp,v 1.31 2006/01/22 17:58:20 fox Exp $                      *
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXDrawable.h"
#include "FXImage.h"
#include "FXColorBar.h"

/*
  Notes:
*/

#define BAR_WIDTH       30
#define BAR_MASK        (COLORBAR_HORIZONTAL|COLORBAR_VERTICAL)

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXColorBar) FXColorBarMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXColorBar::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXColorBar::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXColorBar::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXColorBar::onLeftBtnRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXColorBar::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXColorBar::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorBar::ID_SETHELPSTRING,FXColorBar::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorBar::ID_GETHELPSTRING,FXColorBar::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorBar::ID_SETTIPSTRING,FXColorBar::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXColorBar::ID_GETTIPSTRING,FXColorBar::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXColorBar,FXFrame,FXColorBarMap,ARRAYNUMBER(FXColorBarMap))


// Init
FXColorBar::FXColorBar(){
  flags|=FLAG_ENABLED;
  hsv[0]=0.0f;
  hsv[1]=0.0f;
  hsv[2]=1.0f;
  }


// Make a color bar
FXColorBar::FXColorBar(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  bar=new FXImage(getApp(),NULL,IMAGE_DITHER|IMAGE_KEEP|IMAGE_OWNED|IMAGE_SHMI|IMAGE_SHMP,1,1);
  hsv[0]=0.0f;
  hsv[1]=0.0f;
  hsv[2]=1.0f;
  }


// Create window
void FXColorBar::create(){
  FXFrame::create();
  updatebar();
  bar->create();
  }


// Detach window
void FXColorBar::detach(){
  FXFrame::detach();
  bar->detach();
  }


// Resize the bar
void FXColorBar::layout(){
  register FXint ww,hh;
  ww=width-padleft-padright-(border<<1)-4;
  hh=height-padtop-padbottom-(border<<1)-4;
  if(ww<1) ww=1;
  if(hh<1) hh=1;
  if(bar->getWidth()!=ww || bar->getHeight()!=hh){
    bar->resize(ww,hh);
    updatebar();
    bar->render();
    }
  flags&=~FLAG_DIRTY;
  }


// Recompute the bar image
void FXColorBar::updatebar(){
  register FXint x,y,w,h;
  register FXColor clr;
  FXfloat r,g,b,d;
  w=bar->getWidth();
  h=bar->getHeight();
  if(options&COLORBAR_VERTICAL){
    if(1<h){
      d=1.0f/(h-1.0f);
      for(y=0; y<h; y++){
        fxhsv_to_rgb(r,g,b,hsv[0],hsv[1],1.0f-y*d);
        clr=FXRGB(255.0f*r,255.0f*g,255.0f*b);
        for(x=0; x<w; x++) bar->setPixel(x,y,clr);
        }
      }
    }
  else{
    if(1<w){
      d=1.0f/(w-1.0f);
      for(x=0; x<w; x++){
        fxhsv_to_rgb(r,g,b,hsv[0],hsv[1],x*d);
        clr=FXRGB(255.0f*r,255.0f*g,255.0f*b);
        for(y=0; y<h; y++) bar->setPixel(x,y,clr);
        }
      }
    }
  }


// Get default width
FXint FXColorBar::getDefaultWidth(){
  FXint w=(options&COLORBAR_VERTICAL)?BAR_WIDTH:1;
  return w+4+padleft+padright+(border<<1);
  }


// Get default height
FXint FXColorBar::getDefaultHeight(){
  FXint h=(options&COLORBAR_VERTICAL)?1:BAR_WIDTH;
  return h+4+padtop+padbottom+(border<<1);
  }


// Set help using a message
long FXColorBar::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXColorBar::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXColorBar::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXColorBar::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXColorBar::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXColorBar::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Handle repaint
long FXColorBar::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,padleft,height-(border<<1));
  dc.fillRectangle(width-padright-border,border,padright,height-(border<<1));
  dc.fillRectangle(border+padleft,border,width-padleft-padright-(border<<1),padtop);
  dc.fillRectangle(border+padleft,height-padbottom-border,width-padleft-padright-(border<<1),padbottom);
  dc.drawImage(bar,padleft+border+2,padtop+border+2);
  drawDoubleSunkenRectangle(dc,padleft+border,padtop+border,width-padright-padleft-(border<<1),height-padbottom-padtop-(border<<1));
  drawFrame(dc,0,0,width,height);
  if(options&COLORBAR_VERTICAL)
    drawDoubleRaisedRectangle(dc,border+padleft+2,border+padtop+2+(FXint)((1.0-hsv[2])*(bar->getHeight()-4)),bar->getWidth(),4);
  else
    drawDoubleRaisedRectangle(dc,border+padleft+2+(FXint)(hsv[2]*(bar->getWidth()-4)),border+padtop+2,4,bar->getHeight());
  return 1;
  }


// Moving
long FXColorBar::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint xx,yy,ww,hh,travel,p;
  FXfloat v=hsv[2];
  if(flags&FLAG_PRESSED){
    xx=border+padleft+2;
    yy=border+padtop+2;
    ww=bar->getWidth();
    hh=bar->getHeight();
    if(options&COLORBAR_VERTICAL){
      travel=hh-4;
      p=yy+hh-event->win_y-2;
      }
    else{
      travel=ww-4;
      p=event->win_x-xx-2;
      }
    if(p<0) p=0;
    if(p>travel) p=travel;
    if(0<travel) v=(FXfloat)p/(FXfloat)travel;
    if(v!=hsv[2]){
      hsv[2]=v;
      flags|=FLAG_CHANGED;
      update(xx,yy,ww,hh);
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)hsv);
      }
    flags|=FLAG_CHANGED;
    return 1;
    }
  return 0;
  }


// Move spot to change hue, saturation
long FXColorBar::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint xx,yy,ww,hh,travel,p;
  FXfloat v=hsv[2];
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    xx=border+padleft+2;
    yy=border+padtop+2;
    ww=bar->getWidth();
    hh=bar->getHeight();
    if(options&COLORBAR_VERTICAL){
      travel=hh-4;
      p=yy+hh-event->win_y-2;
      }
    else{
      travel=ww-4;
      p=event->win_x-xx-2;
      }
    if(p<0) p=0;
    if(p>travel) p=travel;
    if(0<travel) v=(FXfloat)p/(FXfloat)travel;
    if(v!=hsv[2]){
      hsv[2]=v;
      flags|=FLAG_CHANGED;
      update(xx,yy,ww,hh);
      if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)hsv);
      }
    flags|=FLAG_PRESSED;
    flags&=~FLAG_UPDATE;
    }
  return 1;
  }


// End spot movement mode
long FXColorBar::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint changed=(flags&FLAG_CHANGED);
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_PRESSED;
    flags&=~FLAG_CHANGED;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(changed && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)hsv);
    return 1;
    }
  return 1;
  }


// Change hue
void FXColorBar::setHue(FXfloat h){
  h=FXCLAMP(0.0f,h,360.0f);
  if(h!=hsv[0]){
    hsv[0]=h;
    updatebar();
    bar->render();
    update(padleft+border+2,padtop+border+2,width-padleft-padright-(border<<1)-4,height-padtop-padbottom-(border<<1)-4);
    }
  }


// Change saturation
void FXColorBar::setSat(FXfloat s){
  s=FXCLAMP(0.0f,s,1.0f);
  if(s!=hsv[1]){
    hsv[1]=s;
    updatebar();
    bar->render();
    update(padleft+border+2,padtop+border+2,width-padleft-padright-(border<<1)-4,height-padtop-padbottom-(border<<1)-4);
    }
  }


// Change saturation
void FXColorBar::setVal(FXfloat v){
  v=FXCLAMP(0.0f,v,1.0f);
  if(v!=hsv[2]){
    hsv[2]=v;
    update(padleft+border+2,padtop+border+2,width-padleft-padright-(border<<1)-4,height-padtop-padbottom-(border<<1)-4);
    }
  }


// Set color bar options
void FXColorBar::setBarStyle(FXuint style){
  FXuint opts=(options&~BAR_MASK) | (style&BAR_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get color bar options
FXuint FXColorBar::getBarStyle() const {
  return (options&BAR_MASK);
  }


// Save data
void FXColorBar::save(FXStream& store) const {
  FXFrame::save(store);
  store << bar;
  store << hsv[0];
  store << hsv[1];
  store << hsv[2];
  store << tip;
  store << help;
  }


// Load data
void FXColorBar::load(FXStream& store){
  FXFrame::load(store);
  store >> bar;
  store >> hsv[0];
  store >> hsv[1];
  store >> hsv[2];
  store >> tip;
  store >> help;
  }


// Destroy
FXColorBar::~FXColorBar(){
  delete bar;
  bar=(FXImage*)-1L;
  }

}
