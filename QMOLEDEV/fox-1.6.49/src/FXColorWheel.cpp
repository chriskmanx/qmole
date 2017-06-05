/********************************************************************************
*                                                                               *
*                        C o l o r W h e e l   W i d g e t                      *
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
* $Id: FXColorWheel.cpp,v 1.49 2006/01/22 17:58:20 fox Exp $                    *
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
#include "FXImage.h"
#include "FXColorWheel.h"

/*
  Notes:
  - We assume the dial is round.
*/

#define WHEELDIAMETER  60   // Default Wheel diameter

using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXColorWheel) FXColorWheelMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXColorWheel::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXColorWheel::onMotion),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXColorWheel::onMouseWheel),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXColorWheel::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXColorWheel::onLeftBtnRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXColorWheel::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXColorWheel::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorWheel::ID_SETHELPSTRING,FXColorWheel::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorWheel::ID_GETHELPSTRING,FXColorWheel::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorWheel::ID_SETTIPSTRING,FXColorWheel::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXColorWheel::ID_GETTIPSTRING,FXColorWheel::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXColorWheel,FXFrame,FXColorWheelMap,ARRAYNUMBER(FXColorWheelMap))


// Make a color wheel
FXColorWheel::FXColorWheel(){
  flags|=FLAG_ENABLED;
  hsv[0]=0.0f;
  hsv[1]=0.0f;
  hsv[2]=1.0f;
  dialx=0;
  dialy=0;
  spotx=0;
  spoty=0;
  }


// Make a color wheel
FXColorWheel::FXColorWheel(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  dial=new FXImage(getApp(),NULL,IMAGE_DITHER|IMAGE_KEEP|IMAGE_OWNED|IMAGE_SHMI|IMAGE_SHMP,WHEELDIAMETER,WHEELDIAMETER);
  hsv[0]=0.0f;
  hsv[1]=0.0f;
  hsv[2]=1.0f;
  dialx=0;
  dialy=0;
  spotx=WHEELDIAMETER/2;
  spoty=WHEELDIAMETER/2;
  }


// Create window
void FXColorWheel::create(){
  FXFrame::create();
  updatedial();
  dial->create();
  }


// Detach window
void FXColorWheel::detach(){
  FXFrame::detach();
  dial->detach();
  }


// Get default width
FXint FXColorWheel::getDefaultWidth(){
  return WHEELDIAMETER+padleft+padright+(border<<1);
  }


// Get default height
FXint FXColorWheel::getDefaultHeight(){
  return WHEELDIAMETER+padtop+padbottom+(border<<1);
  }


// Resize the dial
void FXColorWheel::layout(){
  register FXint ww,hh,ss;
  ww=width-padleft-padright-(border<<1);
  hh=height-padtop-padbottom-(border<<1);
  ss=FXMAX(3,FXMIN(ww,hh));
  dialx=border+padleft+(ww-ss)/2;
  dialy=border+padtop+(hh-ss)/2;
  if((dial->getWidth()!=ss) || (flags&FLAG_DIRTY)){
    if(dial->getWidth()!=ss) dial->resize(ss,ss);
    updatedial();
    dial->render();
    }
  hstoxy(spotx,spoty,hsv[0],hsv[1]);
  flags&=~FLAG_DIRTY;
  }


// Compute x,y location from hue and saturation
FXbool FXColorWheel::hstoxy(FXint& x,FXint& y,FXfloat h,FXfloat s) const {
  register FXfloat r=dial->getWidth()*0.5f;
  register FXfloat a=(h-180.0f)*DTOR;
  x=(FXint)(s*r*cosf(a)+r+0.5f);
  y=(FXint)(s*r*sinf(a)+r+0.5f);
  return TRUE;
  }


// Compute hue and saturation from x,y, return FALSE if outside of dial
FXbool FXColorWheel::xytohs(FXfloat& h,FXfloat& s,FXint x,FXint y) const {
  register FXfloat r=dial->getWidth()*0.5f;
  register FXfloat rx=x-r;
  register FXfloat ry=y-r;
  register FXfloat v=sqrtf(rx*rx+ry*ry);
  h=0.0f;
  s=0.0f;
  if(0.0f<v){
    h=atan2f(ry,rx)*RTOD+180.0f;
    if(v<r){
      s=v/r;
      return TRUE;
      }
    s=1.0f;
    }
  return FALSE;
  }


// Recompute the dial image
void FXColorWheel::updatedial(){
  FXfloat h,s,r,g,b;
  for(register FXint y=0; y<dial->getHeight(); y++){
    for(register FXint x=0; x<dial->getWidth(); x++){
      if(xytohs(h,s,x,y)){
        fxhsv_to_rgb(r,g,b,h,s,hsv[2]);
        dial->setPixel(x,y,FXRGB(255.0f*r,255.0f*g,255.0f*b));
        }
      else{
        dial->setPixel(x,y,backColor);
        }
      }
    }
  }


// Move the spot
void FXColorWheel::movespot(FXint x,FXint y){
  if(spotx!=x || spoty!=y){
    update(dialx+spotx-4,dialy+spoty-4,9,9);
    xytohs(hsv[0],hsv[1],x,y);
    hstoxy(spotx,spoty,hsv[0],hsv[1]);
    update(dialx+spotx-4,dialy+spoty-4,9,9);
    }
  }


// Set help using a message
long FXColorWheel::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXColorWheel::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXColorWheel::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXColorWheel::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXColorWheel::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXColorWheel::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Handle repaint
long FXColorWheel::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint d=dial->getWidth();
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,dialx-border,height-(border<<1));
  dc.fillRectangle(dialx+dial->getWidth(),border,width-border-dialx-dial->getWidth(),height-(border<<1));
  dc.fillRectangle(dialx,border,dial->getWidth(),dialy-border);
  dc.fillRectangle(dialx,dialy+dial->getHeight(),dial->getWidth(),height-border-dialy-dial->getHeight());
  dc.drawImage(dial,dialx,dialy);
  dc.setForeground(borderColor);
  dc.drawArc(dialx+1,dialy,d,d,90*64,45*64);
  dc.drawArc(dialx,dialy+1,d,d,135*64,45*64);
  dc.setForeground(baseColor);
  dc.drawArc(dialx-1,dialy,d,d,270*64,45*64);
  dc.drawArc(dialx,dialy-1,d,d,315*64,45*64);
  dc.setForeground(shadowColor);
  dc.drawArc(dialx,dialy,d,d,45*64,180*64);
  dc.setForeground(hiliteColor);
  dc.drawArc(dialx,dialy,d,d,225*64,180*64);
  dc.setForeground(FXRGB(255,255,255));
  dc.fillArc(dialx+spotx-3,dialy+spoty-3,7,7,0,360*64);
  dc.setForeground(FXRGB(0,0,0));
  dc.fillArc(dialx+spotx-2,dialy+spoty-2,5,5,0,360*64);
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Moving
long FXColorWheel::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(flags&FLAG_PRESSED){
    movespot(event->win_x-dialx,event->win_y-dialy);
    flags|=FLAG_CHANGED;
    if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)hsv);
    return 1;
    }
  return 0;
  }


// Move spot to change hue, saturation
long FXColorWheel::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    movespot(event->win_x-dialx,event->win_y-dialy);
    flags|=FLAG_CHANGED;
    flags&=~FLAG_UPDATE;
    flags|=FLAG_PRESSED;
    if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)hsv);
    }
  return 1;
  }


// End spot movement mode
long FXColorWheel::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
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


// Rotate hue by means of dial
long FXColorWheel::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXfloat amount=((FXEvent*)ptr)->code/12.0f;
  if(isEnabled()){
    if(((FXEvent*)ptr)->state&CONTROLMASK) amount*=0.1f;
    setHue(fmodf(hsv[0]+amount+360.0f,360.0f));
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)hsv);
    return 1;
    }
  return 0;
  }


// Change hue
void FXColorWheel::setHue(FXfloat h){
  h=FXCLAMP(0.0f,h,360.0f);
  if(h!=hsv[0]){
    hsv[0]=h;
    update(dialx+spotx-4,dialy+spoty-4,9,9);
    hstoxy(spotx,spoty,hsv[0],hsv[1]);
    update(dialx+spotx-4,dialy+spoty-4,9,9);
    }
  }


// Change saturation
void FXColorWheel::setSat(FXfloat s){
  s=FXCLAMP(0.0f,s,1.0f);
  if(s!=hsv[1]){
    hsv[1]=s;
    update(dialx+spotx-4,dialy+spoty-4,9,9);
    hstoxy(spotx,spoty,hsv[0],hsv[1]);
    update(dialx+spotx-4,dialy+spoty-4,9,9);
    }
  }


// Change saturation
void FXColorWheel::setVal(FXfloat v){
  v=FXCLAMP(0.0f,v,1.0f);
  if(v!=hsv[2]){
    hsv[2]=v;
    recalc();
    }
  }


// Set hue, saturation, value
void FXColorWheel::setHueSatVal(FXfloat h,FXfloat s,FXfloat v){

  // Clamp
  h=FXCLAMP(0.0f,h,360.0f);
  s=FXCLAMP(0.0f,s,1.0f);
  v=FXCLAMP(0.0f,v,1.0f);

  // Changed after clamping?
  if(hsv[0]!=h || hsv[1]!=s || hsv[2]!=v){

    // Cheap case: just move the ball
    if(hsv[0]!=h || hsv[1]!=s){
      hsv[0]=h;
      hsv[1]=s;
      update(dialx+spotx-4,dialy+spoty-4,9,9);
      hstoxy(spotx,spoty,hsv[0],hsv[1]);
      update(dialx+spotx-4,dialy+spoty-4,9,9);
      }

    // Expensive case: recalculate dial
    if(hsv[2]!=v){
      hsv[2]=v;
      recalc();
      }
    }
  }


// Save data
void FXColorWheel::save(FXStream& store) const {
  FXFrame::save(store);
  store << dial;
  store << hsv[0];
  store << hsv[1];
  store << hsv[2];
  store << tip;
  store << help;
  }


// Load data
void FXColorWheel::load(FXStream& store){
  FXFrame::load(store);
  store >> dial;
  store >> hsv[0];
  store >> hsv[1];
  store >> hsv[2];
  store >> tip;
  store >> help;
  }


// Destroy
FXColorWheel::~FXColorWheel(){
  delete dial;
  dial=(FXImage*)-1L;
  }

}
