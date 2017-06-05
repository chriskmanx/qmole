/********************************************************************************
*                                                                               *
*                      G r a d i e n t B a r   W i d g e t                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXGradientBar.cpp,v 1.71.2.1 2006/08/01 18:04:42 fox Exp $                   *
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
#include "FXGradientBar.h"
#include "FXColorDialog.h"
#include "FXPopup.h"
#include "FXMenuPane.h"
#include "FXMenuCommand.h"

/*
  Notes:

  - Shoot for compatibility with GIMP, to the extent feasible;
    format is:

    "GIMP Gradient"
    number_of_segments
    left middle right r0 g0 b0 a0 r1 g1 b1 a1 type coloring
    left middle right r0 g0 b0 a0 r1 g1 b1 a1 type coloring

    Where type is:

    0 = linear
    1 = curved
    2 = sine
    3 = sphere increasing
    4 = sphere decreasing

    and coloring is:

    0 = RGB
    1 = counter clockwise hue, sat, value
    2 = clockwise hue, sat, value

    FXGradientBar will not implement coloring #1 and #2.

  - Need ability to change alpha for all pivots (not the colors!).

  - Set/get all segmemts API.

  - Drop left of pivot changes left color, right changes right color,
    and on changes both.

  - Pass -1 if many segments are affected; otherwise pass affected segment
    index only.

*/

#define CONTROL_SIZE        9
#define BAR_WIDTH           64
#define BAR_HEIGHT          16

#define TAB_SIZE            9
#define TAB_DIST            5
#define TOP_PAD             3
#define BOTTOM_PAD          3
#define SIDE_PAD            6
#define TAB_PROXIMITY       20
#define ARROW_WIDTH         12
#define ARROW_HEIGHT        6
#define PICK_EXTRA          3

#define INT(x)              ((int)((x)+0.5))
#define BLEND(ch,bg,alpha)  (((bg)*(255-(alpha))+(ch)*(alpha))/255)
#define EPSILON             1.0E-10
#define GRADIENTBAR_MASK    (GRADIENTBAR_HORIZONTAL|GRADIENTBAR_VERTICAL|GRADIENTBAR_NO_CONTROLS|GRADIENTBAR_CONTROLS_TOP|GRADIENTBAR_CONTROLS_BOTTOM|GRADIENTBAR_CONTROLS_LEFT|GRADIENTBAR_CONTROLS_RIGHT)

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXGradientBar) FXGradientBarMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXGradientBar::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXGradientBar::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXGradientBar::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXGradientBar::onLeftBtnRelease),
  FXMAPFUNC(SEL_DND_ENTER,0,FXGradientBar::onDNDEnter),
  FXMAPFUNC(SEL_DND_LEAVE,0,FXGradientBar::onDNDLeave),
  FXMAPFUNC(SEL_DND_DROP,0,FXGradientBar::onDNDDrop),
  FXMAPFUNC(SEL_DND_MOTION,0,FXGradientBar::onDNDMotion),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXGradientBar::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXGradientBar::onQueryHelp),
  FXMAPFUNC(SEL_UPDATE,FXGradientBar::ID_RECENTER,FXGradientBar::onUpdRecenter),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_RECENTER,FXGradientBar::onCmdRecenter),
  FXMAPFUNC(SEL_UPDATE,FXGradientBar::ID_SPLIT,FXGradientBar::onUpdSplit),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_SPLIT,FXGradientBar::onCmdSplit),
  FXMAPFUNC(SEL_UPDATE,FXGradientBar::ID_MERGE,FXGradientBar::onUpdMerge),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_MERGE,FXGradientBar::onCmdMerge),
  FXMAPFUNC(SEL_UPDATE,FXGradientBar::ID_UNIFORM,FXGradientBar::onUpdUniform),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_UNIFORM,FXGradientBar::onCmdUniform),
  FXMAPFUNCS(SEL_COMMAND,FXGradientBar::ID_BLEND_LINEAR,FXGradientBar::ID_BLEND_DECREASING,FXGradientBar::onCmdBlending),
  FXMAPFUNCS(SEL_UPDATE,FXGradientBar::ID_BLEND_LINEAR,FXGradientBar::ID_BLEND_DECREASING,FXGradientBar::onUpdBlending),
  FXMAPFUNCS(SEL_UPDATE,FXGradientBar::ID_LOWER_COLOR,FXGradientBar::ID_UPPER_COLOR,FXGradientBar::onUpdSegColor),
  FXMAPFUNCS(SEL_CHANGED,FXGradientBar::ID_LOWER_COLOR,FXGradientBar::ID_UPPER_COLOR,FXGradientBar::onCmdSegColor),
  FXMAPFUNCS(SEL_COMMAND,FXGradientBar::ID_LOWER_COLOR,FXGradientBar::ID_UPPER_COLOR,FXGradientBar::onCmdSegColor),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_SETHELPSTRING,FXGradientBar::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_GETHELPSTRING,FXGradientBar::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_SETTIPSTRING,FXGradientBar::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXGradientBar::ID_GETTIPSTRING,FXGradientBar::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXGradientBar,FXFrame,FXGradientBarMap,ARRAYNUMBER(FXGradientBarMap))


// For serialization
FXGradientBar::FXGradientBar(){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  sellower=-1;
  selupper=-1;
  dropped=-1;
  current=-1;
  anchor=-1;
  grip=GRIP_NONE;
  where=GRIP_NONE;
  offset=0;
  }


// Construct gradient editor
FXGradientBar::FXGradientBar(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  target=tgt;
  message=sel;
  backColor=getApp()->getHiliteColor();
  selectColor=FXRGB((92*FXREDVAL(backColor))/100,(92*FXGREENVAL(backColor))/100,(92*FXBLUEVAL(backColor))/100);
  bar=new FXImage(getApp(),NULL,IMAGE_DITHER|IMAGE_KEEP|IMAGE_OWNED|IMAGE_SHMI|IMAGE_SHMP,2,2);
  nsegs=3;
  FXMALLOC(&seg,FXGradient,nsegs);
  seg[0].lower=0.0;
  seg[0].middle=0.2;
  seg[0].upper=0.4;
  seg[0].lowerColor=FXRGBA(255,0,0,255);
  seg[0].upperColor=FXRGBA(0,255,0,255);
  seg[0].blend=GRADIENT_BLEND_LINEAR;
  seg[1].lower=0.4;
  seg[1].middle=0.5;
  seg[1].upper=0.6;
  seg[1].lowerColor=FXRGBA(0,0,0,0);
  seg[1].upperColor=FXRGBA(255,255,0,255);
  seg[1].blend=GRADIENT_BLEND_LINEAR;
  seg[2].lower=0.6;
  seg[2].middle=0.8;
  seg[2].upper=1.0;
  seg[2].lowerColor=FXRGBA(0,0,0,0);
  seg[2].upperColor=FXRGBA(255,0,0,255);
  seg[2].blend=GRADIENT_BLEND_LINEAR;
  sellower=-1;
  selupper=-1;
  dropped=-1;
  current=-1;
  anchor=-1;
  grip=GRIP_NONE;
  where=GRIP_NONE;
  offset=0;
  }


// Create window
void FXGradientBar::create(){
  FXFrame::create();
  if(!colorType){colorType=getApp()->registerDragType(colorTypeName);}
  updatebar();
  bar->create();
  }


// Get default width
FXint FXGradientBar::getDefaultWidth(){
  register FXint w=BAR_WIDTH;
  if(options&GRADIENTBAR_VERTICAL){
    w=BAR_HEIGHT;
    if(options&GRADIENTBAR_CONTROLS_LEFT) w+=CONTROL_SIZE+1;
    if(options&GRADIENTBAR_CONTROLS_RIGHT) w+=CONTROL_SIZE+1;
    }
  return w+4+padleft+padright+(border<<1);
  }


// Get default height
FXint FXGradientBar::getDefaultHeight(){
  register FXint h=BAR_WIDTH;
  if(!(options&GRADIENTBAR_VERTICAL)){
    h=BAR_HEIGHT;
    if(options&GRADIENTBAR_CONTROLS_TOP) h+=CONTROL_SIZE+1;
    if(options&GRADIENTBAR_CONTROLS_BOTTOM) h+=CONTROL_SIZE+1;
    }
  return h+4+padtop+padbottom+(border<<1);
  }


// Resize the bar
void FXGradientBar::layout(){
  register FXint ww,hh;
  ww=width-padleft-padright-(border<<1)-4;
  hh=height-padtop-padbottom-(border<<1)-4;
  if(options&GRADIENTBAR_VERTICAL){
    if(options&GRADIENTBAR_CONTROLS_LEFT) ww-=CONTROL_SIZE+1;
    if(options&GRADIENTBAR_CONTROLS_RIGHT) ww-=CONTROL_SIZE+1;
    }
  else{
    if(options&GRADIENTBAR_CONTROLS_TOP) hh-=CONTROL_SIZE+1;
    if(options&GRADIENTBAR_CONTROLS_BOTTOM) hh-=CONTROL_SIZE+1;
    }
  if(ww<2) ww=2;
  if(hh<2) hh=2;
  if((bar->getWidth()!=ww) || (bar->getHeight()!=hh) || (flags&FLAG_DIRTY)){
    if((bar->getWidth()!=ww) || (bar->getHeight()!=hh)){
      bar->resize(ww,hh);
      }
    updatebar();
    bar->render();
    update();
    }
  flags&=~FLAG_DIRTY;
  }


typedef FXdouble (*BLENDFUNC)(FXdouble,FXdouble);


// Linear blend
FXdouble FXGradientBar::blendlinear(FXdouble middle,FXdouble pos){
  register FXdouble factor;
  if(pos<=middle){
    factor=(middle<EPSILON) ? 0.0 : 0.5*pos/middle;
    }
  else{
    pos-=middle;
    middle=1.0-middle;
    factor=(middle<EPSILON) ? 1.0 : 0.5+0.5*pos/middle;
    }
  return factor;
  }


// Power blend
FXdouble FXGradientBar::blendpower(FXdouble middle,FXdouble pos){
  if(middle<EPSILON) middle=EPSILON;
  return pow(pos,log(0.5)/log(middle));
  }


// Sinusoidal blend
FXdouble FXGradientBar::blendsine(FXdouble middle,FXdouble pos){
  pos=blendlinear(middle,pos);
  return (sin((-PI/2.0)+PI*pos)+1.0)/2.0;
  }


// Quadratic increasing blend
FXdouble FXGradientBar::blendincreasing(FXdouble middle,FXdouble pos){
  pos=blendlinear(middle,pos)-1.0;
  return sqrt(1.0-pos*pos);       // Works for convex increasing and concave decreasing
  }


// Quadratic decreasing blend
FXdouble FXGradientBar::blenddecreasing(FXdouble middle,FXdouble pos){
  pos=blendlinear(middle,pos);
  return 1.0-sqrt(1.0-pos*pos);   // Works for convex decreasing and concave increasing
  }


// Fill with gradient ramp
void FXGradientBar::gradient(FXColor *ramp,FXint nramp){
  register FXint s,lr,lg,lb,la,ur,ug,ub,ua,d,l,h,m,i;
  register FXdouble len=seg[nsegs-1].upper-seg[0].lower;
  register FXdouble del=nramp-1;
  register BLENDFUNC blend=NULL;
  register FXdouble f,t;

  FXASSERT(len>0.0);

  // Loop over segments
  for(s=0; s<nsegs; s++){

    // Lower color components
    lr=((FXuchar*)&seg[s].lowerColor)[0];
    lg=((FXuchar*)&seg[s].lowerColor)[1];
    lb=((FXuchar*)&seg[s].lowerColor)[2];
    la=((FXuchar*)&seg[s].lowerColor)[3];

    // Upper color components
    ur=((FXuchar*)&seg[s].upperColor)[0];
    ug=((FXuchar*)&seg[s].upperColor)[1];
    ub=((FXuchar*)&seg[s].upperColor)[2];
    ua=((FXuchar*)&seg[s].upperColor)[3];

    // Pixel range of segment
    l=(FXint)(0.5+(del*(seg[s].lower-seg[0].lower))/len);
    m=(FXint)(0.5+(del*(seg[s].middle-seg[0].lower))/len);
    h=(FXint)(0.5+(del*(seg[s].upper-seg[0].lower))/len);
    FXASSERT(0<=l && l<=m && m<=h && h<nramp);

    // Get blend function
    switch(seg[s].blend){
      case GRADIENT_BLEND_LINEAR:     blend=blendlinear;     break;
      case GRADIENT_BLEND_POWER:      blend=blendpower;      break;
      case GRADIENT_BLEND_SINE:       blend=blendsine;       break;
      case GRADIENT_BLEND_INCREASING: blend=blendincreasing; break;
      case GRADIENT_BLEND_DECREASING: blend=blenddecreasing; break;
      }

    d=h-l;
    if(0<d){
      for(i=l; i<=h; i++){
        FXASSERT(0<=i && i<nramp);
        f=blend(((FXdouble)m-(FXdouble)l)/(FXdouble)d,((FXdouble)i-(FXdouble)l)/(FXdouble)d);
        t=1.0-f;
        ((FXuchar*)&ramp[i])[0]=(FXuchar)(t*lr+f*ur);
        ((FXuchar*)&ramp[i])[1]=(FXuchar)(t*lg+f*ug);
        ((FXuchar*)&ramp[i])[2]=(FXuchar)(t*lb+f*ub);
        ((FXuchar*)&ramp[i])[3]=(FXuchar)(t*la+f*ua);
        }
      }
    }
  }


// Update bar
void FXGradientBar::updatebar(){
  register FXint barw=bar->getWidth();
  register FXint barh=bar->getHeight();
  register FXint x,y,r,g,b,a;
  register FXColor clr;
  FXColor *ramp=NULL;

  // Vertical
  if(options&GRADIENTBAR_VERTICAL){

    // Allocate ramp
    FXMALLOC(&ramp,FXColor,barh);

    // Fill with gradient
    gradient(ramp,barh);

    // Fill image
    for(y=0; y<barh; y++){
      r=((FXuchar*)&ramp[y])[0];
      g=((FXuchar*)&ramp[y])[1];
      b=((FXuchar*)&ramp[y])[2];
      a=((FXuchar*)&ramp[y])[3];
      clr=FXRGB(BLEND(r,255,a), BLEND(g,255,a), BLEND(b,255,a));
      for(x=0; x<barw/2; x++){
        bar->setPixel(x,barh-y-1,clr);
        }
      clr=FXRGB(BLEND(r,0,a), BLEND(g,0,a), BLEND(b,0,a));
      for(x=barw/2; x<barw; x++){
        bar->setPixel(x,barh-y-1,clr);
        }
      }
    }

  // Horizontal
  else{

    // Allocate ramp
    FXMALLOC(&ramp,FXColor,barw);

    // Fill with gradient
    gradient(ramp,barw);

    // Fill image
    for(x=0; x<barw; x++){
      r=((FXuchar*)&ramp[x])[0];
      g=((FXuchar*)&ramp[x])[1];
      b=((FXuchar*)&ramp[x])[2];
      a=((FXuchar*)&ramp[x])[3];
      clr=FXRGB(BLEND(r,255,a), BLEND(g,255,a), BLEND(b,255,a));
      for(y=0; y<barh/2; y++){
        bar->setPixel(x,y,clr);
        }
      clr=FXRGB(BLEND(r,0,a), BLEND(g,0,a), BLEND(b,0,a));
      for(y=barh/2; y<barh; y++){
        bar->setPixel(x,y,clr);
        }
      }
    }

  // Free ramp
  FXFREE(&ramp);
  }


// Draw up arrow
void FXGradientBar::drawUpArrow(FXDCWindow& dc,FXint x,FXint y,FXColor clr){
  FXPoint arrow[3];
  arrow[0].x=x;                arrow[0].y=y;
  arrow[1].x=x-CONTROL_SIZE/2; arrow[1].y=y+CONTROL_SIZE;
  arrow[2].x=x+CONTROL_SIZE/2; arrow[2].y=y+CONTROL_SIZE;
  dc.setForeground(clr);
  dc.fillPolygon(arrow,3);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawLine(x,y,x+CONTROL_SIZE/2,y+CONTROL_SIZE);
  dc.drawLine(x-CONTROL_SIZE/2,y+CONTROL_SIZE,x+CONTROL_SIZE/2,y+CONTROL_SIZE);
  dc.drawLine(x,y,x-CONTROL_SIZE/2,y+CONTROL_SIZE);
  }


// Draw down arrow
void FXGradientBar::drawDnArrow(FXDCWindow& dc,FXint x,FXint y,FXColor clr){
  FXPoint arrow[3];
  arrow[0].x=x-CONTROL_SIZE/2; arrow[0].y=y;
  arrow[1].x=x+CONTROL_SIZE/2; arrow[1].y=y;
  arrow[2].x=x;                arrow[2].y=y+CONTROL_SIZE;
  dc.setForeground(clr);
  dc.fillPolygon(arrow,3);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawLine(x-CONTROL_SIZE/2,y,x+CONTROL_SIZE/2,y);
  dc.drawLine(x,y+CONTROL_SIZE,x-CONTROL_SIZE/2,y);
  dc.drawLine(x,y+CONTROL_SIZE,x+CONTROL_SIZE/2,y);
  }


// Draw right arrow
void FXGradientBar::drawRtArrow(FXDCWindow& dc,FXint x,FXint y,FXColor clr){
  FXPoint arrow[3];
  arrow[0].x=x;              arrow[0].y=y-CONTROL_SIZE/2;
  arrow[1].x=x;              arrow[1].y=y+CONTROL_SIZE/2;
  arrow[2].x=x+CONTROL_SIZE; arrow[2].y=y;
  dc.setForeground(clr);
  dc.fillPolygon(arrow,3);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawLine(x+CONTROL_SIZE,y,x,y-CONTROL_SIZE/2);
  dc.drawLine(x+CONTROL_SIZE,y,x,y+CONTROL_SIZE/2);
  dc.drawLine(x,y-CONTROL_SIZE/2,x,y+CONTROL_SIZE/2);
  }


// Draw left arrow
void FXGradientBar::drawLtArrow(FXDCWindow& dc,FXint x,FXint y,FXColor clr){
  FXPoint arrow[3];
  arrow[0].x=x+CONTROL_SIZE; arrow[0].y=y-CONTROL_SIZE/2;
  arrow[1].x=x+CONTROL_SIZE; arrow[1].y=y+CONTROL_SIZE/2;
  arrow[2].x=x;              arrow[2].y=y;
  dc.setForeground(clr);
  dc.fillPolygon(arrow,3);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawLine(x,y,x+CONTROL_SIZE,y-CONTROL_SIZE/2);
  dc.drawLine(x,y,x+CONTROL_SIZE,y+CONTROL_SIZE/2);
  dc.drawLine(x+CONTROL_SIZE,y-CONTROL_SIZE/2,x+CONTROL_SIZE,y+CONTROL_SIZE/2);
  }


// Draw top arrows
void FXGradientBar::drawTopArrows(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  register FXdouble len=seg[nsegs-1].upper-seg[0].lower;
  register FXint s,l,m,r;
  FXASSERT(len>0.0);
  for(s=0; s<nsegs; s++){
    l=(FXint)(0.5+((w-1)*(seg[s].lower-seg[0].lower))/len);
    m=(FXint)(0.5+((w-1)*(seg[s].middle-seg[0].lower))/len);
    r=(FXint)(0.5+((w-1)*(seg[s].upper-seg[0].lower))/len);
    dc.setForeground(isSegmentSelected(s)?selectColor:backColor);
    dc.fillRectangle(x+l,y,r-l,h);
    if(0<s) drawDnArrow(dc,x+l,y,FXRGB(0,0,0));
    drawDnArrow(dc,x+m,y,FXRGB(255,255,255));
    }
  drawDnArrow(dc,x,y,FXRGB(0,0,0));
  drawDnArrow(dc,x+w-1,y,FXRGB(0,0,0));
  }


// Draw bottom arrows
void FXGradientBar::drawBottomArrows(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  register FXdouble len=seg[nsegs-1].upper-seg[0].lower;
  register FXint s,l,m,r;
  FXASSERT(len>0.0);
  for(s=0; s<nsegs; s++){
    l=(FXint)(0.5+((w-1)*(seg[s].lower-seg[0].lower))/len);
    m=(FXint)(0.5+((w-1)*(seg[s].middle-seg[0].lower))/len);
    r=(FXint)(0.5+((w-1)*(seg[s].upper-seg[0].lower))/len);
    dc.setForeground(isSegmentSelected(s)?selectColor:backColor);
    dc.fillRectangle(x+l,y,r-l,h);
    if(0<s) drawUpArrow(dc,x+l,y,FXRGB(0,0,0));
    drawUpArrow(dc,x+m,y,FXRGB(255,255,255));
    }
  drawUpArrow(dc,x,y,FXRGB(0,0,0));
  drawUpArrow(dc,x+w-1,y,FXRGB(0,0,0));
  }


// Draw left arrows
void FXGradientBar::drawLeftArrows(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  register FXdouble len=seg[nsegs-1].upper-seg[0].lower;
  register FXint s,t,m,b;
  FXASSERT(len>0.0);
  for(s=0; s<nsegs; s++){
    t=(FXint)(0.5+((h-1)*(seg[s].upper-seg[0].lower))/len);
    m=(FXint)(0.5+((h-1)*(seg[s].middle-seg[0].lower))/len);
    b=(FXint)(0.5+((h-1)*(seg[s].lower-seg[0].lower))/len);
    dc.setForeground(isSegmentSelected(s)?selectColor:backColor);
    dc.fillRectangle(x,y+h-t-1,w,t-b);
    if(0<s) drawRtArrow(dc,x,y+h-b-1,FXRGB(0,0,0));
    drawRtArrow(dc,x,y+h-m-1,FXRGB(255,255,255));
    }
  drawRtArrow(dc,x,y,FXRGB(0,0,0));
  drawRtArrow(dc,x,y+h-1,FXRGB(0,0,0));
  }


// Draw right arrows
void FXGradientBar::drawRightArrows(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h){
  register FXdouble len=seg[nsegs-1].upper-seg[0].lower;
  register FXint s,t,m,b;
  FXASSERT(len>0.0);
  for(s=0; s<nsegs; s++){
    t=(FXint)(0.5+((h-1)*(seg[s].upper-seg[0].lower))/len);
    m=(FXint)(0.5+((h-1)*(seg[s].middle-seg[0].lower))/len);
    b=(FXint)(0.5+((h-1)*(seg[s].lower-seg[0].lower))/len);
    dc.setForeground(isSegmentSelected(s)?selectColor:backColor);
    dc.fillRectangle(x,y+h-t-1,w,t-b);
    if(0<s) drawLtArrow(dc,x,y+h-b-1,FXRGB(0,0,0));
    drawLtArrow(dc,x,y+h-m-1,FXRGB(255,255,255));
    }
  drawLtArrow(dc,x,y,FXRGB(0,0,0));
  drawLtArrow(dc,x,y+h-1,FXRGB(0,0,0));
  }


// Handle repaint
long FXGradientBar::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  FXint barx,bary,barw,barh;

  // Frame
  drawFrame(dc,0,0,width,height);

  // Background
  dc.setForeground(baseColor);
  dc.fillRectangle(border,border,padleft,height-(border<<1));
  dc.fillRectangle(width-padright-border,border,padright,height-(border<<1));
  dc.fillRectangle(border+padleft,border,width-padleft-padright-(border<<1),padtop);
  dc.fillRectangle(border+padleft,height-padbottom-border,width-padleft-padright-(border<<1),padbottom);

  barx=padleft+border;
  bary=padtop+border;
  barw=width-padright-padleft-(border<<1);
  barh=height-padbottom-padtop-(border<<1);

  // Sunken well for gradient
  drawDoubleSunkenRectangle(dc,barx,bary,barw,barh);

  barx+=2;
  bary+=2;
  barw-=4;
  barh-=4;

  // Set clip rectangle
  dc.setClipRectangle(barx,bary,barw,barh);

  // Vertical gradient
  if(options&GRADIENTBAR_VERTICAL){

    // Left controls
    if(options&GRADIENTBAR_CONTROLS_LEFT){
      drawLeftArrows(dc,barx,bary,CONTROL_SIZE+1,barh);
      barx+=CONTROL_SIZE+1;
      }

    // Draw the bar itself
    dc.drawImage(bar,barx,bary);
    barx+=bar->getWidth();

    // Right controls
    if(options&GRADIENTBAR_CONTROLS_RIGHT){
      drawRightArrows(dc,barx,bary,CONTROL_SIZE+1,barh);
      }
    }

  // Horizontal gradient
  else{

    // Top controls
    if(options&GRADIENTBAR_CONTROLS_TOP){
      drawTopArrows(dc,barx,bary,barw,CONTROL_SIZE+1);
      bary+=CONTROL_SIZE+1;
      }

    // Draw the bar itself
    dc.drawImage(bar,barx,bary);
    bary+=bar->getHeight();

    // Bottom controls
    if(options&GRADIENTBAR_CONTROLS_BOTTOM){
      drawBottomArrows(dc,barx,bary,barw,CONTROL_SIZE+1);
      }
    }
  return 1;
  }


// Replace the current gradient segments
void FXGradientBar::setGradients(const FXGradient *segments,FXint nsegments){
  if(!segments || nsegments<1){ fxerror("FXGradientBar::setGradients: bad argument."); }
  if(nsegments!=nsegs){
    FXRESIZE(&seg,FXGradient,nsegments);
    nsegs=nsegments;
    if(selupper>=nsegs) selupper=nsegs-1;
    if(sellower>=nsegs) sellower=nsegs-1;
    if(current>=nsegs) current=nsegs-1;
    if(anchor>=nsegs) anchor=nsegs-1;
    }
  memcpy(seg,segments,sizeof(FXGradient)*nsegments);
  recalc();
  }


// Return the gradient segments
void FXGradientBar::getGradients(FXGradient*& segments,FXint& nsegments) const {
  nsegments=0;
  if(FXMALLOC(&segments,FXGradient,nsegs)){
    memcpy(segments,seg,sizeof(FXGradient)*nsegs);
    nsegments=nsegs;
    }
  }


// Select segment
FXbool FXGradientBar::selectSegments(FXint fm,FXint to,FXbool notify){
  if(fm>to || fm<0 || to>=nsegs){ fxerror("FXGradientBar::selectSegments: argument out of range."); }
  if(sellower!=fm || selupper!=to){
    sellower=fm;
    selupper=to;
    update();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_SELECTED,message),NULL);}
    return TRUE;
    }
  return FALSE;
  }


// Deselect all segments
FXbool FXGradientBar::deselectSegments(FXbool notify){
  if(0<=sellower && 0<=selupper){
    sellower=selupper=-1;
    update();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_DESELECTED,message),NULL);}
    return TRUE;
    }
  return FALSE;
  }


// Is segment selected
FXbool FXGradientBar::isSegmentSelected(FXint s) const {
  if(s<0 || s>=nsegs){ fxerror("FXGradientBar::isSegmentSelected: argument out of range."); }
  return sellower<=s && s<=selupper;
  }


// Set current item
void FXGradientBar::setCurrentSegment(FXint index,FXbool notify){
  if(index<-1 || nsegs<=index){ fxerror("%s::setCurrentSegment: index out of range.\n",getClassName()); }
  if(index!=current){
    current=index;
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);}
    }
  }


// Change anchor segment
void FXGradientBar::setAnchorSegment(FXint index){
  if(index<-1 || nsegs<=index){ fxerror("%s::setAnchorSegment: index out of range.\n",getClassName()); }
  anchor=index;
  }


// Move lower point of segment
void FXGradientBar::moveSegmentLower(FXint sg,FXdouble val,FXbool notify){
  if(0<sg && sg<nsegs){
    if(val<seg[sg-1].middle) val=seg[sg-1].middle;
    if(seg[sg].middle<val) val=seg[sg].middle;
    if(seg[sg].lower!=val){
      seg[sg-1].upper=seg[sg].lower=val;
      recalc();
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)sg);}
      }
    }
  }


// Move middle point of segment
void FXGradientBar::moveSegmentMiddle(FXint sg,FXdouble val,FXbool notify){
  if(0<=sg && sg<nsegs){
    if(val<seg[sg].lower) val=seg[sg].lower;
    if(seg[sg].upper<val) val=seg[sg].upper;
    if(seg[sg].middle!=val){
      seg[sg].middle=val;
      recalc();
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)sg);}
      }
    }
  }


// Move upper point of segment
void FXGradientBar::moveSegmentUpper(FXint sg,FXdouble val,FXbool notify){
  if(0<=sg && sg<nsegs-1){
    if(val<seg[sg].middle) val=seg[sg].middle;
    if(seg[sg+1].middle<val) val=seg[sg+1].middle;
    if(seg[sg].upper!=val){
      seg[sg+1].lower=seg[sg].upper=val;
      recalc();
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)sg);}
      }
    }
  }


// Move segments
void FXGradientBar::moveSegments(FXint sglo,FXint sghi,FXdouble val,FXbool notify){
  register FXdouble delta, below,above,room;
  register FXint i;
  if(0<=sglo && sghi<nsegs && sglo<=sghi){
    below=seg[sglo].middle-seg[sglo].lower;
    above=seg[sghi].upper-seg[sglo].middle;
    room=seg[sghi].middle-seg[sglo].middle;
    if(sglo==0){
      if(val<seg[0].lower) val=seg[0].lower;
      }
    else{
      if(val-below<seg[sglo-1].middle) val=seg[sglo-1].middle+below;
      }
    if(sghi==nsegs-1){
      if(val+room>seg[nsegs-1].upper) val=seg[nsegs-1].upper-room;
      }
    else{
      if(val+above>seg[sghi+1].middle) val=seg[sghi+1].middle-above;
      }
    delta=val-seg[sglo].middle;
    if(delta!=0.0){
      for(i=sglo; i<=sghi; i++){
        if(0<i) seg[i].lower+=delta;
        seg[i].middle+=delta;
        if(i<nsegs-1) seg[i].upper+=delta;
        }
      if(0<sglo){
        seg[sglo-1].upper=seg[sglo].lower;
        }
      if(sghi<nsegs-1){
        seg[sghi+1].lower=seg[sghi].upper;
        }
      recalc();
      if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);}
      }
    }
  }


// Split segment at the midpoint
void FXGradientBar::splitSegments(FXint sglo,FXint sghi,FXbool notify){
  register FXint n=sghi-sglo+1;
  register FXint i,j;
  if(0<=sglo && sghi<nsegs && 0<n){
    FXRESIZE(&seg,FXGradient,nsegs+n);
    memmove(&seg[sghi+n],&seg[sghi],sizeof(FXGradient)*(nsegs-sghi));
    for(i=sghi,j=sghi+n-1; sglo<=i; i-=1,j-=2){
      seg[j+1].upper=seg[i].upper;
      seg[j+1].lower=seg[i].middle;
      seg[j+1].middle=0.5*(seg[j+1].upper+seg[j+1].lower);
      seg[j+1].lowerColor=seg[i].upperColor;// FIXME
      seg[j+1].upperColor=seg[i].upperColor;
      seg[j+1].blend=seg[i].blend;
      seg[j+0].upper=seg[i].middle;
      seg[j+0].lower=seg[i].lower;
      seg[j+0].middle=0.5*(seg[j+0].upper+seg[j+0].lower);
      seg[j+0].lowerColor=seg[i].lowerColor;
      seg[j+0].upperColor=seg[i].upperColor;// FIXME
      seg[j+0].blend=seg[i].blend;
      }
    nsegs+=n;
    recalc();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);}
    }
  }


// Merge segments
void FXGradientBar::mergeSegments(FXint sglo,FXint sghi,FXbool notify){
  register FXint n=sghi-sglo;
  if(0<=sglo && sghi<nsegs && 0<n){
    seg[sglo].middle=(n&1)?seg[(sghi+sglo)/2].upper:seg[(sghi+sglo)/2].middle;
    seg[sglo].upper=seg[sghi].upper;
    seg[sglo].upperColor=seg[sghi].upperColor;
    memmove(&seg[sglo+1],&seg[sghi+1],sizeof(FXGradient)*(nsegs-sghi-1));
    FXRESIZE(&seg,FXGradient,nsegs-n);
    nsegs-=n;
    if(selupper>=nsegs) selupper=nsegs-1;
    if(sellower>=nsegs) sellower=nsegs-1;
    if(current>=nsegs) current=nsegs-1;
    if(anchor>=nsegs) anchor=nsegs-1;
    recalc();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);}
    }
  }


// Make segments uniformly distributed
void FXGradientBar::uniformSegments(FXint sglo,FXint sghi,FXbool notify){
  register FXdouble m,d,a;
  register FXint s;
  if(0<=sglo && sghi<nsegs && sglo<=sghi){
    d=sghi-sglo+1;
    m=seg[sghi].upper-seg[sglo].lower;
    a=seg[sglo].lower;
    for(s=sglo; s<=sghi; s++){
      seg[s].lower=a+(FXdouble)(s-sglo)*m/d;
      seg[s].upper=a+(FXdouble)(s-sglo+1)*m/d;
      seg[s].middle=0.5*(seg[s].lower+seg[s].upper);
      }
    recalc();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);}
    }
  }


// Change blend curve of segment
void FXGradientBar::blendSegments(FXint sglo,FXint sghi,FXuint blend,FXbool notify){
  register FXint s;
  if(0<=sglo && sghi<nsegs && sglo<=sghi){
    for(s=sglo; s<=sghi; s++){
      seg[s].blend=blend;
      }
    recalc();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);}
    }
  }


// Determine which segment got hit
FXint FXGradientBar::getSegment(FXint x,FXint y) const {
  register FXdouble shi=seg[nsegs-1].upper;
  register FXdouble slo=seg[0].lower;
  register FXdouble len=shi-slo;
  register FXdouble del;
  register FXint lo,hi,v,s;
  FXASSERT(len>0.0);
  if(options&GRADIENTBAR_VERTICAL){
    if(y<border+padtop+2) return nsegs-1;
    if(y>height-border-padbottom-2) return 0;
    v=height-border-padbottom-y-3;
    del=bar->getHeight()-1;
    }
  else{
    if(x<border+padleft+2) return 0;
    if(x>width-border-padright-2) return nsegs-1;
    v=x-border-padleft-2;
    del=bar->getWidth()-1;
    }
  for(s=0; s<nsegs; s++){
    lo=(FXint)(0.5+(del*(seg[s].lower-slo))/len);
    hi=(FXint)(0.5+(del*(seg[s].upper-slo))/len);
    if(lo<=v && v<=hi) return s;
    }
  return -1;
  }


// Get grip in segment
FXint FXGradientBar::getGrip(FXint sg,FXint x,FXint y) const {
  if(0<=sg && sg<nsegs){
    register FXdouble shi=seg[nsegs-1].upper;
    register FXdouble slo=seg[0].lower;
    register FXdouble len=shi-slo;
    register FXdouble del;
    register FXint lo,hi,md,v;
    FXASSERT(len>0.0);
    if(options&GRADIENTBAR_VERTICAL){
      v=height-border-padbottom-y-3;
      del=bar->getHeight()-1;
      }
    else{
      v=x-border-padleft-2;
      del=bar->getWidth()-1;
      }
    lo=(FXint)(0.5+(del*(seg[sg].lower-slo))/len);
    hi=(FXint)(0.5+(del*(seg[sg].upper-slo))/len);
    if((lo-CONTROL_SIZE/2-PICK_EXTRA)<=v && v<=(hi+CONTROL_SIZE/2+PICK_EXTRA)){
      if(v<=(lo+CONTROL_SIZE/2+PICK_EXTRA)) return GRIP_LOWER;
      if((hi-CONTROL_SIZE/2-PICK_EXTRA)<=v) return GRIP_UPPER;
      md=(FXint)(0.5+(del*(seg[sg].middle-slo))/len);
      if(v<(md-CONTROL_SIZE/2-PICK_EXTRA)) return GRIP_SEG_LOWER;
      if(v>(md+CONTROL_SIZE/2+PICK_EXTRA)) return GRIP_SEG_UPPER;
      return GRIP_MIDDLE;
      }
    }
  return GRIP_NONE;
  }


// Get value given position x,y
FXdouble FXGradientBar::getValue(FXint x,FXint y) const {
  register FXdouble slo=seg[0].lower;
  register FXdouble shi=seg[nsegs-1].upper;
  register FXdouble val;
  if(options&GRADIENTBAR_VERTICAL)
    val=slo+(height-padbottom-border-3-y)*(shi-slo)/(bar->getHeight()-1);
  else
    val=slo+(x-padleft-border-2)*(shi-slo)/(bar->getWidth()-1);
  return FXCLAMP(slo,val,shi);
  }


// Get position of lower edge of segment
FXint FXGradientBar::getSegmentLowerPos(FXint sg) const {
  register FXdouble shi=seg[nsegs-1].upper;
  register FXdouble slo=seg[0].lower;
  register FXdouble len=shi-slo;
  register FXint pos;
  FXASSERT(0<=sg && sg<nsegs);
  FXASSERT(0<len);
  if(options&GRADIENTBAR_VERTICAL){
    pos=height-padbottom-border-3-(FXint)(0.5+((bar->getHeight()-1)*(seg[sg].lower-slo))/len);
    }
  else{
    pos=padleft+border+2+(FXint)(0.5+((bar->getWidth()-1)*(seg[sg].lower-slo))/len);
    }
  return pos;
  }


// Get position of upper edge of segment
FXint FXGradientBar::getSegmentUpperPos(FXint sg) const {
  register FXdouble shi=seg[nsegs-1].upper;
  register FXdouble slo=seg[0].lower;
  register FXdouble len=shi-slo;
  register FXint pos;
  FXASSERT(0<=sg && sg<nsegs);
  FXASSERT(0<len);
  if(options&GRADIENTBAR_VERTICAL){
    pos=height-padbottom-border-3-(FXint)(0.5+((bar->getHeight()-1)*(seg[sg].upper-slo))/len);
    }
  else{
    pos=padleft+border+2+(FXint)(0.5+((bar->getWidth()-1)*(seg[sg].upper-slo))/len);
    }
  return pos;
  }


// Get position of middle of segment
FXint FXGradientBar::getSegmentMiddlePos(FXint sg) const {
  register FXdouble shi=seg[nsegs-1].upper;
  register FXdouble slo=seg[0].lower;
  register FXdouble len=shi-slo;
  register FXint pos;
  FXASSERT(0<=sg && sg<nsegs);
  FXASSERT(0<len);
  if(options&GRADIENTBAR_VERTICAL){
    pos=height-padbottom-border-3-(FXint)(0.5+((bar->getHeight()-1)*(seg[sg].middle-slo))/len);
    }
  else{
    pos=padleft+border+2+(FXint)(0.5+((bar->getWidth()-1)*(seg[sg].middle-slo))/len);
    }
  return pos;
  }


// Mouse moved
long FXGradientBar::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXdouble value;
  FXint g,s;
  if(options&GRADIENTBAR_VERTICAL){
    value=getValue(event->win_x,event->win_y+offset);
    }
  else{
    value=getValue(event->win_x+offset,event->win_y);
    }
  switch(grip){
    case GRIP_LOWER:
      if(0<current) moveSegmentLower(current,value,TRUE);
      return 1;
    case GRIP_MIDDLE:
      moveSegmentMiddle(current,value,TRUE);
      return 1;
    case GRIP_UPPER:
      if(current<nsegs-1) moveSegmentUpper(current,value,TRUE);
      return 1;
    case GRIP_SEG_LOWER:
    case GRIP_SEG_UPPER:
      moveSegments(sellower,selupper,value,TRUE);
      return 1;
    case GRIP_NONE:
      s=getSegment(event->win_x,event->win_y);
      if(0<=s){
        g=getGrip(s,event->win_x,event->win_y);
        if((g==GRIP_MIDDLE) || (g==GRIP_LOWER && 0<s) || (g==GRIP_UPPER && s<nsegs-1)){
          if(options&GRADIENTBAR_VERTICAL){
            setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
            }
          else{
            setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGV_CURSOR));
            }
          return 1;
          }
        }
      setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
      return 1;
    }
  return 0;
  }


// Pressed button
long FXGradientBar::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    setCurrentSegment(getSegment(event->win_x,event->win_y),TRUE);
    if(0<=current){
      grip=getGrip(current,event->win_x,event->win_y);
      if(grip==GRIP_SEG_LOWER || grip==GRIP_SEG_UPPER){
        if((0<=anchor) && (event->state&SHIFTMASK)){
          selectSegments(FXMIN(current,anchor),FXMAX(current,anchor),TRUE);
          }
        else if(!isSegmentSelected(current)){
          selectSegments(current,current,TRUE);
          setAnchorSegment(current);
          }
        offset=getSegmentMiddlePos(sellower);
        }
      else{
        deselectSegments(TRUE);
        if(grip==GRIP_LOWER){
          offset=getSegmentLowerPos(current);
          }
        else if(grip==GRIP_MIDDLE){
          offset=getSegmentMiddlePos(current);
          }
        else if(grip==GRIP_UPPER){
          offset=getSegmentUpperPos(current);
          }
        }
      if(grip!=GRIP_NONE){
        if(options&GRADIENTBAR_VERTICAL){
          setDragCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
          offset=offset-event->win_y;
          }
        else{
          setDragCursor(getApp()->getDefaultCursor(DEF_DRAGV_CURSOR));
          offset=offset-event->win_x;
          }
        }
      flags&=~FLAG_UPDATE;
      }
    else{
      deselectSegments(TRUE);
      }
    return 1;
    }
  return 0;
  }


// Released button
long FXGradientBar::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint g=grip;
  if(isEnabled()){
    ungrab();
    flags&=~FLAG_CHANGED;
    flags|=FLAG_UPDATE;
    grip=GRIP_NONE;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if((0<=current) && (g==GRIP_SEG_LOWER || g==GRIP_SEG_UPPER) && !(event->state&SHIFTMASK) && !event->moved){
      selectSegments(current,current,TRUE);
      }
    setAnchorSegment(current);
    setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
    return 1;
    }
  return 0;
  }


// Handle drag-and-drop enter
long FXGradientBar::onDNDEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onDNDEnter(sender,sel,ptr);
  dropped=-1;
  return 1;
  }


// Handle drag-and-drop leave
long FXGradientBar::onDNDLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onDNDLeave(sender,sel,ptr);
  dropped=-1;
  return 1;
  }


// Handle drag-and-drop motion
long FXGradientBar::onDNDMotion(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;

  // Handle base class first
  if(FXFrame::onDNDMotion(sender,sel,ptr)) return 1;

  // Is it a color being dropped?
  if(offeredDNDType(FROM_DRAGNDROP,colorType)){
    dropped=getSegment(event->win_x,event->win_y);
    if(0<=dropped){
      where=getGrip(dropped,event->win_x,event->win_y);
      if(where!=GRIP_NONE){
        acceptDrop(DRAG_COPY);
        }
      }
    return 1;
    }
  return 0;
  }


// Handle drag-and-drop drop
long FXGradientBar::onDNDDrop(FXObject* sender,FXSelector sel,void* ptr){
  FXushort *clr; FXuint len; FXColor color;

  // Try handling it in base class first
  if(FXFrame::onDNDDrop(sender,sel,ptr)) return 1;

  // Try handle here
  if(0<=dropped){
    if(getDNDData(FROM_DRAGNDROP,colorType,(FXuchar*&)clr,len)){
      color=FXRGBA((clr[0]+128)/257,(clr[1]+128)/257,(clr[2]+128)/257,(clr[3]+128)/257);
      FXFREE(&clr);
      if(where!=GRIP_NONE){
        if(where<=GRIP_SEG_LOWER){
          setSegmentLowerColor(dropped,color,TRUE);
          if(where==GRIP_LOWER && 0<dropped) setSegmentUpperColor(dropped-1,color,TRUE);
          }
        else if(where>=GRIP_SEG_UPPER){
          setSegmentUpperColor(dropped,color,TRUE);
          if(where==GRIP_UPPER && dropped<nsegs-1) setSegmentLowerColor(dropped+1,color,TRUE);
          }
        else{
          setSegmentLowerColor(dropped,color,TRUE);
          setSegmentUpperColor(dropped,color,TRUE);
          }
        }
      return 1;
      }
    }
  return 0;
  }


// Update upper or lower color of current segment
long FXGradientBar::onUpdSegColor(FXObject* sender,FXSelector sel,void*){
  if(0<=current){
    if(FXSELID(sel)==ID_LOWER_COLOR){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&seg[current].lowerColor);
      }
    else if(FXSELID(sel)==ID_UPPER_COLOR){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&seg[current].upperColor);
      }
    }
  return 1;
  }


// Change upper or lower color of current segment
long FXGradientBar::onCmdSegColor(FXObject* sender,FXSelector sel,void*){
  FXColor color;
  if(0<=current){
    if(FXSELID(sel)==ID_LOWER_COLOR){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&color);
      setSegmentLowerColor(current,color,TRUE);
      }
    else if(FXSELID(sel)==ID_UPPER_COLOR){
      sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&color);
      setSegmentUpperColor(current,color,TRUE);
      }
    }
  return 1;
  }


// Update recenter midpoint
long FXGradientBar::onUpdRecenter(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=current)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Recenter midpoint
long FXGradientBar::onCmdRecenter(FXObject*,FXSelector,void*){
  if(0<=current){
    moveSegmentMiddle(current,0.5*(seg[current].lower+seg[current].upper),TRUE);
    }
  return 1;
  }


// Update split segment
long FXGradientBar::onUpdSplit(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=sellower && 0<=selupper)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Split segment
long FXGradientBar::onCmdSplit(FXObject*,FXSelector,void*){
  if(0<=sellower && 0<=selupper){
    splitSegments(sellower,selupper,TRUE);
    selectSegments(sellower,selupper+selupper-sellower+1,TRUE);
    }
  return 1;
  }


// Update merge segments
long FXGradientBar::onUpdMerge(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=sellower && 0<=selupper && sellower<selupper)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Merge selection into one segment
long FXGradientBar::onCmdMerge(FXObject*,FXSelector,void*){
  if(0<=sellower && 0<=selupper){
    mergeSegments(sellower,selupper,TRUE);
    selectSegments(sellower,sellower,TRUE);
    }
  return 1;
  }


// Update make uniform
long FXGradientBar::onUpdUniform(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=sellower && 0<=selupper)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Make selected segments uniform
long FXGradientBar::onCmdUniform(FXObject*,FXSelector,void*){
  if(0<=sellower && 0<=selupper) uniformSegments(sellower,selupper,TRUE);
  return 1;
  }


// Update blending
long FXGradientBar::onUpdBlending(FXObject* sender,FXSelector sel,void*){
  FXuint blend=FXSELID(sel)-ID_BLEND_LINEAR;
  if(0<=sellower && 0<=selupper){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    for(FXint s=sellower; s<=selupper; s++){
      if(seg[s].blend!=blend){ sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL); return 1; }
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change blending
long FXGradientBar::onCmdBlending(FXObject*,FXSelector sel,void*){
  FXuint blend=FXSELID(sel)-ID_BLEND_LINEAR;
  if(0<=sellower && 0<=selupper){
    blendSegments(sellower,selupper,blend,TRUE);
    }
  return 1;
  }


// Set help using a message
long FXGradientBar::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXGradientBar::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXGradientBar::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXGradientBar::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXGradientBar::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXGradientBar::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Get blend cuve of segment
FXuint FXGradientBar::getSegmentBlend(FXint s) const {
  if(s<0 || s>=nsegs){ fxerror("FXGradientBar::getSegmentBlend: argument out of range."); }
  return seg[s].blend;
  }


// Set colors of a segment
void FXGradientBar::setSegmentLowerColor(FXint s,FXColor clr,FXbool notify){
  if(s<0 || s>=nsegs){ fxerror("FXGradientBar::setSegmentLowerColor: argument out of range."); }
  if(seg[s].lowerColor!=clr){
    seg[s].lowerColor=clr;
    recalc();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)s);}
    }
  }


// Set colors of a segment
void FXGradientBar::setSegmentUpperColor(FXint s,FXColor clr,FXbool notify){
  if(s<0 || s>=nsegs){ fxerror("FXGradientBar::setSegmentUpperColor: argument out of range."); }
  if(seg[s].upperColor!=clr){
    seg[s].upperColor=clr;
    recalc();
    if(notify && target){target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)s);}
    }
  }


// Get colors of a segment
FXColor FXGradientBar::getSegmentLowerColor(FXint s) const {
  if(s<0 || s>=nsegs){ fxerror("FXGradientBar::getSegmentLowerColor: argument out of range."); }
  return seg[s].lowerColor;
  }


// Get colors of a segment
FXColor FXGradientBar::getSegmentUpperColor(FXint s) const {
  if(s<0 || s>=nsegs){ fxerror("FXGradientBar::getSegmentUpperColor: argument out of range."); }
  return seg[s].upperColor;
  }


// Get lower value of segment sg
FXdouble FXGradientBar::getSegmentLower(FXint sg) const {
  if(sg<0 || sg>=nsegs){ fxerror("FXGradientBar::getSegmentLower: argument out of range."); }
  return seg[sg].lower;
  }


// Get middle value of segment sg
FXdouble FXGradientBar::getSegmentMiddle(FXint sg) const {
  if(sg<0 || sg>=nsegs){ fxerror("FXGradientBar::getSegmentMiddle: argument out of range."); }
  return seg[sg].middle;
  }


// Get upper value of segment sg
FXdouble FXGradientBar::getSegmentUpper(FXint sg) const {
  if(sg<0 || sg>=nsegs){ fxerror("FXGradientBar::getSegmentUpper: argument out of range."); }
  return seg[sg].upper;
  }


// Set color bar options
void FXGradientBar::setBarStyle(FXuint style){
  FXuint opts=(options&~GRADIENTBAR_MASK) | (style&GRADIENTBAR_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Get color bar options
FXuint FXGradientBar::getBarStyle() const {
  return (options&GRADIENTBAR_MASK);
  }


// Set base color
void FXGradientBar::setSelectColor(FXColor clr){
  if(clr!=selectColor){
    selectColor=clr;
    update();
    }
  }


// Save data
void FXGradientBar::save(FXStream& store) const {
  FXFrame::save(store);
  store << bar;
  store << tip;
  store << help;
  store << selectColor;
  }


// Load data
void FXGradientBar::load(FXStream& store){
  FXFrame::load(store);
  store >> bar;
  store >> tip;
  store >> help;
  store >> selectColor;
  }


// Zap it
FXGradientBar::~FXGradientBar(){
  delete bar;
  FXFREE(&seg);
  bar=(FXImage*)-1L;
  seg=(FXGradient*)-1L;
  }

}
