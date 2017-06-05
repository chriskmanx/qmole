/********************************************************************************
*                                                                               *
*                         C o l o r R i n g   W i d g e t                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXColorRing.cpp,v 1.19 2006/01/22 17:58:20 fox Exp $                     *
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
#include "FXImage.h"
#include "FXColorRing.h"



/*

  Notes:

  - Ranges: Hue 0..360, saturation 0..1, value 0..1.

  - For insight into the workings of this thing a basic grasp of linear algebra
    would be very helpful.

  - The hue-ring part is pretty straightforward and needs no comment; the triangle
    part used for saturation and value needs some explanations:



            V=1 line    S=1 line
                     \ /
                      C
                     / \
       Far S=1 (A)  /   \    Far V=1 (B)
                   / CLR \
                  /       \
                 /         \
                / BLK   WHT \
             --B-------------W----S=0 line
    point V=0 /               \
             /                 \
                               V=1 line

                    Far S=0 (C)


    Observe that V=0 at the POINT B, and S is multi-valued at B [either 0 or 1].

    Also observe that far outside the triangle, S=1 when to the left of BC,
    S=0 when below BW, and V=1 when right of BC.

    When far away, we project the point to the closest side of the triangle,
    and thus have to calculate only one of the two, either S (case B)or V (cases A
    and C).


    Thus we have:

      P = (C - W) * s + W       where P is a point on line WC
      Q = (P - B) * v + B       and Q is a point on the line BP

    Where v is computed as the distance from the line parallel to WC through B,
    normalized such that point W and C is at distance 1 the line.

  - We keep the inner triangle non-degenerate, i.e. it can not be 0-size.  This
    guarantees that determinants are never zero.

  - We use delayed layout to recompute the dial image; this helps during vigoruous
    use of the hue-ring or resizing of the widget.

  - Also, we calculate the triangle corners during layout; this keeps manipulation
    of saturation and value very cheap [just move the inner ball].

  - Maybe for slow machines a mode to NOT update dial during dragging.

  - Now that it works I need some beer....
*/

#define RINGDIAMETER  60        // Default ring outer diameter
#define RINGWIDTH     14        // Default ring width
#define MINTRIANGLE   5         // Minimum triangle radius


using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXColorRing) FXColorRingMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXColorRing::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXColorRing::onMotion),
  FXMAPFUNC(SEL_MOUSEWHEEL,0,FXColorRing::onMouseWheel),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXColorRing::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXColorRing::onLeftBtnRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXColorRing::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXColorRing::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorRing::ID_SETHELPSTRING,FXColorRing::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorRing::ID_GETHELPSTRING,FXColorRing::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorRing::ID_SETTIPSTRING,FXColorRing::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXColorRing::ID_GETTIPSTRING,FXColorRing::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXColorRing,FXFrame,FXColorRingMap,ARRAYNUMBER(FXColorRingMap))


// Init
FXColorRing::FXColorRing(){
  flags|=FLAG_ENABLED;
  hsv[0]=0.0f;
  hsv[1]=0.0f;
  hsv[2]=1.0f;
  ringwidth=RINGWIDTH;
  ringouter=RINGDIAMETER/2;
  ringinner=RINGDIAMETER/2-RINGWIDTH;
  dialx=0;
  dialy=0;
  clrx=clry=0;
  blkx=blky=0;
  whtx=whty=0;
  satvalx=0;
  satvaly=0;
  huex=0;
  huey=0;
  mode=MOUSE_NONE;
  }


// Make a color ring
FXColorRing::FXColorRing(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED;
  dial=new FXImage(getApp(),NULL,IMAGE_DITHER|IMAGE_KEEP|IMAGE_OWNED|IMAGE_SHMI|IMAGE_SHMP,RINGDIAMETER,RINGDIAMETER);
  target=tgt;
  message=sel;
  hsv[0]=0.0f;
  hsv[1]=0.0f;
  hsv[2]=1.0f;
  ringwidth=RINGWIDTH;
  ringouter=RINGDIAMETER/2;
  ringinner=RINGDIAMETER/2-RINGWIDTH;
  dialx=0;
  dialy=0;
  clrx=clry=0;
  blkx=blky=0;
  whtx=whty=0;
  satvalx=0;
  satvaly=0;
  huex=0;
  huey=0;
  mode=MOUSE_NONE;
  }


// Create window
void FXColorRing::create(){
  FXFrame::create();
  updatering();
  dial->create();
  }


// Detach window
void FXColorRing::detach(){
  FXFrame::detach();
  dial->detach();
  }


// Get default width
FXint FXColorRing::getDefaultWidth(){
  return RINGDIAMETER+padleft+padright+(border<<1);
  }


// Get default height
FXint FXColorRing::getDefaultHeight(){
  return RINGDIAMETER+padtop+padbottom+(border<<1);
  }


// Resize the dial
void FXColorRing::layout(){
  register FXint ww=width-padleft-padright-(border<<1);
  register FXint hh=height-padtop-padbottom-(border<<1);
  register FXint ss;

  // Enforce minimum triangle size
  ringinner=FXMIN(ww,hh)/2-ringwidth;
  if(ringinner<=MINTRIANGLE) ringinner=MINTRIANGLE;
  ringouter=ringinner+ringwidth;

  // Size is odd so center falls on whole pixel
  ss=ringouter+ringouter+1;

  // New dial location in widget
  dialx=border+padleft+(ww-ss)/2;
  dialy=border+padtop+(hh-ss)/2;

  // Do work if size changed or marked dirty
  if((dial->getWidth()!=ss) || (flags&FLAG_DIRTY)){

    // Size has changed, resize the off-screen image
    if(dial->getWidth()!=ss) dial->resize(ss,ss);

    // Update ring image
    updatering();

    // Rerender to server
    dial->render();

    update();
    }

  // Update hue ball position
  hueToXY(huex,huey,hsv[0]);

  // Update saturation and value ball position
  satValToXY(satvalx,satvaly,hsv[1],hsv[2]);

  flags&=~FLAG_DIRTY;
  }


// Recompute the dial image
void FXColorRing::updatering(){
  register FXfloat invdet,a,s,v;
  register FXint o2,i2,r2,rx,ry,x,y;
  FXfloat r,g,b;

  // Hue angle in radians
  a=(hsv[0]-180.0f)*DTOR;

  // Calculate triangle points
  clrx=(FXint)(ringinner*cosf(a)+0.5f);
  clry=(FXint)(ringinner*sinf(a)+0.5f);
  blkx=(FXint)(ringinner*cosf(a+2.0f*PI/3.0f)+0.5f);
  blky=(FXint)(ringinner*sinf(a+2.0f*PI/3.0f)+0.5f);
  whtx=(FXint)(ringinner*cosf(a-2.0f*PI/3.0f)+0.5f);
  whty=(FXint)(ringinner*sinf(a-2.0f*PI/3.0f)+0.5f);

  // To test for ring
  o2=ringouter*ringouter;
  i2=ringinner*ringinner;

  // Determinant is OK because of MINTRIANGLE constraint
  invdet=1.0f/((whty-clry)*(whtx-blkx)+(clrx-whtx)*(whty-blky));

  // Loop over pixels
  for(y=0; y<dial->getHeight(); y++){
    ry=y-ringouter;
    for(x=0; x<dial->getWidth(); x++){
      rx=x-ringouter;

      // Inside outer ring
      if((r2=(rx*rx+ry*ry))<=o2){

        // Outside inner ring
        if(i2<=r2){

          // Compute color
          fxhsv_to_rgb(r,g,b,atan2f(ry,rx)*RTOD+180.0f,1.0f,1.0f);
          dial->setPixel(x,y,FXRGB(255.0f*r,255.0f*g,255.0f*b));
          continue;
          }

        // Inside triangle
        if(0<=(clry-blky)*(rx-clrx)-(clrx-blkx)*(ry-clry) && 0<=(whty-clry)*(rx-whtx)-(whtx-clrx)*(ry-whty) && 0<=(blky-whty)*(rx-blkx)-(blkx-whtx)*(ry-blky)){

          // Compute saturation and value in triangle
          v=((whty-clry)*(rx-blkx)+(clrx-whtx)*(ry-blky)) * invdet;
          s=((whty-blky)*(rx-blkx)+(blkx-whtx)*(ry-blky)) * invdet;

          // Compute color
          fxhsv_to_rgb(r,g,b,hsv[0],s,v);
          dial->setPixel(x,y,FXRGB(255.0f*r,255.0f*g,255.0f*b));
          continue;
          }
        }

      // Just set background
      dial->setPixel(x,y,backColor);
      }
    }
  }


// Test if inside hue ring
FXbool FXColorRing::inHueRing(FXint x,FXint y) const {
  register FXint rx=x-dialx-ringouter;
  register FXint ry=y-dialy-ringouter;
  return ringinner*ringinner<=rx*rx+ry*ry;
  }


// Compute hue from position on ring x, y
FXfloat FXColorRing::hueFromXY(FXint x,FXint y) const {
  return atan2f(y-dialy-ringouter,x-dialx-ringouter)*RTOD+180.0f;
  }


// Compute position on ring from hue
void FXColorRing::hueToXY(FXint& x,FXint& y,FXfloat hue) const {
  register FXfloat a=(hue-180.0f)*DTOR;
  register FXfloat r=ringouter-ringwidth*0.5f;
  x=dialx+ringouter+(FXint)(r*cosf(a)+0.5f);
  y=dialy+ringouter+(FXint)(r*sinf(a)+0.5f);
  }



// Test if inside saturation/value triangle
FXbool FXColorRing::inTriangle(FXint x,FXint y) const {
  register FXint rx=x-dialx-ringouter;
  register FXint ry=y-dialy-ringouter;
  return 0<=(clry-blky)*(rx-clrx)-(clrx-blkx)*(ry-clry) && 0<=(whty-clry)*(rx-whtx)-(whtx-clrx)*(ry-whty) && 0<=(blky-whty)*(rx-blkx)-(blkx-whtx)*(ry-blky);
  }


// Compute x,y location from saturation and value
void FXColorRing::satValToXY(FXint& x,FXint& y,FXfloat s,FXfloat v) const {
  register FXfloat px=whtx+(clrx-whtx)*s;
  register FXfloat py=whty+(clry-whty)*s;
  x=dialx+ringouter+blkx+(FXint)((px-blkx)*v+0.5f);
  y=dialy+ringouter+blky+(FXint)((py-blky)*v+0.5f);
  }


// Compute saturation and value given x, y in triangle and hue
void FXColorRing::satValFromXY(FXfloat& s,FXfloat& v,FXint x,FXint y) const {
  register FXint rx=x-dialx-ringouter;
  register FXint ry=y-dialy-ringouter;
  register FXfloat ss,vv;

  // Outside triangle on blk-clr side
  if((clry-blky)*(rx-clrx)-(clrx-blkx)*(ry-clry)<0){
    ss=1.0f;
    vv=(FXfloat)((clrx-blkx)*(rx-blkx)+(clry-blky)*(ry-blky)) / (FXfloat)((clrx-blkx)*(clrx-blkx)+(clry-blky)*(clry-blky));
    }

  // Outside triangle on wht-clr side
  else if((whty-clry)*(rx-whtx)-(whtx-clrx)*(ry-whty)<0){
    vv=1.0f;
    ss=(FXfloat)((clrx-whtx)*(rx-whtx)+(clry-whty)*(ry-whty)) / (FXfloat)((clrx-whtx)*(clrx-whtx)+(clry-whty)*(clry-whty));
    }

  // Outside triangle on blk-wht side
  else if((blky-whty)*(rx-blkx)-(blkx-whtx)*(ry-blky)<0){
    ss=0.0f;
    vv=(FXfloat)((whtx-blkx)*(rx-blkx)+(whty-blky)*(ry-blky)) / (FXfloat)((whtx-blkx)*(whtx-blkx)+(whty-blky)*(whty-blky));
    }

  // Inside the triangle; this is a bit nasty...
  else{
    vv=(FXfloat)((whty-clry)*(rx-blkx)+(clrx-whtx)*(ry-blky)) / (FXfloat)((whty-clry)*(whtx-blkx)+(clrx-whtx)*(whty-blky));

    // Take care of degenerate case at B
    if(vv<=0.0f){
      vv=0.0f;
      ss=0.0f;
      }

    // Generic case
    else{
      if(vv>=1.0f) vv=1.0f;

      // Don't divide by zero
      if(clry==whty){
        ss=(FXfloat) (rx-blkx-vv*(whtx-blkx)) / (vv*(clrx-whtx));
        }
      else{
        ss=(FXfloat) (ry-blky-vv*(whty-blky)) / (vv*(clry-whty));
        }
      }
    }

  // Clamp to range
  s=FXCLAMP(0.0f,ss,1.0f);
  v=FXCLAMP(0.0f,vv,1.0f);
  }



// Set help using a message
long FXColorRing::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXColorRing::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXColorRing::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXColorRing::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXColorRing::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXColorRing::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Handle repaint
long FXColorRing::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);

  // Fill around sides
  dc.setForeground(backColor);
  dc.fillRectangle(border,border,dialx-border,height-(border<<1));
  dc.fillRectangle(dialx+dial->getWidth(),border,width-border-dialx-dial->getWidth(),height-(border<<1));
  dc.fillRectangle(dialx,border,dial->getWidth(),dialy-border);
  dc.fillRectangle(dialx,dialy+dial->getHeight(),dial->getWidth(),height-border-dialy-dial->getHeight());

  // Draw dial
  dc.drawImage(dial,dialx,dialy);

  dc.setForeground(borderColor);
  dc.drawArc(dialx+1,dialy,ringouter+ringouter,ringouter+ringouter,90*64,45*64);
  dc.drawArc(dialx,dialy+1,ringouter+ringouter,ringouter+ringouter,135*64,45*64);
  dc.setForeground(baseColor);
  dc.drawArc(dialx-1,dialy,ringouter+ringouter,ringouter+ringouter,270*64,45*64);
  dc.drawArc(dialx,dialy-1,ringouter+ringouter,ringouter+ringouter,315*64,45*64);
  dc.setForeground(shadowColor);
  dc.drawArc(dialx,dialy,ringouter+ringouter,ringouter+ringouter,45*64,180*64);
  dc.drawArc(dialx+ringwidth,dialy+ringwidth,ringinner+ringinner,ringinner+ringinner,225*64,180*64);
  dc.setForeground(hiliteColor);
  dc.drawArc(dialx,dialy,ringouter+ringouter,ringouter+ringouter,225*64,180*64);
  dc.drawArc(dialx+ringwidth,dialy+ringwidth,ringinner+ringinner,ringinner+ringinner,45*64,180*64);

  // Draw spots
  dc.setForeground(FXRGB(255,255,255));
  dc.fillArc(satvalx-3,satvaly-3,7,7,0,360*64);
  dc.fillArc(huex-3,huey-3,7,7,0,360*64);
  dc.setForeground(FXRGB(0,0,0));
  dc.fillArc(satvalx-2,satvaly-2,5,5,0,360*64);
  dc.fillArc(huex-2,huey-2,5,5,0,360*64);
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Determine if special case applies
FXbool FXColorRing::inCorner(FXint x,FXint y) const {
  register FXint rx=x-dialx-ringouter;
  register FXint ry=y-dialy-ringouter;
  return (ringouter*ringouter<=rx*rx+ry*ry) && (0.99999f<=hsv[1] && 0.99999f<=hsv[2]);
  }


// Moving
long FXColorRing::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXfloat s,v;
  flags&=~FLAG_TIP;
  if(mode!=MOUSE_NONE){
    if((mode==MOUSE_HUE) || inCorner(event->win_x,event->win_y)){       // Changing hue
      setHue(hueFromXY(event->win_x,event->win_y));
      }
    else{                                                               // Changing saturation and value
      satValFromXY(s,v,event->win_x,event->win_y);
      setHueSatVal(hsv[0],s,v);
      }
    flags|=FLAG_CHANGED;
    if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)hsv);
    return 1;
    }
  return 0;
  }


// Move spot to change hue, saturation
long FXColorRing::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXfloat s,v;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(inHueRing(event->win_x,event->win_y)){
      setHue(hueFromXY(event->win_x,event->win_y));
      mode=MOUSE_HUE;
      }
    else if(inTriangle(event->win_x,event->win_y)){
      satValFromXY(s,v,event->win_x,event->win_y);
      setHueSatVal(hsv[0],s,v);
      mode=MOUSE_SATVAL;
      }
    flags|=FLAG_CHANGED;
    flags&=~FLAG_UPDATE;
    if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)hsv);
    }
  return 1;
  }


// End spot movement mode
long FXColorRing::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint changed=(flags&FLAG_CHANGED);
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~FLAG_CHANGED;
    mode=MOUSE_NONE;
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(changed && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)hsv);
    return 1;
    }
  return 1;
  }


// Rotate hue by means of dial
long FXColorRing::onMouseWheel(FXObject*,FXSelector,void* ptr){
  FXfloat amount=((FXEvent*)ptr)->code/12.0f;
  if(isEnabled()){
    if(((FXEvent*)ptr)->state&CONTROLMASK) amount/=10.0f;
    setHue(fmodf(hsv[0]+amount+360.0f,360.0f));
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)hsv);
    return 1;
    }
  return 0;
  }


// Change hue
void FXColorRing::setHue(FXfloat h){
  h=FXCLAMP(0.0f,h,360.0f);
  if(hsv[0]!=h){
    hsv[0]=h;
    update(huex-4,huey-4,9,9);
    hueToXY(huex,huey,hsv[0]);
    update(huex-4,huey-4,9,9);
    recalc();
    }
  }


// Change saturation
void FXColorRing::setSat(FXfloat s){
  s=FXCLAMP(0.0f,s,1.0f);
  if(hsv[1]!=s){
    hsv[1]=s;
    update(satvalx-4,satvaly-4,9,9);
    satValToXY(satvalx,satvaly,hsv[1],hsv[2]);
    update(satvalx-4,satvaly-4,9,9);
    }
  }


// Change saturation
void FXColorRing::setVal(FXfloat v){
  v=FXCLAMP(0.0f,v,1.0f);
  if(hsv[2]!=v){
    hsv[2]=v;
    update(satvalx-4,satvaly-4,9,9);
    satValToXY(satvalx,satvaly,hsv[1],hsv[2]);
    update(satvalx-4,satvaly-4,9,9);
    }
  }



// Set hue, saturation, value
void FXColorRing::setHueSatVal(FXfloat h,FXfloat s,FXfloat v){

  // Clamp
  h=FXCLAMP(0.0f,h,360.0f);
  s=FXCLAMP(0.0f,s,1.0f);
  v=FXCLAMP(0.0f,v,1.0f);

  // Changed after clamping?
  if(hsv[0]!=h || hsv[1]!=s || hsv[2]!=v){

    // Cheap case: just move the ball
    if(hsv[1]!=s || hsv[2]!=v){
      hsv[1]=s;
      hsv[2]=v;
      update(satvalx-4,satvaly-4,9,9);
      satValToXY(satvalx,satvaly,hsv[1],hsv[2]);
      update(satvalx-4,satvaly-4,9,9);
      }

    // Expensive case: recalculate dial
    if(hsv[0]!=h){
      hsv[0]=h;
      recalc();
      }
    }
  }


// Change width of hue ring
void FXColorRing::setRingWidth(FXint rw){
  if(rw<4) rw=4;
  if(ringwidth!=rw){
    ringwidth=rw;
    recalc();
    }
  }


// Save data
void FXColorRing::save(FXStream& store) const {
  FXFrame::save(store);
  store << dial;
  store << hsv[0];
  store << hsv[1];
  store << hsv[2];
  store << ringwidth;
  store << tip;
  store << help;
  }


// Load data
void FXColorRing::load(FXStream& store){
  FXFrame::load(store);
  store >> dial;
  store >> hsv[0];
  store >> hsv[1];
  store >> hsv[2];
  store >> ringwidth;
  store >> tip;
  store >> help;
  }


// Destroy
FXColorRing::~FXColorRing(){
  delete dial;
  dial=(FXImage*)-1L;
  }

}
