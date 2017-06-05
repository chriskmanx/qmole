/********************************************************************************
*                                                                               *
*                               T a b   O b j e c t                             *
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
* $Id: FXTabBar.cpp,v 1.28 2006/01/22 17:58:45 fox Exp $                        *
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
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXTabBar.h"
#include "FXTabBook.h"


/*
  Notes:
  - Should focus go to tab items?
  - Tab items should observe various border styles.
  - TAB/TABTAB should go into content, arrow keys navigate between tabs.
*/


#define TAB_ORIENT_MASK    (TAB_TOP|TAB_LEFT|TAB_RIGHT|TAB_BOTTOM)
#define TABBOOK_MASK       (TABBOOK_SIDEWAYS|TABBOOK_BOTTOMTABS)
#define REVEAL_PIXELS      20

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXTabBar) FXTabBarMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXTabBar::onPaint),
  FXMAPFUNC(SEL_FOCUS_NEXT,0,FXTabBar::onFocusNext),
  FXMAPFUNC(SEL_FOCUS_PREV,0,FXTabBar::onFocusPrev),
  FXMAPFUNC(SEL_FOCUS_UP,0,FXTabBar::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXTabBar::onFocusDown),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FXTabBar::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FXTabBar::onFocusRight),
  FXMAPFUNC(SEL_COMMAND,FXTabBar::ID_OPEN_ITEM,FXTabBar::onCmdOpenItem),
  FXMAPFUNC(SEL_COMMAND,FXTabBar::ID_SETVALUE,FXTabBar::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXTabBar::ID_SETINTVALUE,FXTabBar::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXTabBar::ID_GETINTVALUE,FXTabBar::onCmdGetIntValue),
  FXMAPFUNCS(SEL_UPDATE,FXTabBar::ID_OPEN_FIRST,FXTabBar::ID_OPEN_LAST,FXTabBar::onUpdOpen),
  FXMAPFUNCS(SEL_COMMAND,FXTabBar::ID_OPEN_FIRST,FXTabBar::ID_OPEN_LAST,FXTabBar::onCmdOpen),
  };


// Object implementation
FXIMPLEMENT(FXTabBar,FXPacker,FXTabBarMap,ARRAYNUMBER(FXTabBarMap))


// Make a tab bar
FXTabBar::FXTabBar(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXPacker(p,opts,x,y,w,h,pl,pr,pt,pb,0,0){
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  current=0;
  shift=0;
  }


// Get width
FXint FXTabBar::getDefaultWidth(){
  register FXint w,wtabs,maxtabw,t,ntabs;
  register FXuint hints;
  register FXWindow *child;

  // Left or right tabs
  if(options&TABBOOK_SIDEWAYS){
    wtabs=0;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) t=child->getWidth()-2; else t=child->getDefaultWidth()-2;
        if(t>wtabs) wtabs=t;
        }
      }
    w=wtabs;
    }

  // Top or bottom tabs
  else{
    wtabs=maxtabw=ntabs=0;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) t=child->getWidth(); else t=child->getDefaultWidth();
        if(t>maxtabw) maxtabw=t;
        wtabs+=t;
        ntabs++;
        }
      }
    if(options&PACK_UNIFORM_WIDTH) wtabs=ntabs*maxtabw;
    w=wtabs+5;
    }
  return w+padleft+padright+(border<<1);
  }


// Get height
FXint FXTabBar::getDefaultHeight(){
  register FXint h,htabs,maxtabh,t,ntabs;
  register FXuint hints;
  register FXWindow *child;

  // Left or right tabs
  if(options&TABBOOK_SIDEWAYS){
    htabs=maxtabh=ntabs=0;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_HEIGHT) t=child->getHeight(); else t=child->getDefaultHeight();
        if(t>maxtabh) maxtabh=t;
        htabs+=t;
        ntabs++;
        }
      }
    if(options&PACK_UNIFORM_HEIGHT) htabs=ntabs*maxtabh;
    h=htabs+5;
    }

  // Top or bottom tabs
  else{
    htabs=0;
    for(child=getFirst(); child; child=child->getNext()){
      if(child->shown()){
        hints=child->getLayoutHints();
        if(hints&LAYOUT_FIX_HEIGHT) t=child->getHeight()-2; else t=child->getDefaultHeight()-2;
        if(t>htabs) htabs=t;
        }
      }
    h=htabs;
    }
  return h+padtop+padbottom+(border<<1);
  }


// Recalculate layout
void FXTabBar::layout(){
  register FXint i,px,py,pw,ph,x,y,xx,yy,w,h,maxtabw,maxtabh,cumw,cumh,newcurrent;
  register FXWindow *raisetab=NULL;
  register FXWindow *tab;
  register FXuint hints;

  newcurrent=-1;

  // Measure tabs again
  maxtabw=maxtabh=0;
  for(tab=getFirst(),i=0; tab; tab=tab->getNext(),i++){
    if(tab->shown()){
      hints=tab->getLayoutHints();
      if(newcurrent<0 || i<=current) newcurrent=i;
      if(hints&LAYOUT_FIX_WIDTH) w=tab->getWidth(); else w=tab->getDefaultWidth();
      if(hints&LAYOUT_FIX_HEIGHT) h=tab->getHeight(); else h=tab->getDefaultHeight();
      if(w>maxtabw) maxtabw=w;
      if(h>maxtabh) maxtabh=h;
      }
    }

  // Changes current only if old current no longer visible
  current=newcurrent;

  // Tabs on left or right
  if(options&TABBOOK_SIDEWAYS){

    // Place panel
    py=border+padtop;
    ph=height-padtop-padbottom-(border<<1);

    // Scroll as appropriate
    for(tab=getFirst(),cumh=i=0; tab; tab=tab->getNext(),i++){
      if(tab->shown()){
        hints=tab->getLayoutHints();
        if(hints&LAYOUT_FIX_HEIGHT) h=tab->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=maxtabh;
        else h=tab->getDefaultHeight();
        if(i==current){
          if(tab->getNext()){
            if(cumh+shift+h>ph-REVEAL_PIXELS-2) shift=ph-cumh-h-REVEAL_PIXELS-2;
            }
          else{
            if(cumh+shift+h>ph-2) shift=ph-cumh-h-2;
            }
          if(tab->getPrev()){
            if(cumh+shift<REVEAL_PIXELS+2) shift=REVEAL_PIXELS+2-cumh;
            }
          else{
            if(cumh+shift<2) shift=2-cumh;
            }
          }
        cumh+=h;
        }
      }

    // Adjust shift based on space
    if(shift<ph-cumh-2) shift=ph-cumh-2;
    if(shift>0) shift=0;

    // Place all of the children
    for(tab=getFirst(),yy=py+shift,i=0; tab; tab=tab->getNext(),i++){
      if(tab->shown()){
        hints=tab->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) w=tab->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=maxtabw;
        else w=tab->getDefaultWidth();
        if(hints&LAYOUT_FIX_HEIGHT) h=tab->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=maxtabh;
        else h=tab->getDefaultHeight();
        if(i<current){
          y=yy+2;
          if(y+h>py+ph-2) y=py+ph-2-h;
          if(y<py+2) y=py+2;
          if(options&TABBOOK_BOTTOMTABS)
            tab->position(-4,y,w,h);
          else
            tab->position(width-w+4,y,w,h);
          tab->raise();
          yy+=h;
          }
        else if(i>current){
          y=yy+2;
          if(y+h>py+ph-2) y=py+ph-h-2;
          if(y<py+2) y=py+2;
          if(options&TABBOOK_BOTTOMTABS)
            tab->position(-4,y,w,h);
          else
            tab->position(width-w+4,y,w,h);
          tab->lower();
          yy+=h;
          }
        else{
          y=yy;
          if(y+h>py+ph-2) y=py+ph-h-2;
          if(y<py) y=py;
          if(options&TABBOOK_BOTTOMTABS)
            tab->position(-2,y,w,h);
          else
            tab->position(width-w+2,y,w,h);
          raisetab=tab;
          yy+=h-3;
          }
        }
      }
    }

  // Tabs on top or bottom
  else{

    // Place panel
    px=border+padleft;
    pw=width-padleft-padright-(border<<1);

    // Scroll as appropriate
    for(tab=getFirst(),cumw=i=0; tab; tab=tab->getNext(),i++){
      if(tab->shown()){
        hints=tab->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) w=tab->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=maxtabw;
        else w=tab->getDefaultWidth();
        if(i==current){
          if(tab->getNext()){
            if(cumw+shift+w>pw-REVEAL_PIXELS-2) shift=pw-cumw-w-REVEAL_PIXELS-2;
            }
          else{
            if(cumw+shift+w>pw-2) shift=pw-cumw-w-2;
            }
          if(tab->getPrev()){
            if(cumw+shift<REVEAL_PIXELS+2) shift=REVEAL_PIXELS+2-cumw;
            }
          else{
            if(cumw+shift<2) shift=2-cumw;
            }
          }
        cumw+=w;
        }
      }

    // Adjust shift based on space
    if(shift<pw-cumw-2) shift=pw-cumw-2;
    if(shift>0) shift=0;

    // Place all of the children
    for(tab=getFirst(),xx=px+shift,i=0; tab; tab=tab->getNext(),i++){
      if(tab->shown()){
        hints=tab->getLayoutHints();
        if(hints&LAYOUT_FIX_WIDTH) w=tab->getWidth();
        else if(options&PACK_UNIFORM_WIDTH) w=maxtabw;
        else w=tab->getDefaultWidth();
        if(hints&LAYOUT_FIX_HEIGHT) h=tab->getHeight();
        else if(options&PACK_UNIFORM_HEIGHT) h=maxtabh;
        else h=tab->getDefaultHeight();
        if(i<current){
          x=xx+2;
          if(x+w>px+pw-2) x=px+pw-2-w;
          if(x<px+2) x=px+2;
          if(options&TABBOOK_BOTTOMTABS)
            tab->position(x,-4,w,h);
          else
            tab->position(x,height-h+4,w,h);
          tab->raise();
          xx+=w;
          }
        else if(i>current){
          x=xx+2;
          if(x+w>px+pw-2) x=px+pw-w-2;
          if(x<px+2) x=px+2;
          if(options&TABBOOK_BOTTOMTABS)
            tab->position(xx+2,-4,w,h);
          else
            tab->position(xx+2,height-h+4,w,h);
          tab->lower();
          xx+=w;
          }
        else{
          x=xx;
          if(x+w>px+pw-2) x=px+pw-w-2;
          if(x<px) x=px;
          if(options&TABBOOK_BOTTOMTABS)
            tab->position(xx,-2,w,h);
          else
            tab->position(xx,height-h+2,w,h);
          raisetab=tab;
          xx+=w-3;
          }
        }
      }
    }

  // Raise tab
  if(raisetab) raisetab->raise();

  flags&=~FLAG_DIRTY;
  }


// Set current subwindow
void FXTabBar::setCurrent(FXint panel,FXbool notify){
  register FXint nc=isMemberOf(&FXTabBook::metaClass)?numChildren()>>1:numChildren();
  if(panel!=current && 0<=panel && panel<nc){
    current=panel;
    recalc();
    if(notify && target){ target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXival)current); }
    }
  }


// Handle repaint
long FXTabBar::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  dc.setForeground(backColor);
  dc.fillRectangle(ev->rect.x,ev->rect.y,ev->rect.w,ev->rect.h);
  drawFrame(dc,0,0,width,height);
  return 1;
  }


// Focus moved to next visible tab
long FXTabBar::onFocusNext(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getFocus();
  if(child) child=child->getNext(); else child=getFirst();
  while(child && !child->shown()) child=child->getNext();
  if(child){
    setCurrent(indexOfChild(child),TRUE);
    child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    return 1;
    }
  return 0;
  }


// Focus moved to previous visible tab
long FXTabBar::onFocusPrev(FXObject*,FXSelector,void* ptr){
  FXWindow *child=getFocus();
  if(child) child=child->getPrev(); else child=getLast();
  while(child && !child->shown()) child=child->getPrev();
  if(child){
    setCurrent(indexOfChild(child),TRUE);
    child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    return 1;
    }
  return 0;
  }


// Focus moved up
long FXTabBar::onFocusUp(FXObject*,FXSelector,void* ptr){
  if(options&TABBOOK_SIDEWAYS){
    return handle(this,FXSEL(SEL_FOCUS_PREV,0),ptr);
    }
  return 0;
  }


// Focus moved down
long FXTabBar::onFocusDown(FXObject*,FXSelector,void* ptr){
  if(options&TABBOOK_SIDEWAYS){
    return handle(this,FXSEL(SEL_FOCUS_NEXT,0),ptr);
    }
  return 0;
  }


// Focus moved left
long FXTabBar::onFocusLeft(FXObject*,FXSelector,void* ptr){
  if(!(options&TABBOOK_SIDEWAYS)){
    return handle(this,FXSEL(SEL_FOCUS_PREV,0),ptr);
    }
  return 0;
  }


// Focus moved right
long FXTabBar::onFocusRight(FXObject*,FXSelector,void* ptr){
  if(!(options&TABBOOK_SIDEWAYS)){
    return handle(this,FXSEL(SEL_FOCUS_NEXT,0),ptr);
    }
  return 0;
  }


// Update value from a message
long FXTabBar::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrent((FXint)(FXival)ptr);
  return 1;
  }


// Update value from a message
long FXTabBar::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setCurrent(*((FXint*)ptr));
  return 1;
  }


// Obtain value from text field
long FXTabBar::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXint*)ptr)=getCurrent();
  return 1;
  }


// Open item
long FXTabBar::onCmdOpen(FXObject*,FXSelector sel,void*){
  setCurrent(FXSELID(sel)-ID_OPEN_FIRST,TRUE);
  return 1;
  }


// Update the nth button
long FXTabBar::onUpdOpen(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,((FXSELID(sel)-ID_OPEN_FIRST)==current)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// The sender of the message is the item to open up
long FXTabBar::onCmdOpenItem(FXObject* sender,FXSelector,void*){
  setCurrent(indexOfChild((FXWindow*)sender),TRUE);
  return 1;
  }


// Get tab style
FXuint FXTabBar::getTabStyle() const {
  return (options&TABBOOK_MASK);
  }


// Set tab style
void FXTabBar::setTabStyle(FXuint style){
  FXuint opts=(options&~TABBOOK_MASK) | (style&TABBOOK_MASK);
  if(options!=opts){
    options=opts;
    recalc();
    update();
    }
  }


// Save object to stream
void FXTabBar::save(FXStream& store) const {
  FXPacker::save(store);
  store << current;
  }


// Load object from stream
void FXTabBar::load(FXStream& store){
  FXPacker::load(store);
  store >> current;
  }

}
