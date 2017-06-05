/********************************************************************************
*                                                                               *
*                         R u l e r V i e w   W i d g e t                       *
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
* $Id: FXRulerView.cpp,v 1.16 2006/01/28 20:29:30 fox Exp $                     *
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
#include "FXScrollBar.h"
#include "FXScrollArea.h"
#include "FXRuler.h"
#include "FXRulerView.h"


/*
  Notes:
  - Should implement DND drags/drops, cut/paste
  - Right-mouse scroll.
*/


#define RULERVIEW_MASK (RULERVIEW_ALIGN_STRETCH_X|RULERVIEW_ALIGN_STRETCH_Y)

#define HSCROLLMASK (HSCROLLER_ALWAYS|HSCROLLER_NEVER|HSCROLLING_ON|HSCROLLING_OFF|SCROLLERS_DONT_TRACK)

#define VSCROLLMASK (VSCROLLER_ALWAYS|VSCROLLER_NEVER|VSCROLLING_ON|VSCROLLING_OFF|SCROLLERS_DONT_TRACK)


using namespace FX;


/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXRulerView) FXRulerViewMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXRulerView::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXRulerView::onMotion),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXRulerView::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXRulerView::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXRulerView::ID_SETHELPSTRING,FXRulerView::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXRulerView::ID_GETHELPSTRING,FXRulerView::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXRulerView::ID_SETTIPSTRING,FXRulerView::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXRulerView::ID_GETTIPSTRING,FXRulerView::onCmdGetTip),
  FXMAPFUNC(SEL_CHANGED,FXRulerView::ID_HRULER,FXRulerView::onDocChanged),
  FXMAPFUNC(SEL_CHANGED,FXRulerView::ID_VRULER,FXRulerView::onDocChanged),
  };


// Object implementation
FXIMPLEMENT(FXRulerView,FXScrollArea,FXRulerViewMap,ARRAYNUMBER(FXRulerViewMap))


// Deserialization
FXRulerView::FXRulerView(){
  flags|=FLAG_ENABLED;
  hruler=NULL;
  vruler=NULL;
  filler=NULL;
  docColor=0;
  }


// Construct and init
FXRulerView::FXRulerView(FXComposite* p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):FXScrollArea(p,opts,x,y,w,h){
  hruler=new FXRuler(this,this,ID_HRULER,RULER_HORIZONTAL|RULER_NUMBERS|RULER_ARROW|RULER_TICKS_CENTER|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
  vruler=new FXRuler(this,this,ID_VRULER,RULER_VERTICAL|RULER_NUMBERS|RULER_ARROW|RULER_TICKS_CENTER|LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y|LAYOUT_FILL_ROW);
  filler=new FXFrame(this,FRAME_NONE);
  backColor=getApp()->getShadowColor();
  docColor=getApp()->getBackColor();
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  }


// Get document position X
FXint FXRulerView::getDocumentX() const {
  return vruler->getWidth()+hruler->getDocumentLower();
  }


// Get document position Y
FXint FXRulerView::getDocumentY() const {
  return hruler->getHeight()+vruler->getDocumentLower();
  }


// Set document width
void FXRulerView::setDocumentWidth(FXint w,FXbool notify){
  hruler->setDocumentSize(w,notify);
  }


// Get document width
FXint FXRulerView::getDocumentWidth() const {
  return hruler->getDocumentSize();
  }


// Set document height
void FXRulerView::setDocumentHeight(FXint h,FXbool notify){
  vruler->setDocumentSize(h,notify);
  }


// Get document height
FXint FXRulerView::getDocumentHeight() const {
  return vruler->getDocumentSize();
  }


// Default viewport width
FXint FXRulerView::getViewportWidth(){
  return width-vruler->getDefaultWidth();
  }


// Default viewport height
FXint FXRulerView::getViewportHeight(){
  return height-hruler->getDefaultHeight();
  }


// Determine minimum content width of scroll area
FXint FXRulerView::getContentWidth(){
  return hruler->getContentSize();
  }


// Determine minimum content height of scroll area
FXint FXRulerView::getContentHeight(){
  return vruler->getContentSize();
  }


// Move content
void FXRulerView::moveContents(FXint x,FXint y){
  hruler->setPosition(x);
  vruler->setPosition(y);
  scroll(vruler->getDefaultWidth(),hruler->getDefaultHeight(),viewport_w,viewport_h,x-pos_x,y-pos_y);
  pos_x=x;
  pos_y=y;
  }


// Recalculate layout
void FXRulerView::layout(){
  FXint vrw,hrh;

  // Layout scroll bars and viewport
  FXScrollArea::layout();

  // Place rulers
  vrw=vruler->getDefaultWidth();
  hrh=hruler->getDefaultHeight();
  hruler->position(vrw,0,viewport_w,hrh);
  vruler->position(0,hrh,vrw,viewport_h);
  filler->position(0,0,vrw,hrh);

  // Redraw
  update();

  // Clean
  flags&=~FLAG_DIRTY;
  }


// Draw document background
void FXRulerView::drawBackground(FXDCWindow& dc){
  FXint docx,docy,docw,doch;

  // Background
  dc.setForeground(backColor);
  docx=getDocumentX();
  docy=getDocumentY();
  docw=getDocumentWidth();
  doch=getDocumentHeight();

  dc.fillRectangle(0,0,docx,height);
  dc.fillRectangle(docx+docw,0,width-docx-docw,height);
  dc.fillRectangle(docx,0,docw,docy);
  dc.fillRectangle(docx,docy+doch,docw,height-docy-doch);

  // Document insides
  dc.setForeground(docColor);
  dc.fillRectangle(docx,docy,docw,doch);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawRectangle(docx-1,docy-1,docw+1,doch+1);
  dc.fillRectangle(docx+1,docy+doch+1,docw+2,2);
  dc.fillRectangle(docx+docw+1,docy+1,2,doch+2);
  }


// Draw contents
void FXRulerView::drawContents(FXDCWindow&){
  }


// Handle repaint
long FXRulerView::onPaint(FXObject*,FXSelector,void* ptr){
  FXDCWindow dc(this,(FXEvent*)ptr);
  drawBackground(dc);
  drawContents(dc);
  return 1;
  }


// Moving
long FXRulerView::onMotion(FXObject*,FXSelector,void*ptr){
  setArrowPosX(((FXEvent*)ptr)->win_x-getDocumentX());
  setArrowPosY(((FXEvent*)ptr)->win_y-getDocumentY());
  return 0;
  }


// Set help using a message
long FXRulerView::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXRulerView::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXRulerView::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXRulerView::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXRulerView::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXScrollArea::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXRulerView::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXScrollArea::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Something in the rulers was changed
long FXRulerView::onDocChanged(FXObject*,FXSelector,void* ptr){
  return target && target->handle(this,FXSEL(SEL_CHANGED,message),ptr);
  }


// Set the current document color
void FXRulerView::setDocumentColor(FXColor clr){
  if(clr!=docColor){
    docColor=clr;
    update();
    }
  }


// Set horizontal alignment; the default is RULER_ALIGN_NORMAL
void FXRulerView::setHAlignment(FXuint align,FXbool notify){
  if(hruler->getRulerAlignment()!=align){
    if(align==RULER_ALIGN_STRETCH)
      setScrollStyle(HSCROLLER_NEVER | (getScrollStyle()&VSCROLLMASK));
    else
      setScrollStyle(HSCROLLING_ON | (getScrollStyle()&VSCROLLMASK));
    update();
    hruler->setRulerAlignment(align,notify);
    }
  }


// Return horizontal alignment
FXuint FXRulerView::getHAlignment() const {
  return hruler->getRulerAlignment();
  }


// Set vertical alignment; the default is RULER_ALIGN_NORMAL
void FXRulerView::setVAlignment(FXuint align,FXbool notify){
  if(vruler->getRulerAlignment()!=align){
    if(align==RULER_ALIGN_STRETCH)
      setScrollStyle(VSCROLLER_NEVER | (getScrollStyle()&HSCROLLMASK));
    else
      setScrollStyle(VSCROLLING_ON | (getScrollStyle()&HSCROLLMASK));
    update();
    vruler->setRulerAlignment(align,notify);
    }
  }


// Return vertical alignment
FXuint FXRulerView::getVAlignment() const {
  return vruler->getRulerAlignment();
  }


// Set X arrow position
void FXRulerView::setArrowPosX(FXint x){
  hruler->setValue(x);
  }

// Set X arrow position
void FXRulerView::setArrowPosY(FXint y){
  vruler->setValue(y);
  }

// Get X arrow position
FXint FXRulerView::getArrowPosX() const {
  return hruler->getValue();
  }

// Get Y arrow position
FXint FXRulerView::getArrowPosY() const {
  return vruler->getValue();
  }


// Set the horizontal ruler font
void FXRulerView::setHRulerFont(FXFont *fnt,FXbool notify){
  hruler->setFont(fnt,notify);
  recalc();
  }


// Get the horizontal ruler font
FXFont* FXRulerView::getHRulerFont() const {
  return hruler->getFont();
  }


// Set the vertical ruler font
void FXRulerView::setVRulerFont(FXFont *fnt,FXbool notify){
  vruler->setFont(fnt,notify);
  recalc();
  }


// Get the vertical ruler font
FXFont* FXRulerView::getVRulerFont() const {
  return vruler->getFont();
  }


// Change edge spacing around document
void FXRulerView::setHEdgeSpacing(FXint es,FXbool notify){
  hruler->setEdgeSpacing(es,notify);
  }


// Change edge spacing around document
void FXRulerView::setVEdgeSpacing(FXint es,FXbool notify){
  vruler->setEdgeSpacing(es,notify);
  }


// Return horizontal edge spacing
FXint FXRulerView::getHEdgeSpacing() const {
  return hruler->getEdgeSpacing();
  }


// Return vertical edge spacing
FXint FXRulerView::getVEdgeSpacing() const {
  return vruler->getEdgeSpacing();
  }


// Change horizontal lower margin
void FXRulerView::setHMarginLower(FXint marg,FXbool notify){
  hruler->setMarginLower(marg,notify);
  }


// Change horizontal upper margin
void FXRulerView::setHMarginUpper(FXint marg,FXbool notify){
  hruler->setMarginUpper(marg,notify);
  }


// Get horizontal lower margin
FXint FXRulerView::getHMarginLower() const {
  return hruler->getMarginLower();
  }


// Get horizontal upper margin
FXint FXRulerView::getHMarginUpper() const {
  return hruler->getMarginUpper();
  }


// Change vertical lower margin
void FXRulerView::setVMarginLower(FXint marg,FXbool notify){
  vruler->setMarginLower(marg,notify);
  }


// Change vertical upper margin
void FXRulerView::setVMarginUpper(FXint marg,FXbool notify){
  vruler->setMarginUpper(marg,notify);
  }


// Get vertical lower margin
FXint FXRulerView::getVMarginLower() const {
  return vruler->getMarginLower();
  }


// Get vertical upper margin
FXint FXRulerView::getVMarginUpper() const {
  return vruler->getMarginUpper();
  }


// Change horizontal document number placement
void FXRulerView::setHNumberTicks(FXint ticks,FXbool notify){
  hruler->setNumberTicks(ticks,notify);
  }


// Change vertical document number placement
void FXRulerView::setVNumberTicks(FXint ticks,FXbool notify){
  vruler->setNumberTicks(ticks,notify);
  }


// Return horizontal document number placement
FXint FXRulerView::getHNumberTicks() const {
  return hruler->getNumberTicks();
  }


// Return vertical document number placement
FXint FXRulerView::getVNumberTicks() const {
  return vruler->getNumberTicks();
  }


// Change horizontal document major ticks
void FXRulerView::setHMajorTicks(FXint ticks,FXbool notify){
  hruler->setMajorTicks(ticks,notify);
  }


// Change vertical document major ticks
void FXRulerView::setVMajorTicks(FXint ticks,FXbool notify){
  vruler->setMajorTicks(ticks,notify);
  }


// Return horizontal document major ticks
FXint FXRulerView::getHMajorTicks() const {
  return hruler->getMajorTicks();
  }


// Return vertical document major ticks
FXint FXRulerView::getVMajorTicks() const {
  return vruler->getMajorTicks();
  }


// Change horizontal document medium ticks
void FXRulerView::setHMediumTicks(FXint ticks,FXbool notify){
  hruler->setMediumTicks(ticks,notify);
  }


// Change vertical document medium ticks
void FXRulerView::setVMediumTicks(FXint ticks,FXbool notify){
  vruler->setMediumTicks(ticks,notify);
  }


// Return horizontal document medium ticks
FXint FXRulerView::getHMediumTicks() const {
  return hruler->getMediumTicks();
  }


// Return vertical document medium ticks
FXint FXRulerView::getVMediumTicks() const {
  return vruler->getMediumTicks();
  }


// Change horizontal document tiny ticks
void FXRulerView::setHTinyTicks(FXint ticks,FXbool notify){
  hruler->setTinyTicks(ticks,notify);
  }

// Change vertical document tiny ticks
void FXRulerView::setVTinyTicks(FXint ticks,FXbool notify){
  vruler->setTinyTicks(ticks,notify);
  }


// Return horizontal document tiny ticks
FXint FXRulerView::getHTinyTicks() const {
  return hruler->getTinyTicks();
  }


// Return vertical document tiny ticks
FXint FXRulerView::getVTinyTicks() const {
  return vruler->getTinyTicks();
  }


// Change horizontal pixel per tick spacing
void FXRulerView::setHPixelPerTick(FXdouble space,FXbool notify){
  hruler->setPixelPerTick(space,notify);
  }


// Change vertical pixel per tick spacing
void FXRulerView::setVPixelPerTick(FXdouble space,FXbool notify){
  vruler->setPixelPerTick(space,notify);
  }


// Return horizontal pixel per tick spacing
FXdouble FXRulerView::getHPixelPerTick() const {
  return hruler->getPixelPerTick();
  }


// Return vertical pixel per tick spacing
FXdouble FXRulerView::getVPixelPerTick() const {
  return vruler->getPixelPerTick();
  }


// Set horizontal ruler style
void FXRulerView::setHRulerStyle(FXuint style){
  hruler->setRulerStyle(style);
  }


// Set vertical ruler style
void FXRulerView::setVRulerStyle(FXuint style){
  hruler->setRulerStyle(style);
  }


// Get horizontal ruler style
FXuint FXRulerView::getHRulerStyle() const {
  return hruler->getRulerStyle();
  }


// Get vertical ruler style
FXuint FXRulerView::getVRulerStyle() const {
  return vruler->getRulerStyle();
  }



// Save object to stream
void FXRulerView::save(FXStream& store) const {
  FXScrollArea::save(store);
  store << hruler;
  store << vruler;
  store << filler;
  store << docColor;
  store << tip;
  store << help;
  }


// Load object from stream
void FXRulerView::load(FXStream& store){
  FXScrollArea::load(store);
  store >> hruler;
  store >> vruler;
  store >> filler;
  store >> docColor;
  store >> tip;
  store >> help;
  }


// Destroy
FXRulerView::~FXRulerView(){
  hruler=(FXRuler*)-1L;
  vruler=(FXRuler*)-1L;
  filler=(FXFrame*)-1L;
  }

}
