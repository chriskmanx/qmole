/********************************************************************************
*                                                                               *
*                   M a t r i x   C o n t a i n e r   O b j e c t               *
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
* $Id: FXMatrix.cpp,v 1.35 2006/01/22 17:58:35 fox Exp $                        *
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
#include "FXApp.h"
#include "FXMatrix.h"


/*
  Notes:
  - Need to observe FILL vs CENTER options.
  - Filled items should shrink as well as stretch.
  - Stretch should be proportional.
  - Center mode, non-stretch should be observed.
  - Row/Column stretchable iff all elements in row/column have row/column
    stretch hint.
  - Row/Column may be 0 pixels wide
  - We should probably change layout so hidden children do not
    affect numbering (but if all children in a row/column are
    hidden, make the whole row disappear!).
  - Navigating around
  - Packing order:

    MATRIX_BY_ROWS:
    [1] [4] [7]
    [2] [5] [8]
    [3] [6] [9]

    MATRIX_BY_COLUMNS:
    [1] [2] [3]
    [4] [5] [6]
    [7] [8] [9]

  - Possible solution for spanning rows/columns: have a table
    containing mapping from FXWindow* to (nr,nc) span.  Consult
    table during layout.  All children not listed are 1x1, special
    API add/remove items to the table.  Each layout, any item in the
    table for which there is no corresponding child is removed.
    Advantage: no need to add any special info into FXWindow.  Also,
    no need for any special API to add item into matrix.

*/

#define MAXNUM    512        // Maximum number of columns/rows

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXMatrix) FXMatrixMap[]={
  FXMAPFUNC(SEL_FOCUS_UP,0,FXMatrix::onFocusUp),
  FXMAPFUNC(SEL_FOCUS_DOWN,0,FXMatrix::onFocusDown),
  FXMAPFUNC(SEL_FOCUS_LEFT,0,FXMatrix::onFocusLeft),
  FXMAPFUNC(SEL_FOCUS_RIGHT,0,FXMatrix::onFocusRight),
  };


// Object implementation
FXIMPLEMENT(FXMatrix,FXPacker,FXMatrixMap,ARRAYNUMBER(FXMatrixMap))


// Make a vertical one
FXMatrix::FXMatrix(FXComposite* p,FXint n,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXPacker(p,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  num=FXCLAMP(1,n,MAXNUM);
  }


// Find child at given row, column
FXWindow* FXMatrix::childAtRowCol(FXint r,FXint c) const {
  if(options&MATRIX_BY_COLUMNS){
    return (0<=c && c<num) ? childAtIndex(num*r+c) : NULL;
    }
  else{
    return (0<=r && r<num) ? childAtIndex(r+num*c) : NULL;
    }
  }


// Get child's row
FXint FXMatrix::rowOfChild(const FXWindow* child) const {
  register FXint i=indexOfChild(child);
  return (options&MATRIX_BY_COLUMNS) ? i/num : i%num;
  }


// Get child's column
FXint FXMatrix::colOfChild(const FXWindow* child) const {
  register FXint i=indexOfChild(child);
  return (options&MATRIX_BY_COLUMNS) ? i%num : i/num;
  }


// Focus moved up
long FXMatrix::onFocusUp(FXObject*,FXSelector,void* ptr){
  register FXWindow *child;
  register FXint r,c;
  if(getFocus()){
    r=rowOfChild(getFocus());
    c=colOfChild(getFocus());
    while((child=childAtRowCol(--r,c))!=NULL){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
        }
      }
    }
  else{
    child=getLast();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_UP,0),ptr)) return 1;
        }
      child=child->getPrev();
      }
    }
  return 0;
  }


// Focus moved down
long FXMatrix::onFocusDown(FXObject*,FXSelector,void* ptr){
  register FXWindow *child;
  register FXint r,c;
  if(getFocus()){
    r=rowOfChild(getFocus());
    c=colOfChild(getFocus());
    while((child=childAtRowCol(++r,c))!=NULL){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
        }
      }
    }
  else{
    child=getFirst();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_DOWN,0),ptr)) return 1;
        }
      child=child->getNext();
      }
    }
  return 0;
  }


// Focus moved to left
long FXMatrix::onFocusLeft(FXObject*,FXSelector,void* ptr){
  register FXWindow *child;
  register FXint r,c;
  if(getFocus()){
    r=rowOfChild(getFocus());
    c=colOfChild(getFocus());
    while((child=childAtRowCol(r,--c))!=NULL){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
        }
      }
    }
  else{
    child=getLast();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_LEFT,0),ptr)) return 1;
        }
      child=child->getPrev();
      }
    }
  return 0;
  }


// Focus moved to right
long FXMatrix::onFocusRight(FXObject*,FXSelector,void* ptr){
  register FXWindow *child;
  register FXint r,c;
  if(getFocus()){
    r=rowOfChild(getFocus());
    c=colOfChild(getFocus());
    while((child=childAtRowCol(r,++c))!=NULL){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
        }
      }
    }
  else{
    child=getFirst();
    while(child){
      if(child->shown()){
        if(child->handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr)) return 1;
        if(child->handle(this,FXSEL(SEL_FOCUS_RIGHT,0),ptr)) return 1;
        }
      child=child->getNext();
      }
    }
  return 0;
  }


// Set number of rows (if possible)
void FXMatrix::setNumRows(FXint nr){
  if(nr<1 || nr>=MAXNUM){ fxerror("%s::setNumRows: bad number of rows specified.\n",getClassName()); }
  if(!(options&MATRIX_BY_COLUMNS) && num!=nr){
    num=nr;
    recalc();
    }
  }


// Get number of rows
FXint FXMatrix::getNumRows() const {
  return (num && (options&MATRIX_BY_COLUMNS)) ? (numChildren()+num-1)/num : num;
  }


// Set number of columns (if possible)
void FXMatrix::setNumColumns(FXint nc){
  if(nc<1 || nc>=MAXNUM){ fxerror("%s::setNumColumns: bad number of columns specified.\n",getClassName()); }
  if((options&MATRIX_BY_COLUMNS) && num!=nc){
    num=nc;
    recalc();
    }
  }


// Get number of columns
FXint FXMatrix::getNumColumns() const {
  return (num && !(options&MATRIX_BY_COLUMNS)) ? (numChildren()+num-1)/num : num;
  }


// Compute minimum width based on child layout hints
FXint FXMatrix::getDefaultWidth(){
  register FXint c,n,w,nzcol=0,wmax=0,mw=0;
  register FXWindow *child;
  register FXuint hints;
  FXint colw[MAXNUM];
  for(c=0; c<MAXNUM; c++) colw[c]=0;
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  for(child=getFirst(),n=0; child; child=child->getNext(),n++){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      c=(options&MATRIX_BY_COLUMNS)?n%num:n/num;
      FXASSERT(c<MAXNUM);
      if(w>colw[c]){
        if(colw[c]==0) nzcol++;                             // Count non-zero columns
        wmax+=w-colw[c];
        colw[c]=w;
        }
      }
    }
  if(nzcol>1) wmax+=(nzcol-1)*hspacing;
  return padleft+padright+wmax+(border<<1);
  }



// Compute minimum height based on child layout hints
FXint FXMatrix::getDefaultHeight(){
  register FXint r,n,h,nzrow=0,hmax=0,mh=0;
  register FXWindow *child;
  register FXuint hints;
  FXint rowh[MAXNUM];
  for(r=0; r<MAXNUM; r++) rowh[r]=0;
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();
  for(child=getFirst(),n=0; child; child=child->getNext(),n++){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      r=(options&MATRIX_BY_COLUMNS)?n/num:n%num;
      FXASSERT(r<MAXNUM);
      if(h>rowh[r]){
        if(rowh[r]==0) nzrow++;                             // Count non-zero rows
        hmax+=h-rowh[r];
        rowh[r]=h;
        }
      }
    }
  if(nzrow>1) hmax+=(nzrow-1)*vspacing;
  return padtop+padbottom+hmax+(border<<1);
  }



// Recalculate layout
void FXMatrix::layout(){
  FXint ncol,nrow,nzcol,nzrow,r,c,x,y,w,h,n,e,t;
  FXint rowh[MAXNUM],colw[MAXNUM];
  FXbool srow[MAXNUM],scol[MAXNUM];
  FXint left,right,top,bottom,cw,rh;
  FXint mw=0,mh=0;
  FXint hremain,vremain;
  FXint hsumexpand,hnumexpand;
  FXint vsumexpand,vnumexpand;
  FXWindow *child;
  FXuint hints;

  // Placement rectangle; right/bottom non-inclusive
  left=border+padleft;
  right=width-border-padright;
  top=border+padtop;
  bottom=height-border-padbottom;
  hremain=right-left;
  vremain=bottom-top;

  // Non-zero rows/columns
  nzrow=0;
  nzcol=0;

  // Clear column/row sizes
  for(n=0; n<MAXNUM; n++){
    colw[n]=rowh[n]=0;          // Columns may be 0 size
    srow[n]=scol[n]=1;
    }

  // Get maximum child size
  if(options&PACK_UNIFORM_WIDTH) mw=maxChildWidth();
  if(options&PACK_UNIFORM_HEIGHT) mh=maxChildHeight();

  // Find expandable columns and rows
  for(child=getFirst(),n=0; child; child=child->getNext(),n++){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(options&MATRIX_BY_COLUMNS){r=n/num;c=n%num;}else{r=n%num;c=n/num;}
      FXASSERT(r<MAXNUM && c<MAXNUM);
      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();
      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();
      FXASSERT(w>=0);
      FXASSERT(h>=0);
      if(w>colw[c]){ if(colw[c]==0) nzcol++; colw[c]=w; }
      if(h>rowh[r]){ if(rowh[r]==0) nzrow++; rowh[r]=h; }
      if(!(hints&LAYOUT_FILL_COLUMN)) scol[c]=0;
      if(!(hints&LAYOUT_FILL_ROW)) srow[r]=0;
      }
    }

  // Get number of rows and columns
  if(options&MATRIX_BY_COLUMNS){
    ncol=num;
    nrow=(n+num-1)/num;
    }
  else{
    ncol=(n+num-1)/num;
    nrow=num;
    }

  // Find stretch in columns
  for(c=hsumexpand=hnumexpand=0; c<ncol; c++){
    if(colw[c]){
      if(scol[c]){
        hsumexpand+=colw[c];
        hnumexpand++;
        }
      else{
        hremain-=colw[c];
        }
      }
    }

  // Find stretch in rows
  for(r=vsumexpand=vnumexpand=0; r<nrow; r++){
    if(rowh[r]){
      if(srow[r]){
        vsumexpand+=rowh[r];
        vnumexpand++;
        }
      else{
        vremain-=rowh[r];
        }
      }
    }

  // Substract spacing for non-zero rows/columns
  if(nzcol>1) hremain-=(nzcol-1)*hspacing;
  if(nzrow>1) vremain-=(nzrow-1)*vspacing;

  // Disburse space horizontally
  for(c=e=0,x=border+padleft; c<ncol; c++){
    w=colw[c];
    colw[c]=x;
    if(w){
      if(scol[c]){
        if(hsumexpand>0){                         // Divide proportionally
          t=w*hremain;
          w=t/hsumexpand;
          e+=t%hsumexpand;
          if(e>=hsumexpand){w++;e-=hsumexpand;}
          }
        else{                                     // Divide equally
          FXASSERT(hnumexpand>0);
          w=hremain/hnumexpand;
          e+=hremain%hnumexpand;
          if(e>=hnumexpand){w++;e-=hnumexpand;}
          }
        }
      x+=w+hspacing;
      }
    }
  colw[ncol]=x;

  // Disburse space vertically
  for(r=e=0,y=border+padtop; r<nrow; r++){
    h=rowh[r];
    rowh[r]=y;
    if(h){
      if(srow[r]){
        if(vsumexpand>0){                         // Divide proportionally
          t=h*vremain;
          h=t/vsumexpand;
          e+=t%vsumexpand;
          if(e>=vsumexpand){h++;e-=vsumexpand;}
          }
        else{                                     // Divide equally
          FXASSERT(vnumexpand>0);
          h=vremain/vnumexpand;
          e+=vremain%vnumexpand;
          if(e>=vnumexpand){h++;e-=vnumexpand;}
          }
        }
      y+=h+vspacing;
      }
    }
  rowh[nrow]=y;

  // Do the layout
  for(child=getFirst(),n=0; child; child=child->getNext(),n++){
    if(child->shown()){
      hints=child->getLayoutHints();
      if(options&MATRIX_BY_COLUMNS){r=n/num;c=n%num;}else{r=n%num;c=n/num;}
      cw=colw[c+1]-colw[c]-hspacing;
      rh=rowh[r+1]-rowh[r]-vspacing;

      if(hints&LAYOUT_FIX_WIDTH) w=child->getWidth();
      else if(hints&LAYOUT_FILL_X) w=cw;
      else if(options&PACK_UNIFORM_WIDTH) w=mw;
      else w=child->getDefaultWidth();

      if(hints&LAYOUT_CENTER_X) x=colw[c]+(cw-w)/2;
      else if(hints&LAYOUT_RIGHT) x=colw[c]+cw-w;
      else x=colw[c];

      if(hints&LAYOUT_FIX_HEIGHT) h=child->getHeight();
      else if(hints&LAYOUT_FILL_Y) h=rh;
      else if(options&PACK_UNIFORM_HEIGHT) h=mh;
      else h=child->getDefaultHeight();

      if(hints&LAYOUT_CENTER_Y) y=rowh[r]+(rh-h)/2;
      else if(hints&LAYOUT_BOTTOM) y=rowh[r]+rh-h;
      else y=rowh[r];

      child->position(x,y,w,h);
      }
    }
  flags&=~FLAG_DIRTY;
  }


// Change matrix style
void FXMatrix::setMatrixStyle(FXuint style){
  FXuint opts=(options&~MATRIX_BY_COLUMNS) | (style&MATRIX_BY_COLUMNS);
  if(opts!=options){
    options=opts;
    recalc();
    update();
    }
  }


// Return matrix style
FXuint FXMatrix::getMatrixStyle() const {
  return options&MATRIX_BY_COLUMNS;
  }

}

