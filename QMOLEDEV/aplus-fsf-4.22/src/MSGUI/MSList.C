///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <MSGUI/MSList.H>
#include <MSTypes/MSStandardOps.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSGenericVector.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSGenericVector<MSPixmap *>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSGenericVector<MSPixmap *>)
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSGenericVector<MSPixmap *>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSGenericVector<MSPixmap *>;
#endif


#endif //MSTK_MANUAL_INSTANTIATION

MSList::MSList(MSWidget *owner_,const char *title_) :
MSRowColumnView(owner_,title_) 
{ init(); }

MSList::MSList(MSWidget *owner_,const MSStringVector& title_) :
MSRowColumnView(owner_,title_) 
{ init(); }

MSList::~MSList(void)
{
  if (_pixmapGC!=0) XFreeGC(display(),_pixmapGC);
}

void MSList::init(void)
{
  _dynamic=MSTrue;
  _highlightThickness=0;
  _shadowThickness=2;
  _shadowStyle=MSSunken;
  _acceptTab=MSFalse;
  _rowSpacing=2;
  _spacing=2;
  _rowBg=foreground();
  _selectedRowForeground=background();
  _pixmapGC=0;
  _maxLength=0;
  panner()->shadowThickness(shadowThickness());
  panner()->shadowStyle(shadowStyle());
}

void MSList::update(const MSIndexVector& index_)
{
  if (frozen()==MSFalse)
   {
     if (index_.length()==0) 
      {
        shapeUpdate();
	redrawImmediately();
      }
     else 
      { 
	if (numRows()>vsb()->max()) appendUpdate(index_);
	for (unsigned i=0;i<index_.length();i++) cycleRow(index_(i));           
      }
   }
}

void MSList::appendUpdate(const MSIndexVector &index_)
{
  for (unsigned i=0;i<index_.length();i++)
   {
     unsigned len=rowLength(index_(i));
     _maxLength=(_maxLength>len)?_maxLength:len;
   }
  adjustNumVisible();
}

void MSList::shapeUpdate(void) 
{
  updateInternalState();
  adjustNumVisible();
}

void MSList::updateInternalState(void)
{
  adjustSelection();
  calculateMaxLength();
}

void MSList::adjustSelection(void)
{
  if (selectedRow()>=0&&selectedRow()>=numRows()) _selectedRow=numRows()-1;
  if (numRows()<vsb()->max()&&selectionMode()==MSMultiple)
   {
     unsigned i,index;
     for (i=numRows();i<vsb()->max();i++)
      {
	if ((index=selectionVector().indexOf(i))!=selectionVector().length())
	 {
	   _selectionVector.removeAt(index);
	   lastBlock(-1);
	 }
      }
   }
  // If we are in multiple selection mode, always make sure the current
  // selected row is in the selection vector.
  if (selectionMode()==MSMultiple&&
      selectedRow()!=-1&&
      selectionVector().indexOf(selectedRow())==selectionVector().length())
   {
     _selectionVector.append(selectedRow());
     _selectionVector.sortUp();
   }     
}

void MSList::calculateMaxLength(void)
{
  unsigned nRows=numRows();
  _maxLength=0;
  for (unsigned i=0;i<nRows;i++)
   {
     unsigned len=rowLength(i);
     _maxLength=(_maxLength>len)?_maxLength:len;
   }
}

void MSList::defaultNumVisible(void)
{
  if ((sizeState()&RowsValid)!=RowsValid)
   {
     _rows=5; 
     _rows=(rows()<=numRows())?rows():numRows(); 
     _rows=(rows()<=0)?1:rows(); 
   }
  if ((sizeState()&ColsValid)!=ColsValid)
   {
     int ml=actualNumColumns();
     _columns=ml; 
     _columns=(columns()<=ml)?columns():ml;
     _columns=(columns()<=0)?1:columns(); 
   }
}

void MSList::computeSize(void)
{
  panner()->shadowThickness(shadowThickness());
  if (editor()!=0&&vsb()!=0&&hsb()!=0&&label()!=0)
   {
     int offset=highlightThickness()<<1;
     int poffset=((panner()->highlightThickness()+panner()->shadowThickness())<<1);
     int dWidth=drawWidth();
     int dHeight=drawHeight();
     int w=dWidth+poffset+offset; 
     int h=dHeight+headingsHeight()+poffset+offset;
     if (label()->mapped()==MSTrue) h+=label()->height();
     if (dynamic()==MSTrue)
      {
        if (isHsbEnabled()==MSTrue&&columns()<actualNumColumns()) h+=(hsb()->height()+spacing());
        if (isVsbEnabled()==MSTrue&&rows()<numRows()) w+=(vsb()->width()+spacing());
      }
     else
      {
        if (isHsbEnabled()==MSTrue) h+=(hsb()->height()+spacing());
        if (isVsbEnabled()==MSTrue) w+=(vsb()->width()+spacing());
      }
     resize(w,h);
   }
}

void MSList::placement(void)
{
  if (editor()!=0&&vsb()!=0&&hsb()!=0&&label()!=0) 
   {
     if (editor()->mapped()==MSTrue) unmapEditor();
     
     int offset=highlightThickness();
     int offset2=offset<<1;
     label()->moveTo(offset,offset);
     label()->width(width()-offset2);
     editor()->height(rowHeight());
     
     int y=offset;
     if (label()->mapped()==MSTrue) y+=label()->height();
     panner()->moveTo(offset,y);
     adjustNumVisible();
   }
}

void MSList::adjustNumVisible(void)
{
  if ((sizeState()&AdjustSize)==AdjustSize)
   {
     int nCols=actualNumColumns();
     if (doubleByte()==MSTrue) nCols=nCols/2;
     int nRows=numRows();
     int margin=(highlightThickness()+panner()->shadowThickness()+panner()->highlightThickness())*2;
     int availableWidth=width()-margin;
     int availableHeight=height()-margin-headingsHeight()-(label()->mapped()==MSTrue?label()->height():0);
     int requiredWidth=nCols*charWidth()+columnSpacing()*2;
     int requiredHeight=nRows*rowHeight();
     if (dynamic()==MSTrue)
      {
        if (isHsbEnabled()==MSTrue)
         {
           if (availableWidth>=requiredWidth) hsb()->hide();
           else
            {
              hsb()->show();
              availableHeight-=(hsb()->height()+spacing());
            }
         }
        else hsb()->hide();
        if (isVsbEnabled()==MSTrue)
         {
           if (availableHeight>=requiredHeight) vsb()->hide();
           else
            {
              vsb()->show(); 
              if (isHsbEnabled()==MSTrue&&hsb()->mapped()==MSFalse)
               {
                 availableWidth-=(vsb()->width()+spacing());
                 if (availableWidth<requiredWidth) hsb()->show();
               }
            }
         }
        else vsb()->hide();
      }
     else
      {
        if (isHsbEnabled()==MSTrue) availableHeight-=(hsb()->height()+spacing());
        if (isVsbEnabled()==MSTrue) availableWidth-=(vsb()->width()+spacing());
        if (isVsbEnabled()==MSTrue)
         {
           if (availableHeight>=requiredHeight) vsb()->hide();
           else vsb()->show();
         }
        else vsb()->hide();
        if (isHsbEnabled()==MSTrue)
         {
           if (availableWidth>=requiredWidth) hsb()->hide();
           else hsb()->show();
         }
        else hsb()->hide();
      }
     int offset=highlightThickness();
     int offset2=offset<<1;
     int pH=height()-offset2-(label()->mapped()==MSTrue?label()->height():0);
     int pW=width()-offset2;
     if (isVsbEnabled()==MSTrue)
      {
        if (dynamic()==MSFalse||vsb()->mapped()==MSTrue) pW-=(vsb()->width()+spacing());
      }
     if (isHsbEnabled()==MSTrue)
      {
        if (dynamic()==MSFalse||hsb()->mapped()==MSTrue) pH-=(hsb()->height()+spacing());
      }
     panner()->resize(pW,pH);

     offset=panner()->highlightThickness()+panner()->shadowThickness();
     offset2=offset<<1;
     XRectangle clipRect[1];
     clipRect[0].x=offset;
     clipRect[0].y=offset;
     clipRect[0].width=panner()->width()-offset2;
     clipRect[0].height=panner()->height()-offset2;
     XSetClipRectangles(display(),textGC(),0,0,&clipRect[0],1,Unsorted);

     _rows=computeNumVisibleRows();
     _rows=(rows()<0)?0:rows(); 
     _columns=computeNumVisibleColumns();  
     _columns=(columns()<0)?0:columns();

     adjustFirstRow();
     adjustFirstColumn();
     updateHsb();
     updateVsb();
   }
}

void MSList::drawRow(int row_) 
{ if (row_<numRows()&&inRowRange(row_)==MSTrue) drawRows(panner()->window(),row_,row_); }
void MSList::drawRows(int rs_,int re_) 
{ drawRows(panner()->window(),rs_,re_); }
void MSList::drawRows(Window window_,int rs_,int re_)
{
  if (numRows()>0&&mapped()!=MSFalse&&frozen()!=MSTrue) 
   {
     rs_=(rs_>=firstRow())?rs_:firstRow();  
     re_=(re_<firstRow()+rows())?re_:firstRow()+rows()-1;
     
     int nRows=numRows();
     MSString buffer;
     for (int i=rs_;i<=re_&&i<nRows;i++) 
      {
	const char *pString=formatOutput(buffer.removeAll(),i);
	drawRow(window_,i,pString,buffer.length());
      }
     moveSelection(selectedRow());
   }
}
void MSList::drawRow(Window window_,int row_,const char *pString_,int slen_)
{
  MSBoolean rowselected,selectOutline;
  unsigned long fg,bg;
  Font fid;
  const MSPixmap *pmap;

  if (selected(row_)==MSTrue)
   {
     rowselected=MSTrue;
     fg=selectedRowForegroundColor(row_);
     bg=selectedRowBackgroundColor(row_);
   }
  else
   {
     rowselected=MSFalse;
     fg=rowForeground(row_);
     bg=rowBackground(row_);
   }
  if (row_==selectedRow()) selectOutline=highlighted();
  else selectOutline=MSFalse;
  fid=rowFont(row_);
  pmap=rowPixmap(row_);
  drawActualRow(window_,row_,pString_,slen_,pmap,fg,bg,fid,rowselected,selectOutline);
}

void MSList::drawSelectedRow(Window window_,int row_) 
{ drawSelectedRow(window_,row_,MSTrue); }
void MSList::drawSelectedRow(int row_,MSBoolean selected_) 
{ drawSelectedRow(panner()->window(),row_,selected_); }


void MSList::drawSelectOutline(Window window_,int row_,MSBoolean select_)
{
  if (row_>=firstRow()&&row_<firstRow()+rows())
   {
     int offset=panner()->shadowThickness()+panner()->highlightThickness();
     int yy=computeYCoord(row_);
     int ww=panner()->width()-(offset*2);
     int hrs=rowSpacing()/2;
     int hcs=columnSpacing()/2;
     XSetForeground(display(),textGC(),rowBackground(row_));
     XDrawRectangle(display(),window_,textGC(),offset+hrs,yy+hcs,
		    ww-rowSpacing()-hrs,rowHeight()-columnSpacing()-hcs);
     if (select_==True) XSetForeground(display(),textGC(),selectedRowBackgroundColor(row_));
     XDrawRectangle(display(),window_,textGC(),offset,yy,ww-hrs,rowHeight()-hcs);
   }
}


void MSList::adjustFirstColumn(void)
{
  int ml=actualNumColumns();
  if (firstColumn()!=0&&firstColumn()+columns()>=ml)
   {
     int oldFirstColumn=firstColumn();
     _firstColumn=(ml>columns())?(ml-columns()):0;
     if(oldFirstColumn!=firstColumn()) firstColumnChangeNotify();
   }
}

unsigned MSList::rowLength(unsigned) const
{ return 0; }

int MSList::drawWidth(void)
{ return maxPixmapWidth()+(columns()*charWidth())+2*columnSpacing(); }

int MSList::computeNumVisibleColumns(void)
{
  int offset=panner()->highlightThickness()+panner()->shadowThickness();
  int deltaWidth=panner()->width()-2*offset-2*columnSpacing();
  return (int)(floor((double)deltaWidth/(double)charWidth()));
}

// subclasses should override this method for incremental search capability
void MSList::incrementalSearch(unsigned) {}
void MSList::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *pString_)
{
  MSKeyPress keyTranslation(keysym_,state_);
  if (sensitive()==MSTrue&&hasModel()==MSTrue)
   {
     if (isProtected()==MSFalse) MSRowColumnView::keyPress(pEvent_,keysym_,state_,pString_);
     else
      {
	int slen=(pString_!=0)?strlen(pString_):0;
	if (slen==0&&
	    (keysym_==XK_Control_L||keysym_==XK_Control_R||
	     keysym_==XK_Meta_L||keysym_==XK_Meta_R||
	     keysym_==XK_Shift_L||keysym_==XK_Shift_R)) return;
	if (pEvent_->xkey.state&ControlMask&&keysym_==XK_s&&isearchString().length()>0)
	 {
	   incrementalSearch(1);
	 }
	else if (keysym_==XK_Delete||keysym_==XK_BackSpace)
	 {
	   isearchString().drop(-1);
	   isearchVector().drop(-1);
	   if (isearchVector().length()>0) selectedRow(isearchVector().lastElement());
	 }
	else if (keysym_!=XK_Return&&slen>0)
	 {
	   isearchString()<<pString_;
	   incrementalSearch();
	 }
	else
	 {
	   isearchString()="";
	   isearchVector().removeAll();  
	   if (keysym_==XK_Return) doubleClick();
	   else 
	    {
	      if( keyTranslate(keyTranslation)!=MSTrue);      
	      else server()->bell();
	    }
	 }
      }
   }
}

void MSList::defaultButtonBehavior(const XEvent *pEvent_)
{
  isearchString()="";
  isearchVector().removeAll();

  if (pEvent_->xbutton.subwindow==panner()->window())
   {
     if (traverseFocus(this)==MSTrue||acceptFocus()==MSFalse)
      {
	if (numRows()>0)
	 {
	   if (editorActivate()==MSTrue)
	    {
	      // recompute the the (x,y) of the event relative to the 
	      // position of the panner
	      XEvent *pEvent=(XEvent *)pEvent_;
	      pEvent->xbutton.y-=panner()->y_origin();
	      pEvent->xbutton.x-=panner()->x_origin();

	      if (pEvent->xbutton.y<headingsHeight())
	       {
		 headingAreaSelection(pEvent);
	       }
	      else
	       {
		 dataAreaSelection(pEvent);
	       }
	    }
	 }
      }
   }
  else
   {
     MSWidget *pWidget=widget(pEvent_->xbutton.subwindow);
     if (pWidget!=0)
      {
	if (pWidget->sensitive()==MSTrue)
	 {
	   XEvent *pEvent=(XEvent *)pEvent_;
	   pEvent->xbutton.x-=pWidget->x_origin();
	   pEvent->xbutton.y-=pWidget->y_origin();
	   buttonPressNotify(pWidget,pEvent);
	 }
      }
   }
}

void MSList::headingAreaSelection(const XEvent *)
{}

void MSList::dataAreaSelection(const XEvent *pEvent_)
{
  int row=rowFromEvent(pEvent_);
  if (row>=0)
   {
     if (row<numRows()&&row<firstRow()+rows())
      {
        if(selectionMode()==MSToggle)
         {
           if (pEvent_->xbutton.button==Button1)
            {
              toggleRow(row);
              drawRow(row);
            }
         }
        else if (selectedRow()!=row) 
	 {
	   if (pEvent_->xbutton.button==Button1)
	    {
	      eventTime(pEvent_->xbutton.time);
	      if (selectionMode()==MSMultiple)
	       {
		 // if control key is held down
		 if (pEvent_->xbutton.state&ControlMask)
		  {
		    // Control is held down, so check if the selected item is in
		    // the selection vector, if so, track erase selection, else
		    // track selection
		    if (selectionVector().indexOf(row)!=selectionVector().length()) trackUnselection(row,0);
		    else trackSelection(row,0,MSFalse,MSFalse);
		  }
		 else
		  {
		    // if Shift key is held down
		    if (pEvent_->xbutton.state&ShiftMask) trackSelection(row,0,MSFalse,MSTrue);
		    else trackSelection(row,0,MSTrue,MSFalse);
		  } 
	       }
	      else
	       {
		 clearSelection();
		 selectedRow(row);
	       }
	    }
	   else
	    {
	      clearSelection();
	      if (selectionMode()==MSMultiple) 
	       {
		 lastBlock(row);
		 _selectionVector.append(row);
	       }
	      selectedRow(row);
	    }
	 }	
	else if (selectedRow()==row)
	 {
	   if (pEvent_->xbutton.button==Button1) 
	    {
	      if (isDoubleClick(pEvent_)==MSTrue) defaultDoubleClickBehavior(pEvent_);
	      else if (selectionMode()==MSMultiple)
	       {
		 // if control key is held down, track erase selection
		 if (pEvent_->xbutton.state&ControlMask) trackUnselection(row,0);
		 else
		  {
		    if (pEvent_->xbutton.state&ShiftMask) trackSelection(row,0,MSFalse,MSTrue);
		    else trackSelection(row,0,MSTrue,MSFalse);
		  }
	       }
	      else
	       {
		 if (pEvent_->xbutton.state&ControlMask)
		  {
		    selectedRow(-1);
		    rowColumnSelection();
		  }
	       }
	      return;
	    }
	   else if (pEvent_->xbutton.button!=Button1)
	    {
	      // Let's clear the selection Vector first
	      if (selectionMode()==MSMultiple) 
	       {
		 if (selectionVector().length()>1)
		  {
		    clearSelection();
		    lastBlock(row);
		    _selectionVector.append(row);
		    drawSelectedRow(panner()->window(),row);
		    rowColumnSelection();
		  }
	       }
	    }
	 }
	if (pEvent_->xbutton.button==Button1) defaultButton1Behavior(pEvent_);
	else if (pEvent_->xbutton.button==Button2) defaultButton2Behavior(pEvent_);
	else if (pEvent_->xbutton.button==Button3) defaultButton3Behavior(pEvent_);
      }
   }
}

void MSList::defaultButton2Behavior(const XEvent *pEvent_) 
{
  if (rowDragDrop()==MSTrue) dragRow(pEvent_);
  else if(selectionMode()!=MSToggle) startEditing(pEvent_);
}

int MSList::dragRowFromEvent(const XEvent* pEvent_)
{
  int row;
  if(selectionMode()==MSToggle)
   {
     row=rowFromEvent(pEvent_);
     if (row<0 || row>=numRows() || row>=firstRow()+rows()) return -1;
   }
  else row=selectedRow();
  return row;
}

void MSList::moveRow(int from_,int to_)
{
  if(selectionMode()==MSToggle)
   {
     toggleModeMoveRow(from_,to_);
     return;
   }
  unsigned len=pixmapVector().length();
  if (from_<len&&to_<len)
   {
     MSPixmap *element=pixmapVector().elementAt(from_);
     pixmapVector().removeAt(from_);
     if (from_<to_)
      {
        if (to_>=pixmapVector().length()) pixmapVector().append(element);
        else pixmapVector().insertAt(to_,element);
      }
     else
      {
        pixmapVector().insertAt(to_,element);
      }
   }
  _selectedRow=to_;
  if (selectionMode()==MSMultiple) (_selectionVector.removeAll())<<to_;
}

void MSList::redrawImmediately(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     redrawPixmap()->lock();
     XFillRectangle(display(),redrawPixmap()->pixmap(),backgroundShadowGC(),
		    0,0,panner()->width(),panner()->height());
     drawRows(redrawPixmap()->pixmap(),firstRow(),firstRow()+rows()-1);
     int ht=panner()->highlightThickness();
     int offset=highlightThickness();
     MSRect aRect(ht,ht,panner()->width()-2*ht,panner()->height()-2*ht);
     drawBevel(redrawPixmap()->pixmap(),aRect,shadowStyle(),panner()->shadowThickness()); 
     XCopyArea(display(),redrawPixmap()->pixmap(),panner()->window(),
	       backgroundShadowGC(),
	       0,0,panner()->width(),panner()->height(),0,0);
     updateScrollBars();
     XFlush(display());
     redrawPixmap()->unlock();
   }
}

void MSList::drawSelectedRow(Window window_,int row_,MSBoolean select_)
{
  if (row_>=firstRow()&&(selectedRow()==-1||selectedRow()<numRows())&&
      row_<numRows()&&row_<firstRow()+rows())
   {
     MSString buffer;
     const char *pString=formatOutput(buffer,row_);
     int r=selectedRow();
     if (select_==MSFalse&&row_==selectedRow()) _selectedRow=-1;
     drawRow(window_,row_,pString,buffer.length());
     _selectedRow=r;
   }
}

void MSList::focusIn(void) 
{ 
  highlight();
  drawSelectOutline(panner()->window(),selectedRow(),highlighted());
  if (editor()->mapped()==MSTrue) focusInNotify(editor()); 
}

void MSList::focusOut(void) 
{ 
  unHighlight(); 
  drawSelectOutline(panner()->window(),selectedRow(),highlighted());
  if (editor()->mapped()==MSTrue) focusOutNotify(editor()); 
}

MSBoolean MSList::loseFocus(void) 
{ 
  if (editor()->mapped()==MSTrue) editorActivate(); 
  if (editor()->mapped()==MSTrue) return MSFalse;
  else 
   { 
     unHighlight();
     drawSelectOutline(panner()->window(),selectedRow(),highlighted());
   }
  return MSTrue; 
}

void MSList::doubleClick(void)
{
  if (editor()->mapped()==MSFalse)
   {
    if (selectedRow()>=0&&selectedRow()<numRows())
     { activateCallback(MSWidgetCallback::doubleclick); }
   }
}
 
void MSList::drawCycle(MSColorCycle *cycle_)
{
   // cycleColor mode is not supported in MSList
  int row=cycle_->row();
  unsigned long fg=cycle_->color(cycle_->count());
   
  if (row==-1)
   {
     int first=firstRow();
     int last=lastRow();
     last=(last>numRows()?numRows():last);
     for (int i=first;i<=last;i++) drawCycle(i,fg);
   }
  else if (row<numRows()&&inRowRange(row)==MSTrue) drawCycle(row,fg);
}

void MSList::drawCycle(int row_,unsigned long fg_)
{
  MSString buffer;
  const char *pString=formatOutput(buffer,row_);
  int len=buffer.length();
  MSBoolean rowselected,selectOutline;
  unsigned long bg;
  Font fid;
  const MSPixmap *pmap;
  if (selected(row_)==MSTrue)
   {
     rowselected=MSTrue;
     bg=selectedRowBackgroundColor(row_);
   }
  else
   {
     rowselected=MSFalse;
     bg=rowBackground(row_);
   }
  if (row_==selectedRow()) selectOutline=highlighted();
  else selectOutline=MSFalse;
  fid=rowFont(row_);
  pmap=rowPixmap(row_);
  drawActualRow(panner()->window(),row_,pString,len,pmap,fg_,bg,fid,
                rowselected,selectOutline);
}

void MSList::drawSelected(int row_)
{
  MSString buffer;
  const char *pString=formatOutput(buffer,row_);
  int len=buffer.length();
  const MSPixmap *pmap;
  unsigned long fg=selectedRowForegroundColor(row_);
  unsigned long bg=selectedRowBackgroundColor(row_);
  Font fid=rowFont(row_);
  pmap=rowPixmap(row_);
  drawActualRow(panner()->window(),row_,pString,len,pmap,fg,bg,fid,MSTrue,MSFalse);
}

void MSList::undrawSelected(int row_)
{
  MSString buffer;
  const char *pString=formatOutput(buffer,row_);
  int len=buffer.length();
  unsigned long fg=rowForeground(row_);
  unsigned long bg=rowBackground(row_);
  Font fid=rowFont(row_);
  const MSPixmap *pmap=rowPixmap(row_);
  drawActualRow(panner()->window(),row_,pString,len,pmap,fg,bg,fid,MSFalse,MSFalse);
}

void MSList::trackSelection(int startRow_,int,MSBoolean clear_,MSBoolean shifted_)
{
  Window root,child;
  int rx=0,ry=0,ix=0,iy=0;
  unsigned keys=0;
  int i,row,fr,lr;
  unsigned int index;
  int lastSelectedRow=startRow_;
  MSIndexVector oldsel=selectionVector();
  int previousSelectedRow=selectedRow();

  if (clear_==MSTrue) clearSelection();
  moveSelection(-1);
  if (shifted_==MSTrue&&lastBlock()!=-1&&previousSelectedRow!=-1)
   {
     MSBoolean inside;
     if (previousSelectedRow>lastBlock()) inside=(startRow_>=lastBlock()&&startRow_<=previousSelectedRow)?MSTrue:MSFalse;
     else inside=(startRow_<=lastBlock()&&startRow_>=previousSelectedRow)?MSTrue:MSFalse;
     
     if (inside==MSTrue)
      {
	if (startRow_<previousSelectedRow) unfillSelection(startRow_+1,previousSelectedRow);
	else unfillSelection(previousSelectedRow,startRow_-1);
      }
     else
      {
	if (startRow_<lastBlock()) 
	 {
	   fillSelection(startRow_+1,lastBlock()-1);
	   if (previousSelectedRow>lastBlock()) unfillSelection(lastBlock()+1,previousSelectedRow);
	 }
	else
	 { 
	   fillSelection(lastBlock()+1,startRow_-1);
	   if (previousSelectedRow<lastBlock()) unfillSelection(previousSelectedRow,lastBlock()-1);
	 }
      }
     
     if (previousSelectedRow>lastBlock())
      {
	if ((index=selectionVector().indexOf(lastBlock()+1))!=selectionVector().length())
	   _selectionVector.removeAt(index,previousSelectedRow-lastBlock());
      }
     else
      {
	if ((index=selectionVector().indexOf(previousSelectedRow))!=selectionVector().length())
	   _selectionVector.removeAt(index,lastBlock()-previousSelectedRow);
      }
     startRow_=lastBlock();
     drawSelected(lastSelectedRow); 
   }
  else 
   {
     lastBlock(startRow_);
     drawSelected(lastSelectedRow);
   }
  int startSelect,endSelect;
  MSIndexVector drawnVector; //Vector to keep track of which line is already drawn selected
  if (lastSelectedRow>lastBlock())
   {
     startSelect=lastBlock();
     endSelect=lastSelectedRow;
   }
  else
   {
     startSelect=lastSelectedRow;
     endSelect=lastBlock();
   }
  fr=firstRow();
  lr=lastRow();
  lr=(lr>numRows()?numRows():lr);
  for (i=fr;i<=lr;i++)
   {
     if (selected(i)==MSTrue||(i>=startSelect&&i<=endSelect)) drawnVector<<i;
   }

  int sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while (keys&Button1Mask)
   {
     if (sameScreen==True)
      {
	iy-=panner()->y_origin();
	if (iy<headingsHeight())
	 {
	   if ((row=firstRow())>0)
	    {
	      // To prevent flashing
 	      if (row>startRow_) 
	       {
		 if (selected(row)==MSFalse)
		  {
		    if ((index=drawnVector.indexOf(row))!=drawnVector.length())
		     {
		       undrawSelected(row);
		       drawnVector.removeAt(index);
		     }
		  }
	       }
	      scrollDown(1);
	      row=firstRow();
	    }
	 }
	else if (iy>=panner()->height())
	 {
	   if ((row=lastRow())<numRows()-1)
	    {
	      if (row<startRow_) 
	       {
		 if (selected(row)==MSFalse)
		  {
		    if ((index=drawnVector.indexOf(row))!=drawnVector.length())
		     {
		       undrawSelected(row);
		       drawnVector.removeAt(index);
		     }
		  }
	       }
	      scrollUp(1);
	      row=lastRow();
	    }
	   else row=numRows()-1;
	 }
	else
	 {
	   row=yToRow(iy-headingsHeight())+firstRow();
	   if (row>=numRows()) row=numRows()-1;
	   else if (inRowRange(row)==MSFalse) row--;
	 }
	if (row!=lastSelectedRow)
	 {
	   lastSelectedRow=row;
	   if (row>startRow_)
	    {
	      startSelect=startRow_;
	      endSelect=row;
	    }
	   else 
	    {
	      startSelect=row;
	      endSelect=startRow_;
	    }
	   MSIndexVector dv;
	   fr=firstRow();
	   lr=lastRow();
	   lr=(lr>numRows()?numRows():lr);
	   for (i=fr;i<=lr;i++)
	    {
	      // Not in the selection
	      if (i<startSelect||i>endSelect)
	       {
		 // If it is selected but not in our Drawn Vector draw it.
		 if (selected(i)==MSTrue)
		  {
		    dv<<i;
		    if (drawnVector.indexOf(i)==drawnVector.length()) drawSelected(i);
		  }
		 else
		  {
		    if (drawnVector.indexOf(i)!=drawnVector.length()) undrawSelected(i);
		  }
	       }
	      else
	       {
		 dv<<i;
		 if (drawnVector.indexOf(i)==drawnVector.length()) drawSelected(i);
	       }
	    }
	   drawnVector=dv;
	 }
      }
     sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);	      
   }
  // Merge what we got with the original Selection Vector
  MSIndexVector sv=selectionVector();
  for (i=startSelect;i<=endSelect;i++)
   {
     if (selected(i)==MSFalse) sv<<i;
   }
  sv.sortUp();
  _selectionVector=sv;

  if (lastSelectedRow!=previousSelectedRow) selectedRow(lastSelectedRow);
  else
   {
     if (oldsel.length()!=selectionVector().length()|| oldsel!=selectionVector()) 
      {
	moveSelection(lastSelectedRow);
	rowColumnSelection();
      }
     else moveSelection(lastSelectedRow);
   }
}

void MSList::trackUnselection(int startRow_,int)
{
  Window root,child;
  int rx=0,ry=0,ix=0,iy=0;
  unsigned keys=0;
  int i,row,fr,lr;
  unsigned int index;
  int lastSelectedRow=startRow_;
  int startSelect,endSelect;
  MSIndexVector undrawnVector; //Vector to keep track of which line is already drawn selected

  moveSelection(-1);
  undrawSelected(startRow_);
  startSelect=endSelect=startRow_;
  fr=firstRow();
  lr=lastRow();
  lr=(lr>numRows()?numRows():lr);
  undrawnVector<<startRow_;

  int sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while (keys&Button1Mask)
   {
     if (sameScreen==True)
      {
	iy-=panner()->y_origin();
	if (iy<headingsHeight())
	 {
	   if ((row=firstRow())>0)
	    {
	      if (row>startRow_)
	       {
		 if (selected(row)==MSTrue)
		  {
		    if ((index=undrawnVector.indexOf(row))!=undrawnVector.length())
		     {
		       drawSelected(row);
		       undrawnVector.removeAt(index);
		     }
		  }
	       }
	      scrollDown(1);
	      row=firstRow();
	      undrawSelected(row);
	      if (selected(row)==MSTrue) undrawnVector<<row;
	    }
	 }
	else if (iy>=panner()->height())
	 {
	   if ((row=lastRow())<numRows()-1)
	    {
	      if (row<startRow_)
	       {
		 if (selected(row)==MSTrue)
		  {
		    if ((index=undrawnVector.indexOf(row))!=undrawnVector.length())
		     {
		       drawSelected(row);
		       undrawnVector.removeAt(index);
		     }
		  }
	       }
	      scrollUp(1);
	      row=lastRow();
	      undrawSelected(row);
	      if (selected(row)==MSTrue) undrawnVector<<row;
	   }
	   else row=lastSelectedRow;
	 }
	else
	 {
	   row=yToRow(iy-headingsHeight())+firstRow();
	   if (row>=numRows()) row=numRows()-1;
	   else if (inRowRange(row)==MSFalse) row--;
	 }
	if (row!=lastSelectedRow)
	 {
	   lastSelectedRow=row;
	   if (row>startRow_)
	    {
	      startSelect=startRow_;
	      endSelect=row;
	    }
	   else 
	    {
	      startSelect=row;
	      endSelect=startRow_;
	    }
	   MSIndexVector dv;
	   fr=firstRow();
	   lr=lastRow();
	   lr=(lr>numRows()?numRows():lr);
	   for (i=fr;i<=lr;i++)
	    {
	      // Not in the selection
	      if (i<startSelect||i>endSelect)
	       {
		 // If it is selected but in our UnDrawn Vector draw it
		 if (selected(i)==MSTrue)
		  {
		    if (undrawnVector.indexOf(i)!=undrawnVector.length()) drawSelected(i);
		  }
	       }
	      else
	       {
		 if (selected(i)==MSTrue)
		  {
		    dv<<i;
		    if (undrawnVector.indexOf(i)==undrawnVector.length()) undrawSelected(i);
		  }
	       }
	    }
	   undrawnVector=dv;
	 }
      }
     sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);	      
   }
  // Merge what we got with the original Selection Vector
  MSIndexVector iv;
  for (i=startSelect;i<=endSelect;i++)
   {
     if ((index=selectionVector().indexOf(i))!=selectionVector().length()) iv<<index;
   }
  _selectionVector.remove(iv);
  lastBlock(-1);
  selectedRow(-1);
  rowColumnSelection();
}

void MSList::startEditing(const XEvent *pEvent_)
{
  XEvent *ep=(XEvent *)pEvent_;
  MSString string=selection();
  moveEditorToSelection(string);
  editor()->scrollIndex(firstColumn());
  if (editor()->mapped()==MSTrue)
   {
     ep->xbutton.x-=editor()->x_origin();
     ep->xbutton.y-=editor()->y_origin();
     buttonPressNotify(editor(),ep);
   }
}

void MSList::moveEditorToSelection(const MSString &aString_)
{
  if (selectedRow()<numRows()&&inRowRange(selectedRow())==MSTrue)
   {
     MSBoolean ro=isRowProtected(selectedRow());
     if (ro!=MSTrue)
      {
        int offset=panner()->shadowThickness()+panner()->highlightThickness();
	int x=panner()->x_origin()+offset;
	int y=computeYCoord(selectedRow())+panner()->y_origin();
	int w=panner()->width()-offset*2;
	if (w>panner()->width()-2*offset) w=panner()->width()-2*offset;
	
	Font fid=font();
	editor()->font(fid);
	if (aString_.length()>0) 
         {
	   MSString aString2(aString_);
           aString2.stripTrailing();
           editor()->string(aString2);
         }
        else editor()->editMode(MSTextField::InsertMode);
	editor()->resize(w,rowHeight()); 
	editor()->moveTo(x,y);
        mapEditor();
      }
   }
}

void MSList::edit(void)
{
  if (editor()->mapped()==MSFalse) 
   {
     if (selectedRow()<numRows()&&inRowRange(selectedRow())==MSTrue)
      {
	MSBoolean ro=isRowProtected(selectedRow());        
	if (ro!=MSTrue)
	 {
	   editor()->editMode(MSTextField::InsertMode);
	   MSString string=selection();
	   int len=string.length();
	   moveEditorToSelection(string);
	   if (len>columns()) editor()->scrollIndex(len-columns());
	   else editor()->scrollIndex(0);
	 }
      }
   }
}

MSAttrValueList& MSList::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("selectedRowForeground",
		       server()->colorName(selectedRowForeground()),
		       MSAttrValue::Color);
   return MSRowColumnView::get(avList_);
}

void MSList::set(MSAttrValueList& avList_)
{
  MSRowColumnView::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="selectedRowForeground")
      {
	selectedRowForeground(avList_[i].value());
	index<<i;
      }
   } 
  avList_.remove(index);
}


void MSList::cycleRow(int row_)
{ createCycle(row_); }

int MSList::computeNumVisibleRows(void)
{
  //This method calculates and returns the number of rows that can be fitted
  //into the current window size
  int offset=panner()->highlightThickness()+panner()->shadowThickness();
  int h=panner()->height()-2*offset-headingsHeight();
  int r=0;
  while (h>=rowHeight()) { r++; h-=rowHeight(); }
  return r;
}

void MSList::moveSelection(int row_)
{
  int oldRow=selectedRow();
  
  if (row_==-1)
   {
     _selectedRow=row_;
     undrawSelectedRow(panner()->window(),oldRow);
   }
  else if (row_!=selectedRow())
   {
     _selectedRow=row_;
     undrawSelectedRow(panner()->window(),oldRow);
     drawSelectedRow(panner()->window(),selectedRow());
   }
  else drawSelectedRow(panner()->window(),selectedRow());
}

void MSList::rowColumnSelection(void)  { activateCallback(MSWidgetCallback::selection); }


void MSList::undrawSelectedRow(Window window_,int row_)
{ drawSelectedRow(window_,row_,MSFalse); }

void MSList::updateScrollBars(void)
{
  if (firstRow()!=vsb()->value()) vsb()->valueChange(firstRow());
  if (firstColumn()!=hsb()->value()) hsb()->valueChange(firstColumn());
}

void MSList::createCycle(int row_)
{
  if (numRows()>0) 
   {
     if (cycleList().length()!=0) processCycleTimer();
     if (inRowRange(row_)==MSTrue)
      {
	if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
	   startCycle(row_,0,cycleColors(),cycleColorMode());
	else drawRow(row_);
      }
     if (cycleList().length()!=0)
      {
        if (cycleTimer()==0) _cycleTimer=new CycleTimer(this,cycleInterval());
        else cycleTimer()->reset();
      }
   }
}

MSBoolean MSList::editorActivate(void)
{
  if (editor()->mapped()==MSTrue)
   {
     if (validate(editor()->string(),selectedRow())==MSTrue)
      {
	unmapEditor();
      }
   }
  return (editor()->mapped()==MSTrue)?MSFalse:MSTrue;
}

int MSList::drawHeight(void) 
{ return rows()*rowHeight(); }

void MSList::updateVsb(void)
{
  vsb()->max(numRows());		
  vsb()->valueChange(firstRow());
  vsb()->viewSize(rows());
  vsb()->pageInc(rows()-1);
  vsb()->moveTo(panner()->x_origin()+panner()->width()+spacing(),
                panner()->y_origin()+headingsHeight());
  vsb()->height(panner()->height()-headingsHeight());
}

void MSList::updateHsb(void)
{
  int ml=actualNumColumns();
  if (doubleByte()==MSTrue) ml=ml/2;
  hsb()->max(ml);
  hsb()->valueChange(firstColumn());
  hsb()->viewSize(columns());
  hsb()->pageInc(columns()-1);
  hsb()->moveTo(panner()->x_origin(),panner()->y_origin()+panner()->height()+spacing());
  hsb()->width(panner()->width());
}

void MSList::hsbValueUpdate(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (hsb()->value()<firstColumn()) scrollLeft(firstColumn()-hsb()->value());     
     else if (hsb()->value()>firstColumn()) scrollRight(hsb()->value()-firstColumn());
   }
  else updateScrollBars();
}

void MSList::vsbValueUpdate(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (vsb()->value()<firstRow()) scrollDown(firstRow()-vsb()->value(),selectedRow());
     else if (vsb()->value()>firstRow()) scrollUp(vsb()->value()-firstRow(),selectedRow());
   }
  else updateScrollBars();
}

void MSList::updateSelectedRow(int row_)
{
  if(selectionMode()==MSToggle) return;
  int row=selectedRow();
  if (row_>=0&&row_<numRows())
   {
     if (selectionMode()==MSMultiple) 
     {
	if (selectionVector().indexOf(row_)==selectionVector().length())
	{
	  _selectionVector.append(row_);
	  _selectionVector.sortUp();
	}
     }
     if (inRowRange(row_)==MSTrue) moveSelection(row_);
     else if (row_>lastRow()) scrollUp(row_-(firstRow()+rows())+1,row_);
     else if (row_<firstRow()) scrollDown(firstRow()-row_,row_);
   }
   else if (row_<0) moveSelection(-1);
  rowColumnSelection();

}

void MSList::updateSelectionVector(const MSIndexVector& sv_)
{
  if (selectionMode()==MSMultiple)
   {
     // Make sure none of the values in the new vector is out of bound
     MSIndexVector newVector=sv_;
     newVector.sortUp();
     unsigned i,numrows=numRows();
     for (i=0;i<newVector.length();i++)
      {
	 if (newVector(i)>=numrows)
	  {
	     newVector.drop(i-newVector.length());
	     break;
	  }
      }
     const MSIndexVector osv=selectionVector(); // make a local copy of old select vector
     _selectionVector=newVector;
     for (i=0;i<osv.length();i++) 
      { 
	if (selected(osv(i))==MSFalse) drawRow(osv(i)); 
      }
     const MSIndexVector& nsv=selectionVector(); // only need a reference to new selection vector
     for (i=0;i<nsv.length();i++) drawRow(nsv(i)); 
     lastBlock(-1);
     moveSelection(-1);
   }
  else if(selectionMode()==MSToggle)
   {
     _selectionVector=sv_;
     redraw();
   }
}

MSString MSList::selection(void) 
{
  MSString buffer;
  formatOutput(buffer,selectedRow());
  return buffer;
}

void MSList::cycleCell(MSColorCycle *cycle_)
{
  if (cycle_->count()==cycle_->numCycles()) drawRow(cycle_->row());
  else drawCycle(cycle_);
}

void MSList::updateFirstRow(int row_)
{
  int oldFirstRow=firstRow();
  if (row_<0) row_=0;
  else if (row_>=numRows()) row_=numRows()-1;   
  
  if (row_>=0&&row_<numRows()&&firstRow()!=row_)
   {
     int diff=numRows()-rows();
     _firstRow=(row_>diff)?diff:row_;
     _firstRow=(firstRow()>=0)?firstRow():0;
     redrawImmediately();
     if (selectedRow()>=0&&selectedRow()>=numRows()) selectedRow(numRows()-1);
   }
  if (oldFirstRow!=firstRow()) firstRowChangeNotify();    
}

void MSList::updateFirstColumn(int column_)
{
  int oldFirstColumn=firstColumn();
  if (column_<0) column_=0;
  else if (column_>=actualNumColumns()) column_=actualNumColumns()-1;   

  if (column_<actualNumColumns()&&column_!=firstColumn())
   {
     _firstColumn=column_;
     _firstColumn=(column_>actualNumColumns()-columns())?actualNumColumns()-columns():column_;
     _firstColumn=(firstColumn()>=0)?firstColumn():0;
     redrawImmediately();
     if (oldFirstColumn!=firstColumn()) firstColumnChangeNotify();     // otherwise called in adjustFirstColumn
   }
}

const char *MSList::formatOutput(MSString &buffer_,unsigned)
{ return buffer_.string();}

unsigned long MSList::rowForeground(unsigned row_)
{
   if (foregroundColors().length()==0) return foreground();
   else return foregroundColors()(row_%foregroundColors().length());
}

unsigned long MSList::rowBackground(unsigned row_)
{
   if (backgroundColors().length()==0) return background();
   else return backgroundColors()(row_%backgroundColors().length());
}

Font MSList::rowFont(unsigned)
{ return font(); }

MSBoolean MSList::isRowProtected(unsigned r_)
{ return isProtected(r_); }
//For backward compatibility only
MSBoolean MSList::isProtected(unsigned)
{ return MSRowColumnView::isProtected(); }
MSBoolean MSList::isProtected(void) const
{ return MSRowColumnView::isProtected(); }

MSBoolean MSList::validate(const char *,unsigned)
{ return MSFalse; }

void MSList::calculateRowHeight(void)
{
  int tHeight=textHeight();
  int pHeight=maxPixmapHeight();
  int h=(tHeight>pHeight)?tHeight:pHeight;
  rowHeight(h+2*rowSpacing());
}

const MSPixmap *MSList::rowPixmap(unsigned row_)
{
  if(selectionMode()==MSToggle)
   {
     if (selected(row_)==MSTrue)
      {
        if(pixmapVector().length()>0) return pixmapVector()(0);
      }
     else if(pixmapVector().length()>1) return pixmapVector()(1);
     return 0;
   }
  else
   {
     if (row_<pixmapVector().length()) return pixmapVector()(row_);
     else return 0;
   }
}

int MSList::maxPixmapHeight(void)
{ return pixmapRegistry().maxPixmapHeight(); }

int MSList::maxPixmapWidth(void)
{ return pixmapRegistry().maxPixmapWidth(); }

int MSList::numPixmapColumns(void)
{
  int w=maxPixmapWidth();
  if (w>0)
   {
     return (int)(ceil((double)w/(double)charWidth()));
   }
  else return 0;
}

int MSList::actualNumColumns(void)
{
  return numPixmapColumns()+numColumns();
}

GC MSList::pixmapGC(void)
{
  if (_pixmapGC==0)
   {
     XGCValues values;
     _pixmapGC=XCreateGC(display(),window(),0,&values);
   }
  return _pixmapGC;
}

void MSList::scrollRight(int count_)
{
  int numCols=actualNumColumns();
  if (firstColumn()+columns()<numCols&&count_>0)
   {
     if (firstColumn()+columns()+count_>numCols)
      {
        count_=numCols-(firstColumn()+columns());
      }  
     _firstColumn+=count_;
     redrawImmediately();
     firstColumnChangeNotify();
   }
}

void MSList::right(void)
{
  if (editorActivate()==MSTrue) 
   { 
     if (firstColumn()+columns()-1<actualNumColumns()-1)
      {
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(selectedRow());
	   _selectionVector.append(selectedRow());
	 }
	firstColumn(firstColumn()+1);
      }
   }
}

void MSList::drawActualRow(Window window_,int row_,const char *pString_,int slen_,
                           const MSPixmap *pmap_,unsigned long fg_,unsigned long bg_,Font font_,
                           MSBoolean selected_,MSBoolean selectOutline_)
{
  if (row_>=firstRow()&&row_<firstRow()+rows())
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int xoff=offset+columnSpacing();
     int yy=computeYCoord(row_)+rowSpacing();
     int xx=xoff-(firstColumn()*charWidth());
     int ww=panner()->width()-(offset*2);
     //First fill the background, the size of the rectangle is dependent on
     //whether the row is selected or not
     if (selected_==MSTrue)
      {
        int selectedRowHeight=rowHeight()-2*rowSpacing();
	XSetForeground(display(),textGC(),bg_);
	XFillRectangle(display(),window_,textGC(),xoff,yy,
                       ww-(columnSpacing()<<1),selectedRowHeight);
      }
     else
      {
	XSetForeground(display(),textGC(),bg_);
	XFillRectangle(display(),window_,textGC(),offset,yy-rowSpacing(),
		       ww,rowHeight());
      }
     //Draw the pixmap if it is there
     if (pmap_!=0)
      {
        int tHeight=textHeight();
        int pHeight=pmap_->height();
        int startx=xx;
        int starty=yy;
        if (tHeight>pHeight) starty+=((tHeight-pHeight)/2);
        else yy+=((pHeight-tHeight)/2);
        int pw=maxPixmapWidth();
        //Optimization: if pixmap is completely offscreen, why draw it
        if (startx+pw>=xoff)
         {
           GC gc=pixmapGC();
	   XSetForeground(display(),gc,fg_);
	   XSetBackground(display(),gc,bg_);
           int psx=xoff-startx;
           int psw=pw-psx;
	   copyPixmap(display(),*pmap_,window_,gc,psx,0,psw,pmap_->height(),
		      xoff,starty,startx,starty);
	 }
      }
     xx+=(numPixmapColumns()*charWidth()); 
     //Draw the string
     if (pString_!=0&&slen_>0)
      {
        XSetForeground(display(),textGC(),fg_);
        XSetFont(display(),textGC(),font_);
        const XFontStruct *fontStruct=(font_==font()?
                                       textFontStruct():
                                       server()->fontStruct(font_));
        drawString(display(),window_,textGC(),fontStruct,
                   xx,yy+textAscent(),pString_,slen_);
      }
     if (selectOutline_==MSTrue) drawSelectOutline(window_,row_,highlighted());
     else if (selected_==MSTrue) drawSelectOutline(window_,row_,MSFalse);
   }
}

void MSList::drawString(Display *display_,Window window_,GC gc_,const XFontStruct *fs_,
                        int x_,int y_,const char *pString_,int len_)
{
  XDrawString(display_,window_,gc_,fs_,x_,y_,pString_,len_);
}

void MSList::registerPixmap(const MSPixmap &pixmap_)
{
  pixmapRegistry().add(pixmap_);
}
void MSList::unregisterPixmap(const MSString &pixmapName_)
{
  pixmapRegistry().remove(pixmapName_);
}

const MSPixmap* MSList::pixmap(const MSString& pixmapName_)
{
  return (MSPixmap*)pixmapRegistry().lookup(pixmapName_);
}

void MSList::pixmapList(const MSStringVector &pixmapList_)
{
  unsigned len=pixmapList_.length();
  _pixmapVector.reshape(len);
  for (unsigned i=0;i<len;i++)
   {
     _pixmapVector[i]=(MSPixmap *)pixmapRegistry().lookup(pixmapList_(i));
   }
  redrawImmediately();
}

MSStringVector MSList::pixmapList(void) const
{
  unsigned len=pixmapVector().length();
  MSStringVector list(len);
  for (unsigned i=0;i<len;i++)
   {
     list[i]=pixmapVector()(i)->name();
   }
  return list;
}

MSList::PixmapRegistry::PixmapRegistry(unsigned size_)
:MSHashTable(size_),_maxPixmapWidth(0),_maxPixmapHeight(0)
{}

MSList::PixmapRegistry::~PixmapRegistry(void)
{
  for (unsigned i=0;i<size();i++)
   {
     MSHashEntry *entry=bucket(i);
     MSPixmap *data;
     while (entry!=0)
      {
	_bucket[i]=entry->next();
        data=(MSPixmap *)entry->value();
        if (data!=0) delete data;   
	delete entry;
	entry=bucket(i);
      }
     _bucket[i]=0;
   }
  if (_bucket!=0) delete [] _bucket;
  _size=0;
  _bucket=0;

}

void MSList::PixmapRegistry::remove(const MSString &name_)
{
  MSPixmap *oldPix= (MSPixmap*)lookup(name_);
  if(oldPix!=0)
   {
     MSHashTable::remove(name_);
     if(oldPix->width()==_maxPixmapWidth||oldPix->height()==_maxPixmapHeight)
      {
        // recalculate maximum sizes.
        _maxPixmapHeight=0;
        _maxPixmapWidth=0;
        MSPixmap *pix;
        for (unsigned i=0;i<size();i++) 
         {
           MSHashEntry *entry=bucket(i);
           while (entry!=0)
            {
              pix = (MSPixmap*)entry->value();
              _maxPixmapWidth=(pix->width()>_maxPixmapWidth)?pix->width():_maxPixmapWidth;
              _maxPixmapHeight=(pix->height()>_maxPixmapHeight)?pix->height():_maxPixmapHeight;
              entry=entry->next();
            }
         }
      }
     delete oldPix;
   }
}

void MSList::PixmapRegistry::add(const MSPixmap &pixmap_)
{
  if (lookup(pixmap_.name())==0)
   {
     MSHashTable::add(pixmap_.name(),new MSPixmap(pixmap_));
     _maxPixmapWidth=(pixmap_.width()>_maxPixmapWidth)?pixmap_.width():_maxPixmapWidth;
     _maxPixmapHeight=(pixmap_.height()>_maxPixmapHeight)?pixmap_.height():_maxPixmapHeight;
   }
}

void MSList::toggleRow(unsigned row_)
{
  unsigned index=_selectionVector.indexOf(row_);
  if (index<_selectionVector.length()) 
   {
     _selectionVector.removeAt(index);
   }
  else
   {
    _selectionVector<<row_;
    _selectionVector.sortUp();
   }
  toggleRowNotify(row_);
}

void MSList::toggleRowNotify(unsigned)
{
  activateCallback(MSWidgetCallback::selection);
}

void MSList::toggleModeMoveRow (int from_, int to_)
{
  unsigned len=_selectionVector.length();
  if (len>0)
   {
     MSBoolean selected;
     unsigned index=_selectionVector.indexOf(from_);
     if (index<len) 
      {
        selected=MSTrue;
	_selectionVector.removeAt(index);
        len--;
      }
     else selected=MSFalse;

     if (from_<to_)
      {
        for (unsigned i=0;i<len;i++)
	 {
	   unsigned index=_selectionVector(i);
	   if (index>from_)
	    {
	      if (index<=to_) _selectionVector[i]=index-1;
	      else break;
	    }
	 }
      }
     else
      {
       for (unsigned i=0;i<len;i++)
	{
	   unsigned index=_selectionVector(i);
	   if (index>=to_)
	    {
	      if (index<from_) _selectionVector[i]=index+1;
	      else break;
	    }
	}
      }
     if (selected==MSTrue)
      {
        _selectionVector<<to_;
	_selectionVector.sortUp();
      }
   }
}

void MSList::updateSelectionMode(MSSelectionMode selectionMode_)
{
  //add support for MSToggle model
  if(selectionMode_==MSToggle)
   {
     if(selectionMode_!=selectionMode())
      {
        _selectionMode=selectionMode_;
        if(selectedRow() >= 0)
         {
           if(_selectionVector.length()==0) _selectionVector.append(selectedRow());
           else _selectionVector.sortUp();
           _selectedRow=-1;
         }
      }
     redraw();
   }
  else
   {
     MSRowColumnView::updateSelectionMode(selectionMode_);
   }
}

void MSList::updateBackground(unsigned long oldBg_)
{
  if(oldBg_==selectedRowForeground())  selectedRowForeground(background());
  MSRowColumnView::updateBackground(oldBg_);
}

unsigned long MSList::selectedRowBackgroundColor(int row_)
{
  return selectedRowBackground();
}
  
unsigned long MSList::selectedRowForegroundColor(int row_)
{
  if(selectedRowForeground()!=background()) return selectedRowForeground();
  else return rowBackground(row_);
}

void MSList::selectedRowForeground(const char *fg_)
{ selectedRowForeground(server()->pixel(fg_)); }

void MSList::selectedRowForeground(unsigned long fg_)
{ 
  if (_selectedRowForeground!=fg_)
   {
     _selectedRowForeground=fg_;
     redraw();
   }
}

int MSList::clearSelection(void)
{
  if(selectionMode()==MSToggle) return 0;
  else return MSRowColumnView::clearSelection();
}

void MSList::up(void)
{
  if(selectionMode()==MSToggle)
   {
     if(firstRow()>0) firstRow(firstRow()-1);
   }
  else MSRowColumnView::up();
}

void MSList::down(void)
{
  if(selectionMode()==MSToggle)
   {
     if(rows()<numRows()&&firstRow()<(numRows()-rows()))
      {
        firstRow(firstRow()+1);
      }
   }
  else MSRowColumnView::down();
}
