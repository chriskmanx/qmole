///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <string.h>
#include <MSTypes/MSUtil.H>
#include <MSGUI/MSArrayView.H>
#include <MSGUI/MSBusy.H>
#include <MSGUI/MSGC.H>
#include <MSGUI/MSFontObject.H>

static const unsigned MSArrayViewColumnDefaultEditWidth=256;

#ifdef MS_NO_INLINES
#include <MSGUI/MSArrayViewInlines.C>
#endif

MSArrayView::MSArrayView(MSWidget *owner_,const char *title_) : MSRowColumnView(owner_,title_) 
{
  init(); 
}
MSArrayView::MSArrayView(MSWidget *owner_,const MSStringVector& title_) : MSRowColumnView(owner_,title_) 
{
  init(); 
}

MSArrayView::~MSArrayView(void)
{
  delete _stipple;
}

void MSArrayView::init(void)
{
  _stipple=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);
   //Set default values for attributes
  _fixedColumns=0;
  _rowSeparator=1;
  _columnSeparator=1;
  _columnWidth=9;
  _selectedColumn=-1;
  _selectBg=background();
  XSetStipple(display(),textGC(),_stipple->pixmap());
}

// default methods that can be overriden by subclass that supports
// extra view manipulation functionalities
const char *MSArrayView::viewFormatOutput(MSString &buffer_,unsigned row_,unsigned column_) 
{ return formatOutput(buffer_,row_,column_); }
MSBoolean MSArrayView::viewValidate(const char *string_,unsigned row_,unsigned column_) 
{ return validate(string_,row_,column_);}
MSBoolean MSArrayView::isViewProtected(unsigned row_,unsigned column_)
{ return isCellProtected(row_,column_);}
MSBoolean MSArrayView::isViewValid(unsigned row_,unsigned column_)
{ return isValid(row_,column_);}
unsigned long MSArrayView::viewCellForeground(unsigned row_,unsigned column_)
{ return cellForeground(row_,column_); }
unsigned long MSArrayView::viewCellBackground(unsigned row_,unsigned column_)
{ return cellBackground(row_,column_); }
Font MSArrayView::viewCellFont(unsigned row_,unsigned column_)
{ return cellFont(row_,column_);}
MSAlignment MSArrayView::viewCellAlignment(unsigned row_,unsigned column_)
{ return cellAlignment(row_,column_);}

// default methods that should be overriden by data specific subclasses
const char *MSArrayView::formatOutput(MSString &buffer_,unsigned,unsigned) 
{ return buffer_.string();}
MSBoolean MSArrayView::validate(const char *,unsigned,unsigned) 
{ return MSFalse; }

//"isProtected(row,col)" isonly kept for Backward compatibility
//It's use is now deprecated in favor of isCellProtected
MSBoolean MSArrayView::isCellProtected(unsigned r_,unsigned c_)
{ return isProtected(r_,c_); }
MSBoolean MSArrayView::isProtected(unsigned,unsigned)
{ return MSRowColumnView::isProtected(); }
MSBoolean MSArrayView::isProtected(void) const
{ return MSRowColumnView::isProtected(); }

MSBoolean MSArrayView::isValid(unsigned,unsigned)
{ return sensitive();}
unsigned long MSArrayView::cellForeground(unsigned row_,unsigned)
{
   if (foregroundColors().length()==0) return foreground();
   else return foregroundColors()(row_%foregroundColors().length());
}
unsigned long MSArrayView::cellBackground(unsigned row_,unsigned)
{
   if (backgroundColors().length()==0) return background();
   else return backgroundColors()(row_%backgroundColors().length());
}
Font MSArrayView::cellFont(unsigned,unsigned)
{ return font(); }
MSAlignment MSArrayView::cellAlignment(unsigned,unsigned)
{ return MSLeft; }

MSString MSArrayView::selection(void) 
{
  MSString buffer;
  viewFormatOutput(buffer,selectedRow(),selectedColumn());
  return buffer;
}

int MSArrayView::labelWidth(void)     
{ return 0; }

void MSArrayView::defaultNumVisible(void)
{
  if ((sizeState()&RowsValid)!=RowsValid)
   {
     _rows=5; 
     _rows=(rows()<=numRows())?rows():numRows(); 
   }
  if ((sizeState()&ColsValid)!=ColsValid)
   {
     _columns=2; 
     _columns=(columns()<=numColumns())?columns():numColumns(); 
     _columns=(columns()<=0)?0:columns(); 
   }
}


void MSArrayView::adjustView(void)
{
  //This method resets the first column as a result of the widget being
  //resized or scrolling.
  if (firstColumn()>fixedColumns())
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int deltaWidth=panner()->width()-2*offset-drawWidth();
     int i=firstColumn();
     int count=1;
     while (deltaWidth>0&&i>0)
      {
        deltaWidth-=columnPixelWidth(firstColumn()-count);
        if (deltaWidth>=0) i--;
	count++;
      }
     if (i!=firstColumn())
      {
        _firstColumn=i;
	_firstColumn=(firstColumn()>=fixedColumns())?firstColumn():fixedColumns();
	_columns=computeNumVisibleColumns();     
	updateView();
	firstColumnChangeNotify();     
      }
   }
}

void MSArrayView::adjustFirstColumn(void)
{
  // This method ensures the first column setting makes sense
  int oldFirstColumn=firstColumn();
  if (firstColumn()<fixedColumns()) _firstColumn=fixedColumns();
  else if (firstColumn()>fixedColumns()&&
           firstColumn()+columns()-fixedColumns()>=numColumns())
   {
     _firstColumn=(numColumns()>columns())?
                 (numColumns()-columns()+fixedColumns()):fixedColumns();
     _firstColumn=(firstColumn()>=fixedColumns())?firstColumn():fixedColumns();
   }
  if (oldFirstColumn!=firstColumn()) firstColumnChangeNotify();     
}

void MSArrayView::adjustNumVisible(void)
{
  //This method is called to adjust the following data members: _row, _column,
  //_firstRow, _firstColumn.
  updateInternalState();
  if ((sizeState()&AdjustSize)==AdjustSize)
   {
     _rows=computeNumVisibleRows();
     _rows=(rows()<0)?0:rows(); 
     adjustFirstRow();

     adjustFirstColumn();
     _columns=computeNumVisibleColumns();
     _columns=(columns()<=0)?0:columns(); 

     adjustSelection();     
     updateHsb();
     updateVsb();
   }
}

void MSArrayView::adjustSelection(void)
{
  int sr=selectedRow();
  if (selectedRow()>=0)
   {
     if (selectedRow()>=numRows()&&selectedColumn()>=numColumns())
      {
	_selectedRow=numRows()-1;
	_selectedColumn=numColumns()-1;
      }
     else if (selectedRow()>=numRows()) _selectedRow=numRows()-1;
     if (sr!=selectedRow()&&editor()->mapped()==MSTrue) unmapEditor();
   }
}

void MSArrayView::shapeUpdate(void) 
{
  if (editor()->mapped()==MSTrue) unmapEditor();
  removeAllCycles();
  if (numRows()==0) adjustNumVisible();
  else
   {
     updateInternalState();
     updateHsb();
     updateVsb();
     adjustFirstRow();
     adjustFirstColumn();
     if (selectedRow()>=0)
      {
	if (selectedRow()>=numRows()&&selectedColumn()>=numColumns())
	 {
	   _selectedRow=numRows()-1;
	   _selectedColumn=numColumns()-1;
	 }
	else if (selectedRow()>=numRows()) _selectedRow=numRows()-1;
	else if (selectedColumn()>=numColumns()) _selectedColumn=numColumns()-1;
	if (selectedRow()<0||selectedColumn()<0)
	 {
	   _selectedRow=-1;
	   _selectedColumn=-1;
	 }
      }
     // If we are in multiple selection mode, always make sure the current
     // selected row is in the selection vector.
     if (selectionMode()==MSMultiple&&selectedRow()!=-1&&
	 selectionVector().indexOf(selectedRow())==selectionVector().length())
      {
	_selectionVector.append(selectedRow());
	_selectionVector.sortUp();
      }     
     updateScrollBars();
   }
  redrawImmediately();
}

void MSArrayView::appendUpdate(void) 
{
  int rs=vsb()->max();
  updateVsb();
  if (rs>=firstRow()&&rs<=lastRow()) 
   {
     drawHSeparators(panner()->window(),firstRow(),lastRow(),firstColumn(),lastColumn());
     drawVSeparators(panner()->window(),firstRow(),lastRow(),firstColumn(),lastColumn());
     moveSelection(selectedRow(),selectedColumn());
   }
}


void MSArrayView::updateVsb(void)
{
  vsb()->max(numRows());		
  vsb()->valueChange(firstRow());
  vsb()->viewSize(rows());
  vsb()->pageInc(rows()-1);
  if (isVsbEnabled()==MSTrue) 
   {
     if (rows()<numRows()&&vsb()->width()>1)
      {
	vsb()->moveTo(panner()->x_origin()+panner()->width()+spacing(),
		      panner()->y_origin()+headingsHeight());
	vsb()->height(panner()->height()-headingsHeight());
	if (vsb()->mapped()==MSFalse) vsb()->map();
      }
     else if (vsb()->mapped()==MSTrue) vsb()->unmap();
   }
}

void MSArrayView::vsbValueUpdate(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (vsb()->value()<firstRow()) scrollDown(firstRow()-vsb()->value(),selectedRow());
     else if (vsb()->value()>firstRow()) scrollUp(vsb()->value()-firstRow(),selectedRow());
   }
  else updateScrollBars();
}


void MSArrayView::moveSelection(int row_,int column_)
{
  int oldRow=selectedRow();
  int oldCol=selectedColumn();
  
  if (row_==-1||column_==-1)
   {
     _selectedRow=row_;
     undrawSelectedRow(panner()->window(),oldRow);
     _selectedColumn=column_;
   }
  if (row_==selectedRow()&&column_!=selectedColumn())
   {
     _selectedColumn=column_;
     undrawSelectedCell(panner()->window(),selectedRow(),oldCol);
     drawSelectedCell(panner()->window(),selectedRow(),selectedColumn());
   }
  else if (column_==selectedColumn()&&row_!=selectedRow())
   {
     _selectedRow=row_;
     undrawSelectedRow(panner()->window(),oldRow);
     drawSelectedRow(panner()->window(),selectedRow());
   }
  else if (column_!=selectedColumn()&&row_!=selectedRow())
   {
     _selectedRow=row_;
     undrawSelectedRow(panner()->window(),oldRow);
     _selectedColumn=column_;
     drawSelectedRow(panner()->window(),selectedRow());
   }
  else drawSelectedRow(panner()->window(),selectedRow());
}

void MSArrayView::drawSelectedCell(Window window_,int row_,int column_)
{ drawSelectedCell(window_,row_,column_,MSTrue); }
void MSArrayView::undrawSelectedCell(Window window_,int row_,int column_)
{ drawSelectedCell(window_,row_,column_,MSFalse); }
void MSArrayView::drawSelectedRow(Window window_,int row_)
{ drawSelectedRow(window_,row_,MSTrue); }
void MSArrayView::undrawSelectedRow(Window window_,int row_)
{ drawSelectedRow(window_,row_,MSFalse); }
void MSArrayView::drawSelectedRow(int row_)
{ drawSelectedRow(panner()->window(),row_,MSTrue); }
void MSArrayView::undrawSelectedRow(int row_)
{ drawSelectedRow(panner()->window(),row_,MSFalse); }
void MSArrayView::drawSelectedRow(int row_,MSBoolean select_)
{ drawSelectedRow(panner()->window(),row_,select_); }

void MSArrayView::cleanUpDrawArea(Window window_) 
{ cleanUpBottom(window_);cleanUpRight(window_); }

// used for cell updates only
void MSArrayView::drawRowColumn(int row_,int column_)
{
  if (row_<numRows()&&column_<numColumns()&&
      inColRange(column_)==MSTrue&&inRowRange(row_)==MSTrue)
   {
     drawCell(panner()->window(),computeXCoord(column_),computeYCoord(row_),row_,column_);
   }  
}

void MSArrayView::drawColumn(int column_)
{
  if (column_<numColumns()&&inColRange(column_)==MSTrue)
   {
//     drawHeadings(panner()->window(),column_,column_);
     drawRows(panner()->window(),firstRow(),lastRow(),column_,column_);
   }
}

void MSArrayView::drawSelectedRow(Window window_,int row_,MSBoolean select_)
{
  if (row_<numRows()&&inRowRange(row_)==MSTrue)
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness()+labelWidth();
     int y=computeYCoord(row_);
     int j=0;
     int x=offset;
     int n=numColumns();
     for (j=0;j<n&&j<fixedColumns();j++)
      { 
	if (j!=selectedColumn()) 
         {if (columnBackground(j)!=selectedRowBackground()) drawCell(window_,x,y,row_,j);}
	x+=columnPixelWidth(j);
      }
     for (j=firstColumn();j<n&&j<=lastColumn();j++)
      {
	if (j!=selectedColumn()) 
         {if (columnBackground(j)!=selectedRowBackground()) drawCell(window_,x,y,row_,j);}
	x+=columnPixelWidth(j);
      }
     drawSelectedCell(window_,row_,selectedColumn(),select_);
   }
}

void MSArrayView::drawSelectBevel(MSRect& r_,MSBoolean select_)
{
  int offset=panner()->highlightThickness()+panner()->shadowThickness();
  if (r_.x()+r_.width()>panner()->width()-offset) 
   { r_.width(panner()->width()-offset-r_.x()); }
  (select_==MSTrue)?drawRaised(panner()->window(),r_,columnSpacing()):
               	  undrawBevel(panner()->window(),r_,columnSpacing());
}

void MSArrayView::drawSelectedCell(Window window_,int row_,int column_,MSBoolean select_)
{
  if (inRowRange(row_)==MSTrue&&inColRange(column_)==MSTrue)
   {
     int x=computeXCoord(column_);
     int y=computeYCoord(row_);

     if (select_==MSFalse)
      {
	MSRect aRect(x,y,columnPixelWidth(column_),rowHeight());
        drawSelectBevel(aRect,MSFalse);
      }
     drawCell(panner()->window(),x,y,row_,column_);
     if (select_==MSFalse) 
      {
	drawHSeparators(window_,row_,row_,column_,column_);
	drawVSeparators(window_,row_,row_,column_,column_); 
      }
   }
}

void MSArrayView::updateSelectedRow(int row_)
{
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
     if (selectedRow()==-1&&selectedColumn()==-1) _selectedColumn=0;
     if (inRowRange(row_)==MSTrue) moveSelection(row_,selectedColumn());
     else if (row_>lastRow()) scrollUp(row_-(firstRow()+rows())+1,row_);
     else if (row_<firstRow()) scrollDown(firstRow()-row_,row_);
   }
  else if (row_<0) moveSelection(-1,-1);
  rowColumnSelection();
}
   
void MSArrayView::updateFirstRow(int row_)
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


void MSArrayView::setSelection(int row_,int column_) 
{ 
  if (row_!=selectedRow()||column_!=selectedColumn())
   {
     int oldFirstColumn=firstColumn();
     int oldFirstRow=firstRow();  
     if (column_==-1||row_==-1) moveSelection(-1,-1);
     else
      {
	//If we're in multiple selection mode, add the new selection to the
	//selection vector
	if (row_!=selectedRow()&&selectionMode()==MSMultiple) 
	 {
	   if (selectionVector().indexOf(row_)==selectionVector().length())
	    {
	      _selectionVector.append(row_);
	      _selectionVector.sortUp();
	    }
	 }
	if (column_>=fixedColumns()&&column_<numColumns()&&row_>=0&&row_<numRows())
	 {
	   if (inColRange(column_)==MSTrue&&inRowRange(row_)==MSTrue) moveSelection(row_,column_);
	   else
	    { 
	      if (inColRange(column_)==MSTrue) _selectedColumn=column_;
	      else
	       {
		 _firstColumn=(column_>numColumns()-columns()+fixedColumns())?
		 numColumns()-columns()+fixedColumns():column_;
		 _selectedColumn=column_;	      
	       }
	      if (inRowRange(row_)==MSTrue) _selectedRow=row_;
	      else
	       {
		 _firstRow=(row_>numRows()-rows())? 
		 numRows()-rows():row_;
		 _selectedRow=row_;	      
	       }
	      redrawImmediately();
	    }
	 }
	else if (column_>=0&&column_<fixedColumns())
	 {
	   if (inRowRange(row_)==MSTrue) moveSelection(row_,column_);
	   else 
            {
	      undrawSelectedCell(panner()->window(),selectedRow(),selectedColumn());
	       _selectedColumn=column_;
	      if (row_>lastRow()) scrollUp(row_-(firstRow()+rows())+1,row_);
	      else if (row_<firstRow()) scrollDown(firstRow()-row_,row_);
	    }
	 }
      }
     if (oldFirstColumn!=firstColumn()) firstColumnChangeNotify();
     if (oldFirstRow!=firstRow()) firstRowChangeNotify();  
   }
}

// right arrow
void MSArrayView::scrollRight(int count_)
{
  if (lastColumn()<numColumns()-1&&count_>0)
   {
     unsigned oldFirstColumn=firstColumn();
     if (firstColumn()+columns()+count_-fixedColumns()>numColumns())
      {
        count_=numColumns()-(lastColumn()+1);
      }  
     if (count_>=columns()-fixedColumns())
      {
        _firstColumn+=count_;
	_columns=computeNumVisibleColumns();
	_firstColumn=(firstColumn()>numColumns()-columns()+fixedColumns())? 
	             numColumns()-columns()+fixedColumns():firstColumn();
	updateView();	   
        adjustView();
	if (selectedColumn()>=firstColumn()+columns()-fixedColumns())
	 {
	   _selectedColumn=firstColumn()+columns()-fixedColumns()-1;
	 }
	redrawImmediately();
      }
     else 
      {
        int delta=0;
	int i;
	for (i=0;i<count_;i++) delta+=columnPixelWidth(firstColumn()+i);

        int fw=fixedColumnPixelWidth();
        int offset=panner()->highlightThickness()+panner()->shadowThickness();
        int offset2=offset<<1;
        int src_x=offset+fw+delta+labelWidth();
        int src_y=columnHeadingsOffset();
        int dest_x=offset+fw+labelWidth();
        int dest_y=src_y;
        int h=panner()->height()-offset2;
        int cs=lastColumn()+1;
        int w=0;
        int oldFirst=firstColumn();

        for (i=firstColumn()+count_;i<=lastColumn();i++) w+=columnPixelWidth(i);
         
        cs=(cs<numColumns())?cs:numColumns()-1;
        _firstColumn+=count_;
	_columns=computeNumVisibleColumns();
	_firstColumn=(firstColumn()>numColumns()-columns()+fixedColumns())? 
	             numColumns()-columns()+fixedColumns():firstColumn();
	updateView();	   
	if (selectedColumn()>lastColumn()) _selectedColumn=lastColumn();
        XCopyArea(display(),panner()->window(),panner()->window(),
		  panner()->backgroundShadowGC(),src_x,src_y,w,h,dest_x,dest_y);
        if (columnSeparator()>0)
	 {
           XRectangle *rects=new XRectangle[columns()];
           int n=0;
           int sum=delta;
           for (int k=firstColumn();k<=lastColumn();k++)
	    {
              sum+=columnPixelWidth(k);
              if (sum>w)
	       {
                 rects[n].x=offset+fw+labelWidth()+sum-columnSpacing();
                 rects[n].y=offset+headingsHeight();
                 rects[n].width=columnSpacing();
                 rects[n].height=panner()->height()-offset2-headingsHeight();
                 n++;
	       }
	    }
           if (n>0) XFillRectangles(display(),panner()->window(),
                                    panner()->backgroundShadowGC(),rects,n);
           delete []rects;
	 }
        int cw=0;
	for (i=cs;i<=lastColumn()&&i<numColumns();i++) cw+=columnPixelWidth(i);
        if (dest_x+w+cw<panner()->width()-offset)
	 {
           int nRows=(rows()>0)?rows():1;
           XRectangle *back=new XRectangle[nRows+1];
           XRectangle *sback=new XRectangle[nRows];
           int b=0,sb=0;
           int hh=rowHeight();
           int xx=dest_x+w+cw;
           int yy=offset+headingsHeight();
           int ww=panner()->width()-offset-xx;
           int nr=numRows();
           int inc=rowSeparator();
	   back[b].x=xx;
	   back[b].y=offset;
	   back[b].width=ww;
	   back[b].height=headingsHeight()-rowSpacing();
           b++;
           for (int r=firstRow();r<=lastRow()&&r<nr;r++)
	    {
              if (inc>0) hh=(r%inc==inc-1||r==nr-1)?rowHeight()-rowSpacing():rowHeight();
              if (selected(r)==MSTrue)
	       {
		 sback[sb].x=xx;
		 sback[sb].y=yy;
		 sback[sb].width=ww;
		 sback[sb].height=hh;
		 sb++;
	       }
              else
	       {
                 back[b].x=xx;
		 back[b].y=yy;
		 back[b].width=ww;
		 back[b].height=hh;
                 b++;
	       }
              yy+=rowHeight();
	    }
	   if (b>0) XFillRectangles(display(),panner()->window(),backgroundShadowGC(),back,b);
	   if (sb>0) XFillRectangles(display(),panner()->window(),selectionVectorGC(),sback,sb);
           delete [] back;
           delete [] sback;	   
	 }
        drawRows(panner()->window(),firstRow(),lastRow(),cs,lastColumn());
        redrawHeadings(panner()->window(),cs,lastColumn());
        cleanUpRight(panner()->window());
        cleanUpBottom(panner()->window());
	updateScrollBars();
      }
     if (oldFirstColumn!=firstColumn()) firstColumnChangeNotify();
   }
}

// left arrow
void MSArrayView::scrollLeft(int count_)
{
  if (firstColumn()>fixedColumns()&&count_>0)
   {
     unsigned oldFirstColumn=firstColumn();
     if (firstColumn()-count_<fixedColumns())
      {
        count_=fixedColumns()+firstColumn();
      }  
     if (count_>=columns()-fixedColumns())
      {
        _firstColumn-=count_;
	_firstColumn=(firstColumn()>fixedColumns())?firstColumn():fixedColumns();
	_columns=computeNumVisibleColumns();     
	updateView();
	redrawImmediately();
      }
     else  
      {
        int fw=fixedColumnPixelWidth();
        int delta=0;
        int i;
        for (i=1;i<=count_;i++) delta+=columnPixelWidth(firstColumn()-i);
        int offset=panner()->highlightThickness()+panner()->shadowThickness();
        int offset2=offset<<1;
        int src_x=offset+fw+labelWidth();
        int src_y=columnHeadingsOffset();
        int dest_x=src_x+delta;
        int dest_y=src_y;
        int h=panner()->height()-offset2;
        int w=0;

        _firstColumn-=count_;
	_firstColumn=(firstColumn()>fixedColumns())?firstColumn():fixedColumns();
	_columns=computeNumVisibleColumns();     
        for (i=firstColumn()+count_;i<=lastColumn();i++) w+=columnPixelWidth(i);
	updateView();
        XCopyArea(display(),panner()->window(),panner()->window(),
		  backgroundShadowGC(),src_x,src_y,w,h,dest_x,dest_y);
        if (columnSeparator()>0)
	 {
           XRectangle *rects=new XRectangle[columns()];
           int n=0;
           int sum=0;
           for (int k=firstColumn()+count_;k<=lastColumn()+count_&&sum<delta;k++)
	    {
              sum+=columnPixelWidth(k);
              if (sum<delta)
	       {
                 rects[n].x=offset+fw+labelWidth()+sum-columnSpacing();
                 rects[n].y=offset+headingsHeight();
                 rects[n].width=columnSpacing();
                 rects[n].height=panner()->height()-offset2-headingsHeight();
                 n++;
	       }
	    }
           if (n>0) XFillRectangles(display(),panner()->window(),backgroundShadowGC(),rects,n);
           delete []rects;
	 }
        if (dest_x+w<panner()->width()-offset)
	 {
           int nRows=(rows()>0)?rows():1;
           XRectangle *back=new XRectangle[nRows+1];
           XRectangle *sback=new XRectangle[nRows];
           int b=0,sb=0;
           int hh=rowHeight();
           int xx=dest_x+w;
           int yy=offset+headingsHeight();
           int ww=panner()->width()-offset-xx;
           int nr=numRows();
           int inc=rowSeparator();
	   back[b].x=xx;
	   back[b].y=offset;
	   back[b].width=ww;
	   back[b].height=headingsHeight()-rowSpacing();
           b++;
           for (int r=firstRow();r<=lastRow()&&r<nr;r++)
	    {
              if (inc>0) hh=(r%inc==inc-1||r==nr-1)?rowHeight()-rowSpacing():rowHeight();
              if (selected(r)==MSTrue)
	       {
		 sback[sb].x=xx;
		 sback[sb].y=yy;
		 sback[sb].width=ww;
		 sback[sb].height=hh;
		 sb++;
	       }
              else
	       {
                 back[b].x=xx;
		 back[b].y=yy;
		 back[b].width=ww;
		 back[b].height=hh;
                 b++;
	       }
              yy+=rowHeight();
	    }
	   if (b>0) XFillRectangles(display(),panner()->window(),backgroundShadowGC(),back,b);
	   if (sb>0) XFillRectangles(display(),panner()->window(),selectionVectorGC(),sback,sb);
           delete [] back;
           delete [] sback;	   
	 }
        drawRows(panner()->window(),firstRow(),lastRow(),firstColumn(),firstColumn()+count_);
        redrawHeadings(panner()->window(),firstColumn(),firstColumn()+count_);
        cleanUpRight(panner()->window());
        cleanUpBottom(panner()->window());
	updateScrollBars();
      }
     if (oldFirstColumn!=firstColumn()) firstColumnChangeNotify();     
   }
}

void MSArrayView::postVerticalScrollDraw(int rs_,int re_,MSBoolean drawNewRow_)
{
  drawLabels(panner()->window(),rs_,re_);
  if (drawNewRow_==MSTrue)
   {
     drawRows(panner()->window(),rs_,re_);
   }
  cleanUpBottom(panner()->window());
}

void MSArrayView::left(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (selectedColumn()>0) 
      {
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(selectedRow());
	   _selectionVector.append(selectedRow());
	 }
	selectedColumn(selectedColumn()-1); 
      }
   }
}

void MSArrayView::right(void)
{ 
  if (editorActivate()==MSTrue) 
   { 
     if (selectedColumn()<numColumns()-1) 
      {
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(selectedRow());
	   _selectionVector.append(selectedRow());
	 }
	selectedColumn(selectedColumn()+1); 
      }
   }
}

void MSArrayView::shiftTab(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (selectedRow()==0&&selectedColumn()==0) return;
     int nCols=numColumns();
     int sCol,sRow=selectedRow();
     int i;
     for (i=0,sCol=selectedColumn()-1;i<nCols;i++,sCol--)
      {
        if(sCol<0) // check if we need to wrap around
         {
           if(sRow==0) break;
           sCol=numColumns()-1;
           sRow--;
         }
        if (isViewProtected(sRow,sCol)==MSFalse) 
         { 
           clearSelection();
           if (selectionMode()==MSMultiple) 
            {
              lastBlock(sRow);
              _selectionVector.append(sRow);
            }
           if(sRow==selectedRow()) selectedColumn(sCol);
           else selectedRowColumn(sRow,sCol);
           break; 
         } 
      }
   }
}


void MSArrayView::tab(void)
{
  if (editorActivate()==MSTrue) 
   {
     int nCols=numColumns();
     int sCol,sRow=selectedRow();
     int i;
     if (sRow==numRows()-1&&selectedColumn()==nCols-1) return;
     for (i=0,sCol=selectedColumn()+1;i<nCols;i++,sCol++)
      {
        if(sCol==nCols)  // check if we need to wrap around
         {
           if(sRow==numRows()-1) break;
           sCol=0;
           sRow++;
         }
        if (isViewProtected(sRow,sCol)==MSFalse) 
         { 
           clearSelection();
           if (selectionMode()==MSMultiple) 
            {
              lastBlock(sRow);
              _selectionVector.append(sRow);
            }
           if(sRow==selectedRow()) selectedColumn(sCol);
           else selectedRowColumn(sRow,sCol);
           break;
         } 
      }
   }
}

void MSArrayView::doubleClick(void)
{
  if (editor()->mapped()==MSFalse)
   {
    if (selectedColumn()<numColumns()&&selectedRow()>=0&&selectedRow()<numRows())
     { activateCallback(MSWidgetCallback::doubleclick); }
   }
}

void MSArrayView::rowColumnSelection(void)  { activateCallback(MSWidgetCallback::selection); }
void MSArrayView::columnSelection(void)     { activateCallback(MSWidgetCallback::columnselection); }
void MSArrayView::columnMenuButtonSelection(void)    { activateCallback(MSWidgetCallback::columnmenubutton); }
void MSArrayView::labelSelection(void)      { activateCallback(MSWidgetCallback::labelselection); }

void MSArrayView::defaultButtonBehavior(const XEvent *pEvent_)
{
  // See if the the button press falls inside the panner region,
  // otherwise forward it to the appropriate widget
  if (pEvent_->xbutton.subwindow==panner()->window())
   {
     // See if we can get the focus
     if (traverseFocus(this)==MSTrue)
      {
	// We want to process button event if the widget has data in it
	if (numRows()>0&&numColumns()>0)
	 {
	   // if the editor is active, validate the input and unmap it
	   if (editorActivate()==MSTrue)
	    {
	      // recompute the the (x,y) of the event relative to the 
	      // position of the panner
	      XEvent *pEvent=(XEvent *)pEvent_;
	      pEvent->xbutton.y-=panner()->y_origin();
	      pEvent->xbutton.x-=panner()->x_origin();

	      if (pEvent->xbutton.y<headingsHeight())
	       {
		 if (pEvent->xbutton.x<labelWidth()) headingLabelAreaSelection(pEvent);
		 else headingAreaSelection(pEvent);
	       }
	      else
	       {
		 if (pEvent->xbutton.x<labelWidth()) labelAreaSelection(pEvent);
		 else dataAreaSelection(pEvent);
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

void MSArrayView::headingLabelAreaSelection(const XEvent *)
{ labelSelection(); }

void MSArrayView::headingAreaSelection(const XEvent *pEvent_)
{
  // Figure out the column selected by the button
  int col=columnFromEvent(pEvent_);
  if (inColRange(col)==MSTrue) 
   {
     if (selectedColumn()!=col) 
      {
	int row;
	if (selectedRow()<0&&numRows()>0) row=0;
	else row=selectedRow();
	if (row>=0)
	 {
           if (hasCallback(MSWidgetCallback::columnselection)==MSTrue||
               hasCallback(MSWidgetCallback::columnmenubutton)==MSTrue)
            {
              setSelection(row,col);
            }
           else selectedRowColumn(row,col);
	 }
      }
     if (selectedColumn()>=0&&selectedRow()>=0)
      {
        if (pEvent_->xbutton.button==Button3)
         {
           columnMenuButtonSelection();
         }
        else
         {
           columnSelection();
         }
      }
   }
}

void MSArrayView::labelAreaSelection(const XEvent *pEvent_)
{
  // Figure out the row selected by the button
  int row=yToRow(pEvent_->xbutton.y-headingsHeight())+firstRow();
  if (inRowRange(row)==MSTrue) 
   {
     lastBlock(row);
     if (selectionMode()==MSMultiple) 
      {
	clearSelection();
	_selectionVector.append(row);
      }
     if (selectedRow()!=row) 
      {
	int col;
	if (selectedColumn()<0&&numColumns()>0) col=0;
	else col=selectedColumn();
	if (row>=0)
	 {
	   if (callback(MSWidgetCallback::rowselection)==0) selectedRowColumn(row,col);
	   else setSelection(row,col);
	 }
      }
     rowSelection();       
   }
}

void MSArrayView::dataAreaSelection(const XEvent *pEvent_)
{
  int col=columnFromEvent(pEvent_);
  int row=yToRow(pEvent_->xbutton.y-headingsHeight())+firstRow();
  if (row<numRows()&&col<numColumns()&&inRowRange(row)==MSTrue&&inColRange(col)==MSTrue)
   {
     if (selectedColumn()!=col||selectedRow()!=row)
      {
	eventTime(pEvent_->xbutton.time);
	if (pEvent_->xbutton.button==Button1)
	 {
	   if (selectionMode()==MSMultiple)
	    {
	      // if control key is held down
	      if (pEvent_->xbutton.state&ControlMask)
	       {
                 // Control is held down, so check if the selected item is in
                 // the selection vector, if so, track erase selection, else
                 // track selection
		 if (selectionVector().indexOf(row)!=selectionVector().length()) trackUnselection(row,col);
		 else
		   trackSelection(row,col,MSFalse,MSFalse);
	       }
	      else
	       {
		 if (pEvent_->xbutton.state&ShiftMask) trackSelection(row,col,MSFalse,MSTrue);
		 else trackSelection(row,col,MSTrue,MSFalse);
	       }
	      
	    }
	   else
	    {
	      if ((pEvent_->xbutton.state&ControlMask)&&selectedRow()==row)
	       {
		 selectedRowColumn(-1,-1);
	       }
	      else selectedRowColumn(row,col);
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
	   selectedRowColumn(row,col);				
	 }
      }
     else if (selectedColumn()==col&&selectedRow()==row)
      {
	if (pEvent_->xbutton.button==Button1) 
	 {
	   if (isDoubleClick(pEvent_)==MSTrue) defaultDoubleClickBehavior(pEvent_);
	   else if (selectionMode()==MSMultiple)
	    {
	      // if control key is held down, track erase selection
	      if (pEvent_->xbutton.state&ControlMask) trackUnselection(row,col);
	      else
	       {
		 if (pEvent_->xbutton.state&ShiftMask) trackSelection(row,col,MSFalse,MSTrue);
		 else trackSelection(row,col,MSTrue,MSFalse);
	       }
	    }
	   else
	    {
	      if (pEvent_->xbutton.state&ControlMask)
	       {
		 selectedRowColumn(-1,-1);
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



void MSArrayView::update(const MSIndexVector& aIndexVector_)
{
  if (aIndexVector_.length()==0) 
   {
     if (vsb()->max()!=numRows() || hsb()->max()!=numColumns()) shapeUpdate();
     else redrawImmediately(); 
   }
  else
   {
     if (vsb()->max()!=numRows()) appendUpdate();
     if (numColumns()>1)     
      {
	unsigned nRows=numRows();
	unsigned nColumns=numColumns();
	unsigned row,column,index;
	for (unsigned i=0;i<aIndexVector_.length();i++) 
	{
	  index=aIndexVector_(i);
	  row=index/nColumns;
	  column=index-row*nColumns;
	  cycleRowColumn(row,column);	  
	}
      }
     else 
      {
	for (unsigned i=0;i<aIndexVector_.length();i++) cycleRowColumn(aIndexVector_(i),0);
      }
   }
}

MSBoolean MSArrayView::editorActivate(void)
{
  if (editor()->mapped()==MSTrue)
   {
     if (viewValidate(editor()->string(),selectedRow(),selectedColumn())==MSTrue)
      {
	unmapEditor();
      }
   }
  return (editor()->mapped()==MSTrue)?MSFalse:MSTrue;
}

int MSArrayView::drawHeight(void) 
{ return rows()*rowHeight(); }

int MSArrayView::drawWidth(void)
{
  int sum=0;
  int nc=numColumns();
  if (nc==0) sum=columns()*columnPixelWidth(0);
  else
   {
     int lc=lastColumn();
     sum=labelWidth()+fixedColumnPixelWidth();
     for (int i=firstColumn();i<=lc&&i<nc;i++) sum+=columnPixelWidth(i);
   }
  return sum;
}

unsigned MSArrayView::columnLength(unsigned) 
{ return columnWidth(); }
int MSArrayView::columnPixelWidth(int column_)
{ return (columnLength(column_)*charWidth('M'))+(columnSpacing()<<1); }

int MSArrayView::computeNumVisibleRows(void)
{
  //This method calculates and returns the number of rows that can be fitted
  //into the current window size
  int offset=panner()->highlightThickness()+panner()->shadowThickness();
  int h=panner()->height()-2*offset-headingsHeight();
  int r=0;
  while (h>=rowHeight()) { r++; h-=rowHeight(); }
  return r;
}

int MSArrayView::computeNumVisibleColumns(void)
{
  //This method calculates and returns the number of columns that can be fitted
  //into the current window size
  int offset=panner()->highlightThickness()+panner()->shadowThickness();
  int c=fixedColumns(),fc=firstColumn(),i=fc,nCols=numColumns();
  int deltaWidth=panner()->width()-2*offset-fixedColumnPixelWidth()-labelWidth();
  int sum=columnPixelWidth(i);
   
  while (sum<=deltaWidth&&i<nCols) 
   {
     c++,i++; 
     sum+=columnPixelWidth(i); 
   }  
  return (c==0)?1:c;
}

int MSArrayView::computeXCoord(int column_)
{
  //This method compute the X coordinate of the upper left corner of the specified column
  int xr=panner()->highlightThickness()+panner()->shadowThickness()+labelWidth();
  int i;
  if (column_<fixedColumns()) for(i=0;i<column_;i++) xr+=columnPixelWidth(i);
  else
   {
     xr+=fixedColumnPixelWidth();
     for (i=firstColumn();i<column_;i++) xr+=columnPixelWidth(i);
   }
  return xr;
}


void MSArrayView::redrawImmediately(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     redrawPixmap()->lock();
     XFillRectangle(display(),redrawPixmap()->pixmap(),backgroundShadowGC(),
		    0,0,panner()->width(),panner()->height());
     drawRows(redrawPixmap()->pixmap(),firstRow(),lastRow());
     drawHeadings(redrawPixmap()->pixmap());
     drawLabels(redrawPixmap()->pixmap(),firstRow(),lastRow());
     cleanUpBottom(redrawPixmap()->pixmap());
     int ht=panner()->highlightThickness();
     MSRect aRect(ht,ht,panner()->width()-2*ht,panner()->height()-2*ht);
     drawBevel(redrawPixmap()->pixmap(),
               aRect,panner()->shadowStyle(),panner()->shadowThickness()); 
     XCopyArea(display(),redrawPixmap()->pixmap(),panner()->window(),
	       backgroundShadowGC(),
	       0,0,panner()->width(),panner()->height(),0,0);
     moveSelection(selectedRow(),selectedColumn());
     updateScrollBars();
     XFlush(display());
     redrawPixmap()->unlock();
   }
  
}

void MSArrayView::drawCell(Window window_,int x_,int y_,int row_,int column_,
			   unsigned long foreground_,unsigned long background_,Font fid_,
			   MSBoolean rowSelected_,MSBoolean cellSelected_,MSBoolean cellValid_)
{
  int po=panner()->highlightThickness()+panner()->shadowThickness();	   
  const XFontStruct *fontStruct=columnFontStruct(column_);
  MSFontObject fontObj(fontStruct);
  if (fontObj.font()!=fid_) fontObj.fontStruct(server()->fontStruct(fid_));
  int cpw=columnPixelWidth(column_);
  int acpw=cpw-2*columnSpacing(); // available column width
  int fh=(needRowSep(row_)==MSTrue)?rowHeight()-rowSpacing():rowHeight();
  int ww=(needColSep(column_)==MSTrue)?cpw-columnSpacing():cpw;
  int twidth=fontObj.textWidth("*",1);
  int len=(twidth>0)?acpw/twidth:0;
  if (columns()==1&&x_+ww>panner()->width()-po) ww=panner()->width()-po-x_;
  XSetForeground(display(),backgroundGC(),background_);
  XFillRectangle(display(),window_,backgroundGC(),x_,y_,ww,fh);
  GC gc;
  if (column_==lastColumn()||(columns()<=fixedColumns()&&column_==columns()-1))
   {
     if (column_==numColumns()-1) gc=backgroundShadowGC();
     else 
      {
	gc=(rowSelected_==MSTrue)?selectionVectorGC():backgroundShadowGC();
      }
     XFillRectangle(display(),window_,gc,x_+cpw,y_,panner()->width()-po-(x_+cpw),fh);
   }
  MSString buffer;
  if (viewFormatOutput(buffer,row_,column_)!=0&&buffer.length()>0) 
   {
     int cplen=buffer.length();
     int w=fontObj.textWidth(buffer.string(),cplen);
     // the following is a temporary implementation of cell overflow which
     // displays *'s for a numeric column and truncates string columns.
     // This will not work if fonts are set on a cell basis.
     int xoff=0;
     XSetForeground(display(),textGC(),foreground_);
     XSetFont(display(),textGC(),fid_);
     int delta=(rowHeight()-2*rowSpacing()-fontObj.textHeight())>>1;
     if (cplen<=columnLength(column_)||w<=acpw)
      {
	MSAlignment alignment=viewCellAlignment(row_,column_);
	xoff=alignment==MSCenter?(cpw-w)/2:alignment==MSRight?acpw-w:0;
      }
     else if (columnClipMode(column_)==MSClipStars)
      {
	buffer=MSString::copy("*",len);
	cplen=len;
      }
     else 
      {
	int utw=fontObj.textWidth(buffer.string(),cplen);
	if (cplen>0&&utw>acpw) cplen=computeMaxTextLength(fontObj.fontStruct(),buffer.string(),acpw,cplen);
      }
     if (cellValid_==MSFalse)
      {
	XSetFillStyle(display(),textGC(),FillStippled);
        XSetTSOrigin(display(),textGC(),x_,y_);
      }
     else XSetFillStyle(display(),textGC(),FillSolid);
     int startx=x_+columnSpacing()+xoff;
     int starty=y_+fontObj.textAscent()+delta+rowSpacing();
     XDrawString(display(),window_,textGC(),fontObj.fontStruct(),startx,starty,
		 buffer.string(),cplen);
     if (cellValid_==MSFalse) XSetFillStyle(display(),textGC(),FillSolid);
   }
  if (cellSelected_==MSTrue)
   {
     MSRect aRect(x_,y_,cpw,rowHeight());
     drawSelectBevel(aRect,MSTrue);
   }
}

void MSArrayView::drawCell(Window window_,int x_,int y_,int row_,int column_)
{
  unsigned cl=columnLength(column_);
  unsigned nRows=columnNumRows(column_);
  if (frozen()==MSFalse&&cl!=0&&row_<nRows&&row_<numRows()&&column_<numColumns())
   {
     MSBoolean rowSelected=selected(row_);
     MSBoolean cellSelected=(row_==selectedRow()&&column_==selectedColumn())?MSTrue:MSFalse;
     unsigned long fg=viewCellForeground(row_,column_);
     unsigned long bg;
     if (cellSelected==MSTrue) bg=selectedCellBackground();
     else if (rowSelected==MSTrue) bg=selectedRowBackground();
     else bg=viewCellBackground(row_,column_);
     MSBoolean cellValid=isViewValid(row_,column_);
     Font fid=viewCellFont(row_,column_);
     drawCell(window_,x_,y_,row_,column_,fg,bg,fid,rowSelected,cellSelected,cellValid);
   }
}

void MSArrayView::cleanUpRight(Window window_)
{
  if (rows()>0)
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int x=computeXCoord(lastColumn()+1);
     int w=panner()->width()-offset-x;
     int rh=rowHeight();
     int rs=rowSpacing();
     int dh=rowHeight()-rowSpacing();
     int thickness=rowSpacing()>>1;
     int inc=rowSeparator();
  
     if (w>0&&thickness>0)
      {
        int i=firstRow();
        int y=computeYCoord(i);
        int n=0;
        int k=0;
        int nRows=numRows();
     
        if (inc>0&&lastColumn()>=numColumns()-1)
         {
           XRectangle *back=new XRectangle[rows()];
           for (;i<=lastRow()&&i<nRows;i++)
            {
              if ((i%inc)==(inc-1)||i==nRows-1)
               {
                 back[n].x=x;
                 back[n].y=y+dh;
                 back[n].width=w;
                 back[n].height=rowSpacing();
                 n++;
               }
              y+=rh;
            }
           if (n>0) XFillRectangles(display(),window_,backgroundShadowGC(),back,n);
           delete [] back;
         }
        else
         {
           XRectangle *top=new XRectangle[rows()];
           XRectangle *bottom=new XRectangle[rows()];	
           XRectangle *back=new XRectangle[rows()];	
           for (;i<=lastRow()&&i<nRows;i++)
            {
              if (inc>0&&((i%inc)==(inc-1)||i==nRows-1))
               {
                 dh=rh-rs;
                 top[k].x=bottom[k].x=x;
                 top[k].y=y+dh;
                 bottom[k].y=top[k].y+thickness;
                 top[k].width=bottom[k].width=w;
                 top[k].height=bottom[k].height=thickness;
                 k++;
               }
              else dh=rh;
              if (selected(i)==MSTrue)
               {
                 back[n].x=x;
                 back[n].y=y;
                 back[n].width=w;
                 back[n].height=dh;
                 n++;
               }
              else if (i==selectedRow()) XFillRectangle(display(),window_,selectionVectorGC(),x,y,w,dh);
              y+=rh;
            }
           if (k>0)
            {
              XFillRectangles(display(),window_,bottomShadowGC(),top,k);
              XFillRectangles(display(),window_,topShadowGC(),bottom,k);
            }
           if (n>0) XFillRectangles(display(),window_,selectionVectorGC(),back,n);
           delete [] top;
           delete [] bottom;
           delete [] back;
         }
      }
     if (lastColumn()==numColumns()-1&&inRowRange(selectedRow())==MSTrue)
      {
        int x=computeXCoord(lastColumn())+columnPixelWidth(lastColumn());
        int y=computeYCoord(selectedRow());
        int offset=panner()->highlightThickness()+panner()->shadowThickness();
        XFillRectangle(display(),window_,backgroundShadowGC(),
                       x,y,panner()->width()-offset-x,rowHeight());             
      }
   }
}

void MSArrayView::drawVSeparators(Window window_,int rs_,int re_,int cs_,int ce_)
{
  int inc=columnSeparator();
  if (inc>0&&mapped()!=MSFalse&&frozen()!=MSTrue) 
   {
     int thickness=columnSpacing()>>1;
     if (thickness>0)
      {
	int nRows=numRows();
	int nCols=numColumns();

	re_=(re_<nRows)?re_:nRows-1;
        ce_=(ce_<nCols)?ce_:nCols-1;
	int count=(ce_-cs_)+1;
	count=(count<columns())?count:columns();  
        if (count>=1)
	 {
	   XRectangle *top=new XRectangle[count];
	   XRectangle *bottom=new XRectangle[count];
	   int offset=panner()->highlightThickness()+panner()->shadowThickness();
           int cw=columnPixelWidth(cs_);
	   int x=computeXCoord(cs_)+cw;
	   int y=computeYCoord(rs_);
	   int h=0;
	   int n=0;
	   
	   if (re_==lastRow()&&re_!=nRows-1) h=panner()->height()-y-offset;
	   else
	    {
	      for (int r=rs_;r<=re_;r++) h+=rowHeight();
	      if (re_==nRows-1) h-=rowSpacing();
	    }
	   if (columns()==1&&x>panner()->width()-offset)x=panner()->width()-offset; 
	   for (int i=0;i<count;i++)  
	    {
              int c=cs_+i;
              if ((c%inc)==(inc-1)||c==nCols-1)
	       {
		 bottom[n].x=x-columnSpacing();
		 top[n].x=bottom[n].x+thickness;
		 bottom[n].y=top[n].y=y;
		 if (re_==nRows-1) 
		  { 
		    if (cs_+c==nCols-1)
		     {
		       bottom[n].height=h+thickness;
		       top[n].height=h+rowSpacing();
		     }
		    else bottom[n].height=top[n].height=h-rowSpacing();
		  }
		 bottom[n].height=top[n].height=h;
		 bottom[n].width=top[n].width=thickness;
		 n++;
	       }
	      x+=columnPixelWidth(c+1);
	    }
           if (n>0)
	    {
	      XBFillRectangles(display(),window_,bottomShadowGC(),bottom,n);
	      XFillRectangles(display(),window_,topShadowGC(),top,n);
            }
	   delete [] bottom;
	   delete [] top;
	 }
      }
   }
}

void MSArrayView::drawHSeparators(Window window_,int rs_,int re_,int cs_,int ce_)
{
  if (ce_>=cs_)
   {
     int inc=rowSeparator();
     if (mapped()!=MSFalse&&frozen()!=MSTrue&&inc>0) 
      {
        int thickness=rowSpacing()>>1;
        if (thickness>0)
         {
           int nRows=numRows();
           int nCols=numColumns();
           
           re_=(re_<nRows)?re_:nRows-1;
           ce_=(ce_<nCols)?ce_:nCols-1;
           int offset=panner()->highlightThickness()+panner()->shadowThickness();
           int y=computeYCoord(rs_);
           int x=computeXCoord(cs_);
           int count=re_-rs_+1;     
           XRectangle *top=new XRectangle[count];
           XRectangle *bottom=new XRectangle[count];
           int w=0,n=0;
           
           if (ce_==lastColumn()&&ce_!=numColumns()-1) w=panner()->width()-x-offset;
           else for (int c=cs_;c<=ce_;c++) w+=columnPixelWidth(c);
           w=(w<=panner()->width()-2*offset)?w:panner()->width()-2*offset;   
           
           for (int i=rs_;i<=re_;i++)
            {
              if ((i%inc)==(inc-1)||i==nRows-1)
               {
                 top[n].x=bottom[n].x=x;
                 top[n].y=y+rowHeight()-rowSpacing();
                 bottom[n].y=top[n].y+thickness;
                 if (i==nRows-1&&ce_==nCols-1)
                  {
                    top[n].width=w-thickness;
                    bottom[n].width=w; 
                  }
                 else top[n].width=bottom[n].width=w;
                 top[n].height=bottom[n].height=thickness;
                 n++;
               }
              y+=rowHeight();
            }
           if (n>0)
            {
              XBFillRectangles(display(),window_,panner()->bottomShadowGC(),top,n);
              XFillRectangles(display(),window_,panner()->topShadowGC(),bottom,n);
            }
           delete [] top;
           delete [] bottom;
         }
      }
   }
}

void MSArrayView::drawRow(int row_)
{ drawRow(panner()->window(),row_); }
void MSArrayView::drawRow(Window window_,int row_)
{ if (row_<numRows()&&inRowRange(row_)==MSTrue) drawRows(window_,row_,row_); }
void MSArrayView::drawRows(int rs_,int re_)
{ drawRows(panner()->window(),rs_,re_); }
void MSArrayView::drawRows(Window window_,int rs_,int re_)
{
  drawFixedColumns(window_,rs_,re_);
  drawRows(window_,rs_,re_,firstColumn(),lastColumn());
}
void MSArrayView::drawRows(int rs_,int re_,int cs_,int ce_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   { 
     rs_=(rs_>=firstRow())?rs_:firstRow();  
     re_=(re_<=lastRow())?re_:lastRow();
     cs_=(cs_>=firstColumn())?cs_:firstColumn();  
     ce_=(ce_<=lastColumn())?ce_:lastColumn();
     drawRows(panner()->window(),rs_,re_,cs_,ce_);
   }
}

void MSArrayView::drawRows(Window window_,int rs_,int re_,int cs_,int ce_)
{
  unsigned totalRows=numRows();
  if (totalRows>0&&mapped()==MSTrue&&frozen()==MSFalse)
   {
     MSRect aRect;
     for(int j=cs_;j<=ce_;j++)
      {
	unsigned cl=columnLength(j);
        if (cl!=0&&j<numColumns())
	 {
	   for (int i=rs_;i<=re_;i++)
	    {
	      int xx=computeXCoord(j);
	      int yy=computeYCoord(i);
	      int nRows=columnNumRows(j);
	      if (i<nRows&&i<totalRows)
	       {
		 MSBoolean rowSelected=selected(i);
		 MSBoolean cellSelected=(i==selectedRow()&&j==selectedColumn())?MSTrue:MSFalse;	
		 MSBoolean cellValid=isViewValid(i,j);
		 unsigned long bg,fg=viewCellForeground(i,j);
                 Font fid=viewCellFont(i,j);
		 if (cellSelected==MSTrue)
		  {
		    aRect.configuration(xx,yy,columnPixelWidth(j),rowHeight());
		    bg=selectedCellBackground();
		  }
		 else if (rowSelected==MSTrue) bg=selectedRowBackground();
		 else bg=viewCellBackground(i,j);
		 drawCell(window_,xx,yy,i,j,fg,bg,fid,rowSelected,cellSelected,cellValid);
	       }
	    }
	 }
      }
     drawHSeparators(window_,rs_,re_,cs_,ce_);
     drawVSeparators(window_,rs_,re_,cs_,ce_);
     if (aRect.width()>0) drawSelectBevel(aRect,MSTrue);
   }
}

void MSArrayView::selectedCellBackground(const char *color_)
{ selectedCellBackground(server()->pixel(color_)); }

void MSArrayView::selectedCellBackground(unsigned long pixel_) 
{ 
  if (selectedCellBackground()!=pixel_)
   { 
     _selectBg=pixel_; 
     drawSelectedCell(panner()->window(),selectedRow(),selectedColumn());
   }
}



void MSArrayView::drawCycle(MSColorCycle *cycle_)
{
  int row=cycle_->row();
  int col=cycle_->column();
  MSCycleColorMode mode=cycle_->mode();
  unsigned long color=cycle_->color(cycle_->count());
  int x,y;
  if ((row==-1||row<numRows())&&(col==-1||col<numColumns()))
   {
     if (inRowRange(row)==MSTrue)
      {
	if (inColRange(col)==MSTrue)
	 {
	   x=computeXCoord(col);
	   y=computeYCoord(row);
	   drawCycle(x,y,row,col,color,mode);
	}  
	else if (col==-1)
	 {
	   x=computeXCoord(firstColumn());
	   y=computeYCoord(row);
	   for (int j=firstColumn();j<=lastColumn();j++)
	    {
	      drawCycle(x,y,row,j,color,mode);
	      x+=columnPixelWidth(j);
	    }
	 }
      }
     else if (inColRange(col)==MSTrue&&row==-1)
      { 
	x=computeXCoord(col);
	y=computeYCoord(firstRow());
	for (int i=firstRow();i<=lastRow();i++)
	 {
	   drawCycle(x,y,i,col,color,mode);
	   y+=rowHeight();
	 }
      }
   }
}

void MSArrayView::drawCycle(int x_,int y_,int row_,int column_,unsigned long color_,MSCycleColorMode mode_)
{
  unsigned cl=columnLength(column_);
  unsigned nRows=columnNumRows(column_);
  if (cl!=0&&row_<nRows&&row_<numRows()&&column_<numColumns())
   {
     MSBoolean rowSelected=selected(row_);
     MSBoolean cellSelected=(row_==selectedRow()&&column_==selectedColumn())?MSTrue:MSFalse;
     MSBoolean cellValid=isViewValid(row_,column_);
     Font fid=viewCellFont(row_,column_);
     unsigned long fg;
     unsigned long bg;
     if (mode_==MSReverseVideo)
      {
	fg=viewCellBackground(row_,column_);
	bg=viewCellForeground(row_,column_);
      }
     else
      {
	fg=(mode_==MSForeground)?color_:viewCellForeground(row_,column_);     
	if (mode_==MSBackground) bg=color_;
	else if (cellSelected==MSTrue) bg=selectedCellBackground();
	else if (rowSelected==MSTrue) bg=selectedRowBackground();
	else bg=viewCellBackground(row_,column_);
      }
     drawCell(panner()->window(),x_,y_,row_,column_,fg,bg,fid,rowSelected,cellSelected,cellValid);
   }
}

void MSArrayView::cycleRowColumn(int row_,int column_) 
{ createCycle(row_,column_); }
void MSArrayView::cycleRow(int row_)             
{ createCycle(row_,-1); }
void MSArrayView::cycleColumn(int column_)             
{ createCycle(-1,column_); }

void MSArrayView::cycleCell(MSColorCycle *cycle_)
{
  if (cycle_->count()==cycle_->numCycles()) 
   {
     if (cycle_->column()==-1) drawRow(cycle_->row());
     else if (cycle_->row()==-1) drawColumn(cycle_->column());
     else drawRowColumn(cycle_->row(),cycle_->column());
   }
  else drawCycle(cycle_);
}

int MSArrayView::computeMaxTextLength(const XFontStruct *fs_,const char *pString_,int cw_,int len_)
{
  if (cw_>0&&len_>0)
   {
     int len=len_;
     int tw=XTextWidth(fs_,pString_,len);
     MSFontObject fontObject(fs_);
     int decrement=(fontObject.doubleByte()==MSTrue?2:1);
     if (tw>cw_) 
      {
        while (len>0&&tw>cw_) 
         {
           len-=decrement;
	   tw-=XTextWidth(fs_,pString_+len,decrement);
	 }
      }
     return len;
   }
  return 0;
}


void MSArrayView::focusIn(void) 
{ 
  highlight(); 
  if (editor()->mapped()==MSTrue) focusInNotify(editor()); 
}
void MSArrayView::focusOut(void) 
{ 
  unHighlight(); 
  if (editor()->mapped()==MSTrue) focusOutNotify(editor()); 
}

MSBoolean MSArrayView::loseFocus(void) 
{
  if (editor()->mapped()==MSTrue) editorActivate(); 
  if (editor()->mapped()==MSTrue) return MSFalse;
  else 
   { 
     unHighlight(); 
     return MSTrue; 
   }
}

void MSArrayView::set(MSAttrValueList& avList_)
{
  MSRowColumnView::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="selectedCellBackground")
      {
	selectedCellBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="rowSeparator")
      {
	rowSeparator(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="columnSeparator")
      {
	columnSeparator(avList_[i].value().asInt());
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSArrayView::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("selectedCellBackground",
		       server()->colorName(selectedCellBackground()),
		       MSAttrValue::Color);
  avList_<<MSAttrValue("columnSeparator",MSString(columnSeparator()));
  avList_<<MSAttrValue("rowSeparator",MSString(rowSeparator()));
  avList_<<MSAttrValue("columnselection","",MSAttrValue::Callback);
  avList_<<MSAttrValue("columnmenubutton","",MSAttrValue::Callback);
  avList_<<MSAttrValue("rowselection","",MSAttrValue::Callback);

  return MSRowColumnView::get(avList_);
}

void MSArrayView::trackSelection(int startRow_,int startCol_,MSBoolean clear_,MSBoolean shifted_)
{
  Window root,child;
  int rx=0,ry=0,ix=0,iy=0;
  unsigned keys=0;
  int i,row,fr,lr;
  unsigned int index;
  int lastSelectedRow=startRow_;
  int lastSelectedCol=startCol_;
  MSIndexVector oldsel=selectionVector();
  int previousSelectedRow=selectedRow();
  int previousSelectedCol=selectedColumn();

  if (clear_==MSTrue) clearSelection();
  moveSelection(-1,-1);
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
  for(i=fr;i<=lr;i++)
   {
     if (selected(i)==MSTrue||(i>=startSelect&&i<=endSelect)) drawnVector<<i;
   }

  int sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while (keys&Button1Mask)
   {
     if (sameScreen==True)
      {
	ix-=panner()->x_origin();
	iy-=panner()->y_origin();
	lastSelectedCol=(ix<(fixedColumnPixelWidth()+labelWidth()))?
	xToColumn(ix-labelWidth()):
	xToColumn(ix-labelWidth())+firstColumn();
	if (inColRange(lastSelectedCol)==MSFalse) lastSelectedCol=lastColumn();
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
		 // If it is selected but not in our Drawn Vector
		 // draw it.
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

  if (lastSelectedRow!=previousSelectedRow||lastSelectedCol!=previousSelectedCol)
     selectedRowColumn(lastSelectedRow,lastSelectedCol);
  else if (oldsel.length()!=selectionVector().length()|| oldsel!=selectionVector()) 
   {
     moveSelection(lastSelectedRow,lastSelectedCol);
     rowColumnSelection();
   }
  else moveSelection(lastSelectedRow,lastSelectedCol);
}

void MSArrayView::trackUnselection(int startRow_,int)
{
  Window root,child;
  int rx=0,ry=0,ix=0,iy=0;
  unsigned keys=0;
  int i,row,fr,lr;
  unsigned int index;
  int lastSelectedRow=startRow_;
  int startSelect,endSelect;
  MSIndexVector undrawnVector; //Vector to keep track of which line is already drawn unselected

  moveSelection(-1,-1);
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
		 // If it is selected but in our UnDrawn Vector
		 // draw it.
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
  selectedRowColumn(-1,-1);
  rowColumnSelection();
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSArrayView::drawHeadings(Window) {}  
void MSArrayView::drawHeadings(Window,int,int) {}
void MSArrayView::redrawHeadings(Window window_,int cs_,int ce_)
{ drawHeadings(window_,cs_,ce_);}

void MSArrayView::drawLabels(int rs_,int re_)
{ drawLabels(panner()->window(),rs_,re_); }
void MSArrayView::drawLabels(Window,int,int) {}
void MSArrayView::updateInternalState(void)
{}

void MSArrayView::calculateRowHeight(void)
{
  MSFontObject fontObj;
  int            th=0;
  unsigned       n=numColumns();
  for (unsigned i=0;i<n;i++)
   {
     fontObj.fontStruct(columnFontStruct(i));
     th=MSUtil::max(th,fontObj.textHeight());
   }
  if (th==0) th=textHeight();
  th=(th>0)?th+(rowSpacing()<<1):0;
  if (th!=rowHeight()) rowHeight(th);
}


void MSArrayView::drawFixedColumns(Window window_,int rs_,int re_)
{ if (fixedColumns()>0) drawRows(window_,rs_,re_,0,fixedColumns()-1); }


const XFontStruct *MSArrayView::columnFontStruct(unsigned)
{ return (XFontStruct *) textFontStruct(); }

unsigned MSArrayView::columnNumRows(unsigned) const
{ return numRows(); }

unsigned long MSArrayView::columnBackground(unsigned)
{ return background(); }

unsigned long MSArrayView::columnForeground(unsigned)
{ return foreground(); }

MSClipMode MSArrayView::columnClipMode(unsigned) const
{ return MSNoClipping; }

int MSArrayView::fixedColumnPixelWidth(void)
{
  int sum=0;
  for (unsigned i=0;i<fixedColumns();i++) sum+=columnPixelWidth(i);
  return sum;
}

int MSArrayView::xToColumn(int x_)
{
  int col=0;
  if (x_<fixedColumnPixelWidth()) 
   {
     int w=columnPixelWidth(0);
     unsigned i=0;
     while(w<=x_&&i<fixedColumns())
      {
        i++;
        w+=columnPixelWidth(i);
      }
     col=(i<fixedColumns())?i:fixedColumns()-1;
   }
  else if (x_>panner()->width()) col=columns();
  else
   {
     int w=fixedColumnPixelWidth()+columnPixelWidth(firstColumn());
     int i=firstColumn();
     while(w<=x_&&i<=lastColumn())
      {
        i++;
        w+=columnPixelWidth(i);
      }
     col=i-firstColumn();
   }
  return (col>=0)?col:0;
}

int MSArrayView::inColRange(int c_) 
{ return((c_>=firstColumn()&&c_<=lastColumn())||(c_>=0&&c_<=fixedColumns()-1))?MSTrue:MSFalse; }

int MSArrayView::columnFromEvent(const XEvent *pEvent_)
{
  int col=(pEvent_->xbutton.x<(fixedColumnPixelWidth()+labelWidth()))?
          xToColumn(pEvent_->xbutton.x-labelWidth()):
          xToColumn(pEvent_->xbutton.x-labelWidth())+firstColumn();
  return col;
}

int MSArrayView::lastColumn(void) 
{ return firstColumn()+columns()-fixedColumns()-1;}

void MSArrayView::updateHsb(void)
{
  hsb()->min(fixedColumns());
  hsb()->max(numColumns());
  hsb()->valueChange(firstColumn());
  hsb()->viewSize(columns()-fixedColumns());
  hsb()->pageInc(columns()-fixedColumns()-1);
  if (isHsbEnabled()==MSTrue)
   {
     if (columns()<numColumns()&&hsb()->height()>1)
      {
	int fw=fixedColumnPixelWidth();
	hsb()->moveTo(panner()->x_origin()+fw+labelWidth(),
		      panner()->y_origin()+panner()->height()+spacing());
	hsb()->width(panner()->width()-fw-labelWidth());
	if (hsb()->mapped()==MSFalse) hsb()->map();
      }
     else if (hsb()->mapped()==MSTrue) hsb()->unmap();
   }
}

void MSArrayView::hsbValueUpdate(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (hsb()->value()<firstColumn()) scrollLeft(firstColumn()-hsb()->value());     
     else if (hsb()->value()>firstColumn())
      {
        int delta=hsb()->value()-firstColumn();
	int offset=panner()->highlightThickness()+panner()->shadowThickness();
	int deltaWidth,i;
	if (hsb()->value()+hsb()->viewSize()==hsb()->max())
	 {
	   deltaWidth=panner()->width()-2*offset-fixedColumnPixelWidth()-labelWidth();	
	   i=numColumns();
	   while (deltaWidth>0&&i>fixedColumns())
	    {
	      deltaWidth-=columnPixelWidth(i-1);
	      if (deltaWidth>=0) i--;
	    }
	   if (i<numColumns())
	    {
	      _firstColumn=i;
	      _firstColumn=(firstColumn()>=fixedColumns())?firstColumn():fixedColumns();
	      _columns=computeNumVisibleColumns();     
	      updateView();
	      firstColumnChangeNotify();
	      redrawImmediately();
	    }
	 }
	else if (delta>=hsb()->pageInc()) // page the screen
	 {
           _firstColumn+=delta;
	   _columns=computeNumVisibleColumns();     
           deltaWidth=panner()->width()-2*offset-drawWidth(); // any left over space
	   i=firstColumn();
	   while (deltaWidth>0&&i>0)
	    {
	      deltaWidth-=columnPixelWidth(i-1);
	      if (deltaWidth>=0) i--;
	    }
	   _firstColumn=i;
	   _firstColumn=(firstColumn()>=fixedColumns())?firstColumn():fixedColumns();
	   _columns=computeNumVisibleColumns();     
	   updateView();
	   firstColumnChangeNotify();     
	   redrawImmediately();
	 }
	else scrollRight(delta);
      }
   }
  else updateScrollBars();
}

void MSArrayView::updateView(void)
{
  //This method update the Horizontal scrollbar states
  hsb()->view(firstColumn(),columns()-fixedColumns());	   
  hsb()->pageInc(columns()-fixedColumns()-1);
}

void MSArrayView::cleanUpBottom(Window window_)
{
  if (columns()>0)
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int y=computeYCoord(firstRow()+rows()-1)+rowHeight();
     int h=panner()->height()-offset-y;
     int thickness=columnSpacing()>>1;
     int inc=columnSeparator();
  
     if (h>0&&thickness>0)
      {
        if (firstRow()+rows()>=numRows())
         {
           XFillRectangle(display(),window_,backgroundShadowGC(),
                          offset,y,panner()->width()-(offset<<1),h);
         }
        else
         {
           XRectangle *top=new XRectangle[columns()];
           XRectangle *bottom=new XRectangle[columns()];
           int cw,x;
           int i,n=0;
           int nCols=numColumns();
           if (fixedColumns()>0)
            {
              x=offset+labelWidth();
              for (i=0;i<fixedColumns()&&i<nCols;i++)
               {
                 cw=columnPixelWidth(i);
                 GC gc;
                 if (lastRow()>=columnNumRows(i)-1) gc=backgroundShadowGC();
                 else
                  {
                    gc=backgroundGC();
                    XSetForeground(display(),gc,columnBackground(i));
                  }
                 XFillRectangle(display(),window_,gc,x,y,cw,h);
                 x+=cw;
                 if (inc>0&&((i%inc)==(inc-1)||i==nCols-1))
                  {
                    top[n].x=x-columnSpacing();
                    bottom[n].x=top[n].x+thickness;
                    top[n].y=bottom[n].y=y;
                    top[n].width=bottom[n].width=thickness;
                    top[n].height=bottom[n].height=h;    
                    n++;
                  }
               }
            }
           x=computeXCoord(firstColumn());
           for (i=firstColumn();i<=lastColumn()&&i<nCols;i++)
            {
              cw=columnPixelWidth(i);
              GC gc;
              if (lastRow()>=columnNumRows(i)-1) gc=backgroundShadowGC();
              else
               {
                 gc=backgroundGC();
                 XSetForeground(display(),gc,columnBackground(i));
               }
              XFillRectangle(display(),window_,gc,x,y,cw,h);
              x+=cw;
              if (inc>0&&((i%inc)==(inc-1)||i==nCols-1))
               {
                 top[n].x=x-columnSpacing();
                 bottom[n].x=top[n].x+thickness;
                 top[n].y=bottom[n].y=y;
                 top[n].width=bottom[n].width=thickness;
                 top[n].height=bottom[n].height=h;    
                 n++;
               }
            }
           if (n>0)
            {
              XFillRectangles(display(),window_,bottomShadowGC(),top,n);
              XFillRectangles(display(),window_,topShadowGC(),bottom,n);
            }
           if (x<panner()->width()-offset) 
         XFillRectangle(display(),window_,backgroundShadowGC(),
                        x,y,panner()->width()-x-offset,h);
           delete [] top;
           delete [] bottom;
         }
      }
   }
}

void MSArrayView::drawSelected(int row_)
{
  int cs=firstColumn();
  int ce=lastColumn();
  int fixedCols=fixedColumns();
  int nCols=numColumns();
  int yy=computeYCoord(row_);
  unsigned long bg=selectedRowBackground();
  MSBoolean rowSelected=MSTrue;
  MSBoolean cellSelected=MSFalse;
  
  if (fixedCols>0)
   {
     int nToDraw;
     if (fixedCols<nCols) nToDraw=fixedCols;
     else nToDraw=nCols;
     for (int i=0;i<nToDraw;i++)
      {
	int xx=computeXCoord(i);
	unsigned long fg=viewCellForeground(row_,i);
	MSBoolean cellValid=isViewValid(row_,i);
        Font fid=viewCellFont(row_,i);
	drawCell(panner()->window(),xx,yy,row_,i,fg,bg,fid,rowSelected,cellSelected,cellValid);
      }
     drawHSeparators(panner()->window(),row_,row_,0,fixedCols-1);
     drawVSeparators(panner()->window(),row_,row_,0,fixedCols-1);
   }
  
  if (cs<nCols)
   {
     int endIndex;
     if (ce<nCols) endIndex=ce;
     else endIndex=nCols-1;

     for (int i=cs;i<=endIndex;i++)
      {
	int xx=computeXCoord(i);
	unsigned long fg=viewCellForeground(row_,i);
	MSBoolean cellValid=isViewValid(row_,i);
        Font fid=viewCellFont(row_,i);
	drawCell(panner()->window(),xx,yy,row_,i,fg,bg,fid,rowSelected,cellSelected,cellValid);
      }
     drawHSeparators(panner()->window(),row_,row_,cs,endIndex);
     drawVSeparators(panner()->window(),row_,row_,cs,endIndex);
   } 
}

void MSArrayView::undrawSelected(int row_)
{
int cs=firstColumn();
 int ce=lastColumn();
 int fixedCols=fixedColumns();
 int nCols=numColumns();
 int yy=computeYCoord(row_);
 MSBoolean rowSelected=MSFalse;
 MSBoolean cellSelected=MSFalse;
 
 if (fixedCols>0)
  {
    int nToDraw;
    if (fixedCols<nCols) nToDraw=fixedCols;
    else nToDraw=nCols;
     for (int i=0;i<nToDraw;i++)
      {
	int xx=computeXCoord(i);
	unsigned long fg=viewCellForeground(row_,i);
	unsigned long bg=viewCellBackground(row_,i);
	MSBoolean cellValid=isViewValid(row_,i);
        Font fid=viewCellFont(row_,i);
	drawCell(panner()->window(),xx,yy,row_,i,fg,bg,fid,rowSelected,cellSelected,cellValid);
      }
     drawHSeparators(panner()->window(),row_,row_,0,fixedCols-1);
     drawVSeparators(panner()->window(),row_,row_,0,fixedCols-1);
  }
 
 if (cs<nCols)
  {
    int endIndex;
    if (ce<nCols) endIndex=ce;
    else endIndex=nCols-1;

    for (int i=cs;i<=endIndex;i++)
     {
       int xx=computeXCoord(i);
       unsigned long fg=viewCellForeground(row_,i);
       unsigned long bg=viewCellBackground(row_,i);
       MSBoolean cellValid=isViewValid(row_,i);
       Font fid=viewCellFont(row_,i);
       drawCell(panner()->window(),xx,yy,row_,i,fg,bg,fid,rowSelected,cellSelected,cellValid);
     }
    drawHSeparators(panner()->window(),row_,row_,cs,endIndex);
    drawVSeparators(panner()->window(),row_,row_,cs,endIndex);
  }
}

void MSArrayView::selectedRowColumn(int row_,int column_) 
{
  if (row_!=selectedRow()||column_!=selectedColumn()) 
   {
     setSelection(row_,column_);
     rowColumnSelection();
   }
}

void MSArrayView::selectedColumn(int column_)
{ updateSelectedColumn(column_); }

void MSArrayView::updateSelectedColumn(int column_)
{
  int col=selectedColumn();
  if (column_>=fixedColumns()&&column_<numColumns())
   {
     if (inColRange(column_)==MSTrue) moveSelection(selectedRow(),column_);
     else if (column_>lastColumn())
      {
        _selectedColumn=column_;        
        undrawSelectedCell(panner()->window(),selectedRow(),col);
        scrollRight(column_-(firstColumn()+columns()-fixedColumns())+1);
        drawSelectedCell(panner()->window(),selectedRow(),selectedColumn());
      }
     else if (column_<firstColumn())
      {
        _selectedColumn=column_;
        undrawSelectedCell(panner()->window(),selectedRow(),col);
        scrollLeft(firstColumn()-column_);
        drawSelectedCell(panner()->window(),selectedRow(),selectedColumn());
      }
   }
  else if (column_<0) moveSelection(-1,-1);
  else if (column_<fixedColumns())
   {
     if (inColRange(column_)==MSTrue) moveSelection(selectedRow(),column_);
   }
  if (col!=selectedColumn()&&selectedColumn()!=-1) rowColumnSelection();
}

void MSArrayView::updateFirstColumn(int column_)
{
  if (column_<0) column_=0;
  else if (column_>=numColumns()) column_=numColumns()-1;   

  if (column_>=fixedColumns()&&column_<numColumns()&&column_!=firstColumn())
   {
     _firstColumn=column_;
     unsigned oldFirstColumn=firstColumn();
     _columns=computeNumVisibleColumns();
     adjustFirstColumn();
     redrawImmediately();
     if (selectedColumn()>=0&&selectedColumn()>=numColumns()) selectedColumn(numColumns()-1);
     if (oldFirstColumn==firstColumn()) firstColumnChangeNotify();     // otherwise called in adjustFirstColumn
   }
}

unsigned MSArrayView::columnEditWidth(unsigned)
{ return MSArrayViewColumnDefaultEditWidth; }

void MSArrayView::moveEditorToSelection(const MSString &aString_)
{
  if (selectedRow()<numRows()&&selectedColumn()<numColumns()&& 
      inRowRange(selectedRow())==MSTrue&&inColRange(selectedColumn())==MSTrue)
   {
     if (sensitive()==MSTrue)
      {
	if (selectedRow()<columnNumRows(selectedColumn()))
         {
	   if (isViewProtected(selectedRow(),selectedColumn())!=MSTrue)
	    {
              int x=computeXCoord(selectedColumn())+panner()->x_origin();
	      int y=computeYCoord(selectedRow())+panner()->y_origin();
	      int w=columnPixelWidth(selectedColumn());
	      int offset=panner()->shadowThickness()+panner()->highlightThickness();
	      if (w>panner()->width()-2*offset) w=panner()->width()-2*offset;
	      Font fid=viewCellFont(selectedRow(),selectedColumn());
	      editor()->maxLength(columnEditWidth(selectedColumn()));
	      editor()->font(fid);
	      editor()->moveTo(x,y);
	      editor()->resize(w,rowHeight()); 
	      if (aString_.length()>0)
	       {
		 MSString aString2(aString_);
		 aString2.stripTrailing();
		 editor()->string(aString2);
	       }
              else
	       {
		 editor()->string(aString_);
		 editor()->editMode(MSTextField::InsertMode);
	       }
              mapEditor();
	    }
           else server()->bell();
	 }
      }
     else server()->bell();
   }
}


void MSArrayView::startEditing(const XEvent *pEvent_)
{
  MSString string=selection();
  moveEditorToSelection(string);
  if (editor()->mapped()==MSTrue)
   {
     unsigned cl=columnLength(selectedColumn());
     if (cl!=0)
      {
	int xoff=0;
	MSAlignment alignment=viewCellAlignment(selectedRow(),selectedColumn());
	if (alignment!=MSLeft)
	 {
	   Font fid=viewCellFont(selectedRow(),selectedColumn());
	   const XFontStruct *fontStruct=columnFontStruct(selectedColumn());
	   MSFontObject fontObj(fontStruct);
	   if (fontObj.font()!=fid) fontObj.fontStruct(server()->fontStruct(fid));
	   int cpw=columnPixelWidth(selectedColumn());
	   int slen=string.length();
	   int w=fontObj.textWidth(string,slen);
	   xoff=alignment==MSCenter?(cpw-w)/2:
	        alignment==MSRight?cpw-2*columnSpacing()-w:0;
	   if (xoff<0) xoff=0;
	 }
        // We used to call buttonPressNotify and pass the event data structure 
        // to the text field.  After adding the support for range selection in
        // text field, there's some additional logic in MSTextField::button1Press
        // (MSTextField::trackSelection() in specific) that we don't want to trigger,
        // so we are going to call MSTextField::startEditing() directly here.

        _editor->cursorPosition(_editor->firstCursorPosition());
        MSTextField::EditingMode mode;
        if (pEvent_->xbutton.button==Button2) mode=MSTextField::OverstrikeMode;
        else mode=MSTextField::InsertMode;
        editor()->startEditing(mode,pEvent_->xbutton.x-editor()->x_origin()-xoff);
      }
   }
}

void MSArrayView::clearRows(int rs_,int re_)
{ clearRows(rs_,re_,firstColumn(),lastColumn()); }

void MSArrayView::clearRowColumn(int row_,int column_)
{
  if (frozen()==MSFalse)
   {
     if (inRowRange(row_)==MSTrue&&inColRange(column_)==MSTrue)
      {
	int y=computeYCoord(row_);
	int x=computeXCoord(column_);
	XFillRectangle(display(),panner()->window(),panner()->backgroundShadowGC(),
		       x,y,columnPixelWidth(column_),rowHeight());
      }
   }
}

void MSArrayView::clearRows(int rs_,int re_,int cs_,int ce_)
{
  if (frozen()==MSFalse)
   {
     if (inRowRange(rs_)==MSTrue&&inColRange(cs_)==MSTrue)
      {
	re_=(re_<=lastRow())?re_:lastRow();
	ce_=(ce_<=lastColumn())?ce_:lastColumn();
	
	int i=0,j=0;
	int x=0,y=0;
	y=computeYCoord(rs_);
	for (i=rs_;i<=re_;i++)
	 {
	   x=computeXCoord(cs_);
	   for (j=cs_;j<=ce_;j++)
	    {
	      XFillRectangle(display(),panner()->window(),backgroundShadowGC(),
			     x,y,columnPixelWidth(j),rowHeight());
	      x+=columnPixelWidth(j);
	    }
	   y+=rowHeight();
	 }
	panner()->redraw();
      }
   }
}

void MSArrayView::clearRow(int row_)
{
  int offset=panner()->highlightThickness()+panner()->shadowThickness();
  int w=panner()->width()-(offset<<1);

  XFillRectangle(display(),panner()->window(),backgroundShadowGC(),
	         offset,computeYCoord(row_),w,rowHeight());
}

void MSArrayView::createCycle(int row_,int column_)
{
  if (numRows()>0) 
   {
     if (cycleList().length()!=0) processCycleTimer();
     if (inRowRange(row_)==MSTrue&&inColRange(column_)==MSTrue)
      {
	if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
	   startCycle(row_,column_,cycleColors(),cycleColorMode());
	else drawRowColumn(row_,column_);
      }
     else if (row_==-1&&inColRange(column_)==MSTrue)
      {
        if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
	   startCycle(row_,column_,cycleColors(),cycleColorMode());
	else drawColumn(column_);
      }
     else if (column_==-1&&inRowRange(row_)==MSTrue)
      {
        if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
	   startCycle(row_,column_,cycleColors(),cycleColorMode());
	else drawRow(row_);
      }
     if (cycleList().length()!=0)
      {
        if (cycleTimer()==0) _cycleTimer=new CycleTimer(this,cycleInterval());
        else cycleTimer()->reset();
      }
   }
}

void MSArrayView::updateSelectionVector(const MSIndexVector& sv_)
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
     moveSelection(-1,-1);
   }
}

void MSArrayView::updateSensitivity(void)
{
  MSRowColumnView::updateSensitivity();
  if(firstMap()==MSTrue&&frozen()==MSFalse)
    {
      redrawImmediately();
    }
}
void MSArrayView::rowSeparator(int interval_) 
{ if (rowSeparator()!=interval_) { _rowSeparator=interval_; redrawImmediately(); } }

void MSArrayView::columnSeparator(int cs_) 
{ if (columnSeparator()!=cs_) { _columnSeparator=cs_; redrawImmediately(); } }

void MSArrayView::fixedColumns(int x_)
{
  if (x_!=fixedColumns()) 
   { 
     _fixedColumns=x_;
     adjustFirstColumn();
     if (frozen()==MSFalse) placement();
     redrawImmediately();
   }
} 

void MSArrayView::columnWidth(int cw_) 
{ if (cw_!=_columnWidth) { _columnWidth=cw_;adjustNumVisible();redrawImmediately();}}


int MSArrayView::columnHeadingsOffset(void)
{
  return panner()->highlightThickness()+panner()->shadowThickness();
}

