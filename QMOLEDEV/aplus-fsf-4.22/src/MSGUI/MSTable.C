///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <MSTypes/MSUtil.H>
#include <MSGUI/MSTable.H>
#include <MSGUI/MSBusy.H>
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSTableColumnGroup.H>
#include <MSTypes/MSMessageLog.H>

#ifdef MS_NO_INLINES
#include <MSGUI/MSTableInlines.C>
#endif

static const unsigned MSTableDefaultColumnWidth=9;
static const int MSTableDefaultDragBorder=2;
static const unsigned long MSTableEventMask=(ButtonPressMask|ButtonReleaseMask|ExposureMask);

static const int MSTableOptionMenuSymbolHeight=8;
static const int MSTableOptionMenuSymbolWidth=12;
static const int MSTableOptionMenuSymbolThickness=2;
static const int MSTableOptionMenuMargin=5;


class MSTableGroupIterator : public MSTableColumnGroup::ConstIterator
{
protected:
  MSTable::ColumnGroupList &_tableGroupList;
public:
  MSTableGroupIterator(MSTable::ColumnGroupList &groupList_)
      : _tableGroupList(groupList_)
  {}
  virtual MSBoolean applyTo(const MSTableColumn *,const MSTableColumnGroup::ColumnGroupList &){return MSTrue;}
  virtual MSBoolean applyTo(const MSTableColumnGroup &,const MSTableColumnGroup::ColumnGroupList &);
};

MSBoolean MSTableGroupIterator::applyTo(const MSTableColumnGroup &group_,const MSTableColumnGroup::ColumnGroupList &)
{
  MSManagedPointer<MSTableColumnGroup> newGroup(new MSTableColumnGroup(group_.table()),MSInit);
  newGroup->shallowCopy(group_);
  _tableGroupList<<newGroup;
  return MSTrue;
}

class MSTableColumnIterator : public MSTableColumnGroup::Iterator
{
protected:
  MSTable::ColumnGroupList &_tableGroupList;
  MSWidgetVector &_vector;

public:
  MSTableColumnIterator(MSWidgetVector &vector_,MSTable::ColumnGroupList &tableGroupList_)
      : _vector(vector_),_tableGroupList(tableGroupList_)
  {}
  virtual MSBoolean applyTo(MSTableColumn *,const MSTableColumnGroup::ColumnGroupList &);
  virtual MSBoolean applyTo(MSTableColumnGroup &,const MSTableColumnGroup::ColumnGroupList &){return MSTrue;}
};

MSBoolean MSTableColumnIterator::applyTo(MSTableColumn *column_,const MSTableColumnGroup::ColumnGroupList &groupList_)
{
  unsigned len1=groupList_.length();
  unsigned len2=_tableGroupList.length();
  for (unsigned i=0;i<len1;i++)
   {
     const MSTableColumnGroup *colgroup=groupList_(i);
     for (unsigned j=0;j<len2;j++)
      {
        const MSManagedPointer<MSTableColumnGroup> &group=_tableGroupList(j);
        if (group->shallowCompare(*colgroup)==MSTrue)
         {
           column_->groupList()<<group;
           break;
         }
      }
   }
  _vector<<(MSWidget *)column_;
  return MSTrue;
}

class MSTableSanityCheck : public MSTableColumnGroup::ConstIterator
{
protected:
  const MSWidgetVector &_columns;
  MSBoolean &_sanity;
public:
  MSTableSanityCheck(const MSWidgetVector &columns_,MSBoolean &sanity_)
      :_columns(columns_),_sanity(sanity_)
  {
    _sanity=MSTrue;
  }
  virtual MSBoolean applyTo(const MSTableColumn *,const MSTableColumnGroup::ColumnGroupList &);
  virtual MSBoolean applyTo(const MSTableColumnGroup &,const MSTableColumnGroup::ColumnGroupList &) {return MSTrue;}
};

MSBoolean MSTableSanityCheck::applyTo(const MSTableColumn *col_,const MSTableColumnGroup::ColumnGroupList &)
{
  if (_columns.indexOf((unsigned long)(MSWidget *)col_)==_columns.length())
   {
     _sanity=MSFalse;
     return MSFalse;
   }
  else return MSTrue;
}

void MSTable::ColumnPopupMenu::activate(void)
{
  int si=selectedItem();
  done();
  _table->columnChoiceActivate(_choices(si));
}

MSTable::MSTable(MSWidget *owner_,const char *title_) :
MSReportTable(title_),MSArrayView(owner_,title_)
{ init(); }

MSTable::MSTable(MSWidget *owner_,const MSStringVector& title_) :
MSReportTable(title_),MSArrayView(owner_,title_)
{ init(); }

GC MSTable::moveGC(void)                     
{ return _movegc.gc(); }

void MSTable::init(void)
{
  unsigned long whitePixel=server()->pixel("white");
  unsigned long blackPixel=server()->pixel("black");
  _columnHeadingsHeight=0;
  _groupHeadingsHeight=0;
  _choicesMenu=0;
  _headingFont=font();
  _headingAlignment=MSCenter;
  _headingForeground=server()->defaultForeground();
  _headingsHeight=rowHeight();
  _columnDragDrop=MSTrue;
  _columnResize=MSTrue;
  _showBreaks=MSFalse;
  _dynamicRecompute=MSFalse;
  _dragCursor=new MSDisplayCursor(server(),XC_sb_h_double_arrow,blackPixel,whitePixel);
  _resizeCursor=new MSDisplayCursor(server(),XC_right_side,blackPixel,whitePixel);

  _choiceStyle=ChoicePopupMenu;

  XSetWindowAttributes attributes;
  attributes.save_under=True;
  attributes.override_redirect=True;
  attributes.backing_store=WhenMapped;
  attributes.cursor=dragCursor()->cursor();
  attributes.border_pixel=server()->pixel("yellow");
  _dragWindow=XCreateWindow(display(),server()->root(),
			    0,0,1,1,MSTableDefaultDragBorder,
			    CopyFromParent,InputOutput,CopyFromParent,
			    CWOverrideRedirect|CWSaveUnder|CWBackingStore|CWCursor|CWBorderPixel,
			    &attributes);

  XGCValues values;
  values.foreground=whitePixel^background();
  values.background=blackPixel;
  values.line_width=2;
  values.function=GXxor;
  values.subwindow_mode=IncludeInferiors;
  _movegc.setGCValues(server(),MSTrue,&values,
                      GCForeground|GCBackground|GCLineWidth|GCFunction|GCSubwindowMode);

  selectInput(MSTableEventMask|PointerMotionMask);
}

MSTable::~MSTable(void) 
{
  freeze();
  delete _dragCursor;
  delete _resizeCursor;
  if (_choicesMenu!=0) safeDestroy(_choicesMenu);
  
  XDestroyWindow(display(),dragWindow());
  unsigned i,n=columnList()->count();
  for (i=n-1;i<n;i--) safeDestroy(tableColumn(i));
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--) safeDestroy((MSTableColumn *)hiddenColumnList()->array(i));
}

int MSTable::selectedDataRow(void) const
{
  int dataRow=selectedRow();
  if (dataRow!=-1)
   {
     MSBoolean isBreakRow;
     dataRow=getDataRow(dataRow,isBreakRow);
     if (isBreakRow==MSTrue) dataRow=-1;
     else
      {
	const MSTableColumn *pTableColumn=tableColumn(selectedColumn());
	if (pTableColumn!=0)
	 {
	   if (dataRow>=pTableColumn->numRows()) dataRow=-1;
	 }
	else dataRow=-1;
      }
   }
  return dataRow;
}

void MSTable::selectedDataRow(int selectedDataRow_) 
{
  if (selectedDataRow_==-1) selectedRow(-1);
  else
   {
     if (selectedColumn()==-1&&numColumns()>0) _selectedColumn=0;
     if (selectedColumn()!=-1)
      {
	MSTableColumn *pTableColumn=tableColumn(selectedColumn());
	if (pTableColumn!=0)
	 {
	   if (pTableColumn->numRows()>selectedDataRow_)
	    {
	      if (showBreaks()==MSTrue) selectedRow(adjustPositionForBreaks(selectedDataRow_));
	      else if (viewVector().length()>0)
	       {
		 int viewRow;
		 if ((viewRow=viewVector().indexOf(selectedDataRow_))!=viewVector().length())
		  {
		    selectedRow(viewRow);
		  }
	       }
	      else selectedRow(selectedDataRow_);
	    }
	 }
      }
   }
}

MSIndexVector MSTable::selectionDataVector(void) const
{
  MSIndexVector indexVector;
  for (unsigned i=0;i<selectionVector().length();i++)
   {
     MSBoolean isBreakRow;
     int dataRow=getDataRow(selectionVector()(i),isBreakRow);
     if (isBreakRow==MSFalse) indexVector<<dataRow;
   }
  return indexVector;
}

void MSTable::selectionDataVector(const MSIndexVector &selectionDataVector_)
{
  if (selectionMode()==MSMultiple)
   {
     if (showBreaks()==MSFalse&&viewVector().length()==0) selectionVector(selectionDataVector_);
     else
      {
	MSIndexVector viewSelectionVector;
	for (unsigned i=0;i<selectionDataVector_.length();i++)
	 {
	   if (showBreaks()==MSTrue) viewSelectionVector<<adjustPositionForBreaks(selectionDataVector_(i));
	   else if (viewVector().length()>0)
	    {
	      int viewRow;
	      if ((viewRow=viewVector().indexOf(selectionDataVector_(i)))!=viewVector().length())
	       {
		 viewSelectionVector<<viewRow;
	       }
	    }
	   else viewSelectionVector<<selectionDataVector_(i);
         }
        selectionVector(viewSelectionVector);
      }
   }
}

const MSString& MSTable::virtualHelpString(int x_,int y_)
{
  int row,col;
  if (x_>=0&&y_>=0)
   {
     y_-=panner()->y_origin();
     x_-=panner()->x_origin();     
     col=(x_<fixedColumnPixelWidth())?
         xToColumn(x_-labelWidth()):
         xToColumn(x_-labelWidth())+firstColumn();
     row=yToRow(y_-headingsHeight())+firstRow();
   }
  else
   {
     col=selectedColumn();
     row=selectedRow();
   }
  const MSTableColumn *pTableColumn=tableColumn(col);
  MSBoolean isBreakRow;
  if (pTableColumn!=0&&getDataRow(row,isBreakRow)<pTableColumn->numRows())
   {
     return pTableColumn->helpString();
   }
  return _helpString;
}

unsigned long MSTable::convertForeground(const char *fg_)
{ return server()->pixel(fg_); }

void MSTable::placeColumnAt(MSTableColumn *pColumn_,unsigned index_)
{
  MSBoolean wasFrozen=frozen();
  if (editor()->mapped()==MSTrue) unmapEditor();
  if (wasFrozen==MSFalse) freeze();
  columnList()->insert(pColumn_,index_);
  resetColumnLocations();  
  if (wasFrozen==MSFalse) unfreeze();
}

void MSTable::permuteColumns(const MSIndexVector& aIndexVector_)
{
  MSBoolean wasFrozen=frozen();
  if (editor()->mapped()==MSTrue) unmapEditor();
  if (wasFrozen==MSFalse) freeze();
  MSReportTable::permuteColumns(aIndexVector_);
  if (wasFrozen==MSFalse) adjustView();
  updateInternalState();  
  if (wasFrozen==MSFalse) unfreeze();
}

void MSTable::permuteColumns(const MSSymbolVector& aSymbolVector_)
{
  MSBoolean wasFrozen=frozen();
  if (editor()->mapped()==MSTrue) unmapEditor();
  if (wasFrozen==MSFalse) freeze();
  MSReportTable::permuteColumns(aSymbolVector_);
  if (wasFrozen==MSFalse) adjustView();
  updateInternalState();  
  if (wasFrozen==MSFalse) unfreeze();
}

// insert the column into the column list
void MSTable::insertChild(MSWidget *widget_) 
{
  MSTableColumn *pTableColumn=(MSTableColumn *)widget_;
  addColumn(pTableColumn);
  updateInternalState();
  if (mapped()==MSTrue&&frozen()==MSFalse) updateData();
  if (visible() !=  pTableColumn->visible())
    {
      if(visible()==MSTrue) visibilityUnobscuredNotify(pTableColumn);
      else visibilityObscuredNotify(pTableColumn);
    }
}
// remove child from the column list
void MSTable::removeChild(MSWidget* widget_) 
{
  MSTableColumn *pTableColumn=(MSTableColumn *)widget_;
  if (columnList()->remove(pTableColumn)==MSTrue)
   {
     resetColumnLocations();
     updateInternalState();
     if (mapped()==MSTrue&&frozen()==MSFalse) updateData();
   }
  else hiddenColumnList()->remove(pTableColumn);
}

void MSTable::headingFont(const char *fid_) 
{ 
  headingFont(server()->fontID(fid_));
}

void MSTable::headingFont(Font fid_) 
{ 
  if (headingFont()!=fid_)
   {
     freeze();
     unsigned i,n=numColumns();
     for (i=0;i<n;i++) if (tableColumn(i)->headingFont()==headingFont()) tableColumn(i)->headingFont(fid_);
     n=hiddenColumnList()->count();
     for (i=0;i<n;i++)
      {
	MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
	if (tc->headingFont()==headingFont()) tc->headingFont(fid_);
      }
     _headingFont=fid_;
     unfreeze();
   }
}

void MSTable::headingAlignment(unsigned long alignment_) 
{ 
  if (headingAlignment()!=alignment_)
   {
     freeze();
     unsigned i,n=numColumns();
     for (i=0;i<n;i++) if (tableColumn(i)->headingAlignment()==headingAlignment()) tableColumn(i)->headingAlignment(alignment_);
     n=hiddenColumnList()->count();
     for (i=0;i<n;i++)
      {
	MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
	if (tc->headingAlignment()==headingAlignment()) tc->headingAlignment(alignment_);
      }
     _headingAlignment=alignment_;
     unfreeze();
   }
}

void MSTable::exchangeColumns(unsigned column1_,unsigned column2_) 
{ 
  if (columnList()->exchange(column1_,column2_)==MSTrue)
   {
     if (firstMap()==MSTrue)
      {
	if (editor()->mapped()==MSTrue) unmapEditor();
        if (column1_==0||column2_==0) updateInternalState();
	adjustNumVisible();
	redrawImmediately();
      }
   }
}

void MSTable::updateInternalState(void)
{
  unsigned before=numRows();
  maxRowsClear();
  unsigned n=numColumns();
  if (n>0) for (unsigned i=0;i<n;i++) maxRowsSet(tableColumn(i)->numRows());
  unsigned after=numRows();
  if (selectionMode()==MSMultiple) 
   {
     if(after>=before&&after<vsb()->max()) before=vsb()->max();
     if(after<before)
      {
        unsigned i,index;
        for (i=after;i<before;i++)
         {
           if ((index=selectionVector().indexOf(i))!=selectionVector().length())
            {
              _selectionVector.removeAt(index);
              lastBlock(-1); 
            }
         }
      }
   }
}

int MSTable::fixedReportColumns(void) const 
{ return _fixedReportColumns<0?fixedColumns():_fixedReportColumns; }

void MSTable::updateScreen(void)
{
  if (showBreaks()==MSTrue) redrawImmediately();
}

void MSTable::updateForeground(unsigned long oldfg_)
{ 
  MSArrayView::updateForeground(oldfg_);
  unsigned i,n=numColumns();
  for (i=0;i<n;i++)
   {
     MSTableColumn *tc=(MSTableColumn *)tableColumn(i);
     if (tc->foreground()==oldfg_) tc->foreground(foreground());
   }
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--)
  {
     MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
     if (tc->foreground()==oldfg_) tc->foreground(foreground());
  }
}

void MSTable::updateBackground(unsigned long oldbg_)
{ 
  MSArrayView::updateBackground(oldbg_);
  unsigned i,n=numColumns();
  for (i=0;i<n;i++)
   {
     MSTableColumn *tc=tableColumn(i);
     if (tc->background()==oldbg_) tc->background(background());
   }
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--)
   {
     MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
     tc->background(background());
   }
}

void MSTable::columnChoiceActivate(const MSString& aString_)
{
  if(viewValidate(aString_.string(),selectedRow(),selectedColumn())==MSTrue)
   {
     valueChange();
     //TODO: is this necessary??
     if (showBreaks()==MSTrue) drawRowColumn(selectedRow(),selectedColumn());   
   }
}

MSBoolean MSTable::editorActivate(void)
{
  if (editor()->mapped()==MSTrue)
   {
     if (viewValidate(editor()->string(),selectedRow(),selectedColumn())==MSTrue)
      {
        unmapEditor();
        valueChange();
        if (showBreaks()==MSTrue) drawRowColumn(selectedRow(),selectedColumn());
      }
   }
  return (editor()->mapped()==MSTrue)?MSFalse:MSTrue;
}

void MSTable::valueChange(void) 
{ activateCallback(MSWidgetCallback::valuechange); }

void MSTable::increment(void)
{
  if (selectedRow()!=-1)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(selectedRow(),isBreakRow);
     if (isBreakRow==MSFalse)
      {
	MSTableColumn *pTableColumn=tableColumn(selectedColumn());
	if (pTableColumn!=0&&modelRow<pTableColumn->numRows())
	 {
	   pTableColumn->increment(modelRow);
	 }
      }
   }
}

void MSTable::decrement(void)
{
  if (selectedRow()!=-1)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(selectedRow(),isBreakRow);
     if (isBreakRow==MSFalse)
      {
	MSTableColumn *pTableColumn=tableColumn(selectedColumn());
	if (pTableColumn!=0&&modelRow<pTableColumn->numRows())
	 {
	   pTableColumn->decrement(modelRow);
	 }
      }
   }
}

int MSTable::columnPixelWidth(int column_)
{
  if (column_<numColumns())
   {
     MSTableColumn *pTableColumn=tableColumn(column_);
     if (pTableColumn!=0) return pTableColumn->columnPixelWidth();
     else return MSTableDefaultColumnWidth*charWidth('M')+(columnSpacing()<<1);
   }
  else return MSTableDefaultColumnWidth*charWidth('M')+(columnSpacing()<<1);
}

unsigned MSTable::columnNumRows(unsigned) const
{ return numRows(); }

unsigned MSTable::numRows(unsigned column_) const
{ 
  const MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->numRows():0; 
}

unsigned MSTable::columnLength(unsigned column_)
{
  int cw=0;
  if (column_<numColumns())
   {
     MSTableColumn *pTableColumn=tableColumn(column_);
     if (pTableColumn!=0) cw=pTableColumn->columnWidth();
   }     
  return cw;
}

void MSTable::clearColumn(int column_)
{ if (inColRange(column_)==MSTrue) clearRows(firstRow(),lastRow(),column_,column_); }

void MSTable::drawFixedColumnHeadings(Window window_)
{ if (fixedColumns()>0) drawHeadings(window_,0,fixedColumns()-1); }

void MSTable::drawHeadings(Window window_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   { 
     drawFixedColumnHeadings(window_);
     drawHeadings(window_,firstColumn(),lastColumn()); 
   }
}

void MSTable::drawHeadings(Window window_,int cs_,int ce_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse&&headingsHeight()>0) 
   {
     drawColumnHeadings(window_,cs_,ce_);
     drawGroupHeadings(window_,cs_,ce_);
   }
}

void MSTable::updateColumnHeadings(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   { 
     if (fixedColumns()>0) drawColumnHeadings(panner()->window(),0,fixedColumns()-1);
     drawColumnHeadings(panner()->window(),firstColumn(),lastColumn()); 
   }
}

void MSTable::drawColumnHeadings(Window window_,int cs_,int ce_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse&&headingsHeight()>0) 
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int y=offset;
     int x=computeXCoord(cs_);
     int len=0;
     int cw=0;
     int j,w;
     MSRect aRect;    

     for (j=cs_;j<=ce_&&j<numColumns();j++)
      {
	MSTableColumn *pTableColumn=tableColumn(j);
	cw=pTableColumn->columnPixelWidth();
	if (pTableColumn->heading().length()>0)
	 {
           len=pTableColumn->columnWidth();
           w=panner()->width()-2*(panner()->shadowThickness()+panner()->highlightThickness());
           XFillRectangle(display(),window_,panner()->backgroundShadowGC(),
                          x,y,(cw>w)?w:cw,headingsHeight()-rowSpacing());
           aRect.configuration(x+columnSpacing(),y+rowSpacing(),
                          (cw>w)?w:cw,headingsHeight()-2*rowSpacing());
	   drawColumnHeading(window_,pTableColumn,aRect,len);
	 }
	x+=cw;
      }
     XBFillRectangle(display(),window_,panner()->bottomShadowGC(),
	 	     offset,offset+headingsHeight()-rowSpacing(),
		     panner()->width()-2*offset,rowSpacing());
   }
}

void MSTable::drawCell(Window window_,int x_,int y_,int row_,int column_)
{
  MSArrayView::drawCell(window_,x_,y_,row_,column_);
}

void MSTable::drawCell(Window window_,int x_,int y_,int row_,int column_,
			   unsigned long foreground_,unsigned long background_,Font fid_,
			   MSBoolean rowSelected_,MSBoolean cellSelected_,MSBoolean cellValid_)
{
  MSBoolean hasOptions=columnHasOptions(column_);
  if (hasOptions==MSTrue)
    {
      MSArrayView::drawCell(window_,x_,y_,row_,column_,foreground_,background_,fid_,
			rowSelected_,MSFalse,cellValid_);
    }
  else
    {
      MSArrayView::drawCell(window_,x_,y_,row_,column_,foreground_,background_,fid_,
			rowSelected_,cellSelected_,cellValid_);
    }

  ChoiceStyle style=columnChoiceStyle(column_);
  MSBoolean drawOptionMenu=MSBoolean(hasOptions && (style==ChoiceOptionMenuAlwaysDrawn
                                                    || cellSelected_==MSTrue));
  if (drawOptionMenu==MSTrue)
    {
      int w=columnPixelWidth(column_)-columnSpacing();
      int h=rowHeight()-rowSpacing();
      MSRect aRect(x_,y_,w,h);
      drawRaised(window_,aRect,1);

      aRect.configuration(x_+w-MSTableOptionMenuMargin-MSTableOptionMenuSymbolWidth,
			  y_+(h-MSTableOptionMenuSymbolHeight)/2,
			  MSTableOptionMenuSymbolWidth,
			  MSTableOptionMenuSymbolHeight);
      drawRaised(window_,aRect,MSTableOptionMenuSymbolThickness);
    }
}


void MSTable::appendUpdate(void) 
{
  MSBoolean showBreakStatus=showBreaks();
  unsigned oldFirstRow=firstRow();
  if (showBreakStatus==MSTrue) freeze();
  showBreaks(MSFalse);
  int rs=vsb()->max();
  adjustNumVisible();
  updateInternalState();
  updateVsb();
  if (inRowRange(rs)==MSTrue) 
   {
     drawHSeparators(panner()->window(),firstRow(),lastRow(),0,fixedColumns()-1);
     drawHSeparators(panner()->window(),firstRow(),lastRow(),firstColumn(),lastColumn());
     drawVSeparators(panner()->window(),firstRow(),lastRow(),0,fixedColumns()-1);
     drawVSeparators(panner()->window(),firstRow(),lastRow(),firstColumn(),lastColumn());
     moveSelection(selectedRow(),selectedColumn());
   }
  showBreaks(showBreakStatus);
  if (showBreakStatus==MSTrue)
   {
     MSReportTable::computeBreaks();
     _firstRow=oldFirstRow;
     unfreeze();
   }
}

void MSTable::columnUpdate(int column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean showBreakStatus=showBreaks();
     if (showBreakStatus==MSTrue) freeze();
     showBreaks(MSFalse);
     int nr=numDataRows();
     int fr=firstRow();

     if (editor()->mapped()==MSTrue&&
         selectedColumn()==column_&&selectedRow()>=pTableColumn->numRows()) unmapEditor();

     updateInternalState();
     updateVsb();
     adjustFirstRow();
     adjustSelection();
     // If we are in multiple selection mode, always make sure the current
     // selected row is in the selection vector.
     if (selectionMode()==MSMultiple&&selectedRow()!=-1&&
	 selectionVector().indexOf(selectedRow())==selectionVector().length())
      {
	_selectionVector.append(selectedRow());
	_selectionVector.sortUp();
      }     
     updateScrollBars();
     if (fr!=firstRow()||numDataRows()!=nr) redrawImmediately();
     else
      {
	if (pTableColumn->numRows()<firstRow()+rows()) 
         {
           clearRows(pTableColumn->numRows(),firstRow()+rows()-1,column_,column_);
         }
	drawColumn(column_);
	moveSelection(selectedRow(),selectedColumn());
      }
     showBreaks(showBreakStatus);
     if (showBreakStatus==MSTrue)
      {
        MSReportTable::computeBreaks();
        _firstRow=fr;
        unfreeze();
      }
   }
}

void MSTable::createCycle(int row_,int column_)
{
  if (column_>=0) 
   {
     MSTableColumn *pTableColumn=tableColumn(column_);
     if (pTableColumn!=0)
      {
	unsigned viewRow=row_;
	if (cycleList().length()!=0) processCycleTimer();
	if (row_!=-1)
	 {
	   if (showBreaks()==MSTrue)
	    {
	      viewRow=adjustPositionForBreaks(viewRow);
	    }
	   else if (viewVector().length()>0)
	    {
	      if ((viewRow=viewVector().indexOf(row_))==viewVector().length()) return;
	    }
	 }
	if (inRowRange(viewRow)==MSTrue)
	 {
	   if (pTableColumn->cycleColors().length()>0||pTableColumn->cycleColorMode()==MSReverseVideo)
	      startCycle(viewRow,column_,pTableColumn->cycleColors(),pTableColumn->cycleColorMode());
	   else if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
	      startCycle(viewRow,column_,cycleColors(),cycleColorMode());
	   else drawRowColumn(viewRow,column_);
	 }
	else if (row_==-1)
	 {
	   if (pTableColumn->cycleColors().length()>0||pTableColumn->cycleColorMode()==MSReverseVideo)
	      startCycle(viewRow,column_,pTableColumn->cycleColors(),pTableColumn->cycleColorMode());
	   else if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
	      startCycle(viewRow,column_,cycleColors(),cycleColorMode());
	   else drawColumn(column_);
	 }
	if (cycleList().length()!=0)
	{
	   if (cycleTimer()==0) _cycleTimer=new MSArrayView::CycleTimer(this,cycleInterval());
	   else cycleTimer()->reset();
	}
        updateBreakStatus(row_,column_);
      }
   }
  else if(row_!=-1)   // cycleRow was called.
   {
     unsigned viewRow=row_;
     if (showBreaks()==MSTrue)
      {
        viewRow=adjustPositionForBreaks(viewRow);
      }
     else if (viewVector().length()>0)
      {
        if ((viewRow=viewVector().indexOf(row_))==viewVector().length()) return;
      }
     if (inRowRange(viewRow)==MSTrue)
      {
        int n = numColumns();
        //We have to do a loop here, because cycleColor vector could be different
        //for each column.
        for(int col=0;col<n;col++)
         {
           MSTableColumn *pTableColumn=tableColumn(col);
           if (pTableColumn!=0)
            {
              if (pTableColumn->cycleColors().length()>0||pTableColumn->cycleColorMode()==MSReverseVideo)
                  startCycle(viewRow,col,pTableColumn->cycleColors(),pTableColumn->cycleColorMode());
              else if (cycleColors().length()>0||cycleColorMode()==MSReverseVideo)
                  startCycle(viewRow,col,cycleColors(),cycleColorMode());
              else drawRowColumn(viewRow,col);
            }
         }
      }
   }
}

void MSTable::calculateHeadingsHeight(void)
{
  calculateGroupHeadingsHeight();
  calculateColumnHeadingsHeight();
  headingsHeight(columnHeadingsHeight()+groupHeadingsHeight());
}

void MSTable::calculateColumnHeadingsHeight(void)
{
  MSTableColumn *pTableColumn;
  Font           fid;
  int            th,max=0;
  unsigned       n=numColumns();
  for (unsigned i=0;i<n;i++)
   {
     pTableColumn=tableColumn(i);
     fid=pTableColumn->headingFont();
     pTableColumn->headingFontStruct((XFontStruct *)server()->fontStruct(fid));
     th=pTableColumn->headingHeight();
     max=(th>max)?th:max;
   }
  max=(max>0)?max+(rowSpacing()<<1):0;
  columnHeadingsHeight(max);
}

void MSTable::updateFont(Font oldfid_)
{ 
  MSCompositeText::updateFont(oldfid_);
  freeze();
  unsigned i,n=numColumns();
  for (i=0;i<n;i++)
   {
     MSTableColumn *tc=(MSTableColumn *)tableColumn(i);
     if (tc->font()==oldfid_) tc->font(font());
   }
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--)
  {
     MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
     if (tc->font()==oldfid_) tc->font(font());
  }
  rowHeight(textHeight()+2*rowSpacing());
  headingsHeight(rowHeight());
  unfreeze();
}

void MSTable::updateTitle(void)
{
  unsigned long fg=label()->foreground();
  Font fid=label()->font();
  MSArrayView::updateTitle();
  if (fid!=label()->font()) 
   {
     int hh=headingsHeight();
     calculateHeadingsHeight();
     if (hh!=headingsHeight())
      {
	adjustNumVisible();
	redrawImmediately(); 
      }
     else drawHeadings(panner()->window());   
   }
  else if (fg!=label()->foreground()) drawHeadings(panner()->window()); 
}

void MSTable::visibilityObscured(void)
{
  visible(MSFalse);
  unsigned n=numColumns();
  for (unsigned i=0;i<n;i++) visibilityObscuredNotify(tableColumn(i));
}

void MSTable::visibilityUnobscured(void)
{
  visible(MSTrue);
  unsigned n=numColumns();
  for (unsigned i=0;i<n;i++) visibilityUnobscuredNotify(tableColumn(i));
}

void MSTable::drawColumnHeading(Window window_,MSTableColumn *pTableColumn_,const MSRect& aRect_,int)
{
  XFontStruct *fs=(XFontStruct *)pTableColumn_->headingFontStruct();
  int nc=0;
  int xdelta;
  int ydelta;
  int cw=aRect_.width();
  int tw;
  
  XSetForeground(display(),textGC(),pTableColumn_->headingForeground());
  XSetBackground(display(),textGC(),panner()->background());
  XSetFont(display(),textGC(),pTableColumn_->headingFont());

  if (pTableColumn_->heading().length()>0)
   {
     int n=pTableColumn_->heading().length();
     const char *pString;
     if (pTableColumn_->headingAlignment()&MSTop)
      {
	ydelta=groupHeadingsHeight();
      }
     else if (pTableColumn_->headingAlignment()&MSBottom)
      {
	ydelta=aRect_.height()-pTableColumn_->headingHeight()-rowSpacing();
      }
     else
      {
	ydelta=aRect_.height()-groupHeadingsHeight()-pTableColumn_->headingHeight();
	ydelta=(ydelta>0)?ydelta>>1:0;
        ydelta+=groupHeadingsHeight();
      }
     int y=aRect_.y()+ydelta;
     for (unsigned i=0;i<n;i++)
      {
	const MSString& aString=pTableColumn_->heading()(i);
        tw=0;
        nc=0;
        pString=aString.string();
	nc=aString.length();
	tw=XTextWidth(fs,pString,nc);
        if (nc>0)
	 {
           if (tw>cw) 
            {
              nc=computeMaxTextLength(fs,pString,cw,nc);
              xdelta=0;
	    }
           else
	    {
	      if (pTableColumn_->headingAlignment()&MSLeft)
	       {
		 xdelta=0;
	       }
	      else if (pTableColumn_->headingAlignment()&MSRight)
	       {
		 xdelta=cw-tw-columnSpacing();
	       }
	      else
	       {
		 xdelta=(cw>tw)?(cw-tw)>>1:0;
	       }
	    }
	   XDrawImageString(display(),window_,textGC(),fs,
                            aRect_.x()+xdelta,y+pTableColumn_->headingAscent(),pString,nc);
	 }
	y+=(pTableColumn_->headingAscent()+pTableColumn_->headingDescent());
      }
   }
}

void MSTable::headingAreaSelection(const XEvent *pEvent_)
{
  int col=(pEvent_->xbutton.x<(fixedColumnPixelWidth()+labelWidth()))?
          xToColumn(pEvent_->xbutton.x-labelWidth()):
          xToColumn(pEvent_->xbutton.x-labelWidth())+firstColumn();
  if (inColRange(col)==MSTrue)
   {
     if (pEvent_->xbutton.state&Mod1Mask)
      {
	if(columnDragDrop()==MSTrue) dragColumn(pEvent_,col);
      }
     else 
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
}

void MSTable::dataAreaSelection(const XEvent *pEvent_)
{
  // Figure out the row and column selected by the button
  int col=columnFromEvent(pEvent_);
  MSTableColumn *tcol=0;
  // If the meta key is held down, drag the column
  if (pEvent_->xbutton.state&Mod1Mask)
   {
     if (columnDragDrop()==MSTrue&&inColRange(col)==MSTrue)
      {
	dragColumn(pEvent_,col);
      }
   }
  // else if the pointer is inside a separator, resize the column only if neither the Shift
  // nor Control key is held down
  else if ((!(pEvent_->xbutton.state&ShiftMask)&&!(pEvent_->xbutton.state&ControlMask))&&
	   columnResize()==MSTrue&&
	   insideSeparator(pEvent_->xbutton.x,pEvent_->xbutton.y,col)==MSTrue&&
	   (tcol=tableColumn(col))!=0&&
	   tcol->resizable()==MSTrue)
   {
     resizeColumn(col,pEvent_->xbutton.x);
   }
  else
   {
     XUndefineCursor(display(),panner()->window());
     if (pEvent_->xbutton.button==Button1)
      {
        int row=rowFromEvent(pEvent_);
        MSBoolean isBreakRow;
        int dataRow=getDataRow(row,isBreakRow);
        if (isBreakRow==MSFalse)
         {
           int oldSelectedRow=selectedRow();
           int oldSelectedColumn=selectedColumn();
           MSBoolean inRange=MSBoolean(inColRange(col)&&inRowRange(row));
           MSBoolean hasOptions=columnHasOptions(col);
           
           if (inRange==MSTrue &&
               hasOptions==MSTrue &&
               col==oldSelectedColumn &&
               row==oldSelectedRow &&
               isViewValid(row,col)==MSTrue)
            {
              showOptions(row,col);
              return;
            }
         }
      }
     MSArrayView::dataAreaSelection(pEvent_);
   }
}

void MSTable::calculateRowHeight(void)
{ 
  MSArrayView::calculateRowHeight();
  unsigned ncol=numColumns();
  for (unsigned i=0;i<ncol;i++)
    {
      if (columnHasOptions(i)==MSTrue)
	{
	  rowHeight(rowHeight()+2);
	  break;
	}
    }
}

const MSStringVector &MSTable::cellOptions(int row_,int column_)
{
  static MSStringVector nullStringVector;
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0) return pTableColumn->cellChoices(row_);
  else return nullStringVector;
}

void MSTable::returnKey(void)
{
  if (columnHasOptions(selectedColumn())==MSTrue)
    {
      showOptions(selectedRow(),selectedColumn());
    }
  else MSArrayView::returnKey();
}

MSBoolean MSTable::columnHasOptions(int column_)
{
  if (columnChoiceStyle(column_) != ChoicePopupMenu)
   {
     MSTableColumn *col=tableColumn(column_);
     if(col!=0)  return col->hasOptions();
   }
  return MSFalse;
}

MSTable::ChoiceStyle MSTable::columnChoiceStyle(int)
{ return _choiceStyle; }

void MSTable::defaultButton3Behavior(const XEvent *pEvent_) 
{
  const MSTableColumn *pTableColumn=tableColumn(selectedColumn());
  if (pTableColumn!=0)
   {
     if (pTableColumn->choices().length()>0 &&
         columnChoiceStyle(selectedColumn())==ChoicePopupMenu)
      {
	if (_choicesMenu==0) _choicesMenu=new ColumnPopupMenu(this);
	_choicesMenu->choices(pTableColumn->choices());
	_choicesMenu->background(pTableColumn->background());
	_choicesMenu->foreground(pTableColumn->foreground());
	_choicesMenu->font(pTableColumn->font());		
        int x,y;
	pointerXY(x,y);
	_choicesMenu->moveTo(x,y);
	_choicesMenu->show();
      }
     else if (activateCallback(MSWidgetCallback::menubutton)==MSFalse)
      {
	startEditing(pEvent_);
      }
   }
}

void MSTable::childInsert(MSWidget *widget_)  
{ insertChild(widget_); }
void MSTable::childRemove(MSWidget *widget_)  
{ removeChild(widget_); }
void MSTable::childCreate(MSWidget *widget_)  
{ insertChild(widget_); }
void MSTable::childDestroy(MSWidget *widget_) 
{ removeChild(widget_); }

unsigned MSTable::numColumns(void) const
{ return columnList()->count(); }

unsigned MSTable::numRows(void) const
{
  // viewVector and showBreaks are not supported together,
  // the numRows calculation takes that into consideration.
  if (showBreaks()==MSTrue) return MSReportTable::numRows()+breakIndex().length();
  else if (viewVector().length()>0) return viewVector().length();
  else return MSReportTable::numRows();
}

unsigned MSTable::numDataRows(void) const
{
  // We're not honoring viewVector when showBreaks is On
  if (showBreaks()==MSTrue) return MSReportTable::numRows();
  else if (viewVector().length()>0) return viewVector().length();
  else return MSReportTable::numRows();
}

MSWidgetVector MSTable::children(void)
{
  MSWidgetVector vector;
  unsigned n=columnList()->count();
  unsigned i;
  for (i=0;i<n;i++) vector.append(columnList()->array(i));
  n=hiddenColumnList()->count();
  for (i=0;i<n;i++) vector.append(hiddenColumnList()->array(i));
  return vector;
}

void MSTable::columnResize(MSBoolean columnResize_)
{
  if (_columnResize!=columnResize_)
   {
     _columnResize=columnResize_;
     if (_columnResize==MSTrue) selectInput(MSTableEventMask|PointerMotionMask);
     else
      {
	XUndefineCursor(display(),panner()->window());
	selectInput(MSTableEventMask);
      }
   }
}

void MSTable::shuffleColumns(MSIndexVector &aIndexVector_)
{
   permuteColumns(aIndexVector_);
   activateCallback(MSWidgetCallback::permutecolumns);
}

void MSTable::dragColumn(const XEvent *pEvent_,int column_)
{
  int x=computeXCoord(column_);
  int width=columnPixelWidth(column_);

  // if the whole column does not fit within the panner,just beep and return.
  // We want to disallow drag & drop on partially shown columns.
  if (x+width>panner()->width()-panner()->highlightThickness()-panner()->shadowThickness())
  {
     server()->bell();
     return;
  }
  server()->grabPointer(window(),False,ButtonPressMask|ButtonReleaseMask,
			GrabModeAsync,GrabModeAsync,None,
			dragCursor()->cursor(),CurrentTime);
  int rootx,rooty;
  panner()->rootXY(rootx,rooty);
  int height=panner()->height();
  int y=rooty+headingsHeight();
  XWindowChanges values;
  values.x=x+rootx-MSTableDefaultDragBorder;
  values.y=y;
  values.width=width;
  values.height=height;
  XConfigureWindow(display(),dragWindow(),CWX|CWY|CWWidth|CWHeight,&values);
  XCopyArea(display(),panner()->window(),redrawPixmap()->pixmap(),backgroundGC(),x,0,width,height,x,0);
  drawGroupHeadings(redrawPixmap()->pixmap(),column_,column_);
  XMapRaised(display(),dragWindow());
  XCopyArea(display(),redrawPixmap()->pixmap(),dragWindow(),backgroundGC(),x,0,width,height,0,0);
  int left=computeXCoord(firstColumn())+rootx;
  int right=panner()->width()+rootx;

  int offset=pEvent_->xbutton.x-x;
  int startx=pEvent_->xbutton.x+rootx;
  int lastx=startx;

  Window root,child;
  int rx,ry,ix,iy;
  unsigned keys;
  // We're going to allow either Button1,Button2,or Button3
  int sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while ((keys&Button1Mask)||(keys&Button2Mask)||(keys&Button3Mask))
   {
     if (sameScreen==True)
      {
	int newx=rx-offset;
	if (rx!=lastx)
	 {
	   XMoveWindow(display(),dragWindow(),newx,y);
	   server()->flush();
	   lastx=rx;
	 }
	if (newx<left&&firstColumn()!=fixedColumns()) 
	 {
	   if (newx+width>left||fixedColumns()==0) scrollLeft(1);
	 }
	else if (newx+width>right&&lastColumn()!=numColumns()-1) scrollRight(1);
      }
     sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
   }

  XUnmapWindow(display(),dragWindow());
  server()->ungrabPointer(window(),CurrentTime);
  int finalX=lastx-offset-rootx;
  int col=(finalX<(fixedColumnPixelWidth()+labelWidth()))?
  xToColumn(finalX-labelWidth()):
  xToColumn(finalX-labelWidth())+firstColumn();
  col=(col>=numColumns())?numColumns()-1:col;
  if (col!=column_&&(col!=column_-1||finalX<labelWidth()))
   {
     MSBoolean wasFrozen=frozen();
     if (wasFrozen==MSFalse) freeze();
     
     MSIndexVector aIndexVector(numColumns());
     aIndexVector.series();
     aIndexVector.removeAt(column_);
     if (lastx>startx) //Insert right
      {
	if (col==numColumns()-1||lastx-offset>right) aIndexVector.append(column_);
	else aIndexVector.insertAt(col,column_);
      }
     else // Insert left
      {
	if (col==0&&lastx-offset<rootx+labelWidth()) aIndexVector.insertAt(0,column_);
	else aIndexVector.insertAt(col+1,column_);
      }
     shuffleColumns(aIndexVector);
     // establish firstColumn if in fixedColumns or past numColumns
     if (lastx>startx) //Insert right
      {
	 if (col==numColumns()-1||lastx-offset>right) 
	 {
	    int off=panner()->highlightThickness()+panner()->shadowThickness();
	    int deltaWidth=panner()->width()-2*off-fixedColumnPixelWidth()-labelWidth();
	    int i;
	    for (i=numColumns()-1;i>=0;i--)
	    {
	       deltaWidth-=columnPixelWidth(i);
	       if (deltaWidth<0) 
	       {
		  i++;
		  break;
	       }
	    }
	   firstColumn(i);
	}
      }
     else // Insert left
      {
	if (fixedColumns()>0)
	 {
	   if (col==fixedColumns()-1) firstColumn(fixedColumns());
	 }
	else if (col==0&&lastx-offset<rootx) firstColumn(0);
      }
     if (showBreaks()==MSTrue) computeBreaks();
     if (wasFrozen==MSFalse) unfreeze();
   }
}

void MSTable::motionNotify(const XEvent *pEvent_)
{
  if (pEvent_->xbutton.subwindow==panner()->window())
   {
     XEvent *ep=(XEvent *)pEvent_;
     ep->xbutton.y-=panner()->y_origin();
     ep->xbutton.x-=panner()->x_origin();
     int col;
     if (insideSeparator(ep->xbutton.x,ep->xbutton.y,col)==MSTrue)
      {
	 MSTableColumn *tcol=tableColumn(col);
	if (tcol!=0&&tcol->resizable()==MSTrue) XDefineCursor(display(),panner()->window(),dragCursor()->cursor());
	else XUndefineCursor(display(),panner()->window());
      }
     else XUndefineCursor(display(),panner()->window());
   }
}

MSBoolean MSTable::insideSeparator(int x_,int y_,int &column_)
{
  if (y_>=headingsHeight())
  {
     int offset=labelWidth()+panner()->shadowThickness()+panner()->highlightThickness();
     int limit=panner()->width()-panner()->shadowThickness()-panner()->highlightThickness();
     int rightOffset=columnSpacing()<<1;
     int leftOffset=columnSpacing();
     int i;
     int w=offset;
     int fc=0;
     int lc=fixedColumns();
     for (i=fc;i<lc;i++)
     {
	if ((w+=columnPixelWidth(i))>=limit) return MSFalse;
	else if (x_<=w+leftOffset&&x_>=w-rightOffset)
	{
	   column_=i;
	   return MSTrue;
	}
     }
     fc=firstColumn();
     lc=lastColumn()+1;
     for (i=fc;i<lc;i++)
     {
	if ((w+=columnPixelWidth(i))>=limit) return MSFalse;
	else if (x_<=w+leftOffset&&x_>=w-rightOffset)
	{
	   column_=i;
	   return MSTrue;
	}
     }
  }
  return MSFalse;
}

void MSTable::resizeColumn(int column_,int startPos_)
{
  server()->grabPointer(window(),False,ButtonPressMask|ButtonReleaseMask,
			GrabModeAsync,GrabModeAsync,None,
			resizeCursor()->cursor(),CurrentTime);

  MSTableColumn *column=tableColumn(column_);
  int increment=(column->clipMode()==MSNoClipping)?column->charWidth('W'):column->charWidth('0');
  int width=column->columnWidth();
  int drawX=computeXCoord(column_);
  int drawY=headingsHeight();
  int drawHeight=panner()->height()-headingsHeight()-2;
  int startX=drawX+column->columnPixelWidth();
  int drawWidth=startX-drawX;
  int lastX=startX;
  int offset=startX-startPos_;
  int leftLimit=drawX+columnSpacing();
  int rightLimit=panner()->width()-panner()->highlightThickness()-panner()->shadowThickness();

  XDrawRectangle(display(),panner()->window(),moveGC(),drawX,drawY,drawWidth,drawHeight);
  Window root,child;
  int rx,ry,ix,iy;
  unsigned keys;
  int newWidth;
  int sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  while ((keys&Button1Mask)||(keys&Button2Mask)||(keys&Button3Mask))
   {
     if (sameScreen==True)
      {
	ix+=offset;
	ix=ix>leftLimit?ix:leftLimit;
	ix=ix<rightLimit?ix:rightLimit;
 	if (ix!=lastX)
	 {
	   XDrawRectangle(display(),panner()->window(),moveGC(),drawX,drawY,drawWidth,drawHeight);
	   lastX=ix;
	   drawWidth=lastX-drawX;
	   XDrawRectangle(display(),panner()->window(),moveGC(),drawX,drawY,drawWidth,drawHeight);
	 }
      }
     sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
   }
  XDrawRectangle(display(),panner()->window(),moveGC(),drawX,drawY,drawWidth,drawHeight);

  newWidth=(lastX-drawX-columnSpacing()*2)/increment;
  newWidth=newWidth>0?newWidth:0;
  column->columnWidth(newWidth);
  server()->ungrabPointer(window(),CurrentTime);
  int dummy;
  if (insideSeparator(lastX,iy,dummy)==MSTrue) XDefineCursor(display(),panner()->window(),dragCursor()->cursor());
  else XUndefineCursor(display(),panner()->window());
}

MSTableColumn *MSTable::tableColumn(unsigned column_)
{ return (MSTableColumn *)columnList()->array(column_); }
MSTableColumn *MSTable::tableColumn(const MSSymbol& tag_)
{ return (MSTableColumn *)MSReportTable::reportColumn(tag_); }
const MSTableColumn *MSTable::tableColumn(unsigned column_) const
{ return (MSTableColumn *)columnList()->array(column_); }
const MSTableColumn *MSTable::tableColumn(const MSSymbol& tag_) const
{ return (MSTableColumn *)MSReportTable::reportColumn(tag_); }

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

MSDisplayServer *MSTable::displayServer(void) 
{return server();}

MSIndexVector MSTable::sortUp(const MSIndexVector &sortVector_)
{
  MSApplicationBusy busy;
  MSBoolean wasFrozen=frozen();
  freeze();
  MSIndexVector iv=MSReportTable::sortUp(sortVector_);
  if (showBreaks()==MSTrue) MSReportTable::computeBreaks();
  if (wasFrozen==MSFalse) unfreeze();
  return iv;
}

MSIndexVector MSTable::sortDown(const MSIndexVector &sortVector_)
{
  MSApplicationBusy busy;
  MSBoolean wasFrozen=frozen();
  freeze();
  MSIndexVector iv=MSReportTable::sortDown(sortVector_);
  if (showBreaks()==MSTrue) MSReportTable::computeBreaks();
  if (wasFrozen==MSFalse) unfreeze();
  return iv;
}

MSIndexVector MSTable::sortUp(const MSSymbolVector &sortVector_)
{
  MSApplicationBusy busy;
  MSBoolean wasFrozen=frozen();
  freeze();
  MSIndexVector iv=MSReportTable::sortUp(sortVector_);
  if (showBreaks()==MSTrue) MSReportTable::computeBreaks();
  if (wasFrozen==MSFalse) unfreeze();
  return iv;
}

MSIndexVector MSTable::sortDown(const MSSymbolVector &sortVector_)
{
  MSApplicationBusy busy;
  MSBoolean wasFrozen=frozen();
  freeze();
  MSIndexVector iv=MSReportTable::sortDown(sortVector_);
  if (showBreaks()==MSTrue) MSReportTable::computeBreaks();
  if (wasFrozen==MSFalse) unfreeze();
  return iv;
}

void MSTable::set(MSAttrValueList& avList_)
{
  MSArrayView::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="headingFont")
      {
	headingFont(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="headingAlignment")
      {
	headingAlignment(MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="tags")
      {
	MSString names=avList_[i].value();
	names.change("\\n",MSString('\n'));
	MSStringVector nameVector(names);
	MSSymbolVector vector;
	for (unsigned j=0;j<nameVector.length();j++)
	 {
	   vector<<MSSymbol(nameVector(j));
	 }
	permuteColumns(vector);
	index<<i;
      }
     else if (avList_[i].attribute()=="fixedColumns")
      {
	fixedColumns(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="columnDragDrop")
      {
	columnDragDrop(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="columnResize")
      {
	columnResize(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="dynamicRecompute")
      {
	dynamicRecompute(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showBreaks")
      {
	showBreaks(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="choiceStyle")
      {
        if(avList_[i].value()=="ChoicePopupMenu") choiceStyle(ChoicePopupMenu);
        else if(avList_[i].value()=="ChoiceOptionMenu") choiceStyle(ChoiceOptionMenu);
        else choiceStyle(ChoiceOptionMenuAlwaysDrawn);
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSTable::get(MSAttrValueList& avList_)
{
  MSStringVector aBoolVector("MSFalse\nMSTrue");
  
  MSSymbolVector vector=tags();
  MSString value;
  for (unsigned i=0;i<vector.length();i++)
   {
     value<<vector(i).symbolName();
     if (i<vector.length()-1) value<<"\\n";
   }
  avList_<<MSAttrValue("headingFont",_server->fontName(headingFont()),MSAttrValue::Font); 
  MSStringVector alignmentVector("MSNone\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight");
  avList_<<MSAttrValue("headingAlignment",MSAttrValue::alignmentToString(headingAlignment()),
                       alignmentVector, MSAttrValue::List);
  avList_<<MSAttrValue("tags",value,MSAttrValue::ReadOnly);
  avList_<<MSAttrValue("fixedColumns",MSString(fixedColumns()));
  avList_<<MSAttrValue("columnDragDrop",columnDragDrop()==MSTrue?"MSTrue":"MSFalse",
		       aBoolVector);
  avList_<<MSAttrValue("columnResize",columnResize()==MSTrue?"MSTrue":"MSFalse",
		       aBoolVector);
  avList_<<MSAttrValue("showBreaks",showBreaks()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("dynamicRecompute",dynamicRecompute()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  if(choiceStyle()==ChoicePopupMenu) value = "ChoicePopupMenu";
  else if(choiceStyle()==ChoiceOptionMenu) value = "ChoiceOptionMenu";
  else value = "ChoiceOptionMenuAlwaysDrawn";

  avList_<<MSAttrValue("choiceStyle",value,"ChoicePopupMenu\nChoiceOptionMenu\nChoiceOptionMenuAlwaysDrawn");
  avList_<<MSAttrValue("permutecolumns","",MSAttrValue::Callback);
  avList_<<MSAttrValue("menubutton","",MSAttrValue::Callback);
  avList_<<MSAttrValue("valuechange","",MSAttrValue::Callback);

  return MSArrayView::get(avList_);
}

void MSTable::viewVector(const MSIndexVector& viewVector_)
{
  _viewVector=viewVector_;
  if (showBreaks()==MSFalse)
   {
     setSelection(-1,-1);
     _selectionVector.removeAll();
     lastBlock(-1);
     adjustNumVisible();
     redraw();
   }
}

const XFontStruct *MSTable::columnFontStruct(unsigned column_)
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->fontStruct():textFontStruct();
}

unsigned long MSTable::columnBackground(unsigned column_)
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->background():background();
}

unsigned long MSTable::columnForeground(unsigned column_)
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->foreground():foreground();
}

MSClipMode MSTable::columnClipMode(unsigned column_) const
{
  const MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->clipMode():MSNoClipping;
}

unsigned MSTable::columnEditWidth(unsigned column_)
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->editWidth():0;
}

void MSTable::computeBreaks(void)
{
  MSReportTable::computeBreaks();
  if (showBreaks()==MSTrue)
   {
     updateVsb();
     redrawImmediately();
   }
}

void MSTable::dynamicRecompute(MSBoolean recompute_)
{
  if (recompute_!=_dynamicRecompute)
   {
     if ((_dynamicRecompute=recompute_)==MSTrue)
      {
        unsigned n=columnList()->count();
        for (unsigned i=0;i<n;i++)
         {
           MSTableColumn *tc=tableColumn(i);
           if (tc->breakInvalid().length()>0)
            {
              updateBreaks(i);
              unsigned len=tc->breakInvalid().length();
              for (unsigned j=0;j<len;j++)
               {
                 unsigned row=breakIndex()(tc->breakInvalid()(j))+tc->breakInvalid()(j);
                 if (inRowRange(row)==MSTrue) drawRowColumn(row,i);
               }
              tc->breakInvalid().removeAll();
            }
         }
      }
   }
}

void MSTable::showBreaks(MSBoolean showBreaks_)
{
  if (_showBreaks!=showBreaks_)
   {
     setSelection(-1,-1);
     _selectionVector.removeAll();
     lastBlock(-1);
     if ((_showBreaks=showBreaks_)==MSFalse)
      {
        clearBreaks();
      }
     else
      {
        MSReportTable::computeBreaks();
      }
     adjustNumVisible();
     redraw();
   }
}

// This method returns the corresponding data row of a view row.
// The return value has no meaning if the view row is a break row.
// If view vector is in effect, the return value is the model row
// if it is less than the length of the view vector.
unsigned MSTable::getDataRow(unsigned viewRow_,MSBoolean &isBreakRow_) const
{
  if (showBreaks()==MSTrue)
   {
     unsigned modelRow=adjustRowForBreaks(viewRow_);
     unsigned index=viewRow_-modelRow;
     if (index<breakIndex().length()&&modelRow==breakIndex()(index)) isBreakRow_=MSTrue;
     else isBreakRow_=MSFalse;
     return modelRow;
   }
  else
   {
     isBreakRow_=MSFalse;
     return adjustRowForViewVector(viewRow_);
   }
}

unsigned MSTable::adjustRowForViewVector(unsigned viewRow_) const
{
  if (viewVector().length()==0) return viewRow_;
  else if (viewRow_<viewVector().length()) return viewVector()(viewRow_);
  else return viewVector().length();
}

unsigned MSTable::adjustPositionForBreaks(unsigned dataRow_) const
{
  int n=breakIndex().length();
  unsigned i;
  for (i=0;i<n&&breakIndex()(i)<=dataRow_;i++);
  return dataRow_+i;
}

unsigned MSTable::adjustRowForBreaks(unsigned viewRow_) const
{
  unsigned n=breakIndex().length();
  unsigned i;
  for (i=0;i<n&&breakIndex()(i)+i<viewRow_;i++);
  return viewRow_-i;
}

void MSTable::updateBreakStatus(unsigned row_,unsigned column_)
{
  MSTableColumn *tc=tableColumn(column_);
  if (showBreaks()==MSTrue)
   {
     if (dynamicRecompute()==MSTrue)
      {
        updateBreaks(column_);
        tc->breakInvalid().removeAll();
        unsigned n=breakIndex().length();
        for (unsigned i=0;i<n;i++)
         {
           unsigned row=breakIndex()(i)+i;
           if (inRowRange(row)==MSTrue) drawRowColumn(row,column_);
         }
      }
     else
      {
        unsigned i,n=breakIndex().length();
	if(row_==-1)
	 {
	  tc->breakInvalid().removeAll();
	  for(i=0;i<n;i++) tc->breakInvalid()<<i;
	 }
	else
	 {
	   for (i=0;i<n&&breakIndex()(i)<=row_;i++);
//	   cout<<row_<<"\t"<<i<<endl;
	   if ((i+1==n)||(i+1<n&&breakIndex()(i+1)>row_)) tc->breakInvalid()<<i;
	   for (unsigned lastMatch=0,match=0,j=i+1;j<n;j++)
	    {
	     if (breakIndex()(j)==breakIndex()(j-1)) match++; else match=0;
	     if (match>lastMatch)
	      {
	       if (tc->breakInvalid().indexOf(j)==tc->breakInvalid().length()) tc->breakInvalid()<<j;
	       lastMatch=match;
	      }
	    }
	 }
        n=tc->breakInvalid().length();
        for (i=0;i<n;i++)
         {
           unsigned row=breakIndex()(tc->breakInvalid()(i))+tc->breakInvalid()(i);
           if (inRowRange(row)==MSTrue) drawRowColumn(row,column_);
         }
      }
   }
}

MSString MSTable::selection(void)
{
  MSString buffer;
  if (selectedRow()!=-1)
   {
     int row=selectedRow();
     int column=selectedColumn();
     MSTableColumn *tc=tableColumn(column);
     if (tc!=0)
      {
	MSBoolean isBreakRow;
	unsigned modelRow=getDataRow(row,isBreakRow);
	if (isBreakRow==MSTrue)
	 {
	   unsigned index=row-modelRow;
	   tc->formatBreak(buffer,index,breakColumn()(index));
	 }
	else if (modelRow<tc->numRows())
	 {
	   formatOutput(buffer,modelRow,column);
	 }
      }
   }
  return buffer;
}

const char *MSTable::viewFormatOutput(MSString &buffer_,unsigned row_,unsigned column_)
{
  MSTableColumn *tc=tableColumn(column_);
  if (tc!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(row_,isBreakRow);
     if (isBreakRow==MSTrue)
      {
	unsigned index=row_-modelRow;
	tc->formatBreak(buffer_,index,breakColumn()(index));
      }
     else if (modelRow<tc->numRows())
      {
	// If viewVector is in effect, always call formatOutput (i.e. suppressDuplicate is not honor)
	if ((viewVector().length()!=0&&showBreaks()==MSFalse)||
	    (tc->suppressDuplicate()==MSFalse||tc->isDuplicate(modelRow)==MSFalse))
	   formatOutput(buffer_,modelRow,column_);
      }
   }
  return buffer_.string();
}

MSBoolean MSTable::viewValidate(const char *string_,unsigned viewRow_,unsigned column_) 
{
  // If the view row is a break row return MSFalse,
  // otherwise call validate() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue||modelRow>=pTableColumn->numRows()) return MSFalse;
     else return validate(string_,modelRow,column_);
   }
  else return MSFalse;
}

MSBoolean MSTable::isViewProtected(unsigned viewRow_,unsigned column_)
{
  // If the view row is a break row return MSTrue
  // otherwise call isCellProtected() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue||modelRow>=pTableColumn->numRows()) return MSTrue;
     else return isCellProtected(modelRow,column_);
   }
  else return MSTrue;
}

MSBoolean MSTable::isViewValid(unsigned viewRow_,unsigned column_)
{
  // If the view row is a break row then determine if the view is valid
  // otherwise call isValid() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue)
      {
	// if dynamicRecompute() is set to MSTrue, the screen is always valid
	if (dynamicRecompute()==MSTrue) return MSTrue;
	else
	 {
	   unsigned index=viewRow_-modelRow;
	   unsigned len=pTableColumn->breakInvalid().length();
	   if (len>0&&pTableColumn->breakInvalid().indexOf(index)<len) return MSFalse;
	   else return MSTrue;
	 }
      }
     else if (modelRow<pTableColumn->numRows()) return isValid(modelRow,column_);
     else return MSTrue;
   }
  else return MSTrue;

}

unsigned long MSTable::viewCellForeground(unsigned viewRow_,unsigned column_) 
{
  // If the view row is a break row return the break column's foreground
  // otherwise call cellForeground() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue)
      {
	//Get the break column's handle via the breakColumn vector and return its breakFgPixel
	//If breakFgPixel is never assigned, return the table's foreground
	unsigned index=viewRow_-modelRow;
	unsigned long pixel=tableColumn(breakColumn()(index))->breakFgPixel(index);
	return (pixel<ULONG_MAX?pixel:foreground());
      }
     else if (modelRow<pTableColumn->numRows()) return cellForeground(modelRow,column_);
     else return foreground();
   }
  return foreground();
}

unsigned long MSTable::viewCellBackground(unsigned viewRow_,unsigned column_) 
{
  // If the view row is a break row return the break column's foreground
  // otherwise call cellBackground() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue)
      {
	//Get the break column's handle via the breakColumn vector and return its breakBgPixel
	//If breakFgPixel is never assigned, return the table's background
	unsigned index=viewRow_-modelRow;
	unsigned long pixel=tableColumn(breakColumn()(index))->breakBgPixel(index);
	return (pixel<ULONG_MAX?pixel:background());
      }
     else if (modelRow<pTableColumn->numRows()) return cellBackground(modelRow,column_);
     else return background();
   }
  else return background();
}

Font MSTable::viewCellFont(unsigned viewRow_,unsigned column_)
{
  // If the view row is a break row return our own font
  // otherwise call cellFont() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue||modelRow>=pTableColumn->numRows()) return font();
     else return cellFont(modelRow,column_);
   }
  else return font();
}

MSAlignment MSTable::viewCellAlignment(unsigned viewRow_,unsigned column_)
{
  // If the view row is a break row return the current breakStyle of column
  // otherwise call cellAlignment() with the model row
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSBoolean isBreakRow;
     unsigned modelRow=getDataRow(viewRow_,isBreakRow);
     if (isBreakRow==MSTrue||modelRow>=pTableColumn->numRows())
      {
        unsigned long bs=pTableColumn->breakStyle();
        if (MSCenter&bs) return MSCenter;
        else if (MSRight&bs) return MSRight;
        else return MSLeft;
      }
     else return cellAlignment(modelRow,column_);
   }
  else return MSLeft;
}

const char *MSTable::formatOutput(MSString &buffer_,unsigned row_,unsigned column_)
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0) pTableColumn->formatOutput(buffer_,row_);
  return buffer_.string();
}

MSBoolean MSTable::validate(const char *pString_,unsigned row_,unsigned column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0)
   {
     MSString aString=pString_;
     if (pTableColumn->validationCallback()!=0)
      {
	if (pTableColumn->validationCallback()->validate(aString)==MSFalse) return MSFalse;
      }
     return pTableColumn->validate(aString.string(),row_);
   }
  else return MSFalse;
}

MSBoolean MSTable::isCellProtected(unsigned row_,unsigned column_) 
{
  if (isProtected()==MSFalse)
   {
     MSTableColumn *pTableColumn=tableColumn(column_);
     return (pTableColumn!=0)?pTableColumn->isCellProtected(row_):MSTrue;
   }
  else return MSTrue;
}

MSBoolean MSTable::isValid(unsigned row_,unsigned column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->isValid(row_):sensitive();
}

unsigned long MSTable::cellForeground(unsigned row_,unsigned column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn==0) 
   {
     if (foregroundColors().length()==0) return foreground();
     else return foregroundColors()(row_%foregroundColors().length());
   }
  else return pTableColumn->cellForeground(row_);
}

unsigned long MSTable::cellBackground(unsigned row_,unsigned column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn==0) 
   {
     if (backgroundColors().length()==0) return background();
     else return backgroundColors()(row_%backgroundColors().length());
   }
  else return pTableColumn->cellBackground(row_);
}

Font MSTable::cellFont(unsigned row_,unsigned column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->cellFont(row_):font();
}

MSAlignment MSTable::cellAlignment(unsigned row_,unsigned column_) 
{
  MSTableColumn *pTableColumn=tableColumn(column_);
  return (pTableColumn!=0)?pTableColumn->cellAlignment(row_):MSRight;
}

void MSTable::drawGroupHeadings(Window window_)
{
  if (groupHeadingsHeight()>0)
   {
     if (fixedColumns()>0)
      {
        if (numColumns()<=columns())
         {
           drawGroupHeadings(window_,0,lastColumn());
         }
        else
         {
           drawGroupHeadings(window_,0,fixedColumns()-1);
           drawGroupHeadings(window_,firstColumn(),lastColumn());
         }
      }
     else
      {
        drawGroupHeadings(window_,firstColumn(),lastColumn());
      }
   }
}

void MSTable::calculateGroupHeadingsHeight(void)
{
  groupHeadingsHeightVector().removeAll();
  unsigned len=columnList()->count();
  for (unsigned i=0;i<len;i++)
   {
     MSTableColumn *col=columnList()->array(i);
     calculateGroupHeadingsHeight(col,groupHeadingsHeightVector());
   }
  int groupHeight=(int) groupHeadingsHeightVector().sum();
  groupHeadingsHeight(groupHeight);
}

void MSTable::calculateGroupHeadingsHeight(MSTableColumn *column_,
                                           MSIntVector &heightVector_)
{
  unsigned len=column_->groupList().length();
  for (unsigned i=0;i<len;i++)
   {
     const MSTableColumnGroup *group=column_->groupList()(i);
     int groupHeight=0;
     MSFontObject fontObj;
     if (group->heading().length()!=0)
      {
        fontObj.fontStruct(server()->fontStruct(group->font()));
        groupHeight=fontObj.textHeight()*group->heading().length()+rowSpacing();
      }
     if (heightVector_.length()==i) heightVector_<<groupHeight;
     else heightVector_[i]=MSUtil::max(heightVector_(i),groupHeight);
   }
}

void MSTable::columnGroups(const MSTableColumnGroup &group_)
{
  if (sanityCheck(group_)==MSTrue)
   {
     //Cast away the const for now.
     MSTableColumnGroup &group=(MSTableColumnGroup &)group_;
     //First clean up all old group information
     unsigned i,num=columnList()->count();
     for (i=0;i<num;i++)
      {
        MSTableColumn *column=columnList()->array(i);
        if (column!=0) column->groupList().removeAll();
      }
     num=hiddenColumnList()->count();
     for (i=0;i<num;i++)
      {
        MSTableColumn *column=hiddenColumnList()->array(i);
        if (column!=0) column->groupList().removeAll();
      }
     columnGroupList().removeAll();
     MSTableGroupIterator groupIterator(columnGroupList());
     group.allNodesDo(groupIterator);
     MSWidgetVector vector;
     MSTableColumnIterator columnIterator(vector,columnGroupList());
     group.allNodesDo(columnIterator);
     permuteColumns(vector);
   }
  else
   {
     MSMessageLog::warningMessage("Warning: MSTable - MSTableColumnGroup contains stale pointer to column");
   }
}

MSBoolean MSTable::sanityCheck(const MSTableColumnGroup &group_) const
{
  MSBoolean sanity=MSTrue;
  MSTable *t=(MSTable *)this;
  MSWidgetVector vector=t->children();
  MSTableSanityCheck check(vector,sanity);
  group_.allNodesDo(check);
  return sanity;
}

void MSTable::permuteColumns(const MSWidgetVector& aWidgetVector_)
{
  MSBoolean wasFrozen=frozen();
  if (editor()->mapped()==MSTrue) unmapEditor();
  if (wasFrozen==MSFalse) freeze();
  MSReportTable::permuteColumns(aWidgetVector_);
  if (wasFrozen==MSFalse) adjustView();
  updateInternalState();  
  if (wasFrozen==MSFalse) unfreeze();
}

void MSTable::drawGroupHeading(Window window_,MSTableColumnGroup *group_,int cs_,int ce_,int level_)
{
  int x=computeXCoord(cs_);
  int y=panner()->highlightThickness()+panner()->shadowThickness();
  for (int i=1;i<=level_;i++) y+=groupHeadingsHeightVector()(i-1);
  int w=computeXCoord(ce_)-x+columnPixelWidth(ce_);
  int h=groupHeadingsHeightVector()(level_);
  MSRect rect(x,y,w,h);
  drawGroupHeading(window_,group_,rect);
  drawHSeparator(window_,x,y+h-rowSpacing(),w,rowSpacing());
  if (level_==groupHeadingsHeightVector().length()-1) h+=(columnHeadingsHeight()-rowSpacing());
  drawVSeparator(window_,x+w-rowSpacing(),y,rowSpacing(),h);
}

void MSTable::drawGroupHeadings(Window window_,int cs_,int ce_)
{
  if (groupHeadingsHeight()>0) 
   {
     int x,y,w,h;
     x=computeXCoord(cs_);
     y=panner()->highlightThickness()+panner()->shadowThickness();
     w=computeXCoord(ce_)-x+columnPixelWidth(ce_);
     h=groupHeadingsHeight();
     XFillRectangle(display(),window_,backgroundShadowGC(),x,y,w,h);
     unsigned len=groupHeadingsHeightVector().length();
     unsigned lastRow=groupHeadingsHeightVector().length()-1;
     for (unsigned i=0;i<len;i++)
      {
        if (groupHeadingsHeightVector()(i)>0)
         {
           int groupStart=cs_;
           if (i>0) y+=groupHeadingsHeightVector()(i-1);
           h=groupHeadingsHeightVector()(i);
           for (int col=cs_;col<=ce_;col++)
            {
              ColumnGroupList &columnGroupList=tableColumn(col)->groupList();
              if (columnGroupList.length()>i)
               {
                 MSTableColumnGroup *group=columnGroupList[i];
                 //if it's the last column, draw it
                 if (col==ce_)
                  {
                    //Draw it
                    drawGroupHeading(window_,group,groupStart,col,i);
                    groupStart=col+1;
                  }
                 else 
                  {
                    ColumnGroupList &nextColumnGroupList=tableColumn(col+1)->groupList();
                    if (nextColumnGroupList.length()>i)
                     {
                       MSTableColumnGroup *nextGroup=nextColumnGroupList[i];
                       if (group!=nextGroup)
                        {
                          //Draw it
                          drawGroupHeading(window_,group,groupStart,col,i);
                          groupStart=col+1;
                        }
                     }
                    else
                     {
                       //Draw it
                       drawGroupHeading(window_,group,groupStart,col,i);
                       groupStart=col+1;
                     }
                  }
               }
              else
               {
                 //draw separator
                 x=computeXCoord(col);
                 w=columnPixelWidth(col);
                 int sepH=h;
                 if (i==lastRow) sepH+=(columnHeadingsHeight()-rowSpacing());
                 drawVSeparator(window_,x+w-rowSpacing(),y,rowSpacing(),sepH);
                 groupStart=col+1;
               }
            }
         }
      }
   }
}

void MSTable::drawGroupHeading(Window window_,MSTableColumnGroup *group_,MSRect &aRect_)
{
  int len=group_->heading().length();
  if (len>0)
   {
     MSFontObject fs(server()->fontStruct(group_->font()));
     XSetForeground(display(),textGC(),group_->foreground());
     XSetBackground(display(),textGC(),panner()->background());
     XSetFont(display(),textGC(),group_->font());
     int totalHeight=fs.textHeight()*len;
     int ydelta=(aRect_.height()-totalHeight)/2;
     int y=aRect_.y()+ydelta;
     int cw=aRect_.width();
     for (unsigned i=0;i<len;i++)
      {
        const MSString& aString=group_->heading()(i);
        int nc=aString.length();
        int tw=0;
        const char *pString=aString.string();
        tw=fs.textWidth(pString,nc);
        int xdelta;
        if (nc>0)
         {
           if (tw>cw) 
            {
              nc=computeMaxTextLength(fs.fontStruct(),pString,cw,nc);
              xdelta=0;
            }
           else
            {
              xdelta=(cw>tw)?(cw-tw)>>1:0;
            }
           XDrawImageString(display(),window_,textGC(),fs.fontStruct(),
                            aRect_.x()+xdelta,y+fs.textAscent(),pString,nc);
         }
        y+=fs.textHeight();
      }
   }
}

int MSTable::computeXCoord(int column_)
{
  return MSArrayView::computeXCoord(column_);
}

int MSTable::computeXCoord(MSTableColumn *column_)
{
  //This method compute the X coordinate of the upper left corner of the specified column
  int xr=panner()->highlightThickness()+panner()->shadowThickness()+labelWidth();
  unsigned fc=fixedColumns();
  unsigned i;
  for (i=0;i<fc;i++)
   {
     MSTableColumn *col=tableColumn(i);
     if (col!=column_) xr+=col->columnPixelWidth();
     else return xr;
   }
  unsigned nCols=numColumns();
  for (i=firstColumn();i<nCols;i++)
   {
     MSTableColumn *col=tableColumn(i);
     if (col!=column_) xr+=col->columnPixelWidth();
     else return xr;
   }
  return xr;
}

int MSTable::columnHeadingsOffset(void)
{
  return panner()->highlightThickness()+panner()->shadowThickness()+groupHeadingsHeight();
}

void MSTable::redrawHeadings(Window window_,int cs_,int ce_)
{
  if (mapped()==MSTrue&&frozen()==MSFalse&&headingsHeight()>0) 
   {
     if (groupHeadingsHeight()>0) drawHeadings(window_);
     else
      {
        drawColumnHeadings(window_,cs_,ce_);
        drawGroupHeadings(window_);
      }

   }
}

void MSTable::drawHSeparator(Window window_,int x_,int y_,int width_,int height_)
{
  if (height_>1)
   {
     int halfHeight=height_/2;
     XBFillRectangle(display(),window_,bottomShadowGC(),x_,y_,width_,halfHeight);
     XFillRectangle(display(),window_,topShadowGC(),x_,y_+halfHeight,width_,halfHeight);
   }
}

void MSTable::drawVSeparator(Window window_,int x_,int y_,int width_,int height_)
{
  if (width_>1)
   {
     int halfWidth=width_/2;
     XBFillRectangle(display(),window_,bottomShadowGC(),x_,y_,halfWidth,height_);
     XFillRectangle(display(),window_,topShadowGC(),x_+halfWidth,y_,halfWidth,height_);
   }
}

MSTableColumnGroup MSTable::columnGroups(void) const
{
  unsigned nCols=numColumns();
  MSTableColumnGroup group((MSTable *)this);
  for (unsigned i=0;i<nCols;i++)
   {
     MSTableColumn *col=(MSTableColumn *)tableColumn(i);  //Cast away const-ness here
     const ColumnGroupList &groupList=col->_groupList;
     MSTableColumnGroup *g=&group;
     unsigned len=groupList.length();
     unsigned j=0;
     for (j=0;j<len;j++)
      {
        if (g->isEmpty()==MSFalse&&
            g->nodeList().lastElement().type()==MSTableColumnGroup::Node::Group)
         {
           if (g->nodeList().lastElement().group()->shallowCompare(*groupList(j))==MSTrue)
            {
              g=g->nodeList().lastElement().group();
            }
           else break;
         }
        else break;
      }
     for (;j<len;j++)
      {
        *g<<*groupList(j);
        g=g->nodeList().lastElement().group();
      }
     *g<<col;
   }
  if (group.nodeList().length()==1 &&
      group.nodeList()[0].type()==MSTableColumnGroup::Node::Group)
   {
     return *(group.nodeList()[0].group());
   }
  else return group;
}


unsigned long MSTable::groupForeground(const MSSymbol &groupTag_) const
{
  unsigned len=columnGroupList().length();
  for (unsigned i=0;i<len;i++)
   {
     const MSManagedPointer<MSTableColumnGroup> &group=columnGroupList()[i];
     if (group->tag()==groupTag_) return group->foreground();
   }
  return server()->defaultForeground();
}

void MSTable::groupForeground(const MSSymbol &groupTag_,const char *foreground_)
{
  groupForeground(groupTag_,server()->pixel(foreground_));
}

void MSTable::groupForeground(const MSSymbol &groupTag_,unsigned long foreground_)
{
  unsigned len=columnGroupList().length();
  for (unsigned i=0;i<len;i++)
   {
     MSManagedPointer<MSTableColumnGroup> &group=columnGroupList()[i];
     if (group->tag()==groupTag_)
      {
        if (group->foreground()!=foreground_)
         {
           group->foreground(foreground_);
           drawGroupHeadings(redrawPixmap()->pixmap());
           int x=panner()->shadowThickness()+panner()->highlightThickness();
           int y=x;
           int w=computeXCoord(lastColumn())+columnPixelWidth(lastColumn())-x;
           int h=groupHeadingsHeight();
           XCopyArea(display(),redrawPixmap()->pixmap(),panner()->window(),backgroundShadowGC(),
                     x,y,w,h,x,y);
         }
        return;
      }
   }
}

Font MSTable::groupFont(const MSSymbol &groupTag_) const
{
  unsigned len=columnGroupList().length();
  for (unsigned i=0;i<len;i++)
   {
     const MSManagedPointer<MSTableColumnGroup> &group=columnGroupList()[i];
     if (group->tag()==groupTag_) return group->font();
   }
  return server()->defaultFont();
}

void MSTable::groupFont(const MSSymbol &groupTag_,const char *font_)
{
  groupFont(groupTag_,server()->fontID(font_));
}

void MSTable::groupFont(const MSSymbol &groupTag_,Font font_)
{
  unsigned len=columnGroupList().length();
  for (unsigned i=0;i<len;i++)
   {
     MSManagedPointer<MSTableColumnGroup> &group=columnGroupList()[i];
     if (group->tag()==groupTag_)
      {
        if (group->font()!=font_)
         {
           group->font(font_);
           calculateHeadingsHeight();
           adjustNumVisible();
           redrawImmediately(); 
         }
        return;
      }
   }
}

const MSStringVector &MSTable::groupHeading(const MSSymbol &groupTag_) const
{
  static MSStringVector emptyVector;
  unsigned len=columnGroupList().length();
  for (unsigned i=0;i<len;i++)
   {
     const MSManagedPointer<MSTableColumnGroup> &group=columnGroupList()[i];
     if (group->tag()==groupTag_) return group->heading();
   }
  return emptyVector;
}

void MSTable::groupHeading(const MSSymbol &groupTag_,const MSStringVector &heading_)
{
  unsigned len=columnGroupList().length();
  for (unsigned i=0;i<len;i++)
   {
     MSManagedPointer<MSTableColumnGroup> &group=columnGroupList()[i];
     if (group->tag()==groupTag_)
      {
        if (group->heading()!=heading_)
         {
           group->heading(heading_);
           calculateHeadingsHeight();
           adjustNumVisible();
           redrawImmediately(); 
         }
        return;
      }
   }

}

MSWidgetVector MSTable::columnsOfGroup(const MSSymbol &groupTag_)
{
  MSWidgetVector columns;
  unsigned i,num=columnList()->count();
  for (i=0;i<num;i++)
   {
     MSTableColumn *column=columnList()->array(i);
     if (column!=0)
      {
        ColumnGroupList &list=column->groupList();
        unsigned len=list.length();
        for (unsigned j=0;j<len;j++)
         {
           const MSManagedPointer<MSTableColumnGroup> &group=list[j];
           if (group->tag()==groupTag_)
            {
              columns<<(MSWidget *)column;
              break;
            }
         }
      }
   }
  num=hiddenColumnList()->count();
  for (i=0;i<num;i++)
   {
     MSTableColumn *column=hiddenColumnList()->array(i);
     if (column!=0)
      {
        ColumnGroupList &list=column->groupList();
        unsigned len=list.length();
        for (unsigned j=0;j<len;j++)
         {
           const MSManagedPointer<MSTableColumnGroup> &group=list[j];
           if (group->tag()==groupTag_)
            {
              columns<<(MSWidget *)column;
              break;
            }
         }
      }
   }
  return columns;
}
 

void MSTable::dragRow(const XEvent *pEvent_)
{
  if (showBreaks()==MSFalse)
   {
     MSArrayView::dragRow(pEvent_);
   }
  else server()->bell();
}

void MSTable::moveRow(int from_,int to_)
{
  unsigned i,n=numColumns();
  for (i=0;i<n;i++)
   {
     tableColumn(i)->moveRow(from_,to_);
   }
  
  n=hiddenColumnList()->count();
  for (i=0;i<n;i++)
   {
     ((MSTableColumn *)hiddenColumnList()->array(i))->moveRow(from_,to_);
   }
}

void MSTable::choiceStyle(ChoiceStyle choiceStyle_)
{
  if (_choiceStyle!=choiceStyle_)
    {
      _choiceStyle=choiceStyle_;
      updateChoices();
      redrawImmediately();
    }
}

MSTable::ChoiceStyle MSTable::choiceStyle(void) const
{
  return _choiceStyle;
}

void MSTable::showOptions(int row_,int column_)
{
  const MSTableColumn *pTableColumn=tableColumn(column_);
  if (pTableColumn!=0) 
    {
      if (row_<numRows())
	{
	  const MSStringVector &options=cellOptions(row_,column_);
	  if (options.length()>0)
	    {
	      if (_choicesMenu==0) 
		{
		  _choicesMenu=new ColumnPopupMenu(this);
		  // Should call calculateNaturalSize() here, but it is protected
		}
	      _choicesMenu->choices(options);
	      _choicesMenu->background(pTableColumn->background());
	      _choicesMenu->foreground(pTableColumn->foreground());
	      _choicesMenu->font(pTableColumn->font());		

	      int selectedItem;
	      MSString buffer;
	      formatOutput(buffer,row_,column_);
	      if ((selectedItem=options.indexOf(buffer))==options.length()) selectedItem=0;
	      int rootx,rooty;
	      cellRootXY(row_,column_,rootx,rooty);
	      rooty-=2;
	      MSMenuItem *item=_choicesMenu->taggedMenuItem(selectedItem);
	      if (item!=0)
		{
		  rooty-=item->y();
		}
	      _choicesMenu->moveTo(rootx,rooty);
	      _choicesMenu->show();
	      if (item!=0)
		{
		  int movex=item->x()+item->width()/2;
		  int movey=item->y()+item->height()/2;
		  XWarpPointer(display(),None,_choicesMenu->window(),0,0,0,0,movex,movey);
		  // Send a synthetic event to the choice menu
		  XMotionEvent motionEvent;
		  motionEvent.display=display();
		  motionEvent.window=_choicesMenu->window();
		  motionEvent.root=server()->root();
		  motionEvent.state=Button1Mask;
		  motionEvent.x=movex;
		  motionEvent.y=movey;
		  motionEvent.x_root=rootx+movex;
		  motionEvent.y_root=rooty+movey;
		  buttonMotionNotify(_choicesMenu,(const XEvent *)&motionEvent);
		}
	    }
	}
    }
}

void MSTable::cellRootXY(int row_,int column_,int &rootx_,int &rooty_)
{
  panner()->rootXY(rootx_,rooty_);
  rootx_+=computeXCoord(column_);
  rooty_+=computeYCoord(row_);
}

void MSTable::updateChoices(void)
{
  if(firstMap()==MSTrue&&frozen()==MSFalse)
   {
     int rh=rowHeight();
     calculateRowHeight();
     if(rowHeight()!=rh)
      {
        adjustNumVisible();
        redrawImmediately();
      }
   }
}
