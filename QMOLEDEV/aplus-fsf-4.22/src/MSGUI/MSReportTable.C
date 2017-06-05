/////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <unistd.h>
#include <MSGUI/MSReportTable.H>
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSPrintFontData.H>
#include <MSTypes/MSMessageLog.H>

extern const int MSPointsPerInch;

MSReportTable::MSReportTable(const char *title_,const MSSymbol& tag_) :
_reportTitle(title_), _tag(tag_) { init(); }
MSReportTable::MSReportTable(const MSStringVector& title_,const MSSymbol& tag_) :
_reportTitle(title_), _tag(tag_) { init(); }

void MSReportTable::init(void)
{
  _columnList=new MSPointerArray<MSTableColumn>;
  _hiddenColumnList=new MSPointerArray<MSTableColumn>;
  _fixedReportColumns=-1;
  _numRows=0;
  _style=MSNone;
  _breakStyle=MSLeft;
  _reportTotalStyle=MSLeft;
  _headingStyle=0;
  _headingForeground=0;
  _groupHeadingList=0;
  _breakTextList=0;
  _reportTotalLeading=2;
  _grandTotalBaseText=0;
  _reportTotalOn=MSFalse;
  _reportGrandTotalOn=MSFalse;
  _pageTotalOn=MSFalse;
  _outputStyle=MSP::RowMajor;
  _report=0;
  _groupHeadingHeight=0;
 
  _frameStyle=MSNone;
  _frameOffset=2;
  _frameLineWidth=0;
  _tag=MSSymbol::nullSymbol();
  _orphanRowHeight=0;
  _headingHeight=0;
  _reportHeadingOffset=2;
  _breakOffset=5;
  _breakLeading=2;
  _fgGrayScale=0.;
  _bgGrayScale=1;
  _headingFgGrayScale=0;
  _headingBgGrayScale=1;
  _widowRows=1;
  _orphanRows=1;
  _rowPageSpan=0;
  _columnPageSpan=0;
  _newspaperColumns=1;
  _x_printScale=1;
  _y_printScale=1;
  _tableLeft=0;
  _tableRight=0;
  _tableTop=0;
  _tableBottom=0;
  _pageWidth=0;
  _pageHeight=0;
  _segment=0;
  _pageCount=0;
  _lastPageCount=0;
  _delimiter=" ";
  _fixedFieldWidth=0;
  _pageCountTotal=0;
  _scaleHeaders=MSFalse;
  destroyable(MSFalse);
  topPixel(0);
  bottomPixel(4);
}

MSReportTable::~MSReportTable(void)
{
  destroyable(MSFalse);
  unsigned i,n=columnList()->count();
  for (i=n-1;i<n;i--) delete columnList()->array(i);
  delete _columnList;
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--) delete hiddenColumnList()->array(i);
  delete _hiddenColumnList;
  if (groupHeadingList()!=0)
   {
     for (unsigned i=0;i<groupHeadingList()->count();i++) delete groupHeading(i);
     delete _groupHeadingList;
   }
  if (breakTextList()!=0) delete _breakTextList;
  if (_grandTotalBaseText!=0) delete _grandTotalBaseText;
  if (printManager()!=0) printManager()->removePrintItem(this);
  if (_report!=0) delete _report;
}
////////////////////////////////////////////////////////////////////////////
//
// screen related methods
//
////////////////////////////////////////////////////////////////////////////

void MSReportTable::updateScreen(void) {}

unsigned MSReportTable::dataRows(void) const
{return _numRows;}
unsigned MSReportTable::numColumns(void) const
{return columnList()->count();}

void MSReportTable::headingForeground(const char *fg_)
{ 
  headingForeground(convertForeground(fg_));
}

const char *MSReportTable::formatOutput(MSString &buffer_,unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  if (field!=0) field->formatOutput(buffer_,row_);
  return buffer_.string();
}

void MSReportTable::headingForeground(unsigned long fg_)
{ 
  if (headingForeground()!=fg_)
   {
     unsigned i;
     for (i=0;i<columnList()->count();i++)
      {
        if (reportColumn(i)->headingForeground()==headingForeground()) reportColumn(i)->headingForeground(fg_);
      }
     for (i=0;i<hiddenColumnList()->count();i++)
      {
	MSTableColumn *tc=hiddenColumnList()->array(i);
	if (tc->headingForeground()==headingForeground()) tc->headingForeground(fg_);
      }
     _headingForeground=fg_;
   }
}

MSSymbolVector MSReportTable::tags(void)
{
  MSSymbolVector symbols;
  for (unsigned i=0;i<columnList()->count();i++)
   {
     if (columnList()->array(i)->tag()!=MSSymbol::nullSymbol())
      {
        symbols<<columnList()->array(i)->tag();
      }
   }
  return symbols; 
}

MSSymbolVector MSReportTable::tags(void) const
{
  MSSymbolVector symbols;
  for (unsigned i=0;i<columnList()->count();i++)
   {
     if (columnList()->array(i)->tag()!=MSSymbol::nullSymbol())
      {
        symbols<<columnList()->array(i)->tag();
      }
   }
  return symbols; 
}

MSSymbolVector MSReportTable::hiddenTags(void) const
{
  MSSymbolVector symbols;
  for (unsigned i=0;i<hiddenColumnList()->count();i++)
   {
     if (hiddenColumnList()->array(i)->tag()!=MSSymbol::nullSymbol())
      {
        symbols<<hiddenColumnList()->array(i)->tag();
      }  
   }  
  return symbols; 
}

MSSymbolVector MSReportTable::hiddenTags(void)
{
  MSSymbolVector symbols;
  for (unsigned i=0;i<hiddenColumnList()->count();i++)
   {
     if (hiddenColumnList()->array(i)->tag()!=MSSymbol::nullSymbol())
      {
        symbols<<hiddenColumnList()->array(i)->tag();
      }
   }
  return symbols; 
}

void MSReportTable::addColumn(MSTableColumn *col_)
{ columnList()->add(col_); }

void MSReportTable::removeColumn(MSTableColumn *col_)
{ 
  if (columnList()->remove(col_)==MSTrue)
   {
     for (unsigned i=0;i<columnList()->count();i++) columnList()->array(i)->column(i);
   }
}

void MSReportTable::exchangeColumns(unsigned int x_,unsigned int y_)
{ columnList()->exchange(x_,y_); }

MSTableColumn *MSReportTable::reportColumn(unsigned column_)
{ return (MSTableColumn *)columnList()->array(column_); }
MSTableColumn *MSReportTable::reportColumn(const MSSymbol& tag_)
{ 
  unsigned i,n=columnList()->count();
  for (i=0;i<n;i++) if (tag_==reportColumn(i)->tag()) return reportColumn(i);
  n=hiddenColumnList()->count();
  for (i=0;i<n;i++) if (tag_==hiddenColumnList()->array(i)->tag())
   {
     return (MSTableColumn *)hiddenColumnList()->array(i);
   }
  return 0;
}

const MSTableColumn *MSReportTable::reportColumn(unsigned column_) const
{ return (MSTableColumn *)columnList()->array(column_); }
const MSTableColumn *MSReportTable::reportColumn(const MSSymbol& tag_) const
{ 
  unsigned i,n=columnList()->count();
  for (i=0;i<n;i++) if (tag_==reportColumn(i)->tag()) return reportColumn(i);
  n=hiddenColumnList()->count();
  for (i=0;i<n;i++) if (tag_==hiddenColumnList()->array(i)->tag())
   {
     return (MSTableColumn *)hiddenColumnList()->array(i);
   }
  return 0;
}

unsigned MSReportTable::numRows(unsigned col_) const
{ 
  const MSTableColumn *var=reportColumn(col_); 
  return (var!=0)?var->numRows():0; 
}

void MSReportTable::maxRowsSet(unsigned rows_)
{ _numRows=rows_>_numRows?rows_:_numRows; }

void MSReportTable::placeColumnAt(MSTableColumn *pColumn_,unsigned index_)
{
  columnList()->insert(pColumn_,index_);
  resetColumnLocations();  
}

void MSReportTable::permuteColumns(const MSIndexVector& aIndexVector_)
{
  MSPointerArray<MSTableColumn> *newColumnList=new MSPointerArray<MSTableColumn>;
  MSTableColumn *pBaseColumn;
  MSIndexVector iv;
  unsigned i,index,n=columnList()->count();
  for (i=0;i<aIndexVector_.length();i++)
   {
     index=aIndexVector_(i);
     pBaseColumn=columnList()->array(index);
     if (pBaseColumn!=0)
      {
	 newColumnList->add(pBaseColumn);
	 iv<<index;
      }
   }

  for (i=0;i<iv.length();i++) columnList()->assign(0,iv(i));
  for (i=0;i<n;i++) if ((pBaseColumn=reportColumn(i))!=0) hiddenColumnList()->add(pBaseColumn);

  delete _columnList;
  _columnList=newColumnList;
  resetColumnLocations();
}

void MSReportTable::permuteColumns(const MSSymbolVector& aSymbolVector_)
{
  MSPointerArray<MSTableColumn> *newColumnList=new MSPointerArray<MSTableColumn>;
  MSPointerArray<MSTableColumn> *newHiddenColumnList=new MSPointerArray<MSTableColumn>;  
  MSTableColumn *pBaseColumn;
  unsigned i,j,index,n=columnList()->count(),k=hiddenColumnList()->count();
  for (i=0;i<aSymbolVector_.length();i++)
   {
     const MSSymbol& columnTag=aSymbolVector_(i);
     pBaseColumn=0;
     for (j=0;j<n;j++) // search the columnList
      {
	if (columnList()->array(j)!=0&&columnList()->array(j)->tag()==columnTag)
	 {
	   index=j;
	   pBaseColumn=columnList()->array(index);
	   newColumnList->add(pBaseColumn);
	   columnList()->assign(0,index);
	   break;
	 }
      }
     if (pBaseColumn==0) // did not find it, so search the hiddenColumnList
      {
	 for (j=0;j<k;j++)
	 {
	    if (hiddenColumnList()->array(j)!=0&&hiddenColumnList()->array(j)->tag()==columnTag)
	    {
	       index=j;
	       pBaseColumn=hiddenColumnList()->array(index);
	       newColumnList->add(pBaseColumn);
	       hiddenColumnList()->assign(0,index);
	       break;
	    }
	 }
      }
   }
  for (i=0;i<k;i++)
  {
    if ((pBaseColumn=hiddenColumnList()->array(i))!=0) newHiddenColumnList->add(pBaseColumn);
  }
  for (i=0;i<n;i++)
  {
    if ((pBaseColumn=columnList()->array(i))!=0) newHiddenColumnList->add(pBaseColumn);
  }

  delete _columnList;
  delete _hiddenColumnList;
  _columnList=newColumnList;
  _hiddenColumnList=newHiddenColumnList;  
  resetColumnLocations();
}

void MSReportTable::permuteColumns(const MSWidgetVector &vector_)
{
  MSPointerArray<MSTableColumn> *newColumnList=new MSPointerArray<MSTableColumn>;
  MSPointerArray<MSTableColumn> *newHiddenColumnList=new MSPointerArray<MSTableColumn>;  
  MSTableColumn *pBaseColumn;
  unsigned i,j,index,n=columnList()->count(),k=hiddenColumnList()->count();
  unsigned len=vector_.length();
  for (i=0;i<len;i++)
   {
     const MSWidget *column=vector_(i);
     pBaseColumn=0;
     for (j=0;j<n;j++) // search the columnList
      {
	if (columnList()->array(j)!=0&&columnList()->array(j)==column)
	 {
	   index=j;
	   pBaseColumn=columnList()->array(index);
	   newColumnList->add(pBaseColumn);
	   columnList()->assign(0,index);
	   break;
	 }
      }
     if (pBaseColumn==0) // did not find it, so search the hiddenColumnList
      {
	 for (j=0;j<k;j++)
	 {
	    if (hiddenColumnList()->array(j)!=0&&hiddenColumnList()->array(j)==column)
	    {
	       index=j;
	       pBaseColumn=hiddenColumnList()->array(index);
	       newColumnList->add(pBaseColumn);
	       hiddenColumnList()->assign(0,index);
	       break;
	    }
	 }
      }
   }
  for (i=0;i<k;i++)
  {
    if ((pBaseColumn=hiddenColumnList()->array(i))!=0) newHiddenColumnList->add(pBaseColumn);
  }
  for (i=0;i<n;i++)
  {
    if ((pBaseColumn=columnList()->array(i))!=0) newHiddenColumnList->add(pBaseColumn);
  }

  delete _columnList;
  delete _hiddenColumnList;
  _columnList=newColumnList;
  _hiddenColumnList=newHiddenColumnList;  
  resetColumnLocations();

}

void MSReportTable::resetColumnLocations(void)
{
  for (unsigned i=0;i<columnList()->count();i++) reportColumn(i)->column(i); 
}

void MSReportTable::updateBreaks(unsigned column_)
{
  MSTableColumn *tc=reportColumn(column_);
  if (tc->breakOn()!=MSTrue) tc->breakString().removeAll();
  unsigned columns=numColumns()-1;
  unsigned rows=numRows()+1;
  for (unsigned i=1;i<rows;i++)
   {
     for (unsigned index,j=columns;j<=columns;j--)
      {
        MSTableColumn *field=reportColumn(j);
        if (field->breakOn()==MSTrue)
         {
           if ((index=field->breakIndex().indexOf(i))<field->breakIndex().length())
            {
              MSIndexVector iv;
              tc->breakProcess(iv<<field->breakIndex()(index-1)<<field->breakIndex()(index));
            }
         }
      }
   }
  if (reportTotalOn()==MSTrue)
   {
     MSIndexVector index;
     tc->breakProcess(index<<0<<numRows());
   }
}

void MSReportTable::updateBreaks(void)
{
  MSIndexVector index;
  unsigned i;
  for (i=0;i<columnList()->count();i++)
   {
     MSTableColumn *tc=reportColumn(i);
     if (tc->breakOn()!=MSTrue) tc->breakString().removeAll();
   }
  unsigned columns=numColumns()-1;
  unsigned rows=numRows()+1;
  for (i=1;i<rows;i++)
   {
     for (unsigned index,j=columns;j<=columns;j--)
      {
        MSTableColumn *field=reportColumn(j);
        if (field->breakOn()==MSTrue)
         {
           if ((index=field->breakIndex().indexOf(i))<field->breakIndex().length())
            {
              MSIndexVector iv;
              breakProcess(iv<<field->breakIndex()(index-1)<<field->breakIndex()(index));
            }
         }
      }
   }
}

void MSReportTable::computeBreaks(void)
{
  clearBreaks();
  grandTotal()=0;
  breakIndex().removeAll();
  pageBreakIndex().removeAll();
  breakColumn().removeAll();
  breakTextColumn().removeAll();
  
  int n=numColumns()-1;
  if(n<0) return;
  for (unsigned i=0;i<numRows()+1;i++)
    {
      for (unsigned j=n;j<=n;j--)
       {
	MSTableColumn *field=reportColumn(j);
	if (field!=0&&field->breakOn()==MSTrue&&
	    ((i<field->numRows()&&field->breakCriteria(i)==MSTrue)||i==field->numRows()))
	 {
	   breakIndex()<<i;
	   breakColumn()<<j;
	   if (field->breakTextList().count()>=field->breakIndex().length()) breakTextColumn()<<j;
	   if (field->pageBreakOn()==MSTrue) pageBreakIndex()<<i;
	   field->breakIndex()<<i;
	   breakProcess(field->breakIndex());
	 }
       }
    }
  if (reportTotalOn()==MSTrue||reportGrandTotalOn()==MSTrue)
   {
     computeColumnTotals();
     if (reportTotalOn()==MSTrue)
      {
	breakIndex()<<numRows();
	breakColumn()<<0;
      }
     if (reportGrandTotalOn()==MSTrue)
      {
        
	if (_grandTotalBaseText!=0) delete _grandTotalBaseText;
	_grandTotalBaseText=new MSParagraph(grandTotalText());
	MSStringVector sv(grandTotalText().text());
        MSFloat aFloat(grandTotal());
        MSString buffer;
        MSFloat::MSFloatFormat format=report()->asciiFloatFormat()!=0?report()->asciiFloatFormat():
          MSFloat::Decimal0;
        aFloat.format(buffer,MSFormat(format));
	if (sv.length()>0) sv[sv.length()-1]<<buffer;
	else sv<<buffer;
	grandTotalBaseText()=sv;
        if (grandTotalBaseText().column()>numColumns()) grandTotalBaseText().column(numColumns()-1);
      }
   }
  breakIndex().sortUp();
  pageBreakIndex().sortUp();
}

MSIndexVector MSReportTable::sortUp(const MSIndexVector &sortVector_)
{
  MSIndexVector iv;
  if (sortVector_.length()>0)
   {
     unsigned i,j;
     MSTableColumn *column;
     for (i=0;i<sortVector_.length();i++)
      {
	if ((column=columnList()->array(sortVector_(i)))!=0)
	 {
	   if (column->model()!=0) break;
	   else column=0;
	 }
      }
     if (column!=0)
      {
	unsigned startIndex=i+1;
	MSIndexVector indexVector=column->gradeUp();
	iv=indexVector;
	unsigned count=columnList()->count(),hcount=hiddenColumnList()->count();
	for (i=0;i<count;i++)
	 {
	   MSTableColumn *col=columnList()->array(i);
	   if (col!=0&&col->model()!=0)
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	for (i=0;i<hcount;i++)
	 {
	   MSTableColumn *col=hiddenColumnList()->array(i);
	   if (col!=0&&col->model()!=0)
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	MSIndexVector start,end;
	start.append(0);
	end.append(indexVector.length()-1);
	for (i=startIndex;i<sortVector_.length();i++)
	 {
	   column->range(start,end);
	   MSTableColumn *nextcol=columnList()->array(sortVector_(i));
	   if (nextcol!=0&&nextcol->model()!=0)
	    {
	      indexVector=nextcol->rangeGradeUp(start,end);
	      for (j=0;j<count;j++)
	       {
		 MSTableColumn *col=columnList()->array(j);
		 if (col!=0&&col->model()!=0)
		  {
		    col->permute(indexVector);
		    if (col->weights().length()>0) col->weights().permute(indexVector);
		  }
	       }
	      for (j=0;j<hcount;j++)
	       {
		 MSTableColumn *col=hiddenColumnList()->array(j);
		 if (col!=0&&col->model()!=0)
		  {
		    col->permute(indexVector);
		    if (col->weights().length()>0) col->weights().permute(indexVector);
		  }
	       }
	      iv.permute(indexVector);
	      column=nextcol;
	    }
	 }
      }
   }
  return iv;
}

MSIndexVector MSReportTable::sortDown(const MSIndexVector &sortVector_)
{
  MSIndexVector iv;
  if (sortVector_.length()>0)
   {
     unsigned i,j;
     MSTableColumn *column;
     for (i=0;i<sortVector_.length();i++)
      {
	if ((column=columnList()->array(sortVector_(i)))!=0)
	 {
	   if (column->model()!=0) break;
	   else column=0;
	 }
      }
     if (column!=0)
      {
	unsigned startIndex=i+1;
	MSIndexVector indexVector=column->gradeDown();
	iv=indexVector;
	unsigned count=columnList()->count(),hcount=hiddenColumnList()->count();
	for (i=0;i<count;i++)
	 {
	   MSTableColumn *col=columnList()->array(i);
	   if (col!=0&&col->model()!=0)
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	for (i=0;i<hcount;i++)
	 {
	   MSTableColumn *col=hiddenColumnList()->array(i);
	   if (col!=0&&col->model()!=0)
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	MSIndexVector start,end;
	start.append(0);
	end.append(indexVector.length()-1);
	for (i=startIndex;i<sortVector_.length();i++)
	 {
	   column->range(start,end);
	   MSTableColumn *nextcol=columnList()->array(sortVector_(i));
	   if (nextcol!=0&&nextcol->model()!=0)
	    {
	      indexVector=nextcol->rangeGradeDown(start,end);
	      for (j=0;j<count;j++)
	       {
		 MSTableColumn *col=columnList()->array(j);
		 if (col!=0&&col->model()!=0)
		 {
		   col->permute(indexVector);
		   if (col->weights().length()>0) col->weights().permute(indexVector);
		 }
	       }
	      for (j=0;j<hcount;j++)
	       {
		 MSTableColumn *col=hiddenColumnList()->array(j);
		 if (col!=0&&col->model()!=0)
		  {
		    col->permute(indexVector);
		    if (col->weights().length()>0) col->weights().permute(indexVector);
		  }
	       }
	      iv.permute(indexVector);
	      column=nextcol;
	    }
	 }
      }
   }
  return iv;
}

MSIndexVector MSReportTable::sortUp(const MSSymbolVector &sortVector_)
{
  MSIndexVector iv;
  if (sortVector_.length()>0)
   {
     unsigned i,j,count=columnList()->count(),hcount=hiddenColumnList()->count();
     MSTableColumn *column=0;
     for (i=0;column==0&&i<sortVector_.length();i++)
      {
	for (j=0;j<count;j++)
	 {
	   if (sortVector_(i)==columnList()->array(j)->tag())
	    {
	      column=columnList()->array(j);
	      if (column->model()!=0) break;
	      else column=0;
	    }
	 }
	if (column==0)
	 {
	   for (j=0;j<hcount;j++)
	    {
	      if (sortVector_(i)==hiddenColumnList()->array(j)->tag())
	       {
		 column=hiddenColumnList()->array(j);
		 if (column->model()!=0) break;
		 else column=0;
	       }
	    }
	 }
      }
     if (column!=0)
      {
	unsigned startIndex=i;
	MSIndexVector indexVector=column->gradeUp();
	iv=indexVector;
	for (i=0;i<count;i++)
	 {
	   MSTableColumn *col=columnList()->array(i);
	   if (col!=0&&col->model()!=0)
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	for (i=0;i<hcount;i++)
	 {
	   MSTableColumn *col=hiddenColumnList()->array(i);
	   if (col!=0&&col->model()!=0)
	   {
	     col->permute(indexVector);
	     if (col->weights().length()>0) col->weights().permute(indexVector);
	   }
	 }
	MSIndexVector start,end;
	start.append(0);
	end.append(indexVector.length()-1);
	for (i=startIndex;i<sortVector_.length();i++)
	 {
	   column->range(start,end);
	   MSTableColumn *nextcol=0;
	   for (j=0;j<count;j++)
	    {
	      if (sortVector_(i)==columnList()->array(j)->tag())
	       {
		 nextcol=columnList()->array(j);
		 if (nextcol->model()!=0) break;
		 else nextcol=0;
	       }
	    }
	   if (nextcol==0)
	    {
	      for (j=0;j<hcount;j++)
	       {
		 if (sortVector_(i)==hiddenColumnList()->array(j)->tag())
		  {
		    nextcol=hiddenColumnList()->array(j);
		    if (nextcol->model()!=0) break;
		    else nextcol=0;
		  }
	       }
	    }
	   if (nextcol!=0)
	    {
	      indexVector=nextcol->rangeGradeUp(start,end);
	      for (j=0;j<count;j++)
	       {
		 MSTableColumn *col=columnList()->array(j);
		 if (col!=0&&col->model()!=0)
		  {
		    col->permute(indexVector);
		    if (col->weights().length()>0) col->weights().permute(indexVector);
		  }
	       }
	      for (j=0;j<hcount;j++)
	       {
		 MSTableColumn *col=hiddenColumnList()->array(j);
		 if (col!=0&&col->model()!=0)
		 {
		   col->permute(indexVector);
		   if (col->weights().length()>0) col->weights().permute(indexVector);
		 }
	       }
	      iv.permute(indexVector);
	      column=nextcol;
	    }
	 }
      }
   }
  return iv;
}

MSIndexVector MSReportTable::sortDown(const MSSymbolVector &sortVector_)
{
  MSIndexVector iv;
  if (sortVector_.length()>0)
   {
     unsigned i,j,count=columnList()->count(),hcount=hiddenColumnList()->count();
     MSTableColumn *column=0;
     for (i=0;column==0&&i<sortVector_.length();i++)
      {
	for (j=0;j<count;j++)
	 {
	   if (sortVector_(i)==columnList()->array(j)->tag())
	    {
	      column=columnList()->array(j);
	      if (column->model()!=0) break;
	      else column=0;
	    }
	 }
	if (column==0)
	 {
	   for (j=0;j<hcount;j++)
	    {
	      if (sortVector_(i)==hiddenColumnList()->array(j)->tag())
	       {
		 column=hiddenColumnList()->array(j);
		 if (column->model()!=0) break;
		 else column=0;
	       }
	    }
	 }
      }
     if (column!=0)
      {
	unsigned startIndex=i;
	MSIndexVector indexVector=column->gradeDown();
	iv=indexVector;
	for (i=0;i<count;i++)
	 {
	   MSTableColumn *col=columnList()->array(i);
	   if (col!=0&&col->model()!=0)
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	for (i=0;i<hcount;i++)
	 {
	   MSTableColumn *col=hiddenColumnList()->array(i);
	   if (col!=0&&col->model())
	    {
	      col->permute(indexVector);
	      if (col->weights().length()>0) col->weights().permute(indexVector);
	    }
	 }
	MSIndexVector start,end;
	start.append(0);
	end.append(indexVector.length()-1);
	for (i=startIndex;i<sortVector_.length();i++)
	 {
	   column->range(start,end);
	   MSTableColumn *nextcol=0;
	   for (j=0;j<count;j++)
	    {
	      if (sortVector_(i)==columnList()->array(j)->tag())
	       {
		 nextcol=columnList()->array(j);
		 if (nextcol->model()!=0) break;
		 else nextcol=0;
	       }
	    }
	   if (nextcol==0)
	    {
	      for (j=0;j<hcount;j++)
	       {
		 if (sortVector_(i)==hiddenColumnList()->array(j)->tag())
		  {
		    nextcol=hiddenColumnList()->array(j);
		    if (nextcol->model()!=0) break;
		    else nextcol=0;
		  }
	       }
	    }
	   if (nextcol!=0)
	    {
	      indexVector=nextcol->rangeGradeDown(start,end);
	      for (j=0;j<count;j++)
	       {
		 MSTableColumn *col=columnList()->array(j);
		 if (col!=0&&col->model()!=0)
		  {
		    col->permute(indexVector);
		    if (col->weights().length()>0) col->weights().permute(indexVector);
		  }
	       }
	      for (j=0;j<hcount;j++)
	       {
		 MSTableColumn *col=hiddenColumnList()->array(j);
		 if (col!=0&&col->model()!=0)
		  {
		    col->permute(indexVector);
		    if (col->weights().length()>0) col->weights().permute(indexVector);
		  }
	       }
	      iv.permute(indexVector);
	      column=nextcol;
	    }
	 }
      }
   }
  return iv;
}

////////////////////////////////////////////////////////////////////////////
//
// printing related methods
//
////////////////////////////////////////////////////////////////////////////

MSReport* MSReportTable::report(void)
{
  if (_report==0)
   {
    _report=new MSReport;
    _report->pageOrientation(MSP::Landscape);
   }
  return _report;
}

void MSReportTable::breakText(MSParagraph *x_)
{
  if (_breakTextList==0) _breakTextList=new MSPointerArray<MSParagraph>;
  breakTextList()->add(x_);
}

int MSReportTable::fixedReportColumns(void) const
{return _fixedReportColumns;}

MSDisplayServer *MSReportTable::displayServer(void)
{return 0; }

unsigned long MSReportTable::style(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->style(row_):style();
}

unsigned long MSReportTable::breakStyle(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->breakStyle(row_):breakStyle();
}

int MSReportTable::breakOffset(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->breakOffset(row_):breakOffset();
}

int MSReportTable::breakLeading(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->breakLeading(row_):breakLeading();
}

double MSReportTable::fgGrayScale(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->fgGrayScale(row_):fgGrayScale();
}

double MSReportTable::bgGrayScale(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->bgGrayScale(row_):bgGrayScale();
}

const char *MSReportTable::reportFont(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->reportFont(row_):reportFont().string();
}

const char *MSReportTable::breakFont(unsigned row_,unsigned col_)
{ 
  MSTableColumn *field=reportColumn(col_);
  return field!=0?field->breakFont(row_):breakFont().string();
}

MSParagraph& MSReportTable::addGroupHeading(const MSParagraph& printText_)
{
  if (groupHeadingList()==0) _groupHeadingList=new MSPointerArray<MSParagraph>;
  MSParagraph *printText=new MSParagraph(printText_);
  if (printText->fontName().length()==0) printText->font(reportFont());
  groupHeadingList()->add(printText);
  return *printText;
}
MSParagraph& MSReportTable::addGroupHeading(const MSStringVector& printText_)
{return addGroupHeading(MSParagraph(printText_));}

void MSReportTable::removeGroupHeading(const MSSymbol& tag_)
{
  MSParagraph *printText=0;
  for (unsigned i=0;i<groupHeadingList()->count();i++)
   {
     if (tag_==groupHeading(i)->tag()) printText=groupHeading(i);
   }
  if (printText!=0)
   {
     delete printText;
     groupHeadingList()->remove(printText);
   }
}
void MSReportTable::removeAllGroupHeadings(void)
{
  for (unsigned i=0;i<groupHeadingList()->count();i++) delete groupHeading(i);
  groupHeadingList()->removeAll();
}

MSParagraph& MSReportTable::reportGroupHeading(const MSSymbol& tag_)
{
  for (unsigned i=0;i<groupHeadingList()->count();i++) if (tag_==groupHeading(i)->tag()) return *groupHeading(i);
  MSMessageLog::warningMessage("Warning: group heading \"%s\" not found\n",tag_.symbolName());
  return defaultText();
}

const MSParagraph& MSReportTable::reportGroupHeading(const MSSymbol& tag_) const
{
  for (unsigned i=0;i<groupHeadingList()->count();i++) if (tag_==groupHeading(i)->tag()) return *groupHeading(i);
  MSMessageLog::warningMessage("Warning: group heading \"%s\" not found\n",tag_.symbolName());
  return defaultText();
}

unsigned long MSReportTable::convertForeground(const char *)
{return 0;}

void MSReportTable::generateReport(const char *file_)
{
  report()->addPrintItem(this);
  report()->print(file_);
}

void MSReportTable::breakProcess(MSIndexVector& i_)
{
  for (unsigned int i=0;i<columnList()->count();i++)
   {
     MSTableColumn *tc=reportColumn(i);
     if (tc->breakProcessOn()==MSTrue) tc->breakProcess(i_);
   }
}

void MSReportTable::computeColumnTotals(void)
{
  MSIndexVector iv("0 0");
  MSString buffer;
  for (unsigned int i=0;i<columnList()->count();i++)
   {
     MSTableColumn *tc=reportColumn(i);
     iv[1]=tc->numRows();
     if (tc->breakProcessOn()==MSTrue) tc->breakProcess(iv);
     if (tc->breakString().length()>0)
      {
        buffer=tc->breakString().lastElement();
        grandTotal()+=buffer.asDouble();
      }
   }
}

void MSReportTable::clearBreaks(void)
{ 
  unsigned int n=columnList()->count();
  for (unsigned int i=0;i<columnList()->count();i++)
   {
     if (reportColumn(i)->breakOn()!=MSTrue) reportColumn(i)->breakString().removeAll();
     reportColumn(i)->breakIndex().removeAll();
     reportColumn(i)->breakInvalid().removeAll();
     reportColumn(i)->breakIndex()<<0;
   }
}

MSReportTable& MSReportTable::leftMargin(double x_)
{MSPrintItem::leftMargin(x_); return *this;}
MSReportTable& MSReportTable::rightMargin(double x_)
{MSPrintItem::rightMargin(x_); return *this;}
MSReportTable& MSReportTable::topOffset(double x_)
{MSPrintItem::topOffset(x_); return *this;}
MSReportTable& MSReportTable::bottomOffset(double x_)
{MSPrintItem::bottomOffset(x_); return *this;}
MSReportTable& MSReportTable::topPixel(unsigned x_)
{MSPrintItem::topPixel(x_); return *this;}
MSReportTable& MSReportTable::bottomPixel(unsigned x_)
{MSPrintItem::bottomPixel(x_); return *this;}
MSReportTable& MSReportTable::justification(MSAlignment x_)
{MSPrintItem::justification(x_); return *this;}
MSReportTable& MSReportTable::justification(unsigned long x_)
{MSPrintItem::justification(x_); return *this;}
MSReportTable& MSReportTable::printRow(int x_)
{MSPrintItem::printRow(x_); return *this;}
MSReportTable& MSReportTable::printColumn(int x_)
{MSPrintItem::printColumn(x_); return *this;}
MSReportTable& MSReportTable::reportFont(const char *string_)
{MSPrintItem::printFont(string_); return *this;}
MSReportTable& MSReportTable::style(unsigned long style_)
{
  unsigned long temp=style_&(MSLeft|MSRight|MSTop|MSBottom|MSCenter);
  if (temp>0) justification(temp);
  _style=style_;
  return *this;
}
MSReportTable& MSReportTable::orphanRows(unsigned x_)
{_orphanRows=x_; return *this;}
MSReportTable& MSReportTable::widowRows(unsigned x_)
{_widowRows=x_; return *this;}
MSReportTable& MSReportTable::pageAlignment(unsigned long x_)
{MSPrintItem::pageAlignment(x_); return *this;}
MSReportTable& MSReportTable::pageAlignment(MSAlignment x_)
{return pageAlignment((unsigned long)x_);}
const MSSymbol& MSReportTable::printTag(void) const
{return tag();}
double MSReportTable::leftMargin(void) const
{return MSPrintItem::leftMargin();}
double MSReportTable::rightMargin(void) const
{return MSPrintItem::rightMargin();}
double MSReportTable::topOffset(void) const
{return MSPrintItem::topOffset();}
double MSReportTable::bottomOffset(void) const
{return MSPrintItem::bottomOffset();}
unsigned MSReportTable::topPixel(void) const
{return MSPrintItem::topPixel();}
unsigned MSReportTable::bottomPixel(void) const
{return MSPrintItem::bottomPixel();}
unsigned long MSReportTable::justification(void) const
{return MSPrintItem::justification();}
int MSReportTable::printRow(void) const
{return MSPrintItem::printRow();}
int MSReportTable::printColumn(void) const
{return MSPrintItem::printColumn();}
const MSString& MSReportTable::reportFont(void) const
{return MSPrintItem::printFont();}

int MSReportTable::print(MSReport *report_,int x_,int y_,int,int,int,int)
{
  if (numColumns()==0) { _currentPage++; return 0;}
  MSReport *oldReport= _report;
  report(report_);
  if(report_->outputMode()==ASCII) 
   {
     printASCIITable();
     _report=oldReport;
     return 0;
   }
  int actualHeight=0;
  int y=y_-(segment()==0?topPixel():0);
  int remainingHeight=y_-report_->pageEnd();
  int minHeight=orphanRowHeight()+tableHeaderHeight()+tableFooterHeight();
  if (remainingHeight<=0 || (minHeight>=remainingHeight&&pageBreakRow()<0)||
      (pageBreakRow()<0&&pageAlignment()&MSTop&&y_!=report_->bodyTop(report_->pageCount())))
   {
    _currentPage++;
     pageBreakRow(0);
//Height(minHeight);
     _report=oldReport;
     return remainingHeight;
   }
  else report_->yPixel(y);
  if (pageHeight()>0&&pageHeight()<remainingHeight)
   {
     if (pageAlignment()&MSCenter)
      {
        y-=(remainingHeight-pageHeight())/2;
        actualHeight=remainingHeight;
      }
     else if (pageAlignment()&MSBottom)
      {
        y=report_->pageEnd()+pageHeight();
        actualHeight=remainingHeight;
      }
   }
  int h=y_-y;
  h+=printTableSegment(x_,y);
  if (stackCount()+1<stackPageBreak().length()&&segment()==stackPageBreak()(stackCount()+1))
   {
     _stackCount++;
     y-=h+bottomPixel();
     int newRemainingHeight=y-report_->pageEnd();
     minHeight=orphanRowHeight()+tableHeaderHeight()+tableFooterHeight();
     if (minHeight>=newRemainingHeight)
      {
        _segment++;
//        _pageCount++;
	_currentPage++;
	_printPage++;
        _report=oldReport;
        return pageHeight()-remainingHeight;
      }
     else report_->yPixel(y);
     h+=printTableSegment(x_,y);
     h+=topPixel()+bottomPixel();
   }
  int totalSegments=rowPageBreak().length()-stackPageBreak().length()-1;
  h+=segment()<totalSegments?remainingHeight:bottomPixel();
  if (segment()<totalSegments)
   {
     pageBreakRow(1);
   }
  else 
   {
     pageBreakRow(-1);
   }
//  pageCountIncrement();
  _currentPage++;
  _printPage++;
  _report=oldReport;
  return actualHeight>0?actualHeight:h;
}

int MSReportTable::computePrintSize(MSReport *report_,int,int y_,int w_,int,int topMargins_,int margins_)
{
  int h=0;
  _printPage=1;
  MSReport *oldReport=_report;
  report(report_);
  if (numColumns()>0)
   {
     reset();
     segment(0);
     stackCount(-1);
     if (leftPixel()<0) leftPixel(report_->leftPixel());
     if (rightPixel()<0) rightPixel(report_->rightPixel());
     margins_=(margins_==0?leftPixel()+rightPixel():margins_);
     pageWidth(w_-margins_);
     report_->yPixel(y_-topPixel()-topMargins_);
     printHeight(topMargins_); //hack for now.
     computeTableRowColumnSize();
     
     int startingPageCount=report()->pageCount();

     int remainingHeight=report()->yPixel()-report()->pageEnd();
     int minHeight=orphanRowHeight()+tableHeaderHeight()+tableFooterHeight();
     headerHeights().removeAll();
     footerHeights().removeAll();
     if (remainingHeight<=0||minHeight>=remainingHeight||
	 (pageAlignment()&MSTop&&y_!=report_->bodyTop(report_->pageCount())))
      {
       _residual=1;  //hack
        _pageCount++;
        report()->pageCountIncrement();
        report()->yPixel(report()->bodyTop()-topMargins_);
        remainingHeight=report()->yPixel()-report()->pageEnd();
      }
     computeRowColumnPartitions();
     if (pageCountTotal()>1)
      {
        unsigned index=rowPageBreak().length()-2;
        h=report()->yPixel()-report()->pageEnd();
        pageCount(pageCountTotal()-1+_residual);
	_printPage=pageCountTotal();
        int lastSegment=computeTableSegmentHeight(rowPageBreak()(index),rowPageBreak()(index+1));
	int page=startingPageCount+pageCountTotal();
        int pageSize=report()->bodyTop(page)-report()->bodyBottom(page)-topMargins_;
//        if (lastSegment<pageSize) lastSegment+=tableTop()+tableBottom();
//        else lastSegment=pageSize;
        if (lastSegment+bottomPixel()<pageSize)
	 {
	   lastSegment+=bottomPixel();
	   h=lastSegment;
	 }
	else 
	 {
	   _pageCount++;
	   h=0;
	 }
      }
     else
      {
        pageCount(_residual);
	_printPage=1;
        int columns=newspaperColumns();
        int rows=rowCount();
        int maxRows=rows/columns+(rows%columns>0?1:0);
	double headerHeight=tableHeaderHeight()+tableFooterHeight();
        double height=headingHeight()+tableTop()+tableBottom();
        int maxHeight=0;
        int lastRowLeading=0;
        for (int rh,i=0,j,k=0;i<columns;i++)
         {
           for (rh=0,j=0;j<maxRows&&k<rows;j++,k++) rh+=rowHeights()(k)+rowLeadings()(k);
           lastRowLeading=k>0?rowLeadings()(k-1):0;
           maxHeight=rh>maxHeight?rh:maxHeight;
         }
        height+=maxHeight;
        height+=height*stackPageBreak().length();
        height*=y_printScale();
        height+=topPixel()+(topPixel()+bottomPixel())*(stackPageBreak().length());
	height+=headerHeight*((scaleHeaders()==MSTrue)?y_printScale():1.0);

        if (height>remainingHeight) height-=lastRowLeading;
        if (height+bottomPixel()<remainingHeight) height+=bottomPixel();
        pageHeight((int)(height));
        if (pageAlignment()&MSCenter||pageAlignment()&MSBottom)
	 {
	  _pageCount++;
	  h=0;
	 }
        else h=(int)height;
      }
  // reset table and report's pageCounts
//     pageCount(0);
     report()->pageCount(startingPageCount);
   }
  printHeight(0);
  _residual=h;
  _report=oldReport;
  _printPage=1;
  return h;
}


void MSReportTable::computeTableRowColumnSize(void)
{
  MSTableColumn     *field;
  MSPrintFontData   *fdata;
  const char        *cp;
  int                offset=1;

  setTableParameters();
  computeBreaks();

  // initialize report variables
  int grandTotalOffset=reportGrandTotalOn()==MSTrue?1:0;
  rowCount(dataRows()+breakIndex().length()+breakTextColumn().length()+grandTotalOffset);
  columnCount(numColumns());
  breakCount(0);
  breakTextCount(0);
  columnPageBreak().removeAll();
  rowPageBreak().removeAll();
  //  breakLeading().removeAll();
  pageBreakIndex().removeAll();
  breakTextIndex().removeAll();
  
  // initialize table variables
  x_printScale(1);
  y_printScale(1);
  rowHeights().removeAll();
  rowHeights().reshape(rowCount());
  rowLeadings().removeAll();
  rowLeadings().reshape(rowCount());
  columnWidths().removeAll();
  columnWidths().reshape(columnCount());
  
  int fixedFields=fixedReportColumns()>0?fixedReportColumns():0;
  numFixedFields(fixedFields<numColumns()?fixedFields:0);

  // compute table heading sizes
  computeTableHeadingSize();

  MSIndexVector breakTextColumnCount(numColumns(),0);
  unsigned rows=rowCount()-grandTotalOffset;
  unsigned breakIndexLength=breakIndex().length();
  unsigned pageBreakIndexLength=pageBreakIndex().length();
  unsigned breakTextColumnLength=breakTextColumn().length();
  MSString buffer;
  for (unsigned j=0,t=0;j<numColumns();j++)
   {
     if ((field=reportColumn(j))!=0)
      {
	// p --> pageBreak index
	// b --> column break text index
	// t --> break text index
	// d --> data index
	// k --> break index
	// i --> rowHeights index
	
	for (unsigned b,p=0,d=0,k=0,i=0;i<rows;i++,d++)
	 {
	   if (d<field->numRows())
	    {
	      cp=formatOutput(buffer.removeAll(),d,j);
	      fdata=report()->fontStruct(report()->printFontID(reportFont(d,j)));
	      rowLeadings()[i]=leading(i);
	      int h=rowHeights()(i);
	      rowHeights()[i]=report()->fontSize()>h?report()->fontSize():h;
	      if (cp!=0)
	       {
		 int cw=columnWidths()(j);
		 int w=(int)fdata->textWidth(report()->fontSize(),cp,strlen(cp))+offset;
		 columnWidths()[j]=w>cw?w:cw;
	       }
	    }
	   while (k<breakIndexLength&&d==breakIndex()(k)-1)
	    {
	      MSTableColumn *breakTextField=0;
	      if (breakTextColumnLength>0&&t<breakTextColumnLength)
	       {
		 b=breakTextColumnCount[breakTextColumn()(t)];
		 breakTextColumnCount[breakTextColumn()(t)]++;
		 breakTextField=reportColumn(breakTextColumn()(t++));
	       }
	      if (breakTextField!=0&&b<breakTextField->breakTextList().count()&&
		  breakTextField->breakText(b)->style()&PosAbove)
	       {
		 breakTextIndex()<<i+1;
		 breakText(breakTextField->breakText(b));
		 rowHeights()[i]=rowLeadings()[++i]=0;
	       }
              else if (breakTextIndex().indexOf(i+1)<breakTextIndex().length()) i++;
	      int h=0,leading=0;
              if ((cp=field->formatBreak(buffer.removeAll(),k,breakColumn()(k)))!=0)
               {
                 if (reportTotalOn()==MSTrue&&k==breakColumn().length()-1)
                  {
                    fdata=report()->fontStruct(report()->printFontID(reportTotalFont()));
                    leading=reportTotalLeading();
                  }
                 else
                  {
                    fdata=report()->fontStruct(report()->printFontID(breakFont(k,breakColumn()(k))));
                    leading=breakLeading(k,breakColumn()(k));
                  }
                 int cw=columnWidths()(j);
                 int w=(int)fdata->textWidth(report()->fontSize(),cp,strlen(cp))+offset;
                 columnWidths()[j]=w>cw?w:cw;
                 h=report()->fontSize()+breakOffset(k,breakColumn()(k));
               }
              rowLeadings()[++i]=leading;
	      rowHeights()[i]=(h>rowHeights()(i)?h:rowHeights()(i));
	      if (breakTextField!=0&&b<breakTextField->breakTextList().count()&&
		  !(breakTextField->breakText(b)->style()&PosAbove))
	       {
		 breakTextIndex()<<i+1;
		 breakText(breakTextField->breakText(b));
		 rowHeights()[i]=rowLeadings()[++i]=0;
	       }
              else if (breakTextIndex().indexOf(i+1)<breakTextIndex().length()) i++;
	      k++;
	    } 
	   if (p<pageBreakIndexLength&&d==pageBreakIndex()(p)-1)
	    {
	      pageBreakIndex()<<i+1; p++;
	    }
	 }
      }
   }
  int orphanHeight=0;
  for (unsigned i=0;i<orphanRows()&&i<rowCount();i++)
   {
     orphanHeight+=rowHeights()(i);
     if (i+1<orphanRows()) orphanHeight+=rowLeadings()(i);
   }
  orphanHeight+=headingHeight()+topPixel()+tableTop()+tableBottom();
  orphanRowHeight(orphanHeight);
  
  if (reportGrandTotalOn()==MSTrue)
   {
     computeBreakTextSize(grandTotalBaseText());
     rowLeadings()[rows]=grandTotalBaseText().topPixel();
     rowHeights()[rows]=grandTotalBaseText().printHeight();
   }
}

void MSReportTable::setTableParameters(void)
{
  int cols=newspaperColumns()>0?newspaperColumns():1;
  int frameThickness=frameLineWidth()+frameOffset();
  unsigned long style=frameStyle();
  int frameLeft=style&Box||style&BoxL?frameThickness:0;
  int frameRight=style&Box||style&BoxR?frameThickness:0;
  int frameTop=style&Box||style&BoxT?frameThickness:0;
  int frameBottom=style&Box||style&BoxB?frameThickness:0;

  tableLeft(report()->bodyLeft()+frameLeft);
  int w=pageWidth()-(frameLeft+frameRight)*cols;
  tableRight(tableLeft()+w/cols);
  tableTop(frameTop);
  tableBottom(frameBottom*2);
}

void MSReportTable::computeTableHeadingSize(void)
{
  computeGroupHeadingSize();
  int height=0;
  for (unsigned j=0;j<columnCount();j++) 
   {
     int w,h=0,tw=0;
     MSTableColumn *field=reportColumn(j);
     if (field!=0)
      {
	MSPrintFontData *fdata=report()->fontStruct(report()->printFontID(field->reportHeadingFont()));
	for (unsigned i=0;i<field->heading().length();i++)
	 {
	   w=(int)fdata->textWidth(report()->fontSize(),field->heading()(i),field->heading()(i).length());
	   tw=tw>w?tw:w;
	   h+=report()->fontSize()+leading(0);
	 }
	columnWidths()[j]=tw;
	height=h>height?h:height;
      }
   }
  height+=groupHeadingHeight();
  headingHeight(height+(height>0?reportHeadingOffset():0));
}

void MSReportTable::computeRowColumnPartitions() 
{
  computeColumnPartitions();
  int columnPageCount=columnPageBreak().length()-1;
  if (columnPageCount>0)
   {
     int estimatedPageCount=rowPageSpan();
     int startingPageCount=report()->pageCount();
     int startingYpixel=report()->yPixel();
     double columnHeight;
     double heights=rowHeights().sum()+rowLeadings().sum();
     // use base pagesize to compute estimated table page count so as not
     // to trigger computation of headers and footers until an estimated
     // page count is established.
     int estimatedPageSize=(int)((report()->bodyTopBase()-report()->bodyBottomBase())/
       (report()->uniformScaling()==MSTrue?x_printScale():1));
     int tableHeader=tableHeaderHeight();
     int tableFooter=tableFooterHeight();
     estimatedPageSize-=headingHeight()+tableTop()+tableBottom();
     estimatedPageSize-=(int)((tableHeader+tableFooter)/((scaleHeaders()==MSFalse&&
						    report()->uniformScaling()==MSTrue)?x_printScale():1));
     columnHeight=heights/newspaperColumns();
     // estimate total number of pages for this table to compute the 
     // conditional header/footer sizes for LastPage and IfNextPage
     if (rowPageSpan()==0)
      {
        int actualHeight=(int)columnHeight;
        if (outputStyle()&Stacked)
         {
           actualHeight*=columnPageCount;
           actualHeight+=(topPixel()+bottomPixel())*columnPageCount;
         }
        int remainingHeight=report()->yPixel()-report()->pageEnd();
        remainingHeight-=tableHeader+headingHeight()+tableFooter;
        if (actualHeight>remainingHeight)
         {
           double remainingTableHeight=actualHeight-remainingHeight;
           estimatedPageCount=(int)ceil(remainingTableHeight/estimatedPageSize)+1;
         }
        else estimatedPageCount=1;
      }
     int newPageTotal=startingPageCount+estimatedPageCount-1;
     if (newPageTotal>report()->pageCountTotal()) report()->pageCountTotal(newPageTotal);
     pageCountTotal(estimatedPageCount);
     computeRowPartitions(startingYpixel,columnHeight);
     if (report()->uniformScaling()==MSTrue&&y_printScale()<x_printScale())
      {
        // recompute column partitions at the new scale
        computeColumnPartitions();
        columnPageCount=columnPageBreak().length()-1;
      }
     double total=rowPageBreak().length()-2*stackPageBreak().length()-1;
     if (!(outputStyle()&Stacked)) total*=columnPageCount;
     int tablePageTotal=(int)ceil(total/newspaperColumns());
     totalSegments((int)(total));
     pageCountTotal(tablePageTotal);
     newPageTotal=startingPageCount+tablePageTotal-1;
     if (newPageTotal>report()->pageCountTotal()) report()->pageCountTotal(newPageTotal);
     if (report()->conditionalPageSize()==MSTrue&&tablePageTotal>0&&tablePageTotal!=estimatedPageCount)
      {
        // recompute row partitions using actual page count
        headerHeights().removeAll();
        footerHeights().removeAll();
        report()->pageCount(startingPageCount);
        pageCount(0);
	_printPage=1;
        computeRowPartitions(startingYpixel,columnHeight);
        total=rowPageBreak().length()-2*stackPageBreak().length()-1;
        tablePageTotal=(int)ceil(total/newspaperColumns());
        pageCountTotal(tablePageTotal);
      }
     newPageTotal=startingPageCount+tablePageTotal-1;
     if (newPageTotal>report()->pageCountTotal()) report()->pageCountTotal(newPageTotal);
     if (report()->uniformScaling()==MSTrue)
      {
        if (x_printScale()<y_printScale()) y_printScale(x_printScale());
        else x_printScale(y_printScale());
      }
     report()->yPixel(startingYpixel);
   }
  else pageCountTotal(0);
}

void MSReportTable::computeColumnPartitions(void) 
{
  int xx,xstart=tableLeft();
  unsigned j,actual;
  columnIndex().removeAll();
  columnPageBreak().removeAll();
  tableWidths().removeAll();
  
  if (numFixedFields()>0)
   {
     xx=xstart;
     for (int k=0;k<numFixedFields();k++)
      {
	xx=setTableX(xx,k);
	columnIndex()<<xx;
      }
     fixedFieldWidth(xx-xstart);
   }
  else fixedFieldWidth(0);
  xx=xstart+fixedFieldWidth();
  int set=columnsPerPage(0);
  int partitionWidth=(int)(report()->uniformScaling()==MSTrue?tableRight()/y_printScale():tableRight());
  if (fixedFieldWidth()<partitionWidth)
   {
     columnPageBreak()<<numFixedFields();
     // find column partitions
     for (actual=j=numFixedFields();j<numColumns();j++,actual++)
      {
	if (report()->cancelReportStatus()==MSTrue) return;
	if (xx+columnWidths()(j)>=partitionWidth||(set>1&&actual==set)||set==1) 
	 {
	   if (set>0)
	    {
	      if (set>actual)
	       {
                 set=set<numColumns()?set:numColumns();
                 for (;actual<set;j++,actual++)
                  {
                    xx=setTableX(xx,j);
                    columnIndex()<<xx;
                  }
                 computeXFontScale(xx);
                 if (j<numColumns()) columnPageBreak()<<(j==1?j:j--);
//		 columnPageBreak()<<(j==1?j:j--);
	       }
	      else columnPageBreak()<<(j==1?j:j--);
	    }
	   else columnPageBreak()<<(j==1?j:j--);
           tableWidths()<<(xx-xstart);
	   xx=xstart+fixedFieldWidth();
           _pageCount++;
//	   _printPage++;
	   actual=numFixedFields()-1,set=columnsPerPage((columnPageBreak().length()-1));
           if (j<=numFixedFields()) 
            {
              MSMessageLog::errorMessage("Error: Column width exceeds page width--table not printed!\n");
              columnPageBreak().removeAll();
              rowPageBreak().removeAll();
              return;
            }
	 }
	else
	 {
	   xx=setTableX(xx,j);
	   columnIndex()<<xx;
	 }
      }
     // for the case when columnPageSpan is set to less than
     // the number of columns that would fit on a page
     if (columnIndex().length()<numColumns())
      {
        xx=xstart+fixedFieldWidth();
        for (unsigned i=columnIndex().length();i<numColumns();i++)
         {
	   xx=setTableX(xx,i);
           columnIndex()<<xx;
         }
      }
     tableWidths()<<(xx-xstart);
   }
  else MSMessageLog::errorMessage("Error: Fixed Field Width exceeds page width--print aborted!\n");
  columnPageBreak()<<numColumns();
}

void MSReportTable::computeRowPartitions(int startPixel_,double columnHeight_) 
{
  rowPageBreak().removeAll();
  stackPageBreak().removeAll();
  report()->yPixel(startPixel_);
  computeRowPartitions(columnHeight_);
  int columnPageCount=columnPageBreak().length()-1;
  if (outputStyle()&Stacked)
   {
     for (unsigned i=0;i<columnPageCount-1;i++)
      {
        stackPageBreak()<<rowPageBreak().length()-1;
        int remainingHeight=report()->yPixel()-report()->pageEnd();
        int minHeight=orphanRowHeight()+tableHeaderHeight()+tableFooterHeight();
        if (minHeight>=remainingHeight)
         {
           rowPageBreak()<<0;
           _pageCount++;
	   _printPage++;
           report()->yPixel(report()->bodyTop(report()->pageCount())-printHeight());
         }
        computeRowPartitions(columnHeight_);
      }
   }
}

void MSReportTable::computeRowPartitions(double columnHeight_) 
{
  unsigned i,k;
  MSIntVector columnHeights(newspaperColumns(),0);
  int pageBreakIndexLength=pageBreakIndex().length();
  int set=rowsPerPage(0);
  rowPageBreak()<<0;
  int actual,height=0,column=0;
  int tableHeaders=tableHeaderHeight()+tableFooterHeight();
  int headers=headingHeight()+tableTop()+tableBottom();
  double scale=(report()->uniformScaling()==MSTrue?x_printScale():1);
  int pageSize=(int)(tablePageSize(0)/scale-headers);
  pageSize-=(int)(tableHeaders/((scaleHeaders()==MSFalse)?scale:1));
  // compute the breakText sizes with using the new x-scale computed with the
  // above column partitions.  If the row partitions produce a smaller scale and uniform
  // scaling is enabled the text will not completely fill the cell.
  computeBreakTextSizes();
  MSBoolean evenColumns=numRows()%newspaperColumns()==0?MSTrue:MSFalse;
  for (actual=0,k=0,i=0;i<rowCount();i++,actual++)
   {
     if (report()->cancelReportStatus()==MSTrue) return;
     int rowHeight=rowHeights()(i)+rowLeadings()(i);
     columnHeights[column]+=rowHeight;
     height+=rowHeight;
     if (set==0&&k<pageBreakIndexLength&&i==pageBreakIndex()(k))
      {
        column++;
        k++;
        height=0;
        rowPageBreak()<<i--;
        column=column%newspaperColumns();
        if (column==0)
         {
           _pageCount++;
	   _printPage++;
           report()->pageCountIncrement();
           pageSize=(int)(tablePageSize(rowPageBreak().length()-1)/scale-headingHeight());
//             (report()->uniformScaling()==MSTrue?x_printScale():1);
           pageSize-=(int)((tableHeaderHeight()+tableFooterHeight())/((scaleHeaders()==MSFalse)?scale:1));
         }
      }
     int heightOffset=evenColumns==MSTrue?0:rowHeights()(i);
     if (height-rowLeadings()(i)>pageSize||(set>0&&actual==set)||
         (column<newspaperColumns()-1&&columnHeights(column)>columnHeight_+heightOffset))
      {
        if (set>0)
         {
           if (set>actual)
            {
              set=set<rowCount()?set:rowCount();
              for (i++,actual++;actual<set;i++,actual++) height+=rowHeights()(i)+rowLeadings()(i);
              if (i<rowCount()) rowPageBreak()<<i--;
              computeYFontScale(height);
             }
           else rowPageBreak()<<i--;
         }
        else rowPageBreak()<<i--;
        columnHeights[column]-=rowHeight;
        actual=-1,set=rowsPerPage((rowPageBreak().length()-1));
        height=0;
        column++;
        column=column%newspaperColumns();
        if (column==0)
         {
           _pageCount++;
	   _printPage++;
           report()->pageCountIncrement();
           pageSize=(int)(tablePageSize(rowPageBreak().length()-1)/scale-headingHeight());
           pageSize-=(int)((tableHeaderHeight()+tableFooterHeight())/((scaleHeaders()==MSFalse)?scale:1));
         }
      }
   }
  report()->yPixel(report()->yPixel()-height+headers);
  rowPageBreak()<<rowCount();
}

void MSReportTable::computeXFontScale(int xPixel_)
{
  double w=tableRight()-tableLeft();
  double scale=w/(xPixel_-tableLeft());
  if (scale<x_printScale()) x_printScale(scale);
}

void MSReportTable::computeYFontScale(int y_)
{
  double h=tablePageSize(rowPageBreak().length()-1);
  double y=y_+headingHeight();
  int  headers=tableHeaderHeight()+tableFooterHeight();
  if(scaleHeaders()==MSTrue) y+=headers;
  else h-=headers;
  double scale=h/y;
  if (scale<y_printScale()) y_printScale(scale);
}

int MSReportTable::tablePageSize(int column_) 
{
  int pc=report()->pageCount();
  int ystart=column_>=newspaperColumns()||report()->yPixel()<=tableBottomPosition(pc)?
    tableTopPosition(pc):report()->yPixel();
  return ystart-tableBottomPosition(pc);
}

int MSReportTable::setTableX(int x_,int i_)
{
  int cw=columnWidths()(i_);
  int tabStop=report()->tabStop(i_+1);
  int x=x_+cw+reportColumnSpacing(i_);
  
  if (tabStop!=0&&tabStop>x_&&(tabStop+cw)<tableRight())
   {
     if (report()->tabStop(i_+2)!=0&&(tabStop+cw)>report()->tabStop(i_+2)) tabStop=x;
   }
  else tabStop=x;
  return tabStop;
}

int MSReportTable::setTableY(int y_,int i_)
{
  return y_-=rowHeights()(i_)+rowLeadings()(i_);
}

int MSReportTable::columnsPerPage(int page_)
{
  int span,cols=0;
  if ((span=columnPageSpan())>0)
   {
     cols=(numColumns()-numFixedFields())/span+numFixedFields();
     if (page_==span-1) cols+=(numColumns()-numFixedFields())%span;
   }
  else if (columnControl().length()>0)
   {
     if (page_<columnControl().length()) cols=columnControl()(page_);
     else cols=columnControl().lastElement();
   }
  return cols;
}

int MSReportTable::rowsPerPage(int page_)
{
  int span,rows=0;
  if ((span=rowPageSpan())>0)
   {
     rows=rowCount()/span;
     if (page_==span-1) rows+=rowCount()%span;
   }
  else if (rowControl().length()>0)
   {
     if (page_<rowControl().length()) rows=rowControl()(page_);
     else rows=rowControl().lastElement();
   }
  else if (newspaperColumns()>0)
   {
   }
  return rows;
}

int MSReportTable::leading(int row_)
{
  int spacing=2;
  
  if (leading().length()>0)
   {
     spacing=leading()(row_%leading().length());
   }
  return spacing;
}

int MSReportTable::reportColumnSpacing(int column_)
{
  double spacing=0.25;//defaultColumnSpacing();
  
  if (reportColumnSpacing().length()>0)
   {
     spacing=reportColumnSpacing()(column_%reportColumnSpacing().length());
   }
  return (int)(spacing*MSPointsPerInch);
}

int MSReportTable::computeTableSegmentHeight(int rs_,int re_)
{
  int header=tableHeaderHeight()+tableFooterHeight();
  double scale=scaleHeaders()==MSTrue?y_printScale():1.0;
  int h=headingHeight()+tableTop()+tableBottom();
  for (unsigned i=rs_;i<re_;i++)
   {
     h+=rowHeights()(i)+rowLeadings()(i);
   }
  return (int)(h*y_printScale()+header*scale);
}

int MSReportTable::tableHeaderHeight(void)
{
  while(headerHeights().length()<_printPage)
   {
     computeTableHeaderSize(headerHeights().length()+1);
   }
  return headerHeights()(_printPage-1);
}

int MSReportTable::tableFooterHeight(void)
{
  while(footerHeights().length()<_printPage)
   {
     computeTableFooterSize(footerHeights().length()+1);
   }
  return footerHeights()(_printPage-1);
}

void MSReportTable::computeTableHeaderSize(int pageCount_) 
{
  double height=0;
  int top=report()->bodyTop(report()->pageCount())-printHeight();
  int bottom=report()->bodyBottom(report()->pageCount());
  double scale=(scaleHeaders()==MSTrue)?x_printScale():1.0;
  for (unsigned i=0,n=headerList().count();i<n;i++)
   {
     if (report()->printOnPage(*header(i),pageCount_,pageCountTotal())==MSTrue)
      {
        if (header(i)->printFont().length()==0) header(i)->printFont(report()->defaultFont());
        double h=header(i)->computePrintSize(report(),0,top,(int)(pageWidth()/scale),0,0,4);
        if (top-height-h<bottom)
         {
           removeHeader(header(i));
           MSMessageLog::errorMessage("Error: MSReportTable Header height exceeds page height\n");
           headerHeights().removeAll();
           return;
         }
        else height+=h;
      }
   }
  headerHeights()<<int(height);
}

void MSReportTable::computeTableFooterSize(int pageCount_) 
{
  double height=0;
  int top=report()->bodyTop(report()->pageCount())-printHeight();
  int bottom=report()->bodyBottom(report()->pageCount());
  double scale=(scaleHeaders()==MSTrue)?y_printScale():1.0;
  for (unsigned i=0,n=footerList().count();i<n;i++)
   {
     if (report()->printOnPage(*footer(i),pageCount_,pageCountTotal())==MSTrue)
      {
        // just in case this method is called before the headers method
        int hh=pageCount_<headerHeights().length()?headerHeights()(pageCount_-1):0;
        if (footer(i)->printFont().length()==0) footer(i)->printFont(report()->defaultFont());
        double h=footer(i)->computePrintSize(report(),0,top,(int)(pageWidth()/scale),0,0,4);
        if (top-hh-height-h<bottom)
         {
           removeFooter(footer(i));
           MSMessageLog::errorMessage("Error: MSReport Header and Footer heights exceed page height\n");
           footerHeights().removeAll();
           return;
         }
        else height+=h;
      }
   }
  footerHeights()<<int(height);
}

void MSReportTable::computeBreakTextSize(MSParagraph& breakText_)
{
  int x=(report()->tabStop(0)>tableLeft()?report()->tabStop(0):tableLeft());
  for (unsigned i=0;i<breakText_.column();i++) x=setTableX(x,i);
  int frameThickness=frameLineWidth()+frameOffset();
  int pageFrameThickness=report()->pageFrameLineWidth()+report()->pageFrameOffset();
  int offset=frameThickness+pageFrameThickness;
  int w=(int)(pageWidth()/x_printScale()/newspaperColumns());
  w-=2*offset;
  breakText_.computePrintSize(report(),0,0,w-x+tableLeft(),0,0,offset);
}

void MSReportTable::computeBreakTextSizes(void)
{
  if (breakTextList()!=0)
   {
     for (unsigned i=0;i<breakTextList()->count();i++)
      {
        computeBreakTextSize(*breakText(i));
        rowHeights()[breakTextIndex()(i)]=breakText(i)->height();
      }
   }
}

void MSReportTable::computePageIndices(int i_,int& ri_,int& ci_)
{
  if (stackPageBreak().length()>0)
   {
     ci_=0;
     if (stackCount()<stackPageBreak().length())
      {
        int rowoffset=0;
        int coloffset=0;
        if (i_>=stackPageBreak()(stackCount()))
         {
           rowoffset=stackCount()+1;
           coloffset=stackCount()+1;
         }
        ri_=i_+rowoffset;
        ci_=coloffset;
      }
     else ri_=i_;
   }
  else
   {
     if (outputStyle()==RowMajor)
      {
        int pageCountRow=rowPageBreak().length()-1;
        ri_=i_>=pageCountRow?i_%pageCountRow:i_;
        ci_=(int)i_/pageCountRow;
      }
     else
      {
        int pageCountCol=columnPageBreak().length()-1;
        ci_=i_>=pageCountCol?i_%pageCountCol:i_;
        ri_=(int)i_/pageCountCol;
      }
   }
}

int MSReportTable::tableTopPosition(int pageCount_)
{
  return report()->bodyTop(pageCount_)-tableTop()-printHeight();
}

int MSReportTable::tableBottomPosition(int pageCount_)
{
  return report()->bodyBottom(pageCount_)+tableBottom();
}

void MSReportTable::printTableHeaders(int x_,int y_) 
{
  report()->bodyBottom(report()->pageCount()+currentPage()); //pageCount());
  int top=report()->bodyTop(report()->pageCount())-printHeight();
  double scale=(scaleHeaders()==MSTrue)?x_printScale():1.0;
  
  for (unsigned i=0;i<headerList().count();i++)
   {
     if (report()->printOnPage(*header(i),_printPage,pageCountTotal())==MSTrue)
      {
        int bottom=report()->pageEnd();
        report()->pageEnd((int)(bottom-report()->bodyTopBase()/y_printScale()));
        header(i)->computePrintSize(report(),0,top,(int)(pageWidth()/scale),0,0,4);
        y_-=header(i)->print(report(),x_,y_,0,0,0,0);
        report()->pageEnd(bottom);
      }
   }
}

void MSReportTable::printTableFooters(int x_,int y_)
{
  report()->bodyBottom(report()->pageCount()+currentPage());
  int top=report()->bodyTop(report()->pageCount())-printHeight();
  double scale=(scaleHeaders()==MSTrue)?x_printScale():1.0;
  for (unsigned i=0;i<footerList().count();i++)
   {
     if (report()->printOnPage(*footer(i),_printPage,pageCountTotal())==MSTrue)
      {
        int bottom=report()->pageEnd();
        report()->pageEnd((int)(bottom-report()->bodyTopBase()/y_printScale()));
        footer(i)->computePrintSize(report(),0,top,(int)(pageWidth()/scale),0,0,4);
        y_-=footer(i)->print(report(),x_,y_,0,0,0,0);
        report()->pageEnd(bottom);
      }
   }
}

int MSReportTable::printTableSegment(int x_,int y_)
{
 int height=0,cols,segmentTotal=rowPageBreak().length()-1;
 cols=columnPageBreak().length()-1;
 if(cols >1) segmentTotal*=cols;
 segmentTotal*=newspaperColumns();
  if (segmentTotal>0)
   {
     int ri,ci;
     report()->pout<<"gs "<<endl;
     report()->postScriptStackInit();
     int frameThickness=frameLineWidth()+frameOffset();
     unsigned long style=frameStyle();
     int frameLeft=style&Box||style&BoxL?frameThickness:0;
     int frameRight=style&Box||style&BoxR?frameThickness:0;
     int w=tableWidths()(0)+frameLeft+frameRight;
     int x=x_>=report()->leftPixel()?x_-report()->leftPixel():x_;
     int pageWidth=report()->bodyRight()-report()->bodyLeft();
     int pageSize=(int)((report()->bodyTop(report()->pageCount())-report()->bodyBottom(report()->pageCount())-printHeight())/y_printScale());
     int tableWidth=(int)columnWidths().sum();
     double scale=scaleHeaders()==MSTrue?y_printScale():1.0;

     if(scaleHeaders()==MSTrue)
      {
       int off=x_!=0?x_:report()->leftPixel();
       report()->translateScale(x_printScale(),y_printScale(),off,y_);
       printTableHeaders(x,y_);
       report()->pout<<"gr "<<endl;
      }
     else  printTableHeaders(x,y_);
     if (x_printScale()>=1&&tableWidth<pageWidth)
      {
        int w1=w*newspaperColumns();
        int offset=newspaperColumns()>2?frameLeft*(newspaperColumns()-1):0;
	int margin=_pageWidth-w1-offset;
	if(margin<0)margin=0;
        x=justification()&MSRight?x+margin-frameRight:
          justification()&MSCenter?x+margin/2:x;
      }
     
     report()->translate(x,(y_-report()->bodyTop(report()->pageCount())+printHeight())*y_printScale());
     report()->translateScale(x_printScale(),y_printScale(),report()->leftPixel(),y_-(scaleHeaders()==MSFalse?tableHeaderHeight():0));
     int rowsOffset=reportGrandTotalOn()==MSTrue?1:0;
     for (unsigned j=0;j<newspaperColumns();j++)
      {
        computePageIndices(segment()+j,ri,ci);
        int h=computeTableSegmentHeight(rowPageBreak()(ri),rowPageBreak()(ri+1));
        height=height>h?height:h;
      }
     height=height>pageSize?pageSize:height;
     for (unsigned i=0;i<newspaperColumns()&&segment()<segmentTotal;i++)
      {
        computePageIndices(segment(),ri,ci);
//     if (segment%tableNewspaperColumns()>0) translate(w/table_->x_printScale()+frameLeft,0);
        if (segment()%newspaperColumns()>0) report()->translate(w+frameLeft,0);
        if (numFixedFields()>0)
         {
           printTableHeadings(0,numFixedFields());
           printTableRows(rowPageBreak()(ri),0,rowPageBreak()(ri+1),numFixedFields());
         }
        printTableHeadings(columnPageBreak()(ci),columnPageBreak()(ci+1));
        printTableRows(rowPageBreak()(ri),columnPageBreak()(ci),
                       rowPageBreak()(ri+1),columnPageBreak()(ci+1));
        printTableFrame(tableLeft(),
			tableTopPosition(report()->pageCount())-tableHeaderHeight(),
                        _pageWidth,
			(int)(height-(tableHeaderHeight()+tableFooterHeight())*scale));
        _segment++;
      }
     report()->pout<<"gr "<<endl;
     y_=(int)(y_-height+tableFooterHeight()*scale-tableBottom());
     if(scaleHeaders()==MSTrue)
      {
       int off=x_!=0?x_:report()->leftPixel();
       report()->translateScale(x_printScale(),y_printScale(),off,y_);
       printTableFooters(x,y_);
       report()->pout<<"gr "<<endl;
      }
     else  printTableFooters(x,y_);
     report()->postScriptStackInit();
     report()->fontID(INT_MAX);
   }
  return height;
}

void MSReportTable::printTableHeadings(int cs_,int ce_)
{
  int xx=report()->tabStop(0)>tableLeft()?report()->tabStop(0):tableLeft();
  if (cs_>=numFixedFields())
   {
     xx+=fixedFieldWidth();
     printGroupHeadings(cs_,ce_);     
   }
  int h=headingHeight()-reportHeadingOffset()-groupHeadingHeight();
  int ystart=tableTopPosition(report()->pageCount())-tableHeaderHeight()-groupHeadingHeight();
  
  for (unsigned j=cs_;j<ce_;j++) 
   {
     MSTableColumn *field=reportColumn(j);
     if (field!=0)
      {
	unsigned long style=report()->formatStyle(field->headingStyle());
	report()->bgGrayScale(field->headingBgGrayScale());
	report()->fgGrayScale(field->headingFgGrayScale());
	Font fid=report()->printFontID(field->reportHeadingFont());
	MSPrintFontData *fdata=report()->fontStruct(fid);
	report()->gcValues().font=fid;
	report()->gcValues().line_width=0;
//      report()->gcValues().foreground=field->headingForeground();
	if (report()->gcValues().font==0) report()->fontSize(report()->defaultFontSize());
	int yy=ystart-report()->fontSize();
	int textHeight=(report()->fontSize()+leading(0))*field->heading().length();
	int y=MSTop&style?0:MSBottom&style?h-textHeight:(h-textHeight)/2;
	printCellBox(style,xx,ystart,h,j);
	for (unsigned i=0;i<field->heading().length();i++)
	 {
	   int w=(int)fdata->textWidth(report()->fontSize(),field->heading()(i),field->heading()(i).length());
	   int cs=reportColumnSpacing(j)/2;
	   int cw=columnWidths()(j)+reportColumnSpacing(j);
	   int x=style&Cell?0:MSLeft&style?cs:MSRight&style?cw-w-cs:((cw-w)/2);
	   report()->printReportString(style,xx+x,yy-y,field->heading()(i),field->heading()(i).length());
	   yy-=report()->fontSize()+leading(0);
	 }
	xx=columnIndex()(j);
      }
   }
}

void MSReportTable::printTableRows(int rs_,int cs_,int re_,int ce_)
{
  MSPrintFontData *fdata;
  int w,x,yy;
  unsigned d,i,k,t;
  unsigned long styles;
  const char *cp;
  Font fid;
  MSRect tb;
  int xx=report()->tabStop(0)>tableLeft()?report()->tabStop(0):tableLeft();
  if (cs_>=numFixedFields()) xx+=fixedFieldWidth();
  unsigned breakIndexLength=breakIndex().length();
  unsigned breakTextIndexLength=breakTextIndex().length();
  MSString buffer;
  int count=breakTextCount();
  for (unsigned j=cs_;j<ce_;j++)
   {
     MSTableColumn *field=reportColumn(j);
     yy=tableTopPosition(report()->pageCount())-tableHeaderHeight()-headingHeight();
     int cs=reportColumnSpacing(j)/2;
     int cw=columnWidths()(j)+reportColumnSpacing(j);
     tb.height(0);
     tb.width(cw);
     if (field!=0&&j<numColumns())
      {
	int nRows=field->numRows();
	for (t=breakTextCount(),i=rs_,k=breakCount(),d=rs_-k-t;i<re_;i++,d++)
	 {
	   MSParagraph *text;
	   while ((k<breakIndexLength&&d==(breakIndex()(k))||
		   (t<breakTextIndexLength&&i==breakTextIndex()(t)))&&i<re_)
	    {
	      if (t<breakTextIndexLength&&i==breakTextIndex()(t))
	       {
		 if ((text=breakText(t++))!=0)
		  {
		    if (t>count&&lastPageCount()!=_printPage)
                     {
                       count++;
                       printBreakText(*text,yy);
                     }
		    yy-=rowHeights()(i)+rowLeadings()(i),i++;
		  }
		 else
		  {
		    t++;
		    yy-=rowHeights()(i)+rowLeadings()(i),i++;
		  }
	       }
	      if (reportTotalOn()==MSTrue&&k==breakColumn().length()-1)
	       {
		 fid=report()->printFontID(reportTotalFont());
		 styles=report()->formatStyle(reportTotalStyle());
	       }
	      else if (k<breakIndexLength)
	       {
		 fid=report()->printFontID(breakFont(k,breakColumn()(k)));
		 styles=report()->formatStyle(breakStyle(k,breakColumn()(k)));
	       }
	      if (k<breakIndexLength&&d==(breakIndex()(k)))
	       {
		 fdata=report()->fontStruct(fid);
		 if (field->suppressDuplicate()!=MSTrue)
		  {
		    report()->fgGrayScale(reportColumn(breakColumn()(k))->breakFgGrayScale((unsigned)k));
		    report()->bgGrayScale(reportColumn(breakColumn()(k))->breakBgGrayScale((unsigned)k));
		  }
                 int h=rowHeights()(i)+rowLeadings()(i);
                 printCellBox(field->suppressDuplicate()!=MSTrue?styles:(styles&~Box),xx,yy,h,j);
		 if ((cp=field->formatBreak(buffer.removeAll(),k,breakColumn()(k)))!=0)
		  {
		    report()->gcValues().font=fid;
//		 report()->gcValues().foreground=field->foreground();
		    w=(int)fdata->textWidth(report()->fontSize(),cp,strlen(cp));
		    x=styles&Cell?0:MSLeft&styles?cs:MSRight&styles?cw-cs-w:((cw-w)/2);
		    report()->printReportString(styles,xx+x,yy-report()->fontSize(),cp,strlen(cp));
		  }
		 k++;
		 yy-=rowHeights()(i)+rowLeadings()(i),i++;
	       }
	      tb.height(tb.y()-yy);
	    }
	   if (reportGrandTotalOn()==MSTrue&&i==(rowCount()-1))
	    {
	      i++;
	      if (lastPageCount()!=_printPage&&grandTotalBaseText().text().length()>0)
	       {
		 printBreakText(grandTotalBaseText(),yy);
	       }
	    }
	   if (i<re_)
	    {
	      fid=report()->printFontID(reportFont(d,j));
	      fdata=report()->fontStruct(fid);
	      styles=report()->formatStyle(style(d,j));
	      report()->bgGrayScale(bgGrayScale(d,j));
	      report()->fgGrayScale(fgGrayScale(d,j));
              int h=rowHeights()(i)+rowLeadings()(i);
	      printCellBox(field->suppressDuplicate()!=MSTrue?styles:(styles&~Box),xx,yy,h,j);
	      if (i==rs_||field->suppressDuplicate()!=MSTrue||field->isDuplicate(d)!=MSTrue)
	       {
		 if (tb.height()>0&&field->suppressDuplicate()==MSTrue&&styles&Box)
                  {
                    printCellBox(styles,tb.x(),tb.y(),tb.height(),j);
                  }
		 tb.x(xx); tb.y(yy);
		 if (d<nRows&&(cp=formatOutput(buffer.removeAll(),d,j))!=0)
		  {
		    report()->gcValues().font=fid;
//		    report()->gcValues().foreground=field->foreground();
		    w=(int)fdata->textWidth(report()->fontSize(),cp,strlen(cp));
		    x=styles&Cell?0:MSLeft&styles?cs:MSRight&styles?cw-cs-w:((cw-w)/2);
		    report()->printReportString(styles,xx+x,yy-report()->fontSize(),cp,strlen(cp));
		  }
	       }
	      yy-=rowHeights()(i)+rowLeadings()(i);
	      tb.height(tb.y()-yy);
	    }
	 }
	if (tb.height()>0&&field->suppressDuplicate()==MSTrue&&field->style()&Box)
         {
           printCellBox(field->style(),tb.x(),tb.y(),tb.height(),j);
         }
	xx=columnIndex()(j);
      }
     lastPageCount(_printPage); //?? report()->pageCount());
   }
  report()->pageFrameExtent(xx);
  if (cs_>=numFixedFields()) breakCount(re_!=rowCount()?k:0);
  if (cs_>=numFixedFields()) breakTextCount(re_!=rowCount()?t:0);
}

void MSReportTable::printTableFrame(int x_,int y_,int w_,int h_) 
{
  if (frameStyle()&(Box|BoxL|BoxR|BoxT|BoxB))
   {
     report()->gcValues().line_width=frameLineWidth();
     int frameThickness=frameLineWidth()+frameOffset();
     unsigned long style=frameStyle();
     int frameLeft=style&Box||style&BoxL?frameThickness:0;
     int frameTop=style&Box||style&BoxT?frameThickness:0;
     int frameBottom=style&Box||style&BoxB?frameThickness:0;
//      double x=table()->tableLeft()-frameLeft;
//      double y=tableTop(pageCount())+frameTop-table()->headerHeights; 
     double x=x_-frameLeft;
     double y=y_+frameTop;
     double h=(h_+frameTop+frameBottom)/y_printScale();
     double w=(w_-frameLeft)/x_printScale();
     w/=newspaperColumns();
     int w1=report()->pageFrameExtent()-tableLeft()+frameOffset()+2;
     if (frameStyle()&MSCenter&&w1<w&&w1>0) w=w1;
     report()->printBox(frameStyle(),x,y,w,h);
   }
}

void MSReportTable::printBreakText(MSParagraph& breakText_,int y_)
{
  int x=(report()->tabStop(0)>tableLeft()?report()->tabStop(0):tableLeft());
  for (unsigned i=0;i<breakText_.column();i++) x=setTableX(x,i);
  int w=(int)(pageWidth()/x_printScale()/newspaperColumns());
  int frameThickness=frameLineWidth()+frameOffset();
  int pageFrameThickness=report()->pageFrameLineWidth()+report()->pageFrameOffset();
  int offset=frameThickness+pageFrameThickness;
  w-=2*offset;
  if (report()->printOnPage(breakText_,_printPage,pageCountTotal())==MSTrue)
   {
     int bottom=report()->pageEnd();
     report()->pageEnd((int)(bottom-report()->bodyTopBase()/y_printScale()));
     breakText_.print(report(),x,y_,w-x+tableLeft(),0,0,offset);
     breakText_.computePrintSize(report(),0,0,w-x+tableLeft(),0,0,offset);
     report()->pageEnd(bottom);
   }
}

void MSReportTable::printASCIITable(void)
{
  const char *cp;
  unsigned i,j;
  unsigned long styles;
  MSIntVector cw;
  MSString pad;
  int lastColumn=numColumns()-1;
  int w,len,headingRows=0;
  const char *quotes=outputStyle()&Quoted?"\"":"";

  // compute the number of heading rows
  for (j=0;j<numColumns();j++)
   {
     MSTableColumn *field=reportColumn(j);
     if (field!=0)
      {
        int rows=field->heading().length();
        headingRows=headingRows>rows?headingRows:rows;
      }
   }
  // compute the column widths for the aligned mode
  if (outputStyle()&Aligned)
   {
     MSString buffer;
     for (j=0;j<numColumns();j++)
      {
	MSTableColumn *field=reportColumn(j);
	if (field!=0)
	 {
	   cw<<0;
	   if ((outputStyle()&NoHeadings)==0)
	    {
	      w=field->heading().maxLength();
	      cw[j]=w>cw(j)?w:cw(j);
	    }
	   for (i=0;i<field->numRows();i++)
	    {
	      formatOutput(buffer.removeAll(),i,j);
	      if (buffer.length()>0)
	       {
		 w=buffer.length();
		 cw[j]=w>cw(j)?w:cw(j);
	       }
	    }
	 }
      }
   }
  // print the column headings
  if ((outputStyle()&NoHeadings)==0)
   {
     MSStringVector headings(headingRows);
     for (j=0;j<numColumns();j++)
      {
	MSTableColumn *field=reportColumn(j);
	if (field!=0)
	 {
	   styles=report()->formatStyle(field->headingStyle());
	   for (unsigned k=0;k<headingRows;k++)
	    {
	      if (k<field->heading().length())
	       {
		 cp=field->heading()(k).string();
		 len=field->heading()(k).length();
	       }
	      else 
	       {
		 cp=0;
		 len=0;
	       }
              if (outputStyle()&Aligned)
               {
                 pad=MSString::copy(" ",cw(j)-len);
                 if (styles&MSRight) headings[k]<<pad<<cp;
                 else headings[k]<<cp<<pad;
                 if (j<lastColumn) headings[k]<<delimiter();
               }
              else headings[k]<<quotes<<cp<<quotes<<delimiter();
	    }
	 }
      }
     report()->pout<<headings;
   }
  MSString buffer;
  for (i=0;i<numRows();i++)
   {
     for (j=0;j<numColumns();j++)
      {
	MSTableColumn *field=reportColumn(j);
	if (field!=0&&i<field->numRows())
	 {
	   styles=report()->formatStyle(field->style());
           if (field->format().formatType()==MSFormat::Float&&report()->asciiFloatFormat()!=0)
            {
              MSFloat::MSFloatFormat format=field->format().floatFormat();
              unsigned long modifier=field->format().formatModifier();
              field->format().formatModifier(MSFormat::NoModifier);
              field->format().format(report()->asciiFloatFormat());
              formatOutput(buffer.removeAll(),i,j);
              field->format().format(format);
              field->format().formatModifier(modifier);
            }
           else if (field->format().formatType()==MSFormat::Int&&report()->asciiIntFormat()!=0)
            {
              MSInt::MSIntFormat format=field->format().intFormat();
              unsigned long modifier=field->format().formatModifier();
              field->format().formatModifier(MSFormat::NoModifier);
              field->format().format(report()->asciiIntFormat());
              formatOutput(buffer.removeAll(),i,j);
              field->format().format(format);
              field->format().formatModifier(modifier);
            }
           else formatOutput(buffer.removeAll(),i,j);
           if (outputStyle()&Quoted&&field->valueQuoted()==MSTrue)
            {
              buffer.insert("\"",0)<<"\"";
            }
           len=buffer.length();
	   if (outputStyle()&Aligned) pad=MSString::copy(" ",cw(j)-len);
	   if (styles&MSRight) report()->pout<<pad<<buffer;
	   else report()->pout<<buffer<<pad;
	   if (j<lastColumn) report()->pout<<delimiter();
	 }
      }
     report()->pout<<endl;
   }
}

void MSReportTable::printCellBox(unsigned long mask_,int x_,int y_,int h_,int j_)
{
//  report()->gcValues().foreground=report()->lineColor();
  int cw=Cell&mask_?columnWidths()(j_):columnWidths()(j_)+reportColumnSpacing(j_);
  int w=Cell&mask_?cw:report()->tabStop(j_+1)>x_?report()->tabStop(j_+1)-x_:cw;
  w=w<cw?cw:w;
  if (x_printScale()==1&&(x_+w)>tableRight()) w=tableRight()-x_;
  if (report()->bgGrayScale()<1.) report()->fillRectangle(x_,y_,w,h_);
  report()->printBox(mask_,x_,y_,w,h_);
}

void MSReportTable::computeGroupHeadingSize(void)
{
  MSParagraph *heading;
  MSIntVector  rows;
  if (groupHeadingList()!=0)
   {
     sortGroupHeadingsByColumn();
     for (unsigned k=0;k<groupHeadingList()->count();k++)
      {
	if ((heading=groupHeading(k))!=0&&heading->text().length()>0)
	 {
	   int h =heading->computePrintSize(report(),0,report()->y_org(),report()->x_end(),0,0,2);
	   unsigned j;
           for (j=0;j<rows.length();j++) if (rows(j)==heading->row()) break;
           if (j==rows.length())
            {
              rows<<heading->row();
              groupHeadingHeights()<<(h-(heading->row()==0?heading->topPixel():0));
            }
           else if (h>groupHeadingHeights()(j))
            {
              groupHeadingHeights()[j]=h;
            }
         }
      }
     groupHeadingHeight((int)(groupHeadingHeights().sum()));
   }
}

void MSReportTable::sortGroupHeadingsByColumn(void)
{
  unsigned i,j,k,min,row=0;
  int c,minCol,count=groupHeadingList()->count();
  MSIntVector columns;
  // sort by column
  for (i=0;i<count;i++)
   {
     minCol=groupHeading(i)->column();
     groupHeading(i)->row(0);  // initialize the rows
     for (min=i,j=i+1;j<count;j++)
      {
	if (((c=groupHeading(j)->column())>=0&&c<minCol)||(minCol<0&&c>minCol))
	 {
	   min=j;
	   minCol=c;
	 }
      }
     groupHeadingList()->exchange(min,i);
     minCol=groupHeading(i)->column();
     if ((columns.length()==0||columns.indexOf(minCol)==columns.length())&&minCol>=0) columns<<minCol;
   }
  columns.sortUp();
  // sort by columnSpan
  for (i=0;i<columns.length();i++)
   {
     int span,min,col=columns(i);
     for (j=0;j<count;j++)
      {
	if (groupHeading(j)->column()==col)
	 {
	   int minSpan=groupHeading(j)->columnSpan();
	   for (min=j,k=j+1;k<count;k++)
	    {
	      if (groupHeading(k)->column()==col)
	       {
		 if ((span=groupHeading(k)->columnSpan())<minSpan)
		  {
		    min=k;
		    minSpan=span;
		    groupHeadingList()->exchange(min,j);
		  }
	       }
	    }
	 }
      }
   }
  // first pass at setting the rows using only the starting column
  int maxRows=0;
  for (i=0;i<columns.length();i++)
   {
     int column=columns(i);
     for (row=0,j=0;j<count;j++) if (groupHeading(j)->column()==column) groupHeading(j)->row(row++);
     maxRows=row>maxRows?row:maxRows;
   }

  for (i=0;i<maxRows;i++)
   {
     int span=-1;
     for (j=0;j<count;j++)
      {
        if ((row=groupHeading(j)->row())==i)
         {
           if (groupHeading(j)->column()<=span)
            {
              groupHeading(j)->row(++row);
              maxRows=row>maxRows?row:maxRows;
            }
           else span=groupHeading(j)->column()+groupHeading(j)->columnSpan()-1;
         }
      }
   }
  sortTextListByRow(*groupHeadingList());
}

void MSReportTable::sortTextListByRow(MSPointerArray<MSParagraph>& list_)
{
  for (int i=0;i<list_.count();i++)
   {
     int min=i;
     for (int j=i+1;j<list_.count();j++) if (list_.array(j)->row()<list_.array(min)->row()) min=j;
     list_.exchange(min,i);
   }
}


void MSReportTable::printGroupHeadings(int cs_,int ce_)
{
  // this is a hack for now
  // needs to use MSParagraph computeSize and print methods
  MSParagraph *heading;
  if (groupHeadingList()!=0)
   {
     MSIntVector *columnsPerRow=new MSIntVector[groupHeadingHeights().length()];
     for (unsigned k=0;k<groupHeadingList()->count();k++) 
      {
        int xx=report()->tabStop(0)>tableLeft()?report()->tabStop(0):tableLeft();
        if ((heading=groupHeading(k))!=0/*&&heading->outputText().length()>0*/)
         {
           int column=heading->column();
           int span=column+heading->columnSpan();
           for (int c=cs_;c<ce_;c++) if (c>=column&&c<span) columnsPerRow[heading->row()]<<c;
	   
           if (column<numColumns()&&((column>=cs_&&column<=ce_)||span>cs_))
            {
              int index=(span<ce_?span:ce_)-1;
              int ystart=tableTopPosition(report()->pageCount())-tableHeaderHeight()-groupHeadingHeight();
              for (unsigned r=0;r<heading->row()+1;r++) ystart+=groupHeadingHeights()(r);
              if (column>cs_)
               {
                 xx=columnIndex()(column-1);
               }
              else 
               {
                 xx+=fixedFieldWidth();
                 span-=cs_;
               }
              int cs=reportColumnSpacing(column)/2;
              int cw=columnIndex()(index)-xx;
              int tableRight=columnIndex()(ce_-1);
              if (xx+cw>tableRight||index>=ce_) cw=tableRight-xx;
              report()->bgGrayScale(heading->bgGrayScale());
              report()->fgGrayScale(heading->fgGrayScale());
              unsigned long style=report()->formatStyle(heading->style());
              MSPrintFontData *fdata=report()->fontStruct(heading->fontID());
              report()->fontSize(heading->fontSize());
              report()->gcValues().font=heading->fontID();
              report()->gcValues().line_width=0;
              int yy=(int)(ystart-fdata->fontOffset(report()->fontSize()));
//              int yy=ystart-fontSize();
              int textHeight=heading->height();
              int h=groupHeadingHeights()(heading->row());
              int y=MSTop&style?0:MSBottom&style?h-textHeight:(h-textHeight)/2;
              if (style&Box||style>BoxL)
               {
                 int i,leftHeight=h,rightHeight=h;
                 for (i=heading->row()-1;i>=0;i--) 
                  {
                    if (columnsPerRow[i].indexOf(column)==columnsPerRow[i].length())
                     {
                       leftHeight+=groupHeadingHeights()(i);
                     }
                    if (columnsPerRow[i].indexOf(span-1)==columnsPerRow[i].length())
                     {
                       rightHeight+=groupHeadingHeights()(i);
                     }
                  }
                 printGroupHeadingBox(style,xx,ystart,cw,leftHeight,rightHeight);
               }
              for (int w=0,n=0,i=0;i<heading->outputText().length();i++)
               {
                 const char *cp=heading->outputText()(i);
                 for (;w<cw&&n<heading->outputText()(i).length();n++) w+=(int)fdata->textWidth(report()->fontSize(),cp+n,1);
                 if (w>cw) w-=(int)fdata->textWidth(report()->fontSize(),cp+(--n),1);
                 int x=MSLeft&style?cs:MSRight&style?cw-w-cs:((cw-w)/2);
                 report()->printReportString(style,xx+x,yy-y,cp,n);
                 yy-=heading->fontSize()+heading->leading();
               }
            }
         }
      }
     delete [] columnsPerRow;
   }
}

void MSReportTable::printGroupHeadingBox(unsigned long mask_,int x_,int y_,int w_,int lh_,int rh_)
{
//  report()->gcValues().foreground=report()->lineColor();
  int h=lh_<rh_?lh_:rh_;
  if (report()->bgGrayScale()<1.) report()->fillRectangle(x_,y_,w_,h);
  if (Box&mask_&&lh_==rh_) report()->strokeRectangle(x_,y_,w_,h);
  else 
   {
     report()->setAttributes();
     if (BoxL&mask_||Box&mask_) report()->printLine(x_,y_,x_,y_-lh_);
     if (BoxR&mask_||Box&mask_) report()->printLine(x_+w_,y_,x_+w_,y_-rh_);
     if (BoxT&mask_||Box&mask_) report()->printLine(x_,y_,x_+w_,y_);
     if (BoxB&mask_) report()->printLine(x_,y_-h,x_+w_,y_-h);
   }
}
