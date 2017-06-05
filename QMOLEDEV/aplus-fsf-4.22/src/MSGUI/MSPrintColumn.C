/////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPrintColumn.H>
#include <MSGUI/MSPrintHeaders.H>
#include <MSGUI/MSReport.H>
#include <MSTypes/MSUtil.H>

extern const int  MSPointsPerInch;

MSPrintColumn::MSPrintColumn(MSPrintManager *parent_,unsigned numColumns_,const MSSymbol& tag_) :
    _tag(tag_), MSPrintManager(parent_)
{
  init();
  _numColumns=numColumns_;
  parent_->addPrintItem(this);
}

MSPrintColumn::MSPrintColumn(MSPrintManager *parent_,MSFloatVector& columnWidths_,unsigned numColumns_,const MSSymbol& tag_) : _tag(tag_), MSPrintManager(parent_)
{
  init();
  _numColumns=numColumns_;
  _columnWidths=columnWidths_;
  parent()->addPrintItem(this);
}

MSPrintColumn::MSPrintColumn(unsigned numColumns_,const MSSymbol& tag_) : _tag(tag_)
{
  init();
  _numColumns=numColumns_;
}

MSPrintColumn::MSPrintColumn(MSFloatVector& columnWidths_,unsigned numColumns_,const MSSymbol& tag_) : _tag(tag_)
{
  init();
  _numColumns=numColumns_;
  _columnWidths=columnWidths_;
}

MSPrintColumn::~MSPrintColumn(void)
{
  removeAll();
  if (parent()!=0)
   {
    destroyable(MSFalse);
    parent()->removePrintItem(this);
   }
  if (owner()!=0)
   {
     destroyable(MSFalse);
     owner()->removeHeader(this);
     owner()->removeFooter(this);
   }
}

void MSPrintColumn::init(void)
{
  _owner=0;
  _residualHeight=0;
  _pixelWidth=0;
  _pixelHeight=0;
  _leftIndent=1;
  _rightIndent=1;
  _style=0;
  _headingsOn=MSFalse;
  topPixel(2);
  bottomPixel(2);
}

MSPrintColumn& MSPrintColumn::tag(const MSSymbol& x_)
{_tag=x_; return *this;}

MSPrintColumn& MSPrintColumn::columnWidths(const MSFloatVector& x_)
{_columnWidths=x_; return *this;}

MSPrintColumn& MSPrintColumn::justification(MSAlignment x_)
{MSPrintItem::justification(x_); return *this;}

MSPrintColumn& MSPrintColumn::justification(unsigned long x_)
{MSPrintItem::justification(x_); return *this;}

MSPrintColumn& MSPrintColumn::leftIndent(unsigned x_)
{_leftIndent=x_>0?x_:1; return *this;}

MSPrintColumn& MSPrintColumn::rightIndent(unsigned x_)
{_rightIndent=x_>0?x_:1; return *this;}

MSPrintColumn& MSPrintColumn::topPixel(unsigned x_)
{MSPrintItem::topPixel(x_); return *this;}

MSPrintColumn& MSPrintColumn::bottomPixel(unsigned x_)
{MSPrintItem::bottomPixel(x_); return *this;}

MSPrintColumn& MSPrintColumn::numColumns(unsigned x_)
{_numColumns=x_; return *this;}

MSPrintColumn& MSPrintColumn::printRow(int x_)
{MSPrintItem::printRow(x_); return *this;}

MSPrintColumn& MSPrintColumn::printColumn(int x_)
{MSPrintItem::printColumn(x_); return *this;}

MSPrintColumn& MSPrintColumn::row(int x_)
{MSPrintItem::printRow(x_); return *this;}

MSPrintColumn& MSPrintColumn::column(int x_)
{MSPrintItem::printColumn(x_); return *this;}

MSPrintColumn& MSPrintColumn::occurrence(unsigned long x_)
{MSPrintItem::occurrence(x_); return *this;}

MSPrintColumn& MSPrintColumn::occurrence(Occurrence x_)
{MSPrintItem::occurrence(x_); return *this;}

MSPrintColumn& MSPrintColumn::pageAlignment(unsigned long x_)
{MSPrintItem::pageAlignment(x_); return *this;}

MSPrintColumn& MSPrintColumn::pageAlignment(MSAlignment x_)
{return pageAlignment((unsigned long)x_);}

MSPrintColumn& MSPrintColumn::font(const char *string_)
{MSPrintItem::printFont(string_); return *this;}

MSPrintColumn& MSPrintColumn::topOffset(double x_)
{MSPrintItem::topOffset(x_); return *this;}

MSPrintColumn& MSPrintColumn::bottomOffset(double x_)
{MSPrintItem::bottomOffset(x_); return *this;}

MSPrintColumn& MSPrintColumn::leftMargin(double x_)
{MSPrintItem::leftMargin(x_); return *this;}

MSPrintColumn& MSPrintColumn::rightMargin(double x_)
{MSPrintItem::rightMargin(x_); return *this;}

MSPrintColumn& MSPrintColumn::headingsOn(MSBoolean x_)
{_headingsOn=x_; return *this;}

MSPrintColumn& MSPrintColumn::style(unsigned long style_)
{
  // for backward compatibility before the justification attribute
  unsigned long just=style_&(MSRight|MSLeft|MSTop|MSBottom|MSCenter);
  if (just>0) justification(just);
  _style=style_;
  return *this;
}

double MSPrintColumn::topOffset(void) const
{return MSPrintItem::topOffset();}

double MSPrintColumn::bottomOffset(void) const
{return MSPrintItem::bottomOffset();}

double MSPrintColumn::leftMargin(void) const
{return MSPrintItem::leftMargin();}

double MSPrintColumn::rightMargin(void) const
{return MSPrintItem::rightMargin();}

const MSSymbol& MSPrintColumn::printTag(void) const
{return tag();}

//
// printing methods
//

unsigned MSPrintColumn::columnPixel(unsigned i_)
{
  unsigned x=leftPixel();
  for (unsigned i=0;i<i_;i++) x+=columnPixelWidth(i);
  return x;
}

unsigned MSPrintColumn::columnPixelWidth(unsigned i_)
{
  unsigned len=columnPixelWidth().length();
  return columnPixelWidth()(i_<len?i_:i_%len);
}

void MSPrintColumn::computeColumnWidths(void)
{
  columnPixelWidth().removeAll();
  int len,w=pixelWidth();

  if ((len=columnWidths().length())!=0)
   {
     double scale=1,cw=columnWidths().sum()*MSPointsPerInch;
     if (w>0&&cw>w) scale=w/cw;

     for (unsigned i=0;i<len;i++) columnPixelWidth() << unsigned(_columnWidths[i]*MSPointsPerInch*scale);

     if (len<columnCount()&&cw<w) 
      {
	unsigned residual=(int)(w-cw)/(columnCount()-len);
	for (unsigned j=len;j<columnCount();j++) columnPixelWidth()<<residual;
      }
   }
  else columnPixelWidth()<<(w/columnCount());
}

void MSPrintColumn::sortPrintItemsByRowColumn(void)
{
  unsigned i,j,k,min,rows=0;
  int c,minCol,n,count=printItemList().count();
  columnCount(0); fixedRowCount(0);
  MSIntVector columns;
  for (i=0;i<count;i++)
   {
     minCol=item(i)->printColumn();
     for (min=i,j=i+1;j<count;j++)
      {
	if (((c=item(j)->printColumn())>=0&&c<minCol)||(minCol<0&&c>minCol))
	 {
	   min=j;
	   minCol=c;
	 }
      }
     printItemList().exchange(min,i);
     minCol=item(i)->printColumn();
     if (minCol>=0&&columns.indexOf(minCol)==columns.length()) columns<<minCol;
   }
  columns.sortUp();
  columnRowCounts().reshape(columns.length());
  n=columns.length();
  for (i=0;i<n;i++)
   {
     int row,min,col=columns(i);
     for (rows=0,j=0;j<count;j++)
      {
	if (item(j)->printColumn()==col)
	 {
	   rows++;
	   int minRow=item(j)->printRow();
	   for (min=j,k=j+1;k<count;k++)
	    {
	      if (item(k)->printColumn()==col)
	       {
		 if ((row=item(k)->printRow())<minRow)
		  {
		    min=k;
		    minRow=row;
		    printItemList().exchange(min,j);
		  }
	       }
	    }
	 }
      }
     columnRowCounts().replaceAt(i,rows);
   }
  columnCount(columns.length());
}

void MSPrintColumn::computeConfiguration(void)
{
  int count=printItemList().count();

  if (columnCount()>0)
   {
     int allocated=(int)columnRowCounts().sum();
     //Check if we have distributed less items than we actually have
     if (allocated<count)
      {
       //Redistribute the leftover items evenly into the left over columns.
       //If no more items is left add them into the last column. Not the most
       //elegant solution, but easier to implement. Otherwise a second pass would be
       //required to resort the items by row/column order.
	if (columnCount()<numColumns())
	 {
	   int residualCount=count-allocated;
	   int residualColumns=numColumns()-columnCount();
	   int residualRowCount=residualCount/residualColumns;
	   int residualRows=residualCount%residualColumns;
	   unsigned newCount=0;
	   for (unsigned i=columnCount();i<numColumns();i++)
	    {
	      newCount=residualRowCount;
	      if(residualRows>0) { residualRows--; newCount++;}
	      columnRowCounts()<<newCount;
	    }
	   columnCount(numColumns());
	 }
	else
	 {
	  int residualCount=count-allocated;
	  columnRowCounts()[columnRowCounts().length()-1]+=residualCount;
	 }
      }
   }
  else if (count<=numColumns())
   {
     columnCount(count==0?1:count);
     fixedRowCount(1);
   }
  else if (count>numColumns())
   {
     columnCount(numColumns());
     if ((count%numColumns())==0) fixedRowCount(count/numColumns());
     else fixedRowCount(count/numColumns()+1);
   }
}

void MSPrintColumn::computeChildrenSize(MSReport *report_,int x_,int y_,int,int h_,int topMargins_,int)
{
  unsigned startRow=0,row,col,i;
  int y_start=y_-topPixel();
  int h,y=y_start;
  int headingHeight=0;
  sortPrintItemsByRowColumn();
  computeConfiguration();
  computeColumnWidths();
  pageBreakRow(-1);
  int maxRows=columnRowCounts().max();
  maxRows=maxRows>fixedRowCount()?maxRows:fixedRowCount();
  MSUnsignedVector heights(maxRows,0);
  MSUnsignedVector pageCounts(maxRows,0);
  // if headingsOn is true determine the heading height on the subsequent pages. 
  if (headingsOn()==MSTrue)
   {
     for (col=0,i=0;i<printItemList().count();i+=rowCount(col++))
      {
        MSPrintItem *pItem=item(i);
        pItem->pageAlignment(MSNone);
        if (pItem->printFont().length()==0) pItem->printFont(printFont());
	if(pItem->printRow()>0) continue;
        h=pItem->computePrintSize(report_,x_,report_->bodyTop()-topMargins_,columnPixelWidth(col),h_,topMargins_,indents());
        // It must be less then one page or it can't be used as a heading
        if (pItem->pageCount()==0) heights[0]=heights(0)>h?heights(0):h;
      }
     headingHeight=heights(0);
     startRow=1;
   }
  // check if there is enough room to print anything on the current page
  // if it gets bumped to the next page, increment the report page count
  // so that the children get the right page sizes.
  int border=0;
//  if(style()&MSP::Box) border=2*(lineWidth());
//  else border= (style()&MSP::BoxT?lineWidth():0)+(style()&MSP::BoxB?lineWidth():0);
  int minHeight=topPixel()+headingHeight+border+10; //TODO: where is this 10 came from??????
  int remainingHeight=y_start-report_->pageEnd()-topMargins_;
  int startingPageCount=report_->pageCount();
  if (minHeight>remainingHeight||(owner()==0&&pageAlignment()&MSTop&&y_!=report_->bodyTop()))
   {
     _pageCount++;
     report_->pageCountIncrement();
     y_start=report_->bodyTop()-topPixel();
     y=y_start;
     remainingHeight=y_start-report_->pageEnd()-topMargins_;
   }
  int columnStartPage=report_->pageCount();
  int runningPageCount=pageCount();

  // compute sizes of each of the children on a row basis
  // this not very efficient loop because the list sorted in 
  // column major order, and now we have to traverse in row major
  // order. in future modify list to keep items in row major order.
  int colStart,myRow;
  for(row=(headingsOn()==MSTrue?1:0);row<maxRows;row++)
   {
    colStart=0;
    i=0;
    for(col=0,myRow=0;col<numColumns();col++,myRow=0)
     {
      if(row<rowCount(col))
       {
	i=colStart+row;
//      while(myRow++<row) {if(item(i)->printRow()<(int)row) i++; }

      if(myRow<=rowCount(col)&&i<printItemList().count())
       {
	MSPrintItem *pItem=item(i);
	pItem->pageAlignment(MSNone);
	if (report_->printOnPage(*pItem,report_->pageCount(),report_->pageCountTotal())==MSTrue)
	 {
	  if (pItem->printFont().length()==0) pItem->printFont(printFont());
	  h=pItem->computePrintSize(report_,x_,y,columnPixelWidth(col),h_,
				    topMargins_+headingHeight+border,indents());
	  pageCounts[row]=pageCounts(row)>pItem->pageCount()?pageCounts(row):pItem->pageCount();
	  if(pItem->pageCount()==pageCounts(row))
	   {
	    heights[row]=heights(row)>h?heights(row):h;
	   }
	 }
       }
       }
      report_->pageCount(columnStartPage);
      colStart+=rowCount(col);
      i=colStart;
     }
    if (pageCounts(row)>0)
     {
      runningPageCount =pageCounts(row);
      columnStartPage+=runningPageCount;
      y=report_->bodyTop(columnStartPage);
      report_->pageCount(columnStartPage);
      remainingHeight=y-report_->pageEnd()-topMargins_;
     }
    y-=heights(row);
   }
  // compute residual sizes for the print column based on the
  // items appearing on the last page of the print column
  _pageCount+=(int)pageCounts.sum();
  int resid=0;
  for(int j=maxRows-1;j>=(int)startRow;j--)
   {
     resid+=heights(j);
     if(pageCounts(j)!=0) break;
   }
  if(resid!=0)
   {
    if(pageCount()==0) resid+=topPixel();
    if(headingsOn()==MSTrue) resid+=heights(0);
    if(resid+bottomPixel()<remainingHeight) resid+=bottomPixel();
    else 
     {
       resid=0;
       _pageCount++;
     }
   }
  residual(resid);
  rowHeights()=heights;
  rowPageCounts()=pageCounts;
  report_->pageCount(startingPageCount);
}

unsigned MSPrintColumn::rowCount(unsigned index_)
{
  return index_<columnRowCounts().length()?columnRowCounts()(index_):fixedRowCount();
}

int MSPrintColumn::computePrintSize(MSReport* report_,int x_,int y_,int w_,int h_,int topMargins_,int margins_)
{
  reset();
  if (printItemList().count()>0)
   {
     if (margins_==0)
      {
        if (leftPixel()<0) leftPixel(report_->leftPixel());
        if (rightPixel()<0) rightPixel(report_->rightPixel());
        margins_=leftPixel()+rightPixel();
      }
//     margins_+=(style()&MSP::Box)?lineWidth()*2:style()&MSP::BoxL
     pixelWidth(w_-margins_);
     computeChildrenSize(report_,x_,y_,w_,h_,topMargins_,margins_);
     int remainingHeight=y_-report_->pageEnd()-topMargins_;
     if (pageCount()==0&&(pageAlignment()&MSCenter||pageAlignment()&MSBottom))
      {
       _pageCount++;
       _residual=0;
        residualHeight(remainingHeight);
      }
     else if (residual()!=0&&residual()+bottomPixel()<remainingHeight)
      { 
       _residual+=bottomPixel();
      }
   }
  return residual();
}
  
int MSPrintColumn::print(MSReport *report_,int x_,int y_,int,int,int bottomIndent_,int leftIndent_)
{
  if (report_->outputMode()==ASCII)
   {
     for(int index=0;index<printItemList().count();index++)
      {
        item(index)->print(report_,0,0,0,0,0,0);
      }
     return 0;
   }
  if (printItemList().count()>0)
   {
     unsigned row,col,i,j;
     int currentPageBreakRow=-1;
     int rowHeight,y_start=y_-(pageBreakRow()<0?topPixel():0);
     int row_start=pageBreakRow()>0?pageBreakRow():0;
     int y=y_start;
     int headingHeight=0;
     int left=leftIndent_;
     int top=0;
/*
     int bottom=0;
     if(style()&MSP::Box) top=bottom=lineWidth();
     else
      {
       if(style()&MSP::BoxT) top=lineWidth();
       if(style()&MSP::BoxB) bottom=lineWidth();
      }
     y-=top;
     y_start-=top;
     bottomIndent_+=bottom;
     */
     if (columnPixelWidth().length()==1)
      {
        int residual=pixelWidth()-columnPixelWidth(0)*columnCount();
        left+=residual/2;
      }
     if (headingsOn()==MSTrue&&owner()==0) headingHeight=rowHeights()(0);
     int minHeight=topPixel()+headingHeight+top+10;
     int remainingHeight=y_-report_->pageEnd()-bottomIndent_;
     if (minHeight>remainingHeight||
         (currentPage()==0&&(owner()==0&&pageAlignment()&MSTop&&y_!=report_->bodyTop()))) 
      {
       _currentPage++;
//       pageBreakRow(0);
        return remainingHeight;
      }
     if (pageCount()==0&&residualHeight()<remainingHeight)
      {
        if (pageAlignment()&MSCenter)
         {
           y_start=y_-(remainingHeight-residualHeight())/2;
         }
        else if (pageAlignment()&MSBottom)
         {
 	   y_start=report_->pageEnd()+residualHeight()+bottomIndent_;
         }
      }
     if (headingsOn()==MSTrue)
      {
        headingHeight=rowHeights()(0);
        for (col=0,i=0;i<printItemList().count()&&col<columnRowCounts().length();i+=rowCount(col++))
         {
           int x=x_+columnPixel(col)+left;
           int w=columnPixelWidth(col);
           if (report_->printOnPage(*item(i),report_->pageCount(),report_->pageCountTotal())==MSTrue)
            {
	     //Reset pageBreakRow, so that item thinks that it has never been printed yet.
              item(i)->pageBreakRow(-1);
              item(i)->print(report_,x,y,w,headingHeight,0,leftIndent());
            }
         }
        y_start-=headingHeight;
      }
     else headingHeight=0;
     y=y_start;
     for (i=j=row=row_start,col=0;i<printItemList().count();i++,j++,row++)
      {
       int test=rowCount(col);
        if (row>=rowCount(col))
         {
           i+=rowCount(col)+row_start-j;
           col++;
           row=j=row_start;
           y=y_start;
           if (i>=printItemList().count()) continue;
         }
	if(headingsOn()==MSTrue&&row==0) continue;
        if (y-bottomIndent_>=report_->pageEnd())
         {
           int x=x_+columnPixel(col)+left;
           int w=columnPixelWidth(col);
           if(rowPageCounts()(row)==0) rowHeight=rowHeights()(row);
           else if(rowPageCounts()(row)==item(i)->currentPage()) rowHeight=rowHeights()(row);
           else rowHeight=y-report_->bodyBottom(report_->pageCount())-bottomIndent_;
        
           if (report_->printOnPage(*item(i),report_->pageCount(),report_->pageCountTotal())==MSTrue&&
               (pageBreakRow()<0||item(i)->pageBreakRow()>=0)||row>pageBreakRow())
            {
              item(i)->print(report_,x,y,w,rowHeight,bottomIndent_,leftIndent());
            }
	   //WARNING: The next was changed from "<=" to < which should solve
	   //some 1-pixel edge conditions. 
           if (y-rowHeight-bottomIndent_<report_->pageEnd())
            {
              currentPageBreakRow=row;
              row=rowCount(col);
            }
	   y-=rowHeight;
         }
      }
     pageBreakRow(currentPageBreakRow);
   }
  _currentPage++;
  //This is not entirely correct. However the return value is currently used only in
  //MSReport and it is ignored unless this is the last page of the printed item, in which
  //case that it is exactly the "residual()". Ideally the actuall printed height should be returned.
  return residual();
}

