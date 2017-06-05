///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPrintItem.H>
#include <MSGUI/MSReport.H>

extern const int  MSPointsPerInch;

MSPrintItem::MSPrintItem(void)
{
  _printManager=0;
  _printRow=-1;
  _printColumn=-1;
  _leftPixel=-1;
  _rightPixel=-1;
  _topPixel=0;
  _bottomPixel=0;
  _destroyable=MSTrue;
  _occurrence=EveryPage;
  _justification=MSLeft;
  _pageAlignment=MSNone;
  reset();
}

MSPrintItem::MSPrintItem(const MSPrintItem& printItem_) :
_printRow(printItem_._printRow),
_printColumn(printItem_._printColumn),
_printFont(printItem_._printFont),
_leftPixel(printItem_._leftPixel),
_rightPixel(printItem_._rightPixel),
_topPixel(printItem_._topPixel),
_bottomPixel(printItem_._bottomPixel),
_occurrence(printItem_._occurrence),
_justification(printItem_._justification),
_pageAlignment(printItem_._pageAlignment),
_destroyable(printItem_._destroyable)
{reset();}

MSPrintItem& MSPrintItem::operator=(const MSPrintItem& printItem_)
{
_printRow=printItem_._printRow;
_printColumn=printItem_._printColumn;
_printFont=printItem_._printFont;
_leftPixel=printItem_._leftPixel;
_rightPixel=printItem_._rightPixel;
_topPixel=printItem_._topPixel;
_bottomPixel=printItem_._bottomPixel;
_occurrence=printItem_._occurrence;
_justification=printItem_._justification;
_pageAlignment=printItem_._pageAlignment;
_destroyable=printItem_._destroyable;
reset();
return *this;
}

MSPrintItem::~MSPrintItem(void)
{}
void MSPrintItem::reset(void)
{
  _changed=MSTrue;
  _pageBreakRow=-1;
  _printHeight=0;
  _pageCount=0;
  _residual=0;
  _currentPage=0;
}

double MSPrintItem::leftMargin(void) const
{return _leftPixel<0?-1.0:_leftPixel==5?0.0:(double)_leftPixel/MSPointsPerInch;}
double MSPrintItem::rightMargin(void) const
{return _rightPixel<0?-1.0:_rightPixel==5?0.0:(double)_rightPixel/MSPointsPerInch;}
double MSPrintItem::topOffset(void) const
{return _topPixel/(double)MSPointsPerInch;}
double MSPrintItem::bottomOffset(void) const
{return _bottomPixel/(double)MSPointsPerInch;}

void MSPrintItem::leftMargin(double x_)
{_leftPixel=x_<=0.07?5:(int)(MSPointsPerInch*x_);}
void MSPrintItem::rightMargin(double x_)
{_rightPixel=x_<=0.07?5:(int)(MSPointsPerInch*x_);}
void MSPrintItem::topOffset(double x_)
{_topPixel=int(x_*MSPointsPerInch);}
void MSPrintItem::bottomOffset(double x_)
{_bottomPixel=int(x_*MSPointsPerInch);}

int MSPrintItem::computePrintSize(MSReport*,int,int,int,int,int,int)
{return 0;}
int MSPrintItem::print(MSReport*,int,int,int,int,int,int)
{return 0;}
const MSSymbol& MSPrintItem::printTag(void) const
{return MSSymbol::nullSymbol();}

//
//  MSPageBreakPrintItem
//

MSPageBreakPrintItem::MSPageBreakPrintItem()
{}
MSPageBreakPrintItem::~MSPageBreakPrintItem(void) {}
int MSPageBreakPrintItem::computePrintSize(MSReport *,int,int /*y_*/,int,int,int,int)
{
  _residual=0;
  _pageCount++;
  return 0;
// return y_-report_->pageEnd();
}
int MSPageBreakPrintItem::print(MSReport *report_,int,int y_,int,int,int,int)
{
  _currentPage++;
  return y_-report_->pageEnd();
}

//
//  MSRulePrintItem
//

MSRulePrintItem::MSRulePrintItem(unsigned ruleWidth_)
{
  _ruleWidth=ruleWidth_;
  _fgGrayScale=0;
}
MSRulePrintItem::~MSRulePrintItem(void)
{}

int MSRulePrintItem::computePrintSize(MSReport *report_,int,int y_,int,int,int,int)
{
  reset();
  if (leftPixel()<0) leftPixel(report_->leftPixel());
  if (rightPixel()<0) rightPixel(report_->rightPixel());
  int remainingHeight=y_-report_->pageEnd();
  int h=ruleWidth()+topPixel();
  int y=y_;
  if (h>=remainingHeight)
   {
     _pageCount++;
     y=report_->bodyTop(report_->pageCount()+1);
   }
  if (y-h-bottomPixel()<report_->pageEnd()) h+=bottomPixel();
  _residual=h;
  return h;
}

int MSRulePrintItem::print(MSReport *report_,int x_,int y_,int w_,int,int,int leftMargin_)
{
  int remainingHeight=y_-report_->pageEnd();
  int h=ruleWidth()+topPixel();
  if (h>=remainingHeight)
   {
     pageBreakRow(0);
    _currentPage++;
     return remainingHeight;
   }
  w_-=(leftMargin_==0?leftPixel()+rightPixel():0);
  int x=x_+(leftMargin_==0?leftPixel():leftMargin_);
  int y=(int)(y_-ruleWidth()/2.0);
  printRulePrintItem(report_,x,y,w_);
  if (y_-h-bottomPixel()<report_->pageEnd()) h+=bottomPixel();
  _currentPage++;
  return h;
}

void MSRulePrintItem::printRulePrintItem(MSReport *report_,int x_,int y_,int w_)
{
  report_->gcValues().line_width=ruleWidth();
//  report_->gcValues().foreground=report_->lineColor();   // check grl
//  report_->gcValues().line_style=report_->lineStyle();   // check grl
  report_->fgGrayScale(fgGrayScale());
  report_->setAttributes();
  report_->setFgGrayScale();
  report_->printLine(x_,y_,x_+w_,y_);
}

