/////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <MSGUI/MSGraph.H>
#include <MSGUI/MSReport.H>
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSReportTable.H>
#include <MSGUI/MSPrintFontData.H>
#include <MSTypes/MSMessageLog.H>


const char       *MSDefaultReportFileName ="report.ps";
extern const int  MSDegPerRadian          =(int)(360/(M_PI*2));
static const int  MSClipMargin            =10;
static MSBoolean  FileOpenStatus          =MSFalse;

extern const int MSPointsPerInch;
extern const int MSPageSizeXTable[];
extern const int MSPageSizeYTable[];

MSSymbol MSReport::pagebreak("pagebreak");
MSSymbol MSReport::computesize("computesize");

class MSReportCallback : public MSCallback
{
private:
   MSReport                 *_report;
   MSReportCallbackFunc      _function;
   void                     *_clientData;
public:
  MSReportCallback(MSReport *report_,MSReportCallbackFunc func_,void *clientData_=0):
  _report(report_),_function(func_),_clientData(clientData_) {}
   virtual void process(void);
};

void MSReportCallback::process(void)
{_function(_report,_clientData);}

MSReport::MSReport(void)
{
  reset();
}

MSReport::~MSReport(void)
{
  deleteCallbackList();
}

void MSReport::reset(void) 
{
  removeAllHeaders();
  removeAllFooters();
  fileName(MSDefaultReportFileName);
  _uniformScaling=MSTrue;
  _pageFrameStyle=MSNone;
  _headerOffset=2;
  _footerOffset=2;
  _pageNumberOffset=2;
  _pageFrameOffset=2;
  _pageFrameLineWidth=0;
  _psFontSize=0;
  _callbackListHead=0;
  _pageNumbering=MSTrue;
  _pageNumIncludeStatus=MSFalse;
  _cancelReport=MSFalse;
  _banner.font("Times-Roman-72");
  _banner.justification(MSCenter);
  _banner.xOrigin(1);
  _banner.yOrigin(1);
  _banner.fgGrayScale(0.95);
  _banner.occurrence(Diagonal|EveryPage);
  _printHeaderStatus=MSFalse;        // check grl
  _asciiFloatFormat=0;
  _asciiIntFormat=0;
  pageOrientation(Portrait);
  init();
}

void MSReport::init(void)
{
  _defaultFontID=0;
  _defaultFontSize=10;
  _pageCount=1;
  _pageCountTotal=0;
  _yPixel=topPixel();
  _conditionalPageSize=MSFalse;
  headerHeights().removeAll();
  footerHeights().removeAll();
}

void MSReport::postScriptStackInit(void)
{
  fontID(INT_MAX);
  fgColor(LONG_MAX);
  bgColor(LONG_MAX);
  gscale(1);
  lineStyle(INT_MAX);
  lineWidth(-1);
}

void MSReport::defaultFont(const MSString& aString_)
{
  if (aString_.length()>0)
   {
     _defaultFont=aString_;
     defaultFontID(font(aString_));
     defaultFontSize(fontSize());
   }
}

////////////////////////////////////////////////////////////////////////////
//
// general print methods
//
////////////////////////////////////////////////////////////////////////////

MSBoolean MSReport::reportOpen(void)
{
  MSBoolean status=MSFalse;
  _cancelReport=MSFalse;
  init();
  if(outputMode()==ASCII)
   {
     MSString file(fileName());
     file.change(".ps",".txt",0,1);
     pout.open(file);
     if (pout.fail()==ios::failbit)
      {
        MSMessageLog::errorMessage("Error: opening file %s check file permissions\n",file.string());
      }
     else status = MSTrue;
   }
  else
   {
     initFont();
     if (fontCount()==0)
      {
        MSMessageLog::errorMessage("Error: no fonts allocated - unable to generate report\n");
      }
     else if ((status=printOpen())==MSTrue)
      {
        if (disclaimer().style()>NoDisclaimer) disclaimer().computeSize();
	computePrintScale();
      }
   }
  return status;
}

MSBoolean MSReport::reportClose(void) 
{
  MSBoolean status=MSTrue;
  if (outputMode()==ASCII) pout.close();
  else status=MSPostScript::printClose();
  return status;
}

void MSReport::printProlog(void) 
{
  MSPostScript::printProlog();
}

void MSReport::printEpilog(void) 
{
  pout<< "%%Trailer" <<endl;
  if (cancelReportStatus()==MSTrue) pout<< "%%Report generation terminated at "<<pageCount()<<endl;
  if (outputMode()==EPS) pout<<"end"<<endl<<"%%EOF" <<endl;
}

void MSReport::printSetup(void)
{MSPostScript::printSetup();}

int MSReport::bodyTop(void)
{return bodyTop(pageCount());}

int MSReport::bodyTop(int pageCount_)
{
  while(headerHeights().length()<pageCount_)
   {
     computePageHeaderSize(headerHeights().length()+1);
   }
  return bodyTopBase()-headerOffset()-headerHeights()(pageCount_-1);
}

int MSReport::bodyBottom(void)
{return bodyBottom(pageCount());}

int MSReport::bodyBottom(int pageCount_)
{
  while(footerHeights().length()<pageCount_)
   {
     computePageFooterSize(footerHeights().length()+1);
   }
  return bodyBottomBase()+footerOffset()+footerHeights()(pageCount_-1);
}

void MSReport::computePrintScale(void)
{
  int leftOffset=leftPixel();
  int bottomOffset=bottomPixel();
  int pageWidth,pageHeight;
  
  if (disclaimer().style()!=NoDisclaimer)
   {
     PageOrientation orient=disclaimer().orientation()==1?pageOrientation():disclaimer().orientation();
     int h=disclaimer().height()+disclaimer().topPixel()+disclaimer().bottomPixel();
     if (pageOrientation()==Landscape)
      {
	if (orient==Landscape) bottomOffset=h;
	else leftOffset=h;
      }
     else bottomOffset=h;
   }
  if (pageOrientation()==Landscape)
   {
     pageWidth=MSPageSizeYTable[pageSize()-Letter];
     pageHeight=MSPageSizeXTable[pageSize()-Letter];
     printArea().width(pageWidth-leftOffset-rightPixel());
     printArea().height(pageHeight-topPixel()-bottomOffset);
   }
  else 
   {
     pageWidth=MSPageSizeXTable[pageSize()-Letter];
     pageHeight=MSPageSizeYTable[pageSize()-Letter];
     printArea().width(pageWidth-leftOffset-rightPixel());
     printArea().height(pageHeight-topPixel()-bottomOffset);
   }
  printArea().x(leftOffset);
  printArea().y(bottomOffset+printArea().height());
  x_end(pageWidth);
  y_end(printArea().y()-printArea().height());
  x_org(printArea().x());
  y_org(printArea().y());
  pageEnd(y_end());
}

void MSReport::computePageSize() 
{
  int pageNumberTop=0,pageNumberBottom=0;
  if (pageNumbering()==MSTrue&&pageNumIncludeStatus()!=MSTrue)
   {
     int h=pageNumber().computePrintSize(this,0,printArea().y(),x_end(),0,0,0);
     h+=pageNumberOffset();
     if (pageNumber().text().length()==0) h+=pageNumber().fontSize();
     if (pageNumber().justification()&MSTop) pageNumberTop=h;
     else pageNumberBottom=h;
   }
  int pageFrameThickness=pageFrameLineWidth()+pageFrameOffset();
  int leftOffset=(pageFrameStyle()&Box||pageFrameStyle()&BoxL?pageFrameThickness:0);
  int rightOffset=(pageFrameStyle()&Box||pageFrameStyle()&BoxR?pageFrameThickness:0);
  int topOffset=pageFrameOffset()+(pageFrameStyle()&Box||pageFrameStyle()&BoxT?pageFrameLineWidth():0);
  int bottomOffset=pageFrameOffset()+(pageFrameStyle()&Box||pageFrameStyle()&BoxB?pageFrameLineWidth():0);
  bodyTopBase(printArea().y()-topOffset-pageNumberTop);
  bodyBottomBase(y_end()+pageNumberBottom);
  bodyLeft(printArea().x()+leftOffset);
  bodyRight(printArea().x()+printArea().width()-rightOffset); 
}

void MSReport::startPage(void) 
{
  printPageStart();
//  setClipRectangle(leftPixel(),bottomPixel(),printArea().width(),printArea().height());
  postScriptStackInit();
  computePageNumber();
  printBanner();
  printPageHeaders();
  printPageFooters();
  printPageNumber();
  printPageFrame();
  printPageTranslate();
}

void MSReport::endPage(void) 
{
  disclaimer().print();
  disclaimer().printRule();
  pout<<"endpage"<<endl;
}

void MSReport::printPageFrame(void) 
{
  gcValues().line_width=pageFrameLineWidth();
//  gcValues().foreground=lineColor();
  gcValues().line_style=lineStyle();
  fgGrayScale(0);
  setFgGrayScale();
//   for diagnostics
//   pout<<"255 0 0 tc"<<endl;
//   strokeRectangle(leftPixel(),bottomPixel(),printArea().width(),-printArea().height());
//   pout<<"0 sg"<<endl;

  double x=printArea().x();
  double y=bodyTop(pageCount());
  double w=printArea().width();
  double h=y-bodyBottom(pageCount());
  int offset=pageFrameLineWidth()+pageFrameOffset();
//   if (pageFrameStyle()&Box) 
//    {
//      y+=offset; h+=offset;
//      strokeRectangle(x,y,w,h+offset);
//    }
//   else
//    {
//      setAttributes();
//      if (BoxL&pageFrameStyle()) printLine(x,y,x,y-h);
//      if (BoxR&pageFrameStyle()) printLine(x+w,y,x+w,y-h);
//      if (BoxT&pageFrameStyle()) printLine(x,y+offset,x+w,y+offset);
//      if (BoxB&pageFrameStyle()) printLine(x,y-h-2*offset,x+w,y-h-2*offset);
//    }
  printBox(pageFrameStyle(),x,y,w+2*offset,h+2*offset);
}

void MSReport::printBox(unsigned long style_,double x_,double y_,double w_,double h_)
{
  if (style_&Box) 
   {
     strokeRectangle(x_,y_,w_,h_);
   }
  else
   {
     setAttributes();
     if (BoxL&style_) printLine(x_,y_,x_,y_-h_);
     if (BoxR&style_) printLine(x_+w_,y_,x_+w_,y_-h_);
     if (BoxT&style_) printLine(x_,y_,x_+w_,y_);
     if (BoxB&style_) printLine(x_,y_-h_,x_+w_,y_-h_);
   }
}

void MSReport::updateForeground(int) 
{
  MSPostScript::updateForeground();
}

void MSReport::updateBackground(void) 
{
  MSPostScript::updateBackground();
}

void MSReport::updateFont(void) 
{
  if (gcValues().font!=printFontIDHashTable().notFound())
   {
     fontID(gcValues().font);
     psFontSize(fontSize());
     fontString((char *)fontName(fontID()));
   }
}     

void MSReport::setFont(void)
{
  if (fontID()!=gcValues().font||fontSize()!=psFontSize())
   {
     updateFont();
     pout<< fontSize();
     pout<< "/";
     pout<< fontString();
     pout<< " ";
     pout<< "font";
     pout<< endl;
   }
}

void MSReport::setFgGrayScale(void)
{
  switch(printMode())
   {
   case Mono:
     if (gscale()!=fgGrayScale())
      {
	gscale(fgGrayScale());
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	pout<< " ";
      }
     break;
	
   case Reverse:
     if (gscale()!=bgGrayScale())
      {
	gscale(bgGrayScale());
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	pout<< " ";
      }
     break;
   }
}

void MSReport::setBgGrayScale(void)
{
  switch(printMode())
   {
   case Mono:
     if (gscale()!=bgGrayScale())
      {
	gscale(bgGrayScale());
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	pout<< " ";
      }
     break;
	
   case Reverse:
     if (gscale()!=fgGrayScale())
      {
	gscale(fgGrayScale());
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	pout<< " ";
      }
     break;
   }
}

unsigned long MSReport::formatStyle(unsigned long style_)
{
  if ((style_&MSLeft)&&(style_&MSRight)) style_-=MSRight;
  if ((style_&MSTop)&&(style_&MSBottom)) style_-=MSBottom;
  if ((style_&Underline)&&(style_&DUnderline)) style_-=Underline;
  return style_;
}

MSBoolean MSReport::printOnPage(MSPrintItem& item_,int pageCount_,int pageCountTotal_)
{
  MSBoolean ret=MSFalse;
  if (item_.occurrence()&EveryPage) ret=MSTrue;
  else if (item_.occurrence()&OddPage&&(pageCount_)%2==0) ret=MSFalse;
  else if (item_.occurrence()&EvenPage&&(pageCount_)%2==1) ret=MSFalse;
  else if ((item_.occurrence()&IfNextPage&&pageCount_<pageCountTotal_)||
           (item_.occurrence()&FirstPage&&pageCount_==1)|| 
           (item_.occurrence()&EvenPage&&(pageCount_)%2==0)||
           (item_.occurrence()&OddPage&&(pageCount_)%2==1)||
           (item_.occurrence()&AnyButFirstAndLast&&(pageCount_>1&&pageCount_<pageCountTotal_))||
           (item_.occurrence()&LastPage&&pageCountTotal_>1&&pageCount_==pageCountTotal_)
    ) ret=MSTrue;
  return ret;
}

// general print methods

void MSReport::strokeRectangle(double x_,double y_,double w_,double h_)
{
  setAttributes();
  setFgGrayScale();
  printRectangle(x_,y_,w_,h_);
  pout<< "st";
  pout<< endl;
}

void MSReport::fillRectangle(double x_,double y_,double w_,double h_) 
{
  setAttributes();
  setBgGrayScale();
  printRectangle(x_,y_,w_,h_);
  pout<< "f";
  pout<< endl;
}

void MSReport::setClipRectangle(int x_,int y_,int w_,int h_)
{
  pout<< x_-MSClipMargin;
  pout<< " ";
  pout<< y_+MSClipMargin;
  pout<< " ";
  pout<< "M";
  pout<< " ";
  pout<< -w_+2*MSClipMargin;
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< h_+2*MSClipMargin;
  pout<< " ";
  pout<< w_+2*MSClipMargin;
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< "3";
  pout<< " ";
  pout<< "D";
  pout<< " ";
  pout<< "clip";
  pout<< " ";
  pout<< "n";
  pout<< endl;
}

void MSReport::printPageStart(void) 
{
  pout<<"%%Page: "<<pageCount()<<endl;
  pout<<"beginpage "<<endl;
  if (pageOrientation()==Landscape)
   {
     pout<< MSPageSizeXTable[pageSize()-Letter];
     pout<<" ";
     pout<<"0";
     pout<<" ";
     pout<<"translate";
     pout<<" 90 rotate";
     pout<< endl;
   }
}

void MSReport::printPageTranslate(void)
{
  if (x_printScale()!=1||y_printScale()!=1)
   {
     int top=bodyTop(pageCount());
     pout<<x_printScale();
     pout<<" ";
     pout<<y_printScale();
     pout<<" ";
     pout<<"scale";
     pout<<" ";
     pout<<bodyLeft()/x_printScale()-bodyLeft();
     pout<<" ";
     pout<<top/y_printScale()-top;
     pout<<" ";
     pout<<"translate";
     pout<<" ";
     pout<< endl;
   }
}

void MSReport::printReportString(unsigned long m_,int x_,int y_,const char *str_,int n_) 
{
  if (n_!=0)
   {
     if (Outline&m_)
      {
	gcValues().line_width=0;
	setAttributes();
      }
     setFgGrayScale();
     setFont();
     MSString aString(str_,n_);
     aString.change("\\","\\\\").change("(","\\(").change(")","\\)");
  
     if (aString.length()>0)
      {
	pout<< x_;
	pout<< " ";
	pout<< y_;
	pout<< " ";
	pout<< "M";
	pout<< " ";
	pout<< "(";
	pout<< aString;
	pout<< ")";
	pout<< " ";
	pout<< (Outline&m_?"sh st":"S");
	pout<< endl;
      } 
   }
}

////////////////////////////////////////////////////////////////////////////
//
// header/footer methods
//
////////////////////////////////////////////////////////////////////////////

void MSReport::computePageHeaderSize(int pageCount_) 
{
  double height=0;
  for (unsigned i=0,n=headerList().count();i<n;i++)
   {
     if (printOnPage(*header(i),pageCount_,pageCountTotal())==MSTrue)
      {
        if (header(i)->printFont().length()==0) header(i)->printFont(defaultFont());
        double h=header(i)->computePrintSize(this,0,y_org(),x_end(),0,0,0);
        if (y_org()-height-h<y_end())
         {
           removeHeader(header(i));
           MSMessageLog::errorMessage("Error: MSReport Header height exceeds page height\n");
           // force recomputation of all header heights
           headerHeights().removeAll();
           return;
         }
        else height+=h;
      }
     else conditionalPageSize(MSTrue);
   }
  headerHeights()<<int(height);
}

void MSReport::computePageFooterSize(int pageCount_) 
{
  double height=0;
  for (unsigned i=0,n=footerList().count();i<n;i++)
   {
     if (printOnPage(*footer(i),pageCount_,pageCountTotal())==MSTrue)
      {
        // just in case this method is called before the headers method
        int hh=pageCount()<headerHeights().length()?headerHeights()(pageCount()-1):0;
        if (footer(i)->printFont().length()==0) footer(i)->printFont(defaultFont());
        double h=footer(i)->computePrintSize(this,0,y_org(),x_end(),0,0,0);
        if (y_org()-hh-height-h<bodyBottomBase())
         {
           removeFooter(footer(i));
           MSMessageLog::errorMessage("Error: MSReport Header and Footer heights exceed page height\n");
           // force recomputation of all footer heights
           footerHeights().removeAll();
           return;
         }
        else height+=h;
      }
     else conditionalPageSize(MSTrue);
   }
  footerHeights()<<int(height);
}

void MSReport::printPageHeaders(void) 
{
  int y=bodyTopBase();
  pageNumIncludeStatus(MSFalse);
  
  for (unsigned i=0;i<headerList().count();i++)
   {
     if (printOnPage(*header(i),pageCount(),pageCountTotal())==MSTrue)
      {
        if (pageNumIncludeStatus()==MSFalse) printHeaderStatus(MSTrue);
        y-=header(i)->print(this,0,y,x_end(),0,0,0);
        header(i)->computePrintSize(this,0,y,x_end(),0,0,0);
      }
   }
  printHeaderStatus(MSFalse);
}

void MSReport::printPageFooters(void)
{
  int y=bodyBottom(pageCount())-footerOffset()-pageFrameLineWidth();
  int bottom=pageEnd();
  pageEnd(bodyBottomBase());
  for (unsigned i=0;i<footerList().count();i++)
   {
     if (printOnPage(*footer(i),pageCount(),pageCountTotal())==MSTrue)
      {
        if (pageNumIncludeStatus()==MSFalse) printHeaderStatus(MSTrue);
        y-=footer(i)->print(this,0,y,x_end(),0,0,0);
        footer(i)->computePrintSize(this,0,y,x_end(),0,0,0);
      }
   }
  printHeaderStatus(MSFalse);
  pageEnd(bottom);
}


////////////////////////////////////////////////////////////////////////////
//
// banner methods
//
////////////////////////////////////////////////////////////////////////////

MSParagraph& MSReport::banner(const MSParagraph& banner_)
{
  _banner=banner_;
  return _banner;
}
MSParagraph& MSReport::banner(const MSStringVector& banner_)
{_banner=banner_; return banner(_banner);}
MSParagraph& MSReport::banner(const char *banner_)
{_banner=banner_; return banner(_banner);}

void MSReport::computeBannerExtents(void)
{
  bannerAngle(0);
  bannerWidth(0);
  banner().fontID(font(banner().fontName()));
  banner().fontSize(fontSize());
  if (printArea().width()>0&&banner().text().length()>0)
   {
     unsigned offset=pageSize()-Letter;
     double w=pageOrientation()==Portrait?MSPageSizeXTable[offset]:MSPageSizeYTable[offset];
     double h=pageOrientation()==Portrait?MSPageSizeYTable[offset]:MSPageSizeXTable[offset];

     double maxWidth=0,len=w;
     int x1=banner().xPixel();
     int y1=banner().yPixel();
     MSPrintFontData *fdata=fontStruct(banner().fontID());
     unsigned length=banner().text().length();
     
     for (unsigned i=0;i<length;i++)
      {
	const char *cp=banner().text()[i];
	int tw=(int)fdata->textWidth(banner().fontSize(),cp,strlen(cp));
	maxWidth=tw>maxWidth?tw:maxWidth;
      }
     if (banner().occurrence()&Diagonal)
      {
	int x2=(int)w-x1;
	int y2=(int)h-y1;
	double a=x2-x1;
	double b=y2-y1;
	len=sqrt(a*a+b*b);
	bannerAngle(MSDegPerRadian*atan(b/a));
      }
     bannerWidth(len);
     if (maxWidth>len) banner().fontScale(len/maxWidth);
   }
}

void MSReport::printBanner(void)
{
  if (banner().text().length()>0&&printOnPage(banner(),pageCount(),pageCountTotal())==MSTrue)
   {
     computeBannerExtents();
     MSPrintFontData *fdata=fontStruct(banner().fontID());
     gcValues().font=banner().fontID()!=0?banner().fontID():1;
     fgGrayScale(banner().fgGrayScale());
     bgGrayScale(banner().bgGrayScale());
     fontSize(banner().fontSize());
     pout<<"gs";
     pout<<" ";
     if (Outline&banner().style())
      {
	gcValues().line_width=0;
	setAttributes();
      }
     setFgGrayScale();
     setFont();
     pout<<banner().xPixel();
     pout<<" ";
     pout<<banner().yPixel();
     pout<<" ";
     pout<<"translate";
     pout<<" ";
     if (bannerAngle()!=0)
      {
	pout<<bannerAngle();
	pout<<" ";
	pout<<"rotate";
	pout<<" ";
      }
     if (banner().fontScale()<1)
      {
	pout<<banner().fontScale();
	pout<<" ";
	pout<<"1";
	pout<<" ";
	pout<<"scale";
	pout<<" ";
      }
     int len=banner().text().length();
     int y=(int)(len*fontSize()/2-fdata->fontOffset(fontSize()));
     for (unsigned i=0;i<len;i++)
      {
	const char *cp=banner().text()[i];
	int tw=(int)fdata->textWidth(fontSize(),cp,strlen(cp));
	int x=(int)(MSLeft&banner().justification()?0:MSRight&banner().justification()?bannerWidth():((bannerWidth()-tw)/2));
	MSString aString(cp);
	aString.change("\\","\\\\").change("(","\\(").change(")","\\)");
	if (aString.length()>0)
	 {
	   pout<< x;
	   pout<< " ";
	   pout<< y;
	   pout<< " ";
	   pout<< "M";
	   pout<< " ";
	   pout<< "(";
	   pout<< aString;
	   pout<< ")";
	   pout<< " ";
	   pout<< (Outline&banner().style()?"sh st":"S");
	   pout<< endl;
	 } 
	y-=fontSize()-banner().leading();
      }
     pout<<"gr"<<endl;
   }
}

////////////////////////////////////////////////////////////////////////////
//
// page number methods
//
////////////////////////////////////////////////////////////////////////////

MSBoolean MSReport::insertPageNumString(MSStringVector& aStringVector_) 
{
  MSBoolean status=MSFalse;
  
  for (unsigned i=0;i<aStringVector_.length();i++)
   {
     if (aStringVector_(i).indexOf("%p")<aStringVector_(i).length()||
         aStringVector_(i).indexOf("%t")<aStringVector_(i).length())
      {
        int total=pageCountTotal();
        aStringVector_[i].change("%p",pageNumberString());
        if (pageNumbers().length()==1) total+=pageNumbers()(0)-1;
        aStringVector_[i].change("%t",MSString(total));
        status=MSTrue;
      }
   }
  if (status==MSTrue) pageNumIncludeStatus(MSTrue);
  return status;
}

void MSReport::computePageNumber(void) 
{
  int actual=pageCount()-1;
  int pg=actual;
  if (pageNumbers().length()>0)
   {
     if (pg<pageNumbers().length()) pg=pageNumbers()(actual);
     else pg+=pageNumbers().lastElement();
   }
  else
   {
     actual++;
     pg++;
   }
  if (actual<pageNumberText().length()&&actual>0)
   {
     MSStringVector text(pageNumberText()(actual-1));
     int adjustedCount=pg>actual?pg-actual:pg;
     _pageNumberString=MSString(adjustedCount);
     insertPageNumString(text);
     _pageNumberString=text.asString();
     // _pageNumberString=pageNumberText()(pg);
     return;
   }
  else _pageNumberString=MSString(pg);
}

void MSReport::printPageNumber(void) 
{
  if (pageNumbering()==MSTrue&&pageNumIncludeStatus()!=MSTrue)
   {
     int h=pageNumber().residual()+pageNumberOffset();
     int bottom=pageEnd();
     pageEnd(y_end());
     if (pageNumber().text().length()==0) 
      {
        pageNumber().outputText()<<pageNumberString();
        h+=pageNumber().fontSize();
      }
     else
      {
        insertPageNumString(pageNumber().outputText());
        if (pageNumIncludeStatus()!=MSTrue) pageNumber().outputText().lastElement()<<pageNumberString();
      }
     int y=MSTop&pageNumber().justification()?printArea().y():y_end()+h;
     // to compensate for this being added in the print()
     // method which is not applicable for the pageNumber.
     y-=pageNumber().leading();
     pageNumber().print(this,0,y,x_end(),0,0,0);
     pageNumber().computePrintSize(this,0,y,x_end(),0,0,0);
     pageEnd(bottom);
   }
}

int MSReport::tabStop(int column_)
{
  double tab=0;
  
  if (tabStop().length()>0)
   {
     if (column_<tabStop().length())
      {
	tab=tabStop()(column_);
      }
   }
  return int(tab*MSPointsPerInch);
}

void MSReport::translate(double x_,double y_)
{
  pout<<x_;
  pout<<" ";
  pout<<y_;
  pout<<" ";
  pout<<"translate";
  pout<<" ";
  pout<< endl;
}

void MSReport::push(void)
{
  pout<<"gs";
  pout<<endl;
}

void MSReport::pop(void)
{
  pout<<"gr";
  pout<<endl;
}

////////////////////////////////////////////////////////////////////////////
//
// MSReport callback methods
//
////////////////////////////////////////////////////////////////////////////

void MSReport::callback(const MSSymbol& name_,MSReportCallbackFunc func_,void *clientData_)
{ callback(name_,new MSReportCallback(this,func_,clientData_)); }

void MSReport::callback(const MSSymbol& name_,MSCallback *callback_)
{
  MSReportCallbackNode *node;
  if ((node=callbackNode(name_))!=0) node->callback(callback_);
  else
   {
     node=new MSReportCallbackNode(name_,callback_);
     MSNodeItem *np=new MSNodeItem((void *)node);
     if (callbackListHead()==0) _callbackListHead=new MSNodeItem;
     np->insert(callbackListHead()->next());     
   }
}

MSBoolean MSReport::activateCallback(const MSSymbol& name_)
{
  MSReportCallbackNode *node=callbackNode(name_);
  if (node!=0)
   {
     MSCallback *pCallback=node->callback();
     if (pCallback!=0) pCallback->process();
     return MSTrue;
   }
  return MSFalse;
}

MSCallback *MSReport::callback(const MSSymbol& name_)
{
  MSReportCallbackNode *node=callbackNode(name_);
  return (node!=0)?node->callback():0;
}

MSReportCallbackNode *MSReport::callbackNode(const MSSymbol& name_)
{
  if (callbackListHead()!=0)
   {
     MSNodeItem     *hp=callbackListHead();
     MSNodeItem     *np=hp;
     MSReportCallbackNode *node;
     if (name_!=MSSymbol::nullSymbol())
      {
	while ((np=np->next())!=hp)
	 {
	   node=(MSReportCallbackNode *)np->data();
	   if (node->name()==name_) return node;
	 }
      }
   }
  return 0;
}

void MSReport::deleteCallbackList(void)
{
  if (callbackListHead()!=0)
   {
     MSNodeItem     *hp=callbackListHead();
     MSNodeItem     *np=hp;
     MSReportCallbackNode *node;
     
     while ((np=hp->next())!=hp)
      {
	node=(MSReportCallbackNode *)np->data();
	delete np;
	delete node;
      }
     delete callbackListHead();
   }
}

void MSReport::translateScale(double xScale_,double yScale_,int x_,int y_)
{
  
  if (xScale_!=1||yScale_!=1)
   {
     pout<<xScale_;
     pout<<" ";
     pout<<yScale_;
     pout<<" ";
     pout<<"scale";
     pout<<" ";
     pout<<x_/xScale_-x_;
     pout<<" ";
     pout<<y_/yScale_-y_;
     pout<<" ";
     pout<<"translate";
     pout<<" ";
     pout<< endl;
   }
}

void MSReport::computeOutputSize(void)
{
  pageCountTotal(INT_MAX);
  int lastResidual = 0;
  int lastPageCnt =0;
  //compute sizes in two passes when conditional headers exists
  for (unsigned p=0,passes=2;p<passes;p++)
   {
     int y=bodyTop(pageCount());
     pageEnd(bodyBottom(pageCount()));
     for (unsigned i=0;i<printItemList().count();i++)
      {
        MSPrintItem *pItem=item(i);
        // set the default font if not set for printItem
        if (pItem->printFont().length()==0) pItem->printFont(defaultFont());
        y-=pItem->computePrintSize(this,0,y,x_end(),0,0,0);
        _pageCount+=pItem->pageCount();
	lastPageCnt=pItem->pageCount();
	lastResidual=pItem->residual();
        if (pItem->pageCount()>0)
         {
           y=bodyTop(pageCount())-pItem->residual();
           pageEnd(bodyBottom(pageCount()));
         }
      }
     if (p==0&&conditionalPageSize()==MSTrue)
      {
        headerHeights().removeAll();
        footerHeights().removeAll();
	if(lastPageCnt!=0&&lastResidual==0) _pageCount--;
        pageCountTotal(pageCount());
        pageCount(1);
      }
     else passes=1;
   }
  if(lastPageCnt!=0&&lastResidual==0) _pageCount--;
  pageCountTotal(pageCount());
  pageCount(1);
  activateCallback(computesize);
  pages(pageCountTotal());
}

void MSReport::print(const char *file_)
{
  if (file_!=0) fileName(file_);
  if (reportOpen()==MSTrue&&FileOpenStatus==MSFalse)
   {
     FileOpenStatus=MSTrue;
     if (outputMode()==ASCII)
      {
       for (unsigned i=0;i<printItemList().count();i++)
	{
	  MSPrintItem *pItem=item(i);
	  pItem->print(this,0,0,0,0,0,0);
	}
      }
     else
      {
       MSBoolean endPageStatus=MSFalse;
       postScriptStackInit();
       computePageSize();
       computeOutputSize();
       printProlog();
       printSetup();
       startPage();
       int y=bodyTop(pageCount());
       headerHeights().removeAll();
       footerHeights().removeAll();
       pageEnd(bodyBottom(pageCount()));
       for (unsigned i=0;i<printItemList().count();i++)
	{
	 MSPrintItem *pItem=item(i);
	 y-=pItem->print(this,0,y,x_end(),0,0,0);
	 
	 if (pItem->currentPage()-1<pItem->pageCount())   
	  {
	   endPage();
           endPageStatus=MSTrue;
	   if (pageCount()<pageCountTotal())
	    {
	     if(pItem->residual()!=0||pItem->pageCount()-pItem->currentPage()>0) i--;
	     _pageCount++;
	     pageEnd(bodyBottom(pageCount()));
	     activateCallback(pagebreak);
	     startPage();
              endPageStatus=MSFalse;
	    }
	   y=bodyTop(pageCount());
	  }
	}
       if (endPageStatus==MSFalse) endPage();
      }
   }
  FileOpenStatus=MSFalse;
  reportClose();
}
