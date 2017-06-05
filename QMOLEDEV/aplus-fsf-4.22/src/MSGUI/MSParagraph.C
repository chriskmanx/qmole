///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSPrintFontData.H>
#include <MSGUI/MSReport.H>
#include <MSTypes/MSMessageLog.H>

extern const int  MSPointsPerInch;

MSParagraph::MSParagraph(const MSStringVector& text_,const MSSymbol& tag_):
_text(text_),_tag(tag_)
{init();}
MSParagraph::MSParagraph(const MSStringVector& text_,int row_,int col_,const MSSymbol& tag_):
_text(text_),_tag(tag_)
{
  init();
  row(row_);
  column(col_);
}
MSParagraph::MSParagraph(const MSStringVector& text_,const char *font_,const MSSymbol& tag_):
_text(text_),_tag(tag_)
{
  font(font_);
  init();
}
MSParagraph::MSParagraph(const MSStringVector& text_,int row_,int col_,const char *font_,const MSSymbol& tag_):
_text(text_),_tag(tag_)
{
  font(font_);
  init();
  row(row_);
  column(col_);
}

MSParagraph::MSParagraph(void)
{ init(); }

MSParagraph::MSParagraph(const MSParagraph& paragraph_) : MSPrintItem(paragraph_),
_foreground(paragraph_._foreground),
_background(paragraph_._background),
_orphanRows(paragraph_._orphanRows),
_widowRows(paragraph_._widowRows),
_style(paragraph_._style),
_fontID(paragraph_._fontID),
_fontSize(paragraph_._fontSize),
_leading(paragraph_._leading),
_xPixel(paragraph_._xPixel),
_yPixel(paragraph_._yPixel),
_lineWidth(paragraph_._lineWidth),
_text(paragraph_._text),
_tag(paragraph_._tag),
_fontScale(paragraph_._fontScale),
_fgGrayScale(paragraph_._fgGrayScale),
_bgGrayScale(paragraph_._bgGrayScale),
_firstLineIndentPixel(paragraph_._firstLineIndentPixel),
_lastLineWidth(paragraph_._lastLineWidth),
_textLineWidth(paragraph_._textLineWidth),
_wordSpacing(paragraph_._wordSpacing)
{}

MSParagraph& MSParagraph::operator=(const MSParagraph& paragraph_)
{
  if (&paragraph_!=this)
   {
     _foreground=paragraph_._foreground;
     _background=paragraph_._background;
     _orphanRows=paragraph_._orphanRows;
     _widowRows=paragraph_._widowRows;
     _style=paragraph_._style;
     _fontID=paragraph_._fontID;
     _fontSize=paragraph_._fontSize;
     _leading=paragraph_._leading;
     _xPixel=paragraph_._xPixel;
     _yPixel=paragraph_._yPixel;
     _lineWidth=paragraph_._lineWidth;
     _text=paragraph_._text;
     _tag=paragraph_._tag;
     _fontScale=paragraph_._fontScale;
     _fgGrayScale=paragraph_._fgGrayScale;
     _bgGrayScale=paragraph_._bgGrayScale;
     _firstLineIndentPixel=paragraph_._firstLineIndentPixel;
     _lastLineWidth=paragraph_._lastLineWidth;
     _textLineWidth=paragraph_._textLineWidth;
     _wordSpacing=paragraph_._wordSpacing;
     MSPrintItem::operator=(paragraph_);
   }
  return *this;
}

MSParagraph::~MSParagraph(void)
{}

void MSParagraph::operator=(const MSStringVector& text_)
{_text=text_;}
void MSParagraph::operator=(const MSString& text_)
{_text=text_;}
void MSParagraph::operator=(const char *text_)
{_text=text_;}
MSParagraph::operator const MSStringVector&() const
{return _text;} 

void MSParagraph::init(void)
{
  _foreground=0;
  _background=0;
  _style=0;
  _orphanRows=1;
  _widowRows=1;
  _fontID=0;
  _fontSize=10;
  _leading=1;
  _yPixel=MSPointsPerInch;
  _xPixel=MSPointsPerInch;
  _lineWidth=0;
  _fontScale=1;
  _fgGrayScale=0;
  _bgGrayScale=1;
  _firstLineIndentPixel=0;
  topPixel(1);
}

double MSParagraph::firstLineIndent(void) const
{return (double)_firstLineIndentPixel/MSPointsPerInch;}

double MSParagraph::leftMargin(void) const
{return MSPrintItem::leftMargin();}

double MSParagraph::rightMargin(void) const
{return MSPrintItem::rightMargin();}

MSParagraph& MSParagraph::firstLineIndent(double x_)
{_firstLineIndentPixel=int(MSPointsPerInch*x_); return *this;}

MSParagraph& MSParagraph::leftMargin(double x_)
{MSPrintItem::leftMargin(x_); return *this;}

MSParagraph& MSParagraph::rightMargin(double x_)
{MSPrintItem::rightMargin(x_); return *this;}

int MSParagraph::height(void) const
{return printHeight();}

MSParagraph& MSParagraph::text(const MSStringVector& x_)
{_text=x_; return *this;}
MSParagraph& MSParagraph::tag(const MSSymbol& x_)
{_tag=x_; return *this;}
MSParagraph& MSParagraph::justification(MSAlignment x_)
{MSPrintItem::justification(x_); return *this;}
MSParagraph& MSParagraph::justification(unsigned long x_)
{MSPrintItem::justification(x_); return *this;}
MSParagraph& MSParagraph::leading(unsigned x_)
{_leading=x_; return *this;}
MSParagraph& MSParagraph::topPixel(unsigned x_)
{MSPrintItem::topPixel(x_); return *this;}
MSParagraph& MSParagraph::bottomPixel(unsigned x_)
{MSPrintItem::bottomPixel(x_); return *this;}
MSParagraph& MSParagraph::lineWidth(unsigned x_)
{_lineWidth=x_; return *this;}
MSParagraph& MSParagraph::row(int x_)
{MSPrintItem::printRow(x_); return *this;}
MSParagraph& MSParagraph::column(int x_)
{MSPrintItem::printColumn(x_); return *this;}
MSParagraph& MSParagraph::printRow(int x_)
{MSPrintItem::printRow(x_); return *this;}
MSParagraph& MSParagraph::printColumn(int x_)
{MSPrintItem::printColumn(x_); return *this;}
MSParagraph& MSParagraph::firstColumn(unsigned x_)
{MSPrintItem::printColumn(x_); return *this;}
MSParagraph& MSParagraph::columnSpan(unsigned x_)
{_columnSpan=x_; return *this;}
MSParagraph& MSParagraph::fgGrayScale(double x_)
{if (x_>=0.0&&x_<=1.0) _fgGrayScale=x_; return *this;}
MSParagraph& MSParagraph::bgGrayScale(double x_)
{if (x_>=0.0&&x_<=1.0) _bgGrayScale=x_; return *this;}
MSParagraph& MSParagraph::xOrigin(double x_)
{_xPixel=int(x_*MSPointsPerInch); return *this;}
MSParagraph& MSParagraph::yOrigin(double x_)
{_yPixel=int(x_*MSPointsPerInch); return *this;}
MSParagraph& MSParagraph::topOffset(double x_)
{MSPrintItem::topOffset(x_); return *this;}
MSParagraph& MSParagraph::bottomOffset(double x_)
{MSPrintItem::bottomOffset(x_); return *this;}
MSParagraph& MSParagraph::foreground(const char *x_)
{_foreground=x_; return *this;}
MSParagraph& MSParagraph::background(const char *x_)
{_background=x_; return *this;}
MSParagraph& MSParagraph::fontSize(unsigned x_)
{_fontSize=x_; return *this;}
MSParagraph& MSParagraph::fontScale(double x_)
{_fontScale=x_; return *this;}
MSParagraph& MSParagraph::fontID(Font x_)
{_fontID=x_; return *this;}                           
MSParagraph& MSParagraph::occurrence(unsigned long x_)
{MSPrintItem::occurrence(x_); return *this;}
MSParagraph& MSParagraph::occurrence(Occurrence x_)
{MSPrintItem::occurrence(x_); return *this;}
MSParagraph& MSParagraph::pageAlignment(unsigned long x_)
{MSPrintItem::pageAlignment(x_); return *this;}
MSParagraph& MSParagraph::pageAlignment(MSAlignment x_)
{return pageAlignment((unsigned long)x_);}
MSParagraph& MSParagraph::font(const char *string_)
{MSPrintItem::printFont(string_); return *this;}
MSParagraph& MSParagraph::orphanRows(unsigned x_)
{_orphanRows=x_; return *this;}
MSParagraph& MSParagraph::widowRows(unsigned x_)
{_widowRows=x_; return *this;}

MSParagraph& MSParagraph::style(unsigned long style_)
{
  unsigned long temp=style_&(MSLeft|MSRight|MSTop|MSBottom|MSCenter);
  if (temp>0) justification(temp);
  _style=style_^temp;
  return *this;
}

double MSParagraph::xOrigin(void) const
{return double(_xPixel/MSPointsPerInch);}
double MSParagraph::yOrigin(void) const
{return double(_yPixel/MSPointsPerInch);}
double MSParagraph::topOffset(void) const
{return MSPrintItem::topOffset();}
double MSParagraph::bottomOffset(void) const
{return MSPrintItem::bottomOffset();}

const MSSymbol& MSParagraph::printTag(void) const
{return tag();}

int MSParagraph::computePrintSize(MSReport *report_,int,int y_,int w_,int,int topMargins_,int margins_)
{
  reset();
  if (leftPixel()<0) leftPixel(report_->leftPixel());
  if (rightPixel()<0) rightPixel(report_->rightPixel());
  fontID(report_->font(fontName()));
  fontSize(report_->fontSize());
  margins_=(margins_==0?leftPixel()+rightPixel():margins_);
  textLineWidth(w_-margins_);
  computeParagraphSize(report_);
  int rowHeight=fontSize()+leading();
  if (rowHeight<=0) return 0;
  int h=rowHeight*outputText().length()+topPixel();
  int border=0;
  if(style()&MSP::Box) border=2*(lineWidth());
  else border= (style()&MSP::BoxT?lineWidth():0)+(style()&MSP::BoxB?lineWidth():0);
  int remainingHeight=y_-report_->pageEnd()-topMargins_;
  int orphanRowHeight=rowHeight*orphanRows()-leading()+topPixel() +border;
  int totalHeight=h+border;

  //Skip to next page if there is no more space left on this page,
  //OR if we are not at the beginning of the page AND either we need to be aligned
  //with top of the page or there is not enough space for our orphanRows.
  if (remainingHeight<=0 ||
      (((pageAlignment()&MSTop) || orphanRowHeight>remainingHeight) &&
       (y_!=report_->bodyTop(report_->pageCount()))))
   {
     _pageCount++;
     int pc=report_->pageCount()+1;
     remainingHeight=report_->bodyTop(pc)-report_->bodyBottom(pc)-topMargins_;
   }
  if (h+border-leading()<remainingHeight)
   {
     if	(pageAlignment()&MSCenter||pageAlignment()&MSBottom)
      {
       _pageCount++;
       _residual=0;
       totalHeight=remainingHeight;
      }
     else
      {
       _residual = h+border;
       if (remainingHeight-residual()+leading()-bottomPixel()<0) 
	{
	 _pageCount++;
	 _residual=0;
	}
       else
	{
	 _residual+=bottomPixel();
	 totalHeight +=bottomPixel();
	}
      }
     printHeight(totalHeight);
     return residual();
   }

  //TODO: optimize this. The loop is not really necessary and we
  // could use devision and mod logic here.
  int hh=border+topPixel();
  for (unsigned i=0;i<outputText().length();i++)
   {
     hh+=rowHeight;
     if (hh-leading()>remainingHeight)
      {
        _pageCount++;
        int pc=report_->pageCount()+pageCount();
        remainingHeight=report_->bodyTop(pc)-report_->bodyBottom(pc)-topMargins_;
        hh=border+(i==0?topPixel():0);
        i--;
      }
     _residual=hh;
   }
//  if(_residual-=leading();
  if (remainingHeight-residual()+leading()-bottomPixel()<0) 
   {
     _pageCount++;
     _residual=0;
   }
  else
   {
     _residual+=bottomPixel();
     totalHeight+=bottomPixel();
   }
  if(_residual<0) _residual=0;
  printHeight(totalHeight);
  return residual();
}

void MSParagraph::computeParagraphSize(MSReport* report_)
{
  double w,width=firstLineIndentPixel();
  int    lw=(lineWidth()+1)*((style()&BoxL&&style()&BoxR)||style()&Box?2:style()&BoxL||style()&BoxR?1:0);
  double tlw=textLineWidth()-lw;

  wordSpacing().removeAll();
  outputText().removeAll();
  pageBreakRow(-1);

  MSPrintFontData *fdata=report_->fontStruct(fontID());
  if (fdata!=0)
   {
     double spaceCharWidth;
     double fontSpaceCharWidth=fdata->textWidth(fontSize()," ",1);
     double minSpaceCharWidth=fontSpaceCharWidth/2;
     for (unsigned i=0;i<text().length();i++)
      {
        unsigned numWords=text()(i).numWords();
	unsigned j,wordCount;
        if (justification()&MSLeft&&justification()&MSRight)
         {
           for (j=0,wordCount=0;j<numWords;j++,wordCount++)
            {
              MSString aString(text()(i).word(j));
              double spaceWidth=fontSpaceCharWidth*wordCount;
              spaceCharWidth=fontSpaceCharWidth;
              w=fdata->textWidth(fontSize(),aString,aString.length());
              if ((width+=w)>tlw-spaceWidth)
               {
                 if (w>tlw)
                  {
                    double currentWidth=width-w+minSpaceCharWidth*wordCount;
                    int row=outputText().length()-1;
                    width=dissectWord(report_,aString,(int)(tlw),currentWidth);
                    if (wordCount>0)
                     {
                       double rowWidth=fdata->textWidth(fontSize(),outputText()(row),outputText()(row).length());
                       rowWidth-=fontSpaceCharWidth*wordCount;
                       wordSpacing()<<(tlw-rowWidth)/wordCount;
                     }
                    else wordSpacing()<<minSpaceCharWidth;
                    outputText().lastElement()<<" ";
                    while(outputText().length()-1>wordSpacing().length())
                     {
                       wordSpacing()<<fontSpaceCharWidth;
                     }
                    wordCount=0;
                  }
                 else
                  {
                    spaceCharWidth=(tlw-width)/wordCount;
                    // word spacing too small, remove the last word
                    if (spaceCharWidth<minSpaceCharWidth)
                     {
                       spaceCharWidth=(tlw-width+w)/(wordCount-(wordCount>1?1:0));
                       wordSpacing()<<spaceCharWidth;
                       outputText().lastElement().stripTrailing();
                       width=w;wordCount=0;
                       outputText()<<(aString<<" ");
                     }
                    else outputText().lastElement()<<(aString<<" ");
                  }
               }
              else 
               {
                 if (j==0) outputText()<<(aString<<" ");
                 else outputText().lastElement()<<(aString<<" ");
               }
            }
           if (numWords==0)
            {
              if (j==0) outputText()<<"";
              else outputText().lastElement()<<"";
              width=wordCount=0;
            }
           wordSpacing()<<spaceCharWidth;
         }
        else
         {
           for (j=0;j<numWords;j++)
            {
              MSString aString(wordToNext(text()(i),j));
              w=fdata->textWidth(fontSize(),aString,aString.length());
              if ((width+=w)>tlw)
               {
                 MSString bString(MSString::stripTrailingBlanks(aString));
                 int ww=(int)(fdata->textWidth(fontSize(),bString,bString.length()));
                 if (width-w+ww<tlw)
                  {
                    if (j==0) outputText()<<bString; 
                    else outputText().lastElement()<<bString;
                    outputText()<<"";
                    width=w=0;
                  }
                 else if (ww>tlw)
                  {
                    w=dissectWord(report_,aString,(int)(tlw),width-w);
                  }
                 else
                  {
                    outputText().lastElement().stripTrailing();
                    outputText()<<aString;
                  }
                 width=w;
               }
              else
               {
                 if (j==0) outputText()<<aString; 
                 else outputText().lastElement()<<aString;
               }
            }
           if (numWords==0)
            {
              if (j==0) outputText()<<"";
              else outputText().lastElement()<<"";
              width=0;
            }
         }
        width=0;
      }
     lastLineWidth((int)(width));
   }
  else MSMessageLog::errorMessage("Error: font data not available - unable to print paragraph\n");
}

int MSParagraph::print(MSReport *report_,int x_,int y_,int w_,int h_,int bottomIndent_,int leftMargin_)
{
  if(report_->outputMode()==ASCII)
   {
    report_->pout<<text()<<endl;
    return 0;
   }
  int y=y_;
  int rowHeight=fontSize()+leading();
  int border=0;
  int pageEnd=report_->pageEnd()+bottomIndent_;
  int remainingHeight=y_-pageEnd;
  int bottom=style()&Box||style()&BoxB?lineWidth():0;
  if(style()&MSP::Box) border=2*(lineWidth());
  else border= (style()&MSP::BoxT?lineWidth():0)+(style()&MSP::BoxB?lineWidth():0);
  int orphanRowHeight=rowHeight*orphanRows()-leading()+topPixel()+border;

  if (pageBreakRow()<0&&(remainingHeight<0 || ((orphanRowHeight>remainingHeight||pageAlignment()&MSTop)
                                               &&y_!=report_->bodyTop(report_->pageCount()))))
   {
     pageBreakRow(0);
     _currentPage++;
     return remainingHeight;
   }
  int h;
  if (pageBreakRow()>0)
   {
     int len=outputText().length()-pageBreakRow();
     h=len*rowHeight+border-leading();
     if(h<remainingHeight) h+=bottomPixel();
   }
  else
   {
     h=outputText().length()*rowHeight+topPixel()+bottomPixel()-leading()+border;
     if (h>0&&h<remainingHeight)
      {
        if (pageAlignment()&MSCenter)
         {
           y-=(remainingHeight-h)/2;
         }
        else if (pageAlignment()&MSBottom)
         {
           y=pageEnd+h;
         }
      }
   }
  w_-=(leftMargin_==0?leftPixel()+rightPixel():0);

  if(pageBreakRow()>=(int)outputText().length()) // are we done?
   {
    //hack. sometimes we could fit on previous page even if we didn't think so.
    //TODO: track down that 1 pixel difference.
    if(_currentPage-_pageCount==0) _currentPage++;
     return 0;
   }

  printParagraphDecorations(report_,x_+(leftMargin_==0?leftPixel():0),y,w_,(h_>0)?h_:h,pageEnd);
  printParagraph(report_,x_+(leftMargin_==0?leftPixel():leftMargin_),y,pageEnd+bottom);
  _currentPage++;
  return residual();
}

void MSParagraph::printParagraph(MSReport *report_,int x_,int y_, int pageEnd_)
{
  MSBoolean pageNumStatus=MSFalse;
  MSStringVector originalText;
  if (report_->printHeaderStatus()==MSTrue)
   {
     originalText=outputText();
     pageNumStatus=report_->insertPageNumString(outputText());
   }
  
  unsigned numWords;
  MSPrintFontData *fdata=report_->fontStruct(fontID());
  report_->fontSize(fontSize());
  report_->gcValues().font=fontID();
  int left=style()&Box||style()&BoxL?lineWidth()+1:0;
  int right=style()&Box||style()&BoxR?lineWidth()+1:0;
  int top=style()&Box||style()&BoxT?lineWidth()+1:0;
  
  report_->setFgGrayScale();
  report_->setFont();
  double uoffset=(double)(fdata->underlinePosition()*fontSize())/1000.0;
  double uthickness=(double)(fdata->underlineThichness()*fontSize())/1000.0;
  double y=y_-(pageBreakRow()<=0?topPixel():0);
  y-=fdata->fontOffset(fontSize())+top-leading();
  int rowOffset=(int)(fontSize()+leading()-fdata->fontOffset(fontSize()));
  if (y+leading()<=pageEnd_)
   {
     pageBreakRow(0);
     return;
   }
  int rows=outputText().length();
  for (unsigned i=pageBreakRow()<0?0:pageBreakRow();i<rows;i++)
   {
     MSString aString(outputText()(i));
     double w=fdata->textWidth(fontSize(),aString,aString.length());
     aString.change("\\","\\\\").change("(","\\(").change(")","\\)");
     if (aString.length()>0)
      {
        if (Outline&style())
         {
           report_->gcValues().line_width=0;
           report_->setAttributes();
         }
	if (justification()&MSLeft)
	 {
	   double x=x_+left+(i==0?firstLineIndentPixel():0);
	   report_->pout<<x;
	   report_->pout<<" ";
	   report_->pout<<y;
	   report_->pout<<" ";
	   report_->pout<<"M";
	   report_->pout<<" ";
	   if (justification()&MSRight)
	    {
	      double ws=wordSpacing()(i);
              w=-ws;
	      numWords=outputText()(i).numWords();
	      for (int j=0;j<numWords;j++)
	       {
		 report_->pout<<"(";
		 report_->pout<<aString.word(j);
		 report_->pout<<")";
		 report_->pout<<(Outline&style()?"sh st":"S");
		 report_->pout<<" ";
		 report_->pout<<ws;
		 report_->pout<<" ";
		 report_->pout<<"s";
                 if (j!=0&&j%4==0) report_->pout<<endl;
                 if (style()&Underline)
                  {
                    w+=fdata->textWidth(fontSize(),aString.word(j),aString.lengthOfWord(j))+ws;
                  }
	       }
	    }
	   else 
	    {
	      report_->pout<<"(";
	      report_->pout<<aString;
	      report_->pout<<")";
	      report_->pout<<"S";
	    }
	   report_->pout<<endl;
           if (style()&Underline)
            {
              report_->gcValues().line_width=(int)uthickness;
              report_->setAttributes();
              report_->printLine(x,y+uoffset,x+w,y+uoffset);
            }
	 }
	else
	 {
	   double x=x_+(justification()&MSRight?textLineWidth()-w-right:((textLineWidth()-w)/2));
	   report_->pout<<x;
	   report_->pout<<" ";
	   report_->pout<<y;
	   report_->pout<<" ";
	   report_->pout<<"M";
	   report_->pout<<" ";
	   report_->pout<<"(";
	   report_->pout<<aString;
	   report_->pout<<")";
	   report_->pout<<(Outline&style()?"sh st":"S");
	   report_->pout<<" ";
	   report_->pout<<endl;
           if (style()&Underline)
            {
              report_->gcValues().line_width=(int)uthickness;
              report_->setAttributes();
              report_->printLine(x,y+uoffset,x+w,y+uoffset);
            }
	 }
      }
     y-=fontSize()+leading();
     if (y+leading()<pageEnd_+rowOffset)
      {
        if (i+1<=rows) pageBreakRow(i+1);
        if (pageNumStatus==MSTrue) outputText()=originalText;
        return;
      }
   }
  pageBreakRow(rows);
  if (pageNumStatus==MSTrue) outputText()=originalText;
}

void MSParagraph::printParagraphDecorations(MSReport *report_,int x_,int y_,int w_,int h_, int pageEnd_)
{
  MSPrintFontData *fdata=report_->fontStruct(fontID());
  report_->fgGrayScale(fgGrayScale());
  report_->bgGrayScale(bgGrayScale());
  report_->gcValues().line_width=lineWidth();
  static int MinSize=4;
  if (y_-pageEnd_>MinSize)
   {
     int h=(y_-h_)<pageEnd_?y_-pageEnd_+2:h_;
     if (bgGrayScale()<1.) report_->fillRectangle(x_,y_,w_-1,h-1);
     if(Box&style()) report_->strokeRectangle(x_,y_,w_-1,h-1);
     else report_->printBox(style(),x_,y_,w_,h);
   }
}
  
MSString MSParagraph::wordToNext(const MSString& aString_,unsigned i_) const
{
  static const char whiteSpace[]="\t\n\v\f\r ";
  unsigned end=0,start=aString_.indexOfWord(i_);
  unsigned len=aString_.length();
  if (start<len) end=aString_.indexOfAnyOf(whiteSpace,start);
  unsigned next=end<len?aString_.indexOfAnyBut(whiteSpace,end):end;
  if (next>start)
   {
     start=i_==0?0:start;
     return aString_.subString(start,next-start);
   }
  else return aString_.word(i_);
}

double MSParagraph::dissectWord(MSReport *report_,MSString& word_,int lineWidth_,double width_)
{
  MSPrintFontData *fdata=report_->fontStruct(fontID());
  MSBoolean errorMessage=MSFalse;
  double w,maxWidth=lineWidth_;
  int count=0;
  maxWidth-=width_;
  while(count<word_.length()&&fontSize()>0)
   {
     const char *cp=word_.subString(count,word_.length());
     int j,len=word_.length()-count;
     for (j=0,w=0.0;j<len&&w<=maxWidth;j++)
      {
        w+=fdata->textWidth(fontSize(),cp++,1);
      }
     if (w>maxWidth) j--;
     if (count==0&&j==1&&maxWidth==lineWidth_)
      {
        if (errorMessage==MSFalse)
         {
           MSMessageLog::warningMessage("Warning: unable to print %s at the specified font... "
                                        "Reducing the font size\n",word_.string());
           errorMessage=MSTrue;
         }
        _fontSize--;
      }
     else
      {
        if (maxWidth<lineWidth_)
         {
           outputText().lastElement()<<word_.subString(count,j);
           maxWidth=lineWidth_;
         }
        else outputText()<<word_.subString(count,j);
      }
     count+=j;
   }
  return w;
}

