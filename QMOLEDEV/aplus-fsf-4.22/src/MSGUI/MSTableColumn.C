///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <float.h>
#include <MSTypes/MSUtil.H>
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSTableColumnGroup.H>
#include <MSTypes/MSMessageLog.H>

static const unsigned MSTableColumnDefaultColumnWidth=9;
static const unsigned MSTableColumnDefaultEditWidth=256;

#ifdef MS_NO_INLINES
#include <MSGUI/MSTableColumnInlines.C>
#endif

MSTableColumn::MSTableColumn(MSReportTable *owner_,const char *heading_,const MSSymbol& tag_) : 
_reportTable(owner_),_heading(heading_),_tag(tag_),MSWidgetView(owner_->displayServer())
{ init(); }

MSTableColumn::MSTableColumn(MSReportTable *owner_,const MSStringVector& heading_,const MSSymbol& tag_) : 
_reportTable(owner_),_heading(heading_),_tag(tag_),MSWidgetView(owner_->displayServer())
{ init(); }

void MSTableColumn::init(void)
{
  _style=0;
  _breakStyle=reportTable()->breakStyle();
  _headingStyle=reportTable()->headingStyle();
  _breakOffset=reportTable()->breakOffset();
  _breakLeading=reportTable()->breakLeading();
  _fgGrayScale=reportTable()->fgGrayScale();
  _bgGrayScale=reportTable()->bgGrayScale();
  _breakFgGrayScale=reportTable()->fgGrayScale();
  _breakBgGrayScale=reportTable()->bgGrayScale();
  _headingFgGrayScale=reportTable()->headingFgGrayScale();
  _headingBgGrayScale=reportTable()->headingBgGrayScale();
  _headingForeground=0;
  _suppressDuplicate=MSFalse;
  _breakOn=MSFalse;
  _pageBreakOn=MSFalse;
  _breakProcessOn=MSTrue;
  _breakProcessMode=MSP::Total;
  _column=reportTable()->columnList()->count();
  _breakFgPixel=ULONG_MAX;
  _breakBgPixel=ULONG_MAX;
  _valueQuoted=MSTrue;

  _owner=table();
  if (owner()!=0)
   {
     _bg=owner()->background();
     _fg=owner()->foreground();
     _fontID=owner()->font();
   }
  sensitive(MSTrue);
  _resizable=MSTrue;
  _columnWidth=MSTableColumnDefaultColumnWidth;
  _columnAlignment=MSRight;
  _clipMode=MSNoClipping;
  _editWidth=MSTableColumnDefaultEditWidth;
  _cycleMode=MSForeground;
  headingForeground(reportTable()->headingForeground());
  if (table()!=0)
   {
     _headingFont=table()->headingFont();
     _headingAlignment=table()->headingAlignment();
     fontStruct((XFontStruct *)server()->fontStruct(font()));
     if (font()!=headingFont()) headingFontStruct((XFontStruct *)server()->fontStruct(headingFont()));
     else headingFontStruct((XFontStruct *)fontStruct());
     createGCs();
     childCreateNotify();
   }
  else reportTable()->addColumn(this);
}

MSTableColumn::~MSTableColumn(void) 
{ 
  for (unsigned i=0;i<breakTextList().count();i++) delete breakText(i); 
  _fontStruct=0; 
  _headingFontStruct=0; 
}

void MSTableColumn::format(const MSFormat& aFormat_)
{
  if (format()!=aFormat_)
   {
     if (format().formatType()==MSFormat::NoFormat||
	 aFormat_.formatType()==format().formatType())
      {
        _format=aFormat_;
	redraw();
      }
   }
}

GC MSTableColumn::textGC(void) const               
{ return _textMSGC.gc(); }

void MSTableColumn::headingFont(const char *fid_) 
{ headingFont(server()->fontID(fid_)); }

void MSTableColumn::headingFont(Font fid_) 
{ 
  if (headingFont()!=fid_)
   {
     _headingFont=fid_;
     updateHeading();
   }
}

void MSTableColumn::headingAlignment(unsigned long alignment_)
{ 
  if (headingAlignment()!=alignment_)
   {
     _headingAlignment=alignment_;
     if (table()!=0) table()->updateColumnHeadings();
   }
}

// this does not seem to work properly when it is inlined
// looks like this is a compiler bug - ????
MSTable *MSTableColumn::table(void) const
{ return reportTable()->displayServer()==0?0:(MSTable *)reportTable(); }

// default methods that should be overriden by subclasses
MSBoolean MSTableColumn::validate(const char *,unsigned)
{ return MSFalse; }
MSBoolean MSTableColumn::isValid(unsigned)               
{ return sensitive()==MSFalse ? MSFalse : table()->sensitive(); }

//"isProtected(row)" is only kept for Backward compatibility
//It's use is now deprecated in favor of isCellProtected
MSBoolean MSTableColumn::isCellProtected(unsigned r_)
{ return isProtected(r_); }
MSBoolean MSTableColumn::isProtected(unsigned)
{ return MSWidgetView::isProtected(); }
MSBoolean MSTableColumn::isProtected(void) const
{ return MSWidgetView::isProtected(); }


unsigned long MSTableColumn::cellForeground(unsigned row_)        
{
  if (table()!=0)
   {
     if (table()->foregroundColors().length()==0) return foreground();
     else return table()->foregroundColors()(row_%table()->foregroundColors().length());
   }
  else return foreground();
}
unsigned long MSTableColumn::cellBackground(unsigned row_)        
{
  if (table()!=0)
   {
     if (table()->backgroundColors().length()==0) return background();
     else return table()->backgroundColors()(row_%table()->backgroundColors().length());
   }
  else return background();
}

Font MSTableColumn::cellFont(unsigned)        
{ return font(); }
MSAlignment MSTableColumn::cellAlignment(unsigned)
{ return columnAlignment(); }

// end of default methods 

void MSTableColumn::createGCs(void)
{
  if (table()!=0)
   {
     XGCValues values;
     values.foreground=background();
     _textMSGC.setGCValues(server(),MSTrue,&values,GCForeground);
   }
}

void MSTableColumn::updateSensitivity(void)
{
  MSWidgetView::updateSensitivity();
  redraw();
}

void MSTableColumn::updateForeground(unsigned long oldfg_)
{
  MSWidget::updateForeground(oldfg_);
  if (headingForeground()==oldfg_) headingForeground(foreground());
  redraw();
}

void MSTableColumn::updateBackground(unsigned long oldbg_)
{
  MSWidget::updateBackground(oldbg_);
  textMSGC().foreground(background());
  redraw();
}

void MSTableColumn::updateFont(Font oldfid_) 
{
  MSWidget::updateFont(oldfid_);
  if (table()!=0)
   {
     MSBoolean wasFrozen=table()->frozen();
     table()->freeze();
     if (headingFont()==oldfid_) headingFont(font());
     fontStruct((XFontStruct *)server()->fontStruct(font()));
     table()->calculateRowHeight();
     table()->adjustNumVisible();
     if (wasFrozen==MSFalse) table()->unfreeze();
   }
}

void MSTableColumn::updateData(void) 
{
  if (table()!=0)
   {
     // Process it if this column is not hidden, otherwise don't do anything
     if (table()->hiddenColumnList()->find(this)==MSFalse)
      {
	if (numRows()>=table()->dataRows()) table()->appendUpdate();
	if (hasModel()==MSTrue) table()->columnUpdate(column());
	else
	 {
	   table()->updateInternalState();
	   if (frozen()==MSFalse) table()->clearColumn(column());
	 }
      }
   }
  else reportTable()->maxRowsSet(numRows());
}

void MSTableColumn::redraw(void)
{ if (table()!=0&&frozen()==MSFalse) table()->drawColumn(column()); }

int MSTableColumn::columnPixelWidth(void)
{
  if (table()!=0)
   {
     unsigned len=columnWidth();
     int cw=(clipMode()==MSNoClipping)?charWidth('W'):charWidth('0');
     return (len*cw+2*table()->columnSpacing());
   }
  else return 0;
}

int MSTableColumn::headingHeight(void) const
{ return heading().length()*(headingAscent()+headingDescent()); }

int MSTableColumn::headingWidth(void) const
{
  unsigned n=heading().length();
  int tw=0;
  for (unsigned i=0;i<n;i++)
   {
     const MSString& aString=heading()[i];
     tw=MSUtil::max(tw,XTextWidth((XFontStruct *)headingFontStruct(),
				  (const char *)aString,aString.length()));
   }
  return tw;
}

void MSTableColumn::update(const MSIndexVector& aIndexVector_)
{
  if (table()!=0)
   {
     unsigned tableRows=table()->dataRows();
     // Process it if this column is not hidden, otherwise don't do anything
     if (table()->hiddenColumnList()->find(this)==MSFalse)
      {
	if (table()->frozen()==MSFalse)
	 {
	   if (aIndexVector_.length()==0) 
	    {
	      if (numRows()!=tableRows) 
	       {
		 if (numRows()>tableRows) table()->appendUpdate(); 
		 table()->columnUpdate(column());
	       }
	      else table()->cycleColumn(column());
	    }
	   else
	    {
	      if (numRows()>tableRows) table()->appendUpdate(); 
	      for (unsigned i=0;i<aIndexVector_.length();i++)
	       {
		 table()->cycleRowColumn(aIndexVector_(i),column());
	       }
	    }
	 }
      }
   }
  else reportTable()->maxRowsSet(numRows()); 
}

void MSTableColumn::updateHeading(void) 
{
  if (table()!=0)
   {
     if (table()->firstMap()==MSTrue&&table()->frozen()==MSFalse)
      {
        int hh=table()->headingsHeight();
        table()->calculateHeadingsHeight();
        if (hh!=table()->headingsHeight())
         {
           table()->adjustNumVisible();
           table()->redraw();
         }
        else table()->updateColumnHeadings();
      }
   }
}


void MSTableColumn::updateHeadingForeground(void) 
{
  if (table()!=0) table()->updateColumnHeadings();   
}

void MSTableColumn::columnWidth(unsigned cw_)
{
  if (columnWidth()!=cw_)
   {
     _columnWidth=cw_;
     if (table()!=0)
      {
	table()->adjustNumVisible();
	table()->redraw();
      }
     activateCallback(MSWidgetCallback::columnresize);
   }
}

void MSTableColumn::clipMode(MSClipMode clipMode_)
{
  if (clipMode()!=clipMode_ && clipMode_!=MSClipIndicator)
   {
     _clipMode=clipMode_;
     redraw();
   }
}

void MSTableColumn::columnAlignment(MSAlignment alignment_)
{
  if (columnAlignment()!=alignment_)
   {
     _columnAlignment=alignment_;
     redraw();
   }
}

void MSTableColumn::editWidth(unsigned ew_) 
{ _editWidth=ew_; }
void MSTableColumn::resizable(MSBoolean resizable_)
{ _resizable=resizable_; }
void MSTableColumn::cycleColorMode(MSCycleColorMode cycleMode_)
{ _cycleMode=cycleMode_; }

void MSTableColumn::fontStruct(XFontStruct *fs_)      
{ _fontStruct=fs_; }
void MSTableColumn::headingFontStruct(XFontStruct *fs_) 
{ _headingFontStruct=fs_; }

int MSTableColumn::textWidth(const char *pString_) const
{ 
  if (_fontStruct->max_char_or_byte2>255)
   {
     return (pString_!=0)?
     XTextWidth16((XFontStruct *)_fontStruct,(XChar2b *)pString_,strlen(pString_)/2):0;
   }
  else
   {
     return (pString_!=0)?
     XTextWidth((XFontStruct *)_fontStruct,(char *)pString_,strlen(pString_)):0;
   }
}

int MSTableColumn::textWidth(const char *pString_,unsigned len_) const
{
  if (_fontStruct->max_char_or_byte2>255)
   {
     return (pString_!=0)?
     XTextWidth16((XFontStruct *)_fontStruct,(XChar2b *)pString_,len_/2):0;
   }
  else
   {
     return (pString_!=0)?XTextWidth((XFontStruct *)_fontStruct,(char *)pString_,len_):0;
   }
}

const XCharStruct *MSTableColumn::charStruct(char aChar_) const
{ return &(_fontStruct->per_char[aChar_-_fontStruct->min_char_or_byte2]); }

inline const XCharStruct *MSTableColumn::headingCharStruct(char aChar_) const
{ return &(_headingFontStruct->per_char[aChar_-_headingFontStruct->min_char_or_byte2]); }

int MSTableColumn::charWidth(char aChar_) const
{
  int r=charWidth();
  if (_fontStruct->per_char!=0&&
      aChar_>=_fontStruct->min_char_or_byte2&&aChar_<=_fontStruct->max_char_or_byte2)
   {
     r=charStruct(aChar_)->width;
   }
  return r;
}

int MSTableColumn::headingCharWidth(char aChar_) const
{
  int r=headingCharWidth();
  if (headingFontStruct()->per_char!=0&& aChar_>=headingFontStruct()->min_char_or_byte2&&
      aChar_<=headingFontStruct()->max_char_or_byte2)
   {
     r=headingCharStruct(aChar_)->width;
   }
  return r;
}

void MSTableColumn::choices(const MSStringVector& choices_) 
{
  _choices=choices_;
  if(table()!=0) table()->updateChoices();
}

void MSTableColumn::cycleColors(const MSUnsignedLongVector& colors_) 
{ _cycleColors=colors_; }

void MSTableColumn::cycleColors(const MSStringVector& colors_)
{
  MSUnsignedLongVector cols(colors_.length());
  for (int i=0;i<colors_.length();i++) cols[i]=(server()->pixel(colors_(i)));
  cycleColors(cols);
}

void MSTableColumn::decoupleWidget(void)
{ decouple(); }

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSTableColumn::map(void) {}
void MSTableColumn::unmap(void) {}
void MSTableColumn::increment(int) {}
void MSTableColumn::decrement(int) {}

void MSTableColumn::set(MSAttrValueList& avList_)
{
  MSWidget::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="breakOn")
      {
       breakOn(avList_[i].value().asBoolean());
       index<<i;
      }
     else if (avList_[i].attribute()=="suppressDuplicate")
      {
       suppressDuplicate(avList_[i].value().asBoolean());
       index<<i;
      }
/*     else if (avList_[i].attribute()=="breakFont")
      {
	breakFont(avList_[i].value());
	index<<i;
      }
      */
     else if (avList_[i].attribute()=="breakFg")
      {
	if(avList_[i].value().length()!=0) breakFg(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="breakBg")
      {
  	if(avList_[i].value().length()!=0) breakBg(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="breakProcessOn")
      {
	breakProcessOn(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="breakProcessMode")
      {
       MSStringVector modes="MSP::Total\nMSP::Minimum\nMSP::Maximum\nMSP::Average";
       MSUnsignedLongVector modeValues;
       modeValues.append(MSP::Total);
       modeValues.append(MSP::Minimum); 
       modeValues.append(MSP::Maximum);
       modeValues.append(MSP::Average);

       unsigned long mode=MSAttrValue::stringToEnum(avList_[i].value(),modes,modeValues,MSP::Total,MSTrue);
       breakProcessMode((MSP::BreakProcessMode)mode);
       index<<i;
      }
     else if (avList_[i].attribute()=="heading")
      {
	heading(MSAttrValue::stringToStringVector(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="headingForeground")
      {
	headingForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="headingFont")
      {
	headingFont(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="headingAlignment")
      {
	headingAlignment(MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="cycleColors")
      {
        cycleColors(MSAttrValue::stringToStringVector(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="cycleColorMode")
      {
	if (avList_[i].value()=="MSBackground") cycleColorMode(MSBackground);
	else if (avList_[i].value()=="MSReverseVideo") cycleColorMode(MSReverseVideo);
	else cycleColorMode(MSForeground);
	index<<i;
      }
     else if (avList_[i].attribute()=="clipMode")
      {
	if (avList_[i].value()=="MSClipStars") clipMode(MSClipStars);
	else clipMode(MSNoClipping);
	index<<i;
      }
     else if (avList_[i].attribute()=="columnAlignment")
      {
	if (avList_[i].value()=="MSRight") columnAlignment(MSRight);
	else if (avList_[i].value()=="MSLeft") columnAlignment(MSLeft);
	else columnAlignment(MSCenter);
	index<<i;
      }
     else if (avList_[i].attribute()=="columnWidth")
      {
	columnWidth(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="editWidth")
      {
	editWidth(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="resizable")
      {
	resizable(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="format")
      {
	format(MSFormat(avList_[i].value()));
	index<<i;
      }
     else if(avList_[i].attribute()=="tag")
      {
        if(avList_[i].value().length()==0) tag(MSSymbol::nullSymbol());
        else tag(MSSymbol(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="valueQuoted")
      {
        valueQuoted(avList_[i].value().asBoolean());
        index<<i;
      }
     else if (avList_[i].attribute()=="choices")
      {
        choices(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSTableColumn::get(MSAttrValueList& avList_)
{
  MSStringVector aBoolVector("MSFalse\nMSTrue");
  avList_<<MSAttrValue("tag",tag().symbolName(),MSAttrValue::Control|MSAttrValue::String);
  avList_<<MSAttrValue("heading",MSAttrValue::stringVectorToString(heading()),MSAttrValue::String);
  avList_<<MSAttrValue("headingForeground",server()->colorName(headingForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("headingFont",server()->fontName(headingFont()),MSAttrValue::Font);
  MSStringVector alignmentVector("MSNone\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight");
  avList_<<MSAttrValue("headingAlignment",MSAttrValue::alignmentToString(headingAlignment()),
                       alignmentVector, MSAttrValue::List);
  avList_<<MSAttrValue("cycleColors",MSAttrValue::colorVectorToString(cycleColors(),server()),
                       MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);
  const char *value;
  switch (cycleColorMode())
  {
  case MSBackground:   value="MSBackground";   break;
  case MSReverseVideo: value="MSReverseVideo"; break;
  case MSForeground:
  default:             value="MSForeground";   break;
  }
  avList_<<MSAttrValue("cycleColorMode",value, MSStringVector("MSBackground\nMSForeground\nMSReverseVideo"));
  switch (columnAlignment())
   {
   case MSCenter:  value="MSCenter"; break;
   case MSLeft:    value="MSLeft";   break;
   case MSRight:
   default:        value="MSRight";  break;
   }
  avList_<<MSAttrValue("columnAlignment",value,MSStringVector("MSLeft\nMSRight\nMSCenter"));
  MSStringVector clipModeVector("MSNoClipping\nMSClipStars");
  if(clipMode()==MSClipStars) value="MSClipStars";
  else value="MSNoClipping";
  avList_<<MSAttrValue("clipMode",value,clipModeVector);
  avList_<<MSAttrValue("columnWidth",MSString(columnWidth()));
  avList_<<MSAttrValue("editWidth",MSString(editWidth()));
  avList_<<MSAttrValue("resizable",resizable()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("format",format().asString(),format().formats(),MSAttrValue::String);

  avList_<<MSAttrValue("breakOn",breakOn()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  
  MSStringVector modes="MSP::Total\nMSP::Minimum\nMSP::Maximum\nMSP::Average";
  MSUnsignedLongVector modeValues;
  modeValues.append(MSP::Total);
  modeValues.append(MSP::Minimum);
  modeValues.append(MSP::Maximum);
  modeValues.append(MSP::Average);

  MSString result=MSAttrValue::enumToString(breakProcessMode(),modes,modeValues,modes(0),MSTrue);
  
  avList_<<MSAttrValue("breakProcessMode",result,modes);
  avList_<<MSAttrValue("breakProcessOn",breakProcessOn()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("breakBg",server()->colorName(breakBgPixel()),MSAttrValue::Color);
  avList_<<MSAttrValue("breakFg",server()->colorName(breakFgPixel()),MSAttrValue::Color);
//  avList_<<MSAttrValue("breakFont",server()->fontName(breakFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("suppressDuplicate",suppressDuplicate()==MSTrue?"MSTrue":"MSFalse",aBoolVector);

  avList_<<MSAttrValue("columnresize","",MSAttrValue::Callback);
  avList_<<MSAttrValue("valueQuoted",valueQuoted()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("choices",MSAttrValue::stringVectorToString(choices()),MSAttrValue::StringVector);
  return MSWidget::get(avList_);
}


void MSTableColumn::removeAllBreakText(void)
{
  for (unsigned i=0;i<breakTextList().count();i++) delete breakText(i);
  _breakTextList.removeAll();
}

void MSTableColumn::removeBreakText(const MSSymbol& tag_)
{
  MSParagraph *printText=0;
  for (unsigned i=0;i<breakTextList().count();i++) if (tag_==breakText(i)->tag()) printText=breakText(i);
  if (printText!=0)
   {
     delete printText;
     _breakTextList.remove(printText);
   }
}

MSParagraph& MSTableColumn::addBreakText(const MSParagraph& printText_)
{
  MSParagraph *printText=new MSParagraph(printText_);
  _breakTextList.add(printText);
  return *printText;
}
MSParagraph& MSTableColumn::addBreakText(const MSStringVector& printText_)
{ return addBreakText(MSParagraph(printText_));}
MSParagraph& MSTableColumn::addBreakText(const char *printText_)
{ return addBreakText(MSParagraph(printText_));}
  
const MSParagraph& MSTableColumn::breakText(const MSSymbol& tag_) const
{
  for (unsigned i=0;i<breakTextList().count();i++) if (tag_==breakText(i)->tag()) return *breakText(i);
  MSMessageLog::warningMessage("Warning: breakText \"%s\" not fount\n",tag_.symbolName());
  return defaultText();
}

MSParagraph& MSTableColumn::breakText(const MSSymbol& tag_)
{
  for (unsigned i=0;i<breakTextList().count();i++) if (tag_==breakText(i)->tag()) return *breakText(i);
  MSMessageLog::warningMessage("Warning: breakText \"%s\" not fount\n",tag_.symbolName());
  return defaultText();
}

void MSTableColumn::heading(const MSStringVector& heading_) 
{ 
  if (heading()!=heading_)
   {
     _heading=heading_;
     updateHeading(); 
   }
}

void MSTableColumn::headingForeground(const char *fg_) 
{ headingForeground(reportTable()->convertForeground(fg_)); }

void MSTableColumn::headingForeground(unsigned long fg_) 
{ 
  if (headingForeground()!=fg_)
   {
     _headingForeground=fg_;
     updateHeadingForeground();
   }
}

const MSString& MSTableColumn::reportFont(void) const
{ return _reportFont.length()!=0?(const MSString&)_reportFont:reportTable()->reportFont();}

const MSString& MSTableColumn::breakFont(void) const
{ return _breakFont.length()!=0?(const MSString&)_breakFont:reportTable()->breakFont();}

const MSString& MSTableColumn::reportHeadingFont(void) const
{ return _reportHeadingFont.length()!=0?(const MSString&)_reportHeadingFont:reportTable()->reportHeadingFont(); }

void MSTableColumn::breakFg(const char *color_)
{
  _breakFgString=color_;
  if (reportTable()->displayServer()!=0)
   {
     _breakFgPixel=reportTable()->displayServer()->pixel(color_);
     reportTable()->updateScreen();
   }
}

void MSTableColumn::breakBg(const char *color_)
{
  _breakBgString=color_;
  if (reportTable()->displayServer()!=0)
   {
     _breakBgPixel=reportTable()->displayServer()->pixel(color_);
     reportTable()->updateScreen();
   }
}

const char *MSTableColumn::formatOutput(MSString &buffer_,unsigned)
{return buffer_.string();}

const char *MSTableColumn::formatBreak(MSString &buffer_,unsigned,unsigned)
{return buffer_.string();}

unsigned MSTableColumn::numRows(void) const
{return 0;}

MSBoolean MSTableColumn::breakCriteria(unsigned)
{return MSFalse;}

MSBoolean MSTableColumn::isDuplicate(unsigned)
{return MSFalse;}

unsigned long MSTableColumn::style(unsigned)
{return style();}

unsigned long MSTableColumn::breakStyle(unsigned)
{return breakStyle();}

int MSTableColumn::breakOffset(unsigned)
{return _breakOffset;}

int MSTableColumn::breakLeading(unsigned)
{return _breakLeading;}

const char *MSTableColumn::reportFont(unsigned)
{return reportFont();}

const char *MSTableColumn::breakFont(unsigned)
{return breakFont();}

double MSTableColumn::fgGrayScale(unsigned)
{return fgGrayScale();}

double MSTableColumn::bgGrayScale(unsigned)
{return bgGrayScale();}

double MSTableColumn::breakFgGrayScale(unsigned)
{return breakFgGrayScale();}

double MSTableColumn::breakBgGrayScale(unsigned)
{return breakBgGrayScale();}

const char *MSTableColumn::breakFgString(unsigned)
{return breakFgString();}

const char *MSTableColumn::breakBgString(unsigned)
{return breakBgString();}

unsigned long MSTableColumn::breakFgPixel(unsigned)
{return breakFgPixel();}

unsigned long MSTableColumn::breakBgPixel(unsigned)
{return breakBgPixel();}

unsigned long MSTableColumn::headingStyle(void) const
{return _headingStyle!=0?_headingStyle:reportTable()->headingStyle();}

unsigned long MSTableColumn::style(void) const
{
  unsigned long aStyle=_style!=0?_style:reportTable()->style();
  if ((aStyle&(MSLeft|MSRight|MSCenter))==0)
   {
     aStyle|=columnAlignment();
   }
  return aStyle;
}

unsigned long MSTableColumn::breakStyle(void) const
{ return _breakStyle!=0?_breakStyle:reportTable()->breakStyle(); }

double MSTableColumn::computeColumnFunction(void)
{return 0;}

void MSTableColumn::breakProcess(MSIndexVector&)
{}

MSIndexVector MSTableColumn::gradeUp(void) const
{ return MSIndexVector::nullVector(); }

MSIndexVector MSTableColumn::gradeDown(void) const
{ return MSIndexVector::nullVector(); }

void MSTableColumn::permute(const MSIndexVector &)
{}

void MSTableColumn::range(MSIndexVector &start_,MSIndexVector &end_)
{
  start_.removeAll();
  end_.removeAll();
}

MSIndexVector MSTableColumn::rangeGradeUp(const MSIndexVector &,const MSIndexVector &)
{ return MSIndexVector::nullVector(); }

MSIndexVector MSTableColumn::rangeGradeDown(const MSIndexVector &,const MSIndexVector &)
{ return MSIndexVector::nullVector(); }


MSSymbolVector MSTableColumn::groups(void) const
{
  MSSymbolVector symbols;
  unsigned len=groupList().length();
  for (unsigned i=0;i<len;i++)
   {
     const MSManagedPointer<MSTableColumnGroup> &group=groupList()[i];
     symbols<<group->tag();
   }
  return symbols;
}

MSBoolean MSTableColumn::hasOptions(void)
{
  return choices().length()>0 ? MSTrue : MSFalse;
}

const MSStringVector& MSTableColumn::cellChoices(unsigned)
{
  return choices();
}

void MSTableColumn::moveRow(int, int)
{}
