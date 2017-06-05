#ifndef MSTableColumnINLINES
#define MSTableColumnINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif
  
// These used to be MSBaseTableColumn's inlines
INLINELINKAGE MSReportTable *MSTableColumn::reportTable(void)        const {return _reportTable;}
INLINELINKAGE const MSSymbol& MSTableColumn::tag(void)               const {return _tag;}
INLINELINKAGE unsigned MSTableColumn::column(void)                   const {return _column;}
INLINELINKAGE double MSTableColumn::fgGrayScale(void)                const {return _fgGrayScale;}
INLINELINKAGE double MSTableColumn::bgGrayScale(void)                const {return _bgGrayScale;}
INLINELINKAGE const MSString& MSTableColumn::breakFgString(void)     const {return _breakFgString;}
INLINELINKAGE const MSString& MSTableColumn::breakBgString(void)     const {return _breakBgString;}
INLINELINKAGE unsigned long MSTableColumn::breakFgPixel(void)        const {return _breakFgPixel;}
INLINELINKAGE unsigned long MSTableColumn::breakBgPixel(void)        const {return _breakBgPixel;}
INLINELINKAGE const MSStringVector& MSTableColumn::breakString(void) const {return _breakString;}
INLINELINKAGE MSStringVector& MSTableColumn::breakString(void)             {return _breakString;}
INLINELINKAGE const MSIndexVector& MSTableColumn::breakIndex(void)   const {return _breakIndex;}
INLINELINKAGE const MSIndexVector& MSTableColumn::breakInvalid(void) const {return _breakInvalid;}
INLINELINKAGE MSBoolean MSTableColumn::suppressDuplicate(void)       const {return _suppressDuplicate;}
INLINELINKAGE MSBoolean MSTableColumn::breakOn(void)                 const {return _breakOn;}
INLINELINKAGE MSBoolean MSTableColumn::pageBreakOn(void)             const {return _pageBreakOn;}
INLINELINKAGE MSBoolean MSTableColumn::breakProcessOn(void)          const {return _breakProcessOn;}
INLINELINKAGE int MSTableColumn::breakOffset(void)                   const {return _breakOffset;}
INLINELINKAGE int MSTableColumn::breakLeading(void)                  const {return _breakLeading;}
INLINELINKAGE double MSTableColumn::headingFgGrayScale(void)         const {return _headingFgGrayScale;}
INLINELINKAGE double MSTableColumn::headingBgGrayScale(void)         const {return _headingBgGrayScale;}
INLINELINKAGE double MSTableColumn::breakFgGrayScale(void)           const {return _breakFgGrayScale;}
INLINELINKAGE double MSTableColumn::breakBgGrayScale(void)           const {return _breakBgGrayScale;}
INLINELINKAGE MSP::BreakProcessMode MSTableColumn::breakProcessMode(void) const
{return _breakProcessMode;}
INLINELINKAGE void MSTableColumn::tag(const MSSymbol& x_)           {_tag=x_;}
INLINELINKAGE void MSTableColumn::fgGrayScale(double x_)            {_fgGrayScale=x_<0?0:x_>1?1:x_;}
INLINELINKAGE void MSTableColumn::bgGrayScale(double x_)            {_bgGrayScale=x_<0?0:x_>1?1:x_;}
INLINELINKAGE void MSTableColumn::breakFgGrayScale(double x_)       {_breakFgGrayScale=x_<0?0:x_>1?1:x_;}
INLINELINKAGE void MSTableColumn::breakBgGrayScale(double x_)       {_breakBgGrayScale=x_<0?0:x_>1?1:x_;}
INLINELINKAGE void MSTableColumn::headingFgGrayScale(double x_)     {_headingFgGrayScale=x_<0?0:x_>1?1:x_;}
INLINELINKAGE void MSTableColumn::headingBgGrayScale(double x_)     {_headingBgGrayScale=x_<0?0:x_>1?1:x_;}
INLINELINKAGE void MSTableColumn::style(unsigned long x_)           {_style=x_;}
INLINELINKAGE void MSTableColumn::breakStyle(unsigned long x_)      {_breakStyle=x_;}
INLINELINKAGE void MSTableColumn::headingStyle(unsigned long x_)    {_headingStyle=x_;}
INLINELINKAGE void MSTableColumn::reportFont(const char *x_)        {_reportFont=x_;}
INLINELINKAGE void MSTableColumn::reportHeadingFont(const char *x_) {_reportHeadingFont=x_;}
INLINELINKAGE void MSTableColumn::breakFont(const char *x_)         {_breakFont=x_;}
INLINELINKAGE void MSTableColumn::breakOn(MSBoolean x_)             {_breakOn=x_;}
INLINELINKAGE void MSTableColumn::pageBreakOn(MSBoolean x_)         {_pageBreakOn=x_;}
INLINELINKAGE void MSTableColumn::breakProcessOn(MSBoolean x_)      {_breakProcessOn=x_;}
INLINELINKAGE void MSTableColumn::suppressDuplicate(MSBoolean x_)   {_suppressDuplicate=x_;}
INLINELINKAGE void MSTableColumn::breakOffset(long x_)               {_breakOffset=x_;}
INLINELINKAGE void MSTableColumn::breakLeading(long x_)              {_breakLeading=x_;}
INLINELINKAGE void MSTableColumn::breakString(const MSStringVector& x_)       {_breakString=x_;}
INLINELINKAGE void MSTableColumn::breakProcessMode(MSP::BreakProcessMode x_)  {_breakProcessMode=x_;}
INLINELINKAGE const MSStringVector& MSTableColumn::heading(void)    const {return _heading;}
INLINELINKAGE unsigned long MSTableColumn::headingForeground(void)  const {return _headingForeground;}
INLINELINKAGE void MSTableColumn::weights(const MSFloatVector& x_)        {_weights=x_;}
INLINELINKAGE const MSFloatVector& MSTableColumn::weights(void)     const {return _weights;}
INLINELINKAGE MSFloatVector& MSTableColumn::weights(void)                 {return _weights;}
INLINELINKAGE const MSParagraph& MSTableColumn::defaultText(void)   const {return _defaultText;}
INLINELINKAGE MSParagraph& MSTableColumn::defaultText(void)               {return _defaultText;}
INLINELINKAGE void MSTableColumn::column(unsigned x_)                     {_column=x_;}
INLINELINKAGE MSIndexVector& MSTableColumn::breakIndex(void)              {return _breakIndex;}
INLINELINKAGE MSIndexVector& MSTableColumn::breakInvalid(void)            {return _breakInvalid;}
INLINELINKAGE MSParagraph *MSTableColumn::breakText(unsigned i_)    const {return _breakTextList.array(i_);}
INLINELINKAGE const MSPointerArray<MSParagraph>& MSTableColumn::breakTextList(void) const
{return _breakTextList;}

// These are MSTableColumn's inlines
INLINELINKAGE const MSFormat& MSTableColumn::format(void) const
{ return _format; }
INLINELINKAGE MSFormat& MSTableColumn::format(void)
{ return _format; }

INLINELINKAGE MSBoolean MSTableColumn::resizable(void) const
{ return _resizable; }
INLINELINKAGE Font MSTableColumn::headingFont(void) const
{ return _headingFont;}
INLINELINKAGE unsigned long MSTableColumn::headingAlignment(void) const
{ return _headingAlignment;}
INLINELINKAGE MSGC& MSTableColumn::textMSGC(void) 
{ return _textMSGC; }
INLINELINKAGE const XFontStruct *MSTableColumn::fontStruct(void) const
{ return _fontStruct; }
INLINELINKAGE const XFontStruct *MSTableColumn::headingFontStruct(void) const
{ return _headingFontStruct; }

INLINELINKAGE int MSTableColumn::textAscent(void) const     
{ return _fontStruct->max_bounds.ascent; }
INLINELINKAGE int MSTableColumn::textDescent(void) const    
{ return _fontStruct->max_bounds.descent; }
INLINELINKAGE int MSTableColumn::charWidth(void) const      
{ return _fontStruct->max_bounds.width; }
INLINELINKAGE int MSTableColumn::textHeight(void) const     
{ return (_fontStruct->max_bounds.ascent+_fontStruct->max_bounds.descent); }
INLINELINKAGE int MSTableColumn::headingAscent(void) const    
{ return _headingFontStruct->max_bounds.ascent; }
INLINELINKAGE int MSTableColumn::headingDescent(void) const   
{ return _headingFontStruct->max_bounds.descent; }
INLINELINKAGE int MSTableColumn::headingCharWidth(void) const 
{ return _headingFontStruct->max_bounds.width; }

INLINELINKAGE const MSStringVector& MSTableColumn::choices(void) const  
{ return _choices; }  
INLINELINKAGE MSCycleColorMode MSTableColumn::cycleColorMode(void) const  
{ return _cycleMode; }  
INLINELINKAGE const MSUnsignedLongVector& MSTableColumn::cycleColors(void) const  
{ return _cycleColors; }  
INLINELINKAGE unsigned MSTableColumn::columnWidth(void) const
{ return _columnWidth; }
INLINELINKAGE unsigned MSTableColumn::editWidth(void) const            
{ return _editWidth; }
INLINELINKAGE MSAlignment MSTableColumn::columnAlignment(void) const
{ return _columnAlignment; }
INLINELINKAGE MSClipMode MSTableColumn::clipMode(void) const
{ return _clipMode; }

INLINELINKAGE MSTable::ColumnGroupList &MSTableColumn::groupList(void) {return _groupList;} 
INLINELINKAGE const MSTable::ColumnGroupList &MSTableColumn::groupList(void) const {return _groupList;}

INLINELINKAGE MSBoolean MSTableColumn::valueQuoted(void) const { return _valueQuoted; }
INLINELINKAGE void MSTableColumn::valueQuoted(MSBoolean bool_) { _valueQuoted=bool_; }

#endif



