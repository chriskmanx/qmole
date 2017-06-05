///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////



#ifndef MSTableColumnHEADER
#define MSTableColumnHEADER

#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSSymbolVector.H>
#include <MSGUI/MSPointerArray.H>
#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSWidgetView.H>
#include <MSGUI/MSValidationInterface.H>
#include <MSTypes/MSFormat.H>
#include <MSGUI/MSTable.H>
#include <MSGUI/MSGC.H>

class MSGUIExport MSTableColumn : public MSWidgetView, public MSValidationInterface
{
friend class MSReport;
friend class MSReportTable;
friend class MSTable;
friend class MSTableColumnIterator;
public:
  MSTableColumn(MSReportTable *,const char *heading_=0,
		const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSTableColumn(MSReportTable *,const MSStringVector& heading_,
		const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSTableColumn(void);

  unsigned long style(void) const;
  unsigned long breakStyle(void) const;
  unsigned long headingStyle(void) const;
  const MSString& reportFont(void) const;
  const MSString& breakFont(void) const;
  const MSString& reportHeadingFont(void) const;
  const MSParagraph& breakText(const MSSymbol&) const;
  MSParagraph& breakText(const MSSymbol&);
  MSReportTable *reportTable(void) const;
  const MSSymbol& tag(void) const;
  unsigned column(void) const;
  double fgGrayScale(void) const;
  double bgGrayScale(void) const;
  const MSString& breakFgString(void) const;
  const MSString& breakBgString(void) const;
  unsigned long breakFgPixel(void) const;
  unsigned long breakBgPixel(void) const;
  const MSStringVector& breakString(void) const;
  MSStringVector& breakString(void);
  const MSIndexVector& breakIndex(void) const;
  
  const MSIndexVector& breakInvalid(void) const;
  MSBoolean suppressDuplicate(void) const;
  MSBoolean breakOn(void) const;
  MSBoolean pageBreakOn(void) const;
  MSBoolean breakProcessOn(void) const;
  int breakOffset(void) const;
  int breakLeading(void) const;
  double headingFgGrayScale(void) const;
  double headingBgGrayScale(void) const;
  double breakFgGrayScale(void) const;
  double breakBgGrayScale(void) const;
  MSP::BreakProcessMode breakProcessMode(void) const;
  
  virtual unsigned numRows(void) const;
  virtual MSBoolean breakCriteria(unsigned);
  virtual MSBoolean isDuplicate(unsigned);
  virtual unsigned long style(unsigned);
  virtual unsigned long breakStyle(unsigned);
  virtual int breakOffset(unsigned);
  virtual int breakLeading(unsigned);
  virtual const char *reportFont(unsigned);
  virtual const char *breakFont(unsigned);
  virtual double fgGrayScale(unsigned);
  virtual double bgGrayScale(unsigned);
  virtual double breakFgGrayScale(unsigned);
  virtual double breakBgGrayScale(unsigned);
  virtual const char *breakFgString(unsigned);
  virtual const char *breakBgString(unsigned);
  virtual unsigned long breakFgPixel(unsigned);
  virtual unsigned long breakBgPixel(unsigned);

  void tag(const MSSymbol&);
  void fgGrayScale(double);
  void bgGrayScale(double);
  void breakFgGrayScale(double);
  void breakBgGrayScale(double);
  void headingFgGrayScale(double);
  void headingBgGrayScale(double);
  void style(unsigned long);
  void breakStyle(unsigned long);
  void headingStyle(unsigned long);
  void reportFont(const char *);
  void reportHeadingFont(const char *);
  void breakFont(const char *);
  void breakOn(MSBoolean);
  void pageBreakOn(MSBoolean);
  void breakProcessOn(MSBoolean);
  void suppressDuplicate(MSBoolean);
  void breakOffset(long);
  void breakLeading(long);
  void breakString(const MSStringVector&);
  void breakProcessMode(MSP::BreakProcessMode);
  void breakFg(const char *);
  void breakBg(const char *);
  void removeBreakText(const MSSymbol&);
  void removeAllBreakText(void);
  MSParagraph& addBreakText(const MSParagraph&);
  MSParagraph& addBreakText(const MSStringVector&);
  MSParagraph& addBreakText(const char *);

  const MSStringVector& heading(void) const;
  void heading(const MSStringVector&);

  unsigned long headingForeground(void) const;
  void headingForeground(unsigned long);
  void headingForeground(const char *);

  virtual const char *formatBreak(MSString &,unsigned,unsigned);
  virtual const char *formatOutput(MSString &,unsigned);
  virtual MSIndexVector gradeUp(void) const;
  virtual MSIndexVector gradeDown(void) const;
  virtual void permute(const MSIndexVector&);
  virtual void range(MSIndexVector&,MSIndexVector&);
  virtual MSIndexVector rangeGradeUp(const MSIndexVector&,const MSIndexVector&);
  virtual MSIndexVector rangeGradeDown(const MSIndexVector&,const MSIndexVector&);
  
  void weights(const MSFloatVector&);
  const MSFloatVector& weights(void) const;
  MSFloatVector& weights(void);
  const MSParagraph& defaultText(void) const;
  MSParagraph& defaultText(void);

  MSTable *table(void) const;
  const MSStringVector& choices(void) const;
  const MSUnsignedLongVector& cycleColors(void) const;
  MSCycleColorMode cycleColorMode(void) const;
  MSFormat& format(void);
  const MSFormat& format(void) const;
  unsigned columnWidth(void) const;
  unsigned editWidth(void) const;
  MSAlignment columnAlignment(void) const;
  MSClipMode clipMode(void) const;
  MSBoolean resizable(void) const;
  Font headingFont(void) const;
  unsigned long headingAlignment(void) const;
  MSBoolean valueQuoted(void) const;
  
  void format(const MSFormat&);
  void choices(const MSStringVector&);
  void cycleColorMode(MSCycleColorMode);
  void cycleColors(const MSUnsignedLongVector&);
  void cycleColors(const MSStringVector&);
  void columnWidth(unsigned);
  void editWidth(unsigned);
  void columnAlignment(MSAlignment);
  void clipMode(MSClipMode);
  void resizable(MSBoolean);
  void headingFont(Font);
  void headingFont(const char *);
  void headingAlignment(unsigned long);
  MSSymbolVector groups(void) const;

  void valueQuoted(MSBoolean);
  virtual void redraw(void);
  virtual void map(void);
  virtual void unmap(void);

  MSBoolean sortArrowEnabled(void) const;
  void sortArrowEnabled(MSBoolean);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  MSReportTable   *_reportTable;
  MSSymbol         _tag;
  unsigned         _column;
  unsigned long    _breakFgPixel;
  unsigned long    _breakBgPixel;
  unsigned long    _style;
  unsigned long    _breakStyle;
  unsigned long    _headingStyle;
  unsigned long    _headingForeground;
  double           _headingFgGrayScale;
  double           _headingBgGrayScale;
  double           _breakFgGrayScale;
  double           _breakBgGrayScale;
  double           _fgGrayScale;
  double           _bgGrayScale;
  MSParagraph      _defaultText;
  MSString         _breakFgString;
  MSString         _breakBgString;
  MSString         _reportFont;
  MSString         _reportHeadingFont;
  MSString         _breakFont;
  MSBoolean        _suppressDuplicate;
  MSBoolean        _breakOn;
  MSBoolean        _pageBreakOn;
  MSBoolean        _breakProcessOn;
  MSStringVector   _heading; 
  MSStringVector   _breakString;
  MSIndexVector    _breakIndex;
  MSIndexVector    _breakInvalid;
  MSFloatVector    _weights;
  int              _breakOffset;
  int              _breakLeading;
  MSBoolean        _valueQuoted;
  MSBoolean        _sortArrowEnabled;

  MSP::BreakProcessMode       _breakProcessMode;
  MSPointerArray<MSParagraph> _breakTextList;

  Font                  _headingFont;
  unsigned long         _headingAlignment;
  unsigned              _columnWidth;
  unsigned              _editWidth;
  MSAlignment           _columnAlignment;
  MSClipMode            _clipMode;
  XFontStruct          *_fontStruct;
  XFontStruct          *_headingFontStruct;
  MSGC                  _textMSGC;
  MSUnsignedLongVector  _cycleColors;
  MSCycleColorMode      _cycleMode;
  MSBoolean             _resizable;
  MSFormat              _format;
  MSStringVector        _choices;

  MSTable::ColumnGroupList _groupList;

  void column(unsigned);
  virtual double computeColumnFunction(void);
  virtual void breakProcess(MSIndexVector&);

  MSIndexVector& breakIndex(void);
  MSIndexVector& breakInvalid(void);
  MSParagraph *breakText(unsigned i_) const;
  const MSPointerArray<MSParagraph>& breakTextList(void) const;

  MSGC& textMSGC(void);
  GC textGC(void) const;
  const XFontStruct *fontStruct(void) const;
  const XFontStruct *headingFontStruct(void) const;

  void createGCs(void);
  void init(void);

  void fontStruct(XFontStruct *);
  void headingFontStruct(XFontStruct *);

  int textAscent(void) const;
  int textDescent(void) const;
  int charWidth(void) const;
  int textHeight(void) const;
  int headingAscent(void) const;
  int headingDescent(void) const;
  int headingCharWidth(void) const;
  int headingHeight(void) const;
  int headingWidth(void) const;
  int textWidth(const char *pString_) const;
  int textWidth(const char *pString_,unsigned len_) const;

  const XCharStruct *charStruct(char aChar_) const; 
  const XCharStruct *headingCharStruct(char aChar_) const; 
  int charWidth(char aChar_) const; 
  int headingCharWidth(char aChar_) const;

  virtual void updateHeading(void);
  virtual void updateHeadingForeground(void);
  virtual void updateData(void);
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);  
  virtual void updateBackground(unsigned long);
  virtual void updateSensitivity(void);
  virtual void update(const MSIndexVector&);

  virtual int columnPixelWidth(void);

  virtual MSBoolean validate(const char *,unsigned);
  virtual MSBoolean isCellProtected(unsigned);
  virtual MSBoolean isValid(unsigned);
  virtual unsigned long cellForeground(unsigned);
  virtual unsigned long cellBackground(unsigned);
  virtual Font cellFont(unsigned);
  virtual MSAlignment cellAlignment(unsigned); 

  virtual void moveRow(int, int);
  
  virtual MSBoolean hasOptions(void);
  virtual const MSStringVector& cellChoices(unsigned);

  virtual void increment(int);
  virtual void decrement(int);

  virtual void decoupleWidget(void);

  MSTable::ColumnGroupList &groupList(void);
  const MSTable::ColumnGroupList &groupList(void) const;

 //For backward compatibility only.
  virtual MSBoolean isProtected(unsigned);
  virtual MSBoolean isProtected(void) const;
};

#ifndef MS_NO_INLINES
#ifndef MSTableColumnINLINES
#include <MSGUI/MSTableColumnInlines.C>
#endif
#endif

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSPointerArray<MSParagraph>
#pragma do_not_instantiate MSPointerArray<MSParagraph>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
