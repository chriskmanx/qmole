#ifndef MSReportTableHEADER
#define MSReportTableHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloat.H>
#include <MSTypes/MSSymbolVector.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSNodeList.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSUnsignedVector.H>
#include <MSTypes/MSCallback.H>
#include <MSGUI/MSDisplayServer.H>
#include <MSGUI/MSPrintItem.H>
#include <MSGUI/MSReport.H>
#include <MSGUI/MSTableColumnGroup.H>

class MSGUIExport MSReportTable : public MSPrintItem, public MSPrintHeaders
{
friend class MSTableColumn;

public:
  MSReportTable(const char *title_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSReportTable(const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSReportTable(void);

  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);
  //
  // screen and print methods
  //
  MSSymbolVector tags(void);
  MSSymbolVector tags(void) const;
  MSSymbolVector hiddenTags(void);
  MSSymbolVector hiddenTags(void) const;
  MSTableColumn *reportColumn(const MSSymbol&);
  MSTableColumn *reportColumn(unsigned int);
  const MSTableColumn *reportColumn(const MSSymbol&) const;
  const MSTableColumn *reportColumn(unsigned int) const;
  unsigned numRows(void) const;
  virtual unsigned numColumns(void) const;
  virtual unsigned dataRows(void) const; 
  virtual unsigned numRows(unsigned) const; 
  virtual void exchangeColumns(unsigned int,unsigned int);
  virtual void placeColumnAt(MSTableColumn *,unsigned);
  virtual void permuteColumns(const MSIndexVector& aIndexVector_);
  virtual void permuteColumns(const MSSymbolVector& aSymbolVector_);  
  virtual void permuteColumns(const MSWidgetVector& aColumnVector_);  
  virtual MSIndexVector sortUp(const MSIndexVector &);
  virtual MSIndexVector sortUp(const MSSymbolVector &);
  virtual MSIndexVector sortDown(const MSIndexVector &);
  virtual MSIndexVector sortDown(const MSSymbolVector &);
  virtual const char *formatOutput(MSString &,unsigned,unsigned);

  void headingForeground(unsigned long);
  void headingForeground(const char *);
  //
  // reporting attributes
  //
  MSReport* report(void);
  MSIndexVector& breakTextColumn(void)                      {return _breakTextColumn;}
  MSIndexVector& breakColumn(void)                          {return _breakColumn;}
  const MSSymbol& tag(void)                           const {return _tag;}
  MSBoolean reportTotalOn(void)                       const {return _reportTotalOn;}
  MSBoolean reportGrandTotalOn(void)                  const {return _reportGrandTotalOn;}
  MSBoolean pageTotalOn(void)                         const {return _pageTotalOn;}
  MSFloat& grandTotal(void)                                 {return _grandTotal;}
  MSParagraph& grandTotalText(void)                         {return _grandTotalText;}
  const MSFloat& grandTotal(void)                     const {return _grandTotal;}
  const MSParagraph& grandTotalText(void)             const {return _grandTotalText;}
  const MSStringVector& reportTitle(void)             const {return _reportTitle;}
  const MSString& reportTotalFont(void)               const {return _reportTotalFont;}
  const MSString& breakFont(void)                     const {return _breakFont;}
  const MSIndexVector& breakIndex(void)               const {return _breakIndex;}
  MSIndexVector& breakIndex(void)                           {return _breakIndex;}
  MSIndexVector& pageBreakIndex(void)                       {return _pageBreakIndex;}
  unsigned long frameStyle(void)                      const {return _frameStyle;}
  int frameOffset(void)                               const {return _frameOffset;}
  int frameLineWidth(void)                            const {return _frameLineWidth;}
  unsigned long style(void)                           const {return _style;}
  unsigned long reportTotalStyle(void)                const {return _reportTotalStyle;}
  unsigned long breakStyle(void)                      const {return _breakStyle;}
  unsigned long headingStyle(void)                    const {return _headingStyle;}
  unsigned long headingForeground(void)               const {return _headingForeground;}
  double fgGrayScale(void)                            const {return _fgGrayScale;}
  double bgGrayScale(void)                            const {return _bgGrayScale;}
  double headingFgGrayScale(void)                     const {return _headingFgGrayScale;}
  double headingBgGrayScale(void)                     const {return _headingBgGrayScale;}
  int breakOffset(void)                               const {return _breakOffset;}
  int breakLeading(void)                              const {return _breakLeading;}
  int reportTotalLeading(void)                        const {return _reportTotalLeading;}
  int reportHeadingOffset(void)                       const {return _reportHeadingOffset;}
  const MSFloatVector& reportColumnSpacing(void)      const {return _reportColumnSpacing;}
  const MSString& reportHeadingFont(void)             const {return _reportHeadingFont;}

  virtual int fixedReportColumns(void) const;
  virtual MSDisplayServer *displayServer(void);
  virtual void generateReport(const char *file_=0); 
  virtual void computeBreaks(void);
  virtual void updateBreaks(void);
  virtual void updateBreaks(unsigned);

  void tag(const MSSymbol& x_)                         {_tag=x_;}
  void fixedReportColumns(int x_)                      {_fixedReportColumns=x_;}
  void reportTotalOn(MSBoolean x_)                     {_reportTotalOn=x_;}
  void reportGrandTotalOn(MSBoolean x_)                {_reportGrandTotalOn=x_;}
  void pageTotalOn(MSBoolean x_)                       {_pageTotalOn=x_;}
  void reportTitle(const MSStringVector& x_)           {_reportTitle=x_;}
  void reportTotalFont(const char *x_)                 {_reportTotalFont=x_;}
  void reportHeadingFont(const char *x_)               {_reportHeadingFont=x_;}
  void breakFont(const char *x_)                       {_breakFont=x_;}
  void frameOffset(unsigned x_)                        {_frameOffset=x_;}
  void frameLineWidth(unsigned x_)                     {_frameLineWidth=x_;}
  void frameStyle(unsigned long x_)                    {_frameStyle=x_;}
  void headingStyle(unsigned long x_)                  {_headingStyle=x_;}
  void reportTotalStyle(unsigned long x_)              {_reportTotalStyle=x_;}
  void breakStyle(unsigned long x_)                    {_breakStyle=x_;}
  void reportTotalLeading(int x_)                      {_reportTotalLeading=x_;}
  void reportHeadingOffset(unsigned x_)                {_reportHeadingOffset=x_;}
  void breakOffset(int x_)                             {_breakOffset=x_;}
  void breakLeading(int x_)                            {_breakLeading=x_;}
  void fgGrayScale(double x_)                          {if (x_>=0.0&&x_<=1.0) _fgGrayScale=x_;}
  void bgGrayScale(double x_)                          {if (x_>=0.0&&x_<=1.0) _bgGrayScale=x_;}
  void headingFgGrayScale(double x_)                   {_headingFgGrayScale=x_;}
  void headingBgGrayScale(double x_)                   {_headingBgGrayScale=x_;}
  MSParagraph& grandTotalText(MSParagraph& x_)         {_grandTotalText=x_; return _grandTotalText;}
  MSParagraph& grandTotalText(MSStringVector& x_)      {_grandTotalText=x_; return _grandTotalText;}
  MSParagraph& grandTotalText(const char *x_)          {_grandTotalText=x_; return _grandTotalText;}
  void report(MSReport *x_)                            {_report=x_;}
  void reportColumnSpacing(const MSFloatVector& x_)    {_reportColumnSpacing=x_;}

  const MSParagraph& defaultText(void)          const {return _defaultText;}
  MSParagraph& defaultText(void)                      {return _defaultText;}

  MSBoolean scaleHeaders(void)                  const { return _scaleHeaders;}
  void scaleHeaders(MSBoolean scaleHeaders_)          { _scaleHeaders=scaleHeaders_;}

  // Group Headings 
  //
  void removeGroupHeading(const MSSymbol&);
  void removeAllGroupHeadings(void);
  MSParagraph& addGroupHeading(const MSParagraph&);
  MSParagraph& addGroupHeading(const MSStringVector&);

  MSParagraph& reportGroupHeading(const MSSymbol&);
  const MSParagraph& reportGroupHeading(const MSSymbol&) const;


  //
  // row/column/page control
  //
  const MSUnsignedVector& leading(void)         const {return _leading;}
  unsigned long outputStyle(void)               const {return _outputStyle;}
  unsigned widowRows(void)                      const {return _widowRows;}
  unsigned orphanRows(void)                     const {return _orphanRows;}
  unsigned rowPageSpan(void)                    const {return _rowPageSpan;}
  unsigned columnPageSpan(void)                 const {return _columnPageSpan;}
  const MSUnsignedVector& rowControl(void)      const {return _rowControl;}
  const MSUnsignedVector& columnControl(void)   const {return _columnControl;}
  unsigned newspaperColumns(void)               const {return _newspaperColumns;}
  void leading(const MSUnsignedVector& x_)            {_leading=x_;}
  void outputStyle(MSP::OutputStyle x_)               {_outputStyle=x_;}
  void outputStyle(unsigned long x_)                  {_outputStyle=x_;}
  void rowPageSpan(unsigned x_)                       {_rowPageSpan=x_;}
  void columnPageSpan(unsigned x_)                    {_columnPageSpan=x_;}
  void rowControl(const MSUnsignedVector& x_)         {_rowControl=x_;}
  void columnControl(const MSUnsignedVector& x_)      {_columnControl=x_;}
  void newspaperColumns(unsigned x_)                  {_newspaperColumns=x_>0?x_:1;}
  // 
  // internal values for diagnostics
  // 
  const MSIndexVector& columnIndex(void)        const {return _columnIndex;}
  const MSIndexVector& columnPageBreak(void)    const {return _columnPageBreak;}
  const MSIndexVector& rowPageBreak(void)       const {return _rowPageBreak;}
  const MSIndexVector& stackPageBreak(void)     const {return _stackPageBreak;}
  const MSIndexVector& tableWidths(void)        const {return _tableWidths;}
  // 
  // for printing with MSPrintTool
  // 
  virtual const MSSymbol& printTag(void) const;
  void computeTableRowColumnSize(void);
  void setTableParameters(void);
  void computeGroupHeadingSize(void);
  void computeTableHeadingSize(void);
  void computeColumnPartitions(void);
  void computeRowColumnPartitions(void);
  void computeRowPartitions(double);
  void computeRowPartitions(int,double);
  void computeXFontScale(int);
  void computeYFontScale(int);
  void computePageIndices(int,int&,int&);
  int computeTableSegmentHeight(int,int);
  void computeTableHeaderSize(int);
  void computeTableFooterSize(int);
  void computeBreakTextSizes(void);
  void computeBreakTextSize(MSParagraph&);
  int tableHeaderHeight(void);
  int tableFooterHeight(void);

  int tablePageSize(int);
  int setTableX(int,int);
  int setTableY(int,int);
  int columnsPerPage(int);
  int rowsPerPage(int);
  int leading(int);
  int reportColumnSpacing(int);

  void printCellBox(unsigned long,int,int,int,int);
  void printTableHeadings(int,int);
  void printTableRows(int,int,int,int);
  void printTableFrame(int,int,int,int);
  void printBreakText(MSParagraph&,int);
  void printTableHeaders(int,int);
  void printTableFooters(int,int);
  int printTableSegment(int,int);
  void printTableScale(int,int);
  void printASCIITable(void);

  void printGroupHeadings(int,int);
  void printGroupHeadingBox(unsigned long,int,int,int,int,int);
  void sortGroupHeadingsByColumn(void);
  void sortTextListByRow(MSPointerArray<MSParagraph>&);
  
  MSReportTable& leftMargin(double);
  MSReportTable& rightMargin(double);
  MSReportTable& topOffset(double);
  MSReportTable& bottomOffset(double);
  MSReportTable& topPixel(unsigned);
  MSReportTable& bottomPixel(unsigned);
  MSReportTable& justification(MSAlignment);
  MSReportTable& justification(unsigned long);
  MSReportTable& printRow(int);
  MSReportTable& printColumn(int);
  MSReportTable& reportFont(const char *);
  MSReportTable& style(unsigned long);
  MSReportTable& widowRows(unsigned);
  MSReportTable& orphanRows(unsigned);
  MSReportTable& pageAlignment(MSAlignment);
  MSReportTable& pageAlignment(unsigned long);
  double leftMargin(void) const;
  double rightMargin(void) const;
  double topOffset(void) const;
  double bottomOffset(void) const;
  unsigned topPixel(void) const;
  unsigned bottomPixel(void) const;
  unsigned long justification(void) const;
  int printRow(void) const;
  int printColumn(void) const;
  const MSString& reportFont(void) const;
  unsigned long pageAlignment(void) const {return MSPrintItem::pageAlignment();}

  int tableLeft(void)               const {return _tableLeft;}
  int tableRight(void)              const {return _tableRight;}
  int tableTop(void)                const {return _tableTop;}
  int tableBottom(void)             const {return _tableBottom;}
  int pageWidth(void)               const {return _pageWidth;}
  int pageHeight(void)              const {return _pageHeight;}
  void tableLeft(int x_)                  {_tableLeft=x_;}
  void tableRight(int x_)                 {_tableRight=x_;}
  void tableTop(int x_)                   {_tableTop=x_;}
  void tableBottom(int x_)                {_tableBottom=x_;}
  void pageWidth(int x_)                  {_pageWidth=x_;}
  void pageHeight(int x_)                 {_pageHeight=x_;}
  int tableTopPosition(int);
  int tableBottomPosition(int);

  const MSString& delimiter(void)                           const {return _delimiter;}
  void delimiter(const MSString& x_)                              {_delimiter=x_;}

protected:

  void init(void);
  void clearBreaks(void);
  void resetColumnLocations();  
  void maxRowsClear(void);
  void maxRowsSet(unsigned); 

  virtual void updateScreen(void);
  virtual void breakProcess(MSIndexVector&);
  virtual void computeColumnTotals(void);
  virtual void addColumn(MSTableColumn *);
  virtual void removeColumn(MSTableColumn *);
  virtual unsigned long convertForeground(const char *);
  virtual unsigned long style(unsigned,unsigned);
  virtual unsigned long breakStyle(unsigned,unsigned);
  virtual int breakOffset(unsigned,unsigned);
  virtual int breakLeading(unsigned,unsigned);
  virtual const char *reportFont(unsigned,unsigned);
  virtual const char *breakFont(unsigned,unsigned);
  virtual double fgGrayScale(unsigned,unsigned);
  virtual double bgGrayScale(unsigned,unsigned);

  MSParagraph *groupHeading(unsigned);
  const MSParagraph *groupHeading(unsigned) const;

  MSParagraph& grandTotalBaseText(void)                           {return *_grandTotalBaseText;}
  MSPointerArray<MSTableColumn> *columnList(void)           const {return _columnList;}
  MSPointerArray<MSTableColumn> *columnList(void)                 {return _columnList;}
  MSPointerArray<MSTableColumn> *hiddenColumnList(void)     const {return _hiddenColumnList;}
  MSPointerArray<MSTableColumn> *hiddenColumnList(void)           {return _hiddenColumnList;}

  MSPointerArray<MSParagraph> *breakTextList(void)          const {return _breakTextList;}
  MSPointerArray<MSParagraph> *breakTextList(void)                {return _breakTextList;}
  MSPointerArray<MSParagraph> *groupHeadingList(void)       const {return _groupHeadingList;}
  MSPointerArray<MSParagraph> *groupHeadingList(void)             {return _groupHeadingList;}
  int headingHeight(void)                                   const {return _headingHeight;}
  int orphanRowHeight(void)                                 const {return _orphanRowHeight;}

  MSIndexVector& columnIndex(void)                                {return _columnIndex;}   
  MSIndexVector& columnPageBreak(void)    	                  {return _columnPageBreak;}
  MSIndexVector& rowPageBreak(void)       	                  {return _rowPageBreak;}  
  MSIndexVector& stackPageBreak(void)       	                  {return _stackPageBreak;}  
  MSIndexVector& tableWidths(void)       	                  {return _tableWidths;}  
  MSIntVector& rowHeights(void)                                   {return _rowHeights;}   
  MSIntVector& rowLeadings(void)                                  {return _rowLeadings;}   
  MSIntVector& columnWidths(void)                                 {return _columnWidths;}   
  MSIntVector& groupHeadingHeights(void)                          {return _groupHeadingHeights;}
  int groupHeadingHeight(void)                                    {return _groupHeadingHeight;}
  void groupHeadingHeight(int x_)                                 {_groupHeadingHeight=x_;}
  void headingHeight(int x_)                                      {_headingHeight=x_;}
  void orphanRowHeight(int x_)                                    {_orphanRowHeight=x_;}
  double x_printScale(void)                                       {return _x_printScale;}
  double y_printScale(void)                                       {return _y_printScale;}
  void x_printScale(double x_)                                    {_x_printScale=x_;}
  void y_printScale(double x_)                                    {_y_printScale=x_;}

  int pageCountTotal(void)                                  const {return _pageCountTotal;}
  int totalSegments(void)                                   const {return _totalSegments;}
  int segment(void)                                         const {return _segment;}
  int stackCount(void)                                      const {return _stackCount;}
  int lastPageCount(void)                                   const {return _lastPageCount;}
  void pageCountTotal(int x_)                                     {_pageCountTotal=x_;}
  void totalSegments(int x_)                                      {_totalSegments=x_;}
  void segment(int x_)                                            {_segment=x_;}
  void stackCount(int x_)                                         {_stackCount=x_;}
  void lastPageCount(int x_)                                      {_lastPageCount=x_;}

  //////////////////////////////////////////////////////////////////////////
  //
  // table report methods
  //
  //////////////////////////////////////////////////////////////////////////

  int breakCount(void)                          const {return _breakCount;}
  int breakTextCount(void)                      const {return _breakTextCount;}
  int columnCount(void)                         const {return _columnCount;}
  int rowCount(void)                            const {return _rowCount;}
  int numFixedFields(void)                      const {return _numFixedFields;}
  int fixedFieldWidth(void)                     const {return _fixedFieldWidth;}

  void breakCount(int x_)                             {_breakCount=x_;}
  void breakTextCount(int x_)                         {_breakTextCount=x_;}
  void columnCount(int x_)                            {_columnCount=x_;}
  void rowCount(int x_)                               {_rowCount=x_;}
  void numFixedFields(int x_)                         {_numFixedFields=x_;}
  void fixedFieldWidth(int x_)                        {_fixedFieldWidth=x_;}

  MSIndexVector& breakTextIndex(void)                 {return _breakTextIndex;}
  MSParagraph *breakText(unsigned i_)                 {return breakTextList()->array(i_);}
  void breakText(MSParagraph *x_); 


 
  unsigned long      _headingForeground;
  int                _fixedReportColumns;
private:
  MSIndexVector      _breakTextIndex;
  int                _breakCount;
  int                _breakTextCount;
  int                _rowCount;
  int                _columnCount;
  int                _numFixedFields;
  int                _fixedFieldWidth;
  int                _lastPageCount;
  MSString           _delimiter;

  MSReport          *_report;
  MSParagraph        _grandTotalText;
  MSParagraph       *_grandTotalBaseText;
  MSParagraph        _defaultText;
  MSFloat            _grandTotal;
  MSSymbol           _tag;
  unsigned           _numRows;
  unsigned long      _style;
  unsigned long      _breakStyle;
  int                _breakOffset;
  int                _breakLeading;
  double             _fgGrayScale;
  double             _bgGrayScale;
  double             _headingFgGrayScale;
  double             _headingBgGrayScale;
  MSString           _reportTotalFont;
  MSString           _breakFont;
  MSString           _reportHeadingFont;
  MSStringVector     _reportTitle;
  unsigned long      _headingStyle;
  unsigned long      _reportTotalStyle;
  MSFloatVector      _reportColumnSpacing;
  MSIndexVector      _columnIndex;
  MSIndexVector      _columnPageBreak;
  MSIndexVector      _rowPageBreak;
  MSIndexVector      _stackPageBreak;
  MSIndexVector      _tableWidths;
  MSIndexVector      _breakIndex;
  MSIndexVector      _pageBreakIndex;
  MSIndexVector      _breakColumn;
  MSIndexVector      _breakTextColumn;
  MSBoolean          _reportTotalOn;
  MSBoolean          _reportGrandTotalOn;
  MSBoolean          _pageTotalOn;
  int                _reportTotalLeading;
  int                _headingHeight;
  int                _orphanRowHeight;
  int                _reportHeadingOffset;
  MSIntVector        _rowHeights;
  MSIntVector        _columnWidths;
  MSIntVector        _rowLeadings;
  MSIntVector        _groupHeadingHeights;
  int                _groupHeadingHeight;

  MSUnsignedVector   _rowControl;
  MSUnsignedVector   _columnControl;
  MSUnsignedVector   _leading;
  unsigned           _widowRows;
  unsigned           _orphanRows;
  unsigned           _rowPageSpan;
  unsigned           _columnPageSpan;
  unsigned           _newspaperColumns;
  unsigned           _frameOffset;
  unsigned 	     _frameLineWidth;
  unsigned long      _frameStyle;
  unsigned long      _outputStyle;
  double 	     _x_printScale;
  double	     _y_printScale;

  int                _tableLeft;
  int                _tableRight;
  int                _tableTop;
  int                _tableBottom;
  int                _pageWidth;
  int                _pageHeight;
  int                _segment;
  int                _totalSegments;
  int                _pageCountTotal;
  int                _stackCount;
  int                _printPage;

  MSBoolean          _scaleHeaders;
  int                _topMargin;
  
  MSPointerArray<MSParagraph>   *_groupHeadingList;
  MSPointerArray<MSParagraph>   *_breakTextList;
  MSPointerArray<MSTableColumn> *_columnList;
  MSPointerArray<MSTableColumn> *_hiddenColumnList; 
};

inline MSParagraph *MSReportTable::groupHeading(unsigned i_)
{return (groupHeadingList()!=0)?groupHeadingList()->array(i_):0;}
inline const MSParagraph *MSReportTable::groupHeading(unsigned i_) const
{return (groupHeadingList()!=0)?groupHeadingList()->array(i_):0;}
inline void MSReportTable::maxRowsClear(void)
{_numRows=0;}
inline unsigned MSReportTable::numRows(void) const
{return _numRows;}

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSPointerArray<MSParagraph>
#pragma do_not_instantiate MSPointerArray<MSTableColumn>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif



