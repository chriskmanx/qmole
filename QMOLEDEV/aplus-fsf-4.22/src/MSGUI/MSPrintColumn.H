#ifndef MSPrintColumnHEADER
#define MSPrintColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSPrintItem.H>
#include <MSGUI/MSPrintManager.H>

class MSReportTable;
class MSPrintHeaders;

class MSGUIExport MSPrintColumn : public MSPrintItem, public MSPrintManager
{
friend class MSPrintHeaders;
public:
  MSPrintColumn(MSPrintManager *,unsigned numColumns_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSPrintColumn(MSPrintManager *,MSFloatVector& columnWidths_,unsigned numColumns_=1,
		const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSPrintColumn(unsigned numColumns_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSPrintColumn(MSFloatVector& columnWidths_,unsigned numColumns_=1,
		const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSPrintColumn();

  void init(void);
  const MSSymbol& tag(void)                     const {return _tag;}
  const MSFloatVector& columnWidths(void)       const {return _columnWidths;}
  unsigned long justification(void)             const {return MSPrintItem::justification();}
  const MSString& fontName(void)                const {return printFont();}
  unsigned long occurrence(void)                const {return MSPrintItem::occurrence();}
  unsigned long pageAlignment(void)             const {return MSPrintItem::pageAlignment();}
  unsigned long style(void)                     const {return _style;}
  unsigned leftIndent(void)                     const {return _leftIndent;}
  unsigned rightIndent(void)                    const {return _rightIndent;}
  unsigned indents(void)                        const {return _leftIndent+_rightIndent;}
  unsigned topPixel(void)                       const {return MSPrintItem::topPixel();}
  unsigned bottomPixel(void)                    const {return MSPrintItem::bottomPixel();}
  int printRow(void)                            const {return MSPrintItem::printRow();}
  int printColumn(void)                         const {return MSPrintItem::printColumn();}
  unsigned numColumns(void)                     const {return _numColumns;}
  MSBoolean headingsOn(void)                    const {return _headingsOn;}
  
  MSPrintColumn& tag(const MSSymbol&);
  MSPrintColumn& columnWidths(const MSFloatVector&);
  MSPrintColumn& justification(MSAlignment);
  MSPrintColumn& justification(unsigned long);
  MSPrintColumn& leftIndent(unsigned);
  MSPrintColumn& rightIndent(unsigned);
  MSPrintColumn& topPixel(unsigned);
  MSPrintColumn& bottomPixel(unsigned);
  MSPrintColumn& numColumns(unsigned);
  MSPrintColumn& printRow(int);
  MSPrintColumn& printColumn(int);
  MSPrintColumn& occurrence(unsigned long);
  MSPrintColumn& occurrence(Occurrence);
  MSPrintColumn& pageAlignment(unsigned long);
  MSPrintColumn& pageAlignment(MSAlignment);
  MSPrintColumn& font(const char *);
  MSPrintColumn& style(unsigned long);
  MSPrintColumn& topOffset(double);
  MSPrintColumn& bottomOffset(double);
  MSPrintColumn& leftMargin(double);
  MSPrintColumn& rightMargin(double);
  MSPrintColumn& headingsOn(MSBoolean);

  //obsolete. left for backward compatibility
  MSPrintColumn& row(int);
  MSPrintColumn& column(int);
  int row(void)                                 const {return printRow();}
  int column(void)                              const {return printColumn();}

  double topOffset(void) const;
  double bottomOffset(void) const;
  double leftMargin(void) const;
  double rightMargin(void) const;

protected:
  unsigned columnPixel(unsigned);
  unsigned columnPixelWidth(unsigned);
  void computeChildrenSize(MSReport*,int,int,int,int,int,int);
  void computeColumnWidths(void);
  void sortPrintItemsByRowColumn(void);
  void computeConfiguration(void);
  unsigned rowCount(unsigned);
  MSPrintHeaders *owner(void)                         {return _owner;}
  MSUnsignedVector& rowPageCounts(void)               {return _rowPageCounts;}
  MSUnsignedVector& rowHeights(void)                  {return _rowHeights;}
  MSUnsignedVector& columnPixelWidth(void)            {return _columnPixelWidth;}
  MSUnsignedVector& columnRowCounts(void)             {return _columnRowCounts;}
  int residualHeight(void)                      const {return _residualHeight;} 
  unsigned fixedRowCount(void)                  const {return _fixedRowCount;}
  unsigned columnCount(void)                    const {return _columnCount;}
  unsigned pixelWidth(void)                     const {return _pixelWidth;}
  unsigned pixelHeight(void)                    const {return _pixelHeight;}
  void owner(MSPrintHeaders *x_)                      {_owner=x_;} 
  void residualHeight(int x_)                         {_residualHeight=x_;} 
  void fixedRowCount(unsigned x_)                     {_fixedRowCount=x_;}
  void columnCount(unsigned x_)                       {_columnCount=x_;}
  void pixelWidth(unsigned x_)                        {_pixelWidth=x_;}
  void pixelHeight(unsigned x_)                       {_pixelHeight=x_;}

  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);
  virtual const MSSymbol& printTag(void) const;
  
private:
  MSPrintHeaders      *_owner;
  MSSymbol             _tag;
  MSFloatVector        _columnWidths;
  MSUnsignedVector     _rowPageCounts;
  MSUnsignedVector     _rowHeights;
  MSUnsignedVector     _columnPixelWidth;
  MSUnsignedVector     _columnRowCounts;
  MSBoolean            _headingsOn;
  unsigned long	       _style;
  unsigned   	       _pixelWidth;
  unsigned   	       _pixelHeight;
  unsigned   	       _numColumns;
  unsigned   	       _leftIndent;
  unsigned   	       _rightIndent;
  unsigned   	       _fixedRowCount;
  unsigned   	       _columnCount;
  int		       _residualHeight;
};

#endif
