#ifndef MSParagraphHEADER
#define MSParagraphHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <X11/Xlib.h>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSUnsignedVector.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSIntVector.H>
#include <MSGUI/MSPrintItem.H>

class MSParagraph;

typedef class MSParagraph MSPrintText;

class MSReport;
class MSPostScript;
class MSReportTable;

class MSGUIExport MSParagraph : public MSPrintItem
{
friend class MSReport;
friend class MSReportTable;   //hack that should removed with redesign.
public:
  MSParagraph(const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSParagraph(const MSStringVector&,int row_,int col_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSParagraph(const MSStringVector&,const char *font_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSParagraph(const MSStringVector&,int row_,int col_,const char *font_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSParagraph(const MSParagraph&);
  MSParagraph(void);
  ~MSParagraph(void);

  MSParagraph& operator=(const MSParagraph&);

  void operator=(const MSStringVector&);
  void operator=(const MSString&);
  void operator=(const char *);
  operator const MSStringVector&() const;

  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);

  MSParagraph& firstLineIndent(double);
  MSParagraph& leftMargin(double);
  MSParagraph& rightMargin(double);
  MSParagraph& text(const MSStringVector&);
  MSParagraph& tag(const MSSymbol&);
  MSParagraph& justification(MSAlignment);
  MSParagraph& justification(unsigned long);
  MSParagraph& leading(unsigned);
  MSParagraph& topPixel(unsigned);
  MSParagraph& bottomPixel(unsigned);
  MSParagraph& lineWidth(unsigned);
  MSParagraph& printRow(int);
  MSParagraph& printColumn(int);
  MSParagraph& fgGrayScale(double);
  MSParagraph& bgGrayScale(double);
  MSParagraph& font(const char *);
  MSParagraph& style(unsigned long);
  MSParagraph& xOrigin(double);
  MSParagraph& yOrigin(double);
  MSParagraph& topOffset(double);
  MSParagraph& bottomOffset(double);
  MSParagraph& foreground(const char* );
  MSParagraph& background(const char *);
  MSParagraph& fontSize(unsigned);
  MSParagraph& fontScale(double);
  MSParagraph& fontID(Font);

  MSParagraph& firstColumn(unsigned);
  MSParagraph& columnSpan(unsigned);
  MSParagraph& occurrence(unsigned long);
  MSParagraph& occurrence(Occurrence);
  MSParagraph& pageAlignment(MSAlignment);
  MSParagraph& pageAlignment(unsigned long);
  MSParagraph& orphanRows(unsigned);
  MSParagraph& widowRows(unsigned);

  //obsolete. left for backward compatibility
  MSParagraph& row(int);
  MSParagraph& column(int);
  int row(void)                                 const {return MSPrintItem::printRow();}
  int column(void)                              const {return MSPrintItem::printColumn();}

  const MSStringVector& text(void)              const {return _text;}
  const MSString& fontName(void)                const {return printFont();}
  const MSString& foreground(void)              const {return _foreground;}
  const MSString& background(void)              const {return _background;}
  const MSSymbol& tag(void)                     const {return _tag;}
  unsigned long style(void)                     const {return _style|justification();}
  unsigned widowRows(void)                      const {return _widowRows;}
  unsigned orphanRows(void)                     const {return _orphanRows;}
  int fontSize(void)                            const {return _fontSize;}
  int leading(void)                             const {return _leading;}
  double fontScale(void)                        const {return _fontScale;}
  double fgGrayScale(void)                      const {return _fgGrayScale;}
  double bgGrayScale(void)                      const {return _bgGrayScale;}
  int xPixel(void)                              const {return _xPixel;}
  int yPixel(void)                              const {return _yPixel;}
  int lineWidth(void)                           const {return _lineWidth;}
  int topPixel(void)                            const {return MSPrintItem::topPixel();}
  int bottomPixel(void)                         const {return MSPrintItem::bottomPixel();}
  int printRow(void)                            const {return MSPrintItem::printRow();}
  int printColumn(void)                         const {return MSPrintItem::printColumn();}
  unsigned long occurrence(void)                const {return MSPrintItem::occurrence();}
  unsigned long justification(void)             const {return MSPrintItem::justification();}
  unsigned long pageAlignment(void)             const {return MSPrintItem::pageAlignment();}
  int columnSpan(void)                          const {return _columnSpan;}
  Font fontID(void)                                   {return _fontID;}
  virtual int height(void) const;
  double xOrigin(void) const;
  double yOrigin(void) const;
  double topOffset(void) const;
  double bottomOffset(void) const;
  
  double firstLineIndent(void) const;
  double leftMargin(void) const;
  double rightMargin(void) const;

protected:
  int firstLineIndentPixel(void)                const {return _firstLineIndentPixel;} 
  int lastLineWidth(void)                       const {return _lastLineWidth;} 
  int textLineWidth(void)                       const {return _textLineWidth;} 
  const MSStringVector& outputText(void)        const {return _outputText;}
  MSStringVector& outputText(void)                    {return _outputText;}
  MSFloatVector& wordSpacing(void)                    {return _wordSpacing;}

  void firstLineIndentPixel(int x_)                   {_firstLineIndentPixel=x_;} 
  void lastLineWidth(int x_)                          {_lastLineWidth=x_;} 
  void textLineWidth(int x_)                          {_textLineWidth=x_;} 

  virtual const MSSymbol& printTag(void) const;

  void computeParagraphSize(MSReport*);
  void printParagraph(MSReport*,int,int,int);
  void printParagraphDecorations(MSReport*,int,int,int,int,int);
  double dissectWord(MSReport*,MSString&,int,double);
  MSString wordToNext(const MSString&,unsigned) const;

  void init(void);
  
private:
  MSFloatVector      _wordSpacing;
  MSStringVector     _outputText;
  MSStringVector     _text;
  MSSymbol           _tag;
  MSString           _foreground;
  MSString           _background;
  unsigned long	     _style;
  unsigned           _widowRows;
  unsigned           _orphanRows;
  Font  	     _fontID;
  int   	     _fontSize;
  int   	     _leading;
  int		     _firstLineIndentPixel;
  int                _lastLineWidth;
  int                _textLineWidth;

  int    	     _xPixel;
  int   	     _yPixel;
  int   	     _lineWidth;
  double   	     _fontScale;
  double             _fgGrayScale;
  double             _bgGrayScale;
  int   	     _columnSpan;
};

#endif

