#ifndef MSPrintItemHEADER
#define MSPrintItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSString.H>
#include <MSGUI/MSGUIEnum.H>
#include <MSGUI/MSP.H>

class MSReport;
class MSPrintManager;

class MSGUIExport MSPrintItem : public MSP
{
public:
  MSPrintItem(void);
  MSPrintItem(const MSPrintItem&);
  virtual ~MSPrintItem(void);

  MSPrintItem& operator=(const MSPrintItem&);

  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);
  virtual const MSSymbol& printTag(void) const;
  
  double leftMargin(void) const;
  void   leftMargin(double);

  double rightMargin(void) const;
  void   rightMargin(double);

  double topOffset(void) const;
  void   topOffset(double);

  double bottomOffset(void) const;
  void   bottomOffset(double);

  const MSString& printFont(void)           const {return _printFont;}
  void            printFont(const char *x_)       {_printFont=x_;}

  int  currentPage(void)                    const {return _currentPage;}
  void currentPage(int x_)                        {_currentPage=x_;}

  int  pageCount(void)                      const {return _pageCount;}
  void pageCount(int x_)                          {_pageCount=x_;}

  int  residual(void)                       const {return _residual;}
  void residual(int x_)                           {_residual=x_;}

  int  printRow(void)                       const {return _printRow;}
  void printRow(int x_)                           {_printRow=x_;} 

  int  printColumn(void)                    const {return _printColumn;}
  void printColumn(int x_)                        {_printColumn=x_;} 

  int  printHeight(void)                    const {return _printHeight;}
  void printHeight(int x_)                        {_printHeight=x_;}

  int  pageBreakRow(void)                   const {return _pageBreakRow;} 
  void pageBreakRow(int x_)                       {_pageBreakRow=x_;} 

  unsigned long occurrence(void)            const {return _occurrence;}
  void occurrence(unsigned long x_)               {_occurrence=x_;}
  void occurrence(Occurrence x_)                  {_occurrence=x_;}

  MSBoolean isDestroyable(void)             const {return _destroyable;}
  void destroyable(MSBoolean x_)                  {_destroyable=x_;}

  MSBoolean changed(void)                   const {return _changed;}
  void changed(MSBoolean x_)                      {_changed=x_;}

  unsigned long pageAlignment(void)         const {return _pageAlignment;}
  void pageAlignment(unsigned long x_)            {_pageAlignment=x_;}

  unsigned long justification(void)         const {return _justification;}
  void justification(MSAlignment x_)              {_justification=x_;}
  void justification(unsigned long x_)            {_justification=x_;}

  void currentPageInc(void)                       {_currentPage++;}

  int  leftPixel(void)                      const {return _leftPixel;} 
  void leftPixel(unsigned x_)                     {_leftPixel=x_;}

  int  rightPixel(void)                     const {return _rightPixel;} 
  void rightPixel(unsigned x_)                    {_rightPixel=x_;}

  int  topPixel(void)                       const {return _topPixel;}
  void topPixel(unsigned x_)                      {_topPixel=x_;}

  int  bottomPixel(void)                    const {return _bottomPixel;}
  void bottomPixel(unsigned x_)                   {_bottomPixel=x_;}

  void printManager(MSPrintManager *x_)           {_printManager=x_;}
  MSPrintManager *printManager(void)              {return _printManager;}

protected:
  void reset(void);

  MSPrintManager  *_printManager;
  MSBoolean        _destroyable;
  MSBoolean        _changed;
  int		   _printRow;
  int		   _printColumn;
  int		   _leftPixel;
  int		   _rightPixel;
  int              _topPixel;
  int     	   _bottomPixel;
  int		   _pageBreakRow;
  int              _printHeight;
  unsigned long    _pageAlignment;
  unsigned long	   _occurrence;
  unsigned long    _justification;
  MSString         _printFont;
  int              _currentPage;
  int              _pageCount;
  int              _residual;
};

class MSGUIExport MSPageBreakPrintItem : public MSPrintItem
{
public:
  MSPageBreakPrintItem(void);
  ~MSPageBreakPrintItem(void);

  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);
};

class MSGUIExport MSRulePrintItem : public MSPrintItem
{
public:
  MSRulePrintItem(unsigned ruleWidth_=0);
  ~MSRulePrintItem(void);
  void ruleWidth(unsigned x_)          {if (x_<100) _ruleWidth=x_;}
  void fgGrayScale(double x_)          {if (x_>=0.0&&x_<=1.0) _fgGrayScale=x_;}
  unsigned ruleWidth(void)       const {return _ruleWidth;}
  double fgGrayScale(void)       const {return _fgGrayScale;}

  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);
  void printRulePrintItem(MSReport*,int,int,int);
private:
  int    _ruleWidth;
  double _fgGrayScale;
};

#endif

