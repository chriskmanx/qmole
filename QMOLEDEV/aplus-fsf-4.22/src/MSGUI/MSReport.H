#ifndef MSReportHEADER
#define MSReportHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloat.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSUnsignedVector.H>
#include <MSTypes/MSCallback.H>
#include <MSTypes/MSNodeList.H>
#include <MSGUI/MSGUIEnum.H>
#include <MSGUI/MSRect.H>
#include <MSGUI/MSPostScript.H>
#include <MSGUI/MSPrintHeaders.H>
#include <MSGUI/MSPrintManager.H>

typedef void (*MSReportCallbackFunc)(void*, void*);

class MSReportCallbackNode;

class MSGUIExport MSReport : public MSPostScript, public MSPrintHeaders, public MSPrintManager 
{
public:
  MSReport(void);
  ~MSReport(void);
  virtual void cancelReport(void)                     {_cancelReport=MSTrue;}
  void reset(void);
  void print(const char *file_=0);
  void computeOutputSize(void);

  // callbacks 
  static MSSymbol pagebreak;
  static MSSymbol computesize;

  // defaults inherited by children
  
  void defaultFont(const MSString& x_);
  int defaultFontSize(void)                     const {return _defaultFontSize;}
  Font defaultFontID(void)                      const {return _defaultFontID;}
  const MSString& defaultFont(void)             const {return _defaultFont;}
  
  const MSFloatVector& tabStop(void)            const {return _tabStop;}
  void tabStop(const MSFloatVector& x_)               {_tabStop=x_;}
  //
  // Style methods
  //
  unsigned long pageFrameStyle(void)            const {return _pageFrameStyle;}
  MSBoolean uniformScaling(void)                const {return _uniformScaling;}
  MSFloat::MSFloatFormat asciiFloatFormat(void) const {return (MSFloat::MSFloatFormat)_asciiFloatFormat;}
  MSInt::MSIntFormat asciiIntFormat(void)       const {return (MSInt::MSIntFormat)_asciiIntFormat;}
  void pageFrameStyle(unsigned long x_)               {_pageFrameStyle=x_;}
  void uniformScaling(MSBoolean x_)                   {_uniformScaling=x_;}
  void asciiFloatFormatClear(void)                    {_asciiFloatFormat=0;}
  void asciiFloatFormat(MSFloat::MSFloatFormat x_)    {_asciiFloatFormat=x_;}
  void asciiIntFormatClear(void)                      {_asciiIntFormat=0;}
  void asciiIntFormat(MSInt::MSIntFormat x_)          {_asciiIntFormat=x_;}
  //
  // Miscellaneous dimensions
  //
  int headerOffset(void)                        const {return _headerOffset;}
  int footerOffset(void)                        const {return _footerOffset;}
  int pageNumberOffset(void)                    const {return _pageNumberOffset;}
  int pageFrameOffset(void)                     const {return _pageFrameOffset;}
  int pageFrameLineWidth(void)                  const {return _pageFrameLineWidth;}
  int pageFrameExtent(void)                     const {return _pageFrameExtent;}
  void headerOffset(unsigned x_)                      {_headerOffset=x_;}
  void footerOffset(unsigned x_)                      {_footerOffset=x_;}
  void pageNumberOffset(unsigned x_)                  {_pageNumberOffset=x_;}
  void pageFrameOffset(unsigned x_)                   {_pageFrameOffset=x_;}
  void pageFrameLineWidth(unsigned x_)                {_pageFrameLineWidth=x_;}
  void pageFrameExtent(unsigned x_)                   {_pageFrameExtent=x_;}
  // 
  // internal values for diagnostics
  // 
  int pageCount(void)                           const {return _pageCount;}
  int pageCountTotal(void)                      const {return _pageCountTotal;}
  //
  // page banner methods
  //
  MSParagraph& banner(const MSParagraph&);
  MSParagraph& banner(const MSStringVector&);
  MSParagraph& banner(const char *);
  const MSParagraph& banner(void)               const {return _banner;}
  MSParagraph& banner(void)                           {return _banner;}
  //
  // page numbering methods
  //
  const MSUnsignedVector& pageNumbers(void)     const {return _pageNumbers;}
  const MSParagraph& pageNumber(void)           const {return _pageNumber;}
  MSParagraph& pageNumber(void)                       {return _pageNumber;}
  MSBoolean pageNumbering(void)                 const {return _pageNumbering;}
  void pageNumbers(const MSUnsignedVector& x_)        {_pageNumbers=x_;}
  MSParagraph& pageNumber(const MSParagraph& x_)      {_pageNumber=x_; return _pageNumber;}
  MSParagraph& pageNumber(const MSStringVector& x_)   {_pageNumber=x_; return _pageNumber;}
  MSParagraph& pageNumber(const char *x_)             {_pageNumber=x_; return _pageNumber;}
  void pageNumbering(MSBoolean x_)                    {_pageNumbering=x_;}
  MSStringVector& pageNumberText(void)                {return _pageNumberText;}

  // Callback methods
  void callback(const MSSymbol&,MSCallback *callback_);
  void callback(const MSSymbol&,MSReportCallbackFunc,void *clientData_=0);
  MSCallback *callback(const MSSymbol&);

  // the following methods return the top and bottom positions
  // based on the value of pageCount.
  int bodyTop(int);
  int bodyBottom(int);
  int bodyTop(void);
  int bodyBottom(void);

  int pageEnd(void)                                   {return _pageEnd;}
 void pageEnd(int x_)                                 {_pageEnd=x_;}
  int yPixel(void)                              const {return _yPixel;}
  void yPixel(int x_)                                 {_yPixel=x_;}

  // the following is True when there are different header/footer sizes for
  // different pages.  A lot of unnecessary processing is avoided if not True.
  MSBoolean conditionalPageSize(void)                 {return _conditionalPageSize;}
  void conditionalPageSize(MSBoolean x_)              {_conditionalPageSize=x_;}

  int bodyTopBase(void)                         const {return _bodyTopBase;}
  int bodyBottomBase(void)                      const {return _bodyBottomBase;}
  int bodyLeft(void)                            const {return _bodyLeft;}
  int bodyRight(void)                           const {return _bodyRight;}
  int x_end(void)                               const {return _x_end;}
  int y_end(void)                               const {return _y_end;}
  int psFontSize(void)                          const {return _psFontSize;}
  double fgGrayScale(void)                      const {return _fgGrayScale;}
  double bgGrayScale(void)                      const {return _bgGrayScale;}

  void bodyTopBase(int x_)                            {_bodyTopBase=x_;}
  void bodyBottomBase(int x_)                         {_bodyBottomBase=x_;}
  void bodyLeft(int x_)                               {_bodyLeft=x_;}
  void bodyRight(int x_)                              {_bodyRight=x_;}
  void x_end(int x_)                                  {_x_end=x_;}
  void y_end(int x_)                                  {_y_end=x_;}
  void psFontSize(int x_)                             {_psFontSize=x_;}
  void pageCount(int x_)                              {_pageCount=x_;}
  void pageCountTotal(int x_)                         {_pageCountTotal=x_;}
  void pageCountIncrement(void)                       {_pageCount++;}
  void fgGrayScale(double x_)                         {_fgGrayScale=x_;}
  void bgGrayScale(double x_)                         {_bgGrayScale=x_;}
  void defaultFontID(Font x_)                         {_defaultFontID=x_;}
  void defaultFontSize(int x_)                        {_defaultFontSize=x_;}

  virtual MSBoolean reportOpen(void);
  virtual MSBoolean reportClose(void); 
  virtual void init(void);
  virtual void postScriptStackInit(void);
  virtual void printProlog(void);
  virtual void printEpilog(void);
  virtual void printSetup(void);
  virtual void startPage(void);
  virtual void endPage(void);
  virtual void updateForeground(int);
  virtual void updateBackground(void);
  virtual void updateFont(void);
  virtual void strokeRectangle(double,double,double,double);
  virtual void fillRectangle(double,double,double,double);
  virtual void printReportString(unsigned long,int,int,const char *,int);
  virtual void setClipRectangle(int,int,int,int);

  void computePrintScale(void);
  void computePageSize(void);
  void printPageFrame(void);
  void printPageStart(void); 
  void setFgGrayScale(void);
  void setBgGrayScale(void);
  void setFont(void);
  void printPageTranslate(void);
  void translate(double,double);
  void translateScale(double,double,int,int);
  void printBox(unsigned long,double,double,double,double);
  void push(void);
  void pop(void);
  unsigned long formatStyle(unsigned long);
  MSBoolean printOnPage(MSPrintItem&,int,int);

  //////////////////////////////////////////////////////////////////////////
  //
  // page banner methods
  //
  //////////////////////////////////////////////////////////////////////////
  
  double bannerAngle(void)                            {return _bannerAngle;}
  double bannerXtrans(void)                           {return _bannerXtrans;}
  double bannerYtrans(void)                           {return _bannerYtrans;}
  double bannerWidth(void)                            {return _bannerWidth;}
  void bannerAngle(double x_)                         {_bannerAngle=x_;}
  void bannerWidth(double x_)                         {_bannerWidth=x_;}
  void bannerXtrans(double x_)                        {_bannerXtrans=x_;}
  void bannerYtrans(double x_)                        {_bannerYtrans=x_;}
  void computeBannerExtents(void);
  virtual void printBanner(void);

  //////////////////////////////////////////////////////////////////////////
  //
  // page number methods
  //
  //////////////////////////////////////////////////////////////////////////
  
  MSBoolean insertPageNumString(MSStringVector&);
  const MSString& insertString(void)            const {return _insertString;}
  const MSString& pageNumberString(void)        const {return _pageNumberString;}
  MSBoolean pageNumIncludeStatus(void)          const {return _pageNumIncludeStatus;}
  void pageNumIncludeStatus(MSBoolean x_)             {_pageNumIncludeStatus=x_;}
  MSBoolean cancelReportStatus(void)                  {return _cancelReport;}
  void computePageNumber(void);
  void printPageNumber(void);

  int tabStop(int);
  
  //////////////////////////////////////////////////////////////////////////
  //
  // callback methods
  //
  //////////////////////////////////////////////////////////////////////////
  MSNodeItem *callbackListHead(void);
  MSReportCallbackNode *callbackNode(const MSSymbol&);
  MSBoolean activateCallback(const MSSymbol&);
  void deleteCallbackList(void);
  
  int                _pageCount;
  MSBoolean printHeaderStatus(void)       {return _printHeaderStatus;}

protected:

  //////////////////////////////////////////////////////////////////////////
  //
  //header/footer methods
  //
  //////////////////////////////////////////////////////////////////////////
  
  void computePageHeaderSize(int);
  void computePageFooterSize(int);
  void printPageHeaders(void);
  void printPageFooters(void);
  void printHeaderStatus(MSBoolean x_)    {_printHeaderStatus=x_;}

  //////////////////////////////////////////////////////////////////////////
  //
  //general print methods
  //
  //////////////////////////////////////////////////////////////////////////
  
  MSRect& printArea(void)                            {return _printArea;}
  
private:
  int                _x_end;
  int                _y_end;
  int                _yPixel;
  double             _bannerWidth;
  double             _bannerAngle;
  double             _bannerXtrans;
  double             _bannerYtrans;
  double             _fgGrayScale;
  double             _bgGrayScale;
  
  MSBoolean	     _cancelReport;
  MSBoolean	     _uniformScaling;
  MSBoolean	     _conditionalPageSize;
  MSBoolean	     _printHeaderStatus;
  unsigned long      _pageFrameStyle;
  unsigned           _headerOffset;
  unsigned           _footerOffset;
  unsigned           _pageNumberOffset;
  unsigned           _pageFrameOffset;
  unsigned 	     _pageFrameLineWidth;
  unsigned 	     _pageFrameExtent;
  unsigned 	     _bindingWidth;
  int                _defaultFontSize;
  Font               _defaultFontID;
  MSString           _defaultFont;

  MSParagraph        _banner;

  MSFloatVector      _tabStop;

  MSStringVector     _pageNumberText;
  MSUnsignedVector   _pageNumbers;
  MSParagraph        _pageNumber;
  MSString           _pageNumberString;
  MSBoolean	     _pageNumIncludeStatus;
  MSBoolean	     _pageNumbering;
  MSString           _insertString;

  int                _pageCountTotal;
  int                _psFontSize;
  int                _columnHeadingHeight;
  
  MSRect             _printArea;

  int                _pageEnd;
  int                _bodyTopBase;
  int                _bodyBottomBase;
  int                _bodyLeft;
  int                _bodyRight;

  MSNodeItem        *_callbackListHead;

  unsigned long      _asciiFloatFormat;
  unsigned long      _asciiIntFormat;
};

inline MSNodeItem *MSReport::callbackListHead(void)               
{ return _callbackListHead; }

class MSGUIExport MSReportCallbackNode
{
protected:
  MSCallback *_callback;
  MSSymbol    _name;

public:
  MSReportCallbackNode(const MSSymbol& name_,MSCallback *pCallback_) :
  _name(name_),_callback(pCallback_) {}
  ~MSReportCallbackNode(void) { delete _callback; _callback=0; }

  void callback(MSCallback *pCallback_)
   { if (callback()!=pCallback_) { delete _callback; _callback=pCallback_; } }

  const MSSymbol& name(void) const { return _name; }
  MSCallback *callback(void) const { return _callback; }
};

#endif



