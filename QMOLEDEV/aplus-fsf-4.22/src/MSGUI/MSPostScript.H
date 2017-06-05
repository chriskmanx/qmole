#ifndef MSPostScriptHEADER
#define MSPostScriptHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <sys/types.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSP.H>
#include <MSGUI/MSPrintFont.H>
#include <MSGUI/MSPrintDisclaimer.H>
#include <MSGUI/MSGUIEnum.H>


class MSPrintFontData;

class MSGUIExport MSPrintFontHashTable:public MSHashTable
{
public:
  MSPrintFontHashTable(void):MSHashTable() {}
  MSPrintFontHashTable(int size_):MSHashTable(size_) {}
  ~MSPrintFontHashTable(void);
};

class MSGUIExport MSPostScript:public MSPrintFont,public MSP
{
//friend class MSDisplayPrint;
friend class MSPrintDisclaimer;
public:
  MSPostScript(void);
  ~MSPostScript(void);

  virtual MSBoolean printOpen(void);
  virtual MSBoolean printClose(void); 
  
  MSPrintDisclaimer& disclaimer(void)     const {return *_disclaimer;}
  int x_org(void)                         const {return _x_org;}
  int y_org(void)                         const {return _y_org;}
  PageOrientation pageOrientation(void)   const {return _pageOrientation;}
  PageSize pageSize(void)                 const {return _pageSize;}
  PrintMode printMode(void)               const {return _printMode;}
  unsigned long pageLayout(void)          const {return _pageLayout;}
  OutputMode outputMode(void)             const {return _outputMode;}
  int defaultFontSize(void)               const {return _defaultFontSize;}
  const MSString& defaultFontString(void) const {return _defaultFontString;}
  const MSString& defaultFontName(void)   const {return _defaultFontName;}
  const MSString& fileName(void)          const {return _fileName;}
  const MSString& fontPath(void)          const {return _fontPath;}
  const MSString& fontFileName(void)      const {return _fontFileName;}
  Font fontID(void)                       const {return _fontID;}
  unsigned leftPixel(void)                const {return _leftPixel;}
  unsigned rightPixel(void)               const {return _rightPixel;}
  unsigned topPixel(void)                 const {return _topPixel;}
  unsigned bottomPixel(void)              const {return _bottomPixel;}
  int pages(void)                         const {return _pages;}
  int fontSize(void)                      const {return _fontSize;}
  XGCValues& gcValues(void)                     {return _gcValues;}

  // The fileName, with the suffix corrected for outputMode
  MSString adjustedFileName(void) const;

  void x_org(int x_)                            {_x_org=x_;}
  void y_org(int x_)                            {_y_org=x_;}
  void pageOrientation(PageOrientation x_)      {_pageOrientation=x_;}
  void pageSize(PageSize x_)                    {_pageSize=x_;}
  void printMode(PrintMode x_)                  {_printMode=x_;}
  void pageLayout(unsigned long x_)             {_pageLayout=x_;}
  void outputMode(OutputMode x_)                {_outputMode=x_;}
  void defaultFontSize(int x_)                  {_defaultFontSize=x_;}
  void fileName(const char *x_)                 {_fileName=x_;}  
  void fontID(Font x_)                          {_fontID=x_;}
  void fontSize(int x_)                         {_fontSize=x_;}
  void pages(int x_)                            {_pages=x_;}
  Font font(const char *);
  const char *fontName(Font);
  void fontPath(const char *);
  void defaultFontString(const char *);
  void defaultFontName(const char *);

  void leftMargin(double);
  void rightMargin(double);
  void topMargin(double);
  void bottomMargin(double);
  double leftMargin(void) const;
  double rightMargin(void) const;
  double topMargin(void) const;
  double bottomMargin(void) const;

  ofstream pout;

  void setAttributes(void);
  MSPrintFontData *fontStruct(Font);
  Font printFontID(const char *);
  const MSPrintFontData *fontdata(Font);

  virtual void printLine(double,double,double,double);

protected: 
  enum DrawMode {Stroke=0,Fill=1};

  void initFont(void);
  void initPS(void);
  void addFont(const char *);
  MSBoolean findFont(const char *);

  MSPrintFontHashTable& fontDataHashTable(void) {return _fontDataHashTable;}
  MSHashTable& printFontIDHashTable(void)       {return _printFontIDHashTable;}
  MSStringVector& documentFonts(void)           {return _documentFonts;}

  XColor& fgRGB(void)                      {return _fgRGB;}
  XColor& bgRGB(void)                      {return _bgRGB;}
  int lineWidth(void)                      {return _lineWidth;}
  int lineStyle(void)                      {return _lineStyle;}
  int dashOffset(void)                     {return _dashOffset;}
  int capStyle(void)                       {return _capStyle;}
  int joinStyle(void)                      {return _joinStyle;}
  int fillStyle(void)                      {return _fillStyle;}
  int fillRule(void)                       {return _fillRule;}
  int arcMode(void)                        {return _arcMode;}
  unsigned long fgColor(void)              {return _foreground;}
  unsigned long bgColor(void)              {return _background;}
  double gscale(void)                      {return _gscale;}
  int bwidth(void)                         {return _bwidth;}
  int bheight(void)                        {return _bheight;}
  int x_translate(void)                    {return _x_translate;}
  int y_translate(void)                    {return _y_translate;}
  double x_printScale(void)                {return _x_printScale;}
  double y_printScale(void)                {return _y_printScale;}
  int fontCount(void)                      {return _fontCount;}
  const MSString& fontString(void)   const {return _fontString;}
  const MSString& path(void)         const {return _path;}

  void lineWidth(int x_)                   {_lineWidth=x_;}
  void lineStyle(int x_)                   {_lineStyle=x_;}
  void dashOffset(int x_)                  {_dashOffset=x_;}
  void capStyle(int x_)                    {_capStyle=x_;}
  void joinStyle(int x_)                   {_joinStyle=x_;}
  void fillStyle(int x_)                   {_fillStyle=x_;}
  void fillRule(int x_)                    {_fillRule=x_;}
  void arcMode(int x_)                     {_arcMode=x_;}
  void fgColor(unsigned long x_)           {_foreground=x_;}
  void bgColor(unsigned long x_)           {_background=x_;}
  void gscale(double x_)                   {_gscale=x_;}
  void bwidth(int x_)                      {_bwidth=x_;}
  void bheight(int x_)                     {_bheight=x_;}
  void x_translate(int x_)                 {_x_translate=x_;}
  void y_translate(int x_)                 {_y_translate=x_;}
  void x_printScale(double x_)             {_x_printScale=x_;}
  void y_printScale(double x_)             {_y_printScale=x_;}
  void fontCount(int x_)                   {_fontCount=x_;}
  void fontString(const char *x_)          {_fontString=x_;}
  void fontFileName(const char *x_)        {_fontFileName=x_;}

  static unsigned long blackPixel(void)    {return _blackPixel;}
  static unsigned long whitePixel(void)    {return _whitePixel;}
  static void blackPixel(unsigned long x_) {_blackPixel=x_;}
  static void whitePixel(unsigned long x_) {_whitePixel=x_;}

  void init(void);
  MSBoolean setForeground(void);
  void setBackground(int mode_=0);
  MSBoolean setBGColor(void);
  MSBoolean setFGColor(int mode_=0);
  MSBoolean setLineAttributes(void);
  void setFontAttributes(void);
  void setFillStyle(void);
  void setFillRule(void);
  void setArcMode(void);
  int extractFontSize(const char *);
  const char *extractFontString(const char*);
  const char *formatFontString(const char*);

  virtual void printProlog(void);
  virtual void printEpilog(void);
  virtual void printSetup(void);
  virtual void printArc(int,int,int,int,int,int,int,DrawMode);
  virtual void printRectangle(double,double,double,double);
  virtual void printString(int,int,const char *,int);
  virtual void setClipRectangle(int,int,int,int);
  virtual void updateForeground(int mode_=0);
  virtual void updateBackground(void);
  virtual void updateFont(void);

  int	                _pages;

private:
  int	                _fontSize;
  int                   _defaultFontSize;
  int                   _fontCount;
  Font			_fontID;
  MSStringVector	_documentFonts;
  MSString		_fileName;
  MSString		_defaultFontString;
  MSString		_defaultFontName;
  MSString		_fontName;
  MSString		_fontString;
  MSString              _fontFileName;
  MSString              _fontPath;
  MSString              _path;

  PageOrientation 	_pageOrientation;
  PageSize	 	_pageSize;
  PrintMode	 	_printMode;
  unsigned long	 	_pageLayout;
  OutputMode	 	_outputMode;

  int			_x_org;
  int			_y_org;
  int			_bwidth;
  int			_bheight;
  int			_x_translate;
  int			_y_translate;
  double 		_x_printScale;
  double		_y_printScale;
  double		_fontScale;
  double		_gscale;
  int			_leftPixel;
  int			_rightPixel;
  int			_topPixel;
  int			_bottomPixel;
  XGCValues		_gcValues;
  XColor		_fgRGB;
  XColor		_bgRGB;

  int 			_lineWidth;
  int			_lineStyle;
  int			_dashOffset;
  int			_capStyle;
  int			_joinStyle;
  int			_fillStyle;
  int			_fillRule;
  int			_arcMode;
  unsigned long 	_foreground;
  unsigned long 	_background;
  static unsigned long  _blackPixel;
  static unsigned long  _whitePixel;  
  MSPrintDisclaimer    *_disclaimer;

  MSHashTable           _printFontIDHashTable;
  MSPrintFontHashTable  _fontDataHashTable;
  
};

#endif





