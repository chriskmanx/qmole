#ifndef MSPrintDisclaimerHEADER
#define MSPrintDisclaimerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <X11/Xlib.h>
#include <MSTypes/MSString.H>
#include <MSGUI/MSP.H>

class MSPostScript;

class MSGUIExport MSPrintDisclaimer : public MSP
{
public:
  MSPrintDisclaimer(MSPostScript *);
  ~MSPrintDisclaimer(void);
  void closeFile(void);
  void computeSize(void);
  void print(void);
  void printRule(void);
  const MSString& fileName(void)       const {return _fileName;}
  const MSString& fontName(void)       const {return _fontName;}
  PageOrientation orientation(void)    const {return _orientation;}
  DisclaimerStyle style(void)          const {return _style;}
  int lineWidth(void)                  const {return _lineWidth;}
  int height(void)                     const {return _height;}
  int width(void)                      const {return _width;}
  int fontSize(void)                   const {return _fontSize;}
  int leftPixel(void)                  const {return _leftPixel;}
  int rightPixel(void)                 const {return _rightPixel;}
  int topPixel(void)                   const {return _topPixel;}
  int bottomPixel(void)                const {return _bottomPixel;}

  void fileName(const char *x_)              {_fileName=x_;}  
  void orientation(PageOrientation x_)       {_orientation=x_;}
  void style(DisclaimerStyle x_)             {_style=x_;}
  void lineWidth(int x_)                     {_lineWidth=x_;}
  void height(int x_)                        {_height=x_;}
  
  void font(const MSString&);
  void leftMargin(double);
  void rightMargin(double);
  void topMargin(double);
  void bottomMargin(double);
  double leftMargin(void) const;
  double rightMargin(void) const;
  double topMargin(void) const;
  double bottomMargin(void) const;

protected:
  MSPostScript *owner(void) const;
  ofstream& pout(void);
  int xTrans(void)                     const {return _xTrans;}
  int yTrans(void)                     const {return _yTrans;}
  int rowCount(void)                   const {return _rowCount;}
  int wordCount(int i_)                const {return _wordCount[i_];}
  int spaceWidth(int i_)               const {return _spaceWidth[i_];}
  int residualSpace(int i_)            const {return _residualSpace[i_];}
  Font fontID(void)                    const {return _fontID;}
  const MSString& fontString(void)     const {return _fontString;}
  void xTrans(int x_)                        {_xTrans=x_;}
  void yTrans(int x_)                        {_yTrans=x_;}
  void width(int x_)                         {_width=x_;}
  void fontSize(int x_)                      {_fontSize=x_;}
  void fontID(Font x_)                       {_fontID=x_;}
  void fontString(const char *x_)            {_fontString=x_;}
  
private:
  MSPostScript         *_owner;
  int			_xTrans;
  int			_yTrans;
  int			_rowCount;
  int			_wordCount[buflen];
  int 			_spaceWidth[buflen];
  int 			_residualSpace[buflen];
  int			_leftPixel;
  int			_rightPixel;
  int			_topPixel;
  int			_bottomPixel;
  int			_lineWidth;
  int			_height;
  int			_width;
  int                   _fontSize;
  Font			_fontID;
  MSString              _fontName;
  MSString              _fontString;
  MSString              _fileName;
  DisclaimerStyle	_style;
  PageOrientation       _orientation;
  ifstream     		 pin;
};

//for backward compatibility.
typedef MSPrintDisclaimer  MSDisclaimer;
#endif


