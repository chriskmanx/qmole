#ifndef MSWidgetOutputHEADER
#define MSWidgetOutputHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSWidgetHEADER
#include <MSGUI/MSWidget.H>
#endif
#include <MSGUI/MSShadow.H>
#include <MSGUI/MSGC.H>

class MSDisplayPrint;
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_FSTREAM
#include <fstream>
#else
class ofstream;
#endif
class MSPixmap;

class MSGUIExport MSWidgetOutput : public MSWidget
{
public:  
  class MSGUIExport DisplayPrint
  {
  public:
    MSDisplayPrint *_displayPrint;
    DisplayPrint(void);
    ~DisplayPrint(void);
  };
  
public:
  enum OutputMode {Draw=0,DrawPPM=1,Print=2};
  
protected:
  static OutputMode   _outputMode;
  static DisplayPrint _displayPrint;
  
public:
  MSWidgetOutput(void);
  MSWidgetOutput(MSDisplayServer*);
  MSWidgetOutput(MSWidget*);
  ~MSWidgetOutput(void);
  
  static MSDisplayPrint *displayPrint(void);
  static OutputMode outputMode(void);
  static void outputMode(OutputMode mode_);

  static MSBoolean displayPrintOpen(void);
  static MSBoolean displayPrintOpen(MSWidget *);  
  static MSBoolean displayPrintClose(void);
  static int displayPrintMode(void);
  static Window displayPrintPixmap(void);
  static void displayPrintOriginInc(MSWidget *);
  static void displayPrintOriginDec(MSWidget *);
  static void displayPrintFileName(const char *);
  static void displayPrintXorigin(int);
  static void displayPrintYorigin(int);
  static void displayPrintInit(MSWidget *);
  static void displayPrintClear(void);
  static ofstream& displayPrintStream(void);
  
  void XClearArea(Display *,Window,int,int,int,int,int);
  void XCopyArea(Display *,Window,Window,GC,int,int,int,int,int,int);
  void XDrawArc(Display *,Window,GC,int,int,int,int,int,int);
  void XDrawArcs(Display *,Window,GC,XArc *,int);
  void XDrawLine(Display *,Window,GC,int,int,int,int);
  void XDrawLines(Display *,Window,GC,XPoint *,int,int);
  void XDrawSegments(Display *,Window,GC,XSegment *,int);
  void XDrawPoint(Display *,Window,GC,int,int);
  void XDrawPoints(Display *,Window,GC,XPoint *,int,int);
  void XDrawRectangle(Display *,Window,GC,int,int,int,int);
  void XDrawRectangles(Display *,Window,GC,XRectangle *,int);
  void XDrawString(Display *,Window,GC,const XFontStruct *,int,int,const char *,int);
  void XDrawImageString(Display *,Window,GC,const XFontStruct *,int,int,const char *,int);
  void XFillArc(Display *,Window,GC,int,int,int,int,int,int);
  void XFillArcs(Display *,Window,GC,XArc *,int);
  void XFillPolygon(Display *,Window,GC,XPoint *,int,int,int);
  void XFillRectangle(Display *,Window,GC,int,int,int,int);
  void XFillRectangles(Display *,Window ,GC,XRectangle *,int);
  void XBFillPolygon(Display *,Window ,GC,XPoint *,int,int,int);
  void XBFillRectangle(Display *,Window ,GC,int,int,int,int);
  void XBFillRectangles(Display *,Window ,GC,XRectangle *,int);
  void PFillPolygon(Display *,Window ,GC,XPoint *,int,int,int);
  void PFillRectangle(Display *,Window ,GC,int,int,int,int);
  void PFillRectangles(Display *,Window ,GC,XRectangle *,int);
  int XTextWidth(const XFontStruct *,const char *,int) const;
  int XTextExtents(const XFontStruct *,const char *,int,int*,int*,int*,XCharStruct*);
  void XSetClipRectangles(Display *,GC,int,int,XRectangle *,int,int);
  void XSetDashes(Display *,GC,int,const char *,int);
  void XFreeGC(Display *,GC);
  int XTextLength(const XFontStruct *,const char *) const;
  MSBoolean doubleByte(const XFontStruct *) const;

  void drawFlatShadow(Window,const MSRect&,int,GC);
  void drawBevelShadow(Window,const MSRect&,int,GC,GC);
  void drawEtchedShadow(Window,const MSRect&,MSShadowStyle,int,GC,GC);
  void drawDiamondShadow(Window,const MSRect&,MSBoolean,GC,GC,GC,GC);

public:
  MSShadow& shadow(void);
  const MSShadow& shadow(void) const;
  
  GC highlightGC(void) const;
  GC bottomShadowGC(void) const;
  GC topShadowGC(void) const;
  GC backgroundShadowGC(void) const;
  GC selectShadowGC(void) const;
  unsigned long selectShadowColor(void) const;
  unsigned long topShadowColor(void) const;
  unsigned long bottomShadowColor(void) const;
  unsigned long highlightColor(void) const;

  MSShadowStyle shadowStyle(void) const;
  int highlightThickness(void) const;
  int shadowThickness(void) const;

  void shadowStyle(MSShadowStyle);
  void highlightThickness(int);
  void shadowThickness(int);
  void highlightColor(unsigned long);
  void highlightColor(const char *);  

  virtual void clear(void);

  // methods for builder support
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  void init(void);
  void initColors(void);  
  void createGCs(void);

  MSBoolean highlighted(void) const;
  MSGC& highlightMSGC(void);
  int topShadowOffset(void) const;
  void topShadowOffset(int);
  
  void drawBackground(void);
  void drawHighlight(void);
  void undrawHighlight(void);
  void drawShadow(void);
  void drawShadow(MSShadowStyle);
  void undrawShadow(void);
  void drawRaised(void);
  void drawSunken(void);
  void drawEtchedIn(void);
  void drawEtchedOut(void);
  void drawRaised(Window,const MSRect&,int);
  void drawSunken(Window,const MSRect&,int);
  void drawEtchedIn(Window,const MSRect&,int);
  void drawEtchedOut(Window,const MSRect&,int);

  void undrawBevel(void);
  void undrawBevel(const MSRect&,int);
  void undrawBevel(Window,const MSRect&,int);
  void drawBevel(void);
  void drawBevel(int);
  void drawBevel(const MSRect&,MSShadowStyle);
  void drawBevel(const MSRect&,MSShadowStyle,int);
  void drawBevel(Window,const MSRect&,MSShadowStyle,int);
  void drawBevel(Window,const MSRect&,int,GC,GC);

  void highlight(void);
  void unHighlight(void);
  void drawHighlightBorder(int);
  void undrawHighlightBorder(int);
  void drawHighlightBorder(GC,int,int,int,int,int);

  void copyPixmap(Display*,const MSPixmap&,Window,GC,int,int);
  void copyPixmap(Display*,const MSPixmap&,Window,GC,int,int,int,int,int,int,int,int);
  
  virtual void updateBackground(unsigned long);

private:
  Window windowForDrawing(void);
  MSBoolean canDraw(void);
  int xDrawingOffset(void);
  int yDrawingOffset(void);

protected:
  MSShadow       _shadow;
  MSGC           _highlightMSGC;
  int            _highlightThickness;
  MSBoolean      _highlighted;
  unsigned long  _highlightColor;
  int            _shadowThickness;  
  MSShadowStyle  _shadowStyle;
  int            _topShadowOffset;
};

#ifndef MS_NO_INLINES
#ifndef MSWidgetOutputINLINES
#include <MSGUI/MSWidgetOutputInlines.C>
#endif
#endif

#endif

