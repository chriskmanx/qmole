#ifndef MSDisplayPrintHEADER
#define MSDisplayPrintHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <X11/Xlib.h>

#include <MSTypes/MSNodeList.H>
#ifndef MSWidgetHEADER
#include <MSGUI/MSWidget.H>
#endif

#ifndef MSPixmapHEADER
#include <MSGUI/MSPixmap.H>
#endif
#include <MSGUI/MSPostScript.H>

class MSClipArea;
class MSDashLine;
class MSPrintDisclaimer;

class MSGUIExport MSDisplayPrint:public MSPostScript
{
friend class MSClipArea;
friend class MSDashLine;
friend class MSWidgetOutput;
friend class MSPrintDisclaimer;
public:
  MSDisplayPrint(void);
  ~MSDisplayPrint(void);

  virtual MSBoolean printOpen(MSWidget*);
  virtual MSBoolean printOpen(void);
  virtual MSBoolean printClose(void); 

  void printInit(MSWidget*);
  void printClear(void);
  void defaultFont(Font);
  Font defaultFont(void) const;
  void originInc(MSWidget *widget_);
  void originDec(MSWidget *widget_);

protected: 
  MSWidget *owner(void)                 const {return _owner;}
  XFontStruct *fontInfo(void)           const {return _fontInfo;}
  int fontHeight(void)                  const {return _fontInfo->ascent+_fontInfo->descent;}
  Pixmap ppmPixmap(void)                const {return _ppmPixmap->pixmap();}
  MSClipArea *activeClip(void)          const {return _activeClip;}
  MSDashLine *activeDash(void)          const {return _activeDash;}
  MSBoolean updateDash(void)            const {return _updateDash;}
  int clipCount(void)                   const {return _clipCount;}
  MSBoolean special(void)               const {return _special;}
  Font defaultFontID(void)              const {return _defaultFontID;}

  void activeClip(MSClipArea *x_)             {_activeClip=x_;}
  void clipCount(int x_)                      {_clipCount=x_;}
  void updateDash(MSBoolean x_)               {_updateDash=x_;}
  void defaultFontID(Font x_)                 {_defaultFontID=x_;}
  void activeDash(MSDashLine *);
  void comment(const char *);
  void special(MSBoolean);
  void printPixmap(void); 

  MSClipArea *updateClipGC(GC);
  MSClipArea *findClipGC(GC);
  MSDashLine *findDashGC(GC);
  void deleteAllClipGC(void);
  void deleteAllDashGC(void);

  void init(void);
  void setAttributes(void);
  MSBoolean setLineAttributes(void);
  void setFontAttributes(void);
  void setClipMask(void);
  void setFillStyle(void);
  void setFillRule(void);
  void setArcMode(void);
  void unsetClip(void);
  void setClip(MSClipArea*);
  void setClipWindow(int,int);
  void unsetClipWindow(void);
  void printScale(void);
  void computePrintScale(void);

  virtual void updateGC(GC gc_);
  virtual void updateForeground(int i_=0);
  virtual void updateBackground(void);
  virtual void updateFont(void);

  void printArc(GC,int,int,int,int,int,int);
  void printArcs(GC,XArc *,int);
  void printLine(GC,int,int,int,int);
  void printLines(GC,XPoint *,int,int);
  void printSegments(GC,XSegment *,int);
  void printPoint(GC,int,int);
  void printPoints(GC,XPoint *,int,int);
  void printRectangle(GC,int,int,int,int);
  void printRectangles(GC,XRectangle *,int);
  void printString(GC,int,int,const char *,int);
  void printImageString(GC,int,int,const char *,int);
  void printFillArc(GC,int,int,int,int,int,int);
  void printFillArcs(GC,XArc *,int);
  void printFillPolygon(GC,XPoint *,int,int,int,MSBoolean fill_=MSFalse);
  void printFillRectangle(GC,int,int,int,int,MSBoolean fill_=MSFalse);
  void printFillRectangles(GC,XRectangle *,int,MSBoolean fill_=MSFalse);
  void printSetClipRectangles(GC,int,int,XRectangle *,int,int);
  void printSetDashes(GC,const char *,int);
  void printFreeGC(GC);

  virtual void printArc(int,int,int,int,int,int,int,DrawMode);
  virtual void printLine(double,double,double,double);
  virtual void printRectangle(double,double,double,double);
  virtual void setClipRectangle(int,int,int,int);
  virtual void printString(int,int,const char *,int);
  
private:
  MSPixmap	       *_ppmPixmap;
  MSClipArea	       *_clip;
  MSClipArea	       *_activeClip;
  int			_clipCount;
  MSDashLine	       *_activeDash;
  MSBoolean	        _updateDash;
  MSWidget             *_owner;
  XFontStruct	       *_fontInfo;
  Font			_defaultFontID;

  MSRect		_clipRect;
  MSNodeItem            _clipGCListHead;
  MSNodeItem            _dashGCListHead;
  MSBoolean		_special;
};

class MSGUIExport MSClipArea
{
friend class MSDisplayPrint;
  int _clipNumber;
  GC  _clipID;
protected:
  MSClipArea(GC x_)                     {_clipID=x_;}
  ~MSClipArea(void) {}

  GC clipID(void)                       {return _clipID;}
  int clipNumber(void)                  {return _clipNumber;}
  void clipNumber(int x_)               {_clipNumber=x_;}
};  

class MSGUIExport MSDashLine
{
  int 	 	_dashCt;
  GC	 	_dashID;
  const char   *_dash;
public:
  MSDashLine(GC gc_,const char* d_,int n_) {_dashID=gc_; _dash=d_; _dashCt=n_;}
  ~MSDashLine(void) {}

  GC dashID(void)                    const {return _dashID;}
  int dash(int i_)                   const {return i_<_dashCt?(int)_dash[i_]:0;}
  int dashCt(void)                   const {return _dashCt;}
  void dash(const char *x_)          {_dash=x_;}
  void dashCt(int x_)                {_dashCt=x_;}
};  


#endif
