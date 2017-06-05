#ifndef MSToolTipHEADER
#define MSToolTipHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidget.H>
#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSFontObject.H>

class MSPixmap;
class ToolTipTimer;
class MSGUIExport MSToolTip : public MSWidget
{
public:
  enum TailPosition{Top=1,Bottom=2,Left=4,Right=8};
  enum Style{Bubble,Box};

  MSToolTip(MSDisplayServer *);
  ~MSToolTip(void);
  
  const MSStringVector &toolTip(void) const {return _toolTip;}
  void toolTip(const MSStringVector &,MSWidget *displayFor_=0);

  Style style(void) const {return _style;}
  void style(Style);

  unsigned long delay(void) const {return _delay;}
  void delay(unsigned long);

  void tailPosition(unsigned tailPosition_) {_tailPosition=tailPosition_;}
  unsigned tailPosition(void) const {return _tailPosition;}
    
  MSWidget *displayFor(void) const {return _displayFor;}
  
  static MSBoolean displayToolTip(void);
  static void displayToolTip(MSBoolean);

  virtual void redraw(void);
  virtual void map(void);
  virtual void unmap(void);

private:
friend class ToolTipTimer;

  static MSBoolean _displayToolTip;
  MSStringVector   _toolTip;
  unsigned         _marginHeight;
  unsigned         _marginWidth;
  unsigned         _bubbleMarginHeight;
  unsigned         _bubbleMarginWidth;
  MSFontObject     _fontObject;
  GC               _gc;
  ToolTipTimer    *_timer;
  unsigned long    _delay;
  unsigned         _tailPosition;
  Style            _style;
  MSWidget        *_displayFor;
  
  MSFontObject &fontObject(void) {return _fontObject;}
  GC gc(void) {return _gc;}
  ToolTipTimer *timer(void) {return _timer;}
  unsigned marginHeight(void) {return _marginHeight;}
  unsigned marginWidth(void) {return _marginWidth;}
  unsigned bubbleMarginHeight(void) {return _bubbleMarginHeight;}
  unsigned bubbleMarginWidth(void) {return _bubbleMarginWidth;}

  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void computeSize(void);
  
};

#endif
