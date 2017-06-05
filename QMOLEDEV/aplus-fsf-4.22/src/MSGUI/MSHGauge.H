#ifndef MSHGaugeHEADER
#define MSHGaugeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSHScale.H>

class MSGUIExport MSHGauge : public MSHScale
{
public:
  MSHGauge(MSWidget *,const char *title_=0);
  MSHGauge(MSWidget *,const MSStringVector&);
  MSHGauge(MSWidget *,MSFloat&,const char *title_=0); 
  MSHGauge(MSWidget *,MSFloat&,const MSStringVector&);
  MSHGauge(MSWidget *,MSInt&,const char *title_=0); 
  MSHGauge(MSWidget *,MSInt&,const MSStringVector&);
  ~MSHGauge(void);

  MSScale& operator=(const MSFloat& x_)      {value(x_); return *this;}
  MSScale& operator=(double x_)              {value(x_); return *this;}
  MSScale& operator=(const MSInt& x_)        {value(x_); return *this;}
  MSScale& operator=(int x_)                 {value(x_); return *this;}

  const MSFloat& startValue(void) const      { return _startValue; }
  void   startValue(const MSFloat& value_); 

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

protected:
  enum Direction { Left, Right };

  int gaugeWidth(void)                { return _gaugeWidth; }
  void gaugeWidth(int x_)             { _gaugeWidth=x_;     }
  
  void init(void);
  void drawGauge(void);
  void updateGauge(double);
  void drawGaugeShadow(int,int,int,int,Direction);
  void updateGaugeShadow(int,int,int,int,Direction);

  virtual void redraw(void);
  virtual void updateSliderSize(double);
  virtual void computeLabelOffset(void);
  virtual void drawSubWindows(void);

  virtual void update(const MSIndexVector&);
  virtual void buttonPress(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void drawSliderEtch(void);
  virtual void sliderRedrawNotify(void);
private:
  int 	    _gaugeWidth;
  MSFloat   _startValue;
  Direction _direction;
};


#endif
