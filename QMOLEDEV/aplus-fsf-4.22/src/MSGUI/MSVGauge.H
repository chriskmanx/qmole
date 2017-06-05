#ifndef MSVGaugeHEADER
#define MSVGaugeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSVScale.H>

class MSGUIExport MSVGauge:public MSVScale
{
public:
  MSVGauge(MSWidget *,const char *title_=0);
  MSVGauge(MSWidget *,const MSStringVector&);
  MSVGauge(MSWidget *,MSFloat&,const char *title_=0); 
  MSVGauge(MSWidget *,MSFloat&,const MSStringVector&);
  MSVGauge(MSWidget *,MSInt&,const char *title_=0); 
  MSVGauge(MSWidget *,MSInt&,const MSStringVector&);
  ~MSVGauge(void);

  MSScale& operator=(const MSFloat& x_)      {value(x_); return *this;}
  MSScale& operator=(double x_)              {value(x_); return *this;}
  MSScale& operator=(const MSInt& x_)        {value(x_); return *this;}
  MSScale& operator=(int x_)                 {value(x_); return *this;}

  const MSFloat& startValue(void) const      { return _startValue; }
  void   startValue(const MSFloat& value_); 

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

protected:
  enum Direction { Up, Down };

  int gaugeHeight(void)                      {return _gaugeHeight;}
  void gaugeHeight(int x_)                   {_gaugeHeight=x_;}
  
  void init(void);
  void drawGauge(void);
  void updateGauge(int);
  void drawGaugeShadow(int,int,int,int,Direction);
  void updateGaugeShadow(int,int,int,int,Direction);

  virtual void redraw(void);
  virtual void moveValueWin(int,int);
  virtual void computeLabelOffset(void);
  virtual void updateSliderSize(double);
  virtual void drawSubWindows(void);

  virtual void update(const MSIndexVector&);
  virtual void buttonPress(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void drawSliderEtch(void);
  virtual void sliderRedrawNotify(void);


private:
  int 	_gaugeHeight;
  MSFloat   _startValue;
  Direction _direction;
};

#endif







