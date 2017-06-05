#ifndef MSHScaleHEADER
#define MSHScaleHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSScale.H>

class MSHScale;
class MSHGauge;

class MSGUIExport MSHScale : public MSScale
{
public:
  MSHScale(MSWidget *,const char *title_=0);
  MSHScale(MSWidget *,const MSStringVector&);
  MSHScale(MSWidget *,MSFloat&,const char *title_=0); 
  MSHScale(MSWidget *,MSFloat&,const MSStringVector&);
  MSHScale(MSWidget *,MSInt&,const char *title_=0); 
  MSHScale(MSWidget *,MSInt&,const MSStringVector&);
  ~MSHScale(void);

  virtual int sliderSize(void) const;
  virtual void sliderSize(int);

  MSScale& operator=(const MSFloat& x_)        {value(x_); return *this;}
  MSScale& operator=(double x_)                {value(x_); return *this;}
  MSScale& operator=(const MSInt& x_)          {value(x_); return *this;}
  MSScale& operator=(int x_)                   {value(x_); return *this;}
  
  class MSGUIExport HSlider : public MSScale::Slider
  {
  friend class MSHScale;
  friend class MSHGauge;
  public:
    HSlider(MSWidget *);
    ~HSlider(void);
    virtual void moveTo(int,int);
  protected:
    virtual void drawSliderEtch(void);
  };  

protected:
  void init(void);
  virtual void computeSize(void);
  virtual void computeSliderAreaSize(void);
  virtual void computeSliderScale(void);
  virtual void computeTickInc(void);
  virtual void setSliderPosition(int);
  virtual void computeLabelOffset(void);
  virtual void drawSliderTitles(void);
  virtual void drawTickLabels(void);
  virtual void moveSlider(const XEvent *);
  virtual double pixelToValue(int);
  virtual int valueToPixel(double);
  virtual int incFactor(int,int);
  virtual void drawSliderEtch(void);
};

#endif
