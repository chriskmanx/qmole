#ifndef MSVScaleHEADER
#define MSVScaleHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSScale.H>

class MSVScale;
class MSVGauge;

class MSGUIExport MSVScale : public MSScale
{
public:
  MSVScale(MSWidget *,const char *title_=0);
  MSVScale(MSWidget *,const MSStringVector&);
  MSVScale(MSWidget *,MSFloat&,const char *title_=0); 
  MSVScale(MSWidget *,MSFloat&,const MSStringVector&);
  MSVScale(MSWidget *,MSInt&,const char *title_=0); 
  MSVScale(MSWidget *,MSInt&,const MSStringVector&);
  ~MSVScale(void);

  virtual int sliderSize(void) const;
  virtual void sliderSize(int);
  
  MSScale& operator=(const MSFloat& x_)        {value(x_); return *this;}
  MSScale& operator=(double x_)                {value(x_); return *this;}
  MSScale& operator=(const MSInt& x_)          {value(x_); return *this;}
  MSScale& operator=(int x_)                   {value(x_); return *this;}

  class MSGUIExport VSlider : public MSScale::Slider
  {
  friend class MSVScale;
  friend class MSGauge;
  protected:
    virtual void drawSliderEtch(void);
    virtual void moveTo(int,int);
  public:
    VSlider(MSWidget *);
    ~VSlider(void);
  };  

protected:
  int titleWidth(void)  		     {return _titleWidth;}
  void titleWidth(int x_)		     {_titleWidth=x_>_titleWidth?x_:_titleWidth;}

  void init(void);
  virtual void computeSize(void);
  virtual void computeLabelOffset(void);
  virtual void computeSliderAreaSize(void);
  virtual void computeSliderScale(void);
  virtual void computeTickInc(void);
  virtual void setSliderPosition(int);
  virtual double pixelToValue(int);
  virtual int valueToPixel(double);
  virtual int incFactor(int,int);

  virtual void drawSliderTitles(void);
  virtual void drawTickLabels(void);
  virtual void moveSlider(const XEvent *);
  virtual void drawSliderEtch(void);

  virtual void home(void);
  virtual void end(void);

private:
  int	_titleWidth;
};

#endif


