#ifndef MSHScrollBarHEADER
#define MSHScrollBarHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSScrollBar.H>

class MSHScrollBar;

class MSGUIExport MSHScrollBar : public MSScrollBar
{
public:
  class MSGUIExport HElevator : public MSScrollBar::Elevator
  {
  friend class MSHScrollBar;
  public:
    HElevator(MSHScrollBar *);
    ~HElevator(void);

    virtual void select(void);
    virtual void unselect(void);

  protected:
    virtual void draw(void);
    virtual void configure(void);
  };

public:
  MSHScrollBar(MSWidget *,int min_=0,int max_=100,int inc_=1);
  ~MSHScrollBar(void);

protected:
  void init(void);
  void moveElevator(int x_,int y_);
  virtual void configureForOpenlook(void);
  virtual void configureForMotif(void);

  int calcXValue(int value_);
  int boundsCheckX(int x_);
  virtual int calcSliderValue(int x_,int y_);

  virtual void redrawElevator(void);
  virtual void configureElevator(void);  
  virtual void drawElevatorCable(void);
  virtual void drawPropIndicator(void);
  virtual void drawMarkers(void);
  virtual void adjustPointer(void);

  virtual void motionLoop(void);
  virtual void openlookButton1Press(const XEvent*);
  virtual void openlookButton2Press(const XEvent*);
  virtual void motifButton1Press(const XEvent*);
  virtual void motifButton2Press(const XEvent*);
};

#endif




