#ifndef MSScrollBarHEADER
#define MSScrollBarHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPrimitive.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSPopupMenu.H>
#include <MSIPC/MSRegularTimer.H>
#include <MSIPC/MSIntervalTimer.H>

class MSScrollBar;
class MSArrow;

class MSGUIExport MSScrollBar : public MSPrimitive
{
public:
  class MSGUIExport DelayTimer : public MSRegularTimer
  {
  protected:
    MSScrollBar *_scrollbar;
  public:
    DelayTimer(MSScrollBar *,unsigned long);
    ~DelayTimer(void);
    virtual void process(void);
  };

  class MSGUIExport RepeatTimer : public MSIntervalTimer
  {
  protected:
    MSScrollBar *_scrollbar;
  public:
    RepeatTimer(MSScrollBar *,unsigned long);
    ~RepeatTimer(void);
    virtual void process(void);
  };

  class MSGUIExport PopupMenu : public MSPopupMenu
  {
  protected:
    MSScrollBar *_scrollbar;
    enum {Openlook=0,Motif=1,Windows=2,GotoTop=3,GotoBottom=4};
  public:
    PopupMenu(MSScrollBar *);
    void scrollbar(MSScrollBar *scrollbar_);
    virtual void activate(void);
  };

  class MSGUIExport Elevator : public MSWidgetCommon
  {
  friend class MSScrollBar;
  public:
    MSArrow  *_arrow1;
    MSArrow  *_arrow2;

    void drawArrows(void);
    virtual void configure(void);
    virtual void updateBackground(unsigned long);
  public:
    Elevator(MSScrollBar *);
    ~Elevator(void);
    
    MSArrow *arrow1(void) const;
    MSArrow *arrow2(void) const;
    MSScrollBar *scrollBar(void) const;
    
    virtual void draw(void);
    virtual void redraw(void);
    virtual void select(void);
    virtual void unselect(void);
  };

friend class DelayTimer;
friend class RepeatTimer;
friend class Elevator;
friend class PopupMenu;

public:
  enum Change {Inc,Dec,PageDown,PageUp,ValueChange,Drag,Home,End,NoChange};
  enum Style {Openlook=0,Motif=1,Windows=2};
    
  MSScrollBar(MSWidget *,int min_=0,int max_=100,int inc_=1);
  ~MSScrollBar(void);
  
  int min(void) const;
  int max(void) const;
  int inc(void) const;
  int pageInc(void) const;
  int value(void) const;
  int viewSize(void) const;
  MSScrollBar::Style style(void) const;
  static MSScrollBar::Style defaultStyle(void);

  void min(int);
  void max(int);
  void inc(int);
  void pageInc(int);
  void value(int); 
  void viewSize(int);
  void valueChange(int);            // changes value without invoking callbacks
  void view(int start_,int size_);  // changes view without invoking callbacks
  void style(MSScrollBar::Style);
  static void defaultStyle(MSScrollBar::Style);
  
  virtual void redraw(void);

protected:
  Elevator                *_elevator;
  MSPixmap                *_cablePixmap;
  int                      _value;
  int                      _savedValue;
  int                      _max;
  int                      _min;
  int                      _inc;
  int                      _pageInc;
  int                      _viewSize;
  MSBoolean                _slidingOn;
  MSBoolean                _markersOn;
  MSBoolean                _elevatorOn;
  MSBoolean                _repeatOn;
  int                      _separation_x;
  int                      _separation_y;
  int                      _initial_x;
  int                      _initial_y;
  MSRect                   _sliderAreaRect;
  DelayTimer              *_delayTimer;
  RepeatTimer             *_repeatTimer;
  MSScrollBar::Change      _changeType;
  MSScrollBar::Style       _style;

  MSRect& sliderAreaRect(void);
  MSPixmap *cablePixmap(void) const;
  Elevator *elevator(void) const;
  PopupMenu *popupMenu(void);
  MSScrollBar::Change changeType(void) const;
  MSTimer *delayTimer(void) const;
  MSTimer *repeatTimer(void) const;
  MSBoolean repeatOn(void) const;
  MSBoolean slidingOn(void) const;
  MSBoolean markersOn(void) const;
  MSBoolean elevatorOn(void) const;

  void init(void);
  void createPopupMenu(void);
  
  void processDelayTimer(void);
  void processRepeatTimer(void);

  virtual void redrawElevator(void);
  virtual void configureElevator(void);  
  virtual void drawElevator(void);
  virtual void drawElevatorCable(void);
  virtual void drawPropIndicator(void);
  virtual void drawMarkers(void);
  virtual void drawArrows(void);  

  void updateSliderValue(void);
  void setCablePixmap(void);
  virtual void adjustPointer(void);
  virtual void calcPropIndicator(void);

  void repeat(void);
  void delay(void);

  void startDelayTimer(void);
  void startRepeatTimer(void);
  void stopRepeatTimer(void);

  virtual int calcSliderValue(int,int);
  void release(void);

  // valueChange methods to be overriden by subclasses
  virtual void increment(void);
  virtual void decrement(void);
  virtual void pageDown(void);
  virtual void pageUp(void);
  virtual void drag(void);
  virtual void home(void);
  virtual void end(void);
  virtual void change(void);

  virtual void firstMapNotify(void);
  virtual void notify(int value_);
  virtual void expose(const XEvent*);  
  virtual void buttonPress(const XEvent*);
  virtual void buttonRelease(const XEvent*);
  virtual void button1Press(const XEvent*);
  virtual void button2Press(const XEvent*);
  virtual void button3Press(const XEvent*);
  virtual void openlookButton1Press(const XEvent*);
  virtual void openlookButton2Press(const XEvent*);
  virtual void motifButton1Press(const XEvent*);
  virtual void motifButton2Press(const XEvent*);
  virtual void configure(void);
  virtual void configureForOpenlook(void);
  virtual void configureForMotif(void);
  virtual void configureForWindows(void);

  virtual void updateBackground(unsigned long);

private:
  static MSScrollBar::Style _defaultStyle;
};

#endif









