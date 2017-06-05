#ifndef MSArrowButtonHEADER
#define MSArrowButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSArrow.H>
#include <MSIPC/MSIntervalTimer.H>

class MSGUIExport MSArrowButton : public MSWidgetCommon
{
protected:
  class MSGUIExport MSArrowTimer : public MSIntervalTimer
  {
  protected:
    MSArrowButton *_owner;
    MSArrowButton *owner(void) {return _owner;}
  public:
    MSArrowTimer(unsigned long,MSArrowButton*);
    virtual void process(void); 
  };

friend class MSArrowTimer;

  MSArrow       *_arrow;
  MSArrowTimer  *_arrowTimer;
  unsigned long  _repeatInterval;
  unsigned long  _repeatThreshold;

  MSArrow *arrow(void) {return _arrow;}
  const MSArrow *arrow(void) const {return _arrow;}
  MSArrowTimer *arrowTimer(void) {return _arrowTimer;}
  virtual void configure(void);
  virtual void computeSize(void);
  virtual void firstMapNotify(void);
  virtual void buttonPress(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void button1Release(const XEvent *);
  virtual void updateBackground(unsigned long);
  virtual void activate(void);

  void init(void);
  
public:
  MSArrowButton(MSWidget *owner_);
  MSArrowButton(MSWidget *owner_,MSArrow::ArrowType);
  ~MSArrowButton(void);

  void type(MSArrow::ArrowType);
  MSArrow::ArrowType type(void) const {return arrow()->arrowType();}
  
  unsigned long repeatThreshold(void) const {return _repeatThreshold;}
  void repeatThreshold(unsigned long);

  unsigned long repeatInterval(void) const {return _repeatInterval;}
  void repeatInterval(unsigned long);

  unsigned long arrowColor(void) const  { return arrow()->color();}
  void arrowColor(unsigned long);
  void arrowColor(const char * );

  virtual void naturalSize(void);
  virtual void redraw(void);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
  
};

#endif

