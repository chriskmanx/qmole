#ifndef MSEntryFieldComboHEADER
#define MSEntryFieldComboHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryField.H>
#include <MSIPC/MSIntervalTimer.H>
#include <MSIPC/MSRegularTimer.H>

class MSArrow;

class MSGUIExport MSEntryFieldCombo : public MSEntryField
{

public:
  enum TimerDirection {Up,Down};

  class MSGUIExport RepeatTimer : public MSIntervalTimer
  {
  public:
    RepeatTimer(MSEntryFieldCombo *ef_,unsigned long interval_,TimerDirection direction_=Up);

    void direction(TimerDirection direction_);
    TimerDirection direction(void) const;
    MSEntryFieldCombo *entryField(void);

    virtual void process(void);
  protected:
    MSEntryFieldCombo *_entryField;
    TimerDirection         _direction;
  };

  class MSGUIExport DelayTimer : public MSRegularTimer
  {
  public:
    DelayTimer(MSEntryFieldCombo *ef_,unsigned long interval_,TimerDirection direction_=Up);

    void direction(TimerDirection direction_);
    TimerDirection direction(void) const;
    MSEntryFieldCombo *entryField(void);

    virtual void process(void);
  protected:
    MSEntryFieldCombo *_entryField;
    TimerDirection     _direction;
  };

friend class DelayTimer;
friend class RepeatTimer;


public:
  MSEntryFieldCombo(MSWidget *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSEntryFieldCombo(MSWidget *,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSEntryFieldCombo(void);

  virtual void redraw(void);

protected:
  enum ButtonFlag { UpDownArrows = 0x01, ComboButton = 0x02,
		    TextButton = 0x04 };

  MSBoolean buttonState(ButtonFlag) const;
  void buttonState(ButtonFlag, MSBoolean);

  unsigned long buttonColor(ButtonFlag) const;
  void buttonColor(ButtonFlag, unsigned long);
  void buttonColor(ButtonFlag, const char *);
  
  const MSString& comboButtonLabel(void) const;
  void comboButtonLabel(const MSString&);

  unsigned buttonSpacing(void) const;
  void buttonSpacing(unsigned);

protected:
  unsigned      _buttonSpacing;
  unsigned      _buttonFlags;
  MSArrow      *_upArrow;
  MSArrow      *_downArrow;
  MSArrow      *_comboArrow;
  RepeatTimer  *_repeatTimer;
  DelayTimer   *_delayTimer;
  MSString      _buttonLabel;
  MSRect        _buttonRect;
  MSBoolean     _buttonSelected;
  
  void init(void);

  void setBit(ButtonFlag);
  void unsetBit(ButtonFlag);
  MSBoolean bitState(unsigned)  const;
  
  MSArrow *upArrow(void);
  MSArrow *downArrow(void);
  RepeatTimer *repeatTimer(void);
  MSRect&  buttonRect(void);

  MSBoolean  comboOrTextButton(void) const;
 
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void placement(void);
  virtual void drawArrows(void);
  virtual void armUpArrow(void);
  virtual void armDownArrow(void);
  virtual unsigned computeValuePixelWidth(void);
  virtual unsigned computeButtonPixelWidth(void);
  virtual unsigned computeArrowPixelWidth(void);
  virtual unsigned computeDecorPixelWidth(void);
  virtual void labelPixelWidth(unsigned);

  virtual void drawComboButton(MSBoolean);
  virtual void drawTextButton(MSBoolean);
  virtual void up(void);
  virtual void down(void);

  virtual void textButtonActivate(void);
  virtual void buttonActivate(void);
  virtual void reference(void);

  void processDelayTimer(void);
  void startRepeatTimer(TimerDirection);
  void startDelayTimer(TimerDirection);
  void stopTimers(void);
};


#endif






