#ifndef MSDateEntryFieldHEADER
#define MSDateEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSEntryFieldPlus.H>
#include <MSGUI/MSMonthView.H>
#include <MSTypes/MSDate.H>

class MSGUIExport MSDateEntryField : public MSEntryFieldPlus
{
protected:
  class DateShell;
  class DateMonthView;
  friend class DateShell;
  friend class DateMonthView;

  class MSGUIExport DateMonthView : public MSMonthView
  {
  protected:
    DateShell * dateShell(void)       { return (DateShell *)owner(); }
    MSDateEntryField *dateField(void) { return dateShell()->dateField();}
    
    virtual void keyPress(const XEvent *, KeySym, unsigned int, const char*);
    virtual void buttonPress(const XEvent *);
    virtual void buttonRelease(const XEvent *);

    virtual void home(void);
    virtual void end(void);

    virtual void escape(void);

    virtual void nextMonth(void);
    virtual void prevMonth(void);

    virtual void activate(void);

  public:
    DateMonthView(MSWidget *owner_);
  };

  class MSGUIExport DateShell : public MSWidget
  {
  protected:
    DateMonthView *_monthView;
    MSDateEntryField *_dateField;
  public:
    DateShell(MSDateEntryField* owner_);
    ~DateShell();
    
    DateMonthView * monthView(void)             { return _monthView; }
    const DateMonthView * monthView(void) const { return _monthView; }
    MSDateEntryField * dateField(void)          { return _dateField; }

    virtual void showAt(int, int);
    virtual void show(void);
    virtual void hide(void);

    virtual void escape(void);
    virtual void activate(void);

    void grab(void);
    void ungrab(void);

  protected:    
    virtual void configure(void);
    virtual void childConfigure(MSWidget*);
  };

  
private:
  MSTerm _incrementValue;
  MSDate _minimumValue;  
  MSDate _maximumValue;
  DateShell *_dateShell;
  
protected:

  DateShell * dateShell(void)             { return _dateShell; }
  const DateShell * dateShell(void) const { return _dateShell; }

  void showMonthView(void);
  void monthViewActivate(void);
  void monthViewEscape(void);
  
  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);
  virtual void increment(void);
  virtual void decrement(void);

  virtual void buttonActivate(void);
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);

  virtual void generateInputMask(void);
  MSDate& value(void) const { return *((MSDate *)_model); } 
  void value(const MSDate& value_) const { if (MSView::model()!=0) value()=value_; }

  void init(void);
  virtual void updateData(void);
  
public:
  MSDateEntryField(MSWidget *,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSDateEntryField(MSWidget *,MSDate&,const char *label_=0,
		   const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSDateEntryField(void);

  void model(MSDate&);
  void model(const MSDate&);

  MSDate& viewModel(void)             { return *((MSDate*)_model); }
  const MSDate& viewModel(void) const { return *((MSDate*)_model); }

  const MSTerm& incrementValue(void) const {return _incrementValue;}
  const MSDate& maximumValue(void)   const {return _maximumValue;}
  const MSDate& minimumValue(void)   const {return _minimumValue;}

  void incrementValue(const MSTerm& incrementValue_) {_incrementValue=incrementValue_;}
  void maximumValue(const MSDate& maximumValue_)     {_maximumValue=maximumValue_;}
  void minimumValue(const MSDate& minimumValue_)     {_minimumValue=minimumValue_;}

  MSDateEntryField& operator=(const MSDate& value_) {value(value_); return *this;}
  operator MSDate&()                               {return value();}

  MSBoolean  monthDropDown(void) const   { return buttonState(ComboButton); }
  void monthDropDown(MSBoolean dropDown_);   

  void comboArrowColor(unsigned long color_) { buttonColor(ComboButton, color_); }
  void comboArrowColor(const char * color_)  { buttonColor(ComboButton, color_); }
  unsigned long comboArrowColor(void)        { return buttonColor(ComboButton); }

  MSMonthView *monthView(void);           
  const MSMonthView *monthView(void) const;

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
};

#endif






