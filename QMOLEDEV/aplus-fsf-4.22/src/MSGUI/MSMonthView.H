#ifndef MSMonthViewHEADER
#define MSMonthViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDate.H>
#include <MSGUI/MSPrimitiveText.H>
#include <MSGUI/MSArrowButton.H>
#include <MSGUI/MSDoubleClick.H>

class MSGUIExport MSMonthView : public MSPrimitiveText, public MSDoubleClick
{
public:
  enum TitleFormat { Month, MonthYear };

  MSMonthView(MSWidget *, const char *title_=0);
  MSMonthView(MSWidget *, const MSDate&);
  MSMonthView(MSWidget *, MSDate&);
  ~MSMonthView(void);

  TitleFormat titleFormat(void) const  { return _titleFormat; }
  void titleFormat(TitleFormat);
  
  int margin(void) const { return _margin; }
  void margin(int);

  MSBoolean arrowButtons(void) const { return _arrowButtons; }
  void arrowButtons(MSBoolean);

  unsigned long arrowColor(void) const;
  void arrowColor(const char *);
  void arrowColor(unsigned long);
  
  MSBoolean showSelection(void) const { return _showSelection; }
  void showSelection(MSBoolean);

  MSBoolean showGrid(void) const { return _showGrid; }
  void showGrid(MSBoolean);
  
  MSDate& viewDate(void)              { return (MSDate &)*_model;}
  const MSDate& viewDate(void) const  { return (MSDate &)*_model;}
  void viewDate(const MSDate&);

  void model(MSDate&);
  void model(const MSDate&);

  MSDate& viewModel(void)             { return *((MSDate*)_model); }
  const MSDate& viewModel(void) const { return *((MSDate*)_model); }

  operator MSDate&(void)              { return (MSDate &)*_model;}

  MSMonthView& operator=(const MSDate& date_) { viewDate(date_); return *this; }

  virtual void print(const char *file_=0);
  virtual void redraw(void);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

protected:
  class MSGUIExport GridLocation
  {
    unsigned _row;
    unsigned _column;
  public:
    GridLocation(void) :
    _row(0),_column(0) {}
    GridLocation(const GridLocation& aGL_) :
    _row(aGL_._row),_column(aGL_._column) {}
    GridLocation(unsigned row_,unsigned column_) :
    _row(row_),_column(column_) {}

    GridLocation& operator=(const GridLocation& aGL_) 
    { return _row=aGL_._row,_column=aGL_._column,*this; }

    unsigned row(void)            const {return _row;}
    unsigned column(void)         const {return _column;}
    void row(unsigned row_)             {_row=row_;}
    void column(unsigned column_)       {_column=column_;}
  };

  class MSGUIExport MonthViewArrow : public MSArrowButton
  {
  protected:
    virtual void activate(void);
    virtual void buttonPress(const XEvent *);
    
    MSMonthView * monthView(void) { return (MSMonthView *)owner();}
  public:
    MonthViewArrow(MSWidget *, MSArrow::ArrowType);
  };

  friend class MonthViewArrow;


protected:
  int                 _margin;
  MSRect              _dayRect;
  unsigned            _firstDayOffset;
  MSBoolean           _arrowButtons;
  MonthViewArrow     *_leftArrow;
  MonthViewArrow     *_rightArrow;
  MSBoolean           _showSelection;
  TitleFormat         _titleFormat;
  MSBoolean           _showGrid;
  
  void init(void);

  virtual void computeSize(void);
  virtual void firstMapNotify(void);
  virtual void configure(void);

  virtual void updateTitle(void);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateSensitivity(void);

  virtual void drawCalendar(void);
  virtual void drawDayGrid(void);    
  virtual void drawDay(MSDay);  
  virtual void drawTitle(void);
  virtual void drawDayHeadings(void);
  virtual void drawSelection(MSBoolean);  

  virtual void drawSeparators(void);
  
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char*);
  virtual void buttonPress(const XEvent*);

  virtual void up(void);
  virtual void down(void);
  virtual void right(void);
  virtual void left(void);
  virtual void pageUp(void);
  virtual void pageDown(void);
  virtual void home(void);
  virtual void end(void);
  virtual void returnKey(void);
  virtual void escape(void);
  
  virtual void nextMonth(void);
  virtual void prevMonth(void);

  virtual void activate(void);

  virtual void update(const MSIndexVector&);
  virtual void updateData(void);

  virtual void dateChanged(void);
  
  GridLocation gridLocationFor(MSDay);
  void selectedDay(MSDay);
  MSDay selectedDay(void) const  { return showSelection() ? viewDate().dayOfMonth() : 0; }

  int outerMargin(void) const;
  int idealWidth(void);
  int idealHeight(void);

};

#endif
