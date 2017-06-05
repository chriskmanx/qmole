#ifndef AplusTableHEADER
#define AplusTableHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTable.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>

class AplusTableColumn;

class AplusTable : public MSTable
{
  friend class AplusTableColumn;
  
public:
  AplusTable(MSWidget *);
  ~AplusTable(void);
  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  void grandTotalTextA(::A grandTotalTextA_) {_grandTotalTextA=grandTotalTextA_;}
  ::A grandTotalTextA(void) {return _grandTotalTextA;}
  
protected:
  MSBoolean verifyColumn(::A);
  virtual MSBoolean verifyData(V,::A);
  Font getVFont(V);
  void variables(V *,int);
  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);
  
  virtual void updateData(void);
  virtual void update(V,int,int,UpdateType);
  virtual void update(V,::A,::A,I);
  virtual void update(const MSIndexVector &v_) {MSTable::update(v_);}
  virtual void updateTitle(void);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual unsigned long cellForeground(unsigned, unsigned);
  virtual unsigned long cellBackground(unsigned, unsigned);
  virtual void createCycle(int, int);
  virtual void shuffleColumns(MSIndexVector &);

  MSUnsignedLongVector getCycleColors(int, int) const;
  void referenceCB(void);

  ::A _grandTotalTextA;

  STANDARD_WIDGET_METHODS_H
};

#endif  //AplusTableHEADER
