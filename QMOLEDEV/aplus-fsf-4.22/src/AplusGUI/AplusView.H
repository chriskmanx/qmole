#ifndef AplusViewHEADER
#define AplusViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSList.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>

class  AplusView : public MSList
{
public:
  AplusView(MSWidget *);
  ~AplusView(void);
  virtual MSBoolean verifyData(V,A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);
  virtual unsigned numRows(void) const;
  virtual unsigned numColumns(void) const;
  virtual unsigned maxLength(void) const;
  virtual unsigned rowLength(unsigned) const;

  virtual void drawSelectOutline(Window, int, MSBoolean,MSBoolean=MSFalse);

  virtual unsigned long rowForeground(unsigned);
  virtual unsigned long rowBackground(unsigned);

  virtual void update(V,int,int,UpdateType);
  virtual void update(V,A,A,I);
  virtual void update(const MSIndexVector &v_) { MSList::update(v_); }

  virtual const char *formatOutput(MSString&,unsigned);

  virtual unsigned long selectedRowForegroundColor(int);
  void referenceCB(void);

  STANDARD_WIDGET_METHODS_H
};

#endif  //AplusViewHEADER
