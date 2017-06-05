#ifndef AplusArrayHEADER
#define AplusArrayHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSArrayView.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>

class  AplusArray : public MSArrayView
{
public:
  AplusArray(MSWidget *);
  ~AplusArray(void);
  virtual MSBoolean verifyData(V,A);
  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual MSBoolean validate(const char *,unsigned,unsigned);
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);
  virtual A defaultInFunc(V v_,const char *string);
  virtual unsigned numRows(void) const;
  virtual unsigned numColumns(void) const;

  virtual void update(V,int,int,UpdateType);
  virtual void update(V,A,A,I);
  virtual void update(const MSIndexVector &v_) { MSArrayView::update(v_); }
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual MSBoolean isCellProtected(unsigned, unsigned);
  virtual unsigned long cellForeground(unsigned, unsigned);
  virtual unsigned long cellBackground(unsigned, unsigned);
  virtual Font cellFont(unsigned,unsigned);
  virtual MSClipMode columnClipMode(unsigned) const;
  virtual void createCycle(int, int);
  MSUnsignedLongVector getCycleColors(int, int);
  
  virtual const char *formatOutput(MSString&,unsigned,unsigned);
  virtual MSBoolean validate(V,const char *,unsigned,unsigned);

  virtual void defaultButton3Behavior(const XEvent *);  

  void referenceCB(void);

  STANDARD_WIDGET_METHODS_H
};

#endif  //AplusArrayHEADER
