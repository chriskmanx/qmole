#ifndef AplusEntryFieldHEADER
#define AplusEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AplusGUI/Macros.H>
#include <MSGUI/MSEntryFieldPlus.H>
#include <MSGUI/MSShadow.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusModel.H>

class  AplusEntryField : public MSEntryFieldPlus
{
public:
  AplusEntryField(MSWidget *);
  ~AplusEntryField(void);

  virtual MSBoolean verifyData(V,A);

  // Override the following two functions to make them public
  //
  virtual void edit(void);
  virtual MSBoolean activateEditor(void);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);

  virtual void createCycle(void);
  virtual MSBoolean isProtected(void) const;
  virtual MSBoolean validate(const char *);
  virtual void firstMapNotify(void);
  virtual const char *formatOutput(MSString &);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateTitle(void);
  virtual void increment(void);
  virtual void decrement(void);
  virtual void currentColors(unsigned long&,unsigned long&);

  const char *itemLabel(void);
  A defaultInFunc(V v_,const char *string_);

  void setClipMode(void);

private:
  STANDARD_WIDGET_METHODS_H
};

#endif //AplusEntryFieldHEADER
