#ifndef AplusCommandHEADER
#define AplusCommandHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusEntryField.H>

class AplusCommand : public AplusEntryField
{
public:
  AplusCommand(MSWidget *);
  ~AplusCommand(void);

  virtual MSBoolean verifyData(V v_,A a_);

  const char *buffer(void);
  void buffer(A a_);
  int cursorPosition(void);
  void cursorPosition(int cp_);

  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);
};

#endif // AplusCommandHEADER
