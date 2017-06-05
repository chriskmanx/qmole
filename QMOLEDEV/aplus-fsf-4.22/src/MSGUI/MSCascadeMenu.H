#ifndef MSCascadeMenuHEADER
#define MSCascadeMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenu.H>

class MSCascadeMenuItem;
class MSGUIExport MSCascadeMenu : public MSMenu
{
protected:
  MSCascadeMenuItem *item(void)             { return (MSCascadeMenuItem*)parentMenuItem(); }
  const MSCascadeMenuItem *item(void) const { return (MSCascadeMenuItem*)parentMenuItem(); }

public:
  MSCascadeMenu(MSCascadeMenuItem *);
  ~MSCascadeMenu(void);

  virtual void activate(void);
  virtual void done(void);
  virtual void left(void);
  virtual void right(void);
};

#endif
