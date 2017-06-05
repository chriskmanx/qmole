#ifndef MSPulldownMenuHEADER
#define MSPulldownMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenu.H>

class MSMenuBarItem;
class MSGUIExport MSPulldownMenu : public MSMenu
{
protected:
  MSMenuBarItem *item(void)  { return (MSMenuBarItem*)parentMenuItem(); }
  const MSMenuBarItem *item(void) const { return (MSMenuBarItem*)parentMenuItem(); }

public:
  MSPulldownMenu(MSMenuBarItem *);
  ~MSPulldownMenu(void);

  virtual void activate(void);
  virtual void done(void);
  virtual void left(void);
  virtual void right(void);
};

#endif
