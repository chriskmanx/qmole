#ifndef MSPopupMenuHEADER
#define MSPopupMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenu.H>

class MSGUIExport MSPopupMenu : public MSMenu
{
public:
  MSPopupMenu(MSDisplayServer *);
  ~MSPopupMenu(void);
  virtual void show(void);
  virtual void showAtPointer(void);
};

#endif
