#ifndef MSWindowHEADER
#define MSWindowHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenuShell.H>
#include <MSGUI/MSLayout.H>

class MSGUIExport MSWindow : public MSMenuShell
{
public:
  MSWindow(const char * windowTitle_=0);
  MSWindow(MSDisplayServer *,const char *windowTitle_=0);
  ~MSWindow(void);
  
  MSLayout * layout(void);
  const MSLayout * layout(void) const;

protected:
  void init(void);

private:
};

inline MSLayout * MSWindow::layout(void)
{ return (MSLayout *) _child; }

inline const MSLayout * MSWindow::layout(void) const
{ return (const MSLayout *) _child; }

#endif
  
  

