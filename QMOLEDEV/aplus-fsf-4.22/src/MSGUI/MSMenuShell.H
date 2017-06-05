#ifndef MSMenuShellHEADER
#define MSMenuShellHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSShell.H>
#include <MSGUI/MSMenuBar.H>

class MSGUIExport MSMenuShell : public MSShell
{
public:
  MSMenuShell(const char *windowTitle_=0);
  MSMenuShell(MSDisplayServer *,const char *windowTitle_=0);
  ~MSMenuShell(void);

  MSMenuBar *menuBar(void);
  const MSMenuBar *menuBar(void) const;

protected:
  void init(void);

  int menuBarWidth(void) const;
  int menuBarHeight(void) const;
  MSBoolean placementFlag(void) const;
  virtual int idealShellWidth(void) const;
  virtual int idealShellHeight(void) const;
  virtual void adjustChildPosition(void);
  virtual void adjustChildSize(void);
  virtual void showChildren(void);  

  virtual void childConfigure(MSWidget *);
  virtual void childDestroy(MSWidget *);

  virtual MSBoolean processAltKey(const XEvent *,KeySym,unsigned int,const char *);
  virtual MSBoolean processFunctionKey(const XEvent *,KeySym,unsigned int,const char *);

  virtual MSWidgetVector children(void);

private:
  MSMenuBar *_menuBar;
  MSBoolean  _placementFlag;
};

inline MSMenuBar *MSMenuShell::menuBar(void)
{ return _menuBar; }
inline const MSMenuBar *MSMenuShell::menuBar(void) const
{ return _menuBar; }
inline MSBoolean MSMenuShell::placementFlag(void) const
{ return _placementFlag; }

#endif
