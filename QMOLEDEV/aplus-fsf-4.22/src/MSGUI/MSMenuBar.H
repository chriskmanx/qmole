#ifndef MSMenuBarHEADER
#define MSMenuBarHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenu.H>

class MSGUIExport MSMenuBar : public MSMenu
{
public:
  enum Orientation { Horizontal, Vertical };

protected:
friend class MSMenu;

  Orientation   _orientation;
  
  virtual void computeSize(void);
  virtual void placement(void);

  virtual void pointerLeave(void);
  virtual void selectNone(void);
  virtual void buttonPress(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual MSMenuItem *nextRightItem(void);
  virtual MSMenuItem *nextLeftItem(void);
  virtual MSMenuItem *nextUpItem(void);
  virtual MSMenuItem *nextDownItem(void);

  void dropMenu(MSMenuItem *);
  void moveToMenu(MSMenuItem* );
  
public:
  MSMenuBar(MSWidget *);
  ~MSMenuBar(void);

  MSMenu *selectedMenu(void);
  const MSMenu *selectedMenu(void) const;

  Orientation orientation(void) const { return _orientation; }
  void orientation(Orientation);
  
  virtual void done(void);
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);
  virtual void unmap(void);
  virtual void map(void);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);
  
};


#endif
