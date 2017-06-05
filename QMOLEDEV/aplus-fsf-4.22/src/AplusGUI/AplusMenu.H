#ifndef AplusMenuHEADER
#define AplusMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenuBar.H>
#include <MSGUI/MSPulldownMenu.H>
#include <MSGUI/MSMenuBarItem.H>
#include <MSGUI/MSCascadeMenu.H>
#include <MSGUI/MSCascadeMenuItem.H>
#include <MSGUI/MSWidgetCursor.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>

// The AplusMenu class is a subclass of MSMenuBar.  When a new variable
// is coupled to the class, updateData() calls buildMenu to construct
// the menu structure by recursively walking the nested slot filler.
// When a menuItem is activated, the message is passed all the way up
// to the menubar (unless there is a callback).
//
class  AplusMenu : public MSMenuBar
{
public:
  AplusMenu(MSWidget *, Orientation =Horizontal);
  ~AplusMenu(void);
  virtual MSBoolean verifyData(V,A);
  void mnemonics(A);
  A mnemonics(void);
  
  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);

  virtual void updateData(void);
  virtual void update(V,int,int,UpdateType) {}
  virtual void update(V,A,A,I);
  virtual void update(const MSIndexVector &v_) { MSMenuBar::update(v_); }
    
  virtual void activate(void);

  void buildMenu(A,A);
  void buildPullDown(MSMenuBarItem *,A,S *,int);
  void buildCascadePullDown(MSCascadeMenuItem *,A,S *,int);
  void buildCascades(MSMenu *,A,S *,int);
  void formSymbolVector(MSStringVector &);
  virtual MSMenuBarItem *newMenuBarItem(const char *);
  MSBoolean verifyMnemonic(MSMenu *, A);
  void setMnemonic(MSMenu *, A);
  A getMnemonic(MSMenu*);
  
  STANDARD_WIDGET_METHODS_H
};

#endif // AplusMenuHEADER
