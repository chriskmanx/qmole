#ifndef MSCheckPopupMenuHEADER
#define MSCheckPopupMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1999-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringPopupMenu.H>

class MSCheckPopupMenu : public MSStringPopupMenu
{
protected:
  MSStringVector  _selection;
  unsigned long   _selectColor;

  virtual void updateData(void);
  virtual void receiveEvent(MSEvent&);
  virtual void activate(void);
  
  void resetSelection(void);
  void init(void);
  
public:
  MSCheckPopupMenu(MSDisplayServer *);  
  MSCheckPopupMenu(MSDisplayServer *,MSStringVector& aModel_);
  ~MSCheckPopupMenu(void);

  void selection(const MSStringVector &);
  const MSStringVector &selection(void) const { return _selection; }

  void selectColor(unsigned long);
  void selectColor(const char *);
  unsigned long selectColor(void) const { return _selectColor; }
};



#endif
