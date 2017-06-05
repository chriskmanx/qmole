#ifndef MSStringPopupMenuHEADER
#define MSStringPopupMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPopupMenu.H>

class MSGUIExport MSStringPopupMenu : public MSPopupMenu
{
public:
  MSStringPopupMenu(MSDisplayServer *);  
  MSStringPopupMenu(MSDisplayServer *,MSStringVector& aModel_);

  void model(const MSStringVector& aModel_);
 
  const MSStringVector& viewModel(void) const { return (MSStringVector&)*_model; }  
protected:
  const MSStringVector& stringVector(void) const { return (MSStringVector&)*_model; }  
  
  virtual void updateData(void);
  virtual void receiveEvent(MSEvent&);
};

#endif
