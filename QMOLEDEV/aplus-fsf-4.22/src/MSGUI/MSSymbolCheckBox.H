#ifndef MSSymbolCheckBoxHEADER
#define MSSymbolCheckBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSModelCheckBox.H>
#include <MSTypes/MSSymbolVector.H>

class MSGUIExport MSSymbolCheckBox : public MSModelCheckBox
{
friend class MSCheckButton;
public:  
  MSSymbolCheckBox(MSWidget *,const char *title_=0);
  MSSymbolCheckBox(MSWidget *,const MSStringVector &title_);
  MSSymbolCheckBox(MSWidget *,MSSymbolVector& model_,const char *title_=0);
  MSSymbolCheckBox(MSWidget *,MSSymbolVector& model_,const MSStringVector& title_);
  ~MSSymbolCheckBox(void);
  
  void model(MSSymbolVector&);

  MSSymbolVector& viewModel(void)             { return *((MSSymbolVector*)_model); }
  const MSSymbolVector& viewModel(void) const { return *((MSSymbolVector*)_model); }
  operator MSSymbolVector &()    { return *((MSSymbolVector *)_model); }

protected:
   virtual void drawCurrentState(void);
   virtual void setModel(void);
};

#endif












