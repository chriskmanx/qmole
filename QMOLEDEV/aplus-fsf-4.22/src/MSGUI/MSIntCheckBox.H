#ifndef MSIntCheckBoxHEADER
#define MSIntCheckBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSModelCheckBox.H>
#include <MSTypes/MSIntVector.H>

class MSGUIExport MSIntCheckBox : public MSModelCheckBox
{
friend class MSCheckButton;
public:  
  MSIntCheckBox(MSWidget *,const char *title_=0);
  MSIntCheckBox(MSWidget *,const MSStringVector &title_);
  MSIntCheckBox(MSWidget *,MSIntVector& model_,const char *title_=0);
  MSIntCheckBox(MSWidget *,MSIntVector& model_,const MSStringVector& title_);
  ~MSIntCheckBox(void);

  void model(MSIntVector&);

  MSIntVector& viewModel(void)             { return *((MSIntVector*)_model); }
  const MSIntVector& viewModel(void) const { return *((MSIntVector*)_model); }
  operator MSIntVector &()    { return *((MSIntVector *)_model); }

protected:
   virtual void drawCurrentState(void);
   virtual void setModel(void);
};

#endif












