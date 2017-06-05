#ifndef MSIntRadioBoxHEADER
#define MSIntRadioBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSRadioBox.H>
#include <MSTypes/MSInt.H>

class MSGUIExport MSIntRadioBox : public MSRadioBox
{
friend class MSRadioButton;

public:  
  MSIntRadioBox(MSWidget *,const char *title_=0);
  MSIntRadioBox(MSWidget *,const MSStringVector &);
  MSIntRadioBox(MSWidget *,MSInt& model_,const char *title_=0);
  MSIntRadioBox(MSWidget *,MSInt& model_,const MSStringVector&);
  ~MSIntRadioBox(void);

  void model(MSInt&);

  MSInt& viewModel(void)             { return *((MSInt*)_model); }
  const MSInt& viewModel(void) const { return *((MSInt*)_model); }
  operator MSInt&()  { return *((MSInt *)_model); }
  operator int()     { return (MSView::model()!=0)?(int)*((MSInt *)_model):0; }
  
protected:
  void drawCurrentState(void);
  void setModel(void);
  virtual void arm(MSRadioButton *);
  virtual void receiveEvent(MSEvent &);
  virtual void updateData(void);
  virtual void firstMapNotify(void);
};

#endif












